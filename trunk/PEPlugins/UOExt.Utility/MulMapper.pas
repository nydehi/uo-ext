unit MulMapper;

interface

uses UOExt.Utility.Bindings;

  procedure AskForMulMapping; stdcall;
  function GetMulMappingInfo(AMulName:PAnsiChar):PMappingRec; stdcall;
  function EnshureFreeMappedSpace(AMulName: PAnsiChar; Amount: Cardinal):Boolean; stdcall;

  procedure FreeMulMapping;

implementation

uses Windows, Common, APIHooker, ExecutableSections;

type
  PMappingPage=^TMappingPage;
  TMappingPage=record
    Size: Cardinal;
    Next: PMappingPage;
    Mapping: Array [0..0] of TMappingRec;
  end;

var
  Mapping: PMappingPage;
  LastMappingPage: PMappingPage;
  CloseHandleLock: TRTLCriticalSection;
  MapViewOfFileLock: TRTLCriticalSection;

function CreateFileAHook(ACaller: Cardinal; lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; stdcall;
type
  TCreateFileA = function(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; stdcall;
var
  s:AnsiString;
  i:integer;
  bFound: Boolean;
  Current: PMappingPage;
  lpwFileName: PWideChar;
begin
  lpwFileName := nil;
  i := MultiByteToWideChar(DefaultSystemCodePage, 0, lpFileName, -1, lpwFileName, 0);
  lpwFileName := GetMemory((i+1) * 2);
  MultiByteToWideChar(DefaultSystemCodePage, 0, lpFileName, -1, lpwFileName, i+1);
  if not IsAddressFromExecutable(ACaller) then Begin
    Result := CreateFileW(lpwFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
    Exit;
  End;

  s:=lpFileName;
  i:=Length(s);
  repeat
    if s[i]='.' then begin
      s:=Copy(s, i + 1, Length(s));
      Break;
    end;
    Dec(i);
  until i=0;
  s := UpperCase(s);
  if (s = 'MUL') or (s = 'IDX') then Begin
    dwDesiredAccess := GENERIC_READ + GENERIC_WRITE;
    dwShareMode := FILE_SHARE_READ + FILE_SHARE_WRITE;
  End;

  Result := CreateFileW(lpwFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);

  if (s = 'MUL') or (s = 'IDX') then Begin
    s:=lpFileName;
    i:=Length(s);
    repeat
      if s[i]='\' then begin
        s:=Copy(s, i + 1, Length(s));
        Break;
      end;
      Dec(i);
    until i=0;
    s := UpperCase(s);

    bFound := False;
    if LastMappingPage <> nil then Begin
      bFound := False;
      Current := Mapping;
      repeat
        for i := 0 to Mapping.Size - 1 do if (Current.Mapping[i].MappingPointer <> nil) AND (Current.Mapping[i].FileName = s) then Begin
          Current.Mapping[i].FileHandle := Result;
          Current.Mapping[i].FileLength := GetFileSize(Result, nil);
          bFound := True;
          Break;
        End;
        Current := Current.Next;
      until Current = nil;
      if not bFound then for i := 0 to LastMappingPage^.Size - 1 do if LastMappingPage^.Mapping[i].MappingPointer = nil then Begin
        LastMappingPage^.Mapping[i].FileHandle := Result;
        LastMappingPage^.Mapping[i].FileName := GetMemory(Length(s) + 1);
        ZeroMemory(LastMappingPage^.Mapping[i].FileName, Length(s) + 1);
        CopyMemory(LastMappingPage^.Mapping[i].FileName, @s[1], Length(s));
        LastMappingPage^.Mapping[i].FileLength := GetFileSize(Result, nil);
        bFound := True;
        Break;
      End;
    End;
    if not bFound then Begin
      if LastMappingPage <> nil then Begin
        LastMappingPage.Next := GetMemory(SizeOf(TMappingPage) + SizeOf(TMappingRec)*9);
        LastMappingPage := LastMappingPage.Next;
      End Else Begin
        LastMappingPage := GetMemory(SizeOf(TMappingPage) + SizeOf(TMappingRec)*9);
        Mapping := LastMappingPage;
      End;
      LastMappingPage.Size := 10;
      for i := 0 to 9 do Begin
        LastMappingPage.Mapping[i].MappingPointer := nil;
        LastMappingPage.Mapping[i].AskedFreeSpace := 0;
      End;

      LastMappingPage^.Mapping[0].FileHandle := Result;
      LastMappingPage^.Mapping[0].FileName := GetMemory(Length(s) + 1);
      ZeroMemory(LastMappingPage^.Mapping[0].FileName, Length(s) + 1);
      CopyMemory(LastMappingPage^.Mapping[0].FileName, @s[1], Length(s));
      LastMappingPage^.Mapping[0].FileLength := GetFileSize(Result, nil);
    End;
  End;
end;

function CloseHandleHook(ACaller: Cardinal; hObject: THandle): BOOL; stdcall;
var
  i: Integer;
  Current: PMappingPage;
begin
  EnterCriticalSection(CloseHandleLock);
  THooker.Hooker.TrueAPI;
  Result := CloseHandle(hObject);
  THooker.Hooker.TrueAPIEnd;

  If Result AND IsAddressFromExecutable(ACaller) Then Begin
    if Mapping = nil then Begin
      LeaveCriticalSection(CloseHandleLock);
      Exit;
    End;
    Current := Mapping;
    repeat
      For i := 0 to Current.Size - 1 do Begin
        if Current.Mapping[i].FileHandle = hObject then Current.Mapping[i].FileHandle := INVALID_HANDLE_VALUE;
        if Current.Mapping[i].MappingHandle = hObject then Current.Mapping[i].MappingHandle := INVALID_HANDLE_VALUE;
      End;
      Current := Current.Next;
    until Current = nil;
  End;
  LeaveCriticalSection(CloseHandleLock);
end;

function CreateFileMappingAHook(ACaller:Cardinal; hFile: THandle; lpFileMappingAttributes: PSecurityAttributes;
  flProtect, dwMaximumSizeHigh, dwMaximumSizeLow: DWORD; lpName: PAnsiChar): THandle; stdcall;
var
  i: Integer;
  bFutherProcessing: Boolean;
  Current: PMappingPage;
  lpwName: PWideChar;

Begin
  lpwName := nil;
  i := MultiByteToWideChar(DefaultSystemCodePage, 0, lpName, -1, lpwName, 0);
  lpwName := GetMemory((i+1) * 2);
  MultiByteToWideChar(DefaultSystemCodePage, 0, lpName, -1, lpwName, i+1);
  if not IsAddressFromExecutable(ACaller) then Begin
    Result := CreateFileMappingW(hFile, lpFileMappingAttributes, flProtect, dwMaximumSizeHigh, dwMaximumSizeLow, lpwName);
    Exit;
  End;

  bFutherProcessing := False;
  If hFile <> INVALID_HANDLE_VALUE Then If Mapping <> nil then Begin
    Current := Mapping;
    Repeat
      for i := 0 to Current.Size - 1 do if Current.Mapping[i].FileHandle = hFile then Begin
        flProtect := PAGE_READWRITE;
        bFutherProcessing := True;
        Current.Mapping[i].MappingLength := Current.Mapping[i].FileLength;
        if Current.Mapping[i].AskedFreeSpace > 0 then Begin
          if dwMaximumSizeLow = 0 then Begin
            dwMaximumSizeLow := Current.Mapping[i].FileLength + Current.Mapping[i].AskedFreeSpace;
            Current.Mapping[i].MappingLength := dwMaximumSizeLow;
          End;
        End;

        Break;
      End;
      if bFutherProcessing then Break;
      Current := Current.Next;
    Until Current = nil;
  End;

  Result := CreateFileMappingW(hFile, lpFileMappingAttributes, flProtect, dwMaximumSizeHigh, dwMaximumSizeLow, lpwName);

  If (Result <> INVALID_HANDLE_VALUE) and bFutherProcessing Then Begin
    Current := Mapping;
    Repeat
      For i := 0 to Current.Size - 1 do if Current.Mapping[i].FileHandle = hFile then Begin
        Current.Mapping[i].MappingHandle := Result;
        bFutherProcessing := False;
        Break;
      End;
      if not bFutherProcessing then Break;
      Current := Current.Next;
    Until Current = nil;
  End;
End;

function MapViewOfFileHook(ACaller:Cardinal; hFileMappingObject: THandle; dwDesiredAccess: DWORD;
  dwFileOffsetHigh, dwFileOffsetLow, dwNumberOfBytesToMap: DWORD): Pointer; stdcall;
type
  RIndexRec = packed record
    Lookup: Cardinal;
    Size: Cardinal;
    Extra: Cardinal;
  end;
  PIndexRec = ^RIndexRec;
var
  i: Byte;
  Current: PMappingPage;
  bFutherProcessing: Boolean;
Begin
  EnterCriticalSection(MapViewOfFileLock);
  if not IsAddressFromExecutable(ACaller) then Begin
    THooker.Hooker.TrueAPI;
    Result := MapViewOfFile(hFileMappingObject, dwDesiredAccess, dwFileOffsetHigh, dwFileOffsetLow, dwNumberOfBytesToMap);
    THooker.Hooker.TrueAPIEnd;
    LeaveCriticalSection(MapViewOfFileLock);
    Exit;
  End;

  bFutherProcessing := False;
  If Mapping <> nil then Begin
    Current := Mapping;
    Repeat
      for i := 0 to Current.Size - 1 do if Current.Mapping[i].MappingHandle = hFileMappingObject then Begin
        dwDesiredAccess := FILE_MAP_ALL_ACCESS;
        bFutherProcessing := True;
        Break;
      End;
      if bFutherProcessing then Break;
      Current := Current.Next;
    Until Current = nil;
  End;

  THooker.Hooker.TrueAPI;
  Result := MapViewOfFile(hFileMappingObject, dwDesiredAccess, dwFileOffsetHigh, dwFileOffsetLow, dwNumberOfBytesToMap);
  THooker.Hooker.TrueAPIEnd;
  if Result = nil then Begin
    i := GetLastError;
    WriteLn('GLE = ', i);
  End;


  if (Result <> nil) AND (bFutherProcessing) then begin
    Current := Mapping;
    Repeat
      for i := 0 to Current.Size - 1 do if Current.Mapping[i].MappingHandle = hFileMappingObject then Begin
        Current.Mapping[i].MappingPointer := Result;
        bFutherProcessing := False;
        Break;
      End;
      if not bFutherProcessing then Break;
      Current := Current.Next;
    Until Current = nil;
  end;
  LeaveCriticalSection(MapViewOfFileLock);
End;

procedure AskForMulMapping; stdcall;
Begin
  if THooker.Hooker.Injected then Exit;

  Mapping := nil;
  LastMappingPage := nil;

  THooker.Hooker.HookFunction(@CreateFileAHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'CreateFileA'), TAddCallerFunctionHooker);
  THooker.Hooker.HookFunction(@CloseHandleHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'CloseHandle'), TAddCallerFunctionHooker);
  THooker.Hooker.HookFunction(@MapViewOfFileHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'MapViewOfFile'), TAddCallerFunctionHooker);
  THooker.Hooker.HookFunction(@CreateFileMappingAHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'CreateFileMappingA'), TAddCallerFunctionHooker);
  THooker.Hooker.InjectIt;

  InitializeCriticalSection(CloseHandleLock);
  InitializeCriticalSection(MapViewOfFileLock);
End;

procedure FreeMulMapping;
Var
  Current, Next: PMappingPage;
  i: Integer;
Begin
  if Mapping = nil then Exit;
  THooker.Hooker.Free;
  Current := Mapping;
  repeat
    Next := Current;
    For i := 0 to Current.Size do FreeMemory(Current.Mapping[i].FileName);
    FreeMemory(Current);
  until Next = nil ;
End;

function GetMulMappingInfo(AMulName:PAnsiChar):PMappingRec; stdcall;
var
  Current:PMappingPage;
  i: Cardinal;
  UCMulName: AnsiString;
Begin
  Result := nil;
  if Mapping = nil then Exit;

  SetLength(UCMulName, Length(AMulName));
  CopyMemory(@UCMulName[1], AMulName, Length(AMulName));
  UCMulName := UpperCase(UCMulName);
  Current := Mapping;
  Repeat
    for i := 0 to Current.Size - 1 do if UCMulName = Current.Mapping[i].FileName then Begin
      Result := @Current.Mapping[i];
      Exit;
    End;
    Current := Current.Next;
  Until Current = nil;


End;

function EnshureFreeMappedSpace(AMulName: PAnsiChar; Amount: Cardinal):Boolean; stdcall;
var
  i:integer;
  bFound: Boolean;
  Current: PMappingPage;
  UpString: AnsiString;
Begin
  SetLength(UpString, Length(AMulName));
  CopyMemory(@UpString[1], AMulName, Length(AMulName));
  UpString := UpperCase(UpString);
  bFound := False;
  Result := True;
  if LastMappingPage <> nil then Begin
    Current := Mapping;
    repeat
      for i := 0 to Mapping.Size - 1 do if (Current.Mapping[i].MappingPointer <> nil) AND (Current.Mapping[i].FileName = UpString) then Begin
        if Current.Mapping[i].MappingPointer <> nil then Begin
          Result := False;
          Exit;
        End;
        Current.Mapping[i].AskedFreeSpace := Amount;
        bFound := True;
        Break;
      End;
      Current := Current.Next;
    until Current = nil;
    if not bFound then for i := 0 to LastMappingPage^.Size - 1 do if LastMappingPage^.Mapping[i].MappingPointer = nil then Begin
      LastMappingPage^.Mapping[i].FileName := GetMemory(Length(UpString) + 1);
      ZeroMemory(LastMappingPage^.Mapping[i].FileName, Length(UpString) + 1);
      CopyMemory(LastMappingPage^.Mapping[i].FileName, @UpString[1], Length(UpString));
      LastMappingPage^.Mapping[i].AskedFreeSpace := Amount;
      bFound := True;
      Break;
    End;
  End;
  if not bFound then Begin
    if LastMappingPage <> nil then Begin
      LastMappingPage.Next := GetMemory(SizeOf(TMappingPage) + SizeOf(TMappingRec)*9);
      LastMappingPage := LastMappingPage.Next;
    End Else Begin
      LastMappingPage := GetMemory(SizeOf(TMappingPage) + SizeOf(TMappingRec)*9);
      Mapping := LastMappingPage;
    End;
    LastMappingPage.Size := 10;
    for i := 0 to 9 do Begin
      LastMappingPage.Mapping[i].MappingPointer := nil;
      LastMappingPage.Mapping[i].AskedFreeSpace := 0;
    End;

    LastMappingPage^.Mapping[0].FileName := GetMemory(Length(UpString) + 1);
    ZeroMemory(LastMappingPage^.Mapping[0].FileName, Length(UpString) + 1);
    CopyMemory(LastMappingPage^.Mapping[0].FileName, @UpString[1], Length(UpString));
    LastMappingPage^.Mapping[0].AskedFreeSpace := Amount;
  End;

End;


initialization
  Mapping := nil;
end.
