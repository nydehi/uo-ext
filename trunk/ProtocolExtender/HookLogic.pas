unit HookLogic;

interface

uses Windows, APIHooker, ShardSetup, ListeningThread;

type
  RFileAccess = record
    hFile: THandle;
    hMap: THandle;
    pView: Pointer;
    cFileLength: Cardinal;
  End;

var
  StaticsX: Array [0..5] of RFileAccess;
  StaidxX: Array [0..5] of RFileAccess;

procedure HookIt;

implementation

uses Common, Plugins;




var
  hLoginCFG: THandle;
  cLoginCFGPos: Cardinal;
  LoginCFG: String;
  filledBytes: Cardinal;

function CreateFileAHook(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; stdcall;
var
  s:String;
  i:integer;
begin
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

  if ((Copy(s, 1, 7) = 'STATICS') and (s[8] >= '0') and (s[8] <= '5')) or
     ((Copy(s, 1, 6) = 'STAIDX') and (s[7] >= '0') and (s[7] <= '5')) Then Begin
//    dwShareMode := (FILE_SHARE_READ + FILE_SHARE_WRITE);
    dwDesiredAccess := GENERIC_READ + GENERIC_WRITE;
  End;
  If ((Copy(s, 1, 6) = 'STAIDX') and (s[7] >= '0') and (s[7] <= '5')) Then
    dwDesiredAccess := GENERIC_READ;

  Hooker.TrueAPI(@CreateFileAHook);
  Result:=CreateFileA(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
  Hooker.TrueAPIEnd(@CreateFileAHook);

  if (Copy(s, 1, 7) = 'STATICS') and (s[8] >= '0') and (s[8] <= '5') Then Begin
    StaticsX[StrToInt(s[8])].hFile := Result;
    StaticsX[StrToInt(s[8])].cFileLength :=  GetFileSize(Result, nil);
    {$IFDEF DEBUG}
//    WriteLn('HookLogic: Saving Statics', StrToInt(s[8]), '.mul (', s, ') file handle. Handle :=', Result);
    {$ENDIF}
  End Else If (Copy(s, 1, 6) = 'STAIDX') and (s[7] >= '0') and (s[7] <= '5') Then Begin
    StaidxX[StrToInt(s[7])].hFile := Result;
    StaidxX[StrToInt(s[7])].cFileLength :=  GetFileSize(Result, nil);
    {$IFDEF DEBUG}
//    WriteLn('HookLogic: Saving Staidx', StrToInt(s[7]), '.mul (', s, ') file handle. Handle :=', Result);
    {$ENDIF}
  End Else If s = 'LOGIN.CFG' Then Begin
    hLoginCFG := Result;
    cLoginCFGPos := 0;

    LoginCFG := 'LoginServer=127.0.0.1,' + IntToStr(RunListeningThread);
{$IFDEF DEBUG}
    WriteLn('HookLogic: Proxy server started.');
    WriteLn('HookLogic: Login.cfg:');
    WriteLn('HookLogic: ', LoginCFG);
{$ENDIF}
    Hooker.TrueAPI;
    PluginSystem.Initialize;
    Hooker.TrueAPIEnd;
    filledBytes := 0;
  end;
end;

function ReadFileHook(hFile: THandle; var Buffer: Pointer; nNumberOfBytesToRead: DWORD;
  var lpNumberOfBytesRead: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;
begin
  if hFile = hLoginCFG Then Begin
    Result := True;
    if (Length(LoginCFG) - cLoginCFGPos) > 0 Then
      lpNumberOfBytesRead := Length(LoginCFG) - cLoginCFGPos
    else Begin
      lpNumberOfBytesRead := 0;
      Exit;
    End;
    filledBytes := filledBytes + lpNumberOfBytesRead;
    CopyMemory(@Buffer, @LoginCFG[cLoginCFGPos + 1], lpNumberOfBytesRead);
    cLoginCFGPos := cLoginCFGPos + lpNumberOfBytesRead;
  End Else Begin
    Hooker.TrueAPI(@ReadFileHook);
    Result := ReadFile(hFile, Buffer, nNumberOfBytesToRead, lpNumberOfBytesRead, lpOverlapped);
    Hooker.TrueAPIEnd(@ReadFileHook);
  End;
end;

function CloseHandleHook(hObject: THandle): BOOL; stdcall;
var
  i: Integer;
begin
  Hooker.TrueAPI(@CloseHandleHook);
  Result := CloseHandle(hObject);
  Hooker.TrueAPIEnd(@CloseHandleHook);
  If Result Then Begin
    If hObject = hLoginCFG Then Begin
      hLoginCFG := INVALID_HANDLE_VALUE;
      cLoginCFGPos := 0;
      {$IFDEF DEBUG}
      WriteLn('HookLogic: Client filled with fake login.cfg');
      WriteLn('HookLogic: Filled ', filledBytes, ' of ', Length(LoginCFG));
      {$ENDIF}
    End;
    For i := 0 to 5 do Begin
      If hObject = StaticsX[i].hFile Then Begin
        StaticsX[i].hFile := INVALID_HANDLE_VALUE;
        {$IFDEF DEBUG}
//        WriteLn('HookLogic: Lost Statics', i, '.mul file handle.');
        {$ENDIF}
        Break;
      End;
      If hObject = StaIdxX[i].hFile Then Begin
        StaIdxX[i].hFile := INVALID_HANDLE_VALUE;
        {$IFDEF DEBUG}
//        WriteLn('HookLogic: Lost StaIdx', i, '.mul file handle.');
        {$ENDIF}
        Break;
      End;
      If hObject = StaticsX[i].hMap Then Begin
        StaticsX[i].hMap := INVALID_HANDLE_VALUE;
        {$IFDEF DEBUG}
//        WriteLn('HookLogic: Lost Statics', i, '.mul map handle.');
        {$ENDIF}
        Break;
      End;
      If hObject = StaIdxX[i].hMap Then Begin
        StaIdxX[i].hFile := INVALID_HANDLE_VALUE;
        {$IFDEF DEBUG}
//        WriteLn('HookLogic: Lost StaIdx', i, '.mul map handle.');
        {$ENDIF}
        Break;
      End;
    End;
  End;
end;

function CreateFileMappingAHook(hFile: THandle; lpFileMappingAttributes: PSecurityAttributes;
  flProtect, dwMaximumSizeHigh, dwMaximumSizeLow: DWORD; lpName: PAnsiChar): THandle; stdcall;
var
  i: Integer;
  bFutherProcessing: Boolean;
  lflP:DWORD;
Begin
  bFutherProcessing := False;
  If hFile <> INVALID_HANDLE_VALUE Then For i := 0 to 5 do Begin
    If (StaticsX[i].hFile = hFile) Then Begin
      lflP := flProtect;
      flProtect := {flProtect OR} PAGE_READWRITE;
      bFutherProcessing := True;
      Break;
    End Else If (StaidxX[i].hFile = hFile) Then Begin
      lflP := flProtect;
//      flProtect := {flProtect OR} PAGE_WRITECOPY;
      bFutherProcessing := True;
    End;
  End;
  Hooker.TrueAPI(@CreateFileMappingAHook);
  Result := CreateFileMappingA(hFile, lpFileMappingAttributes, flProtect, dwMaximumSizeHigh, dwMaximumSizeLow, lpName);
  Hooker.TrueAPIEnd(@CreateFileMappingAHook);
  If (Result <> INVALID_HANDLE_VALUE) and bFutherProcessing Then Begin
    For i := 0 to 5 do Begin
      If (StaticsX[i].hFile = hFile) Then Begin
        StaticsX[i].hMap := Result;
        {$IFDEF DEBUG}
//        WriteLn('HookLogic: Get Statics', i, '.mul map handle. Handle := ', Result, ' LastError := ', GetLastError, ' LflP := ', lflP);
        {$ENDIF}
        Break;
      End Else If (StaidxX[i].hFile = hFile) Then Begin
        StaidxX[i].hMap := Result;
        {$IFDEF DEBUG}
//        WriteLn('HookLogic: Get Staidx', i, '.mul map handle. Handle := ', Result, ' LastError := ', GetLastError, ' LflP := ', lflP);
        {$ENDIF}
        Break;
      End;
    End;
  End;
End;

function DuplicateHandleHook(hSourceProcessHandle, hSourceHandle, hTargetProcessHandle: THandle;
  lpTargetHandle: PHandle; dwDesiredAccess: DWORD;
  bInheritHandle: BOOL; dwOptions: DWORD): BOOL; stdcall;
begin
  Hooker.TrueAPI(@DuplicateHandleHook);
  Result := DuplicateHandle(hSourceProcessHandle, hSourceHandle, hTargetProcessHandle, lpTargetHandle, dwDesiredAccess, bInheritHandle, dwOptions);
  Hooker.TrueAPIEnd(@DuplicateHandleHook);
  {$IFDEF DEBUG}
//    WriteLn('HookLogic: DuplicateHandle for ' + IntToStr(hSourceHandle) + ' new handle: ', lpTargetHandle^);
  {$ENDIF}
End;

function MapViewOfFileHook(hFileMappingObject: THandle; dwDesiredAccess: DWORD;
  dwFileOffsetHigh, dwFileOffsetLow, dwNumberOfBytesToMap: DWORD): Pointer; stdcall;
type
  RIdxRec=packed record
    Start : Cardinal;
    Length : Cardinal;
    Extra : Cardinal;
  End;
  PIdxRec = ^RIdxRec;
var
  i, j: Integer;
  bFutherProcessing: Boolean;
  cIR : RIdxRec;
Begin
  bFutherProcessing := False;
  If hFileMappingObject <> INVALID_HANDLE_VALUE Then For i := 0 to 5 do Begin
    If (StaticsX[i].hMap = hFileMappingObject) Then Begin
      dwDesiredAccess := dwDesiredAccess OR FILE_MAP_WRITE;
      bFutherProcessing := True;
      Break;
    End Else If (StaidxX[i].hMap = hFileMappingObject) Then Begin
      dwDesiredAccess := {dwDesiredAccess OR} FILE_MAP_COPY;
      bFutherProcessing := True;
      cIR.Start := $FFFFFFFF;
      cIR.Length := 0;
      cIR.Extra := 0;
      Break;
    End;
  End;

  Hooker.TrueAPI(@MapViewOfFileHook);
  Result := MapViewOfFile(hFileMappingObject, dwDesiredAccess, dwFileOffsetHigh, dwFileOffsetLow, dwNumberOfBytesToMap);
  Hooker.TrueAPIEnd(@MapViewOfFileHook);

  If (Result <> nil) and bFutherProcessing Then Begin
    For i := 0 to 5 do Begin
      If (StaticsX[i].hMap = hFileMappingObject) Then Begin
        StaticsX[i].pView := Result;
        {$IFDEF DEBUG}
        WriteLn('HookLogic: Get Statics', i, '.mul data pointer. Pointer := ', Cardinal(Result), ' length := ', dwNumberOfBytesToMap, ' aLength := ', StaticsX[i].cFileLength);
        {$ENDIF}
        Break;
      End Else If (StaidxX[i].hMap = hFileMappingObject) Then Begin
        StaidxX[i].pView := Result;
        {$IFDEF DEBUG}
        WriteLn('HookLogic: Get Staidx', i, '.mul data pointer. Pointer := ', Cardinal(Result), ' length := ', dwNumberOfBytesToMap, ' aLength := ', StaIdxX[i].cFileLength);
        {$ENDIF}
        For j := 0 to (StaIdxX[i].cFileLength div SizeOf(RIdxRec) - 1) do Begin
          PIdxRec(Cardinal(Result) + j * SizeOf(RIdxRec)).Start := $FFFFFFFF;
//          CopyMemory(@cIR, Pointer(Cardinal(Result) + j * SizeOf(RIdxRec)), SizeOf(RIdxRec));
        End;
        {$IFDEF DEBUG}
        WriteLn('HookLogic: Filled last file with empty data ');
        {$ENDIF}
        Break;
      End;
    End;
  End;

End;

procedure HookIt;
begin
  Hooker.HookFunction(@CreateFileAHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'CreateFileA'));
  Hooker.HookFunction(@ReadFileHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'ReadFile'));
  Hooker.HookFunction(@CloseHandleHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'CloseHandle'));
  Hooker.HookFunction(@CreateFileMappingAHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'CreateFileMappingA'));
//  Hooker.HookFunction(@DuplicateHandleHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'DuplicateHandle'));
  Hooker.HookFunction(@MapViewOfFileHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'MapViewOfFile'));
  Hooker.InjectIt;
end;

var
  i: Integer;
initialization
  For i := 0 to 5 do Begin
    StaticsX[i].hFile := INVALID_HANDLE_VALUE;
    StaidxX[i].hFile := INVALID_HANDLE_VALUE;
    StaticsX[i].hMap := INVALID_HANDLE_VALUE;
    StaidxX[i].hMap := INVALID_HANDLE_VALUE;
  End;
end.
