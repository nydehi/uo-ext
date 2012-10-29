unit PreConnectIPDiscover;

interface

function GetConnInfo(var IP: Cardinal; var Port: Word): Boolean;
function GetTransServPort: Word;

implementation

uses Windows, Common, WinSock2, TLHelp32, ShardSetup;

Procedure UpString(Start: PAnsiChar);
Var
  cChar: PAnsiChar;
Begin
  cChar := Start;
  while cChar^ <> #0 do Begin
    if (cChar^ >= 'a') and (cChar^ <= 'z') then cChar^ := AnsiChar(PByte(cChar)^ + (Ord('A') - Ord('a')));
    cChar := PAnsiChar(Cardinal(cChar) + 1);
  End;
End;

Function CompareStrings(Source: PAnsiChar; ToCompare: AnsiString): Boolean;
var
  cChar: PAnsiChar;
  i: Cardinal;
Begin
  Result := False;
  cChar := Source;
  for i := 1 to Length(ToCompare) - 1 do Begin
    if PAnsiChar(Cardinal(cChar) + i - 1)^ <> ToCompare[i] then Exit;
  End;
  Result := True;
End;

Function FindInStrU(Source, Find: PAnsiChar):Boolean;
var
  cSource, cFind: PAnsiChar;
  cChar: PAnsiChar;
  i, j: Cardinal;
  bFound: Boolean;
Begin
  Result := False;

  cSource := GetMemory(Length(Source) + 1);
  cFind := GetMemory(Length(Find) + 1);
  CopyMemory(cSource, Source, Length(Source) + 1);
  CopyMemory(cFind, Find, Length(Find) + 1);
  UpString(cSource);
  UpString(cFind);

  cChar := cSource;
  For i := 1 to Length(cSource) do Begin
    If PAnsiChar(Cardinal(cChar) + i - 1)^ = cFind^ then Begin
      bFound := True;
      for j := 0 to Length(cFind) do if PAnsiChar(Cardinal(cChar) + i - 1 + j)^ <> cFind[j] then Begin
        bFound := False;
        Break;
      End;
      if bFound then Begin
        Result := True;
        FreeMemory(cSource);
        FreeMemory(cFind);
        Exit;
      End;
    End;
  End;
  FreeMemory(cSource);
  FreeMemory(cFind);
End;

function GetLoginCfgConnInfo(var IP: Cardinal; var Port: Word): Boolean;
var
  F:File;
  fContent: Pointer;
  fCLength: Cardinal;
  cStartChar, cCurrentChar: PAnsiChar;
  sFileName: AnsiString;

Begin
  Result := False;

  sFileName := ShardSetup.UOExtBasePath + 'Login.cfg';
  AssignFile(F, String(sFileName));
  Reset(F, 1);
  fContent := GetMemory(FileSize(F));
  BlockRead(F, fContent^, FileSize(F), fCLength);
  CloseFile(F);
  cCurrentChar := fContent;
  repeat
    if (cCurrentChar^ = ';') then Begin
      repeat
        cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 1);
      until cCurrentChar^ = #10;
    End Else Begin
      UpString(cCurrentChar);
      if CompareStrings(cCurrentChar, 'LOGINSERVER=') then Begin
        cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 12);
        Break;
      End;
    End;
    cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 1);
  until Cardinal(cCurrentChar) >= (Cardinal(fContent) + fCLength);
  FreeMemory(fContent);
  if Cardinal(cCurrentChar) >= (Cardinal(fContent) + fCLength) then Exit;


  cStartChar := cCurrentChar;
  repeat
    cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 1);
  until cCurrentChar^ = ',';
  SetLength(sFileName, Cardinal(cCurrentChar) - Cardinal(cStartChar));
  CopyMemory(@sFileName[1], cStartChar, Cardinal(cCurrentChar) - Cardinal(cStartChar));
  sFileName := sFileName + #0;
  IP := htonl(inet_addr(@sFileName[1]));

  cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 1);
  cStartChar := cCurrentChar;
  repeat
    cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 1);
  until (cCurrentChar^ = ' ') or (cCurrentChar^ = #9) or (cCurrentChar^ = #10) or (cCurrentChar^ = #13) or (cCurrentChar^ = ';');

  SetLength(sFileName, Cardinal(cCurrentChar) - Cardinal(cStartChar));
  CopyMemory(@sFileName[1], cStartChar, Cardinal(cCurrentChar) - Cardinal(cStartChar));
  sFileName := sFileName;
  Port := StrToInt(sFileName);

  Result := True;
End;

function GetRazorConnInfo(var IP: Cardinal; var Port: Word): Boolean;
Const
  BYTESINCREMENT = 4096;
Var
  hOpendedKey: HKEY;
  AIP: PAnsiChar;
  AIPSize: Cardinal;
  QueryResult: LongInt;
Begin
  Result := False;

  If RegOpenKeyExA(HKEY_CURRENT_USER, 'SOFTWARE\Razor', 0, KEY_READ, hOpendedKey) <> ERROR_SUCCESS Then Exit;
  AIPSize := 16;
  AIP := Getmemory(AIPSize);
  QueryResult := RegQueryValueExA(hOpendedKey, 'LastServer', nil, nil, PByte(AIP), @AIPSize);
  If QueryResult <> ERROR_SUCCESS Then Begin
    RegCloseKey(hOpendedKey);
    FreeMemory(AIP);
    Exit;
  End;
  IP := htonl(inet_addr(AIP));

  QueryResult := RegQueryValueExA(hOpendedKey, 'LastPort', nil, nil, PByte(AIP), @AIPSize);
  If QueryResult <> ERROR_SUCCESS Then Begin
    RegCloseKey(hOpendedKey);
    FreeMemory(AIP);
    Exit;
  End;
  Port := StrToInt(AIP);
  RegCloseKey(hOpendedKey);
  FreeMemory(AIP);
  Result := True;
End;

function GetModuleFileNameExA(hProcess: THandle; hModule: HMODULE; lpFilename: PAnsiChar; nSize: DWORD): DWORD; stdcall; external 'psapi.dll' name 'GetModuleFileNameExA';

function ValidateRazor(ProcessId: THandle):Boolean;
type
  TLangInfo = packed record
    wLanguage: Word;
    wCodePage: Word;
  end;
var
  ExeName : Array [0..MAX_PATH] of AnsiChar;
  ExeNameLength, cTmp, cFVLen, cFileVersionLen: Cardinal;
  pFileVersion, pFileInfo: Pointer;

  ProcHandle: THandle;
  sResultURL: AnsiString;
Begin
  Result := False;

  ProcHandle := OpenProcess(PROCESS_QUERY_INFORMATION XOR PROCESS_VM_READ, False, ProcessId);
  if ProcHandle = INVALID_HANDLE_VALUE then begin
    CloseHandle(ProcHandle);
    Exit;
  End;
  ExeNameLength := GetModuleFileNameExA(ProcHandle, 0, @ExeName[0], MAX_PATH + 1);
  CloseHandle(ProcHandle);

  If ExeNameLength = 0 Then Exit;
  cFVLen := GetFileVersionInfoSizeA(@ExeName[0], cTmp);
  pFileVersion := GetMemory(cFVLen);
  GetFileVersionInfoA(@ExeName[0], 0, cFVLen, pFileVersion);

  VerQueryValueA(pFileVersion, '\VarFileInfo\Translation', pFileInfo, cFileVersionLen);
  sResultURL := '\StringFileInfo\'+IntToHex(TLangInfo(pFileInfo^).wLanguage, 4)+IntToHex(TLangInfo(pFileInfo^).wCodePage, 4)+'\ProductName' + #0;
  If VerQueryValueA(pFileVersion, @sResultURL[1], pFileInfo, cFileVersionLen) = False Then Begin
    Exit;
  End;

  Result := CompareStrings(pFileInfo, 'Razor');

  FreeMemory(pFileVersion);
End;

function GetTransServPort: Word;
var
  F:File;
  fContent: Pointer;
  fCLength: Cardinal;
  cStartChar, cCurrentChar: PAnsiChar;
  sFileName: AnsiString;

Begin
  Result := 0;

  sFileName := ShardSetup.UOExtBasePath + 'uo.cfg';
  AssignFile(F, String(sFileName));
  Reset(F, 1);
  fContent := GetMemory(FileSize(F));
  BlockRead(F, fContent^, FileSize(F), fCLength);
  CloseFile(F);
  cCurrentChar := fContent;
  repeat
    if (cCurrentChar^ = ';') then Begin
      repeat
        cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 1);
      until cCurrentChar^ = #10;
    End Else Begin
      UpString(cCurrentChar);
      if CompareStrings(cCurrentChar, 'TRANSLATIONSERVERIPADDRESS=') then Begin
        cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 12);
        Break;
      End;
    End;
    cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 1);
  until Cardinal(cCurrentChar) >= (Cardinal(fContent) + fCLength);
  FreeMemory(fContent);
  if Cardinal(cCurrentChar) >= (Cardinal(fContent) + fCLength) then Exit;

  repeat
    cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 1);
  until cCurrentChar^ = ',';

  cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 1);
  cStartChar := cCurrentChar;
  repeat
    cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 1);
  until (cCurrentChar^ = ' ') or (cCurrentChar^ = #9) or (cCurrentChar^ = #10) or (cCurrentChar^ = #13) or (cCurrentChar^ = ';');

  SetLength(sFileName, Cardinal(cCurrentChar) - Cardinal(cStartChar));
  CopyMemory(@sFileName[1], cStartChar, Cardinal(cCurrentChar) - Cardinal(cStartChar));
  sFileName := sFileName;
  Result := StrToInt(sFileName);
End;

function GetConnInfo(var IP: Cardinal; var Port: Word): Boolean;
var
  hPID, hPPID, hSnapshot: THandle;
  pe : TProcessEntry32A;
  bDone: Boolean;
Begin
  Result := False;
  hPID := GetCurrentProcessId;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnapshot = INVALID_HANDLE_VALUE then Exit;
  pe.dwSize := SizeOf(pe);
  if Process32FirstA(hSnapshot, pe) Then Repeat
    if pe.th32ProcessID = hPID then Begin
      hPPID := pe.th32ParentProcessID;
      bDone := False;
      if Process32FirstA(hSnapshot, pe) then Repeat
        if pe.th32ProcessID = hPPID then Begin

          if FindInStrU(pe.szExeFile, 'RAZOR.EXE') then Begin
            Result := GetRazorConnInfo(IP, Port);
            ShardSetup.Razor := True;
            bDone := True;
            Break;
          End Else If ValidateRazor(pe.th32ProcessID) then Begin
            Result := GetRazorConnInfo(IP, Port);
            ShardSetup.Razor := True;
            bDone := True;
            Break;
          End Else Begin
            Result := GetLoginCfgConnInfo(IP, Port);
            bDone := True;
            Break;
          End;
        End;
      Until not Process32NextA(hSnapshot, pe);
      if bDone then Break;
    End;
  Until not Process32NextA(hSnapshot, pe);
  CloseHandle(hSnapshot);
  If not Result Then Result := GetLoginCfgConnInfo(IP, Port);
  IP := htonl(IP);
  Port := htons(Port);
End;

end.
