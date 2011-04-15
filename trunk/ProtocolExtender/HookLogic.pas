unit HookLogic;

interface

uses Windows, APIHooker, ShardSetup, ListeningThread, PluginsShared;


procedure HookIt;

implementation

uses Common, Plugins;

type
  TMappedFileInfo=record
    FileName: PAnsiChar;
    WantedWorkMethod: Cardinal;

    Handle: THandle;
    Access: Cardinal;
    ShareMode: Cardinal;
    MappingHandle: THandle;
    MappingProtect: Cardinal;
    ClientDataMap: Pointer;
    DesiredAccess: Cardinal;
  end;

var
  hLoginCFG: THandle;
  cLoginCFGPos: Cardinal;
  LoginCFG: AnsiString;
  filledBytes: Cardinal;

function CreateFileAHook(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; stdcall;
var
  s:AnsiString;
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

  THooker.Hooker.TrueAPI;
  Result:=CreateFileA(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
  THooker.Hooker.TrueAPIEnd;

  If s = 'LOGIN.CFG' Then Begin
    hLoginCFG := Result;
    cLoginCFGPos := 0;

    LoginCFG := 'LoginServer=127.0.0.1,' + IntToStr(RunListeningThread);
    {$IFDEF DEBUG}
    WriteLn('HookLogic: Proxy server started.');
    WriteLn('HookLogic: Login.cfg:');
    WriteLn('HookLogic: ', LoginCFG);
    {$ENDIF}
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
    THooker.Hooker.TrueAPI;
    Result := ReadFile(hFile, Buffer, nNumberOfBytesToRead, lpNumberOfBytesRead, lpOverlapped);
    THooker.Hooker.TrueAPIEnd;
  End;
end;

function CloseHandleHook(hObject: THandle): BOOL; stdcall;
begin
  THooker.Hooker.TrueAPI;
  Result := CloseHandle(hObject);
  THooker.Hooker.TrueAPIEnd;
  If Boolean(Result) Then Begin
    If hObject = hLoginCFG Then Begin
      hLoginCFG := INVALID_HANDLE_VALUE;
      cLoginCFGPos := 0;
      {$IFDEF DEBUG}
      WriteLn('HookLogic: Client filled with fake login.cfg');
      WriteLn('HookLogic: Filled ', filledBytes, ' of ', Length(LoginCFG));
      {$ENDIF}
    End;
  End;
end;

procedure HookIt;
begin
  THooker.Hooker.HookFunction(@CreateFileAHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'CreateFileA'));
  THooker.Hooker.HookFunction(@ReadFileHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'ReadFile'));
  THooker.Hooker.HookFunction(@CloseHandleHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'CloseHandle'));
  THooker.Hooker.InjectIt;
end;

end.
