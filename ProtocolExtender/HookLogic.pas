unit HookLogic;

interface

uses Windows, APIHooker, ShardSetup, ListeningThread;
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

  Hooker.TrueAPI(@CreateFileAHook);
  Result:=CreateFileA(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
  Hooker.TrueAPIEnd(@CreateFileAHook);

  If s = 'LOGIN.CFG' Then Begin
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
  End;
end;

procedure HookIt;
begin
  Hooker.HookFunction(@CreateFileAHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'CreateFileA'));
  Hooker.HookFunction(@ReadFileHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'ReadFile'));
  Hooker.HookFunction(@CloseHandleHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'CloseHandle'));
  Hooker.InjectIt;
end;

end.
