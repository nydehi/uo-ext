unit CoreInitialization;

interface

function CoreInitialize:Boolean; stdcall;
//procedure CoreInitialize; stdcall;

implementation

uses Windows, HookLogic, Plugins, Common, UOExtProtocol, WinSock, ShardSetup, zLib;

{$IFDEF DEBUG}
  {$DEFINE DEBUGWINDOW}
{$ENDIF}

{$IFDEF DEBUGWINDOW}
function HandlerRoutine(dwCtrlType: cardinal): bool; stdcall;
begin
  case dwCtrlType of
    CTRL_CLOSE_EVENT: begin
      FreeConsole;
      Result := True;
    end;
    else Result := false;
  end;//of case
end;

var
  oldDllProc : TDLLProc;

procedure Terminator(Reason: Integer);
begin
  If Reason = DLL_PROCESS_DETACH Then
    FreeConsole;
  If Assigned(oldDllProc) Then oldDllProc(Reason);
end;
{$ENDIF}

function InProcess:Boolean;
Begin
  Result := True;
  {$IFDEF DEBUGWINDOW}
  If not AllocConsole Then Begin
    MessageBoxA(0, PAnsiChar(IntToStr(GetLastError)), nil, MB_OK);
    Exit;
  End;
  TTextRec(Output).Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  TTextRec(ErrOutput).Handle := GetStdHandle(STD_ERROR_HANDLE);
  SetConsoleCtrlHandler(@HandlerRoutine, True);
  oldDllProc := DllProc;
  DllProc := @Terminator;
  WriteLn('UOExt.dll Ultima Online (C) protocol and client patch system.');
  WriteLn('Core: Debug window started.');
  WriteLn;
  Write('Compile time directives: DEBUGWINDOW');
  {$IFDEF DEBUG} Write(', DEBUG'); {$ENDIF}
  {$IFDEF RELEASE} Write(', RELEASE');{$ENDIF}
  {$IFDEF WRITELOG} Write(', WRITELOG');{$ENDIF}
  Writeln;

  Write('Plugins supply methods: Internal');
  {$IFDEF PLUGINS_HDD} Write(', HDD');{$ENDIF}
  {$IFDEF PLUGINS_SERVER} Write(', Server');{$ENDIF}
  Writeln;
  Writeln;
  WriteLn('Asking server for UOExt support.');
  {$ENDIF}
  if UOExtProtocol.GetUOExtSupport() Then Begin
    {$IFDEF DEBUG}
    WriteLn('UOExt supported. Config:');
    Write(' Update server: ', inet_ntoa(in_addr(ShardSetup.UpdateIP)), ':', htons(ShardSetup.UpdatePort));
    If ShardSetup.PersistentConnect then WriteLn(' persistent') else WriteLn;
    Write(' Server side protocol is ');
    if ShardSetup.Encrypted then WriteLn('encrypted') else WriteLn('unencrypted');
    WriteLn(' UOExt protocol encapsulation header: ', IntToHex(ShardSetup.InternalProtocolHeader, 2));
    {$ENDIF}
  End Else Begin
   {$IFDEF DEBUG}
   WriteLn('UOExt is not supported on this server. Gracefull exit.');
   Sleep(1000);
   {$ENDIF}
   {$IFDEF DEBUGWINDOW}
   FreeConsole;
   {$ENDIF}
   Result := False;
   Exit;
  End;
  HookIt;
  {$IFDEF DEBUG}
  WriteLn('Hook completed. Initializing plugins.');
  {$ENDIF}
  TPluginSystem.Instance.Initialize;
End;


function CoreInitialize:Boolean; stdcall;
Begin
  Result := InProcess;
End;

{
procedure CoreInitialize; stdcall;
Begin
  InProcess;
End;
}

end.
