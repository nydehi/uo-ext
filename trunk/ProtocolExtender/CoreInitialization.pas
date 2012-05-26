unit CoreInitialization;

interface

function CoreInitialize:Boolean; stdcall;
//procedure CoreInitialize; stdcall;

implementation

uses Windows, HookLogic, Plugins, Common, UOExtProtocol, WinSock, ShardSetup, zLib,
  GUI, ProtocolDescription;

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
  Write('Core: Compile time directives: DEBUGWINDOW');
  {$IFDEF DEBUG} Write(', DEBUG'); {$ENDIF}
  {$IFDEF RELEASE} Write(', RELEASE');{$ENDIF}
  {$IFDEF WRITELOG} Write(', WRITELOG');{$ENDIF}
  WriteLn;
  {$ENDIF}

  ProtocolDescription.Init;

  {$IFDEF DEBUG}
  WriteLn('Core: Plugins supply methods: Internal, Server');
  WriteLn;
  WriteLn;
  WriteLn('Core: Asking server for UOExt support.');
  {$ENDIF}
  If TPluginSystem.Instance.GetDllsFromServer Then Begin
    {$IFDEF DEBUG}
    WriteLn('Core: UOExt supported. Config:');
    Write('Core:  Server side protocol is ');
    if ShardSetup.Encrypted then WriteLn('encrypted') else WriteLn('unencrypted');
    WriteLn('Core:  UOExt protocol encapsulation header: ', IntToHex(ShardSetup.InternalProtocolHeader, 2));
    {$ENDIF}
  End Else Begin
   {$IFDEF DEBUG}
   WriteLn('Core: UOExt is not supported on this server. Gracefull exit.');
   Sleep(1000);
   {$ENDIF}
   {$IFDEF DEBUGWINDOW}
   FreeConsole;
   {$ENDIF}
   Result := False;
   Exit;
  End;

  {$IFDEF DEBUG}
  Write('Core: Creating GUI screen ... ');
  {$ENDIF}
  GUI.CurrGUI := TGUI.Create;
  If not GUI.CurrGUI.Init(ShardSetup.GUIDLLName) Then Begin
    {$IFDEF DEBUG}
    WriteLn('library not found or type mismatch.');
    {$ENDIF}
    GUI.CurrGUI.Free;
    GUI.CurrGUI := nil;
  End Else Begin
    {$IFDEF DEBUG}
    WriteLn('done.');
    {$ENDIF}
  End;

  {$IFDEF DEBUG}
  Write('Core: Hooking APIs for launch ... ');
  {$ENDIF}
  HookIt;
  {$IFDEF DEBUG}
  WriteLn('done.');
  WriteLn('Core: Starting plug-ins loading.');
  {$ENDIF}
  TPluginSystem.Instance.Initialize;

  GUI.CurrGUI.Free;
  GUI.CurrGUI := nil;
End;

function CoreInitialize:Boolean; stdcall;
Begin
  Result := InProcess;
End;

end.
