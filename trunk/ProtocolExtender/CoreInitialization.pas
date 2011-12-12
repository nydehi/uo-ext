unit CoreInitialization;

interface

procedure CoreInitialize; stdcall;

implementation

uses Windows, HookLogic, Plugins, TLHelp32, Common, LoginConfReader;

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

procedure InProcess;
Begin
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
  {$ENDIF}
  HookIt;
  {$IFDEF DEBUG}
  WriteLn('Hook completed. Initializing plugins.');
  {$ENDIF}
  LoginConfReader.TryReadLoginCfg;
  TPluginSystem.Instance.Initialize;
End;

procedure CoreInitialize; stdcall;
Begin
  InProcess;
End;


end.
