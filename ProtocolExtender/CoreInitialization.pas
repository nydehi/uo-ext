unit CoreInitialization;

interface

procedure CoreInitialize; stdcall;

implementation

uses Windows, HookLogic, Plugins, TLHelp32, Common;

{$IFDEF DEBUG}
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
type
  TSimpleProc= procedure;
Begin
  {$IFDEF DEBUG}
  AllocConsole;
  TTextRec(Output).Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  TTextRec(ErrOutput).Handle := GetStdHandle(STD_ERROR_HANDLE);
  SetConsoleCtrlHandler(@HandlerRoutine, True);
  oldDllProc := DllProc;
  DllProc := @Terminator;
  {$ENDIF}
  HookIt;
  {$IFDEF DEBUG}
  WriteLn('Hook completed. Initializing plugins.');
  {$ENDIF}
  PluginSystem.Initialize;
  {$IFDEF DEBUG}
  WriteLn('Initializing done.');
  {$ENDIF}
End;

procedure CoreInitialize; stdcall;
Begin
  InProcess;
End;


end.
