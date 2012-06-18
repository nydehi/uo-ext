unit UOExt.GUI.API;

interface

  function SetLog(LineHandle: Cardinal; ParentHandle: Cardinal; Data: PAnsiChar): Cardinal; stdcall;
  function StartProcess(LineHandle, ParentHandle: Cardinal; ProcessLabel: PAnsiChar; Min, Max, Current: Cardinal): Cardinal; stdcall;
  procedure UpdateProcess(ProcessHandle, Min, Max, Current: Cardinal); stdcall;
  procedure Init; stdcall;
  procedure Free; stdcall;

implementation

uses Windows, GUIThread;

var
  GUI:TGUIThread;

procedure Init; stdcall;
Begin
  if not Assigned(GUI) then Begin
    GUI := TGUIThread.Create;
    GUI.Run;
    while not GUI.CanAcceptCalls do Sleep(1);
    
  End;
End;

procedure Free; stdcall;
var
  i: Integer;
Begin
  if Assigned(GUI) then Begin
    GUI.Stop;
    If WaitForSingleObject(GUI.Handle, 1000) = WAIT_TIMEOUT Then
      If GUI.Running Then GUI.ForceStop;
  End;
  GUI.Free;
  GUI := nil;
End;

function SetLog(LineHandle, ParentHandle: Cardinal; Data: PAnsiChar): Cardinal; stdcall;
Begin
  Result := 0;
  if not Assigned(GUI) then Exit;
  Result := GUI.SetLog(LineHandle, ParentHandle, Data);
End;

function StartProcess(LineHandle, ParentHandle: Cardinal; ProcessLabel: PAnsiChar; Min, Max, Current: Cardinal): Cardinal; stdcall;
Begin
  Result := 0;
  if not Assigned(GUI) then Exit;
  Result := GUI.StartProgress(LineHandle, ParentHandle, ProcessLabel, Min, Max, Current);
End;

procedure UpdateProcess(ProcessHandle, Min, Max, Current: Cardinal); stdcall;
Begin
  if not Assigned(GUI) then Exit;
  GUI.UpdateProgress(ProcessHandle, Min, Max, Current);
End;

initialization
  GUI := nil;
end.
