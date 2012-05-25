program UOExt.gui.Test;

{$APPTYPE CONSOLE}

uses
  Windows;

type
  TSimpleProc = procedure; stdcall;
  TGUISetLog = function(LineHandle, ParentHandle: Cardinal; Data: PAnsiChar): Cardinal; stdcall;
  TGUIStartProcess = function(LineHandle, ParentHandle: Cardinal; ProcessLabel: PAnsiChar; Min, Max, Current: Cardinal): Cardinal; stdcall;
  TGUIUpdateProcess = procedure(ProcessHandle, Min, Max, Current: Cardinal); stdcall;
var
  hLib: THandle;
  SetLog: TGUISetLog;
  StartProc: TGUIStartProcess;
  UpdateProc: TGUIUpdateProcess;

  Root: Cardinal;
  SubText: Cardinal;
  SubProcess, SubProcess2: Cardinal;
  Process: Cardinal;
  i: Cardinal;
begin
  hLib := LoadLibraryA('UOExt.GUI.dll');
  TSimpleProc(GetProcAddress(hLib, 'Init'))();

  @SetLog := GetProcAddress(hLib, 'SetLog');
  @StartProc := GetProcAddress(hLib, 'StartProcess');
  @UpdateProc := GetProcAddress(hLib, 'UpdateProcess');

  Root := SetLog($FFFFFFFF, $FFFFFFFF, 'Initialized');
  Sleep(1000);
  SetLog($FFFFFFFF, Root, 'Sub Initialized');
  Sleep(1000);
  Process := StartProc($FFFFFFFF, $FFFFFFFF, 'Process', 0, 200, 0);
  SubProcess := StartProc($FFFFFFFF, Process, 'Sub Process', 0, 100, 0);
  SubProcess2 := StartProc($FFFFFFFF, Process, 'Sub Process', 0, 100, 0);
  for i := 0 to 100 do Begin
    Sleep(Random(10)+ 1);
    UpdateProc(SubProcess, 0, 100, i);
    UpdateProc(Process, 0, 200, i);
  End;
  for i := 0 to 100 do Begin
    Sleep(Random(10)+ 1);
    UpdateProc(SubProcess2, 0, 100, i);
    UpdateProc(Process, 0, 200, 100 + i);
  End;
  Sleep(5000);
  TSimpleProc(GetProcAddress(hLib, 'Free'))();
end.
