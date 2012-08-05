unit GUI;

interface

uses Windows, PluginsShared;

type
  TGUI = class
  private
    FLib: THandle;

    FSetLog: TGUISetLog;
    FStartProcess: TGUIStartProcess;
    FUpdateProcess: TGUIUpdateProcess;
    FFree: Pointer;
    FPath: AnsiString;
  public
    property Lib: THandle read FLib;
    property SetLog: TGUISetLog read FSetLog;
    property StartProcess: TGUIStartProcess read FStartProcess;
    property UpdateProcess: TGUIUpdateProcess read FUpdateProcess;
    function Init(Path: AnsiString):Boolean;
    procedure Replace;
    destructor Destroy; override;
  end;

var
  CurrGUI: TGUI;

function GUISetLog(LineHandle: LongWord; ParentHandle: Cardinal; Data: PAnsiChar): Cardinal; stdcall;
function GUIStartProcess(LineHandle, ParentHandle: Cardinal; ProcessLabel: PAnsiChar; Min, Max, Current: Cardinal): Cardinal; stdcall;
procedure GUIUpdateProcess(ProcessHandle, Min, Max, Current: Cardinal); stdcall;

implementation

function GUISetLog(LineHandle: Cardinal; ParentHandle: Cardinal; Data: PAnsiChar): Cardinal; stdcall;
Begin
  if Assigned(CurrGUI) and (CurrGUI.Lib <> 0) then
    Result := CurrGUI.SetLog(LineHandle, ParentHandle, Data)
  else
    Result := 0;
End;

function GUIStartProcess(LineHandle, ParentHandle: Cardinal; ProcessLabel: PAnsiChar; Min, Max, Current: Cardinal): Cardinal; stdcall;
Begin
  if Assigned(CurrGUI) and (CurrGUI.Lib <> 0) then
    Result := CurrGUI.StartProcess(LineHandle, ParentHandle, ProcessLabel, Min, Max, Current)
  else
    Result := 0;
End;

procedure GUIUpdateProcess(ProcessHandle, Min, Max, Current: Cardinal); stdcall;
Begin
  if Assigned(CurrGUI) and (CurrGUI.Lib <> 0) then
    CurrGUI.UpdateProcess(ProcessHandle, Min, Max, Current);
End;

function TGUI.Init(Path: AnsiString):Boolean;
type
  TInit = procedure; stdcall;
var
  Init: TInit;
begin
  Result := False;
  Path := Path + #0;
  FPath := Path;
  FLib := LoadLibraryA(@Path[1]);
  if FLib = 0 then Exit;

  @Init := GetProcAddress(FLib, 'Init');
  if not Assigned(Init) then Exit;

  @FSetLog := GetProcAddress(FLib, 'SetLog');
  if not Assigned(FSetLog) then Exit;

  @FStartProcess := GetProcAddress(FLib, 'StartProcess');
  if not Assigned(FStartProcess) then Exit;

  @FUpdateProcess := GetProcAddress(FLib, 'UpdateProcess');
  if not Assigned(FUpdateProcess) then Exit;

  FFree := GetProcAddress(Flib, 'Free');

  Init;
  Result := True;
end;

procedure TGUI.Replace;
type
  TFree = procedure; stdcall;
var
  newPath: AnsiString;
begin
  newPath := Copy(FPath, 1, Length(FPath) - 1) + '.old' + #0;
  MoveFileA(@FPath[1], @newPath[1]);
  newPath := Copy(FPath, 1, Length(FPath) - 1) + '.new' + #0;
  MoveFileA(@newPath[1], @FPath[1]);
  newPath := Copy(FPath, 1, Length(FPath) - 1);
end;

Destructor TGUI.Destroy;
type
  TFree = procedure; stdcall;

begin
  if FLib <> 0 then Begin
    If Assigned(FFree) Then Begin
      TFree(FFree)();
      FreeLibrary(FLib);
    End;
  End;
  Inherited;
end;

end.
