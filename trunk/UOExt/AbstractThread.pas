unit AbstractThread;

interface

uses Windows;

type
  TAbstractThread=class
  private
    FHandle:THandle;
    FThreadId:THandle;
    FRunning:Boolean;
    function MainProc:Integer;
  protected
    FNeedExit:Boolean;
    class var FThreads:Array of TAbstractThread;
    class var FThreadCount: Cardinal;
    function Execute:Integer; virtual; abstract;

  public
    property Handle:THandle read FHandle;
    property ThreadId:THandle read FThreadId;
    property Running:Boolean read FRunning;
    procedure Stop; virtual;
    procedure ForceStop;
    procedure Run;

    class procedure AllStop;

    destructor Destory;
  end;

implementation

function ThreadProc(Who:TAbstractThread):Integer;
begin
  Result:=Who.MainProc;
end;

destructor TAbstractThread.Destory;
begin
  if FRunning then Stop;
  If WaitForSingleObject(FHandle, 1000) = WAIT_TIMEOUT Then
    if FRunning then ForceStop;
  CloseHandle(FHandle);
  inherited;
end;

procedure TAbstractThread.Run;
begin
  FRunning:=True;
  FNeedExit:=False;
  FHandle:=System.BeginThread(nil, 0, @ThreadProc, Pointer(Self), 0, FThreadId);
  SetLength(FThreads, FThreadCount + 1);
  FThreads[FThreadCount] := Self;
  FThreadCount := FThreadCount + 1;
end;

function TAbstractThread.MainProc:Integer;
begin
  Result:=Execute;
  FRunning:=False;
end;

procedure TAbstractThread.Stop;
begin
  FNeedExit:=True;
end;

procedure TAbstractThread.ForceStop;
begin
  TerminateThread(FHandle, 0);
end;

class procedure TAbstractThread.AllStop;
var
  i: Cardinal;
  AllDone: Boolean;
  ForceExit: Cardinal;
begin
  For i := 0 to FThreadCount - 1 do FThreads[i].Stop;

  ForceExit := GetTickCount + 1000;

  Repeat
    Sleep(1);
    AllDone := True;
    For i := 0 to FThreadCount - 1 do if (FThreads[i].Running) then AllDone := False;
  Until AllDone or (GetTickCount() >= ForceExit);

  if not AllDone then For i := 0 to FThreadCount - 1 do FThreads[i].ForceStop;

  SetLength(FThreads, 0);
  FThreadCount := 0;
end;

initialization
  TAbstractThread.FThreadCount := 0;
end.
