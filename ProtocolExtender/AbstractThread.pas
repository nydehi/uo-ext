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
    function Execute:Integer; virtual; abstract;
  public
    property Handle:THandle read FHandle;
    property ThreadId:THandle read FThreadId;
    property Running:Boolean read FRunning;
    procedure Stop;
    procedure ForceStop;
    procedure Run;
  end;

implementation

function ThreadProc(Who:TAbstractThread):Integer;
begin
  Result:=Who.MainProc;
end;

procedure TAbstractThread.Run;
begin
  FRunning:=True;
  FNeedExit:=False;
  FHandle:=System.BeginThread(nil, 0, @ThreadProc, Pointer(Self), 0, FThreadId);
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

end.
