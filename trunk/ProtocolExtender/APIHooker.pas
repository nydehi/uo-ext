unit APIHooker;

interface

uses Windows;

type

  RInjectCode = packed record
    PuhsOp: byte;
    PushArg: pointer;
    RetOp: byte;
  end;

  ROldCode = packed record
    One: dword;
    two: word;
  end;

//  TFunctionHooker=class;
{  private
    FHooked: Boolean;
    FTrueSetted: Boolean;
    FOldData: ROldCode;
    FInject: RInjectCode;
    FLock: TRTLCriticalSection;
    FOriginalFunction: Pointer;
  protected
    procedure Inject;
    procedure BeginTrueFunction;
    procedure EndTrueFunction;
  public
    constructor Create(AInjectFunction, AOriginalFunction: Pointer);
    destructor Destroy; override;
  end;}

  PHookRecord=^THookRecord;
  THookRecord=record
    AHooks: Array [0..9] of TObject;
    ANext: PHookRecord;
  end;

  THooker=class
  private
    FHooks:PHookRecord;
  public
    procedure HookFunction(AInjectFunction, AOriginalFunction: Pointer);
    procedure InjectIt;
    procedure TrueAPI; overload;
    procedure TrueAPI(AFunction: Pointer); overload;
    procedure TrueAPIEnd; overload;
    procedure TrueAPIEnd(AFunction: Pointer); overload;

    constructor Create;
    destructor Destroy; override;
  end;

var
  Hooker : THooker;

implementation

uses TLHelp32, Common;

const
  THREAD_SUSPEND_RESUME = $0002;

type
  TFunctionHooker=class
  private
    FHooked: Boolean;
    FTrueSetted: Boolean;
    FOldData: ROldCode;
    FInject: RInjectCode;
    FLock: TRTLCriticalSection;
    FOriginalFunction: Pointer;
  protected
    procedure Inject;
    procedure BeginTrueFunction;
    procedure EndTrueFunction;
  public
    constructor Create(AInjectFunction, AOriginalFunction: Pointer);
    destructor Destroy; override;
  end;

Function OpenThread(dwDesiredAccess: dword; bInheritHandle: bool; dwThreadId: dword):dword;
                    stdcall; external 'kernel32.dll';

  // TFunctionHooker

constructor TFunctionHooker.Create(AInjectFunction, AOriginalFunction: Pointer);
begin
  Inherited Create;
  FInject.PuhsOp := $68;
  FInject.PushArg := AInjectFunction;
  FInject.RetOp := $C3;
  FHooked := False;
  FTrueSetted := False;
  FOriginalFunction := AOriginalFunction;
  InitializeCriticalSection(FLock);
end;

destructor TFunctionHooker.Destroy;
begin
  If FHooked Then Begin
    If not FTrueSetted Then Begin
      BeginTrueFunction;
      LeaveCriticalSection(FLock);
    End;
  End;
  DeleteCriticalSection(FLock);
  Inherited;
end;

procedure TFunctionHooker.Inject;
var
  Writen:Cardinal;
begin
  ReadProcessMemory(GetCurrentProcess, FOriginalFunction, @FOldData, SizeOf(FOldData), Writen);
  WriteProcessMemory(GetCurrentProcess, FOriginalFunction, @FInject, SizeOf(FInject), Writen);
end;

procedure TFunctionHooker.BeginTrueFunction;
var
  Writen:Cardinal;
begin
  EnterCriticalSection(FLock);
  WriteProcessMemory(GetCurrentProcess, FOriginalFunction, @FOldData, SizeOf(FOldData), Writen);
end;

procedure TFunctionHooker.EndTrueFunction;
var
  Writen:Cardinal;
begin
  ReadProcessMemory(GetCurrentProcess, FOriginalFunction, @FOldData, SizeOf(FOldData), Writen);
  WriteProcessMemory(GetCurrentProcess, FOriginalFunction, @FInject, SizeOf(FInject), Writen);
  LeaveCriticalSection(FLock);
end;
 // THooker

constructor THooker.Create;
begin
  Inherited;
  FHooks := nil;
end;

destructor THooker.Destroy;
var
  pCurrentRec, pLastRec: PHookRecord;
  i: Byte;
begin
  pCurrentRec := FHooks;
  while pCurrentRec <> nil do begin
    For i:= 0 to 9 do if Assigned(pCurrentRec^.AHooks[i]) Then Begin
     pCurrentRec^.AHooks[i].Free;
     pCurrentRec^.AHooks[i] := nil;
    End;
    pLastRec := pCurrentRec;
    pCurrentRec := pCurrentRec^.ANext;
    FreeMemory(pLastRec);
  end;
  Inherited;
end;

procedure THooker.HookFunction(AInjectFunction, AOriginalFunction: Pointer);
var
  pCurrentRec: PHookRecord;
  Sentinel:THookRecord;
  i: Byte;
  AHook: TFunctionHooker;
begin
  AHook := TFunctionHooker.Create(AInjectFunction, AOriginalFunction);
  If FHooks = nil Then Begin
    FHooks := GetMemory(SizeOf(THookRecord));
    ZeroMemory(FHooks, SizeOf(THookRecord));
  End;
  pCurrentRec := @Sentinel;
  Sentinel.ANext := FHooks;
  repeat
    pCurrentRec := pCurrentRec^.ANext;
    For i:=0 to 9 do If pCurrentRec^.AHooks[i] = nil Then Begin
      pCurrentRec^.AHooks[i] := AHook;
      Exit;
    End;
  Until pCurrentRec^.ANext = nil;
  pCurrentRec^.ANext := GetMemory(SizeOf(THookRecord));
  ZeroMemory(pCurrentRec^.ANext, SizeOf(THookRecord));
  pCurrentRec^.ANext^.AHooks[0] := AHook;
end;

procedure THooker.TrueAPI;
var
  pCurrentRec: PHookRecord;
  Sentinel:THookRecord;
  i: Byte;
begin
  If FHooks = nil Then Exit;
  pCurrentRec := @Sentinel;
  Sentinel.ANext := FHooks;
  repeat
    pCurrentRec := pCurrentRec^.ANext;
    For i:=0 to 9 do If pCurrentRec^.AHooks[i] <> nil Then Begin
      TFunctionHooker(pCurrentRec^.AHooks[i]).BeginTrueFunction;
    End;
  Until pCurrentRec^.ANext = nil;
end;

procedure THooker.TrueAPI(AFunction: Pointer);
var
  pCurrentRec: PHookRecord;
  Sentinel:THookRecord;
  i: Byte;
begin
  If FHooks = nil Then Exit;
  pCurrentRec := @Sentinel;
  Sentinel.ANext := FHooks;
  repeat
    pCurrentRec := pCurrentRec^.ANext;
    For i:=0 to 9 do If (pCurrentRec^.AHooks[i] <> nil) and (TFunctionHooker(pCurrentRec^.AHooks[i]).FInject.PushArg = AFunction) Then Begin
      TFunctionHooker(pCurrentRec^.AHooks[i]).BeginTrueFunction;
      Exit;
    End;
  Until pCurrentRec^.ANext = nil;
end;

procedure THooker.TrueAPIEnd;
var
  pCurrentRec: PHookRecord;
  Sentinel:THookRecord;
  i: Byte;
begin
  If FHooks = nil Then Exit;
  pCurrentRec := @Sentinel;
  Sentinel.ANext := FHooks;
  repeat
    pCurrentRec := pCurrentRec^.ANext;
    For i:=0 to 9 do If pCurrentRec^.AHooks[i] <> nil Then Begin
      TFunctionHooker(pCurrentRec^.AHooks[i]).EndTrueFunction;
    End;
  Until pCurrentRec^.ANext = nil;
end;

procedure THooker.TrueAPIEnd(AFunction: Pointer);
var
  pCurrentRec: PHookRecord;
  Sentinel:THookRecord;
  i: Byte;
begin
  If FHooks = nil Then Exit;
  pCurrentRec := @Sentinel;
  Sentinel.ANext := FHooks;
  repeat
    pCurrentRec := pCurrentRec^.ANext;
    For i:=0 to 9 do If (pCurrentRec^.AHooks[i] <> nil) and (TFunctionHooker(pCurrentRec^.AHooks[i]).FInject.PushArg = AFunction) Then Begin
      TFunctionHooker(pCurrentRec^.AHooks[i]).EndTrueFunction;
      Exit;
    End;
  Until pCurrentRec^.ANext = nil;
end;

procedure THooker.InjectIt;
var
{  h, CurrTh, ThrHandle, CurrPr: dword;
  Thread: TThreadEntry32;}
  pCurrentRec: PHookRecord;
  Sentinel:THookRecord;
  i: Byte;
//  ThreadsCount: Cardinal;
//  Data, CData: Pointer;
begin
{
  ThreadsCount := 0;
  CurrTh := GetCurrentThreadId;
  CurrPr := GetCurrentProcessId;
  h := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if h <> INVALID_HANDLE_VALUE then begin
    Thread.dwSize := SizeOf(TThreadEntry32);
    if Thread32First(h, Thread) then repeat
      if (Thread.th32ThreadID <> CurrTh) and (Thread.th32OwnerProcessID = CurrPr) then Inc(ThreadsCount);
    until not Thread32Next(h, Thread);
    Data := GetMemory(SizeOf(THandle) * ThreadsCount);
    CData := Data;
//    MessageBox(0, PChar('Threads count: ' + IntToStr(ThreadsCount)), nil, MB_OK);
    if Thread32First(h, Thread) then repeat
      if (Thread.th32ThreadID <> CurrTh) and (Thread.th32OwnerProcessID = CurrPr) then;
      begin
        ThrHandle := OpenThread(THREAD_SUSPEND_RESUME, false, Thread.th32ThreadID);
        if ThrHandle>0 then begin
          THandle(CData^) := ThrHandle;
          CData := Pointer(Cardinal(CData) + SizeOf(THandle));
          SuspendThread(ThrHandle);
        end;
      end;
    until not Thread32Next(h, Thread);
    CloseHandle(h);
  end;
}

  pCurrentRec := @Sentinel;
  Sentinel.ANext := FHooks;
  repeat
    pCurrentRec := pCurrentRec^.ANext;
    For i:=0 to 9 do If pCurrentRec^.AHooks[i] <> nil Then Begin
      TFunctionHooker(pCurrentRec^.AHooks[i]).Inject;
    End;
  Until pCurrentRec^.ANext = nil;
{
  h := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if h <> INVALID_HANDLE_VALUE then begin
    Thread.dwSize := SizeOf(TThreadEntry32);
    if Thread32First(h, Thread) then repeat
      if (Thread.th32ThreadID <> CurrTh) and (Thread.th32OwnerProcessID = CurrPr) then begin
        ThrHandle := OpenThread(THREAD_SUSPEND_RESUME, false, Thread.th32ThreadID);
        if ThrHandle>0 then begin
          ResumeThread(ThrHandle);
          CloseHandle(ThrHandle);
        end;
      end;
    until not Thread32Next(h, Thread);
    CloseHandle(h);
  end;
}
end;

initialization
  Hooker := THooker.Create;
finalization
  Hooker.Free;
end.
