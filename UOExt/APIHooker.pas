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

  TFunctionHooker=class
  protected
    FHooked: Boolean;
    FTrueSetted: Boolean;
    FOldData: ROldCode;
    FInject: RInjectCode;
    FLock: TRTLCriticalSection;
    FOriginalFunction: Pointer;
    FTrueFuncRequests: Integer;
  public
    property IsTrue: Boolean read FTrueSetted;
    procedure Inject; virtual;
    procedure BeginTrueFunction; virtual;
    procedure EndTrueFunction; virtual;
    constructor Create(AInjectFunction, AOriginalFunction: Pointer); virtual;
    destructor Destroy; override;
  end;

  TAddCallerFunctionHooker=class(TFunctionHooker)
  strict private type
    RTrampoline = packed record
      PopEAX: Byte;
      PushEAX1: Byte;
      PushEAX2: Byte;
      Push1: Byte;
      PushArg: pointer;
      RetOp: byte;
    end;
  private
    FTrampoline: RTrampoline;
  public
    constructor Create(AInjectFunction, AOriginalFunction: Pointer); override;
  end;

  TFunctionHookerClass = class of TFunctionHooker;

  PHookRecord=^THookRecord;
  THookRecord=record
    AHooks: Array [0..9] of TFunctionHooker;
    ANext: PHookRecord;
  end;

  THooker=class
  private
    FHooks:PHookRecord;
    FInjected: Boolean;
  protected
    constructor Create;
  public
    property Injected: Boolean read FInjected;
    procedure HookFunction(AInjectFunction, AOriginalFunction: Pointer); overload;
    procedure HookFunction(AInjectFunction, AOriginalFunction: Pointer; HookClass: TFunctionHookerClass); overload;
    procedure InjectIt;
    procedure Restore;
    procedure TrueAPI; overload;
    procedure TrueAPI(AFunction: Pointer); overload;
    procedure TrueAPIEnd; overload;
    procedure TrueAPIEnd(AFunction: Pointer); overload;

    destructor Destroy; override;

    class function Hooker: THooker;
  end;

implementation

uses Common;

var
  HookerInstance : THooker;


const
  THREAD_SUSPEND_RESUME = $0002;

Function OpenThread(dwDesiredAccess: dword; bInheritHandle: bool; dwThreadId: dword):dword;
                    stdcall; external 'kernel32.dll';

// TFunctionHooker

class function THooker.Hooker: THooker;
Begin
  If not Assigned(HookerInstance) Then HookerInstance := THooker.Create;
  Result := HookerInstance;
End;

constructor TFunctionHooker.Create(AInjectFunction, AOriginalFunction: Pointer);
begin
  Inherited Create;
  FInject.PuhsOp := $68;
  FInject.PushArg := AInjectFunction;
  FInject.RetOp := $C3;
  FHooked := False;
  FTrueSetted := False;
  FOriginalFunction := AOriginalFunction;
  FTrueFuncRequests := 0;
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
  {$IFDEF DEBUG}
  Msg: AnsiString;
  {$ENDIF}
begin
  ReadProcessMemory(GetCurrentProcess, FOriginalFunction, @FOldData, SizeOf(FOldData), Writen);
  {$IFDEF DEBUG}
  If Writen <> SizeOf(FOldData) Then Begin
    Msg := 'Inject. Readed wrong size ('+IntToStr(Writen)+')';
    MessageBoxA(0, @Msg[1], nil, MB_OK);
    Halt(1);
  End;
  {$ENDIF}
  WriteProcessMemory(GetCurrentProcess, FOriginalFunction, @FInject, SizeOf(FInject), Writen);
  {$IFDEF DEBUG}
  If Writen <> SizeOf(FInject) Then Begin
    Msg := 'Inject. Written wrong size ('+IntToStr(Writen)+')';
    MessageBoxA(0, @Msg[1], nil, MB_OK);
    Halt(1);
  End;
  {$ENDIF}
end;

procedure TFunctionHooker.BeginTrueFunction;
var
  Writen:Cardinal;
  {$IFDEF DEBUG}
  Msg: AnsiString;
  GLE: Cardinal;
  {$ENDIF}
begin
  if FTrueSetted then Exit;
  WriteProcessMemory(GetCurrentProcess, FOriginalFunction, @FOldData, SizeOf(FOldData), Writen);
  {$IFDEF DEBUG}
  If Writen <> SizeOf(FInject) Then Begin
    GLE := 0;
    if Writen = 0 then GLE := GetLastError;
    Msg := 'Inject. Written wrong size ('+IntToStr(Writen)+'). GLE := '+IntToStr(GLE);
    MessageBoxA(0, @Msg[1], nil, MB_OK);
    ExitProcess(1);
  End;
  {$ENDIF}
  FTrueSetted := True;
end;

procedure TFunctionHooker.EndTrueFunction;
var
  Writen:Cardinal;
  {$IFDEF DEBUG}
  Msg: AnsiString;
  GLE: Cardinal;
  {$ENDIF}
begin
  if not FTrueSetted then Exit;

  WriteProcessMemory(GetCurrentProcess, FOriginalFunction, @FInject, SizeOf(FInject), Writen);
  {$IFDEF DEBUG}
  If Writen <> SizeOf(FInject) Then Begin
    GLE := 0;
    if Writen = 0 then GLE := GetLastError;
    Msg := 'Inject. Written wrong size ('+IntToStr(Writen)+'). GLE := '+IntToStr(GLE);
    MessageBoxA(0, @Msg[1], nil, MB_OK);
    ExitProcess(1);
  End;
  {$ENDIF}
  FTrueSetted := False;
end;
 // THooker

constructor THooker.Create;
begin
  Inherited;
  FHooks := nil;
  FInjected := False;
end;

destructor THooker.Destroy;
var
  pCurrentRec, pLastRec: PHookRecord;
  i: Byte;
begin
  Restore;
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

procedure THooker.HookFunction(AInjectFunction, AOriginalFunction: Pointer; HookClass: TFunctionHookerClass);
var
  pCurrentRec: PHookRecord;
  Sentinel:THookRecord;
  i: Byte;
  AHook: TFunctionHooker;
begin
  AHook := HookClass.Create(AInjectFunction, AOriginalFunction);
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
  if FInjected then AHook.Inject;

end;

procedure THooker.HookFunction(AInjectFunction, AOriginalFunction: Pointer);
begin
  HookFunction(AInjectFunction, AOriginalFunction, TFunctionHooker);
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
      pCurrentRec^.AHooks[i].BeginTrueFunction;
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
    For i:=0 to 9 do If (pCurrentRec^.AHooks[i] <> nil) and (pCurrentRec^.AHooks[i].FInject.PushArg = AFunction) Then Begin
      pCurrentRec^.AHooks[i].BeginTrueFunction;
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
      pCurrentRec^.AHooks[i].EndTrueFunction;
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
    For i:=0 to 9 do If (pCurrentRec^.AHooks[i] <> nil) and (pCurrentRec^.AHooks[i].FInject.PushArg = AFunction) Then Begin
      pCurrentRec^.AHooks[i].EndTrueFunction;
      Exit;
    End;
  Until pCurrentRec^.ANext = nil;
end;

procedure THooker.InjectIt;
var
  pCurrentRec: PHookRecord;
  Sentinel:THookRecord;
  i: Byte;
begin
  if FInjected then Exit;
  FInjected := True;
  If FHooks <> nil Then Begin
    pCurrentRec := @Sentinel;
    Sentinel.ANext := FHooks;
    repeat
      pCurrentRec := pCurrentRec^.ANext;
      For i:=0 to 9 do If pCurrentRec^.AHooks[i] <> nil Then Begin
        pCurrentRec^.AHooks[i].Inject;
      End;
    Until pCurrentRec^.ANext = nil;
  End;
end;

procedure THooker.Restore;
var
  pCurrentRec: PHookRecord;
  Sentinel:THookRecord;
  i: Byte;
begin
  if not FInjected then Exit;
  FInjected := False;
  If FHooks <> nil Then Begin
    pCurrentRec := @Sentinel;
    Sentinel.ANext := FHooks;
    repeat
      pCurrentRec := pCurrentRec^.ANext;
      For i:=0 to 9 do If pCurrentRec^.AHooks[i] <> nil Then Begin
        pCurrentRec^.AHooks[i].BeginTrueFunction;
      End;
    Until pCurrentRec^.ANext = nil;
  End;
  FInjected := False;
end;

// TAddCallerFunctionHooker

constructor TAddCallerFunctionHooker.Create(AInjectFunction: Pointer; AOriginalFunction: Pointer);
begin
  FTrampoline.PopEAX := $58;
  FTrampoline.PushEAX1 := $50;
  FTrampoline.PushEAX2 := $50;
  FTrampoline.Push1 := $68;
  FTrampoline.PushArg := AInjectFunction;
  FTrampoline.RetOp := $C3;

  Inherited Create(@FTrampoline, AOriginalFunction);
end;

end.
