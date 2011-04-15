unit PluginAPI;

interface

uses PluginsShared;

type
  TPluginInitialization = procedure;
  TProxyStartEvent = procedure;
  TProxyEndEvent = procedure;

  TPluginApi = class
  private
    FRegisterPacketHandler: TRegisterPacketHandler;
    FUnRegisterPacketHandler: TUnRegisterPacketHandler;
    FSendPacket: TSendPacket;
    FRegisterPacketType: TRegisterPacketType;
    FGetNewSerial: TGetNewSerial;
    FFreeSerial: TFreeSerial;
    FGetServerSerial: TGetServerSerial;
    FGetClientSerial: TGetClientSerial;
    FRegisterSyncEventHandler: TRegisterSyncEventHandler;
    FAskSyncEvent: TAskSyncEvent;

    FInterlockedValue: Pointer;

    FProxyStart: TProxyStartEvent;
    FProxyEnd: TProxyEndEvent;
  protected
    procedure RegisterAPIFuncs(APICount: Cardinal; anAPIDescriptors: PAPIFunc);
    procedure DoProxyStart;
    procedure DoProxyEnd;
  public
    property InterlockedValue: Pointer read FInterlockedValue;
    property OnProxyStart: TProxyStartEvent read FProxyStart write FProxyStart;
    property OnProxyEnd: TProxyEndEvent read FProxyEnd write FProxyEnd;

    procedure RegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
    procedure UnRegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
    function SendPacket(Packet: Pointer; Length: Cardinal; ToServer, Direct: Boolean; var Valid: Boolean):Boolean;
    procedure RegisterPacketType(IsCliServ:Boolean; Header:Byte; Size:Word; HandleProc: TPacketLengthDefinition);
    function GetNewSerial(IsMobile:Boolean): Cardinal;
    procedure FreeSerial(Serial: Cardinal);
    function GetServerSerial(Serial:Cardinal):Cardinal;
    function GetClientSerial(Serial:Cardinal):Cardinal;
    function RegisterSyncEventHandler(Event: TSyncEvent): Pointer;
    procedure AskSyncEvent(InterlockedValue: Pointer); overload;
    procedure AskSyncEvent; overload;

    constructor Create;
  end;

  procedure UOExtInit (APICount: Cardinal; anAPIFunc: PAPIFunc) stdcall;
  procedure ProxyStart; stdcall;
  procedure ProxyEnd; stdcall;

var
  API : TPluginApi;
  PluginInitialization : TPluginInitialization;

implementation

uses SysUtils;

// Procedures

procedure UOExtInit (APICount: Cardinal; anAPIFunc: PAPIFunc) stdcall;
begin
  API.RegisterAPIFuncs(APICount, anAPIFunc);
  If Assigned(PluginInitialization) Then PluginInitialization();
end;

procedure ProxyStart stdcall;
Begin
  API.DoProxyStart;
End;

procedure ProxyEnd stdcall;
Begin
  API.DoProxyEnd;
End;

// TPluginApi

constructor TPluginApi.Create;
Begin
  Inherited Create;
  FInterlockedValue := nil;
End;

procedure TPluginApi.RegisterAPIFuncs(APICount: Cardinal; anAPIDescriptors: PAPIFunc);
var
  pAPI: PAPIFunc;
  cFuncType: Cardinal;
  i: Cardinal;
begin
  For i := 0 to APICount - 1 do Begin
    pAPI := PAPIFunc(Cardinal(anAPIDescriptors) + i * SizeOf(RAPIFunc));
    cFuncType := pAPI^.FuncType;
    case cFuncType of
      PF_REGISTERPACKETHANDLER   : FRegisterPacketHandler   := pAPI^.Func;
      PF_UNREGISTERPACKETHANDLER : FUnRegisterPacketHandler := pAPI^.Func;
      PF_SENDPACKET              : FSendPacket              := pAPI^.Func;
      PF_REGISTERPACKETTYPE      : FRegisterPacketType      := pAPI^.Func;
      PF_GETNEWSERIAL            : FGetNewSerial            := pAPI^.Func;
      PF_FREESERIAL              : FFreeSerial              := pAPI^.Func;
      PF_GETSERVERSERIAL         : FGetServerSerial         := pAPI^.Func;
      PF_GETCLIENTSERIAL         : FGetClientSerial         := pAPI^.Func;
      PF_REGISTERSYNCEVENTHANDLER: FRegisterSyncEventHandler:= pAPI^.Func;
      PF_ASKSYNCEVENT            : FAskSyncEvent            := pAPI^.Func;
    end;
  End;
end;

procedure TPluginApi.RegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
begin
  If @FRegisterPacketHandler = nil Then Raise Exception.Create('RegisterPacketHandler not supplied by host');
  FRegisterPacketHandler(Header, Handler);
end;

procedure TPluginApi.UnRegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
begin
  If @FUnRegisterPacketHandler = nil Then Raise Exception.Create('UnRegisterPacketHandler not supplied by host');
  FUnRegisterPacketHandler(Header, Handler);
end;

function TPluginApi.SendPacket(Packet: Pointer; Length: Cardinal; ToServer, Direct: Boolean; var Valid: Boolean):Boolean;
begin
  If @FSendPacket = nil Then Raise Exception.Create('SendPacket not supplied by host');
  Result := FSendPacket(Packet, Length, ToServer, Direct, Valid);
end;

procedure TPluginApi.RegisterPacketType(IsCliServ:Boolean; Header:Byte; Size:Word; HandleProc: TPacketLengthDefinition);
begin
  If @FRegisterPacketType = nil Then Raise Exception.Create('RegisterPacketType not supplied by host');
  FRegisterPacketType(IsCliServ, Header, Size, HandleProc);
end;

function TPluginApi.GetNewSerial(IsMobile:Boolean): Cardinal;
Begin
  If @FGetNewSerial = nil Then Raise Exception.Create('GetNewSerial not supplied by host');
  Result := FGetNewSerial(IsMobile);
End;

procedure TPluginApi.FreeSerial(Serial: Cardinal);
Begin
  If @FFreeSerial = nil Then Raise Exception.Create('FreeSerial not supplied by host');
  FFreeSerial(Serial);
End;

function TPluginApi.GetServerSerial(Serial:Cardinal):Cardinal;
Begin
  If @FGetServerSerial = nil Then Raise Exception.Create('GetServerSerial not supplied by host');
  Result := FGetServerSerial(Serial);
End;

function TPluginApi.GetClientSerial(Serial:Cardinal):Cardinal;
Begin
  If @FGetClientSerial = nil Then Raise Exception.Create('GetClientSerial not supplied by host');
  Result := FGetClientSerial(Serial);
End;

function TPluginApi.RegisterSyncEventHandler(Event: TSyncEvent): Pointer;
Begin
  If @FRegisterSyncEventHandler = nil Then Raise Exception.Create('RegisterSyncEventHandler not supplied by host');
  Result := FRegisterSyncEventHandler(Event);
  FInterlockedValue := Result;
End;

procedure TPluginApi.AskSyncEvent(InterlockedValue: Pointer);
Begin
  If @FAskSyncEvent = nil Then Raise Exception.Create('AskSyncEvent not supplied by host');
  FAskSyncEvent(InterlockedValue);
End;

procedure TPluginApi.AskSyncEvent;
Begin
  If @FAskSyncEvent = nil Then Raise Exception.Create('AskSyncEvent not supplied by host');
  If FInterlockedValue = nil Then Raise Exception.Create('You are trying to AskSyncEvent without supplying SyncEventHandler.');
  FAskSyncEvent(FInterlockedValue);
End;

procedure TPluginApi.DoProxyStart;
begin
  if Assigned(FProxyStart) then FProxyStart();
end;

procedure TPluginApi.DoProxyEnd;
begin
  if Assigned(FProxyEnd) then FProxyEnd();
end;

initialization
  API := TPluginApi.Create;
  PluginInitialization := nil;
finalization
  API.Free;
end.
