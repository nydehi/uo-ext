unit PluginAPI;

interface

uses PluginsShared;

type


//  TPacketProcessed = procedure(Header:Byte; lParam: Pointer; IsFromServerToClient: Boolean);


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
    FAfterPacketCallback:TAfterPacketCallback;

  public
    procedure RegisterPacketHandler(Header:Byte; Handler: TPacketHandler); virtual;
    procedure UnRegisterPacketHandler(Header:Byte; Handler: TPacketHandler); virtual;
    function SendPacket(Packet: Pointer; Length: Cardinal; ToServer, Direct: Boolean; var Valid: Boolean):Boolean; virtual;
    procedure RegisterPacketType(IsCliServ:Boolean; Header:Byte; Size:Word; HandleProc: TPacketLengthDefinition); virtual;
    function GetNewSerial(IsMobile:Boolean): Cardinal; virtual;
    procedure FreeSerial(Serial: Cardinal); virtual;
    function GetServerSerial(Serial:Cardinal):Cardinal; virtual;
    function GetClientSerial(Serial:Cardinal):Cardinal; virtual;
    function RegisterSyncEventHandler(Event: TSyncEvent): Pointer; virtual;
    procedure AskSyncEvent(InterlockedValue: Pointer); virtual;
    procedure AskPacketProcessedEvent(ACallBack: TPacketSendedCallback; lParam: Pointer); virtual;

    function HandlePluginEvent(APluginEvent: Cardinal; APluginEventData: Pointer): Boolean; virtual;

    constructor Create;
  end;

  function DllInit: PDllPlugins; stdcall;
  procedure DllInitDone; stdcall;

  function AddPlugin(APluginInfo: PPluginInfo): Boolean;

implementation

uses Windows;

var
  Plugins: Array of PPluginInfo;
  PluginsCount: Cardinal;

  DllInitInfo: PDllPlugins;

// Procedures

function AddPlugin(APluginInfo: PPluginInfo): Boolean;
Begin
  SetLength(Plugins, PluginsCount + 1);
  Plugins[PluginsCount] := APluginInfo;
  PluginsCount := PluginsCount + 1;
  Result := True;
End;

function DllInit: PDllPlugins; stdcall;
Begin
  if PluginsCount = 0 then Begin
    Result := nil;
    DllInitInfo := nil;
  End else Begin
    DllInitInfo := GetMemory(SizeOf(TDllPlugins) + SizeOf(PPluginInfo) * (PluginsCount - 1));
    Result := DllInitInfo;
    DllInitInfo^.PluginsCount := PluginsCount;
    CopyMemory(@DllInitInfo^.Plugins, @Plugins[0], SizeOf(PPluginInfo) * PluginsCount);
    SetLength(Plugins, 0);
    PluginsCount := 0;
  End;
End;

procedure DllInitDone; stdcall;
Begin
  if DllInitInfo <> nil then FreeMemory(DllInitInfo);
End;

// TPluginApi

constructor TPluginApi.Create;
Begin
  Inherited;
End;

function TPluginApi.HandlePluginEvent(APluginEvent: Cardinal; APluginEventData: Pointer): Boolean;
var
  i: Cardinal;
begin
  Result := False;
  case APluginEvent of
    PE_INIT       : Begin
      for i := 0 to PAPI(APluginEventData)^.APICount -1 do case PAPI(APluginEventData)^.APIs[i].FuncType of
        PF_REGISTERPACKETHANDLER   : FRegisterPacketHandler   := PAPI(APluginEventData)^.APIs[i].Func;
        PF_UNREGISTERPACKETHANDLER : FUnRegisterPacketHandler := PAPI(APluginEventData)^.APIs[i].Func;
        PF_SENDPACKET              : FSendPacket              := PAPI(APluginEventData)^.APIs[i].Func;
        PF_REGISTERPACKETTYPE      : FRegisterPacketType      := PAPI(APluginEventData)^.APIs[i].Func;
        PF_GETNEWSERIAL            : FGetNewSerial            := PAPI(APluginEventData)^.APIs[i].Func;
        PF_FREESERIAL              : FFreeSerial              := PAPI(APluginEventData)^.APIs[i].Func;
        PF_GETSERVERSERIAL         : FGetServerSerial         := PAPI(APluginEventData)^.APIs[i].Func;
        PF_GETCLIENTSERIAL         : FGetClientSerial         := PAPI(APluginEventData)^.APIs[i].Func;
        PF_REGISTERSYNCEVENTHANDLER: FRegisterSyncEventHandler:= PAPI(APluginEventData)^.APIs[i].Func;
        PF_ASKSYNCEVENT            : FAskSyncEvent            := PAPI(APluginEventData)^.APIs[i].Func;
        PF_AFTERPACKETCALLBACK     : FAfterPacketCallback     := PAPI(APluginEventData)^.APIs[i].Func;
      End;
      Result := True;
    End;

    PE_FREE       : Begin
      Result := True;
    End;

    PE_PROXYSTART : Begin
      Result := True;
    End;

    PE_PROXYEND   : Begin
      Result := True;
    End;
  end;
end;

procedure TPluginApi.RegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
begin
  FRegisterPacketHandler(Header, Handler);
end;

procedure TPluginApi.UnRegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
begin
  FUnRegisterPacketHandler(Header, Handler);
end;

function TPluginApi.SendPacket(Packet: Pointer; Length: Cardinal; ToServer, Direct: Boolean; var Valid: Boolean):Boolean;
begin
  Result := FSendPacket(Packet, Length, ToServer, Direct, Valid);
end;

procedure TPluginApi.RegisterPacketType(IsCliServ:Boolean; Header:Byte; Size:Word; HandleProc: TPacketLengthDefinition);
begin
  FRegisterPacketType(IsCliServ, Header, Size, HandleProc);
end;

function TPluginApi.GetNewSerial(IsMobile:Boolean): Cardinal;
Begin
  Result := FGetNewSerial(IsMobile);
End;

procedure TPluginApi.FreeSerial(Serial: Cardinal);
Begin
  FFreeSerial(Serial);
End;

function TPluginApi.GetServerSerial(Serial:Cardinal):Cardinal;
Begin
  Result := FGetServerSerial(Serial);
End;

function TPluginApi.GetClientSerial(Serial:Cardinal):Cardinal;
Begin
  Result := FGetClientSerial(Serial);
End;

function TPluginApi.RegisterSyncEventHandler(Event: TSyncEvent): Pointer;
Begin
  Result := FRegisterSyncEventHandler(Event);
End;

procedure TPluginApi.AskSyncEvent(InterlockedValue: Pointer);
Begin
  FAskSyncEvent(InterlockedValue);
End;

procedure TPluginApi.AskPacketProcessedEvent(ACallBack: TPacketSendedCallback; lParam: Pointer);
begin
  FAfterPacketCallback(ACallBack, lParam);
end;

initialization
  SetLength(Plugins, 0);
  PluginsCount := 0;
end.
