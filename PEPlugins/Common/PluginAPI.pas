unit PluginAPI;

interface

uses PluginsShared;

type


  TDllInitDone = procedure;

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

    FGUISetLog: TGUISetLog;
    FGUIStartProcess: TGUIStartProcess;
    FGUIUpdateProcess: TGUIUpdateProcess;

    FUOExtProtocolRegisterHandler: TUOExtProtocolRegisterHandler;
    FUOExtProtocolUnRegisterHandler: TUOExtProtocolUnRegisterHandler;
    FUOExtProtocolSendPacket: TUOExtProtocolSendPacket;

    FAPISearch: TAPISearch;
    FLoadPluginsLibrary: TLoadPluginsLibrary;

  public
    procedure RegisterPacketHandler(Header:Byte; Handler: TPacketHandler); virtual;
    procedure UnRegisterPacketHandler(Header:Byte; Handler: TPacketHandler); virtual;
    function SendPacket(Packet: Pointer; Length: Cardinal; ToServer, Direct: Boolean; var Valid: Boolean):Boolean; virtual;
    procedure RegisterPacketType(Header:Byte; Size:Word; HandleProc: TPacketLengthDefinition); virtual;
    function GetNewSerial(IsMobile:Boolean): Cardinal; virtual;
    procedure FreeSerial(Serial: Cardinal); virtual;
    function GetServerSerial(Serial:Cardinal):Cardinal; virtual;
    function GetClientSerial(Serial:Cardinal):Cardinal; virtual;
    function RegisterSyncEventHandler(Event: TSyncEvent): Pointer; virtual;
    procedure AskSyncEvent(InterlockedValue: Pointer); virtual;
    function AskPacketProcessedEvent(ACallBack: TPacketSendedCallback; lParam: Pointer): Boolean; virtual;

    function HandlePluginEvent(APluginEvent: Cardinal; APluginEventData: Pointer): Boolean; virtual;

    function GUISetLog(LineHandle: Cardinal; ParentHandle: Cardinal; Data: PAnsiChar): Cardinal; virtual;
    function GUIStartProcess(LineHandle, ParentHandle: Cardinal; ProcessLabel: PAnsiChar; Min, Max, Current: Cardinal): Cardinal; virtual;
    procedure GUIUpdateProcess(ProcessHandle, Min, Max, Current: Cardinal); virtual;

    procedure UOExtProtocolRegisterHandler(Header:Byte; Handler:TUOExtProtocolHandler); virtual;
    procedure UOExtProtocolUnRegisterHandler(Header:Byte; Handler:TUOExtProtocolHandler); virtual;
    procedure UOExtProtocolSendPacket(Header:Byte; Packet: Pointer; Size: Cardinal); virtual;

    function APISearch(APluginName: PAnsiChar; AnAPIName: PAnsiChar; Flags: PCardinal): Pointer; virtual;
    function LoadPluginsLibrary(APath: PAnsiChar):Boolean; virtual;

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
        PF_REGISTERPACKETHANDLER        : FRegisterPacketHandler          := PAPI(APluginEventData)^.APIs[i].Func;
        PF_UNREGISTERPACKETHANDLER      : FUnRegisterPacketHandler        := PAPI(APluginEventData)^.APIs[i].Func;
        PF_SENDPACKET                   : FSendPacket                     := PAPI(APluginEventData)^.APIs[i].Func;
        PF_REGISTERPACKETTYPE           : FRegisterPacketType             := PAPI(APluginEventData)^.APIs[i].Func;

        PF_GETNEWSERIAL                 : FGetNewSerial                   := PAPI(APluginEventData)^.APIs[i].Func;
        PF_FREESERIAL                   : FFreeSerial                     := PAPI(APluginEventData)^.APIs[i].Func;
        PF_GETSERVERSERIAL              : FGetServerSerial                := PAPI(APluginEventData)^.APIs[i].Func;
        PF_GETCLIENTSERIAL              : FGetClientSerial                := PAPI(APluginEventData)^.APIs[i].Func;

        PF_REGISTERSYNCEVENTHANDLER     : FRegisterSyncEventHandler       := PAPI(APluginEventData)^.APIs[i].Func;
        PF_ASKSYNCEVENT                 : FAskSyncEvent                   := PAPI(APluginEventData)^.APIs[i].Func;
        PF_AFTERPACKETCALLBACK          : FAfterPacketCallback            := PAPI(APluginEventData)^.APIs[i].Func;

        PF_GUISETLOG                    : FGUISetLog                      := PAPI(APluginEventData)^.APIs[i].Func;
        PF_GUISTARTPROCESS              : FGUIStartProcess                := PAPI(APluginEventData)^.APIs[i].Func;
        PF_GUIUPDATEPROCESS             : FGUIUpdateProcess               := PAPI(APluginEventData)^.APIs[i].Func;

        PF_UOEXTREGISTERPACKETHANDLER   : FUOExtProtocolRegisterHandler   := PAPI(APluginEventData)^.APIs[i].Func;
        PF_UOEXTUNREGISTERPACKETHANDLER : FUOExtProtocolUnRegisterHandler := PAPI(APluginEventData)^.APIs[i].Func;
        PF_UOEXTSENDPACKET              : FUOExtProtocolSendPacket        := PAPI(APluginEventData)^.APIs[i].Func;

        PF_APISEARCH                    : FAPISearch                      := PAPI(APluginEventData)^.APIs[i].Func;
        PF_LOADPLUGINLIBRARY            : FLoadPluginsLibrary             := PAPI(APluginEventData)^.APIs[i].Func;
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

procedure TPluginApi.RegisterPacketType(Header:Byte; Size:Word; HandleProc: TPacketLengthDefinition);
begin
  FRegisterPacketType(Header, Size, HandleProc);
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

function TPluginApi.AskPacketProcessedEvent(ACallBack: TPacketSendedCallback; lParam: Pointer): Boolean;
begin
  Result := FAfterPacketCallback(ACallBack, lParam);
end;

function TPluginApi.GUISetLog(LineHandle: Cardinal; ParentHandle: Cardinal; Data: PAnsiChar): Cardinal;
Begin
  Result :=  FGUISetLog(LineHandle, ParentHandle, Data);
End;

function TPluginApi.GUIStartProcess(LineHandle, ParentHandle: Cardinal; ProcessLabel: PAnsiChar; Min, Max, Current: Cardinal): Cardinal;
Begin
  Result := FGUIStartProcess(LineHandle, ParentHandle, ProcessLabel, Min, Max, Current);
End;

procedure TPluginApi.GUIUpdateProcess(ProcessHandle, Min, Max, Current: Cardinal);
Begin
  FGUIUpdateProcess(ProcessHandle, Min, Max, Current);
End;

procedure TPluginApi.UOExtProtocolRegisterHandler(Header:Byte; Handler:TUOExtProtocolHandler);
Begin
  FUOExtProtocolRegisterHandler(Header, Handler);
End;

procedure TPluginApi.UOExtProtocolUnRegisterHandler(Header:Byte; Handler:TUOExtProtocolHandler);
Begin
  FUOExtProtocolUnRegisterHandler(Header, Handler);
End;

procedure TPluginApi.UOExtProtocolSendPacket(Header:Byte; Packet: Pointer; Size: Cardinal);
Begin
  FUOExtProtocolSendPacket(Header, Packet, Size);
End;

function TPluginApi.APISearch(APluginName: PAnsiChar; AnAPIName: PAnsiChar; Flags: PCardinal): Pointer;
Begin
  Result := FAPISearch(APluginName, AnAPIName, Flags);
End;

function TPluginApi.LoadPluginsLibrary(APath: PAnsiChar):Boolean;
Begin
  Result := FLoadPluginsLibrary(APath);
End;


initialization
  SetLength(Plugins, 0);
  PluginsCount := 0;
end.
