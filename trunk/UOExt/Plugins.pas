unit Plugins;

interface

uses Windows, WinSock, PluginsShared, Common, Updater;

type


  TPluginSystem=class
  strict private type
    TDllInfo = record
      Handle: THandle;
      PluginsInfo: PDllPlugins;
      PluginsAmount: Cardinal;
      FirstPluginOffset: Cardinal;
    end;
    TProtocolHandlerArray = array [0..255] of TPacketHandler;
    TUOExtPacketHandlerInfo = packed record
      Handler: TUOExtProtocolHandler;
      Plugin: Cardinal;
    end;
    PUOExtPacketHandlerInfo = ^TUOExtPacketHandlerInfo;
    TUOExtProtocolHandlerArray = array [0..255] of TUOExtPacketHandlerInfo;
    TPluginInfo = record
      Handle: Byte;
      InitProc: TPluginProcedure;
      ProtocolHandlers: TProtocolHandlerArray;
      SyncEventCount: Integer;
      EventCallback: TSyncEvent;
      OnPacketSended: TPacketSendedCallback;
      OnPacketSendedParam: Pointer;

      UOExtPacketMin: Byte;
      UOExtPacketAmount: Byte;
      OnUOExtPacketSended: TUOExtPacketSendedCallback;
      OnUOExtPacketSendedParam: Pointer;
    end;
  strict private var
    FDlls: Array of TDllInfo;
    FDllCount: Cardinal;
    FDllPos: Cardinal;

    FPlugins: Array of TPluginInfo;
    FPluginsCount: Cardinal;

    FAPILoaded: Boolean;

    FSocket: TSocket;

    FActivePlugin: Cardinal;
    FCurrentUpdater: TUpdater;

    FUOExtProtocolHandlers: TUOExtProtocolHandlerArray;

    constructor Create;
    procedure ProcessLoadingList(Updater:TUpdater);
    function LoadDll(ADllPath: AnsiString): Boolean;
    procedure LoadAPIFromList(Updater:TUpdater);

    procedure FetchAllUpdateData(Updater: TUpdater);
  private
    FSyncEventCount: Integer;
  strict private class var
    FInstance: TPluginSystem;

    class constructor CCreate;
  public
    class property Instance: TPluginSystem read FInstance;

    property DllCount: Cardinal read FDllCount write FDllCount;
    property PluginsCount: Cardinal read FPluginsCount;

{    property OnRegisterPacketHandler: TOnRegisterPacketHandler read FOnRegisterPacketHandler write FOnRegisterPacketHandler;
    property OnUnRegisterPacketHandler: TOnRegisterPacketHandler read FOnUnregisterPacketHandler write FOnUnregisterPacketHandler;
    property OnRegisterpacketType: TOnRegisterPacketType read FOnRegisterPacketType write FOnRegisterPacketType;}

    {Control from UOExt}
    procedure Initialize(Updater:TUpdater);
    procedure InvokeUpdateProcess(Updater:TUpdater);
    procedure ProxyStart;
    property Socket: TSocket read FSocket;
    procedure ProxyEnd(ServStatus, CliStatus: Integer);
    function ClientToServerPacket(Data: Pointer; var Size:Cardinal): Boolean;
    function ServerToClientPacket(Data: Pointer; var Size:Cardinal): Boolean;
    procedure CheckSyncEvent;
    procedure PacketSended(Header: Byte; IsFromServerToClient: Boolean);

    {Control from Plugins}

    procedure RegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
    procedure UnRegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
    function AfterPacketCallback(ACallBack: TPacketSendedCallback; lParam: Pointer): Boolean;
    function RegisterSyncEventHandler(Event: TSyncEvent): Pointer;

    function UOExtPacket(UOExtHeader:Byte; Data: Pointer; Length: Cardinal): Boolean;
    procedure UOExtRegisterPacketHandler(Header:Byte; Handler: TUOExtProtocolHandler);
    procedure UOExtUnRegisterPacketHandler(Header:Byte; Handler: TUOExtProtocolHandler);

    destructor Destory;
  end;

implementation

uses ClientThread, ProtocolDescription, Serials, zLib, HookLogic
, ShardSetup, PreConnectIPDiscover, GUI;

const
  LastAPIFuncNum = 18;
type
  TRealAPI=packed record
    APICount: Cardinal;
    APIs: Array [0..LastAPIFuncNum] of TAPIFunc;
  end;

var
  API: TRealAPI;

// Plugin functions

procedure RegisterPacketHandler(Header:Byte; Handler: TPacketHandler) stdcall;
begin
  TPluginSystem.Instance.RegisterPacketHandler(Header, Handler);
end;

procedure UnRegisterPacketHandler(Header:Byte; Handler: TPacketHandler) stdcall;
begin
  TPluginSystem.Instance.UnRegisterPacketHandler(Header, Handler);
end;

function AfterPacketCallback(ACallBack: TPacketSendedCallback; lParam: Pointer):Boolean; stdcall;
Begin
  Result := TPluginSystem.Instance.AfterPacketCallback(ACallBack, lParam);
End;

function SendPacket(Packet: Pointer; Length: Cardinal; ToServer, Direct: Boolean; var Valid: Boolean):Boolean; stdcall;
begin
  Result := False;
  If not Assigned(CurrentClientThread) Then Exit;
  Result := CurrentClientThread.SendPacket(Packet, Length, ToServer, Direct, Valid);
end;

procedure RegisterPacketType(Header:Byte; Size:Word; HandleProc: TPacketLengthDefinition) stdcall;
begin
  If not Assigned(HandleProc) Then
    ProtocolDescriptor.AddPacketInfo(Header, Size)
  else
    ProtocolDescriptor.AddPacketInfo(Header, HandleProc);
end;

function RegisterSyncEventHandler(Event: TSyncEvent): Pointer; stdcall;
begin
  Result := TPluginSystem.Instance.RegisterSyncEventHandler(Event);
end;

procedure AskSyncEvent(InterlockedValue: Pointer); stdcall;
begin
  InterlockedIncrement(PInteger(InterlockedValue)^);
  InterlockedIncrement(TPluginSystem.Instance.FSyncEventCount);
end;

procedure UOExtRegisterPacketHandler(Header:Byte; Handler: TUOExtProtocolHandler) stdcall;
begin
  TPluginSystem.Instance.UOExtRegisterPacketHandler(Header, Handler);
end;

procedure UOExtUnRegisterPacketHandler(Header:Byte; Handler: TUOExtProtocolHandler) stdcall;
begin
  TPluginSystem.Instance.UOExtUnRegisterPacketHandler(Header, Handler);
end;

function UOExtSendPacket(Header:Byte; Packet: Pointer; Length: Cardinal):Boolean; stdcall;
begin
  Result := False;
  If not Assigned(CurrentClientThread) Then Exit;
  Result := TPluginSystem.Instance.UOExtPacket(Header, Packet, Length);
end;

// TPluginSystem

class constructor TPluginSystem.CCreate;
Begin
  FInstance := TPluginSystem.Create;
End;

constructor TPluginSystem.Create;
var
  i: Byte;
begin
  Inherited;
  SetLength(FDlls, 0);
  SetLength(FPlugins, 0);
  FDllCount := 0;
  FDllPos := 0;
  FPluginsCount := 0;
  FSocket := 0;
  FActivePlugin := MAXDWORD;
  ZeroMemory(@FUOExtProtocolHandlers, SizeOf(FUOExtProtocolHandlers));
  For i := 0 to 255 Do FUOExtProtocolHandlers[i].Plugin := MAXDWORD;
end;

destructor TPluginSystem.Destory;
begin
  SetLength(FDlls, 0);
  SetLength(FPlugins, 0);
  if FSocket <> 0 then Begin
    closesocket(FSocket);
    FSocket := 0;
  End;
  Inherited;
end;

procedure TPluginSystem.Initialize(Updater:TUpdater);
begin
  FCurrentUpdater := Updater;

  ProcessLoadingList(Updater);
  LoadAPIFromList(Updater);

  FCurrentUpdater := nil;
end;

procedure TPluginSystem.ProcessLoadingList(Updater:TUpdater);
var
  i, j :Byte;
  Path, Dll: AnsiString;
Begin
  if not Assigned(Updater.DllList) then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: Dll list is nil. Can''t continue.');
    {$ENDIF}
    Halt(1);
  End;
  if Updater.DllList^.Amount = 0 then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: Warning: Dll list is empty. This might be an error, but I will continue.');
    {$ENDIF}
    Exit;
  End;

  FDllCount := Updater.DllList^.Amount;
  SetLength(FDlls, FDllCount);
  FDllPos := 0;
  FPluginsCount := 0;
  Path := ExtractFilePath(AnsiString(ParamStr(0))) + 'Plugins\';

  For i := 0 to FDllCount - 1 Do Begin
    Dll := '';
    For j := 0 to 15 do Dll := Dll + IntToHex(Updater.DllList^.Items[i].MD5[j], 2);
    Self.LoadDll(Path + Dll + '.cache');
  End;

End;

function TPluginSystem.LoadDll(ADllPath: AnsiString):Boolean;
var
  DllInitProc: TDllInit;
  hDll: THandle;
begin
  Result := False;
  ADllPath := ADllPath + #0;
  {$IFDEF DEBUG}
  Write('Plugins: Loading library from ', ADllPath, ' ... ');
  {$ENDIF}
  If FAPILoaded or ((FDllPos + 1) > FDllCount) Then Begin
    {$IFDEF DEBUG}
    WriteLn('failed (no more room for libraries).');
    {$ENDIF}
    Exit;
  End;


  hDll := LoadLibraryA(@ADllPath[1]);
  if hDll = INVALID_HANDLE_VALUE then Begin
    FDllCount := FDllCount - 1;
    {$IFDEF DEBUG}
    WriteLn('failed (Library not loaded).');
    {$ENDIF}
    Exit;
  End;

  FDlls[FDllPos].Handle := hDll;
  DllInitProc := GetProcAddress(hDll, 'DllInit');
  if not Assigned(DllInitProc) then Begin
    FDllCount := FDllCount - 1;
    FreeLibrary(hDll);
    {$IFDEF DEBUG}
    WriteLn('failed (DllInit not found).');
    {$ENDIF}
    Exit;
  End;
  FDlls[FDllPos].PluginsInfo := DllInitProc;
  FDlls[FDllPos].PluginsAmount := FDlls[FDllPos].PluginsInfo^.PluginsCount;
  FDlls[FDllPos].FirstPluginOffset := FPluginsCount;
  FPluginsCount := FPluginsCount + FDlls[FDllPos].PluginsAmount;
  {$IFDEF DEBUG}
  WriteLn('done (', FDlls[FDllPos].PluginsAmount, ' plugins found).');
  {$ENDIF}
  FDllPos := FDllPos + 1;
  Result := True;
end;

procedure TPluginSystem.LoadAPIFromList(Updater:TUpdater);
var
  i, j: Cardinal;
  CurrentPluginInfo: PPluginInitInfo;
  CurrentPluginDescriptors : PluginsShared.PPluginInfo;
  InitDone: TDllInitDone;
  CurrentUOExtPacketStart: Byte;
begin
  if FAPILoaded then Exit;
  if not Assigned(Updater.PluginLoadingList) then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: Plugins list is nil. Can''t continue.');
    {$ENDIF}
    Halt(1);
  End;
  FAPILoaded := True;
  if Updater.PluginLoadingList^.Amount = 0 then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: Warning: Plugins list is empty. This might be an error, but I will continue.');
    {$ENDIF}
    Exit;
  End;
  SetLength(FPlugins, Updater.PluginLoadingList^.Amount);

  CurrentUOExtPacketStart := 1;
  For i := 0 to Updater.PluginLoadingList^.Amount - 1 Do Begin
    CurrentPluginInfo := @Updater.PluginLoadingList^.Items[i];
    If CurrentPluginInfo^.Dll > FDllCount Then Begin
      {$IFDEF DEBUG}
      WriteLn('Plugins: Error: Plugins entry ', i,' refers to Out Of Bounds Dll. This is error.');
      {$ENDIF}
      Halt(1);
    End;
    if CurrentPluginInfo^.Plugin > FDlls[CurrentPluginInfo^.Dll].PluginsAmount then Begin
      {$IFDEF DEBUG}
      WriteLn('Plugins: Error: Plugins entry ', i,' refers to Out Of Bounds Plugin ', CurrentPluginInfo^.Plugin,' for Dll ', CurrentPluginInfo^.Dll,'. This is error.');
      {$ENDIF}
      Halt(1);
    End;
    FActivePlugin := i;
    ZeroMemory(@FPlugins[i], SizeOf(TPluginInfo));

    FPlugins[i].UOExtPacketMin := CurrentUOExtPacketStart;

    CurrentPluginDescriptors := FDlls[CurrentPluginInfo^.Dll].PluginsInfo^.Plugins[CurrentPluginInfo^.Plugin];
    If CurrentPluginDescriptors^.DescriptorsCount > 0 Then For j := 0 to CurrentPluginDescriptors^.DescriptorsCount - 1 Do Begin
      if CurrentPluginDescriptors^.Descriptors[j].Descriptor = PD_UOEXTPROTO_PACKETAMOUNT then FPlugins[i].UOExtPacketAmount := CurrentPluginDescriptors^.Descriptors[j].Value;
    End;

    If TPluginProcedure(FDlls[CurrentPluginInfo^.Dll].PluginsInfo^.Plugins[CurrentPluginInfo^.Plugin].InitProcedure)(PE_INIT, @API) Then Begin
      FPlugins[i].InitProc := FDlls[CurrentPluginInfo^.Dll].PluginsInfo^.Plugins[CurrentPluginInfo^.Plugin].InitProcedure;
//      FetchAllUpdateData(Updater);
      InvokeUpdateProcess(Updater);
    End Else Begin
      {$IFDEF DEBUG}
      WriteLn('Plugins: Error: Plugin ', CurrentPluginInfo^.Plugin,' for Dll ', CurrentPluginInfo^.Dll,' filed to initialize. This is error.');
      {$ENDIF}
      Halt(1);
    End;
    FActivePlugin := MAXWORD;
  End;
  For i := 0 to FDllCount - 1 Do Begin
    InitDone := GetProcAddress(FDlls[i].Handle, 'DllInitDone');
    If Assigned(InitDone) Then InitDone();
  End;
  FPluginsCount := Updater.PluginLoadingList^.Amount;
  FDllCount := Updater.DllList^.Amount;
  {$IFDEF DEBUG}
  WriteLn('Plugins: Initialization done. Loaded ', FPluginsCount, ' plugins from ', FDllCount, ' libraries.');
  {$ENDIF}
end;

function TPluginSystem.ClientToServerPacket(Data: Pointer; var Size: Cardinal): Boolean;
var
  iPluginPos: Cardinal;
  bSend, bIsUOExt: Boolean;
  CurrentUOExtHandler: PUOExtPacketHandlerInfo;
begin
  bIsUoExt := PByte(Data)^ = ShardSetup.InternalProtocolHeader;
  bSend := True;
  Result := False;
  if bIsUOExt then Begin
    // Is it really goes here any time?
    CurrentUOExtHandler := @FUOExtProtocolHandlers[PByte(Cardinal(Data) + 3)^];
    if Assigned(CurrentUOExtHandler^.Handler) then Begin
      FActivePlugin := CurrentUOExtHandler^.Plugin;
      CurrentUOExtHandler^.Handler(Pointer(Cardinal(Data) + 3), Size - 3);
      FActivePlugin := MAXDWORD;
    End;
  End Else Begin
    If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
      If Assigned(FPlugins[iPluginPos].ProtocolHandlers[PByte(Data)^]) then Begin
        FActivePlugin := iPluginPos;
        Result := FPlugins[iPluginPos].ProtocolHandlers[PByte(Data)^](Data, Size, bSend, False);
        FActivePlugin := MAXDWORD;
      End;
      if Result or not bSend then Begin
        Result := bSend;
        Exit;
      End;
    End;
    Result := bSend;
  End;
end;

function TPluginSystem.ServerToClientPacket(Data: Pointer; var Size: Cardinal): Boolean;
var
  iPluginPos: Cardinal;
  bSend, bIsUoExt: Boolean;
  CurrentUOExtHandler: PUOExtPacketHandlerInfo;
begin
  bIsUoExt := PByte(Data)^ = ShardSetup.InternalProtocolHeader;
  bSend := True;
  Result := False;
  if bIsUOExt then Begin
    CurrentUOExtHandler := @FUOExtProtocolHandlers[PByte(Cardinal(Data) + 3)^];
    if Assigned(CurrentUOExtHandler^.Handler) then Begin
      FActivePlugin := CurrentUOExtHandler^.Plugin;
      CurrentUOExtHandler^.Handler(Pointer(Cardinal(Data) + 3), Size - 3);
      FActivePlugin := MAXDWORD;
    End;
  End Else Begin
    If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
      if Assigned(FPlugins[iPluginPos].ProtocolHandlers[PByte(Data)^]) then Begin
        FActivePlugin := iPluginPos;
        Result := FPlugins[iPluginPos].ProtocolHandlers[PByte(Data)^](Data, Size, bSend, True);
        FActivePlugin := MAXDWORD;
      End;
      if Result or not bSend then Begin
        Result := bSend;
        Exit;
      End;
    End;
    Result := bSend;
  End;
end;

procedure TPluginSystem.CheckSyncEvent;
var
  iPluginPos: Cardinal;
begin
  If InterlockedExchange(FSyncEventCount, 0) > 0 Then begin
    If FPluginsCount > 0 Then for iPluginPos := 0 to FPluginsCount do begin
      if InterlockedExchange(FPlugins[iPluginPos].SyncEventCount, 0) > 0 Then Begin
        if Assigned(FPlugins[iPluginPos].EventCallback) then Begin
         FActivePlugin := iPluginPos;
         FPlugins[iPluginPos].EventCallback();
         FActivePlugin := MAXDWORD;
        End;
      End;
    end;
  end;
end;

procedure TPluginSystem.ProxyStart;
var
  iPluginPos: Cardinal;
begin
  If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
    ZeroMemory(@FPlugins[iPluginPos].ProtocolHandlers, SizeOf(TProtocolHandlerArray));
    FActivePlugin := iPluginPos;
    try
      TPluginProcedure(FPlugins[iPluginPos].InitProc)(PE_PROXYSTART, nil);
    except
      {$IFDEF DEBUG}
        WriteLn('Plugins: Exception disarmed in plugin ', iPluginPos);
        ReadLn;
      {$ENDIF}
      Halt(1);
    end;
    FActivePlugin := MAXDWORD;
  End;
end;

procedure TPluginSystem.ProxyEnd(ServStatus, CliStatus: Integer);
var
  iPluginPos: Cardinal;
  Arg: TPE_ProxyEndEvent;
begin
  Arg.ConnectedToServer := ServStatus = 1;
  Arg.ConnectedToClient := CliStatus = 1;
  Arg.ServerCloseReason := ServStatus;
  Arg.ClientCloseReason := CliStatus;

  If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
    FActivePlugin := iPluginPos;
    try
      TPluginProcedure(FPlugins[iPluginPos].InitProc)(PE_PROXYEND, @Arg);
    except
      {$IFDEF DEBUG}
        WriteLn('Plugins: Exception disarmed in plugin ', iPluginPos);
        ReadLn;
      {$ENDIF}
      Halt(1);
    end;
    ZeroMemory(@FPlugins[iPluginPos].ProtocolHandlers, SizeOf(TProtocolHandlerArray));
    FActivePlugin := MAXDWORD;
  End;
end;

procedure TPluginSystem.PacketSended(Header: Byte; IsFromServerToClient: Boolean);
var
  iPluginPos: Cardinal;
begin
  if (Header = ShardSetup.InternalProtocolHeader) AND NOT IsFromServerToClient then Begin
    If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
      If Assigned(FPlugins[iPluginPos].OnUOExtPacketSended) then Begin
        FActivePlugin := iPluginPos;
        try
          TUOExtPacketSendedCallback(FPlugins[iPluginPos].OnUOExtPacketSended)(Header, FPlugins[iPluginPos].OnUOExtPacketSendedParam);
        except
          {$IFDEF DEBUG}
            WriteLn('Plugins: Exception disarmed in plugin ', iPluginPos);
            ReadLn;
          {$ENDIF}
          Halt(1);
        end;
        FPlugins[iPluginPos].OnUOExtPacketSended := nil;
        FActivePlugin := MAXDWORD;
      End;
    End;
  End Else Begin
    If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
      if Assigned(FPlugins[iPluginPos].OnPacketSended) then Begin
        FActivePlugin := iPluginPos;
        try
          TPacketSendedCallback(FPlugins[iPluginPos].OnPacketSended)(Header, FPlugins[iPluginPos].OnPacketSendedParam, IsFromServerToClient);
        except
          {$IFDEF DEBUG}
            WriteLn('Plugins: Exception disarmed in plugin ', iPluginPos);
            ReadLn;
          {$ENDIF}
          Halt(1);
        end;
        FPlugins[iPluginPos].OnPacketSended := nil;
        FActivePlugin := MAXDWORD;
      End;
    End;
  End;
end;

function TPluginSystem.RegisterSyncEventHandler(Event: TSyncEvent): Pointer;
begin
  if FActivePlugin <> MAXDWORD then Begin
    FPlugins[FActivePlugin].EventCallback := Event;
    Result := @FPlugins[FActivePlugin].SyncEventCount;
  End Else Begin
    Result := nil;
  End;
end;

procedure TPluginSystem.RegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
Begin
  If FActivePlugin <> MAXDWORD then FPlugins[FActivePlugin].ProtocolHandlers[Header] := Handler;
End;

procedure TPluginSystem.UnRegisterPacketHandler(Header: Byte; Handler: TPacketHandler);
begin
  If FActivePlugin <> MAXDWORD then FPlugins[FActivePlugin].ProtocolHandlers[Header] := nil;
end;

function TPluginSystem.AfterPacketCallback(ACallBack: TPacketSendedCallback; lParam: Pointer): Boolean;
begin
  If FActivePlugin <> MAXDWORD then Begin
    Result := not Assigned(FPlugins[FActivePlugin].OnPacketSended);
    if Result then Begin
      FPlugins[FActivePlugin].OnPacketSended := ACallBack;
      FPlugins[FActivePlugin].OnPacketSendedParam := lParam;
    End;
  End Else Begin
    Result := False;
  End;
end;

procedure TPluginSystem.UOExtRegisterPacketHandler(Header:Byte; Handler: TUOExtProtocolHandler);
Begin
  If FActivePlugin <> MAXDWORD then Begin
    if Header > FPlugins[FActivePlugin].UOExtPacketAmount then Begin
      {$IFDEF DEBUG}
      WriteLn('Plugins: Error: Plugin with handle ', FActivePlugin, ' request for register packet handler for header 0x', IntToHex(Header, 2), ' but it''s more than he exported! This is error. I will die!');
      ReadLn;
      {$ENDIF}
      Halt(1);
    End;
    FUOExtProtocolHandlers[FPlugins[FActivePlugin].UOExtPacketMin + Header].Plugin := FActivePlugin;
    FUOExtProtocolHandlers[FPlugins[FActivePlugin].UOExtPacketMin + Header].Handler := Handler;
  End;
End;

procedure TPluginSystem.UOExtUnRegisterPacketHandler(Header: Byte; Handler: TUOExtProtocolHandler);
begin
  If FActivePlugin <> MAXDWORD then Begin
    If Header > FPlugins[FActivePlugin].UOExtPacketAmount then Begin
      {$IFDEF DEBUG}
      WriteLn('Plugins: Error: Plugin with handle ', FActivePlugin, ' request for unregister packet handler for header 0x', IntToHex(Header, 2), ' but it''s more than he exported! This is error. I will die!');
      ReadLn;
      {$ENDIF}
      Halt(1);
    End;
    ZeroMemory(@FUOExtProtocolHandlers[FPlugins[FActivePlugin].UOExtPacketMin + Header], SizeOf(TUOExtPacketHandlerInfo));
    FUOExtProtocolHandlers[FPlugins[FActivePlugin].UOExtPacketMin + Header].Plugin := MAXDWORD;
  End;
end;

procedure TPluginSystem.FetchAllUpdateData(Updater: TUpdater);
var
  Data: Pointer;
  Size: Cardinal;
Begin
  while Updater.UOExtCanRead do Begin
    Data := Updater.UOExtTryGetPacket(Size, 30000);
    if Data = nil then Exit;
    FActivePlugin := FUOExtProtocolHandlers[PByte(Data)^].Plugin;
    FUOExtProtocolHandlers[PByte(Data)^].Handler(Pointer(Cardinal(Data) + 1), Size - 1);
    FActivePlugin := MAXDWORD;
    FreeMemory(Data);
  End;
End;

procedure TPluginSystem.InvokeUpdateProcess(Updater:TUpdater);
var
  i: Byte;
  bClear : Boolean;
Begin
  Repeat
    Sleep(1);
    FetchAllUpdateData(Updater);
    bClear := True;
    For i := 0 to 255 do if FUOExtProtocolHandlers[i].Plugin <> MAXDWORD then Begin
      bClear := False;
      Break;
    End;
  Until bClear;
End;

function TPluginSystem.UOExtPacket(UOExtHeader:Byte; Data: Pointer; Length: Cardinal): Boolean;
Begin
  Result := False;
  if FActivePlugin = MAXDWORD then Exit;
  Result := FCurrentUpdater.UOExtPacket(FPlugins[FActivePlugin].UOExtPacketMin + UOExtHeader, Data, Length);
End;


initialization
  API.APICount := LastAPIFuncNum + 1;
  API.APIs[0] .FuncType := PF_REGISTERPACKETHANDLER;
  API.APIs[0].Func := @RegisterPacketHandler;
  API.APIs[1].FuncType := PF_UNREGISTERPACKETHANDLER;
  API.APIs[1].Func := @UnRegisterPacketHandler;
  API.APIs[2].FuncType := PF_REGISTERPACKETTYPE;
  API.APIs[2].Func := @RegisterPacketType;
  API.APIs[3].FuncType := PF_SENDPACKET;
  API.APIs[3].Func := @SendPacket;
  API.APIs[4].FuncType := PF_GETNEWSERIAL;
  API.APIs[4].Func := @GetNewSerial;
  API.APIs[5].FuncType := PF_FREESERIAL;
  API.APIs[5].Func := @FreeSerial;
  API.APIs[6].FuncType := PF_GETSERVERSERIAL;
  API.APIs[6].Func := @GetServerSerial;
  API.APIs[7].FuncType := PF_GETCLIENTSERIAL;
  API.APIs[7].Func := @GetClientSerial;
  API.APIs[8].FuncType := PF_REGISTERSYNCEVENTHANDLER;
  API.APIs[8].Func := @RegisterSyncEventHandler;
  API.APIs[9].FuncType := PF_ASKSYNCEVENT;
  API.APIs[9].Func := @AskSyncEvent;
  API.APIs[10].FuncType := PF_ZLIBCOMPRESS2;
  API.APIs[10].Func := @zLib.Compress;
  API.APIs[11].FuncType := PF_ZLIBDECOMPRESS;
  API.APIs[11].Func := @zLib.Decompress;
  API.APIs[12].FuncType := PF_AFTERPACKETCALLBACK;
  API.APIs[12].Func := @AfterPacketCallback;
  API.APIs[13].FuncType := PF_UOEXTREGISTERPACKETHANDLER;
  API.APIs[13].Func := @UOExtRegisterPacketHandler;
  API.APIs[14].FuncType := PF_UOEXTUNREGISTERPACKETHANDLER;
  API.APIs[14].Func := @UOExtUnRegisterPacketHandler;
  API.APIs[15].FuncType := PF_UOEXTSENDPACKET;
  API.APIs[15].Func := @UOExtSendPacket;
  API.APIs[16].FuncType := PF_GUISETLOG;
  API.APIs[16].Func := @GUI.GUISetLog;
  API.APIs[17].FuncType := PF_GUISTARTPROCESS;
  API.APIs[17].Func := @GUI.GUIStartProcess;
  API.APIs[18].FuncType := PF_GUIUPDATEPROCESS;
  API.APIs[18].Func := @GUI.GUIUpdateProcess;
end.
