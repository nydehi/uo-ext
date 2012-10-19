unit Plugins;

interface

uses Windows, WinSock, PluginsShared, Common;

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
    TPluginInfo = record
      Handle: Byte;
      Name: PAnsiChar;
      InitProc: TPluginProcedure;
      ProtocolHandlers: TProtocolHandlerArray;
      SyncEventCount: Integer;
      EventCallback: TSyncEvent;
      OnPacketSended: TPacketSendedCallback;
      OnPacketSendedParam: Pointer;

      ExportedAPI: PPluginAPIInfo;
    end;
  strict private var
    FDlls: Array of TDllInfo;
    FDllCount: Cardinal;

    FPlugins: Array of TPluginInfo;
    FPluginsCount: Cardinal;

    FSocket: TSocket;

    FActivePlugin: Cardinal;
  private
    FSyncEventCount: Integer;
  strict private class var
    FInstance: TPluginSystem;
  public
    class property Instance: TPluginSystem read FInstance;

    property DllCount: Cardinal read FDllCount write FDllCount;
    property PluginsCount: Cardinal read FPluginsCount;

    function LoadDll(ADllPath: AnsiString): Boolean;

    {Control from UOExt}
    procedure Init; overload;
    procedure Init(PluginHandle: Word); overload;
    procedure ProxyStart;
    property Socket: TSocket read FSocket;
    procedure ProxyEnd(ServStatus, CliStatus: Integer);
    function ClientToServerPacket(Data: Pointer; var Size:Cardinal): Boolean;
    function ServerToClientPacket(Data: Pointer; var Size:Cardinal): Boolean;
    procedure CheckSyncEvent;
    procedure PacketSended(Header: Byte; IsFromServerToClient: Boolean);

    function LoadMasterLibrary(APath:AnsiString): Byte;

    {Control from Plugins}

    procedure RegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
    procedure UnRegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
    function AfterPacketCallback(ACallBack: TPacketSendedCallback; lParam: Pointer): Boolean;
    function RegisterSyncEventHandler(Event: TSyncEvent): Pointer;

    function APISearch(APluginName: PAnsiChar; AnAPIName: PAnsiChar; Flags: PCardinal): Pointer;

    constructor Create;
    destructor Destory;
  end;

var
  PluginSystem: TPluginSystem;

implementation

uses ClientThread, ProtocolDescription, zLib, HookLogic
, ShardSetup, PreConnectIPDiscover, GUI;

const
  LastAPIFuncNum = 13;
  LastMasterAPIFuncNum = 14;
type
  TRealAPI=packed record
    APICount: Cardinal;
    APIs: Array [0..LastAPIFuncNum] of TAPIFunc;
  end;
  TMasterAPI=packed record
    APICount: Cardinal;
    APIs: Array [0..LastMasterAPIFuncNum] of TAPIFunc;
  end;

{var
  API: TRealAPI;}

// Local functions
function DuplicatePluginsAPIStruct(AStruct: PPluginAPIInfo): PPluginAPIInfo;
var
  Size, i: Cardinal;
  pWriter: Pointer;
Begin
  Size := 0;
  Size := Size + SizeOf(TPluginAPIInfo) + (AStruct^.Count - 1) * (SizeOf(Pointer) + SizeOf(TPluginAPIEntry));
  If AStruct^.Count > 0 Then for i := 0 to AStruct^.Count - 1 do Size := Size + Length(AStruct^.API[i]^.AName) + 1;
  Result := GetMemory(Size);
  CopyMemory(Result, AStruct, SizeOf(TPluginAPIInfo) + (AStruct^.Count - 1) * SizeOf(Pointer));
  pWriter := Pointer(Cardinal(Result) + SizeOf(TPluginAPIInfo) + (AStruct^.Count - 1) * SizeOf(Pointer));
  If AStruct^.Count > 0 Then for i := 0 to AStruct^.Count - 1 do Begin
    Result^.API[i] := pWriter;
    CopyMemory(pWriter, AStruct.API[i], SizeOf(TPluginAPIEntry));
    pWriter := Pointer(Cardinal(pWriter) + SizeOf(TPluginAPIEntry));
    Result^.API[i].AName := pWriter;
    CopyMemory(pWriter, AStruct^.API[i]^.AName, Length(AStruct^.API[i]^.AName) + 1);
    pWriter := Pointer(Cardinal(pWriter) + Length(AStruct^.API[i]^.AName) + 1);
  End;
End;

procedure DestroyPluginsAPIStruct(AStruct: PPluginAPIInfo);
Begin
  FreeMemory(AStruct);
End;
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

function APISearch(APluginName: PAnsiChar; AnAPIName: PAnsiChar; Flags: PCardinal): Pointer; stdcall;
Begin
  Result := TPluginSystem.Instance.APISearch(APluginName, AnAPIName, Flags);
End;

function LoadPluginsLibrary(APath: PAnsiChar):Boolean; stdcall;
Begin
  Result := TPluginSystem.Instance.LoadDll(APath);
End;

const
  API:TRealAPI = (
    APICount: LastAPIFuncNum + 1;
    APIs: (
      (FuncType: PF_REGISTERPACKETHANDLER;    Func: @RegisterPacketHandler),
      (FuncType: PF_UNREGISTERPACKETHANDLER;  Func: @UnRegisterPacketHandler),
      (FuncType: PF_REGISTERPACKETTYPE;       Func: @RegisterPacketType),
      (FuncType: PF_SENDPACKET;               Func: @SendPacket),
      (FuncType: PF_REGISTERSYNCEVENTHANDLER; Func: @RegisterSyncEventHandler),
      (FuncType: PF_ASKSYNCEVENT;             Func: @AskSyncEvent),
      (FuncType: PF_ZLIBCOMPRESS2;            Func: @zLib.Compress),
      (FuncType: PF_ZLIBDECOMPRESS;           Func: @zLib.Decompress),
      (FuncType: PF_AFTERPACKETCALLBACK;      Func: @AfterPacketCallback),
      (FuncType: PF_GUISETLOG;                Func: @GUI.GUISetLog),
      (FuncType: PF_GUISTARTPROCESS;          Func: @GUI.GUIStartProcess),
      (FuncType: PF_GUIUPDATEPROCESS;         Func: @GUI.GUIUpdateProcess),
      (FuncType: PF_GUICOMMAND;               Func: @GUI.GUICommand),
      (FuncType: PF_APISEARCH;                Func: @APISearch)
    )
  );
  MasterAPI:TMasterAPI = (
    APICount: LastMasterAPIFuncNum + 1;
    APIs: (
      (FuncType: PF_REGISTERPACKETHANDLER;    Func: @RegisterPacketHandler),
      (FuncType: PF_UNREGISTERPACKETHANDLER;  Func: @UnRegisterPacketHandler),
      (FuncType: PF_REGISTERPACKETTYPE;       Func: @RegisterPacketType),
      (FuncType: PF_SENDPACKET;               Func: @SendPacket),
      (FuncType: PF_REGISTERSYNCEVENTHANDLER; Func: @RegisterSyncEventHandler),
      (FuncType: PF_ASKSYNCEVENT;             Func: @AskSyncEvent),
      (FuncType: PF_ZLIBCOMPRESS2;            Func: @zLib.Compress),
      (FuncType: PF_ZLIBDECOMPRESS;           Func: @zLib.Decompress),
      (FuncType: PF_AFTERPACKETCALLBACK;      Func: @AfterPacketCallback),
      (FuncType: PF_GUISETLOG;                Func: @GUI.GUISetLog),
      (FuncType: PF_GUISTARTPROCESS;          Func: @GUI.GUIStartProcess),
      (FuncType: PF_GUIUPDATEPROCESS;         Func: @GUI.GUIUpdateProcess),
      (FuncType: PF_GUICOMMAND;               Func: @GUI.GUICommand),
      (FuncType: PF_APISEARCH;                Func: @APISearch),
      (FuncType: PF_LOADPLUGINLIBRARY;        Func: @LoadPluginsLibrary)
    )
  );
  MasterInit:TPE_MasterPluginInit = (
    API: @MasterAPI;
    Result: 0;
  );

// TPluginSystem

constructor TPluginSystem.Create;
begin
  Inherited;
  SetLength(FDlls, 0);
  SetLength(FPlugins, 0);
  FDllCount := 0;
  FPluginsCount := 0;
  FSocket := 0;
  FActivePlugin := MAXDWORD;
  FInstance := Self;
end;

destructor TPluginSystem.Destory;
var
  i: Integer;
begin
  SetLength(FDlls, 0);
  for i := 0 to FPluginsCount - 1 do DestroyPluginsAPIStruct(FPlugins[i].ExportedAPI);
  SetLength(FPlugins, 0);
  if FSocket <> 0 then Begin
    closesocket(FSocket);
    FSocket := 0;
  End;
  Inherited;
end;

function TPluginSystem.LoadMasterLibrary(APath: AnsiString):Byte;
begin
  Result := 3;
  if LoadDll(APath) then Begin
    If FPluginsCount > 0 Then Begin
      If FPlugins[0].InitProc(PE_MASTERINIT, @MasterInit) Then Result := 0 Else Result := 3;
    End;
  End;

end;

function TPluginSystem.LoadDll(ADllPath: AnsiString):Boolean;
var
  DllInitProc: TDllInit;
  hDll: THandle;
  i, j: Integer;

  Descriptors : PluginsShared.PPluginInfo;
begin
  Result := False;
  ADllPath := ADllPath + #0;
  {$IFDEF DEBUG}
  Write('Plugins: Loading library from ', ADllPath, ' ... ');
  {$ENDIF}

  hDll := LoadLibraryA(@ADllPath[1]);
  if hDll = INVALID_HANDLE_VALUE then Begin
    FDllCount := FDllCount - 1;
    {$IFDEF DEBUG}
    WriteLn('failed (Library not loaded).');
    {$ENDIF}
    Exit;
  End;

  SetLength(FDlls, FDllCount + 1);
  FDlls[FDllCount].Handle := hDll;
  DllInitProc := GetProcAddress(hDll, 'DllInit');
  if not Assigned(DllInitProc) then Begin
    FDllCount := FDllCount - 1;
    FreeLibrary(hDll);
    {$IFDEF DEBUG}
    WriteLn('failed (DllInit not found).');
    {$ENDIF}
    Exit;
  End;
  FDlls[FDllCount].PluginsInfo := DllInitProc;
  FDlls[FDllCount].PluginsAmount := FDlls[FDllCount].PluginsInfo^.PluginsCount;
  FDlls[FDllCount].FirstPluginOffset := FPluginsCount;
  {$IFDEF DEBUG}
  WriteLn('done (', FDlls[FDllCount].PluginsAmount, ' plugins found).');
  {$ENDIF}

  if FDlls[FDllCount].PluginsAmount > 0 then Begin

    For i := 0 to FDlls[FDllCount].PluginsAmount - 1 do Begin
      SetLength(FPlugins, FPluginsCount + 1);
      ZeroMemory(@FPlugins[FPluginsCount], SizeOf(TPluginInfo));
      Descriptors := FDlls[FDllCount].PluginsInfo^.Plugins[i];
      if Descriptors^.DescriptorsCount > 0 Then for j := 0 to Descriptors^.DescriptorsCount - 1 do Begin
        if Descriptors^.Descriptors[j].Descriptor = PD_APIEXPORT then FPlugins[FPluginsCount].ExportedAPI := DuplicatePluginsAPIStruct(PPluginAPIInfo(Descriptors^.Descriptors[j].Value));
      End;
      FPlugins[FPluginsCount].InitProc := FDlls[FDllCount].PluginsInfo^.Plugins[i].InitProcedure;
      FPluginsCount := FPluginsCount + 1;

    End;

    FDllCount := FDllCount + 1;
  End Else Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: Last library unloaded. Reason: No plugins found');
    {$ENDIF}
    FreeLibrary(hDll);
    SetLength(FDlls, FDllCount);
  End;
  Result := True;
end;

function TPluginSystem.ClientToServerPacket(Data: Pointer; var Size: Cardinal): Boolean;
var
  iPluginPos: Cardinal;
  bSend: Boolean;
begin
  bSend := True;
  Result := False;
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
end;

function TPluginSystem.ServerToClientPacket(Data: Pointer; var Size: Cardinal): Boolean;
var
  iPluginPos: Cardinal;
  bSend: Boolean;
begin
  bSend := True;
  Result := False;
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

procedure TPluginSystem.Init;
var
  iPluginPos: Cardinal;
begin
  If (FPluginsCount > 0) Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
    FActivePlugin := iPluginPos;
    try
      TPluginProcedure(FPlugins[iPluginPos].InitProc)(PE_INIT, @API);
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

procedure TPluginSystem.Init(PluginHandle: Word);
begin
  if (FPluginsCount > PluginHandle) then Begin
    FActivePlugin := PluginHandle;
    try
      TPluginProcedure(FPlugins[PluginHandle].InitProc)(PE_INIT, @API);
    except
      {$IFDEF DEBUG}
        WriteLn('Plugins: Exception disarmed in plugin ', PluginHandle);
        ReadLn;
      {$ENDIF}
      Halt(1);
    end;
    FActivePlugin := MAXDWORD;
  End;
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

function TPluginSystem.APISearch(APluginName: PAnsiChar; AnAPIName: PAnsiChar; Flags: PCardinal): Pointer;
var
  i, j: Cardinal;
  PlName, APIName: AnsiString;
begin
  Result := nil;
  PlName := AnsiString(APluginName);
  APIName := AnsiString(AnAPIName);
  for i := 0 to FPluginsCount - 1 do
    if AnsiString(FPlugins[i].Name) = PlName then
      If FPlugins[i].ExportedAPI^.Count > 0 Then for j := 0 to FPlugins[i].ExportedAPI^.Count - 1 do
        if AnsiString(FPlugins[i].ExportedAPI^.API[j]^.AName) = APIName then Begin
          Result := FPlugins[i].ExportedAPI^.API[j]^.AnAPI;
          if Flags <> nil then Flags^ := FPlugins[i].ExportedAPI^.API[j]^.Flags;
          Exit;
        End;
end;

end.
