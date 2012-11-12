unit Plugins;

interface

uses Windows, WinSock2, PluginsShared, Common;

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
      OnPacketSended: TPacketSendedCallback;
      OnPacketSendedParam: Pointer;

      ExportedAPI: PPluginAPIInfo;
      Trampolines: Pointer;
    end;
  strict private var
    FDlls: Array of TDllInfo;
    FDllCount: Cardinal;

    FPlugins: Array of TPluginInfo;
    FPluginsCount: Cardinal;

    FSocket: TSocket;

    FMasterTrampolines: Pointer;
  private
    FThreadLocker: TRTLCriticalSection;
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
    procedure DeInit;
    function ClientToServerPacket(Data: Pointer; var Size:Cardinal): Boolean;
    function ServerToClientPacket(Data: Pointer; var Size:Cardinal): Boolean;
    procedure PacketSended(Header: Byte; IsFromServerToClient: Boolean);

    function LoadMasterLibrary(APath:AnsiString): Cardinal;

    {Control from Plugins}

    procedure RegisterPacketHandler(APlugin:Cardinal; Header:Byte; Handler: TPacketHandler);
    procedure UnRegisterPacketHandler(APlugin:Cardinal; Header:Byte; Handler: TPacketHandler);
    function AfterPacketCallback(APlugin:Cardinal; ACallBack: TPacketSendedCallback; lParam: Pointer): Boolean;

    function APISearch(APlugin:Cardinal; APluginName: PAnsiChar; AnAPIName: PAnsiChar; Flags: PCardinal): Pointer;

    constructor Create;
    destructor Destory;
  end;

var
  PluginSystem: TPluginSystem;

implementation

uses ClientThread, ProtocolDescription, zLib, HookLogic
, ShardSetup, PreConnectIPDiscover, GUI;

const
  LastAPIFuncNum = 11;
  LastMasterAPIFuncNum = 12;
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

procedure RegisterPacketHandler(APlugin: Cardinal; Header:Byte; Handler: TPacketHandler) stdcall;
begin
  TPluginSystem.Instance.RegisterPacketHandler(APlugin, Header, Handler);
end;

procedure UnRegisterPacketHandler(APlugin: Cardinal; Header:Byte; Handler: TPacketHandler) stdcall;
begin
  TPluginSystem.Instance.UnRegisterPacketHandler(APlugin, Header, Handler);
end;

function AfterPacketCallback(APlugin: Cardinal; ACallBack: TPacketSendedCallback; lParam: Pointer):Boolean; stdcall;
Begin
  Result := TPluginSystem.Instance.AfterPacketCallback(APlugin, ACallBack, lParam);
End;

function SendPacket(APlugin: Cardinal; Packet: Pointer; Length: Cardinal; ToServer: Boolean; var Valid: Boolean):Boolean; stdcall;
begin
  Result := False;
  If not Assigned(CurrentClientThread) Then Exit;
  Result := CurrentClientThread.SendPacket(Packet, Length, ToServer, True, Valid);
end;

procedure RegisterPacketType(APlugin: Cardinal; Header:Byte; Size:Word; HandleProc: TPacketLengthDefinition) stdcall;
begin
  If not Assigned(HandleProc) Then
    ProtocolDescriptor.AddPacketInfo(Header, Size)
  else
    ProtocolDescriptor.AddPacketInfo(Header, HandleProc);
end;

function APISearch( APlugin: Cardinal; APluginName: PAnsiChar; AnAPIName: PAnsiChar; Flags: PCardinal): Pointer; stdcall;
Begin
  Result := TPluginSystem.Instance.APISearch(APlugin, APluginName, AnAPIName, Flags);
End;

function LoadPluginsLibrary(APlugin: Cardinal; APath: PAnsiChar):Boolean; stdcall;
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
      (FuncType: PF_ZLIBCOMPRESS2;            Func: @zLib.Compress),
      (FuncType: PF_ZLIBUNCOMPRESS;           Func: @zLib.Uncompress),
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
      (FuncType: PF_ZLIBCOMPRESS2;            Func: @zLib.Compress),
      (FuncType: PF_ZLIBUNCOMPRESS;           Func: @zLib.Uncompress),
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
// Local functions
function MakeAPIForPlugin(APlugin: Cardinal; Master: Boolean): PAPI;
type
  TTrampoline = packed record
    PopEAX: Byte;
    Push1: Byte;
    Plugin: Cardinal;
    PushEAX: Byte;
    Push2: Byte;
    RetPoint: Pointer;
    Ret: Byte;
  end;
  TTrampolines = Array [0..0] of TTrampoline;
  PTrampolines = ^TTrampolines;
var
  WorkStruct: PAPI;
  i: Cardinal;
  Trampolines: PTrampolines;
Begin
  // TODO: Refactor this place. PAPI needs only for PE_INIT call. No need to hold it after Init stage.
  if Master then WorkStruct := PAPI(@MasterAPI) Else WorkStruct := PAPI(@API);
  Result := GetMemory(WorkStruct^.APICount * (SizeOf(TAPIFunc) + SizeOf(TTrampoline)) + 4);
  Trampolines := Pointer(Cardinal(Result) + WorkStruct^.APICount * SizeOf(TAPIFunc) + 4);
  Result^.APICount := WorkStruct^.APICount;
  for i := 0 to WorkStruct^.APICount - 1 do Begin
    Result^.APIs[i].FuncType := WorkStruct^.APIs[i].FuncType;
    Result^.APIs[i].Func := @Trampolines^[i];
    Trampolines^[i].PopEAX := $58;
    Trampolines^[i].Push1 := $68;
    Trampolines^[i].Plugin := APlugin;
    Trampolines^[i].PushEAX := $50;
    Trampolines^[i].Push2 := $68;
    Trampolines^[i].RetPoint := WorkStruct^.APIs[i].Func;
    Trampolines^[i].Ret := $C3;
  End;

End;


procedure FreeAPIForPlugin(AnAPI: PAPI);
Begin
  FreeMemory(AnAPI);
End;


// TPluginSystem

constructor TPluginSystem.Create;
begin
  Inherited;
  SetLength(FDlls, 0);
  SetLength(FPlugins, 0);
  FDllCount := 0;
  FPluginsCount := 0;
  FSocket := 0;
  FInstance := Self;
  FMasterTrampolines := MakeAPIForPlugin(0, True);
  PPointer(@MasterInit.API)^ := FMasterTrampolines;
  InitializeCriticalSection(FThreadLocker);
end;

destructor TPluginSystem.Destory;
var
  i: Integer;
begin
  SetLength(FDlls, 0);
  for i := 0 to FPluginsCount - 1 do Begin
    DestroyPluginsAPIStruct(FPlugins[i].ExportedAPI);
    FreeAPIForPlugin(FPlugins[i].Trampolines);
  End;
  FreeAPIForPlugin(FMasterTrampolines);
  SetLength(FPlugins, 0);
  if FSocket <> 0 then Begin
    closesocket(FSocket);
    FSocket := 0;
  End;
  DeleteCriticalSection(FThreadLocker);
  Inherited;
end;

function TPluginSystem.LoadMasterLibrary(APath: AnsiString):Cardinal;
begin
  Result := 3;
  if LoadDll(APath) then Begin
    If FPluginsCount > 0 Then Begin
      If FPlugins[0].InitProc(PE_MASTERINIT, @MasterInit) Then
        Result := MasterInit.Result
      Else
        Result := 3;
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
  {$IFDEF DEBUG}
  Write('Plugins: Loading library from ', ADllPath, ' ... ');
  {$ENDIF}
  ADllPath := ADllPath + #0;

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
        case Descriptors^.Descriptors[j].Descriptor of
          PD_APIEXPORT: FPlugins[FPluginsCount].ExportedAPI := DuplicatePluginsAPIStruct(PPluginAPIInfo(Descriptors^.Descriptors[j].Value));
          PD_NAME: FPlugins[FPluginsCount].Name := Descriptors^.Descriptors[j].Data;
        end;
      End;
      FPlugins[FPluginsCount].InitProc := FDlls[FDllCount].PluginsInfo^.Plugins[i].InitProcedure;
      FPlugins[FPluginsCount].Trampolines := MakeAPIForPlugin(FPluginsCount, False);
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
  EnterCriticalSection(FThreadLocker);
  If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
    If Assigned(FPlugins[iPluginPos].ProtocolHandlers[PByte(Data)^]) then Begin
      Result := FPlugins[iPluginPos].ProtocolHandlers[PByte(Data)^](Data, Size, bSend, False);
    End;
    if Result or not bSend then Begin
      Result := bSend;
      Exit;
    End;
  End;
  LeaveCriticalSection(FThreadLocker);
  Result := bSend;
end;

function TPluginSystem.ServerToClientPacket(Data: Pointer; var Size: Cardinal): Boolean;
var
  iPluginPos: Cardinal;
  bSend: Boolean;
begin
  bSend := True;
  Result := False;
  EnterCriticalSection(FThreadLocker);
  If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
    if Assigned(FPlugins[iPluginPos].ProtocolHandlers[PByte(Data)^]) then Begin
      Result := FPlugins[iPluginPos].ProtocolHandlers[PByte(Data)^](Data, Size, bSend, True);
    End;
    if Result or not bSend then Begin
      Result := bSend;
      Exit;
    End;
  End;
  LeaveCriticalSection(FThreadLocker);
  Result := bSend;
end;

procedure TPluginSystem.Init;
var
  iPluginPos: Cardinal;
begin
  If (FPluginsCount > 0) Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
    try
      TPluginProcedure(FPlugins[iPluginPos].InitProc)(PE_INIT, FPlugins[iPluginPos].Trampolines);
    except
      {$IFDEF DEBUG}
        WriteLn('Plugins: Exception disarmed in plugin ', iPluginPos);
        ReadLn;
      {$ENDIF}
      Halt(1);
    end;
  End;
end;

procedure TPluginSystem.Init(PluginHandle: Word);
begin
  if (FPluginsCount > PluginHandle) then Begin
    try
      TPluginProcedure(FPlugins[PluginHandle].InitProc)(PE_INIT, FPlugins[PluginHandle].Trampolines);
    except
      {$IFDEF DEBUG}
        WriteLn('Plugins: Exception disarmed in plugin ', PluginHandle);
        ReadLn;
      {$ENDIF}
      Halt(1);
    end;
  End;
end;

procedure TPluginSystem.ProxyStart;
var
  iPluginPos: Cardinal;
begin
  If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
    EnterCriticalSection(FThreadLocker);
    ZeroMemory(@FPlugins[iPluginPos].ProtocolHandlers, SizeOf(TProtocolHandlerArray));
    LeaveCriticalSection(FThreadLocker);
    try
      TPluginProcedure(FPlugins[iPluginPos].InitProc)(PE_PROXYSTART, nil);
    except
      {$IFDEF DEBUG}
        WriteLn('Plugins: Exception disarmed in plugin ', iPluginPos);
        ReadLn;
      {$ENDIF}
      Halt(1);
    end;
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
    try
      TPluginProcedure(FPlugins[iPluginPos].InitProc)(PE_PROXYEND, @Arg);
    except
      {$IFDEF DEBUG}
        WriteLn('Plugins: Exception disarmed in plugin ', iPluginPos);
        ReadLn;
      {$ENDIF}
      Halt(1);
    end;
    EnterCriticalSection(FThreadLocker);
    ZeroMemory(@FPlugins[iPluginPos].ProtocolHandlers, SizeOf(TProtocolHandlerArray));
    LeaveCriticalSection(FThreadLocker);
  End;
end;

procedure TPluginSystem.DeInit;
var
  iPluginPos: Cardinal;
begin
  If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
    EnterCriticalSection(FThreadLocker);
    ZeroMemory(@FPlugins[iPluginPos].ProtocolHandlers, SizeOf(TProtocolHandlerArray));
    LeaveCriticalSection(FThreadLocker);
    try
      TPluginProcedure(FPlugins[iPluginPos].InitProc)(PE_FREE, nil);
    except
      {$IFDEF DEBUG}
        WriteLn('Plugins: Exception disarmed in plugin ', iPluginPos);
        ReadLn;
      {$ENDIF}
      Halt(1);
    end;
  End;
end;

procedure TPluginSystem.PacketSended(Header: Byte; IsFromServerToClient: Boolean);
var
  iPluginPos: Cardinal;
begin
  EnterCriticalSection(FThreadLocker);
  If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
    if Assigned(FPlugins[iPluginPos].OnPacketSended) then Begin
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
    End;
  End;
  LeaveCriticalSection(FThreadLocker);
end;

procedure TPluginSystem.RegisterPacketHandler(APlugin:Cardinal; Header:Byte; Handler: TPacketHandler);
Begin
  EnterCriticalSection(FThreadLocker);
  FPlugins[APlugin].ProtocolHandlers[Header] := Handler;
  LeaveCriticalSection(FThreadLocker);
End;

procedure TPluginSystem.UnRegisterPacketHandler(APlugin:Cardinal; Header: Byte; Handler: TPacketHandler);
begin
  EnterCriticalSection(FThreadLocker);
  FPlugins[APlugin].ProtocolHandlers[Header] := nil;
  LeaveCriticalSection(FThreadLocker);
end;

function TPluginSystem.AfterPacketCallback(APlugin:Cardinal; ACallBack: TPacketSendedCallback; lParam: Pointer): Boolean;
begin
  EnterCriticalSection(FThreadLocker);
  Result := not Assigned(FPlugins[APlugin].OnPacketSended);
  if Result then Begin
    FPlugins[APlugin].OnPacketSended := ACallBack;
    FPlugins[APlugin].OnPacketSendedParam := lParam;
  End;
  LeaveCriticalSection(FThreadLocker);
end;

function TPluginSystem.APISearch(APlugin:Cardinal; APluginName: PAnsiChar; AnAPIName: PAnsiChar; Flags: PCardinal): Pointer;
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
