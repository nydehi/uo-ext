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
    TUOExtPacketHandlerInfo = packed record
      Handler: TUOExtPacketHandler;
      Plugin: Byte;
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

    //Network
    TDllServerInfo = packed record
      Size: Cardinal;
      MD5: Array [0..15] of Byte;
    end;
    PDllServerInfo = ^TDllServerInfo;
    TDllServerInfoList = packed record
      Amount: Byte;
      Items: Array [0..0] of TDllServerInfo;
    end;
    PDllServerInfoList = ^TDllServerInfoList;
    TPluginInitInfo = packed record
      Dll:Byte;
      Plugin: Byte;
      PluginHandle: Byte;
      PacketsCount: Byte;
    end;
    PPluginInitInfo = ^TPluginInitInfo;
    TPluginInitInfoList = packed record
      Amount: Byte;
      Items: Array [0..0] of TPluginInitInfo;
    end;
    PPluginInitInfoList = ^TPluginInitInfoList;

  strict private var
    FDlls: Array of TDllInfo;
    FDllCount: Cardinal;
    FDllPos: Cardinal;
    FDllInit: Boolean;

    FPlugins: Array of TPluginInfo;
    FPluginsCount: Cardinal;

    FAPILoaded: Boolean;

    FSocket: TSocket;

    FActivePlugin: Cardinal;

    // Network
    FDllServerInfoList: PDllServerInfoList;
    FPluginInitInfoList: PPluginInitInfoList;
    FUOExtProtocolHandlers: TUOExtProtocolHandlerArray;

    constructor Create;
    procedure ProcessLoadingList;
    function LoadDll(ADllPath: AnsiString): Boolean;
    procedure LoadAPIFromList;

    // Network
    function UOExtGetPacket(var Size: Cardinal): Pointer;

    function UOExtConnect: Boolean;
    function UOExtGetConfig: Boolean;
    function UOExtGetDllList: Boolean;
    function UOExtGetPluginsLoadingList: Boolean;
    function UOExtGetDlls(WantedList: PByteArray; WantedSize: Word): Boolean;
    procedure UOExtDisconnect;
    function UOExtUpdateServerConnect:Boolean;

    function UOExtGetMissingDlls(var Dlls:Pointer; var Count: Byte): Boolean;
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
    function GetDllsFromServer: Boolean;
    procedure Initialize;
    procedure ProxyStart;
    property Socket: TSocket read FSocket;
    procedure ProxyEnd(ServStatus, CliStatus: Integer);
    function ClientToServerPacket(Data: Pointer; var Size:Cardinal): Boolean;
    function ServerToClientPacket(Data: Pointer; var Size:Cardinal): Boolean;
    procedure CheckSyncEvent;
    procedure PacketSended(Header: Byte; IsFromServerToClient: Boolean);

    function UOExtPacket(Data:Pointer; Size:Cardinal; Internal: Boolean): Boolean;

    {Control from Plugins}

    procedure RegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
    procedure UnRegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
    function AfterPacketCallback(ACallBack: TPacketSendedCallback; lParam: Pointer): Boolean;
    function RegisterSyncEventHandler(Event: TSyncEvent): Pointer;

    procedure UOExtRegisterPacketHandler(Header:Byte; Handler: TUOExtPacketHandler);
    procedure UOExtUnRegisterPacketHandler(Header:Byte; Handler: TUOExtPacketHandler);
    function UOExtAfterPacketCallback(ACallBack: TUOExtPacketSendedCallback; lParam: Pointer): Boolean;

    destructor Destory;
  end;

implementation

uses ClientThread, ProtocolDescription, Serials, zLib, HookLogic
, ShardSetup, PreConnectIPDiscover, GUI;

const
  LastAPIFuncNum = 19;
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

procedure UOExtRegisterPacketHandler(Header:Byte; Handler: TUOExtPacketHandler) stdcall;
begin
  TPluginSystem.Instance.UOExtRegisterPacketHandler(Header, Handler);
end;

procedure UOExtUnRegisterPacketHandler(Header:Byte; Handler: TUOExtPacketHandler) stdcall;
begin
  TPluginSystem.Instance.UOExtUnRegisterPacketHandler(Header, Handler);
end;

function UOExtAfterPacketCallback(ACallBack: TUOExtPacketSendedCallback; lParam: Pointer):Boolean; stdcall;
Begin
  Result := TPluginSystem.Instance.UOExtAfterPacketCallback(ACallBack, lParam);
End;

function UOExtSendPacket(Packet: Pointer; Length: Cardinal):Boolean; stdcall;
begin
  Result := False;
  If not Assigned(CurrentClientThread) Then Exit;
  Result := TPluginSystem.Instance.UOExtPacket(Packet, Length, False);
end;

// TPluginSystem

class constructor TPluginSystem.CCreate;
Begin
  FInstance := TPluginSystem.Create;
End;

constructor TPluginSystem.Create;
begin
  Inherited;
  SetLength(FDlls, 0);
  SetLength(FPlugins, 0);
  FDllCount := 0;
  FDllPos := 0;
  FPluginsCount := 0;
  FSocket := 0;
  FActivePlugin := MAXDWORD;
  FDllServerInfoList := nil;
  FPluginInitInfoList := nil;
  ZeroMemory(@FUOExtProtocolHandlers, SizeOf(FUOExtProtocolHandlers));
end;

destructor TPluginSystem.Destory;
begin
  SetLength(FDlls, 0);
  SetLength(FPlugins, 0);
  if FSocket <> 0 then Begin
    closesocket(FSocket);
    FSocket := 0;
  End;
  if Assigned(FDllServerInfoList) then FreeMemory(FDllServerInfoList);
  if Assigned(FPluginInitInfoList) then FreeMemory(FPluginInitInfoList);
  Inherited;
end;

procedure TPluginSystem.Initialize;
begin
  ProcessLoadingList;

  LoadAPIFromList;

  UOExtDisconnect;
end;

procedure TPluginSystem.ProcessLoadingList;
var
  i, j :Byte;
  Path, Dll: AnsiString;
Begin
  if not Assigned(FDllServerInfoList) then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: Dll list is nil. Can''t continue.');
    {$ENDIF}
    Halt(1);
  End;
  if FDllServerInfoList^.Amount = 0 then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: Warning: Dll list is empty. This might be an error, but I will continue.');
    {$ENDIF}
    Exit;
  End;

  FDllCount := FDllServerInfoList^.Amount;
  SetLength(FDlls, FDllCount);
  FDllPos := 0;
  FPluginsCount := 0;
  Path := ExtractFilePath(AnsiString(ParamStr(0))) + 'Plugins\';

  For i := 0 to FDllCount - 1 Do Begin
    Dll := '';
    For j := 0 to 15 do Dll := Dll + IntToHex(FDllServerInfoList^.Items[i].MD5[j], 2);
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

procedure TPluginSystem.LoadAPIFromList;
var
  i, j: Cardinal;
  CurrentPluginInfo: PPluginInitInfo;
  CurrentPluginDescriptors : PluginsShared.PPluginInfo;
  InitDone: TDllInitDone;
  CurrentUOExtPacketStart: Byte;
begin
  if FAPILoaded then Exit;
  if not FDllInit then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: No libraries loaded. Nothing to initialize.');
    {$ENDIF}
    Exit;
  End;
  if not Assigned(FPluginInitInfoList) then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: Plugins list is nil. Can''t continue.');
    {$ENDIF}
    Halt(1);
  End;
  FAPILoaded := True;
  if FPluginInitInfoList^.Amount = 0 then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: Warning: Plugins list is empty. This might be an error, but I will continue.');
    {$ENDIF}
    Exit;
  End;
  SetLength(FPlugins, FPluginInitInfoList^.Amount);

  CurrentUOExtPacketStart := 1;
  For i := 0 to FPluginInitInfoList^.Amount - 1 Do Begin
    CurrentPluginInfo := @FPluginInitInfoList^.Items[i];
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

    CurrentPluginDescriptors := @FDlls[CurrentPluginInfo^.Dll].PluginsInfo^.Plugins[CurrentPluginInfo^.Plugin];
    If CurrentPluginDescriptors^.DescriptorsCount > 0 Then For j := 0 to CurrentPluginDescriptors^.DescriptorsCount - 1 Do Begin
      if CurrentPluginDescriptors^.Descriptors[j].Descriptor = PD_UOEXTPROTO_PACKETAMOUNT then FPlugins[i].UOExtPacketAmount := CurrentPluginDescriptors^.Descriptors[j].Value;
    End;

    If TPluginProcedure(FDlls[CurrentPluginInfo^.Dll].PluginsInfo^.Plugins[CurrentPluginInfo^.Plugin].InitProcedure)(PE_INIT, @API) Then Begin
      FPlugins[i].InitProc := FDlls[CurrentPluginInfo^.Dll].PluginsInfo^.Plugins[CurrentPluginInfo^.Plugin].InitProcedure;
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
  FPluginsCount := FPluginInitInfoList^.Amount;
  FDllCount := FDllServerInfoList^.Amount;
  FreeMemory(FDllServerInfoList);
  FreeMemory(FPluginInitInfoList);
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

function TPluginSystem.GetDllsFromServer: Boolean;
var
  MissingDllsCount: Byte;
  MissingDlls: Pointer;
Begin
  Result := False;
  MissingDlls := nil;
  If not Self.UOExtConnect then Exit;
  if not Self.UOExtGetConfig then Exit;


  if not Self.UOExtGetDllList then Exit;
  if not Self.UOExtGetPluginsLoadingList then Exit;
  if not Self.UOExtGetMissingDlls(MissingDlls, MissingDllsCount) then Exit;
  if not Self.UOExtGetDlls(MissingDlls, MissingDllsCount) then Exit;
  If Assigned(MissingDlls) Then FreeMemory(MissingDlls);
  Result := True;
End;

function TPluginSystem.UOExtConnect: Boolean;
var
  IP: Cardinal;
  Port: Word;

  WSAData: TWSAData;
  SockAddr: TSockAddr;
  bufPacket: Array [0..20] of Byte;
  bNoDelay: BOOL;
begin
  Result := False;
  If not PreConnectIPDiscover.GetConnInfo(IP, Port) Then Exit;
  ShardSetup.UpdateIP := IP;
  WSAStartup($101, WSAData);

  FSocket := WinSock.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  IF FSocket=INVALID_SOCKET Then Exit;
  ZeroMemory(@SockAddr, SizeOf(SockAddr));
  SockAddr.sin_family:=AF_INET;

  SockAddr.sin_port:=htons(Port);
  SockAddr.sin_addr.S_addr:=htonl(IP);

  If WinSock.connect(FSocket, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR Then Begin
    closesocket(FSocket);
    Exit;
  End;
  bNoDelay := True;
  setsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY, @bNoDelay, SizeOf(bNoDelay));

  ZeroMemory(@bufPacket[0], 21);
  bufPacket[0] := $EF;
  If send(FSocket, bufPacket, 21, 0) <> 21 Then Begin
    closesocket(FSocket);
    Exit;
  End;
  Result := True;
end;

function TPluginSystem.UOExtGetConfig:Boolean;
const
  bufSize = 65535;
var
  WriteOffSet, PacketSize: Cardinal;
  recvResult: Integer;
  startGTC, currGTC: Cardinal;
  fdset: TFDSet;
  tv: TTimeVal;

  bufPacket: Array [0..bufSize] of Byte;
begin
  Result := False;

  WriteOffSet := 0;
  PacketSize := 0;
  startGTC := GetTickCount;
  Repeat
    FD_ZERO(fdset);
    FD_SET(FSocket, fdset);
    currGTC := GetTickCount;
    tv.tv_sec := 5 - (currGTC - startGTC) DIV 1000;
    tv.tv_usec := 5 - (currGTC - startGTC) MOD 1000;
    select(0, @fdset, nil, nil, @tv);
    If FD_ISSET(FSocket, fdset) then Begin
      recvResult := recv(FSocket, Pointer(@bufPacket[WriteOffSet])^, bufSize - WriteOffSet, 0);
      If recvResult <= 0 then Begin
        closesocket(FSocket);
        Exit;
      End;

      WriteOffSet := WriteOffSet + Cardinal(recvResult);
      If PacketSize = 0 then Begin
        If WriteOffSet > 3 then
          if bufPacket[0] <> 0 then Begin
            closesocket(FSocket);
            Exit;
          End;
          PacketSize := htons(PWord(@bufPacket[1])^);
      End;
    End;
  Until ((WriteOffSet >= PacketSize)AND(PacketSize > 0))OR((GetTickCount - startGTC) DIV 1000 > 5);

  If(WriteOffSet < 3)OR(PacketSize < htons(PWord(@bufPacket[1])^)) Then Begin
    {$IFDEF DEBUG}
    WriteLn('UOExtProtocol: Server didn''t respond on UOExt support ask.');
    {$ENDIF}
    closesocket(FSocket);
    Exit;
  End;
  {
    BYTE Header = $FF
    WORD Size
    BYTE Flags 0x01 = Encrypted
    BYTE UOExtHeader
  }

  ShardSetup.Encrypted := bufPacket[3] AND $01 = $01;
  ShardSetup.InternalProtocolHeader := bufPacket[4];
  if(bufPacket[3] AND $02 = $02) Then Begin
    if PCardinal(@bufPacket[5])^ <> 0 then ShardSetup.UpdateIP := PCardinal(@bufPacket[5])^;
    ShardSetup.UpdatePort := htons(PCardinal(@bufPacket[9])^);
    UOExtDisconnect;
    Result := UOExtUpdateServerConnect;
    ShardSetup.UsingUpdateServer := true;
  End Else Begin
    Result := True;
  End;
end;

procedure TPluginSystem.UOExtDisconnect;
begin
  closesocket(FSocket);
  FSocket := INVALID_SOCKET;
  ShardSetup.UsingUpdateServer := False;
  WSACleanup;
end;

function TPluginSystem.UOExtUpdateServerConnect;
var
  WSAData: TWSAData;
  SockAddr: TSockAddr;
  bNoDelay: BOOL;
begin
  Result := False;
  WSAStartup($101, WSAData);

  FSocket := WinSock.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  IF FSocket=INVALID_SOCKET Then Exit;
  ZeroMemory(@SockAddr, SizeOf(SockAddr));
  SockAddr.sin_family:=AF_INET;

  SockAddr.sin_port:=htons(ShardSetup.UpdatePort);
  SockAddr.sin_addr.S_addr:=htonl(ShardSetup.UpdateIP);

  If WinSock.connect(FSocket, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR Then Begin
    closesocket(FSocket);
    Exit;
  End;
  bNoDelay := True;
  setsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY, @bNoDelay, SizeOf(bNoDelay));
  Result := True;
end;

function TPluginSystem.UOExtGetDllList: Boolean;
const
  Packet: Cardinal = 0;
var
  recvPacket: Pointer;
  PacketSize: Cardinal;
  DllListSize: Byte;
Begin
  Result := False;
  Self.UOExtPacket(@Packet, 3, True);

  // Handshake
  recvPacket := Self.UOExtGetPacket(PacketSize);
  if recvPacket = nil then Exit;

  if (PWord(recvPacket)^ <> 0) then Begin
    FreeMemory(recvPacket);
    Exit;
  End;

  // DllList
  recvPacket := Self.UOExtGetPacket(PacketSize);
  if recvPacket = nil then Exit;
  if (PByte(recvPacket)^ <> 0) OR (PByte(Cardinal(recvPacket) + 1)^ <> 1) then Begin
    FreeMemory(recvPacket);
    Exit;
  End;
  DllListSize := (PacketSize - 2) DIV 20;
  FDllServerInfoList := GetMemory(SizeOf(TDllServerInfo) * DllListSize + 1);
  FDllServerInfoList.Amount := DllListSize;
  CopyMemory(@FDllServerInfoList.Items[0], Pointer(Cardinal(recvPacket) + 2), SizeOf(TDllServerInfo) * DllListSize);
  FreeMemory(recvPacket);

  Result := True;
End;

function TPluginSystem.UOExtGetPluginsLoadingList: Boolean;
var
  recvPacket: Pointer;
  PacketSize: Cardinal;
  PluginsListSize: Byte;
Begin
  Result := False;

  // PluginsList
  recvPacket := Self.UOExtGetPacket(PacketSize);
  if recvPacket = nil then Exit;
  if (PByte(recvPacket)^ <> $00) OR (PByte(Cardinal(recvPacket) + 1)^ <> $02) then Begin
    FreeMemory(recvPacket);
    Exit;
  End;
  PluginsListSize := (PacketSize - 2) DIV 4;
  FPluginInitInfoList := GetMemory(SizeOf(TPluginInitInfo) * PluginsListSize + 1);
  FPluginInitInfoList^.Amount := PluginsListSize;
  CopyMemory(@FPluginInitInfoList^.Items[0], Pointer(Cardinal(recvPacket) + 2), SizeOf(TPluginInitInfo) * PluginsListSize);
  FreeMemory(recvPacket);

  Result := True;
End;

function TPluginSystem.UOExtGetDlls(WantedList: PByteArray; WantedSize: Word): Boolean;
var
  Packet: Pointer;
  PacketSize: Cardinal;
  i, j: Byte;
  DllId: Byte;
  SavedDllLength: Cardinal;
  CurrentDll: PDllServerInfo;
  F: File;
  Path: AnsiString;
Begin
  Result := False;
  If WantedSize > MAXWORD - 5 then Exit;

  PacketSize := WantedSize + 2;
  Packet := GetMemory(PacketSize);
  PByteArray(Packet)^[0] := 0;
  PByteArray(Packet)^[1] := 3;
  CopyMemory(Pointer(Cardinal(Packet) + 2), WantedList, WantedSize);
  // C->S ReqdLibrary
  Self.UOExtPacket(Packet, PacketSize, True);
  Freememory(Packet);

  If FDllServerInfoList.Amount > 0 Then For i := 0 to FDllServerInfoList.Amount -1 do Begin
    Packet := Self.UOExtGetPacket(PacketSize);
    if Packet = nil then Exit;
    If (PByteArray(Packet)^[0] <> 0)OR(PByteArray(Packet)^[1] <> 3) then Begin
      FreeMemory(Packet);
      Exit;
    End;
    DllId := PByteArray(Packet)^[3] SHL 8 + PByteArray(Packet)^[2];
    CurrentDll := @FDllServerInfoList.Items[DllId];
    SavedDllLength := 0;
    Path := '';
    for j := 0 to 15 do Path := Path + IntToHex(CurrentDll^.MD5[j], 2);
    Path := ExtractFilePath(AnsiString(ParamStr(0))) + 'Plugins\' + Path + '.cache';
    AssignFile(F, String(Path));
    Rewrite(F, 1);
    BlockWrite(F, Pointer(Cardinal(Packet) + 4)^, PacketSize - 4);
    FreeMemory(Packet);
    SavedDllLength := SavedDllLength + PacketSize - 4;
    Repeat
      Packet := Self.UOExtGetPacket(PacketSize);
      if Packet = nil then Exit;
      if (PByteArray(Packet)^[0] <> 0) OR (PByteArray(Packet)^[1] <> 4) then Begin
        FreeMemory(Packet);
        CloseFile(F);
        Exit;
      End;
      BlockWrite(f, Pointer(Cardinal(Packet) + 2)^, PacketSize - 2);
      SavedDllLength := SavedDllLength + PacketSize - 2;
      FreeMemory(Packet);
    Until SavedDllLength >= CurrentDll^.Size;
    CloseFile(F);
  End;
  Result := True;
End;

function TPluginSystem.UOExtGetMissingDlls(var Dlls:Pointer; var Count: Byte): Boolean;
var
  i: Byte;
Begin
  Dlls := GetMemory(FDllServerInfoList^.Amount);
  If FDllServerInfoList^.Amount > 0 Then for i := 0 to FDllServerInfoList^.Amount - 1 do PByteArray(Dlls)^[i] := i;
  Count := FDllServerInfoList^.Amount;
  Result := True;
End;

procedure TPluginSystem.UOExtRegisterPacketHandler(Header:Byte; Handler: TUOExtPacketHandler);
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

procedure TPluginSystem.UOExtUnRegisterPacketHandler(Header: Byte; Handler: TUOExtPacketHandler);
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
  End;
end;

function TPluginSystem.UOExtAfterPacketCallback(ACallBack: TUOExtPacketSendedCallback; lParam: Pointer): Boolean;
begin
  If FActivePlugin <> MAXDWORD then Begin
    Result := not Assigned(FPlugins[FActivePlugin].OnUOExtPacketSended);
    if Result then Begin
      FPlugins[FActivePlugin].OnUOExtPacketSended := ACallBack;
      FPlugins[FActivePlugin].OnUOExtPacketSendedParam := lParam;
    End;
  End Else Begin
    Result := False;
  End;
end;

Function TPluginSystem.UOExtPacket(Data:Pointer; Size:Cardinal; Internal: Boolean): Boolean;
type
  TUOExtHeader=packed record
    UOHeader: Byte;
    Size: Word;
  end;
var
  Header: TUOExtHeader;
  Buffer: Pointer;
  Valid: Boolean;

  iPluginPos: Cardinal;
Begin
  Result := False;
  if Size + 3 > MAXWORD then Exit;

  Header.UOHeader := ShardSetup.InternalProtocolHeader;
  Header.Size := Size + 2;
  if not ShardSetup.UsingUpdateServer then Header.Size := Header.Size + 1;
  Header.Size := htons(Header.Size);


  if FSocket <> 0 then Begin
    if (ShardSetup.UsingUpdateServer) then Begin
      send(FSocket, Pointer(Cardinal(@Header) + 1)^, SizeOf(Header) - 1, 0);
    End Else Begin
      send(FSocket, Header, SizeOf(Header), 0);
    End;
    send(FSocket, Data^, Size, 0);
    If not Internal Then Begin
      If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
        If Assigned(FPlugins[iPluginPos].OnUOExtPacketSended) then Begin
          FActivePlugin := iPluginPos;
          try
            TUOExtPacketSendedCallback(FPlugins[iPluginPos].OnUOExtPacketSended)(PByte(Data)^, FPlugins[iPluginPos].OnUOExtPacketSendedParam);
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
    End;
  End Else Begin
    Buffer := GetMemory(Size + 3);
    CopyMemory(Buffer, @Header, SizeOf(Header));
    CopyMemory(Pointer(Cardinal(Buffer) + SizeOf(Header)), Data, Size);
    Result := ClientThread.CurrentClientThread.SendPacket(Buffer, Size + 3, True, True, Valid);
    FreeMemory(Buffer);
  End;
End;

function TPluginSystem.UOExtGetPacket(var Size: Cardinal): Pointer;
var
  WriteOffSet: Cardinal;
  recvResult: Integer;
  minSize: Byte;
Begin
  Result := nil;
  if FSocket = 0 then Exit;
  WriteOffSet := 0;
  Size := 0;
  minSize := 2;
  If( not ShardSetup.UsingUpdateServer) Then minSize := minSize + 1;
  Result := GetMemory(minSize);
  Repeat
    recvResult := recv(FSocket, Pointer(Cardinal(Result) + WriteOffSet)^, minSize - WriteOffSet, 0);
    If recvResult <= 0 then Begin
      closesocket(FSocket);
      FSocket := 0;
      FreeMemory(Result);
      Result := nil;
      {$IFDEF DEBUG}
      WriteLn('Plugins: Server closed connection unexpectedly.');
      {$ENDIF}
      Exit;
    End;
    WriteOffSet := WriteOffSet + Cardinal(recvResult);
  Until (WriteOffSet >= minSize);

  If (PByte(Result)^ <> ShardSetup.InternalProtocolHeader)AND (not ShardSetup.UsingUpdateServer) then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: UOExt proto failed. Incomming packet was not UOExt.');
    {$ENDIF}
    Halt(1);
  End;

  if ShardSetup.UsingUpdateServer then
    Size := htons(PWord(Result)^) - 2
  Else
    Size := htons(PWord(Cardinal(Result) + 1)^) - 3;
  FreeMemory(Result);
  Result := GetMemory(Size);
  WriteOffSet := 0;

  Repeat
    recvResult := recv(FSocket, Pointer(Cardinal(Result) + WriteOffSet)^, Size - WriteOffSet, 0);
    If recvResult <= 0 then Begin
      closesocket(FSocket);
      FSocket := 0;
      FreeMemory(Result);
      Result := nil;
      Exit;
    End;
    WriteOffSet := WriteOffSet + Cardinal(recvResult);
  Until (WriteOffSet >= Size);
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
  API.APIs[15].FuncType := PF_UOEXTAFTERPACKETCALLBACK;
  API.APIs[15].Func := @UOExtAfterPacketCallback;
  API.APIs[16].FuncType := PF_UOEXTSENDPACKET;
  API.APIs[16].Func := @UOExtSendPacket;
  API.APIs[17].FuncType := PF_GUISETLOG;
  API.APIs[17].Func := @GUI.GUISetLog;
  API.APIs[18].FuncType := PF_GUISTARTPROCESS;
  API.APIs[18].Func := @GUI.GUIStartProcess;
  API.APIs[19].FuncType := PF_GUIUPDATEPROCESS;
  API.APIs[19].Func := @GUI.GUIUpdateProcess;
end.
