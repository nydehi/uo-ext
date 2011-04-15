unit Plugins;

interface

uses Windows, PluginsShared;

const
  MAXPLUGINIDONPAGE = 9;
type
  PPluginPage = ^TPluginPage;
  TPluginPage = record
    Plugins: Array [0..MAXPLUGINIDONPAGE] of TObject;
    Next: PPluginPage;
  end;

  TOnRegisterPacketHandler = procedure( Sender: TObject; Header:Byte; Handler: TPacketHandler) of object;
  TOnRegisterPacketType = function(Sender: TObject; Header: Byte; Size: Word): Boolean;

  TPlugins=class
  private
    FInitialized: Boolean;
    FPlugins: PPluginPage;
    FCount: Cardinal;
    FOnRegisterPacketHandler: TOnRegisterPacketHandler;
    FOnUnregisterPacketHandler: TOnRegisterPacketHandler;
    FOnRegisterPacketType: TOnRegisterPacketType;
    FSyncEventCount: Integer;

    FCurrentPluginId: Byte;
    FCurrentPluginPage: PPluginPage;
    function First:Boolean;
    function Next:Boolean;
  public
    property Count: Cardinal read FCount;
    property OnRegisterPacketHandler: TOnRegisterPacketHandler read FOnRegisterPacketHandler write FOnRegisterPacketHandler;
    property OnUnRegisterPacketHandler: TOnRegisterPacketHandler read FOnUnregisterPacketHandler write FOnUnregisterPacketHandler;
    property OnRegisterpacketType: TOnRegisterPacketType read FOnRegisterPacketType write FOnRegisterPacketType;
    function AddPlugin(APluginPath: AnsiString): Boolean;
    procedure Initialize;
    procedure ProxyStart;
    procedure ProxyEnd;
    function ClienToServerPacket(Data: Pointer; var Size:Cardinal): Boolean;
    function ServerToClientPacket(Data: Pointer; var Size:Cardinal): Boolean;
    procedure CheckSyncEvent;
    constructor Create;
    destructor Destroy; override;
  end;

var
  PluginSystem: TPlugins;

implementation

uses Common
, ClientThread, ProtocolDescription, Serials, zLib, HookLogic;

type
  TProtocolHandlerArray = array [0..255] of TPacketHandler;

  TPlugin=class
  private
    FPluginPath: AnsiString;
    FHandlers: TProtocolHandlerArray;
    FDllHandle: THandle;
    FLoaded: Boolean;
    FSyncEventCount: Integer;
    FEventCallback: TSyncEvent;
    function CallInitializationProc:Boolean;
  public
    property PluginPath: AnsiString read FPluginPath;
    property SyncEventCount: Integer read FSyncEventCount;
    constructor Create(APluginPath: AnsiString);
    destructor Destroy; override;

    function Load: Boolean;
    procedure ProxyStart;
    procedure ProxyEnd;
    function HandlePacket(Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean;
    procedure RegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
    procedure UnRegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
  end;

const
  LastAPIFuncNum = 12;
var
  aCurrentPlugin : TPlugin;
  API:Array [0 .. LastAPIFuncNum] of RAPIFunc;

// Plugin functions

procedure RegisterPacketHandler(Header:Byte; Handler: TPacketHandler) stdcall;
begin
  aCurrentPlugin.RegisterPacketHandler(Header, Handler);
end;

procedure UnRegisterPacketHandler(Header:Byte; Handler: TPacketHandler) stdcall;
begin
  aCurrentPlugin.UnRegisterPacketHandler(Header, Handler);
end;

function SendPacket(Packet: Pointer; Length: Cardinal; ToServer, Direct: Boolean; var Valid: Boolean):Boolean; stdcall;
begin
  Result := False;
  If not Assigned(CurrentClientThread) Then Exit;
  Result := CurrentClientThread.SendPacket(Packet, Length, ToServer, Direct, Valid);
end;

procedure RegisterPacketType(IsCliServ:Boolean; Header:Byte; Size:Word; HandleProc: TPacketLengthDefinition) stdcall;
begin
  If not Assigned(HandleProc) Then
    ProtocolDescriptor.AddPacketInfo(IsCliServ, Header, Size)
  else
    ProtocolDescriptor.AddPacketInfo(IsCliServ, Header, HandleProc);
end;

function RegisterSyncEventHandler(Event: TSyncEvent): Pointer; stdcall;
begin
  aCurrentPlugin.FEventCallback := Event;
  Result := @aCurrentPlugin.FSyncEventCount;
end;

procedure AskSyncEvent(InterlockedValue: Pointer); stdcall;
begin
  InterlockedIncrement(PInteger(InterlockedValue)^);
  InterlockedIncrement(PluginSystem.FSyncEventCount);
end;

// TPlugin

constructor TPlugin.Create(APluginPath: AnsiString);
begin
  Inherited Create;
  FPluginPath := APluginPath;
  FLoaded := False;
end;

destructor TPlugin.Destroy;
begin
  If FLoaded Then FreeLibrary(FDllHandle);
  Inherited;
end;

function TPlugin.CallInitializationProc:Boolean;
var
  funcIp: TUOExtInit;
Begin
  Result := False;
  @funcIp := GetProcAddress(FDllHandle, 'UOExtInit');
  If not Assigned(funcIp) Then Exit;
  aCurrentPlugin := Self;
  funcIp(LastAPIFuncNum, @API[0]);
  aCurrentPlugin := nil;
  Result := True;
End;

function TPlugin.Load: Boolean;
begin
  Result := False;
  If not FileExists(FPluginPath) then Exit;
  FDllHandle := LoadLibraryA(PAnsiChar(FPluginPath));
  If FDllHandle = INVALID_HANDLE_VALUE Then Exit;
  If not CallInitializationProc Then Begin
    FreeLibrary(FDllHandle);
    Exit;
  End;
  FLoaded := True;
  Result := True;
end;

function TPlugin.HandlePacket(Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean;
begin
  aCurrentPlugin := Self;
  If Assigned(FHandlers[PByte(Data)^]) Then
    Result := FHandlers[PByte(Data)^](Data, Size, Send, IsFromServerToClient)
  else
    Result := False;
  aCurrentPlugin := nil;
end;

procedure TPlugin.RegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
begin
  FHandlers[Header] := Handler;
end;

procedure TPlugin.UnRegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
begin
  FHandlers[Header] := nil;
end;

procedure TPlugin.ProxyEnd;
var
  funcCp: TProxyEnd;
Begin
  @funcCP := GetProcAddress(FDllHandle, 'ProxyEnd');
  If Assigned(funcCP) Then Begin
    aCurrentPlugin := Self;
    funcCP;
    aCurrentPlugin := nil;
  End;
End;

procedure TPlugin.ProxyStart;
var
  funcCp: TProxyStart;
Begin
  @funcCP := GetProcAddress(FDllHandle, 'ProxyStart');
  If Assigned(funcCP) Then Begin
    aCurrentPlugin := Self;
    funcCP;
    aCurrentPlugin := nil;
  End;
End;

// TPlugins

constructor TPlugins.Create;
begin
  Inherited Create;
  FPlugins := nil;
  FCount := 0;
  FInitialized := False;
end;

destructor TPlugins.Destroy;
var
  CPlugins, OldPlugins: PPluginPage;
  i: Byte;
begin
  If FPlugins <> nil Then Begin
    CPlugins := FPlugins;
    while CPlugins <> nil do begin
      For i:= 0 to 9 do If CPlugins^.Plugins[i] <> nil Then CPlugins^.Plugins[i].Free;
      OldPlugins := CPlugins;
      CPlugins := CPlugins^.Next;
      FreeMemory(OldPlugins);
    end;
  End;
  Inherited;
end;

function TPlugins.First:Boolean;
Begin
  Result := FPlugins <> nil;
  If Result Then Begin
    FCurrentPluginPage := FPlugins;
    FCurrentPluginId := 0;
    If FPlugins^.Plugins[0] = nil Then Result := Next;
  End;
End;

function TPlugins.Next:Boolean;
var
  i: Byte;
Begin
  Result := True;
  If FCurrentPluginId <> MAXPLUGINIDONPAGE Then For i := FCurrentPluginId + 1 to MAXPLUGINIDONPAGE do Begin
    If FCurrentPluginPage^.Plugins[i] <> nil Then Begin
      FCurrentPluginId := i;
      Exit;
    End;
  End;
  If FCurrentPluginPage^.Next = nil Then Begin
    Result := False;
    Exit;
  End;
  FCurrentPluginPage := FCurrentPluginPage^.Next;
  FCurrentPluginId := 0;
  If FCurrentPluginPage^.Plugins[0] = nil Then Result := Next;
End;

function TPlugins.AddPlugin(APluginPath: AnsiString): Boolean;
var
  CPlugins: PPluginPage;
  CPlugin: TPlugin;
  i: Byte;
begin
  Result := True;
  If FPlugins <> nil Then Begin
    CPlugins := FPlugins;
    while CPlugins <> nil do begin
      For i:= 0 to 9 do If Assigned(CPlugins^.Plugins[i]) and (TPlugin(CPlugins^.Plugins[i]).PluginPath = APluginPath) Then Exit;
      CPlugins := CPlugins^.Next;
    end;
  End Else Begin
    FPlugins := GetMemory(SizeOf(TPluginPage));
    ZeroMemory(FPlugins, SizeOf(TPluginPage));
  End;
  If not FileExists(APluginPath) Then Begin
    Result := False;
    Exit;
  End;
  CPlugin := TPlugin.Create(APluginPath);
  If not CPlugin.Load Then begin
    Result := False;
    CPlugin.Free;
    Exit;
  End;
  CPlugins := FPlugins;
  repeat
    For i := 0 to 9 do If CPlugins^.Plugins[i] = nil Then Begin
      CPlugins^.Plugins[i] := CPlugin;
      Exit;
    End;
    If CPlugins^.Next <> nil Then
      CPlugins := CPlugins^.Next
    Else
      Break;
  until False;
  CPlugins^.Next := GetMemory(SizeOf(TPluginPage));
  ZeroMemory(CPlugins^.Next, SizeOf(TPluginPage));
  CPlugins^.Next^.Plugins[0] := CPlugin;
end;

procedure TPlugins.Initialize;
var
  sPlgFolder, sSearch: AnsiString;
  SR: WIN32_FIND_DATAA;
  hSearch: THandle;
Begin
  if FInitialized then begin
    MessageBox(0, 'You found it!', nil, MB_OK);
  End;
  {$IFDEF DEBUG}
  WriteLn('Plugins: Loading plugins from HDD.');
  {$ENDIF}
  sPlgFolder := ExtractFilePath(AnsiString(ParamStr(0))) + 'Plugins\';
  sSearch := sPlgFolder + '*.dll' + #0;
  ZeroMemory(@SR, SizeOf(SR));
  hSearch := FindFirstFileA(@sSearch[1], SR);
  If hSearch <> INVALID_HANDLE_VALUE Then Begin
    Repeat
      If (SR.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY Then Continue;
{$IFDEF DEBUG}
      Write('Plugins: Loading plugin: ', SR.cFileName, ' ... ');
{$ENDIF}
      AddPlugin(sPlgFolder + SR.cFileName);
{$IFDEF DEBUG}
      WriteLn('done.');
{$ENDIF}
    until not FindNextFileA(hSearch, SR);
    Windows.FindClose(hSearch);
  End;
  FInitialized := True;
End;

function TPlugins.ClienToServerPacket(Data: Pointer; var Size:Cardinal): Boolean;
var
  bSend: Boolean;
begin
  bSend := True;
  If First Then repeat
    Result := TPlugin(FCurrentPluginPage^.Plugins[FCurrentPluginId]).HandlePacket(Data, Size, bSend, False);
    If Result or not bSend Then Break;
  until not Next;
  Result := bSend;
end;

function TPlugins.ServerToClientPacket(Data: Pointer; var Size:Cardinal): Boolean;
var
  bSend: Boolean;
begin
  bSend := True;
  If First Then repeat
    Result := TPlugin(FCurrentPluginPage^.Plugins[FCurrentPluginId]).HandlePacket(Data, Size, bSend, True);
    If Result or not bSend Then Break;
  until not Next;
  Result := bSend;
end;

procedure TPlugins.CheckSyncEvent;
var
  cPlugin: TPlugin;
begin
  If InterlockedExchange(FSyncEventCount, 0) > 0 Then begin
    If First Then repeat
      cPlugin := TPlugin(FCurrentPluginPage^.Plugins[FCurrentPluginId]);
      If InterlockedExchange(cPlugin.FSyncEventCount, 0) > 0 Then Begin
        If Assigned(cPlugin.FEventCallback) Then Begin
          cPlugin.FEventCallback();
        End;
      End;
    until not Next;
  End;
end;

procedure TPlugins.ProxyEnd;
begin
  If First Then repeat
    TPlugin(FCurrentPluginPage^.Plugins[FCurrentPluginId]).ProxyEnd;
  until not Next;
end;

procedure TPlugins.ProxyStart;
begin
  If First Then repeat
    TPlugin(FCurrentPluginPage^.Plugins[FCurrentPluginId]).ProxyStart;
  until not Next;
end;

initialization
  PluginSystem := TPlugins.Create;
  API[0].FuncType := PF_REGISTERPACKETHANDLER;
  API[0].Func := @RegisterPacketHandler;
  API[1].FuncType := PF_UNREGISTERPACKETHANDLER;
  API[1].Func := @UnRegisterPacketHandler;
  API[2].FuncType := PF_REGISTERPACKETTYPE;
  API[2].Func := @RegisterPacketType;
  API[3].FuncType := PF_SENDPACKET;
  API[3].Func := @SendPacket;
  API[4].FuncType := PF_GETNEWSERIAL;
  API[4].Func := @GetNewSerial;
  API[5].FuncType := PF_FREESERIAL;
  API[5].Func := @FreeSerial;
  API[6].FuncType := PF_GETSERVERSERIAL;
  API[6].Func := @GetServerSerial;
  API[7].FuncType := PF_GETCLIENTSERIAL;
  API[7].Func := @GetClientSerial;
  API[8].FuncType := PF_REGISTERSYNCEVENTHANDLER;
  API[8].Func := @RegisterSyncEventHandler;
  API[9].FuncType := PF_ASKSYNCEVENT;
  API[9].Func := @AskSyncEvent;
  API[10].FuncType := PF_ZLIBCOMPRESS2;
  API[10].Func := @zLib.zCompress2;
  API[11].FuncType := PF_ZLIBDECOMPRESS;
  API[11].Func := @zLib.zDecompress;
//  API[12].FuncType := PF_GETOPENEDHANDLEFORDOSNAME;
//  API[12].Func := @HookLogic.GetOpenedHandleForDOSName;
finalization
  PluginSystem.Free;
end.
