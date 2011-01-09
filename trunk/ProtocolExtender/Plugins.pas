unit Plugins;

interface

uses Windows, PluginsShared;

type
  PPluginPage = ^TPluginPage;
  TPluginPage = record
    Plugins: Array [0..9] of TObject;
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
  public
    property Count: Cardinal read FCount;
    property OnRegisterPacketHandler: TOnRegisterPacketHandler read FOnRegisterPacketHandler write FOnRegisterPacketHandler;
    property OnUnRegisterPacketHandler: TOnRegisterPacketHandler read FOnUnregisterPacketHandler write FOnUnregisterPacketHandler;
    property OnRegisterpacketType: TOnRegisterPacketType read FOnRegisterPacketType write FOnRegisterPacketType;
    function AddPlugin(APluginPath: String): Boolean;
    procedure Initialize;
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
//, SysUtils
, ClientThread, ProtocolDescription, Serials, zLib, HookLogic;

type
  TProtocolHandlerArray = array [0..255] of TPacketHandler;

  TPlugin=class
  private
    FPluginPath: String;
    FHandlers: TProtocolHandlerArray;
    FDllHandle: THandle;
    FLoaded: Boolean;
    FSyncEventCount: Integer;
    FEventCallback: TSyncEvent;
  public
    property PluginPath: String read FPluginPath;
    property SyncEventCount: Integer read FSyncEventCount;
    constructor Create(APluginPath: String);
    destructor Destroy; override;

    function Load: Boolean;
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

constructor TPlugin.Create(APluginPath: String);
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

function TPlugin.Load: Boolean;
var
  funcIp: TInitializationProc;
begin
  Result := False;
  If not FileExists(FPluginPath) then Exit;
  FDllHandle := LoadLibrary(PChar(FPluginPath));
  If FDllHandle = INVALID_HANDLE_VALUE Then Exit;
  @funcIp := GetProcAddress(FDllHandle, 'InitializationProc');
  If not Assigned(funcIp) Then Begin
    FreeLibrary(FDllHandle);
    Exit;
  End;
  aCurrentPlugin := Self;
  funcIp(LastAPIFuncNum, @API[0]);
  aCurrentPlugin := nil;
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

function TPlugins.AddPlugin(APluginPath: String): Boolean;
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
  sPlgFolder, sSearch: String;
  SR: WIN32_FIND_DATA;
  hSearch: THandle;
Begin
  sPlgFolder := ExtractFilePath(ParamStr(0)) + 'Plugins\';
  sSearch := sPlgFolder + '*.plg' + #0;
  ZeroMemory(@SR, SizeOf(WIN32_FIND_DATA));
  hSearch := FindFirstFile(@sSearch[1], SR);
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
    until not FindNextFile(hSearch, SR);
    Windows.FindClose(hSearch);
  End;
End;

function TPlugins.ClienToServerPacket(Data: Pointer; var Size:Cardinal): Boolean;
var
  CPlugins: PPluginPage;
  i: Byte;
  bSend: Boolean;
begin
  Result := False;
  bSend := True;
  If FPlugins = nil Then Exit;
  CPlugins := FPlugins;
  while (CPlugins <> nil) and (not Result) do Begin
    For i:= 0 to 9 do If CPlugins^.Plugins[i] <> nil Then Begin
      Result := TPlugin(CPlugins^.Plugins[i]).HandlePacket(Data, Size, bSend, False);
      If Result or not bSend Then Break;
    End;
    CPlugins := CPlugins.Next;
  End;
  Result := bSend;
end;

function TPlugins.ServerToClientPacket(Data: Pointer; var Size:Cardinal): Boolean;
var
  CPlugins: PPluginPage;
  i: Byte;
  bSend: Boolean;
begin
  Result := False;
  bSend := True;
  If FPlugins = nil Then Exit;
  CPlugins := FPlugins;
  while (CPlugins <> nil) and not (Result or not bSend) do Begin
    For i:= 0 to 9 do If CPlugins^.Plugins[i] <> nil Then Begin
      Result := TPlugin(CPlugins^.Plugins[i]).HandlePacket(Data, Size, bSend, True);
      If Result or not bSend Then Break;
    End;
    CPlugins := CPlugins.Next;
  End;
  Result := bSend;
end;

procedure TPlugins.CheckSyncEvent;
var
  CPlugins: PPluginPage;
  i: Byte;
begin
  If InterlockedExchange(FSyncEventCount, 0) > 0 Then begin
    If FPlugins = nil Then Exit;
    CPlugins := FPlugins;
    while (CPlugins <> nil) do Begin
      For i:= 0 to 9 do If (CPlugins^.Plugins[i] <> nil) Then Begin
        If InterlockedExchange(TPlugin(CPlugins^.Plugins[i]).FSyncEventCount, 0) > 0 Then Begin
          If Assigned(TPlugin(CPlugins^.Plugins[i]).FEventCallback) Then Begin
            aCurrentPlugin := TPlugin(CPlugins^.Plugins[i]);
            TPlugin(CPlugins^.Plugins[i]).FEventCallback();
            aCurrentPlugin := nil;
          End;
        End;
      End;
      CPlugins := CPlugins.Next;
    End;
  End;
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
