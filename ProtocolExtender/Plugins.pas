unit Plugins;

interface

uses Windows, PluginsShared;

type

{  TOnRegisterPacketHandler = procedure(Sender: TObject; Header:Byte; Handler: TPacketHandler) of object;
  TOnRegisterPacketType = function(Sender: TObject; Header: Byte; Size: Word): Boolean;}

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
      InitProc: TPluginProcedure;
      ProtocolHandlers: TProtocolHandlerArray;
      SyncEventCount: Integer;
      EventCallback: TSyncEvent;
      OnPacketSended: TPacketSendedCallback;
      OnPacketSendedParam: Pointer;
    end;

  strict private var
    FDlls: Array of TDllInfo;
    FDllCount: Cardinal;
    FDllPos: Cardinal;
    FDllInit: Boolean;

    FPlugins: Array of TPluginInfo;
    FPluginsCount: Cardinal;

    FAPILoaded: Boolean;

{    FOnRegisterPacketHandler: TOnRegisterPacketHandler;
    FOnUnregisterPacketHandler: TOnRegisterPacketHandler;
    FOnRegisterPacketType: TOnRegisterPacketType;}

    FActivePlugin: Cardinal;


    constructor Create;
    destructor Destory;
    {$IFDEF PLUGINS_HDD}
    function GetHDDDLLsCount: Cardinal;
    procedure LoadFromHDD;
    {$ENDIF}
    {$IFDEF PLUGINS_SERVER}
    procedure LoadFromServer;
    procedure OnPlgLoad(Sender: TObject; APath: String);
    procedure OnPlgCount(Sender: TObject; Amount: Cardinal);
    {$ENDIF}
    function LoadDll(ADllPath: String): Boolean;
    procedure LoadAPI;

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
    procedure Initialize;
    procedure ProxyStart;
    procedure ProxyEnd;
    function ClientToServerPacket(Data: Pointer; var Size:Cardinal): Boolean;
    function ServerToClientPacket(Data: Pointer; var Size:Cardinal): Boolean;
    procedure CheckSyncEvent;
    procedure PacketSended(Header: Byte; IsFromServerToClient: Boolean);

    {Control from Plugins}

    procedure RegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
    procedure UnRegisterPacketHandler(Header:Byte; Handler: TPacketHandler);
    function AfterPacketCallback(ACallBack: TPacketSendedCallback; lParam: Pointer): Boolean;
    function RegisterSyncEventHandler(Event: TSyncEvent): Pointer;
  end;

implementation

uses Common
, ClientThread, ProtocolDescription, Serials, zLib, HookLogic
{$IFDEF PLUGINS_SERVER}, PluginsDownloader{$ENDIF}
, ShardSetup;

const
  LastAPIFuncNum = 12;
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

procedure AfterPacketCallback(ACallBack: TPacketSendedCallback; lParam: Pointer); stdcall;
Begin
  TPluginSystem.Instance.AfterPacketCallback(ACallBack, lParam);
End;

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
  Result := TPluginSystem.Instance.RegisterSyncEventHandler(Event);
end;

procedure AskSyncEvent(InterlockedValue: Pointer); stdcall;
begin
  InterlockedIncrement(PInteger(InterlockedValue)^);
  InterlockedIncrement(TPluginSystem.Instance.FSyncEventCount);
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
  FActivePlugin := MAXDWORD;
end;

destructor TPluginSystem.Destory;
begin
  SetLength(FDlls, 0);
  SetLength(FPlugins, 0);
  Inherited;
end;

procedure TPluginSystem.Initialize;
begin
  {$IFDEF PLUGINS_HDD}
  FDllCount := GetHDDDLLsCount;
  {$IFDEF DEBUG}
  WriteLn('Plugins: Found ', FDllCount, ' libraries on disk.');
  {$ENDIF}
  {$ENDIF}
  {$IFDEF PLUGINS_SERVER}
  LoadFromServer;
  {$ENDIF}
  {$IFDEF PLUGINS_HDD}
  LoadFromHDD;
  {$ENDIF}
  {$IFDEF DEBUG}
  WriteLn('Plugins: Libraries loaded. Initializing plugins (', FPluginsCount, ').');
  {$ENDIF}
  LoadAPI;
end;

{$IFDEF PLUGINS_SERVER}
procedure TPluginSystem.LoadFromServer;
var
  Downloader: TPluginsDownloader;
  sTmpPlgFolder: AnsiString;
  iDownloadResult: Integer;
begin
  sTmpPlgFolder := ExtractFilePath(AnsiString(ParamStr(0))) + 'SrvPlugins\';
  Downloader := TPluginsDownloader.Create(ShardSetup.UpdateIP, ShardSetup.UpdatePort, sTmpPlgFolder);
  Downloader.OnPluginCount := OnPlgCount;
  Downloader.OnDownloaded := OnPlgLoad;
  {$IFDEF DEBUG}
  WriteLn('Plugins: Loading from Server ...');
  {$ENDIF}
  iDownloadResult := Downloader.Execute;
  If iDownloadResult < 0 Then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: Can''t connect to server (', iDownloadResult ,').');
    {$ENDIF}
  End;
  {$IFDEF DEBUG}
  WriteLn('Plugins: Loading from Server ... done.');
  {$ENDIF}
  Downloader.Free;
end;

procedure TPluginSystem.OnPlgCount(Sender: TObject; Amount: Cardinal);
begin
  FDllCount := FDllCount + Amount;
  {$IFDEF DEBUG}
  WriteLn('Plugins: Found ', Amount, ' libraries on server. Downloading ...');
  {$ENDIF}
end;

procedure TPluginSystem.OnPlgLoad(Sender: TObject; APath: string);
begin
  If not LoadDll(APath) Then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: No more space for library. Error or libraries changed.');
    {$ENDIF}
  End;
end;
{$ENDIF} // PLUGINS_SERVER

{$IFDEF PLUGINS_HDD}
procedure TPluginSystem.LoadFromHDD;
var
  sPlgFolder, sSearch: AnsiString;
  SR: WIN32_FIND_DATAA;
  hSearch: THandle;
Begin
  {$IFDEF DEBUG}
  WriteLn('Plugins: Loading libraries from HDD.');
  {$ENDIF}
  sPlgFolder := ExtractFilePath(AnsiString(ParamStr(0))) + 'Plugins\';
  sSearch := sPlgFolder + '*.dll' + #0;
  ZeroMemory(@SR, SizeOf(SR));
  hSearch := FindFirstFileA(@sSearch[1], SR);
  If hSearch <> INVALID_HANDLE_VALUE Then Begin
    Repeat
      If (SR.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY Then Continue;
      If not LoadDll(sPlgFolder + SR.cFileName) Then Begin
        Exit;
      End;
    until not FindNextFileA(hSearch, SR);
    Windows.FindClose(hSearch);
  End;
end;

function TPluginSystem.GetHDDDLLsCount;
var
  sPlgFolder, sSearch: AnsiString;
  SR: WIN32_FIND_DATAA;
  hSearch: THandle;
Begin
  Result := 0;
  sPlgFolder := ExtractFilePath(AnsiString(ParamStr(0))) + 'Plugins\';
  sSearch := sPlgFolder + '*.dll' + #0;
  ZeroMemory(@SR, SizeOf(SR));
  hSearch := FindFirstFileA(@sSearch[1], SR);
  If hSearch <> INVALID_HANDLE_VALUE Then Begin
    Repeat
      If (SR.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY Then Continue;
      Result := Result + 1;
    until not FindNextFileA(hSearch, SR);
    Windows.FindClose(hSearch);
  End;
end;
{$ENDIF} //PLUGINS_HDD

function TPluginSystem.LoadDll(ADllPath: string):Boolean;
var
  DllInitProc: TDllInit;
  hDll: THandle;
begin
  Result := False;
  {$IFDEF DEBUG}
  Write('Plugins: Loading library from ', ADllPath, ' ... ');
  {$ENDIF}
  If not FDllInit Then Begin
    SetLength(FDlls, FDllCount);
    FDllInit := True;
    FDllPos := 0;
    FPluginsCount := 0;
  End;
  If FAPILoaded or ((FDllPos + 1) > FDllCount) Then Begin
    {$IFDEF DEBUG}
    WriteLn('failed (no more room for libraries).');
    {$ENDIF}
    Exit;
  End;


  hDll := LoadLibrary(PChar(ADllPath));
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

procedure TPluginSystem.LoadAPI;
var
  i, j, iPluginsPos, iDllsCount: Cardinal;
  InitDone: TDllInitDone;
begin
  if FAPILoaded then Exit;
  if not FDllInit then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: No libraries loaded. Nothing to initialize.');
    {$ENDIF}
    Exit;
  End;
  FAPILoaded := True;
  SetLength(FPlugins, FPluginsCount);
  iPluginsPos := 0;
  iDllsCount := FDllCount;
  If FDllCount > 0 Then For i := 0 to FDllCount - 1 do Begin
    FDlls[i].FirstPluginOffset := iPluginsPos;
    for j := 0 to FDlls[i].PluginsAmount do Begin
      If TPluginProcedure(FDlls[i].PluginsInfo^.Plugins[j].InitProcedure)(PE_INIT, @API) Then Begin
        FPlugins[iPluginsPos].InitProc := FDlls[i].PluginsInfo^.Plugins[j].InitProcedure;
        iPluginsPos := iPluginsPos + 1;
      End Else Begin
        FDlls[i].PluginsAmount := FDlls[i].PluginsAmount - 1;
        {$IFDEF DEBUG}
        WriteLn('Plugins: One of plugins library (', i, ') failed to initialize plugin (', j, ').');
        {$ENDIF}
      End;
    End;
    FDlls[i].PluginsInfo := nil;
    InitDone := GetProcAddress(FDlls[i].Handle, 'DllInitDone');
    if Assigned(InitDone) then InitDone();
    if FDlls[i].PluginsAmount = 0 then Begin
      {$IFDEF DEBUG}
      WriteLn('Plugins: One of plugins library (', i ,') failed to initialize any plugin. It will be unloaded.');
      {$ENDIF}
      FreeLibrary(FDlls[i].Handle);
      FDlls[i].Handle := INVALID_HANDLE_VALUE;
      iDllsCount := iDllsCount - 1;
    End;
  End;
  FPluginsCount := iPluginsPos;
  FDllCount := iDllsCount;
  {$IFDEF DEBUG}
  WriteLn('Plugins: Initialization done. Loaded ', FPluginsCount, ' plugins from ', FDllCount, ' libraries.');
  {$ENDIF}
end;

function TPluginSystem.ClientToServerPacket(Data: Pointer; var Size: Cardinal): Boolean;
var
  iPluginPos: Cardinal;
  bSend: Boolean;
begin
  bSend := True;
  Result := False;
  If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
    if Assigned(FPlugins[iPluginPos].ProtocolHandlers[PByte(Data)^]) then Begin
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
  Result := bSend;
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
    FActivePlugin := iPluginPos;
    TPluginProcedure(FPlugins[iPluginPos].InitProc)(PE_PROXYSTART, nil);
    FActivePlugin := MAXDWORD;
  End;
end;

procedure TPluginSystem.ProxyEnd;
var
  iPluginPos: Cardinal;
begin
  If FPluginsCount > 0 Then For iPluginPos := 0 to FPluginsCount - 1 do Begin
    FActivePlugin := iPluginPos;
    TPluginProcedure(FPlugins[iPluginPos].InitProc)(PE_PROXYEND, nil);
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
      TPacketSendedCallback(FPlugins[iPluginPos].OnPacketSended)(Header, FPlugins[iPluginPos].OnPacketSendedParam, IsFromServerToClient);
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
  API.APIs[10].Func := @zLib.zCompress2;
  API.APIs[11].FuncType := PF_ZLIBDECOMPRESS;
  API.APIs[11].Func := @zLib.zDecompress;
  API.APIs[12].FuncType := PF_AFTERPACKETCALLBACK;
  API.APIs[12].Func := @AfterPacketCallback;
end.
