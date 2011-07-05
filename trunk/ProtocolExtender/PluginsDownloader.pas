unit PluginsDownloader;

interface

uses Windows, WinSock, Common;

type
  TPluginDownloadEventArgs=packed record
    CurrentBytes: Cardinal;
    TotalBytes: Cardinal;
    CurrentGrandBytes: Cardinal;
    GrandBytes: Cardinal;
    CurrentItems: Word;
    TotalItems: Word;
  end;
  PPluginDownloadEventArgs=^TPluginDownloadEventArgs;


  TPluginDownloadingEvent=function (Sender: TObject; Data: PPluginDownloadEventArgs):Boolean of object;
  TPluginDownloadedEvent=procedure (Sender: TObject; APath: String) of object;
  TPluginCount=procedure (Sender: TObject; Amount: Cardinal) of object;

  TPluginsDownloader=class
  private
    FOnDownloading: TPluginDownloadingEvent;
    FOnDonwloaded: TPluginDownloadedEvent;
    FOnPluginCount: TPluginCount;

    FIP: Cardinal;
    FPort: Cardinal;
    FTempPath: String;

    FBuffer: Pointer;
    FBufferPosition: Cardinal;
    FNeedExit: Boolean;

    FDlls: Array of Cardinal;
    FDllsCRC32: Array of Cardinal;
    FDllNumber: Cardinal;
    FDllData: Pointer;
    FDllDataOffset: Cardinal;

    FEventData: TPluginDownloadEventArgs;

    procedure HandlePackets; inline;
    function ConnectToServer: TSocket; inline;
    procedure Disconnect(ASocket: TSocket);
    function TryRead(Dest: Pointer; Length, Offset: Cardinal; var Readed: Cardinal):Boolean;

    function Packet01DllList: Cardinal;
    function ReadDll: Cardinal;
  protected
    function DoDownloading: Boolean; inline;
    procedure DoDownloaded; inline;
  public
    property OnDownloading: TPluginDownloadingEvent read FOnDownloading write FOnDownloading;
    property OnDownloaded: TPluginDownloadedEvent read FOnDonwloaded write FOnDonwloaded;
    property OnPluginCount: TPluginCount read FOnPluginCount write FOnPluginCount;

    function Execute:Integer;

    constructor Create(AIP: AnsiString; APort: Word; ATempPath: String);
    destructor Destroy; override;
  end;

implementation

uses ShardSetup;

constructor TPluginsDownloader.Create(AIP: AnsiString; APort: Word; ATempPath: string);
begin
  inherited Create;
  AIP := AIP + #0;
  FIP := inet_addr(@AIP[1]);;
  FPort := APort;
  FTempPath := ATempPath;
  FBuffer := nil;
  FBufferPosition := 0;
  FNeedExit := False;
  FDllNumber := 0;
  ZeroMemory(@FEventData, SizeOf(FEventData));
  FDllData := nil;
  FDllDataOffset := 0;
end;

destructor TPluginsDownloader.Destroy;
begin
  if FBuffer <> nil then FreeMemory(FBuffer);
  if FDllData <> nil then FreeMemory(FDllData);
  SetLength(FDlls, 0);
  SetLength(FDllsCRC32, 0);
  Inherited;
end;

function TPluginsDownloader.DoDownloading:Boolean;
begin
  if Assigned(FOnDownloading) then Result := FOnDownloading(Self, @FEventData) else Result := True;
end;

procedure TPluginsDownloader.DoDownloaded;
var
  F: File;
begin
  AssignFile(F, FTempPath + IntToStr(FDllNumber) + '.plg');
  Rewrite(F, 1);
  BlockWrite(F, FDllData^, FDlls[FDllNumber]);
  CloseFile(F);
  if Assigned(FOnDonwloaded) then FOnDonwloaded(Self, FTempPath + IntToStr(FDllNumber) + '.plg');
end;

function TPluginsDownloader.ConnectToServer:TSocket;
var
  SockAddr: TSockAddrIn;
  ITrue: Integer;
begin
  Result := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Result = INVALID_SOCKET then Exit;
  ZeroMemory(@SockAddr, SizeOf(SockAddr));
  SockAddr.sin_family:=AF_INET;
  SockAddr.sin_port:=htons(FPort);
  SockAddr.sin_addr.S_addr:=htonl(FIP);
  If connect(Result, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR Then Begin
    closesocket(Result);
    Result := INVALID_SOCKET;
    Exit;
  End;
  ITrue := 1;
  ioctlsocket(Result, FIONBIO, ITrue);
end;

procedure TPluginsDownloader.Disconnect(ASocket: Integer);
var
  IFalse: Integer;
begin
  IFalse := 0;
  ioctlsocket(ASocket, FIONBIO, IFalse);
  closesocket(ASocket);
end;

function TPluginsDownloader.Execute;
var
  ASocket: TSocket;
  fs:TFDSet;
  TV_Timeout:TTimeVal;
  iRecived: Integer;
begin
  Result := -1;
  ASocket := ConnectToServer;
  if ASocket = INVALID_SOCKET then Exit;


  FBuffer := GetMemory(65536);
  PByte(FBuffer)^ := 01;
  send(ASocket, FBuffer^, 1, 0);
  TV_Timeout.tv_usec := 100;
  repeat
    FD_ZERO(fs);
    FD_SET(ASocket, fs);
    select(0, @fs, nil, nil, @TV_Timeout);
    if FD_ISSET(ASocket, fs) then Begin
      iRecived := recv(ASocket, Pointer(Cardinal(FBuffer) + FBufferPosition)^, 65536, 0);
      if iRecived = 0 then Break;
      FBufferPosition := FBufferPosition + iRecived;
      HandlePackets;
    End;
  until not Self.FNeedExit;
  Disconnect(ASocket);
end;

procedure TPluginsDownloader.HandlePackets;
var
  cProcessedAmount: Cardinal;
  locBuffer: Pointer;
begin
  repeat
    cProcessedAmount := 0;
    if FEventData.TotalItems = 0 then Begin
      if FBufferPosition > 0 then Begin
        If PByte(FBuffer)^ <> 0 then Begin {Wrong stream type}
          FNeedExit := True;
          Exit;
        End;
        cProcessedAmount := Packet01DllList;
        if FNeedExit then Exit;
      End;
    End Else Begin
      cProcessedAmount := ReadDll;
      if FNeedExit then Exit;
    End;
    if cProcessedAmount > 0 then Begin
      if FBufferPosition <> cProcessedAmount then Begin
        locBuffer := GetMemory(65536);
        CopyMemory(locBuffer, Pointer(Cardinal(FBuffer) + cProcessedAmount), FBufferPosition - cProcessedAmount);
        FreeMemory(FBuffer);
        FBuffer := locBuffer;
        FBufferPosition := FBufferPosition - cProcessedAmount;
      End Else Begin
        FBufferPosition := 0;
      End;
    End;
  until (cProcessedAmount = 0) or (FBufferPosition = 0);
end;

function TPluginsDownloader.Packet01DllList: Cardinal;
var
  cReaded, cTotalSize, cMaxDllSize: Cardinal;
  i: Integer;

begin
  cReaded := 1;
  Result := 0;
  if not TryRead(@FEventData.TotalItems, SizeOf(FEventData.TotalItems), 1, cReaded) then Exit;
  if Assigned(FOnPluginCount) then FOnPluginCount(Self, FEventData.TotalItems);

  SetLength(FDlls, FEventData.TotalItems);
  SetLength(FDllsCRC32, FEventData.TotalItems);
  cMaxDllSize := 0;
  cTotalSize := 0;
  for i := 0 to FEventData.TotalItems - 1 do begin
    if not TryRead(@FDlls[i], 4, i * 8 + 1, cReaded) then Exit;
    if not TryRead(@FDllsCRC32[i], 4, i * 8 + 5, cReaded) then Exit; // Don't used right now.
    if cMaxDllSize < FDlls[i] then cMaxDllSize := FDlls[i];
    cTotalSize := cTotalSize + FDlls[i];
  End;
  if cMaxDllSize = 0 then Begin
    FNeedExit := True;
    Exit;
  End;
  FDllData := GetMemory(cMaxDllSize);
  Result := cReaded;
  FEventData.TotalBytes := FDlls[0];
  FEventData.GrandBytes := cTotalSize;
end;

function TPluginsDownloader.ReadDll: Cardinal;
begin
  Result := 0;
  repeat
    if FNeedExit then Begin
      Exit;
    End Else If FDllNumber >= FEventData.TotalItems then Begin
      FNeedExit := True;
      Exit;
    End Else If (FBufferPosition - Result) >= (FDlls[FDllNumber] - FDllDataOffset) Then Begin
      CopyMemory(Pointer(Cardinal(FDllData) + FDllDataOffset), Pointer(Cardinal(FBuffer) + Result), FDlls[FDllNumber] - FDllDataOffset);
      Result := Result + FDlls[FDllNumber] - FDllDataOffset;
      FEventData.CurrentBytes := FEventData.TotalBytes;
      FEventData.CurrentGrandBytes := FEventData.CurrentGrandBytes + FDlls[FDllNumber] - FDllDataOffset;
      If not DoDownloading Then Begin
        FNeedExit := True;
        Exit;
      End;
      DoDownloaded;
      FDllDataOffset := 0;
      FDllNumber := FDllNumber + 1;
      FEventData.TotalBytes := FDlls[FDllNumber];
      FEventData.CurrentItems := FDllNumber;
      FEventData.CurrentBytes := 0;
      If not DoDownloading Then Begin
        FNeedExit := True;
        Exit;
      End;
      If FDllNumber >= FEventData.TotalItems then Begin
        FNeedExit := True;
        Exit;
      End;
    End Else Begin
      CopyMemory(Pointer(Cardinal(FDllData) + FDllDataOffset), Pointer(Cardinal(FBuffer) + Result), FBufferPosition - Result);
      FDllDataOffset := FDllDataOffset + (FBufferPosition - Result);
      Result := FBufferPosition;
      FEventData.CurrentBytes := FEventData.CurrentBytes + (FBufferPosition - Result);
      FEventData.CurrentGrandBytes := FEventData.CurrentGrandBytes + (FBufferPosition - Result);
      If not DoDownloading Then Begin
        FNeedExit := True;
        Exit;
      End;
    End;
  until Result = FBufferPosition;
end;

function TPluginsDownloader.TryRead(Dest: Pointer; Length: Cardinal; Offset: Cardinal; var Readed: Cardinal):Boolean;
begin
  Result := False;
  if (Offset + Length) > FBufferPosition then Exit;
  CopyMemory(Dest, Pointer(Cardinal(FBuffer) + Offset), Length);
  Readed := Readed + Length;
  Result := True;
end;



end.
