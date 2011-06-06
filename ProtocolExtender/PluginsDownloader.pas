unit PluginsDownloader;

interface

uses Windows, WinSock, AbstractThread;

type
  TPluginDownloadingEvent=function (Sender: TObject; ADone, ATotal:Cardinal):Boolean of object;
  TPluginDownloadedEvent=procedure (Sender: TObject; APath: String) of object;

  TPluginsDownloader=class(TAbstractThread)
  private
    FOnDownloading: TPluginDownloadingEvent;
    FOnDonwloaded: TPluginDownloadedEvent;
    FCurrentPlugin: String;

    FIP: Cardinal;
    FPort: Cardinal;
    FTempPath: String;

    FBuffer: Pointer;
    FBufferPosition: Cardinal;

    function HandlePackets:Boolean; inline;
  protected
    function Execute:Integer; override;
    function DoDownloading(ADone, ATotal: Cardinal): Boolean; inline;
    procedure DoDownloaded(APath: String); inline;
  public
    property OnDownloading: TPluginDownloadingEvent read FOnDownloading write FOnDownloading;
    property OnDownloaded: TPluginDownloadedEvent read FOnDonwloaded write FOnDonwloaded;
    property CurrentPlugin: String read FCurrentPlugin;

    constructor Create(AIP: Cardinal; APort: Word; ATempPath: String);
    destructor Destroy; override;
  end;

implementation

uses ShardSetup;

constructor TPluginsDownloader.Create(AIP: Cardinal; APort: Word; ATempPath: string);
begin
  inherited Create;
  FIP := AIP;
  FPort := APort;
  FTempPath := ATempPath;
  FBuffer := nil;
  FBufferPosition := 0;
  Run;
end;

destructor TPluginsDownloader.Destroy;
begin
  if FBuffer <> nil then FreeMemory(FBuffer);
  Inherited;
end;

function TPluginsDownloader.DoDownloading(ADone: Cardinal; ATotal: Cardinal):Boolean;
begin
  if Assigned(FOnDownloading) then Result := FOnDownloading(Self, ADone, ATotal) else Result := False;
end;

procedure TPluginsDownloader.DoDownloaded(APath: string);
begin
  if Assigned(FOnDonwloaded) then FOnDonwloaded(Self, APath);
end;

{
  C->S: Request plugins packet.
    BYTE - Incapsulation header
    WORD - Size of packet = 0x0004
    BYTE - 0x01
  S->C: Return plugins naming.
    BYTE - Incapsulation header
    WORD - Size of packet
    BYTE - 0x01
    LOOP
      DWORD - SizeOfPlugin
      BYTE[30] - Plugin name (ANSI)
    END LOOP
  S->C: Plugin content.
    BYTE - incapsulation header
    WORD - Size of packet
    BYTE - 0x02
    BYTE[*] - Plugin data
}

function TPluginsDownloader.Execute;
var
  ASocket: TSocket;
  SockAddr: TSockAddrIn;
  ITrue, iRecived, iBufferPosition: Integer;
  fs:TFDSet;
  TV_Timeout:TTimeVal;
begin
  Result := -1;
  ASocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if ASocket = INVALID_SOCKET then Exit;
  ZeroMemory(@SockAddr, SizeOf(SockAddr));
  SockAddr.sin_family:=AF_INET;
  SockAddr.sin_port:=htons(FPort);
  SockAddr.sin_addr.S_addr:=htonl(FIP);
  If connect(ASocket, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR Then Exit;
  ITrue := 1;
  ioctlsocket(ASocket, FIONBIO, ITrue);
  FBuffer := GetMemory(65536);
  PByte(FBuffer)^ := ShardSetup.InternalProtocolHeader;
  PWord(Cardinal(FBuffer) + 1)^ := 4;
  PByte(Cardinal(FBuffer) + 3)^ := 1;
  send(ASocket, FBuffer^, 4, 0);
  TV_Timeout.tv_usec := 100;
  repeat
    FD_ZERO(fs);
    FD_SET(ASocket, fs);
    select(0, @fs, nil, nil, @TV_Timeout);
    if FD_ISSET(ASocket, fs) then Begin
      iRecived := recv(ASocket, Pointer(Cardinal(FBuffer) + iBufferPosition)^, 65536, 0);
      if iRecived = 0 then Break;
      FBufferPosition := FBufferPosition + iRecived;
      if FBufferPosition >= 3 then begin
        if PWord(Cardinal(FBuffer) + 1)^ <= FBufferPosition then
          if HandlePackets Then Break;
      end;
    End;
  until not Self.FNeedExit;

  ITrue := 0;
  ioctlsocket(ASocket, FIONBIO, ITrue);
  closesocket(ASocket);
end;

function TPluginsDownloader.HandlePackets;
begin

end;





end.
