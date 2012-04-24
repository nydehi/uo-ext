unit ClientThread;

interface

uses Windows, WinSock, AbstractThread, PacketStream, ProtocolDescription, APIHooker;

type
  TClientThread=class(TAbstractThread)
  private
    FClientConnection:TSocket;
    FServerConnection:TSocket;

    FServerIp:Cardinal;
    FServerPort:Word;

    FLocalPort:Word;

    FCSObj:TPacketStream;
    FSCObj:TPacketStream;
    function ConnectToServer:Boolean;
    procedure Write(What:AnsiString);
    procedure OnCSPacket(Sender:TObject; Packet:Pointer; var Length:Cardinal; var Process:Boolean);
    procedure OnCSPacketDone(Sender:TObject; PacketHeader:Byte);
    procedure OnSCPacket(Sender:TObject; Packet:Pointer; var Length:Cardinal; var Process:Boolean);
    procedure OnSCPacketDone(Sender:TObject; PacketHeader:Byte);
    procedure OnCryptDetected(Sender:Tobject; CryptType: TCryptType; Phase: TCryptPhase);
  protected
    function Execute:Integer; override;
  public
    property ServerIP:Cardinal read FServerIp write FServerIp;
    property ServerPort:Word read FServerPort write FServerPort;
    property LocalPort:Word read FLocalPort write FLocalPort;
    property ClientSocket:TSocket read FClientConnection write FClientConnection;
    function SendPacket(Packet: Pointer; Length: Cardinal; ToServer, Direct: Boolean; var Valid: Boolean):Boolean;
  end;

var
  CCTLock: TRTLCriticalSection;
  CurrentClientThread: TClientThread;

implementation

uses Common, Plugins, Encryption, ShardSetup;

var
  TV_Timeout:timeval;

procedure TClientThread.Write(What:AnsiString);
begin
  {$IFDEF DEBUG}
  WriteLn(FServerIp shr 24, '.', (FServerIp shr 16) and $FF, '.', (FServerIp shr 8) and $FF, '.', FServerIp and $FF, ':', FServerPort, ' ', What);
  {$ENDIF}
end;

function TClientThread.ConnectToServer:Boolean;
var
  SockAddr:TSockAddr;
begin
  THooker.Hooker.TrueAPI;
  Result:=False;
  FServerConnection:=socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  IF FServerConnection=INVALID_SOCKET Then Exit;
  ZeroMemory(@SockAddr, SizeOf(SockAddr));
  SockAddr.sin_family:=AF_INET;
  SockAddr.sin_port:=htons(FServerPort);
  SockAddr.sin_addr.S_addr:=htonl(FServerIp);
  If connect(FServerConnection, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR Then Exit;
  Write('Connection to server established.');
  Result:=True;
  THooker.Hooker.TrueAPIEnd;
end;

function TClientThread.Execute:Integer;
var
  fs:TFDSet;
  ITrue:Integer;
begin
  Result:=-1;
  Write('Thread in.');
  EnterCriticalSection(CCTLock);
  CurrentClientThread := Self;
  LeaveCriticalSection(CCTLock);
  Write('CCT set.');
  TPluginSystem.Instance.ProxyStart;
  Write('ProxyStart done');
  If not ConnectToServer Then Exit;
  Write('Connected to server');
  ITrue:=1;
  ioctlsocket(FClientConnection, FIONBIO, ITrue);
  ioctlsocket(FServerConnection, FIONBIO, ITrue);
  FCSObj:=TPacketStream.Create(FClientConnection, FServerConnection);
  FSCObj:=TPacketStream.Create(FServerConnection, FClientConnection);
  FSCObj.Seed:=1;
  FSCObj.OnPacket:=OnSCPacket;
  FCSObj.OnPacket:=OnCSPacket;
  FSCObj.OnPacketProcessed:=OnSCPacketDone;
  FCSObj.OnPacketProcessed:=OnCSPacketDone;
  FSCObj.IsCliServ:=False;
  FCSObj.IsCliServ:=True;
  FCSObj.OnClientEncryptionDetected:=OnCryptDetected;
  {$IFDEF Debug}
  FSCObj.DebugPresend:=IntToStr(FServerIp shr 24) + '.' + IntToStr((FServerIp shr 16) and $FF) + '.' + IntToStr((FServerIp shr 8) and $FF) + '.' + IntToStr(FServerIp and $FF) + ':' + IntToStr(FServerPort) + ' ';
  FCSObj.DebugPresend:=FSCObj.DebugPresend;
  {$ENDIF}
  Write('Client thread ready to work.');
  repeat
    FD_ZERO(fs);
    FD_SET(FClientConnection, fs);
    FD_SET(FServerConnection, fs);
    select(0, @fs, nil, nil, @TV_Timeout);
    If FD_ISSET(FClientConnection, fs) Then Begin
      If not FCSObj.ProcessNetworkData Then Break;
    End;
    If FD_ISSET(FServerConnection, fs) Then Begin
      If not FSCObj.ProcessNetworkData Then Break;
    end;
    FCSObj.Flush;
    FSCObj.Flush;
    TPluginSystem.Instance.CheckSyncEvent;
  until FNeedExit;
  Write('Connection terminated by some reason.');
  EnterCriticalSection(CCTLock);
  Result:=0;
  ITrue:=0;
  ioctlsocket(FClientConnection, FIONBIO, ITrue);
  ioctlsocket(FServerConnection, FIONBIO, ITrue);
  closesocket(FClientConnection);
  closesocket(FServerConnection);
  Write('Proxy end event');
  TPluginSystem.Instance.ProxyEnd;
  Write('Ready to free objects');
  FCSObj.Free;
  FSCObj.Free;
  If CurrentClientThread = Self Then CurrentClientThread := nil;
  LeaveCriticalSection(CCTLock);
  Write('Thread out.');
end;

procedure TClientThread.OnCSPacket(Sender:TObject; Packet:Pointer; var Length:Cardinal; var Process:Boolean);
begin
  {$IFDEF WRITELOG}
  Write('C->S: Packet: Header: 0x' + IntToHex(PByte(Packet)^, 2) + ' Length: ' + IntToStr(Length));
  WriteDump(Packet, Length);
  {$ENDIF}
  If PByte(Packet)^=239 Then Begin
    FSCObj.Seed:=PCardinal(Cardinal(Packet) + 1)^;
    {$IFDEF Debug}
    Write('Seed is '+ IntToStr(FSCObj.Seed));
    {$ENDIF}
  End;
  If PByte(Packet)^=145 Then Begin
    FSCObj.Compression:=True;
    {$IFDEF Debug}
    Write('S->C: Compression enabled.');
    {$ENDIF}
  End;
  Process := TPluginSystem.Instance.ClientToServerPacket(Packet, Length);
end;

procedure TClientThread.OnSCPacket(Sender:TObject; Packet:Pointer; var Length:Cardinal; var Process:Boolean);
begin
  {$IFDEF WRITELOG}
  Write('S->C: Packet: Header: 0x' + IntToHex(PByte(Packet)^, 2) + ' Length: ' + IntToStr(Length));
  WriteDump(Packet, Length);
  {$ENDIF}
  If PByte(Packet)^=140 Then Begin

    PCardinal(Cardinal(Packet) + 1)^:=  htonl(INADDR_LOOPBACK);
    PWord(Cardinal(Packet) + 5)^:=htons(FLocalPort);
    {$IFDEF Debug}
    Write('S->C: Logging into game server with Auth_ID: '+IntToStr(PCardinal(Cardinal(Packet) + 7)^));
    {$ENDIF}
  End;
  Process := TPluginSystem.Instance.ServerToClientPacket(Packet, Length);
end;

procedure TClientThread.OnSCPacketDone(Sender: TObject; PacketHeader: Byte);
begin
  TPluginSystem.Instance.PacketSended(PacketHeader, True);
end;

procedure TClientThread.OnCSPacketDone(Sender: TObject; PacketHeader: Byte);
begin
  TPluginSystem.Instance.PacketSended(PacketHeader, False);
end;

function TClientThread.SendPacket(Packet: Pointer; Length: Cardinal; ToServer, Direct: Boolean; var Valid: Boolean):Boolean;
{$IFDEF DEBUG}
var
  oldSize: Cardinal;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  oldSize := Length;
  {$ENDIF}
  If ToServer Then
    Valid := FCSObj.DoSendPacket(Packet, Length, Direct, True)
  Else
    Valid := FSCObj.DoSendPacket(Packet, Length, Direct, True);
  {$IFDEF DEBUG}
  If not Valid Then begin
    Write('Plugin''s packet is not correct. Size: ' + IntToStr(oldSize) + ' Expected: ' + IntToStr(Length));
    {$IFDEF WRITELOG}
    WriteDump(Packet, oldSize);
    {$ENDIF}
  End;
  {$ENDIF}
  Result := Valid;
end;

procedure TClientThread.OnCryptDetected(Sender:TObject; CryptType: TCryptType; Phase: TCryptPhase);
var
  CSCrypt, SCCrypt: TNoEncryption;
Begin
  CSCrypt := nil;
  SCCrypt := nil;
  if Phase = cpLogin Then Begin
    // Client -> Server use Login Encryption
    // Server -> Client not encrypted
    If (CryptType = ctLogin) or ShardSetup.Encrypted Then Begin
      CSCrypt := TLoginEncryption.Create(htonl(FCSObj.Seed));
      CSCrypt.NeedDecrypt := (CryptType = ctLogin);
      CSCrypt.NeedEncrypt := ShardSetup.Encrypted;
    End;
  End Else If Phase = cpGame Then Begin
    // Both ways Encrypted.
    If (CryptType = ctGame) or ShardSetup.Encrypted Then Begin
      CSCrypt := TGameEncryptionCS.Create(htonl(FCSObj.Seed));
      SCCrypt := TGameEncryptionSC.Create(htonl(FCSObj.Seed));

      CSCrypt.NeedDecrypt := (CryptType = ctGame);
      SCCrypt.NeedEncrypt := (CryptType = ctGame);

      CSCrypt.NeedEncrypt := ShardSetup.Encrypted;
      SCCrypt.NeedDecrypt := ShardSetup.Encrypted;
    End;
  End;
  If CSCrypt = nil Then CSCrypt := TNoEncryption.Create;
  If SCCrypt = nil Then SCCrypt := TNoEncryption.Create;
  FCSObj.CryptObject := CSCrypt;
  FSCObj.CryptObject := SCCrypt;
End;

initialization
  TV_Timeout.tv_usec:=100;
  CurrentClientThread := nil;
  InitializeCriticalSection(CCTLock);
end.
