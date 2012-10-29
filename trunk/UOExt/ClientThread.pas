unit ClientThread;

interface

uses Windows, WinSock2, AbstractThread, PacketStream, ProtocolDescription, APIHooker;

type
  TClientThread=class(TAbstractThread)
  private
    FClientConnection:TSocket;
    FServerConnection:TSocket;

    FLocalPort:Word;
    FSendEvent:THandle;

    FCSObj:TPacketStream;
    FSCObj:TPacketStream;
    procedure Write(What:AnsiString);
    procedure OnCSPacket(Sender:TObject; Packet:Pointer; var Length:Cardinal; var Process:Boolean);
    procedure OnCSPacketDone(Sender:TObject; PacketHeader:Byte);
    procedure OnSCPacket(Sender:TObject; Packet:Pointer; var Length:Cardinal; var Process:Boolean);
    procedure OnSCPacketDone(Sender:TObject; PacketHeader:Byte);
    procedure OnCryptDetected(Sender:Tobject; CryptType: TCryptType; Phase: TCryptPhase);
  protected
    function Execute:Integer; override;
  public
    property ServerSocket:TSocket read FServerConnection write FServerConnection;
    property LocalPort:Word read FLocalPort write FLocalPort;
    property ClientSocket:TSocket read FClientConnection write FClientConnection;
    function SendPacket(Packet: Pointer; Length: Cardinal; ToServer, Direct: Boolean; var Valid: Boolean):Boolean;

  end;

var
  CurrentClientThread: TClientThread;

  function CreateSocketPair(var ServerSocket: TSocket; var ClientSocket: TSocket): Boolean;
  function ConnectToServer(IP: Integer; Port: Word):TSocket;

implementation

uses Common, Plugins, Encryption, ShardSetup;

var
  TV_Timeout:timeval;

// Local procedures

function CreateSocketPair(var ServerSocket: TSocket; var ClientSocket: TSocket): Boolean;
var
  ListenSocket: TSocket;
  SockAddr:TSockAddrIn;
//  NonBlock:Cardinal;
  SA_Len: Integer;

  wLocalPort: Word;
  iConnResult: Integer;
  WSAGLE: Integer;
begin
  Result := False;

  ListenSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  If ListenSocket=INVALID_SOCKET Then Exit;

  ZeroMemory(@SockAddr, SizeOf(SockAddr));
  SockAddr.sin_family:=AF_INET;
  SockAddr.sin_port:=0;
  SockAddr.sin_addr.S_addr:=htonl(INADDR_LOOPBACK);

  If bind(ListenSocket, TSockAddr(SockAddr), SizeOf(SockAddr)) <> 0 Then Begin
    closesocket(ListenSocket);
    Exit;
  End;
  If listen(ListenSocket, SOMAXCONN) <> 0 Then Begin
    closesocket(ListenSocket);
    Exit;
  End;
  SA_Len := SizeOf(SockAddr);
  If getsockname(ListenSocket, TSockAddr(SockAddr), SA_Len) <> 0 Then Begin
    closesocket(ListenSocket);
    Exit;
  End;

//  NonBlock:=1;
  wLocalPort := ntohs(SockAddr.sin_port);

  ZeroMemory(@SockAddr, SizeOf(SockAddr));
  SockAddr.sin_family:=AF_INET;
  SockAddr.sin_port:=htons(wLocalPort);
  SockAddr.sin_addr.S_addr:=htonl(INADDR_LOOPBACK);

//  ioctlsocket(ClientSocket, FIONBIO, NonBlock);

  iConnResult := connect(ClientSocket, TSockAddr(SockAddr), SizeOf(SockAddr));
  If iConnResult = SOCKET_ERROR Then Begin
    WSAGLE := WSAGetLastError;
    if WSAGLE <> WSAEWOULDBLOCK then Begin
      closesocket(ClientSocket);
      closesocket(ListenSocket);
      Exit;
    End;
  End;

  ServerSocket := accept(ListenSocket, nil, nil);
  If ServerSocket = INVALID_SOCKET Then Begin
    closesocket(ClientSocket);
    closesocket(ListenSocket);
    Exit;
  End;

//  NonBlock := 0;
//  ioctlsocket(ClientSocket, FIONBIO, NonBlock);
  closesocket(ListenSocket);

  Result:=True;
End;

function ConnectToServer(IP: Integer; Port: Word):TSocket;
var
  SA: TSockAddrIn;
Begin
  Result := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  If Result = INVALID_SOCKET Then Begin
    WSASetLastError(WSAECONNREFUSED);
    Exit;
  End;
  ZeroMemory(@SA, SizeOf(SA));
  SA.sin_family := AF_INET;
  SA.sin_addr.S_addr := IP;
  SA.sin_port := Port;
  If connect(Result, TSockAddr(SA), SizeOf(SA)) = SOCKET_ERROR Then Begin
    closesocket(Result);
    Result := INVALID_SOCKET;
  End;
End;


function SocketClosedReason(Sock: TSocket): Integer;
var
  Sock_Set: TFdSet;
  Buff: Byte;
  Size: Integer;
Begin
  Result := 1;
  FD_ZERO(Sock_Set);
  _FD_SET(Sock, Sock_Set);
  select(0, @Sock_Set, nil, nil, @TV_Timeout);
  If FD_ISSET(Sock, Sock_Set) then Begin
    Size := recv(Sock, Buff, 1, MSG_PEEK);
    if Size = 0 then Result := 0;
    if Size = SOCKET_ERROR then Result := WSAGetLastError;
  End;
End;

// TClientThread

procedure TClientThread.Write(What:AnsiString);
begin
  {$IFDEF DEBUG}
  WriteLn('ClientThread: ', What);
  {$ENDIF}
end;

function TClientThread.Execute:Integer;
var
//  ITrue:Cardinal;
  Buffer: Array [0..1] of Byte;
  Valid: Boolean;
  Events: Array [0..2] of WSAEVENT;
  NeEvents: WSANETWORKEVENTS;
begin
  Write('Thread in.');
  repeat
    if CurrentClientThread <> nil then Begin
      CurrentClientThread.FNeedExit := True;
      Events[0] := OpenEventA(EVENT_ALL_ACCESS, False, 'FlushEvent');
      If(Events[0] <> 0) Then Begin
        SetEvent(Events[0]);
        CloseHandle(Events[0]);
      End;
    End;
    Sleep(1);
  until CurrentClientThread = nil;
  Events[0] := CreateEventA(nil, True, False, 'FlushEvent');
  FSendEvent := Events[0];
  CurrentClientThread := Self;
  TPluginSystem.Instance.ProxyStart;
  Write('ProxyStart done');
//  ITrue:=1;
//  ioctlsocket(FClientConnection, FIONBIO, ITrue);
//  ioctlsocket(FServerConnection, FIONBIO, ITrue);
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
  FSCObj.DebugPresend:='ClientThread: ';
  FCSObj.DebugPresend:=FSCObj.DebugPresend;
  {$ENDIF}
  Events[1] := WSACreateEvent;
  Events[2] := WSACreateEvent;
  WSAEventSelect(FClientConnection, Events[1], FD_READ);
  WSAEventSelect(FServerConnection, Events[2], FD_READ);
  Write('Client thread ready to work.');
  repeat
    case WSAWaitForMultipleEvents(3, @Events[0], False, WSA_INFINITE, False) of
      WSA_WAIT_EVENT_0: Begin
        ResetEvent(Events[0]);
      End;
      WSA_WAIT_EVENT_0 + 1: Begin
        if WSAEnumNetworkEvents(FClientConnection, Events[1], NeEvents) <> SOCKET_ERROR Then Begin
          If (NeEvents.lNetworkEvents = FD_READ)AND(NeEvents.iErrorCode[FD_READ_BIT] = 0) Then Begin
            If not FCSObj.ProcessNetworkData Then FNeedExit := True;
          End Else FNeedExit := True;
        End;
      End;
      WSA_WAIT_EVENT_0 + 2: Begin
        if WSAEnumNetworkEvents(FServerConnection, Events[2], NeEvents) <> SOCKET_ERROR Then Begin
          If (NeEvents.lNetworkEvents = FD_READ)AND(NeEvents.iErrorCode[FD_READ_BIT] = 0) Then Begin
            If not FSCObj.ProcessNetworkData Then FNeedExit := True;
          End Else FNeedExit := True;
        End;
      End;
      WSA_WAIT_FAILED: FNeedExit := True;
    end;
    FCSObj.Flush;
    FSCObj.Flush;
  until FNeedExit;
  WSAEventSelect(FClientConnection, Events[1], 0);
  WSAEventSelect(FServerConnection, Events[2], 0);
  WSACloseEvent(Events[1]);
  WSACloseEvent(Events[2]);
  CloseHandle(Events[0]);

  Write('Connection terminated by some reason.');
  TPluginSystem.Instance.ProxyEnd(SocketClosedReason(FServerConnection), SocketClosedReason(FClientConnection));
  If (SocketClosedReason(FClientConnection) = 1)and(not FSCObj.Compression) then Begin
    Buffer[0] := $82;
    Buffer[1] := $FF;
    SendPacket(@Buffer, 2, False, True, Valid);
    FSCObj.Flush;
  End;

  Result:=0;
//  ITrue:=0;
//  ioctlsocket(FClientConnection, FIONBIO, ITrue);
//  ioctlsocket(FServerConnection, FIONBIO, ITrue);
  closesocket(FClientConnection);
  closesocket(FServerConnection);
  FCSObj.Free;
  FSCObj.Free;
  If CurrentClientThread = Self Then CurrentClientThread := nil;
  Write('Thread out.');
end;

procedure TClientThread.OnCSPacket(Sender:TObject; Packet:Pointer; var Length:Cardinal; var Process:Boolean);
begin
  {$IFDEF WRITELOG}
  Write('C->S: Packet: Header: 0x' + IntToHex(PByte(Packet)^, 2) + ' Length: ' + IntToStr(Length));
  WriteDump(Packet, Length);
  {$ENDIF}
(*  If PByte(Packet)^=239 Then Begin
    FSCObj.Seed:=PCardinal(Cardinal(Packet) + 1)^;
    {$IFDEF Debug}
    Write('Seed is '+ IntToStr(FSCObj.Seed));
    {$ENDIF}
  End;*)
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
(*  If PByte(Packet)^=140 Then Begin

    PCardinal(Cardinal(Packet) + 1)^:=  htonl(INADDR_LOOPBACK);
    PWord(Cardinal(Packet) + 5)^:=htons(FLocalPort);
    {$IFDEF Debug}
    Write('S->C: Logging into game server with Auth_ID: '+IntToStr(PCardinal(Cardinal(Packet) + 7)^));
    {$ENDIF}
  End;*)
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
  SetEvent(FSendEvent);
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
      if FCSObj.SeedType then
        CSCrypt := TLoginEncryption.Create(htonl(FCSObj.Seed), FCSObj.ClientMajor, FCSObj.ClientMinor, FCSObj.ClientBuild)
      Else
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
  If not Assigned(CSCrypt)Then CSCrypt := TNoEncryption.Create;
  If not Assigned(SCCrypt) Then SCCrypt := TNoEncryption.Create;
  FCSObj.CryptObject := CSCrypt;
  FSCObj.CryptObject := SCCrypt;
End;

initialization
  TV_Timeout.tv_usec:=100;
  CurrentClientThread := nil;
end.
