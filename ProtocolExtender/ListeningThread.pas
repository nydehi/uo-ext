unit ListeningThread;

interface

uses Windows, WinSock, AbstractThread, ClientThread, ShardSetup;

type
  TServerThread=class(TAbstractThread)
  private
    FRemoteIP: Cardinal;
    FRemotePort: Word;
    FLocalPort: Word;
    FServerSocket: TSocket;
  protected
    function Execute:Integer; override;
  public
    property RemoteIP: Cardinal read FRemoteIP;
    property RemotePort: Word read FRemotePort;
    property LocalPort: Word read FLocalPort;
    constructor Create;
  end;

implementation

uses Common
;

var
  Tv:TTimeVal;

// TServerThread

constructor TServerThread.Create;
begin
  Inherited;
  FRemoteIP := htonl(inet_addr(@ShardSetup.LoginIP[1]));
  FRemotePort := ShardSetup.LoginPort;
end;

function TServerThread.Execute;
var
  cSocket:TSocket;
  SockAddr:TSockAddr;
  SA_Len: Integer;
  NonBlock:Integer;
  fs:TFDSet;
  WSAData:TWSAData;
begin
  Result:=-1;
  WSAStartup($101, WSAData);
  FServerSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  If FServerSocket=INVALID_SOCKET Then Exit;
  ZeroMemory(@SockAddr, SizeOf(SockAddr));
  SockAddr.sin_family:=AF_INET;
  SockAddr.sin_port:=0;
  SockAddr.sin_addr.S_addr:=htonl(INADDR_LOOPBACK);
  If bind(FServerSocket, SockAddr, SizeOf(SockAddr)) <> 0 Then Exit;
  SA_Len := SizeOf(SockAddr);
  If getsockname(FServerSocket, SockAddr, SA_Len) <> 0 Then Exit;
  If listen(FServerSocket, SOMAXCONN) <> 0 Then Exit;
  NonBlock:=1;
  ioctlsocket(FServerSocket, FIONBIO, NonBlock);
  FLocalPort := ntohs(SockAddr.sin_port);
  repeat
    FD_ZERO(fs);
    FD_SET(FServerSocket, fs);
    select(0, @fs, nil, nil, @Tv);
    If fs.fd_count>0 Then begin
      cSocket:=accept(FServerSocket, nil, nil);
      If cSocket<>INVALID_SOCKET Then begin
        while ClientThread.CurrentClientThread <> nil do Sleep(1);

        with TClientThread.Create do begin
          ServerIP:=FRemoteIP;
          ServerPort:=FRemotePort;
          LocalPort := FLocalPort;
          ClientSocket:=cSocket;
          Run;
        end;
      end;
    end;
  until FNeedExit;
  NonBlock := 0;
  ioctlsocket(FServerSocket, FIONBIO, NonBlock);
  closesocket(FServerSocket);
  WSACleanup();
end;

initialization
  Tv.tv_usec:=100;
end.
