unit ServerThread;

interface

uses Windows, WinSock, AbstractThread, ClientThread;

type
  RListeningInfo=record
    IP:Cardinal;
    Port:Word;
  end;
  TServerThread=class(TAbstractThread)
  private
    FListeningInfo:Array of RListeningInfo;
    FListeningInfoCount:Cardinal;
    FListeningSockets:Array of TSocket;
  protected
    function Execute:Integer; override;
  public
    procedure AddListenInfo(Info:RListeningInfo);
  end;

implementation

var
  Tv:TTimeVal;

procedure TServerThread.AddListenInfo(Info:RListeningInfo);
begin
  If not Running Then Begin
    SetLength(FListeningInfo, FListeningInfoCount+1);
    FListeningInfo[FListeningInfoCount].IP:=Info.IP;
    FListeningInfo[FListeningInfoCount].Port:=Info.Port;
    FListeningInfoCount:=FListeningInfoCount+1;
  End;
end;

function TServerThread.Execute;
var
  i:Cardinal;
  cSocket:TSocket;
  SockAddr:TSockAddr;
  NonBlock:Integer;
  fs:TFDSet;
begin
  Result:=-1;
  SetLength(FListeningSockets, FListeningInfoCount);
  For i:=0 to FListeningInfoCount - 1 do begin
    cSocket:=socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    If cSocket=INVALID_SOCKET Then Exit;
    ZeroMemory(@SockAddr, SizeOf(SockAddr));
    SockAddr.sin_family:=AF_INET;
    SockAddr.sin_port:=htons(FListeningInfo[i].Port);
    SockAddr.sin_addr.S_addr:=htonl(FListeningInfo[i].IP);
    If bind(cSocket, SockAddr, SizeOf(SockAddr)) <> 0 Then Exit;
    If listen(cSocket, SOMAXCONN) <> 0 Then Exit;
    NonBlock:=1;
    ioctlsocket(cSocket, FIONBIO, NonBlock);
    FListeningSockets[i]:=cSocket;
    WriteLn('Listening: ', FListeningInfo[i].IP SHR 24, '.', (FListeningInfo[i].IP SHR 16) AND $FF, '.', (FListeningInfo[i].IP SHR 8) AND $FF, '.', FListeningInfo[i].IP AND $FF, ':', FListeningInfo[i].Port);
  end;
  repeat
    FD_ZERO(fs);
    For i:=0 to FListeningInfoCount - 1 do FD_SET(FListeningSockets[i], fs);
    select(0, @fs, nil, nil, @Tv);
    If fs.fd_count>0 Then For i:=0 to fs.fd_count - 1 do begin
      cSocket:=accept(fs.fd_array[i], nil, nil);
      If cSocket<>INVALID_SOCKET Then begin
        with TClientThread.Create do begin
          ServerIP:=INADDR_LOOPBACK;
          ServerPort:=2593;
          ClientSocket:=cSocket;
          Run;
        end;
// Create Client thread here
      end;
    end;
  until FNeedExit;
  For i:=0 to FListeningInfoCount - 1 do closesocket(FListeningSockets[i]);
  WriteLn('Server: Sockets closed.');
end;

initialization
  Tv.tv_usec:=100;
end.
