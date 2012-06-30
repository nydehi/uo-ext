unit HookLogic;

interface

uses Windows, APIHooker, ShardSetup, AbstractThread, PluginsShared, WinSock, Messages;

var
  iIP: Integer;
  iPort: Word;

procedure HookIt;

implementation

uses Common, Plugins, ClientThread;

function connectHook(s: TSocket; var name: TSockAddr; namelen: Integer): Integer; stdcall;
var
  ServSocket, DistantServerSocket: TSocket;
  SockPair:Boolean;
  SockAddr:TSockAddr;
  SA_Len: Integer;
Begin
  Result := SOCKET_ERROR;
  WSASetLastError(WSAECONNREFUSED);
  if iPort = 0 then Begin
    if ShardSetup.Port <> 0 then Begin
      iIP := ShardSetup.IP;
      iPort := ShardSetup.Port;
    End Else Begin
      iIP := name.sin_addr.S_addr;
      iPort := name.sin_port;
    End;
  End;

  DistantServerSocket := ClientThread.ConnectToServer(iIP, iPort);
  if DistantServerSocket = INVALID_SOCKET then Exit;

  THooker.Hooker.TrueAPI;
  SockPair := ClientThread.CreateSocketPair(ServSocket, s);
  THooker.Hooker.TrueAPIEnd;
  if SockPair then Begin
    SA_Len := SizeOf(SockAddr);
    If getsockname(ServSocket, SockAddr, SA_Len) <> 0 Then SockPair := False;
  End;

  if not SockPair then Begin
    closesocket(DistantServerSocket);
    closesocket(ServSocket);
    Exit;
  End;

  with TClientThread.Create do begin
    LocalPort := ntohs(SockAddr.sin_port);
    ServerSocket := DistantServerSocket;
    ClientSocket := ServSocket;
    Run;
  end;
  Result := 0;
End;

procedure HookIt;
begin
  iIP := 0;
  iPort := 0;
  THooker.Hooker.HookFunction(@connectHook, GetProcAddress(GetModuleHandle('wsock32.dll'), 'connect'));
  THooker.Hooker.InjectIt;
end;

end.
