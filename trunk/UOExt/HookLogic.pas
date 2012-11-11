unit HookLogic;

interface

uses Windows, APIHooker, ShardSetup, AbstractThread, PluginsShared, WinSock2, Messages, TLHelp32;

var
  iIP: Integer;
  iPort: Word;
  TransServerPort: Word;

procedure HookIt;
procedure UnHookIt;

implementation

uses Common, Plugins, ClientThread, ExecutableSections;

var
  connectReturnAddr: Cardinal;
  connectHookInvokeAddr: Cardinal;

function connectHookInvoke(s: TSocket; var name: TSockAddrIn; namelen: Integer): Integer; stdcall;
var
  ServSocket, DistantServerSocket: TSocket;
  SockPair:Boolean;
  SockAddr:TSockAddrIn;
  SA_Len: Integer;
  bFromExe: Boolean;
Begin
  bFromExe := False;
  if NOT ((name.sin_addr.S_addr = 16777343) AND (name.sin_port = htons(TransServerPort))) then Begin
    bFromExe := IsAddressFromExecutable(connectReturnAddr);
  End;

  {$IFDEF DEBUG}
  Write('HookLogic: Trying to connect to ', inet_ntoa(name.sin_addr), ':', htons(name.sin_port));
  If(bFromExe) Then Write(' from client');
  WriteLn;
  {$ENDIF}

  if not bFromExe Then Begin
    THooker.Hooker.TrueAPI;
    Result := connect(s, TSockAddr(name), namelen);
    THooker.Hooker.TrueAPIEnd;
    Exit;
  End;


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
    If getsockname(ServSocket, TSockAddr(SockAddr), SA_Len) <> 0 Then SockPair := False;
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

procedure connectHook; asm
  POP EAX
  PUSH EAX
  MOV connectReturnAddr, EAX
  PUSH connectHookInvokeAddr
  RET
end;

procedure HookIt;
begin
  iIP := 0;
  iPort := 0;
  connectHookInvokeAddr := Cardinal(@connectHookInvoke);
  THooker.Hooker.HookFunction(@connectHook, GetProcAddress(GetModuleHandle('wsock32.dll'), 'connect'));
  THooker.Hooker.InjectIt;
end;

procedure UnHookIt;
Begin
  If THooker.Hooker.Injected Then THooker.Hooker.Restore;
  THooker.Hooker.Free;
  THooker.Hooker := nil;
End;

initialization
  TransServerPort := 0;
end.
