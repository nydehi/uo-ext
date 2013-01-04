unit HookLogic;

interface

uses Windows, APIHooker, ShardSetup, AbstractThread, PluginsShared, WinSock2, Messages, TLHelp32;

var
  iIP: Integer;
  iPort: Word;
  TransServerPort: Word;
  iSocket: TSocket;

procedure HookIt;
procedure UnHookIt;

implementation

uses Common, Plugins, ClientThread, ExecutableSections, CoreInitialization;

var
  connectReturnAddr: Cardinal;
  connectHookInvokeAddr: Cardinal;

procedure ExitProcessInvoke(uExitCode: UINT); stdcall;
Begin
  CoreFinalization; // There will be unhooked all hooks, so it's true API after here.
  ExitProcess(uExitCode);
End;

function connectHookInvoke(s: TSocket; var name: TSockAddr; namelen: Integer): Integer; stdcall;
var
  ServSocket, DistantServerSocket: TSocket;
  SockPair:Boolean;
  SockAddr:TSockAddrIn;
  SA_Len: Integer;
  bFromExe: Boolean;
Begin
  bFromExe := False;
  if NOT ((TSockAddrIn(name).sin_addr.S_addr = 16777343) AND (TSockAddrIn(name).sin_port = htons(TransServerPort))) then Begin
    bFromExe := IsAddressFromExecutable(connectReturnAddr);
  End;

  {$IFDEF DEBUG}
  Write('HookLogic: Trying to connect to ', inet_ntoa(TSockAddrIn(name).sin_addr), ':', htons(TSockAddrIn(name).sin_port));
  If(bFromExe) Then Write(' from client');
  WriteLn;
  {$ENDIF}

  if not bFromExe Then Begin
    THooker.Hooker.TrueAPI;
    Result := connect(s, name, namelen);
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
      iIP := TSockAddrIn(name).sin_addr.S_addr;
      iPort := TSockAddrIn(name).sin_port;
    End;
  End;

  if TPluginSystem.Instance.ProtocolHook then Begin
    {$IFDEF DEBUG}
    WriteLn('HookLogic: Creating new protocol thread.');
    {$ENDIF}
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
      UOClientSocket := s;
      Run;
    end;
    Result := 0;
  End Else Begin
    {$IFDEF DEBUG}
    WriteLn('HookLogic: Protocol thread won''t be created.');
    WriteLn('            Because no one plugin ask for it in it''s props.');
    {$ENDIF}
    CopyMemory(@SockAddr, @name, SizeOf(SockAddr));
    SockAddr.sin_addr.S_addr := iIP;
    SockAddr.sin_port := iPort;

    THooker.Hooker.TrueAPI;
    Result := connect(s, TSockAddr(SockAddr), SizeOf(SockAddr));
    THooker.Hooker.TrueAPIEnd;
    TPluginSystem.Instance.ProxyStart(s, INVALID_SOCKET, s); // Do we really need a separate thread here?
  End;
  iSocket := s;
End;

function closesocketHook(s: TSocket): Integer; stdcall;
Begin
  if not TPluginSystem.Instance.ProtocolHook then Begin
    if s = iSocket then TPluginSystem.Instance.ProxyEnd(0, 0);
  End;
  THooker.Hooker.TrueAPI;
  Result := closesocket(s);
  THooker.Hooker.TrueAPIEnd;
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
  THooker.Hooker.HookFunction(@closesocketHook, GetProcAddress(GetModuleHandle('wsock32.dll'), 'closesocket'));
  THooker.Hooker.HookFunction(@ExitProcessInvoke, GetProcAddress(GetModuleHandle('kernel32.dll'), 'ExitProcess'));
  THooker.Hooker.InjectIt;
end;

procedure UnHookIt;
Begin
  THooker.Hooker.Free;
End;

initialization
  TransServerPort := 0;
  iSocket := INVALID_SOCKET;
end.
