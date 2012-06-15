unit HookLogic;

interface

uses Windows, APIHooker, ShardSetup, AbstractThread, PluginsShared, WinSock, Messages;


procedure HookIt;

implementation

uses Common, Plugins, ClientThread;

type
  TWndProc = function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

var
  WndProc: TWndProc;
  iIP: Integer;
  iPort: Word;

function connectHook(s: TSocket; var name: TSockAddr; namelen: Integer): Integer; stdcall;
var
  ServSocket, DistantServerSocket: TSocket;
  SockPair:Boolean;
  SockAddr:TSockAddr;
  SA_Len: Integer;
Begin
  Result := SOCKET_ERROR;
  WSASetLastError(WSAECONNREFUSED);
  iIP := htonl(name.sin_addr.S_addr);
  iPort := htons(name.sin_port);

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

function MyWindowProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
Begin
  if Msg = WM_QUIT then Begin
    TAbstractThread.AllStop;
  End;
  Result := WndProc(hWnd, Msg, wParam, lParam);
End;


function CreateWindowExWHook(dwExStyle: DWORD; lpClassName: PAnsiChar; lpWindowName: PAnsiChar;
  dwStyle: DWORD; X, Y, nWidth, nHeight: Integer; hWndParent: HWND;
  hMenu: HMENU; hInstance: HINST; lpParam: Pointer): HWND; stdcall;
Begin
  THooker.Hooker.TrueAPI;
  Result := CreateWindowExA(dwExStyle, lpClassName, lpWindowName, dwStyle, X, Y, nWidth, nHeight, hWndParent, hMenu, hInstance, lpParam);
  THooker.Hooker.TrueAPIEnd;
  if Result = INVALID_HANDLE_VALUE then Exit;

  WndProc := TWndProc(GetWindowLongPtrW(Result, GWLP_WNDPROC));
  SetWindowLongPtrW(Result, GWLP_WNDPROC, Integer(@MyWindowProc));
End;



procedure HookIt;
begin
  iIP := 0;
  iPort := 0;
  THooker.Hooker.HookFunction(@connectHook, GetProcAddress(GetModuleHandle('wsock32.dll'), 'connect'));
//  THooker.Hooker.HookFunction(@CreateWindowExWHook, GetProcAddress(GetModuleHandle('user32.dll'), 'CreateWindowExW'));
  THooker.Hooker.InjectIt;
end;

end.
