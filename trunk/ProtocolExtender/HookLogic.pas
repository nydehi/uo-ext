unit HookLogic;

interface

uses Windows, APIHooker, ShardSetup, AbstractThread, ListeningThread, PluginsShared, WinSock, Messages;


procedure HookIt;

implementation

uses Common, Plugins;

type
  TWndProc = function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

var
  WndProc: TWndProc;
  ListeningThread: TServerThread;

function connectHook(s: TSocket; var name: TSockAddr; namelen: Integer): Integer; stdcall;
var
  ConnInfo: TSockAddr;
Begin
  ListeningThread := TServerThread.Create;
  ListeningThread.Run;

  CopyMemory(@ConnInfo, @name, SizeOf(ConnInfo));
  ConnInfo.sin_addr.S_addr := htonl(INADDR_LOOPBACK);

  repeat
    ConnInfo.sin_port := htons(ListeningThread.LocalPort);
  until (ConnInfo.sin_port<>0) or not ListeningThread.Running;


  THooker.Hooker.TrueAPI;
  Result := connect(s, ConnInfo, namelen);
  THooker.Hooker.TrueAPIEnd;
End;

function MyWindowProcA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
Begin
  if Msg = WM_QUIT then TAbstractThread.AllStop;
  Result := WndProc(hWnd, Msg, wParam, lParam);
End;


function CreateWindowExAHook(dwExStyle: DWORD; lpClassName: PAnsiChar; lpWindowName: PAnsiChar;
  dwStyle: DWORD; X, Y, nWidth, nHeight: Integer; hWndParent: HWND;
  hMenu: HMENU; hInstance: HINST; lpParam: Pointer): HWND; stdcall;
Begin
  THooker.Hooker.TrueAPI;
  Result := CreateWindowExA(dwExStyle, lpClassName, lpWindowName, dwStyle, X, Y, nWidth, nHeight, hWndParent, hMenu, hInstance, lpParam);
  THooker.Hooker.TrueAPIEnd;
  if Result = INVALID_HANDLE_VALUE then Exit;

  WndProc := TWndProc(GetWindowLongPtrA(Result, GWLP_WNDPROC));
  SetWindowLongPtrA(Result, GWLP_WNDPROC, 0);
End;



procedure HookIt;
begin
  THooker.Hooker.HookFunction(@connectHook, GetProcAddress(GetModuleHandle('wsock32.dll'), 'connect'));
  THooker.Hooker.HookFunction(@CreateWindowExAHook, GetProcAddress(GetModuleHandle('user32.dll'), 'CreateWindowExA'));
  THooker.Hooker.InjectIt;
end;

end.
