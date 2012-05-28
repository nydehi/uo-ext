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
  iIP: Integer;
  iPort: Word;

function connectHook(s: TSocket; var name: TSockAddr; namelen: Integer): Integer; stdcall;
var
  ConnInfo: TSockAddr;
Begin
  if(iIP = 0) Then Begin
    iIP := htonl(name.sin_addr.S_addr);
    iPort := htons(name.sin_port);
  End;

  ListeningThread := TServerThread.Create(iIP, iPort);
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
