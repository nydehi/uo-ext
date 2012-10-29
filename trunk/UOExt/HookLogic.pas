unit HookLogic;

interface

uses Windows, APIHooker, ShardSetup, AbstractThread, PluginsShared, WinSock2, Messages, TLHelp32;

var
  iIP: Integer;
  iPort: Word;
  TransServerPort: Word;

procedure ReadExecutableSections;
procedure HookIt;

implementation

uses Common, Plugins, ClientThread;

type
  RMemoryRange = record
    Start: Cardinal;
    Length: Cardinal;
  end;
  TSectionsRanges = record
    Count: Cardinal;
    Items: Array [0..0] of RMemoryRange;
  end;
  PSectionsRanges = ^TSectionsRanges;
var
  ExeSections: PSectionsRanges;
  RazorCryptSections: PSectionsRanges;
  connectReturnAddr: Cardinal;
  connectHookInvokeAddr: Cardinal;

function ReadSections(aModule: THandle): PSectionsRanges;
type
  TSections = Array [0..999] of TImageSectionHeader;
  PSections = ^TSections;
var
  pDOS: PImageDosHeader;
  pPE: PImageFileHeader;
  pOpt: PImageOptionalHeader;
  pSect: PSections;
  i, j, cSections, cSectionStart: Cardinal;
Begin
  pDOS := Pointer(aModule);
  pPE := Pointer(Cardinal(pDOS) + Cardinal(pDOS^._lfanew) + 4);
  pOpt := Pointer(Cardinal(pPE) + SizeOf(TImageFileHeader));
  pSect := Pointer(Cardinal(pOpt) + SizeOf(TImageOptionalHeader) + (pOpt^.NumberOfRvaAndSizes - IMAGE_NUMBEROF_DIRECTORY_ENTRIES) * SizeOf(TImageDataDirectory));

  cSections := 0;
  For i := 0 to pPE^.NumberOfSections - 1 do Begin
    If pSect^[i].Characteristics AND IMAGE_SCN_MEM_EXECUTE = IMAGE_SCN_MEM_EXECUTE Then Begin
      cSections := cSections + 1;
    End;
  End;

  Result := GetMemory(SizeOf(TSectionsRanges) + (cSections - 1) * SizeOf(RMemoryRange));
  Result^.Count := cSections;
  j := 0;
  For i := 0 to pPE^.NumberOfSections - 1 do Begin
    If pSect^[i].Characteristics AND IMAGE_SCN_MEM_EXECUTE = IMAGE_SCN_MEM_EXECUTE Then Begin
      cSectionStart := Cardinal(pDOS) + pSect^[i].VirtualAddress{ + pOpt^.ImageBase};
      Result^.Items[j].Start := cSectionStart;
      Result^.Items[j].Length := pSect^[i].Misc.VirtualSize;
      j := j + 1;
    End;
  End;
End;

procedure ReadExecutableSections;
Begin
  ExeSections := ReadSections(GetModuleHandleA(nil));
End;

function connectHookInvoke(s: TSocket; var name: TSockAddrIn; namelen: Integer): Integer; stdcall;
var
  ServSocket, DistantServerSocket: TSocket;
  SockPair:Boolean;
  SockAddr:TSockAddrIn;
  SA_Len: Integer;
  i: Cardinal;
  bFromExe: Boolean;
Begin
  bFromExe := False;
  if NOT ((name.sin_addr.S_addr = 16777343) AND (name.sin_port = htons(TransServerPort))) then Begin
    if ShardSetup.Razor AND (RazorCryptSections = nil) then RazorCryptSections := ReadSections(GetModuleHandleA('Crypt.dll'));

    For i := 0 to ExeSections^.Count - 1 do if (ExeSections^.Items[i].Start <= connectReturnAddr) AND ((ExeSections^.Items[i].Length + ExeSections^.Items[i].Start) >= connectReturnAddr) then Begin
      bFromExe := True;
      Break;
    End;
    if not bFromExe and ShardSetup.Razor then for i := 0 to RazorCryptSections^.Count - 1 do if (RazorCryptSections^.Items[i].Start <= connectReturnAddr) AND ((RazorCryptSections^.Items[i].Length + RazorCryptSections^.Items[i].Start) >= connectReturnAddr) then Begin
      bFromExe := True;
      Break;
    End;
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

initialization
  ExeSections := nil;
  RazorCryptSections := nil;
  TransServerPort := 0;
end.
