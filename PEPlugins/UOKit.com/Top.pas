unit Top;

interface

uses Windows, PluginAPI, PluginsShared, WinSock2;

procedure Init(API:TPluginApi);
procedure Clear;
procedure Network_Start(Data: PPE_ProxyStartEvent);
Procedure Network_End;

implementation

uses APIHooker, Common, AbstractThread, WinInet;

const
  cTopName: AnsiString = 'top.uokit.com' + #0;

type
  TTopThread=class(TAbstractThread)
  private
    FLogin: PAnsiChar;
    FShard: PAnsiChar;
    FCharacter: PAnsiChar;
    FIP: Cardinal;
    FPort: Word;

    FTopSockAddr: TSockAddrIn;
    FTopSock: TSocket;
    FEvents: Array [0..1] of WSAEVENT;

    FUserSignature: Cardinal;
    FUserSignatureSendMethod: Byte;
    FUserSignatureSetEvent: THandle;

    FResolveTime: Cardinal;
    function TryResolveName:Boolean;
    function TryConnect:Boolean;

    procedure SendTopData;
    procedure ProcessTopData;

    function DownloadNewVersion(AURL: PAnsiChar):Boolean;
  protected
    function Execute:Integer; override;
    procedure Stop; override;
  public
    property Login: PAnsiChar read FLogin write FLogin;
    property Shard: PAnsiChar read FShard write FShard;
    property Character: PAnsiChar read FCharacter write FCharacter;
    property IP: Cardinal read FIP write FIP;
    property Port: Word read FPort write FPort;

    property UserSignature: Cardinal read FUserSignature;
    property UserSignatureSendMethod: Byte read FUserSignatureSendMethod;
    property UserSirnatureSetEvent: THandle read FUserSignatureSetEvent write FUserSignatureSetEvent;

    property CloseEvent: WSAEVENT read FEvents[0];
  end;

var
  Login, Shard, Character: PAnsiChar;
  ServerSocket: TSocket;
  DataGathered: Boolean;

function TryExtractLoginInfo(lpFileName: PAnsiChar):Boolean;
const
  cDesktop: Array [0..9] of AnsiChar = 'Desktop'+#0;
var
  pCurrent, pReturn: PAnsiChar;
  bFilledNum, bStage, bOffset: Byte;
  ppFilling: PPointer;
Begin
  pCurrent := lpFileName;
  bFilledNum := 0;
  bStage := 0;
  pReturn := pCurrent;
  bOffset := 0;

  While pCurrent^ <> #0 do Begin
    case bStage of
      0: Begin
        If (pCurrent^ = 'D') Then Begin
          bStage := 1;
          bOffset := 1;
          pReturn := pCurrent;
        End;
      End;
      1: Begin
        if (pCurrent^ <> PAnsiChar(Cardinal(@cDesktop) + bOffset)^) then Begin
          If PAnsiChar(Cardinal(@cDesktop) + bOffset)^ = #0 then Begin
            bStage := 2;
            pReturn := PAnsiChar(Cardinal(pCurrent) + 1);
          End Else Begin
            bStage := 0;
            pCurrent := pReturn;
          End;
        End;
        bOffset := bOffset + 1;
      End;
      2: Begin
        If pCurrent^ = '/' then Begin
          ppFilling := nil;
          case bFilledNum of
            0: ppFilling := @Login;
            1: ppFilling := @Shard;
            2: ppFilling := @Character;
          end;

          if ppFilling = nil then Begin
            Result := False;
            Exit;
          End;
          If ppFilling^ <> nil Then FreeMemory(ppFilling^);
          ppFilling^ := GetMemory(Cardinal(pCurrent) - Cardinal(pReturn) + 1);
          ZeroMemory(ppFilling^, Cardinal(pCurrent) - Cardinal(pReturn) + 1);
          CopyMemory(ppFilling^, pReturn, Cardinal(pCurrent) - Cardinal(pReturn));
          bFilledNum := bFilledNum + 1;
          pReturn := PAnsiChar(Cardinal(pCurrent) + 1);
          if bFilledNum >= 3 then Begin
            Result := True;
            Exit;
          End;
        End;
      End;
    End;
    pCurrent := PAnsiChar(Cardinal(pCurrent) + 1);
  End;
  Result := False;
End;

function CreateFileAHook(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; stdcall;
Begin
  THooker.Hooker.TrueAPI;
  Result := CreateFileA(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
  THooker.Hooker.TrueAPIEnd;
  if (Result <> INVALID_HANDLE_VALUE) and not DataGathered then If TryExtractLoginInfo(lpFileName) Then Begin
    DataGathered := True;
    // Send data to Top.
  End;
End;


procedure Init(API:TPluginApi);
const
  cOld: AnsiString = '.old';
var
  hModule : THandle;
  Mem: TMemoryBasicInformation;
  lpModule: PAnsiChar;
  cModuleFileNameLength: Cardinal;
Begin
  Login := nil;
  Shard := nil;
  Character := nil;
  ServerSocket := INVALID_SOCKET;
  DataGathered := False;
  THooker.Hooker.HookFunction(@CreateFileAHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'CreateFileA'));
  THooker.Hooker.InjectIt;

  hModule := INVALID_HANDLE_VALUE;
  If(VirtualQuery(@TTopThread.DownloadNewVersion, Mem, SizeOf(Mem))>0) Then If Mem.AllocationBase <> nil Then hModule := THandle(Mem.AllocationBase);
  If(hModule = INVALID_HANDLE_VALUE) Then Exit;
  lpModule := GetMemory(MAX_PATH + 1);
  ZeroMemory(lpModule, MAX_PATH + 1);
  cModuleFileNameLength := GetModuleFileNameA(hModule, lpModule, MAX_PATH);
  CopyMemory(Pointer(Cardinal(lpModule) + cModuleFileNameLength), @cOld[1], 4);
  if Windows.GetFileAttributesA(lpModule) <> INVALID_FILE_ATTRIBUTES then DeleteFileA(lpModule);

End;

procedure Clear;
Begin
  if Login <> nil then FreeMemory(Login);
  if Shard <> nil then FreeMemory(Shard);
  if Character <> nil then FreeMemory(Character);

End;

procedure Network_Start(Data: PPE_ProxyStartEvent);
Begin
  ServerSocket := Data^.ServerSocket;
End;

Procedure Network_End;
Begin
End;

type
  PAddrInfoA = ^TAddrInfoA;
  TAddrInfoA = packed record
    ai_flags, ai_family, ai_sockettype, ai_protocol: Integer;
    ai_addrlen: Integer;
    ai_canonname: PAnsiChar;
    ai_addr: PSockAddr;
    ai_next: PAddrInfoA;
  end;

function getaddrinfo(nodename, servname: PAnsiChar; hints: PAddrInfoA; var res: PAddrInfoA): Integer; stdcall; external 'ws2_32.dll';
procedure freeaddrinfo(ai: PAddrInfoA); stdcall; external 'ws2_32.dll';

//TTopThread

function TTopThread.TryResolveName:Boolean;
var
  aiHints: TAddrInfoA;
  aiRes: PAddrInfoA;
begin
  aiHints.ai_family := AF_INET;
  aiHints.ai_sockettype := SOCK_STREAM;
  aiHints.ai_protocol := IPPROTO_TCP;

  Result := getaddrinfo(@cTopName[1], nil, @aiHints, aiRes) = 0;
  If Result Then Begin
    {$IFDEF DEBUG}
    Write('UOKit.com: Resolved IP for ', cTopName, ' is ');
    Write(TSockAddrIn(aiRes^.ai_addr^).sin_addr.S_un_b.s_b1, '.');
    Write(TSockAddrIn(aiRes^.ai_addr^).sin_addr.S_un_b.s_b2, '.');
    Write(TSockAddrIn(aiRes^.ai_addr^).sin_addr.S_un_b.s_b3, '.');
    WriteLn(TSockAddrIn(aiRes^.ai_addr^).sin_addr.S_un_b.s_b4);
    {$ENDIF}

    CopyMemory(@FTopSockAddr, aiRes^.ai_addr, SizeOf(FTopSockAddr));
    freeaddrinfo(aiRes);
  End Else Begin
    {$IFDEF DEBUG}
    WriteLn('UOKit.com: Can''t resolve ', cTopName, ' reason: ', WSAGetLastError, ' retry in 1 second.');
    {$ENDIF}
  End;
end;

function TTopThread.TryConnect:Boolean;
begin
  Result := False;

  FTopSock := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  If FTopSock = INVALID_SOCKET Then Begin
    {$IFDEF DEBUG}
    WriteLn('UOKit.com: Can''t create socket. Reason: ', WSAGetLastError);
    {$ENDIF}
  End Else If connect(FTopSock, TSockAddr(FTopSockAddr), SizeOf(FTopSockAddr)) = SOCKET_ERROR Then Begin
    closesocket(FTopSock);
    FTopSock := INVALID_SOCKET;
    {$IFDEF DEBUG}
    WriteLn('UOKit.com: Can''t connect to ', cTopName, '. Reason: ', WSAGetLastError);
    {$ENDIF}
  End Else Begin
    Result := True;
  End;
end;

procedure TTopThread.SendTopData;
type
  TTopDatapacket = packed record
    Header: Byte;
    ProtocolVersion: Byte;
    IP: Cardinal;
    Port: Word;
    DataFlags: Byte;
  end;
  PTopDatapacket = ^TTopDatapacket;
const
  DF_LOGIN = 1;
  DF_SHARD = 2;
  DF_CHARACTER = 4;
  DF_ALL = 7;
  DF_CURRENT = 7;

  ProtocolVersion: Byte = 0;
var
  Packet: PTopDataPacket;
  PacketSize: Integer;
  pPointer: Pointer;
  cCurrentStringLength: Cardinal;
Begin
  PacketSize := SizeOf(TTopDatapacket) + Length(FLogin) + Length(FShard) + Length(FCharacter) + 3;
  Packet := GetMemory(PacketSize);
  ZeroMemory(Packet, PacketSize);

  Packet^.Header := 0;
  Packet^.ProtocolVersion := ProtocolVersion;
  Packet^.IP := FIP;
  Packet^.Port := FPort;
  Packet^.DataFlags := DF_CURRENT;

  pPointer := Pointer(Cardinal(Packet) + SizeOf(TTopDatapacket));

  cCurrentStringLength := Length(FLogin);
  CopyMemory(pPointer, FLogin, cCurrentStringLength);
  pPointer := Pointer(Cardinal(pPointer) + cCurrentStringLength + 1);

  cCurrentStringLength := Length(FShard);
  CopyMemory(pPointer, FShard, cCurrentStringLength);
  pPointer := Pointer(Cardinal(pPointer) + cCurrentStringLength + 1);

  cCurrentStringLength := Length(FCharacter);
  CopyMemory(pPointer, FCharacter, cCurrentStringLength);

  send(FTopSock, Packet^, PacketSize, 0);
  FreeMemory(pPointer);
End;

procedure TTopThread.ProcessTopData;
type
  TUpdatePacket = packed record
    Header: Byte;
    Size: Word;
  end;
var
  Header: Byte;
  UpdatePacket: TUpdatePacket;
  ReadyData: Integer;
  lpURL: PAnsiChar;
Begin
  ReadyData := recv(FTopSock, Header, 1, MSG_PEEK);
  case Header of
    0: Begin // Need update. Next 2 bytes is URL size, Than URL.
      ReadyData := recv(FTopSock, UpdatePacket, SizeOf(UpdatePacket), MSG_PEEK);
      if ReadyData >= UpdatePacket.Size then Begin
        lpURL := GetMemory(UpdatePacket.Size - 2);
        ZeroMemory(lpURL, UpdatePacket.Size - 2);
        recv(FTopSock, UpdatePacket, SizeOf(UpdatePacket), 0);
        recv(FTopSock, lpURL^, UpdatePacket.Size - 3, 0);
        if not DownloadNewVersion(lpURL) then Begin
          MessageBoxA(0, 'Can''t download new version of UOKit.com plugin. Please contact shard owner.', nil, MB_OK);
          Halt(1);
        End;
      End;
    End;
    1: Begin // Need to send this data to server. Not working right now.
      If(ReadyData >= 6) Then Begin
        recv(FTopSock, Header, 1, 0);
        recv(FTopSock, FUserSignatureSendMethod, 1, 0);
        recv(FTopSock, FUserSignature, 4, 0);
        if FUserSignatureSetEvent <> INVALID_HANDLE_VALUE then SetEvent(FUserSignatureSetEvent);
      End;
    End;
  end;

End;

Function TTopThread.DownloadNewVersion(AURL: PAnsiChar):Boolean;
const
  cNew: AnsiString = '.new';
  cOld: AnsiString = '.old';
var
  Mem: TMemoryBasicInformation;
  hModule, hNewFile: THandle;
  lpModule, lpNewFile, lpOldFile: PAnsiChar;
  cModuleLength, cDataAvail: Cardinal;
  hInternet, hConnect, hRequest: Pointer;
  pBuffer: Pointer;
  cBufferSize: Cardinal;
Begin
  Result := False;
  hModule := INVALID_HANDLE_VALUE;
  If(VirtualQuery(@TTopThread.DownloadNewVersion, Mem, SizeOf(Mem))>0) Then If Mem.AllocationBase <> nil Then hModule := THandle(Mem.AllocationBase);
  If(hModule = INVALID_HANDLE_VALUE) Then Exit;
  lpModule := GetMemory(MAX_PATH + 1);
  ZeroMemory(lpModule, MAX_PATH + 1);
  cModuleLength := GetModuleFileNameA(hModule, lpModule, MAX_PATH);

  lpNewFile := GetMemory(cModuleLength + 5);
  ZeroMemory(lpNewFile, cModuleLength + 5);
  CopyMemory(lpNewFile, lpModule, cModuleLength);
  CopyMemory(Pointer(Cardinal(lpNewFile) + cModuleLength), @cNew[1], 4);

  lpOldFile := GetMemory(cModuleLength + 5);
  ZeroMemory(lpOldFile, cModuleLength + 5);
  CopyMemory(lpOldFile, lpModule, cModuleLength);
  CopyMemory(Pointer(Cardinal(lpOldFile) + cModuleLength), @cOld[1], 4);

  cBufferSize := 0;
  pBuffer := nil;

  hNewFile := CreateFileA(lpNewFile, GENERIC_READ XOR GENERIC_WRITE, FILE_SHARE_READ XOR FILE_SHARE_WRITE, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  hInternet := InternetOpenA( 'UOKit.com', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  hConnect := InternetConnectA(hInternet, AURL, INTERNET_DEFAULT_HTTP_PORT, 'anonymous', nil, INTERNET_SERVICE_HTTP, 0, 0);
  hRequest := HttpOpenRequestA(hConnect, nil, AURL, nil, nil, nil, INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_RELOAD, 0);
  If HTTPSendRequestA(hRequest, nil, 0, nil, 0) Then Repeat
    If InternetQueryDataAvailable(hRequest, cDataAvail, 0, 0) Then Begin
      if cDataAvail = 0 then Break;
      if (cBufferSize < cDataAvail)or(pBuffer = nil) then Begin
        cBufferSize := cDataAvail;
        FreeMemory(pBuffer);
        pBuffer := GetMemory(cBufferSize);
      End;
      If not InternetReadFile(hRequest, pBuffer, cBufferSize, cDataAvail) Then Break;
      WriteFile(hNewFile, pBuffer^, cBufferSize, cDataAvail, nil);
    End Else Begin
      if GetLastError = ERROR_NO_MORE_FILES then Break;
    End;
  Until False;
  CloseHandle(hNewFile);
  InternetCloseHandle(hRequest);
  InternetCloseHandle(hConnect);
  InternetCloseHandle(hInternet);

  MoveFileA(lpModule, lpOldFile);
  MoveFileA(lpNewFile, lpModule);

  FreeMemory(lpModule);
  FreeMemory(lpOldFile);
  FreeMemory(lpNewFile);
  FreeMemory(pBuffer);
  Result := True;
End;

function TTopThread.Execute:Integer;
const
  FailTimeout: Cardinal = 1000;
var
  TimeOut: Cardinal;
  NeEvents: WSANETWORKEVENTS;
begin
  FTopSock := INVALID_SOCKET;
  ZeroMemory(@FTopSockAddr, SizeOf(FTopSockAddr));
  FResolveTime := 0;
  FEvents[0] := CreateEventA(nil, True, False, nil);
  FEvents[1] := WSACreateEvent;
  FUserSignature := 0;
  FUserSignatureSetEvent := INVALID_HANDLE_VALUE;

  repeat
    if FTopSock = INVALID_SOCKET then Begin
      If (GetTickCount - FResolveTime) > 60000 Then If TryResolveName Then FResolveTime := GetTickCount;
      If FTopSockAddr.sin_addr.S_addr <> 0 Then If TryConnect Then Begin
        WSAEventSelect(FTopSock, FEvents[1], FD_READ XOR FD_CLOSE);
        SendTopData;
      End;
    End;
    TimeOut := WSA_INFINITE;
    if (FTopSock = INVALID_SOCKET) then TimeOut := FailTimeout;

    case WSAWaitForMultipleEvents(2, @FEvents, False, TimeOut, False) of
      WSA_WAIT_EVENT_0: Break;
      WSA_WAIT_EVENT_0 + 1: Begin
        if WSAEnumNetworkEvents(FTopSock, FEvents[1], NeEvents) <> SOCKET_ERROR Then Begin
          If (NeEvents.lNetworkEvents = FD_READ)AND(NeEvents.iErrorCode[FD_READ_BIT] = 0) Then begin
            ProcessTopData;
          End Else Begin
            WSAEventSelect(FTopSock, FEvents[1], 0);
            closesocket(FTopSock);
            FTopSock := INVALID_SOCKET;
          End;
        End;
      End;
    end;
  until False;
  WSAEventSelect(FTopSock, FEvents[1], 0);
  closesocket(FTopSock);
  WSACloseEvent(FEvents[1]);
  CloseHandle(FEvents[0]);
  {$IFDEF DEBUG}
  WriteLn('UOKit.com: Top connection closed. All done.');
  {$ENDIF}
  Result := 0;
end;

procedure TTopThread.Stop;
Begin
  SetEvent(CloseEvent);
End;


end.
