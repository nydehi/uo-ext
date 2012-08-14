unit Updater;

interface

uses Windows, WinSock, Common, md5;

type
  TDllServerInfo = packed record
    Size: Cardinal;
    MD5: Array [0..15] of Byte;
  end;
  PDllServerInfo = ^TDllServerInfo;
  TDllServerInfoList = packed record
    Amount: Byte;
    Items: Array [0..0] of TDllServerInfo;
  end;
  PDllServerInfoList = ^TDllServerInfoList;
  TPluginInitInfo = packed record
    Dll:Byte;
    Plugin: Byte;
    PluginHandle: Byte;
    PacketsCount: Byte;
  end;
  PPluginInitInfo = ^TPluginInitInfo;
  TPluginInitInfoList = packed record
    Amount: Byte;
    Items: Array [0..0] of TPluginInitInfo;
  end;
  PPluginInitInfoList = ^TPluginInitInfoList;

  TUpdater=class
  strict private var
    // Network
    FSocket: TSocket;

    FDllServerInfoList: PDllServerInfoList;
    FPluginInitInfoList: PPluginInitInfoList;

    FUOExtMD5: TMD5Digest;
    FUOExtGUIMD5: TMD5Digest;
    FNeedUpdateUOExt: Boolean;
    FNeedUpdateUOExtGUI: Boolean;
    FReverse: Boolean;

    procedure ReloadUOExt;

    // Network
    function UOExtGetPacket(var Size: Cardinal): Pointer;

    function TryServerConnect: Boolean;
    function TryUpdaterConnect: Boolean;
    function UOExtHandshake: Boolean;
    function UOExtGetDllList: Boolean;
    function UOExtGetPluginsLoadingList: Boolean;
    function UOExtGetDlls(WantedList: PByteArray; WantedSize: Word): Boolean;
    function UOExtGetEndSequence: Boolean;
    procedure UOExtDisconnect;

    function UOExtGetMissingDlls(var Dlls:Pointer; var Count: Byte): Boolean;

  public
    property DllList: PDllServerInfoList read FDllServerInfoList;
    property PluginLoadingList: PPluginInitInfoList read FPluginInitInfoList;
    constructor Create;
    function GatherMD5Info: Boolean;
    function Connect: Boolean;
    function SelfUpdate: Integer;
    function GetDllsFromServer: Boolean;
    procedure Cleanup;
    destructor Destroy; override;

    function UOExtPacket(Header:Byte; Data:Pointer; Size:Cardinal): Boolean; overload;
    function UOExtPacket(Data:Pointer; Size:Cardinal): Boolean; overload;
    function UOExtTryGetPacket(var Size:Cardinal; Timeout: Cardinal): Pointer;
    function UOExtCanRead: Boolean;

  end;

implementation

uses PreConnectIPDiscover, ShardSetup, GUI;

constructor TUpdater.Create;
var
  oldUOExt: AnsiString;
begin
  Inherited;
  FDllServerInfoList := nil;
  FPluginInitInfoList := nil;

  oldUOExt := ShardSetup.UOExtBasePath + 'UOExt.dll.old';
  if FileExists(oldUOExt) then Begin
    oldUOExt := oldUOExt + #0;
    repeat
      DeleteFileA(@oldUOExt[1]);
      Sleep(100);
    until not FileExists(oldUOExt);
  End;

end;

destructor TUpdater.Destroy;
begin
  Cleanup;
  if FSocket <> 0 then Begin
    closesocket(FSocket);
    FSocket := 0;
  End;

  Inherited;
end;

function TUpdater.GatherMD5Info: Boolean;
var
  F:File;
  pData: Pointer;
  fSize: Cardinal;
Begin
  Result := False;
  if not FileExists(ShardSetup.UOExtBasePath + 'UOExt.dll') then Exit;

  FileMode := 32;
  AssignFile(F, String(ShardSetup.UOExtBasePath + 'UOExt.dll'));
  Reset(F, 1);
  fSize := FileSize(F);
  pData := GetMemory(fSize);
  BlockRead(F, pData^, fSize);
  CloseFile(F);
  FileMode := 2;
  FUOExtMD5 := md5.MD5Buffer(pData^, fSize);
  FreeMemory(pData);
  Result := True;
  If FileExists(ShardSetup.UOExtBasePath + ShardSetup.GUIDLLName) then Begin
    FileMode := 32;
    AssignFile(F, String(ShardSetup.UOExtBasePath + ShardSetup.GUIDLLName));
    Reset(F, 1);
    fSize := FileSize(F);
    pData := GetMemory(fSize);
    BlockRead(F, pData^, fSize);
    CloseFile(F);
    FileMode := 2;
    FUOExtGUIMD5 := md5.MD5Buffer(pData^, fSize);
    FreeMemory(pData);
  End;
End;

function TUpdater.Connect:Boolean;
begin
  Result := False;
  If not TryServerConnect then if not TryUpdaterConnect Then Exit;
  if not UOExtHandshake then Exit;
  Result := True;
end;


function TUpdater.GetDllsFromServer: Boolean;
var
  MissingDllsCount: Byte;
  MissingDlls: Pointer;
Begin
  Result := False;
  if not Self.UOExtGetDllList then Exit;
  if not Self.UOExtGetPluginsLoadingList then Exit;
  if not Self.UOExtGetMissingDlls(MissingDlls, MissingDllsCount) then Exit;
  if not Self.UOExtGetDlls(MissingDlls, MissingDllsCount) then Exit;
  If Assigned(MissingDlls) Then FreeMemory(MissingDlls);
  if not Self.UOExtGetEndSequence then Exit;

  Result := True;
End;

function TUpdater.TryServerConnect: Boolean;
var
  IP: Cardinal;
  Port: Word;

  WSAData: TWSAData;
  SockAddr: TSockAddr;
  bufPacket: Array [0..20] of Byte;
  bNoDelay: BOOL;

  WriteOffSet: Cardinal;
  recvResult: Integer;
  startGTC, currGTC: Cardinal;
  fdset: TFDSet;
  tv: TTimeVal;

begin
  Result := False;
  if ShardSetup.Port = 0 then Begin
    If not PreConnectIPDiscover.GetConnInfo(IP, Port) Then Exit;
  End Else Begin
    IP := ShardSetup.IP;
    Port := ShardSetup.Port;
  End;
  ShardSetup.UpdateIP := IP;
  WSAStartup($101, WSAData);

  FSocket := WinSock.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  IF FSocket=INVALID_SOCKET Then Exit;
  ZeroMemory(@SockAddr, SizeOf(SockAddr));
  SockAddr.sin_family:=AF_INET;

  SockAddr.sin_port:=Port;
  SockAddr.sin_addr.S_addr:=IP;

  If WinSock.connect(FSocket, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR Then Begin
    closesocket(FSocket);
    Exit;
  End;
  bNoDelay := True;
  setsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY, @bNoDelay, SizeOf(bNoDelay));

  ZeroMemory(@bufPacket[0], 21);

  bufPacket[0] := $EF;
  If send(FSocket, bufPacket, 21, 0) <> 21 Then Begin
    closesocket(FSocket);
    Exit;
  End;
  Result := False;
  WriteOffSet := 0;
  startGTC := GetTickCount;
  Repeat
    FD_ZERO(fdset);
    FD_SET(FSocket, fdset);
    currGTC := GetTickCount;
    tv.tv_sec := 5 - (currGTC - startGTC) DIV 1000;
    tv.tv_usec := 5 - (currGTC - startGTC) MOD 1000;
    select(0, @fdset, nil, nil, @tv);
    If FD_ISSET(FSocket, fdset) then Begin
      recvResult := recv(FSocket, Pointer(@bufPacket[WriteOffSet])^, 3 - WriteOffSet, 0);
      If recvResult <= 0 then Begin
        closesocket(FSocket);
        Exit;
      End;
      WriteOffSet := WriteOffSet + Cardinal(recvResult);
      If (WriteOffSet >= 1) AND (bufPacket[0] <> 0) then Begin
        closesocket(FSocket);
        Exit;
      End;
    End;
  Until (WriteOffSet >= 3)OR((GetTickCount - startGTC) DIV 1000 > 5);

  If(WriteOffSet < 3) Then Begin
    {$IFDEF DEBUG}
    WriteLn('UOExtProtocol: Server didn''t respond on UOExt support ask.');
    {$ENDIF}
    closesocket(FSocket);
    Exit;
  End;
  ShardSetup.Encrypted := bufPacket[1] AND $01 = $01;
  ShardSetup.InternalProtocolHeader := bufPacket[2];
  Result := True;
end;

function TUpdater.TryUpdaterConnect:Boolean;
var
  IP: Cardinal;
  Port: Word;

  WSAData: TWSAData;
  SockAddr: TSockAddr;
  bufPacket: Array [0..20] of Byte;
  bNoDelay: BOOL;
begin
  Result := False;
  if ShardSetup.Port = 0 then Begin
    If not PreConnectIPDiscover.GetConnInfo(IP, Port) Then Exit;
  End Else Begin
    IP := ShardSetup.IP;
    Port := ShardSetup.Port;
  End;
  ShardSetup.UpdateIP := IP;
  Port := NOT Port;
  WSAStartup($101, WSAData);

  FSocket := WinSock.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  IF FSocket=INVALID_SOCKET Then Exit;
  ZeroMemory(@SockAddr, SizeOf(SockAddr));
  SockAddr.sin_family:=AF_INET;

  SockAddr.sin_port:=Port;
  SockAddr.sin_addr.S_addr:=IP;

  If WinSock.connect(FSocket, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR Then Begin
    closesocket(FSocket);
    Exit;
  End;
  bNoDelay := True;
  setsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY, @bNoDelay, SizeOf(bNoDelay));
  ShardSetup.UsingUpdateServer := True;
  Result := True;
end;

procedure TUpdater.UOExtDisconnect;
begin
  closesocket(FSocket);
  FSocket := INVALID_SOCKET;
  ShardSetup.UsingUpdateServer := False;
  WSACleanup;
end;

function TUpdater.UOExtHandshake:Boolean;
const
  PacketSize: Word = 35;
var
  Packet: Pointer;
  incPacketSize: Cardinal;
  UpdateSet, FlagsSet: Byte;
begin
  Result := False;

  Packet := GetMemory(PacketSize);
  ZeroMemory(Packet, PacketSize);
  CopyMemory(Pointer(Cardinal(Packet) + 3), @FUOExtMD5, 16);
  CopyMemory(Pointer(Cardinal(Packet) + 19), @FUOExtGUIMD5, 16);
  Self.UOExtPacket(Packet, PacketSize);
  FreeMemory(Packet);

  Packet := Self.UOExtGetPacket(incPacketSize);
  if Packet = nil then Exit;
  if (PWord(Packet)^ <> 0) then Begin
    FreeMemory(Packet);
    Exit;
  End;


  UpdateSet := PByte(Cardinal(Packet) + 2)^;
  FNeedUpdateUOExt := UpdateSet AND $01 = $01;
  FNeedUpdateUOExtGUI := UpdateSet AND $02 = $02;

  FlagsSet := PByte(Cardinal(Packet) + 3)^;
  If (FlagsSet AND $01) <> $01 then Begin
    ShardSetup.Encrypted := (FlagsSet AND $02) = $02;
    if (FlagsSet AND $04) = $04 then Begin
      ShardSetup.IP := PCardinal(Cardinal(Packet) + 4)^;
      ShardSetup.Port := PWord(Cardinal(Packet) + 8)^;
    End;
  End;

  Result := True;
end;

function TUpdater.UOExtGetEndSequence:Boolean;
const
  PacketSize: Word = 35;
var
  Packet: Pointer;
  incPacketSize: Cardinal;
begin
  Result := False;

  Packet := Self.UOExtGetPacket(incPacketSize);
  if Packet = nil then Exit;
  if ((PByte(Packet)^ <> 0)or(PByte(Cardinal(Packet) + 1)^ <> $FF)) then Begin
    FreeMemory(Packet);
    Exit;
  End;
  Result := True;
  FreeMemory(Packet);
end;

function TUpdater.SelfUpdate: Integer;
var
  cUpdateProcess: Cardinal;
  Packet: Pointer;
  PacketSize: Cardinal;

  fileSize, chunkSize, loadedSize: Cardinal;
  cFile: file;
Begin
  Result := -1;
  If FNeedUpdateUOExt then begin
    Packet := Self.UOExtGetPacket(PacketSize);
    If Packet = nil then Exit;
    If ((PByte(Packet)^ <> $00)or(PByte(Cardinal(Packet) + 1)^ <> $05)) then Begin
      FreeMemory(Packet);
      Exit;
    End;
    fileSize := PCardinal(Cardinal(Packet) + 2)^;
    cUpdateProcess := GUI.GUIStartProcess($FFFFFFFF, 0, 'UOExt.dll', 0, fileSize, 0);
    AssignFile(cFile, String(ShardSetup.UOExtBasePath + 'UOExt.dll.new'));
    Rewrite(cFile, 1);
    chunkSize := PacketSize - 6;
    if(chunkSize > fileSize) Then chunkSize := fileSize;
    BlockWrite(cFile, Pointer(Cardinal(Packet) + 6)^, chunkSize);
    loadedSize := chunkSize;
    GUI.GUIUpdateProcess(cUpdateProcess, 0, fileSize, loadedSize);
    FreeMemory(Packet);
    If(loadedSize < fileSize) Then Repeat
      Packet := Self.UOExtGetPacket(PacketSize);
      If Packet = nil then Exit;
      If ((PByte(Packet)^ <> $00)or(PByte(Cardinal(Packet) + 1)^ <> $04)) then Begin
        FreeMemory(Packet);
        Exit;
      End;
      chunkSize := PacketSize - 2;
      BlockWrite(cFile, Pointer(Cardinal(Packet) + 2)^, chunkSize);
      loadedSize := loadedSize + chunkSize;
      GUI.GUIUpdateProcess(cUpdateProcess, 0, fileSize, loadedSize);
    Until loadedSize = fileSize;
    CloseFile(cFile);
    ReloadUOExt;
    Result := 1;
  End Else If FNeedUpdateUOExtGUI then Begin
    Packet := Self.UOExtGetPacket(PacketSize);
    If Packet = nil then Exit;
    If ((PByte(Packet)^ <> $00)or(PByte(Cardinal(Packet) + 1)^ <> $05)) then Begin
      FreeMemory(Packet);
      Exit;
    End;
    fileSize := PCardinal(Cardinal(Packet) + 2)^;
    cUpdateProcess := GUI.GUIStartProcess($FFFFFFFF, 0, 'UOExt.gui.dll', 0, fileSize, 0);
    AssignFile(cFile, String(ShardSetup.UOExtBasePath + ShardSetup.GUIDLLName+'.new'));
    Rewrite(cFile, 1);
    chunkSize := PacketSize - 6;
    if(chunkSize > fileSize) Then chunkSize := fileSize;
    BlockWrite(cFile, Pointer(Cardinal(Packet) + 6)^, chunkSize);
    loadedSize := chunkSize;
    GUI.GUIUpdateProcess(cUpdateProcess, 0, fileSize, loadedSize);
    FreeMemory(Packet);
    If(loadedSize < fileSize) Then Repeat
      Packet := Self.UOExtGetPacket(PacketSize);
      If Packet = nil then Exit;
      If ((PByte(Packet)^ <> $00)or(PByte(Cardinal(Packet) + 1)^ <> $04)) then Begin
        FreeMemory(Packet);
        Exit;
      End;
      chunkSize := PacketSize - 2;
      BlockWrite(cFile, Pointer(Cardinal(Packet) + 2)^, chunkSize);
      loadedSize := loadedSize + chunkSize;
      GUI.GUIUpdateProcess(cUpdateProcess, 0, fileSize, loadedSize);
    Until loadedSize = fileSize;
    CloseFile(cFile);
    Result := 2;
  End;
  if Result = -1 then Result := 0;
End;

procedure TUpdater.ReloadUOExt;
var
  oldLib: AnsiString;
  newLib: AnsiString;
{
  Si:TStartupInfoA;
  Pi:TProcessInformation;
  cd:Array [0..MAX_PATH] of Byte;}
begin
  UOExtDisconnect;
  oldLib := ShardSetup.UOExtBasePath + 'UOExt.dll';
  newLib := ShardSetup.UOExtBasePath + 'UOExt.dll.old';
  MoveFileA(@oldLib[1], @newLib[1]);
  oldLib := ShardSetup.UOExtBasePath + 'UOExt.dll.new';
  newLib := ShardSetup.UOExtBasePath + 'UOExt.dll';
  MoveFileA(@oldLib[1], @newLib[1]);

{
  ZeroMemory(@Si, SizeOf(Si));
  Si.cb := SizeOf(Si);
  newLib := AnsiString(ParamStr(0)) + #0;
  GetCurrentDirectoryA(MAX_PATH, @cd[0]);
  CreateProcessA(nil, @newLib[1], nil, nil, False, 0, nil, @cd[0], Si, Pi);
}
end;


function TUpdater.UOExtGetDllList: Boolean;
const
  Packet: Cardinal = 0;
var
  recvPacket: Pointer;
  PacketSize: Cardinal;
  DllListSize: Byte;
Begin
  Result := False;

  // DllList
  recvPacket := Self.UOExtGetPacket(PacketSize);
  if recvPacket = nil then Exit;
  if (PByte(recvPacket)^ <> 0) OR (PByte(Cardinal(recvPacket) + 1)^ <> 1) then Begin
    FreeMemory(recvPacket);
    Exit;
  End;
  DllListSize := (PacketSize - 2) DIV 20;
  FDllServerInfoList := GetMemory(SizeOf(TDllServerInfo) * DllListSize + 1);
  FDllServerInfoList.Amount := DllListSize;
  CopyMemory(@FDllServerInfoList.Items[0], Pointer(Cardinal(recvPacket) + 2), SizeOf(TDllServerInfo) * DllListSize);
  FreeMemory(recvPacket);

  Result := True;
End;

function TUpdater.UOExtGetPluginsLoadingList: Boolean;
var
  recvPacket: Pointer;
  PacketSize: Cardinal;
  PluginsListSize: Byte;
Begin
  Result := False;

  // PluginsList
  recvPacket := Self.UOExtGetPacket(PacketSize);
  if recvPacket = nil then Exit;
  if (PByte(recvPacket)^ <> $00) OR (PByte(Cardinal(recvPacket) + 1)^ <> $02) then Begin
    FreeMemory(recvPacket);
    Exit;
  End;
  PluginsListSize := (PacketSize - 2) DIV 4;
  FPluginInitInfoList := GetMemory(SizeOf(TPluginInitInfo) * PluginsListSize + 1);
  FPluginInitInfoList^.Amount := PluginsListSize;
  CopyMemory(@FPluginInitInfoList^.Items[0], Pointer(Cardinal(recvPacket) + 2), SizeOf(TPluginInitInfo) * PluginsListSize);
  FreeMemory(recvPacket);

  Result := True;
End;

function TUpdater.UOExtGetDlls(WantedList: PByteArray; WantedSize: Word): Boolean;
var
  Packet: Pointer;
  PacketSize: Cardinal;
  i, j: Byte;
  DllId: Byte;
  SavedDllLength: Cardinal;
  CurrentDll: PDllServerInfo;
  F: File;
  Path: AnsiString;
  cUpdate: Cardinal;
Begin
  cUpdate := GUI.GUIStartProcess($FFFFFFFF, 0, 'Loading', 0, 1, 0);
  Result := False;
  If WantedSize > MAXWORD - 5 then Exit;

  PacketSize := WantedSize + 2;
  Packet := GetMemory(PacketSize);
  PByteArray(Packet)^[0] := 0;
  PByteArray(Packet)^[1] := 3;
  CopyMemory(Pointer(Cardinal(Packet) + 2), WantedList, WantedSize);
  // C->S ReqdLibrary
  Self.UOExtPacket(Packet, PacketSize);
  Freememory(Packet);

  If FDllServerInfoList.Amount > 0 Then For i := 0 to FDllServerInfoList.Amount -1 do Begin
    Packet := Self.UOExtGetPacket(PacketSize);
    if Packet = nil then Exit;
    If (PByteArray(Packet)^[0] <> 0)OR(PByteArray(Packet)^[1] <> 3) then Begin
      FreeMemory(Packet);
      Exit;
    End;
    DllId := PByteArray(Packet)^[3] SHL 8 + PByteArray(Packet)^[2];
    CurrentDll := @FDllServerInfoList.Items[DllId];
    SavedDllLength := 0;
    GUI.GUIUpdateProcess(cUpdate, 0, CurrentDll.Size, SavedDllLength);
    Path := '';
    for j := 0 to 15 do Path := Path + IntToHex(CurrentDll^.MD5[j], 2);
    Path := ShardSetup.UOExtBasePath + 'Plugins\' + Path + '.cache';
    if not DirectoryExists(ShardSetup.UOExtBasePath + 'Plugins\') then CreateDirectoryA(PAnsiChar(ShardSetup.UOExtBasePath + 'Plugins\'), nil);
    AssignFile(F, String(Path));
    Rewrite(F, 1);
    BlockWrite(F, Pointer(Cardinal(Packet) + 4)^, PacketSize - 4);
    FreeMemory(Packet);
    SavedDllLength := SavedDllLength + PacketSize - 4;
    GUI.GUIUpdateProcess(cUpdate, 0, CurrentDll.Size, SavedDllLength);
    Repeat
      Packet := Self.UOExtGetPacket(PacketSize);
      if Packet = nil then Exit;
      if (PByteArray(Packet)^[0] <> 0) OR (PByteArray(Packet)^[1] <> 4) then Begin
        FreeMemory(Packet);
        CloseFile(F);
        Exit;
      End;
      BlockWrite(f, Pointer(Cardinal(Packet) + 2)^, PacketSize - 2);
      SavedDllLength := SavedDllLength + PacketSize - 2;
      FreeMemory(Packet);
      GUI.GUIUpdateProcess(cUpdate, 0, CurrentDll.Size, SavedDllLength);
    Until SavedDllLength >= CurrentDll^.Size;
    CloseFile(F);
  End;
  Result := True;
End;

function TUpdater.UOExtGetMissingDlls(var Dlls:Pointer; var Count: Byte): Boolean;
var
  i: Byte;
Begin
  Dlls := GetMemory(FDllServerInfoList^.Amount);
  If FDllServerInfoList^.Amount > 0 Then for i := 0 to FDllServerInfoList^.Amount - 1 do PByteArray(Dlls)^[i] := i;
  Count := FDllServerInfoList^.Amount;
  Result := True;
End;

function TUpdater.UOExtGetPacket(var Size: Cardinal): Pointer;
var
  WriteOffSet: Cardinal;
  recvResult: Integer;
  minSize: Byte;
Begin
  Result := nil;
  if FSocket = 0 then Exit;
  WriteOffSet := 0;
  Size := 0;
  minSize := 2;
  If( not ShardSetup.UsingUpdateServer) Then minSize := minSize + 1;
  Result := GetMemory(minSize);
  Repeat
    recvResult := recv(FSocket, Pointer(Cardinal(Result) + WriteOffSet)^, minSize - WriteOffSet, 0);
    If recvResult <= 0 then Begin
      closesocket(FSocket);
      FSocket := 0;
      FreeMemory(Result);
      Result := nil;
      {$IFDEF DEBUG}
      WriteLn('Plugins: Server closed connection unexpectedly.');
      {$ENDIF}
      Exit;
    End;
    WriteOffSet := WriteOffSet + Cardinal(recvResult);
  Until (WriteOffSet >= minSize);

  If (PByte(Result)^ <> ShardSetup.InternalProtocolHeader)AND (not ShardSetup.UsingUpdateServer) then Begin
    {$IFDEF DEBUG}
    WriteLn('Plugins: UOExt proto failed. Incomming packet was not UOExt.');
    {$ENDIF}
    Halt(1);
  End;

  if ShardSetup.UsingUpdateServer then
    Size := PWord(Result)^ - 2
  Else
    Size := htons(PWord(Cardinal(Result) + 1)^) - 3;
  FreeMemory(Result);
  Result := GetMemory(Size);
  WriteOffSet := 0;

  Repeat
    recvResult := recv(FSocket, Pointer(Cardinal(Result) + WriteOffSet)^, Size - WriteOffSet, 0);
    If recvResult <= 0 then Begin
      closesocket(FSocket);
      FSocket := 0;
      FreeMemory(Result);
      Result := nil;
      Exit;
    End;
    WriteOffSet := WriteOffSet + Cardinal(recvResult);
  Until (WriteOffSet >= Size);
End;

procedure TUpdater.Cleanup;
Begin
  if Assigned(FDllServerInfoList) then FreeMemory(FDllServerInfoList);
  if Assigned(FPluginInitInfoList) then FreeMemory(FPluginInitInfoList);
End;


Function TUpdater.UOExtPacket(Data:Pointer; Size:Cardinal): Boolean;
type
  TUOExtHeader=packed record
    UOHeader: Byte;
    Size: Word;
  end;
var
  Header: TUOExtHeader;
Begin
  Result := False;
  if Size + 3 > MAXWORD then Exit;

  Header.UOHeader := ShardSetup.InternalProtocolHeader;
  Header.Size := Size + 2;
  if not ShardSetup.UsingUpdateServer then Header.Size := Header.Size + 1;
  Header.Size := htons(Header.Size);


  if FSocket <> 0 then Begin
    if (ShardSetup.UsingUpdateServer) then Begin
      send(FSocket, Pointer(Cardinal(@Header) + 1)^, SizeOf(Header) - 1, 0);
    End Else Begin
      send(FSocket, Header, SizeOf(Header), 0);
    End;
    send(FSocket, Data^, Size, 0);
  End;
End;

function TUpdater.UOExtPacket(Header:Byte; Data:Pointer; Size:Cardinal): Boolean;
type
  TUOExtHeader=packed record
    UOHeader: Byte;
    Size: Word;
  end;
var
  PreHeader: TUOExtHeader;
Begin
  Result := False;
  Size := Size + 1;
  if Size + 3 > MAXWORD then Exit;

  PreHeader.UOHeader := ShardSetup.InternalProtocolHeader;
  PreHeader.Size := Size + 2;
  if not ShardSetup.UsingUpdateServer then PreHeader.Size := PreHeader.Size + 1;
  PreHeader.Size := htons(PreHeader.Size);


  if FSocket <> 0 then Begin
    if (ShardSetup.UsingUpdateServer) then Begin
      send(FSocket, Pointer(Cardinal(@PreHeader) + 1)^, SizeOf(Header) - 1, 0);
    End Else Begin
      send(FSocket, Header, SizeOf(Header), 0);
    End;
    send(FSocket, Header, 1, 0);
    send(FSocket, Data^, Size, 0);
  End;
End;
function TUpdater.UOExtTryGetPacket(var Size:Cardinal; Timeout: Cardinal): Pointer;
Type
  THeader = packed record
    UOHeader: Byte;
    Size: Word;
  end;
Var
  fd: TFDSet;
  time:TTimeVal;
  recvLength, recived: Integer;
  headerSize: Byte;
  header: THeader;
  iPending: Integer;
  PacketSize: Word;

  startTime, deltaTime: Cardinal;

  pBuffer: Pointer;
Begin
  Result := nil;
  if FSocket = 0 then Exit;

  recvLength := 0;
  headerSize := 2;
  startTime := GetTickCount;
  If not ShardSetup.UsingUpdateServer then headerSize := headerSize + 1;
  repeat
    FD_ZERO(fd);
    FD_SET(FSocket, fd);
    deltaTime := GetTickCount() - startTime;
    time.tv_sec := Timeout - deltaTime DIV 1000;
    time.tv_usec := deltaTime MOD 1000;
    select(0, @fd, nil, @fd, @time);
    If not FD_ISSET(FSocket, fd) then Exit;

    ioctlsocket(FSocket, FIONREAD, iPending);
    If(iPending > (headerSize - recvLength)) Then iPending := headerSize - recvLength;

    recived := recv(FSocket, Pointer(Cardinal(@header) +  3 - headerSize)^, iPending, 0);
    if recived <= 0 then Exit;

    recvLength := recvLength + recived;
  until recvLength >= headerSize;
  PacketSize := htons(header.Size);
  PacketSize := PacketSize - 2;
  if not ShardSetup.UsingUpdateServer then PacketSize := PacketSize - 1;
  pBuffer := GetMemory(PacketSize);

  recvLength := 0;
  repeat
    FD_ZERO(fd);
    FD_SET(FSocket, fd);
    deltaTime := GetTickCount() - startTime;
    time.tv_sec := Timeout - deltaTime DIV 1000;
    time.tv_usec := deltaTime MOD 1000;
    select(0, @fd, nil, @fd, @time);
    If not FD_ISSET(FSocket, fd) then Exit;

    ioctlsocket(FSocket, FIONREAD, iPending);
    If(iPending > (PacketSize - recvLength)) Then iPending := PacketSize - recvLength;

    recived := recv(FSocket, Pointer(Cardinal(pBuffer) + Cardinal(recvLength))^, iPending, 0);
    if recived <= 0 then Exit;

    recvLength := recvLength + recived;
  until recvLength >= PacketSize;

  Result := pBuffer;
End;

function TUpdater.UOExtCanRead: Boolean;
var
  iPending: Integer;
Begin
  Result := False;
  if FSocket = 0 then Exit;
  ioctlsocket(FSocket, FIONREAD, iPending);
  Result := iPending > 0;
End;


end.
