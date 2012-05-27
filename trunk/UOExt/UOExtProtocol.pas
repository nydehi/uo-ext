unit UOExtProtocol;

interface
{
uses Windows, WinSock, Common, ShardSetup, PreConnectIPDiscover;

type
  TUOEPDllLoaded = function (Data: Pointer; Size: Cardinal; lParam: Pointer): Boolean;


  TUOExtProtocol=class
  private
    FSocket: TSocket;
    FUOExtSupported: Boolean;
    FUOExtProtocolHeader: Byte;
    FInternalProtocolEnabled: Boolean;

  public
    property UOExtSupported:Boolean read FUOExtSupported;
    property InternalProtocolEnabled: Boolean read FInternalProtocolEnabled;


    procedure SendPacket(PluginId: Byte; Header: Byte; Data:Pointer; Size: Cardinal);
    function RecvPacket(var Data: Pointer; Size: Cardinal): Boolean;
    function Connect: Boolean;

    procedure LoadDlls(Callback: TUOEPDllLoaded; lParam: Pointer);
  end;

function GetUOExtSupport():Boolean;

var
  UOExtProto: TUOExtProtocol;
}
implementation
(*
uses Plugins;

function GetUOExtSupport():Boolean;
Begin
  Result := False;
  UOExtProto := TUOExtProtocol.Create;
  If not UOExtProto.Connect Then Exit;
  if not UOExtProto.UOExtSupported then Begin
    UOExtProto.Free;
    UOExtProto := nil;
  End;

  Result := UOExtProto.UOExtSupported;
End;


function TUOExtProtocol.Connect;
const
  bufSize = 65535;
var
  IP: Cardinal;
  Port: Word;

  WSAData: TWSAData;
  SockAddr: TSockAddr;

  bufPacket: Array [0..bufSize] of Byte;
  WriteOffSet, PacketSize: Cardinal;
  recvResult: Integer;
  startGTC, currGTC: Cardinal;
  fdset: TFDSet;
  tv: TTimeVal;
begin
  Result := False;
  If not PreConnectIPDiscover.GetConnInfo(IP, Port) Then Exit;
  WSAStartup($101, WSAData);

  FSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  IF FSocket=INVALID_SOCKET Then Exit;
  ZeroMemory(@SockAddr, SizeOf(SockAddr));
  SockAddr.sin_family:=AF_INET;

  SockAddr.sin_port:=htons(Port);
  SockAddr.sin_addr.S_addr:=htonl(IP);

  If WinSock.connect(FSocket, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR Then Begin
    closesocket(FSocket);
    Exit;
  End;

  ZeroMemory(@bufPacket[0], 21);
  bufPacket[0] := $EF;
  If send(FSocket, bufPacket, 21, 0) <> 21 Then Begin
    closesocket(FSocket);
    Exit;
  End;

  startGTC := GetTickCount;
  WriteOffSet := 0;
  PacketSize := 0;
  Repeat
    FD_ZERO(fdset);
    FD_SET(FSocket, fdset);
    currGTC := GetTickCount;
    tv.tv_sec := 5 - (currGTC - startGTC) DIV 1000;
    tv.tv_usec := 5 - (currGTC - startGTC) MOD 1000;
    recvResult := select(0, @fdset, nil, nil, @tv);
    If FD_ISSET(FSocket, fdset) then Begin
      recvResult := recv(FSocket, Pointer(@bufPacket[WriteOffSet])^, bufSize - WriteOffSet, 0);
      If recvResult <= 0 then Begin
        closesocket(FSocket);
        Exit;
      End;

      WriteOffSet := WriteOffSet + Cardinal(recvResult);
      If PacketSize = 0 then Begin
        If WriteOffSet > 3 then
          if bufPacket[0] <> 0 then Begin
            closesocket(FSocket);
            Exit;
          End;
          PacketSize := htons(PWord(@bufPacket[1])^);
      End;
    End;
  Until ((WriteOffSet >= PacketSize)AND(PacketSize > 0))OR((GetTickCount - startGTC) DIV 1000 > 5);

  If(WriteOffSet < 3)OR(PacketSize < htons(PWord(@bufPacket[1])^)) Then Begin
    {$IFDEF DEBUG}
    WriteLn('UOExtProtocol: Server didn''t respond on UOExt support ask.');
    {$ENDIF}
    closesocket(FSocket);
    Exit;
  End;

  FUOExtSupported := True;
  FUOExtProtocolHeader := bufPacket[3];

  UpdateIP := PCardinal(@bufPacket[4])^;
  UpdatePort := PWord(@bufPacket[8])^;

  FInternalProtocolEnabled := bufPacket[10] AND $01 = $01;
  PersistentConnect := bufPacket[10] AND $02 = $02;

  Result := True;

End;

procedure TUOExtProtocol.SendPacket(Header: Byte; Data:Pointer; Size: Cardinal);
type
  TUOExtProtoHeader=packed record
    UOHeader: Byte;
    Size:Word;
    UOExtHeader: Byte;
  end;
var
  UOExtHeader:TUOExtProtoHeader;
Begin
  UOExtHeader.UOHeader := FUOExtProtocolHeader;
  UOExtHeader.Size := Size + 4;
  UOExtHeader.UOExtHeader := Header;
  send(FSocket, UOExtHeader, SizeOf(UOExtHeader), 0);
  send(FSocket, Data^, Size, 0);
End;

function TUOExtProtocol.RecvPacketSize: Cardinal;
type
  TUOExtProtoHeader=packed record
    UOHeader: Byte;
    Size:Word;
  end;
var
  UOExtHeader: TUOExtProtoHeader;
Begin
  recv(FSocket, UOExtHeader, SizeOf(UOExtHeader), 0);
  If(UOExtHeader.UOHeader <> ShardSetup.InternalProtocolHeader) Then Begin
  {$IFDEF DEBUG}
    WriteLn('UOExtProtocol: Server returned wrong UOPacket. Can''t continue!');
  {$ENDIF}
    Halt(1);
  End;
  Result := UOExtHeader.Size;
End;

function TUOExtProtocol.RecvPacket(var Data: Pointer; Size: Cardinal): Cardinal;
Begin
  Result := recv(FSocket, Data^, Size, 0);
End;

procedure TUOExtProtocol.LoadDlls(Callback: TUOEPDllLoaded; lParam: Pointer);
type
  TDllInfo = packed record
    CRC32: Cardinal;
    MD5: Array [0..15] of Byte;
    Size: Cardinal;
  end;
  TDllInfoList = Array [0..0] of TDllInfo;
  PDllInfoList = ^TDllInfoList;
var
  UOSize: Word;
  UOEHeader: Byte;
  Packet: Pointer;

  DllInfoList: PDllInfoList;
  DllInfoListAmount: Byte;

  i: Cardinal;
  DllAmount: Byte;
Begin
  // Sending Get Packet
  // BYTE UOHeader
  // WORD Size
  // BYTE GetHeader = 0x00
  send(FSocket, ShardSetup.InternalProtocolHeader, 1, 0);
  UOSize := htons(4);
  send(FSocket, UOSize, 2, 0);
  UOEHeader := 0;
  send(FSocket, UOEHeader, 1, 0);
  UOSize := Self.RecvPacketSize;
  Packet := GetMemory(UOSize);
  Self.RecvPacket(Packet, UOSize);

  // Recived packet is
  // BYTE ListHeader = 0x00
  // LOOP (Size DIV 24) TIMES
  //   CARD CRC32
  //   BYTE(16) MD5
  //   CARD Size
  // END LOOP
  DllInfoList := GetMemory(UOSize - 1);
  CopyMemory(DllInfoList, Packet, UOSize - 1);
  FreeMemory(Packet);
  DllInfoListAmount := (UOSize - 1) DIV 24;

  // Sending packet "We need all"
  // BYTE UOHeader
  // WORD Size
  // LOOP
  //   BYTE Id
  // EndLoop
  Packet := GetMemory(DllInfoListAmount + 3);
  PByte(Packet)^ := ShardSetup.InternalProtocolHeader;
  PWord(Cardinal(Packet) + 1)^ := DllInfoListAmount;
  for i := 0 to DllInfoListAmount -1 do PByteArray(Cardinal(Packet) + 3)^[i] := i;
  send(FSocket, Packet^, DllInfoListAmount + 3, 0);
  FreeMemory(Packet);

  // Recived packet is
  // BYTE DllAmountHeader = 0x01
  // BYTE DllAmount
  Packet := GetMemory(2);
  Self.RecvPacketSize;
  Self.RecvPacket(Packet, 2);
  DllAmount := PByte(Cardinal(Packet) + 1)^;
  for i := 0 to DllAmount - 1 do Begin

  End;




End;
*)
end.
