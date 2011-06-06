unit PacketStream;

interface

uses Windows, WinSock, ProtocolDescription, HuffmanAlgo, Encryption;

type
  TBuffer = class
  private
    FData: Pointer;
    FWritePoint: Pointer;
    FAmount: Cardinal;
    FCapacity: Cardinal;
  public
    property Amount: Cardinal read FAmount;
    property Capacity: Cardinal read FCapacity;
    property Base: Pointer read FData;
    property WritePoint: Pointer read FWritePoint;

    constructor Create; overload;
    constructor Create(AnAmount: Cardinal); overload;
    destructor Destroy; override;

    procedure Push(Data: Pointer; AnAmount: Cardinal); overload;
    procedure Push(Data: Pointer; Offset, AnAmount: Cardinal); overload;
    procedure Pushed(AnAmount: Cardinal);

    procedure Shift(AnAmount: Cardinal); overload;
    procedure Shift(ToWhere: Pointer; AnAmount: Cardinal); overload;

    procedure EnshureFreeSpace(AnAmount: Cardinal);
  end;

  TPacketEvent=procedure(Sender:TObject; Packet:Pointer; var Length:Cardinal; var Process:Boolean) of object;
  TPacketProcessedEvent=procedure(Sender:TObject; PacketHeader:Byte) of object;

  TCryptType = (ctNone, ctLogin, ctGame);
  TCryptPhase = (cpLogin, cpGame);
  TClientEncryptionDetectedEvent=procedure(Sender: TObject; CryptType: TCryptType; Phase: TCryptPhase) of object;

  TPacketStream=class
  protected
    FOnPacketEvent:TPacketEvent;
    FOnPacketProcessedEvent: TPacketProcessedEvent;

    FIncommingBuffer:TBuffer;
    FIncommingSocket:TSocket;

    FOutcommingBuffer:TBuffer;
    FOutcommingSocket:TSocket;

    FSeed:Cardinal;
    FCompression:Boolean;
    FIsCliServ:Boolean;

    {$IFDEF Debug}
    FDebugPresent:AnsiString;
    {$ENDIF}

    FCryptObject: TNoEncryption;
    FOnClientEncryptionDetected: TClientEncryptionDetectedEvent;

    function GetNetworkData:Boolean;
    function GetSeed:Boolean;
    procedure ProcessIncommingData; virtual;
    function DetectEncryption:Boolean;

    procedure SetCryptObject(Value: TNoEncryption);
  public
    {$IFDEF Debug}
    property DebugPresend:AnsiString read FDebugPresent write FDebugPresent;
    {$ENDIF}
    property IncommingSocket:TSocket read FIncommingSocket;
    property OutcommingSocket:TSocket read FOutcommingSocket;
    property Seed:Cardinal read FSeed write FSeed;
    property Compression:Boolean read FCompression write FCompression;
    property IsCliServ:Boolean read FIsCliServ write FIsCliServ;
    property OnClientEncryptionDetected: TClientEncryptionDetectedEvent read FOnClientEncryptionDetected write FOnClientEncryptionDetected;
    property CryptObject: TNoEncryption read FCryptObject write SetCryptObject;
    property OnPacket:TPacketEvent read FOnPacketEvent write FOnPacketEvent;
    property OnPacketProcessed: TPacketProcessedEvent read FOnPacketProcessedEvent write FOnPacketProcessedEvent;

    constructor Create(Incomming:TSocket; Outcomming:TSocket);
    destructor Destroy; override;

    function ProcessNetworkData:Boolean; virtual;
    function DoSendPacket(Data:Pointer; var Length: Cardinal; Direct, Validate: Boolean):Boolean;
    procedure EnQueueOutcommingPacket(Packet:Pointer; Length:Cardinal); virtual;
    procedure Flush; virtual;
  end;

  {$IFDEF WRITELOG}
  procedure WriteDump(Point:Pointer; Len:Cardinal);
  {$ENDIF}

implementation

uses Common, ShardSetup;

var
  TV_Timeout:timeval;

// Local procedures

{$IFDEF WRITELOG}
procedure WriteDump(Point:Pointer; Len:Cardinal);
var
  cLine:AnsiString;
  cPos:Cardinal;
begin
  If Len>0 Then Begin
{
000000 | 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00  |
}
    cPos:=0;
    repeat
      if cPos mod 16 = 0 Then Begin
        if cPos<>0 Then WriteLn(cLine);
        cLine:=IntToStr(cPos);
        if cPos<100000 Then cLine:='0'+cLine;
        if cPos<10000 Then cLine:='0'+cLine;
        if cPos<1000 Then cLine:='0'+cLine;
        if cPos<100 Then cLine:='0'+cLine;
        if cPos<10 Then cLine:='0'+cLine;
        cLine:=cLine + ' | ';
      End;
      cLine:=cLine+IntToHex(PByte(Cardinal(Point)+cPos)^ ,2) + ' ';
      If (cPos + 1) mod 4 = 0 Then cLine:=cLine + ' ';
      cPos:=cPos+1;
    until cPos>=Len;
    WriteLn(cLine);
  End;
end;
{$ENDIF}

//  TBuffer
constructor TBuffer.Create;
Begin
  Inherited;

  FCapacity := 0;
  FData := nil;
  FWritePoint := nil;
End;

constructor TBuffer.Create(AnAmount: Cardinal);
begin
  Inherited Create;

  FCapacity := AnAmount;
  FData := GetMemory(FCapacity);
  FWritePoint := FData;
end;

destructor TBuffer.Destroy;
Begin
  If FData <> nil Then FreeMemory(FData);
  Inherited;
End;

procedure TBuffer.EnshureFreeSpace(AnAmount: Cardinal);
var
  pNewData: Pointer;
Begin
  If AnAmount > (FCapacity - FAmount) Then Begin
    pNewData := GetMemory(FCapacity + AnAmount);
    CopyMemory(pNewData, FData, FAmount);
    FWritePoint := Pointer( Cardinal(pNewData) + FAmount);
    FCapacity := FCapacity + AnAmount;
    FreeMemory(FData);
    FData := pNewData;
  End;
End;

procedure TBuffer.Push(Data: Pointer; AnAmount: Cardinal);
Begin
  Push(Data, 0, AnAmount);
End;

procedure TBuffer.Push(Data: Pointer; Offset, AnAmount: Cardinal);
Begin
  EnshureFreeSpace(AnAmount);
  CopyMemory(FWritePoint, Pointer( Cardinal(Data) + Offset), AnAmount);
  FWritePoint := Pointer( Cardinal(FWritePoint) + AnAmount);
  FAmount := FAmount + AnAmount;
End;

procedure TBuffer.Pushed(AnAmount: Cardinal);
Begin
  FAmount := FAmount + AnAmount;
  FWritePoint := Pointer(Cardinal(FWritePoint) + AnAmount);
End;

procedure TBuffer.Shift(AnAmount: Cardinal);
var
  pTemp: Pointer;
Begin
  If FAmount > AnAmount Then Begin
    pTemp := GetMemory(FAmount - AnAmount);
    CopyMemory(pTemp, Pointer( Cardinal(FData) + AnAmount), FAmount - AnAmount);
    CopyMemory(FData, pTemp, FAmount - AnAmount);
    FreeMemory(pTemp);
    FAmount := FAmount - AnAmount;
    FWritePoint := Pointer( Cardinal(FData) + FAmount);
  End Else Begin
    {$IFDEF Debug}
    if AnAmount > FAmount then WriteLn('Buffer: Shift(', AnAmount, ') called. While there is only ', FAmount, ' left.');
    {$ENDIF}
    FAmount := 0;
    FWritePoint := FData;
  End;
End;

procedure TBuffer.Shift(ToWhere: Pointer; AnAmount: Cardinal);
Begin
  CopyMemory(ToWhere, FData, AnAmount);
  Shift(AnAmount);
End;

// TPacketStream

constructor TPacketStream.Create(Incomming:TSocket; Outcomming:TSocket{; Descriptor:TProtocolDescription});
const
  START_BUFFER_SIZE = 65536;
begin
  Inherited Create;
  FIncommingBuffer := TBuffer.Create(START_BUFFER_SIZE);
  FOutcommingBuffer := TBuffer.Create(START_BUFFER_SIZE);

  FIncommingSocket := Incomming;
  FOutcommingSocket := Outcomming;

  FSeed:=0;
  FCompression:=False;

  FCryptObject := nil;
end;

destructor TPacketStream.Destroy;
begin
  FIncommingBuffer.Free;
  FOutcommingBuffer.Free;
  If Assigned(FCryptObject) Then FCryptObject.Free;
  Inherited Destroy;
end;

procedure TPacketStream.EnQueueOutcommingPacket(Packet:Pointer; Length:Cardinal);
const
  EncodedLength:Cardinal=65536;
var
  Encoded:Array [0..65535] of AnsiChar;
  EncodedLen:Cardinal;
  pOldWork: Pointer;
begin
  If FCompression Then Begin
    EncodedLen:=EncodedLength;
    If not Huffman.compress(@Encoded[0], EncodedLen, Packet, Length) Then Begin
      {$IFDEF Debug}
      WriteLn('Core: EnQueue: compress failed.');
      ReadLn;
      {$ENDIF}
      Halt;
    End;
    Packet:=@Encoded;
    Length:=EncodedLen;
  End;
  pOldWork := FOutcommingBuffer.WritePoint;
  FOutcommingBuffer.Push(Packet, Length);
  If Assigned(FCryptObject) Then FCryptObject.Encrypt(pOldWork, 0, Length);
  {$IFDEF Debug}
  Flush;
  {$ENDIF}
end;

procedure TPacketStream.Flush;
var
  fs: TFDSet;
  Sended: Integer;
  CurrentPoint: Pointer;
  CurrentLength: Cardinal;
begin
  If FOutcommingBuffer.Amount > 0 Then Begin
    CurrentPoint := FOutcommingBuffer.Base;
    CurrentLength := FOutcommingBuffer.Amount;
    repeat
      repeat
        FD_ZERO(fs);
        FD_SET(FOutcommingSocket, fs);
        select(0, nil, @fs, nil, @TV_Timeout);
      until FD_ISSET(FOutcommingSocket, fs);
      Sended := send(FOutcommingSocket, CurrentPoint^, CurrentLength, 0);
      If Sended = SOCKET_ERROR Then Begin
        {$IFDEF Debug}
          WriteLn('Core: PacketStream: Send returned SOCK_ERR. WSAGLE = ', WSAGetLastError);
          Readln;
        {$ENDIF}
        FOutcommingBuffer.Shift(FOutcommingBuffer.Amount - CurrentLength);
        Break;
      End;

      CurrentPoint := Pointer(Cardinal(CurrentPoint) + Sended);
      CurrentLength := CurrentLength - Sended;
    until CurrentLength = 0;
    FOutcommingBuffer.Shift(FOutcommingBuffer.Amount);
  End;
end;

function TPacketStream.GetNetworkData:Boolean;
var
  Readed:Integer;
begin
  Result:=False;

  ioctlsocket(FIncommingSocket, FIONREAD, Readed);
  FIncommingBuffer.EnshureFreeSpace(Readed);
  Readed := recv(FIncommingSocket, FIncommingBuffer.WritePoint^, Readed, 0);
  If (Readed = SOCKET_ERROR) or (Readed = 0) Then Exit;

  {$IFDEF WRITELOG}
  If FIsCliServ Then Begin
    WriteLn('Cli -> Srv: Packet recived. Length: ', Readed);
  End Else Begin
    WriteLn('Srv -> Cli: Packet recived. Length: ', Readed);
  End;
  WriteDump(FIncommingBuffer.WritePoint, Readed);
  {$ENDIF}
  If Assigned(FCryptObject) Then FCryptObject.Decrypt(FIncommingBuffer.WritePoint, 0, Readed);
  FIncommingBuffer.Pushed(Readed);
  Result := True;
end;

function TPacketStream.ProcessNetworkData:Boolean;
begin
  Result:=GetNetworkData();
  If Result Then ProcessIncommingData;
end;

function TPacketStream.GetSeed:Boolean;
var
  Dummy: Cardinal;
Begin
  Result := False;
  If not((FSeed <> 0) or (not FIsCliServ)) Then Begin
    If PByte(FIncommingBuffer.Base)^ = $EF Then Begin // New 0xEF packet for seeding.
      Dummy := ProtocolDescriptor.GetCliServLength($EF);
      If FIncommingBuffer.Amount < Dummy Then Exit; // Wait for additional data
      FSeed := PCardinal(Cardinal(FIncommingBuffer.Base) + 1)^;
      DoSendPacket(FIncommingBuffer.Base, Dummy, False, False);
      FIncommingBuffer.Shift(Dummy);
      // This packet sends without encoding and/or packing.
    End Else Begin
      FSeed := PCardinal(FIncommingBuffer.Base)^;
      FIncommingBuffer.Shift(4);
      FOutcommingBuffer.Push(@FSeed, 4);
    End;
  End;
  Result := True;
End;

function TPacketStream.DetectEncryption:Boolean;
var
  Buffer: PByteArray;
Begin
  Result := False;
  If Assigned(FCryptObject) Then Exit;
  Buffer := PByteArray(FIncommingBuffer.Base);
  If IsCliServ Then Begin
    If FIncommingBuffer.Amount = 62 Then Begin // Login phase
      If (Buffer^[00] = $80) and
        (Buffer^[30] = $00) and
        (Buffer^[60] = $00) Then Begin // no login encryption
        if Assigned(FOnClientEncryptionDetected) Then FOnClientEncryptionDetected(Self, ctNone, cpLogin);
        {$IFDEF Debug}
        WriteLn('No login encryption detected.');
        {$ENDIF}
      End Else Begin
        if Assigned(FOnClientEncryptionDetected) Then FOnClientEncryptionDetected(Self, ctLogin, cpLogin);
        {$IFDEF Debug}
        WriteLn('Login encryption detected.');
        {$ENDIF}
      End;
      Result := True;
    End;
    If FIncommingBuffer.Amount >= 65 Then Begin // Game phase
      If (Buffer^[00] = $91) and
         (Buffer^[04] = ((FSeed shr 24) and $FF)) and
         (Buffer^[03] = ((FSeed shr 16) and $FF)) and
         (Buffer^[02] = ((FSeed shr 8) and $FF)) and
         (Buffer^[01] = (FSeed and $FF)) Then Begin // no encryption
        if Assigned(FOnClientEncryptionDetected) Then FOnClientEncryptionDetected(Self, ctNone, cpGame);
        {$IFDEF Debug}
        WriteLn('No encryption detected.');
        {$ENDIF}
        Result := True;
      End Else Begin
        if Assigned(FOnClientEncryptionDetected) Then FOnClientEncryptionDetected(Self, ctGame, cpGame);
        {$IFDEF Debug}
        WriteLn('Game encryption detected.');
        {$ENDIF}
        Result := True;
      End;
    End;
  End Else Begin
    Result := True;
    {$IFDEF Debug}
    WriteLn('You can''t see this.');
    {$ENDIF}
  End;
End;

procedure TPacketStream.ProcessIncommingData;
const
  DecodedLength:Cardinal=70001;
var
  PacketLength:Cardinal;

  Decoded:Array [0..70000] of AnsiChar;
  DecodedLen:Cardinal;
  SourceLen:Cardinal;
begin
  If not GetSeed Then Exit;
  If not Assigned(FCryptObject) Then If not DetectEncryption Then Exit;

  If FIncommingBuffer.Amount > 0 Then repeat
    If not FCompression Then Begin
      If FIsCliServ Then
        PacketLength:=ProtocolDescriptor.GetCliServLength(FIncommingBuffer.Base, FIncommingBuffer.Amount)
      Else
        PacketLength:=ProtocolDescriptor.GetServCliLength(FIncommingBuffer.Base, FIncommingBuffer.Amount);
      If PacketLength=0 Then Begin
        {$IFDEF Debug}
        WriteLn(FDebugPresent, 'Uncompressed protocol: Some data lost in process.');
        {$ENDIF}
        Break;
      end;
      If PacketLength > FIncommingBuffer.Amount Then Begin
        {$IFDEF Debug}
        WriteLn(FDebugPresent, 'UO packet length more than current data buffer. Waiting next frame.');
        WriteLn(' Packet: ', PByte(FIncommingBuffer.Base)^, ' CalcPacketsize: ', PacketLength, ' AvailData: ', FIncommingBuffer.Amount);
        {$ENDIF}
        Break;
      End;
      DoSendPacket(FIncommingBuffer.Base, PacketLength, False, False);
      FIncommingBuffer.Shift(PacketLength);
    End Else Begin
      SourceLen:=FIncommingBuffer.Amount;
      DecodedLen:=DecodedLength;
      If Huffman.decompress(@Decoded, DecodedLen, FIncommingBuffer.Base, SourceLen) Then Begin
        DoSendPacket(@Decoded, DecodedLen, False, False);
        FIncommingBuffer.Shift(SourceLen);
      End Else Begin
        {$IFDEF Debug}
        WriteLn(FDebugPresent, 'Compressed protocol: Can''t decompress data in buffer. Size := ', SourceLen);
        {$ENDIF}
        Break;
      End;
    End;
  until FIncommingBuffer.Amount <= 0;
end;

function TPacketStream.DoSendPacket(Data:Pointer; var Length: Cardinal; Direct, Validate: Boolean):Boolean;
var
  Process: Boolean;
  NewSize: Cardinal;
  Head: Byte;
begin
  Result := not Validate;
  If Validate Then Begin
    If FIsCliServ Then
      NewSize := ProtocolDescriptor.GetCliServLength(Data, Length)
    Else
      NewSize := ProtocolDescriptor.GetServCliLength(Data, Length);
    Result := NewSize = Length;
    If not Result Then Begin
      Length := NewSize;
      Exit;
    End;
  End;
  If not Direct and Assigned(FOnPacketEvent) Then Begin
    Process := True;
    NewSize := Length;
    FOnPacketEvent(Self, Data, NewSize, Process);
    Head := PByte(Data)^;
    If Process Then Begin
      EnQueueOutcommingPacket(Data, NewSize);
      if Assigned(FOnPacketProcessedEvent) then FOnPacketProcessedEvent(Self, Head);
    End;
  End Else Begin
    EnQueueOutcommingPacket(Data, Length);
  End;
end;

procedure TPacketStream.SetCryptObject(Value: TNoEncryption);
Begin
  If Value <> FCryptObject Then Begin
    If not Assigned(FCryptObject) Then Value.Decrypt(FIncommingBuffer.Base, 0, FIncommingBuffer.Amount);
    FCryptObject := Value;
  End;
End;

initialization
  TV_Timeout.tv_usec:=100;
end.
