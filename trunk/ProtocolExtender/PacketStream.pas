unit PacketStream;

interface

uses Windows, WinSock, ProtocolDescription, HuffmanAlgo;

type
  TPacketEvent=procedure(Sender:TObject; Packet:Pointer; var Length:Cardinal; var Process:Boolean) of object;

  TPacketStream=class
  protected
    FOnPacketEvent:TPacketEvent;

    FIncommingBuffer:Pointer;
    FIncommingBufferLength:Cardinal;
    FIncommingBufferCount:Cardinal;
    FIncommingSocket:TSocket;
    FIncommingLock:TRTLCriticalSection;

    FOutcommingBuffer:Pointer;
    FOutcommingBufferLength:Cardinal;
    FOutcommingBufferCount:Cardinal;
    FOutcommingSocket:TSocket;
    FOutcommingLock:TRTLCriticalSection;

    FSeed:Cardinal;
    FCompression:Boolean;
//    FDescriptor:TProtocolDescription;
    FIsCliServ:Boolean;

    {$IFDEF Debug}
    FProcessingBuffer:Pointer;
    FProcessingBufferLength:Cardinal;
    FProcessingBufferCount:Cardinal;

    FDebugPresent:String;
    {$ENDIF}

    function GetNetworkData:Boolean;
    procedure ProcessIncommingData; virtual;
  public
    {$IFDEF Debug}
    property DebugPresend:String read FDebugPresent write FDebugPresent;
    {$ENDIF}
    property IncommingSocket:TSocket read FIncommingSocket;
    property OutcommingSocket:TSocket read FOutcommingSocket;
    property Seed:Cardinal read FSeed write FSeed;
    property Compression:Boolean read FCompression write FCompression;
    property IsCliServ:Boolean read FIsCliServ write FIsCliServ;
    property OnPacket:TPacketEvent read FOnPacketEvent write FOnPacketEvent;
    constructor Create(Incomming:TSocket; Outcomming:TSocket{; Descriptor:TProtocolDescription});
    destructor Destroy; override;
    function ProcessNetworkData:Boolean; virtual;
    function DoSendPacket(Data:Pointer; var Length: Cardinal; Direct, Validate: Boolean):Boolean;
    procedure EnQueueOutcommingPacket(Packet:Pointer; Length:Cardinal); virtual;
    procedure Flush; virtual;
  end;

  {$IFDEF Debug}
  procedure WriteDump(Point:Pointer; Len:Cardinal);
  {$ENDIF}

implementation

//uses SysUtils;
uses Common;

var
  TV_Timeout:timeval;

// Local procedures

{$IFDEF DEBUG}
procedure WriteDump(Point:Pointer; Len:Cardinal);
var
  cLine:String;
  cPos:Cardinal;
begin
  If Len>0 Then Begin
{
000000 | 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00  |
}
    cPos:=0;
    repeat
      if cPos mod 16 = 0 Then Begin
//        if cPos<>0 Then WriteLn(cLine);
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
//    WriteLn(cLine);
  End;
end;
{$ENDIF}


// TPacketStream

constructor TPacketStream.Create(Incomming:TSocket; Outcomming:TSocket{; Descriptor:TProtocolDescription});
const
  START_BUFFER_SIZE = 65536;
begin
  Inherited Create;
  FOutcommingBuffer:=GetMemory(START_BUFFER_SIZE);
  FOutcommingBufferLength:=START_BUFFER_SIZE;
  FOutcommingBufferCount:=0;
  InitializeCriticalSection(FOutcommingLock);
  FIncommingBuffer:=GetMemory(START_BUFFER_SIZE);
  FIncommingBufferLength:=START_BUFFER_SIZE;
  FIncommingBufferCount:=0;
  InitializeCriticalSection(FIncommingLock);
  FIncommingSocket:=Incomming;
  FOutcommingSocket:=Outcomming;
//  FDescriptor:=Descriptor;
  FSeed:=0;
  FCompression:=False;
(*  {$IFDEF DEBUG}
  FProcessingBuffer:=GetMemory(START_BUFFER_SIZE);
  FProcessingBufferLength:=START_BUFFER_SIZE;
  FProcessingBufferCount:=0;
  {$ENDIF}*)
end;

destructor TPacketStream.Destroy;
begin
  FreeMemory(FOutcommingBuffer);
  DeleteCriticalSection(FOutcommingLock);
  FreeMemory(FIncommingBuffer);
  DeleteCriticalSection(FIncommingLock);
(*  {$IFDEF DEBUG}
  FreeMemory(FProcessingBuffer);
  {$ENDIF}*)
  Inherited Destroy;
end;

procedure TPacketStream.EnQueueOutcommingPacket(Packet:Pointer; Length:Cardinal);
const
  EncodedLength:Cardinal=65536;
var
  Pnt:Pointer;
  Encoded:Array [0..65535] of Char;
  EncodedLen:Cardinal;
(*  {$IFDEF Debug}
  Decoded: Array [0..65535] of Char;
  DecodedLen: Cardinal;
  {$ENDIF}*)
begin
  If FCompression Then Begin
    EncodedLen:=EncodedLength;
    If not Huffman.compress(@Encoded[0], EncodedLen, Packet, Length) Then Begin
      Halt;
    End;
(*    {$IFDEF Debug}
    DecodedLen:=EncodedLength;
    Huffman.decompress(@Decoded[0], DecodedLen, @Encoded[0], EncodedLen);
    If Chr(PByte(Packet)^) <> Decoded[0] Then
      Halt;
    If (Length<>FProcessingBufferCount) Then Begin
      Writeln(FDebugPresent, 'Decompression - Compression create difference.');
      WriteLn('Original packet: Length: ', FProcessingBufferCount);
      WriteDump(FProcessingBuffer, FProcessingBufferCount);
      WriteLn('Recompressed packet: Length: ', EncodedLen);
      WriteDump(Packet, Length);
    End;
    FProcessingBufferCount:=0;
    {$ENDIF}*)
    Packet:=@Encoded;
    Length:=EncodedLen;
  End;
//  EnterCriticalSection(FOutcommingLock);
  If (FOutcommingBufferLength - FOutcommingBufferCount) < Length Then Begin
    Pnt:=GetMemory(FOutcommingBufferLength + Length * 2);
    ZeroMemory(Pnt, FOutcommingBufferLength + Length * 2);
    If FOutcommingBufferCount>0 Then CopyMemory(Pnt, FOutcommingBuffer, FOutcommingBufferCount);
    FreeMemory(FOutcommingBuffer);
    FOutcommingBuffer:=Pnt;
    FOutcommingBufferLength:=FOutcommingBufferLength + Length * 2;
  End;
  CopyMemory(Pointer(Cardinal(FOutcommingBuffer) + FOutcommingBufferCount), Packet, Length);
  FOutcommingBufferCount:=FOutcommingBufferCount + Length;
//  LeaveCriticalSection(FOutcommingLock);
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
  If FOutcommingBufferCount>0 Then Begin
    CurrentPoint := FOutcommingBuffer;
    CurrentLength := FOutcommingBufferCount;
    repeat
      repeat
        FD_ZERO(fs);
        FD_SET(FOutcommingSocket, fs);
        select(0, nil, @fs, nil, @TV_Timeout);
      until FD_ISSET(FOutcommingSocket, fs);
      Sended := send(FOutcommingSocket, CurrentPoint^, CurrentLength, 0);
      If Sended = SOCKET_ERROR Then
        Exit;
      CurrentPoint := Pointer(Cardinal(CurrentPoint) + Sended);
      CurrentLength := CurrentLength - Sended;
    until CurrentLength = 0;
  End;
  FOutcommingBufferCount:=0;
end;

function TPacketStream.GetNetworkData:Boolean;
var
  Readed:Integer;
  {$IFDEF Debug}
  LastCount:Cardinal;
  {$ENDIF}
  fs : TFDSet;
begin
  Result:=False;
  {$IFDEF Debug}
  LastCount:=FIncommingBufferCount;
  {$ENDIF}
  repeat
    FD_ZERO(fs);
    FD_SET(FIncommingSocket, fs);
    select(0, @fs, nil, nil, @TV_Timeout);
    If FD_ISSET(FIncommingSocket, fs) Then Begin
      Readed:=recv(FIncommingSocket, Pointer(Cardinal(FIncommingBuffer) + FIncommingBufferCount)^, FIncommingBufferLength-FIncommingBufferCount, 0);
      If (Readed = SOCKET_ERROR) or (Readed = 0) Then Exit;
      FIncommingBufferCount:=FIncommingBufferCount+Readed;
    end;
  until not FD_ISSET(FIncommingSocket, fs);
  Result := True;
  {$IFDEF Debug}
{    If FIsCliServ Then Begin
      WriteLn('Cli -> Srv: Packet recived. Length: ', FIncommingBufferCount - LastCount);
    End Else Begin
      WriteLn('Srv -> Cli: Packet recived. Length: ', FIncommingBufferCount - LastCount);
    End;
    WriteDump(Pointer(Cardinal(FIncommingBuffer) + LastCount), FIncommingBufferCount - LastCount);}
  {$ENDIF}
end;

function TPacketStream.ProcessNetworkData:Boolean;
begin
  Result:=GetNetworkData();
  If Result Then ProcessIncommingData;
end;

procedure TPacketStream.ProcessIncommingData;
const
  DecodedLength:Cardinal=65536;
var
  PacketLength{, ProcessedPacketLength}:Cardinal;
//  Process:Boolean;
  PCurrent:Pointer;
  PLength:Cardinal;
  PTemp:Pointer;

  Decoded:Array [0..70000] of Char;
  DecodedLen:Cardinal;
  SourceLen:Cardinal;
begin
  If FIsCliServ AND (FSeed=0) Then Begin
    If PByte(FIncommingBuffer)^=239 Then Begin // New 0xEF packet for seeding.
      If FIncommingBufferCount<5 Then Exit;
      FSeed:=PCardinal(Cardinal(FIncommingBuffer) + 1)^;
    End Else Begin
//      WriteLn('Seed not found, Inc. buffer contains: ', FIncommingBufferCount, ' bytes');
      If FIncommingBufferCount<4 Then Exit;
      FSeed:=PCardinal(FIncommingBuffer)^;
      PCardinal(Cardinal(FOutcommingBuffer) + FOutcommingBufferCount)^:=FSeed;
      FOutcommingBufferCount:=FOutcommingBufferCount+4;
      If FIncommingBufferCount=4 Then Begin
//        WriteLn('Seed get. Packet processed. Exit from PID.');
        FIncommingBufferCount:=0;
        Exit;
      end;
      PTemp:=GetMemory(FIncommingBufferCount - 4);
      CopyMemory(PTemp, Pointer(Cardinal(FIncommingBuffer) + 4), FIncommingBufferCount - 4);
      CopyMemory(FIncommingBuffer, PTemp, FIncommingBufferCount - 4);
      FreeMemory(PTemp);
      FIncommingBufferCount:=FIncommingBufferCount - 4;
    End;
  End;
  PCurrent:=FIncommingBuffer;
  PLength:=FIncommingBufferCount;
  repeat
    If not FCompression Then Begin
      If FIsCliServ Then
        PacketLength:=ProtocolDescriptor.GetCliServLength(PCurrent, PLength)
      Else
        PacketLength:=ProtocolDescriptor.GetServCliLength(PCurrent, PLength);
      If PacketLength=0 Then Begin
        {$IFDEF Debug}
        WriteLn(FDebugPresent, 'Uncompressed protocol: Some data lost in process.');
        {$ENDIF}
        Break;
      end;
      If PLength<PacketLength Then Begin
        {$IFDEF Debug}
        WriteLn(FDebugPresent, 'UO packet length more than current data buffer. Waiting next frame.');
        WriteLn(' Packet: ', PByte(PCurrent)^, ' CalcPacketsize: ', PacketLength, ' AvailData: ', PLength);
        {$ENDIF}
        Break;
      End;
      DoSendPacket(PCurrent, PacketLength, False, False);
      PCurrent:=Pointer(Cardinal(PCurrent) + PacketLength);
      PLength:=PLength - PacketLength;
    End Else Begin
      SourceLen:=PLength;
      DecodedLen:=DecodedLength;
      If Huffman.decompress(@Decoded, DecodedLen, PCurrent, SourceLen) Then Begin
(*        {$IFDEF Debug}
        CopyMemory(Pointer(Cardinal(FProcessingBuffer) + FProcessingBufferCount), PCurrent, SourceLen);
        FProcessingBufferCount:=FProcessingBufferCount + SourceLen;
        {$ENDIF}*)
        PCurrent:=Pointer(Cardinal(PCurrent) + SourceLen);
        PLength:=PLength - SourceLen;
        DoSendPacket(@Decoded, DecodedLen, False, False);
      End Else Begin
        {$IFDEF Debug}
(*        If PLength>SourceLen Then Begin
          WriteLn(FDebugPresent, 'Compressed protocol: Data not processed. 1st Byte: ', IntToHex(PByte(PCurrent)^, 2), ' Avail len: ', PLength);
          SourceLen:=PLength+1;
          DecodedLen:=DecodedLength;
          Huffman.decompress(@Decoded, DecodedLen, PCurrent, SourceLen)
        End;*)
        {$ENDIF}
        Break;
      End;
    End;
  until PLength<=0;
  If PLength<>FIncommingBufferCount Then Begin
    If PLength=0 Then Begin
      FIncommingBufferCount:=0;
    End Else Begin
      PTemp:=GetMemory(PLength);
      CopyMemory(PTemp, PCurrent, PLength);
      CopyMemory(FIncommingBuffer, PTemp, PLength);
      FreeMemory(PTemp);
      FIncommingBufferCount:=PLength;
    End;
  End;
end;

function TPacketStream.DoSendPacket(Data:Pointer; var Length: Cardinal; Direct, Validate: Boolean):Boolean;
var
  Process: Boolean;
  NewSize: Cardinal;
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
    If Process Then
      EnQueueOutcommingPacket(Data, NewSize);
  End Else Begin
    EnQueueOutcommingPacket(Data, Length);
  End;
end;

initialization
  TV_Timeout.tv_usec:=100;
end.
