unit Packet;

interface

uses Windows;

type
  TPacketReader=class
  protected
    FBuffer: Pointer;
    FOffset: Cardinal;
    FLength: Cardinal;
    procedure SetOffset(Value: Cardinal);
  public
    property Offset: Cardinal read FOffset write SetOffset;
    procedure Read(Destination: Pointer; Length: Cardinal);
    function ReadByte:Byte;
    function ReadWord:Word;
    function ReadCardinal: Cardinal;
    function ReadInteger: Integer;
    function ReadAsciiFixed(Amount: Word): String;
    function ReadAsciiNull:String;
    constructor Create;
    destructor Destroy; override;
    procedure AssignToBuffer(Buffer: Pointer; Length: Cardinal);
    class function GetPacketReader: TPacketReader;
  end;

  TPacket=class
  private
    FNeedFree: Boolean;
  protected
    FBuffer: Pointer;
    FBufferLength: Cardinal;
    FBufferOffset: Cardinal;
    procedure ReadPacket; virtual; abstract;
    constructor Create(Head: Byte; Size: Word); overload;
  public
    constructor Create(Buffer: Pointer; Length: Cardinal; CopyData: Boolean); overload;
    destructor Destroy; override;
  end;

implementation

//uses SysUtils;

var
  PacketReader: TPacketReader;

// TPacketReader

constructor TPacketReader.Create;
begin
  Inherited;
  FBuffer := nil;
  FLength := 0;
  FOffset := 0;
end;

destructor TPacketReader.Destroy;
begin
  Inherited;
end;

procedure TPacketReader.SetOffset(Value: Cardinal);
begin
  If Value < FLength Then FOffset := Value;
End;

procedure TPacketReader.AssignToBuffer(Buffer: Pointer; Length: Cardinal);
begin
  FBuffer := Buffer;
  FOffset := 0;
  FLength := Length;
end;

procedure TPacketReader.Read(Destination: Pointer; Length: Cardinal);
begin
  If FLength < (FOffset + Length) Then Exit; //raise Exception.CreateFmt('Can''t read %d bytes from packet %d. There is only %d left ', [Length, PByte(FBuffer)^, FLength - FOffset]);
  CopyMemory(Destination, Pointer(Cardinal(FBuffer) + FOffset), Length);
  FOffset := FOffset + Length;
end;

function TPacketReader.ReadByte: Byte;
begin
  Read(@Result, 1);
end;

function TPacketReader.ReadWord: Word;
begin
  Read(@Result, 2);
  Result := (Result shr 8) or ((Result and $FF) shl 8);
end;

function TPacketReader.ReadCardinal: Cardinal;
begin
  Read(@Result, 4);
  Result := (Result shr 24) or (((Result shr 16) and $FF) shl 8) or (((Result shr 8) and $FF) shl 16) or ((Result and $FF) shl 24);
end;

function TPacketReader.ReadInteger: Integer;
var
  Data: Cardinal;
begin
  Read(@Data, 4);
  Data := (Data shr 24) or (((Data shr 16) and $FF) shl 8) or (((Data shr 8) and $FF) shl 16) or ((Data and $FF) shl 24);
  CopyMemory(@Result, @Data, 4);
end;

function TPacketReader.ReadAsciiFixed(Amount: Word): String;
begin
  SetLength(Result, Amount);
  Read(@Result[1], Amount);
end;

function TPacketReader.ReadAsciiNull: String;
var
  i, cStart: Cardinal;
begin
  cStart := (Cardinal(FBuffer) + FOffset);
  For i := cStart to (Cardinal(FBuffer) + FLength) do begin
    If PByte(i)^ = 0 Then Begin
      SetLength(Result, i - cStart);
      CopyMemory(@Result[1], Pointer(cStart), i - cStart);
      FOffset := i + 1;
      Exit;
    End;
  End;
//  raise Exception.Create('Can''t find #0 while reading AsiiNull string');
end;

class function TPacketReader.GetPacketReader: TPacketReader;
begin
  If not Assigned(PacketReader) Then PacketReader := TPacketReader.Create;
  Result := PacketReader;
end;

// TPacket

constructor TPacket.Create(Head: Byte; Size: Word);
begin
  Inherited Create;
  If Size = 0 Then Size := 3;
  FBuffer := GetMemory(Size);
  FBufferLength := Size;
  FBufferOffset := 0;
  PByte(FBuffer)^ := Head;
  FNeedFree := True;
end;

constructor TPacket.Create(Buffer: Pointer; Length: Cardinal; CopyData: Boolean);
begin
  Inherited Create;
  If CopyData Then Begin
    FBuffer := GetMemory(Length);
    FNeedFree := True;
  End Else Begin
    FBuffer := Buffer;
    FNeedFree := False;
  End;
  FBufferLength := Length;
  FBufferOffset := 0;
  ReadPacket;
  If not CopyData Then FBuffer := nil;
end;

destructor TPacket.Destroy;
begin
  If FNeedFree Then FreeMemory(FBuffer);
  Inherited;
end;

initialization
  PacketReader := nil;
finalization
  If Assigned(PacketReader) Then PacketReader.Free;
end.
