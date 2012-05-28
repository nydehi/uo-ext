unit ProtocolDescription;

interface

uses Windows, WinSock, PluginsShared, Common;

type

  RPacketData=record
    case Cardinal of
    0: (Length:Cardinal);
    1: (Handler:TPacketLengthDefinition)
  end;

  TProtocolDescription=class
  private
    FPackets:Array [0..255] of RPacketData;
    FCount:Byte;
  public
    property Count:Byte read FCount;
    constructor Create;
    function GetLength(Data:Pointer; Length:Cardinal):Cardinal; overload;
    function GetLength(Header: Byte):Cardinal; overload;
    procedure AddPacketInfo(Packet:Byte; Length:Cardinal); overload;
    procedure AddPacketInfo(Packet:Byte; Handler:TPacketLengthDefinition); overload;
  end;

  procedure Init;
//  procedure InitProtocolDescription(Descriptor:TProtocolDescription);

var
  ProtocolDescriptor: TProtocolDescription;

implementation

function DefaultPacketHandler(Packet:Pointer; Length:Cardinal):Cardinal; stdcall;
var
  Len:PWord;
begin
  If Length<3 Then
    Result:=0
  else Begin
    Len:=PWord(Cardinal(Packet) + 1);
    Result:=(Len^ shr 8) + ((Len^ and $FF) shl 8);
  End;
end;

Function FindInMemory(Base:Pointer; Size: Cardinal; Mask: Pointer; MaskSize: Cardinal; NullByte:Byte): Pointer;
Var
  i, j: Cardinal;
  bFound: Boolean;
Begin
  Result := nil;
  For i := Cardinal(Base) to Cardinal(Base) + Size - 1 do If PByte(i)^ = PByte(Mask)^ then Begin
    bFound := True;
    For j := i to i + MaskSize - 1 do if NOT( (PByte(j)^ = PByte(Cardinal(Mask) + j - i)^) OR (PByte(Cardinal(Mask) + j - i)^ = NullByte) ) then Begin
      bFound := False;
      Break;
    End;
    if bFound then Begin
        Result := Pointer(i);
        Break;
    End;
  End;
End;

Function FindSignature(Base:Pointer; Size: Cardinal; var SignatureSize: Byte; var HandleOffset: Byte; var SizeOffset: Byte; var SizeSize: Byte):Pointer;
const
  Signature1 : Array [0..19] of Byte = ($00, $80, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $02, $00, $00, $00, $05, $00, $00, $00);
  Signature2 : Array [0..15] of Byte = ($00, $80, $00, $00, $04, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00);
  Signature3 : Array [0..15] of Byte = ($00, $80, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $02, $00, $05, $00, $00, $00);
  Signature4 : Array [0..11] of Byte = ($00, $80, $04, $00, $00, $00, $02, $00, $05, $00, $00, $00);
  Signature1Size = 20;
  Signature2Size = 16;
  Signature3Size = 16;
  Signature4Size = 12;
Begin
  HandleOffset := 0;
  SizeOffset := 4;
  SizeSize := 4;
  Result := FindInMemory(Base, Size, @Signature1, Signature1Size, $FF);
  if Result <> nil then Begin
    SignatureSize := 12;
    SizeOffset := 8;
    Result := Pointer(Cardinal(Result) - 12 * 3 - 8);
    Exit;
  End;
  Result := FindInMemory(Base, Size, @Signature2, Signature2Size, $FF);
  if Result <> nil then Begin
    SignatureSize := 8;
    Result := Pointer(Cardinal(Result) - 8 * 3 - 4);
    Exit;
  End;
  SizeSize := 2;
  Result := FindInMemory(Base, Size, @Signature3, Signature3Size, $FF);
  if Result <> nil then Begin
    SignatureSize := 10;
    SizeOffset := 8;
    Result := Pointer(Cardinal(Result) - 10 * 3 - 8);
    Exit;
  End;
  Result := FindInMemory(Base, Size, @Signature4, Signature4Size, $FF);
  if Result <> nil then Begin
    SignatureSize := 6;
    Result := Pointer(Cardinal(Result) - 6 * 3 - 4);
    Exit;
  End;
End;

Procedure FillProtocolDescription(Descriptor:TProtocolDescription; Table:Pointer; Row: Byte; Id: Byte; Size:Byte; SizeSize: Byte);
var
  lastId: Byte;
  currId: Byte;
  currPnt: Pointer;
  currSize: Word;
  FoundPackets: Word;
Begin
  currPnt := Table;
  FoundPackets := 0;
  currId := PCardinal(Cardinal(currPnt) + Id)^;
  repeat
    if SizeSize = 2 then
      currSize := PWord(Cardinal(currPnt) + Size)^
    Else
      currSize := PCardinal(Cardinal(currPnt) + Size)^;
    if currSize = $8000 then currSize := 0;
    Descriptor.AddPacketInfo(currId, currSize);

    FoundPackets := FoundPackets + 1;

    currPnt := Pointer(Cardinal(currPnt) + Row);

    lastId := currId;
    currId := PCardinal(Cardinal(currPnt) + Id)^;

  Until NOT ( (lastId < currId) OR ((lastId = $F8) AND (currId = $01)) );
  //Somehow, there is 2 error packets: $EE and $EF, that is 8192 in table, and $EF is 21 byte. $EE must be 10...
  Descriptor.AddPacketInfo($EE, 10);
  Descriptor.AddPacketInfo($EF, 21);

  {$IFDEF Debug}
  WriteLn('Protocol: Found ', FoundPackets, ' packets sizes');
  {$ENDIF}
End;

function ReadProtocolDescription(Descriptor:TProtocolDescription):Boolean;
type
  TPorcedure = procedure;
  TSections = Array [0..999] of TImageSectionHeader;
  PSections = ^TSections;
var
  DosHeader: PImageDosHeader;
  PEHeader: PImageFileHeader;
  OptHeader: PImageOptionalHeader;
  Sections: PSections;
  Exe: Pointer;

  i: Word;
  PacketTable: Pointer;
  PacketTableSize: Byte;
  HandleOffset, SizeOffset, SizeSize: Byte;
Begin
  {$IFDEF Debug}
  Write('Protocol: Searching protocol size table in main executable ... ');
  {$ENDIF}
  Exe := Pointer(GetModuleHandle(nil));
  DosHeader := Exe;
  PEHeader := Pointer(Cardinal(Exe) + Cardinal(DosHeader^._lfanew) + 4);
  OptHeader := Pointer(Cardinal(Exe) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader));
  Sections := Pointer(Cardinal(Exe) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader) + SizeOf(TImageOptionalHeader) + (OptHeader^.NumberOfRvaAndSizes - IMAGE_NUMBEROF_DIRECTORY_ENTRIES) * SizeOf(TImageDataDirectory));
  PacketTable := nil;
  PacketTableSize := 0;
  For i := 0 to PEHeader.NumberOfSections - 1 do Begin
    If PAnsiChar(@Sections[i].Name) = '.data' then Begin
      PacketTable := FindSignature(Pointer(Cardinal(Exe) + Sections[i].VirtualAddress), Sections[i].SizeOfRawData, PacketTableSize, HandleOffset, SizeOffset, SizeSize);
      {$IFDEF Debug}
      WriteLn('done');
      WriteLn('Protocol: Table pointer: 0x', IntToHex(Cardinal(PacketTable), 8), ', Row size: ', PacketTableSize);
      {$ENDIF}
      Break;
    End;
  End;

  If PacketTable = nil then Begin
   {$IFDEF Debug}
    WriteLn('done');
    WriteLn('Protocol not found! Aborting.');
    Sleep(5000);
    {$ENDIF}
    Halt(1);
  End;

  FillProtocolDescription(Descriptor, PacketTable, PacketTableSize, HandleOffset, SizeOffset, SizeSize);
  Result := True;
End;

procedure Init;
Begin
  if Assigned(ProtocolDescriptor) then ProtocolDescriptor.Free;
  ProtocolDescriptor := TProtocolDescription.Create;
  ReadProtocolDescription(ProtocolDescriptor);
//  InitProtocolDescription(ProtocolDescriptor);
End;

{
procedure InitProtocolDescription(Descriptor:TProtocolDescription);
begin

  With Descriptor do Begin
// Client - Server
    AddPacketInfo(True, $00, 104);
    AddPacketInfo(True, $01, 5);
    AddPacketInfo(True, $02, 7);
    AddPacketInfo(True, $03, 0);
    AddPacketInfo(True, $04, 2);
    AddPacketInfo(True, $05, 5);
    AddPacketInfo(True, $06, 5);
    AddPacketInfo(True, $07, 7);
    AddPacketInfo(True, $08, 15);
    AddPacketInfo(True, $09, 5);
    AddPacketInfo(True, $0A, 11);
    AddPacketInfo(True, $12, 0);
    AddPacketInfo(True, $13, 10);
    AddPacketInfo(True, $14, 6);
    AddPacketInfo(True, $1D, 5); // Not in 7 core
    AddPacketInfo(True, $22, 3);
    AddPacketInfo(True, $2C, 2);
    AddPacketInfo(True, $32, 2); // Not in 7 core
    AddPacketInfo(True, $34, 10);
    AddPacketInfo(True, $37, 8); // Not in 7 core
    AddPacketInfo(True, $3A, 0);
    AddPacketInfo(True, $3B, 0);
    AddPacketInfo(True, $47, 11);
    AddPacketInfo(True, $48, 73);
    AddPacketInfo(True, $58, 106);
    AddPacketInfo(True, $5D, 73);
    AddPacketInfo(True, $61, 9);
    AddPacketInfo(True, $66, 0);
    AddPacketInfo(True, $6C, 19);
    AddPacketInfo(True, $6F, 0);
    AddPacketInfo(True, $71, 0);
    AddPacketInfo(True, $72, 5);
    AddPacketInfo(True, $73, 2);
    AddPacketInfo(True, $75, 35);
    AddPacketInfo(True, $79, 9);
    AddPacketInfo(True, $7E, 2);
//    AddPacketInfo(True, $7D, 13);
    AddPacketInfo(True, $80, 62);
    AddPacketInfo(True, $83, 39);
    AddPacketInfo(True, $8D, 0); // Not in 7 core
    AddPacketInfo(True, $91, 65);
    AddPacketInfo(True, $95, 9);
    AddPacketInfo(True, $96, 0);
    AddPacketInfo(True, $98, 0);
    AddPacketInfo(True, $9A, 0);
    AddPacketInfo(True, $9B, 258);
    AddPacketInfo(True, $9D, 51);
    AddPacketInfo(True, $9F, 0);
    AddPacketInfo(True, $A0, 3);
    AddPacketInfo(True, $A4, 149);
    AddPacketInfo(True, $A7, 4);
    AddPacketInfo(True, $AB, 0);
    AddPacketInfo(True, $AD, 0);
    AddPacketInfo(True, $B1, 0);
    AddPacketInfo(True, $B3, 0);
    AddPacketInfo(True, $B5, 64);
    AddPacketInfo(True, $B6, 9);
    AddPacketInfo(True, $B8, 0);
    AddPacketInfo(True, $BB, 9);
    AddPacketInfo(True, $BD, 0);
    AddPacketInfo(True, $BE, 0);
    AddPacketInfo(True, $BF, 0);
    AddPacketInfo(True, $C2, 0);
    AddPacketInfo(True, $C8, 2);
    AddPacketInfo(True, $C9, 6);
    AddPacketInfo(True, $CA, 6);
    AddPacketInfo(True, $CF, 0); // Not in 7 core
    AddPacketInfo(True, $D0, 0);
    AddPacketInfo(True, $D1, 1);
    AddPacketInfo(True, $D4, 0);
    AddPacketInfo(True, $D6, 0);
    AddPacketInfo(True, $D7, 0);
    AddPacketInfo(True, $D9, 268);
    AddPacketInfo(True, $DA, 0);
    AddPacketInfo(True, $E0, 0); // Not in 7 core
    AddPacketInfo(True, $E1, 0);
    AddPacketInfo(True, $E4, 0); // Not in 7 core
    AddPacketInfo(True, $E8, 13); // Not in 7 core
    AddPacketInfo(True, $EB, 0); // Not in 7 core
    AddPacketInfo(True, $EC, 0); // Not in 7 core
    AddPacketInfo(True, $ED, 0); // Not in 7 core
    AddPacketInfo(True, $EF, 21);
    AddPacketInfo(True, $F0, 0); // Razor negotiate reply
    AddPacketInfo(True, $F1, 9);
    AddPacketInfo(True, $F4, 0);
    AddPacketInfo(True, $F8, 106);
// Server - Client
    AddPacketInfo(False, $0B, 7);
    AddPacketInfo(False, $11, 0);
    AddPacketInfo(False, $15, 9);
    AddPacketInfo(False, $17, 0);
    AddPacketInfo(False, $1A, 0);
    AddPacketInfo(False, $1B, 37);
    AddPacketInfo(False, $1C, 0);
    AddPacketInfo(False, $1D, 5);
    AddPacketInfo(False, $20, 19);
    AddPacketInfo(False, $21, 8);
    AddPacketInfo(False, $22, 3);
    AddPacketInfo(False, $23, 26);
    AddPacketInfo(False, $24, 9);
    AddPacketInfo(False, $25, 21);
    AddPacketInfo(False, $27, 2);
    AddPacketInfo(False, $29, 1);
    AddPacketInfo(False, $2B, 2);
    AddPacketInfo(False, $2D, 17);
    AddPacketInfo(False, $2C, 2);
    AddPacketInfo(False, $2E, 15);
    AddPacketInfo(False, $2F, 10);
    AddPacketInfo(False, $32, 2);
    AddPacketInfo(False, $38, 7);
    AddPacketInfo(False, $3A, 0);
    AddPacketInfo(False, $3B, 8);
    AddPacketInfo(False, $3C, 0);
    AddPacketInfo(False, $3F, 0);
    AddPacketInfo(False, $4E, 6);
    AddPacketInfo(False, $4F, 2);
    AddPacketInfo(False, $53, 2);
    AddPacketInfo(False, $54, 12);
    AddPacketInfo(False, $55, 1);
    AddPacketInfo(False, $56, 11);
    AddPacketInfo(False, $5B, 4);
    AddPacketInfo(False, $65, 4);
    AddPacketInfo(False, $6C, 19);
    AddPacketInfo(False, $6D, 3);
    AddPacketInfo(False, $6E, 14);
    AddPacketInfo(False, $6F, 0);
    AddPacketInfo(False, $70, 28);
    AddPacketInfo(False, $72, 5);
    AddPacketInfo(False, $73, 2);
    AddPacketInfo(False, $74, 0);
    AddPacketInfo(False, $76, 16);
    AddPacketInfo(False, $77, 17);
    AddPacketInfo(False, $78, 0);
    AddPacketInfo(False, $7B, 2);
    AddPacketInfo(False, $7C, 0);
    AddPacketInfo(False, $81, 0);
    AddPacketInfo(False, $82, 2);
    AddPacketInfo(False, $85, 2);
    AddPacketInfo(False, $86, 0);
    AddPacketInfo(False, $88, 66);
    AddPacketInfo(False, $8B, 0);
    AddPacketInfo(False, $8C, 11);
    AddPacketInfo(False, $90, 19);
    AddPacketInfo(False, $95, 9);
    AddPacketInfo(False, $97, 2);
    AddPacketInfo(False, $98, 0);
    AddPacketInfo(False, $99, 30); //?
    AddPacketInfo(False, $9E, 0);
    AddPacketInfo(False, $A1, 9);
    AddPacketInfo(False, $A2, 9);
    AddPacketInfo(False, $A3, 9);
    AddPacketInfo(False, $A5, 0);
    AddPacketInfo(False, $A6, 0);
    AddPacketInfo(False, $A8, 0);
    AddPacketInfo(False, $A9, 0);
    AddPacketInfo(False, $AA, 5);
    AddPacketInfo(False, $AE, 0);
    AddPacketInfo(False, $AF, 13);
    AddPacketInfo(False, $B0, 0);
    AddPacketInfo(False, $B7, 0);
    AddPacketInfo(False, $B8, 0);
    AddPacketInfo(False, $B9, 5);
    AddPacketInfo(False, $BA, 10);
    AddPacketInfo(False, $BC, 3);
    AddPacketInfo(False, $BD, 0);
    AddPacketInfo(False, $BE, 8);
    AddPacketInfo(False, $BF, 0);
    AddPacketInfo(False, $C0, 36);
    AddPacketInfo(False, $C1, 0);
    AddPacketInfo(False, $C2, 0);
    AddPacketInfo(False, $C3, 0);
    AddPacketInfo(False, $C4, 6);
    AddPacketInfo(False, $C6, 1);
    AddPacketInfo(False, $C7, 49);
    AddPacketInfo(False, $C8, 2);
    AddPacketInfo(False, $C9, 6);
    AddPacketInfo(False, $CA, 6);
    AddPacketInfo(False, $CB, 7);
    AddPacketInfo(False, $CC, 0);
    AddPacketInfo(False, $D1, 2);
    AddPacketInfo(False, $D3, 0);
    AddPacketInfo(False, $DC, 9);
    AddPacketInfo(False, $DD, 0);
    AddPacketInfo(False, $F2, 25);
    AddPacketInfo(False, $E2, 10);
    AddPacketInfo(False, $E3, 0);
    AddPacketInfo(False, $E5, 0);
    AddPacketInfo(False, $E6, 5);
    AddPacketInfo(False, $E7, 12);
    AddPacketInfo(False, $E9, 75);
    AddPacketInfo(False, $EA, 3);
    AddPacketInfo(False, $F0, 0); // Razor negotiate ask
    AddPacketInfo(False, $F3, 26);
    AddPacketInfo(False, $F5, 21);
  end;
end;
}

constructor TProtocolDescription.Create;
begin
  Inherited Create;
  ZeroMemory(@FPackets[0], SizeOf(RPacketData) * 256);
end;

function TProtocolDescription.GetLength(Data:Pointer; Length:Cardinal):Cardinal;
Begin
  Result:=0;
  If Length=0 Then Exit;
  If FPackets[PByte(Data)^].Length<1048576 Then Begin
    Result:=FPackets[PByte(Data)^].Length;
  End Else Begin
    Result:=FPackets[PByte(Data)^].Handler(Data, Length);
  End;
End;

function TProtocolDescription.GetLength(Header: Byte):Cardinal;
Begin
  Result:=FPackets[Header].Length;
  If Result = 0 Then Result := $FFFFFFFF else if Result >= 1048576 Then Result := 0;
End;

procedure TProtocolDescription.AddPacketInfo(Packet:Byte; Length:Cardinal);
Begin
  If FPackets[Packet].Length =0 Then Inc(FCount);
  If Length>0 Then FPackets[Packet].Length:=Length Else FPackets[Packet].Handler:=DefaultPacketHandler;
End;

procedure TProtocolDescription.AddPacketInfo(Packet:Byte; Handler:TPacketLengthDefinition);
Begin
  If FPackets[Packet].Length = 0 Then Inc(FCount);
  FPackets[Packet].Handler:=Handler;
End;

initialization
//  ProtocolDescriptor := TProtocolDescription.Create;
//  InitProtocolDescription(ProtocolDescriptor);
end.
