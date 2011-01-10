unit ProtocolDescription;

interface

uses WinSock, PluginsShared;

type

  RPacketData=record
    case Cardinal of
    0: (Length:Cardinal);
    1: (Handler:TPacketLengthDefinition)
  end;

  TProtocolDescription=class
  private
    FCliServ:Array [0..255] of RPacketData;
    FServCli:Array [0..255] of RPacketData;
    FCSCount:Byte;
    FSCCount:Byte;
  public
    property CliServCount:Byte read FCSCount;
    property ServCliCount:Byte read FSCCount;
    constructor Create;
    function GetCliServLength(Data:Pointer; Length:Cardinal):Cardinal;
    function GetServCliLength(Data:Pointer; Length:Cardinal):Cardinal;
    procedure AddPacketInfo(IsCliServ:Boolean; Packet:Byte; Length:Cardinal); overload;
    procedure AddPacketInfo(IsCliServ:Boolean; Packet:Byte; Handler:TPacketLengthDefinition); overload;
  end;

  procedure InitProtocolDescription(Descriptor:TProtocolDescription);

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
    AddPacketInfo(True, $1D, 5);
    AddPacketInfo(True, $22, 3);
    AddPacketInfo(True, $2C, 2);
    AddPacketInfo(True, $32, 2);
    AddPacketInfo(True, $34, 10);
    AddPacketInfo(True, $37, 8);
    AddPacketInfo(True, $3A, 0);
    AddPacketInfo(True, $3B, 0);
    AddPacketInfo(True, $47, 11);
    AddPacketInfo(True, $48, 73);
    AddPacketInfo(True, $58, 106);
    AddPacketInfo(True, $5D, 73);
    AddPacketInfo(True, $61, 9);
    AddPacketInfo(True, $6C, 19);
    AddPacketInfo(True, $6F, 0);
    AddPacketInfo(True, $72, 5);
    AddPacketInfo(True, $73, 2);
    AddPacketInfo(True, $75, 35);
    AddPacketInfo(True, $79, 9);
    AddPacketInfo(True, $7E, 2);
    AddPacketInfo(True, $7D, 13);
    AddPacketInfo(True, $80, 62);
    AddPacketInfo(True, $83, 39);
    AddPacketInfo(True, $8D, 0);
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
    AddPacketInfo(True, $AD, 0);
    AddPacketInfo(True, $B1, 0);
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
    AddPacketInfo(True, $CF, 0);
    AddPacketInfo(True, $D0, 0);
    AddPacketInfo(True, $D1, 2);
    AddPacketInfo(True, $D6, 0);
    AddPacketInfo(True, $D7, 0);
    AddPacketInfo(True, $D9, 0);
    AddPacketInfo(True, $E0, 0);
    AddPacketInfo(True, $E1, 0);
    AddPacketInfo(True, $E4, 0);
    AddPacketInfo(True, $E8, 13);
    AddPacketInfo(True, $EB, 0);
    AddPacketInfo(True, $EC, 0);
    AddPacketInfo(True, $ED, 0);
    AddPacketInfo(True, $EF, 21);
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
    AddPacketInfo(False, $24, 7);
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
    AddPacketInfo(False, $95, 9);
    AddPacketInfo(False, $97, 2);
    AddPacketInfo(False, $98, 0);
    AddPacketInfo(False, $99, 26);
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
    AddPacketInfo(False, $B8, 0);
    AddPacketInfo(False, $B9, 3);
    AddPacketInfo(False, $BA, 6);
    AddPacketInfo(False, $BC, 3);
    AddPacketInfo(False, $BD, 0);
    AddPacketInfo(False, $BE, 0);
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
  end;
end;


constructor TProtocolDescription.Create;
var
  i:Byte;
begin
  Inherited Create;
  For i:=0 to 255 do begin
    FCliServ[i].Length:=0;
    FServCli[i].Length:=0;
  End;
end;

function TProtocolDescription.GetCliServLength(Data:Pointer; Length:Cardinal):Cardinal;
Begin
  Result:=0;
  If Length=0 Then Exit;
  If FCliServ[PByte(Data)^].Length<1048576 Then Begin
    Result:=FCliServ[PByte(Data)^].Length;
  End Else Begin
    Result:=FCliServ[PByte(Data)^].Handler(Data, Length);
  End;
End;

function TProtocolDescription.GetServCliLength(Data:Pointer; Length:Cardinal):Cardinal;
Begin
  Result:=0;
  If Length=0 Then Exit;
  If FServCli[PByte(Data)^].Length<1048576 Then Begin
    Result:=FServCli[PByte(Data)^].Length;
  End Else Begin
    Result:=FServCli[PByte(Data)^].Handler(Data, Length);
  End;
End;

procedure TProtocolDescription.AddPacketInfo(IsCliServ:Boolean; Packet:Byte; Length:Cardinal);
Begin
  If IsCliServ Then Begin
    If FCliServ[Packet].Length<>0 Then Dec(FCSCount);
    Inc(FCSCount);
    If Length>0 Then FCliServ[Packet].Length:=Length Else FCliServ[Packet].Handler:=DefaultPacketHandler;
  End Else Begin
    If FServCli[Packet].Length<>0 Then Dec(FSCCount);
    Inc(FCSCount);
    If Length>0 Then FServCli[Packet].Length:=Length Else FServCli[Packet].Handler:=DefaultPacketHandler;
  End;
End;

procedure TProtocolDescription.AddPacketInfo(IsCliServ:Boolean; Packet:Byte; Handler:TPacketLengthDefinition);
Begin
  If IsCliServ Then Begin
    If FCliServ[Packet].Length<>0 Then Dec(FCSCount);
    Inc(FCSCount);
    FCliServ[Packet].Handler:=Handler;
  End Else Begin
    If FServCli[Packet].Length<>0 Then Dec(FSCCount);
    Inc(FSCCount);
    FServCli[Packet].Handler:=Handler;
  End;
End;

initialization
  ProtocolDescriptor := TProtocolDescription.Create;
  InitProtocolDescription(ProtocolDescriptor);
end.
