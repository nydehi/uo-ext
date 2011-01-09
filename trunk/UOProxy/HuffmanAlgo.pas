unit HuffmanAlgo;

interface

type
  THuffman=class
  public
    function compress(Dest:Pointer; var DstLen:Cardinal; Source:Pointer; SourceLen:Cardinal):Boolean;
    function decompress(Dest:Pointer; var DstLen:Cardinal; Source:Pointer; var SourceLen:Cardinal):Boolean;
  end;

var
  Huffman:THuffman;

implementation

uses Windows;

type
  THuffmanTable=Array [0..513] of Word;
  TByteArr=Array [0..489] of Byte;
  PByteArr=^TByteArr;

const
  UOTable:THuffmanTable = (
  $2, $000, $5, $01F, $6, $022, $7, $034, $7, $075, $6, $028, $6, $03B, $7, $032,
	$8, $0E0,	$8, $062,	$7, $056,	$8, $079,	$9, $19D,	$8, $097,	$6, $02A,	$7, $057,
	$8, $071,	$8, $05B,	$9, $1CC,	$8, $0A7,	$7, $025,	$7, $04F,	$8, $066,	$8, $07D,
	$9, $191,	$9, $1CE,	$7, $03F,	$9, $090,	$8, $059,	$8, $07B,	$8, $091,	$8, $0C6,
	$6, $02D,	$9, $186,	$8, $06F,	$9, $093,	$A, $1CC,	$8, $05A,	$A, $1AE,	$A, $1C0,
	$9, $148,	$9, $14A,	$9, $082,	$A, $19F,	$9, $171,	$9, $120,	$9, $0E7,	$A, $1F3,
	$9, $14B,	$9, $100,	$9, $190,	$6, $013,	$9, $161,	$9, $125,	$9, $133,	$9, $195,
	$9, $173,	$9, $1CA,	$9, $086,	$9, $1E9,	$9, $0DB,	$9, $1EC,	$9, $08B,	$9, $085,
	$5, $00A,	$8, $096,	$8, $09C,	$9, $1C3,	$9, $19C,	$9, $08F,	$9, $18F,	$9, $091,
	$9, $087,	$9, $0C6,	$9, $177,	$9, $089,	$9, $0D6,	$9, $08C,	$9, $1EE,	$9, $1EB,
	$9, $084,	$9, $164,	$9, $175,	$9, $1CD,	$8, $05E,	$9, $088,	$9, $12B,	$9, $172,
	$9, $10A,	$9, $08D,	$9, $13A,	$9, $11C,	$A, $1E1,	$A, $1E0,	$9, $187,	$A, $1DC,
	$A, $1DF,	$7, $074,	$9, $19F,	$8, $08D,	$8, $0E4,	$7, $079,	$9, $0EA,	$9, $0E1,
	$8, $040,	$7, $041,	$9, $10B,	$9, $0B0,	$8, $06A,	$8, $0C1,	$7, $071,	$7, $078,
	$8, $0B1,	$9, $14C,	$7, $043,	$8, $076,	$7, $066,	$7, $04D,	$9, $08A,	$6, $02F,
	$8, $0C9,	$9, $0CE,	$9, $149,	$9, $160,	$A, $1BA,	$A, $19E,	$A, $39F,	$9, $0E5,
	$9, $194,	$9, $184,	$9, $126,	$7, $030,	$8, $06C,	$9, $121,	$9, $1E8,	$A, $1C1,
	$A, $11D,	$A, $163,	$A, $385,	$A, $3DB,	$A, $17D,	$A, $106,	$A, $397,	$A, $24E,
	$7, $02E,	$8, $098,	$A, $33C,	$A, $32E,	$A, $1E9,	$9, $0BF,	$A, $3DF,	$A, $1DD,
	$A, $32D,	$A, $2ED,	$A, $30B,	$A, $107,	$A, $2E8,	$A, $3DE,	$A, $125,	$A, $1E8,
	$9, $0E9,	$A, $1CD,	$A, $1B5,	$9, $165,	$A, $232,	$A, $2E1,	$B, $3AE,	$B, $3C6,
	$B, $3E2,	$A, $205,	$A, $29A,	$A, $248,	$A, $2CD,	$A, $23B,	$B, $3C5,	$A, $251,
	$A, $2E9,	$A, $252,	$9, $1EA,	$B, $3A0,	$B, $391,	$A, $23C,	$B, $392,	$B, $3D5,
	$A, $233,	$A, $2CC,	$B, $390,	$A, $1BB,	$B, $3A1,	$B, $3C4,	$A, $211,	$A, $203,
	$9, $12A,	$A, $231,	$B, $3E0,	$A, $29B,	$B, $3D7,	$A, $202,	$B, $3AD,	$A, $213,
	$A, $253,	$A, $32C,	$A, $23D,	$A, $23F,	$A, $32F,	$A, $11C,	$A, $384,	$A, $31C,
	$A, $17C,	$A, $30A,	$A, $2E0,	$A, $276,	$A, $250,	$B, $3E3,	$A, $396,	$A, $18F,
	$A, $204,	$A, $206,	$A, $230,	$A, $265,	$A, $212,	$A, $23E,	$B, $3AC,	$B, $393,
	$B, $3E1,	$A, $1DE,	$B, $3D6,	$A, $31D,	$B, $3E5,	$B, $3E4,	$A, $207,	$B, $3C7,
	$A, $277,	$B, $3D4,	$8, $0C0,	$A, $162,	$A, $3DA,	$A, $124,	$A, $1B4,	$A, $264,
	$A, $33D,	$A, $1D1,	$A, $1AF,	$A, $39E,	$A, $24F,	$B, $373,	$A, $249,	$B, $372,
	$9, $167,	$A, $210,	$A, $23A,	$A, $1B8,	$B, $3AF,	$A, $18E,	$A, $2EC,	$7, $062,
	$4, $00D);

// THuffman

function THuffman.compress(Dest:Pointer; var DstLen:Cardinal; Source:Pointer; SourceLen:Cardinal):Boolean;
var
  i:Cardinal;
  bitSet:Byte;
  Written:Cardinal;
  toWrite:Word;
  tWLen:Byte;
begin
  ZeroMemory(Dest, DstLen);
  Written:=0;
  bitSet:=0;
  For i:=0 to SourceLen-1 do begin
    toWrite:=UOTable[PByte(Source)^*2+1];
    tWLen:=UOTable[PByte(Source)^*2];
    repeat
      If (8-bitSet)>=tWLen Then Begin
        PByte(Dest)^:=PByte(Dest)^ OR (toWrite SHL (8-bitSet-tWLen));
        bitSet:=bitSet+tWLen;
        If bitSet=8 Then Begin
          Dest:=Pointer(Cardinal(Dest)+1);
          Inc(Written);
          bitSet:=0;
        End;
        tWLen:=0;
      End Else Begin
        PByte(Dest)^:= PByte(Dest)^ OR (toWrite SHR (tWLen - (8-bitSet)));
        toWrite:=toWrite SHL (16 - tWLen + (8-bitSet));
        toWrite:=toWrite SHR (16 - tWLen + (8-bitSet));
        tWLen:=tWLen-(8-bitSet);
        Dest:=Pointer(Cardinal(Dest)+1);
        Inc(Written);
        bitSet:=0;
      End;
    until tWLen=0;
    Source:=Pointer(Cardinal(Source)+1);
  end;
  toWrite:=UOTable[513];
  tWLen:=UOTable[512];
  repeat
    If (8-bitSet)>=tWLen Then Begin
      PByte(Dest)^:=PByte(Dest)^ OR (toWrite SHL (8-bitSet-tWLen));
      bitSet:=bitSet+tWLen;
      If bitSet=8 Then Begin
        Dest:=Pointer(Cardinal(Dest)+1);
        Inc(Written);
        bitSet:=0;
      End;
      tWLen:=0;
    End Else Begin
      PByte(Dest)^:= PByte(Dest)^ OR (toWrite SHR (tWLen - (8-bitSet)));
      toWrite:=toWrite SHL (16 - tWLen + (8-bitSet));
      toWrite:=toWrite SHR (16 - tWLen + (8-bitSet));
      tWLen:=tWLen-(8-bitSet);
      Dest:=Pointer(Cardinal(Dest)+1);
      Inc(Written);
      bitSet:=0;
    End;
  until tWLen=0;
  If bitSet>0 Then Inc(Written);
  DstLen:=Written;
  Result:=True;
end;

function THuffman.decompress(Dest:Pointer; var DstLen:Cardinal; Source:Pointer; var SourceLen:Cardinal):Boolean;
var
  j, k:Cardinal;
  bitSet:Byte;
  bFound:Boolean;
  bitMask, cValue, cutMask:Cardinal;
  Written, Readed, bitRemain:Cardinal;
Begin
  Result:=False;
  ZeroMemory(Dest, DstLen);
  bitSet:=0;
  Written:=0;
  Readed:=0;
  bitRemain:=SourceLen*8;
  bitMask:=PByte(Cardinal(Source))^ SHL 16;
  If SourceLen>1 Then bitMask:=bitMask + PByte(Cardinal(Source)+1)^ SHL 8;
  If SourceLen>2 Then bitMask:=bitMask + PByte(Cardinal(Source)+2)^;
  If SourceLen>0 Then
  Repeat
    bFound:=False;
    For j:=0 to 256 do If bitRemain>=UOTable[j*2] Then Begin
      cValue:=UOTable[j*2+1] shl (24 - UOTable[j*2]);
      cutMask:=(1 shl (UOTable[j*2]) - 1) shl (24 - UOTable[j*2]);
      If (bitMask and cutMask) = (cValue and cutMask) Then Begin
        If j=256 Then Begin
          bitSet:=bitSet+UOTable[j*2];
          Dec(bitRemain, UOTable[j*2]);
          If bitSet>=8 then Begin
//            Source:=Pointer(Cardinal(Source)+bitSet div 8);
            Readed:=Readed+bitSet div 8;
            bitSet:=bitSet mod 8;
          End;
          If bitSet<>0 Then Inc(Readed);
          SourceLen:=Readed;
          Result:=True;
          DstLen:=Written;
          Exit;
        End Else Begin
          PByte(Dest)^:=j;
          Inc(Written);
          Dest:=Pointer(Cardinal(Dest) + 1);
          bitSet:=bitSet+UOTable[j*2];
          Dec(bitRemain, UOTable[j*2]);
          bitMask:=bitMask SHL UOTable[j*2];
          If bitSet>=8 Then Begin
            bitMask:=bitMask SHR bitSet;
            For k:=1 to bitSet div 8 do Begin
              bitMask:=bitMask SHL 8;
              If (SourceLen-Readed)>=3 Then Begin
                bitMask:=bitMask + PByte(Cardinal(Source)+3)^;
                Source:=Pointer(Cardinal(Source)+1);
              End;
              Inc(Readed);
            End;
            bitSet:=bitSet MOD 8;
            bitMask:=bitMask SHL bitSet;
          End;
        End;
        bFound:=True;
        Break;
      End;
    End;
  Until (not bFound) or (Readed>=SourceLen) or (bitRemain<=0);
  If bitSet<>0 Then Inc(Readed);
  SourceLen:=Readed;
  DstLen:=Written;
End;

initialization
  Huffman:=THuffman.Create;
Finalization
  Huffman.Free;
end.
