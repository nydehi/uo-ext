{******************************************************************************}
{** Base classes for block cipher and hash algorithm implementations **********}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit DCPcrypt;

interface
{$I DCPcrypt.inc}

const
  DCPpage= 'DCPcrypt';

type
  PWord= ^Word;
  PDWord= ^DWord;
  DWord= longword;

  PDWordArray= ^TDWordArray;
  TDWordArray= array[0..1023] of DWord;
  PByteArray= ^TByteArray;
  TByteArray= array[0..4095] of byte;

{******************************************************************************}
{******************************************************************************}
function LRot16(X: Word; c: longint): Word; assembler;
function RRot16(X: Word; c: longint): Word; assembler;
function LRot32(X: DWord; c: longint): DWord; assembler;
function RRot32(X: DWord; c: longint): DWord; assembler;
function SwapDWord(X: DWord): DWord; assembler;

procedure XorBlock(I1, I2, O1: PByteArray; Len: longint);

implementation


function LRot16(X: Word; c: longint): Word; assembler;
asm
  mov ecx,&c
  mov ax,&X
  rol ax,cl
  mov &Result,ax
end;

function RRot16(X: Word; c: longint): Word; assembler;
asm
  mov ecx,&c
  mov ax,&X
  ror ax,cl
  mov &Result,ax
end;

function LRot32(X: DWord; c: longint): DWord; register; assembler;
asm
  mov ecx, edx
  rol eax, cl
end;

function RRot32(X: DWord; c: longint): DWord; register; assembler;
asm
  mov ecx, edx
  ror eax, cl
end;

function SwapDWord(X: DWord): DWord; register; assembler;
asm
  xchg al,ah
  rol  eax,16
  xchg al,ah
end;

procedure XorBlock(I1, I2, O1: PByteArray; Len: longint);
var
  i: longint;
begin
  for i:= 0 to Len-1 do
    O1^[i]:= I1^[i] xor I2^[i];
end;

end.
