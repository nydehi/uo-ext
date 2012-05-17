unit Encryption;

interface

uses Windows, Twofish;

type
  TNoEncryption=class
  protected
    FEncrypt: Boolean;
    FDecrypt: Boolean;
  public
    property NeedEncrypt: Boolean read FEncrypt write FEncrypt;
    property NeedDecrypt: Boolean read FDecrypt write FDecrypt;
    constructor Create;
    function Encrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal): Boolean; virtual;
    function Decrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal): Boolean; virtual;
  end;

  TLoginEncryption=class(TNoEncryption)
  private
    FSeed: Cardinal;
    FEncTable1: Cardinal;
    FEncTable2: Cardinal;
    FDecTable1: Cardinal;
    FDecTable2: Cardinal;
    FKey1: Cardinal;
    FKey2: Cardinal;
    procedure CalculateKeys(Major, Minor, Build: Cardinal);
    procedure CalculateTables;
  public
    constructor Create(Seed: Cardinal); overload;
    constructor Create(Seed, Major, Minor, Build: Cardinal); overload;
    function Encrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal): Boolean; override;
    function Decrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal): Boolean; override;
  end;

  TGameEncryptionCS=class(TNoEncryption)
  private
    FKey: Array [0..15] of Byte;
    FDecryptCipherTable: Array [0..255] of Byte;
    FEncryptCipherTable: Array [0..255] of Byte;
    FDecryptPosition: Cardinal;
    FEncryptPosition: Cardinal;

    FTFDecryptData: TTwofishData;
    FTFEncryptData: TTwofishData;

    procedure RefreshDecryptCipherTable();
    procedure RefreshEncryptCipherTable();
  public
    constructor Create(ASeed: Cardinal);
    function Encrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal): Boolean; override;
    function Decrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal): Boolean; override;
  end;

  TGameEncryptionSC=class(TNoEncryption)
  private
    FKey: Array [0..15] of Byte;
    FXORData: Array [0..15] of Byte; //MD5 Hash
    FDecryptPosition: Cardinal;
    FEncryptPosition: Cardinal;
  public
    constructor Create(ASeed: Cardinal);
    function Encrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal): Boolean; override;
    function Decrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal): Boolean; override;
  end;

implementation

uses Common{, SysUtils}, md5, ShardSetup, WinSock;

// TNoEncryption

constructor TNoEncryption.Create;
Begin
  Inherited;
  FEncrypt := False;
  FDecrypt := False;
End;

function TNoEncryption.Encrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal): Boolean;
Begin
  Result := True;
End;

function TNoEncryption.Decrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal): Boolean;
Begin
  Result := True;
End;

// TLoginEncryption

constructor TLoginEncryption.Create(Seed: Cardinal);
var
  cMajor, cMinor, cBuild: Cardinal;
Begin
  Inherited Create;
  FSeed := Seed;
  CalculateTables;

  if not GetExeVersion(cMajor, cMinor, cBuild) then Halt(1);

  CalculateKeys(cMajor, cMinor, cBuild);
End;

constructor TLoginEncryption.Create(Seed, Major, Minor, Build: Cardinal);
Begin
  Inherited Create;
  FSeed := Seed;
  CalculateTables;
  CalculateKeys(Major, Minor, Build);
End;

procedure TLoginEncryption.CalculateTables;
Begin
  FEncTable1 := ( ( ( NOT FSeed ) XOR $1357 ) SHL 16 ) OR ( ( FSeed XOR $FFFFAAAA ) AND $FFFF );
  FEncTable2 := ( ( FSeed XOR $43210000 ) SHR 16 ) OR ( ( ( NOT FSeed ) XOR $ABCDFFFF ) AND $FFFF0000 );

  FDecTable1 := FEncTable1;
  FDecTable2 := FEncTable2;
End;

procedure TLoginEncryption.CalculateKeys(Major, Minor, Build: Cardinal);
var
  cTmp: Cardinal;
Begin
  cTmp :=  ( ( ( ( Major SHL 9 ) OR Minor ) SHL 10 ) OR Build ) XOR ( ( Build * Build ) SHL 5 );
  FKey1 := ( cTmp SHL 4 ) XOR ( Minor * Minor ) XOR ( Minor * $0B000000) XOR ( Build * $380000 ) XOR $2C13A5FD;

  cTmp :=  ( ( ( ( ( Major SHL 9 ) OR Build ) SHL 10 ) OR Minor ) * 8 ) XOR ( Build * Build * $C00 );
  FKey2 := cTmp XOR ( Minor * Minor ) XOR ( Minor * $68000000 ) XOR ( Build * $1C0000 ) XOR $0A31D527F;
End;

function TLoginEncryption.Decrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal):Boolean;
Var
  cByte: PByte;
  i: Integer;
  tmp1, tmp2 : Cardinal;
Begin
  Result := True;
  If not FDecrypt Then Exit;
  cByte := PByte(Cardinal(Data) + Offset - 1);
  For i := 0 to Length - 1 do Begin
    cByte := PByte(Cardinal(cByte) + 1);
    cByte^ := cByte^ XOR Byte(FDecTable1 AND $FF);

    tmp1 := FDecTable1;
    tmp2 := FDecTable2;

    FDecTable2 := ( ( ( ( ( tmp2 SHR 1 ) OR ( tmp1 SHL 31 ) ) XOR FKey1 ) SHR 1 ) OR ( tmp1 SHL 31 ) ) XOR FKey1;
    FDecTable1 := ( ( tmp1 SHR 1 ) OR ( tmp2 SHL 31) ) XOR FKey2;
  End;
End;

function TLoginEncryption.Encrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal):Boolean;
Var
  cByte: PByte;
  i: Integer;
  tmp1, tmp2 : Cardinal;
Begin
  Result := True;
  If not FEncrypt Then Exit;
  cByte := PByte(Cardinal(Data) + Offset - 1);
  For i := 0 to Length - 1 do Begin
    cByte := PByte(Cardinal(cByte) + 1);
    cByte^ := cByte^ XOR Byte(FEncTable1 AND $FF);

    tmp1 := FEncTable1;
    tmp2 := FEncTable2;

    FEncTable2 := ( ( ( ( ( tmp2 SHR 1 ) OR ( tmp1 SHL 31 ) ) XOR FKey1 ) SHR 1 ) OR ( tmp1 SHL 31 ) ) XOR FKey1;
    FEncTable1 := ( ( tmp1 SHR 1 ) OR ( tmp2 SHL 31) ) XOR FKey2;
  End;
End;

// TGameEncryptionCS

constructor TGameEncryptionCS.Create(ASeed: Cardinal);
Var
  i: Integer;
Begin
  Inherited Create;
  FKey[0] := ( ASeed SHR 24 ) AND $FF;
  FKey[1] := ( ASeed SHR 16 ) AND $FF;
  FKey[2] := ( ASeed SHR  8 ) AND $FF;
  FKey[3] := ( ASeed        ) AND $FF;
  For i := 1 to 3 do Begin
    FKey[ i*4 + 0] := FKey[0];
    FKey[ i*4 + 1] := FKey[1];
    FKey[ i*4 + 2] := FKey[2];
    FKey[ i*4 + 3] := FKey[3];
  End;

  For i := 0 to 255 do Begin
    FDecryptCipherTable[i] := i;
    FEncryptCipherTable[i] := i;
  End;

  TwofishReset(FTFEncryptData);
  TwofishReset(FTFDecryptData);
  TwofishInit(FTFEncryptData, FKey, 128, nil);
  TwofishInit(FTFDecryptData, FKey, 128, nil);

  RefreshEncryptCipherTable;
  RefreshDecryptCipherTable;
End;

procedure TGameEncryptionCS.RefreshEncryptCipherTable();
Var
  i:Cardinal;
Begin
  For i:=0 to 15 do Begin
    TwofishEncryptECB(FTFEncryptData, Pointer( Cardinal(@FEncryptCipherTable[0]) + i * 16)^, Pointer( Cardinal(@FEncryptCipherTable[0]) + i * 16)^);
  End;
  FEncryptPosition := 0;
End;

procedure TGameEncryptionCS.RefreshDecryptCipherTable();
Var
  i:Cardinal;
Begin
  For i:=0 to 15 do Begin
    TwofishEncryptECB(FTFDecryptData, Pointer( Cardinal(@FDecryptCipherTable[0]) + i * 16)^, Pointer( Cardinal(@FDecryptCipherTable[0]) + i * 16)^);
  End;
  FDecryptPosition := 0;
End;

function TGameEncryptionCS.Decrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal):Boolean;
Var
  i: Cardinal;
  cByte: PByte;
Begin
  Result := True;
  If not FDecrypt Then Exit;
  For i:=0 to Length - 1 do Begin
    if FDecryptPosition >= $100 Then RefreshDecryptCipherTable;
    cByte := Pointer(Cardinal(Data) + i);
    cByte^ := cByte^ XOR FDecryptCipherTable[FDecryptPosition];
    FDecryptPosition := FDecryptPosition + 1;
  End;
End;

function TGameEncryptionCS.Encrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal):Boolean;
Var
  i: Cardinal;
  cByte: PByte;
Begin
  Result := True;
  If not FEncrypt Then Exit;
  For i:=0 to Length - 1 do Begin
    if FEncryptPosition >= $100 Then RefreshEncryptCipherTable;
    cByte := Pointer(Cardinal(Data) + i);
    cByte^ := cByte^ XOR FEncryptCipherTable[FEncryptPosition];
    FEncryptPosition := FEncryptPosition + 1;
  End;
End;

// TGameEncryptionSC

constructor TGameEncryptionSC.Create(ASeed: Cardinal);
Var
  i: Byte;
  tmp: TMD5Digest;
  CipherTable: Array [0..255] of Byte;
  tfd : TTwofishData;
Begin
  Inherited Create;
  FKey[0] := ( ASeed SHR 24 ) AND $FF;
  FKey[1] := ( ASeed SHR 16 ) AND $FF;
  FKey[2] := ( ASeed SHR  8 ) AND $FF;
  FKey[3] := ( ASeed        ) AND $FF;
  For i := 1 to 3 do Begin
    FKey[ i*4 + 0] := FKey[0];
    FKey[ i*4 + 1] := FKey[1];
    FKey[ i*4 + 2] := FKey[2];
    FKey[ i*4 + 3] := FKey[3];
  End;

  For i := 0 to 255 do Begin
    CipherTable[i] := i;
  End;

  TwofishReset(tfd);
  TwofishInit(tfd, FKey, 128, nil);

  For i:=0 to 15 do Begin
    TwofishEncryptECB(tfd, Pointer( Cardinal(@CipherTable[0]) + i * 16)^, Pointer( Cardinal(@CipherTable[0]) + i * 16)^);
  End;

  tmp := MD5Buffer(CipherTable, SizeOf(CipherTable));
  CopyMemory(@FXORData, @tmp.v[0], 16);

  FDecryptPosition := 0;
  FEncryptPosition := 0;
End;

function TGameEncryptionSC.Decrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal):Boolean;
Var
  i: Cardinal;
  cByte: PByte;
Begin
  Result := True;
  If not FDecrypt Then Exit;
  For i:=0 to Length - 1 do Begin
    cByte := Pointer(Cardinal(Data) + i);
    cByte^ := cByte^ XOR FXORData[FDecryptPosition];
    FDecryptPosition := (FDecryptPosition + 1) AND $0F;
  End;
End;

function TGameEncryptionSC.Encrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal):Boolean;
Var
  i: Cardinal;
  cByte: PByte;
Begin
  Result := True;
  If not FEncrypt Then Exit;
  For i:=0 to Length - 1 do Begin
    cByte := Pointer(Cardinal(Data) + i);
    cByte^ := cByte^ XOR FXORData[FEncryptPosition];
    FEncryptPosition := (FEncryptPosition + 1) AND $0F;
  End;
End;

end.
