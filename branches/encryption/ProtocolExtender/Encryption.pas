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

  TGameEncryption=class(TNoEncryption)
  private
    FKey: Array [0..15] of Byte;
    cipherTable: Array [0..255] of Byte;
    FXORData: Array [0..15] of Byte; //MD5 Hash
    FRecvPosition: Cardinal;
    FSendPosition: Cardinal;

    FTFData: TTwofishData;
    procedure RefreshCipherTable();
  public
    constructor Create(ASeed: Cardinal);
    function Encrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal): Boolean; override;
    function Decrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal): Boolean; override;
  end;

implementation

uses Common{, SysUtils}, md5, ShardSetup;

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
  ExeName : Array [0..MAX_PATH] of Char;
  ExeNameLength, cTmp, cFVLen, cFileVersionLen: Cardinal;
  pFileVersion, pFileInfo: Pointer;
  cMajor, cMinor, cBuild: Cardinal;
Begin
  Inherited Create;
  FSeed := Seed;
  CalculateTables;

  // Getting Current Exe Version
  ExeNameLength := GetModuleFileName(0, @ExeName[0], MAX_PATH);
  If ExeNameLength = 0 Then Halt(1); // TODO: Refactor this place.
  cFVLen := GetFileVersionInfoSize(@ExeName[0], cTmp);
  pFileVersion := GetMemory(cFVLen);
  GetFileVersionInfo(@ExeName[0], 0, cFVLen, pFileVersion);
  If not VerQueryValue(pFileVersion, '\', pFileInfo, cFileVersionLen) Then Halt(1); //TODO: Refactor this place.

  // Getting Major, Minor and Build values from Version
    cMajor := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
    cMinor := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
    cBuild := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
    //iVer[4] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
  FreeMemory(pFileVersion);

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
    cByte^ := cByte^ XOR FDecTable1;

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
    cByte^ := cByte^ XOR FEncTable1;

    tmp1 := FEncTable1;
    tmp2 := FEncTable2;

    FEncTable2 := ( ( ( ( ( tmp2 SHR 1 ) OR ( tmp1 SHL 31 ) ) XOR FKey1 ) SHR 1 ) OR ( tmp1 SHL 31 ) ) XOR FKey1;
    FEncTable1 := ( ( tmp1 SHR 1 ) OR ( tmp2 SHL 31) ) XOR FKey2;
  End;
End;

// TGameEncryption

constructor TGameEncryption.Create(ASeed: Cardinal);
Var
  i: Integer;
  tmp: TMD5Digest;
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

  For i := 0 to 255 do cipherTable[i] := i;
  FSendPosition := 0;
  FRecvPosition := 0;

  TwofishReset(FTFData);
  TwofishInit(FTFData, FKey, 128, nil);

  RefreshCipherTable;

  tmp := MD5Buffer(cipherTable, SizeOf(cipherTable));
  CopyMemory(@FXORData, @tmp, SizeOf(FXORData));
End;

procedure TGameEncryption.RefreshCipherTable();
Var
  block : Array [0..3] of Cardinal;
  i:Cardinal;
Begin
  For i:=0 to 15 do Begin
    CopyMemory(@block, Pointer( Cardinal(@cipherTable) + i * 16), 16);
    TwofishEncryptECB(FTFData, block, block);
    CopyMemory(Pointer( Cardinal(@cipherTable) + i * 16), @block, 16);
  End;
  FRecvPosition := 0;
End;

function TGameEncryption.Decrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal):Boolean;
Var
  i: Cardinal;
  cByte: PByte;
Begin
  Result := True;
  If not FDecrypt Then Exit;
  For i:=0 to Length - 1 do Begin
    if FRecvPosition >= $100 Then RefreshCipherTable;
    cByte := Pointer(Cardinal(Data) + i);
    cByte^ := cByte^ XOR cipherTable[FRecvPosition];
    FRecvPosition := FRecvPosition + 1;
  End;
End;

function TGameEncryption.Encrypt(Data: Pointer; Offset: Cardinal; Length: Cardinal):Boolean;
var
  cByte: PByte;
  i: Cardinal;
Begin
  Result := True;
  If not FEncrypt Then Exit;
  For i:=0 to Length - 1 do Begin
    cByte := Pointer(Cardinal(Data) + i);
    cByte^ := cByte^ XOR FXORData[FSendPosition];
    FSendPosition := (FSendPosition + 1) AND $0F;
  End;
End;

end.
