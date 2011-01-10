unit Common;

interface

function IntToStr(Value: Integer): string; overload;
function IntToStr(Value: Int64): string; overload;
function IntToHex(Value: Int64; Digits: Integer): string;
function StrToInt(const S: string): Integer;
function UpperCase(const S: string): string;

function FileExists(const AFilePath: String): Boolean;

function ExtractFilePath(const FileName: string): string;

implementation

function IntToStr(Value: Int64): string;
begin
  Str(Value, Result);
end;

function IntToStr(Value: Integer): string;
begin
  Str(Value, Result);
end;

function StrToInt(const S: string): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then Result := 0;
end;

function IntToHex(Value: Int64; Digits: Integer): string;
var
  i: Integer;
const
  Hex: String = '0123456789ABCDEF';
begin
  Result := '';
  For i := 0 to Digits - 1 do begin
    Result := Hex[(Value mod 16) + 1] + Result;
    Value := Value div 16;
  end;
end;

function UpperCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function FileExists(const AFilePath: String): Boolean;
var
  F: File;
Begin
  AssignFile(F, AFilePath);
  {$I-}
  Reset(F, 1);
  {$I+}
  Result := IOResult = 0;
  If Result Then CloseFile(F);
End;

function ExtractFilePath(const FileName: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := Length(FileName) downto 1 do if FileName[i]='\' Then Begin
    Result := Copy(FileName, 1, i);
    Break;
  End;
end;

end.
