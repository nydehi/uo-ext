unit LoginConfReader;

interface

{$IFNDEF PLUGINS_SERVER}
  {$UNDEF LOGIN_CFG}
{$ENDIF}


uses ShardSetup;

procedure TryReadLoginCfg;

implementation

uses Windows, Common;

{$IFDEF LOGIN_CFG}
Procedure UpString(Start: PAnsiChar);
Var
  cChar: PAnsiChar;
Begin
  cChar := Start;
  repeat
    if (cChar^ >= 'a') and (cChar^ <= 'z') then cChar^ := AnsiChar(PByte(cChar)^ + (Ord('A') - Ord('a')));
    cChar := PAnsiChar(Cardinal(cChar) + 1);
  until cChar^ = #13;
End;

Function CompareStrings(Source: PAnsiChar; ToCompare: AnsiString): Boolean;
var
  cChar: PAnsiChar;
  i: Cardinal;
Begin
  Result := False;
  cChar := Source;
  for i := 1 to Length(ToCompare) - 1 do Begin
    if PAnsiChar(Cardinal(cChar) + i - 1)^ <> ToCompare[i] then Exit;
  End;
  Result := True;
End;

procedure SetShardSetup(sUpdateServer: PAnsiChar);
var
  cChar: PAnsiChar;
  sDummy: AnsiString;
Begin
  cChar := sUpdateServer;
  repeat
    cChar := PAnsiChar(Cardinal(cChar) + 1);
  until cChar^ = ',';
  SetLength(ShardSetup.UpdateIP, Cardinal(cChar) - Cardinal(sUpdateServer));
  CopyMemory(@ShardSetup.UpdateIP[1], sUpdateServer, Cardinal(cChar) - Cardinal(sUpdateServer));

  sUpdateServer := PAnsiChar(Cardinal(cChar) + 1);
  cChar := sUpdateServer;
  repeat
    cChar := PAnsiChar(Cardinal(cChar) + 1);
  until (cChar^ = ' ') or (cChar^ = #9) or (cChar^ = #10) or (cChar^ = #13) or (cChar^ = ';');
  SetLength(sDummy, Cardinal(cChar) - Cardinal(sUpdateServer));
  CopyMemory(@sDummy[1], sUpdateServer, Cardinal(cChar) - Cardinal(sUpdateServer));

  ShardSetup.UpdatePort := StrToInt(sDummy);
End;

procedure ReadLoginCfg;
var
  F:File;
  fContent: Pointer;
  fCLength: Cardinal;
  cCurrentChar: PAnsiChar;
  sFileName: AnsiString;
Begin
  sFileName := ExtractFilePath(AnsiString(ParamStr(0))) + 'Login.cfg';
  AssignFile(F, sFileName);
  Reset(F, 1);
  fContent := GetMemory(FileSize(F));
  BlockRead(F, fContent^, FileSize(F), fCLength);
  CloseFile(F);
  cCurrentChar := fContent;
  repeat
    If (cCurrentChar^ = ';') and (PAnsiChar(Cardinal(cCurrentChar) + 1)^ = '#') then Begin
      UpString(cCurrentChar);
      if CompareStrings(cCurrentChar, ';#UOEXT.UPDATESERVER') then Begin
        SetShardSetup(PAnsiChar(cCurrentChar) + 21);
        Break;
      End;
    End Else if (cCurrentChar^ = ';') then Begin
      repeat
        cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 1);
      until cCurrentChar^ = #10;
    End;
    cCurrentChar := PAnsiChar(Cardinal(cCurrentChar) + 1);
  until Cardinal(cCurrentChar) >= (Cardinal(fContent) + fCLength);
  FreeMemory(fContent);
End;
{$ENDIF}


procedure TryReadLoginCfg;
Begin
  {$IFDEF LOGIN_CFG}
  if ShardSetup.ReadLoginCfg then ReadLoginCfg;
  {$ENDIF}
End;

end.
