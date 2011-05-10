unit Serials;

interface

function GetNewSerial(IsMobile:Boolean):Cardinal; stdcall;
procedure FreeSerial(Serial: Cardinal); stdcall;
function GetServerSerial(Serial:Cardinal):Cardinal; stdcall;
function GetClientSerial(Serial:Cardinal):Cardinal; stdcall;

implementation

uses Windows, ShardSetup;

const
  SERIALS_PER_PAGE = 20;
  INVALID_SERIAL: Cardinal = $FFFFFFFF;
type
  PFreeSerialsPage=^TFreeSerialspage;
  TFreeSerialsPage=record
    FreeSerials: Array [0..SERIALS_PER_PAGE] of Cardinal;
    NextPage: PFreeSerialsPage;
  end;

var
  FreeItemSerials: PFreeSerialsPage;
  NextFreeItemSerial: Cardinal;

  FreeMobileSerials: PFreeSerialsPage;
  NextFreeMobileSerial: Cardinal;

// local functions

function GetServerSerial(Serial:Cardinal):Cardinal;
begin
  Result := Serial;
end;

function GetClientSerial(Serial:Cardinal):Cardinal;
begin
  Result := Serial;
end;

procedure FreeMobileSerial(MobileSerial: Cardinal);
var
  cPage: PFreeSerialsPage;
  i: Cardinal;
Begin
  if MobileSerial = (NextFreeMobileSerial - 1) then begin
    NextFreeMobileSerial := NextFreeMobileSerial - 1;
    Exit;
  end;
  cPage := FreeItemSerials;
  if cPage = nil then begin
    cPage := GetMemory(SizeOf(TFreeSerialsPage));
    ZeroMemory(cPage, SizeOf(TFreeSerialsPage));
    For i:=0 to SERIALS_PER_PAGE do cPage^.FreeSerials[i] := INVALID_SERIAL;
    cPage^.FreeSerials[0] := MobileSerial;
    FreeMobileSerials := cPage;
  end else Begin
    repeat
      for i := 0 to SERIALS_PER_PAGE do if cPage^.FreeSerials[i] = INVALID_SERIAL then Begin
        cPage^.FreeSerials[i] := MobileSerial;
        Exit;
      end;
      if cPage^.NextPage = nil then Break;
      cPage := cPage^.NextPage;
    until cPage = nil;
    cPage^.NextPage := GetMemory(SizeOf(TFreeSerialsPage));
    ZeroMemory(cPage^.NextPage, SizeOf(TFreeSerialsPage));
    For i:=0 to SERIALS_PER_PAGE do cPage^.NextPage^.FreeSerials[i] := INVALID_SERIAL;
    cPage^.NextPage^.FreeSerials[0] := MobileSerial;
  End;
End;

procedure FreeItemSerial(ItemSerial: Cardinal);
var
  cPage: PFreeSerialsPage;
  i: Cardinal;
Begin
  if ItemSerial = (NextFreeItemSerial - 1) then Begin
    NextFreeItemSerial := NextFreeItemSerial - 1;
    Exit;
  End;
  cPage := FreeItemSerials;
  if cPage = nil then begin
    cPage := GetMemory(SizeOf(TFreeSerialsPage));
    ZeroMemory(cPage, SizeOf(TFreeSerialsPage));
    For i:=0 to SERIALS_PER_PAGE do cPage^.FreeSerials[i] := INVALID_SERIAL;
    cPage^.FreeSerials[0] := ItemSerial;
    FreeItemSerials := cPage;
  end else Begin
    repeat
      for i := 0 to SERIALS_PER_PAGE do if cPage^.FreeSerials[i] = INVALID_SERIAL then Begin
        cPage^.FreeSerials[i] := ItemSerial;
        Exit;
      end;
      if cPage^.NextPage = nil then Break;
      cPage := cPage^.NextPage;
    until cPage = nil;
    cPage^.NextPage := GetMemory(SizeOf(TFreeSerialsPage));
    ZeroMemory(cPage^.NextPage, SizeOf(TFreeSerialsPage));
    For i:=0 to SERIALS_PER_PAGE do cPage^.NextPage^.FreeSerials[i] := INVALID_SERIAL;
    cPage^.NextPage^.FreeSerials[0] := ItemSerial;
  End;
End;

procedure FreeSerial(Serial: Cardinal);
begin
  If Serial < $40000000 Then
    FreeMobileSerial(Serial)
  Else
    FreeItemSerial(Serial);
end;

function GetNewSerial(IsMobile:Boolean):Cardinal;
var
  i: Cardinal;
  cPage:PFreeSerialsPage;
Begin
  If IsMobile Then Begin
    If FreeMobileSerials = nil Then Begin
      Result := NextFreeMobileSerial;
      NextFreeMobileSerial := NextFreeMobileSerial + 1;
      If NextFreeMobileSerial > ShardSetup.MobileSerialMax Then Begin
        {$IFDEF DEBUG}
        WriteLn('All free local mobile serials in use.');
        {$ELSE}
        MessageBox(0, 'All free local mobile serials in use.', nil, MB_OK);
        {$ENDIF}
        Halt(1);
      End;
    End Else Begin
      Repeat
        For i:=0 to SERIALS_PER_PAGE do If FreeMobileSerials^.FreeSerials[i] <> INVALID_SERIAL Then Begin
          Result := FreeMobileSerials^.FreeSerials[i];
          FreeMobileSerials^.FreeSerials[i] := INVALID_SERIAL;
          Exit;
        End;
        cPage := FreeMobileSerials;
        FreeMobileSerials := FreeMobileSerials^.NextPage;
        FreeMemory(cPage);
      Until FreeMobileSerials = nil;
      Result := GetNewSerial(IsMobile);
    End;
  End Else Begin
    If FreeItemSerials = nil Then Begin
      Result := NextFreeItemSerial;
      NextFreeItemSerial := NextFreeItemSerial + 1;
      If NextFreeItemSerial > ShardSetup.ItemSerialMax Then Begin
        {$IFDEF DEBUG}
        WriteLn('All free local item serials in use.');
        {$ELSE}
        MessageBox(0, 'All free local item serials in use.', nil, MB_OK);
        {$ENDIF}
        Halt(1);
      End;
    End Else Begin
      Repeat
        For i:=0 to SERIALS_PER_PAGE do If FreeItemSerials^.FreeSerials[i] <> INVALID_SERIAL Then Begin
          Result := FreeItemSerials^.FreeSerials[i];
          FreeItemSerials^.FreeSerials[i] := INVALID_SERIAL;
          Exit;
        End;
        cPage := FreeItemSerials;
        FreeItemSerials := FreeItemSerials^.NextPage;
        FreeMemory(cPage);
      Until FreeItemSerials = nil;
      Result := GetNewSerial(IsMobile);
    End;
  End;
End;

initialization
  If (SerialSupplyMethod = ssmProxy) or (SerialSupplyMethod = ssmServer) Then Begin
  {$IFDEF DEBUG}
    Writeln('Proxy or Server method of serial supply not implemented');
  {$ELSE}
    MessageBox(0, 'Proxy or Server method of serial supply not implemented', nil, MB_OK);
  {$ENDIF}
    Halt(1);
  End Else Begin
    FreeItemSerials := nil;
    FreeMobileSerials := nil;
    NextFreeItemSerial := ShardSetup.ItemSerialMin;
    NextFreeMobileSerial := ShardSetup.MobileSerialMin;
  End;
end.
