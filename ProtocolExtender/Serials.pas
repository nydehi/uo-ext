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

procedure FreeSerial(Serial: Cardinal);
var
  cSerialsPage: PFreeSerialsPage;
  i: Cardinal;
begin
  cSerialsPage := nil;
  repeat
    If cSerialsPage = nil Then Begin
      If Serial < $40000000 Then
        cSerialsPage := FreeMobileSerials
      Else
        cSerialsPage := FreeItemSerials;
    End Else cSerialsPage := cSerialsPage^.NextPage;
    For i:= 0 to SERIALS_PER_PAGE do If cSerialsPage^.FreeSerials[i] = 0 Then Begin
      cSerialsPage^.FreeSerials[i] := Serial;
      Exit;
    End;
  Until cSerialsPage^.NextPage = nil;
  cSerialsPage^.NextPage := GetMemory(SizeOf(TFreeSerialsPage));
  ZeroMemory(cSerialsPage^.NextPage, SizeOf(TFreeSerialsPage));
  For i:=0 to SERIALS_PER_PAGE do cSerialsPage^.NextPage^.FreeSerials[i] := INVALID_SERIAL;
  cSerialsPage^.NextPage^.FreeSerials[0] := Serial;
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
      Result := 0;
      For i:=0 to SERIALS_PER_PAGE do If FreeMobileSerials^.FreeSerials[i] <> INVALID_SERIAL Then Begin
        If Result = 0 Then
          Result := FreeMobileSerials^.FreeSerials[i]
        Else
          Exit;
      End;
      cPage := FreeMobileSerials;
      FreeMobileSerials := FreeMobileSerials^.NextPage;
      FreeMemory(cPage);
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
      Result := 0;
      For i:=0 to SERIALS_PER_PAGE do If FreeItemSerials^.FreeSerials[i] <> INVALID_SERIAL Then Begin
        If Result = 0 Then
          Result := FreeItemSerials^.FreeSerials[i]
        Else
          Exit;
      End;
      cPage := FreeItemSerials;
      FreeItemSerials := FreeItemSerials^.NextPage;
      FreeMemory(cPage);
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
