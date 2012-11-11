unit ssStatic;

interface

procedure UpdateScreen;
procedure OnInit;
procedure OnClear;
procedure OnStart;
procedure OnEnd;

implementation

uses plgSyncStatic, ssShared, UOExt.Utility.Bindings, MulWork, Common, Windows;

type
  TCRCHandles = Array [0..0] of THandle;
  TMulFiles = Array [0..0] of TIndexedMul;
  TMapDimension = record
    Width: Word;
    Height: Word;
  end;
  TMapDimensions = Array [0..0] of TMapDimension;
var
  CRCtable: array[0..255] of cardinal;
  CRCHandles: ^TCRCHandles;
  MulFiles: ^TMulFiles;
  MapDimensions: ^TMapDimensions;
  Maps: Byte;

procedure CRCInit;
var
  c: cardinal;
  i, j: integer;
begin
  for i := 0 to 255 do
  begin
    c := i;
    for j := 8 downto 1 do
      if odd(c) then
        c := (c shr 1) xor $EDB88320
      else
        c := (c shr 1);
    CRCtable[i] := c;
  end;
end;

function CRC(OldCRC: cardinal; StPtr: pointer; StLen: integer): cardinal;
asm
  test edx,edx;
  jz @ret;
  neg ecx;
  jz @ret;
  sub edx,ecx; // Address after last element

  push ebx;
  mov ebx,0; // Set ebx=0 & align @next
@next:
  mov bl,al;
  xor bl,byte [edx+ecx];
  shr eax,8;
  xor eax,cardinal [CRCtable+ebx*4];
  inc ecx;
  jnz @next;
  pop ebx;

@ret:
end;

procedure UpdateScreen;
const
  UpdateScreenPacket01: Array [0..7] of Byte = (
    $21, $0, $0, $0, $0, $0, $0, $0
  );
  UpdateScreenPacket02: Array [0..2] of Byte = (
    $22, $0, $0
  );
var
  isValid: Boolean;
Begin
  Sleep(100);
  API.SendPacket(@UpdateScreenPacket01, 8, False, isValid);
  API.SendPacket(@UpdateScreenPacket02, 3, False, isValid);
End;

function CheckCRCFiles:Byte;
var
  Mul: TIndexedMul;
  Path, CRCFile: AnsiString;
  i: Byte;
  Idx: Cardinal;
  F: THandle;
  BlockSize: Cardinal;
  Block: Pointer;
  CRCValue: Cardinal;

  Root, Task, Progress: Cardinal;
  Writed: Cardinal;
  LastProgress: Cardinal;
Begin
  Root := API.GUISetLog($FFFFFFFF, $FFFFFFFF, 'Generating crc files ...');
  Task := $FFFFFFFF;
  Progress := $FFFFFFFF;
  CRCInit;
  Path := ExtractFilePath(AnsiString(ParamStr(0)));
  Result := 0;
  For i := 0 to 255 do If FileExists(Path+'Statics'+IntToStr(i)+'.mul') then Begin
    Result := i;
    Task := API.GUISetLog(Task, Root, PAnsiChar('Processing Statics'+IntToStr(i)+ '.mul'));
    Mul := TIndexedMul.Create;
    Mul.OpenFilePair(Path+'Staidx'+IntToStr(i)+'.mul', Path+'Statics'+IntToStr(i)+'.mul');
    if Progress = $FFFFFFFF then Progress := API.GUIStartProcess($FFFFFFFF, Task, 'Working', 0, 100, 0);
    LastProgress := 0;
    API.GUIUpdateProcess(Progress, 0, Mul.MulLength, 0);

    CRCFile := Path + 'Statics'+IntToStr(i)+'.crc' + #0;
    F := CreateFileA(@CRCFile[1], GENERIC_READ XOR GENERIC_WRITE, FILE_SHARE_READ XOR FILE_SHARE_WRITE, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL XOR FILE_FLAG_SEQUENTIAL_SCAN, 0);
    For Idx := 0 to Mul.IndexCount - 1 do Begin
      CRCValue := $FFFFFFFF;
      Block := Mul.GetDataBlock(Idx, BlockSize, nil);
      if Block <> nil then Begin
        CRCValue := CRC($FFFFFFFF, Block, BlockSize);
      End;
      WriteFile(F, CRCValue, 4, Writed, nil);
      if Writed <> 4 then Halt(1);

      if ((Idx * 100) DIV Mul.IndexCount) <> LastProgress then Begin
          LastProgress := (Idx * 100) DIV Mul.IndexCount;
          API.GUIUpdateProcess(Progress, 0, 100, LastProgress);
      End;
    End;
    CloseHandle(F);
    Mul.Free;
  End;
End;

procedure SendUpdatePacket(Updates: Array of Cardinal; Amount, Map: Byte);
var
  UpdatePacket: Pointer;
  I: Byte;
  CRCVal, Readed: Cardinal;
  IsValid:Boolean;
Begin
  if Amount = 0 then Exit;
  UpdatePacket := GetMemory(Amount * 8 + 4);
  PByte(UpdatePacket)^ := $0C; //Edit TileData from God Client.
  PWord(Cardinal(UpdatePacket) + 1)^ := SwapBytes2(Amount * 8 + 4);
  PByte(Cardinal(UpdatePacket) + 3)^ := Map;
  for i := 0 to Amount - 1 do Begin
    PCardinal(Cardinal(UpdatePacket) + 4 + i*8)^ := SwapBytes4(Updates[i]);
    SetFilePointer(CRCHandles[Map], Updates[i] * 4, nil, FILE_BEGIN);
    ReadFile(CRCHandles[Map], CRCVal, 4, Readed, nil);
    PCardinal(Cardinal(UpdatePacket) + 8 + i*8)^ := SwapBytes4(CRCVal);

  End;
  API.SendPacket(UpdatePacket, Amount * 8 + 4, True, IsValid);
End;

procedure OnPositionChanges(OldPosition: PClientPosition; Delta: PClientPositionDelta; AParam: Pointer); stdcall;
type
  TUpdateRect = record
    Left: Word;
    Top: Word;
    Right: Word;
    Bottom: Word;
  end;
const
  UpdateRange = 18;
  MaxUpdates = 65;
var
  X, Y: Word;
  OldRect, NewRect: TUpdateRect;
  Updates: Array [0..MaxUpdates] Of Cardinal;
  UpdatePos: Byte;
Begin
  NewRect.Left := (OldPosition.X + Delta.X - UpdateRange - 7) DIV 8;
  NewRect.Top := (OldPosition.Y + Delta.Y - UpdateRange - 7) DIV 8;
  NewRect.Right := (OldPosition.X + Delta.X + UpdateRange + 7) DIV 8;
  NewRect.Bottom := (OldPosition.Y + Delta.Y + UpdateRange + 7) DIV 8;

  if OldPosition.Map <> Delta.Map then Begin
    UpdatePos := 0;
    for x := NewRect.Left to NewRect.Right do Begin
      for y := NewRect.Top to NewRect.Bottom do Begin
        Updates[UpdatePos] := (MapDimensions[Delta.Map].Height DIV 8) * X + Y;
        UpdatePos := UpdatePos + 1;
      End;
    End;
  End Else Begin
    ZeroMemory(@OldRect, SizeOf(TUpdateRect));
    if (OldPosition.X - UpdateRange - 7) > 0  then
      OldRect.Left := (OldPosition.X - UpdateRange - 7) DIV 8;
    if (OldPosition.Y - UpdateRange - 7) > 0  then
      OldRect.Top := (OldPosition.Y - UpdateRange - 7) DIV 8;

    if (OldPosition.X + UpdateRange + 7) > 0 then
      OldRect.Right := (OldPosition.X + UpdateRange + 7) DIV 8;
    if (OldPosition.Y + UpdateRange + 7) > 0 then
      OldRect.Bottom := (OldPosition.Y + UpdateRange + 7) DIV 8;

    if (OldRect.Left = NewRect.Left) AND(OldRect.Right = NewRect.Right) AND (OldRect.Top = NewRect.Top) AND (OldRect.Bottom = NewRect.Bottom) then Exit;


    UpdatePos := 0;
    for x := NewRect.Left to NewRect.Right do Begin
      if (OldRect.Left >= x) OR (OldRect.Right <= x) then for y := NewRect.Top to NewRect.Bottom do Begin
        if (OldRect.Top >= y) OR (OldRect.Bottom <= y) then Begin
          Updates[UpdatePos] := (MapDimensions[Delta.Map].Height DIV 8) * X + Y;
          UpdatePos := UpdatePos + 1;

        End;
      End;
    End;
  End;

  SendUpdatePacket(Updates, UpdatePos, Delta.Map);
End;

function OnEditTileData(Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean; stdcall;
var
  Decompressed: Pointer;
  Idx, ChunkSize: Cardinal;
  Map, NeedUpdate: Byte;
  Extra:Cardinal;
  CRCVal: Cardinal;
  destLen: Integer;
  UnCompressResult: Integer;
Begin
  Result := False;
  if not IsFromServerToClient then Exit;
  Result := True;
  Send := False;

  Idx := SwapBytes4(PCardinal(Cardinal(Data) + 3)^);
  Map := PByte(Cardinal(Data) + 7)^;
  Extra := SwapBytes4(PCardinal(Cardinal(Data) + 8)^);
  NeedUpdate := PByte(Cardinal(Data) + 12)^;
  ChunkSize := SwapBytes4(PCardinal(Cardinal(Data) + 13)^);
  if ChunkSize = $FFFFFFFF then Begin
    MulFiles[Map].SetDataBlock(Idx, ChunkSize, Extra, nil);
    CRCVal := $FFFFFFFF;
  End Else Begin
    destLen := ChunkSize;
    if destLen < (Size - 17) then destLen := ChunkSize;
    Decompressed := GetMemory(destLen);
    UnCompressResult := API.zLibUncompress(Decompressed, @destLen, Pointer(Cardinal(Data) + 17), Size - 17);
    if UnCompressResult <> 0 then Begin
      WriteLn('UnCompress returned ', UnCompressResult);
      FreeMemory(Decompressed);
      Exit;
    End;
    MulFiles[Map].SetDataBlock(Idx, ChunkSize, Extra, Decompressed);
    FreeMemory(Decompressed);
    CRCVal := CRC($FFFFFFFF, Decompressed, ChunkSize);
  End;
  SetFilePointer(CRCHandles[Map], Idx * 4, nil, FILE_BEGIN);
  WriteFile(CRCHandles[Map], CRCVal, 4, Extra, nil);

  If NeedUpdate > 0 Then UpdateScreen;
End;


procedure OnInit;
var
  i: Byte;
  j: Cardinal;
  CRCFile: AnsiString;
  CliInfo: PClientInformation;
  MapsInfo: PCli_Map_Info_Data;
Begin
  AddOnPositionChanged(@OnPositionChanges, nil);
  Maps := CheckCRCFiles;
  AskForMulMapping;
  CRCHandles := GetMemory(Maps * SizeOf(THandle));
  for i := 0 to Maps - 1 do Begin
    CRCFile := ExtractFilePath(AnsiString(ParamStr(0)))+ 'Statics'+ IntToStr(i)+'.crc'+#0;
    CRCHandles^[i] := CreateFileA(@CRCFile[1], GENERIC_READ XOR GENERIC_WRITE, FILE_SHARE_READ XOR FILE_SHARE_WRITE, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL XOR FILE_FLAG_SEQUENTIAL_SCAN, 0);
  End;
  CliInfo := GetClientInformation;
  MapsInfo := nil;
  for j := 0 to CliInfo^.Amount - 1 do Begin
    if CliInfo^.Information[j].Parameter = CLI_MAPS_INFO then Begin
      MapsInfo := CliInfo^.Information[j].Data;
      Break;
    End;
  End;

  MapDimensions := GetMemory(SizeOf(TMapDimension)*Maps);
  MulFiles := GetMemory(SizeOf(TMulFiles)*Maps);
  for i := 0 to MapsInfo.Amount - 1 do if i < Maps then Begin
      MapDimensions[i].Width := MapsInfo.Maps[i].Width;
      MapDimensions[i].Height := MapsInfo.Maps[i].Height;
      MulFiles[i] := TIndexedMul.Create;
  End Else Break;
End;

procedure OnClear;
var
  i:Byte;
Begin
  For i := 0 to Maps - 1 Do CloseHandle(CRCHandles^[i]);
  FreeMemory(CRCHandles);
  FreeMemory(MapDimensions);
End;

procedure OnStart;
var
  i: Byte;
  Mapping: PMappingRec;
Begin
  API.RegisterPacketHandler($0C, @OnEditTileData);
  For i := 0 to Maps - 1 Do Begin
    Mapping := GetMulMappingInfo(PAnsiChar('Statics'+Inttostr(i)+'.mul'));
    MulFiles[i].MulMapping := Mapping.MappingHandle;
    MulFiles[i].MulPointer := Mapping.MappingPointer;
    MulFiles[i].MulLength := Mapping.MappingLength;
    Mapping := GetMulMappingInfo(PAnsiChar('Staidx'+Inttostr(i)+'.mul'));
    MulFiles[i].IndexMapping := Mapping.MappingHandle;
    MulFiles[i].IndexPointer := Mapping.MappingPointer;
    MulFiles[i].IndexLength := Mapping.MappingLength;
    MulFiles[i].MulParametersChanged;
  End;
End;

procedure OnEnd;
Begin
  API.UnRegisterPacketHandler($0C, @OnEditTileData);
End;

end.
