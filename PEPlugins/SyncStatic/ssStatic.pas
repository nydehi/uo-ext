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
  TMapDimension = record
    Width: Word;
    Height: Word;
  end;

  TMapDescription = packed record
    MulFile: TIndexedMul;
    CRCHandle: THandle;
    IdxLastData: Cardinal;
    MulLength: Cardinal;
    Dimension: TMapDimension;
  end;
  TMapDescriptions = Array [0..0] of TMapDescription;
var
  Maps: ^TMapDescriptions;
  MapsAmount: Byte;
  CliX, CliY: Word;
  CliDirection: Byte;
  CliZ: ShortInt;

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
  PWord(Cardinal(@UpdateScreenPacket01) + 2)^ := CliX;
  PWord(Cardinal(@UpdateScreenPacket01) + 4)^ := CliY;
  PByte(Cardinal(@UpdateScreenPacket01) + 6)^ := CliDirection;
  PShortInt(Cardinal(@UpdateScreenPacket01) + 7)^ := CliZ;
  API.SendPacket(@UpdateScreenPacket01, 8, False, isValid);
  API.SendPacket(@UpdateScreenPacket02, 3, False, isValid);
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
    SetFilePointer(Maps^[Map].CRCHandle, Updates[i] * 4, nil, FILE_BEGIN);
    ReadFile(Maps^[Map].CRCHandle, CRCVal, 4, Readed, nil);
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
  CliX := OldPosition.X + Delta.X;
  CliY := OldPosition.Y + Delta.Y;
  CliDirection := Delta.Direction;
  CliZ := OldPosition.Z + Delta.Z;
  NewRect.Left := (OldPosition.X + Delta.X - UpdateRange + 7) DIV 8;
  NewRect.Top := (OldPosition.Y + Delta.Y - UpdateRange + 7) DIV 8;
  NewRect.Right := (OldPosition.X + Delta.X + UpdateRange + 7) DIV 8;
  NewRect.Bottom := (OldPosition.Y + Delta.Y + UpdateRange + 7) DIV 8;

  if OldPosition.Map <> Delta.Map then Begin
    UpdatePos := 0;
    for x := NewRect.Left to NewRect.Right do Begin
      for y := NewRect.Top to NewRect.Bottom do Begin
        Updates[UpdatePos] := (Maps^[Delta.Map].Dimension.Height DIV 8) * X + Y;
        UpdatePos := UpdatePos + 1;
      End;
    End;
  End Else Begin
    ZeroMemory(@OldRect, SizeOf(TUpdateRect));
    if (OldPosition.X - UpdateRange - 7) > 0  then
      OldRect.Left := (OldPosition.X - UpdateRange + 7) DIV 8;
    if (OldPosition.Y - UpdateRange - 7) > 0  then
      OldRect.Top := (OldPosition.Y - UpdateRange + 7) DIV 8;

    if (OldPosition.X + UpdateRange + 7) > 0 then
      OldRect.Right := (OldPosition.X + UpdateRange + 7) DIV 8;
    if (OldPosition.Y + UpdateRange + 7) > 0 then
      OldRect.Bottom := (OldPosition.Y + UpdateRange + 7) DIV 8;

    if (OldRect.Left = NewRect.Left) AND (OldRect.Right = NewRect.Right) AND (OldRect.Top = NewRect.Top) AND (OldRect.Bottom = NewRect.Bottom) then Exit;

    UpdatePos := 0;
    For X := NewRect.Left to NewRect.Right Do Begin
      if (OldRect.Left > x) OR (OldRect.Right < x) then Begin
        For Y := NewRect.Top to NewRect.Bottom Do Begin
          Updates[UpdatePos] := (Maps^[Delta.Map].Dimension.Height DIV 8) * X + Y;
          UpdatePos := UpdatePos + 1;
        End;
      End Else For Y := NewRect.Top to NewRect.Bottom Do Begin
        if (OldRect.Top > y) OR (OldRect.Bottom < y) then Begin
          Updates[UpdatePos] := (Maps^[Delta.Map].Dimension.Height DIV 8) * X + Y;
          UpdatePos := UpdatePos + 1;
        End;
      End;
    End;
  End;

  SendUpdatePacket(Updates, UpdatePos, Delta.Map);
End;

function Packet_0C_OnEditTileData(Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean; stdcall;
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
  CRCVal := SwapBytes4(PCardinal(Cardinal(Data) + 12)^);
  NeedUpdate := PByte(Cardinal(Data) + 16)^;
  ChunkSize := SwapBytes4(PCardinal(Cardinal(Data) + 17)^);
  if ChunkSize = $FFFFFFFF then Begin
    Maps^[Map].MulFile.SetDataBlock(Idx, ChunkSize, Extra, nil);
    CRCVal := $FFFFFFFF;
  End Else Begin
    destLen := ChunkSize;
    Decompressed := GetMemory(destLen);
    UnCompressResult := API.zLibUncompress(Decompressed, @destLen, Pointer(Cardinal(Data) + 21), Size - 21);
    if UnCompressResult <> 0 then Begin
      UnCompressResult := API.zLibUncompress(Decompressed, @destLen, Pointer(Cardinal(Data) + 21), Size - 21);
      if UnCompressResult <> 0 then Begin
        WriteLn('UnCompress returned ', UnCompressResult);
        FreeMemory(Decompressed);
        Exit;
      End;
    End;
    if destLen <> ChunkSize then Begin
      WriteLn('UnCompress returned mismatch data size');
      FreeMemory(Decompressed);
      Exit;
    End;
    Maps^[Map].MulFile.SetDataBlock(Idx, ChunkSize, Extra, Decompressed);
    FreeMemory(Decompressed);
  End;
  SetFilePointer(Maps^[Map].CRCHandle, Idx * 4, nil, FILE_BEGIN);
  WriteFile(Maps^[Map].CRCHandle, CRCVal, 4, Extra, nil);

  If NeedUpdate > 0 Then UpdateScreen;
End;

procedure FixFileLength(hFile:THandle; newSize: Cardinal; fillPattern: Cardinal);
var
  oldSize: Cardinal;
  written: Cardinal;
Begin
  SetFilePointer(hFile, 0, nil, FILE_END);
  oldSize := SetFilePointer(hFile, 0, nil, FILE_CURRENT);
  if oldSize > newSize then Begin
    SetFilePointer(hFile, newSize, nil, FILE_BEGIN);
    SetEndOfFile(hFile);
  End Else if newSize > oldSize then Begin
    while oldSize < newSize do Begin
      WriteFile(hFile, fillPattern, SizeOf(fillPattern), written, nil);
      oldSize := oldSize + SizeOf(fillPattern);
    End;
  End;
End;

procedure OnInit;
var
  i: Byte;
  j, MaxSize: Cardinal;
  CRCFile: AnsiString;
  CliInfo: PClientInformation;
  MapsInfo: PCli_Map_Info_Data;
Begin
  AddOnPositionChanged(@OnPositionChanges, nil);
  AskForMulMapping;

  MapsInfo := nil;
  CliInfo := GetClientInformation;
  for j := 0 to CliInfo^.Amount - 1 do Begin
    if CliInfo^.Information[j].Parameter = CLI_MAPS_INFO then Begin
      MapsInfo := CliInfo^.Information[j].Data;
      MapsAmount := MapsInfo^.Amount;
      Maps := GetMemory(MapsAmount * SizeOf(TMapDescriptions));
      Break;
    End;
  End;

  For i := 0 to MapsAmount - 1 do Begin
    CRCFile := ExtractFilePath(AnsiString(ParamStr(0)))+ 'Statics'+ IntToStr(i)+'.crc'+#0;
    Maps^[i].CRCHandle := CreateFileA(@CRCFile[1], GENERIC_READ XOR GENERIC_WRITE, FILE_SHARE_READ XOR FILE_SHARE_WRITE, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL XOR FILE_FLAG_RANDOM_ACCESS, 0);

    Maps^[i].MulFile := TIndexedMul.Create;
    Maps^[i].MulFile.OpenFilePair(ExtractFilePath(AnsiString(ParamStr(0)))+ 'Staidx'+ IntToStr(i)+'.mul', ExtractFilePath(AnsiString(ParamStr(0)))+ 'Statics'+ IntToStr(i)+'.mul');
    MaxSize := Maps^[i].MulFile.MulLength - Maps^[i].MulFile.FreeMulOffset;
    FixFileLength(Maps^[i].CRCHandle, Maps^[i].MulFile.IndexCount * 4, $FFFFFFFF);

    Maps^[i].MulFile.CloseFilePair;
    if MaxSize > 8*1024*1024 then Begin
      MaxSize := 0;
    End Else Begin
      MaxSize := 8*1024*1024 - MaxSize;
    End;

    if MaxSize > 0 then Begin
      CRCFile := 'Statics' + IntToStr(i) + '.mul' + #0;
      EnshureFreeMappedSpace(@CRCFile[1], MaxSize);
    End;

    Maps^[i].Dimension.Width := MapsInfo.Maps[i].Width;
    Maps^[i].Dimension.Height := MapsInfo.Maps[i].Height;
  End;

End;

procedure OnClear;
var
  i:Byte;
Begin
  For i := 0 to MapsAmount - 1 Do Begin
    CloseHandle(Maps^[i].CRCHandle);
    if Assigned(Maps^[i].MulFile) then Maps^[i].MulFile.Free;
  End;
  FreeMemory(Maps);
End;

procedure OnStart;
var
  i: Byte;
  Mapping: PMappingRec;
Begin
  API.RegisterPacketHandler($0C, @Packet_0C_OnEditTileData);
  For i := 0 to MapsAmount - 1 Do Begin
    Mapping := GetMulMappingInfo(PAnsiChar('Statics'+Inttostr(i)+'.mul'));
    Maps^[i].MulFile.MulMapping := Mapping.MappingHandle;
    Maps^[i].MulFile.MulPointer := Mapping.MappingPointer;
    Maps^[i].MulFile.MulLength := Mapping.MappingLength;
    Mapping := GetMulMappingInfo(PAnsiChar('Staidx'+Inttostr(i)+'.mul'));
    Maps^[i].MulFile.IndexMapping := Mapping.MappingHandle;
    Maps^[i].MulFile.IndexPointer := Mapping.MappingPointer;
    Maps^[i].MulFile.IndexLength := Mapping.MappingLength;
    Maps^[i].MulFile.MulParametersChanged;
  End;
End;

procedure OnEnd;
Begin
  API.UnRegisterPacketHandler($0C, @Packet_0C_OnEditTileData);
End;

end.
