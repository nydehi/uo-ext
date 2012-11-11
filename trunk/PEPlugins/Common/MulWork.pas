unit MulWork;

interface

uses Windows;

type
  TOnIndexedMulDefragmentationProcess = procedure(Current: Cardinal; Max: Cardinal; var Stop: Boolean);

  TIndexedMul=class
  private type
    TIdxRecord = packed record
      Lookup: Cardinal;
      Size: Cardinal;
      Extra: Cardinal;
    end;
    TIdxRecords = Array [0..0] of TIdxRecord;
    PIdxRecord = ^TIdxRecord;
  private
    FIndexMapping: THandle;
    FIndexPointer: Pointer;
    FIndexLength: Cardinal;
    FMulMapping: THandle;
    FMulPointer: Pointer;
    FMulLength: Cardinal;

    FFreeMulOffset: Cardinal;

    FIndexCount: Cardinal;

    FFreeOnDestroy: Boolean;

    procedure SetIndexLength(AValue:Cardinal);
  public
    property IndexMapping: THandle read FIndexMapping write FIndexMapping;
    property IndexPointer: Pointer read FIndexPointer write FIndexPointer;
    property IndexLength: Cardinal read FIndexLength write SetIndexLength;
    property IndexCount: Cardinal read FIndexCount;

    property MulMapping: THandle read FMulMapping write FMulMapping;
    property MulPointer: Pointer read FMulPointer write FMulPointer;
    property MulLength: Cardinal read FMulLength write FMulLength;

    procedure MulParametersChanged;
    procedure OpenFilePair(AIndexName, AMulName: AnsiString);
    procedure CloseFilePair;

    function GetDataBlock(Index: Cardinal; var Size: Cardinal; Extra: PCardinal): Pointer;
    function SetDataBlock(Index, Size:Cardinal; Extra: Cardinal; Data: Pointer):Boolean;

//    procedure Defragment(OnProcess: TOnIndexedMulDefragmentationProcess);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

type
  TIdxRec=packed record
    Lookup: Cardinal;
    Size: Cardinal;
    Extra: Cardinal;
  end;

constructor TIndexedMul.Create;
Begin
  Inherited;
  FIndexMapping := 0;
  FIndexPointer := nil;
  FIndexLength := 0;
  FMulMapping := 0;
  FMulPointer := nil;
  FMulLength := 0;
  FIndexCount := 0;
  FFreeMulOffset := 0;
  FFreeOnDestroy := False;
End;

destructor TIndexedMul.Destroy;
begin
  if FFreeOnDestroy then CloseFilePair;
  Inherited;
end;

procedure TIndexedMul.SetIndexLength(AValue: Cardinal);
begin
  if AValue = FIndexLength then Exit;
  FIndexLength := AValue;
  FIndexCount := FIndexLength DIV 12;
end;

procedure TIndexedMul.OpenFilePair(AIndexName: AnsiString; AMulName: AnsiString);
var
  hFile: THandle;
begin
  if FFreeOnDestroy then CloseFilePair;
  AIndexName := AIndexName + #0;
  hFile := CreateFileA(@AIndexName[1], GENERIC_READ XOR GENERIC_WRITE, FILE_SHARE_READ XOR FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  FIndexLength := GetFileSize(hFile, nil);
  FIndexMapping := CreateFileMappingA(hFile, nil, PAGE_READWRITE, 0, 0, nil);
  FIndexPointer := MapViewOfFile(FIndexMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  CloseHandle(hFile);
  hFile := CreateFileA(@AMulName[1], GENERIC_READ XOR GENERIC_WRITE, FILE_SHARE_READ XOR FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  FMulLength := GetFileSize(hFile, nil);
  FMulMapping := CreateFileMappingA(hFile, nil, PAGE_READWRITE, 0, FMulLength + 1*1024*1024, nil);
  FMulPointer := MapViewOfFile(FMulMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  CloseHandle(hFile);
  FFreeOnDestroy := True;
  FIndexCount := FIndexLength DIV 12;
  MulParametersChanged;
end;

procedure TIndexedMul.CloseFilePair;
begin
  if not FFreeOnDestroy then Exit;
  UnmapViewOfFile(FIndexPointer);
  UnmapViewOfFile(FMulPointer);
  CloseHandle(FIndexMapping);
  CloseHandle(FMulMapping);
  FFreeOnDestroy := False;
  FFreeMulOffset := 0;
  FIndexPointer := nil;
  FMulPointer := nil;
end;

procedure TIndexedMul.MulParametersChanged;
var
  i: Cardinal;
  Idx: PIdxRecord;
Begin
  FFreeMulOffset := 0;
  for i := 0 to FIndexCount - 1 do Begin
    Idx := PIdxRecord(Cardinal(FIndexPointer) + i * SizeOf(TIdxRecord));
    if (Idx.Lookup <> MAXDWORD) AND (Idx.Size <> MAXDWORD) then
      if FFreeMulOffset < (Idx^.Lookup + Idx^.Size) then FFreeMulOffset := Idx^.Lookup + Idx^.Size;
  End;
End;


function TIndexedMul.GetDataBlock(Index: Cardinal; var Size: Cardinal; Extra: PCardinal):Pointer;
var
  Idx: PIdxRecord;
begin
  Result := nil;
  if (FIndexPointer = nil)or(FMulPointer = nil) then Exit;
  Idx := Pointer(Cardinal(FIndexPointer) + Index * SizeOf(TidxRecord));
  Size := Idx.Size;
  if Extra <> nil then Extra^ := Idx.Extra;
  if (Size = MAXDWORD) OR (Idx.Lookup = MAXDWORD) then Exit;

  Result := Pointer(Cardinal(FMulPointer) + Idx.Lookup);
end;

function TIndexedMul.SetDataBlock(Index: Cardinal; Size: Cardinal; Extra: Cardinal; Data: Pointer):Boolean;
var
  CurrentIdx:PIdxRecord;
  OldLookup: Cardinal;
Begin
  Result := False;
  If not FFreeOnDestroy then Begin
    If ((FMulLength - FFreeMulOffset - 1 * 1024 * 1024) < Size)AND(Size <> $FFFFFFFF) Then Exit; // 1Mb of file reserved for defragmentation
    CurrentIdx := Pointer(Cardinal(FIndexPointer) + Index * SizeOf(TIdxRecord));
    if Size < $FFFFFFFF then
      CopyMemory(Pointer(Cardinal(FMulPointer) + FFreeMulOffset), Data, Size);
    if Size = $FFFFFFFF then Begin
      CurrentIdx.Size := Size;
      CurrentIdx.Lookup := Size;
    End Else If CurrentIdx.Size <= Size then Begin
      CurrentIdx.Lookup := FFreeMulOffset;
      CurrentIdx.Size := Size;
      FFreeMulOffset := FFreeMulOffset + Size;
    End Else Begin
      CurrentIdx.Size := Size;
      OldLookup := CurrentIdx.Lookup;
      CurrentIdx.Lookup := FFreeMulOffset;
      CopyMemory(Pointer(Cardinal(FMulPointer) + OldLookup), Data, Size);
      CurrentIdx.Lookup := OldLookup;
    End;
  End Else Begin
    CurrentIdx := Pointer(Cardinal(FIndexPointer) + Index * SizeOf(TIdxRecord));
    if CurrentIdx.Size <= Size then Begin
      If (FMulLength - FFreeMulOffset - 1 * 1024 * 1024) < Size Then Exit; // 1Mb of file reserved for defragmentation
      if Size = $FFFFFFFF then Begin
        CurrentIdx.Size := Size;
        CurrentIdx.Lookup := Size;
      End Else Begin
        CopyMemory(Pointer(Cardinal(FMulPointer) + FFreeMulOffset), Data, Size);
        CurrentIdx.Lookup := FFreeMulOffset;
        CurrentIdx.Size := Size;
        FFreeMulOffset := FFreeMulOffset + Size;
      End;
    End Else Begin
      if Size < $FFFFFFFF then
      CopyMemory(Pointer(Cardinal(FMulPointer) + CurrentIdx.Lookup), Data, Size);
      CurrentIdx.Size := Size;
    End;
  End;
  Result := True;
End;

end.
