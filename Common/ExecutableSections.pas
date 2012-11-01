unit ExecutableSections;

interface

uses Windows;

function IsAddressFromExecutable(AnAddress: Cardinal): Boolean;

implementation

type
  RMemoryRange = record
    Start: Cardinal;
    Length: Cardinal;
  end;
  TSectionsRanges = record
    Count: Cardinal;
    Items: Array [0..0] of RMemoryRange;
  end;
  PSectionsRanges = ^TSectionsRanges;
var
  ExeSections: PSectionsRanges;
  RazorCryptSections: PSectionsRanges;
  HasRazor: Boolean;
  Lock: TRTLCriticalSection;

function ReadSections(aModule: THandle): PSectionsRanges;
type
  TSections = Array [0..999] of TImageSectionHeader;
  PSections = ^TSections;
var
  pDOS: PImageDosHeader;
  pPE: PImageFileHeader;
  pOpt: PImageOptionalHeader;
  pSect: PSections;
  i, j, cSections, cSectionStart: Cardinal;
Begin
  pDOS := Pointer(aModule);
  pPE := Pointer(Cardinal(pDOS) + Cardinal(pDOS^._lfanew) + 4);
  pOpt := Pointer(Cardinal(pPE) + SizeOf(TImageFileHeader));
  pSect := Pointer(Cardinal(pOpt) + SizeOf(TImageOptionalHeader) + (pOpt^.NumberOfRvaAndSizes - IMAGE_NUMBEROF_DIRECTORY_ENTRIES) * SizeOf(TImageDataDirectory));

  cSections := 0;
  For i := 0 to pPE^.NumberOfSections - 1 do Begin
    If pSect^[i].Characteristics AND IMAGE_SCN_MEM_EXECUTE = IMAGE_SCN_MEM_EXECUTE Then Begin
      cSections := cSections + 1;
    End;
  End;

  Result := GetMemory(SizeOf(TSectionsRanges) + (cSections - 1) * SizeOf(RMemoryRange));
  Result^.Count := cSections;
  j := 0;
  For i := 0 to pPE^.NumberOfSections - 1 do Begin
    If pSect^[i].Characteristics AND IMAGE_SCN_MEM_EXECUTE = IMAGE_SCN_MEM_EXECUTE Then Begin
      cSectionStart := Cardinal(pDOS) + pSect^[i].VirtualAddress{ + pOpt^.ImageBase};
      Result^.Items[j].Start := cSectionStart;
      Result^.Items[j].Length := pSect^[i].Misc.VirtualSize;
      j := j + 1;
    End;
  End;
End;

procedure ReadAllSections;
Begin
  EnterCriticalSection(Lock);
  if ExeSections = nil then Begin
    ExeSections := ReadSections(GetModuleHandleA(nil));
    HasRazor := GetModuleHandleA('Crypt.dll') <> 0;
    if HasRazor then RazorCryptSections := ReadSections(GetModuleHandleA('Crypt.dll'));
  End;
  LeaveCriticalSection(Lock);
End;

function IsAddressFromExecutable(AnAddress: Cardinal): Boolean;
var
  i: Cardinal;
Begin
  Result := False;
  if ExeSections = nil then ReadAllSections;
  For i := 0 to ExeSections^.Count - 1 do if (ExeSections^.Items[i].Start <= AnAddress) AND ((ExeSections^.Items[i].Length + ExeSections^.Items[i].Start) >= AnAddress) then Begin
    Result := True;
    Exit;
  End;
  if HasRazor then for i := 0 to RazorCryptSections^.Count - 1 do if (RazorCryptSections^.Items[i].Start <= AnAddress) AND ((RazorCryptSections^.Items[i].Length + RazorCryptSections^.Items[i].Start) >= AnAddress) then Begin
    Result := True;
    Exit;
  End;
End;

initialization
  ExeSections := nil;
  InitializeCriticalSection(Lock);
finalization
  DeleteCriticalSection(Lock);
end.
