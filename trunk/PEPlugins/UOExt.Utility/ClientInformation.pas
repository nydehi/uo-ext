unit ClientInformation;

interface

uses UOExt.Utility.Bindings;

function GetClientInformation: PClientInformation; stdcall;

implementation

uses Windows;

type
  TMyClientInformation=packed record
    Amount: Cardinal;
    Information: Array [0..0] of TClientInfo;
  end;

var
  MyClientInformation: ^TMyClientInformation;
  CliInfoGet: Boolean;

function SearchMapSizes:PCli_Map_Info_Data;
type
  TSections = Array [0..999] of TImageSectionHeader;
  PSections = ^TSections;
const
  CFelucca: Array [0..7] of AnsiChar = ( 'F','e','l','u','c','c','a',#0 );
  CTrammel: Array [0..7] of AnsiChar = ( 'T','r','a','m','m','e','l',#0 );
var
  pDOS: PImageDosHeader;
  pPE: PImageFileHeader;
  pOpt: PImageOptionalHeader;
  pSect: PSections;
  i, j, k, cSectionStart, Delta: Cardinal;

  Felucca, Trammel: Pointer;
  FeluccaPnt, TrammelPnt: Pointer;
  bFound : Boolean;
Begin
  Result := nil;
  Felucca := nil;
  Trammel := nil;
  FeluccaPnt := nil;
  TrammelPnt := nil;
  pDOS := Pointer(GetModuleHandleA(nil));
  pPE := Pointer(Cardinal(pDOS) + Cardinal(pDOS^._lfanew) + 4);
  pOpt := Pointer(Cardinal(pPE) + SizeOf(TImageFileHeader));
  pSect := Pointer(Cardinal(pOpt) + SizeOf(TImageOptionalHeader) + (pOpt^.NumberOfRvaAndSizes - IMAGE_NUMBEROF_DIRECTORY_ENTRIES) * SizeOf(TImageDataDirectory));

  For i := 0 to pPE^.NumberOfSections - 1 do Begin
      cSectionStart := Cardinal(pDOS) + pSect^[i].VirtualAddress{ + pOpt^.ImageBase};
      For j := cSectionStart to pSect^[i].Misc.VirtualSize + cSectionStart - 1 do Begin
        If PAnsiChar(j)^ = CFelucca[0] Then Begin
          bFound := True;
          for k := 1 to 7 do Begin
            if PAnsiChar(j+k)^ <> CFelucca[k] then Begin
              bFound := False;
              Break;
            End;
          End;
          if bFound then Begin
            Felucca := Pointer(j);
            if Trammel <> nil then Break;
          End;
        End;
        If PAnsiChar(j)^ = CTrammel[0] Then Begin
          bFound := True;
          for k := 1 to 7 do Begin
            if PAnsiChar(j+k)^ <> CTrammel[k] then Begin
              bFound := False;
              Break;
            End;
          End;
          if bFound then Begin
            Trammel := Pointer(j);
            if Felucca <> nil then Break;
          End;
        End;
      End;
  End;

  if Felucca = nil then Exit;

  For i := 0 to pPE^.NumberOfSections - 5 do Begin
      cSectionStart := Cardinal(pDOS) + pSect^[i].VirtualAddress{ + pOpt^.ImageBase};
      For j := cSectionStart to pSect^[i].Misc.VirtualSize + cSectionStart - 1 do Begin
        if PPointer(j)^ = Felucca then Begin
          FeluccaPnt := Pointer(j);
          if TrammelPnt <> nil then Break;
        End;
        if PPointer(j)^ = Trammel then Begin
          TrammelPnt := Pointer(j);
          If FeluccaPnt <> nil Then Break;
        End;
      End;
  End;

  If (FeluccaPnt = nil)or((TrammelPnt <> nil) and (Cardinal(TrammelPnt) <= Cardinal(FeluccaPnt))) then Exit;

  if TrammelPnt = nil then Begin
    Result := GetMemory(SizeOf(TCli_Map_Info_Data));
    Result.Amount := 1;
    Result.Maps[0].Name := PPointer(Cardinal(FeluccaPnt) + 0)^;
    Result.Maps[0].Width := PCardinal(Cardinal(FeluccaPnt) + 4)^;
    Result.Maps[0].Height := PCardinal(Cardinal(FeluccaPnt) + 8)^;
    Result.Maps[0].MainWidth  := PCardinal(Cardinal(FeluccaPnt) + 12)^;
    Result.Maps[0].MainHeight := PCardinal(Cardinal(FeluccaPnt) + 16)^;
    Result.Maps[0].ClientRecord := FeluccaPnt;
    Exit;
  End;

  Delta := Cardinal(TrammelPnt) - Cardinal(FeluccaPnt);
  For i := 2 to 255 do Begin
    If
          (PCardinal(Cardinal(FeluccaPnt) + i*Delta + 4)^ = 0)
      or  (PCardinal(Cardinal(FeluccaPnt) + i*Delta + 4)^ >= 655536) // Maps can't be more that a WORD size.
      or  (PCardinal(Cardinal(FeluccaPnt) + i*Delta + 8)^ = 0)
      or  (PCardinal(Cardinal(FeluccaPnt) + i*Delta + 8)^ >= 655536)
    Then Begin
      Result := GetMemory(SizeOf(TCli_Map_Info_Data) + SizeOf(TCli_Map_Info)*(i-1));
      Result.Amount := i;
      For j := 0 to i - 1 do Begin
        Result.Maps[j].Name       := PPointer (Cardinal(FeluccaPnt) + j*Delta + 0)^;
        Result.Maps[j].Width      := PCardinal(Cardinal(FeluccaPnt) + j*Delta + 4)^;
        Result.Maps[j].Height     := PCardinal(Cardinal(FeluccaPnt) + j*Delta + 8)^;
        Result.Maps[j].MainWidth  := PCardinal(Cardinal(FeluccaPnt) + j*Delta + 12)^;
        Result.Maps[j].MainHeight := PCardinal(Cardinal(FeluccaPnt) + j*Delta + 16)^;
        Result.Maps[0].ClientRecord := Pointer(Cardinal(FeluccaPnt) + j*Delta);
      End;
      Exit;
    End;
  End;

End;

function GetClientInformation: PClientInformation; stdcall;
Begin
  if not CliInfoGet then Begin
    MyClientInformation := GetMemory(SizeOf(TMyClientInformation));
    MyClientInformation.Amount := 1;
    MyClientInformation.Information[0].Parameter := CLI_MAPS_INFO;
    MyClientInformation.Information[0].Data := SearchMapSizes;
    CliInfoGet := True;
  End;
  Result := PClientInformation(MyClientInformation);


End;

initialization
  CliInfoGet := False;
end.
