unit Executable;

interface

uses Windows, Common;

function Infect32(Wnd: HWnd; Instance: HInst; CmdLine: PAnsiChar; nCmdShow: Integer): Bool; stdcall;

function InfectA(AExecutablePath: PAnsiChar):Boolean; stdcall;
function InfectW(AExecutablePath: PWideChar):Boolean; stdcall;

function ExtractXMLMeta32(Wnd: Hwnd; Instance: HInst; CmdLine: PAnsiChar; nCmdShow: Integer): Bool; stdcall;
implementation

uses PluginsShared;

{$REGION 'Infection'}

function PrepareInjectCode_End(RealStartPoint: Cardinal; var Size: Cardinal):Pointer; forward;

function PrepareInjectCode(RealStartPoint: Cardinal; var Size: Cardinal):Pointer;
Begin
  Result := PrepareInjectCode_End(RealStartPoint, Size);
End;

procedure InjectProc(); asm
         MOV ESI, [ESP]
         CALL @__End
         INC EAX
         MOV EDX, EAX
         PUSH EAX // Pointer to data


         AND ESI, $FFFF0000
@_L1:    CMP WORD PTR [ESI], "ZM"
         JZ @_CPE
@_L2:    SUB ESI, $10000
         JMP @_L1
@_CPE:   MOV EDI, [ESI + $3C]
         ADD EDI, ESI
         CMP DWORD PTR [EDI], "EP"
         JZ @_GK
         JMP @_L2
@_FL:    MOV ESI, $07C800000
@_GK:    XCHG EAX, ESI
         // EAX = kernel base
         MOV EBX, EAX
@_LOAD:  POP EDX    // Pointer to data
         PUSH EDX
         CALL @GPA  // EAX = Address of LoadLibraryExA
         POP EDX
         PUSH EDX
         ADD EDX, 0Fh // Pointer to UOExt.dll
         PUSH 08h
         PUSH 00h
         PUSH EDX
         CALL EAX // EAX now holds UOExt.dll base.
         MOV ECX, EAX // ECX - Handle for UOExt.dll
         POP EDX  // EDX pointer to data
         PUSH EDX
         ADD EDX, 019h // EDX pointer to CoreInitialize
         CALL @GPA // EAX now pointer to CoreInitialize func
         PUSH EDX
         PUSH ECX
         PUSH EBX
         CALL EAX
         POP EBX
         POP ECX
         POP EDX
         CMP AL, 0
         JZ @_ALL_OK
         CMP AL, 1
         JZ @_RELOAD
         POP EDX
         PUSH EDX
         ADD EDX, 028h // EDX pointer to FreeLibrary
         MOV EAX, EBX
         CALL @GPA // EAX pointer to FreeLibrary
         PUSH ECX
         CALL EAX // FreeLibrary
         JMP @_ALL_OK
@_RELOAD:POP EDX
         PUSH EDX
         ADD EDX, 028h // EDX pointer to FreeLibrary
         MOV EAX, EBX
         CALL @GPA // EAX pointer to FreeLibrary
         PUSH ECX
         CALL EAX // FreeLibrary
         MOV EAX, EBX
         JMP @_LOAD
@_ALL_OK:POP EDX
         PUSH 012345678h // Change this for real EntryPoint
         RET


// GetProcAddress EAX = dll base, EDX = Pointer to Procedure name.
@GPA:    PUSH  EBX
         PUSH  ECX
         PUSH  ESI
         PUSH  EDI
         MOV   ESI, EDX
         MOV   EDI, EDX  // EDI = Search API pointer
@_1:     CMP   BYTE PTR [ESI], 0
         JZ    @_2
         INC   ESI
         JMP   @_1
@_2:     INC   ESI
         SUB   ESI, EDX
         MOV   ECX, ESI   // ECX = Length of API to search

         MOV   EBX, EAX   // EBX = kern base.
         MOV   ESI, EAX
         ADD   ESI, 03Ch  // EAX = VA for RVA PE
         MOV   EAX, [ESI]
         MOV   ESI, EBX
         ADD   ESI, EAX   // ESI = VA for PE
         ADD   ESI, 078h  // ESI = VA for RVA export
         MOV   EAX, [ESI]
         MOV   ESI, EBX
         ADD   ESI, EAX   // ESI = VA export
         PUSH  ESI
         ADD   ESI, 020h  // ESI = VA for RVA Names
         MOV   EAX, [ESI]
         MOV   ESI, EBX
         ADD   ESI, EAX   // ESI = VA for Names
         XOR   EDX, EDX

@_3:     PUSH  ESI
         MOV   EAX, [ESI]
         MOV   ESI, EBX
         ADD   ESI, EAX   // ESI = VAR for Name
         PUSH  ECX
         PUSH  EDI
         CLD
         REP   CMPSB
         POP   EDI
         POP   ECX
         POP   ESI
         JZ    @_4
         ADD   ESI, 4
         INC   EDX
         JMP   @_3

@_4:     SHL   EDX, 1     // EDI = offset for Ordinals
         POP   ESI        // ESI = VA for export
         PUSH  ESI
         ADD   ESI, 024h  // ESI = VA for RVA Ordinals
         MOV   EAX, [ESI]
         MOV   ESI, EBX
         ADD   ESI, EAX   // ESI = VA Ordinals
         ADD   ESI, EDX   // ESI = VA for ordinal
         XOR   EDX, EDX
         MOVZX EDX, WORD PTR [ESI]
         SHL   EDX, 2
         POP   ESI        // ESI = VA for export
         ADD   ESI, 01Ch  // ESI = VA for RVA Funcs Address
         MOV   EAX, [ESI]
         MOV   ESI, EBX
         ADD   ESI, EAX
         ADD   ESI, EDX   // ESI = VA for Func Address
         MOV   EAX, [ESI]
         ADD   EAX, EBX
         POP   EDI
         POP   ESI
         POP   ECX
         POP   EBX
         RET
// End of GetProcAddress EAX = dll base, EDX = Pointer to Procedure name.


@_CPos:  MOV EAX, [ESP]
         RET
@__End:  CALL @_CPos
         RET

//       LoadLibraryExA + #0
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
//       UOExt.dll + #0
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
//       CoreInitialize + #0
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
//       FreeLibrary + #0
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
end;

function PrepareInjectCode_End(RealStartPoint: Cardinal; var Size: Cardinal):Pointer;
const
  LLE : Array [0..14] of AnsiChar = 'LoadLibraryExA' + #0;
  UOE : Array [0..9]  of AnsiChar = 'UOExt.dll' + #0;
  CI  : Array [0..14] of AnsiChar = 'CoreInitialize' + #0;
  FL  : Array [0..11] of AnsiChar = 'FreeLibrary' + #0;
Begin
  Size := $14A;
  Result := GetMemory(Size);
  CopyMemory(Result, @InjectProc, Size);
  PCardinal(Cardinal(Result) + $8D)^ := RealStartPoint;
  CopyMemory(Pointer(Cardinal(Result) + Size - 52), @LLE[0], 15);
  CopyMemory(Pointer(Cardinal(Result) + Size - 37), @UOE[0], 10);
  CopyMemory(Pointer(Cardinal(Result) + Size - 27), @CI[0], 15);
  CopyMemory(Pointer(Cardinal(Result) + Size - 12), @FL[0], 12);
End;

function Align(Value, Factor: Cardinal): Cardinal;
Begin
  Result := (Value + (Factor - 1)) AND ( NOT (Factor - 1));
End;

function GetMappingSizeA(AFile: PAnsiChar; var InjectCode: Pointer; var CodeSize: Cardinal): Cardinal;
var
  hFile, hMapping: THandle;
  pFile: Pointer;
  DosHeader: PImageDosHeader;
  PEHeader: PImageFileHeader;
  OptHeader: PImageOptionalHeader;
Begin
  Result := 0;
  hFile := CreateFileA(AFile, GENERIC_READ OR GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hFile = INVALID_HANDLE_VALUE then Begin
    WriteLn('Can''t infect ', AFile, ' CreateFileA failed. GLE: ', GetLastError);
    Halt(1);
  End;
  hMapping := CreateFileMappingA(hFile, nil, PAGE_READWRITE, 0, 0, nil);
  pFile := MapViewOfFile(hMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  if pFile = nil then Begin
    WriteLn('Can''t infect ', AFile, ' MapViewOfFile failed. GLE: ', GetLastError);
    Halt(1);
  End;

  DosHeader := pFile;
  PEHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4);
  if PEHeader^.SizeOfOptionalHeader = 0 then Begin
    Exit;
  End;
  OptHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader));

  InjectCode := PrepareInjectCode(OptHeader^.AddressOfEntryPoint + OptHeader^.ImageBase, CodeSize);

  Result := GetFileSize(hFile, nil) + Align(CodeSize + 7, OptHeader^.FileAlignment);
  UnmapViewOfFile(pFile);
  CloseHandle(hMapping);
  CloseHandle(hFile);
End;

function GetMappingSizeW(AFile: PWideChar; var InjectCode: Pointer; var CodeSize: Cardinal): Cardinal;
var
  hFile, hMapping: THandle;
  pFile: Pointer;
  DosHeader: PImageDosHeader;
  PEHeader: PImageFileHeader;
  OptHeader: PImageOptionalHeader;
Begin
  Result := 0;
  hFile := CreateFileW(AFile, GENERIC_READ OR GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hFile = INVALID_HANDLE_VALUE then Begin
    WriteLn('Can''t infect ', AFile, ' CreateFileA failed. GLE: ', GetLastError);
    Halt(1);
  End;
  hMapping := CreateFileMappingW(hFile, nil, PAGE_READWRITE, 0, 0, nil);
  pFile := MapViewOfFile(hMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  if pFile = nil then Begin
    WriteLn('Can''t infect ', AFile, ' MapViewOfFile failed. GLE: ', GetLastError);
    Halt(1);
  End;

  DosHeader := pFile;
  PEHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4);
  if PEHeader^.SizeOfOptionalHeader = 0 then Begin
    Exit;
  End;
  OptHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader));

  InjectCode := PrepareInjectCode(OptHeader^.AddressOfEntryPoint + OptHeader^.ImageBase, CodeSize);

  Result := GetFileSize(hFile, nil) + Align(CodeSize + 7, OptHeader^.FileAlignment);
  UnmapViewOfFile(pFile);
  CloseHandle(hMapping);
  CloseHandle(hFile);
End;

function Infect32(Wnd: HWnd; Instance: HInst; CmdLine: PAnsiChar; nCmdShow: Integer): Bool; stdcall;
Begin
  Result := InfectA(CmdLine);
End;

function InfectA(AExecutablePath: PAnsiChar):Boolean; stdcall;
type
  TPorcedure = procedure;
  TSections = Array [0..999] of TImageSectionHeader;
  PSections = ^TSections;
var
  DosHeader: PImageDosHeader;
  PEHeader: PImageFileHeader;
  OptHeader: PImageOptionalHeader;
  Sections: PSections;

  hFile, hMapping: THandle;
  pFile: Pointer;

  i, LastSectionId: Word;

  cMaxVirtualAddress, cCodeSize, cInfectMark, cFileSize: Cardinal;
  pCodeData: Pointer;
  sInfectMark: AnsiString;
Begin
  Result := False;

  sInfectMark := 'UOEX';
  PCardinal(@cInfectMark)^ := PCardinal(@sInfectMark[1])^;

  cFileSize := GetMappingSizeA(AExecutablePath, pCodeData, cCodeSize);
  if cFileSize = 0 then Begin
    MessageBox(0, 'This executable not supported', nil, MB_OK);
    Exit;
  End;

  hFile := CreateFileA(AExecutablePath, GENERIC_READ OR GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  hMapping := CreateFileMappingA(hFile, nil, PAGE_READWRITE, 0, cFileSize, nil);
  pFile := MapViewOfFile(hMapping, FILE_MAP_ALL_ACCESS, 0, 0, cFileSize);

  DosHeader := pFile;
  PEHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4);
  OptHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader));
  Sections := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader) + SizeOf(TImageOptionalHeader) + (OptHeader^.NumberOfRvaAndSizes - IMAGE_NUMBEROF_DIRECTORY_ENTRIES) * SizeOf(TImageDataDirectory));

  LastSectionId := 0;
  cMaxVirtualAddress := 0;
  For i := 0 to PEHeader^.NumberOfSections - 1 do Begin
    if cMaxVirtualAddress < Sections^[i].VirtualAddress then Begin
      LastSectionId := i;
      cMaxVirtualAddress := Sections^[i].VirtualAddress;
    End;
  End;

  {Write infect code}
  CopyMemory(Pointer(Cardinal(pFile) + Sections[LastSectionId].PointerToRawData + Sections[LastSectionId].SizeOfRawData), pCodeData, cCodeSize);

  {Fix Entry point}
  OptHeader.AddressOfEntryPoint := Sections[LastSectionId].VirtualAddress + Sections[LastSectionId].SizeOfRawData;

  {Fix Virtual and Physical sizes}
  Sections[LastSectionId].Misc.VirtualSize := Align(Sections[LastSectionId].Misc.VirtualSize + cCodeSize, OptHeader.SectionAlignment);
  Sections[LastSectionId].SizeOfRawData := Align(Sections[LastSectionId].SizeOfRawData + cCodeSize, OptHeader.FileAlignment);
  if Sections[LastSectionId].SizeOfRawData > Sections[LastSectionId].Misc.VirtualSize then
    Sections[LastSectionId].Misc.VirtualSize := Align(Sections[LastSectionId].SizeOfRawData, OptHeader.SectionAlignment);

  OptHeader.SizeOfImage := Sections[LastSectionId].VirtualAddress + Sections[LastSectionId].Misc.VirtualSize;

  {Allowing to execut this section and misc changes}
  Sections[LastSectionId].Characteristics := (Sections[LastSectionId].Characteristics OR $A0000020) AND ( NOT $02000000 );

  UnmapViewOfFile(pFile);
  CloseHandle(hMapping);
  CloseHandle(hFile);

  FreeMemory(pCodeData);

  Result := True;
End;

function InfectW(AExecutablePath: PWideChar):Boolean; stdcall;
type
  TPorcedure = procedure;
  TSections = Array [0..999] of TImageSectionHeader;
  PSections = ^TSections;
var
  DosHeader: PImageDosHeader;
  PEHeader: PImageFileHeader;
  OptHeader: PImageOptionalHeader;
  Sections: PSections;

  hFile, hMapping: THandle;
  pFile: Pointer;

  i, LastSectionId: Word;

  cMaxVirtualAddress, cCodeSize, cInfectMark, cFileSize: Cardinal;
  pCodeData: Pointer;
  sInfectMark: AnsiString;
Begin
  Result := False;

  sInfectMark := 'UOEX';
  PCardinal(@cInfectMark)^ := PCardinal(@sInfectMark[1])^;

  cFileSize := GetMappingSizeW(AExecutablePath, pCodeData, cCodeSize);
  if cFileSize = 0 then Begin
    MessageBox(0, 'This executable not supported', nil, MB_OK);
    Exit;
  End;

  hFile := CreateFileW(AExecutablePath, GENERIC_READ OR GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  hMapping := CreateFileMappingW(hFile, nil, PAGE_READWRITE, 0, cFileSize, nil);
  pFile := MapViewOfFile(hMapping, FILE_MAP_ALL_ACCESS, 0, 0, cFileSize);

  DosHeader := pFile;
  PEHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4);
  OptHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader));
  Sections := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader) + SizeOf(TImageOptionalHeader) + (OptHeader^.NumberOfRvaAndSizes - IMAGE_NUMBEROF_DIRECTORY_ENTRIES) * SizeOf(TImageDataDirectory));

  LastSectionId := 0;
  cMaxVirtualAddress := 0;
  For i := 0 to PEHeader^.NumberOfSections - 1 do Begin
    if cMaxVirtualAddress < Sections^[i].VirtualAddress then Begin
      LastSectionId := i;
      cMaxVirtualAddress := Sections^[i].VirtualAddress;
    End;
  End;

  {Write infect code}
  CopyMemory(Pointer(Cardinal(pFile) + Sections[LastSectionId].PointerToRawData + Sections[LastSectionId].SizeOfRawData), pCodeData, cCodeSize);

  {Fix Entry point}
  OptHeader.AddressOfEntryPoint := Sections[LastSectionId].VirtualAddress + Sections[LastSectionId].SizeOfRawData;

  {Fix Virtual and Physical sizes}
  Sections[LastSectionId].Misc.VirtualSize := Align(Sections[LastSectionId].Misc.VirtualSize + cCodeSize, OptHeader.SectionAlignment);
  Sections[LastSectionId].SizeOfRawData := Align(Sections[LastSectionId].SizeOfRawData + cCodeSize, OptHeader.FileAlignment);
  if Sections[LastSectionId].SizeOfRawData > Sections[LastSectionId].Misc.VirtualSize then
    Sections[LastSectionId].Misc.VirtualSize := Align(Sections[LastSectionId].SizeOfRawData, OptHeader.SectionAlignment);

  OptHeader.SizeOfImage := Sections[LastSectionId].VirtualAddress + Sections[LastSectionId].Misc.VirtualSize;

  {Allowing to execut this section and misc changes}
  Sections[LastSectionId].Characteristics := (Sections[LastSectionId].Characteristics OR $A0000020) AND ( NOT $02000000 );

  UnmapViewOfFile(pFile);
  CloseHandle(hMapping);
  CloseHandle(hFile);

  FreeMemory(pCodeData);

  Result := True;
End;
{$ENDREGION}

{$REGION 'XMLWriter'}
function ExtractXMLMeta32(Wnd: Hwnd; Instance: HInst; CmdLine: PAnsiChar; nCmdShow: Integer): Bool; stdcall;
var
  hLib: THandle;
  DllInit: TDllInit;
  Plugins: PDllPlugins;
  F:Text;
  PlgCntr, DescrCntr: Cardinal;
Begin
  hLib := LoadLibraryA(CmdLine);
  if hLib = INVALID_HANDLE_VALUE then Begin
    WriteLn('Can''t load library ', CmdLine);
    Halt(1);
  End;
  @DllInit := GetProcAddress(hLib, 'DllInit');
  if not Assigned(@DllInit) then Begin
    WriteLn('Can''t load init proc (DllInit) in ', CmdLine);
    Halt(1);
  End;
  Plugins := DllInit;
  if Plugins^.PluginsCount = 0 then Begin
    WriteLn('DllInit returns 0 plugins. It''s not an UOExt plugins library. (', CmdLine,')');
    Halt(1);
  End;

  AssignFile(F, String(AnsiString(CmdLine) + '.xml'));
  Rewrite(F);
  WriteLn(F, '<?xml version="1.0" encoding="utf-8" ?>');
  WriteLn(F, '<Library Plugins="', Plugins^.PluginsCount,'">');

  For PlgCntr := 0 to Plugins^.PluginsCount - 1 do Begin
    WriteLn(F, '  <Plugin Id="', PlgCntr, '" Descriptors="', Plugins^.Plugins[PlgCntr]^.DescriptorsCount, '">');
    if Plugins^.Plugins[PlgCntr]^.DescriptorsCount > 0 then for DescrCntr := 0 to Plugins^.Plugins[PlgCntr]^.DescriptorsCount - 1 do Begin
      if Plugins^.Plugins[PlgCntr]^.Descriptors[DescrCntr].Descriptor = PD_NAME then Begin
        WriteLn(F, '    <Descriptor Id="', Plugins^.Plugins[PlgCntr]^.Descriptors[DescrCntr].Descriptor, '" Value="', PAnsiChar(Plugins^.Plugins[PlgCntr]^.Descriptors[DescrCntr].Value), '" />');
      End Else Begin
        WriteLn(F, '    <Descriptor Id="', Plugins^.Plugins[PlgCntr]^.Descriptors[DescrCntr].Descriptor, '" Value="', Plugins^.Plugins[PlgCntr]^.Descriptors[DescrCntr].Value, '" />');
      End;
    End;
    WriteLn(F, '  </Plugin>');
  End;
  WriteLn(F, '</Library>');

  FreeLibrary(hLib);
  CloseFile(F);
  Result := True;
End;
{$ENDREGION}
end.
