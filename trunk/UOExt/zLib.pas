unit zLib;

interface

function Compress(APlugin: Cardinal; dest: Pointer; destLength: PInteger; source: Pointer; sourceLength: Integer; quality: Integer):Integer; stdcall;
function Uncompress(APlugin: Cardinal; dest: Pointer; destLength: PInteger; source: Pointer; sourceLength: Integer):Integer; stdcall;

implementation

uses Windows, Common;

type
  ZLibError = Integer;
  ZLibQuality = Integer;
  TCompress   = function(dest: Pointer; destLength: PInteger; source: Pointer; sourceLength: Integer):ZLibError; cdecl;
  TCompress2  = function(dest: Pointer; destLength: PInteger; source: Pointer; sourceLength: Integer; quality: ZLibQuality):ZLibError; cdecl;
  TUncompress = function(dest: Pointer; destLength: PInteger; source: Pointer; sourceLength: Integer):ZLibError; cdecl;

var
  zCompress2: TCompress2;
  zUncompress: TUncompress;

var
  Initted:Boolean;
  hDll: THandle;

function InitZLib(zLib32Path: AnsiString): Boolean;
Begin
  Result := False;
  hDll := INVALID_HANDLE_VALUE;

  If not FileExists(zLib32Path) Then Exit;
  hDll := LoadLibraryA(PAnsiChar(zLib32Path));
  If hDll = INVALID_HANDLE_VALUE Then Exit;

  zCompress2 := GetProcAddress(hDll, 'compress2');
  zUncompress := GetProcAddress(hDll, 'uncompress');

  Result := True;
  Initted := True;
End;

function Compress(APlugin: Cardinal; dest: Pointer; destLength: PInteger; source: Pointer; sourceLength: Integer; quality: Integer):Integer; stdcall;
Begin
  Result := -1;
  if not Initted then Begin
    if not InitZLib('zlib32.dll') then Result := -6;
  End else
    Result := zCompress2(dest, destLength, source, sourceLength, quality);
End;

function Uncompress(APlugin: Cardinal; dest: Pointer; destLength: PInteger; source: Pointer; sourceLength: Integer):Integer; stdcall;
Begin
  Result := -1;
  if not Initted then Begin
    if not InitZLib('zlib32.dll') then Result := -6;
  End else
    Result := zUncompress(dest, destLength, source, sourceLength);
End;

initialization
  Initted := False;
  zCompress2 := nil;
  zUncompress := nil;
end.
