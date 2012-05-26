unit zLib;

interface

function InitZLib(zLib32Path, zLib64Path: AnsiString): Boolean;

type
  ZLibError = Integer;
  ZLibQuality = Integer;
  TCompress   = function(dest: Pointer; var destLength: Integer; source: Pointer; sourceLength: Integer):ZLibError; stdcall;
  TCompress2  = function(dest: Pointer; var destLength: Integer; source: Pointer; sourceLength: Integer; quality: ZLibQuality):ZLibError; stdcall;
  TDecompress = function(dest: Pointer; var destLength: Integer; source: Pointer; sourceLength: Integer):ZLibError; stdcall;

var
  zCompress2: TCompress2;
  zDecompress: TDecompress;

function Compress(dest: Pointer; var destLength: Integer; source: Pointer; sourceLength: Integer; quality: Integer):Integer; stdcall;
function Decompress(dest: Pointer; var destLength: Integer; source: Pointer; sourceLength: Integer; quality: Integer):Integer; stdcall;


implementation

uses Windows, Common;

var
  Initted:Boolean;
  hDll: THandle;

function InitZLib(zLib32Path, zLib64Path: AnsiString): Boolean;
var
  bIs64:Boolean;

Begin
  Result := False;
  hDll := INVALID_HANDLE_VALUE;
  //TODO: Add 64-bit check
  bIs64 := False;
  If bIs64 Then Begin
    If not FileExists(zLib64Path) Then Begin
      bIs64 := False;
    End Else Begin
      hDll := LoadLibraryA(PAnsiChar(zLib64Path));
      If hDll = INVALID_HANDLE_VALUE Then bIs64 := False;
    End;
  End;
  If not bIs64 Then Begin
    If not FileExists(zLib32Path) Then Exit;
    hDll := LoadLibraryA(PAnsiChar(zLib32Path));
    If hDll = INVALID_HANDLE_VALUE Then Exit;
  End;

  zCompress2 := GetProcAddress(hDll, 'compress2');
  zDecompress := GetProcAddress(hDll, 'decompress');

  Result := True;
  Initted := True;
End;

function Compress(dest: Pointer; var destLength: Integer; source: Pointer; sourceLength: Integer; quality: Integer):Integer; stdcall;
Begin
  If not Initted Then
    If not InitZLib('zlib32.dll', 'zlib64.dll') Then begin
      Result := -6; //VersionError
      Exit;
    End;
  Result := zCompress2(dest, destLength, source, sourceLength, quality);
End;

function Decompress(dest: Pointer; var destLength: Integer; source: Pointer; sourceLength: Integer; quality: Integer):Integer; stdcall;
Begin
  If not Initted Then
    If not InitZLib('zlib32.dll', 'zlib64.dll') Then Begin
      Result := -6; //VersionError
      Exit;
    End;
  Result := zDecompress(dest, destLength, source, sourceLength);
End;

initialization
  Initted := False;
end.
