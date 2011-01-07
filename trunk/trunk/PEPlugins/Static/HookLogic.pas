unit HookLogic;

interface

procedure HookIt;

implementation

uses Windows, Common, APIHooker, PluginAPI;

type
  RStatic=record
    Indexer: THandle;
    Data: THandle;
  end;

const
  MaxMapNum = 4;
var
  Maps: Array [0..MaxMapNum] of RStatic;

function CreateFileAHook(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; stdcall;
var
  sRealName: String;
  i: Integer;
begin
  Result := INVALID_HANDLE_VALUE;
  sRealName:=lpFileName;
  i:=Length(sRealName);
  repeat
    if sRealName[i]='\' then begin
      sRealName:=UpperCase(Copy(sRealName, i + 1, Length(sRealName)));
      Break;
    end;
    Dec(i);
  until i=0;
  MessageBox(0, PCHar('Open file: ' + sRealName), nil, MB_OK);

  If (Copy(sRealName, 1, 7) = 'STATICS') and (Copy(sRealName, 9, 4) = '.MUL') Then Begin
    i := StrToInt(Copy(sRealName, 8, 1));
    If (i > 4) Then Begin
      MessageBox(0, PChar('Can''t work with ' + IntToStr(i) + ' map. This client unsupported with Static plugin.'), nil, MB_OK);
      Halt(1);
    End;
    dwShareMode:=(FILE_SHARE_READ + FILE_SHARE_WRITE);
    Hooker.TrueAPI(@CreateFileAHook);
    Result:=CreateFileA(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
    Hooker.TrueAPIEnd(@CreateFileAHook);
    Maps[i].Data := Result;
  End;
  If (Copy(sRealName, 1, 6) = 'STAIDX') and (Copy(sRealName, 8, 4) = '.MUL') Then Begin
    i := StrToInt(Copy(sRealName, 7, 1));
    If (i > 4) Then Begin
      MessageBox(0, PChar('Can''t work with ' + IntToStr(i) + ' map. This client unsupported with Static plugin.'), nil, MB_OK);
      Halt(1);
    End;
    dwShareMode:=(FILE_SHARE_READ + FILE_SHARE_WRITE);
    Hooker.TrueAPI(@CreateFileAHook);
    Result:=CreateFileA(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
    Hooker.TrueAPIEnd(@CreateFileAHook);
    Maps[i].Indexer := Result;
  End;
  If Result = INVALID_HANDLE_VALUE Then Begin
    Hooker.TrueAPI(@CreateFileAHook);
    Result:=CreateFileA(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
    Hooker.TrueAPIEnd(@CreateFileAHook);
  End;


end;

function ReadFileHook(hFile: THandle; var Buffer: Pointer; nNumberOfBytesToRead: DWORD;
  var lpNumberOfBytesRead: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;
var
  i: Integer;
begin

{  for i:=0 to MaxMapNum do Begin
    If Maps[i].Indexer = hFile Then Begin
      MessageBox(0, PChar('Map: ' + IntToStr(i) + ' Block read: ' + IntToStr(nNumberOfBytesToRead div 12)), nil, MB_OK);
    End;
  End;}

  Hooker.TrueAPI(@ReadFileHook);
  Result := ReadFile(hFile, Buffer, nNumberOfBytesToRead, lpNumberOfBytesRead, lpOverlapped);
  Hooker.TrueAPIEnd(@ReadFileHook);
end;

procedure HookIt;
begin
  Hooker.HookFunction(@CreateFileAHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'CreateFileA'));
  Hooker.HookFunction(@ReadFileHook, GetProcAddress(GetModuleHandle('kernel32.dll'), 'ReadFile'));
  Hooker.InjectIt;
end;

end.
