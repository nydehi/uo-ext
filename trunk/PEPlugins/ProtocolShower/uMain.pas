unit uMain;

interface

uses Windows, PluginsShared;

implementation

uses PluginAPI, SysUtils;

procedure WriteDump(Point:Pointer; Len:Cardinal);
var
  cLine:String;
  cPos:Cardinal;
begin
  If Len>0 Then Begin
{
000000 | 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00  |
}
    cPos:=0;
    repeat
      if cPos mod 16 = 0 Then Begin
        if cPos<>0 Then WriteLn(cLine);
        cLine:=IntToStr(cPos);
        if cPos<100000 Then cLine:='0'+cLine;
        if cPos<10000 Then cLine:='0'+cLine;
        if cPos<1000 Then cLine:='0'+cLine;
        if cPos<100 Then cLine:='0'+cLine;
        if cPos<10 Then cLine:='0'+cLine;
        cLine:=cLine + ' | ';
      End;
      cLine:=cLine+IntToHex(PByte(Cardinal(Point)+cPos)^ ,2) + ' ';
      If (cPos + 1) mod 4 = 0 Then cLine:=cLine + ' ';
      cPos:=cPos+1;
    until cPos>=Len;
    WriteLn(cLine);
  End;
end;

function PacketHandle(Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean stdcall;
begin
  If IsFromServerToClient Then
    Write('Server -> Client: ')
  Else
    Write('Client -> Server: ');
  WriteLn('Packet: ', IntToHex(PByte(Data)^, 2), ' Size: ', Size);
  WriteDump(Data, Size);
  WriteLn;
  Result := False;
end;

procedure Init;
var
  i: Byte;
begin
  For i:= 0 to 255 do Begin
    API.RegisterPacketHandler(i, @PacketHandle);
  End;
  AllocConsole;
  TTextRec(Output).Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  TTextRec(ErrOutput).Handle := GetStdHandle(STD_ERROR_HANDLE);
  Writeln('Console started');
end;

initialization
  PluginInitialization := @Init;
end.
