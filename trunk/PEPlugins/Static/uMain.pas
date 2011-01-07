unit uMain;

interface

implementation

uses Windows, PluginAPI, APIHooker, Common, Packet, HookLogic;

var
  NewBlock: Pointer;
  NewBlockPosition: Pointer;
  NewBlockSize: Cardinal;

function ExtenderLength(Packet:Pointer; Length:Cardinal):Cardinal; stdcall;
begin
  Result :=0;
  If (Length < 4) Then Exit;
  Result := PWord(Cardinal(Packet) + 2)^;
end;

function ExtenderPacket(Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean;
var
  Reader : TPacketReader;
  Facet: Byte;
  BlockChunkSize: Cardinal;
  Block: Integer;
Begin
  Send := False;
  Result := True;

  If PByte(Cardinal(Data) + 1)^ = $01 Then Begin
    // NewBlockHeader
    Reader := TPacketReader.GetPacketReader;
    Reader.AssignToBuffer(Data, Size);
    Reader.Offset := 4;
    Facet := Reader.ReadByte;
    Block := Reader.ReadCardinal;
    BlockChunkSize := Reader.ReadInteger;
    If BlockChunkSize < Size Then Begin
      NewBlockSize := Block;
      NewBlock := GetMemory(NewBlockSize);
      ZeroMemory(NewBlock, NewBlockSize);
      CopyMemory(NewBlock, Pointer(Cardinal(Data) + 1), BlockChunkSize);
    End;
  End;
End;

procedure Init;
begin

end;

initialization
  PluginAPI.PluginInitialization := @Init;
end.
