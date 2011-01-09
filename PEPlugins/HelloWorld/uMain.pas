unit uMain;

interface

implementation

uses Windows, WinSock, PluginAPI;

var
  HelloWorldPacket: Array [0..56] of Byte;

function HelloHandler(Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean stdcall;
var
  isValid: Boolean;
begin
  API.UnRegisterPacketHandler($3A, @HelloHandler);
  API.SendPacket(@HelloWorldPacket, 57, False, True, isValid);
  Result := False;
end;

procedure Init;
var
  sDummy: String;
Begin
  API.RegisterPacketHandler($3A, @HelloHandler);

// Filling Hello world packet
  ZeroMemory(@HelloWorldPacket, 57);
  PByte(@HelloWorldPacket)^ :=  $1C; //Header (1Byte)
  PWord(Cardinal(@HelloWorldPacket) + 1)^ :=  htons(57); // SizeOfpacket (2Bytes)
  PCardinal(Cardinal(@HelloWorldPacket) + 3)^ :=  $FFFFFFFF; //Serial (4Bytes)
  PWord(Cardinal(@HelloWorldPacket) + 7)^ :=  $FFFF; // Graphic (2Bytes)
  PByte(Cardinal(@HelloWorldPacket) + 9)^ :=  $00; // MessageType (1Byte)
  PWord(Cardinal(@HelloWorldPacket) + 10)^ :=  htons($03B2); // Hue (2Bytes)
  PWord(Cardinal(@HelloWorldPacket) + 12)^ :=  htons($0003); // Font (2Bytes)
  sDummy := 'System';
  CopyMemory(PChar(Cardinal(@HelloWorldPacket) + 14), @sDummy[1], 6); // Name (30Bytes)
  sDummy := 'Hello World!' + #0;
  CopyMemory(PChar(Cardinal(@HelloWorldPacket) + 44), @sDummy[1], 12); // Message (Various)
End;

initialization
  PluginInitialization := @Init;
end.
