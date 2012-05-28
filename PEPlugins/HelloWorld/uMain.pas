unit uMain;

interface

implementation

uses Windows, WinSock, PluginAPI, PluginsShared;

var
  HelloWorldPacket: Array [0..63] of Byte;
  API: TPluginApi;
  PluginInfo: PPluginInfo;

function HelloHandler(Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean stdcall;
var
  isValid: Boolean;
begin
  API.UnRegisterPacketHandler($3A, @HelloHandler);
  API.SendPacket(@HelloWorldPacket, 64, False, True, isValid);
  Result := False;
end;

procedure Init;
var
  sDummy: AnsiString;
Begin
  API.RegisterPacketHandler($3A, @HelloHandler);

// Filling Hello world packet
  ZeroMemory(@HelloWorldPacket, 64);
  PByte(@HelloWorldPacket)^ :=  $1C; //Header (1Byte)
  PWord(Cardinal(@HelloWorldPacket) + 1)^ :=  htons(64); // SizeOfpacket (2Bytes)
  PCardinal(Cardinal(@HelloWorldPacket) + 3)^ :=  $FFFFFFFF; //Serial (4Bytes)
  PWord(Cardinal(@HelloWorldPacket) + 7)^ :=  $FFFF; // Graphic (2Bytes)
  PByte(Cardinal(@HelloWorldPacket) + 9)^ :=  $00; // MessageType (1Byte)
  PWord(Cardinal(@HelloWorldPacket) + 10)^ :=  htons($03B2); // Hue (2Bytes)
  PWord(Cardinal(@HelloWorldPacket) + 12)^ :=  htons($0003); // Font (2Bytes)
  sDummy := 'System';
  CopyMemory(PChar(Cardinal(@HelloWorldPacket) + 14), @sDummy[1], 6); // Name (30Bytes)
  sDummy := 'UOExt: Hello World!' + #0;
  CopyMemory(PChar(Cardinal(@HelloWorldPacket) + 44), @sDummy[1], 20); // Message (Various)
End;

procedure Clear;
Begin
  API.UnRegisterPacketHandler($3A, @HelloHandler);
End;

function PluginInit(APluginEvent: Cardinal; APluginEventData: Pointer): Boolean; stdcall;
Begin
  if APluginEvent = PE_INIT then Begin
    API := TPluginApi.Create;
    FreeMemory(PluginInfo);
  End;
  Result := API.HandlePluginEvent(APluginEvent, APluginEventData);
  if APluginEvent = PE_PROXYSTART then Init;
  if APluginEvent = PE_PROXYEND then Clear;
  if APluginEvent = PE_FREE Then API.Free;
End;

type
  TMyDescription = packed record
    InitProcedure: Pointer;
    Size: Cardinal;
    Data: Array [0..1] of TPluginDescriptor;
  end;

const
  Name: AnsiString = 'Hello World' + #0;
  Description:TMyDescription = (
    InitProcedure : @PluginInit;
    Size: 2;
    Data: (
      ( Descriptor: PD_NAME;                    Value: 0 ),
      ( Descriptor: PD_UOEXTPROTO_PACKETAMOUNT; Value: 0 )
    )
  );


initialization
  PPointer(@Description.Data[0].Value)^ := @Name[1];

  PluginAPI.AddPlugin(@Description);
end.
