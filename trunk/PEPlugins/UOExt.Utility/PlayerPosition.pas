unit PlayerPosition;

interface

uses PluginAPI, PluginsShared, UOExt.Utility.Bindings;

procedure RegisterHandlers(AnAPI: TPluginApi);
procedure UnRegisterHandlers;

function AddOnPositionChanged(Callback: TOnPositionChanged; AParam: Pointer): Cardinal; stdcall;
procedure RemoveOnPositionChanged(AHandle: Cardinal); stdcall;

implementation

uses Windows, Common;

type
  TCallbacks=record
    Callback: TOnPositionChanged;
    AParam: Pointer;
  end;

var
  API: TPluginApi;
  PlayerPos: TClientPosition;
  PlayerPosDelta: TClientPositionDelta;
  PlayerSerial: Cardinal;
  Initialized: Boolean;

  Callbacks: Array of TCallbacks;
  Callbacks_Length: Cardinal;
  FreeCallbacks: Cardinal;

function AddOnPositionChanged(Callback: TOnPositionChanged; AParam: Pointer): Cardinal;
Begin
  if FreeCallbacks = 0 then Begin
    Result := Callbacks_Length;
    SetLength(Callbacks, Callbacks_Length + 1);
  End Else Begin
    For Result := 0 to Callbacks_Length - 1 do If not Assigned(Callbacks[Result].Callback) then Begin
      FreeCallbacks := FreeCallbacks - 1;
      Break;
    End;
  End;
  Callbacks[Result].Callback := Callback;
  Callbacks[Result].AParam := AParam;
  Callbacks_Length := Callbacks_Length + 1;
End;

procedure RemoveOnPositionChanged(AHandle: Cardinal);
Begin
  Callbacks[AHandle].Callback := nil;
  if AHandle = Callbacks_Length then Begin
    Callbacks_Length := Callbacks_Length - 1;
    SetLength(Callbacks, Callbacks_Length);
  End Else
    FreeCallbacks := FreeCallbacks + 1;
End;

procedure OnPlayerPositionChanged(APackeHead: Byte; lParam: Pointer; IsFromServerToClient: Boolean); stdcall;
var
  i: Cardinal;
Begin
  If Callbacks_Length > 0 Then for i := 0 to Callbacks_Length - 1 do if Assigned(Callbacks[i].Callback) then
    Callbacks[i].Callback(@PlayerPos, @PlayerPosDelta, Callbacks[i].AParam);

  PlayerPos.X := PlayerPos.X + PlayerPosDelta.X;
  PlayerPos.Y := PlayerPos.Y + PlayerPosDelta.Y;
  PlayerPos.Z := PlayerPos.Z + PlayerPosDelta.Z;
  PlayerPos.Direction := PlayerPosDelta.Direction;
  PlayerPos.Map := PlayerPosDelta.Map;
End;

// 0x02 Movement Request (http://ruosi.org/packetguide/index.xml#client02)
// BYTE PacketID = 0x02
// BYTE Direction (0x00 North, 0x01 Right, 0x02 East, 0x03 Down, 0x04 South, 0x05 Left, 0x06 West, 0x07 Up, 0x80 Running flag)
// BYTE Sequence Number
// DWORD Fastwalk Prevention Key
function ClientWalkRequest(Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean; stdcall;
Begin
  ZeroMemory(@PlayerPosDelta, SizeOf(PlayerPosDelta));
  PlayerPosDelta.Direction := PByte(Cardinal(Data) + 1)^ AND $7F;

  if PlayerPosDelta.Direction = PlayerPos.Direction then case PlayerPosDelta.Direction of
    $00 : PlayerPosDelta.Y := -1;
    $01 : Begin
      PlayerPosDelta.X := +1;
      PlayerPosDelta.Y := -1;
    End;
    $02 : PlayerPosDelta.X := 1;
    $03 : Begin
      PlayerPosDelta.X := 1;
      PlayerPosDelta.Y := 1;
    End;
    $04 : PlayerPosDelta.Y := 1;
    $05 : Begin
      PlayerPosDelta.X := -1;
      PlayerPosDelta.Y := 1;
    End;
    $06 : PlayerPosDelta.X := -1;
    $07 : Begin
      PlayerPosDelta.X := -1;
      PlayerPosDelta.Y := -1;
    End;
  end;
  Result := False;
  API.AfterPacketCallback(OnPlayerPositionChanged, nil);
End;

// 0x21 Movement Rejected (http://ruosi.org/packetguide/index.xml#server21)
// BYTE PacketID = 0x21
// BYTE Sequence
// WORD X
// WORD Y
// BYTE Direction
// SBYTE Z
function ServerWalkReject(Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean; stdcall;
Begin
  PlayerPosDelta.X := SwapBytes2(PWord(Cardinal(Data) + 2)^) - PlayerPos.X;
  PlayerPosDelta.Y := SwapBytes2(PWord(Cardinal(Data) + 4)^) - PlayerPos.Y;
  PlayerPosDelta.Direction := PByte(Cardinal(Data) + 6)^ AND $7F;
  PlayerPosDelta.Z := PShortInt(Cardinal(Data) + 7)^ - PlayerPos.Z;
  Result := False;
  API.AfterPacketCallback(OnPlayerPositionChanged, nil);
End;

// 0x1B Login Confirm (http://ruosi.org/packetguide/index.xml#server1B)
// BYTE PacketID = 0x1B
// DWORD Serial
// DWORD Dummy = 0x00000000
// WORD Body
// WORD X
// WORD Y
// WORD Z
// BYTE Direction
// BYTE Dummy = 0x00
// DWORD Dummy = 0xFFFFFFFF
// WORD Dummy = 0x0000
// WORD Dummy = 0x0000
// WORD MapWidth
// WORD MapHeight
// BYTE[6] Dummy = 0x00[6]
function LoginConfirm(Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean; stdcall;
Begin
  PlayerSerial := SwapBytes4(PDWord(Cardinal(Data) + 1)^);

  PlayerPosDelta.X := SwapBytes2(PWord(Cardinal(Data) + 11)^);
  PlayerPosDelta.Y := SwapBytes2(PWord(Cardinal(Data) + 13)^);
  PlayerPosDelta.Z := SwapBytes2(PWord(Cardinal(Data) + 15)^);
  PlayerPosDelta.Direction := PByte(Cardinal(Data) + 17)^ AND $7F;
  Result := False;
  API.AfterPacketCallback(OnPlayerPositionChanged, nil);
End;

// 0xBF.08 Map change (http://ruosi.org/packetguide/index.xml#serverBF.08)
// BYTE PacketId = 0xBF
// WORD PacketSize
// WORD BFPacketId = 0x08
// BYTE MapID (0x00 Felucca, 0x01 Trammel, 0x02 Ilshenar, 0x03 Malas, 0x04 Tokuno, 0x05 TerMur)
function BFPacket08(Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean; stdcall;
Begin
  if SwapBytes2(PWord(Cardinal(Data) + 3)^) = $0008 then Begin
    PlayerPosDelta.Map := PByte(Cardinal(Data) + 5)^;
    API.AfterPacketCallback(OnPlayerPositionChanged, nil);
  End;
  Result := False;
End;

// 0x20 Mobile Update (http://ruosi.org/packetguide/index.xml#server20)
// BYTE PacketId = 0x20
// DWORD Serial
// WORD Body
// BYTE Dummy = 0x00
// WORD Hue
// BYTE Flags
// WORD X
// WORD Y
// WORD Dummy = 0x0000
// BYTE Direction
// SBYTE Z
function MobileUpdate(Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean; stdcall;
begin
  if SwapBytes4(PDWord(Cardinal(Data) + 1)^) = PlayerSerial then begin
    PlayerPosDelta.X := SwapBytes2(PWord(Cardinal(Data) + 11)^) - PlayerPos.X;
    PlayerPosDelta.Y := SwapBytes2(PWord(Cardinal(Data) + 13)^) - PlayerPos.Y;
    PlayerPosDelta.Direction := PByte(Cardinal(Data) + 17)^ AND $7F;
    PlayerPosDelta.Z := PShortInt(Cardinal(Data) + 18)^ - PlayerPos.Z;
    API.AfterPacketCallback(OnPlayerPositionChanged, nil);
  end;
  Result := False;
end;

procedure RegisterHandlers(AnAPI: TPluginApi);
Begin
  Initialized := Callbacks_Length > 0;
  if not Initialized then Exit;

  API := AnAPI;
  API.RegisterPacketHandler($02, @ClientWalkRequest);
  API.RegisterPacketHandler($1B, @LoginConfirm);
  API.RegisterPacketHandler($20, @MobileUpdate);
  API.RegisterPacketHandler($21, @ServerWalkReject);
  API.RegisterPacketHandler($BF, @BFPacket08);
  ZeroMemory(@PlayerPos, SizeOf(TClientPosition));
  ZeroMemory(@PlayerPosDelta, SizeOf(TClientPositionDelta));
End;

procedure UnRegisterHandlers;
Begin
  if not Initialized then Exit;

  API.UnRegisterPacketHandler($02, @ClientWalkRequest);
  API.UnRegisterPacketHandler($1B, @LoginConfirm);
  API.UnRegisterPacketHandler($20, @MobileUpdate);
  API.UnRegisterPacketHandler($21, @ServerWalkReject);
  API.UnRegisterPacketHandler($BF, @BFPacket08);
End;


initialization
  SetLength(Callbacks, 0);
  FreeCallbacks := 0;
  Callbacks_Length := 0;
end.
