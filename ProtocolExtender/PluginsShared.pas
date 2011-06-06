unit PluginsShared;

interface

const
  PF_REGISTERPACKETHANDLER = 1;
  PF_UNREGISTERPACKETHANDLER = 2;
  PF_REGISTERPACKETTYPE = 3;
  PF_SENDPACKET = 4;
  PF_GETNEWSERIAL = 5;
  PF_FREESERIAL = 6;
  PF_GETSERVERSERIAL = 7;
  PF_GETCLIENTSERIAL = 8;
  PF_REGISTERSYNCEVENTHANDLER = 9;
  PF_ASKSYNCEVENT = 10;
  PF_ZLIBCOMPRESS2 = 11;
  PF_ZLIBDECOMPRESS = 12;
  PF_AFTERPACKETCALLBACK = 13;

type
  TPacketHandler = function (Data: Pointer; var Size:Cardinal; var Send: Boolean; IsFromServerToClient: Boolean):Boolean; stdcall;
  (**
    Send - Send this package to reciver.
    Result - If true - this event breaks bubbling.
    If Send = false then Result forced to true.
  **)

  TPacketSendedCallback = procedure(APackeHead: Byte; lParam: Pointer; IsFromServerToClient: Boolean); stdcall;

  TPacketLengthDefinition=function(Packet:Pointer; Length:Cardinal):Cardinal; stdcall;
  (**
    In this procedure you can't register or unregister handlers.
  **)

  RAPIFunc = packed record
    FuncType: Cardinal;
    Func: Pointer;
  End;
  PAPIFunc=^RAPIFunc;

  TUOExtInit = procedure (APICount: Cardinal; anAPIFunc: PAPIFunc) stdcall;
  TProxyStart = procedure; stdcall;
  TProxyEnd = procedure; stdcall;

  TRegisterPacketHandler = procedure(Header:Byte; Handler: TPacketHandler) stdcall;
  TUnRegisterPacketHandler = procedure(Header: Byte; Handler: TPacketHandler) stdcall;
  TRegisterPacketType = procedure(IsCliServ:Boolean; Header:Byte; Size:Word; HandleProc: TPacketLengthDefinition) stdcall;
  (**
    If HandleProc <> nil Than Size will be ignored.
    If Size = 0 and HandleProc = nil then default handler will work.
    Default handler think that packet size is word right after Header (2-nd and 3-d byte of the packet)

    If you want to make your own definitor (i.e:
      BYTE UOHeader
      BYTE YourHeader
      [WORD PacketSize] // Only if Packet has various size
      BYTES Data
    ) than you just need to write it ;)
  **)
  TSendPacket = function(Packet: Pointer; Length: Cardinal; ToServer, Direct: Boolean; var Valid: Boolean):Boolean; stdcall;
  TGetNewSerial = function(IsMobile:Boolean): Cardinal; stdcall;
  TFreeSerial = procedure(Serial: Cardinal); stdcall;
  TGetServerSerial = function(Serial:Cardinal):Cardinal; stdcall;
  TGetClientSerial = function(Serial:Cardinal):Cardinal; stdcall;
  TAfterPacketCallback = procedure(ACallBack: TPacketSendedCallback; lParam: Pointer); stdcall;

  TSyncEvent = procedure; stdcall;
  TRegisterSyncEventHandler = function(Event: TSyncEvent): Pointer; stdcall;
  TAskSyncEvent = procedure(InterlockedValue: Pointer); stdcall;
  (**
    Can be called from anywhere. Thread safe. (Must be)
  **)

  TzLibCompress2 = function(dest: Pointer; var destLength: Integer; source: Pointer; sourceLength: Integer; quality: Integer):Integer; stdcall;
  TzLibDecompress = function(dest: Pointer; var destLength: Integer; source: Pointer; sourceLength: Integer):Integer; stdcall;

implementation

end.
