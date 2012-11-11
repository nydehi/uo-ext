unit UOExt.Utility.Bindings;

interface

type
  TClientPosition=packed record
    X: Word;
    Y: Word;
    Direction: Byte;
    Z: ShortInt;
    Map: Byte;
  end;
  PClientPosition = ^TClientPosition;

  TClientPositionDelta=packed record
    X: Integer;
    Y: Integer;
    Direction: Byte;
    Z: SmallInt;
    Map: Byte;
  end;
  PClientPositionDelta = ^TClientPositionDelta;

  TClientInfo=packed record
    Parameter: Cardinal;
    case Integer of
      0: (Value: Cardinal);
      1: (Data: Pointer);
  end;

  TClientInformation=packed record
    Amount: Cardinal;
    Information: Array [0..0] of TClientInfo;
  end;
  PClientInformation=^TClientInformation;

  TOnPositionChanged = procedure(OldPosition: PClientPosition; Delta: PClientPositionDelta; AParam: Pointer); stdcall;

  TAddOnPositionChanged = function(Callback: TOnPositionChanged; AParam: Pointer): Cardinal; stdcall;
  TRemoveOnPositionChanged = procedure(AHandle: Cardinal); stdcall;

  TIsAddressFromExecutable = function (AnAddress: Cardinal): Boolean; stdcall;

  TGetClientInformation = function:PClientInformation; stdcall;

  TAskForMulMapping = procedure; stdcall;

  TMappingRec = packed record
    FileName: PAnsiChar;
    FileHandle: THandle;
    FileLength: Cardinal;
    MappingHandle: THandle;
    MappingLength: Cardinal;
    AskedFreeSpace: Cardinal;
    MappingPointer: Pointer;
  end;
  PMappingRec = ^TMappingRec;

  TGetMulMappingInfo = function(AMulName:PAnsiChar):PMappingRec; stdcall;
  TEnshureFreeMappedSpace = function(AMulName: PAnsiChar; Amount: Cardinal):Boolean; stdcall;


  TCli_Map_Info=packed record
    Name: PAnsiChar;
    Width: Word;
    Height: Word;
    MainWidth: Word;
    MainHeight: Word;
    ClientRecord: Pointer;
  end;
  TCli_Map_Info_Data=packed record
    Amount: Cardinal;
    Maps: Array [0..0] of TCli_Map_Info;
  end;
  PCli_Map_Info_Data = ^TCli_Map_Info_Data;

const
  CLI_MAPS_INFO = 1;

implementation

end.
