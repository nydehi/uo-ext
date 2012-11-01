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

  TClientPositionDelta=packed record
    X: Integer;
    Y: Integer;
    Direction: Byte;
    Z: SmallInt;
    Map: Byte;
  end;

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

  TOnPositionChanged = procedure(OldPosition: TClientPosition; Delta: TClientPositionDelta; AParam: Pointer); stdcall;

  TAddOnPositionChanged = function(Callback: TOnPositionChanged; AParam: Pointer): Cardinal; stdcall;
  TRemoveOnPositionChanged = procedure(AHandle: Cardinal); stdcall;

  TIsAddressFromExecutable = function (AnAddress: Cardinal): Boolean; stdcall;

  TGetClientInformation = function:PClientInformation; stdcall;

  TAskForMulMapping = procedure; stdcall;

  TMappingRec = packed record
    FileName: PAnsiChar;
    FileHandle: THandle;
    MappingHandle: THandle;
    MappingLength: Cardinal;
    MappingPointer: Pointer;
  end;
  PMappingRec = ^TMappingRec;

  TGetMulMappingInfo = function(AMulName:PAnsiChar):PMappingRec; stdcall;


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
