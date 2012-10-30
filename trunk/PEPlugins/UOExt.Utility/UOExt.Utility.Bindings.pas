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

  TOnPositionChanged = procedure(OldPosition: TClientPosition; Delta: TClientPositionDelta; AParam: Pointer); stdcall;

  TAddOnPositionChanged = function(Callback: TOnPositionChanged; AParam: Pointer): Cardinal; stdcall;
  TRemoveOnPositionChanged = procedure(AHandle: Cardinal); stdcall;

implementation

end.
