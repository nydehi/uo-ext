unit ssShared;

interface

uses PluginAPI, UOExt.Utility.Bindings;

var
  AddOnPositionChanged: TAddOnPositionChanged;
  RemoveOnPositionChanged: TRemoveOnPositionChanged;
  GetClientInformation: TGetClientInformation;
  AskForMulMapping: TAskForMulMapping;
  GetMulMappingInfo: TGetMulMappingInfo;

procedure LoadAPIs(AnAPI: TPluginApi);

implementation

procedure LoadAPIs(AnAPI: TPluginApi);
Begin
  AddOnPositionChanged := AnAPI.APISearch('UOExt.Utility', 'AddOnPositionChanged', nil);
  RemoveOnPositionChanged := AnAPI.APISearch('UOExt.Utility', 'RemoveOnPositionChanged', nil);
  GetClientInformation := AnAPI.APISearch('UOExt.Utility', 'GetClientInformation', nil);
  AskForMulMapping := AnAPI.APISearch('UOExt.Utility', 'AskForMulMapping', nil);
  GetMulMappingInfo := AnAPI.APISearch('UOExt.Utility', 'GetMulMappingInfo', nil);
End;

end.
