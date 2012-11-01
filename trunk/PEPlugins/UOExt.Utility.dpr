library UOExt.Utility;

{$E plg}
uses
  PluginAPI in 'Common\PluginAPI.pas',
  PluginsShared in '..\UOExt\PluginsShared.pas',
  Common in '..\Common\Common.pas',
  PlayerPosition in 'UOExt.Utility\PlayerPosition.pas',
  plg_UOExt.Utility in 'plg_UOExt.Utility.pas',
  UOExt.Utility.Bindings in 'UOExt.Utility\UOExt.Utility.Bindings.pas',
  ExecutableSections in '..\Common\ExecutableSections.pas',
  ClientInformation in 'UOExt.Utility\ClientInformation.pas',
  MulMapper in 'UOExt.Utility\MulMapper.pas',
  APIHooker in '..\UOExt\APIHooker.pas';

exports PluginAPI.DllInit, PluginAPI.DllInitDone;
begin
end.
