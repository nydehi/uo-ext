library MP.HddLoad;

uses
  PluginAPI in 'Common\PluginAPI.pas',
  plgHddLoad in 'MP.HddLoad\plgHddLoad.pas',
  PluginsShared in '..\UOExt\PluginsShared.pas',
  Common in '..\Common\Common.pas';

{$E plg}
exports PluginAPI.DllInit, PluginAPI.DllInitDone;
begin
end.
