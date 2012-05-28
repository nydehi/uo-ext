library HelloWorld;

uses
  PluginsShared in '..\UOExt\PluginsShared.pas',
  PluginAPI in 'Common\PluginAPI.pas',
  uMain in 'HelloWorld\uMain.pas';

{$E plg}
exports PluginAPI.DllInit, PluginAPI.DllInitDone;
begin
end.
 