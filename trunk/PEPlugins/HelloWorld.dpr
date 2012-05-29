library HelloWorld;

uses
  PluginsShared in '..\UOExt\PluginsShared.pas',
  PluginAPI in 'Common\PluginAPI.pas',
  plgHelloWorld in 'plgHelloWorld.pas';

{$E plg}
exports PluginAPI.DllInit, PluginAPI.DllInitDone;
begin
end.
 