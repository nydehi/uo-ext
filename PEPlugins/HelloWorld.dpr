library HelloWorld;

uses
  plgHelloWorld in 'plgHelloWorld.pas',
  AbstractThread in '..\UOExt\AbstractThread.pas',
  PluginAPI in 'Common\PluginAPI.pas',
  PluginsShared in '..\UOExt\PluginsShared.pas';

{$E plg}
exports PluginAPI.DllInit, PluginAPI.DllInitDone;
begin
end.
 