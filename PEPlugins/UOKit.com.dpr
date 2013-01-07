library UOKit.com;

{$E plg}

uses
  PluginAPI in 'Common\PluginAPI.pas',
  plgUOKit.com in 'plgUOKit.com.pas',
  PluginsShared in '..\UOExt\PluginsShared.pas',
  APIHooker in '..\UOExt\APIHooker.pas',
  Top in 'UOKit.com\Top.pas',
  Common in '..\Common\Common.pas',
  AbstractThread in '..\UOExt\AbstractThread.pas';

exports PluginAPI.DllInit, PluginAPI.DllInitDone;
begin
end.
