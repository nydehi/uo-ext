library HelloWorld;

uses
  PluginsShared in '..\ProtocolExtender\PluginsShared.pas',
  PluginAPI in 'Common\PluginAPI.pas',
  uMain in 'HelloWorld\uMain.pas',
  Packet in 'Common\Packet.pas';

{$E plg}
exports InitializationProc;
begin
end.
 