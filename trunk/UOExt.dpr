library UOExt;

uses
  Windows,
  AbstractThread in 'UOExt\AbstractThread.pas',
  Common in 'Common\Common.pas',
  PacketStream in 'UOExt\PacketStream.pas',
  HuffmanAlgo in 'UOExt\HuffmanAlgo.pas',
  ProtocolDescription in 'UOExt\ProtocolDescription.pas',
  ClientThread in 'UOExt\ClientThread.pas',
  HookLogic in 'UOExt\HookLogic.pas',
  ShardSetup in 'UOExt\ShardSetup.pas',
  Plugins in 'UOExt\Plugins.pas',
  PluginsShared in 'UOExt\PluginsShared.pas',
  zLib in 'UOExt\zLib.pas',
  Encryption in 'UOExt\Encryption.pas',
  DCPcrypt in 'Common\DCPcrypt.pas',
  Twofish in 'Common\Twofish.pas',
  PreConnectIPDiscover in 'UOExt\PreConnectIPDiscover.pas',
  CoreInitialization in 'UOExt\CoreInitialization.pas',
  Executable in 'UOExt\Executable.pas',
  APIHooker in 'UOExt\APIHooker.pas',
  GUI in 'UOExt\GUI.pas',
  md5 in 'Common\md5.pas',
  PluginsExceptionHandling in 'UOExt\PluginsExceptionHandling.pas';

exports
  CoreInitialization.CoreInitialize
  , Executable.Infect32
  , Executable.InfectA
  , Executable.InfectW
  , Executable.ExtractXMLMeta32
;

begin
end.
