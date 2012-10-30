library UOExt;

uses
  Windows,
  AbstractThread in 'UOExt\AbstractThread.pas',
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
  APIHooker in 'UOExt\APIHooker.pas',
  GUI in 'UOExt\GUI.pas',
  md5 in 'Common\md5.pas',
  Common in 'Common\Common.pas';

exports
  CoreInitialization.CoreInitialize;

begin
end.
