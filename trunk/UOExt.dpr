library UOExt;

{$R *.res}

uses
  Windows,
  AbstractThread in 'ProtocolExtender\AbstractThread.pas',
  ListeningThread in 'ProtocolExtender\ListeningThread.pas',
  Common in 'ProtocolExtender\Common.pas',
  PacketStream in 'ProtocolExtender\PacketStream.pas',
  HuffmanAlgo in 'ProtocolExtender\HuffmanAlgo.pas',
  ProtocolDescription in 'ProtocolExtender\ProtocolDescription.pas',
  ClientThread in 'ProtocolExtender\ClientThread.pas',
  APIHooker in 'ProtocolExtender\APIHooker.pas',
  HookLogic in 'ProtocolExtender\HookLogic.pas',
  ShardSetup in 'ProtocolExtender\ShardSetup.pas',
  Plugins in 'ProtocolExtender\Plugins.pas',
  PluginsShared in 'ProtocolExtender\PluginsShared.pas',
  Serials in 'ProtocolExtender\Serials.pas',
  zLib in 'ProtocolExtender\zLib.pas',
  Encryption in 'ProtocolExtender\Encryption.pas',
  DCPcrypt in 'ProtocolExtender\DCPcrypt.pas',
  Twofish in 'ProtocolExtender\Twofish.pas',
  md5 in 'ProtocolExtender\md5.pas',
  CoreInitialization in 'ProtocolExtender\CoreInitialization.pas';

exports CoreInitialization.CoreInitialize;

begin
end.
