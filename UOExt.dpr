library UOExt;

uses
  Windows,
<<<<<<< .mine
  AbstractThread in 'ProtocolExtender\AbstractThread.pas',
  ListeningThread in 'ProtocolExtender\ListeningThread.pas',
  Common in 'ProtocolExtender\Common.pas',
  PacketStream in 'ProtocolExtender\PacketStream.pas',
  HuffmanAlgo in 'ProtocolExtender\HuffmanAlgo.pas',
  ProtocolDescription in 'ProtocolExtender\ProtocolDescription.pas',
  ClientThread in 'ProtocolExtender\ClientThread.pas',
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
  UOExtProtocol in 'ProtocolExtender\UOExtProtocol.pas',
  PreConnectIPDiscover in 'ProtocolExtender\PreConnectIPDiscover.pas',
  CoreInitialization in 'ProtocolExtender\CoreInitialization.pas',
  Executable in 'ProtocolExtender\Executable.pas',
  APIHooker in 'ProtocolExtender\APIHooker.pas',
  GUI in 'ProtocolExtender\GUI.pas';
=======
  AbstractThread in 'UOExt\AbstractThread.pas',
  ListeningThread in 'UOExt\ListeningThread.pas',
  Common in 'UOExt\Common.pas',
  PacketStream in 'UOExt\PacketStream.pas',
  HuffmanAlgo in 'UOExt\HuffmanAlgo.pas',
  ProtocolDescription in 'UOExt\ProtocolDescription.pas',
  ClientThread in 'UOExt\ClientThread.pas',
  HookLogic in 'UOExt\HookLogic.pas',
  ShardSetup in 'UOExt\ShardSetup.pas',
  Plugins in 'UOExt\Plugins.pas',
  PluginsShared in 'UOExt\PluginsShared.pas',
  Serials in 'UOExt\Serials.pas',
  zLib in 'UOExt\zLib.pas',
  Encryption in 'UOExt\Encryption.pas',
  DCPcrypt in 'UOExt\DCPcrypt.pas',
  Twofish in 'UOExt\Twofish.pas',
  md5 in 'UOExt\md5.pas',
  UOExtProtocol in 'UOExt\UOExtProtocol.pas',
  PreConnectIPDiscover in 'UOExt\PreConnectIPDiscover.pas',
  CoreInitialization in 'UOExt\CoreInitialization.pas',
  PluginsDownloader in 'UOExt\PluginsDownloader.pas',
  Executable in 'UOExt\Executable.pas',
  APIHooker in 'UOExt\APIHooker.pas';
>>>>>>> .r60

exports
  CoreInitialization.CoreInitialize
  , Executable.RunUOReg name 'RunUOReg'
  , Executable.RunUOA name 'RunUOA'
  , Executable.RunUOW name 'RunUOW'
  , Executable.StartSuspendedA
  , Executable.StartSuspendedW
  , Executable.InjectThisDllA
  , Executable.InjectThisDllW
  , Executable.InjectDllA
  , Executable.InjectDllW
  , Executable.ResumeLoading
  , Executable.RunUO32
  , Executable.Infect32
  , Executable.InfectA
  , Executable.InfectW
;

begin
end.
