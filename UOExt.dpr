library UOExt;

uses
  Windows,
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
  Executable in 'UOExt\Executable.pas',
  APIHooker in 'UOExt\APIHooker.pas',
  GUI in 'UOExt\GUI.pas',
  Updater in 'UOExt\Updater.pas';

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
