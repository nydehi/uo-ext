library SyncStatic;

{$E plg}
uses
  PluginAPI in 'Common\PluginAPI.pas',
  PluginsShared in '..\UOExt\PluginsShared.pas',
  Common in '..\Common\Common.pas',
  plgSyncStatic in 'plgSyncStatic.pas',
  ssStatic in 'SyncStatic\ssStatic.pas',
  UOExt.Utility.Bindings in 'UOExt.Utility\UOExt.Utility.Bindings.pas',
  ssShared in 'SyncStatic\ssShared.pas',
  MulWork in 'Common\MulWork.pas';

exports PluginAPI.DllInit, PluginAPI.DllInitDone;
begin
end.
