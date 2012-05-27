library UOExt.gui;

uses
  GUIThread in 'UOExt.gui\GUIThread.pas',
  AbstractThread in 'UOExt\AbstractThread.pas',
  UOExt.GUI.API in 'UOExt.gui\UOExt.GUI.API.pas',
  PluginsShared in 'UOExt\PluginsShared.pas';

{$R UOExt.gui.res}

exports UOExt.GUI.API.Init, UOExt.GUI.API.Free, UOExt.GUI.API.SetLog, UOExt.GUI.API.StartProcess, UOExt.GUI.API.UpdateProcess;

begin
end.
