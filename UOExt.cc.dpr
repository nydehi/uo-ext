program UOExt.cc;

{$APPTYPE CONSOLE}


uses
  Executable in 'UOExt.cc\Executable.pas',
  Common in 'Common\Common.pas',
  PluginsShared in 'UOExt\PluginsShared.pas';

procedure Help;
Begin
  Writeln('Usage:');
  Writeln('  ', Paramstr(0), ' <COMMAND> [<ARGUMENT>]');
  Writeln;
  Writeln('Params:');
  Writeln('  <COMMAND> :');
  Writeln('    /HELP   - Prints this message.');
  Writeln('    /INFECT - Infects <ARGUMENT> executable with UOExt.dll call at start.');
  Writeln('    /META   - Extracts meta information from <ARGUMENT> UOExt plugin.');
  Writeln('    /META   - And writes it in XML format.');
End;

var
  Command: AnsiString;
  Argument: AnsiString;
begin
  Writeln('UOExt Commandline Console utilitary propgram');
  Writeln('Copyright 2012 Warstone. RF.');
  Writeln;
  Command := UpperCase(AnsiString(ParamStr(1)));
  Argument := UpperCase(AnsiString(ParamStr(2)));
  if (Command = '/HELP') OR (Command = '/H') OR (Command = '/?') then Help
  Else if (Command = '/INFECT') then Begin
    Argument := Argument + #0;
    InfectA(@Argument[1]);
  End Else if (Command = '/META') then ExtractXMLMeta32(Argument)
  Else
    Help;
end.
