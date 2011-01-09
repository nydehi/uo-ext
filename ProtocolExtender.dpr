library ProtocolExtender;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{$R *.res}

uses
  Windows,
  AbstractThread in 'UOProxy\AbstractThread.pas',
  ListeningThread in 'ProtocolExtender\ListeningThread.pas',
  Common in 'UOProxy\Common.pas',
  PacketStream in 'UOProxy\PacketStream.pas',
  HuffmanAlgo in 'UOProxy\HuffmanAlgo.pas',
  ProtocolDescription in 'UOProxy\ProtocolDescription.pas',
  ClientThread in 'UOProxy\ClientThread.pas',
  APIHooker in 'ProtocolExtender\APIHooker.pas',
  HookLogic in 'ProtocolExtender\HookLogic.pas',
  ShardSetup in 'ProtocolExtender\ShardSetup.pas',
  Plugins in 'ProtocolExtender\Plugins.pas',
  PluginsShared in 'ProtocolExtender\PluginsShared.pas',
  Serials in 'ProtocolExtender\Serials.pas',
  zLib in 'ProtocolExtender\zLib.pas';

{$IFDEF DEBUG}
function HandlerRoutine(dwCtrlType: cardinal): bool; stdcall;
begin
  case dwCtrlType of
    CTRL_CLOSE_EVENT: begin
      FreeConsole;
      Result := True;
    end;
    else Result := false;
  end;//of case
end;

var
  oldDllProc : TDLLProc;

procedure Terminator(Reason: Integer);
begin
  If Reason = DLL_PROCESS_DETACH Then
    FreeConsole;
  If Assigned(oldDllProc) Then oldDllProc(Reason);
end;
{$ENDIF}

begin
{$IFDEF DEBUG}
  AllocConsole;
  TTextRec(Output).Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  TTextRec(ErrOutput).Handle := GetStdHandle(STD_ERROR_HANDLE);
  SetConsoleCtrlHandler(@HandlerRoutine, True);
  oldDllProc := DllProc;
  DllProc := @Terminator;
{$ENDIF}
  HookIt;
end.
