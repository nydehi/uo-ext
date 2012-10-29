unit CoreInitialization;

interface

function CoreInitialize:Byte; stdcall;

implementation

uses Windows, HookLogic, Plugins, Common, WinSock2, ShardSetup, zLib,
  GUI, ProtocolDescription, PreConnectIPDiscover, Qos;

{$IFDEF DEBUG}
  {$DEFINE DEBUGWINDOW}
{$ENDIF}

{$IFDEF DEBUGWINDOW}
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

procedure CreateConsole;
{$IFDEF DEBUGWINDOW}
var
  AlreadyAllocated: Boolean;
{$ENDIF}
Begin
  {$IFDEF DEBUGWINDOW}
  AlreadyAllocated := GetFileType(GetStdHandle(STD_OUTPUT_HANDLE)) <> FILE_TYPE_UNKNOWN;
  If not AlreadyAllocated Then AllocConsole;
  TTextRec(Output).Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  TTextRec(ErrOutput).Handle := GetStdHandle(STD_ERROR_HANDLE);
  SetConsoleCtrlHandler(@HandlerRoutine, True);
  oldDllProc := DllProc;
  DllProc := @Terminator;

  if AlreadyAllocated then Begin
    WriteLn;
    WriteLn('UOExt.dll was successfully updated (or it think so).');
    WriteLn;
  End;
  WriteLn('UOExt.dll Ultima Online (C) protocol and client patch system.');
  WriteLn('Core: Debug window started.');
  WriteLn;
  Write('Core: Compile time directives: DEBUGWINDOW');
  {$IFDEF DEBUG} Write(', DEBUG'); {$ENDIF}
  {$IFDEF RELEASE} Write(', RELEASE');{$ENDIF}
  {$IFDEF WRITELOG} Write(', WRITELOG');{$ENDIF}
  WriteLn;
  {$ELSE}
  If GetFileType(GetStdHandle(STD_OUTPUT_HANDLE)) <> FILE_TYPE_UNKNOWN Then FreeConsole;
  {$ENDIF}
End;

procedure CriticalError(Msg: AnsiString);
Begin
  {$IFDEF Debug}
  WriteLn(Msg);
  WriteLn('Press any key to exit.');
  Readln;
  {$ELSE}
  Msg := Msg + #0;
  MessageBoxA(0, @Msg[1], nil, MB_OK);
  {$ENDIF}
  Halt(1);
End;

procedure GetIPOverride;
var
  Host, sPort: AnsiString;
  rHost: PHostEnt;
  WSAData: TWSAData;
Begin
  if ParamCount >= 1 then Begin
    WSAStartup($0202, WSAData);
    Host := Split2(':', AnsiString(ParamStr(1)), sPort);
    Host := Host + #0;
    rHost := gethostbyname(@Host[1]);
    ShardSetup.Port := htons(StrToInt(sPort));
    ShardSetup.IP := PCardinal(rHost.h_addr_list)^;
    WSACleanup;
  End;
End;

function InProcess:Byte;
var
  uMainLine: Cardinal;
  uStatusLine: Cardinal;
  MasterResult: Cardinal;
Begin
// Create console if needed
  CreateConsole;

// Create GUI
  GUI.CurrGUI := TGUI.Create;
  GUI.CurrGUI.Init(ShardSetup.UOExtBasePath + ShardSetup.GUIDLLName);

  uMainLine := GUI.GUISetLog(0, $FFFFFFFF, $FFFFFFFF, 'Loading ... ');
  GUI.GUISetLog(0, $FFFFFFFF, uMainLine, 'Initializing');

  ShardSetup.UOExtBasePath := ExtractFilePath(AnsiString(ParamStr(0)));
  DeleteFileA(PAnsiChar(ShardSetup.UOExtBasePath + 'UOExt.packetlog.log')); // Delete file, if any
  GetIPOverride; // Check for IP override
  HookLogic.ReadExecutableSections; // Read all code sections for futher hook
  HookLogic.TransServerPort := PreConnectIPDiscover.GetTransServPort; // For 2.0.3 - avoid TransServ connection
  ProtocolDescription.Init; // Try to fill protocol table from client

  PluginSystem := TPluginSystem.Create;
  MasterResult := PluginSystem.LoadMasterLibrary(ShardSetup.UOExtBasePath + 'UOExt\Master.plg');

  case MasterResult of
    1: Begin
      {$IFDEF DEBUG}
      Writeln('Core: Core has been updated. Reloading...');
      {$ENDIF}
      Result := 1;
      Exit;
    End;
    2: Begin
      {$IFDEF DEBUG}
      WriteLn('Core: GUI has been updated.');
      {$ENDIF}
      GUI.CurrGUI.Replace;
      GUI.CurrGUI.Free;
      GUI.CurrGUI := TGUI.Create;
      uMainLine := $FFFFFFFF;
    End;
    3: Begin
      CriticalError('Core: Failed to self-update. Critical!');
    End;
  end;

// WinAPI hook needed, before plugins init.
  uStatusLine := GUI.GUISetLog(0, $FFFFFFFF, uMainLine, 'Hooking');
  {$IFDEF DEBUG}
  Write('Core: Hooking APIs for launch ... ');
  {$ENDIF}
  HookIt;

  // Ok. All Init is done! Loading plugins.
  GUI.GUISetLog(0, uStatusLine, uMainLine, 'Initializing plugins.');
  {$IFDEF DEBUG}
  WriteLn('done.');
  WriteLn('Core: Starting plug-ins loading.');
  {$ENDIF}
  PluginSystem.Init;

// Clean GUI. It's need only on Initialize process.
  GUI.CurrGUI.Free;
  GUI.CurrGUI := nil;

  Result := 0;
End;

function CoreInitialize:Byte; stdcall;
Begin
  Result := InProcess;
  {$IFDEF DEBUGWINDOW}
  if Result = 2 then FreeConsole;
  {$ENDIF}
  if Assigned(GUI.CurrGUI) then GUI.CurrGUI.Free;
End;

end.
