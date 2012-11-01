unit plgHddLoad;

interface

implementation

uses Windows, PluginAPI, PluginsShared, Common;

var
  API: TPluginApi;

Procedure LoadPluginLibraries;
var
  Path, Lib : AnsiString;
  Dat: _WIN32_FIND_DATAA;
  hFind: THandle;
Begin
  Path := ExtractFilePath(AnsiString(ParamStr(0))) + 'UOExt\Plugins\*.plg' + #0;
  hFind := FindFirstFileA(@path[1], Dat);
  if hFind <> INVALID_HANDLE_VALUE then Repeat
    Lib := ExtractFilePath(AnsiString(ParamStr(0))) + 'UOExt\Plugins\' + Dat.cFileName + #0;
    API.LoadPluginsLibrary(@lib[1]);
  Until not FindNextFileA(hFind, Dat);
End;

function PluginInit(APluginEvent: Cardinal; APluginEventData: Pointer): Boolean; stdcall;
Begin
  if APluginEvent = PE_MASTERINIT then Begin
    API := TPluginApi.Create;
    Result := API.HandlePluginEvent(PE_INIT, PPE_MasterPluginInit(APluginEventData)^.API);
    LoadPluginLibraries;
    PPE_MasterPluginInit(APluginEventData)^.Result := 0;
  End Else Begin
    Result := API.HandlePluginEvent(APluginEvent, APluginEventData);
  End;
  if APluginEvent = PE_FREE Then API.Free;
End;

type
  TMyDescription = packed record
    InitProcedure: Pointer;
    Size: Cardinal;
    Data: Array [0..0] of TPluginDescriptor;
  end;

const
  Name: AnsiString = 'Master: Hdd Load' + #0;
  Description:TMyDescription = (
    InitProcedure : @PluginInit;
    Size: 1;
    Data: (
      ( Descriptor: PD_NAME;                    Value: 0 )
    )
  );


initialization
  PPointer(@Description.Data[0].Value)^ := @Name[1];

  PluginAPI.AddPlugin(@Description);
end.
