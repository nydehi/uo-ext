unit plgUOKit.com;

interface

implementation

uses PluginAPI, PluginsShared, Top;

var
  API: TPluginApi;

function PluginInit(APluginEvent: Cardinal; APluginEventData: Pointer): Boolean; stdcall;
Begin
  if APluginEvent = PE_INIT then API := TPluginApi.Create;
  Result := API.HandlePluginEvent(APluginEvent, APluginEventData);
  case APluginEvent of
    PE_INIT: Top.Init(API);
    PE_PROXYSTART: Top.Network_Start(APluginEventData);
    PE_PROXYEND: Top.Network_End;
    PE_FREE : Top.Clear;
  end;
  if APluginEvent = PE_FREE Then API.Free;
End;

type
  TMyDescription = packed record
    InitProcedure: Pointer;
    Size: Cardinal;
    Data: Array [0..0] of TPluginDescriptor;
  end;

const
  Name_Plugin: Array [0..9] of AnsiChar = ( 'U','O','K','i','t','.','c','o','m',#0 );

  Description:TMyDescription = (
    InitProcedure : @PluginInit;
    Size: 1;
    Data: (
      ( Descriptor: PD_NAME;                    Data: @Name_Plugin )
    )
  );


initialization
  PluginAPI.AddPlugin(@Description);
end.
