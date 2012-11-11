unit plg_UOExt.Utility;

interface

uses Windows, WinSock, PluginAPI, PluginsShared;

var
  API: TPluginApi;

implementation

uses PlayerPosition, ExecutableSections, ClientInformation, MulMapper;

function IsAddressFromExecutable(AnAddress: Cardinal): Boolean; stdcall;
Begin
  Result := ExecutableSections.IsAddressFromExecutable(AnAddress);
End;

procedure Init;
Begin
  AskForMulMapping;
End;

procedure Clear;
Begin
  FreeMulMapping;
End;

procedure Network_Start;
Begin
  PlayerPosition.RegisterHandlers(API);
End;

Procedure Network_End;
Begin
  PlayerPosition.UnRegisterHandlers;
End;

function PluginInit(APluginEvent: Cardinal; APluginEventData: Pointer): Boolean; stdcall;
Begin
  if APluginEvent = PE_INIT then API := TPluginApi.Create;
  Result := API.HandlePluginEvent(APluginEvent, APluginEventData);
  case APluginEvent of
    PE_INIT: Init;
    PE_PROXYSTART: Network_Start;
    PE_PROXYEND: Network_End;
    PE_FREE : Clear;
  end;
  if APluginEvent = PE_FREE Then API.Free;
End;

type
  TMyDescription = packed record
    InitProcedure: Pointer;
    Size: Cardinal;
    Data: Array [0..1] of TPluginDescriptor;
  end;
  TMyPluginAPIInfo=packed record
    Count: Cardinal;
    API: Array [0..6] of PPluginAPIEntry;
  end;

const
  Name_AddOnPositionChanged: Array [0..20] of AnsiChar = ('A','d','d','O','n','P','o','s','i','t','i','o','n','C','h','a','n','g','e','d',#0 );
  Name_RemoveOnPositionChanged: Array [0..23] of AnsiChar = ( 'R','e','m','o','v','e','O','n','P','o','s','i','t','i','o','n','C','h','a','n','g','e','d',#0 );
  Name_Plugin: Array [0..13] of AnsiChar = ( 'U','O','E','x','t','.','U','t','i','l','i','t','y',#0 );
  Name_IsAddressFromExecutable: Array [0..23] of AnsiChar = ( 'I','s','A','d','d','r','e','s','s','F','r','o','m','E','x','e','c','u','t','a','b','l','e',#0 );
  Name_GetClientInformation: Array [0..20] of AnsiChar = ( 'G','e','t','C','l','i','e','n','t','I','n','f','o','r','m','a','t','i','o','n',#0 );
  Name_AskForMulMapping: Array [0..16] of AnsiChar = ( 'A','s','k','F','o','r','M','u','l','M','a','p','p','i','n','g',#0 );
  Name_GetMulMappingInfo: Array [0..17] of AnsiChar = ( 'G','e','t','M','u','l','M','a','p','p','i','n','g','I','n','f','o',#0 );
  Name_EnshureFreeMappedSpace: Array [0..22] of AnsiChar = ( 'E','n','s','h','u','r','e','F','r','e','e','M','a','p','p','e','d','S','p','a','c','e',#0 );

  API_AddOnPositionChanged: TPluginAPIEntry = (
    AName: @Name_AddOnPositionChanged;
    AnAPI: @AddOnPositionChanged;
    Flags: UF_INPROXY;
  );
  API_RemoveOnPositionChanged: TPluginAPIEntry = (
    AName: @Name_RemoveOnPositionChanged;
    AnAPI: @RemoveOnPositionChanged;
    Flags: UF_INPROXY;
  );
  API_IsAddressFromExecutable: TPluginAPIEntry = (
    AName: @Name_IsAddressFromExecutable;
    AnAPI: @IsAddressFromExecutable;
    Flags: UF_ALLTHREADS;
  );
  API_GetClientInformation: TPluginAPIEntry = (
    AName: @Name_GetClientInformation;
    AnAPI: @GetClientInformation;
    Flags: 0;
  );
  API_AskForMulMapping: TPluginAPIEntry = (
    AName: @Name_AskForMulMapping;
    AnAPI: @AskForMulMapping;
    Flags: UF_INCLIENT;
  );
  API_GetMulMappingInfo: TPluginAPIEntry = (
    AName: @Name_GetMulMappingInfo;
    AnAPI: @GetMulMappingInfo;
    Flags: UF_INPROXY;
  );
  API_EnshureFreeMappedSpace: TPluginAPIEntry = (
    AName: @Name_EnshureFreeMappedSpace;
    AnAPI: @EnshureFreeMappedSpace;
    Flags: UF_INPROXY;
  );

  PluginAPIInfo:TMyPluginAPIInfo = (
    Count: 7;
    API: (
        @API_AddOnPositionChanged
      , @API_RemoveOnPositionChanged
      , @API_IsAddressFromExecutable
      , @API_GetClientInformation
      , @API_AskForMulMapping
      , @API_GetMulMappingInfo
      , @API_EnshureFreeMappedSpace
    )
  );

  Description:TMyDescription = (
    InitProcedure : @PluginInit;
    Size: 2;
    Data: (
      ( Descriptor: PD_NAME;                    Data: @Name_Plugin )
     ,( Descriptor: PD_APIEXPORT;               Data: @PluginAPIInfo )
    )
  );


initialization
  PluginAPI.AddPlugin(@Description);
end.
