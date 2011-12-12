unit ShardSetup;

interface

{$IFNDEF PLUGINS_SERVER}
  {$UNDEF LOGIN_CFG}
{$ENDIF}

type
  TSerialSupplyMethods = (ssmStatic, ssmProxy, ssmServer);

var
  LoginIP:AnsiString = '127.0.0.1';
  LoginPort:Word = 2593;
  {$IFDEF PLUGINS_SERVER}
  UpdateIP: AnsiString = '127.0.0.1';
  UpdatePort: Word = 2594;
  {$ENDIF}
  SerialSupplyMethod: TSerialSupplyMethods = ssmStatic;
  ItemSerialMin:Cardinal = $70000001;
  ItemSerialMax:Cardinal = $7FFFFFFF;
  MobileSerialMin:Cardinal = $30000001;
  MobileSerialMax:Cardinal = $3FFFFFFF;
  Encrypted:Boolean = False;
  EnableIntertalProtocol: Boolean = False;
  InternalProtocolHeader: Byte = $FF;


  ReadLoginCfg: Boolean = {$IFDEF LOGIN_CFG}True{$ELSE}False{$ENDIF};

implementation

end.
