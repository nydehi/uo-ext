unit ShardSetup;

interface

uses WinSock;

type
  TSerialSupplyMethods = (ssmStatic, ssmProxy, ssmServer);

var
  UpdateIP: Cardinal = INADDR_LOOPBACK;
  UpdatePort: Word = 2594;
  PersistentConnect: Boolean = False;

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
