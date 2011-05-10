unit ShardSetup;

interface

type
  TSerialSupplyMethods = (ssmStatic, ssmProxy, ssmServer);

var
  LoginIP:AnsiString = '127.0.0.1';
  LoginPort:Word = 2593;
  SerialSupplyMethod: TSerialSupplyMethods = ssmStatic;
  ItemSerialMin:Cardinal = $70000001;
  ItemSerialMax:Cardinal = $7FFFFFFF;
  MobileSerialMin:Cardinal = $30000001;
  MobileSerialMax:Cardinal = $3FFFFFFF;
  Encrypted:Boolean = False;
  EnableIntertalProtocol: Boolean = False;
  InternalProtocolHeader: Byte = $FF;

implementation

end.
