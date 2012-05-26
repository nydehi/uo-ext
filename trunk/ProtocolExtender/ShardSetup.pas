unit ShardSetup;

interface

uses WinSock;

type
  TSerialSupplyMethods = (ssmStatic, ssmProxy, ssmServer);

var
  UpdateIP: Cardinal = 0;
  UpdatePort: Word = 0;
  PersistentConnect: Boolean = False;
  UsingUpdateServer: Boolean = False;

  SerialSupplyMethod: TSerialSupplyMethods = ssmStatic;
  ItemSerialMin:Cardinal = $70000001;
  ItemSerialMax:Cardinal = $7FFFFFFF;
  MobileSerialMin:Cardinal = $30000001;
  MobileSerialMax:Cardinal = $3FFFFFFF;
  Encrypted:Boolean = False;
  EnableIntertalProtocol: Boolean = False;
  InternalProtocolHeader: Byte = $FF;

  GUIDLLName: AnsiString = 'UOExt.GUI.dll';
implementation

end.
