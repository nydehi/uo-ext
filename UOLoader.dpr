program UOLoader;

uses
  Forms,
  UOLMain in 'UOLoader\UOLMain.pas' {FMain};

{$R *.res}

var
  DefaultDLLName: String;
begin
  Application.Initialize;
  If ParamCount = 0 Then Application.CreateForm(TFMain, FMain);
  Application.Run;
  If ParamCount >= 1 Then
  Begin
    If ParamCount >= 2 Then Begin
      if (ParamCount >= 3) AND (ParamStr(3) = 'dll') then
        UOLMain.RunUODll(ParamStr(1), ParamStr(2))
      Else
        UOLMain.RunUO(ParamStr(1), ParamStr(2));
    End Else
      UOLMain.RunUO(ParamStr(1), 'UOExt.dll');
    Halt;
  End;
end.
