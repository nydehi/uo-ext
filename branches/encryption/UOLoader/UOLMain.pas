unit UOLMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, ExtCtrls, XPman;

type
  TFMain = class(TForm)
    BRun: TButton;
    BPathSelect: TButton;
    CBCloseAfter: TCheckBox;
    LUOPath: TLabel;
    EUOPath: TEdit;
    OD: TOpenDialog;
    EDLLName: TEdit;
    LDLLName: TLabel;
    procedure BRunClick(Sender: TObject);
    procedure BPathSelectClick(Sender: TObject);
    procedure NeedSaveOptions(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    bLoading:Boolean;
    procedure SaveOptions;
    procedure LoadOptions;
    { Private declarations }
  public
    { Public declarations }
  end;

function EnableDebugPrivilege():Boolean;
Function InjectDll(Process: dword; ModulePath: PChar): boolean;
Function RunUO(ExeName, InjectDllName:String):Boolean;
var
  FMain: TFMain;

implementation

{$R *.dfm}

function EnableDebugPrivilege():Boolean;
var
 hToken: dword;
 SeDebugNameValue: Int64;
 tkp: TOKEN_PRIVILEGES;
 ReturnLength: dword;
begin
 Result:=false;
 //Добавляем привилегию SeDebugPrivilege
 //Получаем токен нашего процесса
 OpenProcessToken(INVALID_HANDLE_VALUE, TOKEN_ADJUST_PRIVILEGES
                  or TOKEN_QUERY, hToken);
 //Получаем LUID привилегии
 if not LookupPrivilegeValue(nil, 'SeDebugPrivilege', SeDebugNameValue) then
  begin
   CloseHandle(hToken);
   exit;
  end;
 tkp.PrivilegeCount := 1;
 tkp.Privileges[0].Luid := SeDebugNameValue;
 tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
 //Добавляем привилегию к процессу
 AdjustTokenPrivileges(hToken, false, tkp, SizeOf(TOKEN_PRIVILEGES),
                       tkp, ReturnLength);
 if GetLastError() <> ERROR_SUCCESS then exit;
 Result:=true;
end;

Function InjectDll(Process: dword; ModulePath: PChar): boolean;
var
  Memory:pointer;
  Code: dword;
  BytesWritten: dword;
  ThreadId: dword;
  hThread: dword;
  hKernel32: dword;
  Inject: packed record
           PushCommand:byte;
           PushArgument:DWORD;
           CallCommand:WORD;
           CallAddr:DWORD;
           PushExitThread:byte;
           ExitThreadArg:dword;
           CallExitThread:word;
           CallExitThreadAddr:DWord;
           AddrLoadLibrary:pointer;
           AddrExitThread:pointer;
           LibraryName:array[0..MAX_PATH] of char;
          end;
begin
  Result := false;
  Memory := VirtualAllocEx(Process, nil, sizeof(Inject),
                           MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if Memory = nil then Exit;

  Code := dword(Memory);
  //инициализация внедряемого кода:
  Inject.PushCommand    := $68;
  inject.PushArgument   := code + $1E;
  inject.CallCommand    := $15FF;
  inject.CallAddr       := code + $16;
  inject.PushExitThread := $68;
  inject.ExitThreadArg  := 0;
  inject.CallExitThread := $15FF;
  inject.CallExitThreadAddr := code + $1A;
  hKernel32 := GetModuleHandle('kernel32.dll');
  inject.AddrLoadLibrary := GetProcAddress(hKernel32, 'LoadLibraryA');
  inject.AddrExitThread  := GetProcAddress(hKernel32, 'ExitThread');
  lstrcpy(@inject.LibraryName, ModulePath);
  //записать машинный код по зарезервированному адресу
  WriteProcessMemory(Process, Memory, @inject, sizeof(inject), BytesWritten);
  //выполнить машинный код
  hThread := CreateRemoteThread(Process, nil, 0, Memory, nil, 0, ThreadId);
  if hThread = 0 then Exit;
  CloseHandle(hThread);
  Result := True;
end;

function RunUO(ExeName, InjectDllName: String):Boolean;
var
  ExeDir:String;
  Si:STARTUPINFO;
  Pi:PROCESS_INFORMATION;
begin
  Result := False;
  if not EnableDebugPrivilege then begin
    MessageBox(0, PChar('Невозможно получить дополнительные привелегии.'+#13#10+'Возможно у вас недостаточно прав.'), nil, MB_OK);
    Exit;
  end;
  ExeDir:=ExtractFileDir(ExeName);
  GetStartupInfo(Si);
  CreateProcess(PChar(ExeName), nil, nil, nil, False, CREATE_SUSPENDED, nil, PChar(ExeDir), Si, Pi);
  if not InjectDll(Pi.hProcess,  PCHar(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+InjectDllName)) then begin
    MessageBox(0, PChar('Не удалось сделать инъекцию в '+ExeName+#13#10+'Возможно нет прав или не хватает ' + InjectDllName + #13#10+'УО запустится в обычном режиме.'), nil, MB_OK);
  end;
  ResumeThread(Pi.hThread);
  Result := True;
end;

procedure TFMain.BRunClick(Sender: TObject);
begin
  RunUO(EUOPath.Text, EDLLName.Text);
  If CBCloseAfter.Checked then Close;
end;

procedure TFMain.BPathSelectClick(Sender: TObject);
begin
  OD.InitialDir:=ExtractFilePath(EUOPath.Text);
  If OD.Execute then begin
    EUOPath.Text:=OD.FileName;
    SaveOptions;
  end;
end;

procedure TFMain.SaveOptions;
var
  Ini:TIniFile;
begin
  Ini:=TIniFile.Create(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+ 'Options.ini');
  Ini.WriteString('Options','UOPath',EUOPath.Text);
  Ini.WriteBool('Options','CloseAfter',CBCloseAfter.Checked);
  Ini.UpdateFile;
  Ini.Free;
end;

procedure TFMain.LoadOptions;
var
  Ini:TIniFile;
begin
  Ini:=TIniFile.Create(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+ 'Options.ini');
  EUOPath.Text:=Ini.ReadString('Options','UOPath','');
  CBCloseAfter.Checked:=Ini.ReadBool('Options','CloseAfter',False);
  Ini.Free;
end;

procedure TFMain.NeedSaveOptions(Sender: TObject);
begin
  if not bLoading then SaveOptions;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  bLoading:=True;
  LoadOptions;
  bLoading:=False;
end;

end.
