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
    CBUseUOExt: TCheckBox;
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

Function EnableDebugPrivilege():Boolean;
Function InjectDll(Process: dword; ModulePath, InitProcedureName: PAnsiChar): boolean;
Function RunUO(ExeName, InjectDllName:String):Boolean;
Function RunUODll(ExeName, InjectDllName: String): Boolean;

var
  FMain: TFMain;

implementation

{$R *.dfm}

{ Local procedures }

function EnableDebugPrivilege():Boolean;
var
 hToken: THANDLE;
 SeDebugNameValue: Int64;
 tkp: TOKEN_PRIVILEGES;
 ReturnLength: dword;
begin
 Result:=false;
 //Добавляем привилегию SeDebugPrivilege
 //Получаем токен нашего процесса
 hToken:=INVALID_HANDLE_VALUE;
 If not OpenProcessToken(INVALID_HANDLE_VALUE, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) Then Exit;
 //Получаем LUID привилегии
 SeDebugNameValue:=0;
 if not LookupPrivilegeValue(nil, 'SeDebugPrivilege', SeDebugNameValue) then
  begin
   CloseHandle(hToken);
   exit;
  end;
 tkp.PrivilegeCount := 1;
 tkp.Privileges[0].Luid := SeDebugNameValue;
 tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
 //Добавляем привилегию к процессу
 ReturnLength:=0;
 AdjustTokenPrivileges(hToken, false, tkp, SizeOf(TOKEN_PRIVILEGES),
                       tkp, ReturnLength);
 if GetLastError() <> ERROR_SUCCESS then exit;
 Result:=true;
end;

Function InjectDll(Process: dword; ModulePath, InitProcedureName: PAnsiChar): Boolean;
const
  InjectThreadSize = 568;
var
  Memory:pointer;
  Code, ICode: dword;
  BytesWritten: dword;
  ThreadId: dword;
  hThread: dword;
  hKernel32: dword;
  pInject: Pointer;
begin
// Here is codegenerate for CreateRemoteThread procedure
// Main idea is:
//   hLibrary := LoadLibraryA('<ProtocolExtender.dll>');
//   @pProcedure := GetProcAddress(hLibrary, 'CoreInitialize');
//   pProcedure(); // stdcall, no params.
//   ExitThread(0);
// Asm code:
//   PUSH <PointerToDllName>        ; 68 GG GG GG GG
//   CALL <PointerToLoadLibraryA>   ; 15 FF HH HH HH HH ; EAX = hLibrary
//   PUSH <PointerToCoreInitialize> ; 68 II II II II
//   PUSH EAX                       ; 50
//   CALL <PointerToGetProcAddress> ; 15 FF JJ JJ JJ JJ ; EAX = @pProcedure
//   CALL EAX                       ; D0 FF
//   PUSH 0                         ; 00 6A
//   CALL <PointerToExitThread>     ; 15 FF KK KK KK KK
//
// Addr  00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
// 0000: 68 GG GG GG GG 15 FF HH HH HH HH 68 II II II II
// 0010: 50 15 FF JJ JJ JJ JJ FF D0 6A 00 15 FF KK KK KK
// 0020: KK
// Code ends here. Data begins here.
// 0020:    hh hh hh hh jj jj jj jj kk kk kk kk
// 002D - 0132 : DllName: Array [0..MAX_PATH] of Byte; Addr of GG GG GG GG
// 0133 - 0237 : DllInitProc: Array [0..MAX_PATH] of Byte; Addr of II II II II
//
// Needed memory: 568 bytes.


  Result := False;
  Memory := VirtualAllocEx(Process, nil, InjectThreadSize,
                           MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if Memory = nil then Exit;

  Code := dword(Memory);

  pInject := GetMemory(InjectThreadSize);
  ICode := DWord(pInject);
  hKernel32 := GetModuleHandle('kernel32.dll');

  PByte(pInject)^        := $68;          // PUSH OpCode
  PDWord(ICode + $01)^   := Code + $2D;   // PUSH Argument
  PWord(ICode  + $05)^   := $15FF;        // Call OpCode (LoadLibraryA)
  PDWord(ICode + $07)^   := Code + $21;   // Call Argument
  PByte(ICode  + $0B)^   := $68;          // PUSH OpCode
  PDWord(ICode + $0C)^   := Code + $0133; // PUSH Argument
  PByte(ICode  + $10)^   := $50;          // PUSH EAX OpCode
  PWord(ICode  + $11)^   := $15FF;        // Call OpCode (GetProcAddr)
  PDWord(ICode + $13)^   := Code + $25;   // Call Argument
  PWord(ICode  + $17)^   := $D0FF;        // CALL EAX OpCode
  PWord(ICode  + $19)^   := $006A;        // PUSH 0 OpCode
  PWord(ICode  + $1B)^   := $15FF;        // Call OpCode (ExitThread)
  PDWord(ICode + $1D)^   := Code + $29;   // Call Argument
  // Code generation ends. Procedure pointers begin.
  PDWord(ICode + $21)^   := DWord(GetProcAddress(hKernel32, 'LoadLibraryA'));
  PDWord(ICode + $25)^   := DWord(GetProcAddress(hKernel32, 'GetProcAddress'));
  PDWord(ICode + $29)^   := DWord(GetProcAddress(hKernel32, 'ExitThread'));
  // Procedure pointers end. String constants begin.
  lstrcpyA(PAnsiChar(ICode + $2D), ModulePath);
  lstrcpyA(PAnsiChar(ICode + $0133), InitProcedureName);

  //???????? ???????? ??? ?? ?????????????????? ??????
  WriteProcessMemory(Process, Memory, pInject, InjectThreadSize, BytesWritten);
  FreeMemory(pInject);
  //????????? ???????? ???
  hThread := CreateRemoteThread(Process, nil, 0, Memory, nil, 0, ThreadId);

  if hThread = 0 then Exit;
  WaitForSingleObject(hThread, INFINITE);
  CloseHandle(hThread);
  Result := True;
end;

function RunUO(ExeName, InjectDllName: String):Boolean;
var
  ExeDir:String;
  Si:STARTUPINFO;
  Pi:PROCESS_INFORMATION;
  InjectDllPath: AnsiString;
begin
  Result := False;
  if not EnableDebugPrivilege then begin
    MessageBox(0, PChar('Невозможно получить дополнительные привелегии.'+#13#10+'Возможно у вас недостаточно прав.'), nil, MB_OK);
    Exit;
  end;
  ExeDir:=ExtractFileDir(ExeName);
  Si.cb:=SizeOf(Si);
  GetStartupInfo(Si);
  Pi.dwProcessId:=0;
  CreateProcess(PChar(ExeName), nil, nil, nil, False, CREATE_SUSPENDED, nil, PChar(ExeDir), Si, Pi);
  InjectDllPath := AnsiString(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+InjectDllName) + #0;
  if not InjectDll(Pi.hProcess,  @InjectDllPath[1], 'CoreInitialize') then begin
    MessageBox(0, PChar('Не удалось сделать инъекцию в '+ExeName+#13#10+'Возможно нет прав или не хватает ' + InjectDllName + #13#10+'УО запустится в обычном режиме.'), nil, MB_OK);
  end;
  ResumeThread(Pi.hThread);
  Result := True;
end;

function RunUODll(ExeName, InjectDllName: String): Boolean;
Type
  TRunUOA = function (AExecutablePath: PAnsiChar): Boolean; stdcall;
  TRunUOW = function (AExecutablePath: PWideChar): Boolean; stdcall;
Var
  hDll: THandle;
  Func: TRunUOW;
Begin
  Result := False;
  hDll := LoadLibrary(PChar(InjectDllName));
  if hDll = INVALID_HANDLE_VALUE then Exit;
  @Func := GetProcAddress(hDll, 'RunUOW');
  if @Func = nil then Exit;
  Result := Func(PWideChar(ExeName));
End;

{ TFMain }

procedure TFMain.BRunClick(Sender: TObject);
begin
  if not CBUseUOExt.Checked then
    RunUO(EUOPath.Text, EDLLName.Text)
  Else
    RunUODll(EUOPath.Text, EDLLName.Text);
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
  Ini.WriteBool('Options','UseDllRun',CBUseUOExt.Checked);
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
  CBUseUOExt.Checked:=Ini.ReadBool('Options','UseDllRun',False);
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


