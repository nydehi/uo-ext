unit Executable;

interface

uses Windows;

function RunUOReg: Cardinal; stdcall;
function RunUOA(AExecutablePath: PAnsiChar): Cardinal; stdcall;
function RunUOW(AExecutablePath: PWideChar): Cardinal; stdcall;

function PrepareToRunUO: Boolean; stdcall;
function StartSuspendedA(AExecutablePath: PAnsiChar; var AProcInfo: PROCESS_INFORMATION): Boolean; stdcall;
function StartSuspendedW(AExecutablePath: PWideChar; var AProcInfo: PROCESS_INFORMATION): Boolean; stdcall;
function InjectThisDllA(ASuspendedProcess:THandle): Boolean; stdcall;
function InjectThisDllW(ASuspendedProcess:THandle): Boolean; stdcall;
function InjectDllA(ASuspendedProcess:THandle; ADllPath, AInitProcedure: PAnsiChar): Boolean; stdcall;
function InjectDllW(ASuspendedProcess:THandle; ADllPath: PWideChar; AInitProcedure: PAnsiChar): Boolean; stdcall;
function ResumeLoading(AThreadHandle: THandle): Boolean; stdcall;

implementation

function RunUOA(AExecutablePath: PAnsiChar): Cardinal; stdcall;
var
  Pi: PROCESS_INFORMATION;
Begin
  Result := INVALID_HANDLE_VALUE;
  if not PrepareToRunUO then Exit;
  If not StartSuspendedA(AExecutablePath, Pi) then Exit;
  if not InjectThisDllA(Pi.hProcess) then Begin
    CloseHandle(Pi.hThread);
    TerminateProcess(Pi.hProcess, 0);
    CloseHandle(Pi.hProcess);
    Exit;
  End;
  if not ResumeLoading(Pi.hThread) then Begin
    CloseHandle(Pi.hThread);
    TerminateProcess(Pi.hProcess, 0);
    CloseHandle(Pi.hProcess);
    Exit;
  End;
  CloseHandle(Pi.hThread);
  CloseHandle(Pi.hProcess);
  Result := Pi.dwProcessId;
End;

function RunUOW(AExecutablePath: PWideChar): Cardinal; stdcall;
var
  Pi: PROCESS_INFORMATION;
Begin
  Result := INVALID_HANDLE_VALUE;
  if not PrepareToRunUO then Exit;
  If not StartSuspendedW(AExecutablePath, Pi) then Exit;
  if not InjectThisDllW(Pi.hProcess) then Begin
    CloseHandle(Pi.hThread);
    TerminateProcess(Pi.hProcess, 0);
    CloseHandle(Pi.hProcess);
    Exit;
  End;
  if not ResumeLoading(Pi.hThread) then Begin
    CloseHandle(Pi.hThread);
    TerminateProcess(Pi.hProcess, 0);
    CloseHandle(Pi.hProcess);
    Exit;
  End;
  CloseHandle(Pi.hThread);
  CloseHandle(Pi.hProcess);
  Result := Pi.dwProcessId;
End;


function PrepareToRunUO: Boolean; stdcall;
var
 hToken: dword;
 SeDebugNameValue: Int64;
 tkp: TOKEN_PRIVILEGES;
 ReturnLength: dword;
begin
 Result:=false;
 //Adding SeDebugPrivilege
 //Get our process's token
 OpenProcessToken(INVALID_HANDLE_VALUE, TOKEN_ADJUST_PRIVILEGES
                  or TOKEN_QUERY, hToken);
 //Getting privilege's LUID
 if not LookupPrivilegeValue(nil, 'SeDebugPrivilege', SeDebugNameValue) then
  begin
   CloseHandle(hToken);
   exit;
  end;
 tkp.PrivilegeCount := 1;
 tkp.Privileges[0].Luid := SeDebugNameValue;
 tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
 //Adding privilege to process
 AdjustTokenPrivileges(hToken, false, tkp, SizeOf(TOKEN_PRIVILEGES),
                       tkp, ReturnLength);
 if GetLastError() <> ERROR_SUCCESS then exit;
 Result:=true;
end;

function StartSuspendedA(AExecutablePath: PAnsiChar; var AProcInfo: PROCESS_INFORMATION): Boolean; stdcall;
Var
  Si:STARTUPINFOA;
  FileDir : Array [0..MAX_PATH] of AnsiChar;
  FD_Size: Cardinal;
  pDummy: PAnsiChar;
Begin
  Result := False;
  ZeroMemory(@Si, SizeOf(Si));;
  Si.cb := SizeOf(Si);
  FD_Size := MAX_PATH;
  If GetFullPathNameA(AExecutablePath, FD_Size, @FileDir, pDummy) = 0 Then Exit;
  pDummy^ := #0;
  Result := CreateProcessA(AExecutablePath, nil, nil, nil, False, CREATE_SUSPENDED, nil, @FileDir, Si, AProcInfo);
End;

function StartSuspendedW(AExecutablePath: PWideChar; var AProcInfo: PROCESS_INFORMATION): Boolean; stdcall;
Var
  Si:STARTUPINFOW;
  FileDir : Array [0..MAX_PATH] of WideChar;
  FD_Size: Cardinal;
  pDummy: PWideChar;
Begin
  Result := False;
  ZeroMemory(@Si, SizeOf(Si));;
  Si.cb := SizeOf(Si);
  FD_Size := MAX_PATH;
  If GetFullPathNameW(AExecutablePath, FD_Size, @FileDir, pDummy) = 0 Then Exit;
  pDummy^ := #0;
  Result := CreateProcessW(AExecutablePath, nil, nil, nil, False, CREATE_SUSPENDED, nil, @FileDir, Si, AProcInfo);
End;

Function InjectDllA(ASuspendedProcess:THandle; ADllPath, AInitProcedure: PAnsiChar): Boolean; stdcall;
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
  Memory := VirtualAllocEx(ASuspendedProcess, nil, InjectThreadSize,
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
//  PByte(ICode  + $00)^   := $C9;          // Ret
  // Code generation ends. Procedure pointers begin.
  PDWord(ICode + $21)^   := DWord(GetProcAddress(hKernel32, 'LoadLibraryA'));
  PDWord(ICode + $25)^   := DWord(GetProcAddress(hKernel32, 'GetProcAddress'));
  PDWord(ICode + $29)^   := DWord(GetProcAddress(hKernel32, 'ExitThread'));
  // Procedure pointers end. String constants begin.
  lstrcpyA(PAnsiChar(ICode + $2D), ADllPath);
  lstrcpyA(PAnsiChar(ICode + $0133), AInitProcedure);

  //Write generated code to reserved address
  WriteProcessMemory(ASuspendedProcess, Memory, pInject, InjectThreadSize, BytesWritten);
  FreeMemory(pInject);
  //Execute generated data
  hThread := CreateRemoteThread(ASuspendedProcess, nil, 0, Memory, nil, 0, ThreadId);

  if hThread = 0 then Exit;
  WaitForSingleObject(hThread, INFINITE);
  CloseHandle(hThread);
  Result := True;
end;

Function InjectDllW(ASuspendedProcess:THandle; ADllPath: PWideChar; AInitProcedure: PAnsiChar): Boolean; stdcall;
const
  InjectThreadSize = 829;
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
//   hLibrary := LoadLibraryW('<ProtocolExtender.dll>');
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
// 002D - 0237 : DllName: Array [0..MAX_PATH] of WideChar; Addr of GG GG GG GG
// 0238 - 033D : DllInitProc: Array [0..MAX_PATH] of Byte; Addr of II II II II
//
// Needed memory: 829 bytes.


  Result := False;
  Memory := VirtualAllocEx(ASuspendedProcess, nil, InjectThreadSize,
                           MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if Memory = nil then Exit;

  Code := dword(Memory);

  pInject := GetMemory(InjectThreadSize);
  ICode := DWord(pInject);
  hKernel32 := GetModuleHandle('kernel32.dll');

  PByte(pInject)^        := $68;          // PUSH OpCode
  PDWord(ICode + $01)^   := Code + $2D;   // PUSH Argument
  PWord(ICode  + $05)^   := $15FF;        // Call OpCode (LoadLibraryW)
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
//  PByte(ICode  + $00)^   := $C9;          // Ret
  // Code generation ends. Procedure pointers begin.
  PDWord(ICode + $21)^   := DWord(GetProcAddress(hKernel32, 'LoadLibraryW'));
  PDWord(ICode + $25)^   := DWord(GetProcAddress(hKernel32, 'GetProcAddress'));
  PDWord(ICode + $29)^   := DWord(GetProcAddress(hKernel32, 'ExitThread'));
  // Procedure pointers end. String constants begin.
  lstrcpyW(PWideChar(ICode + $2D), ADllPath);
  lstrcpyA(PAnsiChar(ICode + $0238), AInitProcedure);

  //Write generated code to reserved address
  WriteProcessMemory(ASuspendedProcess, Memory, pInject, InjectThreadSize, BytesWritten);
  FreeMemory(pInject);
  //Execute generated data
  hThread := CreateRemoteThread(ASuspendedProcess, nil, 0, Memory, nil, 0, ThreadId);

  if hThread = 0 then Exit;
  WaitForSingleObject(hThread, INFINITE);
  CloseHandle(hThread);
  Result := True;
end;


function ResumeLoading(AThreadHandle: THandle): Boolean; stdcall;
Begin
  Result := ResumeThread(AThreadHandle) <> $FFFFFFFF;
End;


function InjectThisDllA(ASuspendedProcess:THandle): Boolean; stdcall;
const
  ProcedureStart: AnsiString = 'CoreInitialize' + #0;
var
  Buffer: Array [0..MAX_PATH] of AnsiChar;
  BufferLength: Cardinal;
Begin
  Result := False;
  BufferLength := MAX_PATH + 1;
  If GetModuleFileNameA(HInstance, @Buffer, BufferLength) = ERROR_INSUFFICIENT_BUFFER Then Exit;
  Result := InjectDllA(ASuspendedProcess, @Buffer, @ProcedureStart[1]);
End;

function InjectThisDllW(ASuspendedProcess:THandle): Boolean; stdcall;
const
  ProcedureStart: AnsiString = 'CoreInitialize' + #0;
var
  Buffer: Array [0..MAX_PATH] of WideChar;
  BufferLength: Cardinal;
Begin
  Result := False;
  BufferLength := MAX_PATH + 1;
  If GetModuleFileNameW(HInstance, @Buffer, BufferLength) = ERROR_INSUFFICIENT_BUFFER Then Exit;
  Result := InjectDllW(ASuspendedProcess, @Buffer, @ProcedureStart[1]);
End;

function RunUOReg: Cardinal; stdcall;
Const
  BYTESINCREMENT = 4096;
Var
  hOpendedKey: HKEY;
  AFilePath: PAnsiChar;
  AFilePathSize: Cardinal;
  QueryResult: LongInt;
Begin
  Result := 0;

  If RegOpenKeyExA(HKEY_LOCAL_MACHINE, 'SOFTWARE\Origin Worlds Online\Ultima Online\1.0', 0, KEY_READ, hOpendedKey) <> ERROR_SUCCESS Then Exit;
  AFilePathSize := MAX_PATH + 1;
  AFilePath := GetMemory(AFilePathSize);
  QueryResult := RegQueryValueExA(hOpendedKey, 'ExePath', nil, nil, PByte(AFilePath), @AFilePathSize);
  If QueryResult = ERROR_MORE_DATA Then Begin
    FreeMemory(AFilePath);
    AFilePathSize := AFilePathSize + 1;
    AFilePath := GetMemory(AFilePathSize);
    QueryResult := RegQueryValueExA(hOpendedKey, 'ExePath', nil, nil, PByte(AFilePath), @AFilePathSize);
  End;
  RegCloseKey(hOpendedKey);
  If QueryResult = ERROR_SUCCESS Then Result := RunUOA(AFilePath);

  FreeMemory(AFilePath);
End;

end.
