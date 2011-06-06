unit Executable;

interface

uses Windows, Common;

function RunUOReg: Boolean; stdcall;
function RunUOA(AExecutablePath: PAnsiChar): Boolean; stdcall;
function RunUOW(AExecutablePath: PWideChar): Boolean; stdcall;
function RunUO32(Wnd: HWnd; Instance: HInst; CmdLine: PAnsiChar; nCmdShow: Integer): Bool; stdcall;

function StartSuspendedA(AExecutablePath: PAnsiChar): Boolean; stdcall;
function StartSuspendedW(AExecutablePath: PWideChar): Boolean; stdcall;
function InjectDllA(ADllPath, AInitProcedure: PAnsiChar): Boolean; stdcall;
function InjectDllW(ADllPath: PWideChar; AInitProcedure: PAnsiChar): Boolean; stdcall;
function InjectThisDllA: Boolean; stdcall;
function InjectThisDllW: Boolean; stdcall;
function ResumeLoading: Boolean; stdcall;

implementation

type
  PRunProcessParams=^RRunProcessParams;
  RRunProcessParams=record
    RunA: Boolean;
    Cmd: Pointer;
    Dir: Pointer;
  end;
  PInjectParams=^RInjectParams;
  RInjectParams=record
    InjectA: Boolean;
    ADllPath: Pointer;
    AInitProcedure: PAnsiChar;
  end;


Var
  DebugThreadCommand: Integer;
  DebugThreadArgumant: Integer;
  DebugThreadResult: Integer;
  hDebugThread: THandle;

function DebugSetProcessKillOnExit(KillOnExit:BOOL):BOOL; stdcall; external kernel32 name 'DebugSetProcessKillOnExit';

Function InjectDllAInvoke(ASuspendedProcess:THandle; ADllPath, AInitProcedure: PAnsiChar): THandle;
const
  InjectThreadSize = 568;
var
  Memory:pointer;
  Code, ICode: dword;
  BytesWritten: dword;
  ThreadId: dword;
  hKernel32: dword;
  pInject: Pointer;
begin
// Here is codegenerate for CreateRemoteThread procedure
// Main idea is:
//   hLibrary := LoadLibraryA('<ADllPath>');
//   @pProcedure := GetProcAddress(hLibrary, '<AInitProcedure>');
//   pProcedure(); // stdcall, no params.
//   ExitThread(0);
// Asm code:
//   PUSH <PointerToDllName>        ; 68 GG GG GG GG
//   CALL <PointerToLoadLibraryA>   ; 15 FF HH HH HH HH ; EAX = hLibrary
//   PUSH <PointerToCoreInitialize> ; 68 II II II II
//   PUSH EAX                       ; 50
//   CALL <PointerToGetProcAddress> ; 15 FF JJ JJ JJ JJ ; EAX = @pProcedure
//   CALL EAX                       ; D0 FF
//   PUSH 0                         ; 6A 00
//   CALL <PointerToExitThread>     ; 15 FF KK KK KK KK
//
// Addr  00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
// 0000: 68 GG GG GG GG 15 FF HH HH HH HH 68 II II II II
// 0010: 50 15 FF JJ JJ JJ JJ FF D0 33 C0 C3 XX XX XX XX
// 0020: XX
// Code ends here. Data begins here.
// 0020:    hh hh hh hh jj jj jj jj kk kk kk kk
// 002D - 0132 : DllName: Array [0..MAX_PATH] of Byte; Addr of GG GG GG GG
// 0133 - 0237 : DllInitProc: Array [0..MAX_PATH] of Byte; Addr of II II II II
//
// Needed memory: 568 bytes.


  Result := 0;
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
  Result := CreateRemoteThread(ASuspendedProcess, nil, 0, Memory, nil, 0, ThreadId);
end;

Function InjectDllWInvoke(ASuspendedProcess:THandle; ADllPath: PWideChar; AInitProcedure: PAnsiChar): THandle;
const
  InjectThreadSize = 829;
var
  Memory:pointer;
  Code, ICode: dword;
  BytesWritten: dword;
  ThreadId: dword;
  hKernel32: dword;
  pInject: Pointer;
begin
// Here is codegenerate for CreateRemoteThread procedure
// Main idea is:
//   hLibrary := LoadLibraryW('<ADllPath>');
//   @pProcedure := GetProcAddress(hLibrary, '<AInitProcedure>');
//   pProcedure(); // stdcall, no params.
//   ExitThread(0);
// Asm code:
//   PUSH <PointerToDllName>        ; 68 GG GG GG GG
//   CALL <PointerToLoadLibraryW>   ; 15 FF HH HH HH HH ; EAX = hLibrary
//   PUSH <PointerToCoreInitialize> ; 68 II II II II
//   PUSH EAX                       ; 50
//   CALL <PointerToGetProcAddress> ; 15 FF JJ JJ JJ JJ ; EAX = @pProcedure
//   CALL EAX                       ; D0 FF
//   PUSH 0                         ; 6A 00
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


  Result := 0;
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
  PDWord(ICode + $0C)^   := Code + $0238; // PUSH Argument
  PByte(ICode  + $10)^   := $50;          // PUSH EAX OpCode
  PWord(ICode  + $11)^   := $15FF;        // Call OpCode (GetProcAddr)
  PDWord(ICode + $13)^   := Code + $25;   // Call Argument
  PWord(ICode  + $17)^   := $D0FF;        // CALL EAX OpCode
  PWord(ICode  + $19)^   := $006A;        // PUSH 0 OpCode
  PWord(ICode  + $1B)^   := $15FF;        // Call OpCode (ExitThread)
  PDWord(ICode + $1D)^   := Code + $29;   // Call Argument

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
  Result := CreateRemoteThread(ASuspendedProcess, nil, 0, Memory, nil, 0, ThreadId);
end;


procedure SendDebugCommand(Command: Integer; Argument: Integer);
Begin
  while InterlockedCompareExchange(DebugThreadCommand, 0, 0) <> 0 do sleep(0);
  InterlockedExchange(DebugThreadArgumant, Argument);
  InterlockedExchange(DebugThreadCommand, Command);
End;

function GetDebugThreadResult:Integer;
Begin
  while InterlockedCompareExchange(DebugThreadResult, 0, 0) = 0 do sleep(0);
  Result := InterlockedExchange(DebugThreadResult, 0);
End;

function InvokeDebugCommand(Command: Integer; Argument: Integer): Integer;
Begin
  SendDebugCommand(Command, Argument);
  Result := GetDebugThreadResult;
End;

function DebugThread(lParam: PRunProcessParams): DWORD; stdcall;

  function GetCommand(Wait: Boolean; var Argument: Integer): Integer;
  Begin
    if Wait then Begin
      Repeat
        Result := InterlockedExchange(DebugThreadCommand, 0);
      Until Result <> 0;
    End Else Begin
      Result := InterlockedExchange(DebugThreadCommand, 0);
      if Result = 0 then Exit;
    End;
    Argument := InterlockedExchange(DebugThreadArgumant, 0);
  End;
  procedure SetResult(Result: Integer);
  Begin
    while InterlockedCompareExchange(DebugThreadResult, 0, 0) <> 0 do sleep(0);
    InterlockedExchange(DebugThreadResult, Result);
  End;

var
  dbg: _DEBUG_EVENT;
  SiA: STARTUPINFOA;
  SiW: STARTUPINFOW;
  Pi:PROCESS_INFORMATION;
  NeedExit, ProcessCreated: Boolean;
  cmdValue, cmdArgument: Integer;
  hThread, ThreadId: Cardinal;
Begin
  Result := 0;

  // RunUOProcess
  if lParam^.RunA then Begin
    ZeroMemory(@SiA, SizeOf(SiA));;
    SiA.cb := SizeOf(SiA);
    ProcessCreated := CreateProcessA(lParam^.Cmd, nil, nil, nil, False, CREATE_SUSPENDED XOR DEBUG_PROCESS, nil, lParam^.Dir, SiA, Pi);
  End Else Begin
    ZeroMemory(@SiW, SizeOf(SiW));;
    SiW.cb := SizeOf(SiW);
    ProcessCreated := CreateProcessW(lParam^.Cmd, nil, nil, nil, False, CREATE_SUSPENDED XOR DEBUG_PROCESS, nil, lParam^.Dir, SiW, Pi);
  End;
  if ProcessCreated then Begin
    SetResult(1);
  End Else begin
    SetResult(-1);
    Exit;
  End;
  DebugSetProcessKillOnExit(False);

  NeedExit := False;
  hThread := 0;
  cmdArgument := 0;
  while not NeedExit do Begin
    If WaitForDebugEvent(dbg, 1) Then Begin
      ContinueDebugEvent(dbg.dwProcessId, dbg.dwThreadId, DBG_EXCEPTION_NOT_HANDLED);
    End;
    cmdValue := GetCommand(False, cmdArgument);
    case cmdValue of
      1: NeedExit := True; // Break Thread
      2: Begin // Inject Dll to Process
          If PInjectParams(cmdArgument)^.InjectA Then Begin
            hThread := InjectDllAInvoke(Pi.hProcess, PInjectParams(cmdArgument)^.ADllPath, PInjectParams(cmdArgument)^.AInitProcedure);
          End Else Begin
            hThread := InjectDllWInvoke(Pi.hProcess, PInjectParams(cmdArgument)^.ADllPath, PInjectParams(cmdArgument)^.AInitProcedure);
          End;
          if hThread = 0 then SetResult(-1);
        End;
    end;
    if hThread <> 0 then Begin
      GetExitCodeThread(hThread, ThreadId);
      if ThreadId <> STILL_ACTIVE then Begin
        CloseHandle(hThread);
        hThread := 0;
        SetResult(1);
      End;
    End;
  End;
  ResumeThread(Pi.hThread);
{  CloseHandle(Pi.hThread);
  CloseHandle(Pi.hProcess);}
End;



function RunUO32(Wnd: HWnd; Instance: HInst; CmdLine: PAnsiChar; nCmdShow: Integer): Bool; stdcall;
Begin
  If (CmdLine<>'') and FileExists(CmdLine) Then
    Result := RunUOA(CmdLine)
  Else Begin
    Result := RunUOReg;
    if Result = False then
      MessageBox(Wnd, 'Can''t find UO client executable. You must supply correct filepath and/or fix UO registry entries.', nil, MB_OK);
  End;
End;

function RunUOA(AExecutablePath: PAnsiChar): Boolean; stdcall;
Begin
  Result := False;
  If not StartSuspendedA(AExecutablePath) then Exit;
  Result := InjectThisDllA;
  ResumeLoading;
End;

function RunUOW(AExecutablePath: PWideChar): Boolean; stdcall;
Begin
  Result := False;
  If not StartSuspendedW(AExecutablePath) then Exit;
  Result := InjectThisDllW;
  ResumeLoading;
End;

function StartSuspendedAInvoke(AExecutablePath: PAnsiChar): Boolean;
Var
  FileDir : Array [0..MAX_PATH] of AnsiChar;
  FD_Size: Cardinal;
  pDummy: PAnsiChar;
  thId: Cardinal;
  DbgRunInfo:RRunProcessParams;
Begin
  Result := False;
  FD_Size := MAX_PATH;
  If GetFullPathNameA(AExecutablePath, FD_Size, @FileDir, pDummy) = 0 Then Exit;
  pDummy^ := #0;
  DebugThreadCommand := 0;
  DebugThreadResult := 0;
  DbgRunInfo.RunA := True;
  DbgRunInfo.Cmd := AExecutablePath;
  DbgRunInfo.Dir := @FileDir;
  hDebugThread := CreateThread(nil, 0, @DebugThread, @DbgRunInfo, 0, thId);
  If hDebugThread = INVALID_HANDLE_VALUE Then Result := False;
  If GetDebugThreadResult = -1 Then Exit;
  Result := True;
End;

function StartSuspendedWInvoke(AExecutablePath: PWideChar): Boolean;
Var
  FileDir : Array [0..MAX_PATH] of WideChar;
  FD_Size: Cardinal;
  pDummy: PWideChar;
  thId: Cardinal;
  DbgRunInfo:RRunProcessParams;
Begin
  Result := False;
  FD_Size := MAX_PATH;
  If GetFullPathNameW(AExecutablePath, FD_Size, @FileDir, pDummy) = 0 Then Exit;
  pDummy^ := #0;
  DebugThreadCommand := 0;
  DebugThreadResult := 0;
  DbgRunInfo.RunA := False;
  DbgRunInfo.Cmd := AExecutablePath;
  DbgRunInfo.Dir := @FileDir;
  hDebugThread := CreateThread(nil, 0, @DebugThread, @DbgRunInfo, 0, thId);
  If hDebugThread = INVALID_HANDLE_VALUE Then Result := False;
  If GetDebugThreadResult = -1 Then Exit;
  Result := True;
End;

function StartSuspendedA(AExecutablePath: PAnsiChar): Boolean; stdcall;
Begin
  Result := StartSuspendedAInvoke(AExecutablePath);
End;

function StartSuspendedW(AExecutablePath: PWideChar): Boolean; stdcall;
Begin
  Result := StartSuspendedWInvoke(AExecutablePath);
End;

function InjectThisDllA: Boolean; stdcall;
const
  ProcedureStart: AnsiString = 'CoreInitialize' + #0;
var
  Buffer: Array [0..MAX_PATH] of AnsiChar;
  BufferLength: Cardinal;
Begin
  Result := False;
  BufferLength := MAX_PATH + 1;
  If GetModuleFileNameA(HInstance, @Buffer, BufferLength) = ERROR_INSUFFICIENT_BUFFER Then Exit;
  Result := InjectDllA(@Buffer, @ProcedureStart[1]);
End;

function InjectThisDllW: Boolean; stdcall;
const
  ProcedureStart: AnsiString = 'CoreInitialize' + #0;
var
  Buffer: Array [0..MAX_PATH] of WideChar;
  BufferLength: Cardinal;
Begin
  Result := False;
  BufferLength := MAX_PATH + 1;
  If GetModuleFileNameW(HInstance, @Buffer, BufferLength) = ERROR_INSUFFICIENT_BUFFER Then Exit;
  Result := InjectDllW(@Buffer, @ProcedureStart[1]);
End;

function InjectDllA(ADllPath, AInitProcedure: PAnsiChar): Boolean; stdcall;
Var
  cmdParams: RInjectParams;
Begin
  cmdParams.InjectA := True;
  cmdParams.ADllPath := ADllPath;
  cmdParams.AInitProcedure := AInitProcedure;
  SendDebugCommand(2, Integer(@cmdParams));
  Result := GetDebugThreadResult = 1;
End;

function InjectDllW(ADllPath: PWideChar; AInitProcedure: PAnsiChar): Boolean; stdcall;
Var
  cmdParams: RInjectParams;
Begin
  cmdParams.InjectA := False;
  cmdParams.ADllPath := ADllPath;
  cmdParams.AInitProcedure := AInitProcedure;
  SendDebugCommand(2, Integer(@cmdParams));
  Result := GetDebugThreadResult = 1;
End;

function ResumeLoading: Boolean; stdcall;
Begin
  SendDebugCommand(1, 0);
  WaitForSingleObject(hDebugThread, INFINITE);
  Result := True;
End;


function RunUOReg: Boolean; stdcall;
Const
  BYTESINCREMENT = 4096;
Var
  hOpendedKey: HKEY;
  AFilePath: PAnsiChar;
  AFilePathSize: Cardinal;
  QueryResult: LongInt;
Begin
  Result := False;

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
