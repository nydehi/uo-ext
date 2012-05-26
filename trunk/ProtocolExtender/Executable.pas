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

function Infect32(Wnd: HWnd; Instance: HInst; CmdLine: PAnsiChar; nCmdShow: Integer): Bool; stdcall;

function InfectA(AExecutablePath: PAnsiChar):Boolean; stdcall;
function InfectW(AExecutablePath: PWideChar):Boolean; stdcall;

Function InjectDllAInvoke(ASuspendedProcess:THandle; ADllPath, AInitProcedure: PAnsiChar): THandle;
Function InjectDllWInvoke(ASuspendedProcess:THandle; ADllPath: PWideChar; AInitProcedure: PAnsiChar): THandle;

implementation

{$REGION 'Injection'}
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
//   If not pProcedure() Then // stdcall, no params.
//     FreeLibrary(hLibrary); // If server does not support UOExt - remove it from client
//   ExitThread(0);
// Asm code:
//   PUSH <PointerToDllName>        ; 68 GG GG GG GG
//   CALL <PointerToLoadLibraryA>   ; 15 FF HH HH HH HH ; EAX = hLibrary
//   PUSH EAX                       ; 50
//   PUSH <PointerToCoreInitialize> ; 68 II II II II
//   PUSH EAX                       ; 50
//   CALL <PointerToGetProcAddress> ; 15 FF JJ JJ JJ JJ ; EAX = @pProcedure
//   CALL EAX                       ; D0 FF
//   TEST AL, AL                    ; 84 C0
//   JNZ label1                     ; EB 0A
//   POP EAX                        ; 58
//   CALL <PointerToFreeLibrary>    ; 15 FF LL LL LL LL
//   JMP label2                     ; EB 01
//   POP EAX (label1)               ; 58
//   PUSH 0 (label2)                ; 6A 00
//   CALL <PointerToExitThread>     ; 15 FF KK KK KK KK
//   TODO: Make this done!
// Addr  00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
// 0000: 68 GG GG GG GG 15 FF HH HH HH HH 50 68 II II II
// 0010: II 50 15 FF JJ JJ JJ JJ FF D0 84 C0 EB 0A 50 15
// 0020: FF LL LL LL LL EB 01 58 6A 00 15 FF KK KK KK KK
// Code ends here. Data begins here.
// 0030: hh hh hh hh jj jj jj jj kk kk kk kk ll ll ll ll
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

{$ENDREGION}


{$REGION 'Infection'}

function PrepareInjectCode_End(RealStartPoint: Cardinal; var Size: Cardinal):Pointer; forward;

function PrepareInjectCode(RealStartPoint: Cardinal; var Size: Cardinal):Pointer;
Begin
  Result := PrepareInjectCode_End(RealStartPoint, Size);
End;

procedure InjectProc(); asm
         MOV ESI, [ESP]
         CALL @__End
         INC EAX
         MOV EDX, EAX
         PUSH EAX // Pointer to data


         AND ESI, $FFFF0000
@_L1:    CMP BYTE PTR [EBP + 5], $00
         JZ @_FL
         CMP WORD PTR [ESI], "ZM"
         JZ @_CPE
@_L2:    SUB ESI, $10000
         DEC BYTE PTR [EBP + 5]
         JMP @_L1
@_CPE:   MOV EDI, [ESI + $3C]
         ADD EDI, ESI
         CMP DWORD PTR [EDI], "EP"
         JZ @_GK
         JMP @_L2
@_FL:    MOV ESI, $0BFF70000
@_GK:    XCHG EAX, ESI
         // EAX = kernel base
         MOV EBX, EAX
         POP EDX    // Pointer to data
         PUSH EDX
         CALL @GPA  // EAX = Address of LoadLibraryExA
         POP EDX
         PUSH EDX
         ADD EDX, 0Fh // Pointer to UOExt.dll
         PUSH 08h
         PUSH 00h
         PUSH EDX
         CALL EAX // EAX now holds UOExt.dll base.
         MOV ECX, EAX // ECX - Handle for UOExt.dll
         POP EDX  // EDX pointer to data
         PUSH EDX
         ADD EDX, 019h // EDX pointer to CoreInitialize
         CALL @GPA // EAX now pointer to CoreInitialize func
         CALL EAX
         TEST AL, AL
         JNZ @_ALL_OK
         POP EDX
         ADD EDX, 028h // EDX pointer to FreeLibrary
         MOV EAX, EBX
         CALL @GPA // EAX pointer to FreeLibrary
         PUSH ECX
         CALL EAX // FreeLibrary
         JMP @_NO_POP
@_ALL_OK:POP EDX
@_NO_POP:PUSH 012345678h // Change this for real EntryPoint
         RET


// GetProcAddress EAX = dll base, EDX = Pointer to Procedure name.
@GPA:    PUSH  EBX
         PUSH  ECX
         PUSH  ESI
         PUSH  EDI
         MOV   ESI, EDX
         MOV   EDI, EDX  // EDI = Search API pointer
@_1:     CMP   BYTE PTR [ESI], 0
         JZ    @_2
         INC   ESI
         JMP   @_1
@_2:     INC   ESI
         SUB   ESI, EDX
         MOV   ECX, ESI   // ECX = Length of API to search

         MOV   EBX, EAX   // EBX = kern base.
         MOV   ESI, EAX
         ADD   ESI, 03Ch  // EAX = VA for RVA PE
         MOV   EAX, [ESI]
         MOV   ESI, EBX
         ADD   ESI, EAX   // ESI = VA for PE
         ADD   ESI, 078h  // ESI = VA for RVA export
         MOV   EAX, [ESI]
         MOV   ESI, EBX
         ADD   ESI, EAX   // ESI = VA export
         PUSH  ESI
         ADD   ESI, 020h  // ESI = VA for RVA Names
         MOV   EAX, [ESI]
         MOV   ESI, EBX
         ADD   ESI, EAX   // ESI = VA for Names
         XOR   EDX, EDX

@_3:     PUSH  ESI
         MOV   EAX, [ESI]
         MOV   ESI, EBX
         ADD   ESI, EAX   // ESI = VAR for Name
         PUSH  ECX
         PUSH  EDI
         CLD
         REP   CMPSB
         POP   EDI
         POP   ECX
         POP   ESI
         JZ    @_4
         ADD   ESI, 4
         INC   EDX
         JMP   @_3

@_4:     SHL   EDX, 1     // EDI = offset for Ordinals
         POP   ESI        // ESI = VA for export
         PUSH  ESI
         ADD   ESI, 024h  // ESI = VA for RVA Ordinals
         MOV   EAX, [ESI]
         MOV   ESI, EBX
         ADD   ESI, EAX   // ESI = VA Ordinals
         ADD   ESI, EDX   // ESI = VA for ordinal
         XOR   EDX, EDX
         MOVZX EDX, WORD PTR [ESI]
         SHL   EDX, 2
         POP   ESI        // ESI = VA for export
         ADD   ESI, 01Ch  // ESI = VA for RVA Funcs Address
         MOV   EAX, [ESI]
         MOV   ESI, EBX
         ADD   ESI, EAX
         ADD   ESI, EDX   // ESI = VA for Func Address
         MOV   EAX, [ESI]
         ADD   EAX, EBX
         POP   EDI
         POP   ESI
         POP   ECX
         POP   EBX
         RET
// End of GetProcAddress EAX = dll base, EDX = Pointer to Procedure name.


@_CPos:  MOV EAX, [ESP]
         RET
@__End:  CALL @_CPos
         RET

//       LoadLibraryExA + #0
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
//       UOExt.dll + #0
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
//       CoreInitialize + #0
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
//       FreeLibrary + #0
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
end;

function PrepareInjectCode_End(RealStartPoint: Cardinal; var Size: Cardinal):Pointer;
const
  LLE : Array [0..14] of AnsiChar = 'LoadLibraryExA' + #0;
  UOE : Array [0..9]  of AnsiChar = 'UOExt.dll' + #0;
  CI  : Array [0..14] of AnsiChar = 'CoreInitialize' + #0;
  FL  : Array [0..11] of AnsiChar = 'FreeLibrary' + #0;
Begin
  Size := $135;
  Result := GetMemory(Size);
  CopyMemory(Result, @InjectProc, Size);
  PCardinal(Cardinal(Result) + $78)^ := RealStartPoint;
  CopyMemory(Pointer(Cardinal(Result) + $101), @LLE[0], 15);
  CopyMemory(Pointer(Cardinal(Result) + $110), @UOE[0], 10);
  CopyMemory(Pointer(Cardinal(Result) + $11A), @CI[0], 15);
  CopyMemory(Pointer(Cardinal(Result) + $129), @FL[0], 12);
End;

function Align(Value, Factor: Cardinal): Cardinal;
Begin
  Result := (Value + (Factor - 1)) AND ( NOT (Factor - 1));
End;

function GetMappingSizeA(AFile: PAnsiChar; var InjectCode: Pointer; var CodeSize: Cardinal): Cardinal;
var
  hFile, hMapping: THandle;
  pFile: Pointer;
  DosHeader: PImageDosHeader;
  PEHeader: PImageFileHeader;
  OptHeader: PImageOptionalHeader;
Begin
  Result := 0;
  hFile := CreateFileA(AFile, GENERIC_READ OR GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hFile = INVALID_HANDLE_VALUE then Begin
    WriteLn('Can''t infect ', AFile, ' CreateFileA failed. GLE: ', GetLastError);
    Halt(1);
  End;
  hMapping := CreateFileMappingA(hFile, nil, PAGE_READWRITE, 0, 0, nil);
  pFile := MapViewOfFile(hMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  if pFile = nil then Begin
    WriteLn('Can''t infect ', AFile, ' MapViewOfFile failed. GLE: ', GetLastError);
    Halt(1);
  End;

  DosHeader := pFile;
  PEHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4);
  if PEHeader^.SizeOfOptionalHeader = 0 then Begin
    Exit;
  End;
  OptHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader));

  InjectCode := PrepareInjectCode(OptHeader^.AddressOfEntryPoint + OptHeader^.ImageBase, CodeSize);

  Result := GetFileSize(hFile, nil) + Align(CodeSize + 7, OptHeader^.FileAlignment);
  UnmapViewOfFile(pFile);
  CloseHandle(hMapping);
  CloseHandle(hFile);
End;

function GetMappingSizeW(AFile: PWideChar; var InjectCode: Pointer; var CodeSize: Cardinal): Cardinal;
var
  hFile, hMapping: THandle;
  pFile: Pointer;
  DosHeader: PImageDosHeader;
  PEHeader: PImageFileHeader;
  OptHeader: PImageOptionalHeader;
Begin
  Result := 0;
  hFile := CreateFileW(AFile, GENERIC_READ OR GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hFile = INVALID_HANDLE_VALUE then Begin
    WriteLn('Can''t infect ', AFile, ' CreateFileA failed. GLE: ', GetLastError);
    Halt(1);
  End;
  hMapping := CreateFileMappingW(hFile, nil, PAGE_READWRITE, 0, 0, nil);
  pFile := MapViewOfFile(hMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  if pFile = nil then Begin
    WriteLn('Can''t infect ', AFile, ' MapViewOfFile failed. GLE: ', GetLastError);
    Halt(1);
  End;

  DosHeader := pFile;
  PEHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4);
  if PEHeader^.SizeOfOptionalHeader = 0 then Begin
    Exit;
  End;
  OptHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader));

  InjectCode := PrepareInjectCode(OptHeader^.AddressOfEntryPoint + OptHeader^.ImageBase, CodeSize);

  Result := GetFileSize(hFile, nil) + Align(CodeSize + 7, OptHeader^.FileAlignment);
  UnmapViewOfFile(pFile);
  CloseHandle(hMapping);
  CloseHandle(hFile);
End;

function Infect32(Wnd: HWnd; Instance: HInst; CmdLine: PAnsiChar; nCmdShow: Integer): Bool; stdcall;
Begin
  Result := InfectA(CmdLine);
End;

function InfectA(AExecutablePath: PAnsiChar):Boolean; stdcall;
type
  TPorcedure = procedure;
  TSections = Array [0..999] of TImageSectionHeader;
  PSections = ^TSections;
var
  DosHeader: PImageDosHeader;
  PEHeader: PImageFileHeader;
  OptHeader: PImageOptionalHeader;
  Sections: PSections;

  hFile, hMapping: THandle;
  pFile: Pointer;

  i, LastSectionId: Word;

  cMaxVirtualAddress, cCodeSize, cInfectMark, cFileSize: Cardinal;
  pCodeData: Pointer;
  sInfectMark: AnsiString;
Begin
  Result := False;

  sInfectMark := 'UOEX';
  PCardinal(@cInfectMark)^ := PCardinal(@sInfectMark[1])^;

  cFileSize := GetMappingSizeA(AExecutablePath, pCodeData, cCodeSize);
  if cFileSize = 0 then Begin
    MessageBox(0, 'This executable not supported', nil, MB_OK);
    Exit;
  End;

  hFile := CreateFileA(AExecutablePath, GENERIC_READ OR GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  hMapping := CreateFileMappingA(hFile, nil, PAGE_READWRITE, 0, cFileSize, nil);
  pFile := MapViewOfFile(hMapping, FILE_MAP_ALL_ACCESS, 0, 0, cFileSize);

  DosHeader := pFile;
  PEHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4);
  OptHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader));
  Sections := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader) + SizeOf(TImageOptionalHeader) + (OptHeader^.NumberOfRvaAndSizes - IMAGE_NUMBEROF_DIRECTORY_ENTRIES) * SizeOf(TImageDataDirectory));

  LastSectionId := 0;
  cMaxVirtualAddress := 0;
  For i := 0 to PEHeader^.NumberOfSections - 1 do Begin
    if cMaxVirtualAddress < Sections^[i].VirtualAddress then Begin
      LastSectionId := i;
      cMaxVirtualAddress := Sections^[i].VirtualAddress;
    End;
  End;

  {Write infect code}
  CopyMemory(Pointer(Cardinal(pFile) + Sections[LastSectionId].PointerToRawData + Sections[LastSectionId].SizeOfRawData), pCodeData, cCodeSize);

  {Fix Entry point}
  OptHeader.AddressOfEntryPoint := Sections[LastSectionId].VirtualAddress + Sections[LastSectionId].SizeOfRawData;

  {Fix Virtual and Physical sizes}
  Sections[LastSectionId].Misc.VirtualSize := Align(Sections[LastSectionId].Misc.VirtualSize + cCodeSize, OptHeader.SectionAlignment);
  Sections[LastSectionId].SizeOfRawData := Align(Sections[LastSectionId].SizeOfRawData + cCodeSize, OptHeader.FileAlignment);
  if Sections[LastSectionId].SizeOfRawData > Sections[LastSectionId].Misc.VirtualSize then
    Sections[LastSectionId].Misc.VirtualSize := Align(Sections[LastSectionId].SizeOfRawData, OptHeader.SectionAlignment);

  OptHeader.SizeOfImage := Sections[LastSectionId].VirtualAddress + Sections[LastSectionId].Misc.VirtualSize;

  {Allowing to execut this section and misc changes}
  Sections[LastSectionId].Characteristics := (Sections[LastSectionId].Characteristics OR $A0000020) AND ( NOT $02000000 );

  UnmapViewOfFile(pFile);
  CloseHandle(hMapping);
  CloseHandle(hFile);

  FreeMemory(pCodeData);

  Result := True;
End;

function InfectW(AExecutablePath: PWideChar):Boolean; stdcall;
type
  TPorcedure = procedure;
  TSections = Array [0..999] of TImageSectionHeader;
  PSections = ^TSections;
var
  DosHeader: PImageDosHeader;
  PEHeader: PImageFileHeader;
  OptHeader: PImageOptionalHeader;
  Sections: PSections;

  hFile, hMapping: THandle;
  pFile: Pointer;

  i, LastSectionId: Word;

  cMaxVirtualAddress, cCodeSize, cInfectMark, cFileSize: Cardinal;
  pCodeData: Pointer;
  sInfectMark: AnsiString;
Begin
  Result := False;

  sInfectMark := 'UOEX';
  PCardinal(@cInfectMark)^ := PCardinal(@sInfectMark[1])^;

  cFileSize := GetMappingSizeW(AExecutablePath, pCodeData, cCodeSize);
  if cFileSize = 0 then Begin
    MessageBox(0, 'This executable not supported', nil, MB_OK);
    Exit;
  End;

  hFile := CreateFileW(AExecutablePath, GENERIC_READ OR GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  hMapping := CreateFileMappingW(hFile, nil, PAGE_READWRITE, 0, cFileSize, nil);
  pFile := MapViewOfFile(hMapping, FILE_MAP_ALL_ACCESS, 0, 0, cFileSize);

  DosHeader := pFile;
  PEHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4);
  OptHeader := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader));
  Sections := Pointer(Cardinal(pFile) + Cardinal(DosHeader^._lfanew) + 4 + SizeOf(TImageFileHeader) + SizeOf(TImageOptionalHeader) + (OptHeader^.NumberOfRvaAndSizes - IMAGE_NUMBEROF_DIRECTORY_ENTRIES) * SizeOf(TImageDataDirectory));

  LastSectionId := 0;
  cMaxVirtualAddress := 0;
  For i := 0 to PEHeader^.NumberOfSections - 1 do Begin
    if cMaxVirtualAddress < Sections^[i].VirtualAddress then Begin
      LastSectionId := i;
      cMaxVirtualAddress := Sections^[i].VirtualAddress;
    End;
  End;

  {Write infect code}
  CopyMemory(Pointer(Cardinal(pFile) + Sections[LastSectionId].PointerToRawData + Sections[LastSectionId].SizeOfRawData), pCodeData, cCodeSize);

  {Fix Entry point}
  OptHeader.AddressOfEntryPoint := Sections[LastSectionId].VirtualAddress + Sections[LastSectionId].SizeOfRawData;

  {Fix Virtual and Physical sizes}
  Sections[LastSectionId].Misc.VirtualSize := Align(Sections[LastSectionId].Misc.VirtualSize + cCodeSize, OptHeader.SectionAlignment);
  Sections[LastSectionId].SizeOfRawData := Align(Sections[LastSectionId].SizeOfRawData + cCodeSize, OptHeader.FileAlignment);
  if Sections[LastSectionId].SizeOfRawData > Sections[LastSectionId].Misc.VirtualSize then
    Sections[LastSectionId].Misc.VirtualSize := Align(Sections[LastSectionId].SizeOfRawData, OptHeader.SectionAlignment);

  OptHeader.SizeOfImage := Sections[LastSectionId].VirtualAddress + Sections[LastSectionId].Misc.VirtualSize;

  {Allowing to execut this section and misc changes}
  Sections[LastSectionId].Characteristics := (Sections[LastSectionId].Characteristics OR $A0000020) AND ( NOT $02000000 );

  UnmapViewOfFile(pFile);
  CloseHandle(hMapping);
  CloseHandle(hFile);

  FreeMemory(pCodeData);

  Result := True;
End;
{$ENDREGION}

end.
