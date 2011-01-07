program UOProxy;

uses
  Windows,
  WinSock,
  AbstractThread in 'UOProxy\AbstractThread.pas',
  ClientThread in 'UOProxy\ClientThread.pas',
  Common in 'UOProxy\Common.pas',
  HuffmanAlgo in 'UOProxy\HuffmanAlgo.pas',
  PacketStream in 'UOProxy\PacketStream.pas',
  ProtocolDescription in 'UOProxy\ProtocolDescription.pas',
  ServerThread in 'UOProxy\ServerThread.pas';

{$APPTYPE CONSOLE}

var
  SrvThread:TServerThread;

function HandlerRoutine(dwCtrlType: cardinal): bool; stdcall;
  begin
    case dwCtrlType of
      CTRL_CLOSE_EVENT: begin
          SrvThread.Stop;
          Result:=true;
      end;
      else Result:=false;
    end;//of case
  end;

var
  Info:RListeningInfo;
  WSAData:TWSAData;
begin
  WriteLn('UO Protocol converter V0.01');
  WriteLn('Copyright Warstone (C) 2008 - 2009.');
  {$IFDEF Debug}
  WriteLn('Debug build.');
  {$ENDIF}
  WriteLn;
  SetConsoleCtrlHandler(@HandlerRoutine, True);
  WSAStartup($101, WSAData);
  SrvThread:=TServerThread.Create;
  Info.IP:=INADDR_LOOPBACK;
  Info.Port:=7775;
  SrvThread.AddListenInfo(Info);
  SrvThread.Run;
  while SrvThread.Running do Sleep(1);
  WSACleanup();
end.
