unit Main;

interface

uses Windows, Messages, AbstractThread;


implementation

type
  TDataLine = record
    Text: AnsiString;
    ProgressMin: Cardinal;
    ProgressMax: Cardinal;
    Progress: Cardinal;
    ProgressWnd: HWND;
    TotalOffset: Cardinal;
  end;
  PDataLine = ^TDataLine;

  TUpdatingLine = record
    CurrentLine: Cardinal;
    ParentLine: Cardinal;
    Text: AnsiString;
    ProgressMin: Cardinal;
    ProgressMax: Cardinal;
    Progress: Cardinal;
    HasProgress: Boolean;
  end;
  TGUIThread = class(TAbstractThread)
  private
    FDataLines: Array of TDataLine;
    FUpdateEventFree: THandle;
    FUpdateEvent: THandle;
    FUpdateEventDone: THandle;
    FWindow: HWND;
    FFont: HFONT;

    FInsertData: TUpdatingLine;
    FLastInsertedHandle: Cardinal;
    procedure InvokeUpdateLog;
  protected
    function Execute: Integer; override;
  public
    function SetLog(LogHandle, ParentHandle: Cardinal; Text: AnsiString): Cardinal;
    function StartProgress(LogHandle, ParentHandle: Cardinal; ProcessLabel: AnsiString; Min, Max, Current: Cardinal): Cardinal;
    function UpdateProgress(ProcessHandle, Min, Max, Current: Cardinal): Cardinal;

    procedure Stop; override;
  end;

// Local procedures

function GUIWindowProcA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Thread: TGUIThread;
  bProcessed: Boolean;
  DC: HDC;
  i: Byte;
  Text: PAnsiString;
  rct: TRect;
  cY: Integer;
  CurrentData: PDataLine;
Begin
  Result := 0;
  Thread := TGUIThread(GetWindowLong(hWnd, GWL_USERDATA));
  bProcessed := False;
  case Msg of
    WM_QUIT: Begin
      Thread.FNeedExit := True;
    End;
    WM_PAINT: Begin
      DC := GetWindowDC(hWnd);
      BeginPath(DC);
      SelectObject(DC, Thread.FFont);
      cY := 160;
      For i := 0 to 3 do Begin
        CurrentData := @Thread.FDataLines[High(Thread.FDataLines) - 3 + i];
        Text := @CurrentData^.Text;
        rct.Left := 30 + CurrentData^.TotalOffset * 5;
        rct.Top := cY;
        rct.Bottom := 0;
        rct.Right := 0;
        DrawTextExA(DC, @Text^[1], Length(Text^), rct, DT_CALCRECT XOR DT_LEFT XOR DT_TOP XOR DT_SINGLELINE XOR DT_NOPREFIX, nil);
        DrawTextExA(DC, @Text^[1], Length(Text^), rct, DT_LEFT XOR DT_TOP XOR DT_SINGLELINE XOR DT_NOPREFIX, nil);
        if CurrentData^.ProgressMax > 0 then Begin
          if rct.Right + 5 < 290 then Begin
            rct.Left := rct.Right + 5;
            rct.Right := 290;
            rct.Top := rct.Top + (rct.Bottom - rct.Top) DIV 4;
            rct.Bottom := rct.Bottom - (rct.Bottom - rct.Top) DIV 3;
            Rectangle(DC, rct.Left, rct.Top, rct.Right, rct.Bottom);

            if CurrentData^.Progress > CurrentData^.ProgressMax then CurrentData^.Progress := CurrentData^.ProgressMax;
            rct.Right := (rct.Right - rct.Left DIV (CurrentData^.ProgressMax - CurrentData^.ProgressMin)) * CurrentData^.Progress;
            Rectangle(DC, rct.Left, rct.Top, rct.Right, rct.Bottom);
          End;
        End;
        cY := rct.Bottom + 1;
      End;

      EndPath(DC);
      ReleaseDC(hWnd, DC);
      bProcessed := True;
    End;
  end;
  if not bProcessed then Result := DefWindowProcA(hWnd, Msg, wParam, lParam);
End;

// TGUIThread

function TGUIThread.Execute:Integer;
var
  WndClass: TWndClassExA;
  ClassAtom: Word;

  Msg: TMsg;
Begin
  Result := 1;

  WndClass.cbSize := SizeOf(WndClass);
  WndClass.style := CS_CLASSDC XOR CS_NOCLOSE XOR CS_OWNDC;
  WndClass.lpfnWndProc := @GUIWindowProcA;
  WndClass.cbClsExtra := 0;
  WndClass.cbWndExtra := 4;
  WndClass.hInstance := 0;
  WndClass.hIcon := 0;
  WndClass.hCursor := 0;
  WndClass.hbrBackground := COLOR_WINDOW;
  WndClass.lpszMenuName := nil;
  WndClass.lpszClassName := 'UOExt.GUI';
  WndClass.hIconSm := 0;

  ClassAtom := RegisterClassExA(WndClass);
  if ClassAtom = 0 then Exit;





  FWindow := CreateWindowExA(WS_EX_TOPMOST, 'UOExt.GUI', nil, 0, 0, 0, 320, 240, 0, 0, 0, nil);
  SetWindowLongA(FWindow, GWL_USERDATA, Integer(@Self));
  If FWindow = 0 then Exit;

  FFont := CreateFont(-MulDiv(8, GetDeviceCaps(GetWindowDC(FWindow), LOGPIXELSY), 72), 0, 0, 0, FW_NORMAL, 0, 0, 0, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH, 'Arial');
  if FFont = 0 then Exit;
  SendMessageA(FWindow, WM_SETFONT, FFont, 0);

  ShowWindow(FWindow, SW_SHOW);

  FUpdateEvent := CreateEventA(nil, False, False, 'UpdateEvent');
  If FUpdateEvent = 0 Then Exit;
  FUpdateEventDone := CreateEventA(nil, True, False, 'UpdateEventDone');
  If FUpdateEventDone = 0 Then Exit;
  FUpdateEventFree := CreateEventA(nil, True, True, 'UpdateEventFree');
  If FUpdateEventFree = 0 Then Exit;

  while GetMessageA(Msg, 0, 0, 0) do Begin
    TranslateMessage(Msg);
    DispatchMessageA(Msg);
    If WaitForSingleObject(FUpdateEvent, 0) = WAIT_OBJECT_0 Then Begin
      InvokeUpdateLog;
    End;
  End;

  CloseHandle(FUpdateEvent);
  CloseHandle(FUpdateEventFree);
  DestroyWindow(FWindow);
End;


procedure TGUIThread.InvokeUpdateLog;
begin
  SetLength(FDataLines, High(FDataLines) + 1);
  ZeroMemory(@FDataLines[High(FDataLines)], SizeOf(TDataLine));
  FDataLines[High(FDataLines)].Text := FInsertData.Text;
  FDataLines[High(FDataLines)].ProgressMin := FInsertData.ProgressMin;
  FDataLines[High(FDataLines)].ProgressMax := FInsertData.ProgressMax;
  FDataLines[High(FDataLines)].Progress := FInsertData.Progress;
  FDataLines[High(FDataLines)].ProgressWnd := INVALID_HANDLE_VALUE;
  if FInsertData.ParentLine <> MAXLONG then
    FDataLines[High(FDataLines)].TotalOffset := FDataLines[FInsertData.ParentLine].TotalOffset + 1;
  UpdateWindow(FWindow);
  ResetEvent(FUpdateEventDone);
end;

procedure TGUIThread.Stop;
begin
  PostMessage(FWindow, WM_QUIT, 0, 0);
end;

// TGUIThread - Thread safe
function TGUIThread.SetLog(LogHandle: Cardinal; ParentHandle: Cardinal; Text: AnsiString):Cardinal;
begin
  Result := MAXLONG;
  If WaitForSingleObject(FUpdateEventFree, INFINITE) <> WAIT_OBJECT_0 Then Exit;
  ZeroMemory(@FInsertData, SizeOf(FInsertData));
  FInsertData.CurrentLine := LogHandle;
  FInsertData.ParentLine := ParentHandle;
  FInsertData.Text:= Text;
  SetEvent(FUpdateEvent);
  If WaitForSingleObject(FUpdateEventDone, INFINITE) <> WAIT_OBJECT_0 then Exit;
  Result := FLastInsertedHandle;
  SetEvent(FUpdateEventFree);
end;

function TGUIThread.StartProgress(LogHandle: Cardinal; ParentHandle: Cardinal; ProcessLabel: AnsiString; Min: Cardinal; Max: Cardinal; Current: Cardinal): Cardinal;
begin
  Result := MAXLONG;
  If WaitForSingleObject(FUpdateEventFree, INFINITE) <> WAIT_OBJECT_0 Then Exit;
  ZeroMemory(@FInsertData, SizeOf(FInsertData));
  FInsertData.CurrentLine := LogHandle;
  FInsertData.ParentLine := ParentHandle;
  FInsertData.Text:= ProcessLabel;
  FInsertData.ProgressMin := Min;
  FInsertData.ProgressMax := Max;
  FInsertData.Progress := Current;
  FInsertData.HasProgress := True;
  SetEvent(FUpdateEvent);
  If WaitForSingleObject(FUpdateEventDone, INFINITE) <> WAIT_OBJECT_0 then Exit;
  Result := FLastInsertedHandle;
  SetEvent(FUpdateEventFree);
end;

function TGUIThread.UpdateProgress(ProcessHandle: Cardinal; Min: Cardinal; Max: Cardinal; Current: Cardinal): Cardinal;
begin
  Result := MAXLONG;
  If WaitForSingleObject(FUpdateEventFree, INFINITE) <> WAIT_OBJECT_0 Then Exit;
  ZeroMemory(@FInsertData, SizeOf(FInsertData));
  FInsertData.CurrentLine := ProcessHandle;
  FInsertData.ProgressMin := Min;
  FInsertData.ProgressMax := Max;
  FInsertData.Progress := Current;
  FInsertData.HasProgress := True;
  SetEvent(FUpdateEvent);
  If WaitForSingleObject(FUpdateEventDone, INFINITE) <> WAIT_OBJECT_0 then Exit;
  Result := FLastInsertedHandle;
  SetEvent(FUpdateEventFree);
end;

end.
