unit GUIThread;

interface

uses Windows, Messages, AbstractThread;


type
  TDataLine = record
    Text: AnsiString;
    ProgressMin: Cardinal;
    ProgressMax: Cardinal;
    Progress: Cardinal;
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
    FCanAcceptCalls:Boolean;
    FDataLines: Array of TDataLine;
    FUpdateEventFree: THandle;
    FUpdateEvent: THandle;
    FUpdateEventDone: THandle;
    FFreeEvent: THandle;
    FWindow: HWND;
    FFont: HFONT;
    FUpdateRect: TRect;
    FBufferBitmap: HBITMAP;
    FBufferDC: HDC;

    FInsertData: TUpdatingLine;
    FLastInsertedHandle: Cardinal;
    procedure InvokeUpdateLog;
  protected
    function Execute: Integer; override;
  public
    property CanAcceptCalls: Boolean read FCanAcceptCalls;
    function SetLog(LogHandle, ParentHandle: Cardinal; Text: AnsiString): Cardinal;
    function StartProgress(LogHandle, ParentHandle: Cardinal; ProcessLabel: AnsiString; Min, Max, Current: Cardinal): Cardinal;
    function UpdateProgress(ProcessHandle, Min, Max, Current: Cardinal): Cardinal;

    procedure Stop; override;
  end;

implementation

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
  iMaxPos, iCurrPos: Integer;
  GWLA: Integer;
  PaintInfo: tagPAINTSTRUCT;
  oBrush, cBrush: HBRUSH;
Begin
  Result := 0;
  GWLA := GetWindowLongA(hWnd, GWL_USERDATA);
  Thread := TGUIThread(GWLA);
  bProcessed := False;
  case Msg of
    WM_QUIT: Begin
      Thread.FNeedExit := True;
    End;
    WM_SIZE: Begin
      If Thread.FBufferBitmap <> 0 then DeleteObject(Thread.FBufferBitmap);
      if Thread.FBufferDC <> 0 then DeleteObject(Thread.FBufferDC);
      DC := GetDC(hWnd);
      Thread.FBufferDC := CreateCompatibleDC(DC);
      Thread.FBufferBitmap := CreateCompatibleBitmap(DC, lParam AND $FFFF, lParam SHR 16);
      ReleaseDC(hWnd, DC);
      SelectObject(Thread.FBufferDC, Thread.FBufferBitmap);
      SelectObject(Thread.FBufferDC, GetClassLongA(hWnd, GCL_HBRBACKGROUND));
      SelectObject(Thread.FBufferDC, Thread.FFont);
    End;
    WM_ERASEBKGND: Begin
      Result := 1;
      bProcessed := True;
    End;
    WM_PAINT: Begin
      SelectObject(Thread.FBufferDC, GetClassLongA(hWnd, GCL_HBRBACKGROUND));
      GetClientRect(hWnd, rct);
      Rectangle(Thread.FBufferDC, 0, 0, rct.Right - rct.Left, rct.Bottom - rct.Top);
      SetBkMode(Thread.FBufferDC, TRANSPARENT);
      DC := Thread.FBufferDC;

      cY := 160;
      iMaxPos := High(Thread.FDataLines);
      if iMaxPos > 3 then iMaxPos := 3;
      if iMaxPos >= 0 then For i := 0 to iMaxPos do Begin
        iCurrPos := High(Thread.FDataLines);
        if iCurrPos <= 3 then iCurrPos := 0 else iCurrPos := iCurrPos - 3;

        CurrentData := @Thread.FDataLines[iCurrPos + i];
        Text := @CurrentData^.Text;
        rct.Left := 30 + CurrentData^.TotalOffset * 5;
        rct.Top := cY;
        rct.Bottom := 0;
        rct.Right := 0;
        DrawTextExA(DC, @Text^[1], Length(Text^), rct, DT_CALCRECT XOR DT_LEFT XOR DT_TOP XOR DT_SINGLELINE XOR DT_NOPREFIX, nil);
        DrawTextExA(DC, @Text^[1], Length(Text^), rct, DT_LEFT XOR DT_TOP XOR DT_SINGLELINE XOR DT_NOPREFIX, nil);
        cY := rct.Bottom + 1;
        if CurrentData^.ProgressMax > 0 then Begin
          if rct.Right + 5 < 290 then Begin
            rct.Left := rct.Right + 5;
            rct.Right := 290;
            rct.Top := rct.Top + 1;
            rct.Bottom := rct.Bottom - 1;
            Rectangle(DC, rct.Left, rct.Top, rct.Right, rct.Bottom);

            If CurrentData^.Progress > CurrentData^.ProgressMax then CurrentData^.Progress := CurrentData^.ProgressMax;
            If(rct.Right - rct.Left > 4) Then Begin
              rct.Right := rct.Right - 2;
              rct.Left := rct.Left + 2;
            End;
            rct.Right := ((rct.Right - rct.Left) * CurrentData^.Progress) DIV (CurrentData^.ProgressMax - CurrentData^.ProgressMin) + rct.Left;
            If(rct.Bottom - rct.Top > 4) Then Begin
              rct.Top := rct.Top + 2;
              rct.Bottom := rct.Bottom - 2;
            End;
            cBrush := CreateSolidBrush(0);
            oBrush := SelectObject(DC, cBrush);
            Rectangle(DC, rct.Left, rct.Top, rct.Right, rct.Bottom);
            SelectObject(DC, oBrush);
            DeleteObject(cBrush);
          End;
        End;
      End;

      GetClientRect(hWnd, rct);
      DC := BeginPaint(hWnd, PaintInfo);
      BitBlt(DC, 0, 0, rct.Right - rct.Left, rct.Bottom - rct.Top, Thread.FBufferDC, 0, 0, SRCCOPY);
      EndPaint(DC, PaintInfo);
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
  hBackgroundImage: HBITMAP;
  DC: HDC;
  WaitEvents: Array[0..1] of THandle;
  tmsg: tagMSG;
Begin
  Result := 1;
  PeekMessage(tmsg, 0, WM_USER, WM_USER, PM_NOREMOVE);

  hBackgroundImage := LoadBitmapA(HInstance, 'MAINIMAGE');


  WndClass.cbSize := SizeOf(WndClass);
  WndClass.style := 0;
  WndClass.lpfnWndProc := @GUIWindowProcA;
  WndClass.cbClsExtra := 0;
  WndClass.cbWndExtra := 4;
  WndClass.hInstance := 0;
  WndClass.hIcon := 0;
  WndClass.hCursor := 0;
  WndClass.hbrBackground := CreatePatternBrush(hBackgroundImage);
  WndClass.lpszMenuName := nil;
  WndClass.lpszClassName := 'UOExt.GUI';
  WndClass.hIconSm := LoadCursor(0, IDC_ARROW);
  DeleteObject(hBackgroundImage);

  UnregisterClassA('UOExt.GUI', 0);
  ClassAtom := RegisterClassExA(WndClass);
  if ClassAtom = 0 then Exit;

  SetLength(FDataLines, 0);


  FWindow := CreateWindowExA(WS_EX_TOPMOST XOR WS_EX_TOOLWINDOW, 'UOExt.GUI', nil, WS_POPUP XOR WS_VISIBLE XOR WS_SYSMENU XOR WS_VISIBLE, (GetSystemMetrics(SM_CXSCREEN) - 320) DIV 2, (GetSystemMetrics(SM_CYSCREEN) - 240) DIV 2, 320, 240, 0, 0, 0, nil);
  FUpdateRect.Left := 30;
  FUpdateRect.Right := 290;
  FUpdateRect.Top := 160;
  FUpdateRect.Bottom := 210;

  SetWindowLongA(FWindow, GWL_USERDATA, Integer(Self));
  If FWindow = 0 then Exit;

  FFont := CreateFont(-MulDiv(7, GetDeviceCaps(GetWindowDC(FWindow), LOGPIXELSY), 72), 0, 0, 0, FW_NORMAL, 0, 0, 0, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH, 'Arial');
  if FFont = 0 then Exit;
  SendMessageA(FWindow, WM_SETFONT, FFont, 0);

  DC := GetDC(FWindow);
  FBufferDC := CreateCompatibleDC(DC);
  FBufferBitmap := CreateCompatibleBitmap(DC, 320, 240);
  ReleaseDC(FWindow, DC);
  SelectObject(FBufferDC, FBufferBitmap);
  SelectObject(FBufferDC, WndClass.hbrBackground);
  SelectObject(FBufferDC, FFont);
  ShowWindow(FWindow, SW_SHOW);

  FUpdateEvent := CreateEventA(nil, False, False, 'UpdateEvent');
  If FUpdateEvent = 0 Then Exit;
  FUpdateEventDone := CreateEventA(nil, False, False, 'UpdateEventDone');
  If FUpdateEventDone = 0 Then Exit;
  FUpdateEventFree := CreateEventA(nil, False, True, 'UpdateEventFree');
  If FUpdateEventFree = 0 Then Exit;
  FFreeEvent := CreateEventA(nil, False, False, 'FreeEvent');
  If FUpdateEvent = 0 Then Exit;

  WaitEvents[0]:= FUpdateEvent;
  WaitEvents[1]:= FFreeEvent;
  FCanAcceptCalls := True;
  while not FNeedExit do Begin
    IF MsgWaitForMultipleObjects(1, FUpdateEvent, False, INFINITE, QS_ALLINPUT) = WAIT_OBJECT_0 Then Begin
      If FNeedExit then Break;
      InvokeUpdateLog;
    End Else Begin
      If FNeedExit then Break;
      GetMessageA(Msg, 0, 0, 0);
      TranslateMessage(Msg);
      DispatchMessageA(Msg);
    End;
  End;
  DestroyWindow(FWindow);
  DeleteObject(FFont);

  CloseHandle(FUpdateEvent);
  CloseHandle(FUpdateEventDone);
  CloseHandle(FUpdateEventFree);
End;


procedure TGUIThread.InvokeUpdateLog;
var
  iPos: Integer;
begin
  iPos := High(FDataLines) + 1;

  if FInsertData.CurrentLine = $FFFFFFFF then Begin
    SetLength(FDataLines, iPos + 1);
    ZeroMemory(@FDataLines[iPos], SizeOf(TDataLine));
  End Else
    iPos := FInsertData.CurrentLine;
  if (iPos > High(FDataLines))or(iPos < Low(FDataLines)) then Exit;

  if FInsertData.HasProgress then Begin
    if FInsertData.Text <> '' then Begin
      FDataLines[iPos].Text := FInsertData.Text;
    if FInsertData.ParentLine <> $FFFFFFFF then
      FDataLines[iPos].TotalOffset := FDataLines[FInsertData.ParentLine].TotalOffset + 1;
    End;
    FDataLines[iPos].ProgressMin := FInsertData.ProgressMin;
    FDataLines[iPos].ProgressMax := FInsertData.ProgressMax;
    FDataLines[iPos].Progress := FInsertData.Progress;
  End Else Begin
    FDataLines[iPos].Text := FInsertData.Text;
    if FInsertData.ParentLine <> $FFFFFFFF then
      FDataLines[iPos].TotalOffset := FDataLines[FInsertData.ParentLine].TotalOffset + 1;
  End;

  InvalidateRect(FWindow, @FUpdateRect, True);
  UpdateWindow(FWindow);

  FLastInsertedHandle := iPos;
  SetEvent(FUpdateEventDone);
end;

procedure TGUIThread.Stop;
begin
  FNeedExit := True;
  SetEvent(FFreeEvent);
  PostMessageA(FWindow, WM_QUIT, 0, 0);
end;

// TGUIThread - Thread safe
function TGUIThread.SetLog(LogHandle: Cardinal; ParentHandle: Cardinal; Text: AnsiString):Cardinal;
begin
  Result := $FFFFFFFF;
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
  Result := $FFFFFFFF;
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
  Result := $FFFFFFFF;
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
