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
    FBufferDC: HDC;
    FRawDC: HDC;
    FRawImage: HBITMAP;
    FSelectedImage: HBITMAP;
    FSize: TSize;
    FWindowPosition: TPoint;
    FZeroPoint: TPoint;
    FUpdateRect: TRect;
    FShowedLines: Byte;

    FInsertData: TUpdatingLine;
    FLastInsertedHandle: Cardinal;
    procedure InvokeUpdateLog;
    procedure InvokeUpdateWindow;
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

procedure gblur(src:Pointer; Width: Cardinal; Height: Cardinal; Radius: Cardinal);
var
  r: Real;
  N: Cardinal;
  window: Array of Real;
  s2, Sum: Real;
  i, j, k: Cardinal;
  l: Integer;
  pTmp: Pointer;
  tmpRow: Pointer;
Begin
  N := Radius;
  s2 := 2*N*N / 9;
  SetLength(window, N*2);

  window[N] := 1;
  for i := 1 to N - 1 do Begin
    window[N + i] := Exp(-i*i/s2);
    window[N - i] := window[N + i];
  End;

  tmpRow := GetMemory(4*width);
  ZeroMemory(tmpRow, 4*width);
  for j := 0 to Height - 1 do Begin
    for i := 0 to Width - 1 do Begin
      Sum := 0;
      r := 0;
      for k := 0 to 2*N - 1 do Begin
        l := i + k - N;
        If(l>=0)and (Cardinal(l) < Width) then Begin
          pTmp := Pointer(Cardinal(src) + (j*Width + Cardinal(l))*4);
          r := r + PByte(Cardinal(pTmp) + 2)^ * window[k];
          sum := sum + window[k];
        End;
      End;
      pTmp := Pointer(Cardinal(tmpRow) + i*4);
      PByte(Cardinal(pTmp) + 2)^ := Round(r / sum);
    End;
    CopyMemory(Pointer(Cardinal(src) + (j*width)*4), tmpRow, width*4);
  End;
  FreeMemory(tmpRow);

  tmpRow := GetMemory(4*Height);
  ZeroMemory(tmpRow, 4*Height);
  for i := 0 to width - 1 do Begin
    for j := 0 to Height - 1 do Begin
      Sum := 0;
      r := 0;
      for k := 0 to 2*N do Begin
        l := j + k - N;
        If(l>=0)and (Cardinal(l) < Height) then Begin
          pTmp := Pointer(Cardinal(src) + (Cardinal(l)*Width + i)*4);
          r := r + PByte(Cardinal(pTmp) + 2)^ * window[k];
          sum := sum + window[k];
        End;
      End;
      pTmp := Pointer(Cardinal(tmpRow) + (j)*4);
      PByte(Cardinal(pTmp) + 2)^ := Round(r / sum);
    End;
    for j := 0 to Height - 1 do Begin
      PCardinal(Cardinal(src) + (j*width + i)*4)^ := PCardinal(Cardinal(tmpRow) + j*4)^;
    End;
  End;
  FreeMemory(tmpRow);
End;

procedure RedToAlpha(src:Pointer; Width: Cardinal; Height: Cardinal);
var
  i: Cardinal;
  pTmp: Pointer;
Begin
  pTmp := src;
  for i := 0 to Width * Height - 1 do Begin
    if PByte(Cardinal(pTmp) + 2)^ = 0 then Begin
      PByte(Cardinal(pTmp) + 3)^ := $00;
    End Else Begin
      PByte(Cardinal(pTmp) + 3)^ := PByte(Cardinal(pTmp) + 2)^;
      PByte(Cardinal(pTmp) + 2)^ := $00;
    End;
    pTmp := Pointer(Cardinal(pTmp) + 4);
  End;
End;

procedure RenderImage(var DC:HDC; var Img:HBITMAP);
var
  screenDC: HDC;
  bFont, mFont, oFont: HFONT;
  BMIH: tagBITMAPINFO;
  pImg: Pointer;
Begin
  if DC <> INVALID_HANDLE_VALUE then Begin
    DeleteObject(Img);
    DeleteDC(DC);
  End;

  screenDC := GetDC(0);
  DC := CreateCompatibleDC(screenDC);
  ReleaseDC(0, screenDC);
  BMIH.bmiHeader.biSize := SizeOf(BMIH.bmiHeader);
  BMIH.bmiHeader.biWidth := 320;
  BMIH.bmiHeader.biHeight := 240;
  BMIH.bmiHeader.biPlanes := 1;
  BMIH.bmiHeader.biBitCount := 32;
  BMIH.bmiHeader.biCompression := BI_RGB;

  Img := CreateDIBSection(DC, BMIH, 0, pImg, 0, 0);
  SelectObject(DC, Img);

  bFont := CreateFontA(-MulDiv(64, GetDeviceCaps(DC, LOGPIXELSY), 72), 0, 0, 0, FW_BOLD, 0, 0, 0, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH, 'Arial');
  mFont := CreateFontA(-MulDiv(20, GetDeviceCaps(DC, LOGPIXELSY), 72), 0, 0, 0, FW_BOLD, 0, 0, 0, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH, 'Arial');

  oFont := SelectObject(DC, bFont);
  SetTextColor(DC, $FF);
  SetBkMode(DC, TRANSPARENT);

  TextOutA(DC, 31, 5, 'UOExt', 5);
  SelectObject(DC, mFont);

  TextOutA(DC, 45, 95, 'We do what EA can''t', 19);
  SelectObject(DC, bFont);

  gblur(pImg, 320, 240, 15);

  TextOutA(DC, 26, 0, 'UOExt', 5);
  SelectObject(DC, mFont);

  TextOutA(DC, 40, 90, 'We do what EA can''t', 19);
  SelectObject(DC, oFont);

  DeleteObject(bFont);
  DeleteObject(mFont);

  gblur(pImg, 320, 240, 3);
  RedToAlpha(pImg, 320, 240);

End;


// TGUIThread

function TGUIThread.Execute:Integer;
var
  WndClass: TWndClassExA;
  ClassAtom: Word;

  Msg: TMsg;
  WaitEvents: Array[0..1] of THandle;
  tmsg: tagMSG;
Begin
  Result := 1;
  PeekMessage(tmsg, 0, WM_USER, WM_USER, PM_NOREMOVE);

//  hBackgroundImage := LoadBitmapA(HInstance, 'MAINIMAGE');

  RenderImage(FRawDC, FRawImage);
  FSize.cx := 320;
  FSize.cy := 240;
  FWindowPosition.X := (GetSystemMetrics(SM_CXSCREEN) - FSize.cx) DIV 2;
  FWindowPosition.Y := (GetSystemMetrics(SM_CYSCREEN) - FSize.cy) DIV 2;
  FZeroPoint.X := 0;
  FZeroPoint.Y := 0;

  FUpdateRect.Left := 25;
  FUpdateRect.Right := 295;
  FUpdateRect.Top := 160;
  FUpdateRect.Bottom := 220;
  FShowedLines := 3;

  WndClass.cbSize := SizeOf(WndClass);
  WndClass.style := 0;
  WndClass.lpfnWndProc := @DefWindowProcA;
  WndClass.cbClsExtra := 0;
  WndClass.cbWndExtra := 0;
  WndClass.hInstance := 0;
  WndClass.hIcon := 0;
  WndClass.hCursor := 0;
  WndClass.hbrBackground := 0;
  WndClass.lpszMenuName := nil;
  WndClass.lpszClassName := 'UOExt.GUI';
  WndClass.hIconSm := LoadCursor(0, IDC_ARROW);

  UnregisterClassA('UOExt.GUI', 0);
  ClassAtom := RegisterClassExA(WndClass);
  if ClassAtom = 0 then Exit;

  SetLength(FDataLines, 0);

  FWindow := CreateWindowExA(WS_EX_TOPMOST XOR WS_EX_TOOLWINDOW XOR WS_EX_LAYERED, 'UOExt.GUI', nil, WS_POPUP XOR WS_VISIBLE XOR WS_SYSMENU XOR WS_VISIBLE, FWindowPosition.X, FWindowPosition.Y, FSize.cx, FSize.cy, 0, 0, 0, nil);
  If FWindow = 0 then Exit;

  FFont := CreateFont(-MulDiv(7, GetDeviceCaps(GetWindowDC(FWindow), LOGPIXELSY), 72), 0, 0, 0, FW_NORMAL, 0, 0, 0, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH, 'Arial');
  if FFont = 0 then Exit;
  SendMessageA(FWindow, WM_SETFONT, FFont, 0);
  InvokeUpdateWindow;
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

procedure TGUIThread.InvokeUpdateWindow;
var
  ScreenDC, TmpDC: HDC;
  BF: TBlendFunction;
  iMaxPos, iCurrPos, i, cY, iX, iY: Integer;
  CurrentData: PDataLine;
  Text: PAnsiString;
  rct: TRect;
  oBrush, cBrush: HBRUSH;
  BMIH: tagBITMAPINFO;
  pImg: Pointer;
  BMP: HBITMAP;
begin
  ScreenDC := GetDC(0);
  if FBufferDC <> 0 then DeleteDC(FBufferDC);
  FBufferDC := CreateCompatibleDC(ScreenDC);

  If FSelectedImage <> 0 Then DeleteObject(FSelectedImage);
  FSelectedImage := CopyImage(FRawImage, IMAGE_BITMAP, 0, 0, 0);
  SelectObject(FBufferDC, FSelectedImage);


  TmpDC := CreateCompatibleDC(0);
  SelectObject(TmpDC, FFont);
  BMIH.bmiHeader.biSize := SizeOf(BMIH.bmiHeader);
  BMIH.bmiHeader.biWidth := FUpdateRect.Right - FUpdateRect.Left;
  BMIH.bmiHeader.biHeight := FUpdateRect.Bottom - FUpdateRect.Top;
  BMIH.bmiHeader.biPlanes := 1;
  BMIH.bmiHeader.biBitCount := 32;
  BMIH.bmiHeader.biCompression := BI_RGB;
  BMP := CreateDIBSection(TmpDC, BMIH, 0, pImg, 0, 0);
  SelectObject(TmpDC, BMP);
  SetBkColor(TmpDC, $FFFFFF);
  SetBkMode(TmpDC, TRANSPARENT);
  SetTextColor(TmpDC, 0);

  cBrush := CreateSolidBrush($FFFFFF);
  oBrush := SelectObject(TmpDC, cBrush);
  FloodFill(TmpDC, 0, 0, $FF);
  SelectObject(TmpDC, oBrush);
  DeleteObject(cBrush);


  iMaxPos := High(FDataLines);
  iCurrPos := iMaxPos;
  cY := 0;
  if iCurrPos <= 3 then iCurrPos := 0 else iCurrPos := iCurrPos - 3;
  if iMaxPos > 3 then iMaxPos := 3;
  if iMaxPos >= 0 then For i := 0 to iMaxPos do Begin
    CurrentData := @FDataLines[iCurrPos + i];
    Text := @CurrentData^.Text;
    rct.Left := CurrentData^.TotalOffset * 5;
    rct.Top := cY;
    rct.Bottom := 0;
    rct.Right := 0;
    DrawTextExA(TmpDC, @Text^[1], Length(Text^), rct, DT_CALCRECT XOR DT_LEFT XOR DT_TOP XOR DT_SINGLELINE XOR DT_NOPREFIX, nil);
    DrawTextExA(TmpDC, @Text^[1], Length(Text^), rct, DT_LEFT XOR DT_TOP XOR DT_SINGLELINE XOR DT_NOPREFIX, nil);
    cY := rct.Bottom + 1;
    if CurrentData^.ProgressMax > 0 then Begin
      if rct.Right + 5 < (FUpdateRect.Right - FUpdateRect.Left) then Begin
        rct.Left := rct.Right + 5;
        rct.Right := (FUpdateRect.Right - FUpdateRect.Left);
        rct.Top := rct.Top + 1;
        rct.Bottom := rct.Bottom - 1;
        Rectangle(TmpDC, rct.Left, rct.Top, rct.Right, rct.Bottom);

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
        oBrush := SelectObject(TmpDC, cBrush);
        Rectangle(TmpDC, rct.Left, rct.Top, rct.Right, rct.Bottom);
        SelectObject(TmpDC, oBrush);
        DeleteObject(cBrush);
      End;
    End;
  End;
  if iMaxPos > 0 Then for iX := 0 to BMIH.bmiHeader.biWidth - 1 do for iY := 0 to BMIH.bmiHeader.biHeight - 1 do Begin
    PByte(Cardinal(pImg) + 3)^ := 255 - PByte(Cardinal(pImg) + 2)^;
    ZeroMemory(pImg, 3);
    pImg := Pointer(Cardinal(pImg) + 4);
  End;

  BF.BlendOp := AC_SRC_OVER;
  BF.BlendFlags := 0;
  BF.SourceConstantAlpha := $FF;
  BF.AlphaFormat := AC_SRC_ALPHA;
  AlphaBlend(FBufferDC, FUpdateRect.Left, FUpdateRect.Top, FUpdateRect.Right - FUpdateRect.Left, FUpdateRect.Bottom - FUpdateRect.Top, TmpDC, 0, 0, FUpdateRect.Right - FUpdateRect.Left, FUpdateRect.Bottom - FUpdateRect.Top, BF);
  DeleteDC(TmpDC);
  DeleteObject(BMP);

  BF.SourceConstantAlpha := $FF;
  BF.AlphaFormat := AC_SRC_ALPHA;

  UpdateLayeredWindow(FWindow, ScreenDC, @FWindowPosition, @FSize, FBufferDC, @FZeroPoint, 0, @BF, ULW_ALPHA);
  ReleaseDC(0, ScreenDC);
end;


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

  InvokeUpdateWindow;

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
