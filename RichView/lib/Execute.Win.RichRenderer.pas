unit Execute.Win.RichRenderer;

{ RichDocument (c)2018 by Execute SARL <contact@execute.fr> }

interface
{$DEFINE CROSSVCL_PATCH}
uses
  Winapi.Windows,
  System.UITypes,
  System.Classes,
  System.SysUtils, // StrPLCopy
  Execute.RichDocuments,
  Execute.RichRenderers;

type
  TRichWinControlItem = class(TRichControlItem)
  protected
    procedure Draw(ADC: HDC; ADrawItem: TDrawItem); virtual;
  end;

  TBrushHandle = class
    Color : TColor;
    Handle: HBrush;
    Next  : TBrushHandle;
  end;

  TFontHandle = record
    Font  : TRichFontItem;
    Handle: HFont;
  end;

  TWinRichRenderer = class(TRichRenderer)
  private
    FPPI     : Integer;

    FStyle   : TRichStyleItem;

    FHFont   : HFont;
    FHBrush  : HBrush;

    FBrush   : TBrushHandle;
    FBrushes : TBrushHandle;

    FFonts   : array of TFontHandle;
    FnFont   : Integer;

    FSelect  : Integer;

    function GetFont: hFont;
    function GetBrush(AColor: TColor): HBrush;

    function SetBrush(AColor: TColor): HBrush;
    procedure DoneBrush;

    procedure InitStyle;
    procedure DoneStyle;

    procedure InitDC;
    procedure DoneDC;
  protected
    FDC     : HDC;
    function SetStyle(AStyle: TRichStyleItem): Integer; override;
    function MeasureText(RichItem: TRichItem; var ItemPos: TItemPos; Len: Integer): Integer; override;
    procedure UpdateSelection(DrawItem: TDrawItem);
    procedure DrawText(DrawItem: TDrawItem);
    procedure DrawSpecial(DrawItem: TDrawItem);
    procedure DrawControl(DrawItem: TDrawItem); virtual;
    procedure SetSelection(X, Y: Integer; Start: Boolean); override;
  public
    constructor Create;
    procedure Paint(DC: hDC; x, y: Integer);
  end;

implementation

function DrawPos(DC: HDC): TPoint;
begin
  MoveToEx(DC, 0, 0, @Result);
  MoveToEx(DC, Result.X, Result.Y, nil);
end;

{ TWinRichRenderer }

constructor TWinRichRenderer.Create;
var
  Screen: HDC;
begin
  inherited;
  Screen := GetDC(0);
  FPPI := GetDeviceCaps(Screen, LOGPIXELSY);
  ReleaseDC(0, Screen);
end;

procedure TWinRichRenderer.InitDC;
begin
  if FDC = 0 then
  begin
    FDC := GetDC(0);
    FHFont := GetCurrentObject(FDC, OBJ_FONT);
    FHBrush := GetCurrentObject(FDC, OBJ_BRUSH);
    InitStyle();
  end;
end;

procedure TWinRichRenderer.DoneDC;
begin
  if FDC <> 0 then
  begin
    DoneStyle();
    SelectObject(FDC, FHFont);
    SelectObject(FDC, FHBrush);
    ReleaseDC(0, FDC);
    FDC := 0;
  end;
end;

function TWinRichRenderer.SetBrush(AColor: TColor): HBrush;
begin
  if (FBrush = nil) or (FBrush.Color <> AColor) then
  begin
    Result := SelectObject(FDC, GetBrush(AColor));
  end else begin
    Result := 0;
  end;
end;

procedure TWinRichRenderer.DoneBrush;
var
  Brush: TBrushHandle;
begin
  FBrush := nil;
  if FHBrush <> 0 then
    SelectObject(FDC, FHBrush);
  while FBrushes <> nil do
  begin
    Brush := FBrushes;
    FBrushes := Brush.Next;
    DeleteObject(Brush.Handle);
    Brush.Free;
  end;
end;

procedure TWinRichRenderer.InitStyle;
begin
  SetLength(FFonts, Document.Fonts.Count);
  FnFont := 0;
  FStyle := nil;
end;

procedure TWinRichRenderer.SetSelection(X, Y: Integer; Start: Boolean);
begin
  InitDC();
  inherited;
  DoneDC();
end;


function TWinRichRenderer.MeasureText(RichItem: TRichItem;
  var ItemPos: TItemPos; Len: Integer): Integer;
var
  Text: PChar;
begin
  Text := Pointer(RichItem.Text);
  Inc(Text, ItemPos.Start);
  GetTextExtentExPoint(FDC, Text, Len, ItemPos.MaxWidth - ItemPos.Left, @Result, nil, ItemPos.Size);
  if Result = 0 then
    Inc(Result);
  if Result < Len then
    GetTextExtentExPoint(FDC, Text, Result, ItemPos.MaxWidth - ItemPos.Left, nil, nil, ItemPos.Size);
end;

function TWinRichRenderer.SetStyle(AStyle: TRichStyleItem): Integer;
begin
  if FStyle <> AStyle then
  begin
    FStyle := AStyle;
    Result := SelectObject(FDC, GetFont);
  end else begin
    Result := 0;
  end;
end;

procedure TWinRichRenderer.DoneStyle;
var
  Index: Integer;
begin
  FStyle := nil;
  if FHFont <> 0 then
    SelectObject(FDC, FHFont);
  for Index := 0 to FnFont - 1 do
    DeleteObject(FFonts[Index].Handle);
  FFonts := nil;
end;

procedure TWinRichRenderer.DrawControl(DrawItem: TDrawItem);
begin
  if DrawItem.RichItem.Control is TRichWinControlItem then
  begin
    TRichWinControlItem(DrawItem.RichItem.Control).Draw(FDC, DrawItem);
  end else begin
//    SetBrush($FFFFFF);
    Rectangle(FDC, DrawItem.Left, DrawItem.Top, DrawItem.Right, DrawItem.Bottom);
  end;
  UpdateSelection(DrawItem);
end;

procedure TWinRichRenderer.DrawSpecial(DrawItem: TDrawItem);
begin
  case DrawItem.Start of
    -1: // Break
    begin
      MoveToEx(FDC, DrawItem.Left, DrawItem.Top + 5, nil);
      LineTo(FDC, DrawItem.Right, DrawItem.Top + 5);
    end;
  end;
  UpdateSelection(DrawItem);
end;

procedure TWinRichRenderer.UpdateSelection(DrawItem: TDrawItem);
var
  Start, Stop: Integer;
  P: TPoint;
begin
  Document.IsSelection(DrawItem.RichItem, Start, Stop);
  if Start >= 0 then
    FSelect := 0;
  if FSelect = 0 then
  begin
    P.Y := DrawItem.Rect.Top;
    if Start <= 0 then
      P.X := DrawItem.Rect.Left
    else
      P.X := DrawItem.Rect.Right;
    Document.SetSelectionPosition(DrawItem.RichItem, P, DrawItem.Rect.Height);
    if (Start <= 0) and ((Stop = -1) or (Stop > 0)) then
    begin
      PatBlt(FDC, DrawItem.Left, DrawItem.Top, DrawItem.Rect.Width, DrawItem.Rect.Height, DSTINVERT);
    end;
  end;
  if Stop >= 0 then
  begin
    FSelect := +2;
    SetBkColor(FDC, $FFFFFF);
  end;
end;

procedure TWinRichRenderer.DrawText(DrawItem: TDrawItem);
var
  Start, Stop: Integer;
  Text : PChar;
  a,b,c: Integer;
begin
  SetStyle(DrawItem.RichItem.Style);
  Text := @DrawItem.Text[DrawItem.Start];

  a := DrawItem.Count; // before selection
  b := 0;              // inside selection
  c := 0;              // after selection

  // we're inside the selection
  if FSelect = 0 then
  begin
    b := a; // inside selection
    a := 0; // nothing before
  end;

  if Document.IsSelection(DrawItem.RichItem, Start, Stop) or (FSelect = 0) then
  begin

    // selection starts before the item
    if (Start >= 0) and (Start <= DrawItem.Start + a) then
    begin
      b := a;
      a := Start - DrawItem.Start;
      Dec(b, a);
      if a < 0 then
        a := 0;
      FSelect := 0;  // we are inside the selection
    end;

    // selection ends before end of item
    if (Stop >= 0) and (DrawItem.Start + DrawItem.Count >= Stop) then
    begin
      c := DrawItem.Start + DrawItem.Count - Stop; // after selection
      Dec(b, c);
      FSelect := +1; // we are after the selection
    end;

  end;

  MoveToEx(FDC, DrawItem.Left, DrawItem.Top, nil);
  if a > 0 then
  begin
    TextOut(FDC, 0, 0, Text, a);
  end;
  Document.SetSelectionPosition(DrawItem.RichItem, DrawPos(FDC), DrawItem.Rect.Height);
  if b > 0 then
  begin
    SetBkColor(FDC, $FF9933);
    SetTextColor(FDC, $FFFFFF);
    TextOut(FDC, 0, 0, @Text[a], b);
  end;
  if FSelect = +1 then
  begin
    if b > 0 then
      Document.SetSelectionPosition(DrawItem.RichItem, DrawPos(FDC), DrawItem.Rect.Height);
    SetBkColor(FDC, $FFFFFF);
    SetTextColor(FDC, DrawItem.RichItem.Style.Color);
    FSelect := +2;
  end;
  if c > 0 then
  begin
    TextOut(FDC, 0, 0, @Text[a + b], c);
  end;

end;

function TWinRichRenderer.GetFont: hFont;
var
  Font : TRichFontItem;
  Index: Integer;
  Log  : TLogFont;
begin
  SetTextColor(FDC, FStyle.Color);

  Font := FStyle.Font;
  for Index := 0 to FnFont - 1 do
  begin
    if FFonts[Index].Font = Font then
      Exit(FFonts[Index].Handle);
  end;

  FillChar(Log, SizeOf(Log), 0);
  Log.lfHeight := -MulDiv(Font.Size, FPPI, 72);
  Log.lfWidth := 0;
  Log.lfEscapement := Font.Angle;
  Log.lfOrientation := Font.Angle;
  if TFontStyle.fsBold in Font.Style then
    Log.lfWeight := FW_BOLD
  else
    Log.lfWeight := FW_NORMAL;
  Log.lfItalic := Byte(TFontStyle.fsItalic in Font.Style);
  Log.lfUnderline := Byte(TFontStyle.fsUnderline in Font.Style);
  Log.lfStrikeOut := Byte(TFontStyle.fsStrikeOut in Font.Style);
  Log.lfCharSet := Byte(Font.Charset);
  StrPLCopy(Log.lfFaceName, {UTF8ToString}(Font.Name), Length(Log.lfFaceName) - 1);
  Log.lfQuality := Ord(Font.Quality);
  if Log.lfOrientation <> 0 then
    Log.lfOutPrecision := OUT_TT_ONLY_PRECIS
  else
    Log.lfOutPrecision := OUT_DEFAULT_PRECIS;
  Log.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  case Font.Pitch of
    TFontPitch.fpVariable: Log.lfPitchAndFamily := VARIABLE_PITCH;
    TFontPitch.fpFixed   : Log.lfPitchAndFamily := FIXED_PITCH;
  else
    Log.lfPitchAndFamily := DEFAULT_PITCH;
  end;
  Result := CreateFontIndirect(Log);

  FFonts[FnFont].Font := Font;
  FFonts[FnFont].Handle := Result;
  Inc(FnFont);
end;

function TWinRichRenderer.GetBrush(AColor: TColor): HBrush;
begin
  FBrush := FBrushes;
  while FBrush <> nil do
  begin
    if FBrush.Color = AColor then
      Exit(FBrush.Handle);
    FBrush := FBrush.Next;
  end;
  FBrush := TBrushHandle.Create;
  FBrush.Color := AColor;
  FBrush.Handle := CreateSolidBrush(AColor);
  FBrush.Next := FBrushes;
  FBrushes := FBrush;
  Result := FBrush.Handle;
end;

procedure TWinRichRenderer.Paint(DC: hDC; x, y: Integer);
var
  Item: TDrawItem;
  Rect: TRect;
  Align: Integer;
  Org  : TPoint;
begin
  FDC := DC;
  FHFont := GetCurrentObject(FDC, OBJ_FONT);
  FHBrush := GetCurrentObject(FDC, OBJ_BRUSH);
  try
    SetBrush($FFFFFF);
    Rect := TRect.Create(0, 0, FWidth, FHeight);
    FillRect(FDC, Rect, FBrush.Handle);

    if Document.Styles.Count = 0 then
      Exit;

    SetViewPortOrgEx(FDC, x, y, @Org);

    InitStyle;
    try
      if FNeedFormat then
        Format;

      Item := Items.First;
      if Item = nil then
        Exit;

      FSelect := -1;
      Align := GetTextAlign(FDC);
//      SetBrush($00FF00);
      try
        SetTextAlign(FDC, Align or TA_UPDATECP);
        repeat
//          Rectangle(DC, Item.Left, Item.Top, Item.Right, Item.Bottom); // DEBUG
          if Item.Start < 0 then
          begin
            DrawSpecial(Item);
          end else
          if Item.RichItem.Control = nil then
          begin
            DrawText(Item);
          end else begin
            DrawControl(Item);
          end;
//          Rectangle(DC, Item.Left, Item.Top, Item.Right, Item.Bottom); // DEBUG
        until Items.Next(Item) = False;
      finally
        SetTextAlign(FDC, Align);
      end;

      //Rectangle(DC, Document.Margins.Left, Document.Margins.Top, Document.Margins.Left + FSize.cx, Document.Margins.Top + FSize.Cy);

    finally
      DoneStyle();
    end;

    SetViewPortOrgEx(FDC, Org.x, Org.y, nil);

  finally
    DoneBrush();
    FDC := 0;
  end;
end;


{ TRichWinControlItem }

procedure TRichWinControlItem.Draw(ADC: HDC; ADrawItem: TDrawItem);
begin
  Rectangle(ADC, ADrawItem.Left, ADrawItem.Top, ADrawItem.Right, ADrawItem.Bottom);
end;

end.
