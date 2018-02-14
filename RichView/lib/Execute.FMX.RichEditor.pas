unit Execute.FMX.RichEditor;

{ RichDocument (c)2018 by Execute SARL <contact@execute.fr> }

// do not work for now :(

interface

uses
  System.Types,
  System.UITypes,
  System.Classes,
  System.SysUtils,
  System.Math.Vectors,
  FMX.Controls,
  FMX.Objects,
  FMX.ImgList,
  FMX.Graphics,
  FMX.TextLayout,
  Execute.RichDocuments,
  Execute.RichRenderers;

type
  TFMXRichDocument = class;
  TRichEditor = class;

  TRichFMXControlItem = class(TRichControlItem)
  protected
    procedure Draw(Canvas: TCanvas; ADrawItem: TDrawItem); virtual; abstract;
  end;

  TRichBullet = class(TRichFMXControlItem)
  private
    FDocument  : TFMXRichDocument;
    FImageIndex: TImageIndex;
  protected
    procedure Draw(Canvas: TCanvas; ADrawItem: TDrawItem); override;
  public
    constructor Create(ADocument: TFMXRichDocument; AImageIndex: TImageIndex);
    function GetSize(var ASize: TSize): Boolean; override;
  end;

  TRichImage = class(TRichFMXControlItem)
  private
    FImage: TImage;
  protected
    procedure Draw(Canvas: TCanvas; ADrawItem: TDrawItem); override;
  public
    constructor Create(AImage: TImage);
    function GetSize(var ASize: TSize): Boolean; override;
  end;

  TFMXRichDocument = class(TRichDocument)
  private
    FEditor   : TRichEditor;
    FImageList: TImageList;
    procedure SetImageList(Value: TImageList);
  public
    procedure AddBullet(AImageIndex: TImageIndex);
    procedure AddImage(AImage: TImage);
    property Images: TImageList read FImageList write SetImageList;
  end;

  TFMXRichRenderer = class(TRichRenderer)
  private
    FEditor   : TRichEditor;
    FSelect : Integer;
    FCaret: TPointF;
    FCanvas: TCanvas;
    FTextLayout: TTextLayout;
    function MeasureText(RichItem: TRichItem; var ItemPos: TItemPos; Len: Integer): Integer; override;
    procedure TextOut(Text: PChar; Len: Integer; Selected: Boolean);
    procedure UpdateSelection(DrawItem: TDrawItem);
    procedure DrawText(DrawItem: TDrawItem);
    procedure DrawSpecial(DrawItem: TDrawItem);
    procedure DrawControl(DrawItem: TDrawItem);
    function SetStyle(AStyle: TRichStyleItem): Integer; override;
    procedure SetSelection(X, Y: Integer; Start: Boolean); override;
  public
    procedure Paint(Canvas: TCanvas; x, y: Integer);
  end;

  TRichEditor = class(TControl)
  private
    FDocument : TFMXRichDocument;
    FRenderer : TFMXRichRenderer;
    FScrollPos: TPoint;
    FPageSize : TSize;
//    procedure UpdateScrollbars(Redraw: Boolean);
//    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
//    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
  protected
    procedure Paint; override;
//    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
//    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
//    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
//    procedure SetupCaret;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    property Document: TFMXRichDocument read FDocument;
  end;

implementation

{ TFMXRichDocument }

procedure TFMXRichDocument.AddBullet(AImageIndex: TImageIndex);
begin
  AddControl(TRichBullet.Create(Self, AImageIndex));
end;

procedure TFMXRichDocument.AddImage(AImage: TImage);
begin
  AddControl(TRichImage.Create(AImage));
end;

procedure TFMXRichDocument.SetImageList(Value: TImageList);
begin
  if FImageList <> Value then
  begin
//    if (FImageList = nil) or (Value = nil) or (FImageList.Width <> Value.Width) or (FImageList.Height <> Value.Height) then
    begin
      FEditor.FRenderer.NeedFormat;
    end;

    FImageList := Value;
    FEditor.Repaint;
  end;
end;

{ TRichEditor }

constructor TRichEditor.Create(AOwner: TComponent);
begin
  inherited;
  FDocument := TFMXRichDocument.Create;
  FRenderer := TFMXRichRenderer.Create;
  FDocument.FEditor := Self;
  FRenderer.FEditor := Self;
  FRenderer.Document := FDocument;
end;

destructor TRichEditor.Destroy;
begin
  FDocument.Free;
  FRenderer.Free;
  inherited;
end;

procedure TRichEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft then
  begin
    FRenderer.MouseDown(Button, Shift, Round(X) + FScrollPos.X, Round(Y) + FScrollPos.Y);
    SetFocus();
//    SetupCaret;
    Repaint;
  end;
end;

procedure TRichEditor.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if ssLeft in Shift then
  begin
    FRenderer.MouseMove(Shift, Round(X) + FScrollPos.X, Round(Y) + FScrollPos.Y);
//    SetupCaret;
    Repaint;
  end;
end;

procedure TRichEditor.Paint;
begin
  inherited;
  FRenderer.Paint(Canvas, - FScrollPos.X, - FScrollPos.Y);
  FPageSize.cx := FDocument.Margins.Left + FRenderer.Size.cx + FDocument.Margins.Right;
  FPageSize.cy := FDocument.Margins.Top + FRenderer.Size.cy + FDocument.Margins.Bottom;
//  UpdateScrollBars(True);
end;

procedure TRichEditor.Resize;
begin
  inherited;
  FRenderer.SetViewSize(Round(Width), Round(Height));
end;

{ TRichBullet }

constructor TRichBullet.Create(ADocument: TFMXRichDocument;
  AImageIndex: TImageIndex);
begin
  inherited Create;
  FDocument := ADocument;
  FImageIndex := AImageIndex;
end;

procedure TRichBullet.Draw(Canvas: TCanvas; ADrawItem: TDrawItem);
begin
  FDocument.FImageList.Draw(Canvas, TRectF.Create(ADrawItem.Left, ADrawITem.Top, ADrawItem.Right, ADrawItem.Bottom), FImageIndex);
end;

function TRichBullet.GetSize(var ASize: TSize): Boolean;
begin
  Result := True;
  with FDocument.FImageList.Source.Items[FimageIndex].MultiResBitmap.Items[0] do
  begin
    ASize.cx := Width;
    ASize.cy := Height;
  end;
end;

{ TRichImage }

constructor TRichImage.Create(AImage: TImage);
begin
  inherited Create;
  FImage := AImage;
end;

procedure TRichImage.Draw(Canvas: TCanvas; ADrawItem: TDrawItem);
var
  State: TCanvasSaveState;
begin
  State := Canvas.SaveState;
  FImage.PaintTo(Canvas, TRectF.Create(ADrawItem.Left, ADrawITem.Top, ADrawItem.Right, ADrawItem.Bottom));
  Canvas.RestoreState(State);
end;

function TRichImage.GetSize(var ASize: TSize): Boolean;
begin
  Result := True;
  ASize.cx := Round(FImage.Width);
  ASize.cy := Round(FImage.Height);
end;

{ TFMXRichRenderer }

procedure TFMXRichRenderer.DrawControl(DrawItem: TDrawItem);
begin
  if DrawItem.RichItem.Control is TRichFMXControlItem then
  begin
    TRichFMXControlItem(DrawItem.RichItem.Control).Draw(FCanvas, DrawItem);
  end else begin
    FCanvas.Stroke.Kind := TBrushKind.Solid;
    FCanvas.Stroke.Color := TAlphaColorRec.Red;
    FCanvas.DrawRect(TRectF.Create(DrawItem.Left, DrawItem.Top, DrawItem.Right, DrawItem.Bottom), 0 ,0, [], 1);
  end;
//  UpdateSelection(DrawItem);
end;

procedure TFMXRichRenderer.DrawSpecial(DrawItem: TDrawItem);
begin
  case DrawItem.Start of
    -1: // Break
    begin
      FCanvas.Stroke.Kind := TBrushKind.Solid;
      FCanvas.Stroke.Color := TAlphaColorRec.Black;
      FCanvas.DrawLine(TPointF.Create(DrawItem.Left, DrawItem.Top + 5.5), TPointF.Create(DrawItem.Right, DrawItem.Top + 5.5), 1);
    end;
  end;
//  UpdateSelection(DrawItem);
end;

procedure TFMXRichRenderer.DrawText(DrawItem: TDrawItem);
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

  FCaret.X := DrawItem.Left;
  FCaret.Y := DrawItem.Top;
  if a > 0 then
  begin
    TextOut(Text, a, False);
  end;
  Document.SetSelectionPosition(DrawItem.RichItem, FCaret.Round, DrawItem.Rect.Height);
  if b > 0 then
  begin
//    SetBkColor(FDC, $FF9933);
    FCanvas.Fill.Color := $3399FF or TAlphaColorRec.Alpha;
    FTextLayout.Color := TAlphaColorRec.White;
    TextOut(@Text[a], b, True);
  end;
  if FSelect = +1 then
  begin
    if b > 0 then
      Document.SetSelectionPosition(DrawItem.RichItem, FCaret.Round, DrawItem.Rect.Height);
//    SetBkColor(FDC, $FFFFFF);
    FCanvas.Fill.Color := TAlphaColorRec.White;
    FTextLayout.Color := DrawItem.RichItem.Style.Color or TAlphaColorRec.Alpha;
    FSelect := +2;
  end;
  if c > 0 then
  begin
    TextOut(@Text[a + b], c, False);
  end;

end;


function TFMXRichRenderer.MeasureText(RichItem: TRichItem;
  var ItemPos: TItemPos; Len: Integer): Integer;
var
  Ptr: PChar;
  Str: string;
begin
  Ptr := Pointer(RichItem.Text);
  Inc(Ptr, ItemPos.Start);
  repeat
    SetString(Str, Ptr, Len);
    FTextLayout.BeginUpdate;
    FTextLayout.Text := Str;
    FTextLayout.EndUpdate;
    ItemPos.Size.cx := Round(FTextLayout.TextWidth);
    ItemPos.Size.cy := Round(FTextLayout.TextHeight);
    Result := Len;
    Dec(Len);
  until (Result <= 1) or (ItemPos.Size.cx <= ItemPos.MaxWidth);
end;

procedure TFMXRichRenderer.Paint(Canvas: TCanvas; x, y: Integer);
var
  Item: TDrawItem;
begin
  FCanvas := Canvas;
  FTextLayout := TTextLayoutManager.DefaultTextLayout.Create(FCanvas);
  try
    FCanvas.Fill.Color := TAlphaColorRec.White;
    FCanvas.FillRect(TRectF.Create(0, 0, FWidth, FHeight), 0, 0, [], 1);

    if Document.Styles.Count = 0 then
      Exit;

    if FNeedFormat then
      Format;

    Item := Items.First;
    if Item = nil then
      Exit;

    FSelect := -1;

    FCanvas.MultiplyMatrix(TMatrix.CreateTranslation(x, y));

    repeat
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
    until Items.Next(Item) = False;
  finally
    FreeAndNil(FTextLayout);
  end;
end;

procedure TFMXRichRenderer.SetSelection(X, Y: Integer; Start: Boolean);
var
  Temp: Boolean;
begin
  Temp := FTextLayout = nil;
  if Temp then
  begin
    FCanvas := FEditor.Canvas;
    FTextLayout := TTextLayoutManager.DefaultTextLayout.Create(FCanvas);
  end;
  inherited;
  if Temp then
  begin
    FreeAndNil(FTextLayout);
    FCanvas := nil;
  end;
end;

function TFMXRichRenderer.SetStyle(AStyle: TRichStyleItem): Integer;
begin
  if AStyle = nil then
    Exit; // ??
  FTextLayout.BeginUpdate;
  FTextLayout.Color := AStyle.Color or TAlphaColorRec.Alpha;
  FTextLayout.Font.Family := AStyle.Font.Name;
  FTextLayout.Font.Size := (AStyle.Font.Size * 96) / 72; // 72dpi VCL -> 96dpi FMX
  FTextLayout.Font.Style := AStyle.Font.Style;
  FTextLayout.EndUpdate;
end;

procedure TFMXRichRenderer.TextOut(Text: PChar; Len: Integer; Selected: Boolean);
var
  Str: string;
  r  : TRectF;
begin
  SetString(Str, Text, Len);
  FTextLayout.BeginUpdate;
  FTextLayout.TopLeft := FCaret;
  FTextLayout.Text := Str;
  FTextLayout.EndUpdate;
  r := FTextLayout.TextRect;
  if Selected then
  begin
    FCanvas.FillRect(r, 0, 0, [], 1);
  end;
  FTextLayout.RenderLayout(FCanvas);
  FCaret.X := FCaret.X + r.Width;
end;

procedure TFMXRichRenderer.UpdateSelection(DrawItem: TDrawItem);
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
      //PatBlt(FDC, DrawItem.Left, DrawItem.Top, DrawItem.Rect.Width, DrawItem.Rect.Height, DSTINVERT);
    end;
  end;
  if Stop >= 0 then
  begin
    FSelect := +2;
    //SetBkColor(FDC, $FFFFFF);
  end;
end;


end.
