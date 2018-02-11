unit Execute.VCL.RichEditor;

{ RichDocument (c)2018 by Execute SARL <contact@execute.fr> }

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.CommCtrl,
  System.Classes,
  System.UITypes,
  System.Math,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Execute.RichDocuments,
  Execute.RichRenderers,
  Execute.Win.RichRenderer;

type
  TVCLRichDocument = class;
  TRichEditor = class;

  TRichBullet = class(TRichWinControlItem)
  private
    FDocument  : TVCLRichDocument;
    FImageIndex: TImageIndex;
  protected
    procedure Draw(ADC: HDC; ADrawItem: TDrawItem); override;
  public
    constructor Create(ADocument: TVCLRichDocument; AImageIndex: TImageIndex);
    function GetSize(var ASize: TSize): Boolean; override;
  end;

  TRichImage = class(TRichWinControlItem)
  private
    FImage: TImage;
  protected
    procedure Draw(ADC: HDC; ADrawItem: TDrawItem); override;
  public
    constructor Create(AImage: TImage);
    function GetSize(var ASize: TSize): Boolean; override;
  end;

  TVCLRichDocument = class(TRichDocument)
  private
    FEditor   : TRichEditor;
    FImageList: TImageList;
    procedure SetImageList(Value: TImageList);
  public
    procedure AddBullet(AImageIndex: TImageIndex);
    procedure AddImage(AImage: TImage);
    property Images: TImageList read FImageList write SetImageList;
  end;

  TVCLRichRenderer = class(TWinRichRenderer)
  end;

  TRichEditor = class(TWinControl)
  private
    FDocument : TVCLRichDocument;
    FRenderer : TVCLRichRenderer;
    FScrollPos: TPoint;
    FPageSize : TSize;
    procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure UpdateScrollbars(Redraw: Boolean);
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
  protected
    procedure PaintWindow(DC: HDC); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure SetupCaret;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    property Document: TVCLRichDocument read FDocument;
  end;

implementation

{ TRichBullet }

constructor TRichBullet.Create(ADocument: TVCLRichDocument;
  AImageIndex: TImageIndex);
begin
  inherited Create;
  FDocument := ADocument;
  FImageIndex := AImageIndex;
end;

function TRichBullet.GetSize(var ASize: TSize): Boolean;
begin
  Result := FDocument.FImageList <> nil;
  if Result then
  begin
    ASize.cx := FDocument.FImageList.Width;
    ASize.cy := FDocument.FImageList.Height;
  end;
end;

function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone: Result := CLR_NONE;
    clDefault: Result := CLR_DEFAULT;
  end;
end;

procedure TRichBullet.Draw(ADC: HDC; ADrawItem: TDrawItem);
begin
  if FDocument.FImageList = nil then
    Exit;
//  FDocument.FImageList.Draw(Canvas, DrawItem.Left, DrawItem.Top, FImageIndex);
  with FDocument.FImageList do
    ImageList_DrawEx(Handle, FImageIndex, ADC, ADrawItem.Left, ADrawItem.Top, 0, 0,
        CLR_NONE, CLR_NONE, ILD_NORMAL)
end;

{ TRichImage }

constructor TRichImage.Create(AImage: TImage);
begin
  inherited Create;
  FImage := AImage;
end;

procedure TRichImage.Draw(ADC: HDC; ADrawItem: TDrawItem);
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := ADC;
    Canvas.Draw(ADrawItem.Left, ADrawItem.Top, FImage.Picture.Graphic);
    Canvas.Handle := 0;
  finally
    Canvas.Free;
  end;
end;

function TRichImage.GetSize(var ASize: TSize): Boolean;
begin
  Result := True;
  ASize.cx := FImage.Width;
  ASize.cy := FImage.Height;
end;

{ TVCLRichDocument }

procedure TVCLRichDocument.AddImage(AImage: TImage);
begin
  AddControl(TRichImage.Create(AImage));
end;

procedure TVCLRichDocument.SetImageList(Value: TImageList);
begin
  if FImageList <> Value then
  begin
    if (FImageList = nil) or (Value = nil) or (FImageList.Width <> Value.Width) or (FImageList.Height <> Value.Height) then
    begin
      FEditor.FRenderer.NeedFormat;
    end;

    FImageList := Value;
    FEditor.Invalidate;
  end;
end;

procedure TVCLRichDocument.AddBullet(AImageIndex: TImageIndex);
begin
  AddControl(TRichBullet.Create(Self, AImageIndex));
end;

{ TRichEditor }

constructor TRichEditor.Create(AOwner: TComponent);
begin
  inherited;
  FDocument := TVCLRichDocument.Create;
  FRenderer := TVCLRichRenderer.Create;
  FDocument.FEditor := Self;
  FRenderer.Document := FDocument;
end;

destructor TRichEditor.Destroy;
begin
  FDocument.Free;
  FRenderer.Free;
  inherited;
end;

function TRichEditor.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if WheelDelta > 0 then
    Dec(FScrollPos.Y, 40)
  else
    Inc(FScrollPos.Y, 40);
  UpdateScrollBars(True);
  Invalidate;
  Result := True;
end;

procedure TRichEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    FRenderer.MouseDown(Button, Shift, X + FScrollPos.X, Y + FScrollPos.Y);
    SetFocus();
    SetupCaret;
    Invalidate;
  end;
end;

procedure TRichEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
  begin
    FRenderer.MouseMove(Shift, X + FScrollPos.X, Y + FScrollPos.Y);
    SetupCaret;
    Invalidate;
  end;
end;

procedure TRichEditor.WMEraseBkGnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TRichEditor.WMHScroll(var Msg: TWMHScroll);
var
  Info: TScrollInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(Info);
  Info.fMask := SIF_TRACKPOS;
  GetScrollInfo(Handle, SB_HORZ, Info);
  case Msg.ScrollCode of
    SB_BOTTOM        : FScrollPos.X := FPageSize.cx;
    SB_ENDSCROLL     : ;
    SB_LINELEFT      : Dec(FScrollPos.X);
    SB_LINERIGHT     : Inc(FScrollPos.X);
    SB_PAGELEFT      : Dec(FScrollPos.X, 10);
    SB_PAGERIGHT     : Inc(FScrollPos.X, 10);
    SB_THUMBPOSITION,
    SB_THUMBTRACK    : FScrollPos.X := Info.nTrackPos;
    SB_TOP           : FScrollPos.X := 0;
  end;
  UpdateScrollBars(False);
  Invalidate;
end;

procedure TRichEditor.WMKillFocus(var Msg: TWMKillFocus);
begin
  DestroyCaret;
end;

procedure TRichEditor.WMPaint(var MSg: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

procedure TRichEditor.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  SetupCaret;
end;

procedure TRichEditor.WMVScroll(var Msg: TWMVScroll);
var
  Info: TScrollInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(Info);
  Info.fMask := SIF_TRACKPOS;
  GetScrollInfo(Handle, SB_VERT, Info);
  case Msg.ScrollCode of
    SB_BOTTOM        : FScrollPos.Y := FPageSize.cy;
    SB_ENDSCROLL     : ;
    SB_LINELEFT      : Dec(FScrollPos.Y, 20);
    SB_LINERIGHT     : Inc(FScrollPos.Y, 20);
    SB_PAGELEFT      : Dec(FScrollPos.Y, ClientHeight div 20);
    SB_PAGERIGHT     : Inc(FScrollPos.Y, ClientHeight div 20);
    SB_THUMBPOSITION,
    SB_THUMBTRACK    : FScrollPos.Y := Info.nTrackPos;
    SB_TOP           : FScrollPos.Y := 0;
  end;
  UpdateScrollBars(False);
  Invalidate;
end;

procedure TRichEditor.PaintWindow(DC: HDC);
var
  BDC: HDC;
  Bmp: HBitmap;
begin
{$IFDEF MSWINDOWS} // do not use DoubleBuffered under CrossVCL
  BDC := CreateCompatibleDC(DC);
  BMP := CreateCompatibleBitmap(DC, Width, Height);
  SelectObject(BDC, BMP);
{$ELSE}
  BDC := DC;
{$ENDIF}

  FRenderer.Paint(BDC, - FScrollPos.X, - FScrollPos.Y);
  FPageSize.cx := FDocument.Margins.Left + FRenderer.Size.cx + FDocument.Margins.Right;
  FPageSize.cy := FDocument.Margins.Top + FRenderer.Size.cy + FDocument.Margins.Bottom;
  UpdateScrollBars(True);

  if Focused then
    HideCaret(Handle);

{$IFDEF MSWINDOWS}
  BitBlt(DC, 0, 0, Width, Height, BDC, 0, 0, SRCCOPY);
{$ENDIF}

  if Focused then
  begin
    SetupCaret;
//    ShowCaret(Handle);
  end;

{$IFDEF MSWINDOWS}
  DeleteObject(BMP);
  DeleteDC(BDC);
{$ENDIF}
end;

procedure TRichEditor.Resize;
begin
  inherited;
  FRenderer.SetViewSize(Width, Height);
end;

procedure TRichEditor.SetupCaret;
var
  Caret: TRichCaret;
begin
  FDocument.GetCaret(Caret);
  CreateCaret(Handle, 0, 2, Caret.Height);
  SetCaretPos(Caret.Left - FScrollPos.X, Caret.Top - FScrollPos.Y);
  ShowCaret(Handle);
end;

procedure TRichEditor.UpdateScrollbars(Redraw: Boolean);
var
  Info: TScrollInfo;
begin
{
  nMin   nPos             nMax
  [<     [======]        >]
  :.........nPage.........:
}
  Info.cbSize := SizeOf(Info);
  Info.fMask := SIF_POS or SIF_RANGE or SIF_PAGE;
  Info.nMin := 0;

  Info.nPage := ClientWidth;
  FScrollPos.X := Max(0, Min(FPageSize.cx - Integer(Info.nPage), FScrollPos.X));
  Info.nMax := FPageSize.cx;
  Info.nPos := FScrollPos.X;
  SetScrollInfo(Handle, SB_HORZ, Info, Redraw);

  Info.nPage := ClientHeight;
  FScrollPos.Y := Max(0, Min(FPageSize.cy - Integer(Info.nPage), FScrollPos.Y));
  Info.nMax := FPageSize.cy;
  Info.nPos := FScrollPos.Y;
  SetScrollInfo(Handle, SB_VERT, Info, Redraw);
end;

end.
