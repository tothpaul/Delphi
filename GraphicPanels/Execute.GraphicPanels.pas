unit Execute.GraphicPanels;
{
  (c)2016-2018 By Execute SARL, Paul TOTH <contact@execute.fr>
  http://www.execute.fr
}
interface
{-$DEFINE TRACE}
uses
{$IFDEF DESIGN}
  DesignIntf,
{$ENDIF}
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Graphics;

type
  TGraphicPanel = class(TGraphicControl)
  private
  // Parent of this component (when it's not a TWinControl)
    FGraphicParent: TGraphicPanel;
  // Childs of this component (because it's not a TWinControl)
    FControls     : TList;
  // Padding from TWinControl
    FPadding      : TPadding;
  // Enable/DisableAlign
    FAlignLevel   : Integer;
  // this order the components with the same Align property
    FAlignWeight  : Integer;
  // used to compute size for alNone with Anchors
    FOldSize      : TSize;
  {$IFDEF DESIGN}
    FSelection    : TGraphicPanel;
    FMouseDown    : TPoint;
    function Designer: IDesigner;
    function SelectChild(Pos: TPoint): TGraphicPanel;
    function DoSelect(const Pos: TPoint): TGraphicPanel;
    procedure SetTopLeft(x, y: Integer);
  {$ENDIF}
  // set Parent as a TGraphicPanel
    procedure SetGraphicParent(const Value: TGraphicPanel);
    procedure SetAlignWeight(Value: Integer);
    procedure SetPadding(const Value: TPadding);
    procedure DoPaddingChange(Sender: TObject);
  // top level GraphicPanel
    function GetRoot: TGraphicPanel;
  // Paint this component from the Root point of view
    procedure Render;
  // is there any child with an Align or Anchors property that will alter it's size or position ?
    function AlignWork: Boolean;
  protected
    procedure RequestAlign; override;
    procedure AlignControl;
    procedure AlignControls(var Rect: TRect); virtual;
    procedure AdjustClientRect(var Rect: TRect); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure SetParentComponent(Value: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetClientOrigin: TPoint; override;
    procedure Invalidate; override;
    procedure DisableAlign;
    procedure EnableAlign;
    procedure Realign;
  {$IFDEF DESIGN}
    function DesignWndProc(var Msg: TMessage): Boolean; override;
  {$ENDIF}
    function Right: Integer; inline;
    function Bottom: Integer; inline;
    property GraphicParent: TGraphicPanel read FGraphicParent write SetGraphicParent;
  published
    property AlignWeight: Integer read FAlignWeight write SetAlignWeight;
    property Padding: TPadding read FPadding write SetPadding;
    property Align; // BEFORE Anchors !
    property Anchors;
    property Caption;
    property Color;
    property ParentColor;
  end;

implementation

{ TGraphicPanel }

procedure TGraphicPanel.AdjustClientRect(var Rect: TRect);
begin
  Inc(Rect.Left, FPadding.Left);
  Inc(Rect.Top, FPadding.Top);
  Dec(Rect.Right, FPadding.Right);
  Dec(Rect.Bottom, FPadding.Bottom);
end;

procedure TGraphicPanel.AlignControl;
var
  Rect: TRect;
begin
  if (csDestroying in ComponentState) then
    Exit;
  // DisableAlign
  if FAlignLevel <> 0 then
    ControlState := ControlState + [csAlignmentNeeded]
  else begin
    DisableAlign;
    try
      Rect := GetClientRect;
      AlignControls(Rect);
    finally
      ControlState := ControlState - [csAlignmentNeeded];
      EnableAlign;
    end;
  end;
end;

function SortForAlign(p1, p2: Pointer): Integer;
const
// alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom
  WEIGHT: array[TAlign] of Integer = (
    0, // alNone
    6, // alTop
    5, // alBottom
    4, // alLeft
    3, // alRight
    2, // alClient
    1  // alCustom
  );

var
  g1: TGraphicPanel absolute p1;
  g2: TGraphicPanel absolute p2;
begin
  if (g1.Align in [alNone, alClient, alCustom])
  or (g2.Align in [alNone, alClient, alCustom]) then
  begin
    Result := WEIGHT[g2.Align] - WEIGHT[g1.Align];
    if Result = 0 then
      Result := g2.FAlignWeight - g1.FAlignWeight;
  end else begin
    Result := g2.FAlignWeight - g1.FAlignWeight;
    if Result = 0 then
      Result := WEIGHT[g2.Align] - WEIGHT[g1.Align];
  end;
  if Result = 0 then // same Align & same Weight
  begin
    case g1.Align of
      alTop   : Result := g2.Top - g1.Top;
      alBottom: Result := g2.Bottom - g1.Bottom;
      alLeft  : Result := g2.Left - g1.Left;
      alRight : Result := g2.Right - g1.Right;
      alNone,
      alClient,
      alCustom: Result := g2.FGraphicParent.FControls.IndexOf(p1) - g1.GraphicParent.FControls.IndexOf(p2);
    end;
  end;
end;

procedure TGraphicPanel.AlignControls(var Rect: TRect);
var
  AlignList: TList;
  Index: Integer;
  Control: TGraphicControl;
  Margin: TRect;
  Anchor: Integer;
begin
  if AlignWork then
  begin
    AdjustClientRect(Rect);
    Rect.Offset(Left, Top);
    AlignList := TList.Create;
    try
      AlignList.Assign(FControls);
      AlignList.Sort(SortForAlign); // this is important !
      for Index := 0 to AlignList.Count - 1 do
      begin
        Control := AlignList[Index];
        if Control.AlignWithMargins then
        begin
        // warning, it's easy to use but confuse also
        //  Left  = Margins.Left
        //  Width = Margins.Right
        //  Right = Left + Width = Margins.Left + Margins.Right !
          Margin.Top := Control.Margins.Top;
          Margin.Left := Control.Margins.Left;
          Margin.Width := Control.Margins.Right;
          Margin.Height := Control.Margins.Bottom;
        end else begin
          FillChar(Margin, SizeOf(Margin), 0);
        end;
        case Control.Align of
          alNone  :
          begin
            // https://quality.embarcadero.com/browse/RSP-16478
            case Byte(Control.Anchors * [akLeft, akRight]) of
              0: // horizontal "center"
              begin
                Anchor := Control.Left;
                Inc(Anchor, (Width - FOldSize.cx) div 2);
                Control.Left := Anchor;
              end;
              1 shl Ord(alRight) :
              begin
                Anchor := Control.Left;
                Inc(Anchor, Width - FOldSize.cx);
                Control.Left := Anchor;
              end;
            else // Left/Right
              Anchor := Control.Width;
              Inc(Anchor, Width - FOldSize.cx);
              Control.Width := Anchor;
            end;

            case Byte(Control.Anchors * [akTop, akBottom]) of
              0: // vertical "center"
              begin
                Anchor := Control.Top;
                Inc(Anchor, (Height - FOldSize.cy) div 2);
                Control.Top := Anchor;
              end;
              1 shl Ord(alBottom) :
              begin
                Anchor := Control.Top;
                Inc(Anchor, Width - FOldSize.cx);
                Control.Top := Anchor;
              end;
            else // Top/Bottom
              Anchor := Control.Height;
              Inc(Anchor, Height - FOldSize.cy);
              Control.Height := Anchor;
            end;
          end;
          alTop   :
          begin
            Anchor := Control.Height;
            if akBottom in Control.Anchors then
            begin
              Inc(Anchor, Height - FOldSize.cy);
            end;
            Control.SetBounds(Rect.Left + Margin.Left, Rect.Top + Margin.Top, Rect.Width - Margin.Right, Anchor);
            Inc(Rect.Top, Control.Height + Margin.Bottom);
          end;
          alBottom:
          begin
            Anchor := Control.Height;
            if akTop in Control.Anchors then
            begin
              Inc(Anchor, Height - FOldSize.cy);
            end;
            Control.SetBounds(Rect.Left + Margin.Left, Rect.Bottom - Anchor - Margin.Height, Rect.Width - Margin.Right, Anchor);
            Dec(Rect.Bottom, Control.Height + Margin.Bottom);
          end;
          alLeft  :
          begin
            Anchor := Control.Width;
            if akRight in Control.Anchors then
            begin
              Inc(Anchor, Width - FOldSize.cx);
            end;
            Control.SetBounds(Rect.Left + Margin.Left, Rect.Top + Margin.Top, Anchor, Rect.Height - Margin.Bottom);
            Inc(Rect.Left, Control.Width + Margin.Right);
          end;
          alRight :
          begin
            Anchor := Control.Width;
            if akLeft in Control.Anchors then
            begin
              Inc(Anchor, Width - FOldSize.cx);
            end;
            Control.SetBounds(Rect.Right - Anchor - Margin.Width, Rect.Top + Margin.Top, Anchor, Rect.Height - Margin.Bottom);
            Dec(Rect.Right, Control.Width + Margin.Right);
          end;
          alClient:
          begin
            Control.SetBounds(Rect.Left + Margin.Left, Rect.Top + Margin.Top, Rect.Width - Margin.Right, Rect.Height - Margin.Bottom);
          end;
          alCustom: { todo } ;
        end;
      end;
    finally
      AlignList.Free;
    end;
  end
end;

function TGraphicPanel.AlignWork: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := FControls.Count - 1 downto 0 do
    if (TGraphicControl(FControls[I]).Align <> alNone) or
      (TGraphicControl(FControls[I]).Anchors <> [akLeft, akTop]) then Exit;
  Result := False;
end;

function TGraphicPanel.Bottom: Integer;
begin
  Result := Top + Height;
end;

constructor TGraphicPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls{, csDesignInteractive}];
  FControls := TList.Create;
  FPadding := TPadding.Create(Self);
  FPadding.OnChange := DoPaddingChange;
end;

{$IFDEF DESIGN}
function TGraphicPanel.Designer: IDesigner;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(GetRoot);
  Result := Form.Designer as IDesigner;
end;

function TGraphicPanel.DesignWndProc(var Msg: TMessage): Boolean;
begin
//{$IFDEF TRACE}WriteLn('DesignWndProc(', Msg.Msg,')');{$ENDIF}
  Result := False;
  case Msg.Msg of
    WM_LBUTTONDOWN:
    begin
      FMouseDown := TWMLButtonDown(Msg).Pos;
      FSelection := SelectChild(FMouseDown);
      if FSelection.FGraphicParent <> nil then
      begin
        FMouseDown.X := FSelection.Left - FMouseDown.X;
        FMouseDown.Y := FSelection.Top - FMouseDown.Y;
      end;
      Designer.SelectComponent(FSelection);
      Result := True; // don't know how to hide the selector :/
    end;
    WM_LBUTTONUP:
    begin
      Designer.Modified;
      Result := True;
    end;
    WM_MOUSEMOVE:
    begin
      if (Msg.WParam and MK_LBUTTON) <> 0 then
      begin
        if FSelection <> nil then
        begin
          if FSelection.FGraphicParent = nil then
          begin
            FSelection.SetTopLeft(
              FSelection.Left + TWMMouseMove(Msg).XPos - FMouseDown.X,
              FSelection.Top + TWMMouseMove(Msg).YPos - FMouseDown.Y
            );
          end else begin
            FSelection.SetTopLeft(
              TWMMouseMove(Msg).XPos + FMouseDown.X,
              TWMMouseMove(Msg).YPos + FMouseDown.Y
            );
          end;
        end;
      end;
      Result := True; // required!
    end;
  end;
end;

function TGraphicPanel.SelectChild(Pos: TPoint): TGraphicPanel;
var
  Index: Integer;
begin
  Pos.Offset(Left, Top);
  for Index := FControls.Count - 1 downto 0 do
  begin
    Result := TGraphicPanel(FControls[Index]).DoSelect(Pos);
    if Result <> nil then
      Exit;
  end;
  Result := Self;
end;

function TGraphicPanel.DoSelect(const Pos: TPoint): TGraphicPanel;
var
  Index: Integer;
begin
  for Index := 0 to FControls.Count - 1 do
  begin
    Result := TGraphicPanel(FControls[Index]).DoSelect(Pos);
    if Result <> nil then
      Exit;
  end;
  if BoundsRect.Contains(Pos) then
    Result := Self
  else
    Result := nil;
end;

procedure TGraphicPanel.SetTopLeft(x, y: Integer);
var
  dx, dy: Integer;
  Index : Integer;
begin
  dx := x - Left;
  dy := y - Top;
  DisableAlign;
  for Index := 0 to FControls.Count - 1 do
  begin
    with TGraphicPanel(FControls[Index]) do
      SetTopLeft(Left + dx, Top + dy);
  end;
  SetBounds(x, y, Width, Height);
  EnableAlign;
end;
{$ENDIF}

destructor TGraphicPanel.Destroy;
var
  I: Integer;
  Instance: TGraphicPanel;
begin
  if FGraphicParent <> nil then
    FGraphicParent.FControls.Remove(Self);
  I := FControls.Count;
  while I <> 0 do
  begin
    Instance := FControls[I - 1];
    Instance.Parent := nil;
    Instance.Destroy;
    I := FControls.Count;
  end;
  FControls.Free;
  FPadding.Free;
  inherited;
end;

procedure TGraphicPanel.DisableAlign;
begin
  Inc(FAlignLevel);
end;

procedure TGraphicPanel.DoPaddingChange(Sender: TObject);
  begin
  Realign;
  end;

procedure TGraphicPanel.EnableAlign;
begin
  Dec(FAlignLevel);
  if FAlignLevel = 0 then
  begin
    if csAlignmentNeeded in ControlState then
      Realign;
  end;
end;

procedure TGraphicPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  Index: Integer;
  Control: TControl;
begin
  for Index := 0 to FControls.Count - 1 do
  begin
    Control := FControls[Index];
    if Control.Owner = Root then
      Proc(Control);
  end;
end;

function TGraphicPanel.GetClientOrigin: TPoint;
begin
//{$IFDEF TRACE}WriteLn(Name, '.GetClientOrigin');{$ENDIF}
  if FGraphicParent = nil then
  begin
    Result := inherited GetClientOrigin;
  end else begin
    Result := FGraphicParent.GetClientOrigin;
    Inc(Result.X, Left);
    Inc(Result.Y, Top);
  end;
end;

function TGraphicPanel.GetParentComponent: TComponent;
begin
  if FGraphicParent <> nil then
    Result := FGraphicParent
  else
    Result := inherited GetParentComponent;
end;

function TGraphicPanel.GetRoot: TGraphicPanel;
begin
  Result := Self;
  while Result.FGraphicParent <> nil do
    Result := Result.FGraphicParent;
end;

function TGraphicPanel.HasParent: Boolean;
begin
  Result := (FGraphicParent <> nil) or inherited HasParent;
end;

procedure TGraphicPanel.Invalidate;
begin
{$IFDEF TRACE}WriteLn(Name, '.Invalidate');{$ENDIF}
  if FGraphicParent = nil then
    inherited
  else
    GetRoot.Invalidate;
end;

procedure TGraphicPanel.Paint;
var
  Rect : TRect;
  Str  : string;
  Index: Integer;
begin
  if (Width < 0) or (Height < 0) then
    Exit;
//{$IFDEF TRACE}WriteLn(Name, '.Paint');{$ENDIF}
  Rect := ClientRect;
  with Canvas do
  begin
    Pen.Color := clBlack;
    Brush.Color := Self.Color;
    Rectangle(Rect);
    Str := Caption;
    TextRect(Rect, Str, [tfCenter, tfVerticalCenter, tfSingleLine, tfEndEllipsis]);
  end;

  for Index := 0 to FControls.Count - 1 do
    TGraphicPanel(FControls[Index]).Render;
end;

procedure TGraphicPanel.Realign;
begin
  AlignControl;
end;

procedure TGraphicPanel.Render;
var
  Root: TGraphicPanel;
  Save: Integer;
begin
  Root := GetRoot;
  Canvas.Handle := Root.Canvas.Handle;
  try
    Save := SaveDC(Canvas.Handle);

    SetViewportOrgEx(Canvas.Handle, Left - Root.Left, Top - Root.Top, nil);
    IntersectClipRect(Canvas.Handle, 0, 0, Width, Height);

    Paint;

    RestoreDC(Canvas.Handle, Save);
  finally
    Canvas.Handle := 0;
  end;
end;

procedure TGraphicPanel.RequestAlign;
begin
{$IFDEF TRACE}WriteLn(Name, '.RequestAlign');{$ENDIF}
  if FGraphicParent = nil then
  begin
    inherited;
  end else begin
    FGraphicParent.AlignControl;
  end;
  AlignControl;
end;

function TGraphicPanel.Right: Integer;
begin
  Result := Left + Width;
end;

procedure TGraphicPanel.SetAlignWeight(Value: Integer);
begin
  if Value <> FAlignWeight then
  begin
    FAlignWeight := Value;
    RequestAlign;
  end;
end;

procedure TGraphicPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  FOldSize.cx := Width;
  FOldSize.cy := Height;
end;

procedure TGraphicPanel.SetGraphicParent(const Value: TGraphicPanel);
begin
  if Value <> FGraphicParent then
  begin
    if FGraphicParent <> nil then
      FGraphicParent.FControls.Remove(Self);
    FGraphicParent := Value;
    if FGraphicParent <> nil then
      FGraphicParent.FControls.Add(Self);
  end;
  Parent := nil;
end;

procedure TGraphicPanel.SetPadding(const Value: TPadding);
begin
  FPadding.Assign(Value);
end;
procedure TGraphicPanel.SetParentComponent(Value: TComponent);
begin
{$IFDEF TRACE}WriteLn(Name, '.SetParent');{$ENDIF}
  if (FGraphicParent <> Value) then
  begin
    if (Value is TGraphicPanel) then
      SetGraphicParent(TGraphicPanel(Value))
    else begin
      SetGraphicParent(nil);
      inherited;
  end;
end;
end;

initialization
{$IFDEF TRACE}AllocConsole;{$ENDIF}
end.
