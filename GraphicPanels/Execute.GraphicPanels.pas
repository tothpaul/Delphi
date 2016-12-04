unit Execute.GraphicPanels;
{
  (c)2016 By Execute SARL, Paul TOTH <contact@execute.fr>
  http://www.execute.fr
}
interface

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
    FGraphicParent: TGraphicPanel;
    FControls     : TList;
  {$IFDEF DESIGN}
    FSelection    : TGraphicPanel;
    FMouseDown    : TPoint;
    function Designer: IDesigner;
    function SelectChild(Pos: TPoint): TGraphicPanel;
    procedure SetTopLeft(x, y: Integer);
  {$ENDIF}
    procedure SetGraphicParent(const Value: TGraphicPanel);
    function GetRoot: TGraphicPanel;
    procedure Render;
    function DoSelect(const Pos: TPoint): TGraphicPanel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure SetParentComponent(Value: TComponent); override;
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetClientOrigin: TPoint; override;
    procedure Invalidate; override;
  {$IFDEF DESIGN}
    function DesignWndProc(var Msg: TMessage): Boolean; override;
  {$ENDIF}
    property GraphicParent: TGraphicPanel read FGraphicParent write SetGraphicParent;
  published
    property Caption;
    property Color;
  end;

implementation

{ TGraphicPanel }

constructor TGraphicPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls{, csDesignInteractive}];
  FControls := TList.Create;
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
//            GetRoot.Parent.Invalidate;
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
  for Index := 0 to FControls.Count - 1 do
  begin
    Result := TGraphicPanel(FControls[Index]).DoSelect(Pos);
    if Result <> nil then
      Exit;
  end;
  Result := Self;
end;

procedure TGraphicPanel.SetTopLeft(x, y: Integer);
var
  dx, dy: Integer;
  Index : Integer;
begin
  dx := x - Left;
  dy := y - Top;
  SetBounds(x, y, Width, Height);
  for Index := 0 to FControls.Count - 1 do
  begin
    with TGraphicPanel(FControls[Index]) do
      SetTopLeft(Left + dx, Top + dy);
  end;
end;
{$ENDIF}

destructor TGraphicPanel.Destroy;
begin
  if FGraphicParent <> nil then
    FGraphicParent.FControls.Remove(Self);
  FControls.Free;
  inherited;
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

procedure TGraphicPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  Index: Integer;
  Control: TControl;
begin
  for Index := 0 to FControls.Count - 1 do
  begin
    Control := FControls[Index];
    if Control.Owner = Root then Proc(Control);
  end;
end;

function TGraphicPanel.GetClientOrigin: TPoint;
begin
  if FGraphicParent = nil then
  begin
    inherited;
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
  if FGraphicParent = nil then
    inherited
  else
    FGraphicParent.Invalidate;
end;

procedure TGraphicPanel.Paint;
var
  Rect : TRect;
  Str  : string;
  Index: Integer;
begin
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

procedure TGraphicPanel.Render;
var
  Root: TGraphicPanel;
  Save: Integer;
begin
  Root := GetRoot;
  Canvas.Lock;
  try
    Canvas.Handle := FGraphicParent.Canvas.Handle;

    Save := SaveDC(Canvas.Handle);

    SetViewportOrgEx(Canvas.Handle, Left - Root.Left, Top - Root.Top, nil);
    IntersectClipRect(Canvas.Handle, 0, 0, Width, Height);

    Paint;

    RestoreDC(Canvas.Handle, Save);
  finally
    Canvas.Unlock;
  end;
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

procedure TGraphicPanel.SetParentComponent(Value: TComponent);
begin
  if (FGraphicParent <> Value) then
  begin
    if (Value is TGraphicPanel) then
      SetGraphicParent(TGraphicPanel(Value))
    else
      inherited;
  end;
end;

end.
