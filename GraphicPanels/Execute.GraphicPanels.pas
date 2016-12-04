unit Execute.GraphicPanels;
{
  (c)2016 By Execute SARL, Paul TOTH <contact@execute.fr>
  http://www.execute.fr
}
interface

uses
  DesignIntf,
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
    procedure SetGraphicParent(const Value: TGraphicPanel);
    function GetRoot: TGraphicPanel;
    procedure Render;
    function SelectChild(Pos: TPoint): Boolean;
    function DoSelect(const Pos: TPoint): Boolean;
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
//    function DesignWndProc(var Msg: TMessage): Boolean; override;
  published
    property Caption;
    property Color;
    property GraphicParent: TGraphicPanel read FGraphicParent write SetGraphicParent;
  end;

implementation

{ TGraphicPanel }

constructor TGraphicPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls{, csDesignInteractive}];
  FControls := TList.Create;
end;

//function TGraphicPanel.DesignWndProc(var Msg: TMessage): Boolean;
//begin
//  Result := False;
//  case Msg.Msg of
//    WM_LBUTTONDOWN: SelectChild(TWMLButtonDown(Msg).Pos);   <-- GraphicPanel2 move out of the form Top = Left = -813575024
//  end;
//end;

destructor TGraphicPanel.Destroy;
begin
  if FGraphicParent <> nil then
    FGraphicParent.FControls.Remove(Self);
  FControls.Free;
  inherited;
end;

function TGraphicPanel.DoSelect(const Pos: TPoint): Boolean;
var
  Index: Integer;
  Form : TCustomForm;
begin
  Result := True;
  for Index := 0 to FControls.Count - 1 do
  begin
    if TGraphicPanel(FControls[Index]).DoSelect(Pos) then
      Exit;
  end;
  if BoundsRect.Contains(Pos) then
  begin
    Form := GetParentForm(GetRoot);
    (Form.Designer as IDesigner).SelectComponent(Self);
    Exit;
  end;
  Result := False;
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

function TGraphicPanel.SelectChild(Pos: TPoint): Boolean;
var
  Index: Integer;
begin
  Pos.Offset(Left, Top);
  for Index := 0 to FControls.Count - 1 do
  begin
    if TGraphicPanel(FControls[Index]).DoSelect(Pos) then
      Exit(True);
  end;
  Result := False;
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
