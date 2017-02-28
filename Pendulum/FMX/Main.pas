unit Main;
{
  Pendulum for Delphi Berlin (c)2017 by Execute SARL

  from

    "Pendulum Waves" video https://www.youtube.com/watch?v=yVkdfJ9PkRQ

  Javascript implementation

    http://www.interactiveds.com.au/images/DP.htm
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  System.Math.Vectors, FMX.Types3D, FMX.Objects3D, FMX.Controls3D,
  FMX.MaterialSources, FMX.Viewport3D, FMX.Ani;

const
  BALL_COUNT  = 15;      // Nombre de balles
  LENGTH0     = 450;     // Longueur du plus long fil
  PERIOD      = 50;      // Nombre de cycles avant de revenir à la position initiale
  START_ANGLE = 30;      // Angle de départ
  DT          = 1;    // Facteur de vitesse
  g           = 9.80665; // Gravité terrestre

type
  TBall = record
    Length  : Single;
    Angle   : Single;
    Speed   : Single;
  end;

  TForm1 = class(TForm)
    Timer1: TTimer;
    Circle1: TCircle;
    Line1: TLine;
    Viewport3D1: TViewport3D;
    Cylinder1: TCylinder;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    Sphere1: TSphere;
    Dummy1: TDummy;
    Dummy2: TDummy;
    FloatAnimation1: TFloatAnimation;
    Viewport3D2: TViewport3D;
    ProxyObject1: TProxyObject;
    Viewport3D3: TViewport3D;
    ProxyObject2: TProxyObject;
    Cube1: TCube;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
    FBalls  : array[0..BALL_COUNT - 1] of TBall;
    FLines  : array[0..BALL_COUNT - 1] of TLine;
    FDummies: array[0..BALL_COUNT - 1] of TDummy;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
var
  Index: Integer;
  T0, t: Single;
begin
  FLines[BALL_COUNT - 1] := Line1;
  FDummies[BALL_COUNT - 1] := Dummy1;
  Dummy1.Position.Z := BALL_COUNT - 8;

  for Index := BALL_COUNT - 2 downto 0 do
  begin
    FLines[Index] := Line1.Clone(Self) as TLine;
    FLines[Index].Parent := Self;

    FDummies[Index] := Dummy1.Clone(Self) as TDummy;
    FDummies[Index].Parent := Dummy2;
    FDummies[Index].Position.Z := Index - 7;
  end;

  T0 := Sqrt(LENGTH0 / g);
  for Index := 0 to BALL_COUNT - 1 do
  begin
    t := T0 * PERIOD / (PERIOD + Index);
    FBalls[Index].Length := g * t * t;
    FBalls[Index].Angle := -START_ANGLE * PI/180;
    FBalls[Index].Speed := 0;

    FLines[Index].Height := FBalls[Index].Length;
    TCircle(FLines[Index].Children[0]).Position.Y := FBalls[Index].Length;

    TCylinder(FDummies[Index].Children[0]).Height := FBalls[Index].Length / 20;
    TCylinder(FDummies[Index].Children[0]).Position.Y := FBalls[Index].Length / 40;
    TSphere(FDummies[Index].Children[0].Children[0]).Position.Y := FBalls[Index].Length / 40;
  end;
end;

procedure TForm1.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  Index : Integer;
  x1, y1: Integer;
  x2, y2: Integer;
  r     : TRectF;
begin
  Timer1.Tag := 0;
  x1 := ClientWidth div 4;
  y1 := 20;
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Color := TAlphaColors.Gray;
  Canvas.Fill.Color := TAlphaColors.White;
  for Index := BALL_COUNT - 1 downto 0 do
  begin
    with FBalls[Index] do
    begin
      x2 := x1 + Round(Length * Sin(Angle));
      y2 := y1 + Round(Length * Cos(Angle));
    end;
    Canvas.Stroke.Thickness := 1;
    Canvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(x2, y2), 1);
    Canvas.Stroke.Thickness := 2;
    r := TRectF.Create(x2 - 15, y2 - 15, x2 + 15, y2 + 15);
    Canvas.DrawEllipse(r, 1);
    Canvas.FillEllipse(r, 1);

    FLines[Index].RotationAngle := FBalls[Index].Angle * 180/PI;
    FDummies[Index].RotationAngle.Z := FBalls[Index].Angle * 180/PI;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Index: Integer;
begin
  if Timer1.Tag = 0 then
  begin
    Timer1.Tag := 1;
    for Index := BALL_COUNT - 1 downto 0 do
    begin
      with FBalls[Index] do
      begin
        Speed := Speed + DT * (-g / Length) * Sin(Angle);
        Angle := Angle + DT * Speed;
      end;
    end;
    Invalidate;
  end;
end;

end.
