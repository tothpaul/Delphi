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
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.DateUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.Samples.Spin;

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
    Label1: TLabel;
    Button1: TButton;
    procedure Timer1Timer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
    FBalls: array[0..BALL_COUNT - 1] of TBall;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Index: Integer;
  T0, t: Single;
begin
  T0 := Sqrt(LENGTH0 / g);
  for Index := 0 to BALL_COUNT - 1 do
  begin
    t := T0 * PERIOD / (PERIOD + Index);
    FBalls[Index].Length := g * t * t;
    FBalls[Index].Angle := -START_ANGLE * PI/180;
    FBalls[Index].Speed := 0;
  end;
  Tag := 0;
  Label1.Tag := 0;
  Label1.Caption := '0';
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  Index : Integer;
  x1, y1: Integer;
  x2, y2: Integer;
begin
  Timer1.Tag := 0;
  x1 := ClientWidth div 2;
  y1 := 20;
  Canvas.Pen.Color := clGray;
  Canvas.Brush.Color := clWhite;
  for Index := BALL_COUNT - 1 downto 0 do
  begin
    with FBalls[Index] do
    begin
      x2 := x1 + Round(Length * Sin(Angle));
      y2 := y1 + Round(Length * Cos(Angle));
    end;
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(x1, y1);
    Canvas.LineTo(x2, y2);
    Canvas.Pen.Width := 2;
    Canvas.Ellipse(x2 - 15, y2 - 15, x2 + 15, y2 + 15);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Index: Integer;
begin
  for Index := BALL_COUNT - 1 downto 0 do
  begin
    with FBalls[Index] do
    begin
      Speed := Speed + DT * (-g / Length) * Sin(Angle);
      Angle := Angle + DT * Speed;
    end;
  end;
  if FBalls[0].Speed < 0 then
  begin
    if Tag = 0 then
    begin
      Tag := 1;
      Label1.Tag := Label1.Tag + 1;
      Label1.Caption := IntToStr(Label1.Tag);
    end;
  end else begin
    Tag := 0;
  end;

  if Timer1.Tag = 0 then
  begin
    Timer1.Tag := 1;
    InvalidateRect(Handle, nil, True);
  end;
end;

end.
