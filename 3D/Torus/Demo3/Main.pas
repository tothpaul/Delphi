unit Main;

interface
{$POINTERMATH ON}
uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes, System.Math, System.DateUtils,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  Execute.Pixels,
  Execute.Parallelization;

type
  TVector = record
    x, y, z: Single;
  end;

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
    Pixels: TPixels;
    ScaleM: Integer;
    ScaleD: Integer;
    Title : string;
    Start : TDateTime;
    Frames: Integer;
    Eye   : TVector;
    Para  : TParallelizer;
    procedure Render;
    function PixelColor(x, y: Integer): Integer;
    procedure DrawPixel(Sender: TObject; Index: Integer);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  phix  : Byte;
  phiy  : Byte;
  phiz  : Byte;
  cosx, cosy: Single;
  sinx, siny: Single;
  cosz, sinz: Single;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Title := Caption;
  Start := Now();
  Eye.x := 0;
  Eye.y := 0;
  Eye.z := -800;
  Para := TParallelizer.Create;
  Para.OnIteration := DrawPixel;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Pixels.Resize(ClientWidth, ClientHeight);

  if Pixels.Width / 320 > Pixels.Height / 200 then
  begin
    ScaleD := Pixels.Height;
    ScaleM := 200;
  end else begin
    ScaleD := Pixels.Width;
    ScaleM := 320;
  end;

  Start := Now();
  Frames := 0;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Time: Int64;
begin
  Inc(phix, 1);
  Inc(phiy, 1);
  Inc(phiz, 2);
  if Timer1.Tag = 0 then
  begin
    Timer1.Tag := 1;
    InvalidateRect(Handle, nil, False);
  end;
  Time := SecondsBetween(Now, Start);
  if Time > 0 then
  begin
    Caption := Title + ' [' + IntToStr(Frames div Time) + ' FPS]';
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Inc(Frames);
  Render();
  Pixels.Draw(0, 0, Canvas);
  Timer1.Tag := 0;
end;

procedure TForm1.Render;
var
  x, y: Integer;
  Ofs : Integer;
begin
  cosx := cos(phix * 2*PI/255);
  cosy := cos(phiy * 2*PI/255);
  cosz := cos(phiz * 2*PI/255);
  sinx := sin(phix * 2*PI/255);
  siny := sin(phiy * 2*PI/255);
  sinz := sin(phiz * 2*PI/255);

  Para.Iterate(Pixels.Width * Pixels.Height);
//  Ofs := 0;
//  for y := 0 to Pixels.Height - 1 do
//  begin
//    for x := 0 to Pixels.Width - 1 do
//    begin
//      Pixels.Data[Ofs] := PixelColor(x, y);
//      Inc(Ofs);
//    end;
//  end;
end;

procedure TForm1.DrawPixel(Sender: TObject; Index: Integer);
begin
  Pixels.Data[Index] := PixelColor(Index mod Pixels.Width, Index div Pixels.Width);
end;

type
  TIntersection = record
    Position: TVector;
    Normal  : TVector;
    Color   : TRGBQuad;
    Dist    : Single;
  end;

// raycast_torus : (a² + b² + c² + d² – e²)(a² + b² + c² + d² – e² = 4R² * (x² + y²)

// raycast_sphere: x2 + y2 + z2 = R2

function raycast_sphere(const Src, Dst: TVector; var Inter: TIntersection; const Center: TVector; Radius: Single; Color: Integer): Boolean;
var
  P: TVector;
  a: Single;
  b: Single;
  c: Single;
  d: Single;
  t: Single;
begin
  Result := False;

  P.x := Dst.x - Src.x;
  P.y := Dst.y - Src.y;
  P.z := Dst.z - Src.z;

  a := sqr(P.x) + sqr(P.y) + sqr(P.z);
  b := 2 * (Src.x - Center.x) * P.x +
       2 * (Src.y - Center.y) * P.y +
       2 * (Src.z - Center.z) * P.z;
  c := sqr(Src.x - Center.x) +
       sqr(Src.y - Center.y) +
       sqr(Src.z - Center.z) -
       Sqr(Radius);

  // d = b² - 4ac
  d := sqr(b)-4*a*c;

  if d > 0 then
  begin
    t := (-b - sqrt(d)) / (2 * a);
    if t < 0 then
      Exit;
    P.x := t * p.x;
    P.y := t * p.y;
    P.z := t * p.z;
    d := sqr(P.x) + sqr(P.y) + sqr(P.z);
    if d < Inter.Dist then
    begin
      Inter.Dist := d;
      Inter.Position.x := Src.x + P.x;
      Inter.Position.y := Src.y + P.y;
      Inter.Position.z := Src.z + P.z;
      Inter.Normal.x := Inter.Position.x - Center.x;
      Inter.Normal.y := Inter.Position.y - Center.y;
      Inter.Normal.z := Inter.Position.z - Center.z;
      Cardinal(Inter.Color) := Color;
      Result := True;
    end;
  end;
end;

function raycast_sphere1(const Src, Dst: TVector; var Inter: TIntersection): Boolean;
const
  Center : TVector = (x:0; y:0; z:0);
  Radius = 50;
begin
  Result :=  raycast_sphere(Src, Dst, Inter, Center, Radius, $704800);
end;

function raycast_sphere2(const Src, Dst: TVector; var Inter: TIntersection): Boolean;
const
  Radius = 50;
var
  Center: TVector;
  x1,y1,z1: Single;
  x2,y2,z2: Single;
begin
  Center.x := 110;
  Center.y := 0;
  Center.z := 0;

  x1 := Center.x;
  y1 := Center.y * cosx - Center.z * sinx;
  z1 := Center.y * sinx + Center.z * cosx;

  x2 := x1 * cosy + z1 * siny;
  y2 := y1;
  z2 := z1 * cosy - x1 * siny;

  Center.x := x2 * cosz - y2 * sinz;
  Center.y := x2 * sinz + y2 * cosz;
  Center.z := z2;

  Result :=  raycast_sphere(Src, Dst, Inter, Center, Radius, $204800);
end;

procedure Normalize(var p: TVector);
var
  l: Single;
begin
  l := 1 / sqrt(p.x * p.x + p.y * p.y + p.z * p.z);
  p.x := p.x * l;
  p.y := p.y * l;
  p.z := p.z * l;
end;

function GetColor(const Color: TRGBQuad; const Vector: TVector): Integer;
const
  Light: TVector = (x: 1; y: 1; z: 1);
var
  f: Single;
begin
  f := 2 * ABS((Light.x * Vector.x) + (Light.y * Vector.y) + (Light.z * Vector.z) * 96);
  Result := RGB(Color.rgbRed  + Round(f / 1.6), Color.rgbGreen + Round(f / 1.4), Color.rgbBlue + Round(f));
end;

function TForm1.PixelColor(x, y: Integer): Integer;
var
  Ray   : TVector;
  fx, fy: Single;
  qx, qy: Single;
  d     : Single;
  c     : Integer;
  I     : TIntersection;
  h     : Boolean;
begin
  fx := ((x - Pixels.Width div 2) * ScaleM) / ScaleD;
  fy := ((y - Pixels.Height div 2) * ScaleM) / ScaleD;

  Ray.x := fx;
  Ray.y := fy;
  Ray.z := 0;

  I.Dist := 1e100;
  h := raycast_sphere1(Eye, Ray, I);
  h := raycast_sphere2(Eye, Ray, I) or h;
  if h then
  begin
    Normalize(I.Normal);
    Result := GetColor(I.Color, I.Normal);
    Exit;
  end;

  // Background
  fy := (y * 200) div Pixels.Height;
  if fy < 128 then
    Result := RGB(80  + Round((2 * fy) / 1.6), 40 + Round((2 * fy) / 1.4), Round(2 * fy))
  else begin
    fy := fy - 128;
    Result := RGB(40 + Round((2 * fy) / 1.4), 80 + Round((2 * fy) / 1.6), Round(2 * fy));
  end;
end;


end.
