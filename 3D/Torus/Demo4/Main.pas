unit Main;

interface
{$POINTERMATH ON}
uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes, System.DateUtils,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  Execute.Pixels,
  Execute.Parallelization;

type
  TForm4 = class(TForm)
    Timer1: TTimer;
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
    Pixels: TPixels;
    ScaleM, ScaleD: Integer;
    Para  : TParallelizer;
    Title : string;
    Start : TDateTime;
    Frames: Integer;
    procedure Render;
    function PixelColor(x, y: Integer): Integer;
    procedure DrawPixel(Sender: TObject; Index: Integer);
  public
    { Déclarations publiques }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

var
  phix  : Byte;
  phiy  : Byte;
  phiz  : Byte;
  cosx, cosy: Single;
  sinx, siny: Single;
  cosz, sinz: Single;

procedure TForm4.FormCreate(Sender: TObject);
begin
  Title := Caption;
  Start := Now();
  Para := TParallelizer.Create;
  Para.OnIteration := DrawPixel;
end;

procedure TForm4.FormResize(Sender: TObject);
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

procedure TForm4.Timer1Timer(Sender: TObject);
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

procedure TForm4.FormPaint(Sender: TObject);
begin
  Inc(Frames);
  cosx := cos(phix * 2*PI/255);
  cosy := cos(phiy * 2*PI/255);
  cosz := cos(phiz * 2*PI/255);
  sinx := sin(phix * 2*PI/255);
  siny := sin(phiy * 2*PI/255);
  sinz := sin(phiz * 2*PI/255);
  Render();
  Pixels.Draw(0, 0, Canvas);
  Timer1.Tag := 0;
end;

procedure TForm4.Render;
//var
//  Ofs : Integer;
//  x, y: Integer;
//begin
//  Ofs := 0;
//  for y := 0 to Pixels.Height - 1 do
//  begin
//    for x := 0 to Pixels.Width - 1 do
//    begin
//      Pixels.Data[Ofs] := PixelColor(x, y);
//      Inc(Ofs);
//    end;
//  end;
begin
  Para.Iterate(Pixels.Width * Pixels.Height);
end;


procedure TForm4.DrawPixel(Sender: TObject; Index: Integer);
begin
  Pixels.Data[Index] := PixelColor(Index mod Pixels.Width, Index div Pixels.Width);
end;

type
  vec2 = record
    x, y: Single;
    constructor Create(ax, ay: Single);
  end;

  vec3 = record
    x, y, z: Single;
    constructor Create(ax, ay, az: Single);
    class operator implicit(f: Single): vec3; inline;
    class operator Add(const v1, v2: vec3): vec3; inline;
    class operator Subtract(const v1, v2: vec3): vec3; inline;
    class operator multiply(const v: vec3; f: Single): vec3; inline;
    class operator divide(const v: vec3; f: Single): vec3; inline;
    function xy: vec2; inline;
    function xz: vec2; inline;
  end;

constructor vec2.Create(ax: Single; ay: Single);
begin
  x := ax;
  y := ay;
end;

constructor vec3.Create(ax: Single; ay: Single; az: Single);
begin
  x := ax;
  y := ay;
  z := az;
end;

function vec3.xy: vec2;
begin
  Result.x := x;
  Result.y := y;
end;


function vec3.xz: vec2;
begin
  Result.x := x;
  Result.y := z;
end;

class operator vec3.implicit(f: Single): vec3;
begin
  Result.x := f;
  Result.y := f;
  Result.z := f;
end;

class operator vec3.Add(const v1, v2: vec3): vec3;
begin
  Result.x := v1.x + v2.x;
  Result.y := v1.y + v2.y;
  Result.z := v1.z + v2.z;
end;

class operator vec3.Subtract(const v1, v2: vec3): vec3;
begin
  Result.x := v1.x - v2.x;
  Result.y := v1.y - v2.y;
  Result.z := v1.z - v2.z;
end;

class operator vec3.multiply(const v: vec3; f: Single): vec3;
begin
  Result.x := v.x * f;
  Result.y := v.y * f;
  Result.z := v.z * f;
end;

class operator vec3.divide(const v: vec3; f: Single): vec3;
begin
  Result := v * (1/f);
end;

function max(a, b: Single): Single; overload;
begin
  if a < b then
    Result := b
  else
    Result := a;
end;

function max(const v1, v2: vec3): vec3; overload;
begin
  Result.x := max(v1.x, v2.x);
  Result.y := max(v1.y, v2.y);
  Result.z := max(v1.z, v2.z);
end;

function abs(a: Single): Single; overload;
begin
  if a < 0 then
    Result := -a
  else
    Result := a;
end;

function abs(const v: vec3): vec3; overload;
begin
  Result.x := abs(v.x);
  Result.y := abs(v.y);
  Result.z := abs(v.z);
end;

function length(const v: vec2): Single; overload;
begin
  Result := sqrt(v.x * v.x + v.y * v.y);
end;

function length(const v: vec3): Single; overload;
begin
  Result := sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
end;

function normalize(const v: vec3): vec3;
var
  l: Single;
begin
  l := length(v);
  if abs(l) > 0.001 then
    Result := v / l;
end;

function dot(const x, y: vec3): Single;
begin
  Result := x.x * y.x +
            x.y * y.y +
            x.z * y.z;
end;

function R(const p: vec3): vec3;
var
  x1,y1,z1: Single;
  x2,y2,z2: Single;
begin
  x1 := p.x;
  y1 := p.y * cosx - p.z * sinx;
  z1 := p.y * sinx + p.z * cosx;

  x2 := x1 * cosy + z1 * siny;
  y2 := y1;
  z2 := z1 * cosy - x1 * siny;

  Result.x := x2 * cosz - y2 * sinz;
  Result.y := x2 * sinz + y2 * cosz;
  Result.z := z2;
end;

function R45(const p: vec3): vec3;
begin
  Result.x := p.x;
  Result.y := p.z;
  Result.z := p.y;
end;

function fmod(a, b: Single): Single;
begin
  Result := a - Round(a / b) * b;
end;

function distance(p: vec3; var pColor: Integer): single;
var
  d: Single;
begin
  p := R(p);

//  p.x := fmod(p.x, 50);
//  p.x := p.x + fmod(p.y, 10) - 5;
//  p.y := p.y + sqrt(p.z * p.z) - p.Z;
//  p.z := fmod(p.z, 100);

//  Result := length(p) - 50;
  Result := length(vec2.Create(length(p.xy) - 60, p.z)) - 28;

  pColor := $204800;

  p := R45(p);

//  p.x := p.x + 100;
//  d := length(p) - 50;

  p.x := p.x + 50;
  d := length(vec2.Create(length(p.xy) - 60, p.z)) - 28;

  if d < Result then
  begin
    Result := d;
    pColor := $704800;
  end;
end;

function GetColor(const Color: TRGBQuad; const Vector: vec3): Integer;
const
  Light: vec3 = (x: 1; y: 1; z: 1);
var
  f: Single;
begin
  f := 2 * ABS((Light.x * Vector.x) + (Light.y * Vector.y) + (Light.z * Vector.z) * 96);
  Result := RGB(Color.rgbRed  + Round(f / 1.6), Color.rgbGreen + Round(f / 1.4), Color.rgbBlue + Round(f));
end;

function TForm4.PixelColor(x, y: Integer): Integer;
var
  p: vec3;
  n: vec3;
  k: vec3;
  l: Single;
  f: Single;
  d: Single;
  step: Integer;
  c: Integer;
begin
  p.x := 0;
  p.y := 0;
  p.z := -800;

  n.x := ((x - Pixels.Width div 2) * ScaleM) / ScaleD;
  n.y := ((y - Pixels.Height div 2) * ScaleM) / ScaleD;
  n.z := 0;

  n := n - p;
  l := length(n);

  f := 0;

  k := p;
  for step := 1 to 90 do
  begin
    d := distance(k, c);
    if d < 1 then
     break;
    f := f + d;
    if f > l*2 then
      break;
    k := p + n * (f/l);
  end;

  if d < 1 then
  begin
    Result := GetColor(TRGBQuad(c), normalize(k));
  end else begin
    // Background
    f := (y * 200) div Pixels.Height;
    if f < 128 then
      Result := RGB(80  + Round((2 * f) / 1.6), 40 + Round((2 * f) / 1.4), Round(2 * f))
    else begin
      f := f - 128;
      Result := RGB(40 + Round((2 * f) / 1.4), 80 + Round((2 * f) / 1.6), Round(2 * f));
    end;
  end;

end;


end.
