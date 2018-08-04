unit Main;

interface
{$POINTERMATH ON}
uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes, System.DateUtils,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, System.Math,

  Execute.Trigo,
  Execute.Pixels,
  Execute.Torus;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
    ScaleM: Integer;
    ScaleD: Integer;
    Title : string;
    Start : TDateTime;
    Frames: Integer;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  Pixels   : TPixels;
  BaseShade: Word;

function Shade(const vector:TVector): Integer;
begin
  Result := BaseShade + ABS(Round((Light.x * Vector.x) + (Light.y * Vector.y) + (Light.z * Vector.z) * 96));
end;

procedure DrawWorld;
var
  i : Integer;
begin
  rotate(Coords, Coords, VertexCount);
  project(Coords, VertexCount, Form1.ScaleM, Form1.ScaleD);

  rotate(VNormals, Shades, VertexCount);
  rotate(FNormals, Hidden, FaceCount);

  for i := 0 to FaceCount-1 do
   if Hidden[i].z < 0 then
     with Faces[i] do
//      Pixels.Flat(Coords[a], Coords[b], Coords[c], Shade(Hidden[i]));
      Pixels.Gouraud(Coords[a], Coords[b], Coords[c], Shade(Shades[a]), Shade(Shades[b]), Shade(Shades[c]));
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  fac = 2;
var
  i: Integer;
begin
  Title := Caption;
  Start := Now();

  // Blue gradient
  for i:= 0 to 127 do
  begin
    Pixels.Palette[i] := RGB(80  + Round((i * fac) / 1.6), 40 + Round((i * fac) / 1.4), i * fac);
  end;

  // Green gradient
  for i:= 0 to 127 do
  begin
    Pixels.Palette[128 + i] :=  RGB(40 + Round((i * fac) / 1.4), 80 + Round((i * fac) / 1.6), i * fac);
  end;

  phix := 0;
  phiy := 0;
  phiz := 0;
  with Light do
  begin
    x := 1;
    y := 1;
    z := 1;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Pixels.Resize(ClientWidth, ClientHeight);

  if Pixels.Width / 320 > Pixels.Height / 200 then
  begin
    ScaleM := Pixels.Height;
    ScaleD := 200;
  end else begin
    ScaleM := Pixels.Width;
    ScaleD := 320;
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
var
  Index, x, y: Integer;
  a, b, c: TVertex;
begin
  Inc(Frames);

  Pixels.InitZBuffer;

  // Background color
  for Index := 0 to Pixels.Height - 1 do
  begin
    y := (Index * 200) div Pixels.Height;
    for x := 0 to Pixels.Width - 1 do
      Pixels.Data[Index * Pixels.Width + x] := Pixels.Palette[y];
  end;

  // First Torus
  BaseShade := 1;
  InitTrigo(phix, phiy, phiz);
  Translate(  0, 0, 0, Vertices, Coords, VertexCount, ScaleM, ScaleD);
  DrawWorld;

  // 2nd Torus
  BaseShade := 128;
  InitTrigo(phix + 64, phiy, phiz);
  Translate(160, 0, 0, Vertices, Coords, VertexCount, ScaleM, ScaleD);
  DrawWorld;

  Pixels.Draw(0, 0, Canvas);

  Timer1.Tag := 0;
end;


end.
