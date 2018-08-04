unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  Execute.Trigo,
  Execute.Pixels,
  Execute.Object3D;

type
  TForm2 = class(TForm)
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
    procedure DrawGouraud(Obj: TObject3D; Base: Integer);
    procedure DrawFlat(Obj: TObject3D; Base: Integer);
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

var
 Torus   : TTorus;
 Cube    : TCube;
 Cylinder: TCylinder;
 Sphere  : TGSphere;

 Body    : TCylinder;

 Cyl2    : TCylinder;
 Cyl3    : TCylinder;

 Eyes    : TSphere;

procedure TForm2.DrawFlat(Obj: TObject3D; Base: Integer);
var
  i: Integer;
  ShadeA: Integer;
begin
  Obj.Project(ScaleM/ScaleD);
  for i := 0 to Obj.FaceCount - 1 do
   if (Obj.Hidden[i].z < 0) then
     with Obj.Faces[i] do
     begin
       Pixels.Flat(
         Obj.Coords[a],
         Obj.Coords[b],
         Obj.Coords[c],
         Base + Shade(Obj.Hidden[i])
       );
     end;
end;

procedure TForm2.DrawGouraud(Obj: TObject3D; Base: Integer);
var
  i: Integer;
  ShadeA: Integer;
begin
  Obj.Project(ScaleM/ScaleD);
  for i := 0 to Obj.FaceCount - 1 do
   if (Obj.Hidden[i].z < 0) then
     with Obj.Faces[i] do
     begin
       Pixels.Gouraud(
         Obj.Coords[a],
         Obj.Coords[b],
         Obj.Coords[c],
         Base + Shade(Obj.Shades[a]),
         Base + Shade(Obj.Shades[b]),
         Base + Shade(Obj.Shades[c])
       );
     end;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Torus := TTorus.Create(8, 6, 100, 20);
  Torus.ComputeNormals(64);

  Cyl3 := TCylinder.Create(8, 150, 110, 85);
  Cyl3.ComputeNormals(32);

  Cube := TCube.Create(150, 150, 100);
  Cube.ComputeNormals(64);

  Cylinder := TCylinder.Create(6, 100, 40, 10);
  Cylinder.ComputeNormals(64);

  Sphere := TGSphere.Create(10, 100);
  Sphere.ComputeNormals(64);

  Body := TCylinder.Create(6, 50, 50, 50);
  Body.ComputeNormals(64);

  Cyl2 := TCylinder.Create(6, 40, 40, 15);
  Cyl2.ComputeNormals(64);

  Eyes := TSphere.Create(6, 20);
  Eyes.ComputeNormals(10);

  for i:= 1 to 64 do
  begin
    Pixels.Palette[i]:= RGB(80 + Round(i * 1/1.6), 40 + Round(i * 2/1.4), i * 2);
    Pixels.Palette[i + 63] := RGB(40 + Round(i * 2/1.4), 80 + Round(i * 2/1.6), i * 2);
    Pixels.Palette[i + 127] := RGB(i * 2, 40 + Round(i * 2/1.4), 80 + Round(i * 2/1.6));
  end;

  with Light do
  begin
    x:=33;
    y:=-10;
    z:=-10;
  end;

  phix := 74;
  phiy := 32;
  phiz := 0;
end;

procedure TForm2.FormResize(Sender: TObject);
begin
  Pixels.Resize(ClientWidth, ClientHeight);

  if Pixels.Width / 360 > Pixels.Height / 240 then
  begin
    ScaleM := Pixels.Height;
    ScaleD := 240;
  end else begin
    ScaleM := Pixels.Width;
    ScaleD := 360;
  end;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  inc(phix,0);
  inc(phiy,2);
  inc(phiz,0);
  InvalidateRect(Handle, nil, False);
end;

procedure TForm2.FormPaint(Sender: TObject);
var
  x, y, Index: Integer;
begin
  Pixels.InitZBuffer;

  {$POINTERMATH ON}
  for Index := 0 to Pixels.Height - 1 do
  begin
    y := (Index * 240) div Pixels.Height;
    for x := 0 to Pixels.Width - 1 do
      Pixels.Data[Index * Pixels.Width + x] := Pixels.Palette[Byte(63 - (y div 4))];
  end;

  Torus.Translate(0, 0, -55);
  Torus.Rotate(phix+5,phiy,phiz);
  DrawGouraud(Torus, 32);

  Cyl3.Translate(0, 0, -100);
  Cyl3.Rotate(phix+5,phiy,phiz);
  DrawGouraud(Cyl3, 32-2);

  Cube.Translate(0, 0, +170);
  Cube.Rotate(phix-10,phiy,phiz);
  DrawFlat(Cube, 64 + 32);

  Cylinder.Translate(0, 10, 100);
  Cylinder.Rotate(phix+64,phiy,phiz);
  DrawGouraud(Cylinder, 128+32);

  Body.Translate(0, 0, 115);
  Body.Rotate(phix-15,phiy,phiz);
  DrawGouraud(Body, 128+32);

  Sphere.Translate(0, 0, 0);
  Sphere.Rotate(phix,phiy,phiz);
  DrawGouraud(Sphere, 128+32);

  Cyl2.Translate(0, 10, 110);
  Cyl2.Rotate(phix+30,phiy,phiz);
  DrawGouraud(Cyl2, 128+32);

  Eyes.Translate(0, 10, 85);
  Eyes.Rotate(phix+75,phiy+20,phiz);
  DrawGouraud(Eyes, 54);

  Eyes.Translate(0, 10, 85);
  Eyes.Rotate(phix+75,phiy+255-20,phiz);
  DrawGouraud(Eyes, 54);

  Pixels.Draw(0, 0, Canvas);
end;

end.
