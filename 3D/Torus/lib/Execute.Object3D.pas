unit Execute.Object3D;

{ 3D Objects & ZBuffer

Copyright (C) 1997-98 TothPaul@mygale.org
 http://www.mygale.org/~tothpaul

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

Interface

{$N+}

uses
  Execute.Trigo;

type
 TVector = TVertex;

 TVertices = array of TVertex;
 TFaces    = array of TFace;
 TVectors  = array of TVector;
 TCoords   = array of TCoord;

 TObject3D = class
   VerticeCount: Integer;
   Vertices    : TVertices;
   FaceCount   : Integer;
   Faces       : TFaces;
   FNormals    : TVectors;
   VNormals    : TVectors;
   Coords      : TCoords;
   Shades      : TVectors;
   Hidden      : TVectors;
   MnX,MnY,MnZ : Single;
   MxX,MxY     : Single;
   constructor Create(AVertices, AFaces: Integer);
   procedure ComputeNormals(Normal: Integer);
   procedure Translate(tx,ty,tz: Single);
   procedure Rotate(PhiX,PhiY,PhiZ: Byte);
   procedure Project(Scale: Single);
 end;

 TTorus = class(TObject3D)
   Step1: Integer;
   Step2: Integer;
   Size : Integer;
   constructor Create(AStep1, AStep2, Radius1, Radius2: Integer);
   procedure SetSize(Radius1, Radius2: Integer);
 end;

 TCube = class(TObject3D)
   constructor Create(Width, Height, Depth: Integer);
 end;

 TCylinder = class(TObject3D)
   constructor Create(Step, Height, Radius1, Radius2: Integer);
 end;

 TSphere = class(TObject3D)
   constructor Create(Step, Radius: Integer);
 end;

 TGSphere = class(TObject3D)
   constructor Create(Step, Radius: Integer);
 end;

var
 Light: TVector;

function Shade(const vector: TVector): Integer;
function iRound(i: Single): Integer;

Implementation

{- Trigo ----------------------------------------------------------}
const
  FACTOR = 1 SHL 8;

var
 sinx,siny,sinz: Single;
 cosx,cosy,cosz: Single;

function iRound(i: Single): Integer;
 begin
  if i < -32768 / 2 then Result := -32768 div 2 else
  if i > +32767 / 2 then Result := +32767 div 2 else iRound := Round(i);
 end;

procedure RotateFloat(S, D: Pointer; Count: Integer);
var
  src: ^TVertex absolute S;
  dst: ^TVertex absolute D;
  i  : Integer;
  rx,ry,rz: Single;
begin
  for i:= 0 to Count-1 do
  begin
    with src^ do
    begin
    { rotate x : y'=y*cosx-z*sinx / z'=y*sinx+z*cosx}
     ry :=  y * cosx -  z * sinx;
     rz :=  y * sinx +  z * cosx;
    { rotate y : x'=x*cosy+z*siny / z'=z*cosy-x*siny}
     rx :=  x * cosy + rz * siny;
     rz := rz * cosy -  x * siny;
    end;
    with dst^ do
    begin
   { rotate z : x'=x*cosz-y*sinz / y'=x*sinz+y*cosz }
      x := rx *cosz - ry * sinz;
      y := rx *sinz + ry * cosz;
      z := rz;
    end;
    Inc(src);
    Inc(dst);
  end;
end;


{- Shade ----------------------------------------------------------}

function Shade(const vector: TVector): Integer;
 begin
  Shade:={ABS}(iRound(
             (Light.x*Vector.x)
            +(Light.y*Vector.y)
            +(Light.z*Vector.z)
            ) div 100);
 end;

{- Object3D -------------------------------------------------------}

constructor TObject3D.Create(AVertices,AFaces: Integer);
begin
  VerticeCount := AVertices;
  FaceCount := AFaces;
  SetLength(Vertices, VerticeCount);
  SetLength(Faces, FaceCount);
  SetLength(VNormals, VerticeCount);
  SetLength(FNormals, FaceCount);
  SetLength(Coords, VerticeCount);
  SetLength(Shades, VerticeCount);
  SetLength(Hidden, FaceCount);
end;

procedure TObject3D.ComputeNormals(Normal: Integer);
var
  i,j,nf: Integer;
  x1,y1,z1,x2,y2,z2,vl:single;
  nx,ny,nz:single;
begin

  for i := 0 to FaceCount - 1 do
  begin
    with Faces[i] do
    begin
      x1:=Vertices[b].x - Vertices[a].x;
      y1:=Vertices[b].y - Vertices[a].y;
      z1:=Vertices[b].z - Vertices[a].z;
      x2:=Vertices[c].x - Vertices[a].x;
      y2:=Vertices[c].y - Vertices[a].y;
      z2:=Vertices[c].z - Vertices[a].z;
    end;
    nx := y1 * z2 - y2 * z1;
    ny := z1 * x2 - z2 * x1;
    nz := x1 * y2 - x2 * y1;
    vl := sqrt(nx * nx + ny * ny + nz * nz);
    if vl = 0 then
      vl := Normal
    else
      vl := Normal / vl;
    with FNormals[i] do
    begin
      x := nx * vl;
      y := ny * vl;
      z := nz * vl;
    end;
  end;

  for i := 0 to VerticeCount - 1 do
  begin
    x1 := 0;
    y1 := 0;
    z1 := 0;
    nf := 0;
    for j := 0 to FaceCount - 1 do
    begin
      with Faces[j] do
        if (a = i) or (b = i) or (c = i) then
        begin
          with FNormals[j] do
          begin
            x1 := x1 + x;
            y1 := y1 + y;
            z1 := z1 + z;
          end;
          Inc(nf);
        end;
    end;
    if nf <> 0 then
    begin
      x1 := x1 / nf;
      y1 := y1 / nf;
      z1 := z1 / nf;
      vl := sqrt(x1 * x1 + y1 * y1 + z1 * z1);
      if vl = 0 then
        vl := Normal
      else
        vl := Normal / vl;
      with VNormals[i] do
      begin
        x := x1 * vl;
        y := y1 * vl;
        z := z1 * vl;
      end;
    end;
  end;
end;

procedure TObject3D.Translate(tx,ty,tz: Single);
var
  i: Integer;
begin
  for i := 0 to VerticeCount - 1 do
    with Vertices[i] do
    begin
      Coords[i].x := x + tx;
      Coords[i].y := y + ty;
      Coords[i].z := z + tz;
    end;
end;

procedure TObject3D.Rotate(PhiX,PhiY,PhiZ: Byte);
var
  i: Integer;
begin
  sinx := Sin(PhiX * 2 * PI / 255);
  cosx := Cos(PhiX * 2 * PI / 255);
  siny := Sin(PhiY * 2 * PI / 255);
  cosy := Cos(PhiY * 2 * PI / 255);
  sinz := Sin(PhiZ * 2 * PI / 255);
  cosz := Cos(PhiZ * 2 * PI / 255);
  RotateFloat(Coords, Coords, VerticeCount);
  RotateFloat(VNormals, Shades, VerticeCount);
  RotateFloat(FNormals, Hidden, FaceCount);
end;

procedure TObject3D.Project(Scale: Single);
var
  i: Integer;
  f: Single;
begin
  for i := 0 to VerticeCount - 1 do
  begin
    with Coords[i] do
    begin
      f := z / Scale + 800;
      z := z * 16 + 32000;
      x := +256 * Scale * x / f;
      y := -256 * Scale * y / f;
    end;
  end;
end;


{- Torus -----------------------------------------------------------}

Constructor TTorus.Create(AStep1, AStep2, Radius1, Radius2: Integer);
 begin
  Step1 := AStep1;
  Step2 := AStep2;
  Size  := Step1 * Step2;
  inherited Create(Size, 2 * Size);
  SetSize(Radius1, Radius2);
end;

procedure TTorus.SetSize(Radius1, Radius2: Integer);
var
 i: Integer;
 a1,a2: Single;
 s1,s2: Integer;
 x1,y1: Single;
begin
  a1 := 2 * PI / Step1;
  a2 := 2 * PI / Step2;

  i := 0;
  for s1 := 0 to Step1 - 1 do
  begin
    x1 := cos(s1 * a1) * Radius1;
    y1 := sin(s1 * a1) * Radius1;
    for s2 := 0 to Step2 - 1 do
    begin
      With Vertices[i] do
      begin
        X := y1 + Sin(s1 * a1) * cos(s2 * a2) * Radius2;
        Y := x1 + Cos(s1 * a1) * cos(s2 * a2) * Radius2;
        Z := sin(s2 * a2) * Radius2;
      end;
      Inc(i);
    end;
  end;

  i := 0;
  for s1 := 0 To Step1 - 1 do
    for s2 := 0 To Step2 - 1 do
    begin
      with Faces[i] do
      begin
        a := s1 * Step2 + s2;
        b := s1 * Step2 + (s2 + 1) mod Step2;
        c := (s1 * Step2 + s2 + Step2) mod Size;
      end;
      Inc(i);
      with Faces[i] do
      begin
        a := s1 * Step2 + (s2 + 1) mod Step2;
        b := (s1 * Step2 + (s2 + 1) mod Step2 + Step2) mod Size;
        c := (s1 * Step2 + s2 + Step2) mod Size;
      end;
      Inc(i);
    end;

end;

{- Cube ------------------------------------------------------------}

constructor TCube.Create(Width, Height, Depth: Integer);
const
  size: array[1..3 * 8] of Integer = (
   -1,-1,+1,
   +1,-1,+1,
   -1,+1,+1,
   +1,+1,+1,
   -1,-1,-1,
   +1,-1,-1,
   -1,+1,-1,
   +1,+1,-1
  );
  edge: array[1..3 * 12] of Byte =(
   0,2,1, 1,2,3,
   4,5,6, 5,7,6,
   0,1,4, 4,1,5,
   6,7,2, 2,7,3,
   0,4,6, 6,2,0,
   7,5,1, 7,1,3
  );
var
  i: Integer;
begin
  inherited Create(8, 12);
  Width := Width  div 2;
  Height:= Height div 2;
  Depth := Depth  div 2;
  for i := 0 to VerticeCount-1 do
    with Vertices[i] do
    begin
      x := size[3 * i + 2] * Width;
      y := size[3 * i + 1] * Height;
      z := size[3 * i + 3] * Depth;
    end;
  for i := 0 to FaceCount - 1 do
    with Faces[i] do
    begin
      a := edge[3 * i + 1];
      b := edge[3 * i + 2];
      c := edge[3 * i + 3];
    end;
 end;

{- Cylinder -------------------------------------------------------}
constructor TCylinder.Create(Step,Height,Radius1,Radius2: Integer);
var
  size: Integer;
  a: Single;
  s: Integer;
  i: Integer;
begin
  size := 2 * step;
  Height := Height div 2;
  inherited Create(Size + 2, 2 * Size);

  with Vertices[size] do begin x:=0; y:=0; z:=-Height end;
  with Vertices[size+1] do begin x:=0; y:=0; z:=+Height end;

  a := 2 * PI / Step;
  i := 0;
  for s := 0 to Step - 1 do
  begin
    with Vertices[i] do
    begin
      X := Sin(s * a) * Radius1;
      Y := Cos(s * a) * Radius1;
      Z := -Height;
    end;
    Inc(i);
    with Vertices[i] do
    begin
      X := Sin(s * a) * Radius2;
      Y := Cos(s * a) * Radius2;
      Z := +Height;
    end;
    Inc(i);
  end;

  i := 0;
  for s := 0 to Step - 1 do
  begin
    with Faces[i] do
    begin
      a := size;
      b := (2 * s + 0) mod size;
      c := (2 * s + 2) mod size;
    end;
    Inc(i);
    with Faces[i] do
    begin
      a := size + 1;
      b := (2 * s + 3) mod size;
      c := (2 * s + 1) mod size;
    end;
    Inc(i);
  end;

  for s := 0 to Step - 1 do
  begin
    with Faces[i] do
    begin
      a := (2 * s + 0) mod size;
      b := (2 * s + 1) mod size;
      c := (2 * s + 2) mod size;
    end;
    Inc(i);
    with Faces[i] do
    begin
      a := (2 * s + 3) mod size;
      b := (2 * s + 2) mod size;
      c := (2 * s + 1) mod size;
    end;
    Inc(i);
  end;
end;

{- Sphere ---------------------------------------------------------}
constructor TSphere.Create(Step, Radius: Integer);
var
  Size: Integer;
  a1  : Single;
  s1  : Integer;
  s2  : Integer;
  i   : Integer;
  z2  : Single;
begin
  a1 := PI / Step;
  Size := Step * Step + Step;

  inherited Create(Size, 2 * Size);

  i := 0;
  for s1 := 0 to Step do
  begin
    z2 := cos(s1 * a1) * Radius;
    for s2 := 0 to Step - 1 do
    begin
      with Vertices[i] do
      begin
        x := sin(s2 * 2 * a1) * sin(s1 * a1) * Radius;
        y := cos(s2 * 2 * a1) * sin(s1 * a1) * Radius;
        z := z2;
      end;
      Inc(i);
    end;
  end;

  i := 0;
  for s1 := 0 to Step do
    for s2 := 0 to Step - 1 do
    begin
      with Faces[i] do
      begin
        a := s1 * Step + s2;
        b := s1 * Step + (s2 + 1) mod Step;
        c :=(s1 * Step + s2 + Step) mod Size;
      end;
      Inc(i);
      with Faces[i] do
      begin
        a := s1 * Step + (s2 + 1) mod Step;
        b :=(s1 * Step + (s2 + 1) mod Step + Step) mod Size;
        c :=(s1 * Step + s2 + Step) mod Size;
      end;
      Inc(i);
    end;
 end;

{- GSphere ---------------------------------------------------------}
constructor TGSphere.Create(Step,Radius: Integer);
var
  Size: Integer;
  a1: Single;
  s1: Integer;
  s2: Integer;
   i: Integer;
  z2: Single;
  da: Single;
  ds: Integer;
begin
  a1 := PI / Step;
  Size := Step * Step + Step;

  inherited Create(Size, 2 * Size);

  i := 0;
  for s1 := 0 to Step do
  begin
    z2 := cos(s1 * a1) * Radius;
    if odd(s1) then da := +a1 else da := 0;
    for s2 := 0 to Step - 1 do
    begin
      with Vertices[i] do
      begin
        x := sin(s2 * 2 * a1 + da) * sin(s1 * a1) * Radius;
        y := cos(s2 * 2 * a1 + da) * sin(s1 * a1) * Radius;
        z := z2;
      end;
      Inc(i);
    end;
  end;

  i := 0;
  for s1 := 0 to Step do
  begin
    if odd(s1) then ds := +1 else ds := 0;
    for s2 := 0 to Step - 1 do
    begin
      with Faces[i] do
      begin
        a := s1 * Step + s2;
        b := s1 * Step + (s2 + 1) mod Step;
        c :=(s1 * Step + (s2 + ds) mod Step + Step) mod Size;
      end;
      Inc(i);
      with Faces[i] do
      begin
        a := s1 * Step + (s2 + 1) mod Step;
        b :=(s1 * Step + (s2 + 1 + ds) mod Step + Step) mod Size;
        c :=(s1 * Step + (s2 + ds) mod Step + Step) mod Size;
      end;
     Inc(i);
    end;
  end;
end;

end.
