unit Execute.Trigo;

{
  (c)2017 Execute SARL
  http://www.execute.fr
}

interface

{$N+}

type
  TVertex = record
    x, y, z: Single;
  end;

  TFace = record
    a, b, c: Integer;
  end;

  TCoord = TVertex;

  TCoords = array of TCoord;

  TVector = TCoord;
  TVectors = array of TVector;

var
  phix, phiy, phiz: Byte;

  Light: TVector;


procedure Normals(var Faces: array of TFace;
  var Vertices, FNormals, VNormals: array of TVertex);

procedure InitTrigo(phix, phiy, phiz: Byte);
procedure translate(x, y, z: integer; var src, dst: array of TVertex; Count: Integer; m, d: Integer);
procedure rotate(var src, dst: array of TVertex; Count: Integer);
procedure project(var dst: array of TVertex; Count, m, d: Integer);

implementation

const
  CENTER_Z = 1000;

var
  sintab, costab: array [0 .. 255] of Single;
  sinx, siny, sinz: Single;
  cosx, cosy, cosz: Single;
  rx, ry, rz: Integer;

procedure Normals(var Faces: array of TFace;
  var Vertices, FNormals, VNormals: array of TVertex);
var
  i, j, nf: Integer;
  x1, y1, z1, x2, y2, z2, vl: single;
  nx, ny, nz: single;
begin
  { Face Normals }
  For i := Low(Faces) To High(Faces) do
  begin
    with Faces[i] do
    begin
      x1 := Vertices[b].x - Vertices[a].x;
      y1 := Vertices[b].y - Vertices[a].y;
      z1 := Vertices[b].z - Vertices[a].z;
      x2 := Vertices[c].x - Vertices[a].x;
      y2 := Vertices[c].y - Vertices[a].y;
      z2 := Vertices[c].z - Vertices[a].z;
    end;
    nx := y1 * z2 - y2 * z1;
    ny := z1 * x2 - z2 * x1;
    nz := x1 * y2 - x2 * y1;
    vl := sqrt(nx * nx + ny * ny + nz * nz);
    with FNormals[i] do
    begin
      x := nx / vl;
      y := ny / vl;
      z := nz / vl;
    end;
  end;
  { Vertex Normals }
  for i := Low(Vertices) to High(Vertices) do
  begin
    x1 := 0;
    y1 := 0;
    z1 := 0;
    nf := 0;
    for j := Low(Faces) to High(Faces) do
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
//      x1 := x1 / nf;
//      y1 := y1 / nf;
//      z1 := z1 / nf;
      vl := sqrt(x1 * x1 + y1 * y1 + z1 * z1);
      with VNormals[i] do
      begin
        x := x1 / vl;
        y := y1 / vl;
        z := z1 / vl;
      end;
    end;
  end;
end;

procedure InitTrigo(phix, phiy, phiz: Byte);
begin
  sinx := SinTab[phix];
  cosx := CosTab[phix];
  siny := SinTab[phiy];
  cosy := CosTab[phiy];
  sinz := SinTab[phiz];
  cosz := CosTab[phiz];
end;

procedure translate(x, y, z: integer; var src, dst: array of TVertex; Count: Integer; m, d: Integer);
var
  v1: ^TVertex;
  v2: ^TVertex;
begin
  v1 := @src[0];
  v2 := @dst[0];
  while count > 0 do
  begin
    v2.x := (v1.x + x) * m / d;
    v2.y := (v1.y + y) * m / d;
    v2.z := (v1.z + z) * m / d;
    Inc(v1);
    Inc(v2);
    Dec(Count);
  end;
end;


procedure rotate(var src, dst: array of TVertex; Count: Integer);
var
  v1: ^TVertex;
  v2: ^TVertex;
  x1, y1, z1: Single;
  x2, y2, z2: Single;
begin
  v1 := @src[0];
  v2 := @dst[0];
  while count > 0 do
  begin
    // rotate x : y'=y*cosx-z*sinx / z'=y*sinx+z*cosx
    x1 := v1.x;
    y1 := v1.y * cosx - v1.z * sinx;
    z1 := v1.y * sinx + v1.z * cosx;

    // rotate y : x'=x*cosy+z*siny / z'=z*coxy-x*siny
    x2 := x1 * cosy + z1 * siny;
    y2 := y1;
    z2 := z1 * cosy - x1 * siny;

    // rotate z : x'=x*cosz-y*sinz / y'=x*sinz+y*cosz
    v2.x := x2 * cosz - y2 * sinz;
    v2.y := x2 * sinz + y2 * cosz;
    v2.z := z2;

    Inc(v1);
    Inc(v2);
    Dec(Count);
  end;
end;

procedure Project(var dst: array of TVertex; count, m, d: Integer);
var
  v: ^TVertex;
begin
  v := @dst[0];
  while count > 0 do
  begin
    v.z := (v.z * d) / m + CENTER_Z;
    v.x := 256 * v.x / v.z;
    v.y := 256 * v.y / v.z;
    Inc(v);
    Dec(Count);
  end;
end;

procedure Init;
var
  x: byte;
begin
  for x := 0 to 255 do
  begin
    sintab[x] := sin(x * PI / 128);
    costab[x] := cos(x * PI / 128);
  end;
end;

initialization
  Init();
end.
