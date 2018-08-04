unit Execute.Torus;

interface
{
  (c)2017 Execute SARL
  http://www.execute.fr
}

uses
  Execute.Trigo;

const
  H_DIVISIONS = 16;
  V_DIVISIONS = 10;

  OUTER_R = 170;
  INNER_R =  80;

  VertexCount = H_DIVISIONS * V_DIVISIONS;
  FaceCount   = 2 * VertexCount;

var
  Vertices: array [0 .. VertexCount - 1] of TVertex;
  Faces   : array [0 .. FaceCount - 1] of TFace;

  FNormals: array [0 .. FaceCount - 1] of TVertex;
  VNormals: array [0 .. VertexCount - 1] of TVertex;

  Coords : array [0 .. VertexCount - 1] of TVertex;
  Shades : array [0 .. VertexCount - 1] of TVertex;
  Hidden : array [0 .. FaceCount - 1] of TVertex;

implementation

procedure Init;
const
  H_PI = 2 * PI / H_DIVISIONS;
  V_PI = 2 * PI / V_DIVISIONS;
var
  horAngle : Integer;
  vertAngle: Integer;
  Index    : integer;
  i, j, nf : integer;
  x1, y1, z1,
  x2, y2, z2, vl: Single;
  nx, ny, nz    : single;
begin
  { Vertices }
  Index := 0;
  for horAngle := 0 to H_DIVISIONS - 1 do
  begin
    x1 := Cos(horAngle * H_PI) * OUTER_R;
    y1 := Sin(horAngle * H_PI) * OUTER_R;
    for vertAngle := 0 to V_DIVISIONS - 1 do
    begin
      with Vertices[Index] do
      begin
        X := Round(y1 + Cos(vertAngle * V_PI) * Sin(horAngle * H_PI) * INNER_R);
        Y := Round(x1 + Cos(vertAngle * V_PI) * Cos(horAngle * H_PI) * INNER_R);
        Z := Round(Sin(vertAngle * V_PI) * INNER_R);
      end;
      Inc(Index);
    end;
  end;

  { Faces }
  Index := 0;
  for horAngle := 0 to H_DIVISIONS - 1 do
  begin
    for vertAngle := 0 to V_DIVISIONS - 1 do
    begin

      with Faces[Index] do
      begin
        a := horAngle * V_DIVISIONS + vertAngle;
        b := horAngle * V_DIVISIONS + (vertAngle + 1) Mod V_DIVISIONS;
        c := (horAngle * V_DIVISIONS + vertAngle + V_DIVISIONS) Mod VertexCount;
      end;
      Inc(index);

      with Faces[Index] do
      begin
        a := horAngle * V_DIVISIONS + (vertAngle + 1) Mod V_DIVISIONS;
        b := (horAngle * V_DIVISIONS + (vertAngle + 1) Mod V_DIVISIONS + V_DIVISIONS) Mod VertexCount;
        c := (horAngle * V_DIVISIONS + vertAngle + V_DIVISIONS) Mod VertexCount;
      end;
      Inc(index);

    end;
  end;
end;

initialization
  Init();
  Normals(Faces, Vertices, FNormals, VNormals);
end.
