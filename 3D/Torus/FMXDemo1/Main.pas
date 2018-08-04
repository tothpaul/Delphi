unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics, 
  FMX.Dialogs, System.Math.Vectors, FMX.Controls3D, FMX.Objects3D,
  FMX.MaterialSources, Execute.Torus, FMX.Ani;

type
  TForm1 = class(TForm3D)
    Mesh1: TMesh;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    FloatAnimation1: TFloatAnimation;
    Mesh2: TMesh;
    LightMaterialSource2: TLightMaterialSource;
    FloatAnimation2: TFloatAnimation;
    FloatAnimation3: TFloatAnimation;
    procedure Form3DCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Form3DCreate(Sender: TObject);
var
  I: Integer;
begin
  Mesh1.Data.VertexBuffer.Length := VertexCount;
  for I := 0 to VertexCount - 1 do
  begin
    Mesh1.Data.VertexBuffer.Vertices[I] := TPoint3D.Create(Vertices[I].x, Vertices[I].y, Vertices[I].z);
    Mesh1.Data.VertexBuffer.Normals[I] := TPoint3D.Create(VNormals[I].x, VNormals[I].y, VNormals[I].z);
  end;

  Mesh1.Data.IndexBuffer.Length := 3 * FaceCount;
  for I := 0 to FaceCount - 1 do
  begin
    Mesh1.Data.IndexBuffer.Indices[3 * i] := Faces[I].a;
    Mesh1.Data.IndexBuffer.Indices[3 * i + 1] := Faces[I].c;
    Mesh1.Data.IndexBuffer.Indices[3 * i + 2] := Faces[I].b;
  end;

//  Mesh1.Data.CalcFaceNormals();

  Mesh2.Data.Assign(Mesh1.Data);
end;

end.
