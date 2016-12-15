unit Execute.LensEffect;

{*****************************************************}
{                                                     }
{ Copyright(c)2016 Execute SARL <contact@execute.fr>  }
{               All rights reserved                   }
{                                                     }
{*****************************************************}

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  System.RTLConsts,
  FMX.Types3D,
  FMX.Materials;

type
  TLensEffect = class
  private
    FTexture   : TTexture;
    FContext   : TContext3D;
    FDivisions : Integer;
    FVer       : TVertexBuffer;
    FIdx       : TIndexBuffer;
    FMat       : TTextureMaterial;
  public
    constructor Create(Divisions: Integer);
    destructor Destroy; override;
    procedure SetSize(Width, Height: Integer; Scale: Single);
    procedure Render(Texture: TTexture);
    property Context: TContext3D read FContext;
  end;

implementation

const
  PID2 = PI/2;

function Distort(Context: TContext3D; u, v, Scale: Single): TPoint3D;
var
  x, y, r: Single;
  theta  : Single;
begin
// 0..1 => -1 .. +1
  x := 2 * u - 1;
  y := 2 * v - 1;

  r := sqrt(x * x + y * y);

  // make a perfect circle
  if r > 1 then
  begin
    r := 1;
  end;

  theta := arctan2(y, x);

  // https://www.daniweb.com/programming/software-development/threads/327687/opengl-lens-distortion

  // SINE XY
//    x := sin(x * PID2);
//    y := sin(y * PID2);

  // SINE R
  r := sin(PID2 * r);
  x := r * cos(theta);
  y := r * sin(theta);

  // SQUARE XY
//    x := x * x * SIGN(x);
//    y := y * y * SIGN(y);

  // SQUARE R
//    r := r * r;
//    x := r * cos(theta);
//    y := r * sin(theta);

  // ASINE XY
//    x := arcsin(x) / PID2;
//    y := arcsin(y) / PID2;

  // ASINE R
//    r := arcsin(r) / PID2;
//    x := r * cos(theta);
//    y := r * sin(theta);
//
  x := x * (1 + Scale);
  y := y * (1 + Scale);

  x := (x + 1) / 2;
  y := (y + 1) / 2;

  Result.X := x * Context.Width;
  Result.y := y * Context.Height;
  Result.Z := 0;
end;


{ TLensEffect }

constructor TLensEffect.Create(Divisions: Integer);
var
  ix, iy: Integer;
  sx, sy: Integer;
  su, sv: Single;
  v     : Integer;
begin
  FTexture := TTexture.Create;
  FTexture.Style := [TTextureStyle.RenderTarget];
  FMat := TTextureMaterial.Create;

  FDivisions := Divisions;
  { FDivision = 3
     16 Vertice    = (x + 1) * (x + 1)
     18 Triangles  =      x * 2 * x
     +--+--+--+
     |  |  |  |
     +--+--+--|
     |  |  |  |
     +--+--+--|
     |  |  |  |
     +--+--+--|

  }
  FVer := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], (FDivisions + 1) * (FDivisions + 1));
  FIdx := TIndexBuffer.Create(3 * FDivisions * 2 * FDivisions);

  v := 0;
  for ix := 0 to FDivisions  do
  begin
    for iy := 0 to FDivisions do
    begin
      su := ix / FDivisions;
      sv := iy / FDivisions;
      FVer.TexCoord0[v] := TPointF.Create(su, sv);
      Inc(v);
    end;
  end;

  v := 0;
  for ix := 0 to FDivisions - 1 do
    for iy := 0 to FDivisions - 1 do
    begin
      FIdx[v + 0] := ix     + (FDivisions + 1) *  iy;
      FIdx[v + 1] := ix + 1 + (FDivisions + 1) *  iy;
      FIdx[v + 2] := ix     + (FDivisions + 1) * (iy + 1);
      Inc(v, 3);
      FIdx[v + 0] := ix     + (FDivisions + 1) * (iy + 1);
      FIdx[v + 1] := ix + 1 + (FDivisions + 1) *  iy;
      FIdx[v + 2] := ix + 1 + (FDivisions + 1) * (iy + 1);
      Inc(v, 3);
    end;
end;

destructor TLensEffect.Destroy;
begin
  FIdx.Free;
  FVer.Free;
  FMat.Free;
  FContext.Free;
  FTexture.Free;
  inherited;
end;

procedure TLensEffect.SetSize(Width, Height: Integer; Scale: Single);
var
  ix, iy: Integer;
  su, sv: Single;
  v     : Integer;
begin
  FreeAndNil(FContext);

  FTexture.SetSize(Width, Height);
  FContext := TContextManager.CreateFromTexture(FTexture, TMultisample.None, False);

  v := 0;
  for ix := 0 to FDivisions  do
  begin
    for iy := 0 to FDivisions do
    begin
      su := ix / FDivisions;
      sv := iy / FDivisions;
      FVer.Vertices[v] := Distort(FContext, su, sv, Scale);
      Inc(v);
    end;
  end;
end;

procedure TLensEffect.Render(Texture: TTexture);
begin
  if FContext.BeginScene then
  try
    FContext.SetMatrix(TMatrix3D.Identity);
    FContext.SetContextState(TContextState.cs2DScene);
    FContext.SetContextState(TContextState.csZWriteOff);
    FContext.SetContextState(TContextState.csZTestOff);
    FContext.SetContextState(TContextState.csAllFace);
    FContext.SetContextState(TContextState.csAlphaBlendOff);
    FContext.SetContextState(TContextState.csScissorOff);
    FContext.Clear(TAlphaColors.Gray);

    FMat.Texture := Texture;

    FContext.DrawTriangles(FVer, FIdx, FMat, 1);
  finally
    FContext.EndScene;
  end;
end;


end.
