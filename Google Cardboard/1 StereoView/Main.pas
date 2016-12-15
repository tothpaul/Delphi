unit Main;

{
   Let's have Real 3D with Delphi Berlin

   (c)2016 Paul TOTH, Execute SARL
   http://www.execute.fr

   https://www.barnsten.com/fr/blog/firemonkey.html
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.Types3D, FMX.Objects3D, FMX.Controls3D,
  FMX.MaterialSources, FMX.Viewport3D, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Effects, FMX.Filter.Effects, FMX.Ani, FMX.Layers3D, FMX.Objects,

  Execute.StereoViewPort;

type
  TForm2 = class(TForm)
    Viewport3D1: TViewport3D;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    Grid3D1: TGrid3D;
    ZFar: TDummy;
    cbStereo: TCheckBox;
    Text3D2: TText3D;
    Cube2: TCube;
    Cube3: TCube;
    lbNote: TLabel;
    lbTip: TLabel;
    cbRotation: TCheckBox;
    faRotation: TFloatKeyAnimation;
    cbTranslation: TCheckBox;
    faTranslation: TFloatKeyAnimation;
    procedure cbStereoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure cbRotationChange(Sender: TObject);
    procedure cbTranslationChange(Sender: TObject);
  private
    { Déclarations privées }
    FEyeDistance: Single;
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.cbTranslationChange(Sender: TObject);
begin
  faTranslation.Enabled := cbTranslation.IsChecked;
  Grid3D1.Position.X := 0;
end;

procedure TForm2.cbRotationChange(Sender: TObject);
begin
  faRotation.Enabled := cbRotation.IsChecked;
  Grid3D1.RotationAngle.Z := 0;
end;

procedure TForm2.cbStereoChange(Sender: TObject);
begin
  ViewPort3D1.Stereo := cbStereo.IsChecked;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
{$IFDEF ANDROID}
  cbStereo.Visible := False;
  lbTip.Visible := False;
  lbNote.Visible := False;
  cbRotation.Visible := False;
  cbTranslation.Visible := False;

  Viewport3D1.Align := TAlignLayout.Client;
  Viewport3D1.Stereo := True;
{$ENDIF}
  FEyeDistance := Viewport3D1.EyeDistance;
end;

procedure TForm2.Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var
  Scale: Single;
begin
  if Viewport3D1.Stereo then
  begin
    Scale := (2 * x) / Width;
    Viewport3D1.EyeDistance := FEyeDistance * Scale;
    Text3D2.Text := IntToStr(Round(Scale * 100)) + '%';
  end;

end;

end.
