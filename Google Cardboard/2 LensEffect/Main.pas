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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Types3D,
  System.Math.Vectors, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.MaterialSources, FMX.Objects3D, FMX.Ani, FMX.Controls3D, FMX.Viewport3D,
  FMX.Effects, FMX.Filter.Effects,

  Execute.StereoViewPort;

type
  TForm5 = class(TForm)
    Viewport3D1: TViewport3D;
    Light1: TLight;
    ZFar: TDummy;
    Grid3D1: TGrid3D;
    Cube1: TCube;
    faRotation: TFloatKeyAnimation;
    faTranslation: TFloatKeyAnimation;
    LightMaterialSource1: TLightMaterialSource;
    cbStereo: TCheckBox;
    cbDistortion: TCheckBox;
    Cube2: TCube;
    FloatAnimation1: TFloatAnimation;
    Grid3D2: TGrid3D;
    TrackBar1: TTrackBar;
    procedure cbStereoChange(Sender: TObject);
    procedure cbDistortionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Déclarations privées }
    procedure OnException(Sender: TObject; E: Exception);
  public
    { Déclarations publiques }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

procedure TForm5.cbDistortionChange(Sender: TObject);
begin
  ViewPort3D1.Distortion := cbDistortion.IsChecked;
  Invalidate();
end;

procedure TForm5.cbStereoChange(Sender: TObject);
begin
  ViewPort3D1.Stereo := cbStereo.IsChecked;
  Invalidate();
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  Application.OnException := OnException;
end;

procedure TForm5.OnException(Sender: TObject; E: Exception);
begin
  Application.OnException := nil;
  ShowMessage(e.Message);
  Application.Terminate;
end;

procedure TForm5.TrackBar1Change(Sender: TObject);
begin
  Viewport3D1.Expand := TrackBar1.Value / 100;
end;

end.
