program StereoView;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form2},
  Execute.StereoViewPort in '..\lib\Execute.StereoViewPort.pas',
  Execute.LensEffect in '..\lib\Execute.LensEffect.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.FormFactor.Orientations := [TFormOrientation.Landscape];
  Application.Run;
end.
