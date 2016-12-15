program LensEffect;

uses
  System.StartUpCopy,
  FMX.Forms,
  Execute.LensEffect in '..\lib\Execute.LensEffect.pas',
  Execute.StereoViewPort in '..\lib\Execute.StereoViewPort.pas',
  Main in 'Main.pas' {Form5};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.FormFactor.Orientations := [TFormOrientation.Landscape];
  Application.Run;
end.
