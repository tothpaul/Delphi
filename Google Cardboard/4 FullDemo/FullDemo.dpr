program FullDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  Execute.LensEffect in '..\lib\Execute.LensEffect.pas',
  Execute.StereoViewPort in '..\lib\Execute.StereoViewPort.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
