program FMXDemo1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  Execute.Torus in '..\lib\Execute.Torus.pas',
  Execute.Trigo in '..\lib\Execute.Trigo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
