program Demo1;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Execute.Trigo in '..\lib\Execute.Trigo.pas',
  Execute.Pixels in '..\lib\Execute.Pixels.pas',
  Execute.Torus in '..\lib\Execute.Torus.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
