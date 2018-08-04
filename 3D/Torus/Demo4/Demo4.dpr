program Demo4;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form4},
  Execute.Pixels in '..\lib\Execute.Pixels.pas',
  Execute.Trigo in '..\lib\Execute.Trigo.pas',
  Execute.Parallelization in '..\lib\Execute.Parallelization.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
