program Demo3;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Execute.Pixels in '..\lib\Execute.Pixels.pas',
  Execute.Trigo in '..\lib\Execute.Trigo.pas',
  Execute.Parallelization in '..\lib\Execute.Parallelization.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
