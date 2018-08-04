program GLDemo1;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form3},
  Execute.Torus in '..\lib\Execute.Torus.pas',
  Execute.Trigo in '..\lib\Execute.Trigo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
