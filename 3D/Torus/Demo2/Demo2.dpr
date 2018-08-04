program Demo2;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form2},
  Execute.Pixels in '..\lib\Execute.Pixels.pas',
  Execute.Trigo in '..\lib\Execute.Trigo.pas',
  Execute.Object3D in '..\lib\Execute.Object3D.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
