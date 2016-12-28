program SelfDebug;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  DelphiTDS in '..\lib\DelphiTDS.pas',
  Execute.Trees in '..\lib\Execute.Trees.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
