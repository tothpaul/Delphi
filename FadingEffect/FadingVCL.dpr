program FadingVCL;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  UTransition in 'UTransition.pas' {Transition};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TTransition, Transition);
  Application.Run;
end.
