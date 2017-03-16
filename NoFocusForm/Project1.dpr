program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Execute.TextBoxWKeyboard in 'Execute.TextBoxWKeyboard.pas',
  Execute.HoverKeyboard in 'Execute.HoverKeyboard.pas' {HoverKeyboard};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
