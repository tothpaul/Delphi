program ComboEditDemo;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Execute.ComboEdit in 'Execute.ComboEdit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
