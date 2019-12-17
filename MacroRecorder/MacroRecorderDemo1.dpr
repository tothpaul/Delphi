program MacroRecorderDemo1;

uses
  Vcl.Forms,
  MacroRecorderDemo1.Main in 'MacroRecorderDemo1.Main.pas' {Form1},
  Execute.MacroPlayer in 'Execute.MacroPlayer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
