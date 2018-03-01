program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Execute.IdSSLSChannel in '..\lib\Execute.IdSSLSChannel.pas',
  Execute.SChannel in '..\lib\Execute.SChannel.pas',
  Execute.WinSSPI in '..\lib\Execute.WinSSPI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
