program AsciiShapeEditor;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Execute.AsciiShapes in 'Execute.AsciiShapes.pas';

{$R *.res}

begin
  System.ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
