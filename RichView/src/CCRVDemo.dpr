program CCRVDemo;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form2},
  Execute.VCL.RichEditor in '..\lib\Execute.VCL.RichEditor.pas',
  Execute.RichDocuments in '..\lib\Execute.RichDocuments.pas',
  Execute.Win.RichRenderer in '..\lib\Execute.Win.RichRenderer.pas',
  Execute.RichRenderers in '..\lib\Execute.RichRenderers.pas',
  Execute.SiblingLists in '..\lib\Execute.SiblingLists.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
