program RichViewFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXMain in 'FMXMain.pas' {Form1},
  Execute.FMX.RichEditor in '..\lib\Execute.FMX.RichEditor.pas',
  Execute.RichDocuments in '..\lib\Execute.RichDocuments.pas',
  Execute.SiblingLists in '..\lib\Execute.SiblingLists.pas',
  Execute.RichRenderers in '..\lib\Execute.RichRenderers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
