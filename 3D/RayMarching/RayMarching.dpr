program RayMarching;

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  Execute.GLPanel in '..\lib\Execute.GLPanel.pas' {GLPanel: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
