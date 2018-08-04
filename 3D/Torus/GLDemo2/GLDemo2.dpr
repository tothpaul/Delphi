program GLDemo2;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form5},
  Execute.GLPanel in '..\..\lib\Execute.GLPanel.pas' {GLPanel: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
