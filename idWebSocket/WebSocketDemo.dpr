program WebSocketDemo;

uses
  Vcl.Forms,
  WebSocketDemo.Main in 'WebSocketDemo.Main.pas' {Form1},
  Execute.idWebSocket in 'Execute.idWebSocket.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
