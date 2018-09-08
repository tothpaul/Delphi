unit WebSocketDemo.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ShellAPI,

  System.SysUtils, System.Variants, System.Classes, System.NetEncoding,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  IdContext, IdCustomHTTPServer, IdHeaderList,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdHTTPServer,

  Execute.idWebSocket;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    IdHTTPServer1: TIdHTTPServer;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
    WebSocket: TWebSocket;
    procedure SendMessage(const Str: string);
    procedure OnWebSocketMessage(Sender: TWebSocket; const Str: string);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  SendMessage(Edit1.Text);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://127.0.0.1', nil, nil, SW_SHOW);
end;

procedure TForm1.IdHTTPServer1CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin

  if TWebSocket.IsWebSocket(AContext, ARequestInfo, AResponseInfo, WebSocket) then
  begin

    // Warning ! the OnMessage event is not raised in the Main thread !
    WebSocket.OnMessage := OnWebSocketMessage;
    // Send some texte (it can be JSON also)
    TThread.Synchronize(nil, procedure
    begin
      SendMessage('Welcome !');
    end
    );
    // this function does not return until the session is closed
    WebSocket.ReadLoop();
    // we can now destroy the component
    FreeAndNil(WebSocket);

  end else begin

    AResponseInfo.ContentType := 'text/html';
    AResponseInfo.ContentText :=
      '<!DOCTYPE HTML>'#10
    + '<html>'#10
    + '<head>'#10
    + '<title>WebSocket demo (c)2018 Execute SARL</title>'#10
    + '<script type="text/javascript">'#10
    + 'var ws;'#10
    + 'function log(s) {'#10
    + ' document.getElementById("log").innerHTML += s + "\n";'#10
    + '}'#10
    + 'function sendMessage(s) {'#10
    + '  log(">> " + s);'#10
    + '  ws.send(s);'#10
    + '}'#10
    + 'function send() {'#10
    + '  sendMessage(document.getElementById("text").value);'#10
    + '}'#10
    + 'function init() {'#10
    + ' ws = new WebSocket("ws://127.0.0.1");'#10
    + ' ws.onopen = function() {'#10
    + '   log("WebSocket opened");'#10
    + '   sendMessage("Hello ?");'#10
    + ' }'#10
    + ' ws.onclose = function() {'#10
    + '   log("WebSocket closed");'#10
    + ' }'#10
    + ' ws.onmessage = function(e) {'#10
    + '   log("<< " + e.data);'#10
    + ' }'#10
    + '}'#10
    + '</script>'#10
    + '</head>'#10
    + '<body onLoad="init()">'#10
    + '<input type="text" id="text"> <button onclick="send()">Send</button>'#10
    + '<pre id="log"></pre>'#10
    + '</body>'#10
    + '</html>';

  end;
end;

procedure TForm1.OnWebSocketMessage(Sender: TWebSocket; const Str: string);
begin
  TThread.Synchronize(nil, procedure
  begin
    Memo1.Lines.Add('<< ' + Str);
  end);
end;

procedure TForm1.SendMessage(const Str: string);
begin
// the WebSocket should be in a threadsafe list of connected clients !
  if WebSocket <> nil then
  begin
    Memo1.Lines.Add('>> ' + Str);
    WebSocket.SendMessage(Str);
  end;
end;

end.
