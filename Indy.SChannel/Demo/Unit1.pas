unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IdIOHandler,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, Execute.IdSSLSChannel;

type
  TForm1 = class(TForm)
    IdHTTP1: TIdHTTP;
    Button1: TButton;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    Button2: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
    SChannel: TIdSSLIOHandlerSocketSChannel;
    procedure Load(IOHandler: TIdIOHandler);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Load(IdSSLIOHandlerSocketOpenSSL1);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if SChannel = nil then
    SChannel := TIdSSLIOHandlerSocketSChannel.Create(Self);
  Load(SChannel);
end;

procedure TForm1.Load(IOHandler: TIdIOHandler);
var
  Reply: TMemoryStream;
begin
  IdHTTP1.IOHandler := IOHandler;
  Reply := TMemoryStream.Create;
  try
    IdHTTP1.Get(Edit1.Text, Reply);
    Reply.Position := 0;
    Memo1.Lines.LoadFromStream(Reply);
  finally
    Reply.Free;
  end;
end;

end.
