# SChannel TLS support for Indy

With this code you can replace the OpenSSL dependend TIdSSLIOHandlerSocketOpenSSL component with a SChannel ([Secure Channel](https://msdn.microsoft.com/fr-fr/library/windows/desktop/aa380123(v=vs.85).aspx)) based component.

```Pascal
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
```
