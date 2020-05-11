unit Execute.idWebSocket;

{
  WebSocket handler for Delphi Tokyo and Indy
  (c)2018 Execute SARL
  http://www.execute.Fr
}

interface

uses
  System.SysUtils,
  System.NetEncoding,
  IdGlobal,
  IdContext,
  IdCustomHTTPServer,
  IdHashSHA;

type
  TWebSocket = class;

  TWebSocketMessage = record
  private
    FData : TidBytes;
    FStart: Integer;
    FSize : Integer;
    procedure GetSize(Len: Integer);
    function DataSize: Int64;
    function Ping: Boolean;
    function Pong: TIdBytes;
    function GetData(var AData: TBytes; var IsText: Boolean): Boolean;
    procedure Consume;
    function Close: Boolean;
    procedure CloseMessage;
  public
    procedure SetText(const Str: string);
    procedure SetData(const Data: TBytes; IsText: Boolean = False);
  end;

  TWebSocketDataEvent = procedure(Socket: TWebSocket; const Data: TBytes; var IsText: Boolean) of object;
  TWebSocketMessageEvent = procedure(Socket: TWebSocket; const Str: string) of object;

  TWebSocket = class
  private
    FContext  : TIdContext;
    FOnData   : TWebSocketDataEvent;
    FOnMessage: TWebSocketMessageEvent;
  public
    constructor Create(const Key: string; AContext: TIdContext; AResponseInfo: TIdHTTPResponseInfo);
    procedure SendMessage(const Str: string);
    procedure ReadLoop();
    procedure Close;
    class function IsWebSocket(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo; out WebSocket: TWebSocket): Boolean;
    property OnData: TWebSocketDataEvent read FOnData write FOnData;
    property OnMessage: TWebSocketMessageEvent read FOnMessage write FOnMessage;
  end;

implementation

procedure TWebSocketMessage.SetText(const Str: string);
begin
  SetData(TEncoding.UTF8.GetBytes(Str), True);
end;

procedure TWebSocketMessage.SetData(const Data: TBytes; IsText: Boolean = False);
var
  len: Int64;
  x,c: Integer;
  i  : Integer;
begin
  len := Length(Data);
  c := 2 + Len;
  if Len > 125 then
  begin
    Inc(c, 2);
    if Len > 65535 then
      Inc(c, 6);
  end;
  SetLength(FData, c);
  if IsText then
    FData[0] := $81  // FIN, TEXT
  else
    FData[0] := $82; // FIN, Binary
  x := 1;
  if Len < 126 then
  begin
    FData[x] := Len; // MASK=0|LEN=??????
    Inc(x);
  end else begin
    if Len < 65565 then
    begin
      FData[x] := 126;
      c := 2;
    end else begin
      FData[x] := 127;
      c := 8;
    end;
    Inc(x, c);
    for i := 0 to c - 1 do
    begin
      FData[x] := Len and $FF;
      Dec(x);
      Len := Len shr 8;
    end;
    Inc(x, c + 1);
    Len := Length(Data);
  end;
  Move(Data[0], FData[x], Len);
end;

procedure TWebSocketMessage.CloseMessage;
begin
  SetLength(FData, 2);
  FData[0] := $88; // FIN + CLOSE
  FData[1] := $00; // Empty message
end;

procedure TWebSocketMessage.GetSize(Len: Integer);
var
  Index: Integer;
begin
  FSize := 0;
  for Index := 0 to Len - 1 do
  begin
    Assert(Length(FData) >= FStart);
    FSize := FSize shl 8 + FData[FStart];
    Inc(FStart);
  end;
end;

function TWebSocketMessage.DataSize: Int64;
begin
  Assert(Length(FData) >= 2);
  FStart := 2; // Head
  FSize := FData[1] and $7F;
  case FSize of
    126:
    begin
      Assert(Length(FData) >= 4);
      GetSize(2);
    end;
    127:
    begin
      Assert(Length(FData) >= 10);
      GetSize(8);
    end;
  end;
  Inc(FStart, 4); // Mask
  Result := FSize;
end;

function TWebSocketMessage.Ping: Boolean;
var
  Index: Integer;
begin
  Assert((FStart > 0) and (Length(FData) >= FStart + FSize));
  if (FData[1] and $80) = 0 then // Not masked
  begin
    FData[0] := $88; // FIN + CLOSE
    Result := False;
  end else begin
    // Unmask
    for Index := 0 to FSize - 1 do
    begin
      FData[FStart + Index] := FData[FStart + Index] xor FData[FStart - 4 + Index mod 4];
    end;
    Result := FData[0] = $89; // FIN + PING
  end;
end;

function TWebSocketMessage.Pong: TIdBytes;
begin
  Assert((FStart > 0) and (Length(FData) >= FStart + FSize));
  SetLength(Result, FStart - 4 + FSize);
  Result[0] := $8A; // Pong
  Move(FData[1], Result[1], FStart - 5); // Size
  Result[1] := Result[1] and $7F;  // not masked
  Move(FData[FStart], Result[FStart - 4], FSize); // Payload
  Consume;
end;

function TWebSocketMessage.GetData(var AData: TBytes; var IsText: Boolean): Boolean;
var
  Index: Integer;
begin
  Assert((FStart > 0) and (Length(FData) >= FStart + FSize));
  if FData[0] = $88 then // FIN + CLOSE
    Result := False
  else begin
    Index := Length(AData);
    SetLength(AData, Index + FSize);
    Move(FData[FStart], AData[Index], FSize);
    Result := FData[0] and $80 > 0; // FIN
    IsText := FData[0] and $7F = 1; // Text
    Consume;
  end;
end;

function TWebSocketMessage.Close: Boolean;
begin
  Result := (Length(FData) > 0) and (FData[0] = $88); // FIN + CLOSE
end;

procedure TWebSocketMessage.Consume;
begin
  Assert((FStart > 0) and (Length(FData) >= FStart + FSize));
  Delete(FData, 0, FStart + FSize);
end;

procedure TWebSocket.Close;
var
  M: TWebSocketMessage;
begin
  M.CloseMessage;
  FContext.Connection.IOHandler.Write(M.FData);
  FContext.Connection.Disconnect;
end;

constructor TWebSocket.Create(const Key: string; AContext: TIdContext; AResponseInfo: TIdHTTPResponseInfo);
var
  Hash: TIdHashSHA1;
  Sign: string;
begin
  FContext := AContext;

  Hash := TIdHashSHA1.Create;
  Sign := TNetEncoding.Base64.EncodeBytesToString(Hash.HashString(Key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));
  Hash.Free;

  AResponseInfo.ResponseNo := 101; // HTTP/1.1 101 Switching Protocols
  AResponseInfo.ResponseText := 'Switching Protocols';

  AResponseInfo.Connection := 'Upgrade'; // fix for Safari (but better solution anyway)
  AResponseInfo.CustomHeaders.AddValue('Upgrade', 'websocket');
  AResponseInfo.CustomHeaders.AddValue('Sec-WebSocket-Accept', Sign);

  AResponseInfo.ContentType := 'application/json'; // dummy

  AResponseInfo.WriteHeader; // send the Header
end;

procedure TWebSocket.SendMessage(const Str: string);
var
  M: TWebSocketMessage;
begin
  M.SetText(Str);
  FContext.Connection.IOHandler.Write(M.FData);
end;

class function TWebSocket.IsWebSocket(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
  out WebSocket: TWebSocket): Boolean;
var
  Key, Ver : string;
begin
  Key := ARequestInfo.RawHeaders.Values['Sec-WebSocket-Key'];
  Ver := ARequestInfo.RawHeaders.Values['Sec-WebSocket-Version'];
  if (Key <> '') and (Ver = '13') then
  begin
    WebSocket := TWebSocket.Create(Key, AContext, AResponseInfo);
    Result := True;
  end else begin
    Result := False;
  end;
end;

procedure TWebSocket.ReadLoop();
var
  M: TWebSocketMessage;
  D: TBytes;
  T: Boolean;
begin
  repeat
    M.FData := nil;
    FContext.Connection.IOHandler.ReadBytes(M.FData, 6, True); // 2 head + 4 mask
    if (M.FData = nil) or (M.FData[0] = $88) then
      Break;
    case M.FData[1] and $7F of
      126: FContext.Connection.IOHandler.ReadBytes(M.FData, 2, True); // word  Size
      127: FContext.Connection.IOHandler.ReadBytes(M.FData, 8, True); // Int64 Size
    end;
    FContext.Connection.IOHandler.ReadBytes(M.FData, M.DataSize, True);
    if M.Ping then
      FContext.Connection.IOHandler.Write(M.Pong)
    else begin
      if M.GetData(D, T) then
      begin
        if Assigned(FOnData) then
          FOnData(Self, D, T);
        if T and Assigned(FOnMessage) then
          FOnMessage(Self, TEncoding.UTF8.GetString(D));
        D := nil;
      end;
    end;
  until M.Close; // FIN + CLOSE
end;

end.
