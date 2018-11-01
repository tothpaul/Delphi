unit Execute.IdSSLSChannel;
{
  TLS SChannel for Indy (c)2018 Execute SARL

	2018.11.01 - added Proxy support
}
interface
{-$DEFINE LOG}
uses
{$IFDEF LOG}
  Winapi.Windows,
{$ENDIF}
  System.SysUtils,
  IdGlobal,
  IdSSL,
  IdURI,
  IdCustomTransparentProxy,
  Execute.SChannel;

type
  SChannelError = class(Exception)
    CertStatus: Cardinal;
    constructor Create(AMsg: string; AStatus: Cardinal);
  end;

  TCredentialsEvent = procedure(Sender: TObject) of object;

  TIdSSLIOHandlerSocketSChannel = class(TIdSSLIOHandlerSocketBase)
  private
    FSSL: THandle;
    FOnCredentials: TCredentialsEvent;
    procedure SetCredentials(Value: TCredentialsEvent);
    procedure ConnectSSL;
    procedure CloseSSL;
    function GetTargetHost: string;
  protected
    procedure SetPassThrough(const Value: Boolean); override;
    function RecvEnc(var ABuffer: TIdBytes): Integer; override;
    function SendEnc(const ABuffer: TIdBytes; const AOffset, ALength: Integer): Integer; override;
    procedure ConnectClient; override;
  public
    function Clone:  TIdSSLIOHandlerSocketBase; override;
    procedure StartSSL; override;
    procedure Close; override;
    function Connected: Boolean; override;
    function Readable(AMSec: Integer = IdTimeoutDefault): Boolean; override;
    property OnCredentials: TCredentialsEvent read FOnCredentials write SetCredentials;
  end;

implementation

{ TIdSSLIOHandlerSocketSChannel }

function TIdSSLIOHandlerSocketSChannel.Clone: TIdSSLIOHandlerSocketBase;
begin
{$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.Clone');{$ENDIF}
  Result := TIdSSLIOHandlerSocketSChannel.Create(nil);
end;

procedure TIdSSLIOHandlerSocketSChannel.CloseSSL;
begin
{$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.CloseSSL');{$ENDIF}
  if FSSL <> 0 then
  begin
   {$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.CloseSSL - SSLClose');{$ENDIF}
    SSLClose(FSSL);
    FSSL := 0;
  end;
end;

procedure TIdSSLIOHandlerSocketSChannel.Close;
begin
{$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.Close');{$ENDIF}
  CloseSSL;
  inherited;
end;

procedure TIdSSLIOHandlerSocketSChannel.ConnectClient;
begin
{$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.ConnectClient');{$ENDIF}
//  CloseSSL;
  inherited;
  StartSSL;
end;

function TIdSSLIOHandlerSocketSChannel.Connected: Boolean;
begin
{
  I'm not sure is this is correct, but when Indy check for KeepAlive connexion, it tries to read data and this can lead to a session timeout
}
  if Passthrough then
    Result := inherited Connected
  else
  Result := FSSL <> 0;
 {$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.Connected = ', Result);{$ENDIF}
end;

function TIdSSLIOHandlerSocketSChannel.Readable(AMSec: Integer): Boolean;
begin
{
  Not sure of this code either
}
 {$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.Readable(', AMSec, ')');{$ENDIF}
  if (FSSL <> 0) and (PassThrough = False) then
    Result := True
  else
    Result := inherited Readable(AMSec);
 {$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.Readable(', AMSec, ') = ', Result);{$ENDIF}
end;

function TIdSSLIOHandlerSocketSChannel.RecvEnc(var ABuffer: TIdBytes): Integer;
begin
{$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.RecvEnc(', Length(ABuffer), ')');{$ENDIF}
  Result := SSLRead(FSSL, ABuffer[0], Length(ABuffer));
{$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.RecvEnc(', Length(ABuffer), ') = ', Result);{$ENDIF}
end;

function TIdSSLIOHandlerSocketSChannel.SendEnc(const ABuffer: TIdBytes;
  const AOffset, ALength: Integer): Integer;
var
  Ofs: Integer;
  Len: Integer;
  Cnt: Integer;
begin
{$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.SendEnc(', Length(ABuffer) ,')');{$ENDIF}
  Ofs := AOffset;
  Len := ALength;
  while Len > 0 do
  begin
    Cnt := SSLWrite(FSSL, ABuffer[Ofs], Len);
    if Cnt <= 0 then
    begin
    {$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.SendEnc(', Length(ABuffer) ,') = ', Cnt);{$ENDIF}
      Exit(Cnt);
    end;
    Inc(Ofs, Cnt);
    Dec(Len, Cnt);
  end;
  Result := Ofs - AOffset;
{$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.SendEnc(', Length(ABuffer) ,') = ', Result);{$ENDIF}
end;

procedure DoCredentials(SSL: Integer; UserData: Pointer);
begin
  with TIdSSLIOHandlerSocketSChannel(UserData) do
  begin
    if Assigned(FOnCredentials) then
      FOnCredentials(TIdSSLIOHandlerSocketSChannel(UserData));
  end;
end;

procedure TIdSSLIOHandlerSocketSChannel.SetCredentials(
  Value: TCredentialsEvent);
begin
  FOnCredentials := Value;
  if FSSL <> 0 then
  begin
    if Assigned(FOnCredentials) then
      SSLCredentialsCallBack(FSSL, DoCredentials, Self)
    else
      SSLCredentialsCallBack(FSSL, nil, nil);
  end;
end;

procedure TIdSSLIOHandlerSocketSChannel.SetPassThrough(const Value: Boolean);
begin
{$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.SetPassThrough (', Value,')');{$ENDIF}
  if fPassThrough <> Value then begin
    if not Value then begin
      if BindingAllocated then begin
        ConnectSSL;
      end;
    end;
    fPassThrough := Value;
  end;
end;

procedure TIdSSLIOHandlerSocketSChannel.StartSSL;
begin
  if not PassThrough then
    ConnectSSL;
end;

procedure TIdSSLIOHandlerSocketSChannel.ConnectSSL;
var
  aHost: AnsiString;
begin
  aHost := AnsiString(GetTargetHost);
{$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.ConnectSSL (', aHost,')');{$ENDIF}
  FSSL := SSLStart(Binding.Handle, aHost);
  if FSSL = 0 then
    raise SChannelError.Create('SChannel initialization fails'#13 + SSLError, CertStatus);
  if Assigned(FOnCredentials) then
    SSLCredentialsCallBack(FSSL, DoCredentials, Self);
end;

function TIdSSLIOHandlerSocketSChannel.GetTargetHost: string;
var
  LURI: TIdURI;
  LTransparentProxy, LNextTransparentProxy: TIdCustomTransparentProxy;
begin
  Result := '';

  if URIToCheck <> '' then
  begin
    LURI := TIdURI.Create(URIToCheck);
    try
      Result := LURI.Host;
    finally
      LURI.Free;
    end;
    if Result <> '' then
      Exit;
  end;

  LTransparentProxy := FTransparentProxy;
  if Assigned(LTransparentProxy) then
  begin
    if LTransparentProxy.Enabled then
    begin
      repeat
        LNextTransparentProxy := LTransparentProxy.ChainedProxy;
        if not Assigned(LNextTransparentProxy) then Break;
        if not LNextTransparentProxy.Enabled then Break;
        LTransparentProxy := LNextTransparentProxy;
      until False;
      Result := LTransparentProxy.Host;
      if Result <> '' then
        Exit;
    end;
  end;

  Result := Host;
end;

{ SChannelError }

constructor SChannelError.Create(AMsg: string; AStatus: Cardinal);
begin
  CertStatus := AStatus;
  inherited create(AMsg);
end;

initialization
{$IFDEF LOG}AllocConsole;{$ENDIF}
end.