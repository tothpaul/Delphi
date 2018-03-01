unit Execute.IdSSLSChannel;
{
  TLS SChannel for Indy (c)2018 Execute SARL

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
  Execute.SChannel;

type
  TIdSSLIOHandlerSocketSChannel = class(TIdSSLIOHandlerSocketBase)
  private
    FSSL: THandle;
    procedure CloseSSL;
  protected
    function RecvEnc(var ABuffer: TIdBytes): Integer; override;
    function SendEnc(const ABuffer: TIdBytes; const AOffset, ALength: Integer): Integer; override;
    procedure ConnectClient; override;
  public
    function Clone:  TIdSSLIOHandlerSocketBase; override;
    procedure StartSSL; override;
    procedure Close; override;
    function Connected: Boolean; override;
    function Readable(AMSec: Integer = IdTimeoutDefault): Boolean; override;
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
  Result := FSSL <> 0;
 {$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.Connected = ', Result);{$ENDIF}
end;

function TIdSSLIOHandlerSocketSChannel.Readable(AMSec: Integer): Boolean;
begin
{
  Not sure of this code either
}
 {$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.Readable(', AMSec, ')');{$ENDIF}
  if FSSL <> 0 then
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

procedure TIdSSLIOHandlerSocketSChannel.StartSSL;
begin
{$IFDEF LOG}System.WriteLn('TIdSSLIOHandlerSocketSChannel.StartSSL');{$ENDIF}
  FSSL := SSLStart(Binding.Handle, AnsiString(Host));
  if FSSL = 0 then
    raise Exception.Create('SChannel initialization fails'#13 + SSLError);
end;

initialization
{$IFDEF LOG}AllocConsole;{$ENDIF}
end.
