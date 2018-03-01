unit Execute.SChannel;
{
   SChannel for Delphi Tokyo (c)2018 Execute SARL
}
interface
{$DEFINE LOG}
{$POINTERMATH ON}
uses
  Winapi.Windows,
  Winapi.Winsock,
  System.Types,
  Execute.WinSSPI,
  System.Classes,
  System.SysUtils;

type
{ Create an descendant of TSSLValidator to add a Validation process }
  TSSLValidator = class
  public
    constructor Create;
    destructor Destroy; override;
    function ValidateChain(Chain: PCCERT_CHAIN_CONTEXT; var Status: CERT_CHAIN_POLICY_STATUS): Boolean; virtual;
    function ValidateElement(Element: PCERT_CHAIN_ELEMENT): Boolean; virtual;
  end;

{ Is SChannel available }
function SSLAvailable: Boolean;

{ Start a TLS connexion over a socket }
function SSLStart(Socket: Integer; const Host: AnsiString = ''): Integer;

{ some data left ? }
function SSLPending(SSL: Integer): Boolean;
{ Read from the SSL handle }
function SSLRead(SSL: Integer; var Data; Size: Integer): Integer;
{ Write to the SSL handle }
function SSLWrite(SSL: Integer; var Data; Size: Integer): Integer;
{ Close the SSL connexion }
function SSLClose(SSL: Integer): Integer;

{ If you wonder why the code is built like this, I have an Execute.OpenSSL unit that offers the same functions for OpenSSL }

var
  SSLError: string;

implementation

const
  IO_BUFFER_SIZE  = $10000;

var
  Validators : TList;
  Initialized: Boolean;
  SSPI: PSecurityFunctionTable;
  MyStore: HCERTSTORE;

procedure FreeValidators;
var
  Index: Integer;
begin
  if Validators = nil then
    Exit;
  for Index := Validators.Count - 1 downto 0 do
  begin
    TSSLValidator(Validators[Index]).Free;
  end;
  Validators.Free;
end;

function Validate(Chain: PCCERT_CHAIN_CONTEXT; var Status: CERT_CHAIN_POLICY_STATUS): Boolean;
var
  Index: Integer;
begin
  Result := True;
  if Validators <> nil then
    for Index := Validators.Count - 1 downto 0 do
    begin
      if TSSLValidator(Validators[Index]).ValidateChain(Chain, Status) then
        Exit;
    end;
  Result := False;
end;

{ TSSLValidator }

constructor TSSLValidator.Create;
begin
  inherited;
  if Validators = nil then
    Validators := TList.Create;
  Validators.Add(Self);
end;

destructor TSSLValidator.Destroy;
begin
  Validators.Remove(Self);
  inherited;
end;

function TSSLValidator.ValidateChain(Chain: PCCERT_CHAIN_CONTEXT;
  var Status: CERT_CHAIN_POLICY_STATUS): Boolean;
begin
  Result := False;
  if Status.dwError <> CERT_E_UNTRUSTEDROOT then
    Exit;
  if (Status.lChainIndex < 0) or (DWORD(Status.lChainIndex) > Chain.cChain) then
    Exit;
  if (Status.lElementIndex < 0) or (DWORD(Status.lElementIndex) > Chain.rgpChain[Status.lChainIndex].cElement) then
    Exit;
  Result := ValidateElement(Chain.rgpChain[Status.lChainIndex].rgpElement[Status.lElementIndex]);
end;

function TSSLValidator.ValidateElement(
  Element: PCERT_CHAIN_ELEMENT): Boolean;
begin
  Result := False;
end;

function SendData(Socket: Integer; Data: PByte; Size: Integer): Integer;
var
  Count: Integer;
begin
  Result := 0;
  while Size > 0 do
  begin
    Count := send(Socket, Data[Result], Size, 0);
    if Count <= 0 then
    begin
      SSLError := 'send returns ' + IntToStr(WSAGetLastError);
      {$IFDEF LOG}WriteLn(SSLError);{$ENDIF}
      Exit(Count);
    end;
    Inc(Result, Count);
    Dec(Size, Result);
  end;
end;

function SendSecBuffer(Socket: Integer; var Buffer: TSecBuffer): Boolean;
begin
  Result := True;
  if (Buffer.cbBuffer > 0) and (Buffer.pvBuffer <> nil) then
  begin
    if SendData(Socket, PByte(buffer.pvBuffer), Buffer.cbBuffer) <= 0 then
      Exit(False);
    SSPI.FreeContextBuffer(Buffer.pvBuffer);
    Buffer.pvBuffer := nil;
  end;
end;

type
  TSSLInit = set of (iCredentials, iContext);

  TSSLInfo = record
    Init       : TSSLInit;
    Error      : Cardinal;
  // Connected socket
    Socket     : Integer;
  // Remote server name
    Servername : string;
  // SChanel credentials
    SChannel   : SCHANNEL_CRED;
  // Credentials
    Credentials: TCredHandle;
  // TLS Context
    Context    : TCtxtHandle;
  // Data received from the socket (crypted)
    RecvBuffer : array of AnsiChar;
    RecvCount  : Integer;
  // Data decrypted
    DataBuffer : array of AnsiChar;
    DataCount  : Integer;
    DataStart  : Integer;
  // Output buffer
    BuffSizes  : TSecPkgContextStreamSizes;
    SendBuffer : array of Byte;
    function Start: Boolean;
    procedure Clean;
    function Read: Integer;
    function ReadLoop: Integer;
    function InitBuffer: Boolean;
    function GetClientCredentials: Boolean;
    function VerifyServer: Boolean;
    function Readable: Integer;
    function Decrypt(var Data; Size: Integer): Integer;
    function Encrypt(var Data; Size: Integer): Integer;
  end;
  PSSLInfo = ^TSSLInfo;

function TSSLInfo.Start;
var
  WHost: string;
  OutBuffers: array[0..0] of TSecBuffer;
  OutBuffer : TSecBufferDesc;
  Flags: DWORD;
  Error: Cardinal;
begin
  Result := False;

  FillChar(SChannel, SizeOf(SChannel), 0);
  SChannel.dwVersion := SCHANNEL_CRED_VERSION;
  SChannel.grbitEnabledProtocols := SP_PROT_SSL3TLS1;//SP_PROT_TLS1;
  SChannel.dwFlags := SCH_CRED_NO_DEFAULT_CREDS or SCH_CRED_MANUAL_CRED_VALIDATION;

  Error := SSPI.AcquireCredentialsHandle(
    nil,
    UNISP_NAME,
    SECPKG_CRED_OUTBOUND,
    nil,
   @SChannel,
    nil,
    nil,
   @Credentials,
   nil
  );

  if Error <> SEC_E_OK then
  begin
    SSLError := 'AcquireCredentialsHandle returns ' + IntToHex(Error, 8);
    Exit;
  end;
  Init := [iCredentials];

  //  Initiate a ClientHello message and generate a token.

  OutBuffer.ulVersion := SECBUFFER_VERSION;
  OutBuffer.cBuffers := 1;
  OutBuffer.pBuffers := Addr(OutBuffers[0]);

  OutBuffers[0].cbBuffer := 0;
  OutBuffers[0].BufferType := SECBUFFER_TOKEN;
  OutBuffers[0].pvBuffer := nil;

  WHost := Servername;

  // 'InitializeSecurityContext returns 80090318'
  Error := SSPI.InitializeSecurityContext(
   @Credentials,
    nil,   // nil on first call, to create the Context
    PChar(WHost),
    ISC_REQ_SEQUENCE_DETECT or ISC_REQ_REPLAY_DETECT or ISC_REQ_CONFIDENTIALITY
    or ISC_RET_EXTENDED_ERROR or ISC_REQ_ALLOCATE_MEMORY or ISC_REQ_STREAM,
    0,
    SECURITY_NATIVE_DREP,
    nil,
    0,
   @Context,
   @OutBuffer,
    Flags,
    nil
  );
  if (Error <> SEC_I_CONTINUE_NEEDED) then
  begin
    SSLError := 'First call to InitializeSecurityContext returns ' + IntToHex(Error, 8);
    Exit;
  end;
  Init := Init + [iContext];

  // Send response to server if there is one.
  if not SendSecBuffer(Socket, OutBuffers[0]) then
  begin
    Exit;
  end;

  // Loop until the handshake is finished or an error occurs.
  SetLength(RecvBuffer, IO_BUFFER_SIZE);
  RecvCount := 0;
  {$IFDEF LOG}WriteLn('[SSL] ReadLoop');{$ENDIF}
  if ReadLoop < 0 then
    Exit;

  if VerifyServer = False then
    Exit;

  if InitBuffer = False then
    Exit;

  Result := True;
end;

procedure TSSLInfo.Clean;
begin
  if iCredentials in Init then
  begin
    SSPI.FreeCredentialsHandle(@Credentials);
  end;
  if iContext in Init then
  begin
    SSPI.DeleteSecurityContext(@Context);
  end;
  Init := [];
end;

function TSSLInfo.Read: Integer;
begin
  Result := recv(Socket, RecvBuffer[RecvCount], Length(RecvBuffer) - RecvCount, 0);
  if Result > 0 then
    Inc(RecvCount, Result)
  else  begin
    Error := WSAGetLastError;
    SSLError := 'recv returns ' + IntToHex(Error, 8);
  end;
end;

function TSSLInfo.Readable: Integer;
var
  Buffers: array[0..3] of TSecBuffer;
  Buffer : TSecBufferDesc;
  Count  : Integer;
  Index  : Integer;
begin
  Result := DataCount - DataStart;

  if Result = 0 then
  begin
    DataCount := 0;
    DataStart := 0;

    repeat

      // Next Message
      Error := 0;
      repeat

        if (RecvCount = 0) or (Error = SEC_E_INCOMPLETE_MESSAGE) then
        begin
          Result := Read();
          if Result <= 0 then
            Exit;
        end;

        FillChar(Buffers, SizeOf(Buffers), 0);
        Buffers[0].cbBuffer := RecvCount;
        Buffers[0].BufferType := SECBUFFER_DATA;
        Buffers[0].pvBuffer := @RecvBuffer[0];

  //      Buffers[1].BufferType := SECBUFFER_EMPTY;  FillChar() did that already
  //      Buffers[2].BufferType := SECBUFFER_EMPTY;
  //      Buffers[3].BufferType := SECBUFFER_EMPTY;

        Buffer.ulVersion := SECBUFFER_VERSION;
        Buffer.cBuffers := 4;
        Buffer.pBuffers := Addr(Buffers[0]);

        // The encrypted message is decrypted in place, overwriting the original contents of its buffer.
        Error := SSPI.DecryptMessage(@Context, @Buffer, 0, nil);
      {$IFDEF LOG}WriteLn('SSPI.DecryptMessage = ', Error, ' / 0x', IntToHex(Error, 8));{$ENDIF}

      until Error <> SEC_E_INCOMPLETE_MESSAGE;

      // Session expires
      if Error = SEC_I_CONTEXT_EXPIRED then
      begin
        SSLError := 'DecryptMessage returns SEC_I_CONTEXT_EXPIRED';
      {$IFDEF LOG}WriteLn(SSLError);{$ENDIF}
        Exit(-1);
      end;

      RecvCount := 0;
      for Index := 1 to 3 do
      begin
        case Buffers[Index].BufferType of
          // Decrypted data
          SECBUFFER_DATA:
          begin
            if DataCount + Buffers[Index].cbBuffer > Length(DataBuffer) then
            begin
              SetLength(DataBuffer, DataCount + Buffers[Index].cbBuffer);
            end;
           {$IFDEF LOG}WriteLn('Decrypt ',Buffers[Index].cbBuffer,' bytes ');{$ENDIF}
            Move(Buffers[Index].pvBuffer^, DataBuffer[DataCount], Buffers[Index].cbBuffer);
            Inc(DataCount, Buffers[Index].cbBuffer);
          end;
          // Extra data
          SECBUFFER_EXTRA:
          begin
            Assert(Buffers[Index].cbBuffer <= Length(RecvBuffer));
            RecvCount := Buffers[Index].cbBuffer;
            Move(Buffers[Index].pvBuffer^, RecvBuffer[0], RecvCount);
          end;
        end;
      end;

      if Error = SEC_I_RENEGOTIATE then
      begin
      {$IFDEF LOG}WriteLn('SEC_I_RENEGOTIATE');{$ENDIF}
        if ReadLoop <= 0 then
          Exit(-1);
      end;

      if Error <> SEC_E_OK then
        Exit(-1);

    until DataCount > 0;

    Result := DataCount; // DataStart = 0
  end;
end;

function TSSLInfo.ReadLoop: Integer;
var
  InBuffers : array[0..1] of TSecBuffer;
  InBuffer  : TSecBufferDesc;
  OutBuffers: array[0..0] of TSecBuffer;
  OutBuffer : TSecBufferDesc;
  DoRead    : Boolean;
  Flags     : DWORD;
  Error     : Cardinal;
  Source    : PByte;
begin
  // input data
  InBuffer.ulVersion := SECBUFFER_VERSION;
  InBuffer.cBuffers := 2;
  inBuffer.pBuffers := Addr(InBuffers[0]);

  // output data
  OutBuffer.ulVersion := SECBUFFER_VERSION;
  OutBuffer.cBuffers := 1;
  OutBuffer.pBuffers := Addr(OutBuffers[0]);

  DoRead := True;
  repeat
    if DoRead then
    begin
      DoRead := False;
      {$IFDEF LOG}WriteLn('[SSL] ReadLoop.Read');{$ENDIF}
      Result := Read;
      if Result <= 0 then
        Exit;
    end;

    // available input data
    InBuffers[0].cbBuffer := RecvCount;
    InBuffers[0].BufferType := SECBUFFER_TOKEN;
    InBuffers[0].pvBuffer := @RecvBuffer[0];

    // used when there's extra data in the input buffer
    InBuffers[1].cbBuffer := 0;
    InBuffers[1].BufferType := SECBUFFER_EMPTY;
    InBuffers[1].pvBuffer := nil;

    // output data
    OutBuffers[0].cbBuffer := 0;
    OutBuffers[0].BufferType := SECBUFFER_TOKEN;
    OutBuffers[0].pvBuffer := nil;

  {$IFDEF LOG}WriteLn('[SSL] ReadLoop.InitializeSecurityContext, InputSize = ', RecvCount);{$ENDIF}
    Error := SSPI.InitializeSecurityContext(
     @Credentials,
     @Context,
      nil,
      ISC_REQ_SEQUENCE_DETECT or ISC_REQ_REPLAY_DETECT or ISC_REQ_CONFIDENTIALITY
      or ISC_RET_EXTENDED_ERROR or ISC_REQ_ALLOCATE_MEMORY or ISC_REQ_STREAM,
      0,
      SECURITY_NATIVE_DREP,
     @InBuffer,
      0,
      nil,
     @OutBuffer,
      Flags,
      nil
    );

    {$IFDEF LOG}
    WriteLn('Error = ', Error, ' / 0x', IntToHex(Error, 8), ', Output = ', OutBuffers[0].cbBuffer, ', InputLeft = ', InBuffers[1].cbBuffer);
    {$ENDIF}

    // envoyer la sortie
    if not SendSecBuffer(Socket, OutBuffers[0]) then
    begin
      Exit(-1);
    end;

    // Extra data
    if (InBuffers[1].cbBuffer > 0) and (InBuffers[1].BufferType = SECBUFFER_EXTRA) then
    begin
     {$IFDEF LOG}WriteLn('[SSL] ReadLoop.SECBUFFER_EXTRA');{$ENDIF}
      Source := @RecvBuffer[RecvCount - InBuffers[1].cbBuffer];
      RecvCount := InBuffers[1].cbBuffer;
     {$IFDEF LOG}WriteLn('[SSL] ReadLoop.Move(InBuffer, RecvBufer, ', RecvCount, ')');{$ENDIF}
      Move(Source^, RecvBuffer[0], RecvCount);
    end else begin
     {$IFDEF LOG}WriteLn('[SSL] ReadLoop.0');{$ENDIF}
      if Error <> SEC_E_INCOMPLETE_MESSAGE then  // keep the message !
        RecvCount := 0;
      //DoRead := True; // required ?
    end;

    if (Error = SEC_E_INCOMPLETE_MESSAGE) or (Error = SEC_I_CONTINUE_NEEDED) or (Error = SEC_E_INCOMPLETE_MESSAGE) then
    begin
      DoRead := True;
      Continue;
    end;

    if Error = SEC_I_INCOMPLETE_CREDENTIALS then
    begin
     {$IFDEF LOG}WriteLn('[SSL] ReadLoop.SEC_I_INCOMPLETE_CREDENTIALS');{$ENDIF}
      GetClientCredentials;
      Assert(DoRead = False);//DoRead := False; // do not read more, we have to send data first
      Continue;
    end;

    // Check for fatal error.
    if FAILED(Error) then
    begin
     {$IFDEF LOG}WriteLn('[SSL] ReadLoop.FAILED');{$ENDIF}
      SSLError := 'InitializeSecurityContext returns ' + IntToHex(Error, 8);
      Exit(-1);
    end;


  until Error = SEC_E_OK;
  {$IFDEF LOG}WriteLn('[SSL] ReadLoop.OK');{$ENDIF}
end;

function TSSLInfo.GetClientCredentials: Boolean;
var
  Error: Integer;
  Issuer: TSecPkgContextIssuerListInfoEx;
  ChainPara: CERT_CHAIN_FIND_BY_ISSUER_PARA;
  ChainCtxt: PCCERT_CHAIN_CONTEXT;
  ChainIndex: DWORD;
  ElementIndex: DWORD;
  pCertContext: PCCERT_CONTEXT;
  Creds: TCredHandle;
begin
{$IFDEF LOG}WriteLn('[SSL] GetClientCredentials');{$ENDIF}
  FillChar(Issuer, SizeOf(Issuer), 0);
  Error := SSPI.QueryContextAttributes(@Context, SECPKG_ATTR_ISSUER_LIST_EX, @Issuer);
  if Error <> SEC_E_OK then
  begin
    SSLError := 'QueryContextAttributes(SECPKG_ATTR_ISSUER_LIST_EX) returns ' + IntToHex(Error, 8);
    Exit(False);
  end;

  FillChar(ChainPara, SizeOf(ChainPara), 0);
  ChainPara.cbSize := SizeOf(ChainPara);
  ChainPara.pszUsageIdentifier := szOID_PKIX_KP_CLIENT_AUTH;
  ChainPara.cIssuer := Issuer.cIssuers;
  ChainPara.rgIssuer := Issuer.aIssuers;
  ChainPara.pdwIssuerChainIndex := @ChainIndex;
  ChainPara.pdwIssuerElementIndex := @ElementIndex;
  ChainIndex := 0;
  ElementIndex := 0;

  ChainCtxt := nil;
  repeat
    ChainCtxt := CertFindChainInStore(
      MyStore,
      X509_ASN_ENCODING,
      0,
      CERT_CHAIN_FIND_BY_ISSUER,
     @ChainPara,
      ChainCtxt
    );
    if ChainCtxt = nil then
    begin
      SSLError := 'CertFindChainInStore returns nil';
      Exit(False);
    end;

    pCertContext := ChainCtxt.rgpChain[0].rgpElement[0].pCertContext;
    SChannel.dwVersion := SCHANNEL_CRED_VERSION;
    SChannel.cCreds := 1;
    SChannel.paCred := @pCertContext;

    Error := SSPI.AcquireCredentialsHandle(
      nil,
      UNISP_NAME,
      SECPKG_CRED_OUTBOUND,
      nil,
     @SChannel,
      nil,
      nil,
     @Creds,
      nil
    );
  until Error = SEC_E_OK;
{$IFDEF LOG}WriteLn('[SSL] GetClientCredentials.NewCredentiels');{$ENDIF}
  SSPI.FreeCredentialsHandle(@Credentials);
  Credentials := Creds;
end;

function TSSLInfo.VerifyServer: Boolean;
const
  USAGES: array[0..2] of PAnsiChar = (
    szOID_PKIX_KP_SERVER_AUTH,
    szOID_SERVER_GATED_CRYPTO,
    szOID_SGC_NETSCAPE
  );
var
  Error : Integer;
  Server: PCCERT_CONTEXT;
  ChainPara: CERT_CHAIN_PARA;
  Chain : PCCERT_CHAIN_CONTEXT;
  HTTPS : HTTPSPolicyCallbackData;
  Policy: CERT_CHAIN_POLICY_PARA;
  Status: CERT_CHAIN_POLICY_STATUS;
begin
  Server := nil;
  Error := SSPI.QueryContextAttributes(@Context, SECPKG_ATTR_REMOTE_CERT_CONTEXT, @Server);
  if Error <> 0 then
  begin
    SSLError := 'QueryCredentialsAttributes returns ' + IntToHex(Error, 8);
    Exit(False);
  end;

  FillChar(ChainPara, sizeof(ChainPara), 0);
  ChainPara.cbSize := sizeof(ChainPara);
  ChainPara.RequestedUsage.dwType := USAGE_MATCH_TYPE_OR;
  ChainPara.RequestedUsage.Usage.cUsageIdentifier     := Length(USAGES);
  ChainPara.RequestedUsage.Usage.rgpszUsageIdentifier := PAnsiChar(@USAGES);

  Result := CertGetCertificateChain(
    0,
    Server,
    nil,
    Server.hCertStore,
    ChainPara,
    0,
    nil,
    Chain
  );

  if Result then
  begin

    FillChar(HTTPS, sizeof(HTTPS), 0);
    HTTPS.cbSize := SizeOf(HTTPS);
    HTTPS.dwAuthType := AUTHTYPE_SERVER;
    HTTPS.fdwChecks := 0;
    HTTPS.pwszServerName := PChar(ServerName);

    FillChar(Policy, SizeOf(Policy), 0);
    Policy.cbSize := sizeof(Policy);
    Policy.pvExtraPolicyPara := @HTTPS;

    FillChar(Status, SizeOf(Status), 0);
    Status.cbSize := SizeOf(Status);

    Result := CertVerifyCertificateChainPolicy(
      CERT_CHAIN_POLICY_SSL,
      Chain,
      Policy,
      Status);

    if Result then
    begin
      if Status.dwError = CERT_E_UNTRUSTEDROOT then
        Result := Validate(Chain, Status)
      else
        Result := Status.dwError = 0;
      if Result = False then
        SSLError := 'CertVerifyCertificateChainPolicy.Status = ' + IntToHex(Status.dwError, 8);// 800B0109
    end;

    CertFreeCertificateChain(Chain);
  end;

  CertFreeCertificateContext(Server);
end;

function TSSLInfo.InitBuffer: Boolean;
var
  Error: Integer;
begin
  Error := SSPI.QueryContextAttributes(@Context, SECPKG_ATTR_STREAM_SIZES, @BuffSizes);
  if Error <> SEC_E_OK then
  begin
    SSLError := 'QueryContextAttributes(SECPKG_ATTR_STREAM_SIZES) returns ' + IntToHex(Error, 8);
    Exit(False);
  end;
  SetLength(RecvBuffer, BuffSizes.cbHeader + BuffSizes.cbMaximumMessage + BuffSizes.cbTrailer);
  SetLength(SendBuffer, BuffSizes.cbHeader + BuffSizes.cbMaximumMessage + BuffSizes.cbTrailer);
  RecvCount := 0;
  DataCount := 0;
  DataStart := 0;
  Result := True;
end;

function TSSLInfo.Decrypt(var Data; Size: Integer): Integer;
begin
  Result := Readable;
  if Result > 0 then
  begin
    if Result > Size then
      Result := Size;
    Move(DataBuffer[DataStart], Data, Result);
    Inc(DataStart, Result);
  end;
end;

function TSSLInfo.Encrypt(var Data; Size: Integer): Integer;
var
  Source : PByte;
  Count  : Integer;
  Buffers: array[0..3] of TSecBuffer;
  Buffer : TSecBufferDesc;
begin
  Result := 0;
  Source := @Data;
  while Size > 0 do
  begin
    if Size > BuffSizes.cbMaximumMessage then
      Count := BuffSizes.cbMaximumMessage
    else
      Count := Size;
    Move(Source[Result], SendBuffer[BuffSizes.cbHeader], Count);
    Inc(Result, Count);
    Dec(Size, Count);

    Buffers[0].cbBuffer := BuffSizes.cbHeader;
    Buffers[0].BufferType := SECBUFFER_STREAM_HEADER;
    Buffers[0].pvBuffer := @SendBuffer[0];

    Buffers[1].cbBuffer := Count;
    Buffers[1].BufferType := SECBUFFER_DATA;
    Buffers[1].pvBuffer := @SendBuffer[BuffSizes.cbHeader];

    Buffers[2].cbBuffer := BuffSizes.cbTrailer;
    Buffers[2].BufferType := SECBUFFER_STREAM_TRAILER;
    Buffers[2].pvBuffer := @SendBuffer[BuffSizes.cbHeader + Count];

    Buffers[3].BufferType := SECBUFFER_EMPTY;

    Buffer.ulVersion := SECBUFFER_VERSION;
    Buffer.cBuffers := 4;
    Buffer.pBuffers := Addr(Buffers[0]);

    Error := SSPI.EncryptMessage(@Context, 0, @Buffer, 0);
    if FAILED(Error) then
    begin
      SSLError := 'EncryptMessage returns ' + IntToHex(Error, 8);
    {$IFDEF LOG}WriteLn(SSLError);{$ENDIF}
      Exit(-1);
    end;

    Count := SendData(Socket, PByte(SendBuffer), Buffers[0].cbBuffer + Buffers[1].cbBuffer + Buffers[2].cbBuffer);
    if Count <= 0 then
      Exit(Count);
  end;
end;


function SSLAvailable: Boolean;
begin
  Result := Initialized;
  if not Initialized then
  begin
   {$IFDEF LOG}WriteLn('SSLAvailable');{$ENDIF}
    Initialized := True;

    SSPI := InitSecurityInterface();
    if SSPI = nil then
    begin
      SSLError := 'InitSecurityInterface() returns nil';
      Exit;
    end;

    MyStore := CertOpenSystemStore(0, 'MY');
    if MyStore = 0 then
    begin
      SSLError := 'CertOpenSystemStore(0, ''MY'') returns 0';
      Exit;
    end;

    Result := True;
  end;
end;

function SSLStart(Socket: Integer; const Host: AnsiString = ''): Integer;
var
  Info: PSSLInfo;
begin
  Result := 0;

  if SSLAvailable = False then
  begin
    Exit;
  end;

  New(Info);
  Info.Init := [];
  Info.Socket := Socket;
  Info.Servername := Host;
  if not Info.Start then
  begin
    Info.Clean;
    Dispose(Info);
    Exit;
  end;

  Result := Integer(Info);
end;

function SSLConnect(SSL: Integer): Boolean;
var
  Info: PSSLInfo absolute SSL;
begin
  Result := SSL <> 0;
  if Result then
  begin
    Info.InitBuffer;
  end;
end;

function SSLPending(SSL: Integer): Boolean;
var
  Info: PSSLInfo absolute SSL;
begin
  Result := (Info <> nil) and (Info.Readable > 0);
end;

function SSLRead(SSL: Integer; var Data; Size: Integer): Integer;
var
  Info: PSSLInfo absolute SSL;
begin
  Result := Info.Decrypt(Data, Size);
end;

function SSLWrite(SSL: Integer; var Data; Size: Integer): Integer;
var
  Info: PSSLInfo absolute SSL;
begin
  Result := Info.Encrypt(Data, Size);
end;

function SSLClose(SSL: Integer): Integer;
var
  Info: PSSLInfo absolute SSL;
begin
  if SSL = 0 then
    Exit;
  Info.Clean;
  Dispose(Info);
end;

initialization
{$IFDEF LOG}AllocConsole;{$ENDIF}
  Validators := nil;
finalization
  FreeValidators;
end.
