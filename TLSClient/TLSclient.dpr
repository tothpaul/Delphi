program TLSclient;
{
 SSPI Schannel gmail TLS connection example for Delphi Tokyo (c) 2017 Execute SARL

 based on
 http://www.coastrd.com/c-schannel-smtp

 with info from
 http://ftp.icpdas.com/pub/beta_version/VHM/wince600/at91sam9g45m10ek_armv4i/cesysgen/sdk/inc/schannel.h
 https://github.com/vohung96/exodus/blob/8f7ea641421279b1dc03326114901e4781aa2c7d/exodus/idSSLSchannel.pas
 https://github.com/erdincay/doubleCommander/blob/97c9fdd3c28a5545eb055e2010fe4b3396c3794f/plugins/wfx/ftp/synapse/ssl_winssl_lib.pas
}

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  Winapi.Windows,
  Winapi.Winsock,
  System.SysUtils,
  SChannel in 'SChannel.pas';

// This C example is designed as more of a guide than a library to be plugged into an application
// That module required a couple of major re-writes and is available upon request
// This will work with connections on port 587 that upgrade a plain text session to an encrypted session with STARTTLS as covered here.

// TLSclient.c -

const
  IO_BUFFER_SIZE  = $10000;
  DLL_NAME        = 'Secur32.dll';
  NT4_DLL_NAME    = 'Security.dll';

// Globals.
var
  fVerbose        : Boolean = FALSE;

  iPortNumber     : Integer = 465; // gmail TLS
  pszServerName   : string = 'smtp.gmail.com'; // DNS name of server
  pszUser         : string = ''; // if specified, a certificate in "MY" store is searched for

  dwProtocol      : Cardinal = SP_PROT_TLS1; // SP_PROT_TLS1; // SP_PROT_PCT1; SP_PROT_SSL2; SP_PROT_SSL3; 0=default
  aiKeyExch       : ALG_ID  = 0; // = default; CALG_DH_EPHEM; CALG_RSA_KEYX;

  fUseProxy       : Boolean = FALSE;
  pszProxyServer  : string = 'proxy';
  iProxyPort      : Integer = 80;

  hMyCertStore    : HCERTSTORE = 0;
  g_hSecurity     : HMODULE = 0;

  SchannelCred    : SCHANNEL_CRED;
  g_pSSPI         : PSecurityFunctionTable;

(*****************************************************************************)
procedure DisplayWinVerifyTrustError(Status: DWORD);
var
  pszName: string;
begin
  case HRESULT(Status) of
    CERT_E_EXPIRED:                pszName := 'CERT_E_EXPIRED';
    CERT_E_VALIDITYPERIODNESTING:  pszName := 'CERT_E_VALIDITYPERIODNESTING';
    CERT_E_ROLE:                   pszName := 'CERT_E_ROLE';
    CERT_E_PATHLENCONST:           pszName := 'CERT_E_PATHLENCONST';
    CERT_E_CRITICAL:               pszName := 'CERT_E_CRITICAL';
    CERT_E_PURPOSE:                pszName := 'CERT_E_PURPOSE';
    CERT_E_ISSUERCHAINING:         pszName := 'CERT_E_ISSUERCHAINING';
    CERT_E_MALFORMED:              pszName := 'CERT_E_MALFORMED';
    CERT_E_UNTRUSTEDROOT:          pszName := 'CERT_E_UNTRUSTEDROOT';
    CERT_E_CHAINING:               pszName := 'CERT_E_CHAINING';
    TRUST_E_FAIL:                  pszName := 'TRUST_E_FAIL';
    CERT_E_REVOKED:                pszName := 'CERT_E_REVOKED';
    CERT_E_UNTRUSTEDTESTROOT:      pszName := 'CERT_E_UNTRUSTEDTESTROOT';
    CERT_E_REVOCATION_FAILURE:     pszName := 'CERT_E_REVOCATION_FAILURE';
    CERT_E_CN_NO_MATCH:            pszName := 'CERT_E_CN_NO_MATCH';
    CERT_E_WRONG_USAGE:            pszName := 'CERT_E_WRONG_USAGE';
  else                             pszName := '(unknown)';
  end;
  Writeln('Error ', IntToHex(Status) ,' (', pszName ,') returned by CertVerifyCertificateChainPolicy!');
end;


(*****************************************************************************)
procedure DisplayWinSockError(ErrCode: DWORD);
var
  pszName: string;  // http://www.sockets.com/err_lst1.htm#WSANO_DATA
begin
  case(ErrCode) of // http://msdn.microsoft.com/en-us/library/ms740668(VS.85).aspx
    10035:  pszName := 'WSAEWOULDBLOCK    ';
    10036:  pszName := 'WSAEINPROGRESS    ';
    10037:  pszName := 'WSAEALREADY       ';
    10038:  pszName := 'WSAENOTSOCK       ';
    10039:  pszName := 'WSAEDESTADDRREQ   ';
    10040:  pszName := 'WSAEMSGSIZE       ';
    10041:  pszName := 'WSAEPROTOTYPE     ';
    10042:  pszName := 'WSAENOPROTOOPT    ';
    10043:  pszName := 'WSAEPROTONOSUPPORT';
    10044:  pszName := 'WSAESOCKTNOSUPPORT';
    10045:  pszName := 'WSAEOPNOTSUPP     ';
    10046:  pszName := 'WSAEPFNOSUPPORT   ';
    10047:  pszName := 'WSAEAFNOSUPPORT   ';
    10048:  pszName := 'WSAEADDRINUSE     ';
    10049:  pszName := 'WSAEADDRNOTAVAIL  ';
    10050:  pszName := 'WSAENETDOWN       ';
    10051:  pszName := 'WSAENETUNREACH    ';
    10052:  pszName := 'WSAENETRESET      ';
    10053:  pszName := 'WSAECONNABORTED   ';
    10054:  pszName := 'WSAECONNRESET     ';
    10055:  pszName := 'WSAENOBUFS        ';
    10056:  pszName := 'WSAEISCONN        ';
    10057:  pszName := 'WSAENOTCONN       ';
    10058:  pszName := 'WSAESHUTDOWN      ';
    10059:  pszName := 'WSAETOOMANYREFS   ';
    10060:  pszName := 'WSAETIMEDOUT      ';
    10061:  pszName := 'WSAECONNREFUSED   ';
    10062:  pszName := 'WSAELOOP          ';
    10063:  pszName := 'WSAENAMETOOLONG   ';
    10064:  pszName := 'WSAEHOSTDOWN      ';
    10065:  pszName := 'WSAEHOSTUNREACH   ';
    10066:  pszName := 'WSAENOTEMPTY      ';
    10067:  pszName := 'WSAEPROCLIM       ';
    10068:  pszName := 'WSAEUSERS         ';
    10069:  pszName := 'WSAEDQUOT         ';
    10070:  pszName := 'WSAESTALE         ';
    10071:  pszName := 'WSAEREMOTE        ';
    10091:  pszName := 'WSASYSNOTREADY    ';
    10092:  pszName := 'WSAVERNOTSUPPORTED';
    10093:  pszName := 'WSANOTINITIALISED ';
    11001:  pszName := 'WSAHOST_NOT_FOUND ';
    11002:  pszName := 'WSATRY_AGAIN      ';
    11003:  pszName := 'WSANO_RECOVERY    ';
    11004:  pszName := 'WSANO_DATA        ';
    else pszName := '';
  end;
  WriteLn('Error ', ErrCode ,' (',pszName,')');
end;

(*****************************************************************************)
procedure DisplaySECError(ErrCode: DWORD);
var
  pszName: string; // WinError.h
begin
  case(ErrCode) of
    SEC_E_BUFFER_TOO_SMALL:
        pszName := 'SEC_E_BUFFER_TOO_SMALL - The message buffer is too small. Used with the Digest SSP.';

    SEC_E_CRYPTO_SYSTEM_INVALID:
        pszName := 'SEC_E_CRYPTO_SYSTEM_INVALID - The cipher chosen for the security context is not supported. Used with the Digest SSP.';

    SEC_E_INCOMPLETE_MESSAGE:
        pszName := 'SEC_E_INCOMPLETE_MESSAGE - The data in the input buffer is incomplete. The application needs to read more data from the server and call DecryptMessage (General) again.';

    SEC_E_INVALID_HANDLE:
        pszName := 'SEC_E_INVALID_HANDLE - A context handle that is not valid was specified in the phContext parameter. Used with the Digest and Schannel SSPs.';

    SEC_E_INVALID_TOKEN:
        pszName := 'SEC_E_INVALID_TOKEN - The buffers are of the wrong type or no buffer of type SECBUFFER_DATA was found. Used with the Schannel SSP.';

    SEC_E_MESSAGE_ALTERED:
        pszName := 'SEC_E_MESSAGE_ALTERED - The message has been altered. Used with the Digest and Schannel SSPs.';

    SEC_E_OUT_OF_SEQUENCE:
        pszName := 'SEC_E_OUT_OF_SEQUENCE - The message was not received in the correct sequence.';

    SEC_E_QOP_NOT_SUPPORTED:
        pszName := 'SEC_E_QOP_NOT_SUPPORTED - Neither confidentiality nor integrity are supported by the security context. Used with the Digest SSP.';

    SEC_I_CONTEXT_EXPIRED:
        pszName := 'SEC_I_CONTEXT_EXPIRED - The message sender has finished using the connection and has initiated a shutdown.';

    SEC_I_RENEGOTIATE:
        pszName := 'SEC_I_RENEGOTIATE - The remote party requires a new handshake sequence or the application has just initiated a shutdown.';

    SEC_E_ENCRYPT_FAILURE:
        pszName := 'SEC_E_ENCRYPT_FAILURE - The specified data could not be encrypted.';

    SEC_E_DECRYPT_FAILURE:
        pszName := 'SEC_E_DECRYPT_FAILURE - The specified data could not be decrypted.';

    else
        pszName := '';
  end;
  WriteLn('Error ', ErrCode ,' ', pszName);
end;

(*****************************************************************************)
procedure DisplayCertChain(pServerCert: PCCERT_CONTEXT; fLocal: BOOL);
var
  szName: array[0..999] of Char;
  pCurrentCert, pIssuerCert: PCCERT_CONTEXT;
  dwVerificationFlags: DWORD;
begin
  WriteLn;

  // display leaf name
  if(0 = CertNameToStr(
     pServerCert.dwCertEncodingType,
     pServerCert.pCertInfo.Subject,
     CERT_X500_NAME_STR or CERT_NAME_STR_NO_PLUS_FLAG,
     szName, sizeof(szName) )) then
  begin
    WriteLn('**** Error ',GetLastError(),' building subject name');
  end;

  if(fLocal) then
    WriteLn('Client subject: ', szName)
  else
    WriteLn('Server subject: ', szName);

  if(0 = CertNameToStr(
    pServerCert.dwCertEncodingType,
    pServerCert.pCertInfo.Issuer,
    CERT_X500_NAME_STR or CERT_NAME_STR_NO_PLUS_FLAG,
    szName, sizeof(szName) ) ) then
  begin
    WriteLn('**** Error ', GetLastError(), ' building issuer name');
  end;

  if(fLocal) then
    WriteLn('Client issuer: ', szName)
  else
    WriteLn('Server issuer: ', szName);

  // display certificate chain
  pCurrentCert := pServerCert;
  while (pCurrentCert <> nil) do
  begin
    dwVerificationFlags := 0;
    pIssuerCert := CertGetIssuerCertificateFromStore(pServerCert.hCertStore, pCurrentCert, nil, @dwVerificationFlags);
    if(pIssuerCert = nil) then
    begin
      if(pCurrentCert <> pServerCert) then
        CertFreeCertificateContext(pCurrentCert);
      break;
    end;

    if(0 = CertNameToStr(
      pIssuerCert.dwCertEncodingType,
      pIssuerCert.pCertInfo.Subject,
      CERT_X500_NAME_STR or CERT_NAME_STR_NO_PLUS_FLAG,
      szName, sizeof(szName) ) ) then
    begin
      WriteLn('**** Error ',GetLastError,' building subject name');
    end;

    WriteLn('CA subject: ', szName);

    if(0 = CertNameToStr(
      pIssuerCert.dwCertEncodingType,
      pIssuerCert.pCertInfo.Issuer,
      CERT_X500_NAME_STR or CERT_NAME_STR_NO_PLUS_FLAG,
      szName, sizeof(szName) ) ) then
    begin
      WriteLn('**** Error ',GetLastError,' building issuer name');
    end;

    WriteLn('CA issuer: ', szName);

    if(pCurrentCert <> pServerCert) then
      CertFreeCertificateContext(pCurrentCert);

    pCurrentCert := pIssuerCert;
    pIssuerCert := nil;
  end;
end;

(*****************************************************************************)
procedure DisplayConnectionInfo(phContext: pCtxtHandle);
var
    Status: SECURITY_STATUS;
    ConnectionInfo: TSecPkgContextConnectionInfo;
begin
    Status := g_pSSPI.QueryContextAttributes(phContext, SECPKG_ATTR_CONNECTION_INFO, @ConnectionInfo);
    if(Status <> SEC_E_OK) then
    begin
      WriteLn('Error ',Status,' querying connection info');
      Exit;
    end;

    WriteLn;

    case (ConnectionInfo.dwProtocol) of
      SP_PROT_TLS1_CLIENT:
          WriteLn('Protocol: TLS1');

      SP_PROT_SSL3_CLIENT:
          WriteLn('Protocol: SSL3');

      SP_PROT_PCT1_CLIENT:
          WriteLn('Protocol: PCT');

      SP_PROT_SSL2_CLIENT:
          WriteLn('Protocol: SSL2');

      else
          WriteLn('Protocol: ', ConnectionInfo.dwProtocol);
    end;

    case (ConnectionInfo.aiCipher) of

      CALG_RC4:
          WriteLn('Cipher: RC4');

      CALG_3DES:
          WriteLn('Cipher: Triple DES');

      CALG_RC2:
          WriteLn('Cipher: RC2');

      CALG_DES,
      CALG_CYLINK_MEK:
          WriteLn('Cipher: DES');

      CALG_SKIPJACK:
          WriteLn('Cipher: Skipjack');

      else
          WriteLn('Cipher: ', ConnectionInfo.aiCipher);
    end;

    WriteLn('Cipher strength: ', ConnectionInfo.dwCipherStrength);

    case (ConnectionInfo.aiHash) of

      CALG_MD5:
          WriteLn('Hash: MD5');

      CALG_SHA:
          WriteLn('Hash: SHA');

      else
          WriteLn('Hash: ', ConnectionInfo.aiHash);
    end;

    WriteLn('Hash strength: ', ConnectionInfo.dwHashStrength);

    case (ConnectionInfo.aiExch) of
      CALG_RSA_KEYX,
      CALG_RSA_SIGN:
          WriteLn('Key exchange: RSA');

      CALG_KEA_KEYX:
          WriteLn('Key exchange: KEA');

      CALG_DH_EPHEM:
          WriteLn('Key exchange: DH Ephemeral');

      else
          WriteLn('Key exchange: ', ConnectionInfo.aiExch);
    end;

    WriteLn('Key exchange strength: ', ConnectionInfo.dwExchStrength);
end;


(*****************************************************************************)
procedure PrintHexDump(length: DWORD; buffer: PBYTE);
var
  i, count, index: DWORD;
const
  rgbDigits : array[0..$F] of AnsiChar = '0123456789abcdef';
var
  rgbLine: array[0..99] of AnsiChar;
  cbLine: Integer;
begin
  //for(index = 0; length; length -= count, buffer += count, index += count)
  while length > 0 do begin
    if length > 16 then
      count := 16
    else
      count := length;
    //sprintf(rgbLine, "%4.4x  ",index);
    rgbline[0] := rgbDigits[(index shr 12) and $F];
    rgbline[1] := rgbDigits[(index shr 8) and $F];
    rgbline[2] := rgbDigits[(index shr 4) and $F];
    rgbline[3] := rgbDigits[index and $F];
    rgbline[4] := ' ';
    rgbline[5] := ' ';
    cbLine := 6;

    for i := 0 to  count - 1 do
    begin
        rgbLine[cbLine] := rgbDigits[buffer[i] shr 4]; inc(cbLine);
        rgbLine[cbLine] := rgbDigits[buffer[i] and $f]; inc(cbLine);
        if(i = 7) then rgbLine[cbLine] := ':'
        else rgbLine[cbLine] := ' '; Inc(cbLine);
    end;

    while i < 16 do
    begin
        rgbLine[cbLine] := ' '; inc(cbLine);
        rgbLine[cbLine] := ' '; inc(cbLine);
        rgbLine[cbLine] := ' '; inc(cbLine);
        inc(i);
    end;
    rgbLine[cbLine] := ' '; inc(cbLine);

    for i := 0 to count - 1 do
    begin
        if(buffer[i] < 32) or (buffer[i] > 126) or (buffer[i] = Ord('%')) then rgbLine[cbLine] := '.'
        else rgbLine[cbLine] := AnsiChar(buffer[i]); inc(cbLine);
    end;
    rgbLine[cbLine] := #0; inc(cbLine);
    WriteLn(rgbLine);

    dec(length, count);
    inc(buffer, count);
    inc(index, count);
  end;
end;

(*****************************************************************************)
procedure PrintText( length: DWORD; buffer : PBYTE );// handle unprintable charaters
var
  i: Integer; //
begin
  WriteLn; // "length = %d bytes \n", length);
  for i := 0 to length - 1 do
  begin
    if( buffer[i] = 10) or (buffer[i] = 13 ) then
      Write(AnsiChar(buffer[i]))
    else
    if( buffer[i] < 32) or (buffer[i] > 126) or (buffer[i] = Ord('%') ) then
      Write('.')
    else
      Write(AnsiChar(buffer[i]));
  end;
  WriteLn;
end;

(*****************************************************************************)
procedure WriteDataToFile( const pszData: string; pbData: PBYTE; cbData : DWORD );
var
  f: file;
begin
  AssignFile(f, pszData);
  Rewrite(f, 1);
  blockWrite(f, pbData^, cbData);
  CloseFile(f);
end;

(*****************************************************************************)
function LoadSecurityLibrary: Boolean; // load SSPI.DLL, set up a special table - PSecurityFunctionTable
var
  pInitSecurityInterface: INIT_SECURITY_INTERFACE;
  VerInfo: OSVERSIONINFO ;
  lpszDLL: string;
begin
  //  Find out which security DLL to use, depending on
  //  whether we are on Win2K, NT or Win9x
  VerInfo.dwOSVersionInfoSize := sizeof (OSVERSIONINFO);
  if (False = GetVersionEx (VerInfo) ) then
    Exit(FALSE);

  if ( VerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT) and (VerInfo.dwMajorVersion = 4 ) then
  begin
    lpszDLL := NT4_DLL_NAME;
  end
  else
  if ( VerInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) or
     (VerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT ) then
  begin
    lpszDLL := DLL_NAME;
  end
  else begin
    WriteLn('System not recognize' );
    Exit(FALSE);
  end;


  //  Load Security DLL
  g_hSecurity := LoadLibrary(PChar(lpszDLL));
  if(g_hSecurity = 0) then
  begin
    WriteLn('Error ',GetLastError,' loading ', lpszDLL );
    Exit(FALSE);
  end;

  @pInitSecurityInterface := GetProcAddress(g_hSecurity, 'InitSecurityInterfaceW' );
  if(@pInitSecurityInterface = nil) then
  begin
    WriteLn( 'Error ', GetLastError ,' reading InitSecurityInterface entry point.');
    Exit(FALSE);
  end;

  g_pSSPI := pInitSecurityInterface();
  if(g_pSSPI = nil) then
  begin
    WriteLn('Error ',GetLastError,' reading security interface.');
    Exit(FALSE);
  end;

  Result := TRUE; // and PSecurityFunctionTable
end;


(*****************************************************************************)
procedure UnloadSecurityLibrary;
begin
  FreeLibrary(g_hSecurity);
  g_hSecurity := 0;
end;


(*****************************************************************************)
function VerifyServerCertificate(pServerCert: PCCERT_CONTEXT; const pszServerName: string; dwCertFlags : DWORD): DWORD;
var
  polHttps: HTTPSPolicyCallbackData;
  PolicyPara: CERT_CHAIN_POLICY_PARA;
  PolicyStatus: CERT_CHAIN_POLICY_STATUS;
  ChainPara : CERT_CHAIN_PARA;
  pChainContext: PCCERT_CHAIN_CONTEXT;
  cchServerName, Status: DWORD                                         ;
const
  rgszUsages: array[0..2] of PAnsiChar = (
    szOID_PKIX_KP_SERVER_AUTH,
    szOID_SERVER_GATED_CRYPTO,
    szOID_SGC_NETSCAPE
  );

 cUsages : DWORD = sizeof(rgszUsages) div sizeof(LPSTR);

label
    cleanup;
begin
    pChainContext := nil;

    if(pServerCert = nil) then
    begin
      Status := SEC_E_WRONG_PRINCIPAL;
      goto cleanup;
    end;

    // Build certificate chain.
    ZeroMemory(@ChainPara, sizeof(ChainPara));
    ChainPara.cbSize := sizeof(ChainPara);
    ChainPara.RequestedUsage.dwType := USAGE_MATCH_TYPE_OR;
    ChainPara.RequestedUsage.Usage.cUsageIdentifier     := cUsages;
    ChainPara.RequestedUsage.Usage.rgpszUsageIdentifier := PAnsiChar(@rgszUsages);

    if(False = CertGetCertificateChain(
      0,
      pServerCert,
      nil,
      pServerCert.hCertStore,
      ChainPara,
      0,
      nil,
      pChainContext ) ) then
    begin
      Status := GetLastError();
      WriteLn('Error ',Status,' returned by CertGetCertificateChain');
      goto cleanup;
    end;


    // Validate certificate chain.
    ZeroMemory(@polHttps, sizeof(HTTPSPolicyCallbackData));
    polHttps.cbSize             := sizeof(HTTPSPolicyCallbackData);
    polHttps.dwAuthType         := AUTHTYPE_SERVER;
    polHttps.fdwChecks          := dwCertFlags;
    polHttps.pwszServerName     := PChar(pszServerName);

    FillChar(PolicyPara, sizeof(PolicyPara), 0);
    PolicyPara.cbSize            := sizeof(PolicyPara);
    PolicyPara.pvExtraPolicyPara := @polHttps;

    FillChar(PolicyStatus, sizeof(PolicyStatus), 0);
    PolicyStatus.cbSize := sizeof(PolicyStatus);

    if(False = CertVerifyCertificateChainPolicy(
      CERT_CHAIN_POLICY_SSL,
      pChainContext,
      PolicyPara,
      PolicyStatus ) ) then
    begin
      Status := GetLastError();
      WriteLn('Error ',Status,' returned by CertVerifyCertificateChainPolicy!');
      goto cleanup;
    end;

    if(PolicyStatus.dwError <> 0) then
    begin
      Status := PolicyStatus.dwError;
      DisplayWinVerifyTrustError(Status);
      goto cleanup;
    end;

    Status := SEC_E_OK;

cleanup:
    if(pChainContext <> nil) then
      CertFreeCertificateChain(pChainContext);

    Result := Status;
end;


(*****************************************************************************)
function CreateCredentials(const pszUser: string; phCreds: PCredHandle): SECURITY_STATUS;
var
  tsExpiry: TimeStamp;
  Status: SECURITY_STATUS;
  cSupportedAlgs: DWORD;
  rgbSupportedAlgs: array[0..15] of ALG_ID;
  pCertContext: PCCERT_CONTEXT;
begin
  cSupportedAlgs := 0;
  pCertContext := nil;

  // Open the "MY" certificate store, where IE stores client certificates.
  // Windows maintains 4 stores -- MY, CA, ROOT, SPC.
  if(hMyCertStore = 0) then
  begin
    hMyCertStore := CertOpenSystemStore(0, 'MY');
    if(0 = hMyCertStore) then
    begin
      WriteLn('**** Error ',GetLastError(),' returned by CertOpenSystemStore');
      Exit(SEC_E_NO_CREDENTIALS);
    end;
  end;


  // If a user name is specified, then attempt to find a client
  // certificate. Otherwise, just create a NULL credential.
  if(pszUser <> '') then
  begin
    // Find client certificate. Note that this sample just searches for a
    // certificate that contains the user name somewhere in the subject name.
    // A real application should be a bit less casual.
    pCertContext := CertFindCertificateInStore(
      hMyCertStore,                     // hCertStore
      X509_ASN_ENCODING,             // dwCertEncodingType
      0,                                             // dwFindFlags
      CERT_FIND_SUBJECT_STR_A,// dwFindType
      PChar(pszUser),                         // *pvFindPara
      nil );                                 // pPrevCertContext


    if(pCertContext = nil) then
    begin
        WriteLn('**** Error ',GetLastError,' returned by CertFindCertificateInStore');
                    if( GetLastError() = CRYPT_E_NOT_FOUND ) then WriteLn('CRYPT_E_NOT_FOUND - property doesn''t exist');
                    Exit(SEC_E_NO_CREDENTIALS);
    end;
  end;


  // Build Schannel credential structure. Currently, this sample only
  // specifies the protocol to be used (and optionally the certificate,
  // of course). Real applications may wish to specify other parameters as well.
  ZeroMemory( @SchannelCred, sizeof(SchannelCred) );

  SchannelCred.dwVersion  := SCHANNEL_CRED_VERSION;
  if(pCertContext <> nil) then
  begin
    SchannelCred.cCreds     := 1;
    SchannelCred.paCred     := @pCertContext;
  end;

  SchannelCred.grbitEnabledProtocols := dwProtocol;

  if(aiKeyExch <> 0) then
  begin
    rgbSupportedAlgs[cSupportedAlgs] := aiKeyExch;
    Inc(cSupportedAlgs);
  end;

  if(cSupportedAlgs > 0) then
  begin
    SchannelCred.cSupportedAlgs    := cSupportedAlgs;
    SchannelCred.palgSupportedAlgs := @rgbSupportedAlgs[0];
  end;

  SchannelCred.dwFlags := SchannelCred.dwFlags or SCH_CRED_NO_DEFAULT_CREDS;

  // The SCH_CRED_MANUAL_CRED_VALIDATION flag is specified because
  // this sample verifies the server certificate manually.
  // Applications that expect to run on WinNT, Win9x, or WinME
  // should specify this flag and also manually verify the server
  // certificate. Applications running on newer versions of Windows can
  // leave off this flag, in which case the InitializeSecurityContext
  // function will validate the server certificate automatically.
  SchannelCred.dwFlags := SchannelCred.dwFlags or SCH_CRED_MANUAL_CRED_VALIDATION;


  // Create an SSPI credential.
  Status := g_pSSPI.AcquireCredentialsHandle(
    nil,                 // Name of principal
    UNISP_NAME,          // Name of package
    SECPKG_CRED_OUTBOUND,// Flags indicating use
    nil,                 // Pointer to logon ID
   @SchannelCred,        // Package specific data
    nil,                 // Pointer to GetKey() func
    nil,                 // Value to pass to GetKey()
    phCreds,             // (out) Cred Handle
   @tsExpiry );          // (out) Lifetime (optional)

  if(Status <> SEC_E_OK) then
    WriteLn('**** Error ',Status,' returned by AcquireCredentialsHandle');

  // cleanup: Free the certificate context. Schannel has already made its own copy.
  if(pCertContext <> nil) then
    CertFreeCertificateContext(pCertContext);

  Result := Status;
end;

(*****************************************************************************)
function ConnectToServer(const pszServerName: string; iPortNumber: Integer; var pSocket: TSOCKET): Integer;
var
  Socket : TSOCKET ;
  sin: sockaddr_in;
  hp: phostent;
  pbMessage: AnsiString;//array[0..199] of Byte;
  cbMessage: DWORD;
  aHost: AnsiString;
begin

  Socket := Winapi.Winsock.socket(PF_INET, SOCK_STREAM, 0);
  if(Socket = INVALID_SOCKET) then
  begin
    WriteLn('**** Error ',WSAGetLastError,' creating socket');
    DisplayWinSockError( WSAGetLastError() );
    Exit(WSAGetLastError());
  end;


  if(fUseProxy) then
  begin
    sin.sin_family := AF_INET;
    sin.sin_port := ntohs(iProxyPort);
    aHost := AnsiString(pszProxyServer);
    hp := gethostbyname(PAnsiChar(aHost));
    if(hp = nil) then
    begin
      WriteLn('**** Error ',WSAGetLastError,' returned by gethostbyname using Proxy');
      DisplayWinSockError( WSAGetLastError() );
      Exit(WSAGetLastError());
    end
    else
      sin.sin_addr := PInAddr(hp.h_addr^)^;
  end
  else // No proxy used
  begin
    sin.sin_family := AF_INET;
    sin.sin_port := htons(iPortNumber);
    aHost := AnsiString(pszServerName);
    hp := gethostbyname(PAnsiChar(aHost));
    if (hp = nil) then
    begin
      WriteLn('**** Error returned by gethostbyname');
      DisplayWinSockError( WSAGetLastError() );
      Exit(WSAGetLastError());
    end
    else
      sin.sin_addr := PInAddr(hp.h_addr^)^;
  end;


  if(connect(Socket, sin, sizeof(sin)) = SOCKET_ERROR) then
  begin
    WriteLn('**** Error ',WSAGetLastError,' connecting to "',pszServerName,'" (',inet_ntoa(sin.sin_addr),')');
    closesocket(Socket);
    DisplayWinSockError( WSAGetLastError() );
    Exit( WSAGetLastError());
  end;


  if(fUseProxy) then
  begin
     pbMessage := 'CONNECT '
                + pszServerName + ':' + IntToStr(iPortNumber)
                + ' HTTP/1.0'#13#10
                + 'User-Agent: webclient'#13#10
                + #13#10;
     cbMessage := Length(pbMessage);

    // Send message to proxy server
    if(send(Socket, pbMessage, cbMessage, 0) = SOCKET_ERROR) then
    begin
      WriteLn('**** Error ',WSAGetLastError,' sending message to proxy!');
      DisplayWinSockError( WSAGetLastError() );
      Exit(WSAGetLastError());
    end;

    // Receive message from proxy server
    SetLength(pbMessage, 200);
    cbMessage := recv(Socket, pbMessage[1], 200, 0);
    if(cbMessage = SOCKET_ERROR) then
    begin
      WriteLn('**** Error ',WSAGetLastError,' receiving message from proxy');
      DisplayWinSockError( WSAGetLastError() );
      Exit(WSAGetLastError());
    end;
    // this sample is limited but in normal use it
    // should continue to receive until CR LF CR LF is received
  end;
  pSocket := Socket;

  Result := SEC_E_OK;
end;

(*****************************************************************************)
function DisconnectFromServer(Socket: TSOCKET; phCreds: PCredHandle; phContext: PCtxtHandle): LONG;
var
  pbMessage: PBYTE;
  dwType, dwSSPIFlags, dwSSPIOutFlags, cbMessage, cbData, Status: DWORD;
  OutBuffer: TSecBufferDesc;
  OutBuffers: array[0..0] of TSecBuffer;
  tsExpiry: TimeStamp;
label
  cleanup;
begin

  dwType := SCHANNEL_SHUTDOWN; // Notify schannel that we are about to close the connection.

  OutBuffers[0].pvBuffer   := @dwType;
  OutBuffers[0].BufferType := SECBUFFER_TOKEN;
  OutBuffers[0].cbBuffer   := sizeof(dwType);

  OutBuffer.cBuffers  := 1;
  OutBuffer.pBuffers  := Addr(OutBuffers[0]);
  OutBuffer.ulVersion := SECBUFFER_VERSION;

  Status := g_pSSPI.ApplyControlToken(phContext, @OutBuffer);
  if(FAILED(Status)) then begin WriteLn('**** Error ',Status,' returned by ApplyControlToken'); goto cleanup; end;


  // Build an SSL close notify message.
  dwSSPIFlags := ISC_REQ_SEQUENCE_DETECT   or
                ISC_REQ_REPLAY_DETECT     or
                ISC_REQ_CONFIDENTIALITY   or
                ISC_RET_EXTENDED_ERROR    or
                ISC_REQ_ALLOCATE_MEMORY   or
                ISC_REQ_STREAM;

  OutBuffers[0].pvBuffer   := nil;
  OutBuffers[0].BufferType := SECBUFFER_TOKEN;
  OutBuffers[0].cbBuffer   := 0;

  OutBuffer.cBuffers  := 1;
  OutBuffer.pBuffers  := Addr(OutBuffers[0]);
  OutBuffer.ulVersion := SECBUFFER_VERSION;

  Status := g_pSSPI.InitializeSecurityContext(
    phCreds,
    phContext,
    nil,
    dwSSPIFlags,
    0,
    SECURITY_NATIVE_DREP,
    nil,
    0,
    phContext,
   @OutBuffer,
    dwSSPIOutFlags,
   @tsExpiry
  );

  if(FAILED(Status)) then
  begin
    WriteLn('**** Error ',Status,' returned by InitializeSecurityContext');
    goto cleanup;
  end;

  pbMessage := OutBuffers[0].pvBuffer;
  cbMessage := OutBuffers[0].cbBuffer;


  // Send the close notify message to the server.
  if(pbMessage <> nil) and (cbMessage <> 0) then
  begin
    cbData := send(Socket, pbMessage^, cbMessage, 0);
    if(cbData = SOCKET_ERROR) or (cbData = 0) then
    begin
      Status := WSAGetLastError();
      WriteLn('**** Error ',Status,' sending close notify');
      DisplayWinSockError( WSAGetLastError() );
      goto cleanup;
    end;
    WriteLn('Sending Close Notify');
    WriteLn(cbData, ' bytes of handshake data sent');
    if(fVerbose) then begin PrintHexDump(cbData, pbMessage); WriteLn; end;
    g_pSSPI.FreeContextBuffer(pbMessage); // Free output buffer.
  end;


cleanup:
  g_pSSPI.DeleteSecurityContext(phContext); // Free the security context.
  closesocket(Socket); // Close the socket.

  Result := Status;
end;



(*****************************************************************************)
procedure GetNewClientCredentials(phCreds: PCredHandle; phContext: pCtxtHandle);
var
    IssuerListInfo: TSecPkgContextIssuerListInfoEx;
    pChainContext: PCCERT_CHAIN_CONTEXT;
    FindByIssuerPara: CERT_CHAIN_FIND_BY_ISSUER_PARA;
    pCertContext: PCCERT_CONTEXT;
    tsExpiry: TimeStamp;
    Status: SECURITY_STATUS;
begin

  // Read list of trusted issuers from schannel.
  Status := g_pSSPI.QueryContextAttributes( phContext, SECPKG_ATTR_ISSUER_LIST_EX, @IssuerListInfo );
  if(Status <> SEC_E_OK) then
  begin
    WriteLn('Error ',Status,' querying issuer list info');
    Exit;
  end;

  // Enumerate the client certificates.
  ZeroMemory(@FindByIssuerPara, sizeof(FindByIssuerPara));

  FindByIssuerPara.cbSize := sizeof(FindByIssuerPara);
  FindByIssuerPara.pszUsageIdentifier := szOID_PKIX_KP_CLIENT_AUTH;
  FindByIssuerPara.dwKeySpec := 0;
  FindByIssuerPara.cIssuer   := IssuerListInfo.cIssuers;
  FindByIssuerPara.rgIssuer  := Addr(IssuerListInfo.aIssuers[0]);

  pChainContext := nil;

  while(TRUE) do
  begin   // Find a certificate chain.
    pChainContext := CertFindChainInStore(
      hMyCertStore,
      X509_ASN_ENCODING,
      0,
      CERT_CHAIN_FIND_BY_ISSUER,
     @FindByIssuerPara,
      pChainContext
    );
    if (pChainContext = nil) then
    begin
      WriteLn('Error ',GetLastError,' finding cert chain');
      break;
    end;

    WriteLn;
    WriteLn('certificate chain found');

    // Get pointer to leaf certificate context.
    pCertContext := pChainContext.rgpChain[0].rgpElement[0].pCertContext;

    // Create schannel credential.
    SchannelCred.dwVersion := SCHANNEL_CRED_VERSION;
    SchannelCred.cCreds := 1;
    SchannelCred.paCred := @pCertContext;

    Status := g_pSSPI.AcquireCredentialsHandle(
      nil,                   // Name of principal
      UNISP_NAME,            // Name of package
      SECPKG_CRED_OUTBOUND,  // Flags indicating use
      nil,                   // Pointer to logon ID
     @SchannelCred,          // Package specific data
      nil,                   // Pointer to GetKey() func
      nil,                   // Value to pass to GetKey()
      phCreds,               // (out) Cred Handle
     @tsExpiry );            // (out) Lifetime (optional)

    if(Status <> SEC_E_OK) then
    begin
      WriteLn('**** Error ',Status,' returned by AcquireCredentialsHandle');
      Continue;
    end;

    WriteLn;
    WriteLn('new schannel credential created');

    g_pSSPI.FreeCredentialsHandle(phCreds); // Destroy the old credentials.
  end;
end;

(*****************************************************************************)
function ClientHandshakeLoop(
  Socket: TSOCKET          ;         // in
  phCreds: PCredHandle     ;        // in
  phContext: pCtxtHandle;      // in, out
  fDoInitialRead: BOOL            ; // in
  pExtraData: pSecBuffer)    // out
 : SECURITY_STATUS ;
var

    OutBuffer, InBuffer: TSecBufferDesc;
    InBuffers: array[0..1] of TSecBuffer; OutBuffers: array[0..0] of TSecBuffer;
    dwSSPIFlags, dwSSPIOutFlags: DWORD; cbData: Integer; cbIoBuffer: DWORD;
    tsExpiry: TimeStamp       ;
    scRet: SECURITY_STATUS ;
    IoBuffer: PUCHAR          ;
    fDoRead: BOOL            ;
begin

  dwSSPIFlags := ISC_REQ_SEQUENCE_DETECT   or ISC_REQ_REPLAY_DETECT     or ISC_REQ_CONFIDENTIALITY   or
                ISC_RET_EXTENDED_ERROR    or ISC_REQ_ALLOCATE_MEMORY   or ISC_REQ_STREAM;


  // Allocate data buffer.
  IoBuffer := PUCHAR(LocalAlloc(LMEM_FIXED, IO_BUFFER_SIZE));
  if(IoBuffer = nil) then begin WriteLn('**** Out of memory (1)'); Exit(SEC_E_INTERNAL_ERROR); end;
  cbIoBuffer := 0;
  fDoRead := fDoInitialRead;



  // Loop until the handshake is finished or an error occurs.
  scRet := SEC_I_CONTINUE_NEEDED;

  while( scRet = SEC_I_CONTINUE_NEEDED) or
       ( scRet = SEC_E_INCOMPLETE_MESSAGE) or
       ( scRet = SEC_I_INCOMPLETE_CREDENTIALS ) do
  begin
    if(0 = cbIoBuffer) or (scRet = SEC_E_INCOMPLETE_MESSAGE) then // Read data from server.
    begin
      if(fDoRead) then
      begin
        cbData := recv(Socket, IoBuffer[cbIoBuffer], IO_BUFFER_SIZE - cbIoBuffer, 0 );
        if(cbData = SOCKET_ERROR) then
        begin
          WriteLn('**** Error ',WSAGetLastError,' reading data from server');
          scRet := SEC_E_INTERNAL_ERROR;
          break;
        end;
        if(cbData = 0) then
        begin
          WriteLn('**** Server unexpectedly disconnected');
          scRet := SEC_E_INTERNAL_ERROR;
          break;
        end;
        WriteLn(cbData, ' bytes of handshake data received');
        if(fVerbose) then
        begin
          PrintHexDump(cbData, PByte(@IoBuffer[cbIoBuffer]));
          WriteLn;
        end;
        Inc(cbIoBuffer, cbData);
      end
      else
        fDoRead := TRUE;
    end;

    // Set up the input buffers. Buffer 0 is used to pass in data
    // received from the server. Schannel will consume some or all
    // of this. Leftover data (if any) will be placed in buffer 1 and
    // given a buffer type of SECBUFFER_EXTRA.
    InBuffers[0].pvBuffer   := IoBuffer;
    InBuffers[0].cbBuffer   := cbIoBuffer;
    InBuffers[0].BufferType := SECBUFFER_TOKEN;

    InBuffers[1].pvBuffer   := nil;
    InBuffers[1].cbBuffer   := 0;
    InBuffers[1].BufferType := SECBUFFER_EMPTY;

    InBuffer.cBuffers       := 2;
    InBuffer.pBuffers       := Addr(InBuffers[0]);
    InBuffer.ulVersion      := SECBUFFER_VERSION;


    // Set up the output buffers. These are initialized to NULL
    // so as to make it less likely we'll attempt to free random
    // garbage later.
    OutBuffers[0].pvBuffer  := nil;
    OutBuffers[0].BufferType:= SECBUFFER_TOKEN;
    OutBuffers[0].cbBuffer  := 0;

    OutBuffer.cBuffers      := 1;
    OutBuffer.pBuffers      := Addr(OutBuffers[0]);
    OutBuffer.ulVersion     := SECBUFFER_VERSION;


    // Call InitializeSecurityContext.
    scRet := g_pSSPI.InitializeSecurityContext(
      phCreds,
      phContext,
      nil,
      dwSSPIFlags,
      0,
      SECURITY_NATIVE_DREP,
      @InBuffer,
      0,
      nil,
      @OutBuffer,
      dwSSPIOutFlags,
      @tsExpiry );


    // If InitializeSecurityContext was successful (or if the error was
    // one of the special extended ones), send the contends of the output
    // buffer to the server.
    if(scRet = SEC_E_OK) or
      (scRet = SEC_I_CONTINUE_NEEDED) or
      (FAILED(scRet) and ((dwSSPIOutFlags and ISC_RET_EXTENDED_ERROR) <> 0)) then
    begin
      if(OutBuffers[0].cbBuffer <> 0) and (OutBuffers[0].pvBuffer <> nil) then
      begin
        cbData := send(Socket, OutBuffers[0].pvBuffer^, OutBuffers[0].cbBuffer, 0 );
        if(cbData = SOCKET_ERROR) or (cbData = 0) then
        begin
          WriteLn('**** Error ',WSAGetLastError,' sending data to server (2)');
          DisplayWinSockError( WSAGetLastError() );
          g_pSSPI.FreeContextBuffer(OutBuffers[0].pvBuffer);
          g_pSSPI.DeleteSecurityContext(phContext);
          Exit(SEC_E_INTERNAL_ERROR);
        end;
        WriteLn(cbData,' bytes of handshake data sent');
        if(fVerbose) then begin PrintHexDump(cbData, OutBuffers[0].pvBuffer); WriteLn; end;

        // Free output buffer.
        g_pSSPI.FreeContextBuffer(OutBuffers[0].pvBuffer);
        OutBuffers[0].pvBuffer := nil;
      end;
    end;



    // If InitializeSecurityContext returned SEC_E_INCOMPLETE_MESSAGE,
    // then we need to read more data from the server and try again.
    if(scRet = SEC_E_INCOMPLETE_MESSAGE) then continue;


    // If InitializeSecurityContext returned SEC_E_OK, then the
    // handshake completed successfully.
    if(scRet = SEC_E_OK) then
    begin
      // If the "extra" buffer contains data, this is encrypted application
      // protocol layer stuff. It needs to be saved. The application layer
      // will later decrypt it with DecryptMessage.
      WriteLn('Handshake was successful');

      if(InBuffers[1].BufferType = SECBUFFER_EXTRA) then
      begin
        pExtraData.pvBuffer := Pointer(LocalAlloc( LMEM_FIXED, InBuffers[1].cbBuffer ));
        if(pExtraData.pvBuffer = nil) then begin WriteLn('**** Out of memory (2)'); Exit(SEC_E_INTERNAL_ERROR); end;

        MoveMemory( pExtraData.pvBuffer,
                    IoBuffer + (cbIoBuffer - InBuffers[1].cbBuffer),
                    InBuffers[1].cbBuffer );

        pExtraData.cbBuffer   := InBuffers[1].cbBuffer;
        pExtraData.BufferType := SECBUFFER_TOKEN;

        WriteLn( pExtraData.cbBuffer, ' bytes of app data was bundled with handshake data');
      end
      else
      begin
          pExtraData.pvBuffer   := nil;
          pExtraData.cbBuffer   := 0;
          pExtraData.BufferType := SECBUFFER_EMPTY;
      end;
      break; // Bail out to quit
    end;

    // Check for fatal error.
    if(FAILED(scRet)) then begin WriteLn('**** Error 0x',IntToHex(scRet),' returned by InitializeSecurityContext (2)'); break; end;

    // If InitializeSecurityContext returned SEC_I_INCOMPLETE_CREDENTIALS,
    // then the server just requested client authentication.
    if(scRet = SEC_I_INCOMPLETE_CREDENTIALS) then
    begin
      // Busted. The server has requested client authentication and
      // the credential we supplied didn't contain a client certificate.
      // This function will read the list of trusted certificate
      // authorities ("issuers") that was received from the server
      // and attempt to find a suitable client certificate that
      // was issued by one of these. If this function is successful,
      // then we will connect using the new certificate. Otherwise,
      // we will attempt to connect anonymously (using our current credentials).
      GetNewClientCredentials(phCreds, phContext);

      // Go around again.
      fDoRead := FALSE;
      scRet := SEC_I_CONTINUE_NEEDED;
      continue;
    end;

    // Copy any leftover data from the "extra" buffer, and go around again.
    if ( InBuffers[1].BufferType = SECBUFFER_EXTRA ) then
    begin
      MoveMemory( IoBuffer, IoBuffer + (cbIoBuffer - InBuffers[1].cbBuffer), InBuffers[1].cbBuffer );
      cbIoBuffer := InBuffers[1].cbBuffer;
    end
    else
      cbIoBuffer := 0;
  end;

  // Delete the security context in the case of a fatal error.
  if(FAILED(scRet))
    then g_pSSPI.DeleteSecurityContext(phContext);
  LocalFree(IoBuffer);

  Result := scRet;
end;


(*****************************************************************************)
function PerformClientHandshake(
        Socket: TSOCKET;        // in
        phCreds: PCredHandle;       // in
  const pszServerName: string; // in
        phContext: pCtxtHandle ;     // out
        pExtraData: pSecBuffer )   // out
: SECURITY_STATUS;
var

    OutBuffer: TSecBufferDesc   ;
    OutBuffers: array[0..0] of TSecBuffer;
    dwSSPIFlags, dwSSPIOutFlags, cbData: DWORD;
    tsExpiry: TimeStamp       ;
    scRet: SECURITY_STATUS;
begin

  dwSSPIFlags := ISC_REQ_SEQUENCE_DETECT   or ISC_REQ_REPLAY_DETECT     or ISC_REQ_CONFIDENTIALITY   or
                ISC_RET_EXTENDED_ERROR    or ISC_REQ_ALLOCATE_MEMORY   or ISC_REQ_STREAM;


  //  Initiate a ClientHello message and generate a token.
  OutBuffers[0].pvBuffer   := nil;
  OutBuffers[0].BufferType := SECBUFFER_TOKEN;
  OutBuffers[0].cbBuffer   := 0;

  OutBuffer.cBuffers  := 1;
  OutBuffer.pBuffers  := Addr(OutBuffers[0]);
  OutBuffer.ulVersion := SECBUFFER_VERSION;

  scRet := g_pSSPI.InitializeSecurityContext(
    phCreds,
    nil,
    PChar(pszServerName),
    dwSSPIFlags,
    0,
    SECURITY_NATIVE_DREP,
    nil,
    0,
    phContext,
   @OutBuffer,
    dwSSPIOutFlags,
   @tsExpiry );

  if(scRet <> SEC_I_CONTINUE_NEEDED) then begin WriteLn('**** Error ',scRet,' returned by InitializeSecurityContext (1)'); Exit(scRet); end;

  // Send response to server if there is one.
  if(OutBuffers[0].cbBuffer <> 0) and (OutBuffers[0].pvBuffer <> nil) then
  begin
      cbData := send( Socket, OutBuffers[0].pvBuffer^, OutBuffers[0].cbBuffer, 0 );
      if( cbData = SOCKET_ERROR) or (cbData = 0 ) then
      begin
          WriteLn('**** Error ',WSAGetLastError,' sending data to server (1)');
          g_pSSPI.FreeContextBuffer(OutBuffers[0].pvBuffer);
          g_pSSPI.DeleteSecurityContext(phContext);
          Exit(SEC_E_INTERNAL_ERROR);
      end;
      WriteLn(cbData, ' bytes of handshake data sent');
      if(fVerbose) then begin PrintHexDump(cbData, OutBuffers[0].pvBuffer); WriteLn; end;
      g_pSSPI.FreeContextBuffer(OutBuffers[0].pvBuffer); // Free output buffer.
      OutBuffers[0].pvBuffer := nil;
  end;

  Result := ClientHandshakeLoop(Socket, phCreds, phContext, TRUE, pExtraData);
end;



(*****************************************************************************)
function EncryptSend( Socket: TSOCKET; phContext: pCtxtHandle; pbIoBuffer: PBYTE; Sizes : TSecPkgContextStreamSizes): DWORD;
// http://msdn.microsoft.com/en-us/library/aa375378(VS.85).aspx
// The encrypted message is encrypted in place, overwriting the original contents of its buffer.
var
  scRet: SECURITY_STATUS    ;            // unsigned long cbBuffer;    // Size of the buffer, in bytes
  Message: TSecBufferDesc        ;        // unsigned long BufferType;  // Type of the buffer (below)
  Buffers: array[0..3] of TSecBuffer;    // void    SEC_FAR * pvBuffer;   // Pointer to the buffer
  cbMessage, cbData: DWORD;
  pbMessage: PAnsiChar;
begin

  pbMessage := PAnsiChar(@pbIoBuffer[Sizes.cbHeader]); // Offset by "header size"
  cbMessage := strlen(pbMessage);
      Writeln('Sending ',pbMessage,' bytes of plaintext:'); PrintText(cbMessage, PByte(pbMessage));
  if(fVerbose) then begin PrintHexDump(cbMessage, PByte(pbMessage)); WriteLn; end;


      // Encrypt the HTTP request.
  Buffers[0].pvBuffer     := pbIoBuffer;                                // Pointer to buffer 1
  Buffers[0].cbBuffer     := Sizes.cbHeader;                        // length of header
  Buffers[0].BufferType   := SECBUFFER_STREAM_HEADER;    // Type of the buffer

  Buffers[1].pvBuffer     := pbMessage;                                // Pointer to buffer 2
  Buffers[1].cbBuffer     := cbMessage;                                // length of the message
  Buffers[1].BufferType   := SECBUFFER_DATA;                        // Type of the buffer

  Buffers[2].pvBuffer     := pbMessage + cbMessage;        // Pointer to buffer 3
  Buffers[2].cbBuffer     := Sizes.cbTrailer;                    // length of the trailor
  Buffers[2].BufferType   := SECBUFFER_STREAM_TRAILER;    // Type of the buffer

  Buffers[3].pvBuffer     := nil;//SECBUFFER_EMPTY;                    // Pointer to buffer 4
  Buffers[3].cbBuffer     := SECBUFFER_EMPTY;                    // length of buffer 4
  Buffers[3].BufferType   := SECBUFFER_EMPTY;                    // Type of the buffer 4


  Message.ulVersion       := SECBUFFER_VERSION;    // Version number
  Message.cBuffers        := 4;                                    // Number of buffers - must contain four SecBuffer structures.
  Message.pBuffers        := Addr(Buffers[0]);                        // Pointer to array of buffers
  scRet := g_pSSPI.EncryptMessage(phContext, 0, @Message, 0); // must contain four SecBuffer structures.
  if(FAILED(scRet)) then begin WriteLn('**** Error ',scRet,' returned by EncryptMessage'); Exit(scRet); end;


  // Send the encrypted data to the server.                                            len                                                                         flags
  cbData := send( Socket, pbIoBuffer^,    Buffers[0].cbBuffer + Buffers[1].cbBuffer + Buffers[2].cbBuffer,    0 );

  WriteLn(cbData, ' bytes of encrypted data sent');
  if(fVerbose) then begin PrintHexDump(cbData, pbIoBuffer); WriteLn; end;

  Result := cbData; // send( Socket, pbIoBuffer,    Sizes.cbHeader + strlen(pbMessage) + Sizes.cbTrailer,  0 );

end;


(*****************************************************************************)
function ReadDecrypt( Socket: TSOCKET; phCreds: PCredHandle; phContext: pCtxtHandle; pbIoBuffer: PBYTE; cbIoBufferLength : DWORD): SECURITY_STATUS;

// calls recv() - blocking socket read
// http://msdn.microsoft.com/en-us/library/ms740121(VS.85).aspx

// The encrypted message is decrypted in place, overwriting the original contents of its buffer.
// http://msdn.microsoft.com/en-us/library/aa375211(VS.85).aspx

var
  ExtraBuffer: TSecBuffer                ;
  pDataBuffer, pExtraBuffer: pSecBuffer                ;

  scRet: SECURITY_STATUS    ;            // unsigned long cbBuffer;    // Size of the buffer, in bytes
  Message: TSecBufferDesc        ;        // unsigned long BufferType;  // Type of the buffer (below)
  Buffers: array[0..3] of TSecBuffer                ;    // void    SEC_FAR * pvBuffer;   // Pointer to the buffer

  cbIoBuffer, cbData, length: DWORD                        ;
  buff: PBYTE                        ;
  i: Integer;
begin
  // Read data from server until done.
  cbIoBuffer := 0;
  scRet := 0;
  while(TRUE) do // Read some data.
  begin
    if( cbIoBuffer = 0) or (scRet = SEC_E_INCOMPLETE_MESSAGE ) then// get the data
    begin
      cbData := recv(Socket, pbIoBuffer[cbIoBuffer], cbIoBufferLength - cbIoBuffer, 0);
      if(cbData = SOCKET_ERROR) then
      begin
        WriteLn('**** Error ',WSAGetLastError,' reading data from server');
        scRet := SEC_E_INTERNAL_ERROR;
        break;
      end;
      if(cbData = 0) then // Server disconnected.
      begin
        if(cbIoBuffer <> 0) then
        begin
          WriteLn('**** Server unexpectedly disconnected');
          scRet := SEC_E_INTERNAL_ERROR;
          Exit(scRet);
        end;
        break; // All Done
      end;
      // success
      WriteLn(cbData, ' bytes of (encrypted) application data received');
      if(fVerbose) then
      begin
        PrintHexDump(cbData, pbIoBuffer + cbIoBuffer);
        WriteLn;
      end;
      Inc(cbIoBuffer, cbData);
    end;


    // Decrypt the received data.
    Buffers[0].pvBuffer     := pbIoBuffer;
    Buffers[0].cbBuffer     := cbIoBuffer;
    Buffers[0].BufferType   := SECBUFFER_DATA;  // Initial Type of the buffer 1
    Buffers[1].BufferType   := SECBUFFER_EMPTY; // Initial Type of the buffer 2
    Buffers[2].BufferType   := SECBUFFER_EMPTY; // Initial Type of the buffer 3
    Buffers[3].BufferType   := SECBUFFER_EMPTY; // Initial Type of the buffer 4

    Message.ulVersion       := SECBUFFER_VERSION;    // Version number
    Message.cBuffers        := 4;                                    // Number of buffers - must contain four SecBuffer structures.
    Message.pBuffers        := Addr(Buffers[0]);                        // Pointer to array of buffers

    scRet := g_pSSPI.DecryptMessage(phContext, @Message, 0, nil);
    if( scRet = SEC_I_CONTEXT_EXPIRED ) then break; // Server signalled end of session
//      if( scRet = SEC_E_INCOMPLETE_MESSAGE - Input buffer has partial encrypted record, read more
    if( scRet <> SEC_E_OK) and
      ( scRet <> SEC_I_RENEGOTIATE) and
      ( scRet <> SEC_I_CONTEXT_EXPIRED ) then
    begin
      WriteLn('**** DecryptMessage ');
      DisplaySECError(scRet);
      Exit(scRet);
    end;



    // Locate data and (optional) extra buffers.
    pDataBuffer  := nil;
    pExtraBuffer := nil;
    for i := 1 to 3 do
    begin
      if( pDataBuffer  = nil) and (Buffers[i].BufferType = SECBUFFER_DATA  ) then pDataBuffer  := @Buffers[i];
      if( pExtraBuffer = nil) and (Buffers[i].BufferType = SECBUFFER_EXTRA ) then pExtraBuffer := @Buffers[i];
    end;


    // Display the decrypted data.
    if(pDataBuffer <> nil) then
    begin
      length := pDataBuffer.cbBuffer;
      if( length <> 0 ) then // check if last two chars are CR LF
      begin
        buff := pDataBuffer.pvBuffer; // printf( "n-2= %d, n-1= %d \n", buff[length-2], buff[length-1] );
        WriteLn('Decrypted data: ',length,' bytes');
        PrintText( length, buff );
        if(fVerbose) then
        begin
          PrintHexDump(length, buff);
          WriteLn;
        end;
        if( buff[length-2] = 13) and (buff[length-1] = 10 ) then
          break; // printf("Found CRLF\n");
      end;
    end;

    // Move any "extra" data to the input buffer.
    if(pExtraBuffer <> nil) then
    begin
      MoveMemory(pbIoBuffer, pExtraBuffer.pvBuffer, pExtraBuffer.cbBuffer);
      cbIoBuffer := pExtraBuffer.cbBuffer; // printf("cbIoBuffer= %d  \n", cbIoBuffer);
    end
    else
    cbIoBuffer := 0;

    // The server wants to perform another handshake sequence.
    if(scRet = SEC_I_RENEGOTIATE) then
    begin
      WriteLn('Server requested renegotiate!');
      scRet := ClientHandshakeLoop( Socket, phCreds, phContext, FALSE, @ExtraBuffer);
      if(scRet <> SEC_E_OK) then Exit(scRet);

      if(ExtraBuffer.pvBuffer <> nil) then // Move any "extra" data to the input buffer.
      begin
          MoveMemory(pbIoBuffer, ExtraBuffer.pvBuffer, ExtraBuffer.cbBuffer);
          cbIoBuffer := ExtraBuffer.cbBuffer;
      end;
    end;
  end; // Loop till CRLF is found at the end of the data

  Result := SEC_E_OK;
end;

// tiny sprintf
procedure sprintf(target, source: PAnsiChar);
begin
  repeat
    target^ := source^;
    inc(target);
    if source^ = #0 then
      Break;
    inc(source);
  until False;
end;


(*****************************************************************************)
function SMTPsession(
  Socket :TSOCKET;               // in
  phCreds: PCredHandle;    // in
  phContext: pCtxtHandle)  // in
: SECURITY_STATUS;
var
  Sizes:TSecPkgContextStreamSizes ;            // unsigned long cbBuffer;    // Size of the buffer, in bytes
  scRet: SECURITY_STATUS                        ;            // unsigned long BufferType;  // Type of the buffer (below)
  pbIoBuffer: PBYTE                                            ; // void    SEC_FAR * pvBuffer;   // Pointer to the buffer
  cbIoBufferLength, cbData: DWORD                                            ;
begin

  // Read stream encryption properties.
  scRet := g_pSSPI.QueryContextAttributes( phContext, SECPKG_ATTR_STREAM_SIZES, @Sizes );
  if(scRet <> SEC_E_OK) then
  begin
    WriteLn('**** Error ',scRet,' reading SECPKG_ATTR_STREAM_SIZES');
    Exit(scRet);
  end;

  // Create a buffer.
  cbIoBufferLength := Sizes.cbHeader  +  Sizes.cbMaximumMessage  +  Sizes.cbTrailer;
  pbIoBuffer       := PByte(LocalAlloc(LMEM_FIXED, cbIoBufferLength));
  if(pbIoBuffer = nil) then
  begin
    WriteLn('**** Out of memory (2)');
    Exit(SEC_E_INTERNAL_ERROR);
  end;

  // Receive a Response
  scRet := ReadDecrypt( Socket, phCreds, phContext, pbIoBuffer, cbIoBufferLength );
  if( scRet <> SEC_E_OK ) then
    Exit(scRet);

  // Build the request - must be < maximum message size
  sprintf( PAnsiChar(@pbIoBuffer[Sizes.cbHeader]), 'EHLO smtp.gmail.com'#13#10 ); // message begins after the header

  // Send a request.
  cbData := EncryptSend( Socket, phContext, pbIoBuffer, Sizes );
  if(cbData = SOCKET_ERROR) or (cbData = 0) then
  begin
    WriteLn('**** Error ',WSAGetLastError,' sending data to server (3)');
    Exit(SEC_E_INTERNAL_ERROR);
  end;

  // Receive a Response
  scRet := ReadDecrypt( Socket, phCreds, phContext, pbIoBuffer, cbIoBufferLength );
  if( scRet <> SEC_E_OK ) then
    Exit(scRet);

  // Build the request - must be < maximum message size
  sprintf( PAnsiChar(@pbIoBuffer[Sizes.cbHeader]), 'QUIT'#13#10 ); // message begins after the header

  // Send a request.
  cbData := EncryptSend( Socket, phContext, pbIoBuffer, Sizes );
  if(cbData = SOCKET_ERROR) or (cbData = 0) then
  begin
    WriteLn('**** Error ',WSAGetLastError,' sending data to server (3)');
    Exit(SEC_E_INTERNAL_ERROR);
  end;


  // Receive a Response
  scRet := ReadDecrypt( Socket, phCreds, phContext, pbIoBuffer, cbIoBufferLength );
  if( scRet <> SEC_E_OK ) then
    Exit(scRet);


  Result := SEC_E_OK;
end;


(*****************************************************************************)
//void _cdecl main( int argc, char *argv[] )
var
  WsaData: TWSADATA ;
  Socket: TSOCKET   = INVALID_SOCKET;

  hClientCreds: TCredHandle ;
  hContext: TCtxtHandle ;
  fCredsInitialized: BOOL    = FALSE;
  fContextInitialized: BOOL  = FALSE;

  ExtraData: TSecBuffer  ;
  Status: SECURITY_STATUS ;

  pRemoteCertContext: PCCERT_CONTEXT  = nil;
label
  cleanup;
begin

  if(not LoadSecurityLibrary() ) then
  begin
    WriteLn('Error initializing the security library');
    goto cleanup;
  end; //
  WriteLn('----- SSPI Initialized');


  // Initialize the WinSock subsystem.
  if(WSAStartup($0101, &WsaData) = SOCKET_ERROR) then// Winsock.h
  begin
    WriteLn('Error ',GetLastError,' returned by WSAStartup');
    goto cleanup;
  end; //
  WriteLn('----- WinSock Initialized');


  // Create credentials.
  if(0 <> CreateCredentials(pszUser, @hClientCreds)) then
  begin
    WriteLn('Error creating credentials');
    goto cleanup;
  end;
  fCredsInitialized := TRUE; //
  WriteLn('----- Credentials Initialized');

  // Connect to server.
  if(0 <> ConnectToServer(pszServerName, iPortNumber, &Socket)) then
  begin
    WriteLn('Error connecting to server');
    goto cleanup;
  end; //
  WriteLn('----- Connectd To Server');

  // Perform handshake
  if(0 <> PerformClientHandshake( Socket, @hClientCreds, pszServerName, @hContext, @ExtraData ) ) then
  begin
    WriteLn('Error performing handshake');
    goto cleanup;
  end;
  fContextInitialized := TRUE; //
  WriteLn('----- Client Handshake Performed');

  // Authenticate server's credentials. Get server's certificate.
  Status := g_pSSPI.QueryContextAttributes( @hContext, SECPKG_ATTR_REMOTE_CERT_CONTEXT, @pRemoteCertContext );
  if(Status <> SEC_E_OK) then
  begin
    WriteLn('Error ',Status,' querying remote certificate');
    goto cleanup;
  end; //
  WriteLn('----- Server Credentials Authenticated');

  // Display server certificate chain.
  DisplayCertChain( pRemoteCertContext, FALSE ); //
  WriteLn('----- Certificate Chain Displayed ');

  // Attempt to validate server certificate.
  Status := VerifyServerCertificate( pRemoteCertContext, pszServerName, 0 );
  if(0 <> Status) then
  begin
    WriteLn('**** Error ',Status,' authenticating server credentials!');
    goto cleanup;
  end;
  // The server certificate did not validate correctly. At this point, we cannot tell
  // if we are connecting to the correct server, or if we are connecting to a
  // "man in the middle" attack server - Best to just abort the connection.
  WriteLn('----- Server Certificate Verified');

  // Free the server certificate context.
  CertFreeCertificateContext(pRemoteCertContext);
  pRemoteCertContext := nil; //
  WriteLn('----- Server certificate context released ');

  // Display connection info.
  DisplayConnectionInfo(@hContext); //
  WriteLn('----- Secure Connection Info');

  // Send Request, recover response. LPSTR pszRequest = "EHLO";
  if(0 <>  SMTPsession( Socket, @hClientCreds, @hContext ) ) then
  begin
    WriteLn('Error SMTP Session ');
    goto cleanup;
  end; //
  WriteLn('----- SMTP session Complete ');

  // Send a close_notify alert to the server and close down the connection.
  if(0 <> DisconnectFromServer(Socket, @hClientCreds, @hContext)) then
  begin
    WriteLn('Error disconnecting from server');
    goto cleanup;
  end;
  fContextInitialized := FALSE;
  Socket := INVALID_SOCKET; //
  WriteLn('----- Disconnected From Server');

cleanup: //
 WriteLn('----- Begin Cleanup');

  // Free the server certificate context.
  if(pRemoteCertContext <> nil) then
  begin
    CertFreeCertificateContext(pRemoteCertContext);
    pRemoteCertContext := nil;
  end;

  // Free SSPI context handle.
  if(fContextInitialized) then
  begin
    g_pSSPI.DeleteSecurityContext(@hContext);
    fContextInitialized := FALSE;
  end;

  // Free SSPI credentials handle.
  if(fCredsInitialized) then
  begin
    g_pSSPI.FreeCredentialsHandle(@hClientCreds);
    fCredsInitialized := FALSE;
  end;

  // Close socket.
  if(Socket <> INVALID_SOCKET) then closesocket(Socket);

  // Shutdown WinSock subsystem.
  WSACleanup();

  // Close "MY" certificate store.
  if(hMyCertStore <> 0) then CertCloseStore(hMyCertStore, 0);

  UnloadSecurityLibrary();

  WriteLn('----- All Done ----- ');


  ReadLn;
end.
