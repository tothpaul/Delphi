unit Execute.WinSSPI;
{
  SChannel (SSPI + CryptoLib) (c)2017-2019 Execute SARL

  partialy based on wcrypt2 (http://delphi-jedi.org)
}
interface

uses
  Winapi.Windows;

type
  ALG_ID = Cardinal;

  HCERTSTORE = type THandle;
  HCRYPTMSG = type THandle;
  HCERTCHAINENGINE = type THandle;
  HCRYPTPROV = type THandle;
  HCRYPTHASH = type THandle;
  HCRYPTKEY = type THandle;
  HCRYPTPROV_OR_NCRYPT_KEY_HANDLE = THandle;

  CRYPTOAPI_BLOB = record
    cbData :DWORD;
    pbData :PAnsiChar; // PBYTE but PAnsiChar is better for debug purpose
  end;
  CRYPT_DATA_BLOB    = CRYPTOAPI_BLOB;
  CRYPT_ATTR_BLOB    = CRYPTOAPI_BLOB;
  CRYPT_INTEGER_BLOB = CRYPTOAPI_BLOB;
  CRYPT_OBJID_BLOB   = CRYPTOAPI_BLOB;
  CERT_NAME_BLOB     = CRYPTOAPI_BLOB;

  PCRYPT_ATTR_BLOB   = ^CRYPT_ATTR_BLOB;
  PCERT_NAME_BLOB    = ^CERT_NAME_BLOB;

  CRYPT_ATTRIBUTE = record
    pszObjId : LPSTR;
    cValue   : DWORD;
    rgValue  : PCRYPT_ATTR_BLOB;
  end;
  PCRYPT_ATTRIBUTE = ^CRYPT_ATTRIBUTE;

  CRYPT_ALGORITHM_IDENTIFIER = record
    pszObjId   :LPSTR;
    Parameters :CRYPT_OBJID_BLOB;
  end;

  CRYPT_BIT_BLOB = record
    cbData      :DWORD;
    pbData      :PANSICHAR; // PBYTE
    cUnusedBits :DWORD;
  end;

  CERT_PUBLIC_KEY_INFO = record
    Algorithm :CRYPT_ALGORITHM_IDENTIFIER;
    PublicKey :CRYPT_BIT_BLOB;
  end;
  PCERT_PUBLIC_KEY_INFO = ^CERT_PUBLIC_KEY_INFO;


  CERT_EXTENSION = record
    pszObjId :LPSTR;
    fCritical :BOOL;
    Value :CRYPT_OBJID_BLOB;
  end;
  PCERT_EXTENSION = ^CERT_EXTENSION;

  CERT_INFO = record
    dwVersion              :DWORD;
    SerialNumber           :CRYPT_INTEGER_BLOB;
    SignatureAlgorithm     :CRYPT_ALGORITHM_IDENTIFIER;
    Issuer                 :CERT_NAME_BLOB;
    NotBefore              :TFILETIME;
    NotAfter               :TFILETIME;
    Subject                :CERT_NAME_BLOB;
    SubjectPublicKeyInfo   :CERT_PUBLIC_KEY_INFO;
    IssuerUniqueId         :CRYPT_BIT_BLOB;
    SubjectUniqueId        :CRYPT_BIT_BLOB;
    cExtension             :DWORD;
    rgExtension            :PCERT_EXTENSION;
  end;
  PCERT_INFO = ^CERT_INFO;

  CERT_CONTEXT = record
    dwCertEncodingType :DWORD;
    pbCertEncoded :PBYTE;
    cbCertEncoded :DWORD;
    pCertInfo :PCERT_INFO;
    hCertStore :HCERTSTORE;
    function FindExtension(OID: PAnsiChar): PCERT_EXTENSION;
  end;
  PCCERT_CONTEXT = ^CERT_CONTEXT;
  PPCCERT_CONTEXT = ^PCCERT_CONTEXT;
  SCHANNEL_CRED = record
    dwVersion: DWORD;
    cCreds: DWORD;
    paCred: ^PCCERT_CONTEXT;
    hRootStore: HCERTSTORE;
    cMappers: DWORD;
    aphMappers: Pointer;
    cSupportedAlgs: DWORD;
    palgSupportedAlgs: ^ALG_ID;
    grbitEnabledProtocols: DWORD;
    dwMinimumCipherStrength: DWORD;
    dwMaximumCipherStrength: DWORD;
    dwSessionLifespan: DWORD;
    dwFlags: DWORD;
    dwCredFormat: DWORD;
  end;

  HTTPSPolicyCallbackData = record
    //union {
    //cbStruct: DWORD;
      cbSize: DWORD;
    //};
    dwAuthType: DWORD;
    fdwChecks: DWORD;
    pwszServerName: PWCHAR;
  end;

  CERT_CHAIN_POLICY_PARA = record
    cbSize: DWORD;
    dwFlags: DWORD;
    pvExtraPolicyPara: Pointer;
  end;
  PCERT_CHAIN_POLICY_PARA = ^CERT_CHAIN_POLICY_PARA;

  CERT_CHAIN_POLICY_STATUS = record
    cbSize: DWORD;
    dwError: DWORD;
    lChainIndex: LONG;
    lElementIndex: LONG;
    pvExtraPolicyStatus: Pointer;
  end;
  PCERT_CHAIN_POLICY_STATUS = ^CERT_CHAIN_POLICY_STATUS;

  CTL_USAGE = record
    cUsageIdentifier: DWORD;
    rgpszUsageIdentifier: LPSTR;
  end;

  CERT_ENHKEY_USAGE = CTL_USAGE;
  PCERT_ENHKEY_USAGE = ^CERT_ENHKEY_USAGE;

  CERT_USAGE_MATCH = record
    dwType: DWORD;
    Usage: CERT_ENHKEY_USAGE;
  end;

  CERT_CHAIN_PARA = record
    cbSize: DWORD;
    RequestedUsage: CERT_USAGE_MATCH;
  end;
  PCERT_CHAIN_PARA = ^CERT_CHAIN_PARA;

  CERT_TRUST_STATUS = record
    dwErrorStatus: DWORD;
    dwInfoStatus: DWORD;
  end;

  CERT_REVOCATION_INFO = record
    cbSize: DWORD;
    dwRevocationResult: DWORD;
    pszRevocationOid: LPCSTR;
    pvOidSpecificInfo: LPVOID;
  end;
  PCERT_REVOCATION_INFO = ^CERT_REVOCATION_INFO;

  CERT_CHAIN_ELEMENT = record
    cbSize: DWORD;
    pCertContext: PCCERT_CONTEXT;
    TrustStatus: CERT_TRUST_STATUS;
    pRevocationInfo: PCERT_REVOCATION_INFO;
    pIssuanceUsage: PCERT_ENHKEY_USAGE;
    pApplicationUsage: PCERT_ENHKEY_USAGE;
    pwszExtendedErrorInfo: PChar;
  end;
  PCERT_CHAIN_ELEMENT = ^CERT_CHAIN_ELEMENT;

  CTL_ENTRY = record
    SubjectIdentifier: CRYPT_DATA_BLOB; // For instance, it's hash
    cAttribute: DWORD;
    rgAttribute: PCRYPT_ATTRIBUTE; // OPTIONAL
  end;
  PCTL_ENTRY = ^CTL_ENTRY;


  CTL_INFO = record
    dwVersion           :DWORD;
    SubjectUsage        :CTL_USAGE;
    ListIdentifier      :CRYPT_DATA_BLOB;     // OPTIONAL
    SequenceNumber      :CRYPT_INTEGER_BLOB;  // OPTIONAL
    ThisUpdate          :TFILETIME;
    NextUpdate          :TFILETIME;           // OPTIONAL
    SubjectAlgorithm    :CRYPT_ALGORITHM_IDENTIFIER;
    cCTLEntry           :DWORD;
    rgCTLEntry          :PCTL_ENTRY;          // OPTIONAL
    cExtension          :DWORD;
    rgExtension         :PCERT_EXTENSION;     // OPTIONAL
  end;
  PCTL_INFO = ^CTL_INFO;

  CTL_CONTEXT = record
    dwMsgAndCertEncodingType :DWORD;
    pbCtlEncoded :PBYTE;
    cbCtlEncoded :DWORD;
    pCtlInfo :PCTL_INFO;
    hCertStore :HCERTSTORE;
    hCryptMsg :HCRYPTMSG;
    pbCtlContent :PBYTE;
    cbCtlContent :DWORD;
  end;
  PCCTL_CONTEXT = ^CTL_CONTEXT;

  CERT_TRUST_LIST_INFO = record
    cbSize: DWORD;
    pCtlEntry: PCTL_ENTRY;
    pCtlContext: PCCTL_CONTEXT;
  end;
  PCERT_TRUST_LIST_INFO = ^CERT_TRUST_LIST_INFO;

  CERT_SIMPLE_CHAIN = record
    cbSize: DWORD;
    TrustStatus: CERT_TRUST_STATUS;
    cElement: DWORD;
    rgpElement: ^PCERT_CHAIN_ELEMENT;
    pTrustListInfo: PCERT_TRUST_LIST_INFO;
  end;

  PCERT_SIMPLE_CHAIN = ^CERT_SIMPLE_CHAIN;

  PCCERT_CHAIN_CONTEXT = ^CERT_CHAIN_CONTEXT;

  PPCERT_SIMPLE_CHAIN= ^PCERT_SIMPLE_CHAIN;

  CERT_CHAIN_CONTEXT = record // 56 bytes
    cbSize     : DWORD;
    TrustStatus: CERT_TRUST_STATUS;
    cChain     : DWORD;
    rgpChain   : PPCERT_SIMPLE_CHAIN; // warning ! ^^CERT_SIMPLE_CHAIN
    // Following is returned when CERT_CHAIN_RETURN_LOWER_QUALITY_CONTEXTS
    // is set in dwFlags
    cLowerQualityChainContext: DWORD;
    rgpLowerQualityChainContext: ^PCCERT_CHAIN_CONTEXT;
    // fHasRevocationFreshnessTime is only set if we are able to retrieve
    // revocation information for all elements checked for revocation.
    // For a CRL its CurrentTime - ThisUpdate.
    //
    // dwRevocationFreshnessTime is the largest time across all elements
    // checked.
    fHasRevocationFreshnessTime: BOOL;
    dwRevocationFreshnessTime  : DWORD;
    // Flags passed when created via CertGetCertificateChain
    dwCreationFlags : DWORD;
    // Following is updated with unique Id when the chain context is logged.
    ChainId: TGUID;
  end;

// SSPI
  SECURITY_STATUS = LONG;

  SECURITY_INTEGER = TLargeInteger;
  PSECURITY_INTEGER = ^SECURITY_INTEGER;
  TimeStamp = SECURITY_INTEGER;
  PTimeStamp = ^TimeStamp;

  TSecHandle = record
    dwLower: ULONG_PTR;
    dwUpper: ULONG_PTR;
  end;
  TCredHandle = TSecHandle;
  TCtxtHandle = TSecHandle;
  PSecHandle  = ^TSecHandle;
  PCredHandle = PSecHandle;
  PCtxtHandle = PSecHandle;

  TSecPkgInfo = record
    fCapabilities: Cardinal;        // Capability bitmask
    wVersion: Word;                 // Version of driver
    wRPCID: Word;                   // ID for RPC Runtime
    cbMaxToken: Cardinal;           // Size of authentication token (max)
    Name: PChar;                    // Text name
    Comment: PChar;                 // Comment
  end;
  PSecPkgInfo = ^TSecPkgInfo;

  TSecBuffer = record
    cbBuffer  : Cardinal;             // Size of the buffer, in bytes
    BufferType: Cardinal;           // Type of the buffer (below)
    pvBuffer  : Pointer;              // Pointer to the buffer
  end;
  PSecBuffer = ^TSecBuffer;

  TSecBufferDesc = record
    ulVersion: Cardinal;            // Version number
    cBuffers: Cardinal;             // Number of buffers
    pBuffers: ^PSecBuffer;           // Pointer to array of buffers
  end;
  PSecBufferDesc = ^TSecBufferDesc;

  TSecPkgContextStreamSizes = record
    cbHeader: Cardinal;
    cbTrailer: Cardinal;
    cbMaximumMessage: Cardinal;
    cBuffers: Cardinal;
    cbBlockSize: Cardinal;
  end;

  TSecGetKeyFn = procedure(
    Arg: Pointer;                 // Argument passed in
    Principal: Pointer;           // Principal ID
    KeyVer: Cardinal;             // Key Version
    out Key: Pointer;             // Returned ptr to key
    out Status: SECURITY_STATUS   // returned status
    ); stdcall;

  TSecurityFunctionTable = record

    dwVersion: Cardinal;

    EnumerateSecurityPackages: function(
      out pcPackages: Cardinal;
      out ppPackageInfo: PSecPkgInfo): SECURITY_STATUS; stdcall;

    QueryCredentialsAttributes: function(
      phCredential: PCredHandle;
      ulAttribute: Cardinal;
      pBuffer: Pointer): SECURITY_STATUS; stdcall;

    AcquireCredentialsHandle: function(
      pszPrincipal: PChar;
      pszPackage: PChar;
      fCredentialUse: Cardinal;
      pvLogonId: Pointer;
      pAuthData: Pointer;
      pGetKeyFn: TSecGetKeyFn;
      pvGetKeyArgument: Pointer;
      phCredential: PCredHandle;
      ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;

    FreeCredentialsHandle: function(
      phCredential: PCredHandle): SECURITY_STATUS; stdcall;

    Reserved2: Pointer;

    InitializeSecurityContext: function(
      phCredential: PCredHandle;
      phContext: PCtxtHandle;
      pszTargetName: PChar;
      fContextReq: Cardinal;
      Reserved1: Cardinal;
      TargetDataRep: Cardinal;
      pInput: PSecBufferDesc;
      Reserved2: Cardinal;
      phNewContext: PCtxtHandle;
      pOutput: PSecBufferDesc;
      out pfContextAttr: Cardinal;
      ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;

    AcceptSecurityContext: function(
      phCredential: PCredHandle;
      phContext: PCtxtHandle;
      pInput: PSecBufferDesc;
      fContextReq: Cardinal;
      TargetDataRep: Cardinal;
      phNewContext: PCtxtHandle;
      pOutput: PSecBufferDesc;
      out pfContextAttr: Cardinal;
      ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;

    CompleteAuthToken: function(
      phContext: PCtxtHandle;
      pToken: PSecBufferDesc): SECURITY_STATUS; stdcall;

    DeleteSecurityContext: function(
      phContext: PCtxtHandle): SECURITY_STATUS; stdcall;

    ApplyControlToken: function(
      phContext: PCtxtHandle;
      pInput: PSecBufferDesc): SECURITY_STATUS; stdcall;

    QueryContextAttributes: function(
      phContext: PCtxtHandle;
      ulAttribute: Cardinal;
      pBuffer: Pointer): SECURITY_STATUS; stdcall;

    ImpersonateSecurityContext: function(
      phContext: PCtxtHandle): SECURITY_STATUS; stdcall;

    RevertSecurityContext: function(
      phContext: PCtxtHandle): SECURITY_STATUS; stdcall;

    MakeSignature: function(
      phContext: PCtxtHandle;
      fQOP: Cardinal;
      pMessage: PSecBufferDesc;
      MessageSeqNo: Cardinal): SECURITY_STATUS; stdcall;

    VerifySignature: function(
      phContext: PCtxtHandle;
      pMessage: PSecBufferDesc;
      MessageSeqNo: Cardinal;
      out pfQOP: Cardinal): SECURITY_STATUS; stdcall;

    FreeContextBuffer: function(
      pvContextBuffer: PVOID): SECURITY_STATUS; stdcall;

    QuerySecurityPackageInfo: function(
      pszPackageName: PChar;
      out ppPackageInfo: PSecPkgInfo): SECURITY_STATUS; stdcall;

    Reserved3: Pointer;
    Reserved4: Pointer;

    ExportSecurityContext: function(
      phContext: PCtxtHandle;
      fFlags: ULONG;
      pPackedContext: PSecBuffer;
      out pToken: PPointer
      ): SECURITY_STATUS; stdcall;

    ImportSecurityContext: function(
      pszPackage: PChar;
      pPackedContext: PSecBuffer;
      Token: Pointer;
      out phContext: PCtxtHandle
      ): SECURITY_STATUS; stdcall;

    AddCredentials: function(
      hCredentials: PCredHandle;
      pszPrincipal: PChar;
      pszPackage: PChar;
      fCredentialUse: Cardinal;
      pAuthData: Pointer;
      pGetKeyFn: TSecGetKeyFn;
      pvGetKeyArgument: Pointer;
      ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;

    Reserved8: Pointer;

    QuerySecurityContextToken: function(
      phContext: PCtxtHandle;
      out Token: Pointer): SECURITY_STATUS; stdcall;

    EncryptMessage: function(
      phContext: PCtxtHandle;
      fQOP: Cardinal;
      pMessage: PSecBufferDesc;
      MessageSeqNo: Cardinal): SECURITY_STATUS; stdcall;

    DecryptMessage: function(
      phContext: PCtxtHandle;
      pMessage: PSecBufferDesc;
      MessageSeqNo: Cardinal;
      pfQOP: PCardinal): SECURITY_STATUS; stdcall;

    // Fields below this are available in OSes after w2k
    SetContextAttributes: function(
      phContext: PCtxtHandle;
      ulAttribute: Cardinal;
      pBuffer: Pointer;
      cbBuffer: Cardinal): SECURITY_STATUS; stdcall;

    // Fields below this are available in OSes after W2k3SP1
    SetCredentialsAttributes: function(
      phCredential: PCredHandle;
      ulAttribute: Cardinal;
      pBuffer: Pointer;
      cbBuffer: Cardinal): SECURITY_STATUS; stdcall;

    Reserved9: Pointer;
  end;
  PSecurityFunctionTable = ^TSecurityFunctionTable;

  INIT_SECURITY_INTERFACE = function: PSecurityFunctionTable; stdcall;

  // http://forum.ru-board.com/topic.cgi?forum=33&topic=8529&start=508&limit=1&m=1
  TRSAPubKey = Record
    Magic  : Integer;                 { Has to be RSA1        }
    BitLen : Integer;                 { # of bits in modulus  }
    PubExp : Integer;                 { public exponent       }
  End;                                { modulus data follows  }

const
  PROV_RSA_FULL = 1;

  CRYPT_SILENT = 64;
  CRYPT_VERIFYCONTEXT = $F0000000;

  CRYPT_ACQUIRE_CACHE_FLAG               = $00000001;
  CRYPT_ACQUIRE_USE_PROV_INFO_FLAG       = $00000002;
  CRYPT_ACQUIRE_COMPARE_KEY_FLAG         = $00000004;
  CRYPT_ACQUIRE_SILENT_FLAG              = $00000040;

  AT_SIGNATURE = 2;

  CRYPT_EXPORTABLE = 1;
  RSA1024BIT_KEY = $04000000;

  HP_HASHSIZE = 4;
  HP_HASHVAL  = 2;

  PUBLICKEYBLOB = 6;

Function CryptAcquireContext(var phProv : HCryptProv;
                              pszContainer,pszProvider : PChar;
                              dwProvType,dwFlags : Integer) : Bool; stdcall; external advapi32 Name 'CryptAcquireContextW';

function CryptAcquireCertificatePrivateKey(
      pCert: PCCERT_CONTEXT;
      dwFlags: DWORD;
      pvParameters: Pointer;
 var  phCryptProvOrNCryptKey: HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
 var  pdwKeySpec: DWORD;
 var  pfCallerFreeProvOrNCryptKey: BOOL
): BOOL; stdcall; external 'crypt32.dll';

function CertCreateCertificateContext(
  dwCertEncodingType: DWORD;
  pbCertEncoded: Pointer;
  cbCertEncoded: DWORD
): PCCERT_CONTEXT; stdcall; external 'crypt32.dll';

function CertEnumCertificatesInStore(
  hCertStore       : HCERTSTORE;
  pPrevCertContext : PCCERT_CONTEXT
): PCCERT_CONTEXT; stdcall; external 'crypt32.dll';

function CertAddEncodedCertificateToStore(
  hCertStore        : HCERTSTORE;
  dwCertEncodingType: DWORD;
  pbCertEncoded     : Pointer;
  cbCertEncoded     : DWORD;
  dwAddDisposition  : DWORD;
  ppCertContext     : PPCCERT_CONTEXT
): BOOL; stdcall; external 'crypt32.dll';

const
  CERT_KEY_PROV_HANDLE_PROP_ID                = 1;
  CERT_KEY_PROV_INFO_PROP_ID                  = 2;
  CERT_SHA1_HASH_PROP_ID                      = 3;
  CERT_MD5_HASH_PROP_ID                       = 4;
  CERT_HASH_PROP_ID                           = CERT_SHA1_HASH_PROP_ID;
  CERT_KEY_CONTEXT_PROP_ID                    = 5;
  CERT_KEY_SPEC_PROP_ID                       = 6;
  CERT_IE30_RESERVED_PROP_ID                  = 7;
  CERT_PUBKEY_HASH_RESERVED_PROP_ID           = 8;
  CERT_ENHKEY_USAGE_PROP_ID                   = 9;
  CERT_CTL_USAGE_PROP_ID                      = CERT_ENHKEY_USAGE_PROP_ID;
  CERT_NEXT_UPDATE_LOCATION_PROP_ID           = 10;
  CERT_FRIENDLY_NAME_PROP_ID                  = 11;
  CERT_PVK_FILE_PROP_ID                       = 12;
  CERT_DESCRIPTION_PROP_ID                    = 13;
  CERT_ACCESS_STATE_PROP_ID                   = 14;
  CERT_SIGNATURE_HASH_PROP_ID                 = 15;
  CERT_SMART_CARD_DATA_PROP_ID                = 16;
  CERT_EFS_PROP_ID                            = 17;
  CERT_FORTEZZA_DATA_PROP_ID                  = 18;
  CERT_ARCHIVED_PROP_ID                       = 19;
  CERT_KEY_IDENTIFIER_PROP_ID                 = 20;
  CERT_AUTO_ENROLL_PROP_ID                    = 21;
  CERT_PUBKEY_ALG_PARA_PROP_ID                = 22;
  CERT_CROSS_CERT_DIST_POINTS_PROP_ID         = 23;
  CERT_ISSUER_PUBLIC_KEY_MD5_HASH_PROP_ID     = 24;
  CERT_SUBJECT_PUBLIC_KEY_MD5_HASH_PROP_ID    = 25;
  CERT_ENROLLMENT_PROP_ID                     = 26;
  CERT_DATE_STAMP_PROP_ID                     = 27;
  CERT_ISSUER_SERIAL_NUMBER_MD5_HASH_PROP_ID  = 28;
  CERT_SUBJECT_NAME_MD5_HASH_PROP_ID          = 29;
  CERT_EXTENDED_ERROR_INFO_PROP_ID            = 30;
  CERT_RENEWAL_PROP_ID                        = 64;
  CERT_ARCHIVED_KEY_HASH_PROP_ID              = 65;
  CERT_FIRST_RESERVED_PROP_ID                 = 66;
  CERT_NCRYPT_KEY_HANDLE_PROP_ID              = 78;

  CERT_STORE_ADD_USE_EXISTING = 2;

function CertEnumCertificateContextProperties(
  pCertContext: PCCERT_CONTEXT;
  dwPropID    : DWORD
): DWORD; stdcall; external 'crypt32.dll';

function CertGetCertificateContextProperty(
      pCertContext: PCCERT_CONTEXT;
      dwPropID    : DWORD;
      pvData      : Pointer;
  var pcbData     : DWORD
): BOOL; stdcall; external 'crypt32.dll';

function CryptImportPublicKeyInfo(
      hCryptProv: HCRYPTPROV;
      dwCertEncodingType: DWORD;
      pInfo: PCERT_PUBLIC_KEY_INFO;
  var phKey: HCRYPTKEY
): BOOL; stdcall; external 'crypt32.dll';

function CryptGenKey(hProv : HCryptProv; Algid : Integer; dwFlags : Integer;
                 var phKey : HCryptKey) : Bool; stdcall; external advapi32;

function CryptCreateHash(hProv : HCryptProv;
                         Algid : Integer;
                         hKey : HCryptKey;
                         dwFlags : Integer;
                     var phHash : HCryptHash) : Bool; stdcall; external advapi32;

function CryptHashData(hHash : HCryptHash;
                       Const pbData : Pointer;
                       dwDataLen,dwFlags : Integer) : Bool; stdcall; external advapi32;

function CryptGetHashParam(hHash : HCryptHash;
                           dwParam : Integer;
                           pbData : Pointer;
                           Var pdwDataLen : Cardinal;
                           dwFlags : Integer) : Bool; stdcall; external advapi32;

function CryptSignHash(hHash : HCryptHash;
                        dwKeySpec : Integer;
                        sDescription : PChar;
                        dwFlags : Integer;
                        pbSignature : Pointer;
                    var pdwSigLen : Cardinal) : Bool; stdcall; external advapi32 name 'CryptSignHashW';

function CryptExportKey(hKey,hExpKey : HCryptKey;
                        dwBlobType,dwFlags : Integer;
                        pbData : Pointer;
                        Var pdwDataLen : Cardinal) : Bool; stdcall; external advapi32;

Function CryptDestroyKey(hKey : HCryptKey) : Bool; StdCall; external advapi32;

Function CryptDestroyHash(hHash : HCryptHash) : Bool; StdCall; external advapi32;

Function CryptReleaseContext(hProv : HCryptProv; dwFlags : Integer) : Bool; StdCall; external advapi32;

function InitSecurityInterface: PSecurityFunctionTable; stdcall; external 'secur32.dll' name 'InitSecurityInterfaceW';

const
// SSPI
  SECPKG_CRED_INBOUND        = $00000001;
  SECPKG_CRED_OUTBOUND       = $00000002;
  SECPKG_CRED_BOTH           = $00000003;
  SECPKG_CRED_DEFAULT        = $00000004;
  SECPKG_CRED_RESERVED       = $F0000000;

  SECBUFFER_VERSION          = 0;

  SECBUFFER_EMPTY            = 0;   // Undefined, replaced by provider
  SECBUFFER_DATA             = 1;   // Packet data
  SECBUFFER_TOKEN            = 2;   // Security token
  SECBUFFER_PKG_PARAMS       = 3;   // Package specific parameters
  SECBUFFER_MISSING          = 4;   // Missing Data indicator
  SECBUFFER_EXTRA            = 5;   // Extra data
  SECBUFFER_STREAM_TRAILER   = 6;   // Security Trailer
  SECBUFFER_STREAM_HEADER    = 7;   // Security Header
  SECBUFFER_NEGOTIATION_INFO = 8;   // Hints from the negotiation pkg
  SECBUFFER_PADDING          = 9;   // non-data padding
  SECBUFFER_STREAM           = 10;  // whole encrypted message
  SECBUFFER_MECHLIST         = 11;
  SECBUFFER_MECHLIST_SIGNATURE = 12;
  SECBUFFER_TARGET           = 13;  // obsolete
  SECBUFFER_CHANNEL_BINDINGS = 14;
  SECBUFFER_CHANGE_PASS_RESPONSE = 15;
  SECBUFFER_TARGET_HOST      = 16;
  SECBUFFER_ALERT            = 17;

  SECBUFFER_ATTRMASK                     = $F0000000;
  SECBUFFER_READONLY                     = $80000000;  // Buffer is read-only, no checksum
  SECBUFFER_READONLY_WITH_CHECKSUM       = $10000000;  // Buffer is read-only, and checksummed
  SECBUFFER_RESERVED                     = $60000000;  // Flags reserved to security system

  SECURITY_NATIVE_DREP       = $00000010;
  SECURITY_NETWORK_DREP      = $00000000;

  SECPKG_ATTR_SIZES           = 0;
  SECPKG_ATTR_NAMES           = 1;
  SECPKG_ATTR_LIFESPAN        = 2;
  SECPKG_ATTR_DCE_INFO        = 3;
  SECPKG_ATTR_STREAM_SIZES    = 4;
  SECPKG_ATTR_KEY_INFO        = 5;
  SECPKG_ATTR_AUTHORITY       = 6;
  SECPKG_ATTR_PROTO_INFO      = 7;
  SECPKG_ATTR_PASSWORD_EXPIRY = 8;
  SECPKG_ATTR_SESSION_KEY     = 9;
  SECPKG_ATTR_PACKAGE_INFO    = 10;
  SECPKG_ATTR_USER_FLAGS      = 11;
  SECPKG_ATTR_NEGOTIATION_INFO = 12;
  SECPKG_ATTR_NATIVE_NAMES    = 13;
  SECPKG_ATTR_FLAGS           = 14;
// These attributes exist only in Win XP and greater
  SECPKG_ATTR_USE_VALIDATED   = 15;
  SECPKG_ATTR_CREDENTIAL_NAME = 16;
  SECPKG_ATTR_TARGET_INFORMATION = 17;
  SECPKG_ATTR_ACCESS_TOKEN    = 18;
// These attributes exist only in Win2K3 and greater
  SECPKG_ATTR_TARGET          = 19;
  SECPKG_ATTR_AUTHENTICATION_ID = 20;
// These attributes exist only in Win2K3SP1 and greater
  SECPKG_ATTR_LOGOFF_TIME     = 21;
// win7 or greater
  SECPKG_ATTR_NEGO_KEYS         = 22;
  SECPKG_ATTR_PROMPTING_NEEDED  = 24;
  SECPKG_ATTR_UNIQUE_BINDINGS   = 25;
  SECPKG_ATTR_ENDPOINT_BINDINGS = 26;
  SECPKG_ATTR_CLIENT_SPECIFIED_TARGET = 27;
  SECPKG_ATTR_LAST_CLIENT_TOKEN_STATUS = 30;
  SECPKG_ATTR_NEGO_PKG_INFO        = 31; // contains nego info of packages
  SECPKG_ATTR_NEGO_STATUS          = 32; // contains the last error
  SECPKG_ATTR_CONTEXT_DELETED      = 33; // a context has been deleted
// win8 or greater
  SECPKG_ATTR_DTLS_MTU        = 34;
  SECPKG_ATTR_DATAGRAM_SIZES  = SECPKG_ATTR_STREAM_SIZES;
  SECPKG_ATTR_SUBJECT_SECURITY_ATTRIBUTES = 128;

  ISC_REQ_DELEGATE               = $00000001;
  ISC_REQ_MUTUAL_AUTH            = $00000002;
  ISC_REQ_REPLAY_DETECT          = $00000004;
  ISC_REQ_SEQUENCE_DETECT        = $00000008;
  ISC_REQ_CONFIDENTIALITY        = $00000010;
  ISC_REQ_USE_SESSION_KEY        = $00000020;
  ISC_REQ_PROMPT_FOR_CREDS       = $00000040;
  ISC_REQ_USE_SUPPLIED_CREDS     = $00000080;
  ISC_REQ_ALLOCATE_MEMORY        = $00000100;
  ISC_REQ_USE_DCE_STYLE          = $00000200;
  ISC_REQ_DATAGRAM               = $00000400;
  ISC_REQ_CONNECTION             = $00000800;
  ISC_REQ_CALL_LEVEL             = $00001000;
  ISC_REQ_FRAGMENT_SUPPLIED      = $00002000;
  ISC_REQ_EXTENDED_ERROR         = $00004000;
  ISC_REQ_STREAM                 = $00008000;
  ISC_REQ_INTEGRITY              = $00010000;
  ISC_REQ_IDENTIFY               = $00020000;
  ISC_REQ_NULL_SESSION           = $00040000;
  ISC_REQ_MANUAL_CRED_VALIDATION = $00080000;
  ISC_REQ_RESERVED1              = $00100000;
  ISC_REQ_FRAGMENT_TO_FIT        = $00200000;
// This exists only in Windows Vista and greater
  ISC_REQ_FORWARD_CREDENTIALS    = $00400000;
  ISC_REQ_NO_INTEGRITY           = $00800000; // honored only by SPNEGO
  ISC_REQ_USE_HTTP_STYLE         = $01000000;
  ISC_REQ_UNVERIFIED_TARGET_NAME = $20000000;
  ISC_REQ_CONFIDENTIALITY_ONLY   = $40000000; // honored by SPNEGO/Kerberos
  ISC_RET_DELEGATE               = $00000001;
  ISC_RET_MUTUAL_AUTH            = $00000002;
  ISC_RET_REPLAY_DETECT          = $00000004;
  ISC_RET_SEQUENCE_DETECT        = $00000008;
  ISC_RET_CONFIDENTIALITY        = $00000010;
  ISC_RET_USE_SESSION_KEY        = $00000020;
  ISC_RET_USED_COLLECTED_CREDS   = $00000040;
  ISC_RET_USED_SUPPLIED_CREDS    = $00000080;
  ISC_RET_ALLOCATED_MEMORY       = $00000100;
  ISC_RET_USED_DCE_STYLE         = $00000200;
  ISC_RET_DATAGRAM               = $00000400;
  ISC_RET_CONNECTION             = $00000800;
  ISC_RET_INTERMEDIATE_RETURN    = $00001000;
  ISC_RET_CALL_LEVEL             = $00002000;
  ISC_RET_EXTENDED_ERROR         = $00004000;
  ISC_RET_STREAM                 = $00008000;
  ISC_RET_INTEGRITY              = $00010000;
  ISC_RET_IDENTIFY               = $00020000;
  ISC_RET_NULL_SESSION           = $00040000;
  ISC_RET_MANUAL_CRED_VALIDATION = $00080000;
  ISC_RET_RESERVED1              = $00100000;
  ISC_RET_FRAGMENT_ONLY          = $00200000;
// This exists only in Windows Vista and greater
  ISC_RET_FORWARD_CREDENTIALS    = $00400000;
  ISC_RET_USED_HTTP_STYLE        = $01000000;
  ISC_RET_NO_ADDITIONAL_TOKEN    = $02000000; // *INTERNAL*
  ISC_RET_REAUTHENTICATION       = $08000000; // *INTERNAL*
  ISC_RET_CONFIDENTIALITY_ONLY   = $40000000; // honored by SPNEGO/Kerberos

// CryptoLib

  CERT_SIMPLE_NAME_STR = 1;
  CERT_OID_NAME_STR = 2;
  CERT_X500_NAME_STR = 3;

  CERT_NAME_STR_REVERSE_FLAG    = $02000000;
  CERT_NAME_STR_COMMA_FLAG      = $04000000;
  CERT_NAME_STR_CRLF_FLAG       = $08000000;
  CERT_NAME_STR_NO_QUOTING_FLAG = $10000000;
  CERT_NAME_STR_NO_PLUS_FLAG    = $20000000;
  CERT_NAME_STR_SEMICOLON_FLAG  = $40000000;

  // Algorithm classes
  ALG_CLASS_ANY          = 0;
  ALG_CLASS_SIGNATURE    = (1 shl 13);
  ALG_CLASS_MSG_ENCRYPT  = (2 shl 13);
  ALG_CLASS_DATA_ENCRYPT = (3 shl 13);
  ALG_CLASS_HASH         = (4 shl 13);
  ALG_CLASS_KEY_EXCHANGE = (5 shl 13);

  // Algorithm types
  ALG_TYPE_ANY           = 0;
  ALG_TYPE_DSS           = (1 shl 9);
  ALG_TYPE_RSA           = (2 shl 9);
  ALG_TYPE_BLOCK         = (3 shl 9);
  ALG_TYPE_STREAM        = (4 shl 9);
  ALG_TYPE_DH            = (5 shl 9);
  ALG_TYPE_SECURECHANNEL = (6 shl 9);

  // Generic sub-ids
  ALG_SID_ANY = 0;

  // Some RSA sub-ids
  ALG_SID_RSA_ANY        = 0;
  ALG_SID_RSA_PKCS       = 1;
  ALG_SID_RSA_MSATWORK   = 2;
  ALG_SID_RSA_ENTRUST    = 3;
  ALG_SID_RSA_PGP        = 4;

  // Some DSS sub-ids
  ALG_SID_DSS_ANY        = 0;
  ALG_SID_DSS_PKCS       = 1;
  ALG_SID_DSS_DMS        = 2;

  // Block cipher sub ids
  // DES sub_ids
  ALG_SID_DES            = 1;
  ALG_SID_3DES           = 3;
  ALG_SID_DESX           = 4;
  ALG_SID_IDEA           = 5;
  ALG_SID_CAST           = 6;
  ALG_SID_SAFERSK64      = 7;
  ALD_SID_SAFERSK128     = 8;
  ALG_SID_SAFERSK128     = 8;
  ALG_SID_3DES_112       = 9;
  ALG_SID_CYLINK_MEK     = 12;
  ALG_SID_RC5            = 13;

  //Added Sept. 2010 source Windows 7 sdk
  ALG_SID_AES_128 = 14;
  ALG_SID_AES_192 = 15;
  ALG_SID_AES_256 = 16;
  ALG_SID_AES = 17;

  // Fortezza sub-ids
  ALG_SID_SKIPJACK       = 10;
  ALG_SID_TEK            = 11;


  // KP_MODE
  CRYPT_MODE_CBCI        = 6;  {ANSI CBC Interleaved}
  CRYPT_MODE_CFBP        = 7;  {ANSI CFB Pipelined}
  CRYPT_MODE_OFBP        = 8;  {ANSI OFB Pipelined}
  CRYPT_MODE_CBCOFM      = 9;  {ANSI CBC + OF Masking}
  CRYPT_MODE_CBCOFMI     = 10; {ANSI CBC + OFM Interleaved}

  // RC2 sub-ids
  ALG_SID_RC2            = 2;

  // Stream cipher sub-ids
  ALG_SID_RC4            = 1;
  ALG_SID_SEAL           = 2;

  // Diffie-Hellman sub-ids
  ALG_SID_DH_SANDF       = 1;
  ALG_SID_DH_EPHEM       = 2;
  ALG_SID_AGREED_KEY_ANY = 3;
  ALG_SID_KEA            = 4;

  // Hash sub ids
  ALG_SID_MD2            = 1;
  ALG_SID_MD4            = 2;
  ALG_SID_MD5            = 3;
  ALG_SID_SHA            = 4;
  ALG_SID_SHA1           = 4;
  ALG_SID_MAC            = 5;
  ALG_SID_RIPEMD         = 6;
  ALG_SID_RIPEMD160      = 7;
  ALG_SID_SSL3SHAMD5     = 8;
  ALG_SID_HMAC           = 9;
  //Added Sept. 2010 source Windows 7 SDK
  ALG_SID_SHA_256        = 12;
  ALG_SID_SHA_384        = 13;
  ALG_SID_SHA_512        = 14;

  // secure channel sub ids
  ALG_SID_SSL3_MASTER          = 1;
  ALG_SID_SCHANNEL_MASTER_HASH = 2;
  ALG_SID_SCHANNEL_MAC_KEY     = 3;
  ALG_SID_PCT1_MASTER          = 4;
  ALG_SID_SSL2_MASTER          = 5;
  ALG_SID_TLS1_MASTER          = 6;
  ALG_SID_SCHANNEL_ENC_KEY     = 7;

  // Our silly example sub-id
  ALG_SID_EXAMPLE              = 80;

  CALG_MD2              = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MD2);
  CALG_MD4              = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MD4);
  CALG_MD5              = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MD5);
  CALG_SHA              = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA);
  CALG_SHA1             = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA1);
  CALG_MAC              = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MAC);
  CALG_RSA_SIGN         = (ALG_CLASS_SIGNATURE or ALG_TYPE_RSA or ALG_SID_RSA_ANY);
  CALG_DSS_SIGN         = (ALG_CLASS_SIGNATURE or ALG_TYPE_DSS or ALG_SID_DSS_ANY);
  CALG_RSA_KEYX         = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_RSA or ALG_SID_RSA_ANY);
  CALG_DES              = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_DES);
  CALG_3DES_112         = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_3DES_112);
  CALG_3DES             = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_3DES);
  CALG_RC2              = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_RC2);
  CALG_RC4              = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_STREAM or ALG_SID_RC4);
  CALG_SEAL             = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_STREAM or ALG_SID_SEAL);
  CALG_DH_SF            = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_DH_SANDF);
  CALG_DH_EPHEM         = (ALG_CLASS_KEY_EXCHANGE  or  ALG_TYPE_DH  or  ALG_SID_DH_EPHEM);
  CALG_AGREEDKEY_ANY    = (ALG_CLASS_KEY_EXCHANGE  or ALG_TYPE_DH or ALG_SID_AGREED_KEY_ANY);
  CALG_KEA_KEYX         = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_KEA);
  CALG_HUGHES_MD5       = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_ANY or ALG_SID_MD5);
  CALG_SKIPJACK         = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_SKIPJACK);
  CALG_TEK              = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_TEK);
  CALG_CYLINK_MEK       = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_CYLINK_MEK);
  CALG_SSL3_SHAMD5      = (ALG_CLASS_HASH  or  ALG_TYPE_ANY  or  ALG_SID_SSL3SHAMD5);
  CALG_SSL3_MASTER      = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SSL3_MASTER);
  CALG_SCHANNEL_MASTER_HASH = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SCHANNEL_MASTER_HASH);
  CALG_SCHANNEL_MAC_KEY = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SCHANNEL_MAC_KEY);
  CALG_SCHANNEL_ENC_KEY = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SCHANNEL_ENC_KEY);
  CALG_PCT1_MASTER      = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_PCT1_MASTER);
  CALG_SSL2_MASTER      = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SSL2_MASTER);
  CALG_TLS1_MASTER      = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_TLS1_MASTER);
  CALG_RC5              = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_RC5);
  CALG_HMAC             = (ALG_CLASS_HASH  or  ALG_TYPE_ANY  or  ALG_SID_HMAC);
  //Added Sept. 2010 source Windows 7 SDK
  CALG_AES_128 = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_128);
  CALG_AES_192 = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_192);
  CALG_AES_256 = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_256);
  CALG_AES = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES);
  CALG_SHA_256 = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA_256);
  CALG_SHA_384 = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA_384);
  CALG_SHA_512 = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA_512);

//* flag/identifiers for protocols we support */
  SP_PROT_PCT1_SERVER             = $00000001;
  SP_PROT_PCT1_CLIENT             = $00000002;
  SP_PROT_PCT1                    = (SP_PROT_PCT1_SERVER or SP_PROT_PCT1_CLIENT);

  SP_PROT_SSL2_SERVER             = $00000004;
  SP_PROT_SSL2_CLIENT             = $00000008;
  SP_PROT_SSL2                    = (SP_PROT_SSL2_SERVER or SP_PROT_SSL2_CLIENT);

  SP_PROT_SSL3_SERVER             = $00000010;
  SP_PROT_SSL3_CLIENT             = $00000020;
  SP_PROT_SSL3                    = (SP_PROT_SSL3_SERVER or SP_PROT_SSL3_CLIENT);

  SP_PROT_TLS1_SERVER             = $00000040;
  SP_PROT_TLS1_CLIENT             = $00000080;
  SP_PROT_TLS1                    = (SP_PROT_TLS1_SERVER or SP_PROT_TLS1_CLIENT);

  SP_PROT_TLS1_1_SERVER           = $00000100;
  SP_PROT_TLS1_1_CLIENT           = $00000200;
  SP_PROT_TLS1_1                  = (SP_PROT_TLS1_1_SERVER or SP_PROT_TLS1_1_CLIENT);

  SP_PROT_TLS1_2_SERVER           = $00000400;
  SP_PROT_TLS1_2_CLIENT           = $00000800;
  SP_PROT_TLS1_2                  = (SP_PROT_TLS1_2_SERVER or SP_PROT_TLS1_2_CLIENT);

  SP_PROT_TLS                     = SP_PROT_TLS1 or SP_PROT_TLS1_2;

  SP_PROT_SSL3TLS1_CLIENTS        = (SP_PROT_TLS1_CLIENT or SP_PROT_SSL3_CLIENT);
  SP_PROT_SSL3TLS1_SERVERS        = (SP_PROT_TLS1_SERVER or SP_PROT_SSL3_SERVER);
  SP_PROT_SSL3TLS1                = (SP_PROT_SSL3 or SP_PROT_TLS1);

  SP_PROT_UNI_SERVER              = $40000000;
  SP_PROT_UNI_CLIENT              = $80000000;
  SP_PROT_UNI                     = (SP_PROT_UNI_SERVER or SP_PROT_UNI_CLIENT);

  SP_PROT_ALL                     = $ffffffff;
  SP_PROT_NONE                    = 0;
  SP_PROT_CLIENTS                 = (SP_PROT_PCT1_CLIENT or SP_PROT_SSL2_CLIENT or SP_PROT_SSL3_CLIENT or SP_PROT_UNI_CLIENT or SP_PROT_TLS1_CLIENT);
  SP_PROT_SERVERS                 = (SP_PROT_PCT1_SERVER or SP_PROT_SSL2_SERVER or SP_PROT_SSL3_SERVER or SP_PROT_UNI_SERVER or SP_PROT_TLS1_SERVER);



  SEC_I_CONTINUE_NEEDED       = $00090312;
  SEC_I_CONTEXT_EXPIRED       = $00090317;
  SEC_I_INCOMPLETE_CREDENTIALS= $00090320;
  SEC_I_RENEGOTIATE           = $00090321;

  SEC_E_OK                    = 0;

  SEC_E_INSUFFICIENT_MEMORY   = $80090300;
  SEC_E_INVALID_HANDLE        = $80090301;
  SEC_E_UNSUPPORTED_FUNCTION  = $80090302;
  SEC_E_INTERNAL_ERROR        = $80090304;
  SEC_E_INVALID_TOKEN         = $80090308;
  SEC_E_QOP_NOT_SUPPORTED     = $8009030A;
  SEC_E_UNKNOWN_CREDENTIALS   = $8009030D;
  SEC_E_NO_CREDENTIALS        = $8009030E;
  SEC_E_MESSAGE_ALTERED       = $8009030F;
  SEC_E_OUT_OF_SEQUENCE       = $80090310;
  SEC_E_INCOMPLETE_MESSAGE    = $80090318;
  SEC_E_BUFFER_TOO_SMALL      = $80090321;
  SEC_E_WRONG_PRINCIPAL       = $80090322;
  SEC_E_UNTRUSTED_ROOT        = $80090325;
  SEC_E_ILLEGAL_MESSAGE       = $80090326;
  SEC_E_ENCRYPT_FAILURE       = $80090329;
  SEC_E_DECRYPT_FAILURE       = $80090330;
  SEC_E_ALGORITHM_MISMATCH    = $80090331;
  SEC_E_CRYPTO_SYSTEM_INVALID = $80090337;

  CRYPT_E_NOT_FOUND           = $80092004;

  // https://msdn.microsoft.com/fr-fr/library/windows/desktop/aa377188(v=vs.85).aspx
  CERT_E_UNTRUSTEDROOT        = $800B0109;
  CERT_E_CN_NO_MATCH          = $800B010F;  // The certificate's CN name does not match the passed value.

// QueryContextAttributes/QueryCredentialsAttribute extensions

  SECPKG_ATTR_REMOTE_CERT_CONTEXT  = $53;  // returns PCCERT_CONTEXT
  SECPKG_ATTR_LOCAL_CERT_CONTEXT   = $54;  // returns PCCERT_CONTEXT
  SECPKG_ATTR_ROOT_STORE           = $55;  // returns HCERTCONTEXT to the root store
  SECPKG_ATTR_SUPPORTED_ALGS       = $56;  // returns SecPkgCred_SupportedAlgs
  SECPKG_ATTR_CIPHER_STRENGTHS     = $57;  // returns SecPkgCred_CipherStrengths
  SECPKG_ATTR_SUPPORTED_PROTOCOLS  = $58;  // returns SecPkgCred_SupportedProtocols
  SECPKG_ATTR_ISSUER_LIST_EX       = $59;  // returns SecPkgContext_IssuerListInfoEx
  SECPKG_ATTR_CONNECTION_INFO      = $5a;  // returns SecPkgContext_ConnectionInfo

  szOID_PKIX_KP_SERVER_AUTH = '1.3.6.1.5.5.7.3.1';
  szOID_SERVER_GATED_CRYPTO = '1.3.6.1.4.1.311.10.3.3';
  szOID_SGC_NETSCAPE        = '2.16.840.1.113730.4.1';
  szOID_PKIX_KP_CLIENT_AUTH = '1.3.6.1.5.5.7.3.2';

  SCHANNEL_SHUTDOWN = 1;
  SCHANNEL_CRED_VERSION = 4;

  SCH_CRED_NO_SYSTEM_MAPPER                    = $00000002;
  SCH_CRED_NO_SERVERNAME_CHECK                 = $00000004;
  SCH_CRED_MANUAL_CRED_VALIDATION              = $00000008;
  SCH_CRED_NO_DEFAULT_CREDS                    = $00000010;
  SCH_CRED_AUTO_CRED_VALIDATION                = $00000020;
  SCH_CRED_USE_DEFAULT_CREDS                   = $00000040;

  SCH_CRED_REVOCATION_CHECK_END_CERT           = $00000100;
  SCH_CRED_REVOCATION_CHECK_CHAIN              = $00000200;
  SCH_CRED_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT = $00000400;
  SCH_CRED_IGNORE_NO_REVOCATION_CHECK          = $00000800;
  SCH_CRED_IGNORE_REVOCATION_OFFLINE           = $00001000;

  AUTHTYPE_CLIENT = 1;
  AUTHTYPE_SERVER = 2;

  USAGE_MATCH_TYPE_AND = 0;
  USAGE_MATCH_TYPE_OR  = 1;

  UNISP_NAME = 'Microsoft Unified Security Protocol Provider';

  MS_ENH_RSA_AES_PROV = 'Microsoft Enhanced RSA and AES Cryptographic Provider';

  PROV_RSA_AES = 24;

  CERT_CHAIN_POLICY_BASE              =LPCSTR(1);
  CERT_CHAIN_POLICY_AUTHENTICODE      =LPCSTR(2);
  CERT_CHAIN_POLICY_AUTHENTICODE_TS   =LPCSTR(3);
  CERT_CHAIN_POLICY_SSL               =LPCSTR(4);
  CERT_CHAIN_POLICY_BASIC_CONSTRAINTS =LPCSTR(5);
  CERT_CHAIN_POLICY_NT_AUTH           =LPCSTR(6);
  CERT_CHAIN_POLICY_MICROSOFT_ROOT    =LPCSTR(7);

  CERT_INFO_VERSION_FLAG                 = 1;
  CERT_INFO_SERIAL_NUMBER_FLAG           = 2;
  CERT_INFO_SIGNATURE_ALGORITHM_FLAG     = 3;
  CERT_INFO_ISSUER_FLAG                  = 4;
  CERT_INFO_NOT_BEFORE_FLAG              = 5;
  CERT_INFO_NOT_AFTER_FLAG               = 6;
  CERT_INFO_SUBJECT_FLAG                 = 7;
  CERT_INFO_SUBJECT_PUBLIC_KEY_INFO_FLAG = 8;
  CERT_INFO_ISSUER_UNIQUE_ID_FLAG        = 9;
  CERT_INFO_SUBJECT_UNIQUE_ID_FLAG       = 10;
  CERT_INFO_EXTENSION_FLAG               = 11;

  CERT_COMPARE_SHIFT = 16;
  CERT_COMPARE_ANY = 0;
  CERT_COMPARE_SHA1_HASH = 1;
  CERT_COMPARE_NAME = 2;
  CERT_COMPARE_ATTR = 3;
  CERT_COMPARE_MD5_HASH  = 4;
  CERT_COMPARE_PROPERTY = 5;
  CERT_COMPARE_PUBLIC_KEY = 6;
  CERT_COMPARE_HASH = CERT_COMPARE_SHA1_HASH;
  CERT_COMPARE_NAME_STR_A = 7;
  CERT_COMPARE_NAME_STR_W = 8;
  CERT_COMPARE_KEY_SPEC = 9;
  CERT_COMPARE_ENHKEY_USAGE = 10;
  CERT_COMPARE_CTL_USAGE = CERT_COMPARE_ENHKEY_USAGE;

  CERT_FIND_ANY = (CERT_COMPARE_ANY shl CERT_COMPARE_SHIFT);
  CERT_FIND_SHA1_HASH = (CERT_COMPARE_SHA1_HASH shl CERT_COMPARE_SHIFT);
  CERT_FIND_MD5_HASH = (CERT_COMPARE_MD5_HASH shl CERT_COMPARE_SHIFT);
  CERT_FIND_HASH = CERT_FIND_SHA1_HASH;
  CERT_FIND_PROPERTY = (CERT_COMPARE_PROPERTY shl CERT_COMPARE_SHIFT);
  CERT_FIND_PUBLIC_KEY = (CERT_COMPARE_PUBLIC_KEY shl CERT_COMPARE_SHIFT);
  CERT_FIND_EXISTING = $000D0000;

  CERT_FIND_SUBJECT_NAME = (CERT_COMPARE_NAME shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG);
  CERT_FIND_SUBJECT_ATTR = (CERT_COMPARE_ATTR shl CERT_COMPARE_SHIFT or  CERT_INFO_SUBJECT_FLAG);
  CERT_FIND_ISSUER_NAME = (CERT_COMPARE_NAME shl CERT_COMPARE_SHIFT or  CERT_INFO_ISSUER_FLAG);
  CERT_FIND_ISSUER_ATTR = (CERT_COMPARE_ATTR shl CERT_COMPARE_SHIFT or   CERT_INFO_ISSUER_FLAG);
  CERT_FIND_SUBJECT_STR_A =  (CERT_COMPARE_NAME_STR_A shl CERT_COMPARE_SHIFT or   CERT_INFO_SUBJECT_FLAG);
  CERT_FIND_SUBJECT_STR_W =  (CERT_COMPARE_NAME_STR_W shl CERT_COMPARE_SHIFT or   CERT_INFO_SUBJECT_FLAG);
  CERT_FIND_SUBJECT_STR = CERT_FIND_SUBJECT_STR_W;
  CERT_FIND_ISSUER_STR_A = (CERT_COMPARE_NAME_STR_A shl CERT_COMPARE_SHIFT or  CERT_INFO_ISSUER_FLAG);
  CERT_FIND_ISSUER_STR_W =  (CERT_COMPARE_NAME_STR_W shl CERT_COMPARE_SHIFT or  CERT_INFO_ISSUER_FLAG);
  CERT_FIND_ISSUER_STR = CERT_FIND_ISSUER_STR_W;
  CERT_FIND_KEY_SPEC = (CERT_COMPARE_KEY_SPEC shl CERT_COMPARE_SHIFT);
  CERT_FIND_ENHKEY_USAGE = (CERT_COMPARE_ENHKEY_USAGE shl CERT_COMPARE_SHIFT);
  CERT_FIND_CTL_USAGE = CERT_FIND_ENHKEY_USAGE;

  CRYPT_ASN_ENCODING  = $00000001;
  CRYPT_NDR_ENCODING = $00000002;
  X509_ASN_ENCODING = $00000001;
  X509_NDR_ENCODING = $00000002;
  PKCS_7_ASN_ENCODING = $00010000;
  PKCS_7_NDR_ENCODING = $00020000;

  CERT_CHAIN_FIND_BY_ISSUER       = 1;

type
  TSecPkgContextConnectionInfo = record
    dwProtocol      : DWORD;
    aiCipher        : ALG_ID;
    dwCipherStrength: DWORD;
    aiHash          : ALG_ID;
    dwHashStrength  : DWORD;
    aiExch          : ALG_ID;
    dwExchStrength  : DWORD;
  end;

  TSecPkgContextIssuerListInfoEx = record
    aIssuers: PCERT_NAME_BLOB;
    cIssuers: Cardinal;
  end;

  PFN_CERT_CHAIN_FIND_BY_ISSUER_CALLBACK = function(pCert: PCCERT_CONTEXT; pvFindArg: Pointer): Integer; stdcall;

  CERT_CHAIN_FIND_BY_ISSUER_PARA = record
    cbSize: DWORD;
    pszUsageIdentifier: LPCSTR;
    dwKeySpec: DWORD;
    dwAcquirePrivateKeyFlags: DWORD;
    cIssuer: DWORD;
    rgIssuer: PCERT_NAME_BLOB;
    pfnFindCallback: PFN_CERT_CHAIN_FIND_BY_ISSUER_CALLBACK;
    pvFindArg: Pointer;
    pdwIssuerChainIndex: PDWORD;
    pdwIssuerElementIndex: PDWORD;
  end;

  DATA_BLOB = CRYPTOAPI_BLOB;
  PDATA_BLOB = ^DATA_BLOB;

  CRYPTPROTECT_PROMPTSTRUCT = record
    cbSize:        DWORD;
    dwPromptFlags: DWORD;
    hwndApp:       HWND;
    szPrompt:      LPCWSTR;
  end;
  PCRYPTPROTECT_PROMPTSTRUCT = ^CRYPTPROTECT_PROMPTSTRUCT;

const
  CERT_NAME_EMAIL_TYPE            = 1;
  CERT_NAME_RDN_TYPE              = 2;
  CERT_NAME_ATTR_TYPE             = 3;
                szOID_COMMON_NAME = '2.5.4.3';
   szOID_AUTHORITY_KEY_IDENTIFIER = '2.5.29.1';
      szOID_KEY_USAGE_RESTRICTION = '2.5.29.4';
           szOID_SUBJECT_ALT_NAME = '2.5.29.7';
            szOID_ISSUER_ALT_NAME = '2.5.29.8';
          szOID_SUBJECT_ALT_NAME2 = '2.5.29.17';
  CERT_NAME_SIMPLE_DISPLAY_TYPE   = 4;
  CERT_NAME_FRIENDLY_DISPLAY_TYPE = 5;
  CERT_NAME_DNS_TYPE              = 6;

  CERT_NAME_SEARCH_ALL_NAMES_FLAG    = $00000002;
  CERT_NAME_STR_ENABLE_PUNYCODE_FLAG = $00200000;

function CertGetNameStringW(
    pCertContext : PCCERT_CONTEXT;
    dwType       : DWORD;
    dwFlags      : DWORD;
    pvTypePara   : Pointer;
    pszNameString: PChar;
    cchNameString: DWORD
): DWORD; stdcall; external 'crypt32.dll';

function CertNameToStr(
        dwCertEncodingType: DWORD;
  const pName: CERT_NAME_BLOB;
        dwStrType: DWORD;
        psz: PChar;
        csz: DWORD
):DWORD ; stdcall; external 'crypt32.dll' name 'CertNameToStrW';

function CertGetIssuerCertificateFromStore(
  hCertStore :HCERTSTORE;
  pSubjectContext :PCCERT_CONTEXT;
  pPrevIssuerContext :PCCERT_CONTEXT; //OPTIONAL
  pdwFlags :PDWORD
):PCCERT_CONTEXT; stdcall; external 'crypt32.dll';

function CertFreeCertificateContext(pCertContext :PCCERT_CONTEXT):BOOL ; stdcall; external 'crypt32.dll';

function CertGetCertificateChain (
         hChainEngine: HCERTCHAINENGINE;
         pCertContext: PCCERT_CONTEXT;
         pTime: PFILETIME;
         hAdditionalStore: HCERTSTORE;
   const pChainPara: CERT_CHAIN_PARA;
         dwFlags: DWORD;
         pvReserved: pointer;  //LPVOID;
    var  ppChainContext: PCCERT_CHAIN_CONTEXT): bool; stdcall; external 'crypt32.dll';

function CertVerifyCertificateChainPolicy(
             pszPolicyOID:                LPCSTR;
             pChainContext:               PCCERT_CHAIN_CONTEXT;
       const pPolicyPara:                 CERT_CHAIN_POLICY_PARA;
       const pPolicyStatus:               CERT_CHAIN_POLICY_STATUS): bool; stdcall; external 'crypt32.dll';

function CertFreeCertificateChainEngine (
                hChainEngine: HCERTCHAINENGINE): bool; stdcall; external 'crypt32.dll';

function CertFreeCertificateChain (
                  pChainContext: PCCERT_CHAIN_CONTEXT): bool; stdcall; external 'crypt32.dll';

function CertOpenSystemStore(hProv :HCRYPTPROV;
                             szSubsystemProtocol :LPWSTR):HCERTSTORE ; stdcall; external 'crypt32.dll' name 'CertOpenSystemStoreW';

function CertFindCertificateInStore(hCertStore :HCERTSTORE;
                                    dwCertEncodingType :DWORD;
                                    dwFindFlags :DWORD;
                                    dwFindType :DWORD;
                              const pvFindPara :PVOID;
                                    pPrevCertContext :PCCERT_CONTEXT
                                    ):PCCERT_CONTEXT ; stdcall; external 'crypt32.dll';

function CertFindExtension(
  pszObjId    : PAnsiChar;
  cExtensions : DWORD;
  rgExtensions: PCERT_EXTENSION
): PCERT_EXTENSION; stdcall; external 'crypt32.dll';

const
  CRYPT_DECODE_NOCOPY_FLAG = 1;

  X509_NAME = PAnsiChar(7);

function CryptDecodeObject(
     dwCertEncodingType : DWORD;
     lpszStructType     : PAnsiChar;
     pbEncoded          : Pointer;
     cbEncoded          : DWORD;
     dwFlags            : DWORD;
     pvStructInfo       : Pointer;
 var pcbStructInfo      : DWORD
): BOOL; stdcall; external 'crypt32.dll';

function CryptDecodeObjectEx(
     dwCertEncodingType : DWORD;
     lpszStructType     : PAnsiChar;
     pbEncoded          : Pointer;
     cbEncoded          : DWORD;
     dwFlags            : DWORD;
     pDecodePara        : Pointer;// PCRYPT_DECODE_PARA;
     pvStructInfo       : Pointer;
 var pcbStructInfo      : DWORD
): BOOL; stdcall; external 'crypt32.dll';

function CertFindChainInStore(
         hCertStore:          HCERTSTORE;
         dwCertEncodingType:  DWORD;
         dwFindFlags:         DWORD;
         dwFindType:          DWORD;
   const pvFindPara: pointer;
         pPrevChainContext:  PCCERT_CHAIN_CONTEXT): PCCERT_CHAIN_CONTEXT; stdcall; external 'crypt32.dll';

function CertCloseStore(hCertStore :HCERTSTORE; dwFlags :DWORD):BOOL ; stdcall; external 'crypt32.dll';

function CryptProtectData(
  var pDataIn         : DATA_BLOB;
      ppszDataDecr    : PChar;
      pOptionalEntropy: PDATA_BLOB;
      pvReserved      : Integer;
      pPromptStruct   : PCRYPTPROTECT_PROMPTSTRUCT;
      dwFlags         : DWORD;
  var pDataOut        : DATA_BLOB
): BOOL; stdcall; external 'crypt32.dll';


function CryptUnprotectData(
  var pDataIn         : DATA_BLOB;
      ppszDataDecr    : PPChar;
      pOptionalEntropy: PDATA_BLOB;
      pvReserved      : Integer;
      pPromptStruct   : PCRYPTPROTECT_PROMPTSTRUCT;
      dwFlags         : DWORD;
  var pDataOut        : DATA_BLOB
): BOOL; stdcall; external 'crypt32.dll';


const
  CRYPT_USER_PROTECTED  = $00000002;
  PKCS12_NO_PERSIST_KEY = $00008000;

function PFXImportCertStore(
  var pPFX: CRYPT_DATA_BLOB;
      szPassword: PChar;
      dwFlags : DWORD
) : HCERTSTORE; stdcall; external 'crypt32.dll' name 'PFXImportCertStore';

function CertName(Cert: PCCERT_CONTEXT; var Blob: CERT_NAME_BLOB; Flags: DWORD = CERT_X500_NAME_STR or CERT_NAME_STR_NO_PLUS_FLAG or CERT_NAME_STR_REVERSE_FLAG): string;
function CertGetNameString(Cert: PCCERT_CONTEXT; dwType, dwFlags: DWORD; pvTypePara: Pointer): string;
function SerialNumber(const Number: CRYPT_INTEGER_BLOB): string;

implementation

function CertName(Cert: PCCERT_CONTEXT; var Blob: CERT_NAME_BLOB; Flags: DWORD = CERT_X500_NAME_STR or CERT_NAME_STR_NO_PLUS_FLAG or CERT_NAME_STR_REVERSE_FLAG): string;
var
  Len: Integer;
begin
  Len := CertNameToStr(Cert.dwCertEncodingType, Blob, Flags, nil, 0);
  if Len <= 1 then
    Exit('');
  SetLength(Result, Len - 1); // string has already an extra #0
  CertNameToStr(Cert.dwCertEncodingType, Blob, Flags, @Result[1], Len + 1);
end;

function CertGetNameString(Cert: PCCERT_CONTEXT; dwType, dwFlags: DWORD; pvTypePara: Pointer): string;
var
  Len: Integer;
begin
  dwFlags := dwFlags or CERT_NAME_SEARCH_ALL_NAMES_FLAG or CERT_NAME_STR_ENABLE_PUNYCODE_FLAG;
  Len := CertGetNameStringW(Cert, dwType, dwFlags, pvTypePara, nil, 0);
  if Len <= 1 then
    Result := ''
  else begin
    SetLength(Result, Len - 1);
    CertGetNameStringW(Cert, dwType, dwFlags, pvTypePara, Pointer(Result), Len);
  end;
end;

function SerialNumber(const Number: CRYPT_INTEGER_BLOB): string;
// FF00 -> 255,0  => 65280
// 1980 -> 25,128 -> 0
// 028C ->  2,140 -> 8
// 0041 ->  0, 65 -> 2
// 0006 ->  0,  6 -> 5
// 0000 ->      0 -> 6
var
  Source: Integer;
  Index : Integer;
  Value : Integer;
  Digits: Integer;
  Store : Integer;
  Add   : Integer;
  Bytes : TArray<Byte>;
begin
  Digits := Number.cbData;
  SetLength(Bytes, Digits);
    Move(Number.pbData^, Bytes[0], Digits);
  Result := '';
  Dec(Digits);
  while Digits >= 0 do
  begin
    Value := 0;
    for Index := Digits downto 0 do
    begin
      Value := Value * 256 + Bytes[Index];
      Bytes[Index] := Value div 10;
      Value := Value mod 10;
    end;
    Result := Char(Ord('0') + Value) + Result;
    if Bytes[Digits] = 0 then
      Dec(Digits);
  end;
end;

{ CERT_CONTEXT }

function CERT_CONTEXT.FindExtension(OID: PAnsiChar): PCERT_EXTENSION;
begin
  Result := CertFindExtension(OID, pCertInfo.cExtension, pCertInfo.rgExtension);
end;

procedure test();
var
  n: CRYPT_INTEGER_BLOB;
begin
  n.cbData := 2;
  n.pbData := #0#255;
  Assert(SerialNumber(n) = '65280');
end;

initialization
{$IFDEF WIN32}
  Assert(SizeOf(CERT_CHAIN_CONTEXT) = 56);
//  test();
{$ENDIF}
end.

