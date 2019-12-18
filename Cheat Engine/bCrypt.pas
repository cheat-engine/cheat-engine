unit bCrypt;

{$mode objfpc}{$H+}

interface

{$ifdef windows}

uses
  windows, Classes, SysUtils;

type
  NTSTATUS = ULONG;
  BCRYPT_ALG_HANDLE = handle;
  BCRYPT_KEY_HANDLE = handle;
  BCRYPT_HASH_HANDLE = handle;
  BCRYPT_HANDLE = handle;
  PBCRYPT_KEY_HANDLE = ^BCRYPT_KEY_HANDLE;

const
  BCRYPT_ALG_HANDLE_HMAC_FLAG = 8;
  BCRYPT_PROV_DISPATCH = 1;

  BCRYPT_RSAFULLPRIVATE_BLOB = 'RSAFULLPRIVATEBLOB';
  BCRYPT_RSAPUBLIC_BLOB      = 'RSAPUBLICBLOB';
  BCRYPT_RSAPRIVATE_BLOB     = 'RSAPRIVATEBLOB';

  BCRYPT_DSA_PUBLIC_BLOB     = 'DSAPUBLICBLOB';
  BCRYPT_DSA_PRIVATE_BLOB    = 'DSAPRIVATEBLOB';

  BCRYPT_ECCPUBLIC_BLOB           ='ECCPUBLICBLOB';
  BCRYPT_ECCPRIVATE_BLOB          ='ECCPRIVATEBLOB';



  BCRYPT_OBJECT_LENGTH        = 'ObjectLength';
  BCRYPT_ALGORITHM_NAME       = 'AlgorithmName';
  BCRYPT_PROVIDER_HANDLE      = 'ProviderHandle';
  BCRYPT_CHAINING_MODE        = 'ChainingMode';
  BCRYPT_BLOCK_LENGTH         = 'BlockLength';
  BCRYPT_KEY_LENGTH           = 'KeyLength';
  BCRYPT_KEY_OBJECT_LENGTH    = 'KeyObjectLength';
  BCRYPT_KEY_STRENGTH         = 'KeyStrength';
  BCRYPT_KEY_LENGTHS          = 'KeyLengths';
  BCRYPT_BLOCK_SIZE_LIST      = 'BlockSizeList';
  BCRYPT_EFFECTIVE_KEY_LENGTH = 'EffectiveKeyLength';
  BCRYPT_HASH_LENGTH          = 'HashDigestLength';
  BCRYPT_HASH_OID_LIST        = 'HashOIDList';
  BCRYPT_PADDING_SCHEMES      = 'PaddingSchemes';
  BCRYPT_SIGNATURE_LENGTH     = 'SignatureLength';
  BCRYPT_HASH_BLOCK_LENGTH    = 'HashBlockLength';
  BCRYPT_AUTH_TAG_LENGTH      = 'AuthTagLength';
  BCRYPT_PRIMITIVE_TYPE       = 'PrimitiveType';
  BCRYPT_IS_KEYED_HASH        = 'IsKeyedHash';


function initialize_bCrypt: boolean;

var BCryptOpenAlgorithmProvider: function(out phAlgoritm: BCRYPT_ALG_HANDLE; pszAlgID: PWCHAR; pszImplementation: PWCHAR; dwFlags: DWORD): NTSTATUS; stdcall;
var BCryptCloseAlgorithmProvider: function(hAlgoritm: BCRYPT_ALG_HANDLE; dwFlags: ULONG): NTSTATUS; stdcall;
var BCryptGenerateKeyPair : function(hAlgoritm: BCRYPT_ALG_HANDLE; out phKey: BCRYPT_KEY_HANDLE; dwLength: ULONG; dwFlags: ULONG):NTSTATUS; stdcall;
var BCryptDestroyKey: function(hKey: BCRYPT_KEY_HANDLE):NTSTATUS; stdcall;

var BCryptSetProperty: function(hObject: BCRYPT_HANDLE; pszProperty: pwidechar; pbInput: pointer; cbInput: ULONG; dwFlags: ULONG):NTSTATUS; stdcall;
var BCryptGetProperty: function(hObject: BCRYPT_HANDLE; pszProperty: pwidechar; pbOutput: pointer; cbOutput: ULONG; out pcbResult: ULONG; dwFlags: ULONG):NTSTATUS; stdcall;

var BCryptFinalizeKeyPair: function(hKey: BCRYPT_KEY_HANDLE; dwFlags: ULONG): NTSTATUS; stdcall;
var BCryptExportKey: function(hKey: BCRYPT_KEY_HANDLE; hExportKey: BCRYPT_KEY_HANDLE; pszBlobType: pwchar; pbOutput: pointer; cbOutput: ULONG; out pcbResult: ULONG; dwFlags: ULONG): NTSTATUS; stdcall;
var BCryptImportKeyPair: function(hAlgorithm: BCRYPT_HANDLE; hImportKey: BCRYPT_KEY_HANDLE; pszBlobType: pwchar; out phKey: BCRYPT_KEY_HANDLE; pbInput: pointer; cbInput: ULONG; dwFlags: ULONG): NTSTATUS; stdcall;
var BCryptImportKey: function(hAlgorithm: BCRYPT_ALG_HANDLE; hImportKey: BCRYPT_KEY_HANDLE; pszBlobType: pwchar; out phKey: BCRYPT_KEY_HANDLE; pbKeyObject: pointer; cbKeyObject: ULONG; pbInput: pointer; cbInput: ULONG; dwFlags: ULONG): NTSTATUS; stdcall;

var BCryptCreateHash: function(hAlgorithm: BCRYPT_HANDLE; out phHash: BCRYPT_HASH_HANDLE; pbHashObject: pointer; cbHashObject: ULONG; pbSecret: pointer; cbSecret: ULONG; dwFlags: ULONG): NTSTATUS; stdcall;
var BCryptHashData: function(hHash: BCRYPT_HASH_HANDLE; pbInput: pointer; cbInput: ULONG; dwFlags: ULONG): NTSTATUS; stdcall;
var BCryptFinishHash: function(hHash: BCRYPT_HASH_HANDLE; pbOutput: pointer; cbOutput: ULONG; dwFlags: ULONG): NTSTATUS; stdcall;

var BCryptSignHash: function(hKey: BCRYPT_KEY_HANDLE; pPaddingInfo: pointer; pbInput: pointer; cbInput: DWORD; pbOutput: pointer; cbOutput: DWORD; pcbResult: PDWORD; dwFlags: ULONG): NTSTATUS; stdcall;
var BCryptVerifySignature: function(hKey: BCRYPT_KEY_HANDLE; pPaddingInfo: pointer; pbHash: pointer; cbHash: DWORD; pbSignature: pointer; cbSignature: DWORD; dwFlags: ULONG): NTSTATUS; stdcall;

var BCryptDestroyHash: function(hKey: BCRYPT_HASH_HANDLE):NTSTATUS; stdcall;


var BCryptEncrypt: function(hKey: BCRYPT_KEY_HANDLE; pbInput: pointer; cbInput: ULONG; pPaddingInfo: PVOID; pbIV: PUCHAR; cbIV: ULONG; pbOutput: PUCHAR; cbOutput: ULONG; pcbResult: PULONG; dwflags: DWORD): NTSTATUS; stdcall;
var BCryptDecrypt: function(hKey: BCRYPT_KEY_HANDLE; pbInput: pointer; cbInput: ULONG; pPaddingInfo: PVOID; pbIV: PUCHAR; cbIV: ULONG; pbOutput: PUCHAR; cbOutput: ULONG; pcbResult: PULONG; dwflags: DWORD): NTSTATUS; stdcall;

{$endif}

implementation

var bcryptlib: HModule=0;

{$ifdef windows}

function initialize_bCrypt: boolean;
begin
  if bcryptlib=0 then
  begin
    bcryptlib:=LoadLibrary('bcrypt.dll');
    if bcryptlib<>0 then
    begin
      pointer(BCryptOpenAlgorithmProvider):=GetProcAddress(bcryptlib,'BCryptOpenAlgorithmProvider');
      pointer(BCryptCloseAlgorithmProvider):=GetProcAddress(bcryptlib,'BCryptCloseAlgorithmProvider');
      pointer(BCryptGenerateKeyPair):=GetProcAddress(bcryptlib,'BCryptGenerateKeyPair');
      pointer(BCryptDestroyKey):=GetProcAddress(bcryptlib,'BCryptDestroyKey');
      pointer(BCryptSetProperty):=GetProcAddress(bcryptlib,'BCryptSetProperty');
      pointer(BCryptGetProperty):=GetProcAddress(bcryptlib,'BCryptGetProperty');
      pointer(BCryptFinalizeKeyPair):=GetProcAddress(bcryptlib,'BCryptFinalizeKeyPair');
      pointer(BCryptExportKey):=GetProcAddress(bcryptlib,'BCryptExportKey');
      pointer(BCryptImportKeyPair):=GetProcAddress(bcryptlib,'BCryptImportKeyPair');
      pointer(BCryptImportKey):=GetProcAddress(bcryptlib,'BCryptImportKey');
      pointer(BCryptCreateHash):=GetProcAddress(bcryptlib,'BCryptCreateHash');
      pointer(BCryptHashData):=GetProcAddress(bcryptlib,'BCryptHashData');
      pointer(BCryptFinishHash):=GetProcAddress(bcryptlib,'BCryptFinishHash');
      pointer(BCryptSignHash):=GetProcAddress(bcryptlib,'BCryptSignHash');
      pointer(BCryptVerifySignature):=GetProcAddress(bcryptlib,'BCryptVerifySignature');
      pointer(BCryptDestroyHash):=GetProcAddress(bcryptlib,'BCryptDestroyHash');
      pointer(BCryptEncrypt):=GetProcAddress(bcryptlib,'BCryptEncrypt');
      pointer(BCryptDecrypt):=GetProcAddress(bcryptlib,'BCryptDecrypt');
    end;
  end;

  result:=(bcryptlib<>0) and assigned(BCryptOpenAlgorithmProvider) and assigned(BCryptVerifySignature) and assigned(BCryptSignHash) and assigned(BCryptImportKeyPair);


end;
  {$endif}


end.

