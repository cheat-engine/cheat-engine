unit feces;
//friends endorsing cheat engine system

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bcrypt, DOM, xmlutils, XmlRead, XMLWrite, dialogs, windows;

procedure signTable(cheattable: TDOMElement);
function isProperlySigned(cheattable: TDOMElement): boolean;

implementation

uses cefuncproc, CustomBase85;


var publictablekey: array [0..139] of byte =($45, $43, $53, $35, $42, $00, $00, $00,
$01, $A3, $7A, $45, $2A, $66, $60, $85, $C7, $50, $9D, $8C, $3F, $34, $57, $D3,
$FF, $50, $E3, $32, $CA, $4C, $4D, $61, $9B, $00, $19, $7E, $61, $6B, $1F, $52,
$50, $7E, $01, $94, $8B, $F0, $A4, $91, $49, $FC, $58, $32, $D8, $43, $60, $F7,
$F1, $46, $F5, $CB, $A0, $AB, $0B, $26, $D4, $1D, $9D, $BE, $40, $C8, $12, $30,
$CA, $15, $01, $30, $A9, $4D, $03, $6E, $4E, $4A, $5E, $85, $CF, $85, $5D, $D7,
$24, $47, $36, $A6, $25, $2B, $B0, $48, $7E, $95, $8F, $F2, $9A, $FF, $B3, $C9,
$C9, $97, $85, $FB, $59, $4F, $8A, $D4, $FF, $A4, $80, $A4, $AE, $92, $B8, $48,
$64, $74, $05, $7C, $97, $90, $A3, $7E, $0E, $72, $76, $5B, $B4, $D8, $18, $E5,
$A6, $A2, $E3, $47);

threadvar pathtosigfile: pchar;

//useless protection but hey, why not
var
  EncodePointer:function(p: pointer):pointer; stdcall;
  DecodePointer:function(p: pointer):pointer; stdcall;

var rv: dword=0;
function EncodePointerNI(p: pointer):pointer; stdcall; //not implemented (unpatched XP)
begin
  if rv=0 then
    rv:=1+random($fffffffd);

  result:=pointer(ptruint(p) xor rv);
end;

function DecodePointerNI(p: pointer):pointer; stdcall;
begin
  if rv=0 then exit;

  result:=pointer(ptruint(p) xor rv);
end;



procedure getXmlfileWithoutSignature(cheattable: TDOMElement; output: tstream);
var
  signature: TDOMNode;
begin
  signature:=CheatTable.FindNode('Signature');

  if signature<>nil then
  begin
    CheatTable.DetachChild(signature);
    signature.free;
  end;

  if cheattable<>nil then
  begin
    //showmessage(cheattable.TextContent);
    WriteXML(cheattable,output);
//    output.WriteAnsiString(cheattable.TextContent);
  end;
end;

function isProperlySigned(cheattable: TDOMElement): boolean;
begin
  //get the public key from the signature
  //check the signature of the public key with publictablekey

  //check the signatureless version of the cheat table with this public key

end;

procedure signTable(cheattable: TDOMElement);
var
  f: tfilestream;
  od: TOpenDialog;

  s: ntstatus;

  hAlgoritm: BCRYPT_ALG_HANDLE=0;
  hashAlgoritm: BCRYPT_ALG_HANDLE=0;
  hkey: BCRYPT_KEY_HANDLE=0;
  hHash: BCRYPT_HASH_HANDLE=0;
  m: tmemorystream=nil;
  publicdata: tmemorystream=nil;

  cheattablecontents: TMemorystream=nil;

  x: dword;
  publicsectionsize: integer;
  size: dword;
  objectlength: dword;
  bHashObject: pointer=nil;
  hashlength: integer;
  hashbuffer: pointer=nil;

  signsize,signsize2: integer;
  signedbuffer: pointer=nil;

  signature: TDOMNode;
  signedhash: TDOMNode;
  publickey: TDOMNode;
  doc: TDOMDocument;
  xx: TDOMNode;

  tempstr: pchar=nil;
begin
  if not initialize_bCrypt then
    raise exception.create('bcrypt could not be used');

   pathtosigfile:=strnew('e:\zzz.cesig');

   doc:=cheattable.OwnerDocument;



  //get the private key from the signature file
  if pathtosigfile=nil then
  begin
    od:=TOpenDialog.Create(nil);
    try
      od.Title:='Select your cheat engine signature file';
      od.Filter:='Cheat engine signature files|*.CESIG';
      od.Options:=od.Options+[ofFileMustExist, ofDontAddToRecent];
      if od.execute then
        pathtosigfile:=strnew(pchar(od.FileName));
    finally
      od.free;
    end;
  end;

  try
    s:=BCryptOpenAlgorithmProvider(hAlgoritm, 'ECDSA_P521', nil, 0);
    if not succeeded(s) then raise exception.create('Could not open the algorithm provider');

    m:=tmemorystream.create();
    m.LoadFromFile(pathtosigfile);

    x:=m.ReadDWord; //customstring length
    m.position:=m.position+x; //string
    x:=m.ReadDWord; //public key size (140)
    m.position:=m.position+x; //public key
    x:=m.ReadDWord; //signature size
    m.position:=m.position+x; //signature describing the custom string and public key

    publicsectionsize:=m.position;

    x:=m.readdword; //private key size

    s:=BCryptImportKeyPair(hAlgoritm, 0, BCRYPT_ECCPRIVATE_BLOB, hKey, pointer(ptruint(m.memory)+m.position), x, 0);
    if not succeeded(s) then raise exception.create('Failed to load private key');

    publicdata:=tmemorystream.create;
    m.position:=0;
    publicdata.CopyFrom(m,publicsectionsize);

    FillMemory(m.memory,m.size,$ce);
    freeandnil(m);

    //get the hashless version of this table
    cheattablecontents:=TMemoryStream.create;
    getXmlfileWithoutSignature(cheattable, cheattablecontents);

    //generate a hash based on it
    s:=BCryptOpenAlgorithmProvider(hashAlgoritm, 'SHA512', nil, 0);
    if not succeeded(s) then raise exception.create('Failed creating has algorithm provider');

    objectlength:=0;
    s:=BCryptGetProperty(hashAlgoritm, BCRYPT_OBJECT_LENGTH, @objectlength, sizeof(DWORD), size, 0);
    if not succeeded(s) then raise exception.create('Failed getting the object length');

    getmem(bHashObject, objectlength);
    zeromemory(bHashObject, objectlength);

    hHash:=0;
    s:=BCryptCreateHash(hashAlgoritm, hHash, bHashObject, objectlength, nil, 0, 0);
    if not Succeeded(s) then raise exception.create('Failed creating hash');

    s:=BCryptHashData(hHash, cheattablecontents.Memory, cheattablecontents.size, 0);
    if not Succeeded(s) then raise exception.create('Failed hashing table');

    freeandnil(cheattablecontents);

    s:=BCryptGetProperty(hashAlgoritm, BCRYPT_HASH_LENGTH, @hashlength, sizeof(DWORD), size, 0);
    if not succeeded(s) then raise exception.create('Failed to get hashlength');

    getmem(hashbuffer, hashlength);
    s:=BCryptFinishHash(hHash, hashbuffer, hashlength, 0);
    if not succeeded(s) then raise exception.create('Failed to finish the hash');

    //sign that hash with hKey and add it to the table
    signsize:=0;
    s:=BCryptSignHash(hKey, nil, hashbuffer, hashlength, nil, 0, @signsize, 0);
    if not succeeded(s) then raise exception.create('Failed to get the signature size');

    getmem(signedbuffer, signsize);
    signsize2:=0;
    ZeroMemory(signedbuffer, signsize);
    s:=BCryptSignHash(hKey, nil, hashbuffer, hashlength, signedbuffer, signsize, @signsize2, 0);
    if not succeeded(s) then raise exception.create('Failed to sign the hash');



    signature:=CheatTable.AppendChild(doc.CreateElement('Signature'));
    signedhash:=Signature.AppendChild(doc.CreateElement('SignedHash'));

    getmem(tempstr, (signsize2 div 4) * 5 + 5 );
    BinToBase85(signedbuffer, tempstr,signsize2);

    signedhash.TextContent:=tempstr;
    TDOMElement(signedhash).SetAttribute('HashSize',IntToStr(signsize2));
    freemem(tempstr);
    tempstr:=nil;

    //and add the public key to the table as well
    publickey:=Signature.AppendChild(doc.CreateElement('PublicKey'));
    getmem(tempstr, (publicdata.Size div 4) * 5 + 5 );
    BinToBase85(publicdata.Memory, tempstr,publicdata.Size);
    publickey.TextContent:=tempstr;
    TDOMElement(publickey).SetAttribute('Size',IntToStr(publicdata.Size));

    freemem(tempstr);
    tempstr:=nil;

  finally
    if m<>nil then
    begin
      FillMemory(m.memory,m.size,$ce);
      freeandnil(m);
    end;
    if publicdata<>nil then publicdata.free;
    if hkey<>0 then BCryptDestroyKey(hkey);
    if hAlgoritm<>0 then BCryptCloseAlgorithmProvider(hAlgoritm,0);
    if hHash<>0 then BCryptDestroyHash(hHash);

    if hashAlgoritm<>0 then BCryptCloseAlgorithmProvider(hashAlgoritm,0);

    if cheattablecontents<>nil then
      freeandnil(cheattablecontents);

    if bhashobject<>nil then
      freemem(bHashObject);

    if hashbuffer<>nil then
      freemem(hashbuffer);

    if signedbuffer<>nil then
      freemem(signedbuffer);

    if tempstr<>nil then
      freeandnil(tempstr);
  end;


end;

var k32: HMODULE;
initialization
  k32:=GetModuleHandle('kernel32.dll');
  pointer(encodepointer):=GetProcAddress(k32,'EncodePointer');
  pointer(decodepointer):=GetProcAddress(k32,'DecodePointer');

  if not assigned(encodepointer) then
    (encodepointer):=@EncodePointerNI;

  if not assigned(decodepointer) then
    (decodepointer):=@DecodePointerNI;

end.

