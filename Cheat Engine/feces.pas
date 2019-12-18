unit feces;
//friends endorsing cheat engine system

{$mode objfpc}{$H+}

interface

{$ifdef windows}
uses
  Classes, SysUtils, bcrypt, DOM, xmlutils, XmlRead, XMLWrite, dialogs, windows,
  graphics, math;

function canSignTables: boolean;
procedure signTable(cheattable: TDOMElement);
procedure signTableFile(f: string);
function isProperlySigned(cheattable: TDOMElement; out specialstring: string; out imagepos: integer; out image: tpicture): boolean;

{$endif}

implementation

{$ifdef windows}

uses cefuncproc, CustomBase85, registry, formsettingsunit;

resourcestring
  rsFailedToGetSignatureSize = 'Failed to get the signature size';
  rsFailedToFinishTheHash = 'Failed to finish the hash';
  rsFailedToGetHashlength = 'Failed to get hashlength';
  rsFailedHashingTable = 'Failed hashing table';
  rsFailedCreatingHash = 'Failed creating hash';
  rsFailedGettingTheObjectLength = 'Failed getting the object length';
  rsFailedCreatingHasAlgorithmProvider = 'Failed creating has algorithm '
    +'provider';
  rsFailedToLoadPrivateKey = 'Failed to load private key';
  rsIsPasswordCorrect='Is the password correct?';
  rsCouldNotOpenTheAlgorithmProvider = 'Could not open the algorithm provider';
  rsBcryptCouldNotBeUsed = 'bcrypt could not be used';
  rsSelectYourCheatEngineSignatureFile = 'Select your cheat engine signature '
    +'file';
  rsCheatEngineSignatureFiles = 'Cheat engine signature files';
  rsThisTableHasBeenModified = 'This table has been modified. To load this '
    +'table, remove the signature part with an editor (And check the file for '
    +'suspicious things while you''re at it)';
  rsFailedToFinishTheHash2 = 'Failed to finish the hash 2';
  rsFailedToGetHashlength2 = 'Failed to get hashlength 2';
  rsFailedHashingTable2 = 'Failed hashing table 2';
  rsFailedCreatingHash2 = 'Failed creating hash 2';
  rsInvalidPublicKey = 'The provided public key is invalid(Not signed by the '
    +'Cheat Engine guy). Remove the signature section to load this table';
  rsFailedCreatingHasAlgorithmProvider2 = 'Failed creating has algorithm '
    +'provider';
  rsFailedToLoadTheTablePublicKey = 'Failed to load the table public key';
  rsFailedToLoadCheatEnginePublicKey = 'Failed to load cheat engine public key';
  rsNoSignedHash = 'This table''s signature does not contain a SignedHash '
    +'element';
  rsNoPublicKey =
    'This table''s signature does not contain a PublicKey element';


var
  cheatenginepublictablekey: BCRYPT_KEY_HANDLE=0;
  publictablekey: array [0..139] of byte =($45, $43, $53, $35, $42, $00, $00,
   $00, $01, $A3, $7A, $45, $2A, $66, $60, $85, $C7, $50, $9D, $8C, $3F, $34,
   $57, $D3, $FF, $50, $E3, $32, $CA, $4C, $4D, $61, $9B, $00, $19, $7E, $61,
   $6B, $1F, $52, $50, $7E, $01, $94, $8B, $F0, $A4, $91, $49, $FC, $58, $32,
   $D8, $43, $60, $F7, $F1, $46, $F5, $CB, $A0, $AB, $0B, $26, $D4, $1D, $9D,
   $BE, $40, $C8, $12, $30, $CA, $15, $01, $30, $A9, $4D, $03, $6E, $4E, $4A,
   $5E, $85, $CF, $85, $5D, $D7, $24, $47, $36, $A6, $25, $2B, $B0, $48, $7E,
   $95, $8F, $F2, $9A, $FF, $B3, $C9, $C9, $97, $85, $FB, $59, $4F, $8A, $D4,
   $FF, $A4, $80, $A4, $AE, $92, $B8, $48, $64, $74, $05, $7C, $97, $90, $A3,
   $7E, $0E, $72, $76, $5B, $B4, $D8, $18, $E5, $A6, $A2, $E3, $47);

threadvar pathtosigfile: pchar;
threadvar passwordhash: pbyte;

var passwordhashlength: integer;

//useless protection but hey, why not

type TCanSign=(csUnknown, csYes, csNo);

var
  EncodePointer:function(p: pointer):pointer; stdcall;
  DecodePointer:function(p: pointer):pointer; stdcall;

  _cansignstate: TCanSign=csUnknown;

var rv: dword=0;
function EncodePointerNI(p: pointer):pointer; stdcall; //not implemented (unpatched XP)
begin
  if rv=0 then
    rv:=1+random($fffffffd);

  result:=pointer(ptruint(p) xor rv);
end;

function DecodePointerNI(p: pointer):pointer; stdcall;
begin
  if rv=0 then exit(p);

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

function isProperlySigned(cheattable: TDOMElement; out specialstring: string; out imagepos: integer; out image: TPicture): boolean;
var
  signature: TDOMNode;
  publicKey: TDOMNode;
  signedhash: TDOMNode;

  size: integer;

  tablepublickey: BCRYPT_KEY_HANDLE=0;

  hAlgoritm: BCRYPT_ALG_HANDLE=0;
  hashAlgoritm: BCRYPT_ALG_HANDLE=0;

  hHash: BCRYPT_HASH_HANDLE=0;

  publickeysize: integer;
  publickeyblock: pointer=nil;

  publicdata: tmemorystream=nil;
  keysize: integer;

  signaturesize: integer;
  sig: pointer;

  objectlength: dword;
  bHashObject: pointer=nil;
  hashlength: integer;
  hashbuffer: pointer=nil;


  customstring: string;
  s: ntstatus;
  x: dword;
  ps: pchar;
  str: string;

  cheattablecontents: TMemorystream=nil;

  tablesignature: pointer=nil;
  tablesignaturesize: integer;

  sigversion: word;

  imagebuf: pointer;
  imagestream: TMemorystream;
begin
  image:=nil;

  if not initialize_bCrypt then
    exit(false);

  signature:=CheatTable.FindNode('Signature');
  if signature=nil then exit(false);

  signedhash:=signature.FindNode('SignedHash');
  if signedhash=nil then
    raise exception.create(rsNoSignedHash);


  tablesignaturesize:=strtoint(TDOMElement(signedhash).Attributes.GetNamedItem('HashSize').TextContent);
  try
    getmem(tablesignature, tablesignaturesize);
    str:=signedhash.TextContent;
    x:=Base85ToBin(pchar(str), tablesignature);
    if x<>tablesignaturesize then
      raise exception.create('a thing happened');

    //get the public key from the signature
    publicKey:=signature.FindNode('PublicKey');
    if publickey=nil then
      raise exception.create(rsNoPublicKey);


    publickeysize:=strtoint(TDOMElement(publickey).Attributes.GetNamedItem('Size').TextContent);

    getmem(publickeyblock, publickeysize);

    str:=publickey.TextContent;
    x:=Base85ToBin(pchar(str), publickeyblock);


    publicdata:=tmemorystream.create;
    publicdata.WriteBuffer(publickeyblock^, publickeysize);
    publicdata.position:=0;
    publicdata.Seek(0, soFromBeginning);

    customstring:=publicdata.ReadAnsiString;
    specialstring:=customstring;
    keysize:=publicdata.ReadWord;
    sigversion:=publicdata.ReadWord;


    s:=BCryptOpenAlgorithmProvider(hAlgoritm, 'ECDSA_P521', nil, 0);
    if not succeeded(s) then raise exception.create(
      rsCouldNotOpenTheAlgorithmProvider);

    if cheatenginepublictablekey=0 then
    begin
      s:=BCryptImportKeyPair(hAlgoritm, 0, BCRYPT_ECCPUBLIC_BLOB, cheatenginepublictablekey, @publictablekey[0], 140, 0);
      if not succeeded(s) then raise exception.create(
        rsFailedToLoadCheatEnginePublicKey);
    end;

    //load the public key of this table while we're at it
    s:=BCryptImportKeyPair(hAlgoritm, 0, BCRYPT_ECCPUBLIC_BLOB, tablepublickey, pointer(ptruint(publicdata.Memory)+publicdata.position), keysize, 0);
    if not succeeded(s) then raise exception.create(
      rsFailedToLoadTheTablePublicKey);


    publicdata.Position:=publicdata.Position+keysize;

    imagepos:=-1;
    image:=nil;

    if sigversion>=1 then
    begin
      imagepos:=publicdata.ReadByte;
      if imagepos<>0 then
      begin
        x:=publicdata.readdword; //image size

        imagestream:=tmemorystream.create;
        getmem(imagebuf, x);
        try
          publicdata.ReadBuffer(imagebuf^, x);
          imagestream.writebuffer(imagebuf^,x);
          imagestream.position:=0;

          image:=TPicture.Create;
          image.LoadFromStream(imagestream);

        finally
          imagestream.free;
          FreeMemAndNil(imagebuf);
        end;
      end;
    end;



    publickeysize:=publicdata.position; //reuse this


    signaturesize:=publicdata.ReadDWord;
    sig:=pointer(ptruint(publicdata.memory)+publicdata.position);

    //check the signature of the public key with publictablekey
    //create a hash of the data in publicdata.memory to publicdata.memory+publickeysize
    s:=BCryptOpenAlgorithmProvider(hashAlgoritm, 'SHA512', nil, 0);
    if not succeeded(s) then raise exception.create(rsFailedCreatingHasAlgorithmProvider2);

    objectlength:=0;
    s:=BCryptGetProperty(hashAlgoritm, BCRYPT_OBJECT_LENGTH, @objectlength, sizeof(DWORD), size, 0);
    if not succeeded(s) then raise exception.create(rsFailedGettingTheObjectLength);

    getmem(bHashObject, objectlength);
    zeromemory(bHashObject, objectlength);

    hHash:=0;
    s:=BCryptCreateHash(hashAlgoritm, hHash, bHashObject, objectlength, nil, 0, 0);
    if not Succeeded(s) then raise exception.create(rsFailedCreatingHash);

    s:=BCryptHashData(hHash, publicdata.Memory, publickeysize, 0);
    if not Succeeded(s) then raise exception.create(rsFailedHashingTable);

    s:=BCryptGetProperty(hashAlgoritm, BCRYPT_HASH_LENGTH, @hashlength, sizeof(DWORD), size, 0);
    if not succeeded(s) then raise exception.create(rsFailedToGetHashlength);

    getmem(hashbuffer, hashlength);
    s:=BCryptFinishHash(hHash, hashbuffer, hashlength, 0);
    if not succeeded(s) then raise exception.create(rsFailedToFinishTheHash);


    //now verify this hash with the signature and the ce public key

    s:=BCryptVerifySignature(cheatenginepublictablekey,nil,hashbuffer,hashlength,sig, signaturesize,0);
    if not succeeded(s) then raise exception.create(rsInvalidPublicKey);

    //still here so the public key is valid

    BCryptDestroyHash(hHash);
    hHash:=0;
    FreeMemAndNil(hashbuffer);
    hashbuffer:=nil;

    //now hash the table(without signature section) and verify that with the 'SignedHash'

    cheattablecontents:=TMemoryStream.create;
    getXmlfileWithoutSignature(cheattable, cheattablecontents);

    s:=BCryptCreateHash(hashAlgoritm, hHash, bHashObject, objectlength, nil, 0, 0);
    if not Succeeded(s) then raise exception.create(rsFailedCreatingHash2);

    s:=BCryptHashData(hHash, cheattablecontents.Memory, cheattablecontents.size, 0);
    if not Succeeded(s) then raise exception.create(rsFailedHashingTable2);

    s:=BCryptGetProperty(hashAlgoritm, BCRYPT_HASH_LENGTH, @hashlength, sizeof(DWORD), size, 0);
    if not succeeded(s) then raise exception.create(rsFailedToGetHashlength2);

    getmem(hashbuffer, hashlength);
    s:=BCryptFinishHash(hHash, hashbuffer, hashlength, 0);
    if not succeeded(s) then raise exception.create(rsFailedToFinishTheHash2);


    s:=BCryptVerifySignature(tablepublickey,nil,hashbuffer,hashlength,tablesignature, tablesignaturesize,0);
    if not succeeded(s) then raise exception.create(rsThisTableHasBeenModified);


    result:=true;






  finally
    if tablesignature<>nil then
      FreeMemAndNil(tablesignature);

    if publickeyblock<>nil then
      FreeMemAndNil(publickeyblock);

    if publicdata<>nil then
      freeandnil(publicdata);

    if hAlgoritm<>0 then
      BCryptCloseAlgorithmProvider(hAlgoritm,0);

    if hashAlgoritm<>0 then
      BCryptCloseAlgorithmProvider(hashAlgoritm,0);

    if hHash<>0 then
      BCryptDestroyHash(hHash);

    if cheattablecontents<>nil then
      freeandnil(cheattablecontents);

    if hashbuffer<>nil then
      FreeMemAndNil(hashbuffer);


    if tablepublickey<>0 then
      BCryptDestroyKey(tablepublickey);
  end;



  //check the signatureless version of the cheat table with this public key
end;


function canSignTables: boolean;
var reg: tregistry;
begin
  if _cansignstate=csUnknown then
  begin
    result:=FileExists(GetCEdir+'cansign.txt') or FileExists(GetCEDir+'mysignature.cesig');

    if result then
      _cansignstate:=csYes
    else
      _cansignstate:=csNo;
    exit;
  end;

  result:=_cansignstate=csYes;
end;

procedure signTableFile(f: string);
var
  d: TXMLDocument;
  e: TDOMElement;
begin
  ReadXMLFile(d, f);
  e:=TDOMElement(d.FindNode('CheatTable'));
  signtable(e);
  WriteXMLFile(d,f);
end;

procedure generateHash(password: pointer; passwordsize: integer; var hash: pointer; var hashsize: integer);
var
  s: ntstatus;
  hashAlgoritm: BCRYPT_ALG_HANDLE;
  hhash: BCRYPT_HASH_HANDLE;
  objectlength: dword;
  bHashObject: pointer;

  size: ulong;
  i,j: integer;
  secondaryvalue: byte;
begin
  hash:=nil;
  hashsize:=0;
  if passwordsize=0 then exit;

  s:=BCryptOpenAlgorithmProvider(hashAlgoritm, 'SHA512', nil, 0);
  if succeeded(s) then
  begin
    objectlength:=0;
    s:=BCryptGetProperty(hashAlgoritm, BCRYPT_OBJECT_LENGTH, @objectlength, sizeof(DWORD), size, 0);
    if succeeded(s) then
    begin
      getmem(bHashObject, objectlength);
      zeromemory(bHashObject, objectlength);
      hHash:=0;
      s:=BCryptCreateHash(hashAlgoritm, hHash, bHashObject, objectlength, nil, 0, 0);
      if succeeded(s) then
      begin
        s:=BCryptHashData(hHash, password, passwordsize, 0);
        if succeeded(s) then
        begin
          s:=BCryptGetProperty(hashAlgoritm, BCRYPT_HASH_LENGTH, @hashsize, sizeof(DWORD), size, 0);
          if succeeded(s) then
          begin
            getmem(hash, hashsize);
            s:=BCryptFinishHash(hHash, hash, hashsize, 0);

          end;
        end;
        BCryptDestroyHash(hashAlgoritm);
      end;
      FreeMemAndNil(bHashObject);
      BCryptCloseAlgorithmProvider(hashAlgoritm,0);
    end;
  end;
end;

procedure getPasswordHash(password: string; out pwhash: pointer; out pwhashlength: integer; wantedsize: integer);
var
  hash: array of byte;

  initialHash: pbyte;
  initialhashsize: integer;

  partialhash: pbyte;
  partialhashsize: integer;

  hashpos: integer;
  copysize: integer;
  i,j: integer;
begin
  pwhash:=nil;
  if password='' then exit;

  setlength(hash,wantedsize);


  //generate hashes until it's the size of the buffer
  hashpos:=0;

  generateHash(@password[1],length(password),initialhash,initialhashsize);

  j:=1;
  for i:=0 to initialhashsize-1 do
  begin
    initialhash[i]:=initialhash[i] xor ord(password[j]);
    inc(j);
    if j>length(password) then j:=1;
  end;

  generateHash(initialhash,initialhashsize,partialhash,partialhashsize);
  FreeMemAndNil(initialhash);

  copysize:=ifthen(partialhashsize>wantedsize, wantedsize, partialhashsize);
  copymemory(@hash[0],partialhash, copysize);
  FreeMemAndNil(partialhash);

  inc(hashpos, copysize);

  while hashpos<wantedsize do
  begin
    generateHash(@hash[hashpos-copysize],copysize,partialhash, partialhashsize);

    copysize:=ifthen(partialhashsize+hashpos>wantedsize, wantedsize-hashpos, partialhashsize);
    copymemory(@hash[hashpos],partialhash, copysize);
    FreeMemAndNil(partialhash);
    inc(hashpos,copysize);
  end;

  getmem(pwhash, wantedsize);
  copymemory(pwhash, @hash[0],wantedsize);

  setlength(hash,0);
end;


procedure passwordDecode(buffer: pbyte; buffersize: integer; pwhash: pbyte);
var
  i: integer;
begin
  for i:=0 to buffersize-1 do
    buffer[i]:=buffer[i] xor pwhash[i];
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
  sigversion: word;
  password: string;

  pwhash: pbyte;
  pwhashlength: integer;
begin
  if not initialize_bCrypt then
    raise exception.create(rsBcryptCouldNotBeUsed);

  doc:=cheattable.OwnerDocument;

  //get the private key from the signature file
  if FileExists(GetCEDir+'mysignature.cesig') then
    pathtosigfile:=encodepointer(strnew(pchar(GetCEDir+'mysignature.cesig')));

  if (pathtosigfile=nil) or (not FileExists(pchar(decodepointer(pathtosigfile)))) then
  begin
    od:=TOpenDialog.Create(nil);
    try
      od.Title:=rsSelectYourCheatEngineSignatureFile;
      od.Filter:=rsCheatEngineSignatureFiles+'|*.CESIG';
      od.Options:=od.Options+[ofFileMustExist, ofDontAddToRecent];
      if od.execute then
        pathtosigfile:=encodepointer(strnew(pchar(od.FileName)))
      else
        exit;
    finally
      od.free;
    end;
  end;

  try
    s:=BCryptOpenAlgorithmProvider(hAlgoritm, 'ECDSA_P521', nil, 0);
    if not succeeded(s) then raise exception.create(rsCouldNotOpenTheAlgorithmProvider);

    m:=tmemorystream.create();
    m.LoadFromFile(pchar(decodepointer(pathtosigfile)));

    x:=m.ReadDWord; //customstring length
    m.position:=m.position+x; //string
    x:=m.ReadWord; //public key size (140)
    sigversion:=m.ReadWord; //version

    password:='';
    pwhash:=nil;
    if sigversion>=2 then
    begin
      if (passwordhash=nil) or formsettings.cbAlwaysAskForPassword.checked then
      begin
        if passwordhash<>nil then
        begin
          FreeMem(decodepointer(passwordhash));
          passwordhash:=nil;
        end;

        if InputQuery('CE Signature', 'Enter your password', true, password)=false then exit;
          getPasswordHash(password, pwhash, pwhashlength, m.size-m.position);
      end
      else
      begin
        pwhash:=decodepointer(passwordhash);
        pwhashlength:=passwordhashlength;
      end;
      passwordhash:=nil; //in case it's wrong/changed

      if pwhash<>nil then
        passwordDecode(pointer(ptruint(m.memory)+m.position), m.size-m.position, pwhash);
    end;

    try
      m.position:=m.position+x; //public key
      if sigversion>=1 then
      begin
        x:=m.ReadByte;
        if x<>0 then
        begin
          x:=m.readdword; //image size
          m.position:=m.position+x //skip the image
        end;
      end;
      x:=m.ReadDWord; //signature size
      m.position:=m.position+x; //signature describing the custom string+image and public key

      publicsectionsize:=m.position;

      x:=m.readdword; //private key size

      s:=BCryptImportKeyPair(hAlgoritm, 0, BCRYPT_ECCPRIVATE_BLOB, hKey, pointer(ptruint(m.memory)+m.position), x, 0);
      if not succeeded(s) then raise exception.create(rsFailedToLoadPrivateKey);

      publicdata:=tmemorystream.create;
      m.position:=0;
      publicdata.CopyFrom(m,publicsectionsize);

      FillMemory(m.memory,m.size,$ce);
      freeandnil(m);

      //get the hashless version of this table
      cheattablecontents:=TMemoryStream.create;
      getXmlfileWithoutSignature(cheattable, cheattablecontents);

      //showmessage(pchar(cheattablecontents.Memory));

      //generate a hash based on it
      s:=BCryptOpenAlgorithmProvider(hashAlgoritm, 'SHA512', nil, 0);
      if not succeeded(s) then raise exception.create(rsFailedCreatingHasAlgorithmProvider);

      objectlength:=0;
      s:=BCryptGetProperty(hashAlgoritm, BCRYPT_OBJECT_LENGTH, @objectlength, sizeof(DWORD), size, 0);
      if not succeeded(s) then raise exception.create(rsFailedGettingTheObjectLength);

      getmem(bHashObject, objectlength);
      zeromemory(bHashObject, objectlength);

      hHash:=0;
      s:=BCryptCreateHash(hashAlgoritm, hHash, bHashObject, objectlength, nil, 0, 0);
      if not Succeeded(s) then raise exception.create(rsFailedCreatingHash);

      s:=BCryptHashData(hHash, cheattablecontents.Memory, cheattablecontents.size, 0);
      if not Succeeded(s) then raise exception.create(rsFailedHashingTable);

      freeandnil(cheattablecontents);

      s:=BCryptGetProperty(hashAlgoritm, BCRYPT_HASH_LENGTH, @hashlength, sizeof(DWORD), size, 0);
      if not succeeded(s) then raise exception.create(rsFailedToGetHashlength);

      getmem(hashbuffer, hashlength);
      s:=BCryptFinishHash(hHash, hashbuffer, hashlength, 0);
      if not succeeded(s) then raise exception.create(rsFailedToFinishTheHash);

      //sign that hash with hKey and add it to the table
      signsize:=0;
      s:=BCryptSignHash(hKey, nil, hashbuffer, hashlength, nil, 0, @signsize, 0);
      if not succeeded(s) then raise exception.create(rsFailedToGetSignatureSize);

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
      FreeMemAndNil(tempstr);


      //and add the public key to the table as well
      publickey:=Signature.AppendChild(doc.CreateElement('PublicKey'));
      getmem(tempstr, (publicdata.Size div 4) * 5 + 5 );
      BinToBase85(publicdata.Memory, tempstr,publicdata.Size);
      publickey.TextContent:=tempstr;
      TDOMElement(publickey).SetAttribute('Size',IntToStr(publicdata.Size));

      FreeMemAndNil(tempstr);


      if pwhash<>nil then
      begin
        if not formsettings.cbAlwaysAskForPassword.checked then
        begin
          passwordhash:=encodepointer(pwhash); //so it's not needed to ask again
          passwordhashlength:=pwhashlength;
        end
        else
          FreeMemAndNil(pwhash);
      end;
    except
      on e:exception do
        if pwhash<>nil then
          raise exception.create(e.Message+#13#10+'('+rsIsPasswordCorrect+')')
        else
          raise;
    end;
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
      FreeMemAndNil(bHashObject);

    if hashbuffer<>nil then
      FreeMemAndNil(hashbuffer);

    if signedbuffer<>nil then
      FreeMemAndNil(signedbuffer);

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

{$endif}

end.

