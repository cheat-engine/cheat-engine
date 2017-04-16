unit feces;
//friends endorsing cheat engine system

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bcrypt, DOM, xmlutils, XmlRead, XMLWrite;

function isFecesMember: boolean;
procedure signTable(table: TXMLDocument);
function isProperlySigned(table: TXMLDocument): boolean;

implementation

uses cefuncproc;


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

function removesignature(table: TXMLDocument): string;
begin
  //removes the <signature> section if there is any, and convert the result into a string

end;

function isProperlySigned(table: TXMLDocument): boolean;
begin
  //get the public key from the signature
  //check the signature of the public key with publictablekey

  //check the signatureless version of the cheat table with this public key

end;

procedure signTable(table: TXMLDocument);
begin
  if isFecesMember=false then
    raise exception.create('Your are not a part of feces');

  //get the private key from cheatengine.signature
  //get the hashless version of this table
  //generate a hash based on it
  //sign that hash and add it to the table
  //and add the public key of cheatengine.signature to the table as well
end;

function isFecesMember: boolean;
begin
  result:=fileexists(GetCEdir+'cheatengine.signature');
end;

end.

