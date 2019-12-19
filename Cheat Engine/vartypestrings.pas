unit vartypestrings;
{
This unit is to help with translations for the vartype strings used throughout CE.
The vartype stringnames are used in the cheat table so they may not be translated, but the visual representation of the screen can
}

{$mode delphi}

interface

uses
  Classes, SysUtils;

resourcestring
  rs_vtAll='All';
  rs_vtBinary='Binary';
  rs_vtByteArray='Array of byte';
  rs_vtByte='Byte';
  rs_vtWord='2 Bytes';
  rs_vtDword='4 Bytes';
  rs_vtQword='8 Bytes';
  rs_vtSingle='Float';
  rs_vtDouble='Double';
  rs_vtString='String';
  rs_vtWidestring='Widestring';
  rs_vtUnicodeString='Unicode String';
  rs_vtCodePageString='CodePage String';
  rs_vtPointer='Pointer';
  rs_vtAutoAssembler='Auto Assembler Script';
  rs_vtCustom='Custom';
  rs_vtGrouped='Grouped';

const
  eng_vtAll='All';
  eng_vtBinary='Binary';
  eng_vtByteArray='Array of byte';
  eng_vtByte='Byte';
  eng_vtWord='2 Bytes';
  eng_vtDword='4 Bytes';
  eng_vtQword='8 Bytes';
  eng_vtSingle='Float';
  eng_vtDouble='Double';
  eng_vtString='String';
  eng_vtUnicodeString='Unicode String';
  eng_vtCodePageString='CodePage String';
  eng_vtPointer='Pointer';
  eng_vtAutoAssembler='Auto Assembler Script';
  eng_vtCustom='Custom';
  eng_vtGrouped='Grouped';

function ConvertTranslatedVartypeToEnglish(t: string): string; //I don't think I should ever call this, but it's there for people modding ce that accidentally use it anyhow...

implementation

function ConvertTranslatedVartypeToEnglish(t: string): string;
begin
  //should never be called
  result:=t;

  t:=lowercase(trim(t));
  if t=lowercase(rs_vtAll) then
    result:=eng_vtAll else
  if t=lowercase(rs_vtBinary) then
    result:=eng_vtBinary else
  if t=lowercase(rs_vtByteArray) then
    result:=eng_vtByteArray else
  if t=lowercase(rs_vtByte) then
    result:=eng_vtByte else
  if t=lowercase(rs_vtWord) then
    result:=eng_vtWord else
  if t=lowercase(rs_vtDword) then
    result:=eng_vtDword else
  if t=lowercase(rs_vtQword) then
    result:=eng_vtQword else
  if t=lowercase(rs_vtSingle) then
    result:=eng_vtSingle else
  if t=lowercase(rs_vtDouble) then
    result:=eng_vtDouble else
  if t=lowercase(rs_vtString) then
    result:=eng_vtString else
  if t=lowercase(rs_vtUnicodeString) then
    result:=eng_vtUnicodeString else
  if t=lowercase(rs_vtCodePageString) then
    result:=eng_vtCodePageString else
  if t=lowercase(rs_vtPointer) then
    result:=eng_vtPointer else
  if t=lowercase(rs_vtAutoAssembler) then
    result:=eng_vtAutoAssembler else
  if t=lowercase(rs_vtCustom) then
    result:=eng_vtCustom;




end;


end.

