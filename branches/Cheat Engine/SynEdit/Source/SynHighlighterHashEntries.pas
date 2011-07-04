{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHashEntries.pas, released 2000-04-21.

The Initial Author of this file is Michael Hieke.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterHashEntries.pas,v 1.5 2004/07/09 13:03:55 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{
@abstract(Support classes for SynEdit highlighters that create the keyword lists at runtime.)
@author(Michael Hieke)
@created(2000-04-21)
@lastmod(2001-09-07)
The classes in this unit can be used to use the hashing algorithm while still
having the ability to change the set of keywords.
}

{$IFNDEF QSYNHIGHLIGHTERHASHENTRIES}
unit SynHighlighterHashEntries;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QSynEditTypes,
{$ELSE}
  SynEditTypes,
{$ENDIF}
  Classes;

type
  { Class to hold the keyword to recognize, its length and its token kind. The
    keywords that have the same hashvalue are stored in a single-linked list,
    with the Next property pointing to the next entry. The entries are ordered
    over the keyword length. }
  TSynHashEntry = class(TObject)
  protected
    { Points to the next keyword entry with the same hashvalue. }
    fNext: TSynHashEntry;
    { Length of the keyword. }
    fKeyLen: integer;
    { The keyword itself. }
    fKeyword: string;
    { Keyword token kind, has to be typecasted to the real token kind type. }
    fKind: integer;
  public
    { Adds a keyword entry with the same hashvalue. Depending on the length of
      the two keywords it might return Self and store NewEntry in the Next
      pointer, or return NewEntry and make the Next property of NewEntry point
      to Self. This way the order of keyword length is preserved. }
    function AddEntry(NewEntry: TSynHashEntry): TSynHashEntry; virtual;
    { Creates a keyword entry for the given keyword and token kind. }
    constructor Create(const AKey: string; AKind: integer);
    { Destroys the keyword entry and all other keyword entries Next points to. }
    destructor Destroy; override;
  public
    { The keyword itself. }
    property Keyword: string read fKeyword;
    { Length of the keyword. }
    property KeywordLen: integer read fKeyLen;
    { Keyword token kind, has to be typecasted to the real token kind type. }
    property Kind: integer read fKind;
    { Points to the next keyword entry with the same hashvalue. }
    property Next: TSynHashEntry read fNext;
  end;


{$IFNDEF SYN_COMPILER_4_UP}
  {$IFNDEF SYN_CPPB_3}
    {$DEFINE LIST_CLEAR_NOT_VIRTUAL}
  {$ENDIF}
{$ENDIF}

  { A list of keyword entries, stored as single-linked lists under the hashvalue
    of the keyword. }
  TSynHashEntryList = class(TList)
  protected
    { Returns the first keyword entry for a given hashcalue, or nil. }
    function Get(HashKey: Integer): TSynHashEntry;
    { Adds a keyword entry under its hashvalue. Will grow the list count when
      necessary, so the maximum hashvalue should be limited outside. The correct
      order of keyword entries is maintained. }
    procedure Put(HashKey: Integer; Entry: TSynHashEntry);
  public
{$IFDEF LIST_CLEAR_NOT_VIRTUAL}
    { Overridden destructor clears the list and frees all contained keyword
      entries. }
    destructor Destroy; override;
    { Clears the list and frees all contained keyword entries. }
    procedure DeleteEntries;
{$ELSE}
    { Clears the list and frees all contained keyword entries. }
    procedure Clear; override;
{$ENDIF}
  public
    { Type-safe access to the first keyword entry for a hashvalue. }
    property Items[Index: integer]: TSynHashEntry read Get write Put; default;
  end;

  { Procedural type for adding keyword entries to a TSynHashEntryList when
    iterating over all the keywords contained in a string. }
  TEnumerateKeywordEvent = procedure(AKeyword: string; AKind: integer)
    of object;

{ This procedure will call AKeywordProc for all keywords in KeywordList. A
  keyword is considered any number of successive chars that are contained in
  Identifiers, with chars not contained in Identifiers before and after them. }
procedure EnumerateKeywords(AKind: integer; KeywordList: string;
  Identifiers: TSynIdentChars; AKeywordProc: TEnumerateKeywordEvent);

implementation

uses
  SysUtils;

procedure EnumerateKeywords(AKind: integer; KeywordList: string;
  Identifiers: TSynIdentChars; AKeywordProc: TEnumerateKeywordEvent);
var
  pStart, pEnd: PChar;
  Keyword: string;
begin
  if Assigned(AKeywordProc) and (KeywordList <> '') then begin
    pEnd := PChar(KeywordList);
    pStart := pEnd;
    repeat
      // skip over chars that are not in Identifiers
      while (pStart^ <> #0) and not (pStart^ in Identifiers) do
        Inc(pStart);
      if pStart^ = #0 then break;
      // find the last char that is in Identifiers
      pEnd := pStart + 1;
      while (pEnd^ <> #0) and (pEnd^ in Identifiers) do
        Inc(pEnd);
      // call the AKeywordProc with the keyword
      SetString(Keyword, pStart, pEnd - pStart);
      AKeywordProc(Keyword, AKind);
      Keyword := '';
      // pEnd points to a char not in Identifiers, restart after that
      pStart := pEnd + 1;
    until (pStart^ = #0) or (pEnd^ = #0);
  end;
end;

{ TSynHashEntry }

constructor TSynHashEntry.Create(const AKey: string; AKind: integer);
begin
  inherited Create;
  fKeyLen := Length(AKey);
  fKeyword := AKey;
  fKind := AKind;
end;

destructor TSynHashEntry.Destroy;
begin
  fNext.Free;
  inherited Destroy;
end;

function TSynHashEntry.AddEntry(NewEntry: TSynHashEntry): TSynHashEntry;
begin
  Result := Self;
  if Assigned(NewEntry) then begin
    if CompareText(NewEntry.Keyword, fKeyword) = 0 then
      raise Exception.CreateFmt('Keyword "%s" already in list', [fKeyword]);
    if NewEntry.fKeyLen < fKeyLen then begin
      NewEntry.fNext := Self;
      Result := NewEntry;
    end else if Assigned(fNext) then
      fNext := fNext.AddEntry(NewEntry)
    else
      fNext := NewEntry;
  end;
end;

{ TSynHashEntryList }

{$IFDEF LIST_CLEAR_NOT_VIRTUAL}
destructor TSynHashEntryList.Destroy;
begin
  DeleteEntries;
  inherited Destroy;
end;

procedure TSynHashEntryList.DeleteEntries;
{$ELSE}
procedure TSynHashEntryList.Clear;
{$ENDIF}
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    TSynHashEntry(Items[i]).Free;
  inherited Clear;
end;

function TSynHashEntryList.Get(HashKey: Integer): TSynHashEntry;
begin
  if (HashKey >= 0) and (HashKey < Count) then
    Result := inherited Items[HashKey]
  else
    Result := nil;
end;

procedure TSynHashEntryList.Put(HashKey: Integer; Entry: TSynHashEntry);
var
  ListEntry: TSynHashEntry;
begin
  if HashKey >= Count then
    Count := HashKey + 1;
  ListEntry := TSynHashEntry(inherited Items[HashKey]);
  // if there is already a hashentry for this hashvalue let it decide
  // where to put the new entry in its single linked list
  if Assigned(ListEntry) then
    Entry := ListEntry.AddEntry(Entry);
  inherited Items[HashKey] := Entry;
end;

end.

