{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditWildcardSearch.pas, released 2003-06-21.

The original author of this file is Michael Elsdoerfer.
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

$Id: SynEditWildcardSearch.pas,v 1.2 2003/09/20 17:42:52 miracle2k Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITWILDCARDSEARCH}
unit SynEditWildcardSearch;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QSynEdit,
  QSynEditTypes,
  QSynRegExpr,
  QSynEditMiscClasses,
  QSynEditRegexSearch
{$ELSE}
  SynEdit,
  SynEditTypes,
  SynRegExpr,
  SynEditRegexSearch,
{$ENDIF}
  Classes;

type
  TSynEditWildcardSearch = class(TSynEditRegexSearch)
  private
    fPattern: string;
  protected
    function GetPattern: string; override;
    procedure SetPattern(const Value: string); override;
    procedure SetOptions(const Value: TSynSearchOptions); override;
    function GetLength(aIndex: integer): integer; override;
    function GetResult(aIndex: integer): integer; override;
    function GetResultCount: integer; override;
    // Converts the Wildcard to a regular expression
    function WildCardToRegExpr(AWildCard: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindAll(const NewText: string): integer; override;
    function Replace(const aOccurrence, aReplacement: string): string; override;        //slm 11/29/02
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QConsts;
{$ELSE}
  Consts;
{$ENDIF}

{ TSynEditWildcardSearch }

constructor TSynEditWildcardSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPattern := '';
end;

destructor TSynEditWildcardSearch.Destroy;
begin
  inherited;
end;

function TSynEditWildcardSearch.FindAll(const NewText: string): integer;
begin
  Result := inherited FindAll(NewText);
end;

function TSynEditWildcardSearch.Replace(const aOccurrence, aReplacement: string): string;
begin
  Result := inherited Replace(aOccurrence, aReplacement);
end;   

function TSynEditWildcardSearch.GetLength(aIndex: integer): integer;
begin
  Result := inherited GetLength(aIndex);
end;

function TSynEditWildcardSearch.GetPattern: string;
begin
  Result := fPattern;
end;

function TSynEditWildcardSearch.GetResult(aIndex: integer): integer;
begin
  Result := inherited GetResult(aIndex);
end;

function TSynEditWildcardSearch.GetResultCount: integer;
begin
  Result := inherited GetResultCount;
end;

procedure TSynEditWildcardSearch.SetOptions(const Value: TSynSearchOptions);
begin
  inherited;
end;

procedure TSynEditWildcardSearch.SetPattern(const Value: string);
begin
  fPattern := Value;
  // Convert into a real regular expression and assign it
  inherited SetPattern(WildCardToRegExpr(Value));
end;

function TSynEditWildcardSearch.WildCardToRegExpr(
  AWildCard: string): string;
var i: integer;
begin
  Result := '';

  for i := 1 to length(AWildCard) do
    case AWildCard[i] of
      '*': Result := Result + '.*';
      '?': Result := Result + '.?';
      else Result := Result + AWildCard[i];
    end;
end;

end.

