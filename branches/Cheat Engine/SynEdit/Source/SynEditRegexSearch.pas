{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditRegexSearch.pas, released 2002-07-26.

Original Code by Eduardo Mauro, Gerald Nunn and Flávio Etrusco.
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

$Id: SynEditRegexSearch.pas,v 1.5 2003/08/31 11:22:54 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITREGEXSEARCH}
unit SynEditRegexSearch;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QSynEditTypes,
  QSynRegExpr,
  QSynEditMiscClasses,
{$ELSE}
  SynEditTypes,
  SynRegExpr,
  SynEditMiscClasses,
{$ENDIF}
  Classes;

type
  TSynEditRegexSearch = class(TSynEditSearchCustom)
  private
    fRegex : TRegExpr;
    fPositions: TList;
    fLengths: TList;
  protected
    function GetPattern: string; override;
    procedure SetPattern(const Value: string); override;
    procedure SetOptions(const Value: TSynSearchOptions); override;
    function GetLength(aIndex: integer): integer; override;
    function GetResult(aIndex: integer): integer; override;
    function GetResultCount: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindAll(const NewText: string): integer; override;
    function Replace(const aOccurrence, aReplacement: string): string; override;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QConsts;
{$ELSE}
  Consts;
{$ENDIF}

{ TSynEditRegexSearch }

constructor TSynEditRegexSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRegex := TRegExpr.Create;
  fPositions := TList.Create;
  fLengths := TList.Create;
end;

destructor TSynEditRegexSearch.Destroy;
begin
  inherited;
  fRegex.Free;
  fPositions.Free;
  fLengths.Free;
end;

function TSynEditRegexSearch.FindAll(const NewText: string): integer;

  procedure AddResult(const aPos, aLength: integer);
  begin
    fPositions.Add( pointer(aPos) );
    fLengths.Add( pointer(aLength) );
  end;

begin
  fPositions.Clear;
  fLengths.Clear;
  if fRegex.Exec( NewText ) then
  begin
    AddResult( fRegex.MatchPos[0], fRegex.MatchLen[0] );
    Result := 1;
    while fRegex.ExecNext do
    begin
      AddResult( fRegex.MatchPos[0], fRegex.MatchLen[0] );
      Inc( Result );
    end;
  end
  else
    Result := 0;
end;

function TSynEditRegexSearch.Replace(const aOccurrence, aReplacement: string): string;   
begin
  Result := fRegex.Replace( aOccurrence, aReplacement, True );
end;   

function TSynEditRegexSearch.GetLength(aIndex: integer): integer;
begin
  Result := integer( fLengths[ aIndex ] );
end;

function TSynEditRegexSearch.GetPattern: string;
begin
  Result := fRegex.Expression;
end;

function TSynEditRegexSearch.GetResult(aIndex: integer): integer;
begin
  Result := integer( fPositions[ aIndex ] );
end;

function TSynEditRegexSearch.GetResultCount: integer;
begin
  Result := fPositions.Count;
end;

procedure TSynEditRegexSearch.SetOptions(const Value: TSynSearchOptions);
begin
  fRegex.ModifierI := not( ssoMatchCase in Value );
end;

procedure TSynEditRegexSearch.SetPattern(const Value: string);
begin
  fRegex.Expression := Value;
end;

end.

