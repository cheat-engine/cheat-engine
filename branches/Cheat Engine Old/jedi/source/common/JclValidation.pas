{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclValidation.pas                                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is Ivo Bauer.                                         }
{ Portions created by Ivo Bauer are Copyright Ivo Bauer. All rights reserved.                      }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains ISBN validation routines                                                      }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclValidation;

{$I jcl.inc}

interface

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

// ISBN: International Standard Book Number
function IsValidISBN(const ISBN: AnsiString): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclValidation.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

{ TODO -cDoc : Donator: Ivo Bauer }
function IsValidISBN(const ISBN: AnsiString): Boolean;
//
// References:
// ===========
// [1] http://isbn-international.org/en/userman/chapter4.html
//
type
  TISBNPart = (ipGroupID, ipPublisherID, ipTitleID, ipCheckDigit);
  TISBNPartSizes = array [TISBNPart] of Integer;
const
  ISBNSize = 13;
  ISBNDigits = ['0'..'9'];
  ISBNSpecialDigits = ['x', 'X'];
  ISBNSeparators = [#32, '-'];
  ISBNCharacters = ISBNDigits + ISBNSpecialDigits + ISBNSeparators;
var
  CurPtr, EndPtr: Integer;
  Accumulator, Counter: Integer;
  Part: TISBNPart;
  PartSizes: TISBNPartSizes;

  function IsPartSizeValid(APart: TISBNPart): Boolean;
  const
    MaxPartSizes: TISBNPartSizes = (5, 7, 6, 1);
  begin
    Result := PartSizes[APart] <= MaxPartSizes[APart];
  end;

begin
  Result := False;
  // At first, check the overall string length.
  if Length(ISBN) <> ISBNSize then
    Exit;

  CurPtr := 1;
  EndPtr := ISBNSize - 1;
  Accumulator := 0;
  Counter := 10;
  Part := ipGroupID;
  {$IFNDEF CLR}
  FillChar(PartSizes[Low(PartSizes)], SizeOf(PartSizes), 0);
  {$ENDIF ~CLR}

  while CurPtr <= EndPtr do
  begin
    if ISBN[CurPtr] in ISBNCharacters then
    begin
      if ISBN[CurPtr] in ISBNSeparators then
      begin
        // Switch to the next ISBN part, but take care of two conditions:
        // 1. Do not let Part go beyond its upper bound (ipCheckDigit).
        // 2. Verify if the current ISBN part does not exceed its size limit.
        if (Part < High(Part)) and IsPartSizeValid(Part) then
          Inc(Part)
        else
          Exit;
      end
      else // CurPtr^ in [ISBNDigits, ISBNSpecialDigits]
      begin
        // Is it the last character of the string?
        if (CurPtr = EndPtr) then
        begin
          // Check the following conditions:
          // 1. Make sure current ISBN Part equals to ipCheckDigit.
          // 2. Verify if the check digit does not exceed its size limit.
          if (Part <> High(Part)) and not IsPartSizeValid(Part) then
            Exit;
        end
        else
          // Special check digit is allowed to occur only at the end of ISBN.
          if ISBN[CurPtr] in ISBNSpecialDigits then
            Exit;

        // Increment the size of the current ISBN part.
        Inc(PartSizes[Part]);

        // Increment the accumulator by current ISBN digit multiplied by a weight.
        // To get more detailed information, please refer to the web site [1].
        if (CurPtr = EndPtr) and (ISBN[CurPtr] in ISBNSpecialDigits) then
          Inc(Accumulator, 10 * Counter)
        else
          Inc(Accumulator, (Ord(ISBN[CurPtr]) - Ord('0')) * Counter);
        Dec(Counter);
      end;
      Inc(CurPtr);
    end
    else
      Exit;
  end;
  // Accumulator content must be divisible by 11 without a remainder.
  Result := (Accumulator mod 11) = 0;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
