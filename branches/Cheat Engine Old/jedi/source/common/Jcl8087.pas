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
{ The Original Code is Jcl8087.pas                                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright Marcel van Brakel. All rights reserved.      }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   ESB Consultancy                                                                                }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones                                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains various routine for manipulating the math coprocessor. This includes such     }
{ things as querying and setting the rounding precision of  floating point operations and          }
{ retrieving the coprocessor's status word.                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit Jcl8087;

{$I jcl.inc}

interface

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

type
  T8087Precision = (pcSingle, pcReserved, pcDouble, pcExtended);
  T8087Rounding = (rcNearestOrEven, rcDownInfinity, rcUpInfinity, rcChopOrTruncate);
  T8087Infinity = (icProjective, icAffine);
  T8087Exception = (emInvalidOp, emDenormalizedOperand, emZeroDivide, emOverflow,
    emUnderflow, emPrecision);
  T8087Exceptions = set of T8087Exception;

const
  All8087Exceptions = [Low(T8087Exception)..High(T8087Exception)];

function Get8087ControlWord: Word;
function Get8087Infinity: T8087Infinity;
function Get8087Precision: T8087Precision;
function Get8087Rounding: T8087Rounding;
function Get8087StatusWord(ClearExceptions: Boolean): Word;

function Set8087Infinity(const Infinity: T8087Infinity): T8087Infinity;
function Set8087Precision(const Precision: T8087Precision): T8087Precision;
function Set8087Rounding(const Rounding: T8087Rounding): T8087Rounding;
function Set8087ControlWord(const Control: Word): Word;

function ClearPending8087Exceptions: T8087Exceptions;
function GetPending8087Exceptions: T8087Exceptions;
function GetMasked8087Exceptions: T8087Exceptions;
function SetMasked8087Exceptions(Exceptions: T8087Exceptions; ClearBefore: Boolean = True): T8087Exceptions;
function Mask8087Exceptions(Exceptions: T8087Exceptions): T8087Exceptions;
function Unmask8087Exceptions(Exceptions: T8087Exceptions; ClearBefore: Boolean = True): T8087Exceptions;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/Jcl8087.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

const
  X87ExceptBits = $3F;

function Get8087ControlWord: Word; assembler;
asm
        {$IFDEF FPC}
        SUB     ESP, $2
        {$ELSE}
        SUB     ESP, TYPE WORD
        {$ENDIF FPC}
        FSTCW   [ESP]
        FWAIT
        POP AX
end;

function Get8087Infinity: T8087Infinity;
begin
  Result := T8087Infinity((Get8087ControlWord and $1000) shr 12);
end;

function Get8087Precision: T8087Precision;
begin
  Result := T8087Precision((Get8087ControlWord and $0300) shr 8);
end;

function Get8087Rounding: T8087Rounding;
begin
  Result := T8087Rounding((Get8087ControlWord and $0C00) shr 10);
end;

function Get8087StatusWord(ClearExceptions: Boolean): Word; assembler;
asm
        TEST    AX, AX                // if ClearExceptions then
        JE      @@NoClearExceptions
        FSTSW   AX                    //   get status word (clears exceptions)
        RET
@@NoClearExceptions:                  // else
        FNSTSW  AX                    //   get status word (without clearing exceptions)
end;

function Set8087Infinity(const Infinity: T8087Infinity): T8087Infinity;
var
  CW: Word;
begin
  CW := Get8087ControlWord;
  Result := T8087Infinity((CW and $1000) shr 12);
  Set8087ControlWord((CW and $EFFF) or (Word(Infinity) shl 12));
end;

function Set8087Precision(const Precision: T8087Precision): T8087Precision;
var
  CW: Word;
begin
  CW := Get8087ControlWord;
  Result := T8087Precision((CW and $0300) shr 8);
  Set8087ControlWord((CW and $FCFF) or (Word(Precision) shl 8));
end;

function Set8087Rounding(const Rounding: T8087Rounding): T8087Rounding;
var
  CW: Word;
begin
  CW := Get8087ControlWord;
  Result := T8087Rounding((CW and $0C00) shr 10);
  Set8087ControlWord((CW and $F3FF) or (Word(Rounding) shl 10));
end;

function Set8087ControlWord(const Control: Word): Word; assembler;
asm
        FNCLEX
        {$IFDEF FPC}
        SUB     ESP, $2
        {$ELSE}
        SUB     ESP, TYPE WORD
        {$ENDIF FPC}
        FSTCW   [ESP]
        XCHG    [ESP], AX
        FLDCW   [ESP]
        {$IFDEF FPC}
        ADD     ESP, $2
        {$ELSE}
        ADD     ESP, TYPE WORD
        {$ENDIF FPC}
end;

function ClearPending8087Exceptions: T8087Exceptions;
asm
        FNSTSW  AX
        AND     AX, X87ExceptBits
        FNCLEX
end;

function GetPending8087Exceptions: T8087Exceptions;
asm
        FNSTSW  AX
        AND     AX, X87ExceptBits
end;

function GetMasked8087Exceptions: T8087Exceptions;
asm
        {$IFDEF FPC}
        SUB     ESP, $2
        {$ELSE}
        SUB     ESP, TYPE WORD
        {$ENDIF FPC}
        FSTCW   [ESP]
        FWAIT
        POP     AX
        AND     AX, X87ExceptBits
end;

function SetMasked8087Exceptions(Exceptions: T8087Exceptions; ClearBefore: Boolean): T8087Exceptions;
asm
        TEST    DL, DL             // if ClearBefore then
        JZ      @1
        FNCLEX                     // clear pending exceptions
@1:
        {$IFDEF FPC}
        SUB     ESP, $2
        {$ELSE}
        SUB     ESP, TYPE WORD
        {$ENDIF FPC}
        FSTCW   [ESP]
        FWAIT
        AND     AX, X87ExceptBits  // mask exception mask bits 0..5
        MOV     DX, [ESP]
        AND     WORD PTR [ESP], NOT X87ExceptBits
        OR      [ESP], AX
        FLDCW   [ESP]
        {$IFDEF FPC}
        ADD     ESP, $2
        {$ELSE}
        ADD     ESP, TYPE WORD
        {$ENDIF FPC}
        MOV     AX, DX
        AND     AX, X87ExceptBits
end;

function Mask8087Exceptions(Exceptions: T8087Exceptions): T8087Exceptions;
begin
  Result := GetMasked8087Exceptions;
  Exceptions := Exceptions + Result;
  SetMasked8087Exceptions(Exceptions, False);
end;

function Unmask8087Exceptions(Exceptions: T8087Exceptions; ClearBefore: Boolean): T8087Exceptions;
begin
  Result := GetMasked8087Exceptions;
  Exceptions := Result - Exceptions;
  SetMasked8087Exceptions(Exceptions, ClearBefore);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
