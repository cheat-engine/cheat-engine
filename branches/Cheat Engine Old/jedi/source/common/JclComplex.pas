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
{ The Original Code is JclComplex.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Alexei Koudinov. Portions created by               }
{ Alexei Koudinov are Copyright (C) of Alexei Koudinov. All Rights Reserved.                       }                         
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Alexei Koudinov                                                                                }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma  (mthoma)                                                                       }
{   Petr Vones (pvones)                                                                            }        
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Class for working with complex numbers.                                                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclComplex;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils,
  JclBase, JclMath, JclResources, JclStrings;

const
  TComplex_VERSION = 5.01;

type
  TComplexKind = (crRectangular, crPolar);

  TCoords = record
    X: Float;     // rectangular real
    Y: Float;     // rectangular imaginary
    R: Float;     // polar 1
    Theta: Float; // polar 2
  end;

  TRectCoord = record
    X: Float;
    Y: Float;
  end;

  TJclComplex = class(TObject)
  private   {z = x + yi}
    FCoord: TCoords;
    FFracLen: Byte;
    function MiscalcSingle(const X: Float): Float;
    procedure MiscalcComplex; // eliminates miscalculation
    procedure FillCoords(const ComplexType: TComplexKind);
    function GetRectangularString: string;
    function GetPolarString: string;
    procedure SetRectangularString(StrToParse: string);
    procedure SetPolarString(StrToParse: string);
    procedure SetFracLen(const X: Byte);
    function GetRadius: Float;
    function GetAngle: Float;
    function NormalizeAngle(Value: Float): Float;
  protected
    function Assign(const Coord: TCoords; const ComplexType: TComplexKind = crRectangular): TJclComplex; overload;
    function CoreAdd(const First, Second: TRectCoord): TRectCoord;
    function CoreDiv(const First, Second: TRectCoord): TRectCoord;
    function CoreMul(const First, Second: TRectCoord): TRectCoord;
    function CoreSub(const First, Second: TRectCoord): TRectCoord;
    function CoreLn (const LnValue: TRectCoord): TRectCoord;
    function CoreExp(const ExpValue: TRectCoord): TRectCoord;
    function CorePwr(First, Second, Polar: TRectCoord): TRectCoord;
    function CoreIntPwr(First: TRectCoord; const Polar: TRectCoord; const Pwr: Integer): TRectCoord;
    function CoreRealPwr(First: TRectCoord; const Polar: TRectCoord; const Pwr: Float): TRectCoord;
    function CoreRoot(First: TRectCoord; const Polar: TRectCoord; const K, N: Word): TRectCoord;
    function CoreCos(const Value: TRectCoord): TRectCoord;
    function CoreSin(const Value: TRectCoord): TRectCoord;
    function CoreTan(const Value: TRectCoord): TRectCoord;
    function CoreCot(const Value: TRectCoord): TRectCoord;
    function CoreSec(const Value: TRectCoord): TRectCoord;
    function CoreCsc(const Value: TRectCoord): TRectCoord;
    function CoreCosH(const Value: TRectCoord): TRectCoord;
    function CoreSinH(const Value: TRectCoord): TRectCoord;
    function CoreTanH(const Value: TRectCoord): TRectCoord;
    function CoreCotH(const Value: TRectCoord): TRectCoord;
    function CoreSecH(const Value: TRectCoord): TRectCoord;
    function CoreCscH(const Value: TRectCoord): TRectCoord;
    function CoreI0(const Value: TRectCoord): TRectCoord;
    function CoreJ0(const Value: TRectCoord): TRectCoord;
    function CoreApproxLnGamma(const Value: TRectCoord): TRectCoord;
    function CoreLnGamma(Value: TRectCoord): TRectCoord;
    function CoreGamma(const Value: TRectCoord): TRectCoord;
  public
    //----------- constructors
    constructor Create; overload;
    constructor Create(const X, Y: Float; const ComplexType: TComplexKind = crRectangular); overload;

    //----------- complex numbers assignment routines
    function Assign(const X, Y: Float; const ComplexType: TComplexKind = crRectangular): TJclComplex; overload;
    function AssignZero: TJclComplex;
    function AssignOne: TJclComplex;
    function Duplicate: TJclComplex;

    //----------- arithmetics -- modify the object itself
    function CAdd(const AddValue: TJclComplex): TJclComplex; overload;
    function CAdd(const X, Y: Float; const ComplexType: TComplexKind = crRectangular): TJclComplex; overload;
    function CDiv(const DivValue: TJclComplex): TJclComplex; overload;
    function CDiv(const X, Y: Float; const ComplexType: TComplexKind = crRectangular): TJclComplex; overload;
    function CMul(const MulValue: TJclComplex): TJclComplex; overload;
    function CMul(const X, Y: Float; const ComplexType: TComplexKind = crRectangular): TJclComplex; overload;
    function CSub(const SubValue: TJclComplex): TJclComplex; overload;
    function CSub(const X, Y: Float; const ComplexType: TComplexKind = crRectangular): TJclComplex; overload;
    function CNeg: TJclComplex;
    function CConjugate: TJclComplex;

    //----------- arithmetics -- creates new resulting object
    function CNewAdd(const AddValue: TJclComplex): TJclComplex; overload;
    function CNewAdd(const X, Y: Float; const ComplexType: TComplexKind = crRectangular): TJclComplex; overload;
    function CNewDiv(const DivValue: TJclComplex): TJclComplex; overload;
    function CNewDiv(const X, Y: Float; const ComplexType: TComplexKind = crRectangular): TJclComplex; overload;
    function CNewMul(const MulValue: TJclComplex): TJclComplex; overload;
    function CNewMul(const X, Y: Float; const ComplexType: TComplexKind = crRectangular): TJclComplex; overload;
    function CNewSub(const SubValue: TJclComplex): TJclComplex; overload;
    function CNewSub(const X, Y: Float; const ComplexType: TComplexKind = crRectangular): TJclComplex; overload;
    function CNewNeg: TJclComplex;
    function CNewConjugate: TJclComplex;

    //----------- natural log and exponential functions
    function CLn: TJclComplex;
    function CNewLn: TJclComplex;
    function CExp: TJclComplex;
    function CNewExp: TJclComplex;
    function CPwr(const PwrValue: TJclComplex): TJclComplex; overload;
    function CPwr(const X, Y: Float; const ComplexType: TComplexKind = crRectangular): TJclComplex; overload;
    function CNewPwr(PwrValue: TJclComplex): TJclComplex; overload;
    function CNewPwr(const X, Y: Float; const ComplexType: TComplexKind = crRectangular): TJclComplex; overload;
    function CIntPwr(const Pwr: Integer): TJclComplex; overload;
    function CNewIntPwr(const Pwr: Integer): TJclComplex; overload;
    function CRealPwr(const Pwr: Float): TJclComplex; overload;
    function CNewRealPwr(const Pwr: Float): TJclComplex; overload;
    function CRoot(const K, N: Word): TJclComplex; overload;
    function CNewRoot(const K, N: Word): TJclComplex; overload;
    function CSqrt: TJclComplex; overload;
    function CNewSqrt: TJclComplex; overload;

    //----------- trigonometric functions
    function CCos: TJclComplex;
    function CNewCos: TJclComplex;
    function CSin: TJclComplex;
    function CNewSin: TJclComplex;
    function CTan: TJclComplex;
    function CNewTan: TJclComplex;
    function CCot: TJclComplex;
    function CNewCot: TJclComplex;
    function CSec: TJclComplex;
    function CNewSec: TJclComplex;
    function CCsc: TJclComplex;
    function CNewCsc: TJclComplex;

    //----------- complex hyperbolic functions
    function CCosH: TJclComplex;
    function CNewCosH: TJclComplex;
    function CSinH: TJclComplex;
    function CNewSinH: TJclComplex;
    function CTanH: TJclComplex;
    function CNewTanH: TJclComplex;
    function CCotH: TJclComplex;
    function CNewCotH: TJclComplex;
    function CSecH: TJclComplex;
    function CNewSecH: TJclComplex;
    function CCscH: TJclComplex;
    function CNewCscH: TJclComplex;

    //----------- complex Bessel functions of order zero
    function CI0: TJclComplex;
    function CNewI0: TJclComplex;
    function CJ0: TJclComplex;
    function CNewJ0: TJclComplex;

    function CApproxLnGamma: TJclComplex;
    function CNewApproxLnGamma: TJclComplex;
    function CLnGamma: TJclComplex;
    function CNewLnGamma: TJclComplex;
    function CGamma: TJclComplex;
    function CNewGamma: TJclComplex;

    //----------- miscellaneous routines
    function AbsoluteValue: Float; overload;
    function AbsoluteValue(const Coord: TRectCoord): Float; overload;
    function AbsoluteValueSqr: Float; overload;
    function AbsoluteValueSqr(const Coord: TRectCoord): Float; overload;
    function FormatExtended(const X: Float): string;

    property FracLength: Byte read FFracLen write SetFracLen default 8;

    //----------- getting different parts of the number
    property RealPart: Float read FCoord.X;
    property ImaginaryPart: Float read FCoord.Y;
    property Radius: Float read GetRadius;
    property Angle: Float read GetAngle;

    //----------- format output
    property AsString: string read GetRectangularString write SetRectangularString;
    property AsPolarString: string read GetPolarString write SetPolarString;

    {$IFDEF CLR}
    { TODO : Implement operators }
    {$ENDIF CLR}
  end;

var
  ComplexPrecision: Float = 1E-14;

const
  MaxTerm: Byte = 35;
  EpsilonSqr: Float = 1E-20;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclComplex.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

const
  MaxFracLen = 18;
  RectOne: TRectCoord = (X: 1.0; Y: 0.0);
  RectZero: TRectCoord = (X: 0.0; Y: 0.0);
  RectInfinity: TRectCoord = (X: Infinity; Y: Infinity);

function Coordinates(const cX, cY: Float; CoordType: TComplexKind): TCoords;
begin
  case CoordType of
    crRectangular:
      begin
        Result.X := cX;
        Result.Y := cY;
        Result.R := 0.0;
        Result.Theta := 0.0;
      end;
    crPolar:
      begin
        Result.X := 0.0;
        Result.Y := 0.0;
        Result.R := cX;
        Result.Theta := cY;
      end;
  end;
end;

function RectCoord(X, Y: Float): TRectCoord; overload;
begin
  Result.X := X;
  Result.Y := Y;
end;

function RectCoord(Value: TJclComplex): TRectCoord; overload;
begin
  Result.X := Value.FCoord.X;
  Result.Y := Value.FCoord.Y;
end;

//=== { TJclComplex } ========================================================

constructor TJclComplex.Create;
begin
  inherited Create;
  AssignZero;
  FFracLen := MaxFracLen;
end;

constructor TJclComplex.Create(const X, Y: Float; const ComplexType: TComplexKind);
begin
  inherited Create;
  Assign(X, Y, ComplexType);
  FFracLen := MaxFracLen;
end;

procedure TJclComplex.FillCoords(const ComplexType: TComplexKind);
begin
  MiscalcComplex;
  case ComplexType of
    crPolar:
      begin
        FCoord.X := FCoord.R * Cos(FCoord.Theta);
        FCoord.Y := FCoord.R * Sin(FCoord.Theta);
      end;
    crRectangular:
      if FCoord.X = 0.0 then
      begin
        FCoord.R := Abs(FCoord.Y);
        FCoord.Theta := PiOn2 * Sgn(FCoord.Y);
      end
      else
      begin
        FCoord.R := AbsoluteValue;
        FCoord.Theta := {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.ArcTan(FCoord.Y / FCoord.X);
        if FCoord.X < 0.0 then
          FCoord.Theta := FCoord.Theta + Pi * Sgn(FCoord.Y);
      end;
  end;
  MiscalcComplex;
end;

function TJclComplex.MiscalcSingle(const X: Float): Float;
begin
  Result := X;
  if Abs(Result) < ComplexPrecision then
    Result := 0.0;
end;

procedure TJclComplex.MiscalcComplex; // eliminates miscalculation
begin
  FCoord.X := MiscalcSingle(FCoord.X);
  FCoord.Y := MiscalcSingle(FCoord.Y);
  FCoord.R := MiscalcSingle(FCoord.R);
  if FCoord.R = 0.0 then
    FCoord.Theta := 0.0
  else
    FCoord.Theta := MiscalcSingle(FCoord.Theta);
end;

function TJclComplex.Assign(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
begin
  Result := Assign(Coordinates(X, Y, ComplexType), ComplexType);
end;

function TJclComplex.Assign(const Coord: TCoords; const ComplexType: TComplexKind): TJclComplex;
begin
  FCoord := Coord;
  FillCoords(ComplexType);
  MiscalcComplex;
  Result := Self;
end;

function TJclComplex.AssignZero: TJclComplex;
begin
  Result := Assign(0.0, 0.0, crRectangular);
end;

function TJclComplex.AssignOne: TJclComplex;
begin
  Result := Assign(1.0, 0.0, crRectangular);
end;

function TJclComplex.GetRectangularString: string;
const
  cImaginary = 'i';
begin
  MiscalcComplex;
  if (FCoord.X = 0.0) and (FCoord.Y = 0.0) then
    Result := '0'
  else
  if FCoord.X <> 0.0 then
  begin
    Result := FormatExtended(FCoord.X);
    if FCoord.Y > 0.0 then
      Result := Result + '+'
    else
    if FCoord.Y < 0.0 then
      Result := Result + '-';
    if FCoord.Y <> 0.0 then
      Result := Result + FormatExtended(Abs(FCoord.Y)) + cImaginary;
  end
  else
    Result := FormatExtended(FCoord.Y) + cImaginary;
end;

function TJclComplex.GetPolarString: string;
begin
  FillCoords(crRectangular);
  Result := FormatExtended(FCoord.R) + '*CIS(' + FormatExtended(FCoord.Theta) + ')';
end;

procedure TJclComplex.SetRectangularString(StrToParse: string);
var
  SignPos: Integer;
  RealPart, ImagPart: Float;
begin
  StrToParse := StrRemoveChars(StrToParse, [' ']);
  SignPos := StrFind('+', StrToParse, 2);
  if SignPos = 0 then
    SignPos := StrFind('-', StrToParse, 2);
  if SignPos > 0 then
  begin
    try
      RealPart := StrToFloat(Copy(StrToParse, 1, SignPos - 1));
    except
      {$IFDEF CLR}
      raise EJclMathError.Create(RsComplexInvalidString);
      {$ELSE}
      raise EJclMathError.CreateRes(@RsComplexInvalidString);
      {$ENDIF CLR}
    end;
    try
      ImagPart := StrToFloat(Copy(StrToParse, SignPos, Length(StrToParse) - SignPos));
    except
      {$IFDEF CLR}
      raise EJclMathError.Create(RsComplexInvalidString);
      {$ELSE}
      raise EJclMathError.CreateRes(@RsComplexInvalidString);
      {$ENDIF CLR}
    end;
  end
  else
  begin
    if (StrToParse[Length(StrToParse)] = 'i') or (StrToParse[Length(StrToParse)] = 'I') then
    begin
      RealPart := 0.0;
      try
        ImagPart := StrToFloat(Copy(StrToParse, 1, Length(StrToParse) - 1));
      except
        {$IFDEF CLR}
        raise EJclMathError.Create(RsComplexInvalidString);
        {$ELSE}
        raise EJclMathError.CreateRes(@RsComplexInvalidString);
        {$ENDIF CLR}
      end;
    end
    else
    begin
      try
        RealPart := StrToFloat(StrToParse);
      except
        {$IFDEF CLR}
        raise EJclMathError.Create(RsComplexInvalidString);
        {$ELSE}
        raise EJclMathError.CreateRes(@RsComplexInvalidString);
        {$ENDIF CLR}
      end;
      ImagPart := 0.0;
    end;
  end;
  Assign(RealPart, ImagPart, crRectangular);
end;

procedure TJclComplex.SetPolarString(StrToParse: string);
var
  AstPos: Integer;
  Radius, Angle: Float;
begin
  {$IFDEF CLR}
  StrToParse := StrRemoveChars(StrToParse, [' ']).toUpper;
  {$ELSE}
  StrToParse := AnsiUpperCase(StrRemoveChars(StrToParse, [' ']));
  {$ENDIF CLR}
  AstPos := Pos('*', StrToParse);
  if AstPos = 0 then
    {$IFDEF CLR}
    raise EJclMathError.Create(RsComplexInvalidString);
    {$ELSE}
    raise EJclMathError.CreateRes(@RsComplexInvalidString);
    {$ENDIF CLR}
  try
    Radius := StrToFloat(StrLeft(StrToParse, AstPos - 1));
  except
    {$IFDEF CLR}
    raise EJclMathError.Create(RsComplexInvalidString);
    {$ELSE}
    raise EJclMathError.CreateRes(@RsComplexInvalidString);
    {$ENDIF CLR}
  end;
  AstPos := Pos('(', StrToParse);
  if AstPos = 0 then
    {$IFDEF CLR}
    raise EJclMathError.Create(RsComplexInvalidString);
    {$ELSE}
    raise EJclMathError.CreateRes(@RsComplexInvalidString);
    {$ENDIF CLR}
  try
    Angle := StrToFloat(Copy(StrToParse, AstPos + 1, Length(StrToParse) - AstPos - 1));
  except
    {$IFDEF CLR}
    raise EJclMathError.Create(RsComplexInvalidString);
    {$ELSE}
    raise EJclMathError.CreateRes(@RsComplexInvalidString);
    {$ENDIF CLR}
  end;
  Assign(Radius, Angle, crPolar);
end;

function TJclComplex.Duplicate: TJclComplex;
begin
  Result := TJclComplex.Create(FCoord.X, FCoord.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//=== arithmetics ============================================================

function TJclComplex.CoreAdd(const First, Second: TRectCoord): TRectCoord;
begin
  Result.X := First.X + Second.X;
  Result.Y := First.Y + Second.Y;
end;

function TJclComplex.CAdd(const AddValue: TJclComplex): TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreAdd(RectCoord(Self), RectCoord(AddValue));
  FCoord.X := ResCoord.X;
  FCoord.Y := ResCoord.Y;
  Result := Self;
end;

function TJclComplex.CAdd(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CAdd(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

function TJclComplex.CNewAdd(const AddValue: TJclComplex): TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreAdd(RectCoord(Self), RectCoord(AddValue));
  Result := TJclComplex.Create(ResCoord.X, ResCoord.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CNewAdd(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CNewAdd(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

function TJclComplex.CoreDiv(const First, Second: TRectCoord): TRectCoord;
var
  Denom: Float;
begin
  Denom := Sqr(Second.X) + Sqr(Second.Y);
  Result.X := (First.X * Second.X + First.Y * Second.Y) / Denom;
  Result.Y := (First.Y * Second.X - First.X * Second.Y) / Denom;
end;

function TJclComplex.CDiv(const DivValue: TJclComplex): TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreDiv(RectCoord(Self), RectCoord(DivValue));
  FCoord.X := ResCoord.X;
  FCoord.Y := ResCoord.Y;
  Result := Self;
end;

function TJclComplex.CDiv(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CDiv(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

function TJclComplex.CNewDiv(const DivValue: TJclComplex): TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreDiv(RectCoord(Self), RectCoord(DivValue));
  Result := TJclComplex.Create(ResCoord.X, ResCoord.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CNewDiv(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CNewDiv(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

function TJclComplex.CoreMul(const First, Second: TRectCoord): TRectCoord;
begin
  Result.X := First.X * Second.X - First.Y * Second.Y;
  Result.Y := First.X * Second.Y + First.Y * Second.X;
end;

function TJclComplex.CMul(const MulValue: TJclComplex): TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreMul(RectCoord(Self), RectCoord(MulValue));
  FCoord.X := ResCoord.X;
  FCoord.Y := ResCoord.Y;
  Result := Self;
end;

function TJclComplex.CMul(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CMul(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

function TJclComplex.CNewMul(const MulValue: TJclComplex): TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreMul(RectCoord(Self), RectCoord(MulValue));
  Result := TJclComplex.Create(ResCoord.X, ResCoord.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CNewMul(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CNewMul(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

function TJclComplex.CoreSub(const First, Second: TRectCoord): TRectCoord;
begin
  Result.X := First.X - Second.X;
  Result.Y := First.Y - Second.Y;
end;

function TJclComplex.CSub(const SubValue: TJclComplex): TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSub(RectCoord(Self), RectCoord(SubValue));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CSub(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CSub(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

function TJclComplex.CNewSub(const SubValue: TJclComplex): TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSub(RectCoord(Self), RectCoord(SubValue));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CNewSub(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CNewSub(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

function TJclComplex.CNeg;
begin
  FCoord.X := -FCoord.X;
  FCoord.Y := -FCoord.Y;
  Result := Self;
end;

function TJclComplex.CNewNeg;
begin
  Result := TJclComplex.Create(-FCoord.X, -FCoord.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CConjugate;
begin
  FCoord.Y := -FCoord.Y;
  Result := Self;
end;

function TJclComplex.CNewConjugate;
begin
  Result := TJclComplex.Create(FCoord.X, -FCoord.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//=== natural log and exponential functions ==================================

function TJclComplex.CoreLn(const LnValue: TRectCoord): TRectCoord;
begin
  Result.X := {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.Ln(LnValue.X);
  Result.Y := NormalizeAngle(LnValue.Y);
end;

function TJclComplex.CLn: TJclComplex;
var
  ResCoord: TRectCoord;
begin
  FillCoords(crRectangular);
  ResCoord := CoreLn(RectCoord(FCoord.R, FCoord.Theta));
  FCoord.X := ResCoord.X;
  FCoord.Y := ResCoord.Y;
  Result := Self;
end;

function TJclComplex.CNewLn: TJclComplex;
var
  ResCoord: TRectCoord;
begin
  FillCoords(crRectangular);
  ResCoord := CoreLn(RectCoord(FCoord.R, FCoord.Theta));
  Result := TJclComplex.Create(ResCoord.X, ResCoord.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreExp(const ExpValue: TRectCoord): TRectCoord;
var
  ExpX: Float;
begin
  ExpX := Exp(ExpValue.X);
  Result.X := ExpX * Cos(ExpValue.Y);
  Result.Y := ExpX * Sin(ExpValue.Y);
end;

function TJclComplex.CExp: TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreExp(RectCoord(FCoord.X, FCoord.Y));
  FCoord.X := ResCoord.X;
  FCoord.Y := ResCoord.Y;
  Result := Self;
end;

function TJclComplex.CNewExp: TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreExp(RectCoord(FCoord.X, FCoord.Y));
  Result := TJclComplex.Create(ResCoord.X, ResCoord.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CorePwr(First, Second, Polar: TRectCoord): TRectCoord;
begin
  First.X := MiscalcSingle(First.X);
  First.Y := MiscalcSingle(First.Y);
  Second.X := MiscalcSingle(Second.X);
  Second.Y := MiscalcSingle(Second.Y);
  if AbsoluteValueSqr(First) = 0.0 then
    if AbsoluteValueSqr(Second) = 0.0 then
      Result := RectOne
    else
      Result := RectZero
  else
    Result := CoreExp(CoreMul(Second, CoreLn(Polar)));
end;

function TJclComplex.CPwr(const PwrValue: TJclComplex): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CorePwr(RectCoord(Self), RectCoord(PwrValue), RectCoord(FCoord.R, FCoord.Theta));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CPwr(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CPwr(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

function TJclComplex.CNewPwr(PwrValue: TJclComplex): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CorePwr(RectCoord(Self), RectCoord(PwrValue), RectCoord(FCoord.R, FCoord.Theta));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CNewPwr(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CNewPwr(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

function TJclComplex.CoreIntPwr(First: TRectCoord; const Polar: TRectCoord; const Pwr: Integer): TRectCoord;
begin
  First.X := MiscalcSingle(First.X);
  First.Y := MiscalcSingle(First.Y);
  if AbsoluteValueSqr(First) = 0.0 then
    if Pwr = 0 then
      Result := RectOne
    else
      Result := RectZero
  else
    Result := RectCoord(PowerInt(Polar.X, Pwr), NormalizeAngle(Pwr * Polar.Y));
end;

function TJclComplex.CIntPwr(const Pwr: Integer): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CoreIntPwr(RectCoord(Self), RectCoord(FCoord.R, FCoord.Theta), Pwr);
  FCoord.R := ResValue.X;
  FCoord.Theta := ResValue.Y;
  FillCoords(crPolar);
  Result := Self;
end;

function TJclComplex.CNewIntPwr(const Pwr: Integer): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CoreIntPwr(RectCoord(Self), RectCoord(FCoord.R, FCoord.Theta), Pwr);
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crPolar);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreRealPwr(First: TRectCoord; const Polar: TRectCoord; const Pwr: Float): TRectCoord;
begin
  First.X := MiscalcSingle(First.X);
  First.Y := MiscalcSingle(First.Y);
  if AbsoluteValueSqr(First) = 0.0 then
    if MiscalcSingle(Pwr) = 0.0 then
      Result := RectOne
    else
      Result := RectZero
  else
    Result := RectCoord(Power(Polar.X, Pwr), NormalizeAngle(Pwr * Polar.Y));
end;

function TJclComplex.CRealPwr(const Pwr: Float): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CoreRealPwr(RectCoord(Self), RectCoord(FCoord.R, FCoord.Theta), Pwr);
  FCoord.R := ResValue.X;
  FCoord.Theta := ResValue.Y;
  FillCoords(crPolar);
  Result := Self;
end;

function TJclComplex.CNewRealPwr(const Pwr: Float): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CoreRealPwr(RectCoord(Self), RectCoord(FCoord.R, FCoord.Theta), Pwr);
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crPolar);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreRoot(First: TRectCoord; const Polar: TRectCoord; const K, N: Word): TRectCoord;
begin
  First.X := MiscalcSingle(First.X);
  First.Y := MiscalcSingle(First.Y);
  if AbsoluteValue(First) = 0.0 then
    Result := RectZero
  else
    Result := RectCoord(Power(Polar.X, 1.0 / N), NormalizeAngle((Polar.Y + K * TwoPi) / N));
end;

function TJclComplex.CRoot(const K, N: Word): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CoreRoot(RectCoord(Self), RectCoord(FCoord.R, FCoord.Theta), K, N);
  FCoord.R := ResValue.X;
  FCoord.Theta := ResValue.Y;
  FillCoords(crPolar);
  Result := Self;
end;

function TJclComplex.CNewRoot(const K, N: Word): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CoreRoot(RectCoord(Self), RectCoord(FCoord.R, FCoord.Theta), K, N);
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crPolar);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CSqrt: TJclComplex;
begin
  Result := CRoot(0, 2);
end;

function TJclComplex.CNewSqrt: TJclComplex;
begin
  Result := CNewRoot(0, 2);
end;

//=== trigonometric functions ================================================

function TJclComplex.CoreCos(const Value: TRectCoord): TRectCoord;
begin
  Result := RectCoord(Cos(Value.X) * CosH(Value.Y), -Sin(Value.X) * SinH(Value.Y));
end;

function TJclComplex.CCos: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCos(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewCos: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCos(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreSin(const Value: TRectCoord): TRectCoord;
begin
  Result := RectCoord(Sin(Value.X) * CosH(Value.Y), Cos(Value.X) * SinH(Value.Y));
end;

function TJclComplex.CSin: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSin(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewSin: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSin(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreTan(const Value: TRectCoord): TRectCoord;
var
  TempValue: Float;
begin
  TempValue := Cos(2.0 * Value.X) + CosH(2.0 * Value.Y);
  if MiscalcSingle(TempValue) <> 0.0 then
    Result := RectCoord(Sin(2.0 * Value.X) / TempValue, SinH(2.0 * Value.Y) / TempValue)
  else
    Result := RectInfinity;
end;

function TJclComplex.CTan: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreTan(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewTan: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreTan(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreCot(const Value: TRectCoord): TRectCoord;
var
  TempValue: Float;
begin
  TempValue := Cosh(2.0 * Value.Y) - Cos(2.0 * Value.X);
  if MiscalcSingle(TempValue) <> 0.0 then
    Result := RectCoord(Sin(2.0 * Value.X) / TempValue, -SinH(2.0 * Value.Y) / TempValue)
  else
    Result := RectInfinity;
end;

function TJclComplex.CCot: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCot(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewCot: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCot(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreSec(const Value: TRectCoord): TRectCoord;
var
  TempValue: TRectCoord;
begin
  TempValue := CoreCos(Value);
  if MiscalcSingle(AbsoluteValue(TempValue)) <> 0.0 then
    Result := CoreDiv(RectOne, TempValue)
  else
    Result := RectInfinity;
end;

function TJclComplex.CSec: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSec(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewSec: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSec(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreCsc(const Value: TRectCoord): TRectCoord;
var
  TempValue: TRectCoord;
begin
  TempValue := CoreSin(Value);
  if MiscalcSingle(AbsoluteValue(TempValue)) <> 0.0 then
    Result := CoreDiv(RectOne, TempValue)
  else
    Result := RectInfinity;
end;

function TJclComplex.CCsc: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCsc(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewCsc: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCsc(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//=== hyperbolic functions ===================================================

function TJclComplex.CoreCosH(const Value: TRectCoord): TRectCoord;
begin
  Result := RectCoord(CosH(Value.X) * Cos(Value.Y), SinH(Value.X) * Sin(Value.Y));
end;

function TJclComplex.CCosH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCosH(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewCosH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCosH(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreSinH(const Value: TRectCoord): TRectCoord;
begin
  Result := RectCoord(SinH(Value.X) * Cos(Value.Y), CosH(Value.X) * Sin(Value.Y));
end;

function TJclComplex.CSinH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSinH(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewSinH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSinH(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreTanH(const Value: TRectCoord): TRectCoord;
var
  TempValue: Float;
begin
  TempValue := CosH(2.0 * Value.X) + Cos(2.0 * Value.Y);
  if MiscalcSingle(TempValue) <> 0.0 then
    Result := RectCoord(SinH(2.0 * Value.X) / TempValue, Sin(2.0 * Value.Y) / TempValue)
  else
    Result := RectInfinity;
end;

function TJclComplex.CTanH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreTanH(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewTanH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreTanH(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreCotH(const Value: TRectCoord): TRectCoord;
var
  TempValue: Float;
begin
  TempValue := Cosh(2.0 * Value.X) - Cos(2.0 * Value.Y);
  if MiscalcSingle(TempValue) <> 0.0 then
    Result := RectCoord(SinH(2.0 * Value.X) / TempValue, -Sin(2.0 * Value.Y) / TempValue)
  else
    Result := RectInfinity;
end;

function TJclComplex.CCotH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCotH(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewCotH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCotH(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreSecH(const Value: TRectCoord): TRectCoord;
var
  TempValue: TRectCoord;
begin
  TempValue := CoreCosH(Value);
  if MiscalcSingle(AbsoluteValue(TempValue)) <> 0.0 then
    Result := CoreDiv(RectOne, TempValue)
  else
    Result := RectInfinity;
end;

function TJclComplex.CSecH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSecH(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewSecH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSecH(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreCscH(const Value: TRectCoord): TRectCoord;
var
  TempValue: TRectCoord;
begin
  TempValue := CoreSinH(Value);
  if MiscalcSingle(AbsoluteValue(TempValue)) <> 0.0 then
    Result := CoreDiv(RectOne, TempValue)
  else
    Result := RectInfinity;
end;

function TJclComplex.CCscH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCscH(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewCscH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCscH(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//=== complex Bessel functions of order zero =================================

function TJclComplex.CoreI0(const Value: TRectCoord): TRectCoord;
var
  ZSqr25, Term: TRectCoord;
  I: Integer;
  SizeSqr: Float;
begin
  Result := RectOne;
  ZSqr25 := CoreMul(Value, Value);
  ZSqr25 := RectCoord(0.25 * ZSqr25.X, 0.25 * ZSqr25.Y);
  Term := ZSqr25;
  Result := CoreAdd(Result, ZSqr25);
  I := 1;
  repeat
    Term := CoreMul(ZSqr25, Term);
    Inc(I);
    Term := RectCoord(Term.X / Sqr(I), Term.Y / Sqr(I));
    Result := CoreAdd(Result, Term);
    SizeSqr := Sqr(Term.X) + Sqr(Term.Y);
  until (I > MaxTerm) or (SizeSqr < EpsilonSqr);
end;

function TJclComplex.CI0: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreI0(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewI0: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreI0(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreJ0(const Value: TRectCoord): TRectCoord;
var
  ZSqr25, Term: TRectCoord;
  I: Integer;
  SizeSqr: Float;
  AddFlag: Boolean;
begin
  Result := RectOne;
  ZSqr25 := CoreMul(Value, Value);
  ZSqr25 := RectCoord(0.25 * ZSqr25.X, 0.25 * ZSqr25.Y);
  Term := ZSqr25;
  Result := CoreSub(Result, ZSqr25);
  AddFlag := False;
  I := 1;
  repeat
    Term := CoreMul(ZSqr25, Term);
    Inc(I);
    AddFlag := not AddFlag;
    Term := RectCoord(Term.X / Sqr(I), Term.Y / Sqr(I));
    if AddFlag then
      Result := CoreAdd(Result, Term)
    else
      Result := CoreSub(Result, Term);
    SizeSqr := Sqr(Term.X) + Sqr(Term.Y);
  until (I > MaxTerm) or (SizeSqr < EpsilonSqr);
end;

function TJclComplex.CJ0: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreJ0(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewJ0: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreJ0(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreApproxLnGamma(const Value: TRectCoord): TRectCoord;
const
  C: array [1..8] of Float =
   (1.0 / 12.0, -1.0 / 360.0, 1.0 / 1260.0, -1.0 / 1680.0,
    1.0 / 1188.0, -691.0 / 360360.0, 1.0 / 156.0, -3617.0 / 122400.0);
var
  I: Integer;
  Powers: array [1..8] of TRectCoord;
  Temp1, Temp2: TRectCoord;
begin
  Temp1 := CoreLn(Value);
  Temp2 := RectCoord(Value.X - 0.5, Value.Y);
  Result := CoreAdd(Temp1, Temp2);
  Result := CoreSub(Result, Value);
  Result.X := Result.X + hLn2PI;

  Temp1 := RectOne;
  Powers[1] := CoreDiv(Temp1, Value);
  Temp2 := CoreMul(powers[1], Powers[1]);
  for I := 2 to 8 do
    Powers[I] := CoreMul(Powers[I - 1], Temp2);
  for I := 8 downto 1 do
  begin
    Temp1 := RectCoord(C[I] * Powers[I].X, C[I] * Powers[I].Y);
    Result := CoreAdd(Result, Temp1);
  end;
end;

function TJclComplex.CApproxLnGamma: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreApproxLnGamma(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewApproxLnGamma: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreApproxLnGamma(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreLnGamma(Value: TRectCoord): TRectCoord;
var
  LNA, Temp: TRectCoord;
begin
  if (Value.X <= 0.0) and (MiscalcSingle(Value.Y) = 0.0) then
    if MiscalcSingle(Int(Value.X - 1E-8) - Value.X) = 0.0 then
    begin
      Result := RectInfinity;
      Exit;
    end;

  if Value.Y < 0.0 then
  begin
    Value := RectCoord(Value.X, -Value.Y);
    Result := CoreLnGamma(Value);
    Result := RectCoord(Result.X, -Result.Y);
  end
  else
  begin
    if Value.X < 9.0 then
    begin
      LNA := CoreLn(Value);
      Value := RectCoord(Value.X + 1, Value.Y);
      Temp := CoreLnGamma(Value);
      Result := CoreSub(Temp, LNA);
    end
    else
      CoreApproxLnGamma(Value);
  end;
end;

function TJclComplex.CLnGamma: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreLnGamma(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewLnGamma: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreLnGamma(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

function TJclComplex.CoreGamma(const Value: TRectCoord): TRectCoord;
var
  LNZ: TRectCoord;
begin
  LNZ := CoreLnGamma(Value);
  if LNZ.X > 75.0 then
    Result := RectInfinity
  else
    if LNZ.X < -200.0 then
      Result := RectZero
    else
      Result := CoreExp(LNZ);
end;

function TJclComplex.CGamma: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreGamma(RectCoord(Self));
  FCoord.X := ResValue.X;
  FCoord.Y := ResValue.Y;
  Result := Self;
end;

function TJclComplex.CNewGamma: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreGamma(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.X, ResValue.Y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//=== miscellaneous ==========================================================

function TJclComplex.AbsoluteValue: Float;
begin
  Result := Sqrt(Sqr(FCoord.X) + Sqr(FCoord.Y));
end;

function TJclComplex.AbsoluteValue(const Coord: TRectCoord): Float;
begin
  Result := Sqrt(Sqr(Coord.X) + Sqr(Coord.Y));
end;

function TJclComplex.AbsoluteValueSqr: Float;
begin
  Result := Sqr(FCoord.X) + Sqr(FCoord.Y);
end;

function TJclComplex.AbsoluteValueSqr(const Coord: TRectCoord): Float;
begin
  Result := Sqr(Coord.X) + Sqr(Coord.Y);
end;

function TJclComplex.FormatExtended(const X: Float): string;
begin
  Result := FloatToStrF(X, ffFixed, FFracLen, FFracLen);
end;

procedure TJclComplex.SetFracLen(const X: Byte);
begin
  if X > MaxFracLen then
    FFracLen := MaxFracLen
  else
    FFracLen := X;
end;

function TJclComplex.GetRadius: Float;
begin
  FillCoords(crRectangular);
  Result := FCoord.R;
end;

function TJclComplex.GetAngle: Float;
begin
  FillCoords(crRectangular);
  Result := FCoord.Theta;
end;

function TJclComplex.NormalizeAngle(Value: Float): Float;
begin
  FillCoords(crRectangular);
  while Value > Pi do
    Value := Value - TwoPi;
  while Value < -Pi do
    Value := Value + TwoPi;
  Value := MiscalcSingle(Value);
  Result := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
