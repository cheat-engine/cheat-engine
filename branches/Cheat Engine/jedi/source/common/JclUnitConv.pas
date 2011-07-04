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
{ The Original Code is JclUnitConv.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright Marcel van Brakel. All rights reserved.      }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   ESB Consultancy                                                                                }
{   Manlio Laschena                                                                                }
{   Allan Lyons                                                                                    }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Scott Price (scottprice)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains routines to perform conversion between various units such as length coordinate,         }
{ temperature, angle, mass and pressure conversions.                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclUnitConv;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils,
  JclBase;

const
  { Temperature constants }

  CelsiusFreezingPoint    = 0.0;
  FahrenheitFreezingPoint = 32.0;
  KelvinFreezingPoint     = 273.15;
  CelsiusBoilingPoint     = 100.0 + CelsiusFreezingPoint;
  FahrenheitBoilingPoint  = 180.0 + FahrenheitFreezingPoint;
  KelvinBoilingPoint      = 100.0 + KelvinFreezingPoint;
  CelsiusAbsoluteZero     = -273.15;
  FahrenheitAbsoluteZero  = -459.67;
  KelvinAbsoluteZero      = 0.0;

  { Newly added for Rankine and Reaumur Support by scottprice }
  RankineAbsoluteZero = 0.0;
  RankineAtFahrenheitZero = 459.67;
  RankineFreezingPoint = 491.67;
  RankineBoilingPoint = 180 + RankineFreezingPoint;
  ReaumurAbsoluteZero = -218.52;
  ReaumurFreezingPoint = 0.0;
  ReaumurBoilingPoint = 80.0;


  { Mathematical constants }

  DegPerCycle: Float      = 360.0;
  DegPerGrad: Float       = 0.9;
  DegPerRad: Float        = 57.295779513082320876798154814105;
  GradPerCycle: Float     = 400.0;
  GradPerDeg: Float       = 1.1111111111111111111111111111111;
  GradPerRad: Float       = 63.661977236758134307553505349006;
  RadPerCycle: Float      = 6.283185307179586476925286766559;
  RadPerDeg: Float        = 0.017453292519943295769236907684886;
  RadPerGrad: Float       = 0.015707963267948966192313216916398;
  CyclePerDeg: Float      = 0.0027777777777777777777777777777778;
  CyclePerGrad: Float     = 0.0025;
  CyclePerRad: Float      = 0.15915494309189533576888376337251;
  ArcMinutesPerDeg        = 60.0;
  ArcSecondsPerArcMinute  = 60.0;
  ArcSecondsPerDeg        = ArcSecondsPerArcMinute * ArcMinutesPerDeg;
  DegPerArcMinute         = 1 / ArcMinutesPerDeg;
  DegPerArcSecond         = 1 / ArcSecondsPerDeg;


type
  { Exception classes }
  EUnitConversionError = class(EJclError);

  ETemperatureConversionError = class(EUnitConversionError);

  { Temperature type enumeration used for the general routine allowing for
    a more dynamic specification of the source or target temperature types }
  TTemperatureType = (ttCelsius, ttFahrenheit, ttKelvin, ttRankine, ttReaumur);


function HowAOneLinerCanBiteYou(const Step, Max: Longint): Longint;
function MakePercentage(const Step, Max: Longint): Longint;

{ New Temperature routines }
{ Old temperature routines removed and archived incase required again - scottprice }

function CelsiusToFahrenheit(const Temperature: Float): Float;
function CelsiusToKelvin(const Temperature: Float): Float;
function CelsiusToRankine(const Temperature: Float): Float;
function CelsiusToReaumur(const Temperature: Float): Float;
function FahrenheitToCelsius(const Temperature: Float): Float;
function FahrenheitToKelvin(const Temperature: Float): Float;
function FahrenheitToRankine(const Temperature: Float): Float;
function FahrenheitToReaumur(const Temperature: Float): Float;
function KelvinToCelsius(const Temperature: Float): Float;
function KelvinToFahrenheit(const Temperature: Float): Float;
function KelvinToRankine(const Temperature: Float): Float;
function KelvinToReaumur(const Temperature: Float): Float;
function RankineToCelsius(const Temperature: Float): Float;
function RankineToFahrenheit(const Temperature: Float): Float;
function RankineToKelvin(const Temperature: Float): Float;
function RankineToReaumur(const Temperature: Float): Float;
function ReaumurToCelsius(const Temperature: Float): Float;
function ReaumurToFahrenheit(const Temperature: Float): Float;
function ReaumurToKelvin(const Temperature: Float): Float;
function ReaumurToRankine(const Temperature: Float): Float;
function ConvertTemperature(const FromType, ToType: TTemperatureType; const Temperature: Float): Float;
function CelsiusTo(ToType: TTemperatureType; const Temperature: Float): Float;
function FahrenheitTo(ToType: TTemperatureType; const Temperature: Float): Float;
function KelvinTo(ToType: TTemperatureType; const Temperature: Float): Float;
function RankineTo(ToType: TTemperatureType; const Temperature: Float): Float;
function ReaumurTo(ToType: TTemperatureType; const Temperature: Float): Float;

{ Angle conversion }

function CycleToDeg(const Cycles: Float): Float;
function CycleToGrad(const Cycles: Float): Float;
function CycleToRad(const Cycles: Float): Float;
function DegToCycle(const Degrees: Float): Float;
function DegToGrad(const Degrees: Float): Float;
function DegToRad(const Degrees: Float): Float;
function GradToCycle(const Grads: Float): Float;
function GradToDeg(const Grads: Float): Float;
function GradToRad(const Grads: Float): Float;
function RadToCycle(const Radians: Float): Float;
function RadToDeg(const Radians: Float): Float;
function RadToGrad(const Radians: Float): Float;
function DmsToDeg(const D, M: Integer; const S: Float): Float;
function DmsToRad(const D, M: Integer; const S: Float): Float;
procedure DegToDms(const Degrees: Float; out D, M: Integer; out S: Float);
function DegToDmsStr(const Degrees: Float; const SecondPrecision: Cardinal = 3): string;

{ Coordinate conversion }

procedure CartesianToPolar(const X, Y: Float; out R, Phi: Float);
procedure PolarToCartesian(const R, Phi: Float; out X, Y: Float);
procedure CartesianToCylinder(const X, Y, Z: Float; out R, Phi, Zeta: Float);
procedure CartesianToSpheric(const X, Y, Z: Float; out Rho, Phi, Theta: Float);
procedure CylinderToCartesian(const R, Phi, Zeta: Float; out X, Y, Z: Float);
procedure SphericToCartesian(const Rho, Theta, Phi: Float; out X, Y, Z: Float);

{ Length conversion }

function CmToInch(const Cm: Float): Float;
function InchToCm(const Inch: Float): Float;
function FeetToMetre(const Feet: Float): Float;
function MetreToFeet(const Metre: Float): Float;
function YardToMetre(const Yard: Float): Float;
function MetreToYard(const Metre: Float): Float;
function NmToKm(const Nm: Float): Float;
function KmToNm(const Km: Float): Float;
function KmToSm(const Km: Float): Float;
function SmToKm(const Sm: Float): Float;

{ Volume conversion }

function LitreToGalUs(const Litre: Float): Float;
function GalUsToLitre(const GalUs: Float): Float;
function GalUsToGalCan(const GalUs: Float): Float;
function GalCanToGalUs(const GalCan: Float): Float;
function GalUsToGalUk(const GalUs: Float): Float;
function GalUkToGalUs(const GalUk: Float): Float;
function LitreToGalCan(const Litre: Float): Float;
function GalCanToLitre(const GalCan: Float): Float;
function LitreToGalUk(const Litre: Float): Float;
function GalUkToLitre(const GalUk: Float): Float;

{ Mass conversion }

function KgToLb(const Kg: Float): Float;
function LbToKg(const Lb: Float): Float;
function KgToOz(const Kg: Float): Float;
function OzToKg(const Oz: Float): Float;
function CwtUsToKg(const Cwt: Float): Float;
function CwtUkToKg(const Cwt: Float): Float;
function KaratToKg(const Karat: Float): Float;
function KgToCwtUs(const Kg: Float): Float;
function KgToCwtUk(const Kg: Float): Float;
function KgToKarat(const Kg: Float): Float;
function KgToSton(const Kg: Float): Float;
function KgToLton(const Kg: Float): Float;
function StonToKg(const STon: Float): Float;
function LtonToKg(const Lton: Float): Float;
function QrUsToKg(const Qr: Float): Float;
function QrUkToKg(const Qr: Float): Float;
function KgToQrUs(const Kg: Float): Float;
function KgToQrUk(const Kg: Float): Float;

{ Pressure conversion }

function PascalToBar(const Pa: Float): Float;
function PascalToAt(const Pa: Float): Float;
function PascalToTorr(const Pa: Float): Float;
function BarToPascal(const Bar: Float): Float;
function AtToPascal(const At: Float): Float;
function TorrToPascal(const Torr: Float): Float;

{ Other conversions }

function KnotToMs(const Knot: Float): Float;
function HpElectricToWatt(const HpE: Float): Float;
function HpMetricToWatt(const HpM: Float): Float;
function MsToKnot(const Ms: Float): Float;
function WattToHpElectric(const W: Float): Float;
function WattToHpMetric(const W: Float): Float;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclUnitConv.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclMath, JclResources;

function HowAOneLinerCanBiteYou(const Step, Max: Longint): Longint;
begin
  Result := MakePercentage(Step, Max);
end;

function MakePercentage(const Step, Max: Longint): Longint;
begin
  Assert(Max <> 0);
  Result := Round((Step * 100.0) / Max);
end;

//=== Temperature conversion =================================================

procedure TemperatureBelowAbsoluteError;
begin
  {$IFDEF CLR}
  raise ETemperatureConversionError.Create(RsConvTempBelowAbsoluteZero);
  {$ELSE}
  raise ETemperatureConversionError.CreateRes(@RsConvTempBelowAbsoluteZero);
  {$ENDIF CLR}
end;

function CelsiusToFahrenheit(const Temperature: Float): Float;
begin
  if Temperature < CelsiusAbsoluteZero then
    TemperatureBelowAbsoluteError;

  Result := (((FahrenheitBoilingPoint-FahrenheitFreezingPoint) /
    CelsiusBoilingPoint) * Temperature) + FahrenheitFreezingPoint;

  // °F = °C × 1.8 + 32
  // Alternative:  Result := Temperature * 1.8 + 32;
end;

function CelsiusToKelvin(const Temperature: Float): Float;
begin
  if Temperature < CelsiusAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // K = °C + 273.15
  Result := Temperature + KelvinFreezingPoint;
end;

function CelsiusToRankine(const Temperature: Float): Float;
begin
  if Temperature < CelsiusAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °R = (°C × 1.8) + 32 + 459.67
  if Temperature = CelsiusAbsoluteZero then
  begin
    Result := RankineAbsoluteZero;
  end else
  begin
    Result := RankineFreezingPoint - FahrenheitFreezingPoint +
      CelsiusToFahrenheit(Temperature);
  end;
end;

function CelsiusToReaumur(const Temperature: Float): Float;
begin
  if Temperature < CelsiusAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °R = °C × 0.8
  Result := Temperature * 0.8;
end;

function FahrenheitToCelsius(const Temperature: Float): Float;
begin
  if Temperature < FahrenheitAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °C = (°F - 32) / 1.8
  Result := (CelsiusBoilingPoint /
    (FahrenheitBoilingPoint-FahrenheitFreezingPoint)) *
    (Temperature - FahrenheitFreezingPoint);
end;

function FahrenheitToKelvin(const Temperature: Float): Float;
begin
  if Temperature < FahrenheitAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // K = (°F + 459.67) / 1.8
  Result := FahrenheitToCelsius(Temperature) + KelvinFreezingPoint;
end;

function FahrenheitToRankine(const Temperature: Float): Float;
begin
  if Temperature < FahrenheitAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °Ra = °F + 459.67
  Result := Temperature + RankineAtFahrenheitZero;
end;

function FahrenheitToReaumur(const Temperature: Float): Float;
begin
  if Temperature < FahrenheitAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °R = (°F - 32) / 2.25
  Result := (Temperature - FahrenheitFreezingPoint) / 2.25;
end;

function KelvinToCelsius(const Temperature: Float): Float;
begin
  if Temperature < KelvinAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °C = K - 273.15
  Result := Temperature - KelvinFreezingPoint;
end;

function KelvinToFahrenheit(const Temperature: Float): Float;
begin
  if Temperature < KelvinAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °F = K × 1.8 - 459.67
  Result := FahrenheitToCelsius(Temperature - KelvinFreezingPoint);
end;

function KelvinToRankine(const Temperature: Float): Float;
begin
  if Temperature < KelvinAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °Ra = K × 1.8
  Result := Temperature * 1.8;
end;

function KelvinToReaumur(const Temperature: Float): Float;
begin
  if Temperature < KelvinAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °R = (K - 273.15) × 0.8
  Result := (Temperature - KelvinFreezingPoint) * 0.8;
end;

function RankineToCelsius(const Temperature: Float): Float;
begin
  if Temperature < RankineAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °C = (°R - 32 - 459.67) / 1.8
  Result := (Temperature - RankineFreezingPoint) / 1.8;
end;

function RankineToFahrenheit(const Temperature: Float): Float;
begin
  if Temperature < RankineAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °F = °R - 459.67
  Result := Temperature - RankineAtFahrenheitZero;
end;

function RankineToKelvin(const Temperature: Float): Float;
begin
  if Temperature < RankineAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // K = °R / 1.8
  Result := Temperature / 1.8;
end;

function RankineToReaumur(const Temperature: Float): Float;
begin
  if Temperature < RankineAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °R = (°Ra - 32 - 459.67) / 2.25
  Result := (Temperature - RankineFreezingPoint) / 2.25;
end;

function ReaumurToCelsius(const Temperature: Float): Float;
begin
  if Temperature < ReaumurAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °C = °R × 1.25
  Result := Temperature * 1.25;
end;

function ReaumurToFahrenheit(const Temperature: Float): Float;
begin
  if Temperature < ReaumurAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °F = °R × 2.25 + 32
  Result := (Temperature * 2.25) + FahrenheitFreezingPoint;
end;

function ReaumurToKelvin(const Temperature: Float): Float;
begin
  if Temperature < ReaumurAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // K = °R × 1.25 + 273.15
  Result := (Temperature * 1.25) + KelvinFreezingPoint;
end;

function ReaumurToRankine(const Temperature: Float): Float;
begin
  if Temperature < ReaumurAbsoluteZero then
    TemperatureBelowAbsoluteError;

  // °Ra = °R × 2.25 + 32 + 459.67
  Result := (Temperature * 2.25) + RankineFreezingPoint;
end;

function ConvertTemperature(const FromType, ToType: TTemperatureType; const Temperature: Float): Float;
const
  cToType = 'ToType';
  cFromType = 'FromType';
begin
  case FromType of
    { All conversions from Celcius to other formats are listed here }
    ttCelsius:
      begin
        case ToType of
          ttFahrenheit:
            Result := CelsiusToFahrenheit(Temperature);
          ttKelvin:
            Result := CelsiusToKelvin(Temperature);
          ttRankine:
            Result := CelsiusToRankine(Temperature);
          ttReaumur:
            Result := CelsiusToReaumur(Temperature);
        else
          {$IFDEF CLR}
          raise EInvalidOp.CreateFmt(RsTempConvTypeError, [cToType]);
          {$ELSE}
          raise EInvalidOp.CreateResFmt(@RsTempConvTypeError, [cToType]);
          {$ENDIF CLR}
        end;
      end;
    { All conversions from Fahrenheit to other formats are listed here }
    ttFahrenheit:
      begin
        case ToType of
          ttCelsius:
            Result := FahrenheitToCelsius(Temperature);
          ttKelvin:
            Result := FahrenheitToKelvin(Temperature);
          ttRankine:
            Result := FahrenheitToRankine(Temperature);
          ttReaumur:
            Result := FahrenheitToReaumur(Temperature);
        else
          {$IFDEF CLR}
          raise EInvalidOp.CreateFmt(RsTempConvTypeError, [cToType]);
          {$ELSE}
          raise EInvalidOp.CreateResFmt(@RsTempConvTypeError, [cToType]);
          {$ENDIF CLR}
        end;
      end;
    { All conversions from Kelvin to other formats are listed here }
    ttKelvin:
      begin
        case ToType of
          ttCelsius:
            Result := KelvinToCelsius(Temperature);
          ttFahrenheit:
            Result := KelvinToFahrenheit(Temperature);
          ttRankine:
            Result := KelvinToRankine(Temperature);
          ttReaumur:
            Result := KelvinToReaumur(Temperature);
        else
          {$IFDEF CLR}
          raise EInvalidOp.CreateFmt(RsTempConvTypeError, [cToType]);
          {$ELSE}
          raise EInvalidOp.CreateResFmt(@RsTempConvTypeError, [cToType]);
          {$ENDIF CLR}
        end;
      end;
    { All conversions from Kelvin to other formats are listed here }
    ttRankine:
      begin
        case ToType of
          ttCelsius:
            Result := RankineToCelsius(Temperature);
          ttFahrenheit:
            Result := RankineToFahrenheit(Temperature);
          ttKelvin:
            Result := RankineToKelvin(Temperature);
          ttReaumur:
            Result := RankineToReaumur(Temperature);
        else
          {$IFDEF CLR}
          raise EInvalidOp.CreateFmt(RsTempConvTypeError, [cToType]);
          {$ELSE}
          raise EInvalidOp.CreateResFmt(@RsTempConvTypeError, [cToType]);
          {$ENDIF CLR}
        end;
      end;
    { All conversions from Reaumur to other formats are listed here }
    ttReaumur:
      begin
        case ToType of
          ttCelsius:
            Result := ReaumurToCelsius(Temperature);
          ttFahrenheit:
            Result := ReaumurToFahrenheit(Temperature);
          ttKelvin:
            Result := ReaumurToKelvin(Temperature);
          ttRankine:
            Result := ReaumurToRankine(Temperature);
        else
          {$IFDEF CLR}
          raise EInvalidOp.CreateFmt(RsTempConvTypeError, [cToType]);
          {$ELSE}
          raise EInvalidOp.CreateResFmt(@RsTempConvTypeError, [cToType]);
          {$ENDIF CLR}
        end;
      end;
  else
    {$IFDEF CLR}
    raise EInvalidOp.CreateFmt(RsTempConvTypeError, [cFromType]);
    {$ELSE}
    raise EInvalidOp.CreateResFmt(@RsTempConvTypeError, [cFromType]);
    {$ENDIF CLR}
  end;
end;

function CelsiusTo(ToType: TTemperatureType; const Temperature: Float): Float;
begin
  Result := ConvertTemperature(ttCelsius, ToType, Temperature);
end;

function FahrenheitTo(ToType: TTemperatureType; const Temperature: Float): Float;
begin
  Result := ConvertTemperature(ttFahrenheit, ToType, Temperature);
end;

function KelvinTo(ToType: TTemperatureType; const Temperature: Float): Float;
begin
  Result := ConvertTemperature(ttKelvin, ToType, Temperature);
end;

function RankineTo(ToType: TTemperatureType; const Temperature: Float): Float;
begin
  Result := ConvertTemperature(ttRankine, ToType, Temperature);
end;

function ReaumurTo(ToType: TTemperatureType; const Temperature: Float): Float;
begin
  Result := ConvertTemperature(ttReaumur, ToType, Temperature);
end;

//=== Angle conversion =======================================================

function CycleToDeg(const Cycles: Float): Float;
begin
  Result := Cycles * DegPerCycle;
end;

function CycleToGrad(const Cycles: Float): Float;
begin
  Result := Cycles * GradPerCycle;
end;

function CycleToRad(const Cycles: Float): Float;
begin
  Result := Cycles * RadPerCycle;
end;

function DegToGrad(const Degrees: Float): Float;
begin
  Result := Degrees * GradPerDeg;
end;

function DegToCycle(const Degrees: Float): Float;
begin
  Result := Degrees * CyclePerDeg;
end;

function DegToRad(const Degrees: Float): Float;
begin
  Result := Degrees * RadPerDeg;
end;

function GradToCycle(const Grads: Float): Float;
begin
  Result := Grads * CyclePerGrad;
end;

function GradToDeg(const Grads: Float): Float;
begin
  Result := Grads * DegPerGrad;
end;

function GradToRad(const Grads: Float): Float;
begin
  Result := Grads * RadPerGrad;
end;

function RadToCycle(const Radians: Float): Float;
begin
  Result := Radians * CyclePerRad;
end;

function RadToDeg(const Radians: Float): Float;
begin
  Result := Radians * DegPerRad;
end;

function RadToGrad(const Radians: Float): Float;
begin
  Result := Radians * GradPerRad;
end;

function DmsToDeg(const D, M: Integer; const S: Float): Float;
begin
  DomainCheck((M < 0) or (M > 60) or (S < 0.0) or (S > 60.0));
  Result := Abs(D) + M * DegPerArcMinute + S * DegPerArcSecond;
  if D < 0 then
    Result := -Result;
end;

function DmsToRad(const D, M: Integer; const S: Float): Float;
begin
  Result := DegToRad(DmsToDeg(D, M, S));
end;

procedure DegToDms(const Degrees: Float; out D, M: Integer; out S: Float);
var
  DD, MM: Float;
begin
  DD := Abs(Degrees);
  MM := Frac(DD) * ArcMinutesPerDeg;
  D := Trunc(DD);
  M := Trunc(MM);
  S := Frac(MM) * ArcSecondsPerArcMinute;
  if Degrees < 0 then
    D := -D;
end;

function DegToDmsStr(const Degrees: Float; const SecondPrecision: Cardinal = 3): string;
var
  D, M: Integer;
  S: Float;
begin
  DegToDMS(Degrees, D, M, S);
  Result := Format('%d° %d'' %.*f"', [D, M, SecondPrecision, S]);
end;

//=== Coordinate conversion ==================================================

procedure CartesianToCylinder(const X, Y, Z: Float; out R, Phi, Zeta: Float);
begin
  Zeta := Z;
  CartesianToPolar(X, Y, R, Phi);
end;

procedure CartesianToPolar(const X, Y: Float; out R, Phi: Float);
begin
  R := Sqrt(Sqr(X) + Sqr(Y));
  Phi := ArcTan2(Y, X);
  if Phi < 0 then
    Phi := Phi + TwoPi;
end;

procedure CartesianToSpheric(const X, Y, Z: Float; out Rho, Phi, Theta: Float);
begin
  Rho := Sqrt(X*X + Y*Y + Z*Z);
  Phi := ArcTan2(Y, X);
  if Phi < 0 then
    Phi := Phi + TwoPi;
  Theta := 0;
  if Rho > 0 then
    Theta := ArcCos(Z/Rho);
end;

procedure CylinderToCartesian(const R, Phi, Zeta: Float; out X, Y, Z: Float);
var
  Sine, CoSine: Float;
begin
  SinCos(Phi, Sine, Cosine);
  X := R * CoSine;
  Y := R * Sine;
  Z := Zeta;
end;

procedure PolarToCartesian(const R, Phi: Float; out X, Y: Float);
var
  Sine, CoSine: Float;
begin
  SinCos(Phi, Sine, CoSine);
  X := R * CoSine;
  Y := R * Sine;
end;

procedure SphericToCartesian(const Rho, Theta, Phi: Float; out X, Y, Z: Float);
var
  SineTheta, CoSineTheta: Float;
  SinePhi, CoSinePhi: Float;
begin
  SinCos(Theta, SineTheta, CoSineTheta);
  SinCos(Phi, SinePhi, CoSinePhi);
  X := Rho * SineTheta * CoSinePhi;
  Y := Rho * SineTheta * SinePhi;
  Z := Rho * CoSineTheta;
end;

//=== Length conversion ======================================================

function CmToInch(const Cm: Float): Float;
begin
  Result := Cm / 2.54;
end;

function InchToCm(const Inch: Float): Float;
begin
  Result := Inch * 2.54;
end;

function FeetToMetre(const Feet: Float): Float;
begin
  Result := Feet * 0.3048;
end;

function MetreToFeet(const Metre: Float): Float;
begin
  Result := Metre / 0.3048;
end;

function YardToMetre(const Yard: Float): Float;
begin
  Result := Yard * 0.9144;
end;

function MetreToYard(const Metre: Float): Float;
begin
  Result := Metre / 0.9144;
end;

function NmToKm(const Nm: Float): Float;
begin
  Result := Nm * 1.852;
end;

function KmToNm(const Km: Float): Float;
begin
  Result := Km / 1.852;
end;

function KmToSm(const Km: Float): Float;
begin
  Result := Km / 1.609344;
end;

function SmToKm(const Sm: Float): Float;
begin
  Result := Sm * 1.609344;
end;

//=== Volume conversion ======================================================

function LitreToGalUs(const Litre: Float): Float;
begin
  Result := Litre / 3.785411784;
end;

function GalUsToLitre(const GalUs: Float): Float;
begin
  Result := GalUs * 3.785411784;
end;

function GalUsToGalCan(const GalUs: Float): Float;
begin
  Result := GalUs / 1.2009499255;
end;

function GalCanToGalUs(const GalCan: Float): Float;
begin
  Result := GalCan * 1.2009499255;
end;

function GalUsToGalUk(const GalUs: Float): Float;
begin
  Result := GalUs / 1.20095045385;
end;

function GalUkToGalUs(const GalUk: Float): Float;
begin
  Result := GalUk * 1.20095045385;
end;

function LitreToGalCan(const Litre: Float): Float;
begin
  Result := Litre / 4.54609;
end;

function GalCanToLitre(const GalCan: Float): Float;
begin
  Result := GalCan * 4.54609;
end;

function LitreToGalUk(const Litre: Float): Float;
begin
  Result := Litre / 4.54609;
end;

function GalUkToLitre(const GalUk: Float): Float;
begin
  Result := GalUk * 4.54609;
end;

//=== Mass conversion ========================================================

function KgToLb(const Kg: Float): Float;
begin
  Result := Kg / 0.45359237;
end;

function LbToKg(const Lb: Float): Float;
begin
  Result := Lb * 0.45359237;
end;

function KgToOz(const Kg: Float): Float;
begin
  Result := Kg * 35.2739619496;
end;

function OzToKg(const Oz: Float): Float;
begin
  Result := Oz / 35.2739619496;
end;

function QrUsToKg(const Qr: Float) : Float;
begin
  Result := Qr * 11.34;
end;

function QrUkToKg(const Qr: Float) : Float;
begin
  Result := Qr * 12.7;
end;

function KgToQrUs(const Kg: Float) : Float;
begin
  Result := Kg / 11.34;
end;

function KgToQrUk(const Kg: Float) : Float;
begin
  Result := Kg / 12.7;
end;

function CwtUsToKg(const Cwt: Float) : Float;
begin
  Result := Cwt * 45.35924;
end;

function CwtUkToKg(const Cwt: Float) : Float;
begin
  Result := Cwt * 50.80235;
end;

function KgToCwtUs(const Kg: Float) : Float;
begin
  Result := Kg / 45.35924;
end;

function KgToCwtUk(const Kg: Float) : Float;
begin
  Result := Kg / 50.80235;
end;

function LtonToKg(const Lton: Float) : Float;
begin
  Result := Lton * 1016.047;
end;

function StonToKg(const Ston: Float) : Float;
begin
  Result := Ston * 907.1847;
end;

function KgToLton(const Kg: Float) : Float;
begin
  Result := Kg / 1016.047;
end;

function KgToSton(const Kg: Float) : Float;
begin
  Result := Kg / 907.1847;
end;

function KgToKarat(const Kg: Float) : Float;
begin
  Result := Kg / 0.0002;
end;

function KaratToKg(const Karat: Float) : Float;
begin
  Result := Karat * 0.0002;
end;


//=== Pressure conversion ====================================================

function PascalToBar(const Pa: Float): Float;
begin
  Result := Pa / 100000.0;
end;

function PascalToAt(const Pa: Float): Float;
begin
  Result := Pa / (9.80665 * 10000.0);
end;

function PascalToTorr(const Pa: Float): Float;
begin
  Result := Pa / 133.3224;
end;

function BarToPascal(const Bar: Float): Float;
begin
  Result := Bar * 100000.0;
end;

function AtToPascal(const At: Float): Float;
begin
  Result := At * (9.80665 * 10000.0);
end;

function TorrToPascal(const Torr: Float): Float;
begin
  Result := Torr * 133.3224;
end;

//=== Other conversion =======================================================

function KnotToMs(const Knot: Float): Float;
begin
  Result := Knot * 0.514444444444;
end;

function HpElectricToWatt(const HpE: Float): Float;
begin
  Result := HpE * 746.0;
end;

function HpMetricToWatt(const HpM: Float): Float;
begin
  Result := HpM * 735.4988;
end;

function MsToKnot(const Ms: Float): Float;
begin
  Result := Ms / 0.514444444444;
end;

function WattToHpElectric(const W: Float): Float;
begin
  Result := W / 746.0;
end;

function WattToHpMetric(const W: Float): Float;
begin
  Result := W / 735.4988;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
