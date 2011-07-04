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
{ The Original Code is JclStatistics.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is ESB Consultancy.                                   }
{ Portions created by ESB Consultancy are Copyright ESB Consultancy. All rights reserved.          }
{                                                                                                  }
{ Contributors (in alphabetical order):                                                            }
{   ESB Consultancy                                                                                }
{   Fred Hovey                                                                                     }
{   Marcel van Brakel                                                                              }
{   Matthias Thoma                                                                                 }
{   Robert Marquardt                                                                               }
{   Robert Rossmair                                                                                }
{   Petr Vones                                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various common statistics routines to calculate, for example, the arithmetic mean, geometric     }
{ meanor median of a set of numbers.                                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

{ TODO : Test cases! }

unit JclStatistics;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase, JclMath;

type
  EJclStatisticsError = class(EJclMathError);

{ Mean functions }

function ArithmeticMean(const X: TDynFloatArray): Float;
function GeometricMean(const X: TDynFloatArray): Float;
function HarmonicMean(const X: TDynFloatArray): Float;
function HeronianMean(const A, B: Float): Float;

{ Miscellanous }

function BinomialCoeff(N, R: Cardinal): Float;
function IsPositiveFloatArray(const X: TDynFloatArray): Boolean;
function MaxFloatArray(const B: TDynFloatArray): Float;
function MaxFloatArrayIndex(const B: TDynFloatArray): Integer;
function Median(const X: TDynFloatArray): Float;
{$IFNDEF CLR}
function MedianUnsorted(const X: TDynFloatArray): Float;
{$ENDIF ~CLR}
function MinFloatArray(const B: TDynFloatArray): Float;
function MinFloatArrayIndex(const B: TDynFloatArray): Integer;
function Permutation(N, R: Cardinal): Float;
function Combinations(N, R: Cardinal): Float;
function SumOfSquares(const X: TDynFloatArray): Float;
function PopulationVariance(const X: TDynFloatArray): Float;
procedure PopulationVarianceAndMean(const X: TDynFloatArray; var Variance, Mean: Float);
function SampleVariance(const X: TDynFloatArray): Float;
procedure SampleVarianceAndMean(const X: TDynFloatArray; var Variance, Mean: Float);
function StdError(const X: TDynFloatArray): Float; overload;
function StdError(const Variance: Float; const SampleSize: Integer): Float; overload;
function SumFloatArray(const B: TDynFloatArray): Float;
function SumSquareDiffFloatArray(const B: TDynFloatArray; Diff: Float): Float;
function SumSquareFloatArray(const B: TDynFloatArray): Float;
function SumPairProductFloatArray(const X, Y: TDynFloatArray): Float;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclStatistics.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclLogic,
  {$IFNDEF CLR}
  JclSysUtils,
  {$ENDIF ~CLR}
  JclResources;

//=== Local helpers ==========================================================

function GetDynLength(const X: TDynFloatArray): Integer;
begin
  Result := Length(X);
end;

function GetDynLengthNotNull(const X: TDynFloatArray): Integer;
begin
  Result := Length(X);
  if Result = 0 then
    {$IFDEF CLR}
    raise EJclMathError.Create(RsEmptyArray);
    {$ELSE}
    raise EJclMathError.CreateRes(@RsEmptyArray);
    {$ENDIF CLR}
end;

procedure InvalidSampleSize(SampleSize: Integer);
begin
  {$IFDEF CLR}
  raise EJclStatisticsError.CreateFmt(RsInvalidSampleSize, [SampleSize]);
  {$ELSE}
  raise EJclStatisticsError.CreateResFmt(@RsInvalidSampleSize, [SampleSize]);
  {$ENDIF CLR}
end;

function GetSampleSize(const Sample: TDynFloatArray; MinValidSize: Integer = 1): Integer;
begin
  Result := Length(Sample);
  if Result < MinValidSize then
    InvalidSampleSize(Result);
end;

//=== Mean Functions =========================================================

function ArithmeticMean(const X: TDynFloatArray): Float;
begin
  Result := SumFloatArray(X) / Length(X);
end;

function GeometricMean(const X: TDynFloatArray): Float;
var
  I, N: Integer;
begin
  N := GetSampleSize(X);
  Result := 1.0;
  for I := 0 to N - 1 do
  begin
    if X[I] <= PrecisionTolerance then
      {$IFDEF CLR}
      raise EJclMathError.Create(RsNonPositiveArray);
      {$ELSE}
      raise EJclMathError.CreateRes(@RsNonPositiveArray);
      {$ENDIF CLR}
    Result := Result * X[I];
  end;
  Result := Power(Result, 1 / N);
end;

function HarmonicMean(const X: TDynFloatArray): Float;
var
  I, N: Integer;
begin
  Result := 0.0;
  N := GetSampleSize(X);
  for I := 0 to N - 1 do
  begin
    if X[I] <= PrecisionTolerance then
      {$IFDEF CLR}
      raise EJclMathError.Create(RsNonPositiveArray);
      {$ELSE}
      raise EJclMathError.CreateRes(@RsNonPositiveArray);
      {$ENDIF CLR}
    Result := Result + 1 / X[I];
  end;
  Result := N / Result;
end;

function HeronianMean(const A, B: Float): Float;
begin
  Assert(A >= 0);
  Assert(B >= 0);
  Result := (A + Sqrt(A * B) + B) / 3;
end;

//=== Miscellanous ===========================================================

function BinomialCoeff(N, R: Cardinal): Float;
var
  I: Integer;
  K: LongWord;
begin
  if (N = 0) or (R > N) or (N > MaxFactorial) then
  begin
    Result := 0.0;
    Exit;
  end;
  Result := 1.0;
  if not ((R = 0) or (R = N)) then
  begin
    if R > N div 2 then
    R := N - R;
    K := 2;
    try
      for I := N - R + 1 to N do
      begin
        Result := Result * I;
        if K <= R then
        begin
          Result := Result / K;
          Inc(K);
        end;
      end;
      Result := Int(Result + 0.5);
    except
      Result := -1.0;
    end;
  end;
end;


function IsPositiveFloatArray(const X: TDynFloatArray): Boolean;
var
  I, N: Integer;
begin
  Result := False;
  N := GetDynLengthNotNull(X);
  for I := 0 to N - 1 do
    if X[I] <= PrecisionTolerance then
      Exit;
  Result := True;
end;

function MaxFloatArray(const B: TDynFloatArray): Float;
var
  I, N: Integer;
begin
  N := GetDynLengthNotNull(B);
  Result := B[0];
  for I := 1 to N - 1 do
    if B[I] > Result then
      Result := B[I];
end;

function MaxFloatArrayIndex(const B: TDynFloatArray): Integer;
var
  I, N: Integer;
  Max: Float;
begin
  Result := 0;
  N := GetDynLengthNotNull(B);
  Max := B[0];
  for I := 1 to N - 1 do
    if B[I] > Max then
    begin
      Max := B[I];
      Result := I;
    end;
end;

// The FloatArray X must be presorted so Median can calculate the correct value.
//            Y_{(n+1)/2}                     if N is odd
// Median = { 1/2 * (Y_{n/2} + Y_{1+(n/2) }   if N is even

function Median(const X: TDynFloatArray): Float;
var
  N: Integer;
begin
  N := GetSampleSize(X);
  if N = 1 then
    Result := X[0]
  else
  if Odd(N) then
    Result := X[N div 2]
  else
    Result := (X[N div 2 - 1] + X[N div 2]) / 2;
end;

{$IFNDEF CLR}
function MedianUnsorted(const X: TDynFloatArray): Float;
var
  SortedList: TDynFloatArray;

begin
  // We need to sort the values first
  SortedList := Copy(X);
  // type cast to Pointer for the sake of FPC
  SortDynArray(Pointer(SortedList), SizeOf(Float),DynArrayCompareFloat);

  // and call the median function afterwards
  Result := Median(SortedList);
end;
{$ENDIF ~CLR}

function MinFloatArray(const B: TDynFloatArray): Float;
var
  I, N: Integer;
begin
  N := GetDynLengthNotNull(B);
  Result := B[0];
  for I := 1 to N - 1 do
    if B[I] < Result then
      Result := B[I];
end;

function MinFloatArrayIndex(const B: TDynFloatArray): Integer;
var
  I, N: Integer;
  Min: Float;
begin
  Result := 0;
  N := GetDynLengthNotNull(B);
  Min := B[0];
  for I := 1 to N - 1 do
    if B[I] < Min then
    begin
      Min := B[I];
      Result := I;
    end;
end;

function Permutation(N, R: Cardinal): Float;
var
  I : Integer;
begin
  if (N = 0) or (R > N) or (N > MaxFactorial) then
  begin
    Result := 0.0;
    Exit;
  end;
  Result := 1.0;
  if R <> 0 then
    try
      for I := N downto N - R + 1 do
        Result := Result * I;
      Result := Int(Result + 0.5);
    except
      Result := -1.0;
    end;
end;

{ TODO -cDoc : Donator: Fred Hovey }
function Combinations(N, R: Cardinal): Float;
begin
  Result := Factorial(R);
  if IsFloatZero(Result) then
   Result := -1.0
  else
   Result := Permutation(N, R) / Result;
end;

{ TODO -cDoc : donator: Fred Hovey, contributor: Robert Rossmair }
function SumOfSquares(const X: TDynFloatArray): Float;
var
  I, N: Integer;
  Sum: Float;
begin
  N := GetSampleSize(X);
  Result := Sqr(X[0]);
  Sum := X[0];
  for I := 1 to N - 1 do
  begin
    Result := Result + Sqr(X[I]);
    Sum := Sum + X[I];
  end;
  Result := Result - Sum * Sum / N;
end;

{ TODO -cDoc : Contributors: Fred Hovey, Robert Rossmair }
function PopulationVariance(const X: TDynFloatArray): Float;
begin
  // Length(X) = 0 would cause SumOfSquares() to raise an exception before the division is executed.
  Result := SumOfSquares(X) / Length(X);
end;

procedure PopulationVarianceAndMean(const X: TDynFloatArray; var Variance, Mean: Float);
var
  I, N: Integer;
  Sum, SumSq: Float;
begin
  N := GetSampleSize(X);
  SumSq := Sqr(X[0]);
  Sum := X[0];
  for I := 1 to N - 1 do
  begin
    SumSq := SumSq + Sqr(X[I]);
    Sum := Sum + X[I];
  end;
  Mean := Sum / N;
  Variance := (SumSq / N) - Sqr(Mean);
end;

{ TODO -cDoc : Contributors: Fred Hovey, Robert Rossmair }
function SampleVariance(const X: TDynFloatArray): Float;
var
  N: Integer;
begin
  N := GetSampleSize(X, 2);
  Result := SumOfSquares(X) / (N - 1)
end;

{ TODO -cDoc : Contributors: Fred Hovey, Robert Rossmair }
procedure SampleVarianceAndMean(const X: TDynFloatArray; var Variance, Mean: Float);
var
  I, N: Integer;
  Sum, SumSq: Float;
begin
  N := GetSampleSize(X);
  SumSq := Sqr(X[0]);
  Sum := X[0];
  for I := 1 to N - 1 do
  begin
    SumSq := SumSq + Sqr(X[I]);
    Sum := Sum + X[I];
  end;
  Mean := Sum / N;
  if N < 2 then
    InvalidSampleSize(N);
  //Variance := (SumSq / (N - 1)) - Sqr(Sum / (N - 1)) => WRONG!!!!
  Variance := (SumSq - Sum * Sum / N) / (N - 1)
end;

{ TODO -cDoc : Donator: Fred Hovey, contributor: Robert Rossmair }
function StdError(const X: TDynFloatArray): Float;
begin
  // Length(X) = 0 would cause SampleVariance() to raise an exception before the division is
  // executed.
  Result := Sqrt(SampleVariance(X) / Length(X));
end;

{ TODO -cDoc : Donator: Fred Hovey, contributor: Robert Rossmair }
function StdError(const Variance: Float; const SampleSize: Integer): Float;
begin
  if SampleSize = 0 then
    InvalidSampleSize(SampleSize);
  Result := Sqrt(Variance / SampleSize);
end;

function SumFloatArray(const B: TDynFloatArray): Float;
var
  I, N: Integer;
begin
  Result := 0.0;
  N := GetDynLength(B);
  if N <> 0 then
  begin
    Result := B[0];
    for I := 1 to N - 1 do
      Result := Result + B[I];
  end;
end;

function SumSquareDiffFloatArray(const B: TDynFloatArray; Diff: Float): Float;
var
  I, N: Integer;
begin
  Result := 0.0;
  N := GetDynLength(B);
  if N <> 0 then
  begin
    Result := Sqr(B[0] - Diff);
    for I := 1 to N - 1 do
      Result := Result + Sqr(B[I] - Diff);
  end;
end;

function SumSquareFloatArray(const B: TDynFloatArray): Float;
var
  I, N: Integer;
begin
  Result := 0.0;
  N := GetDynLength(B);
  if N <> 0 then
  begin
    Result := Sqr(B[0]);
    for I := 1 to N - 1 do
      Result := Result + Sqr(B[I]);
  end;
end;

function SumPairProductFloatArray(const X, Y: TDynFloatArray): Float;
var
  I, N: Integer;
begin
  Result := 0.0;
  N := Min(Length(X), Length(Y));
  if N <> 0 then
  begin
    Result := X[0] * Y[0];
    for I := 1 to N - 1 do
      Result := Result + X[I] * Y[I];
  end;
end;

function ChiSquare(const X: TDynFloatArray): Float;  { TODO -cDoc : ChiSquare }
var
  I, N: Integer;
  Sum: Float;
begin
  N := GetDynLengthNotNull(X);
  Result := Sqr(X[0]);
  Sum := X[0];
  for I := 1 to N - 1 do
  begin
    Result := Result + Sqr(X[I]);
    Sum := Sum + X[I];
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
