{*
===============================================================================
The original notice of the softfloat package is shown below. The conversion
to pascal was done by Carl Eric Codere in 2002 (ccodere@ieee.org).
===============================================================================

This C source file is part of the SoftFloat IEC/IEEE Floating-Point
Arithmetic Package, Release 2a.

Written by John R. Hauser.  This work was made possible in part by the
International Computer Science Institute, located at Suite 600, 1947 Center
Street, Berkeley, California 94704.  Funding was partially provided by the
National Science Foundation under grant MIP-9311980.  The original version
of this code was written as part of a project to build a fixed-point vector
processor in collaboration with the University of California at Berkeley,
overseen by Profs. Nelson Morgan and John Wawrzynek.  More information
is available through the Web page
`http://HTTP.CS.Berkeley.EDU/~jhauser/arithmetic/SoftFloat.html'.

THIS SOFTWARE IS DISTRIBUTED AS IS, FOR FREE.  Although reasonable effort
has been made to avoid it, THIS SOFTWARE MAY CONTAIN FAULTS THAT WILL AT
TIMES RESULT IN INCORRECT BEHAVIOR.  USE OF THIS SOFTWARE IS RESTRICTED TO
PERSONS AND ORGANIZATIONS WHO CAN AND WILL TAKE FULL RESPONSIBILITY FOR ANY
AND ALL LOSSES, COSTS, OR OTHER PROBLEMS ARISING FROM ITS USE.

Derivative works are acceptable, even for commercial purposes, so long as
(1) they include prominent notice that the work is derivative, and (2) they
include prominent notice akin to these four paragraphs for those parts of
this code that are retained.

===============================================================================

The float80 and float128 part is translated from the softfloat package
by Florian Klaempfl and contained the following copyright notice

The code might contain some duplicate stuff because the floatx80/float128 port was
done based on the 64 bit enabled softfloat code.

===============================================================================

This C source file is part of the SoftFloat IEC/IEEE Floating-point Arithmetic
Package, Release 2b.

Written by John R. Hauser.  This work was made possible in part by the
International Computer Science Institute, located at Suite 600, 1947 Center
Street, Berkeley, California 94704.  Funding was partially provided by the
National Science Foundation under grant MIP-9311980.  The original version
of this code was written as part of a project to build a fixed-point vector
processor in collaboration with the University of California at Berkeley,
overseen by Profs. Nelson Morgan and John Wawrzynek.  More information
is available through the Web page `http://www.cs.berkeley.edu/~jhauser/
arithmetic/SoftFloat.html'.

THIS SOFTWARE IS DISTRIBUTED AS IS, FOR FREE.  Although reasonable effort has
been made to avoid it, THIS SOFTWARE MAY CONTAIN FAULTS THAT WILL AT TIMES
RESULT IN INCORRECT BEHAVIOR.  USE OF THIS SOFTWARE IS RESTRICTED TO PERSONS
AND ORGANIZATIONS WHO CAN AND WILL TAKE FULL RESPONSIBILITY FOR ALL LOSSES,
COSTS, OR OTHER PROBLEMS THEY INCUR DUE TO THE SOFTWARE, AND WHO FURTHERMORE
EFFECTIVELY INDEMNIFY JOHN HAUSER AND THE INTERNATIONAL COMPUTER SCIENCE
INSTITUTE (possibly via similar legal warning) AGAINST ALL LOSSES, COSTS, OR
OTHER PROBLEMS INCURRED BY THEIR CUSTOMERS AND CLIENTS DUE TO THE SOFTWARE.

Derivative works are acceptable, even for commercial purposes, so long as
(1) the source code for the derivative work includes prominent notice that
the work is derivative, and (2) the source code includes prominent notice with
these four paragraphs for those parts of this code that are retained.


===============================================================================
*}

{$define FPC_SOFTFLOAT_FLOATX80}
{$define FPC_SOFTFLOAT_FLOAT128}

{ the softfpu unit can be also embedded directly into the system unit }

{$if not(defined(fpc_softfpu_interface)) and not(defined(fpc_softfpu_implementation))}

{$mode objfpc}
unit sfloat;

{ Overflow checking must be disabled,
  since some operations expect overflow!
}
{$Q-}
{$goto on}

interface
{$endif not(defined(fpc_softfpu_interface)) and not(defined(fpc_softfpu_implementation))}

{$if not(defined(fpc_softfpu_implementation))}
{
-------------------------------------------------------------------------------
Software IEC/IEEE floating-point types.
-------------------------------------------------------------------------------
}
TYPE
  float32 = longword;
  { we use here a record in the function header because
    the record allows bitwise conversion to single }
  float32rec = record
    float32 : float32;
  end;

  flag = byte;
  uint8 = byte;
  int8 = shortint;
  uint16 = word;
  int16 = smallint;
  uint32 = longword;
  int32 = longint;

  bits8 = byte;
  sbits8 = shortint;
  bits16 = word;
  sbits16 = smallint;
  sbits32 = longint;
  bits32 = longword;
{$ifndef fpc}
  qword = int64;
{$endif}
  { now part of the system unit
  uint64 = qword;
  }
  bits64 = qword;
  sbits64 = int64;

{$ifdef ENDIAN_LITTLE}
  float64 = packed record
    low: bits32;
    high: bits32;
  end;

  int64rec = packed record
    low: bits32;
    high: bits32;
  end;

  floatx80 = packed record
    low : qword;
    high : word;
  end;

  float128 = packed record
    low : qword;
    high : qword;
  end;
{$else}
  float64 = packed record
    high,low : bits32;
  end;

  int64rec = packed record
    high,low : bits32;
  end;

  floatx80 = packed record
    high : word;
    low : qword;
  end;

  float128 = packed record
    high : qword;
    low : qword;
  end;
{$endif}


function floatx80_to_float32(a: floatx80): float32;

{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is less than
the corresponding value `b', and 0 otherwise.  The comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_lt(a: float64;b: float64): flag; compilerproc;
{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is less than
or equal to the corresponding value `b', and 0 otherwise.  The comparison
is performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_le(a: float64;b: float64): flag; compilerproc;
{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is equal to
the corresponding value `b', and 0 otherwise.  The comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_eq(a: float64;b: float64): flag; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the square root of the double-precision floating-point value `a'.
The operation is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Procedure float64_sqrt( a: float64; var out: float64 ); compilerproc;
{*
-------------------------------------------------------------------------------
Returns the remainder of the double-precision floating-point value `a'
with respect to the corresponding value `b'.  The operation is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_rem(a: float64; b : float64) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of dividing the double-precision floating-point value `a'
by the corresponding value `b'.  The operation is performed according to the
IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_div(a: float64; b : float64) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of multiplying the double-precision floating-point values
`a' and `b'.  The operation is performed according to the IEC/IEEE Standard
for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_mul( a: float64; b:float64) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of subtracting the double-precision floating-point values
`a' and `b'.  The operation is performed according to the IEC/IEEE Standard
for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_sub(a: float64; b : float64) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of adding the double-precision floating-point values `a'
and `b'.  The operation is performed according to the IEC/IEEE Standard for
Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_add( a: float64; b : float64) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Rounds the double-precision floating-point value `a' to an integer,
and returns the result as a double-precision floating-point value.  The
operation is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_round_to_int(a: float64) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the double-precision floating-point value
`a' to the single-precision floating-point format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_to_float32(a: float64) : float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the double-precision floating-point value
`a' to the 32-bit two's complement integer format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic, except that the conversion is always rounded toward zero.
If `a' is a NaN, the largest positive integer is returned.  Otherwise, if
the conversion overflows, the largest integer with the same sign as `a' is
returned.
-------------------------------------------------------------------------------
*}
Function float64_to_int32_round_to_zero(a: float64 ): int32; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the double-precision floating-point value
`a' to the 32-bit two's complement integer format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic---which means in particular that the conversion is rounded
according to the current rounding mode.  If `a' is a NaN, the largest
positive integer is returned.  Otherwise, if the conversion overflows, the
largest integer with the same sign as `a' is returned.
-------------------------------------------------------------------------------
*}
Function float64_to_int32(a: float64): int32; compilerproc;
{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is less than
the corresponding value `b', and 0 otherwise.  The comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_lt( a:float32rec ; b : float32rec): flag; compilerproc;
{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is less than
or equal to the corresponding value `b', and 0 otherwise.  The comparison
is performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_le( a: float32rec; b : float32rec ):flag; compilerproc;
{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is equal to
the corresponding value `b', and 0 otherwise.  The comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_eq( a:float32rec; b:float32rec): flag; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the square root of the single-precision floating-point value `a'.
The operation is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_sqrt(a: float32rec ): float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the remainder of the single-precision floating-point value `a'
with respect to the corresponding value `b'.  The operation is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_rem(a: float32rec; b: float32rec ):float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of dividing the single-precision floating-point value `a'
by the corresponding value `b'.  The operation is performed according to the
IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_div(a: float32rec;b: float32rec ): float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of multiplying the single-precision floating-point values
`a' and `b'.  The operation is performed according to the IEC/IEEE Standard
for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_mul(a: float32rec; b: float32rec ) : float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of subtracting the single-precision floating-point values
`a' and `b'.  The operation is performed according to the IEC/IEEE Standard
for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_sub( a: float32rec ; b:float32rec ): float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of adding the single-precision floating-point values `a'
and `b'.  The operation is performed according to the IEC/IEEE Standard for
Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_add( a: float32rec; b:float32rec ): float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Rounds the single-precision floating-point value `a' to an integer,
and returns the result as a single-precision floating-point value.  The
operation is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_round_to_int( a: float32rec): float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the single-precision floating-point value
`a' to the double-precision floating-point format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_to_float64( a : float32rec) : Float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the single-precision floating-point value
`a' to the 32-bit two's complement integer format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic, except that the conversion is always rounded toward zero.
If `a' is a NaN, the largest positive integer is returned.  Otherwise, if
the conversion overflows, the largest integer with the same sign as `a' is
returned.
-------------------------------------------------------------------------------
*}
Function float32_to_int32_round_to_zero( a: Float32rec ): int32; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the single-precision floating-point value
`a' to the 32-bit two's complement integer format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic---which means in particular that the conversion is rounded
according to the current rounding mode.  If `a' is a NaN, the largest
positive integer is returned.  Otherwise, if the conversion overflows, the
largest integer with the same sign as `a' is returned.
-------------------------------------------------------------------------------
*}
Function float32_to_int32( a : float32rec) : int32; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the 32-bit two's complement integer `a' to
the double-precision floating-point format.  The conversion is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function int32_to_float64( a: int32) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the 32-bit two's complement integer `a' to
the single-precision floating-point format.  The conversion is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function int32_to_float32( a: int32): float32rec; compilerproc;

{*----------------------------------------------------------------------------
| Returns the result of converting the 64-bit two's complement integer `a'
| to the double-precision floating-point format.  The conversion is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}
Function int64_to_float64( a: int64 ): float64; compilerproc;

{*----------------------------------------------------------------------------
| Returns the result of converting the 64-bit two's complement integer `a'
| to the single-precision floating-point format.  The conversion is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}
Function int64_to_float32( a: int64 ): float32rec; compilerproc;


{$ifdef FPC_SOFTFLOAT_FLOAT128}
function float128_is_nan( a : float128): flag;
function float128_is_signaling_nan( a : float128): flag;
function float128_to_int32(a: float128): int32;
function float128_to_int32_round_to_zero(a: float128): int32;
function float128_to_int64(a: float128): int64;
function float128_to_int64_round_to_zero(a: float128): int64;
function float128_to_float32(a: float128): float32;
function float128_to_float64(a: float128): float64;
function float64_to_float128( a : float64) : float128;
{$ifdef FPC_SOFTFLOAT_FLOAT80}
function float128_to_floatx80(a: float128): floatx80;
{$endif FPC_SOFTFLOAT_FLOAT80}
function float128_round_to_int(a: float128): float128;
function float128_add(a: float128; b: float128): float128;
function float128_sub(a: float128; b: float128): float128;
function float128_mul(a: float128; b: float128): float128;
function float128_div(a: float128; b: float128): float128;
function float128_rem(a: float128; b: float128): float128;
function float128_sqrt(a: float128): float128;
function float128_eq(a: float128; b: float128): flag;
function float128_le(a: float128; b: float128): flag;
function float128_lt(a: float128; b: float128): flag;
function float128_eq_signaling(a: float128; b: float128): flag;
function float128_le_quiet(a: float128; b: float128): flag;
function float128_lt_quiet(a: float128; b: float128): flag;
{$endif FPC_SOFTFLOAT_FLOAT128}

CONST
{-------------------------------------------------------------------------------
Software IEC/IEEE floating-point underflow tininess-detection mode.
-------------------------------------------------------------------------------
*}
    float_tininess_after_rounding  = 0;
    float_tininess_before_rounding = 1;

{*
-------------------------------------------------------------------------------
Underflow tininess-detection mode, statically initialized to default value.
(The declaration in `softfloat.h' must match the `int8' type here.)
-------------------------------------------------------------------------------
*}

const float_detect_tininess: int8 = float_tininess_after_rounding;

{$endif  not(defined(fpc_softfpu_implementation))}

{$if not(defined(fpc_softfpu_interface)) and not(defined(fpc_softfpu_implementation))}
implementation
{$endif not(defined(fpc_softfpu_interface)) and not(defined(fpc_softfpu_implementation))}


{$if not(defined(fpc_softfpu_interface))}
{
*****************************************************************************
*----------------------------------------------------------------------------*
* Primitive arithmetic functions, including multi-word arithmetic, and       *
* division and square root approximations.  (Can be specialized to target if *
* desired.)                                                                  *
* ---------------------------------------------------------------------------*
*****************************************************************************

*----------------------------------------------------------------------------
| Takes a 64-bit fixed-point value `absZ' with binary point between bits 6
| and 7, and returns the properly rounded 32-bit integer corresponding to the
| input.  If `zSign' is 1, the input is negated before being converted to an
| integer.  Bit 63 of `absZ' must be zero.  Ordinarily, the fixed-point input
| is simply rounded to an integer, with the inexact exception raised if the
| input cannot be represented exactly as an integer.  However, if the fixed-
| point input is too large, the invalid exception is raised and the largest
| positive or negative integer is returned.
*----------------------------------------------------------------------------* }

function roundAndPackInt32( zSign: flag; absZ : bits64): int32;
var
    roundingMode: int8;
    roundNearestEven: flag;
    roundIncrement, roundBits: int8;
    z: int32;
begin
    roundingMode := softfloat_rounding_mode;
    roundNearestEven := ord( roundingMode = float_round_nearest_even );
    roundIncrement := $40;
    if ( roundNearestEven=0 ) then
    begin
        if ( roundingMode = float_round_to_zero ) then
        begin
            roundIncrement := 0;
        end
        else begin
            roundIncrement := $7F;
            if ( zSign<>0 ) then
            begin
                if ( roundingMode = float_round_up ) then
                  roundIncrement := 0;
            end
            else begin
                if ( roundingMode = float_round_down ) then
                  roundIncrement := 0;
            end;
        end;
    end;
    roundBits := absZ and $7F;
    absZ := ( absZ + roundIncrement ) shr 7;
    absZ := absZ and not( ord( ( roundBits xor  $40 ) = 0 ) and roundNearestEven );
    z := absZ;
    if ( zSign<>0 ) then
      z := - z;
    if ( ( absZ shr 32 ) or ( z and ( ord( z < 0 ) xor  zSign ) ) )<>0 then
    begin
        float_raise( float_flag_invalid );
        if zSign<>0 then
          result:=sbits32($80000000)
        else
          result:=$7FFFFFFF;
        exit;
    end;
    if ( roundBits<>0 ) then
      softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
    result:=z;
end;

{----------------------------------------------------------------------------
| Takes the 128-bit fixed-point value formed by concatenating `absZ0' and
| `absZ1', with binary point between bits 63 and 64 (between the input words),
| and returns the properly rounded 64-bit integer corresponding to the input.
| If `zSign' is 1, the input is negated before being converted to an integer.
| Ordinarily, the fixed-point input is simply rounded to an integer, with
| the inexact exception raised if the input cannot be represented exactly as
| an integer.  However, if the fixed-point input is too large, the invalid
| exception is raised and the largest positive or negative integer is
| returned.
*----------------------------------------------------------------------------}

function roundAndPackInt64( zSign: flag; absZ0: bits64; absZ1 : bits64): int64;
var
    roundingMode: int8;
    roundNearestEven, increment: flag;
    z: int64;
label
    overflow;
begin
    roundingMode := softfloat_rounding_mode;
    roundNearestEven := ord( roundingMode = float_round_nearest_even );
    increment := ord( sbits64(absZ1) < 0 );
    if ( roundNearestEven=0 ) then
    begin
        if ( roundingMode = float_round_to_zero ) then
        begin
            increment := 0;
        end
        else begin
            if ( zSign<>0 ) then
            begin
                increment := ord(( roundingMode = float_round_down ) and (absZ1<>0));
            end
            else begin
                increment := ord(( roundingMode = float_round_up ) and (absZ1<>0));
            end;
        end;
    end;
    if ( increment<>0 ) then
    begin
        inc(absZ0);
        if ( absZ0 = 0 ) then
          goto overflow;
        absZ0 := absZ0 and not( ord( bits64( absZ1 shl 1 ) = 0 ) and roundNearestEven );
    end;
    z := absZ0;
    if ( zSign<>0 ) then
      z := - z;
    if ( (z<>0) and (( ord( z < 0 ) xor  zSign )<>0) ) then
    begin
 overflow:
        float_raise( float_flag_invalid );
        if zSign<>0 then
          result:=int64($8000000000000000)
        else
          result:=int64($7FFFFFFFFFFFFFFF);
    end;
    if ( absZ1<>0 ) then
      softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
    result:=z;
end;

{
-------------------------------------------------------------------------------
Shifts `a' right by the number of bits given in `count'.  If any nonzero
bits are shifted off, they are ``jammed'' into the least significant bit of
the result by setting the least significant bit to 1.  The value of `count'
can be arbitrarily large; in particular, if `count' is greater than 32, the
result will be either 0 or 1, depending on whether `a' is zero or nonzero.
The result is stored in the location pointed to by `zPtr'.
-------------------------------------------------------------------------------
}
Procedure shift32RightJamming( a: bits32 ; count: int16 ; VAR zPtr :bits32);
var
  z: Bits32;
Begin
    if ( count = 0 ) then
        z := a
   else
    if ( count < 32 ) then
    Begin
        z := ( a shr count ) or bits32( (( a shl ( ( - count ) AND 31 )) ) <> 0);
    End
   else
    Begin
        z := bits32( a <> 0 );
    End;
    zPtr := z;
End;

{----------------------------------------------------------------------------
| Shifts the 128-bit value formed by concatenating `a0' and `a1' right by the
| number of bits given in `count'.  Any bits shifted off are lost.  The value
| of `count' can be arbitrarily large; in particular, if `count' is greater
| than 128, the result will be 0.  The result is broken into two 64-bit pieces
| which are stored at the locations pointed to by `z0Ptr' and `z1Ptr'.
*----------------------------------------------------------------------------}

procedure shift128Right(a0: bits64; a1: bits64; count: int16; var z0Ptr: bits64; z1Ptr : bits64);
var
    z0, z1: bits64;
    negCount: int8;
begin
    negCount := ( - count ) and 63;

    if ( count = 0 ) then
    begin
        z1 := a1;
        z0 := a0;
    end
    else if ( count < 64 ) then
    begin
        z1 := ( a0 shl negCount ) or ( a1 shr count );
        z0 := a0 shr count;
    end
    else
    begin
    	  if ( count shl 64 )<>0 then
          z1 := a0 shr ( count and 63 )
        else
          z1 := 0;
        z0 := 0;
    end;
    z1Ptr := z1;
    z0Ptr := z0;
end;


{----------------------------------------------------------------------------
| Shifts the 128-bit value formed by concatenating `a0' and `a1' right by the
| number of bits given in `count'.  If any nonzero bits are shifted off, they
| are ``jammed'' into the least significant bit of the result by setting the
| least significant bit to 1.  The value of `count' can be arbitrarily large;
| in particular, if `count' is greater than 128, the result will be either
| 0 or 1, depending on whether the concatenation of `a0' and `a1' is zero or
| nonzero.  The result is broken into two 64-bit pieces which are stored at
| the locations pointed to by `z0Ptr' and `z1Ptr'.
*----------------------------------------------------------------------------}

procedure shift128RightJamming(a0,a1 : bits64; count : int16; var z0Ptr, z1Ptr : bits64);
var
    z0,z1 : bits64;
    negCount : int8;
begin
    negCount := ( - count ) and 63;

    if ( count = 0 ) then begin
        z1 := a1;
        z0 := a0;
    end
    else if ( count < 64 ) then begin
        z1 := ( a0 shl negCount ) or ( a1 shr count ) or ord( ( a1 shl negCount ) <> 0 );
        z0 := a0>>count;
    end
    else begin
        if ( count = 64 ) then begin
            z1 := a0 or ord( a1 <> 0 );
        end
        else if ( count < 128 ) then begin
            z1 := ( a0 shr ( count and 63 ) ) or ord( ( ( a0 shl negCount ) or a1 ) <> 0 );
        end
        else begin
            z1 := ord( ( a0 or a1 ) <> 0 );
        end;
        z0 := 0;
    end;
    z1Ptr := z1;
    z0Ptr := z0;
end;

{
-------------------------------------------------------------------------------
Shifts the 64-bit value formed by concatenating `a0' and `a1' right by the
number of bits given in `count'.  Any bits shifted off are lost.  The value
of `count' can be arbitrarily large; in particular, if `count' is greater
than 64, the result will be 0.  The result is broken into two 32-bit pieces
which are stored at the locations pointed to by `z0Ptr' and `z1Ptr'.
-------------------------------------------------------------------------------
}
Procedure
 shift64Right(
     a0 :bits32; a1: bits32; count:int16; VAR z0Ptr:bits32; VAR z1Ptr:bits32);
Var
  z0, z1: bits32;
  negCount : int8;
Begin
    negCount := ( - count ) AND 31;

    if ( count = 0 ) then
    Begin
        z1 := a1;
        z0 := a0;
    End
    else if ( count < 32 ) then
    Begin
        z1 := ( a0 shl negCount ) OR ( a1 shr count );
        z0 := a0 shr count;
    End
   else
    Begin
        if (count < 64) then
          z1 := ( a0 shr ( count AND 31 ) )
        else
          z1 := 0;
        z0 := 0;
    End;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{
-------------------------------------------------------------------------------
Shifts the 64-bit value formed by concatenating `a0' and `a1' right by the
number of bits given in `count'.  If any nonzero bits are shifted off, they
are ``jammed'' into the least significant bit of the result by setting the
least significant bit to 1.  The value of `count' can be arbitrarily large;
in particular, if `count' is greater than 64, the result will be either 0
or 1, depending on whether the concatenation of `a0' and `a1' is zero or
nonzero.  The result is broken into two 32-bit pieces which are stored at
the locations pointed to by `z0Ptr' and `z1Ptr'.
-------------------------------------------------------------------------------
}
Procedure
 shift64RightJamming(
     a0:bits32; a1: bits32; count:int16; VAR Z0Ptr :bits32;VAR z1Ptr: bits32 );
VAR
    z0, z1 : bits32;
    negCount : int8;
Begin
    negCount := ( - count ) AND 31;

    if ( count = 0 ) then
    Begin
        z1 := a1;
        z0 := a0;
    End
   else
    if ( count < 32 ) then
    Begin
        z1 := ( a0 shl negCount ) OR ( a1 shr count ) OR bits32( ( a1 shl negCount ) <> 0 );
        z0 := a0 shr count;
    End
   else
    Begin
        if ( count = 32 ) then
        Begin
            z1 := a0 OR bits32( a1 <> 0 );
        End
       else
        if ( count < 64 ) Then
        Begin
            z1 := ( a0 shr ( count AND 31 ) ) OR bits32( ( ( a0 shl negCount ) OR a1 ) <> 0 );
        End
       else
        Begin
            z1 := bits32( ( a0 OR a1 ) <> 0 );
        End;
        z0 := 0;
    End;
    z1Ptr := z1;
    z0Ptr := z0;
End;


{*----------------------------------------------------------------------------
| Shifts `a' right by the number of bits given in `count'.  If any nonzero
| bits are shifted off, they are ``jammed'' into the least significant bit of
| the result by setting the least significant bit to 1.  The value of `count'
| can be arbitrarily large; in particular, if `count' is greater than 64, the
| result will be either 0 or 1, depending on whether `a' is zero or nonzero.
| The result is stored in the location pointed to by `zPtr'.
*----------------------------------------------------------------------------*}

procedure shift64RightJamming(a: bits64; count: int16; var zPtr : bits64);
var
    z: bits64;
begin
    if ( count = 0 ) then
    begin
        z := a;
    end
    else if ( count < 64 ) then
    begin
        z := ( a shr count ) or ord( ( a  shl ( ( - count ) and 63 ) ) <> 0 );
    end
    else
    begin
        z := ord( a <> 0 );
    end;
    zPtr := z;
end;



{*
-------------------------------------------------------------------------------
Shifts the 96-bit value formed by concatenating `a0', `a1', and `a2' right
by 32 _plus_ the number of bits given in `count'.  The shifted result is
at most 64 nonzero bits; these are broken into two 32-bit pieces which are
stored at the locations pointed to by `z0Ptr' and `z1Ptr'.  The bits shifted
off form a third 32-bit result as follows:  The _last_ bit shifted off is
the most-significant bit of the extra result, and the other 31 bits of the
extra result are all zero if and only if _all_but_the_last_ bits shifted off
were all zero.  This extra result is stored in the location pointed to by
`z2Ptr'.  The value of `count' can be arbitrarily large.
    (This routine makes more sense if `a0', `a1', and `a2' are considered
to form a fixed-point value with binary point between `a1' and `a2'.  This
fixed-point value is shifted right by the number of bits given in `count',
and the integer part of the result is returned at the locations pointed to
by `z0Ptr' and `z1Ptr'.  The fractional part of the result may be slightly
corrupted as described above, and is returned at the location pointed to by
`z2Ptr'.)
-------------------------------------------------------------------------------
}
Procedure
 shift64ExtraRightJamming(
     a0: bits32;
     a1: bits32;
     a2: bits32;
     count: int16;
     VAR z0Ptr: bits32;
     VAR z1Ptr: bits32;
     VAR z2Ptr: bits32
 );
Var
    z0, z1, z2: bits32;
    negCount : int8;
Begin
    negCount := ( - count ) AND 31;

    if ( count = 0 ) then
    Begin
        z2 := a2;
        z1 := a1;
        z0 := a0;
    End
   else
    Begin
        if ( count < 32 ) Then
        Begin
            z2 := a1 shl negCount;
            z1 := ( a0 shl negCount ) OR ( a1 shr count );
            z0 := a0 shr count;
        End
       else
        Begin
            if ( count = 32 ) then
            Begin
                z2 := a1;
                z1 := a0;
            End
           else
            Begin
                a2 := a2 or a1;
                if ( count < 64 ) then
                Begin
                    z2 := a0 shl negCount;
                    z1 := a0 shr ( count AND 31 );
                End
               else
                Begin
                    if count = 64 then
                       z2 := a0
                    else
                       z2 := bits32(a0 <> 0);
                    z1 := 0;
                End;
            End;
            z0 := 0;
        End;
        z2 := z2 or bits32( a2 <> 0 );
    End;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*
-------------------------------------------------------------------------------
Shifts the 64-bit value formed by concatenating `a0' and `a1' left by the
number of bits given in `count'.  Any bits shifted off are lost.  The value
of `count' must be less than 32.  The result is broken into two 32-bit
pieces which are stored at the locations pointed to by `z0Ptr' and `z1Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 shortShift64Left(
     a0:bits32; a1:bits32; count:int16; VAR z0Ptr:bits32; VAR z1Ptr:bits32 );
Begin

    z1Ptr := a1 shl count;
    if count = 0 then
      z0Ptr := a0
    else
      z0Ptr := ( a0 shl count ) OR ( a1 shr ( ( - count ) AND 31 ) );
End;

{*
-------------------------------------------------------------------------------
Shifts the 96-bit value formed by concatenating `a0', `a1', and `a2' left
by the number of bits given in `count'.  Any bits shifted off are lost.
The value of `count' must be less than 32.  The result is broken into three
32-bit pieces which are stored at the locations pointed to by `z0Ptr',
`z1Ptr', and `z2Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 shortShift96Left(
     a0: bits32;
     a1: bits32;
     a2: bits32;
     count: int16;
     VAR z0Ptr: bits32;
     VAR z1Ptr: bits32;
     VAR z2Ptr: bits32
 );
Var
    z0, z1, z2: bits32;
    negCount: int8;
Begin
    z2 := a2 shl count;
    z1 := a1 shl count;
    z0 := a0 shl count;
    if ( 0 < count ) then
    Begin
        negCount := ( ( - count ) AND 31 );
        z1 := z1 or (a2 shr negCount);
        z0 := z0 or (a1 shr negCount);
    End;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*----------------------------------------------------------------------------
| Shifts the 128-bit value formed by concatenating `a0' and `a1' left by the
| number of bits given in `count'.  Any bits shifted off are lost.  The value
| of `count' must be less than 64.  The result is broken into two 64-bit
| pieces which are stored at the locations pointed to by `z0Ptr' and `z1Ptr'.
*----------------------------------------------------------------------------*}

procedure shortShift128Left(a0: bits64; a1: bits64; count: int16; var z0Ptr: bits64; z1Ptr : bits64);
begin
    z1Ptr := a1 shl count;
    if count=0 then
      z0Ptr:=a0
    else
      z0Ptr:=( a0 shl count ) or ( a1 shr ( ( - count ) and 63 ) );
end;

{*
-------------------------------------------------------------------------------
Adds the 64-bit value formed by concatenating `a0' and `a1' to the 64-bit
value formed by concatenating `b0' and `b1'.  Addition is modulo 2^64, so
any carry out is lost.  The result is broken into two 32-bit pieces which
are stored at the locations pointed to by `z0Ptr' and `z1Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 add64(
     a0:bits32; a1:bits32; b0:bits32; b1:bits32; VAR z0Ptr:bits32; VAR z1Ptr:bits32 );
Var
    z1: bits32;
Begin
    z1 := a1 + b1;
    z1Ptr := z1;
    z0Ptr := a0 + b0 + bits32( z1 < a1 );
End;

{*
-------------------------------------------------------------------------------
Adds the 96-bit value formed by concatenating `a0', `a1', and `a2' to the
96-bit value formed by concatenating `b0', `b1', and `b2'.  Addition is
modulo 2^96, so any carry out is lost.  The result is broken into three
32-bit pieces which are stored at the locations pointed to by `z0Ptr',
`z1Ptr', and `z2Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 add96(
     a0: bits32;
     a1: bits32;
     a2: bits32;
     b0: bits32;
     b1: bits32;
     b2: bits32;
     VAR z0Ptr: bits32;
     VAR z1Ptr: bits32;
     VAR z2Ptr: bits32
 );
var
    z0, z1, z2: bits32;
    carry0, carry1: int8;
Begin
    z2 := a2 + b2;
    carry1 := int8( z2 < a2 );
    z1 := a1 + b1;
    carry0 := int8( z1 < a1 );
    z0 := a0 + b0;
    z1 := z1 + carry1;
    z0 := z0 + bits32( z1 < carry1 );
    z0 := z0 + carry0;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*----------------------------------------------------------------------------
| Shifts the 192-bit value formed by concatenating `a0', `a1', and `a2' left
| by the number of bits given in `count'.  Any bits shifted off are lost.
| The value of `count' must be less than 64.  The result is broken into three
| 64-bit pieces which are stored at the locations pointed to by `z0Ptr',
| `z1Ptr', and `z2Ptr'.
*----------------------------------------------------------------------------*}

procedure shortShift192Left(a0,a1,a2 : bits64;count : int16;var z0Ptr,z1Ptr,z2Ptr : bits64);
var
    z0, z1, z2 : bits64;
    negCount : int8;
begin
    z2 := a2 shl count;
    z1 := a1 shl count;
    z0 := a0 shl count;
    if ( 0 < count ) then
    begin
        negCount := ( ( - count ) and 63 );
        z1 := z1 or (a2 shr negCount);
        z0 := z0 or (a1 shr negCount);
    end;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
end;

{*----------------------------------------------------------------------------
| Adds the 128-bit value formed by concatenating `a0' and `a1' to the 128-bit
| value formed by concatenating `b0' and `b1'.  Addition is modulo 2^128, so
| any carry out is lost.  The result is broken into two 64-bit pieces which
| are stored at the locations pointed to by `z0Ptr' and `z1Ptr'.
*----------------------------------------------------------------------------*}

procedure add128( a0, a1, b0, b1 : bits64; var z0Ptr, z1Ptr : bits64);inline;
var
    z1 : bits64;
begin
    z1 := a1 + b1;
    z1Ptr := z1;
    z0Ptr := a0 + b0 + ord( z1 < a1 );
end;

{*----------------------------------------------------------------------------
| Adds the 192-bit value formed by concatenating `a0', `a1', and `a2' to the
| 192-bit value formed by concatenating `b0', `b1', and `b2'.  Addition is
| modulo 2^192, so any carry out is lost.  The result is broken into three
| 64-bit pieces which are stored at the locations pointed to by `z0Ptr',
| `z1Ptr', and `z2Ptr'.
*----------------------------------------------------------------------------*}

procedure add192(a0,a1,a2,b0,b1,b2: bits64; var z0Ptr,z1Ptr,z2Ptr : bits64);
var
    z0, z1, z2 : bits64;
    carry0, carry1 : int8;
begin
    z2 := a2 + b2;
    carry1 := ord( z2 < a2 );
    z1 := a1 + b1;
    carry0 := ord( z1 < a1 );
    z0 := a0 + b0;
    inc(z1, carry1);
    inc(z0, ord( z1 < carry1 ));
    inc(z0, carry0);
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
end;

{*
-------------------------------------------------------------------------------
Subtracts the 64-bit value formed by concatenating `b0' and `b1' from the
64-bit value formed by concatenating `a0' and `a1'.  Subtraction is modulo
2^64, so any borrow out (carry out) is lost.  The result is broken into two
32-bit pieces which are stored at the locations pointed to by `z0Ptr' and
`z1Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 sub64(
     a0: bits32; a1 : bits32; b0 :bits32; b1: bits32; VAR z0Ptr:bits32; VAR z1Ptr: bits32 );
Begin
    z1Ptr := a1 - b1;
    z0Ptr := a0 - b0 - bits32( a1 < b1 );
End;

{*
-------------------------------------------------------------------------------
Subtracts the 96-bit value formed by concatenating `b0', `b1', and `b2' from
the 96-bit value formed by concatenating `a0', `a1', and `a2'.  Subtraction
is modulo 2^96, so any borrow out (carry out) is lost.  The result is broken
into three 32-bit pieces which are stored at the locations pointed to by
`z0Ptr', `z1Ptr', and `z2Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 sub96(
     a0:bits32;
     a1:bits32;
     a2:bits32;
     b0:bits32;
     b1:bits32;
     b2:bits32;
     VAR z0Ptr:bits32;
     VAR z1Ptr:bits32;
     VAR z2Ptr:bits32
 );
Var
    z0, z1, z2: bits32;
    borrow0, borrow1: int8;
Begin
    z2 := a2 - b2;
    borrow1 := int8( a2 < b2 );
    z1 := a1 - b1;
    borrow0 := int8( a1 < b1 );
    z0 := a0 - b0;
    z0 := z0 - bits32( z1 < borrow1 );
    z1 := z1 - borrow1;
    z0 := z0 -borrow0;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*----------------------------------------------------------------------------
| Subtracts the 128-bit value formed by concatenating `b0' and `b1' from the
| 128-bit value formed by concatenating `a0' and `a1'.  Subtraction is modulo
| 2^128, so any borrow out (carry out) is lost.  The result is broken into two
| 64-bit pieces which are stored at the locations pointed to by `z0Ptr' and
| `z1Ptr'.
*----------------------------------------------------------------------------*}

procedure sub128( a0, a1, b0, b1 : bits64; var z0Ptr, z1Ptr : bits64);
begin
    z1Ptr := a1 - b1;
    z0Ptr := a0 - b0 - ord( a1 < b1 );
end;


{*----------------------------------------------------------------------------
| Subtracts the 192-bit value formed by concatenating `b0', `b1', and `b2'
| from the 192-bit value formed by concatenating `a0', `a1', and `a2'.
| Subtraction is modulo 2^192, so any borrow out (carry out) is lost.  The
| result is broken into three 64-bit pieces which are stored at the locations
| pointed to by `z0Ptr', `z1Ptr', and `z2Ptr'.
*----------------------------------------------------------------------------*}

procedure sub192(a0,a1,a2,b0,b1,b2: bits64; var z0Ptr,z1Ptr,z2Ptr : bits64);
var
    z0, z1, z2 : bits64;
    borrow0, borrow1 : int8;
begin
    z2 := a2 - b2;
    borrow1 := ord( a2 < b2 );
    z1 := a1 - b1;
    borrow0 := ord( a1 < b1 );
    z0 := a0 - b0;
    dec(z0, ord( z1 < borrow1 ));
    dec(z1, borrow1);
    dec(z0, borrow0);
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
end;

{*
-------------------------------------------------------------------------------
Multiplies `a' by `b' to obtain a 64-bit product.  The product is broken
into two 32-bit pieces which are stored at the locations pointed to by
`z0Ptr' and `z1Ptr'.
-------------------------------------------------------------------------------
*}
Procedure mul32To64( a:bits32; b:bits32; VAR z0Ptr: bits32; VAR z1Ptr
:bits32 );
Var
    aHigh, aLow, bHigh, bLow: bits16;
    z0, zMiddleA, zMiddleB, z1: bits32;
Begin
    aLow := a and $ffff;
    aHigh := a shr 16;
    bLow := b and $ffff;
    bHigh := b shr 16;
    z1 := ( bits32( aLow) ) * bLow;
    zMiddleA := ( bits32 (aLow) ) * bHigh;
    zMiddleB := ( bits32 (aHigh) ) * bLow;
    z0 := ( bits32 (aHigh) ) * bHigh;
    zMiddleA := zMiddleA + zMiddleB;
    z0 := z0 + ( ( bits32 ( zMiddleA < zMiddleB ) ) shl 16 ) + ( zMiddleA shr 16 );
    zMiddleA := zmiddleA shl 16;
    z1 := z1 + zMiddleA;
    z0 := z0 + bits32( z1 < zMiddleA );
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*
-------------------------------------------------------------------------------
Multiplies the 64-bit value formed by concatenating `a0' and `a1' by `b'
to obtain a 96-bit product.  The product is broken into three 32-bit pieces
which are stored at the locations pointed to by `z0Ptr', `z1Ptr', and
`z2Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 mul64By32To96(
     a0:bits32;
     a1:bits32;
     b:bits32;
     VAR z0Ptr:bits32;
     VAR z1Ptr:bits32;
     VAR z2Ptr:bits32
 );
Var
    z0, z1, z2, more1: bits32;
Begin
    mul32To64( a1, b, z1, z2 );
    mul32To64( a0, b, z0, more1 );
    add64( z0, more1, 0, z1, z0, z1 );
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*
-------------------------------------------------------------------------------
Multiplies the 64-bit value formed by concatenating `a0' and `a1' to the
64-bit value formed by concatenating `b0' and `b1' to obtain a 128-bit
product.  The product is broken into four 32-bit pieces which are stored at
the locations pointed to by `z0Ptr', `z1Ptr', `z2Ptr', and `z3Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 mul64To128(
     a0:bits32;
     a1:bits32;
     b0:bits32;
     b1:bits32;
     VAR z0Ptr:bits32;
     VAR z1Ptr:bits32;
     VAR z2Ptr:bits32;
     VAR z3Ptr:bits32
 );
Var
    z0, z1, z2, z3: bits32;
    more1, more2: bits32;
Begin

    mul32To64( a1, b1, z2, z3 );
    mul32To64( a1, b0, z1, more2 );
    add64( z1, more2, 0, z2, z1, z2 );
    mul32To64( a0, b0, z0, more1 );
    add64( z0, more1, 0, z1, z0, z1 );
    mul32To64( a0, b1, more1, more2 );
    add64( more1, more2, 0, z2, more1, z2 );
    add64( z0, z1, 0, more1, z0, z1 );
    z3Ptr := z3;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*----------------------------------------------------------------------------
| Multiplies `a' by `b' to obtain a 128-bit product.  The product is broken
| into two 64-bit pieces which are stored at the locations pointed to by
| `z0Ptr' and `z1Ptr'.
*----------------------------------------------------------------------------*}

procedure mul64To128( a, b : bits64; var z0Ptr, z1Ptr : bits64);
var
    aHigh, aLow, bHigh, bLow : bits32;
    z0, zMiddleA, zMiddleB, z1 : bits64;
begin
    aLow := a;
    aHigh := a shr 32;
    bLow := b;
    bHigh := b shr 32;
    z1 := ( bits64(aLow) ) * bLow;
    zMiddleA := ( bits64( aLow )) * bHigh;
    zMiddleB := ( bits64( aHigh )) * bLow;
    z0 := ( bits64(aHigh) ) * bHigh;
    inc(zMiddleA, zMiddleB);
    inc(z0 ,( ( bits64( zMiddleA < zMiddleB ) ) shl 32 ) + ( zMiddleA shr 32 ));
    zMiddleA := zMiddleA shl 32;
    inc(z1, zMiddleA);
    inc(z0, ord( z1 < zMiddleA ));
    z1Ptr := z1;
    z0Ptr := z0;
end;

{*----------------------------------------------------------------------------
| Multiplies the 128-bit value formed by concatenating `a0' and `a1' to the
| 128-bit value formed by concatenating `b0' and `b1' to obtain a 256-bit
| product.  The product is broken into four 64-bit pieces which are stored at
| the locations pointed to by `z0Ptr', `z1Ptr', `z2Ptr', and `z3Ptr'.
*----------------------------------------------------------------------------*}

procedure mul128To256(a0,a1,b0,b1 : bits64;var z0Ptr,z1Ptr,z2Ptr,z3Ptr : bits64);
var
    z0,z1,z2,z3,more1,more2 : bits64;
begin
    mul64To128( a1, b1, z2, z3 );
    mul64To128( a1, b0, z1, more2 );
    add128( z1, more2, 0, z2, z1, z2 );
    mul64To128( a0, b0, z0, more1 );
    add128( z0, more1, 0, z1, z0, z1 );
    mul64To128( a0, b1, more1, more2 );
    add128( more1, more2, 0, z2, more1, z2 );
    add128( z0, z1, 0, more1, z0, z1 );
    z3Ptr := z3;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
end;


{*----------------------------------------------------------------------------
| Multiplies the 128-bit value formed by concatenating `a0' and `a1' by
| `b' to obtain a 192-bit product.  The product is broken into three 64-bit
| pieces which are stored at the locations pointed to by `z0Ptr', `z1Ptr', and
| `z2Ptr'.
*----------------------------------------------------------------------------*}
procedure mul128By64To192(a0,a1,b : bits64;var z0Ptr,z1Ptr,z2Ptr : bits64);
var
    z0, z1, z2, more1 : bits64;
begin
    mul64To128( a1, b, z1, z2 );
    mul64To128( a0, b, z0, more1 );
    add128( z0, more1, 0, z1, z0, z1 );
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
end;

{*----------------------------------------------------------------------------
| Returns an approximation to the 64-bit integer quotient obtained by dividing
| `b' into the 128-bit value formed by concatenating `a0' and `a1'.  The
| divisor `b' must be at least 2^63.  If q is the exact quotient truncated
| toward zero, the approximation returned lies between q and q + 2 inclusive.
| If the exact quotient q is larger than 64 bits, the maximum positive 64-bit
| unsigned integer is returned.
*----------------------------------------------------------------------------*}

Function estimateDiv128To64( a0:bits64; a1: bits64; b:bits64): bits64;
var
    b0, b1, rem0, rem1, term0, term1, z : bits64;
begin
    if ( b <= a0 ) then
      begin
        result:=qword( $FFFFFFFFFFFFFFFF );
        exit;
      end;
    b0 := b shr 32;
    if ( b0 shl 32 <= a0 ) then
      z:=qword( $FFFFFFFF00000000 )
    else
      z:=( a0 div b0 ) shl 32;
    mul64To128( b, z, term0, term1 );
    sub128( a0, a1, term0, term1, rem0, rem1 );
    while ( ( sbits64(rem0) ) < 0 ) do begin
        dec(z,qword( $100000000 ));
        b1 := b shl 32;
        add128( rem0, rem1, b0, b1, rem0, rem1 );
    end;
    rem0 := ( rem0 shl 32 ) or ( rem1 shr 32 );
    if ( b0 shl 32 <= rem0 ) then
      z:=z or $FFFFFFFF
    else
      z:=z or rem0 div b0;
    result:=z;
end;


{*
-------------------------------------------------------------------------------
Returns an approximation to the 32-bit integer quotient obtained by dividing
`b' into the 64-bit value formed by concatenating `a0' and `a1'.  The
divisor `b' must be at least 2^31.  If q is the exact quotient truncated
toward zero, the approximation returned lies between q and q + 2 inclusive.
If the exact quotient q is larger than 32 bits, the maximum positive 32-bit
unsigned integer is returned.
-------------------------------------------------------------------------------
*}
Function estimateDiv64To32( a0:bits32; a1: bits32; b:bits32): bits32;
Var
    b0, b1: bits32;
    rem0, rem1, term0, term1: bits32;
    z: bits32;
Begin
    if ( b <= a0 ) then
    Begin
       estimateDiv64To32 := $FFFFFFFF;
       exit;
    End;
    b0 := b shr 16;
    if ( b0 shl 16 <= a0 ) then
       z:= $FFFF0000
     else
       z:= ( a0 div b0 ) shl 16;
    mul32To64( b, z, term0, term1 );
    sub64( a0, a1, term0, term1, rem0, rem1 );
    while ( ( sbits32 (rem0) ) < 0 ) do
    Begin
        z := z - $10000;
        b1 := b shl 16;
        add64( rem0, rem1, b0, b1, rem0, rem1 );
    End;
    rem0 := ( rem0 shl 16 ) OR ( rem1 shr 16 );
    if ( b0 shl 16 <= rem0 ) then
      z := z or $FFFF
    else
      z := z or (rem0 div b0);
    estimateDiv64To32 := z;

End;

{*
-------------------------------------------------------------------------------
Returns an approximation to the square root of the 32-bit significand given
by `a'.  Considered as an integer, `a' must be at least 2^31.  If bit 0 of
`aExp' (the least significant bit) is 1, the integer returned approximates
2^31*sqrt(`a'/2^31), where `a' is considered an integer.  If bit 0 of `aExp'
is 0, the integer returned approximates 2^31*sqrt(`a'/2^30).  In either
case, the approximation returned lies strictly within +/-2 of the exact
value.
-------------------------------------------------------------------------------
*}
Function estimateSqrt32( aExp: int16; a: bits32 ): bits32;
    const sqrtOddAdjustments: array[0..15] of bits16 = (
        $0004, $0022, $005D, $00B1, $011D, $019F, $0236, $02E0,
        $039C, $0468, $0545, $0631, $072B, $0832, $0946, $0A67
    );
    const sqrtEvenAdjustments: array[0..15] of bits16 = (
        $0A2D, $08AF, $075A, $0629, $051A, $0429, $0356, $029E,
        $0200, $0179, $0109, $00AF, $0068, $0034, $0012, $0002
    );
Var
    index: int8;
    z: bits32;
Begin

    index := ( a shr 27 ) AND 15;
    if ( aExp AND 1 ) <> 0  then
    Begin
        z := $4000 + ( a shr 17 ) - sqrtOddAdjustments[ index ];
        z := ( ( a div z ) shl 14 ) + ( z shl 15 );
        a := a shr 1;
    End
    else
    Begin
        z := $8000 + ( a shr 17 ) - sqrtEvenAdjustments[ index ];
        z := a div z + z;
        if ( $20000 <= z ) then
          z := $FFFF8000
        else
          z := ( z shl 15 );
        if ( z <= a ) then
        Begin
           estimateSqrt32 := bits32 ( ( sbits32 (a )) shr 1 );
           exit;
        End;
    End;
    estimateSqrt32 := ( ( estimateDiv64To32( a, 0, z ) ) shr 1 ) + ( z shr 1 );
End;

{*
-------------------------------------------------------------------------------
Returns the number of leading 0 bits before the most-significant 1 bit of
`a'.  If `a' is zero, 32 is returned.
-------------------------------------------------------------------------------
*}
Function countLeadingZeros32( a:bits32 ): int8;

    const countLeadingZerosHigh:array[0..255] of int8 = (
        8, 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    );
Var
    shiftCount: int8;
Begin

    shiftCount := 0;
    if ( a < $10000 ) then
    Begin
        shiftCount := shiftcount + 16;
        a := a shl 16;
    End;
    if ( a < $1000000 ) then
    Begin
        shiftCount := shiftcount + 8;
        a := a shl 8;
    end;
    shiftCount := shiftcount + countLeadingZerosHigh[ a shr 24 ];
    countLeadingZeros32:= shiftCount;
End;

{*----------------------------------------------------------------------------
| Returns the number of leading 0 bits before the most-significant 1 bit of
| `a'.  If `a' is zero, 64 is returned.
*----------------------------------------------------------------------------*}

function countLeadingZeros64( a : bits64): int8;
var
 shiftcount : int8;
Begin
    shiftCount := 0;
    if ( a <  (bits64(1)  shl 32 )) then
        shiftCount := shiftcount + 32
    else
        a := a shr 32;
    shiftCount := shiftCount + countLeadingZeros32( a );
    countLeadingZeros64:= shiftCount;
End;



{*
-------------------------------------------------------------------------------
Returns 1 if the 64-bit value formed by concatenating `a0' and `a1' is
equal to the 64-bit value formed by concatenating `b0' and `b1'.  Otherwise,
returns 0.
-------------------------------------------------------------------------------
*}
Function eq64( a0: bits32; a1:bits32 ;b0:bits32; b1:bits32 ): flag;
Begin
    eq64 :=  flag( a0 = b0 ) and flag( a1 = b1 );
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the 64-bit value formed by concatenating `a0' and `a1' is less
than or equal to the 64-bit value formed by concatenating `b0' and `b1'.
Otherwise, returns 0.
-------------------------------------------------------------------------------
*}
Function le64( a0: bits32; a1:bits32 ;b0:bits32; b1:bits32 ): flag;
Begin

    le64:= flag( a0 < b0 ) or flag( ( a0 = b0 ) and ( a1 <= b1 ) );

End;

{*
-------------------------------------------------------------------------------
Returns 1 if the 64-bit value formed by concatenating `a0' and `a1' is less
than the 64-bit value formed by concatenating `b0' and `b1'.  Otherwise,
returns 0.
-------------------------------------------------------------------------------
*}
Function lt64( a0: bits32; a1:bits32 ;b0:bits32; b1:bits32 ): flag;
Begin
    lt64 := flag( a0 < b0 ) or flag( ( a0 = b0 ) and ( a1 < b1 ) );
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the 64-bit value formed by concatenating `a0' and `a1' is not
equal to the 64-bit value formed by concatenating `b0' and `b1'.  Otherwise,
returns 0.
-------------------------------------------------------------------------------
*}
Function ne64( a0: bits32; a1:bits32 ;b0:bits32; b1:bits32 ): flag;
Begin
    ne64:= flag( a0 <> b0 ) or flag( a1 <> b1 );
End;

const
  float128_default_nan_high = qword($FFFFFFFFFFFFFFFF);
  float128_default_nan_low = qword($FFFFFFFFFFFFFFFF);


{
****************************************************************************
*                      End Low-Level arithmetic                             *
*****************************************************************************

}


{

-------------------------------------------------------------------------------
Functions and definitions to determine:  (1) whether tininess for underflow
is detected before or after rounding by default, (2) what (if anything)
happens when exceptions are raised, (3) how signaling NaNs are distinguished
from quiet NaNs, (4) the default generated quiet NaNs, and (4) how NaNs
are propagated from function inputs to output.  These details are ENDIAN
specific
-------------------------------------------------------------------------------
}

{$IFDEF ENDIAN_LITTLE}
{

-------------------------------------------------------------------------------
Internal canonical NaN format.
-------------------------------------------------------------------------------
}
TYPE
 commonNaNT = packed record
   sign: flag;
   high, low : bits32;
 end;
{
-------------------------------------------------------------------------------
The pattern for a default generated single-precision NaN.
-------------------------------------------------------------------------------
}
const float32_default_nan = $FFC00000;

{
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is a NaN;
otherwise returns 0.
-------------------------------------------------------------------------------
}
Function float32_is_nan( a : float32 ): flag;
Begin

    float32_is_nan:= flag( $FF000000 < bits32 ( a shl 1 ) );

End;

{
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is a signaling
NaN; otherwise returns 0.
-------------------------------------------------------------------------------
}
Function float32_is_signaling_nan( a : float32  ): flag;
Begin

    float32_is_signaling_nan := flag
      ( ( ( a shr 22 ) and $1FF ) = $1FE ) and( a and $003FFFFF );

End;

{
-------------------------------------------------------------------------------
Returns the result of converting the single-precision floating-point NaN
`a' to the canonical NaN format.  If `a' is a signaling NaN, the invalid
exception is raised.
-------------------------------------------------------------------------------
}
Procedure float32ToCommonNaN( a: float32; VAR c:commonNaNT  );
var
    z : commonNaNT ;
Begin
    if ( float32_is_signaling_nan( a ) <> 0) then
       float_raise( float_flag_invalid );
    z.sign := a shr 31;
    z.low := 0;
    z.high := a shl 9;
    c := z;

End;

{
-------------------------------------------------------------------------------
Returns the result of converting the canonical NaN `a' to the single-
precision floating-point format.
-------------------------------------------------------------------------------
}
Function commonNaNToFloat32( a : commonNaNT ): float32;
Begin
    commonNaNToFloat32 := ( ( bits32 (a.sign) ) shl 31 ) or $7FC00000 or ( a.high shr 9 );
End;

{
-------------------------------------------------------------------------------
Takes two single-precision floating-point values `a' and `b', one of which
is a NaN, and returns the appropriate NaN result.  If either `a' or `b' is a
signaling NaN, the invalid exception is raised.
-------------------------------------------------------------------------------
}
Function propagateFloat32NaN( a : float32 ; b: float32 ): float32;
Var
    aIsNaN, aIsSignalingNaN, bIsNaN, bIsSignalingNaN: flag;
label returnLargerSignificand;
Begin
    aIsNaN := float32_is_nan( a );
    aIsSignalingNaN := float32_is_signaling_nan( a );
    bIsNaN := float32_is_nan( b );
    bIsSignalingNaN := float32_is_signaling_nan( b );
    a := a or $00400000;
    b := b or $00400000;
    if ( aIsSignalingNaN or bIsSignalingNaN ) <> 0 then
        float_raise( float_flag_invalid );
    if ( aIsSignalingNaN )<> 0  then
    Begin
        if ( bIsSignalingNaN ) <>  0 then
          goto returnLargerSignificand;
        if bIsNan <> 0 then
          propagateFloat32NaN := b
        else
          propagateFloat32NaN := a;
        exit;
    End
    else if ( aIsNaN <> 0) then
    Begin
        if ( bIsSignalingNaN or not bIsNaN )<> 0 then
        Begin
           propagateFloat32NaN := a;
           exit;
        End;
 returnLargerSignificand:
        if ( bits32 ( a shl 1 ) < bits32 ( b shl 1 ) ) then
        Begin
           propagateFloat32NaN := b;
           exit;
        End;
        if ( bits32 ( b shl 1 ) < bits32 ( a shl 1 ) ) then
        Begin
           propagateFloat32NaN :=  a;
        End;
        if a < b then
          propagateFloat32NaN := a
        else
          propagateFloat32NaN := b;
        exit;
    End
    else
    Begin
        propagateFloat32NaN := b;
        exit;
    End;

End;

{*
-------------------------------------------------------------------------------
The pattern for a default generated double-precision NaN.  The `high' and
`low' values hold the most- and least-significant bits, respectively.
-------------------------------------------------------------------------------
*}
const
    float64_default_nan_high = $FFF80000;
    float64_default_nan_low  = $00000000;

{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is a NaN;
otherwise returns 0.
-------------------------------------------------------------------------------
*}
Function float64_is_nan( a : float64 ) : flag;
Begin

    float64_is_nan :=
           flag( $FFE00000 <= bits32 ( a.high shl 1 ) )
        and ( a.low or ( a.high and $000FFFFF ) );

End;

{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is a signaling
NaN; otherwise returns 0.
-------------------------------------------------------------------------------
*}
Function float64_is_signaling_nan( a : float64 ): flag;
Begin

    float64_is_signaling_nan :=
           flag( ( ( a.high shr 19 ) and $FFF ) = $FFE )
        and ( a.low or ( a.high and $0007FFFF ) );
End;

{*
-------------------------------------------------------------------------------
Returns the result of converting the double-precision floating-point NaN
`a' to the canonical NaN format.  If `a' is a signaling NaN, the invalid
exception is raised.
-------------------------------------------------------------------------------
*}
Procedure float64ToCommonNaN( a : float64; VAR c:commonNaNT );
Var
    z : commonNaNT;
Begin
    if ( float64_is_signaling_nan( a )<>0 ) then
        float_raise( float_flag_invalid );
    z.sign := a.high shr 31;
    shortShift64Left( a.high, a.low, 12, z.high, z.low );
    c := z;

End;

function float64ToCommonNaN( a : float64 ) : commonNaNT;
Var
    z : commonNaNT;
Begin
    if ( float64_is_signaling_nan( a )<>0 ) then
        float_raise( float_flag_invalid );
    z.sign := a.high shr 31;
    shortShift64Left( a.high, a.low, 12, z.high, z.low );
    result := z;

End;
{*
-------------------------------------------------------------------------------
Returns the result of converting the canonical NaN `a' to the double-
precision floating-point format.
-------------------------------------------------------------------------------
*}
Procedure commonNaNToFloat64( a : commonNaNT; VAR c: float64  );
Var
    z: float64;
Begin
    shift64Right( a.high, a.low, 12, z.high, z.low );
    z.high := z.high or ( ( bits32 (a.sign) ) shl 31 ) or $7FF80000;
    c := z;
End;

{*
-------------------------------------------------------------------------------
Takes two double-precision floating-point values `a' and `b', one of which
is a NaN, and returns the appropriate NaN result.  If either `a' or `b' is a
signaling NaN, the invalid exception is raised.
-------------------------------------------------------------------------------
*}
Procedure propagateFloat64NaN( a: float64; b: float64 ; VAR c: float64 );
Var
    aIsNaN, aIsSignalingNaN, bIsNaN, bIsSignalingNaN: flag;
    label returnLargerSignificand;
Begin
    aIsNaN := float64_is_nan( a );
    aIsSignalingNaN := float64_is_signaling_nan( a );
    bIsNaN := float64_is_nan( b );
    bIsSignalingNaN := float64_is_signaling_nan( b );
    a.high := a.high or $00080000;
    b.high := b.high or $00080000;
    if ( aIsSignalingNaN or bIsSignalingNaN )<> 0 then
        float_raise( float_flag_invalid );
    if ( aIsSignalingNaN )<>0 then
    Begin
        if ( bIsSignalingNaN )<>0 then
            goto returnLargerSignificand;
        if bIsNan <> 0 then
           c := b
        else
           c := a;
        exit;
    End
    else if ( aIsNaN )<> 0 then
    Begin
        if ( bIsSignalingNaN or not bIsNaN ) <> 0 then
        Begin
          c := a;
           exit;
        End;
 returnLargerSignificand:
        if ( lt64( a.high shl 1, a.low, b.high shl 1, b.low ) ) <> 0 then
        Begin
           c := b;
           exit;
        End;
        if ( lt64( b.high shl 1, b.low, a.high shl 1, a.low ) ) <> 0 then
        Begin
           c := a;
           exit;
        End;
        if a.high < b.high then
         c := a
        else
         c := b;
        exit;
    End
    else
    Begin
        c := b;
        exit;
    End;
End;

{*----------------------------------------------------------------------------
| Returns 1 if the 128-bit value formed by concatenating `a0' and `a1' is less
| than the 128-bit value formed by concatenating `b0' and `b1'.  Otherwise,
| returns 0.
*----------------------------------------------------------------------------*}

function lt128(a0: bits64; a1: bits64; b0: bits64; b1 : bits64): flag;inline;
begin
    result := ord(( a0 < b0 ) or ( ( a0 = b0 ) and ( a1 < b1 ) ));
end;

{*----------------------------------------------------------------------------
| Returns 1 if the quadruple-precision floating-point value `a' is a NaN;
| otherwise returns 0.
*----------------------------------------------------------------------------*}

function float128_is_nan( a : float128): flag;
begin
    result:= ord(( int64( $FFFE000000000000 ) <= bits64( a.high shl 1 ) )
        and ( (a.low<>0) or (( a.high and int64( $0000FFFFFFFFFFFF ) )<>0 ) ));
end;

{*----------------------------------------------------------------------------
| Returns 1 if the quadruple-precision floating-point value `a' is a
| signaling NaN; otherwise returns 0.
*----------------------------------------------------------------------------*}

function float128_is_signaling_nan( a : float128): flag;
begin
    result:=ord(( ( ( a.high shr 47 ) and $FFFF ) = $FFFE ) and
        ( (a.low<>0) or (( a.high and int64( $00007FFFFFFFFFFF ) )<>0) ));
end;

{*----------------------------------------------------------------------------
| Returns the result of converting the quadruple-precision floating-point NaN
| `a' to the canonical NaN format.  If `a' is a signaling NaN, the invalid
| exception is raised.
*----------------------------------------------------------------------------*}

function float128ToCommonNaN( a : float128): commonNaNT;
var
    z: commonNaNT;
    qhigh,qlow : qword;
begin
    if ( float128_is_signaling_nan( a )<>0)  then
      float_raise( float_flag_invalid );
    z.sign := a.high shr 63;
    shortShift128Left( a.high, a.low, 16, qhigh, qlow );
    z.high:=qhigh shr 32;
    z.low:=qhigh and $ffffffff;
    result:=z;
end;

{*----------------------------------------------------------------------------
| Returns the result of converting the canonical NaN `a' to the quadruple-
| precision floating-point format.
*----------------------------------------------------------------------------*}

function commonNaNToFloat128( a : commonNaNT): float128;
var
    z: float128;
begin
    shift128Right( a.high, a.low, 16, z.high, z.low );
    z.high := z.high or ( ( bits64(a.sign) ) shl 63 ) or int64( $7FFF800000000000 );
    result:=z;
end;

{*----------------------------------------------------------------------------
| Takes two quadruple-precision floating-point values `a' and `b', one of
| which is a NaN, and returns the appropriate NaN result.  If either `a' or
| `b' is a signaling NaN, the invalid exception is raised.
*----------------------------------------------------------------------------*}

function propagateFloat128NaN( a: float128; b : float128): float128;
var
    aIsNaN, aIsSignalingNaN, bIsNaN, bIsSignalingNaN: flag;
label
    returnLargerSignificand;
begin
    aIsNaN := float128_is_nan( a );
    aIsSignalingNaN := float128_is_signaling_nan( a );
    bIsNaN := float128_is_nan( b );
    bIsSignalingNaN := float128_is_signaling_nan( b );
    a.high := a.high or int64( $0000800000000000 );
    b.high := b.high or int64( $0000800000000000 );
    if ( aIsSignalingNaN or bIsSignalingNaN )<>0 then
       float_raise( float_flag_invalid );
    if ( aIsSignalingNaN )<>0 then
    begin
        if ( bIsSignalingNaN )<>0 then
          goto returnLargerSignificand;
        if bIsNaN<>0 then
          result := b
        else
          result := a;
        exit;
    end
    else if ( aIsNaN )<>0 then
    begin
        if ( bIsSignalingNaN or not( bIsNaN) )<>0 then
          begin
          	result := a;
          	exit;
          end;
 returnLargerSignificand:
        if ( lt128( a.high shl 1, a.low, b.high shl 1, b.low ) )<>0 then
          begin
          	result := b;
          	exit;
          end;
        if ( lt128( b.high shl 1, b.low, a.high shl 1, a.low ) )<>0 then
          begin
          	result := a;
          	exit
          end;
        if ( a.high < b.high ) then
          result := a
        else
          result := b;
        exit;
    end
    else
    result:=b;
end;


{$ELSE}

{ Big endian code }
{----------------------------------------------------------------------------
| Internal canonical NaN format.
*----------------------------------------------------------------------------}
type
 commonNANT = packed record
  sign : flag;
  high, low : bits32;
 end;

{----------------------------------------------------------------------------
| The pattern for a default generated single-precision NaN.
*----------------------------------------------------------------------------}
const float32_default_nan = $7FFFFFFF;

{----------------------------------------------------------------------------
| Returns 1 if the single-precision floating-point value `a' is a NaN;
| otherwise returns 0.
*----------------------------------------------------------------------------}
function float32_is_nan(a:  float32): flag;
begin
    float32_is_nan := flag( $FF000000 < bits32( a shl 1 ) );
end;

{----------------------------------------------------------------------------
| Returns 1 if the single-precision floating-point value `a' is a signaling
| NaN; otherwise returns 0.
*----------------------------------------------------------------------------}
function float32_is_signaling_nan(a: float32):flag;
 begin
   float32_is_signaling_nan := flag( ( ( a shr 22 ) and $1FF ) = $1FE ) and flag( boolean((a and $003FFFFF)<>0) );
 end;

{----------------------------------------------------------------------------
| Returns the result of converting the single-precision floating-point NaN
| `a' to the canonical NaN format.  If `a' is a signaling NaN, the invalid
| exception is raised.
*----------------------------------------------------------------------------}
Procedure float32ToCommonNaN( a: float32; VAR c:commonNaNT  );
 var
  z: commonNANT;
 begin
   if float32_is_signaling_nan(a)<>0 then
      float_raise(float_flag_invalid);
   z.sign := a shr 31;
   z.low := 0;
   z.high := a shl 9;
   c:=z;
 end;

{----------------------------------------------------------------------------
| Returns the result of converting the canonical NaN `a' to the single-
| precision floating-point format.
*----------------------------------------------------------------------------}
function CommonNanToFloat32(a : CommonNaNT): float32;
 begin
    CommonNanToFloat32:= ( ( bits32( a.sign )) shl 31 ) OR $7FC00000 OR ( a.high shr 9 );
 end;

{----------------------------------------------------------------------------
| Takes two single-precision floating-point values `a' and `b', one of which
| is a NaN, and returns the appropriate NaN result.  If either `a' or `b' is a
| signaling NaN, the invalid exception is raised.
*----------------------------------------------------------------------------}
function  propagateFloat32NaN( a: float32 ; b: float32): float32;
 var
  aIsNaN, aIsSignalingNaN, bIsNaN, bIsSignalingNaN: flag;
 begin
    aIsNaN := float32_is_nan( a );
    aIsSignalingNaN := float32_is_signaling_nan( a );
    bIsNaN := float32_is_nan( b );
    bIsSignalingNaN := float32_is_signaling_nan( b );
    a := a or $00400000;
    b := b or $00400000;
    if ( aIsSignalingNaN or bIsSignalingNaN )<>0 then
       float_raise( float_flag_invalid );
    if bIsSignalingNaN<>0 then
        propagateFloat32Nan := b
    else if aIsSignalingNan<>0 then
        propagateFloat32Nan := a
    else if bIsNan<>0 then
        propagateFloat32Nan := b
    else
        propagateFloat32Nan := a;
 end;


{----------------------------------------------------------------------------
| The pattern for a default generated double-precision NaN.  The `high' and
| `low' values hold the most- and least-significant bits, respectively.
*----------------------------------------------------------------------------}
const
    float64_default_nan_high = $7FFFFFFF;
    float64_default_nan_low  = $FFFFFFFF;

{----------------------------------------------------------------------------
| Returns 1 if the double-precision floating-point value `a' is a NaN;
| otherwise returns 0.
*----------------------------------------------------------------------------}

function float64_is_nan(a: float64): flag;
 begin
    float64_is_nan := flag (
           ( $FFE00000 <= bits32 ( a.high shl 1 ) )
        and ( (a.low<>0) or (( a.high and $000FFFFF )<>0) ));
 end;

{----------------------------------------------------------------------------
| Returns 1 if the double-precision floating-point value `a' is a signaling
| NaN; otherwise returns 0.
*----------------------------------------------------------------------------}
function float64_is_signaling_nan( a:float64): flag;
 begin
    float64_is_signaling_nan := flag(
           ( ( ( a.high shr 19 ) and $FFF ) = $FFE )
        and ( (a.low<>0) or ( ( a.high and $0007FFFF )<>0) ));

 end;

{----------------------------------------------------------------------------
| Returns the result of converting the double-precision floating-point NaN
| `a' to the canonical NaN format.  If `a' is a signaling NaN, the invalid
| exception is raised.
*----------------------------------------------------------------------------}
Procedure float64ToCommonNaN( a : float64; VAR c:commonNaNT );
 var
   z : commonNaNT;
 begin
    if ( float64_is_signaling_nan( a )<>0 ) then
        float_raise( float_flag_invalid );
    z.sign := a.high shr 31;
    shortShift64Left( a.high, a.low, 12, z.high, z.low );
    c:=z;
 end;

{----------------------------------------------------------------------------
| Returns the result of converting the canonical NaN `a' to the double-
| precision floating-point format.
*----------------------------------------------------------------------------}
Procedure commonNaNToFloat64( a : commonNaNT; VAR c: float64  );
 var
  z: float64;
 begin
    shift64Right( a.high, a.low, 12, z.high, z.low );
    z.high := z.high or ( ( bits32 (a.sign) ) shl 31 ) or $7FF80000;
    c:=z;
 end;

{----------------------------------------------------------------------------
| Takes two double-precision floating-point values `a' and `b', one of which
| is a NaN, and returns the appropriate NaN result.  If either `a' or `b' is a
| signaling NaN, the invalid exception is raised.
*----------------------------------------------------------------------------}
Procedure propagateFloat64NaN( a: float64; b: float64 ; VAR c: float64 );
var
 aIsNaN, aIsSignalingNaN, bIsNaN, bIsSignalingNaN : flag;
 begin
    aIsNaN := float64_is_nan( a );
    aIsSignalingNaN := float64_is_signaling_nan( a );
    bIsNaN := float64_is_nan( b );
    bIsSignalingNaN := float64_is_signaling_nan( b );
    a.high := a.high or $00080000;
    b.high := b.high or $00080000;
    if ( (aIsSignalingNaN<>0) or (bIsSignalingNaN<>0) ) then
       float_raise( float_flag_invalid );
    if bIsSignalingNaN<>0 then
        c := b
    else if aIsSignalingNan<>0 then
        c := a
    else if bIsNan<>0 then
        c := b
    else
        c := a;
 end;

{$ENDIF}

{**************************************************************************}
{                        END ENDIAN SPECIFIC CODE                          }
{**************************************************************************}


{*
-------------------------------------------------------------------------------
Returns the fraction bits of the single-precision floating-point value `a'.
-------------------------------------------------------------------------------
*}
Function ExtractFloat32Frac(a : Float32) : Bits32;
 Begin
    ExtractFloat32Frac := A AND $007FFFFF;
 End;

{*
-------------------------------------------------------------------------------
Returns the exponent bits of the single-precision floating-point value `a'.
-------------------------------------------------------------------------------
*}
Function extractFloat32Exp( a: float32 ): Int16;
  Begin
    extractFloat32Exp := (a shr 23) AND $FF;
  End;

{*
-------------------------------------------------------------------------------
Returns the sign bit of the single-precision floating-point value `a'.
-------------------------------------------------------------------------------
*}
Function extractFloat32Sign( a: float32 ): Flag;
  Begin
    extractFloat32Sign := a shr 31;
  End;

{*
-------------------------------------------------------------------------------
Normalizes the subnormal single-precision floating-point value represented
by the denormalized significand `aSig'.  The normalized exponent and
significand are stored at the locations pointed to by `zExpPtr' and
`zSigPtr', respectively.
-------------------------------------------------------------------------------
*}
Procedure normalizeFloat32Subnormal( aSig : bits32; VAR zExpPtr: Int16; VAR zSigPtr :bits32);
 Var
   ShiftCount : BYTE;
 Begin

    shiftCount := countLeadingZeros32( aSig ) - 8;
    zSigPtr := aSig shl shiftCount;
    zExpPtr := 1 - shiftCount;
  End;

{*
-------------------------------------------------------------------------------
Packs the sign `zSign', exponent `zExp', and significand `zSig' into a
single-precision floating-point value, returning the result.  After being
shifted into the proper positions, the three fields are simply added
together to form the result.  This means that any integer portion of `zSig'
will be added into the exponent.  Since a properly normalized significand
will have an integer portion equal to 1, the `zExp' input should be 1 less
than the desired result exponent whenever `zSig' is a complete, normalized
significand.
-------------------------------------------------------------------------------
*}
Function packFloat32( zSign: Flag; zExp : Int16; zSig: Bits32 ): Float32;
 Begin

    packFloat32 := ( ( bits32( zSign) ) shl 31 ) + ( ( bits32 (zExp) ) shl 23 )
      + zSig;
 End;

{*
-------------------------------------------------------------------------------
Takes an abstract floating-point value having sign `zSign', exponent `zExp',
and significand `zSig', and returns the proper single-precision floating-
point value corresponding to the abstract input.  Ordinarily, the abstract
value is simply rounded and packed into the single-precision format, with
the inexact exception raised if the abstract input cannot be represented
exactly.  However, if the abstract value is too large, the overflow and
inexact exceptions are raised and an infinity or maximal finite value is
returned.  If the abstract value is too small, the input value is rounded to
a subnormal number, and the underflow and inexact exceptions are raised if
the abstract input cannot be represented exactly as a subnormal single-
precision floating-point number.
    The input significand `zSig' has its binary point between bits 30
and 29, which is 7 bits to the left of the usual location.  This shifted
significand must be normalized or smaller.  If `zSig' is not normalized,
`zExp' must be 0; in that case, the result returned is a subnormal number,
and it must not require rounding.  In the usual case that `zSig' is
normalized, `zExp' must be 1 less than the ``true'' floating-point exponent.
The handling of underflow and overflow follows the IEC/IEEE Standard for
Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function roundAndPackFloat32( zSign : Flag; zExp : Int16; zSig : Bits32 ) : float32;
 Var
   roundingMode : BYTE;
   roundNearestEven : Flag;
   roundIncrement, roundBits : BYTE;
   IsTiny : Flag;
 Begin
    roundingMode := softfloat_rounding_mode;
    if (roundingMode = float_round_nearest_even) then
      Begin
        roundNearestEven := Flag(TRUE);
      end
    else
       roundNearestEven := Flag(FALSE);
    roundIncrement := $40;
    if ( Boolean(roundNearestEven)  = FALSE)  then
      Begin
        if ( roundingMode = float_round_to_zero ) Then
          Begin
            roundIncrement := 0;
          End
        else
          Begin
            roundIncrement := $7F;
            if ( zSign <> 0 ) then
              Begin
                if roundingMode = float_round_up then roundIncrement := 0;
              End
            else
              Begin
                if roundingMode = float_round_down then roundIncrement := 0;
              End;
         End
      End;
    roundBits := zSig AND $7F;
    if ($FD <= bits16 (zExp) ) then
     Begin
        if (( $FD < zExp ) OR  ( zExp = $FD ) AND ( sbits32 ( zSig + roundIncrement ) < 0 ) ) then
          Begin
             float_raise( float_flag_overflow OR float_flag_inexact );
             roundAndPackFloat32:=packFloat32( zSign, $FF, 0 ) - Flag( roundIncrement = 0 );
             exit;
          End;
        if ( zExp < 0 ) then
          Begin
            isTiny :=
                   flag(( float_detect_tininess = float_tininess_before_rounding )
                OR ( zExp < -1 )
                OR ( (zSig + roundIncrement) < $80000000 ));
            shift32RightJamming( zSig, - zExp, zSig );
            zExp := 0;
            roundBits := zSig AND $7F;
            if ( (isTiny = flag(TRUE)) and (roundBits<>0) ) then exit ;//invalid

          End;
    End;
    if ( roundBits )<> 0 then
       softfloat_exception_flags := float_flag_inexact OR softfloat_exception_flags;
    zSig := ( zSig + roundIncrement ) shr 7;
    zSig := zSig AND not bits32( bits32( ( roundBits XOR $40 ) = 0 ) and roundNearestEven );
    if ( zSig = 0 ) then zExp := 0;
    roundAndPackFloat32 := packFloat32( zSign, zExp, zSig );
    exit;
  End;

{*
-------------------------------------------------------------------------------
Takes an abstract floating-point value having sign `zSign', exponent `zExp',
and significand `zSig', and returns the proper single-precision floating-
point value corresponding to the abstract input.  This routine is just like
`roundAndPackFloat32' except that `zSig' does not have to be normalized.
Bit 31 of `zSig' must be zero, and `zExp' must be 1 less than the ``true''
floating-point exponent.
-------------------------------------------------------------------------------
*}
Function normalizeRoundAndPackFloat32( zSign: flag; zExp: int16; zSig:bits32 ): float32;
  Var
    ShiftCount : int8;
  Begin
    shiftCount := countLeadingZeros32( zSig ) - 1;
    normalizeRoundAndPackFloat32 := roundAndPackFloat32( zSign, zExp - shiftCount, zSig shl shiftCount );
  End;

{*
-------------------------------------------------------------------------------
Returns the most-significant 20 fraction bits of the double-precision
floating-point value `a'.
-------------------------------------------------------------------------------
*}
Function extractFloat64Frac0(a: float64): bits32;
  Begin
    extractFloat64Frac0 := a.high and $000FFFFF;
  End;

{*
-------------------------------------------------------------------------------
Returns the least-significant 32 fraction bits of the double-precision
floating-point value `a'.
-------------------------------------------------------------------------------
*}
Function extractFloat64Frac1(a: float64): bits32;
  Begin
    extractFloat64Frac1 := a.low;
  End;


{$define FPC_SYSTEM_HAS_extractFloat64Frac}
Function extractFloat64Frac(a: float64): bits64;
  Begin
    extractFloat64Frac := bits64(a) and $000FFFFFFFFFFFFF;
  End;

{*
-------------------------------------------------------------------------------
Returns the exponent bits of the double-precision floating-point value `a'.
-------------------------------------------------------------------------------
*}
Function extractFloat64Exp(a: float64): int16;
 Begin
    extractFloat64Exp:= ( a.high shr 20 ) AND $7FF;
 End;

{*
-------------------------------------------------------------------------------
Returns the sign bit of the double-precision floating-point value `a'.
-------------------------------------------------------------------------------
*}
Function extractFloat64Sign(a: float64) : flag;
 Begin
    extractFloat64Sign := a.high shr 31;
 End;

{*
-------------------------------------------------------------------------------
Normalizes the subnormal double-precision floating-point value represented
by the denormalized significand formed by the concatenation of `aSig0' and
`aSig1'.  The normalized exponent is stored at the location pointed to by
`zExpPtr'.  The most significant 21 bits of the normalized significand are
stored at the location pointed to by `zSig0Ptr', and the least significant
32 bits of the normalized significand are stored at the location pointed to
by `zSig1Ptr'.
-------------------------------------------------------------------------------
*}
Procedure normalizeFloat64Subnormal(
     aSig0: bits32;
     aSig1: bits32;
     VAR zExpPtr : Int16;
     VAR zSig0Ptr : Bits32;
     VAR zSig1Ptr : Bits32
 );
 Var
  ShiftCount : Int8;
 Begin
    if ( aSig0 = 0 ) then
      Begin
        shiftCount := countLeadingZeros32( aSig1 ) - 11;
        if ( shiftCount < 0 ) then
          Begin
            zSig0Ptr := aSig1 shr ( - shiftCount );
            zSig1Ptr := aSig1 shl ( shiftCount AND 31 );
          End
        else
           Begin
            zSig0Ptr := aSig1 shl shiftCount;
            zSig1Ptr := 0;
           End;
        zExpPtr := - shiftCount - 31;
      End
    else
      Begin
        shiftCount := countLeadingZeros32( aSig0 ) - 11;
        shortShift64Left( aSig0, aSig1, shiftCount, zSig0Ptr, zSig1Ptr );
        zExpPtr := 1 - shiftCount;
      End;
  End;

procedure normalizeFloat64Subnormal(aSig : bits64;var zExpPtr : int16; var zSigPtr : bits64);
var
  shiftCount : int8;
begin
    shiftCount := countLeadingZeros64( aSig ) - 11;
    zSigPtr := aSig shl shiftCount;
    zExpPtr := 1 - shiftCount;
end;


{*
-------------------------------------------------------------------------------
Packs the sign `zSign', the exponent `zExp', and the significand formed by
the concatenation of `zSig0' and `zSig1' into a double-precision floating-
point value, returning the result.  After being shifted into the proper
positions, the three fields `zSign', `zExp', and `zSig0' are simply added
together to form the most significant 32 bits of the result.  This means
that any integer portion of `zSig0' will be added into the exponent.  Since
a properly normalized significand will have an integer portion equal to 1,
the `zExp' input should be 1 less than the desired result exponent whenever
`zSig0' and `zSig1' concatenated form a complete, normalized significand.
-------------------------------------------------------------------------------
*}
Procedure
 packFloat64( zSign: Flag; zExp: Int16; zSig0: Bits32; zSig1 : Bits32; VAR c : float64);
 var
    z: Float64;
 Begin

    z.low := zSig1;
    z.high := ( ( bits32 (zSign) ) shl 31 ) + ( ( bits32 (zExp) ) shl 20 ) + zSig0;
    c := z;
 End;


{*----------------------------------------------------------------------------
| Packs the sign `zSign', exponent `zExp', and significand `zSig' into a
| double-precision floating-point value, returning the result.  After being
| shifted into the proper positions, the three fields are simply added
| together to form the result.  This means that any integer portion of `zSig'
| will be added into the exponent.  Since a properly normalized significand
| will have an integer portion equal to 1, the `zExp' input should be 1 less
| than the desired result exponent whenever `zSig' is a complete, normalized
| significand.
*----------------------------------------------------------------------------*}

function packFloat64( zSign: flag; zExp: int16; zSig : bits64): float64;inline;
begin
    result := float64(( ( bits64(zSign) ) shl 63 ) + ( ( bits64(zExp) ) shl 52 ) + zSig);
end;

{*
-------------------------------------------------------------------------------
Takes an abstract floating-point value having sign `zSign', exponent `zExp',
and extended significand formed by the concatenation of `zSig0', `zSig1',
and `zSig2', and returns the proper double-precision floating-point value
corresponding to the abstract input.  Ordinarily, the abstract value is
simply rounded and packed into the double-precision format, with the inexact
exception raised if the abstract input cannot be represented exactly.
However, if the abstract value is too large, the overflow and inexact
exceptions are raised and an infinity or maximal finite value is returned.
If the abstract value is too small, the input value is rounded to a
subnormal number, and the underflow and inexact exceptions are raised if the
abstract input cannot be represented exactly as a subnormal double-precision
floating-point number.
    The input significand must be normalized or smaller.  If the input
significand is not normalized, `zExp' must be 0; in that case, the result
returned is a subnormal number, and it must not require rounding.  In the
usual case that the input significand is normalized, `zExp' must be 1 less
than the ``true'' floating-point exponent.  The handling of underflow and
overflow follows the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Procedure
 roundAndPackFloat64(
     zSign: Flag; zExp: Int16; zSig0: Bits32; zSig1: Bits32; zSig2: Bits32; Var c: Float64 );
 Var
   roundingMode : Int8;
   roundNearestEven, increment, isTiny : Flag;
 Begin

    roundingMode := softfloat_rounding_mode;
    roundNearestEven := flag( roundingMode = float_round_nearest_even );
    increment := flag( sbits32 (zSig2) < 0 );
    if ( roundNearestEven  = flag(FALSE) ) then
      Begin
        if ( roundingMode = float_round_to_zero ) then
            increment := 0
        else
          Begin
            if ( zSign )<> 0 then
              Begin
                increment := flag( roundingMode = float_round_down ) and zSig2;
              End
            else
              Begin
                increment := flag( roundingMode = float_round_up ) and zSig2;
              End
          End
      End;
    if ( $7FD <= bits16 (zExp) ) then
      Begin
        if (( $7FD < zExp )
             or (( zExp = $7FD )
                  and (eq64( $001FFFFF, $FFFFFFFF, zSig0, zSig1 )<>0)
                  and (increment<>0)
                )
           ) then
           Begin
            float_raise( float_flag_overflow OR  float_flag_inexact );
            if (( roundingMode = float_round_to_zero )
                 or ( (zSign<>0) and ( roundingMode = float_round_up ) )
                 or ( (zSign = 0) and ( roundingMode = float_round_down ) )
               ) then
              Begin
                packFloat64( zSign, $7FE, $000FFFFF, $FFFFFFFF, c );
                exit;
              End;
            packFloat64( zSign, $7FF, 0, 0, c );
            exit;
           End;
        if ( zExp < 0 ) then
           Begin
            isTiny :=
                   flag( float_detect_tininess = float_tininess_before_rounding )
                or flag( zExp < -1 )
                or  flag(increment = 0)
                or flag(lt64( zSig0, zSig1, $001FFFFF, $FFFFFFFF)<>0);
            shift64ExtraRightJamming(
                zSig0, zSig1, zSig2, - zExp, zSig0, zSig1, zSig2 );
            zExp := 0;
            if ( isTiny<>0) and (zSig2<>0 ) then float_raise( float_flag_underflow );
            if ( roundNearestEven )<>0 then
              Begin
                increment := flag( sbits32 (zSig2) < 0 );
              End
            else
              Begin
                if ( zSign )<>0 then
                  Begin
                    increment := flag( roundingMode = float_round_down ) and zSig2;
                  End
                else
                  Begin
                    increment := flag( roundingMode = float_round_up ) and zSig2;
                  End
              End;
        End;
    End;
    if ( zSig2 )<>0 then
       softfloat_exception_flags := softfloat_exception_flags OR  float_flag_inexact;
    if ( increment )<>0 then
      Begin
        add64( zSig0, zSig1, 0, 1, zSig0, zSig1 );
        zSig1 := zSig1 and not ( bits32(flag( zSig2 + zSig2 = 0 )) and roundNearestEven );
      End
    else
      Begin
        if ( ( zSig0 or zSig1 ) = 0 ) then zExp := 0;
      End;
    packFloat64( zSign, zExp, zSig0, zSig1, c );
 End;

{*----------------------------------------------------------------------------
| Takes an abstract floating-point value having sign `zSign', exponent `zExp',
| and significand `zSig', and returns the proper double-precision floating-
| point value corresponding to the abstract input.  Ordinarily, the abstract
| value is simply rounded and packed into the double-precision format, with
| the inexact exception raised if the abstract input cannot be represented
| exactly.  However, if the abstract value is too large, the overflow and
| inexact exceptions are raised and an infinity or maximal finite value is
| returned.  If the abstract value is too small, the input value is rounded
| to a subnormal number, and the underflow and inexact exceptions are raised
| if the abstract input cannot be represented exactly as a subnormal double-
| precision floating-point number.
|     The input significand `zSig' has its binary point between bits 62
| and 61, which is 10 bits to the left of the usual location.  This shifted
| significand must be normalized or smaller.  If `zSig' is not normalized,
| `zExp' must be 0; in that case, the result returned is a subnormal number,
| and it must not require rounding.  In the usual case that `zSig' is
| normalized, `zExp' must be 1 less than the ``true'' floating-point exponent.
| The handling of underflow and overflow follows the IEC/IEEE Standard for
| Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function roundAndPackFloat64( zSign: flag; zExp: int16; zSig : bits64): float64;
var
    roundingMode: int8;
    roundNearestEven: flag;
    roundIncrement, roundBits: int16;
    isTiny: flag;
begin
    roundingMode := softfloat_rounding_mode;
    roundNearestEven := ord( roundingMode = float_round_nearest_even );
    roundIncrement := $200;
    if ( roundNearestEven=0 ) then
    begin
        if ( roundingMode = float_round_to_zero ) then
        begin
            roundIncrement := 0;
        end
        else begin
            roundIncrement := $3FF;
            if ( zSign<>0 ) then
            begin
                if ( roundingMode = float_round_up ) then
                  roundIncrement := 0;
            end
            else begin
                if ( roundingMode = float_round_down ) then
                  roundIncrement := 0;
            end
        end
    end;
    roundBits := zSig and $3FF;
    if ( $7FD <= bits16(zExp) ) then
    begin
        if (    ( $7FD < zExp )
             or (    ( zExp = $7FD )
                  and ( sbits64( zSig + roundIncrement ) < 0 ) )
           ) then
           begin
            float_raise( float_flag_overflow or float_flag_inexact );
            result := float64(qword(packFloat64( zSign, $7FF, 0 )) - ord( roundIncrement = 0 ));
            exit;
        end;
        if ( zExp < 0 ) then
        begin
            isTiny := ord(
                   ( float_detect_tininess = float_tininess_before_rounding )
                or ( zExp < -1 )
                or ( (zSig + roundIncrement) < int64( $8000000000000000 ) ) );
            shift64RightJamming( zSig, - zExp, zSig );
            zExp := 0;
            roundBits := zSig and $3FF;
            if ( isTiny and roundBits )<>0 then
              float_raise( float_flag_underflow );
        end
    end;
    if ( roundBits<>0 ) then
      softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
    zSig := ( zSig + roundIncrement ) shr 10;
    zSig := zSig and not( ord( ( roundBits xor $200 ) = 0 ) and roundNearestEven );
    if ( zSig = 0 ) then
      zExp := 0;
    result:=packFloat64( zSign, zExp, zSig );
end;

{*
-------------------------------------------------------------------------------
Takes an abstract floating-point value having sign `zSign', exponent `zExp',
and significand formed by the concatenation of `zSig0' and `zSig1', and
returns the proper double-precision floating-point value corresponding
to the abstract input.  This routine is just like `roundAndPackFloat64'
except that the input significand has fewer bits and does not have to be
normalized.  In all cases, `zExp' must be 1 less than the ``true'' floating-
point exponent.
-------------------------------------------------------------------------------
*}
Procedure
 normalizeRoundAndPackFloat64(
     zSign:flag; zExp:int16; zSig0:bits32; zSig1:bits32; VAR c: float64 );
 Var
   shiftCount : int8;
   zSig2 : bits32;
 Begin

    if ( zSig0 = 0 ) then
     Begin
        zSig0 := zSig1;
        zSig1 := 0;
        zExp := zExp -32;
     End;
    shiftCount := countLeadingZeros32( zSig0 ) - 11;
    if ( 0 <= shiftCount ) then
      Begin
        zSig2 := 0;
        shortShift64Left( zSig0, zSig1, shiftCount, zSig0, zSig1 );
      End
    else
      Begin
        shift64ExtraRightJamming
          (zSig0, zSig1, 0, - shiftCount, zSig0, zSig1, zSig2 );
      End;
    zExp := zExp - shiftCount;
    roundAndPackFloat64( zSign, zExp, zSig0, zSig1, zSig2, c );
  End;

{*
-------------------------------------------------------------------------------
Returns the result of converting the 32-bit two's complement integer `a' to
the single-precision floating-point format.  The conversion is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function int32_to_float32( a: int32): float32rec; compilerproc;
 Var
  zSign : Flag;
 Begin

    if ( a = 0 ) then
      Begin
       int32_to_float32.float32 := 0;
       exit;
      End;
    if ( a = sbits32 ($80000000) ) then
      Begin
       int32_to_float32.float32 := packFloat32( 1, $9E, 0 );
       exit;
      end;
    zSign := flag( a < 0 );
    If zSign<>0 then
      a := -a;
    int32_to_float32.float32:=
      normalizeRoundAndPackFloat32( zSign, $9C, a );
 End;


{*
-------------------------------------------------------------------------------
Returns the result of converting the 32-bit two's complement integer `a' to
the double-precision floating-point format.  The conversion is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function int32_to_float64( a: int32) : float64;{$ifdef fpc} [public,Alias:'INT32_TO_FLOAT64'];compilerproc;{$endif}
  var
    zSign : flag;
    absA : bits32;
    shiftCount : int8;
    zSig0, zSig1 : bits32;
  Begin

    if ( a = 0 ) then
      Begin
       packFloat64( 0, 0, 0, 0, result );
       exit;
      end;
    zSign := flag( a < 0 );
    if ZSign<>0 then
      AbsA := -a
    else
      AbsA := a;
    shiftCount := countLeadingZeros32( absA ) - 11;
    if ( 0 <= shiftCount ) then
      Begin
        zSig0 := absA shl shiftCount;
        zSig1 := 0;
      End
    else
      Begin
        shift64Right( absA, 0, - shiftCount, zSig0, zSig1 );
      End;
    packFloat64( zSign, $412 - shiftCount, zSig0, zSig1, result );
  End;

{*
-------------------------------------------------------------------------------
Returns the result of converting the single-precision floating-point value
`a' to the 32-bit two's complement integer format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic---which means in particular that the conversion is rounded
according to the current rounding mode.  If `a' is a NaN, the largest
positive integer is returned.  Otherwise, if the conversion overflows, the
largest integer with the same sign as `a' is returned.
-------------------------------------------------------------------------------
*}
Function float32_to_int32( a : float32rec) : int32;compilerproc;
  Var
    aSign: flag;
    aExp, shiftCount: int16;
    aSig, aSigExtra: bits32;
    z: int32;
    roundingMode: int8;
  Begin

    aSig := extractFloat32Frac( a.float32 );
    aExp := extractFloat32Exp( a.float32 );
    aSign := extractFloat32Sign( a.float32 );
    shiftCount := aExp - $96;
    if ( 0 <= shiftCount ) then
      Begin
        if ( $9E <= aExp ) then
          Begin
            if ( a.float32 <> $CF000000 ) then
              Begin
                float_raise( float_flag_invalid );
                if ( (aSign=0) or ( ( aExp = $FF ) and (aSig<>0) ) ) then
                  Begin
                    float32_to_int32 := $7FFFFFFF;
                    exit;
                  End;
              End;
            float32_to_int32 := sbits32 ($80000000);
            exit;
          End;
        z := ( aSig or $00800000 ) shl shiftCount;
        if ( aSign<>0 ) then z := - z;
      End
    else
      Begin
        if ( aExp < $7E ) then
          Begin
            aSigExtra := aExp OR aSig;
            z := 0;
          End
        else
         Begin
            aSig := aSig OR $00800000;
            aSigExtra := aSig shl ( shiftCount and 31 );
            z := aSig shr ( - shiftCount );
         End;
        if ( aSigExtra<>0 ) then
          softfloat_exception_flags := softfloat_exception_flags
             or float_flag_inexact;
        roundingMode := softfloat_rounding_mode;
        if ( roundingMode = float_round_nearest_even ) then
          Begin
            if ( sbits32 (aSigExtra) < 0 ) then
              Begin
                Inc(z);
                if ( bits32 ( aSigExtra shl 1 ) = 0 ) then
                  z := z and not 1;
              End;
              if ( aSign<>0 ) then
                z := - z;
          End
        else
          Begin
            aSigExtra := flag( aSigExtra <> 0 );
            if ( aSign<>0 ) then
             Begin
                z := z + (flag( roundingMode = float_round_down ) and aSigExtra);
                z := - z;
             End
            else
             Begin
                z := z + (flag( roundingMode = float_round_up ) and aSigExtra);
             End
          End;
      End;
   float32_to_int32 := z;
  End;

{*
-------------------------------------------------------------------------------
Returns the result of converting the single-precision floating-point value
`a' to the 32-bit two's complement integer format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic, except that the conversion is always rounded toward zero.
If `a' is a NaN, the largest positive integer is returned.  Otherwise, if
the conversion overflows, the largest integer with the same sign as `a' is
returned.
-------------------------------------------------------------------------------
*}
Function float32_to_int32_round_to_zero( a: Float32rec ): int32;compilerproc;
 Var
    aSign : flag;
    aExp, shiftCount : int16;
    aSig : bits32;
    z : int32;
 Begin
    aSig := extractFloat32Frac( a.float32 );
    aExp := extractFloat32Exp( a.float32 );
    aSign := extractFloat32Sign( a.float32 );
    shiftCount := aExp - $9E;
    if ( 0 <= shiftCount ) then
      Begin
        if ( a.float32 <> $CF000000 ) then
          Begin
            float_raise( float_flag_invalid );
            if ( (aSign=0) or ( ( aExp = $FF ) and (aSig<>0) ) ) then
              Begin
                float32_to_int32_round_to_zero := $7FFFFFFF;
                exit;
              end;
          End;
        float32_to_int32_round_to_zero:= sbits32 ($80000000);
        exit;
      End
    else
      if ( aExp <= $7E ) then
      Begin
        if ( aExp or aSig )<>0 then
           softfloat_exception_flags :=
             softfloat_exception_flags or float_flag_inexact;
        float32_to_int32_round_to_zero := 0;
        exit;
      End;
    aSig := ( aSig or $00800000 ) shl 8;
    z := aSig shr ( - shiftCount );
    if ( bits32 ( aSig shl ( shiftCount and 31 ) )<> 0 ) then
      Begin
           softfloat_exception_flags :=
             softfloat_exception_flags or float_flag_inexact;
      End;
    if ( aSign<>0 ) then z := - z;
    float32_to_int32_round_to_zero := z;
 End;

{*
-------------------------------------------------------------------------------
Returns the result of converting the single-precision floating-point value
`a' to the double-precision floating-point format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_to_float64( a : float32rec) : Float64;compilerproc;
  Var
    aSign : flag;
    aExp : int16;
    aSig, zSig0, zSig1: bits32;
    tmp : CommonNanT;
  Begin
    aSig := extractFloat32Frac( a.float32 );
    aExp := extractFloat32Exp( a.float32 );
    aSign := extractFloat32Sign( a.float32 );
    if ( aExp = $FF ) then
      Begin
        if ( aSig<>0 ) then
          Begin
            float32ToCommonNaN(a.float32, tmp);
            commonNaNToFloat64(tmp , result);
            exit;
          End;
          packFloat64( aSign, $7FF, 0, 0, result);
          exit;
      End;
    if ( aExp = 0 ) then
      Begin
        if ( aSig = 0 ) then
          Begin
            packFloat64( aSign, 0, 0, 0, result );
            exit;
          end;
        normalizeFloat32Subnormal( aSig, aExp, aSig );
        Dec(aExp);
      End;
    shift64Right( aSig, 0, 3, zSig0, zSig1 );
    packFloat64( aSign, aExp + $380, zSig0, zSig1, result );
  End;

{*
-------------------------------------------------------------------------------
Rounds the single-precision floating-point value `a' to an integer,
and returns the result as a single-precision floating-point value.  The
operation is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_round_to_int( a: float32rec): float32rec;compilerproc;
  Var
    aSign: flag;
    aExp: int16;
    lastBitMask, roundBitsMask: bits32;
    roundingMode: int8;
    z: float32;
  Begin
    aExp := extractFloat32Exp( a.float32 );
    if ( $96 <= aExp ) then
     Begin
        if ( ( aExp = $FF ) and (extractFloat32Frac( a.float32 )<>0) ) then
          Begin
            float32_round_to_int.float32 := propagateFloat32NaN( a.float32, a.float32 );
            exit;
          End;
        float32_round_to_int:=a;
        exit;
     End;
    if ( aExp <= $7E ) then
      Begin
        if ( bits32 ( a.float32 shl 1 ) = 0 ) then
          Begin
             float32_round_to_int:=a;
             exit;
          end;
        softfloat_exception_flags
          := softfloat_exception_flags OR  float_flag_inexact;
        aSign := extractFloat32Sign( a.float32 );

        case ( softfloat_rounding_mode ) of
         float_round_nearest_even:
            Begin
              if ( ( aExp = $7E ) and (extractFloat32Frac( a.float32 )<>0) ) then
                Begin
                  float32_round_to_int.float32 := packFloat32( aSign, $7F, 0 );
                  exit;
                End;
            End;
         float_round_down:
            Begin
              if aSign <> 0 then
                 float32_round_to_int.float32 := $BF800000
              else
                 float32_round_to_int.float32 := 0;
              exit;
            End;
         float_round_up:
            Begin
              if aSign <> 0 then
                 float32_round_to_int.float32 := $80000000
              else
                 float32_round_to_int.float32 := $3F800000;
              exit;
            End;
        end;
        float32_round_to_int.float32 := packFloat32( aSign, 0, 0 );
      End;
    lastBitMask := 1;
    {_____________________________!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
    lastBitMask := lastBitMask shl ($96 - aExp);
    roundBitsMask := lastBitMask - 1;
    z := a.float32;
    roundingMode := softfloat_rounding_mode;
    if ( roundingMode = float_round_nearest_even ) then
      Begin
        z := z + (lastBitMask shr 1);
        if ( ( z and roundBitsMask ) = 0 ) then
           z := z and not lastBitMask;
      End
    else if ( roundingMode <> float_round_to_zero ) then
      Begin
        if ( (extractFloat32Sign( z ) xor flag(roundingMode = float_round_up ))<>0 ) then
          Begin
            z := z + roundBitsMask;
          End;
      End;
    z := z and not roundBitsMask;
    if ( z <> a.float32 ) then
      softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
    float32_round_to_int.float32 := z;
  End;

{*
-------------------------------------------------------------------------------
Returns the result of adding the absolute values of the single-precision
floating-point values `a' and `b'.  If `zSign' is 1, the sum is negated
before being returned.  `zSign' is ignored if the result is a NaN.
The addition is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function addFloat32Sigs( a:float32; b: float32; zSign:flag ): float32;
  Var
    aExp, bExp, zExp: int16;
    aSig, bSig, zSig: bits32;
    expDiff: int16;
    label roundAndPack;
  Begin
    aSig:=extractFloat32Frac( a );
    aExp:=extractFloat32Exp( a );
    bSig:=extractFloat32Frac( b );
    bExp := extractFloat32Exp( b );
    expDiff := aExp - bExp;
    aSig := aSig shl 6;
    bSig := bSig shl 6;
    if ( 0 < expDiff ) then
    Begin
        if ( aExp = $FF ) then
          Begin
            if ( aSig <> 0) then
              Begin
                addFloat32Sigs := propagateFloat32NaN( a, b );
                exit;
              End;
            addFloat32Sigs := a;
            exit;
          End;
        if ( bExp = 0 ) then
          Begin
             Dec(expDiff);
          End
        else
          Begin
            bSig := bSig or $20000000;
          End;
        shift32RightJamming( bSig, expDiff, bSig );
        zExp := aExp;
    End
    else
    If ( expDiff < 0 ) then
      Begin
        if ( bExp = $FF ) then
        Begin
            if ( bSig<>0 ) then
              Begin
                addFloat32Sigs := propagateFloat32NaN( a, b );
                exit;
              end;

            addFloat32Sigs := packFloat32( zSign, $FF, 0 );
            exit;
        End;
        if ( aExp = 0 ) then
          Begin
            Inc(expDiff);
          End
        else
          Begin
            aSig := aSig OR $20000000;
          End;
        shift32RightJamming( aSig, - expDiff, aSig );
        zExp := bExp;
    End
    else
    Begin
        if ( aExp = $FF ) then
        Begin
            if ( aSig OR  bSig )<> 0 then
              Begin
                addFloat32Sigs := propagateFloat32NaN( a, b );
                exit;
              end;
            addFloat32Sigs := a;
            exit;
        End;
        if ( aExp = 0 ) then
          Begin
             addFloat32Sigs := packFloat32( zSign, 0, ( aSig + bSig ) shr 6 );
             exit;
          end;
        zSig := $40000000 + aSig + bSig;
        zExp := aExp;
        goto roundAndPack;
    End;
    aSig := aSig OR $20000000;
    zSig := ( aSig + bSig ) shl 1;
    Dec(zExp);
    if ( sbits32 (zSig) < 0 ) then
      Begin
        zSig := aSig + bSig;
        Inc(zExp);
      End;
 roundAndPack:
    addFloat32Sigs := roundAndPackFloat32( zSign, zExp, zSig );
 End;

{*
-------------------------------------------------------------------------------
Returns the result of subtracting the absolute values of the single-
precision floating-point values `a' and `b'.  If `zSign' is 1, the
difference is negated before being returned.  `zSign' is ignored if the
result is a NaN.  The subtraction is performed according to the IEC/IEEE
Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function subFloat32Sigs( a:float32; b:float32; zSign:flag ): float32;
  Var
    aExp, bExp, zExp: int16;
    aSig, bSig, zSig: bits32;
    expDiff : int16;
    label aExpBigger;
    label bExpBigger;
    label aBigger;
    label bBigger;
    label normalizeRoundAndPack;
  Begin
    aSig := extractFloat32Frac( a );
    aExp := extractFloat32Exp( a );
    bSig := extractFloat32Frac( b );
    bExp := extractFloat32Exp( b );
    expDiff := aExp - bExp;
    aSig := aSig shl 7;
    bSig := bSig shl 7;
    if ( 0 < expDiff ) then goto aExpBigger;
    if ( expDiff < 0 ) then goto bExpBigger;
    if ( aExp = $FF ) then
    Begin
        if ( aSig OR  bSig )<> 0 then
          Begin
           subFloat32Sigs := propagateFloat32NaN( a, b );
           exit;
          End;
        float_raise( float_flag_invalid );
        subFloat32Sigs := float32_default_nan;
        exit;
    End;
    if ( aExp = 0 ) then
    Begin
        aExp := 1;
        bExp := 1;
    End;
    if ( bSig < aSig ) Then goto aBigger;
    if ( aSig < bSig ) Then goto bBigger;
    subFloat32Sigs := packFloat32( flag(softfloat_rounding_mode = float_round_down), 0, 0 );
    exit;
 bExpBigger:
    if ( bExp = $FF ) then
    Begin
        if ( bSig<>0 ) then
        Begin
          subFloat32Sigs := propagateFloat32NaN( a, b );
          exit;
        End;
        subFloat32Sigs := packFloat32( zSign XOR 1, $FF, 0 );
        exit;
    End;
    if ( aExp = 0 ) then
      Begin
        Inc(expDiff);
      End
    else
      Begin
        aSig := aSig OR $40000000;
      End;
    shift32RightJamming( aSig, - expDiff, aSig );
    bSig := bSig OR $40000000;
 bBigger:
    zSig := bSig - aSig;
    zExp := bExp;
    zSign := zSign xor 1;
    goto normalizeRoundAndPack;
 aExpBigger:
    if ( aExp = $FF ) then
      Begin
        if ( aSig <> 0) then
          Begin
            subFloat32Sigs := propagateFloat32NaN( a, b );
            exit;
          End;
        subFloat32Sigs := a;
        exit;
      End;
    if ( bExp = 0 ) then
      Begin
        Dec(expDiff);
      End
    else
      Begin
        bSig := bSig OR $40000000;
      End;
    shift32RightJamming( bSig, expDiff, bSig );
    aSig := aSig OR $40000000;
 aBigger:
    zSig := aSig - bSig;
    zExp := aExp;
 normalizeRoundAndPack:
    Dec(zExp);
    subFloat32Sigs := normalizeRoundAndPackFloat32( zSign, zExp, zSig );
  End;

{*
-------------------------------------------------------------------------------
Returns the result of adding the single-precision floating-point values `a'
and `b'.  The operation is performed according to the IEC/IEEE Standard for
Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_add( a: float32rec; b:float32rec ): float32rec; compilerproc;
  Var
    aSign, bSign: Flag;
  Begin
    aSign := extractFloat32Sign( a.float32 );
    bSign := extractFloat32Sign( b.float32 );
    if ( aSign = bSign ) then
      Begin
        float32_add.float32 := addFloat32Sigs( a.float32, b.float32, aSign );
      End
    else
      Begin
        float32_add.float32 := subFloat32Sigs( a.float32, b.float32, aSign );
      End;
  End;

{*
-------------------------------------------------------------------------------
Returns the result of subtracting the single-precision floating-point values
`a' and `b'.  The operation is performed according to the IEC/IEEE Standard
for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_sub( a: float32rec ; b:float32rec ): float32rec;compilerproc;
  Var
    aSign, bSign: flag;
  Begin
    aSign := extractFloat32Sign( a.float32 );
    bSign := extractFloat32Sign( b.float32 );
    if ( aSign = bSign ) then
      Begin
        float32_sub.float32 := subFloat32Sigs( a.float32, b.float32, aSign );
      End
    else
      Begin
        float32_sub.float32 := addFloat32Sigs( a.float32, b.float32, aSign );
      End;
  End;

{*
-------------------------------------------------------------------------------
Returns the result of multiplying the single-precision floating-point values
`a' and `b'.  The operation is performed according to the IEC/IEEE Standard
for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_mul(a: float32rec; b: float32rec ) : float32rec; compilerproc;

  Var
    aSign, bSign, zSign: flag;
    aExp, bExp, zExp : int16;
    aSig, bSig, zSig0, zSig1: bits32;
  Begin
    aSig := extractFloat32Frac( a.float32 );
    aExp := extractFloat32Exp( a.float32 );
    aSign := extractFloat32Sign( a.float32 );
    bSig := extractFloat32Frac( b.float32 );
    bExp := extractFloat32Exp( b.float32 );
    bSign := extractFloat32Sign( b.float32 );
    zSign := aSign xor bSign;
    if ( aExp = $FF ) then
    Begin
        if ( (aSig<>0) OR ( ( bExp = $FF ) AND  (bSig<>0) ) ) then
        Begin
            float32_mul.float32 := propagateFloat32NaN( a.float32, b.float32 );
        End;
        if ( ( bExp OR  bSig ) = 0 ) then
        Begin
            float_raise( float_flag_invalid );
            float32_mul.float32 := float32_default_nan;
            exit;
        End;
        float32_mul.float32 := packFloat32( zSign, $FF, 0 );
        exit;
    End;
    if ( bExp = $FF ) then
    Begin
        if ( bSig <> 0 ) then
        Begin
           float32_mul.float32 := propagateFloat32NaN( a.float32, b.float32 );
           exit;
        End;
        if ( ( aExp OR  aSig ) = 0 ) then
        Begin
            float_raise( float_flag_invalid );
            float32_mul.float32 := float32_default_nan;
            exit;
        End;
        float32_mul.float32 := packFloat32( zSign, $FF, 0 );
        exit;
    End;
    if ( aExp = 0 ) then
    Begin
        if ( aSig = 0 ) then
        Begin
           float32_mul.float32 := packFloat32( zSign, 0, 0 );
           exit;
        End;
        normalizeFloat32Subnormal( aSig, aExp, aSig );
    End;
    if ( bExp = 0 ) then
    Begin
        if ( bSig = 0 ) then
         Begin
           float32_mul.float32 := packFloat32( zSign, 0, 0 );
           exit;
         End;
        normalizeFloat32Subnormal( bSig, bExp, bSig );
    End;
    zExp := aExp + bExp - $7F;
    aSig := ( aSig OR  $00800000 ) shl 7;
    bSig := ( bSig OR  $00800000 ) shl 8;
    mul32To64( aSig, bSig, zSig0, zSig1 );
    zSig0 := zSig0 OR bits32( zSig1 <> 0 );
    if ( 0 <= sbits32 ( zSig0 shl 1 ) ) then
    Begin
        zSig0 := zSig0 shl 1;
        Dec(zExp);
    End;
    float32_mul.float32 := roundAndPackFloat32( zSign, zExp, zSig0 );
 End;

{*
-------------------------------------------------------------------------------
Returns the result of dividing the single-precision floating-point value `a'
by the corresponding value `b'.  The operation is performed according to the
IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_div(a: float32rec;b: float32rec ): float32rec; compilerproc;
  Var
    aSign, bSign, zSign: flag;
    aExp, bExp, zExp: int16;
    aSig, bSig, zSig, rem0, rem1, term0, term1: bits32;
  Begin
    aSig := extractFloat32Frac( a.float32 );
    aExp := extractFloat32Exp( a.float32 );
    aSign := extractFloat32Sign( a.float32 );
    bSig := extractFloat32Frac( b.float32 );
    bExp := extractFloat32Exp( b.float32 );
    bSign := extractFloat32Sign( b.float32 );
    zSign := aSign xor bSign;
    if ( aExp = $FF ) then
      Begin
        if ( aSig <> 0 ) then
        Begin
           float32_div.float32 := propagateFloat32NaN( a.float32, b.float32 );
           exit;
        End;
        if ( bExp = $FF ) then
        Begin
            if ( bSig <> 0) then
            Begin
              float32_div.float32 := propagateFloat32NaN( a.float32, b.float32 );
            End;
            float_raise( float_flag_invalid );
            float32_div.float32 := float32_default_nan;
            exit;
        End;
        float32_div.float32 := packFloat32( zSign, $FF, 0 );
        exit;
      End;
    if ( bExp = $FF ) then
    Begin
        if ( bSig <> 0) then
        Begin
          float32_div.float32 := propagateFloat32NaN( a.float32, b.float32 );
          exit;
        End;
        float32_div.float32 := packFloat32( zSign, 0, 0 );
        exit;
    End;
    if ( bExp = 0 ) Then
    Begin
        if ( bSig = 0 ) Then
        Begin
            if ( ( aExp OR  aSig ) = 0 ) then
            Begin
                float_raise( float_flag_invalid );
                float32_div.float32 := float32_default_nan;
                exit;
            End;
            float_raise( float_flag_divbyzero );
            float32_div.float32 := packFloat32( zSign, $FF, 0 );
            exit;
        End;
        normalizeFloat32Subnormal( bSig, bExp, bSig );
    End;
    if ( aExp = 0 ) Then
    Begin
        if ( aSig = 0 ) Then
        Begin
          float32_div.float32 := packFloat32( zSign, 0, 0 );
          exit;
        End;
        normalizeFloat32Subnormal( aSig, aExp, aSig );
    End;
    zExp := aExp - bExp + $7D;
    aSig := ( aSig OR  $00800000 ) shl 7;
    bSig := ( bSig OR  $00800000 ) shl 8;
    if ( bSig <= ( aSig + aSig ) ) then
    Begin
        aSig := aSig shr 1;
        Inc(zExp);
    End;
    zSig := estimateDiv64To32( aSig, 0, bSig );
    if ( ( zSig and $3F ) <= 2 ) then
    Begin
        mul32To64( bSig, zSig, term0, term1 );
        sub64( aSig, 0, term0, term1, rem0, rem1 );
        while ( sbits32 (rem0) < 0 ) do
        Begin
            Dec(zSig);
            add64( rem0, rem1, 0, bSig, rem0, rem1 );
        End;
        zSig := zSig or bits32( rem1 <> 0 );
    End;
    float32_div.float32 := roundAndPackFloat32( zSign, zExp, zSig );

  End;

{*
-------------------------------------------------------------------------------
Returns the remainder of the single-precision floating-point value `a'
with respect to the corresponding value `b'.  The operation is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_rem(a: float32rec; b: float32rec ):float32rec; compilerproc;
  Var
    aSign, bSign, zSign: flag;
    aExp, bExp, expDiff: int16;
    aSig, bSig, q, allZero, alternateASig: bits32;
    sigMean: sbits32;
  Begin
    aSig := extractFloat32Frac( a.float32 );
    aExp := extractFloat32Exp( a.float32 );
    aSign := extractFloat32Sign( a.float32 );
    bSig := extractFloat32Frac( b.float32 );
    bExp := extractFloat32Exp( b.float32 );
    bSign := extractFloat32Sign( b.float32 );
    if ( aExp = $FF ) then
    Begin
        if ( (aSig<>0) OR ( ( bExp = $FF ) AND  (bSig <>0)) ) then
        Begin
            float32_rem.float32 := propagateFloat32NaN( a.float32, b.float32 );
            exit;
        End;
        float_raise( float_flag_invalid );
        float32_rem.float32 := float32_default_nan;
        exit;
    End;
    if ( bExp = $FF ) then
    Begin
        if ( bSig <> 0 ) then
        Begin
          float32_rem.float32 := propagateFloat32NaN( a.float32, b.float32 );
          exit;
        End;
        float32_rem := a;
        exit;
    End;
    if ( bExp = 0 ) then
    Begin
        if ( bSig = 0 ) then
        Begin
            float_raise( float_flag_invalid );
            float32_rem.float32 := float32_default_nan;
            exit;
        End;
        normalizeFloat32Subnormal( bSig, bExp, bSig );
    End;
    if ( aExp = 0 ) then
    Begin
        if ( aSig = 0 ) then
        Begin
           float32_rem := a;
           exit;
        End;
        normalizeFloat32Subnormal( aSig, aExp, aSig );
    End;
    expDiff := aExp - bExp;
    aSig := ( aSig OR  $00800000 ) shl 8;
    bSig := ( bSig OR  $00800000 ) shl 8;
    if ( expDiff < 0 ) then
    Begin
        if ( expDiff < -1 ) then
        Begin
           float32_rem := a;
           exit;
        End;
        aSig := aSig shr 1;
    End;
    q := bits32( bSig <= aSig );
    if ( q <> 0) then
       aSig := aSig - bSig;
    expDiff := expDiff - 32;
    while ( 0 < expDiff ) do
    Begin
        q := estimateDiv64To32( aSig, 0, bSig );
        if (2 < q) then
         q := q - 2
        else
         q := 0;
        aSig := - ( ( bSig shr 2 ) * q );
        expDiff := expDiff - 30;
    End;
    expDiff := expDiff + 32;
    if ( 0 < expDiff ) then
    Begin
        q := estimateDiv64To32( aSig, 0, bSig );
        if (2 < q) then
         q := q - 2
        else
         q := 0;
        q := q shr (32 - expDiff);
        bSig := bSig shr 2;
        aSig := ( ( aSig shr 1 ) shl ( expDiff - 1 ) ) - bSig * q;
    End
    else
    Begin
        aSig := aSig shr 2;
        bSig := bSig shr 2;
    End;
    Repeat
        alternateASig := aSig;
        Inc(q);
        aSig := aSig - bSig;
    Until not ( 0 <= sbits32 (aSig) );
    sigMean := aSig + alternateASig;
    if ( ( sigMean < 0 ) OR ( ( sigMean = 0 ) AND  (( q and 1 )<>0) ) ) then
    Begin
        aSig := alternateASig;
    End;
    zSign := flag( sbits32 (aSig) < 0 );
    if ( zSign<>0 ) then
      aSig := - aSig;
    float32_rem.float32 := normalizeRoundAndPackFloat32( aSign xor zSign, bExp, aSig );
  End;

{*
-------------------------------------------------------------------------------
Returns the square root of the single-precision floating-point value `a'.
The operation is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_sqrt(a: float32rec ): float32rec;compilerproc;
Var
    aSign : flag;
    aExp, zExp : int16;
    aSig, zSig, rem0, rem1, term0, term1: bits32;
    label roundAndPack;
Begin
    aSig := extractFloat32Frac( a.float32 );
    aExp := extractFloat32Exp( a.float32 );
    aSign := extractFloat32Sign( a.float32 );
    if ( aExp = $FF ) then
    Begin
        if ( aSig <> 0) then
        Begin
           float32_sqrt.float32 := propagateFloat32NaN( a.float32, 0 );
           exit;
        End;
        if ( aSign = 0) then
        Begin
          float32_sqrt := a;
          exit;
        End;
        float_raise( float_flag_invalid );
        float32_sqrt.float32 := float32_default_nan;
        exit;
    End;
    if ( aSign <> 0) then
    Begin
        if ( ( aExp OR  aSig ) = 0 ) then
        Begin
           float32_sqrt := a;
           exit;
        End;
        float_raise( float_flag_invalid );
        float32_sqrt.float32 := float32_default_nan;
        exit;
    End;
    if ( aExp = 0 ) then
    Begin
        if ( aSig = 0 ) then
        Begin
           float32_sqrt.float32 := 0;
           exit;
        End;
        normalizeFloat32Subnormal( aSig, aExp, aSig );
    End;
    zExp := ( ( aExp - $7F ) shr 1 ) + $7E;
    aSig := ( aSig OR  $00800000 ) shl 8;
    zSig := estimateSqrt32( aExp, aSig ) + 2;
    if ( ( zSig and $7F ) <= 5 ) then
    Begin
        if ( zSig < 2 ) then
        Begin
            zSig := $7FFFFFFF;
            goto roundAndPack;
        End
        else
        Begin
            aSig  := aSig shr (aExp and 1);
            mul32To64( zSig, zSig, term0, term1 );
            sub64( aSig, 0, term0, term1, rem0, rem1 );
            while ( sbits32 (rem0) < 0 ) do
            Begin
                Dec(zSig);
                shortShift64Left( 0, zSig, 1, term0, term1 );
                term1 := term1 or 1;
                add64( rem0, rem1, term0, term1, rem0, rem1 );
            End;
            zSig := zSig OR bits32( ( rem0 OR  rem1 ) <> 0 );
        End;
    End;
    shift32RightJamming( zSig, 1, zSig );
 roundAndPack:
    float32_sqrt.float32 := roundAndPackFloat32( 0, zExp, zSig );
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is equal to
the corresponding value `b', and 0 otherwise.  The comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_eq( a:float32rec; b:float32rec): flag; compilerproc;
Begin
    if ((( extractFloat32Exp( a.float32 ) = $FF ) AND  (extractFloat32Frac( a.float32 )<>0))
         OR ( ( extractFloat32Exp( b.float32 ) = $FF ) AND  (extractFloat32Frac( b.float32 )<>0) )
       ) then
    Begin
        if ( (float32_is_signaling_nan( a.float32 )<>0) OR (float32_is_signaling_nan( b.float32 )<>0) ) then
        Begin
            float_raise( float_flag_invalid );
        End;
        float32_eq := 0;
        exit;
    End;
    float32_eq := flag( a.float32 = b.float32 ) OR flag( bits32 ( ( a.float32 OR  b.float32 ) shl 1 ) = 0 );
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is less than
or equal to the corresponding value `b', and 0 otherwise.  The comparison
is performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_le( a: float32rec; b : float32rec ):flag;compilerproc;
var
    aSign, bSign: flag;
Begin

    if (    ( ( extractFloat32Exp( a.float32 ) = $FF ) AND  (extractFloat32Frac( a.float32 )<>0) )
         OR ( ( extractFloat32Exp( b.float32 ) = $FF ) AND  (extractFloat32Frac( b.float32 )<>0) )
       ) then
    Begin
        float_raise( float_flag_invalid );
        float32_le := 0;
        exit;
    End;
    aSign := extractFloat32Sign( a.float32 );
    bSign := extractFloat32Sign( b.float32 );
    if ( aSign <> bSign ) then
    Begin
       float32_le :=  aSign OR flag( bits32 ( ( a.float32 OR  b.float32 ) shl 1 ) = 0 );
       exit;
    End;
    float32_le := flag(flag( a.float32 = b.float32 ) OR flag( aSign xor flag( a.float32 < b.float32 ) ));

End;

{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is less than
the corresponding value `b', and 0 otherwise.  The comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_lt( a:float32rec ; b : float32rec): flag; compilerproc;
var
    aSign, bSign: flag;
Begin

    if (    ( ( extractFloat32Exp( a.float32 ) = $FF ) AND  (extractFloat32Frac( a.float32 ) <>0))
         OR ( ( extractFloat32Exp( b.float32 ) = $FF ) AND  (extractFloat32Frac( b.float32 ) <>0) )
       ) then
    Begin
        float_raise( float_flag_invalid );
        float32_lt :=0;
        exit;
    End;
    aSign := extractFloat32Sign( a.float32 );
    bSign := extractFloat32Sign( b.float32 );
    if ( aSign <> bSign ) then
    Begin
       float32_lt := aSign AND  flag( bits32 ( ( a.float32 OR  b.float32 ) shl 1 ) <> 0 );
       exit;
    End;
    float32_lt := flag(flag( a.float32 <> b.float32 ) AND  flag( aSign xor flag( a.float32 < b.float32 ) ));

End;

{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is equal to
the corresponding value `b', and 0 otherwise.  The invalid exception is
raised if either operand is a NaN.  Otherwise, the comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_eq_signaling( a: float32; b: float32) : flag;
Begin

    if (    ( ( extractFloat32Exp( a ) = $FF ) AND  (extractFloat32Frac( a ) <> 0))
         OR ( ( extractFloat32Exp( b ) = $FF ) AND  (extractFloat32Frac( b ) <> 0))
       ) then
    Begin
        float_raise( float_flag_invalid );
        float32_eq_signaling := 0;
        exit;
    End;
    float32_eq_signaling := (flag( a = b ) OR flag( bits32 ( ( a OR  b ) shl 1 ) = 0 ));
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is less than or
equal to the corresponding value `b', and 0 otherwise.  Quiet NaNs do not
cause an exception.  Otherwise, the comparison is performed according to the
IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_le_quiet( a: float32 ; b : float32 ): flag;
Var
    aSign, bSign: flag;
    aExp, bExp: int16;
Begin
    if (    ( ( extractFloat32Exp( a ) = $FF ) AND  (extractFloat32Frac( a )<>0) )
         OR ( ( extractFloat32Exp( b ) = $FF ) AND  (extractFloat32Frac( b )<>0) )
       ) then
    Begin
        if ( (float32_is_signaling_nan( a )<>0) OR (float32_is_signaling_nan( b )<>0) ) then
        Begin
            float_raise( float_flag_invalid );
        End;
        float32_le_quiet := 0;
        exit;
    End;
    aSign := extractFloat32Sign( a );
    bSign := extractFloat32Sign( b );
    if ( aSign <> bSign ) then
    Begin
       float32_le_quiet := aSign OR flag( bits32 ( ( a OR  b ) shl 1 ) = 0 );
       exit;
    End;
    float32_le_quiet := flag(flag( a = b ) OR flag( aSign xor flag( a < b ) ));
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is less than
the corresponding value `b', and 0 otherwise.  Quiet NaNs do not cause an
exception.  Otherwise, the comparison is performed according to the IEC/IEEE
Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_lt_quiet( a: float32 ; b: float32 ): flag;
Var
   aSign, bSign: flag;
Begin
    if (    ( ( extractFloat32Exp( a ) = $FF ) AND  (extractFloat32Frac( a )<>0) )
         OR ( ( extractFloat32Exp( b ) = $FF ) AND  (extractFloat32Frac( b )<>0) )
       ) then
    Begin
        if ( (float32_is_signaling_nan( a )<>0) OR (float32_is_signaling_nan( b )<>0) ) then
        Begin
            float_raise( float_flag_invalid );
        End;
        float32_lt_quiet := 0;
        exit;
    End;
    aSign := extractFloat32Sign( a );
    bSign := extractFloat32Sign( b );
    if ( aSign <> bSign ) then
    Begin
        float32_lt_quiet := aSign AND  flag( bits32 ( ( a OR  b ) shl 1 ) <> 0 );
        exit;
    End;
    float32_lt_quiet := flag(flag( a <> b ) AND  ( aSign xor flag( a < b ) ));
End;

{*
-------------------------------------------------------------------------------
Returns the result of converting the double-precision floating-point value
`a' to the 32-bit two's complement integer format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic---which means in particular that the conversion is rounded
according to the current rounding mode.  If `a' is a NaN, the largest
positive integer is returned.  Otherwise, if the conversion overflows, the
largest integer with the same sign as `a' is returned.
-------------------------------------------------------------------------------
*}
Function float64_to_int32(a: float64): int32;{$ifdef fpc} [public,Alias:'FLOAT64_TO_INT32'];compilerproc;{$endif}
var
    aSign: flag;
    aExp, shiftCount: int16;
    aSig0, aSig1, absZ, aSigExtra: bits32;
    z: int32;
    roundingMode: int8;
    label invalid;
Begin
    aSig1 := extractFloat64Frac1( a );
    aSig0 := extractFloat64Frac0( a );
    aExp := extractFloat64Exp( a );
    aSign := extractFloat64Sign( a );
    shiftCount := aExp - $413;
    if ( 0 <= shiftCount ) then
    Begin
        if ( $41E < aExp ) then
        Begin
            if ( ( aExp = $7FF ) AND  (( aSig0 OR  aSig1 )<>0) ) then
               aSign := 0;
            goto invalid;
        End;
        shortShift64Left(
            aSig0 OR  $00100000, aSig1, shiftCount, absZ, aSigExtra );
        if ( $80000000 < absZ ) then
          goto invalid;
    End
    else
    Begin
        aSig1 := flag( aSig1 <> 0 );
        if ( aExp < $3FE ) then
        Begin
            aSigExtra := aExp OR  aSig0 OR  aSig1;
            absZ := 0;
        End
        else
        Begin
            aSig0 := aSig0 OR $00100000;
            aSigExtra := ( aSig0 shl ( shiftCount and 31 ) ) OR  aSig1;
            absZ := aSig0 shr ( - shiftCount );
        End;
    End;
    roundingMode := softfloat_rounding_mode;
    if ( roundingMode = float_round_nearest_even ) then
    Begin
        if ( sbits32(aSigExtra) < 0 ) then
        Begin
            Inc(absZ);
            if ( bits32 ( aSigExtra shl 1 ) = 0 ) then
               absZ :=  absZ and not 1;
        End;
        if aSign <> 0 then
          z := - absZ
        else
          z := absZ;
    End
    else
    Begin
        aSigExtra := bits32( aSigExtra <> 0 );
        if ( aSign <> 0) then
        Begin
            z := - (   absZ
                    + ( int32( roundingMode = float_round_down ) and aSigExtra ) );
        End
        else
        Begin
            z := absZ + ( int32( roundingMode = float_round_up ) and aSigExtra );
        End
    End;
    if ( (( aSign xor flag( z < 0 ) )<>0) AND  (z<>0) ) then
    Begin
 invalid:
        float_raise( float_flag_invalid );
        if (aSign <> 0 ) then
          float64_to_int32 := sbits32 ($80000000)
        else
          float64_to_int32 :=  $7FFFFFFF;
        exit;
    End;
    if ( aSigExtra <> 0) then
       softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
    float64_to_int32 := z;
End;


{*
-------------------------------------------------------------------------------
Returns the result of converting the double-precision floating-point value
`a' to the 32-bit two's complement integer format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic, except that the conversion is always rounded toward zero.
If `a' is a NaN, the largest positive integer is returned.  Otherwise, if
the conversion overflows, the largest integer with the same sign as `a' is
returned.
-------------------------------------------------------------------------------
*}
Function float64_to_int32_round_to_zero(a: float64 ): int32;
{$ifdef fpc} [public,Alias:'FLOAT64_TO_INT32_ROUND_TO_ZERO'];compilerproc;{$endif}
Var
    aSign: flag;
    aExp, shiftCount: int16;
    aSig0, aSig1, absZ, aSigExtra: bits32;
    z: int32;
    label invalid;
 Begin
    aSig1 := extractFloat64Frac1( a );
    aSig0 := extractFloat64Frac0( a );
    aExp := extractFloat64Exp( a );
    aSign := extractFloat64Sign( a );
    shiftCount := aExp - $413;
    if ( 0 <= shiftCount ) then
    Begin
        if ( $41E < aExp ) then
        Begin
            if ( ( aExp = $7FF ) AND  (( aSig0 OR  aSig1 )<>0) ) then
               aSign := 0;
            goto invalid;
        End;
        shortShift64Left(
            aSig0 OR  $00100000, aSig1, shiftCount, absZ, aSigExtra );
    End
    else
    Begin
        if ( aExp < $3FF ) then
        Begin
            if ( aExp OR  aSig0 OR  aSig1 )<>0 then
            Begin
                softfloat_exception_flags :=
                  softfloat_exception_flags or float_flag_inexact;
            End;
            float64_to_int32_round_to_zero := 0;
            exit;
        End;
        aSig0 := aSig0 or $00100000;
        aSigExtra := ( aSig0 shl ( shiftCount and 31 ) ) OR  aSig1;
        absZ := aSig0 shr ( - shiftCount );
    End;
    if aSign <> 0 then
      z := - absZ
    else
      z := absZ;
    if ( (( aSign xor flag( z < 0 )) <> 0) AND  (z<>0) ) then
    Begin
 invalid:
        float_raise( float_flag_invalid );
        if (aSign <> 0) then
          float64_to_int32_round_to_zero := sbits32 ($80000000)
        else
          float64_to_int32_round_to_zero :=  $7FFFFFFF;
        exit;
    End;
    if ( aSigExtra <> 0) then
       softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
    float64_to_int32_round_to_zero := z;
 End;

{*
-------------------------------------------------------------------------------
Returns the result of converting the double-precision floating-point value
`a' to the single-precision floating-point format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_to_float32(a: float64 ): float32rec;compilerproc;
Var
    aSign: flag;
    aExp: int16;
    aSig0, aSig1, zSig: bits32;
    allZero: bits32;
    tmp : CommonNanT;
Begin
    aSig1 := extractFloat64Frac1( a );
    aSig0 := extractFloat64Frac0( a );
    aExp := extractFloat64Exp( a );
    aSign := extractFloat64Sign( a );
    if ( aExp = $7FF ) then
    Begin
        if ( aSig0 OR  aSig1 ) <> 0 then
        Begin
            float64ToCommonNaN( a, tmp );
            float64_to_float32.float32 := commonNaNToFloat32( tmp );
            exit;
        End;
        float64_to_float32.float32 := packFloat32( aSign, $FF, 0 );
        exit;
    End;
    shift64RightJamming( aSig0, aSig1, 22, allZero, zSig );
    if ( aExp <> 0) then
      zSig := zSig OR $40000000;
    float64_to_float32.float32 := roundAndPackFloat32( aSign, aExp - $381, zSig );
End;

{*
-------------------------------------------------------------------------------
Rounds the double-precision floating-point value `a' to an integer,
and returns the result as a double-precision floating-point value.  The
operation is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
function float64_round_to_int(a: float64) : Float64;{$ifdef fpc} [public,Alias:'FLOAT64_ROUND_TO_INT'];compilerproc;{$endif}

Var
    aSign: flag;
    aExp: int16;
    lastBitMask, roundBitsMask: bits32;
    roundingMode: int8;
    z: float64;
Begin
    aExp := extractFloat64Exp( a );
    if ( $413 <= aExp ) then
    Begin
        if ( $433 <= aExp ) then
        Begin
            if (    ( aExp = $7FF )
                 AND
            (
            ( extractFloat64Frac0( a ) OR  extractFloat64Frac1( a )
            ) <>0)
            )  then
            Begin
                propagateFloat64NaN( a, a, result );
                exit;
            End;
            result := a;
            exit;
        End;
        lastBitMask := 1;
        lastBitMask := ( lastBitMask shl ( $432 - aExp ) ) shl 1;
        roundBitsMask := lastBitMask - 1;
        z := a;
        roundingMode := softfloat_rounding_mode;
        if ( roundingMode = float_round_nearest_even ) then
        Begin
            if ( lastBitMask <> 0) then
            Begin
                add64( z.high, z.low, 0, lastBitMask shr 1, z.high, z.low );
                if ( ( z.low and roundBitsMask ) = 0 ) then
                   z.low := z.low and not lastBitMask;
            End
            else
            Begin
                if ( sbits32 (z.low) < 0 ) then
                Begin
                    Inc(z.high);
                    if ( bits32 ( z.low shl 1 ) = 0 ) then
                      z.high := z.high and not 1;
                End;
            End;
        End
        else if ( roundingMode <> float_round_to_zero ) then
        Begin
            if (   extractFloat64Sign( z )
                 xor flag( roundingMode = float_round_up ) )<> 0 then
            Begin
                add64( z.high, z.low, 0, roundBitsMask, z.high, z.low );
            End;
        End;
        z.low := z.low and not roundBitsMask;
    End
    else
    Begin
        if ( aExp <= $3FE ) then
        Begin
            if ( ( ( bits32 ( a.high shl 1 ) ) OR  a.low ) = 0 ) then
            Begin
                result := a;
                exit;
            End;
            softfloat_exception_flags := softfloat_exception_flags or
               float_flag_inexact;
            aSign := extractFloat64Sign( a );
            case ( softfloat_rounding_mode ) of
             float_round_nearest_even:
               Begin
                if (    ( aExp = $3FE )
                     AND  ( (extractFloat64Frac0( a ) OR  extractFloat64Frac1( a ) )<>0)
                   ) then
                Begin
                    packFloat64( aSign, $3FF, 0, 0, result );
                    exit;
                End;

               End;
               float_round_down:
                Begin
                  if aSign<>0 then
                   packFloat64( 1, $3FF, 0, 0, result )
                  else
                   packFloat64( 0, 0, 0, 0, result );
                  exit;
                End;
             float_round_up:
                Begin
                  if aSign <> 0 then
                   packFloat64( 1, 0, 0, 0, result )
                  else
                   packFloat64( 0, $3FF, 0, 0, result );
                  exit;
                End;
            end;
            packFloat64( aSign, 0, 0, 0, result );
            exit;
        End;
        lastBitMask := 1;
        lastBitMask := lastBitMask shl ($413 - aExp);
        roundBitsMask := lastBitMask - 1;
        z.low := 0;
        z.high := a.high;
        roundingMode := softfloat_rounding_mode;
        if ( roundingMode = float_round_nearest_even ) then
        Begin
            z.high := z.high + lastBitMask shr 1;
            if ( ( ( z.high and roundBitsMask ) OR  a.low ) = 0 ) then
            Begin
                z.high := z.high and not lastBitMask;
            End;
        End
        else if ( roundingMode <> float_round_to_zero ) then
        Begin
            if (   extractFloat64Sign( z )
                 xor flag( roundingMode = float_round_up ) )<> 0 then
            Begin
                z.high := z.high or bits32( a.low <> 0 );
                z.high := z.high + roundBitsMask;
            End;
        End;
        z.high := z.high and not roundBitsMask;
    End;
    if ( ( z.low <> a.low ) OR ( z.high <> a.high ) ) then
    Begin
        softfloat_exception_flags :=
          softfloat_exception_flags or float_flag_inexact;
    End;
    result := z;
End;


{*
-------------------------------------------------------------------------------
Returns the result of adding the absolute values of the double-precision
floating-point values `a' and `b'.  If `zSign' is 1, the sum is negated
before being returned.  `zSign' is ignored if the result is a NaN.
The addition is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Procedure addFloat64Sigs( a:float64 ; b: float64 ; zSign:flag; Var out: float64 );
Var
    aExp, bExp, zExp: int16;
    aSig0, aSig1, bSig0, bSig1, zSig0, zSig1, zSig2: bits32;
    expDiff: int16;
    label shiftRight1;
    label roundAndPack;
Begin
    aSig1 := extractFloat64Frac1( a );
    aSig0 := extractFloat64Frac0( a );
    aExp := extractFloat64Exp( a );
    bSig1 := extractFloat64Frac1( b );
    bSig0 := extractFloat64Frac0( b );
    bExp := extractFloat64Exp( b );
    expDiff := aExp - bExp;
    if ( 0 < expDiff ) then
    Begin
        if ( aExp = $7FF ) then
        Begin
            if ( aSig0 OR  aSig1 ) <> 0 then
            Begin
              propagateFloat64NaN( a, b, out );
              exit;
            end;
            out := a;
            exit;
        End;
        if ( bExp = 0 ) then
        Begin
            Dec(expDiff);
        End
        else
        Begin
            bSig0 := bSig0 or $00100000;
        End;
        shift64ExtraRightJamming(
            bSig0, bSig1, 0, expDiff, bSig0, bSig1, zSig2 );
        zExp := aExp;
    End
    else if ( expDiff < 0 ) then
    Begin
        if ( bExp = $7FF ) then
        Begin
            if ( bSig0 OR  bSig1 ) <> 0 then
            Begin
               propagateFloat64NaN( a, b, out );
               exit;
            End;
            packFloat64( zSign, $7FF, 0, 0, out );
        End;
        if ( aExp = 0 ) then
        Begin
            Inc(expDiff);
        End
        else
        Begin
            aSig0 := aSig0 or $00100000;
        End;
        shift64ExtraRightJamming(
            aSig0, aSig1, 0, - expDiff, aSig0, aSig1, zSig2 );
        zExp := bExp;
    End
    else
    Begin
        if ( aExp = $7FF ) then
        Begin
            if ( aSig0 OR  aSig1 OR  bSig0 OR  bSig1 ) <> 0 then
            Begin
                propagateFloat64NaN( a, b, out );
                exit;
            End;
            out := a;
            exit;
        End;
        add64( aSig0, aSig1, bSig0, bSig1, zSig0, zSig1 );
        if ( aExp = 0 ) then
        Begin
           packFloat64( zSign, 0, zSig0, zSig1, out );
           exit;
        End;
        zSig2 := 0;
        zSig0 := zSig0 or $00200000;
        zExp := aExp;
        goto shiftRight1;
    End;
    aSig0 := aSig0 or $00100000;
    add64( aSig0, aSig1, bSig0, bSig1, zSig0, zSig1 );
    Dec(zExp);
    if ( zSig0 < $00200000 ) then
       goto roundAndPack;
    Inc(zExp);
 shiftRight1:
    shift64ExtraRightJamming( zSig0, zSig1, zSig2, 1, zSig0, zSig1, zSig2 );
 roundAndPack:
    roundAndPackFloat64( zSign, zExp, zSig0, zSig1, zSig2, out );

End;

{*
-------------------------------------------------------------------------------
Returns the result of subtracting the absolute values of the double-
precision floating-point values `a' and `b'.  If `zSign' is 1, the
difference is negated before being returned.  `zSign' is ignored if the
result is a NaN.  The subtraction is performed according to the IEC/IEEE
Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Procedure subFloat64Sigs( a:float64; b: float64 ; zSign:flag; Var out: float64 );
Var
    aExp, bExp, zExp: int16;
    aSig0, aSig1, bSig0, bSig1, zSig0, zSig1: bits32;
    expDiff: int16;
    z: float64;
    label aExpBigger;
    label bExpBigger;
    label aBigger;
    label bBigger;
    label normalizeRoundAndPack;
Begin
    aSig1 := extractFloat64Frac1( a );
    aSig0 := extractFloat64Frac0( a );
    aExp := extractFloat64Exp( a );
    bSig1 := extractFloat64Frac1( b );
    bSig0 := extractFloat64Frac0( b );
    bExp := extractFloat64Exp( b );
    expDiff := aExp - bExp;
    shortShift64Left( aSig0, aSig1, 10, aSig0, aSig1 );
    shortShift64Left( bSig0, bSig1, 10, bSig0, bSig1 );
    if ( 0 < expDiff ) then goto aExpBigger;
    if ( expDiff < 0 ) then goto bExpBigger;
    if ( aExp = $7FF ) then
    Begin
        if ( aSig0 OR  aSig1 OR  bSig0 OR  bSig1 ) <> 0 then
        Begin
            propagateFloat64NaN( a, b, out );
            exit;
        End;
        float_raise( float_flag_invalid );
        z.low := float64_default_nan_low;
        z.high := float64_default_nan_high;
        out := z;
        exit;
    End;
    if ( aExp = 0 ) then
    Begin
        aExp := 1;
        bExp := 1;
    End;
    if ( bSig0 < aSig0 ) then goto aBigger;
    if ( aSig0 < bSig0 ) then goto bBigger;
    if ( bSig1 < aSig1 ) then goto aBigger;
    if ( aSig1 < bSig1 ) then goto bBigger;
    packFloat64( flag(softfloat_rounding_mode = float_round_down), 0, 0, 0 , out);
    exit;
 bExpBigger:
    if ( bExp = $7FF ) then
    Begin
        if ( bSig0 OR  bSig1 ) <> 0 then
        Begin
           propagateFloat64NaN( a, b, out );
           exit;
        End;
        packFloat64( zSign xor 1, $7FF, 0, 0, out );
        exit;
    End;
    if ( aExp = 0 ) then
    Begin
        Inc(expDiff);
    End
    else
    Begin
        aSig0 := aSig0 or $40000000;
    End;
    shift64RightJamming( aSig0, aSig1, - expDiff, aSig0, aSig1 );
    bSig0 := bSig0 or $40000000;
 bBigger:
    sub64( bSig0, bSig1, aSig0, aSig1, zSig0, zSig1 );
    zExp := bExp;
    zSign := zSign xor 1;
    goto normalizeRoundAndPack;
 aExpBigger:
    if ( aExp = $7FF ) then
    Begin
        if ( aSig0 OR  aSig1 ) <> 0 then
        Begin
           propagateFloat64NaN( a, b, out );
           exit;
        End;
        out :=  a;
        exit;
    End;
    if ( bExp = 0 ) then
    Begin
        Dec(expDiff);
    End
    else
    Begin
        bSig0 := bSig0 or $40000000;
    End;
    shift64RightJamming( bSig0, bSig1, expDiff, bSig0, bSig1 );
    aSig0 := aSig0 or $40000000;
 aBigger:
    sub64( aSig0, aSig1, bSig0, bSig1, zSig0, zSig1 );
    zExp := aExp;
 normalizeRoundAndPack:
    Dec(zExp);
    normalizeRoundAndPackFloat64( zSign, zExp - 10, zSig0, zSig1, out );

End;

{*
-------------------------------------------------------------------------------
Returns the result of adding the double-precision floating-point values `a'
and `b'.  The operation is performed according to the IEC/IEEE Standard for
Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_add( a: float64; b : float64) : Float64;
{$ifdef fpc}[public,Alias:'FLOAT64_ADD'];compilerproc;{$endif}
Var
    aSign, bSign: flag;
Begin
    aSign := extractFloat64Sign( a );
    bSign := extractFloat64Sign( b );
    if ( aSign = bSign ) then
    Begin
         addFloat64Sigs( a, b, aSign, result );
    End
    else
    Begin
        subFloat64Sigs( a, b, aSign, result );
    End;
End;

{*
-------------------------------------------------------------------------------
Returns the result of subtracting the double-precision floating-point values
`a' and `b'.  The operation is performed according to the IEC/IEEE Standard
for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_sub(a: float64; b : float64) : Float64;
{$ifdef fpc}[public,Alias:'FLOAT64_SUB'];compilerproc;{$endif}
Var
    aSign, bSign: flag;
Begin
    aSign := extractFloat64Sign( a );
    bSign := extractFloat64Sign( b );
    if ( aSign = bSign ) then
    Begin
        subFloat64Sigs( a, b, aSign, result );
    End
    else
    Begin
        addFloat64Sigs( a, b, aSign, result );
    End;
End;

{*
-------------------------------------------------------------------------------
Returns the result of multiplying the double-precision floating-point values
`a' and `b'.  The operation is performed according to the IEC/IEEE Standard
for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_mul( a: float64; b:float64) : Float64;
{$ifdef fpc}[public,Alias:'FLOAT64_MUL'];compilerproc;{$endif}
Var
    aSign, bSign, zSign: flag;
    aExp, bExp, zExp: int16;
    aSig0, aSig1, bSig0, bSig1, zSig0, zSig1, zSig2, zSig3: bits32;
    z: float64;
    label invalid;
Begin
    aSig1 := extractFloat64Frac1( a );
    aSig0 := extractFloat64Frac0( a );
    aExp := extractFloat64Exp( a );
    aSign := extractFloat64Sign( a );
    bSig1 := extractFloat64Frac1( b );
    bSig0 := extractFloat64Frac0( b );
    bExp := extractFloat64Exp( b );
    bSign := extractFloat64Sign( b );
    zSign := aSign xor bSign;
    if ( aExp = $7FF ) then
    Begin
        if (    (( aSig0 OR  aSig1 ) <>0)
             OR ( ( bExp = $7FF ) AND  (( bSig0 OR  bSig1 )<>0) ) ) then
        Begin
            propagateFloat64NaN( a, b, result );
            exit;
        End;
        if ( ( bExp OR  bSig0 OR  bSig1 ) = 0 ) then goto invalid;
        packFloat64( zSign, $7FF, 0, 0, result );
        exit;
    End;
    if ( bExp = $7FF ) then
    Begin
        if ( bSig0 OR  bSig1 )<> 0 then
        Begin
          propagateFloat64NaN( a, b, result );
          exit;
        End;
        if ( ( aExp OR  aSig0 OR  aSig1 ) = 0 ) then
        Begin
 invalid:
            float_raise( float_flag_invalid );
            z.low := float64_default_nan_low;
            z.high := float64_default_nan_high;
            result := z;
            exit;
        End;
        packFloat64( zSign, $7FF, 0, 0, result );
        exit;
    End;
    if ( aExp = 0 ) then
    Begin
        if ( ( aSig0 OR  aSig1 ) = 0 ) then
        Begin
           packFloat64( zSign, 0, 0, 0, result );
           exit;
        End;
        normalizeFloat64Subnormal( aSig0, aSig1, aExp, aSig0, aSig1 );
    End;
    if ( bExp = 0 ) then
    Begin
        if ( ( bSig0 OR  bSig1 ) = 0 ) then
        Begin
          packFloat64( zSign, 0, 0, 0, result );
          exit;
        End;
        normalizeFloat64Subnormal( bSig0, bSig1, bExp, bSig0, bSig1 );
    End;
    zExp := aExp + bExp - $400;
    aSig0 := aSig0 or $00100000;
    shortShift64Left( bSig0, bSig1, 12, bSig0, bSig1 );
    mul64To128( aSig0, aSig1, bSig0, bSig1, zSig0, zSig1, zSig2, zSig3 );
    add64( zSig0, zSig1, aSig0, aSig1, zSig0, zSig1 );
    zSig2 := zSig2 or flag( zSig3 <> 0 );
    if ( $00200000 <= zSig0 ) then
    Begin
        shift64ExtraRightJamming(
            zSig0, zSig1, zSig2, 1, zSig0, zSig1, zSig2 );
        Inc(zExp);
    End;
    roundAndPackFloat64( zSign, zExp, zSig0, zSig1, zSig2, result );
End;

{*
-------------------------------------------------------------------------------
Returns the result of dividing the double-precision floating-point value `a'
by the corresponding value `b'.  The operation is performed according to the
IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_div(a: float64; b : float64) : Float64;
{$ifdef fpc}[public,Alias:'FLOAT64_DIV'];compilerproc;{$endif}
Var
    aSign, bSign, zSign: flag;
    aExp, bExp, zExp: int16;
    aSig0, aSig1, bSig0, bSig1, zSig0, zSig1, zSig2: bits32;
    rem0, rem1, rem2, rem3, term0, term1, term2, term3: bits32;
    z: float64;
    label invalid;
Begin
    aSig1 := extractFloat64Frac1( a );
    aSig0 := extractFloat64Frac0( a );
    aExp := extractFloat64Exp( a );
    aSign := extractFloat64Sign( a );
    bSig1 := extractFloat64Frac1( b );
    bSig0 := extractFloat64Frac0( b );
    bExp := extractFloat64Exp( b );
    bSign := extractFloat64Sign( b );
    zSign := aSign xor bSign;
    if ( aExp = $7FF ) then
    Begin
        if ( aSig0 OR  aSig1 )<> 0 then
        Begin
           propagateFloat64NaN( a, b, result );
           exit;
        end;
        if ( bExp = $7FF ) then
        Begin
            if ( bSig0 OR  bSig1 )<>0 then
            Begin
               propagateFloat64NaN( a, b, result );
               exit;
            End;
            goto invalid;
        End;
        packFloat64( zSign, $7FF, 0, 0, result );
        exit;
    End;
    if ( bExp = $7FF ) then
    Begin
        if ( bSig0 OR  bSig1 )<> 0 then
        Begin
          propagateFloat64NaN( a, b, result );
          exit;
        End;
        packFloat64( zSign, 0, 0, 0, result );
        exit;
    End;
    if ( bExp = 0 ) then
    Begin
        if ( ( bSig0 OR  bSig1 ) = 0 ) then
        Begin
            if ( ( aExp OR  aSig0 OR  aSig1 ) = 0 ) then
            Begin
 invalid:
                float_raise( float_flag_invalid );
                z.low := float64_default_nan_low;
                z.high := float64_default_nan_high;
                result := z;
                exit;
            End;
            float_raise( float_flag_divbyzero );
            packFloat64( zSign, $7FF, 0, 0, result );
            exit;
        End;
        normalizeFloat64Subnormal( bSig0, bSig1, bExp, bSig0, bSig1 );
    End;
    if ( aExp = 0 ) then
    Begin
        if ( ( aSig0 OR  aSig1 ) = 0 ) then
        Begin
           packFloat64( zSign, 0, 0, 0, result );
           exit;
        End;
        normalizeFloat64Subnormal( aSig0, aSig1, aExp, aSig0, aSig1 );
    End;
    zExp := aExp - bExp + $3FD;
    shortShift64Left( aSig0 OR  $00100000, aSig1, 11, aSig0, aSig1 );
    shortShift64Left( bSig0 OR  $00100000, bSig1, 11, bSig0, bSig1 );
    if ( le64( bSig0, bSig1, aSig0, aSig1 )<>0 ) then
    Begin
        shift64Right( aSig0, aSig1, 1, aSig0, aSig1 );
        Inc(zExp);
    End;
    zSig0 := estimateDiv64To32( aSig0, aSig1, bSig0 );
    mul64By32To96( bSig0, bSig1, zSig0, term0, term1, term2 );
    sub96( aSig0, aSig1, 0, term0, term1, term2, rem0, rem1, rem2 );
    while ( sbits32 (rem0) < 0 ) do
    Begin
        Dec(zSig0);
        add96( rem0, rem1, rem2, 0, bSig0, bSig1, rem0, rem1, rem2 );
    End;
    zSig1 := estimateDiv64To32( rem1, rem2, bSig0 );
    if ( ( zSig1 and $3FF ) <= 4 ) then
    Begin
        mul64By32To96( bSig0, bSig1, zSig1, term1, term2, term3 );
        sub96( rem1, rem2, 0, term1, term2, term3, rem1, rem2, rem3 );
        while ( sbits32 (rem1) < 0 ) do
        Begin
            Dec(zSig1);
            add96( rem1, rem2, rem3, 0, bSig0, bSig1, rem1, rem2, rem3 );
        End;
        zSig1 := zSig1 or flag( ( rem1 OR  rem2 OR  rem3 ) <> 0 );
    End;
    shift64ExtraRightJamming( zSig0, zSig1, 0, 11, zSig0, zSig1, zSig2 );
    roundAndPackFloat64( zSign, zExp, zSig0, zSig1, zSig2, result );

End;

{*
-------------------------------------------------------------------------------
Returns the remainder of the double-precision floating-point value `a'
with respect to the corresponding value `b'.  The operation is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_rem(a: float64; b : float64) : float64;
{$ifdef fpc}[public,Alias:'FLOAT64_REM'];compilerproc;{$endif}
Var
    aSign, bSign, zSign: flag;
    aExp, bExp, expDiff: int16;
    aSig0, aSig1, bSig0, bSig1, q, term0, term1, term2: bits32;
    allZero, alternateASig0, alternateASig1, sigMean1: bits32;
    sigMean0: sbits32;
    z: float64;
    label invalid;
Begin
    aSig1 := extractFloat64Frac1( a );
    aSig0 := extractFloat64Frac0( a );
    aExp := extractFloat64Exp( a );
    aSign := extractFloat64Sign( a );
    bSig1 := extractFloat64Frac1( b );
    bSig0 := extractFloat64Frac0( b );
    bExp := extractFloat64Exp( b );
    bSign := extractFloat64Sign( b );
    if ( aExp = $7FF ) then
    Begin
        if ((( aSig0 OR  aSig1 )<>0)
             OR ( ( bExp = $7FF ) AND  (( bSig0 OR  bSig1 )<>0) ) ) then
        Begin
            propagateFloat64NaN( a, b, result );
            exit;
        End;
        goto invalid;
    End;
    if ( bExp = $7FF ) then
    Begin
        if ( bSig0 OR  bSig1 ) <> 0 then
        Begin
          propagateFloat64NaN( a, b, result );
          exit;
        End;
        result := a;
        exit;
    End;
    if ( bExp = 0 ) then
    Begin
        if ( ( bSig0 OR  bSig1 ) = 0 ) then
        Begin
 invalid:
            float_raise( float_flag_invalid );
            z.low := float64_default_nan_low;
            z.high := float64_default_nan_high;
            result := z;
            exit;
        End;
        normalizeFloat64Subnormal( bSig0, bSig1, bExp, bSig0, bSig1 );
    End;
    if ( aExp = 0 ) then
    Begin
        if ( ( aSig0 OR  aSig1 ) = 0 ) then
        Begin
           result := a;
           exit;
        End;
        normalizeFloat64Subnormal( aSig0, aSig1, aExp, aSig0, aSig1 );
    End;
    expDiff := aExp - bExp;
    if ( expDiff < -1 ) then
    Begin
       result := a;
       exit;
    End;
    shortShift64Left(
        aSig0 OR  $00100000, aSig1, 11 - flag( expDiff < 0 ), aSig0, aSig1 );
    shortShift64Left( bSig0 OR  $00100000, bSig1, 11, bSig0, bSig1 );
    q := le64( bSig0, bSig1, aSig0, aSig1 );
    if ( q )<>0 then
       sub64( aSig0, aSig1, bSig0, bSig1, aSig0, aSig1 );
    expDiff := expDiff - 32;
    while ( 0 < expDiff ) do
    Begin
        q := estimateDiv64To32( aSig0, aSig1, bSig0 );
        if 4 < q then
          q:= q - 4
        else
          q := 0;
        mul64By32To96( bSig0, bSig1, q, term0, term1, term2 );
        shortShift96Left( term0, term1, term2, 29, term1, term2, allZero );
        shortShift64Left( aSig0, aSig1, 29, aSig0, allZero );
        sub64( aSig0, 0, term1, term2, aSig0, aSig1 );
        expDiff := expDiff - 29;
    End;
    if ( -32 < expDiff ) then
    Begin
        q := estimateDiv64To32( aSig0, aSig1, bSig0 );
        if 4 < q then
          q := q - 4
        else
          q := 0;
        q := q shr (- expDiff);
        shift64Right( bSig0, bSig1, 8, bSig0, bSig1 );
        expDiff := expDiff + 24;
        if ( expDiff < 0 ) then
        Begin
            shift64Right( aSig0, aSig1, - expDiff, aSig0, aSig1 );
        End
        else
        Begin
            shortShift64Left( aSig0, aSig1, expDiff, aSig0, aSig1 );
        End;
        mul64By32To96( bSig0, bSig1, q, term0, term1, term2 );
        sub64( aSig0, aSig1, term1, term2, aSig0, aSig1 );
    End
    else
    Begin
        shift64Right( aSig0, aSig1, 8, aSig0, aSig1 );
        shift64Right( bSig0, bSig1, 8, bSig0, bSig1 );
    End;
    Repeat
        alternateASig0 := aSig0;
        alternateASig1 := aSig1;
        Inc(q);
        sub64( aSig0, aSig1, bSig0, bSig1, aSig0, aSig1 );
    Until not ( 0 <= sbits32 (aSig0) );
    add64(
        aSig0, aSig1, alternateASig0, alternateASig1, bits32(sigMean0), sigMean1 );
    if (    ( sigMean0 < 0 )
         OR ( ( ( sigMean0 OR  sigMean1 ) = 0 ) AND  (( q AND 1 )<>0) ) ) then
    Begin
        aSig0 := alternateASig0;
        aSig1 := alternateASig1;
    End;
    zSign := flag( sbits32 (aSig0) < 0 );
    if ( zSign <> 0 ) then
       sub64( 0, 0, aSig0, aSig1, aSig0, aSig1 );
    normalizeRoundAndPackFloat64( aSign xor zSign, bExp - 4, aSig0, aSig1, result );
End;

{*
-------------------------------------------------------------------------------
Returns the square root of the double-precision floating-point value `a'.
The operation is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Procedure float64_sqrt( a: float64; var out: float64 );
{$ifdef fpc}[public,Alias:'FLOAT64_SQRT'];compilerproc;{$endif}
Var
    aSign: flag;
    aExp, zExp: int16;
    aSig0, aSig1, zSig0, zSig1, zSig2, doubleZSig0: bits32;
    rem0, rem1, rem2, rem3, term0, term1, term2, term3: bits32;
    z: float64;
    label invalid;
Begin
    aSig1 := extractFloat64Frac1( a );
    aSig0 := extractFloat64Frac0( a );
    aExp := extractFloat64Exp( a );
    aSign := extractFloat64Sign( a );
    if ( aExp = $7FF ) then
    Begin
        if ( aSig0 OR  aSig1 ) <> 0 then
        Begin
           propagateFloat64NaN( a, a, out );
           exit;
        End;
        if ( aSign = 0) then
        Begin
          out := a;
          exit;
        End;
        goto invalid;
    End;
    if ( aSign <> 0 ) then
    Begin
        if ( ( aExp OR  aSig0 OR  aSig1 ) = 0 ) then
        Begin
           out := a;
           exit;
        End;
 invalid:
        float_raise( float_flag_invalid );
        z.low := float64_default_nan_low;
        z.high := float64_default_nan_high;
        out := z;
        exit;
    End;
    if ( aExp = 0 ) then
    Begin
        if ( ( aSig0 OR  aSig1 ) = 0 ) then
        Begin
           packFloat64( 0, 0, 0, 0, out );
           exit;
        End;
        normalizeFloat64Subnormal( aSig0, aSig1, aExp, aSig0, aSig1 );
    End;
    zExp := ( ( aExp - $3FF ) shr 1 ) + $3FE;
    aSig0 := aSig0 or $00100000;
    shortShift64Left( aSig0, aSig1, 11, term0, term1 );
    zSig0 := ( estimateSqrt32( aExp, term0 ) shr 1 ) + 1;
    if ( zSig0 = 0 ) then
       zSig0 := $7FFFFFFF;
    doubleZSig0 := zSig0 + zSig0;
    shortShift64Left( aSig0, aSig1, 9 - ( aExp and 1 ), aSig0, aSig1 );
    mul32To64( zSig0, zSig0, term0, term1 );
    sub64( aSig0, aSig1, term0, term1, rem0, rem1 );
    while ( sbits32 (rem0) < 0 ) do
    Begin
        Dec(zSig0);
        doubleZSig0 := doubleZSig0 - 2;
        add64( rem0, rem1, 0, doubleZSig0 OR  1, rem0, rem1 );
    End;
    zSig1 := estimateDiv64To32( rem1, 0, doubleZSig0 );
    if ( ( zSig1 and $1FF ) <= 5 ) then
    Begin
        if ( zSig1 = 0 ) then
           zSig1 := 1;
        mul32To64( doubleZSig0, zSig1, term1, term2 );
        sub64( rem1, 0, term1, term2, rem1, rem2 );
        mul32To64( zSig1, zSig1, term2, term3 );
        sub96( rem1, rem2, 0, 0, term2, term3, rem1, rem2, rem3 );
        while ( sbits32 (rem1) < 0 ) do
        Begin
            Dec(zSig1);
            shortShift64Left( 0, zSig1, 1, term2, term3 );
            term3 := term3 or 1;
            term2 := term2 or doubleZSig0;
            add96( rem1, rem2, rem3, 0, term2, term3, rem1, rem2, rem3 );
        End;
        zSig1 := zSig1 or bits32( ( rem1 OR  rem2 OR  rem3 ) <> 0 );
    End;
    shift64ExtraRightJamming( zSig0, zSig1, 0, 10, zSig0, zSig1, zSig2 );
    roundAndPackFloat64( 0, zExp, zSig0, zSig1, zSig2, out );
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is equal to
the corresponding value `b', and 0 otherwise.  The comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_eq(a: float64; b: float64): flag;
{$ifdef fpc}[public,Alias:'FLOAT64_EQ'];compilerproc;{$endif}
Begin
    if
         (
                ( extractFloat64Exp( a ) = $7FF )
            AND
                (
                    (extractFloat64Frac0( a )  OR  extractFloat64Frac1( a )) <>0
                )
         )
         OR (
                ( extractFloat64Exp( b ) = $7FF )
           AND  (
                    (extractFloat64Frac0( b ) OR  (extractFloat64Frac1( b )) <> 0
                )
           )
       ) then
    Begin
        if ( (float64_is_signaling_nan( a )<>0) OR (float64_is_signaling_nan( b )<>0) ) then
            float_raise( float_flag_invalid );
        float64_eq := 0;
        exit;
    End;
    float64_eq := flag(
           ( a.low = b.low )
        AND  (    ( a.high = b.high )
             OR (    ( a.low = 0 )
                  AND  ( bits32 ( ( a.high OR  b.high ) shl 1 ) = 0 ) )
           ));
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is less than
or equal to the corresponding value `b', and 0 otherwise.  The comparison
is performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_le(a: float64;b: float64): flag;
{$ifdef fpc}[public,Alias:'FLOAT64_LE'];compilerproc;{$endif}
Var
    aSign, bSign: flag;
Begin
    if
         (
                ( extractFloat64Exp( a ) = $7FF )
            AND
                (
                    (extractFloat64Frac0( a )  OR  extractFloat64Frac1( a )) <>0
                )
         )
         OR (
                ( extractFloat64Exp( b ) = $7FF )
           AND  (
                    (extractFloat64Frac0( b ) OR  (extractFloat64Frac1( b )) <> 0
                )
           )
       ) then
    Begin
        float_raise( float_flag_invalid );
        float64_le := 0;
        exit;
    End;
    aSign := extractFloat64Sign( a );
    bSign := extractFloat64Sign( b );
    if ( aSign <> bSign ) then
    Begin
        float64_le := flag(
               (aSign <> 0)
            OR (    ( ( bits32 ( ( a.high OR  b.high ) shl 1 ) ) OR  a.low OR  b.low )
                 = 0 ));
        exit;
    End;
    if aSign <> 0 then
      float64_le := le64( b.high, b.low, a.high, a.low )
    else
      float64_le := le64( a.high, a.low, b.high, b.low );
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is less than
the corresponding value `b', and 0 otherwise.  The comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_lt(a: float64;b: float64): flag;
{$ifdef fpc}[public,Alias:'FLOAT64_LT'];compilerproc;{$endif}
Var
    aSign, bSign: flag;
Begin
    if
         (
                ( extractFloat64Exp( a ) = $7FF )
            AND
                (
                    (extractFloat64Frac0( a )  OR  extractFloat64Frac1( a )) <>0
                )
         )
         OR (
                ( extractFloat64Exp( b ) = $7FF )
           AND  (
                    (extractFloat64Frac0( b ) OR  (extractFloat64Frac1( b )) <> 0
                )
           )
       ) then
    Begin
        float_raise( float_flag_invalid );
        float64_lt := 0;
        exit;
    End;
    aSign := extractFloat64Sign( a );
    bSign := extractFloat64Sign( b );
    if ( aSign <> bSign ) then
    Begin
        float64_lt := flag(
               (aSign <> 0)
            AND  (    ( ( bits32 ( ( a.high OR  b.high ) shl 1 ) ) OR  a.low OR  b.low )
                 <> 0 ));
        exit;
    End;
    if aSign <> 0 then
       float64_lt := lt64( b.high, b.low, a.high, a.low )
    else
       float64_lt := lt64( a.high, a.low, b.high, b.low );
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is equal to
the corresponding value `b', and 0 otherwise.  The invalid exception is
raised if either operand is a NaN.  Otherwise, the comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_eq_signaling( a: float64; b: float64): flag;
Begin

    if
         (
                ( extractFloat64Exp( a ) = $7FF )
            AND
                (
                    (extractFloat64Frac0( a )  OR  extractFloat64Frac1( a )) <>0
                )
         )
         OR (
                ( extractFloat64Exp( b ) = $7FF )
           AND  (
                    (extractFloat64Frac0( b ) OR  (extractFloat64Frac1( b )) <> 0
                )
           )
       ) then
    Begin
        float_raise( float_flag_invalid );
        float64_eq_signaling := 0;
        exit;
    End;
    float64_eq_signaling := flag(
           ( a.low = b.low )
        AND  (    ( a.high = b.high )
             OR (    ( a.low = 0 )
                  AND  ( bits32 ( ( a.high OR  b.high ) shl 1 ) = 0 ) )
           ));
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is less than or
equal to the corresponding value `b', and 0 otherwise.  Quiet NaNs do not
cause an exception.  Otherwise, the comparison is performed according to the
IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_le_quiet(a: float64 ; b: float64 ): flag;
Var
    aSign, bSign : flag;
Begin
    if
         (
                ( extractFloat64Exp( a ) = $7FF )
            AND
                (
                    (extractFloat64Frac0( a )  OR  extractFloat64Frac1( a )) <>0
                )
         )
         OR (
                ( extractFloat64Exp( b ) = $7FF )
           AND  (
                    (extractFloat64Frac0( b ) OR  (extractFloat64Frac1( b )) <> 0
                )
           )
       ) then
    Begin
        if ( (float64_is_signaling_nan( a )<>0)  OR (float64_is_signaling_nan( b )<>0) ) then
            float_raise( float_flag_invalid );
        float64_le_quiet := 0;
        exit;
    End;
    aSign := extractFloat64Sign( a );
    bSign := extractFloat64Sign( b );
    if ( aSign <> bSign ) then
    Begin
     float64_le_quiet := flag
      ((aSign <> 0)
            OR (    ( ( bits32 ( ( a.high OR  b.high ) shl 1 ) ) OR  a.low OR  b.low )
                 = 0 ));
        exit;
    End;
    if aSign <> 0 then
      float64_le_quiet := le64( b.high, b.low, a.high, a.low )
    else
      float64_le_quiet := le64( a.high, a.low, b.high, b.low );
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is less than
the corresponding value `b', and 0 otherwise.  Quiet NaNs do not cause an
exception.  Otherwise, the comparison is performed according to the IEC/IEEE
Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_lt_quiet(a: float64; b: float64 ): Flag;
Var
    aSign, bSign: flag;
Begin
    if
         (
                ( extractFloat64Exp( a ) = $7FF )
            AND
                (
                    (extractFloat64Frac0( a )  OR  extractFloat64Frac1( a )) <>0
                )
         )
         OR (
                ( extractFloat64Exp( b ) = $7FF )
           AND  (
                    (extractFloat64Frac0( b ) OR  (extractFloat64Frac1( b )) <> 0
                )
           )
       ) then
    Begin
        if ( (float64_is_signaling_nan( a )<>0) OR (float64_is_signaling_nan( b )<>0) ) then
            float_raise( float_flag_invalid );
        float64_lt_quiet := 0;
        exit;
    End;
    aSign := extractFloat64Sign( a );
    bSign := extractFloat64Sign( b );
    if ( aSign <> bSign ) then
    Begin
      float64_lt_quiet := flag(
               (aSign<>0)
            AND  (    ( ( bits32 ( ( a.high OR  b.high ) shl 1 ) ) OR  a.low OR  b.low )
                 <> 0 ));
        exit;
    End;
    If aSign <> 0 then
      float64_lt_quiet :=  lt64( b.high, b.low, a.high, a.low )
    else
      float64_lt_quiet := lt64( a.high, a.low, b.high, b.low );
End;


{*----------------------------------------------------------------------------
| Returns the result of converting the 64-bit two's complement integer `a'
| to the single-precision floating-point format.  The conversion is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}
function int64_to_float32( a: int64 ): float32rec; compilerproc;
var
    zSign : flag;
    absA : uint64;
    shiftCount: int8;
    zSig : bits32;
    intval : int64rec;
Begin
    if ( a = 0 ) then
      begin
       int64_to_float32.float32 := 0;
       exit;
      end;
    if a < 0 then
      zSign := flag(TRUE)
    else
      zSign := flag(FALSE);
    if zSign<>0 then
       absA := -a
    else
       absA := a;
    shiftCount := countLeadingZeros64( absA ) - 40;
    if ( 0 <= shiftCount ) then
      begin
        int64_to_float32.float32:= packFloat32( zSign, $95 - shiftCount, absA shl shiftCount );
      end
    else
       begin
        shiftCount := shiftCount + 7;
        if ( shiftCount < 0 ) then
          begin
            intval.low := int64rec(AbsA).low;
            intval.high := int64rec(AbsA).high;
            shift64RightJamming( intval.low, intval.high, - shiftCount,
               intval.low, intval.high);
            int64rec(absA).low := intval.low;
            int64rec(absA).high := intval.high;
          end
        else
            absA := absA shl shiftCount;
        int64_to_float32.float32:=roundAndPackFloat32( zSign, $9C - shiftCount, absA );
      end;
End;


{*----------------------------------------------------------------------------
| Returns the result of converting the 64-bit two's complement integer `a'
| to the double-precision floating-point format.  The conversion is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}
function int64_to_float64( a: int64 ): float64;
{$ifdef fpc}[public,Alias:'INT64_TO_FLOAT64'];compilerproc;{$endif}
var
 zSign : flag;
 float_result : float64;
 intval : int64rec;
 AbsA : bits64;
 shiftcount : int8;
 zSig0, zSig1 : bits32;
Begin
    if ( a = 0 ) then
      Begin
       packFloat64( 0, 0, 0, 0, result );
       exit;
      end;
    zSign := flag( a < 0 );
    if ZSign<>0 then
      AbsA := -a
    else
      AbsA := a;
    shiftCount := countLeadingZeros64( absA ) - 11;
    if ( 0 <= shiftCount ) then
      Begin
        absA := absA shl shiftcount;
        zSig0:=int64rec(absA).high;
        zSig1:=int64rec(absA).low;
      End
    else
      Begin
        shift64Right( absA, 0, - shiftCount, zSig0, zSig1 );
      End;
    packFloat64( zSign, $432 - shiftCount, zSig0, zSig1, float_result );
    int64_to_float64:= float_result;
End;


{*----------------------------------------------------------------------------
| Returns 1 if the 128-bit value formed by concatenating `a0' and `a1'
| is equal to the 128-bit value formed by concatenating `b0' and `b1'.
| Otherwise, returns 0.
*----------------------------------------------------------------------------*}

function eq128( a0: bits64; a1: bits64; b0: bits64; b1 : bits64): flag;inline;
begin
    result := ord(( a0 = b0 ) and ( a1 = b1 ));
end;

{*----------------------------------------------------------------------------
| Returns 1 if the 128-bit value formed by concatenating `a0' and `a1' is less
| than or equal to the 128-bit value formed by concatenating `b0' and `b1'.
| Otherwise, returns 0.
*----------------------------------------------------------------------------*}

function le128( a0: bits64; a1: bits64; b0: bits64; b1 : bits64): flag;inline;
begin
    result:=ord(( a0 < b0 ) or ( ( a0 = b0 ) and ( a1 <= b1 ) ));
end;

{*----------------------------------------------------------------------------
| Shifts the 192-bit value formed by concatenating `a0', `a1', and `a2' right
| by 64 _plus_ the number of bits given in `count'.  The shifted result is
| at most 128 nonzero bits; these are broken into two 64-bit pieces which are
| stored at the locations pointed to by `z0Ptr' and `z1Ptr'.  The bits shifted
| off form a third 64-bit result as follows:  The _last_ bit shifted off is
| the most-significant bit of the extra result, and the other 63 bits of the
| extra result are all zero if and only if _all_but_the_last_ bits shifted off
| were all zero.  This extra result is stored in the location pointed to by
| `z2Ptr'.  The value of `count' can be arbitrarily large.
|     (This routine makes more sense if `a0', `a1', and `a2' are considered
| to form a fixed-point value with binary point between `a1' and `a2'.  This
| fixed-point value is shifted right by the number of bits given in `count',
| and the integer part of the result is returned at the locations pointed to
| by `z0Ptr' and `z1Ptr'.  The fractional part of the result may be slightly
| corrupted as described above, and is returned at the location pointed to by
| `z2Ptr'.)
*----------------------------------------------------------------------------*}

procedure shift128ExtraRightJamming(
     a0: bits64;
     a1: bits64;
     a2: bits64;
     count: int16;
     var z0Ptr: bits64;
     var z1Ptr: bits64;
     var z2Ptr: bits64);
var
    z0, z1, z2: bits64;
    negCount: int8;
begin
    negCount := ( - count ) and 63;

    if ( count = 0 ) then
    begin
        z2 := a2;
        z1 := a1;
        z0 := a0;
    end
    else begin
        if ( count < 64 ) then
        begin
            z2 := a1 shr negCount;
            z1 := ( a0 shl negCount ) or  ( a1 shr count );
            z0 := a0 shr count;
        end
        else begin
            if ( count = 64 ) then
            begin
                z2 := a1;
                z1 := a0;
            end
            else begin
                a2 := a2 or a1;
                if ( count < 128 ) then
                begin
                    z2 := a0 shl negCount;
                    z1 := a0 shr ( count and 63 );
                end
                else begin
                	  if ( count = 128 ) then
                      z2 :=  a0
                    else
                      z2 := ord( a0 <> 0 );
                    z1 := 0;
                end;
            end;
            z0 := 0;
        end;
        z2 := z2 or ord( a2 <> 0 );
    end;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;

end;

{*----------------------------------------------------------------------------
| Shifts the 128-bit value formed by concatenating `a0' and `a1' right by 64
| _plus_ the number of bits given in `count'.  The shifted result is at most
| 64 nonzero bits; this is stored at the location pointed to by `z0Ptr'.  The
| bits shifted off form a second 64-bit result as follows:  The _last_ bit
| shifted off is the most-significant bit of the extra result, and the other
| 63 bits of the extra result are all zero if and only if _all_but_the_last_
| bits shifted off were all zero.  This extra result is stored in the location
| pointed to by `z1Ptr'.  The value of `count' can be arbitrarily large.
|     (This routine makes more sense if `a0' and `a1' are considered to form
| a fixed-point value with binary point between `a0' and `a1'.  This fixed-
| point value is shifted right by the number of bits given in `count', and
| the integer part of the result is returned at the location pointed to by
| `z0Ptr'.  The fractional part of the result may be slightly corrupted as
| described above, and is returned at the location pointed to by `z1Ptr'.)
*----------------------------------------------------------------------------*}

procedure shift64ExtraRightJamming(a0: bits64; a1: bits64; count: int16; var z0Ptr: bits64; var z1Ptr : bits64);
var
    z0, z1: bits64;
    negCount: int8;
begin
    negCount := ( - count ) and 63;

    if ( count = 0 ) then
    begin
        z1 := a1;
        z0 := a0;
    end
    else if ( count < 64 ) then
    begin
        z1 := ( a0 shl negCount ) or ord( a1 <>  0 );
        z0 := a0 shr count;
    end
    else begin
        if ( count = 64 ) then
        begin
            z1 := a0 or ord( a1 <>  0 );
        end
        else begin
            z1 := ord( ( a0 or a1 ) <>  0 );
        end;
        z0 := 0;
    end;
    z1Ptr := z1;
    z0Ptr := z0;
end;

{$ifdef FPC_SOFTFLOAT_FLOATX80}

{*----------------------------------------------------------------------------
| Returns the fraction bits of the extended double-precision floating-point
| value `a'.
*----------------------------------------------------------------------------*}

function extractFloatx80Frac(a : floatx80): bits64;inline;
begin
    result:=a.low;
end;

{*----------------------------------------------------------------------------
| Returns the exponent bits of the extended double-precision floating-point
| value `a'.
*----------------------------------------------------------------------------*}

function extractFloatx80Exp(a : floatx80): int32;inline;
begin
    result:=a.high and $7FFF;
end;

{*----------------------------------------------------------------------------
| Returns the sign bit of the extended double-precision floating-point value
| `a'.
*----------------------------------------------------------------------------*}

function extractFloatx80Sign(a : floatx80): flag;inline;
begin
    result:=a.high shr 15;
end;

{*----------------------------------------------------------------------------
| Normalizes the subnormal extended double-precision floating-point value
| represented by the denormalized significand `aSig'.  The normalized exponent
| and significand are stored at the locations pointed to by `zExpPtr' and
| `zSigPtr', respectively.
*----------------------------------------------------------------------------*}

procedure normalizeFloatx80Subnormal( aSig: bits64; var zExpPtr: int32; var zSigPtr : bits64);
var
    shiftCount: int8;
begin
    shiftCount := countLeadingZeros64( aSig );
    zSigPtr := aSig shl shiftCount;
    zExpPtr := 1 - shiftCount;
end;

{*----------------------------------------------------------------------------
| Packs the sign `zSign', exponent `zExp', and significand `zSig' into an
| extended double-precision floating-point value, returning the result.
*----------------------------------------------------------------------------*}

function packFloatx80( zSign: flag; zExp: int32; zSig : bits64): floatx80;
var
    z: floatx80;
begin
    z.low := zSig;
    z.high := (  bits16(zSign) shl 15 ) + zExp;
    result:=z;
end;

{*----------------------------------------------------------------------------
| Takes an abstract floating-point value having sign `zSign', exponent `zExp',
| and extended significand formed by the concatenation of `zSig0' and `zSig1',
| and returns the proper extended double-precision floating-point value
| corresponding to the abstract input.  Ordinarily, the abstract value is
| rounded and packed into the extended double-precision format, with the
| inexact exception raised if the abstract input cannot be represented
| exactly.  However, if the abstract value is too large, the overflow and
| inexact exceptions are raised and an infinity or maximal finite value is
| returned.  If the abstract value is too small, the input value is rounded to
| a subnormal number, and the underflow and inexact exceptions are raised if
| the abstract input cannot be represented exactly as a subnormal extended
| double-precision floating-point number.
|     If `roundingPrecision' is 32 or 64, the result is rounded to the same
| number of bits as single or double precision, respectively.  Otherwise, the
| result is rounded to the full precision of the extended double-precision
| format.
|     The input significand must be normalized or smaller.  If the input
| significand is not normalized, `zExp' must be 0; in that case, the result
| returned is a subnormal number, and it must not require rounding.  The
| handling of underflow and overflow follows the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

{
function roundAndPackFloatx80(roundingPrecision: int8; zSign: flag; zExp: int32; zSig0: bits64; zSig1: bits64): floatx80;
var
    roundingMode: int8;
    roundNearestEven, increment, isTiny: flag;
    roundIncrement, roundMask, roundBits: int64;
label
    precision80;
begin
    roundingMode := softfloat_rounding_mode;
    roundNearestEven := flag( roundingMode = float_round_nearest_even );
    if ( roundingPrecision = 80 ) then
      goto precision80;
    if ( roundingPrecision = 64 ) then
    begin
        roundIncrement := int64( $0000000000000400 );
        roundMask := int64( $00000000000007FF );
    end
    else if ( roundingPrecision = 32 ) then
    begin
        roundIncrement := int64( $0000008000000000 );
        roundMask := int64( $000000FFFFFFFFFF );
    end
    else begin
        goto precision80;
    end;
    zSig0 := zSig0 or ord( zSig1 <> 0 );
    if ( not (roundNearestEven<>0) ) then
    begin
        if ( roundingMode = float_round_to_zero ) then
        begin
            roundIncrement := 0;
        end
        else begin
            roundIncrement := roundMask;
            if ( zSign<>0 ) then
            begin
                if ( roundingMode = float_round_up ) then
                  roundIncrement := 0;
            end
            else begin
                if ( roundingMode = float_round_down ) then
                  roundIncrement := 0;
            end;
        end;
    end;
    roundBits := zSig0 and roundMask;
    if ( $7FFD <= (bits32) ( zExp - 1 ) ) begin
        if (    ( $7FFE < zExp )
             or ( ( zExp = $7FFE ) and ( zSig0 + roundIncrement < zSig0 ) )
           ) begin
            goto overflow;
        end;
        if ( zExp <= 0 ) begin
            isTiny =
                   ( float_detect_tininess = float_tininess_before_rounding )
                or ( zExp < 0 )
                or ( zSig0 <= zSig0 + roundIncrement );
            shift64RightJamming( zSig0, 1 - zExp, zSig0 );
            zExp := 0;
            roundBits := zSig0 and roundMask;
            if ( isTiny and roundBits ) float_raise( float_flag_underflow );
            if ( roundBits ) softfloat_exception_flags |= float_flag_inexact;
            zSig0 += roundIncrement;
            if ( (sbits64) zSig0 < 0 ) zExp := 1;
            roundIncrement := roundMask + 1;
            if ( roundNearestEven and ( roundBits shl 1 = roundIncrement ) ) begin
                roundMask |= roundIncrement;
            end;
            zSig0 = ~ roundMask;
            result:=packFloatx80( zSign, zExp, zSig0 );
        end;
    end;
    if ( roundBits ) softfloat_exception_flags |= float_flag_inexact;
    zSig0 += roundIncrement;
    if ( zSig0 < roundIncrement ) begin
        ++zExp;
        zSig0 := LIT64( $8000000000000000 );
    end;
    roundIncrement := roundMask + 1;
    if ( roundNearestEven and ( roundBits shl 1 = roundIncrement ) ) begin
        roundMask |= roundIncrement;
    end;
    zSig0 = ~ roundMask;
    if ( zSig0 = 0 ) zExp := 0;
    result:=packFloatx80( zSign, zExp, zSig0 );
 precision80:
    increment := ( (sbits64) zSig1 < 0 );
    if ( ! roundNearestEven ) begin
        if ( roundingMode = float_round_to_zero ) begin
            increment := 0;
        end;
        else begin
            if ( zSign ) begin
                increment := ( roundingMode = float_round_down ) and zSig1;
            end;
            else begin
                increment := ( roundingMode = float_round_up ) and zSig1;
            end;
        end;
    end;
    if ( $7FFD <= (bits32) ( zExp - 1 ) ) begin
        if (    ( $7FFE < zExp )
             or (    ( zExp = $7FFE )
                  and ( zSig0 = LIT64( $FFFFFFFFFFFFFFFF ) )
                  and increment
                )
           ) begin
            roundMask := 0;
 overflow:
            float_raise( float_flag_overflow or float_flag_inexact );
            if (    ( roundingMode = float_round_to_zero )
                 or ( zSign and ( roundingMode = float_round_up ) )
                 or ( ! zSign and ( roundingMode = float_round_down ) )
               ) begin
                result:=packFloatx80( zSign, $7FFE, ~ roundMask );
            end;
            result:=packFloatx80( zSign, $7FFF, LIT64( $8000000000000000 ) );
        end;
        if ( zExp <= 0 ) begin
            isTiny =
                   ( float_detect_tininess = float_tininess_before_rounding )
                or ( zExp < 0 )
                or ! increment
                or ( zSig0 < LIT64( $FFFFFFFFFFFFFFFF ) );
            shift64ExtraRightJamming( zSig0, zSig1, 1 - zExp, zSig0, zSig1 );
            zExp := 0;
            if ( isTiny and zSig1 ) float_raise( float_flag_underflow );
            if ( zSig1 ) softfloat_exception_flags |= float_flag_inexact;
            if ( roundNearestEven ) begin
                increment := ( (sbits64) zSig1 < 0 );
            end;
            else begin
                if ( zSign ) begin
                    increment := ( roundingMode = float_round_down ) and zSig1;
                end;
                else begin
                    increment := ( roundingMode = float_round_up ) and zSig1;
                end;
            end;
            if ( increment ) begin
                ++zSig0;
                zSig0 =
                    ~ ( ( (bits64) ( zSig1 shl 1 ) = 0 ) and roundNearestEven );
                if ( (sbits64) zSig0 < 0 ) zExp := 1;
            end;
            result:=packFloatx80( zSign, zExp, zSig0 );
        end;
    end;
    if ( zSig1 ) softfloat_exception_flags |= float_flag_inexact;
    if ( increment ) begin
        ++zSig0;
        if ( zSig0 = 0 ) begin
            ++zExp;
            zSig0 := LIT64( $8000000000000000 );
        end;
        else begin
            zSig0 = ~ ( ( (bits64) ( zSig1 shl 1 ) = 0 ) and roundNearestEven );
        end;
    end;
    else begin
        if ( zSig0 = 0 ) zExp := 0;
    end;
    result:=packFloatx80( zSign, zExp, zSig0 );

end;
}

{*----------------------------------------------------------------------------
| Takes an abstract floating-point value having sign `zSign', exponent
| `zExp', and significand formed by the concatenation of `zSig0' and `zSig1',
| and returns the proper extended double-precision floating-point value
| corresponding to the abstract input.  This routine is just like
| `roundAndPackFloatx80' except that the input significand does not have to be
| normalized.
*----------------------------------------------------------------------------*}

{
function normalizeRoundAndPackFloatx80(roundingPrecision: int8; zSign: flag; zExp: int32; zSig0: bits64; zSig1: bits64): floatx80;
var
    shiftCount: int8;
begin
    if ( zSig0 = 0 ) then
    begin
        zSig0 := zSig1;
        zSig1 := 0;
        zExp -= 64;
    end;
    shiftCount := countLeadingZeros64( zSig0 );
    shortShift128Left( zSig0, zSig1, shiftCount, zSig0, zSig1 );
    zExp := zExp - shiftCount;
    result := roundAndPackFloatx80( roundingPrecision, zSign, zExp, zSig0, zSig1 );

end; }

{*----------------------------------------------------------------------------
| Returns the result of converting the extended double-precision floating-
| point value `a' to the 32-bit two's complement integer format.  The
| conversion is performed according to the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic---which means in particular that the conversion
| is rounded according to the current rounding mode.  If `a' is a NaN, the
| largest positive integer is returned.  Otherwise, if the conversion
| overflows, the largest integer with the same sign as `a' is returned.
*----------------------------------------------------------------------------*}

{
function floatx80_to_int32(a: floatx80): int32;
var
    aSign: flag;
    aExp, shiftCount: int32;
    aSig: bits64;
begin
    aSig := extractFloatx80Frac( a );
    aExp := extractFloatx80Exp( a );
    aSign := extractFloatx80Sign( a );
    if ( ( aExp = $7FFF ) and (bits64) and ( aSig shl 1 ) ) then aSign := 0;
    shiftCount := $4037 - aExp;
    if ( shiftCount <= 0 ) shiftCount := 1;
    shift64RightJamming( aSig, shiftCount, aSig );
    result := roundAndPackInt32( aSign, aSig );

end;    }

{*----------------------------------------------------------------------------
| Returns the result of converting the extended double-precision floating-
| point value `a' to the 32-bit two's complement integer format.  The
| conversion is performed according to the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic, except that the conversion is always rounded
| toward zero.  If `a' is a NaN, the largest positive integer is returned.
| Otherwise, if the conversion overflows, the largest integer with the same
| sign as `a' is returned.
*----------------------------------------------------------------------------*}

{
function floatx80_to_int32_round_to_zero(a: floatx80): int32;
var
    aSign: flag;
    aExp, shiftCount: int32;
    aSig, savedASig: bits64;
    z: int32;
begin
    aSig := extractFloatx80Frac( a );
    aExp := extractFloatx80Exp( a );
    aSign := extractFloatx80Sign( a );
    if ( $401E < aExp ) begin
        if ( ( aExp = $7FFF ) and (bits64) ( aSig shl 1 ) ) aSign := 0;
        goto invalid;
    end;
    else if ( aExp < $3FFF ) begin
        if ( aExp or aSig ) softfloat_exception_flags or= float_flag_inexact;
        result := 0;
    end;
    shiftCount := $403E - aExp;
    savedASig := aSig;
    aSig >>= shiftCount;
    z := aSig;
    if ( aSign ) z := - z;
    if ( ( z < 0 ) xor aSign ) begin
 invalid:
        float_raise( float_flag_invalid );
        result := aSign ? (sbits32) $80000000 : $7FFFFFFF;
    end;
    if ( ( aSig shl shiftCount ) <> savedASig ) begin
        softfloat_exception_flags or= float_flag_inexact;
    end;
    result := z;

end;
}

{*----------------------------------------------------------------------------
| Returns the result of converting the extended double-precision floating-
| point value `a' to the 64-bit two's complement integer format.  The
| conversion is performed according to the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic---which means in particular that the conversion
| is rounded according to the current rounding mode.  If `a' is a NaN,
| the largest positive integer is returned.  Otherwise, if the conversion
| overflows, the largest integer with the same sign as `a' is returned.
*----------------------------------------------------------------------------*}

{
function floatx80_to_int64(a: floatx80): int64;
var
    aSign: flag;
    aExp, shiftCount: int32;
    aSig, aSigExtra: bits64;
begin

    aSig := extractFloatx80Frac( a );
    aExp := extractFloatx80Exp( a );
    aSign := extractFloatx80Sign( a );
    shiftCount := $403E - aExp;
    if ( shiftCount <= 0 ) begin
        if ( shiftCount ) begin
            float_raise( float_flag_invalid );
            if (    ! aSign
                 or (    ( aExp = $7FFF )
                      and ( aSig <> LIT64( $8000000000000000 ) ) )
               ) begin
                result := LIT64( $7FFFFFFFFFFFFFFF );
            end;
            result := (sbits64) LIT64( $8000000000000000 );
        end;
        aSigExtra := 0;
    end;
    else begin
        shift64ExtraRightJamming( aSig, 0, shiftCount, aSig, aSigExtra );
    end;
    result := roundAndPackInt64( aSign, aSig, aSigExtra );

end;
}

{*----------------------------------------------------------------------------
| Returns the result of converting the extended double-precision floating-
| point value `a' to the 64-bit two's complement integer format.  The
| conversion is performed according to the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic, except that the conversion is always rounded
| toward zero.  If `a' is a NaN, the largest positive integer is returned.
| Otherwise, if the conversion overflows, the largest integer with the same
| sign as `a' is returned.
*----------------------------------------------------------------------------*}

{
function floatx80_to_int64_round_to_zero(a: floatx80): int64;
var
    aSign: flag;
    aExp, shiftCount: int32;
    aSig: bits64;
    z: int64;
begin
    aSig := extractFloatx80Frac( a );
    aExp := extractFloatx80Exp( a );
    aSign := extractFloatx80Sign( a );
    shiftCount := aExp - $403E;
    if ( 0 <= shiftCount ) begin
        aSig = LIT64( $7FFFFFFFFFFFFFFF );
        if ( ( a.high <> $C03E ) or aSig ) begin
            float_raise( float_flag_invalid );
            if ( ! aSign or ( ( aExp = $7FFF ) and aSig ) ) begin
                result := LIT64( $7FFFFFFFFFFFFFFF );
            end;
        end;
        result := (sbits64) LIT64( $8000000000000000 );
    end;
    else if ( aExp < $3FFF ) begin
        if ( aExp or aSig ) softfloat_exception_flags or= float_flag_inexact;
        result := 0;
    end;
    z := aSig>>( - shiftCount );
    if ( (bits64) ( aSig shl ( shiftCount and 63 ) ) ) begin
        softfloat_exception_flags or= float_flag_inexact;
    end;
    if ( aSign ) z := - z;
    result := z;

end; }

{*----------------------------------------------------------------------------
| Returns the result of converting the extended double-precision floating-
| point value `a' to the single-precision floating-point format.  The
| conversion is performed according to the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function floatx80_to_float32(a: floatx80): float32;
var
    aSign: flag;
    aExp: int32;
    aSig: bits64;
begin
    aSig := extractFloatx80Frac( a );
    aExp := extractFloatx80Exp( a );
    aSign := extractFloatx80Sign( a );
    if ( aExp = $7FFF ) then
    begin
      if ( ( aSig shl 1 )>0 ) then
      begin
        result := 0; //exit commonNaNToFloat32( floatx80ToCommonNaN( a ) );
      end;
      result := packFloat32( aSign, $FF, 0 );
      exit;

    end;
    shift64RightJamming( aSig, 33, aSig );
    if ( aExp or aSig )>0 then aExp -= $3F81;

    result := roundAndPackFloat32( aSign, aExp, aSig );
    exit;
end;

{*----------------------------------------------------------------------------
| Returns the result of converting the extended double-precision floating-
| point value `a' to the double-precision floating-point format.  The
| conversion is performed according to the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function floatx80_to_float64(a: floatx80): float64;
var
    aSign: flag;
    aExp: int32;
    aSig, zSig: bits64;
begin
    aSig := extractFloatx80Frac( a );
    aExp := extractFloatx80Exp( a );
    aSign := extractFloatx80Sign( a );
    if ( aExp = $7FFF ) then
    begin
        if ( ( aSig shl 1 ) >0 ) then
        begin
          result.low:=0;
          result.high:=0;
          exit;
          //  result := commonNaNToFloat64( floatx80ToCommonNaN( a ) );
        end;
        result := packFloat64( aSign, $7FF, 0 );
        exit;
    end;
    shift64RightJamming( aSig, 1, zSig );
    if ( aExp or aSig )>0 then aExp -= $3C01;
    result := roundAndPackFloat64( aSign, aExp, zSig );

end;

{$ifdef FPC_SOFTFLOAT_FLOAT128}
{*----------------------------------------------------------------------------
| Returns the result of converting the extended double-precision floating-
| point value `a' to the quadruple-precision floating-point format.  The
| conversion is performed according to the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

{
function floatx80_to_float128(a: floatx80): float128;
var
    aSign: flag;
    aExp: int16;
    aSig, zSig0, zSig1: bits64;
begin
    aSig := extractFloatx80Frac( a );
    aExp := extractFloatx80Exp( a );
    aSign := extractFloatx80Sign( a );
    if ( ( aExp = $7FFF ) and (( aSig shl 1 )>0) ) then begin
        //result := commonNaNToFloat128( floatx80ToCommonNaN( a ) );
        exit;
    end;
    shift128Right( aSig shl 1, 0, 16, zSig0, zSig1 );
    result := packFloat128( aSign, aExp, zSig0, zSig1 );

end; }

{$endif FPC_SOFTFLOAT_FLOAT128}

{*----------------------------------------------------------------------------
| Rounds the extended double-precision floating-point value `a' to an integer,
| and Returns the result as an extended quadruple-precision floating-point
| value.  The operation is performed according to the IEC/IEEE Standard for
| Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

{
function floatx80_round_to_int(a: floatx80): floatx80;
var
    aSign: flag;
    aExp: int32;
    lastBitMask, roundBitsMask: bits64;
    roundingMode: int8;
    z: floatx80;
begin
    aExp := extractFloatx80Exp( a );
    if ( $403E <= aExp ) begin
        if ( ( aExp = $7FFF ) and (bits64) ( extractFloatx80Frac( a ) shl 1 ) ) begin
            result := propagateFloatx80NaN( a, a );
        end;
        result := a;
    end;
    if ( aExp < $3FFF ) begin
        if (    ( aExp = 0 )
             and ( (bits64) ( extractFloatx80Frac( a ) shl 1 ) = 0 ) ) begin
            result := a;
        end;
        softfloat_exception_flags or= float_flag_inexact;
        aSign := extractFloatx80Sign( a );
        switch ( softfloat_rounding_mode ) begin
         case float_round_nearest_even:
            if ( ( aExp = $3FFE ) and (bits64) ( extractFloatx80Frac( a ) shl 1 )
               ) begin
                result :=
                    packFloatx80( aSign, $3FFF, LIT64( $8000000000000000 ) );
            end;
            break;
         case float_round_down:
            result :=
                  aSign ?
                      packFloatx80( 1, $3FFF, LIT64( $8000000000000000 ) )
                : packFloatx80( 0, 0, 0 );
         case float_round_up:
            result :=
                  aSign ? packFloatx80( 1, 0, 0 )
                : packFloatx80( 0, $3FFF, LIT64( $8000000000000000 ) );
        end;
        result := packFloatx80( aSign, 0, 0 );
    end;
    lastBitMask := 1;
    lastBitMask  shl = $403E - aExp;
    roundBitsMask := lastBitMask - 1;
    z := a;
    roundingMode := softfloat_rounding_mode;
    if ( roundingMode = float_round_nearest_even ) begin
        z.low += lastBitMask>>1;
        if ( ( z.low and roundBitsMask ) = 0 ) z.low = ~ lastBitMask;
    end;
    else if ( roundingMode <> float_round_to_zero ) begin
        if ( extractFloatx80Sign( z ) xor ( roundingMode = float_round_up ) ) begin
            z.low += roundBitsMask;
        end;
    end;
    z.low = ~ roundBitsMask;
    if ( z.low = 0 ) begin
        ++z.high;
        z.low := LIT64( $8000000000000000 );
    end;
    if ( z.low <> a.low ) softfloat_exception_flags or= float_flag_inexact;
    result := z;

end;  }

{*----------------------------------------------------------------------------
| Returns the result of adding the absolute values of the extended double-
| precision floating-point values `a' and `b'.  If `zSign' is 1, the sum is
| negated before being returned.  `zSign' is ignored if the result is a NaN.
| The addition is performed according to the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

{
function addFloatx80Sigs(a: floatx80; b: floatx80; zSign : flag): floatx80;
var
    aExp, bExp, zExp: int32;
    aSig, bSig, zSig0, zSig1: bits64;
    expDiff: int32;
begin
    aSig := extractFloatx80Frac( a );
    aExp := extractFloatx80Exp( a );
    bSig := extractFloatx80Frac( b );
    bExp := extractFloatx80Exp( b );
    expDiff := aExp - bExp;
    if ( 0 < expDiff ) begin
        if ( aExp = $7FFF ) begin
            if ( (bits64) ( aSig shl 1 ) ) result := propagateFloatx80NaN( a, b );
            result := a;
        end;
        if ( bExp = 0 ) --expDiff;
        shift64ExtraRightJamming( bSig, 0, expDiff, bSig, zSig1 );
        zExp := aExp;
    end;
    else if ( expDiff < 0 ) begin
        if ( bExp = $7FFF ) begin
            if ( (bits64) ( bSig shl 1 ) ) result := propagateFloatx80NaN( a, b );
            result := packFloatx80( zSign, $7FFF, LIT64( $8000000000000000 ) );
        end;
        if ( aExp = 0 ) ++expDiff;
        shift64ExtraRightJamming( aSig, 0, - expDiff, aSig, zSig1 );
        zExp := bExp;
    end;
    else begin
        if ( aExp = $7FFF ) begin
            if ( (bits64) ( ( aSig or bSig ) shl 1 ) ) begin
                result := propagateFloatx80NaN( a, b );
            end;
            result := a;
        end;
        zSig1 := 0;
        zSig0 := aSig + bSig;
        if ( aExp = 0 ) begin
            normalizeFloatx80Subnormal( zSig0, zExp, zSig0 );
            goto roundAndPack;
        end;
        zExp := aExp;
        goto shiftRight1;
    end;
    zSig0 := aSig + bSig;
    if ( (sbits64) zSig0 < 0 ) goto roundAndPack;
 shiftRight1:
    shift64ExtraRightJamming( zSig0, zSig1, 1, zSig0, zSig1 );
    zSig0 or= LIT64( $8000000000000000 );
    ++zExp;
 roundAndPack:
    result :=
        roundAndPackFloatx80(
            floatx80_rounding_precision, zSign, zExp, zSig0, zSig1 );

end;
}
{*----------------------------------------------------------------------------
| Returns the result of subtracting the absolute values of the extended
| double-precision floating-point values `a' and `b'.  If `zSign' is 1, the
| difference is negated before being returned.  `zSign' is ignored if the
| result is a NaN.  The subtraction is performed according to the IEC/IEEE
| Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

{
function subFloatx80Sigs(a: floatx80; b: floatx80; zSign : flag): floatx80;
var
    aExp, bExp, zExp: int32;
    aSig, bSig, zSig0, zSig1: bits64;
    expDiff: int32;
    z: floatx80;
begin
    aSig := extractFloatx80Frac( a );
    aExp := extractFloatx80Exp( a );
    bSig := extractFloatx80Frac( b );
    bExp := extractFloatx80Exp( b );
    expDiff := aExp - bExp;
    if ( 0 < expDiff ) goto aExpBigger;
    if ( expDiff < 0 ) goto bExpBigger;
    if ( aExp = $7FFF ) begin
        if ( (bits64) ( ( aSig or bSig ) shl 1 ) ) begin
            result := propagateFloatx80NaN( a, b );
        end;
        float_raise( float_flag_invalid );
        z.low := floatx80_default_nan_low;
        z.high := floatx80_default_nan_high;
        result := z;
    end;
    if ( aExp = 0 ) begin
        aExp := 1;
        bExp := 1;
    end;
    zSig1 := 0;
    if ( bSig < aSig ) goto aBigger;
    if ( aSig < bSig ) goto bBigger;
    result := packFloatx80( softfloat_rounding_mode = float_round_down, 0, 0 );
 bExpBigger:
    if ( bExp = $7FFF ) begin
        if ( (bits64) ( bSig shl 1 ) ) result := propagateFloatx80NaN( a, b );
        result := packFloatx80( zSign xor 1, $7FFF, LIT64( $8000000000000000 ) );
    end;
    if ( aExp = 0 ) ++expDiff;
    shift128RightJamming( aSig, 0, - expDiff, aSig, zSig1 );
 bBigger:
    sub128( bSig, 0, aSig, zSig1, zSig0, zSig1 );
    zExp := bExp;
    zSign  xor = 1;
    goto normalizeRoundAndPack;
 aExpBigger:
    if ( aExp = $7FFF ) begin
        if ( (bits64) ( aSig shl 1 ) ) result := propagateFloatx80NaN( a, b );
        result := a;
    end;
    if ( bExp = 0 ) --expDiff;
    shift128RightJamming( bSig, 0, expDiff, bSig, zSig1 );
 aBigger:
    sub128( aSig, 0, bSig, zSig1, zSig0, zSig1 );
    zExp := aExp;
 normalizeRoundAndPack:
    result :=
        normalizeRoundAndPackFloatx80(
            floatx80_rounding_precision, zSign, zExp, zSig0, zSig1 );

end;
 }
{*----------------------------------------------------------------------------
| Returns the result of adding the extended double-precision floating-point
| values `a' and `b'.  The operation is performed according to the IEC/IEEE
| Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}
   {
function floatx80_add(a: floatx80; b: floatx80): floatx80;
var
    aSign, bSign: flag;
begin
    aSign := extractFloatx80Sign( a );
    bSign := extractFloatx80Sign( b );
    if ( aSign = bSign ) begin
        result := addFloatx80Sigs( a, b, aSign );
    end;
    else begin
        result := subFloatx80Sigs( a, b, aSign );
    end;

end;
     }
{*----------------------------------------------------------------------------
| Returns the result of subtracting the extended double-precision floating-
| point values `a' and `b'.  The operation is performed according to the
| IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}
  {
function floatx80_sub(a: floatx80; b: floatx80 ): floatx80;
var
    aSign, bSign: flag;
begin
    aSign := extractFloatx80Sign( a );
    bSign := extractFloatx80Sign( b );
    if ( aSign = bSign ) begin
        result := subFloatx80Sigs( a, b, aSign );
    end;
    else begin
        result := addFloatx80Sigs( a, b, aSign );
    end;

end;}

{*----------------------------------------------------------------------------
| Returns the result of multiplying the extended double-precision floating-
| point values `a' and `b'.  The operation is performed according to the
| IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}
     {
function floatx80_mul(a: floatx80; b: floatx80): floatx80;
var
    aSign, bSign, zSign: flag;
    aExp, bExp, zExp: int32;
    aSig, bSig, zSig0, zSig1: bits64;
    z: floatx80;
begin
    aSig := extractFloatx80Frac( a );
    aExp := extractFloatx80Exp( a );
    aSign := extractFloatx80Sign( a );
    bSig := extractFloatx80Frac( b );
    bExp := extractFloatx80Exp( b );
    bSign := extractFloatx80Sign( b );
    zSign := aSign xor bSign;
    if ( aExp = $7FFF ) begin
        if (    (bits64) ( aSig shl 1 )
             or ( ( bExp = $7FFF ) and (bits64) ( bSig shl 1 ) ) ) begin
            result := propagateFloatx80NaN( a, b );
        end;
        if ( ( bExp or bSig ) = 0 ) goto invalid;
        result := packFloatx80( zSign, $7FFF, LIT64( $8000000000000000 ) );
    end;
    if ( bExp = $7FFF ) begin
        if ( (bits64) ( bSig shl 1 ) ) result := propagateFloatx80NaN( a, b );
        if ( ( aExp or aSig ) = 0 ) begin
 invalid:
            float_raise( float_flag_invalid );
            z.low := floatx80_default_nan_low;
            z.high := floatx80_default_nan_high;
            result := z;
        end;
        result := packFloatx80( zSign, $7FFF, LIT64( $8000000000000000 ) );
    end;
    if ( aExp = 0 ) begin
        if ( aSig = 0 ) result := packFloatx80( zSign, 0, 0 );
        normalizeFloatx80Subnormal( aSig, aExp, aSig );
    end;
    if ( bExp = 0 ) begin
        if ( bSig = 0 ) result := packFloatx80( zSign, 0, 0 );
        normalizeFloatx80Subnormal( bSig, bExp, bSig );
    end;
    zExp := aExp + bExp - $3FFE;
    mul64To128( aSig, bSig, zSig0, zSig1 );
    if ( 0 < (sbits64) zSig0 ) begin
        shortShift128Left( zSig0, zSig1, 1, zSig0, zSig1 );
        --zExp;
    end;
    result :=
        roundAndPackFloatx80(
            floatx80_rounding_precision, zSign, zExp, zSig0, zSig1 );

end;    }

{*----------------------------------------------------------------------------
| Returns the result of dividing the extended double-precision floating-point
| value `a' by the corresponding value `b'.  The operation is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}
   {
function floatx80_div(a: floatx80; b: floatx80 ): floatx80;
var
    aSign, bSign, zSign: flag;
    aExp, bExp, zExp: int32;
    aSig, bSig, zSig0, zSig1: bits64;
    rem0, rem1, rem2, term0, term1, term2: bits64;
    z: floatx80;
begin
    aSig := extractFloatx80Frac( a );
    aExp := extractFloatx80Exp( a );
    aSign := extractFloatx80Sign( a );
    bSig := extractFloatx80Frac( b );
    bExp := extractFloatx80Exp( b );
    bSign := extractFloatx80Sign( b );
    zSign := aSign xor bSign;
    if ( aExp = $7FFF ) begin
        if ( (bits64) ( aSig shl 1 ) ) result := propagateFloatx80NaN( a, b );
        if ( bExp = $7FFF ) begin
            if ( (bits64) ( bSig shl 1 ) ) result := propagateFloatx80NaN( a, b );
            goto invalid;
        end;
        result := packFloatx80( zSign, $7FFF, LIT64( $8000000000000000 ) );
    end;
    if ( bExp = $7FFF ) begin
        if ( (bits64) ( bSig shl 1 ) ) result := propagateFloatx80NaN( a, b );
        result := packFloatx80( zSign, 0, 0 );
    end;
    if ( bExp = 0 ) begin
        if ( bSig = 0 ) begin
            if ( ( aExp or aSig ) = 0 ) begin
 invalid:
                float_raise( float_flag_invalid );
                z.low := floatx80_default_nan_low;
                z.high := floatx80_default_nan_high;
                result := z;
            end;
            float_raise( float_flag_divbyzero );
            result := packFloatx80( zSign, $7FFF, LIT64( $8000000000000000 ) );
        end;
        normalizeFloatx80Subnormal( bSig, bExp, bSig );
    end;
    if ( aExp = 0 ) begin
        if ( aSig = 0 ) result := packFloatx80( zSign, 0, 0 );
        normalizeFloatx80Subnormal( aSig, aExp, aSig );
    end;
    zExp := aExp - bExp + $3FFE;
    rem1 := 0;
    if ( bSig <= aSig ) begin
        shift128Right( aSig, 0, 1, aSig, rem1 );
        ++zExp;
    end;
    zSig0 := estimateDiv128To64( aSig, rem1, bSig );
    mul64To128( bSig, zSig0, term0, term1 );
    sub128( aSig, rem1, term0, term1, rem0, rem1 );
    while ( (sbits64) rem0 < 0 ) begin
        --zSig0;
        add128( rem0, rem1, 0, bSig, rem0, rem1 );
    end;
    zSig1 := estimateDiv128To64( rem1, 0, bSig );
    if ( (bits64) ( zSig1 shl 1 ) <= 8 ) begin
        mul64To128( bSig, zSig1, term1, term2 );
        sub128( rem1, 0, term1, term2, rem1, rem2 );
        while ( (sbits64) rem1 < 0 ) begin
            --zSig1;
            add128( rem1, rem2, 0, bSig, rem1, rem2 );
        end;
        zSig1 or= ( ( rem1 or rem2 ) <> 0 );
    end;
    result :=
        roundAndPackFloatx80(
            floatx80_rounding_precision, zSign, zExp, zSig0, zSig1 );

end;  }

{*----------------------------------------------------------------------------
| Returns the remainder of the extended double-precision floating-point value
| `a' with respect to the corresponding value `b'.  The operation is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

{
function floatx80_rem(a: floatx80; b: floatx80 ): floatx80;
var
    aSign, bSign, zSign: flag;
    aExp, bExp, expDiff: int32;
    aSig0, aSig1, bSig: bits64;
    q, term0, term1, alternateASig0, alternateASig1: bits64;
    z: floatx80;
begin
    aSig0 := extractFloatx80Frac( a );
    aExp := extractFloatx80Exp( a );
    aSign := extractFloatx80Sign( a );
    bSig := extractFloatx80Frac( b );
    bExp := extractFloatx80Exp( b );
    bSign := extractFloatx80Sign( b );
    if ( aExp = $7FFF ) begin
        if (    (bits64) ( aSig0 shl 1 )
             or ( ( bExp = $7FFF ) and (bits64) ( bSig shl 1 ) ) ) begin
            result := propagateFloatx80NaN( a, b );
        end;
        goto invalid;
    end;
    if ( bExp = $7FFF ) begin
        if ( (bits64) ( bSig shl 1 ) ) result := propagateFloatx80NaN( a, b );
        result := a;
    end;
    if ( bExp = 0 ) begin
        if ( bSig = 0 ) begin
 invalid:
            float_raise( float_flag_invalid );
            z.low := floatx80_default_nan_low;
            z.high := floatx80_default_nan_high;
            result := z;
        end;
        normalizeFloatx80Subnormal( bSig, bExp, bSig );
    end;
    if ( aExp = 0 ) begin
        if ( (bits64) ( aSig0 shl 1 ) = 0 ) result := a;
        normalizeFloatx80Subnormal( aSig0, aExp, aSig0 );
    end;
    bSig or= LIT64( $8000000000000000 );
    zSign := aSign;
    expDiff := aExp - bExp;
    aSig1 := 0;
    if ( expDiff < 0 ) begin
        if ( expDiff < -1 ) result := a;
        shift128Right( aSig0, 0, 1, aSig0, aSig1 );
        expDiff := 0;
    end;
    q := ( bSig <= aSig0 );
    if ( q ) aSig0 -= bSig;
    expDiff -= 64;
    while ( 0 < expDiff ) begin
        q := estimateDiv128To64( aSig0, aSig1, bSig );
        q := ( 2 < q ) ? q - 2 : 0;
        mul64To128( bSig, q, term0, term1 );
        sub128( aSig0, aSig1, term0, term1, aSig0, aSig1 );
        shortShift128Left( aSig0, aSig1, 62, aSig0, aSig1 );
        expDiff -= 62;
    end;
    expDiff += 64;
    if ( 0 < expDiff ) begin
        q := estimateDiv128To64( aSig0, aSig1, bSig );
        q := ( 2 < q ) ? q - 2 : 0;
        q >>= 64 - expDiff;
        mul64To128( bSig, q shl ( 64 - expDiff ), term0, term1 );
        sub128( aSig0, aSig1, term0, term1, aSig0, aSig1 );
        shortShift128Left( 0, bSig, 64 - expDiff, term0, term1 );
        while ( le128( term0, term1, aSig0, aSig1 ) ) begin
            ++q;
            sub128( aSig0, aSig1, term0, term1, aSig0, aSig1 );
        end;
    end;
    else begin
        term1 := 0;
        term0 := bSig;
    end;
    sub128( term0, term1, aSig0, aSig1, alternateASig0, alternateASig1 );
    if (    lt128( alternateASig0, alternateASig1, aSig0, aSig1 )
         or (    eq128( alternateASig0, alternateASig1, aSig0, aSig1 )
              and ( q and 1 ) )
       ) begin
        aSig0 := alternateASig0;
        aSig1 := alternateASig1;
        zSign := ! zSign;
    end;
    result :=
        normalizeRoundAndPackFloatx80(
            80, zSign, bExp + expDiff, aSig0, aSig1 );

end;    }

{*----------------------------------------------------------------------------
| Returns the square root of the extended double-precision floating-point
| value `a'.  The operation is performed according to the IEC/IEEE Standard
| for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}
  {
function floatx80_sqrt(a: floatx80): floatx80;
var
    aSign: flag;
    aExp, zExp: int32;
    aSig0, aSig1, zSig0, zSig1, doubleZSig0: bits64;
    rem0, rem1, rem2, rem3, term0, term1, term2, term3: bits64;
    z: floatx80;
label
    invalid;
begin
    aSig0 := extractFloatx80Frac( a );
    aExp := extractFloatx80Exp( a );
    aSign := extractFloatx80Sign( a );
    if ( aExp = $7FFF ) begin
        if ( (bits64) ( aSig0 shl 1 ) ) result := propagateFloatx80NaN( a, a );
        if ( ! aSign ) result := a;
        goto invalid;
    end;
    if ( aSign ) begin
        if ( ( aExp or aSig0 ) = 0 ) result := a;
 invalid:
        float_raise( float_flag_invalid );
        z.low := floatx80_default_nan_low;
        z.high := floatx80_default_nan_high;
        result := z;
    end;
    if ( aExp = 0 ) begin
        if ( aSig0 = 0 ) result := packFloatx80( 0, 0, 0 );
        normalizeFloatx80Subnormal( aSig0, aExp, aSig0 );
    end;
    zExp := ( ( aExp - $3FFF )>>1 ) + $3FFF;
    zSig0 := estimateSqrt32( aExp, aSig0>>32 );
    shift128Right( aSig0, 0, 2 + ( aExp and 1 ), aSig0, aSig1 );
    zSig0 := estimateDiv128To64( aSig0, aSig1, zSig0 shl 32 ) + ( zSig0 shl 30 );
    doubleZSig0 := zSig0 shl 1;
    mul64To128( zSig0, zSig0, term0, term1 );
    sub128( aSig0, aSig1, term0, term1, rem0, rem1 );
    while ( (sbits64) rem0 < 0 ) begin
        --zSig0;
        doubleZSig0 -= 2;
        add128( rem0, rem1, zSig0>>63, doubleZSig0 or 1, rem0, rem1 );
    end;
    zSig1 := estimateDiv128To64( rem1, 0, doubleZSig0 );
    if ( ( zSig1 and LIT64( $3FFFFFFFFFFFFFFF ) ) <= 5 ) begin
        if ( zSig1 = 0 ) zSig1 := 1;
        mul64To128( doubleZSig0, zSig1, term1, term2 );
        sub128( rem1, 0, term1, term2, rem1, rem2 );
        mul64To128( zSig1, zSig1, term2, term3 );
        sub192( rem1, rem2, 0, 0, term2, term3, rem1, rem2, rem3 );
        while ( (sbits64) rem1 < 0 ) begin
            --zSig1;
            shortShift128Left( 0, zSig1, 1, term2, term3 );
            term3 or= 1;
            term2 or= doubleZSig0;
            add192( rem1, rem2, rem3, 0, term2, term3, rem1, rem2, rem3 );
        end;
        zSig1 or= ( ( rem1 or rem2 or rem3 ) <> 0 );
    end;
    shortShift128Left( 0, zSig1, 1, zSig0, zSig1 );
    zSig0 or= doubleZSig0;
    result :=
        roundAndPackFloatx80(
            floatx80_rounding_precision, 0, zExp, zSig0, zSig1 );

end;  }

{*----------------------------------------------------------------------------
| Returns 1 if the extended double-precision floating-point value `a' is
| equal to the corresponding value `b', and 0 otherwise.  The comparison is
| performed according to the IEC/IEEE Standard for Binary Floating-Point
| Arithmetic.
*----------------------------------------------------------------------------*}
    {
function floatx80_eq(a: floatx80; b: floatx80 ): flag;
begin
    if (    (    ( extractFloatx80Exp( a ) = $7FFF )
              and (bits64) ( extractFloatx80Frac( a ) shl 1 ) )
         or (    ( extractFloatx80Exp( b ) = $7FFF )
              and (bits64) ( extractFloatx80Frac( b ) shl 1 ) )
       ) begin
        if (    floatx80_is_signaling_nan( a )
             or floatx80_is_signaling_nan( b ) ) begin
            float_raise( float_flag_invalid );
        end;
        result := 0;
    end;
    result :=
           ( a.low = b.low )
        and (    ( a.high = b.high )
             or (    ( a.low = 0 )
                  and ( (bits16) ( ( a.high or b.high ) shl 1 ) = 0 ) )
           );

end;  }

{*----------------------------------------------------------------------------
| Returns 1 if the extended double-precision floating-point value `a' is
| less than or equal to the corresponding value `b', and 0 otherwise.  The
| comparison is performed according to the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

{
function floatx80_le(a: floatx80; b: floatx80 ): flag;
var
    aSign, bSign: flag;
begin
    if (    (    ( extractFloatx80Exp( a ) = $7FFF )
              and (bits64) ( extractFloatx80Frac( a ) shl 1 ) )
         or (    ( extractFloatx80Exp( b ) = $7FFF )
              and (bits64) ( extractFloatx80Frac( b ) shl 1 ) )
       ) begin
        float_raise( float_flag_invalid );
        result := 0;
    end;
    aSign := extractFloatx80Sign( a );
    bSign := extractFloatx80Sign( b );
    if ( aSign <> bSign ) begin
        result :=
               aSign
            or (    ( ( (bits16) ( ( a.high or b.high ) shl 1 ) ) or a.low or b.low )
                 = 0 );
    end;
    result :=
          aSign ? le128( b.high, b.low, a.high, a.low )
        : le128( a.high, a.low, b.high, b.low );

end;   }

{*----------------------------------------------------------------------------
| Returns 1 if the extended double-precision floating-point value `a' is
| less than the corresponding value `b', and 0 otherwise.  The comparison
| is performed according to the IEC/IEEE Standard for Binary Floating-Point
| Arithmetic.
*----------------------------------------------------------------------------*}

{
function floatx80_lt(a: floatx80; b: floatx80 ): flag;
var
    aSign, bSign: flag;
begin
    if (    (    ( extractFloatx80Exp( a ) = $7FFF )
              and (bits64) ( extractFloatx80Frac( a ) shl 1 ) )
         or (    ( extractFloatx80Exp( b ) = $7FFF )
              and (bits64) ( extractFloatx80Frac( b ) shl 1 ) )
       ) begin
        float_raise( float_flag_invalid );
        result := 0;
    end;
    aSign := extractFloatx80Sign( a );
    bSign := extractFloatx80Sign( b );
    if ( aSign <> bSign ) begin
        result :=
               aSign
            and (    ( ( (bits16) ( ( a.high or b.high ) shl 1 ) ) or a.low or b.low )
                 <> 0 );
    end;
    result :=
          aSign ? lt128( b.high, b.low, a.high, a.low )
        : lt128( a.high, a.low, b.high, b.low );

end; }

{*----------------------------------------------------------------------------
| Returns 1 if the extended double-precision floating-point value `a' is equal
| to the corresponding value `b', and 0 otherwise.  The invalid exception is
| raised if either operand is a NaN.  Otherwise, the comparison is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

{
function floatx80_eq_signaling(a: floatx80; b: floatx80 ): flag;
begin
    if (    (    ( extractFloatx80Exp( a ) = $7FFF )
              and (bits64) ( extractFloatx80Frac( a ) shl 1 ) )
         or (    ( extractFloatx80Exp( b ) = $7FFF )
              and (bits64) ( extractFloatx80Frac( b ) shl 1 ) )
       ) begin
        float_raise( float_flag_invalid );
        result := 0;
    end;
    result :=
           ( a.low = b.low )
        and (    ( a.high = b.high )
             or (    ( a.low = 0 )
                  and ( (bits16) ( ( a.high or b.high ) shl 1 ) = 0 ) )
           );

end;     }

{*----------------------------------------------------------------------------
| Returns 1 if the extended double-precision floating-point value `a' is less
| than or equal to the corresponding value `b', and 0 otherwise.  Quiet NaNs
| do not cause an exception.  Otherwise, the comparison is performed according
| to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

{
function floatx80_le_quiet(a: floatx80; b: floatx80 ): flag;
var
    aSign, bSign: flag;
begin
    if (    (    ( extractFloatx80Exp( a ) = $7FFF )
              and (bits64) ( extractFloatx80Frac( a ) shl 1 ) )
         or (    ( extractFloatx80Exp( b ) = $7FFF )
              and (bits64) ( extractFloatx80Frac( b ) shl 1 ) )
       ) begin
        if (    floatx80_is_signaling_nan( a )
             or floatx80_is_signaling_nan( b ) ) begin
            float_raise( float_flag_invalid );
        end;
        result := 0;
    end;
    aSign := extractFloatx80Sign( a );
    bSign := extractFloatx80Sign( b );
    if ( aSign <> bSign ) begin
        result :=
               aSign
            or (    ( ( (bits16) ( ( a.high or b.high ) shl 1 ) ) or a.low or b.low )
                 = 0 );
    end;
    result :=
          aSign ? le128( b.high, b.low, a.high, a.low )
        : le128( a.high, a.low, b.high, b.low );

end;   }

{*----------------------------------------------------------------------------
| Returns 1 if the extended double-precision floating-point value `a' is less
| than the corresponding value `b', and 0 otherwise.  Quiet NaNs do not cause
| an exception.  Otherwise, the comparison is performed according to the
| IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

{
function floatx80_lt_quiet(a: floatx80; b: floatx80 ): flag;
var
    aSign, bSign: flag;
begin
    if (    (    ( extractFloatx80Exp( a ) = $7FFF )
              and (bits64) ( extractFloatx80Frac( a ) shl 1 ) )
         or (    ( extractFloatx80Exp( b ) = $7FFF )
              and (bits64) ( extractFloatx80Frac( b ) shl 1 ) )
       ) begin
        if (    floatx80_is_signaling_nan( a )
             or floatx80_is_signaling_nan( b ) ) begin
            float_raise( float_flag_invalid );
        end;
        result := 0;
    end;
    aSign := extractFloatx80Sign( a );
    bSign := extractFloatx80Sign( b );
    if ( aSign <> bSign ) begin
        result :=
               aSign
            and (    ( ( (bits16) ( ( a.high or b.high ) shl 1 ) ) or a.low or b.low )
                 <> 0 );
    end;
    result :=
          aSign ? lt128( b.high, b.low, a.high, a.low )
        : lt128( a.high, a.low, b.high, b.low );

end;   }

{$endif FPC_SOFTFLOAT_FLOATX80}


{$ifdef FPC_SOFTFLOAT_FLOAT128}

{*----------------------------------------------------------------------------
| Returns the least-significant 64 fraction bits of the quadruple-precision
| floating-point value `a'.
*----------------------------------------------------------------------------*}

function extractFloat128Frac1(a : float128): bits64;
begin
    result:=a.low;
end;

{*----------------------------------------------------------------------------
| Returns the most-significant 48 fraction bits of the quadruple-precision
| floating-point value `a'.
*----------------------------------------------------------------------------*}

function extractFloat128Frac0(a : float128): bits64;
begin
    result:=a.high and int64($0000FFFFFFFFFFFF);
end;

{*----------------------------------------------------------------------------
| Returns the exponent bits of the quadruple-precision floating-point value
| `a'.
*----------------------------------------------------------------------------*}

function extractFloat128Exp(a : float128): int32;
begin
    result:=( a.high shr 48 ) and $7FFF;
end;

{*----------------------------------------------------------------------------
| Returns the sign bit of the quadruple-precision floating-point value `a'.
*----------------------------------------------------------------------------*}

function extractFloat128Sign(a : float128): flag;
begin
    result:=a.high shr 63;
end;

{*----------------------------------------------------------------------------
| Normalizes the subnormal quadruple-precision floating-point value
| represented by the denormalized significand formed by the concatenation of
| `aSig0' and `aSig1'.  The normalized exponent is stored at the location
| pointed to by `zExpPtr'.  The most significant 49 bits of the normalized
| significand are stored at the location pointed to by `zSig0Ptr', and the
| least significant 64 bits of the normalized significand are stored at the
| location pointed to by `zSig1Ptr'.
*----------------------------------------------------------------------------*}

procedure normalizeFloat128Subnormal(
     aSig0: bits64;
     aSig1: bits64;
     var zExpPtr: int32;
     var zSig0Ptr: bits64;
     var zSig1Ptr: bits64);
var
    shiftCount: int8;
begin
    if ( aSig0 = 0 ) then
    begin
        shiftCount := countLeadingZeros64( aSig1 ) - 15;
        if ( shiftCount < 0 ) then
        begin
            zSig0Ptr := aSig1 shr ( - shiftCount );
            zSig1Ptr := aSig1 shl ( shiftCount and 63 );
        end
        else begin
            zSig0Ptr := aSig1 shl shiftCount;
            zSig1Ptr := 0;
        end;
        zExpPtr := - shiftCount - 63;
    end
    else begin
        shiftCount := countLeadingZeros64( aSig0 ) - 15;
        shortShift128Left( aSig0, aSig1, shiftCount, zSig0Ptr, zSig1Ptr );
        zExpPtr := 1 - shiftCount;
    end;

end;

{*----------------------------------------------------------------------------
| Packs the sign `zSign', the exponent `zExp', and the significand formed
| by the concatenation of `zSig0' and `zSig1' into a quadruple-precision
| floating-point value, returning the result.  After being shifted into the
| proper positions, the three fields `zSign', `zExp', and `zSig0' are simply
| added together to form the most significant 32 bits of the result.  This
| means that any integer portion of `zSig0' will be added into the exponent.
| Since a properly normalized significand will have an integer portion equal
| to 1, the `zExp' input should be 1 less than the desired result exponent
| whenever `zSig0' and `zSig1' concatenated form a complete, normalized
| significand.
*----------------------------------------------------------------------------*}

function packFloat128( zSign: flag; zExp: int32; zSig0: bits64; zSig1: bits64) : float128;
var
    z: float128;
begin
    z.low := zSig1;
    z.high := ( ( bits64(zSign) ) shl 63 ) + ( ( bits64(zExp) ) shl 48 ) + zSig0;
    result:=z;
end;

{*----------------------------------------------------------------------------
| Takes an abstract floating-point value having sign `zSign', exponent `zExp',
| and extended significand formed by the concatenation of `zSig0', `zSig1',
| and `zSig2', and returns the proper quadruple-precision floating-point value
| corresponding to the abstract input.  Ordinarily, the abstract value is
| simply rounded and packed into the quadruple-precision format, with the
| inexact exception raised if the abstract input cannot be represented
| exactly.  However, if the abstract value is too large, the overflow and
| inexact exceptions are raised and an infinity or maximal finite value is
| returned.  If the abstract value is too small, the input value is rounded to
| a subnormal number, and the underflow and inexact exceptions are raised if
| the abstract input cannot be represented exactly as a subnormal quadruple-
| precision floating-point number.
|     The input significand must be normalized or smaller.  If the input
| significand is not normalized, `zExp' must be 0; in that case, the result
| returned is a subnormal number, and it must not require rounding.  In the
| usual case that the input significand is normalized, `zExp' must be 1 less
| than the ``true'' floating-point exponent.  The handling of underflow and
| overflow follows the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function roundAndPackFloat128(zSign: flag; zExp: int32; zSig0: bits64; zSig1: bits64; zSig2: bits64): float128;
var
    roundingMode: int8;
    roundNearestEven, increment, isTiny: flag;
begin
    roundingMode := softfloat_rounding_mode;
    roundNearestEven := ord( roundingMode = float_round_nearest_even );
    increment := ord( sbits64(zSig2) < 0 );
    if ( roundNearestEven=0 ) then
    begin
        if ( roundingMode = float_round_to_zero ) then
        begin
            increment := 0;
        end
        else begin
            if ( zSign<>0 ) then
            begin
                increment := ord( roundingMode = float_round_down ) and zSig2;
            end
            else begin
                increment := ord( roundingMode = float_round_up ) and zSig2;
            end;
        end;
    end;
    if ( $7FFD <= bits32(zExp) ) then
    begin
        if (    ord( $7FFD < zExp )
             or (    ord( zExp = $7FFD )
                  and eq128(
                         int64( $0001FFFFFFFFFFFF ),
                         int64( $FFFFFFFFFFFFFFFF ),
                         zSig0,
                         zSig1
                     )
                  and increment
                )
           )<>0 then
           begin
            float_raise( float_flag_overflow or float_flag_inexact );
            if (    ord( roundingMode = float_round_to_zero )
                 or ( zSign and ord( roundingMode = float_round_up ) )
                 or ( not(zSign) and ord( roundingMode = float_round_down ) )
               )<>0 then
               begin
                result :=
                    packFloat128(
                        zSign,
                        $7FFE,
                        int64( $0000FFFFFFFFFFFF ),
                        int64( $FFFFFFFFFFFFFFFF )
                    );
            end;
            result:=packFloat128( zSign, $7FFF, 0, 0 );
        end;
        if ( zExp < 0 ) then
        begin
            isTiny :=
                   ord(( float_detect_tininess = float_tininess_before_rounding )
                or ( zExp < -1 )
                or not( increment<>0 )
                or boolean(lt128(
                       zSig0,
                       zSig1,
                       int64( $0001FFFFFFFFFFFF ),
                       int64( $FFFFFFFFFFFFFFFF )
                   )));
            shift128ExtraRightJamming(
                zSig0, zSig1, zSig2, - zExp, zSig0, zSig1, zSig2 );
            zExp := 0;
            if ( isTiny and zSig2 )<>0 then
              float_raise( float_flag_underflow );
            if ( roundNearestEven<>0 ) then
            begin
                increment := ord( sbits64(zSig2) < 0 );
            end
            else begin
                if ( zSign<>0 ) then
                begin
                    increment := ord( roundingMode = float_round_down ) and zSig2;
                end
                else begin
                    increment := ord( roundingMode = float_round_up ) and zSig2;
                end;
            end;
        end;
    end;
    if ( zSig2<>0 ) then
      softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
    if ( increment<>0 ) then
    begin
        add128( zSig0, zSig1, 0, 1, zSig0, zSig1 );
        zSig1 := zSig1 and  not( ord( zSig2 + zSig2 = 0 ) and roundNearestEven );
    end
    else begin
        if ( ( zSig0 or zSig1 ) = 0 ) then
          zExp := 0;
    end;
    result:=packFloat128( zSign, zExp, zSig0, zSig1 );
end;

{*----------------------------------------------------------------------------
| Takes an abstract floating-point value having sign `zSign', exponent `zExp',
| and significand formed by the concatenation of `zSig0' and `zSig1', and
| returns the proper quadruple-precision floating-point value corresponding
| to the abstract input.  This routine is just like `roundAndPackFloat128'
| except that the input significand has fewer bits and does not have to be
| normalized.  In all cases, `zExp' must be 1 less than the ``true'' floating-
| point exponent.
*----------------------------------------------------------------------------*}

function normalizeRoundAndPackFloat128(zSign: flag; zExp: int32; zSig0: bits64; zSig1: bits64): float128;
var
    shiftCount: int8;
    zSig2: bits64;
begin
    if ( zSig0 = 0 ) then
    begin
        zSig0 := zSig1;
        zSig1 := 0;
        dec(zExp, 64);
    end;
    shiftCount := countLeadingZeros64( zSig0 ) - 15;
    if ( 0 <= shiftCount ) then
    begin
        zSig2 := 0;
        shortShift128Left( zSig0, zSig1, shiftCount, zSig0, zSig1 );
    end
    else begin
        shift128ExtraRightJamming(
            zSig0, zSig1, 0, - shiftCount, zSig0, zSig1, zSig2 );
    end;
    dec(zExp, shiftCount);
    result:=roundAndPackFloat128( zSign, zExp, zSig0, zSig1, zSig2 );

end;

{*----------------------------------------------------------------------------
| Returns the result of converting the quadruple-precision floating-point
| value `a' to the 32-bit two's complement integer format.  The conversion
| is performed according to the IEC/IEEE Standard for Binary Floating-Point
| Arithmetic---which means in particular that the conversion is rounded
| according to the current rounding mode.  If `a' is a NaN, the largest
| positive integer is returned.  Otherwise, if the conversion overflows, the
| largest integer with the same sign as `a' is returned.
*----------------------------------------------------------------------------*}

function float128_to_int32(a: float128): int32;
var
    aSign: flag;
    aExp, shiftCount: int32;
    aSig0, aSig1: bits64;
begin
    aSig1 := extractFloat128Frac1( a );
    aSig0 := extractFloat128Frac0( a );
    aExp := extractFloat128Exp( a );
    aSign := extractFloat128Sign( a );
    if ( ord( aExp = $7FFF ) and ( aSig0 or aSig1 ) )<>0 then
      aSign := 0;
    if ( aExp<>0 ) then
      aSig0 := aSig0 or int64( $0001000000000000 );
    aSig0 := aSig0 or ord( aSig1 <> 0 );
    shiftCount := $4028 - aExp;
    if ( 0 < shiftCount ) then
      shift64RightJamming( aSig0, shiftCount, aSig0 );
    result := roundAndPackInt32( aSign, aSig0 );

end;

{*----------------------------------------------------------------------------
| Returns the result of converting the quadruple-precision floating-point
| value `a' to the 32-bit two's complement integer format.  The conversion
| is performed according to the IEC/IEEE Standard for Binary Floating-Point
| Arithmetic, except that the conversion is always rounded toward zero.  If
| `a' is a NaN, the largest positive integer is returned.  Otherwise, if the
| conversion overflows, the largest integer with the same sign as `a' is
| returned.
*----------------------------------------------------------------------------*}

function float128_to_int32_round_to_zero(a: float128): int32;
var
    aSign: flag;
    aExp, shiftCount: int32;
    aSig0, aSig1, savedASig: bits64;
    z: int32;
label
    invalid;
begin
    aSig1 := extractFloat128Frac1( a );
    aSig0 := extractFloat128Frac0( a );
    aExp := extractFloat128Exp( a );
    aSign := extractFloat128Sign( a );
    aSig0 := aSig0 or ord( aSig1 <> 0 );
    if ( $401E < aExp ) then
    begin
        if ( ord( aExp = $7FFF ) and aSig0 )<>0 then
          aSign := 0;
        goto invalid;
    end
    else if ( aExp < $3FFF ) then
    begin
        if ( aExp or aSig0 )<>0 then
          softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
        result := 0;
        exit;
    end;
    aSig0 := aSig0 or int64( $0001000000000000 );
    shiftCount := $402F - aExp;
    savedASig := aSig0;
    aSig0 := aSig0 shr shiftCount;
    z := aSig0;
    if ( aSign )<>0 then
      z := - z;
    if ( ord( z < 0 ) xor aSign )<>0 then
    begin
 invalid:
        float_raise( float_flag_invalid );
        if aSign<>0 then
          result:=$80000000
        else
          result:=$7FFFFFFF;
        exit;
    end;
    if ( ( aSig0 shl shiftCount ) <> savedASig ) then
    begin
        softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
    end;
    result := z;
end;

{*----------------------------------------------------------------------------
| Returns the result of converting the quadruple-precision floating-point
| value `a' to the 64-bit two's complement integer format.  The conversion
| is performed according to the IEC/IEEE Standard for Binary Floating-Point
| Arithmetic---which means in particular that the conversion is rounded
| according to the current rounding mode.  If `a' is a NaN, the largest
| positive integer is returned.  Otherwise, if the conversion overflows, the
| largest integer with the same sign as `a' is returned.
*----------------------------------------------------------------------------*}

function float128_to_int64(a: float128): int64;
var
    aSign: flag;
    aExp, shiftCount: int32;
    aSig0, aSig1: bits64;
begin
    aSig1 := extractFloat128Frac1( a );
    aSig0 := extractFloat128Frac0( a );
    aExp := extractFloat128Exp( a );
    aSign := extractFloat128Sign( a );
    if ( aExp<>0 ) then
      aSig0 := aSig0 or int64( $0001000000000000 );
    shiftCount := $402F - aExp;
    if ( shiftCount <= 0 ) then
    begin
        if ( $403E < aExp ) then
        begin
            float_raise( float_flag_invalid );
            if (    (aSign=0)
                 or (    ( aExp = $7FFF )
                      and ( (aSig1<>0) or ( aSig0 <> int64( $0001000000000000 ) ) )
                    )
               ) then
               begin
                result := int64( $7FFFFFFFFFFFFFFF );
            end;
            result := int64( $8000000000000000 );
        end;
        shortShift128Left( aSig0, aSig1, - shiftCount, aSig0, aSig1 );
    end
    else begin
        shift64ExtraRightJamming( aSig0, aSig1, shiftCount, aSig0, aSig1 );
    end;
    result := roundAndPackInt64( aSign, aSig0, aSig1 );

end;

{*----------------------------------------------------------------------------
| Returns the result of converting the quadruple-precision floating-point
| value `a' to the 64-bit two's complement integer format.  The conversion
| is performed according to the IEC/IEEE Standard for Binary Floating-Point
| Arithmetic, except that the conversion is always rounded toward zero.
| If `a' is a NaN, the largest positive integer is returned.  Otherwise, if
| the conversion overflows, the largest integer with the same sign as `a' is
| returned.
*----------------------------------------------------------------------------*}

function float128_to_int64_round_to_zero(a: float128): int64;
var
    aSign: flag;
    aExp, shiftCount: int32;
    aSig0, aSig1: bits64;
    z: int64;
begin
    aSig1 := extractFloat128Frac1( a );
    aSig0 := extractFloat128Frac0( a );
    aExp := extractFloat128Exp( a );
    aSign := extractFloat128Sign( a );
    if ( aExp<>0 ) then
      aSig0 := aSig0 or int64( $0001000000000000 );
    shiftCount := aExp - $402F;
    if ( 0 < shiftCount ) then
    begin
        if ( $403E <= aExp ) then
        begin
            aSig0 := aSig0 and int64( $0000FFFFFFFFFFFF );
            if (    ( a.high = int64( $C03E000000000000 ) )
                 and ( aSig1 < int64( $0002000000000000 ) ) ) then
            begin
                if ( aSig1<>0 ) then
                  softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
            end
            else begin
                float_raise( float_flag_invalid );
                if ( (aSign=0) or ( ( aExp = $7FFF ) and (( aSig0 or aSig1 )<>0) ) ) then
                begin
                    result := int64( $7FFFFFFFFFFFFFFF );
                    exit;
                end;
            end;
            result := int64( $8000000000000000 );
            exit;
        end;
        z := ( aSig0 shl shiftCount ) or ( aSig1>>( ( - shiftCount ) and 63 ) );
        if ( int64( aSig1 shl shiftCount )<>0 ) then
        begin
            softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
        end;
    end
    else begin
        if ( aExp < $3FFF ) then
        begin
            if ( aExp or aSig0 or aSig1 )<>0 then
            begin
                softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
            end;
            result := 0;
            exit;
        end;
        z := aSig0 shr ( - shiftCount );
        if (    (aSig1<>0)
             or ( (shiftCount<>0) and (int64( aSig0 shl ( shiftCount and 63 ) )<>0) ) ) then
        begin
            softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
        end;
    end;
    if ( aSign<>0 ) then
      z := - z;
    result := z;

end;

{*----------------------------------------------------------------------------
| Returns the result of converting the quadruple-precision floating-point
| value `a' to the single-precision floating-point format.  The conversion
| is performed according to the IEC/IEEE Standard for Binary Floating-Point
| Arithmetic.
*----------------------------------------------------------------------------*}

function float128_to_float32(a: float128): float32;
var
    aSign: flag;
    aExp: int32;
    aSig0, aSig1: bits64;
    zSig: bits32;
begin
    aSig1 := extractFloat128Frac1( a );
    aSig0 := extractFloat128Frac0( a );
    aExp := extractFloat128Exp( a );
    aSign := extractFloat128Sign( a );
    if ( aExp = $7FFF ) then
    begin
        if ( aSig0 or aSig1 )<>0 then
        begin
            result := commonNaNToFloat32( float128ToCommonNaN( a ) );
            exit;
        end;
        result := packFloat32( aSign, $FF, 0 );
        exit;
    end;
    aSig0 := aSig0 or ord( aSig1 <> 0 );
    shift64RightJamming( aSig0, 18, aSig0 );
    zSig := aSig0;
    if ( aExp or zSig )<>0 then
    begin
        zSig := zSig or $40000000;
        dec(aExp,$3F81);
    end;
    result := roundAndPackFloat32( aSign, aExp, zSig );

end;

{*----------------------------------------------------------------------------
| Returns the result of converting the quadruple-precision floating-point
| value `a' to the double-precision floating-point format.  The conversion
| is performed according to the IEC/IEEE Standard for Binary Floating-Point
| Arithmetic.
*----------------------------------------------------------------------------*}

function float128_to_float64(a: float128): float64;
var
    aSign: flag;
    aExp: int32;
    aSig0, aSig1: bits64;
begin
    aSig1 := extractFloat128Frac1( a );
    aSig0 := extractFloat128Frac0( a );
    aExp := extractFloat128Exp( a );
    aSign := extractFloat128Sign( a );
    if ( aExp = $7FFF ) then
    begin
        if ( aSig0 or aSig1 )<>0 then
        begin
            commonNaNToFloat64( float128ToCommonNaN( a ),result);
            exit;
        end;
        result:=packFloat64( aSign, $7FF, 0);
        exit;
    end;
    shortShift128Left( aSig0, aSig1, 14, aSig0, aSig1 );
    aSig0 := aSig0 or ord( aSig1 <> 0 );
    if ( aExp or aSig0 )<>0 then
    begin
        aSig0 := aSig0 or int64( $4000000000000000 );
        dec(aExp,$3C01);
    end;
    result := roundAndPackFloat64( aSign, aExp, aSig0 );
end;

{$ifdef FPC_SOFTFLOAT_FLOATX80}

{*----------------------------------------------------------------------------
| Returns the result of converting the quadruple-precision floating-point
| value `a' to the extended double-precision floating-point format.  The
| conversion is performed according to the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

{
function float128_to_floatx80(a: float128): floatx80;
var
    aSign: flag;
    aExp: int32;
    aSig0, aSig1: bits64;
begin
    aSig1 := extractFloat128Frac1( a );
    aSig0 := extractFloat128Frac0( a );
    aExp := extractFloat128Exp( a );
    aSign := extractFloat128Sign( a );
    if ( aExp = $7FFF ) begin
        if ( aSig0 or aSig1 ) begin
            result := commonNaNToFloatx80( float128ToCommonNaN( a ) );
            exit;
        end;
        result := packFloatx80( aSign, $7FFF, int64( $8000000000000000 ) );
        exit;
    end;
    if ( aExp = 0 ) begin
        if ( ( aSig0 or aSig1 ) = 0 ) then
          begin
            result := packFloatx80( aSign, 0, 0 );
            exit;
          end;
        normalizeFloat128Subnormal( aSig0, aSig1, aExp, aSig0, aSig1 );
    end;
    else begin
        aSig0 or= int64( $0001000000000000 );
    end;
    shortShift128Left( aSig0, aSig1, 15, aSig0, aSig1 );
    result := roundAndPackFloatx80( 80, aSign, aExp, aSig0, aSig1 );

end;   }

{$endif FPC_SOFTFLOAT_FLOATX80}

{*----------------------------------------------------------------------------
| Rounds the quadruple-precision floating-point value `a' to an integer, and
| Returns the result as a quadruple-precision floating-point value.  The
| operation is performed according to the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function float128_round_to_int(a: float128): float128;
var
    aSign: flag;
    aExp: int32;
    lastBitMask, roundBitsMask: bits64;
    roundingMode: int8;
    z: float128;
begin
    aExp := extractFloat128Exp( a );
    if ( $402F <= aExp ) then
    begin
        if ( $406F <= aExp ) then
        begin
            if (    ( aExp = $7FFF )
                 and (( extractFloat128Frac0( a ) or extractFloat128Frac1( a ) )<>0)
               ) then
               begin
                result := propagateFloat128NaN( a, a );
                exit;
            end;
            result := a;
            exit;
        end;
        lastBitMask := 1;
        lastBitMask := ( lastBitMask shl ( $406E - aExp ) ) shl 1;
        roundBitsMask := lastBitMask - 1;
        z := a;
        roundingMode := softfloat_rounding_mode;
        if ( roundingMode = float_round_nearest_even ) then
        begin
            if ( lastBitMask )<>0 then
            begin
                add128( z.high, z.low, 0, lastBitMask shr 1, z.high, z.low );
                if ( ( z.low and roundBitsMask ) = 0 ) then
                  z.low := z.low and not(lastBitMask);
            end
            else begin
                if ( sbits64(z.low) < 0 ) then
                begin
                    inc(z.high);
                    if ( bits64( z.low shl 1 ) = 0 ) then
                      z.high := z.high and not(1);
                end;
            end;
        end
        else if ( roundingMode <> float_round_to_zero ) then
        begin
            if (   extractFloat128Sign( z )
                 xor ord( roundingMode = float_round_up ) )<>0 then
            begin
                add128( z.high, z.low, 0, roundBitsMask, z.high, z.low );
            end;
        end;
        z.low := z.low and not(roundBitsMask);
    end
    else begin
        if ( aExp < $3FFF ) then
        begin
            if ( ( ( bits64( a.high shl 1 ) ) or a.low ) = 0 ) then
              begin
                result := a;
                exit;
              end;
            softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
            aSign := extractFloat128Sign( a );
            case softfloat_rounding_mode of
            float_round_nearest_even:
                if (    ( aExp = $3FFE )
                     and (   (extractFloat128Frac0( a )<>0)
                          or (extractFloat128Frac1( a )<>0) )
                   ) then begin
                   begin
                     result := packFloat128( aSign, $3FFF, 0, 0 );
                     exit;
                   end;
                end;
             float_round_down:
               begin
                 if aSign<>0 then
                   result:=packFloat128( 1, $3FFF, 0, 0 )
                 else
                   result:=packFloat128( 0, 0, 0, 0 );
                 exit;
               end;
             float_round_up:
               begin
                 if aSign<>0 then
                   result := packFloat128( 1, 0, 0, 0 )
                 else
                   result:=packFloat128( 0, $3FFF, 0, 0 );
                exit;
               end;
            end;
            result := packFloat128( aSign, 0, 0, 0 );
            exit;
        end;
        lastBitMask := 1;
        lastBitMask  := lastBitMask shl ($402F - aExp);
        roundBitsMask := lastBitMask - 1;
        z.low := 0;
        z.high := a.high;
        roundingMode := softfloat_rounding_mode;
        if ( roundingMode = float_round_nearest_even ) then begin
            inc(z.high,lastBitMask shr 1);
            if ( ( ( z.high and roundBitsMask ) or a.low ) = 0 ) then begin
                z.high := z.high and not(lastBitMask);
            end;
        end
        else if ( roundingMode <> float_round_to_zero ) then begin
            if (   (extractFloat128Sign( z )<>0)
                 xor ( roundingMode = float_round_up ) ) then begin
                z.high := z.high or ord( a.low <> 0 );
                z.high := z.high+roundBitsMask;
            end;
        end;
        z.high := z.high and not(roundBitsMask);
    end;
    if ( ( z.low <> a.low ) or ( z.high <> a.high ) ) then begin
        softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
    end;
    result := z;

end;

{*----------------------------------------------------------------------------
| Returns the result of adding the absolute values of the quadruple-precision
| floating-point values `a' and `b'.  If `zSign' is 1, the sum is negated
| before being returned.  `zSign' is ignored if the result is a NaN.
| The addition is performed according to the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function addFloat128Sigs(a,b : float128; zSign : flag ): float128;
var
    aExp, bExp, zExp: int32;
    aSig0, aSig1, bSig0, bSig1, zSig0, zSig1, zSig2: bits64;
    expDiff: int32;
label
    shiftRight1,roundAndPack;
begin
    aSig1 := extractFloat128Frac1( a );
    aSig0 := extractFloat128Frac0( a );
    aExp := extractFloat128Exp( a );
    bSig1 := extractFloat128Frac1( b );
    bSig0 := extractFloat128Frac0( b );
    bExp := extractFloat128Exp( b );
    expDiff := aExp - bExp;
    if ( 0 < expDiff ) then begin
        if ( aExp = $7FFF ) then begin
            if ( aSig0 or aSig1 )<>0 then
              begin
                result := propagateFloat128NaN( a, b );
                exit;
              end;
            result := a;
            exit;
        end;
        if ( bExp = 0 ) then begin
            dec(expDiff);
        end
        else begin
            bSig0 := bSig0 or int64( $0001000000000000 );
        end;
        shift128ExtraRightJamming(
            bSig0, bSig1, 0, expDiff, bSig0, bSig1, zSig2 );
        zExp := aExp;
    end
    else if ( expDiff < 0 ) then begin
        if ( bExp = $7FFF ) then begin
            if ( bSig0 or bSig1 )<>0 then
              begin
                result := propagateFloat128NaN( a, b );
                exit;
              end;
            result := packFloat128( zSign, $7FFF, 0, 0 );
            exit;
        end;
        if ( aExp = 0 ) then begin
            inc(expDiff);
        end
        else begin
            aSig0 := aSig0 or int64( $0001000000000000 );
        end;
        shift128ExtraRightJamming(
            aSig0, aSig1, 0, - expDiff, aSig0, aSig1, zSig2 );
        zExp := bExp;
    end
    else begin
        if ( aExp = $7FFF ) then begin
            if ( aSig0 or aSig1 or bSig0 or bSig1 )<>0 then begin
                result := propagateFloat128NaN( a, b );
                exit;
            end;
            result := a;
            exit;
        end;
        add128( aSig0, aSig1, bSig0, bSig1, zSig0, zSig1 );
        if ( aExp = 0 ) then
          begin
            result := packFloat128( zSign, 0, zSig0, zSig1 );
            exit;
          end;
        zSig2 := 0;
        zSig0 := zSig0 or int64( $0002000000000000 );
        zExp := aExp;
        goto shiftRight1;
    end;
    aSig0 := aSig0 or int64( $0001000000000000 );
    add128( aSig0, aSig1, bSig0, bSig1, zSig0, zSig1 );
    dec(zExp);
    if ( zSig0 < int64( $0002000000000000 ) ) then goto roundAndPack;
    inc(zExp);
 shiftRight1:
    shift128ExtraRightJamming(
        zSig0, zSig1, zSig2, 1, zSig0, zSig1, zSig2 );
 roundAndPack:
    result := roundAndPackFloat128( zSign, zExp, zSig0, zSig1, zSig2 );

end;

{*----------------------------------------------------------------------------
| Returns the result of subtracting the absolute values of the quadruple-
| precision floating-point values `a' and `b'.  If `zSign' is 1, the
| difference is negated before being returned.  `zSign' is ignored if the
| result is a NaN.  The subtraction is performed according to the IEC/IEEE
| Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function subFloat128Sigs( a, b : float128;  zSign : flag): float128;
var
    aExp, bExp, zExp: int32;
    aSig0, aSig1, bSig0, bSig1, zSig0, zSig1: bits64;
    expDiff: int32;
    z: float128;
label
    aExpBigger,bExpBigger,aBigger,bBigger,normalizeRoundAndPack;
begin
    aSig1 := extractFloat128Frac1( a );
    aSig0 := extractFloat128Frac0( a );
    aExp := extractFloat128Exp( a );
    bSig1 := extractFloat128Frac1( b );
    bSig0 := extractFloat128Frac0( b );
    bExp := extractFloat128Exp( b );
    expDiff := aExp - bExp;
    shortShift128Left( aSig0, aSig1, 14, aSig0, aSig1 );
    shortShift128Left( bSig0, bSig1, 14, bSig0, bSig1 );
    if ( 0 < expDiff ) then goto aExpBigger;
    if ( expDiff < 0 ) then goto bExpBigger;
    if ( aExp = $7FFF ) then begin
        if ( aSig0 or aSig1 or bSig0 or bSig1 )<>0 then begin
            result := propagateFloat128NaN( a, b );
            exit;
        end;
        float_raise( float_flag_invalid );
        z.low := float128_default_nan_low;
        z.high := float128_default_nan_high;
        result := z;
        exit;
    end;
    if ( aExp = 0 ) then begin
        aExp := 1;
        bExp := 1;
    end;
    if ( bSig0 < aSig0 ) then goto aBigger;
    if ( aSig0 < bSig0 ) then goto bBigger;
    if ( bSig1 < aSig1 ) then goto aBigger;
    if ( aSig1 < bSig1 ) then goto bBigger;
    result := packFloat128( ord(softfloat_rounding_mode = float_round_down), 0, 0, 0 );
    exit;
 bExpBigger:
    if ( bExp = $7FFF ) then begin
        if ( bSig0 or bSig1 )<>0 then
          begin
            result := propagateFloat128NaN( a, b );
            exit;
          end;

        result := packFloat128( zSign xor 1, $7FFF, 0, 0 );
        exit;
    end;
    if ( aExp = 0 ) then begin
        inc(expDiff);
    end
    else begin
        aSig0 := aSig0 or int64( $4000000000000000 );
    end;
    shift128RightJamming( aSig0, aSig1, - expDiff, aSig0, aSig1 );
    bSig0 := bSig0 or int64( $4000000000000000 );
 bBigger:
    sub128( bSig0, bSig1, aSig0, aSig1, zSig0, zSig1 );
    zExp := bExp;
    zSign := zSign xor 1;
    goto normalizeRoundAndPack;
 aExpBigger:
    if ( aExp = $7FFF ) then begin
        if ( aSig0 or aSig1 )<>0 then
          begin
            result := propagateFloat128NaN( a, b );
            exit;
          end;
        result := a;
        exit;
    end;
    if ( bExp = 0 ) then begin
        dec(expDiff);
    end
    else begin
        bSig0 := bSig0 or int64( $4000000000000000 );
    end;
    shift128RightJamming( bSig0, bSig1, expDiff, bSig0, bSig1 );
    aSig0 := aSig0 or int64( $4000000000000000 );
 aBigger:
    sub128( aSig0, aSig1, bSig0, bSig1, zSig0, zSig1 );
    zExp := aExp;
 normalizeRoundAndPack:
    dec(zExp);
    result := normalizeRoundAndPackFloat128( zSign, zExp - 14, zSig0, zSig1 );

end;

{*----------------------------------------------------------------------------
| Returns the result of adding the quadruple-precision floating-point values
| `a' and `b'.  The operation is performed according to the IEC/IEEE Standard
| for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function float128_add(a: float128; b: float128): float128;
var
    aSign, bSign: flag;
begin
    aSign := extractFloat128Sign( a );
    bSign := extractFloat128Sign( b );
    if ( aSign = bSign ) then begin
        result := addFloat128Sigs( a, b, aSign );
    end
    else begin
        result := subFloat128Sigs( a, b, aSign );
    end;

end;

{*----------------------------------------------------------------------------
| Returns the result of subtracting the quadruple-precision floating-point
| values `a' and `b'.  The operation is performed according to the IEC/IEEE
| Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function float128_sub(a: float128; b: float128): float128;
var
    aSign, bSign: flag;
begin
    aSign := extractFloat128Sign( a );
    bSign := extractFloat128Sign( b );
    if ( aSign = bSign ) then begin
        result := subFloat128Sigs( a, b, aSign );
    end
    else begin
        result := addFloat128Sigs( a, b, aSign );
    end;

end;

{*----------------------------------------------------------------------------
| Returns the result of multiplying the quadruple-precision floating-point
| values `a' and `b'.  The operation is performed according to the IEC/IEEE
| Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function float128_mul(a: float128; b: float128): float128;
var
    aSign, bSign, zSign: flag;
    aExp, bExp, zExp: int32;
    aSig0, aSig1, bSig0, bSig1, zSig0, zSig1, zSig2, zSig3: bits64;
    z: float128;
label
    invalid;
begin
    aSig1 := extractFloat128Frac1( a );
    aSig0 := extractFloat128Frac0( a );
    aExp := extractFloat128Exp( a );
    aSign := extractFloat128Sign( a );
    bSig1 := extractFloat128Frac1( b );
    bSig0 := extractFloat128Frac0( b );
    bExp := extractFloat128Exp( b );
    bSign := extractFloat128Sign( b );
    zSign := aSign xor bSign;
    if ( aExp = $7FFF ) then begin
        if (    (( aSig0 or aSig1 )<>0)
             or ( ( bExp = $7FFF ) and (( bSig0 or bSig1 )<>0) ) ) then begin
            result := propagateFloat128NaN( a, b );
            exit;
        end;
        if ( ( bExp or bSig0 or bSig1 ) = 0 ) then goto invalid;
        result := packFloat128( zSign, $7FFF, 0, 0 );
        exit;
    end;
    if ( bExp = $7FFF ) then begin
        if ( bSig0 or bSig1 )<>0 then
          begin
            result := propagateFloat128NaN( a, b );
            exit;
          end;
        if ( ( aExp or aSig0 or aSig1 ) = 0 ) then begin
 invalid:
            float_raise( float_flag_invalid );
            z.low := float128_default_nan_low;
            z.high := float128_default_nan_high;
            result := z;
            exit;
        end;
        result := packFloat128( zSign, $7FFF, 0, 0 );
        exit;
    end;
    if ( aExp = 0 ) then begin
        if ( ( aSig0 or aSig1 ) = 0 ) then
          begin
            result := packFloat128( zSign, 0, 0, 0 );
            exit;
          end;
        normalizeFloat128Subnormal( aSig0, aSig1, aExp, aSig0, aSig1 );
    end;
    if ( bExp = 0 ) then begin
        if ( ( bSig0 or bSig1 ) = 0 ) then
          begin
            result := packFloat128( zSign, 0, 0, 0 );
            exit;
          end;
        normalizeFloat128Subnormal( bSig0, bSig1, bExp, bSig0, bSig1 );
    end;
    zExp := aExp + bExp - $4000;
    aSig0 := aSig0 or int64( $0001000000000000 );
    shortShift128Left( bSig0, bSig1, 16, bSig0, bSig1 );
    mul128To256( aSig0, aSig1, bSig0, bSig1, zSig0, zSig1, zSig2, zSig3 );
    add128( zSig0, zSig1, aSig0, aSig1, zSig0, zSig1 );
    zSig2 := zSig2 or ord( zSig3 <> 0 );
    if ( int64( $0002000000000000 ) <= zSig0 ) then begin
        shift128ExtraRightJamming(
            zSig0, zSig1, zSig2, 1, zSig0, zSig1, zSig2 );
        inc(zExp);
    end;
    result := roundAndPackFloat128( zSign, zExp, zSig0, zSig1, zSig2 );

end;

{*----------------------------------------------------------------------------
| Returns the result of dividing the quadruple-precision floating-point value
| `a' by the corresponding value `b'.  The operation is performed according to
| the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function float128_div(a: float128; b: float128): float128;
var
    aSign, bSign, zSign: flag;
    aExp, bExp, zExp: int32;
    aSig0, aSig1, bSig0, bSig1, zSig0, zSig1, zSig2: bits64;
    rem0, rem1, rem2, rem3, term0, term1, term2, term3: bits64;
    z: float128;
label
    invalid;
begin
    aSig1 := extractFloat128Frac1( a );
    aSig0 := extractFloat128Frac0( a );
    aExp := extractFloat128Exp( a );
    aSign := extractFloat128Sign( a );
    bSig1 := extractFloat128Frac1( b );
    bSig0 := extractFloat128Frac0( b );
    bExp := extractFloat128Exp( b );
    bSign := extractFloat128Sign( b );
    zSign := aSign xor bSign;
    if ( aExp = $7FFF ) then begin
        if ( aSig0 or aSig1 )<>0 then
        begin
          result := propagateFloat128NaN( a, b );
          exit;
        end;
        if ( bExp = $7FFF ) then begin
            if ( bSig0 or bSig1 )<>0 then
              begin
                result := propagateFloat128NaN( a, b );
                exit;
              end;
            goto invalid;
        end;
        result := packFloat128( zSign, $7FFF, 0, 0 );
        exit;
    end;
    if ( bExp = $7FFF ) then begin
        if ( bSig0 or bSig1 )<>0 then
          begin
            result := propagateFloat128NaN( a, b );
            exit;
          end;
        result := packFloat128( zSign, 0, 0, 0 );
        exit;
    end;
    if ( bExp = 0 ) then begin
        if ( ( bSig0 or bSig1 ) = 0 ) then begin
            if ( ( aExp or aSig0 or aSig1 ) = 0 ) then begin
 invalid:
                float_raise( float_flag_invalid );
                z.low := float128_default_nan_low;
                z.high := float128_default_nan_high;
                result := z;
                exit;
            end;
            float_raise( float_flag_divbyzero );
            result := packFloat128( zSign, $7FFF, 0, 0 );
            exit;
        end;
        normalizeFloat128Subnormal( bSig0, bSig1, bExp, bSig0, bSig1 );
    end;
    if ( aExp = 0 ) then begin
        if ( ( aSig0 or aSig1 ) = 0 ) then
          begin
            result := packFloat128( zSign, 0, 0, 0 );
            exit;
          end;
        normalizeFloat128Subnormal( aSig0, aSig1, aExp, aSig0, aSig1 );
    end;
    zExp := aExp - bExp + $3FFD;
    shortShift128Left(
        aSig0 or int64( $0001000000000000 ), aSig1, 15, aSig0, aSig1 );
    shortShift128Left(
        bSig0 or int64( $0001000000000000 ), bSig1, 15, bSig0, bSig1 );
    if ( le128( bSig0, bSig1, aSig0, aSig1 )<>0 ) then begin
        shift128Right( aSig0, aSig1, 1, aSig0, aSig1 );
        inc(zExp);
    end;
    zSig0 := estimateDiv128To64( aSig0, aSig1, bSig0 );
    mul128By64To192( bSig0, bSig1, zSig0, term0, term1, term2 );
    sub192( aSig0, aSig1, 0, term0, term1, term2, rem0, rem1, rem2 );
    while ( sbits64(rem0) < 0 ) do begin
        dec(zSig0);
        add192( rem0, rem1, rem2, 0, bSig0, bSig1, rem0, rem1, rem2 );
    end;
    zSig1 := estimateDiv128To64( rem1, rem2, bSig0 );
    if ( ( zSig1 and $3FFF ) <= 4 ) then begin
        mul128By64To192( bSig0, bSig1, zSig1, term1, term2, term3 );
        sub192( rem1, rem2, 0, term1, term2, term3, rem1, rem2, rem3 );
        while ( sbits64(rem1) < 0 ) do begin
            dec(zSig1);
            add192( rem1, rem2, rem3, 0, bSig0, bSig1, rem1, rem2, rem3 );
        end;
        zSig1 := zSig1 or ord( ( rem1 or rem2 or rem3 ) <> 0 );
    end;
    shift128ExtraRightJamming( zSig0, zSig1, 0, 15, zSig0, zSig1, zSig2 );
    result := roundAndPackFloat128( zSign, zExp, zSig0, zSig1, zSig2 );

end;

{*----------------------------------------------------------------------------
| Returns the remainder of the quadruple-precision floating-point value `a'
| with respect to the corresponding value `b'.  The operation is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function float128_rem(a: float128; b: float128): float128;
var
    aSign, bSign, zSign: flag;
    aExp, bExp, expDiff: int32;
    aSig0, aSig1, bSig0, bSig1, q, term0, term1, term2: bits64;
    allZero, alternateASig0, alternateASig1, sigMean1: bits64;
    sigMean0: sbits64;
    z: float128;
label
    invalid;
begin
    aSig1 := extractFloat128Frac1( a );
    aSig0 := extractFloat128Frac0( a );
    aExp := extractFloat128Exp( a );
    aSign := extractFloat128Sign( a );
    bSig1 := extractFloat128Frac1( b );
    bSig0 := extractFloat128Frac0( b );
    bExp := extractFloat128Exp( b );
    bSign := extractFloat128Sign( b );
    if ( aExp = $7FFF ) then begin
        if (    (( aSig0 or aSig1 )<>0)
             or ( ( bExp = $7FFF ) and (( bSig0 or bSig1 )<>0) ) ) then begin
            result := propagateFloat128NaN( a, b );
            exit;
        end;
        goto invalid;
    end;
    if ( bExp = $7FFF ) then begin
        if ( bSig0 or bSig1 )<>0 then
          begin
            result := propagateFloat128NaN( a, b );
            exit;
          end;
        result := a;
        exit;
    end;
    if ( bExp = 0 ) then begin
        if ( ( bSig0 or bSig1 ) = 0 ) then begin
 invalid:
            float_raise( float_flag_invalid );
            z.low := float128_default_nan_low;
            z.high := float128_default_nan_high;
            result := z;
            exit;
        end;
        normalizeFloat128Subnormal( bSig0, bSig1, bExp, bSig0, bSig1 );
    end;
    if ( aExp = 0 ) then begin
        if ( ( aSig0 or aSig1 ) = 0 ) then
          begin
            result := a;
            exit;
          end;
        normalizeFloat128Subnormal( aSig0, aSig1, aExp, aSig0, aSig1 );
    end;
    expDiff := aExp - bExp;
    if ( expDiff < -1 ) then
      begin
        result := a;
        exit;
      end;
    shortShift128Left(
        aSig0 or int64( $0001000000000000 ),
        aSig1,
        15 - ord( expDiff < 0 ),
        aSig0,
        aSig1
    );
    shortShift128Left(
        bSig0 or int64( $0001000000000000 ), bSig1, 15, bSig0, bSig1 );
    q := le128( bSig0, bSig1, aSig0, aSig1 );
    if ( q )<>0 then sub128( aSig0, aSig1, bSig0, bSig1, aSig0, aSig1 );
    dec(expDiff,64);
    while ( 0 < expDiff ) do begin
        q := estimateDiv128To64( aSig0, aSig1, bSig0 );
        if ( 4 < q ) then
          q := q - 4
        else
          q := 0;
        mul128By64To192( bSig0, bSig1, q, term0, term1, term2 );
        shortShift192Left( term0, term1, term2, 61, term1, term2, allZero );
        shortShift128Left( aSig0, aSig1, 61, aSig0, allZero );
        sub128( aSig0, 0, term1, term2, aSig0, aSig1 );
        dec(expDiff,61);
    end;
    if ( -64 < expDiff ) then begin
        q := estimateDiv128To64( aSig0, aSig1, bSig0 );
        if ( 4 < q ) then
          q := q - 4
        else
          q := 0;
        q := q shr (- expDiff);
        shift128Right( bSig0, bSig1, 12, bSig0, bSig1 );
        inc(expDiff,52);
        if ( expDiff < 0 ) then begin
            shift128Right( aSig0, aSig1, - expDiff, aSig0, aSig1 );
        end
        else begin
            shortShift128Left( aSig0, aSig1, expDiff, aSig0, aSig1 );
        end;
        mul128By64To192( bSig0, bSig1, q, term0, term1, term2 );
        sub128( aSig0, aSig1, term1, term2, aSig0, aSig1 );
    end
    else begin
        shift128Right( aSig0, aSig1, 12, aSig0, aSig1 );
        shift128Right( bSig0, bSig1, 12, bSig0, bSig1 );
    end;
    repeat
        alternateASig0 := aSig0;
        alternateASig1 := aSig1;
        inc(q);
        sub128( aSig0, aSig1, bSig0, bSig1, aSig0, aSig1 );
    until not( 0 <= sbits64(aSig0) );
    add128(
        aSig0, aSig1, alternateASig0, alternateASig1, bits64(sigMean0), sigMean1 );
    if (    ( sigMean0 < 0 )
         or ( ( ( sigMean0 or sigMean1 ) = 0 ) and (( q and 1 )<>0) ) ) then begin
        aSig0 := alternateASig0;
        aSig1 := alternateASig1;
    end;
    zSign := ord( sbits64(aSig0) < 0 );
    if ( zSign<>0 ) then sub128( 0, 0, aSig0, aSig1, aSig0, aSig1 );
    result :=
        normalizeRoundAndPackFloat128( aSign xor zSign, bExp - 4, aSig0, aSig1 );

end;

{*----------------------------------------------------------------------------
| Returns the square root of the quadruple-precision floating-point value `a'.
| The operation is performed according to the IEC/IEEE Standard for Binary
| Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function float128_sqrt(a: float128): float128;
var
    aSign: flag;
    aExp, zExp: int32;
    aSig0, aSig1, zSig0, zSig1, zSig2, doubleZSig0: bits64;
    rem0, rem1, rem2, rem3, term0, term1, term2, term3: bits64;
    z: float128;
label
    invalid;
begin
    aSig1 := extractFloat128Frac1( a );
    aSig0 := extractFloat128Frac0( a );
    aExp := extractFloat128Exp( a );
    aSign := extractFloat128Sign( a );
    if ( aExp = $7FFF ) then begin
        if ( aSig0 or aSig1 )<>0 then
          begin
            result := propagateFloat128NaN( a, a );
            exit;
          end;
        if ( aSign=0 ) then
          begin
            result := a;
            exit;
          end;
        goto invalid;
    end;
    if ( aSign<>0 ) then begin
        if ( ( aExp or aSig0 or aSig1 ) = 0 ) then
          begin
            result := a;
            exit;
          end;
 invalid:
        float_raise( float_flag_invalid );
        z.low := float128_default_nan_low;
        z.high := float128_default_nan_high;
        result := z;
        exit;
    end;
    if ( aExp = 0 ) then begin
        if ( ( aSig0 or aSig1 ) = 0 ) then
        begin
          result := packFloat128( 0, 0, 0, 0 );
          exit;
        end;
        normalizeFloat128Subnormal( aSig0, aSig1, aExp, aSig0, aSig1 );
    end;
    zExp := ( ( aExp - $3FFF )>>1 ) + $3FFE;
    aSig0 := aSig0 or int64( $0001000000000000 );
    zSig0 := estimateSqrt32( aExp, aSig0>>17 );
    shortShift128Left( aSig0, aSig1, 13 - ( aExp and 1 ), aSig0, aSig1 );
    zSig0 := estimateDiv128To64( aSig0, aSig1, zSig0 shl 32 ) + ( zSig0 shl 30 );
    doubleZSig0 := zSig0 shl 1;
    mul64To128( zSig0, zSig0, term0, term1 );
    sub128( aSig0, aSig1, term0, term1, rem0, rem1 );
    while ( sbits64(rem0) < 0 ) do begin
        dec(zSig0);
        dec(doubleZSig0,2);
        add128( rem0, rem1, zSig0 shr 63, doubleZSig0 or 1, rem0, rem1 );
    end;
    zSig1 := estimateDiv128To64( rem1, 0, doubleZSig0 );
    if ( ( zSig1 and $1FFF ) <= 5 ) then begin
        if ( zSig1 = 0 ) then zSig1 := 1;
        mul64To128( doubleZSig0, zSig1, term1, term2 );
        sub128( rem1, 0, term1, term2, rem1, rem2 );
        mul64To128( zSig1, zSig1, term2, term3 );
        sub192( rem1, rem2, 0, 0, term2, term3, rem1, rem2, rem3 );
        while ( sbits64(rem1) < 0 ) do begin
            dec(zSig1);
            shortShift128Left( 0, zSig1, 1, term2, term3 );
            term3 := term3 or 1;
            term2 := term2 or doubleZSig0;
            add192( rem1, rem2, rem3, 0, term2, term3, rem1, rem2, rem3 );
        end;
        zSig1 := zSig1 or ord( ( rem1 or rem2 or rem3 ) <> 0 );
    end;
    shift128ExtraRightJamming( zSig0, zSig1, 0, 14, zSig0, zSig1, zSig2 );
    result := roundAndPackFloat128( 0, zExp, zSig0, zSig1, zSig2 );

end;

{*----------------------------------------------------------------------------
| Returns 1 if the quadruple-precision floating-point value `a' is equal to
| the corresponding value `b', and 0 otherwise.  The comparison is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function float128_eq(a: float128; b: float128): flag;
begin
    if (    (    ( extractFloat128Exp( a ) = $7FFF )
              and (( extractFloat128Frac0( a ) or extractFloat128Frac1( a ))<>0 ) )
         or (    ( extractFloat128Exp( b ) = $7FFF )
              and ( (extractFloat128Frac0( b ) or extractFloat128Frac1( b ))<>0 ) )
       ) then begin
        if (    (float128_is_signaling_nan( a )<>0)
             or (float128_is_signaling_nan( b )<>0) ) then begin
            float_raise( float_flag_invalid );
        end;
        result := 0;
        exit;
    end;
    result := ord(
           ( a.low = b.low )
        and (    ( a.high = b.high )
             or (    ( a.low = 0 )
                  and ( bits64( ( a.high or b.high ) shl 1 ) = 0 ) )
           ));

end;

{*----------------------------------------------------------------------------
| Returns 1 if the quadruple-precision floating-point value `a' is less than
| or equal to the corresponding value `b', and 0 otherwise.  The comparison
| is performed according to the IEC/IEEE Standard for Binary Floating-Point
| Arithmetic.
*----------------------------------------------------------------------------*}

function float128_le(a: float128; b: float128): flag;
var
    aSign, bSign: flag;
begin
    if (    (    ( extractFloat128Exp( a ) = $7FFF )
              and (( extractFloat128Frac0( a ) or extractFloat128Frac1( a ))<>0 ) )
         or (    ( extractFloat128Exp( b ) = $7FFF )
              and ( (extractFloat128Frac0( b ) or extractFloat128Frac1( b ))<>0 ) )
       ) then begin
        float_raise( float_flag_invalid );
        result := 0;
        exit;
    end;
    aSign := extractFloat128Sign( a );
    bSign := extractFloat128Sign( b );
    if ( aSign <> bSign ) then begin
        result := ord(
               (aSign<>0)
            or (    ( ( bits64 ( ( a.high or b.high ) shl 1 ) ) or a.low or b.low )
                 = 0 ));
        exit;
    end;
    if aSign<>0 then
      result := le128( b.high, b.low, a.high, a.low )
    else
      result := le128( a.high, a.low, b.high, b.low );

end;

{*----------------------------------------------------------------------------
| Returns 1 if the quadruple-precision floating-point value `a' is less than
| the corresponding value `b', and 0 otherwise.  The comparison is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function float128_lt(a: float128; b: float128): flag;
var
    aSign, bSign: flag;
begin
    if (    (    ( extractFloat128Exp( a ) = $7FFF )
              and (( extractFloat128Frac0( a ) or extractFloat128Frac1( a ))<>0 ) )
         or (    ( extractFloat128Exp( b ) = $7FFF )
              and ( (extractFloat128Frac0( b ) or extractFloat128Frac1( b ))<>0 ) )
       ) then begin
        float_raise( float_flag_invalid );
        result := 0;
        exit;
    end;
    aSign := extractFloat128Sign( a );
    bSign := extractFloat128Sign( b );
    if ( aSign <> bSign ) then begin
        result := ord(
               (aSign<>0)
            and (    ( ( bits64( ( a.high or b.high ) shl 1 ) ) or a.low or b.low )
                 <> 0 ));
        exit;
    end;
    if aSign<>0 then
      result := lt128( b.high, b.low, a.high, a.low )
    else
      result := lt128( a.high, a.low, b.high, b.low );

end;

{*----------------------------------------------------------------------------
| Returns 1 if the quadruple-precision floating-point value `a' is equal to
| the corresponding value `b', and 0 otherwise.  The invalid exception is
| raised if either operand is a NaN.  Otherwise, the comparison is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function float128_eq_signaling(a: float128; b: float128): flag;
begin
    if (    (    ( extractFloat128Exp( a ) = $7FFF )
              and ( ( extractFloat128Frac0( a ) or extractFloat128Frac1( a ))<>0 ) )
         or (    ( extractFloat128Exp( b ) = $7FFF )
              and ( (extractFloat128Frac0( b ) or extractFloat128Frac1( b ))<>0 ) )
       ) then begin
        float_raise( float_flag_invalid );
        result := 0;
        exit;
    end;
    result := ord(
           ( a.low = b.low )
        and (    ( a.high = b.high )
             or (    ( a.low = 0 )
                  and ( bits64 ( ( a.high or b.high ) shl 1 ) = 0 ) )
           ));

end;

{*----------------------------------------------------------------------------
| Returns 1 if the quadruple-precision floating-point value `a' is less than
| or equal to the corresponding value `b', and 0 otherwise.  Quiet NaNs do not
| cause an exception.  Otherwise, the comparison is performed according to the
| IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function float128_le_quiet(a: float128; b: float128): flag;
var
    aSign, bSign: flag;
begin
    if (    (    ( extractFloat128Exp( a ) = $7FFF )
              and ( ( extractFloat128Frac0( a ) or extractFloat128Frac1( a ))<>0 ) )
         or (    ( extractFloat128Exp( b ) = $7FFF )
              and ( (extractFloat128Frac0( b ) or extractFloat128Frac1( b ))<>0 ) )
       ) then begin
        if (    (float128_is_signaling_nan( a )<>0)
             or (float128_is_signaling_nan( b )<>0) ) then begin
            float_raise( float_flag_invalid );
        end;
        result := 0;
        exit;
    end;
    aSign := extractFloat128Sign( a );
    bSign := extractFloat128Sign( b );
    if ( aSign <> bSign ) then begin
        result := ord(
               (aSign<>0)
            or (    ( ( bits64( ( a.high or b.high ) shl 1 ) ) or a.low or b.low )
                 = 0 ));
        exit;
    end;
    if aSign<>0 then
      result := le128( b.high, b.low, a.high, a.low )
    else
      result := le128( a.high, a.low, b.high, b.low );

end;

{*----------------------------------------------------------------------------
| Returns 1 if the quadruple-precision floating-point value `a' is less than
| the corresponding value `b', and 0 otherwise.  Quiet NaNs do not cause an
| exception.  Otherwise, the comparison is performed according to the IEC/IEEE
| Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}

function float128_lt_quiet(a: float128; b: float128): flag;
var
    aSign, bSign: flag;
begin
    if (    (    ( extractFloat128Exp( a ) = $7FFF )
              and (( extractFloat128Frac0( a ) or extractFloat128Frac1( a ))<>0 ) )
         or (    ( extractFloat128Exp( b ) = $7FFF )
              and ( (extractFloat128Frac0( b ) or extractFloat128Frac1( b ))<>0 ) )
       ) then begin
        if ( (float128_is_signaling_nan( a )<>0)
             or (float128_is_signaling_nan( b )<>0) ) then begin
            float_raise( float_flag_invalid );
        end;
        result := 0;
        exit;
    end;
    aSign := extractFloat128Sign( a );
    bSign := extractFloat128Sign( b );
    if ( aSign <> bSign ) then begin
        result := ord(
               (aSign<>0)
            and (    ( ( bits64( ( a.high or b.high ) shl 1 ) ) or a.low or b.low )
                 <> 0 ));
        exit;
    end;
    if aSign<>0 then
      result:=lt128( b.high, b.low, a.high, a.low )
    else
      result:=lt128( a.high, a.low, b.high, b.low );

end;

{----------------------------------------------------------------------------
| Returns the result of converting the double-precision floating-point value
| `a' to the quadruple-precision floating-point format.  The conversion is
| performed according to the IEC/IEEE Standard for Binary Floating-Point
| Arithmetic.
*----------------------------------------------------------------------------}

function float64_to_float128( a : float64) : float128;
var
    aSign : flag;
    aExp : int16;
    aSig, zSig0, zSig1 : bits64;
begin
    aSig := extractFloat64Frac( a );
    aExp := extractFloat64Exp( a );
    aSign := extractFloat64Sign( a );
    if ( aExp = $7FF ) then begin
        if ( aSig<>0 ) then
          result:=commonNaNToFloat128( float64ToCommonNaN( a ) );
        result:=packFloat128( aSign, $7FFF, 0, 0 );
        exit;
    end;
    if ( aExp = 0 ) then begin
        if ( aSig = 0 ) then
          begin
            result:=packFloat128( aSign, 0, 0, 0 );
            exit;
          end;

        normalizeFloat64Subnormal( aSig, aExp, aSig );
        dec(aExp);
    end;
    shift128Right( aSig, 0, 4, zSig0, zSig1 );
    result:=packFloat128( aSign, aExp + $3C00, zSig0, zSig1 );

end;

{$endif FPC_SOFTFLOAT_FLOAT128}

{$endif not(defined(fpc_softfpu_interface))}

{$if not(defined(fpc_softfpu_interface)) and not(defined(fpc_softfpu_implementation))}

end.

{$endif not(defined(fpc_softfpu_interface)) and not(defined(fpc_softfpu_implementation))}

