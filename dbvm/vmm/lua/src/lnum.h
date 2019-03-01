/*
** $Id: lnum.h,v ... $
** Internal Number model
** See Copyright Notice in lua.h
*/

#ifndef lnum_h
#define lnum_h

//#include <math.h>

#include "lobject.h"

/*
** The luai_num* macros define the primitive operations over 'lua_Number's
** (not 'lua_Integer's, not 'lua_Complex').
*/
#define luai_numadd(a,b)	((a)+(b))
#define luai_numsub(a,b)	((a)-(b))
#define luai_nummul(a,b)	((a)*(b))
#define luai_numdiv(a,b)	((a)/(b))
#define luai_nummod(a,b)	((a) - _LF(floor)((a)/(b))*(b))
#define luai_numpow(a,b)	(_LF(pow)(a,b))
#define luai_numunm(a)		(-(a))
#define luai_numeq(a,b)	    ((a)==(b))
#define luai_numlt(a,b)	    ((a)<(b))
#define luai_numle(a,b)	    ((a)<=(b))

/*
* If '-ffast-math' is used, there are no NaNs or Infs. We shouldn't pretend
* there is.
*/
#ifdef __FAST_MATH__
# define luai_numisnan(a) (0)   /* has no concept of NANs */ 
#elif (defined __STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)

#else
# define luai_numisnan(a) (!luai_numeq((a), (a)))
#endif

#ifdef LUA_TINT
  int try_addint( lua_Integer *r, lua_Integer ib, lua_Integer ic );
  int try_subint( lua_Integer *r, lua_Integer ib, lua_Integer ic );
  int try_mulint( lua_Integer *r, lua_Integer ib, lua_Integer ic );
  int try_divint( lua_Integer *r, lua_Integer ib, lua_Integer ic );
  int try_modint( lua_Integer *r, lua_Integer ib, lua_Integer ic );
  int try_powint( lua_Integer *r, lua_Integer ib, lua_Integer ic );
  int try_unmint( lua_Integer *r, lua_Integer ib );
#endif

#ifdef LNUM_COMPLEX
  static inline lua_Complex luai_vectunm( lua_Complex a ) { return -a; }
  static inline lua_Complex luai_vectadd( lua_Complex a, lua_Complex b ) { return a+b; }
  static inline lua_Complex luai_vectsub( lua_Complex a, lua_Complex b ) { return a-b; }
  static inline lua_Complex luai_vectmul( lua_Complex a, lua_Complex b ) { return a*b; }
  static inline lua_Complex luai_vectdiv( lua_Complex a, lua_Complex b ) { return a/b; }

/* 
 * Complex power
 *
 * C99 'cpow' gives inaccurate results for many common cases s.a. (1i)^2 -> 
 * -1+1.2246467991474e-16i (OS X 10.4, gcc 4.0.1 build 5367)
 * 
 * [(a+bi)^(c+di)] = (r^c) * exp(-d*t) * cos(c*t + d*ln(r)) +
 *                 = (r^c) * exp(-d*t) * sin(c*t + d*ln(r)) *i
 * r = sqrt(a^2+b^2), t = arctan( b/a )
 * 
 * Reference: <http://home.att.net/~srschmitt/complexnumbers.html>
 * Could also be calculated using: x^y = exp(ln(x)*y)
 *
 * Note: Defined here (and not in .c) so 'lmathlib.c' can share the 
 *       implementation.
 */
  static inline
  lua_Complex luai_vectpow( lua_Complex a, lua_Complex b )
  {
    lua_Number ar= _LF(creal)(a), ai= _LF(cimag)(a);
    lua_Number br= _LF(creal)(b), bi= _LF(cimag)(b);
    
    if (ai==0 && bi==0) {     /* a^c (real) */
        return luai_numpow( ar, br );
    } 

    int br_int= (int)br;
    
    if ( ai!=0 && bi==0 && br_int==br && br_int!=0 && br_int!=INT_MIN ) { 
        /* (a+bi)^N, N = { +-1,+-2, ... +-INT_MAX } 
        */
        lua_Number k= luai_numpow( _LF(sqrt) (ar*ar + ai*ai), br );
        lua_Number cos_z, sin_z;

        /* Situation depends upon c (N) in the following manner:
         * 
         * N%4==0                                => cos(c*t)=1, sin(c*t)=0
         * (N*sign(b))%4==1 or (N*sign(b))%4==-3 => cos(c*t)=0, sin(c*t)=1
         * N%4==2 or N%4==-2                     => cos(c*t)=-1, sin(c*t)=0
         * (N*sign(b))%4==-1 or (N*sign(b))%4==3 => cos(c*t)=0, sin(c*t)=-1
         */
      int br_int_abs = br_int<0 ? -br_int:br_int;
      
      switch( (br_int_abs%4) * (br_int<0 ? -1:1) * (ai<0 ? -1:1) ) {
        case 0:             cos_z=1, sin_z=0; break;
        case 2: case -2:    cos_z=-1, sin_z=0; break;
        case 1: case -3:    cos_z=0, sin_z=1; break;
        case 3: case -1:    cos_z=0, sin_z=-1; break;
        default:            lua_assert(0); return 0;
      }
      return k*cos_z + (k*sin_z)*I;
    }
    return _LF(cpow) ( a, b );
  }
#endif

LUAI_FUNC int luaO_str2d (const char *s, lua_Number *res1, lua_Integer *res2);
LUAI_FUNC void luaO_num2buf( char *s, const TValue *o );

LUAI_FUNC int /*bool*/ tt_integer_valued( const TValue *o, lua_Integer *ref );

#endif
