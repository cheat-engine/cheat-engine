/*
** $Id: lmathlib.c,v 1.67.1.1 2007/12/27 13:02:25 roberto Exp $
** Standard mathematical library
** See Copyright Notice in lua.h
*/

#include <stdlib.h>
#include <math.h>

#define lmathlib_c
#define LUA_LIB

#include "lua.h"

#include "lauxlib.h"
#include "lualib.h"

/* 'luai_vectpow()' as a replacement for 'cpow()'. Defined in the header; we
 * don't intrude the code libs internal functions.
 */
#ifdef LNUM_COMPLEX
# include "lnum.h"    
#endif

#undef PI
#ifdef LNUM_FLOAT
# define PI (3.14159265358979323846F)
#elif defined(M_PI)
# define PI M_PI
#else
# define PI (3.14159265358979323846264338327950288)
#endif
#define RADIANS_PER_DEGREE (PI/180)

#undef HUGE
#ifdef LNUM_FLOAT
# define HUGE HUGE_VALF
#elif defined(LNUM_LDOUBLE)
# define HUGE HUGE_VALL
#else
# define HUGE HUGE_VAL
#endif

static int math_abs (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_pushnumber(L, _LF(cabs) (luaL_checkcomplex(L,1)));
#else
  lua_pushnumber(L, _LF(fabs) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_sin (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(csin) (luaL_checkcomplex(L,1)));
#else
  lua_pushnumber(L, _LF(sin) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_sinh (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(csinh) (luaL_checkcomplex(L,1)));
#else
  lua_pushnumber(L, _LF(sinh) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_cos (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(ccos) (luaL_checkcomplex(L,1)));
#else
  lua_pushnumber(L, _LF(cos) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_cosh (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(ccosh) (luaL_checkcomplex(L,1)));
#else
  lua_pushnumber(L, _LF(cosh) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_tan (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(ctan) (luaL_checkcomplex(L,1)));
#else
  lua_pushnumber(L, _LF(tan) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_tanh (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(ctanh) (luaL_checkcomplex(L,1)));
#else
  lua_pushnumber(L, _LF(tanh) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_asin (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(casin) (luaL_checkcomplex(L,1)));
#else
  lua_pushnumber(L, _LF(asin) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_acos (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(cacos) (luaL_checkcomplex(L,1)));
#else
  lua_pushnumber(L, _LF(acos) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_atan (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(catan) (luaL_checkcomplex(L,1)));
#else
  lua_pushnumber(L, _LF(atan) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_atan2 (lua_State *L) {
  /* scalars only */
  lua_pushnumber(L, _LF(atan2) (luaL_checknumber(L, 1), luaL_checknumber(L, 2)));
  return 1;
}

static int math_ceil (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_Complex v= luaL_checkcomplex(L, 1);
  lua_pushcomplex(L, _LF(ceil) (_LF(creal)(v)) + _LF(ceil) (_LF(cimag)(v))*I);
#else
  lua_pushnumber(L, _LF(ceil) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_floor (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_Complex v= luaL_checkcomplex(L, 1);
  lua_pushcomplex(L, _LF(floor) (_LF(creal)(v)) + _LF(floor) (_LF(cimag)(v))*I);
#else
  lua_pushnumber(L, _LF(floor) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_fmod (lua_State *L) {  
  /* scalars only */
  lua_pushnumber(L, _LF(fmod) (luaL_checknumber(L, 1), luaL_checknumber(L, 2)));
  return 1;
}

static int math_modf (lua_State *L) {
  /* scalars only */
  lua_Number ip;
  lua_Number fp = _LF(modf) (luaL_checknumber(L, 1), &ip);
  lua_pushnumber(L, ip);
  lua_pushnumber(L, fp);
  return 2;
}

static int math_sqrt (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(csqrt) (luaL_checkcomplex(L,1)));
#else
  lua_pushnumber(L, _LF(sqrt) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_pow (lua_State *L) {
#ifdef LNUM_COMPLEX
  /* C99 'cpow' gives somewhat inaccurate results (i.e. (-1)^2 = -1+1.2246467991474e-16i). 
  * 'luai_vectpow' smoothens such, reusing it is the reason we need to #include "lnum.h".
  */
  lua_pushcomplex(L, luai_vectpow(luaL_checkcomplex(L,1), luaL_checkcomplex(L,2)));
#else
  lua_pushnumber(L, _LF(pow) (luaL_checknumber(L, 1), luaL_checknumber(L, 2)));
#endif
  return 1;
}

static int math_log (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(clog) (luaL_checkcomplex(L,1)));
#else
  lua_pushnumber(L, _LF(log) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_log10 (lua_State *L) {
#ifdef LNUM_COMPLEX
  /* Not in standard <complex.h> , but easy to calculate: log_a(x) = log_b(x) / log_b(a) 
  */
  lua_pushcomplex(L, _LF(clog) (luaL_checkcomplex(L,1)) / _LF(log) (10));
#else
  lua_pushnumber(L, _LF(log10) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_exp (lua_State *L) {
#ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(cexp) (luaL_checkcomplex(L,1)));
#else
  lua_pushnumber(L, _LF(exp) (luaL_checknumber(L, 1)));
#endif
  return 1;
}

static int math_deg (lua_State *L) {
  lua_pushnumber(L, luaL_checknumber(L, 1)/RADIANS_PER_DEGREE);
  return 1;
}

static int math_rad (lua_State *L) {
  lua_pushnumber(L, luaL_checknumber(L, 1)*RADIANS_PER_DEGREE);
  return 1;
}

static int math_frexp (lua_State *L) {
  int e;
  lua_pushnumber(L, _LF(frexp) (luaL_checknumber(L, 1), &e));
  lua_pushinteger(L, e);
  return 2;
}

static int math_ldexp (lua_State *L) {
  lua_pushnumber(L, _LF(ldexp) (luaL_checknumber(L, 1), luaL_checkint(L, 2)));
  return 1;
}



static int math_min (lua_State *L) {
  /* scalars only */
  int n = lua_gettop(L);  /* number of arguments */
  lua_Number dmin = luaL_checknumber(L, 1);
  int i;
  for (i=2; i<=n; i++) {
    lua_Number d = luaL_checknumber(L, i);
    if (d < dmin)
      dmin = d;
  }
  lua_pushnumber(L, dmin);
  return 1;
}


static int math_max (lua_State *L) {
  /* scalars only */
  int n = lua_gettop(L);  /* number of arguments */
  lua_Number dmax = luaL_checknumber(L, 1);
  int i;
  for (i=2; i<=n; i++) {
    lua_Number d = luaL_checknumber(L, i);
    if (d > dmax)
      dmax = d;
  }
  lua_pushnumber(L, dmax);
  return 1;
}


static int math_random (lua_State *L) {
  /* the `%' avoids the (rare) case of r==1, and is needed also because on
     some systems (SunOS!) `rand()' may return a value larger than RAND_MAX */
  lua_Number r = (lua_Number)(rand()%RAND_MAX) / (lua_Number)RAND_MAX;
  int n= lua_gettop(L);  /* number of arguments */
  if (n==0) {	/* no arguments: range [0,1) */
    lua_pushnumber(L, r);
  } else if (n<=2) {	/* int range [1,u] or [l,u] */
    int l= n==1 ? 1 : luaL_checkint(L, 1);
    int u = luaL_checkint(L, n);
    int tmp;
    lua_Number d;
    luaL_argcheck(L, l<=u, n, "interval is empty");
    d= _LF(floor)(r*(u-l+1));
    lua_number2int(tmp,d);
    lua_pushinteger(L, l+tmp);
  } else {
    return luaL_error(L, "wrong number of arguments");
  }
  return 1;
}


static int math_randomseed (lua_State *L) {
  srand(luaL_checkint(L, 1));
  return 0;
}

/* 
* Lua 5.1 does not have acosh, asinh, atanh for scalars (not ANSI C)
*/
#if __STDC_VERSION__ >= 199901L
static int math_acosh (lua_State *L) {
# ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(cacosh) (luaL_checkcomplex(L,1)));
# else
  lua_pushnumber(L, _LF(acosh) (luaL_checknumber(L,1)));
# endif
  return 1;
}
static int math_asinh (lua_State *L) {
# ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(casinh) (luaL_checkcomplex(L,1)));
# else
  lua_pushnumber(L, _LF(asinh) (luaL_checknumber(L,1)));
# endif
  return 1;
}
static int math_atanh (lua_State *L) {
# ifdef LNUM_COMPLEX
  lua_pushcomplex(L, _LF(catanh) (luaL_checkcomplex(L,1)));
# else
  lua_pushnumber(L, _LF(atanh) (luaL_checknumber(L,1)));
# endif
  return 1;
}
#endif

/* 
 * C99 complex functions, not covered above.
*/
#ifdef LNUM_COMPLEX
static int math_arg (lua_State *L) {
  lua_pushnumber(L, _LF(carg) (luaL_checkcomplex(L,1)));
  return 1;
}

static int math_imag (lua_State *L) {
  lua_pushnumber(L, _LF(cimag) (luaL_checkcomplex(L,1)));
  return 1;
}

static int math_real (lua_State *L) {
  lua_pushnumber(L, _LF(creal) (luaL_checkcomplex(L,1)));
  return 1;
}

static int math_conj (lua_State *L) {
  lua_pushcomplex(L, _LF(conj) (luaL_checkcomplex(L,1)));
  return 1;
}

static int math_proj (lua_State *L) {
  lua_pushcomplex(L, _LF(cproj) (luaL_checkcomplex(L,1)));
  return 1;
}
#endif


static const luaL_Reg mathlib[] = {
  {"abs",   math_abs},
  {"acos",  math_acos},
  {"asin",  math_asin},
  {"atan2", math_atan2},
  {"atan",  math_atan},
  {"ceil",  math_ceil},
  {"cosh",   math_cosh},
  {"cos",   math_cos},
  {"deg",   math_deg},
  {"exp",   math_exp},
  {"floor", math_floor},
  {"fmod",   math_fmod},
  {"frexp", math_frexp},
  {"ldexp", math_ldexp},
  {"log10", math_log10},
  {"log",   math_log},
  {"max",   math_max},
  {"min",   math_min},
  {"modf",   math_modf},
  {"pow",   math_pow},
  {"rad",   math_rad},
  {"random",     math_random},
  {"randomseed", math_randomseed},
  {"sinh",   math_sinh},
  {"sin",   math_sin},
  {"sqrt",  math_sqrt},
  {"tanh",   math_tanh},
  {"tan",   math_tan},
#if __STDC_VERSION__ >= 199901L
  {"acosh",  math_acosh},
  {"asinh",  math_asinh},
  {"atanh",  math_atanh},
#endif
#ifdef LNUM_COMPLEX
  {"arg",   math_arg},
  {"imag",  math_imag},
  {"real",  math_real},
  {"conj",  math_conj},
  {"proj",  math_proj},
#endif
  {NULL, NULL}
};


/*
** Open math library
*/
LUALIB_API int luaopen_math (lua_State *L) {
  luaL_register(L, LUA_MATHLIBNAME, mathlib);
  lua_pushnumber(L, PI);
  lua_setfield(L, -2, "pi");
  lua_pushnumber(L, HUGE);
  lua_setfield(L, -2, "huge");
  lua_pushinteger(L, LUA_INTEGER_MAX );
  lua_setfield(L, -2, "hugeint");
#if defined(LUA_COMPAT_MOD)
  lua_getfield(L, -1, "fmod");
  lua_setfield(L, -2, "mod");
#endif
  return 1;
}

