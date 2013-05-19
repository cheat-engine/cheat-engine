/*
** $Id: luaconf.h,v 1.82.1.7 2008/02/11 16:25:08 roberto Exp $
** Configuration file for Lua (external)
** See Copyright Notice in lua.h
*/


#ifndef lconfig_h
#define lconfig_h

#include <limits.h>
#include <stddef.h>

#define LUA_BUILD_AS_DLL
#define LNUM_INT64

/*
** ==================================================================
** Search for "@@" to find all configurable definitions.
** ===================================================================
*/


/*
@@ LUA_PATH and LUA_CPATH are the names of the environment variables that
@* Lua check to set its paths.
@@ LUA_INIT is the name of the environment variable that Lua
@* checks for initialization code.
** CHANGE them if you want different names.
*/
#define LUA_PATH        "LUA_PATH"
#define LUA_CPATH       "LUA_CPATH"
#define LUA_INIT	"LUA_INIT"

//#define LUA_USER_H	"lua_user.h"


/*
@@ LUA_PATH_DEFAULT is the default path that Lua uses to look for
@* Lua libraries.
@@ LUA_CPATH_DEFAULT is the default path that Lua uses to look for
@* C libraries.
** CHANGE them if your machine has a non-conventional directory
** hierarchy or if you want to install your libraries in
** non-conventional directories.
*/
#if defined(_WIN32)
/*
** In Windows, any exclamation mark ('!') in the path is replaced by the
** path of the directory of the executable file of the current process.
*/
#define LUA_LDIR	"!\\lua\\"
#define LUA_CDIR	"!\\"
#define LUA_PATH_DEFAULT  \
		".\\?.lua;"  LUA_LDIR"?.lua;"  LUA_LDIR"?\\init.lua;" \
		             LUA_CDIR"?.lua;"  LUA_CDIR"?\\init.lua"
#define LUA_CPATH_DEFAULT \
		".\\?.dll;"  LUA_CDIR"?.dll;" LUA_CDIR"loadall.dll;" \
		LUA_CDIR"clibs\\?.dll;" LUA_CDIR"clibs\\loadall.dll;" \
		".\\?51.dll;"  LUA_CDIR"?51.dll;" LUA_CDIR"clibs\\?51.dll" 

#else
#define LUA_ROOT	"/usr/local/"
#define LUA_LDIR	LUA_ROOT "share/lua/5.1/"
#define LUA_CDIR	LUA_ROOT "lib/lua/5.1/"
#define LUA_PATH_DEFAULT  \
		"./?.lua;"  LUA_LDIR"?.lua;"  LUA_LDIR"?/init.lua;" \
		            LUA_CDIR"?.lua;"  LUA_CDIR"?/init.lua"
#define LUA_CPATH_DEFAULT \
		"./?.so;"  LUA_CDIR"?.so;" LUA_CDIR"loadall.so" \
		"./lib?51.so;" LUA_CDIR"lib?51.so;"
#endif


/*
@@ LNUM_DOUBLE | LNUM_FLOAT | LNUM_LDOUBLE: Generic Lua number mode
@@ LNUM_INT32 | LNUM_INT64: Integer type (optional)
@@ LNUM_COMPLEX: Define for using 'a+bi' numbers
@*
@* You can combine LNUM_xxx but only one of each group. I.e. 'LNUM_FLOAT
@* LNUM_INT32 LNUM_COMPLEX' gives float range complex numbers, with 
@* 32-bit scalar integer range optimized.
*/
/*#define LNUM_DOUBLE*/


/*
** LUA_NUMBER is the type of floating point number in Lua
** LUA_NUMBER_SCAN is the format for reading numbers.
** LUA_NUMBER_FMT is the format for writing numbers.
*/
#ifdef LNUM_FLOAT
# define LUA_NUMBER         float
# define LUA_NUMBER_SCAN    "%f"
# define LUA_NUMBER_FMT     "%g"  
#elif defined(LNUM_LDOUBLE)
# define LUA_NUMBER         long double
# define LUA_NUMBER_SCAN    "%Lg"
# define LUA_NUMBER_FMT     "%.20Lg"
#else
# define LUA_NUMBER	        double
# define LUA_NUMBER_SCAN    "%lf"
# define LUA_NUMBER_FMT     "%.14g"
#endif


/*
@@ LUAI_BITSINT defines the number of bits in an int.
** CHANGE here if Lua cannot automatically detect the number of bits of
** your machine. Probably you do not need to change this.
*/
/* avoid overflows in comparison */
#if INT_MAX-20 < 32760
#define LUAI_BITSINT	16
#elif INT_MAX > 2147483640L
/* int has at least 32 bits */
#define LUAI_BITSINT	32
#else
#error "you must define LUA_BITSINT with number of bits in an integer"
#endif


/*
** LUA_INTEGER is the integer type used by lua_pushinteger/lua_tointeger/lua_isinteger.
** LUA_INTEGER_SCAN is the format for reading integers
** LUA_INTEGER_FMT is the format for writing integers
*/
#ifdef LNUM_INT64
# define LUA_INTEGER	long long
# ifdef _MSC_VER
#  define lua_str2ul    _strtoui64
# else
#  define lua_str2ul    strtoull
# endif
# define LUA_INTEGER_SCAN "%lld"
# define LUA_INTEGER_FMT "%lld"
# define LUA_INTEGER_MAX 0x7fffffffffffffffLL       /* 2^63-1 */ 
# define LUA_INTEGER_MIN (-LUA_INTEGER_MAX - 1LL)   /* -2^63 */
/* */
#else
# if LUAI_BITSINT == 32
#  define LUA_INTEGER   int
#  define LUA_INTEGER_SCAN "%d"
#  define LUA_INTEGER_FMT "%d"
# else
/* Note: 'LUA_INTEGER' being 'ptrdiff_t' (as in Lua 5.1) causes problems with
 *       'printf()' operations. Also 'unsigned ptrdiff_t' is invalid. */
#  define LUA_INTEGER   long
#  define LUA_INTEGER_SCAN "%ld"
#  define LUA_INTEGER_FMT "%ld"
# endif
# define LUA_INTEGER_MAX 0x7FFFFFFF             /* 2^31-1 */
#endif

#ifndef lua_str2ul
# define lua_str2ul (unsigned LUA_INTEGER)strtoul
#endif
#ifndef LUA_INTEGER_MIN
# define LUA_INTEGER_MIN (-LUA_INTEGER_MAX -1)  /* -2^16|32 */
#endif


/*
@@ LUA_API is a mark for all core API functions.
@@ LUALIB_API is a mark for all standard library functions.
** CHANGE them if you need to define those functions in some special way.
** For instance, if you want to create one Windows DLL with the core and
** the libraries, you may want to use the following definition (define
** LUA_BUILD_AS_DLL to get it).
*/
#if defined(LUA_BUILD_AS_DLL)

#if defined(LUA_CORE) || defined(LUA_LIB)
#define LUA_API __declspec(dllexport)
#else
#define LUA_API __declspec(dllimport)
#endif

#else

#define LUA_API		extern

#endif

/* more often than not the libs go together with the core */
#define LUALIB_API	LUA_API


/*
@@ LUA_IDSIZE gives the maximum size for the description of the source
@* of a function in debug information.
** CHANGE it if you want a different size.
*/
#define LUA_IDSIZE	60


/*
@@ LUAL_BUFFERSIZE is the buffer size used by the lauxlib buffer system.
*/
#define LUAL_BUFFERSIZE		BUFSIZ


/* }================================================================== */


/*
@@ LUA_COMPAT_GETN controls compatibility with old getn behavior.
** CHANGE it (define it) if you want exact compatibility with the
** behavior of setn/getn in Lua 5.0.
*/
#undef LUA_COMPAT_GETN

/*
@@ LUA_COMPAT_LOADLIB controls compatibility about global loadlib.
** CHANGE it to undefined as soon as you do not need a global 'loadlib'
** function (the function is still available as 'package.loadlib').
*/
#undef LUA_COMPAT_LOADLIB

/*
@@ LUA_COMPAT_VARARG controls compatibility with old vararg feature.
** CHANGE it to undefined as soon as your programs use only '...' to
** access vararg parameters (instead of the old 'arg' table).
*/
#define LUA_COMPAT_VARARG

/*
@@ LUA_COMPAT_MOD controls compatibility with old math.mod function.
** CHANGE it to undefined as soon as your programs use 'math.fmod' or
** the new '%' operator instead of 'math.mod'.
*/
#define LUA_COMPAT_MOD

/*
@@ LUA_COMPAT_LSTR controls compatibility with old long string nesting
@* facility.
** CHANGE it to 2 if you want the old behaviour, or undefine it to turn
** off the advisory error when nesting [[...]].
*/
#define LUA_COMPAT_LSTR		1

/*
@@ LUA_COMPAT_GFIND controls compatibility with old 'string.gfind' name.
** CHANGE it to undefined as soon as you rename 'string.gfind' to
** 'string.gmatch'.
*/
#define LUA_COMPAT_GFIND

/*
@@ LUA_COMPAT_OPENLIB controls compatibility with old 'luaL_openlib'
@* behavior.
** CHANGE it to undefined as soon as you replace to 'luaL_register'
** your uses of 'luaL_openlib'
*/
#define LUA_COMPAT_OPENLIB

/*
@@ LUA_COMPAT_TOINTEGER controls compatibility with 5.1 'lua_tointeger()';
@* allows values not fitting in 'lua_Integer' to be rounded.
*/
#define LUA_COMPAT_TOINTEGER

/* =================================================================== */

/*
** Local configuration. You can use this space to add your redefinitions
** without modifying the main part of the file.
*/



#endif

