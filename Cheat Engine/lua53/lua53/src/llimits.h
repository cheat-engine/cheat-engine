/*
** $Id: llimits.h,v 1.125 2014/12/19 13:30:23 roberto Exp $
** Limits, basic types, and some other 'installation-dependent' definitions
** See Copyright Notice in lua.h
*/

#ifndef llimits_h
#define llimits_h


#include <limits.h>
#include <stddef.h>


#include "lua.h"

/*
** 'lu_mem' and 'l_mem' are unsigned/signed integers big enough to count
** the total memory used by Lua (in bytes). Usually, 'size_t' and
** 'ptrdiff_t' should work, but we use 'long' for 16-bit machines.
*/
#if defined(LUAI_MEM)		/* { external definitions? */
typedef LUAI_UMEM lu_mem;
typedef LUAI_MEM l_mem;
#elif LUAI_BITSINT >= 32	/* }{ */
typedef size_t lu_mem;
typedef ptrdiff_t l_mem;
#else  /* 16-bit ints */	/* }{ */
typedef unsigned long lu_mem;
typedef long l_mem;
#endif				/* } */


/* chars used as small naturals (so that 'char' is reserved for characters) */
typedef unsigned char lu_byte;


/* maximum value for size_t */
#define MAX_SIZET	((size_t)(~(size_t)0))

/* maximum size visible for Lua (must be representable in a lua_Integer */
#define MAX_SIZE	(sizeof(size_t) < sizeof(lua_Integer) ? MAX_SIZET \
                          : (size_t)(LUA_MAXINTEGER))


#define MAX_LUMEM	((lu_mem)(~(lu_mem)0))

#define MAX_LMEM	((l_mem)(MAX_LUMEM >> 1))


#define MAX_INT		INT_MAX  /* maximum value of an int */


/*
** conversion of pointer to integer:
** this is for hashing only; there is no problem if the integer
** cannot hold the whole pointer value
*/
#define point2int(p)	((unsigned int)((size_t)(p) & UINT_MAX))



/* type to ensure maximum alignment */
#if defined(LUAI_USER_ALIGNMENT_T)
typedef LUAI_USER_ALIGNMENT_T L_Umaxalign;
#else
typedef union { double u; void *s; lua_Integer i; long l; } L_Umaxalign;
#endif



/* types of 'usual argument conversions' for lua_Number and lua_Integer */
typedef LUAI_UACNUMBER l_uacNumber;
typedef LUAI_UACINT l_uacInt;


/* internal assertions for in-house debugging */
#if defined(lua_assert)
#define check_exp(c,e)		(lua_assert(c), (e))
/* to avoid problems with conditions too long */
#define lua_longassert(c)	{ if (!(c)) lua_assert(0); }
#else
#define lua_assert(c)		((void)0)
#define check_exp(c,e)		(e)
#define lua_longassert(c)	((void)0)
#endif

/*
** assertion for checking API calls
*/
#if defined(LUA_USE_APICHECK)
#include <assert.h>
#define luai_apicheck(e)	assert(e)
#else
#define luai_apicheck(e)	lua_assert(e)
#endif


#define api_check(e,msg)	luai_apicheck((e) && msg)


#if !defined(UNUSED)
#define UNUSED(x)	((void)(x))	/* to avoid warnings */
#endif


#define cast(t, exp)	((t)(exp))

#define cast_void(i)	cast(void, (i))
#define cast_byte(i)	cast(lu_byte, (i))
#define cast_num(i)	cast(lua_Number, (i))
#define cast_int(i)	cast(int, (i))
#define cast_uchar(i)	cast(unsigned char, (i))


/* cast a signed lua_Integer to lua_Unsigned */
#if !defined(l_castS2U)
#define l_castS2U(i)	((lua_Unsigned)(i))
#endif

/*
** cast a lua_Unsigned to a signed lua_Integer; this cast is
** not strict ISO C, but two-complement architectures should
** work fine.
*/
#if !defined(l_castU2S)
#define l_castU2S(i)	((lua_Integer)(i))
#endif


/*
** non-return type
*/
#if defined(__GNUC__)
#define l_noret		void __attribute__((noreturn))
#elif defined(_MSC_VER) && _MSC_VER >= 1200
#define l_noret		void __declspec(noreturn)
#else
#define l_noret		void
#endif



/*
** maximum depth for nested C calls and syntactical nested non-terminals
** in a program. (Value must fit in an unsigned short int.)
*/
#if !defined(LUAI_MAXCCALLS)
#define LUAI_MAXCCALLS		200
#endif

/*
** maximum number of upvalues in a closure (both C and Lua). (Value
** must fit in an unsigned char.)
*/
#define MAXUPVAL	UCHAR_MAX


/*
** type for virtual-machine instructions;
** must be an unsigned with (at least) 4 bytes (see details in lopcodes.h)
*/
#if LUAI_BITSINT >= 32
typedef unsigned int Instruction;
#else
typedef unsigned long Instruction;
#endif




/* minimum size for the string table (must be power of 2) */
#if !defined(MINSTRTABSIZE)
#define MINSTRTABSIZE	64	/* minimum size for "predefined" strings */
#endif


/* minimum size for string buffer */
#if !defined(LUA_MINBUFFER)
#define LUA_MINBUFFER	32
#endif


#if !defined(lua_lock)
#define lua_lock(L)	((void) 0)
#define lua_unlock(L)	((void) 0)
#endif

#if !defined(luai_threadyield)
#define luai_threadyield(L)	{lua_unlock(L); lua_lock(L);}
#endif


/*
** these macros allow user-specific actions on threads when you defined
** LUAI_EXTRASPACE and need to do something extra when a thread is
** created/deleted/resumed/yielded.
*/
#if !defined(luai_userstateopen)
#define luai_userstateopen(L)		((void)L)
#endif

#if !defined(luai_userstateclose)
#define luai_userstateclose(L)		((void)L)
#endif

#if !defined(luai_userstatethread)
#define luai_userstatethread(L,L1)	((void)L)
#endif

#if !defined(luai_userstatefree)
#define luai_userstatefree(L,L1)	((void)L)
#endif

#if !defined(luai_userstateresume)
#define luai_userstateresume(L,n)	((void)L)
#endif

#if !defined(luai_userstateyield)
#define luai_userstateyield(L,n)	((void)L)
#endif



/*
** macro to control inclusion of some hard tests on stack reallocation
*/
#if !defined(HARDSTACKTESTS)
#define condmovestack(L)	((void)0)
#else
/* realloc stack keeping its size */
#define condmovestack(L)	luaD_reallocstack((L), (L)->stacksize)
#endif

#if !defined(HARDMEMTESTS)
#define condchangemem(L)	condmovestack(L)
#else
#define condchangemem(L)  \
	((void)(!(G(L)->gcrunning) || (luaC_fullgc(L, 0), 1)))
#endif

#endif
