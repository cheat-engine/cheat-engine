/*
config.h

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#ifndef CONFIG_H
#define CONFIG_H

/* diStorm version number. */
#define DISTORM_VER 0x01071e
#define SUPPORT_64BIT_OFFSET 1

#define DARKBYTESFUCKEDUPCROSSCOMPILER 1

#ifdef DARKBYTESFUCKEDUPCROSSCOMPILER
typedef signed char int8_t;
typedef short int int16_t;
typedef int int32_t;
typedef long long int int64_t;
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long int uint64_t;
#endif


#include "common.h" /* strlen, memset, memcpy - can be easily self implemented for libc independency. */

/*
 * 64 bit offsets support:
 * This macro should be defined from compiler command line flags, e.g: -DSUPPORT_64BIT_OFFSET
 * #define SUPPORT_64BIT_OFFSET
 * Note: make sure that the caller (library user) defines it too!
 */

/*
 * If you compile diStorm as a .DLL file, make sure you uncomment the next line.
 * So the interface functions will be exported, otherwise they are useable only as a library.
 * For example, the Python extension module defines this macro in its configuration.
 */
/* #define _DLL */

/*
 * diStorm now supports little/big endian CPU's.
 * It should detect the endianness according to predefined macro's of the compiler.
 * If you don't use GCC/MSVC you will have to define it on your own.
 */

/* These macros are used in order to make the code portable. */
#ifdef __GNUC__

#ifndef DARKBYTESFUCKEDUPCROSSCOMPILER
#include <stdint.h>
#endif

#define _PACKED_ __attribute__((__packed__))
#define _DLLEXPORT_
#define _FASTCALL_
#define _INLINE_ static __inline__
/* GCC ignores this directive... */
/*#define _FASTCALL_ __attribute__((__fastcall__)) */

/* Set endianity (supposed to be LE though): */
#ifdef __BIG_ENDIAN__
	#define BE_SYSTEM
#endif

/* End of __GCC__ */

#elif __WATCOMC__

#include <stdint.h>

#define _PACKED_
#define _DLLEXPORT_
#define _FASTCALL_
#define _INLINE_ __inline

/* End of __WATCOMC__ */

#elif __DMC__

#include <stdint.h>

#define _PACKED_
#define _DLLEXPORT_
#define _FASTCALL_
#define _INLINE_ static __inline

/* End of __DMC__ */

#elif __TINYC__

#include <stdint.h>

#define _PACKED_
#define _DLLEXPORT_
#define _FASTCALL_
#define _INLINE_ static

/* End of __TINYC__ */

#elif _MSC_VER

/* Since MSVC isn't shipped with stdint.h, we will have our own: */
typedef signed __int64		int64_t;
typedef unsigned __int64	uint64_t;
typedef signed __int32		int32_t;
typedef unsigned __int32	uint32_t;
typedef signed __int16		int16_t;
typedef unsigned __int16	uint16_t;
typedef signed __int8		int8_t;
typedef unsigned __int8		uint8_t;

#define _PACKED_
#define _DLLEXPORT_ __declspec(dllexport)
#define _FASTCALL_ __fastcall
#define _INLINE_ static __inline

/* Set endianity (supposed to be LE though): */
#ifndef _M_IX86
	#define BE_SYSTEM
#endif

#endif /* #elif _MSC_VER */

/* 32 or 64 bits integer for instruction offset. */
#ifdef SUPPORT_64BIT_OFFSET
	#define OFFSET_INTEGER uint64_t
#else
	#define OFFSET_INTEGER uint32_t
#endif

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

/* If the library isn't compiled as a .DLL don't export functions. */
#ifndef _DLL
#undef _DLLEXPORT_
#define _DLLEXPORT_
#endif

/* Define stream read functions for big endian systems. */
#ifdef BE_SYSTEM
/*
 * These functions can read from the stream safely!
 * Swap endianity of input to little endian.
 */
_INLINE_ int16_t RSHORT(const uint8_t *s)
{
	return s[0] | (s[1] << 8);
}
_INLINE_ uint16_t RUSHORT(const uint8_t *s)
{
	return s[0] | (s[1] << 8);
}
_INLINE_ int32_t RLONG(const uint8_t *s)
{
	return s[0] | (s[1] << 8) | (s[2] << 16) | (s[3] << 24);
}
_INLINE_ uint32_t RULONG(const uint8_t *s)
{
	return s[0] | (s[1] << 8) | (s[2] << 16) | (s[3] << 24);
}
#else
/* Little endian macro's will just make the cast. */
#define RSHORT(x) *(int16_t *)x
#define RUSHORT(x) *(uint16_t *)x
#define RLONG(x) *(int32_t *)x
#define RULONG(x) *(uint32_t *)x
#endif

#endif /* CONFIG_H */
