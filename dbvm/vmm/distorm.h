/* diStorm64 1.7.30 */

/*
distorm.h

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.

This file is used in win32proj and linuxproj.
*/

#ifndef DISTORM_H
#define DISTORM_H

#include "config.h"

/*
 * 64 bit offsets support:
 * If the diStorm library you use was compiled with 64 bits offsets,
 * make sure you compile your own code with the following macro set:
 * SUPPORT_64BIT_OFFSET
 * Otherwise comment it out, or you will get a linker error of an unresolved symbol...
 */

/* TINYC has a problem with some 64bits library functions, so pass. */
#ifndef __TINYC__
    /* Make sure it wasn't already defined by the compiler itself. */
    #ifndef SUPPORT_64BIT_OFFSET
	    #define SUPPORT_64BIT_OFFSET
    #endif
#endif

/* If your compiler doesn't support stdint.h, define your own 64 bits type. */
#ifdef SUPPORT_64BIT_OFFSET
	#ifdef _MSC_VER
		#define OFFSET_INTEGER unsigned __int64
	#else
    #ifndef DARKBYTESFUCKEDUPCROSSCOMPILER
      #include <stdint.h>
    #endif
		#define OFFSET_INTEGER uint64_t
	#endif
#else
	/* 32 bit offsets are used. */
	#define OFFSET_INTEGER unsigned long
#endif


/* Support C++ compilers */
#ifdef __cplusplus
extern "C" {
#endif

/* Decodes modes of the disassembler, 16 bits or 32 bits or 64 bits for AMD64, x86-64. */
typedef enum {Decode16Bits = 0, Decode32Bits = 1, Decode64Bits = 2} _DecodeType;

typedef OFFSET_INTEGER _OffsetType;

/* Static size of strings. Do not change this value. */
#define MAX_TEXT_SIZE (60)
typedef struct {
	unsigned int length;
	unsigned char p[MAX_TEXT_SIZE]; /* p is a null terminated string. */
} _WString;

/* This structure holds all information the disassembler generates per instruction. */
typedef struct {
	_WString mnemonic; /* Mnemonic of decoded instruction, prefixed if required by REP, LOCK etc. */
	_WString operands; /* Operands of the decoded instruction, up to 3 operands, comma-seperated. */
	_WString instructionHex; /* Hex dump - little endian, including prefixes. */
	unsigned int size; /* Size of decoded instruction. */
	_OffsetType offset; /* Start offset of the decoded instruction. */
} _DecodedInst;

/* Return code of the decoding function. */
typedef enum {DECRES_NONE, DECRES_SUCCESS, DECRES_MEMORYERR, DECRES_INPUTERR} _DecodeResult;

/* distorm_decode
 * Input:
 *         offset - Origin of the given code (virtual address that is), NOT an offset in code.
 *         code - Pointer to the code buffer to be disassembled.
 *         length - Amount of bytes that should be decoded from the code buffer.
 *         dt - Decoding mode, 16 bits (Decode16Bits), 32 bits (Decode32Bits) or AMD64 (Decode64Bits).
 *         result - Array of type _DecodeInst which will be used by this function in order to return the disassembled instructions.
 *         maxInstructions - The maximum number of entries in the result array that you pass to this function, so it won't exceed its bound.
 *         usedInstructionsCount - Number of the instruction that successfully were disassembled and written to the result array.
 * Output: usedInstructionsCount will hold the number of entries used in the result array
 *         and the result array itself will be filled with the disassembled instructions.
 * Return: DECRES_SUCCESS on success (no more to disassemble), DECRES_INPUTERR on input error (null code buffer, invalid decoding mode, etc...),
 *         DECRES_MEMORYERR when there are not enough entries to use in the result array, BUT YOU STILL have to check for usedInstructionsCount!
 * Side-Effects: Even if the return code is DECRES_MEMORYERR, there might STILL be data in the
 *               array you passed, this function will try to use as much entries as possible!
 * Notes:  1)The minimal size of maxInstructions is 15.
 *         2)You will have to synchronize the offset,code and length by yourself if you pass code fragments and not a complete code block!
 */
#ifdef SUPPORT_64BIT_OFFSET
	_DecodeResult distorm_decode64(_OffsetType codeOffset, const unsigned char* code, int codeLen, _DecodeType dt, _DecodedInst result[], unsigned int maxInstructions, unsigned int* usedInstructionsCount);
	#define distorm_decode distorm_decode64
#else
	_DecodeResult distorm_decode32(_OffsetType codeOffset, const unsigned char* code, int codeLen, _DecodeType dt, _DecodedInst result[], unsigned int maxInstructions, unsigned int* usedInstructionsCount);
	#define distorm_decode distorm_decode32
#endif

/*
 * distorm_version
 * Input:
 *        none
 *
 * Output: unsigned int - version of compiler library.
 */
unsigned int distorm_version();

#ifdef __cplusplus
} /* End Of Extern */
#endif

#endif /* DISTORM_H */
