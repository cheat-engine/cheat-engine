/*
textdefs.h

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#ifndef TEXTDEFS_H
#define TEXTDEFS_H

#include "../config.h"

#include "wstring.h"

#define PLUS_DISP_CHR '+'
#define MINUS_DISP_CHR '-'
/* CHR0 is used for FPU, ST(i) register */
#define OPEN_CHR0 '('
#define CLOSE_CHR0 ')'
#define OPEN_CHR '['
#define CLOSE_CHR ']'
extern int8_t SEP_STR[3];
#define SEP_CHR ','
#define SP_CHR ' '
#define SEG_OFF_CHR ':'

/*
Naming Convention:

* get - returns a pointer to a string.
* str - concatenates to string.

* hex - means the function is used for hex dump (number is padded to required size) - Little Endian output.
* code - means the function is used for disassembled instruction - Big Endian output.
* off - means the function is used for 64bit offset - Big Endian output.

* sp - space character in front of the string.
* h - '0x' in front of the string.

* b - byte
* w - word
* dw - double word
* qw - quad word

* all numbers are in HEX.
*/

extern int8_t TextBTable[256][4];

_INLINE_ int8_t* get_hex_b(unsigned int x)
{
	return &TextBTable[x & 255][1]; /* Skip space character. */
}

void _FASTCALL_ str_hex_b(_WString* s, unsigned int x);
void _FASTCALL_ str_code_hb(_WString* s, unsigned int x);

void _FASTCALL_ str_hex_sp_b(_WString* s, unsigned int x);
void _FASTCALL_ str_code_sp_hb(_WString* s, unsigned int x);

void _FASTCALL_ str_hex_sp_w(_WString* s, unsigned int x);
void _FASTCALL_ str_code_hw(_WString* s, unsigned int x);

void _FASTCALL_ str_hex_sp_dw(_WString* s, uint32_t x);
void _FASTCALL_ str_code_hdw(_WString* s, uint32_t x);

void _FASTCALL_ str_hex_sp_qw(_WString* s, uint8_t src[8]);
void _FASTCALL_ str_code_hqw(_WString* s, uint8_t src[8]);

#ifdef SUPPORT_64BIT_OFFSET
void _FASTCALL_ str_off64(_WString* s, OFFSET_INTEGER x);
#endif

#endif /* TEXTDEFS_H */
