/*
prefix.h

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#ifndef PREFIX_H
#define PREFIX_H

#include "../config.h"

#include "decoder.h"
#include "wstring.h"

/*
* This holds the prefixes state for the current instruction we decode.
* totalPrefixes includes all specific prefixes that the instruction got.
* start is a pointer to the first prefix to take into account.
* last is a pointer to the last byte we scanned.
* Other pointers are used to keep track of prefixes positions and help us know if they appeared already and where.
*/
typedef struct {
	unsigned int unusedCount, specialPrefixesSize, isREXPrefixValid;
	_iflags totalPrefixes, usedPrefixes;
	const uint8_t *lokrepPos, *segovrdPos, *opsizePos, *addrsizePos, *rexpos, *start, *last;
} _PrefixState;

/* Describe the type of the prefix and which one it was in a flag manner. */
typedef enum {PRE_NONE = -1, PRE_LOKREP, PRE_SEGOVRD, PRE_OPSIZE, PRE_ADDRSIZE, PRE_REX} _PrefixType;

typedef struct {
	_iflags flag;
	_PrefixType type;
} _PrefixInfo;

/*
* Intel supports 4 types of prefixes, whereas AMD supports 5 types (lock is seperated from rep/nz).
* REX is the fifth prefix type, this time I'm based on AMD64.
*/
#define MAX_PREFIXES (5)

int is_prefix(unsigned int ch, _DecodeType dt);
void decode_prefixes(const uint8_t* code, int codeLen, _PrefixState* ps, _DecodeType dt);
void get_unused_prefixes_list(uint8_t unusedList[MAX_PREFIXES], _PrefixState* ps);

void str_seg_text(_WString* s, _PrefixState* ps, _DecodeType dt);

#endif /* PREFIX_H */
