/*
decoder.h

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#ifndef DECODER_H
#define DECODER_H

#include "../config.h"

#include "wstring.h"

typedef unsigned int _iflags;

/* DEFAULT instructions decoding mode. */
typedef enum {Decode16Bits = 0, Decode32Bits = 1, Decode64Bits = 2} _DecodeType;

typedef OFFSET_INTEGER _OffsetType;

typedef struct {
	_WString mnemonic;
	_WString operands;
	_WString instructionHex;
	unsigned int size;
	_OffsetType offset;
} _DecodedInst;

typedef struct {
	const uint8_t* code;
	int codeLen;
	_OffsetType codeOffset;
} _CodeInfo;

typedef enum {DECRES_NONE, DECRES_SUCCESS, DECRES_MEMORYERR, DECRES_INPUTERR} _DecodeResult;
_DecodeResult internal_decode(_OffsetType codeOffset, const uint8_t* code, int codeLen, _DecodeType dt, _DecodedInst result[], unsigned int maxResultCount, unsigned int* usedEntriesCount);

_DecodeType ADDR_SIZE_AFFECT(_DecodeType dt, _iflags totalPrefixes);
_DecodeType OP_SIZE_AFFECT(_DecodeType dt, _iflags totalPrefixes, unsigned int rex, _iflags instFlags);

#endif /* DECODER_H */
