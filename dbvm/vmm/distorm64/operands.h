/*
operands.h

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#ifndef OPERANDS_H
#define OPERANDS_H

#include "../config.h"

#include "decoder.h"
#include "prefix.h"
#include "wstring.h"
#include "instructions.h"

/* Return codes from extract_operand. */
typedef enum {EO_HALT, EO_CONTINUE, EO_SUFFIX} _ExOpRCType;

_ExOpRCType extract_operand(_CodeInfo* ci,
                           _WString* instructionHex, _WString* operandText, _OpType type, _OpType op2,
                           _OperandNumberType opNum, _iflags instFlags, unsigned int modrm,
                           _PrefixState* ps, _DecodeType dt, int* lockableInstruction);

#endif /* OPERANDS_H */
