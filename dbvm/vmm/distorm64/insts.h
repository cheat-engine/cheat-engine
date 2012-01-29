/*
insts.h

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#ifndef INSTS_H
#define INSTS_H

#include "instructions.h"

/* Root Trie DB */
extern _InstNode Instructions;
/* 3DNow! Trie DB */
extern _InstNode Table_0F_0F;
/* NOP/XCHG instruction. */
extern _InstInfo II_90;
/* LEA instruction. */
extern _InstInfo II_8D;

/* See instructions.cpp for more info. */

#endif /* INSTS_H */
