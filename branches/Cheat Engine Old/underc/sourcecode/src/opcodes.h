// OPCODES.H

#ifndef __OPCODES_H
#define __OPCODES_H

#define TOK(t)
#define TNAME(t)
#define OP0(t) t,
#define OP1(t) t,
#define JOP(t) t,

enum Opcodes {
    END_OP,
#include "ops.h"
    EOP
};  

#undef TOK
#undef TNAME
#undef OP0
#undef OP1
#undef JOP

#endif
