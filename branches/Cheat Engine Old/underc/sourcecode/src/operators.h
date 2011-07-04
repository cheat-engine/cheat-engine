// operators.h

#ifndef __OPERATORS_H
#define __OPERATORS_H

#include "classlib.h"
#ifdef NEEDS_LOOKUP
#include "tokens.h"
#endif

namespace Operators {

void init();
string name_from_id(int id);
#ifdef NEEDS_LOOKUP
int lookup(int ch, TokenStream& ts);
#endif
void add(char *first, int id,...);
void init_lookup();

};

#endif

