// Fblock.h

#ifndef __FBLOCK_H
#define __FBLOCK_H
#include "export.h"

// FBlock contains all that the runtime system needs to know about a function
struct EXPORT FBlock {
    PInstruction pstart;     // actual pcode
    int       nlocal;        // reserved for local variables (dwords)
    int       nargs;         // no. of args explicitly passed (dwords)
    int       ntotal;        // the sum of nlocal and nargs
    Table*    context;       // symbol table context
    Class*    class_ptr;     // class pointer - NULL if not method
    Entry*    entry;         // specific entry in table
    Function* function;      // actual corresponding function object
    void*     data;          // extra data (will be addr of wrapped native code)
    XTrace*   trace;         // optional trace object

    // more to follow....
    FBlock (PInstruction pc=NULL, int na=0)
    { pstart = pc;  nargs = na; nlocal = 0; context = NULL; data = NULL; }

    void *native_addr()   { return data; }

    static FBlock *create(Entry *pe, Class *pc);
    void finalize(int sz);
};

typedef FBlock *PFBlock;
typedef FBlock **PPFBlock;

#endif




