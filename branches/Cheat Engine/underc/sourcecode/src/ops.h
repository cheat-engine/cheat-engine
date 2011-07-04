/* ops.h
 * Lists and classifies the TOKs used in the Assembler
 * note: the TOKs will be #defined by BISON; this is merely
 *       used to map the string onto the value
 ************************************************************
*/
// keywords!
        TOK(CLASS)
        TOK(PROC)
    TOK(VIRTUAL)
    TOK(STATIC)
        TOK(ARG)
        TOK(LOAD)
        TOK(LLOCAL)
// typenames!
    TNAME(INT)
    TNAME(FLOAT)
    TNAME(CHAR)
    TNAME(DOUBLE)
    TNAME(SHORT)

// Built-in functions
//  CDECL(printf)
        

// Opcodes that take no operands
//------------------------------
    OP0(DROP)    // drop stack (double word)
    OP0(DROP2)   // drop 2 stack items (quad word)
        OP0(DUP)     // ditto, stack dup 
        OP0(DUP2)
    OP0(DOS)     // drop object stack
    OP0(TOSX)    // ditto,              push X stack
        OP0(TOSD)    // ditto,              push ODS
    OP0(RET)     // return from sub, no return value
    OP0(RETI)    // dword return value
    OP0(RETD)    // qword return value
    OP0(MUL)    // integer multiplication
    OP0(DIV)    // signed division
    OP0(IDIV)   // unsigned division
    OP0(ADD)
    OP0(SUB)
    OP0(MOD)    // modulo
    OP0(SHL)
    OP0(SHR)
    OP0(AND)
    OP0(OR)
    OP0(XOR)
    OP0(NOT)
    OP0(BNOT)
    OP0(EQ)
    OP0(NEQ)
    OP0(LESS)
    OP0(GREAT)
    OP0(LE)
    OP0(GE)
    OP0(NEG)
//.......floating point operations (double precision).....
    OP0(FMUL)       
    OP0(FDIV)
    OP0(FADD)
    OP0(FSUB)
    OP0(FEQ)
    OP0(FNEQ)
    OP0(FLESS)
    OP0(FGREAT) 
    OP0(FLE)
    OP0(FGE)
    OP0(FNEG)
//.......conversion operations
    OP0(D2I)    // double to integer
    OP0(I2D)    // integer to double
    OP0(F2D)    // float to double
    OP0(D2F)    // double to float
    OP0(I2F)    // int to float
	OP0(F2I)    // and vice versa!
    OP0(I2B)    // 32-bit integer to 8-bit bool 

// Opcodes that take one operand
    OP1(ENTER)      // enter & set up object destructor stack
    OP1(PODS)       // push ODS
    OP1(UNWIND)     // unwind ODS
    OP1(TUNWIND)    // unwind temporary ODS
    OP1(TPODS)      // push temp ODS & e-stack
    OP1(PUSH_THIS)  // push obj ptr onto e-stack
    OP1(PUSH)       // push value onto e-stack
    OP1(PUSHC)
    OP1(PUSHW)
        OP1(PUSHI)
    OP1(PUSHD)  
    OP1(PUSHF)
    OP1(PUSHS)
    OP1(POP)        // pop value from stack into variable
    OP1(POPC)
    OP1(POPW)
    OP1(POPI)
    OP1(POPD)
    OP1(POPF)
    OP1(PUSHSS)      // push value using addr on stack
    OP1(PUSHSC)
    OP1(PUSHSW)
    OP1(PUSHSI)
    OP1(PUSHSD)
    OP1(PUSHSF)
    OP1(POPS)       // pop 
    OP1(POPSC)
    OP1(POPSW)
    OP1(POPSI)
    OP1(POPSD)  
    OP1(ADDCC)
    OP1(ADDCW)
    OP1(ADDCI)
    OP1(ADDCD)
    OP1(ADDPC)
    OP1(ADDPW)
    OP1(ADDPI)
    OP1(ADDPD)
    OP1(ADDSW)
    OP1(ADDSI)
    OP1(ADDSD)
    OP1(ADDSN)

    OP1(INC)        // increment variable by one
    OP1(INCC)
    OP1(INCW)
    OP1(INCI)
    OP1(INCS)
    OP1(DEC)        // decrement variable by one
    OP1(DECC)
    OP1(DECW)
    OP1(DECI)
    OP1(DECS)
    OP1(INCP)       // increment ptr by one
    OP1(INCPC)
    OP1(INCPW)
    OP1(INCPI)
    OP1(INCPD)
    OP1(DECP)       // decrement ptr by one
    OP1(DECPC)
    OP1(DECPW)
    OP1(DECPI)
    OP1(DECPD)
    //OP1(INCS)       // increment stack ref by one
    OP1(INCSC)
    OP1(INCSW)
    OP1(INCSI)
    //OP1(DECS)       // decrement stack ref by one
    OP1(DECSC)
    OP1(DECSW)
    OP1(DECSI)

    OP1(COPY)       // mem copy
    OP1(SWAP)       // swap two dwords on stack
	OP1(SWAPD)      // swap dword with qword on stack
	OP1(REVN)       // reverse N items on stack (REVN 2 equiv. to SWAP)
    OP1(JSWITCH)     // do switch table jump 
    OP1(PEA)    // push effective address
    OP1(PERA)   // push effective _relative_ address
    OP1(LOS)    // load object stack    
    OP1(LOSS)       // ditto, but from TOS
        OP1(CALL)       // normal pcode call
    OP1(CALLN)  // native code call
    OP1(CCALL)  // constructor call
    OP1(CCALLV)     // vector constructor call
    OP1(VCALL)  // virtual method call
    OP1(CALLD)      // call & drop OS 
        OP0(CALLS)      // call using fblock on stack
        OP0(VCALLS)     // call using VMT slot on stack
    OP1(CHKVMT)     // check imported object ptr's VMT
    OP1(VTABLE_PATCH)  // patch native vtable
	OP1(STALC)         // alloc object on call stack
        OP1(DCAST)  // dynamic cast
    JOP(JMP)        // various jumps
    JOP(JZ)
    JOP(JNZ)
    JOP(JZND)
    JOP(JNZND)
        JOP(JINC)
    OP1(INIT_JINC)
        OP1(THROW_EX)
    OP1(POPIF)
	OP1(PUSHIF)
    OP1(CCALLX)
    OP1(VCALLX)
	OP1(ADDSP) // *add 1.2.3b
       
   //////// Miscelaneous testing ad-hoc instructions ///////
    JOP(LOOP)
    OP1(SETC)
    OP1(SHOWS)
    OP1(SHOWI)
    OP1(SHOWD)
    OP1(SHOWV)
    OP0(HALT)
    OP0(NOP)

