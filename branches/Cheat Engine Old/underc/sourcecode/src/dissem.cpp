/* DISSEM.CPP
 * Stackcode Dissembler
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/
#include "common.h"
#include "opcodes.h"
#include "breakpoints.h"
#include "directcall.h"

extern const Instruction *end_of_code; // shared w/ ENGINE
extern char *mDataSeg;

string get_opcode_name(int op);  // at end of this module...

char *data_ptr(int offs)
{ return mDataSeg + offs; }

void dissemble(PFBlock fb)
{
 Opcodes opcode;
 int rmode,rdata, k = 0;
 string name;
 Instruction *pi = fb->pstart;
 while (pi != end_of_code) {
    opcode = (Opcodes)pi->opcode;   
	// *add 1.2.4 HALT+data is not always a breakpoint!
    // (it is used as a NOP + <any useful tag data>)
    if (opcode == HALT) {
        if (pi->data < MAX_BREAKPOINTS) {
          Breakpoint *pb = Breakpoint::from_id(pi->data);
          Instruction ai = pb->saved_instruction();
          cout << "*";
          opcode = (Opcodes)ai.opcode;
          rmode = ai.rmode;  rdata = ai.data;
        } else { opcode = NOP; rdata = pi->data; }
    } else {
      rmode  = pi->rmode;  rdata  = pi->data;
    }
    name =  get_opcode_name(opcode);
    cout << k++ << ' ' <<  name << '\t';
    if (opcode == CCALL || opcode == CALL || opcode == CALLD || opcode == CALLN) {
       FBlock* pfb;
       void *data = data_ptr(rdata);
       if (opcode == CALLN)
           pfb = Builtin::imported_fblock_from_function((void*)((NFBlock *)data)->pfn);
       else pfb = PFBlock(data_ptr(rdata));     
      if (pfb) Function::from_fun_block(pfb)->dump(cout);      
    } else 
    if (opcode == JSWITCH) {
      int *swb = (int *)data_ptr(rdata);
      int sz = *swb++;
      int def = *swb++;
      cout << '(' << sz << ',' << def << ") ";
      for (int i = 0; i < sz; i++) cout << *swb++ << ' ' << *swb++ << ' ';
    }
    else
    if (opcode == TOSD || opcode == TPODS) {
       PClass pc = *(PClass *)data_ptr(rdata);
       cout << pc->name();
    }
    else {
     if (rmode) 
      switch(rmode) {
      case DIRECT: cout << "D ";  break;
      case SREL:   cout << "R ";  break;
      case OREL:   cout << "S ";  break;
     }
     if (rdata != 0) cout << rdata;
   }
   cout << endl;  
   if (opcode == RET || opcode == RETI || opcode == RETD) break;
   pi++;
 }
}

#include <map>
typedef std::map<int,string> NameMap;
NameMap opc_map;

string get_opcode_name(int op)
{
 return opc_map[op];
}

void opcode_add(char *name, int id)
{
  opc_map[id] = name;
}

#include "opcodes.h"

#define TOK(n) 
#define TNAME(n) 

void dissembler_init()
{
#define OP0(n)   opcode_add(#n,n);
#define OP1 OP0
#define JOP(n)   opcode_add(#n,n);
# include "ops.h"
}

