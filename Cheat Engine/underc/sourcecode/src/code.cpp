/* CODE.CPP
 * Code generation
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/
#include "common.h"
#include "code.h"
#include "opcodes.h"
#include "tparser.h"
#include "directcall.h"
#include "loaded_module_list.h"
    
#include <map>
namespace {
 std::map<int,int> equiv_op,float_equiv;
}

typedef unsigned char uchar;

int make_word(uchar c1, uchar c2)
{
  return c1 + (c2 << 8);
}

int builtin_conversion(Type t, Type te); 

void UCContext::jump(int op, Label *lbl)
{
  emit(op,NONE,lbl->addr());
}

void UCContext::emit(int opcode, PExpr e)
{
  if (!e) emit(opcode,NONE,0);
  else {
   assert(e->is_entry());
   PEntry pe = e->entry();
   if (pe->data == THIS_OFFSET) emit(PUSH_THIS,NONE,0);
   else emit(opcode,pe->rmode,pe->data);
  }
}

void UCContext::emit(int opcode, int rm, int data)
{
   emitc(opcode,(RMode)rm,data);
}

void UCContext::emit_data_instruction(int opcode, unsigned long data)
{
// The operand for a number of instructions is a 20bit offset to an allocated 32-bit word.
 emit(opcode,DIRECT,Parser::global().alloc_int(data));
}

typedef unsigned long ulong; 

void UCContext::emit_push_int(int sz) 
{
   emit_data_instruction(PUSHI,(ulong) sz);
}

void UCContext::emit_type_instr(int opcode, Type t)
{
   emit_data_instruction(opcode,Parser::AsTType(t));
}

void UCContext::emit_except_throw(Type t)
{
   emit_type_instr(THROW_EX,t);
}

void UCContext::emit_dynamic_cast(Type t)
{
  emit_type_instr(DCAST,t);
}

void UCContext::init()
{
 static int equivalents[] = {STAR,MUL,DIVIDE,DIV,PLUS,ADD,MINUS,SUB,
   MODULO,MOD,LSHIFT,SHL,RSHIFT,SHR,BIN_AND,AND,BIN_OR,OR,
   BIN_XOR,XOR,EQUAL,EQ,NOT_EQUAL,NEQ,LESS_THAN,LESS,GREATER,GREAT,
   LEQ,LE,GEQ,GE,UMINUS,NEG,LOG_NOT,NOT};
 static int float_equivalents[] = {MUL,FMUL,DIV,FDIV,ADD,FADD,SUB,FSUB,
   EQ,FEQ,LESS,FLESS,GREAT,FGREAT,NEG,FNEG,NEQ,FNEQ,LE,FLE,GE,FGE}; 

 int i;
 for (i = 0; i < sizeof(equivalents)/sizeof(int); i+=2)
   equiv_op[equivalents[i]] = equivalents[i+1];

 for (i = 0; i < sizeof(float_equivalents)/sizeof(int); i+=2)
   float_equiv[float_equivalents[i]] = float_equivalents[i+1];
}

// *fix 0.9.3 Replaced naive is_double check in builtin conversions and stack droppings
bool is_double_number(const Type& t)
{ return t.is_double() && !t.is_pointer() /*!t.is_ref_or_ptr()*/ ; }

int UCContext::builtin_conversion(Type t, Type te)
{
// *fix 1.1.4 We could not do a simple float to int conversion!
// note: returning 0 means no operation!
   if (t.is_int()) {
       if(te.is_int()) return 0;
	   if(!te.is_pointer()) {
         if(te.is_double()) return(D2I); else
         if(te.is_float())  return(F2I);
		 else return 0;
       }  
       else return 0;
   } else
   if (t.is_float()) {
       if(t.is_double() && !t.is_pointer()) {
         if(te.is_int()) return(I2D); else
         if(te.is_single()) return(F2D); else return 0;
       } else
       if(t.is_single()) {
        if(te.is_double() && !te.is_pointer()) return(D2F); else
        if(te.is_int()) return(I2F); else return 0;
       }
       else return 0;
   }
   else return 0;
}

///------------------Label class and management functions-----------------------------
void Label::here(int offs)
{
  m_offset =   m_code->ip_offset()+offs;
  if (m_refs.size() > 0) 
    for(LI p = m_refs.begin(); p != m_refs.end(); ++p)
     (*p)->data = m_offset;
}

int Label::addr()
{
  if(m_offset >= 0) return m_offset;
  m_refs.push_back(m_code->current_pi());
  return 0;
}

void Label::remove(Instruction *pi)
{
  m_refs.remove(pi);
}

// *add 1.2.5 A specialized operation: patch the instruction before the first reference with a NOP
void Label::patch_with_NOP()
{
 Instruction* ii = m_refs.front();
 ii--;
 ii->opcode = HALT;
 ii->rmode  = 0;
 ii->data   = 999;
}

///---------- Switch block generation
SwitchBlock::SwitchBlock(UCContext *code) : m_code(code), m_default_jmp(0)
{
  m_start = code->current_pi();
}

void SwitchBlock::add_jump(int val)
{
 m_list.push_back(val);
 m_list.push_back(m_code->ip_offset());
}

void SwitchBlock::add_default()
{
 m_default_jmp = m_code->ip_offset();
}

SwitchBlock *SwitchBlock::construct()
{
 if(!m_default_jmp) m_default_jmp = m_code->ip_offset();
 int sz = m_list.size()+2;
 int *block = new int[sz];
 block[0] = m_list.size()/2;
 block[1] = m_default_jmp;
 IntList::iterator ili;
 int i = 2;
 for(ili = m_list.begin(); ili != m_list.end(); ++ili) 
   block[i++] = *ili;
 m_start->data = Parser::global().alloc(sz*sizeof(int),block);
 delete block;
 return this;
}

void UCContext::emit_stack_op(int op, Type t)
{
// the stack operations DUP & DROP come in single and double varieties.
 if (is_double_number(t)) emit(op+1);
 else if (t != t_void) emit(op);
}

// a var encodes in three distinct ways
// a NULL reference means that this is relative to TOS!
void UCContext::emit_reference(PExpr ex, int flags, int tcode)
{
 if (ex) {
   PEntry pe = ex->entry();
   Type t = ex->type();
  // *add 1.1.4 bit fields have their own instruction
   if (pe->is_bitfield()) {
     emit ((flags & LVALUE ? POPIF : PUSHIF),ex);
   } else
// special encoding for _function_ IREFs.  Clearly this is not
// complete - it assumes that there is only one fn in the set!
   if (t.is_function()) {
        FunctionEntry *pfe = (FunctionEntry *)pe->data;
        emit_push_int((int) pfe->back()->fun_block());
   } else
// *fix 1.2.0 Special case for regular references; their 'value' is a pointer
   if ((flags & AS_PTR) && ! t.is_plain_reference()) emit(PEA,ex);
   else emit ( (flags & LVALUE ? POPC : PUSHC) + tcode,ex);
 } else {
  if (!(flags & AS_PTR)) emit( (flags & LVALUE ? POPSC : PUSHSC)+tcode,NONE,0);
 }
}

#include <stdarg.h>
int choose(int val,...)
{
  int key,value;
  va_list ap;
  va_start(ap,val);
  do {
    key = va_arg(ap,int);
    if (key==0) return 0;  // not found!
    value = va_arg(ap,int);
  } while (val != key);
  va_end(ap);
  return value;
}

enum { CHAR_SZ, SHORT_SZ, POINTER_SZ, DOUBLE_SZ };

int size_code(Type t)
{
  if (t.is_pointer()) return POINTER_SZ;
  if (t.is_char() || t.is_bool())    return CHAR_SZ;  // *fix 1.2.4 bools are now one byte
  if (t.is_short())   return SHORT_SZ;
  if (t.is_double())  return DOUBLE_SZ;
  else return POINTER_SZ;
}

bool passthrough_conversion(Type t,PExpr ex)
{
   if (ex==NULL) return false;
   if (ex->op() != BCAST) return false;
   return UCContext::builtin_conversion(t,ex->type()) == 0;
}

// interpretation of flags....
//  bool dynamic()       { return flags & 2; }
//  bool load_ODS()      { return flags & 1; }

int ConstructBlock::make(PExpr er, int sz, bool is_dynamic, FBlock *pfb)
{
  using Parser::global;
  int icb = global().alloc(sizeof(ConstructBlock),NULL);
  ConstructBlock *pcb = (ConstructBlock *)global().addr(icb);
  Class *pc = pfb->class_ptr; 
  pcb->cls = pc;
  pcb->vmt = pc->get_VMT();
  pcb->no = sz;
  pcb->pfb = pfb;
  pcb->sz = pc->size();
  pcb->flags = 0;
  if (is_dynamic) pcb->flags = 2;
  if (er != NULL && er->is_entry()) { //*NOTE*
      bool is_auto = er->entry()->is_stack_relative();
      pcb->flags += 1;
      if (! is_auto) pcb->flags += 4;
  }
  return icb;
}

void push_object_ptr(UCContext *cntxt, PExpr e)
{
   // push expression on the Object Stack
//   if (Parser::state.in_method()) cntxt->pushed_op(true);
   if (e && e->is_entry() && e->entry()->type.is_object()) cntxt->emit(LOS,e);
   else {
       if (e) cntxt->compile(e);    
       cntxt->emit(LOSS);
   }
}

int offset_to_fun_block(Function *pf) 
{
 return Parser::global().offset(pf->fun_block());
}

static bool dont_use_map(Function *pf)
{
 return pf->class_context()->has_true_VMT();
}

void compile_function_call(UCContext* code, int op, int flags, PExpr ex, bool use_obj=false, PExpr obj=NULL, int icb=0, bool is_dynamic=false)
{
// *fix 0.9.6 The DCALL function type _forces_ a function to be directly called, virtual or not
     PExprList args = ex->arg_list(); 
     Type ret_type;
     Instruction* pi;
     bool function_ptr_call = op==EXPR || op==EXPR_METHOD;
     PFunction pf = function_ptr_call ? NULL : ex->function();
     int sz = 0;
     // If this was a method, then the object ptr is at the end of the arguments
     //*PASOP* Also true for _static methods_???
     //*unless we are explicitly given an object ptr, of course.
     if (!use_obj && (op==METHOD_CALL || op==EXPR_METHOD)) {
        obj = args->back();
        args->pop_back();
     }

   // compile the args in _reverse_ order!
     ExprList::reverse_iterator ali;
     if (args != NULL) 
       for (ali = args->rbegin(); ali != args->rend(); ++ali) { 
          code->compile(*ali);
		  if (is_double_number((*ali)->type())) { 
            sz += 2;
		  } else ++sz;
      }      
	 bool was_method_call = obj != NULL || is_dynamic;  
     if (was_method_call) {// the object stack is pushed immediately before the call
	    push_object_ptr(code,obj);             // obj ptr onto the object stack
     }
     // *change 1.2.0 PUSH_THIS etc has been moved to emit_native_function_call()

     if (function_ptr_call) {  // call via a function ptr...
         PExpr e1 = ex->arg1();
         ret_type = e1->type().as_signature()->return_type();
         code->compile(e1); 
 // issue: stdarg??
         code->emit(CALLS); 
           // and obviously we gotta do VCALLS.... 
     } else {
       ret_type = pf->return_type();    
       // calls to native cdecl-style routines like printf()
       // require us to push the number of arguments!
      if (pf->stdarg() && pf->builtin())  code->emit_push_int(sz);
      // *add 1.2.3b Single-instruction functions can be safely inlined
      if (Parser::debug.attempt_inline && (pi = CodeGenerator::has_one_instruction(pf)) != NULL) {
         code->out(pi);
      } else {   
// *add 1.2.0 Imported classes may need the VMT-map equivalents!
// *add 1.2.4 Virtual calls are proceeded by a 'NOP' carrying the original fun block!
      if (pf->is_virtual() && op!=DCALL) {
	        if (! Parser::debug.attempt_inline) code->emit(HALT,DIRECT,offset_to_fun_block(pf));
            code->emit(dont_use_map(pf) ? VCALL : VCALLX,DIRECT,pf->slot_id()); 
      } else if (icb != 0) { 
        if (icb > 0)
            code->emit(CCALLV,DIRECT,icb);                    // vector ctor/dor,
        else 
            code->emit(dont_use_map(pf) ? CCALL : CCALLX ,DIRECT,offset_to_fun_block(pf)); // scalar ctor w/ VMT.
      } else { // plain call, no frills.
        code->emit(CALL,DIRECT,offset_to_fun_block(pf));
      }   
	  // *add 1.2.3b Correct stack pointer for cdecl calls (remember that builtins get arg sz pushed as well!)
	  int sp_diff,rt; 
	  if (pf->stdarg()
		  && (sp_diff = sz - pf->fun_block()->nargs + (pf->builtin() ? 1 : 0)) > 0)
      {
		    if (is_double_number(ret_type)) rt = 2;
			else if (ret_type != t_void) rt = 1;
			else rt = 0;
	  	    code->emit(ADDSP,NONE,make_word(rt,sp_diff));
      }
      }
     }
     // if there's a supplied object, then caller will handle this
     // CCONTEXT case!
     if (!use_obj && obj != NULL) code->emit(DOS);
     if (flags & DROP_VALUE) code->emit_stack_op(DROP,ret_type);
}

void UCContext::emit_return(Type rt)
{
 // *fix 0.9.3 double& was being returned w/ RETD!
 int op;
 if (rt.is_double() && ! rt.is_ref_or_ptr()) op = RETD; else
 if (rt == t_void) op = RET;  
 else op = RETI;
 emit(op);
}

void UCContext::emit_native_function_call(Function *pfn, CALLFN fn)
{
 Type rt = pfn->return_type();
// *change 1.2.0 Moved from compile_function_call()
// for cdecl-style method imports, 
 if (pfn->is_cdecl() && pfn->is_method()) {
      // push mOP onto exec stack
     emit(PUSH_THIS,0);       
     //  do note that returned objects are by a another hidden arg
     // *fix 1.1.4 The return type must specifically be an _object_, not a pointer to an object!
     if (rt.is_object()) emit(SWAP,0);
 }

 emit(CALLN,DIRECT,NFBlock::create(pfn,fn));

 pfn->fun_block()->data = (void *)fn; // convenient place to keep this!

 // We need to check imported class object ptrs to see if they have a VMT....
 // Any imported class ptrs need their UC VMT patched with CHKVMT
 // *fix 1.1.0 Imported references as well as pointers!
 
 
 if (rt.is_class() && rt.is_ref_or_ptr()) {
    PClass pc = pc = rt.as_class();
	if (pc->imported() && pc->has_VMT()) 
      out(CodeGenerator::instruction_with_pointer(CHKVMT,pc));
 }
 // *add 1.2.9 Sometimes necessary to clear out upper 3bytes of 32-bit boolean values
 else
 if (rt == t_bool) {
   emit(I2B);
 }

}

void UCContext::compile(PExpr ex, int flags)
{
  static Label l1(this),l2(this);
  int tcode,ttype,opcode,op = ex->op();
  PExpr e1 = ex->arg1(), e2 = ex->arg2();
  Type t = ex->type();  
  //t.strip_qualifiers();
  opcode = equiv_op[op];
  if (opcode) {
  // the type of an operator usually tells us what code to emit
  // except in the case of relational ops!
      if (t.is_bool()) t = e1->type(); 
      // *fix 0.9.3 -- ptr check missing!
      if (t.is_double() && ! t.is_pointer())
         opcode = float_equiv[opcode];
      compile(e1);
      if (e2) compile(e2);
      emit(opcode,NONE,0);
      return;
  } 
  tcode = size_code(t);

  // at this point we can remove any non-trival typecast...
  if (passthrough_conversion(t,e1)) e1 = e1->arg1();

  switch(op) {
  case ARRAY: {
    bool is_entry = e1->is_entry();
    int  array_size = is_entry ? Parser::array_size(e1->entry()) : 0;
    if (t.is_pointer()) {
      t.decr_pointer();
      tcode = size_code(t);
    }
    compile(e2);    // put index on stack
    // *add 1.2.5 Check for array bounds, if requested
    if (Parser::debug.range_check && array_size > 1) {
        emit_push_int(array_size);
        emit(CALLN,DIRECT,Builtin::range_check_function());
    }
    if (t.is_class()) {
      int sz = t.size();
      if (sz > sizeof(double)) {
        emit_push_int(sz);
        emit(MUL);    // TOS will be index*sizeof(T)
        compile(e1);  // will give us the addr      
        emit(ADD);
        break;
       } else {
        if (sz == 4) tcode = 2; else tcode = 3;
       }
    } 
    if (is_entry) {
       if (array_size > 1) emit(ADDCC + tcode,e1);
       else emit(ADDPC + tcode,e1);
    } else {
      compile(e1);
      if (tcode==0) emit(ADD); 
      else emit(ADDSW+tcode-1);
    }
  } break;
  case DOT:
     push_object_ptr(this,e1);
     compile(e2,flags);  
     emit(DOS);
   break;
  case CCONTEXT: // constructor call, w/ or w/out object disposal
  { 
      Function *pf = e2->function();
	  PClass pc = pf->class_context();
      compile_function_call(this,FUNCTION,flags,e2,true,e1,pc->has_VMT() ? -1 : 0); 

    // *fix 1.0.0 Any temporaries must be unwound before pushing obj on ODS
	// (only if we are a plain auto var)
      if (t==t_void) Parser::check_temp_context(); 

      // *fix 1.2.0L allow for override (needed for GCC imports) 
      if (Parser::temp_context()->no_auto_dtor() ) emit(DOS);
      else {	        
        if (e1->is_entry()) {
           PEntry pe = e1->entry();
	       if (pc->destructor() == NULL && ! pc->has_VMT()) emit(DOS);
           else {
               // Local 'auto' objects get pushed onto the ODS;
               // *add 1.2.3 Put statically created objects in the static ODL (Object Destructor List)
               bool is_auto = pe->is_stack_relative();
               bool is_direct = pe->is_direct();
               if (! is_auto && ! is_direct) emit(DOS); 
               else if (is_auto) {
                   // *fix 1.2.3 Only set the first object if it _will_ be on the ODS
                   Expressions::set_first_object(e1);  // set as the first object on the stack frame
                   emit_data_instruction(TOSD,(ulong)pc);
               }
               else { // if (is_direct)
                // *change 1.2.9 The static ODS has been retired; instead, we keep
                // program and global ODLs which are evaluated at program close and
                // session close respectively.
                 if (pc->destructor())
                   LoadedModuleList::ODL_add(pe->global_ptr(),pc->destructor());
               } 
           }
        }	  
        else emit(TOSX);
      }    
  }
  break;
  case FCONTEXT:  // function returning an object requiring destruction 
  // *change 1.0.0 We use TPODS instruction before functions returning objects...
  	 push_object_ptr(this, e1);
	 Expressions::set_first_object(e1);  // set as the first object on the stack frame
     emit_data_instruction(TPODS,(ulong)t.as_class());
  break;
  case DCONTEXT: // dynamic scalar ctor or dtor call.
  { 
      Function *pf = e1->function();
	  PClass pc = pf->class_context();
	  int icb = (pc->has_VMT() && pf->is_constructor()) ? -1 : 0;
      compile_function_call(this,METHOD_CALL,flags,e1,true,NULL,icb,true); 
      emit(TOSX);
  }
  break;
  case VCONTEXT: // vector ctor or dtor;  static if we're passed an array entry
  {
      Function *pf = e2->function();
      int arr_sz;
      PEntry pe;
	  if (e1) {
           pe = e1->entry();
		   arr_sz = Parser::array_size(pe);
	  } else arr_sz = 0;
	  bool is_dynamic = arr_sz == 0;
      // *change 1.2.9 For each object in the _static_ array, add to the ODL;
      // The vector ctor instruction no longer pushes the ODS for static objects.
      if (! is_dynamic && pe->is_direct()) {
          PClass pc = pf->class_context();
          if (pc->destructor()) {
            char* ptr = (char*)pe->global_ptr();
            int tsz = pe->type.size();
            for (int i = 0; i < arr_sz; i++) {
              LoadedModuleList::ODL_add(ptr,pc->destructor());
              ptr += tsz;
            }
          }
      } 
      int icb = ConstructBlock::make(e1,arr_sz,is_dynamic,pf->fun_block());              
      compile_function_call(this,FUNCTION,flags,e2,true,e1,icb,is_dynamic);      
  }
  break;
  case PASS_BY_VALUE: { // needed to match MSVC object model
       int sz = t.size();
       emit(STALC,NONE,sz/sizeof(int));  // allocate on stack (pushes object stack!)
	   compile(e1);           // construct object
	   emit(DOS);             // pop OS 
       // *fix 1.2.0 There was code to reverse stuff on stack, but the UC stack now grows 
       // downwards, like most processor stacks.  This stuff has been broken since 1.1.0!!
     }
  break;

  case ADDR:
    compile(e1,AS_PTR);
    break;
  case DEREF:
    // Special case: dereferencing a pointer to an object
    // *fix 0.9.7 left stack droppings because we didn't use flags (DROP!)
    // *hack 0.9.8 AS_PTR case is different. Surely there is some pattern here?
    // *hack 1.1.2 Being on the RHS of a reference init. is now explicit!
    if (flags & AS_PTR) {
        if (! (flags & AS_REF) && t.is_class() && !t.is_pointer() && !t.is_plain_reference())
			compile(e1,flags);
        else 
          compile(e1);      
    } else {
        if (t.is_class() && !t.is_pointer()) compile(e1,flags);
        else {
          compile(e1);
          // Special case: a function ptr
          if (t.is_function()) break;
          emit_reference(NULL,flags,tcode); 
        }
    }
    break;
   case IREF:  // objects and arrays produce references
    t = ex->entry()->type; // redundant??
    if (t.is_object() || Parser::array_size(ex->entry()) > 1) emit( PEA, ex);
    else {
       if (t.is_reference()) tcode = POINTER_SZ;  // *fix 0.9.7 deref. double references
       emit_reference(ex,flags,tcode);
    }
    break;
   case ASSIGN:
    if (!e1) { // special case when ref is already on stack!
       emit(DUP);        // dup the pointer
       compile(e2);      // compile expr
       emit(is_double_number(e2->type()) ? SWAPD : SWAP);     // so ptr is TOS
       emit_reference(NULL,LVALUE,tcode);
    } else {
    // *fix 1.2.0 Assignments were not coded correctly in lvalue situations (e.g 'int& ri = (i = 10)')
      bool do_push = !(flags & DROP_VALUE), as_ref = (flags & AS_REF);
      compile(e2);
      if (do_push & ! as_ref) emit_stack_op(DUP,e2->type());
      compile(e1,LVALUE);
      if (do_push & as_ref) compile(e1,AS_PTR);
    }
    break;
   case INCR:      case DECR:
   case INCR_PTR:  case DECR_PTR:
   // 
    opcode = 0;
    if (t.is_pointer()) {
      t.decr_pointer();
      ttype = size_code(t);
    } else ttype = tcode;
    if (e1->is_entry()) { 
       Type at = e1->entry()->type;
       if (at.is_class()) {
        int sz = t.size();
        if (sz == sizeof(int)) ttype = 2; else
        if (sz == sizeof(double)) ttype = 3;
        else ttype = -1;
       }
       if (ttype != -1) {
        if (!(at.is_ref_or_ptr() && (op==INCR || op==DECR)))
         opcode = choose(op,INCR,INCC,DECR,DECC,INCR_PTR,INCPC,DECR_PTR,DECPC,0);
       } else { // ptr to struct!! Encode p = p + sizeof(T)
          if (e2!=NULL) emit_reference(e1,0,2); // postfix
          emit_reference(e1,0,2);
          emit_push_int(t.size());
          emit(op==INCR_PTR ? ADD : SUB);
          if(e2==NULL) emit(DUP); // prefix
          emit_reference(e1,LVALUE,2);
          break;
       }
    }
    if (opcode == 0) {
       opcode = (op==INCR || op==INCR_PTR) ? INCSC : DECSC;
       compile(e1,AS_PTR);                   // forced to yield a reference
       if (!(flags & DROP_VALUE)) emit(DUP); // dup if we're pushing value
       e1 = NULL;                            // force stack-relative push/pop!
    }
    if (e2==NULL) emit(opcode + ttype, e1);  // prefix
    if (!(flags & DROP_VALUE)) { 
      emit_reference(e1,flags,tcode);
      // w/ postfix, must keep the reference on TOS!
      if (e2!=NULL && e1==NULL) emit(SWAP,NONE,0);
    }
    if (e2!=NULL) emit(opcode + ttype, e1);  // postfix 
    break;

   case FUNCTION:  case DCALL: case EXPR:  case METHOD_CALL:  case EXPR_METHOD:
    compile_function_call(this,op,flags,ex);
    break;
   case BCAST:
    compile(e1);
    op = builtin_conversion(t,e1->type());
    if (op) emit(op);
    break;
   case LOG_OR: case LOG_AND:
    {
    Label l1(this);
    compile(e1);     
    jump(op==LOG_OR ? JNZND : JZND,&l1);
    compile(e2);
    Parser::check_temp_context(); // *fix 0.9.6 NB to do this _before_ any jumps
    l1.here();
    }
    break;
   case ARITH_IF:
    { Label l1(this),l2(this);
     compile(e1);
     jump(JZ,&l1);
     compile(e2->arg1(),flags);
     jump(JMP,&l2);
     l1.here();
     compile(e2->arg2(),flags);
     l2.here();  
    } break;
   case COMMA: 
   case APPEND:
     compile(e1,op == COMMA ? DROP_VALUE : 0);
     compile(e2,flags);
     break;
   case COPY_BLOCK:
     compile(e1);
     if (!(flags & DROP_VALUE)) emit(DUP);
     compile(e2);
     emit(COPY,NONE,e1->type().size());
     break;
   case INIT_REF: // *hack 1.1.2 Make reference init. case more explicit! (see DEREF)
     compile(e2,AS_PTR | AS_REF);
     compile(e1,LVALUE);
     break;
   case DYNACAST:
     compile(e1);
     emit_dynamic_cast(t);
     break;
   case REF_STUB: // make a reference out of a non-reference (not needed for constants)
     compile(e1); // put expression on stack
     compile(e2,LVALUE); // pop into temp variable 
     compile(e2,AS_PTR); // push addr of temp
     break;
   default:
     cerr << "Unrecognized opcode: " << op << endl;
     //throw string("compile failed");
     break;
   }
}




