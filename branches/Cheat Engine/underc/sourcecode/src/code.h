// CODE.H
#ifndef __CODE_H
#define __CODE_H

typedef std::list<Instruction *> InstructionList;
typedef InstructionList::iterator LI;
typedef int (*CALLFN)();

class UCContext;

class Label {
    int m_offset;
    InstructionList m_refs;
    UCContext* m_code;
    Table* m_context;
public:
    Label(UCContext *code)
    { m_offset = -1; m_code = code; m_context = NULL; }

    void   context(Table* cntxt) { m_context = cntxt; }
    Table* context()             { return m_context;  }
    bool   addr_is_set()         { return m_offset > -1; }
    void here(int offs = 0);
    int addr();
	void remove(Instruction *pi);
    void patch_with_NOP();
};


typedef std::list<int> IntList;

class SwitchBlock {
protected:
   IntList m_list;
   int m_default_jmp;
   UCContext *m_code;
   Instruction *m_start;
public:
   SwitchBlock(UCContext *code);
   void add_jump(int val);
   void add_default();
   SwitchBlock *construct();
};

struct JIncBlock {
  int num;
  int idx;
  int size;
  int *ptr;
  int jmp;
  int flags;
};

class ConstructBlock {
private:
  PClass cls;
  PPClass vmt;
  int no,sz;
  PFBlock pfb;
  int flags;
public:
  static int make(PExpr er, int sz, bool is_dynamic, FBlock *pfb);
  PPClass get_VMT()    { return vmt; }
  bool dynamic()       { return flags & 2; }
  bool load_ODS()      { return flags & 1; }
  bool static_object() { return flags & 4; }
  PFBlock fblock()     { return pfb; }
  PClass class_ptr()   { return cls; }
  int size()           { return sz;  }
  int num()            { return no;  }
};


const int LVALUE = 1, DROP_VALUE = 2, AS_PTR = 4, AS_REF = 8;

class UCContext: public CodeGenerator {
public:
 static void init();
 static int builtin_conversion(Type t, Type te);
 void emit(int opcode, PExpr e); 
 void emit(int opcode, int rm=0, int data=0);
 void emit_data_instruction(int opcode, unsigned long data);
 void emit_push_int(int sz); 
 void emit_reference(PExpr ex, int flags, int tcode);
 void jump(int op, Label *lbl);
 void emit_stack_op(int op, Type t);
 void emit_type_instr(int opcode, Type t);
 void emit_except_throw(Type t);
 void emit_dynamic_cast(Type t);
 void emit_return(Type rt);
 void emit_native_function_call(Function *pfn, CALLFN fn);
 void compile(PExpr ex, int flags = 0);
};


#endif

