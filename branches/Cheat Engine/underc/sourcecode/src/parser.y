/* PARSER.Y
 * C++ Grammar
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/

%{
/* *SJD* essential prototypes for C++ compilation */
int yylex();
int yyerror(const char *s);
#include <stdlib.h>
#define xmalloc malloc

#define MSDOS
#include "common.h"
using namespace Parser;
using namespace Expressions;

#define YYERROR_VERBOSE 1
#define YYDEBUG 1

//#define YYPURE 

/* some shortcuts */
typedef Stack<bool,40> BoolStack;
BoolStack als(false);
BoolStack dcl_stack;
BoolStack typedef_stack;

bool dump_it = false;
PEntry last_type_entry;

PExpr gFunInit = NULL;  // used to flag pure virtual methods...

void dcl_set(bool yes, bool comma_flag) {
 dcl_stack.push(state.in_declaration);
 if (dcl_stack.depth() > 40) outln("runaway dcl stack");
 state.in_declaration = yes;
 als.push(comma_flag);  /* force ',' _not_ to be COMMA */
}

void dcl_reset() { 
 state.in_declaration = dcl_stack.pop();
 als.pop();
}

void force_comma_flag() { 
 als.clear();
 dcl_stack.clear();
 dcl_set(false,false);
}

bool in_arg_list()   {
 return als.TOS();
}

inline void enter_arglist() { dcl_set(false); }

void leave_arglist() { dcl_reset(); }

string tag_name; /* A fiddle */
bool IEF=false;
/*BoolStack ief_stack;*/
void IEF_set()   { /*ief_stack.push(IEF);*/ IEF=true; }
void IEF_reset() { IEF=false; /*ief_stack.pop();*/ } 

void ttpush(TType t) { tpush(AsType(t)); }

void raise_error(string msg) 
{
 int yyerror(const char *);

 if (state.err != "") { msg = state.err; state.err = ""; }
 state.reset();
 als.clear(); als.push(false); 
 yyerror(msg.c_str());
}

bool check_error()
{
 if (state.err != "") {
    raise_error(state.err);
	state.err = "";
	return true;
 }
 return false;
}

%}

%union{
  int    val; 
  long   ctype;
  char*  str;
  Entry* entry;
  Expr*  expression;
  ExprList *elist;
  Class *classptr;
  TypeList *typelist;
}

%token <str>   TOKEN
%token <entry> IDEN,CONSTANT,TYPENAME,TYPENAME_FUNCTION,TEMPLATE_NAME, TEMPLATE_NAME_EXPR,
%token <classptr> THIS_CLASSNAME
%token FLOAT,DOUBLE,UNSIGNED,INT,SHORT,LONG,CHAR,VOID,BOOL
%token TYPEDEF,CLASS,STRUCT,ENUM,OPERATOR,GOTO,UNION
%token <val> STATIC_CAST,CONST_CAST,DYNAMIC_CAST,REINTERPRET_CAST, STRUCT_X, CLASS_X, STRUCT_Y, CLASS_Y, UNION_Y
%token IF,ELSE,WHILE,DO,FOR,SWITCH,CASE,RETURN,CONTINUE,BREAK,OPERATOR,DEFAULT
%token NAMESPACE,USING,TRY,CATCH,THROW,TEMPLATE,EXTERN
%token THREEDOT, TYPEOF, EXPLICIT, FRIEND, LAMBDA, FAKE_INIT_LIST
%token <val> CONST,STATIC,STDCALL,API,VIRTUAL,PRIVATE,PROTECTED,PUBLIC,CLASS,STRUCT,UNION

/* C++ operators, in increasing order of precedence */
%left COMMA
%right <val> ASSIGN,MUL_A,DIV_A,MOD_A,ADD_A,MINUS_A,SHL_A,SHR_A,BAND_A,BOR_A,XOR_A
%left ARITH_IF
%left LOG_OR
%left LOG_AND
%left BIN_OR
%left BIN_XOR
%left BIN_AND
%left EQUAL, NOT_EQUAL
%left LESS_THAN, LEQ, GREATER, GEQ
%left LSHIFT,RSHIFT
%left PLUS,MINUS
%left STAR,DIVIDE,MODULO
%left MEMBER_ARROW, MEMBER_DOT
%right NEW,DELETE,TYPECAST,DEREF,ADDR,UPLUS,UMINUS,LOG_NOT,BIN_NOT,INCR,DECR,SIZEOF
%left TYPE_CONSTRUCT,FUN_CALL,ARRAY,ARROW,DOT
%left  BINARY_SCOPE
%right UNARY_SCOPE

%type <val> assign_op, poss_array
%type <ctype> tname_expr, tname_exp2, token, type_expr, type_expression,type_bracket,function_front
%type <ctype> typename_function, typename_class, typename_expr, template_class, template_expr, templ_item
%type <ctype> conversion_operator, class_parm, scope, pointer_expr
%type <val> poss_unsigned, poss_int,poss_const,typecast_type
%type <val> access_modifier,poss_access_modifier,class_or_struct,poss_derived, template_class_header, struct_or_class_x,
%type <val> class_or_struct_ex
%type <str> poss_tag, class_name, poss_class_name, class_id, class_item, template_class_name, token_or_typename
/* these should be 'expression'!! */
%type <expression> _expr,expr,poss_int_const,poss_initialization,poss_size,condition,poss_expr,brace_item
%type <expression> array_expr
%type <entry> scoped_name
%type <elist> expr_list,function_arg_list,poss_function_arg_list,init_list,brace_list, brace_expr
%type <classptr> this_classname
%type <typelist> template_type_list, templ_item_list
%%

program: statement_list ;

statement_list: /*empty*/
 |  statement                {statement_end();}
 | statement_list statement  {statement_end();}
;

block:
 '{'  { state.init_block(PLAIN_BLOCK); IEF_set(); }
  statement_list
  '}' { state.finalize_block(); IEF_reset();} 
;

statement: ';' /* empty statement */ 
  | block
  | declaration         
  | function_declaration
  | function_definition 
  | friend_declaration
  | expr ';'             {expression_end($1);}
  | typedef_stmt
  | access_specifier
  | if_stmt
  | if_else_stmt
  | while_stmt
  | do_stmt
  | for_stmt
  | switch_stmt
  | return_stmt
  | case_label
  | break_stmt
  | continue_stmt
  | goto_stmt
  | goto_label
  | template_class_declaration
  | template_function_declaration
  | try_catch_stmt
  | throw_stmt
  | namespace_declaration
  | using_directive
  | using_declaration
  | extern_c_declaration
  | error ';'   { raise_error("Error in statement"); YYABORT; }
;


declaration:
  mod_type_name tname_expr_list ';'  
  { 
    Type dt = tpop();
	dcl_reset();
	state.check_dcl_init(dt);
 }
;

typedef_stmt:
  TYPEDEF
  { state.in_typedef = true; }
   declaration_stmt
  { state.in_typedef = false; }
;

declaration_stmt:
  declaration
| function_declaration
;

 /* constructor/destructors */
this_classname:
  THIS_CLASSNAME 
  {
   dcl_set();
   state.token_stack.push($1->constructor_name());
   tpush(t_void);
   state.in_construct_destruct = IsConstructor;
   $$ = $1;
  }
;

construct_destruct:
  this_classname {}       
| BIN_NOT this_classname   
{
  state.token_stack.TOS() = $2->destructor_name();
  state.in_construct_destruct = IsDestructor;
}
;

conversion_operator:
 OPERATOR  mod_type_name tname_exp2 arg_list
 { 
   state.token_stack.push(CONVERSION_OPNAME);
   ttpush($3); $$ = $3;
  }
;

function_front:
  mod_type_name tname_expr arg_list poss_const poss_initialization
  { $$=$2; stots($$); state.member_is_const = $4; gFunInit = $5; }
;

explicit_mod: EXPLICIT
 { state.modifier = Explicit; }
;

ctor_dtor_dcl:
 construct_destruct arg_list ';'   
 {  
   state.declare_function(t_void,state.token_stack.pop());
   tpop();
   check_error();
 }
;

function_declaration:
 function_front ';'  
 {
   state.declare_function(AsType($1),state.token_stack.pop(), gFunInit); 
   dcl_reset();
   gFunInit = NULL;
   tpop();
   check_error();
 }
| ctor_dtor_dcl
| explicit_mod ctor_dtor_dcl
| conversion_operator ';'
  { 
    state.declare_function(AsType($1),state.token_stack.pop());
	tpop();
	check_error();
  }
;

/***** extern "C" ******/
/* *fix 1.1.4 
 *   - plain 'extern' is no longer confused with 'extern "C"'!
 *   - 'extern "C"' no longer has to be followed by a block
*/
extern_c_declaration: 
  extern_qualifier any_declaration            { state.extern_flag = false; }
| extern_c '{' statement_list '}'             { state.extern_flag_C = false; }
| extern_c any_declaration                    { state.extern_flag_C = false; }
;

any_declaration:
  function_declaration
| declaration
;

extern_c:
   EXTERN CONSTANT { state.extern_flag_C = true; }
;

extern_qualifier:
   EXTERN          { state.extern_flag = true; }
;

/***********************Function definitions ***/

function_definition:
  function_front block
  { }
| ctor_dtor
{}
| explicit_mod ctor_dtor
{}
| conversion_operator block
{}
;

ctor_dtor:
   construct_destruct arg_list poss_class_init_list block
;

/* w/ constructors and init lists, we construct the function first
   (before the init list) and then initialize the function
 */
poss_class_init_list: /*EMPTY*/
 | ':'  
   {   state.init_block(CONSTRUCTOR_BLOCK);  }
   class_init_list 
   {  check_error(); state.in_method = true; }
;

/*add 1.2.7 Fake syntax __init_list__ used to fool parser when grabbing init list + body */
class_init_list:
   FAKE_INIT_LIST
|   class_init_item
|  class_init_item COMMA class_init_list
;

class_init_item:
  typename_function function_arg_list
    { 
	 ((Class*)state.context().parent_context())
	    ->add_class_init_list(AsType($1).as_class()->entry(),$2);
	/* fix 1.2.3a Can crash UC if we don't catch errors in the init list */
	 if (check_error()) YYABORT;  
	}

| 
 IDEN function_arg_list
    { ((Class*)state.context().parent_context())
	      ->add_class_init_list($1,$2);
	 if (check_error()) YYABORT;
    }
;

/***********************General type expressions***/
type_expr:
 mod_type_name tname_expr
 { $$ = $2;  }
;

poss_const: /*EMPTY*/ { $$=0; }
 | CONST { $$=1; }
;

mod_type_name:
  type_name                      {dcl_set();}
| modifiers type_name            {dcl_set();}
| CONST type_name                {dcl_set(); tots().make_const();} 
| modifiers CONST type_name      {dcl_set(); tots().make_const();}
;

modifiers: 
   STATIC  {state.modifier = Static;  }
 | VIRTUAL {state.modifier = Virtual; }
 | STDCALL {state.modifier = Stdcall; }
 | API     {state.modifier = Api;     }
;

open_parens:  '(' {dcl_set(false);}
;

close_parens: ')' {dcl_reset();}
;

type_name: typename_class                 { ttpush($1);  }  
 | TYPEOF open_parens expr close_parens   { tpush(typeof_op($3)); }
 | integer                                { } 
 | UNSIGNED integer                       { tots().make_unsigned(); }
 | UNSIGNED                               { tpush(t_int); tots().make_unsigned(); }
 | FLOAT                                  { tpush(t_float);  }
 | DOUBLE                                 { tpush(t_double); }
 | BOOL                                   { tpush(t_bool); } 
 | VOID                                   { tpush(t_void); }
 | class_declaration                      { }
 | enum_stmt                              { }
;

poss_unsigned: /*empty*/  { $$=0; }
   | UNSIGNED            { $$=1; }
;
poss_int: /*empty*/       { $$=0; }
   | INT                 { $$=1; }
;

integer: INT         { tpush(t_int);   }
  | SHORT            { tpush(t_short); } 
  | LONG             { tpush(t_long);  } 
/*
  | SHORT  INT       { tpush(t_short); } 
  | LONG   INT       { tpush(t_long);  } 
*/
  | CHAR             { tpush(t_char);  }
;

pointer_expr:
'(' STAR token ')' { $$ = incr_ptr($3); }
;

array_expr:
   '[' {dcl_set(false);}
	 poss_int_const
	']' %prec ARRAY  { dcl_reset(); $$ = $3; }
;

tname_expr:  /*empty*/          { $$=ttots(); state.token_stack.push(""); }
  | token                       { $$=$1;   }
  | pointer_expr arg_list       { Type t = AsType($1); t.decr_pointer(); $$ = AsTType(state.signature_type(t));     }
  | '(' scope STAR token ')' end_scope  arg_list
                                 /* fix 1.2.3a Set the class type for this method ptr declaration first */
                                { state.class_dcl = AsType($2); $$ = AsTType(state.signature_type(AsType($4)));  } 
  | STAR poss_const tname_expr  { $$ = incr_ptr($3);         }
  | ADDR  tname_expr            { $$ = make_ref($2);         }
  | scope token end_scope       { $$ = $2;   state.class_dcl = AsType($1); }
  | scope conversion_operator
    end_scope                   { $$ = $2;   state.class_dcl = AsType($1); }
  | tname_expr array_expr       { $$ = make_array($1,$2); check_error();   }
  | pointer_expr array_expr     { $$ = make_array($1,$2); check_error();   } 
  | error { raise_error("Error in type expression"); YYABORT; }
;

/* conversion_operator requires special consideration! */
tname_exp2:  /*empty*/    { $$ = ttots(); }
  |  STAR tname_exp2      { $$ = incr_ptr($2); }
  |  ADDR tname_exp2      { $$ = make_ref($2); }
;

token: TOKEN { $$=ttots(); state.token_stack.push($1); }
;

/**** Function prototype lists *******/
begin_list: '(' { dcl_set(false); state.begin_args(); }
;

end_list: ')' { dcl_reset(); }
;

arg_list: 
 begin_list type_list end_list /*%prec FUN_CALL     */
;

type_list: /*empty*/
 | type_expr_init                 
 | type_expr_init ',' type_list  
 | THREEDOT
 { 
   state.add_to_arg_list(t_void,"...",0);
 }
;  

type_expr_init:
  type_expr poss_initialization
{  
  state.add_to_arg_list(AsType($1),state.token_stack.pop(),$2);
  tpop();
  dcl_reset();
}
;

/**** Declarative item ******/
init_tname_expr: tname_expr poss_initialization
  {
  string name = state.token_stack.pop();
  Type t = AsType($1);
  temp_context()->reserved_space(t.size());
  state.add_variable(t,name,$2,state.modifier);
  if (check_error()) YYABORT;
  } 
 ;

tname_expr_list: /*empty*/
 | init_tname_expr
 | init_tname_expr ',' tname_expr_list 
;

poss_int_const: /*empty*/ { $$=NULL; }
 | expr {$$=$1;}


/* Initializations occur in a declarative context, so it's
 * important that they reset/set this state properly 
 */
init_assign: ASSIGN { dcl_set(false); }
;

poss_initialization: /*empty*/               { $$=NULL; }
  | init_assign 
   expr
  { dcl_reset(); $$=$2; }

  | init_list
  { $$=expr_list_op($1,true); }

  | init_assign '{'    { enter_arglist();}
     brace_list '}'    
  {leave_arglist(); dcl_reset(); $$=expr_list_op($4,false); } 

  | ':' CONSTANT
  { $$ = new Expr(ECONST,t_void,$2,NULL); }

;

/* ----array initialization------ */


brace_expr: '{'  brace_list '}' { $$ = $2; }
;

brace_item: _expr { $$ = $1; }
  | brace_expr    { $$ = expr_list_op($1,false); } 
;

brace_list: brace_item        { $$ = new ExprList; $$->push_back($1);  }
  | brace_list ',' brace_item { $$ = $1; $$->push_back($3); } 
;

/* brace_list: expr_list { $$=$1; }; simplified */


/****** CLASSES AND STRUCTS *******/
/* *change 1.1.4 this is now defined as a part of a declaration, as C intended */

access_modifier:
 PUBLIC      {$$=Public;}
 | PRIVATE   {$$=Private;} 
 | PROTECTED {$$=Protected;}
;

poss_access_modifier: /*EMPTY*/ { $$=Default; } | access_modifier
;

class_or_struct: CLASS | STRUCT | UNION
;

class_or_struct_ex: CLASS_Y  { $$ = CLASS; }
| STRUCT_Y                   { $$ = STRUCT; } 
| UNION_Y                    { $$ = UNION;  }
;

poss_derived: /*EMPTY*/ { $$=NotDerived; }
 | ':' poss_access_modifier typename_class 
 { $$=$2; state.class_dcl = AsType($3); }
;

token_or_typename:
    TOKEN                   { $$ = $1;               }
  | TYPENAME                { $$ = $1->name.c_str(); }
;

class_name: /*empty*/       { $$ = "";               } 
  | token_or_typename       { $$ = $1;               } 
  | template_class          
   { PClass pc = AsType($1).as_class();
     $$ = pc->name().c_str();
    }
  | TEMPLATE_NAME           { $$ = $1->name.c_str(); }
;    

class_id: { dcl_set();} class_name { dcl_reset(); $$ = $2;  }
;

/* may well be useful to restrict the possible statements here */

class_declaration:
    class_or_struct class_id poss_derived '{'
      { 
	   tpush(state.add_class($1,$2,$3,state.class_dcl));
       if (check_error()) YYABORT;
	   state.init_block(CLASS_BLOCK); IEF_set();
       typedef_stack.push(state.in_typedef);
       state.in_typedef = false;
      }

	statement_list '}' 

     {
	   state.finalize_block(); IEF_reset();
  	   state.in_typedef = typedef_stack.pop(); 
	 } 

  | class_or_struct_ex class_id  
     { tpush(state.add_class($1,$2,ForwardClass,t_void)); }	 
;

access_specifier: access_modifier ':'
  { state.set_access_mode($1); }
;	

friend_declaration:
   FRIEND { state.in_friend_dcl = true; } function_declaration 
|  FRIEND CLASS class_name ';'{ state.add_friend_class($2); } 
;

/*** Enumerations **********/
/*** Note that getting the token value is often unreliable! ***/
/* *change 1.1.4 this is now defined as a part of a declaration, as C intended;
   Each enum constant is now added immediately, so that they may themselves be
   used to initialize further constants.
 */
enum_stmt:
  ENUM { dcl_set(); } poss_tag  
   {
    tpush(state.do_enum(tag_name));
    if (check_error()) YYABORT;
   }
  poss_enum_list
  { dcl_reset(); }
;

poss_enum_list: /*empty*/
| '{' enum_list '}'
;

poss_tag: /*EMPTY*/ {$$=NULL; tag_name="";} 
| TOKEN {$$=$1; tag_name=$1;}
| TYPENAME {$$=""; tag_name=$1->name.c_str();}
;

enum_item:
 TOKEN poss_initialization
 { 
  state.add_enum(tots(),$1,$2);
 }
;

enum_list: /*EMPTY*/
 | enum_item 
 | enum_list ',' enum_item
 ;

/* namespaces and using directives *****/
/*REF:NAMESPACE*/

namespace_declaration:
  NAMESPACE { dcl_set(); } poss_class_name 
  { dcl_reset(); 
    state.add_namespace($3 ? $3 : "");
    state.in_class = true;
    if (check_error()) YYABORT;
   }
  block 
;

poss_class_name: /*empty*/  { $$ = NULL; }
 | class_name { $$ = $1;  }
;

using_directive:
  USING NAMESPACE TYPENAME ';'
  /* note: cd also express this as $3->type.as_class()!! */
   { 
   	if ((Namespace *)$3->data != &state.context()) 
      state.context().inject_namespace((Namespace *)$3->data);
    else raise_error("Cannot inject a namespace into itself");
    if (check_error()) YYABORT;
   }
;

using_declaration:
  USING scoped_name ';'  
   {  state.context().inject_entry($2);  }
| USING scope TEMPLATE_NAME end_scope
   { state.context().inject_entry($3); }
| USING typename_class ';'
   { state.context().inject_entry(last_type_entry);  }
;

/* goto statements **********************/
goto_stmt:
  GOTO { dcl_set(true); } TOKEN { dcl_reset(); } ';'
  { do_goto($3); }
;

goto_label:
   TOKEN ':' { goto_label_new($1); }
|  IDEN  ':' { goto_label_existing($1); }
;


/* expressions *************************/

scope:
  typename_expr BINARY_SCOPE 
  { state.begin_scope(AsType($1)); $$ = $1; }
/*
| 
  TYPEOF open_parens expr close_parens BINARY_SCOPE
  { 
    Type t = typeof_op($3);
    state.begin_scope(t);
	$$ = AsTType(t);
  }
*/
;

global_scope:
  BINARY_SCOPE %prec UNARY_SCOPE
  { state.begin_scope(&global()); }

end_scope:
  { state.end_scope(); }
;

assign_op: 
 MUL_A | DIV_A | MOD_A | ADD_A | MINUS_A
 | SHL_A | SHR_A | BAND_A | BOR_A | XOR_A
;

scoped_name: IDEN  
| scope scoped_name end_scope
  { $$ = $2; }
| global_scope IDEN end_scope
  { $$ = $2; }
;

expr: _expr
 {   if (check_error()) YYABORT;   $$ = $1; }
;  

poss_size: /*EMPTY*/ { $$ = NULL; }
 | '[' _expr ']' %prec ARRAY { $$ = $2; }
;                

type_expression: type_expr 
{ dcl_reset(); $$=$1; state.token_stack.pop(); tpop(); }
;

typecast_type: STATIC_CAST | CONST_CAST | DYNAMIC_CAST | REINTERPRET_CAST
;

type_bracket: LESS_THAN type_expression GREATER 
                  { $$=$2; }
;


_expr: 
 CONSTANT       { $$ =  entry_op($1); }
 | scoped_name         { $$ =  entry_op($1); } 
/* | scoped_name template_type_list { $$ = entry_op($1); } */
 | '(' _expr ')' { $$ =  $2; }
/* binary arith operators */
| _expr STAR _expr    { $$=arith_op(STAR,$1,$3); }
| _expr DIVIDE _expr    { $$=arith_op(DIVIDE,$1,$3); }
| _expr MODULO _expr    { $$=arith_op(MODULO,$1,$3); }
| _expr PLUS _expr   { $$=arith_op(PLUS,$1,$3); }
| _expr MINUS _expr  { $$=arith_op(MINUS,$1,$3); }
| _expr BIN_AND _expr { $$=arith_op(BIN_AND,$1,$3); }
| _expr BIN_OR _expr { $$=arith_op(BIN_OR,$1,$3); }
/* unary arith */
| PLUS _expr %prec UPLUS     { $$=$2; }
| MINUS _expr %prec UMINUS   { $$=unary_op(UMINUS,$2); }
/* relational */
| LOG_NOT _expr         { $$=relational_op(LOG_NOT,$2,NULL); }
| _expr LESS_THAN _expr   { $$=relational_op(LESS_THAN,$1,$3); }
| _expr GREATER _expr   { $$=relational_op(GREATER,$1,$3); }
| _expr LEQ _expr   { $$=relational_op(LEQ,$1,$3); }
| _expr GEQ _expr   { $$=relational_op(GEQ,$1,$3); }
| _expr EQUAL _expr   { $$=equal_op($1,$3); }
| _expr NOT_EQUAL _expr   { $$=relational_op(NOT_EQUAL,$1,$3); }
| _expr LOG_AND _expr   { $$=relational_op(LOG_AND,$1,$3); }
| _expr LOG_OR _expr   { $$=relational_op(LOG_OR,$1,$3); }
/* assignment */
| _expr ASSIGN _expr { $$=assign_op($1,$3); }
| _expr assign_op _expr { $$=compound_assign_op($2,$1,$3); }
/* inc & dec */
| _expr INCR        { $$=inc_dec_op(INCR,$1,true); }
| INCR _expr        { $$=inc_dec_op(INCR,$2,false); }
| _expr DECR        { $$=inc_dec_op(DECR,$1,true); }
| DECR _expr        { $$=inc_dec_op(DECR,$2,false); }
/* misc. */
/* *fix 0.9.2 sizeof() now works properly w/ arrays */
/* *fix 1.2.3 sizeof(int)*N was broken; we now require the parens */
| SIZEOF '(' _expr ')'     { 
    if ($3->is_entry()) $$ = sizeof_op(size_of_entry($3->entry()));
	              else  $$ = sizeof_op($3->type().size());
 }
| SIZEOF '(' type_expression ')' { $$=sizeof_op(AsType($3).size());  }
| _expr ARITH_IF _expr ':' _expr { $$=arith_if_op($1,$3,$5); }
| _expr COMMA _expr  { $$=bin_op(COMMA,$1,$3); }
| NEW type_name poss_size 
  { $$ = new_op(tpop(),$3,NULL);  }

| NEW TYPENAME_FUNCTION  { tpush($2->type); } function_arg_list /*typename_function*/
  { $$ = new_op(tpop(),NULL,$4);  }

| NEW type_name function_arg_list
  { $$ = new_op(tpop(),NULL,$3);  }
| DELETE poss_array _expr            { $$ = delete_op($3,$2==1); }
| STAR _expr  %prec DEREF  { $$ = deref_op($2); }
| ADDR _expr               { $$ = addr_op($2); }
/** typecasts *****/
| typecast_type type_bracket '(' _expr ')' 
  { $$ = typecast_op($1,AsType($2),$4); }

| TYPENAME_FUNCTION  function_arg_list  /*typename_function*/
  { $$ = function_cast_op($1->type /*AsType($1)*/,$2); }

| '(' type_expression ')' _expr  %prec TYPECAST
  { $$ = typecast_op(REINTERPRET_CAST,AsType($2),$4); }

/* bitwise */
| BIN_NOT _expr        { $$=unary_op(BIN_NOT,$2); }
| _expr LSHIFT _expr    { $$=bin_op(LSHIFT,$1,$3); }
| _expr RSHIFT _expr    { $$=bin_op(RSHIFT,$1,$3); }
| _expr ADDR _expr %prec BIN_AND  { $$=bin_op(BIN_AND,$1,$3); }
| _expr BIN_XOR _expr  { $$=bin_op(BIN_XOR,$1,$3); }
| _expr ADDR _expr %prec BIN_OR { $$=bin_op(BIN_OR,$1,$3); }

| _expr '[' _expr ']' %prec ARRAY
  { $$=array_op($1, $3); }
  
| _expr function_arg_list { $$=function_op($1,$2); }
  
| _expr DOT 
  {dcl_set(); } 
  TOKEN
  { dcl_reset(); $$=selection_op($1,$4,false);  }
  
| _expr ARROW
  {dcl_set();}
  TOKEN
  { dcl_reset(); $$=selection_op($1,$4,true);  } 

/* *add 1.2.9a We forgot the .* operator! */
| _expr MEMBER_ARROW
  {dcl_set();}
  TOKEN
  { dcl_reset(); $$=selection_op($1,$4,true,true);  }

| error { raise_error("error in expression"); YYABORT; } 
;

poss_array: /*EMPTY*/ { $$ = 0; }
| '[' ']'  { $$ = 1; }
;

poss_function_arg_list: /*EMPTY*/ { $$ = NULL; }
| function_arg_list { $$ = $1; }
;

function_arg_list:  '(' ')'     { $$ = new ExprList; }
|   init_list                   { $$ = $1; }
;

init_list: 
  begin_list
  expr_list
  end_list %prec FUN_CALL            { $$ = $2; } 
;

expr_list: 
   _expr                { $$ = new ExprList; $$->push_back($1);  }
 | expr_list ',' _expr  { $$ = $1; $$->push_back($3); }  
 | error               { raise_error("Error in arg list"); YYABORT; }
 ;

/*** Control statements **************************/
condition:
   '(' expr ')' { $$ = $2; }
/* |  '(' declaration ')' { $$ */
;

poss_expr: /*EMPTY*/ { $$ = NULL; }
 | expr { $$ = $1; }
;

controlled_statement:
  {IEF_set();}
  statement
  {IEF_reset(); check_temp_context(); }
;

if_front:
  IF condition { do_loop_start($2,false); }
;

if_stmt: 
  if_front controlled_statement { do_loop_end(false); }
;

if_else_stmt:
  if_front controlled_statement   
  ELSE                             { do_loop_end(true,true); } 
  controlled_statement             { do_loop_end(false);     }
;

while_stmt:
  WHILE                  { label_here(); state.in_loop = true; } 
  condition              { do_loop_start($3,true); }
  controlled_statement   { do_loop_end(true); state.in_loop = false;}
;

/* fix 1.2.3 break was broken in do-while statements */
do_stmt:
  DO                           { label_here(); push_label_stack(); state.in_loop = true;}
   controlled_statement      
  WHILE condition              { do_do_end($5); state.in_loop = false}
;

for_init: ';'
|  expr ';'    { code().compile($1,DROP_VALUE); }
|  declaration
;

for_stmt:
  FOR  '(' { state.init_block(PLAIN_BLOCK); }   /*correct scoping for any dcls*/
    for_init                   { label_here();	state.in_loop = true; } 
    poss_expr ';'
    poss_expr ')'              { do_loop_start($6,true); }
  controlled_statement         { do_for_end($8);    state.in_loop = false;
								 state.finalize_block();
							   }
;

switch_stmt:
  SWITCH '(' expr ')' { do_switch($3); } block
;

return_stmt: RETURN poss_expr ';'
  { if (!do_return ($2)) { check_error(); YYABORT; }  }
;

case_label:
  CASE expr ':'
  { do_case ($2);   }
| DEFAULT ':'
  { do_case(NULL);  } 
;

break_stmt:  BREAK ';'
 { if (!do_break_continue(BREAK)) { check_error(); YYABORT; } }
;

continue_stmt: CONTINUE ';'
 { if (!do_break_continue(CONTINUE)) { check_error(); YYABORT; } }
;

/*** Exception Handling ****/

try_catch_stmt:
  try_block catch_block_list
  { do_end_try_catch_block(true); }
;

try_block:
  TRY
  { state.init_block(PLAIN_BLOCK); IEF_set(); 
	do_start_try_block();  }
  except_block
;

except_block:
  '{' statement_list '}'

   { state.finalize_block(); IEF_reset();
     do_end_try_catch_block(false);   } 
;

catch_block:
  CATCH arg_list
  { state.init_block(PLAIN_BLOCK); IEF_set(); 
    do_start_catch_block(); if (check_error()) YYABORT; }
  except_block
;

catch_block_list:
  catch_block
| catch_block catch_block_list
;

throw_stmt:
  THROW poss_expr ';'
  { do_throw($2); }
;

/************* Templates *****************/

typename_function:
  TYPENAME_FUNCTION { $$ = AsTType($1->type); }
| template_class    { $$ = $1; }
;

/***** Declaration context types ****/
typename_class:
  TYPENAME       { last_type_entry = $1; $$ = AsTType($1->type); }
| template_class { $$ = $1; }
| scope typename_class end_scope { $$ = $2; }
;

/***** Expression context types ***/
typename_expr:
  TYPENAME_FUNCTION { $$ = AsTType($1->type); }
| template_expr    { $$ = $1; } 
| scope typename_expr end_scope { $$ = $2; }
;

template_class:
TEMPLATE_NAME template_type_list
   { $$ = AsTType(Template::get_template_type($1,$2)); }
;

template_expr:
TEMPLATE_NAME_EXPR template_type_list
   { $$ = AsTType(Template::get_template_type($1,$2)); }
;


template_header:
 TEMPLATE template_type_list
 { Template::do_template_header($2); }
;

template_function_declaration:
 template_header
   function_front 
  { Template::do_function_template(); 
  	if (yychar != YYEMPTY) yyclearin;
  }
;

template_class_declaration:
  template_class_header  template_class_name
   {
    dcl_reset();    
    Template::do_class_template($1,$2,yychar,NULL);
	if (yychar != YYEMPTY) yyclearin;
   }
| template_class_header  template_class_name template_type_list
   {
    dcl_reset();        
    Template::do_class_template($1,$2,yychar,$3);
	if (yychar != YYEMPTY) yyclearin;
   }
;

struct_or_class_x:
  STRUCT_X | CLASS_X
;

template_class_header:
  template_header struct_or_class_x
   { dcl_set(); $$ = $2; }
;

template_class_name: TOKEN           { $$ = $1;                     }
  |         TEMPLATE_NAME            { $$ = $1->name.c_str();       }
;    

/* note: this will work as long as they're just plain CONSTANTs!*/
begin_templ_list:  LESS_THAN { dcl_set(true); state.begin_templ_list(); }
;
end_templ_list:    GREATER   { dcl_reset();   state.end_templ_list();   }
;

template_type_list: 
  begin_templ_list end_templ_list                 { $$ = new TypeList; }
| begin_templ_list templ_item_list end_templ_list { $$ = $2; }
;

/* rather like expr_list! */
templ_item_list:  
  templ_item                       { $$ = new TypeList; $$->push_back(AsType($1)); }
| templ_item_list ',' templ_item   { $$ = $1; $$->push_back(AsType($3));   } 
;

class_item:
  CLASS TOKEN { $$ = $2; }
| CLASS TYPENAME { $$ = $2->name.c_str(); }
;

class_parm: 
 class_item    {
   string c = $1;
   $$ = AsTType(Template::dummy(t_null,c));
  }
;

/* This is a bit cheeky - handles both kinds of template type lists! */
templ_item:
  type_expr        {
                     $$ = AsTType(Template::dummy(AsType($1),state.token_stack.pop()));
	                 dcl_reset();  tpop();
	               }
| CONSTANT         { $$ = AsTType(Template::dummy($1)); }
| class_parm       { $$ = $1; }
;

     
%%  



