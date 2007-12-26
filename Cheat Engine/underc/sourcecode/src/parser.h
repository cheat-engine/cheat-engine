typedef union {
  int val;      /* opcodes and types */
  Entry *tptr;  /* identifiers and constants */
} YYSTYPE;
#define LBRACE  258
#define RBRACE  259
#define LPAREN  260
#define RPAREN  261
#define CLASS   262
#define PROC    263
#define VIRTUAL 264
#define STATIC  265
#define ARG 266
#define LOAD    267
#define LLOCAL  268
#define STRING  269
#define INTEGER 270
#define NUMBER  271
#define IDEN    272
#define USERTYPE    273
#define OPCODE  274
#define JUMP    275
#define DOT 276
#define SCOPE   277
#define COLON   278
#define COMMA   279
#define RBOX    280
#define LBOX    281
#define DELIM   282
#define TYPENAME    283


extern YYSTYPE yylval;

