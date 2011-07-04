typedef union{
  int    val; 
  long   ctype;
  char*  str;
  Entry* entry;
  Expr*  expression;
  ExprList *elist;
  Class *classptr;
  TypeList *typelist;
} YYSTYPE;
#define	TOKEN	257
#define	IDEN	258
#define	CONSTANT	259
#define	TYPENAME	260
#define	TYPENAME_FUNCTION	261
#define	TEMPLATE_NAME	262
#define	TEMPLATE_NAME_EXPR	263
#define	THIS_CLASSNAME	264
#define	FLOAT	265
#define	DOUBLE	266
#define	UNSIGNED	267
#define	INT	268
#define	SHORT	269
#define	LONG	270
#define	CHAR	271
#define	VOID	272
#define	BOOL	273
#define	TYPEDEF	274
#define	CLASS	275
#define	STRUCT	276
#define	ENUM	277
#define	OPERATOR	278
#define	STATIC_CAST	279
#define	CONST_CAST	280
#define	DYNAMIC_CAST	281
#define	REINTERPRET_CAST	282
#define	STRUCT_X	283
#define	CLASS_X	284
#define	STRUCT_Y	285
#define	CLASS_Y	286
#define	IF	287
#define	ELSE	288
#define	WHILE	289
#define	DO	290
#define	FOR	291
#define	SWITCH	292
#define	CASE	293
#define	RETURN	294
#define	CONTINUE	295
#define	BREAK	296
#define	DEFAULT	297
#define	NAMESPACE	298
#define	USING	299
#define	TRY	300
#define	CATCH	301
#define	THROW	302
#define	TEMPLATE	303
#define	EXTERN	304
#define	THREEDOT	305
#define	TYPEOF	306
#define	EXPLICIT	307
#define	FRIEND	308
#define	LAMBDA	309
#define	CONST	310
#define	STATIC	311
#define	STDCALL	312
#define	API	313
#define	VIRTUAL	314
#define	PRIVATE	315
#define	PROTECTED	316
#define	PUBLIC	317
#define	COMMA	318
#define	ASSIGN	319
#define	MUL_A	320
#define	DIV_A	321
#define	MOD_A	322
#define	ADD_A	323
#define	MINUS_A	324
#define	SHL_A	325
#define	SHR_A	326
#define	BAND_A	327
#define	BOR_A	328
#define	XOR_A	329
#define	ARITH_IF	330
#define	LOG_OR	331
#define	LOG_AND	332
#define	BIN_OR	333
#define	BIN_XOR	334
#define	BIN_AND	335
#define	EQUAL	336
#define	NOT_EQUAL	337
#define	LESS_THAN	338
#define	LEQ	339
#define	GREATER	340
#define	GEQ	341
#define	LSHIFT	342
#define	RSHIFT	343
#define	PLUS	344
#define	MINUS	345
#define	STAR	346
#define	DIVIDE	347
#define	MODULO	348
#define	MEMBER_ARROW	349
#define	MEMBER_DOT	350
#define	NEW	351
#define	DELETE	352
#define	TYPECAST	353
#define	DEREF	354
#define	ADDR	355
#define	UPLUS	356
#define	UMINUS	357
#define	LOG_NOT	358
#define	BIN_NOT	359
#define	INCR	360
#define	DECR	361
#define	SIZEOF	362
#define	TYPE_CONSTRUCT	363
#define	FUN_CALL	364
#define	ARRAY	365
#define	ARROW	366
#define	DOT	367
#define	BINARY_SCOPE	368
#define	UNARY_SCOPE	369


extern YYSTYPE yylval;
