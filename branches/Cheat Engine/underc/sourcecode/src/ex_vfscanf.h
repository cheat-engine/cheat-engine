/* A simplified vfscanf that uses a custom text grabber
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/
#ifndef __EX_VFSCANF
#define __EX_VFSCANF
#include <stdarg.h>
#include <stdio.h>

typedef char *( *BUFFGETTER )(char *);

int ex_vfscanf(BUFFGETTER getter, char *fmt, va_list va);

int con_fprintf(FILE *out, char *fmt, ...);
char *con_fgets(char *buff, int sz, FILE *in);
int con_fscanf(FILE *in, char *fmt,...);

int str_vprintf(char *fmt,va_list args);
char *skip_ws(char *s);
char *str_getter(char *p);
void str_gets(char *buff);
int str_eof(int);
void *str_cpy(void *_ptr, int mode);

extern void *_str_out, *_str_in;

#endif
