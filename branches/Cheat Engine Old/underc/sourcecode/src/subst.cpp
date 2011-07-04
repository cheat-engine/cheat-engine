// subst.cpp
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "classlib.h"
#include "tokens.h"

void insert(char *str, char *ins)
{
 int sz = strlen(ins);
 memmove(str+sz,str,strlen(str)+1);
 memcpy(str,ins,sz);
}

/// a useful function for extracting a substring
char *copy_str(char *tok, char *start, char *end)
{
 int sz = (long)end - (long)start;
 strncpy(tok,start,sz);
 tok[sz] = '\0';
 return tok;
}

long convert_int(char *buff, int base)
{
// *fix 1.2.1 Use 'strtoul' instead of 'strtol' to input ints larger than MAXINT
 char endptr[10];
 long val = strtoul(buff,(char **)&endptr,base);
 return val;
}

char *copy_chars(char* q, char* p)
{
    while (*q = *p++) q++;
    return q;
}

char *copy_token(char *p, char *tok)
{
     char *ptok = tok;
     int i = 0;
     while (iscsym(*p) && i < MAX_IDEN_SIZE)
         *ptok++ = *p++;
     *ptok = '\0';
     return p;
}

int index_into(char **table, char *sym)
{
    int i;
    for (i = 0; table[i] != NULL; i++)
       if (strcmp(sym,table[i]) == 0) return i;    
    return -1;
}

// *fix 1.2.2b (Eric) This now works with empty strings
void copy_quote(char*& q, char*& p)
{
    // Copy leading quote, inside of string, trailing quote,
    do { *q++ = *p++; }
    while (*p && (*p != '"' || p[-1] == '\\'));
    if (*p) *q++ = *p++;
}

const char ARG_MARKER = 0x7f;

char *massage_subst_string(char *buff, char **args, char *str)
{
   char *p = str, *q = buff;
   int i;
   char token[MAX_IDEN_SIZE];

   while (*p) {
       // *fix 1.2.2 Argument may begin with '_' as well as a letter!
      while (*p && !iscsymf(*p)) {
           // *fix 1.2.2 (Eric)..quoted strings are copied verbatim
          if (*p == '\"') {
              copy_quote(q,p);
              break;            
          }          
          if (p[0] == '#' && p[1] == '#') {
            // *fix 1.2.2 (Eric) Skip whitespace before and after the ##
              while (q > buff && isspace(q[-1])) --q;
              // *change 1.2.2 keep token-pasting operator...
              *q++ = *p++;
              *q++ = *p++;
              while (isspace(*p)) ++p;
          }
          else *q++ = *p++;
      } 
      if (*p == 0) break;

      //...pick up the identifier, and check it against the map
      p = copy_token(p,token);
      i = index_into(args,token);
      i++;

      //...if it is indeed a formal argument, then encode as the byte index,
      //...otherwise just copy to output buffer.
      // *change 1.2.2b (Eric) encode as unique macro-arg marker followed by byte index
      if (i > 0) { *q++ = ARG_MARKER; *q++ = (char)i; } 
      else
        q = copy_chars(q,token);
   }
   *q = '\0';
   return buff;
}

void poss_macro_expand(TokenStream& tok, char*&q, char*& p)
{
  char temp_buff[TT_BUFFSIZE];
  char token[MAX_IDEN_SIZE];

  p = copy_token(p,token);
  // is it a macro?  Otherwise just copy out....                
  if (tok.macro_attempt_process(p,temp_buff,token))
     q = copy_chars(q,temp_buff);
  else
     q = copy_chars(q,token);
}

// *fix 1.2.2b (Eric) quote \n\r\t\\\" when they are being stringified
static char* stringify(char* q, char* p)
{
 bool in_quote = false;
 do {
     if (p[0] == '\"' || (in_quote && p[0] == '\\' && strchr("nrt\\\"",p[1]))) *q++ = '\\';
     if (p[0] == '\\' && in_quote && strchr("nrt",p[1])) *q++ = *p++;
     if (p[0] == '\"') in_quote = !in_quote;
 } while (*q++ = *p++);
 return q-1;
}


void macro_substitute(TokenStream& tok,char *str, char *buff)
{
 char *p = str, *q = buff;
 while (*p) {
    // found a token
    if (iscsymf(*p)) poss_macro_expand(tok,q,p); 
    else if (*p == '\"') copy_quote(q,p);
    else *q++ = *p++;
 }
 *q = '\0';
}

void substitute_args(TokenStream& tok, char **args, int n)
{
  char temp_buff[TT_BUFFSIZE];
  for(int i = 0; i < n; i++)
  {
    macro_substitute(tok,args[i],temp_buff);
    args[i] = strdup(temp_buff);
  }
}

// *change 1.2.2 substitute() now properly recursively expands any arguments
// before further preprocessing occurs.  This sorts out Eric's subtle but
// important problem with substitution order.  There seems no obvious way to
// check for buffer overrun, so caveat emptor.
 
char *substitute(TokenStream& tok, char *buff, char **actual_args, char *subst)
 //----------------------------------------------------------------------------
 {
   char temp_buff[TT_BUFFSIZE],args_buff[STRSIZE];
   char *p = subst, *q = temp_buff;
   while (*p) {
      if (*p == ARG_MARKER) { // ie. 1..NARGS
        char *arg = actual_args[p[1]-1]; // get following (one-based) argument
        // stringize or token-pasting operator suppresses arg expansion
        // *fix 1.2.2a Token-pasting can operate the _other way_ of course!
        if (p > subst && (p[-1]=='#' || p[2]=='#')) { 
           bool stringize = p[-1] == '#' && (p-1 == subst || p[-2] != '#');           
           // the last char(s) were '#' - discard them!
           if (stringize) { q--; *q++ = '\"'; } 
           else if (p[-1]=='#')  q -= 2;
           // copy the substitution, without expanding the argument
           q = (stringize? stringify : copy_chars)(q,arg);
           if (stringize) *q++ = '\"';
           p += 2;  // skip the special marker _and_ the arg index
           // ignore '##' afterwards, unless it's followed by a parameter....
           if (p[0] == '#' && p[1] == '#' && p[2] != ARG_MARKER) p+= 2;
        }
        else { // must expand any arguments first!
         macro_substitute(tok,arg,args_buff);
         q = copy_chars(q,args_buff);
         p += 2;
        }  
      }
      else *q++ = *p++;
   }
   *q = '\0';
   macro_substitute(tok,temp_buff,buff);
   return buff;
 }

