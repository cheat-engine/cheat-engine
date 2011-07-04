/* A simplified vfscanf that uses a custom text grabber
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/

#include "classlib.h"
#include "ex_vfscanf.h"

#include <string.h>
#include <ctype.h>

struct UCString {
  char *m_str;
  int m_sz;
};

static UCString in_buff;
static string out_buff;
void *_str_in = (void *)&in_buff, *_str_out = (void *)&out_buff;

void str_gets(char *buff)
{
 char *ps = in_buff.m_str, *op = buff;
 if (! *ps) ps = NULL;
 else {
  while (*ps && *ps != '\n') *op++ = *ps++;
 }
 *op++ = '\0';
 in_buff.m_str = ps;
}

int str_eof(int)
{
 return in_buff.m_str == NULL;
}

#ifndef _WCON
// NOTE(10);
int con_fprintf(FILE *out, char *fmt, ...)
{
 va_list ap;
 int ich;
 va_start(ap,fmt);
 if (out==_str_out) ich = str_vprintf(fmt,ap); // NOTE(10) wuz ap
 else ich = vfprintf(out,fmt,ap);
 va_end(ap);
 return ich;
}

char *con_fgets(char *buff, int sz, FILE *in)
{
 if(in==_str_in) str_gets(buff); 
 else fgets(buff,sz,in);
 return buff;
}

int con_fscanf(FILE *in, char *fmt,...)
// ONLY used to do i/o on strings!
{
  va_list ap;
  int ret;
  va_start(ap,fmt);
  ret = ex_vfscanf(str_getter,fmt,ap);
  va_end(ap);
  return ret;
}
#endif

//NOTE(10)

void *_new_ex(int sz);  // in directcall.cpp

void *str_cpy(void *_ptr, int mode)
{
  UCString *ptr = (UCString *)_ptr;
  switch(mode) {
  case 1: // copy user string into our input buffer 
	in_buff = *ptr;
	break;
  case 2: // copy our output buffer to user string
// *fix 1.1.0 Use the official allocator, since this will be disposed of automatically...
    ptr->m_str = (char *)_new_ex(out_buff.length()+1);
	strcpy(ptr->m_str, out_buff.c_str());
	ptr->m_sz = out_buff.length();
    break;
  case 3: // copy user string to our output buffer
    out_buff = ptr->m_str;
	break;
  }
  return _ptr;
}

static char con_obuff[1024]; //*NOTE*

// NOTE(10)
int str_vprintf(char *fmt,va_list args)
{
 int ich = vsprintf(con_obuff,fmt,args);
 out_buff += con_obuff;
 return ich;
}

char *skip_ws(char *s) 
{
  while (*s != '\0' && isspace(*s)) s++;
  return s;
}

char *str_getter(char *p)
{
 if (p != NULL) in_buff.m_str = p;
 else {
   p = in_buff.m_str;
   if (p == NULL) return NULL;
   p = skip_ws(p);
   if (*p == '\0') p = NULL;
   in_buff.m_str = p;
 }
 return p;
}

int ex_vfscanf(BUFFGETTER getter, char *fmt, va_list va)
 {
 // A simplified vfscanf implementation,
 // Only recognizes %d, %u, %ld, %lu, %lg, %f, %lf and
 // whitespace. 
        int k=0;     
		char *s = NULL;
 	
        while (1)
        {
           if (fmt[0]=='%')  {
              int n;
              s = getter(NULL);
              if (s == NULL) return 0;
              if (fmt[1]=='d')            {
                k+=sscanf(s,"%d%n",va_arg(va,int*),&n); 
                s+=n;
                fmt+=2;
              } else
              if (fmt[1]=='s')            {
                k+=sscanf(s,"%s%n",va_arg(va,char*),&n); 
                s+=n;
                fmt+=2;
              }
              else if (fmt[1]=='u')    {
                 k+=sscanf(s,"%u%n",va_arg(va,int*),&n);  
                 s+=n;
                 fmt+=2;
              }
              else if (fmt[1]=='l' && fmt[2]=='d')    {
              k+=sscanf(s,"%ld%n",va_arg(va,long*),&n);
              s+=n;
              fmt+=3;
            }
            else if (fmt[1]=='l' && fmt[2]=='u')    {
              k+=sscanf(s,"%lu%n",va_arg(va,long*),&n);
              s+=n;
              fmt+=3;
            }
            else if (fmt[1]=='l' && (fmt[2]=='g' || fmt[2]=='f'))   {
			  double *ptr = va_arg(va,double*);
              k+=sscanf(s,"%lg%n",ptr,&n);
              s+=n;
              fmt+=3;
            }
            else if (fmt[1]=='f') {
              k+=sscanf(s,"%f%n",va_arg(va,float*),&n);
              s+=n;
              fmt+=2;
            }
            else            {
              fprintf(stderr,"scanf: unsupported format '%s'\n",fmt);
              return k;
            };
           }
           else if (isspace(*fmt)) {
		     //s = skip_and_grab(in,s);
             fmt++; 
           }
           else if (*fmt=='\0') { 
             return k;
           }  else   { 
            fprintf(stderr,"scanf: unexpected char in format '%s'\n",fmt);
            return k;
          }
		  getter(s);
        }
   }
