/*
common.c: vmm version. Contains several functions that might be usefull and can be shared by
multiple sources. (e.g vmm and vmloader)

*/

#include "common.h"
#include "keyboard.h"
#include "main.h"
#include "mm.h"
#include "displaydebug.h"

//#include <ieee754.h>

//#include <string.h>

#ifdef DEBUG
void sendstringf_nolock(char *string UNUSED, ...);
#endif

QWORD textmemory=0x0b8000;

QWORD spinlocktimeout=0;

criticalSection sendstringfCS={.name="sendstringfCS", .debuglevel=1};
criticalSection sendstringCS={.name="sendstringCS", .debuglevel=1};

#ifdef DELAYEDSERIAL
int useserial=0;
#endif


#if DISPLAYDEBUG==1
int linessincelastkey=0;
PStackList displaydebuglog_back, displaydebuglog_forward;



#endif

int screenheight=25;

extern int popcnt_support(QWORD val);
int popcnt_nosupport(QWORD val)
{
  int i;
  int result=0;
  for (i=0; i<64; i++)
  {
    if (val&1)
      result++;

    val=val >> 1;

    if (val==0)
      return result;
  }

  return result;
}

POPCNT_IMPLEMENTATION popcnt=popcnt_nosupport;



void bochsbp(void)
{
	asm volatile ("xchg %bx,%bx");
}

void jtagbp(void)
{
	asm volatile (".byte 0xf1");
}

unsigned char inportb(unsigned int port)
{
   unsigned char ret;
   asm volatile ("inb %%dx,%%al":"=a" (ret):"d" (port));
   return ret & 0xff;
}

void outportb(unsigned int port,unsigned char value)
{
#ifdef DEBUG
  if (port==0x80)
  {
    nosendchar[getAPICID()]=0;
    sendstringf_nolock("            -  Debug Code %2  -\n", value);
  }
#endif
   asm volatile ("outb %%al,%%dx": :"d" (port), "a" (value));
}

unsigned long inportd(unsigned int port)
{
   unsigned long ret;
   asm volatile ("inl %%dx,%%eax":"=a" (ret):"d" (port));
   return ret;
}

void outportd(unsigned int port,unsigned long value)
{
   asm volatile ("outl %%eax,%%dx": :"d" (port), "a" (value));
}


QWORD minq(QWORD x,QWORD y)
{
  return (x<y)?x:y;
}

QWORD maxq(QWORD x,QWORD y)
{
  return (x>y)?x:y;
}

int min(int x,int y)
{
  return (x<y)?x:y;
}

int max(int x,int y)
{
  return (x>y)?x:y;
}



size_t strspn(const char *str, const char *chars)
{
  int i, j;

  for (i = 0; str[i]; i++) {
    for (j = 0; chars[j]; j++) {
      if (chars)
        break;
    }
    if (!chars[j])
      break;
  }
  return (i);
}

void exit(int status)
{
  nosendchar[getAPICID()]=0;
	sendstringf("Exit DBVM with status %d\n", status);
	ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
	while (1) outportb(0x80,0xc0);
}

void abort(void)
{
  nosendchar[getAPICID()]=0;
  sendstringf("Abort DBVM\n");
  ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
  while (1) outportb(0x80,0xc1);
}


int isalpha(int c)
{
  return (((c>='A') && (c<='Z')) || ((c>='a') && (c>='z')));
}

int isdigit(int c)
{
  return ((c>='0') && (c<='9'));
}

int isspace(int c)
{
  return (c==' ');
}

int iscntrl(int c)
{
	switch (c)
	{
		case '\n':
		case '\r':
		case '\t':
		case '\f':
		case '\a':
			return 1;
		default:
			return 0;
	}
}

int toupper(int c)
{
  //unset bit 5
  return (c & (~(1<<5)));
}

int tolower(int c)
{
  //set bit 5
  return (c | (1<< 5));
}

int isgraph(int c)
{
  return (c>=33) && (c<=126);
}

int islower(int c)
{
  return (c>=97) && (c<=122);

}

int isupper(int c)
{
  return (c>=65) && (c<=90);

}

int isprint(int c)
{
  return (c>=32) && (c<=126);
}

int ispunct(int c)
{
  return isprint(c) && (!isspace(c)) && (!isalpha(c));
}



int isxdigit(int c)
{
  return ((c>=48) && (c<=57)) || ((c>=65) && (c<=70)) || ((c>=97) && (c<=102));
}


double floor(double x)
{
  return (int) x - (x < (int) x);
}

double ceil(double x)
{
  return (int) x + (x > (int) x);
}

double fmod(double a, double b)
{
    return (a - b * floor(a / b));
}

double sqrt(double x)
{
	sendstringf("DBVM doesn't do powers or roots atm");
	return x;
}


double pow(double x, double y)
{
	sendstringf("DBVM doesn't do powers or roots atm");
	return x*y;
}

double frexp(double x UNUSED, int *exp UNUSED)
{
  sendstringf("DBVM doesn't do frexp yet");
  return 0;
}


int isalnum(int c)
{
  return (isalpha(c) || isdigit(c));
}

int memcmp(const void *s1, const void *s2, size_t n)
{
  unsigned int i;
  unsigned char *m1=(unsigned char *)s1;
  unsigned char *m2=(unsigned char *)s2;

  for (i=0; i<n; i++)
  {
    if (m1[i]>m2[i])
      return 1;

    if (m1[i]<m2[i])
      return -1;
  }

  return 0;

}

int strcmp(const char *s1, const char *s2)
{
  int i=0;
  while (s1[i] || s2[i])
  {
    if (s1[i]>s2[i])
      return 1;

    if (s1[i]<s2[i])
      return -1;

    i++;
  }

  return 0;
}


char *addCharToString(char c, char* string, int lastpos, int *stringsize)
/*
 * Adds a char to a variablesize string
 * */
{
  if ((*stringsize==0) || (string==NULL))
  {
    string=(char *)malloc(8);
    *stringsize=8;
  }

  if (lastpos>=(*stringsize))
  {
    int newsize=min((*stringsize)*2, (*stringsize)+4096);
    string=(char *)realloc(string, newsize);

    *stringsize=newsize;
  }

  string[lastpos]=c;
  return string;
}

int strncmp(const char *s1, const char *s2, size_t n)
{
  unsigned int i=0;
  while ((i<n) && (s1[i] || s2[i]))
  {
    if (s1[i]>s2[i])
      return 1;

    if (s1[i]<s2[i])
      return -1;

    i++;
  }

  return 0;
}

char *strstr(const char *haystack, const char *needle)
{
  int i;
  int needlelength=strlen(needle);
  for (i=0; haystack[i]; i++)
  {
    char *currentstring=(char *)&haystack[i];
    if (strncmp(currentstring,needle,needlelength)==0)
      return currentstring;
  }

  return NULL;
}

size_t strcspn(const char *s, const char *reject)
{
	int i,j;
	for (i=0; s[i]; i++)
	{
		for (j=0; reject[j]; j++)
		{
			if (s[i]==reject[j])
				return i;
		}
	}

	return i-1;
}

int strcoll(const char *s1, const char *s2)
{
  return strcmp(s1,s2);
}

char *strchr(const char *s, int c)
{
  int i;
  for (i=0; s[i]; i++)
    if (s[i]==c)
      return (char*)&s[i];

  return NULL;
}

char *strpbrk(const char *s, const char *accept)
{
  int i, j;

  int al=strlen(accept);

  for (i = 0; s[i] != '\0';i++)
  {
    for (j=0; j<al; j++)
      if (s[i]== accept[j])
      {
        return (char *)&s[i];
      }
  }

  return NULL;
}

#define ISSPACE(c) (c==' ')

double atof(char* num)
 {
     if (!num || !*num)
         return 0;
     double integerPart = 0;
     double fractionPart = 0;
     int divisorForFraction = 1;
     int sign = 1;
     int inFraction = 0;
     /*Take care of +/- sign*/
     if (*num == '-')
     {
         ++num;
         sign = -1;
     }
     else if (*num == '+')
     {
         ++num;
     }
     while (*num != '\0')
     {
         if (*num >= '0' && *num <= '9')
         {
             if (inFraction)
             {
                 /*See how are we converting a character to integer*/
                 fractionPart = fractionPart*10 + (*num - '0');
                 divisorForFraction *= 10;
             }
             else
             {
                 integerPart = integerPart*10 + (*num - '0');
             }
         }
         else if (*num == '.')
         {
             if (inFraction)
                 return sign * (integerPart + fractionPart/divisorForFraction);
             else
                 inFraction = 1;
         }
         else
         {
             return sign * (integerPart + fractionPart/divisorForFraction);
         }
         ++num;
     }
     return sign * (integerPart + fractionPart/divisorForFraction);
 }

double strtod(const char *str, char **ptr)
{
  char *p;

    p = (char *)str;

    if (ptr==NULL)
      return atof((char *)str);

    while (ISSPACE (*p))
      ++p;

    if (*p == '+' || *p == '-')
      ++p;

    /* INF or INFINITY.  */
    if ((p[0] == 'i' || p[0] == 'I')
        && (p[1] == 'n' || p[1] == 'N')
        && (p[2] == 'f' || p[2] == 'F'))
      {
        if ((p[3] == 'i' || p[3] == 'I')
      && (p[4] == 'n' || p[4] == 'N')
      && (p[5] == 'i' || p[5] == 'I')
      && (p[6] == 't' || p[6] == 'T')
      && (p[7] == 'y' || p[7] == 'Y'))
    {
      *ptr = p + 8;
      return atof ((char *)str);
    }
        else
    {
      *ptr = p + 3;
      return atof ((char *)str);
    }
      }

    /* NAN or NAN(foo).  */
    if ((p[0] == 'n' || p[0] == 'N')
        && (p[1] == 'a' || p[1] == 'A')
        && (p[2] == 'n' || p[2] == 'N'))
      {
        p += 3;
        if (*p == '(')
    {
      ++p;
      while (*p != '\0' && *p != ')')
        ++p;
      if (*p == ')')
        ++p;
    }
        *ptr = p;
        return atof ((char *)str);
      }

    /* digits, with 0 or 1 periods in it.  */
    if (isdigit(*p) || *p == '.')
      {
        int got_dot = 0;
        while (isdigit (*p) || (!got_dot && *p == '.'))
    {
      if (*p == '.')
        got_dot = 1;
      ++p;
    }

        /* Exponent.  */
        if (*p == 'e' || *p == 'E')
    {
      int i;
      i = 1;
      if (p[i] == '+' || p[i] == '-')
        ++i;
      if (isdigit (p[i]))
        {
          while (isdigit (p[i]))
      ++i;
          *ptr = p + i;
          return atof ((char *)str);
        }
    }
        *ptr = p;
        return atof ((char *)str);
      }
    /* Didn't find any digits.  Doesn't look like a number.  */
    *ptr = (char *)str;
    return 0.0;
}

void *memchr(const void *s, int c, size_t n)
{
  size_t i;
  unsigned char usc=(unsigned char)c;
  unsigned char *a=(unsigned char *)s;

  for (i=0; i<n; i++)
  {
    if (a[i]==usc)
      return &a[i];
  }

  return NULL;
}

int vbuildstring(char *str, int size, char *string, __builtin_va_list arglist)
{
  struct
  {
    int type;
    int size;
    int digitcount;
  } varlist[64];
  //unsigned char varlist[64];
  char temps[100];
  char workstring[strlen(string)];
  int i,_i,l,strpos,vlc;
  //int count;

  //int debug=0;
  l=strlen(string);
  vlc=0;

  if (size==0)
    return 0;

  strpos=0;

  // work on the copy of string, not the original
  for (i=0; (unsigned int)i<strlen(string); i++)
    workstring[i]=string[i];

  zeromemory(varlist,64);

  for (i=0; i<64; i++)
  {
    varlist[i].type=255;
    varlist[i].digitcount=0;
  }


  // parse the string for known operators
  for (i=0; i<l; i++)
  {
    if (workstring[i]=='%')
    {
      int additional=0; //gets added to the size

      workstring[i]=0;

      if ((i+1<l) && (workstring[i+1]=='.'))
      {
        workstring[i+1]=0;
        additional++;
        if ((i+2<l) && (isdigit(workstring[i+2])))  //%.#(xxxx)
        {
          varlist[vlc].digitcount=workstring[i+2]-'0';
          workstring[i+2]=0;

          additional++;
          i+=2;
        }
      }

      if ((i+1<l) && (workstring[i+1]=='l'))
      {

        if ((i+2<l) && (workstring[i+2]=='l'))
        {
          if ((i+3<l) && (workstring[i+3]=='d')) //%lld
          {
            varlist[vlc].type=7; //64-bit decimal
            varlist[vlc].size=4;
          }
          else
          if ((i+3<l) && (workstring[i+3]=='x')) //%llx
          {
            varlist[vlc].type=8; //64-bit(16 char) hexadecimal
            varlist[vlc].size=4;
          }
        }

      }

      if (workstring[i+1]=='d') //decimal
      {
        varlist[vlc].type=0;
        varlist[vlc].size=2;
      }
      else
      if (workstring[i+1]=='x') //hex
      {
        varlist[vlc].type=1;
        varlist[vlc].size=2;
      }
      else
      if (workstring[i+1]=='8') //8 char hex (%8)
      {
        varlist[vlc].type=3;
        varlist[vlc].size=2;
      }
      else
      if (workstring[i+1]=='p') //6 char hex (%p)
      {
        varlist[vlc].type=4;
        varlist[vlc].size=2;
      }
      else
      if (workstring[i+1]=='6') //16 char hex (%8)
      {
        varlist[vlc].type=4;
        varlist[vlc].size=2;
      }
      else
      if (workstring[i+1]=='2') //2 char hex (%2)
      {
        varlist[vlc].type=6;
        varlist[vlc].size=2;
      }
      else
      if (workstring[i+1]=='s') //string
      {
        varlist[vlc].type=2;
        varlist[vlc].size=2;
      }
      else
      if (workstring[i+1]=='c') //char
      {
        varlist[vlc].type=5;
        varlist[vlc].size=2;
      }

      varlist[vlc].size+=additional;

      workstring[i+1]=0;
      vlc++;

      if (vlc>=64) //todo: malloc/realloc
        break;
    }
  }

  i=0;
  vlc=0;


  while ((i<l) && (strpos<size))
  {
    if (workstring[i]==0)
    {
      if (varlist[vlc].type==255)
      {
        printstring("UNDEFINED VARLIST",60,22,2,4);
        sendstring("UNDEFINED VARLIST");

        return 0;

      }

      switch (varlist[vlc].type)
      {
        case 0: //decimal
        {
          unsigned int x;
          x=__builtin_va_arg(arglist,unsigned int);
          itoa(x,10,temps,100);

          _i=strlen(temps);
          if (strpos+_i>=size)
            _i=size-(strpos+_i-size);

          copymem(&str[strpos],temps,_i);
          strpos+=_i;
          break;
        }

        case 1: //hex
        {
          unsigned int x;
          x=__builtin_va_arg(arglist,unsigned int);
          itoa(x,16,temps,100);

          _i=strlen(temps);
          if (strpos+_i>=size)
            _i=size-(strpos+_i-size);

          copymem(&str[strpos],temps,_i);
          strpos+=_i;

          break;
        }

        case 3: //%8, DWORD
        {
          unsigned int x;
          x=__builtin_va_arg(arglist,unsigned int);
          itoa(x,16,temps,100);

          appendzero(temps,8,100);

          _i=strlen(temps);
          if (strpos+_i>=size)
            _i=size-(strpos+_i-size);

          copymem(&str[strpos],temps,_i);
          strpos+=_i;
          break;
        }

        case 6: //%2, char
        {

          unsigned char x;
          x=__builtin_va_arg(arglist,int);


          itoa(x,16,temps,100);
          appendzero(temps,2,100);

          _i=strlen(temps);

          if (strpos+_i>=size)
            _i=size-(strpos+_i-size);

          copymem(&str[strpos],temps,_i);
          strpos+=_i;
          break;
        }

        case 7: //%lld
        {
          long long x;
          x=__builtin_va_arg(arglist,long long);
          lltoa(x,10,temps,100);

          _i=strlen(temps);
          if (strpos+_i>=size)
            _i=size-(strpos+_i-size);

          copymem(&str[strpos],temps,_i);
          strpos+=_i;
          break;
        }



        case 255:
          printstring("UNDEFINED VARLIST",40,21,2,4);
          sendstring("UNDEFINED VARLIST");
          /*printstring(string,40,22,2,4);
          printstring(temps,40,23,2,4);
          printstring(str,40,24,2,4);*/\

          if (strpos>=size)
            strpos=size-1;

          str[strpos]=0;

          return strpos;
          break;

      }

      //todo: move to switch/case above
      if (varlist[vlc].type==2) //string
      {
        char *s=__builtin_va_arg(arglist,char *);

        _i=strlen(s);
        if (strpos+_i>size)
          _i=size-(strpos+_i-size);

        copymem(&str[strpos],s,_i);
        strpos+=_i;
      }

      if ((varlist[vlc].type==4) || (varlist[vlc].type==8)) //16 char hex or llx
      {
        unsigned long long i=__builtin_va_arg(arglist,unsigned long long);

        lltoa(i,16,temps,100);
        if (varlist[vlc].type==4)
          appendzero(temps,16,100);
        else
        if (varlist[vlc].digitcount>0)
          appendzero(temps,varlist[vlc].digitcount,100);


        _i=strlen(temps);
        if (strpos+_i>=size)
          _i=size-(strpos+_i-size);

        copymem(&str[strpos],temps,_i);
        strpos+=_i;
      }

      if (varlist[vlc].type==5) //char
      {
        int c=__builtin_va_arg(arglist,int);

        str[strpos]=(char)c;
        strpos++;
      }

      //todo: ^^^^^^^^

      i+=varlist[vlc].size;

      vlc++; //next paramtype
      continue;
    }
    else
    {
      //else a normal char
      str[strpos]=workstring[i];
      strpos++;

      if (strpos>=size)
      {
        str[size-1]=0;
        return size; //enough
      }
      i++;
    }

  }



  if (strpos>=size)
    strpos=size-1;


  str[strpos]=0;
  return strpos;
}

void sendstring(char *s UNUSED)
{
#ifdef DELAYEDSERIAL
  if (!useserial) return;
#endif

#ifdef DEBUG
  #if DISPLAYDEBUG==1
    displayline(s);
  #else
    int i;

    if (nosendchar[getAPICID()])
        return;


    csEnter(&sendstringCS);

    for (i=0; s[i] ; i++)
      sendchar(s[i]);

    csLeave(&sendstringCS);
  #endif
#endif
}

#ifdef DEBUG
void sendstringf_nolock(char *string UNUSED, ...)
{
#ifdef DELAYEDSERIAL
  if (!useserial) return;
#endif
  nosendchar[getAPICID()]=0;

  __builtin_va_list arglist;
  char temps[200];
  int sl,i;

  __builtin_va_start(arglist,string);
  sl=vbuildstring(temps,200,string,arglist);
  __builtin_va_end(arglist);

  #if DISPLAYDEBUG==1
    displayline(temps); //instead of sending the output to the serial port, output to the display
  #else
    if (sl>0)
    {
      for (i=0; i<sl; i++)
        sendchar(temps[i]);
    }
  #endif
}
#endif

void sendstringf(char *string UNUSED, ...)
{
#ifdef DELAYEDSERIAL
  if (!useserial) return;
#endif


#ifdef DEBUG
  __builtin_va_list arglist;
  char temps[200];
  int sl,i;

  if (nosendchar[getAPICID()])
      return;



  __builtin_va_start(arglist,string);
  sl=vbuildstring(temps,200,string,arglist);
  __builtin_va_end(arglist);

  #if DISPLAYDEBUG==1
    displayline(temps); //instead of sending the output to the serial port, output to the display
  #else
    csEnter(&sendstringfCS);
    csEnter(&sendstringCS);

    if (sl>0)
    {
      for (i=0; i<sl; i++)
        sendchar(temps[i]);
    }

    csLeave(&sendstringCS);
    csLeave(&sendstringfCS);
  #endif
#endif
}

int sprintf(char *str, const char *format, ...)
{
  __builtin_va_list arglist;
  int sl;

  __builtin_va_start(arglist,format);
  sl=vbuildstring(str,4096,(char *)format,arglist);
  __builtin_va_end(arglist);

  return sl;
}

int snprintf(char *str, size_t size, const char *format, ...)
{
  __builtin_va_list arglist;
  int sl;

  __builtin_va_start(arglist,format);
  sl=vbuildstring(str,size,(char *)format,arglist);
  __builtin_va_end(arglist);

  return sl;
}




unsigned int getAPICID(void)
{
  UINT64 a,b,c,d;
  a=0;
  b=0;
  c=0;
  d=0;

  a=1;
  _cpuid(&a,&b,&c,&d);

  return (b >> 24)+1;
}

void mrewStartRead(Pmultireadexclusivewritesychronizer MREW)
{
  while (1)
  {
    spinlock(&MREW->lock);
    if (MREW->writers)
    {
      MREW->lock=0;
      continue;
    }
    else
    {
      MREW->readers++;
      MREW->lock=0;
      break;
    }
  }
}

void mrewEndRead(Pmultireadexclusivewritesychronizer MREW)
{
  MREW->readers--;
}

void mrewStartWrite(Pmultireadexclusivewritesychronizer MREW)
{
  while (1)
  {
    spinlock(&MREW->lock);
    if ((MREW->readers) || (MREW->writers))
    {
      MREW->lock=0;
      _pause();
      continue;
    }
    else
    {
      MREW->writers++;
      MREW->lock=0;
      break;
    }
  }
}

void mrewEndWrite(Pmultireadexclusivewritesychronizer MREW)
{
  MREW->writers=0;
}


void csEnter(PcriticalSection CS)
{
#ifdef DEBUG
  if (CS->ignorelock)
    return;

#endif


  int apicid=getAPICID()+1; //+1 so it never returns 0

  if ((CS->locked) && (CS->apicid==apicid))
  {
    //already locked but the locker is this cpu, so allow, just increase lockcount
    CS->lockcount++;
    return;
  }






#ifdef DEBUG
  //sendstringf_nolock("%d",getcpuinfo()->cpunr);
  if (spinlock(&(CS->locked)))
  {
    while (1)
    {
      nosendchar[getAPICID()]=0;
      if ((emergencyOutputOnly==FALSE) || (CS->debuglevel>emergencyOutputLevel)) //similar to a BSOD
      {
        emergencyOutputOnly=TRUE;
        emergencyOutputAPICID=getAPICID();
        emergencyOutputLevel=CS->debuglevel;
      }

      sendstringf_nolock("%d: spinlock timeout. CS Name=",getcpuinfo()->cpunr); //todo: more info
      if (CS->name)
        sendstringf_nolock(CS->name);

      sendstringf_nolock("\n");

      sendstringf_nolock("CS->apicid=%d CS->lockcount=%d\n", CS->apicid, CS->lockcount);

      pcpuinfo c=firstcpuinfo;
      while (c)
      {
        if ((int)(c->apicid)==(int)(CS->apicid-1))
        {
          sendstringf_nolock("Locked by cpunr %d\n", c->cpunr);
          sendstringf_nolock("LastVMCall=%x\n", c->LastVMCall);
          sendstringf_nolock("insideHandler=%d\n", c->insideHandler);

          break;
        }
        c=c->next;
      }





    }

  }
#else
  spinlock(&(CS->locked)); //sets CS->locked to 1
#endif

  asm volatile ("": : :"memory");

  //here so the lock is aquired and locked is 1
  CS->lockcount=1;
  CS->apicid=apicid;
}

void csLeave(PcriticalSection CS)
{
#ifdef DEBUG
  if (CS->ignorelock)
    return;
#endif

  int apicid=getAPICID()+1; //+1 so it never returns 0
  int locked=CS->locked;
  int ownerAPICID=CS->apicid;


  if ((CS->locked) && (CS->apicid==apicid))
  {
    asm volatile ("": : :"memory");
    CS->lockcount--;
    asm volatile ("": : :"memory");
    if (CS->lockcount==0)
    {
      //unlock
      CS->apicid=-1; //set to an invalid apicid
      asm volatile ("": : :"memory");
      CS->locked=0;
      asm volatile ("": : :"memory");

    }
  }
  else
  {
#ifdef DEBUG
    nosendchar[getAPICID()]=0;
    sendstringf_nolock("csLeave called for a non-locked or non-owned critical section.  Name=%s\n", CS->name);
#endif
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1)
    {
#ifdef DEBUG
      if (CS->ignorelock)
      {
        outportb(0x80,0xc5); //todo: return
      }
#endif

      outportb(0x80,0xc2);
      if (locked)
      {
        outportb(0x80,0xc3);
      }
      if (ownerAPICID!=apicid)
      {
        outportb(0x80,0xc4);
      }


    }
  }
}


void zeromemory(volatile void *address, unsigned int size)
{
  unsigned int i;
  volatile unsigned char *a=(volatile unsigned char *)address;
  for (i=0; i < size; i++)
    a[i]=0;
}


volatile void* copymem(volatile void *dest, volatile const void *src, size_t size)
{
  unsigned int i;
  volatile unsigned char *d=dest,*s=(volatile unsigned char *)src;

  for (i=0; i<size; i++)
    d[i]=s[i];

  return dest;
}

void *memcpy(void *dest, const void *src, size_t n)
{
  return (void *)copymem(dest,src,n);
}

void *memset(void *s, int c, size_t n)
{
  unsigned int i;
  volatile unsigned char *dest=s;
  for (i=0; i<n; i++)
    dest[i]=c;

  return (void *)dest;
}

size_t strlen(const char *s)
{
  int length=0;

  for (length=0; s[length]; length++) ;
  return length;
}

char *strncat(char *dest, const char *src, size_t n)
{
  size_t dest_len = strlen(dest);
  size_t i;

  for (i = 0 ; i < n && src[i] != '\0' ; i++)
    dest[dest_len + i] = src[i];

  dest[dest_len + i] = '\0';

  return dest;
}

char *strcat(char *dest, const char *src)
{
  int i,j=strlen((char *)dest);
  for (i=0; src[i] ; i++,j++)
    dest[j]=src[i];

  dest[j]=0;

  return (char *)dest;
}

char *strcpy(char *dest, const char *src)
{
  int i=strlen((char *)src);
  int j;
  for (j=0; j<i; j++)
    dest[j]=src[j];

  dest[i]=0;

  return (char *)dest;
}

char *strncpy(char *dest, const char *src, size_t n)
{
  size_t i;

  for (i = 0; i < n && src[i] != '\0'; i++)
	dest[i] = src[i];

  for ( ; i < n; i++)
    dest[i] = '\0';

  return dest;

}



void appendzero(char *string, int wantedsize,int maxstringsize)
{
/* basicly to be used after itoa */
  int i=0;
  int zerostoadd=wantedsize-strlen(string);
	char newstring[wantedsize+1];

  if ((zerostoadd+(int)strlen(string))>=maxstringsize)
    return; //not enough memory


	for (i=0; i<zerostoadd; i++)
    newstring[i]='0';


  newstring[zerostoadd]=0;
  newstring[wantedsize]=0;

  strcat(newstring,string);
  strcpy(string,newstring);

  string[maxstringsize-1]=0;
}

int abs(int j)
{
	if (j<0)
		return -j;
	else
		return j;
}

unsigned long long power(unsigned int x,unsigned int y)
{
	unsigned int i;
	unsigned long long result=1;

	if (y==0)
		result=1;

	for (i=0; i<y; i++)
		result*=x;

	return result;
}

unsigned long long int strtoull(const char *nptr, char **endptr, int base)
{
	int err;
	unsigned long long result=atoi2((char *)nptr, base, &err);


    if (endptr)
    {
        if (err==0)
        	err=strlen(nptr)+1;

    	*endptr=(char *)(&nptr[err]);
    }

    return result;
}

unsigned long long atoi2(char* input, int base, int *err)
{
	int i,j=0,start=0;
  unsigned char c;
	unsigned long long result=0;
	int negative=0;

	//parse the string according to the used base
	if (base<2)
	{
		if (err)
			*err=-1;
    return 0;
	}

	if (base>36)
	{
		if (err)
			*err=-1;
		return 0;
	}

  //some override checks
	//if it starts with 0x or $ make it base16 and set start to 2 or 1 respectivly
	while (input[start])
	{
		if (input[start]=='-')
		{
			negative=!negative;
      start++;
		}
		else
		if (input[start]=='$')
		{
			base=16;
			start++;
		}
		else
		if ((input[start]=='0') && (input[start+1]=='x'))
		{
			base=16;
			start+=2;
		}
		else
			break; //nothing to parse
	}


	for (i=strlen(input)-1; i>=start ; i--,j++)
  {
    if ((input[i] >='0') && (input[i] <= '9')) //check if it's in the range of 0 to 9
      c=input[i]-'0';
		else
    if ((input[i] >='a') && (input[i] <= 'z')) //check if it's in the range of 0 to 9
      c=10+input[i]-'a';
		else
    if ((input[i] >='A') && (input[i] <= 'Z')) //check if it's in the range of 0 to 9
      c=10+input[i]-'A';
		else
		{
			if (err)
				*err=i; //not a valid character
			return result;
		}

		if (c>=base)
		{
			if (err)
				*err=i; //not a valid character
			return result;
		}

		/* c now contains the value in numerical state, now adjust it for i and the given base) */
		result=result+c*(power(base,j));

		if (negative)
			result=(int)(-result);
	}

	if (err)
		*err=0;

  //sendstringf("atoi result is %6\n\r",result);
	return result;
}

int lltoa(unsigned long long value,int base, char *output,int maxsize)
/* base: 10=decimal, 16=hexadecimal, 8 = octal, 2=binary */
{
  char tempbuf[maxsize]; /* will get the string but in reverse */
  int i,j,t;

  if (base<2)
    return -1;

  if (base>36)
    return -1;

  if ((value==0) && (maxsize>1))
  {
    output[0]='0';
    output[1]=0;
    return 2;
  }


  for (i=0; (value>0) && (i<maxsize); i++)
  {
    t=value % base;
    if (t<=9)
      tempbuf[i]='0'+t;
    else
      tempbuf[i]='a'+t-10;

    value=value / base;
  }

  /* we now have the string in reverse order, so put it in output reverse... */
  t=i-1;
  for (j=0;t>=0;t--,j++)
    output[j]=tempbuf[t];

  if (i<maxsize)
    output[i]=0;
  else
    output[maxsize-1]=0;

  return i; //return how many bytes are used
}

int itoa(unsigned int value,int base, char *output,int maxsize)
/* base: 10=decimal, 16=hexadecimal, 8 = octal, 2=binary , 1=youraloser, 0=diebitch */
{
  char tempbuf[maxsize]; /* will get the string but in reverse */
  int i,j,t;

	if (base<2)
    return -1;

	if (base>36)
		return -1;

  if (value==0 && maxsize>1)
	{
    output[0]='0';
		output[1]=0;
	  return 2;
	}


	for (i=0; (value>0) && (i<maxsize); i++)
	{
		t=value % base;
    if (t<=9)
      tempbuf[i]='0'+t;
    else
      tempbuf[i]='a'+t-10;

	  value=value / base;
	}

  /* we now have the string in reverse order, so put it in output reverse... */
  t=i-1;
  for (j=0;t>=0;t--,j++)
    output[j]=tempbuf[t];

  if (i<maxsize)
		output[i]=0;
	else
    output[maxsize-1]=0;

	return i; //return how many bytes are used
}


void sendchar(char c UNUSED)
{
#ifdef DELAYEDSERIAL
  if (!useserial) return;
#endif


#if (defined SERIALPORT) && (SERIALPORT != 0)
	unsigned char x;

	if (!c)
	  return; //don't sent 0 bytes...


  x=c;
  if (x>0x80)
    c='.';

  if (nosendchar[getAPICID()])
    return;

  if (emergencyOutputOnly)
  {
    if (getAPICID()!=emergencyOutputAPICID)
      return;
  }


  if (c=='\r') //to deal with an obsolete linefeed not needed anymore
    return;

  x=inportb(SERIALPORT+5);

  //while ((x & 0x20) != 0x20)
  while ((x & 0x40) != 0x40)
  {
    _pause();
	  x=inportb(SERIALPORT+5);
  }

	outportb(SERIALPORT,c);

	if (1) //terminal emu under bochs, it NEEDS an linefeed, and this'll make it compatible with the non linefeed strings passed
	{
	  if (c=='\n')
	  {
	    x=inportb(SERIALPORT+5);

	    while ((x & 0x20) != 0x20)
	      x=inportb(SERIALPORT+5);

	    outportb(SERIALPORT,'\r');
	  }
	}
#endif

}

int getchar(void)
{
#ifdef DELAYEDSERIAL
  if (!useserial) return 0;
#endif


#if DISPLAYDEBUG==1
  return kbd_getchar();
#endif
#if (defined SERIALPORT) && (SERIALPORT != 0)
/* returns 0 when no char is pressed
	 use readstring to wait for keypresses */

  while ((inportb(SERIALPORT+5) & 0x60) != 0x60)
  {
    _pause();
  }


	if (inportb(SERIALPORT+5) & 0x1)
  {
    return inportb(SERIALPORT);
  }
	else
#endif
		return 0;

}

char inputa=0,inputb=0;
char waitforchar(void)
{
#if DISPLAYDEBUG==1
  return kbd_getchar();
#endif

  char c=0;
#if (defined SERIALPORT) && (SERIALPORT != 0)
	while (c==0)
  {
    c=getchar();
  }
#endif
  return c;



/*  while (inputa==0) ;//memedit at this address
  inputa=0;
  return inputb;*/

}

int readstringc(char *s, int minlength, int maxlength)
{
  int i=0;
  //keeps reading till it hits minlength, but can go over till maxlength
  while (i<minlength)
  {
    s[i]=kbd_getchar();
    if ((s[i]==13) || (s[i]==10))
    {
      s[i]=0;
      return i;
    }
    displayline("%c",s[i]);

    if (s[i]) i++;
  }

  if (i<maxlength)
    s[i]=0;

  return i;
}

int readstring(char *s, int minlength, int maxlength)
{
	int i=0;
	//keeps reading till it hits minlength or enter, but can go over till maxlength (depending on the size of the uart buffer)
	while (i<minlength)
	{
    s[i]=waitforchar();
		if ((s[i]==13) || (s[i]==10))
		{
			s[i]=0;
			return i;
		}
		sendchar(s[i]);

		if (s[i]) i++;
	}

  if (i<maxlength)
    s[i]=0;
  else
    s[maxlength-1]=0;




	return i;
}

#if (DISPLAYDEBUG==0)
  int askingforkey=0;
#endif

void setCursorPos(unsigned char x, unsigned char y)
{
#if (DISPLAYDEBUG==0)
  if (!loadedOS)
#endif
  {
    int cursorpos=y*80+x;
    outportb(0x3D4, 14);
    outportb(0x3D5, (cursorpos>>8));
    outportb(0x3D4, 15);
    outportb(0x3D5, cursorpos);
  }
}

void updateCursor(void)
{
  setCursorPos(currentdisplayrow, currentdisplayline);
}

void printchar(char c, int x, int y, char foreground, char background)
{
  PTEXTVIDEO tv=(PTEXTVIDEO)textmemory;
#if (DISPLAYDEBUG==0)
  if (!loadedOS)
#endif
  {
    tv[y*80+x].character=c;
    tv[y*80+x].foregroundcolor=foreground;
    tv[y*80+x].backgroundcolor=background;
  }
}

void getdisplaychar(int x, int y, PTEXTVIDEO charinfo)
{
  PTEXTVIDEO tv=(PTEXTVIDEO)textmemory;
#if (DISPLAYDEBUG==0)
  if (!loadedOS)
#endif
  {
    *charinfo=tv[y*80+x];
  }
}

void getdisplayline(int y, TEXTVIDEOLINE lineinfo)
{
  PTEXTVIDEO tv=(PTEXTVIDEO)textmemory;
#if (DISPLAYDEBUG==0)
  if (!loadedOS)
#endif
  {
    copymem(lineinfo, &tv[y*80], sizeof(TEXTVIDEOLINE));
  }
}


void printstring(char *s, int x, int y, char foreground, char background)
{
  int i;
  for (i=0; s[i]; i++,x++)
    printchar(s[i],x%80,y+(x/80),foreground,background);
}


void push(PStackList stackobject, void *data, int size)
{
  PStackListEntry previous=stackobject->last;
  PStackListEntry new=(PStackListEntry)malloc(sizeof(StackListEntry));
  new->data=malloc(size);
  new->previous=previous;
  copymem(new->data, data, size);

  stackobject->last=new;
}

int pop(PStackList stackobject, void *data, int size)
{
  PStackListEntry old=stackobject->last;

  if (old)
  {
    stackobject->last=old->previous;
    copymem(data, old->data, size);
    free(old->data);
    free(old);
    return 1;
  }
  else
    return 0; //empty list
}

volatile int zl=0;
void movelinesdown(void)
/*
 * Moves the lines down (if there is a log to get the previous line from)
 */
{
  PTEXTVIDEO tv=(PTEXTVIDEO)textmemory;
  TEXTVIDEO thischar;
  int x,y;


 // while (zl==0);

#if DISPLAYDEBUG==1
  //save the bottom line to the displaydebuglog_forward buffer
  if (displaydebuglog_forward) //possible it's NULL
    push(displaydebuglog_forward, &tv[24*80], sizeof(TEXTVIDEOLINE));
#endif

  for (y=24; y>=0; y--)
  {
    for (x=0; x<80; x++)
    {
      //move this char to the one at top
      getdisplaychar(x,y,&thischar);
      tv[(y+1)*80+x]=thischar;
    }
  }

#if DISPLAYDEBUG==1
  //get the top line from the displaydebuglog_back
  if (displaydebuglog_back) //possible it's NULL
    pop(displaydebuglog_back, tv, sizeof(TEXTVIDEOLINE));
#endif
}

void movelinesup(void)
/*
 * Moves the lines up
 */
{
  PTEXTVIDEO tv=(PTEXTVIDEO)textmemory;
  TEXTVIDEO thischar;


#if DISPLAYDEBUG==1
  //save the top line to the displaydebug buffer
  if (displaydebuglog_back) //possible it's NULL
    push(displaydebuglog_back, tv, sizeof(TEXTVIDEOLINE));
#endif

#if (DISPLAYDEBUG==0)
  if (!loadedOS)
#endif
  {
    int x,y;
    for (y=1; y<25; y++)
    {
      for (x=0; x<80; x++)
      {
        //move this char to the one at top
        getdisplaychar(x,y,&thischar);
        tv[(y-1)*80+x]=thischar;
      }
    }

    y=24;
    thischar.character=' ';
    thischar.backgroundcolor=0;
    thischar.foregroundcolor=15;
    for (x=0; x<80; x++)
      tv[y*80+x]=thischar;
  }

#if DISPLAYDEBUG==1
  //load the bottom line with the displaydebuglog_forward
  if (displaydebuglog_forward)
    pop(displaydebuglog_forward,&tv[24*80], sizeof(TEXTVIDEOLINE));
#endif

}

void nextline(void)
/*
 * move the 'cursor' down one line
 */
{


  currentdisplayrow=0; //all the way to the left

  if (currentdisplayline>=24)
  {
    movelinesup(); //all other lines go up one line and currentdisplayline stays the same
    currentdisplayline=24; //just set it in case 'something' sets it different
  }
  else
    currentdisplayline++;
//#if DISPLAYDEBUG==1
  #if 0 //disabling this while testing uefi graphics boot (nokb)
  {
    linessincelastkey++;
    if (linessincelastkey>=screenheight-1)
    {
      unsigned char c;
      int done=0;
      displayline("Press space to continue");
      while (done==0)
      {
        c=kbd_getchar();

        //displayline("(c=%x)", c);

        switch (c)
        {
          case 3: //page up
            if (displaydebuglog_back->last)
              movelinesdown();
            //not yet implemented
            break;

          case 4: //page down
            if (displaydebuglog_forward->last)
              movelinesup();
            break;

          case 1: //home
            while (displaydebuglog_back->last)
              movelinesdown();

            break;

          case 2: //end (or default)
          case ' ':
          {
            while (displaydebuglog_forward->last) //scroll to the end before continue
              movelinesup();

            done=1;
          }

        }
      }

      currentdisplayrow=0;
      displayline("                         ");
      currentdisplayrow=0;
      linessincelastkey=0;
    }
  }
#endif
}

void displayline(char *s, ...)
/* Displays a line on the screen and sets the 'currentline' down one pos
 */
{
  __builtin_va_list arglist;
  char temps[200];
  int sl,i;

  __builtin_va_start(arglist,s);
  sl=vbuildstring(temps,200,s,arglist);
  __builtin_va_end(arglist);


  if (sl>0)
  {

#ifdef DEBUG
#if (DISPLAYDEBUG==0)
  sendstringf(temps);
#endif
#endif

    csEnter(&sendstringfCS);
    csEnter(&sendstringCS);


    for (i=0; i<sl; i++)
    {
      if (temps[i])
      {
        if (temps[i]=='\n')
        {
          nextline();
          continue;
        }

        if (temps[i]=='\r') //not handled
          continue;
      }


      printchar(temps[i],currentdisplayrow,currentdisplayline,15,0);
      currentdisplayrow++;

      if ((currentdisplayrow % 80)==0)
        nextline();
    }


    updateCursor();

    csLeave(&sendstringCS);
    csLeave(&sendstringfCS);
  }
}

unsigned int generateCRC(unsigned char *ptr, int size)
{
  int i=0;
  unsigned int cval=0;
  while (i<size)
  {
    cval = ptr[i] + cval;
    i++;
  }

  return cval;
}

void resync(void)
{
	UINT64 a,b,c,d;
	_pause();
	_cpuid(&a,&b,&c,&d);
}

void sendDissectedFlags(PRFLAGS rflags)
{
  if (rflags==0)
	  return;

#ifdef DEBUG
  sendstringf("rflags=%6 ( ",*(DWORD*)rflags);

  if (rflags->CF)
    sendstring("CF ");


  if (rflags->PF)
    sendstring("PF ");

  if (rflags->AF)
    sendstring("AF ");

  if (rflags->ZF)
    sendstring("ZF ");

  if (rflags->SF)
    sendstring("SF ");

  if (rflags->TF)
    sendstring("TF ");

  if (rflags->IF)
    sendstring("IF ");

  if (rflags->DF)
    sendstring("DF ");

  if (rflags->OF)
    sendstring("OF ");

  if (rflags->NT)
    sendstring("NT ");

  if (rflags->RF)
    sendstring("RF ");

  if (rflags->VM)
    sendstring("VM ");

  if (rflags->AC)
    sendstring("AC ");

  if (rflags->VIF)
    sendstring("VIF ");

  if (rflags->VIP)
    sendstring("VIP ");

  if (rflags->ID)
    sendstring("ID ");

  if (rflags->IOPL)
    sendstringf("IOPL%d ",rflags->IOPL);

  sendstring(")");
#endif
}

void showstatec(ULONG *stack)
/* send the state of the registers to the serial port */
{
  if (stack==NULL)
	  return;

  sendstringf("s[0]=%8 s[1]=%8\n\r",stack[0],stack[1]);
  return;
}

#if DISPLAYDEBUG
void initialize_displaydebuglogs()
{
  displaydebuglog_back=malloc(sizeof(StackList));
  displaydebuglog_back->last=NULL;
  displaydebuglog_forward=malloc(sizeof(StackList));;
  displaydebuglog_forward->last=NULL;
}
#endif

void setGDTENTRYBase(PGDT_ENTRY entry, DWORD base)
{
  entry->Base0_23=base;
  entry->Base24_31=base >> 24;
}

DWORD getGDTENTRYBase(PGDT_ENTRY entry)
{
  return ((DWORD)entry->Base24_31 << 24) | ((DWORD)entry->Base0_23);
}

void setGDTENTRYLimit(PGDT_ENTRY entry, DWORD limit)
{
  if (limit>=0x100000) //give as pagecount
  {
    entry->G=1;

    limit=limit / 4096;
  }
  else
    entry->G=0;

  entry->Limit0_15=limit;
  entry->Limit16_19=limit >> 16;
}

GDT_ENTRY Build16BitDataSegmentDescriptor(DWORD baseaddress, DWORD size)
{
  GDT_ENTRY result;
  setGDTENTRYBase(&result, baseaddress);
  setGDTENTRYLimit(&result, size-1);

  result.Type=2; //data segment, no expand down, writable
  result.NotSystem=1;
  result.DPL=0;
  result.P=1;
  result.AVL=0;
  result.L=0;
  result.B_D=0; //16-bit

  return result;
}

GDT_ENTRY Build16BitCodeSegmentDescriptor(DWORD baseaddress, DWORD size)
{
  GDT_ENTRY result=Build16BitDataSegmentDescriptor(baseaddress, size);
  result.Type=0xa; //code segment, readable
  return result;
}

GDT_ENTRY Build32BitDataSegmentDescriptor(DWORD baseaddress, DWORD size)
{
  GDT_ENTRY result;
  setGDTENTRYBase(&result, baseaddress);
  setGDTENTRYLimit(&result, size-1);

  result.Type=2; //data segment, no expand down, writable
  result.NotSystem=1;
  result.DPL=0;
  result.P=1;
  result.AVL=0;
  result.L=0;
  result.B_D=1; //32-bit

  return result;
}

GDT_ENTRY Build32BitCodeSegmentDescriptor(DWORD baseaddress, DWORD size)
{
  GDT_ENTRY result=Build32BitDataSegmentDescriptor(baseaddress, size);
  result.Type=0xa; //code segment, readable
  return result;
}

int getCPUCount()
{
  return vmmentrycount;
}

void *gdtbase32;
void *stackbase32;
int call32bit(DWORD address)
{
  int r;
  //setup a stack and gdt

  QWORD oldgdtbase=getGDTbase();
  QWORD oldgdtsize=getGDTsize();

  if (gdtbase32==NULL)
  {
    void *mem=malloc(32*1024);
    gdtbase32=mapMemory((void*)0x10000000,(void *)oldgdtbase,oldgdtsize);

    stackbase32=mapMemory((void *)0x20000000,mem,32*1024);
    stackbase32=(void *)((QWORD)stackbase32+(32*1024)-8);
  }



  setGDT((QWORD)gdtbase32,oldgdtsize);

  r=call32bita(address,(DWORD)((QWORD)stackbase32));

  setGDT(oldgdtbase,oldgdtsize);

  return r;
}

int Initialized=0;
void InitCommon()
//call once
{
  if (Initialized) return;

  QWORD rax=1;
  QWORD rbx=0;
  QWORD rcx=0;
  QWORD rdx=0;
  _cpuid(&rax, &rbx, &rcx, &rdx);

  if (rcx & (1 << 23))
    popcnt=popcnt_support;
}
