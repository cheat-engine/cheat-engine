/*
common.c: 32-bit version Contains several functions that might be useful and can be shared by
multiple sources. (e.g vmm and vmloader)

edit: yeah, not vmm anymore as it's 64-bit, so vmloader only

*/

#include "common.h"

criticalSection sendstringfCS;
criticalSection sendstringCS;

int vbuildstring(char *str, int size, char *string, __builtin_va_list arglist)
{
  unsigned char varlist[64];
  char temps[100];
  char workstring[strlen(string)];
  int i,_i,l,strpos,vlc;

  l=strlen(string);
  vlc=0;

  if (size==0)
    return 0;

  strpos=0;

  // work on the copy of string, not the original
  for (i=0; i<strlen(string); i++)
    workstring[i]=string[i];

  zeromemory(varlist,64);

  for (i=0; i<64; i++)
    varlist[i]=255;


  // parse the string for known operators
  for (i=0; i<l; i++)
  {
    if (workstring[i]=='%')
    {
      workstring[i]=0;

      if (workstring[i+1]=='d') //decimal
      {
        varlist[vlc]=0;
      }
      else
      if (workstring[i+1]=='x') //hex
      {
        varlist[vlc]=1;
      }
      else
      if (workstring[i+1]=='8') //8 char hex (%8)
      {
        varlist[vlc]=3;
      }
      else
      if (workstring[i+1]=='p') //8 char hex (%8)
      {
        varlist[vlc]=3;
      }
      else
      if (workstring[i+1]=='6') //16 char hex (%8)
      {
        varlist[vlc]=4;
      }
      else
      if (workstring[i+1]=='2') //2 char hex (%2)
      {
        varlist[vlc]=6;
      }
      else
      if (workstring[i+1]=='s') //string
      {
        varlist[vlc]=2;
      }
      else
      if (workstring[i+1]=='c') //char
      {
        varlist[vlc]=5;
      }

      workstring[i+1]=0;
      vlc++;
    }
  }

  i=0;
  vlc=0;


  while ((i<l) && (strpos<size))
  {
    if (workstring[i]==0)
    {
      if (varlist[vlc]==255)
      {
        printstring("UNDEFINED VARLIST",60,22,2,4);
        while (1);
      }

      switch (varlist[vlc])
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
          itoa(_i,16,temps,100);

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

        case 255:
          printstring("UNDEFINED VARLIST",40,21,2,4);
          /*printstring(string,40,22,2,4);
          printstring(temps,40,23,2,4);
          printstring(str,40,24,2,4);*/\

          if (strpos>=size)
            strpos=size-1;

          str[strpos]=0;

          return strpos;
          break;

      }


      if (varlist[vlc]==2) //string
      {
        char *s=__builtin_va_arg(arglist,char *);

        _i=strlen(s);
        if (strpos+_i>size)
          _i=size-(strpos+_i-size);

        copymem(&str[strpos],s,_i);
        strpos+=_i;


      }

      if (varlist[vlc]==4) //16 char hex
      {
        unsigned long long i=__builtin_va_arg(arglist,unsigned long long);
        unsigned int p1=i;
        unsigned int p2=(unsigned long long)(i>>32);

        itoa(p2,16,temps,100);
        appendzero(temps,8,100);

        _i=8;
        if (strpos+_i>size)
          _i=size-(strpos+_i-size);

        copymem(&str[strpos],temps,_i);
        strpos+=_i;

        if (strpos>=size)
        {
          str[size-1]=0;
          return size; //enough
        }

        itoa(p1,16,temps,100);
        appendzero(temps,8,100);

        _i=8;
        if (strpos+_i>size)
          _i=size-(strpos+_i-size);

        copymem(&str[strpos],temps,_i);
        strpos+=_i;

      }

      if (varlist[vlc]==5) //char
      {
        int c=__builtin_va_arg(arglist,int);

        str[strpos]=(char)c;
        strpos++;

      }

      i+=2;
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

void sendstring(char *s)
{

  int i;

  csEnter(&sendstringCS);

  for (i=0; s[i] ; i++)
    sendchar(s[i]);

  csLeave(&sendstringCS);

}

void sendstringf(char *string, ...)
{
  __builtin_va_list arglist;
  char temps[200];
  int sl,i;

  __builtin_va_start(arglist,string);
  sl=vbuildstring(temps,200,string,arglist);
  __builtin_va_end(arglist);

  csEnter(&sendstringfCS);
  csEnter(&sendstringCS);

  if (sl>0)
  {
    for (i=0; i<sl; i++)
      sendchar(temps[i]);
  }

  csLeave(&sendstringCS);
  csLeave(&sendstringfCS);
  return;
}

int getAPICID(void)
{
  unsigned int a,b,c,d;
  a=1;
  _cpuid(&a,&b,&c,&d);

  return (b >> 24)+1;
}

void csEnter(PcriticalSection CS)
{
  int apicid=getAPICID()+1; //+1 so it never returns 0


  //get current apicid (from cpuid)

  if ((CS->locked) && (CS->apicid==apicid))
  {
    //already locked but the locker is this cpu, so allow, just increase lockcount
    CS->lockcount++;
    return;
  }

  spinlock(&(CS->locked)); //sets CS->locked to 1

  //here so the lock is aquired and locked is 1
  CS->lockcount=1;
  CS->apicid=apicid;
}

void csLeave(PcriticalSection CS)
{
  int apicid=getAPICID()+1; //+1 so it never returns 0

  if ((CS->locked) && (CS->apicid==apicid))
  {
    CS->lockcount--;
    if (CS->lockcount==0)
    {
      //unlock
      CS->apicid=-1; //set to a invalid apicid
      CS->locked=0;
    }
  }
}

void zeromemory(void *address, unsigned int size)
{
  unsigned int i;
  volatile unsigned char *a=(volatile unsigned char *)address;
  for (i=0; i < size; i++)
    a[i]=0;
}

int debugzeromem=0;
void zeromemoryd(void *address, unsigned int size)
{
  unsigned int i;
  volatile unsigned char *a=(volatile unsigned char *)address;
  for (i=0; i < size; i++)
  {
    if ((debugzeromem) && ((i%0x1000)==0))
    {
      sendstringf("i=%x\n", i);
    }

    a[i]=0;
  }
}


void copymem(void *dest, void *src,int size)
{
  int i;
  unsigned char *d=dest,*s=src;

  for (i=0; i<size; i++)
    d[i]=s[i];
}

unsigned int strlen(char *string)
{
  int length=0;

	for (length=0; string[length]; length++) ;
  return length;
}

char* strcat(char *dest, char *src)
{
  int i,j=strlen(dest);
  for (i=0; src[i] ; i++,j++)
    dest[j]=src[i];

  dest[j]=0;

  return dest;
}

char* strcpy(char *dest, char *src)
{
  int i=strlen(src);
  int j;
  for (j=0; j<i; j++)
    dest[j]=src[j];

  dest[i]=0;
  return dest;
}

void appendzero(char *string, int wantedsize,int maxstringsize)
{
/* basicly to be used after itoa */
  int i=0;
  int zerostoadd=wantedsize-strlen(string);
  char newstring[wantedsize+1];

  if ((zerostoadd+strlen(string))>=maxstringsize)
    return; //not enough memory


  for (i=0; i<zerostoadd; i++)
    newstring[i]='0';


  newstring[zerostoadd]=0;
  newstring[wantedsize]=0;

  strcat(newstring,string);
  strcpy(string,newstring);

  string[maxstringsize-1]=0;
}

unsigned int power(unsigned int x,unsigned int y)
{
	int i;
	unsigned int result=1;

	if (y==0)
		result=1;

	for (i=0; i<y; i++)
		result*=x;

	return result;
}

unsigned int atoi(char* input, int base, int *err)
{
	int i,j=0,start=0;
  unsigned char c;
	unsigned int result=0;
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

		/* c now contains the value in numerical state, now adjust it for i and the base given) */
		result=result+c*(power(base,j));

		if (negative)
			result=(int)(-result);
	}

	if (err)
		*err=0;
	return result;
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

void sendchar(char c)
{
	char x;

#if (!defined SERIALPORT) || (SERIALPORT == 0)
	return;
#endif

  if (c=='\r')
    return;

  if (nosendchar[getAPICID()])
    return;

  x=inportb(SERIALPORT+5);
 //while ((x & 0x20) != 0x20)
  while ((x & 0x40) != 0x40)
	  x=inportb(SERIALPORT+5);

	outportb(SERIALPORT,c);

	if (c=='\n')
	{
	  x=inportb(SERIALPORT+5);
	  while ((x & 0x20) != 0x20)
	    x=inportb(SERIALPORT+5);

	  outportb(SERIALPORT,'\r');
	}


}

void waitforkeypress(void)
{
  char x=inportb(0x60);

  while (x==inportb(0x60)) ;
}

char getchar(void)
{
/* returns 0 when no char is pressed
	 use readstring to wait for keypresses */
#if (!defined SERIALPORT) || (SERIALPORT == 0)
	return 1;
#else
	if (inportb(SERIALPORT+5) & 0x1)
    return inportb(SERIALPORT);
	else
		return 0;
#endif


}

char waitforchar(void)
{
  char c=0;
	while (c==0)
	  c=getchar();

  return c;
}


int readstring(char *s, int minlength, int maxlength)
{
	int i=0;
	//keeps reading till it hits minlength, but can go over till maxlength (depending on the size of the uart buffer)
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

  s[i]=0;

  //minlength reached
  while ((i<maxlength) && (s[i]))
	{
	  s[i]=getchar();
		i++;
		if (s[i])
			sendchar(s[i]);
	}

	return i;
}


void printchar(char c, int x, int y, char foreground, char background)
{
  PTEXTVIDEO tv=(PTEXTVIDEO)0x0b8000;
  tv[y*80+x].character=c;
  tv[y*80+x].foregroundcolor=foreground;
	tv[y*80+x].backgroundcolor=background;
}

void getdisplaychar(int x, int y, PTEXTVIDEO charinfo)
{
  PTEXTVIDEO tv=(PTEXTVIDEO)0x0b8000;
  *charinfo=tv[y*80+x];
}


void printstring(char *s, int x, int y, char foreground, char background)
{
  int i;
  for (i=0; s[i]; i++,x++)
    printchar(s[i],x%80,y+(x/80),foreground,background);
}

void movelinesup(void)
{
  PTEXTVIDEO tv=(PTEXTVIDEO)0x0b8000;
  TEXTVIDEO thischar;

  int x,y;
  for (y=0; y<25; y++)  //change y=0 to y=7 for a nice effect
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
}

void updateCursor(void)
{
  int cursorpos;
  cursorpos=currentdisplayline*80+currentdisplayrow;


  outportb(0x3D4, 14);
  outportb(0x3D5, (cursorpos>>8));
  outportb(0x3D4, 15);
  outportb(0x3D5, cursorpos);
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
    csEnter(&sendstringfCS);
    csEnter(&sendstringCS);

    for (i=0; i<sl; i++)
    {
      if (temps[i]=='\n')
      {
        nextline();
        continue;
      }

      if (temps[i]=='\r') //not handled
        continue;


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

int generateCRC(unsigned char *ptr, int size)
{
  int i=0;
  unsigned int cval=0;
  while (i<size)
  {
    cval = ( ptr[i] + cval ) % 65536;
    i++;
  }

  return cval;
}

void showstatec(ULONG *stack)
/* send the state of the registers to the serial port */
{
  sendstringf("idtbase=%8 gdtbase=%8\n\r",stack[0],stack[1]);
  sendstringf("cr3=%8 cr2=%8 cr1=%8 cr0=%8\n\r",stack[2],stack[3],stack[4],stack[5]);
  sendstringf("eflags=%8\n\r",stack[6]);


  /*
  sendstringf("cr0=%8\n\r",getCR0());
  sendstringf("cr3=%8\n\r",getCR3());
  sendstringf("cr4=%8\n\r",getCR4());
  sendstringf("GDT base=%8\n\r",getGDTbase());
  sendstringf("IDT base=%8\n\r",getIDTbase());
  */


}
