/* example from http://barnyard.syr.edu/quickies/led.c */

/* led.c: print out number as if on 7 line led display. I.e., write integer
   given on command line like this:  
      _   _       _  
   |  _|  _| |_| |_  
   | |_   _|   |  _| etc.

   We assume the terminal behaves like a classical teletype. So the top
   lines of all digits have to be printed first, then the middle lines of
   all digits, etc.

   By Terry R. McConnell

compile: cc -o led led.c

If you just want to link in the subroutine print_led that does all the
work, compile with -DNO_MAIN, and declare the following in any source file
that uses the call:

extern void print_led(unsigned long x, char *buf);

Bug: you cannot call repeatedly to print more than one number to a line.
That would require curses or some other terminal API that allows moving the
cursor to a previous line.

*/



#include <stdlib.h>
#include <stdio.h>

#define MAX_DIGITS 32
#define NO_MAIN


/* Print the top line of the digit d into buffer. 
   Does not null terminate buffer. */

void topline(int d, char *p){

   *p++ = ' ';
   switch(d){

      /* all these have _ on top line */

      case 0:
      case 2:
      case 3:
      case 5:
      case 7:
      case 8:
      case 9:
         *p++ = '_';
         break;
      default:
         *p++=' ';

   }
   *p++=' ';
}

/* Print the middle line of the digit d into the buffer. 
   Does not null terminate. */

void midline(int d, char *p){

   switch(d){

      /* those that have leading | on middle line */

      case 0:
      case 4:
      case 5:
      case 6:
      case 8:
      case 9:
         *p++='|';
         break;
      default:
         *p++=' ';	
   }
   switch(d){

      /* those that have _ on middle line */

      case 2:
      case 3:
      case 4:
      case 5:
      case 6:
      case 8:
      case 9:
         *p++='_';
         break;
      default:
         *p++=' ';

   }
   switch(d){

      /* those that have closing | on middle line */

      case 0:
      case 1:
      case 2:
      case 3:
      case 4:
      case 7:
      case 8:
      case 9:
         *p++='|';
         break;
      default:
         *p++=' ';

   }
}

/* Print the bottom line of the digit d. Does not null terminate. */

void botline(int d, char *p){


   switch(d){

      /* those that have leading | on bottom line */

      case 0:
      case 2:
      case 6:
      case 8:
         *p++='|';
         break;
      default:
         *p++=' ';	
   }
   switch(d){

      /* those that have _ on bottom line */

      case 0:
      case 2:
      case 3:
      case 5:
      case 6:
      case 8:
         *p++='_';
         break;
      default:
         *p++=' ';

   }
   switch(d){

      /* those that have closing | on bottom line */

      case 0:
      case 1:
      case 3:
      case 4:
      case 5:
      case 6:
      case 7:
      case 8:
      case 9:
         *p++='|';
         break;
      default:
         *p++=' ';

   }
}

/* Write the led representation of integer to string buffer. */

void print_led(unsigned long x, char *buf)
{

   int i=0,n;
   static int d[MAX_DIGITS];


   /* extract digits from x */

   n = ( x == 0L ? 1 : 0 );  /* 0 is a digit, hence a special case */

   while(x){
      d[n++] = (int)(x%10L);
      if(n >= MAX_DIGITS)break;
      x = x/10L;
   }

   /* print top lines of all digits */

   for(i=n-1;i>=0;i--){
      topline(d[i],buf);
      buf += 3;
      *buf++=' ';
   }
   *buf++='\n'; /* move teletype to next line */

   /* print middle lines of all digits */

   for(i=n-1;i>=0;i--){
      midline(d[i],buf);
      buf += 3;
      *buf++=' ';
   }
   *buf++='\n';

   /* print bottom lines of all digits */

   for(i=n-1;i>=0;i--){
      botline(d[i],buf);
      buf += 3;
      *buf++=' ';
   }
   *buf++='\n';
   *buf='\0';
}

int main()
{
   char buf[5*MAX_DIGITS];
   print_led(1234567, buf);
   printf("%s\n",buf);

   return 0;
}

#ifndef NO_MAIN
int main(int argc, char **argv)
{

   int i=0,n;
   long x;
   static int d[MAX_DIGITS];
   char buf[5*MAX_DIGITS];

   if(argc != 2){
      fprintf(stderr,"led: usage: led integer\n");
      return 1;
   }

   /* fetch argument from command line */

   x = atol(argv[1]);

   /* sanity check */

   if(x<0){
      fprintf(stderr,"led: %d must be non-negative\n",x);
      return 1;
   }

   print_led(x,buf);
   printf("%s\n",buf);

   return 0;

}
#endif

/* vim: set expandtab ts=4 sw=3 sts=3 tw=80 :*/
