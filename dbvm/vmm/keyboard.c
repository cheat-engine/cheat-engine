#include "common.h"

#define dataport 0x60
#define commandport 0x64

void kdb_readycommand(void)
{
	unsigned char x;
	x=(inportb(commandport) >> 1) & 1;
	while (x)
	{
		inportb(dataport);
		x=(inportb(commandport) >> 1) & 1;
	}
	
	x=inportb(commandport) & 1;;
	while (x)
	{
		inportb(dataport);
		x=inportb(commandport) & 1;;
	}	
}

unsigned char kbd_getstatus(void)
{
	return inportb(commandport);
}

unsigned char kdb_getoutputport(void)
{
	kdb_readycommand();	
	outportb(commandport,0xd0);	
	return inportb(dataport);	
}

void kdb_setoutputport(unsigned char bt)
{
	kdb_readycommand();	
	outportb(commandport,0xd1);	
	outportb(dataport,bt);
}

unsigned char kdb_getinputport(void)
{
	kdb_readycommand();	
	outportb(commandport,0xc0);	
	return inportb(dataport);		
}

unsigned char kdb_getcommandbyte(void)
{
	kdb_readycommand();	
	outportb(commandport,0x20);	
	return inportb(dataport);		
}

void kdb_setcommandbyte(unsigned char bt)
{
	kdb_readycommand();	
	outportb(commandport,0x60);	
	outportb(dataport,bt);
}

int kdb_iskeypressed(void)
{
	return inportb(commandport) & 1;	
}


void kdb_waitforkeypress(void)
{
  while (!kdb_iskeypressed())
  	resync();  
}

unsigned char kbd_getkey(void)
{
	kdb_waitforkeypress();
	return inportb(dataport);
}

char kbd_convertscancodetochar(unsigned char scancode, int scancodeset )
{
  //displayline(" -%x- ", scancode);
	if (scancodeset==1)
	{
		switch (scancode)
		{
			case 1: return 27; //escape
			case 2: return '1';
			case 3: return '2';
			case 4: return '3';
			case 5: return '4';
			case 6: return '5';
			case 7: return '6';
			case 8: return '7';
			case 9: return '8';
			case 0xa: return '9';
			case 0xb: return '0';
			case 0xc: return '-';
			case 0xd: return '+';
			case 0xe: return 8; //backspace
			case 0xf: return 9; //tab
			case 0x10: return 'q';
			case 0x11: return 'w';
			case 0x12: return 'e';
			case 0x13: return 'r';
			case 0x14: return 't';
			case 0x15: return 'y';
			case 0x16: return 'u';
			case 0x17: return 'i';
			case 0x18: return 'o';
			case 0x19: return 'p';
			case 0x1a: return '[';
			case 0x1b: return ']';
			case 0x1c: return '\n';
			case 0x1e: return 'a';
			case 0x1f: return 's';
			case 0x20: return 'd';
			case 0x21: return 'f';
			case 0x22: return 'g';
			case 0x23: return 'h';
			case 0x24: return 'j';
			case 0x25: return 'k';
			case 0x26: return 'l';
			case 0x27: return ';';
			case 0x28: return '\'';
			case 0x29: return '`';
			case 0x2b: return '\\';
			case 0x2c: return 'z';
			case 0x2d: return 'x';
			case 0x2e: return 'c';
			case 0x2f: return 'v';
			case 0x30: return 'b';
			case 0x31: return 'n';
			case 0x32: return 'm';
			case 0x33: return ',';
			case 0x34: return '.';
			case 0x35: return '/';
			case 0x39: return ' ';
			

			case 0x47: return 1; //home
			case 0x49: return 3; //page up
			case 0x4f: return 2; //end
			case 0x51: return 4; //page down

			default:  return 0;
		}
	} else return 0;
}



char kbd_getchar(void)
{
	//kbd_getkey returns the scancode of the keyboard, which will need to be converted to a pressed char
	char k=kbd_convertscancodetochar(kbd_getkey(),1);
  while (!k)
    k=kbd_convertscancodetochar(kbd_getkey(),1); 
  
  return k; //todo, check which scancoce set is currently being used
}
