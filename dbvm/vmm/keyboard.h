#ifndef KEYBOARD_H_
#define KEYBOARD_H_

//keyboard
//void waitforkeypress(void);
unsigned char kbd_getstatus(void);
unsigned char kdb_getoutputport(void);
unsigned char kdb_getinputport(void);
unsigned char kdb_getcommandbyte(void);

void kdb_setoutputport(unsigned char bt);
void kdb_setcommandbyte(unsigned char bt);

int kdb_iskeypressed(void);
void kdb_waitforkeypress(void);
unsigned char kbd_getkey(void);
char kbd_convertscancodetochar(unsigned char scancode, int scancodeset );
char kbd_getchar(void);


#endif /*KEYBOARD_H_*/
