//stdarg.h
#ifndef __STDARG_H
#define __STDARG_H
 // *fix 1.2.3b va_list is now char* for compatibility,
//  and the args are at increasing addresses (was broken by stack direction change)
 typedef char *va_list;
 #define va_start(ap,p)  ap = (char*)&p + 4
 #define va_arg(ap,T) *((T*&)ap)++
 #define va_end(ap)
#endif


