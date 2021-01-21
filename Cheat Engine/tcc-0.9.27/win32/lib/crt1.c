// =============================================
// crt1.c

// _UNICODE for tchar.h, UNICODE for API
#include <tchar.h>

#include <stdio.h>
#include <stdlib.h>

#define _UNKNOWN_APP    0
#define _CONSOLE_APP    1
#define _GUI_APP        2

#define _MCW_PC         0x00030000 // Precision Control
#define _PC_24          0x00020000 // 24 bits
#define _PC_53          0x00010000 // 53 bits
#define _PC_64          0x00000000 // 64 bits

#ifdef _UNICODE
#define __tgetmainargs __wgetmainargs
#define _tstart _wstart
#define _tmain wmain
#define _runtmain _runwmain
#else
#define __tgetmainargs __getmainargs
#define _tstart _start
#define _tmain main
#define _runtmain _runmain
#endif

typedef struct { int newmode; } _startupinfo;
int __cdecl __tgetmainargs(int *pargc, _TCHAR ***pargv, _TCHAR ***penv, int globb, _startupinfo*);
void __cdecl __set_app_type(int apptype);
unsigned int __cdecl _controlfp(unsigned int new_value, unsigned int mask);
extern int _tmain(int argc, _TCHAR * argv[], _TCHAR * env[]);

/* Allow command-line globbing with "int _dowildcard = 1;" in the user source */
int _dowildcard;

void _tstart(void)
{
    __TRY__
    _startupinfo start_info = {0};

    // Sets the current application type
    __set_app_type(_CONSOLE_APP);

    // Set default FP precision to 53 bits (8-byte double)
    // _MCW_PC (Precision control) is not supported on ARM
#if defined __i386__ || defined __x86_64__
    _controlfp(_PC_53, _MCW_PC);
#endif

    __tgetmainargs( &__argc, &__targv, &_tenviron, _dowildcard, &start_info);
    exit(_tmain(__argc, __targv, _tenviron));
}

int _runtmain(int argc, /* as tcc passed in */ char **argv)
{
#ifdef UNICODE
    _startupinfo start_info = {0};

    __tgetmainargs(&__argc, &__targv, &_tenviron, _dowildcard, &start_info);
    /* may be wrong when tcc has received wildcards (*.c) */
    if (argc < __argc) {
        __targv += __argc - argc;
        __argc = argc;
    }
#else
    __argc = argc;
    __targv = argv;
#endif
#if defined __i386__ || defined __x86_64__
    _controlfp(_PC_53, _MCW_PC);
#endif
    return _tmain(__argc, __targv, _tenviron);
}

// =============================================
