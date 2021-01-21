#include <stdio.h>

#if defined _WIN32 && !defined __TINYC__
# define _ "_"
#else
# define _
#endif

int x3(void)
{
    printf(" x3");
    return 3;
}

/* That callx4 is defined globally (as if ".globl callx4")
   is a TCC extension.  GCC doesn't behave like this.  */
void callx4(void);
__asm__(_"callx4: call "_"x4; ret;"
#ifndef __TINYC__
    " .global "_"callx4"
#endif
);

extern void x5(void);

void callx5_again(void);
void callx5_again(void)
{
    x5();
    asm("call "_"x6");
}

static void x6()
{
    printf(" x6-2");
}
