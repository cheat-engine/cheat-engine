#include <stdio.h>

#if defined _WIN32 && !defined __TINYC__
# define _ "_"
#else
# define _
#endif

static int x1_c(void)
{
    printf(" x1");
    return 1;
}

asm(".text;"_"x1: call "_"x1_c; ret");

void callx4(void);
void callx5_again(void);

void x6()
{
    printf(" x6-1");
}

int main(int argc, char *argv[])
{
    printf("*");
    asm("call "_"x1");
    asm("call "_"x2");
    asm("call "_"x3");
    callx4();
    asm("call "_"x5");
    callx5_again();
    x6();
    printf(" *\n");
    return 0;
}

static
int x2(void)
{
    printf(" x2");
    return 2;
}

extern int x3(void);

void x4(void)
{
    printf(" x4");
}

void x5(void);
void x5(void)
{
    printf(" x5");
}
