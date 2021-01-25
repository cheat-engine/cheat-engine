#include <stdio.h>
#include <assert.h>

#if !(defined( _WIN32) || (defined(__FreeBSD__) && defined(__i386__)))
#define __fastcall __attribute((fastcall))
#endif

#ifndef __leading_underscore
#define SYMBOL(x) _##x
#else
#define SYMBOL(x) x
#endif

/////////////////////////////////////////////////////////////////////////
//////////                TRAP FRAMEWORK
/////////////////////////////////////////////////////////////////////////
// if you cast 'TRAP' to a function pointer and call it,
//   it will save all 8 registers,
//   and jump into C-code (previously set using 'SET_TRAP_HANDLER(x)'),
//   in C-code you can pop DWORDs from stack and modify registers
//

void *SYMBOL(trap_handler);

extern unsigned char SYMBOL(trap)[];
asm (
    ".text;"
    "_trap:;"
    "pushl %esp;"
    "pusha;"
    "addl $0x4, 0xc(%esp);"
    "pushl %esp;"
    "call *_trap_handler;"
    "addl $0x4, %esp;"
    "movl 0xc(%esp), %eax;"
    "movl %eax, 0x20(%esp);"
    "popa;"
    "popl %esp;"
	"ret;"
);

struct trapframe {
    unsigned edi, esi, ebp, esp, ebx, edx, ecx, eax;
};


#define M_FLOAT(addr) (*(float *)(addr))
#define M_DWORD(addr) (*(unsigned *)(addr))
#define M_WORD(addr) (*(unsigned short *)(addr))
#define M_BYTE(addr) (*(unsigned char *)(addr))
#define R_EAX ((tf)->eax)
#define R_ECX ((tf)->ecx)
#define R_EDX ((tf)->edx)
#define R_EBX ((tf)->ebx)
#define R_ESP ((tf)->esp)
#define R_EBP ((tf)->ebp)
#define R_ESI ((tf)->esi)
#define R_EDI ((tf)->edi)

#define ARG(x) (M_DWORD(R_ESP + (x) * 4))

#define RETN(x) do { \
    M_DWORD(R_ESP + (x)) = M_DWORD(R_ESP); \
    R_ESP += (x); \
} while (0)

#define DUMP() do { \
    unsigned i; \
    printf("EAX: %08X\n", R_EAX); \
    printf("ECX: %08X\n", R_ECX); \
    printf("EDX: %08X\n", R_EDX); \
    printf("EBX: %08X\n", R_EBX); \
    printf("ESP: %08X\n", R_ESP); \
    printf("EBP: %08X\n", R_EBP); \
    printf("ESI: %08X\n", R_ESI); \
    printf("EDI: %08X\n", R_EDI); \
    printf("\n"); \
    printf("[RETADDR]: %08X\n", M_DWORD(R_ESP)); \
    for (i = 1; i <= 8; i++) { \
        printf("[ARG%4d]: %08X\n", i, ARG(i)); \
    } \
} while (0)

#define SET_TRAP_HANDLER(x) ((SYMBOL(trap_handler)) = (x))
#define TRAP ((void *) &SYMBOL(trap))



/////////////////////////////////////////////////////////////////////////
//////////                SAFECALL FRAMEWORK
/////////////////////////////////////////////////////////////////////////
// this framework will convert any calling convention to cdecl
// usage: first set call target with 'SET_SAFECALL_TARGET(x)'
//        then cast 'SAFECALL' to target function pointer type and invoke it
//        after calling, 'ESPDIFF' is the difference of old and new esp

void *SYMBOL(sc_call_target);
unsigned SYMBOL(sc_retn_addr);
unsigned SYMBOL(sc_old_esp);
unsigned SYMBOL(sc_new_esp);

extern unsigned char SYMBOL(safecall)[];
asm (
    ".text;"
    "_safecall:;"
    "popl _sc_retn_addr;"
    "movl %esp, _sc_old_esp;"
    "call *_sc_call_target;"
    "movl %esp, _sc_new_esp;"
    "movl _sc_old_esp, %esp;"
	"jmp *_sc_retn_addr;"
);

#define SET_SAFECALL_TARGET(x) ((SYMBOL(sc_call_target)) = (x))
#define SAFECALL ((void *) &SYMBOL(safecall))
#define ESPDIFF (SYMBOL(sc_new_esp) - SYMBOL(sc_old_esp))


/////////////////////////////////////////////////////////////////////////
//////////                TEST FASTCALL INVOKE
/////////////////////////////////////////////////////////////////////////

void check_fastcall_invoke_0(struct trapframe *tf)
{
    //DUMP();
    RETN(0);
}

void check_fastcall_invoke_1(struct trapframe *tf)
{
    //DUMP();
    assert(R_ECX == 0x11111111);
    RETN(0);
}
void check_fastcall_invoke_2(struct trapframe *tf)
{
    //DUMP();
    assert(R_ECX == 0x11111111);
    assert(R_EDX == 0x22222222);
    RETN(0);
}
void check_fastcall_invoke_3(struct trapframe *tf)
{
    //DUMP();
    assert(R_ECX == 0x11111111);
    assert(R_EDX == 0x22222222);
    assert(ARG(1) == 0x33333333);
    RETN(1*4);
}
void check_fastcall_invoke_4(struct trapframe *tf)
{
    //DUMP();
    assert(R_ECX == 0x11111111);
    assert(R_EDX == 0x22222222);
    assert(ARG(1) == 0x33333333);
    assert(ARG(2) == 0x44444444);
    RETN(2*4);
}

void check_fastcall_invoke_5(struct trapframe *tf)
{
    //DUMP();
    assert(R_ECX == 0x11111111);
    assert(R_EDX == 0x22222222);
    assert(ARG(1) == 0x33333333);
    assert(ARG(2) == 0x44444444);
    assert(ARG(3) == 0x55555555);
    RETN(3*4);
}

void test_fastcall_invoke()
{
    SET_TRAP_HANDLER(check_fastcall_invoke_0);
    ((void __fastcall (*)(void)) TRAP)();

    SET_TRAP_HANDLER(check_fastcall_invoke_1);
    ((void __fastcall (*)(unsigned)) TRAP)(0x11111111);

    SET_TRAP_HANDLER(check_fastcall_invoke_2);
    ((void __fastcall (*)(unsigned, unsigned)) TRAP)(0x11111111, 0x22222222);

    SET_TRAP_HANDLER(check_fastcall_invoke_3);
    ((void __fastcall (*)(unsigned, unsigned, unsigned)) TRAP)(0x11111111, 0x22222222, 0x33333333);

    SET_TRAP_HANDLER(check_fastcall_invoke_4);
    ((void __fastcall (*)(unsigned, unsigned, unsigned, unsigned)) TRAP)(0x11111111, 0x22222222, 0x33333333, 0x44444444);

    SET_TRAP_HANDLER(check_fastcall_invoke_5);
    ((void __fastcall (*)(unsigned, unsigned, unsigned, unsigned, unsigned)) TRAP)(0x11111111, 0x22222222, 0x33333333, 0x44444444, 0x55555555);
}


/////////////////////////////////////////////////////////////////////////
//////////                TEST FUNCTION CODE GENERATION
/////////////////////////////////////////////////////////////////////////

int __fastcall check_fastcall_espdiff_0(void)
{
    return 0;
}

int __fastcall check_fastcall_espdiff_1(int a)
{
    return a;
}

int __fastcall check_fastcall_espdiff_2(int a, int b)
{
    return a + b;
}

int __fastcall check_fastcall_espdiff_3(int a, int b, int c)
{
    return a + b + c;
}

int __fastcall check_fastcall_espdiff_4(int a, int b, int c, int d)
{
    return a + b + c + d;
}

int __fastcall check_fastcall_espdiff_5(int a, int b, int c, int d, int e)
{
    return a + b + c + d + e;
}

void test_fastcall_espdiff()
{
    int x;
    SET_SAFECALL_TARGET(check_fastcall_espdiff_0);
    x = ((typeof(&check_fastcall_espdiff_0))SAFECALL)();
    assert(x == 0);
    assert(ESPDIFF == 0);

    SET_SAFECALL_TARGET(check_fastcall_espdiff_1);
    x = ((typeof(&check_fastcall_espdiff_1))SAFECALL)(1);
    assert(x == 1);
    assert(ESPDIFF == 0);

    SET_SAFECALL_TARGET(check_fastcall_espdiff_2);
    x = ((typeof(&check_fastcall_espdiff_2))SAFECALL)(1, 2);
    assert(x == 1 + 2);
    assert(ESPDIFF == 0);

    SET_SAFECALL_TARGET(check_fastcall_espdiff_3);
    x = ((typeof(&check_fastcall_espdiff_3))SAFECALL)(1, 2, 3);
    assert(x == 1 + 2 + 3);
    assert(ESPDIFF == 1*4);

    SET_SAFECALL_TARGET(check_fastcall_espdiff_4);
    x = ((typeof(&check_fastcall_espdiff_4))SAFECALL)(1, 2, 3, 4);
    assert(x == 1 + 2 + 3 + 4);
    assert(ESPDIFF == 2*4);

    SET_SAFECALL_TARGET(check_fastcall_espdiff_5);
    x = ((typeof(&check_fastcall_espdiff_5))SAFECALL)(1, 2, 3, 4, 5);
    assert(x == 1 + 2 + 3 + 4 + 5);
    assert(ESPDIFF == 3*4);
}

int main()
{
#define N 10000
    int i;

    for (i = 1; i <= N; i++) {
        test_fastcall_espdiff();
    }

    for (i = 1; i <= N; i++) {
        test_fastcall_invoke();
    }

    puts("TEST OK");
    return 0;
}
