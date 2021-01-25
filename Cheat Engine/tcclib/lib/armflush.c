/* armflush.c - flush the instruction cache

   __clear_cache is used in tccrun.c,  It is a built-in
   intrinsic with gcc.  However tcc in order to compile
   itself needs this function */

#ifdef __TINYC__

/* syscall wrapper */
unsigned _tccsyscall(unsigned syscall_nr, ...);

/* arm-tcc supports only fake asm currently */
__asm__(
    ".global _tccsyscall\n"
    "_tccsyscall:\n"
    ".int 0xe92d4080\n"  // push    {r7, lr}
    ".int 0xe1a07000\n"  // mov     r7, r0
    ".int 0xe1a00001\n"  // mov     r0, r1
    ".int 0xe1a01002\n"  // mov     r1, r2
    ".int 0xe1a02003\n"  // mov     r2, r3
    ".int 0xef000000\n"  // svc     0x00000000
    ".int 0xe8bd8080\n"  // pop     {r7, pc}
    );

/* from unistd.h: */
#if defined(__thumb__) || defined(__ARM_EABI__)
# define __NR_SYSCALL_BASE      0x0
#else
# define __NR_SYSCALL_BASE      0x900000
#endif
#define __ARM_NR_BASE           (__NR_SYSCALL_BASE+0x0f0000)
#define __ARM_NR_cacheflush     (__ARM_NR_BASE+2)

#define syscall _tccsyscall

#else

#define _GNU_SOURCE
#include <unistd.h>
#include <sys/syscall.h>
#include <stdio.h>

#endif

/* Flushing for tccrun */
void __clear_cache(void *beginning, void *end)
{
/* __ARM_NR_cacheflush is kernel private and should not be used in user space.
 * However, there is no ARM asm parser in tcc so we use it for now */
#if 1
    syscall(__ARM_NR_cacheflush, beginning, end, 0);
#else
    __asm__ ("push {r7}\n\t"
             "mov r7, #0xf0002\n\t"
             "mov r2, #0\n\t"
             "swi 0\n\t"
             "pop {r7}\n\t"
             "ret");
#endif
}
