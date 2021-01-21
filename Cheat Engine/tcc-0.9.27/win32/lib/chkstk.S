/* ---------------------------------------------- */
/* chkstk86.s */

/* ---------------------------------------------- */
#ifndef __x86_64__
/* ---------------------------------------------- */

.globl __chkstk

__chkstk:
    xchg    (%esp),%ebp     /* store ebp, get ret.addr */
    push    %ebp            /* push ret.addr */
    lea     4(%esp),%ebp    /* setup frame ptr */
    push    %ecx            /* save ecx */
    mov     %ebp,%ecx
P0:
    sub     $4096,%ecx
    test    %eax,(%ecx)
    sub     $4096,%eax
    cmp     $4096,%eax
    jge     P0
    sub     %eax,%ecx
    test    %eax,(%ecx)

    mov     %esp,%eax
    mov     %ecx,%esp
    mov     (%eax),%ecx     /* restore ecx */
    jmp     *4(%eax)

/* ---------------------------------------------- */
#else
/* ---------------------------------------------- */

.globl __chkstk

__chkstk:
    xchg    (%rsp),%rbp     /* store ebp, get ret.addr */
    push    %rbp            /* push ret.addr */
    lea     8(%rsp),%rbp    /* setup frame ptr */
    push    %rcx            /* save ecx */
    mov     %rbp,%rcx
    movslq  %eax,%rax
P0:
    sub     $4096,%rcx
    test    %rax,(%rcx)
    sub     $4096,%rax
    cmp     $4096,%rax
    jge     P0
    sub     %rax,%rcx
    test    %rax,(%rcx)

    mov     %rsp,%rax
    mov     %rcx,%rsp
    mov     (%rax),%rcx     /* restore ecx */
    jmp     *8(%rax)

/* ---------------------------------------------- */
/* setjmp/longjmp support */

.globl tinyc_getbp
tinyc_getbp:
    mov %rbp,%rax
    ret

/* ---------------------------------------------- */
#endif
/* ---------------------------------------------- */


/* ---------------------------------------------- */
#ifndef __x86_64__
/* ---------------------------------------------- */

/*
    int _except_handler3(
       PEXCEPTION_RECORD exception_record,
       PEXCEPTION_REGISTRATION registration,
       PCONTEXT context,
       PEXCEPTION_REGISTRATION dispatcher
    );

    int __cdecl _XcptFilter(
       unsigned long xcptnum,
       PEXCEPTION_POINTERS pxcptinfoptrs
    );

    struct _sehrec {
       void *esp;                // 0
       void *exception_pointers; // 1
       void *prev;               // 2
       void *handler;            // 3
       void *scopetable;         // 4
       int trylevel;             // 5
       void *ebp                 // 6
    };

    // this is what the assembler code below means:
    __try
    {
         // ...
    }
    __except (_XcptFilter(GetExceptionCode(), GetExceptionInformation()))
    {
         exit(GetExceptionCode());
    }
*/

.globl _exception_info
_exception_info:
    mov 1*4-24(%ebp),%eax
    ret

.globl _exception_code
_exception_code:
    call _exception_info
    mov (%eax),%eax
    mov (%eax),%eax
    ret

seh_filter:
    call _exception_info
    push %eax
    call _exception_code
    push %eax
    call _XcptFilter
    add $ 8,%esp
    ret

seh_except:
    mov 0*4-24(%ebp),%esp
    call _exception_code
    push %eax
    call _exit

// msvcrt wants scopetables aligned and in read-only segment (using .text)
.align 4
seh_scopetable:
    .long -1
    .long seh_filter
    .long seh_except

seh_handler:
    jmp _except_handler3

.globl ___try__
___try__:
.globl __try__
__try__:
    push %ebp
    mov 8(%esp),%ebp

//    void *esp;
    lea 12(%esp),%eax
    mov %eax,0*4(%ebp)

//    void *exception_pointers;
    xor %eax,%eax
    mov %eax,1*4(%ebp)

//    void *prev;
    mov %fs:0,%eax
    mov %eax,2*4(%ebp)

//    void *handler;
    mov $ seh_handler,%eax
    mov %eax,3*4(%ebp)

//    void *scopetable;
    mov $ seh_scopetable,%eax
    mov %eax,4*4(%ebp)

//    int trylevel;
    xor %eax,%eax
    mov %eax,5*4(%ebp)

//    register new SEH
    lea 2*4(%ebp),%eax
    mov %eax,%fs:0

    pop %ebp
    ret

/* ---------------------------------------------- */
#else
/* ---------------------------------------------- */

/* SEH on x86-64 not implemented */

/* ---------------------------------------------- */
#endif
/* ---------------------------------------------- */
