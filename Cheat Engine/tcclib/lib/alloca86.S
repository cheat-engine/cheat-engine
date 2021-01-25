/* ---------------------------------------------- */
/* alloca86.S */

#ifdef __leading_underscore
# define _(s) _##s
#else
# define _(s) s
#endif

.globl _(alloca), _(__alloca)
_(alloca):
_(__alloca):
    push    %ebp
    mov     %esp,%ebp
    mov     8(%ebp),%eax
    add     $3,%eax
    and     $-4,%eax
#ifdef _WIN32
    jmp     .+16 #p2
p1:
    sub     $4096,%esp
    sub     $4096,%eax
    test    %eax,(%esp)
p2:
    cmp     $4096,%eax
    jae     p1
#endif
    sub     %eax,%esp
    mov     4(%ebp),%eax
    mov     0(%ebp),%ebp
    add     $8,%esp
    push    %eax
    lea     8(%esp),%eax
    ret

/* ---------------------------------------------- */
