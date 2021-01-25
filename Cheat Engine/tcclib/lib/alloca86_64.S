/* ---------------------------------------------- */
/* alloca86_64.S */

#ifdef __leading_underscore
# define _(s) _##s
#else
# define _(s) s
#endif

.globl _(alloca)
_(alloca):
    pop     %rdx
#ifdef _WIN32
    mov     %rcx,%rax
#else
    mov     %rdi,%rax
#endif
    add     $15,%rax
    and     $-16,%rax
    jz      p3

#ifdef _WIN32
p1:
    cmp     $4096,%rax
    jbe     p2
    test    %rax,-4096(%rsp)
    sub     $4096,%rsp
    sub     $4096,%rax
    jmp p1
p2:
#endif
    sub     %rax,%rsp
    mov     %rsp,%rax
p3:
    push    %rdx
    ret

/* ---------------------------------------------- */
