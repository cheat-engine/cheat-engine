/* ---------------------------------------------- */
/* alloca86-bt.S */

#ifdef __leading_underscore
# define _(s) _##s
#else
# define _(s) s
#endif

.globl _(__bound_alloca)
_(__bound_alloca):
    pop     %edx
    pop     %eax
    mov     %eax, %ecx
    add     $3+1,%eax
    and     $-4,%eax
    jz      p6

#ifdef _WIN32
p4:
    cmp     $4096,%eax
    jbe     p5
    test    %eax,-4096(%esp)
    sub     $4096,%esp
    sub     $4096,%eax
    jmp p4

p5:
#endif

    sub     %eax,%esp
    mov     %esp,%eax

    push    %edx
    push    %eax
    push    %ecx
    push    %eax
    call    _(__bound_new_region)
    add     $8, %esp
    pop     %eax
    pop     %edx

p6:
    push    %edx
    push    %edx
    ret

/* mark stack as nonexecutable */
#if defined __ELF__ && defined __linux__
    .section    .note.GNU-stack,"",@progbits
#endif
/* ---------------------------------------------- */
