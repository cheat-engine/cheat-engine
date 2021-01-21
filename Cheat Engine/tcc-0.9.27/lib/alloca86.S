/* ---------------------------------------------- */
/* alloca86.S */

.globl alloca

alloca:
    pop     %edx
    pop     %eax
    add     $3,%eax
    and     $-4,%eax
    jz      p3

#ifdef _WIN32
p1:
    cmp     $4096,%eax
    jbe     p2
    test    %eax,-4096(%esp)
    sub     $4096,%esp
    sub     $4096,%eax
    jmp p1
p2:
#endif

    sub     %eax,%esp
    mov     %esp,%eax
p3:
    push    %edx
    push    %edx
    ret

/* ---------------------------------------------- */
