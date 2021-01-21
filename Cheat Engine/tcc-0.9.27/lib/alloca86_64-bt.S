/* ---------------------------------------------- */
/* alloca86_64.S */

.globl __bound_alloca
__bound_alloca:

#ifdef _WIN32
    # bound checking is not implemented
    pop     %rdx
    mov     %rcx,%rax
    add     $15,%rax
    and     $-16,%rax
    jz      p3

p1:
    cmp     $4096,%rax
    jbe     p2
    test    %rax,-4096(%rsp)
    sub     $4096,%rsp
    sub     $4096,%rax
    jmp p1
p2:

    sub     %rax,%rsp
    mov     %rsp,%rax
    add     $32,%rax

p3:
    push    %rdx
    ret
#else
    pop     %rdx
    mov     %rdi,%rax
    mov     %rax,%rsi	# size, a second parm to the __bound_new_region

    add     $15,%rax
    and     $-16,%rax
    jz      p3


    sub     %rax,%rsp
    mov     %rsp,%rdi	# pointer, a first parm to the __bound_new_region
    mov     %rsp,%rax

    push    %rdx
    push    %rax
    call   __bound_new_region
    pop     %rax
    pop     %rdx

p3:
    push    %rdx
    ret
#endif

/* ---------------------------------------------- */
