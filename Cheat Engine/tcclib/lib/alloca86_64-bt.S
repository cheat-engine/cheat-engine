/* ---------------------------------------------- */
/* alloca86_64.S */

#ifdef __leading_underscore
# define _(s) _##s
#else
# define _(s) s
#endif

.globl _(__bound_alloca)
_(__bound_alloca):
#ifdef _WIN32
    inc %rcx            # add one extra to separate regions
    jmp _(alloca)
.globl _(__bound_alloca_nr)
_(__bound_alloca_nr):
    dec     %rcx
    push    %rax
    mov     %rcx,%rdx
    mov     %rax,%rcx
    sub     $32,%rsp
    call    _(__bound_new_region)
    add     $32,%rsp
    pop     %rax
    ret
#else
    pop     %rdx
    mov     %rdi,%rax
    mov     %rax,%rsi	# size, a second parm to the __bound_new_region

    add     $15 + 1,%rax  # add one extra to separate regions
    and     $-16,%rax
    jz      p3


    sub     %rax,%rsp
    mov     %rsp,%rdi	# pointer, a first parm to the __bound_new_region
    mov     %rsp,%rax

    push    %rdx
    push    %rax
    call    _(__bound_new_region)
    pop     %rax
    pop     %rdx

p3:
    push    %rdx
    ret
#endif

/* ---------------------------------------------- */
