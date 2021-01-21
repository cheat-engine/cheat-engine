# gas comment with ``gnu'' style quotes

/* some directive tests */

   .byte 0xff
   .byte 1, 2, 3
   .short 1, 2, 3
   .word 1, 2, 3
   .long 1, 2, 3
   .int 1, 2, 3
   .align 8
   .byte 1
/* .align 16, 0x90 gas is too clever for us with 0x90 fill */
   .balign 4, 0x92
   .align 16, 0x91 /* 0x91 tests the non-clever behaviour */
   .skip 3
   .skip 15, 0x90
   .string "hello\0world"
/* Macro expansion should work like with C, the #n shouldn't be parsed
   as asm line comment */
#define __stringify(n) #n
#define stringify(n) __stringify(n)
   .skip 8,0x90
   .asciz stringify(BLA)
   .skip 8,0x90

# 28 "asmtest.S"        # a line directive (and a line comment)
        movl %eax, %ebx # some more asm comment
/* some label tests */
L1:
        movl %eax, %ebx
        mov 0x10000, %eax
L2:
        movl $L2 - L1, %ecx
var1:
        nop ; nop ; nop ; nop

        mov var1, %eax

/* instruction tests */
movl %eax, %ebx
mov 0x10000, %eax
mov 0x10000, %ax
mov 0x10000, %al
mov %al, 0x10000
                
mov $1, %edx
mov $1, %dx
mov $1, %cl
movb $2, 0x100(%ebx,%edx,2)
movw $2, 0x100(%ebx,%edx,2)
movl $2, 0x100(%ebx,%edx,2)
movl %eax, 0x100(%ebx,%edx,2)
movl 0x100(%ebx,%edx,2), %edx
movw %ax, 0x100(%ebx,%edx,2)

movw $0x1122,%si
movl $0x112233,%edx
movl $0x80000000, %esi
movl $-0x7fffffff, %edi
#ifdef __x86_64__
mov $0x11223344,%rbx
movq $0x11223344,%rbx
mov $0x1122334455,%rbx
movq $0x1122334455,%rbx
movl $0x11334455,(%rbx)
#endif

mov %eax, 0x12(,%edx,2)
        
#ifdef __i386__
mov %cr3, %edx
mov %ecx, %cr3
movl %cr3, %eax
movl %tr3, %eax
movl %db3, %ebx
movl %dr6, %eax
#else
mov %cr3, %rdx
mov %rcx, %cr3
movq %cr3, %rax
movq %db3, %rbx
movq %dr6, %rax
mov %cr8, %rsi
mov %rdi, %cr8
#endif
movl %fs, %ecx
movl %ebx, %fs

#ifdef __x86_64__
movq %r8, %r9
movq %r10, %r11
movq %r12, %r13
movq %r14, %r15
movq %rax, %r9
movq %r15, %rsi
inc %r9b
dec %r10w
not %r11d
negq %r12
decb %r13b
incw %r14w
notl %r15d
#endif

     movsbl 0x1000, %eax
     movsbw 0x1000, %ax
     movswl 0x1000, %eax

     movzbl 0x1000, %eax
     movzbw 0x1000, %ax
     movzwl 0x1000, %eax
            
     movzb 0x1000, %eax
     movzb 0x1000, %ax
                
     mov $0x12345678,%eax

#ifdef __x86_64__
     movzb 0x1000, %rax
     movzbq 0x1000, %rbx
     movsbq 0x1000, %rdx
     movzwq 0x1000, %rdi
     movswq 0x1000, %rdx
     movslq %eax, %rcx
     mov $0x12345678,%rax
     mov $0x12345678,%rdx
     mov $0x12345678,%r10
     mov $0x123456789abcdef0,%rax
     mov $0x123456789abcdef0,%rcx
     mov $0x123456789abcdef0,%r11
#endif
        
#ifdef __i386__
  pushl %eax
  push %eax
  push %cs
#else
  pushq %rax
  push %rax
#endif
  pushw %ax
  push %gs
  push $1
  push $100
  push 0x42(%eax)
  pop 0x43(%esi)
                                                
#ifdef __i386__
  popl %eax
  pop %eax
  pop %ds
#else
  popq %rax
  pop %rax
#endif
  popw %ax
  pop %fs
          
  xchg %eax, %ecx
  xchg %edx, %eax
  xchg %bx, 0x10000
  xchg 0x10000, %ebx
  xchg 0x10000, %dl

  in $100, %al               
  in $100, %ax               
  in $100, %eax
  in %dx, %al
  in %dx, %ax               
  in %dx, %eax
  inb %dx
  inw %dx               
  inl %dx

  out %al, $100                       
  out %ax, $100                       
  out %eax, $100                       

  /* NOTE: gas is bugged here, so size must be added */
  outb %al, %dx                       
  outw %ax, %dx                       
  outl %eax, %dx                       

  leal 0x1000(%ebx), %ecx
  lea 0x1000(%ebx), %ecx

#ifdef __i386__
  les 0x2000, %eax
  lds 0x2000, %ebx
  lss 0x2000, %edx
#endif
  lfs 0x2000, %ecx
  lgs 0x2000, %edx

addl $0x123, %eax
add $0x123, %ebx
add $-16, %ecx
add $-0x123, %esi
add $1, %bx
add $1, %ebx
add $-1, %bx
add $-1, %ebx
add $127, %bx
addl $127, %ebx
addl $-128, %ebx
addl $-128, %ebx
addl $-129, %ebx
addl $128, %ebx
addl $255, %ebx
addl $256, %ebx
andb $0xf, %ah
andb $-15, %cl
xorb $127, %dh
cmpb $42, (%eax)
addl $0x123, 0x100
addl $0x123, 0x100(%ebx)
addl $0x123, 0x100(%ebx,%edx,2)
addl $0x123, 0x100(%esp)
addl $0x123, (3*8)(%esp)
addl $0x123, (%ebp)
addl $0x123, (%esp)
cmpl $0x123, (%esp)

#ifdef __x86_64__
xor %bl,%ah
xor %bl,%r8b
xor %r9b,%bl
xor %sil,%cl
add %eax,(%r8d)
add %ebx,(%r9)
add %edx,(%r10d,%r11d)
add %ecx,(%r12,%r13)
add %esi,(%r14,%r15,4)
add %edi,0x1000(%rbx,%r12,8)
add %r11,0x1000(%ebp,%r9d,8)
movb $12, %ah
movb $13, %bpl
movb $14, %dil
movb $15, %r12b
#endif

add %eax, (%ebx)
add (%ebx), %eax
                
or %dx, (%ebx)
or (%ebx), %si
        
add %cl, (%ebx)
add (%ebx), %dl

    inc %edx
    incl 0x10000
    incb 0x10000
    dec %dx
  
  test $1, %al
  test $1, %cl

  testl $1, 0x1000
  testb $1, 0x1000
  testw $1, 0x1000
  test %eax, %ebx
  test %eax, 0x1000
  test 0x1000, %edx

    not %edx
    notw 0x10000
    notl 0x10000
    notb 0x10000

    neg %edx
    negw 0x10000
    negl 0x10000
    negb 0x10000

    imul %ecx
    mul %edx
    mulb %cl

    imul %eax, %ecx
    imul 0x1000, %cx
    imul $10, %eax, %ecx
    imul $10, %ax, %cx
    imul $10, %eax
    imul $0x1100000, %eax
    imul $1, %eax
    
    idivw 0x1000
    div %ecx
    div %bl
    div %ecx, %eax

and $15,%bx
and $-20,%edx

shl %edx
shl $10, %edx
shl %cl, %edx

shld $1, %eax, %edx
shld %cl, %eax, %edx
shld %eax, %edx

shrd $1, %eax, %edx
shrd %cl, %eax, %edx
shrd %eax, %edx

L4:
call 0x1000
call L4
#ifdef __i386__
call *%eax
#else
call *%rax
#endif
call *0x1000
call func1

.global L5,L6

L5:
L6:

#ifdef __i386__
lcall $0x100, $0x1000
#else
lcall *0x100
lcall *(%rax)
#endif

jmp 0x1000
jmp *(%edi)
#ifdef __i386__
jmp *%eax
#else
jmp *%rax
#endif
jmp *0x1000

#ifdef __i386__
ljmp $0x100, $0x1000
#else
ljmp *0x100
ljmp *(%rdi)
ljmpl *(%esi)
ljmpw *(%esi)
#endif

ret
ret $10
#ifdef __i386__
retl
retl $10
#else
retq
retq $10
#endif

lret

lret $10

enter $1234, $10

L3:
 jo 0x1000
 jnp 0x1001
 jne 0x1002
 jg 0x1003

 jo L3
 jnp L3
 jne L3
 jg L3

 loopne L3
 loopnz L3
 loope L3
 loopz L3
 loop L3
 jecxz L3

        
 seto %al
 setc %al
 setcb %al
 setnp 0x1000
 setl 0xaaaa
 setg %dl

 fadd
 fadd %st(1), %st
 fadd %st(0), %st(1)
 fadd %st(3)

 fmul %st(0),%st(0)
 fmul %st(0),%st(1)

 faddp %st(5)
 faddp
 faddp %st(1), %st

 fadds 0x1000
 fiadds 0x1002
 faddl 0x1004
 fiaddl 0x1006

 fmul
 fmul %st(1), %st
 fmul %st(3)

 fmulp %st(5)
 fmulp
 fmulp %st(1), %st

 fmuls 0x1000
 fimuls 0x1002
 fmull 0x1004
 fimull 0x1006

 fsub
 fsub %st(1), %st
 fsub %st(3)

 fsubp %st(5)
 fsubp
 fsubp %st(1), %st

 fsubs 0x1000
 fisubs 0x1002
 fsubl 0x1004
 fisubl 0x1006

 fsubr
 fsubr %st(1), %st
 fsubr %st(3)

 fsubrp %st(5)
 fsubrp
 fsubrp %st(1), %st

 fsubrs 0x1000
 fisubrs 0x1002
 fsubrl 0x1004
 fisubrl 0x1006

 fdiv
 fdiv %st(1), %st
 fdiv %st(3)

 fdivp %st(5)
 fdivp
 fdivp %st(1), %st

 fdivs 0x1000
 fidivs 0x1002
 fdivl 0x1004
 fidivl 0x1006

 fcom %st(3)

 fcoms 0x1000
 ficoms 0x1002
 fcoml 0x1004
 ficoml 0x1006

 fcomp %st(5)
 fcomp
 fcompp

 fcomps 0x1000
 ficomps 0x1002
 fcompl 0x1004
 ficompl 0x1006

 fld %st(5)
 fldl 0x1000
 flds 0x1002
 fildl 0x1004
 fst %st(4)
 fstp %st(6)
 fstpt 0x1006
 fbstp 0x1008

 fxch
 fxch %st(4)

 fucom %st(6)
 fucomp %st(3)
 fucompp

 finit
 fninit
 fldcw 0x1000
 fnstcw 0x1002
 fstcw 0x1002
 fnstsw 0x1004
 fnstsw (%eax)
 fstsw 0x1004
 fstsw (%eax)
 fnclex
 fclex
 fnstenv 0x1000
 fstenv 0x1000
 fldenv 0x1000
 fnsave 0x1002
 fsave 0x1000
 frstor 0x1000
 ffree %st(7)
 ffreep %st(6)
 
    ftst
    fxam
    fld1
    fldl2t
    fldl2e
    fldpi
    fldlg2
    fldln2
    fldz

    f2xm1
    fyl2x
    fptan
    fpatan
    fxtract
    fprem1
    fdecstp
    fincstp
    fprem
    fyl2xp1
    fsqrt
    fsincos
    frndint
    fscale
    fsin
    fcos
    fchs
    fabs
    fnop
    fwait

bswap %edx
bswapl %ecx
xadd %ecx, %edx
xaddb %dl, 0x1000
xaddw %ax, 0x1000
xaddl %eax, 0x1000
cmpxchg %ecx, %edx
cmpxchgb %dl, 0x1000
cmpxchgw %ax, 0x1000
cmpxchgl %eax, 0x1000
invlpg 0x1000
cmpxchg8b 0x1002
#ifdef __x86_64__
cmpxchg16b (%rax)
cmpxchg16b (%r10,%r11)
#endif

fcmovb %st(5), %st
fcmove %st(5), %st
fcmovbe %st(5), %st
fcmovu %st(5), %st
fcmovnb %st(5), %st
fcmovne %st(5), %st
fcmovnbe %st(5), %st
fcmovnu %st(5), %st
fcomi %st(5), %st
fucomi %st(5), %st
fcomip %st(5), %st
fucomip %st(5), %st



 cmovo 0x1000, %eax
 cmovs 0x1000, %eax
 cmovns %edx, %edi
 cmovne %ax, %si
 cmovbw %ax, %di
 cmovnbel %edx, %ecx
#ifdef __x86_64__
 bswapq %rsi
 bswapq %r10
 cmovz %rdi,%rbx
 cmovpeq %rsi, %rdx
#endif

int $3
int $0x10

#ifdef __i386__
    pusha
    popa
#endif
    clc # another comment
    cld # a comment with embedded ' tick
    cli
    clts
    cmc
    lahf
    sahf
#ifdef __i386__
    pushfl
    popfl
#else
    pushfq
    popfq
#endif
    pushf
    popf
    stc
    std
    sti
#ifdef __i386__
    aaa
    aas
    daa
    das
    aad
    aam
    into
#endif
    cbw
    cwd
    cwde
    cdq
    cbtw
    cwtd
    cwtl
    cltd
    leave
    int3
    iret
    rsm
    hlt
    wait
    nop

    /* XXX: handle prefixes */
#if 0
    aword
    addr16
#endif
    lock
    rep
    repe
    repz
    repne
    repnz
    nop

    lock ;negl (%eax)
    wait ;pushf
    rep  ;stosb
    repe ;lodsb
    repz ;cmpsb
    repne;movsb
    repnz;outsb

    /* handle one-line prefix + ops */
    lock  negl (%eax)
    wait  pushf
    rep   stosb
    repe  lodsb
    repz  cmpsb
    repne movsb
    repnz outsb
    
    invd
    wbinvd
    cpuid
    wrmsr
    rdtsc
    rdmsr
    rdpmc
    ud2
#ifdef __x86_64__
    syscall
    sysret
    sysretq
    lfence
    mfence
    sfence
    prefetchnta 0x18(%rdx)
    prefetcht0 (%rcx)
    prefetcht1 (%rsi)
    prefetcht2 (%rdi)
    prefetchw (%rdi)
    clflush 0x1000(%rax,%rcx)
    fxsaveq (%rdx)
    fxsaveq (%r11)
    fxrstorq (%rcx)
    fxrstorq (%r10)

#endif

    lar %ax,%dx
    lar %eax,%dx
    lar %ax,%edx
    lar %eax,%edx
#ifdef __x86_64__
    lar %ax,%rdx
    lar %eax,%rdx
#endif
    emms
    movd %edx, %mm3
    movd 0x1000, %mm2
    movd %mm4, %ecx
    movd %mm5, 0x1000
                    
    movq 0x1000, %mm2
    movq %mm4, 0x1000
    
    pand 0x1000, %mm3
    pand %mm4, %mm5
    
    psllw $1, %mm6
    psllw 0x1000, %mm7
    psllw %mm2, %mm7

    xlat
    cmpsb
    scmpw
    insl
    outsw
    lodsb
    slodl
    movsb
    movsl
    smovb
    scasb
    sscaw
    stosw
    sstol

    bsf 0x1000, %ebx
    bsr 0x1000, %ebx
    bt %edx, 0x1000
    btl $2, 0x1000
    btc %edx, 0x1000
    btcl $2, 0x1000
    btr %edx, 0x1000
    btrl $2, 0x1000
    bts %edx, 0x1000
    btsl $2, 0x1000

        
        
#ifdef __i386__
    boundl %edx, 0x10000
    boundw %bx, 0x1000

    arpl %bx, 0x1000
#endif
    lar 0x1000, %eax
    lgdt 0x1000
    lidt 0x1000
    lldt 0x1000
    sgdt 0x1000
    sidt 0x1000
    sldt 0x1000
#ifdef __x86_64__
    lgdtq 0x1000
    lidtq 0x1000
    sgdtq 0x1000
    sidtq 0x1000

    swapgs

    str %rdx
    str %r9
#endif

    lmsw 0x1000
    lsl 0x1000, %ecx
    ltr 0x1000
    ltr %si
    smsw 0x1000
    str 0x1000
    str %ecx
    str %dx
    
    verr 0x1000
    verw 0x1000
  
#ifdef __i386__
    push %ds
    pushw %ds
    pushl %ds
    pop %ds
    popw %ds
    popl %ds
#endif
    fxsave 1(%ebx)
    fxrstor 1(%ecx)
#ifdef __i386__
    pushl $1
#else
    pushq $1
#endif
    pushw $1
    push $1

#ifdef __ASSEMBLER__ // should be defined, for S files
    inc %eax
#endif

#ifndef _WIN32
ft1: ft2: ft3: ft4: ft5: ft6: ft7: ft8: ft9:
    xor %eax, %eax
    ret

.type ft1,STT_FUNC
.type ft2,@STT_FUNC
.type ft3,%STT_FUNC
.type ft4,"STT_FUNC"
.type ft5,function
.type ft6,@function
.type ft7,%function
.type ft8,"function"
#endif

    pause
.rept 6
    nop
.endr
.fill 4,1,0x90

.section .text.one,"ax"
nop
.previous
.pushsection .text.one,"ax"
nop
.pushsection .text.two,"ax"
nop
.popsection
.popsection

1: ud2
.pushsection __bug_table,"a"
.align 8
2: .long 1b - 2b
   .long 0x600000 - 2b
   .long 1b + 42
   .long 43 + 1b
   .long 2b + 144
   .long 145 + 2b
   .word 164, 0
   .org 2b+32
#ifdef __x86_64__
   .quad 1b
#else
   .long 1b
#endif
.popsection
3: mov %eax,%ecx
4:
.pushsection .text.three, "ax"
nop
.skip (-((4b-3b) > 0) * 2) , 0x90
.popsection

.globl overrideme
.weak overrideme
  nop
.globl notimplemented
notimplemented:
  ret
.set overrideme, notimplemented
overrideme = notimplemented
overrideme:
  ret

    movd %esi, %mm1
    movd %edi, %xmm2
    movd (%ebx), %mm3
    movd (%ebx), %xmm3
    movd %mm1, %esi
    movd %xmm2, %edi
    movd %mm3, (%edx)
    movd %xmm3, (%edx)
#ifdef __x86_64__
    movd %rsi, %mm1
    movd %rdi, %xmm2
    movd (%rbx), %mm3
    movd (%rbx), %xmm3
    movd %mm1, %r12
    movd %xmm2, %rdi
    movd %mm3, (%r8)
    movd %xmm3, (%r13)
#endif

    movq (%ebp), %mm1
    movq %mm2, (%edi)
    movq (%edi), %xmm3
    movq %mm4, %mm5
#ifdef __x86_64__
    movq %rcx, %mm1
    movq %rdx, %xmm2
    movq %r13, %xmm3
    /* movq mem64->xmm is encoded as f30f7e by GAS, but as
       660f6e by tcc (which really is a movd and would need 
       a REX.W prefix to be movq).  */
    movq (%rsi), %xmm3
    movq %mm1, %rdx
    movq %xmm3, %rcx
    movq %xmm4, (%rsi)
#endif

#define TEST_MMX_SSE(insn) \
    insn %mm1, %mm2; \
    insn %xmm2, %xmm3; \
    insn (%ebx), %xmm3;
#define TEST_MMX_SSE_I8(insn) \
    TEST_MMX_SSE(insn) \
    insn $0x42, %mm4; \
    insn $0x42, %xmm4;

    TEST_MMX_SSE(packssdw)
    TEST_MMX_SSE(packsswb)
    TEST_MMX_SSE(packuswb)
    TEST_MMX_SSE(paddb)
    TEST_MMX_SSE(paddw)
    TEST_MMX_SSE(paddd)
    TEST_MMX_SSE(paddsb)
    TEST_MMX_SSE(paddsw)
    TEST_MMX_SSE(paddusb)
    TEST_MMX_SSE(paddusw)
    TEST_MMX_SSE(pand)
    TEST_MMX_SSE(pandn)
    TEST_MMX_SSE(pcmpeqb)
    TEST_MMX_SSE(pcmpeqw)
    TEST_MMX_SSE(pcmpeqd)
    TEST_MMX_SSE(pcmpgtb)
    TEST_MMX_SSE(pcmpgtw)
    TEST_MMX_SSE(pcmpgtd)
    TEST_MMX_SSE(pmaddwd)
    TEST_MMX_SSE(pmulhw)
    TEST_MMX_SSE(pmullw)
    TEST_MMX_SSE(por)
    TEST_MMX_SSE(psllw)
TEST_MMX_SSE_I8(psllw)
    TEST_MMX_SSE(pslld)
TEST_MMX_SSE_I8(pslld)
    TEST_MMX_SSE(psllq)
TEST_MMX_SSE_I8(psllq)
    TEST_MMX_SSE(psraw)
TEST_MMX_SSE_I8(psraw)
    TEST_MMX_SSE(psrad)
TEST_MMX_SSE_I8(psrad)
    TEST_MMX_SSE(psrlw)
TEST_MMX_SSE_I8(psrlw)
    TEST_MMX_SSE(psrld)
TEST_MMX_SSE_I8(psrld)
    TEST_MMX_SSE(psrlq)
TEST_MMX_SSE_I8(psrlq)
    TEST_MMX_SSE(psubb)
    TEST_MMX_SSE(psubw)
    TEST_MMX_SSE(psubd)
    TEST_MMX_SSE(psubsb)
    TEST_MMX_SSE(psubsw)
    TEST_MMX_SSE(psubusb)
    TEST_MMX_SSE(psubusw)
    TEST_MMX_SSE(punpckhbw)
    TEST_MMX_SSE(punpckhwd)
    TEST_MMX_SSE(punpckhdq)
    TEST_MMX_SSE(punpcklbw)
    TEST_MMX_SSE(punpcklwd)
    TEST_MMX_SSE(punpckldq)
    TEST_MMX_SSE(pxor)

    cvtpi2ps %mm1, %xmm2
    cvtpi2ps (%ebx), %xmm2
    TEST_MMX_SSE(pmaxsw)
    TEST_MMX_SSE(pmaxub)
    TEST_MMX_SSE(pminsw)
    TEST_MMX_SSE(pminub)
