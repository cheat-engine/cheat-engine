;RCX: 1st integer argument
;RDX: 2nd integer argument
;R8: 3rd integer argument
;R9: 4th integer argument

CALLBACK        struct
A		 		qword ?
S				qword ?
CALLBACK        ends


ASMENTRY_STACK	struct ;keep this 16 byte aligned
	Scratchspace	qword ?
	Scratchspace2	qword ? 
	Scratchspace3	qword ?
	Scratchspace4	qword ? 	
	Originalmxcsr	qword ?	
	OriginalRAX		qword ?
	OriginalRBX		qword ?
	OriginalRCX		qword ?
	OriginalRDX		qword ?
	OriginalRSI		qword ?
	OriginalRDI		qword ?
	OriginalRBP		qword ?
	OriginalRSP		qword ? ;not really 'original'
	OriginalR8		qword ?
	OriginalR9		qword ?
	OriginalR10		qword ?
	OriginalR11		qword ?
	OriginalR12		qword ?
	OriginalR13		qword ?
	OriginalR14		qword ?
	OriginalR15		qword ?
	OriginalES		qword ?
	OriginalDS		qword ?		
	OriginalSS		qword ?	
	
ASMENTRY_STACK	ends


_TEXT SEGMENT 'CODE'

EXTERN interrupt1_centry : proc
EXTERN Int1JumpBackLocation : CALLBACK

PUBLIC interrupt1_asmentry
interrupt1_asmentry:
		;save stack position
		sub rsp,4096

		cld			
		push 0 ;push an errorcode on the stack so the stackindex enum type can stay the same relative to interrupts that do have an errorcode (int 14)
		
		;stack is aligned at this point
		sub rsp,SIZEOF ASMENTRY_STACK
		
		
		mov (ASMENTRY_STACK PTR [rsp]).OriginalRBP,rbp
		lea rbp,(ASMENTRY_STACK PTR [rsp]).OriginalRAX
		
		
		mov (ASMENTRY_STACK PTR [rsp]).OriginalRAX,rax	
		mov (ASMENTRY_STACK PTR [rsp]).OriginalRBX,rbx
		mov (ASMENTRY_STACK PTR [rsp]).OriginalRCX,rcx
		mov (ASMENTRY_STACK PTR [rsp]).OriginalRDX,rdx
		mov (ASMENTRY_STACK PTR [rsp]).OriginalRSI,rsi
		mov (ASMENTRY_STACK PTR [rsp]).OriginalRDI,rdi
		mov (ASMENTRY_STACK PTR [rsp]).OriginalRSP,rsp
		mov (ASMENTRY_STACK PTR [rsp]).OriginalR8,r8
		mov (ASMENTRY_STACK PTR [rsp]).OriginalR9,r9
		mov (ASMENTRY_STACK PTR [rsp]).OriginalR10,r10
		mov (ASMENTRY_STACK PTR [rsp]).OriginalR11,r11
		mov (ASMENTRY_STACK PTR [rsp]).OriginalR12,r12
		mov (ASMENTRY_STACK PTR [rsp]).OriginalR13,r13
		mov (ASMENTRY_STACK PTR [rsp]).OriginalR14,r14
		mov (ASMENTRY_STACK PTR [rsp]).OriginalR15,r15


	
		
		
		
		mov ax,ds
		mov word ptr (ASMENTRY_STACK PTR [rsp]).OriginalDS,ax
		
		mov ax,es
		mov word ptr (ASMENTRY_STACK PTR [rsp]).OriginalES,ax
		
		mov ax,ss
		mov word ptr (ASMENTRY_STACK PTR [rsp]).OriginalSS,ax		
		

		mov ax,2bh 
		mov ds,ax
		mov es,ax
		
		mov ax,18h
		mov ss,ax
		
		
		cmp qword ptr [rbp+8*21],010h ;check if origin is in kernelmode (check ss)
		je skipswap1 ;if so, skip the swapgs
		
		swapgs ;swap gs with the kernel version (not to self fix when called from inside kernel)
		
skipswap1:
		
		
		
		stmxcsr dword ptr (ASMENTRY_STACK PTR [rsp]).Originalmxcsr
		
		mov (ASMENTRY_STACK PTR [rsp]).scratchspace2,1f80h		
		ldmxcsr dword ptr (ASMENTRY_STACK PTR [rsp]).scratchspace2

		
		mov rcx,rbp
		call interrupt1_centry
		
		ldmxcsr dword ptr (ASMENTRY_STACK PTR [rsp]).Originalmxcsr
		
		cmp qword ptr [rbp+8*21],10h ;was it a kernelmode interrupt ?
		je skipswap2 ;if so, skip the swapgs part
				
		swapgs ;swap back
skipswap2:

		cmp al,1


		;restore state
		mov ax,word ptr (ASMENTRY_STACK PTR [rsp]).OriginalDS
		mov ds,ax
		
		mov ax,word ptr (ASMENTRY_STACK PTR [rsp]).OriginalES
		mov es,ax
		
		mov ax,word ptr (ASMENTRY_STACK PTR [rsp]).OriginalSS
		mov ss,ax
		
		
		mov rax,(ASMENTRY_STACK PTR [rsp]).OriginalRAX
		mov rbx,(ASMENTRY_STACK PTR [rsp]).OriginalRBX
		mov rcx,(ASMENTRY_STACK PTR [rsp]).OriginalRCX
		mov rdx,(ASMENTRY_STACK PTR [rsp]).OriginalRDX
		mov rsi,(ASMENTRY_STACK PTR [rsp]).OriginalRSI
		mov rdi,(ASMENTRY_STACK PTR [rsp]).OriginalRDI
		mov r8,(ASMENTRY_STACK PTR [rsp]).OriginalR8
		mov r9,(ASMENTRY_STACK PTR [rsp]).OriginalR9
		mov r10,(ASMENTRY_STACK PTR [rsp]).OriginalR10
		mov r11,(ASMENTRY_STACK PTR [rsp]).OriginalR11
		mov r12,(ASMENTRY_STACK PTR [rsp]).OriginalR12
		mov r13,(ASMENTRY_STACK PTR [rsp]).OriginalR13
		mov r14,(ASMENTRY_STACK PTR [rsp]).OriginalR14
		mov r15,(ASMENTRY_STACK PTR [rsp]).OriginalR15

	
		je skip_original_int1
		
		;stack unwind
		mov rbp,(ASMENTRY_STACK PTR [rsp]).OriginalRBP
		add rsp,SIZEOF ASMENTRY_STACK  ;+8 for the push 0
		add rsp,4096
		add rsp,8
		
		jmp [Int1JumpBackLocation.A] ;<-works fine	


skip_original_int1:
		;stack unwind
		mov rbp,(ASMENTRY_STACK PTR [rsp]).OriginalRBP
		add rsp,SIZEOF ASMENTRY_STACK  ;+8 for the push 0		
		add rsp,4096
		add rsp,8
		
		iretq

	
_TEXT   ENDS
        END	