;RCX: 1st integer argument
;RDX: 2nd integer argument
;R8: 3rd integer argument
;R9: 4th integer argument

GDTDesc	STRUCT
Limit	WORD	?
Base	QWORD	?
GDTDesc	ENDS

S_ORIGINALSTATE STRUCT
_cpucount		QWORD ?
_originalEFER   QWORD ?
_originalLME	QWORD ?
_idtbase		QWORD ?
_idtlimit		QWORD ?
_gdtbase		QWORD ?
_gdtlimit		QWORD ?
_cr0			QWORD ?
_cr2			QWORD ?
_cr3			QWORD ?
_cr4			QWORD ?
_dr7			QWORD ?
_rip			QWORD ?
_rax			QWORD ?
_rbx			QWORD ?
_rcx			QWORD ?
_rdx			QWORD ?
_rsi			QWORD ?
_rdi			QWORD ?
_rbp			QWORD ?
_rsp			QWORD ?
_r8				QWORD ?
_r9				QWORD ?
_r10			QWORD ?
_r11			QWORD ?
_r12			QWORD ?
_r13			QWORD ?
_r14			QWORD ?
_r15			QWORD ?
_rflags			QWORD ?
_cs				QWORD ?
_ss				QWORD ?
_ds				QWORD ?
_es				QWORD ?
_fs				QWORD ?
_gs				QWORD ?
_tr				QWORD ?
_ldt			QWORD ?	
_fsbase			QWORD ?
_gsbase			QWORD ?
S_ORIGINALSTATE ENDS
PS_ORIGNALSTATE TYPEDEF PTR S_ORIGINALSTATE


EXTERN NewGDTDescriptor: GDTDesc
EXTERN NewGDTDescriptorVA: QWORD
EXTERN pagedirptrbasePA: QWORD
EXTERN TemporaryPagingSetupPA: QWORD
EXTERN enterVMM2PA: QWORD
EXTERN originalstatePA: QWORD
EXTERN enterVMM2: QWORD

EXTERN originalstate: PS_ORIGNALSTATE
EXTERN vmmPA: QWORD

_TEXT SEGMENT 'CODE'



PUBLIC enterVMM
enterVMM:
begin:
	xchg bx,bx ;trigger bochs breakpoint
	

	;setup the GDT
	nop
	nop
	nop
	nop
	nop
	nop	

	;switch to identity mapped pagetable
	mov cr3,rdx
	jmp short weee
weee:




	
	;now jump to the physical address (identity mapped to the same virtual address)
	mov rax,secondentry
	mov r8,enterVMM
    sub rax,r8
	add rax,rsi ;add the physical address to the offset location	
	
	jmp rax

secondentry:
	;contrary to the 32-bit setup, we don't disable paging to make the switch to 64-bit, we're already there
	;we can just set the CR3 value
	
	
;----------TEST----------
;	waitforready:
;	mov dx,0ec05h
;	in al,dx
;	and al,20h
;	cmp al,20h
;	jne waitforready
;	
;	mov dx,0ec00h
;	mov al,'1'
;	out dx,al
;^^^^^^^^TEST^^^^^^^^
	

	;enable PAE and PSE (just to make sure)
	mov eax,30h
	mov cr4,rax	
	
	mov cr3,rcx
	jmp short weee2
weee2:


	
	mov rax,007ffff0h
	mov rbx,0
	mov ds,bx
	mov es,bx
	mov fs,bx
	mov gs,bx
	mov ss,bx
	mov rsp,rax
	
	mov rax,cr0
	or eax,10000h
	mov cr0,rax ;enable WP bit
		
	nop
	nop
	nop
	nop
	nop	
	
	jmp fword ptr [vmmjump]
	;jmp fword ptr [vmmjump] ;one thing that I don't mind about x64, relative addressing, so no need to change it by me


extrastorage:
	nop
	nop
	nop
	nop
	nop

vmmjump:	
	dd 00400000h
	dw 50h	
	

detectionstring:
	db 0ceh
	db 0ceh
	db 0ceh
	db 0ceh
	db 0ceh
	db 0ceh
	db 0ceh
	
	
PUBLIC enterVMMPrologue
enterVMMPrologue:	
	cli ;goodbye interrupts
	
	push rbx
	mov rbx,originalstate
	mov (S_ORIGINALSTATE PTR [rbx])._rax,rax
	pop rbx
	
	mov rax,originalstate	
	mov (S_ORIGINALSTATE PTR [rax])._rbx,rbx
	mov (S_ORIGINALSTATE PTR [rax])._rcx,rcx
	mov (S_ORIGINALSTATE PTR [rax])._rdx,rdx
	mov (S_ORIGINALSTATE PTR [rax])._rsi,rsi
	mov (S_ORIGINALSTATE PTR [rax])._rdi,rdi	
	mov (S_ORIGINALSTATE PTR [rax])._rbp,rbp
	mov (S_ORIGINALSTATE PTR [rax])._rsp,rsp
	mov (S_ORIGINALSTATE PTR [rax])._r8,r8
	mov (S_ORIGINALSTATE PTR [rax])._r9,r9
	mov (S_ORIGINALSTATE PTR [rax])._r10,r10
	mov (S_ORIGINALSTATE PTR [rax])._r11,r11
	mov (S_ORIGINALSTATE PTR [rax])._r12,r12
	mov (S_ORIGINALSTATE PTR [rax])._r13,r13
	mov (S_ORIGINALSTATE PTR [rax])._r14,r14
	mov (S_ORIGINALSTATE PTR [rax])._r15,r15
	
	mov rbx,enterVMMEpilogue
	mov (S_ORIGINALSTATE PTR [rax])._rip,rbx
	
	;jmp enterVMMEpilogue ;test to see if the loader is bugged
	
	;still here, loader didn't crash, start executing the move to the dbvm environment
	xchg bx,bx ;bochs break
	
	mov rbx,NewGDTDescriptorVA
	lgdt fword ptr [rbx]
	
	mov rcx,pagedirptrbasePA
	mov rdx,TemporaryPagingSetupPA 
	mov rsi,enterVMM2PA
	

	jmp [enterVMM2]
			
PUBLIC enterVMMEpilogue
enterVMMEpilogue:
	nop
	nop
	push rax
	push rbx
	push rcx
	push rdx	
	cpuid
	pop rdx
	pop rcx
	pop rbx
	pop rax
	nop						
	nop	
	nop	
	
	
	

	
	mov r8,originalstate	
	
	;mov rbx,(S_ORIGINALSTATE PTR [r8])._tr
	;ltr bx

	mov rbx,(S_ORIGINALSTATE PTR [r8])._ss
	mov ss,bx	
	mov rbx,(S_ORIGINALSTATE PTR [r8])._ds
	mov ds,bx
	mov rbx,(S_ORIGINALSTATE PTR [r8])._es
	mov es,bx
	mov rbx,(S_ORIGINALSTATE PTR [r8])._fs
	mov fs,bx
	mov rbx,(S_ORIGINALSTATE PTR [r8])._gs
	mov gs,bx
	
	
	
	mov rcx,0c0000100h
	mov rax,(S_ORIGINALSTATE PTR [r8])._fsbase
	mov rdx,rax
	shr rdx,32
	wrmsr
	
	mov rcx,0c0000101h
	mov rax,(S_ORIGINALSTATE PTR [r8])._gsbase
	mov rdx,rax
	shr rdx,32
	wrmsr

	

	
		
	mov rax,originalstate	
	mov rbx,(S_ORIGINALSTATE PTR [rax])._rbx
	mov rcx,(S_ORIGINALSTATE PTR [rax])._rcx
	mov rdx,(S_ORIGINALSTATE PTR [rax])._rdx
	mov rsi,(S_ORIGINALSTATE PTR [rax])._rsi
	mov rdi,(S_ORIGINALSTATE PTR [rax])._rdi
	mov rbp,(S_ORIGINALSTATE PTR [rax])._rbp
	mov rsp,(S_ORIGINALSTATE PTR [rax])._rsp
	mov r8,(S_ORIGINALSTATE PTR [rax])._r8
	mov r9,(S_ORIGINALSTATE PTR [rax])._r9
	mov r10,(S_ORIGINALSTATE PTR [rax])._r10
	mov r11,(S_ORIGINALSTATE PTR [rax])._r11
	mov r12,(S_ORIGINALSTATE PTR [rax])._r12
	mov r13,(S_ORIGINALSTATE PTR [rax])._r13
	mov r14,(S_ORIGINALSTATE PTR [rax])._r14
	mov r15,(S_ORIGINALSTATE PTR [rax])._r15
	mov rax,(S_ORIGINALSTATE PTR [rax])._rax
	
	;crashtest
	;mov rax,0deadh
	;mov [rax],rax
	
	;sti
	ret
	nop
	nop
	nop

_TEXT   ENDS
        END