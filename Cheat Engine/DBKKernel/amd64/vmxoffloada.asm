;RCX: 1st integer argument
;RDX: 2nd integer argument
;R8: 3rd integer argument
;R9: 4th integer argument

GDTDesc	STRUCT
Limit	WORD	?
Base	QWORD	?
GDTDesc	ENDS

EXTERN NewGDTDescriptor: GDTDesc
EXTERN NewGDTDescriptorVA: QWORD
EXTERN pagedirptrbasePA: QWORD
EXTERN TemporaryPagingSetupPA: QWORD
EXTERN enterVMM2PA: QWORD
EXTERN originalstatePA: QWORD
EXTERN enterVMM2: QWORD


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
	
	
	;mov rax,rdi ;tell dbvm it's an OS entry and what location the start info is	
		
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
	xchg bx,bx ;bochs break
	
	mov rbx,NewGDTDescriptorVA
	lgdt fword ptr [rbx]
	
	mov rcx,pagedirptrbasePA
	mov rdx,TemporaryPagingSetupPA ;for the mov cr3,rcx
	mov rsi,enterVMM2PA
	mov rdi,originalstatePA
	

	call [enterVMM2]
			
PUBLIC enterVMMEpilogue
enterVMMEpilogue:
	nop
	nop
	nop						
	nop
	nop	
	ret

_TEXT   ENDS
        END