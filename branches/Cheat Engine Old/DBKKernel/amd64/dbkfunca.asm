;RCX: 1st integer argument
;RDX: 2nd integer argument
;R8: 3rd integer argument
;R9: 4th integer argument

;I should probably start converting to inrinsics

_TEXT SEGMENT 'CODE'
PUBLIC getCS
getCS:
	mov ax,cs
	ret

PUBLIC getSS
getSS:
	mov ax,ss
	ret
	
PUBLIC getDS
getDS:
	mov ax,ds
	ret
	
PUBLIC getES
getES:
	mov ax,es
	ret	
	
PUBLIC getFS
getFS:
	mov ax,fs
	ret
	
PUBLIC getGS
getGS:
	mov ax,gs
	ret	
	
PUBLIC GetTR
GetTR:
	STR AX
	ret	
	
PUBLIC GetLDT
GetLDT:
	SLDT ax
	ret
	
PUBLIC GetGDT
GetGDT:
	SGDT [rcx]
	ret
	
PUBLIC _fxsave
_fxsave:
    fxsave [rcx]
    ret
	
PUBLIC getRSP
getRSP:
	mov rax,rsp
	add rax,8 ;undo the call push
	ret	
	
PUBLIC getRBP
getRBP:
    push rbp
    pop rax	
	ret	
	
PUBLIC getRAX
getRAX:	
	ret							
	
PUBLIC getRBX
getRBX:
	mov rax,rbx
	ret	
	
PUBLIC getRCX
getRCX:
	mov rax,rcx
	ret	
	
PUBLIC getRDX
getRDX:
	mov rax,rdx
	ret		
	
PUBLIC getRSI
getRSI:
	mov rax,rsi
	ret		
	
PUBLIC getRDI
getRDI:
	mov rax,rdi
	ret		
	
PUBLIC getR8
getR8:
	mov rax,r8
	ret		
	
PUBLIC getR9
getR9:
	mov rax,r9
	ret		
	
PUBLIC getR10
getR10:
	mov rax,r10
	ret		
	
PUBLIC getR11
getR11:
	mov rax,r11
	ret		
	
PUBLIC getR12
getR12:
	mov rax,r12
	ret		
	
PUBLIC getR13
getR13:
	mov rax,r13
	ret		
	
PUBLIC getR14
getR14:
	mov rax,r14
	ret		
	
PUBLIC getR15
getR15:
	mov rax,r15
	ret														


_TEXT   ENDS
        END

