;RCX: 1st integer argument
;RDX: 2nd integer argument
;R8: 3rd integer argument
;R9: 4th integer argument

CALLBACK        struct
A		 		qword ?
S				qword ?
CALLBACK        ends

_TEXT SEGMENT 'CODE'

EXTERN interrupt3_centry : proc
EXTERN Int3JumpBackLocation : CALLBACK

EXTERN interrupt14_centry : proc
EXTERN Int14JumpBackLocation : CALLBACK

PUBLIC interrupt3_asmentry
interrupt3_asmentry:
		;save stack position
		push 0 ;push an errorcode on the stack so the stackindex can stay the same relative to interrupts that do have an errorcode (int 14)
		push rbp
		mov rbp,rsp

		;save state
		push rax
		push rcx
		push rdx
		push rbx
		push rsp
		push rbp
		push rsi
		push rdi
	
		xor rax,rax
		mov ax,ds
		push rax

		mov ax,es
		push rax
		
		mov ax,fs
		push rax
		
		mov ax,gs
		push rax		

		
		;and now the extra registers
		push r8
		push r9
		push r10
		push r11
		push r12
		push r13
		push r14
		push r15
		
		mov ax,2bh 
		mov ds,ax
		mov es,ax
		mov gs,ax
		mov ax,53h
		mov fs,ax
		
		mov rcx,rbp
		call interrupt3_centry
		
		cmp al,1 ;set flag

		;restore state
		pop r15
		pop r14
		pop r13
		pop r12
		pop r11
		pop r10
		pop r9
		pop r8
		
		pop rax
		mov gs,ax
		pop rax
		mov fs,ax
		pop rax
		mov es,ax
		pop rax
		mov ds,ax
		
		pop rdi
		pop rsi
		pop rbp
		pop rax ;skip pop rsp
		pop rbx
		pop rdx
		pop rcx
		pop rax

		pop rbp
		
		je skip_original_int3
		;not skipping the original int3
		add rsp,8 ;undo errorcode push (shame it affects the eflags reg)
			
		push Int3JumpBackLocation.S ;cs
		push Int3JumpBackLocation.A ;address		
		retf
		;jmp far [Int3JumpBackLocation] just won't work here
		


skip_original_int3:
		add rsp,8 ;undo errorcode push
		iretq


PUBLIC interrupt14_asmentry
interrupt14_asmentry:
		;save stack position
		push 0 ;push an errorcode on the stack so the stackindex can stay the same relative to interrupts that do have an errorcode (int 14)
		push rbp
		mov rbp,rsp

		;save state
		push rax
		push rcx
		push rdx
		push rbx
		push rsp
		push rbp
		push rsi
		push rdi
	
		xor rax,rax
		mov ax,ds
		push rax

		mov ax,es
		push rax
		
		mov ax,fs
		push rax
		
		mov ax,gs
		push rax		

		
		;and now the extra registers
		push r8
		push r9
		push r10
		push r11
		push r12
		push r13
		push r14
		push r15
		
		mov ax,2bh 
		mov ds,ax
		mov es,ax
		mov gs,ax
		mov ax,53h
		mov fs,ax
		
		mov rcx,rbp
		call interrupt14_centry
		
		cmp al,1 ;set flag

		;restore state
		pop r15
		pop r14
		pop r13
		pop r12
		pop r11
		pop r10
		pop r9
		pop r8
		
		pop rax
		mov gs,ax
		pop rax
		mov fs,ax
		pop rax
		mov es,ax
		pop rax
		mov ds,ax
		
		pop rdi
		pop rsi
		pop rbp
		pop rax ;skip pop rsp
		pop rbx
		pop rdx
		pop rcx
		pop rax

		pop rbp
		
		je skip_original_int14
		;not skipping the original int14
		add rsp,8 ;undo errorcode push (shame it affects the eflags reg)
			
		push Int14JumpBackLocation.S ;cs
		push Int14JumpBackLocation.A ;address		
		retf
		;jmp far [Int14JumpBackLocation] just won't work here
		


skip_original_int14:
		add rsp,8 ;undo errorcode push
		iretq
	
_TEXT   ENDS
        END	