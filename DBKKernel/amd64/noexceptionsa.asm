_TEXT SEGMENT 'CODE'

PUBLIC NoException14
NoException14:

;Security cookies sucks, so getjmp/longjmp are not usable
;So, just falling back to an exceptionless Copy command instead
;or disassemble the instruction RIP points at

;rsp=errorcode
;rsp+8=rip
;rsp+10=cs     ;20
;rsp+18=eflags
;rsp+20=rsp
;rsp+28=ss

add rsp,8 ;skip the errorcode
push rax ;push rax -state as above again
mov rax,ExceptionlessCopy_Exception
mov [rsp+8],rax
pop rax
iretq ;go to the designated return address


PUBLIC ExceptionlessCopy_Internal
;rcx=destination
;rdx=source
;r8=size in bytes

ExceptionlessCopy_Internal:
push rbp
mov rbp,rsp

;[rbp] = old rbp value
;[rbp+8] = return address
;[rbp+10h] - [rbp+30h] = scratchspace

mov [rbp+10h],rsi
mov [rbp+18h],rdi

mov rsi,rdx
mov rdi,rcx
mov rcx,r8

rep movsb  ;todo: split this up into movsq, movsd, movsw, movsd, or some of those other string routines

;on exception just exit

ExceptionlessCopy_Exception:
mov rsi,[rbp+10h]
mov rdi,[rbp+18h]
sub r8,rcx ;decrease the number of bytes left from the total amount of bytes to get the total bytes written
mov rax,r8 

pop rbp
ret



_TEXT   ENDS
        END	