;RCX: 1st integer argument
;RDX: 2nd integer argument
;R8: 3rd integer argument
;R9: 4th integer argument


;vmcall:  rdx = password1  info(rax)->password(@offset 4)=password 2

;vmcall(info)

extern vmx_password1 : QWORD
extern vmx_password3 : QWORD

_TEXT SEGMENT 'CODE'
PUBLIC dovmcall_intel
dovmcall_intel:
	mov rax,rcx
	mov rdx,vmx_password1
	mov rcx,vmx_password3
	vmcall
	ret
	
PUBLIC dovmcall_amd
dovmcall_amd:
	mov rax,rcx
	mov rdx,vmx_password1
	mov rcx,vmx_password3
	vmmcall
	ret	
	
_TEXT   ENDS
        END	