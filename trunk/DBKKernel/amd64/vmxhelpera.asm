;RCX: 1st integer argument
;RDX: 2nd integer argument
;R8: 3rd integer argument
;R9: 4th integer argument

_TEXT SEGMENT 'CODE'
PUBLIC dovmcall_intel
dovmcall_intel:
	mov rax,rcx
	vmcall
	ret
	
PUBLIC dovmcall_amd
dovmcall_amd:
	mov rax,rcx
	vmmcall
	ret	
	
_TEXT   ENDS
        END	