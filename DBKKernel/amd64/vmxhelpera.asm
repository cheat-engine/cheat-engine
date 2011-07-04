;RCX: 1st integer argument
;RDX: 2nd integer argument
;R8: 3rd integer argument
;R9: 4th integer argument

_TEXT SEGMENT 'CODE'
PUBLIC dovmcall
dovmcall:
	mov rax,rcx
	;mov rdx,rdx ;hey, guess what, it's already set properly!
	vmcall
	ret
	
_TEXT   ENDS
        END	