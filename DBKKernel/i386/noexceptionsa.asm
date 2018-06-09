_TEXT SEGMENT 'CODE'

PUBLIC _NoException14
_NoException14:
		add esp,4 ;undo errorcode
		;xor ecx,ecx
		;iretd

		push eax
		mov eax, ExceptionlessCopy_Exception
		mov dword ptr [esp+4],eax
		pop eax
		iretd


PUBLIC _ExceptionlessCopy_Internal
_ExceptionlessCopy_Internal:
		push ebp
		mov ebp, esp

		; [ebp]=old ebp
		; [ebp+4]=return address
		; [ebp+8]=destination
		; [ebp+0ch]=source
		; [ebp+10h]=size
		
		push ecx
		push edi
		push esi

		mov esi, [ebp+0ch]
		mov edi, [ebp+08h]
		mov ecx, [ebp+10h]

		rep movsb ;todo: split this up into movsq, movsd, movsw, movsd, or some of those other string routines

	ExceptionlessCopy_Exception:
		sub [ebp+10h],ecx
		mov eax, [ebp + 10h] ; decrease the number of bytes left from the total amount of bytes to get the total bytes written
		pop esi
		pop edi
		pop ecx		
		pop ebp
		ret ;cdecl 

_TEXT   ENDS

        END	


