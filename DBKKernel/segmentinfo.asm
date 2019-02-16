_TEXT SEGMENT 'CODE'

PUBLIC _getAccessRights@8										
_getAccessRights@8:
  push ebp
  mov ebp,esp
  
  push ecx
  mov ecx,[ebp+8] ;0=old ebp, 4=return address, 8=param1
  lar eax,ecx
  jnz getAccessRights_invalid

  shr eax,8
  and eax,0f0ffh

  jmp getAccessRights_exit

  
  getAccessRights_invalid:
  mov eax,010000h

getAccessRights_exit:
  pop ecx
  pop ebp
  ret

PUBLIC _getSegmentLimit@8										
_getSegmentLimit@8:
  push ebp
  mov ebp,esp  
  push ecx

  mov ecx,[ebp+8]
  xor eax,eax
  lsl eax,ecx

  pop ecx
  pop ebp
  ret

_TEXT   ENDS
        END

