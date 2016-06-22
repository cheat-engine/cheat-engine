autoAssemble([[
  alloc(cpuidcaller, 128)
  registersymbol(cpuidcaller)

  cpuidcaller:
  //parameter is a pointer to a block of 16 bytes which acts as input and output
  //layout:
  //index / eax_result
  //ebx_result
  //ecx_result
  //edx_result
  [32-bit]
  mov eax,[esp+4]
  mov eax,[eax]
  mov ecx,[eax+8]
  push ebx
  push ecx
  push edx


  [/32-bit]

  [64-bit]
  
  mov r8,rcx //save rcx
  mov r9,rbx //save rbx
  mov eax,[rcx]
  mov ecx,[rcx+8]
  [/64-bit]
  cpuid

  [32-bit]
  push edi
  mov edi,[esp+14]
  mov [edi],eax
  mov [edi+4],ebx
  mov [edi+8],ecx
  mov [edi+c],edx
  pop edi

  pop edx
  pop ecx
  pop ebx
  ret 4
  [/32-bit]

  [64-bit]
  mov [r8],eax
  mov [r8+4],ebx
  mov [r8+8],ecx
  mov [r8+c],edx

  mov rbx,r9 //restore rbx
  ret
  [/64-bit]
]], true) 

function CPUID(i,j)
  local s=createMemoryStream()
  s.Size=20 --allocate 20 bytes (yes, you can do this)
  writeIntegerLocal(s.Memory,i)
  writeIntegerLocal(s.Memory+8,j)
  executeCodeLocal("cpuidcaller", s.Memory)


  local r={}
  r.eax=readIntegerLocal(s.Memory)
  r.ebx=readIntegerLocal(s.Memory+4)
  r.ecx=readIntegerLocal(s.Memory+8)
  r.edx=readIntegerLocal(s.Memory+12)

  s.destroy()
  return r
end 