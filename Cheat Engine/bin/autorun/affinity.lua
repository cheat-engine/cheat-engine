autoAssemble([[
  alloc(GetAffinity, 128)
  registersymbol(GetAffinity)

  GetAffinity:
  [32-bit]
  mov eax,[esp+4]
  push [eax+8]
  push [eax+4]
  push ffffffff //push [eax]
  [/32-bit]

  [64-bit]
  sub rsp,28
  mov rax,rcx
  mov rcx,ffffffffffffffff //[rax]
  mov rdx,[rax+8]
  mov r8,[rax+10]
  [/64-bit]
  call GetProcessAffinityMask
  [32-bit]
  ret 4
  [/32-bit]

  [64-bit]
  add rsp,28
  ret
  [/64-bit]

]], true)

function GetAffinity(ph)
  local psize
  local s=createMemoryStream()

  if cheatEngineIs64Bit() then
    psize=8
    s.Size=24
  else
    psize=4
    s.Size=12
  end

  if ph==nil then
    writePointerLocal(s.Memory,-1) --currentProcessHandle
  else
    writePointerLocal(s.Memory,ph)
  end

  writePointerLocal(s.Memory+psize,s.Memory)
  writePointerLocal(s.Memory+psize*2,s.Memory+psize)
  if executeCodeLocal("GetAffinity", s.Memory)==0 then return nil end


  local r={}
  r.ProcessAffinity=readPointerLocal(s.Memory)
  r.SystemAffinity=readPointerLocal(s.Memory+psize);

  s.destroy()
  return r
end


----------------------------------------------------------------------
--                           Set Affinity                           --
----------------------------------------------------------------------
autoAssemble([[
  alloc(SetAffinity, 128)
  registersymbol(SetAffinity)

  SetAffinity:
  [32-bit]
  mov eax,[esp+4]
  push [eax+4]
  push [eax]
  [/32-bit]

  [64-bit]
  sub rsp,28
  mov rax,rcx
  mov rcx,[rax]
  mov rdx,[rax+8]
  [/64-bit]
  call SetProcessAffinityMask
  [32-bit]
  ret 4
  [/32-bit]

  [64-bit]
  add rsp,28
  ret
  [/64-bit]

]], true)

function SetAffinity(mask, ph)
  local psize
  local s=createMemoryStream()

  if cheatEngineIs64Bit() then
    psize=8
    s.Size=16
  else
    psize=4
    s.Size=8
  end

  if ph==nil then
    writePointerLocal(s.Memory,-1) --currentProcessHandle
  else
    writePointerLocal(s.Memory,ph)
  end

  writePointerLocal(s.Memory+psize,mask)
  if executeCodeLocal("SetAffinity", s.Memory)==0 then return nil end

  s.destroy()
  return true
end

----

autoAssemble([[
  alloc(SetAffinityThread, 128)
  registersymbol(SetAffinityThread)

  SetAffinityThread:
  [32-bit]
  mov eax,[esp+4]
  push [eax+4]
  push [eax]
  //call GetCurrentThread
  //push eax
  [/32-bit]

  [64-bit]
  push rbx 
  push rdi
  sub rsp,28
  
  mov rbx,rcx

//  call GetCurrentThread    
//  mov rcx,rax
  mov rcx,[rbx]  
  mov rdx,[rbx+8]

  [/64-bit]
  call SetThreadAffinityMask
  [32-bit]
  ret 4
  [/32-bit]

  [64-bit]
  add rsp,28
  pop rdi
  pop rbx
  ret
  [/64-bit]

]], true)


function SetAffinityThread(mask, th)
  local psize
  local s=createMemoryStream()

  if cheatEngineIs64Bit() then
    psize=8
    s.Size=16
  else
    psize=4
    s.Size=8
  end

  if (th==nil) then
    writePointerLocal(s.Memory,-2) --currentthreadhandle
  else
    writePointerLocal(s.Memory,th) --currentthreadhandle
  end

  writePointerLocal(s.Memory+psize,mask)
  if executeCodeLocal("SetAffinityThread", s.Memory)==0 then return nil end

  s.destroy()
  return true
end

function getCPUCount()
  local c=0;
  local a=GetAffinity().SystemAffinity;

  while a>0 do
    if (a & 1) == 1 then c=c+1 end
    a=a >> 1;
  end
  return c
end

function getCPUNR() --to test if it works or not
  return CPUID(1).ebx >> 24
end

--return GetAffinity(), string.format("%x", getAddress('GetAffinity', true))