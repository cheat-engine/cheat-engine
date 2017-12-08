BITS 64

;param passing in 64-bit (linux ABI, NOT windows)
;1=rdi
;2=rsi
;3=rdx
;4=rcx

GLOBAL testfunction

testfunction:
  mov rax,cr3
  ret


GLOBAL brk

brk:
  db 0xcc
  ret


global readMSR
readMSR:
  xchg ecx,edi
  rdmsr ;return goes into edx:eax , which just so happens to be the needed value
  shl rdx,32
  add rax,rdx
  xchg ecx,edi
  ret

global getCR0
getCR0:
  mov rax,cr0
  ret

global getCR2
getCR2:
  mov rax,cr2
  ret

global getCR3
getCR3:
  mov rax,cr3
  ret

global getCR4
getCR4:
  mov rax,cr4
  ret

global getSS
getSS:
  mov ax,ss
  ret

global getCS
getCS:
  mov ax,cs
  ret

global getDS
getDS:
  mov ax,ds
  ret

global getES
getES:
  mov ax,es
  ret

global getFS
getFS:
  mov ax,fs
  ret

global getGS
getGS:
  mov ax,gs
  ret

global getLDT
getLDT:
  SLDT ax
  ret

global getTR
getTR:
  STR AX
  ret


global getDR0
getDR0:
  mov rax,dr0
  ret

global getDR1
getDR1:
  mov rax,dr1
  ret

global getDR2
getDR2:
  mov rax,dr2
  ret

global getDR3
getDR3:
  mov rax,dr3
  ret

global getDR6
getDR6:
  mov rax,dr6
  ret

global getDR7
getDR7:
  mov rax,dr7
  ret

global getGDT
getGDT:
  SGDT [rdi]
  ret

global getIDT
getIDT:
  SIDT [rdi]
  ret

global getEflags
getEflags:
  pushfq
  pop rax
  ret

global getRSP
getRSP:
  mov rax,rsp
  add rax,8 ;undo the call push
  ret

global getRBP
getRBP:
  push rbp
  pop rax
  ret


GLOBAL getRAX
getRAX:
  ret

GLOBAL getRBX
getRBX:
  mov rax,rbx
  ret

GLOBAL getRCX
getRCX:
  mov rax,rcx
  ret

GLOBAL getRDX
getRDX:
  mov rax,rdx
  ret

GLOBAL getRSI
getRSI:
  mov rax,rsi
  ret

GLOBAL getRDI
getRDI:
  mov rax,rdi
  ret

GLOBAL getR8
getR8:
  mov rax,r8
  ret

GLOBAL getR9
getR9:
  mov rax,r9
  ret

GLOBAL getR10
getR10:
  mov rax,r10
  ret

GLOBAL getR11
getR11:
  mov rax,r11
  ret

GLOBAL getR12
getR12:
  mov rax,r12
  ret

GLOBAL getR13
getR13:
  mov rax,r13
  ret

GLOBAL getR14
getR14:
  mov rax,r14
  ret

GLOBAL getR15
getR15:
  mov rax,r15
  ret


GLOBAL disableInterrupts
disableInterrupts:
  cli
  ret

GLOBAL enableInterrupts
enableInterrupts:
  sti
  ret



