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


global timeCheck
timeCheck:
  ;rdi is a pointer to an array of 5 qwords
  sub rsp,64+8
  mov [rsp+0],r8
  mov [rsp+0x08],r9
  mov [rsp+0x10],r10
  mov [rsp+0x18],r11
  mov [rsp+0x20],r12


  rdtsc
  mov r8d,edx
  shl r8,32
  or r8d,eax

  rdtsc
  mov r9d,edx
  shl r9,32
  or r9d,eax

  rdtsc
  mov r10d,edx
  shl r10,32
  or r10d,eax

  rdtsc
  mov r11d,edx
  shl r11,32
  or r11d,eax

  rdtsc
  mov r12d,edx
  shl r12,32
  mov r12d,eax

  mov [rdi],r8
  mov [rdi+0x8],r9
  mov [rdi+0x10],r10
  mov [rdi+0x18],r11
  mov [rdi+0x20],r12


  mov r8,[rsp+0]
  mov r9,[rsp+0x08]
  mov r10,[rsp+0x10]
  mov r11,[rsp+0x18]
  mov r12,[rsp+0x20]

  add rsp,64+8
  ret

global readMSR
readMSR:
  xchg ecx,edi
  rdmsr ;return goes into edx:eax , which just so happens to be the needed value
  shl rdx,32
  add rax,rdx
  xchg ecx,edi
  ret


global writeMSR
writeMSR:
  xchg ecx,edi
  mov eax,esi
  mov rdx,rsi
  shr rdx,32

  wrmsr ;write edx:eax into ecx

  xchg ecx,edi
  ret


global setCR0
setCR0:
  mov cr0,rdi
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

GLOBAL getTSC
getTSC:
  xor rax,rax
  xor rdx,rdx
  rdtsc
  shl rdx,32
  or rax,rdx

  ret

GLOBAL getAccessRights
getAccessRights:
  xor rax,rax
  lar rax,di
  jnz getAccessRights_invalid
  shr rax,8
  and rax,0f0ffh
  ret
  getAccessRights_invalid:
  mov rax,010000h
  ret


GLOBAL getSegmentLimit
getSegmentLimit:
  xor rax,rax
  lsl rax,di
  ret


GLOBAL disableInterrupts
disableInterrupts:
  cli
  ret

GLOBAL enableInterrupts
enableInterrupts:
  sti
  ret


GLOBAL dovmcall
dovmcall:
  push rdx
  mov rax,rdi
  mov rdx,rsi
  vmcall
  pop rdx
  ret

GLOBAL dovmcall2
dovmcall2:
  push r8
  push r9
  mov r8,rdx
  mov r9,rcx

  mov rax,rdi
  mov rdx,rsi
  vmcall
  mov [r8],rax
  mov [r9],rdx
  pop r9
  pop r8
  ret

;extern UINT64 dovmcall(void *vmcallinfo, unsigned int level1pass);
;extern void dovmcall2(void *vmcallinfo, unsigned int level1pass, QWORD *r1, QWORD *r2);
                              ;rdi                     rsi           rdx           rcx



%define SERIALPORT 0b070h

GLOBAL SerialPort
SerialPort:
dd 0

GLOBAL enableSerial
enableSerial:
mov dx,[rel SerialPort] ;3f9h
add dx,1
mov al,0h
out dx,al

mov dx,[rel SerialPort]; 3fbh
add dx,3
mov al,80h
out dx ,al ;access baud rate generator

mov dx,[rel SerialPort]; 3f8h
mov al,1h  ;0c=9600 (1152000/divisor)
out dx,al ;9600 baud

mov dx,[rel SerialPort]; 3f9h
add dx,1
mov al,0h ;high part of devisor
out dx,al ;

mov dx,[rel SerialPort]; 3fbh
add dx,3
mov al,3h
out dx,al ;8 bits, no parity, one stop
ret



GLOBAL waitforkeypress
waitforkeypress:
mov dx,[rel  SerialPort] ;0x3fd
add dx,5
waitforkeypress2:
in al,dx
and al,1
cmp al,1
jne waitforkeypress2
mov dx,[rel SerialPort] ;0x3f8
in al,dx
ret


GLOBAL sendchar32
sendchar32:
xor rax,rax
push rcx
mov rcx,0

sendchar32loop:
mov dx,[rel SerialPort] ;3fdh
add dx,5
in al,dx
and al,0x20

add rcx,1
cmp rcx,100
jae sendchar32loopbreak

cmp al,0x20
jne sendchar32loop

sendchar32loopbreak:

pop rcx

mov dx,[rel SerialPort]; 0x3f8
mov al,dil
out dx,al
ret


