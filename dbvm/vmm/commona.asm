BITS 64

;param passing in 64-bit
;1=rdi
;2=rsi
;3=rdx
;4=rcx

extern loadedOS
extern textmemory
;
;getcpunr
;
global getcpunr
getcpunr:
mov eax, dword [fs:0x14]
ret


;
;getcpuinfo
;e
global getcpuinfo
getcpuinfo:
mov rax, qword [fs:0]
ret

;
;
;
global clearScreen
clearScreen:
cmp qword [loadedOS],0
jne clearScreenExit
mov rdi,[textmemory]
mov rax,0x0f200f200f200f20
mov rcx,2*80*25 / 8
rep stosq
clearScreenExit:
ret

;------------------;
;ULONG getRSP(void);
;------------------;
global getRSP
getRSP:
mov rax,rsp
add rax,8 ;for the push of the call
ret

;--------------------------------------------------------------;
;void cpuid(UINT64 *rax, UINT64 *rbx, UINT64 *rcx, UINT64 *rdx);
;--------------------------------------------------------------;
global _cpuid
_cpuid:
push rax
push rbx
push rcx
push rdx
push r8
push r9

;rdi ;param 1 (address of eax)
;rsi ;param 2 (address of ebx)
mov r8,rdx ;param 3 (address of ecx)
mov r9,rcx ;param 4 (address of edx)

mov rax,[rdi]
mov rbx,[rsi]
mov rcx,[r8]
mov rdx,[r9]

cpuid

mov [rdi],rax
mov [rsi],rbx
mov [r8],rcx
mov [r9],rdx

pop r9
pop r8
pop rdx
pop rcx
pop rbx
pop rax
ret

;---------------------------;
;int spinlock(int *lockvar);
;---------------------------;
global spinlock
spinlock:
lock bts dword [rdi],0 ;put the value of bit nr 0 into CF, and then set it to 1
jc spinlock_wait
ret

spinlock_wait:
pause
cmp dword [rdi],0
je spinlock
jmp spinlock_wait

;old implementation
spinlock_old:

;rdi contains the address of the lock

spinlock_loop:
;serialize
push rbx
push rcx
push rdx
xor eax,eax
cpuid ;serialize
pop rdx
pop rcx
pop rbx

;check lock
cmp dword [rdi],0
je spinlock_getlock
pause
jmp spinlock_loop

spinlock_getlock:
mov eax,1
xchg eax,dword [rdi] ;try to lock
cmp eax,0 ;test if successful, if eax=0 then that means it was unlocked
jne spinlock_loop

ret

global enableserial
enableserial:
%ifdef SERIALPORT
%if SERIALPORT != 0
push rdx
pushfq
mov dx,SERIALPORT+1 ;3f9h
mov al,0h
out dx,al

mov dx,SERIALPORT+3 ;3fbh
mov al,80h
out dx ,al ;access baud rate generator

mov dx,SERIALPORT ;3f8h
mov al,1h  ;(1152000/divisor)
out dx,al ;

mov dx,SERIALPORT+1 ;3f9h
mov al,0h ;high part of devisor
out dx,al ;

mov dx,SERIALPORT+3 ;3fbh
mov al,3h
out dx,al ;8 bits, no parity, one stop
popfq
pop rdx
%endif
%endif
ret



global debugbreak
debugbreak:
db 0xcc
ret

global popcnt_support
popcnt_support:
popcnt rax,rdi
ret


