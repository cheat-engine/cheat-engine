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
%if DISPLAYDEBUG == 1
extern linessincelastkey;
%endif

clearScreen:
cmp qword [loadedOS],0
jne clearScreenExit
mov rdi,[textmemory]
mov rax,0x0f200f200f200f20
mov rcx,2*80*25 / 8
rep stosq

%if DISPLAYDEBUG == 1
mov byte [linessincelastkey],0
%endif
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

;------------------;
;ULONG getRBP(void);
;------------------;
global getRBP
getRBP:
mov rax,rbp
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

mrewlock:


;------------------------------------------------------------------;
;void lockedQwordIncrement(volatile QWORD *address, QWORD inccount);
;------------------------------------------------------------------;
global lockedQwordIncrement
lockedQwordIncrement:
lock add qword [rdi],rsi
ret


;qword getTSC
getTSC:
push rdx
rdtsc
shl rax,32
shr rax,32

shl rdx,32
or rax,rdx
pop rdx
ret

%ifdef SERIALPORT
%if SERIALPORT != 0
extern spinlocktimeout
%endif
%endif


;---------------------------;
;int spinlock(int *lockvar);
;---------------------------;

global spinlock
spinlock:
%ifdef SERIALPORT
%if SERIALPORT != 0
sub rsp,8 ;[rsp=starttime]
call getTSC
mov [rsp],rax
%endif
%endif

spinlock_afterinit:

lock bts dword [rdi],0 ;put the value of bit nr 0 into CF, and then set it to 1
jc spinlock_wait

xor rax,rax ;return 0:lock obtained
%ifdef SERIALPORT
%if SERIALPORT != 0
add rsp,8
%endif
%endif
ret

spinlock_wait:
%ifdef SERIALPORT
%if SERIALPORT != 0
cmp qword [spinlocktimeout],0
je spinlock_aftertimeoutcheck

;check the time
call getTSC
sub rax,[rsp]
cmp rax,[spinlocktimeout]
ja spinlock_timeout

spinlock_aftertimeoutcheck:
%endif
%endif

pause
cmp dword [rdi],0
je spinlock_afterinit
jmp spinlock_wait

%ifdef SERIALPORT
%if SERIALPORT != 0
spinlock_timeout:
mov rax,1
add rsp,8
ret
%endif
%endif



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

;int setjmp(jmp_buf env);
global setjmp
setjmp:
;save RBX, RBP, and R12–R15, the rest can be changed by function calls
mov rax,[rsp]
mov [rdi+0x00],rax ;RIP return
mov [rdi+0x08],rbp
mov [rdi+0x10],rbx
mov [rdi+0x18],r12
mov [rdi+0x20],r13
mov [rdi+0x28],r14
mov [rdi+0x30],r15
pushfq
pop rax
mov [rdi+0x38],rax
mov [rdi+0x40],rsp
xor rax,rax ;set return 0
setjmpreturn:

mov rsi,[rdi+0x00]
mov [rsp],rsi

mov rbp,[rdi+0x08]
mov rbx,[rdi+0x10]
mov r12,[rdi+0x18]
mov r13,[rdi+0x20]
mov r14,[rdi+0x28]
mov r15,[rdi+0x30]
push qword [rdi+0x38]
popfq

ret

global call32bita
call32bita:
;switch to a 32-bit code segment, call the given function, and return back to 64-bit
;rdi=address
;rsi=temp stack address

mov qword [rsi],rsp
sub rsi,8
mov rsp,rsi
mov eax,edi

jmp far [call32bit_32bitcodeaddress]

call32bit_32bitcodeaddress:
dd call32bit_32bitcode
dw 88

BITS 32
call32bit_32bitcode:
nop
mov dx,0x8
mov ss,dx
mov es,dx
mov ds,dx
mov esp,esi
nop
nop
push eax
pop ebx
nop
call eax
nop
nop
jmp 80:call32bit_64bitcode

BITS 64
call32bit_64bitcode:
nop
nop
mov dx,0
mov ds,dx
mov es,dx
mov ss,dx
mov ss,dx

add rsp,8
mov rdx,[rsp]
mov rsp,rdx


ret



;void longjmp(jmp_buf env, int val);
global longjmp
longjmp:
;iretq works:
;tempRIP ← Pop();
;tempCS ← Pop();
;tempEFLAGS ← Pop();
;tempRSP ← Pop();
;tempSS ← Pop();

sub rsp,5*8

mov rax,setjmpreturn
mov [rsp+0x00],rax ;rip=setjmpreturn
mov qword [rsp+0x08],0x50 ;cs
mov qword [rsp+0x10],2 ;eflags
mov rax,[rdi+0x40]
mov [rsp+0x18],rax ;rsp
mov qword [rsp+0x20],8 ;ss
mov rax,rsi ;set the new return
iretq ;using iret so nmi's can go again as well
