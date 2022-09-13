BITS 64

;param passing in 64-bit (linux ABI, NOT windows)
;1=rdi
;2=rsi
;3=rdx
;4=rcx

extern vmm_entry
extern cinthandler
extern menu
extern memorylist
extern clearScreen
extern getAPICID



GLOBAL amain
GLOBAL vmmstart
GLOBAL pagedirlvl4
GLOBAL isAP
GLOBAL loadedOS
GLOBAL bootdisk
GLOBAL nakedcall
GLOBAL nextstack
GLOBAL _vmread
GLOBAL _vmwrite
GLOBAL _vmread2
GLOBAL _vmwrite2

GLOBAL _vmclear
GLOBAL _vmptrld
GLOBAL _vmxon
GLOBAL _vmxoff
GLOBAL _vmlaunch
GLOBAL _vmresume

GLOBAL getGDTbase
GLOBAL getIDTbase
GLOBAL getGDTsize
GLOBAL getIDTsize
GLOBAL setGDT
GLOBAL setIDT

GLOBAL vmmentrycount
GLOBAL initcs

GLOBAL extramemory
GLOBAL extramemorysize

GLOBAL contiguousmemoryPA
GLOBAL contiguousmemorysize

GLOBAL dbvmversion
GLOBAL exportlist

%define VMCALL db 0x0f, 0x01, 0xc1 ;vmcall

;everything here is in virtual memory, paging has already been setup properly

GLOBAL _start
_start:
amain:

jmp short afterinitvariables
times 16-($-$$) db 0x90 ;pad with nop's till a nice 16-byte alignment



loadedOS:           dq 0 ;physical address of the loadedOS section
vmmstart:           dq 0 ;physical address of virtual address 00400000 (obsoletish...)
pagedirlvl4:        dq 0 ;virtual address of the pml4 page (the memory after this page is free)
nextstack:          dq 0 ;start of stack for the next cpu
extramemory:        dq 0 ;physical address of a contiguous block of physical memory available to DBVM
extramemorysize:    dq 0 ;number of pages in extramemory
contiguousmemoryPA: dq 0 ;physical address of a contiguous block of physical memory available for device access
contiguousmemorysize:dq 0 ;number of pages left in contiguousmemoryPA
dbvmversion:        dq 11
exportlist:         dq 0
;uefibooted:         dq 0 ;if set it means this has to launch the AP cpu's as well

initcs: dd 0 ;critical section to block entering cpus.  Each CPU sets up the stack for the next CPU (so there will always be one too many)
vmmentrycount: dd 0  ;The number of times 0x00400000 has been executed (in short, the number of CPU's launched)

;lasttsc: dq 0

afterinitvariables:

lock add dword [vmmentrycount],1

initcs_trylock:
lock bts dword [initcs],0 ;put the value of bit nr 0 into CF, and then set it to 1
jnc launchcpu  ;if it was 0, launch the cpu

;it's locked, wait
initcs_waitloop:
pause
cmp dword [initcs],0
je  initcs_trylock ;it is 0, try to lock
jmp initcs_waitloop


launchcpu:

;db 0xf1 ;debug test
mov rax,[nextstack] ;setup the stack
mov rsp,rax

and rsp,-0x10; //,0xfffffffffffffff0 ;should not be needed as it 'should' be aligned to begin with


mov rax,cr4
or rax,0x200 ;enable fxsave
mov cr4,rax

cmp qword [loadedOS],0
jne afterbootvarcollection

cmp byte [isAP],0
jne afterbootvarcollection



;save the 64-bit regs (mainly for the upper bits)

mov [0x7100],rax
mov [0x7108],rbx
mov [0x7110],rcx
mov [0x7118],rdx
mov [0x7120],rsi
mov [0x7128],rdi
mov [0x7130],rbp
mov [0x7138],r8
mov [0x7140],r9
mov [0x7148],r10
mov [0x7150],r11
mov [0x7158],r12
mov [0x7160],r13
mov [0x7168],r14
mov [0x7170],r15



afterbootvarcollection:
call vmm_entry

vmm_entry_exit:
jmp vmm_entry_exit

dq 0
dq 0


align 16,db 0
isAP:              	dd 0
bootdisk:           dd 0



global vmcall_amd
vmcall_amd:
  vmmcall
  ret

global vmcall_intel
vmcall_intel:
  vmcall
  ret

global vmcall_instr
vmcall_instr: dq vmcall_intel

global vmcalltest_asm
vmcalltest_asm:
  sub rsp,8
  sub rsp,12

  mov dword [rsp],12
  mov dword [rsp+4],0xfedcba98
  mov dword [rsp+8],0

  ;xchg bx,bx
  mov rax,rsp
  mov rdx,0x76543210
  call [vmcall_instr]

  add rsp,8+12
  ret


extern Password1
extern Password3

global _vmcall
_vmcall:
  sub rsp,8
  mov rax,rdi  ;data
  mov rdx,[Password1]  ;password1
  mov rcx,[Password3]
  call [vmcall_instr]
  add rsp,8
  ret




global vmcall_setintredirects
vmcall_setintredirects:
;also int3 and int14
  sub rsp,8
  sub rsp,0x20

  mov dword [rsp],0x1c ;size of struct
  mov dword [rsp+4],0xfedcba98 ;p2
  mov dword [rsp+8],9 ;VMCALL_REDIRECTINT1

  mov dword [rsp+0xc],1 ;idt redirect instead of intredirect
  mov qword [rsp+0x14], inthandler1
  xor eax,eax
  mov ax,cs
  mov dword [rsp+0x1c], eax


   ;int3
  mov rax,rsp
  mov rdx,0x76543210 ;p1
  call [vmcall_instr]

  mov rax,rsp
  mov dword [rsp+8],24 ;VMCALL_REDIRECTINT3
  mov dword [rsp+0xc],1 ;idt redirect instead of intredirect
  mov qword [rsp+0x14], inthandler3
  call [vmcall_instr]

  mov rax,rsp
  mov dword [rsp+8],22 ;VMCALL_REDIRECTINT14
  mov dword [rsp+0xc],1 ;idt redirect instead of intredirect
  mov qword [rsp+0x14], inthandler14
  call [vmcall_instr]


  add rsp,8+0x20
  ret

global SaveExtraHostState
;void SaveExtraHostState(VMCB_PA)
SaveExtraHostState:
  ;xchg bx,bx
  xchg rax,rdi
  vmsave
  xchg rax,rdi
  ret

struc vmxloop_amd_stackframe
  saved_r15:      resq 1
  saved_r14:      resq 1
  saved_r13:      resq 1
  saved_r12:      resq 1
  saved_r11:      resq 1
  saved_r10:      resq 1
  saved_r9:       resq 1
  saved_r8:       resq 1
  saved_rbp:      resq 1
  saved_rsi:      resq 1
  saved_rdi:      resq 1
  saved_rdx:      resq 1
  saved_rcx:      resq 1
  saved_rbx:      resq 1
  saved_rax:      resq 1
  saved_fsbase:   resq 1
  fxsavespace:    resb 512 ;fxsavespace must be aligned

  psavedstate:    resq 1 ;saved param4
  vmcb_hostsave_PA: resq 1 ;saved param3
  vmcb_PA:        resq 1 ;saved param2
  currentcpuinfo: resq 1 ;saved param1
  ;At entry RSP points here
  returnaddress:  resq 1


endstruc
extern vmexit_amd

align 16
global vmxloop_amd
vmxloop_amd:
;xchg bx,bx ;break by bochs

sub rsp, vmxloop_amd_stackframe_size

mov [rsp+currentcpuinfo],rdi
mov [rsp+vmcb_PA], rsi
mov [rsp+vmcb_hostsave_PA], rdx
mov [rsp+psavedstate], rcx

clgi ;no more interrupts from this point on. (Not even some special interrupts)


mov rax,rcx
cmp rax,0
je notloadedos_amd

;setup the initial state
mov rbx,[rax+0x08]
mov rcx,[rax+0x10]
mov rdx,[rax+0x18]
mov rsi,[rax+0x20]
mov rdi,[rax+0x28]
mov rbp,[rax+0x30]
mov r8,[rax+0x40]
mov r9,[rax+0x48]
mov r10,[rax+0x50]
mov r11,[rax+0x58]
mov r12,[rax+0x60]
mov r13,[rax+0x68]
mov r14,[rax+0x70]
mov r15,[rax+0x78]

jmp vmrun_loop


notloadedos_amd:
;init to startup state (or offloados state)

xor rax,rax
mov rbx,rax
mov rcx,rax
mov rdx,rax
mov rdi,rax
mov rbp,rax
mov r8, rax
mov r9, rax
mov r10,rax
mov r11,rax
mov r12,rax
mov r13,rax
mov r14,rax
mov r15,rax
mov rsi,rax


vmrun_loop:
;xchg bx,bx
mov rax,[rsp+vmcb_hostsave_PA]
vmsave

mov rax,[rsp+vmcb_PA]  ;for those wondering, RAX is stored in the vmcb->RAX field, not here
vmload
vmrun ;rax
vmsave

mov rax,[rsp+vmcb_hostsave_PA]
vmload ;this way I don't have to fuck with the fsbase msr


db 0x48
fxsave [rsp+fxsavespace]
movdqa [rsp+fxsavespace+0xA0], xmm0
movdqa [rsp+fxsavespace+0xB0], xmm1
movdqa [rsp+fxsavespace+0xC0], xmm2
movdqa [rsp+fxsavespace+0xD0], xmm3
movdqa [rsp+fxsavespace+0xE0], xmm4
movdqa [rsp+fxsavespace+0xF0], xmm5
movdqa [rsp+fxsavespace+0x100], xmm6
movdqa [rsp+fxsavespace+0x110], xmm7
movdqa [rsp+fxsavespace+0x120], xmm8
movdqa [rsp+fxsavespace+0x130], xmm9
movdqa [rsp+fxsavespace+0x140], xmm10
movdqa [rsp+fxsavespace+0x150], xmm11
movdqa [rsp+fxsavespace+0x160], xmm12
movdqa [rsp+fxsavespace+0x170], xmm13
movdqa [rsp+fxsavespace+0x180], xmm14
movdqa [rsp+fxsavespace+0x190], xmm15

mov [rsp+saved_r15],r15
mov [rsp+saved_r14],r14
mov [rsp+saved_r13],r13
mov [rsp+saved_r12],r12
mov [rsp+saved_r11],r11
mov [rsp+saved_r10],r10
mov [rsp+saved_r9],r9
mov [rsp+saved_r8],r8
mov [rsp+saved_rbp],rbp
mov [rsp+saved_rsi],rsi
mov [rsp+saved_rdi],rdi
mov [rsp+saved_rdx],rdx
mov [rsp+saved_rcx],rcx
mov [rsp+saved_rbx],rbx
mov [rsp+saved_rax],rax



mov rdi,[rsp+currentcpuinfo]
lea rsi,[rsp+saved_r15] ;vmregisters
lea rdx,[rsp+fxsavespace] ;fxsave

call vmexit_amd

;check return. If everything ok restore and jump to vmrun_loop
cmp eax,1
je vmrun_exit


db 0x48
fxrstor [rsp+fxsavespace]
movdqa xmm0, [rsp+fxsavespace+0xA0]
movdqa xmm1, [rsp+fxsavespace+0xB0]
movdqa xmm2, [rsp+fxsavespace+0xC0]
movdqa xmm3, [rsp+fxsavespace+0xD0]
movdqa xmm4, [rsp+fxsavespace+0xE0]
movdqa xmm5, [rsp+fxsavespace+0xF0]
movdqa xmm6, [rsp+fxsavespace+0x100]
movdqa xmm7, [rsp+fxsavespace+0x110]
movdqa xmm8, [rsp+fxsavespace+0x120]
movdqa xmm9, [rsp+fxsavespace+0x130]
movdqa xmm10, [rsp+fxsavespace+0x140]
movdqa xmm11, [rsp+fxsavespace+0x150]
movdqa xmm12, [rsp+fxsavespace+0x160]
movdqa xmm13, [rsp+fxsavespace+0x170]
movdqa xmm14, [rsp+fxsavespace+0x180]
movdqa xmm15, [rsp+fxsavespace+0x190]

mov r15,[rsp+saved_r15]
mov r14,[rsp+saved_r14]
mov r13,[rsp+saved_r13]
mov r12,[rsp+saved_r12]
mov r11,[rsp+saved_r11]
mov r10,[rsp+saved_r10]
mov r9,[rsp+saved_r9]
mov r8,[rsp+saved_r8]
mov rbp,[rsp+saved_rbp]
mov rsi,[rsp+saved_rsi]
mov rdi,[rsp+saved_rdi]
mov rdx,[rsp+saved_rdx]
mov rcx,[rsp+saved_rcx]
mov rbx,[rsp+saved_rbx]


jmp vmrun_loop



vmrun_exit:
add rsp,vmxloop_amd_stackframe_size-8
ret


global doVMRUN
;------------------------------------------------------------------------;
;QWORD doVMRUN(QWORD VMCB_PA, VMRegisters *vmregisters, QWORD dbvmhost_PA, QWORD emulatedhost_PA);
;------------------------------------------------------------------------;
doVMRUN:
;1=rdi=VMCB_PA
;2=rsi=vmregisters
;3=rdx=dbvmhost_PA
;4=rcx=emulatedhost_PA
xchg bx,bx

sub rsp, vmxloop_amd_stackframe_size-8

;store the host state
mov [rsp+saved_rbx],rbx
mov [rsp+saved_rcx],rcx
mov [rsp+saved_rdx],rdx
mov [rsp+saved_rbp],rbp
mov [rsp+saved_rsi],rsi
mov [rsp+saved_rdi],rdi
mov [rsp+saved_r8],r8
mov [rsp+saved_r9],r9
mov [rsp+saved_r10],r10
mov [rsp+saved_r11],r11
mov [rsp+saved_r12],r12
mov [rsp+saved_r13],r13
mov [rsp+saved_r14],r14
mov [rsp+saved_r15],r15

;dbvm doesn't need to store the fxstate. (has nothing stored there anyhow)

mov rax,[rsp+saved_rsi] ;vmregisters (amd stackframe)
mov r15,[rax+saved_r15]
mov r14,[rax+saved_r14]
mov r13,[rax+saved_r13]
mov r12,[rax+saved_r12]
mov r11,[rax+saved_r11]
mov r10,[rax+saved_r10]
mov r9,[rax+saved_r9]
mov r8,[rax+saved_r8]
mov rbp,[rax+saved_rbp]
mov rsi,[rax+saved_rsi]
mov rdi,[rax+saved_rdi]
mov rdx,[rax+saved_rdx]
mov rcx,[rax+saved_rcx]
mov rbx,[rax+saved_rbx]

mov rax,[rsp+saved_rdx] ;dbvmhost_pa
vmsave ;store the current state

mov rax,[rsp+saved_rcx] ;host_pa
vmload ;load the state of the host right before vmrun

mov rax,[rsp+saved_rdi] ;emulated guest vmcb pa
vmrun
mov rax,[rsp+saved_rcx]  ;save the new state to the previous host vmcb (it will be reposible for calling vmsave next)
vmsave

;restore dbvm state
mov rax,[rsp+saved_rdx]
vmload


;store the guest state
mov rax,[rsp+saved_rsi]
db 0x48
fxsave [rax+fxsavespace] ;save fpu

mov [rax+saved_rbx],rbx
mov [rax+saved_rcx],rcx
mov [rax+saved_rdx],rdx
mov [rax+saved_rsi],rsi
mov [rax+saved_rdi],rdi
mov [rax+saved_rbp],rbp
mov [rax+saved_r8],r8
mov [rax+saved_r9],r9
mov [rax+saved_r10],r10
mov [rax+saved_r11],r11
mov [rax+saved_r12],r12
mov [rax+saved_r13],r13
mov [rax+saved_r14],r14
mov [rax+saved_r15],r15

mov rax,[rsp+saved_rax]
mov rbx,[rsp+saved_rbx]
mov rcx,[rsp+saved_rcx]
mov rdx,[rsp+saved_rdx]
mov rsi,[rsp+saved_rsi]
mov rdi,[rsp+saved_rdi]
mov rbp,[rsp+saved_rbp]
mov r8,[rsp+saved_r8]
mov r9,[rsp+saved_r9]
mov r10,[rsp+saved_r10]
mov r11,[rsp+saved_r11]
mov r12,[rsp+saved_r12]
mov r13,[rsp+saved_r13]
mov r14,[rsp+saved_r14]
mov r15,[rsp+saved_r15]

add rsp,vmxloop_amd_stackframe_size-8
ret



global vmxloop
extern vmexit
;-------------------------;
;int vmxloop(cpuinfo *cpu, UINT64 *rax);
;-------------------------;
vmxloop: ;esp=return address, edi = cpuinfo structure pointer, rsi=mapped loadedOS eax base
;0


pushfq   ;8

push rax ;16
push rbx ;24
push rcx ;32
push rdx ;40
push rdi ;48
push rsi ;56
push rbp ;64
push r8  ;72
push r9  ;80
push r10 ;88
push r11 ;96
push r12 ;112
push r13 ;120
push r14 ;128
push r15 ;136

mov rax,0x6c14
vmwrite rax,rsp ;host_esp

mov rax,0x6c16
mov rdx,vmxloop_vmexit
vmwrite rax,rdx  ;host_eip

cmp rsi,0
je notloadedOS

osoffload:
;xchg bx,bx
mov rax,[rsi]
mov rbx,[rsi+0x08]
mov rcx,[rsi+0x10]
mov rdx,[rsi+0x18]
mov rdi,[rsi+0x28]
mov rbp,[rsi+0x30]
mov r8,[rsi+0x40]
mov r9,[rsi+0x48]
mov r10,[rsi+0x50]
mov r11,[rsi+0x58]
mov r12,[rsi+0x60]
mov r13,[rsi+0x68]
mov r14,[rsi+0x70]
mov r15,[rsi+0x78]

mov rsi,[rsi+0x20]

jmp aftersetup

notloadedOS:
xor rax,rax
mov rbx,rax
mov rcx,rax
mov rdx,rax
mov rdi,rax
mov rsi,1 ;for the skipAPTerminationWait parameter for reboot
mov rbp,rax
mov r8, rax
mov r9, rax
mov r10,rax
mov r11,rax
mov r12,rax
mov r13,rax
mov r14,rax
mov r15,rax

aftersetup:
vmlaunch
;just continued through, restore state

%ifdef JTAG
db 0xf1 ;jtag breakpoint
%endif

nop
nop ;just making sure as for some reason kvm's gdb continues here, instead of the previous instruction
nop

pop r15 ;128
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rbp
pop rsi
pop rdi
pop rdx
pop rcx
pop rbx
pop rax ;8

jc vmxloop_fullerr
jz vmxloop_halferr
jmp vmxloop_weirderr


vmxloop_fullerr:
mov eax,1
popfq ;(esp-0)
ret

vmxloop_halferr:
mov eax,2
popfq ;(esp-0)
ret

vmxloop_weirderr:
mov eax,3
popfq ;(esp-0)
ret

align 16
vmxloop_vmexit:
;cli
;ok, this should be executed

;cmp dword [fs:0x14],0
;je isbootcpu



;isbootcpu:

;save registers

;db 0xf1


sub rsp,15*8

mov [rsp+14*8],rax
mov [rsp+11*8],rdx


mov [rsp],r15
mov [rsp+1*8],r14
mov [rsp+2*8],r13
mov [rsp+3*8],r12
mov [rsp+4*8],r11
mov [rsp+5*8],r10
mov [rsp+6*8],r9
mov [rsp+7*8],r8
mov [rsp+8*8],rbp
mov [rsp+9*8],rsi
mov [rsp+10*8],rdi
mov [rsp+12*8],rcx
mov [rsp+13*8],rbx


;set host into a 'valid' state
mov rbp,rsp


fucker:
mov rdi,[rbp+128+ 72] ; param1:currentcpuinfo (rdi of the original host registers, so past the guest registers, inside the host save state)
mov rsi,rbp ; param2: pointer to the guest registers (stored on stack)

cmp rdi,0
jne notfucker

;xchg bx,bx
wbinvd

mov rbx,0x681e  ;RIP
vmread rax,rbx

mov rbx,0x6808  ;CS
vmread rbx,rbx



mov rdi,[rsp+128+ 72] ; param1:currentcpuinfo (rdi of the original host registers, so past the guest registers, inside the host save state)
mov rsi,rsp ; param2: pointer to the guest registers (stored on stack)


notfucker:
;sub rbp,8

;xchg bx,bx ;boxhs bp

and rsp,-0x10 ;0xfffffffffffffff0;
sub rsp,512
db 0x48
fxsave [rsp]
mov rdx,rsp ;param 3, pointer to fxsave

sub rsp,32

;xchg bx,bx

call vmexit

add rsp,32
db 0x48
fxrstor [rsp]


mov rsp,rbp

cmp ax,0xce00
je vmxloop_guestlaunch

cmp ax,0xce01
je vmxloop_guestresume

cmp al,1  ;returnvalue of 1 = quit vmx
jae vmxloop_exitvm


;returned 0, so

;restore vmx registers (esp-36)
pop r15
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rbp
pop rsi
pop rdi
pop rdx
pop rcx
pop rbx
pop rax

;and resume
vmresume

;never executed unless on error
;restore state of vmm


pop r15
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rbp
pop rsi
pop rdi
pop rdx
pop rcx
pop rbx
pop rax ;skip rax, rax contains the result
popfq ;restore flags (esp)
mov rax,3
ret

vmxloop_guestlaunch:
;restore vmx registers (esp-36)
pop r15
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rbp
pop rsi
pop rdi
pop rdx
pop rcx
pop rbx
pop rax

vmlaunch


;db 0xf1 ;debug

;never executed unless on error
;restore state of vmm


mov dword [fs:0x10],0xce00 ;exitreason 0xce00
jmp vmxloop_vmexit

vmxloop_guestresume:
;restore vmx registers (esp-36)
pop r15
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rbp
pop rsi
pop rdi
pop rdx
pop rcx
pop rbx
pop rax

vmresume

db 0xf1 ;debug

;never executed unless on error
mov dword [fs:0x10],0xce01 ;exitreason 0xce01  (resume fail)
jmp vmxloop_vmexit

vmxloop_exitvm:  ;(esp-68)
;user quit or couldn't be handled
xor eax,eax  ;0, so ok



vmxloop_exit: ;(esp)
add rsp,120  ;128=eax=eflags=error, 136=ebx=eflags, 120=
pop r15
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rbp
pop rsi
pop rdi
pop rdx
pop rcx
pop rbx
add rsp,8 ;;skip rax, rax contains the result
popfq ;restore flags (esp)
ret


db 0xcc
db 0xcc
db 0xcc

;---------------------;
;void setRFLAGS(void);
;---------------------;
global setRFLAGS
setRFLAGS:
push rdi
popfq
ret
;---------------------;
;ULONG getRFLAGS(void);
;---------------------;
global getRFLAGS
getRFLAGS:
pushfq
pop rax
ret

db 0xcc
db 0xcc
db 0xcc

;-----------------------------------;
;void setIDT(UINT64 base, WORD size);
;-----------------------------------;
setIDT:
push rbp
mov rbp,rsp
sub rbp,20

mov [rbp],si
mov [rbp+2],rdi
lidt [rbp]

pop rbp
ret

db 0xcc
db 0xcc
db 0xcc
;-----------------------------------;
;void setGDT(UINT64 base, WORD size);
;-----------------------------------;
setGDT:
push rbp
mov rbp,rsp
sub rbp,20

mov [rbp],si
mov [rbp+2],rdi
lgdt [rbp]

pop rbp
ret

db 0xcc
db 0xcc
db 0xcc

;----------------------;
;WORD getGDTsize(void);
;----------------------;
getGDTsize:
push rbp
mov rbp,rsp
sub rbp,20
sgdt [rbp]
xor ax,ax
mov ax,[rbp]
pop rbp
ret

db 0xcc
db 0xcc
db 0xcc

;----------------------;
;WORD getIDTsize(void);
;----------------------;
getIDTsize:
push rbp
mov rbp,rsp
sub rbp,20
sidt [rbp]
xor ax,ax
mov ax,[rbp]
pop rbp
ret

db 0xcc
db 0xcc
db 0xcc

;----------------------;
;QWORD getGDTbase(void);
;----------------------;
getGDTbase:
push rbp
mov rbp,rsp
sub rbp,20
sgdt [rbp]
mov rax,[rbp+2]
pop rbp
ret

db 0xcc
db 0xcc
db 0xcc

;----------------------;
;QWORD getIDTbase(void);
;----------------------;
getIDTbase:
push rbp
mov rbp,rsp
sub rbp,20
sidt [rbp]
mov rax,[rbp+2]
pop rbp
ret

db 0xcc
db 0xcc
db 0xcc

;--------------------------;
;WORD getTaskRegister(void);
;--------------------------;
GLOBAL getTaskRegister
getTaskRegister:
str ax
ret

;-------------------------------------;
;void loadTaskRegister(ULONG selector);
;-------------------------------------;
GLOBAL loadTaskRegister
loadTaskRegister:
mov ax,di
ltr ax
ret

db 0xcc
db 0xcc
db 0xcc
;---------------------------;
;UINT64 _vmread(ULONG index);
;---------------------------;
align 16,db 0xcc
_vmread:
vmread rax,rdi
ret

;---------------------------------------;
;int _vmread2(ULONG index, QWORD *value);
;---------------------------------------;
_vmread2:
vmread rax,rdi
jc _vmread2_err1
jz _vmread2_err2
mov qword [rsi],rax
xor rax,rax
ret

_vmread2_err1:
mov eax,1
ret

_vmread2_err2:
mov eax,2
ret

db 0xcc
db 0xcc
db 0xcc

;---------------------------------------;
;void _vmwrite(ULONG index,UINT64 value);
;---------------------------------------;
align 16,db 0xcc
_vmwrite:
vmwrite rdi,rsi
jc _vmwrite_err1
jz _vmwrite_err2
xor eax,eax
ret

_vmwrite_err1:
mov eax,1
ret

_vmwrite_err2:
mov eax,2
ret

db 0xcc
db 0xcc
db 0xcc

;---------------------------------------;
;int _vmwrite2(ULONG index, QWORD value);
;---------------------------------------;
_vmwrite2:
jmp _vmwrite

;---------------------------------------;
;int vmclear(unsigned long long address);
;---------------------------------------;
_vmclear:
push rdi
vmclear [rsp]
pop rdi
jc vmclear_err
jz vmclear_err2
xor rax,rax
ret
db 0xcc
db 0xcc
db 0xcc

vmclear_err:
mov rax,1
ret

vmclear_err2:
mov rax,2
ret
db 0xcc
db 0xcc
db 0xcc


;---------------------------;
;int vmptrst(QWORD *address);
;---------------------------;
_vmptrst:
vmptrst [rdi]
ret


;-------------------------------------;
;int vmptrld(PHYSICAL_ADDRESS address);
;-------------------------------------;
_vmptrld:
push rdi
vmptrld [rsp]
pop rdi
jc vmptrld_err
jz vmptrld_err2

xor rax,rax
ret
db 0xcc
db 0xcc
db 0xcc

vmptrld_err:
mov rax,1
ret

vmptrld_err2:
mov rax,2
ret
db 0xcc
db 0xcc
db 0xcc

;-----------------------------------;
;int vmxon(PHYSICAL_ADDRESS address);
;-----------------------------------;
_vmxon:
push rdi
vmxon [rsp]				 ;vmxon [eax]
pop rdi

jc vmxon_err
xor rax,rax
ret
db 0xcc
db 0xcc
db 0xcc

vmxon_err:
mov rax,1
ret
db 0xcc
db 0xcc
db 0xcc

;-----------------;
;void vmxoff(void);
;-----------------;
_vmxoff:
vmxoff
ret
db 0xcc
db 0xcc
db 0xcc

;------------------;
;int vmlaunch(void);
;------------------;
_vmlaunch:
;setup the launch registers
mov eax,0
mov ebx,0
mov ecx,0
mov edx,0xf00
mov edi,0
mov esi,0
vmlaunch
jc vmlaunch_err
jz vmlaunch_err_half
xor eax,eax
ret
db 0xcc
db 0xcc
db 0xcc

vmlaunch_err:
mov eax,1
ret
db 0xcc
db 0xcc
db 0xcc

vmlaunch_err_half:
mov eax,2
ret
db 0xcc
db 0xcc
db 0xcc

;-------------------;
;void vmresume(void);
;-------------------;
_vmresume:
vmresume
ret ;not really needed...
db 0xcc
db 0xcc
db 0xcc

;-------------------------------------;
;unsigned long long readMSR(ULONG msr);
;-------------------------------------;
global readMSR
readMSR:
xchg ecx,edi
rdmsr ;return goes into edx:eax , which just so happens to be the needed value
shl rdx,32
add rax,rdx
xchg ecx,edi
ret
db 0xcc
db 0xcc
db 0xcc

;-------------------------------------;
;unsigned long long writeMSR(ULONG msr, unsigned long long newvalue);
;-------------------------------------;
global writeMSR
writeMSR:

push rcx
push rax
push rdx
mov ecx,edi
mov eax,esi
mov rdx,rsi
shr rdx,32

wrmsr
pop rdx
pop rax
pop rcx
ret
db 0xcc
db 0xcc
db 0xcc

;------------------------------------;
;void xsetbv(ULONG xcr, UINT64 value);
;------------------------------------;
global _xsetbv
_xsetbv:
push rcx
push rax
push rdx
mov ecx,edi
mov eax,esi
mov rdx,rsi
shr rdx,32

xsetbv

pop rdx
pop rax
pop rcx
ret
db 0xcc
db 0xcc
db 0xcc


;----------------;
;int3bptest(void);
;----------------;
global int3bptest
int3bptest:
nop
nop
db 0x66
db 0x67
db 0xcc
nop
nop
ret

;-------------------;
;void testcode(int x);
;-------------------;
global testcode
testcode:
mov rax,rdi
nop
nop
nop
cmp rax,rdi
jne testcode_end
nop
nop
testcode_end:
nop
nop
nop

;popad
ret
db 0xcc
db 0xcc
db 0xcc



global hascpuid
;------------------;
;int hascpuid(void);
;------------------;
hascpuid:
push rdx

pushfq
pop rax
mov rdx,rax

xor rax,1000000000000000000000b
push rax
popfq
pushfq
pop rax
cmp rax,rdx
pop rdx

je hascpuid_no ;same as original (unchanged)
mov rax,1
ret
db 0xcc
db 0xcc
db 0xcc

hascpuid_no:
xor rax,rax
ret
db 0xcc
db 0xcc
db 0xcc

global stopautomation
;-------------------------;
;void stopautomation(void);
;-------------------------;
stopautomation:
mov rax,0xcececece
VMCALL
ret
db 0xcc
db 0xcc
db 0xcc


global cLIDT
;----------------------;
;cLIDT(void *idtloader);
;----------------------;
cLIDT:
;rdi contains the address of the new idt descriptor
lidt [rdi]
ret
db 0xcc
db 0xcc
db 0xcc

global getCR0
;------------;
;ULONG getCR0(void);
;------------;
getCR0:
mov rax,cr0
ret
db 0xcc
db 0xcc
db 0xcc

global setCR0
;--------------------;
;setCR0(ULONG newcr0);
;--------------------;
setCR0:
mov cr0,rdi
ret
db 0xcc
db 0xcc
db 0xcc




global getCR2
;------------;
;UINT64 getCR2(void);
;------------;
getCR2:
mov rax,cr2
ret
db 0xcc
db 0xcc
db 0xcc

global setCR2
;--------------------;
;setCR2(UINT64 newcr2);
;--------------------;
setCR2:
mov cr2,rdi
ret
db 0xcc
db 0xcc
db 0xcc

global setCR3
;------------;
;setCR3(UINT64 newcr3);
;------------;
setCR3:
mov cr3,rdi
ret
db 0xcc
db 0xcc
db 0xcc

global getCR3
;------------;
;ULONG getCR3(void);
;------------;
getCR3:
mov rax,cr3
ret
db 0xcc
db 0xcc
db 0xcc

global getCR4
;------------------;
;ULONG getCR4(void);
;------------------;
getCR4:
mov rax,cr4
ret
db 0xcc
db 0xcc
db 0xcc

global setCR4
;--------------------;
;setCR4(ULONG newcr4);
;--------------------;
setCR4:
mov cr4,rdi
ret
db 0xcc
db 0xcc
db 0xcc

global setCR8
;--------------------;
;setCR8(QWORD newcr8);
;--------------------;
setCR8:
mov cr8,rdi
ret

global getCR8
getCR8:
mov rax,cr8
ret


global getDR0
;------------------;
;ULONG getDR0(void);
;------------------;
getDR0:
mov rax,dr0
ret
db 0xcc
db 0xcc
db 0xcc


global setDR0
;--------------------;
;setDR0(ULONG newdr0);
;--------------------;
setDR0:
mov dr0,rdi
ret
db 0xcc
db 0xcc
db 0xcc


global getDR1
;------------------;
;ULONG getDR1(void);
;------------------;
getDR1:
mov rax,dr1
ret
db 0xcc
db 0xcc
db 0xcc

global setDR1
;--------------------;
;setDR1(ULONG newdr1);
;--------------------;
setDR1:
mov dr1,rdi
ret
db 0xcc
db 0xcc
db 0xcc

global getDR2
;------------------;
;ULONG getDR2(void);
;------------------;
getDR2:
mov rax,dr2
ret
db 0xcc
db 0xcc
db 0xcc

global setDR2
;--------------------;
;setDR2(ULONG newdr2);
;--------------------;
setDR2:
mov dr2,rdi
ret
db 0xcc
db 0xcc
db 0xcc

global getDR3
;------------------;
;ULONG getDR3(void);
;------------------;
getDR3:
mov rax,dr3
ret
db 0xcc
db 0xcc
db 0xcc

global setDR3
;--------------------;
;setDR3(ULONG newdr3);
;--------------------;
setDR3:
mov dr3,rdi
ret
db 0xcc
db 0xcc
db 0xcc

global getDR6
;------------------;
;ULONG getDR6(void);
;------------------;
getDR6:
mov rax,dr6
ret
db 0xcc
db 0xcc
db 0xcc

global setDR6
;--------------------;
;setDR6(UINT64 newdr6);
;--------------------;
setDR6:
mov dr6,rdi
ret
db 0xcc
db 0xcc
db 0xcc

global getDR7
;------------------;
;ULONG getDR7(void);
;------------------;
getDR7:
mov rax,dr7
ret
db 0xcc
db 0xcc
db 0xcc

global setDR7
;--------------------;
;setDR7(UINT64 newdr7);
;--------------------;
setDR7:
mov dr7,rdi
ret
db 0xcc
db 0xcc
db 0xcc

global _invpcid
;--------------------------;
;_invlpg(int type, 128data);
;--------------------------;
_invpcid:
db 0x66,0x0f,0x38,0x82,0x3e ;invpcid rdi,[rsi]
ret

global _invept
;--------------------------;
;_invept(int type, 128data);  type must be either 1(local for specific ept pointer) or 2(global for all vpids)
;--------------------------;
_invept:
invept rdi,[rsi]
ret


global _invept2
;---------------------------;
;_invept2(int type, 128data);  type must be either 1(local for specific ept pointer) or 2(global for all vpids)
;---------------------------;
_invept2:
invept rdi,[rsi]
jc _invept2_err1
jz _invept2_err2
xor rax,rax
ret

_invept2_err1:
mov eax,1
ret

_invept2_err2:
mov eax,2
ret



global _invvpid
;--------------------------;
;_invvpid(int type, 128data);  type must be either 0(specific linear address for specific vpid) 1(local for specific vpid) or 2(global for all vpids)
;--------------------------;
_invvpid:
invvpid rdi,[rsi]
ret

global _invvpid2
;----------------------------;
;_invvpid2(int type, 128data);  type must be either 0(specific linear address for specific vpid) 1(local for specific vpid) or 2(global for all vpids)
;----------------------------;
_invvpid2:
invvpid rdi,[rsi]
jc _vmread2_err1
jz _vmread2_err2
xor rax,rax
ret

_invvpid2_err1:
mov eax,1
ret

_invvpid2_err2:
mov eax,2
ret




global _invlpg
;-----------------------;
;_invlpg(UINT64 address);
;-----------------------;
_invlpg:
invlpg [rdi]
ret
db 0xcc
db 0xcc
db 0xcc

global _wbinvd
_wbinvd:
wbinvd
ret

global _invd
_invd:
invd
ret

global _rdtsc
;-------------------------------;
;unsigned long long _rdtsc(void);
;-------------------------------;
_rdtsc:
rdtsc
shl rdx,32
add rax,rdx
ret
db 0xcc
db 0xcc
db 0xcc

global _pause
;-------------------------------;
;void _pause(void);
;-------------------------------;
_pause:
nop
nop
nop
pause
nop
nop
nop
ret
db 0xcc
db 0xcc
db 0xcc



%macro	_inthandler	1
global inthandler%1
inthandler%1:
;xchg bx,bx
cli ;is probably already done, but just to be sure

;db 0xf1 ; jtag break

push %1
jmp inthandlerx
db 0xcc
db 0xcc
db 0xcc
%endmacro


inthandlerx: ;called by the _inthandler macro after it has set it's int nr
push rax ;8
push rbx ;16
push rcx ;24
push rdx ;32
push rdi ;
push rsi ;
push rbp ;
push r8  ;64
push r9  ;
push r10 ;
push r11 ;
push r12 ;
push r13 ;
push r14 ;
push r15 ;128

mov rsi,[rsp+120] ;param2 (intnr)
mov rdi,rsp ;param1 (stack)

mov rbp,rsp
and rsp,-0x10; 0xfffffffffffffff0
sub rsp,32

call cinthandler

mov rsp,rbp


pop r15
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rbp
pop rsi
pop rdi
pop rdx
pop rcx
pop rbx

cmp rax,0
pop rax
je inthandlerx_noerrorcode

;errocode
add rsp,16 ;undo push (intnr) and errorcode
jmp inthandlerx_exit

inthandlerx_noerrorcode:
add rsp,8  ;undo push (intnr)

inthandlerx_exit:
iretq



db 0xcc
db 0xcc
db 0xcc

_inthandler 0
_inthandler 1
_inthandler 2
_inthandler 3
_inthandler 4
_inthandler 5
_inthandler 6
_inthandler 7
_inthandler 8
_inthandler 9
_inthandler 10
_inthandler 11
_inthandler 12
_inthandler 13
_inthandler 14
_inthandler 15
_inthandler 16
_inthandler 17
_inthandler 18
_inthandler 19
_inthandler 20
_inthandler 21
_inthandler 22
_inthandler 23
_inthandler 24
_inthandler 25
_inthandler 26
_inthandler 27
_inthandler 28
_inthandler 29
_inthandler 30
_inthandler 31
_inthandler 32
_inthandler 33
_inthandler 34
_inthandler 35
_inthandler 36
_inthandler 37
_inthandler 38
_inthandler 39
_inthandler 40
_inthandler 41
_inthandler 42
_inthandler 43
_inthandler 44
_inthandler 45
_inthandler 46
_inthandler 47
_inthandler 48
_inthandler 49
_inthandler 50
_inthandler 51
_inthandler 52
_inthandler 53
_inthandler 54
_inthandler 55
_inthandler 56
_inthandler 57
_inthandler 58
_inthandler 59
_inthandler 60
_inthandler 61
_inthandler 62
_inthandler 63
_inthandler 64
_inthandler 65
_inthandler 66
_inthandler 67
_inthandler 68
_inthandler 69
_inthandler 70
_inthandler 71
_inthandler 72
_inthandler 73
_inthandler 74
_inthandler 75
_inthandler 76
_inthandler 77
_inthandler 78
_inthandler 79
_inthandler 80
_inthandler 81
_inthandler 82
_inthandler 83
_inthandler 84
_inthandler 85
_inthandler 86
_inthandler 87
_inthandler 88
_inthandler 89
_inthandler 90
_inthandler 91
_inthandler 92
_inthandler 93
_inthandler 94
_inthandler 95
_inthandler 96
_inthandler 97
_inthandler 98
_inthandler 99
_inthandler 100
_inthandler 101
_inthandler 102
_inthandler 103
_inthandler 104
_inthandler 105
_inthandler 106
_inthandler 107
_inthandler 108
_inthandler 109
_inthandler 110
_inthandler 111
_inthandler 112
_inthandler 113
_inthandler 114
_inthandler 115
_inthandler 116
_inthandler 117
_inthandler 118
_inthandler 119
_inthandler 120
_inthandler 121
_inthandler 122
_inthandler 123
_inthandler 124
_inthandler 125
_inthandler 126
_inthandler 127
_inthandler 128
_inthandler 129
_inthandler 130
_inthandler 131
_inthandler 132
_inthandler 133
_inthandler 134
_inthandler 135
_inthandler 136
_inthandler 137
_inthandler 138
_inthandler 139
_inthandler 140
_inthandler 141
_inthandler 142
_inthandler 143
_inthandler 144
_inthandler 145
_inthandler 146
_inthandler 147
_inthandler 148
_inthandler 149
_inthandler 150
_inthandler 151
_inthandler 152
_inthandler 153
_inthandler 154
_inthandler 155
_inthandler 156
_inthandler 157
_inthandler 158
_inthandler 159
_inthandler 160
_inthandler 161
_inthandler 162
_inthandler 163
_inthandler 164
_inthandler 165
_inthandler 166
_inthandler 167
_inthandler 168
_inthandler 169
_inthandler 170
_inthandler 171
_inthandler 172
_inthandler 173
_inthandler 174
_inthandler 175
_inthandler 176
_inthandler 177
_inthandler 178
_inthandler 179
_inthandler 180
_inthandler 181
_inthandler 182
_inthandler 183
_inthandler 184
_inthandler 185
_inthandler 186
_inthandler 187
_inthandler 188
_inthandler 189
_inthandler 190
_inthandler 191
_inthandler 192
_inthandler 193
_inthandler 194
_inthandler 195
_inthandler 196
_inthandler 197
_inthandler 198
_inthandler 199
_inthandler 200
_inthandler 201
_inthandler 202
_inthandler 203
_inthandler 204
_inthandler 205
_inthandler 206
_inthandler 207
_inthandler 208
_inthandler 209
_inthandler 210
_inthandler 211
_inthandler 212
_inthandler 213
_inthandler 214
_inthandler 215
_inthandler 216
_inthandler 217
_inthandler 218
_inthandler 219
_inthandler 220
_inthandler 221
_inthandler 222
_inthandler 223
_inthandler 224
_inthandler 225
_inthandler 226
_inthandler 227
_inthandler 228
_inthandler 229
_inthandler 230
_inthandler 231
_inthandler 232
_inthandler 233
_inthandler 234
_inthandler 235
_inthandler 236
_inthandler 237
_inthandler 238
_inthandler 239
_inthandler 240
_inthandler 241
_inthandler 242
_inthandler 243
_inthandler 244
_inthandler 245
_inthandler 246
_inthandler 247
_inthandler 248
_inthandler 249
_inthandler 250
_inthandler 251
_inthandler 252
_inthandler 253
_inthandler 254
_inthandler 255

tester:
mov al,1
lp:
mov byte [0x0b8000],'Y'
mov byte [0x0b8001],al
inc al
mov byte [0x0b8002],'o'
mov byte [0x0b8003],al
inc al
mov byte [0x0b8004],'u'
mov byte [0x0b8005],al
inc al
mov byte [0x0b8006],' '
mov byte [0x0b8007],al
inc al
mov byte [0x0b8008],'R'
mov byte [0x0b8009],al
inc al
mov byte [0x0b800a],'o'
mov byte [0x0b800b],al
inc al
mov byte [0x0b800c],'c'
mov byte [0x0b800d],al
inc al
mov byte [0x0b800e],'k'
mov byte [0x0b800f],al
inc al
jmp lp


;---------------------;
;void changetask(void);
;---------------------;
global changetask
changetask:
nop
nop
bits 32
call 64:0
bits 64
nop
nop

ret

;-------------------;
;void tasktest(void);
;-------------------;
global tasktest
tasktest:
pushfq
pop rax
and rax,0x10000
cmp rax,0x10000
je tasktest_RF

mov byte [0xb8000],'B';
mov byte [0xb8002],'L';
mov byte [0xb8004],'A';
jmp tasktest_return

tasktest_RF:
mov byte [0xb8000],'R';
mov byte [0xb8002],'F';
mov byte [0xb8004],'1';

tasktest_return:
nop
nop
nop
nop
nop
iret
nop
nop
jmp tasktest


bits 64
global virtual8086_start
;----------------------------;
;void virtual8086_start(void);
;----------------------------;
virtual8086_start:
;called from 64-bit, so still in 64-bit mode



jmp far [moveto32bitstart]

global moveto32bitstart
moveto32bitstart:
dd 0x2000
dw 24

bits 32
global virtual8086entry32bit
virtual8086entry32bit:
;this gets moved to 0x2000


mov byte [0xb8000],'1';
mov byte [0xb8001],15;

;disable paging
mov eax,cr0
and eax,0x7FFFFFFF
mov cr0,eax

xor eax,eax
mov cr3,eax

mov byte [0xb8000],'2';
mov byte [0xb8001],15;



;unset IA32_EFER_LME to 0 (disable 64 bits)
mov ecx,0xc0000080
rdmsr
and eax,0xFFFFFEFF
wrmsr


mov eax,cr4
or eax,1
mov cr4,eax

mov byte [0xb8000],'3';
mov byte [0xb8001],15;


;xchg bx,bx
mov word [0x40000],0x4f
mov eax,[0x3004]
mov dword [0x40002],eax
lgdt [0x40000]


mov byte [0xb8000],'4';
mov byte [0xb8001],15;



;xchg bx,bx
;xor eax,eax
;mov cr0,eax
jmp (4*8):(0x2000+entry16-virtual8086entry32bit)

bits 16

entry16:
xor eax,eax
mov cr0,eax
jmp 0:(0x2000+realmodetest-virtual8086entry32bit)


;mov eax,[0x3000]
;mov cr3,eax
;mov eax,cr0
;or eax,0x80000000 ;enable paging
;mov cr0,eax
;jmp virtual8086entry32bit2

;virtual8086entry32bit2:

bits 32
nop
mov ax,8
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax
mov ss,ax

;mov byte [0xdead],1

mov word [0x40008],256*8
mov eax,[0x3008] ;idt table
mov dword [0x4000a],eax
lidt [0x40008]

mov ax,56
ltr ax
nop
nop
nop
jmp 64:0

notcrashed:
jmp notcrashed

;global idttable32
;idttable32:
;times 256*8 db 0   ; Make space


global inthandler_32
inthandler_32:
;protected mode interrupt
push ebp
mov ebp,esp

;ebp+0x0=old ebp
;ebp+0x4=intnr
;ebp+0x8=eip
;ebp+0xc=cs
;ebp+0x10=eflags
;ebp+0x14=esp
;ebp+0x18=ss
pushfd
push ebx
push eax
push ds
mov ax,8
mov ds,ax

;save state in realmode stack
sub word [ebp+0x14],6 ;decrease stack with 6 (3 pushes)

xor eax,eax
xor ebx,ebx
mov ax,[ebp+0x18] ;ss
mov bx,[ebp+0x14] ;sp
shl eax,4
add eax,ebx

;eax now contains the stack address in realmode
mov bx,[ebp+8]
mov [eax],bx ;save ip
mov bx,[ebp+0xc]
mov [eax+2],bx ;save cs
mov bx,[ebp+0x10]
mov [eax+4],bx ;save eflags


;change return link
mov eax,[ebp+4] ;eax gets the intnr
mov bx,word [eax*4]   ;ip
mov ax,word [eax*4+2] ;cs
mov [ebp+0x8],bx
mov [ebp+0xc],ax
pop ds
pop eax
pop ebx
popfd
pop ebp
add esp,4 ;get rid of push intnr
iret

bits 16
global realmodetest
realmodetest:

xor eax,eax
mov cr0,eax
mov cr3,eax
mov cr4,eax

xor eax,eax
xor ebx,ebx
xor ecx,ecx
xor edx,edx
xor esi,esi
xor edi,edi
xor ebp,ebp
xor esp,esp





mov ax,0xb800
mov ds,ax
mov byte [ds:0],'5'
mov byte [ds:1],15

jmp 0:(0x2000+realmodetest_b-virtual8086entry32bit)
realmodetest_b:

mov ax,0xb800
mov ds,ax
mov byte [ds:0],'6'
mov byte [ds:1],15


mov ax,0x8000
mov ds,ax
mov es,ax
mov word [0],0x400   ;  256*4
mov dword [2],0
lidt [0x0]

mov word [0],0   ;  0
mov dword [2],0
lgdt [0x0]


nop
nop
nop
xor ax,ax
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax
mov ax,0x4000
mov ss,ax
mov sp,0xff00
nop
nop
nop
cli
nop

mov ax,0xb800
mov ds,ax
mov byte [ds:0],'7'
mov byte [ds:1],15

;call 0xc000:0000



mov ah,0
mov al,3
int 10h


mov ax,0xb800
mov ds,ax
mov byte [ds:0],'8'
mov byte [ds:1],15

;at 80x25 now

showpsod:
xchg bx,bx
cld
mov ax,0b800h
mov si,0
mov di,0
mov ds,ax
mov es,ax
mov cx,(80*25)
mov ax,05000h  ;pink background, black text
rep stosw

;write the message (stored at 40000h)

mov ax,06000h
mov ds,ax
mov si,0
mov di,0

psod_loop:
cmp byte [ds:si],0
je psod_lock ;reached end

cmp byte [ds:si],10
je _newline

cmp byte [ds:si],13
je _newline

jmp nonewline

_newline:
xor dx,dx
add di,80*2
mov ax,di
mov cx,80*2
div cx
sub di,dx

add si,1
jmp psod_loop


;newline

nonewline:
mov al,[ds:si]
mov [es:di],al

add si,1
add di,2

jmp psod_loop


psod_lock:
jmp psod_lock



bits 64

;--------------------;
;void infloop(void);
;--------------------;
global infloop
infloop:
nop
nop
xchg bx,bx
cpuid
nop
;hlt
nop
nop
xchg bx,bx ;should never happen
nop
jmp infloop



;--------------------;
;void quickboot(void);
;--------------------;
global quickboot
quickboot:
;quickboot is called by the virtual machine as initial boot startup
call clearScreen

xor rax,rax
mov dr7,rax
mov dr6,rax
mov dr0,rax
mov dr1,rax
mov dr2,rax
mov dr3,rax

xor edx,edx
mov ecx,0xc0000100 ;IA32_FS_BASE_MSR
wrmsr


;set the upper bits
mov rax,[0x7100]
mov rbx,[0x7108]
mov rcx,[0x7110]
mov rdx,[0x7118]
mov rsi,[0x7120]
mov rdi,[0x7128]
mov rbp,[0x7130]
mov r8, [0x7138]
mov r9, [0x7140]
mov r10,[0x7148]
mov r11,[0x7150]
mov r12,[0x7158]
mov r13,[0x7160]
mov r14,[0x7168]
mov r15,[0x7170]



;nop
;nop
;xchg bx,bx
;mov eax,0
;push 0x197
;popfq
;cpuid
;nop
;nop

;disable cpuid bit
push rax
pushfq
pop rax
and rax,0xFFDFF32A
or rax,0x80
push rax
popfq
pop rax


mov word [0x40000],0x80
mov dword [0x40002],0x50000
mov dword [0x40006],0
lgdt [0x40000]

jmp far [movetorealstart]

movetorealstart:
dd 0x00020000
dw 24

global movetoreal
global movetoreal_end
bits 32
movetoreal: ;this gets moved to 0x00020000
nop
nop
mov eax,cr0
mov ebx,cr4
nop
mov cr4,ebx
mov cr0,eax

mov ax,8
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax
mov ss,ax
mov esp,0x5000
mov eax,0
cpuid



;disable paging
mov eax,cr0
and eax,0x7FFFFFFF
mov cr0,eax

xor eax,eax
cpuid
xor eax,eax

mov cr3,eax

;unset IA32_EFER_LME to 0 (disable 64 bits)
mov ecx,0xc0000080
rdmsr
and eax,0xFFFFFEFF
wrmsr
nop



;mov byte [0x1dead],2
nop
nop

;go to 16 bit
jmp 32:0x0000+(real16-movetoreal)

global real16
real16:
bits 16
nop

;setup datasegment, just for the fun of it
mov ax,40
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax
mov ss,ax

xor eax,eax
mov cr0,eax
nop

jmp realmode

global realmode
realmode:
mov ax,0x8000
mov ss,ax
mov sp,0xfffe

mov bx,ds
mov cx,cs

mov eax,cr3
mov cr3,eax

mov ax,ds
mov dx,cs


rmstart:
call rmbegin
rmbegin:
pop bp


pushf
push 0x2000
push vmxstartup-movetoreal
iret


rmmarker:
dw 0x0000+(vmxstartup-movetoreal)
dw 0x2000

;still 16 bits here
;---------------------;
;void vmxstartup(void);
;---------------------;
global vmxstartup
global vmxstartup_end
vmxstartup:
nop

mov ax,0x8000
mov ds,ax
mov es,ax
mov word [0],0x3ff   ;  256*4
mov dword [2],0
lidt [0x0]

mov word [0],0   ;  0
mov dword [2],0
lgdt [0x0]

;xchg bx,bx
nop
mov ecx,0xc0000080 ;test to see how it handles an efer write
xor eax,eax
xor edx,edx
wrmsr

nop
;xchg bx,bx
nop

;mov ecx,0xc0010117 ;cause an exit
;rdmsr


xor eax,eax
xor ebx,ebx
xor ecx,ecx
xor edx,edx
xor ebp,ebp
xor edi,edi
xor esi,esi
mov cr4,eax
nop
nop
nop

mov ax,0x1234
mov eax,CR0

mov bx,0x2345
mov ebx,CR0

vm_basicinit:
;xchg bx,bx

xor ax,ax
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax
mov ax,0x7000
mov ss,ax
mov sp,0x8000

mov eax,[0x7dfa] ;restore cr0 with the stored value of cr0
mov cr0,eax
;jmp 0:0x7c00

mov ax,0x2000
mov ds,ax
mov si,(str_alive-movetoreal)
call sendstring16
xor ax,ax
mov ds,ax

nop
;db 0xf1
nop
nop
cld
sti
nop

;loopafterint:
nop
;cpuid
nop
;jmp loopafterint

mov ax,3
int 0x10

push ds
mov ax,0x2000
mov ds,ax
mov si,(str_alive-movetoreal)
call printstring

mov si,(str_settingup-movetoreal)
call printstring

mov si,(str_launching-movetoreal)
call printstring

pop ds

mov byte [0x7c00],1

;loopy:
;jmp loopy

hwtest:
nop
nop
nop

;mov byte [0x7c0e],0x80

mov ax,0x3000
mov es,ax

mov ax,0 ;reset disk
mov dl,[0x7c0e]
clc
int 0x13
nop
nop
jnc pass1

mov cl,1
call printerror

jmp notok

pass1:
nop
nop
mov ax,0
mov dl,[0x7c0e]
clc
int 0x13
jnc pass2

mov cl,2
call printerror


jmp notok

pass2:
nop
nop
nop
mov ax,0x0201
mov bx,0x8000
mov ch,0
mov cl,0x1
mov dl,[0x7c0e] ;dl contains hd
mov dh,0
clc
int 0x13
jnc pass3

mov cl,3
call printerror

jmp notok

pass3:
mov ax,0x0201
mov bx,0x8000
mov ch,0
mov cl,0x2
mov dh,0
mov dl,[0x7c0e]
clc
int 0x13
jnc pass4

mov cl,4
call printerror

jmp notok

pass4:
mov ax,0
clc
int 0x13
jnc pass5

mov cl,5
call printerror

jmp notok

pass5:
mov ax,0x0201
mov bx,0x8000
mov ch,0
mov cl,0x3
mov dh,0
mov dl,[0x7c0e]
clc
int 0x13
jnc pass6

mov cl,6
call printerror

jmp notok

pass6:
readagain2: ;final read

xor ax,ax
mov es,ax

sti
mov ax,0x0201
mov bx,0x7c00 ;;final actual read
mov ch,0
mov cl,0x1
mov dh,0
mov dl,[0x7c0e]
push dx
clc
int 0x13
pop dx
jnc pass7

mov cl,7
call printerror

jmp notok
nop
nop

pass7:
jmp readok

notok:
sti

mov si,(str_givingup-movetoreal)
call printstring

notok_loop:
sti
mov ax,0xb800
mov ds,ax
mov byte [0],'X'
mov byte [1],025h
mov byte [2],'X'
mov byte [3],025h
mov byte [4],'X'
mov byte [5],025h
mov byte [6],'X'
mov byte [7],025h
nop
nop
cpuid
nop
nop
jmp notok_loop


notok2:
sti
xor ax,ax
mov ds,ax
cmp byte [0x48d],0
je hmm


mov ax,0xb800
mov ds,ax
mov byte [0],'F';
mov byte [1],4;
mov byte [2],'U';
mov byte [3],4;
mov byte [4],'C';
mov byte [5],4;
mov byte [6],'K';
mov byte [7],4;

jmp notok2

hmm:
sti
mov ax,0xb800
mov ds,ax
mov byte [0],'H';
mov byte [1],4;
mov byte [2],'M';
mov byte [3],4;
mov byte [4],'M';
mov byte [5],4;
mov byte [6],'M';
mov byte [7],4;
jmp hmm



bt_test:
nop
nop
cpuid
nop
nop
jmp bt_test

readok:
mov ax,0xb800
mov ds,ax
mov byte [0],'B';
mov byte [1],6;
mov byte [2],'O';
mov byte [3],6;
mov byte [4],'O';
mov byte [5],6;
mov byte [6],'T';
mov byte [7],6;

;jmp readok



xor ax,ax
mov ds,ax

xor di,di
mov ss,ax
mov sp,0xfffe

;setup initial vars (excluding dl whith is set)
mov ax,[0x7022]
mov ss,ax

mov ax,[0x7026]
mov es,ax

mov ax,[0x7028]
mov fs,ax

mov ax,[0x702a]
mov gs,ax

mov eax,[0x700c] ;edx
mov al,dl ;save dl
mov edx,eax



mov eax,[0x7000]
mov ebx,[0x7004]
mov ecx,[0x7008]
mov esi,[0x7010]
mov edi,[0x7014]
mov ebp,[0x7018]
mov esp,[0x701c]

lgdt [0x7030] ;restore gdt

push word [0x702c] ;restore eflags
popf

push ax
mov ax,[0x7024]
mov ds,ax
pop ax


beforeboot:
xchg bx,bx
;jmp beforeboot
;int 19h

jmp 0x0000:0x7c00

;test^^^^
global bochswaitforsipiloop
bochswaitforsipiloop:
nop
cpuid
nop
jmp bochswaitforsipiloop

sendstring16:
;ds:si=pointer to string
mov cl,[si]
call send16

inc si
cmp byte [si],0
jne sendstring16
ret


send16:

;param cl=byte to send
push ax
push dx

waitforready:
%ifdef SERIALPORT
%if SERIALPORT != 0
mov dx,SERIALPORT+5 ;3fdh
in al,dx
and al,0x20
cmp al,0x20
jne waitforready

mov dx,SERIALPORT ;0x3f8
mov al,cl
out dx,al
%endif
%endif

pop dx
pop ax
ret

printstring:
;ds:si points to string
push ax
push cx

printstring_loop:
mov cl,[si]
call printchar

inc si
cmp byte [si],0
jne printstring_loop

pop cx
pop ax
ret

printchar:
cmp cl,13
jne notnewline

call newline
ret


notnewline:
push es
push ax
push bx
;get the display buffer offset

mov ax,0xb800
mov es,ax

mov al,80 ;line
mul byte [display_y-movetoreal]  ;multiply 80 with the value in display_y

xor bx,bx
mov bl,byte [display_x-movetoreal]
add ax,bx
shl ax,1

push di
mov di,ax
mov [es:di],cl
mov byte [es:di+1],7
pop di

inc byte [display_x-movetoreal]
cmp byte [display_x-movetoreal],80
jb printchar_exit

call newline

printchar_exit:
call updatecursorpos
pop bx
pop ax
pop es
ret

newline:
mov byte [display_x-movetoreal],0
inc byte [display_y-movetoreal]

cmp byte [display_y-movetoreal],25
jb newline_exit

mov ax,0x601 ;scroll 1 line
mov bh,7
mov cx,0
mov dl,79
mov dh,24
int 0x10

dec byte [display_y-movetoreal]

newline_exit:
call updatecursorpos

ret

updatecursorpos:
push ax
push bx
push dx
mov ax,0x200
mov bh,0
mov dl,[display_x-movetoreal]
mov dh,[display_y-movetoreal]
int 0x10

pop dx
pop bx
pop ax
ret

printerror:
;cl contains the error number (0..9)
push si
mov si,(str_fail-movetoreal)
call printstring

push cx
add cx,48
call printchar
pop cx

mov si,(str_newline-movetoreal)
call printstring
pop si
ret

;cursor:
display_x:
db 0

display_y:
db 0

str_alive:
db "I am alive!",13,0

str_settingup:
db "Setting things up",13,0

str_launching:
db "Launching OS",13,0

str_failedtoload1:
db "Failed to read from the given boot disk",13,0

str_fail:
db "Failure at point ",0

str_newline:
db 13,0


str_givingup:
db "Failure to launch. Time to reboot",13,0

global vmxstartup_end
vmxstartup_end:


global realmode_inthooks
realmode_inthooks:

;;---------------------------realmode int hooks---------------------------;;
;For launchtime running in unrestricted mode
;alternatively, I could edit the EPT

global realmode_inthook_new12
realmode_inthook_new12:
nop
;db 0xf1
nop
push bp
call getip
getip:
pop bp



mov ax,[cs:bp+realmode_inthook_conventional_memsize-getip]
pop bp

;clear CF flag if it's set
push bp
mov bp,sp
;bp+0=old bp
;bp+2=rip
;bp+4=cs
;bp+6=rflags
and word [bp+6],0xFFFE
pop bp
iret

jmp realmode_inthook_jmp_to_original12

global realmode_inthook_new15
realmode_inthook_new15:
nop
pushf
nop
;db 0xf1
nop
nop
cmp ah,0x88
je realmode_inthook_new15_88

cmp ax,0xe801
je calldbvm

cmp ax,0xe820
je calldbvm

popf
;not a dbvm hooked situation
jmp realmode_inthook_jmp_to_original15
calldbvm:
popf

global realmode_inthook_calladdress
realmode_inthook_calladdress:
vmcall
;still here so it got handled
jc realmode_inthook_return_cf1

;cf=0
;clear CF flag if it's set
push bp
mov bp,sp
and word [bp+6],0xFFFE
pop bp
iret

realmode_inthook_return_cf1:
;cf=1
push bp
mov bp,sp
or word [bp+6],0x1
pop bp
iret

realmode_inthook_new15_88:
mov ax,0xfc00  ;64MB

;clear CF flag if it's set
push bp
mov bp,sp
and word [bp+6],0xFFFE
pop bp

iret



global realmode_inthook_original12
realmode_inthook_jmp_to_original12:
db 0xea
realmode_inthook_original12:
dd 0

global realmode_inthook_original15
realmode_inthook_jmp_to_original15:
db 0xea
realmode_inthook_original15:
dd 0

global realmode_inthook_conventional_memsize
realmode_inthook_conventional_memsize:
dw 0

global realmode_inthooks_end
realmode_inthooks_end:


dd 0x00ce1337
db 0x90
db 0x90
db 0x90

db 0xea
db 0xce
db 0xce
db 0xaa
db 0xbb

