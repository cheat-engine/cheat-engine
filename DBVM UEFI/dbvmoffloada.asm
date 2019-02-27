BITS 64

;param passing in 64-bit (linux ABI, NOT windows)
;1=rdi
;2=rsi
;3=rdx
;4=rcx

struc GDTDesc
 g_Limit resw 1
 g_Base resq 1
endstruc

struc OState
.cpucount   resq 1
.originalEFER   resq 1
.originalLME  resq 1
.idtbase    resq 1
.idtlimit   resq 1
.gdtbase    resq 1
.gdtlimit   resq 1
.cr0      resq 1
.cr2      resq 1
.cr3      resq 1
.cr4      resq 1
.dr7      resq 1
.rip      resq 1
.rax      resq 1
.rbx      resq 1
.rcx      resq 1
.rdx      resq 1
.rsi      resq 1
.rdi      resq 1
.rbp      resq 1
.rsp      resq 1
.r8       resq 1
.r9       resq 1
.r10      resq 1
.r11      resq 1
.r12      resq 1
.r13      resq 1
.r14      resq 1
.r15      resq 1
.rflags     resq 1
.cs       resq 1
.ss       resq 1
.ds       resq 1
.es       resq 1
.fs       resq 1
.gs       resq 1
.tr       resq 1
.ldt      resq 1
.cs_AccessRights  resq 1
.ss_AccessRights  resq 1
.ds_AccessRights  resq 1
.es_AccessRights  resq 1
.fs_AccessRights  resq 1
.gs_AccessRights  resq 1
.cs_Limit resq 1
.ss_Limit resq 1
.ds_Limit resq 1
.es_Limit resq 1
.fs_Limit resq 1
.gs_Limit resq 1
.fsbase     resq 1
.gsbase     resq 1
endstruc



EXTERN NewGDTDescriptor
EXTERN NewGDTDescriptorVA
EXTERN DBVMPML4PA
EXTERN TemporaryPagingSetupPA
EXTERN enterVMM2PA
EXTERN originalstatePA
EXTERN enterVMM2

EXTERN originalstate
EXTERN vmmPA
EXTERN InitStackPA

GLOBAL doSystemTest
doSystemTest:
  mov rax,0x402
  mov dr7,rax
  mov rax,dr7
  cmp rax,0x402
  je pass1

  ;fail test1
  mov rax,1
  ret

pass1:
  cpuid
  mov rax,dr7
  cmp rax,0x402
  je pass2

  ;fail test 2
  mov rax,2
  ret

pass2:
  xor rax,rax
  ret

GLOBAL enterVMM
enterVMM:
begin:
  xchg bx,bx ;trigger bochs breakpoint


  ;setup the GDT
  nop
  nop
  nop
  nop
  nop
  nop

  ;switch to identity mapped pagetable
 ; mov cr3,rdx
  ;jmp short weee
;weee:





  ;now jump to the physical address (identity mapped to the same virtual address)
  lea rax,[rel secondentry]
  lea r8,[rel enterVMM]
  sub rax,r8
  add rax,rsi ;add the physical address to the offset location

  jmp rax

secondentry:
  ;contrary to the 32-bit setup, we don't disable paging to make the switch to 64-bit, we're already there
  ;we can just set the CR3 value


;----------TEST----------
; waitforready:
; mov dx,0ec05h
; in al,dx
; and al,20h
; cmp al,20h
; jne waitforready
;
; mov dx,0ec00h
; mov al,'1'
; out dx,al
;^^^^^^^^TEST^^^^^^^^


  ;enable PAE and PSE (just to make sure)
  mov eax,30h
  mov cr4,rax

  mov cr3,rcx
  jmp short weee2
weee2:



  mov rax,rdi
  xor rbx,rbx
  mov ds,bx
  mov es,bx
  mov fs,bx
  mov gs,bx
  mov ss,bx
  ;mov rsp,rax

  mov rax,cr0
  or eax,10000h
  mov cr0,rax ;enable WP bit

  nop
  nop
  nop
  nop
  nop

  jmp far dword [rel vmmjump]

  ;jmp fword ptr [vmmjump] ;one thing that I don't mind about x64, relative addressing, so no need to change it by me
 ;; jmp far [vmmjump]


extrastorage:
  nop
  nop
  nop
  nop
  nop

  dd 0
vmmjump:
  dd 00400000h
  dw 50h


detectionstring:
  db 0ceh
  db 0ceh
  db 0ceh
  db 0ceh
  db 0ceh
  db 0ceh
  db 0ceh


GLOBAL enterVMMPrologue
enterVMMPrologue:
  cli ;goodbye interrupts

  push rbx
  mov rbx,[rel originalstate]
  mov [rbx + OState.rax ],rax
  pop rbx

  mov rax,[rel originalstate]
  mov [rax+OState.rbx],rbx
  mov [rax+OState.rcx],rcx
  mov [rax+OState.rdx],rdx
  mov [rax+OState.rsi],rsi
  mov [rax+OState.rdi],rdi
  mov [rax+OState.rbp],rbp
  mov [rax+OState.rsp],rsp
  mov [rax+OState.r8],r8
  mov [rax+OState.r9],r9
  mov [rax+OState.r10],r10
  mov [rax+OState.r11],r11
  mov [rax+OState.r12],r12
  mov [rax+OState.r13],r13
  mov [rax+OState.r14],r14
  mov [rax+OState.r15],r15

  lea rbx,[rel enterVMMEpilogue]
  mov [rax+OState.rip],rbx

  ;jmp enterVMMEpilogue ;test to see if the loader is bugged

  ;still here, loader didn't crash, start executing the move to the dbvm environment
  xchg bx,bx ;bochs break

  mov rbx,[rel NewGDTDescriptorVA]
  lgdt [rbx]

  mov rcx,[rel DBVMPML4PA]
  ;mov rcx,[rel pagedirptrbasePA]
  mov rdx,[rel TemporaryPagingSetupPA]
  mov rsi,[rel enterVMM2PA]
  ;mov rdi,[rel InitStackPA]

  ;mov r8,enterVMM2
  jmp [rel enterVMM2]

global enterVMMEpilogue
enterVMMEpilogue:
  nop
  nop
  push rax
  push rbx
  push rcx
  push rdx
  cpuid
  pop rdx
  pop rcx
  pop rbx
  pop rax
  nop
  nop
  nop





  mov r8,[rel originalstate]

  mov rbx,[r8+OState.ss]
  mov ss,bx
  mov rbx,[r8+OState.ds]
  mov ds,bx
  mov rbx,[r8+OState.es]
  mov es,bx
  mov rbx,[r8+OState.fs]
  mov fs,bx
  mov rbx,[r8+OState.gs]
  mov gs,bx



  mov rcx,0c0000100h
  mov rax,[r8+OState.fsbase]
  mov rdx,rax
  shr rdx,32
  wrmsr

  mov rcx,0c0000101h
  mov rax,[r8+OState.gsbase]
  mov rdx,rax
  shr rdx,32
  wrmsr





  mov rax,[rel originalstate]
  mov rbx,[rax+OState.rbx]
  mov rcx,[rax+OState.rcx]
  mov rdx,[rax+OState.rdx]
  mov rsi,[rax+OState.rsi]
  mov rdi,[rax+OState.rdi]
  mov rbp,[rax+OState.rbp]
  mov rsp,[rax+OState.rsp]
  mov r8,[rax+OState.r8]
  mov r9,[rax+OState.r9]
  mov r10,[rax+OState.r10]
  mov r11,[rax+OState.r11]
  mov r12,[rax+OState.r12]
  mov r13,[rax+OState.r13]
  mov r14,[rax+OState.r14]
  mov r15,[rax+OState.r15]
  mov rax,[rax+OState.rax]

  ;crashtest
  ;mov rax,0deadh
  ;mov [rax],rax

  ;sti
  ret
  nop
  nop
  nop
