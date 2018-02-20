BITS 64
;param passing in 64-bit
;1=rdi
;2=rsi
;3=rdx
;4=rcx

extern APIC_ID
extern APIC_SVR
extern IA32_APIC_BASE

;------------------------------;
;void initcpus(QLONG apic_base, DWORD entrypage)
;------------------------------;
global initcpus
initcpus:
mov dword [rdi+0x300],0xc4500


mov ecx,00100000h
waiter2:
dec ecx
loop waiter2


shr esi,12
or esi,0xc4600
mov dword [rdi+0x300],esi  ;30000

;mov dword [rdi+0x300],0xc4630  ;30000

ret

;---------------------------------;
;unsigned int apic_getBootID(void);
;---------------------------------;
global apic_getBootID
apic_getBootID:
mov eax,[APIC_ID] ;apic_id is 64-bit, but this gets the first 32 bits, which is fine
mov eax,[eax]
and eax,0ff000000h
ret


BITS 16

;The AP Boot code.  This gets copied to entrypage before execution
global APbootcode
global APbootcodeEnd
APbootcode:
;change from real mode to protected/paged mode
jmp APbootcode_AfterVars

global APBootVar_CR3
APBootVar_CR3:
dq 0

global APBootVar_GDT
APBootVar_GDT:
times 24 DQ 0

global APBootVar_Size
APBootVar_Size:
dw 0
dd APBootVar_GDT-APbootcode

APBootVar_ToAPbootcode32:
dw 24
dd APbootcode32-APbootcode


APbootcode_AfterVars:
;cmp bx,0xce ;debug code so I can tell which cpu will run
;jne APbootcode_AfterVars

mov ax,cs
mov ss,ax
mov ds,ax
mov es,ax
mov sp,0xffe

shl eax,4
or dword [APBootVar_Size-APbootcode+2],eax
or dword [APBootVar_ToAPbootcode32-APbootcode+2],eax

db 0x66
lgdt [APBootVar_Size-APbootcode]


mov ebx,APbootcode32-APbootcode
add ebx,eax

mov eax,cr0
or eax,1
mov cr0,eax

db 0x67
;jmp far 16:(APBootVar_ToAPbootcode32-APbootcode);
jmp far 16:(APbootcode32-APbootcode);

bits 32
APbootcode32:

mov eax,[APBootVar_CR3-APbootcode]
mov cr3,eax

mov eax,0x30  ;PAE and PSE
mov cr4,eax

;set IA32_EFER_LME to 1 (enable 64 bits)
mov ecx,0xc0000080
rdmsr
or eax,0x100
wrmsr

mov eax,cr0
or eax,0x80000020 ;set the pg bit  (and ne bit)
;or eax,0x80000000 ;set the pg bit
mov cr0,eax

jmp 80:0x00400000 ; go to 64-bit
nop
nop
nop
nop
nop
jmp $ ;got here, error

times 4096-($-APbootcode) db 0   ; Pad to 4096 bytes



APbootcodeEnd:
