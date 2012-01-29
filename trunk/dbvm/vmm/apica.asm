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
;void initcpus(QLONG apic_base)
;------------------------------;
global initcpus
initcpus:
mov byte [0x2000],12


mov dword [rdi+0x300],0xc4500


mov ecx,00100000h
waiter2:
dec ecx
loop waiter2


mov dword [rdi+0x300],0xc4630

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

