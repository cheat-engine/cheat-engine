BITS 64

;param passing in 64-bit (linux ABI, NOT windows)
;1=rdi
;2=rsi
;3=rdx
;4=rcx

extern vbe_initptr
extern stack16

global doVBEINIT
doVBEINIT: ;address 0 of the callsegment (0x138)
push r15
mov r15,rsp

;first switch to a 32-bit code segment with the segment within 0x10000
mov si,8
mov ss,si
mov ds,si
mov es,si

mov rsp,0xf000fff0 ;stack16

call far [doVBEINIT_32bitlimitedcodeaddress]

mov si,8
mov ss,si
mov ds,si
mov es,si

mov rsp,r15
pop r15
ret

doVBEINIT_32bitlimitedcodeaddress:
dd doVBEINIT_32bitcode-doVBEINIT
dw 0x138

BITS 32
doVBEINIT_32bitcode:

;mov cx,0x26*8
;mov ss,cx ;set up a 16-bit stack
push 0x26*8
pop ss
and esp,0xffff

db 0x66
call far [vbe_initptr]
;db 0x66 ;test
;call far [doVBEINIT_16bitlimitedcodeaddress]

mov si,8
mov ss,si
or esp,0xf0000000

retf


BITS 64
;doVBE Call this from vbe stubs
doVBE: ;address 0 of the callsegment (0x138)
nop
;db 0xf1
nop

mov rsi,0xf
mov cr8,rsi

push r15
mov r15,rsp

mov si,8
mov ss,si
;mov ds,si
;mov es,si

;first switch to a 32-bit code segment with the segment within 0x10000
mov rsp,0xf000fff0 ;stack16
call far [doVBE_32bitlimitedcodeaddress] ;I could probably do it directly to 16 bit
mov si,8
mov ss,si
mov ds,si
mov es,si

mov rsp,r15
pop r15
ret

doVBE_32bitlimitedcodeaddress:
dd doVBE_32bitcode-doVBEINIT
dw 0x138

doVBE_16bitlimitedcodeaddress:
dw doVBE_16bitcode-doVBEINIT
dw 0x140

global vbe_entry
vbe_entry:
dw 0
dw 0

BITS 32
doVBE_32bitcode:

mov si,0x30*8
mov ss,si
and esp,0xffff
db 0x66
call far [doVBE_16bitlimitedcodeaddress]

nop
mov si,8
mov ss,si
or esp,0xf0000000
nop
nop
retf


bits 16

doVBE_16bitcode:

mov si,0x26*8;
mov ss,si
nop
nop
push bp
mov bp,sp
pushf ;it's an interrupt call
call far [cs:vbe_entry-doVBEINIT]
nop
mov sp,bp
pop bp
retf

;VBE functions:
;VBEcall_GetControllerInfo(WORD dataselector)

BITS 64
global VBEcall_GetControllerInfo
VBEcall_GetControllerInfo:
mov ax,0x4f00
mov es,di
xor di,di
call doVBE
mov si,8
mov es,si
ret

;int VBEcall_GetModeInfo(WORD mode, WORD dataselector);
global VBEcall_GetModeInfo
VBEcall_GetModeInfo:
mov ax,0x4f01
mov cx,di
mov es,si
xor di,di
call doVBE
mov si,8
mov es,si
ret

;int VBEcall_SetMode(WORD mode, WORD dataselector);
global VBEcall_SetMode
VBEcall_SetMode:
push rbx
mov ax,0x4f02
mov bx,di
mov es,si
xor di,di
call doVBE

mov si,8
mov es,si

pop rbx
ret


;int VBEcall_SaveRestoreState(BYTE operationmode,WORD stateflags, WORD dataselector)
global VBEcall_SaveRestoreState
VBEcall_SaveRestoreState:
push rbx
push rcx

mov ax,0x4f04
mov bx,di
mov cx,si
mov si,dx
mov es,si
xor di,di
xor bx,bx
call doVBE

mov si,8
mov es,si
pop rcx
mov [rcx],bx
pop rbx
ret

global VBEcall_ResetStart
VBEcall_ResetStart:
mov ax,0x4f07

push rbx
push rcx
push rdx
xor rbx,rbx
xor rcx,rcx
xor rdx,rdx

mov bl,2

call doVBE

pop rdx
pop rcx
pop rbx
ret
