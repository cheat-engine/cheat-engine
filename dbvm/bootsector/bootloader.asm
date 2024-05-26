;16-bit bootsector
;this will load the bigger loader to address 3000:0000
;
BITS 16
GLOBAL main
;SECTION .text
main:
jmp short loader ;0-1
nop ;2

OEMName:        db 'CETC2.1' ;3-9
dw 0 ;a-b (set by make)
dw 0 ;c-d (set by make)
bootdrive:        db 0 ;e (set during boot)
bootdrive2:     db 0 ;f (set by installer)
userdefinedbootdrive: db 0; 10 (set by installer, when set to 1 bootdrive2 is used instead of bootdrive+1 or if from floppy/cd 0x80)
runwithoutmenu:   db 0; 11 (set by installer, when set to 1 the menu will be skipped)


db 0 ;12
dw 0x00 ;13-14
db 0; 0xf8 15
dw 0x0000 ;16-17
SPT: dw 62 ;18-19  for some reason my system won'y boot up with these missing so:
NOH: dw 65 ;1a-1b

;----------------------

SectorsPerTrack:  dw 0
NumberOfHeads:    dw 0
NumberOfCylinders:  dw 0
currenttracksector: dw 0x2
currenthead:    dw 0




loader:
;cflush
wbinvd
; ------------------------------
; Save the current CPU state   ;
; ------------------------------

; save old data segment and make new data segment zero
mov [cs:0x7000],eax ; save eax
mov ax,ds          ; data segment pointer
mov [cs:0x7024],ax ; save it
xor ax,ax          ; ax=0
mov ds,ax          ; data segment is zero now
; save general purpose registers
mov [0x7004],ebx
mov [0x7008],ecx
mov [0x700c],edx
mov [0x7010],esi
mov [0x7014],edi
mov [0x7018],ebp
mov [0x701c],esp

mov ax,ss ; save stack segment
mov [0x7022],ax
mov ax,es ; save extra segment
mov [0x7026],ax
mov ax,fs ; save general purpose segment
mov [0x7028],ax
mov ax,gs ; save general purpose segment
mov [0x702a],ax

; save the eflags
pushf
pop ax
mov [0x702c],ax 

cli ; clear the interrupt flag (ignore maskable interrupts)

sgdt [0x7030] ; save the current Global Descriptor Table Register


mov eax,cr0 ; save the control register
mov [start_CR0],eax

;setup gs segment
mov ax,0xb800
mov gs,ax

;basic initialization
mov ax,3000h
mov es,ax  ;destination to 0x3000
mov ax,cs
mov ds,ax  ;make sure data segment points to here

mov [bootdrive],dl ;store the bootdrive register in ds:[bootdrive] (so local segment)


xor ax,ax
mov ss,ax  ;might as well setup a place for the stack (0:7bf0)
mov sp,0x3ff0

call zeroUsableMemory
call clearscreen

mov si,msg
call writeline




sti ;re enable interrupts

push es
push ds



;reset disk
mov ax,0
int 13h

;get disk info
push es ;old version of bochs bypass
mov ax,0x6000
mov es,ax
mov di,0
mov ax,0800h
int 13h
pop es ;older version of bochs bug bypass (too lazy to recompile)


inc dh
mov [NumberOfHeads],dh
mov ax,cx
and ax,3fh
inc ax
mov [SectorsPerTrack],ax
mov al,ch
shr ch,6
mov ah,ch
mov [NumberOfCylinders],ah ;not used, but store it anyhow just for the fun of it


mov cx,(size/512)+1  ; number of sectors to read
mov bx,0x0000 ;offset to store at


;now read the vmloader program from the disk to 3000:0000+)

push cx


reader:
mov ax,0x0201 ;read 1 sector
mov cx,[currenttracksector]  ;track/sector
mov dh,[currenthead]
mov dl,[bootdrive] ; set dl to the bootdrive

clc
int 13h
sti ;some bioses may disable this on return
jnc reader_ok

reader_err:
mov ah,0
mov dl,[bootdrive]
int 13h
jmp reader

reader_ok:

;successfull read, adjust parameters for next sector
add bx,512 ;place to store at
cmp bx,0
jne checksector
;overflow, vmloader has become bigger than 64KB, we need a new segment (0x40000?)

mov bx,es
add bx,0x1000 ;to the next free segment, 0x3000->0x4000->0x5000
mov es,bx
xor bx,bx

checksector:
;sector
mov dx,[currenttracksector]
inc dl
mov [currenttracksector],dx

push bx
mov bl,'.'
call writechar
pop bx

cmp dl,[SectorsPerTrack]
jb adjusted_currentsector

mov dl,1
mov [currenttracksector],dx

;head
inc byte [currenthead]
mov dl,[currenthead]
cmp dl,[NumberOfHeads]
jne adjusted_head

mov byte [currenthead],0

;cylinder
inc dh
mov [currenthead],dx

adjusted_head:

adjusted_currentsector:
;check if done
pop cx
dec cx
cmp cx,0
push cx
jne reader


pop cx

pop ds
pop es

done:

mov dl,[bootdrive] ;just to be sure

mov dword [0x7c00],0 ;location of lock (0=free)

inc byte [currentline]

;done

jmp 3000h:0000h  ;0x30000


currentline: db 0
currentcolumn: dw 0


writechar:
push ax
mov al,80
mul byte [currentline] ;ax = al*currentline  (80*currentline)
add ax,[currentcolumn]
shl ax,1 ;multiply ax with 2

xchg ax,di

;ax=currentcolumn*currentline*2
mov byte [gs:di],bl
mov byte [gs:di+1],12

xchg ax,di

pop ax
inc word [currentcolumn]


ret


writeline:
mov ah,160
mov al,[currentline]
mul ah
mov di,ax

writeline_loop:
cmp byte [si],0
je writeline_done

mov al,[si]
mov [gs:di],al
mov byte [gs:di+1],15

add di,2
inc si
jmp writeline_loop

writeline_done:
inc byte [currentline]
ret

clearscreen:
push es
push gs
pop es
push cx
mov cx,2*80*25 / 4
rep stosd
pop cx
pop es
ret


; https://wiki.osdev.org/Detecting_Memory_(x86)#Detecting_Low_Memory
zeroUsableMemory:
push es
pusha
xor ax,ax
int 0x12 ;get low memory size in KB
test ax,ax
je zeroUsabeMemory_error

cmp ax,0x86
je zeroUsabeMemory_error

cmp ax,0x80
je zeroUsabeMemory_error

jmp zeroUsabeMemory_start
zeroUsabeMemory_error:
mov ax,600 ;just guess...

zeroUsabeMemory_start:
sub ax,64 ;take out 64KB as the wipe starts at 0x10000
xchg bx,ax
xor eax,eax
mov si,0x1000

zeroUsableMemory_clear:
mov es,si
mov cx,(0x1000/4)
xor di,di
rep stosd
add si,0x1000
sub bx,64


jnb zeroUsableMemory_clear
popa
pop es
ret


msg:
db 'DBVM BS',0


times 506-($-$$) db 0   ; Pad to 509 bytes

start_CR0:
        dd 0            ; storage location of original CR0

BootSignature:
        dw 0aa55h       ; BootSector signature ; 512 bytes
