;Loaded by the bootloader
;This will load the highlevel vmm code at the highest memory
;location possible, and setup the system to protected and paged mode

BITS 16
GLOBAL amain
amain:

;should be 3000:0000
;load vmm code into memory
;
;

cli




waitforopening:

mov esi,edx
cpuid ;serialize
mov edx,esi

mov ax,0
mov ds,ax

cmp dword [0x7c00],0
je trytolock
pause
jmp waitforopening

trytolock:
mov eax,1
xchg eax,dword [0x7c00] ;try to lock
cmp eax,0 ;test if successful
jne waitforopening ;nope, something else already made it 1

;continue
sti
jmp real_entry


column:
db 0

row:
db 0

display16:
;cl=byte to send
push ax
push dx

mov dl, byte [ds:bp+column-begin]
mov dh, byte [ds:bp+row-begin]


mov ax,0xb800
mov es,ax

mov al,dh
mov ah,160
imul ah ; ax=ah*al  (row*160)

;add the column
push dx
and dx,0x00ff ;remove the row part of the dx register
shl dx,1 ;multiply by 2
add ax, dx
pop dx

xchg si,ax

;mov si,0


mov byte [es:si], cl ;//dl ;cl
mov byte [es:si+1], 15

;mov byte [es:si+2], dh
;mov byte [es:si+3], 15

xchg si,ax


inc dl
cmp dl, 80
jae nextrow
jmp display16_done

nextrow:
mov dl,0 ;set column to 0
inc dh ;go to next row

cmp dh,25
jb display16_done

mov dh,0


display16_done:
mov byte [ds:bp+column-begin], dl
mov byte [ds:bp+row-begin], dh
pop dx
pop ax
ret


;some 16-bit general purpose functions
sendstring16:
;ds:si=pointer to string
mov cl,[si]
call send16

inc si
cmp byte [si],'$'
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

inttostr16:
;ax = value
;di = pointer to buffer
;si = start of string (output)
;cx = max length of buffer
push ax
push bx
push cx
push di


mov si,di
add si,cx
dec si
mov byte [si],'$'
dec si

;worker
inttostr16_worker:
cmp si,di
jb inttostr16_end ; end of stringspace

xor dx,dx
mov bx,10
div bx

add dl,030h
mov [si],dl
dec si

cmp ax,0
jne inttostr16_worker

mov cx,si
inc cx

inttostr16_fillspace:
cmp si,di
jb inttostr16_end
mov byte [si],0x20
dec si

jmp inttostr16_fillspace



inttostr16_end:
mov si,cx

pop di
pop cx
pop bx
pop ax

ret



inttohex16:
;ax = value
;di = pointer to buffer
;si = start of string (output)
;cx = max length of buffer
push ax
push bx
push cx
push di


mov si,di
add si,cx
dec si
mov byte [si],'$' ;make this 0
dec si

;worker
inttohex16_worker:
cmp si,di
jb inttohex16_end ; end of stringspace

xor dx,dx
mov bx,16
div bx

cmp dl,09h
ja alpha
add dl,'0' ; add from '0'
jmp writehex

alpha:
sub dl,0ah
add dl,'a' ; add from 'a'



writehex:
mov [si],dl
dec si

cmp ax,0
jne inttohex16_worker

mov cx,si
inc cx

inttohex16_fillspace:
cmp si,di
jb inttohex16_end
mov byte [si],0x20
dec si

jmp inttohex16_fillspace



inttohex16_end:
mov si,cx

pop di
pop cx
pop bx
pop ax

ret

generatestring16:


str88: db '88=$'
stre801: db 'e801:$'
stre820: db 'e820:',13,10,'$'
strax: db 'ax=$'
strbx: db 'bx=$'
strcx: db 'cx=$'
strdx: db 'dx=$'
strsmaperror: db 'SMAP ERROR!',13,10,'$'
tempbuf: db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

newline: db 13,10,'$'

GLOBAL reservedmem_listcount
reservedmem_listcount: dd 0

GLOBAL readerror
readerror: dd 0

global sectorsread
sectorsread: dd 0

global a20state
a20state: dd 0

global bootdisk
bootdisk: dd 0

global SectorsPerTrack
SectorsPerTrack:	dd 0

global NumberOfHeads
NumberOfHeads:		dd 0

global NumberOfCylinders
NumberOfCylinders: 	dd 0

vesa_info_success:
  db "get VESA info success",13,10,"$"

vesa_info_fail:
  db "get VESA info failed",13,10,"$"

str_failed_getting_data:
  db "   Failure"

str_videomode:
  db "videomode 0x$"



checka20:
  push ds
  push es

  xor ax,ax
  mov ds,ax
  dec ax
  mov es,ax

  mov ax,[es:10h]       ; read word at FFFF:0010 (1 meg)
  not ax                ; 1's complement
  push word [0]         ; save word at 0000:0000 (0)
  mov [0],ax            ; word at 0 = ~(word at 1 meg)
  mov ax,[0]            ; read it back
  cmp ax,[es:10h]       ; fail if word at 0 == word at 1 meg
  pop word [0]

  pop es
  pop ds
  je checka20_noa20 ;it's the same

  mov ax,1
  ret

checka20_noa20:
  mov ax,0
  ret
;^^^^^^^^^^^^^^^^^^^^^

retry: db 'retry reading disk',13

waitforkeypress:
%ifdef SERIALPORT
%if SERIALPORT != 0
mov dx,SERIALPORT+5 ;0x3fd
waitforkeypress2:
in al,dx
and al,1
cmp al,1
jne waitforkeypress2
mov dx,SERIALPORT ;0x3f8
in al,dx
%endif
%endif
ret


real_entry:

;ok

;setup rs232 (serial port communications)



;ok

;-----------------------Real Entry point--------------------
mov ax,0x8000
cli
mov ss,ax
mov sp,0xfff0
sti

xor ebp,ebp
xor esi,esi
mov ax,cs
mov ds,ax
mov es,ax

start:
call begin
begin:
pop bp ; bp now contains the address of begin

;ok



lea di,[ds:bp+reservedmem_listcount-begin]
cmp byte [ds:di],0
jne gdtsetup

mov [ds:bp+bootdisk-begin],dl ;save bootdisk

cmp byte [bp+reservedmem_listcount-begin],0
jne gdtsetup ;skip setting up hardware and enumerating memory, it's already done

%ifdef SERIALPORT
%if SERIALPORT !=0
;jmp serialsetupdone
mov dx,SERIALPORT+1 ;3f9h
mov al,0h
out dx,al

mov dx,SERIALPORT+3; 3fbh
mov al,80h
out dx ,al ;access baud rate generator

mov dx,SERIALPORT; 3f8h
mov al,1h  ;0c=9600 (1152000/divisor)
out dx,al ;9600 baud

mov dx,SERIALPORT+1; 3f9h
mov al,0h ;high part of devisor
out dx,al ;

mov dx,SERIALPORT+3; 3fbh
mov al,3h
out dx,al ;8 bits, no parity, one stop
%endif
%endif

serialsetupdone:



;begin with setting up diskinfo
mov ax,0x6000
mov es,ax
xor di,di

mov ax,0x0800
mov dl,[ds:bp+bootdisk-begin]
int 13h

inc dh
mov [ds:bp+NumberOfHeads-begin],dh
mov ax,cx
and ax,3fh
inc ax
mov [ds:bp+SectorsPerTrack-begin],ax
shr cx,6
inc cx
mov [ds:bp+NumberOfCylinders-begin],cx


;enumerate the memory regions that can not be used by dbos
;int 15h

;init buffer
mov ax,08000h
mov es,ax
mov di,0
mov ebx,0

enummem:
;mov byte [es:0xdead],2
;enumerate the memory regions available to the operating system and save them
mov eax,0e820h
mov ecx,20
mov edx,0534D4150h


int 15h

jc endofenummem
cmp eax,0534D4150h
jne endofenummem

cmp ecx,20
jne endofenummem

push di
lea di,[ds:bp+reservedmem_listcount-begin]
inc dword [ds:di]
pop di
add di,20

cmp ebx,0
jne enummem ;still more to go

endofenummem:



lea di,[ds:bp+reservedmem_listcount-begin]
mov ax,word [di]

lea di,[ds:bp+tempbuf-begin]
mov cx,16
call inttohex16



lea si,[ds:bp+strnormal-begin]
call sendstring16


mov cl,';'
call send16

;call waitforkeypress


gdtsetup:




;enable a20
in al,92h
test al,02h
jne hasa20

or al,02h
out 92h,al


noa20:
hasa20:

call checka20
lea di,[bp+a20state-begin]
mov [di],ax


;------------------GDT setup
mov ax,0x4000
mov es,ax

mov word [es:0],0x6f
mov dword [es:2],0x50000

; 0x3000:0x02b3
mov ax,0x5000
mov es,ax
mov dword [es:0],0
mov dword [es:4],0
mov dword [es:8],0x0000ffff   ;32-bit data
mov dword [es:12],0x00cf9200
mov dword [es:16],0x0000ffff  ;stack (unused)
mov dword [es:20],0x00cf9600

;lea ax,[bp+entry32-begin]
;shl eax,16
;add eax,0xffff
mov dword [es:24],0x0000ffff  ;32-bit code
mov dword [es:28],0x00cf9b00  ;limit=ffff*4KB base=0



mov dword [es:32],0x0000ffff  ;16-bit code
mov dword [es:36],0x00009a00

mov dword [es:40],0x0000ffff  ;16-bit data
mov dword [es:44],0x00009200

mov dword [es:48],0x0000ffff  ;16-bit code, base 0x30000
mov dword [es:52],0x00009a03

mov dword [es:56],0  ;32-bit task , main vmm task
mov dword [es:60],0

mov dword [es:64],0  ;64-bit task , main vmm64 task
mov dword [es:68],0

mov dword [es:72],0  ;
mov dword [es:76],0

;64-bit code segment
mov dword [es:80], 0        ;64-bit code (there is no limit checking)
mov dword [es:84], 0x00A09A00
mov dword [es:88],0
mov dword [es:92],0

;64bit tss descriptor (initialized later)
mov dword [es:96], 0
mov dword [es:100],0
mov dword [es:104],0
mov dword [es:108],0

;64-bit data segment
;there is none...

mov ax,0x4000
mov fs,ax
db 0x66
LGDT [fs:0]




mov ax,0xffff
mov ds,ax



;setup to protected mode
cli ;no more ints for now

mov ebx,cr0
or ebx,1
mov cr0,ebx

jmp short shortjump
shortjump:

nop
nop
mov bx,cs

bits 32
db 0x66
jmp 24:entry32

;----------------------------------------------------------;
;					PROTECTED MODE :32 bit section         ;
;----------------------------------------------------------;
bits 32




jmp short entry32

extern _vmloader_main

strnormal:
db 'normal',13,10,'$'

strcalledlgdt:
db 'Called LGDT (not)',13,10,'$'

sendstring32:
;esi=pointer to zstring
mov cl,[esi]
call sendchar32

inc esi
cmp byte [esi],0
jne sendstring32
ret


sendchar32:
%ifdef SERIALPORT
%if SERIALPORT != 0
mov edx,SERIALPORT+5 ;3fdh
in al,dx
and al,0x20
cmp al,0x20
jne sendchar32


mov edx,SERIALPORT; 0x3f8
mov al,cl
out dx,al
%endif
%endif
ret


entry32:

shl ebx,16
mov bx,ds

mov cx,es
shl ecx,16
mov cx,fs


;set segments to valid entries (ss is kinda important)
mov ax,8
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax

mov ax,8
mov ss,ax
mov esp,0x003ffffc ;4mb , the user should have that at least, else i will crash anyhow

mov [readerror],ebx
mov [sectorsread],ecx

;zero the stack
xor eax,eax
mov edi,0x003f0000
mov ecx,0xffff
rep stosb

;0018:0303de
call _vmloader_main  ; 0x30452


mov al,1
lp:
mov byte [0x0b8000],'l'
mov byte [0x0b8001],al
inc al
mov byte [0x0b8002],'o'
mov byte [0x0b8003],al
inc al
mov byte [0x0b8004],'s'
mov byte [0x0b8005],al
inc al
mov byte [0x0b8006],'e'
mov byte [0x0b8007],al
inc al
mov byte [0x0b8008],'r'
mov byte [0x0b8009],al
inc al


jmp lp

strInsideVMM: db 'Inside gotoVMM',13,10,0
strSetGDT: db 'Have set the GDT',13,10,0

global pagetablebase
pagetablebase: dd 0

global pagedirptrbase
pagedirptrbase: dd 0

global vmmPA
vmmPA: dd 0

global gotoVMM
gotoVMM:
;the new gdt address is at VMMSIZE+1*4096 & 0xfff
;the pagetablebase (cr3) should be at VMMSIZE+2*4096 & 0xfffff000

mov ebx,[vmmPA]

mov esi,strInsideVMM
call sendstring32



mov eax,[pagedirptrbase]
mov cr3,eax

;set the gdt
mov word [0x40000],0x6f
mov dword [0x40002],0x00400000+VMMSIZE+1*4096 & 0xfffff000
lgdt [0x40000]

mov esi,strSetGDT
call sendstring32


mov dl,[bootdisk] ;so the vmm knows the bootdisk, for whatever reason it migth need it

;mov eax,cr4
;or eax,0x30 ; enable PAE and PSE (to enable 2MB pages)
;mov cr4,eax
mov eax,0x30
mov cr4,eax


;set IA32_EFER_LME to 1 (enable 64 bits)
mov ecx,0xc0000080
rdmsr
or eax,0x100
wrmsr

mov eax,cr0
or eax,0x80000020 ;set the pg bit  (and ne bit)
mov cr0,eax
xor eax,eax ;tell the vmm that it's loaded from a clean setup



jmp 80:0x00400000 ; go to 64-bit

global halt
;----------------------------;
;int halt(void);
;----------------------------;
halt:
hlt
jmp halt

global readsectorasm
;----------------------------;
;int readsectorasm(void);
;----------------------------;
;7000:0000 contains the data to read, 7000:8000 the data needed to return
;6000:0000 points to the data to fill in
;This routine will switch to realmode , execute int13, and then switch back to realmode
readsectorasm:
;save state
mov dword [0x00078000],eax
mov dword [0x00078004],ebx
mov dword [0x00078008],ecx
mov dword [0x0007800c],edx
mov dword [0x00078010],esi
mov dword [0x00078014],edi
mov dword [0x00078018],ebp
mov dword [0x0007801c],esp


mov eax,readsector16protected
and eax,0xffff
mov dword [0x00078020],eax
mov dword [0x00078024],48  ;16-bit code segment starting at 0x30000



nop
nop
nop
jmp far dword [0x00078020]

readsector16protected:
;in 16bit protected mode now

bits 16
mov ax,40 ;set segment registers to 16-bit
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax
mov ss,ax

;switch to real
xor eax,eax
mov cr0,eax

mov ax,0x6000
mov ds,ax
mov eax,readsector16
mov word [0],ax
mov word [2],0x3000
jmp far word [0]  ;refresh cs to point to proper base

readsector16:


;realmode here
mov ax,0x7000
mov ds,ax
mov ax,0x6000 ;target to write to (512 bytes)
mov es,ax

mov ss,ax
mov sp,0xfffe ;end of that segment

sti

;call int13
mov ax,0x0201 ;read 1 sector
xor bx,bx ;write to 0x0 ;(so ds:bx=7000:0000)

;mov word [0xdead],0xdead  ;;debug for bochs (watch write 0x7dead)

mov cl,[0]  ;sector
mov dh,[1]  ;head
mov ch,[2]  ;cylinder
mov dl,[3]  ;drive

int 0x13

jc readsector_error
mov eax,1
jmp readsector_afterread

readsector_error:
xor eax,eax


readsector_afterread:
mov bx,0x7000
mov ds,bx
mov dword [0x8000],eax ;set new eax value

;go back to the protected mode caller
cli ;no more ints for now

mov ebx,cr0
or ebx,1
mov cr0,ebx

jmp short readsector_sj
readsector_sj:

nop
nop

bits 32
db 0x66
jmp 24:readsector_return

readsector_return:
mov ax,8
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax
mov ss,ax

mov eax,dword [0x00078000]
mov ebx,dword [0x00078004]
mov ecx,dword [0x00078008]
mov edx,dword [0x0007800c]
mov esi,dword [0x00078010]
mov edi,dword [0x00078014]
mov ebp,dword [0x00078018]
mov esp,dword [0x0007801c]

ret
