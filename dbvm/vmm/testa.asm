BITS 64

GLOBAL testBranch

testBranch:
push rax
nop
nop
nop
pushfq
pop rax
or eax,0x100 ;set TF flag
push rax
popfq ;Normal state this will cause a break
nop
nop
nop
jmp testBranch_1 ;branch state this will cause a break
nop
nop
nop
testBranch_1:
mov eax,0x123
mov eax,0x456
nop
nop
pop rax

ret

GLOBAL cloakTestFunction
GLOBAL cloakTestFunctionEnd
GLOBAL cloakTestFunctionInstruction

align 16,db 0
cloakTestFunction:
xor eax,eax
nop
nop
nop

cloakTestFunctionInstruction:
mov eax,12345678
nop
nop
nop
nop
ret
nop
nop
nop
cloakTestFunctionEnd:
