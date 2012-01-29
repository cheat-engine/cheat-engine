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
