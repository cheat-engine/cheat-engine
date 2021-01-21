#define STR1(u) # u
#define pass(a) a
#define __ASM_REG(reg)         STR1(one##reg)
#define _ASM_DX         __ASM_REG(tok)
X162 pass(__ASM_REG(tok))
X161 pass(_ASM_DX)
X163 pass(STR1(one##tok))

X170 pass(x ## y)
X171 pass(x pass(##) y)

#define Y(x) Z(x)
#define X Y
X180 return X(X(1));
