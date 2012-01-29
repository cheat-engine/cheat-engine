/*
x86defs.c

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#include "x86defs.h"
#include "instructions.h"


/* NOTE: str_x86def is optimized to copy only 8 bytes from these strings. */

_DefText _CONDITIONS[16] = {{1, "O"}, {2, "NO"}, {1, "C"}, {2, "NC"}, {1, "Z"}, {2, "NZ"}, {2, "BE"}, {1, "A"}, {1, "S"}, {2, "NS"}, {1, "P"}, {2, "NP"}, {1, "L"}, {2, "GE"}, {2, "NG"}, {1, "G"}};
_DefText _CONDITIONS_PSEUDO[8] = {{2, "EQ"}, {2, "LT"}, {2, "LE"}, {5, "UNORD"}, {3, "NEQ"}, {3, "NLT"}, {3, "NLE"}, {3, "ORD"}};

_DefText _CREGS[CREGS_MAX] = {{3, "CR0"}, {0, ""}, {3, "CR2"}, {3, "CR3"}, {3, "CR4"}, {0, ""}, {0, ""}, {0, ""}, {3, "CR8"}};
_DefText _DREGS[DREGS_MAX] = {{3, "DR0"}, {3, "DR1"}, {3, "DR2"}, {3, "DR3"}, {0, ""}, {0, ""}, {3, "DR6"}, {3, "DR7"}};
_DefText _SREGS[SEG_REGS_MAX] = {{2, "ES"}, {2, "CS"}, {2, "SS"}, {2, "DS"}, {2, "FS"}, {2, "GS"}};

_DefText _REGS8[16] = {{2, "AL"}, {2, "CL"}, {2, "DL"}, {2, "BL"}, {2, "AH"}, {2, "CH"}, {2, "DH"}, {2, "BH"}, {3, "R8B"}, {3, "R9B"}, {4, "R10B"}, {4, "R11B"}, {4, "R12B"}, {4, "R13B"}, {4, "R14B"}, {4, "R15B"}};
_DefText _REGS8_REX[16] = {{2, "AL"}, {2, "CL"}, {2, "DL"}, {2, "BL"}, {3, "SPL"}, {3, "BPL"}, {3, "SIL"}, {3, "DIL"}, {3, "R8B"}, {3, "R9B"}, {4, "R10B"}, {4, "R11B"}, {4, "R12B"}, {4, "R13B"}, {4, "R14B"}, {4, "R15B"}};
_DefText _REGS16[16] = {{2, "AX"}, {2, "CX"}, {2, "DX"}, {2, "BX"}, {2, "SP"}, {2, "BP"}, {2, "SI"}, {2, "DI"}, {3, "R8W"}, {3, "R9W"}, {4, "R10W"}, {4, "R11W"}, {4, "R12W"}, {4, "R13W"}, {4, "R14W"}, {4, "R15W"}};
_DefText _REGS32[16] = {{3, "EAX"}, {3, "ECX"}, {3, "EDX"}, {3, "EBX"}, {3, "ESP"}, {3, "EBP"}, {3, "ESI"}, {3, "EDI"}, {3, "R8D"}, {3, "R9D"}, {4, "R10D"}, {4, "R11D"}, {4, "R12D"}, {4, "R13D"}, {4, "R14D"}, {4, "R15D"}};
_DefText _REGS64[16] = {{3, "RAX"}, {3, "RCX"}, {3, "RDX"}, {3, "RBX"}, {3, "RSP"}, {3, "RBP"}, {3, "RSI"}, {3, "RDI"}, {2, "R8"}, {2, "R9"}, {3, "R10"}, {3, "R11"}, {3, "R12"}, {3, "R13"}, {3, "R14"}, {3, "R15"}};

_DefText _REGSMMX[8] = {{3, "MM0"}, {3, "MM1"}, {3, "MM2"}, {3, "MM3"}, {3, "MM4"}, {3, "MM5"}, {3, "MM6"}, {3, "MM7"}};
_DefText _REGSSSE[16] = {{4, "XMM0"}, {4, "XMM1"}, {4, "XMM2"}, {4, "XMM3"}, {4, "XMM4"}, {4, "XMM5"}, {4, "XMM6"}, {4, "XMM7"}, {4, "XMM8"}, {4, "XMM9"}, {5, "XMM10"}, {5, "XMM11"}, {5, "XMM12"}, {5, "XMM13"}, {5, "XMM14"}, {5, "XMM15"}};

_DefText _MODS16[8] = {{6, "[BX+SI"}, {6, "[BX+DI"}, {6, "[BP+SI"}, {6, "[BP+DI"}, {3, "[SI"}, {3, "[DI"}, {3, "[BP"}, {3, "[BX"}};
_DefText _MODS32[16] = {{4, "[EAX"}, {4, "[ECX"}, {4, "[EDX"}, {4, "[EBX"}, {4, "[ESP"}, {4, "[EBP"}, {4, "[ESI"}, {4, "[EDI"}, {4, "[R8D"}, {4, "[R9D"}, {5, "[R10D"}, {5, "[R11D"}, {5, "[R12D"}, {5, "[R13D"}, {5, "[R14D"}, {5, "[R15D"}};
_DefText _MODS64[16] = {{4, "[RAX"}, {4, "[RCX"}, {4, "[RDX"}, {4, "[RBX"}, {4, "[RSP"}, {4, "[RBP"}, {4, "[RSI"}, {4, "[RDI"}, {3, "[R8"}, {3, "[R9"}, {4, "[R10"}, {4, "[R11"}, {4, "[R12"}, {4, "[R13"}, {4, "[R14"}, {4, "[R15"}};

/* There's no a base for EBP. */
_DefText _BASE32[16] = {{3, "EAX"}, {3, "ECX"}, {3, "EDX"}, {3, "EBX"}, {3, "ESP"}, {3, "EBP"}, {3, "ESI"}, {3, "EDI"}, {3, "R8D"}, {3, "R9D"}, {4, "R10D"}, {4, "R11D"}, {4, "R12D"}, {4, "R13D"}, {4, "R14D"}, {4, "R15D"}};
_DefText _BASE64[16] = {{3, "RAX"}, {3, "RCX"}, {3, "RDX"}, {3, "RBX"}, {3, "RSP"}, {3, "RBP"}, {3, "RSI"}, {3, "RDI"}, {2, "R8"}, {2, "R9"}, {3, "R10"}, {3, "R11"}, {3, "R12"}, {3, "R13"}, {3, "R14"}, {3, "R15"}};
/* There's no an index for rSP. */
_DefText _INDEX32[16] = {{3, "EAX"}, {3, "ECX"}, {3, "EDX"}, {3, "EBX"}, {0, ""}, {3, "EBP"}, {3, "ESI"}, {3, "EDI"}, {3, "R8D"}, {3, "R9D"}, {4, "R10D"}, {4, "R11D"}, {4, "R12D"}, {4, "R13D"}, {4, "R14D"}, {4, "R15D"}};
_DefText _INDEX64[16] = {{3, "RAX"}, {3, "RCX"}, {3, "RDX"}, {3, "RBX"}, {0, ""}, {3, "RBP"}, {3, "RSI"}, {3, "RDI"}, {2, "R8"}, {2, "R9"}, {3, "R10"}, {3, "R11"}, {3, "R12"}, {3, "R13"}, {3, "R14"}, {3, "R15"}};

_DefText _SCALE32[4] = {{0, ""} , {2, "*2"}, {2, "*4"}, {2, "*8"}};

int8_t FPU_STACK_TEXT[] = "ST";
int8_t ONE_CONST_TEXT[] = "0x1";
int8_t REG_RIP_TEXT[] = "RIP";

int8_t BYTE_UNDEFINED[] = "DB";
int8_t TEXT_8_BITS[] = "BYTE ";
int8_t TEXT_16_BITS[] = "WORD ";
int8_t TEXT_32_BITS[] = "DWORD ";
int8_t TEXT_64_BITS[] = "QWORD ";
int8_t TEXT_80_BITS[] = "TBYTE ";
int8_t TEXT_128_BITS[] = "DQWORD ";

int8_t PREFIX_LOCK_TEXT[] = "LOCK ";
int8_t PREFIX_REP_TEXT[] = "REP ";
int8_t PREFIX_REPNZ_TEXT[] = "REPNZ ";
int8_t PREFIX_CS_TEXT[] = "CS";
int8_t PREFIX_SS_TEXT[] = "SS";
int8_t PREFIX_DS_TEXT[] = "DS";
int8_t PREFIX_ES_TEXT[] = "ES";
int8_t PREFIX_FS_TEXT[] = "FS";
int8_t PREFIX_GS_TEXT[] = "GS";

int8_t SUFFIX_SIZE_BYTE = 'B';
int8_t SUFFIX_SIZE_WORD = 'W';
int8_t SUFFIX_SIZE_DWORD = 'D';
int8_t SUFFIX_SIZE_QWORD = 'Q';

int8_t SHORT_OPERAND[] = "SHORT ";
int8_t SMALL_OPERAND[] = "SMALL ";
int8_t LARGE_OPERAND[] = "LARGE ";

int8_t WAIT_INSTRUCTION_MNEMONIC[] = "WAIT";

_InstInfo II_arpl = {INT_INFO, ISCT_INTEGER, OT_REG16, OT_RM16, (int8_t*) "\x04" "ARPL", INST_INCLUDE_MODRM};
/*
 * 1.5.16 - MOVSXD is now being decoded properly, definition was incorrect.
 * When 0x63 is prefixed with 0x48, it becomes MOVSXD.
 * When 0x63 is NOT prefixed, it is MOVZXD. (Which is the same as MOV reg, reg in effectiveness, only in 64bits, of course).
 * Note: MOVZXD supports db 0x66 -> MOVZXDW RAX, AX.
 */
_InstInfoEx II_movsxd = {INT_INFO, ISCT_INTEGER, OT_RM16_32, OT_REG64, (int8_t*) "\x07" "MOVZXDW", INST_INCLUDE_MODRM | INST_USE_EXMNEMONIC | INST_USE_EXMNEMONIC2 | INST_PRE_REX | INST_64BITS, OT_NONE, OT_NONE, (int8_t*) "\x06" "MOVZXD", (int8_t*)"\x06" "MOVSXD"};

_InstInfo II_nop = {INT_INFO, ISCT_INTEGER, OT_NONE, OT_NONE, (int8_t*) "\x03" "NOP", INST_EXCLUDE_MODRM};
