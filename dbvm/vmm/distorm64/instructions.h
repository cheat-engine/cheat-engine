/*
instructions.h

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#ifndef INSTRUCTIONS_H
#define INSTRUCTIONS_H

#include "../config.h"

#include "decoder.h"
#include "prefix.h"

/*
 * Operand type possibilities:
 * Note "_FULL" suffix indicates to decode the operand as 16 bits or 32 bits depends on DecodeType -
 * actually, it depends on the decoding mode, unless there's an operand/address size prefix.
 * For example, the code: 33 c0 could be decoded/executed as XOR AX, AX or XOR EAX, EAX.
 */
typedef enum OpType {
	/* No operand is set */
	OT_NONE = 0,

	/* Read a byte(8 bits) immediate */
	OT_IMM8,
	/* Force a read of a word(16 bits) immediate, used by ret only */
	OT_IMM16,
	/* Read a word/dword immediate */
	OT_IMM_FULL,
	/* Read a double-word(32 bits) immediate */
	OT_IMM32,

	/* Special immediate for two instructions, AAM, AAD which will output the byte only if it's not 0xa (base 10) */
	OT_IMM_AADM,

	/* Read a signed extended byte(8 bits) immediate */
	OT_SEIMM8,

	/* Use a 8bit register */
	OT_REG8,
	/* Use a 16bit register */
	OT_REG16,
	/* Use a 16/32/64bit register */
	OT_REG_FULL,
	/* Use a 32bit register */
	OT_REG32,
	/* MOVSXD uses 64 bits register */
	OT_REG64,
	/*
	 * If used with REX the reg operand size becomes 64 bits, otherwise 32 bits.
	 * VMX instructions are promoted automatically without a REX prefix.
	 */
	OT_REG32_64,
	/* Extract a 32bit register from the RM field, used for instructions with register operands only */
	OT_REG32_RM,
	/* Used only by MOV CR/DR(n). Promoted with REX onlly. */
	OT_FREG32_64_RM,

	/* Use or read (indirection) a 8bit register or immediate byte */
	OT_RM8,
	/* Some instructions force 16 bits (mov sreg, rm16) */
	OT_RM16,
	/* Use or read a 16/32/64bit register or immediate word/dword/qword */
	OT_RM_FULL,
	/* Use or read a 32bit register or immediate dword */
	OT_RM32,
	/*
	 * 32 or 64 bits (with REX) operand size indirection memory operand.
	 * Some instructions are promoted automatically without a REX prefix.
	 */
	OT_RM32_64,
	/* 16 or 32 bits RM. This is used only with MOVZXD instruction in 64bits. */
	OT_RM16_32,
	/* Same as OT_RMXX but POINTS to 16 bits [cannot use GENERAL-PURPOSE REG!] */
	OT_FPUM16,
	/* Same as OT_RMXX but POINTS to 32 bits (single precision) [cannot use GENERAL-PURPOSE REG!] */
	OT_FPUM32,
	/* Same as OT_RMXX but POINTS to 64 bits (double precision) [cannot use GENERAL-PURPOSE REG!] */
	OT_FPUM64,
	/* Same as OT_RMXX but POINTS to 80 bits (extended precision) [cannot use GENERAL-PURPOSE REG!] */
	OT_FPUM80,

	/*
	 * Special operand type for SSE4 where the ModR/M might
	 * be a 32 bits register or 8 bits memory indirection operand.
	 */
	OT_R32_M8,
	/*
	 * Special ModR/M for PINSRW, which need a 16 bits memory operand or 32 bits register.
	 * In 16 bits decoding mode R32 becomes R16, operand size cannot affect this.
	 */
	OT_R32_M16,
	/*
	 * Special type for SSE4, ModR/M might be a 32 bits or 64 bits (with REX) register or
	 * a 8 bits memory indirection operand.
	 */
	OT_R32_64_M8,
	/*
	 * Special type for SSE4, ModR/M might be a 32 bits or 64 bits (with REX) register or
	 * a 16 bits memory indirection operand.
	 */
	OT_R32_64_M16,
	/*
	 * Special operand type for MOV reg16/32/64/mem16, segReg 8C /r. and SMSW.
	 * It supports all decoding modes, but if used as a memory indirection it's a 16 bit ModR/M indirection.
	 */
	OT_RFULL_M16,

	/* Use a control register */
	OT_CREG,
	/* Use a debug register */
	OT_DREG,
	/* Use a segment register */
	OT_SREG,
	/*
	 * SEG is encoded in the flags of the opcode itself!
	 * This is used for specific "push SS" where SS is a segment where
	 * each "push SS" has an absolutely different opcode byte.
	 * We need this to detect whether an operand size prefix is used.
	 */
	OT_SEG,
	
	/* Use AL */
	OT_ACC8,
	/* Use AX (FSTSW) */
	OT_ACC16,
	/* Use AX/EAX/RAX */
	OT_ACC_FULL,
	/* Use AX/EAX, no REX is possible for RAX, used only with IN/OUT which don't support 64 bit registers */
	OT_ACC_FULL_NOT64,

	/*
	 * Read one word (seg), and a word/dword/qword (depends on operand size) from memory.
	 * JMP FAR [EBX] means EBX point to 16:32 ptr.
	 */
	OT_MEM16_FULL,
	/* Read one word (seg) and a word/dword/qword (depends on operand size), usually SEG:OFF, JMP 1234:1234 */
	OT_PTR16_FULL,
	/* Read one word (limit) and a dword/qword (limit) (depends on operand size), used by SGDT, SIDT, LGDT, LIDT. */
	OT_MEM16_3264,

	/* Read a byte(8 bits) immediate and calculate it relatively to the current offset of the instruction being decoded */
	OT_RELCB,
	/* Read a word/dword immediate and calculate it relatively to the current offset of the instruction being decoded */
	OT_RELC_FULL,

	/* Use general memory indirection, with varying sizes: */
	OT_MEM,
	OT_MEM32,
	/* Memory dereference for MOVNTI, either 32 or 64 bits (with REX). */
	OT_MEM32_64,
	OT_MEM64,
	OT_MEM128,
	/* Used for cmpxchg8b/16b. */
	OT_MEM64_128,

	/* Read an immediate as an absolute address, size is known by instruction, used by MOV (offset) only */
	OT_MOFFS,
	/* Use an immediate of 1, as for SHR R/M, 1 */
	OT_CONST1,
	/* Use CL, as for SHR R/M, CL */
	OT_REGCL,

	/*
	 * Instruction-Block for one byte long instructions, used by INC/DEC/PUSH/POP/XCHG,
	 * REG is extracted from the value of opcode
	 * Use a 8bit register
	 */
	OT_IB_RB,
	/* Use a 32 or 64bit (with REX) register, used by BSWAP */
	OT_IB_R_DW_QW,
	/* Use a 16/32/64bit register */
	OT_IB_R_FULL,

	/* Use [(r)SI] as INDIRECTION, for repeatable instructions */
	OT_REGI_ESI,
	/* Use [(r)DI] as INDIRECTION, for repeatable instructions */
	OT_REGI_EDI,
	/* Use [(r)BX + AL] as INDIRECTIOM, used by XLAT only */
	OT_REGI_EBXAL,
	/* Use [(r)AX] as INDIRECTION, used by AMD's SVM instructions */
	OT_REGI_EAX,
	/* Use DX, as for OUTS DX, BYTE [SI] */
	OT_REGDX,
	/* Use ECX in INVLPGA instruction */
	OT_REGECX,

	/* FPU registers: */
	OT_FPU_SI, /* ST(i) */
	OT_FPU_SSI, /* ST(0), ST(i) */
	OT_FPU_SIS, /* ST(i), ST(0) */

	/* MMX registers: */
	OT_MM,
	/* Extract the MMX register from the RM bits this time (used when the REG bits are used for opcode extension) */
	OT_MM_RM,
	/* ModR/M points to 32 bits MMX variable */
	OT_MM32,
	/* ModR/M points to 32 bits MMX variable */
	OT_MM64,

	/* SSE registers: */
	OT_XMM,
	/* Extract the SSE register from the RM bits this time (used when the REG bits are used for opcode extension) */
	OT_XMM_RM,
	/* ModR/M points to 16 bits SSE variable */
	OT_XMM16,
	/* ModR/M points to 32 bits SSE variable */
	OT_XMM32,
	/* ModR/M points to 64 bits SSE variable */
	OT_XMM64,
	/* ModR/M points to 128 bits SSE variable */
	OT_XMM128,
	/* Implied XMM0 register as operand, used in SSE4. */
	OT_REGXMM0,

	/*
	 * DUMMY for cases like CALL WORD [BX+DI], we would like to omit this "WORD". It's useless,
	 * because the DWORD/WORD/BYTE mechanism is being done automatically, we need some way to disable it in such cases...
	 */
	OT_DUMMY
} _OpType;

/* Flags for instruction: */

/* Empty flags indicator: */
#define INST_FLAGS_NONE ((_iflags)-1)

/*
 * Explicitly define that the instruction doesn't require a ModRM byte.
 * NOTE its value is 0! you can't do much with it, it is used for instructions that for sure don't use the ModR/M byte.
 */
#define INST_EXCLUDE_MODRM (0)
/* The instruction we are going to decode has a ModR/M byte. */
#define INST_INCLUDE_MODRM (1)
/* Special treatment for instructions which are in the divided-category but still needs the whole byte for ModR/M... */
#define INST_NOT_DIVIDED (1 << 1)
/*
 * Used explicitly in repeatable instructions,
 * which needs a suffix letter in their mnemonic to specify operation-size (depend on operands).
 */
#define INST_16BITS (1 << 2)
/* If the opcode is supported by 80286 and upper models (16/32 bits). */
#define INST_32BITS (1 << 3)
/*
 * Prefix flags (4 types: lock/rep, seg override, addr-size, oper-size)
 * There are several specific instructions that can follow LOCK prefix,
 * note that they must be using a memory operand form, otherwise they generate an exception.
 */
#define INST_PRE_LOCK (1 << 4)
/* REPNZ prefix for string instructions only - means an instruction can follow it. */
#define INST_PRE_REPNZ (1 << 5)
/* REP prefix for string instructions only - means an instruction can follow it. */
#define INST_PRE_REP (1 << 6)
/* CS override prefix. */
#define INST_PRE_CS (1 << 7)
/* SS override prefix. */
#define INST_PRE_SS (1 << 8)
/* DS override prefix. */
#define INST_PRE_DS (1 << 9)
/* ES override prefix. */
#define INST_PRE_ES (1 << 10)
/* FS override prefix. Funky Segment :) */
#define INST_PRE_FS (1 << 11)
/* GS override prefix. Groovy Segment, of course not, duh ! */
#define INST_PRE_GS (1 << 12)
/* Switch operand size from 32 to 16 and vice versa. */
#define INST_PRE_OP_SIZE (1 << 13)
/* Switch address size from 32 to 16 and vice versa. */
#define INST_PRE_ADDR_SIZE (1 << 14)
/* Native instructions which needs suffix letter to indicate their operation-size (and don't depend on operands). */
#define INST_NATIVE (1 << 15)
/* Use extended mnemonic, means it's an _InstInfoEx structure, which contains another mnemonic for 32 bits specifically. */
#define INST_USE_EXMNEMONIC (1 << 16)
/* Use third operand, means it's an _InstInfoEx structure, which contains another operand for special instructions. */
#define INST_USE_OP3 (1 << 17)
/* Use fourth operand, means it's an _InstInfoEx structure, which contains another operand for special instructions. */
#define INST_USE_OP4 (1 << 18)
/* The instruction's mnemonic depends on the mod value of the ModR/M byte (mod=11, mod!=11). */
#define INST_MODRM_BASED (1 << 19)
/* The instruction uses a ModR/M byte which the MOD must be 11 (for registers operands only). */
#define INST_MODRR (1 << 20)
/* The way of 3DNow! instructions are built, we have to handle their locating specially. Suffix imm8 tells which instruction it is. */
#define INST_3DNOW_FETCH (1 << 21)
/* The instruction needs two suffixes, one for the comparison type (imm8) and the second for its operation size indication (second mnemonic). */
#define INST_PSEUDO_OPCODE (1 << 22)
/* Invalid instruction at 64 bits decoding mode. */
#define INST_INVALID_64BITS (1 << 23)
/* Specific instruction is can be promoted to 64 bits (without REX it is promoted automatically). */
#define INST_64BITS (1 << 24)
/* Indicates the instruction must be REX prefixed in order to use 64 bits operands. */
#define INST_PRE_REX (1 << 25)
/* Third mnemonic is set. */
#define INST_USE_EXMNEMONIC2 (1 << 26)
/* Instruction is only valid in 64 bits decoding mode. */
#define INST_64BITS_FETCH (1 << 27)

#define INST_PRE_REPS (INST_PRE_REPNZ | INST_PRE_REP)
#define INST_PRE_LOKREP_MASK (INST_PRE_LOCK | INST_PRE_REPNZ | INST_PRE_REP)
#define INST_PRE_SEGOVRD_MASK (INST_PRE_CS | INST_PRE_SS | INST_PRE_DS | INST_PRE_ES | INST_PRE_FS | INST_PRE_GS)

/* Instructions Set classes: */
/* Indicates the instruction belongs to the General Integer set. */
#define ISCT_INTEGER 1
/* Indicates the instruction belongs to the 387 FPU set. */
#define ISCT_FPU 2
/* Indicates the instruction belongs to the P6 set. */
#define ISCT_P6 3
/* Indicates the instruction belongs to the MMX set. */
#define ISCT_MMX 4
/* Indicates the instruction belongs to the SSE set. */
#define ISCT_SSE 5
/* Indicates the instruction belongs to the SSE2 set. */
#define ISCT_SSE2 6
/* Indicates the instruction belongs to the SSE3 set. */
#define ISCT_SSE3 7
/* Indicates the instruction belongs to the SSSE3 set. */
#define ISCT_SSSE3 8
/* Indicates the instruction belongs to the SSE4.1 set. */
#define ISCT_SSE4_1 9
/* Indicates the instruction belongs to the SSE4.2 set. */
#define ISCT_SSE4_2 10
/* Indicates the instruction belongs to the AMD's SSE4.A set. */
#define ISCT_SSE4_A 11
/* Indicates the instruction belongs to the 3DNow! set. */
#define ISCT_3DNOW 12
/* Indicates the instruction belongs to the 3DNow! Extensions set. */
#define ISCT_3DNOWEXT 13
/* Indicates the instruction belongs to the VMX (Intel) set. */
#define ISCT_VMX 14
/* Indicates the instruction belongs to the SVM (AMD) set. */
#define ISCT_SVM 15

/*
 * Indicates which operand is being decoded.
 * Destination (1st), Source (2nd), op3 (3rd), op4 (4th).
 * Its main purpose to help the decode-operands function know whether its the first operand (+ it's indirection + there's a lock prefix).
 */
typedef enum {ONT_NONE = -1, ONT_1, ONT_2, ONT_3, ONT_4} _OperandNumberType;

#define MAX_MNEMONIC_LENGTH (32)

#ifdef _MSC_VER
 #pragma pack(push, 1)
#endif

/*
 * Info about the instruction, source/dest types, its name in text and flags.
 * This structure is used for the instructions DB and NOT for the disassembled result code!
 * This is the BASE structure, there are extentions to this structure below.
 */

typedef struct _PACKED_ {
	uint8_t type;
	uint8_t isc;
	uint8_t s, d; /* OpType */
	int8_t* mnemonic;
	_iflags flags;
} _InstInfo;

/*
 * There are merely few instructions which need a second mnemonic for 32 bits.
 * Or a third for 64 bits. Therefore sometimes the second mnemonic is empty but not the third.
 * In all decoding modes the first mnemonic is the default.
 * A flag will indicate it uses another mnemonic.
 *
 * There are a couple of (SSE4) instructions in the whole DB which need both op3 and 3rd mnemonic for 64bits,
 * therefore, I decided to make the extended structure contain all extra info in the same structure.
 * There are a few instructions (SHLD/SHRD/IMUL and SSE too) which use third operand (or a fourth).
 * A flag will indicate it uses a third/fourth operand.
 *
 */
typedef struct _PACKED_ {
	uint8_t type;
	uint8_t isc;
	uint8_t s, d; /* OpType */
	int8_t* mnemonic;
	_iflags flags;
	uint8_t op3, op4; /* OpType */
	int8_t* mnemonic2;
	int8_t* mnemonic3;
} _InstInfoEx;

/* Trie data structure node type: */
typedef enum {
	INT_NOTEXISTS = -1, /* Not exists (this is used for a return code only). */
	INT_NONE, /* No instruction info or list set. */
	INT_INFO, /* It's an instruction info. */
	INT_LIST_GROUP,
	INT_LIST_FULL,
	INT_LIST_DIVIDED
} _InstNodeType;

/*
 * A node in the instructions DB;
 * Can be both a node or an info, depends on type.
 */
typedef struct _PACKED_ InstNode{
	uint8_t type;
	uint8_t* ids;
	_InstInfo** list; /* The second level might point to _InstNode, this is determined by type in runtime. */
} _InstNode;

#ifdef _MSC_VER
 #pragma pack(pop)
#endif

typedef enum {OPERAND_SIZE_NONE = 0, OPERAND_SIZE8, OPERAND_SIZE16, OPERAND_SIZE32, OPERAND_SIZE64, OPERAND_SIZE80, OPERAND_SIZE128} _OperandSizeType;

/*
 * Used for letting the extract operand know the type of operands without knowing the
 * instruction itself yet, because of the way those instructions work.
 */
extern _InstInfo II_3dnow;

_InstInfo* locate_inst(const uint8_t** code, int* codeLen, _OffsetType* codeOffset, _WString* instructionHex, _PrefixState* ps, _DecodeType dt);
_InstInfo* locate_3dnow_inst(_CodeInfo* ci, _WString* instructionHex);

/* Concatenates a text describing the size used for indirections form. (MOV *WORD* [BX], 0x12) when it's not cleared from operands. */
void str_indirection_text(_WString* s, _OperandSizeType opSize);

#endif /* INSTRUCTIONS_H */
