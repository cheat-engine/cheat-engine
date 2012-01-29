/*
decoder.c

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#include "decoder.h"

#include "prefix.h"
#include "textdefs.h"
#include "x86defs.h"
#include "operands.h"

/* Instruction Prefixes - Opcode - ModR/M - SIB - Displacement - Immediate */

_DecodeType ADDR_SIZE_AFFECT(_DecodeType dt, _iflags totalPrefixes)
{
	/* Switch to non default mode if prefix exists, only for ADDRESS SIZE. */
	if (totalPrefixes & INST_PRE_ADDR_SIZE) {
		switch (dt)
		{
			case Decode16Bits:
				dt = Decode32Bits;
			break;
			case Decode32Bits:
				dt = Decode16Bits;
			break;
			case Decode64Bits:
				dt = Decode32Bits;
			break;
		}
	}

	return dt;
}

_DecodeType OP_SIZE_AFFECT(_DecodeType dt, _iflags totalPrefixes, unsigned int rex, _iflags instFlags)
{
	switch (dt)
	{
		case Decode16Bits: /* Toggle to 32 bits. */
			if (totalPrefixes & INST_PRE_OP_SIZE) dt = Decode32Bits;
		break;
		case Decode32Bits: /* Toggle to 16 bits. */
			if (totalPrefixes & INST_PRE_OP_SIZE) dt = Decode16Bits;
		break;
		case Decode64Bits:
			/*
			 * REX Prefix toggles data size to 64 bits.
			 * Operand size prefix toggles data size to 16.
			 * Default data size is 32 bits.
			 * Promoted instructions are 64 bits if they don't require a REX perfix.
			 * Non promoted instructions are 64 bits if the REX prefix exists.
			 */
			if (totalPrefixes & INST_PRE_OP_SIZE) dt = Decode16Bits;
			/* Automatically promoted instructions have only INST_64BITS SET! */
			else if (((instFlags & (INST_64BITS | INST_PRE_REX)) == INST_64BITS) ||
			/* Other instructions in 64 bits can be promoted only with a REX prefix. */
			         ((totalPrefixes & INST_PRE_REX) && (rex & PREFIX_REX_W))) dt = Decode64Bits;
			else dt = Decode32Bits; /* Default. */
		break;
	}
	return dt;
}

static int decode_inst(const uint8_t* code, int codeLen, _OffsetType codeOffset,
                       _PrefixState* ps, _DecodeType dt, _DecodedInst* di)
{
	/* The ModR/M byte of the current instruction. */
	unsigned int modrm = 0;

	/* The REX prefix byte value. */
	unsigned int rex = 0;

	/*
	 * Backup original input, so we can use it later if a problem occurs
	 * (like not enough data for decoding, invalid opcode, etc).
	 */
	_OffsetType lastCodeOffset = codeOffset;
	const uint8_t* lastCode = code;

	/* Holds the info about the current found instruction. */
	_InstInfo* ii = NULL;

	/* Used only for special CMP instructions which have pseudo opcodes suffix. */
	unsigned int cmpType = 0;

	/* Return code from extract_operand. */
	_ExOpRCType rc = EO_HALT;

	/*
	 * Indicates whether it is right to LOCK the instruction by decoding its first operand.
	 * Only then you know if it's ok to output the LOCK prefix's text...
	 * Used for first operand only.
	 */
	int lockable = 0;

	/*
	 * Dummy lockable...ExtractOperand writes the pointer no matter what.
	 * We pass it so we won't have an exception, and because real lock can only work on first operand.
	 */
	int dummyLockable;

	/* Return code, TRUE for decoding success, FALSE for failure. */
	int retCode = TRUE;

	/* Packed params to pass to extract_operand, used for optimizing. */
	_CodeInfo ci;

	memset(di, 0, sizeof(_DecodedInst));

	ii = locate_inst(&code, &codeLen, &codeOffset, &di->instructionHex, ps, dt);

	/*
	 * In this point we know the instruction we are about to decode and its operands (unless, it's an invalid one!),
	 * so it makes it the right time for decoding-type suitability testing.
	 * Which practically means, don't allow 32 bits instructions in 16 bits decoding mode, but do allow
	 * 16 bits instructions in 32 bits decoding mode, of course...

	 * NOTE: Make sure the instruction set for 32 bits has explicitly this specfic flag set.
	 * NOTE2: Make sure the instruction set for 64 bits has explicitly this specfic flag set.

	 * If this is the case, drop what we've got and restart all over after DB'ing that byte.

	 * Though, don't drop an instruction which is also supported in 16 and 32 bits.
	*/

	/* ! ! ! (V1.4.10) DISABLED UNTIL FURTHER NOTICE ! ! ! Decode16Bits CAN NOW DECODE 32 BITS INSTRUCTIONS ! ! !*/
	/* if (ii && (dt == Decode16Bits) && (ii->flags & INST_32BITS) && (~ii->flags & INST_16BITS)) ii = NULL; */

	/* Drop instructions which are invalid in 64 bits. */
	if (ii && (dt == Decode64Bits) && (ii->flags & INST_INVALID_64BITS)) ii = NULL;

	/* If it's only a 64 bits instruction drop it in other decoding modes. */
	if (ii && (dt != Decode64Bits) && (ii->flags & INST_64BITS_FETCH)) ii = NULL;

	/* code points to the last instruction-byte read, so if the instruction needs the ModR/M byte, we'll just read it. */
	if (ii && (ii->d != OT_NONE) && (ii->d != OT_DUMMY) && (ii->flags & INST_INCLUDE_MODRM)) {
		modrm = *code;
		str_hex_b(&di->instructionHex, modrm);

		if (--codeLen < 0) ii = NULL;
		code++;
		codeOffset++;

		/* If the instruction uses only register operands, mod=11 is a must! */
		if (ii && (ii->flags & INST_MODRR) && (modrm < INST_DIVIDED_MODRM)) ii = NULL;
	}

	/* Store REX value for further tests. */
	if (ps->isREXPrefixValid) rex = *ps->rexpos;

	/*
	 * V 1.7.28 - Treat NOP/XCHG specially.
	 * If we're not in 64bits restore XCHG to NOP, since in the DB it's XCHG.
	 * Else if we're in 64bits examine REX, if exists, and decide which instruction should should go to output.
	 * 48 90 XCHG RAX, RAX is a true NOP (eat REX in this case because it's valid).
	 * 90 XCHG EAX, EAX is a true NOP (and not high dword of RAX = 0 because it should be a 32 bits operation).
	 */
	if (ii == &II_90) {
		if ((dt != Decode64Bits) || (~rex & PREFIX_REX_B)) ii = &II_nop;
		if (rex & PREFIX_REX_W) ps->usedPrefixes |= INST_PRE_REX;
	}

	/* V 1.7.29 - Ignore segment override prefixes for LEA instruction. */
	if (ii == &II_8D) ps->totalPrefixes &= ~(INST_PRE_SEGOVRD_MASK);

	ci.code = code;
	ci.codeLen = codeLen;
	ci.codeOffset = codeOffset;

	if (ii && (ii->d != OT_NONE) && (ii->d != OT_DUMMY)) {
		rc = extract_operand(&ci, &di->instructionHex, &di->operands, (_OpType)ii->d, (_OpType)ii->s, ONT_1, ii->flags, modrm, ps, dt, &lockable);
		if (rc == EO_HALT) ii = NULL;
	}

	if (ii && (ii->s != OT_NONE) && (ii->s != OT_DUMMY)) {
		strcat_WSN(&di->operands, SEP_STR);
		rc = extract_operand(&ci, &di->instructionHex, &di->operands, (_OpType)ii->s, (_OpType)ii->d, ONT_2, ii->flags, modrm, ps, dt, &dummyLockable);
		if (rc == EO_HALT) ii = NULL;
	}

	/* Use third operand, only if the flags says this InstInfo requires it. */
	if (ii && (ii->flags & INST_USE_OP3) && (((_InstInfoEx*)ii)->op3 != OT_NONE) && (((_InstInfoEx*)ii)->op3 != OT_DUMMY)) {
		strcat_WSN(&di->operands, SEP_STR);
		rc = extract_operand(&ci, &di->instructionHex, &di->operands, (_OpType)((_InstInfoEx*)ii)->op3, OT_NONE, ONT_3, ii->flags, modrm, ps, dt, &dummyLockable);
		if (rc == EO_HALT) ii = NULL;
	}

	/* V 1.7.26 Support for a fourth operand is added for INSERTQ instruction. */
	if (ii && (ii->flags & INST_USE_OP4)) {
		strcat_WSN(&di->operands, SEP_STR);
		rc = extract_operand(&ci, &di->instructionHex, &di->operands, (_OpType)((_InstInfoEx*)ii)->op4, OT_NONE, ONT_4, ii->flags, modrm, ps, dt, &dummyLockable);
		if (rc == EO_HALT) ii = NULL;
	}

	/*
	 * Remove extra space in operands text buffer if required.
	 * V1.5.12 - The pos test was added to avoid memory access exceeding bounds.
	 */
	if ((di->operands.pos >= 2) && (di->operands.p[di->operands.pos-2] == SEP_CHR)) {
		di->operands.p[di->operands.pos-2] = '\0';
		di->operands.pos -= 2;
	}

	/* If it were a 3DNow! instruction, we will have to find the instruction itself now that we got its operands extracted. */
	if (ii && ii->flags & INST_3DNOW_FETCH) ii = locate_3dnow_inst(&ci, &di->instructionHex);

	code = ci.code;
	codeLen = ci.codeLen;
	codeOffset = ci.codeOffset;

	/*
	 * There's a limit of 15 bytes on instruction length. The only way to violate
	 * this limit is by putting redundant prefixes before an instruction.
	 * start points to first prefix if any, otherwise it points to instruction first byte.
	 */
	if ((code - ps->start) > INST_MAXIMUM_SIZE) ii = NULL; /* Drop instruction. */

	/* Check whether pseudo opcode is needed, only for CMP instructions: */
	if (ii && (ii->flags & INST_PSEUDO_OPCODE)) {
		cmpType = *code;
		/* Comparison type must be between 0 to 8, otherwise Reserved. */
		if (cmpType >= INST_CMP_MAX_RANGE) ii = NULL;
		else str_hex_b(&di->instructionHex, cmpType);

		if (--codeLen < 0) ii = NULL;
		code++;
		codeOffset++;
	}

	/*
	* If ii is valid, we located the instruction in the DB and extracted operands.
	* Otherwise, some problem occurred while decoding it.
	*/

	if (ii) {
		/*
		 * Use the correct mnemonic according to the DT.
		 * If we are in 32 bits decoding mode it doesn't necessarily mean we will choose mnemonic2, alas,
		 * it means that if there is a mnemonic2, it will be used.
		 */
		strclear_WS(&di->mnemonic);

		/* It's time for adding prefixes' texts if needed. */

		/* Start with prefix LOCK. */
		if ((lockable == 1) && (ii->flags & INST_PRE_LOCK)) {
			ps->usedPrefixes |= INST_PRE_LOCK;

			strcpy_WSN(&di->mnemonic, PREFIX_LOCK_TEXT);
		} else if ((ii->flags & INST_PRE_REPNZ) && (ps->totalPrefixes & INST_PRE_REPNZ)) {
			ps->usedPrefixes |= INST_PRE_REPNZ;

			strcpy_WS(&di->mnemonic, PREFIX_REPNZ_TEXT);
		} else if ((ii->flags & INST_PRE_REP) && (ps->totalPrefixes & INST_PRE_REP)) {
			ps->usedPrefixes |= INST_PRE_REP;

			strcpy_WSN(&di->mnemonic, PREFIX_REP_TEXT);
		}

		/* If it's JeCXZ the ADDR_SIZE prefix affects them. */
		if ((ii->flags & (INST_PRE_ADDR_SIZE | INST_USE_EXMNEMONIC)) == (INST_PRE_ADDR_SIZE | INST_USE_EXMNEMONIC)) {
			ps->usedPrefixes |= (ps->totalPrefixes & INST_PRE_ADDR_SIZE);
			if (ADDR_SIZE_AFFECT(dt, ps->totalPrefixes) == Decode16Bits) strcatlen_WS(&di->mnemonic, &ii->mnemonic[1], ii->mnemonic[0]);
			else if (ADDR_SIZE_AFFECT(dt, ps->totalPrefixes) == Decode32Bits) strcatlen_WS(&di->mnemonic, &((_InstInfoEx*)ii)->mnemonic2[1], ((_InstInfoEx*)ii)->mnemonic2[0]);
			/* Ignore REX.W in 64bits, JECXZ is promoted. */
			else /* Decode64Bits */ strcatlen_WS(&di->mnemonic, &((_InstInfoEx*)ii)->mnemonic3[1], ((_InstInfoEx*)ii)->mnemonic3[0]);
		}

		/* V1.1.8 LOOPxx instructions are also native instruction, but they are special case ones, ADDR_SIZE prefix affects them. */
		else if ((ii->flags & (INST_PRE_ADDR_SIZE | INST_NATIVE)) == (INST_PRE_ADDR_SIZE | INST_NATIVE)) {
			strcpylen_WS(&di->mnemonic, &ii->mnemonic[1], ii->mnemonic[0]);

			/* We only enter the next if statement if the addr-size prefix is set, this behaviour is Native's. */
			/* Ignore REX.W in 64bits, LOOPxx is promoted. */
			if (ps->totalPrefixes & INST_PRE_ADDR_SIZE) {
				ps->usedPrefixes |= (ps->totalPrefixes & INST_PRE_ADDR_SIZE);
				if (ADDR_SIZE_AFFECT(dt, ps->totalPrefixes) == Decode16Bits) chrcat_WS(&di->mnemonic, SUFFIX_SIZE_WORD);
				else if (ADDR_SIZE_AFFECT(dt, ps->totalPrefixes) == Decode32Bits) chrcat_WS(&di->mnemonic, SUFFIX_SIZE_DWORD);
				else /* Decode64Bits */ chrcat_WS(&di->mnemonic, SUFFIX_SIZE_QWORD);
			}
		}
		/*
		 * Note:
		 * If the instruction is prefixed by operand size we will format it in the non-default decoding mode!
		 * So there might be a situation that an instruction of 32 bit gets formatted in 16 bits decoding mode.
		 * Both ways should end up with a corrected and expected formatting of the text.
		*/
		else if (OP_SIZE_AFFECT(dt, ps->totalPrefixes, rex, ii->flags) == Decode16Bits) { /* Decode16Bits */

			/*
			 * If it's a special instruction which has two mnemonics, then use the 16 bits one + update usedPrefixes.
			 * Note: use 16 bits mnemonic if that instruction supports 32 bit or 64 bit explicitly.
			 */
			if ((ii->flags & INST_USE_EXMNEMONIC) && ((ii->flags & (INST_32BITS | INST_64BITS)) == 0)) ps->usedPrefixes |= (ps->totalPrefixes & INST_PRE_OP_SIZE);
			strcatlen_WS(&di->mnemonic, &ii->mnemonic[1], ii->mnemonic[0]);

			/* Add a suffix letter for repeatable/xlat instructions only. */
			if (rc == EO_SUFFIX) {
				/* Is it a 16 bits operation? */
				if (ii->flags & INST_16BITS) chrcat_WS(&di->mnemonic, SUFFIX_SIZE_WORD);
				else chrcat_WS(&di->mnemonic, SUFFIX_SIZE_BYTE); /* It's 8 bits operation then. */
			}

			/* Add a suffix, if it's a native instruction (making it 16 bits). */
			if ((ii->flags & INST_NATIVE) && (ps->totalPrefixes & INST_PRE_OP_SIZE)) {
				ps->usedPrefixes |= INST_PRE_OP_SIZE;

				chrcat_WS(&di->mnemonic, SUFFIX_SIZE_WORD);
			}
		} else if (OP_SIZE_AFFECT(dt, ps->totalPrefixes, rex, ii->flags) == Decode32Bits) { /* Decode32Bits */

			/* Give a chance for special mnemonic instruction in 32 bits decoding. */
			if (ii->flags & INST_USE_EXMNEMONIC) {
				ps->usedPrefixes |= (ps->totalPrefixes & INST_PRE_OP_SIZE);
				/* Is it a special instruction which has another mnemonic for mod=11 ? */
				if (ii->flags & INST_MODRM_BASED) {
					if (modrm >= INST_DIVIDED_MODRM) strcatlen_WS(&di->mnemonic, &ii->mnemonic[1], ii->mnemonic[0]);
					else strcatlen_WS(&di->mnemonic, &((_InstInfoEx*)ii)->mnemonic2[1], ((_InstInfoEx*)ii)->mnemonic2[0]);
				}
				/* Or is it a special CMP instruction which needs a pseudo opcode suffix ? */
				else if (ii->flags & INST_PSEUDO_OPCODE) {
					/* So we have to read the imm8 which tells us which comparison type it is. */
					strcpylen_WS(&di->mnemonic, &ii->mnemonic[1], ii->mnemonic[0]);
					str_x86def(&di->mnemonic, &_CONDITIONS_PSEUDO[cmpType]);
					strcatlen_WS(&di->mnemonic, &((_InstInfoEx*)ii)->mnemonic2[1], ((_InstInfoEx*)ii)->mnemonic2[0]);

				} else strcatlen_WS(&di->mnemonic, &((_InstInfoEx*)ii)->mnemonic2[1], ((_InstInfoEx*)ii)->mnemonic2[0]); /* Two-mnemonics instructions. */
			} else strcatlen_WS(&di->mnemonic, &ii->mnemonic[1], ii->mnemonic[0]);

			/* Add a suffix letter for repeatable(/xlat) instructions only. */
			if (rc == EO_SUFFIX) {
				/* Is it a 16 bits operation (means 32 bits in 32 bit decoding mode)? */
				if (ii->flags & INST_16BITS) chrcat_WS(&di->mnemonic, SUFFIX_SIZE_DWORD);
				else chrcat_WS(&di->mnemonic, SUFFIX_SIZE_BYTE); /* It's 8 bits operation then. */
			}
			
			/* Add a suffix, if it's a native instruction (making it 32 bits). */
			if ((ii->flags & INST_NATIVE) && (ps->totalPrefixes & INST_PRE_OP_SIZE)) {
				ps->usedPrefixes |= INST_PRE_OP_SIZE;

				chrcat_WS(&di->mnemonic, SUFFIX_SIZE_DWORD);
			}
		} else { /* Decode64Bits */

			/* /////////////////////////////////////////////////////////////////////////////////////////// */
			if (ii->flags & (INST_USE_EXMNEMONIC | INST_USE_EXMNEMONIC2)) {
				/* Is it a special instruction which has another mnemonic for mod=11 ? */
				if (ii->flags & INST_MODRM_BASED) {
					if (modrm >= INST_DIVIDED_MODRM) strcatlen_WS(&di->mnemonic, &ii->mnemonic[1], ii->mnemonic[0]);
					else strcatlen_WS(&di->mnemonic, &((_InstInfoEx*)ii)->mnemonic2[1], ((_InstInfoEx*)ii)->mnemonic2[0]);
				}
				/* Or is it a special CMP instruction which needs a pseudo opcode suffix ? */
				else if (ii->flags & INST_PSEUDO_OPCODE) {
					/* So we have to read the imm8 which tells us which comparison type it is. */
					strcpylen_WS(&di->mnemonic, &ii->mnemonic[1], ii->mnemonic[0]);
					str_x86def(&di->mnemonic, &_CONDITIONS_PSEUDO[cmpType]);
					strcatlen_WS(&di->mnemonic, &((_InstInfoEx*)ii)->mnemonic2[1], ((_InstInfoEx*)ii)->mnemonic2[0]);
				} else /* Use third mnemonic, for 64 bits. */
					if ((ii->flags & INST_USE_EXMNEMONIC2) && (ps->isREXPrefixValid) && (rex & PREFIX_REX_W)) {
						ps->usedPrefixes |= INST_PRE_REX;
						strcatlen_WS(&di->mnemonic, &((_InstInfoEx*)ii)->mnemonic3[1], ((_InstInfoEx*)ii)->mnemonic3[0]);
					} else strcatlen_WS(&di->mnemonic, &((_InstInfoEx*)ii)->mnemonic2[1], ((_InstInfoEx*)ii)->mnemonic2[0]); /* Use second mnemonic. */
			} else strcatlen_WS(&di->mnemonic, &ii->mnemonic[1], ii->mnemonic[0]);
			/* /////////////////////////////////////////////////////////////////////////////////////////// */

			/* Add a suffix letter for repeatable(/xlat) instructions only. */
			if (rc == EO_SUFFIX) {
				/* Is it a 16 bits operation (means 64 bits in 64 bit decoding mode)? */
				if (ii->flags & INST_16BITS) {
					if (ii->flags & INST_64BITS) {
						ps->usedPrefixes |= INST_PRE_REX;
						chrcat_WS(&di->mnemonic, SUFFIX_SIZE_QWORD);
					}
					else chrcat_WS(&di->mnemonic, SUFFIX_SIZE_DWORD);
				}
				else chrcat_WS(&di->mnemonic, SUFFIX_SIZE_BYTE); /* It's 8 bits operation then. */
			}

			/* Add a suffix, if it's a native instruction (making it 64 bits). */
			if (ii->flags & INST_NATIVE) {
				if (ps->totalPrefixes & INST_PRE_OP_SIZE) {	/* Operand size. */
					ps->usedPrefixes |= INST_PRE_OP_SIZE;
					chrcat_WS(&di->mnemonic, SUFFIX_SIZE_WORD); /* 16 Bits. */
				}
				/* See if a REX prefix is required, otherwise ignore it. */
				else if ((ps->totalPrefixes & INST_PRE_REX) && (rex & PREFIX_REX_W) && ((ii->flags & (INST_64BITS | INST_PRE_REX)) == (INST_64BITS | INST_PRE_REX))) {
					 /* REX.W is set, mark it. */
					ps->usedPrefixes |= INST_PRE_REX;
					chrcat_WS(&di->mnemonic, SUFFIX_SIZE_QWORD); /* 64 Bits. */
				}
			}
		}
	}
	else {
		strclear_WS(&di->operands);
		/* Special case for WAIT instruction: If it's dropped, you have to return a valid instruction! */
		if (*lastCode == WAIT_INSTRUCTION_CODE) {
			codeOffset = lastCodeOffset + 1;
			strcpy_WS(&di->instructionHex, get_hex_b(WAIT_INSTRUCTION_CODE));
			strcpy_WSN(&di->mnemonic, WAIT_INSTRUCTION_MNEMONIC);
			ps->usedPrefixes = 0;
		} else {
			/*
			 * Exclude the first byte, couldn't decode that instruction.
			 * So just DB(define byte) it and skip it.
			 * Fix codeOffset, because we've lost track of them (code, codeOffset, codeLen)!
			 * The caller calculates them again every instruction according to the size we return which is based on codeOffset.
			 * A fix for codeOffset is necessary because below the decoded-instruction size depends on it.
			 */
			codeOffset = lastCodeOffset + 1; /* + 1 for what we are DB'ing. */
			strcpy_WS(&di->instructionHex, get_hex_b(*lastCode));

			strcpy_WSN(&di->mnemonic, BYTE_UNDEFINED);
			str_code_sp_hb(&di->mnemonic, *lastCode);

			/* Clean operands just in case... */
			ps->usedPrefixes = 0; /* Drop'em all. */

			/* Mark that we didn't manage to decode the instruction well. */
			retCode = FALSE;
		}
	}

	/* Calculate the size of the instruction we've just decoded. */
	di->size = (unsigned int)(codeOffset - lastCodeOffset);
	return retCode;
}

_DecodeResult internal_decode(_OffsetType codeOffset, const uint8_t* code, int codeLen, _DecodeType dt, _DecodedInst result[], unsigned int maxResultCount, unsigned int* usedInstructionsCount)
{
	_PrefixState ps;
	unsigned int prefixSize;

	/*
	 * This is used for printing only, it is the real offset of where the whole instruction begins.
	 * We need this variable in addition to codeOffset, because prefixes might change the real offset an instruction begins at.
	 * So we keep track of both.
	 */
	_OffsetType startCodeOffset = 0;

	_WString hex;
	int isInstructionPopped = FALSE;
	uint8_t unusedPrefixesList[MAX_PREFIXES];
	const uint8_t* p;
	unsigned int i;

	/* Current working decoded instruction. */
	unsigned int nextPos = 0; /* in result. */
	_DecodedInst *pdi = NULL, di;

	/* No entries are used yet. */
	*usedInstructionsCount = 0;

	/* Decode instructions as long as we have what to decode/enough room in entries. */
	while (codeLen > 0) {

		/* startCodeOffset holds the displayed offset of current instruction. */
		startCodeOffset = codeOffset;

		/* Decode prefixes: */
		strclear_WS(&hex);

		memset(&ps, 0, sizeof(_PrefixState));
		ps.start = code;
		ps.last = code;
		prefixSize = 0;

		if (is_prefix(*code, dt)) { /* Most chances there are no prefixes at all, so give it a try. */
			decode_prefixes(code, codeLen, &ps, dt);
			/* Count prefixes, start points to first valid prefix. */
			prefixSize = (int)(ps.last - ps.start);
			/* Drop extra prefixes if any. */
			if (ps.start != code) {

				/* Make sure there is enough room. */
				if (nextPos + (ps.start - code) > maxResultCount) return DECRES_MEMORYERR; /* Not enough then. */

				for (p = code; p < ps.start; p++, codeOffset++, codeLen--, code++) {
					/* Use next entry. */
					pdi = &result[nextPos++];

					strcpy_WSN(&pdi->mnemonic, BYTE_UNDEFINED);
					str_code_sp_hb(&pdi->mnemonic, *p);
					strclear_WS(&pdi->operands);
					strcpy_WS(&pdi->instructionHex, get_hex_b(*p));
					pdi->size = 1;
					pdi->offset = codeOffset;
				}
				*usedInstructionsCount = nextPos; /* So far so good. */

				/* These prefixes were really dropped, so update the displayable offset as well. */
				startCodeOffset = codeOffset;
			}
			/* Update variables to take effect of the prefixes existence. */
			codeLen -= prefixSize;
			/*
			 * It might be that we will just notice that we ran out of bytes,
			 * so we will have to drop all prefixes and halt.
			 */
			if (codeLen <= 0) {

				/* Make sure there is enough room. */
				if (nextPos + (ps.last - code) > maxResultCount) return DECRES_MEMORYERR;

				for (p = code; p < ps.last; p++, startCodeOffset++) {
					/* Use next entry. */
					pdi = &result[nextPos++];

					strcpy_WSN(&pdi->mnemonic, BYTE_UNDEFINED);
					str_code_sp_hb(&pdi->mnemonic, *p);
					strclear_WS(&pdi->operands);
					strcpy_WS(&pdi->instructionHex, get_hex_b(*p));
					pdi->size = 1;
					pdi->offset = startCodeOffset;
				}
				*usedInstructionsCount = nextPos; /* Include them all. */
				break; /* Bye bye, out of bytes. */
			}
			code += prefixSize;
			codeOffset += prefixSize;
			/*usedInstructionsCount = nextPos; */ /* Take them into account. */
		}

		/*
		 * Now we decode the instruction and only then we do further prefixes handling.
		 * This is because the instruction could not be decoded at all, or an instruction requires
		 * a mandatory prefix, or some of the prefixes were useless, etc...

		 * Even if there were a mandatory prefix, we already took into account its size as a normal prefix.
		 * so prefixSize includes that, and the returned size in di is simply the size of the real(=without prefixes) instruction.

		 * If the instruction were decoded successfully, we will have to build a prefix hex string and drop unused prefixes.
		 * Otherwise drop all instructions.

		 * In 64 bits, we have to validate that the REX prefix precedes immediately the first opcode byte.
		 * If so, remove it, and take into account we disabled it, in the unused prefixes!
		 */
		if (dt == Decode64Bits) {
			if (ps.totalPrefixes & INST_PRE_REX) {
				if (ps.rexpos != (code-1)) ps.totalPrefixes &= ~INST_PRE_REX;
				else {
					ps.isREXPrefixValid = 1;
					/*
					* Now check out if Operand Size is still valid, because REX has precedence over Operand Size prefix.
					* So if this is the case, we will simply disable Operand Size prefix.
					* Note that it happens only if REX.W is set !
					*/
					if (*ps.rexpos & PREFIX_REX_W) ps.totalPrefixes &= ~INST_PRE_OP_SIZE; /* Could be mandatory prefix later. */
				}
			}
			/* In 64 bits, segment overrides of CS, DS, ES and SS are ignored. So don't take'em into account. */
			ps.totalPrefixes &= ~(INST_PRE_CS | INST_PRE_SS | INST_PRE_DS | INST_PRE_ES);
		}

		/*
		 * Make sure there is at least one more entry to fill, for the upcoming instruction.
		 * Though in practice it could be used for an ignored prefix...
		 */
		if (nextPos + 1 > maxResultCount) return DECRES_MEMORYERR;

		if (prefixSize > 0) {

			/*
			 * Assume: if instruction is prefixed, it won't ignore the prefixes.
			 * This is because we rely on the fact that the disassembler will decode 99% of the times real code.
			 * Thus, it will be faster to treat the prefixes as correct, in case the assumption is wrong,
			 * we will have to fold back, and pay twice for this assumption, because we will have to backup pdi and restore it later.
			 * For example: we assume c is 0 in most of the times (if it's 1 we subtract b): a += b; if (c) a -= 2 * b;
			 */

			pdi = &result[nextPos++];
			isInstructionPopped = FALSE;
			if (decode_inst(code, codeLen, codeOffset, &ps, dt, pdi)) {

				/*
				 * Build a prefix string including all VALID prefixes. Maximum 5 prefixes.
				 * Note that we don't care whether or not we turned the last prefix into a mandatory one,
				 * ps.last is changed if required to affect this.
				 */
				for (p = ps.start; p < ps.last; p++) {
					str_hex_b(&hex, *p);
					chrcat_WS(&hex, SP_CHR);
				}

				/* List the prefixes which weren't used at all, although they are treated as valid. */
				get_unused_prefixes_list(unusedPrefixesList, &ps);

				if (ps.unusedCount > 0) {
					if (nextPos + ps.unusedCount > maxResultCount) return DECRES_MEMORYERR;

					/* Pop the instruction from the array. */
					memcpy(&di, pdi, sizeof(_DecodedInst));
					isInstructionPopped = TRUE;
					nextPos--;

					/* Insert the dropped prefixes in their place. */
					for (i = 0; i < ps.unusedCount; i++) {
						/* Use next entry. */
						pdi = &result[nextPos++];

						strcpy_WSN(&pdi->mnemonic, BYTE_UNDEFINED);
						str_code_sp_hb(&pdi->mnemonic, unusedPrefixesList[i]);
						strclear_WS(&pdi->operands);
						strcpy_WS(&pdi->instructionHex, get_hex_b(unusedPrefixesList[i]));
						pdi->size = 1;
						pdi->offset = startCodeOffset;
					}
				}
				/*
				 * Intentionally usedInstructionsCount is not updated here,
				 * simply because if we run out of entries, we won't be able to synchronize the decoding again from this point.
				 * This is true, because unused prefixes can't be skipped (we don't know their exact order).
				 */
			} else { /* No success in decoding. */
				/*
				 * Drop all prefixes, because the instruction wasn't successfully decoded.
				 * After we tried to decode an instruction, it could be that we have a mandatory prefix
				 * that we would like to drop too (along with REX).
				 */
				ps.last += ps.specialPrefixesSize;

				if (ps.last - ps.start > 0) {
					if (nextPos + (ps.last - ps.start) > maxResultCount) return DECRES_MEMORYERR;

					/* Pop the instruction from the array. */
					memcpy(&di, pdi, sizeof(_DecodedInst));
					isInstructionPopped = TRUE;
					nextPos--;

					for (p = ps.start; p < ps.last; p++, startCodeOffset++) {
						/* Use next entry. */
						pdi = &result[nextPos++];

						strcpy_WSN(&pdi->mnemonic, BYTE_UNDEFINED);
						str_code_sp_hb(&pdi->mnemonic, *p);
						strclear_WS(&pdi->operands);
						strcpy_WS(&pdi->instructionHex, get_hex_b(*p));
						pdi->size = 1;
						pdi->offset = startCodeOffset;
					}
					prefixSize = 0;
					*usedInstructionsCount = nextPos;
				}
			}

			if (isInstructionPopped) {
				/* Restore instruction. */
				pdi = &result[nextPos++];
				memcpy(pdi, &di, sizeof(_DecodedInst));
			} /* else pdi already points to the instruction. */

			/* Advance to next instruction. */
			codeLen -= pdi->size;
			codeOffset += pdi->size;
			code += pdi->size;

			pdi->size += prefixSize;
			pdi->offset = startCodeOffset;

			/* Concat the instruction's hex to the prefixes' hex. */
			strcatlen_WS(&hex, (const int8_t*)pdi->instructionHex.p, pdi->instructionHex.pos);
			memcpy(&pdi->instructionHex.p, &hex.p, hex.pos + 1);

		} else { /* No prefixes! Only instruction. */
			/* Use the next available entry without doubt. */
			pdi = &result[nextPos++];
			decode_inst(code, codeLen, codeOffset, &ps, dt, pdi);
			pdi->offset = startCodeOffset;

			/* Advance to next instruction. */
			codeLen -= pdi->size;
			codeOffset += pdi->size;
			code += pdi->size;
		}

		/* Alright, the caller can read, at least, up to this one. */
		*usedInstructionsCount = nextPos;
	}

	return DECRES_SUCCESS;
}
