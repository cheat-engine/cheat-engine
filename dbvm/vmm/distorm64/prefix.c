/*
prefix.c

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#include "prefix.h"

#include "textdefs.h"
#include "x86defs.h"
#include "instructions.h"

/*
 * The main purpose of this module is to keep track of all kind of prefixes a single instruction may have.
 * The problem is that a single instruction may have up to five different prefix-type.
 * That's why I have to detect such cases and drop those excess prefixes.
 */

int is_prefix(unsigned int ch, _DecodeType dt)
{
	switch (ch) {
		/* for i in xrange(0x40, 0x50): print "case 0x%2x:" % i */
		case 0x40: /* REX: */
		case 0x41:
		case 0x42:
		case 0x43:
		case 0x44:
		case 0x45:
		case 0x46:
		case 0x47:
		case 0x48:
		case 0x49:
		case 0x4a:
		case 0x4b:
		case 0x4c:
		case 0x4d:
		case 0x4e:
		case 0x4f: return (dt == Decode64Bits);
		case PREFIX_LOCK: return TRUE;
		case PREFIX_REPNZ: return TRUE;
		case PREFIX_REP: return TRUE;
		case PREFIX_CS: return TRUE;
		case PREFIX_SS: return TRUE;
		case PREFIX_DS: return TRUE;
		case PREFIX_ES: return TRUE;
		case PREFIX_FS: return TRUE;
		case PREFIX_GS: return TRUE;
		case PREFIX_OP_SIZE: return TRUE;
		case PREFIX_ADDR_SIZE: return TRUE;
	}
	return FALSE;
}

static const uint8_t* PREFIX_MIN(const uint8_t* a, const uint8_t* b, const uint8_t* c, const uint8_t* d, const uint8_t* def)
{
	/*
	 * Check for null.
	 * Return smallest (=first good prefix to take into account).
	 */

	if (!a && !b && !c && !d) return def;
	if (!a) a = (const uint8_t*)~0; /* MAX PTR ? :) */
	if (!b) b = (const uint8_t*)~0;
	if (!c) c = (const uint8_t*)~0;
	if (!d) d = (const uint8_t*)~0;

	if (b < a) a = b;
	if (c < a) a = c;
	if (d < a) a = d;
	return a == (const uint8_t*)~0 ? def : a;
}

/* Return the flag and type of given prefix. */
void get_prefix_flag(unsigned int ch, _PrefixInfo* pi, _DecodeType dt)
{
	pi->flag = INST_FLAGS_NONE;
	pi->type = PRE_NONE;
	/*
	NOTE: AMD treat lock/rep as two different groups... But I am based on Intel.

	- Lock and Repeat:
			- 0xF0 — LOCK
			- 0xF2 — REPNE/REPNZ
			- 0xF3 - REP/REPE/REPZ
	- Segment Override:
			- 0x2E - CS
			- 0x36 - SS
			- 0x3E - DS
			- 0x26 - ES
			- 0x64 - FS
			- 0x65 - GS
		- Operand-Size Override: 0x66, switching default size.
		- Address-Size Override: 0x67, switching default size.

		64 Bits:
		- REX: 0x40 - 0x4f, extends register access.
	*/

	switch (ch)
	{
		/* REX type, 64 bits decoding mode only: */
		case 0x40:
		case 0x41:
		case 0x42:
		case 0x43:
		case 0x44:
		case 0x45:
		case 0x46:
		case 0x47:
		case 0x48:
		case 0x49:
		case 0x4a:
		case 0x4b:
		case 0x4c:
		case 0x4d:
		case 0x4e:
		case 0x4f:
			if (dt == Decode64Bits) {
				pi->flag = INST_PRE_REX;
				pi->type = PRE_REX;
			}
		break;

		/* LOCK and REPx type: */
		case PREFIX_LOCK: pi->flag = INST_PRE_LOCK; pi->type = PRE_LOKREP; break;
		case PREFIX_REPNZ: pi->flag = INST_PRE_REPNZ; pi->type = PRE_LOKREP; break;
		case PREFIX_REP: pi->flag = INST_PRE_REP; pi->type = PRE_LOKREP; break;

		/* Seg Overide type: */
		case PREFIX_CS: pi->flag = INST_PRE_CS; pi->type = PRE_SEGOVRD; break;
		case PREFIX_SS: pi->flag = INST_PRE_SS; pi->type = PRE_SEGOVRD; break;
		case PREFIX_DS: pi->flag = INST_PRE_DS; pi->type = PRE_SEGOVRD; break;
		case PREFIX_ES: pi->flag = INST_PRE_ES; pi->type = PRE_SEGOVRD; break;
		case PREFIX_FS: pi->flag = INST_PRE_FS; pi->type = PRE_SEGOVRD; break;
		case PREFIX_GS: pi->flag = INST_PRE_GS; pi->type = PRE_SEGOVRD; break;

		/* Op Size type: */
		case PREFIX_OP_SIZE: pi->flag = INST_PRE_OP_SIZE; pi->type = PRE_OPSIZE; break;

		/* Addr Size type: */
		case PREFIX_ADDR_SIZE: pi->flag = INST_PRE_ADDR_SIZE; pi->type = PRE_ADDRSIZE; break;
	}
}

void decode_prefixes(const uint8_t* code, int codeLen, _PrefixState* ps, _DecodeType dt)
{
	/*
	 * First thing to do, scan for prefixes, there are five types of prefixes.
	 * There may be up to five prefixes before a single instruction, not the same type, no special order,
	 * except REX must precede immediately the first opcode.
	 * BTW - This is the reason why I didn't make the REP prefixes part of the instructions (STOS/SCAS/etc).
	 *
	 * Another thing, the instruction maximum size is 15 bytes, thus if we read more than 15 bytes, we will halt.
	 */

	_PrefixInfo pi;
	ps->start = code;

	while ((--codeLen >= 0) || (code - ps->start >= INST_MAXIMUM_SIZE)) {
		/* Examine what type of prefix we got. */
		get_prefix_flag(*code, &pi, dt);
		if (pi.flag == INST_FLAGS_NONE) break; /* Halt scanning. */

		/*
		 * If we got something that is ALREADY included,
		 * we will have to skip that many bytes 'till we get past the first occurrence.
		 * Take a look: XYzABCUVz, the result would be: ABCUVz;  XYz are being dropped (we have to skip them),
		 * because then we would have z included twice, which is not allowed by 80x86.
		 */
		switch (pi.type)
		{
			case PRE_LOKREP:
				if ((ps->totalPrefixes & INST_PRE_LOKREP_MASK) != 0) { /* Is it second time we got this same type prefix? */
					/* Remove all flags of this group, because we don't know which is the set one. */
					ps->totalPrefixes &= ~INST_PRE_LOKREP_MASK;

					/*
					 * Check whether we have to remove other types.
					 * We remove other types if they appeared before the first repeating type.
					 * We also have to update flags and positions.
					 */
					if (ps->segovrdPos && ps->segovrdPos < ps->lokrepPos) {
						/* Update flags, remove any flag that is segment override concerned. */
						ps->totalPrefixes &= ~INST_PRE_SEGOVRD_MASK;
						ps->segovrdPos = NULL;
					}
					if (ps->opsizePos && ps->opsizePos < ps->lokrepPos) {
						ps->totalPrefixes &= ~INST_PRE_OP_SIZE; /* No need for mask, it's a single bit. */
						ps->opsizePos = NULL;
					}
					if (ps->addrsizePos && ps->addrsizePos < ps->lokrepPos) {
						ps->totalPrefixes &= ~INST_PRE_ADDR_SIZE;
						ps->addrsizePos = NULL;
					}
					if (ps->rexpos && ps->rexpos < ps->lokrepPos) {
						ps->totalPrefixes &= ~INST_PRE_REX;
						ps->rexpos = NULL;
					}

					/*
					 * Update current type position to last ^good^ position.
					 * Notice we update the position ONLY after we removed dropped prefixes, so they're dropped relative to the old one.
					 */
					ps->lokrepPos = code;

					/*
					 * start points to the first prefix we take into account (ignoring dropped prefixes).
					 * Notice we do the assignment after the above if statements, because maybe they could affect the result.
					 */
					ps->start = PREFIX_MIN(ps->segovrdPos, ps->opsizePos, ps->addrsizePos, ps->rexpos, ps->lokrepPos);
				} else {
					/* Update position to first occurence. */
					ps->lokrepPos = code;
				}
				/* Set flags anyways, if it's second time we cleaned the flags of this group already. */
				ps->totalPrefixes |= pi.flag;
			break;
			case PRE_SEGOVRD:
				if ((ps->totalPrefixes & INST_PRE_SEGOVRD_MASK) != 0) {
					ps->totalPrefixes &= ~INST_PRE_SEGOVRD_MASK;

					if (ps->lokrepPos && ps->lokrepPos < ps->segovrdPos) {
						ps->totalPrefixes &= ~INST_PRE_LOKREP_MASK;
						ps->lokrepPos = NULL;
					}
					if (ps->opsizePos && ps->opsizePos < ps->segovrdPos) {
						ps->totalPrefixes &= ~INST_PRE_OP_SIZE;
						ps->opsizePos = NULL;
					}
					if (ps->addrsizePos && ps->addrsizePos < ps->segovrdPos) {
						ps->totalPrefixes &= ~INST_PRE_ADDR_SIZE;
						ps->addrsizePos = NULL;
					}
					if (ps->rexpos && ps->rexpos < ps->segovrdPos) {
						ps->totalPrefixes &= ~INST_PRE_REX;
						ps->rexpos = NULL;
					}
					ps->segovrdPos = code;
					ps->start = PREFIX_MIN(ps->lokrepPos, ps->opsizePos, ps->addrsizePos, ps->rexpos, ps->segovrdPos);
				} else {
					ps->segovrdPos = code;
				}
				ps->totalPrefixes |= pi.flag;
			break;
			case PRE_OPSIZE:
				if (ps->totalPrefixes & pi.flag) {
					if (ps->lokrepPos && ps->lokrepPos < ps->opsizePos) {
						ps->totalPrefixes &= ~INST_PRE_LOKREP_MASK;
						ps->lokrepPos =	NULL;
					}
					if (ps->segovrdPos && ps->segovrdPos < ps->opsizePos) {
						ps->totalPrefixes &= ~INST_PRE_SEGOVRD_MASK;
						ps->segovrdPos = NULL;
					}
					if (ps->addrsizePos && ps->addrsizePos < ps->opsizePos) {
						ps->totalPrefixes &= ~INST_PRE_ADDR_SIZE;
						ps->addrsizePos = NULL;
					}
					if (ps->rexpos && ps->rexpos < ps->opsizePos) {
						ps->totalPrefixes &= ~INST_PRE_REX;
						ps->rexpos = NULL;
					}
					ps->opsizePos = code;
					ps->start = PREFIX_MIN(ps->lokrepPos, ps->segovrdPos, ps->addrsizePos, ps->rexpos, ps->opsizePos);
				} else {
					ps->totalPrefixes |= pi.flag;
					ps->opsizePos = code;
				}
			break;
			case PRE_ADDRSIZE:
				if (ps->totalPrefixes & pi.flag) {
					if (ps->lokrepPos && ps->lokrepPos < ps->addrsizePos) {
						ps->totalPrefixes &= ~INST_PRE_LOKREP_MASK;
						ps->lokrepPos = NULL;
					}
					if (ps->segovrdPos && ps->segovrdPos < ps->addrsizePos) {
						ps->totalPrefixes &= ~INST_PRE_SEGOVRD_MASK;
						ps->segovrdPos = NULL;
					}
					if (ps->opsizePos && ps->opsizePos < ps->addrsizePos) {
						ps->totalPrefixes &= ~INST_PRE_OP_SIZE;
						ps->opsizePos = NULL;
					}
					if (ps->rexpos && ps->rexpos < ps->addrsizePos) {
						ps->totalPrefixes &= ~INST_PRE_REX;
						ps->rexpos = NULL;
					}
					ps->addrsizePos = code;
					ps->start = PREFIX_MIN(ps->lokrepPos, ps->segovrdPos, ps->opsizePos, ps->rexpos, ps->addrsizePos);
				} else {
					ps->totalPrefixes |= pi.flag;
					ps->addrsizePos = code;
				}
			break;
			case PRE_REX:
				if (ps->totalPrefixes & pi.flag) {
					if (ps->lokrepPos && ps->lokrepPos < ps->rexpos) {
						ps->totalPrefixes &= ~INST_PRE_LOKREP_MASK;
						ps->lokrepPos = NULL;
					}
					if (ps->segovrdPos && ps->segovrdPos < ps->rexpos) {
						ps->totalPrefixes &= ~INST_PRE_SEGOVRD_MASK;
						ps->segovrdPos = NULL;
					}
					if (ps->opsizePos && ps->opsizePos < ps->rexpos) {
						ps->totalPrefixes &= ~INST_PRE_OP_SIZE;
						ps->opsizePos = NULL;
					}
					if (ps->addrsizePos && ps->addrsizePos < ps->rexpos) {
						ps->totalPrefixes &= ~INST_PRE_ADDR_SIZE;
						ps->addrsizePos = NULL;
					}
					ps->rexpos = code;
					ps->start = PREFIX_MIN(ps->lokrepPos, ps->segovrdPos, ps->opsizePos, ps->addrsizePos, ps->rexpos);
				} else {
					ps->totalPrefixes |= pi.flag;
					ps->rexpos = code;
				}
				break;
				default: return;
		}
		code++;
	}
	/*
	 * Save last byte scanned address, so the decoder could keep on scanning from this point and on and on and on.
	 * In addition the decoder is able to know that the last byte could lead to MMX/SSE instructions (preceding REX if exists).
	 */
	ps->last = code; /* ps->last points to the next byte following the last prefix byte! */
}

/*
* This function gets the prefix state of the last instruction and the total prefixes
* of that instruction which we got from the code itself,
* then it DB's everything which wasn't used, by looking at the instruction's prefixes we GOT
* and the USED prefixes (which is returned by the extract_operand).
* Finally, it returns a string of the output (unused prefixes).

* Note: This function should get the valid prefixes which weren't dropped.
* 			The Decode function deals with the dropped ones by itself.

* Here is a small sample for showing a case where there are some prefixes which weren't used, and should be DB'ed.

* 16 bits decoding mode:
* 40 ~ INC AX
* 66 40 ~ INC EAX
* 66 2E 40 ~ DB 2E; INC EAX
* 67 2E ~ DB 67; INC AX

* The whole thing is working by using the usedPrefixes, this variable gets updated
* whenever the decoder mechanism is affected by any one of the totalPrefixes prefixes.
* There are a few spots in the code along some of the decoder functions which are responsible for updating the usedPrefixes.

* When calling this function, you assume totalPrefixes contains all non-dropped prefixes for that instruction,
* and also the usedPrefixes is filled already.
*/

void get_unused_prefixes_list(uint8_t unusedList[MAX_PREFIXES], _PrefixState* ps)
{
	/* We might have 5 unused prefixes at most, up to 5 prefixes for one instruction. */
	const uint8_t* ptrs[MAX_PREFIXES] = {0};

	unsigned int i, j;
	const uint8_t* tmp = NULL;

	memset(unusedList, 0, MAX_PREFIXES);

	/*
	 * We have to restore at this point flags that were disabled manually, because they are ignored in 64 bits.
	 * Therefore, we will have to check whether they were set before we disabled them and now reenable them in order
	 * to output them as unused prefixes.
	 */

	/* Check out whether the REX prefix was ignored... and reenable it, so we can see if it were used or not. */
	if (ps->rexpos != NULL) {
		ps->totalPrefixes |= INST_PRE_REX;
		/* Reenable operand size prefix, so it will be dropped as unused. */
		if ((ps->opsizePos != NULL) && (*ps->rexpos & PREFIX_REX_W)) ps->totalPrefixes |= INST_PRE_OP_SIZE;
	}
	/* It could be that we disabled any one of the segment overrides. */
	if ((ps->segovrdPos != NULL) && ((ps->totalPrefixes & (INST_PRE_SEGOVRD_MASK)) == 0)) {
		 /*
		  * We have to reenable it manually.
		  * Doesn't really matter which one we use to enable it.
		  */
		ps->totalPrefixes |= INST_PRE_CS;
	}

	/* Remove all used prefixes. */
	ps->totalPrefixes &= ~ps->usedPrefixes;

	/* Caller function depends on this value, so it will know how many unused prefixes there are. */
	ps->unusedCount = 0;

	/* All are used? Cool then. */
	if (ps->totalPrefixes == ps->usedPrefixes) return ;

	/*
	 * Determine what types of prefixes were unused.
	 * Every type has only one pointer to that prefix.
	 */
	if (ps->totalPrefixes & INST_PRE_OP_SIZE) ptrs[ps->unusedCount++] = ps->opsizePos;
	if (ps->totalPrefixes & INST_PRE_ADDR_SIZE) ptrs[ps->unusedCount++] = ps->addrsizePos;
	if (ps->totalPrefixes & INST_PRE_LOKREP_MASK) ptrs[ps->unusedCount++] = ps->lokrepPos;
	if (ps->totalPrefixes & INST_PRE_SEGOVRD_MASK) ptrs[ps->unusedCount++] = ps->segovrdPos;
	if (ps->totalPrefixes & INST_PRE_REX) ptrs[ps->unusedCount++] = ps->rexpos;

	/*
	 * Sort them, so you output them by their real order.
	 * Bubble yak is good enough.
	 */
	for (i = 0; i < ps->unusedCount; i++) {
		for (j = 0; j < ps->unusedCount; j++) {
			if (ptrs[j] > ptrs[i]) {
				tmp = ptrs[j];
				ptrs[j] = ptrs[i];
				ptrs[i] = tmp;
			}
		}
	}

	/* Get values and store in the given array. */
	for (i = 0; i < ps->unusedCount; i++)
		unusedList[i] = *ptrs[i];
}


/*
 * Concatenates a string of the used segment by examining the prefixes.
 * Side Effects - This function will change the prefix state variables on certain cases.
 */
void str_seg_text(_WString* s, _PrefixState* ps, _DecodeType dt)
{
	_iflags flags = ps->totalPrefixes & INST_PRE_SEGOVRD_MASK;
	/* Segment Override prefixes are ignored in 64 bits. */
	if (flags == 0) return;

	/* 64 bits mode ignoers some prefixes. */
	if (dt != Decode64Bits) {
		switch (flags)
		{
			case INST_PRE_CS:
				ps->usedPrefixes |= INST_PRE_CS;
				strcat_WSN(s, PREFIX_CS_TEXT);
				chrcat_WS(s, SEG_OFF_CHR);
			return;
			case INST_PRE_SS:
				ps->usedPrefixes |= INST_PRE_SS;
				strcat_WSN(s, PREFIX_SS_TEXT);
				chrcat_WS(s, SEG_OFF_CHR);
			return;
			case INST_PRE_DS:
				ps->usedPrefixes |= INST_PRE_DS;
				strcat_WSN(s, PREFIX_DS_TEXT);
				chrcat_WS(s, SEG_OFF_CHR);
			return;
			case INST_PRE_ES:
				ps->usedPrefixes |= INST_PRE_ES;
				strcat_WSN(s, PREFIX_ES_TEXT);
				chrcat_WS(s, SEG_OFF_CHR);
			return;
		}
	}
	switch (flags)
	{
		case INST_PRE_FS:
			ps->usedPrefixes |= INST_PRE_FS;
			strcat_WSN(s, PREFIX_FS_TEXT);
			chrcat_WS(s, SEG_OFF_CHR);
		return;
		case INST_PRE_GS:
			ps->usedPrefixes |= INST_PRE_GS;
			strcat_WSN(s, PREFIX_GS_TEXT);
			chrcat_WS(s, SEG_OFF_CHR);
		return;
	}
}
