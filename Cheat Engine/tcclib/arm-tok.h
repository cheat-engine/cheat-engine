/* ------------------------------------------------------------------ */
/* WARNING: relative order of tokens is important.                    */

/* register */

 DEF_ASM(r0)
 DEF_ASM(r1)
 DEF_ASM(r2)
 DEF_ASM(r3)
 DEF_ASM(r4)
 DEF_ASM(r5)
 DEF_ASM(r6)
 DEF_ASM(r7)
 DEF_ASM(r8)
 DEF_ASM(r9)
 DEF_ASM(r10)
 DEF_ASM(r11) /* fp */
 DEF_ASM(r12) /* ip[c] */
 DEF_ASM(r13) /* sp */
 DEF_ASM(r14) /* lr */
 DEF_ASM(r15) /* pc */

/* register macros */

 DEF_ASM(fp) /* alias for r11 */
 DEF_ASM(ip) /* alias for r12 */
 DEF_ASM(sp) /* alias for r13 */
 DEF_ASM(lr) /* alias for r14 */
 DEF_ASM(pc) /* alias for r15 */

 /* data processing directives */

 DEF_ASM(asl)

#define ARM_INSTRUCTION_GROUP(tok) ((((tok) - TOK_ASM_nopeq) & 0xFFFFFFF0) + TOK_ASM_nopeq)

/* Note: condition code is 4 bits */
#define DEF_ASM_CONDED(x) \
  DEF(TOK_ASM_ ## x ## eq, #x "eq") \
  DEF(TOK_ASM_ ## x ## ne, #x "ne") \
  DEF(TOK_ASM_ ## x ## cs, #x "cs") \
  DEF(TOK_ASM_ ## x ## cc, #x "cc") \
  DEF(TOK_ASM_ ## x ## mi, #x "mi") \
  DEF(TOK_ASM_ ## x ## pl, #x "pl") \
  DEF(TOK_ASM_ ## x ## vs, #x "vs") \
  DEF(TOK_ASM_ ## x ## vc, #x "vc") \
  DEF(TOK_ASM_ ## x ## hi, #x "hi") \
  DEF(TOK_ASM_ ## x ## ls, #x "ls") \
  DEF(TOK_ASM_ ## x ## ge, #x "ge") \
  DEF(TOK_ASM_ ## x ## lt, #x "lt") \
  DEF(TOK_ASM_ ## x ## gt, #x "gt") \
  DEF(TOK_ASM_ ## x ## le, #x "le") \
  DEF(TOK_ASM_ ## x, #x) \
  DEF(TOK_ASM_ ## x ## rsvd, #x "rsvd")

/* Note: add new tokens after nop (MUST always use DEF_ASM_CONDED) */

 DEF_ASM_CONDED(nop)
 DEF_ASM_CONDED(wfe)
 DEF_ASM_CONDED(wfi)
 DEF_ASM_CONDED(swi)

 /* misc */
 DEF_ASM_CONDED(clz)

 /* size conversion */

 DEF_ASM_CONDED(sxtb)
 DEF_ASM_CONDED(sxth)
 DEF_ASM_CONDED(uxtb)
 DEF_ASM_CONDED(uxth)
 DEF_ASM_CONDED(movt)
 DEF_ASM_CONDED(movw)

 /* multiplication */

 DEF_ASM_CONDED(mul)
 DEF_ASM_CONDED(muls)
 DEF_ASM_CONDED(mla)
 DEF_ASM_CONDED(mlas)
 DEF_ASM_CONDED(smull)
 DEF_ASM_CONDED(smulls)
 DEF_ASM_CONDED(umull)
 DEF_ASM_CONDED(umulls)
 DEF_ASM_CONDED(smlal)
 DEF_ASM_CONDED(smlals)
 DEF_ASM_CONDED(umlal)
 DEF_ASM_CONDED(umlals)

 /* load/store */

 DEF_ASM_CONDED(ldr)
 DEF_ASM_CONDED(ldrb)
 DEF_ASM_CONDED(str)
 DEF_ASM_CONDED(strb)
 DEF_ASM_CONDED(ldrex)
 DEF_ASM_CONDED(ldrexb)
 DEF_ASM_CONDED(strex)
 DEF_ASM_CONDED(strexb)
 DEF_ASM_CONDED(ldrh)
 DEF_ASM_CONDED(ldrsh)
 DEF_ASM_CONDED(ldrsb)
 DEF_ASM_CONDED(strh)

 DEF_ASM_CONDED(stmda)
 DEF_ASM_CONDED(ldmda)
 DEF_ASM_CONDED(stm)
 DEF_ASM_CONDED(ldm)
 DEF_ASM_CONDED(stmia)
 DEF_ASM_CONDED(ldmia)
 DEF_ASM_CONDED(stmdb)
 DEF_ASM_CONDED(ldmdb)
 DEF_ASM_CONDED(stmib)
 DEF_ASM_CONDED(ldmib)

 /* instruction macros */

 DEF_ASM_CONDED(push)
 DEF_ASM_CONDED(pop)

 /* branches */

 DEF_ASM_CONDED(b)
 DEF_ASM_CONDED(bl)
 DEF_ASM_CONDED(bx)
 DEF_ASM_CONDED(blx)

 /* data processing instructions; order is important */

 DEF_ASM_CONDED(and)
 DEF_ASM_CONDED(ands)
 DEF_ASM_CONDED(eor)
 DEF_ASM_CONDED(eors)
 DEF_ASM_CONDED(sub)
 DEF_ASM_CONDED(subs)
 DEF_ASM_CONDED(rsb)
 DEF_ASM_CONDED(rsbs)
 DEF_ASM_CONDED(add)
 DEF_ASM_CONDED(adds)
 DEF_ASM_CONDED(adc)
 DEF_ASM_CONDED(adcs)
 DEF_ASM_CONDED(sbc)
 DEF_ASM_CONDED(sbcs)
 DEF_ASM_CONDED(rsc)
 DEF_ASM_CONDED(rscs)
 DEF_ASM_CONDED(tst)
 DEF_ASM_CONDED(tsts) // necessary here--but not useful to the user
 DEF_ASM_CONDED(teq)
 DEF_ASM_CONDED(teqs) // necessary here--but not useful to the user
 DEF_ASM_CONDED(cmp)
 DEF_ASM_CONDED(cmps) // necessary here--but not useful to the user
 DEF_ASM_CONDED(cmn)
 DEF_ASM_CONDED(cmns) // necessary here--but not useful to the user
 DEF_ASM_CONDED(orr)
 DEF_ASM_CONDED(orrs)
 DEF_ASM_CONDED(mov)
 DEF_ASM_CONDED(movs)
 DEF_ASM_CONDED(bic)
 DEF_ASM_CONDED(bics)
 DEF_ASM_CONDED(mvn)
 DEF_ASM_CONDED(mvns)

 DEF_ASM_CONDED(lsl)
 DEF_ASM_CONDED(lsls)
 DEF_ASM_CONDED(lsr)
 DEF_ASM_CONDED(lsrs)
 DEF_ASM_CONDED(asr)
 DEF_ASM_CONDED(asrs)
 DEF_ASM_CONDED(ror)
 DEF_ASM_CONDED(rors)
 DEF_ASM_CONDED(rrx)
 DEF_ASM_CONDED(rrxs)
