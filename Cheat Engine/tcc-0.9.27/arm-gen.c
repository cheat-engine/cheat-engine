/*
 *  ARMv4 code generator for TCC
 *
 *  Copyright (c) 2003 Daniel Glöckner
 *  Copyright (c) 2012 Thomas Preud'homme
 *
 *  Based on i386-gen.c by Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef TARGET_DEFS_ONLY

#if defined(TCC_ARM_EABI) && !defined(TCC_ARM_VFP)
#error "Currently TinyCC only supports float computation with VFP instructions"
#endif

/* number of available registers */
#ifdef TCC_ARM_VFP
#define NB_REGS            13
#else
#define NB_REGS             9
#endif

#ifndef TCC_CPU_VERSION
# define TCC_CPU_VERSION 5
#endif

/* a register can belong to several classes. The classes must be
   sorted from more general to more precise (see gv2() code which does
   assumptions on it). */
#define RC_INT     0x0001 /* generic integer register */
#define RC_FLOAT   0x0002 /* generic float register */
#define RC_R0      0x0004
#define RC_R1      0x0008
#define RC_R2      0x0010
#define RC_R3      0x0020
#define RC_R12     0x0040
#define RC_F0      0x0080
#define RC_F1      0x0100
#define RC_F2      0x0200
#define RC_F3      0x0400
#ifdef TCC_ARM_VFP
#define RC_F4      0x0800
#define RC_F5      0x1000
#define RC_F6      0x2000
#define RC_F7      0x4000
#endif
#define RC_IRET    RC_R0  /* function return: integer register */
#define RC_LRET    RC_R1  /* function return: second integer register */
#define RC_FRET    RC_F0  /* function return: float register */

/* pretty names for the registers */
enum {
    TREG_R0 = 0,
    TREG_R1,
    TREG_R2,
    TREG_R3,
    TREG_R12,
    TREG_F0,
    TREG_F1,
    TREG_F2,
    TREG_F3,
#ifdef TCC_ARM_VFP
    TREG_F4,
    TREG_F5,
    TREG_F6,
    TREG_F7,
#endif
    TREG_SP = 13,
    TREG_LR,
};

#ifdef TCC_ARM_VFP
#define T2CPR(t) (((t) & VT_BTYPE) != VT_FLOAT ? 0x100 : 0)
#endif

/* return registers for function */
#define REG_IRET TREG_R0 /* single word int return register */
#define REG_LRET TREG_R1 /* second word return register (for long long) */
#define REG_FRET TREG_F0 /* float return register */

#ifdef TCC_ARM_EABI
#define TOK___divdi3 TOK___aeabi_ldivmod
#define TOK___moddi3 TOK___aeabi_ldivmod
#define TOK___udivdi3 TOK___aeabi_uldivmod
#define TOK___umoddi3 TOK___aeabi_uldivmod
#endif

/* defined if function parameters must be evaluated in reverse order */
#define INVERT_FUNC_PARAMS

/* defined if structures are passed as pointers. Otherwise structures
   are directly pushed on stack. */
/* #define FUNC_STRUCT_PARAM_AS_PTR */

/* pointer size, in bytes */
#define PTR_SIZE 4

/* long double size and alignment, in bytes */
#ifdef TCC_ARM_VFP
#define LDOUBLE_SIZE  8
#endif

#ifndef LDOUBLE_SIZE
#define LDOUBLE_SIZE  8
#endif

#ifdef TCC_ARM_EABI
#define LDOUBLE_ALIGN 8
#else
#define LDOUBLE_ALIGN 4
#endif

/* maximum alignment (for aligned attribute support) */
#define MAX_ALIGN     8

#define CHAR_IS_UNSIGNED

/******************************************************/
#else /* ! TARGET_DEFS_ONLY */
/******************************************************/
#include "tcc.h"

enum float_abi float_abi;

ST_DATA const int reg_classes[NB_REGS] = {
    /* r0 */ RC_INT | RC_R0,
    /* r1 */ RC_INT | RC_R1,
    /* r2 */ RC_INT | RC_R2,
    /* r3 */ RC_INT | RC_R3,
    /* r12 */ RC_INT | RC_R12,
    /* f0 */ RC_FLOAT | RC_F0,
    /* f1 */ RC_FLOAT | RC_F1,
    /* f2 */ RC_FLOAT | RC_F2,
    /* f3 */ RC_FLOAT | RC_F3,
#ifdef TCC_ARM_VFP
 /* d4/s8 */ RC_FLOAT | RC_F4,
/* d5/s10 */ RC_FLOAT | RC_F5,
/* d6/s12 */ RC_FLOAT | RC_F6,
/* d7/s14 */ RC_FLOAT | RC_F7,
#endif
};

static int func_sub_sp_offset, last_itod_magic;
static int leaffunc;

#if defined(TCC_ARM_EABI) && defined(TCC_ARM_VFP)
static CType float_type, double_type, func_float_type, func_double_type;
ST_FUNC void arm_init(struct TCCState *s)
{
    float_type.t = VT_FLOAT;
    double_type.t = VT_DOUBLE;
    func_float_type.t = VT_FUNC;
    func_float_type.ref = sym_push(SYM_FIELD, &float_type, FUNC_CDECL, FUNC_OLD);
    func_double_type.t = VT_FUNC;
    func_double_type.ref = sym_push(SYM_FIELD, &double_type, FUNC_CDECL, FUNC_OLD);

    float_abi = s->float_abi;
#ifndef TCC_ARM_HARDFLOAT
    tcc_warning("soft float ABI currently not supported: default to softfp");
#endif
}
#else
#define func_float_type func_old_type
#define func_double_type func_old_type
#define func_ldouble_type func_old_type
ST_FUNC void arm_init(struct TCCState *s)
{
#if 0
#if !defined (TCC_ARM_VFP)
    tcc_warning("Support for FPA is deprecated and will be removed in next"
                " release");
#endif
#if !defined (TCC_ARM_EABI)
    tcc_warning("Support for OABI is deprecated and will be removed in next"
                " release");
#endif
#endif
}
#endif

static int two2mask(int a,int b) {
  return (reg_classes[a]|reg_classes[b])&~(RC_INT|RC_FLOAT);
}

static int regmask(int r) {
  return reg_classes[r]&~(RC_INT|RC_FLOAT);
}

/******************************************************/

#if defined(TCC_ARM_EABI) && !defined(CONFIG_TCC_ELFINTERP)
const char *default_elfinterp(struct TCCState *s)
{
    if (s->float_abi == ARM_HARD_FLOAT)
        return "/lib/ld-linux-armhf.so.3";
    else
        return "/lib/ld-linux.so.3";
}
#endif

void o(uint32_t i)
{
  /* this is a good place to start adding big-endian support*/
  int ind1;
  if (nocode_wanted)
    return;
  ind1 = ind + 4;
  if (!cur_text_section)
    tcc_error("compiler error! This happens f.ex. if the compiler\n"
         "can't evaluate constant expressions outside of a function.");
  if (ind1 > cur_text_section->data_allocated)
    section_realloc(cur_text_section, ind1);
  cur_text_section->data[ind++] = i&255;
  i>>=8;
  cur_text_section->data[ind++] = i&255;
  i>>=8;
  cur_text_section->data[ind++] = i&255;
  i>>=8;
  cur_text_section->data[ind++] = i;
}

static uint32_t stuff_const(uint32_t op, uint32_t c)
{
  int try_neg=0;
  uint32_t nc = 0, negop = 0;

  switch(op&0x1F00000)
  {
    case 0x800000: //add
    case 0x400000: //sub
      try_neg=1;
      negop=op^0xC00000;
      nc=-c;
      break;
    case 0x1A00000: //mov
    case 0x1E00000: //mvn
      try_neg=1;
      negop=op^0x400000;
      nc=~c;
      break;
    case 0x200000: //xor
      if(c==~0)
	return (op&0xF010F000)|((op>>16)&0xF)|0x1E00000;
      break;
    case 0x0: //and
      if(c==~0)
	return (op&0xF010F000)|((op>>16)&0xF)|0x1A00000;
    case 0x1C00000: //bic
      try_neg=1;
      negop=op^0x1C00000;
      nc=~c;
      break;
    case 0x1800000: //orr
      if(c==~0)
	return (op&0xFFF0FFFF)|0x1E00000;
      break;
  }
  do {
    uint32_t m;
    int i;
    if(c<256) /* catch undefined <<32 */
      return op|c;
    for(i=2;i<32;i+=2) {
      m=(0xff>>i)|(0xff<<(32-i));
      if(!(c&~m))
	return op|(i<<7)|(c<<i)|(c>>(32-i));
    }
    op=negop;
    c=nc;
  } while(try_neg--);
  return 0;
}


//only add,sub
void stuff_const_harder(uint32_t op, uint32_t v) {
  uint32_t x;
  x=stuff_const(op,v);
  if(x)
    o(x);
  else {
    uint32_t a[16], nv, no, o2, n2;
    int i,j,k;
    a[0]=0xff;
    o2=(op&0xfff0ffff)|((op&0xf000)<<4);;
    for(i=1;i<16;i++)
      a[i]=(a[i-1]>>2)|(a[i-1]<<30);
    for(i=0;i<12;i++)
      for(j=i<4?i+12:15;j>=i+4;j--)
	if((v&(a[i]|a[j]))==v) {
	  o(stuff_const(op,v&a[i]));
	  o(stuff_const(o2,v&a[j]));
	  return;
	}
    no=op^0xC00000;
    n2=o2^0xC00000;
    nv=-v;
    for(i=0;i<12;i++)
      for(j=i<4?i+12:15;j>=i+4;j--)
	if((nv&(a[i]|a[j]))==nv) {
	  o(stuff_const(no,nv&a[i]));
	  o(stuff_const(n2,nv&a[j]));
	  return;
	}
    for(i=0;i<8;i++)
      for(j=i+4;j<12;j++)
	for(k=i<4?i+12:15;k>=j+4;k--)
	  if((v&(a[i]|a[j]|a[k]))==v) {
	    o(stuff_const(op,v&a[i]));
	    o(stuff_const(o2,v&a[j]));
	    o(stuff_const(o2,v&a[k]));
	    return;
	  }
    no=op^0xC00000;
    nv=-v;
    for(i=0;i<8;i++)
      for(j=i+4;j<12;j++)
	for(k=i<4?i+12:15;k>=j+4;k--)
	  if((nv&(a[i]|a[j]|a[k]))==nv) {
	    o(stuff_const(no,nv&a[i]));
	    o(stuff_const(n2,nv&a[j]));
	    o(stuff_const(n2,nv&a[k]));
	    return;
	  }
    o(stuff_const(op,v&a[0]));
    o(stuff_const(o2,v&a[4]));
    o(stuff_const(o2,v&a[8]));
    o(stuff_const(o2,v&a[12]));
  }
}

uint32_t encbranch(int pos, int addr, int fail)
{
  addr-=pos+8;
  addr/=4;
  if(addr>=0x1000000 || addr<-0x1000000) {
    if(fail)
      tcc_error("FIXME: function bigger than 32MB");
    return 0;
  }
  return 0x0A000000|(addr&0xffffff);
}

int decbranch(int pos)
{
  int x;
  x=*(uint32_t *)(cur_text_section->data + pos);
  x&=0x00ffffff;
  if(x&0x800000)
    x-=0x1000000;
  return x*4+pos+8;
}

/* output a symbol and patch all calls to it */
void gsym_addr(int t, int a)
{
  uint32_t *x;
  int lt;
  while(t) {
    x=(uint32_t *)(cur_text_section->data + t);
    t=decbranch(lt=t);
    if(a==lt+4)
      *x=0xE1A00000; // nop
    else {
      *x &= 0xff000000;
      *x |= encbranch(lt,a,1);
    }
  }
}

void gsym(int t)
{
  gsym_addr(t, ind);
}

#ifdef TCC_ARM_VFP
static uint32_t vfpr(int r)
{
  if(r<TREG_F0 || r>TREG_F7)
    tcc_error("compiler error! register %i is no vfp register",r);
  return r - TREG_F0;
}
#else
static uint32_t fpr(int r)
{
  if(r<TREG_F0 || r>TREG_F3)
    tcc_error("compiler error! register %i is no fpa register",r);
  return r - TREG_F0;
}
#endif

static uint32_t intr(int r)
{
  if(r == TREG_R12)
    return 12;
  if(r >= TREG_R0 && r <= TREG_R3)
    return r - TREG_R0;
  if (r >= TREG_SP && r <= TREG_LR)
    return r + (13 - TREG_SP);
  tcc_error("compiler error! register %i is no int register",r);
}

static void calcaddr(uint32_t *base, int *off, int *sgn, int maxoff, unsigned shift)
{
  if(*off>maxoff || *off&((1<<shift)-1)) {
    uint32_t x, y;
    x=0xE280E000;
    if(*sgn)
      x=0xE240E000;
    x|=(*base)<<16;
    *base=14; // lr
    y=stuff_const(x,*off&~maxoff);
    if(y) {
      o(y);
      *off&=maxoff;
      return;
    }
    y=stuff_const(x,(*off+maxoff)&~maxoff);
    if(y) {
      o(y);
      *sgn=!*sgn;
      *off=((*off+maxoff)&~maxoff)-*off;
      return;
    }
    stuff_const_harder(x,*off&~maxoff);
    *off&=maxoff;
  }
}

static uint32_t mapcc(int cc)
{
  switch(cc)
  {
    case TOK_ULT:
      return 0x30000000; /* CC/LO */
    case TOK_UGE:
      return 0x20000000; /* CS/HS */
    case TOK_EQ:
      return 0x00000000; /* EQ */
    case TOK_NE:
      return 0x10000000; /* NE */
    case TOK_ULE:
      return 0x90000000; /* LS */
    case TOK_UGT:
      return 0x80000000; /* HI */
    case TOK_Nset:
      return 0x40000000; /* MI */
    case TOK_Nclear:
      return 0x50000000; /* PL */
    case TOK_LT:
      return 0xB0000000; /* LT */
    case TOK_GE:
      return 0xA0000000; /* GE */
    case TOK_LE:
      return 0xD0000000; /* LE */
    case TOK_GT:
      return 0xC0000000; /* GT */
  }
  tcc_error("unexpected condition code");
  return 0xE0000000; /* AL */
}

static int negcc(int cc)
{
  switch(cc)
  {
    case TOK_ULT:
      return TOK_UGE;
    case TOK_UGE:
      return TOK_ULT;
    case TOK_EQ:
      return TOK_NE;
    case TOK_NE:
      return TOK_EQ;
    case TOK_ULE:
      return TOK_UGT;
    case TOK_UGT:
      return TOK_ULE;
    case TOK_Nset:
      return TOK_Nclear;
    case TOK_Nclear:
      return TOK_Nset;
    case TOK_LT:
      return TOK_GE;
    case TOK_GE:
      return TOK_LT;
    case TOK_LE:
      return TOK_GT;
    case TOK_GT:
      return TOK_LE;
  }
  tcc_error("unexpected condition code");
  return TOK_NE;
}

/* load 'r' from value 'sv' */
void load(int r, SValue *sv)
{
  int v, ft, fc, fr, sign;
  uint32_t op;
  SValue v1;

  fr = sv->r;
  ft = sv->type.t;
  fc = sv->c.i;

  if(fc>=0)
    sign=0;
  else {
    sign=1;
    fc=-fc;
  }

  v = fr & VT_VALMASK;
  if (fr & VT_LVAL) {
    uint32_t base = 0xB; // fp
    if(v == VT_LLOCAL) {
      v1.type.t = VT_PTR;
      v1.r = VT_LOCAL | VT_LVAL;
      v1.c.i = sv->c.i;
      load(TREG_LR, &v1);
      base = 14; /* lr */
      fc=sign=0;
      v=VT_LOCAL;
    } else if(v == VT_CONST) {
      v1.type.t = VT_PTR;
      v1.r = fr&~VT_LVAL;
      v1.c.i = sv->c.i;
      v1.sym=sv->sym;
      load(TREG_LR, &v1);
      base = 14; /* lr */
      fc=sign=0;
      v=VT_LOCAL;
    } else if(v < VT_CONST) {
      base=intr(v);
      fc=sign=0;
      v=VT_LOCAL;
    }
    if(v == VT_LOCAL) {
      if(is_float(ft)) {
	calcaddr(&base,&fc,&sign,1020,2);
#ifdef TCC_ARM_VFP
        op=0xED100A00; /* flds */
        if(!sign)
          op|=0x800000;
        if ((ft & VT_BTYPE) != VT_FLOAT)
          op|=0x100;   /* flds -> fldd */
        o(op|(vfpr(r)<<12)|(fc>>2)|(base<<16));
#else
	op=0xED100100;
	if(!sign)
	  op|=0x800000;
#if LDOUBLE_SIZE == 8
	if ((ft & VT_BTYPE) != VT_FLOAT)
	  op|=0x8000;
#else
	if ((ft & VT_BTYPE) == VT_DOUBLE)
	  op|=0x8000;
	else if ((ft & VT_BTYPE) == VT_LDOUBLE)
	  op|=0x400000;
#endif
	o(op|(fpr(r)<<12)|(fc>>2)|(base<<16));
#endif
      } else if((ft & (VT_BTYPE|VT_UNSIGNED)) == VT_BYTE
                || (ft & VT_BTYPE) == VT_SHORT) {
	calcaddr(&base,&fc,&sign,255,0);
	op=0xE1500090;
	if ((ft & VT_BTYPE) == VT_SHORT)
	  op|=0x20;
	if ((ft & VT_UNSIGNED) == 0)
	  op|=0x40;
	if(!sign)
	  op|=0x800000;
	o(op|(intr(r)<<12)|(base<<16)|((fc&0xf0)<<4)|(fc&0xf));
      } else {
	calcaddr(&base,&fc,&sign,4095,0);
	op=0xE5100000;
	if(!sign)
	  op|=0x800000;
        if ((ft & VT_BTYPE) == VT_BYTE || (ft & VT_BTYPE) == VT_BOOL)
          op|=0x400000;
        o(op|(intr(r)<<12)|fc|(base<<16));
      }
      return;
    }
  } else {
    if (v == VT_CONST) {
      op=stuff_const(0xE3A00000|(intr(r)<<12),sv->c.i);
      if (fr & VT_SYM || !op) {
        o(0xE59F0000|(intr(r)<<12));
        o(0xEA000000);
        if(fr & VT_SYM)
	  greloc(cur_text_section, sv->sym, ind, R_ARM_ABS32);
        o(sv->c.i);
      } else
        o(op);
      return;
    } else if (v == VT_LOCAL) {
      op=stuff_const(0xE28B0000|(intr(r)<<12),sv->c.i);
      if (fr & VT_SYM || !op) {
	o(0xE59F0000|(intr(r)<<12));
	o(0xEA000000);
	if(fr & VT_SYM) // needed ?
	  greloc(cur_text_section, sv->sym, ind, R_ARM_ABS32);
	o(sv->c.i);
	o(0xE08B0000|(intr(r)<<12)|intr(r));
      } else
	o(op);
      return;
    } else if(v == VT_CMP) {
      o(mapcc(sv->c.i)|0x3A00001|(intr(r)<<12));
      o(mapcc(negcc(sv->c.i))|0x3A00000|(intr(r)<<12));
      return;
    } else if (v == VT_JMP || v == VT_JMPI) {
      int t;
      t = v & 1;
      o(0xE3A00000|(intr(r)<<12)|t);
      o(0xEA000000);
      gsym(sv->c.i);
      o(0xE3A00000|(intr(r)<<12)|(t^1));
      return;
    } else if (v < VT_CONST) {
      if(is_float(ft))
#ifdef TCC_ARM_VFP
        o(0xEEB00A40|(vfpr(r)<<12)|vfpr(v)|T2CPR(ft)); /* fcpyX */
#else
	o(0xEE008180|(fpr(r)<<12)|fpr(v));
#endif
      else
	o(0xE1A00000|(intr(r)<<12)|intr(v));
      return;
    }
  }
  tcc_error("load unimplemented!");
}

/* store register 'r' in lvalue 'v' */
void store(int r, SValue *sv)
{
  SValue v1;
  int v, ft, fc, fr, sign;
  uint32_t op;

  fr = sv->r;
  ft = sv->type.t;
  fc = sv->c.i;

  if(fc>=0)
    sign=0;
  else {
    sign=1;
    fc=-fc;
  }

  v = fr & VT_VALMASK;
  if (fr & VT_LVAL || fr == VT_LOCAL) {
    uint32_t base = 0xb; /* fp */
    if(v < VT_CONST) {
      base=intr(v);
      v=VT_LOCAL;
      fc=sign=0;
    } else if(v == VT_CONST) {
      v1.type.t = ft;
      v1.r = fr&~VT_LVAL;
      v1.c.i = sv->c.i;
      v1.sym=sv->sym;
      load(TREG_LR, &v1);
      base = 14; /* lr */
      fc=sign=0;
      v=VT_LOCAL;
    }
    if(v == VT_LOCAL) {
       if(is_float(ft)) {
	calcaddr(&base,&fc,&sign,1020,2);
#ifdef TCC_ARM_VFP
        op=0xED000A00; /* fsts */
        if(!sign)
          op|=0x800000;
        if ((ft & VT_BTYPE) != VT_FLOAT)
          op|=0x100;   /* fsts -> fstd */
        o(op|(vfpr(r)<<12)|(fc>>2)|(base<<16));
#else
	op=0xED000100;
	if(!sign)
	  op|=0x800000;
#if LDOUBLE_SIZE == 8
	if ((ft & VT_BTYPE) != VT_FLOAT)
	  op|=0x8000;
#else
	if ((ft & VT_BTYPE) == VT_DOUBLE)
	  op|=0x8000;
	if ((ft & VT_BTYPE) == VT_LDOUBLE)
	  op|=0x400000;
#endif
	o(op|(fpr(r)<<12)|(fc>>2)|(base<<16));
#endif
	return;
      } else if((ft & VT_BTYPE) == VT_SHORT) {
	calcaddr(&base,&fc,&sign,255,0);
	op=0xE14000B0;
	if(!sign)
	  op|=0x800000;
	o(op|(intr(r)<<12)|(base<<16)|((fc&0xf0)<<4)|(fc&0xf));
      } else {
	calcaddr(&base,&fc,&sign,4095,0);
	op=0xE5000000;
	if(!sign)
	  op|=0x800000;
        if ((ft & VT_BTYPE) == VT_BYTE || (ft & VT_BTYPE) == VT_BOOL)
          op|=0x400000;
        o(op|(intr(r)<<12)|fc|(base<<16));
      }
      return;
    }
  }
  tcc_error("store unimplemented");
}

static void gadd_sp(int val)
{
  stuff_const_harder(0xE28DD000,val);
}

/* 'is_jmp' is '1' if it is a jump */
static void gcall_or_jmp(int is_jmp)
{
  int r;
  if ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
    uint32_t x;
    /* constant case */
    x=encbranch(ind,ind+vtop->c.i,0);
    if(x) {
      if (vtop->r & VT_SYM) {
	/* relocation case */
	greloc(cur_text_section, vtop->sym, ind, R_ARM_PC24);
      } else
	put_elf_reloc(symtab_section, cur_text_section, ind, R_ARM_PC24, 0);
      o(x|(is_jmp?0xE0000000:0xE1000000));
    } else {
      if(!is_jmp)
	o(0xE28FE004); // add lr,pc,#4
      o(0xE51FF004);   // ldr pc,[pc,#-4]
      if (vtop->r & VT_SYM)
	greloc(cur_text_section, vtop->sym, ind, R_ARM_ABS32);
      o(vtop->c.i);
    }
  } else {
    /* otherwise, indirect call */
    r = gv(RC_INT);
    if(!is_jmp)
      o(0xE1A0E00F);       // mov lr,pc
    o(0xE1A0F000|intr(r)); // mov pc,r
  }
}

static int unalias_ldbl(int btype)
{
#if LDOUBLE_SIZE == 8
    if (btype == VT_LDOUBLE)
      btype = VT_DOUBLE;
#endif
    return btype;
}

/* Return whether a structure is an homogeneous float aggregate or not.
   The answer is true if all the elements of the structure are of the same
   primitive float type and there is less than 4 elements.

   type: the type corresponding to the structure to be tested */
static int is_hgen_float_aggr(CType *type)
{
  if ((type->t & VT_BTYPE) == VT_STRUCT) {
    struct Sym *ref;
    int btype, nb_fields = 0;

    ref = type->ref->next;
    btype = unalias_ldbl(ref->type.t & VT_BTYPE);
    if (btype == VT_FLOAT || btype == VT_DOUBLE) {
      for(; ref && btype == unalias_ldbl(ref->type.t & VT_BTYPE); ref = ref->next, nb_fields++);
      return !ref && nb_fields <= 4;
    }
  }
  return 0;
}

struct avail_regs {
  signed char avail[3]; /* 3 holes max with only float and double alignments */
  int first_hole; /* first available hole */
  int last_hole; /* last available hole (none if equal to first_hole) */
  int first_free_reg; /* next free register in the sequence, hole excluded */
};

#define AVAIL_REGS_INITIALIZER (struct avail_regs) { { 0, 0, 0}, 0, 0, 0 }

/* Find suitable registers for a VFP Co-Processor Register Candidate (VFP CPRC
   param) according to the rules described in the procedure call standard for
   the ARM architecture (AAPCS). If found, the registers are assigned to this
   VFP CPRC parameter. Registers are allocated in sequence unless a hole exists
   and the parameter is a single float.

   avregs: opaque structure to keep track of available VFP co-processor regs
   align: alignment constraints for the param, as returned by type_size()
   size: size of the parameter, as returned by type_size() */
int assign_vfpreg(struct avail_regs *avregs, int align, int size)
{
  int first_reg = 0;

  if (avregs->first_free_reg == -1)
    return -1;
  if (align >> 3) { /* double alignment */
    first_reg = avregs->first_free_reg;
    /* alignment constraint not respected so use next reg and record hole */
    if (first_reg & 1)
      avregs->avail[avregs->last_hole++] = first_reg++;
  } else { /* no special alignment (float or array of float) */
    /* if single float and a hole is available, assign the param to it */
    if (size == 4 && avregs->first_hole != avregs->last_hole)
      return avregs->avail[avregs->first_hole++];
    else
      first_reg = avregs->first_free_reg;
  }
  if (first_reg + size / 4 <= 16) {
    avregs->first_free_reg = first_reg + size / 4;
    return first_reg;
  }
  avregs->first_free_reg = -1;
  return -1;
}

/* Returns whether all params need to be passed in core registers or not.
   This is the case for function part of the runtime ABI. */
int floats_in_core_regs(SValue *sval)
{
  if (!sval->sym)
    return 0;

  switch (sval->sym->v) {
    case TOK___floatundisf:
    case TOK___floatundidf:
    case TOK___fixunssfdi:
    case TOK___fixunsdfdi:
#ifndef TCC_ARM_VFP
    case TOK___fixunsxfdi:
#endif
    case TOK___floatdisf:
    case TOK___floatdidf:
    case TOK___fixsfdi:
    case TOK___fixdfdi:
      return 1;

    default:
      return 0;
  }
}

/* Return the number of registers needed to return the struct, or 0 if
   returning via struct pointer. */
ST_FUNC int gfunc_sret(CType *vt, int variadic, CType *ret, int *ret_align, int *regsize) {
#ifdef TCC_ARM_EABI
    int size, align;
    size = type_size(vt, &align);
    if (float_abi == ARM_HARD_FLOAT && !variadic &&
        (is_float(vt->t) || is_hgen_float_aggr(vt))) {
        *ret_align = 8;
	*regsize = 8;
        ret->ref = NULL;
        ret->t = VT_DOUBLE;
        return (size + 7) >> 3;
    } else if (size <= 4) {
        *ret_align = 4;
	*regsize = 4;
        ret->ref = NULL;
        ret->t = VT_INT;
        return 1;
    } else
        return 0;
#else
    return 0;
#endif
}

/* Parameters are classified according to how they are copied to their final
   destination for the function call. Because the copying is performed class
   after class according to the order in the union below, it is important that
   some constraints about the order of the members of this union are respected:
   - CORE_STRUCT_CLASS must come after STACK_CLASS;
   - CORE_CLASS must come after STACK_CLASS, CORE_STRUCT_CLASS and
     VFP_STRUCT_CLASS;
   - VFP_STRUCT_CLASS must come after VFP_CLASS.
   See the comment for the main loop in copy_params() for the reason. */
enum reg_class {
	STACK_CLASS = 0,
	CORE_STRUCT_CLASS,
	VFP_CLASS,
	VFP_STRUCT_CLASS,
	CORE_CLASS,
	NB_CLASSES
};

struct param_plan {
    int start; /* first reg or addr used depending on the class */
    int end; /* last reg used or next free addr depending on the class */
    SValue *sval; /* pointer to SValue on the value stack */
    struct param_plan *prev; /*  previous element in this class */
};

struct plan {
    struct param_plan *pplans; /* array of all the param plans */
    struct param_plan *clsplans[NB_CLASSES]; /* per class lists of param plans */
};

#define add_param_plan(plan,pplan,class)                        \
    do {                                                        \
        pplan.prev = plan->clsplans[class];                     \
        plan->pplans[plan ## _nb] = pplan;                      \
        plan->clsplans[class] = &plan->pplans[plan ## _nb++];   \
    } while(0)

/* Assign parameters to registers and stack with alignment according to the
   rules in the procedure call standard for the ARM architecture (AAPCS).
   The overall assignment is recorded in an array of per parameter structures
   called parameter plans. The parameter plans are also further organized in a
   number of linked lists, one per class of parameter (see the comment for the
   definition of union reg_class).

   nb_args: number of parameters of the function for which a call is generated
   float_abi: float ABI in use for this function call
   plan: the structure where the overall assignment is recorded
   todo: a bitmap that record which core registers hold a parameter

   Returns the amount of stack space needed for parameter passing

   Note: this function allocated an array in plan->pplans with tcc_malloc. It
   is the responsibility of the caller to free this array once used (ie not
   before copy_params). */
static int assign_regs(int nb_args, int float_abi, struct plan *plan, int *todo)
{
  int i, size, align;
  int ncrn /* next core register number */, nsaa /* next stacked argument address*/;
  int plan_nb = 0;
  struct param_plan pplan;
  struct avail_regs avregs = AVAIL_REGS_INITIALIZER;

  ncrn = nsaa = 0;
  *todo = 0;
  plan->pplans = tcc_malloc(nb_args * sizeof(*plan->pplans));
  memset(plan->clsplans, 0, sizeof(plan->clsplans));
  for(i = nb_args; i-- ;) {
    int j, start_vfpreg = 0;
    CType type = vtop[-i].type;
    type.t &= ~VT_ARRAY;
    size = type_size(&type, &align);
    size = (size + 3) & ~3;
    align = (align + 3) & ~3;
    switch(vtop[-i].type.t & VT_BTYPE) {
      case VT_STRUCT:
      case VT_FLOAT:
      case VT_DOUBLE:
      case VT_LDOUBLE:
      if (float_abi == ARM_HARD_FLOAT) {
        int is_hfa = 0; /* Homogeneous float aggregate */

        if (is_float(vtop[-i].type.t)
            || (is_hfa = is_hgen_float_aggr(&vtop[-i].type))) {
          int end_vfpreg;

          start_vfpreg = assign_vfpreg(&avregs, align, size);
          end_vfpreg = start_vfpreg + ((size - 1) >> 2);
          if (start_vfpreg >= 0) {
            pplan = (struct param_plan) {start_vfpreg, end_vfpreg, &vtop[-i]};
            if (is_hfa)
              add_param_plan(plan, pplan, VFP_STRUCT_CLASS);
            else
              add_param_plan(plan, pplan, VFP_CLASS);
            continue;
          } else
            break;
        }
      }
      ncrn = (ncrn + (align-1)/4) & ~((align/4) - 1);
      if (ncrn + size/4 <= 4 || (ncrn < 4 && start_vfpreg != -1)) {
        /* The parameter is allocated both in core register and on stack. As
	 * such, it can be of either class: it would either be the last of
	 * CORE_STRUCT_CLASS or the first of STACK_CLASS. */
        for (j = ncrn; j < 4 && j < ncrn + size / 4; j++)
          *todo|=(1<<j);
        pplan = (struct param_plan) {ncrn, j, &vtop[-i]};
        add_param_plan(plan, pplan, CORE_STRUCT_CLASS);
        ncrn += size/4;
        if (ncrn > 4)
          nsaa = (ncrn - 4) * 4;
      } else {
        ncrn = 4;
        break;
      }
      continue;
      default:
      if (ncrn < 4) {
        int is_long = (vtop[-i].type.t & VT_BTYPE) == VT_LLONG;

        if (is_long) {
          ncrn = (ncrn + 1) & -2;
          if (ncrn == 4)
            break;
        }
        pplan = (struct param_plan) {ncrn, ncrn, &vtop[-i]};
        ncrn++;
        if (is_long)
          pplan.end = ncrn++;
        add_param_plan(plan, pplan, CORE_CLASS);
        continue;
      }
    }
    nsaa = (nsaa + (align - 1)) & ~(align - 1);
    pplan = (struct param_plan) {nsaa, nsaa + size, &vtop[-i]};
    add_param_plan(plan, pplan, STACK_CLASS);
    nsaa += size; /* size already rounded up before */
  }
  return nsaa;
}

#undef add_param_plan

/* Copy parameters to their final destination (core reg, VFP reg or stack) for
   function call.

   nb_args: number of parameters the function take
   plan: the overall assignment plan for parameters
   todo: a bitmap indicating what core reg will hold a parameter

   Returns the number of SValue added by this function on the value stack */
static int copy_params(int nb_args, struct plan *plan, int todo)
{
  int size, align, r, i, nb_extra_sval = 0;
  struct param_plan *pplan;
  int pass = 0;

   /* Several constraints require parameters to be copied in a specific order:
      - structures are copied to the stack before being loaded in a reg;
      - floats loaded to an odd numbered VFP reg are first copied to the
        preceding even numbered VFP reg and then moved to the next VFP reg.

      It is thus important that:
      - structures assigned to core regs must be copied after parameters
        assigned to the stack but before structures assigned to VFP regs because
        a structure can lie partly in core registers and partly on the stack;
      - parameters assigned to the stack and all structures be copied before
        parameters assigned to a core reg since copying a parameter to the stack
        require using a core reg;
      - parameters assigned to VFP regs be copied before structures assigned to
        VFP regs as the copy might use an even numbered VFP reg that already
        holds part of a structure. */
again:
  for(i = 0; i < NB_CLASSES; i++) {
    for(pplan = plan->clsplans[i]; pplan; pplan = pplan->prev) {

      if (pass
          && (i != CORE_CLASS || pplan->sval->r < VT_CONST))
        continue;

      vpushv(pplan->sval);
      pplan->sval->r = pplan->sval->r2 = VT_CONST; /* disable entry */
      switch(i) {
        case STACK_CLASS:
        case CORE_STRUCT_CLASS:
        case VFP_STRUCT_CLASS:
          if ((pplan->sval->type.t & VT_BTYPE) == VT_STRUCT) {
            int padding = 0;
            size = type_size(&pplan->sval->type, &align);
            /* align to stack align size */
            size = (size + 3) & ~3;
            if (i == STACK_CLASS && pplan->prev)
              padding = pplan->start - pplan->prev->end;
            size += padding; /* Add padding if any */
            /* allocate the necessary size on stack */
            gadd_sp(-size);
            /* generate structure store */
            r = get_reg(RC_INT);
            o(0xE28D0000|(intr(r)<<12)|padding); /* add r, sp, padding */
            vset(&vtop->type, r | VT_LVAL, 0);
            vswap();
            vstore(); /* memcpy to current sp + potential padding */

            /* Homogeneous float aggregate are loaded to VFP registers
               immediately since there is no way of loading data in multiple
               non consecutive VFP registers as what is done for other
               structures (see the use of todo). */
            if (i == VFP_STRUCT_CLASS) {
              int first = pplan->start, nb = pplan->end - first + 1;
              /* vpop.32 {pplan->start, ..., pplan->end} */
              o(0xECBD0A00|(first&1)<<22|(first>>1)<<12|nb);
              /* No need to write the register used to a SValue since VFP regs
                 cannot be used for gcall_or_jmp */
            }
          } else {
            if (is_float(pplan->sval->type.t)) {
#ifdef TCC_ARM_VFP
              r = vfpr(gv(RC_FLOAT)) << 12;
              if ((pplan->sval->type.t & VT_BTYPE) == VT_FLOAT)
                size = 4;
              else {
                size = 8;
                r |= 0x101; /* vpush.32 -> vpush.64 */
              }
              o(0xED2D0A01 + r); /* vpush */
#else
              r = fpr(gv(RC_FLOAT)) << 12;
              if ((pplan->sval->type.t & VT_BTYPE) == VT_FLOAT)
                size = 4;
              else if ((pplan->sval->type.t & VT_BTYPE) == VT_DOUBLE)
                size = 8;
              else
                size = LDOUBLE_SIZE;

              if (size == 12)
                r |= 0x400000;
              else if(size == 8)
                r|=0x8000;

              o(0xED2D0100|r|(size>>2)); /* some kind of vpush for FPA */
#endif
            } else {
              /* simple type (currently always same size) */
              /* XXX: implicit cast ? */
              size=4;
              if ((pplan->sval->type.t & VT_BTYPE) == VT_LLONG) {
                lexpand_nr();
                size = 8;
                r = gv(RC_INT);
                o(0xE52D0004|(intr(r)<<12)); /* push r */
                vtop--;
              }
              r = gv(RC_INT);
              o(0xE52D0004|(intr(r)<<12)); /* push r */
            }
            if (i == STACK_CLASS && pplan->prev)
              gadd_sp(pplan->prev->end - pplan->start); /* Add padding if any */
          }
          break;

        case VFP_CLASS:
          gv(regmask(TREG_F0 + (pplan->start >> 1)));
          if (pplan->start & 1) { /* Must be in upper part of double register */
            o(0xEEF00A40|((pplan->start>>1)<<12)|(pplan->start>>1)); /* vmov.f32 s(n+1), sn */
            vtop->r = VT_CONST; /* avoid being saved on stack by gv for next float */
          }
          break;

        case CORE_CLASS:
          if ((pplan->sval->type.t & VT_BTYPE) == VT_LLONG) {
            lexpand_nr();
            gv(regmask(pplan->end));
            pplan->sval->r2 = vtop->r;
            vtop--;
          }
          gv(regmask(pplan->start));
          /* Mark register as used so that gcall_or_jmp use another one
             (regs >=4 are free as never used to pass parameters) */
          pplan->sval->r = vtop->r;
          break;
      }
      vtop--;
    }
  }

  /* second pass to restore registers that were saved on stack by accident.
     Maybe redundant after the "lvalue_save" patch in tccgen.c:gv() */
  if (++pass < 2)
    goto again;

  /* Manually free remaining registers since next parameters are loaded
   * manually, without the help of gv(int). */
  save_regs(nb_args);

  if(todo) {
    o(0xE8BD0000|todo); /* pop {todo} */
    for(pplan = plan->clsplans[CORE_STRUCT_CLASS]; pplan; pplan = pplan->prev) {
      int r;
      pplan->sval->r = pplan->start;
      /* An SValue can only pin 2 registers at best (r and r2) but a structure
         can occupy more than 2 registers. Thus, we need to push on the value
         stack some fake parameter to have on SValue for each registers used
         by a structure (r2 is not used). */
      for (r = pplan->start + 1; r <= pplan->end; r++) {
        if (todo & (1 << r)) {
          nb_extra_sval++;
          vpushi(0);
          vtop->r = r;
        }
      }
    }
  }
  return nb_extra_sval;
}

/* Generate function call. The function address is pushed first, then
   all the parameters in call order. This functions pops all the
   parameters and the function address. */
void gfunc_call(int nb_args)
{
  int r, args_size;
  int def_float_abi = float_abi;
  int todo;
  struct plan plan;

#ifdef TCC_ARM_EABI
  int variadic;

  if (float_abi == ARM_HARD_FLOAT) {
    variadic = (vtop[-nb_args].type.ref->f.func_type == FUNC_ELLIPSIS);
    if (variadic || floats_in_core_regs(&vtop[-nb_args]))
      float_abi = ARM_SOFTFP_FLOAT;
  }
#endif
  /* cannot let cpu flags if other instruction are generated. Also avoid leaving
     VT_JMP anywhere except on the top of the stack because it would complicate
     the code generator. */
  r = vtop->r & VT_VALMASK;
  if (r == VT_CMP || (r & ~1) == VT_JMP)
    gv(RC_INT);

  args_size = assign_regs(nb_args, float_abi, &plan, &todo);

#ifdef TCC_ARM_EABI
  if (args_size & 7) { /* Stack must be 8 byte aligned at fct call for EABI */
    args_size = (args_size + 7) & ~7;
    o(0xE24DD004); /* sub sp, sp, #4 */
  }
#endif

  nb_args += copy_params(nb_args, &plan, todo);
  tcc_free(plan.pplans);

  /* Move fct SValue on top as required by gcall_or_jmp */
  vrotb(nb_args + 1);
  gcall_or_jmp(0);
  if (args_size)
      gadd_sp(args_size); /* pop all parameters passed on the stack */
#if defined(TCC_ARM_EABI) && defined(TCC_ARM_VFP)
  if(float_abi == ARM_SOFTFP_FLOAT && is_float(vtop->type.ref->type.t)) {
    if((vtop->type.ref->type.t & VT_BTYPE) == VT_FLOAT) {
      o(0xEE000A10); /*vmov s0, r0 */
    } else {
      o(0xEE000B10); /* vmov.32 d0[0], r0 */
      o(0xEE201B10); /* vmov.32 d0[1], r1 */
    }
  }
#endif
  vtop -= nb_args + 1; /* Pop all params and fct address from value stack */
  leaffunc = 0; /* we are calling a function, so we aren't in a leaf function */
  float_abi = def_float_abi;
}

/* generate function prolog of type 't' */
void gfunc_prolog(CType *func_type)
{
  Sym *sym,*sym2;
  int n, nf, size, align, rs, struct_ret = 0;
  int addr, pn, sn; /* pn=core, sn=stack */
  CType ret_type;

#ifdef TCC_ARM_EABI
  struct avail_regs avregs = AVAIL_REGS_INITIALIZER;
#endif

  sym = func_type->ref;
  func_vt = sym->type;
  func_var = (func_type->ref->f.func_type == FUNC_ELLIPSIS);

  n = nf = 0;
  if ((func_vt.t & VT_BTYPE) == VT_STRUCT &&
      !gfunc_sret(&func_vt, func_var, &ret_type, &align, &rs))
  {
    n++;
    struct_ret = 1;
    func_vc = 12; /* Offset from fp of the place to store the result */
  }
  for(sym2 = sym->next; sym2 && (n < 4 || nf < 16); sym2 = sym2->next) {
    size = type_size(&sym2->type, &align);
#ifdef TCC_ARM_EABI
    if (float_abi == ARM_HARD_FLOAT && !func_var &&
        (is_float(sym2->type.t) || is_hgen_float_aggr(&sym2->type))) {
      int tmpnf = assign_vfpreg(&avregs, align, size);
      tmpnf += (size + 3) / 4;
      nf = (tmpnf > nf) ? tmpnf : nf;
    } else
#endif
    if (n < 4)
      n += (size + 3) / 4;
  }
  o(0xE1A0C00D); /* mov ip,sp */
  if (func_var)
    n=4;
  if (n) {
    if(n>4)
      n=4;
#ifdef TCC_ARM_EABI
    n=(n+1)&-2;
#endif
    o(0xE92D0000|((1<<n)-1)); /* save r0-r4 on stack if needed */
  }
  if (nf) {
    if (nf>16)
      nf=16;
    nf=(nf+1)&-2; /* nf => HARDFLOAT => EABI */
    o(0xED2D0A00|nf); /* save s0-s15 on stack if needed */
  }
  o(0xE92D5800); /* save fp, ip, lr */
  o(0xE1A0B00D); /* mov fp, sp */
  func_sub_sp_offset = ind;
  o(0xE1A00000); /* nop, leave space for stack adjustment in epilog */

#ifdef TCC_ARM_EABI
  if (float_abi == ARM_HARD_FLOAT) {
    func_vc += nf * 4;
    avregs = AVAIL_REGS_INITIALIZER;
  }
#endif
  pn = struct_ret, sn = 0;
  while ((sym = sym->next)) {
    CType *type;
    type = &sym->type;
    size = type_size(type, &align);
    size = (size + 3) >> 2;
    align = (align + 3) & ~3;
#ifdef TCC_ARM_EABI
    if (float_abi == ARM_HARD_FLOAT && !func_var && (is_float(sym->type.t)
        || is_hgen_float_aggr(&sym->type))) {
      int fpn = assign_vfpreg(&avregs, align, size << 2);
      if (fpn >= 0)
        addr = fpn * 4;
      else
        goto from_stack;
    } else
#endif
    if (pn < 4) {
#ifdef TCC_ARM_EABI
        pn = (pn + (align-1)/4) & -(align/4);
#endif
      addr = (nf + pn) * 4;
      pn += size;
      if (!sn && pn > 4)
        sn = (pn - 4);
    } else {
#ifdef TCC_ARM_EABI
from_stack:
        sn = (sn + (align-1)/4) & -(align/4);
#endif
      addr = (n + nf + sn) * 4;
      sn += size;
    }
    sym_push(sym->v & ~SYM_FIELD, type, VT_LOCAL | lvalue_type(type->t),
             addr + 12);
  }
  last_itod_magic=0;
  leaffunc = 1;
  loc = 0;
}

/* generate function epilog */
void gfunc_epilog(void)
{
  uint32_t x;
  int diff;
  /* Copy float return value to core register if base standard is used and
     float computation is made with VFP */
#if defined(TCC_ARM_EABI) && defined(TCC_ARM_VFP)
  if ((float_abi == ARM_SOFTFP_FLOAT || func_var) && is_float(func_vt.t)) {
    if((func_vt.t & VT_BTYPE) == VT_FLOAT)
      o(0xEE100A10); /* fmrs r0, s0 */
    else {
      o(0xEE100B10); /* fmrdl r0, d0 */
      o(0xEE301B10); /* fmrdh r1, d0 */
    }
  }
#endif
  o(0xE89BA800); /* restore fp, sp, pc */
  diff = (-loc + 3) & -4;
#ifdef TCC_ARM_EABI
  if(!leaffunc)
    diff = ((diff + 11) & -8) - 4;
#endif
  if(diff > 0) {
    x=stuff_const(0xE24BD000, diff); /* sub sp,fp,# */
    if(x)
      *(uint32_t *)(cur_text_section->data + func_sub_sp_offset) = x;
    else {
      int addr;
      addr=ind;
      o(0xE59FC004); /* ldr ip,[pc+4] */
      o(0xE04BD00C); /* sub sp,fp,ip  */
      o(0xE1A0F00E); /* mov pc,lr */
      o(diff);
      *(uint32_t *)(cur_text_section->data + func_sub_sp_offset) = 0xE1000000|encbranch(func_sub_sp_offset,addr,1);
    }
  }
}

/* generate a jump to a label */
int gjmp(int t)
{
  int r;
  if (nocode_wanted)
    return t;
  r=ind;
  o(0xE0000000|encbranch(r,t,1));
  return r;
}

/* generate a jump to a fixed address */
void gjmp_addr(int a)
{
  gjmp(a);
}

/* generate a test. set 'inv' to invert test. Stack entry is popped */
int gtst(int inv, int t)
{
  int v, r;
  uint32_t op;

  v = vtop->r & VT_VALMASK;
  r=ind;

  if (nocode_wanted) {
    ;
  } else if (v == VT_CMP) {
    op=mapcc(inv?negcc(vtop->c.i):vtop->c.i);
    op|=encbranch(r,t,1);
    o(op);
    t=r;
  } else if (v == VT_JMP || v == VT_JMPI) {
    if ((v & 1) == inv) {
      if(!vtop->c.i)
	vtop->c.i=t;
      else {
	uint32_t *x;
	int p,lp;
	if(t) {
          p = vtop->c.i;
          do {
	    p = decbranch(lp=p);
          } while(p);
	  x = (uint32_t *)(cur_text_section->data + lp);
	  *x &= 0xff000000;
	  *x |= encbranch(lp,t,1);
	}
	t = vtop->c.i;
      }
    } else {
      t = gjmp(t);
      gsym(vtop->c.i);
    }
  }
  vtop--;
  return t;
}

/* generate an integer binary operation */
void gen_opi(int op)
{
  int c, func = 0;
  uint32_t opc = 0, r, fr;
  unsigned short retreg = REG_IRET;

  c=0;
  switch(op) {
    case '+':
      opc = 0x8;
      c=1;
      break;
    case TOK_ADDC1: /* add with carry generation */
      opc = 0x9;
      c=1;
      break;
    case '-':
      opc = 0x4;
      c=1;
      break;
    case TOK_SUBC1: /* sub with carry generation */
      opc = 0x5;
      c=1;
      break;
    case TOK_ADDC2: /* add with carry use */
      opc = 0xA;
      c=1;
      break;
    case TOK_SUBC2: /* sub with carry use */
      opc = 0xC;
      c=1;
      break;
    case '&':
      opc = 0x0;
      c=1;
      break;
    case '^':
      opc = 0x2;
      c=1;
      break;
    case '|':
      opc = 0x18;
      c=1;
      break;
    case '*':
      gv2(RC_INT, RC_INT);
      r = vtop[-1].r;
      fr = vtop[0].r;
      vtop--;
      o(0xE0000090|(intr(r)<<16)|(intr(r)<<8)|intr(fr));
      return;
    case TOK_SHL:
      opc = 0;
      c=2;
      break;
    case TOK_SHR:
      opc = 1;
      c=2;
      break;
    case TOK_SAR:
      opc = 2;
      c=2;
      break;
    case '/':
    case TOK_PDIV:
      func=TOK___divsi3;
      c=3;
      break;
    case TOK_UDIV:
      func=TOK___udivsi3;
      c=3;
      break;
    case '%':
#ifdef TCC_ARM_EABI
      func=TOK___aeabi_idivmod;
      retreg=REG_LRET;
#else
      func=TOK___modsi3;
#endif
      c=3;
      break;
    case TOK_UMOD:
#ifdef TCC_ARM_EABI
      func=TOK___aeabi_uidivmod;
      retreg=REG_LRET;
#else
      func=TOK___umodsi3;
#endif
      c=3;
      break;
    case TOK_UMULL:
      gv2(RC_INT, RC_INT);
      r=intr(vtop[-1].r2=get_reg(RC_INT));
      c=vtop[-1].r;
      vtop[-1].r=get_reg_ex(RC_INT,regmask(c));
      vtop--;
      o(0xE0800090|(r<<16)|(intr(vtop->r)<<12)|(intr(c)<<8)|intr(vtop[1].r));
      return;
    default:
      opc = 0x15;
      c=1;
      break;
  }
  switch(c) {
    case 1:
      if((vtop[-1].r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
	if(opc == 4 || opc == 5 || opc == 0xc) {
	  vswap();
	  opc|=2; // sub -> rsb
	}
      }
      if ((vtop->r & VT_VALMASK) == VT_CMP ||
          (vtop->r & (VT_VALMASK & ~1)) == VT_JMP)
        gv(RC_INT);
      vswap();
      c=intr(gv(RC_INT));
      vswap();
      opc=0xE0000000|(opc<<20)|(c<<16);
      if((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
	uint32_t x;
	x=stuff_const(opc|0x2000000,vtop->c.i);
	if(x) {
	  r=intr(vtop[-1].r=get_reg_ex(RC_INT,regmask(vtop[-1].r)));
	  o(x|(r<<12));
	  goto done;
	}
      }
      fr=intr(gv(RC_INT));
      r=intr(vtop[-1].r=get_reg_ex(RC_INT,two2mask(vtop->r,vtop[-1].r)));
      o(opc|(r<<12)|fr);
done:
      vtop--;
      if (op >= TOK_ULT && op <= TOK_GT) {
        vtop->r = VT_CMP;
        vtop->c.i = op;
      }
      break;
    case 2:
      opc=0xE1A00000|(opc<<5);
      if ((vtop->r & VT_VALMASK) == VT_CMP ||
          (vtop->r & (VT_VALMASK & ~1)) == VT_JMP)
        gv(RC_INT);
      vswap();
      r=intr(gv(RC_INT));
      vswap();
      opc|=r;
      if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
	fr=intr(vtop[-1].r=get_reg_ex(RC_INT,regmask(vtop[-1].r)));
	c = vtop->c.i & 0x1f;
	o(opc|(c<<7)|(fr<<12));
      } else {
        fr=intr(gv(RC_INT));
	c=intr(vtop[-1].r=get_reg_ex(RC_INT,two2mask(vtop->r,vtop[-1].r)));
	o(opc|(c<<12)|(fr<<8)|0x10);
      }
      vtop--;
      break;
    case 3:
      vpush_global_sym(&func_old_type, func);
      vrott(3);
      gfunc_call(2);
      vpushi(0);
      vtop->r = retreg;
      break;
    default:
      tcc_error("gen_opi %i unimplemented!",op);
  }
}

#ifdef TCC_ARM_VFP
static int is_zero(int i)
{
  if((vtop[i].r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
    return 0;
  if (vtop[i].type.t == VT_FLOAT)
    return (vtop[i].c.f == 0.f);
  else if (vtop[i].type.t == VT_DOUBLE)
    return (vtop[i].c.d == 0.0);
  return (vtop[i].c.ld == 0.l);
}

/* generate a floating point operation 'v = t1 op t2' instruction. The
 *    two operands are guaranteed to have the same floating point type */
void gen_opf(int op)
{
  uint32_t x;
  int fneg=0,r;
  x=0xEE000A00|T2CPR(vtop->type.t);
  switch(op) {
    case '+':
      if(is_zero(-1))
        vswap();
      if(is_zero(0)) {
        vtop--;
        return;
      }
      x|=0x300000;
      break;
    case '-':
      x|=0x300040;
      if(is_zero(0)) {
        vtop--;
        return;
      }
      if(is_zero(-1)) {
        x|=0x810000; /* fsubX -> fnegX */
        vswap();
        vtop--;
        fneg=1;
      }
      break;
    case '*':
      x|=0x200000;
      break;
    case '/':
      x|=0x800000;
      break;
    default:
      if(op < TOK_ULT || op > TOK_GT) {
        tcc_error("unknown fp op %x!",op);
        return;
      }
      if(is_zero(-1)) {
        vswap();
        switch(op) {
          case TOK_LT: op=TOK_GT; break;
          case TOK_GE: op=TOK_ULE; break;
          case TOK_LE: op=TOK_GE; break;
          case TOK_GT: op=TOK_ULT; break;
        }
      }
      x|=0xB40040; /* fcmpX */
      if(op!=TOK_EQ && op!=TOK_NE)
        x|=0x80; /* fcmpX -> fcmpeX */
      if(is_zero(0)) {
        vtop--;
        o(x|0x10000|(vfpr(gv(RC_FLOAT))<<12)); /* fcmp(e)X -> fcmp(e)zX */
      } else {
        x|=vfpr(gv(RC_FLOAT));
        vswap();
        o(x|(vfpr(gv(RC_FLOAT))<<12));
        vtop--;
      }
      o(0xEEF1FA10); /* fmstat */

      switch(op) {
        case TOK_LE: op=TOK_ULE; break;
        case TOK_LT: op=TOK_ULT; break;
        case TOK_UGE: op=TOK_GE; break;
        case TOK_UGT: op=TOK_GT; break;
      }

      vtop->r = VT_CMP;
      vtop->c.i = op;
      return;
  }
  r=gv(RC_FLOAT);
  x|=vfpr(r);
  r=regmask(r);
  if(!fneg) {
    int r2;
    vswap();
    r2=gv(RC_FLOAT);
    x|=vfpr(r2)<<16;
    r|=regmask(r2);
  }
  vtop->r=get_reg_ex(RC_FLOAT,r);
  if(!fneg)
    vtop--;
  o(x|(vfpr(vtop->r)<<12));
}

#else
static uint32_t is_fconst()
{
  long double f;
  uint32_t r;
  if((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
    return 0;
  if (vtop->type.t == VT_FLOAT)
    f = vtop->c.f;
  else if (vtop->type.t == VT_DOUBLE)
    f = vtop->c.d;
  else
    f = vtop->c.ld;
  if(!ieee_finite(f))
    return 0;
  r=0x8;
  if(f<0.0) {
    r=0x18;
    f=-f;
  }
  if(f==0.0)
    return r;
  if(f==1.0)
    return r|1;
  if(f==2.0)
    return r|2;
  if(f==3.0)
    return r|3;
  if(f==4.0)
    return r|4;
  if(f==5.0)
    return r|5;
  if(f==0.5)
    return r|6;
  if(f==10.0)
    return r|7;
  return 0;
}

/* generate a floating point operation 'v = t1 op t2' instruction. The
   two operands are guaranteed to have the same floating point type */
void gen_opf(int op)
{
  uint32_t x, r, r2, c1, c2;
  //fputs("gen_opf\n",stderr);
  vswap();
  c1 = is_fconst();
  vswap();
  c2 = is_fconst();
  x=0xEE000100;
#if LDOUBLE_SIZE == 8
  if ((vtop->type.t & VT_BTYPE) != VT_FLOAT)
    x|=0x80;
#else
  if ((vtop->type.t & VT_BTYPE) == VT_DOUBLE)
    x|=0x80;
  else if ((vtop->type.t & VT_BTYPE) == VT_LDOUBLE)
    x|=0x80000;
#endif
  switch(op)
  {
    case '+':
      if(!c2) {
	vswap();
	c2=c1;
      }
      vswap();
      r=fpr(gv(RC_FLOAT));
      vswap();
      if(c2) {
	if(c2>0xf)
	  x|=0x200000; // suf
	r2=c2&0xf;
      } else {
	r2=fpr(gv(RC_FLOAT));
      }
      break;
    case '-':
      if(c2) {
	if(c2<=0xf)
	  x|=0x200000; // suf
	r2=c2&0xf;
	vswap();
	r=fpr(gv(RC_FLOAT));
	vswap();
      } else if(c1 && c1<=0xf) {
	x|=0x300000; // rsf
	r2=c1;
	r=fpr(gv(RC_FLOAT));
	vswap();
      } else {
	x|=0x200000; // suf
	vswap();
	r=fpr(gv(RC_FLOAT));
	vswap();
	r2=fpr(gv(RC_FLOAT));
      }
      break;
    case '*':
      if(!c2 || c2>0xf) {
	vswap();
	c2=c1;
      }
      vswap();
      r=fpr(gv(RC_FLOAT));
      vswap();
      if(c2 && c2<=0xf)
	r2=c2;
      else
	r2=fpr(gv(RC_FLOAT));
      x|=0x100000; // muf
      break;
    case '/':
      if(c2 && c2<=0xf) {
	x|=0x400000; // dvf
	r2=c2;
	vswap();
	r=fpr(gv(RC_FLOAT));
	vswap();
      } else if(c1 && c1<=0xf) {
	x|=0x500000; // rdf
	r2=c1;
	r=fpr(gv(RC_FLOAT));
	vswap();
      } else {
	x|=0x400000; // dvf
	vswap();
	r=fpr(gv(RC_FLOAT));
	vswap();
	r2=fpr(gv(RC_FLOAT));
      }
      break;
    default:
      if(op >= TOK_ULT && op <= TOK_GT) {
	x|=0xd0f110; // cmfe
/* bug (intention?) in Linux FPU emulator
   doesn't set carry if equal */
	switch(op) {
	  case TOK_ULT:
	  case TOK_UGE:
	  case TOK_ULE:
	  case TOK_UGT:
            tcc_error("unsigned comparison on floats?");
	    break;
	  case TOK_LT:
            op=TOK_Nset;
	    break;
	  case TOK_LE:
            op=TOK_ULE; /* correct in unordered case only if AC bit in FPSR set */
	    break;
	  case TOK_EQ:
	  case TOK_NE:
	    x&=~0x400000; // cmfe -> cmf
	    break;
	}
	if(c1 && !c2) {
	  c2=c1;
	  vswap();
	  switch(op) {
            case TOK_Nset:
              op=TOK_GT;
	      break;
            case TOK_GE:
	      op=TOK_ULE;
	      break;
	    case TOK_ULE:
              op=TOK_GE;
	      break;
            case TOK_GT:
              op=TOK_Nset;
	      break;
	  }
	}
	vswap();
	r=fpr(gv(RC_FLOAT));
	vswap();
	if(c2) {
	  if(c2>0xf)
	    x|=0x200000;
	  r2=c2&0xf;
	} else {
	  r2=fpr(gv(RC_FLOAT));
	}
	vtop[-1].r = VT_CMP;
	vtop[-1].c.i = op;
      } else {
        tcc_error("unknown fp op %x!",op);
	return;
      }
  }
  if(vtop[-1].r == VT_CMP)
    c1=15;
  else {
    c1=vtop->r;
    if(r2&0x8)
      c1=vtop[-1].r;
    vtop[-1].r=get_reg_ex(RC_FLOAT,two2mask(vtop[-1].r,c1));
    c1=fpr(vtop[-1].r);
  }
  vtop--;
  o(x|(r<<16)|(c1<<12)|r2);
}
#endif

/* convert integers to fp 't' type. Must handle 'int', 'unsigned int'
   and 'long long' cases. */
ST_FUNC void gen_cvt_itof1(int t)
{
  uint32_t r, r2;
  int bt;
  bt=vtop->type.t & VT_BTYPE;
  if(bt == VT_INT || bt == VT_SHORT || bt == VT_BYTE) {
#ifndef TCC_ARM_VFP
    uint32_t dsize = 0;
#endif
    r=intr(gv(RC_INT));
#ifdef TCC_ARM_VFP
    r2=vfpr(vtop->r=get_reg(RC_FLOAT));
    o(0xEE000A10|(r<<12)|(r2<<16)); /* fmsr */
    r2|=r2<<12;
    if(!(vtop->type.t & VT_UNSIGNED))
      r2|=0x80;                /* fuitoX -> fsituX */
    o(0xEEB80A40|r2|T2CPR(t)); /* fYitoX*/
#else
    r2=fpr(vtop->r=get_reg(RC_FLOAT));
    if((t & VT_BTYPE) != VT_FLOAT)
      dsize=0x80;    /* flts -> fltd */
    o(0xEE000110|dsize|(r2<<16)|(r<<12)); /* flts */
    if((vtop->type.t & (VT_UNSIGNED|VT_BTYPE)) == (VT_UNSIGNED|VT_INT)) {
      uint32_t off = 0;
      o(0xE3500000|(r<<12));        /* cmp */
      r=fpr(get_reg(RC_FLOAT));
      if(last_itod_magic) {
	off=ind+8-last_itod_magic;
	off/=4;
	if(off>255)
	  off=0;
      }
      o(0xBD1F0100|(r<<12)|off);    /* ldflts */
      if(!off) {
        o(0xEA000000);              /* b */
        last_itod_magic=ind;
        o(0x4F800000);              /* 4294967296.0f */
      }
      o(0xBE000100|dsize|(r2<<16)|(r2<<12)|r); /* adflt */
    }
#endif
    return;
  } else if(bt == VT_LLONG) {
    int func;
    CType *func_type = 0;
    if((t & VT_BTYPE) == VT_FLOAT) {
      func_type = &func_float_type;
      if(vtop->type.t & VT_UNSIGNED)
        func=TOK___floatundisf;
      else
        func=TOK___floatdisf;
#if LDOUBLE_SIZE != 8
    } else if((t & VT_BTYPE) == VT_LDOUBLE) {
      func_type = &func_ldouble_type;
      if(vtop->type.t & VT_UNSIGNED)
        func=TOK___floatundixf;
      else
        func=TOK___floatdixf;
    } else if((t & VT_BTYPE) == VT_DOUBLE) {
#else
    } else if((t & VT_BTYPE) == VT_DOUBLE || (t & VT_BTYPE) == VT_LDOUBLE) {
#endif
      func_type = &func_double_type;
      if(vtop->type.t & VT_UNSIGNED)
        func=TOK___floatundidf;
      else
        func=TOK___floatdidf;
    }
    if(func_type) {
      vpush_global_sym(func_type, func);
      vswap();
      gfunc_call(1);
      vpushi(0);
      vtop->r=TREG_F0;
      return;
    }
  }
  tcc_error("unimplemented gen_cvt_itof %x!",vtop->type.t);
}

/* convert fp to int 't' type */
void gen_cvt_ftoi(int t)
{
  uint32_t r, r2;
  int u, func = 0;
  u=t&VT_UNSIGNED;
  t&=VT_BTYPE;
  r2=vtop->type.t & VT_BTYPE;
  if(t==VT_INT) {
#ifdef TCC_ARM_VFP
    r=vfpr(gv(RC_FLOAT));
    u=u?0:0x10000;
    o(0xEEBC0AC0|(r<<12)|r|T2CPR(r2)|u); /* ftoXizY */
    r2=intr(vtop->r=get_reg(RC_INT));
    o(0xEE100A10|(r<<16)|(r2<<12));
    return;
#else
    if(u) {
      if(r2 == VT_FLOAT)
        func=TOK___fixunssfsi;
#if LDOUBLE_SIZE != 8
      else if(r2 == VT_LDOUBLE)
	func=TOK___fixunsxfsi;
      else if(r2 == VT_DOUBLE)
#else
      else if(r2 == VT_LDOUBLE || r2 == VT_DOUBLE)
#endif
	func=TOK___fixunsdfsi;
    } else {
      r=fpr(gv(RC_FLOAT));
      r2=intr(vtop->r=get_reg(RC_INT));
      o(0xEE100170|(r2<<12)|r);
      return;
    }
#endif
  } else if(t == VT_LLONG) { // unsigned handled in gen_cvt_ftoi1
    if(r2 == VT_FLOAT)
      func=TOK___fixsfdi;
#if LDOUBLE_SIZE != 8
    else if(r2 == VT_LDOUBLE)
      func=TOK___fixxfdi;
    else if(r2 == VT_DOUBLE)
#else
    else if(r2 == VT_LDOUBLE || r2 == VT_DOUBLE)
#endif
      func=TOK___fixdfdi;
  }
  if(func) {
    vpush_global_sym(&func_old_type, func);
    vswap();
    gfunc_call(1);
    vpushi(0);
    if(t == VT_LLONG)
      vtop->r2 = REG_LRET;
    vtop->r = REG_IRET;
    return;
  }
  tcc_error("unimplemented gen_cvt_ftoi!");
}

/* convert from one floating point type to another */
void gen_cvt_ftof(int t)
{
#ifdef TCC_ARM_VFP
  if(((vtop->type.t & VT_BTYPE) == VT_FLOAT) != ((t & VT_BTYPE) == VT_FLOAT)) {
    uint32_t r = vfpr(gv(RC_FLOAT));
    o(0xEEB70AC0|(r<<12)|r|T2CPR(vtop->type.t));
  }
#else
  /* all we have to do on i386 and FPA ARM is to put the float in a register */
  gv(RC_FLOAT);
#endif
}

/* computed goto support */
void ggoto(void)
{
  gcall_or_jmp(1);
  vtop--;
}

/* Save the stack pointer onto the stack and return the location of its address */
ST_FUNC void gen_vla_sp_save(int addr) {
    SValue v;
    v.type.t = VT_PTR;
    v.r = VT_LOCAL | VT_LVAL;
    v.c.i = addr;
    store(TREG_SP, &v);
}

/* Restore the SP from a location on the stack */
ST_FUNC void gen_vla_sp_restore(int addr) {
    SValue v;
    v.type.t = VT_PTR;
    v.r = VT_LOCAL | VT_LVAL;
    v.c.i = addr;
    load(TREG_SP, &v);
}

/* Subtract from the stack pointer, and push the resulting value onto the stack */
ST_FUNC void gen_vla_alloc(CType *type, int align) {
    int r = intr(gv(RC_INT));
    o(0xE04D0000|(r<<12)|r); /* sub r, sp, r */
#ifdef TCC_ARM_EABI
    if (align < 8)
        align = 8;
#else
    if (align < 4)
        align = 4;
#endif
    if (align & (align - 1))
        tcc_error("alignment is not a power of 2: %i", align);
    o(stuff_const(0xE3C0D000|(r<<16), align - 1)); /* bic sp, r, #align-1 */
    vpop();
}

/* end of ARM code generator */
/*************************************************************/
#endif
/*************************************************************/
