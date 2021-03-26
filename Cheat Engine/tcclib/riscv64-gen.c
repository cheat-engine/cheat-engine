#ifdef TARGET_DEFS_ONLY

// Number of registers available to allocator:
#define NB_REGS 19 // x10-x17 aka a0-a7, f10-f17 aka fa0-fa7, xxx, ra, sp
#define NB_ASM_REGS 32
#define CONFIG_TCC_ASM

#define TREG_R(x) (x) // x = 0..7
#define TREG_F(x) (x + 8) // x = 0..7

// Register classes sorted from more general to more precise:
#define RC_INT (1 << 0)
#define RC_FLOAT (1 << 1)
#define RC_R(x) (1 << (2 + (x))) // x = 0..7
#define RC_F(x) (1 << (10 + (x))) // x = 0..7

#define RC_IRET (RC_R(0)) // int return register class
#define RC_IRE2 (RC_R(1)) // int 2nd return register class
#define RC_FRET (RC_F(0)) // float return register class

#define REG_IRET (TREG_R(0)) // int return register number
#define REG_IRE2 (TREG_R(1)) // int 2nd return register number
#define REG_FRET (TREG_F(0)) // float return register number

#define PTR_SIZE 8

#define LDOUBLE_SIZE 16
#define LDOUBLE_ALIGN 16

#define MAX_ALIGN 16

#define CHAR_IS_UNSIGNED

#else
#define USING_GLOBALS
#include "tcc.h"
#include <assert.h>

ST_DATA const char *target_machine_defs =
    "__riscv\0"
    "__riscv_xlen 64\0"
    "__riscv_flen 64\0"
    "__riscv_div\0"
    "__riscv_mul\0"
    "__riscv_fdiv\0"
    "__riscv_fsqrt\0"
    "__riscv_float_abi_double\0"
    ;

#define XLEN 8

#define TREG_RA 17
#define TREG_SP 18

ST_DATA const int reg_classes[NB_REGS] = {
  RC_INT | RC_R(0),
  RC_INT | RC_R(1),
  RC_INT | RC_R(2),
  RC_INT | RC_R(3),
  RC_INT | RC_R(4),
  RC_INT | RC_R(5),
  RC_INT | RC_R(6),
  RC_INT | RC_R(7),
  RC_FLOAT | RC_F(0),
  RC_FLOAT | RC_F(1),
  RC_FLOAT | RC_F(2),
  RC_FLOAT | RC_F(3),
  RC_FLOAT | RC_F(4),
  RC_FLOAT | RC_F(5),
  RC_FLOAT | RC_F(6),
  RC_FLOAT | RC_F(7),
  0,
  1 << TREG_RA,
  1 << TREG_SP
};

#if defined(CONFIG_TCC_BCHECK)
static addr_t func_bound_offset;
static unsigned long func_bound_ind;
ST_DATA int func_bound_add_epilog;
#endif

static int ireg(int r)
{
    if (r == TREG_RA)
      return 1; // ra
    if (r == TREG_SP)
      return 2; // sp
    assert(r >= 0 && r < 8);
    return r + 10;  // tccrX --> aX == x(10+X)
}

static int is_ireg(int r)
{
    return (unsigned)r < 8 || r == TREG_RA || r == TREG_SP;
}

static int freg(int r)
{
    assert(r >= 8 && r < 16);
    return r - 8 + 10;  // tccfX --> faX == f(10+X)
}

static int is_freg(int r)
{
    return r >= 8 && r < 16;
}

ST_FUNC void o(unsigned int c)
{
    int ind1 = ind + 4;
    if (nocode_wanted)
        return;
    if (ind1 > cur_text_section->data_allocated)
        section_realloc(cur_text_section, ind1);
    write32le(cur_text_section->data + ind, c);
    ind = ind1;
}

static void EIu(uint32_t opcode, uint32_t func3,
               uint32_t rd, uint32_t rs1, uint32_t imm)
{
    o(opcode | (func3 << 12) | (rd << 7) | (rs1 << 15) | (imm << 20));
}

static void ER(uint32_t opcode, uint32_t func3,
               uint32_t rd, uint32_t rs1, uint32_t rs2, uint32_t func7)
{
    o(opcode | func3 << 12 | rd << 7 | rs1 << 15 | rs2 << 20 | func7 << 25);
}

static void EI(uint32_t opcode, uint32_t func3,
               uint32_t rd, uint32_t rs1, uint32_t imm)
{
    assert(! ((imm + (1 << 11)) >> 12));
    EIu(opcode, func3, rd, rs1, imm);
}

static void ES(uint32_t opcode, uint32_t func3,
               uint32_t rs1, uint32_t rs2, uint32_t imm)
{
    assert(! ((imm + (1 << 11)) >> 12));
    o(opcode | (func3 << 12) | ((imm & 0x1f) << 7) | (rs1 << 15)
      | (rs2 << 20) | ((imm >> 5) << 25));
}

// Patch all branches in list pointed to by t to branch to a:
ST_FUNC void gsym_addr(int t_, int a_)
{
    uint32_t t = t_;
    uint32_t a = a_;
    while (t) {
        unsigned char *ptr = cur_text_section->data + t;
        uint32_t next = read32le(ptr);
        uint32_t r = a - t, imm;
        if ((r + (1 << 21)) & ~((1U << 22) - 2))
          tcc_error("out-of-range branch chain");
        imm =   (((r >> 12) &  0xff) << 12)
            | (((r >> 11) &     1) << 20)
            | (((r >>  1) & 0x3ff) << 21)
            | (((r >> 20) &     1) << 31);
        write32le(ptr, r == 4 ? 0x33 : 0x6f | imm); // nop || j imm
        t = next;
    }
}

static int load_symofs(int r, SValue *sv, int forstore)
{
    static Sym label;
    int rr, doload = 0;
    int fc = sv->c.i, v = sv->r & VT_VALMASK;
    if (sv->r & VT_SYM) {
        assert(v == VT_CONST);
        if (sv->sym->type.t & VT_STATIC) { // XXX do this per linker relax
            greloca(cur_text_section, sv->sym, ind,
                    R_RISCV_PCREL_HI20, sv->c.i);
            sv->c.i = 0;
        } else {
            if (((unsigned)fc + (1 << 11)) >> 12)
              tcc_error("unimp: large addend for global address (0x%lx)", (long)sv->c.i);
            greloca(cur_text_section, sv->sym, ind,
                    R_RISCV_GOT_HI20, 0);
            doload = 1;
        }
        if (!label.v) {
            label.v = tok_alloc(".L0 ", 4)->tok;
            label.type.t = VT_VOID | VT_STATIC;
        }
        label.c = 0; /* force new local ELF symbol */
        put_extern_sym(&label, cur_text_section, ind, 0);
        rr = is_ireg(r) ? ireg(r) : 5;
        o(0x17 | (rr << 7));   // auipc RR, 0 %pcrel_hi(sym)+addend
        greloca(cur_text_section, &label, ind,
                doload || !forstore
                  ? R_RISCV_PCREL_LO12_I : R_RISCV_PCREL_LO12_S, 0);
        if (doload) {
            EI(0x03, 3, rr, rr, 0); // ld RR, 0(RR)
        }
    } else if (v == VT_LOCAL || v == VT_LLOCAL) {
        rr = 8; // s0
        if (fc != sv->c.i)
          tcc_error("unimp: store(giant local off) (0x%lx)", (long)sv->c.i);
        if (((unsigned)fc + (1 << 11)) >> 12) {
            rr = is_ireg(r) ? ireg(r) : 5; // t0
            o(0x37 | (rr << 7) | ((0x800 + fc) & 0xfffff000)); //lui RR, upper(fc)
            ER(0x33, 0, rr, rr, 8, 0); // add RR, RR, s0
            sv->c.i = fc << 20 >> 20;
        }
    } else
      tcc_error("uhh");
    return rr;
}

static void load_large_constant(int rr, int fc, uint32_t pi)
{
    if (fc < 0)
	pi++;
    o(0x37 | (rr << 7) | (((pi + 0x800) & 0xfffff000))); // lui RR, up(up(fc))
    EI(0x13, 0, rr, rr, (int)pi << 20 >> 20);   // addi RR, RR, lo(up(fc))
    EI(0x13, 1, rr, rr, 12); // slli RR, RR, 12
    EI(0x13, 0, rr, rr, (fc + (1 << 19)) >> 20);  // addi RR, RR, up(lo(fc))
    EI(0x13, 1, rr, rr, 12); // slli RR, RR, 12
    fc = fc << 12 >> 12;
    EI(0x13, 0, rr, rr, fc >> 8);  // addi RR, RR, lo1(lo(fc))
    EI(0x13, 1, rr, rr, 8); // slli RR, RR, 8
}

ST_FUNC void load(int r, SValue *sv)
{
    int fr = sv->r;
    int v = fr & VT_VALMASK;
    int rr = is_ireg(r) ? ireg(r) : freg(r);
    int fc = sv->c.i;
    int bt = sv->type.t & VT_BTYPE;
    int align, size;
    if (fr & VT_LVAL) {
        int func3, opcode = is_freg(r) ? 0x07 : 0x03, br;
        size = type_size(&sv->type, &align);
        assert (!is_freg(r) || bt == VT_FLOAT || bt == VT_DOUBLE);
        if (bt == VT_FUNC) /* XXX should be done in generic code */
          size = PTR_SIZE;
        func3 = size == 1 ? 0 : size == 2 ? 1 : size == 4 ? 2 : 3;
        if (size < 4 && !is_float(sv->type.t) && (sv->type.t & VT_UNSIGNED))
          func3 |= 4;
        if (v == VT_LOCAL || (fr & VT_SYM)) {
            br = load_symofs(r, sv, 0);
            fc = sv->c.i;
        } else if (v < VT_CONST) {
            br = ireg(v);
            /*if (((unsigned)fc + (1 << 11)) >> 12)
              tcc_error("unimp: load(large addend) (0x%x)", fc);*/
            fc = 0; // XXX store ofs in LVAL(reg)
        } else if (v == VT_LLOCAL) {
            br = load_symofs(r, sv, 0);
            fc = sv->c.i;
            EI(0x03, 3, rr, br, fc); // ld RR, fc(BR)
            br = rr;
            fc = 0;
        } else if (v == VT_CONST) {
            int64_t si = sv->c.i;
            si >>= 32;
            if (si != 0) {
		load_large_constant(rr, fc, si);
                fc &= 0xff;
            } else {
                o(0x37 | (rr << 7) | ((0x800 + fc) & 0xfffff000)); //lui RR, upper(fc)
                fc = fc << 20 >> 20;
	    }
            br = rr;
	} else {
            tcc_error("unimp: load(non-local lval)");
        }
        EI(opcode, func3, rr, br, fc); // l[bhwd][u] / fl[wd] RR, fc(BR)
    } else if (v == VT_CONST) {
        int rb = 0, do32bit = 8, zext = 0;
        assert((!is_float(sv->type.t) && is_ireg(r)) || bt == VT_LDOUBLE);
        if (fr & VT_SYM) {
            rb = load_symofs(r, sv, 0);
            fc = sv->c.i;
            do32bit = 0;
        }
        if (is_float(sv->type.t) && bt != VT_LDOUBLE)
          tcc_error("unimp: load(float)");
        if (fc != sv->c.i) {
            int64_t si = sv->c.i;
            si >>= 32;
            if (si != 0) {
		load_large_constant(rr, fc, si);
                fc &= 0xff;
                rb = rr;
                do32bit = 0;
            } else if (bt == VT_LLONG) {
                /* A 32bit unsigned constant for a 64bit type.
                   lui always sign extends, so we need to do an explicit zext.*/
                zext = 1;
            }
        }
        if (((unsigned)fc + (1 << 11)) >> 12)
            o(0x37 | (rr << 7) | ((0x800 + fc) & 0xfffff000)), rb = rr; //lui RR, upper(fc)
        if (fc || (rr != rb) || do32bit || (fr & VT_SYM))
          EI(0x13 | do32bit, 0, rr, rb, fc << 20 >> 20); // addi[w] R, x0|R, FC
        if (zext) {
            EI(0x13, 1, rr, rr, 32); // slli RR, RR, 32
            EI(0x13, 5, rr, rr, 32); // srli RR, RR, 32
        }
    } else if (v == VT_LOCAL) {
        int br = load_symofs(r, sv, 0);
        assert(is_ireg(r));
        fc = sv->c.i;
        EI(0x13, 0, rr, br, fc); // addi R, s0, FC
    } else if (v < VT_CONST) { /* reg-reg */
        //assert(!fc); XXX support offseted regs
        if (is_freg(r) && is_freg(v))
          ER(0x53, 0, rr, freg(v), freg(v), bt == VT_DOUBLE ? 0x11 : 0x10); //fsgnj.[sd] RR, V, V == fmv.[sd] RR, V
        else if (is_ireg(r) && is_ireg(v))
          EI(0x13, 0, rr, ireg(v), 0); // addi RR, V, 0 == mv RR, V
        else {
            int func7 = is_ireg(r) ? 0x70 : 0x78;
            size = type_size(&sv->type, &align);
            if (size == 8)
              func7 |= 1;
            assert(size == 4 || size == 8);
            o(0x53 | (rr << 7) | ((is_freg(v) ? freg(v) : ireg(v)) << 15)
              | (func7 << 25)); // fmv.{w.x, x.w, d.x, x.d} RR, VR
        }
    } else if (v == VT_CMP) {
        int op = vtop->cmp_op;
        int a = vtop->cmp_r & 0xff;
        int b = (vtop->cmp_r >> 8) & 0xff;
        int inv = 0;
        switch (op) {
            case TOK_ULT:
            case TOK_UGE:
            case TOK_ULE:
            case TOK_UGT:
            case TOK_LT:
            case TOK_GE:
            case TOK_LE:
            case TOK_GT:
                if (op & 1) { // remove [U]GE,GT
                    inv = 1;
                    op--;
                }
                if ((op & 7) == 6) { // [U]LE
                    int t = a; a = b; b = t;
                    inv ^= 1;
                }
                ER(0x33, (op > TOK_UGT) ? 2 : 3, rr, a, b, 0); // slt[u] d, a, b
                if (inv)
                  EI(0x13, 4, rr, rr, 1); // xori d, d, 1
                break;
            case TOK_NE:
            case TOK_EQ:
                if (rr != a || b)
                  ER(0x33, 0, rr, a, b, 0x20); // sub d, a, b
                if (op == TOK_NE)
                  ER(0x33, 3, rr, 0, rr, 0); // sltu d, x0, d == snez d,d
                else
                  EI(0x13, 3, rr, rr, 1); // sltiu d, d, 1 == seqz d,d
                break;
        }
    } else if ((v & ~1) == VT_JMP) {
        int t = v & 1;
        assert(is_ireg(r));
        EI(0x13, 0, rr, 0, t);      // addi RR, x0, t
        gjmp_addr(ind + 8);
        gsym(fc);
        EI(0x13, 0, rr, 0, t ^ 1);  // addi RR, x0, !t
    } else
      tcc_error("unimp: load(non-const)");
}

ST_FUNC void store(int r, SValue *sv)
{
    int fr = sv->r & VT_VALMASK;
    int rr = is_ireg(r) ? ireg(r) : freg(r), ptrreg;
    int fc = sv->c.i;
    int bt = sv->type.t & VT_BTYPE;
    int align, size = type_size(&sv->type, &align);
    assert(!is_float(bt) || is_freg(r) || bt == VT_LDOUBLE);
    /* long doubles are in two integer registers, but the load/store
       primitives only deal with one, so do as if it's one reg.  */
    if (bt == VT_LDOUBLE)
      size = align = 8;
    if (bt == VT_STRUCT)
      tcc_error("unimp: store(struct)");
    if (size > 8)
      tcc_error("unimp: large sized store");
    assert(sv->r & VT_LVAL);
    if (fr == VT_LOCAL || (sv->r & VT_SYM)) {
        ptrreg = load_symofs(-1, sv, 1);
        fc = sv->c.i;
    } else if (fr < VT_CONST) {
        ptrreg = ireg(fr);
        /*if (((unsigned)fc + (1 << 11)) >> 12)
          tcc_error("unimp: store(large addend) (0x%x)", fc);*/
        fc = 0; // XXX support offsets regs
    } else if (fr == VT_CONST) {
        int64_t si = sv->c.i;
        ptrreg = 8; // s0
        si >>= 32;
        if (si != 0) {
	    load_large_constant(ptrreg, fc, si);
            fc &= 0xff;
        } else {
            o(0x37 | (ptrreg << 7) | ((0x800 + fc) & 0xfffff000)); //lui RR, upper(fc)
            fc = fc << 20 >> 20;
	}
    } else
      tcc_error("implement me: %s(!local)", __FUNCTION__);
    ES(is_freg(r) ? 0x27 : 0x23,                          // fs... | s...
       size == 1 ? 0 : size == 2 ? 1 : size == 4 ? 2 : 3, // ... [wd] | [bhwd]
       ptrreg, rr, fc);                                   // RR, fc(base)
}

static void gcall_or_jmp(int docall)
{
    int tr = docall ? 1 : 5; // ra or t0
    if ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST &&
        ((vtop->r & VT_SYM) && vtop->c.i == (int)vtop->c.i)) {
        /* constant symbolic case -> simple relocation */
        greloca(cur_text_section, vtop->sym, ind,
                R_RISCV_CALL_PLT, (int)vtop->c.i);
        o(0x17 | (tr << 7));   // auipc TR, 0 %call(func)
        EI(0x67, 0, tr, tr, 0);// jalr  TR, r(TR)
    } else if (vtop->r < VT_CONST) {
        int r = ireg(vtop->r);
        EI(0x67, 0, tr, r, 0);      // jalr TR, 0(R)
    } else {
        int r = TREG_RA;
        load(r, vtop);
        r = ireg(r);
        EI(0x67, 0, tr, r, 0);      // jalr TR, 0(R)
    }
}

#if defined(CONFIG_TCC_BCHECK)

static void gen_bounds_call(int v)
{
    Sym *sym = external_helper_sym(v);

    greloca(cur_text_section, sym, ind, R_RISCV_CALL_PLT, 0);
    o(0x17 | (1 << 7));   // auipc TR, 0 %call(func)
    EI(0x67, 0, 1, 1, 0); // jalr  TR, r(TR)
}

static void gen_bounds_prolog(void)
{
    /* leave some room for bound checking code */
    func_bound_offset = lbounds_section->data_offset;
    func_bound_ind = ind;
    func_bound_add_epilog = 0;
    o(0x00000013);  /* ld a0,#lbound section pointer */
    o(0x00000013);
    o(0x00000013);  /* nop -> call __bound_local_new */
    o(0x00000013);
}

static void gen_bounds_epilog(void)
{
    static Sym label;
    addr_t saved_ind;
    addr_t *bounds_ptr;
    Sym *sym_data;
    int offset_modified = func_bound_offset != lbounds_section->data_offset;

    if (!offset_modified && !func_bound_add_epilog)
        return;

    /* add end of table info */
    bounds_ptr = section_ptr_add(lbounds_section, sizeof(addr_t));
    *bounds_ptr = 0;

    sym_data = get_sym_ref(&char_pointer_type, lbounds_section,
                           func_bound_offset, lbounds_section->data_offset);

    if (!label.v) {
        label.v = tok_alloc(".LB0 ", 4)->tok;
        label.type.t = VT_VOID | VT_STATIC;
    }
    /* generate bound local allocation */
    if (offset_modified) {
        saved_ind = ind;
        ind = func_bound_ind;
        label.c = 0; /* force new local ELF symbol */
        put_extern_sym(&label, cur_text_section, ind, 0);
        greloca(cur_text_section, sym_data, ind, R_RISCV_GOT_HI20, 0);
        o(0x17 | (10 << 7));    // auipc a0, 0 %pcrel_hi(sym)+addend
        greloca(cur_text_section, &label, ind, R_RISCV_PCREL_LO12_I, 0);
        EI(0x03, 3, 10, 10, 0); // ld a0, 0(a0)
        gen_bounds_call(TOK___bound_local_new);
        ind = saved_ind;
    }

    /* generate bound check local freeing */
    o(0xe02a1101); /* addi sp,sp,-32  sd   a0,0(sp)   */
    o(0xa82ae42e); /* sd   a1,8(sp)   fsd  fa0,16(sp) */
    label.c = 0; /* force new local ELF symbol */
    put_extern_sym(&label, cur_text_section, ind, 0);
    greloca(cur_text_section, sym_data, ind, R_RISCV_GOT_HI20, 0);
    o(0x17 | (10 << 7));    // auipc a0, 0 %pcrel_hi(sym)+addend
    greloca(cur_text_section, &label, ind, R_RISCV_PCREL_LO12_I, 0);
    EI(0x03, 3, 10, 10, 0); // ld a0, 0(a0)
    gen_bounds_call(TOK___bound_local_delete);
    o(0x65a26502); /* ld   a0,0(sp)   ld   a1,8(sp)   */
    o(0x61052542); /* fld  fa0,16(sp) addi sp,sp,32   */
}
#endif

static void reg_pass_rec(CType *type, int *rc, int *fieldofs, int ofs)
{
    if ((type->t & VT_BTYPE) == VT_STRUCT) {
        Sym *f;
        if (type->ref->type.t == VT_UNION)
          rc[0] = -1;
        else for (f = type->ref->next; f; f = f->next)
          reg_pass_rec(&f->type, rc, fieldofs, ofs + f->c);
    } else if (type->t & VT_ARRAY) {
        if (type->ref->c < 0 || type->ref->c > 2)
          rc[0] = -1;
        else {
            int a, sz = type_size(&type->ref->type, &a);
            reg_pass_rec(&type->ref->type, rc, fieldofs, ofs);
            if (rc[0] > 2 || (rc[0] == 2 && type->ref->c > 1))
              rc[0] = -1;
            else if (type->ref->c == 2 && rc[0] && rc[1] == RC_FLOAT) {
              rc[++rc[0]] = RC_FLOAT;
              fieldofs[rc[0]] = ((ofs + sz) << 4)
                                | (type->ref->type.t & VT_BTYPE);
            } else if (type->ref->c == 2)
              rc[0] = -1;
        }
    } else if (rc[0] == 2 || rc[0] < 0 || (type->t & VT_BTYPE) == VT_LDOUBLE)
      rc[0] = -1;
    else if (!rc[0] || rc[1] == RC_FLOAT || is_float(type->t)) {
      rc[++rc[0]] = is_float(type->t) ? RC_FLOAT : RC_INT;
      fieldofs[rc[0]] = (ofs << 4) | ((type->t & VT_BTYPE) == VT_PTR ? VT_LLONG : type->t & VT_BTYPE);
    } else
      rc[0] = -1;
}

static void reg_pass(CType *type, int *prc, int *fieldofs, int named)
{
    prc[0] = 0;
    reg_pass_rec(type, prc, fieldofs, 0);
    if (prc[0] <= 0 || !named) {
        int align, size = type_size(type, &align);
        prc[0] = (size + 7) >> 3;
        prc[1] = prc[2] = RC_INT;
        fieldofs[1] = (0 << 4) | (size <= 1 ? VT_BYTE : size <= 2 ? VT_SHORT : size <= 4 ? VT_INT : VT_LLONG);
        fieldofs[2] = (8 << 4) | (size <= 9 ? VT_BYTE : size <= 10 ? VT_SHORT : size <= 12 ? VT_INT : VT_LLONG);
    }
}

ST_FUNC void gfunc_call(int nb_args)
{
    int i, align, size, areg[2];
    int *info = tcc_malloc((nb_args + 1) * sizeof (int));
    int stack_adj = 0, tempspace = 0, stack_add, ofs, splitofs = 0;
    SValue *sv;
    Sym *sa;

#ifdef CONFIG_TCC_BCHECK
    int bc_save = tcc_state->do_bounds_check;
    if (tcc_state->do_bounds_check)
        gbound_args(nb_args);
#endif

    areg[0] = 0; /* int arg regs */
    areg[1] = 8; /* float arg regs */
    sa = vtop[-nb_args].type.ref->next;
    for (i = 0; i < nb_args; i++) {
        int nregs, byref = 0, tempofs;
        int prc[3], fieldofs[3];
        sv = &vtop[1 + i - nb_args];
        sv->type.t &= ~VT_ARRAY; // XXX this should be done in tccgen.c
        size = type_size(&sv->type, &align);
        if (size > 16) {
            if (align < XLEN)
              align = XLEN;
            tempspace = (tempspace + align - 1) & -align;
            tempofs = tempspace;
            tempspace += size;
            size = align = 8;
            byref = 64 | (tempofs << 7);
        }
        reg_pass(&sv->type, prc, fieldofs, sa != 0);
        if (!sa && align == 2*XLEN && size <= 2*XLEN)
          areg[0] = (areg[0] + 1) & ~1;
        nregs = prc[0];
        if (size == 0)
            info[i] = 0;
        else if ((prc[1] == RC_INT && areg[0] >= 8)
            || (prc[1] == RC_FLOAT && areg[1] >= 16)
            || (nregs == 2 && prc[1] == RC_FLOAT && prc[2] == RC_FLOAT
                && areg[1] >= 15)
            || (nregs == 2 && prc[1] != prc[2]
                && (areg[1] >= 16 || areg[0] >= 8))) {
            info[i] = 32;
            if (align < XLEN)
              align = XLEN;
            stack_adj += (size + align - 1) & -align;
            if (!sa) /* one vararg on stack forces the rest on stack */
              areg[0] = 8, areg[1] = 16;
        } else {
            info[i] = areg[prc[1] - 1]++;
            if (!byref)
              info[i] |= (fieldofs[1] & VT_BTYPE) << 12;
            assert(!(fieldofs[1] >> 4));
            if (nregs == 2) {
                if (prc[2] == RC_FLOAT || areg[0] < 8)
                  info[i] |= (1 + areg[prc[2] - 1]++) << 7;
                else {
                    info[i] |= 16;
                    stack_adj += 8;
                }
                if (!byref) {
                    assert((fieldofs[2] >> 4) < 2048);
                    info[i] |= fieldofs[2] << (12 + 4); // includes offset
                }
            }
        }
        info[i] |= byref;
        if (sa)
          sa = sa->next;
    }
    stack_adj = (stack_adj + 15) & -16;
    tempspace = (tempspace + 15) & -16;
    stack_add = stack_adj + tempspace;
    if (stack_add) {
        if (stack_add >= 0x1000) {
            o(0x37 | (5 << 7) | (-stack_add & 0xfffff000)); //lui t0, upper(v)
            EI(0x13, 0, 5, 5, -stack_add << 20 >> 20); // addi t0, t0, lo(v)
            ER(0x33, 0, 2, 2, 5, 0); // add sp, sp, t0
        }
        else
            EI(0x13, 0, 2, 2, -stack_add);   // addi sp, sp, -adj
        for (i = ofs = 0; i < nb_args; i++) {
            if (info[i] & (64 | 32)) {
                vrotb(nb_args - i);
                size = type_size(&vtop->type, &align);
                if (info[i] & 64) {
                    vset(&char_pointer_type, TREG_SP, 0);
                    vpushi(stack_adj + (info[i] >> 7));
                    gen_op('+');
                    vpushv(vtop); // this replaces the old argument
                    vrott(3);
                    indir();
                    vtop->type = vtop[-1].type;
                    vswap();
                    vstore();
                    vpop();
                    size = align = 8;
                }
                if (info[i] & 32) {
                    if (align < XLEN)
                      align = XLEN;
                    /* Once we support offseted regs we can do this:
                       vset(&vtop->type, TREG_SP | VT_LVAL, ofs);
                       to construct the lvalue for the outgoing stack slot,
                       until then we have to jump through hoops.  */
                    vset(&char_pointer_type, TREG_SP, 0);
                    ofs = (ofs + align - 1) & -align;
                    vpushi(ofs);
                    gen_op('+');
                    indir();
                    vtop->type = vtop[-1].type;
                    vswap();
                    vstore();
                    vtop->r = vtop->r2 = VT_CONST; // this arg is done
                    ofs += size;
                }
                vrott(nb_args - i);
            } else if (info[i] & 16) {
                assert(!splitofs);
                splitofs = ofs;
                ofs += 8;
            }
        }
    }
    for (i = 0; i < nb_args; i++) {
        int ii = info[nb_args - 1 - i], r = ii, r2 = r;
        if (!(r & 32)) {
            CType origtype;
            int loadt;
            r &= 15;
            r2 = r2 & 64 ? 0 : (r2 >> 7) & 31;
            assert(r2 <= 16);
            vrotb(i+1);
            origtype = vtop->type;
            size = type_size(&vtop->type, &align);
            if (size == 0)
                goto done;
            loadt = vtop->type.t & VT_BTYPE;
            if (loadt == VT_STRUCT) {
                loadt = (ii >> 12) & VT_BTYPE;
            }
            if (info[nb_args - 1 - i] & 16) {
                assert(!r2);
                r2 = 1 + TREG_RA;
            }
            if (loadt == VT_LDOUBLE) {
                assert(r2);
                r2--;
            } else if (r2) {
                test_lvalue();
                vpushv(vtop);
            }
            vtop->type.t = loadt | (vtop->type.t & VT_UNSIGNED);
            gv(r < 8 ? RC_R(r) : RC_F(r - 8));
            vtop->type = origtype;

            if (r2 && loadt != VT_LDOUBLE) {
                r2--;
                assert(r2 < 16 || r2 == TREG_RA);
                vswap();
                gaddrof();
                vtop->type = char_pointer_type;
                vpushi(ii >> 20);
#ifdef CONFIG_TCC_BCHECK
		if ((origtype.t & VT_BTYPE) == VT_STRUCT)
                    tcc_state->do_bounds_check = 0;
#endif
                gen_op('+');
#ifdef CONFIG_TCC_BCHECK
		tcc_state->do_bounds_check = bc_save;
#endif
                indir();
                vtop->type = origtype;
                loadt = vtop->type.t & VT_BTYPE;
                if (loadt == VT_STRUCT) {
                    loadt = (ii >> 16) & VT_BTYPE;
                }
                save_reg_upstack(r2, 1);
                vtop->type.t = loadt | (vtop->type.t & VT_UNSIGNED);
                load(r2, vtop);
                assert(r2 < VT_CONST);
                vtop--;
                vtop->r2 = r2;
            }
            if (info[nb_args - 1 - i] & 16) {
                ES(0x23, 3, 2, ireg(vtop->r2), splitofs); // sd t0, ofs(sp)
                vtop->r2 = VT_CONST;
            } else if (loadt == VT_LDOUBLE && vtop->r2 != r2) {
                assert(vtop->r2 <= 7 && r2 <= 7);
                /* XXX we'd like to have 'gv' move directly into
                   the right class instead of us fixing it up.  */
                EI(0x13, 0, ireg(r2), ireg(vtop->r2), 0); // mv Ra+1, RR2
                vtop->r2 = r2;
            }
done:
            vrott(i+1);
        }
    }
    vrotb(nb_args + 1);
    save_regs(nb_args + 1);
    gcall_or_jmp(1);
    vtop -= nb_args + 1;
    if (stack_add) {
        if (stack_add >= 0x1000) {
            o(0x37 | (5 << 7) | (stack_add & 0xfffff000)); //lui t0, upper(v)
            EI(0x13, 0, 5, 5, stack_add << 20 >> 20); // addi t0, t0, lo(v)
            ER(0x33, 0, 2, 2, 5, 0); // add sp, sp, t0
        }
        else
            EI(0x13, 0, 2, 2, stack_add);      // addi sp, sp, adj
   }
   tcc_free(info);
}

static int func_sub_sp_offset, num_va_regs, func_va_list_ofs;

ST_FUNC void gfunc_prolog(Sym *func_sym)
{
    CType *func_type = &func_sym->type;
    int i, addr, align, size;
    int param_addr = 0;
    int areg[2];
    Sym *sym;
    CType *type;

    sym = func_type->ref;
    loc = -16; // for ra and s0
    func_sub_sp_offset = ind;
    ind += 5 * 4;

    areg[0] = 0, areg[1] = 0;
    addr = 0;
    /* if the function returns by reference, then add an
       implicit pointer parameter */
    size = type_size(&func_vt, &align);
    if (size > 2 * XLEN) {
        loc -= 8;
        func_vc = loc;
        ES(0x23, 3, 8, 10 + areg[0]++, loc); // sd a0, loc(s0)
    }
    /* define parameters */
    while ((sym = sym->next) != NULL) {
        int byref = 0;
        int regcount;
        int prc[3], fieldofs[3];
        type = &sym->type;
        size = type_size(type, &align);
        if (size > 2 * XLEN) {
            type = &char_pointer_type;
            size = align = byref = 8;
        }
        reg_pass(type, prc, fieldofs, 1);
        regcount = prc[0];
        if (areg[prc[1] - 1] >= 8
            || (regcount == 2
                && ((prc[1] == RC_FLOAT && prc[2] == RC_FLOAT && areg[1] >= 7)
                    || (prc[1] != prc[2] && (areg[1] >= 8 || areg[0] >= 8))))) {
            if (align < XLEN)
              align = XLEN;
            addr = (addr + align - 1) & -align;
            param_addr = addr;
            addr += size;
        } else {
            loc -= regcount * 8; // XXX could reserve only 'size' bytes
            param_addr = loc;
            for (i = 0; i < regcount; i++) {
                if (areg[prc[1+i] - 1] >= 8) {
                    assert(i == 1 && regcount == 2 && !(addr & 7));
                    EI(0x03, 3, 5, 8, addr); // ld t0, addr(s0)
                    addr += 8;
                    ES(0x23, 3, 8, 5, loc + i*8); // sd t0, loc(s0)
                } else if (prc[1+i] == RC_FLOAT) {
                    ES(0x27, (size / regcount) == 4 ? 2 : 3, 8, 10 + areg[1]++, loc + (fieldofs[i+1] >> 4)); // fs[wd] FAi, loc(s0)
                } else {
                    ES(0x23, 3, 8, 10 + areg[0]++, loc + i*8); // sd aX, loc(s0) // XXX
                }
            }
        }
        sym_push(sym->v & ~SYM_FIELD, &sym->type,
                 (byref ? VT_LLOCAL : VT_LOCAL) | VT_LVAL,
                 param_addr);
    }
    func_va_list_ofs = addr;
    num_va_regs = 0;
    if (func_var) {
        for (; areg[0] < 8; areg[0]++) {
            num_va_regs++;
            ES(0x23, 3, 8, 10 + areg[0], -8 + num_va_regs * 8); // sd aX, loc(s0)
        }
    }
#ifdef CONFIG_TCC_BCHECK
    if (tcc_state->do_bounds_check)
        gen_bounds_prolog();
#endif
}

ST_FUNC int gfunc_sret(CType *vt, int variadic, CType *ret,
                       int *ret_align, int *regsize)
{
    int align, size = type_size(vt, &align), nregs;
    int prc[3], fieldofs[3];
    *ret_align = 1;
    *regsize = 8;
    if (size > 16)
      return 0;
    reg_pass(vt, prc, fieldofs, 1);
    nregs = prc[0];
    if (nregs == 2 && prc[1] != prc[2])
      return -1;  /* generic code can't deal with this case */
    if (prc[1] == RC_FLOAT) {
        *regsize = size / nregs;
    }
    ret->t = fieldofs[1] & VT_BTYPE;
    ret->ref = NULL;
    return nregs;
}

ST_FUNC void arch_transfer_ret_regs(int aftercall)
{
    int prc[3], fieldofs[3];
    reg_pass(&vtop->type, prc, fieldofs, 1);
    assert(prc[0] == 2 && prc[1] != prc[2] && !(fieldofs[1] >> 4));
    assert(vtop->r == (VT_LOCAL | VT_LVAL));
    vpushv(vtop);
    vtop->type.t = fieldofs[1] & VT_BTYPE;
    (aftercall ? store : load)(prc[1] == RC_INT ? REG_IRET : REG_FRET, vtop);
    vtop->c.i += fieldofs[2] >> 4;
    vtop->type.t = fieldofs[2] & VT_BTYPE;
    (aftercall ? store : load)(prc[2] == RC_INT ? REG_IRET : REG_FRET, vtop);
    vtop--;
}

ST_FUNC void gfunc_epilog(void)
{
    int v, saved_ind, d, large_ofs_ind;

#ifdef CONFIG_TCC_BCHECK
    if (tcc_state->do_bounds_check)
        gen_bounds_epilog();
#endif

    loc = (loc - num_va_regs * 8);
    d = v = (-loc + 15) & -16;

    if (v >= (1 << 11)) {
        d = 16;
        o(0x37 | (5 << 7) | ((0x800 + (v-16)) & 0xfffff000)); //lui t0, upper(v)
        EI(0x13, 0, 5, 5, (v-16) << 20 >> 20); // addi t0, t0, lo(v)
        ER(0x33, 0, 2, 2, 5, 0); // add sp, sp, t0
    }
    EI(0x03, 3, 1, 2, d - 8 - num_va_regs * 8);  // ld ra, v-8(sp)
    EI(0x03, 3, 8, 2, d - 16 - num_va_regs * 8); // ld s0, v-16(sp)
    EI(0x13, 0, 2, 2, d);      // addi sp, sp, v
    EI(0x67, 0, 0, 1, 0);      // jalr x0, 0(x1), aka ret
    large_ofs_ind = ind;
    if (v >= (1 << 11)) {
        EI(0x13, 0, 8, 2, d - num_va_regs * 8);      // addi s0, sp, d
        o(0x37 | (5 << 7) | ((0x800 + (v-16)) & 0xfffff000)); //lui t0, upper(v)
        EI(0x13, 0, 5, 5, (v-16) << 20 >> 20); // addi t0, t0, lo(v)
        ER(0x33, 0, 2, 2, 5, 0x20); // sub sp, sp, t0
        gjmp_addr(func_sub_sp_offset + 5*4);
    }
    saved_ind = ind;

    ind = func_sub_sp_offset;
    EI(0x13, 0, 2, 2, -d);     // addi sp, sp, -d
    ES(0x23, 3, 2, 1, d - 8 - num_va_regs * 8);  // sd ra, d-8(sp)
    ES(0x23, 3, 2, 8, d - 16 - num_va_regs * 8); // sd s0, d-16(sp)
    if (v < (1 << 11))
      EI(0x13, 0, 8, 2, d - num_va_regs * 8);      // addi s0, sp, d
    else
      gjmp_addr(large_ofs_ind);
    if ((ind - func_sub_sp_offset) != 5*4)
      EI(0x13, 0, 0, 0, 0);      // addi x0, x0, 0 == nop
    ind = saved_ind;
}

ST_FUNC void gen_va_start(void)
{
    vtop--;
    vset(&char_pointer_type, VT_LOCAL, func_va_list_ofs);
}

ST_FUNC void gen_fill_nops(int bytes)
{
    if ((bytes & 3))
      tcc_error("alignment of code section not multiple of 4");
    while (bytes > 0) {
        EI(0x13, 0, 0, 0, 0);      // addi x0, x0, 0 == nop
        bytes -= 4;
    }
}

// Generate forward branch to label:
ST_FUNC int gjmp(int t)
{
    if (nocode_wanted)
      return t;
    o(t);
    return ind - 4;
}

// Generate branch to known address:
ST_FUNC void gjmp_addr(int a)
{
    uint32_t r = a - ind, imm;
    if ((r + (1 << 21)) & ~((1U << 22) - 2)) {
        o(0x17 | (5 << 7) | (((r + 0x800) & 0xfffff000))); // lui RR, up(r)
        r = (int)r << 20 >> 20;
        EI(0x67, 0, 0, 5, r);      // jalr x0, r(t0)
    } else {
        imm = (((r >> 12) &  0xff) << 12)
            | (((r >> 11) &     1) << 20)
            | (((r >>  1) & 0x3ff) << 21)
            | (((r >> 20) &     1) << 31);
        o(0x6f | imm); // jal x0, imm ==  j imm
    }
}

ST_FUNC int gjmp_cond(int op, int t)
{
    int tmp;
    int a = vtop->cmp_r & 0xff;
    int b = (vtop->cmp_r >> 8) & 0xff;
    switch (op) {
        case TOK_ULT: op = 6; break;
        case TOK_UGE: op = 7; break;
        case TOK_ULE: op = 7; tmp = a; a = b; b = tmp; break;
        case TOK_UGT: op = 6; tmp = a; a = b; b = tmp; break;
        case TOK_LT:  op = 4; break;
        case TOK_GE:  op = 5; break;
        case TOK_LE:  op = 5; tmp = a; a = b; b = tmp; break;
        case TOK_GT:  op = 4; tmp = a; a = b; b = tmp; break;
        case TOK_NE:  op = 1; break;
        case TOK_EQ:  op = 0; break;
    }
    o(0x63 | (op ^ 1) << 12 | a << 15 | b << 20 | 8 << 7); // bOP a,b,+4
    return gjmp(t);
}

ST_FUNC int gjmp_append(int n, int t)
{
    void *p;
    /* insert jump list n into t */
    if (n) {
        uint32_t n1 = n, n2;
        while ((n2 = read32le(p = cur_text_section->data + n1)))
            n1 = n2;
        write32le(p, t);
        t = n;
    }
    return t;
}

static void gen_opil(int op, int ll)
{
    int a, b, d;
    int func3 = 0;
    ll = ll ? 0 : 8;
    if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
        int fc = vtop->c.i;
        if (fc == vtop->c.i && !(((unsigned)fc + (1 << 11)) >> 12)) {
            int cll = 0;
            int m = ll ? 31 : 63;
            vswap();
            gv(RC_INT);
            a = ireg(vtop[0].r);
            --vtop;
            d = get_reg(RC_INT);
            ++vtop;
            vswap();
            switch (op) {
                case '-':
                    if (fc <= -(1 << 11))
                      break;
                    fc = -fc;
                case '+':
                    func3 = 0; // addi d, a, fc
                    cll = ll;
                do_cop:
                    EI(0x13 | cll, func3, ireg(d), a, fc);
                    --vtop;
                    if (op >= TOK_ULT && op <= TOK_GT) {
                      vset_VT_CMP(TOK_NE);
                      vtop->cmp_r = ireg(d) | 0 << 8;
                    } else
                      vtop[0].r = d;
                    return;
                case TOK_LE:
                    if (fc >= (1 << 11) - 1)
                      break;
                    ++fc;
                case TOK_LT:  func3 = 2; goto do_cop; // slti d, a, fc
                case TOK_ULE:
                    if (fc >= (1 << 11) - 1 || fc == -1)
                      break;
                    ++fc;
                case TOK_ULT: func3 = 3; goto do_cop; // sltiu d, a, fc
                case '^':     func3 = 4; goto do_cop; // xori d, a, fc
                case '|':     func3 = 6; goto do_cop; // ori  d, a, fc
                case '&':     func3 = 7; goto do_cop; // andi d, a, fc
                case TOK_SHL: func3 = 1; cll = ll; fc &= m; goto do_cop; // slli d, a, fc
                case TOK_SHR: func3 = 5; cll = ll; fc &= m; goto do_cop; // srli d, a, fc
                case TOK_SAR: func3 = 5; cll = ll; fc = 1024 | (fc & m); goto do_cop;

                case TOK_UGE: /* -> TOK_ULT */
                case TOK_UGT: /* -> TOK_ULE */
                case TOK_GE:  /* -> TOK_LT */
                case TOK_GT:  /* -> TOK_LE */
                    gen_opil(op - 1, !ll);
                    vtop->cmp_op ^= 1;
                    return;

                case TOK_NE:
                case TOK_EQ:
                    if (fc)
                      gen_opil('-', !ll), a = ireg(vtop++->r);
                    --vtop;
                    vset_VT_CMP(op);
                    vtop->cmp_r = a | 0 << 8;
                    return;
            }
        }
    }
    gv2(RC_INT, RC_INT);
    a = ireg(vtop[-1].r);
    b = ireg(vtop[0].r);
    vtop -= 2;
    d = get_reg(RC_INT);
    vtop++;
    vtop[0].r = d;
    d = ireg(d);
    switch (op) {
    default:
        if (op >= TOK_ULT && op <= TOK_GT) {
            vset_VT_CMP(op);
            vtop->cmp_r = a | b << 8;
            break;
        }
        tcc_error("implement me: %s(%s)", __FUNCTION__, get_tok_str(op, NULL));
        break;

    case '+':
        ER(0x33 | ll, 0, d, a, b, 0); // add d, a, b
        break;
    case '-':
        ER(0x33 | ll, 0, d, a, b, 0x20); // sub d, a, b
        break;
    case TOK_SAR:
        ER(0x33 | ll | ll, 5, d, a, b, 0x20); // sra d, a, b
        break;
    case TOK_SHR:
        ER(0x33 | ll | ll, 5, d, a, b, 0); // srl d, a, b
        break;
    case TOK_SHL:
        ER(0x33 | ll, 1, d, a, b, 0); // sll d, a, b
        break;
    case '*':
        ER(0x33 | ll, 0, d, a, b, 1); // mul d, a, b
        break;
    case '/':
        ER(0x33 | ll, 4, d, a, b, 1); // div d, a, b
        break;
    case '&':
        ER(0x33, 7, d, a, b, 0); // and d, a, b
        break;
    case '^':
        ER(0x33, 4, d, a, b, 0); // xor d, a, b
        break;
    case '|':
        ER(0x33, 6, d, a, b, 0); // or d, a, b
        break;
    case '%':
        ER(ll ? 0x3b:  0x33, 6, d, a, b, 1); // rem d, a, b
        break;
    case TOK_UMOD:
        ER(0x33 | ll, 7, d, a, b, 1); // remu d, a, b
        break;
    case TOK_PDIV:
    case TOK_UDIV:
        ER(0x33 | ll, 5, d, a, b, 1); // divu d, a, b
        break;
    }
}

ST_FUNC void gen_opi(int op)
{
    gen_opil(op, 0);
}

ST_FUNC void gen_opl(int op)
{
    gen_opil(op, 1);
}

ST_FUNC void gen_opf(int op)
{
    int rs1, rs2, rd, dbl, invert;
    if (vtop[0].type.t == VT_LDOUBLE) {
        CType type = vtop[0].type;
        int func = 0;
        int cond = -1;
        switch (op) {
        case '*': func = TOK___multf3; break;
        case '+': func = TOK___addtf3; break;
        case '-': func = TOK___subtf3; break;
        case '/': func = TOK___divtf3; break;
        case TOK_EQ: func = TOK___eqtf2; cond = 1; break;
        case TOK_NE: func = TOK___netf2; cond = 0; break;
        case TOK_LT: func = TOK___lttf2; cond = 10; break;
        case TOK_GE: func = TOK___getf2; cond = 11; break;
        case TOK_LE: func = TOK___letf2; cond = 12; break;
        case TOK_GT: func = TOK___gttf2; cond = 13; break;
        default: assert(0); break;
        }
        vpush_helper_func(func);
        vrott(3);
        gfunc_call(2);
        vpushi(0);
        vtop->r = REG_IRET;
        vtop->r2 = cond < 0 ? TREG_R(1) : VT_CONST;
        if (cond < 0)
            vtop->type = type;
        else {
            vpushi(0);
            gen_opil(op, 1);
        }
        return;
    }

    gv2(RC_FLOAT, RC_FLOAT);
    assert(vtop->type.t == VT_DOUBLE || vtop->type.t == VT_FLOAT);
    dbl = vtop->type.t == VT_DOUBLE;
    rs1 = freg(vtop[-1].r);
    rs2 = freg(vtop->r);
    vtop--;
    invert = 0;
    switch(op) {
    default:
        assert(0);
    case '+':
        op = 0; // fadd
    arithop:
        rd = get_reg(RC_FLOAT);
        vtop->r = rd;
        rd = freg(rd);
        ER(0x53, 7, rd, rs1, rs2, dbl | (op << 2)); // fop.[sd] RD, RS1, RS2 (dyn rm)
        break;
    case '-':
        op = 1; // fsub
        goto arithop;
    case '*':
        op = 2; // fmul
        goto arithop;
    case '/':
        op = 3; // fdiv
        goto arithop;
    case TOK_EQ:
        op = 2; // EQ
    cmpop:
        rd = get_reg(RC_INT);
        vtop->r = rd;
        rd = ireg(rd);
        ER(0x53, op, rd, rs1, rs2, dbl | 0x50); // fcmp.[sd] RD, RS1, RS2 (op == eq/lt/le)
        if (invert)
          EI(0x13, 4, rd, rd, 1); // xori RD, 1
        break;
    case TOK_NE:
        invert = 1;
        op = 2; // EQ
        goto cmpop;
    case TOK_LT:
        op = 1; // LT
        goto cmpop;
    case TOK_LE:
        op = 0; // LE
        goto cmpop;
    case TOK_GT:
        op = 1; // LT
        rd = rs1, rs1 = rs2, rs2 = rd;
        goto cmpop;
    case TOK_GE:
        op = 0; // LE
        rd = rs1, rs1 = rs2, rs2 = rd;
        goto cmpop;
    }
}

ST_FUNC void gen_cvt_sxtw(void)
{
    /* XXX on risc-v the registers are usually sign-extended already.
       Let's try to not do anything here.  */
}

ST_FUNC void gen_cvt_itof(int t)
{
    int rr = ireg(gv(RC_INT)), dr;
    int u = vtop->type.t & VT_UNSIGNED;
    int l = (vtop->type.t & VT_BTYPE) == VT_LLONG;
    if (t == VT_LDOUBLE) {
        int func = l ?
          (u ? TOK___floatunditf : TOK___floatditf) :
          (u ? TOK___floatunsitf : TOK___floatsitf);
        vpush_helper_func(func);
        vrott(2);
        gfunc_call(1);
        vpushi(0);
        vtop->type.t = t;
        vtop->r = REG_IRET;
        vtop->r2 = TREG_R(1);
    } else {
        vtop--;
        dr = get_reg(RC_FLOAT);
        vtop++;
        vtop->r = dr;
        dr = freg(dr);
        EIu(0x53, 7, dr, rr, ((0x68 | (t == VT_DOUBLE ? 1 : 0)) << 5) | (u ? 1 : 0) | (l ? 2 : 0)); // fcvt.[sd].[wl][u]
    }
}

ST_FUNC void gen_cvt_ftoi(int t)
{
    int ft = vtop->type.t & VT_BTYPE;
    int l = (t & VT_BTYPE) == VT_LLONG;
    int u = t & VT_UNSIGNED;
    if (ft == VT_LDOUBLE) {
        int func = l ?
          (u ? TOK___fixunstfdi : TOK___fixtfdi) :
          (u ? TOK___fixunstfsi : TOK___fixtfsi);
        vpush_helper_func(func);
        vrott(2);
        gfunc_call(1);
        vpushi(0);
        vtop->type.t = t;
        vtop->r = REG_IRET;
    } else {
        int rr = freg(gv(RC_FLOAT)), dr;
        vtop--;
        dr = get_reg(RC_INT);
        vtop++;
        vtop->r = dr;
        dr = ireg(dr);
        EIu(0x53, 1, dr, rr, ((0x60 | (ft == VT_DOUBLE ? 1 : 0)) << 5) | (u ? 1 : 0) | (l ? 2 : 0)); // fcvt.[wl][u].[sd] rtz
    }
}

ST_FUNC void gen_cvt_ftof(int dt)
{
    int st = vtop->type.t & VT_BTYPE, rs, rd;
    dt &= VT_BTYPE;
    if (st == dt)
      return;
    if (dt == VT_LDOUBLE || st == VT_LDOUBLE) {
        int func = (dt == VT_LDOUBLE) ?
            (st == VT_FLOAT ? TOK___extendsftf2 : TOK___extenddftf2) :
            (dt == VT_FLOAT ? TOK___trunctfsf2 : TOK___trunctfdf2);
        /* We can't use gfunc_call, as func_old_type works like vararg
           functions, and on riscv unnamed float args are passed like
           integers.  But we really need them in the float argument registers
           for extendsftf2/extenddftf2.  So, do it explicitely.  */
        save_regs(1);
        if (dt == VT_LDOUBLE)
          gv(RC_F(0));
        else {
            gv(RC_R(0));
            assert(vtop->r2 < 7);
            if (vtop->r2 != 1 + vtop->r) {
                EI(0x13, 0, ireg(vtop->r) + 1, ireg(vtop->r2), 0); // mv Ra+1, RR2
                vtop->r2 = 1 + vtop->r;
            }
        }
        vpush_helper_func(func);
        gcall_or_jmp(1);
        vtop -= 2;
        vpushi(0);
        vtop->type.t = dt;
        if (dt == VT_LDOUBLE)
          vtop->r = REG_IRET, vtop->r2 = REG_IRET+1;
        else
          vtop->r = REG_FRET;
    } else {
        assert (dt == VT_FLOAT || dt == VT_DOUBLE);
        assert (st == VT_FLOAT || st == VT_DOUBLE);
        rs = gv(RC_FLOAT);
        rd = get_reg(RC_FLOAT);
        if (dt == VT_DOUBLE)
          EI(0x53, 0, freg(rd), freg(rs), 0x21 << 5); // fcvt.d.s RD, RS (no rm)
        else
          EI(0x53, 7, freg(rd), freg(rs), (0x20 << 5) | 1); // fcvt.s.d RD, RS (dyn rm)
        vtop->r = rd;
    }
}

ST_FUNC void ggoto(void)
{
    gcall_or_jmp(0);
    vtop--;
}

ST_FUNC void gen_vla_sp_save(int addr)
{
    ES(0x23, 3, 8, 2, addr); // sd sp, fc(s0)
}

ST_FUNC void gen_vla_sp_restore(int addr)
{
    EI(0x03, 3, 2, 8, addr); // ld sp, fc(s0)
}

ST_FUNC void gen_vla_alloc(CType *type, int align)
{
    int rr;
#if defined(CONFIG_TCC_BCHECK)
    if (tcc_state->do_bounds_check)
        vpushv(vtop);
#endif
    rr = ireg(gv(RC_INT));
#if defined(CONFIG_TCC_BCHECK)
    if (tcc_state->do_bounds_check)
        EI(0x13, 0, rr, rr, 15+1);   // addi RR, RR, 15+1
    else
#endif
    EI(0x13, 0, rr, rr, 15);   // addi RR, RR, 15
    EI(0x13, 7, rr, rr, -16);  // andi, RR, RR, -16
    ER(0x33, 0, 2, 2, rr, 0x20); // sub sp, sp, rr
    vpop();
#if defined(CONFIG_TCC_BCHECK)
    if (tcc_state->do_bounds_check) {
        vpushi(0);
        vtop->r = TREG_R(0);
        o(0x00010513); /* mv a0,sp */
        vswap();
        vpush_helper_func(TOK___bound_new_region);
        vrott(3);
        gfunc_call(2);
        func_bound_add_epilog = 1;
    }
#endif
}
#endif
