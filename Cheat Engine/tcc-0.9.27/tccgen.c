/*
 *  TCC - Tiny C Compiler
 * 
 *  Copyright (c) 2001-2004 Fabrice Bellard
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

#include "tcc.h"

/********************************************************/
/* global variables */

/* loc : local variable index
   ind : output code index
   rsym: return symbol
   anon_sym: anonymous symbol index
*/
ST_DATA int rsym, anon_sym, ind, loc;

ST_DATA Sym *sym_free_first;
ST_DATA void **sym_pools;
ST_DATA int nb_sym_pools;

ST_DATA Sym *global_stack;
ST_DATA Sym *local_stack;
ST_DATA Sym *define_stack;
ST_DATA Sym *global_label_stack;
ST_DATA Sym *local_label_stack;
static int local_scope;
static int in_sizeof;
static int section_sym;

ST_DATA int vlas_in_scope; /* number of VLAs that are currently in scope */
ST_DATA int vla_sp_root_loc; /* vla_sp_loc for SP before any VLAs were pushed */
ST_DATA int vla_sp_loc; /* Pointer to variable holding location to store stack pointer on the stack when modifying stack pointer */

ST_DATA SValue __vstack[1+VSTACK_SIZE], *vtop, *pvtop;

ST_DATA int const_wanted; /* true if constant wanted */
ST_DATA int nocode_wanted; /* no code generation wanted */
#define NODATA_WANTED (nocode_wanted > 0) /* no static data output wanted either */
#define STATIC_DATA_WANTED (nocode_wanted & 0xC0000000) /* only static data output */
ST_DATA int global_expr;  /* true if compound literals must be allocated globally (used during initializers parsing */
ST_DATA CType func_vt; /* current function return type (used by return instruction) */
ST_DATA int func_var; /* true if current function is variadic (used by return instruction) */
ST_DATA int func_vc;
ST_DATA int last_line_num, last_ind, func_ind; /* debug last line number and pc */
ST_DATA const char *funcname;
ST_DATA int g_debug;

ST_DATA CType char_pointer_type, func_old_type, int_type, size_type, ptrdiff_type;

ST_DATA struct switch_t {
    struct case_t {
        int64_t v1, v2;
	int sym;
    } **p; int n; /* list of case ranges */
    int def_sym; /* default symbol */
} *cur_switch; /* current switch */

/* ------------------------------------------------------------------------- */

static void gen_cast(CType *type);
static void gen_cast_s(int t);
static inline CType *pointed_type(CType *type);
static int is_compatible_types(CType *type1, CType *type2);
static int parse_btype(CType *type, AttributeDef *ad);
static CType *type_decl(CType *type, AttributeDef *ad, int *v, int td);
static void parse_expr_type(CType *type);
static void init_putv(CType *type, Section *sec, unsigned long c);
static void decl_initializer(CType *type, Section *sec, unsigned long c, int first, int size_only);
static void block(int *bsym, int *csym, int is_expr);
static void decl_initializer_alloc(CType *type, AttributeDef *ad, int r, int has_init, int v, int scope);
static void decl(int l);
static int decl0(int l, int is_for_loop_init, Sym *);
static void expr_eq(void);
static void vla_runtime_type_size(CType *type, int *a);
static void vla_sp_restore(void);
static void vla_sp_restore_root(void);
static int is_compatible_unqualified_types(CType *type1, CType *type2);
static inline int64_t expr_const64(void);
static void vpush64(int ty, unsigned long long v);
static void vpush(CType *type);
static int gvtst(int inv, int t);
static void gen_inline_functions(TCCState *s);
static void skip_or_save_block(TokenString **str);
static void gv_dup(void);

ST_INLN int is_float(int t)
{
    int bt;
    bt = t & VT_BTYPE;
    return bt == VT_LDOUBLE || bt == VT_DOUBLE || bt == VT_FLOAT || bt == VT_QFLOAT;
}

/* we use our own 'finite' function to avoid potential problems with
   non standard math libs */
/* XXX: endianness dependent */
ST_FUNC int ieee_finite(double d)
{
    int p[4];
    memcpy(p, &d, sizeof(double));
    return ((unsigned)((p[1] | 0x800fffff) + 1)) >> 31;
}

/* compiling intel long double natively */
#if (defined __i386__ || defined __x86_64__) \
    && (defined TCC_TARGET_I386 || defined TCC_TARGET_X86_64)
# define TCC_IS_NATIVE_387
#endif

ST_FUNC void test_lvalue(void)
{
    if (!(vtop->r & VT_LVAL))
        expect("lvalue");
}

ST_FUNC void check_vstack(void)
{
    if (pvtop != vtop)
        tcc_error("internal compiler error: vstack leak (%d)", vtop - pvtop);
}

/* ------------------------------------------------------------------------- */
/* vstack debugging aid */

#if 0
void pv (const char *lbl, int a, int b)
{
    int i;
    for (i = a; i < a + b; ++i) {
        SValue *p = &vtop[-i];
        printf("%s vtop[-%d] : type.t:%04x  r:%04x  r2:%04x  c.i:%d\n",
            lbl, i, p->type.t, p->r, p->r2, (int)p->c.i);
    }
}
#endif

/* ------------------------------------------------------------------------- */
/* start of translation unit info */
ST_FUNC void tcc_debug_start(TCCState *s1)
{
    if (s1->do_debug) {
        char buf[512];

        /* file info: full path + filename */
        section_sym = put_elf_sym(symtab_section, 0, 0,
                                  ELFW(ST_INFO)(STB_LOCAL, STT_SECTION), 0,
                                  text_section->sh_num, NULL);
        getcwd(buf, sizeof(buf));
#ifdef _WIN32
        normalize_slashes(buf);
#endif
        pstrcat(buf, sizeof(buf), "/");
        put_stabs_r(buf, N_SO, 0, 0,
                    text_section->data_offset, text_section, section_sym);
        put_stabs_r(file->filename, N_SO, 0, 0,
                    text_section->data_offset, text_section, section_sym);
        last_ind = 0;
        last_line_num = 0;
    }

    /* an elf symbol of type STT_FILE must be put so that STB_LOCAL
       symbols can be safely used */
    put_elf_sym(symtab_section, 0, 0,
                ELFW(ST_INFO)(STB_LOCAL, STT_FILE), 0,
                SHN_ABS, file->filename);
}

/* put end of translation unit info */
ST_FUNC void tcc_debug_end(TCCState *s1)
{
    if (!s1->do_debug)
        return;
    put_stabs_r(NULL, N_SO, 0, 0,
        text_section->data_offset, text_section, section_sym);

}

/* generate line number info */
ST_FUNC void tcc_debug_line(TCCState *s1)
{
    if (!s1->do_debug)
        return;
    if ((last_line_num != file->line_num || last_ind != ind)) {
        put_stabn(N_SLINE, 0, file->line_num, ind - func_ind);
        last_ind = ind;
        last_line_num = file->line_num;
    }
}

/* put function symbol */
ST_FUNC void tcc_debug_funcstart(TCCState *s1, Sym *sym)
{
    char buf[512];

    if (!s1->do_debug)
        return;

    /* stabs info */
    /* XXX: we put here a dummy type */
    snprintf(buf, sizeof(buf), "%s:%c1",
             funcname, sym->type.t & VT_STATIC ? 'f' : 'F');
    put_stabs_r(buf, N_FUN, 0, file->line_num, 0,
                cur_text_section, sym->c);
    /* //gr gdb wants a line at the function */
    put_stabn(N_SLINE, 0, file->line_num, 0);

    last_ind = 0;
    last_line_num = 0;
}

/* put function size */
ST_FUNC void tcc_debug_funcend(TCCState *s1, int size)
{
    if (!s1->do_debug)
        return;
    put_stabn(N_FUN, 0, 0, size);
}

/* ------------------------------------------------------------------------- */
ST_FUNC int tccgen_compile(TCCState *s1)
{
    cur_text_section = NULL;
    funcname = "";
    anon_sym = SYM_FIRST_ANOM;
    section_sym = 0;
    const_wanted = 0;
    nocode_wanted = 0x80000000;

    /* define some often used types */
    int_type.t = VT_INT;
    char_pointer_type.t = VT_BYTE;
    mk_pointer(&char_pointer_type);
#if PTR_SIZE == 4
    size_type.t = VT_INT | VT_UNSIGNED;
    ptrdiff_type.t = VT_INT;
#elif LONG_SIZE == 4
    size_type.t = VT_LLONG | VT_UNSIGNED;
    ptrdiff_type.t = VT_LLONG;
#else
    size_type.t = VT_LONG | VT_LLONG | VT_UNSIGNED;
    ptrdiff_type.t = VT_LONG | VT_LLONG;
#endif
    func_old_type.t = VT_FUNC;
    func_old_type.ref = sym_push(SYM_FIELD, &int_type, 0, 0);
    func_old_type.ref->f.func_call = FUNC_CDECL;
    func_old_type.ref->f.func_type = FUNC_OLD;

    tcc_debug_start(s1);

#ifdef TCC_TARGET_ARM
    arm_init(s1);
#endif

#ifdef INC_DEBUG
    printf("%s: **** new file\n", file->filename);
#endif

    parse_flags = PARSE_FLAG_PREPROCESS | PARSE_FLAG_TOK_NUM | PARSE_FLAG_TOK_STR;
    next();
    decl(VT_CONST);
    gen_inline_functions(s1);
    check_vstack();
    /* end of translation unit info */
    tcc_debug_end(s1);
    return 0;
}

/* ------------------------------------------------------------------------- */
ST_FUNC ElfSym *elfsym(Sym *s)
{
  if (!s || !s->c)
    return NULL;
  return &((ElfSym *)symtab_section->data)[s->c];
}

/* apply storage attributes to Elf symbol */
ST_FUNC void update_storage(Sym *sym)
{
    ElfSym *esym;
    int sym_bind, old_sym_bind;

    esym = elfsym(sym);
    if (!esym)
        return;

    if (sym->a.visibility)
        esym->st_other = (esym->st_other & ~ELFW(ST_VISIBILITY)(-1))
            | sym->a.visibility;

    if (sym->type.t & VT_STATIC)
        sym_bind = STB_LOCAL;
    else if (sym->a.weak)
        sym_bind = STB_WEAK;
    else
        sym_bind = STB_GLOBAL;
    old_sym_bind = ELFW(ST_BIND)(esym->st_info);
    if (sym_bind != old_sym_bind) {
        esym->st_info = ELFW(ST_INFO)(sym_bind, ELFW(ST_TYPE)(esym->st_info));
    }

#ifdef TCC_TARGET_PE
    if (sym->a.dllimport)
        esym->st_other |= ST_PE_IMPORT;
    if (sym->a.dllexport)
        esym->st_other |= ST_PE_EXPORT;
#endif

#if 0
    printf("storage %s: bind=%c vis=%d exp=%d imp=%d\n",
        get_tok_str(sym->v, NULL),
        sym_bind == STB_WEAK ? 'w' : sym_bind == STB_LOCAL ? 'l' : 'g',
        sym->a.visibility,
        sym->a.dllexport,
        sym->a.dllimport
        );
#endif
}

/* ------------------------------------------------------------------------- */
/* update sym->c so that it points to an external symbol in section
   'section' with value 'value' */

ST_FUNC void put_extern_sym2(Sym *sym, int sh_num,
                            addr_t value, unsigned long size,
                            int can_add_underscore)
{
    int sym_type, sym_bind, info, other, t;
    ElfSym *esym;
    const char *name;
    char buf1[256];
#ifdef CONFIG_TCC_BCHECK
    char buf[32];
#endif

    if (!sym->c) {
        name = get_tok_str(sym->v, NULL);
#ifdef CONFIG_TCC_BCHECK
        if (tcc_state->do_bounds_check) {
            /* XXX: avoid doing that for statics ? */
            /* if bound checking is activated, we change some function
               names by adding the "__bound" prefix */
            switch(sym->v) {
#ifdef TCC_TARGET_PE
            /* XXX: we rely only on malloc hooks */
            case TOK_malloc:
            case TOK_free:
            case TOK_realloc:
            case TOK_memalign:
            case TOK_calloc:
#endif
            case TOK_memcpy:
            case TOK_memmove:
            case TOK_memset:
            case TOK_strlen:
            case TOK_strcpy:
            case TOK_alloca:
                strcpy(buf, "__bound_");
                strcat(buf, name);
                name = buf;
                break;
            }
        }
#endif
        t = sym->type.t;
        if ((t & VT_BTYPE) == VT_FUNC) {
            sym_type = STT_FUNC;
        } else if ((t & VT_BTYPE) == VT_VOID) {
            sym_type = STT_NOTYPE;
        } else {
            sym_type = STT_OBJECT;
        }
        if (t & VT_STATIC)
            sym_bind = STB_LOCAL;
        else
            sym_bind = STB_GLOBAL;
        other = 0;
#ifdef TCC_TARGET_PE
        if (sym_type == STT_FUNC && sym->type.ref) {
            Sym *ref = sym->type.ref;
            if (ref->f.func_call == FUNC_STDCALL && can_add_underscore) {
                sprintf(buf1, "_%s@%d", name, ref->f.func_args * PTR_SIZE);
                name = buf1;
                other |= ST_PE_STDCALL;
                can_add_underscore = 0;
            }
        }
#endif
        if (tcc_state->leading_underscore && can_add_underscore) {
            buf1[0] = '_';
            pstrcpy(buf1 + 1, sizeof(buf1) - 1, name);
            name = buf1;
        }
        if (sym->asm_label)
            name = get_tok_str(sym->asm_label, NULL);
        info = ELFW(ST_INFO)(sym_bind, sym_type);
        sym->c = put_elf_sym(symtab_section, value, size, info, other, sh_num, name);
    } else {
        esym = elfsym(sym);
        esym->st_value = value;
        esym->st_size = size;
        esym->st_shndx = sh_num;
    }
    update_storage(sym);
}

ST_FUNC void put_extern_sym(Sym *sym, Section *section,
                           addr_t value, unsigned long size)
{
    int sh_num = section ? section->sh_num : SHN_UNDEF;
    put_extern_sym2(sym, sh_num, value, size, 1);
}

/* add a new relocation entry to symbol 'sym' in section 's' */
ST_FUNC void greloca(Section *s, Sym *sym, unsigned long offset, int type,
                     addr_t addend)
{
    int c = 0;

    if (nocode_wanted && s == cur_text_section)
        return;

    if (sym) {
        if (0 == sym->c)
            put_extern_sym(sym, NULL, 0, 0);
        c = sym->c;
    }

    /* now we can add ELF relocation info */
    put_elf_reloca(symtab_section, s, offset, type, c, addend);
}

#if PTR_SIZE == 4
ST_FUNC void greloc(Section *s, Sym *sym, unsigned long offset, int type)
{
    greloca(s, sym, offset, type, 0);
}
#endif

/* ------------------------------------------------------------------------- */
/* symbol allocator */
static Sym *__sym_malloc(void)
{
    Sym *sym_pool, *sym, *last_sym;
    int i;

    sym_pool = tcc_malloc(SYM_POOL_NB * sizeof(Sym));
    dynarray_add(&sym_pools, &nb_sym_pools, sym_pool);

    last_sym = sym_free_first;
    sym = sym_pool;
    for(i = 0; i < SYM_POOL_NB; i++) {
        sym->next = last_sym;
        last_sym = sym;
        sym++;
    }
    sym_free_first = last_sym;
    return last_sym;
}

static inline Sym *sym_malloc(void)
{
    Sym *sym;
#ifndef SYM_DEBUG
    sym = sym_free_first;
    if (!sym)
        sym = __sym_malloc();
    sym_free_first = sym->next;
    return sym;
#else
    sym = tcc_malloc(sizeof(Sym));
    return sym;
#endif
}

ST_INLN void sym_free(Sym *sym)
{
#ifndef SYM_DEBUG
    sym->next = sym_free_first;
    sym_free_first = sym;
#else
    tcc_free(sym);
#endif
}

/* push, without hashing */
ST_FUNC Sym *sym_push2(Sym **ps, int v, int t, int c)
{
    Sym *s;

    s = sym_malloc();
    memset(s, 0, sizeof *s);
    s->v = v;
    s->type.t = t;
    s->c = c;
    /* add in stack */
    s->prev = *ps;
    *ps = s;
    return s;
}

/* find a symbol and return its associated structure. 's' is the top
   of the symbol stack */
ST_FUNC Sym *sym_find2(Sym *s, int v)
{
    while (s) {
        if (s->v == v)
            return s;
        else if (s->v == -1)
            return NULL;
        s = s->prev;
    }
    return NULL;
}

/* structure lookup */
ST_INLN Sym *struct_find(int v)
{
    v -= TOK_IDENT;
    if ((unsigned)v >= (unsigned)(tok_ident - TOK_IDENT))
        return NULL;
    return table_ident[v]->sym_struct;
}

/* find an identifier */
ST_INLN Sym *sym_find(int v)
{
    v -= TOK_IDENT;
    if ((unsigned)v >= (unsigned)(tok_ident - TOK_IDENT))
        return NULL;
    return table_ident[v]->sym_identifier;
}

/* push a given symbol on the symbol stack */
ST_FUNC Sym *sym_push(int v, CType *type, int r, int c)
{
    Sym *s, **ps;
    TokenSym *ts;

    if (local_stack)
        ps = &local_stack;
    else
        ps = &global_stack;
    s = sym_push2(ps, v, type->t, c);
    s->type.ref = type->ref;
    s->r = r;
    /* don't record fields or anonymous symbols */
    /* XXX: simplify */
    if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
        /* record symbol in token array */
        ts = table_ident[(v & ~SYM_STRUCT) - TOK_IDENT];
        if (v & SYM_STRUCT)
            ps = &ts->sym_struct;
        else
            ps = &ts->sym_identifier;
        s->prev_tok = *ps;
        *ps = s;
        s->sym_scope = local_scope;
        if (s->prev_tok && s->prev_tok->sym_scope == s->sym_scope)
            tcc_error("redeclaration of '%s'",
                get_tok_str(v & ~SYM_STRUCT, NULL));
    }
    return s;
}

/* push a global identifier */
ST_FUNC Sym *global_identifier_push(int v, int t, int c)
{
    Sym *s, **ps;
    s = sym_push2(&global_stack, v, t, c);
    /* don't record anonymous symbol */
    if (v < SYM_FIRST_ANOM) {
        ps = &table_ident[v - TOK_IDENT]->sym_identifier;
        /* modify the top most local identifier, so that
           sym_identifier will point to 's' when popped */
        while (*ps != NULL && (*ps)->sym_scope)
            ps = &(*ps)->prev_tok;
        s->prev_tok = *ps;
        *ps = s;
    }
    return s;
}

/* pop symbols until top reaches 'b'.  If KEEP is non-zero don't really
   pop them yet from the list, but do remove them from the token array.  */
ST_FUNC void sym_pop(Sym **ptop, Sym *b, int keep)
{
    Sym *s, *ss, **ps;
    TokenSym *ts;
    int v;

    s = *ptop;
    while(s != b) {
        ss = s->prev;
        v = s->v;
        /* remove symbol in token array */
        /* XXX: simplify */
        if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
            ts = table_ident[(v & ~SYM_STRUCT) - TOK_IDENT];
            if (v & SYM_STRUCT)
                ps = &ts->sym_struct;
            else
                ps = &ts->sym_identifier;
            *ps = s->prev_tok;
        }
	if (!keep)
	    sym_free(s);
        s = ss;
    }
    if (!keep)
	*ptop = b;
}

/* ------------------------------------------------------------------------- */

static void vsetc(CType *type, int r, CValue *vc)
{
    int v;

    if (vtop >= vstack + (VSTACK_SIZE - 1))
        tcc_error("memory full (vstack)");
    /* cannot let cpu flags if other instruction are generated. Also
       avoid leaving VT_JMP anywhere except on the top of the stack
       because it would complicate the code generator.

       Don't do this when nocode_wanted.  vtop might come from
       !nocode_wanted regions (see 88_codeopt.c) and transforming
       it to a register without actually generating code is wrong
       as their value might still be used for real.  All values
       we push under nocode_wanted will eventually be popped
       again, so that the VT_CMP/VT_JMP value will be in vtop
       when code is unsuppressed again.

       Same logic below in vswap(); */
    if (vtop >= vstack && !nocode_wanted) {
        v = vtop->r & VT_VALMASK;
        if (v == VT_CMP || (v & ~1) == VT_JMP)
            gv(RC_INT);
    }

    vtop++;
    vtop->type = *type;
    vtop->r = r;
    vtop->r2 = VT_CONST;
    vtop->c = *vc;
    vtop->sym = NULL;
}

ST_FUNC void vswap(void)
{
    SValue tmp;
    /* cannot vswap cpu flags. See comment at vsetc() above */
    if (vtop >= vstack && !nocode_wanted) {
        int v = vtop->r & VT_VALMASK;
        if (v == VT_CMP || (v & ~1) == VT_JMP)
            gv(RC_INT);
    }
    tmp = vtop[0];
    vtop[0] = vtop[-1];
    vtop[-1] = tmp;
}

/* pop stack value */
ST_FUNC void vpop(void)
{
    int v;
    v = vtop->r & VT_VALMASK;
#if defined(TCC_TARGET_I386) || defined(TCC_TARGET_X86_64)
    /* for x86, we need to pop the FP stack */
    if (v == TREG_ST0) {
        o(0xd8dd); /* fstp %st(0) */
    } else
#endif
    if (v == VT_JMP || v == VT_JMPI) {
        /* need to put correct jump if && or || without test */
        gsym(vtop->c.i);
    }
    vtop--;
}

/* push constant of type "type" with useless value */
ST_FUNC void vpush(CType *type)
{
    vset(type, VT_CONST, 0);
}

/* push integer constant */
ST_FUNC void vpushi(int v)
{
    CValue cval;
    cval.i = v;
    vsetc(&int_type, VT_CONST, &cval);
}

/* push a pointer sized constant */
static void vpushs(addr_t v)
{
  CValue cval;
  cval.i = v;
  vsetc(&size_type, VT_CONST, &cval);
}

/* push arbitrary 64bit constant */
ST_FUNC void vpush64(int ty, unsigned long long v)
{
    CValue cval;
    CType ctype;
    ctype.t = ty;
    ctype.ref = NULL;
    cval.i = v;
    vsetc(&ctype, VT_CONST, &cval);
}

/* push long long constant */
static inline void vpushll(long long v)
{
    vpush64(VT_LLONG, v);
}

ST_FUNC void vset(CType *type, int r, int v)
{
    CValue cval;

    cval.i = v;
    vsetc(type, r, &cval);
}

static void vseti(int r, int v)
{
    CType type;
    type.t = VT_INT;
    type.ref = NULL;
    vset(&type, r, v);
}

ST_FUNC void vpushv(SValue *v)
{
    if (vtop >= vstack + (VSTACK_SIZE - 1))
        tcc_error("memory full (vstack)");
    vtop++;
    *vtop = *v;
}

static void vdup(void)
{
    vpushv(vtop);
}

/* rotate n first stack elements to the bottom
   I1 ... In -> I2 ... In I1 [top is right]
*/
ST_FUNC void vrotb(int n)
{
    int i;
    SValue tmp;

    tmp = vtop[-n + 1];
    for(i=-n+1;i!=0;i++)
        vtop[i] = vtop[i+1];
    vtop[0] = tmp;
}

/* rotate the n elements before entry e towards the top
   I1 ... In ... -> In I1 ... I(n-1) ... [top is right]
 */
ST_FUNC void vrote(SValue *e, int n)
{
    int i;
    SValue tmp;

    tmp = *e;
    for(i = 0;i < n - 1; i++)
        e[-i] = e[-i - 1];
    e[-n + 1] = tmp;
}

/* rotate n first stack elements to the top
   I1 ... In -> In I1 ... I(n-1)  [top is right]
 */
ST_FUNC void vrott(int n)
{
    vrote(vtop, n);
}

/* push a symbol value of TYPE */
static inline void vpushsym(CType *type, Sym *sym)
{
    CValue cval;
    cval.i = 0;
    vsetc(type, VT_CONST | VT_SYM, &cval);
    vtop->sym = sym;
}

/* Return a static symbol pointing to a section */
ST_FUNC Sym *get_sym_ref(CType *type, Section *sec, unsigned long offset, unsigned long size)
{
    int v;
    Sym *sym;

    v = anon_sym++;
    sym = global_identifier_push(v, type->t | VT_STATIC, 0);
    sym->type.ref = type->ref;
    sym->r = VT_CONST | VT_SYM;
    put_extern_sym(sym, sec, offset, size);
    return sym;
}

/* push a reference to a section offset by adding a dummy symbol */
static void vpush_ref(CType *type, Section *sec, unsigned long offset, unsigned long size)
{
    vpushsym(type, get_sym_ref(type, sec, offset, size));  
}

/* define a new external reference to a symbol 'v' of type 'u' */
ST_FUNC Sym *external_global_sym(int v, CType *type, int r)
{
    Sym *s;

    s = sym_find(v);
    if (!s) {
        /* push forward reference */
        s = global_identifier_push(v, type->t | VT_EXTERN, 0);
        s->type.ref = type->ref;
        s->r = r | VT_CONST | VT_SYM;
    } else if (IS_ASM_SYM(s)) {
        s->type.t = type->t | (s->type.t & VT_EXTERN);
        s->type.ref = type->ref;
        update_storage(s);
    }
    return s;
}

/* Merge some type attributes.  */
static void patch_type(Sym *sym, CType *type)
{
    if (!(type->t & VT_EXTERN)) {
        if (!(sym->type.t & VT_EXTERN))
            tcc_error("redefinition of '%s'", get_tok_str(sym->v, NULL));
        sym->type.t &= ~VT_EXTERN;
    }

    if (IS_ASM_SYM(sym)) {
        /* stay static if both are static */
        sym->type.t = type->t & (sym->type.t | ~VT_STATIC);
        sym->type.ref = type->ref;
    }

    if (!is_compatible_types(&sym->type, type)) {
        tcc_error("incompatible types for redefinition of '%s'",
                  get_tok_str(sym->v, NULL));

    } else if ((sym->type.t & VT_BTYPE) == VT_FUNC) {
        int static_proto = sym->type.t & VT_STATIC;
        /* warn if static follows non-static function declaration */
        if ((type->t & VT_STATIC) && !static_proto && !(type->t & VT_INLINE))
            tcc_warning("static storage ignored for redefinition of '%s'",
                get_tok_str(sym->v, NULL));

        if (0 == (type->t & VT_EXTERN)) {
            /* put complete type, use static from prototype */
            sym->type.t = (type->t & ~VT_STATIC) | static_proto;
            if (type->t & VT_INLINE)
                sym->type.t = type->t;
            sym->type.ref = type->ref;
        }

    } else {
        if ((sym->type.t & VT_ARRAY) && type->ref->c >= 0) {
            /* set array size if it was omitted in extern declaration */
            if (sym->type.ref->c < 0)
                sym->type.ref->c = type->ref->c;
            else if (sym->type.ref->c != type->ref->c)
                tcc_error("conflicting type for '%s'", get_tok_str(sym->v, NULL));
        }
        if ((type->t ^ sym->type.t) & VT_STATIC)
            tcc_warning("storage mismatch for redefinition of '%s'",
                get_tok_str(sym->v, NULL));
    }
}


/* Merge some storage attributes.  */
static void patch_storage(Sym *sym, AttributeDef *ad, CType *type)
{
    if (type)
        patch_type(sym, type);

#ifdef TCC_TARGET_PE
    if (sym->a.dllimport != ad->a.dllimport)
        tcc_error("incompatible dll linkage for redefinition of '%s'",
            get_tok_str(sym->v, NULL));
    sym->a.dllexport |= ad->a.dllexport;
#endif
    sym->a.weak |= ad->a.weak;
    if (ad->a.visibility) {
        int vis = sym->a.visibility;
        int vis2 = ad->a.visibility;
        if (vis == STV_DEFAULT)
            vis = vis2;
        else if (vis2 != STV_DEFAULT)
            vis = (vis < vis2) ? vis : vis2;
        sym->a.visibility = vis;
    }
    if (ad->a.aligned)
        sym->a.aligned = ad->a.aligned;
    if (ad->asm_label)
        sym->asm_label = ad->asm_label;
    update_storage(sym);
}

/* define a new external reference to a symbol 'v' */
static Sym *external_sym(int v, CType *type, int r, AttributeDef *ad)
{
    Sym *s;
    s = sym_find(v);
    if (!s) {
        /* push forward reference */
        s = sym_push(v, type, r | VT_CONST | VT_SYM, 0);
        s->type.t |= VT_EXTERN;
        s->a = ad->a;
        s->sym_scope = 0;
    } else {
        if (s->type.ref == func_old_type.ref) {
            s->type.ref = type->ref;
            s->r = r | VT_CONST | VT_SYM;
            s->type.t |= VT_EXTERN;
        }
        patch_storage(s, ad, type);
    }
    return s;
}

/* push a reference to global symbol v */
ST_FUNC void vpush_global_sym(CType *type, int v)
{
    vpushsym(type, external_global_sym(v, type, 0));
}

/* save registers up to (vtop - n) stack entry */
ST_FUNC void save_regs(int n)
{
    SValue *p, *p1;
    for(p = vstack, p1 = vtop - n; p <= p1; p++)
        save_reg(p->r);
}

/* save r to the memory stack, and mark it as being free */
ST_FUNC void save_reg(int r)
{
    save_reg_upstack(r, 0);
}

/* save r to the memory stack, and mark it as being free,
   if seen up to (vtop - n) stack entry */
ST_FUNC void save_reg_upstack(int r, int n)
{
    int l, saved, size, align;
    SValue *p, *p1, sv;
    CType *type;

    if ((r &= VT_VALMASK) >= VT_CONST)
        return;
    if (nocode_wanted)
        return;

    /* modify all stack values */
    saved = 0;
    l = 0;
    for(p = vstack, p1 = vtop - n; p <= p1; p++) {
        if ((p->r & VT_VALMASK) == r ||
            ((p->type.t & VT_BTYPE) == VT_LLONG && (p->r2 & VT_VALMASK) == r)) {
            /* must save value on stack if not already done */
            if (!saved) {
                /* NOTE: must reload 'r' because r might be equal to r2 */
                r = p->r & VT_VALMASK;
                /* store register in the stack */
                type = &p->type;
                if ((p->r & VT_LVAL) ||
                    (!is_float(type->t) && (type->t & VT_BTYPE) != VT_LLONG))
#if PTR_SIZE == 8
                    type = &char_pointer_type;
#else
                    type = &int_type;
#endif
                size = type_size(type, &align);
                loc = (loc - size) & -align;
                sv.type.t = type->t;
                sv.r = VT_LOCAL | VT_LVAL;
                sv.c.i = loc;
                store(r, &sv);
#if defined(TCC_TARGET_I386) || defined(TCC_TARGET_X86_64)
                /* x86 specific: need to pop fp register ST0 if saved */
                if (r == TREG_ST0) {
                    o(0xd8dd); /* fstp %st(0) */
                }
#endif
#if PTR_SIZE == 4
                /* special long long case */
                if ((type->t & VT_BTYPE) == VT_LLONG) {
                    sv.c.i += 4;
                    store(p->r2, &sv);
                }
#endif
                l = loc;
                saved = 1;
            }
            /* mark that stack entry as being saved on the stack */
            if (p->r & VT_LVAL) {
                /* also clear the bounded flag because the
                   relocation address of the function was stored in
                   p->c.i */
                p->r = (p->r & ~(VT_VALMASK | VT_BOUNDED)) | VT_LLOCAL;
            } else {
                p->r = lvalue_type(p->type.t) | VT_LOCAL;
            }
            p->r2 = VT_CONST;
            p->c.i = l;
        }
    }
}

#ifdef TCC_TARGET_ARM
/* find a register of class 'rc2' with at most one reference on stack.
 * If none, call get_reg(rc) */
ST_FUNC int get_reg_ex(int rc, int rc2)
{
    int r;
    SValue *p;
    
    for(r=0;r<NB_REGS;r++) {
        if (reg_classes[r] & rc2) {
            int n;
            n=0;
            for(p = vstack; p <= vtop; p++) {
                if ((p->r & VT_VALMASK) == r ||
                    (p->r2 & VT_VALMASK) == r)
                    n++;
            }
            if (n <= 1)
                return r;
        }
    }
    return get_reg(rc);
}
#endif

/* find a free register of class 'rc'. If none, save one register */
ST_FUNC int get_reg(int rc)
{
    int r;
    SValue *p;

    /* find a free register */
    for(r=0;r<NB_REGS;r++) {
        if (reg_classes[r] & rc) {
            if (nocode_wanted)
                return r;
            for(p=vstack;p<=vtop;p++) {
                if ((p->r & VT_VALMASK) == r ||
                    (p->r2 & VT_VALMASK) == r)
                    goto notfound;
            }
            return r;
        }
    notfound: ;
    }
    
    /* no register left : free the first one on the stack (VERY
       IMPORTANT to start from the bottom to ensure that we don't
       spill registers used in gen_opi()) */
    for(p=vstack;p<=vtop;p++) {
        /* look at second register (if long long) */
        r = p->r2 & VT_VALMASK;
        if (r < VT_CONST && (reg_classes[r] & rc))
            goto save_found;
        r = p->r & VT_VALMASK;
        if (r < VT_CONST && (reg_classes[r] & rc)) {
        save_found:
            save_reg(r);
            return r;
        }
    }
    /* Should never comes here */
    return -1;
}

/* move register 's' (of type 't') to 'r', and flush previous value of r to memory
   if needed */
static void move_reg(int r, int s, int t)
{
    SValue sv;

    if (r != s) {
        save_reg(r);
        sv.type.t = t;
        sv.type.ref = NULL;
        sv.r = s;
        sv.c.i = 0;
        load(r, &sv);
    }
}

/* get address of vtop (vtop MUST BE an lvalue) */
ST_FUNC void gaddrof(void)
{
    vtop->r &= ~VT_LVAL;
    /* tricky: if saved lvalue, then we can go back to lvalue */
    if ((vtop->r & VT_VALMASK) == VT_LLOCAL)
        vtop->r = (vtop->r & ~(VT_VALMASK | VT_LVAL_TYPE)) | VT_LOCAL | VT_LVAL;


}

#ifdef CONFIG_TCC_BCHECK
/* generate lvalue bound code */
static void gbound(void)
{
    int lval_type;
    CType type1;

    vtop->r &= ~VT_MUSTBOUND;
    /* if lvalue, then use checking code before dereferencing */
    if (vtop->r & VT_LVAL) {
        /* if not VT_BOUNDED value, then make one */
        if (!(vtop->r & VT_BOUNDED)) {
            lval_type = vtop->r & (VT_LVAL_TYPE | VT_LVAL);
            /* must save type because we must set it to int to get pointer */
            type1 = vtop->type;
            vtop->type.t = VT_PTR;
            gaddrof();
            vpushi(0);
            gen_bounded_ptr_add();
            vtop->r |= lval_type;
            vtop->type = type1;
        }
        /* then check for dereferencing */
        gen_bounded_ptr_deref();
    }
}
#endif

static void incr_bf_adr(int o)
{
    vtop->type = char_pointer_type;
    gaddrof();
    vpushi(o);
    gen_op('+');
    vtop->type.t = (vtop->type.t & ~(VT_BTYPE|VT_DEFSIGN))
        | (VT_BYTE|VT_UNSIGNED);
    vtop->r = (vtop->r & ~VT_LVAL_TYPE)
        | (VT_LVAL_BYTE|VT_LVAL_UNSIGNED|VT_LVAL);
}

/* single-byte load mode for packed or otherwise unaligned bitfields */
static void load_packed_bf(CType *type, int bit_pos, int bit_size)
{
    int n, o, bits;
    save_reg_upstack(vtop->r, 1);
    vpush64(type->t & VT_BTYPE, 0); // B X
    bits = 0, o = bit_pos >> 3, bit_pos &= 7;
    do {
        vswap(); // X B
        incr_bf_adr(o);
        vdup(); // X B B
        n = 8 - bit_pos;
        if (n > bit_size)
            n = bit_size;
        if (bit_pos)
            vpushi(bit_pos), gen_op(TOK_SHR), bit_pos = 0; // X B Y
        if (n < 8)
            vpushi((1 << n) - 1), gen_op('&');
        gen_cast(type);
        if (bits)
            vpushi(bits), gen_op(TOK_SHL);
        vrotb(3); // B Y X
        gen_op('|'); // B X
        bits += n, bit_size -= n, o = 1;
    } while (bit_size);
    vswap(), vpop();
    if (!(type->t & VT_UNSIGNED)) {
        n = ((type->t & VT_BTYPE) == VT_LLONG ? 64 : 32) - bits;
        vpushi(n), gen_op(TOK_SHL);
        vpushi(n), gen_op(TOK_SAR);
    }
}

/* single-byte store mode for packed or otherwise unaligned bitfields */
static void store_packed_bf(int bit_pos, int bit_size)
{
    int bits, n, o, m, c;

    c = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    vswap(); // X B
    save_reg_upstack(vtop->r, 1);
    bits = 0, o = bit_pos >> 3, bit_pos &= 7;
    do {
        incr_bf_adr(o); // X B
        vswap(); //B X
        c ? vdup() : gv_dup(); // B V X
        vrott(3); // X B V
        if (bits)
            vpushi(bits), gen_op(TOK_SHR);
        if (bit_pos)
            vpushi(bit_pos), gen_op(TOK_SHL);
        n = 8 - bit_pos;
        if (n > bit_size)
            n = bit_size;
        if (n < 8) {
            m = ((1 << n) - 1) << bit_pos;
            vpushi(m), gen_op('&'); // X B V1
            vpushv(vtop-1); // X B V1 B
            vpushi(m & 0x80 ? ~m & 0x7f : ~m);
            gen_op('&'); // X B V1 B1
            gen_op('|'); // X B V2
        }
        vdup(), vtop[-1] = vtop[-2]; // X B B V2
        vstore(), vpop(); // X B
        bits += n, bit_size -= n, bit_pos = 0, o = 1;
    } while (bit_size);
    vpop(), vpop();
}

static int adjust_bf(SValue *sv, int bit_pos, int bit_size)
{
    int t;
    if (0 == sv->type.ref)
        return 0;
    t = sv->type.ref->auxtype;
    if (t != -1 && t != VT_STRUCT) {
        sv->type.t = (sv->type.t & ~VT_BTYPE) | t;
        sv->r = (sv->r & ~VT_LVAL_TYPE) | lvalue_type(sv->type.t);
    }
    return t;
}

/* store vtop a register belonging to class 'rc'. lvalues are
   converted to values. Cannot be used if cannot be converted to
   register value (such as structures). */
ST_FUNC int gv(int rc)
{
    int r, bit_pos, bit_size, size, align, rc2;

    /* NOTE: get_reg can modify vstack[] */
    if (vtop->type.t & VT_BITFIELD) {
        CType type;

        bit_pos = BIT_POS(vtop->type.t);
        bit_size = BIT_SIZE(vtop->type.t);
        /* remove bit field info to avoid loops */
        vtop->type.t &= ~VT_STRUCT_MASK;

        type.ref = NULL;
        type.t = vtop->type.t & VT_UNSIGNED;
        if ((vtop->type.t & VT_BTYPE) == VT_BOOL)
            type.t |= VT_UNSIGNED;

        r = adjust_bf(vtop, bit_pos, bit_size);

        if ((vtop->type.t & VT_BTYPE) == VT_LLONG)
            type.t |= VT_LLONG;
        else
            type.t |= VT_INT;

        if (r == VT_STRUCT) {
            load_packed_bf(&type, bit_pos, bit_size);
        } else {
            int bits = (type.t & VT_BTYPE) == VT_LLONG ? 64 : 32;
            /* cast to int to propagate signedness in following ops */
            gen_cast(&type);
            /* generate shifts */
            vpushi(bits - (bit_pos + bit_size));
            gen_op(TOK_SHL);
            vpushi(bits - bit_size);
            /* NOTE: transformed to SHR if unsigned */
            gen_op(TOK_SAR);
        }
        r = gv(rc);
    } else {
        if (is_float(vtop->type.t) && 
            (vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
            unsigned long offset;
            /* CPUs usually cannot use float constants, so we store them
               generically in data segment */
            size = type_size(&vtop->type, &align);
            if (NODATA_WANTED)
                size = 0, align = 1;
            offset = section_add(data_section, size, align);
            vpush_ref(&vtop->type, data_section, offset, size);
	    vswap();
	    init_putv(&vtop->type, data_section, offset);
	    vtop->r |= VT_LVAL;
        }
#ifdef CONFIG_TCC_BCHECK
        if (vtop->r & VT_MUSTBOUND) 
            gbound();
#endif

        r = vtop->r & VT_VALMASK;
        rc2 = (rc & RC_FLOAT) ? RC_FLOAT : RC_INT;
#ifndef TCC_TARGET_ARM64
        if (rc == RC_IRET)
            rc2 = RC_LRET;
#ifdef TCC_TARGET_X86_64
        else if (rc == RC_FRET)
            rc2 = RC_QRET;
#endif
#endif
        /* need to reload if:
           - constant
           - lvalue (need to dereference pointer)
           - already a register, but not in the right class */
        if (r >= VT_CONST
         || (vtop->r & VT_LVAL)
         || !(reg_classes[r] & rc)
#if PTR_SIZE == 8
         || ((vtop->type.t & VT_BTYPE) == VT_QLONG && !(reg_classes[vtop->r2] & rc2))
         || ((vtop->type.t & VT_BTYPE) == VT_QFLOAT && !(reg_classes[vtop->r2] & rc2))
#else
         || ((vtop->type.t & VT_BTYPE) == VT_LLONG && !(reg_classes[vtop->r2] & rc2))
#endif
            )
        {
            r = get_reg(rc);
#if PTR_SIZE == 8
            if (((vtop->type.t & VT_BTYPE) == VT_QLONG) || ((vtop->type.t & VT_BTYPE) == VT_QFLOAT)) {
                int addr_type = VT_LLONG, load_size = 8, load_type = ((vtop->type.t & VT_BTYPE) == VT_QLONG) ? VT_LLONG : VT_DOUBLE;
#else
            if ((vtop->type.t & VT_BTYPE) == VT_LLONG) {
                int addr_type = VT_INT, load_size = 4, load_type = VT_INT;
                unsigned long long ll;
#endif
                int r2, original_type;
                original_type = vtop->type.t;
                /* two register type load : expand to two words
                   temporarily */
#if PTR_SIZE == 4
                if ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
                    /* load constant */
                    ll = vtop->c.i;
                    vtop->c.i = ll; /* first word */
                    load(r, vtop);
                    vtop->r = r; /* save register value */
                    vpushi(ll >> 32); /* second word */
                } else
#endif
                if (vtop->r & VT_LVAL) {
                    /* We do not want to modifier the long long
                       pointer here, so the safest (and less
                       efficient) is to save all the other registers
                       in the stack. XXX: totally inefficient. */
               #if 0
                    save_regs(1);
               #else
                    /* lvalue_save: save only if used further down the stack */
                    save_reg_upstack(vtop->r, 1);
               #endif
                    /* load from memory */
                    vtop->type.t = load_type;
                    load(r, vtop);
                    vdup();
                    vtop[-1].r = r; /* save register value */
                    /* increment pointer to get second word */
                    vtop->type.t = addr_type;
                    gaddrof();
                    vpushi(load_size);
                    gen_op('+');
                    vtop->r |= VT_LVAL;
                    vtop->type.t = load_type;
                } else {
                    /* move registers */
                    load(r, vtop);
                    vdup();
                    vtop[-1].r = r; /* save register value */
                    vtop->r = vtop[-1].r2;
                }
                /* Allocate second register. Here we rely on the fact that
                   get_reg() tries first to free r2 of an SValue. */
                r2 = get_reg(rc2);
                load(r2, vtop);
                vpop();
                /* write second register */
                vtop->r2 = r2;
                vtop->type.t = original_type;
            } else if ((vtop->r & VT_LVAL) && !is_float(vtop->type.t)) {
                int t1, t;
                /* lvalue of scalar type : need to use lvalue type
                   because of possible cast */
                t = vtop->type.t;
                t1 = t;
                /* compute memory access type */
                if (vtop->r & VT_LVAL_BYTE)
                    t = VT_BYTE;
                else if (vtop->r & VT_LVAL_SHORT)
                    t = VT_SHORT;
                if (vtop->r & VT_LVAL_UNSIGNED)
                    t |= VT_UNSIGNED;
                vtop->type.t = t;
                load(r, vtop);
                /* restore wanted type */
                vtop->type.t = t1;
            } else {
                /* one register type load */
                load(r, vtop);
            }
        }
        vtop->r = r;
#ifdef TCC_TARGET_C67
        /* uses register pairs for doubles */
        if ((vtop->type.t & VT_BTYPE) == VT_DOUBLE) 
            vtop->r2 = r+1;
#endif
    }
    return r;
}

/* generate vtop[-1] and vtop[0] in resp. classes rc1 and rc2 */
ST_FUNC void gv2(int rc1, int rc2)
{
    int v;

    /* generate more generic register first. But VT_JMP or VT_CMP
       values must be generated first in all cases to avoid possible
       reload errors */
    v = vtop[0].r & VT_VALMASK;
    if (v != VT_CMP && (v & ~1) != VT_JMP && rc1 <= rc2) {
        vswap();
        gv(rc1);
        vswap();
        gv(rc2);
        /* test if reload is needed for first register */
        if ((vtop[-1].r & VT_VALMASK) >= VT_CONST) {
            vswap();
            gv(rc1);
            vswap();
        }
    } else {
        gv(rc2);
        vswap();
        gv(rc1);
        vswap();
        /* test if reload is needed for first register */
        if ((vtop[0].r & VT_VALMASK) >= VT_CONST) {
            gv(rc2);
        }
    }
}

#ifndef TCC_TARGET_ARM64
/* wrapper around RC_FRET to return a register by type */
static int rc_fret(int t)
{
#ifdef TCC_TARGET_X86_64
    if (t == VT_LDOUBLE) {
        return RC_ST0;
    }
#endif
    return RC_FRET;
}
#endif

/* wrapper around REG_FRET to return a register by type */
static int reg_fret(int t)
{
#ifdef TCC_TARGET_X86_64
    if (t == VT_LDOUBLE) {
        return TREG_ST0;
    }
#endif
    return REG_FRET;
}

#if PTR_SIZE == 4
/* expand 64bit on stack in two ints */
static void lexpand(void)
{
    int u, v;
    u = vtop->type.t & (VT_DEFSIGN | VT_UNSIGNED);
    v = vtop->r & (VT_VALMASK | VT_LVAL);
    if (v == VT_CONST) {
        vdup();
        vtop[0].c.i >>= 32;
    } else if (v == (VT_LVAL|VT_CONST) || v == (VT_LVAL|VT_LOCAL)) {
        vdup();
        vtop[0].c.i += 4;
    } else {
        gv(RC_INT);
        vdup();
        vtop[0].r = vtop[-1].r2;
        vtop[0].r2 = vtop[-1].r2 = VT_CONST;
    }
    vtop[0].type.t = vtop[-1].type.t = VT_INT | u;
}
#endif

#ifdef TCC_TARGET_ARM
/* expand long long on stack */
ST_FUNC void lexpand_nr(void)
{
    int u,v;

    u = vtop->type.t & (VT_DEFSIGN | VT_UNSIGNED);
    vdup();
    vtop->r2 = VT_CONST;
    vtop->type.t = VT_INT | u;
    v=vtop[-1].r & (VT_VALMASK | VT_LVAL);
    if (v == VT_CONST) {
      vtop[-1].c.i = vtop->c.i;
      vtop->c.i = vtop->c.i >> 32;
      vtop->r = VT_CONST;
    } else if (v == (VT_LVAL|VT_CONST) || v == (VT_LVAL|VT_LOCAL)) {
      vtop->c.i += 4;
      vtop->r = vtop[-1].r;
    } else if (v > VT_CONST) {
      vtop--;
      lexpand();
    } else
      vtop->r = vtop[-1].r2;
    vtop[-1].r2 = VT_CONST;
    vtop[-1].type.t = VT_INT | u;
}
#endif

#if PTR_SIZE == 4
/* build a long long from two ints */
static void lbuild(int t)
{
    gv2(RC_INT, RC_INT);
    vtop[-1].r2 = vtop[0].r;
    vtop[-1].type.t = t;
    vpop();
}
#endif

/* convert stack entry to register and duplicate its value in another
   register */
static void gv_dup(void)
{
    int rc, t, r, r1;
    SValue sv;

    t = vtop->type.t;
#if PTR_SIZE == 4
    if ((t & VT_BTYPE) == VT_LLONG) {
        if (t & VT_BITFIELD) {
            gv(RC_INT);
            t = vtop->type.t;
        }
        lexpand();
        gv_dup();
        vswap();
        vrotb(3);
        gv_dup();
        vrotb(4);
        /* stack: H L L1 H1 */
        lbuild(t);
        vrotb(3);
        vrotb(3);
        vswap();
        lbuild(t);
        vswap();
    } else
#endif
    {
        /* duplicate value */
        rc = RC_INT;
        sv.type.t = VT_INT;
        if (is_float(t)) {
            rc = RC_FLOAT;
#ifdef TCC_TARGET_X86_64
            if ((t & VT_BTYPE) == VT_LDOUBLE) {
                rc = RC_ST0;
            }
#endif
            sv.type.t = t;
        }
        r = gv(rc);
        r1 = get_reg(rc);
        sv.r = r;
        sv.c.i = 0;
        load(r1, &sv); /* move r to r1 */
        vdup();
        /* duplicates value */
        if (r != r1)
            vtop->r = r1;
    }
}

/* Generate value test
 *
 * Generate a test for any value (jump, comparison and integers) */
ST_FUNC int gvtst(int inv, int t)
{
    int v = vtop->r & VT_VALMASK;
    if (v != VT_CMP && v != VT_JMP && v != VT_JMPI) {
        vpushi(0);
        gen_op(TOK_NE);
    }
    if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
        /* constant jmp optimization */
        if ((vtop->c.i != 0) != inv)
            t = gjmp(t);
        vtop--;
        return t;
    }
    return gtst(inv, t);
}

#if PTR_SIZE == 4
/* generate CPU independent (unsigned) long long operations */
static void gen_opl(int op)
{
    int t, a, b, op1, c, i;
    int func;
    unsigned short reg_iret = REG_IRET;
    unsigned short reg_lret = REG_LRET;
    SValue tmp;

    switch(op) {
    case '/':
    case TOK_PDIV:
        func = TOK___divdi3;
        goto gen_func;
    case TOK_UDIV:
        func = TOK___udivdi3;
        goto gen_func;
    case '%':
        func = TOK___moddi3;
        goto gen_mod_func;
    case TOK_UMOD:
        func = TOK___umoddi3;
    gen_mod_func:
#ifdef TCC_ARM_EABI
        reg_iret = TREG_R2;
        reg_lret = TREG_R3;
#endif
    gen_func:
        /* call generic long long function */
        vpush_global_sym(&func_old_type, func);
        vrott(3);
        gfunc_call(2);
        vpushi(0);
        vtop->r = reg_iret;
        vtop->r2 = reg_lret;
        break;
    case '^':
    case '&':
    case '|':
    case '*':
    case '+':
    case '-':
        //pv("gen_opl A",0,2);
        t = vtop->type.t;
        vswap();
        lexpand();
        vrotb(3);
        lexpand();
        /* stack: L1 H1 L2 H2 */
        tmp = vtop[0];
        vtop[0] = vtop[-3];
        vtop[-3] = tmp;
        tmp = vtop[-2];
        vtop[-2] = vtop[-3];
        vtop[-3] = tmp;
        vswap();
        /* stack: H1 H2 L1 L2 */
        //pv("gen_opl B",0,4);
        if (op == '*') {
            vpushv(vtop - 1);
            vpushv(vtop - 1);
            gen_op(TOK_UMULL);
            lexpand();
            /* stack: H1 H2 L1 L2 ML MH */
            for(i=0;i<4;i++)
                vrotb(6);
            /* stack: ML MH H1 H2 L1 L2 */
            tmp = vtop[0];
            vtop[0] = vtop[-2];
            vtop[-2] = tmp;
            /* stack: ML MH H1 L2 H2 L1 */
            gen_op('*');
            vrotb(3);
            vrotb(3);
            gen_op('*');
            /* stack: ML MH M1 M2 */
            gen_op('+');
            gen_op('+');
        } else if (op == '+' || op == '-') {
            /* XXX: add non carry method too (for MIPS or alpha) */
            if (op == '+')
                op1 = TOK_ADDC1;
            else
                op1 = TOK_SUBC1;
            gen_op(op1);
            /* stack: H1 H2 (L1 op L2) */
            vrotb(3);
            vrotb(3);
            gen_op(op1 + 1); /* TOK_xxxC2 */
        } else {
            gen_op(op);
            /* stack: H1 H2 (L1 op L2) */
            vrotb(3);
            vrotb(3);
            /* stack: (L1 op L2) H1 H2 */
            gen_op(op);
            /* stack: (L1 op L2) (H1 op H2) */
        }
        /* stack: L H */
        lbuild(t);
        break;
    case TOK_SAR:
    case TOK_SHR:
    case TOK_SHL:
        if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
            t = vtop[-1].type.t;
            vswap();
            lexpand();
            vrotb(3);
            /* stack: L H shift */
            c = (int)vtop->c.i;
            /* constant: simpler */
            /* NOTE: all comments are for SHL. the other cases are
               done by swapping words */
            vpop();
            if (op != TOK_SHL)
                vswap();
            if (c >= 32) {
                /* stack: L H */
                vpop();
                if (c > 32) {
                    vpushi(c - 32);
                    gen_op(op);
                }
                if (op != TOK_SAR) {
                    vpushi(0);
                } else {
                    gv_dup();
                    vpushi(31);
                    gen_op(TOK_SAR);
                }
                vswap();
            } else {
                vswap();
                gv_dup();
                /* stack: H L L */
                vpushi(c);
                gen_op(op);
                vswap();
                vpushi(32 - c);
                if (op == TOK_SHL)
                    gen_op(TOK_SHR);
                else
                    gen_op(TOK_SHL);
                vrotb(3);
                /* stack: L L H */
                vpushi(c);
                if (op == TOK_SHL)
                    gen_op(TOK_SHL);
                else
                    gen_op(TOK_SHR);
                gen_op('|');
            }
            if (op != TOK_SHL)
                vswap();
            lbuild(t);
        } else {
            /* XXX: should provide a faster fallback on x86 ? */
            switch(op) {
            case TOK_SAR:
                func = TOK___ashrdi3;
                goto gen_func;
            case TOK_SHR:
                func = TOK___lshrdi3;
                goto gen_func;
            case TOK_SHL:
                func = TOK___ashldi3;
                goto gen_func;
            }
        }
        break;
    default:
        /* compare operations */
        t = vtop->type.t;
        vswap();
        lexpand();
        vrotb(3);
        lexpand();
        /* stack: L1 H1 L2 H2 */
        tmp = vtop[-1];
        vtop[-1] = vtop[-2];
        vtop[-2] = tmp;
        /* stack: L1 L2 H1 H2 */
        /* compare high */
        op1 = op;
        /* when values are equal, we need to compare low words. since
           the jump is inverted, we invert the test too. */
        if (op1 == TOK_LT)
            op1 = TOK_LE;
        else if (op1 == TOK_GT)
            op1 = TOK_GE;
        else if (op1 == TOK_ULT)
            op1 = TOK_ULE;
        else if (op1 == TOK_UGT)
            op1 = TOK_UGE;
        a = 0;
        b = 0;
        gen_op(op1);
        if (op == TOK_NE) {
            b = gvtst(0, 0);
        } else {
            a = gvtst(1, 0);
            if (op != TOK_EQ) {
                /* generate non equal test */
                vpushi(TOK_NE);
                vtop->r = VT_CMP;
                b = gvtst(0, 0);
            }
        }
        /* compare low. Always unsigned */
        op1 = op;
        if (op1 == TOK_LT)
            op1 = TOK_ULT;
        else if (op1 == TOK_LE)
            op1 = TOK_ULE;
        else if (op1 == TOK_GT)
            op1 = TOK_UGT;
        else if (op1 == TOK_GE)
            op1 = TOK_UGE;
        gen_op(op1);
        a = gvtst(1, a);
        gsym(b);
        vseti(VT_JMPI, a);
        break;
    }
}
#endif

static uint64_t gen_opic_sdiv(uint64_t a, uint64_t b)
{
    uint64_t x = (a >> 63 ? -a : a) / (b >> 63 ? -b : b);
    return (a ^ b) >> 63 ? -x : x;
}

static int gen_opic_lt(uint64_t a, uint64_t b)
{
    return (a ^ (uint64_t)1 << 63) < (b ^ (uint64_t)1 << 63);
}

/* handle integer constant optimizations and various machine
   independent opt */
static void gen_opic(int op)
{
    SValue *v1 = vtop - 1;
    SValue *v2 = vtop;
    int t1 = v1->type.t & VT_BTYPE;
    int t2 = v2->type.t & VT_BTYPE;
    int c1 = (v1->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    int c2 = (v2->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    uint64_t l1 = c1 ? v1->c.i : 0;
    uint64_t l2 = c2 ? v2->c.i : 0;
    int shm = (t1 == VT_LLONG) ? 63 : 31;

    if (t1 != VT_LLONG && (PTR_SIZE != 8 || t1 != VT_PTR))
        l1 = ((uint32_t)l1 |
              (v1->type.t & VT_UNSIGNED ? 0 : -(l1 & 0x80000000)));
    if (t2 != VT_LLONG && (PTR_SIZE != 8 || t2 != VT_PTR))
        l2 = ((uint32_t)l2 |
              (v2->type.t & VT_UNSIGNED ? 0 : -(l2 & 0x80000000)));

    if (c1 && c2) {
        switch(op) {
        case '+': l1 += l2; break;
        case '-': l1 -= l2; break;
        case '&': l1 &= l2; break;
        case '^': l1 ^= l2; break;
        case '|': l1 |= l2; break;
        case '*': l1 *= l2; break;

        case TOK_PDIV:
        case '/':
        case '%':
        case TOK_UDIV:
        case TOK_UMOD:
            /* if division by zero, generate explicit division */
            if (l2 == 0) {
                if (const_wanted)
                    tcc_error("division by zero in constant");
                goto general_case;
            }
            switch(op) {
            default: l1 = gen_opic_sdiv(l1, l2); break;
            case '%': l1 = l1 - l2 * gen_opic_sdiv(l1, l2); break;
            case TOK_UDIV: l1 = l1 / l2; break;
            case TOK_UMOD: l1 = l1 % l2; break;
            }
            break;
        case TOK_SHL: l1 <<= (l2 & shm); break;
        case TOK_SHR: l1 >>= (l2 & shm); break;
        case TOK_SAR:
            l1 = (l1 >> 63) ? ~(~l1 >> (l2 & shm)) : l1 >> (l2 & shm);
            break;
            /* tests */
        case TOK_ULT: l1 = l1 < l2; break;
        case TOK_UGE: l1 = l1 >= l2; break;
        case TOK_EQ: l1 = l1 == l2; break;
        case TOK_NE: l1 = l1 != l2; break;
        case TOK_ULE: l1 = l1 <= l2; break;
        case TOK_UGT: l1 = l1 > l2; break;
        case TOK_LT: l1 = gen_opic_lt(l1, l2); break;
        case TOK_GE: l1 = !gen_opic_lt(l1, l2); break;
        case TOK_LE: l1 = !gen_opic_lt(l2, l1); break;
        case TOK_GT: l1 = gen_opic_lt(l2, l1); break;
            /* logical */
        case TOK_LAND: l1 = l1 && l2; break;
        case TOK_LOR: l1 = l1 || l2; break;
        default:
            goto general_case;
        }
	if (t1 != VT_LLONG && (PTR_SIZE != 8 || t1 != VT_PTR))
	    l1 = ((uint32_t)l1 |
		(v1->type.t & VT_UNSIGNED ? 0 : -(l1 & 0x80000000)));
        v1->c.i = l1;
        vtop--;
    } else {
        /* if commutative ops, put c2 as constant */
        if (c1 && (op == '+' || op == '&' || op == '^' || 
                   op == '|' || op == '*')) {
            vswap();
            c2 = c1; //c = c1, c1 = c2, c2 = c;
            l2 = l1; //l = l1, l1 = l2, l2 = l;
        }
        if (!const_wanted &&
            c1 && ((l1 == 0 &&
                    (op == TOK_SHL || op == TOK_SHR || op == TOK_SAR)) ||
                   (l1 == -1 && op == TOK_SAR))) {
            /* treat (0 << x), (0 >> x) and (-1 >> x) as constant */
            vtop--;
        } else if (!const_wanted &&
                   c2 && ((l2 == 0 && (op == '&' || op == '*')) ||
                          (op == '|' &&
                            (l2 == -1 || (l2 == 0xFFFFFFFF && t2 != VT_LLONG))) ||
                          (l2 == 1 && (op == '%' || op == TOK_UMOD)))) {
            /* treat (x & 0), (x * 0), (x | -1) and (x % 1) as constant */
            if (l2 == 1)
                vtop->c.i = 0;
            vswap();
            vtop--;
        } else if (c2 && (((op == '*' || op == '/' || op == TOK_UDIV ||
                          op == TOK_PDIV) &&
                           l2 == 1) ||
                          ((op == '+' || op == '-' || op == '|' || op == '^' ||
                            op == TOK_SHL || op == TOK_SHR || op == TOK_SAR) &&
                           l2 == 0) ||
                          (op == '&' &&
                            (l2 == -1 || (l2 == 0xFFFFFFFF && t2 != VT_LLONG))))) {
            /* filter out NOP operations like x*1, x-0, x&-1... */
            vtop--;
        } else if (c2 && (op == '*' || op == TOK_PDIV || op == TOK_UDIV)) {
            /* try to use shifts instead of muls or divs */
            if (l2 > 0 && (l2 & (l2 - 1)) == 0) {
                int n = -1;
                while (l2) {
                    l2 >>= 1;
                    n++;
                }
                vtop->c.i = n;
                if (op == '*')
                    op = TOK_SHL;
                else if (op == TOK_PDIV)
                    op = TOK_SAR;
                else
                    op = TOK_SHR;
            }
            goto general_case;
        } else if (c2 && (op == '+' || op == '-') &&
                   (((vtop[-1].r & (VT_VALMASK | VT_LVAL | VT_SYM)) == (VT_CONST | VT_SYM))
                    || (vtop[-1].r & (VT_VALMASK | VT_LVAL)) == VT_LOCAL)) {
            /* symbol + constant case */
            if (op == '-')
                l2 = -l2;
	    l2 += vtop[-1].c.i;
	    /* The backends can't always deal with addends to symbols
	       larger than +-1<<31.  Don't construct such.  */
	    if ((int)l2 != l2)
	        goto general_case;
            vtop--;
            vtop->c.i = l2;
        } else {
        general_case:
                /* call low level op generator */
                if (t1 == VT_LLONG || t2 == VT_LLONG ||
                    (PTR_SIZE == 8 && (t1 == VT_PTR || t2 == VT_PTR)))
                    gen_opl(op);
                else
                    gen_opi(op);
        }
    }
}

/* generate a floating point operation with constant propagation */
static void gen_opif(int op)
{
    int c1, c2;
    SValue *v1, *v2;
#if defined _MSC_VER && defined _AMD64_
    /* avoid bad optimization with f1 -= f2 for f1:-0.0, f2:0.0 */
    volatile
#endif
    long double f1, f2;

    v1 = vtop - 1;
    v2 = vtop;
    /* currently, we cannot do computations with forward symbols */
    c1 = (v1->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    c2 = (v2->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    if (c1 && c2) {
        if (v1->type.t == VT_FLOAT) {
            f1 = v1->c.f;
            f2 = v2->c.f;
        } else if (v1->type.t == VT_DOUBLE) {
            f1 = v1->c.d;
            f2 = v2->c.d;
        } else {
            f1 = v1->c.ld;
            f2 = v2->c.ld;
        }

        /* NOTE: we only do constant propagation if finite number (not
           NaN or infinity) (ANSI spec) */
        if (!ieee_finite(f1) || !ieee_finite(f2))
            goto general_case;

        switch(op) {
        case '+': f1 += f2; break;
        case '-': f1 -= f2; break;
        case '*': f1 *= f2; break;
        case '/': 
            if (f2 == 0.0) {
                if (const_wanted)
                    tcc_error("division by zero in constant");
                goto general_case;
            }
            f1 /= f2; 
            break;
            /* XXX: also handles tests ? */
        default:
            goto general_case;
        }
        /* XXX: overflow test ? */
        if (v1->type.t == VT_FLOAT) {
            v1->c.f = f1;
        } else if (v1->type.t == VT_DOUBLE) {
            v1->c.d = f1;
        } else {
            v1->c.ld = f1;
        }
        vtop--;
    } else {
    general_case:
        gen_opf(op);
    }
}

static int pointed_size(CType *type)
{
    int align;
    return type_size(pointed_type(type), &align);
}

static void vla_runtime_pointed_size(CType *type)
{
    int align;
    vla_runtime_type_size(pointed_type(type), &align);
}

static inline int is_null_pointer(SValue *p)
{
    if ((p->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        return 0;
    return ((p->type.t & VT_BTYPE) == VT_INT && (uint32_t)p->c.i == 0) ||
        ((p->type.t & VT_BTYPE) == VT_LLONG && p->c.i == 0) ||
        ((p->type.t & VT_BTYPE) == VT_PTR &&
         (PTR_SIZE == 4 ? (uint32_t)p->c.i == 0 : p->c.i == 0));
}

static inline int is_integer_btype(int bt)
{
    return (bt == VT_BYTE || bt == VT_SHORT || 
            bt == VT_INT || bt == VT_LLONG);
}

/* check types for comparison or subtraction of pointers */
static void check_comparison_pointer_types(SValue *p1, SValue *p2, int op)
{
    CType *type1, *type2, tmp_type1, tmp_type2;
    int bt1, bt2;
    
    /* null pointers are accepted for all comparisons as gcc */
    if (is_null_pointer(p1) || is_null_pointer(p2))
        return;
    type1 = &p1->type;
    type2 = &p2->type;
    bt1 = type1->t & VT_BTYPE;
    bt2 = type2->t & VT_BTYPE;
    /* accept comparison between pointer and integer with a warning */
    if ((is_integer_btype(bt1) || is_integer_btype(bt2)) && op != '-') {
        if (op != TOK_LOR && op != TOK_LAND )
            tcc_warning("comparison between pointer and integer");
        return;
    }

    /* both must be pointers or implicit function pointers */
    if (bt1 == VT_PTR) {
        type1 = pointed_type(type1);
    } else if (bt1 != VT_FUNC) 
        goto invalid_operands;

    if (bt2 == VT_PTR) {
        type2 = pointed_type(type2);
    } else if (bt2 != VT_FUNC) { 
    invalid_operands:
        tcc_error("invalid operands to binary %s", get_tok_str(op, NULL));
    }
    if ((type1->t & VT_BTYPE) == VT_VOID || 
        (type2->t & VT_BTYPE) == VT_VOID)
        return;
    tmp_type1 = *type1;
    tmp_type2 = *type2;
    tmp_type1.t &= ~(VT_DEFSIGN | VT_UNSIGNED | VT_CONSTANT | VT_VOLATILE);
    tmp_type2.t &= ~(VT_DEFSIGN | VT_UNSIGNED | VT_CONSTANT | VT_VOLATILE);
    if (!is_compatible_types(&tmp_type1, &tmp_type2)) {
        /* gcc-like error if '-' is used */
        if (op == '-')
            goto invalid_operands;
        else
            tcc_warning("comparison of distinct pointer types lacks a cast");
    }
}

/* generic gen_op: handles types problems */
ST_FUNC void gen_op(int op)
{
    int u, t1, t2, bt1, bt2, t;
    CType type1;

redo:
    t1 = vtop[-1].type.t;
    t2 = vtop[0].type.t;
    bt1 = t1 & VT_BTYPE;
    bt2 = t2 & VT_BTYPE;
        
    if (bt1 == VT_STRUCT || bt2 == VT_STRUCT) {
        tcc_error("operation on a struct");
    } else if (bt1 == VT_FUNC || bt2 == VT_FUNC) {
	if (bt2 == VT_FUNC) {
	    mk_pointer(&vtop->type);
	    gaddrof();
	}
	if (bt1 == VT_FUNC) {
	    vswap();
	    mk_pointer(&vtop->type);
	    gaddrof();
	    vswap();
	}
	goto redo;
    } else if (bt1 == VT_PTR || bt2 == VT_PTR) {
        /* at least one operand is a pointer */
        /* relational op: must be both pointers */
        if (op >= TOK_ULT && op <= TOK_LOR) {
            check_comparison_pointer_types(vtop - 1, vtop, op);
            /* pointers are handled are unsigned */
#if PTR_SIZE == 8
            t = VT_LLONG | VT_UNSIGNED;
#else
            t = VT_INT | VT_UNSIGNED;
#endif
            goto std_op;
        }
        /* if both pointers, then it must be the '-' op */
        if (bt1 == VT_PTR && bt2 == VT_PTR) {
            if (op != '-')
                tcc_error("cannot use pointers here");
            check_comparison_pointer_types(vtop - 1, vtop, op);
            /* XXX: check that types are compatible */
            if (vtop[-1].type.t & VT_VLA) {
                vla_runtime_pointed_size(&vtop[-1].type);
            } else {
                vpushi(pointed_size(&vtop[-1].type));
            }
            vrott(3);
            gen_opic(op);
            vtop->type.t = ptrdiff_type.t;
            vswap();
            gen_op(TOK_PDIV);
        } else {
            /* exactly one pointer : must be '+' or '-'. */
            if (op != '-' && op != '+')
                tcc_error("cannot use pointers here");
            /* Put pointer as first operand */
            if (bt2 == VT_PTR) {
                vswap();
                t = t1, t1 = t2, t2 = t;
            }
#if PTR_SIZE == 4
            if ((vtop[0].type.t & VT_BTYPE) == VT_LLONG)
                /* XXX: truncate here because gen_opl can't handle ptr + long long */
                gen_cast_s(VT_INT);
#endif
            type1 = vtop[-1].type;
            type1.t &= ~VT_ARRAY;
            if (vtop[-1].type.t & VT_VLA)
                vla_runtime_pointed_size(&vtop[-1].type);
            else {
                u = pointed_size(&vtop[-1].type);
                if (u < 0)
                    tcc_error("unknown array element size");
#if PTR_SIZE == 8
                vpushll(u);
#else
                /* XXX: cast to int ? (long long case) */
                vpushi(u);
#endif
            }
            gen_op('*');
#if 0
/* #ifdef CONFIG_TCC_BCHECK
    The main reason to removing this code:
	#include <stdio.h>
	int main ()
	{
	    int v[10];
	    int i = 10;
	    int j = 9;
	    fprintf(stderr, "v+i-j  = %p\n", v+i-j);
	    fprintf(stderr, "v+(i-j)  = %p\n", v+(i-j));
	}
    When this code is on. then the output looks like 
	v+i-j = 0xfffffffe
	v+(i-j) = 0xbff84000
    */
            /* if evaluating constant expression, no code should be
               generated, so no bound check */
            if (tcc_state->do_bounds_check && !const_wanted) {
                /* if bounded pointers, we generate a special code to
                   test bounds */
                if (op == '-') {
                    vpushi(0);
                    vswap();
                    gen_op('-');
                }
                gen_bounded_ptr_add();
            } else
#endif
            {
                gen_opic(op);
            }
            /* put again type if gen_opic() swaped operands */
            vtop->type = type1;
        }
    } else if (is_float(bt1) || is_float(bt2)) {
        /* compute bigger type and do implicit casts */
        if (bt1 == VT_LDOUBLE || bt2 == VT_LDOUBLE) {
            t = VT_LDOUBLE;
        } else if (bt1 == VT_DOUBLE || bt2 == VT_DOUBLE) {
            t = VT_DOUBLE;
        } else {
            t = VT_FLOAT;
        }
        /* floats can only be used for a few operations */
        if (op != '+' && op != '-' && op != '*' && op != '/' &&
            (op < TOK_ULT || op > TOK_GT))
            tcc_error("invalid operands for binary operation");
        goto std_op;
    } else if (op == TOK_SHR || op == TOK_SAR || op == TOK_SHL) {
        t = bt1 == VT_LLONG ? VT_LLONG : VT_INT;
        if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (t | VT_UNSIGNED))
          t |= VT_UNSIGNED;
        t |= (VT_LONG & t1);
        goto std_op;
    } else if (bt1 == VT_LLONG || bt2 == VT_LLONG) {
        /* cast to biggest op */
        t = VT_LLONG | VT_LONG;
        if (bt1 == VT_LLONG)
            t &= t1;
        if (bt2 == VT_LLONG)
            t &= t2;
        /* convert to unsigned if it does not fit in a long long */
        if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED) ||
            (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED))
            t |= VT_UNSIGNED;
        goto std_op;
    } else {
        /* integer operations */
        t = VT_INT | (VT_LONG & (t1 | t2));
        /* convert to unsigned if it does not fit in an integer */
        if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED) ||
            (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED))
            t |= VT_UNSIGNED;
    std_op:
        /* XXX: currently, some unsigned operations are explicit, so
           we modify them here */
        if (t & VT_UNSIGNED) {
            if (op == TOK_SAR)
                op = TOK_SHR;
            else if (op == '/')
                op = TOK_UDIV;
            else if (op == '%')
                op = TOK_UMOD;
            else if (op == TOK_LT)
                op = TOK_ULT;
            else if (op == TOK_GT)
                op = TOK_UGT;
            else if (op == TOK_LE)
                op = TOK_ULE;
            else if (op == TOK_GE)
                op = TOK_UGE;
        }
        vswap();
        type1.t = t;
        type1.ref = NULL;
        gen_cast(&type1);
        vswap();
        /* special case for shifts and long long: we keep the shift as
           an integer */
        if (op == TOK_SHR || op == TOK_SAR || op == TOK_SHL)
            type1.t = VT_INT;
        gen_cast(&type1);
        if (is_float(t))
            gen_opif(op);
        else
            gen_opic(op);
        if (op >= TOK_ULT && op <= TOK_GT) {
            /* relational op: the result is an int */
            vtop->type.t = VT_INT;
        } else {
            vtop->type.t = t;
        }
    }
    // Make sure that we have converted to an rvalue:
    if (vtop->r & VT_LVAL)
        gv(is_float(vtop->type.t & VT_BTYPE) ? RC_FLOAT : RC_INT);
}

#ifndef TCC_TARGET_ARM
/* generic itof for unsigned long long case */
static void gen_cvt_itof1(int t)
{
#ifdef TCC_TARGET_ARM64
    gen_cvt_itof(t);
#else
    if ((vtop->type.t & (VT_BTYPE | VT_UNSIGNED)) == 
        (VT_LLONG | VT_UNSIGNED)) {

        if (t == VT_FLOAT)
            vpush_global_sym(&func_old_type, TOK___floatundisf);
#if LDOUBLE_SIZE != 8
        else if (t == VT_LDOUBLE)
            vpush_global_sym(&func_old_type, TOK___floatundixf);
#endif
        else
            vpush_global_sym(&func_old_type, TOK___floatundidf);
        vrott(2);
        gfunc_call(1);
        vpushi(0);
        vtop->r = reg_fret(t);
    } else {
        gen_cvt_itof(t);
    }
#endif
}
#endif

/* generic ftoi for unsigned long long case */
static void gen_cvt_ftoi1(int t)
{
#ifdef TCC_TARGET_ARM64
    gen_cvt_ftoi(t);
#else
    int st;

    if (t == (VT_LLONG | VT_UNSIGNED)) {
        /* not handled natively */
        st = vtop->type.t & VT_BTYPE;
        if (st == VT_FLOAT)
            vpush_global_sym(&func_old_type, TOK___fixunssfdi);
#if LDOUBLE_SIZE != 8
        else if (st == VT_LDOUBLE)
            vpush_global_sym(&func_old_type, TOK___fixunsxfdi);
#endif
        else
            vpush_global_sym(&func_old_type, TOK___fixunsdfdi);
        vrott(2);
        gfunc_call(1);
        vpushi(0);
        vtop->r = REG_IRET;
        vtop->r2 = REG_LRET;
    } else {
        gen_cvt_ftoi(t);
    }
#endif
}

/* force char or short cast */
static void force_charshort_cast(int t)
{
    int bits, dbt;

    /* cannot cast static initializers */
    if (STATIC_DATA_WANTED)
	return;

    dbt = t & VT_BTYPE;
    /* XXX: add optimization if lvalue : just change type and offset */
    if (dbt == VT_BYTE)
        bits = 8;
    else
        bits = 16;
    if (t & VT_UNSIGNED) {
        vpushi((1 << bits) - 1);
        gen_op('&');
    } else {
        if ((vtop->type.t & VT_BTYPE) == VT_LLONG)
            bits = 64 - bits;
        else
            bits = 32 - bits;
        vpushi(bits);
        gen_op(TOK_SHL);
        /* result must be signed or the SAR is converted to an SHL
           This was not the case when "t" was a signed short
           and the last value on the stack was an unsigned int */
        vtop->type.t &= ~VT_UNSIGNED;
        vpushi(bits);
        gen_op(TOK_SAR);
    }
}

/* cast 'vtop' to 'type'. Casting to bitfields is forbidden. */
static void gen_cast_s(int t)
{
    CType type;
    type.t = t;
    type.ref = NULL;
    gen_cast(&type);
}

static void gen_cast(CType *type)
{
    int sbt, dbt, sf, df, c, p;

    /* special delayed cast for char/short */
    /* XXX: in some cases (multiple cascaded casts), it may still
       be incorrect */
    if (vtop->r & VT_MUSTCAST) {
        vtop->r &= ~VT_MUSTCAST;
        force_charshort_cast(vtop->type.t);
    }

    /* bitfields first get cast to ints */
    if (vtop->type.t & VT_BITFIELD) {
        gv(RC_INT);
    }

    dbt = type->t & (VT_BTYPE | VT_UNSIGNED);
    sbt = vtop->type.t & (VT_BTYPE | VT_UNSIGNED);

    if (sbt != dbt) {
        sf = is_float(sbt);
        df = is_float(dbt);
        c = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
        p = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == (VT_CONST | VT_SYM);
#if !defined TCC_IS_NATIVE && !defined TCC_IS_NATIVE_387
        c &= dbt != VT_LDOUBLE;
#endif
        if (c) {
            /* constant case: we can do it now */
            /* XXX: in ISOC, cannot do it if error in convert */
            if (sbt == VT_FLOAT)
                vtop->c.ld = vtop->c.f;
            else if (sbt == VT_DOUBLE)
                vtop->c.ld = vtop->c.d;

            if (df) {
                if ((sbt & VT_BTYPE) == VT_LLONG) {
                    if ((sbt & VT_UNSIGNED) || !(vtop->c.i >> 63))
                        vtop->c.ld = vtop->c.i;
                    else
                        vtop->c.ld = -(long double)-vtop->c.i;
                } else if(!sf) {
                    if ((sbt & VT_UNSIGNED) || !(vtop->c.i >> 31))
                        vtop->c.ld = (uint32_t)vtop->c.i;
                    else
                        vtop->c.ld = -(long double)-(uint32_t)vtop->c.i;
                }

                if (dbt == VT_FLOAT)
                    vtop->c.f = (float)vtop->c.ld;
                else if (dbt == VT_DOUBLE)
                    vtop->c.d = (double)vtop->c.ld;
            } else if (sf && dbt == (VT_LLONG|VT_UNSIGNED)) {
                vtop->c.i = vtop->c.ld;
            } else if (sf && dbt == VT_BOOL) {
                vtop->c.i = (vtop->c.ld != 0);
            } else {
                if(sf)
                    vtop->c.i = vtop->c.ld;
                else if (sbt == (VT_LLONG|VT_UNSIGNED))
                    ;
                else if (sbt & VT_UNSIGNED)
                    vtop->c.i = (uint32_t)vtop->c.i;
#if PTR_SIZE == 8
                else if (sbt == VT_PTR)
                    ;
#endif
                else if (sbt != VT_LLONG)
                    vtop->c.i = ((uint32_t)vtop->c.i |
                                  -(vtop->c.i & 0x80000000));

                if (dbt == (VT_LLONG|VT_UNSIGNED))
                    ;
                else if (dbt == VT_BOOL)
                    vtop->c.i = (vtop->c.i != 0);
#if PTR_SIZE == 8
                else if (dbt == VT_PTR)
                    ;
#endif
                else if (dbt != VT_LLONG) {
                    uint32_t m = ((dbt & VT_BTYPE) == VT_BYTE ? 0xff :
                                  (dbt & VT_BTYPE) == VT_SHORT ? 0xffff :
                                  0xffffffff);
                    vtop->c.i &= m;
                    if (!(dbt & VT_UNSIGNED))
                        vtop->c.i |= -(vtop->c.i & ((m >> 1) + 1));
                }
            }
        } else if (p && dbt == VT_BOOL) {
            vtop->r = VT_CONST;
            vtop->c.i = 1;
        } else {
            /* non constant case: generate code */
            if (sf && df) {
                /* convert from fp to fp */
                gen_cvt_ftof(dbt);
            } else if (df) {
                /* convert int to fp */
                gen_cvt_itof1(dbt);
            } else if (sf) {
                /* convert fp to int */
                if (dbt == VT_BOOL) {
                     vpushi(0);
                     gen_op(TOK_NE);
                } else {
                    /* we handle char/short/etc... with generic code */
                    if (dbt != (VT_INT | VT_UNSIGNED) &&
                        dbt != (VT_LLONG | VT_UNSIGNED) &&
                        dbt != VT_LLONG)
                        dbt = VT_INT;
                    gen_cvt_ftoi1(dbt);
                    if (dbt == VT_INT && (type->t & (VT_BTYPE | VT_UNSIGNED)) != dbt) {
                        /* additional cast for char/short... */
                        vtop->type.t = dbt;
                        gen_cast(type);
                    }
                }
#if PTR_SIZE == 4
            } else if ((dbt & VT_BTYPE) == VT_LLONG) {
                if ((sbt & VT_BTYPE) != VT_LLONG) {
                    /* scalar to long long */
                    /* machine independent conversion */
                    gv(RC_INT);
                    /* generate high word */
                    if (sbt == (VT_INT | VT_UNSIGNED)) {
                        vpushi(0);
                        gv(RC_INT);
                    } else {
                        if (sbt == VT_PTR) {
                            /* cast from pointer to int before we apply
                               shift operation, which pointers don't support*/
                            gen_cast_s(VT_INT);
                        }
                        gv_dup();
                        vpushi(31);
                        gen_op(TOK_SAR);
                    }
                    /* patch second register */
                    vtop[-1].r2 = vtop->r;
                    vpop();
                }
#else
            } else if ((dbt & VT_BTYPE) == VT_LLONG ||
                       (dbt & VT_BTYPE) == VT_PTR ||
                       (dbt & VT_BTYPE) == VT_FUNC) {
                if ((sbt & VT_BTYPE) != VT_LLONG &&
                    (sbt & VT_BTYPE) != VT_PTR &&
                    (sbt & VT_BTYPE) != VT_FUNC) {
                    /* need to convert from 32bit to 64bit */
                    gv(RC_INT);
                    if (sbt != (VT_INT | VT_UNSIGNED)) {
#if defined(TCC_TARGET_ARM64)
                        gen_cvt_sxtw();
#elif defined(TCC_TARGET_X86_64)
                        int r = gv(RC_INT);
                        /* x86_64 specific: movslq */
                        o(0x6348);
                        o(0xc0 + (REG_VALUE(r) << 3) + REG_VALUE(r));
#else
#error
#endif
                    }
                }
#endif
            } else if (dbt == VT_BOOL) {
                /* scalar to bool */
                vpushi(0);
                gen_op(TOK_NE);
            } else if ((dbt & VT_BTYPE) == VT_BYTE || 
                       (dbt & VT_BTYPE) == VT_SHORT) {
                if (sbt == VT_PTR) {
                    vtop->type.t = VT_INT;
                    tcc_warning("nonportable conversion from pointer to char/short");
                }
                force_charshort_cast(dbt);
#if PTR_SIZE == 4
            } else if ((dbt & VT_BTYPE) == VT_INT) {
                /* scalar to int */
                if ((sbt & VT_BTYPE) == VT_LLONG) {
                    /* from long long: just take low order word */
                    lexpand();
                    vpop();
                } 
                /* if lvalue and single word type, nothing to do because
                   the lvalue already contains the real type size (see
                   VT_LVAL_xxx constants) */
#endif
            }
        }
    } else if ((dbt & VT_BTYPE) == VT_PTR && !(vtop->r & VT_LVAL)) {
        /* if we are casting between pointer types,
           we must update the VT_LVAL_xxx size */
        vtop->r = (vtop->r & ~VT_LVAL_TYPE)
                  | (lvalue_type(type->ref->type.t) & VT_LVAL_TYPE);
    }
    vtop->type = *type;
}

/* return type size as known at compile time. Put alignment at 'a' */
ST_FUNC int type_size(CType *type, int *a)
{
    Sym *s;
    int bt;

    bt = type->t & VT_BTYPE;
    if (bt == VT_STRUCT) {
        /* struct/union */
        s = type->ref;
        *a = s->r;
        return s->c;
    } else if (bt == VT_PTR) {
        if (type->t & VT_ARRAY) {
            int ts;

            s = type->ref;
            ts = type_size(&s->type, a);

            if (ts < 0 && s->c < 0)
                ts = -ts;

            return ts * s->c;
        } else {
            *a = PTR_SIZE;
            return PTR_SIZE;
        }
    } else if (IS_ENUM(type->t) && type->ref->c == -1) {
        return -1; /* incomplete enum */
    } else if (bt == VT_LDOUBLE) {
        *a = LDOUBLE_ALIGN;
        return LDOUBLE_SIZE;
    } else if (bt == VT_DOUBLE || bt == VT_LLONG) {
#ifdef TCC_TARGET_I386
#ifdef TCC_TARGET_PE
        *a = 8;
#else
        *a = 4;
#endif
#elif defined(TCC_TARGET_ARM)
#ifdef TCC_ARM_EABI
        *a = 8; 
#else
        *a = 4;
#endif
#else
        *a = 8;
#endif
        return 8;
    } else if (bt == VT_INT || bt == VT_FLOAT) {
        *a = 4;
        return 4;
    } else if (bt == VT_SHORT) {
        *a = 2;
        return 2;
    } else if (bt == VT_QLONG || bt == VT_QFLOAT) {
        *a = 8;
        return 16;
    } else {
        /* char, void, function, _Bool */
        *a = 1;
        return 1;
    }
}

/* push type size as known at runtime time on top of value stack. Put
   alignment at 'a' */
ST_FUNC void vla_runtime_type_size(CType *type, int *a)
{
    if (type->t & VT_VLA) {
        type_size(&type->ref->type, a);
        vset(&int_type, VT_LOCAL|VT_LVAL, type->ref->c);
    } else {
        vpushi(type_size(type, a));
    }
}

static void vla_sp_restore(void) {
    if (vlas_in_scope) {
        gen_vla_sp_restore(vla_sp_loc);
    }
}

static void vla_sp_restore_root(void) {
    if (vlas_in_scope) {
        gen_vla_sp_restore(vla_sp_root_loc);
    }
}

/* return the pointed type of t */
static inline CType *pointed_type(CType *type)
{
    return &type->ref->type;
}

/* modify type so that its it is a pointer to type. */
ST_FUNC void mk_pointer(CType *type)
{
    Sym *s;
    s = sym_push(SYM_FIELD, type, 0, -1);
    type->t = VT_PTR | (type->t & VT_STORAGE);
    type->ref = s;
}

/* compare function types. OLD functions match any new functions */
static int is_compatible_func(CType *type1, CType *type2)
{
    Sym *s1, *s2;

    s1 = type1->ref;
    s2 = type2->ref;
    if (!is_compatible_types(&s1->type, &s2->type))
        return 0;
    /* check func_call */
    if (s1->f.func_call != s2->f.func_call)
        return 0;
    /* XXX: not complete */
    if (s1->f.func_type == FUNC_OLD || s2->f.func_type == FUNC_OLD)
        return 1;
    if (s1->f.func_type != s2->f.func_type)
        return 0;
    while (s1 != NULL) {
        if (s2 == NULL)
            return 0;
        if (!is_compatible_unqualified_types(&s1->type, &s2->type))
            return 0;
        s1 = s1->next;
        s2 = s2->next;
    }
    if (s2)
        return 0;
    return 1;
}

/* return true if type1 and type2 are the same.  If unqualified is
   true, qualifiers on the types are ignored.

   - enums are not checked as gcc __builtin_types_compatible_p () 
 */
static int compare_types(CType *type1, CType *type2, int unqualified)
{
    int bt1, t1, t2;

    t1 = type1->t & VT_TYPE;
    t2 = type2->t & VT_TYPE;
    if (unqualified) {
        /* strip qualifiers before comparing */
        t1 &= ~(VT_CONSTANT | VT_VOLATILE);
        t2 &= ~(VT_CONSTANT | VT_VOLATILE);
    }

    /* Default Vs explicit signedness only matters for char */
    if ((t1 & VT_BTYPE) != VT_BYTE) {
        t1 &= ~VT_DEFSIGN;
        t2 &= ~VT_DEFSIGN;
    }
    /* XXX: bitfields ? */
    if (t1 != t2)
        return 0;
    /* test more complicated cases */
    bt1 = t1 & VT_BTYPE;
    if (bt1 == VT_PTR) {
        type1 = pointed_type(type1);
        type2 = pointed_type(type2);
        return is_compatible_types(type1, type2);
    } else if (bt1 == VT_STRUCT) {
        return (type1->ref == type2->ref);
    } else if (bt1 == VT_FUNC) {
        return is_compatible_func(type1, type2);
    } else {
        return 1;
    }
}

/* return true if type1 and type2 are exactly the same (including
   qualifiers). 
*/
static int is_compatible_types(CType *type1, CType *type2)
{
    return compare_types(type1,type2,0);
}

/* return true if type1 and type2 are the same (ignoring qualifiers).
*/
static int is_compatible_unqualified_types(CType *type1, CType *type2)
{
    return compare_types(type1,type2,1);
}

/* print a type. If 'varstr' is not NULL, then the variable is also
   printed in the type */
/* XXX: union */
/* XXX: add array and function pointers */
static void type_to_str(char *buf, int buf_size, 
                 CType *type, const char *varstr)
{
    int bt, v, t;
    Sym *s, *sa;
    char buf1[256];
    const char *tstr;

    t = type->t;
    bt = t & VT_BTYPE;
    buf[0] = '\0';

    if (t & VT_EXTERN)
        pstrcat(buf, buf_size, "extern ");
    if (t & VT_STATIC)
        pstrcat(buf, buf_size, "static ");
    if (t & VT_TYPEDEF)
        pstrcat(buf, buf_size, "typedef ");
    if (t & VT_INLINE)
        pstrcat(buf, buf_size, "inline ");
    if (t & VT_VOLATILE)
        pstrcat(buf, buf_size, "volatile ");
    if (t & VT_CONSTANT)
        pstrcat(buf, buf_size, "const ");

    if (((t & VT_DEFSIGN) && bt == VT_BYTE)
        || ((t & VT_UNSIGNED)
            && (bt == VT_SHORT || bt == VT_INT || bt == VT_LLONG)
            && !IS_ENUM(t)
            ))
        pstrcat(buf, buf_size, (t & VT_UNSIGNED) ? "unsigned " : "signed ");

    buf_size -= strlen(buf);
    buf += strlen(buf);

    switch(bt) {
    case VT_VOID:
        tstr = "void";
        goto add_tstr;
    case VT_BOOL:
        tstr = "_Bool";
        goto add_tstr;
    case VT_BYTE:
        tstr = "char";
        goto add_tstr;
    case VT_SHORT:
        tstr = "short";
        goto add_tstr;
    case VT_INT:
        tstr = "int";
        goto maybe_long;
    case VT_LLONG:
        tstr = "long long";
    maybe_long:
        if (t & VT_LONG)
            tstr = "long";
        if (!IS_ENUM(t))
            goto add_tstr;
        tstr = "enum ";
        goto tstruct;
    case VT_FLOAT:
        tstr = "float";
        goto add_tstr;
    case VT_DOUBLE:
        tstr = "double";
        goto add_tstr;
    case VT_LDOUBLE:
        tstr = "long double";
    add_tstr:
        pstrcat(buf, buf_size, tstr);
        break;
    case VT_STRUCT:
        tstr = "struct ";
        if (IS_UNION(t))
            tstr = "union ";
    tstruct:
        pstrcat(buf, buf_size, tstr);
        v = type->ref->v & ~SYM_STRUCT;
        if (v >= SYM_FIRST_ANOM)
            pstrcat(buf, buf_size, "<anonymous>");
        else
            pstrcat(buf, buf_size, get_tok_str(v, NULL));
        break;
    case VT_FUNC:
        s = type->ref;
        type_to_str(buf, buf_size, &s->type, varstr);
        pstrcat(buf, buf_size, "(");
        sa = s->next;
        while (sa != NULL) {
            type_to_str(buf1, sizeof(buf1), &sa->type, NULL);
            pstrcat(buf, buf_size, buf1);
            sa = sa->next;
            if (sa)
                pstrcat(buf, buf_size, ", ");
        }
        pstrcat(buf, buf_size, ")");
        goto no_var;
    case VT_PTR:
        s = type->ref;
        if (t & VT_ARRAY) {
            snprintf(buf1, sizeof(buf1), "%s[%d]", varstr ? varstr : "", s->c);
            type_to_str(buf, buf_size, &s->type, buf1);
            goto no_var;
        }
        pstrcpy(buf1, sizeof(buf1), "*");
        if (t & VT_CONSTANT)
            pstrcat(buf1, buf_size, "const ");
        if (t & VT_VOLATILE)
            pstrcat(buf1, buf_size, "volatile ");
        if (varstr)
            pstrcat(buf1, sizeof(buf1), varstr);
        type_to_str(buf, buf_size, &s->type, buf1);
        goto no_var;
    }
    if (varstr) {
        pstrcat(buf, buf_size, " ");
        pstrcat(buf, buf_size, varstr);
    }
 no_var: ;
}

/* verify type compatibility to store vtop in 'dt' type, and generate
   casts if needed. */
static void gen_assign_cast(CType *dt)
{
    CType *st, *type1, *type2;
    char buf1[256], buf2[256];
    int dbt, sbt;

    st = &vtop->type; /* source type */
    dbt = dt->t & VT_BTYPE;
    sbt = st->t & VT_BTYPE;
    if (sbt == VT_VOID || dbt == VT_VOID) {
	if (sbt == VT_VOID && dbt == VT_VOID)
	    ; /*
	      It is Ok if both are void
	      A test program:
	        void func1() {}
		void func2() {
		  return func1();
		}
	      gcc accepts this program
	      */
	else
    	    tcc_error("cannot cast from/to void");
    }
    if (dt->t & VT_CONSTANT)
        tcc_warning("assignment of read-only location");
    switch(dbt) {
    case VT_PTR:
        /* special cases for pointers */
        /* '0' can also be a pointer */
        if (is_null_pointer(vtop))
            goto type_ok;
        /* accept implicit pointer to integer cast with warning */
        if (is_integer_btype(sbt)) {
            tcc_warning("assignment makes pointer from integer without a cast");
            goto type_ok;
        }
        type1 = pointed_type(dt);
        /* a function is implicitly a function pointer */
        if (sbt == VT_FUNC) {
            if ((type1->t & VT_BTYPE) != VT_VOID &&
                !is_compatible_types(pointed_type(dt), st))
                tcc_warning("assignment from incompatible pointer type");
            goto type_ok;
        }
        if (sbt != VT_PTR)
            goto error;
        type2 = pointed_type(st);
        if ((type1->t & VT_BTYPE) == VT_VOID || 
            (type2->t & VT_BTYPE) == VT_VOID) {
            /* void * can match anything */
        } else {
            //printf("types %08x %08x\n", type1->t, type2->t);
            /* exact type match, except for qualifiers */
            if (!is_compatible_unqualified_types(type1, type2)) {
		/* Like GCC don't warn by default for merely changes
		   in pointer target signedness.  Do warn for different
		   base types, though, in particular for unsigned enums
		   and signed int targets.  */
		if ((type1->t & (VT_BTYPE|VT_LONG)) != (type2->t & (VT_BTYPE|VT_LONG))
                    || IS_ENUM(type1->t) || IS_ENUM(type2->t)
                    )
		    tcc_warning("assignment from incompatible pointer type");
	    }
        }
        /* check const and volatile */
        if ((!(type1->t & VT_CONSTANT) && (type2->t & VT_CONSTANT)) ||
            (!(type1->t & VT_VOLATILE) && (type2->t & VT_VOLATILE)))
            tcc_warning("assignment discards qualifiers from pointer target type");
        break;
    case VT_BYTE:
    case VT_SHORT:
    case VT_INT:
    case VT_LLONG:
        if (sbt == VT_PTR || sbt == VT_FUNC) {
            tcc_warning("assignment makes integer from pointer without a cast");
        } else if (sbt == VT_STRUCT) {
            goto case_VT_STRUCT;
        }
        /* XXX: more tests */
        break;
    case VT_STRUCT:
    case_VT_STRUCT:
        if (!is_compatible_unqualified_types(dt, st)) {
        error:
            type_to_str(buf1, sizeof(buf1), st, NULL);
            type_to_str(buf2, sizeof(buf2), dt, NULL);
            tcc_error("cannot cast '%s' to '%s'", buf1, buf2);
        }
        break;
    }
 type_ok:
    gen_cast(dt);
}

/* store vtop in lvalue pushed on stack */
ST_FUNC void vstore(void)
{
    int sbt, dbt, ft, r, t, size, align, bit_size, bit_pos, rc, delayed_cast;

    ft = vtop[-1].type.t;
    sbt = vtop->type.t & VT_BTYPE;
    dbt = ft & VT_BTYPE;
    if ((((sbt == VT_INT || sbt == VT_SHORT) && dbt == VT_BYTE) ||
         (sbt == VT_INT && dbt == VT_SHORT))
	&& !(vtop->type.t & VT_BITFIELD)) {
        /* optimize char/short casts */
        delayed_cast = VT_MUSTCAST;
        vtop->type.t = ft & VT_TYPE;
        /* XXX: factorize */
        if (ft & VT_CONSTANT)
            tcc_warning("assignment of read-only location");
    } else {
        delayed_cast = 0;
        if (!(ft & VT_BITFIELD))
            gen_assign_cast(&vtop[-1].type);
    }

    if (sbt == VT_STRUCT) {
        /* if structure, only generate pointer */
        /* structure assignment : generate memcpy */
        /* XXX: optimize if small size */
            size = type_size(&vtop->type, &align);

            /* destination */
            vswap();
            vtop->type.t = VT_PTR;
            gaddrof();

            /* address of memcpy() */
#ifdef TCC_ARM_EABI
            if(!(align & 7))
                vpush_global_sym(&func_old_type, TOK_memcpy8);
            else if(!(align & 3))
                vpush_global_sym(&func_old_type, TOK_memcpy4);
            else
#endif
            /* Use memmove, rather than memcpy, as dest and src may be same: */
            vpush_global_sym(&func_old_type, TOK_memmove);

            vswap();
            /* source */
            vpushv(vtop - 2);
            vtop->type.t = VT_PTR;
            gaddrof();
            /* type size */
            vpushi(size);
            gfunc_call(3);

        /* leave source on stack */
    } else if (ft & VT_BITFIELD) {
        /* bitfield store handling */

        /* save lvalue as expression result (example: s.b = s.a = n;) */
        vdup(), vtop[-1] = vtop[-2];

        bit_pos = BIT_POS(ft);
        bit_size = BIT_SIZE(ft);
        /* remove bit field info to avoid loops */
        vtop[-1].type.t = ft & ~VT_STRUCT_MASK;

        if ((ft & VT_BTYPE) == VT_BOOL) {
            gen_cast(&vtop[-1].type);
            vtop[-1].type.t = (vtop[-1].type.t & ~VT_BTYPE) | (VT_BYTE | VT_UNSIGNED);
        }

        r = adjust_bf(vtop - 1, bit_pos, bit_size);
        if (r == VT_STRUCT) {
            gen_cast_s((ft & VT_BTYPE) == VT_LLONG ? VT_LLONG : VT_INT);
            store_packed_bf(bit_pos, bit_size);
        } else {
            unsigned long long mask = (1ULL << bit_size) - 1;
            if ((ft & VT_BTYPE) != VT_BOOL) {
                /* mask source */
                if ((vtop[-1].type.t & VT_BTYPE) == VT_LLONG)
                    vpushll(mask);
                else
                    vpushi((unsigned)mask);
                gen_op('&');
            }
            /* shift source */
            vpushi(bit_pos);
            gen_op(TOK_SHL);
            vswap();
            /* duplicate destination */
            vdup();
            vrott(3);
            /* load destination, mask and or with source */
            if ((vtop->type.t & VT_BTYPE) == VT_LLONG)
                vpushll(~(mask << bit_pos));
            else
                vpushi(~((unsigned)mask << bit_pos));
            gen_op('&');
            gen_op('|');
            /* store result */
            vstore();
            /* ... and discard */
            vpop();
        }
    } else if (dbt == VT_VOID) {
        --vtop;
    } else {
#ifdef CONFIG_TCC_BCHECK
            /* bound check case */
            if (vtop[-1].r & VT_MUSTBOUND) {
                vswap();
                gbound();
                vswap();
            }
#endif
            rc = RC_INT;
            if (is_float(ft)) {
                rc = RC_FLOAT;
#ifdef TCC_TARGET_X86_64
                if ((ft & VT_BTYPE) == VT_LDOUBLE) {
                    rc = RC_ST0;
                } else if ((ft & VT_BTYPE) == VT_QFLOAT) {
                    rc = RC_FRET;
                }
#endif
            }
            r = gv(rc);  /* generate value */
            /* if lvalue was saved on stack, must read it */
            if ((vtop[-1].r & VT_VALMASK) == VT_LLOCAL) {
                SValue sv;
                t = get_reg(RC_INT);
#if PTR_SIZE == 8
                sv.type.t = VT_PTR;
#else
                sv.type.t = VT_INT;
#endif
                sv.r = VT_LOCAL | VT_LVAL;
                sv.c.i = vtop[-1].c.i;
                load(t, &sv);
                vtop[-1].r = t | VT_LVAL;
            }
            /* two word case handling : store second register at word + 4 (or +8 for x86-64)  */
#if PTR_SIZE == 8
            if (((ft & VT_BTYPE) == VT_QLONG) || ((ft & VT_BTYPE) == VT_QFLOAT)) {
                int addr_type = VT_LLONG, load_size = 8, load_type = ((vtop->type.t & VT_BTYPE) == VT_QLONG) ? VT_LLONG : VT_DOUBLE;
#else
            if ((ft & VT_BTYPE) == VT_LLONG) {
                int addr_type = VT_INT, load_size = 4, load_type = VT_INT;
#endif
                vtop[-1].type.t = load_type;
                store(r, vtop - 1);
                vswap();
                /* convert to int to increment easily */
                vtop->type.t = addr_type;
                gaddrof();
                vpushi(load_size);
                gen_op('+');
                vtop->r |= VT_LVAL;
                vswap();
                vtop[-1].type.t = load_type;
                /* XXX: it works because r2 is spilled last ! */
                store(vtop->r2, vtop - 1);
            } else {
                store(r, vtop - 1);
            }

        vswap();
        vtop--; /* NOT vpop() because on x86 it would flush the fp stack */
        vtop->r |= delayed_cast;
    }
}

/* post defines POST/PRE add. c is the token ++ or -- */
ST_FUNC void inc(int post, int c)
{
    test_lvalue();
    vdup(); /* save lvalue */
    if (post) {
        gv_dup(); /* duplicate value */
        vrotb(3);
        vrotb(3);
    }
    /* add constant */
    vpushi(c - TOK_MID); 
    gen_op('+');
    vstore(); /* store value */
    if (post)
        vpop(); /* if post op, return saved value */
}

ST_FUNC void parse_mult_str (CString *astr, const char *msg)
{
    /* read the string */
    if (tok != TOK_STR)
        expect(msg);
    cstr_new(astr);
    while (tok == TOK_STR) {
        /* XXX: add \0 handling too ? */
        cstr_cat(astr, tokc.str.data, -1);
        next();
    }
    cstr_ccat(astr, '\0');
}

/* If I is >= 1 and a power of two, returns log2(i)+1.
   If I is 0 returns 0.  */
static int exact_log2p1(int i)
{
  int ret;
  if (!i)
    return 0;
  for (ret = 1; i >= 1 << 8; ret += 8)
    i >>= 8;
  if (i >= 1 << 4)
    ret += 4, i >>= 4;
  if (i >= 1 << 2)
    ret += 2, i >>= 2;
  if (i >= 1 << 1)
    ret++;
  return ret;
}

/* Parse __attribute__((...)) GNUC extension. */
static void parse_attribute(AttributeDef *ad)
{
    int t, n;
    CString astr;
    
redo:
    if (tok != TOK_ATTRIBUTE1 && tok != TOK_ATTRIBUTE2)
        return;
    next();
    skip('(');
    skip('(');
    while (tok != ')') {
        if (tok < TOK_IDENT)
            expect("attribute name");
        t = tok;
        next();
        switch(t) {
        case TOK_SECTION1:
        case TOK_SECTION2:
            skip('(');
	    parse_mult_str(&astr, "section name");
            ad->section = find_section(tcc_state, (char *)astr.data);
            skip(')');
	    cstr_free(&astr);
            break;
        case TOK_ALIAS1:
        case TOK_ALIAS2:
            skip('(');
	    parse_mult_str(&astr, "alias(\"target\")");
            ad->alias_target = /* save string as token, for later */
              tok_alloc((char*)astr.data, astr.size-1)->tok;
            skip(')');
	    cstr_free(&astr);
            break;
	case TOK_VISIBILITY1:
	case TOK_VISIBILITY2:
            skip('(');
	    parse_mult_str(&astr,
			   "visibility(\"default|hidden|internal|protected\")");
	    if (!strcmp (astr.data, "default"))
	        ad->a.visibility = STV_DEFAULT;
	    else if (!strcmp (astr.data, "hidden"))
	        ad->a.visibility = STV_HIDDEN;
	    else if (!strcmp (astr.data, "internal"))
	        ad->a.visibility = STV_INTERNAL;
	    else if (!strcmp (astr.data, "protected"))
	        ad->a.visibility = STV_PROTECTED;
	    else
                expect("visibility(\"default|hidden|internal|protected\")");
            skip(')');
	    cstr_free(&astr);
            break;
        case TOK_ALIGNED1:
        case TOK_ALIGNED2:
            if (tok == '(') {
                next();
                n = expr_const();
                if (n <= 0 || (n & (n - 1)) != 0) 
                    tcc_error("alignment must be a positive power of two");
                skip(')');
            } else {
                n = MAX_ALIGN;
            }
            ad->a.aligned = exact_log2p1(n);
	    if (n != 1 << (ad->a.aligned - 1))
	      tcc_error("alignment of %d is larger than implemented", n);
            break;
        case TOK_PACKED1:
        case TOK_PACKED2:
            ad->a.packed = 1;
            break;
        case TOK_WEAK1:
        case TOK_WEAK2:
            ad->a.weak = 1;
            break;
        case TOK_UNUSED1:
        case TOK_UNUSED2:
            /* currently, no need to handle it because tcc does not
               track unused objects */
            break;
        case TOK_NORETURN1:
        case TOK_NORETURN2:
            /* currently, no need to handle it because tcc does not
               track unused objects */
            break;
        case TOK_CDECL1:
        case TOK_CDECL2:
        case TOK_CDECL3:
            ad->f.func_call = FUNC_CDECL;
            break;
        case TOK_STDCALL1:
        case TOK_STDCALL2:
        case TOK_STDCALL3:
            ad->f.func_call = FUNC_STDCALL;
            break;
#ifdef TCC_TARGET_I386
        case TOK_REGPARM1:
        case TOK_REGPARM2:
            skip('(');
            n = expr_const();
            if (n > 3) 
                n = 3;
            else if (n < 0)
                n = 0;
            if (n > 0)
                ad->f.func_call = FUNC_FASTCALL1 + n - 1;
            skip(')');
            break;
        case TOK_FASTCALL1:
        case TOK_FASTCALL2:
        case TOK_FASTCALL3:
            ad->f.func_call = FUNC_FASTCALLW;
            break;            
#endif
        case TOK_MODE:
            skip('(');
            switch(tok) {
                case TOK_MODE_DI:
                    ad->attr_mode = VT_LLONG + 1;
                    break;
                case TOK_MODE_QI:
                    ad->attr_mode = VT_BYTE + 1;
                    break;
                case TOK_MODE_HI:
                    ad->attr_mode = VT_SHORT + 1;
                    break;
                case TOK_MODE_SI:
                case TOK_MODE_word:
                    ad->attr_mode = VT_INT + 1;
                    break;
                default:
                    tcc_warning("__mode__(%s) not supported\n", get_tok_str(tok, NULL));
                    break;
            }
            next();
            skip(')');
            break;
        case TOK_DLLEXPORT:
            ad->a.dllexport = 1;
            break;
        case TOK_DLLIMPORT:
            ad->a.dllimport = 1;
            break;
        default:
            if (tcc_state->warn_unsupported)
                tcc_warning("'%s' attribute ignored", get_tok_str(t, NULL));
            /* skip parameters */
            if (tok == '(') {
                int parenthesis = 0;
                do {
                    if (tok == '(') 
                        parenthesis++;
                    else if (tok == ')') 
                        parenthesis--;
                    next();
                } while (parenthesis && tok != -1);
            }
            break;
        }
        if (tok != ',')
            break;
        next();
    }
    skip(')');
    skip(')');
    goto redo;
}

static Sym * find_field (CType *type, int v)
{
    Sym *s = type->ref;
    v |= SYM_FIELD;
    while ((s = s->next) != NULL) {
	if ((s->v & SYM_FIELD) &&
	    (s->type.t & VT_BTYPE) == VT_STRUCT &&
	    (s->v & ~SYM_FIELD) >= SYM_FIRST_ANOM) {
	    Sym *ret = find_field (&s->type, v);
	    if (ret)
	        return ret;
	}
	if (s->v == v)
	  break;
    }
    return s;
}

static void struct_add_offset (Sym *s, int offset)
{
    while ((s = s->next) != NULL) {
	if ((s->v & SYM_FIELD) &&
	    (s->type.t & VT_BTYPE) == VT_STRUCT &&
	    (s->v & ~SYM_FIELD) >= SYM_FIRST_ANOM) {
	    struct_add_offset(s->type.ref, offset);
	} else
	  s->c += offset;
    }
}

static void struct_layout(CType *type, AttributeDef *ad)
{
    int size, align, maxalign, offset, c, bit_pos, bit_size;
    int packed, a, bt, prevbt, prev_bit_size;
    int pcc = !tcc_state->ms_bitfields;
    int pragma_pack = *tcc_state->pack_stack_ptr;
    Sym *f;

    maxalign = 1;
    offset = 0;
    c = 0;
    bit_pos = 0;
    prevbt = VT_STRUCT; /* make it never match */
    prev_bit_size = 0;

//#define BF_DEBUG

    for (f = type->ref->next; f; f = f->next) {
        if (f->type.t & VT_BITFIELD)
            bit_size = BIT_SIZE(f->type.t);
        else
            bit_size = -1;
        size = type_size(&f->type, &align);
        a = f->a.aligned ? 1 << (f->a.aligned - 1) : 0;
        packed = 0;

        if (pcc && bit_size == 0) {
            /* in pcc mode, packing does not affect zero-width bitfields */

        } else {
            /* in pcc mode, attribute packed overrides if set. */
            if (pcc && (f->a.packed || ad->a.packed))
                align = packed = 1;

            /* pragma pack overrides align if lesser and packs bitfields always */
            if (pragma_pack) {
                packed = 1;
                if (pragma_pack < align)
                    align = pragma_pack;
                /* in pcc mode pragma pack also overrides individual align */
                if (pcc && pragma_pack < a)
                    a = 0;
            }
        }
        /* some individual align was specified */
        if (a)
            align = a;

        if (type->ref->type.t == VT_UNION) {
	    if (pcc && bit_size >= 0)
	        size = (bit_size + 7) >> 3;
	    offset = 0;
	    if (size > c)
	        c = size;

	} else if (bit_size < 0) {
            if (pcc)
                c += (bit_pos + 7) >> 3;
	    c = (c + align - 1) & -align;
	    offset = c;
	    if (size > 0)
	        c += size;
	    bit_pos = 0;
	    prevbt = VT_STRUCT;
	    prev_bit_size = 0;

	} else {
	    /* A bit-field.  Layout is more complicated.  There are two
	       options: PCC (GCC) compatible and MS compatible */
            if (pcc) {
		/* In PCC layout a bit-field is placed adjacent to the
                   preceding bit-fields, except if:
                   - it has zero-width
                   - an individual alignment was given
                   - it would overflow its base type container and
                     there is no packing */
                if (bit_size == 0) {
            new_field:
		    c = (c + ((bit_pos + 7) >> 3) + align - 1) & -align;
		    bit_pos = 0;
                } else if (f->a.aligned) {
                    goto new_field;
                } else if (!packed) {
                    int a8 = align * 8;
	            int ofs = ((c * 8 + bit_pos) % a8 + bit_size + a8 - 1) / a8;
                    if (ofs > size / align)
                        goto new_field;
                }

                /* in pcc mode, long long bitfields have type int if they fit */
                if (size == 8 && bit_size <= 32)
                    f->type.t = (f->type.t & ~VT_BTYPE) | VT_INT, size = 4;

                while (bit_pos >= align * 8)
                    c += align, bit_pos -= align * 8;
                offset = c;

		/* In PCC layout named bit-fields influence the alignment
		   of the containing struct using the base types alignment,
		   except for packed fields (which here have correct align).  */
		if (f->v & SYM_FIRST_ANOM
                    // && bit_size // ??? gcc on ARM/rpi does that
                    )
		    align = 1;

	    } else {
		bt = f->type.t & VT_BTYPE;
		if ((bit_pos + bit_size > size * 8)
                    || (bit_size > 0) == (bt != prevbt)
                    ) {
		    c = (c + align - 1) & -align;
		    offset = c;
		    bit_pos = 0;
		    /* In MS bitfield mode a bit-field run always uses
		       at least as many bits as the underlying type.
		       To start a new run it's also required that this
		       or the last bit-field had non-zero width.  */
		    if (bit_size || prev_bit_size)
		        c += size;
		}
		/* In MS layout the records alignment is normally
		   influenced by the field, except for a zero-width
		   field at the start of a run (but by further zero-width
		   fields it is again).  */
		if (bit_size == 0 && prevbt != bt)
		    align = 1;
		prevbt = bt;
                prev_bit_size = bit_size;
	    }

	    f->type.t = (f->type.t & ~(0x3f << VT_STRUCT_SHIFT))
		        | (bit_pos << VT_STRUCT_SHIFT);
	    bit_pos += bit_size;
	}
	if (align > maxalign)
	    maxalign = align;

#ifdef BF_DEBUG
	printf("set field %s offset %-2d size %-2d align %-2d",
	       get_tok_str(f->v & ~SYM_FIELD, NULL), offset, size, align);
	if (f->type.t & VT_BITFIELD) {
	    printf(" pos %-2d bits %-2d",
                    BIT_POS(f->type.t),
                    BIT_SIZE(f->type.t)
                    );
	}
	printf("\n");
#endif

	if (f->v & SYM_FIRST_ANOM && (f->type.t & VT_BTYPE) == VT_STRUCT) {
	    Sym *ass;
	    /* An anonymous struct/union.  Adjust member offsets
	       to reflect the real offset of our containing struct.
	       Also set the offset of this anon member inside
	       the outer struct to be zero.  Via this it
	       works when accessing the field offset directly
	       (from base object), as well as when recursing
	       members in initializer handling.  */
	    int v2 = f->type.ref->v;
	    if (!(v2 & SYM_FIELD) &&
		(v2 & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
		Sym **pps;
		/* This happens only with MS extensions.  The
		   anon member has a named struct type, so it
		   potentially is shared with other references.
		   We need to unshare members so we can modify
		   them.  */
		ass = f->type.ref;
		f->type.ref = sym_push(anon_sym++ | SYM_FIELD,
				       &f->type.ref->type, 0,
				       f->type.ref->c);
		pps = &f->type.ref->next;
		while ((ass = ass->next) != NULL) {
		    *pps = sym_push(ass->v, &ass->type, 0, ass->c);
		    pps = &((*pps)->next);
		}
		*pps = NULL;
	    }
	    struct_add_offset(f->type.ref, offset);
	    f->c = 0;
	} else {
	    f->c = offset;
	}

	f->r = 0;
    }

    if (pcc)
        c += (bit_pos + 7) >> 3;

    /* store size and alignment */
    a = bt = ad->a.aligned ? 1 << (ad->a.aligned - 1) : 1;
    if (a < maxalign)
        a = maxalign;
    type->ref->r = a;
    if (pragma_pack && pragma_pack < maxalign && 0 == pcc) {
        /* can happen if individual align for some member was given.  In
           this case MSVC ignores maxalign when aligning the size */
        a = pragma_pack;
        if (a < bt)
            a = bt;
    }
    c = (c + a - 1) & -a;
    type->ref->c = c;

#ifdef BF_DEBUG
    printf("struct size %-2d align %-2d\n\n", c, a), fflush(stdout);
#endif

    /* check whether we can access bitfields by their type */
    for (f = type->ref->next; f; f = f->next) {
        int s, px, cx, c0;
        CType t;

        if (0 == (f->type.t & VT_BITFIELD))
            continue;
        f->type.ref = f;
        f->auxtype = -1;
        bit_size = BIT_SIZE(f->type.t);
        if (bit_size == 0)
            continue;
        bit_pos = BIT_POS(f->type.t);
        size = type_size(&f->type, &align);
        if (bit_pos + bit_size <= size * 8 && f->c + size <= c)
            continue;

        /* try to access the field using a different type */
        c0 = -1, s = align = 1;
        for (;;) {
            px = f->c * 8 + bit_pos;
            cx = (px >> 3) & -align;
            px = px - (cx << 3);
            if (c0 == cx)
                break;
            s = (px + bit_size + 7) >> 3;
            if (s > 4) {
                t.t = VT_LLONG;
            } else if (s > 2) {
                t.t = VT_INT;
            } else if (s > 1) {
                t.t = VT_SHORT;
            } else {
                t.t = VT_BYTE;
            }
            s = type_size(&t, &align);
            c0 = cx;
        }

        if (px + bit_size <= s * 8 && cx + s <= c) {
            /* update offset and bit position */
            f->c = cx;
            bit_pos = px;
	    f->type.t = (f->type.t & ~(0x3f << VT_STRUCT_SHIFT))
		        | (bit_pos << VT_STRUCT_SHIFT);
            if (s != size)
                f->auxtype = t.t;
#ifdef BF_DEBUG
            printf("FIX field %s offset %-2d size %-2d align %-2d "
                "pos %-2d bits %-2d\n",
                get_tok_str(f->v & ~SYM_FIELD, NULL),
                cx, s, align, px, bit_size);
#endif
        } else {
            /* fall back to load/store single-byte wise */
            f->auxtype = VT_STRUCT;
#ifdef BF_DEBUG
            printf("FIX field %s : load byte-wise\n",
                 get_tok_str(f->v & ~SYM_FIELD, NULL));
#endif
        }
    }
}

/* enum/struct/union declaration. u is VT_ENUM/VT_STRUCT/VT_UNION */
static void struct_decl(CType *type, int u)
{
    int v, c, size, align, flexible;
    int bit_size, bsize, bt;
    Sym *s, *ss, **ps;
    AttributeDef ad, ad1;
    CType type1, btype;

    memset(&ad, 0, sizeof ad);
    next();
    parse_attribute(&ad);
    if (tok != '{') {
        v = tok;
        next();
        /* struct already defined ? return it */
        if (v < TOK_IDENT)
            expect("struct/union/enum name");
        s = struct_find(v);
        if (s && (s->sym_scope == local_scope || tok != '{')) {
            if (u == s->type.t)
                goto do_decl;
            if (u == VT_ENUM && IS_ENUM(s->type.t))
                goto do_decl;
            tcc_error("redefinition of '%s'", get_tok_str(v, NULL));
        }
    } else {
        v = anon_sym++;
    }
    /* Record the original enum/struct/union token.  */
    type1.t = u == VT_ENUM ? u | VT_INT | VT_UNSIGNED : u;
    type1.ref = NULL;
    /* we put an undefined size for struct/union */
    s = sym_push(v | SYM_STRUCT, &type1, 0, -1);
    s->r = 0; /* default alignment is zero as gcc */
do_decl:
    type->t = s->type.t;
    type->ref = s;

    if (tok == '{') {
        next();
        if (s->c != -1)
            tcc_error("struct/union/enum already defined");
        /* cannot be empty */
        /* non empty enums are not allowed */
        ps = &s->next;
        if (u == VT_ENUM) {
            long long ll = 0, pl = 0, nl = 0;
	    CType t;
            t.ref = s;
            /* enum symbols have static storage */
            t.t = VT_INT|VT_STATIC|VT_ENUM_VAL;
            for(;;) {
                v = tok;
                if (v < TOK_UIDENT)
                    expect("identifier");
                ss = sym_find(v);
                if (ss && !local_stack)
                    tcc_error("redefinition of enumerator '%s'",
                              get_tok_str(v, NULL));
                next();
                if (tok == '=') {
                    next();
		    ll = expr_const64();
                }
                ss = sym_push(v, &t, VT_CONST, 0);
                ss->enum_val = ll;
                *ps = ss, ps = &ss->next;
                if (ll < nl)
                    nl = ll;
                if (ll > pl)
                    pl = ll;
                if (tok != ',')
                    break;
                next();
                ll++;
                /* NOTE: we accept a trailing comma */
                if (tok == '}')
                    break;
            }
            skip('}');
            /* set integral type of the enum */
            t.t = VT_INT;
            if (nl >= 0) {
                if (pl != (unsigned)pl)
                    t.t = (LONG_SIZE==8 ? VT_LLONG|VT_LONG : VT_LLONG);
                t.t |= VT_UNSIGNED;
            } else if (pl != (int)pl || nl != (int)nl)
                t.t = (LONG_SIZE==8 ? VT_LLONG|VT_LONG : VT_LLONG);
            s->type.t = type->t = t.t | VT_ENUM;
            s->c = 0;
            /* set type for enum members */
            for (ss = s->next; ss; ss = ss->next) {
                ll = ss->enum_val;
                if (ll == (int)ll) /* default is int if it fits */
                    continue;
                if (t.t & VT_UNSIGNED) {
                    ss->type.t |= VT_UNSIGNED;
                    if (ll == (unsigned)ll)
                        continue;
                }
                ss->type.t = (ss->type.t & ~VT_BTYPE)
                    | (LONG_SIZE==8 ? VT_LLONG|VT_LONG : VT_LLONG);
            }
        } else {
            c = 0;
            flexible = 0;
            while (tok != '}') {
                if (!parse_btype(&btype, &ad1)) {
		    skip(';');
		    continue;
		}
                while (1) {
		    if (flexible)
		        tcc_error("flexible array member '%s' not at the end of struct",
                              get_tok_str(v, NULL));
                    bit_size = -1;
                    v = 0;
                    type1 = btype;
                    if (tok != ':') {
			if (tok != ';')
                            type_decl(&type1, &ad1, &v, TYPE_DIRECT);
                        if (v == 0) {
                    	    if ((type1.t & VT_BTYPE) != VT_STRUCT)
                        	expect("identifier");
                    	    else {
				int v = btype.ref->v;
				if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
				    if (tcc_state->ms_extensions == 0)
                        		expect("identifier");
				}
                    	    }
                        }
                        if (type_size(&type1, &align) < 0) {
			    if ((u == VT_STRUCT) && (type1.t & VT_ARRAY) && c)
			        flexible = 1;
			    else
			        tcc_error("field '%s' has incomplete type",
                                      get_tok_str(v, NULL));
                        }
                        if ((type1.t & VT_BTYPE) == VT_FUNC ||
                            (type1.t & VT_STORAGE))
                            tcc_error("invalid type for '%s'", 
                                  get_tok_str(v, NULL));
                    }
                    if (tok == ':') {
                        next();
                        bit_size = expr_const();
                        /* XXX: handle v = 0 case for messages */
                        if (bit_size < 0)
                            tcc_error("negative width in bit-field '%s'", 
                                  get_tok_str(v, NULL));
                        if (v && bit_size == 0)
                            tcc_error("zero width for bit-field '%s'", 
                                  get_tok_str(v, NULL));
			parse_attribute(&ad1);
                    }
                    size = type_size(&type1, &align);
                    if (bit_size >= 0) {
                        bt = type1.t & VT_BTYPE;
                        if (bt != VT_INT && 
                            bt != VT_BYTE && 
                            bt != VT_SHORT &&
                            bt != VT_BOOL &&
                            bt != VT_LLONG)
                            tcc_error("bitfields must have scalar type");
                        bsize = size * 8;
                        if (bit_size > bsize) {
                            tcc_error("width of '%s' exceeds its type",
                                  get_tok_str(v, NULL));
                        } else if (bit_size == bsize
                                    && !ad.a.packed && !ad1.a.packed) {
                            /* no need for bit fields */
                            ;
                        } else if (bit_size == 64) {
                            tcc_error("field width 64 not implemented");
                        } else {
                            type1.t = (type1.t & ~VT_STRUCT_MASK)
                                | VT_BITFIELD
                                | (bit_size << (VT_STRUCT_SHIFT + 6));
                        }
                    }
                    if (v != 0 || (type1.t & VT_BTYPE) == VT_STRUCT) {
                        /* Remember we've seen a real field to check
			   for placement of flexible array member. */
			c = 1;
                    }
		    /* If member is a struct or bit-field, enforce
		       placing into the struct (as anonymous).  */
                    if (v == 0 &&
			((type1.t & VT_BTYPE) == VT_STRUCT ||
			 bit_size >= 0)) {
		        v = anon_sym++;
		    }
                    if (v) {
                        ss = sym_push(v | SYM_FIELD, &type1, 0, 0);
                        ss->a = ad1.a;
                        *ps = ss;
                        ps = &ss->next;
                    }
                    if (tok == ';' || tok == TOK_EOF)
                        break;
                    skip(',');
                }
                skip(';');
            }
            skip('}');
	    parse_attribute(&ad);
	    struct_layout(type, &ad);
        }
    }
}

static void sym_to_attr(AttributeDef *ad, Sym *s)
{
    if (s->a.aligned && 0 == ad->a.aligned)
        ad->a.aligned = s->a.aligned;
    if (s->f.func_call && 0 == ad->f.func_call)
        ad->f.func_call = s->f.func_call;
    if (s->f.func_type && 0 == ad->f.func_type)
        ad->f.func_type = s->f.func_type;
    if (s->a.packed)
        ad->a.packed = 1;
}

/* Add type qualifiers to a type. If the type is an array then the qualifiers
   are added to the element type, copied because it could be a typedef. */
static void parse_btype_qualify(CType *type, int qualifiers)
{
    while (type->t & VT_ARRAY) {
        type->ref = sym_push(SYM_FIELD, &type->ref->type, 0, type->ref->c);
        type = &type->ref->type;
    }
    type->t |= qualifiers;
}

/* return 0 if no type declaration. otherwise, return the basic type
   and skip it. 
 */
static int parse_btype(CType *type, AttributeDef *ad)
{
    int t, u, bt, st, type_found, typespec_found, g;
    Sym *s;
    CType type1;

    memset(ad, 0, sizeof(AttributeDef));
    type_found = 0;
    typespec_found = 0;
    t = VT_INT;
    bt = st = -1;
    type->ref = NULL;

    while(1) {
        switch(tok) {
        case TOK_EXTENSION:
            /* currently, we really ignore extension */
            next();
            continue;

            /* basic types */
        case TOK_CHAR:
            u = VT_BYTE;
        basic_type:
            next();
        basic_type1:
            if (u == VT_SHORT || u == VT_LONG) {
                if (st != -1 || (bt != -1 && bt != VT_INT))
                    tmbt: tcc_error("too many basic types");
                st = u;
            } else {
                if (bt != -1 || (st != -1 && u != VT_INT))
                    goto tmbt;
                bt = u;
            }
            if (u != VT_INT)
                t = (t & ~(VT_BTYPE|VT_LONG)) | u;
            typespec_found = 1;
            break;
        case TOK_VOID:
            u = VT_VOID;
            goto basic_type;
        case TOK_SHORT:
            u = VT_SHORT;
            goto basic_type;
        case TOK_INT:
            u = VT_INT;
            goto basic_type;
        case TOK_LONG:
            if ((t & VT_BTYPE) == VT_DOUBLE) {
                t = (t & ~(VT_BTYPE|VT_LONG)) | VT_LDOUBLE;
            } else if ((t & (VT_BTYPE|VT_LONG)) == VT_LONG) {
                t = (t & ~(VT_BTYPE|VT_LONG)) | VT_LLONG;
            } else {
                u = VT_LONG;
                goto basic_type;
            }
            next();
            break;
#ifdef TCC_TARGET_ARM64
        case TOK_UINT128:
            /* GCC's __uint128_t appears in some Linux header files. Make it a
               synonym for long double to get the size and alignment right. */
            u = VT_LDOUBLE;
            goto basic_type;
#endif
        case TOK_BOOL:
            u = VT_BOOL;
            goto basic_type;
        case TOK_FLOAT:
            u = VT_FLOAT;
            goto basic_type;
        case TOK_DOUBLE:
            if ((t & (VT_BTYPE|VT_LONG)) == VT_LONG) {
                t = (t & ~(VT_BTYPE|VT_LONG)) | VT_LDOUBLE;
            } else {
                u = VT_DOUBLE;
                goto basic_type;
            }
            next();
            break;
        case TOK_ENUM:
            struct_decl(&type1, VT_ENUM);
        basic_type2:
            u = type1.t;
            type->ref = type1.ref;
            goto basic_type1;
        case TOK_STRUCT:
            struct_decl(&type1, VT_STRUCT);
            goto basic_type2;
        case TOK_UNION:
            struct_decl(&type1, VT_UNION);
            goto basic_type2;

            /* type modifiers */
        case TOK_CONST1:
        case TOK_CONST2:
        case TOK_CONST3:
            type->t = t;
            parse_btype_qualify(type, VT_CONSTANT);
            t = type->t;
            next();
            break;
        case TOK_VOLATILE1:
        case TOK_VOLATILE2:
        case TOK_VOLATILE3:
            type->t = t;
            parse_btype_qualify(type, VT_VOLATILE);
            t = type->t;
            next();
            break;
        case TOK_SIGNED1:
        case TOK_SIGNED2:
        case TOK_SIGNED3:
            if ((t & (VT_DEFSIGN|VT_UNSIGNED)) == (VT_DEFSIGN|VT_UNSIGNED))
                tcc_error("signed and unsigned modifier");
            t |= VT_DEFSIGN;
            next();
            typespec_found = 1;
            break;
        case TOK_REGISTER:
        case TOK_AUTO:
        case TOK_RESTRICT1:
        case TOK_RESTRICT2:
        case TOK_RESTRICT3:
            next();
            break;
        case TOK_UNSIGNED:
            if ((t & (VT_DEFSIGN|VT_UNSIGNED)) == VT_DEFSIGN)
                tcc_error("signed and unsigned modifier");
            t |= VT_DEFSIGN | VT_UNSIGNED;
            next();
            typespec_found = 1;
            break;

            /* storage */
        case TOK_EXTERN:
            g = VT_EXTERN;
            goto storage;
        case TOK_STATIC:
            g = VT_STATIC;
            goto storage;
        case TOK_TYPEDEF:
            g = VT_TYPEDEF;
            goto storage;
       storage:
            if (t & (VT_EXTERN|VT_STATIC|VT_TYPEDEF) & ~g)
                tcc_error("multiple storage classes");
            t |= g;
            next();
            break;
        case TOK_INLINE1:
        case TOK_INLINE2:
        case TOK_INLINE3:
            t |= VT_INLINE;
            next();
            break;

            /* GNUC attribute */
        case TOK_ATTRIBUTE1:
        case TOK_ATTRIBUTE2:
            parse_attribute(ad);
            if (ad->attr_mode) {
                u = ad->attr_mode -1;
                t = (t & ~(VT_BTYPE|VT_LONG)) | u;
            }
            break;
            /* GNUC typeof */
        case TOK_TYPEOF1:
        case TOK_TYPEOF2:
        case TOK_TYPEOF3:
            next();
            parse_expr_type(&type1);
            /* remove all storage modifiers except typedef */
            type1.t &= ~(VT_STORAGE&~VT_TYPEDEF);
	    if (type1.ref)
                sym_to_attr(ad, type1.ref);
            goto basic_type2;
        default:
            if (typespec_found)
                goto the_end;
            s = sym_find(tok);
            if (!s || !(s->type.t & VT_TYPEDEF))
                goto the_end;
            t &= ~(VT_BTYPE|VT_LONG);
            u = t & ~(VT_CONSTANT | VT_VOLATILE), t ^= u;
            type->t = (s->type.t & ~VT_TYPEDEF) | u;
            type->ref = s->type.ref;
            if (t)
                parse_btype_qualify(type, t);
            t = type->t;
            /* get attributes from typedef */
            sym_to_attr(ad, s);
            next();
            typespec_found = 1;
            st = bt = -2;
            break;
        }
        type_found = 1;
    }
the_end:
    if (tcc_state->char_is_unsigned) {
        if ((t & (VT_DEFSIGN|VT_BTYPE)) == VT_BYTE)
            t |= VT_UNSIGNED;
    }
    /* VT_LONG is used just as a modifier for VT_INT / VT_LLONG */
    bt = t & (VT_BTYPE|VT_LONG);
    if (bt == VT_LONG)
        t |= LONG_SIZE == 8 ? VT_LLONG : VT_INT;
#ifdef TCC_TARGET_PE
    if (bt == VT_LDOUBLE)
        t = (t & ~(VT_BTYPE|VT_LONG)) | VT_DOUBLE;
#endif
    type->t = t;
    return type_found;
}

/* convert a function parameter type (array to pointer and function to
   function pointer) */
static inline void convert_parameter_type(CType *pt)
{
    /* remove const and volatile qualifiers (XXX: const could be used
       to indicate a const function parameter */
    pt->t &= ~(VT_CONSTANT | VT_VOLATILE);
    /* array must be transformed to pointer according to ANSI C */
    pt->t &= ~VT_ARRAY;
    if ((pt->t & VT_BTYPE) == VT_FUNC) {
        mk_pointer(pt);
    }
}

ST_FUNC void parse_asm_str(CString *astr)
{
    skip('(');
    parse_mult_str(astr, "string constant");
}

/* Parse an asm label and return the token */
static int asm_label_instr(void)
{
    int v;
    CString astr;

    next();
    parse_asm_str(&astr);
    skip(')');
#ifdef ASM_DEBUG
    printf("asm_alias: \"%s\"\n", (char *)astr.data);
#endif
    v = tok_alloc(astr.data, astr.size - 1)->tok;
    cstr_free(&astr);
    return v;
}

static int post_type(CType *type, AttributeDef *ad, int storage, int td)
{
    int n, l, t1, arg_size, align;
    Sym **plast, *s, *first;
    AttributeDef ad1;
    CType pt;

    if (tok == '(') {
        /* function type, or recursive declarator (return if so) */
        next();
	if (td && !(td & TYPE_ABSTRACT))
	  return 0;
	if (tok == ')')
	  l = 0;
	else if (parse_btype(&pt, &ad1))
	  l = FUNC_NEW;
	else if (td)
	  return 0;
	else
	  l = FUNC_OLD;
        first = NULL;
        plast = &first;
        arg_size = 0;
        if (l) {
            for(;;) {
                /* read param name and compute offset */
                if (l != FUNC_OLD) {
                    if ((pt.t & VT_BTYPE) == VT_VOID && tok == ')')
                        break;
                    type_decl(&pt, &ad1, &n, TYPE_DIRECT | TYPE_ABSTRACT);
                    if ((pt.t & VT_BTYPE) == VT_VOID)
                        tcc_error("parameter declared as void");
                    arg_size += (type_size(&pt, &align) + PTR_SIZE - 1) / PTR_SIZE;
                } else {
                    n = tok;
                    if (n < TOK_UIDENT)
                        expect("identifier");
                    pt.t = VT_VOID; /* invalid type */
                    next();
                }
                convert_parameter_type(&pt);
                s = sym_push(n | SYM_FIELD, &pt, 0, 0);
                *plast = s;
                plast = &s->next;
                if (tok == ')')
                    break;
                skip(',');
                if (l == FUNC_NEW && tok == TOK_DOTS) {
                    l = FUNC_ELLIPSIS;
                    next();
                    break;
                }
		if (l == FUNC_NEW && !parse_btype(&pt, &ad1))
		    tcc_error("invalid type");
            }
        } else
            /* if no parameters, then old type prototype */
            l = FUNC_OLD;
        skip(')');
        /* NOTE: const is ignored in returned type as it has a special
           meaning in gcc / C++ */
        type->t &= ~VT_CONSTANT; 
        /* some ancient pre-K&R C allows a function to return an array
           and the array brackets to be put after the arguments, such 
           that "int c()[]" means something like "int[] c()" */
        if (tok == '[') {
            next();
            skip(']'); /* only handle simple "[]" */
            mk_pointer(type);
        }
        /* we push a anonymous symbol which will contain the function prototype */
        ad->f.func_args = arg_size;
        ad->f.func_type = l;
        s = sym_push(SYM_FIELD, type, 0, 0);
        s->a = ad->a;
        s->f = ad->f;
        s->next = first;
        type->t = VT_FUNC;
        type->ref = s;
    } else if (tok == '[') {
	int saved_nocode_wanted = nocode_wanted;
        /* array definition */
        next();
        if (tok == TOK_RESTRICT1)
            next();
        n = -1;
        t1 = 0;
        if (tok != ']') {
            if (!local_stack || (storage & VT_STATIC))
                vpushi(expr_const());
            else {
		/* VLAs (which can only happen with local_stack && !VT_STATIC)
		   length must always be evaluated, even under nocode_wanted,
		   so that its size slot is initialized (e.g. under sizeof
		   or typeof).  */
		nocode_wanted = 0;
		gexpr();
	    }
            if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                n = vtop->c.i;
                if (n < 0)
                    tcc_error("invalid array size");
            } else {
                if (!is_integer_btype(vtop->type.t & VT_BTYPE))
                    tcc_error("size of variable length array should be an integer");
                t1 = VT_VLA;
            }
        }
        skip(']');
        /* parse next post type */
        post_type(type, ad, storage, 0);
        if (type->t == VT_FUNC)
            tcc_error("declaration of an array of functions");
        t1 |= type->t & VT_VLA;
        
        if (t1 & VT_VLA) {
            loc -= type_size(&int_type, &align);
            loc &= -align;
            n = loc;

            vla_runtime_type_size(type, &align);
            gen_op('*');
            vset(&int_type, VT_LOCAL|VT_LVAL, n);
            vswap();
            vstore();
        }
        if (n != -1)
            vpop();
	nocode_wanted = saved_nocode_wanted;
                
        /* we push an anonymous symbol which will contain the array
           element type */
        s = sym_push(SYM_FIELD, type, 0, n);
        type->t = (t1 ? VT_VLA : VT_ARRAY) | VT_PTR;
        type->ref = s;
    }
    return 1;
}

/* Parse a type declarator (except basic type), and return the type
   in 'type'. 'td' is a bitmask indicating which kind of type decl is
   expected. 'type' should contain the basic type. 'ad' is the
   attribute definition of the basic type. It can be modified by
   type_decl().  If this (possibly abstract) declarator is a pointer chain
   it returns the innermost pointed to type (equals *type, but is a different
   pointer), otherwise returns type itself, that's used for recursive calls.  */
static CType *type_decl(CType *type, AttributeDef *ad, int *v, int td)
{
    CType *post, *ret;
    int qualifiers, storage;

    /* recursive type, remove storage bits first, apply them later again */
    storage = type->t & VT_STORAGE;
    type->t &= ~VT_STORAGE;
    post = ret = type;

    while (tok == '*') {
        qualifiers = 0;
    redo:
        next();
        switch(tok) {
        case TOK_CONST1:
        case TOK_CONST2:
        case TOK_CONST3:
            qualifiers |= VT_CONSTANT;
            goto redo;
        case TOK_VOLATILE1:
        case TOK_VOLATILE2:
        case TOK_VOLATILE3:
            qualifiers |= VT_VOLATILE;
            goto redo;
        case TOK_RESTRICT1:
        case TOK_RESTRICT2:
        case TOK_RESTRICT3:
            goto redo;
	/* XXX: clarify attribute handling */
	case TOK_ATTRIBUTE1:
	case TOK_ATTRIBUTE2:
	    parse_attribute(ad);
	    break;
        }
        mk_pointer(type);
        type->t |= qualifiers;
	if (ret == type)
	    /* innermost pointed to type is the one for the first derivation */
	    ret = pointed_type(type);
    }

    if (tok == '(') {
	/* This is possibly a parameter type list for abstract declarators
	   ('int ()'), use post_type for testing this.  */
	if (!post_type(type, ad, 0, td)) {
	    /* It's not, so it's a nested declarator, and the post operations
	       apply to the innermost pointed to type (if any).  */
	    /* XXX: this is not correct to modify 'ad' at this point, but
	       the syntax is not clear */
	    parse_attribute(ad);
	    post = type_decl(type, ad, v, td);
	    skip(')');
	}
    } else if (tok >= TOK_IDENT && (td & TYPE_DIRECT)) {
	/* type identifier */
	*v = tok;
	next();
    } else {
	if (!(td & TYPE_ABSTRACT))
	  expect("identifier");
	*v = 0;
    }
    post_type(post, ad, storage, 0);
    parse_attribute(ad);
    type->t |= storage;
    return ret;
}

/* compute the lvalue VT_LVAL_xxx needed to match type t. */
ST_FUNC int lvalue_type(int t)
{
    int bt, r;
    r = VT_LVAL;
    bt = t & VT_BTYPE;
    if (bt == VT_BYTE || bt == VT_BOOL)
        r |= VT_LVAL_BYTE;
    else if (bt == VT_SHORT)
        r |= VT_LVAL_SHORT;
    else
        return r;
    if (t & VT_UNSIGNED)
        r |= VT_LVAL_UNSIGNED;
    return r;
}

/* indirection with full error checking and bound check */
ST_FUNC void indir(void)
{
    if ((vtop->type.t & VT_BTYPE) != VT_PTR) {
        if ((vtop->type.t & VT_BTYPE) == VT_FUNC)
            return;
        expect("pointer");
    }
    if (vtop->r & VT_LVAL)
        gv(RC_INT);
    vtop->type = *pointed_type(&vtop->type);
    /* Arrays and functions are never lvalues */
    if (!(vtop->type.t & VT_ARRAY) && !(vtop->type.t & VT_VLA)
        && (vtop->type.t & VT_BTYPE) != VT_FUNC) {
        vtop->r |= lvalue_type(vtop->type.t);
        /* if bound checking, the referenced pointer must be checked */
#ifdef CONFIG_TCC_BCHECK
        if (tcc_state->do_bounds_check)
            vtop->r |= VT_MUSTBOUND;
#endif
    }
}

/* pass a parameter to a function and do type checking and casting */
static void gfunc_param_typed(Sym *func, Sym *arg)
{
    int func_type;
    CType type;

    func_type = func->f.func_type;
    if (func_type == FUNC_OLD ||
        (func_type == FUNC_ELLIPSIS && arg == NULL)) {
        /* default casting : only need to convert float to double */
        if ((vtop->type.t & VT_BTYPE) == VT_FLOAT) {
            gen_cast_s(VT_DOUBLE);
        } else if (vtop->type.t & VT_BITFIELD) {
            type.t = vtop->type.t & (VT_BTYPE | VT_UNSIGNED);
	    type.ref = vtop->type.ref;
            gen_cast(&type);
        }
    } else if (arg == NULL) {
        tcc_error("too many arguments to function");
    } else {
        type = arg->type;
        type.t &= ~VT_CONSTANT; /* need to do that to avoid false warning */
        gen_assign_cast(&type);
    }
}

/* parse an expression and return its type without any side effect. */
static void expr_type(CType *type, void (*expr_fn)(void))
{
    nocode_wanted++;
    expr_fn();
    *type = vtop->type;
    vpop();
    nocode_wanted--;
}

/* parse an expression of the form '(type)' or '(expr)' and return its
   type */
static void parse_expr_type(CType *type)
{
    int n;
    AttributeDef ad;

    skip('(');
    if (parse_btype(type, &ad)) {
        type_decl(type, &ad, &n, TYPE_ABSTRACT);
    } else {
        expr_type(type, gexpr);
    }
    skip(')');
}

static void parse_type(CType *type)
{
    AttributeDef ad;
    int n;

    if (!parse_btype(type, &ad)) {
        expect("type");
    }
    type_decl(type, &ad, &n, TYPE_ABSTRACT);
}

static void parse_builtin_params(int nc, const char *args)
{
    char c, sep = '(';
    CType t;
    if (nc)
        nocode_wanted++;
    next();
    while ((c = *args++)) {
	skip(sep);
	sep = ',';
	switch (c) {
	    case 'e': expr_eq(); continue;
	    case 't': parse_type(&t); vpush(&t); continue;
	    default: tcc_error("internal error"); break;
	}
    }
    skip(')');
    if (nc)
        nocode_wanted--;
}

ST_FUNC void unary(void)
{
    int n, t, align, size, r, sizeof_caller;
    CType type;
    Sym *s;
    AttributeDef ad;

    sizeof_caller = in_sizeof;
    in_sizeof = 0;
    type.ref = NULL;
    /* XXX: GCC 2.95.3 does not generate a table although it should be
       better here */
 tok_next:
    switch(tok) {
    case TOK_EXTENSION:
        next();
        goto tok_next;
    case TOK_LCHAR:
#ifdef TCC_TARGET_PE
        t = VT_SHORT|VT_UNSIGNED;
        goto push_tokc;
#endif
    case TOK_CINT:
    case TOK_CCHAR: 
	t = VT_INT;
 push_tokc:
	type.t = t;
	vsetc(&type, VT_CONST, &tokc);
        next();
        break;
    case TOK_CUINT:
        t = VT_INT | VT_UNSIGNED;
        goto push_tokc;
    case TOK_CLLONG:
        t = VT_LLONG;
	goto push_tokc;
    case TOK_CULLONG:
        t = VT_LLONG | VT_UNSIGNED;
	goto push_tokc;
    case TOK_CFLOAT:
        t = VT_FLOAT;
	goto push_tokc;
    case TOK_CDOUBLE:
        t = VT_DOUBLE;
	goto push_tokc;
    case TOK_CLDOUBLE:
        t = VT_LDOUBLE;
	goto push_tokc;
    case TOK_CLONG:
        t = (LONG_SIZE == 8 ? VT_LLONG : VT_INT) | VT_LONG;
	goto push_tokc;
    case TOK_CULONG:
        t = (LONG_SIZE == 8 ? VT_LLONG : VT_INT) | VT_LONG | VT_UNSIGNED;
	goto push_tokc;
    case TOK___FUNCTION__:
        if (!gnu_ext)
            goto tok_identifier;
        /* fall thru */
    case TOK___FUNC__:
        {
            void *ptr;
            int len;
            /* special function name identifier */
            len = strlen(funcname) + 1;
            /* generate char[len] type */
            type.t = VT_BYTE;
            mk_pointer(&type);
            type.t |= VT_ARRAY;
            type.ref->c = len;
            vpush_ref(&type, data_section, data_section->data_offset, len);
            if (!NODATA_WANTED) {
                ptr = section_ptr_add(data_section, len);
                memcpy(ptr, funcname, len);
            }
            next();
        }
        break;
    case TOK_LSTR:
#ifdef TCC_TARGET_PE
        t = VT_SHORT | VT_UNSIGNED;
#else
        t = VT_INT;
#endif
        goto str_init;
    case TOK_STR:
        /* string parsing */
        t = VT_BYTE;
        if (tcc_state->char_is_unsigned)
            t = VT_BYTE | VT_UNSIGNED;
    str_init:
        if (tcc_state->warn_write_strings)
            t |= VT_CONSTANT;
        type.t = t;
        mk_pointer(&type);
        type.t |= VT_ARRAY;
        memset(&ad, 0, sizeof(AttributeDef));
        decl_initializer_alloc(&type, &ad, VT_CONST, 2, 0, 0);
        break;
    case '(':
        next();
        /* cast ? */
        if (parse_btype(&type, &ad)) {
            type_decl(&type, &ad, &n, TYPE_ABSTRACT);
            skip(')');
            /* check ISOC99 compound literal */
            if (tok == '{') {
                    /* data is allocated locally by default */
                if (global_expr)
                    r = VT_CONST;
                else
                    r = VT_LOCAL;
                /* all except arrays are lvalues */
                if (!(type.t & VT_ARRAY))
                    r |= lvalue_type(type.t);
                memset(&ad, 0, sizeof(AttributeDef));
                decl_initializer_alloc(&type, &ad, r, 1, 0, 0);
            } else {
                if (sizeof_caller) {
                    vpush(&type);
                    return;
                }
                unary();
                gen_cast(&type);
            }
        } else if (tok == '{') {
	    int saved_nocode_wanted = nocode_wanted;
            if (const_wanted)
                tcc_error("expected constant");
            /* save all registers */
            save_regs(0);
            /* statement expression : we do not accept break/continue
               inside as GCC does.  We do retain the nocode_wanted state,
	       as statement expressions can't ever be entered from the
	       outside, so any reactivation of code emission (from labels
	       or loop heads) can be disabled again after the end of it. */
            block(NULL, NULL, 1);
	    nocode_wanted = saved_nocode_wanted;
            skip(')');
        } else {
            gexpr();
            skip(')');
        }
        break;
    case '*':
        next();
        unary();
        indir();
        break;
    case '&':
        next();
        unary();
        /* functions names must be treated as function pointers,
           except for unary '&' and sizeof. Since we consider that
           functions are not lvalues, we only have to handle it
           there and in function calls. */
        /* arrays can also be used although they are not lvalues */
        if ((vtop->type.t & VT_BTYPE) != VT_FUNC &&
            !(vtop->type.t & VT_ARRAY))
            test_lvalue();
        mk_pointer(&vtop->type);
        gaddrof();
        break;
    case '!':
        next();
        unary();
        if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
            gen_cast_s(VT_BOOL);
            vtop->c.i = !vtop->c.i;
        } else if ((vtop->r & VT_VALMASK) == VT_CMP)
            vtop->c.i ^= 1;
        else {
            save_regs(1);
            vseti(VT_JMP, gvtst(1, 0));
        }
        break;
    case '~':
        next();
        unary();
        vpushi(-1);
        gen_op('^');
        break;
    case '+':
        next();
        unary();
        if ((vtop->type.t & VT_BTYPE) == VT_PTR)
            tcc_error("pointer not accepted for unary plus");
        /* In order to force cast, we add zero, except for floating point
	   where we really need an noop (otherwise -0.0 will be transformed
	   into +0.0).  */
	if (!is_float(vtop->type.t)) {
	    vpushi(0);
	    gen_op('+');
	}
        break;
    case TOK_SIZEOF:
    case TOK_ALIGNOF1:
    case TOK_ALIGNOF2:
        t = tok;
        next();
        in_sizeof++;
        expr_type(&type, unary); /* Perform a in_sizeof = 0; */
        s = vtop[1].sym; /* hack: accessing previous vtop */
        size = type_size(&type, &align);
        if (s && s->a.aligned)
            align = 1 << (s->a.aligned - 1);
        if (t == TOK_SIZEOF) {
            if (!(type.t & VT_VLA)) {
                if (size < 0)
                    tcc_error("sizeof applied to an incomplete type");
                vpushs(size);
            } else {
                vla_runtime_type_size(&type, &align);
            }
        } else {
            vpushs(align);
        }
        vtop->type.t |= VT_UNSIGNED;
        break;

    case TOK_builtin_expect:
	/* __builtin_expect is a no-op for now */
	parse_builtin_params(0, "ee");
	vpop();
        break;
    case TOK_builtin_types_compatible_p:
	parse_builtin_params(0, "tt");
	vtop[-1].type.t &= ~(VT_CONSTANT | VT_VOLATILE);
	vtop[0].type.t &= ~(VT_CONSTANT | VT_VOLATILE);
	n = is_compatible_types(&vtop[-1].type, &vtop[0].type);
	vtop -= 2;
	vpushi(n);
        break;
    case TOK_builtin_choose_expr:
	{
	    int64_t c;
	    next();
	    skip('(');
	    c = expr_const64();
	    skip(',');
	    if (!c) {
		nocode_wanted++;
	    }
	    expr_eq();
	    if (!c) {
		vpop();
		nocode_wanted--;
	    }
	    skip(',');
	    if (c) {
		nocode_wanted++;
	    }
	    expr_eq();
	    if (c) {
		vpop();
		nocode_wanted--;
	    }
	    skip(')');
	}
        break;
    case TOK_builtin_constant_p:
	parse_builtin_params(1, "e");
	n = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
	vtop--;
	vpushi(n);
        break;
    case TOK_builtin_frame_address:
    case TOK_builtin_return_address:
        {
            int tok1 = tok;
            int level;
            next();
            skip('(');
            if (tok != TOK_CINT) {
                tcc_error("%s only takes positive integers",
                          tok1 == TOK_builtin_return_address ?
                          "__builtin_return_address" :
                          "__builtin_frame_address");
            }
            level = (uint32_t)tokc.i;
            next();
            skip(')');
            type.t = VT_VOID;
            mk_pointer(&type);
            vset(&type, VT_LOCAL, 0);       /* local frame */
            while (level--) {
                mk_pointer(&vtop->type);
                indir();                    /* -> parent frame */
            }
            if (tok1 == TOK_builtin_return_address) {
                // assume return address is just above frame pointer on stack
                vpushi(PTR_SIZE);
                gen_op('+');
                mk_pointer(&vtop->type);
                indir();
            }
        }
        break;
#ifdef TCC_TARGET_X86_64
#ifdef TCC_TARGET_PE
    case TOK_builtin_va_start:
	parse_builtin_params(0, "ee");
        r = vtop->r & VT_VALMASK;
        if (r == VT_LLOCAL)
            r = VT_LOCAL;
        if (r != VT_LOCAL)
            tcc_error("__builtin_va_start expects a local variable");
        vtop->r = r;
	vtop->type = char_pointer_type;
	vtop->c.i += 8;
	vstore();
        break;
#else
    case TOK_builtin_va_arg_types:
	parse_builtin_params(0, "t");
	vpushi(classify_x86_64_va_arg(&vtop->type));
	vswap();
	vpop();
        break;
#endif
#endif

#ifdef TCC_TARGET_ARM64
    case TOK___va_start: {
	parse_builtin_params(0, "ee");
        //xx check types
        gen_va_start();
        vpushi(0);
        vtop->type.t = VT_VOID;
        break;
    }
    case TOK___va_arg: {
	parse_builtin_params(0, "et");
	type = vtop->type;
	vpop();
        //xx check types
        gen_va_arg(&type);
        vtop->type = type;
        break;
    }
    case TOK___arm64_clear_cache: {
	parse_builtin_params(0, "ee");
        gen_clear_cache();
        vpushi(0);
        vtop->type.t = VT_VOID;
        break;
    }
#endif
    /* pre operations */
    case TOK_INC:
    case TOK_DEC:
        t = tok;
        next();
        unary();
        inc(0, t);
        break;
    case '-':
        next();
        unary();
        t = vtop->type.t & VT_BTYPE;
	if (is_float(t)) {
            /* In IEEE negate(x) isn't subtract(0,x), but rather
	       subtract(-0, x).  */
	    vpush(&vtop->type);
	    if (t == VT_FLOAT)
	        vtop->c.f = -1.0 * 0.0;
	    else if (t == VT_DOUBLE)
	        vtop->c.d = -1.0 * 0.0;
	    else
	        vtop->c.ld = -1.0 * 0.0;
	} else
	    vpushi(0);
	vswap();
	gen_op('-');
        break;
    case TOK_LAND:
        if (!gnu_ext)
            goto tok_identifier;
        next();
        /* allow to take the address of a label */
        if (tok < TOK_UIDENT)
            expect("label identifier");
        s = label_find(tok);
        if (!s) {
            s = label_push(&global_label_stack, tok, LABEL_FORWARD);
        } else {
            if (s->r == LABEL_DECLARED)
                s->r = LABEL_FORWARD;
        }
        if (!s->type.t) {
            s->type.t = VT_VOID;
            mk_pointer(&s->type);
            s->type.t |= VT_STATIC;
        }
        vpushsym(&s->type, s);
        next();
        break;

    case TOK_GENERIC:
    {
	CType controlling_type;
	int has_default = 0;
	int has_match = 0;
	int learn = 0;
	TokenString *str = NULL;

	next();
	skip('(');
	expr_type(&controlling_type, expr_eq);
	controlling_type.t &= ~(VT_CONSTANT | VT_VOLATILE | VT_ARRAY);
	for (;;) {
	    learn = 0;
	    skip(',');
	    if (tok == TOK_DEFAULT) {
		if (has_default)
		    tcc_error("too many 'default'");
		has_default = 1;
		if (!has_match)
		    learn = 1;
		next();
	    } else {
	        AttributeDef ad_tmp;
		int itmp;
	        CType cur_type;
		parse_btype(&cur_type, &ad_tmp);
		type_decl(&cur_type, &ad_tmp, &itmp, TYPE_ABSTRACT);
		if (compare_types(&controlling_type, &cur_type, 0)) {
		    if (has_match) {
		      tcc_error("type match twice");
		    }
		    has_match = 1;
		    learn = 1;
		}
	    }
	    skip(':');
	    if (learn) {
		if (str)
		    tok_str_free(str);
		skip_or_save_block(&str);
	    } else {
		skip_or_save_block(NULL);
	    }
	    if (tok == ')')
		break;
	}
	if (!str) {
	    char buf[60];
	    type_to_str(buf, sizeof buf, &controlling_type, NULL);
	    tcc_error("type '%s' does not match any association", buf);
	}
	begin_macro(str, 1);
	next();
	expr_eq();
	if (tok != TOK_EOF)
	    expect(",");
	end_macro();
        next();
	break;
    }
    // special qnan , snan and infinity values
    case TOK___NAN__:
        vpush64(VT_DOUBLE, 0x7ff8000000000000ULL);
        next();
        break;
    case TOK___SNAN__:
        vpush64(VT_DOUBLE, 0x7ff0000000000001ULL);
        next();
        break;
    case TOK___INF__:
        vpush64(VT_DOUBLE, 0x7ff0000000000000ULL);
        next();
        break;

    default:
    tok_identifier:
        t = tok;
        next();
        if (t < TOK_UIDENT)
            expect("identifier");
        s = sym_find(t);
        if (!s || IS_ASM_SYM(s)) {
            const char *name = get_tok_str(t, NULL);
            if (tok != '(')
                tcc_error("'%s' undeclared", name);
            /* for simple function calls, we tolerate undeclared
               external reference to int() function */
            if (tcc_state->warn_implicit_function_declaration
#ifdef TCC_TARGET_PE
                /* people must be warned about using undeclared WINAPI functions
                   (which usually start with uppercase letter) */
                || (name[0] >= 'A' && name[0] <= 'Z')
#endif
            )
                tcc_warning("implicit declaration of function '%s'", name);
            s = external_global_sym(t, &func_old_type, 0); 
        }

        r = s->r;
        /* A symbol that has a register is a local register variable,
           which starts out as VT_LOCAL value.  */
        if ((r & VT_VALMASK) < VT_CONST)
            r = (r & ~VT_VALMASK) | VT_LOCAL;

        vset(&s->type, r, s->c);
        /* Point to s as backpointer (even without r&VT_SYM).
	   Will be used by at least the x86 inline asm parser for
	   regvars.  */
	vtop->sym = s;

        if (r & VT_SYM) {
            vtop->c.i = 0;
        } else if (r == VT_CONST && IS_ENUM_VAL(s->type.t)) {
            vtop->c.i = s->enum_val;
        }
        break;
    }
    
    /* post operations */
    while (1) {
        if (tok == TOK_INC || tok == TOK_DEC) {
            inc(1, tok);
            next();
        } else if (tok == '.' || tok == TOK_ARROW || tok == TOK_CDOUBLE) {
            int qualifiers;
            /* field */ 
            if (tok == TOK_ARROW) 
                indir();
            qualifiers = vtop->type.t & (VT_CONSTANT | VT_VOLATILE);
            test_lvalue();
            gaddrof();
            /* expect pointer on structure */
            if ((vtop->type.t & VT_BTYPE) != VT_STRUCT)
                expect("struct or union");
            if (tok == TOK_CDOUBLE)
                expect("field name");
            next();
            if (tok == TOK_CINT || tok == TOK_CUINT)
                expect("field name");
	    s = find_field(&vtop->type, tok);
            if (!s)
                tcc_error("field not found: %s",  get_tok_str(tok & ~SYM_FIELD, &tokc));
            /* add field offset to pointer */
            vtop->type = char_pointer_type; /* change type to 'char *' */
            vpushi(s->c);
            gen_op('+');
            /* change type to field type, and set to lvalue */
            vtop->type = s->type;
            vtop->type.t |= qualifiers;
            /* an array is never an lvalue */
            if (!(vtop->type.t & VT_ARRAY)) {
                vtop->r |= lvalue_type(vtop->type.t);
#ifdef CONFIG_TCC_BCHECK
                /* if bound checking, the referenced pointer must be checked */
                if (tcc_state->do_bounds_check && (vtop->r & VT_VALMASK) != VT_LOCAL)
                    vtop->r |= VT_MUSTBOUND;
#endif
            }
            next();
        } else if (tok == '[') {
            next();
            gexpr();
            gen_op('+');
            indir();
            skip(']');
        } else if (tok == '(') {
            SValue ret;
            Sym *sa;
            int nb_args, ret_nregs, ret_align, regsize, variadic;

            /* function call  */
            if ((vtop->type.t & VT_BTYPE) != VT_FUNC) {
                /* pointer test (no array accepted) */
                if ((vtop->type.t & (VT_BTYPE | VT_ARRAY)) == VT_PTR) {
                    vtop->type = *pointed_type(&vtop->type);
                    if ((vtop->type.t & VT_BTYPE) != VT_FUNC)
                        goto error_func;
                } else {
                error_func:
                    expect("function pointer");
                }
            } else {
                vtop->r &= ~VT_LVAL; /* no lvalue */
            }
            /* get return type */
            s = vtop->type.ref;
            next();
            sa = s->next; /* first parameter */
            nb_args = regsize = 0;
            ret.r2 = VT_CONST;
            /* compute first implicit argument if a structure is returned */
            if ((s->type.t & VT_BTYPE) == VT_STRUCT) {
                variadic = (s->f.func_type == FUNC_ELLIPSIS);
                ret_nregs = gfunc_sret(&s->type, variadic, &ret.type,
                                       &ret_align, &regsize);
                if (!ret_nregs) {
                    /* get some space for the returned structure */
                    size = type_size(&s->type, &align);
#ifdef TCC_TARGET_ARM64
                /* On arm64, a small struct is return in registers.
                   It is much easier to write it to memory if we know
                   that we are allowed to write some extra bytes, so
                   round the allocated space up to a power of 2: */
                if (size < 16)
                    while (size & (size - 1))
                        size = (size | (size - 1)) + 1;
#endif
                    loc = (loc - size) & -align;
                    ret.type = s->type;
                    ret.r = VT_LOCAL | VT_LVAL;
                    /* pass it as 'int' to avoid structure arg passing
                       problems */
                    vseti(VT_LOCAL, loc);
                    ret.c = vtop->c;
                    nb_args++;
                }
            } else {
                ret_nregs = 1;
                ret.type = s->type;
            }

            if (ret_nregs) {
                /* return in register */
                if (is_float(ret.type.t)) {
                    ret.r = reg_fret(ret.type.t);
#ifdef TCC_TARGET_X86_64
                    if ((ret.type.t & VT_BTYPE) == VT_QFLOAT)
                      ret.r2 = REG_QRET;
#endif
                } else {
#ifndef TCC_TARGET_ARM64
#ifdef TCC_TARGET_X86_64
                    if ((ret.type.t & VT_BTYPE) == VT_QLONG)
#else
                    if ((ret.type.t & VT_BTYPE) == VT_LLONG)
#endif
                        ret.r2 = REG_LRET;
#endif
                    ret.r = REG_IRET;
                }
                ret.c.i = 0;
            }
            if (tok != ')') {
                for(;;) {
                    expr_eq();
                    gfunc_param_typed(s, sa);
                    nb_args++;
                    if (sa)
                        sa = sa->next;
                    if (tok == ')')
                        break;
                    skip(',');
                }
            }
            if (sa)
                tcc_error("too few arguments to function");
            skip(')');
            gfunc_call(nb_args);

            /* return value */
            for (r = ret.r + ret_nregs + !ret_nregs; r-- > ret.r;) {
                vsetc(&ret.type, r, &ret.c);
                vtop->r2 = ret.r2; /* Loop only happens when r2 is VT_CONST */
            }

            /* handle packed struct return */
            if (((s->type.t & VT_BTYPE) == VT_STRUCT) && ret_nregs) {
                int addr, offset;

                size = type_size(&s->type, &align);
		/* We're writing whole regs often, make sure there's enough
		   space.  Assume register size is power of 2.  */
		if (regsize > align)
		  align = regsize;
                loc = (loc - size) & -align;
                addr = loc;
                offset = 0;
                for (;;) {
                    vset(&ret.type, VT_LOCAL | VT_LVAL, addr + offset);
                    vswap();
                    vstore();
                    vtop--;
                    if (--ret_nregs == 0)
                        break;
                    offset += regsize;
                }
                vset(&s->type, VT_LOCAL | VT_LVAL, addr);
            }
        } else {
            break;
        }
    }
}

ST_FUNC void expr_prod(void)
{
    int t;

    unary();
    while (tok == '*' || tok == '/' || tok == '%') {
        t = tok;
        next();
        unary();
        gen_op(t);
    }
}

ST_FUNC void expr_sum(void)
{
    int t;

    expr_prod();
    while (tok == '+' || tok == '-') {
        t = tok;
        next();
        expr_prod();
        gen_op(t);
    }
}

static void expr_shift(void)
{
    int t;

    expr_sum();
    while (tok == TOK_SHL || tok == TOK_SAR) {
        t = tok;
        next();
        expr_sum();
        gen_op(t);
    }
}

static void expr_cmp(void)
{
    int t;

    expr_shift();
    while ((tok >= TOK_ULE && tok <= TOK_GT) ||
           tok == TOK_ULT || tok == TOK_UGE) {
        t = tok;
        next();
        expr_shift();
        gen_op(t);
    }
}

static void expr_cmpeq(void)
{
    int t;

    expr_cmp();
    while (tok == TOK_EQ || tok == TOK_NE) {
        t = tok;
        next();
        expr_cmp();
        gen_op(t);
    }
}

static void expr_and(void)
{
    expr_cmpeq();
    while (tok == '&') {
        next();
        expr_cmpeq();
        gen_op('&');
    }
}

static void expr_xor(void)
{
    expr_and();
    while (tok == '^') {
        next();
        expr_and();
        gen_op('^');
    }
}

static void expr_or(void)
{
    expr_xor();
    while (tok == '|') {
        next();
        expr_xor();
        gen_op('|');
    }
}

static void expr_land(void)
{
    expr_or();
    if (tok == TOK_LAND) {
	int t = 0;
	for(;;) {
	    if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                gen_cast_s(VT_BOOL);
		if (vtop->c.i) {
		    vpop();
		} else {
		    nocode_wanted++;
		    while (tok == TOK_LAND) {
			next();
			expr_or();
			vpop();
		    }
		    nocode_wanted--;
		    if (t)
		      gsym(t);
		    gen_cast_s(VT_INT);
		    break;
		}
	    } else {
		if (!t)
		  save_regs(1);
		t = gvtst(1, t);
	    }
	    if (tok != TOK_LAND) {
		if (t)
		  vseti(VT_JMPI, t);
		else
		  vpushi(1);
		break;
	    }
	    next();
	    expr_or();
	}
    }
}

static void expr_lor(void)
{
    expr_land();
    if (tok == TOK_LOR) {
	int t = 0;
	for(;;) {
	    if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                gen_cast_s(VT_BOOL);
		if (!vtop->c.i) {
		    vpop();
		} else {
		    nocode_wanted++;
		    while (tok == TOK_LOR) {
			next();
			expr_land();
			vpop();
		    }
		    nocode_wanted--;
		    if (t)
		      gsym(t);
		    gen_cast_s(VT_INT);
		    break;
		}
	    } else {
		if (!t)
		  save_regs(1);
		t = gvtst(0, t);
	    }
	    if (tok != TOK_LOR) {
		if (t)
		  vseti(VT_JMP, t);
		else
		  vpushi(0);
		break;
	    }
	    next();
	    expr_land();
	}
    }
}

/* Assuming vtop is a value used in a conditional context
   (i.e. compared with zero) return 0 if it's false, 1 if
   true and -1 if it can't be statically determined.  */
static int condition_3way(void)
{
    int c = -1;
    if ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST &&
	(!(vtop->r & VT_SYM) || !vtop->sym->a.weak)) {
	vdup();
        gen_cast_s(VT_BOOL);
	c = vtop->c.i;
	vpop();
    }
    return c;
}

static void expr_cond(void)
{
    int tt, u, r1, r2, rc, t1, t2, bt1, bt2, islv, c, g;
    SValue sv;
    CType type, type1, type2;

    expr_lor();
    if (tok == '?') {
        next();
	c = condition_3way();
        g = (tok == ':' && gnu_ext);
        if (c < 0) {
            /* needed to avoid having different registers saved in
               each branch */
            if (is_float(vtop->type.t)) {
                rc = RC_FLOAT;
#ifdef TCC_TARGET_X86_64
                if ((vtop->type.t & VT_BTYPE) == VT_LDOUBLE) {
                    rc = RC_ST0;
                }
#endif
            } else
                rc = RC_INT;
            gv(rc);
            save_regs(1);
            if (g)
                gv_dup();
            tt = gvtst(1, 0);

        } else {
            if (!g)
                vpop();
            tt = 0;
        }

        if (1) {
            if (c == 0)
                nocode_wanted++;
            if (!g)
                gexpr();

            type1 = vtop->type;
            sv = *vtop; /* save value to handle it later */
            vtop--; /* no vpop so that FP stack is not flushed */
            skip(':');

            u = 0;
            if (c < 0)
                u = gjmp(0);
            gsym(tt);

            if (c == 0)
                nocode_wanted--;
            if (c == 1)
                nocode_wanted++;
            expr_cond();
            if (c == 1)
                nocode_wanted--;

            type2 = vtop->type;
            t1 = type1.t;
            bt1 = t1 & VT_BTYPE;
            t2 = type2.t;
            bt2 = t2 & VT_BTYPE;
            type.ref = NULL;

            /* cast operands to correct type according to ISOC rules */
            if (is_float(bt1) || is_float(bt2)) {
                if (bt1 == VT_LDOUBLE || bt2 == VT_LDOUBLE) {
                    type.t = VT_LDOUBLE;

                } else if (bt1 == VT_DOUBLE || bt2 == VT_DOUBLE) {
                    type.t = VT_DOUBLE;
                } else {
                    type.t = VT_FLOAT;
                }
            } else if (bt1 == VT_LLONG || bt2 == VT_LLONG) {
                /* cast to biggest op */
                type.t = VT_LLONG | VT_LONG;
                if (bt1 == VT_LLONG)
                    type.t &= t1;
                if (bt2 == VT_LLONG)
                    type.t &= t2;
                /* convert to unsigned if it does not fit in a long long */
                if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED) ||
                    (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED))
                    type.t |= VT_UNSIGNED;
            } else if (bt1 == VT_PTR || bt2 == VT_PTR) {
		/* If one is a null ptr constant the result type
		   is the other.  */
		if (is_null_pointer (vtop))
		  type = type1;
		else if (is_null_pointer (&sv))
		  type = type2;
                /* XXX: test pointer compatibility, C99 has more elaborate
		   rules here.  */
		else
		  type = type1;
            } else if (bt1 == VT_FUNC || bt2 == VT_FUNC) {
                /* XXX: test function pointer compatibility */
                type = bt1 == VT_FUNC ? type1 : type2;
            } else if (bt1 == VT_STRUCT || bt2 == VT_STRUCT) {
                /* XXX: test structure compatibility */
                type = bt1 == VT_STRUCT ? type1 : type2;
            } else if (bt1 == VT_VOID || bt2 == VT_VOID) {
                /* NOTE: as an extension, we accept void on only one side */
                type.t = VT_VOID;
            } else {
                /* integer operations */
                type.t = VT_INT | (VT_LONG & (t1 | t2));
                /* convert to unsigned if it does not fit in an integer */
                if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED) ||
                    (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED))
                    type.t |= VT_UNSIGNED;
            }
            /* keep structs lvalue by transforming `(expr ? a : b)` to `*(expr ? &a : &b)` so
               that `(expr ? a : b).mem` does not error  with "lvalue expected" */
            islv = (vtop->r & VT_LVAL) && (sv.r & VT_LVAL) && VT_STRUCT == (type.t & VT_BTYPE);
            islv &= c < 0;

            /* now we convert second operand */
            if (c != 1) {
                gen_cast(&type);
                if (islv) {
                    mk_pointer(&vtop->type);
                    gaddrof();
                } else if (VT_STRUCT == (vtop->type.t & VT_BTYPE))
                    gaddrof();
            }

            rc = RC_INT;
            if (is_float(type.t)) {
                rc = RC_FLOAT;
#ifdef TCC_TARGET_X86_64
                if ((type.t & VT_BTYPE) == VT_LDOUBLE) {
                    rc = RC_ST0;
                }
#endif
            } else if ((type.t & VT_BTYPE) == VT_LLONG) {
                /* for long longs, we use fixed registers to avoid having
                   to handle a complicated move */
                rc = RC_IRET;
            }

            tt = r2 = 0;
            if (c < 0) {
                r2 = gv(rc);
                tt = gjmp(0);
            }
            gsym(u);

            /* this is horrible, but we must also convert first
               operand */
            if (c != 0) {
                *vtop = sv;
                gen_cast(&type);
                if (islv) {
                    mk_pointer(&vtop->type);
                    gaddrof();
                } else if (VT_STRUCT == (vtop->type.t & VT_BTYPE))
                    gaddrof();
            }

            if (c < 0) {
                r1 = gv(rc);
                move_reg(r2, r1, type.t);
                vtop->r = r2;
                gsym(tt);
                if (islv)
                    indir();
            }
        }
    }
}

static void expr_eq(void)
{
    int t;
    
    expr_cond();
    if (tok == '=' ||
        (tok >= TOK_A_MOD && tok <= TOK_A_DIV) ||
        tok == TOK_A_XOR || tok == TOK_A_OR ||
        tok == TOK_A_SHL || tok == TOK_A_SAR) {
        test_lvalue();
        t = tok;
        next();
        if (t == '=') {
            expr_eq();
        } else {
            vdup();
            expr_eq();
            gen_op(t & 0x7f);
        }
        vstore();
    }
}

ST_FUNC void gexpr(void)
{
    while (1) {
        expr_eq();
        if (tok != ',')
            break;
        vpop();
        next();
    }
}

/* parse a constant expression and return value in vtop.  */
static void expr_const1(void)
{
    const_wanted++;
    nocode_wanted++;
    expr_cond();
    nocode_wanted--;
    const_wanted--;
}

/* parse an integer constant and return its value. */
static inline int64_t expr_const64(void)
{
    int64_t c;
    expr_const1();
    if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        expect("constant expression");
    c = vtop->c.i;
    vpop();
    return c;
}

/* parse an integer constant and return its value.
   Complain if it doesn't fit 32bit (signed or unsigned).  */
ST_FUNC int expr_const(void)
{
    int c;
    int64_t wc = expr_const64();
    c = wc;
    if (c != wc && (unsigned)c != wc)
        tcc_error("constant exceeds 32 bit");
    return c;
}

/* return the label token if current token is a label, otherwise
   return zero */
static int is_label(void)
{
    int last_tok;

    /* fast test first */
    if (tok < TOK_UIDENT)
        return 0;
    /* no need to save tokc because tok is an identifier */
    last_tok = tok;
    next();
    if (tok == ':') {
        return last_tok;
    } else {
        unget_tok(last_tok);
        return 0;
    }
}

#ifndef TCC_TARGET_ARM64
static void gfunc_return(CType *func_type)
{
    if ((func_type->t & VT_BTYPE) == VT_STRUCT) {
        CType type, ret_type;
        int ret_align, ret_nregs, regsize;
        ret_nregs = gfunc_sret(func_type, func_var, &ret_type,
                               &ret_align, &regsize);
        if (0 == ret_nregs) {
            /* if returning structure, must copy it to implicit
               first pointer arg location */
            type = *func_type;
            mk_pointer(&type);
            vset(&type, VT_LOCAL | VT_LVAL, func_vc);
            indir();
            vswap();
            /* copy structure value to pointer */
            vstore();
        } else {
            /* returning structure packed into registers */
            int r, size, addr, align;
            size = type_size(func_type,&align);
            if ((vtop->r != (VT_LOCAL | VT_LVAL) ||
                 (vtop->c.i & (ret_align-1)))
                && (align & (ret_align-1))) {
                loc = (loc - size) & -ret_align;
                addr = loc;
                type = *func_type;
                vset(&type, VT_LOCAL | VT_LVAL, addr);
                vswap();
                vstore();
                vpop();
                vset(&ret_type, VT_LOCAL | VT_LVAL, addr);
            }
            vtop->type = ret_type;
            if (is_float(ret_type.t))
                r = rc_fret(ret_type.t);
            else
                r = RC_IRET;

            if (ret_nregs == 1)
                gv(r);
            else {
                for (;;) {
                    vdup();
                    gv(r);
                    vpop();
                    if (--ret_nregs == 0)
                      break;
                    /* We assume that when a structure is returned in multiple
                       registers, their classes are consecutive values of the
                       suite s(n) = 2^n */
                    r <<= 1;
                    vtop->c.i += regsize;
                }
            }
        }
    } else if (is_float(func_type->t)) {
        gv(rc_fret(func_type->t));
    } else {
        gv(RC_IRET);
    }
    vtop--; /* NOT vpop() because on x86 it would flush the fp stack */
}
#endif

static int case_cmp(const void *pa, const void *pb)
{
    int64_t a = (*(struct case_t**) pa)->v1;
    int64_t b = (*(struct case_t**) pb)->v1;
    return a < b ? -1 : a > b;
}

static void gcase(struct case_t **base, int len, int *bsym)
{
    struct case_t *p;
    int e;
    int ll = (vtop->type.t & VT_BTYPE) == VT_LLONG;
    gv(RC_INT);
    while (len > 4) {
        /* binary search */
        p = base[len/2];
        vdup();
	if (ll)
	    vpushll(p->v2);
	else
	    vpushi(p->v2);
        gen_op(TOK_LE);
        e = gtst(1, 0);
        vdup();
	if (ll)
	    vpushll(p->v1);
	else
	    vpushi(p->v1);
        gen_op(TOK_GE);
        gtst_addr(0, p->sym); /* v1 <= x <= v2 */
        /* x < v1 */
        gcase(base, len/2, bsym);
        if (cur_switch->def_sym)
            gjmp_addr(cur_switch->def_sym);
        else
            *bsym = gjmp(*bsym);
        /* x > v2 */
        gsym(e);
        e = len/2 + 1;
        base += e; len -= e;
    }
    /* linear scan */
    while (len--) {
        p = *base++;
        vdup();
	if (ll)
	    vpushll(p->v2);
	else
	    vpushi(p->v2);
        if (p->v1 == p->v2) {
            gen_op(TOK_EQ);
            gtst_addr(0, p->sym);
        } else {
            gen_op(TOK_LE);
            e = gtst(1, 0);
            vdup();
	    if (ll)
	        vpushll(p->v1);
	    else
	        vpushi(p->v1);
            gen_op(TOK_GE);
            gtst_addr(0, p->sym);
            gsym(e);
        }
    }
}

static void block(int *bsym, int *csym, int is_expr)
{
    int a, b, c, d, cond;
    Sym *s;

    /* generate line number info */
    if (tcc_state->do_debug)
        tcc_debug_line(tcc_state);

    if (is_expr) {
        /* default return value is (void) */
        vpushi(0);
        vtop->type.t = VT_VOID;
    }

    if (tok == TOK_IF) {
        /* if test */
	int saved_nocode_wanted = nocode_wanted;
        next();
        skip('(');
        gexpr();
        skip(')');
	cond = condition_3way();
        if (cond == 1)
            a = 0, vpop();
        else
            a = gvtst(1, 0);
        if (cond == 0)
	    nocode_wanted |= 0x20000000;
        block(bsym, csym, 0);
	if (cond != 1)
	    nocode_wanted = saved_nocode_wanted;
        c = tok;
        if (c == TOK_ELSE) {
            next();
            d = gjmp(0);
            gsym(a);
	    if (cond == 1)
	        nocode_wanted |= 0x20000000;
            block(bsym, csym, 0);
            gsym(d); /* patch else jmp */
	    if (cond != 0)
		nocode_wanted = saved_nocode_wanted;
        } else
            gsym(a);
    } else if (tok == TOK_WHILE) {
	int saved_nocode_wanted;
	nocode_wanted &= ~0x20000000;
        next();
        d = ind;
        vla_sp_restore();
        skip('(');
        gexpr();
        skip(')');
        a = gvtst(1, 0);
        b = 0;
        ++local_scope;
	saved_nocode_wanted = nocode_wanted;
        block(&a, &b, 0);
	nocode_wanted = saved_nocode_wanted;
        --local_scope;
        gjmp_addr(d);
        gsym(a);
        gsym_addr(b, d);
    } else if (tok == '{') {
        Sym *llabel;
        int block_vla_sp_loc = vla_sp_loc, saved_vlas_in_scope = vlas_in_scope;

        next();
        /* record local declaration stack position */
        s = local_stack;
        llabel = local_label_stack;
        ++local_scope;
        
        /* handle local labels declarations */
        if (tok == TOK_LABEL) {
            next();
            for(;;) {
                if (tok < TOK_UIDENT)
                    expect("label identifier");
                label_push(&local_label_stack, tok, LABEL_DECLARED);
                next();
                if (tok == ',') {
                    next();
                } else {
                    skip(';');
                    break;
                }
            }
        }
        while (tok != '}') {
	    if ((a = is_label()))
		unget_tok(a);
	    else
	        decl(VT_LOCAL);
            if (tok != '}') {
                if (is_expr)
                    vpop();
                block(bsym, csym, is_expr);
            }
        }
        /* pop locally defined labels */
        label_pop(&local_label_stack, llabel, is_expr);
        /* pop locally defined symbols */
        --local_scope;
	/* In the is_expr case (a statement expression is finished here),
	   vtop might refer to symbols on the local_stack.  Either via the
	   type or via vtop->sym.  We can't pop those nor any that in turn
	   might be referred to.  To make it easier we don't roll back
	   any symbols in that case; some upper level call to block() will
	   do that.  We do have to remove such symbols from the lookup
	   tables, though.  sym_pop will do that.  */
	sym_pop(&local_stack, s, is_expr);

        /* Pop VLA frames and restore stack pointer if required */
        if (vlas_in_scope > saved_vlas_in_scope) {
            vla_sp_loc = saved_vlas_in_scope ? block_vla_sp_loc : vla_sp_root_loc;
            vla_sp_restore();
        }
        vlas_in_scope = saved_vlas_in_scope;
        
        next();
    } else if (tok == TOK_RETURN) {
        next();
        if (tok != ';') {
            gexpr();
            gen_assign_cast(&func_vt);
            if ((func_vt.t & VT_BTYPE) == VT_VOID)
                vtop--;
            else
                gfunc_return(&func_vt);
        }
        skip(';');
        /* jump unless last stmt in top-level block */
        if (tok != '}' || local_scope != 1)
            rsym = gjmp(rsym);
	nocode_wanted |= 0x20000000;
    } else if (tok == TOK_BREAK) {
        /* compute jump */
        if (!bsym)
            tcc_error("cannot break");
        *bsym = gjmp(*bsym);
        next();
        skip(';');
	nocode_wanted |= 0x20000000;
    } else if (tok == TOK_CONTINUE) {
        /* compute jump */
        if (!csym)
            tcc_error("cannot continue");
        vla_sp_restore_root();
        *csym = gjmp(*csym);
        next();
        skip(';');
    } else if (tok == TOK_FOR) {
        int e;
	int saved_nocode_wanted;
	nocode_wanted &= ~0x20000000;
        next();
        skip('(');
        s = local_stack;
        ++local_scope;
        if (tok != ';') {
            /* c99 for-loop init decl? */
            if (!decl0(VT_LOCAL, 1, NULL)) {
                /* no, regular for-loop init expr */
                gexpr();
                vpop();
            }
        }
        skip(';');
        d = ind;
        c = ind;
        vla_sp_restore();
        a = 0;
        b = 0;
        if (tok != ';') {
            gexpr();
            a = gvtst(1, 0);
        }
        skip(';');
        if (tok != ')') {
            e = gjmp(0);
            c = ind;
            vla_sp_restore();
            gexpr();
            vpop();
            gjmp_addr(d);
            gsym(e);
        }
        skip(')');
	saved_nocode_wanted = nocode_wanted;
        block(&a, &b, 0);
	nocode_wanted = saved_nocode_wanted;
        gjmp_addr(c);
        gsym(a);
        gsym_addr(b, c);
        --local_scope;
        sym_pop(&local_stack, s, 0);

    } else 
    if (tok == TOK_DO) {
	int saved_nocode_wanted;
	nocode_wanted &= ~0x20000000;
        next();
        a = 0;
        b = 0;
        d = ind;
        vla_sp_restore();
	saved_nocode_wanted = nocode_wanted;
        block(&a, &b, 0);
        skip(TOK_WHILE);
        skip('(');
        gsym(b);
	gexpr();
	c = gvtst(0, 0);
	gsym_addr(c, d);
	nocode_wanted = saved_nocode_wanted;
        skip(')');
        gsym(a);
        skip(';');
    } else
    if (tok == TOK_SWITCH) {
        struct switch_t *saved, sw;
	int saved_nocode_wanted = nocode_wanted;
	SValue switchval;
        next();
        skip('(');
        gexpr();
        skip(')');
	switchval = *vtop--;
        a = 0;
        b = gjmp(0); /* jump to first case */
        sw.p = NULL; sw.n = 0; sw.def_sym = 0;
        saved = cur_switch;
        cur_switch = &sw;
        block(&a, csym, 0);
	nocode_wanted = saved_nocode_wanted;
        a = gjmp(a); /* add implicit break */
        /* case lookup */
        gsym(b);
        qsort(sw.p, sw.n, sizeof(void*), case_cmp);
        for (b = 1; b < sw.n; b++)
            if (sw.p[b - 1]->v2 >= sw.p[b]->v1)
                tcc_error("duplicate case value");
        /* Our switch table sorting is signed, so the compared
           value needs to be as well when it's 64bit.  */
        if ((switchval.type.t & VT_BTYPE) == VT_LLONG)
            switchval.type.t &= ~VT_UNSIGNED;
        vpushv(&switchval);
        gcase(sw.p, sw.n, &a);
        vpop();
        if (sw.def_sym)
          gjmp_addr(sw.def_sym);
        dynarray_reset(&sw.p, &sw.n);
        cur_switch = saved;
        /* break label */
        gsym(a);
    } else
    if (tok == TOK_CASE) {
        struct case_t *cr = tcc_malloc(sizeof(struct case_t));
        if (!cur_switch)
            expect("switch");
	nocode_wanted &= ~0x20000000;
        next();
        cr->v1 = cr->v2 = expr_const64();
        if (gnu_ext && tok == TOK_DOTS) {
            next();
            cr->v2 = expr_const64();
            if (cr->v2 < cr->v1)
                tcc_warning("empty case range");
        }
        cr->sym = ind;
        dynarray_add(&cur_switch->p, &cur_switch->n, cr);
        skip(':');
        is_expr = 0;
        goto block_after_label;
    } else 
    if (tok == TOK_DEFAULT) {
        next();
        skip(':');
        if (!cur_switch)
            expect("switch");
        if (cur_switch->def_sym)
            tcc_error("too many 'default'");
        cur_switch->def_sym = ind;
        is_expr = 0;
        goto block_after_label;
    } else
    if (tok == TOK_GOTO) {
        next();
        if (tok == '*' && gnu_ext) {
            /* computed goto */
            next();
            gexpr();
            if ((vtop->type.t & VT_BTYPE) != VT_PTR)
                expect("pointer");
            ggoto();
        } else if (tok >= TOK_UIDENT) {
            s = label_find(tok);
            /* put forward definition if needed */
            if (!s) {
                s = label_push(&global_label_stack, tok, LABEL_FORWARD);
            } else {
                if (s->r == LABEL_DECLARED)
                    s->r = LABEL_FORWARD;
            }
            vla_sp_restore_root();
	    if (s->r & LABEL_FORWARD)
                s->jnext = gjmp(s->jnext);
            else
                gjmp_addr(s->jnext);
            next();
        } else {
            expect("label identifier");
        }
        skip(';');
    } else if (tok == TOK_ASM1 || tok == TOK_ASM2 || tok == TOK_ASM3) {
        asm_instr();
    } else {
        b = is_label();
        if (b) {
            /* label case */
	    next();
            s = label_find(b);
            if (s) {
                if (s->r == LABEL_DEFINED)
                    tcc_error("duplicate label '%s'", get_tok_str(s->v, NULL));
                gsym(s->jnext);
                s->r = LABEL_DEFINED;
            } else {
                s = label_push(&global_label_stack, b, LABEL_DEFINED);
            }
            s->jnext = ind;
            vla_sp_restore();
            /* we accept this, but it is a mistake */
        block_after_label:
	    nocode_wanted &= ~0x20000000;
            if (tok == '}') {
                tcc_warning("deprecated use of label at end of compound statement");
            } else {
                if (is_expr)
                    vpop();
                block(bsym, csym, is_expr);
            }
        } else {
            /* expression case */
            if (tok != ';') {
                if (is_expr) {
                    vpop();
                    gexpr();
                } else {
                    gexpr();
                    vpop();
                }
            }
            skip(';');
        }
    }
}

/* This skips over a stream of tokens containing balanced {} and ()
   pairs, stopping at outer ',' ';' and '}' (or matching '}' if we started
   with a '{').  If STR then allocates and stores the skipped tokens
   in *STR.  This doesn't check if () and {} are nested correctly,
   i.e. "({)}" is accepted.  */
static void skip_or_save_block(TokenString **str)
{
    int braces = tok == '{';
    int level = 0;
    if (str)
      *str = tok_str_alloc();

    while ((level > 0 || (tok != '}' && tok != ',' && tok != ';' && tok != ')'))) {
	int t;
	if (tok == TOK_EOF) {
	     if (str || level > 0)
	       tcc_error("unexpected end of file");
	     else
	       break;
	}
	if (str)
	  tok_str_add_tok(*str);
	t = tok;
	next();
	if (t == '{' || t == '(') {
	    level++;
	} else if (t == '}' || t == ')') {
	    level--;
	    if (level == 0 && braces && t == '}')
	      break;
	}
    }
    if (str) {
	tok_str_add(*str, -1);
	tok_str_add(*str, 0);
    }
}

#define EXPR_CONST 1
#define EXPR_ANY   2

static void parse_init_elem(int expr_type)
{
    int saved_global_expr;
    switch(expr_type) {
    case EXPR_CONST:
        /* compound literals must be allocated globally in this case */
        saved_global_expr = global_expr;
        global_expr = 1;
        expr_const1();
        global_expr = saved_global_expr;
        /* NOTE: symbols are accepted, as well as lvalue for anon symbols
	   (compound literals).  */
        if (((vtop->r & (VT_VALMASK | VT_LVAL)) != VT_CONST
	     && ((vtop->r & (VT_SYM|VT_LVAL)) != (VT_SYM|VT_LVAL)
		 || vtop->sym->v < SYM_FIRST_ANOM))
#ifdef TCC_TARGET_PE
                 || ((vtop->r & VT_SYM) && vtop->sym->a.dllimport)
#endif
            )
            tcc_error("initializer element is not constant");
        break;
    case EXPR_ANY:
        expr_eq();
        break;
    }
}

/* put zeros for variable based init */
static void init_putz(Section *sec, unsigned long c, int size)
{
    if (sec) {
        /* nothing to do because globals are already set to zero */
    } else {
        vpush_global_sym(&func_old_type, TOK_memset);
        vseti(VT_LOCAL, c);
#ifdef TCC_TARGET_ARM
        vpushs(size);
        vpushi(0);
#else
        vpushi(0);
        vpushs(size);
#endif
        gfunc_call(3);
    }
}

/* t is the array or struct type. c is the array or struct
   address. cur_field is the pointer to the current
   field, for arrays the 'c' member contains the current start
   index.  'size_only' is true if only size info is needed (only used
   in arrays).  al contains the already initialized length of the
   current container (starting at c).  This returns the new length of that.  */
static int decl_designator(CType *type, Section *sec, unsigned long c,
                           Sym **cur_field, int size_only, int al)
{
    Sym *s, *f;
    int index, index_last, align, l, nb_elems, elem_size;
    unsigned long corig = c;

    elem_size = 0;
    nb_elems = 1;
    if (gnu_ext && (l = is_label()) != 0)
        goto struct_field;
    /* NOTE: we only support ranges for last designator */
    while (nb_elems == 1 && (tok == '[' || tok == '.')) {
        if (tok == '[') {
            if (!(type->t & VT_ARRAY))
                expect("array type");
            next();
            index = index_last = expr_const();
            if (tok == TOK_DOTS && gnu_ext) {
                next();
                index_last = expr_const();
            }
            skip(']');
            s = type->ref;
	    if (index < 0 || (s->c >= 0 && index_last >= s->c) ||
		index_last < index)
	        tcc_error("invalid index");
            if (cur_field)
		(*cur_field)->c = index_last;
            type = pointed_type(type);
            elem_size = type_size(type, &align);
            c += index * elem_size;
            nb_elems = index_last - index + 1;
        } else {
            next();
            l = tok;
        struct_field:
            next();
            if ((type->t & VT_BTYPE) != VT_STRUCT)
                expect("struct/union type");
	    f = find_field(type, l);
            if (!f)
                expect("field");
            if (cur_field)
                *cur_field = f;
	    type = &f->type;
            c += f->c;
        }
        cur_field = NULL;
    }
    if (!cur_field) {
        if (tok == '=') {
            next();
        } else if (!gnu_ext) {
	    expect("=");
        }
    } else {
        if (type->t & VT_ARRAY) {
	    index = (*cur_field)->c;
	    if (type->ref->c >= 0 && index >= type->ref->c)
	        tcc_error("index too large");
            type = pointed_type(type);
            c += index * type_size(type, &align);
        } else {
            f = *cur_field;
	    while (f && (f->v & SYM_FIRST_ANOM) && (f->type.t & VT_BITFIELD))
	        *cur_field = f = f->next;
            if (!f)
                tcc_error("too many field init");
	    type = &f->type;
            c += f->c;
        }
    }
    /* must put zero in holes (note that doing it that way
       ensures that it even works with designators) */
    if (!size_only && c - corig > al)
	init_putz(sec, corig + al, c - corig - al);
    decl_initializer(type, sec, c, 0, size_only);

    /* XXX: make it more general */
    if (!size_only && nb_elems > 1) {
        unsigned long c_end;
        uint8_t *src, *dst;
        int i;

        if (!sec) {
	    vset(type, VT_LOCAL|VT_LVAL, c);
	    for (i = 1; i < nb_elems; i++) {
		vset(type, VT_LOCAL|VT_LVAL, c + elem_size * i);
		vswap();
		vstore();
	    }
	    vpop();
        } else if (!NODATA_WANTED) {
	    c_end = c + nb_elems * elem_size;
	    if (c_end > sec->data_allocated)
	        section_realloc(sec, c_end);
	    src = sec->data + c;
	    dst = src;
	    for(i = 1; i < nb_elems; i++) {
		dst += elem_size;
		memcpy(dst, src, elem_size);
	    }
	}
    }
    c += nb_elems * type_size(type, &align);
    if (c - corig > al)
      al = c - corig;
    return al;
}

/* store a value or an expression directly in global data or in local array */
static void init_putv(CType *type, Section *sec, unsigned long c)
{
    int bt;
    void *ptr;
    CType dtype;

    dtype = *type;
    dtype.t &= ~VT_CONSTANT; /* need to do that to avoid false warning */

    if (sec) {
	int size, align;
        /* XXX: not portable */
        /* XXX: generate error if incorrect relocation */
        gen_assign_cast(&dtype);
        bt = type->t & VT_BTYPE;

        if ((vtop->r & VT_SYM)
            && bt != VT_PTR
            && bt != VT_FUNC
            && (bt != (PTR_SIZE == 8 ? VT_LLONG : VT_INT)
                || (type->t & VT_BITFIELD))
            && !((vtop->r & VT_CONST) && vtop->sym->v >= SYM_FIRST_ANOM)
            )
            tcc_error("initializer element is not computable at load time");

        if (NODATA_WANTED) {
            vtop--;
            return;
        }

	size = type_size(type, &align);
	section_reserve(sec, c + size);
        ptr = sec->data + c;

        /* XXX: make code faster ? */
	if ((vtop->r & (VT_SYM|VT_CONST)) == (VT_SYM|VT_CONST) &&
	    vtop->sym->v >= SYM_FIRST_ANOM &&
	    /* XXX This rejects compound literals like
	       '(void *){ptr}'.  The problem is that '&sym' is
	       represented the same way, which would be ruled out
	       by the SYM_FIRST_ANOM check above, but also '"string"'
	       in 'char *p = "string"' is represented the same
	       with the type being VT_PTR and the symbol being an
	       anonymous one.  That is, there's no difference in vtop
	       between '(void *){x}' and '&(void *){x}'.  Ignore
	       pointer typed entities here.  Hopefully no real code
	       will every use compound literals with scalar type.  */
	    (vtop->type.t & VT_BTYPE) != VT_PTR) {
	    /* These come from compound literals, memcpy stuff over.  */
	    Section *ssec;
	    ElfSym *esym;
	    ElfW_Rel *rel;
	    esym = elfsym(vtop->sym);
	    ssec = tcc_state->sections[esym->st_shndx];
	    memmove (ptr, ssec->data + esym->st_value, size);
	    if (ssec->reloc) {
		/* We need to copy over all memory contents, and that
		   includes relocations.  Use the fact that relocs are
		   created it order, so look from the end of relocs
		   until we hit one before the copied region.  */
		int num_relocs = ssec->reloc->data_offset / sizeof(*rel);
		rel = (ElfW_Rel*)(ssec->reloc->data + ssec->reloc->data_offset);
		while (num_relocs--) {
		    rel--;
		    if (rel->r_offset >= esym->st_value + size)
		      continue;
		    if (rel->r_offset < esym->st_value)
		      break;
		    /* Note: if the same fields are initialized multiple
		       times (possible with designators) then we possibly
		       add multiple relocations for the same offset here.
		       That would lead to wrong code, the last reloc needs
		       to win.  We clean this up later after the whole
		       initializer is parsed.  */
		    put_elf_reloca(symtab_section, sec,
				   c + rel->r_offset - esym->st_value,
				   ELFW(R_TYPE)(rel->r_info),
				   ELFW(R_SYM)(rel->r_info),
#if PTR_SIZE == 8
				   rel->r_addend
#else
				   0
#endif
				  );
		}
	    }
	} else {
            if (type->t & VT_BITFIELD) {
                int bit_pos, bit_size, bits, n;
                unsigned char *p, v, m;
                bit_pos = BIT_POS(vtop->type.t);
                bit_size = BIT_SIZE(vtop->type.t);
                p = (unsigned char*)ptr + (bit_pos >> 3);
                bit_pos &= 7, bits = 0;
                while (bit_size) {
                    n = 8 - bit_pos;
                    if (n > bit_size)
                        n = bit_size;
                    v = vtop->c.i >> bits << bit_pos;
                    m = ((1 << n) - 1) << bit_pos;
                    *p = (*p & ~m) | (v & m);
                    bits += n, bit_size -= n, bit_pos = 0, ++p;
                }
            } else
            switch(bt) {
		/* XXX: when cross-compiling we assume that each type has the
		   same representation on host and target, which is likely to
		   be wrong in the case of long double */
	    case VT_BOOL:
		vtop->c.i = vtop->c.i != 0;
	    case VT_BYTE:
		*(char *)ptr |= vtop->c.i;
		break;
	    case VT_SHORT:
		*(short *)ptr |= vtop->c.i;
		break;
	    case VT_FLOAT:
		*(float*)ptr = vtop->c.f;
		break;
	    case VT_DOUBLE:
		*(double *)ptr = vtop->c.d;
		break;
	    case VT_LDOUBLE:
#if defined TCC_IS_NATIVE_387
                if (sizeof (long double) >= 10) /* zero pad ten-byte LD */
                    memcpy(ptr, &vtop->c.ld, 10);
#ifdef __TINYC__
                else if (sizeof (long double) == sizeof (double))
                    __asm__("fldl %1\nfstpt %0\n" : "=m" (*ptr) : "m" (vtop->c.ld));
#endif
                else if (vtop->c.ld == 0.0)
                    ;
                else
#endif
                if (sizeof(long double) == LDOUBLE_SIZE)
		    *(long double*)ptr = vtop->c.ld;
                else if (sizeof(double) == LDOUBLE_SIZE)
		    *(double *)ptr = (double)vtop->c.ld;
                else
                    tcc_error("can't cross compile long double constants");
		break;
#if PTR_SIZE != 8
	    case VT_LLONG:
		*(long long *)ptr |= vtop->c.i;
		break;
#else
	    case VT_LLONG:
#endif
	    case VT_PTR:
		{
		    addr_t val = vtop->c.i;
#if PTR_SIZE == 8
		    if (vtop->r & VT_SYM)
		      greloca(sec, vtop->sym, c, R_DATA_PTR, val);
		    else
		      *(addr_t *)ptr |= val;
#else
		    if (vtop->r & VT_SYM)
		      greloc(sec, vtop->sym, c, R_DATA_PTR);
		    *(addr_t *)ptr |= val;
#endif
		    break;
		}
	    default:
		{
		    int val = vtop->c.i;
#if PTR_SIZE == 8
		    if (vtop->r & VT_SYM)
		      greloca(sec, vtop->sym, c, R_DATA_PTR, val);
		    else
		      *(int *)ptr |= val;
#else
		    if (vtop->r & VT_SYM)
		      greloc(sec, vtop->sym, c, R_DATA_PTR);
		    *(int *)ptr |= val;
#endif
		    break;
		}
	    }
	}
        vtop--;
    } else {
        vset(&dtype, VT_LOCAL|VT_LVAL, c);
        vswap();
        vstore();
        vpop();
    }
}

/* 't' contains the type and storage info. 'c' is the offset of the
   object in section 'sec'. If 'sec' is NULL, it means stack based
   allocation. 'first' is true if array '{' must be read (multi
   dimension implicit array init handling). 'size_only' is true if
   size only evaluation is wanted (only for arrays). */
static void decl_initializer(CType *type, Section *sec, unsigned long c, 
                             int first, int size_only)
{
    int len, n, no_oblock, nb, i;
    int size1, align1;
    int have_elem;
    Sym *s, *f;
    Sym indexsym;
    CType *t1;

    /* If we currently are at an '}' or ',' we have read an initializer
       element in one of our callers, and not yet consumed it.  */
    have_elem = tok == '}' || tok == ',';
    if (!have_elem && tok != '{' &&
	/* In case of strings we have special handling for arrays, so
	   don't consume them as initializer value (which would commit them
	   to some anonymous symbol).  */
	tok != TOK_LSTR && tok != TOK_STR &&
	!size_only) {
	parse_init_elem(!sec ? EXPR_ANY : EXPR_CONST);
	have_elem = 1;
    }

    if (have_elem &&
	!(type->t & VT_ARRAY) &&
	/* Use i_c_parameter_t, to strip toplevel qualifiers.
	   The source type might have VT_CONSTANT set, which is
	   of course assignable to non-const elements.  */
	is_compatible_unqualified_types(type, &vtop->type)) {
        init_putv(type, sec, c);
    } else if (type->t & VT_ARRAY) {
        s = type->ref;
        n = s->c;
        t1 = pointed_type(type);
        size1 = type_size(t1, &align1);

        no_oblock = 1;
        if ((first && tok != TOK_LSTR && tok != TOK_STR) || 
            tok == '{') {
            if (tok != '{')
                tcc_error("character array initializer must be a literal,"
                    " optionally enclosed in braces");
            skip('{');
            no_oblock = 0;
        }

        /* only parse strings here if correct type (otherwise: handle
           them as ((w)char *) expressions */
        if ((tok == TOK_LSTR && 
#ifdef TCC_TARGET_PE
             (t1->t & VT_BTYPE) == VT_SHORT && (t1->t & VT_UNSIGNED)
#else
             (t1->t & VT_BTYPE) == VT_INT
#endif
            ) || (tok == TOK_STR && (t1->t & VT_BTYPE) == VT_BYTE)) {
	    len = 0;
            while (tok == TOK_STR || tok == TOK_LSTR) {
                int cstr_len, ch;

                /* compute maximum number of chars wanted */
                if (tok == TOK_STR)
                    cstr_len = tokc.str.size;
                else
                    cstr_len = tokc.str.size / sizeof(nwchar_t);
                cstr_len--;
                nb = cstr_len;
                if (n >= 0 && nb > (n - len))
                    nb = n - len;
                if (!size_only) {
                    if (cstr_len > nb)
                        tcc_warning("initializer-string for array is too long");
                    /* in order to go faster for common case (char
                       string in global variable, we handle it
                       specifically */
                    if (sec && tok == TOK_STR && size1 == 1) {
                        if (!NODATA_WANTED)
                            memcpy(sec->data + c + len, tokc.str.data, nb);
                    } else {
                        for(i=0;i<nb;i++) {
                            if (tok == TOK_STR)
                                ch = ((unsigned char *)tokc.str.data)[i];
                            else
                                ch = ((nwchar_t *)tokc.str.data)[i];
			    vpushi(ch);
                            init_putv(t1, sec, c + (len + i) * size1);
                        }
                    }
                }
                len += nb;
                next();
            }
            /* only add trailing zero if enough storage (no
               warning in this case since it is standard) */
            if (n < 0 || len < n) {
                if (!size_only) {
		    vpushi(0);
                    init_putv(t1, sec, c + (len * size1));
                }
                len++;
            }
	    len *= size1;
        } else {
	    indexsym.c = 0;
	    f = &indexsym;

          do_init_list:
	    len = 0;
	    while (tok != '}' || have_elem) {
		len = decl_designator(type, sec, c, &f, size_only, len);
		have_elem = 0;
		if (type->t & VT_ARRAY) {
		    ++indexsym.c;
		    /* special test for multi dimensional arrays (may not
		       be strictly correct if designators are used at the
		       same time) */
		    if (no_oblock && len >= n*size1)
		        break;
		} else {
		    if (s->type.t == VT_UNION)
		        f = NULL;
		    else
		        f = f->next;
		    if (no_oblock && f == NULL)
		        break;
		}

		if (tok == '}')
		    break;
		skip(',');
	    }
        }
        /* put zeros at the end */
	if (!size_only && len < n*size1)
	    init_putz(sec, c + len, n*size1 - len);
        if (!no_oblock)
            skip('}');
        /* patch type size if needed, which happens only for array types */
        if (n < 0)
            s->c = size1 == 1 ? len : ((len + size1 - 1)/size1);
    } else if ((type->t & VT_BTYPE) == VT_STRUCT) {
	size1 = 1;
        no_oblock = 1;
        if (first || tok == '{') {
            skip('{');
            no_oblock = 0;
        }
        s = type->ref;
        f = s->next;
        n = s->c;
	goto do_init_list;
    } else if (tok == '{') {
        next();
        decl_initializer(type, sec, c, first, size_only);
        skip('}');
    } else if (size_only) {
	/* If we supported only ISO C we wouldn't have to accept calling
	   this on anything than an array size_only==1 (and even then
	   only on the outermost level, so no recursion would be needed),
	   because initializing a flex array member isn't supported.
	   But GNU C supports it, so we need to recurse even into
	   subfields of structs and arrays when size_only is set.  */
        /* just skip expression */
        skip_or_save_block(NULL);
    } else {
	if (!have_elem) {
	    /* This should happen only when we haven't parsed
	       the init element above for fear of committing a
	       string constant to memory too early.  */
	    if (tok != TOK_STR && tok != TOK_LSTR)
	      expect("string constant");
	    parse_init_elem(!sec ? EXPR_ANY : EXPR_CONST);
	}
        init_putv(type, sec, c);
    }
}

/* parse an initializer for type 't' if 'has_init' is non zero, and
   allocate space in local or global data space ('r' is either
   VT_LOCAL or VT_CONST). If 'v' is non zero, then an associated
   variable 'v' of scope 'scope' is declared before initializers
   are parsed. If 'v' is zero, then a reference to the new object
   is put in the value stack. If 'has_init' is 2, a special parsing
   is done to handle string constants. */
static void decl_initializer_alloc(CType *type, AttributeDef *ad, int r, 
                                   int has_init, int v, int scope)
{
    int size, align, addr;
    TokenString *init_str = NULL;

    Section *sec;
    Sym *flexible_array;
    Sym *sym = NULL;
    int saved_nocode_wanted = nocode_wanted;
#ifdef CONFIG_TCC_BCHECK
    int bcheck = tcc_state->do_bounds_check && !NODATA_WANTED;
#endif

    if (type->t & VT_STATIC)
        nocode_wanted |= NODATA_WANTED ? 0x40000000 : 0x80000000;

    flexible_array = NULL;
    if ((type->t & VT_BTYPE) == VT_STRUCT) {
        Sym *field = type->ref->next;
        if (field) {
            while (field->next)
                field = field->next;
            if (field->type.t & VT_ARRAY && field->type.ref->c < 0)
                flexible_array = field;
        }
    }

    size = type_size(type, &align);
    /* If unknown size, we must evaluate it before
       evaluating initializers because
       initializers can generate global data too
       (e.g. string pointers or ISOC99 compound
       literals). It also simplifies local
       initializers handling */
    if (size < 0 || (flexible_array && has_init)) {
        if (!has_init) 
            tcc_error("unknown type size");
        /* get all init string */
        if (has_init == 2) {
	    init_str = tok_str_alloc();
            /* only get strings */
            while (tok == TOK_STR || tok == TOK_LSTR) {
                tok_str_add_tok(init_str);
                next();
            }
	    tok_str_add(init_str, -1);
	    tok_str_add(init_str, 0);
        } else {
	    skip_or_save_block(&init_str);
        }
        unget_tok(0);

        /* compute size */
        begin_macro(init_str, 1);
        next();
        decl_initializer(type, NULL, 0, 1, 1);
        /* prepare second initializer parsing */
        macro_ptr = init_str->str;
        next();
        
        /* if still unknown size, error */
        size = type_size(type, &align);
        if (size < 0) 
            tcc_error("unknown type size");
    }
    /* If there's a flex member and it was used in the initializer
       adjust size.  */
    if (flexible_array &&
	flexible_array->type.ref->c > 0)
        size += flexible_array->type.ref->c
	        * pointed_size(&flexible_array->type);
    /* take into account specified alignment if bigger */
    if (ad->a.aligned) {
	int speca = 1 << (ad->a.aligned - 1);
        if (speca > align)
            align = speca;
    } else if (ad->a.packed) {
        align = 1;
    }

    if (NODATA_WANTED)
        size = 0, align = 1;

    if ((r & VT_VALMASK) == VT_LOCAL) {
        sec = NULL;
#ifdef CONFIG_TCC_BCHECK
        if (bcheck && (type->t & VT_ARRAY)) {
            loc--;
        }
#endif
        loc = (loc - size) & -align;
        addr = loc;
#ifdef CONFIG_TCC_BCHECK
        /* handles bounds */
        /* XXX: currently, since we do only one pass, we cannot track
           '&' operators, so we add only arrays */
        if (bcheck && (type->t & VT_ARRAY)) {
            addr_t *bounds_ptr;
            /* add padding between regions */
            loc--;
            /* then add local bound info */
            bounds_ptr = section_ptr_add(lbounds_section, 2 * sizeof(addr_t));
            bounds_ptr[0] = addr;
            bounds_ptr[1] = size;
        }
#endif
        if (v) {
            /* local variable */
#ifdef CONFIG_TCC_ASM
	    if (ad->asm_label) {
		int reg = asm_parse_regvar(ad->asm_label);
		if (reg >= 0)
		    r = (r & ~VT_VALMASK) | reg;
	    }
#endif
            sym = sym_push(v, type, r, addr);
            sym->a = ad->a;
        } else {
            /* push local reference */
            vset(type, r, addr);
        }
    } else {
        if (v && scope == VT_CONST) {
            /* see if the symbol was already defined */
            sym = sym_find(v);
            if (sym) {
                patch_storage(sym, ad, type);
                /* we accept several definitions of the same global variable. */
                if (!has_init && sym->c && elfsym(sym)->st_shndx != SHN_UNDEF)
                    goto no_alloc;
            }
        }

        /* allocate symbol in corresponding section */
        sec = ad->section;
        if (!sec) {
            if (has_init)
                sec = data_section;
            else if (tcc_state->nocommon)
                sec = bss_section;
        }

        if (sec) {
	    addr = section_add(sec, size, align);
#ifdef CONFIG_TCC_BCHECK
            /* add padding if bound check */
            if (bcheck)
                section_add(sec, 1, 1);
#endif
        } else {
            addr = align; /* SHN_COMMON is special, symbol value is align */
	    sec = common_section;
        }

        if (v) {
            if (!sym) {
                sym = sym_push(v, type, r | VT_SYM, 0);
                patch_storage(sym, ad, NULL);
            }
            /* Local statics have a scope until now (for
               warnings), remove it here.  */
            sym->sym_scope = 0;
            /* update symbol definition */
	    put_extern_sym(sym, sec, addr, size);
        } else {
            /* push global reference */
            sym = get_sym_ref(type, sec, addr, size);
	    vpushsym(type, sym);
	    vtop->r |= r;
        }

#ifdef CONFIG_TCC_BCHECK
        /* handles bounds now because the symbol must be defined
           before for the relocation */
        if (bcheck) {
            addr_t *bounds_ptr;

            greloca(bounds_section, sym, bounds_section->data_offset, R_DATA_PTR, 0);
            /* then add global bound info */
            bounds_ptr = section_ptr_add(bounds_section, 2 * sizeof(addr_t));
            bounds_ptr[0] = 0; /* relocated */
            bounds_ptr[1] = size;
        }
#endif
    }

    if (type->t & VT_VLA) {
        int a;

        if (NODATA_WANTED)
            goto no_alloc;

        /* save current stack pointer */
        if (vlas_in_scope == 0) {
            if (vla_sp_root_loc == -1)
                vla_sp_root_loc = (loc -= PTR_SIZE);
            gen_vla_sp_save(vla_sp_root_loc);
        }

        vla_runtime_type_size(type, &a);
        gen_vla_alloc(type, a);
#if defined TCC_TARGET_PE && defined TCC_TARGET_X86_64
        /* on _WIN64, because of the function args scratch area, the
           result of alloca differs from RSP and is returned in RAX.  */
        gen_vla_result(addr), addr = (loc -= PTR_SIZE);
#endif
        gen_vla_sp_save(addr);
        vla_sp_loc = addr;
        vlas_in_scope++;

    } else if (has_init) {
	size_t oldreloc_offset = 0;
	if (sec && sec->reloc)
	  oldreloc_offset = sec->reloc->data_offset;
        decl_initializer(type, sec, addr, 1, 0);
	if (sec && sec->reloc)
	  squeeze_multi_relocs(sec, oldreloc_offset);
        /* patch flexible array member size back to -1, */
        /* for possible subsequent similar declarations */
        if (flexible_array)
            flexible_array->type.ref->c = -1;
    }

 no_alloc:
    /* restore parse state if needed */
    if (init_str) {
        end_macro();
        next();
    }

    nocode_wanted = saved_nocode_wanted;
}

/* parse a function defined by symbol 'sym' and generate its code in
   'cur_text_section' */
static void gen_function(Sym *sym)
{
    nocode_wanted = 0;
    ind = cur_text_section->data_offset;
    /* NOTE: we patch the symbol size later */
    put_extern_sym(sym, cur_text_section, ind, 0);
    funcname = get_tok_str(sym->v, NULL);
    func_ind = ind;
    /* Initialize VLA state */
    vla_sp_loc = -1;
    vla_sp_root_loc = -1;
    /* put debug symbol */
    tcc_debug_funcstart(tcc_state, sym);
    /* push a dummy symbol to enable local sym storage */
    sym_push2(&local_stack, SYM_FIELD, 0, 0);
    local_scope = 1; /* for function parameters */
    gfunc_prolog(&sym->type);
    local_scope = 0;
    rsym = 0;
    block(NULL, NULL, 0);
    nocode_wanted = 0;
    gsym(rsym);
    gfunc_epilog();
    cur_text_section->data_offset = ind;
    label_pop(&global_label_stack, NULL, 0);
    /* reset local stack */
    local_scope = 0;
    sym_pop(&local_stack, NULL, 0);
    /* end of function */
    /* patch symbol size */
    elfsym(sym)->st_size = ind - func_ind;
    tcc_debug_funcend(tcc_state, ind - func_ind);
    /* It's better to crash than to generate wrong code */
    cur_text_section = NULL;
    funcname = ""; /* for safety */
    func_vt.t = VT_VOID; /* for safety */
    func_var = 0; /* for safety */
    ind = 0; /* for safety */
    nocode_wanted = 0x80000000;
    check_vstack();
}

static void gen_inline_functions(TCCState *s)
{
    Sym *sym;
    int inline_generated, i, ln;
    struct InlineFunc *fn;

    ln = file->line_num;
    /* iterate while inline function are referenced */
    do {
        inline_generated = 0;
        for (i = 0; i < s->nb_inline_fns; ++i) {
            fn = s->inline_fns[i];
            sym = fn->sym;
            if (sym && sym->c) {
                /* the function was used: generate its code and
                   convert it to a normal function */
                fn->sym = NULL;
                if (file)
                    pstrcpy(file->filename, sizeof file->filename, fn->filename);
                sym->type.t &= ~VT_INLINE;

                begin_macro(fn->func_str, 1);
                next();
                cur_text_section = text_section;
                gen_function(sym);
                end_macro();

                inline_generated = 1;
            }
        }
    } while (inline_generated);
    file->line_num = ln;
}

ST_FUNC void free_inline_functions(TCCState *s)
{
    int i;
    /* free tokens of unused inline functions */
    for (i = 0; i < s->nb_inline_fns; ++i) {
        struct InlineFunc *fn = s->inline_fns[i];
        if (fn->sym)
            tok_str_free(fn->func_str);
    }
    dynarray_reset(&s->inline_fns, &s->nb_inline_fns);
}

/* 'l' is VT_LOCAL or VT_CONST to define default storage type, or VT_CMP
   if parsing old style parameter decl list (and FUNC_SYM is set then) */
static int decl0(int l, int is_for_loop_init, Sym *func_sym)
{
    int v, has_init, r;
    CType type, btype;
    Sym *sym;
    AttributeDef ad;

    while (1) {
        if (!parse_btype(&btype, &ad)) {
            if (is_for_loop_init)
                return 0;
            /* skip redundant ';' if not in old parameter decl scope */
            if (tok == ';' && l != VT_CMP) {
                next();
                continue;
            }
            if (l != VT_CONST)
                break;
            if (tok == TOK_ASM1 || tok == TOK_ASM2 || tok == TOK_ASM3) {
                /* global asm block */
                asm_global_instr();
                continue;
            }
            if (tok >= TOK_UIDENT) {
               /* special test for old K&R protos without explicit int
                  type. Only accepted when defining global data */
                btype.t = VT_INT;
            } else {
                if (tok != TOK_EOF)
                    expect("declaration");
                break;
            }
        }
        if (tok == ';') {
	    if ((btype.t & VT_BTYPE) == VT_STRUCT) {
		int v = btype.ref->v;
		if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) >= SYM_FIRST_ANOM)
        	    tcc_warning("unnamed struct/union that defines no instances");
                next();
                continue;
	    }
            if (IS_ENUM(btype.t)) {
                next();
                continue;
            }
        }
        while (1) { /* iterate thru each declaration */
            type = btype;
	    /* If the base type itself was an array type of unspecified
	       size (like in 'typedef int arr[]; arr x = {1};') then
	       we will overwrite the unknown size by the real one for
	       this decl.  We need to unshare the ref symbol holding
	       that size.  */
	    if ((type.t & VT_ARRAY) && type.ref->c < 0) {
		type.ref = sym_push(SYM_FIELD, &type.ref->type, 0, type.ref->c);
	    }
            type_decl(&type, &ad, &v, TYPE_DIRECT);
#if 0
            {
                char buf[500];
                type_to_str(buf, sizeof(buf), &type, get_tok_str(v, NULL));
                printf("type = '%s'\n", buf);
            }
#endif
            if ((type.t & VT_BTYPE) == VT_FUNC) {
                if ((type.t & VT_STATIC) && (l == VT_LOCAL)) {
                    tcc_error("function without file scope cannot be static");
                }
                /* if old style function prototype, we accept a
                   declaration list */
                sym = type.ref;
                if (sym->f.func_type == FUNC_OLD && l == VT_CONST)
                    decl0(VT_CMP, 0, sym);
            }

            if (gnu_ext && (tok == TOK_ASM1 || tok == TOK_ASM2 || tok == TOK_ASM3)) {
                ad.asm_label = asm_label_instr();
                /* parse one last attribute list, after asm label */
                parse_attribute(&ad);
                if (tok == '{')
                    expect(";");
            }

#ifdef TCC_TARGET_PE
            if (ad.a.dllimport || ad.a.dllexport) {
                if (type.t & (VT_STATIC|VT_TYPEDEF))
                    tcc_error("cannot have dll linkage with static or typedef");
                if (ad.a.dllimport) {
                    if ((type.t & VT_BTYPE) == VT_FUNC)
                        ad.a.dllimport = 0;
                    else
                        type.t |= VT_EXTERN;
                }
            }
#endif
            if (tok == '{') {
                if (l != VT_CONST)
                    tcc_error("cannot use local functions");
                if ((type.t & VT_BTYPE) != VT_FUNC)
                    expect("function definition");

                /* reject abstract declarators in function definition
		   make old style params without decl have int type */
                sym = type.ref;
                while ((sym = sym->next) != NULL) {
                    if (!(sym->v & ~SYM_FIELD))
                        expect("identifier");
		    if (sym->type.t == VT_VOID)
		        sym->type = int_type;
		}
                
                /* XXX: cannot do better now: convert extern line to static inline */
                if ((type.t & (VT_EXTERN | VT_INLINE)) == (VT_EXTERN | VT_INLINE))
                    type.t = (type.t & ~VT_EXTERN) | VT_STATIC;

                /* put function symbol */
                sym = external_global_sym(v, &type, 0);
                type.t &= ~VT_EXTERN;
                patch_storage(sym, &ad, &type);

                /* static inline functions are just recorded as a kind
                   of macro. Their code will be emitted at the end of
                   the compilation unit only if they are used */
                if ((type.t & (VT_INLINE | VT_STATIC)) == 
                    (VT_INLINE | VT_STATIC)) {
                    struct InlineFunc *fn;
                    const char *filename;
                           
                    filename = file ? file->filename : "";
                    fn = tcc_malloc(sizeof *fn + strlen(filename));
                    strcpy(fn->filename, filename);
                    fn->sym = sym;
		    skip_or_save_block(&fn->func_str);
                    dynarray_add(&tcc_state->inline_fns,
				 &tcc_state->nb_inline_fns, fn);
                } else {
                    /* compute text section */
                    cur_text_section = ad.section;
                    if (!cur_text_section)
                        cur_text_section = text_section;
                    gen_function(sym);
                }
                break;
            } else {
		if (l == VT_CMP) {
		    /* find parameter in function parameter list */
		    for (sym = func_sym->next; sym; sym = sym->next)
			if ((sym->v & ~SYM_FIELD) == v)
			    goto found;
		    tcc_error("declaration for parameter '%s' but no such parameter",
			      get_tok_str(v, NULL));
found:
		    if (type.t & VT_STORAGE) /* 'register' is okay */
		        tcc_error("storage class specified for '%s'",
				  get_tok_str(v, NULL));
		    if (sym->type.t != VT_VOID)
		        tcc_error("redefinition of parameter '%s'",
				  get_tok_str(v, NULL));
		    convert_parameter_type(&type);
		    sym->type = type;
		} else if (type.t & VT_TYPEDEF) {
                    /* save typedefed type  */
                    /* XXX: test storage specifiers ? */
                    sym = sym_find(v);
                    if (sym && sym->sym_scope == local_scope) {
                        if (!is_compatible_types(&sym->type, &type)
                            || !(sym->type.t & VT_TYPEDEF))
                            tcc_error("incompatible redefinition of '%s'",
                                get_tok_str(v, NULL));
                        sym->type = type;
                    } else {
                        sym = sym_push(v, &type, 0, 0);
                    }
                    sym->a = ad.a;
                    sym->f = ad.f;
                } else {
                    r = 0;
                    if ((type.t & VT_BTYPE) == VT_FUNC) {
                        /* external function definition */
                        /* specific case for func_call attribute */
                        type.ref->f = ad.f;
                    } else if (!(type.t & VT_ARRAY)) {
                        /* not lvalue if array */
                        r |= lvalue_type(type.t);
                    }
                    has_init = (tok == '=');
                    if (has_init && (type.t & VT_VLA))
                        tcc_error("variable length array cannot be initialized");
                    if (((type.t & VT_EXTERN) && (!has_init || l != VT_CONST)) ||
			((type.t & VT_BTYPE) == VT_FUNC) ||
                        ((type.t & VT_ARRAY) && (type.t & VT_STATIC) &&
                         !has_init && l == VT_CONST && type.ref->c < 0)) {
                        /* external variable or function */
                        /* NOTE: as GCC, uninitialized global static
                           arrays of null size are considered as
                           extern */
                        type.t |= VT_EXTERN;
                        sym = external_sym(v, &type, r, &ad);
                        if (ad.alias_target) {
                            ElfSym *esym;
                            Sym *alias_target;
                            alias_target = sym_find(ad.alias_target);
                            esym = elfsym(alias_target);
                            if (!esym)
                                tcc_error("unsupported forward __alias__ attribute");
                            /* Local statics have a scope until now (for
                               warnings), remove it here.  */
                            sym->sym_scope = 0;
                            put_extern_sym2(sym, esym->st_shndx, esym->st_value, esym->st_size, 0);
                        }
                    } else {
                        if (type.t & VT_STATIC)
                            r |= VT_CONST;
                        else
                            r |= l;
                        if (has_init)
                            next();
                        else if (l == VT_CONST)
                            /* uninitialized global variables may be overridden */
                            type.t |= VT_EXTERN;
                        decl_initializer_alloc(&type, &ad, r, has_init, v, l);
                    }
                }
                if (tok != ',') {
                    if (is_for_loop_init)
                        return 1;
                    skip(';');
                    break;
                }
                next();
            }
            ad.a.aligned = 0;
        }
    }
    return 0;
}

static void decl(int l)
{
    decl0(l, 0, NULL);
}

/* ------------------------------------------------------------------------- */
