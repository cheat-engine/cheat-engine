/*
 *  TCC - Tiny C Compiler - Support for -run switch
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

/* only native compiler supports -run */
#ifdef TCC_IS_NATIVE

#ifdef CONFIG_TCC_BACKTRACE
typedef struct rt_context
{
    /* --> tccelf.c:tcc_add_btstub wants those below in that order: */
    Stab_Sym *stab_sym, *stab_sym_end;
    char *stab_str;
    ElfW(Sym) *esym_start, *esym_end;
    char *elf_str;
    addr_t prog_base;
    void *bounds_start;
    struct rt_context *next;
    /* <-- */
    int num_callers;
    addr_t ip, fp, sp;
    void *top_func;
    jmp_buf jmp_buf;
    char do_jmp;
} rt_context;

static rt_context g_rtctxt;
static void set_exception_handler(void);
static int _rt_error(void *fp, void *ip, const char *fmt, va_list ap);
static void rt_exit(int code);
#endif /* CONFIG_TCC_BACKTRACE */

/* defined when included from lib/bt-exe.c */
#ifndef CONFIG_TCC_BACKTRACE_ONLY

#ifndef _WIN32
# include <sys/mman.h>
#endif

static void set_pages_executable(TCCState *s1, void *ptr, unsigned long length);
static int tcc_relocate_ex(TCCState *s1, void *ptr, addr_t ptr_diff);

#ifdef _WIN64
#ifdef TCC_TARGET_X86_64
static void *win64_add_function_table(TCCState *s1);
static void win64_del_function_table(void *);
#endif
#endif

/* ------------------------------------------------------------- */
/* Do all relocations (needed before using tcc_get_symbol())
   Returns -1 on error. */

LIBTCCAPI int tcc_relocate(TCCState *s1, void *ptr)
{
    int size;
    addr_t ptr_diff = 0;

    if (TCC_RELOCATE_AUTO != ptr)
        return tcc_relocate_ex(s1, ptr, 0);

    size = tcc_relocate_ex(s1, NULL, 0);
    if (size < 0)
        return -1;

#ifdef HAVE_SELINUX
{
    /* Using mmap instead of malloc */
    void *prx;
    char tmpfname[] = "/tmp/.tccrunXXXXXX";
    int fd = mkstemp(tmpfname);
    unlink(tmpfname);
    ftruncate(fd, size);

    size = (size + (PAGESIZE-1)) & ~(PAGESIZE-1);
    ptr = mmap(NULL, size * 2, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    /* mmap RX memory at a fixed distance */
    prx = mmap((char*)ptr + size, size, PROT_READ|PROT_EXEC, MAP_SHARED|MAP_FIXED, fd, 0);
    if (ptr == MAP_FAILED || prx == MAP_FAILED)
	tcc_error("tccrun: could not map memory");
    dynarray_add(&s1->runtime_mem, &s1->nb_runtime_mem, (void*)(addr_t)(size*2));
    ptr_diff = (char*)prx - (char*)ptr;
    close(fd);
    //printf("map %p %p %p\n", ptr, prx, (void*)ptr_diff);
}
#else
	ptr = tcc_malloc(size);
#endif
    tcc_relocate_ex(s1, ptr, ptr_diff); /* no more errors expected */
    dynarray_add(&s1->runtime_mem, &s1->nb_runtime_mem, ptr);
    return 0;
}

ST_FUNC void tcc_run_free(TCCState *s1)
{
    int i;

    for (i = 0; i < s1->nb_runtime_mem; ++i) {
#ifdef HAVE_SELINUX
        unsigned size = (unsigned)(addr_t)s1->runtime_mem[i++];
        munmap(s1->runtime_mem[i], size);
#else
#ifdef _WIN64
#ifdef TCC_TARGET_X86_64
        win64_del_function_table(*(void**)s1->runtime_mem[i]);
#endif
#endif
        tcc_free(s1->runtime_mem[i]);
#endif
    }
    tcc_free(s1->runtime_mem);
}

static void run_cdtors(TCCState *s1, const char *start, const char *end,
                       int argc, char **argv, char **envp)
{
    void **a = (void **)get_sym_addr(s1, start, 0, 0);
    void **b = (void **)get_sym_addr(s1, end, 0, 0);
    while (a != b)
        ((void(*)(int, char **, char **))*a++)(argc, argv, envp);
}

/* launch the compiled program with the given arguments */
LIBTCCAPI int tcc_run(TCCState *s1, int argc, char **argv)
{
    int (*prog_main)(int, char **, char **), ret;
#ifdef CONFIG_TCC_BACKTRACE
    rt_context *rc = &g_rtctxt;
#endif

#if defined(__APPLE__) || defined(__FreeBSD__)
    char **envp = NULL;
#elif defined(__OpenBSD__) || defined(__NetBSD__)
    extern char **environ;
    char **envp = environ;
#else
    char **envp = environ;
#endif

    s1->runtime_main = s1->nostdlib ? "_start" : "main";
    if ((s1->dflag & 16) && (addr_t)-1 == get_sym_addr(s1, s1->runtime_main, 0, 1))
        return 0;
#ifdef CONFIG_TCC_BACKTRACE
    if (s1->do_debug)
        tcc_add_symbol(s1, "exit", rt_exit);
#endif
    if (tcc_relocate(s1, TCC_RELOCATE_AUTO) < 0)
	//if (tcc_relocate(s1, 0xce000000) < 0)
        return -1;
    prog_main = (void*)get_sym_addr(s1, s1->runtime_main, 1, 1);

#ifdef CONFIG_TCC_BACKTRACE
    memset(rc, 0, sizeof *rc);
    if (s1->do_debug) {
        void *p;
        rc->stab_sym = (Stab_Sym *)stab_section->data;
        rc->stab_sym_end = (Stab_Sym *)(stab_section->data + stab_section->data_offset);
        rc->stab_str = (char *)stab_section->link->data;
        rc->esym_start = (ElfW(Sym) *)(symtab_section->data);
        rc->esym_end = (ElfW(Sym) *)(symtab_section->data + symtab_section->data_offset);
        rc->elf_str = (char *)symtab_section->link->data;
#if PTR_SIZE == 8
        rc->prog_base = text_section->sh_addr & 0xffffffff00000000ULL;
#endif
        rc->top_func = tcc_get_symbol(s1, "main");
        rc->num_callers = s1->rt_num_callers;
        rc->do_jmp = 1;
        if ((p = tcc_get_symbol(s1, "__rt_error")))
            *(void**)p = _rt_error;
#ifdef CONFIG_TCC_BCHECK
        if (s1->do_bounds_check) {
            if ((p = tcc_get_symbol(s1, "__bound_init")))
                ((void(*)(void*, int))p)(bounds_section->data, 1);
        }
#endif
        set_exception_handler();
    }
#endif

    errno = 0; /* clean errno value */
    fflush(stdout);
    fflush(stderr);
    /* These aren't C symbols, so don't need leading underscore handling.  */
    run_cdtors(s1, "__init_array_start", "__init_array_end", argc, argv, envp);
#ifdef CONFIG_TCC_BACKTRACE
    if (!rc->do_jmp || !(ret = setjmp(rc->jmp_buf)))
#endif
    {
        ret = prog_main(argc, argv, envp);
    }
    run_cdtors(s1, "__fini_array_start", "__fini_array_end", 0, NULL, NULL);
    if ((s1->dflag & 16) && ret)
        fprintf(s1->ppfp, "[returns %d]\n", ret), fflush(s1->ppfp);
    return ret;
}

#if defined TCC_TARGET_I386 || defined TCC_TARGET_X86_64
/* To avoid that x86 processors would reload cached instructions
   each time when data is written in the near, we need to make
   sure that code and data do not share the same 64 byte unit */
 #define RUN_SECTION_ALIGNMENT 63
#else
 #define RUN_SECTION_ALIGNMENT 0
#endif

/* relocate code. Return -1 on error, required size if ptr is NULL,
   otherwise copy code into buffer passed by the caller */
static int tcc_relocate_ex(TCCState *s1, void *ptr, addr_t ptr_diff)
{
    Section *s;
    unsigned offset, length, align, max_align, i, k, f;
    addr_t mem, addr;

    if (NULL == ptr) {
        s1->nb_errors = 0;
#ifdef TCC_TARGET_PE
        pe_output_file(s1, NULL);
#else
        tcc_add_runtime(s1);
		resolve_common_syms(s1);
        build_got_entries(s1);
#endif
        if (s1->nb_errors)
            return -1;
    }

    offset = max_align = 0, mem = (addr_t)ptr;
#ifdef _WIN64
	if (s1->binary_writer_func == 0)  //Cheat Engine does not put that there
      offset += sizeof (void*); /* space for function_table pointer */
#endif
    for (k = 0; k < 2; ++k) {
        f = 0, addr = k ? mem : mem + ptr_diff;
        for(i = 1; i < s1->nb_sections; i++) {
            s = s1->sections[i];
            if (0 == (s->sh_flags & SHF_ALLOC))
                continue;
            if (k != !(s->sh_flags & SHF_EXECINSTR))
                continue;
            align = s->sh_addralign - 1;
            if (++f == 1 && align < RUN_SECTION_ALIGNMENT)
                align = RUN_SECTION_ALIGNMENT;
            if (max_align < align)
                max_align = align;
            offset += -(addr + offset) & align;
            s->sh_addr = mem ? addr + offset : 0;
            offset += s->data_offset;
#if 0
            if (mem)
                printf("%-16s %p  len %04x  align %2d\n",
                    s->name, (void*)s->sh_addr, (unsigned)s->data_offset, align + 1);
#endif
        }
    }

    /* relocate symbols */
    relocate_syms(s1, s1->symtab, !(s1->nostdlib));
    if (s1->nb_errors)
        return -1;

    if (0 == mem)
        return offset + max_align;

#ifdef TCC_TARGET_PE
    s1->pe_imagebase = mem;
#endif

    /* relocate each section */
    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        if (s->reloc)
            relocate_section(s1, s);
    }
#if !defined(TCC_TARGET_PE) || defined(TCC_TARGET_MACHO)
    relocate_plt(s1);
#endif

    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
		if (0 == (s->sh_flags & SHF_ALLOC))
			continue;
		
        length = s->data_offset;
        ptr = (void*)s->sh_addr;
        if (s->sh_flags & SHF_EXECINSTR)
            ptr = (char*)((addr_t)ptr - ptr_diff);
		if (NULL == s->data || s->sh_type == SHT_NOBITS)
		{
			//cheat engine binary writer addition start
			if (s1->binary_writer_func)
			{
				if (length)
				{
					void *zeromem = tcc_malloc(length);
                    memset(zeromem, 0, length);
					//ZeroMemory(zeromem, length);
					s1->binary_writer_func(s1->binary_writer_param, ptr, zeromem, length);
					tcc_free(zeromem);
				}
				
			}
			else
		    //cheat engine binary writer addition stop
				memset(ptr, 0, length);
		}
		else
		{
			//cheat engine binary writer addition start
			if (s1->binary_writer_func)
			{
				if (length)
					s1->binary_writer_func(s1->binary_writer_param, ptr, s->data, length);
			}
			else
			//cheat engine binary writer addition stop
				memcpy(ptr, s->data, length);
		}
        /* mark executable sections as executable in memory */
        if (s->sh_flags & SHF_EXECINSTR)
            set_pages_executable(s1, (char*)((addr_t)ptr + ptr_diff), length);
    }

#ifdef _WIN64
#ifdef TCC_TARGET_X86_64
	if (s1->binary_writer_func==0)
		*(void**)mem = win64_add_function_table(s1);
#endif
#endif

    return 0;
}

/* ------------------------------------------------------------- */
/* allow to run code in memory */

static void set_pages_executable(TCCState *s1, void *ptr, unsigned long length)
{
  //Cheat Engine modification:  This code is not needed
  return;
  //Cheat Engine modification end



#ifdef _WIN32
    unsigned long old_protect;
    VirtualProtect(ptr, length, PAGE_EXECUTE_READWRITE, &old_protect);
#else
    void __clear_cache(void *beginning, void *end);
# ifndef HAVE_SELINUX
    addr_t start, end;
    start = (addr_t)ptr & ~(PAGESIZE - 1);
    end = (addr_t)ptr + length;
    end = (end + PAGESIZE - 1) & ~(PAGESIZE - 1);
    if (mprotect((void *)start, end - start, PROT_READ | PROT_WRITE | PROT_EXEC))
        tcc_error("mprotect failed: did you mean to configure --with-selinux?");
# endif
/* XXX: BSD sometimes dump core with bad system call */
# if (defined(TCC_TARGET_ARM) && \
      !defined(__FreeBSD__) && !defined(__OpenBSD__) && !defined(__NetBSD__)) || \
     defined(TCC_TARGET_ARM64)
    __clear_cache(ptr, (char *)ptr + length);
# endif
#endif
}

#ifdef _WIN64
#ifdef TCC_TARGET_X86_64
static void *win64_add_function_table(TCCState *s1)
{
    void *p = NULL;
    if (s1->uw_pdata) {
        p = (void*)s1->uw_pdata->sh_addr;
        RtlAddFunctionTable(
            (RUNTIME_FUNCTION*)p,
            s1->uw_pdata->data_offset / sizeof (RUNTIME_FUNCTION),
            s1->pe_imagebase
            );
        s1->uw_pdata = NULL;
    }
    return p;
}
#endif

#ifdef TCC_TARGET_X86_64
static void win64_del_function_table(void *p)
{
    if (p) {
        RtlDeleteFunctionTable((RUNTIME_FUNCTION*)p);
    }
}
#endif
#endif
#endif //ndef CONFIG_TCC_BACKTRACE_ONLY
/* ------------------------------------------------------------- */
#ifdef CONFIG_TCC_BACKTRACE

static int rt_vprintf(const char *fmt, va_list ap)
{
    int ret = vfprintf(stderr, fmt, ap);
    fflush(stderr);
    return ret;
}

static int rt_printf(const char *fmt, ...)
{
    va_list ap;
    int r;
    va_start(ap, fmt);
    r = rt_vprintf(fmt, ap);
    va_end(ap);
    return r;
}

#define INCLUDE_STACK_SIZE 32

/* print the position in the source file of PC value 'pc' by reading
   the stabs debug information */
static addr_t rt_printline (rt_context *rc, addr_t wanted_pc,
    const char *msg, const char *skip)
{
    char func_name[128];
    addr_t func_addr, last_pc, pc;
    const char *incl_files[INCLUDE_STACK_SIZE];
    int incl_index, last_incl_index, len, last_line_num, i;
    const char *str, *p;
    ElfW(Sym) *esym;
    Stab_Sym *sym;

next:
    func_name[0] = '\0';
    func_addr = 0;
    incl_index = 0;
    last_pc = (addr_t)-1;
    last_line_num = 1;
    last_incl_index = 0;

    for (sym = rc->stab_sym + 1; sym < rc->stab_sym_end; ++sym) {
        str = rc->stab_str + sym->n_strx;
        pc = sym->n_value;

        switch(sym->n_type) {
        case N_SLINE:
            if (func_addr)
                goto rel_pc;
        case N_SO:
        case N_SOL:
            goto abs_pc;
        case N_FUN:
            if (sym->n_strx == 0) /* end of function */
                goto rel_pc;
        abs_pc:
#if PTR_SIZE == 8
            /* Stab_Sym.n_value is only 32bits */
            pc += rc->prog_base;
#endif
            goto check_pc;
        rel_pc:
            pc += func_addr;
        check_pc:
            if (pc >= wanted_pc && wanted_pc >= last_pc)
                goto found;
            break;
        }

        switch(sym->n_type) {
            /* function start or end */
        case N_FUN:
            if (sym->n_strx == 0)
                goto reset_func;
            p = strchr(str, ':');
            if (0 == p || (len = p - str + 1, len > sizeof func_name))
                len = sizeof func_name;
            pstrcpy(func_name, len, str);
            func_addr = pc;
            break;
            /* line number info */
        case N_SLINE:
            last_pc = pc;
            last_line_num = sym->n_desc;
            last_incl_index = incl_index;
            break;
            /* include files */
        case N_BINCL:
            if (incl_index < INCLUDE_STACK_SIZE)
                incl_files[incl_index++] = str;
            break;
        case N_EINCL:
            if (incl_index > 1)
                incl_index--;
            break;
            /* start/end of translation unit */
        case N_SO:
            incl_index = 0;
            if (sym->n_strx) {
                /* do not add path */
                len = strlen(str);
                if (len > 0 && str[len - 1] != '/')
                    incl_files[incl_index++] = str;
            }
        reset_func:
            func_name[0] = '\0';
            func_addr = 0;
            last_pc = (addr_t)-1;
            break;
            /* alternative file name (from #line or #include directives) */
        case N_SOL:
            if (incl_index)
                incl_files[incl_index-1] = str;
            break;
        }
    }

    func_name[0] = '\0';
    func_addr = 0;
    last_incl_index = 0;

    /* we try symtab symbols (no line number info) */
    for (esym = rc->esym_start + 1; esym < rc->esym_end; ++esym) {
        int type = ELFW(ST_TYPE)(esym->st_info);
        if (type == STT_FUNC || type == STT_GNU_IFUNC) {
            if (wanted_pc >= esym->st_value &&
                wanted_pc < esym->st_value + esym->st_size) {
                pstrcpy(func_name, sizeof(func_name),
                    rc->elf_str + esym->st_name);
                func_addr = esym->st_value;
                goto found;
            }
        }
    }

    if ((rc = rc->next))
        goto next;

found:
    i = last_incl_index;
    if (i > 0) {
        str = incl_files[--i];
        if (skip[0] && strstr(str, skip))
            return (addr_t)-1;
        rt_printf("%s:%d: ", str, last_line_num);
    } else
        rt_printf("%08llx : ", (long long)wanted_pc);
    rt_printf("%s %s", msg, func_name[0] ? func_name : "???");
#if 0
    if (--i >= 0) {
        rt_printf(" (included from ");
        for (;;) {
            rt_printf("%s", incl_files[i]);
            if (--i < 0)
                break;
            rt_printf(", ");
        }
        rt_printf(")");
    }
#endif
    return func_addr;
}

static int rt_get_caller_pc(addr_t *paddr, rt_context *rc, int level);

static int _rt_error(void *fp, void *ip, const char *fmt, va_list ap)
{
    rt_context *rc = &g_rtctxt;
    addr_t pc = 0;
    char skip[100];
    int i, level, ret, n;
    const char *a, *b, *msg;

    if (fp) {
        /* we're called from tcc_backtrace. */
        rc->fp = (addr_t)fp;
        rc->ip = (addr_t)ip;
        msg = "";
    } else {
        /* we're called from signal/exception handler */
        msg = "RUNTIME ERROR: ";
    }

    skip[0] = 0;
    /* If fmt is like "^file.c^..." then skip calls from 'file.c' */
    if (fmt[0] == '^' && (b = strchr(a = fmt + 1, fmt[0]))) {
        memcpy(skip, a, b - a), skip[b - a] = 0;
        fmt = b + 1;
    }

    n = rc->num_callers ? rc->num_callers : 6;
    for (i = level = 0; level < n; i++) {
        ret = rt_get_caller_pc(&pc, rc, i);
        a = "%s";
        if (ret != -1) {
            pc = rt_printline(rc, pc, level ? "by" : "at", skip);
            if (pc == (addr_t)-1)
                continue;
            a = ": %s";
        }
        if (level == 0) {
            rt_printf(a, msg);
            rt_vprintf(fmt, ap);
        } else if (ret == -1)
            break;
        rt_printf("\n");
        if (ret == -1 || (pc == (addr_t)rc->top_func && pc))
            break;
        ++level;
    }

    rc->ip = rc->fp = 0;
    return 0;
}

/* emit a run time error at position 'pc' */
static int rt_error(const char *fmt, ...)
{
    va_list ap;
    int ret;
    va_start(ap, fmt);
    ret = _rt_error(0, 0, fmt, ap);
    va_end(ap);
    return ret;
}

static void rt_exit(int code)
{
    rt_context *rc = &g_rtctxt;
    if (rc->do_jmp)
        longjmp(rc->jmp_buf, code ? code : 256);
    exit(code);
}

/* ------------------------------------------------------------- */

#ifndef _WIN32
# include <signal.h>
# ifndef __OpenBSD__
#  include <sys/ucontext.h>
# endif
#else
# define ucontext_t CONTEXT
#endif

/* translate from ucontext_t* to internal rt_context * */
static void rt_getcontext(ucontext_t *uc, rt_context *rc)
{
#if defined _WIN64
    rc->ip = uc->Rip;
    rc->fp = uc->Rbp;
    rc->sp = uc->Rsp;
#elif defined _WIN32
    rc->ip = uc->Eip;
    rc->fp = uc->Ebp;
    rc->sp = uc->Esp;
#elif defined __i386__
# if defined(__APPLE__)
    rc->ip = uc->uc_mcontext->__ss.__eip;
    rc->fp = uc->uc_mcontext->__ss.__ebp;
# elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__) || defined(__DragonFly__)
    rc->ip = uc->uc_mcontext.mc_eip;
    rc->fp = uc->uc_mcontext.mc_ebp;
# elif defined(__dietlibc__)
    rc->ip = uc->uc_mcontext.eip;
    rc->fp = uc->uc_mcontext.ebp;
# elif defined(__NetBSD__)
    rc->ip = uc->uc_mcontext.__gregs[_REG_EIP];
    rc->fp = uc->uc_mcontext.__gregs[_REG_EBP];
# elif defined(__OpenBSD__)
    rc->ip = uc->sc_eip;
    rc->fp = uc->sc_ebp;
# elif !defined REG_EIP && defined EIP /* fix for glibc 2.1 */
    rc->ip = uc->uc_mcontext.gregs[EIP];
    rc->fp = uc->uc_mcontext.gregs[EBP];
# else
    rc->ip = uc->uc_mcontext.gregs[REG_EIP];
    rc->fp = uc->uc_mcontext.gregs[REG_EBP];
# endif
#elif defined(__x86_64__)
# if defined(__APPLE__)
    rc->ip = uc->uc_mcontext->__ss.__rip;
    rc->fp = uc->uc_mcontext->__ss.__rbp;
# elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__) || defined(__DragonFly__)
    rc->ip = uc->uc_mcontext.mc_rip;
    rc->fp = uc->uc_mcontext.mc_rbp;
# elif defined(__NetBSD__)
    rc->ip = uc->uc_mcontext.__gregs[_REG_RIP];
    rc->fp = uc->uc_mcontext.__gregs[_REG_RBP];
# elif defined(__OpenBSD__)
    rc->ip = uc->sc_rip;
    rc->fp = uc->sc_rbp;
# else
    rc->ip = uc->uc_mcontext.gregs[REG_RIP];
    rc->fp = uc->uc_mcontext.gregs[REG_RBP];
# endif
#elif defined(__arm__) && defined(__NetBSD__)
    rc->ip = uc->uc_mcontext.__gregs[_REG_PC];
    rc->fp = uc->uc_mcontext.__gregs[_REG_FP];
#elif defined(__arm__) && defined(__OpenBSD__)
    rc->ip = uc->sc_pc;
    rc->fp = uc->sc_r11;
#elif defined(__arm__) && defined(__FreeBSD__)
    rc->ip = uc->uc_mcontext.__gregs[_REG_PC];
    rc->fp = uc->uc_mcontext.__gregs[_REG_FP];
#elif defined(__arm__)
    rc->ip = uc->uc_mcontext.arm_pc;
    rc->fp = uc->uc_mcontext.arm_fp;
#elif defined(__aarch64__) && defined(__FreeBSD__)
    rc->ip = uc->uc_mcontext.mc_gpregs.gp_elr; /* aka REG_PC */
    rc->fp = uc->uc_mcontext.mc_gpregs.gp_x[29];
#elif defined(__aarch64__) && defined(__NetBSD__)
    rc->ip = uc->uc_mcontext.__gregs[_REG_PC];
    rc->fp = uc->uc_mcontext.__gregs[_REG_FP];
#elif defined(__aarch64__) && defined(__OpenBSD__)
    rc->ip = uc->sc_elr;
    rc->fp = uc->sc_x[29];
#elif defined(__aarch64__)
    rc->ip = uc->uc_mcontext.pc;
    rc->fp = uc->uc_mcontext.regs[29];
#elif defined(__riscv)
    rc->ip = uc->uc_mcontext.__gregs[REG_PC];
    rc->fp = uc->uc_mcontext.__gregs[REG_S0];
#endif
}

/* ------------------------------------------------------------- */
#ifndef _WIN32
/* signal handler for fatal errors */
static void sig_error(int signum, siginfo_t *siginf, void *puc)
{
    rt_context *rc = &g_rtctxt;
    rt_getcontext(puc, rc);

    switch(signum) {
    case SIGFPE:
        switch(siginf->si_code) {
        case FPE_INTDIV:
        case FPE_FLTDIV:
            rt_error("division by zero");
            break;
        default:
            rt_error("floating point exception");
            break;
        }
        break;
    case SIGBUS:
    case SIGSEGV:
        rt_error("invalid memory access");
        break;
    case SIGILL:
        rt_error("illegal instruction");
        break;
    case SIGABRT:
        rt_error("abort() called");
        break;
    default:
        rt_error("caught signal %d", signum);
        break;
    }
    rt_exit(255);
}

#ifndef SA_SIGINFO
# define SA_SIGINFO 0x00000004u
#endif

/* Generate a stack backtrace when a CPU exception occurs. */
static void set_exception_handler(void)
{
    struct sigaction sigact;
    /* install TCC signal handlers to print debug info on fatal
       runtime errors */
    sigact.sa_flags = SA_SIGINFO | SA_RESETHAND;
#if 0//def SIGSTKSZ // this causes signals not to work at all on some (older) linuxes
    sigact.sa_flags |= SA_ONSTACK;
#endif
    sigact.sa_sigaction = sig_error;
    sigemptyset(&sigact.sa_mask);
    sigaction(SIGFPE, &sigact, NULL);
    sigaction(SIGILL, &sigact, NULL);
    sigaction(SIGSEGV, &sigact, NULL);
    sigaction(SIGBUS, &sigact, NULL);
    sigaction(SIGABRT, &sigact, NULL);
#if 0//def SIGSTKSZ
    /* This allows stack overflow to be reported instead of a SEGV */
    {
        stack_t ss;
        static unsigned char stack[SIGSTKSZ] __attribute__((aligned(16)));

        ss.ss_sp = stack;
        ss.ss_size = SIGSTKSZ;
        ss.ss_flags = 0;
        sigaltstack(&ss, NULL);
    }
#endif
}

#else /* WIN32 */

/* signal handler for fatal errors */
static long __stdcall cpu_exception_handler(EXCEPTION_POINTERS *ex_info)
{
    rt_context *rc = &g_rtctxt;
    unsigned code;
    rt_getcontext(ex_info->ContextRecord, rc);

    switch (code = ex_info->ExceptionRecord->ExceptionCode) {
    case EXCEPTION_ACCESS_VIOLATION:
	rt_error("invalid memory access");
        break;
    case EXCEPTION_STACK_OVERFLOW:
        rt_error("stack overflow");
        break;
    case EXCEPTION_INT_DIVIDE_BY_ZERO:
        rt_error("division by zero");
        break;
    case EXCEPTION_BREAKPOINT:
    case EXCEPTION_SINGLE_STEP:
        rc->ip = *(addr_t*)rc->sp;
        rt_error("breakpoint/single-step exception:");
        return EXCEPTION_CONTINUE_SEARCH;
    default:
        rt_error("caught exception %08x", code);
        break;
    }
    if (rc->do_jmp)
        rt_exit(255);
    return EXCEPTION_EXECUTE_HANDLER;
}

/* Generate a stack backtrace when a CPU exception occurs. */
static void set_exception_handler(void)
{
    SetUnhandledExceptionFilter(cpu_exception_handler);
}

#endif

/* ------------------------------------------------------------- */
/* return the PC at frame level 'level'. Return negative if not found */
#if defined(__i386__) || defined(__x86_64__)
static int rt_get_caller_pc(addr_t *paddr, rt_context *rc, int level)
{
    addr_t ip, fp;
    if (level == 0) {
        ip = rc->ip;
    } else {
        ip = 0;
        fp = rc->fp;
        while (--level) {
            /* XXX: check address validity with program info */
            if (fp <= 0x1000)
                break;
            fp = ((addr_t *)fp)[0];
        }
        if (fp > 0x1000)
            ip = ((addr_t *)fp)[1];
    }
    if (ip <= 0x1000)
        return -1;
    *paddr = ip;
    return 0;
}

#elif defined(__arm__)
static int rt_get_caller_pc(addr_t *paddr, rt_context *rc, int level)
{
    /* XXX: only supports linux/bsd */
#if !defined(__linux__) && \
    !defined(__FreeBSD__) && !defined(__OpenBSD__) && !defined(__NetBSD__)
    return -1;
#else
    if (level == 0) {
        *paddr = rc->ip;
    } else {
        addr_t fp = rc->fp;
        while (--level)
            fp = ((addr_t *)fp)[0];
        *paddr = ((addr_t *)fp)[2];
    }
    return 0;
#endif
}

#elif defined(__aarch64__)
static int rt_get_caller_pc(addr_t *paddr, rt_context *rc, int level)
{
    if (level == 0) {
        *paddr = rc->ip;
    } else {
        addr_t *fp = (addr_t*)rc->fp;
        while (--level)
            fp = (addr_t *)fp[0];
        *paddr = fp[1];
    }
    return 0;
}

#elif defined(__riscv)
static int rt_get_caller_pc(addr_t *paddr, rt_context *rc, int level)
{
    if (level == 0) {
        *paddr = rc->ip;
    } else {
        addr_t *fp = (addr_t*)rc->fp;
        while (--level && fp >= (addr_t*)0x1000)
            fp = (addr_t *)fp[-2];
        if (fp < (addr_t*)0x1000)
          return -1;
        *paddr = fp[-1];
    }
    return 0;
}

#else
#warning add arch specific rt_get_caller_pc()
static int rt_get_caller_pc(addr_t *paddr, rt_context *rc, int level)
{
    return -1;
}

#endif
#endif /* CONFIG_TCC_BACKTRACE */
/* ------------------------------------------------------------- */
#ifdef CONFIG_TCC_STATIC

/* dummy function for profiling */
ST_FUNC void *dlopen(const char *filename, int flag)
{
    return NULL;
}

ST_FUNC void dlclose(void *p)
{
}

ST_FUNC const char *dlerror(void)
{
    return "error";
}

typedef struct TCCSyms {
    char *str;
    void *ptr;
} TCCSyms;


/* add the symbol you want here if no dynamic linking is done */
static TCCSyms tcc_syms[] = {
#if !defined(CONFIG_TCCBOOT)
#define TCCSYM(a) { #a, &a, },
    TCCSYM(printf)
    TCCSYM(fprintf)
    TCCSYM(fopen)
    TCCSYM(fclose)
#undef TCCSYM
#endif
    { NULL, NULL },
};

ST_FUNC void *dlsym(void *handle, const char *symbol)
{
    TCCSyms *p;
    p = tcc_syms;
    while (p->str != NULL) {
        if (!strcmp(p->str, symbol))
            return p->ptr;
        p++;
    }
    return NULL;
}

#endif /* CONFIG_TCC_STATIC */
#endif /* TCC_IS_NATIVE */
/* ------------------------------------------------------------- */
