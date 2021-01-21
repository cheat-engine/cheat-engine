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

#ifndef _WIN32
# include <sys/mman.h>
#endif

#ifdef CONFIG_TCC_BACKTRACE
# ifndef _WIN32
#  include <signal.h>
#  ifndef __OpenBSD__
#   include <sys/ucontext.h>
#  endif
# else
#  define ucontext_t CONTEXT
# endif
ST_DATA int rt_num_callers = 6;
ST_DATA const char **rt_bound_error_msg;
ST_DATA void *rt_prog_main;
static int rt_get_caller_pc(addr_t *paddr, ucontext_t *uc, int level);
static void rt_error(ucontext_t *uc, const char *fmt, ...);
static void set_exception_handler(void);
#endif

static void set_pages_executable(void *ptr, unsigned long length);
static int tcc_relocate_ex(TCCState *s1, void *ptr, addr_t ptr_diff);

#ifdef _WIN64
static void *win64_add_function_table(TCCState *s1);
static void win64_del_function_table(void *);
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

    ptr = mmap (NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    prx = mmap (NULL, size, PROT_READ|PROT_EXEC, MAP_SHARED, fd, 0);
    if (ptr == MAP_FAILED || prx == MAP_FAILED)
	tcc_error("tccrun: could not map memory");
    dynarray_add(&s1->runtime_mem, &s1->nb_runtime_mem, (void*)(addr_t)size);
    dynarray_add(&s1->runtime_mem, &s1->nb_runtime_mem, prx);
    ptr_diff = (char*)prx - (char*)ptr;
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
        munmap(s1->runtime_mem[i++], size);
        munmap(s1->runtime_mem[i], size);
#else
#ifdef _WIN64
        win64_del_function_table(*(void**)s1->runtime_mem[i]);
#endif
        tcc_free(s1->runtime_mem[i]);
#endif
    }
    tcc_free(s1->runtime_mem);
}

/* launch the compiled program with the given arguments */
LIBTCCAPI int tcc_run(TCCState *s1, int argc, char **argv)
{
    int (*prog_main)(int, char **);

    s1->runtime_main = "main";
    if ((s1->dflag & 16) && !find_elf_sym(s1->symtab, s1->runtime_main))
        return 0;
    if (tcc_relocate(s1, TCC_RELOCATE_AUTO) < 0)
        return -1;
    prog_main = tcc_get_symbol_err(s1, s1->runtime_main);

#ifdef CONFIG_TCC_BACKTRACE
    if (s1->do_debug) {
        set_exception_handler();
        rt_prog_main = prog_main;
    }
#endif

    errno = 0; /* clean errno value */

#ifdef CONFIG_TCC_BCHECK
    if (s1->do_bounds_check) {
        void (*bound_init)(void);
        void (*bound_exit)(void);
        void (*bound_new_region)(void *p, addr_t size);
        int  (*bound_delete_region)(void *p);
        int i, ret;

        /* set error function */
        rt_bound_error_msg = tcc_get_symbol_err(s1, "__bound_error_msg");
        /* XXX: use .init section so that it also work in binary ? */
        bound_init = tcc_get_symbol_err(s1, "__bound_init");
        bound_exit = tcc_get_symbol_err(s1, "__bound_exit");
        bound_new_region = tcc_get_symbol_err(s1, "__bound_new_region");
        bound_delete_region = tcc_get_symbol_err(s1, "__bound_delete_region");

        bound_init();
        /* mark argv area as valid */
        bound_new_region(argv, argc*sizeof(argv[0]));
        for (i=0; i<argc; ++i)
            bound_new_region(argv[i], strlen(argv[i]) + 1);

        ret = (*prog_main)(argc, argv);

        /* unmark argv area */
        for (i=0; i<argc; ++i)
            bound_delete_region(argv[i]);
        bound_delete_region(argv);
        bound_exit();
        return ret;
    }
#endif
    return (*prog_main)(argc, argv);
}

#if defined TCC_TARGET_I386 || defined TCC_TARGET_X86_64
 #define RUN_SECTION_ALIGNMENT 63
#else
 #define RUN_SECTION_ALIGNMENT 15
#endif

/* relocate code. Return -1 on error, required size if ptr is NULL,
   otherwise copy code into buffer passed by the caller */
static int tcc_relocate_ex(TCCState *s1, void *ptr, addr_t ptr_diff)
{
    Section *s;
    unsigned offset, length, fill, i, k;
    addr_t mem;

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

    offset = 0, mem = (addr_t)ptr;
    fill = -mem & RUN_SECTION_ALIGNMENT;
#ifdef _WIN64
    offset += sizeof (void*);
#endif
    for (k = 0; k < 2; ++k) {
        for(i = 1; i < s1->nb_sections; i++) {
            s = s1->sections[i];
            if (0 == (s->sh_flags & SHF_ALLOC))
                continue;
            if (k != !(s->sh_flags & SHF_EXECINSTR))
                continue;
            offset += fill;
            if (!mem)
                s->sh_addr = 0;
            else if (s->sh_flags & SHF_EXECINSTR)
                s->sh_addr = mem + offset + ptr_diff;
            else
                s->sh_addr = mem + offset;
#if 0
            if (mem)
                printf("%-16s +%02lx %p %04x\n",
                    s->name, fill, (void*)s->sh_addr, (unsigned)s->data_offset);
#endif
            offset += s->data_offset;
            fill = -(mem + offset) & 15;
        }
#if RUN_SECTION_ALIGNMENT > 15
        /* To avoid that x86 processors would reload cached instructions each time
           when data is written in the near, we need to make sure that code and data
           do not share the same 64 byte unit */
        fill = -(mem + offset) & RUN_SECTION_ALIGNMENT;
#endif
    }

    /* relocate symbols */
    relocate_syms(s1, s1->symtab, 1);
    if (s1->nb_errors)
        return -1;

    if (0 == mem)
        return offset + RUN_SECTION_ALIGNMENT;

#ifdef TCC_TARGET_PE
    s1->pe_imagebase = mem;
#endif

    /* relocate each section */
    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        if (s->reloc)
            relocate_section(s1, s);
    }
    relocate_plt(s1);

    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        if (0 == (s->sh_flags & SHF_ALLOC))
            continue;
        length = s->data_offset;
        ptr = (void*)s->sh_addr;
        if (s->sh_flags & SHF_EXECINSTR)
            ptr = (char*)ptr - ptr_diff;
        if (NULL == s->data || s->sh_type == SHT_NOBITS)
            memset(ptr, 0, length);
        else
            memcpy(ptr, s->data, length);
        /* mark executable sections as executable in memory */
        if (s->sh_flags & SHF_EXECINSTR)
            set_pages_executable((char*)ptr + ptr_diff, length);
    }

#ifdef _WIN64
    *(void**)mem = win64_add_function_table(s1);
#endif

    return 0;
}

/* ------------------------------------------------------------- */
/* allow to run code in memory */

static void set_pages_executable(void *ptr, unsigned long length)
{
#ifdef _WIN32
    unsigned long old_protect;
    VirtualProtect(ptr, length, PAGE_EXECUTE_READWRITE, &old_protect);
#else
    void __clear_cache(void *beginning, void *end);
# ifndef HAVE_SELINUX
    addr_t start, end;
#  ifndef PAGESIZE
#   define PAGESIZE 4096
#  endif
    start = (addr_t)ptr & ~(PAGESIZE - 1);
    end = (addr_t)ptr + length;
    end = (end + PAGESIZE - 1) & ~(PAGESIZE - 1);
    if (mprotect((void *)start, end - start, PROT_READ | PROT_WRITE | PROT_EXEC))
        tcc_error("mprotect failed: did you mean to configure --with-selinux?");
# endif
# if defined TCC_TARGET_ARM || defined TCC_TARGET_ARM64
    __clear_cache(ptr, (char *)ptr + length);
# endif
#endif
}

#ifdef _WIN64
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

static void win64_del_function_table(void *p)
{
    if (p) {
        RtlDeleteFunctionTable((RUNTIME_FUNCTION*)p);
    }
}
#endif

/* ------------------------------------------------------------- */
#ifdef CONFIG_TCC_BACKTRACE

ST_FUNC void tcc_set_num_callers(int n)
{
    rt_num_callers = n;
}

/* print the position in the source file of PC value 'pc' by reading
   the stabs debug information */
static addr_t rt_printline(addr_t wanted_pc, const char *msg)
{
    char func_name[128], last_func_name[128];
    addr_t func_addr, last_pc, pc;
    const char *incl_files[INCLUDE_STACK_SIZE];
    int incl_index, len, last_line_num, i;
    const char *str, *p;

    Stab_Sym *stab_sym = NULL, *stab_sym_end, *sym;
    int stab_len = 0;
    char *stab_str = NULL;

    if (stab_section) {
        stab_len = stab_section->data_offset;
        stab_sym = (Stab_Sym *)stab_section->data;
        stab_str = (char *) stabstr_section->data;
    }

    func_name[0] = '\0';
    func_addr = 0;
    incl_index = 0;
    last_func_name[0] = '\0';
    last_pc = (addr_t)-1;
    last_line_num = 1;

    if (!stab_sym)
        goto no_stabs;

    stab_sym_end = (Stab_Sym*)((char*)stab_sym + stab_len);
    for (sym = stab_sym + 1; sym < stab_sym_end; ++sym) {
        switch(sym->n_type) {
            /* function start or end */
        case N_FUN:
            if (sym->n_strx == 0) {
                /* we test if between last line and end of function */
                pc = sym->n_value + func_addr;
                if (wanted_pc >= last_pc && wanted_pc < pc)
                    goto found;
                func_name[0] = '\0';
                func_addr = 0;
            } else {
                str = stab_str + sym->n_strx;
                p = strchr(str, ':');
                if (!p) {
                    pstrcpy(func_name, sizeof(func_name), str);
                } else {
                    len = p - str;
                    if (len > sizeof(func_name) - 1)
                        len = sizeof(func_name) - 1;
                    memcpy(func_name, str, len);
                    func_name[len] = '\0';
                }
                func_addr = sym->n_value;
            }
            break;
            /* line number info */
        case N_SLINE:
            pc = sym->n_value + func_addr;
            if (wanted_pc >= last_pc && wanted_pc < pc)
                goto found;
            last_pc = pc;
            last_line_num = sym->n_desc;
            /* XXX: slow! */
            strcpy(last_func_name, func_name);
            break;
            /* include files */
        case N_BINCL:
            str = stab_str + sym->n_strx;
        add_incl:
            if (incl_index < INCLUDE_STACK_SIZE) {
                incl_files[incl_index++] = str;
            }
            break;
        case N_EINCL:
            if (incl_index > 1)
                incl_index--;
            break;
        case N_SO:
            if (sym->n_strx == 0) {
                incl_index = 0; /* end of translation unit */
            } else {
                str = stab_str + sym->n_strx;
                /* do not add path */
                len = strlen(str);
                if (len > 0 && str[len - 1] != '/')
                    goto add_incl;
            }
            break;
        }
    }

no_stabs:
    /* second pass: we try symtab symbols (no line number info) */
    incl_index = 0;
    if (symtab_section)
    {
        ElfW(Sym) *sym, *sym_end;
        int type;

        sym_end = (ElfW(Sym) *)(symtab_section->data + symtab_section->data_offset);
        for(sym = (ElfW(Sym) *)symtab_section->data + 1;
            sym < sym_end;
            sym++) {
            type = ELFW(ST_TYPE)(sym->st_info);
            if (type == STT_FUNC || type == STT_GNU_IFUNC) {
                if (wanted_pc >= sym->st_value &&
                    wanted_pc < sym->st_value + sym->st_size) {
                    pstrcpy(last_func_name, sizeof(last_func_name),
                            (char *) symtab_section->link->data + sym->st_name);
                    func_addr = sym->st_value;
                    goto found;
                }
            }
        }
    }
    /* did not find any info: */
    fprintf(stderr, "%s %p ???\n", msg, (void*)wanted_pc);
    fflush(stderr);
    return 0;
 found:
    i = incl_index;
    if (i > 0)
        fprintf(stderr, "%s:%d: ", incl_files[--i], last_line_num);
    fprintf(stderr, "%s %p", msg, (void*)wanted_pc);
    if (last_func_name[0] != '\0')
        fprintf(stderr, " %s()", last_func_name);
    if (--i >= 0) {
        fprintf(stderr, " (included from ");
        for (;;) {
            fprintf(stderr, "%s", incl_files[i]);
            if (--i < 0)
                break;
            fprintf(stderr, ", ");
        }
        fprintf(stderr, ")");
    }
    fprintf(stderr, "\n");
    fflush(stderr);
    return func_addr;
}

/* emit a run time error at position 'pc' */
static void rt_error(ucontext_t *uc, const char *fmt, ...)
{
    va_list ap;
    addr_t pc;
    int i;

    fprintf(stderr, "Runtime error: ");
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n");

    for(i=0;i<rt_num_callers;i++) {
        if (rt_get_caller_pc(&pc, uc, i) < 0)
            break;
        pc = rt_printline(pc, i ? "by" : "at");
        if (pc == (addr_t)rt_prog_main && pc)
            break;
    }
}

/* ------------------------------------------------------------- */
#ifndef _WIN32

/* signal handler for fatal errors */
static void sig_error(int signum, siginfo_t *siginf, void *puc)
{
    ucontext_t *uc = puc;

    switch(signum) {
    case SIGFPE:
        switch(siginf->si_code) {
        case FPE_INTDIV:
        case FPE_FLTDIV:
            rt_error(uc, "division by zero");
            break;
        default:
            rt_error(uc, "floating point exception");
            break;
        }
        break;
    case SIGBUS:
    case SIGSEGV:
        if (rt_bound_error_msg && *rt_bound_error_msg)
            rt_error(uc, *rt_bound_error_msg);
        else
            rt_error(uc, "dereferencing invalid pointer");
        break;
    case SIGILL:
        rt_error(uc, "illegal instruction");
        break;
    case SIGABRT:
        rt_error(uc, "abort() called");
        break;
    default:
        rt_error(uc, "caught signal %d", signum);
        break;
    }
    exit(255);
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
    sigact.sa_sigaction = sig_error;
    sigemptyset(&sigact.sa_mask);
    sigaction(SIGFPE, &sigact, NULL);
    sigaction(SIGILL, &sigact, NULL);
    sigaction(SIGSEGV, &sigact, NULL);
    sigaction(SIGBUS, &sigact, NULL);
    sigaction(SIGABRT, &sigact, NULL);
}

/* ------------------------------------------------------------- */
#ifdef __i386__

/* fix for glibc 2.1 */
#ifndef REG_EIP
#define REG_EIP EIP
#define REG_EBP EBP
#endif

/* return the PC at frame level 'level'. Return negative if not found */
static int rt_get_caller_pc(addr_t *paddr, ucontext_t *uc, int level)
{
    addr_t fp;
    int i;

    if (level == 0) {
#if defined(__APPLE__)
        *paddr = uc->uc_mcontext->__ss.__eip;
#elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__) || defined(__DragonFly__)
        *paddr = uc->uc_mcontext.mc_eip;
#elif defined(__dietlibc__)
        *paddr = uc->uc_mcontext.eip;
#elif defined(__NetBSD__)
        *paddr = uc->uc_mcontext.__gregs[_REG_EIP];
#elif defined(__OpenBSD__)
        *paddr = uc->sc_eip;
#else
        *paddr = uc->uc_mcontext.gregs[REG_EIP];
#endif
        return 0;
    } else {
#if defined(__APPLE__)
        fp = uc->uc_mcontext->__ss.__ebp;
#elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__) || defined(__DragonFly__)
        fp = uc->uc_mcontext.mc_ebp;
#elif defined(__dietlibc__)
        fp = uc->uc_mcontext.ebp;
#elif defined(__NetBSD__)
        fp = uc->uc_mcontext.__gregs[_REG_EBP];
#elif defined(__OpenBSD__)
        *paddr = uc->sc_ebp;
#else
        fp = uc->uc_mcontext.gregs[REG_EBP];
#endif
        for(i=1;i<level;i++) {
            /* XXX: check address validity with program info */
            if (fp <= 0x1000 || fp >= 0xc0000000)
                return -1;
            fp = ((addr_t *)fp)[0];
        }
        *paddr = ((addr_t *)fp)[1];
        return 0;
    }
}

/* ------------------------------------------------------------- */
#elif defined(__x86_64__)

/* return the PC at frame level 'level'. Return negative if not found */
static int rt_get_caller_pc(addr_t *paddr, ucontext_t *uc, int level)
{
    addr_t fp;
    int i;

    if (level == 0) {
        /* XXX: only support linux */
#if defined(__APPLE__)
        *paddr = uc->uc_mcontext->__ss.__rip;
#elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__) || defined(__DragonFly__)
        *paddr = uc->uc_mcontext.mc_rip;
#elif defined(__NetBSD__)
        *paddr = uc->uc_mcontext.__gregs[_REG_RIP];
#else
        *paddr = uc->uc_mcontext.gregs[REG_RIP];
#endif
        return 0;
    } else {
#if defined(__APPLE__)
        fp = uc->uc_mcontext->__ss.__rbp;
#elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__) || defined(__DragonFly__)
        fp = uc->uc_mcontext.mc_rbp;
#elif defined(__NetBSD__)
        fp = uc->uc_mcontext.__gregs[_REG_RBP];
#else
        fp = uc->uc_mcontext.gregs[REG_RBP];
#endif
        for(i=1;i<level;i++) {
            /* XXX: check address validity with program info */
            if (fp <= 0x1000)
                return -1;
            fp = ((addr_t *)fp)[0];
        }
        *paddr = ((addr_t *)fp)[1];
        return 0;
    }
}

/* ------------------------------------------------------------- */
#elif defined(__arm__)

/* return the PC at frame level 'level'. Return negative if not found */
static int rt_get_caller_pc(addr_t *paddr, ucontext_t *uc, int level)
{
    addr_t fp, sp;
    int i;

    if (level == 0) {
        /* XXX: only supports linux */
#if defined(__linux__)
        *paddr = uc->uc_mcontext.arm_pc;
#else
        return -1;
#endif
        return 0;
    } else {
#if defined(__linux__)
        fp = uc->uc_mcontext.arm_fp;
        sp = uc->uc_mcontext.arm_sp;
        if (sp < 0x1000)
            sp = 0x1000;
#else
        return -1;
#endif
        /* XXX: specific to tinycc stack frames */
        if (fp < sp + 12 || fp & 3)
            return -1;
        for(i = 1; i < level; i++) {
            sp = ((addr_t *)fp)[-2];
            if (sp < fp || sp - fp > 16 || sp & 3)
                return -1;
            fp = ((addr_t *)fp)[-3];
            if (fp <= sp || fp - sp < 12 || fp & 3)
                return -1;
        }
        /* XXX: check address validity with program info */
        *paddr = ((addr_t *)fp)[-1];
        return 0;
    }
}

/* ------------------------------------------------------------- */
#elif defined(__aarch64__)

static int rt_get_caller_pc(addr_t *paddr, ucontext_t *uc, int level)
{
    if (level < 0)
        return -1;
    else if (level == 0) {
        *paddr = uc->uc_mcontext.pc;
        return 0;
    }
    else {
        addr_t *fp = (addr_t *)uc->uc_mcontext.regs[29];
        int i;
        for (i = 1; i < level; i++)
            fp = (addr_t *)fp[0];
        *paddr = fp[1];
        return 0;
    }
}

/* ------------------------------------------------------------- */
#else

#warning add arch specific rt_get_caller_pc()
static int rt_get_caller_pc(addr_t *paddr, ucontext_t *uc, int level)
{
    return -1;
}

#endif /* !__i386__ */

/* ------------------------------------------------------------- */
#else /* WIN32 */

static long __stdcall cpu_exception_handler(EXCEPTION_POINTERS *ex_info)
{
    EXCEPTION_RECORD *er = ex_info->ExceptionRecord;
    CONTEXT *uc = ex_info->ContextRecord;
    switch (er->ExceptionCode) {
    case EXCEPTION_ACCESS_VIOLATION:
        if (rt_bound_error_msg && *rt_bound_error_msg)
            rt_error(uc, *rt_bound_error_msg);
        else
	    rt_error(uc, "access violation");
        break;
    case EXCEPTION_STACK_OVERFLOW:
        rt_error(uc, "stack overflow");
        break;
    case EXCEPTION_INT_DIVIDE_BY_ZERO:
        rt_error(uc, "division by zero");
        break;
    default:
        rt_error(uc, "exception caught");
        break;
    }
    return EXCEPTION_EXECUTE_HANDLER;
}

/* Generate a stack backtrace when a CPU exception occurs. */
static void set_exception_handler(void)
{
    SetUnhandledExceptionFilter(cpu_exception_handler);
}

/* return the PC at frame level 'level'. Return non zero if not found */
static int rt_get_caller_pc(addr_t *paddr, CONTEXT *uc, int level)
{
    addr_t fp, pc;
    int i;
#ifdef _WIN64
    pc = uc->Rip;
    fp = uc->Rbp;
#else
    pc = uc->Eip;
    fp = uc->Ebp;
#endif
    if (level > 0) {
        for(i=1;i<level;i++) {
	    /* XXX: check address validity with program info */
	    if (fp <= 0x1000 || fp >= 0xc0000000)
		return -1;
	    fp = ((addr_t*)fp)[0];
	}
        pc = ((addr_t*)fp)[1];
    }
    *paddr = pc;
    return 0;
}

#endif /* _WIN32 */
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
