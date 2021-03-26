#ifdef TARGET_DEFS_ONLY

#define EM_TCC_TARGET EM_RISCV

#define R_DATA_32  R_RISCV_32
#define R_DATA_PTR R_RISCV_64
#define R_JMP_SLOT R_RISCV_JUMP_SLOT
#define R_GLOB_DAT R_RISCV_64
#define R_COPY     R_RISCV_COPY
#define R_RELATIVE R_RISCV_RELATIVE

#define R_NUM      R_RISCV_NUM

#define ELF_START_ADDR 0x00010000
#define ELF_PAGE_SIZE 0x1000

#define PCRELATIVE_DLLPLT 1
#define RELOCATE_DLLPLT 1

#else /* !TARGET_DEFS_ONLY */

//#define DEBUG_RELOC
#include "tcc.h"

/* Returns 1 for a code relocation, 0 for a data relocation. For unknown
   relocations, returns -1. */
int code_reloc (int reloc_type)
{
    switch (reloc_type) {

    case R_RISCV_BRANCH:
    case R_RISCV_CALL:
    case R_RISCV_JAL:
        return 1;

    case R_RISCV_GOT_HI20:
    case R_RISCV_PCREL_HI20:
    case R_RISCV_PCREL_LO12_I:
    case R_RISCV_PCREL_LO12_S:
    case R_RISCV_32_PCREL:
    case R_RISCV_SET6:
    case R_RISCV_SUB6:
    case R_RISCV_ADD16:
    case R_RISCV_ADD32:
    case R_RISCV_ADD64:
    case R_RISCV_SUB16:
    case R_RISCV_SUB32:
    case R_RISCV_SUB64:
    case R_RISCV_32:
    case R_RISCV_64:
        return 0;

    case R_RISCV_CALL_PLT:
        return 1;
    }
    return -1;
}

/* Returns an enumerator to describe whether and when the relocation needs a
   GOT and/or PLT entry to be created. See tcc.h for a description of the
   different values. */
int gotplt_entry_type (int reloc_type)
{
    switch (reloc_type) {
    case R_RISCV_ALIGN:
    case R_RISCV_RELAX:
    case R_RISCV_RVC_BRANCH:
    case R_RISCV_RVC_JUMP:
    case R_RISCV_JUMP_SLOT:
    case R_RISCV_SET6:
    case R_RISCV_SUB6:
    case R_RISCV_ADD16:
    case R_RISCV_SUB16:
        return NO_GOTPLT_ENTRY;

    case R_RISCV_BRANCH:
    case R_RISCV_CALL:
    case R_RISCV_PCREL_HI20:
    case R_RISCV_PCREL_LO12_I:
    case R_RISCV_PCREL_LO12_S:
    case R_RISCV_32_PCREL:
    case R_RISCV_ADD32:
    case R_RISCV_ADD64:
    case R_RISCV_SUB32:
    case R_RISCV_SUB64:
    case R_RISCV_32:
    case R_RISCV_64:
    case R_RISCV_JAL:
    case R_RISCV_CALL_PLT:
        return AUTO_GOTPLT_ENTRY;

    case R_RISCV_GOT_HI20:
        return ALWAYS_GOTPLT_ENTRY;
    }
    return -1;
}

ST_FUNC unsigned create_plt_entry(TCCState *s1, unsigned got_offset, struct sym_attr *attr)
{
    Section *plt = s1->plt;
    uint8_t *p;
    unsigned plt_offset;

    if (plt->data_offset == 0)
        section_ptr_add(plt, 32);
    plt_offset = plt->data_offset;

    p = section_ptr_add(plt, 16);
    write64le(p, got_offset);
    return plt_offset;
}

/* relocate the PLT: compute addresses and offsets in the PLT now that final
   address for PLT and GOT are known (see fill_program_header) */
ST_FUNC void relocate_plt(TCCState *s1)
{
    uint8_t *p, *p_end;

    if (!s1->plt)
      return;

    p = s1->plt->data;
    p_end = p + s1->plt->data_offset;

    if (p < p_end) {
        uint64_t plt = s1->plt->sh_addr;
        uint64_t got = s1->got->sh_addr;
        uint64_t off = (got - plt + 0x800) >> 12;
        if ((off + ((uint32_t)1 << 20)) >> 21)
            tcc_error("Failed relocating PLT (off=0x%lx, got=0x%lx, plt=0x%lx)", (long)off, (long)got, (long)plt);
        write32le(p, 0x397 | (off << 12)); // auipc t2, %pcrel_hi(got)
        write32le(p + 4, 0x41c30333); // sub t1, t1, t3
        write32le(p + 8, 0x0003be03   // ld t3, %pcrel_lo(got)(t2)
                         | (((got - plt) & 0xfff) << 20));
        write32le(p + 12, 0xfd430313); // addi t1, t1, -(32+12)
        write32le(p + 16, 0x00038293   // addi t0, t2, %pcrel_lo(got)
                          | (((got - plt) & 0xfff) << 20));
        write32le(p + 20, 0x00135313); // srli t1, t1, log2(16/PTRSIZE)
        write32le(p + 24, 0x0082b283); // ld t0, PTRSIZE(t0)
        write32le(p + 28, 0x000e0067); // jr t3
        p += 32;
        while (p < p_end) {
            uint64_t pc = plt + (p - s1->plt->data);
            uint64_t addr = got + read64le(p);
            uint64_t off = (addr - pc + 0x800) >> 12;
            if ((off + ((uint32_t)1 << 20)) >> 21)
                tcc_error("Failed relocating PLT (off=0x%lx, addr=0x%lx, pc=0x%lx)", (long)off, (long)addr, (long)pc);
            write32le(p, 0xe17 | (off << 12)); // auipc t3, %pcrel_hi(func@got)
            write32le(p + 4, 0x000e3e03 // ld t3, %pcrel_lo(func@got)(t3)
                             | (((addr - pc) & 0xfff) << 20));
            write32le(p + 8, 0x000e0367); // jalr t1, t3
            write32le(p + 12, 0x00000013); // nop
            p += 16;
        }
    }

    if (s1->got->relocplt) {
	int mem = s1->output_type == TCC_OUTPUT_MEMORY;
        ElfW_Rel *rel;

        p = s1->got->data;
        for_each_elem(s1->got->relocplt, 0, rel, ElfW_Rel) {
	    int sym_index = ELFW(R_SYM)(rel->r_info);
	    ElfW(Sym) *sym = &((ElfW(Sym) *)symtab_section->data)[sym_index];
            write64le(p + rel->r_offset, mem ? sym->st_value + rel->r_addend : s1->plt->sh_addr);
	}
    }
}

void relocate(TCCState *s1, ElfW_Rel *rel, int type, unsigned char *ptr,
              addr_t addr, addr_t val)
{
    uint64_t off64;
    uint32_t off32;
    int sym_index = ELFW(R_SYM)(rel->r_info), esym_index;
    ElfW(Sym) *sym = &((ElfW(Sym) *)symtab_section->data)[sym_index];

    switch(type) {
    case R_RISCV_ALIGN:
    case R_RISCV_RELAX:
        return;

    case R_RISCV_BRANCH:
        off64 = val - addr;
        if ((off64 + (1 << 12)) & ~(uint64_t)0x1ffe)
          tcc_error("R_RISCV_BRANCH relocation failed"
                    " (val=%lx, addr=%lx)", (long)val, (long)addr);
        off32 = off64 >> 1;
        write32le(ptr, (read32le(ptr) & ~0xfe000f80)
                       | ((off32 & 0x800) << 20)
                       | ((off32 & 0x3f0) << 21)
                       | ((off32 & 0x00f) << 8)
                       | ((off32 & 0x400) >> 3));
        return;
    case R_RISCV_JAL:
        off64 = val - addr;
        if ((off64 + (1 << 21)) & ~(((uint64_t)1 << 22) - 2))
          tcc_error("R_RISCV_JAL relocation failed"
                    " (val=%lx, addr=%lx)", (long)val, (long)addr);
        off32 = off64;
        write32le(ptr, (read32le(ptr) & 0xfff)
                       | (((off32 >> 12) &  0xff) << 12)
                       | (((off32 >> 11) &     1) << 20)
                       | (((off32 >>  1) & 0x3ff) << 21)
                       | (((off32 >> 20) &     1) << 31));
        return;
    case R_RISCV_CALL:
    case R_RISCV_CALL_PLT:
        write32le(ptr, (read32le(ptr) & 0xfff)
                       | ((val - addr + 0x800) & ~0xfff));
        write32le(ptr + 4, (read32le(ptr + 4) & 0xfffff)
                           | (((val - addr) & 0xfff) << 20));
        return;
    case R_RISCV_PCREL_HI20:
#ifdef DEBUG_RELOC
        printf("PCREL_HI20: val=%lx addr=%lx\n", (long)val, (long)addr);
#endif
        off64 = (int64_t)(val - addr + 0x800) >> 12;
        if ((off64 + ((uint64_t)1 << 20)) >> 21)
          tcc_error("R_RISCV_PCREL_HI20 relocation failed: off=%lx cond=%lx sym=%s",
                    (long)off64, (long)((int64_t)(off64 + ((uint64_t)1 << 20)) >> 21),
                    symtab_section->link->data + sym->st_name);
        write32le(ptr, (read32le(ptr) & 0xfff)
                       | ((off64 & 0xfffff) << 12));
        last_hi.addr = addr;
        last_hi.val = val;
        return;
    case R_RISCV_GOT_HI20:
        val = s1->got->sh_addr + get_sym_attr(s1, sym_index, 0)->got_offset;
        off64 = (int64_t)(val - addr + 0x800) >> 12;
        if ((off64 + ((uint64_t)1 << 20)) >> 21)
          tcc_error("R_RISCV_GOT_HI20 relocation failed");
        last_hi.addr = addr;
        last_hi.val = val;
        write32le(ptr, (read32le(ptr) & 0xfff)
                       | ((off64 & 0xfffff) << 12));
        return;
    case R_RISCV_PCREL_LO12_I:
#ifdef DEBUG_RELOC
        printf("PCREL_LO12_I: val=%lx addr=%lx\n", (long)val, (long)addr);
#endif
        if (val != last_hi.addr)
          tcc_error("unsupported hi/lo pcrel reloc scheme");
        val = last_hi.val;
        addr = last_hi.addr;
        write32le(ptr, (read32le(ptr) & 0xfffff)
                       | (((val - addr) & 0xfff) << 20));
        return;
    case R_RISCV_PCREL_LO12_S:
        if (val != last_hi.addr)
          tcc_error("unsupported hi/lo pcrel reloc scheme");
        val = last_hi.val;
        addr = last_hi.addr;
        off32 = val - addr;
        write32le(ptr, (read32le(ptr) & ~0xfe000f80)
                       | ((off32 & 0xfe0) << 20)
                       | ((off32 & 0x01f) << 7));
        return;

    case R_RISCV_RVC_BRANCH:
        off64 = (val - addr);
        if ((off64 + (1 << 8)) & ~(uint64_t)0x1fe)
          tcc_error("R_RISCV_RVC_BRANCH relocation failed"
                    " (val=%lx, addr=%lx)", (long)val, (long)addr);
        off32 = off64;
        write16le(ptr, (read16le(ptr) & 0xe383)
                       | (((off32 >> 5) & 1) << 2)
                       | (((off32 >> 1) & 3) << 3)
                       | (((off32 >> 6) & 3) << 5)
                       | (((off32 >> 3) & 3) << 10)
                       | (((off32 >> 8) & 1) << 12));
        return;
    case R_RISCV_RVC_JUMP:
        off64 = (val - addr);
        if ((off64 + (1 << 11)) & ~(uint64_t)0xffe)
          tcc_error("R_RISCV_RVC_BRANCH relocation failed"
                    " (val=%lx, addr=%lx)", (long)val, (long)addr);
        off32 = off64;
        write16le(ptr, (read16le(ptr) & 0xe003)
                       | (((off32 >>  5) & 1) << 2)
                       | (((off32 >>  1) & 7) << 3)
                       | (((off32 >>  7) & 1) << 6)
                       | (((off32 >>  6) & 1) << 7)
                       | (((off32 >> 10) & 1) << 8)
                       | (((off32 >>  8) & 3) << 9)
                       | (((off32 >>  4) & 1) << 11)
                       | (((off32 >> 11) & 1) << 12));
        return;

    case R_RISCV_32:
        if (s1->output_type == TCC_OUTPUT_DLL) {
            /* XXX: this logic may depend on TCC's codegen
               now TCC uses R_RISCV_RELATIVE even for a 64bit pointer */
            qrel->r_offset = rel->r_offset;
            qrel->r_info = ELFW(R_INFO)(0, R_RISCV_RELATIVE);
            /* Use sign extension! */
            qrel->r_addend = (int)read32le(ptr) + val;
            qrel++;
        }
        add32le(ptr, val);
        return;
    case R_RISCV_64:
        if (s1->output_type == TCC_OUTPUT_DLL) {
            esym_index = get_sym_attr(s1, sym_index, 0)->dyn_index;
            qrel->r_offset = rel->r_offset;
            if (esym_index) {
                qrel->r_info = ELFW(R_INFO)(esym_index, R_RISCV_64);
                qrel->r_addend = rel->r_addend;
                qrel++;
                break;
            } else {
                qrel->r_info = ELFW(R_INFO)(0, R_RISCV_RELATIVE);
                qrel->r_addend = read64le(ptr) + val;
                qrel++;
            }
        }
    case R_RISCV_JUMP_SLOT:
        add64le(ptr, val);
        return;
    case R_RISCV_ADD64:
        write64le(ptr, read64le(ptr) + val);
        return;
    case R_RISCV_ADD32:
        write32le(ptr, read32le(ptr) + val);
        return;
    case R_RISCV_SUB64:
        write64le(ptr, read64le(ptr) - val);
        return;
    case R_RISCV_SUB32:
        write32le(ptr, read32le(ptr) - val);
        return;
    case R_RISCV_ADD16:
        write16le(ptr, read16le(ptr) + val);
        return;
    case R_RISCV_SUB16:
        write16le(ptr, read16le(ptr) - val);
        return;
    case R_RISCV_SET6:
        *ptr = (*ptr & ~0x3f) | (val & 0x3f);
        return;
    case R_RISCV_SUB6:
        *ptr = (*ptr & ~0x3f) | ((*ptr - val) & 0x3f);
        return;

    case R_RISCV_32_PCREL:
    case R_RISCV_COPY:
        /* XXX */
        return;

    default:
        fprintf(stderr, "FIXME: handle reloc type %x at %x [%p] to %x\n",
                type, (unsigned)addr, ptr, (unsigned)val);
        return;
    }
}
#endif
