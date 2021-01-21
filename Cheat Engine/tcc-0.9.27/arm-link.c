#ifdef TARGET_DEFS_ONLY

#define EM_TCC_TARGET EM_ARM

/* relocation type for 32 bit data relocation */
#define R_DATA_32   R_ARM_ABS32
#define R_DATA_PTR  R_ARM_ABS32
#define R_JMP_SLOT  R_ARM_JUMP_SLOT
#define R_GLOB_DAT  R_ARM_GLOB_DAT
#define R_COPY      R_ARM_COPY
#define R_RELATIVE  R_ARM_RELATIVE

#define R_NUM       R_ARM_NUM

#define ELF_START_ADDR 0x00008000
#define ELF_PAGE_SIZE  0x1000

#define PCRELATIVE_DLLPLT 1
#define RELOCATE_DLLPLT 0

enum float_abi {
    ARM_SOFTFP_FLOAT,
    ARM_HARD_FLOAT,
};

#else /* !TARGET_DEFS_ONLY */

#include "tcc.h"

/* Returns 1 for a code relocation, 0 for a data relocation. For unknown
   relocations, returns -1. */
int code_reloc (int reloc_type)
{
    switch (reloc_type) {
	case R_ARM_MOVT_ABS:
	case R_ARM_MOVW_ABS_NC:
	case R_ARM_THM_MOVT_ABS:
	case R_ARM_THM_MOVW_ABS_NC:
	case R_ARM_ABS32:
	case R_ARM_REL32:
	case R_ARM_GOTPC:
	case R_ARM_GOTOFF:
	case R_ARM_GOT32:
	case R_ARM_COPY:
	case R_ARM_GLOB_DAT:
	case R_ARM_NONE:
            return 0;

        case R_ARM_PC24:
        case R_ARM_CALL:
	case R_ARM_JUMP24:
	case R_ARM_PLT32:
	case R_ARM_THM_PC22:
	case R_ARM_THM_JUMP24:
	case R_ARM_PREL31:
	case R_ARM_V4BX:
	case R_ARM_JUMP_SLOT:
            return 1;
    }

    tcc_error ("Unknown relocation type: %d", reloc_type);
    return -1;
}

/* Returns an enumerator to describe whether and when the relocation needs a
   GOT and/or PLT entry to be created. See tcc.h for a description of the
   different values. */
int gotplt_entry_type (int reloc_type)
{
    switch (reloc_type) {
	case R_ARM_NONE:
	case R_ARM_COPY:
	case R_ARM_GLOB_DAT:
	case R_ARM_JUMP_SLOT:
            return NO_GOTPLT_ENTRY;

        case R_ARM_PC24:
        case R_ARM_CALL:
	case R_ARM_JUMP24:
	case R_ARM_PLT32:
	case R_ARM_THM_PC22:
	case R_ARM_THM_JUMP24:
	case R_ARM_MOVT_ABS:
	case R_ARM_MOVW_ABS_NC:
	case R_ARM_THM_MOVT_ABS:
	case R_ARM_THM_MOVW_ABS_NC:
	case R_ARM_PREL31:
	case R_ARM_ABS32:
	case R_ARM_REL32:
	case R_ARM_V4BX:
            return AUTO_GOTPLT_ENTRY;

	case R_ARM_GOTPC:
	case R_ARM_GOTOFF:
            return BUILD_GOT_ONLY;

	case R_ARM_GOT32:
            return ALWAYS_GOTPLT_ENTRY;
    }

    tcc_error ("Unknown relocation type: %d", reloc_type);
    return -1;
}

ST_FUNC unsigned create_plt_entry(TCCState *s1, unsigned got_offset, struct sym_attr *attr)
{
    Section *plt = s1->plt;
    uint8_t *p;
    unsigned plt_offset;

    /* when building a DLL, GOT entry accesses must be done relative to
       start of GOT (see x86_64 example above)  */
    if (s1->output_type == TCC_OUTPUT_DLL)
        tcc_error("DLLs unimplemented!");

    /* empty PLT: create PLT0 entry that push address of call site and
       jump to ld.so resolution routine (GOT + 8) */
    if (plt->data_offset == 0) {
        p = section_ptr_add(plt, 20);
        write32le(p,    0xe52de004); /* push {lr}         */
        write32le(p+4,  0xe59fe004); /* ldr lr, [pc, #4] */
        write32le(p+8,  0xe08fe00e); /* add lr, pc, lr    */
        write32le(p+12, 0xe5bef008); /* ldr pc, [lr, #8]! */
        /* p+16 is set in relocate_plt */
    }
    plt_offset = plt->data_offset;

    if (attr->plt_thumb_stub) {
        p = section_ptr_add(plt, 4);
        write32le(p,   0x4778); /* bx pc */
        write32le(p+2, 0x46c0); /* nop   */
    }
    p = section_ptr_add(plt, 16);
    /* Jump to GOT entry where ld.so initially put address of PLT0 */
    write32le(p,   0xe59fc004); /* ldr ip, [pc, #4] */
    write32le(p+4, 0xe08fc00c); /* add ip, pc, ip */
    write32le(p+8, 0xe59cf000); /* ldr pc, [ip] */
    /* p + 12 contains offset to GOT entry once patched by relocate_plt */
    write32le(p+12, got_offset);
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
        int x = s1->got->sh_addr - s1->plt->sh_addr - 12;
        write32le(s1->plt->data + 16, x - 16);
        p += 20;
        while (p < p_end) {
            if (read32le(p) == 0x46c04778) /* PLT Thumb stub present */
                p += 4;
            add32le(p + 12, x + s1->plt->data - p);
            p += 16;
        }
    }
}

void relocate_init(Section *sr) {}

void relocate(TCCState *s1, ElfW_Rel *rel, int type, unsigned char *ptr, addr_t addr, addr_t val)
{
    ElfW(Sym) *sym;
    int sym_index;

    sym_index = ELFW(R_SYM)(rel->r_info);
    sym = &((ElfW(Sym) *)symtab_section->data)[sym_index];

    switch(type) {
        case R_ARM_PC24:
        case R_ARM_CALL:
        case R_ARM_JUMP24:
        case R_ARM_PLT32:
            {
                int x, is_thumb, is_call, h, blx_avail, is_bl, th_ko;
                x = (*(int *) ptr) & 0xffffff;
#ifdef DEBUG_RELOC
		printf ("reloc %d: x=0x%x val=0x%x ", type, x, val);
#endif
                (*(int *)ptr) &= 0xff000000;
                if (x & 0x800000)
                    x -= 0x1000000;
                x <<= 2;
                blx_avail = (TCC_CPU_VERSION >= 5);
                is_thumb = val & 1;
                is_bl = (*(unsigned *) ptr) >> 24 == 0xeb;
                is_call = (type == R_ARM_CALL || (type == R_ARM_PC24 && is_bl));
                x += val - addr;
#ifdef DEBUG_RELOC
		printf (" newx=0x%x name=%s\n", x,
			(char *) symtab_section->link->data + sym->st_name);
#endif
                h = x & 2;
                th_ko = (x & 3) && (!blx_avail || !is_call);
                if (th_ko || x >= 0x2000000 || x < -0x2000000)
                    tcc_error("can't relocate value at %x,%d",addr, type);
                x >>= 2;
                x &= 0xffffff;
                /* Only reached if blx is avail and it is a call */
                if (is_thumb) {
                    x |= h << 24;
                    (*(int *)ptr) = 0xfa << 24; /* bl -> blx */
                }
                (*(int *) ptr) |= x;
            }
            return;
        /* Since these relocations only concern Thumb-2 and blx instruction was
           introduced before Thumb-2, we can assume blx is available and not
           guard its use */
        case R_ARM_THM_PC22:
        case R_ARM_THM_JUMP24:
            {
                int x, hi, lo, s, j1, j2, i1, i2, imm10, imm11;
                int to_thumb, is_call, to_plt, blx_bit = 1 << 12;
                Section *plt;

                /* weak reference */
                if (sym->st_shndx == SHN_UNDEF &&
                    ELFW(ST_BIND)(sym->st_info) == STB_WEAK)
                    return;

                /* Get initial offset */
                hi = (*(uint16_t *)ptr);
                lo = (*(uint16_t *)(ptr+2));
                s = (hi >> 10) & 1;
                j1 = (lo >> 13) & 1;
                j2 = (lo >> 11) & 1;
                i1 = (j1 ^ s) ^ 1;
                i2 = (j2 ^ s) ^ 1;
                imm10 = hi & 0x3ff;
                imm11 = lo & 0x7ff;
                x = (s << 24) | (i1 << 23) | (i2 << 22) |
                    (imm10 << 12) | (imm11 << 1);
                if (x & 0x01000000)
                    x -= 0x02000000;

                /* Relocation infos */
                to_thumb = val & 1;
                plt = s1->plt;
                to_plt = (val >= plt->sh_addr) &&
                         (val < plt->sh_addr + plt->data_offset);
                is_call = (type == R_ARM_THM_PC22);

                if (!to_thumb && !to_plt && !is_call) {
                    int index;
                    uint8_t *p;
                    char *name, buf[1024];
                    Section *text_section;

                    name = (char *) symtab_section->link->data + sym->st_name;
                    text_section = s1->sections[sym->st_shndx];
                    /* Modify reloc to target a thumb stub to switch to ARM */
                    snprintf(buf, sizeof(buf), "%s_from_thumb", name);
                    index = put_elf_sym(symtab_section,
                                        text_section->data_offset + 1,
                                        sym->st_size, sym->st_info, 0,
                                        sym->st_shndx, buf);
                    to_thumb = 1;
                    val = text_section->data_offset + 1;
                    rel->r_info = ELFW(R_INFO)(index, type);
                    /* Create a thumb stub function to switch to ARM mode */
                    put_elf_reloc(symtab_section, text_section,
                                  text_section->data_offset + 4, R_ARM_JUMP24,
                                  sym_index);
                    p = section_ptr_add(text_section, 8);
                    write32le(p,   0x4778); /* bx pc */
                    write32le(p+2, 0x46c0); /* nop   */
                    write32le(p+4, 0xeafffffe); /* b $sym */
                }

                /* Compute final offset */
                x += val - addr;
                if (!to_thumb && is_call) {
                    blx_bit = 0; /* bl -> blx */
                    x = (x + 3) & -4; /* Compute offset from aligned PC */
                }

                /* Check that relocation is possible
                   * offset must not be out of range
                   * if target is to be entered in arm mode:
                     - bit 1 must not set
                     - instruction must be a call (bl) or a jump to PLT */
                if (!to_thumb || x >= 0x1000000 || x < -0x1000000)
                    if (to_thumb || (val & 2) || (!is_call && !to_plt))
                        tcc_error("can't relocate value at %x,%d",addr, type);

                /* Compute and store final offset */
                s = (x >> 24) & 1;
                i1 = (x >> 23) & 1;
                i2 = (x >> 22) & 1;
                j1 = s ^ (i1 ^ 1);
                j2 = s ^ (i2 ^ 1);
                imm10 = (x >> 12) & 0x3ff;
                imm11 = (x >> 1) & 0x7ff;
                (*(uint16_t *)ptr) = (uint16_t) ((hi & 0xf800) |
                                     (s << 10) | imm10);
                (*(uint16_t *)(ptr+2)) = (uint16_t) ((lo & 0xc000) |
                                (j1 << 13) | blx_bit | (j2 << 11) |
                                imm11);
            }
            return;
        case R_ARM_MOVT_ABS:
        case R_ARM_MOVW_ABS_NC:
            {
                int x, imm4, imm12;
                if (type == R_ARM_MOVT_ABS)
                    val >>= 16;
                imm12 = val & 0xfff;
                imm4 = (val >> 12) & 0xf;
                x = (imm4 << 16) | imm12;
                if (type == R_ARM_THM_MOVT_ABS)
                    *(int *)ptr |= x;
                else
                    *(int *)ptr += x;
            }
            return;
        case R_ARM_THM_MOVT_ABS:
        case R_ARM_THM_MOVW_ABS_NC:
            {
                int x, i, imm4, imm3, imm8;
                if (type == R_ARM_THM_MOVT_ABS)
                    val >>= 16;
                imm8 = val & 0xff;
                imm3 = (val >> 8) & 0x7;
                i = (val >> 11) & 1;
                imm4 = (val >> 12) & 0xf;
                x = (imm3 << 28) | (imm8 << 16) | (i << 10) | imm4;
                if (type == R_ARM_THM_MOVT_ABS)
                    *(int *)ptr |= x;
                else
                    *(int *)ptr += x;
            }
            return;
        case R_ARM_PREL31:
            {
                int x;
                x = (*(int *)ptr) & 0x7fffffff;
                (*(int *)ptr) &= 0x80000000;
                x = (x * 2) / 2;
                x += val - addr;
                if((x^(x>>1))&0x40000000)
                    tcc_error("can't relocate value at %x,%d",addr, type);
                (*(int *)ptr) |= x & 0x7fffffff;
            }
        case R_ARM_ABS32:
            *(int *)ptr += val;
            return;
        case R_ARM_REL32:
            *(int *)ptr += val - addr;
            return;
        case R_ARM_GOTPC:
            *(int *)ptr += s1->got->sh_addr - addr;
            return;
        case R_ARM_GOTOFF:
            *(int *)ptr += val - s1->got->sh_addr;
            return;
        case R_ARM_GOT32:
            /* we load the got offset */
            *(int *)ptr += s1->sym_attrs[sym_index].got_offset;
            return;
        case R_ARM_COPY:
            return;
        case R_ARM_V4BX:
            /* trade Thumb support for ARMv4 support */
            if ((0x0ffffff0 & *(int*)ptr) == 0x012FFF10)
                *(int*)ptr ^= 0xE12FFF10 ^ 0xE1A0F000; /* BX Rm -> MOV PC, Rm */
            return;
        case R_ARM_GLOB_DAT:
        case R_ARM_JUMP_SLOT:
            *(addr_t *)ptr = val;
            return;
        case R_ARM_NONE:
            /* Nothing to do.  Normally used to indicate a dependency
               on a certain symbol (like for exception handling under EABI).  */
            return;
        case R_ARM_RELATIVE:
#ifdef TCC_TARGET_PE
            add32le(ptr, val - s1->pe_imagebase);
#endif
            /* do nothing */
            return;
        default:
            fprintf(stderr,"FIXME: handle reloc type %x at %x [%p] to %x\n",
                type, (unsigned)addr, ptr, (unsigned)val);
            return;
    }
}

#endif /* !TARGET_DEFS_ONLY */
