#ifdef TARGET_DEFS_ONLY

#define EM_TCC_TARGET EM_386

/* relocation type for 32 bit data relocation */
#define R_DATA_32   R_386_32
#define R_DATA_PTR  R_386_32
#define R_JMP_SLOT  R_386_JMP_SLOT
#define R_GLOB_DAT  R_386_GLOB_DAT
#define R_COPY      R_386_COPY
#define R_RELATIVE  R_386_RELATIVE

#define R_NUM       R_386_NUM

#define ELF_START_ADDR 0x08048000
#define ELF_PAGE_SIZE  0x1000

#define PCRELATIVE_DLLPLT 0
#define RELOCATE_DLLPLT 0

#else /* !TARGET_DEFS_ONLY */

#include "tcc.h"

/* Returns 1 for a code relocation, 0 for a data relocation. For unknown
   relocations, returns -1. */
int code_reloc (int reloc_type)
{
    switch (reloc_type) {
	case R_386_RELATIVE:
	case R_386_16:
        case R_386_32:
	case R_386_GOTPC:
	case R_386_GOTOFF:
	case R_386_GOT32:
	case R_386_GOT32X:
	case R_386_GLOB_DAT:
	case R_386_COPY:
            return 0;

	case R_386_PC16:
	case R_386_PC32:
	case R_386_PLT32:
	case R_386_JMP_SLOT:
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
	case R_386_RELATIVE:
	case R_386_16:
	case R_386_GLOB_DAT:
	case R_386_JMP_SLOT:
	case R_386_COPY:
            return NO_GOTPLT_ENTRY;

        case R_386_32:
	    /* This relocations shouldn't normally need GOT or PLT
	       slots if it weren't for simplicity in the code generator.
	       See our caller for comments.  */
            return AUTO_GOTPLT_ENTRY;

	case R_386_PC16:
	case R_386_PC32:
            return AUTO_GOTPLT_ENTRY;

	case R_386_GOTPC:
	case R_386_GOTOFF:
            return BUILD_GOT_ONLY;

	case R_386_GOT32:
	case R_386_GOT32X:
	case R_386_PLT32:
            return ALWAYS_GOTPLT_ENTRY;
    }

    tcc_error ("Unknown relocation type: %d", reloc_type);
    return -1;
}

ST_FUNC unsigned create_plt_entry(TCCState *s1, unsigned got_offset, struct sym_attr *attr)
{
    Section *plt = s1->plt;
    uint8_t *p;
    int modrm;
    unsigned plt_offset, relofs;

    /* on i386 if we build a DLL, we add a %ebx offset */
    if (s1->output_type == TCC_OUTPUT_DLL)
        modrm = 0xa3;
    else
        modrm = 0x25;

    /* empty PLT: create PLT0 entry that pushes the library identifier
       (GOT + PTR_SIZE) and jumps to ld.so resolution routine
       (GOT + 2 * PTR_SIZE) */
    if (plt->data_offset == 0) {
        p = section_ptr_add(plt, 16);
        p[0] = 0xff; /* pushl got + PTR_SIZE */
        p[1] = modrm + 0x10;
        write32le(p + 2, PTR_SIZE);
        p[6] = 0xff; /* jmp *(got + PTR_SIZE * 2) */
        p[7] = modrm;
        write32le(p + 8, PTR_SIZE * 2);
    }
    plt_offset = plt->data_offset;

    /* The PLT slot refers to the relocation entry it needs via offset.
       The reloc entry is created below, so its offset is the current
       data_offset */
    relofs = s1->got->reloc ? s1->got->reloc->data_offset : 0;

    /* Jump to GOT entry where ld.so initially put the address of ip + 4 */
    p = section_ptr_add(plt, 16);
    p[0] = 0xff; /* jmp *(got + x) */
    p[1] = modrm;
    write32le(p + 2, got_offset);
    p[6] = 0x68; /* push $xxx */
    write32le(p + 7, relofs);
    p[11] = 0xe9; /* jmp plt_start */
    write32le(p + 12, -(plt->data_offset));
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
        add32le(p + 2, s1->got->sh_addr);
        add32le(p + 8, s1->got->sh_addr);
        p += 16;
        while (p < p_end) {
            add32le(p + 2, s1->got->sh_addr);
            p += 16;
        }
    }
}

static ElfW_Rel *qrel; /* ptr to next reloc entry reused */

void relocate_init(Section *sr)
{
    qrel = (ElfW_Rel *) sr->data;
}

void relocate(TCCState *s1, ElfW_Rel *rel, int type, unsigned char *ptr, addr_t addr, addr_t val)
{
    int sym_index, esym_index;

    sym_index = ELFW(R_SYM)(rel->r_info);

    switch (type) {
        case R_386_32:
            if (s1->output_type == TCC_OUTPUT_DLL) {
                esym_index = s1->sym_attrs[sym_index].dyn_index;
                qrel->r_offset = rel->r_offset;
                if (esym_index) {
                    qrel->r_info = ELFW(R_INFO)(esym_index, R_386_32);
                    qrel++;
                    return;
                } else {
                    qrel->r_info = ELFW(R_INFO)(0, R_386_RELATIVE);
                    qrel++;
                }
            }
            add32le(ptr, val);
            return;
        case R_386_PC32:
            if (s1->output_type == TCC_OUTPUT_DLL) {
                /* DLL relocation */
                esym_index = s1->sym_attrs[sym_index].dyn_index;
                if (esym_index) {
                    qrel->r_offset = rel->r_offset;
                    qrel->r_info = ELFW(R_INFO)(esym_index, R_386_PC32);
                    qrel++;
                    return;
                }
            }
            add32le(ptr, val - addr);
            return;
        case R_386_PLT32:
            add32le(ptr, val - addr);
            return;
        case R_386_GLOB_DAT:
        case R_386_JMP_SLOT:
            write32le(ptr, val);
            return;
        case R_386_GOTPC:
            add32le(ptr, s1->got->sh_addr - addr);
            return;
        case R_386_GOTOFF:
            add32le(ptr, val - s1->got->sh_addr);
            return;
        case R_386_GOT32:
        case R_386_GOT32X:
            /* we load the got offset */
            add32le(ptr, s1->sym_attrs[sym_index].got_offset);
            return;
        case R_386_16:
            if (s1->output_format != TCC_OUTPUT_FORMAT_BINARY) {
            output_file:
                tcc_error("can only produce 16-bit binary files");
            }
            write16le(ptr, read16le(ptr) + val);
            return;
        case R_386_PC16:
            if (s1->output_format != TCC_OUTPUT_FORMAT_BINARY)
                goto output_file;
            write16le(ptr, read16le(ptr) + val - addr);
            return;
        case R_386_RELATIVE:
#ifdef TCC_TARGET_PE
            add32le(ptr, val - s1->pe_imagebase);
#endif
            /* do nothing */
            return;
        case R_386_COPY:
            /* This relocation must copy initialized data from the library
            to the program .bss segment. Currently made like for ARM
            (to remove noise of default case). Is this true?
            */
            return;
        default:
            fprintf(stderr,"FIXME: handle reloc type %d at %x [%p] to %x\n",
                type, (unsigned)addr, ptr, (unsigned)val);
            return;
    }
}

#endif /* !TARGET_DEFS_ONLY */
