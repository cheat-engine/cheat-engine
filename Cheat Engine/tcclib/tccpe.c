/*
 *  TCCPE.C - PE file output for the Tiny C Compiler
 *
 *  Copyright (c) 2005-2007 grischka
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

#define PE_MERGE_DATA
/* #define PE_PRINT_SECTIONS */

#ifndef _WIN32
#define stricmp strcasecmp
#define strnicmp strncasecmp
#include <sys/stat.h> /* chmod() */
#endif

#ifdef TCC_TARGET_X86_64
# define ADDR3264 ULONGLONG
# define PE_IMAGE_REL IMAGE_REL_BASED_DIR64
# define REL_TYPE_DIRECT R_X86_64_64
# define R_XXX_THUNKFIX R_X86_64_PC32
# define R_XXX_RELATIVE R_X86_64_RELATIVE
# define R_XXX_FUNCCALL R_X86_64_PC32
# define IMAGE_FILE_MACHINE 0x8664
# define RSRC_RELTYPE 3

#elif defined TCC_TARGET_ARM
# define ADDR3264 DWORD
# define PE_IMAGE_REL IMAGE_REL_BASED_HIGHLOW
# define REL_TYPE_DIRECT R_ARM_ABS32
# define R_XXX_THUNKFIX R_ARM_ABS32
# define R_XXX_RELATIVE R_ARM_RELATIVE
# define R_XXX_FUNCCALL R_ARM_PC24
# define R_XXX_FUNCCALL2 R_ARM_ABS32
# define IMAGE_FILE_MACHINE 0x01C0
# define RSRC_RELTYPE 7 /* ??? (not tested) */

#elif defined TCC_TARGET_I386
# define ADDR3264 DWORD
# define PE_IMAGE_REL IMAGE_REL_BASED_HIGHLOW
# define REL_TYPE_DIRECT R_386_32
# define R_XXX_THUNKFIX R_386_32
# define R_XXX_RELATIVE R_386_RELATIVE
# define R_XXX_FUNCCALL R_386_PC32
# define IMAGE_FILE_MACHINE 0x014C
# define RSRC_RELTYPE 7 /* DIR32NB */

#endif

#ifndef IMAGE_NT_SIGNATURE
/* ----------------------------------------------------------- */
/* definitions below are from winnt.h */

typedef unsigned char BYTE;
typedef unsigned short WORD;
typedef unsigned int DWORD;
typedef unsigned long long ULONGLONG;
#pragma pack(push, 1)

typedef struct _IMAGE_DOS_HEADER {  /* DOS .EXE header */
    WORD e_magic;         /* Magic number */
    WORD e_cblp;          /* Bytes on last page of file */
    WORD e_cp;            /* Pages in file */
    WORD e_crlc;          /* Relocations */
    WORD e_cparhdr;       /* Size of header in paragraphs */
    WORD e_minalloc;      /* Minimum extra paragraphs needed */
    WORD e_maxalloc;      /* Maximum extra paragraphs needed */
    WORD e_ss;            /* Initial (relative) SS value */
    WORD e_sp;            /* Initial SP value */
    WORD e_csum;          /* Checksum */
    WORD e_ip;            /* Initial IP value */
    WORD e_cs;            /* Initial (relative) CS value */
    WORD e_lfarlc;        /* File address of relocation table */
    WORD e_ovno;          /* Overlay number */
    WORD e_res[4];        /* Reserved words */
    WORD e_oemid;         /* OEM identifier (for e_oeminfo) */
    WORD e_oeminfo;       /* OEM information; e_oemid specific */
    WORD e_res2[10];      /* Reserved words */
    DWORD e_lfanew;        /* File address of new exe header */
} IMAGE_DOS_HEADER, *PIMAGE_DOS_HEADER;

#define IMAGE_NT_SIGNATURE  0x00004550  /* PE00 */
#define SIZE_OF_NT_SIGNATURE 4

typedef struct _IMAGE_FILE_HEADER {
    WORD    Machine;
    WORD    NumberOfSections;
    DWORD   TimeDateStamp;
    DWORD   PointerToSymbolTable;
    DWORD   NumberOfSymbols;
    WORD    SizeOfOptionalHeader;
    WORD    Characteristics;
} IMAGE_FILE_HEADER, *PIMAGE_FILE_HEADER;


#define IMAGE_SIZEOF_FILE_HEADER 20

typedef struct _IMAGE_DATA_DIRECTORY {
    DWORD   VirtualAddress;
    DWORD   Size;
} IMAGE_DATA_DIRECTORY, *PIMAGE_DATA_DIRECTORY;


typedef struct _IMAGE_OPTIONAL_HEADER {
    /* Standard fields. */
    WORD    Magic;
    BYTE    MajorLinkerVersion;
    BYTE    MinorLinkerVersion;
    DWORD   SizeOfCode;
    DWORD   SizeOfInitializedData;
    DWORD   SizeOfUninitializedData;
    DWORD   AddressOfEntryPoint;
    DWORD   BaseOfCode;
#ifndef TCC_TARGET_X86_64
    DWORD   BaseOfData;
#endif
    /* NT additional fields. */
    ADDR3264 ImageBase;
    DWORD   SectionAlignment;
    DWORD   FileAlignment;
    WORD    MajorOperatingSystemVersion;
    WORD    MinorOperatingSystemVersion;
    WORD    MajorImageVersion;
    WORD    MinorImageVersion;
    WORD    MajorSubsystemVersion;
    WORD    MinorSubsystemVersion;
    DWORD   Win32VersionValue;
    DWORD   SizeOfImage;
    DWORD   SizeOfHeaders;
    DWORD   CheckSum;
    WORD    Subsystem;
    WORD    DllCharacteristics;
    ADDR3264 SizeOfStackReserve;
    ADDR3264 SizeOfStackCommit;
    ADDR3264 SizeOfHeapReserve;
    ADDR3264 SizeOfHeapCommit;
    DWORD   LoaderFlags;
    DWORD   NumberOfRvaAndSizes;
    IMAGE_DATA_DIRECTORY DataDirectory[16];
} IMAGE_OPTIONAL_HEADER32, IMAGE_OPTIONAL_HEADER64, IMAGE_OPTIONAL_HEADER;

#define IMAGE_DIRECTORY_ENTRY_EXPORT          0   /* Export Directory */
#define IMAGE_DIRECTORY_ENTRY_IMPORT          1   /* Import Directory */
#define IMAGE_DIRECTORY_ENTRY_RESOURCE        2   /* Resource Directory */
#define IMAGE_DIRECTORY_ENTRY_EXCEPTION       3   /* Exception Directory */
#define IMAGE_DIRECTORY_ENTRY_SECURITY        4   /* Security Directory */
#define IMAGE_DIRECTORY_ENTRY_BASERELOC       5   /* Base Relocation Table */
#define IMAGE_DIRECTORY_ENTRY_DEBUG           6   /* Debug Directory */
/*      IMAGE_DIRECTORY_ENTRY_COPYRIGHT       7      (X86 usage) */
#define IMAGE_DIRECTORY_ENTRY_ARCHITECTURE    7   /* Architecture Specific Data */
#define IMAGE_DIRECTORY_ENTRY_GLOBALPTR       8   /* RVA of GP */
#define IMAGE_DIRECTORY_ENTRY_TLS             9   /* TLS Directory */
#define IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG    10   /* Load Configuration Directory */
#define IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT   11   /* Bound Import Directory in headers */
#define IMAGE_DIRECTORY_ENTRY_IAT            12   /* Import Address Table */
#define IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT   13   /* Delay Load Import Descriptors */
#define IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR 14   /* COM Runtime descriptor */

/* Section header format. */
#define IMAGE_SIZEOF_SHORT_NAME         8

typedef struct _IMAGE_SECTION_HEADER {
    BYTE    Name[IMAGE_SIZEOF_SHORT_NAME];
    union {
            DWORD   PhysicalAddress;
            DWORD   VirtualSize;
    } Misc;
    DWORD   VirtualAddress;
    DWORD   SizeOfRawData;
    DWORD   PointerToRawData;
    DWORD   PointerToRelocations;
    DWORD   PointerToLinenumbers;
    WORD    NumberOfRelocations;
    WORD    NumberOfLinenumbers;
    DWORD   Characteristics;
} IMAGE_SECTION_HEADER, *PIMAGE_SECTION_HEADER;

#define IMAGE_SIZEOF_SECTION_HEADER     40

typedef struct _IMAGE_EXPORT_DIRECTORY {
    DWORD Characteristics;
    DWORD TimeDateStamp;
    WORD MajorVersion;
    WORD MinorVersion;
    DWORD Name;
    DWORD Base;
    DWORD NumberOfFunctions;
    DWORD NumberOfNames;
    DWORD AddressOfFunctions;
    DWORD AddressOfNames;
    DWORD AddressOfNameOrdinals;
} IMAGE_EXPORT_DIRECTORY,*PIMAGE_EXPORT_DIRECTORY;

typedef struct _IMAGE_IMPORT_DESCRIPTOR {
    union {
        DWORD Characteristics;
        DWORD OriginalFirstThunk;
    };
    DWORD TimeDateStamp;
    DWORD ForwarderChain;
    DWORD Name;
    DWORD FirstThunk;
} IMAGE_IMPORT_DESCRIPTOR;

typedef struct _IMAGE_BASE_RELOCATION {
    DWORD   VirtualAddress;
    DWORD   SizeOfBlock;
//  WORD    TypeOffset[1];
} IMAGE_BASE_RELOCATION;

#define IMAGE_SIZEOF_BASE_RELOCATION     8

#define IMAGE_REL_BASED_ABSOLUTE         0
#define IMAGE_REL_BASED_HIGH             1
#define IMAGE_REL_BASED_LOW              2
#define IMAGE_REL_BASED_HIGHLOW          3
#define IMAGE_REL_BASED_HIGHADJ          4
#define IMAGE_REL_BASED_MIPS_JMPADDR     5
#define IMAGE_REL_BASED_SECTION          6
#define IMAGE_REL_BASED_REL32            7
#define IMAGE_REL_BASED_DIR64           10

#define IMAGE_SCN_CNT_CODE                  0x00000020
#define IMAGE_SCN_CNT_INITIALIZED_DATA      0x00000040
#define IMAGE_SCN_CNT_UNINITIALIZED_DATA    0x00000080
#define IMAGE_SCN_MEM_DISCARDABLE           0x02000000
#define IMAGE_SCN_MEM_SHARED                0x10000000
#define IMAGE_SCN_MEM_EXECUTE               0x20000000
#define IMAGE_SCN_MEM_READ                  0x40000000
#define IMAGE_SCN_MEM_WRITE                 0x80000000

#pragma pack(pop)

/* ----------------------------------------------------------- */
#endif /* ndef IMAGE_NT_SIGNATURE */
/* ----------------------------------------------------------- */

#ifndef IMAGE_REL_BASED_DIR64
# define IMAGE_REL_BASED_DIR64 10
#endif

#pragma pack(push, 1)
struct pe_header
{
    IMAGE_DOS_HEADER doshdr;
    BYTE dosstub[0x40];
    DWORD nt_sig;
    IMAGE_FILE_HEADER filehdr;
#ifdef TCC_TARGET_X86_64
    IMAGE_OPTIONAL_HEADER64 opthdr;
#else
#ifdef _WIN64
    IMAGE_OPTIONAL_HEADER32 opthdr;
#else
    IMAGE_OPTIONAL_HEADER opthdr;
#endif
#endif
};

struct pe_reloc_header {
    DWORD offset;
    DWORD size;
};

struct pe_rsrc_header {
    struct _IMAGE_FILE_HEADER filehdr;
    struct _IMAGE_SECTION_HEADER sectionhdr;
};

struct pe_rsrc_reloc {
    DWORD offset;
    DWORD size;
    WORD type;
};
#pragma pack(pop)

/* ------------------------------------------------------------- */
/* internal temporary structures */

enum {
    sec_text = 0,
    sec_data ,
    sec_bss ,
    sec_idata ,
    sec_pdata ,
    sec_other ,
    sec_rsrc ,
    sec_stab ,
    sec_stabstr ,
    sec_reloc ,
    sec_last
};

#if 0
static const DWORD pe_sec_flags[] = {
    0x60000020, /* ".text"     , */
    0xC0000040, /* ".data"     , */
    0xC0000080, /* ".bss"      , */
    0x40000040, /* ".idata"    , */
    0x40000040, /* ".pdata"    , */
    0xE0000060, /* < other >   , */
    0x40000040, /* ".rsrc"     , */
    0x42000802, /* ".stab"     , */
    0x42000040, /* ".reloc"    , */
};
#endif

struct section_info {
    int cls;
    char name[32];
    DWORD sh_addr;
    DWORD sh_size;
    DWORD pe_flags;
    Section *sec;
    DWORD data_size;
    IMAGE_SECTION_HEADER ish;
};

struct import_symbol {
    int sym_index;
    int iat_index;
    int thk_offset;
};

struct pe_import_info {
    int dll_index;
    int sym_count;
    struct import_symbol **symbols;
};

struct pe_info {
    TCCState *s1;
    Section *reloc;
    Section *thunk;
    const char *filename;
    int type;
    DWORD sizeofheaders;
    ADDR3264 imagebase;
    const char *start_symbol;
    DWORD start_addr;
    DWORD imp_offs;
    DWORD imp_size;
    DWORD iat_offs;
    DWORD iat_size;
    DWORD exp_offs;
    DWORD exp_size;
    int subsystem;
    DWORD section_align;
    DWORD file_align;
    struct section_info **sec_info;
    int sec_count;
    struct pe_import_info **imp_info;
    int imp_count;
};

#define PE_NUL 0
#define PE_DLL 1
#define PE_GUI 2
#define PE_EXE 3
#define PE_RUN 4

/* --------------------------------------------*/

static const char *pe_export_name(TCCState *s1, ElfW(Sym) *sym)
{
    const char *name = (char*)symtab_section->link->data + sym->st_name;
    if (s1->leading_underscore && name[0] == '_' && !(sym->st_other & ST_PE_STDCALL))
        return name + 1;
    return name;
}

static int pe_find_import(TCCState * s1, ElfW(Sym) *sym)
{
    char buffer[200];
    const char *s, *p;
    int sym_index = 0, n = 0;
    int a, err = 0;

    do {
        s = pe_export_name(s1, sym);
        a = 0;
        if (n) {
            /* second try: */
	    if (sym->st_other & ST_PE_STDCALL) {
                /* try w/0 stdcall deco (windows API convention) */
	        p = strrchr(s, '@');
	        if (!p || s[0] != '_')
	            break;
	        strcpy(buffer, s+1)[p-s-1] = 0;
	    } else if (s[0] != '_') { /* try non-ansi function */
	        buffer[0] = '_', strcpy(buffer + 1, s);
	    } else if (0 == memcmp(s, "__imp_", 6)) { /* mingw 2.0 */
	        strcpy(buffer, s + 6), a = 1;
	    } else if (0 == memcmp(s, "_imp__", 6)) { /* mingw 3.7 */
	        strcpy(buffer, s + 6), a = 1;
	    } else {
	        continue;
	    }
	    s = buffer;
        }
        sym_index = find_elf_sym(s1->dynsymtab_section, s);
        // printf("find (%d) %d %s\n", n, sym_index, s);
        if (sym_index
            && ELFW(ST_TYPE)(sym->st_info) == STT_OBJECT
            && 0 == (sym->st_other & ST_PE_IMPORT)
            && 0 == a
            ) err = -1, sym_index = 0;
    } while (0 == sym_index && ++n < 2);
    return n == 2 ? err : sym_index;
}

/*----------------------------------------------------------------------------*/

static int dynarray_assoc(void **pp, int n, int key)
{
    int i;
    for (i = 0; i < n; ++i, ++pp)
    if (key == **(int **) pp)
        return i;
    return -1;
}

static DWORD umin(DWORD a, DWORD b)
{
    return a < b ? a : b;
}

static DWORD umax(DWORD a, DWORD b)
{
    return a < b ? b : a;
}

static DWORD pe_file_align(struct pe_info *pe, DWORD n)
{
    return (n + (pe->file_align - 1)) & ~(pe->file_align - 1);
}

static DWORD pe_virtual_align(struct pe_info *pe, DWORD n)
{
    return (n + (pe->section_align - 1)) & ~(pe->section_align - 1);
}

static void pe_align_section(Section *s, int a)
{
    int i = s->data_offset & (a-1);
    if (i)
        section_ptr_add(s, a - i);
}

static void pe_set_datadir(struct pe_header *hdr, int dir, DWORD addr, DWORD size)
{
    hdr->opthdr.DataDirectory[dir].VirtualAddress = addr;
    hdr->opthdr.DataDirectory[dir].Size = size;
}

struct pe_file {
    FILE *op;
    DWORD sum;
    unsigned pos;
};

static int pe_fwrite(void *data, int len, struct pe_file *pf)
{
    WORD *p = data;
    DWORD sum;
    int ret, i;
    pf->pos += (ret = fwrite(data, 1, len, pf->op));
    sum = pf->sum;
    for (i = len; i > 0; i -= 2) {
        sum += (i >= 2) ? *p++ : *(BYTE*)p;
        sum = (sum + (sum >> 16)) & 0xFFFF;
    }
    pf->sum = sum;
    return len == ret ? 0 : -1;
}

static void pe_fpad(struct pe_file *pf, DWORD new_pos)
{
    char buf[256];
    int n, diff = new_pos - pf->pos;
    memset(buf, 0, sizeof buf);
    while (diff > 0) {
        diff -= n = umin(diff, sizeof buf);
        fwrite(buf, n, 1, pf->op);
    }
    pf->pos = new_pos;
}

/*----------------------------------------------------------------------------*/
static int pe_write(struct pe_info *pe)
{
    static const struct pe_header pe_template = {
    {
    /* IMAGE_DOS_HEADER doshdr */
    0x5A4D, /*WORD e_magic;         Magic number */
    0x0090, /*WORD e_cblp;          Bytes on last page of file */
    0x0003, /*WORD e_cp;            Pages in file */
    0x0000, /*WORD e_crlc;          Relocations */

    0x0004, /*WORD e_cparhdr;       Size of header in paragraphs */
    0x0000, /*WORD e_minalloc;      Minimum extra paragraphs needed */
    0xFFFF, /*WORD e_maxalloc;      Maximum extra paragraphs needed */
    0x0000, /*WORD e_ss;            Initial (relative) SS value */

    0x00B8, /*WORD e_sp;            Initial SP value */
    0x0000, /*WORD e_csum;          Checksum */
    0x0000, /*WORD e_ip;            Initial IP value */
    0x0000, /*WORD e_cs;            Initial (relative) CS value */
    0x0040, /*WORD e_lfarlc;        File address of relocation table */
    0x0000, /*WORD e_ovno;          Overlay number */
    {0,0,0,0}, /*WORD e_res[4];     Reserved words */
    0x0000, /*WORD e_oemid;         OEM identifier (for e_oeminfo) */
    0x0000, /*WORD e_oeminfo;       OEM information; e_oemid specific */
    {0,0,0,0,0,0,0,0,0,0}, /*WORD e_res2[10];      Reserved words */
    0x00000080  /*DWORD   e_lfanew;        File address of new exe header */
    },{
    /* BYTE dosstub[0x40] */
    /* 14 code bytes + "This program cannot be run in DOS mode.\r\r\n$" + 6 * 0x00 */
    0x0e,0x1f,0xba,0x0e,0x00,0xb4,0x09,0xcd,0x21,0xb8,0x01,0x4c,0xcd,0x21,0x54,0x68,
    0x69,0x73,0x20,0x70,0x72,0x6f,0x67,0x72,0x61,0x6d,0x20,0x63,0x61,0x6e,0x6e,0x6f,
    0x74,0x20,0x62,0x65,0x20,0x72,0x75,0x6e,0x20,0x69,0x6e,0x20,0x44,0x4f,0x53,0x20,
    0x6d,0x6f,0x64,0x65,0x2e,0x0d,0x0d,0x0a,0x24,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    },
    0x00004550, /* DWORD nt_sig = IMAGE_NT_SIGNATURE */
    {
    /* IMAGE_FILE_HEADER filehdr */
    IMAGE_FILE_MACHINE, /*WORD    Machine; */
    0x0003, /*WORD    NumberOfSections; */
    0x00000000, /*DWORD   TimeDateStamp; */
    0x00000000, /*DWORD   PointerToSymbolTable; */
    0x00000000, /*DWORD   NumberOfSymbols; */
#if defined(TCC_TARGET_X86_64)
    0x00F0, /*WORD    SizeOfOptionalHeader; */
    0x022F  /*WORD    Characteristics; */
#define CHARACTERISTICS_DLL 0x222E
#elif defined(TCC_TARGET_I386)
    0x00E0, /*WORD    SizeOfOptionalHeader; */
    0x030F  /*WORD    Characteristics; */
#define CHARACTERISTICS_DLL 0x230E
#elif defined(TCC_TARGET_ARM)
    0x00E0, /*WORD    SizeOfOptionalHeader; */
    0x010F, /*WORD    Characteristics; */
#define CHARACTERISTICS_DLL 0x230F
#endif
},{
    /* IMAGE_OPTIONAL_HEADER opthdr */
    /* Standard fields. */
#ifdef TCC_TARGET_X86_64
    0x020B, /*WORD    Magic; */
#else
    0x010B, /*WORD    Magic; */
#endif
    0x06, /*BYTE    MajorLinkerVersion; */
    0x00, /*BYTE    MinorLinkerVersion; */
    0x00000000, /*DWORD   SizeOfCode; */
    0x00000000, /*DWORD   SizeOfInitializedData; */
    0x00000000, /*DWORD   SizeOfUninitializedData; */
    0x00000000, /*DWORD   AddressOfEntryPoint; */
    0x00000000, /*DWORD   BaseOfCode; */
#ifndef TCC_TARGET_X86_64
    0x00000000, /*DWORD   BaseOfData; */
#endif
    /* NT additional fields. */
#if defined(TCC_TARGET_ARM)
    0x00100000,	    /*DWORD   ImageBase; */
#else
    0x00400000,	    /*DWORD   ImageBase; */
#endif
    0x00001000, /*DWORD   SectionAlignment; */
    0x00000200, /*DWORD   FileAlignment; */
    0x0004, /*WORD    MajorOperatingSystemVersion; */
    0x0000, /*WORD    MinorOperatingSystemVersion; */
    0x0000, /*WORD    MajorImageVersion; */
    0x0000, /*WORD    MinorImageVersion; */
    0x0004, /*WORD    MajorSubsystemVersion; */
    0x0000, /*WORD    MinorSubsystemVersion; */
    0x00000000, /*DWORD   Win32VersionValue; */
    0x00000000, /*DWORD   SizeOfImage; */
    0x00000200, /*DWORD   SizeOfHeaders; */
    0x00000000, /*DWORD   CheckSum; */
    0x0002, /*WORD    Subsystem; */
    0x0000, /*WORD    DllCharacteristics; */
    0x00100000, /*DWORD   SizeOfStackReserve; */
    0x00001000, /*DWORD   SizeOfStackCommit; */
    0x00100000, /*DWORD   SizeOfHeapReserve; */
    0x00001000, /*DWORD   SizeOfHeapCommit; */
    0x00000000, /*DWORD   LoaderFlags; */
    0x00000010, /*DWORD   NumberOfRvaAndSizes; */

    /* IMAGE_DATA_DIRECTORY DataDirectory[16]; */
    {{0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
     {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}}
    }};

    struct pe_header pe_header = pe_template;

    int i;
    struct pe_file pf = {0};
    DWORD file_offset;
    struct section_info *si;
    IMAGE_SECTION_HEADER *psh;
    TCCState *s1 = pe->s1;

    pf.op = fopen(pe->filename, "wb");
    if (NULL == pf.op) {
        tcc_error_noabort("could not write '%s': %s", pe->filename, strerror(errno));
        return -1;
    }

    pe->sizeofheaders = pe_file_align(pe,
        sizeof (struct pe_header)
        + pe->sec_count * sizeof (IMAGE_SECTION_HEADER)
        );

    file_offset = pe->sizeofheaders;

    if (2 == pe->s1->verbose)
        printf("-------------------------------"
               "\n  virt   file   size  section" "\n");
    for (i = 0; i < pe->sec_count; ++i) {
        DWORD addr, size;
        const char *sh_name;

        si = pe->sec_info[i];
        sh_name = si->name;
        addr = si->sh_addr - pe->imagebase;
        size = si->sh_size;
        psh = &si->ish;

        if (2 == pe->s1->verbose)
            printf("%6x %6x %6x  %s\n",
                (unsigned)addr, (unsigned)file_offset, (unsigned)size, sh_name);

        switch (si->cls) {
            case sec_text:
                if (!pe_header.opthdr.BaseOfCode)
                    pe_header.opthdr.BaseOfCode = addr;
                break;

            case sec_data:
#ifndef TCC_TARGET_X86_64
                if (!pe_header.opthdr.BaseOfData)
                    pe_header.opthdr.BaseOfData = addr;
#endif
                break;

            case sec_bss:
                break;

            case sec_reloc:
                pe_set_datadir(&pe_header, IMAGE_DIRECTORY_ENTRY_BASERELOC, addr, size);
                break;

            case sec_rsrc:
                pe_set_datadir(&pe_header, IMAGE_DIRECTORY_ENTRY_RESOURCE, addr, size);
                break;

            case sec_pdata:
                pe_set_datadir(&pe_header, IMAGE_DIRECTORY_ENTRY_EXCEPTION, addr, size);
                break;
        }

        if (pe->imp_size) {
            pe_set_datadir(&pe_header, IMAGE_DIRECTORY_ENTRY_IMPORT,
                pe->imp_offs, pe->imp_size);
            pe_set_datadir(&pe_header, IMAGE_DIRECTORY_ENTRY_IAT,
                pe->iat_offs, pe->iat_size);
        }
        if (pe->exp_size) {
            pe_set_datadir(&pe_header, IMAGE_DIRECTORY_ENTRY_EXPORT,
                pe->exp_offs, pe->exp_size);
        }

        memcpy(psh->Name, sh_name, umin(strlen(sh_name), sizeof psh->Name));

        psh->Characteristics = si->pe_flags;
        psh->VirtualAddress = addr;
        psh->Misc.VirtualSize = size;
        pe_header.opthdr.SizeOfImage =
            umax(pe_virtual_align(pe, size + addr), pe_header.opthdr.SizeOfImage);

        if (si->data_size) {
            psh->PointerToRawData = file_offset;
            file_offset = pe_file_align(pe, file_offset + si->data_size);
            psh->SizeOfRawData = file_offset - psh->PointerToRawData;
            if (si->cls == sec_text)
                pe_header.opthdr.SizeOfCode += psh->SizeOfRawData;
            else
                pe_header.opthdr.SizeOfInitializedData += psh->SizeOfRawData;
        }
    }

    //pe_header.filehdr.TimeDateStamp = time(NULL);
    pe_header.filehdr.NumberOfSections = pe->sec_count;
    pe_header.opthdr.AddressOfEntryPoint = pe->start_addr;
    pe_header.opthdr.SizeOfHeaders = pe->sizeofheaders;
    pe_header.opthdr.ImageBase = pe->imagebase;
    pe_header.opthdr.Subsystem = pe->subsystem;
    if (pe->s1->pe_stack_size)
        pe_header.opthdr.SizeOfStackReserve = pe->s1->pe_stack_size;
    if (PE_DLL == pe->type)
        pe_header.filehdr.Characteristics = CHARACTERISTICS_DLL;
    pe_header.filehdr.Characteristics |= pe->s1->pe_characteristics;

    pe_fwrite(&pe_header, sizeof pe_header, &pf);
    for (i = 0; i < pe->sec_count; ++i)
        pe_fwrite(&pe->sec_info[i]->ish, sizeof(IMAGE_SECTION_HEADER), &pf);

    file_offset = pe->sizeofheaders;
    for (i = 0; i < pe->sec_count; ++i) {
        Section *s;
        si = pe->sec_info[i];
        for (s = si->sec; s; s = s->prev) {
            pe_fpad(&pf, file_offset);
            pe_fwrite(s->data, s->data_offset, &pf);
            if (s->prev)
                file_offset += s->prev->sh_addr - s->sh_addr;
        }
        file_offset = si->ish.PointerToRawData + si->ish.SizeOfRawData;
        pe_fpad(&pf, file_offset);
    }

    pf.sum += file_offset;
    fseek(pf.op, offsetof(struct pe_header, opthdr.CheckSum), SEEK_SET);
    pe_fwrite(&pf.sum, sizeof (DWORD), &pf);

    fclose (pf.op);
#ifndef _WIN32
    chmod(pe->filename, 0777);
#endif

    if (2 == pe->s1->verbose)
        printf("-------------------------------\n");
    if (pe->s1->verbose)
        printf("<- %s (%u bytes)\n", pe->filename, (unsigned)file_offset);

    return 0;
}

/*----------------------------------------------------------------------------*/

static struct import_symbol *pe_add_import(struct pe_info *pe, int sym_index)
{
    int i;
    int dll_index;
    struct pe_import_info *p;
    struct import_symbol *s;
    ElfW(Sym) *isym;

    isym = (ElfW(Sym) *)pe->s1->dynsymtab_section->data + sym_index;
    dll_index = isym->st_size;

    i = dynarray_assoc ((void**)pe->imp_info, pe->imp_count, dll_index);
    if (-1 != i) {
        p = pe->imp_info[i];
        goto found_dll;
    }
    p = tcc_mallocz(sizeof *p);
    p->dll_index = dll_index;
    dynarray_add(&pe->imp_info, &pe->imp_count, p);

found_dll:
    i = dynarray_assoc ((void**)p->symbols, p->sym_count, sym_index);
    if (-1 != i)
        return p->symbols[i];

    s = tcc_mallocz(sizeof *s);
    dynarray_add(&p->symbols, &p->sym_count, s);
    s->sym_index = sym_index;
    return s;
}

void pe_free_imports(struct pe_info *pe)
{
    int i;
    for (i = 0; i < pe->imp_count; ++i) {
        struct pe_import_info *p = pe->imp_info[i];
        dynarray_reset(&p->symbols, &p->sym_count);
    }
    dynarray_reset(&pe->imp_info, &pe->imp_count);
}

/*----------------------------------------------------------------------------*/
static void pe_build_imports(struct pe_info *pe)
{
    int thk_ptr, ent_ptr, dll_ptr, sym_cnt, i;
    DWORD rva_base = pe->thunk->sh_addr - pe->imagebase;
    int ndlls = pe->imp_count;
    TCCState *s1 = pe->s1;

    for (sym_cnt = i = 0; i < ndlls; ++i)
        sym_cnt += pe->imp_info[i]->sym_count;

    if (0 == sym_cnt)
        return;

    pe_align_section(pe->thunk, 16);
    pe->imp_size = (ndlls + 1) * sizeof(IMAGE_IMPORT_DESCRIPTOR);
    pe->iat_size = (sym_cnt + ndlls) * sizeof(ADDR3264);
    dll_ptr = pe->thunk->data_offset;
    thk_ptr = dll_ptr + pe->imp_size;
    ent_ptr = thk_ptr + pe->iat_size;
    pe->imp_offs = dll_ptr + rva_base;
    pe->iat_offs = thk_ptr + rva_base;
    section_ptr_add(pe->thunk, pe->imp_size + 2*pe->iat_size);

    for (i = 0; i < pe->imp_count; ++i) {
        IMAGE_IMPORT_DESCRIPTOR *hdr;
        int k, n, dllindex;
        ADDR3264 v;
        struct pe_import_info *p = pe->imp_info[i];
        const char *name;
        DLLReference *dllref;

        dllindex = p->dll_index;
        if (dllindex)
            name = tcc_basename((dllref = pe->s1->loaded_dlls[dllindex-1])->name);
        else
            name = "", dllref = NULL;

        /* put the dll name into the import header */
        v = put_elf_str(pe->thunk, name);
        hdr = (IMAGE_IMPORT_DESCRIPTOR*)(pe->thunk->data + dll_ptr);
        hdr->FirstThunk = thk_ptr + rva_base;
        hdr->OriginalFirstThunk = ent_ptr + rva_base;
        hdr->Name = v + rva_base;

        for (k = 0, n = p->sym_count; k <= n; ++k) {
            if (k < n) {
                int iat_index = p->symbols[k]->iat_index;
                int sym_index = p->symbols[k]->sym_index;
                ElfW(Sym) *imp_sym = (ElfW(Sym) *)pe->s1->dynsymtab_section->data + sym_index;
                ElfW(Sym) *org_sym = (ElfW(Sym) *)symtab_section->data + iat_index;
                const char *name = (char*)pe->s1->dynsymtab_section->link->data + imp_sym->st_name;
                int ordinal;

                org_sym->st_value = thk_ptr;
                org_sym->st_shndx = pe->thunk->sh_num;

                if (dllref)
                    v = 0, ordinal = imp_sym->st_value; /* ordinal from pe_load_def */
                else
                    ordinal = 0, v = imp_sym->st_value; /* address from tcc_add_symbol() */

#ifdef TCC_IS_NATIVE
                if (pe->type == PE_RUN) {
                    if (dllref) {
                        if ( !dllref->handle )
                            dllref->handle = LoadLibrary(dllref->name);
                        v = (ADDR3264)GetProcAddress(dllref->handle, ordinal?(char*)0+ordinal:name);
                    }
                    if (!v)
                        tcc_error_noabort("could not resolve symbol '%s'", name);
                } else
#endif
                if (ordinal) {
                    v = ordinal | (ADDR3264)1 << (sizeof(ADDR3264)*8 - 1);
                } else {
                    v = pe->thunk->data_offset + rva_base;
                    section_ptr_add(pe->thunk, sizeof(WORD)); /* hint, not used */
                    put_elf_str(pe->thunk, name);
                }

            } else {
                v = 0; /* last entry is zero */
            }

            *(ADDR3264*)(pe->thunk->data+thk_ptr) =
            *(ADDR3264*)(pe->thunk->data+ent_ptr) = v;
            thk_ptr += sizeof (ADDR3264);
            ent_ptr += sizeof (ADDR3264);
        }
        dll_ptr += sizeof(IMAGE_IMPORT_DESCRIPTOR);
    }
}

/* ------------------------------------------------------------- */

struct pe_sort_sym
{
    int index;
    const char *name;
};

static int sym_cmp(const void *va, const void *vb)
{
    const char *ca = (*(struct pe_sort_sym**)va)->name;
    const char *cb = (*(struct pe_sort_sym**)vb)->name;
    return strcmp(ca, cb);
}

static void pe_build_exports(struct pe_info *pe)
{
    ElfW(Sym) *sym;
    int sym_index, sym_end;
    DWORD rva_base, base_o, func_o, name_o, ord_o, str_o;
    IMAGE_EXPORT_DIRECTORY *hdr;
    int sym_count, ord;
    struct pe_sort_sym **sorted, *p;
    TCCState *s1 = pe->s1;

    FILE *op;
    char buf[260];
    const char *dllname;
    const char *name;

    rva_base = pe->thunk->sh_addr - pe->imagebase;
    sym_count = 0, sorted = NULL, op = NULL;

    sym_end = symtab_section->data_offset / sizeof(ElfW(Sym));
    for (sym_index = 1; sym_index < sym_end; ++sym_index) {
        sym = (ElfW(Sym)*)symtab_section->data + sym_index;
        name = pe_export_name(pe->s1, sym);
        if (sym->st_other & ST_PE_EXPORT) {
            p = tcc_malloc(sizeof *p);
            p->index = sym_index;
            p->name = name;
            dynarray_add(&sorted, &sym_count, p);
        }
#if 0
        if (sym->st_other & ST_PE_EXPORT)
            printf("export: %s\n", name);
        if (sym->st_other & ST_PE_STDCALL)
            printf("stdcall: %s\n", name);
#endif
    }

    if (0 == sym_count)
        return;

    qsort (sorted, sym_count, sizeof *sorted, sym_cmp);

    pe_align_section(pe->thunk, 16);
    dllname = tcc_basename(pe->filename);

    base_o = pe->thunk->data_offset;
    func_o = base_o + sizeof(IMAGE_EXPORT_DIRECTORY);
    name_o = func_o + sym_count * sizeof (DWORD);
    ord_o = name_o + sym_count * sizeof (DWORD);
    str_o = ord_o + sym_count * sizeof(WORD);

    hdr = section_ptr_add(pe->thunk, str_o - base_o);
    hdr->Characteristics        = 0;
    hdr->Base                   = 1;
    hdr->NumberOfFunctions      = sym_count;
    hdr->NumberOfNames          = sym_count;
    hdr->AddressOfFunctions     = func_o + rva_base;
    hdr->AddressOfNames         = name_o + rva_base;
    hdr->AddressOfNameOrdinals  = ord_o + rva_base;
    hdr->Name                   = str_o + rva_base;
    put_elf_str(pe->thunk, dllname);

#if 1
    /* automatically write exports to <output-filename>.def */
    pstrcpy(buf, sizeof buf, pe->filename);
    strcpy(tcc_fileextension(buf), ".def");
    op = fopen(buf, "wb");
    if (NULL == op) {
        tcc_error_noabort("could not create '%s': %s", buf, strerror(errno));
    } else {
        fprintf(op, "LIBRARY %s\n\nEXPORTS\n", dllname);
        if (pe->s1->verbose)
            printf("<- %s (%d symbol%s)\n", buf, sym_count, &"s"[sym_count < 2]);
    }
#endif

    for (ord = 0; ord < sym_count; ++ord)
    {
        p = sorted[ord], sym_index = p->index, name = p->name;
        /* insert actual address later in relocate_section() */
        put_elf_reloc(symtab_section, pe->thunk,
            func_o, R_XXX_RELATIVE, sym_index);
        *(DWORD*)(pe->thunk->data + name_o)
            = pe->thunk->data_offset + rva_base;
        *(WORD*)(pe->thunk->data + ord_o)
            = ord;
        put_elf_str(pe->thunk, name);
        func_o += sizeof (DWORD);
        name_o += sizeof (DWORD);
        ord_o += sizeof (WORD);
        if (op)
            fprintf(op, "%s\n", name);
    }

    pe->exp_offs = base_o + rva_base;
    pe->exp_size = pe->thunk->data_offset - base_o;
    dynarray_reset(&sorted, &sym_count);
    if (op)
        fclose(op);
}

/* ------------------------------------------------------------- */
static void pe_build_reloc (struct pe_info *pe)
{
    DWORD offset, block_ptr, sh_addr, addr;
    int count, i;
    ElfW_Rel *rel, *rel_end;
    Section *s = NULL, *sr;
    struct pe_reloc_header *hdr;

    sh_addr = offset = block_ptr = count = i = 0;
    rel = rel_end = NULL;

    for(;;) {
        if (rel < rel_end) {
            int type = ELFW(R_TYPE)(rel->r_info);
            addr = rel->r_offset + sh_addr;
            ++ rel;
            if (type != REL_TYPE_DIRECT)
                continue;
            if (count == 0) { /* new block */
                block_ptr = pe->reloc->data_offset;
                section_ptr_add(pe->reloc, sizeof(struct pe_reloc_header));
                offset = addr & 0xFFFFFFFF<<12;
            }
            if ((addr -= offset)  < (1<<12)) { /* one block spans 4k addresses */
                WORD *wp = section_ptr_add(pe->reloc, sizeof (WORD));
                *wp = addr | PE_IMAGE_REL<<12;
                ++count;
                continue;
            }
            -- rel;

        } else if (s) {
            sr = s->reloc;
            if (sr) {
                rel = (ElfW_Rel *)sr->data;
                rel_end = (ElfW_Rel *)(sr->data + sr->data_offset);
                sh_addr = s->sh_addr;
            }
            s = s->prev;
            continue;

        } else if (i < pe->sec_count) {
            s = pe->sec_info[i]->sec, ++i;
            continue;

        } else if (!count)
            break;

        /* fill the last block and ready for a new one */
        if (count & 1) /* align for DWORDS */
            section_ptr_add(pe->reloc, sizeof(WORD)), ++count;
        hdr = (struct pe_reloc_header *)(pe->reloc->data + block_ptr);
        hdr -> offset = offset - pe->imagebase;
        hdr -> size = count * sizeof(WORD) + sizeof(struct pe_reloc_header);
        count = 0;
    }
}

/* ------------------------------------------------------------- */
static int pe_section_class(Section *s)
{
    int type, flags;
    const char *name;

    type = s->sh_type;
    flags = s->sh_flags;
    name = s->name;
    if (flags & SHF_ALLOC) {
        if (type == SHT_PROGBITS) {
            if (flags & SHF_EXECINSTR)
                return sec_text;
            if (flags & SHF_WRITE)
                return sec_data;
            if (0 == strcmp(name, ".rsrc"))
                return sec_rsrc;
            if (0 == strcmp(name, ".iedat"))
                return sec_idata;
            if (0 == strcmp(name, ".pdata"))
                return sec_pdata;
        } else if (type == SHT_NOBITS) {
            if (flags & SHF_WRITE)
                return sec_bss;
        }
    } else {
        if (0 == strcmp(name, ".reloc"))
            return sec_reloc;
    }
    if (0 == memcmp(name, ".stab", 5))
        return name[5] ? sec_stabstr : sec_stab;
    if (flags & SHF_ALLOC)
        return sec_other;
    return -1;
}

static int pe_assign_addresses (struct pe_info *pe)
{
    int i, k, o, c;
    DWORD addr;
    int *section_order;
    struct section_info *si;
    Section *s;

    if (PE_DLL == pe->type)
        pe->reloc = new_section(pe->s1, ".reloc", SHT_PROGBITS, 0);
    // pe->thunk = new_section(pe->s1, ".iedat", SHT_PROGBITS, SHF_ALLOC);

    section_order = tcc_malloc(pe->s1->nb_sections * sizeof (int));
    for (o = k = 0 ; k < sec_last; ++k) {
        for (i = 1; i < pe->s1->nb_sections; ++i) {
            s = pe->s1->sections[i];
            if (k == pe_section_class(s))
                section_order[o++] = i;
        }
    }

    si = NULL;
    addr = pe->imagebase + 1;

    for (i = 0; i < o; ++i) {
        k = section_order[i];
        s = pe->s1->sections[k];
        c = pe_section_class(s);

        if ((c == sec_stab || c == sec_stabstr) && 0 == pe->s1->do_debug)
            continue;

#ifdef PE_MERGE_DATA
        if (c == sec_bss)
            c = sec_data;
#endif
        if (si && c == si->cls) {
            /* merge with previous section */
            s->sh_addr = addr = ((addr - 1) | (16 - 1)) + 1;
        } else {
            si = NULL;
            s->sh_addr = addr = pe_virtual_align(pe, addr);
        }

        if (c == sec_data && NULL == pe->thunk)
            pe->thunk = s;

        if (s == pe->thunk) {
            pe_build_imports(pe);
            pe_build_exports(pe);
        }
        if (s == pe->reloc)
            pe_build_reloc (pe);

        if (0 == s->data_offset)
            continue;

        if (si)
            goto add_section;

        si = tcc_mallocz(sizeof *si);
        dynarray_add(&pe->sec_info, &pe->sec_count, si);

        strcpy(si->name, s->name);
        si->cls = c;
        si->sh_addr = addr;

        si->pe_flags = IMAGE_SCN_MEM_READ;
        if (s->sh_flags & SHF_EXECINSTR)
            si->pe_flags |= IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE;
        else if (s->sh_type == SHT_NOBITS)
            si->pe_flags |= IMAGE_SCN_CNT_UNINITIALIZED_DATA;
        else
            si->pe_flags |= IMAGE_SCN_CNT_INITIALIZED_DATA;
        if (s->sh_flags & SHF_WRITE)
            si->pe_flags |= IMAGE_SCN_MEM_WRITE;
        if (0 == (s->sh_flags & SHF_ALLOC))
            si->pe_flags |= IMAGE_SCN_MEM_DISCARDABLE;

add_section:
        addr += s->data_offset;
        si->sh_size = addr - si->sh_addr;
        if (s->sh_type != SHT_NOBITS) {
            Section **ps = &si->sec;
            while (*ps)
                ps = &(*ps)->prev;
            *ps = s, s->prev = NULL;
            si->data_size = si->sh_size;
        }
        //printf("%08x %05x %08x %s\n", si->sh_addr, si->sh_size, si->pe_flags, s->name);
    }
    tcc_free(section_order);
#if 0
    for (i = 1; i < pe->s1->nb_sections; ++i) {
        Section *s = pe->s1->sections[i];
        int type = s->sh_type;
        int flags = s->sh_flags;
        printf("section %-16s %-10s %08x %04x %s,%s,%s\n",
            s->name,
            type == SHT_PROGBITS ? "progbits" :
            type == SHT_NOBITS ? "nobits" :
            type == SHT_SYMTAB ? "symtab" :
            type == SHT_STRTAB ? "strtab" :
            type == SHT_RELX ? "rel" : "???",
            s->sh_addr,
            s->data_offset,
            flags & SHF_ALLOC ? "alloc" : "",
            flags & SHF_WRITE ? "write" : "",
            flags & SHF_EXECINSTR ? "exec" : ""
            );
    }
    pe->s1->verbose = 2;
#endif
    return 0;
}

/*----------------------------------------------------------------------------*/

static int pe_isafunc(TCCState *s1, int sym_index)
{
    Section *sr = text_section->reloc;
    ElfW_Rel *rel, *rel_end;
    ElfW(Addr)info = ELFW(R_INFO)(sym_index, R_XXX_FUNCCALL);
#ifdef R_XXX_FUNCCALL2
    ElfW(Addr)info2 = ELFW(R_INFO)(sym_index, R_XXX_FUNCCALL2);
#endif
    if (!sr)
        return 0;
    rel_end = (ElfW_Rel *)(sr->data + sr->data_offset);
    for (rel = (ElfW_Rel *)sr->data; rel < rel_end; rel++) {
        if (rel->r_info == info)
            return 1;
#ifdef R_XXX_FUNCCALL2
        if (rel->r_info == info2)
            return 1;
#endif
    }
    return 0;
}

/*----------------------------------------------------------------------------*/
static int pe_check_symbols(struct pe_info *pe)
{
    ElfW(Sym) *sym;
    int sym_index, sym_end;
    int ret = 0;
    TCCState *s1 = pe->s1;

    pe_align_section(text_section, 8);

    sym_end = symtab_section->data_offset / sizeof(ElfW(Sym));
    for (sym_index = 1; sym_index < sym_end; ++sym_index) {

        sym = (ElfW(Sym) *)symtab_section->data + sym_index;
        if (sym->st_shndx == SHN_UNDEF) {

			
            const char *name = (char*)symtab_section->link->data + sym->st_name;
            unsigned type = ELFW(ST_TYPE)(sym->st_info);
            int imp_sym = pe_find_import(pe->s1, sym);
            struct import_symbol *is;

			if (imp_sym <= 0)
			{
				//Cheat Engine Symbol Lookup Addition Start
				//Check if CE knows it
				if (s1->symbol_lookup_func)
				{
					void *addr = s1->symbol_lookup_func(s1->symbol_lookup_data, name);
					if (addr)
					{
						tcc_add_symbol(s1, name, addr);						

						imp_sym = pe_find_import(pe->s1, sym);
						if (imp_sym < 0)
						{
							//ugh						
							
							set_global_sym(s1, name, NULL, (addr_t)(uintptr_t)addr);						

							imp_sym = pe_find_import(pe->s1, sym);
						}
					}
				}

				sym->st_other |= ST_PE_IMPORT;
				//Cheat Engine Symbol Lookup Addition Start				
				

				if (imp_sym <= 0)
					goto not_found;
			}

            if (type == STT_NOTYPE) {
                /* symbols from assembler have no type, find out which */
                if (pe_isafunc(s1, sym_index))
                    type = STT_FUNC;
                else
                    type = STT_OBJECT;
            }

            is = pe_add_import(pe, imp_sym);

            if (type == STT_FUNC) {
                unsigned long offset = is->thk_offset;
                if (offset) {
                    /* got aliased symbol, like stricmp and _stricmp */
                } else {
                    char buffer[100];
                    unsigned char *p;

                    /* add a helper symbol, will be patched later in
                       pe_build_imports */
                    sprintf(buffer, "IAT.%s", name);
                    is->iat_index = put_elf_sym(
                        symtab_section, 0, sizeof(DWORD),
                        ELFW(ST_INFO)(STB_GLOBAL, STT_OBJECT),
                        0, SHN_UNDEF, buffer);

                    offset = text_section->data_offset;
                    is->thk_offset = offset;

                    /* add the 'jmp IAT[x]' instruction */
#ifdef TCC_TARGET_ARM
                    p = section_ptr_add(text_section, 8+4); // room for code and address
                    write32le(p + 0, 0xE59FC000); // arm code ldr ip, [pc] ; PC+8+0 = 0001xxxx
                    write32le(p + 4, 0xE59CF000); // arm code ldr pc, [ip]
                    put_elf_reloc(symtab_section, text_section,
                        offset + 8, R_XXX_THUNKFIX, is->iat_index); // offset to IAT position
#else
                    p = section_ptr_add(text_section, 8);
                    write16le(p, 0x25FF);
#ifdef TCC_TARGET_X86_64
                    write32le(p + 2, (DWORD)-4);
#endif
                    put_elf_reloc(symtab_section, text_section, 
                        offset + 2, R_XXX_THUNKFIX, is->iat_index);
#endif
                }
                /* tcc_realloc might have altered sym's address */
                sym = (ElfW(Sym) *)symtab_section->data + sym_index;

                /* patch the original symbol */
                sym->st_value = offset;
                sym->st_shndx = text_section->sh_num;
                sym->st_other &= ~ST_PE_EXPORT; /* do not export */
                continue;
            }

            if (type == STT_OBJECT) { /* data, ptr to that should be */
                if (0 == is->iat_index) {
                    /* original symbol will be patched later in pe_build_imports */
                    is->iat_index = sym_index;
                    continue;
                }
            }

        not_found:
            if (ELFW(ST_BIND)(sym->st_info) == STB_WEAK)
                /* STB_WEAK undefined symbols are accepted */
                continue;
            tcc_error_noabort("undefined symbol '%s'%s", name,
                imp_sym < 0 ? ", missing __declspec(dllimport)?":"");
            ret = -1;

        } else if (pe->s1->rdynamic
                   && ELFW(ST_BIND)(sym->st_info) != STB_LOCAL) {
            /* if -rdynamic option, then export all non local symbols */
            sym->st_other |= ST_PE_EXPORT;
        }
    }
    return ret;
}

/*----------------------------------------------------------------------------*/
#ifdef PE_PRINT_SECTIONS
static void pe_print_section(FILE * f, Section * s)
{
    /* just if you're curious */
    BYTE *p, *e, b;
    int i, n, l, m;
    p = s->data;
    e = s->data + s->data_offset;
    l = e - p;

    fprintf(f, "section  \"%s\"", s->name);
    if (s->link)
        fprintf(f, "\nlink     \"%s\"", s->link->name);
    if (s->reloc)
        fprintf(f, "\nreloc    \"%s\"", s->reloc->name);
    fprintf(f, "\nv_addr   %08X", (unsigned)s->sh_addr);
    fprintf(f, "\ncontents %08X", (unsigned)l);
    fprintf(f, "\n\n");

    if (s->sh_type == SHT_NOBITS)
        return;

    if (0 == l)
        return;

    if (s->sh_type == SHT_SYMTAB)
        m = sizeof(ElfW(Sym));
    else if (s->sh_type == SHT_RELX)
        m = sizeof(ElfW_Rel);
    else
        m = 16;

    fprintf(f, "%-8s", "offset");
    for (i = 0; i < m; ++i)
        fprintf(f, " %02x", i);
    n = 56;

    if (s->sh_type == SHT_SYMTAB || s->sh_type == SHT_RELX) {
        const char *fields1[] = {
            "name",
            "value",
            "size",
            "bind",
            "type",
            "other",
            "shndx",
            NULL
        };

        const char *fields2[] = {
            "offs",
            "type",
            "symb",
            NULL
        };

        const char **p;

        if (s->sh_type == SHT_SYMTAB)
            p = fields1, n = 106;
        else
            p = fields2, n = 58;

        for (i = 0; p[i]; ++i)
            fprintf(f, "%6s", p[i]);
        fprintf(f, "  symbol");
    }

    fprintf(f, "\n");
    for (i = 0; i < n; ++i)
        fprintf(f, "-");
    fprintf(f, "\n");

    for (i = 0; i < l;)
    {
        fprintf(f, "%08X", i);
        for (n = 0; n < m; ++n) {
            if (n + i < l)
                fprintf(f, " %02X", p[i + n]);
            else
                fprintf(f, "   ");
        }

        if (s->sh_type == SHT_SYMTAB) {
            ElfW(Sym) *sym = (ElfW(Sym) *) (p + i);
            const char *name = s->link->data + sym->st_name;
            fprintf(f, "  %04X  %04X  %04X   %02X    %02X    %02X   %04X  \"%s\"",
                    (unsigned)sym->st_name,
                    (unsigned)sym->st_value,
                    (unsigned)sym->st_size,
                    (unsigned)ELFW(ST_BIND)(sym->st_info),
                    (unsigned)ELFW(ST_TYPE)(sym->st_info),
                    (unsigned)sym->st_other,
                    (unsigned)sym->st_shndx,
                    name);

        } else if (s->sh_type == SHT_RELX) {
            ElfW_Rel *rel = (ElfW_Rel *) (p + i);
            ElfW(Sym) *sym =
                (ElfW(Sym) *) s->link->data + ELFW(R_SYM)(rel->r_info);
            const char *name = s->link->link->data + sym->st_name;
            fprintf(f, "  %04X   %02X   %04X  \"%s\"",
                    (unsigned)rel->r_offset,
                    (unsigned)ELFW(R_TYPE)(rel->r_info),
                    (unsigned)ELFW(R_SYM)(rel->r_info),
                    name);
        } else {
            fprintf(f, "   ");
            for (n = 0; n < m; ++n) {
                if (n + i < l) {
                    b = p[i + n];
                    if (b < 32 || b >= 127)
                        b = '.';
                    fprintf(f, "%c", b);
                }
            }
        }
        i += m;
        fprintf(f, "\n");
    }
    fprintf(f, "\n\n");
}

static void pe_print_sections(TCCState *s1, const char *fname)
{
    Section *s;
    FILE *f;
    int i;
    f = fopen(fname, "w");
    for (i = 1; i < s1->nb_sections; ++i) {
        s = s1->sections[i];
        pe_print_section(f, s);
    }
    pe_print_section(f, s1->dynsymtab_section);
    fclose(f);
}
#endif

/* ------------------------------------------------------------- */
/* helper function for load/store to insert one more indirection */

#if defined TCC_TARGET_I386 || defined TCC_TARGET_X86_64
ST_FUNC SValue *pe_getimport(SValue *sv, SValue *v2)
{
    int r2;
    if ((sv->r & (VT_VALMASK|VT_SYM)) != (VT_CONST|VT_SYM) || (sv->r2 != VT_CONST))
        return sv;
    if (!sv->sym->a.dllimport)
        return sv;
    // printf("import %04x %04x %04x %s\n", sv->type.t, sv->sym->type.t, sv->r, get_tok_str(sv->sym->v, NULL));
    memset(v2, 0, sizeof *v2);
    v2->type.t = VT_PTR;
    v2->r = VT_CONST | VT_SYM | VT_LVAL;
    v2->sym = sv->sym;

    r2 = get_reg(RC_INT);
    load(r2, v2);
    v2->r = r2;
    if ((uint32_t)sv->c.i) {
        vpushv(v2);
        vpushi(sv->c.i);
        gen_opi('+');
        *v2 = *vtop--;
    }
    v2->type.t = sv->type.t;
    v2->r |= sv->r & VT_LVAL;
    return v2;
}
#endif

ST_FUNC int pe_putimport(TCCState *s1, int dllindex, const char *name, addr_t value)
{
    return set_elf_sym(
        s1->dynsymtab_section,
        value,
        dllindex, /* st_size */
        ELFW(ST_INFO)(STB_GLOBAL, STT_NOTYPE),
        0,
        value ? SHN_ABS : SHN_UNDEF,
        name
        );
}

static int pe_add_dllref(TCCState *s1, const char *dllname)
{
    int i;
    for (i = 0; i < s1->nb_loaded_dlls; ++i)
        if (0 == strcmp(s1->loaded_dlls[i]->name, dllname))
            return i + 1;
    tcc_add_dllref(s1, dllname);
    return s1->nb_loaded_dlls;
}

/* ------------------------------------------------------------- */

static int read_mem(int fd, unsigned offset, void *buffer, unsigned len)
{
    lseek(fd, offset, SEEK_SET);
    return len == read(fd, buffer, len);
}

/* ------------------------------------------------------------- */

PUB_FUNC int tcc_get_dllexports(const char *filename, char **pp)
{
    int l, i, n, n0, ret;
    char *p;
    int fd;

    IMAGE_SECTION_HEADER ish;
    IMAGE_EXPORT_DIRECTORY ied;
    IMAGE_DOS_HEADER dh;
    IMAGE_FILE_HEADER ih;
    DWORD sig, ref, addr, ptr, namep;

    int pef_hdroffset, opt_hdroffset, sec_hdroffset;

    n = n0 = 0;
    p = NULL;
    ret = -1;

    fd = open(filename, O_RDONLY | O_BINARY);
    if (fd < 0)
        goto the_end_1;
    ret = 1;
    if (!read_mem(fd, 0, &dh, sizeof dh))
        goto the_end;
    if (!read_mem(fd, dh.e_lfanew, &sig, sizeof sig))
        goto the_end;
    if (sig != 0x00004550)
        goto the_end;
    pef_hdroffset = dh.e_lfanew + sizeof sig;
    if (!read_mem(fd, pef_hdroffset, &ih, sizeof ih))
        goto the_end;
    opt_hdroffset = pef_hdroffset + sizeof ih;
    if (ih.Machine == 0x014C) {
        IMAGE_OPTIONAL_HEADER32 oh;
        sec_hdroffset = opt_hdroffset + sizeof oh;
        if (!read_mem(fd, opt_hdroffset, &oh, sizeof oh))
            goto the_end;
        if (IMAGE_DIRECTORY_ENTRY_EXPORT >= oh.NumberOfRvaAndSizes)
            goto the_end_0;
        addr = oh.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
    } else if (ih.Machine == 0x8664) {
        IMAGE_OPTIONAL_HEADER64 oh;
        sec_hdroffset = opt_hdroffset + sizeof oh;
        if (!read_mem(fd, opt_hdroffset, &oh, sizeof oh))
            goto the_end;
        if (IMAGE_DIRECTORY_ENTRY_EXPORT >= oh.NumberOfRvaAndSizes)
            goto the_end_0;
        addr = oh.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
    } else
        goto the_end;

    //printf("addr: %08x\n", addr);
    for (i = 0; i < ih.NumberOfSections; ++i) {
        if (!read_mem(fd, sec_hdroffset + i * sizeof ish, &ish, sizeof ish))
            goto the_end;
        //printf("vaddr: %08x\n", ish.VirtualAddress);
        if (addr >= ish.VirtualAddress && addr < ish.VirtualAddress + ish.SizeOfRawData)
            goto found;
    }
    goto the_end_0;

found:
    ref = ish.VirtualAddress - ish.PointerToRawData;
    if (!read_mem(fd, addr - ref, &ied, sizeof ied))
        goto the_end;

    namep = ied.AddressOfNames - ref;
    for (i = 0; i < ied.NumberOfNames; ++i) {
        if (!read_mem(fd, namep, &ptr, sizeof ptr))
            goto the_end;
        namep += sizeof ptr;
        for (l = 0;;) {
            if (n+1 >= n0)
                p = tcc_realloc(p, n0 = n0 ? n0 * 2 : 256);
            if (!read_mem(fd, ptr - ref + l++, p + n, 1)) {
                tcc_free(p), p = NULL;
                goto the_end;
            }
            if (p[n++] == 0)
                break;
        }
    }
    if (p)
        p[n] = 0;
the_end_0:
    ret = 0;
the_end:
    close(fd);
the_end_1:
    *pp = p;
    return ret;
}

/* -------------------------------------------------------------
 *  This is for compiled windows resources in 'coff' format
 *  as generated by 'windres.exe -O coff ...'.
 */

static int pe_load_res(TCCState *s1, int fd)
{
    struct pe_rsrc_header hdr;
    Section *rsrc_section;
    int i, ret = -1, sym_index;
    BYTE *ptr;
    unsigned offs;

    if (!read_mem(fd, 0, &hdr, sizeof hdr))
        goto quit;

    if (hdr.filehdr.Machine != IMAGE_FILE_MACHINE
        || hdr.filehdr.NumberOfSections != 1
        || strcmp((char*)hdr.sectionhdr.Name, ".rsrc") != 0)
        goto quit;

    rsrc_section = new_section(s1, ".rsrc", SHT_PROGBITS, SHF_ALLOC);
    ptr = section_ptr_add(rsrc_section, hdr.sectionhdr.SizeOfRawData);
    offs = hdr.sectionhdr.PointerToRawData;
    if (!read_mem(fd, offs, ptr, hdr.sectionhdr.SizeOfRawData))
        goto quit;
    offs = hdr.sectionhdr.PointerToRelocations;
    sym_index = put_elf_sym(symtab_section, 0, 0, 0, 0, rsrc_section->sh_num, ".rsrc");
    for (i = 0; i < hdr.sectionhdr.NumberOfRelocations; ++i) {
        struct pe_rsrc_reloc rel;
        if (!read_mem(fd, offs, &rel, sizeof rel))
            goto quit;
        // printf("rsrc_reloc: %x %x %x\n", rel.offset, rel.size, rel.type);
        if (rel.type != RSRC_RELTYPE)
            goto quit;
        put_elf_reloc(symtab_section, rsrc_section,
            rel.offset, R_XXX_RELATIVE, sym_index);
        offs += sizeof rel;
    }
    ret = 0;
quit:
    return ret;
}

/* ------------------------------------------------------------- */

static char *trimfront(char *p)
{
    while (*p && (unsigned char)*p <= ' ')
	++p;
    return p;
}

static char *trimback(char *a, char *e)
{
    while (e > a && (unsigned char)e[-1] <= ' ')
	--e;
    *e = 0;;
    return a;
}

/* ------------------------------------------------------------- */
static int pe_load_def(TCCState *s1, int fd)
{
    int state = 0, ret = -1, dllindex = 0, ord;
    char line[400], dllname[80], *p, *x;
    FILE *fp;

    fp = fdopen(dup(fd), "rb");
    while (fgets(line, sizeof line, fp))
    {
        p = trimfront(trimback(line, strchr(line, 0)));
        if (0 == *p || ';' == *p)
            continue;

        switch (state) {
        case 0:
            if (0 != strnicmp(p, "LIBRARY", 7))
                goto quit;
            pstrcpy(dllname, sizeof dllname, trimfront(p+7));
            ++state;
            continue;

        case 1:
            if (0 != stricmp(p, "EXPORTS"))
                goto quit;
            ++state;
            continue;

        case 2:
            dllindex = pe_add_dllref(s1, dllname);
            ++state;
            /* fall through */
        default:
            /* get ordinal and will store in sym->st_value */
            ord = 0;
            x = strchr(p, ' ');
            if (x) {
                *x = 0, x = strrchr(x + 1, '@');
                if (x) {
                    char *d;
                    ord = (int)strtol(x + 1, &d, 10);
                    if (*d)
                        ord = 0;
                }
            }
            pe_putimport(s1, dllindex, p, ord);
            continue;
        }
    }
    ret = 0;
quit:
    fclose(fp);
    return ret;
}

/* ------------------------------------------------------------- */
static int pe_load_dll(TCCState *s1, const char *filename)
{
    char *p, *q;
    int index, ret;

    ret = tcc_get_dllexports(filename, &p);
    if (ret) {
        return -1;
    } else if (p) {
        index = pe_add_dllref(s1, filename);
        for (q = p; *q; q += 1 + strlen(q))
            pe_putimport(s1, index, q, 0);
        tcc_free(p);
    }
    return 0;
}

/* ------------------------------------------------------------- */
ST_FUNC int pe_load_file(struct TCCState *s1, int fd, const char *filename)
{
    int ret = -1;
    char buf[10];
    if (0 == strcmp(tcc_fileextension(filename), ".def"))
        ret = pe_load_def(s1, fd);
    else if (pe_load_res(s1, fd) == 0)
        ret = 0;
    else if (read_mem(fd, 0, buf, 4) && 0 == memcmp(buf, "MZ", 2))
        ret = pe_load_dll(s1, filename);
    return ret;
}

/* ------------------------------------------------------------- */
#ifdef TCC_TARGET_X86_64
static unsigned pe_add_uwwind_info(TCCState *s1)
{
    if (NULL == s1->uw_pdata) {
        s1->uw_pdata = find_section(s1, ".pdata");
        s1->uw_pdata->sh_addralign = 4;
    }
    if (0 == s1->uw_sym)
        s1->uw_sym = put_elf_sym(symtab_section, 0, 0, 0, 0, text_section->sh_num, ".uw_base");
    if (0 == s1->uw_offs) {
        /* As our functions all have the same stackframe, we use one entry for all */
        static const unsigned char uw_info[] = {
            0x01, // UBYTE: 3 Version , UBYTE: 5 Flags
            0x04, // UBYTE Size of prolog
            0x02, // UBYTE Count of unwind codes
            0x05, // UBYTE: 4 Frame Register (rbp), UBYTE: 4 Frame Register offset (scaled)
            // USHORT * n Unwind codes array
            // 0x0b, 0x01, 0xff, 0xff, // stack size
            0x04, 0x03, // set frame ptr (mov rsp -> rbp)
            0x01, 0x50  // push reg (rbp)
        };

        Section *s = text_section;
        unsigned char *p;

        section_ptr_add(s, -s->data_offset & 3); /* align */
        s1->uw_offs = s->data_offset;
        p = section_ptr_add(s, sizeof uw_info);
        memcpy(p, uw_info, sizeof uw_info);
    }

    return s1->uw_offs;
}

ST_FUNC void pe_add_unwind_data(unsigned start, unsigned end, unsigned stack)
{
    TCCState *s1 = tcc_state;
    Section *pd;
    unsigned o, n, d;
    struct /* _RUNTIME_FUNCTION */ {
      DWORD BeginAddress;
      DWORD EndAddress;
      DWORD UnwindData;
    } *p;

    d = pe_add_uwwind_info(s1);
    pd = s1->uw_pdata;
    o = pd->data_offset;
    p = section_ptr_add(pd, sizeof *p);

    /* record this function */
    p->BeginAddress = start;
    p->EndAddress = end;
    p->UnwindData = d;

    /* put relocations on it */
    for (n = o + sizeof *p; o < n; o += sizeof p->BeginAddress)
        put_elf_reloc(symtab_section, pd, o, R_XXX_RELATIVE, s1->uw_sym);
}
#endif
/* ------------------------------------------------------------- */
#ifdef TCC_TARGET_X86_64
#define PE_STDSYM(n,s) n
#else
#define PE_STDSYM(n,s) "_" n s
#endif

static void tcc_add_support(TCCState *s1, const char *filename)
{
    if (tcc_add_dll(s1, filename, 0) < 0)
        tcc_error_noabort("%s not found", filename);
}

static void pe_add_runtime(TCCState *s1, struct pe_info *pe)
{
    const char *start_symbol;
    int pe_type;

    if (TCC_OUTPUT_DLL == s1->output_type) {
        pe_type = PE_DLL;
        start_symbol = PE_STDSYM("__dllstart","@12");
    } else {
        const char *run_symbol;
        if (find_elf_sym(symtab_section, PE_STDSYM("WinMain","@16"))) {
            start_symbol = "__winstart";
            run_symbol = "__runwinmain";
            pe_type = PE_GUI;
        } else if (find_elf_sym(symtab_section, PE_STDSYM("wWinMain","@16"))) {
            start_symbol = "__wwinstart";
            run_symbol = "__runwwinmain";
            pe_type = PE_GUI;
        } else if (find_elf_sym(symtab_section, "wmain")) {
            start_symbol = "__wstart";
            run_symbol = "__runwmain";
            pe_type = PE_EXE;
        } else {
            start_symbol =  "__start";
            run_symbol = "__runmain";
            pe_type = PE_EXE;

        }
        if (TCC_OUTPUT_MEMORY == s1->output_type)
            start_symbol = run_symbol;
    }

    pe->start_symbol = start_symbol + 1;
    if (!s1->leading_underscore || strchr(start_symbol, '@'))
        ++start_symbol;

#ifdef CONFIG_TCC_BACKTRACE
    if (s1->do_backtrace) {
#ifdef CONFIG_TCC_BCHECK
        if (s1->do_bounds_check && s1->output_type != TCC_OUTPUT_DLL)
            tcc_add_support(s1, "bcheck.o");
#endif
        if (s1->output_type == TCC_OUTPUT_EXE)
            tcc_add_support(s1, "bt-exe.o");
        if (s1->output_type == TCC_OUTPUT_DLL)
            tcc_add_support(s1, "bt-dll.o");
        if (s1->output_type != TCC_OUTPUT_DLL)
            tcc_add_support(s1, "bt-log.o");
        if (s1->output_type != TCC_OUTPUT_MEMORY)
            tcc_add_btstub(s1);
    }
#endif

    /* grab the startup code from libtcc1.a */
#ifdef TCC_IS_NATIVE
    if (TCC_OUTPUT_MEMORY != s1->output_type || s1->runtime_main)
#endif
    set_global_sym(s1, start_symbol, NULL, 0);

    if (0 == s1->nostdlib) {
        static const char *libs[] = {
            "msvcrt", "kernel32", "", "user32", "gdi32", NULL
        };
        const char **pp, *p;
        if (strlen(TCC_LIBTCC1) > 0)
            tcc_add_support(s1, TCC_LIBTCC1);
        for (pp = libs; 0 != (p = *pp); ++pp) {
            if (*p)
                tcc_add_library_err(s1, p);
            else if (PE_DLL != pe_type && PE_GUI != pe_type)
                break;
        }
    }

    /* need this for 'tccelf.c:relocate_section()' */
    if (TCC_OUTPUT_DLL == s1->output_type)
        s1->output_type = TCC_OUTPUT_EXE;
    if (TCC_OUTPUT_MEMORY == s1->output_type)
        pe_type = PE_RUN;
    pe->type = pe_type;
}

static void pe_set_options(TCCState * s1, struct pe_info *pe)
{
    if (PE_DLL == pe->type) {
        /* XXX: check if is correct for arm-pe target */
        pe->imagebase = 0x10000000;
    } else {
#if defined(TCC_TARGET_ARM)
        pe->imagebase = 0x00010000;
#else
        pe->imagebase = 0x00400000;
#endif
    }

#if defined(TCC_TARGET_ARM)
    /* we use "console" subsystem by default */
    pe->subsystem = 9;
#else
    if (PE_DLL == pe->type || PE_GUI == pe->type)
        pe->subsystem = 2;
    else
        pe->subsystem = 3;
#endif
    /* Allow override via -Wl,-subsystem=... option */
    if (s1->pe_subsystem != 0)
        pe->subsystem = s1->pe_subsystem;

    /* set default file/section alignment */
    if (pe->subsystem == 1) {
        pe->section_align = 0x20;
        pe->file_align = 0x20;
    } else {
        pe->section_align = 0x1000;
        pe->file_align = 0x200;
    }

    if (s1->section_align != 0)
        pe->section_align = s1->section_align;
    if (s1->pe_file_align != 0)
        pe->file_align = s1->pe_file_align;

    if ((pe->subsystem >= 10) && (pe->subsystem <= 12))
        pe->imagebase = 0;

    if (s1->has_text_addr)
        pe->imagebase = s1->text_addr;
}

ST_FUNC int pe_output_file(TCCState *s1, const char *filename)
{
    int ret;
    struct pe_info pe;
    int i;

    memset(&pe, 0, sizeof pe);
    pe.filename = filename;
    pe.s1 = s1;
    s1->filetype = 0;

#ifdef CONFIG_TCC_BCHECK
    tcc_add_bcheck(s1);
#endif
    tcc_add_pragma_libs(s1);
    pe_add_runtime(s1, &pe);
    resolve_common_syms(s1);
    pe_set_options(s1, &pe);

    ret = pe_check_symbols(&pe);
    if (ret)
        ;
    else if (filename) {
        pe_assign_addresses(&pe);
        relocate_syms(s1, s1->symtab, 0);
        s1->pe_imagebase = pe.imagebase;
        for (i = 1; i < s1->nb_sections; ++i) {
            Section *s = s1->sections[i];
            if (s->reloc) {
                relocate_section(s1, s);
            }
        }
        pe.start_addr = (DWORD)
            (get_sym_addr(s1, pe.start_symbol, 1, 1) - pe.imagebase);
        if (s1->nb_errors)
            ret = -1;
        else
            ret = pe_write(&pe);
        dynarray_reset(&pe.sec_info, &pe.sec_count);
    } else {
#ifdef TCC_IS_NATIVE
        pe.thunk = data_section;
        pe_build_imports(&pe);
        s1->runtime_main = pe.start_symbol;
#ifdef TCC_TARGET_X86_64
        s1->uw_pdata = find_section(s1, ".pdata");
#endif
#endif
    }

    pe_free_imports(&pe);

#ifdef PE_PRINT_SECTIONS
    pe_print_sections(s1, "tcc.log");
#endif
    return ret;
}

/* ------------------------------------------------------------- */

