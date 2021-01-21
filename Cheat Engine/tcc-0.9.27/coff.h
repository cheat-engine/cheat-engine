/**************************************************************************/
/*  COFF.H                                                                */
/*     COFF data structures and related definitions used by the linker    */
/**************************************************************************/

/*------------------------------------------------------------------------*/
/*  COFF FILE HEADER                                                      */
/*------------------------------------------------------------------------*/
struct filehdr {
        unsigned short  f_magic;        /* magic number */
        unsigned short  f_nscns;        /* number of sections */
        long            f_timdat;       /* time & date stamp */
        long            f_symptr;       /* file pointer to symtab */
        long            f_nsyms;        /* number of symtab entries */
        unsigned short  f_opthdr;       /* sizeof(optional hdr) */
        unsigned short  f_flags;        /* flags */
        unsigned short  f_TargetID;     /* for C6x = 0x0099 */
        };

/*------------------------------------------------------------------------*/
/*  File header flags                                                     */
/*------------------------------------------------------------------------*/
#define  F_RELFLG   0x01       /* relocation info stripped from file       */
#define  F_EXEC     0x02       /* file is executable (no unresolved refs)  */
#define  F_LNNO     0x04       /* line numbers stripped from file          */
#define  F_LSYMS    0x08       /* local symbols stripped from file         */
#define  F_GSP10    0x10       /* 34010 version                            */
#define  F_GSP20    0x20       /* 34020 version                            */
#define  F_SWABD    0x40       /* bytes swabbed (in names)                 */
#define  F_AR16WR   0x80       /* byte ordering of an AR16WR (PDP-11)      */
#define  F_LITTLE   0x100      /* byte ordering of an AR32WR (vax)         */
#define  F_BIG      0x200      /* byte ordering of an AR32W (3B, maxi)     */
#define  F_PATCH    0x400      /* contains "patch" list in optional header */
#define  F_NODF     0x400   

#define F_VERSION    (F_GSP10  | F_GSP20)   
#define F_BYTE_ORDER (F_LITTLE | F_BIG)
#define FILHDR  struct filehdr

/* #define FILHSZ  sizeof(FILHDR)  */
#define FILHSZ  22                /* above rounds to align on 4 bytes which causes problems */

#define COFF_C67_MAGIC 0x00c2

/*------------------------------------------------------------------------*/
/*  Macros to recognize magic numbers                                     */
/*------------------------------------------------------------------------*/
#define ISMAGIC(x)      (((unsigned short)(x))==(unsigned short)magic)
#define ISARCHIVE(x)    ((((unsigned short)(x))==(unsigned short)ARTYPE))
#define BADMAGIC(x)     (((unsigned short)(x) & 0x8080) && !ISMAGIC(x))


/*------------------------------------------------------------------------*/
/*  OPTIONAL FILE HEADER                                                  */
/*------------------------------------------------------------------------*/
typedef struct aouthdr {
        short   magic;          /* see magic.h                          */
        short   vstamp;         /* version stamp                        */
        long    tsize;          /* text size in bytes, padded to FW bdry*/
        long    dsize;          /* initialized data "  "                */
        long    bsize;          /* uninitialized data "   "             */
        long    entrypt;        /* entry pt.                            */
        long    text_start;     /* base of text used for this file      */
        long    data_start;     /* base of data used for this file      */
} AOUTHDR;

#define AOUTSZ  sizeof(AOUTHDR)

/*----------------------------------------------------------------------*/
/*      When a UNIX aout header is to be built in the optional header,  */
/*      the following magic numbers can appear in that header:          */ 
/*                                                                      */
/*              AOUT1MAGIC : default : readonly sharable text segment   */
/*              AOUT2MAGIC:          : writable text segment            */
/*              PAGEMAGIC  :         : configured for paging            */
/*----------------------------------------------------------------------*/
#define AOUT1MAGIC 0410
#define AOUT2MAGIC 0407
#define PAGEMAGIC  0413


/*------------------------------------------------------------------------*/
/*  COMMON ARCHIVE FILE STRUCTURES                                        */
/*                                                                        */
/*       ARCHIVE File Organization:                                       */
/*       _______________________________________________                  */
/*       |__________ARCHIVE_MAGIC_STRING_______________|                  */
/*       |__________ARCHIVE_FILE_MEMBER_1______________|                  */
/*       |                                             |                  */
/*       |       Archive File Header "ar_hdr"          |                  */
/*       |.............................................|                  */
/*       |       Member Contents                       |                  */
/*       |               1. External symbol directory  |                  */
/*       |               2. Text file                  |                  */
/*       |_____________________________________________|                  */
/*       |________ARCHIVE_FILE_MEMBER_2________________|                  */
/*       |               "ar_hdr"                      |                  */
/*       |.............................................|                  */
/*       |       Member Contents (.o or text file)     |                  */
/*       |_____________________________________________|                  */
/*       |       .               .               .     |                  */
/*       |       .               .               .     |                  */
/*       |       .               .               .     |                  */
/*       |_____________________________________________|                  */
/*       |________ARCHIVE_FILE_MEMBER_n________________|                  */
/*       |               "ar_hdr"                      |                  */
/*       |.............................................|                  */
/*       |               Member Contents               |                  */
/*       |_____________________________________________|                  */
/*                                                                        */
/*------------------------------------------------------------------------*/

#define COFF_ARMAG   "!<arch>\n"
#define SARMAG  8
#define ARFMAG  "`\n"

struct ar_hdr           /* archive file member header - printable ascii */
{
        char    ar_name[16];    /* file member name - `/' terminated */
        char    ar_date[12];    /* file member date - decimal */
        char    ar_uid[6];      /* file member user id - decimal */
        char    ar_gid[6];      /* file member group id - decimal */
        char    ar_mode[8];     /* file member mode - octal */
        char    ar_size[10];    /* file member size - decimal */
        char    ar_fmag[2];     /* ARFMAG - string to end header */
};


/*------------------------------------------------------------------------*/
/*  SECTION HEADER                                                        */
/*------------------------------------------------------------------------*/
struct scnhdr {
        char            s_name[8];      /* section name */
        long            s_paddr;        /* physical address */
        long            s_vaddr;        /* virtual address */
        long            s_size;         /* section size */
        long            s_scnptr;       /* file ptr to raw data for section */
        long            s_relptr;       /* file ptr to relocation */
        long            s_lnnoptr;      /* file ptr to line numbers */
        unsigned int	s_nreloc;       /* number of relocation entries */
        unsigned int	s_nlnno;        /* number of line number entries */
        unsigned int	s_flags;        /* flags */
		unsigned short	s_reserved;     /* reserved byte */
		unsigned short  s_page;         /* memory page id */
        };

#define SCNHDR  struct scnhdr
#define SCNHSZ  sizeof(SCNHDR)

/*------------------------------------------------------------------------*/
/* Define constants for names of "special" sections                       */
/*------------------------------------------------------------------------*/
/* #define _TEXT    ".text" */
#define _DATA    ".data"
#define _BSS     ".bss"
#define _CINIT   ".cinit"
#define _TV      ".tv"

/*------------------------------------------------------------------------*/
/* The low 4 bits of s_flags is used as a section "type"                  */
/*------------------------------------------------------------------------*/
#define STYP_REG    0x00  /* "regular" : allocated, relocated, loaded */
#define STYP_DSECT  0x01  /* "dummy"   : not allocated, relocated, not loaded */
#define STYP_NOLOAD 0x02  /* "noload"  : allocated, relocated, not loaded */
#define STYP_GROUP  0x04  /* "grouped" : formed of input sections */
#define STYP_PAD    0x08  /* "padding" : not allocated, not relocated, loaded */
#define STYP_COPY   0x10  /* "copy"    : used for C init tables - 
                                                not allocated, relocated,
                                                loaded;  reloc & lineno
                                                entries processed normally */
#define STYP_TEXT   0x20   /* section contains text only */
#define STYP_DATA   0x40   /* section contains data only */
#define STYP_BSS    0x80   /* section contains bss only */

#define STYP_ALIGN  0x100  /* align flag passed by old version assemblers */
#define ALIGN_MASK  0x0F00 /* part of s_flags that is used for align vals */
#define ALIGNSIZE(x) (1 << ((x & ALIGN_MASK) >> 8))


/*------------------------------------------------------------------------*/
/*  RELOCATION ENTRIES                                                    */
/*------------------------------------------------------------------------*/
struct reloc
{
   long            r_vaddr;        /* (virtual) address of reference */
   short           r_symndx;       /* index into symbol table */
   unsigned short  r_disp;         /* additional bits for address calculation */
   unsigned short  r_type;         /* relocation type */
};

#define RELOC   struct reloc
#define RELSZ   10                 /* sizeof(RELOC) */

/*--------------------------------------------------------------------------*/
/*   define all relocation types                                            */
/*--------------------------------------------------------------------------*/

#define R_ABS           0         /* absolute address - no relocation       */
#define R_DIR16         01        /* UNUSED                                 */
#define R_REL16         02        /* UNUSED                                 */
#define R_DIR24         04        /* UNUSED                                 */
#define R_REL24         05        /* 24 bits, direct                        */
#define R_DIR32         06        /* UNUSED                                 */
#define R_RELBYTE      017        /* 8 bits, direct                         */
#define R_RELWORD      020        /* 16 bits, direct                        */
#define R_RELLONG      021        /* 32 bits, direct                        */
#define R_PCRBYTE      022        /* 8 bits, PC-relative                    */
#define R_PCRWORD      023        /* 16 bits, PC-relative                   */
#define R_PCRLONG      024        /* 32 bits, PC-relative                   */
#define R_OCRLONG      030        /* GSP: 32 bits, one's complement direct  */
#define R_GSPPCR16     031        /* GSP: 16 bits, PC relative (in words)   */
#define R_GSPOPR32     032        /* GSP: 32 bits, direct big-endian        */
#define R_PARTLS16     040        /* Brahma: 16 bit offset of 24 bit address*/
#define R_PARTMS8      041        /* Brahma: 8 bit page of 24 bit address   */
#define R_PARTLS7      050        /* DSP: 7 bit offset of 16 bit address    */
#define R_PARTMS9      051        /* DSP: 9 bit page of 16 bit address      */
#define R_REL13        052        /* DSP: 13 bits, direct                   */


/*------------------------------------------------------------------------*/
/*  LINE NUMBER ENTRIES                                                   */
/*------------------------------------------------------------------------*/
struct lineno
{
        union
        {
                long    l_symndx ;      /* sym. table index of function name
                                                iff l_lnno == 0      */
                long    l_paddr ;       /* (physical) address of line number */
        }               l_addr ;
        unsigned short  l_lnno ;        /* line number */
};

#define LINENO  struct lineno
#define LINESZ  6       /* sizeof(LINENO) */


/*------------------------------------------------------------------------*/
/*   STORAGE CLASSES                                                      */
/*------------------------------------------------------------------------*/
#define  C_EFCN          -1    /* physical end of function */
#define  C_NULL          0
#define  C_AUTO          1     /* automatic variable */
#define  C_EXT           2     /* external symbol */
#define  C_STAT          3     /* static */
#define  C_REG           4     /* register variable */
#define  C_EXTDEF        5     /* external definition */
#define  C_LABEL         6     /* label */
#define  C_ULABEL        7     /* undefined label */
#define  C_MOS           8     /* member of structure */
#define  C_ARG           9     /* function argument */
#define  C_STRTAG        10    /* structure tag */
#define  C_MOU           11    /* member of union */
#define  C_UNTAG         12    /* union tag */
#define  C_TPDEF         13    /* type definition */
#define C_USTATIC        14    /* undefined static */
#define  C_ENTAG         15    /* enumeration tag */
#define  C_MOE           16    /* member of enumeration */
#define  C_REGPARM       17    /* register parameter */
#define  C_FIELD         18    /* bit field */

#define  C_BLOCK         100   /* ".bb" or ".eb" */
#define  C_FCN           101   /* ".bf" or ".ef" */
#define  C_EOS           102   /* end of structure */
#define  C_FILE          103   /* file name */
#define  C_LINE          104   /* dummy sclass for line number entry */
#define  C_ALIAS         105   /* duplicate tag */
#define  C_HIDDEN        106   /* special storage class for external */
                               /* symbols in dmert public libraries  */

/*------------------------------------------------------------------------*/
/*  SYMBOL TABLE ENTRIES                                                  */
/*------------------------------------------------------------------------*/

#define  SYMNMLEN   8      /*  Number of characters in a symbol name */
#define  FILNMLEN   14     /*  Number of characters in a file name */
#define  DIMNUM     4      /*  Number of array dimensions in auxiliary entry */


struct syment
{
        union
        {
                char            _n_name[SYMNMLEN];      /* old COFF version */
                struct
                {
                        long    _n_zeroes;      /* new == 0 */
                        long    _n_offset;      /* offset into string table */
                } _n_n;
                char            *_n_nptr[2];    /* allows for overlaying */
        } _n;
        long                    n_value;        /* value of symbol */
        short                   n_scnum;        /* section number */
        unsigned short          n_type;         /* type and derived type */
        char                    n_sclass;       /* storage class */
        char                    n_numaux;       /* number of aux. entries */
};

#define n_name          _n._n_name
#define n_nptr          _n._n_nptr[1]
#define n_zeroes        _n._n_n._n_zeroes
#define n_offset        _n._n_n._n_offset

/*------------------------------------------------------------------------*/
/* Relocatable symbols have a section number of the                       */
/* section in which they are defined.  Otherwise, section                 */
/* numbers have the following meanings:                                   */
/*------------------------------------------------------------------------*/
#define  N_UNDEF  0                     /* undefined symbol */
#define  N_ABS    -1                    /* value of symbol is absolute */
#define  N_DEBUG  -2                    /* special debugging symbol  */
#define  N_TV     (unsigned short)-3    /* needs transfer vector (preload) */
#define  P_TV     (unsigned short)-4    /* needs transfer vector (postload) */


/*------------------------------------------------------------------------*/
/* The fundamental type of a symbol packed into the low                   */
/* 4 bits of the word.                                                    */
/*------------------------------------------------------------------------*/
#define  _EF    ".ef"

#define  T_NULL     0          /* no type info */
#define  T_ARG      1          /* function argument (only used by compiler) */
#define  T_CHAR     2          /* character */
#define  T_SHORT    3          /* short integer */
#define  T_INT      4          /* integer */
#define  T_LONG     5          /* long integer */
#define  T_FLOAT    6          /* floating point */
#define  T_DOUBLE   7          /* double word */
#define  T_STRUCT   8          /* structure  */
#define  T_UNION    9          /* union  */
#define  T_ENUM     10         /* enumeration  */
#define  T_MOE      11         /* member of enumeration */
#define  T_UCHAR    12         /* unsigned character */
#define  T_USHORT   13         /* unsigned short */
#define  T_UINT     14         /* unsigned integer */
#define  T_ULONG    15         /* unsigned long */

/*------------------------------------------------------------------------*/
/* derived types are:                                                     */
/*------------------------------------------------------------------------*/
#define  DT_NON      0          /* no derived type */
#define  DT_PTR      1          /* pointer */
#define  DT_FCN      2          /* function */
#define  DT_ARY      3          /* array */

#define MKTYPE(basic, d1,d2,d3,d4,d5,d6) \
       ((basic) | ((d1) <<  4) | ((d2) <<  6) | ((d3) <<  8) |\
                  ((d4) << 10) | ((d5) << 12) | ((d6) << 14))

/*------------------------------------------------------------------------*/
/* type packing constants and macros                                      */
/*------------------------------------------------------------------------*/
#define  N_BTMASK_COFF     017
#define  N_TMASK_COFF      060
#define  N_TMASK1_COFF     0300
#define  N_TMASK2_COFF     0360
#define  N_BTSHFT_COFF     4
#define  N_TSHIFT_COFF     2

#define  BTYPE_COFF(x)  ((x) & N_BTMASK_COFF)  
#define  ISINT(x)  (((x) >= T_CHAR && (x) <= T_LONG) ||   \
		    ((x) >= T_UCHAR && (x) <= T_ULONG) || (x) == T_ENUM)
#define  ISFLT_COFF(x)  ((x) == T_DOUBLE || (x) == T_FLOAT)
#define  ISPTR_COFF(x)  (((x) & N_TMASK_COFF) == (DT_PTR << N_BTSHFT_COFF)) 
#define  ISFCN_COFF(x)  (((x) & N_TMASK_COFF) == (DT_FCN << N_BTSHFT_COFF))
#define  ISARY_COFF(x)  (((x) & N_TMASK_COFF) == (DT_ARY << N_BTSHFT_COFF))
#define  ISTAG_COFF(x)  ((x)==C_STRTAG || (x)==C_UNTAG || (x)==C_ENTAG)

#define  INCREF_COFF(x) ((((x)&~N_BTMASK_COFF)<<N_TSHIFT_COFF)|(DT_PTR<<N_BTSHFT_COFF)|(x&N_BTMASK_COFF))
#define  DECREF_COFF(x) ((((x)>>N_TSHIFT_COFF)&~N_BTMASK_COFF)|((x)&N_BTMASK_COFF))


/*------------------------------------------------------------------------*/
/*  AUXILIARY SYMBOL ENTRY                                                */
/*------------------------------------------------------------------------*/
union auxent
{
	struct
	{
		long            x_tagndx;       /* str, un, or enum tag indx */
		union
		{
			struct
			{
				unsigned short  x_lnno; /* declaration line number */
				unsigned short  x_size; /* str, union, array size */
			} x_lnsz;
			long    x_fsize;        /* size of function */
		} x_misc;
		union
		{
			struct                  /* if ISFCN, tag, or .bb */
			{
				long    x_lnnoptr;      /* ptr to fcn line # */
				long    x_endndx;       /* entry ndx past block end */
			}       x_fcn;
			struct                  /* if ISARY, up to 4 dimen. */
			{
				unsigned short  x_dimen[DIMNUM];
			}       x_ary;
		}               x_fcnary;
		unsigned short  x_regcount;   /* number of registers used by func */
	}       x_sym;
	struct
	{
		char    x_fname[FILNMLEN];
	}       x_file;
	struct
	{
		long    x_scnlen;          /* section length */
		unsigned short  x_nreloc;  /* number of relocation entries */
		unsigned short  x_nlinno;  /* number of line numbers */
	}       x_scn;
};

#define SYMENT  struct syment
#define SYMESZ  18      /* sizeof(SYMENT) */

#define AUXENT  union auxent
#define AUXESZ  18      /* sizeof(AUXENT) */

/*------------------------------------------------------------------------*/
/*  NAMES OF "SPECIAL" SYMBOLS                                            */
/*------------------------------------------------------------------------*/
#define _STEXT          ".text"
#define _ETEXT          "etext"
#define _SDATA          ".data"
#define _EDATA          "edata"
#define _SBSS           ".bss"
#define _END            "end"
#define _CINITPTR       "cinit"

/*--------------------------------------------------------------------------*/
/*  ENTRY POINT SYMBOLS                                                     */
/*--------------------------------------------------------------------------*/
#define _START          "_start"
#define _MAIN           "_main"
    /*  _CSTART         "_c_int00"          (defined in params.h)  */


#define _TVORIG         "_tvorig"
#define _TORIGIN        "_torigin"
#define _DORIGIN        "_dorigin"

#define _SORIGIN        "_sorigin"
