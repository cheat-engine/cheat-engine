#ifndef VMXOFFLOAD_H
#define VMXOFFLOAD_H

void cleanupDBVM();
void initializeDBVM(PCWSTR dbvmimgpath);
void vmxoffload(void);

void vmxoffload_override(CCHAR cpunr, PKDEFERRED_ROUTINE Dpc, PVOID DeferredContext, PVOID *SystemArgument1, PVOID *SystemArgument2);

VOID vmxoffload_dpc(
	__in struct _KDPC *Dpc,
	__in_opt PVOID DeferredContext,
	__in_opt PVOID SystemArgument1,
	__in_opt PVOID SystemArgument2
	);

typedef struct _DBVMOffloadMemInfo
{
	UINT64 *List;
	int Count;
} DBVMOffloadMemInfo, *PDBVMOffloadMemInfo;

#pragma pack (1)
typedef struct _PTE
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // read/write
        unsigned US        :  1; // user/supervisor
        unsigned PWT       :  1; // page-level write-through
        unsigned PCD       :  1; // page-level cache disabled
        unsigned A         :  1; // accessed
        unsigned D         :  1; // dirty
        unsigned PAT       :  1; // PAT
        unsigned G         :  1; // global page
        unsigned A1        :  1; // available 1 aka copy-on-write
        unsigned A2        :  1; // available 2/ is 1 when paged to disk
        unsigned A3        :  1; // available 3
        unsigned PFN       : 20; // page-frame number		
} *PPTE;

typedef struct _PDE
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // read/write
        unsigned US        :  1; // user/supervisor
        unsigned PWT       :  1; // page-level write-through
        unsigned PCD       :  1; // page-level cache disabled
        unsigned A         :  1; // accessed
        unsigned D         :  1; // dirty
        unsigned PS        :  1; // reserved (0)
        unsigned G         :  1; // reserved (0)
        unsigned A1        :  1; // available 1 aka copy-on-write
        unsigned A2        :  1; // available 2/ is 1 when paged to disk
        unsigned A3        :  1; // available 3
        unsigned PFN       : 20; // page-frame number
} *PPDE;

typedef struct _PDE2MB
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // read/write
        unsigned US        :  1; // user/supervisor
        unsigned PWT       :  1; // page-level write-through
        unsigned PCD       :  1; // page-level cache disabled
        unsigned A         :  1; // accessed
        unsigned reserved1 :  1; // reserved (0)
        unsigned PS        :  1; // reserved (0)
        unsigned reserved3 :  1; // reserved (0)
        unsigned A1        :  1; // available 1 aka copy-on-write
        unsigned A2        :  1; // available 2/ is 1 when paged to disk
        unsigned A3        :  1; // available 3
        unsigned PFN       : 20; // page-frame number (>> 13 instead of >>12);
} *PPDE2MB;



typedef struct _PTE_PAE
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // read/write
        unsigned US        :  1; // user/supervisor
        unsigned PWT       :  1; // page-level write-through
        unsigned PCD       :  1; // page-level cache disabled
        unsigned A         :  1; // accessed
        unsigned D         :  1; // dirty
        unsigned PAT       :  1; // 
        unsigned G         :  1; // global page
        unsigned A1        :  1; // available 1 aka copy-on-write
        unsigned A2        :  1; // available 2/ is 1 when paged to disk
        unsigned A3        :  1; // available 3
		unsigned PFN_LOW : 20;
		unsigned PFN_HIGH : 32;
} PTE_PAE, *PPTE_PAE;

typedef struct _PDE_PAE
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // read/write
        unsigned US        :  1; // user/supervisor
        unsigned PWT       :  1; // page-level write-through
        unsigned PCD       :  1; // page-level cache disabled
        unsigned A         :  1; // accessed
        unsigned D         :  1; // dirty
        unsigned PS        :  1; // pagesize
        unsigned G         :  1; // reserved (0)
        unsigned A1        :  1; // available 1 aka copy-on-write
        unsigned A2        :  1; // available 2/ is 1 when paged to disk
        unsigned A3        :  1; // available 3
		unsigned PFN_LOW : 20;
		unsigned PFN_HIGH : 32;
} PDE_PAE, *PPDE_PAE;

typedef struct _PDE2MB_PAE
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // read/write
        unsigned US        :  1; // user/supervisor
        unsigned PWT       :  1; // page-level write-through
        unsigned PCD       :  1; // page-level cache disabled
        unsigned A         :  1; // accessed
        unsigned reserved1 :  1; // reserved (0)
        unsigned PS        :  1; // reserved (0)
        unsigned reserved3 :  1; // reserved (0)
        unsigned A1        :  1; // available 1 aka copy-on-write
        unsigned A2        :  1; // available 2/ is 1 when paged to disk
        unsigned A3        :  1; // available 3
        unsigned PAT       :  1; //
		unsigned PFN_LOW : 19;
		unsigned PFN_HIGH : 32;
} *PPDE2MB_PAE;



typedef struct _PDPTE_PAE
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // Read Write
        unsigned US        :  1; // User supervisor                
        unsigned PWT       :  1; // page-level write-through
        unsigned PCD       :  1; // page-level cache disabled
		unsigned reserved0 : 1; // reserved
		unsigned reserved1 : 1; // reserved
		unsigned reserved2 : 1; // reserved
		unsigned reserved3 : 1; // reserved
        unsigned A1        :  1; // available 1 aka copy-on-write
        unsigned A2        :  1; // available 2/ is 1 when paged to disk
        unsigned A3        :  1; // available 3
		unsigned PFN_LOW : 20;
		unsigned PFN_HIGH : 32;
} *PPDPTE_PAE;

#endif
