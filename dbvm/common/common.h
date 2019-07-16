#ifndef COMMON_H_
#define COMMON_H_

#ifndef SERIALPORT
  #define SERIALPORT 0
#endif

#define BYTE unsigned char
#define WORD unsigned short int
#define ULONG unsigned int
#define DWORD unsigned int
#define UINT64 unsigned long long
#define QWORD unsigned long long
#define NULL 0

typedef struct _criticalSection
{
  int locked;
  int apicid;
  int lockcount;

} criticalSection, *PcriticalSection;

extern int debugzeromem;

extern char inportb(short int port) __attribute__((stdcall));
extern void outportb(short int port, char value) __attribute__((stdcall));
extern void _cpuid(ULONG *eax, ULONG *ebx, ULONG *ecx, ULONG *edx) __attribute__((stdcall));
extern ULONG getESP(void) __attribute__((stdcall));

int itoa(unsigned int value,int base, char *output,int maxsize);
unsigned int atoi(char* input, int base, int *err);

void zeromemory(void *address, unsigned int size);
void printchar(char c, int x, int y, char foreground, char background);
void printstring(char *s, int x, int y, char foreground, char background);
void sendchar(char c);
void sendstring(char *s);
char getchar(void);
char waitforchar(void);
int readstring(char *s, int minlength, int maxlength);
unsigned int strlen(char *string);
char* strcat(char *dest, char *src);
char* strcpy(char *dest, char *src);
void copymem(void *dest, void *src,int size);
int getAPICID(void);
int generateCRC(unsigned char *ptr, int size);

void appendzero(char *string, int wantedsize,int maxstringsize);

int vbuildstring(char *str, int size, char *string, __builtin_va_list arglist);
void displayline(char *s, ...);
void sendstringf(char *string, ...);
void showstate(void);

void csEnter(PcriticalSection CS);
void csLeave(PcriticalSection CS);

void spinlock(int *CS);


void waitforkeypress(void);


typedef struct textvideo {
  char character; 
  char foregroundcolor : 4;
  char backgroundcolor : 4;
} TEXTVIDEO, *PTEXTVIDEO;

unsigned char nosendchar[256];

int currentdisplayline;
int currentdisplayrow;



typedef struct _ARD {   
//Address Range Descriptor Structure
        unsigned int BaseAddrLow;
        unsigned int BaseAddrHigh;
        unsigned int LengthLow;
        unsigned int LengthHigh;
        unsigned int Type;
} *PARD;


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
} __attribute__((__packed__)) *PPTE;

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
} __attribute__((__packed__)) *PPDE;

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
} __attribute__((__packed__)) *PPDE2MB;



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
        unsigned PFN       : 24; // page-frame number
        unsigned reserved  : 28;
} __attribute__((__packed__)) *PPTE_PAE;

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
        unsigned PFN       : 24; // page-frame number
        unsigned reserved4 : 28;
} __attribute__((__packed__)) *PPDE_PAE;

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
        unsigned PFN       : 23; // page-frame number (>> 13 instead of >>12);
        unsigned reserved4 : 28;
} __attribute__((__packed__)) *PPDE2MB_PAE;


typedef struct _PDPTE_PAE
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // Read Write
        unsigned US        :  1; // User supervisor                
        unsigned PWT       :  1; // page-level write-through
        unsigned PCD       :  1; // page-level cache disabled
        unsigned reserved2 :  4; // reserved
        unsigned A1        :  1; // available 1 aka copy-on-write
        unsigned A2        :  1; // available 2/ is 1 when paged to disk
        unsigned A3        :  1; // available 3
        unsigned PFN       : 24; // page-frame number
        unsigned reserved3 : 28;
} __attribute__((__packed__)) *PPDPTE_PAE;


typedef struct _TSS
{
        ULONG Previous_Task_Link  : 16;
        ULONG Reserved1       : 16;
        ULONG ESP0;
        ULONG SS0             : 16;
        ULONG Reserved2       : 16;
        ULONG ESP1;
        ULONG SS1             : 16;
        ULONG Reserved3       : 16;
        ULONG ESP2;
        ULONG SS2             : 16;
        ULONG Reserved4       : 16;
        ULONG CR3;
        ULONG EIP;
        ULONG EFLAGS;
        ULONG EAX;
        ULONG ECX;
        ULONG EDX;
        ULONG EBX;
        ULONG ESP;
        ULONG EBP;
        ULONG ESI;
        ULONG EDI;
        ULONG ES              : 16;
        ULONG Reserved5       : 16;
        ULONG CS              : 16;
        ULONG Reserved6       : 16;
        ULONG SS              : 16;
        ULONG Reserved7       : 16;
        ULONG DS              : 16;
        ULONG Reserved8       : 16;
        ULONG FS              : 16;
        ULONG Reserved9       : 16;
        ULONG GS              : 16;
        ULONG Reserved10      : 16;
        ULONG LDTss           : 16;
        ULONG Reserved11      : 16;
        ULONG Trap            : 1;
        ULONG Reserved12      : 15;
        ULONG IOBASE          : 16;
} __attribute__((__packed__)) TSS ,*PTSS;

typedef struct
{
  unsigned Limit0_15          : 16;
  unsigned Base0_23           : 24;  
  unsigned Type               : 4;
  unsigned System             : 1;
  unsigned DPL                : 2;
  unsigned P                  : 1;  
  unsigned Limit16_19         : 4;
  unsigned AVL                : 1;
  unsigned Reserved           : 1;
  unsigned B_D                : 1;
  unsigned G                  : 1;
  unsigned Base24_31          : 8;  
} __attribute__((__packed__)) GDT_ENTRY, *PGDT_ENTRY;

typedef struct tagINT_VECTOR
{
  WORD  wLowOffset;
  WORD  wSelector;
  BYTE  bUnused;
  BYTE  bAccess;  
  WORD  wHighOffset;
} INT_VECTOR, *PINT_VECTOR;

#pragma pack(2) //allignment of 2 bytes
typedef struct tagIDT
{    
  WORD wLimit;
  PINT_VECTOR vector;
} IDT, *PIDT;
#pragma pack()


typedef struct tagEFLAGS
{
	unsigned CF			:1; // 0
	unsigned reserved1	:1; // 1
	unsigned PF			:1; // 2
	unsigned reserved2	:1; // 3
	unsigned AF			:1; // 4
	unsigned reserved3	:1; // 5
	unsigned ZF			:1; // 6
	unsigned SF			:1; // 7
	unsigned TF			:1; // 8
	unsigned IF			:1; // 9
	unsigned DF			:1; // 10
	unsigned OF			:1; // 11
	unsigned IOPL		:2; // 12+13
	unsigned NT			:1; // 14
	unsigned reserved4	:1; // 15
	unsigned RF			:1; // 16
	unsigned VM			:1; // 17
	unsigned AC			:1; // 18
	unsigned VIF		:1; // 19
	unsigned VIP		:1; // 20
	unsigned ID			:1; // 21
	unsigned reserved5	:10; // 22-31
} __attribute__((__packed__)) EFLAGS,*PEFLAGS;


   


#endif
