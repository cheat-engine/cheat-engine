#ifndef COMMON_H_
#define COMMON_H_

#define MAX_STACK_SIZE 0x10000

#if (defined SERIALPORT) && (SERIALPORT != 0)
  #define DEBUG //comment for release
  #define DEBUGINTHANDLER //comment for release
#endif

// #define DISPLAYDEBUG //send serialport debug output to the display
#define ULTIMAPDEBUG //for debugging ultimap (I seem to have misplaced my serial port...)


#define BYTE unsigned char
#define WORD unsigned short int
#define ULONG unsigned int
#define DWORD unsigned int
#define UINT32 unsigned int
#define UINT64 unsigned long long
#define QWORD UINT64
#define NULL 0

typedef volatile struct _criticalSection
{
  int locked;
  int apicid;
  int lockcount;
} criticalSection, *PcriticalSection;

typedef union
{
  UINT64 value;
  struct
  {
    unsigned CF     :1; // 0
    unsigned reserved1  :1; // 1
    unsigned PF     :1; // 2
    unsigned reserved2  :1; // 3
    unsigned AF     :1; // 4
    unsigned reserved3  :1; // 5
    unsigned ZF     :1; // 6
    unsigned SF     :1; // 7
    unsigned TF     :1; // 8
    unsigned IF     :1; // 9
    unsigned DF     :1; // 10
    unsigned OF     :1; // 11
    unsigned IOPL   :2; // 12+13
    unsigned NT     :1; // 14
    unsigned reserved4  :1; // 15
    unsigned RF     :1; // 16
    unsigned VM     :1; // 17
    unsigned AC     :1; // 18
    unsigned VIF    :1; // 19
    unsigned VIP    :1; // 20
    unsigned ID     :1; // 21
    unsigned reserved5  :32; // 22-63
    unsigned reserved6  :10; // 22-63
  };
} __attribute__((__packed__)) RFLAGS,*PRFLAGS;



//extern UINT64 inportb(UINT64 port);
//extern void outportb(UINT64 port, UINT64 value);

inline void bochsbp(void);
inline unsigned char inportb(unsigned int port);
inline void outportb(unsigned int port,unsigned char value);

inline unsigned long inportd(unsigned int port);
inline void outportd(unsigned int port,unsigned long value);

extern void clearScreen(void);


extern void _cpuid(UINT64 *rax, UINT64 *rbx, UINT64 *rcx, UINT64 *rdx);
extern ULONG getRSP(void);

int itoa(unsigned int value,int base, char *output,int maxsize);
unsigned long long atoi(char* input, int base, int *err);

void zeromemory(volatile void *address, unsigned int size);
void printchar(char c, int x, int y, char foreground, char background);
void printstring(char *s, int x, int y, char foreground, char background);
void sendchar(char c);

extern void enableserial(void);


//#ifdef DEBUG
  void sendstring(char *s);
  void sendstringf(char *string, ...);
//#endif

/*
#ifndef DEBUG
  #define sendstringf(s,x...)
  #define sendstring(s)
#endif
*/


void setCursorPos(unsigned char x, unsigned char y);
char getchar(void);
char waitforchar(void);
int readstring(char *s, int minlength, int maxlength);
int readstringc(char *s, int minlength, int maxlength);
int strlen(volatile const char *string);
char *strcat(volatile char *dest, volatile const char *src);
char *strcpy(volatile char *dest, volatile const char *src);
volatile void* copymem(volatile void *dest, volatile const void *src, int size);
void* memcpy(volatile void *dest, volatile const void *src, int size);
void* memset(volatile void *dest, int c, int size);

unsigned int getAPICID(void);
unsigned int generateCRC(unsigned char *ptr, int size);

void appendzero(char *string, int wantedsize,int maxstringsize);

void displayline(char *s, ...);
int vbuildstring(char *str, int size, char *string, __builtin_va_list arglist);

void sendDissectedFlags(PRFLAGS rflags);


void csEnter(PcriticalSection CS);
void csLeave(PcriticalSection CS);

int spinlock(volatile int *CS); //returns 0
void resync(void);

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
} __attribute__((__packed__)) *PARD;


typedef volatile struct _PTE
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

typedef volatile struct _PDE
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

typedef volatile struct _PDE2MB
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



typedef volatile struct _PTE_PAE
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
        unsigned reserved  : 23;
        unsigned EXB       :  1;
} __attribute__((__packed__)) *PPTE_PAE;

typedef volatile struct _PDE_PAE
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
        unsigned PFN       : 28; // page-frame number
        unsigned reserved4 : 23;
        unsigned EXB       :  1;
} __attribute__((__packed__)) *PPDE_PAE;

typedef volatile struct _PDE2MB_PAE
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
        unsigned PFN       : 27; // page-frame number (>> 13 instead of >>12);
        unsigned reserved4 : 23;
        unsigned EXB       :  1;
} __attribute__((__packed__)) *PPDE2MB_PAE;


typedef volatile struct _PDPTE_PAE
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
        unsigned PFN       : 28; // page-frame number
        unsigned reserved3 : 23;
        unsigned EXB       :  1;
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

typedef struct _TSS64
{
        unsigned reserved1    : 32;
        unsigned long long RSP0;
        unsigned long long RSP1;
        unsigned long long RSP2;
        unsigned long long reserved2;

        unsigned long long IST1;
        unsigned long long IST2;
        unsigned long long IST3;
        unsigned long long IST4;
        unsigned long long IST5;
        unsigned long long IST6;
        unsigned long long IST7;
        unsigned long long reserved3;
        unsigned reserved4: 16;
        unsigned IOBASE   : 16;
} __attribute__((__packed__)) TSS64 ,*PTSS64;

typedef struct //for AMD Bits 55:52 and 47:40 of the GDT
{
  union {
    WORD SegmentAttrib;
    struct {
      ULONG Segment_type : 4; //0-3
      ULONG S            : 1; //4
      ULONG DPL          : 2; //5-6
      ULONG P            : 1; //7
      ULONG AVL          : 1; //
      ULONG L            : 1; //
      ULONG D_B          : 1; //
      ULONG G            : 1; //
    };
  };
} Segment_Attribs, *PSegment_Attribs;

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
} __attribute__((__packed__)) GDT_ENTRY, *PGDT_ENTRY; //64-bit is not used (and would mess with the >> 3 anyhow)

typedef struct tagINT_VECTOR
{
  WORD  wLowOffset;
  WORD  wSelector;
  BYTE  bUnused;
  BYTE  bAccess;
  WORD  wHighOffset;
  DWORD Offset32_63;
  DWORD whatever;
} INT_VECTOR, *PINT_VECTOR;

typedef struct tagINT_VECTOR32
{
  WORD  wLowOffset;
  WORD  wSelector;
  BYTE  bUnused;
  BYTE  bAccess;
  WORD  wHighOffset;
} INT_VECTOR32, *PINT_VECTOR32;

#pragma pack(2) //allignment of 2 bytes
typedef struct tagIDT
{
  WORD wLimit;
  PINT_VECTOR vector;
} IDT, *PIDT;
#pragma pack()

/* obsolete, use rflags now
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
} __attribute__((__packed__)) EFLAGS,*PEFLAGS;*/


extern criticalSection sendstringfCS;
extern criticalSection sendstringCS;


#endif
