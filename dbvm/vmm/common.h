#ifndef COMMON_H_
#define COMMON_H_

#include <stddef.h>


//#define DELAYEDSERIAL

#define AMDNP  //enable AMD nested paging support

#ifdef DELAYEDSERIAL
extern int useserial;
#endif

#define STATISTICS

//#define TSCHOOK

#define MAX_STACK_SIZE 0x10000

#if (defined SERIALPORT) && (SERIALPORT != 0)
  #define DEBUG //comment for release
  #define DEBUGINTHANDLER //comment for release
  #define CHECKAPICID
#endif

#if (DISPLAYDEBUG==1)
  #define DEBUG
  #define DEBUGINTHANDLER
  #define CHECKAPICID
#endif



#define ULTIMAPDEBUG //for debugging ultimap (I seem to have misplaced my serial port...)

#define EXIT_FAILURE 0xffffffff
#define EXIT_SUCCESS 0

#define INT_MAX __INT_MAX__
#define INT_MIN (-INT_MAX - 1)
#define SHRT_MAX __SHRT_MAX__
#define SCHAR_MAX __SCHAR_MAX__
#define UCHAR_MAX (SCHAR_MAX * 2 + 1)


#define BYTE unsigned char
#define WORD unsigned short int
#define ULONG unsigned int
#define DWORD unsigned int
#define UINT32 unsigned int
#define UINT64 unsigned long long
#define QWORD UINT64
#ifndef NULL
#define NULL (void*)0
#endif

typedef int VMSTATUS;
#define VM_OK 0
#define VM_ERROR 1


typedef int BOOL;

#define TRUE 1
#define FALSE 0

#define UNUSED __attribute__((unused))

typedef struct{
  QWORD RIP;
  QWORD RBP;
  QWORD RBX;
  QWORD R12;
  QWORD R13;
  QWORD R14;
  QWORD R15;
  QWORD RFLAGS;
  QWORD RSP;
} _jmp_buf;

typedef _jmp_buf volatile jmp_buf[1]; //array holding one item, RSP

extern void longjmp(jmp_buf env, int val);
extern int setjmp(jmp_buf env);

#define try { int lastexception; jmp_buf previousexception; previousexception[0]=getcpuinfo()->OnException[0]; if ((lastexception=setjmp(getcpuinfo()->OnException))==0) {
#define except } else { QWORD UNUSED ExceptionRIP=getcpuinfo()->LastExceptionRIP;
#define tryend } getcpuinfo()->OnException[0]=previousexception[0]; }

typedef volatile struct _criticalSection
{
  volatile int locked;
  volatile int apicid;
  volatile int lockcount;
  char *name;
  int debuglevel;
#ifdef DEBUG
  int ignorelock;
#endif
} criticalSection, *PcriticalSection;


typedef volatile struct _multireadexclusivewritesychronizer
{
  volatile int lock;
  volatile int readers;
  volatile int writers; //max 1

} multireadexclusivewritesychronizer, *Pmultireadexclusivewritesychronizer;

typedef struct _stacklistentry{
  struct _stacklistentry *previous;
  void *data;
} StackListEntry, *PStackListEntry;

typedef struct _stacklist {
  PStackListEntry last;
} StackList, *PStackList;

void push(PStackList stackobject, void *data, int size);
int pop(PStackList stackobject, void *data, int size);


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
    unsigned reserved5  :11; // 22-63
    unsigned reserved6  :32; // 22-63
  }__attribute__((__packed__));
} __attribute__((__packed__)) RFLAGS,*PRFLAGS;



//extern UINT64 inportb(UINT64 port);
//extern void outportb(UINT64 port, UINT64 value);

void bochsbp(void);
void jtagbp(void);

/* Input a byte from a port */
unsigned char inportb(unsigned int port);

void outportb(unsigned int port,unsigned char value);

unsigned long inportd(unsigned int port);

void outportd(unsigned int port,unsigned long value);

int abs(int j);

extern void clearScreen(void);
extern void debugbreak(void);

extern void _cpuid(UINT64 *rax, UINT64 *rbx, UINT64 *rcx, UINT64 *rdx);
extern ULONG getRSP(void);
extern ULONG getRBP(void);

int itoa(unsigned int value,int base, char *output,int maxsize);
int lltoa(unsigned long long value,int base, char *output,int maxsize);
//int atoi(const char *nptr);
unsigned long long atoi2(char* input, int base, int *err);
unsigned long long int strtoull(const char *nptr, char **endptr, int base);

void zeromemory(volatile void *address, unsigned int size);
void printchar(char c, int x, int y, char foreground, char background);
void printstring(char *s, int x, int y, char foreground, char background);
void sendchar(char c);

extern void enableserial(void);



size_t strspn(const char *str, const char *chars);
int isalpha(int c);
int isdigit(int c);
int isalnum(int c);
int isspace(int c);
int iscntrl(int c);
int toupper(int c);
int tolower(int c);
int isprint(int c);

int isgraph(int c);
int islower(int c);
int ispunct(int c);
int isupper(int c);
int islower(int c);
int isxdigit(int c);



QWORD minq(QWORD x,QWORD y);
QWORD maxq(QWORD x,QWORD y);
int min(int x,int y);
int max(int x,int y);


double floor(double x);
double ceil(double x);
double pow(double x, double y);
double sqrt(double x);
double frexp(double x, int *exp);

double fmod(double a, double b);

int strcoll(const char *s1, const char *s2);


//#ifdef DEBUG
  void sendstring(char *s);
  void sendstringf(char *string, ...);
#ifdef DEBUG
  void sendstringf_nolock(char *string UNUSED, ...); //debug only
#endif

  int sprintf(char *str, const char *format, ...);
  int snprintf(char *str, size_t size, const char *format, ...);


  char *strchr(const char *s, int c);

  char *addCharToString(char c, char* string, int lastpos, int *stringsize);

//#endif

/*
#ifndef DEBUG
  #define sendstringf(s,x...)
  #define sendstring(s)
#endif
*/


void exit(int status);
void abort(void);

void setCursorPos(unsigned char x, unsigned char y);
int getchar(void);
char waitforchar(void);
int readstring(char *s, int minlength, int maxlength);
int readstringc(char *s, int minlength, int maxlength);
size_t strlen(const char *s);
char *strcat(char *dest, const char *src);
char *strncat(char *dest, const char *src, size_t n);
char *strcpy(char *dest, const char *src);
char *strncpy(char *dest, const char *src, size_t n);

volatile void* copymem(volatile void *dest, volatile const void *src, size_t size);
void *memcpy(void *dest, const void *src, size_t n);
void *memset(void *s, int c, size_t n);
int memcmp(const void *s1, const void *s2, size_t n);
int strcmp(const char *s1, const char *s2);
int strncmp(const char *s1, const char *s2, size_t n);
char *strstr(const char *haystack, const char *needle);
size_t strcspn(const char *s, const char *reject);

char *strpbrk(const char *s, const char *accept);
double strtod(const char *nptr, char **endptr);

void *memchr(const void *s, int c, size_t n);

unsigned int getAPICID(void);
unsigned int generateCRC(unsigned char *ptr, int size);

void appendzero(char *string, int wantedsize,int maxstringsize);

void displayline(char *s, ...);
int vbuildstring(char *str, int size, char *string, __builtin_va_list arglist);




void sendDissectedFlags(PRFLAGS rflags);


void csEnter(PcriticalSection CS);
void csLeave(PcriticalSection CS);

#ifdef DEBUG
extern QWORD spinlocktimeout;
#endif
int spinlock(volatile int *CS); //returns 0

void lockedQwordIncrement(volatile QWORD *address, QWORD inccount);

void resync(void);

typedef struct textvideo {
  char character;
  char foregroundcolor : 4;
  char backgroundcolor : 4;
} TEXTVIDEO, *PTEXTVIDEO;

typedef TEXTVIDEO TEXTVIDEOLINE[80];

unsigned char nosendchar[256];

int emergencyOutputOnly;
int emergencyOutputLevel;
unsigned char emergencyOutputAPICID;



int currentdisplayline;
int currentdisplayrow;


typedef struct _ARD {
//Address Range Descriptor Structure
        unsigned int BaseAddrLow;
        unsigned int BaseAddrHigh;
        unsigned int LengthLow;
        unsigned int LengthHigh;
        unsigned int Type;
} __attribute__((__packed__)) ARD, *PARD;


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
        unsigned RW        :  1; // read/writedisplaydebugbackwardslog
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
        unsigned PFN       : 28; // page-frame number
        unsigned reserved  : 23;
        unsigned EXB       :  1;
} __attribute__((__packed__)) _PTE_PAE, *PPTE_PAE;

typedef volatile struct _PDE_PAE
{
        unsigned P         :  1; // 0: present (1 = present)
        unsigned RW        :  1; // 1: read/write
        unsigned US        :  1; // 2: user/supervisor
        unsigned PWT       :  1; // 3: page-level write-through
        unsigned PCD       :  1; // 4: page-level cache disabled
        unsigned A         :  1; // 5: accessed
        unsigned D         :  1; // 6: dirty
        unsigned PS        :  1; // 7: pagesize
        unsigned G         :  1; // 8: reserved (0)
        unsigned A1        :  1; // 9: available 1 aka copy-on-write
        unsigned A2        :  1; // 10: available 2/ is 1 when paged to disk
        unsigned A3        :  1; // 11: available 3
        unsigned PFN       : 28; // page-frame number
        unsigned reserved4 : 23;
        unsigned EXB       :  1;
} __attribute__((__packed__)) _PDE_PAE, *PPDE_PAE;

typedef volatile struct _PDE2MB_PAE
{
        unsigned P         :  1; // 0: present (1 = present)
        unsigned RW        :  1; // 1: read/write
        unsigned US        :  1; // 2: user/supervisor
        unsigned PWT       :  1; // 3: page-level write-through
        unsigned PCD       :  1; // 4: page-level cache disabled
        unsigned A         :  1; // 5: accessed
        unsigned reserved1 :  1; // 6: reserved (0)
        unsigned PS        :  1; // 7: reserved (0)
        unsigned reserved3 :  1; // 8: reserved (0)
        unsigned A1        :  1; // 9: available 1 aka copy-on-write
        unsigned A2        :  1; // 10: available 2/ is 1 when paged to disk
        unsigned A3        :  1; // 11: available 3
        unsigned PAT       :  1; //
        unsigned PFN       : 27; // page-frame number (>> 13 instead of >>12);
        unsigned reserved4 : 23;
        unsigned EXB       :  1;
} __attribute__((__packed__)) _PDE2MB_PAE, *PPDE2MB_PAE;

typedef volatile struct _PDPTE_PAE
{
        unsigned P         :  1; // 0: present (1 = present)
        unsigned RW        :  1; // 1: Read Write
        unsigned US        :  1; // 2: User supervisor
        unsigned PWT       :  1; // 3: page-level write-through
        unsigned PCD       :  1; // 4: page-level cache disabled
        unsigned reserved2 :  4; // 5-8: reserved
        unsigned A1        :  1; // 9: available 1 aka copy-on-write
        unsigned A2        :  1; // 10: available 2/ is 1 when paged to disk
        unsigned A3        :  1; // 11: available 3
        unsigned PFN       : 28; // page-frame number
        unsigned reserved3 : 23;
        unsigned EXB       :  1;
} __attribute__((__packed__)) *PPDPTE_PAE;

typedef struct _PPDPTE_PAE_BS
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // Read Write
        unsigned US        :  1; // User supervisor
        unsigned PWT       :  1; // page-level write-through
        unsigned PCD       :  1; // page-level cache disabled
        unsigned A         :  1;
        unsigned D         :  1;
        unsigned PS        :  1;
        unsigned G         :  1;
        unsigned A1        :  1; // available 1 aka copy-on-write
        unsigned A2        :  1; // available 2/ is 1 when paged to disk
        unsigned A3        :  1; // available 3
        unsigned PFN       : 24; // page-frame number
        unsigned reserved3 : 28;
} __attribute__((__packed__)) _PDPTE_PAE_BS, *PPDPTE_PAE_BS;

typedef volatile struct _PPML4
{
        unsigned P         :  1; // 0: present (1 = present)
        unsigned RW        :  1; // 1: Read Write
        unsigned US        :  1; // 2: User supervisor
        unsigned PWT       :  1; // 3: page-level write-through
        unsigned PCD       :  1; // 4: page-level cache disabled
        unsigned reserved2 :  4; // 5-8: reserved
        unsigned A1        :  1; // 9: available 1 aka copy-on-write
        unsigned A2        :  1; // 10: available 2/ is 1 when paged to disk
        unsigned A3        :  1; // 11: available 3
        unsigned PFN       : 28; // page-frame number
        unsigned reserved3 : 23;
        unsigned EXB       :  1;
} __attribute__((__packed__)) *PPML4;


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
  unsigned NotSystem          : 1;
  unsigned DPL                : 2;
  unsigned P                  : 1;
  unsigned Limit16_19         : 4;
  unsigned AVL                : 1;
  unsigned L                  : 1; //long mode
  unsigned B_D                : 1;
  unsigned G                  : 1;
  unsigned Base24_31          : 8;
} __attribute__((__packed__)) GDT_ENTRY, *PGDT_ENTRY; //64-bit is not used (and would mess with the >> 3 anyhow)

typedef struct tagINT_VECTOR
{
  WORD  wLowOffset;
  WORD  wSelector;
  BYTE  bUnused; //3 bits of bUnused describe the IST
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


typedef struct _INVPCIDDESCRIPTOR
{
  unsigned PCID:12;
  QWORD zero:52;
  QWORD LinearAddress;
} __attribute__((__packed__)) INVPCIDDESCRIPTOR, *PINVPCIDDESCRIPTOR;

typedef struct _INVEPTDESCRIPTOR
{
  QWORD EPTPointer;
  QWORD Zero;
} __attribute__((__packed__)) INVEPTDESCRIPTOR, *PINVEPTDESCRIPTOR;

typedef struct _INVVPIDDESCRIPTOR
{
  unsigned VPID:12;
  QWORD zero:52;
  QWORD LinearAddress;
} __attribute__((__packed__)) INVVPIDDESCRIPTOR, *PINVVPIDDESCRIPTOR;

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

extern QWORD textmemory; //points to physical address 0x0b8000

typedef int (*POPCNT_IMPLEMENTATION)(QWORD val);
extern POPCNT_IMPLEMENTATION popcnt;

extern int getcpunr();
int call32bit(DWORD address);
extern int call32bita(DWORD address, DWORD stackaddress);

#if DISPLAYDEBUG
void initialize_displaydebuglogs();
#endif

DWORD getGDTENTRYBase(PGDT_ENTRY entry);
void setGDTENTRYBase(PGDT_ENTRY entry, DWORD base);

GDT_ENTRY Build16BitDataSegmentDescriptor(DWORD baseaddress, DWORD size);
GDT_ENTRY Build16BitCodeSegmentDescriptor(DWORD baseaddress, DWORD size);
GDT_ENTRY Build32BitDataSegmentDescriptor(DWORD baseaddress, DWORD size);
GDT_ENTRY Build32BitCodeSegmentDescriptor(DWORD baseaddress, DWORD size);

int getCPUCount();

void InitCommon();

#include <asm-generic/errno-base.h>
int errno; //todo: implement this


#endif
