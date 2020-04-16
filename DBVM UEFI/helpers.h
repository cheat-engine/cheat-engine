/*
 * helpers.h
 *
 *  Created on: Nov 11, 2017
 *      Author: eric
 */

#ifndef HELPERS_H_
#define HELPERS_H_

extern EFI_SYSTEM_TABLE *st;

#pragma pack(2) //alignment of 2 bytes
typedef struct tagGDT
{
  UINT16 wLimit;
  UINT64 vector;
} GDT, *PGDT;
#pragma pack()

#pragma pack(1) //alignment of 1 byte
typedef struct tagINT_VECTOR
{
  UINT16  wLowOffset;
  UINT16  wSelector;
  UINT8  bUnused;
  UINT8    bAccessFlags;

  /*
  unsigned gatetype  : 3; //101=Task, 110=interrupt, 111=trap
  unsigned gatesize  : 1; //1=32bit, 0=16bit
  unsigned zero      : 1;
  unsigned DPL       : 2;
  unsigned P         : 1;
  */
  UINT16  wHighOffset;
  UINT32 TopOffset;
  UINT32 Reserved;

} INT_VECTOR, *PINT_VECTOR;
#pragma pack()

#pragma pack(2) //allignemnt of 2 byte
typedef struct tagIDT
{
  UINT16 wLimit;
  PINT_VECTOR vector;
} IDT, *PIDT;
#pragma pack()

typedef struct
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
  unsigned reserved5  :10; // 22-31
  unsigned reserved6  :8;
  unsigned reserved7  :8;
  unsigned reserved8  :8;
  unsigned reserved9  :8;
} EFLAGS,*PEFLAGS;


extern int testfunction(void);
extern int brk(void);

extern unsigned long long readMSR(int msr);
extern void writeMSR(int msr, unsigned long long value);
VOID *AllocatePersistentMemory(int size);

EFI_STATUS AllocatePages(IN EFI_ALLOCATE_TYPE            Type,  IN EFI_MEMORY_TYPE              MemoryType,  IN UINTN                        NoPages,  OUT EFI_PHYSICAL_ADDRESS        *Memory  );
extern void setCR0(UINT64 newcr0);
extern UINT64 getCR0(void);
extern UINT64 getCR2(void);
extern UINT64 getCR3(void);
extern UINT64 getCR4(void);

extern UINT16 getSS(void);
extern UINT16 getCS(void);
extern UINT16 getDS(void);
extern UINT16 getES(void);
extern UINT16 getFS(void);
extern UINT16 getGS(void);

extern UINT16 getGDT(PGDT gdt);
extern UINT16 getIDT(PIDT idt);
extern UINT16 getLDT(void);
extern UINT16 getTR(void);

extern UINT64 getDR0(void);
extern UINT64 getDR1(void);
extern UINT64 getDR2(void);
extern UINT64 getDR3(void);
extern UINT64 getDR6(void);
extern UINT64 getDR7(void);

extern EFLAGS getEflags(void);

extern UINT64 getRSP(void);
extern UINT64 getRBP(void);
extern UINT64 getRAX(void);
extern UINT64 getRBX(void);
extern UINT64 getRCX(void);
extern UINT64 getRDX(void);
extern UINT64 getRSI(void);
extern UINT64 getRDI(void);

extern UINT64 getR8(void);
extern UINT64 getR9(void);
extern UINT64 getR10(void);
extern UINT64 getR11(void);
extern UINT64 getR12(void);
extern UINT64 getR13(void);
extern UINT64 getR14(void);
extern UINT64 getR15(void);

extern UINT64 getAccessRights(UINT64 segment);
extern UINT64 getSegmentLimit(UINT64 segment);

extern void disableInterrupts(void);
extern void enableInterrupts(void);

extern UINT64 dovmcall(void *vmcallinfo, unsigned int level1pass);
extern void dovmcall2(void *vmcallinfo, unsigned int level1pass, UINT64 *r1, UINT64 *r2);
extern UINT64 getTSC(void);

extern void timeCheck(UINT64 *arr);

extern UINTN cpucount;

#endif /* HELPERS_H_ */
