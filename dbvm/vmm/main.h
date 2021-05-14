#ifndef MAIN_H_
#define MAIN_H_

#include "common.h"
#include "vmmhelper.h"



void reboot(int skipAPTermination);
void apentryvmx();


void startvmx(pcpuinfo currentcpuinfo);
void CheckCRCValues(void);

extern void vmcall_amd(void);
extern void vmcall_intel(void);

extern void *vmcall_instr; //holds a pointer to either vmcall_amd or vmcall_intel
extern int vmcalltest_asm(void);
extern int vmcall_setintredirects(void);
extern QWORD _vmcall(void* data);

extern void _pause(void);
extern UINT64 _vmread(ULONG index);
extern int _vmread2(ULONG index, UINT64 *result);
extern void _vmwrite(ULONG index,UINT64 value);
extern int _vmwrite2(ULONG index, UINT64 result);
extern int _vmclear(unsigned long long address);
extern int _vmptrld(unsigned long long address);
extern int _vmxon(unsigned long long address);
extern int _vmlaunch(void);
extern void _vmresume(void);
extern void _vmxoff(void);
extern void cLIDT(void *idtloader);
extern unsigned long long readMSR(ULONG msr);
extern void writeMSR(ULONG msr, UINT64 newvalue);
extern void _xsetbv(ULONG xcr, UINT64 value);
extern int stopautomation(void);
extern int hascpuid(void);
extern UINT64 getCR0(void);
extern UINT64 getCR2(void);
extern UINT64 getCR3(void);
extern UINT64 getCR4(void);
extern UINT64 getCR8(void);
extern UINT64 getDR0(void);
extern UINT64 setDR0(UINT64 newdr0);
extern UINT64 getDR1(void);
extern UINT64 setDR1(UINT64 newdr0);
extern UINT64 getDR2(void);
extern UINT64 setDR2(UINT64 newdr0);
extern UINT64 getDR3(void);
extern UINT64 setDR3(UINT64 newdr0);
extern UINT64 getDR6(void);
extern UINT64 setDR6(UINT64 newdr6);
extern UINT64 getDR7(void);
extern UINT64 setDR7(UINT64 newdr7);
extern void setIDT(UINT64 base, WORD size);
extern void setGDT(UINT64 base, WORD size);
extern UINT64 getIDTbase(void);
extern UINT64 getGDTbase(void);
extern WORD getIDTsize(void);
extern WORD getGDTsize(void);

//minor mistake fix and I hate renaming the function
#define getGDTlimit getGDTsize

extern UINT64 getRFLAGS(void);
extern void setRFLAGS(UINT64 rflags);
extern void loadTaskRegister(ULONG selector);
extern WORD getTaskRegister(void);
extern ULONG setCR0(UINT64 newcr0);
extern ULONG setCR2(UINT64 newcr2);
extern ULONG setCR3(UINT64 newcr3);
extern ULONG setCR4(UINT64 newcr4);
extern ULONG setCR8(UINT64 newcr8);
extern void _invlpg(UINT64 address);
extern void _invpcid(int type, PINVPCIDDESCRIPTOR datablock);
extern void _invept(int type, PINVEPTDESCRIPTOR datablock);
extern int _invept2(int type, PINVEPTDESCRIPTOR datablock);
extern void _invvpid(int type, PINVVPIDDESCRIPTOR datablock);
extern int _invvpid2(int type, PINVVPIDDESCRIPTOR datablock);
extern void _wbinvd(void);
extern void _invd(void);


extern UINT64 _rdtsc(void);
extern void quickboot(void);
extern void infloop(void);

void *idttable32;
void *jumptable;
extern void virtual8086_start(void);
extern int realmodetest;
extern int moveto32bitstart;
extern int virtual8086entry32bit;
extern int inthandler_32;
extern int real16;
extern int realmode;
extern int movetoreal;
extern int movetoreal_end;
extern int bochswaitforsipiloop;
extern UINT64 loadedOS;
PTSS mainTSS;

int vmxloop(pcpuinfo currentcpuinfo, UINT64 *eaxbase);
int vmxloop_amd(pcpuinfo currentcpuinfo, UINT64 vmcb_pa, UINT64 *eaxbase);

extern int vmxstartup_end;

extern unsigned long long IA32_APIC_BASE;
extern unsigned long long APIC_ID;
extern unsigned long long APIC_SVR;

extern int cpu_stepping;
extern int cpu_model;
extern int cpu_familyID;
extern int cpu_type;
extern int cpu_ext_modelID;
extern int cpu_ext_familyID;

extern int testcode(int i,int i2, int i3, int i4);

extern void changetask(void);
extern void tasktest(void);
extern void int3bptest(void);


volatile void       *RealmodeRing0Stack;
volatile PTSS64     ownTSS;
volatile PTSS       VirtualMachineTSS_V8086;
unsigned char *ffpage;
PPDE_PAE   ffpagedir;
PPTE_PAE   ffpagetable;

volatile void       *GDT_BASE;
int GDT_SIZE;
int IDT_SIZE;

void menu(void);

//filled in by vmm.map parser
QWORD        Password1; //edx : if the upper bits of the field is filled in, this will disable access for 32-bit targets
ULONG        Password2; //memory
QWORD        Password3; //ecx  ^ see password1


extern QWORD dbvmversion;

//crc checksums
unsigned int originalVMMcrc;


int isAMD;
int AMD_hasDecodeAssists;
int AMD_hasNRIPS;

extern int autostart;
extern int IntHandlerDebug;
volatile int NMIcount;

extern pcpuinfo firstcpuinfo;

#define vmclear _vmclear
#define vmptrld _vmptrld
#define vmxon _vmxon
#define vmlaunch _vmlaunch
#define vmresume _vmxresume
#define vmxoff _vmxoff
#define vmread _vmread
#define vmwrite _vmwrite
#define vmread2 _vmread2
#define vmwrite2 _vmwrite2

void menu(void);

#endif /*MAIN_H_*/
