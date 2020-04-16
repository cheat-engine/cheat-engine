#ifndef REALMODEEMU_H_
#define REALMODEEMU_H_

#include "vmmhelper.h"

typedef enum {Register8Bit=0, Register16Bit=1, Register32Bit=2, RegisterSreg=3 } _registerType;

typedef struct
{
  unsigned RM         :3;
  unsigned RegOpcode  :3;
  unsigned MOD        :2;
} __attribute__((__packed__)) MODRM,*PMODRM;

typedef struct
{
  unsigned Base       :3;
  unsigned Index      :3;
  unsigned Scale      :2;
} __attribute__((__packed__)) SIB,*PSIB;



typedef struct
{
  int error;
  int opperandsize; //0=16 bit, 1=32-bit, 2=64-bit
  int addresssize;
  int rep, repne;
  UINT64 segmentbase;
  UINT64 address;
  int lock;
  unsigned char *instruction;
  int opperand;
  int opperand2;
  signed int size;
} __attribute__((__packed__)) INSTRUCTIONDATA, *PINSTRUCTIONDATA;

int emulateRMinterrupt(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, int intnr);
BOOL emulateRealMode(pcpuinfo currentcpuinfo, VMRegisters *vmregisters);

#endif /*REALMODEEMU_H_*/
