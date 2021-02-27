/*
 * vmcallstructs.h
 *
 *  Created on: Mar 12, 2018
 *      Author: erich
 */

#ifndef VMM_VMCALLSTRUCTS_H_
#define VMM_VMCALLSTRUCTS_H_


typedef struct
{
  DWORD size;
  DWORD password2;
  DWORD command;
} __attribute__((__packed__))  VMCALL_BASIC, *PVMCALL_BASIC;

typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD sourcePA; //physical address to read
  DWORD bytesToRead; //number of bytes to read
  QWORD destinationVA; //virtual address to write to
  DWORD nopagefault; //if not 0 stop at the first pagefault
} __attribute__((__packed__)) VMCALL_READPHYSICALMEMORY, *PVMCALL_READPHYSICALMEMORY;

typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD destinationPA; //physical address to read
  DWORD bytesToWrite; //number of bytes to read
  QWORD sourceVA; //virtual address to write to
  DWORD nopagefault; //if not 0 stop at the first pagefault
} __attribute__((__packed__)) VMCALL_WRITEPHYSICALMEMORY, *PVMCALL_WRITEPHYSICALMEMORY;



typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD PhysicalAddress;
  QWORD OptionalField1; //usermode loop address for DBVMBP
  QWORD OptionalField2; //kernelmode loop address for DBVMBP
  int Size;
  int Options; //binary.
               //  Bit 0: 0=Log RIP once. 1=Log RIP multiple times (when different registers)
               //  Bit 1: 0=Only log given Physical Address. 1=Log everything in the page(s) that is/are affected
               //  Bit 2: 0=Do not save FPU/XMM data, 1=Also save FPU/XMM data
               //  Bit 3: 0=Do not save a stack snapshot, 1=Save stack snapshot
               //  Bit 4: 0=This CPU only.  1=All CPU's
               //  Bit 5: 1=Interrupt mode. Don't log, just interrupt

  int MaxEntryCount; //DBVM will allocate this buffer
  int ID; //ID describing this watcher for this CPU (keep track of this on a per cpu basis if you do more than 1 and don't specify all cpu's)
} __attribute__((__packed__)) VMCALL_WATCH_PARAM, *PVMCALL_WATCH_PARAM;

typedef struct
{
  VMCALL_BASIC vmcall;
  DWORD ID;
} __attribute__((__packed__)) VMCALL_WATCH_DISABLE_PARAM, *PVMCALL_WATCH_DISABLE_PARAM;


typedef struct
{
  VMCALL_BASIC vmcall;
  DWORD ID;
  QWORD results; //virtual address receiving log
  DWORD resultsize;
  DWORD copied; //the number of bytes copied so far (This is a repeating instruction)
} __attribute__((__packed__)) VMCALL_WATCH_RETRIEVELOG_PARAM, *PVMCALL_WATCH_RETRIEVELOG_PARAM;


typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD results; //virtual address receiving log
  DWORD resultsize;
  DWORD copied; //the number of bytes copied so far (This is a repeating instruction)
} __attribute__((__packed__)) VMCALL_TRACEONBP_RETRIEVELOG_PARAM, *PVMCALL_TRACEONBP_RETRIEVELOG_PARAM;




typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD physicalAddress;
  QWORD mode;
} __attribute__((__packed__)) VMCALL_CLOAK_ACTIVATE_PARAM, *PVMCALL_CLOAK_ACTIVATE_PARAM,
                              VMCALL_CLOAK_DEACTIVATE_PARAM, *PVMCALL_CLOAK_DEACTIVATE_PARAM;

typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD physicalAddress;
  DWORD size;
  DWORD whitelist_flags; //0=no whitelist...why?, 1=ip range whitelist, 2=cr3 whilelist, 3=both ip+cr3 whitelist
  QWORD whitelist_ipfromrange;
  QWORD whitelist_iptorange;
  QWORD whitelist_cr3;
} __attribute__((__packed__)) VMCALL_CLOAKEX_ACTIVATE_PARAM, *PVMCALL_CLOAKEX_ACTIVATE_PARAM;

typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD physicalAddress;
  QWORD destination;
} __attribute__((__packed__)) VMCALL_CLOAK_READ_PARAM, *PVMCALL_CLOAK_READ_PARAM;

typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD physicalAddress;
  QWORD source;
} __attribute__((__packed__)) VMCALL_CLOAK_WRITE_PARAM, *PVMCALL_CLOAK_WRITE_PARAM;

typedef struct
{
  struct
  {
    unsigned changeRAX : 1;
    unsigned changeRBX : 1;
    unsigned changeRCX : 1;
    unsigned changeRDX : 1;
    unsigned changeRSI : 1;
    unsigned changeRDI : 1;
    unsigned changeRBP : 1;
    unsigned changeRSP : 1;
    unsigned changeRIP : 1;
    unsigned changeR8  : 1;
    unsigned changeR9  : 1;
    unsigned changeR10 : 1;
    unsigned changeR11 : 1;
    unsigned changeR12 : 1;
    unsigned changeR13 : 1;
    unsigned changeR14 : 1;
    unsigned changeR15 : 1;
    //flags reg:
    unsigned changeCF : 1;
    unsigned changePF : 1;
    unsigned changeAF : 1;
    unsigned changeZF : 1;
    unsigned changeSF : 1;
    unsigned changeOF : 1;
    unsigned newCF : 1;
    unsigned newPF : 1;
    unsigned newAF : 1;
    unsigned newZF : 1;
    unsigned newSF : 1;
    unsigned newOF : 1;
    unsigned reserved : 3;
  } Flags;

  QWORD changeXMM; //16 nibbles, each bit is one dword
  QWORD changeFP;
  QWORD newRAX;
  QWORD newRBX;
  QWORD newRCX;
  QWORD newRDX;
  QWORD newRSI;
  QWORD newRDI;
  QWORD newRBP;
  QWORD newRSP;
  QWORD newRIP;
  QWORD newR8;
  QWORD newR9;
  QWORD newR10;
  QWORD newR11;
  QWORD newR12;
  QWORD newR13;
  QWORD newR14;
  QWORD newR15;

  QWORD newFP0;
  QWORD newFP0_H;
  QWORD newFP1;
  QWORD newFP1_H;
  QWORD newFP2;
  QWORD newFP2_H;
  QWORD newFP3;
  QWORD newFP3_H;
  QWORD newFP4;
  QWORD newFP4_H;
  QWORD newFP5;
  QWORD newFP5_H;
  QWORD newFP6;
  QWORD newFP6_H;
  QWORD newFP7;
  QWORD newFP7_H;
  QWORD newXMM0;
  QWORD newXMM0_H;
  QWORD newXMM1;
  QWORD newXMM1_H;
  QWORD newXMM2;
  QWORD newXMM2_H;
  QWORD newXMM3;
  QWORD newXMM3_H;
  QWORD newXMM4;
  QWORD newXMM4_H;
  QWORD newXMM5;
  QWORD newXMM5_H;
  QWORD newXMM6;
  QWORD newXMM6_H;
  QWORD newXMM7;
  QWORD newXMM7_H;
  QWORD newXMM8;
  QWORD newXMM8_H;
  QWORD newXMM9;
  QWORD newXMM9_H;
  QWORD newXMM10;
  QWORD newXMM10_H;
  QWORD newXMM11;
  QWORD newXMM11_H;
  QWORD newXMM12;
  QWORD newXMM12_H;
  QWORD newXMM13;
  QWORD newXMM13_H;
  QWORD newXMM14;
  QWORD newXMM14_H;
  QWORD newXMM15;
  QWORD newXMM15_H;

} __attribute__((__packed__)) CHANGEREGONBPINFO, *PCHANGEREGONBPINFO;

typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD physicalAddress;
  CHANGEREGONBPINFO changereginfo;
} __attribute__((__packed__)) VMCALL_CLOAK_CHANGEREG_PARAM, *PVMCALL_CLOAK_CHANGEREG_PARAM;



typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD physicalAddress;
  DWORD flags;
  //bit 0: log fpu
  //bit 1: log stack (say goodbye to your memory)
  //bit todo: 1=hide stepping from pushf
  //bit todo: 1=follow interrupts, 0=do not follow interrupts
  DWORD tracecount;
} __attribute__((__packed__)) VMCALL_CLOAK_TRACEONBP_PARAM, *PVMCALL_CLOAK_TRACEONBP_PARAM;



typedef struct
{
  VMCALL_BASIC vmcall;
  DWORD count;
  DWORD maxcount;
} __attribute__((__packed__)) VMCALL_CLOAK_TRACEONBP_GETSTATUS_PARAM, *PVMCALL_CLOAK_TRACEONBP_GETSTATUS_PARAM;


typedef struct
{
  VMCALL_BASIC vmcall;
  int force;
} __attribute__((__packed__)) VMCALL_CLOAK_TRACEONBP_REMOVE_PARAM, *PVMCALL_CLOAK_TRACEONBP_REMOVE_PARAM;

typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD results;
  QWORD resultsize;
  QWORD copied;
} __attribute__((__packed__)) VMCALL_CLOAK_TRACEONBP_READLOG_PARAM, *PVMCALL_CLOAK_TRACEONBP_READLOG_PARAM;





typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD physicalAddress;
} __attribute__((__packed__)) VMCALL_CLOAK_REMOVECHANGEREG_PARAM, *PVMCALL_CLOAK_REMOVECHANGEREG_PARAM;


typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD physicalAddress;
  QWORD list;
  int listsize;
} __attribute__((__packed__)) VMCALL_WATCH_GETLIST_PARAM, *PVMCALL_WATCH_GETLIST_PARAM;

typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD destination;
} __attribute__((__packed__)) VMCALL_LOGCR3_STOP_PARAM, *PVMCALL_LOGCR3_STOP_PARAM;


typedef struct
{
  VMCALL_BASIC vmcall;
  int type; //0=pre, 1=post


  QWORD virtualAddress;
  unsigned int bytesize;
  QWORD internalAddress; //caller must set this to 0
  unsigned int bytescopied;

} __attribute__((__packed__)) VMCALL_REGISTER_PLUGIN_PARAM, *PVMCALL_REGISTER_PLUGIN_PARAM;

typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD PhysicalPages[0];
} __attribute__((__packed__)) VMCALL_ADD_MEMORY_PARAM, *PVMCALL_ADD_MEMORY_PARAM;

#ifdef STATISTICS
typedef struct
{
  VMCALL_BASIC vmcall;
  int eventcounter[56];
  int globaleventcounter[56];
} __attribute__((__packed__)) VMCALL_GET_STATISTICS_PARAM, *PVMCALL_GET_STATISTICS_PARAM;
#endif



typedef struct
{
  VMCALL_BASIC vmcall;
  int enabled;
  int timeout;
} __attribute__((__packed__)) VMCALL_SETTSCADJUST_PARAM, *PVMCALL_SETTSCADJUST_PARAM;

typedef struct
{
  VMCALL_BASIC vmcall;
  double speedhackspeed;
} __attribute__((__packed__)) VMCALL_SETSPEEDHACK_PARAM, *PVMCALL_SETSPEEDHACK_PARAM;


#endif /* VMM_VMCALLSTRUCTS_H_ */
