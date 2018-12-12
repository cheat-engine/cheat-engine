/*
 * vbe3.c
 *
 *  Created on: Mar 2, 2018
 *      Author: eric heijnen
 *
 *      For displaying error messages on the screen and basic menu system in case of fatal error
 *
 *      todo: Font's and stuff
 *
 *      Mar6: Suspend work on this for now, it's too unpredictable with windows' gpu drivers.
 *      The mode gpu mode switches, but the memory seems locked
 *      Perhaps launching windows in VGA mode might work, but not useful for situations to explain to novice users what went wrong
 */

#include "vbe3.h"
#include "mm.h"
#include "main.h"

#define DISPLAYBASE 0xc0000000 //has to be below 0x100000000 as segments can only address 32-bit addresses
#define EMULATEDBIOSBLOCK 0xc0010000
#define STACK16 0xf0000000
#define GRAPHICS_MEMORY 0xd0000000

unsigned char *displaybioscopy=(unsigned char *)DISPLAYBASE;
unsigned char *emulatedBiosBlock=(unsigned char *)EMULATEDBIOSBLOCK;
unsigned char *stack16=(unsigned char *)STACK16;
unsigned char *graphics=(unsigned char *)GRAPHICS_MEMORY;
int GraphicsPageSize;

extern int doVBEINIT(void);

//call functions:
extern int VBEcall_ResetStart();
extern int VBEcall_SaveRestoreState(BYTE operationmode,WORD stateflags, WORD dataselector,  WORD *size);
extern int VBEcall_SetMode(WORD mode, WORD dataselector);
extern int VBEcall_GetModeInfo(WORD mode, WORD dataselector);
extern int VBEcall_GetControllerInfo(WORD dataselector);


int VBE3Initialized=0;

VBE_ModeInfo currentMode;
DWORD currentColor;
int drawPage;
int displayPage;

typedef struct _LONGJMP16
{
  WORD offset;
  WORD cs;
} __attribute__((__packed__)) LONGJMP16;

typedef struct _LONGJMP32
{
  DWORD offset;
  WORD cs;
} __attribute__((__packed__)) LONGJMP32;

typedef struct _LONGJMP64
{
  QWORD offset;
  WORD cs;
} __attribute__((__packed__)) LONGJMP64;

LONGJMP16 vbe_initptr; //just don't use the rex prefix
extern LONGJMP16 vbe_entry;

void *VBEPtrToAddress(DWORD vbeptr)
{
  PGDT_ENTRY currentgdt=(PGDT_ENTRY)getGDTbase();
  GDT_ENTRY e=currentgdt[vbeptr >> 19];

  if (e.P)
  {
    QWORD address=getGDTENTRYBase(&e);
    address+=vbeptr & 0xffff;

    return (void*)address;
  }
  else
    return NULL;
}

int initializeVBE3()
{
  //scan for VBE3
  if (VBE3Initialized)
    return 1;

  int PMInfoBlock=0;
  volatile unsigned char *biossource=(volatile unsigned char *)0xc0000;
  unsigned char *copy=malloc2(65536);

  //map this to DISPLAYBASE
  sendstringf("initializeVBE3:\n");
  sendstringf("Copying memory and scanning for PMID (sizeof(VBE3_PM_InfoBlock)=%d) \n", sizeof(VBE3_PM_InfoBlock));

  int i;
  for (i=0; i<65536; i++)
  {
    PVBE3_PM_InfoBlock b=(PVBE3_PM_InfoBlock)&biossource[i];
    copy[i]=biossource[i];

    if (b->ID==0x44494d50) //'PMID'
    {
      //check the signature
      sendstringf("Found a VBE3 Protected Mode Info Block at %8 ",0xc0000+i);
      unsigned char checksum=0;
      unsigned int j;
      for (j=i; j<i+sizeof(VBE3_PM_InfoBlock); j++)
        checksum+=biossource[j];

      if (checksum==0)
      {
        sendstringf("with a proper checksum\n");
        PMInfoBlock=i;
      }
      else
        sendstringf("with a bad checksum\n");
    }
  }

  //map the copy to DISPLAYBASE
  if (PMInfoBlock)
  {
    mapMemory(displaybioscopy, copy, 65536);
    PVBE3_PM_InfoBlock pmib=(PVBE3_PM_InfoBlock)&displaybioscopy[PMInfoBlock];

    //setup the segment registers
    PGDT_ENTRY currentgdt=(PGDT_ENTRY)getGDTbase();

    if (pmib->entrypoint==0)
    {
      sendstringf("PMInfoBlock has no protected mode entry point\n");
      return 0;
    }

    mapMemory(emulatedBiosBlock, malloc2(4096), 4096);
    zeromemory(emulatedBiosBlock, 4096);

    GDT_ENTRY desc_bios=Build16BitDataSegmentDescriptor(EMULATEDBIOSBLOCK, 4096);
    GDT_ENTRY desc_a0000=Build16BitDataSegmentDescriptor(0xa0000, 65536);
    GDT_ENTRY desc_b0000=Build16BitDataSegmentDescriptor(0xb0000, 65536);
    GDT_ENTRY desc_b8000=Build16BitDataSegmentDescriptor(0xb8000, 32768);
    GDT_ENTRY desc_csdata=Build16BitDataSegmentDescriptor(DISPLAYBASE, 65536);
    GDT_ENTRY desc_cscode=Build16BitCodeSegmentDescriptor(DISPLAYBASE, 65536);

    mapMemory(stack16, malloc2(65536), 65536);
    zeromemory(stack16,65536);
    GDT_ENTRY desc_stack=Build16BitDataSegmentDescriptor(STACK16, 65536);
    GDT_ENTRY desc_stack32=Build32BitDataSegmentDescriptor(STACK16, 65536);
    GDT_ENTRY desc_callsegment=Build32BitCodeSegmentDescriptor((DWORD)(QWORD)doVBEINIT, 65536);
    GDT_ENTRY desc_callsegment16=Build16BitCodeSegmentDescriptor((DWORD)(QWORD)doVBEINIT, 65536);

    GDT_ENTRY desc_genericdata=Build16BitDataSegmentDescriptor(0, 65536);



    currentgdt[0x20]=desc_bios;
    currentgdt[0x21]=desc_a0000;
    currentgdt[0x22]=desc_b0000;
    currentgdt[0x23]=desc_b8000;
    currentgdt[0x24]=desc_csdata;
    currentgdt[0x25]=desc_cscode;
    currentgdt[0x26]=desc_stack;
    currentgdt[0x27]=desc_callsegment;
    currentgdt[0x28]=desc_callsegment16;
    currentgdt[0x29]=desc_genericdata; //selector 148 is a generic segment for data access pointers
    currentgdt[0x30]=desc_stack32;



    pmib->BIOSDataSel=0x20*8;
    pmib->A0000Sel=0x21*8;
    pmib->B0000Sel=0x22*8;
    pmib->B8000Sel=0x23*8;
    pmib->CodeSegSel=0x24*8;

    pmib->InProtectMode=1;

    //0x300+(cpunr*8) are 16-bit stack selectors

    //call the initializer
    //setup the init ptr
    vbe_initptr.cs=0x25*8;
    vbe_initptr.offset=pmib->PMInitialize;

    vbe_entry.cs=0x25*8;
    vbe_entry.offset=pmib->entrypoint;

    sendstringf("Before calling VBEINIT\n");

    doVBEINIT();

    sendstringf("After calling VBEINIT\n");

    VBE3Initialized=1;

    return 1;
  }
  else
  {
    sendstringf("No VBE PMIB block found\n");
    return 0; //no VBE3
  }
}

int VBE_GetModeInfo(WORD mode, PVBE_ModeInfo info)
{
  //map info at 0xc0020000
  void *info32=mapMemory((void *)0xc0020000, info, sizeof(VBE_ModeInfo));
  PGDT_ENTRY currentgdt=(PGDT_ENTRY)getGDTbase();
  setGDTENTRYBase(&currentgdt[0x29], (QWORD)info32);

  return VBEcall_GetModeInfo(mode, 0x29*8)==0x4f;
}


int VBE_GetControllerInfo(PVBE_ControllerInfo info)
{
  //map info at 0xc0020000
  void *info32=mapMemory((void *)0xc0020000, info, sizeof(VBE_ControllerInfo));
  PGDT_ENTRY currentgdt=(PGDT_ENTRY)getGDTbase();
  setGDTENTRYBase(&currentgdt[0x29], (QWORD)info32);

  return VBEcall_GetControllerInfo(0x29*8)==0x4f;
}

int VBE_SetMode(WORD mode, PVBE_CRTCInfo crtcinfo)
{
  //map info at 0xc0020000
  void *info32=mapMemory((void *)0xc0020000, crtcinfo, sizeof(VBE_CRTCInfo));
  PGDT_ENTRY currentgdt=(PGDT_ENTRY)getGDTbase();
  setGDTENTRYBase(&currentgdt[0x29], (QWORD)info32);

  int r=VBEcall_SetMode(mode, 0x29*8);

  sendstringf("VBEcall_SetMode returned %x\n", r);

  if (r==0x4f)
  {
    //save the details about this mode
    mode=mode & 0x1ff;
    if (VBE_GetModeInfo(mode, &currentMode))
    {
      //map the physical memory for this mode
      sendstringf("VBE3: Changed resolution to: %dx%d %d bit\n",currentMode.XResolution, currentMode.YResolution, currentMode.BitsPerPixel);

      DWORD size=currentMode.LinBytesPerScanLine*currentMode.YResolution;
      GraphicsPageSize=size;

      size*=currentMode.LinNumberOfImagePages+1;

      DWORD m=0xd0000000;
      DWORD p=currentMode.PhysBasePtr;
      while (size)
      {
        int actualsize=size;
        if (actualsize>1024*1024)
          actualsize=1024*1024;

        DWORD *temp=mapPhysicalMemory(p,actualsize);
        mapMemory((void*)(QWORD)m, temp, actualsize);

        unmapPhysicalMemory(temp, actualsize);

        size-=actualsize;
        m+=actualsize;
        p+=actualsize;
      }
    }

    return 1;
  }
  else
    return 0;
}

int VBE_SaveState(void *statestore, int size)
{
  WORD s;
  void *buffer=mapMemory((void *)0xc0020000, statestore, size);
  PGDT_ENTRY currentgdt=(PGDT_ENTRY)getGDTbase();
  setGDTENTRYBase(&currentgdt[0x29], (QWORD)buffer);

  return (VBEcall_SaveRestoreState(1, 0xf,0x29*8, &s)==0x4f);
}

int VBE_RestoreState(void *statestore, int size)
{
  WORD s;
  void *buffer=mapMemory((void *)0xc0020000, statestore, size);
  PGDT_ENTRY currentgdt=(PGDT_ENTRY)getGDTbase();
  setGDTENTRYBase(&currentgdt[0x29], (QWORD)buffer);

  return (VBEcall_SaveRestoreState(2, 0xf,0x29*8, &s)==0x4f);
}

int VBE_GetStateStoreSize()
{
  WORD size;
  if (VBEcall_SaveRestoreState(0, 0xf,0x29*8, &size)==0x4f)
    return size*64;
  else
    return 0;
}

int VBE_ResetStart()
{
  return VBEcall_ResetStart();
}

int VBE_SetDrawPage(int i)
{
  if (i<=currentMode.LinNumberOfImagePages) //LinNumberOfImagePages is number of pages-1
  {
    graphics=(void *)(QWORD)(0xd0000000+i*GraphicsPageSize);
    return 1;
  }
  else
    return 0;
}

//-----
DWORD VBE_ARGBToDispayColor(DWORD ARGB)
{
  //sendstringf("  LinBlueFieldPosition=%d (mask=%d)\n", currentMode.LinBlueFieldPosition, currentMode.LinBlueMaskSize);
  //sendstringf("  LinGreenFieldPosition=%d (mask=%d)\n", currentMode.LinGreenFieldPosition, currentMode.LinGreenMaskSize);
  //sendstringf("  LinRedFieldPosition=%d (mask=%d)\n", currentMode.LinRedFieldPosition, currentMode.LinRedMaskSize);
  //sendstringf("  LinRsvdFieldPosition=%d (mask=%d)\n", currentMode.LinRsvdFieldPosition, currentMode.LinRsvdMaskSize);

  DWORD B=ARGB & 0xff;
  DWORD G=(ARGB >> 8) & 0xff;
  DWORD R=(ARGB >> 16) & 0xff;
  DWORD A=(ARGB >> 24) & 0xff;


  WORD maxValueB=1 << currentMode.LinBlueMaskSize;
  WORD maxValueG=1 << currentMode.LinGreenMaskSize;
  WORD maxValueR=1 << currentMode.LinRedMaskSize;
  WORD maxValueA=1 << currentMode.LinRsvdMaskSize;

  if ((currentMode.DirectColorModeInfo & 2)==0)
  {
    //sendstringf("No support for reserved bits\n");
    maxValueA=0;
  }

  float c;
  if (maxValueB)
  {
    if (maxValueB!=256)
    {
      c=maxValueB/256.0f;
      B=B*c;
      if (B>=maxValueB)
        B=maxValueB-1;
    }
  }
  else
    B=0;

  if (maxValueG)
  {
    if (maxValueG!=256)
    {
      c=maxValueG/256.0f;
      G=G*c;
      if (G>=maxValueG)
        G=maxValueG-1;
    }
  }
  else
    G=0;

  if (maxValueR)
  {
    if (maxValueR!=256)
    {
      c=maxValueR/256.0f;
      R=R*c;
      if (R>=maxValueR)
        R=maxValueR-1;
    }
  }
  else
    R=0;

  if (maxValueA)
  {
    if (maxValueA!=256)
    {
      c=maxValueA/256.0f;
      A=A*c;
      if (A>=maxValueA)
        A=maxValueA-1;
    }
  }
  else
    A=0;

  B=B << currentMode.LinBlueFieldPosition;
  G=G << currentMode.LinGreenFieldPosition;
  R=R << currentMode.LinRedFieldPosition;
  A=A << currentMode.LinRsvdFieldPosition;
  return B | G | R | A;
}

void VBE_SetPenColor(DWORD ARGB)
{
  //convert the ARGB to the format this mode uses
  currentColor=VBE_ARGBToDispayColor(ARGB);
}

void VBE_SetPixel(int x,int y)
{
  int i=currentMode.BytesPerScanLine*y+(x*currentMode.BitsPerPixel/8);
  *(DWORD *)&graphics[i]=currentColor;
}

void VBE_DrawLine(int x1, int y1, int x2,int y2)
{
  float diff=y2-y1;
  float distance=x2-x1;
  float a=diff/distance;
  int x;

  if (x2<x1)
  {
    //swap
    int s=x1;
    x1=x2;
    x2=s;

    s=y1;
    y1=y2;
    y2=s;
  }

  for (x=x1; x<=x2; x++)
  {
    int y=y1+(a*(x-x1));
    VBE_SetPixel(x,y);
  }
}

void VBE_DrawBox(int x1, int y1, int x2, int y2)
{
  int x,y;

  if (x2<x1)
  {
    //swap
    int s=x1;
    x1=x2;
    x2=s;

    s=y1;
    y1=y2;
    y2=s;
  }

  for (y=y1; y<=y2; y++)
  {
    int i=currentMode.BytesPerScanLine*y;
    DWORD *line=(DWORD *)&graphics[i];

    for (x=x1; x<=x2; x++)
      line[x]=currentColor;
  }

}

