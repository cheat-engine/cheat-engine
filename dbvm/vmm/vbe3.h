/*
 * vbe3.h
 *
 *  Created on: Mar 2, 2018
 *      Author: eric heijnen
 */

#ifndef VMM_VBE3_H_
#define VMM_VBE3_H_

#include "common.h"

int initializeVBE3();

typedef struct _VBE3_PM_InfoBlock
{
  DWORD ID; //'PMID' 0x44494d50 PM Info Block Signature
  WORD entrypoint; //Offset of PM entry point within BIOS
  WORD PMInitialize; //Offset of PM initialization entry point
  WORD BIOSDataSel; //Selector to BIOS data area emulation block
  WORD A0000Sel; //Selector to access A0000h physical mem
  WORD B0000Sel; //Selector to access B0000h physical mem
  WORD B8000Sel; //Selector to access B8000h physical mem
  WORD CodeSegSel; //Selector to access code segment as data
  unsigned char InProtectMode; //Set to 1 when in protected mode
  unsigned char Checksum; //Checksum byte for structure
} __attribute__((__packed__)) VBE3_PM_InfoBlock, *PVBE3_PM_InfoBlock;

typedef struct _VBE_ControllerInfo
{
  DWORD VbeSignature; //VBE Signature. 'VESA' (0x41534556) or set to 'VBE2' (0x32454256)
  WORD VbeVersion; //VBE Version
  DWORD OemStringPtr; //VbeFarPtr to OEM String
  DWORD Capabilities; //Capabilities of graphics controller
  DWORD VideoModePtr; //VbeFarPtr to VideoModeList
  WORD TotalMemory; //Number of 64kb memory blocks
  WORD OemSoftwareRev; //VBE implementation Software revision
  DWORD OemVendorNamePtr; //VbeFarPtr to Vendor Name String
  DWORD OemProductNamePtr; //VbeFarPtr to Product Name String
  DWORD OemProductRevPtr; //VbeFarPtr to Product Revision String
  BYTE  Reserved[222];
  BYTE  OemData[256];
} __attribute__((__packed__)) VBE_ControllerInfo, *PVBE_ControllerInfo;

typedef struct _VBE_ModeInfo
{
  WORD ModeAttributes;
  BYTE WinAAttributes;
  BYTE WinBAttributes;
  WORD WinGranularity;
  WORD WinSize;
  WORD WinASegment;
  WORD WinBSegment;
  DWORD WinFuncPtr;
  WORD BytesPerScanLine;
  //1.2
  WORD XResolution;
  WORD YResolution;
  BYTE XCharSize;
  BYTE YCharSize;
  BYTE NumberOfPlanes;
  BYTE BitsPerPixel;
  BYTE NumberOfBanks;
  BYTE MemoryModel;
  BYTE BankSize;
  BYTE NumberOfImagePages;
  BYTE Reserved;

  BYTE RedMaskSize;
  BYTE RedFieldPosition;
  BYTE GreenMaskSize;
  BYTE GreenFieldPosition;
  BYTE BlueMaskSize;
  BYTE BlueFieldPosition;
  BYTE RsvdMaskSize;
  BYTE RsvdFieldPosition;
  BYTE DirectColorModeInfo;

  //2.0
  DWORD PhysBasePtr;
  DWORD Reserved2;
  WORD Reserved3;

  //3.0
  WORD LinBytesPerScanLine;
  BYTE BnkNumberOfImagePages;
  BYTE LinNumberOfImagePages;
  BYTE LinRedMaskSize;
  BYTE LinRedFieldPosition;
  BYTE LinGreenMaskSize;
  BYTE LinGreenFieldPosition;
  BYTE LinBlueMaskSize;
  BYTE LinBlueFieldPosition;
  BYTE LinRsvdMaskSize;
  BYTE LinRsvdFieldPosition;
  DWORD MaxPixelClock;
  BYTE filler[189];
} __attribute__((__packed__)) VBE_ModeInfo, *PVBE_ModeInfo;

typedef struct _VBE_CRTCInfo
{
  WORD HorizontalTotal;
  WORD HorizontalSyncStart;
  WORD orizontalSyncEnd;
  WORD VerticalTotal;
  WORD VerticalSyncStart;
  WORD VerticalSyncEnd;
  BYTE Flags;
  DWORD PixelClock;
  WORD RefreshRate;
} __attribute__((__packed__)) VBE_CRTCInfo, *PVBE_CRTCInfo;


void *VBEPtrToAddress(DWORD vbeptr);

int VBE_GetControllerInfo(PVBE_ControllerInfo info);
int VBE_GetModeInfo(WORD mode, PVBE_ModeInfo info);
int VBE_SetMode(WORD mode, PVBE_CRTCInfo crtcinfo);
int VBE_SaveState(void *statestore, int size);
int VBE_RestoreState(void *statestore, int size);
int VBE_GetStateStoreSize();
int VBE_SetDrawPage(int i);
int VBE_ResetStart();


//----- Draw operations -----
void VBE_SetPenColor(DWORD ARGB);
void VBE_SetPixel(int x,int y);
void VBE_DrawLine(int x1, int y1, int x2,int y2);
void VBE_DrawBox(int x1, int y1, int x2, int y2);

#endif /* VMM_VBE3_H_ */
