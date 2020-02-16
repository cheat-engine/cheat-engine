/*
 * offloados.h
 *
 *  Created on: Sep 2, 2009
 *      Author: erich
 */

#ifndef OFFLOADOS_H_
#define OFFLOADOS_H_

typedef struct
{
  UINT64    cpucount;
  UINT64    originalEFER;
  UINT64    originalLME;
  UINT64    idtbase;
  UINT64    idtlimit;
  UINT64    gdtbase;
  UINT64    gdtlimit;
  UINT64    cr0;
  UINT64    cr2;
  UINT64    cr3;
  UINT64    cr4;
  UINT64    dr7;
  UINT64    rip;

  UINT64    rax;
  UINT64    rbx;
  UINT64    rcx;
  UINT64    rdx;
  UINT64    rsi;
  UINT64    rdi;
  UINT64    rbp;
  UINT64    rsp;
  UINT64    r8;
  UINT64    r9;
  UINT64    r10;
  UINT64    r11;
  UINT64    r12;
  UINT64    r13;
  UINT64    r14;
  UINT64    r15;

  UINT64    rflags;
  UINT64    cs;
  UINT64    ss;
  UINT64    ds;
  UINT64    es;
  UINT64    fs;
  UINT64    gs;
  UINT64    tr;
  UINT64    ldt;

  UINT64    cs_AccessRights;
  UINT64    ss_AccessRights;
  UINT64    ds_AccessRights;
  UINT64    es_AccessRights;
  UINT64    fs_AccessRights;
  UINT64    gs_AccessRights;

  UINT64    cs_Limit;
  UINT64    ss_Limit;
  UINT64    ds_Limit;
  UINT64    es_Limit;
  UINT64    fs_Limit;
  UINT64    gs_Limit;

  UINT64    fsbase;
  UINT64    gsbase;
  UINT64    APEntryPage; //page below 1MB (for AP cpu bootcode)
  UINT64    FrameBufferBase; //in case of uefi boot the framebuffer address for output
  UINT64    FrameBufferSize;
  UINT64    HorizontalResolution;
  UINT64    VerticalResolution;
  UINT64    PixelsPerScanLine;
  UINT64    PixelFormat;
} __attribute__((__packed__)) OriginalState, *POriginalState;


typedef struct
{
  UINT64 startAddress;
  UINT64 byteSize;
} __attribute__((__packed__)) UncachedRegion, *PUncachedRegion;

#endif /* OFFLOADOS_H_ */
