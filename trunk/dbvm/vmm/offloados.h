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

  UINT64    fsbase;
  UINT64    gsbase;
} __attribute__((__packed__)) OriginalState, *POriginalState;

#endif /* OFFLOADOS_H_ */
