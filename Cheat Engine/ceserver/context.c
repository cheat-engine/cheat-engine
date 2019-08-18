/*
 * context.c
 *
 *  Created on: May 18, 2015
 *      Author: eric
 */


#include "context.h"

int getRegisters(int tid, CONTEXT_REGS *registerstore)
{

#ifndef NT_PRSTATUS
  return safe_ptrace(PTRACE_GETREGS, tid, 0, registerstore);
#else
  struct iovec iov;
  iov.iov_base=registerstore;
  iov.iov_len=sizeof(CONTEXT_REGS);
  return safe_ptrace(PTRACE_GETREGSET, tid, (void*)NT_PRSTATUS, &iov);
#endif
}

int setRegisters(int tid, CONTEXT_REGS *registerstore)
{
#ifndef NT_PRSTATUS
  return safe_ptrace(PTRACE_SETREGS, tid, 0, registerstore);
#else
  struct iovec iov;
  iov.iov_base=registerstore;
  iov.iov_len=sizeof(CONTEXT_REGS);
  return safe_ptrace(PTRACE_SETREGSET, tid, (void*)NT_PRSTATUS, &iov);
#endif
}


