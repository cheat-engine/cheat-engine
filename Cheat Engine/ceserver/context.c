/*
 * context.c
 *
 *  Created on: May 18, 2015
 *      Author: eric
 */


#include "context.h"
#include "api.h"

int getContext(int tid, CONTEXT *context)
{
  context->type=0;
  context->structsize=sizeof(CONTEXT);

  debug_log("getContext(%d)\n", tid);

  debug_log("context=%p\n", context);
  debug_log("context->structsize=%p\n", &context->structsize);
  debug_log("context->type=%p\n", &context->type);
  debug_log("context->regs=%p\n", &context->regs);
  debug_log("context->fp=%p\n", &context->fp);

#ifndef NT_PRSTATUS
  int r,r2;
  r=safe_ptrace(PTRACE_GETREGS, tid, 0, &context->regs);

  if (r==0)
  {
#ifdef __arm__
    debug_log("PC=%x\n", context->regs.ARM_pc);
    debug_log("ARM_cpsr=%x", context->regs.ARM_cpsr);

    if (context->regs.ARM_cpsr & (1<<5))
      debug_log(" (THUMB MODE)");

    debug_log("\n");
    r2=safe_ptrace(PTRACE_GETVFPREGS, tid, 0, &context->fp);
#endif

#ifdef __i386__
    debug_log("EIP=%x\n", context->regs.eip);

    r2=safe_ptrace(PTRACE_GETFPXREGS, tid,0, &context->fp);

#endif

#ifdef __x86_64__
    debug_log("RIP=%x\n", context->regs.rip);

    r2=safe_ptrace(PTRACE_GETFPREGS, tid,0, &context->fp);

#endif

  }

#ifdef __i386__
  context->type=0;
#endif

#ifdef __x86_64__
  context->type=1;
#endif

#ifdef __arm__
  context->type=2;
#endif

#ifdef __aarch64__
  context->type=3;
#endif



  return r;
#else
  int r,r2;
  struct iovec iov;
  iov.iov_base=&context->regs;
  iov.iov_len=sizeof(CONTEXT_REGS);

  debug_log("iov_base=%p sizeof(CONTEXT_REGS)=%d\n",iov.iov_base, sizeof(CONTEXT_REGS));
  r=safe_ptrace(PTRACE_GETREGSET, tid, (void*)NT_PRSTATUS, &iov);

  if (r==0)
  {
    debug_log("iov.iov_len=%d\n", iov.iov_len);

#ifdef __i386__
    debug_log("x86 context\n");
    context->type=0;
#endif

#ifdef __x86_64__
    debug_log("x86_64 context\n");
    context->type=1;
#endif

#ifdef __arm__
    debug_log("This is an arm context\n");
    context->type=2;
#endif

    iov.iov_base=&context->fp;
#ifdef __aarch64__
    if (iov.iov_len==sizeof(struct pt_regs32))
    {
      debug_log("This is an aarch32 context\n");
      context->type=2;
      iov.iov_base=&context->fp32;


      context->structsize=((uintptr_t)(&context->fp32)-(uintptr_t)context)+sizeof(CONTEXT_FP32);

      iov.iov_len=sizeof(CONTEXT_FP32);
      debug_log("iov_base=%p Trying to get NT_ARM_VFP stats (iov_len=%d)\n", iov.iov_base, iov.iov_len);

      r2=safe_ptrace(PTRACE_GETREGSET, tid, (void*)NT_ARM_VFP, &iov);
      debug_log("Getting NT_ARM_VFP returned %d (iov_len=%d)\n", r2, iov.iov_len);

    }
    else
    {
      debug_log("This is an aarch64 context\n");
      context->type=3;

      iov.iov_len=sizeof(CONTEXT_FP);
      debug_log("iov_base=%p Trying to get FPREG stats (iov_len=%d)\n", iov.iov_base, iov.iov_len);

      r2=safe_ptrace(PTRACE_GETREGSET, tid, (void*)NT_PRFPREG, &iov);
      debug_log("Getting FPREG returned %d (iov_len=%d)\n", r2, iov.iov_len);

    }
#endif



  }
  return r;
#endif

}

int setContext(int tid, CONTEXT *context)
{
  int r;
  //todo FPU
  debug_log("setContext\n");
#ifndef NT_PRSTATUS
  return safe_ptrace(PTRACE_SETREGS, tid, 0, &context->regs);
#else
  struct iovec iov;
  iov.iov_base=&context->regs;
  iov.iov_len=sizeof(CONTEXT_REGS);
  r=safe_ptrace(PTRACE_SETREGSET, tid, (void*)NT_PRSTATUS, &iov);

  if (r==0)
  {
    switch (context->type)
    {
      case 0:
        break;

      case 1:
        break;

      case 2:
        break;

      case 3:
        //set the fpu fields

        break;

      default:
        break;
    }

  }

  return r;
#endif
}


