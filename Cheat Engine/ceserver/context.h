/*
 * context.h
 *
 *  Created on: May 18, 2015
 *      Author: eric
 */

#ifndef CONTEXT_H_
#define CONTEXT_H_

#include <stdint.h>

#ifdef HAS_LINUX_USER_H
#include <linux/user.h>
#else
#include <sys/user.h>
#endif

#include <sys/ptrace.h>
#include <linux/socket.h>

#ifdef __aarch64__
#include <elf.h>
#endif



#include <sys/socket.h>


#ifdef __i386__
  typedef struct user_regs_struct CONTEXT_REGS;
  typedef struct user_fpxregs_struct CONTEXT_FP;
#endif

#ifdef __x86_64__
  typedef struct user_regs_struct CONTEXT_REGS;
  typedef struct user_fpregs_struct CONTEXT_FP;
#endif


#ifdef __arm__
  typedef struct pt_regs CONTEXT_REGS;
  typedef struct pv_fpregs {
    uint64_t regs[32];
    uint32_t control; //todo: confirm this
  } CONTEXT_FP;
#endif


#ifdef __aarch64__
  struct pt_regs32 {
    uint32_t uregs[18];
  };

  typedef struct {
    uint64_t fpregs[32];
    uint32_t xxx;
  } CONTEXT_FP32;

  typedef struct user_pt_regs CONTEXT_REGS;
  typedef struct user_fpsimd_state CONTEXT_FP;

#endif

#pragma pack(push, 1)
typedef struct
{
  uint32_t structsize;
  uint32_t type;
  union
  {
#ifdef __aarch64__
    struct {
      struct pt_regs32 regs32; //alt
      CONTEXT_FP32 fp32;
    };
#endif
#ifdef __x86_64__
    //todo: 32-bit x86 context regs32;
#endif
    struct {
      CONTEXT_REGS regs;
      CONTEXT_FP fp;
    };
  };

} CONTEXT, *PCONTEXT;
#pragma pack(pop)

int getContext(int tid, CONTEXT *context);
int setContext(int tid, CONTEXT *context);


#endif /* CONTEXT_H_ */
