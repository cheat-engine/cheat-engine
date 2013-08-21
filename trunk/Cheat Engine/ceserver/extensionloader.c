/*
 * extentionloader.c
 *
 *  Created on: Aug 19, 2013
 *      Author: eric
 *
 *  Used for loading an module that will extend the ceserver
 *  client<-->ceserver<-->extention
 *
 *  It doesn't HAVE to be used if the forced module load method works (Do not assume so)
 *
 *  How it works:
 *  Ptrace the target  (this means it must be loaded BEFORE the debugger is attached)
 *  Cause a stop and make sure it's runnable (Not sure if it executes if it's suspended for some reason. e.g: wait for event/critical section that may never happen)
 *  Change the current instruction pointer to the beginning of dlopen and the register/stack state setup to execute
 *  Set the return addres to an invalid return address (e.g 0x0ce0)
 *  Execute it and wait till a sigtrap/sigseg happens on that specific invalid address
 *  Then restore the state back
 *
 *  On arm: Bit J and T in CPSR define the current execution state
 *  J T
 *  0 0 = ARM
 *  0 1 = Thumb
 *  1 0 = Jazelle (java...)
 *  1 1 = ThumbEE*
 *
 *  So set to 0 0 and restore that as well
 *
 *
 *  Problem: It doesn't return properly when the registers are changed when it's waiting in a syscall, so only change it when outside of a syscall
 *  Better solution: It seems it failed because the stop was at a syscall, so the program counter was decremented tithe the size of the syscall
 *  To prevent this RESTART change EAX to 0 so it won't do the restart.  Untested on ARM
 *
 *  Problem2: In android dlopen is in /system/bin/linker but not using a symbol (so ce's symbollist can't be used to find the address)
 *
 *  dlopen("libdl.so", RTLD_NOW) actually works in android and dlsym as well. (point to the linker version)
 *  This is useful since this makes it cross compatible with normal linux.
 *  for some reason getting the address of dlopen in x86 returns a local stub and I don't know yet how to prevent those stubs
 *
 *  so, to find dlopen find address range dlopen is in in this process (/proc/selfpid/maps), get the base address of that specific module
 *  and then add that offset to the same named module in the target process
 *
 */
#include <stdio.h>
#include <sys/wait.h>
#include <sys/ptrace.h>
#include <errno.h>

#include "extensionloader.h"
#include "api.h"


int LoadExtention(HANDLE hProcess, uintptr_t dlopenpos)
{
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);

    if (p->isDebugged)
    {
      return FALSE; //For now. Else wake a thread, and do the same stuff
    }

    ptrace(PTRACE_ATTACH, p->pid, 0,0);
    int pid=-1;
    int status;
    while (pid==-1)
    {
      pid=waitpid(-1, &status, __WALL);

      if ((pid==-1) && (errno!=EINTR))
      {
        printf("LoadExtension wait fail. :%d\n", errno);
        return FALSE; //something bad happened
      }
    }


    //save the current state and set the state to what I need it to be

#ifdef __arm__
  struct pt_regs regs;
#else
  struct user_regs_struct regs;
#endif

      if (ptrace(PTRACE_GETREGS, pid, 0, &regs)!=0)
      {
        printf("PTRACE_GETREGS FAILED\n");
        return FALSE;
      }

#ifdef __arm__
#else
      printf("rax=%lx\n", regs.rax);
      printf("rbp=%lx\n", regs.rbp);
      printf("rsp=%lx\n", regs.rsp);
      printf("orig_rax=%lx\n", regs.rax);
      printf("rip=%lx\n", regs.rip);

      regs.rip=0x4007ad; //dechp (test)
      regs.rsp=regs.rsp-8;

      if (ptrace(PTRACE_SETREGS, pid, 0, &regs)!=0)
      {
        printf("PTRACE_SETREGS FAILED\n");
        return FALSE;
      }


#endif





  }
  else
    printf("Invalid handle\n");

  return FALSE;
}
