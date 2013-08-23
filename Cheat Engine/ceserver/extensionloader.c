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
 *  To prevent this RESTART change EAX to 0 so it won't do the restart.  Also works on ARM
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
#include <stdint.h>
#include <string.h>

#ifdef __arm__
#include <linux/user.h>
#else
#include <sys/user.h>
#endif

#include <dlfcn.h>

int WaitForPid()
{
  int status;
  int pid=-1;
  while (pid==-1)
  {
    pid=waitpid(-1, &status, __WALL);
    if ((pid==-1) && (errno!=EINTR))
    {
      printf("LoadExtension wait fail. :%d\n", errno);
      return -1; //something bad happened
    }
  }
  return pid;
}

int showRegisters(int pid)
{
#ifdef __arm__
  struct pt_regs r;
#else
  struct user_regs_struct r;
#endif

  if (ptrace(PTRACE_GETREGS, pid, 0, &r)!=0)
  {
    printf("PTRACE_GETREGS FAILED\n");
    return;
  }

#ifdef __arm__
  printf("r0=%lx\n", r.ARM_r0);
  printf("orig_r0=%lx\n", r.ARM_ORIG_r0);
  printf("pc=%lx\n", r.ARM_pc);
#else
  printf("RAX=%lx\n", r.rax);
  printf("orig_rax=%lx\n", r.orig_rax);
  printf("rip=%lx\n", r.rip);
#endif


}

uintptr_t finddlopen(int pid)
{
    void *libdl;
    void *realdlopen;
  libdl=dlopen("libdl.so", RTLD_NOW);

    printf("libdl=%p\n", libdl);

    realdlopen=dlsym(libdl,"dlopen");
    printf("dlopen=%p\n", dlopen);
    printf("realdlopen=%p\n", realdlopen);
#ifndef __arm__
    if (dlopen==realdlopen)
      printf("Please tell db what you did to get this to function (excluding manually editing this if statement)\n");
#endif


    //open /proc/self/maps and look up the region that holds realdlopen

    FILE *maps=fopen("/proc/self/maps", "r");

    char x[200];
    char currentmodule[256];
    char modulepath[256];

    unsigned long long currentmodulestart;

    currentmodule[0]=0;


    while (fgets(x, 200, maps))
    {
      unsigned long long start;
      unsigned long long stop;
      printf("%s", x);

      sscanf(x, "%llx-%llx %*s %*s %*s %*s %s\n", &start, &stop, modulepath);

      if (strcmp(modulepath, currentmodule)!=0)
      {
         strcpy(currentmodule, modulepath);
         currentmodulestart=start;
      }

      if (
           (((uintptr_t)realdlopen)>=start) &&
           (((uintptr_t)realdlopen)<stop)
         )
      {
        unsigned int offset=(uintptr_t)realdlopen-currentmodulestart;
        char mapsfilename[255];
        printf("found it. Module: %s Offset=%x\n", currentmodule, offset);

        //find this module in the target process and apply this offset to get the address of dlopen
        sprintf(mapsfilename, "/proc/%d/maps", pid);
        FILE *maps2=fopen(mapsfilename, "r");
        if (maps2)
        {
          char y[200];
          while (fgets(y, 200, maps2))
          {
             printf("%s", y);
             sscanf(y, "%llx-%llx %*s %*s %*s %*s %s\n", &start, &stop, modulepath);
             if (strcmp(modulepath, currentmodule)==0)
             {
                printf("found the module in the target process\n");
                fclose(maps);
                fclose(maps2);
                return start+offset;
             }
          }
          fclose(maps2);

        }
        else
        {
           printf("Failure to open %s\n", mapsfilename);
        }


        fclose(maps);
        return 0;
      }
      else printf("Nope\n");

    }

    fclose(maps);
}

void writeString(int pid, uintptr_t address, char *string)
{
  int l=strlen(string)+1;
  long *p;
  long v;
  int i;
  int bs;
  i=0;

  printf("l=%d\n", l);


  while (i<l)
  {
    p=(long *)&string[i];
    if ((l-i)<sizeof(long))
    {
      bs=sizeof(long);
      v=*p;
    }
    else
    {
      v=string[i];
      bs=1;
    }

    ptrace(PTRACE_POKEDATA, pid, address+i, v);
    i+=bs;
  }
}

int loadExtension(int pid)
{
    uintptr_t dlopen;

    printf("Phase 1: Find dlopen in target\n");

    dlopen=finddlopen(pid);
    printf("dlopen=%p\n", (void *)dlopen);

    ptrace(PTRACE_ATTACH, pid, 0,0);

    int status;

    pid=WaitForPid();

    printf("After wait. PID=%d\n", pid);

    ptrace(PTRACE_CONT,pid,0,0);

    kill(pid, SIGSTOP);

    pid=WaitForPid();


    showRegisters(pid);



printf("After wait 2. PID=%d\n", pid);




    //save the current state and set the state to what I need it to be

#ifdef __arm__
  struct pt_regs origregs;
  struct pt_regs newregs;
#else
  struct user_regs_struct origregs;
  char bla[1024];


  struct user_regs_struct newregs;
  char bla2[1024];
#endif

      if (ptrace(PTRACE_GETREGS, pid, 0, &newregs)!=0)
      {
        printf("PTRACE_GETREGS FAILED\n");
        return 0;
      }

      if (ptrace(PTRACE_GETREGS, pid, 0, &origregs)!=0)
      {
        printf("PTRACE_GETREGS FAILED 2\n");
        return 0;
      }



      uintptr_t returnaddress=0x0ce0;


#ifdef __arm__
      newregs.ARM_lr=returnaddress;
      newregs.ARM_pc=0x84c0;
      newregs.ARM_r0=0;

      printf("r0=%lx\n", origregs.ARM_r0);
      printf("orig_r0=%lx\n", origregs.ARM_ORIG_r0);
      printf("pc=%lx\n", origregs.ARM_pc);

#else
      printf("rax=%lx\n", origregs.rax);
      printf("rbp=%lx\n", origregs.rbp);
      printf("rsp=%lx\n", origregs.rsp);
      printf("orig_rax=%lx\n", origregs.orig_rax);
      printf("rip=%lx\n", origregs.rip);



      //allocate stackspace
      newregs.rsp=newregs.rsp-0x28-16;
      //set the return address

      printf("Writing 0x0ce0 to %lx\n", newregs.rsp);
      if (ptrace(PTRACE_POKEDATA, pid, newregs.rsp, returnaddress)!=0)
      {
        printf("Failed to write return address\n");
        return 0;
      }

      if (ptrace(PTRACE_POKEDATA, pid, newregs.rsp-8, returnaddress)!=0)
      {
        printf("Fuck\n");
        return 0;
      }

    if (ptrace(PTRACE_POKEDATA, pid, newregs.rsp+8, returnaddress)!=0)
      {
        printf("Fuck\n");
        return 0;
      }


     //write the path at rsp+10
     writeString(pid, newregs.rsp+0x18, "/root/bla.so");



     returnaddress=ptrace(PTRACE_PEEKDATA, pid, newregs.rsp, 0);
     printf("[%lx]=%lx", newregs.rsp, returnaddress);


      newregs.rip=dlopen; //+2 //(test)
      newregs.rax=0;
      newregs.rdi=newregs.rsp+0x18;
      newregs.rsi=RTLD_NOW;
      newregs.orig_rax=0;




#endif

      if (ptrace(PTRACE_SETREGS, pid, 0, &newregs)!=0)
      {
        printf("PTRACE_SETREGS FAILED\n");
        return 0;
      }

      if (ptrace(PTRACE_GETREGS, pid, 0, &newregs)!=0)
      {
        printf("PTRACE_GETREGS FAILED 4\n");
        return 0;
      }

     printf("after setregs:\n");

#ifdef __arm__
     printf("r0=%lx\n", origregs.ARM_r0);
     printf("orig_r0=%lx\n", origregs.ARM_ORIG_r0);
     printf("pc=%lx\n", origregs.ARM_pc);
#else
     printf("rax=%lx\n", newregs.rax);
     printf("rdi=%lx\n", newregs.rdi);
     printf("rsi=%lx\n", newregs.rsi);
     printf("rbp=%lx\n", newregs.rbp);
     printf("rsp=%lx\n", newregs.rsp);
     printf("orig_rax=%lx\n", newregs.orig_rax);
     printf("rip=%lx\n", newregs.rip);
#endif

    printf("\n\nContinuing thread\n");

  printf("press enter\n");
   getchar();

int ptr;
    ptr=ptrace(PTRACE_CONT,pid,0,SIGCONT);

    printf("PRACE_CONT=%d\n", ptr);
    if (ptr!=0)
      {
        printf("PTRACE_CONT FAILED\n");
        return;
      }

      //wait for this thread to crash

      pid=-1;
      while (pid==-1)
      {
        pid=waitpid(-1, &status,  WUNTRACED| __WALL);

        if ((pid==-1) && (errno!=EINTR))
        {
          printf("LoadExtension wait fail. :%d\n", errno);
          return; //something bad happened
        }

        if (pid==0)
          pid=-1;
        printf(".");
      }

     printf("after wait: pid=%d (status=%x)\n", pid, status);

     siginfo_t si;
     if (ptrace(PTRACE_GETSIGINFO, pid, NULL, &si)!=0)
     {
       printf("GETSIGINFO FAILED\n");
       return;
     }

     printf("si.si_signo=%d\n", si.si_signo);



     if (ptrace(PTRACE_GETREGS, pid, 0, &newregs)!=0)
     {
       printf("PTRACE_GETREGS FAILED (2)\n");
       return;
     }

#ifdef __arm__
    printf("r0=%lx\n", newregs.ARM_r0);
    printf("orig_r0=%lx\n", newregs.ARM_ORIG_r0);
    printf("pc=%lx\n", newregs.ARM_pc);
#else

     printf("rax=%lx\n", newregs.rax);
     printf("rdi=%lx\n", newregs.rdi);
     printf("rsi=%lx\n", newregs.rsi);
     printf("rbp=%lx\n", newregs.rbp);
     printf("rsp=%lx\n", newregs.rsp);
     printf("orig_rax=%lx\n", newregs.rax);
     printf("rip=%lx\n", newregs.rip);

#endif

     if (ptrace(PTRACE_SETREGS, pid, 0, &origregs)!=0)
     {
       printf("PTRACE_SETREGS FAILED (20\n");
       return;
     }

     printf("Detaching\n");
     if (ptrace(PTRACE_DETACH, pid,0,0)!=0)
       printf("PTRACE_DETACH FAILED\n");

     printf("End...\n");
}
