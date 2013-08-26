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
 *  If ARM so set to 0 0 and restore that as well
 *  Note that The least significant bit in an address specifier also determines if it's THUMB or ARM
 *  It doesn't seem to matter if you set the least significant bit in the PC register. It will ignore that but on execute. (probably a good idea to clear that bit anyhow)
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

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <libgen.h>

#include "porthelp.h"
#include "api.h"

#ifndef SUN_LEN //missing in android (copy from linux sys/un.h)

/* Evaluate to actual length of the `sockaddr_un' structure.  */
# define SUN_LEN(ptr) ((size_t) (((struct sockaddr_un *) 0)->sun_path)        \
          + strlen ((ptr)->sun_path))
#endif



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

int openExtension(int pid, int *openedSocket)
{
  int i;
  int s;
  int al;
  char name[256];
  s=socket(AF_UNIX, SOCK_STREAM, 0);
  printf("s=%d\n", s);

  sprintf(name, " ceserver_extension%d", pid);

  struct sockaddr_un address;
  address.sun_family=AF_UNIX;
  strcpy(address.sun_path, name);

  al=SUN_LEN(&address);

  address.sun_path[0]=0;
  i=connect(s, (struct sockaddr *)&address, al);

  if (i==0)
  {
    printf("Successful connection\n");
    *openedSocket=s;
    return 1;
  }
  else
  {
    close(s);
    return 0;
  }
}

int isExtensionLoaded(int pid)
{
  int s;
  int result=openExtension(pid, &s);

  if (result)
    close(s);

  return result;
}

int loadExtension(int pid, char *path)
{
    uintptr_t dlopen;
    uintptr_t str;
    int pathlen=strlen(path)+1; //0-terminater

    printf("loadExtension(%d, %s)\n", pid, path);

    printf("Phase 0: Check if it's already open\n");
    if (isExtensionLoaded(pid))
    {
      printf("Already loaded\n");
      return TRUE;
    }
    else
      printf("Not yet loaded\n");



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
        ptrace(PTRACE_DETACH, pid,0,0);

        return FALSE;
      }

      if (ptrace(PTRACE_GETREGS, pid, 0, &origregs)!=0)
      {
        printf("PTRACE_GETREGS FAILED 2\n");
        ptrace(PTRACE_DETACH, pid,0,0);

        return FALSE;
      }



      uintptr_t returnaddress=0x0ce0;


#ifdef __arm__
      //allocate space in the stack

      newregs.ARM_sp-=8+4*((pathlen+3)/ 4);

      //not sur eif [sp] is written to with a push or if it's [sp-4] and then sp decreased, so start at sp+4 instead
      str=newregs.ARM_sp+4;
      writeString(pid, str, path);




      newregs.ARM_lr=returnaddress;
      newregs.ARM_pc=dlopen;
      newregs.ARM_r0=str;
      newregs.ARM_r1=RTLD_NOW;

      if (newregs.ARM_pc & 1)
      {
         //THUMB Address link
         printf("THUMB destination\n");
         newregs.ARM_cpsr=newregs.ARM_cpsr | (1 << 5);

         //not sure how to set the J bit (thumbee uses it...)
         //for now disable it until a bug happens
         newregs.ARM_cpsr=newregs.ARM_cpsr & (~(1<<25)); //unset J


      }
      else
      {
        printf("ARM destination\n");
        printf("newregs.ARM_cpsr was %x\n", newregs.ARM_cpsr);
        newregs.ARM_cpsr=newregs.ARM_cpsr & (~(1<<5)); //unset T
        newregs.ARM_cpsr=newregs.ARM_cpsr & (~(1<<25)); //unset J
        printf("newregs.ARM_cpsr is %x\n", newregs.ARM_cpsr);
      }

      printf("r0=%lx\n", origregs.ARM_r0);
      printf("orig_r0=%lx\n", origregs.ARM_ORIG_r0);
      printf("pc=%lx\n", origregs.ARM_pc);
      printf("cpsr=%lx\n", origregs.ARM_cpsr);

#else
      printf("rax=%lx\n", origregs.rax);
      printf("rbp=%lx\n", origregs.rbp);
      printf("rsp=%lx\n", origregs.rsp);
      printf("orig_rax=%lx\n", origregs.orig_rax);
      printf("rip=%lx\n", origregs.rip);



      //allocate stackspace
      newregs.rsp=newregs.rsp-0x28-(8*((pathlen+7) / 8));

      //check that the first 4 bits of rsp are 1000 (8) (aligned with the function return push)
      if ((newregs.rsp & 0xf)!=8)
      {
        printf("Aligning stack.  Was %llx", newregs.rsp);
        newregs.rsp-=8;
        newregs.rsp&=~(0xf); //clear the first 4 bits

        newregs.rsp=newregs.rsp | 8; //set to 8

        printf(" is now %llx\n", newregs.rsp);
      }
      //set the return address

      printf("Writing 0x0ce0 to %lx\n", newregs.rsp);
      if (ptrace(PTRACE_POKEDATA, pid, newregs.rsp, returnaddress)!=0)
      {
        printf("Failed to write return address\n");
        ptrace(PTRACE_DETACH, pid,0,0);

        return FALSE;
      }

      if (ptrace(PTRACE_POKEDATA, pid, newregs.rsp-8, returnaddress)!=0)
      {
        printf("Fuck\n");
        ptrace(PTRACE_DETACH, pid,0,0);

        return FALSE;
      }

      if (ptrace(PTRACE_POKEDATA, pid, newregs.rsp+8, returnaddress)!=0)
      {
        printf("Fuck\n");
        ptrace(PTRACE_DETACH, pid,0,0);

        return FALSE;
      }


     //write the path at rsp+10

     str=newregs.rsp+0x18;
     writeString(pid, str, path);

     printf("str=%p\n", (void *)str);



     returnaddress=ptrace(PTRACE_PEEKDATA, pid, newregs.rsp, 0);
     printf("[%lx]=%lx", newregs.rsp, returnaddress);


      newregs.rip=dlopen; //+2 //(test)
      newregs.rax=0;
      newregs.rdi=str;
      newregs.rsi=RTLD_NOW;
      newregs.orig_rax=0;




#endif

      if (ptrace(PTRACE_SETREGS, pid, 0, &newregs)!=0)
      {
        printf("PTRACE_SETREGS FAILED\n");
        ptrace(PTRACE_DETACH, pid,0,0);

        return FALSE;
      }

      if (ptrace(PTRACE_GETREGS, pid, 0, &newregs)!=0)
      {
        printf("PTRACE_GETREGS FAILED 4\n");
        ptrace(PTRACE_DETACH, pid,0,0);

        return FALSE;
      }

     printf("after setregs:\n");

#ifdef __arm__
     printf("r0=%lx\n", newregs.ARM_r0);
     printf("orig_r0=%lx\n", newregs.ARM_ORIG_r0);
     printf("pc=%lx\n", newregs.ARM_pc);
     printf("cpsr=%lx\n", newregs.ARM_cpsr);
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


int ptr;
    ptr=ptrace(PTRACE_CONT,pid,(void *)0,(void *)SIGCONT);

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

          return FALSE;
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
       ptrace(PTRACE_DETACH, pid,0,0);

       return FALSE;
     }

     printf("si.si_signo=%d\n", si.si_signo);



     if (ptrace(PTRACE_GETREGS, pid, 0, &newregs)!=0)
     {
       printf("PTRACE_GETREGS FAILED (2)\n");
       ptrace(PTRACE_DETACH, pid,0,0);

       return FALSE;
     }

#ifdef __arm__
    printf("r0=%lx\n", newregs.ARM_r0);
    printf("orig_r0=%lx\n", newregs.ARM_ORIG_r0);
    printf("pc=%lx\n", newregs.ARM_pc);
    printf("cpsr=%lx\n", newregs.ARM_cpsr);
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
     }

     printf("Detaching\n");
     if (ptrace(PTRACE_DETACH, pid,0,0)!=0)
       printf("PTRACE_DETACH FAILED\n");

     printf("End...\n");
}

int loadCEServerExtension(HANDLE hProcess)
{
  printf("loadCEServerExtension\n");
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);



    if (p->hasLoadedExtension==0)
    {
      char modulepath[256], modulepath2[256];
      if (readlink("/proc/self/exe", modulepath2, 256)!=-1)
      {
        sprintf(modulepath, "%s/libceserver-extension.so", dirname(modulepath2));
      }
      else
      {
        strcpy(modulepath, "libceserver-extension.so");
      }




      if (p->isDebugged)
      {
        printf("This process is being debugged. Checking if it's already loaded\n");

        pthread_mutex_lock(&p->extensionMutex);
        p->hasLoadedExtension=openExtension(p->pid, &p->extensionFD);
        pthread_mutex_unlock(&p->extensionMutex);
      }
      else
      {
        pthread_mutex_lock(&p->extensionMutex);
        if (p->hasLoadedExtension==0) //still 0
        {

          if (p->neverForceLoadExtension==0)
          {
            printf("Calling loadExtension\n");
            p->hasLoadedExtension=loadExtension(p->pid, modulepath);
          }

          if (p->hasLoadedExtension)
            p->hasLoadedExtension=openExtension(p->pid, &p->extensionFD);
        }

        pthread_mutex_unlock(&p->extensionMutex);
      }


    }

    return p->hasLoadedExtension;
  }
  else
  {
    printf("Invalid handle type");
    return 0;
  }
}
