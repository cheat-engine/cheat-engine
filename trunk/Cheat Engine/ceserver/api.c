/*
 * api.c
 *
 *  Created on: Jul 20, 2011
 *      Author: erich
 *
 *      This unit will implement the api's CE uses
 *      This will be the main point of interest when porting to another system
 */

//#define _XOPEN_SOURCE 500

//todo for in the far future: Hook syscalls


#include <stdio.h>
#include <pthread.h>



#include <stddef.h>
#include <string.h>
#include <stdlib.h>

#include <stdint.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <fcntl.h>
#include <unistd.h>

#include <strings.h>

#include <sys/mman.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <sys/syscall.h>
#include <signal.h>


#include <sys/eventfd.h>

#include <errno.h>

#include <semaphore.h>
#include <sys/queue.h>


#ifdef __arm__
#include <linux/user.h>

//blatantly stolen from the kernel source
#define PTRACE_GETHBPREGS 29
#define PTRACE_SETHBPREGS 30

/* Breakpoint */
#define ARM_BREAKPOINT_EXECUTE  0

/* Watchpoints */
#define ARM_BREAKPOINT_LOAD     1
#define ARM_BREAKPOINT_STORE    2

/* Privilege Levels */
#define ARM_BREAKPOINT_PRIV     1
#define ARM_BREAKPOINT_USER     2

/* Lengths */
#define ARM_BREAKPOINT_LEN_1    0x1
#define ARM_BREAKPOINT_LEN_2    0x3
#define ARM_BREAKPOINT_LEN_4    0xf
#define ARM_BREAKPOINT_LEN_8    0xff

static inline unsigned int encode_ctrl_reg(int mismatch, int len, int type, int privilege, int enabled)
{
        return (mismatch << 22) | (len << 5) | (type << 3) | (privilege << 1) | enabled;
}


#endif

#if defined(__i386__) || defined(__x86_64__)
#include <sys/user.h>
#endif




#include "api.h"
#include "porthelp.h"
#include "ceserver.h"
#include "threads.h"

//#include <vector>
sem_t sem_DebugThreadEvent;

pthread_mutex_t memorymutex;
pthread_mutex_t debugsocketmutex;
//pthread_mutex_t mut_RPM;

int log=0;

typedef struct
{
  int ReferenceCount;
  int processListIterator;
  int processCount;
  PProcessListEntry processList;
} ProcessList, *PProcessList;

typedef struct
{
  int ReferenceCount;
  int moduleListIterator;
  int moduleCount;
  PModuleListEntry moduleList;
} ModuleList, *PModuleList;




int WakeDebuggerThread()
{
  sem_post(&sem_DebugThreadEvent);
}

void mychildhandler(int signal, struct siginfo *info, void *context)
{
  //printf("Child event: %d\n", info->si_pid);
  int orig_errno = errno;
  WakeDebuggerThread();
  errno = orig_errno;
}

int GetDebugPort(HANDLE hProcess)
//return the debugserver fd
{
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);
    if (p->isDebugged)
    {
      return p->debuggerServer;
    }
  }

  return -1;

}

int StartDebug(HANDLE hProcess)
{
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);

    struct sigaction childactionhandler;
    if (p->isDebugged)
    {
      printf("Trying to start debugging a process that is already debugged\n");
      return FALSE;
    }

    //attach to each task
    //first get a threadlist
    InitializeProcessThreadlist(p);

    //setup an event
    memset(&childactionhandler, 0, sizeof(childactionhandler));
    childactionhandler.sa_handler=(void *)mychildhandler;
    childactionhandler.sa_flags=SA_SIGINFO;



    sigaction(SIGCHLD, &childactionhandler, NULL);




    //read the taskid's from /proc/pid/task
    char _taskdir[255];
    DIR *taskdir;

    sprintf(_taskdir, "/proc/%d/task", p->pid);

    taskdir=opendir(_taskdir);

    if (taskdir)
    {
      struct dirent *d;

      d=readdir(taskdir);
      while (d)
      {
        int tid=atoi(d->d_name);

        if (tid)
        {
          ThreadData td;
          memset(&td, 0, sizeof(td));
          td.tid=tid;
          td.isPaused=0;
          AddThreadToProcess(p, &td);

          pthread_mutex_lock(&memorymutex); //so there's no ptrace_attach busy when attaching after opening and reading memory

          if (ptrace(PTRACE_ATTACH, tid,0,0)<0)
            printf("Failed to attach to thread %d\n", tid);
          else
          {
            DebugEvent createThreadEvent;



            if (p->isDebugged==0)
            {
              p->isDebugged=1; //at least one made it...
              p->debuggerThreadID=pthread_self();

              socketpair(PF_LOCAL, SOCK_STREAM, 0, &p->debuggerServer);

              //first event, create process
              DebugEvent createProcessEvent;
              createProcessEvent.debugevent=-2; //create process
              createProcessEvent.threadid=p->pid;
              AddDebugEventToQueue(p, &createProcessEvent);

            }

            createThreadEvent.debugevent=-1; //create thread event (virtual event)
            createThreadEvent.threadid=tid;
            AddDebugEventToQueue(p, &createThreadEvent);


           // p->debuggerThreadID=syscall(SYS_gettid);
          }

          pthread_mutex_unlock(&memorymutex);

        }

        d=readdir(taskdir);
      }

      closedir(taskdir);

    }
    else
    {
      printf("Failure opening %s",_taskdir);
    }


    return p->isDebugged;

  }
  else
  {
    printf("Invalid handle\n");
    return FALSE;
  }

}

int SetBreakpoint(HANDLE hProcess, int tid, void *Address, int bptype, int bpsize)
/*
 * Sets a breakpoint of the specifed type at the given address
 * tid of -1 means ALL threads
 * bptype: 0 = Execute
 * bptype: 1 = write
 * bptype: 2 = read (only)
 * bptype: 3 = Read/Write
 *
 * returns TRUE if the breakpoint was set *
 */
{
  int result=FALSE;


  printf("SetBreakpoint(%d, %p, %d, %d)\n", tid, Address, bptype, bpsize);
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);



    if (p->debuggerThreadID==pthread_self())
    {

      int isdebugged=p->debuggedThreadEvent.threadid;
      int wtid;
      DebugEvent de;



      printf("SetBreakpoint from debuggerthread\n");

      if (tid==-1)
      {
        int i,r;
        printf("Calling SetBreakpoint for all threads\n");

        for (i=0; i<p->threadlistpos; i++)
        {
          r=SetBreakpoint(hProcess, p->threadlist[i].tid, Address, bptype, bpsize);
          if (r)
            result=TRUE; //at least one thread succeeded
        }

      }
      else
      {
        PThreadData td=GetThreadData(p, tid);
        int wasPaused;

        if (td==NULL) //should NEVER happen
          return FALSE;

        wasPaused=td->isPaused;

        printf("Calling setbreakpoint for thread :%d\n", tid);

        printf("isdebugged=%d\n", isdebugged);

        if (wasPaused==0)
        {
          //manual
          printf("Target thread wasn't stopped yet\n");

          int k=0;

          printf("td=%p\n", td);
          printf("td->isPaused=%d\n", td->isPaused);




          wtid=tid;
          while ((td) && (td->isPaused==0) && (k<10))
          {
            printf("Not yet paused\n");
            syscall(__NR_tkill, tid, SIGSTOP);

            if (WaitForDebugEventNative(p, &de, tid, 100))
            {
              wtid=de.threadid;
              break;
            }
            k++;
          }



          if (wtid!=tid)
          {
            printf("<<<================UNEXPECTED TID (wtid=%d tid=%d)================>>>\n", wtid, tid);
          }

          printf("k=%d\n", k);

          if (k==10)
          {
            printf("Timeout when waiting for thread\n");
          }
        }
        else
        {
          printf("The thread I wanted to break was already broken. Yeeeh\n");
          wtid=isdebugged;
        }

        //debugging the given tid
        printf("Setting breakpoint in thread %d\n", wtid);

#ifdef __arm__
    //hwbps
        int val;

        if (ptrace(PTRACE_GETHBPREGS, wtid, 0, &val)==0)
        {
          int i;
          unsigned int hwbpreg;
          printf("BPREG0 (Info)=%x\n", val);

          printf("Setting bp address\n");



          if (bptype==0)
          {
            //execute


            i=ptrace(PTRACE_SETHBPREGS, wtid, 1, &Address);
            printf("i1=%d\n", i, hwbpreg);

            //right now i'm not really sure how the breakpoint len is set and why it works in some cases and why not in other cases
            result=i==0;

            hwbpreg=encode_ctrl_reg(0, ARM_BREAKPOINT_LEN_4, ARM_BREAKPOINT_EXECUTE, 0, 1);
            if (ptrace(PTRACE_SETHBPREGS, wtid, 2, &hwbpreg)<0) //according to my guess, this should usually work, but just in case...
            {
              hwbpreg=encode_ctrl_reg(0, ARM_BREAKPOINT_LEN_2, ARM_BREAKPOINT_EXECUTE, 0, 1);
              if (ptrace(PTRACE_SETHBPREGS, wtid, 2, &hwbpreg)<0)
              {
                hwbpreg=encode_ctrl_reg(0, ARM_BREAKPOINT_LEN_1, ARM_BREAKPOINT_EXECUTE, 0, 1);
                if (ptrace(PTRACE_SETHBPREGS, wtid, 2, &hwbpreg)<0)
                {
                  //last try, 8 ?
                  hwbpreg=encode_ctrl_reg(0, ARM_BREAKPOINT_LEN_8, ARM_BREAKPOINT_EXECUTE, 0, 1);
                  if (ptrace(PTRACE_SETHBPREGS, wtid, 2, &hwbpreg)<0)
                  {
                    printf("Failure to set breakpoint\n");
                    result=FALSE;
                  }
                }

              }
            }

            printf("hwbpreg=%x\n", hwbpreg);
          }
          else
          {
            //watchpoint
            //(negative)
            int btype;

            printf("watchpoint\n");

            i=ptrace(PTRACE_SETHBPREGS, wtid, -1, &Address);
            printf("i1=%d\n", i, hwbpreg);

            btype=0;
            if (bptype==1)
              btype=ARM_BREAKPOINT_STORE;
            else
            if (bptype==2)
              btype=ARM_BREAKPOINT_LOAD;
            else
            if (bptype==3)
              btype=ARM_BREAKPOINT_STORE | ARM_BREAKPOINT_LOAD;

            hwbpreg=encode_ctrl_reg(0, ARM_BREAKPOINT_LEN_4, btype, 0, 1);
            i=ptrace(PTRACE_SETHBPREGS, wtid, -2, &hwbpreg);

            printf("i=%d  (hwbpreg=%x)\n", i, hwbpreg);
            result=i==0;

          }

        }


#endif

#if defined __i386__ || defined __x86_64__
        //debug regs
        //PTRACE_SETREGS
        int r,r2;

        DWORD newdr7;


        newdr7=1; //currently implement only 1 bp

        if (bptype==2) //x86 does not support read onlyhw bps
          bptype=3;

        newdr7=newdr7 | (bptype << 16); //bptype

        if (bpsize<=1)
          newdr7=newdr7 | (0 << 18);
        else
        if (bpsize<=2)
          newdr7=newdr7 | (1 << 18);
        else
          newdr7=newdr7 | (3 << 18);


        r=ptrace(PTRACE_POKEUSER, wtid, offsetof(struct user, u_debugreg[0]), Address);
        r2=ptrace(PTRACE_POKEUSER, wtid, offsetof(struct user, u_debugreg[7]), newdr7);

        result=(r==0) && (r2==0);
        if (!result)
        {
          printf("Failure setting breakpoint\n");
        }

        printf("result=%d  (r=%d r2=%d)\n", result, r, r2);


#endif


        if (wasPaused==0)
        {
          int r;

          printf("Continue self broken thread\n");

          if (de.debugevent!=SIGSTOP) //in case a breakpoint or something else happened before sigstop happened
          {


            printf("Not a SIGSTOP. Adding to queue and leave suspended\n");
            AddDebugEventToQueue(p, &de);
            td->isPaused=1; //mark as paused for other api's
          }
          else
          {

            r=ptrace(PTRACE_CONT, wtid, 0,0);
            printf("PTRACE_CONT=%d\n", r);

            td->isPaused=0;
          }
        }
      }

    //

      printf("end of SetBreakpoint reached. result=%d\n", result);


    }
    else
    {
      //not the debugger thread. Send a message to the debuggerthread to execute this command
      printf("SetBreakpoint from outside the debuggerthread. Waking debuggerthread\n");
      //setup a sb command
  #pragma pack(1)
      struct
      {
        char command;
        HANDLE hProcess;
        int tid;
        unsigned long long Address;
        int bptype;
        int bpsize;
      } sb;
  #pragma pack()

      sb.command=CMD_SETBREAKPOINT;
      sb.hProcess=hProcess;
      sb.tid=tid;
      sb.Address=(uintptr_t)Address;
      sb.bptype=bptype;
      sb.bpsize=bpsize;

      if (pthread_mutex_lock(&debugsocketmutex) == 0)
      {
        printf("Sending message to the debuggerthread\n");

        sendall(p->debuggerClient, &sb, sizeof(sb), 0);
        WakeDebuggerThread();
        recvall(p->debuggerClient, &result, sizeof(result), MSG_WAITALL);

        printf("Received reply from debugger thread: %d\n", result);


        pthread_mutex_unlock(&debugsocketmutex);
      }

    }

  }
  else
  {
    printf("Invalid handle\n");
  }

  return result;

}

int RemoveBreakpoint(HANDLE hProcess, int tid)
/*
 * Removes the breakpoint (only 1 for now)
 */
{
  int result=FALSE;

  printf("RemoveBreakpoint(%d)\n", tid);
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);

    if (p->debuggerThreadID==pthread_self())
    {
      int isdebugged=p->debuggedThreadEvent.threadid;
      int wtid;
      DebugEvent de;

      printf("Called from the debuggerthread itself\n");

      if (tid==-1)
      {
        int i;
        printf("Calling RemoveBreakpoint for all threads\n");
        for (i=0; i<p->threadlistpos; i++)
          RemoveBreakpoint(hProcess, p->threadlist[i].tid);
      }
      else
      {
        printf("specific thread\n");

        PThreadData td=GetThreadData(p, tid);
        int wasPaused=td->isPaused;

        if (wasPaused==0)
        {
          //manual
          printf("Not currently paused\n");
          printf("Going to kill and wait for this thread\n");

          int k;


          k=0;
          while ((td) && (td->isPaused==0) && (k<10))
          {
            syscall(__NR_tkill, tid, SIGSTOP);
            if (WaitForDebugEventNative(p, &de, tid, 100))
              break;
            k++;
          }

          wtid=de.threadid;


          printf("----AFTER WAIT----\n");

          printf("after wtid=%d\n", wtid);
          printf("^^^^AFTER WAIT^^^^\n");
        }
        else
        {
          printf("The thread I wanted to break was already broken. Yeeeh\n");
          wtid=isdebugged;
        }

        //debugging the given tid
        printf("Removing breakpoint from thread %d\n", wtid);


#ifdef __arm__
        int bpreg=0;
        int i,i2,i3;
        void *a=NULL;

        printf("arm\n");

        i=ptrace(PTRACE_SETHBPREGS, wtid, -1, &bpreg);
        printf("i1=%d\n", i);
        i2=ptrace(PTRACE_SETHBPREGS, wtid, 2, &bpreg);
        printf("i2=%d\n", i2);



        i3=ptrace(PTRACE_SETHBPREGS, wtid, 1, &a);

        result=(i==0) && (i2==0) && (i3==0);
#endif

#if defined(__i386__) || defined (__x86_64__)
        int r;
        printf("x86\n");

        r=ptrace(PTRACE_POKEUSER, wtid, offsetof(struct user, u_debugreg[7]), 0);
        if (r==0)
          result=TRUE;
        else
          printf("Failure removing breakpoint from thread %d\n", wtid);


#endif

        if (wasPaused==0)
        {
          int r;
          PThreadData td=GetThreadData(p, tid);

          printf("Continue self broken thread\n");

          if (de.debugevent!=SIGSTOP) //in case a breakpoint or something else happened before sigstop happened
          {
            printf("Not a SIGSTOP. Adding to queue and leave suspended\n");
            AddDebugEventToQueue(p, &de);


            td->isPaused=1;
          }
          else
          {
            r=ptrace(PTRACE_CONT, wtid, 0,0);
            printf("PTRACE_CONT=%d\n", r);

            td->isPaused=0;
          }
        }


      }

    }
    else
    {
      printf("Called from a secondary thread\n");
#pragma pack(1)
      struct
      {
        char command;
        HANDLE hProcess;
        int tid;
      } rb;
#pragma pack()

      rb.command=CMD_REMOVEBREAKPOINT;
      rb.hProcess=hProcess;
      rb.tid=tid;


      if (pthread_mutex_lock(&debugsocketmutex) == 0)
      {

        printf("Sending message to the debuggerthread\n");

        sendall(p->debuggerClient, &rb, sizeof(rb), 0);
        WakeDebuggerThread();
        recvall(p->debuggerClient, &result, sizeof(result), MSG_WAITALL);


        pthread_mutex_unlock(&debugsocketmutex);
      }

    }

  }
  else
    printf("Invalid handle\n");

  return result;
}

int GetThreadContext(HANDLE hProcess, int tid, PCONTEXT Context, int type)
/*
 * Gets the context of the given thread
 * Freezes/Resumes the thread for you if it isn't suspended yet
 * type is the data to be gathered (currently ignored but may be used in the future for specific data)
 */
{
  int r=FALSE;
  printf("GetThreadContext(%d)\n", tid);

  if (tid<=0)
  {
    printf("Invalid tid\n");
    return FALSE;
  }

  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);



    if (p->debuggerThreadID==pthread_self())
    {
      PThreadData td=GetThreadData(p, tid);

      printf("Inside debuggerthread\n");

      if (td)
      {
        DebugEvent de;
        int wasPaused=td->isPaused;
        int k=0;


        while ((td->isPaused==0) && (k<10))
        {
          printf("This thread was not paused. Pausing it\n");
          syscall(__NR_tkill, tid, SIGSTOP);
          if (WaitForDebugEventNative(p, &de, tid, 100))
            break;

          k++;
        }

        //the thread is paused, so fetch the data



        k=ptrace(PTRACE_GETREGS, tid, 0, &Context->regs);
        printf("ptrace returned %d\n", k);

        if (k==0)
          r=TRUE;
        else
          r=FALSE;


        if (!wasPaused)
        {
          //continue if sigstop
          PThreadData td=GetThreadData(p, tid);

          printf("The thread was not paused, so resuming it now\n");

          if (de.debugevent!=SIGSTOP) //in case a breakpoint or something else happened before sigstop happened
          {
            printf("Not a SIGSTOP. Adding to queue and leave suspended\n");
            AddDebugEventToQueue(p, &de);
            td->isPaused=1;
          }
          else
          {
            r=(r && (ptrace(PTRACE_CONT, de.threadid, 0,0)==0));


            td->isPaused=0;
            printf("r=%d\n", r);
          }
        }


      }
      else
        printf("Invalid tid\n");

    }
    else
    {
      printf("Not the debugger thread. Pass to serverthread");
#pragma pack(1)
      struct
      {
        char command;
        HANDLE hProcess;
        int tid;
        int type;
      } gtc;
#pragma pack()

      gtc.command=CMD_GETTHREADCONTEXT;
      gtc.hProcess=hProcess;
      gtc.tid=tid;
      gtc.type=type;


      if (pthread_mutex_lock(&debugsocketmutex) == 0)
      {
        printf("Sending message to the debuggerthread\n");

        sendall(p->debuggerClient, &gtc, sizeof(gtc), 0);
        WakeDebuggerThread();
        recvall(p->debuggerClient, &r, sizeof(r), MSG_WAITALL);

        if (r)
        {
          //followed by the contextsize
          uint32_t structsize;

          recvall(p->debuggerClient, &structsize, sizeof(structsize), MSG_WAITALL);
          recvall(p->debuggerClient, &Context->regs, structsize, MSG_WAITALL); //and context
        }


        pthread_mutex_unlock(&debugsocketmutex);
      }

    }
  }
  else
    printf("invalid handle\n");


  return r;
}

int SetThreadContext(HANDLE hProcess, int tid, void *Context, int type)
/*
 * Sets the context of the given thread
 * Fails if the thread is not suspended first
 */
{

}

int SuspendThread(HANDLE hProcess, int tid)
/*
 * Increase the suspendcount. If it was 0, stop the given thread if it isn't stopped yet
 * If the thread was already stopped, look up this event in the queue and remove it
 * if it's the currently debugged thread it will not be in the queue, but continueFromDebugEvent
 * will see it is suspended, and as a result, NOT continue it, but store the debug event into the thread
 * If it wasn't stopped, stop it, and fetch the debug event
 */
{
  int result=-1;

  printf("SuspendThread(%d)\n", tid);
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);
    PThreadData t=GetThreadData(p, tid);

    if (t==NULL)
    {
      printf("Invalid thread\n");
      return -1;
    }

    if (p->debuggerThreadID==pthread_self())
    {
      //inside the debuggerthrad
      printf("Inside the debugger thread.\n");

      if (t->isPaused)
      {
        printf("Already paused\n");

        if (t->suspendCount==0)
        {
          //first time suspend. Gather the debug event and remove it from the queue (hardly ever, most of the time it's just the currently broken thread)

          if (p->debuggedThreadEvent.threadid==tid) //it's the currently suspended thread
            t->suspendedDevent=p->debuggedThreadEvent;
          else
          {
            //go through the queuelist to find the debug event and remove it
            t->suspendedDevent=*FindThreadDebugEventInQueue(p,tid);
            RemoveThreadDebugEventFromQueue(p, tid);
          }
        }

        t->suspendCount++;


      }
      else
      {
        DebugEvent de;
        printf("Not yet paused\n");

        while (t->isPaused==0)
        {
          syscall(__NR_tkill, tid, SIGSTOP);
          if (WaitForDebugEventNative(p, &t->suspendedDevent, tid, 100))
            break;

        }

        t->suspendCount++;

      }

      return t->suspendCount;


      //PThreadData t=GetThreadData(tid);
    }
    else
    {
      printf("Not from the debugger thread. Switching...\n");
#pragma pack(1)
      struct
      {
        char command;
        HANDLE hProcess;
        int tid;
      } st;
#pragma pack()

      st.command=CMD_SUSPENDTHREAD;
      st.hProcess=hProcess;
      st.tid=tid;

      if (pthread_mutex_lock(&debugsocketmutex) == 0)
      {
        sendall(p->debuggerClient, &st, sizeof(st), 0);
        WakeDebuggerThread();
        recvall(p->debuggerClient, &result, sizeof(result), MSG_WAITALL);

        pthread_mutex_unlock(&debugsocketmutex);


      }
    }
  }
  else
  {
    printf("invalid handle\n");
    result=-1;
  }

  return result;

}

int ResumeThread(HANDLE hProcess, int tid)
/*
 * Decrease suspendcount. If 0, resume the thread by adding the stored debug event back to the queue
 */
{
  int result;

  printf("ResumeThread(%d)\n", tid);
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);
    PThreadData t=GetThreadData(p, tid);

    if (t==NULL)
    {
      printf("Invalid thread\n");
      return -1;
    }

    if (p->debuggerThreadID==pthread_self())
    {
      //inside the debuggerthrad
      printf("Inside the debugger thread.\n");

      if ((t->isPaused) && (t->suspendCount>0))
      {

        t->suspendCount--;

        result=t->suspendCount;


        if (t->suspendCount==0)
        {
          //reached 0, continue process if sigstop, else add to queue
          PThreadData td=GetThreadData(p, tid);
          printf("suspeneCount==0\n");

          if (t->suspendedDevent.debugevent==SIGSTOP)
          {
            printf("SIGSTOP: Continue thread without queing\n");
            ptrace(PTRACE_CONT, t->suspendedDevent.threadid, 0,0);
            td->isPaused=0;
          }
          else
          {
            printf("Not a SIGSTOP Add to the event queue\n");
            td->isPaused=1;
            AddDebugEventToQueue(p, &t->suspendedDevent);
            WakeDebuggerThread();
          }
        }
      }
      else
      {
        printf("Failure resuming this thread\n");

      }


      //PThreadData t=GetThreadData(tid);
    }
    else
    {
      printf("Not from the debugger thread. Switching...\n");
#pragma pack(1)
      struct
      {
        char command;
        HANDLE hProcess;
        int tid;
      } rt;
#pragma pack()

      rt.command=CMD_RESUMETHREAD;
      rt.hProcess=hProcess;
      rt.tid=tid;

      if (pthread_mutex_lock(&debugsocketmutex) == 0)
      {
        sendall(p->debuggerClient, &rt, sizeof(rt), 0);
        WakeDebuggerThread();
        recvall(p->debuggerClient, &result, sizeof(result), MSG_WAITALL);

        pthread_mutex_unlock(&debugsocketmutex);
      }
    }
  }
  else
  {
    printf("invalid handle\n");
    result=-1;
  }

  return result;
}

int RemoveThreadDebugEventFromQueue(PProcessData p, int tid)
/*
 * removes the queue for the debug event
 */
{
  int result;
  struct DebugEventQueueElement *deqe;

  pthread_mutex_lock(&p->debugEventQueueMutex);

  deqe=p->debugEventQueue.tqh_first;
  while (deqe)
  {
    if (deqe->de.threadid==tid)
    {
      TAILQ_REMOVE(&p->debugEventQueue, deqe, entries);

      free(deqe);
      break;
    }

    deqe->entries.tqe_next;
  }


  pthread_mutex_unlock(&p->debugEventQueueMutex);
  return result;
}

PDebugEvent FindThreadDebugEventInQueue(PProcessData p, int tid)
/*
 * Finds the DebugEvent for this specific thread if there is one
 * Note that it returns a pointer to it. If you plan on removing it from the queue, copy the results manually
 */
{
  PDebugEvent result=NULL;
  struct DebugEventQueueElement *deqe;
  pthread_mutex_lock(&p->debugEventQueueMutex);

  deqe=p->debugEventQueue.tqh_first;
  while (deqe)
  {
    if (deqe->de.threadid==tid)
    {
      result=&deqe->de;
      break;
    }

    deqe->entries.tqe_next;
  }


  pthread_mutex_unlock(&p->debugEventQueueMutex);
  return result;
}

void AddDebugEventToQueue(PProcessData p, PDebugEvent devent)
{
  struct DebugEventQueueElement *deqe;

  if (devent->debugevent==SIGSTOP)
  {
    printf("<<<<<--------------------SIGSTOP ADDED TO THE QUEUE!\n");
  }

  pthread_mutex_lock(&p->debugEventQueueMutex);

  deqe=malloc(sizeof(struct DebugEventQueueElement));
  deqe->de=*devent;

  TAILQ_INSERT_TAIL(&p->debugEventQueue, deqe, entries);

  pthread_mutex_unlock(&p->debugEventQueueMutex);
}

int GetStopSignalFromThread(int tid)
{
  siginfo_t si;
  if (ptrace(PTRACE_GETSIGINFO, tid, NULL, &si)==0)
    return si.si_signo;
  else
    return -1;
}

int WaitForDebugEventNative(PProcessData p, PDebugEvent devent, int tid, int timeout)
/*
 * Waits for a debug event for a specific thread, queues the devent if not the expected tid
 * Only call this from a debugger thread
 */
{
  int currentTID;
  int status;
  int r;
  int i;





  //printf("WaitForDebugEventNative (p=%p, devent=%p, tid=%d timeout=%d)\n", p, devent, tid, timeout);


  //first check if there is already a thread waiting
  currentTID=1;
  while (currentTID>0)
  {
    currentTID=waitpid(tid, &status, __WALL | WNOHANG);

    //printf("First waitid=%d\n", currentTID);

    if (currentTID>0)
    {
      devent->threadid=currentTID;
      devent->debugevent=GetStopSignalFromThread(devent->threadid);

      PThreadData td=GetThreadData(p, currentTID);
      if (td)
        td->isPaused=1;


      if ((tid==-1) || (currentTID==tid))
        return TRUE;

      //still here, this wasn't what I was looking for...
      //add it to the queue

      //printf("Unexpected event from thread %d while waiting for %d\n", currentTID, tid);



      AddDebugEventToQueue(p, devent);
    }
    //try again, perhaps there is another one available right now
  }

 // printf("Checking for debug server command\n");

//  fflush(stdout);

  //still here
  //CheckForAndDispatchCommand(p->debuggerServer);


 // printf("After check and dispatch\n");
 // fflush(stdout);


  if (timeout>=0)
  {
    //wait till there is an event

    if (timeout>0)
    {
      struct timespec abstime;
      struct timeval current,wanted, diff;
      int timedwait;

     // printf("timed wait\n");

      memset(&abstime, 0, sizeof(abstime));
      gettimeofday(&current,NULL);

      diff.tv_sec=(timeout / 1000);
      diff.tv_usec=(timeout % 1000)*1000;

      timeradd(&current, &diff, &wanted);

      abstime.tv_sec=wanted.tv_sec;
      abstime.tv_nsec=wanted.tv_usec*1000;

      currentTID=-1;
      while (currentTID<=0)
      {
        timedwait=sem_timedwait(&sem_DebugThreadEvent, &abstime);
       // if (log==1)
      //    printf("log=1: sem_timedwait=%d\n", timedwait);

        if (timedwait==0)
        {
          //it got signaled
          //check if there is a debugger thread message waiting
         // if (log==1)
         //   printf("Checking for dispatch command\n");

          i=CheckForAndDispatchCommand(p->debuggerServer);
          //if (log==1)
          //  printf("CheckForAndDispatchCommand returned %d\n", i);


         // printf("Calling waitpid(%d, %p, %x)\n", tid, &status, WNOHANG| __WALL);
          currentTID=waitpid(tid, &status, __WALL | WNOHANG);

         // printf("currentTID = %d\n", currentTID);

          if (currentTID>0)
          {
            devent->threadid=currentTID;
            devent->debugevent=GetStopSignalFromThread(devent->threadid);

            PThreadData td=GetThreadData(p, currentTID);
            if (td)
              td->isPaused=1;

            if ((tid==-1) || (currentTID==tid))
              return TRUE;

            //still here
           // printf("Still here so currentTID(%d) is not the same as tid (%d)\n", currentTID, tid);
            AddDebugEventToQueue(p, devent);
          }
          currentTID=-1; //retry

        }
        else
        {
          if (errno==ETIMEDOUT)
          {
            //printf("timeout\n");
            return FALSE;
          }
          //else
          //  printf("Not a timeout. Retry\n");
        }
      }
    }
    else
    {
      //no wait
      //printf("no wait\n");


    }

  }
  else
  {
    //printf("Infinite wait\n");
    currentTID=-1;
    while (currentTID<0)
    {
      currentTID=waitpid(tid, &status, __WALL);
      if (currentTID>0)
      {
        devent->threadid=currentTID;
        devent->debugevent=GetStopSignalFromThread(devent->threadid);

        PThreadData td=GetThreadData(p, currentTID);
        if (td)
          td->isPaused=1;

        if ((tid==-1) || (currentTID!=tid))
          return TRUE;

        //still here
        AddDebugEventToQueue(p, devent);
      }

      if ((currentTID==-1) && (errno!=EINTR))
      {
        printf("WaitForDebugEventNative: Infinite wait: Could not wait for tid %d (errno=%d)\n", tid, errno);
        return FALSE; //something bad happened
      }

      currentTID=-1;
    }

  }

  //printf("Returning false\n");


  return FALSE;

}


int WaitForDebugEvent(HANDLE hProcess, PDebugEvent devent, int timeout)
/*
 *Waits for the specified timeout in milliseconds, called by the ce gui
 *Does not care about which thread to wait for
 */
{
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);

    if (p->debuggedThreadEvent.threadid==0)
    {
      int r;

      int status;
      int tid;
      struct DebugEventQueueElement *de=NULL;

      //check the queue (first one in the list)
      pthread_mutex_lock(&p->debugEventQueueMutex);
      de=TAILQ_FIRST(&p->debugEventQueue);

      if (de)
        TAILQ_REMOVE(&p->debugEventQueue, de, entries);

      pthread_mutex_unlock(&p->debugEventQueueMutex);

      if (de) //there was a queued event, return it
      {
        printf("Returning queued event (sig=%d,  thread=%d)\n", de->de.debugevent, de->de.threadid);
        if (de->de.debugevent==SIGSTOP)
        {
          printf("<---Something queued a SIGSTOP--->\n");
        }

        *devent=de->de;
        p->debuggedThreadEvent=*devent;
        free(de);
        return 1;
      }

      //still here, so no queued event yet

      r=WaitForDebugEventNative(p, devent, -1, timeout);

      if (r)
        p->debuggedThreadEvent=*devent;

      return r;
    }
    else
    {
      *devent=p->debuggedThreadEvent;
      printf("Can not wait for a debug event when a thread is still paused\n");
      printf("tid=%d  debugevent=%d\n", (int)devent->threadid, devent->debugevent);
      return 1; //success and just handle this event
    }

  }
  return 0;
}

int ContinueFromDebugEvent(HANDLE hProcess, int tid, int ignoresignal)
{
  //printf("ContinueFromDebugEvent called\n");
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);
    PThreadData td=GetThreadData(p, tid);
    siginfo_t si;

   //printf("td==%p\n", td);

    if (p->debuggedThreadEvent.debugevent<0) //virtual debug event. Just ignore
    {
      printf("Virtual event. Ignore\n");
      p->debuggedThreadEvent.threadid=0;
      p->debuggedThreadEvent.debugevent=0;
      return 1; //ignore it
    }


    if (td==NULL)
    {
      printf("Invalid thread\n");
      p->debuggedThreadEvent.threadid=0;

      return 0; //invalid thread
    }

   // printf("td->suspendcount=%d\n", td->suspendCount);

    if (td->suspendCount>0)
    {
      printf("Tried to continue a suspended thread (suspendcount=%d)\n", td->suspendCount);
      return 1; //keep it suspended
    }

   // printf("p->debuggedThreadEvent.debugevent=%d\n", p->debuggedThreadEvent.debugevent);

    //if (p->debuggedThreadEvent.threadid!=tid)
    //  printf("Unexpected thread continue. Expected %d got %d\n", p->debuggedThreadEvent.threadid, tid);




    if (ptrace(PTRACE_GETSIGINFO, tid, NULL, &si)==0)
    {
      int signal=ignoresignal?0:si.si_signo;

     // printf("si.si_signo=%d\n", si.si_signo);
     // printf("si.si_code=%d\n", si.si_code);



      if ((signal==19) || (signal==21)) //STOPPED
      {
        signal=0;
      }

      //printf("Continue %d with signal %d\n", tid, signal);

      int result;
      if (ignoresignal==2)
      {
        printf("Single step\n");

        result=ptrace(PTRACE_SINGLESTEP, tid, 0,0);
        if (result!=0)
        {
          printf("FAIL: %d!\n", errno);
          printf("tid=%d\n", tid);


          while (1);
        }
      }
      else
      {
        result=ptrace(PTRACE_CONT, tid, 0,signal);
      }



     // printf("Continue result=%d\n", result);

      if (td)
        td->isPaused=0;


      if (result<0)
      {
        printf("Failure to continue thread %d with signal %d\n", tid, signal);
        RemoveThreadFromProcess(p, tid);
        p->debuggedThreadEvent.threadid=0;
        return 0;
      }
      else
      {
        p->debuggedThreadEvent.threadid=0;
        return 1;
      }
    }
    else
      printf("Failure getting sig info\n");

  }
  else
    printf("Invalid handle\n");

  return 0;
}

int StopDebug(HANDLE hProcess)
{
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);
    int i;
    for (i=0; i<p->threadlistpos;i++)
      if (ptrace(PTRACE_DETACH, p->threadlist[i].tid,0,0)<0)
        printf("Failed to detach from %d\n", p->threadlist[i]);
  }

  return 1;

}

int WriteProcessMemoryDebug(HANDLE hProcess, PProcessData p, void *lpAddress, void *buffer, int size)
{
  int byteswritten=0;
  int i;

  printf("WriteProcessMemoryDebug:");
  for (i=0; i<size; i++)
  {
    printf("%.2x ", ((unsigned char *)buffer)[i]);
  }

  printf("\n");




  if (p->debuggerThreadID==pthread_self()) //this is the debugger thread
  {
    int isdebugged=p->debuggedThreadEvent.threadid;
    DebugEvent event;

    //printf("ReadProcessMemoryDebug inside debuggerthread (thread debbugged=%d)\n", isdebugged);

    if (!isdebugged)
    {
     // printf("Not currently debugging a thread. Suspending a random thread\n");
      kill(p->pid, SIGSTOP);

      //printf("Going to wait for debug event\n");
      WaitForDebugEventNative(p, &event, -1, -1); //wait for it myself

     // printf("After WaitForDebugEventNative (tid=%d)\n", event.threadid);
    }



    //write the bytes
    {
      int offset=0;
      int max=size-sizeof(long int);

      long int *address=(long int *)buffer;


      while (offset<max)
      {
        printf("offset=%d max=%d\n", offset, max);
        ptrace(PTRACE_POKEDATA, p->pid, (void*)((uintptr_t)lpAddress+offset), (void *)*address);

        address++;
        offset+=sizeof(long int);
        byteswritten+=sizeof(long int);
      }

      if (offset<size)
      {
        printf("Still some bytes left: %d\n", size-offset);
        //still a few bytes left
        long int oldvalue=ptrace(PTRACE_PEEKDATA, p->pid,  (void *)(uintptr_t)lpAddress+offset, (void*)0);

        unsigned char *oldbuf=(unsigned char *)&oldvalue;
        unsigned char *newmem=(unsigned char *)address;
        int i;

        printf("oldvalue=%lx\n", oldvalue);

        for (i=0; i< (size-offset); i++)
          oldbuf[i]=newmem[i];

        printf("newvalue=%lx\n", oldvalue);


        i=ptrace(PTRACE_POKEDATA, p->pid, (void*)((uintptr_t)lpAddress+offset), (void *)oldvalue);

        printf("ptrace poke returned %d\n", i);
        if (i>=0)
          byteswritten+=size-offset;

      }

    }



    if (!isdebugged)
    {
      PThreadData td=GetThreadData(p, event.threadid);

      //if a SIGSTOP happened just continue it, else the next time WaitForDebugEvent() runs it will get this event from the queue
      if (event.debugevent==SIGSTOP)
      {

      //  printf("Continue from sigstop\n");

        ptrace(PTRACE_CONT, event.threadid, 0,0);

        if (td)
          td->isPaused=0;


      }
      else
      {
        printf("WriteProcessMemoryDebug: Adding unexpected signal to eventqueue (event.debugevent=%d event.threadid)\n", event.debugevent, event.threadid);

        AddDebugEventToQueue(p, &event);
        if (td)
          td->isPaused=1;
      }
    }

  }
  else
  {
    printf("WriteProcessMemoryDebug from outside the debuggerthread. Waking debuggerthread\n");

    //setup a rpm command
#pragma pack(1)
    struct
    {
      uint8_t command;   //1
      uint32_t pHandle;  //5
      uint64_t address;  //13
      uint32_t size;     //17
      unsigned char data[size]; //not sure if this works. need testing
    } wpm;
#pragma pack()


    printf("sizeof wpm=%d\n", sizeof(wpm));
    wpm.command=CMD_WRITEPROCESSMEMORY;
    wpm.pHandle=hProcess;
    wpm.address=(uintptr_t)lpAddress;
    wpm.size=size;
    memcpy(wpm.data, buffer, size);



    //and write it to the p->debuggerthreadfd and read out the data
    //aquire lock (I don't want other threads messing with the client socket)
    if (pthread_mutex_lock(&debugsocketmutex) == 0)
    {
      sendall(p->debuggerClient, &wpm, sizeof(wpm), 0);
      WakeDebuggerThread();

      recvall(p->debuggerClient, &byteswritten, sizeof(byteswritten), MSG_WAITALL);

      pthread_mutex_unlock(&debugsocketmutex);

      printf("after recvall byteswritten=%d\n", byteswritten);
    }
  }


  return byteswritten;
}

int WriteProcessMemory(HANDLE hProcess, void *lpAddress, void *buffer, int size)
{
  int written=0;

  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);

    if (p->isDebugged) //&& cannotdealwithotherthreads
    {
      //printf("This process is being debugged\n");
      //use the debugger specific readProcessMemory implementation
      return WriteProcessMemoryDebug(hProcess, p, lpAddress, buffer, size);
    }

    if (pthread_mutex_lock(&memorymutex) == 0)
    {
      if (ptrace(PTRACE_ATTACH, p->pid,0,0)==0)
      {
        int status;
        pid_t pid=wait(&status);
        int offset=0;
        int max=size-sizeof(long int);

        long int *address=(long int *)buffer;


        while (offset<max)
        {
          printf("offset=%d max=%d\n", offset, max);
          ptrace(PTRACE_POKEDATA, pid, (void*)((uintptr_t)lpAddress+offset), (void *)*address);

          address++;
          offset+=sizeof(long int);

          written+=sizeof(long int);
        }

        if (offset<size)
        {
        	printf("Still some bytes left: %d\n", size-offset);
          //still a few bytes left
          long int oldvalue=ptrace(PTRACE_PEEKDATA, pid,  (void *)(uintptr_t)lpAddress+offset, (void*)0);

          unsigned char *oldbuf=(unsigned char *)&oldvalue;
          unsigned char *newmem=(unsigned char *)address;
          int i;

          printf("oldvalue=%lx\n", oldvalue);

          for (i=0; i< (size-offset); i++)
            oldbuf[i]=newmem[i];

          printf("newvalue=%lx\n", oldvalue);


          i=ptrace(PTRACE_POKEDATA, pid, (void*)((uintptr_t)lpAddress+offset), (void *)oldvalue);

          printf("ptrace poke returned %d\n", i);
          if (i>=0)
        	  written+=size-offset;

        }




        ptrace(PTRACE_DETACH, pid,0,0);
      }
      else
        printf("PTRACE ATTACH FAILED\n");

      pthread_mutex_unlock(&memorymutex);
    }



  }

  return written;
}

int ReadProcessMemoryDebug(HANDLE hProcess, PProcessData p, void *lpAddress, void *buffer, int size)
{
  //problem: Only the thread that is currently debugging a stopped thread has read access
  //work around: add a que to the debugger thread that fills in this
  //que a ReadProcessMemory command with the debugger

  //problem 2: If the debugger thread has broken on a breakpoint and another thread wants to read memory it can't
  //e.g: t1:WaitForDebugEvent  t1:Returned. T1: recv  t2: ReadProcessMemory...
  //solution, let the debuggerthread wait for a socket event with select on 2 fd's. One from the connected socket, and one from the que.
  //the que will contain normal rpm command


  int bytesread=0;




  if (p->debuggerThreadID==pthread_self()) //this is the debugger thread
  {
    int isdebugged=p->debuggedThreadEvent.threadid;
    DebugEvent event;

    //printf("ReadProcessMemoryDebug inside debuggerthread (thread debbugged=%d)\n", isdebugged);

    if (!isdebugged)
    {
     // printf("Not currently debugging a thread. Suspending a random thread\n");
      kill(p->pid, SIGSTOP);

      //printf("Going to wait for debug event\n");
      WaitForDebugEventNative(p, &event, -1, -1); //wait for it myself

     // printf("After WaitForDebugEventNative (tid=%d)\n", event.threadid);
    }

    bytesread=-1;
    while (bytesread==-1)
    {
      bytesread=pread(p->mem, buffer, size, (uintptr_t)lpAddress);

      if ((bytesread<0) && (errno!=EINTR))
      {
        printf("pread failed and not due to a signal\n");
        bytesread=0;
        break;
      }
    }

    if (!isdebugged)
    {
      PThreadData td=GetThreadData(p, event.threadid);

      //if a SIGSTOP happened just continue it, else the next time WaitForDebugEvent() runs it will get this event from the queue
      if (event.debugevent==SIGSTOP)
      {

      //  printf("Continue from sigstop\n");

        ptrace(PTRACE_CONT, event.threadid, 0,0);

        if (td)
          td->isPaused=0;


      }
      else
      {
        printf("ReadProcessMemoryDebug: Adding unexpected signal to eventqueue (event.debugevent=%d event.threadid)\n", event.debugevent, event.threadid);

        AddDebugEventToQueue(p, &event);
        if (td)
          td->isPaused=1;
      }
    }


  }
  else
  {

    int tid=p->pid; //p->threadlist[p->threadlistpos-1];
   // printf("ReadProcessMemoryDebug from outside the debuggerthread. Waking debuggerthread\n");

    //setup a rpm command
#pragma pack(1)
    struct
    {
      uint8_t command;
      uint32_t pHandle;
      uint64_t address;
      uint32_t size;
      uint8_t compressed;
    } rpm;
#pragma pack()


    rpm.command=CMD_READPROCESSMEMORY;
    rpm.pHandle=hProcess;
    rpm.address=(uintptr_t)lpAddress;
    rpm.size=size;
    rpm.compressed=0;

    //and write it to the p->debuggerthreadfd and read out the data
    //aquire lock (I don't want other threads messing with the client socket)
    if (pthread_mutex_lock(&debugsocketmutex) == 0)
    {
    //  printf("Sending message to the debuggerthread\n");

      sendall(p->debuggerClient, &rpm, sizeof(rpm), 0);


      //log=1;
      //printf("Waking debugger thread and waiting for result\n");
      WakeDebuggerThread();

      clock_t t1=clock();

      recvall(p->debuggerClient, &bytesread, sizeof(bytesread), MSG_WAITALL);

      clock_t t2=clock();

      //printf("received result after %d\n", t2-t1);
      log=0;
    //  printf("After waiting for debugger thread: bytesread=%d\n", bytesread);

      if (bytesread>0)
        recvall(p->debuggerClient, buffer, bytesread, MSG_WAITALL);


      pthread_mutex_unlock(&debugsocketmutex);
    }

    //bytesread=read...
  }


  return bytesread;
}

int ReadProcessMemory(HANDLE hProcess, void *lpAddress, void *buffer, int size)
{
  //idea in case this is too slow. always read a full page and keep the last 16 accessed pages.
  //only on cache miss, or if the cache is older than 1000 milliseconds fetch the page.
  //keep in mind that this routine can get called by multiple threads at the same time


  //todo: Try process_vm_readv

  //printf("ReadProcessMemory\n");
  int read=0;
  if (GetHandleType(hProcess) == htProcesHandle )
  { //valid handle
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);

    //printf("hProcess=%d, lpAddress=%p, buffer=%p, size=%d\n", hProcess, lpAddress, buffer, size);

    if (p->isDebugged) //&& cannotdealwithotherthreads
    {
      //printf("This process is being debugged\n");
      //use the debugger specific readProcessMemory implementation
      return ReadProcessMemoryDebug(hProcess, p, lpAddress, buffer, size);
    }

    //printf("Read without debug\n");

    if (pthread_mutex_lock(&memorymutex) == 0)
    {


        if (ptrace(PTRACE_ATTACH, p->pid,0,0)==0)
        {
          int status;
          pid_t pid=wait(&status);

          //printf("wait returned %d with status %d\n", pid, status);
          //printf("p->mem=%d\n", p->mem);

          read=pread(p->mem, buffer, size, (uintptr_t)lpAddress);
          if (read==-1)
          {
            read=0;
            printf("pread error for address %p\n", lpAddress);
          }

          ptrace(PTRACE_DETACH, pid,0,0);
        }
        else
          printf("ptrace attach failed (pid=%d)\n", p->pid);


      pthread_mutex_unlock(&memorymutex);
    }
    else
      printf("For some reason I failed to obtain a lock\n");
  }
  else
    printf("RPM: invalid handle\n");

  return read;
}


int VirtualQueryEx(HANDLE hProcess, void *lpAddress, PRegionInfo rinfo)
{
  /*
   * Alternate method: read pagemaps and look up the pfn in /proc/kpageflags (needs to 2 files open and random seeks through both files, so not sure if slow or painfully slow...)
   */

  //VirtualQueryEx stub port. Not a real port, and returns true if successful and false on error
  int found=0;

  //printf("VirtualQueryEx(%p)", lpAddress);

  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);
    FILE *maps=fopen(p->maps, "r");
    if (maps)
    {
      char x[200];
      //printf("Opened %s\n", p->maps);


      rinfo->protection=0;
      rinfo->baseaddress=(uintptr_t)lpAddress & ~0xfff;
      lpAddress=(void *)(uintptr_t)(rinfo->baseaddress);

      while (fgets(x, 200, maps) )
      {
        unsigned long long start=0, stop=0;
        char protectionstring[25];

        sscanf(x, "%llx-%llx %s", &start, &stop, protectionstring);
       // printf("%llx - %llx : %s\n", start,stop, protectionstring);

        if (stop > ((uintptr_t)lpAddress) ) //we passed it
        {
          found=1;

          if (((uintptr_t)lpAddress) >= start )
          {
            //it's inside the region, so useable

            int w,x;

            if (index(protectionstring, 'x'))
              x=1;
            else
              x=0;

            if (index(protectionstring, 'w'))
              w=1;
            else
              w=0;

            if (x)
            {
              //executable
              if (w)
                rinfo->protection=PAGE_EXECUTE_READWRITE;
              else
                rinfo->protection=PAGE_EXECUTE_READ;
            }
            else
            {
              //not executable
              if (w)
                rinfo->protection=PAGE_READWRITE;
              else
                rinfo->protection=PAGE_READONLY;
            }

            rinfo->size=stop-rinfo->baseaddress;
          }
          else
          {
            rinfo->size=start-rinfo->baseaddress;
            rinfo->protection=PAGE_NOACCESS;
          }





          break;
        }



      }


      fclose(maps);

      return found;
    }
    else
    {
      printf("failed opening %s\n", p->maps);
    }
  }

  return 0;
}

int SearchHandleListProcessCallback(PProcessData data, int *pid)
/*
 * Callback. Called during the call to SearchHandleList
 * Searchdata is a pointer to the processid to be looked for
 */
{
  return (data->pid==*pid);
}

HANDLE OpenProcess(DWORD pid)
{
  //check if the process exists
  char processpath[100];
  int handle;
  sprintf(processpath, "/proc/%d/", pid);


  //check if this process has already been opened
  handle=SearchHandleList(htProcesHandle, SearchHandleListProcessCallback, &pid);
  if (handle)
  {
    printf("Already opened. Returning same handle\n");
    PProcessData p=(PProcessData)GetPointerFromHandle(handle);
    p->ReferenceCount++;
    return handle;
  }

  //still here, so not opened yet


  if (chdir(processpath)==0)
  {
    //success

    //create a process info structure and return a handle to it
    PProcessData p=(PProcessData)malloc(sizeof(ProcessData));

    p->ReferenceCount=1;
    p->pid=pid;
    p->path=strdup(processpath);

    sprintf(processpath,"/proc/%d/maps", pid);
    p->maps=strdup(processpath);

    sprintf(processpath,"/proc/%d/mem", pid);
    p->mem=open(processpath, O_RDONLY);


    p->isDebugged=0;

    p->threadlistmax=0;
    p->threadlistpos=0;
    p->threadlist=NULL;

    p->debuggedThreadEvent.threadid=0;

    pthread_mutex_init(&p->debugEventQueueMutex, NULL);

    /*
    p->queuedDebugEventList=NULL;
    p->queuedDebugEventListPos=0;
    p->queuedDebugEventListMax=0;
    */

    TAILQ_INIT(&p->debugEventQueue);






    return CreateHandleFromPointer(p, htProcesHandle);


  }
  else
    return 0; //could not find the process

}


BOOL Process32Next(HANDLE hSnapshot, PProcessListEntry processentry)
{
  //get the current iterator of the list and increase it. If the max has been reached, return false
 // printf("Process32Next\n");

  if (GetHandleType(hSnapshot) == htTHSProcess)
  {
    PProcessList pl=(PProcessList)GetPointerFromHandle(hSnapshot);

    if (pl->processListIterator<pl->processCount)
    {
      processentry->PID=pl->processList[pl->processListIterator].PID;
      processentry->ProcessName=pl->processList[pl->processListIterator].ProcessName; //no need to copy
      pl->processListIterator++;

      return TRUE;
    }
    else
      return FALSE;
  }
  else
    return FALSE;
}




BOOL Process32First(HANDLE hSnapshot, PProcessListEntry processentry)
{
  //Get a processentry from the processlist snapshot. fill the given processentry with the data.

 // printf("Process32First\n");
  if (GetHandleType(hSnapshot) == htTHSProcess)
  {
    PProcessList pl=(PProcessList)GetPointerFromHandle(hSnapshot);
    pl->processListIterator=0;
    return Process32Next(hSnapshot, processentry);
  }
  else
    return FALSE;
}


BOOL Module32Next(HANDLE hSnapshot, PModuleListEntry moduleentry)
{
  //get the current iterator of the list and increase it. If the max has been reached, return false
 // printf("Module32Next\n");

  if (GetHandleType(hSnapshot) == htTHSModule)
  {
    PModuleList ml=(PModuleList)GetPointerFromHandle(hSnapshot);

    if (ml->moduleListIterator<ml->moduleCount)
    {
      moduleentry->baseAddress=ml->moduleList[ml->moduleListIterator].baseAddress;
      moduleentry->moduleName=ml->moduleList[ml->moduleListIterator].moduleName;
      moduleentry->moduleSize=ml->moduleList[ml->moduleListIterator].moduleSize;

      ml->moduleListIterator++;

      return TRUE;
    }
    else
    {
     // printf("Returning false because ml->moduleListIterator=%d and ml->moduleCount=%d\n", ml->moduleListIterator, ml->moduleCount);
      return FALSE;
    }
  }
  else
    return FALSE;
}


BOOL Module32First(HANDLE hSnapshot, PModuleListEntry moduleentry)
{

  //printf("Module32First\n");
  if (GetHandleType(hSnapshot) == htTHSModule)
  {
    PModuleList ml=(PModuleList)GetPointerFromHandle(hSnapshot);
    ml->moduleListIterator=0;
    return Module32Next(hSnapshot, moduleentry);
  }
  else
    return FALSE;
}


HANDLE CreateToolhelp32Snapshot(DWORD dwFlags, DWORD th32ProcessID)
{

  if (dwFlags & TH32CS_SNAPPROCESS)
  {
    //create a processlist which process32first/process32next will make use of. Not often called so you may make it as slow as you wish
    int max=2048;
    PProcessList pl=(PProcessList)malloc(sizeof(ProcessList));

    //printf("Creating processlist\n");

    pl->ReferenceCount=1;
    pl->processCount=0;
    pl->processList=(PProcessListEntry)malloc(sizeof(ProcessListEntry)* max);

    DIR *procfolder=opendir("/proc/");

    struct dirent *currentfile;

    while ((currentfile=readdir(procfolder)) != NULL)
    {

      if (strspn(currentfile->d_name, "1234567890")==strlen(currentfile->d_name))
      {
        int pid;
        char exepath[80];
        char processpath[255];
        sprintf(exepath, "/proc/%s/exe", currentfile->d_name);

        int i=readlink(exepath, processpath, 254);
        if (i != -1)
        {
          char extrafile[255];
          int f;
          processpath[i]=0;

          sprintf(extrafile, "/proc/%s/cmdline", currentfile->d_name);

          f=open(extrafile, O_RDONLY);
          if (i!=-1)
          {
        	i=read(f, extrafile, 255);
        	if (i>=0)
        		extrafile[i]=0;
        	else
        		extrafile[0]=0;

        	strcat(processpath," ");
        	strcat(processpath,extrafile);

            close(f);
          }


          sscanf(currentfile->d_name, "%d", &pid);
         // printf("%d - %s\n", pid, processpath);

          //add this process to the list
          pl->processList[pl->processCount].PID=pid;
          pl->processList[pl->processCount].ProcessName=strdup(processpath);

          pl->processCount++;

          if (pl->processCount>=max)
          {
            max=max*2;
            pl->processList=(PProcessListEntry)realloc(pl->processList, max*sizeof(ProcessListEntry));
          }
        }


      }

    }

    closedir(procfolder);

    return CreateHandleFromPointer(pl, htTHSProcess);
  }
  else
  if (dwFlags & TH32CS_SNAPMODULE)
  {
    //make a list of all the modules loaded by processid th32ProcessID
    //the module list
    int max=64;
    char mapfile[255];
    FILE *f=NULL;
    snprintf(mapfile, 255, "/proc/%d/maps", th32ProcessID);

    PModuleList ml=(PModuleList)malloc(sizeof(ModuleList));

    printf("Creating module list for process %d\n", th32ProcessID);

    ml->ReferenceCount=1;
    ml->moduleCount=0;
    ml->moduleList=(PModuleListEntry)malloc(sizeof(ModuleListEntry)*max);


    f=fopen(mapfile, "r");


    if (f)
    {
      char s[256];

      PModuleListEntry mle=NULL;



      while (fgets(s, 255, f)) //read a line into s
      {
        unsigned long long start, stop;
        char memoryrange[64],modulepath[255];

        modulepath[0]='\0';

        sscanf(s, "%llx-%llx %*s %*s %*s %*s %s\n", &start, &stop, modulepath);

        if (modulepath[0]) //it's something
        {
          if ((mle) && (strcmp(modulepath, mle->moduleName)==0))
          {
            //same module as the last entry, adjust the size to encapsule this (may mark non module memory as module memory)
            mle->moduleSize=stop-(mle->baseAddress);
            continue;
          }

          //new module, or not linkable

//          printf("%llx : %s\n", start, modulepath);


          mle=&ml->moduleList[ml->moduleCount];
          mle->moduleName=strdup(modulepath);
          mle->baseAddress=start;
          mle->moduleSize=stop-start;

          ml->moduleCount++;

          if (ml->moduleCount>=max)
          {
            //printf("reallocate modulelist\n");
            max=max*2;
            ml->moduleList=(PModuleListEntry)realloc(ml->moduleList, max* sizeof(ModuleListEntry));
          }


        }
        else
          mle=NULL;



      }

      fclose(f);

      return CreateHandleFromPointer(ml, htTHSModule);
    }
    else
    {
      //printf("Failed opening %s\n", mapfile);
      return 0;
    }



  }


  return 0;
}

void CloseHandle(HANDLE h)
{
  int i;
  handleType ht=GetHandleType(h);

 // printf("CloseHandle(%d)\n", h);
  if (ht==htTHSModule)
  {
    ModuleList *ml=(PModuleList)GetPointerFromHandle(h);
    ml->ReferenceCount--;
    if (ml->ReferenceCount<=0)
    {
      //free all the processnames in the list
      for (i=0; i<ml->moduleCount; i++)
        free(ml->moduleList[i].moduleName);

      free(ml->moduleList); //free the list
      free(ml); //free the descriptor

      RemoveHandle(h);
    }

  }
  if (ht==htTHSProcess)
  {
    ProcessList *pl=(PProcessList)GetPointerFromHandle(h);


    pl->ReferenceCount--;

    if (pl->ReferenceCount<=0)
    {
      //free all the processnames in the list
      for (i=0; i<pl->processCount; i++)
        free(pl->processList[i].ProcessName);

      free(pl->processList); //free the list
      free(pl); //free the descriptor

      RemoveHandle(h);
    }
  }
  else
  if (ht==htProcesHandle)
  {
    PProcessData pd=(PProcessData)GetPointerFromHandle(h);

    pd->ReferenceCount--;
    if (pd->ReferenceCount<=0)
    {
      free(pd->maps);
      free(pd->path);
      close(pd->mem);
      free(pd);

      RemoveHandle(h);
    }
  }
  else
    RemoveHandle(h); //no idea what it is...


}

void initAPI()
{
  pthread_mutex_init(&memorymutex, NULL);
  pthread_mutex_init(&debugsocketmutex, NULL);

  sem_init(&sem_DebugThreadEvent, 0, 0); //locked by default

}


