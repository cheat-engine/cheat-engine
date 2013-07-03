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
//#include <sys/user.h>
#include <signal.h>


#include <sys/eventfd.h>

#include <errno.h>

#include <semaphore.h>




#include "api.h"
#include "porthelp.h"
#include "ceserver.h"

//#include <vector>
sem_t sem_ChildEvent;

pthread_mutex_t memorymutex;
pthread_mutex_t debugsocketmutex;
//pthread_mutex_t mut_RPM;



typedef struct
{
  int processListIterator;
  int processCount;
  PProcessListEntry processList;
} ProcessList, *PProcessList;


void InitializeProcessThreadlist(PProcessData p)
{
  if (p->threadlist==NULL) //new list
  {
    p->threadlist=malloc(sizeof(int)*64); //preallocate room for 64
    p->threadlistmax=64;
  }

  p->threadlistpos=0;
}

void AddThreadToProcess(PProcessData p, int tid)
{
  if (p->threadlist==NULL)
    InitializeProcessThreadlist(p);

  if (p->threadlistpos==p->threadlistmax)
  {
    //realloc
    p->threadlist=realloc(p->threadlist, sizeof(int)*p->threadlistmax*2);
    if (p->threadlist==NULL)
    {
      printf("REALLOC FAILED!\n");
      exit(2);
    }
    p->threadlistmax=p->threadlistmax*2;

  }

  p->threadlist[p->threadlistpos]=tid;
  p->threadlistpos++;
}

int RemoveThreadFromProcess(PProcessData p, int tid)
{
  int i;
  for (i=0; i<p->threadlistpos; i++)
    if (p->threadlist[i]==tid)
    {
      //found it
      int j;
      for (j=i; j<p->threadlistpos-1; j++)
        p->threadlist[j]=p->threadlist[j+1];

      p->threadlistpos--;
      return 1;
    }

  return 0;

}

volatile int test=0;
void mychildhandler(int signal, struct siginfo *info, void *context)
{
  //printf("Child event: %d\n", info->si_pid);
  int orig_errno = errno;
  sem_post(&sem_ChildEvent);
  errno = orig_errno;

  test++;
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
          AddThreadToProcess(p, tid);

          if (ptrace(PTRACE_ATTACH, tid,0,0)<0)
            printf("Failed to attach to thread %d\n", tid);
          else
          {
            p->isDebugged=1; //at least one made it...
            p->debuggerThreadID=pthread_self();


            socketpair(PF_LOCAL, SOCK_STREAM, 0, &p->debuggerServer);
           // p->debuggerThreadID=syscall(SYS_gettid);
          }

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
 * tid of -1 means ALL breakpoints
 *
 * returns TRUE if the breakpoint was set *
 */
{
  printf("SetBreakpoint(%d)\n", tid);
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);



      //thread specific breakpoint (or recursive call)


    if (p->debuggerThreadID==pthread_self())
    {
      int isdebugged=p->debuggedThread;
      int wtid;

      printf("SetBreakpoint from debuggerthread\n");

      if (tid==-1)
      {
        int i;
        printf("Calling SetBreakpoint for all threads\n");
        for (i=0; i<p->threadlistpos; i++)
          SetBreakpoint(hProcess, p->threadlist[i], Address, bptype, bpsize);
      }
      else
      {
        printf("Calling setbreakpoint for thread :%d\n", tid);

        printf("isdebugged=%d\n", isdebugged);

        if (isdebugged!=tid)
        {
          //manual
          int status;

          printf("Different thread or no thread was broken\n");
          printf("Going to kill and wait for this thread\n");

          int k;
          test=0;
          //k=pthread_kill(tid, SIGSTOP);

          //tkill(tid, SIGSTOP); //ok, this doesn't work...

          //syscall(SYS_tkill, tid, SIGSTOP); //this also doesn't work everywhere

          syscall(__NR_tkill, tid, SIGSTOP); //this does :)

         // printf("SYS_tkill=%d\n",SYS_tkill);
         // printf("__NR_tkill=%d\n",__NR_tkill);


          while ((wtid=waitpid(tid, &status, __WALL))<=0) ;


          printf("----AFTER WAIT----\n");

          printf("after wtid=%d\n", wtid);
          printf("^^^^AFTER WAIT^^^^\n");
        }
        else
          printf("The thread I wanted to break was already broken. Yeeeh\n");

        //debugging the given tid
        printf("Setting breakpoint in thread %d\n", wtid);

        if (isdebugged!=tid)
        {
          int r;
          printf("Continue self broken thread\n");
          r=ptrace(PTRACE_CONT, wtid, 0,0);
          printf("PTRACE_CONT=%d\n", r);
        }
      }

    //


#ifdef __arm__
    //hwbps
#endif

#if defined __i386__ || defined __x86_64__
    //debug regs
    //PTRACE_SETREGS

      /*
      unsigned char *data=malloc(8192*4);

      int i,j;
      for (i=0; i<p->threadlistpos; i++)
      {

        j=ptrace(PTRACE_GETREGS, p->threadlist[i], 0, data);
        printf("PTRACE_GETREGS(%d)=%d\n", p->threadlist[i], j);
      }
*/


#endif
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
        int result;
        printf("Sending message to the debuggerthread\n");

        sendall(p->debuggerClient, &sb, sizeof(sb), 0);

        kill(tid, SIGSTOP); //this will wake the debuggerthread if it was sleeping

        recv(p->debuggerClient, &result, sizeof(result), MSG_WAITALL);


        pthread_mutex_unlock(&debugsocketmutex);
      }

    }

  }
  else
  {
    printf("Invalid handle\n");
  }

}

int WaitForDebugEvent(HANDLE hProcess, PDebugEvent devent, int timeout)
/*
 * Waits for the specified timeout in milliseconds
 */
{
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);

    if (p->debuggedThread==0)
    {
      int status;
      int tid;


      //check if there was a thread waiting since last time
      if (timeout!=-1)
      {
        tid=waitpid(-1, &status, WNOHANG| __WALL);


        if (tid<=0)
        {
          //wait failed, wait for the child signal for at most "timeout" millisecond.

          struct timespec abstime;
          struct timeval current,wanted, diff;
          int timedwait;

          memset(&abstime, 0, sizeof(abstime));
          gettimeofday(&current,NULL);

          diff.tv_sec=(timeout / 1000);
          diff.tv_usec=(timeout % 1000)*1000;


          timeradd(&current, &diff, &wanted);

          abstime.tv_sec=wanted.tv_sec;
          abstime.tv_nsec=wanted.tv_usec*1000;

        //  printf("No child waiting. Going to wait\n");
         // pthread_mutex_lock(&hasDebugSignalMutex);

          while (tid<=0)
          {

            timedwait=sem_timedwait(&sem_ChildEvent, &abstime);
           // timedwait=pthread_cond_timedwait(&hasDebugSignal, &hasDebugSignalMutex, &abstime);

            printf("after wait: %d\n", timedwait);

           // pthread_mutex_unlock(&hasDebugSignalMutex);


            if (timedwait==0)
              tid=waitpid(-1, &status, WNOHANG | __WALL);
            else
            {
              int e=errno;
              printf("errno=%d", errno);
              if (e==ETIMEDOUT)
              {
                printf("=Timeout\n");
                return FALSE; //no debug message
              }
              else
              if (e==EINTR)
              {
                printf("=Interrupted by signal\n");
              }
              else
              if (e==EINVAL)
              {
                printf("=Invalid time\n");
              }
              else
                printf("=WTF?\n");

            }
          }


        }
      }
      else
      {
        tid=-1;
        while ((tid=waitpid(-1, &status, __WALL))==-1)
        {
          if (errno!=EINTR)
          {
            printf("Unknown error during waitpid :%d\n", errno);
            break;
          }
        }

      }


      p->debuggedThread=tid;

      if (WIFEXITED(status))
      {
        RemoveThreadFromProcess(p, tid);
        p->debuggedThreadSignal=-1;
        printf("EXIT\n");
      }
      else
      if (WIFSIGNALED(status))
      {
        p->debuggedThreadSignal=WTERMSIG(status);
        printf("SIGNAL\n");
      }
      else
      if (WIFSTOPPED(status))
      {
        p->debuggedThreadSignal=WSTOPSIG(status);
        printf("STOP\n");


      }
      else
      {
        p->debuggedThreadSignal=0;
        printf("WTF!\n");
      }

      devent->debugevent=p->debuggedThreadSignal;
      devent->threadid=tid;

      if (!WIFEXITED(status))
        printf("%d: Break due to signal %d (status=%x)\n", tid, p->debuggedThreadSignal, status);
      else
        printf("%x terminated\n", tid);


      return 1;
    }
    else
    {
      printf("Can not wait for a debug event when a thread is still paused\n");
      return 1; //success and just handle this event
    }

  }
  return 0;
}

int ContinueFromDebugEvent(HANDLE hProcess, int tid, int ignoresignal)
{
  printf("ContinueFromDebugEvent called\n");
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);
    siginfo_t si;

    if (ptrace(PTRACE_GETSIGINFO, tid, NULL, &si)==0)
    {
      int signal=ignoresignal?0:si.si_signo;

      printf("si.si_signo=%d\n", si.si_signo);



      if ((signal==19) || (signal==21)) //STOPPED
      {
        signal=0;
      }

      printf("Continue %d with signal %d\n", tid, signal);

      int result=ptrace(PTRACE_CONT, tid, 0,signal);


      printf("Continue result=%d\n", result);

      if (result<0)
      {
        printf("Failure to continue thread %d with signal %d\n", tid, signal);
        RemoveThreadFromProcess(p, tid);
        p->debuggedThread=0;
        return 0;
      }
      else
      {
        p->debuggedThread=0;
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
      if (ptrace(PTRACE_DETACH, p->threadlist[i],0,0)<0)
        printf("Failed to detach from %d\n", p->threadlist[i]);
  }

  return 1;

}

int WriteProcessMemory(HANDLE hProcess, void *lpAddress, void *buffer, int size)
{
  int written=0;

  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);

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
    int isdebugged=p->debuggedThread;
    DebugEvent event;

    printf("ReadProcessMemoryDebug inside debuggerthread (isdebugged=%d)\n", isdebugged);

    if (!isdebugged)
    {
      kill(p->pid, SIGSTOP);
      WaitForDebugEvent(hProcess, &event, -1); //wait for it myself
    }

    bytesread=pread(p->mem, buffer, size, (uintptr_t)lpAddress);

    if ((!isdebugged) && (p->debuggedThread) && (p->debuggedThreadSignal==SIGSTOP))
    {
      //if a SIGSTOP happened just continue it, else the next time WaitForDebugEvent() runs it will get this event
      ContinueFromDebugEvent(hProcess, event.threadid ,0);
    }


  }
  else
  {

    int tid=p->pid; //p->threadlist[p->threadlistpos-1];
    printf("ReadProcessMemoryDebug from outside the debuggerthread. Waking debuggerthread\n");

    //setup a rpm command
#pragma pack(1)
    struct
    {
      char command;
      int pHandle;
      unsigned long long address;
      int size;
    } rpm;
#pragma pack()


    rpm.command=CMD_READPROCESSMEMORY;
    rpm.pHandle=hProcess;
    rpm.address=(uintptr_t)lpAddress;
    rpm.size=size;

    //and write it to the p->debuggerthreadfd and read out the data
    //aquire lock (I don't want other threads messing with the client socket)
    if (pthread_mutex_lock(&debugsocketmutex) == 0)
    {
      printf("Sending message to the debuggerthread\n");

      sendall(p->debuggerClient, &rpm, sizeof(rpm), 0);

      kill(tid, SIGSTOP); //this will wake the debuggerthread if it was sleeping

      recv(p->debuggerClient, &bytesread, sizeof(bytesread), MSG_WAITALL);
      printf("bytesread=%d\n", bytesread);
      recv(p->debuggerClient, buffer, bytesread, MSG_WAITALL);


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


  int read=0;
  if (GetHandleType(hProcess) == htProcesHandle )
  { //valid handle
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);

    //printf("hProcess=%d, lpAddress=%p, buffer=%p, size=%d\n", hProcess, lpAddress, buffer, size);

    if (p->isDebugged) //&& cannotdealwithotherthreads
    {
      //use the debugger specific readProcessMemory implementation
      return ReadProcessMemoryDebug(hProcess, p, lpAddress, buffer, size);
    }


    if (pthread_mutex_lock(&memorymutex) == 0)
    {
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
            printf("pread error\n");
          }

          ptrace(PTRACE_DETACH, pid,0,0);
        }
        //else
        //  printf("ptrace attach failed\n");
      }

      pthread_mutex_unlock(&memorymutex);
    }
  }
  else
    printf("RPM: invalid handle\n");

  return read;
}


int VirtualQueryEx(HANDLE hProcess, void *lpAddress, PRegionInfo rinfo)
{
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


HANDLE OpenProcess(DWORD pid)
{
  //check if the process exists
  char processpath[100];
  sprintf(processpath, "/proc/%d/", pid);

  if (chdir(processpath)==0)
  {
    //success

    //create a process info structure and return a handle to it
    PProcessData p=(PProcessData)malloc(sizeof(ProcessData));

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

    p->debuggedThread=0;


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



HANDLE CreateToolhelp32Snapshot(DWORD dwFlags, DWORD th32ProcessID)
{

  if (dwFlags & TH32CS_SNAPPROCESS)
  {
    //create a processlist which process32first/process32next will make use of. Not often called so you may make it as slow as you wish
    int max=2048;
    PProcessList pl=(PProcessList)malloc(sizeof(ProcessList));

    //printf("Creating processlist\n");

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
            pl->processList=(PProcessListEntry)realloc(pl->processList, max);
          }
        }


      }

    }

    closedir(procfolder);

    return CreateHandleFromPointer(pl, htTHSProcess);
  }

  return 0;
}

void initAPI()
{
  pthread_mutex_init(&memorymutex, NULL);
  pthread_mutex_init(&debugsocketmutex, NULL);

 // sem_init(&sem_RPMDone, 0, 0); //locked by default
  sem_init(&sem_ChildEvent, 0, 0); //locked by default

}

void CloseHandle(HANDLE h)
{
  handleType ht=GetHandleType(h);

 // printf("CloseHandle(%d)\n", h);
  if (ht==htTHSProcess)
  {
    ProcessList *pl=(PProcessList)GetPointerFromHandle(h);
    int i;
    //free all the processnames in the list

    for (i=0; i<pl->processCount; i++)
      free(pl->processList[i].ProcessName);

    free(pl->processList); //free the list
    free(pl); //free the descriptor
  }
  else
  if (ht==htProcesHandle)
  {
    PProcessData pd=(PProcessData)GetPointerFromHandle(h);
    free(pd->maps);
    free(pd->path);
    close(pd->mem);
    free(pd);
  }

  RemoveHandle(h);
}




