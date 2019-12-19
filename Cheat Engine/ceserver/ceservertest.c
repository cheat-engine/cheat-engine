/*
 * ceservertest.c
 *
 *  Created on: Jul 1, 2013
 *      Author: eric
 *
 *  this is basically a standalone client process, but because i'm lazy it runs in the same context as the server
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "ceserver.h"
#include "api.h" //for debugevent

int pHandle;

int cenet_connect(void)
{
  int fd;
  int i;
  int PORT=52736;

  struct sockaddr_in addr;

  fd=socket(AF_INET, SOCK_STREAM, 0);


  memset(&addr, 0, sizeof(addr));
  addr.sin_family=AF_INET;
  addr.sin_port=htons(PORT);
  addr.sin_addr.s_addr=htonl(INADDR_LOOPBACK); //0x1600a8c0; //INADDR_LOOPBACK;

  debug_log("calling connect...\n");
  i=connect(fd, (struct sockaddr *)&addr, sizeof(addr));

  debug_log("after connect. %d\n", i);


  return fd;
}

int cenet_OpenProcess(int fd, int pid)
{
#pragma pack(1)
  struct
  {
    char command;
    int pid;
  } op;
#pragma pack()

  int pHandle;

  debug_log("cenet_OpenProcess(%d,%d)\n", fd, pid);



  op.command=CMD_OPENPROCESS;
  op.pid=pid;

  pHandle=0;

  sendall(fd, &op, sizeof(op), 0);
  recv(fd, &pHandle, sizeof(pHandle),MSG_WAITALL);

  return pHandle;
}

int cenet_startDebugger(int fd, int pHandle)
{
#pragma pack(1)
  struct
  {
    char command;
    HANDLE pHandle;
  } sd;
#pragma pack()

  int result;


  sd.command=CMD_STARTDEBUG;
  sd.pHandle=pHandle;

  sendall(fd, &sd, sizeof(sd), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);

  return result;

}

int cenet_waitForDebugEvent(int fd, int pHandle, DebugEvent* devent, int timeout)
{
#pragma pack(1)
  struct
  {
    char command;
    HANDLE pHandle;
    int timeout;
  } wfd;
#pragma pack()


  int result;




  wfd.command=CMD_WAITFORDEBUGEVENT;
  wfd.pHandle=pHandle;
  wfd.timeout=timeout;

  sendall(fd, &wfd, sizeof(wfd), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);
  if (result)
    recv(fd, devent, sizeof(DebugEvent), MSG_WAITALL);

  debug_log(">>>>>>>>>>>>>>>>>>cenet_waitForDebugEvent returned<<<<<<<<<<<<<<<<\n");

  return result;

}

int cenet_continueFromDebugEvent(int fd, int pHandle, int tid, int ignore)
{
#pragma pack(1)
  struct
  {
    char command;
    HANDLE pHandle;
    int tid;
    int ignore;
  } cfd;
#pragma pack()

  int result;


  cfd.command=CMD_CONTINUEFROMDEBUGEVENT;
  cfd.pHandle=pHandle;
  cfd.tid=tid;
  cfd.ignore=ignore;

  sendall(fd, &cfd, sizeof(cfd), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);

  return result;
}

int cenet_readProcessMemory(int fd, int pHandle, unsigned long long address, void *dest, int size)
{
#pragma pack(1)
  struct
  {
    char command;
    int pHandle;
    unsigned long long address;
    int size;
  } rpm;
#pragma pack()

  int result;

  debug_log("cenet_readProcessMemory(%d, %d, %llx, %p, %d)", fd, pHandle, address, dest, size);

  rpm.command=CMD_READPROCESSMEMORY;
  rpm.pHandle=pHandle;
  rpm.address=address;
  rpm.size=size;

  sendall(fd, &rpm, sizeof(rpm), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);

  debug_log("result=%d\n", result);
  recv(fd, dest, result, MSG_WAITALL);


  return result;

}

int cenet_setBreakpoint(int fd, int pHandle, int tid, void *Address, int bptype, int bpsize, int debugreg)
{
#pragma pack(1)
    struct
    {
      char command;
      HANDLE hProcess;
      int tid;
      int debugreg;
      uint64_t Address;
      int bptype;
      int bpsize;
    } sb;
#pragma pack()
    int result;

  debug_log("cenet_setBreakpoint sizeof(sb)=%d\n", sizeof(sb));
  sb.command=CMD_SETBREAKPOINT;
  sb.hProcess=pHandle;
  sb.tid=tid;
  sb.debugreg=debugreg;
  sb.Address=(uintptr_t)Address;
  sb.bptype=bptype;
  sb.bpsize=bpsize;

  sendall(fd, &sb, sizeof(sb), 0);


  recv(fd, &result, sizeof(result), MSG_WAITALL);

  return result;
}

int cenet_removeBreakpoint(int fd, int pHandle, int tid, int debugreg, int waswatchpoint)
{
#pragma pack(1)
    struct
    {
      char command;
      HANDLE hProcess;
      uint32_t tid;
      uint32_t debugreg;
      uint32_t waswatchpoint;
    } rb;
#pragma pack()
    int result;

  debug_log("cenet_removeBreakpoint\n");
  rb.command=CMD_REMOVEBREAKPOINT;
  rb.hProcess=pHandle;
  rb.tid=tid;
  rb.debugreg=debugreg;
  rb.waswatchpoint=waswatchpoint;

  sendall(fd, &rb, sizeof(rb), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);

  return result;
}

#define ARM_DBG_READ(N, M, OP2, VAL) do {\
         asm volatile("mrc p14, 0, %0, " #N "," #M ", " #OP2 : "=r" (VAL));\
} while (0)


int cenet_VirtualQueryExFull(int fd, int pHandle, DWORD flags)
{
#pragma pack(1)
    struct
    {
      char command;
      HANDLE hProcess;
      uint8_t flags;
    } vqef;
#pragma pack()

    vqef.command=CMD_VIRTUALQUERYEXFULL;
    vqef.hProcess=pHandle;
    vqef.flags=flags;

    sendall(fd, &vqef, sizeof(vqef),0);



}

void *CESERVERTEST_DEBUGGERTHREAD(void *arg)
{
  int count=0;
  int fd=cenet_connect();

  int dscr=0;
#ifdef __arm__
  ARM_DBG_READ(c0, c1, 0, dscr);
  debug_log("after: %x\n", dscr);
#endif



  if (cenet_startDebugger(fd, pHandle))
  {
    int i;
    DebugEvent devent;

    debug_log("cenet_startDebugger=true\n");

    while (1)
    {
      count++;
      debug_log("count=%d\n", count);

      if (count==4)
      {
        debug_log("going to set breakpoint\n");

        i=cenet_setBreakpoint(fd, pHandle, -1, 0x00ce0000, 3, 4,0);
        //cenet_setBreakpoint(fd, pHandle, -1, 0x85e4, 0, 1,2);
        //cenet_setBreakpoint(fd, pHandle, -1, 0x85e4, 0, 1,3);
       // cenet_setBreakpoint(fd, pHandle, -1, 0x85e4, 0, 1,4);

        //cenet_setBreakpoint(fd, pHandle, -1, 0xa000, 3, 4, 0);


        debug_log("cenet_setBreakpoint returned %d\n",i);
      }

      i=cenet_waitForDebugEvent(fd, pHandle, &devent, 2000);
      if (i)
      {

        if (devent.debugevent==5)
        {
          debug_log("TRAP (thread %d)\n", devent.threadid);
         // debug_log("Going to remove breakpoint\n");
          //cenet_removeBreakpoint(fd, pHandle, devent.threadid,0,1);

          //printf("After removeBreakpoint\n");

          cenet_continueFromDebugEvent(fd, pHandle, devent.threadid, 2); //single step

          i=cenet_waitForDebugEvent(fd, pHandle, &devent, 2000);
          debug_log("after single step. i=%d\n",i);
          debug_log("devent.debugevent=%d (thread %d)\n", devent.debugevent, devent.threadid);

        //  cenet_setBreakpoint(fd, pHandle, devent.threadid,0xce0000, 3,4,0);


         // cenet_setBreakpoint(fd, pHandle, -1, 0xa000, 3, 4);

         // debug_log("cenet_removeBreakpoint returned\n");
          cenet_continueFromDebugEvent(fd, pHandle, devent.threadid, 1); //continue unhandled/single step
        }
        else
          cenet_continueFromDebugEvent(fd, pHandle, devent.threadid, 0);
      }
    }

  }
  else
  {
    debug_log("Failed to start the debugger\n");
  }

  return NULL;

}

void *CESERVERTEST_RPMTHREAD(void *arg)
{
  int fd=cenet_connect();
  int i;
int hp;

  while (1)
  {
    //cenet_readProcessMemory(fd, 0x601048, &hp, 4)
#ifdef __arm__
    i=cenet_readProcessMemory(fd, pHandle, 0xa000, &hp, 4);
#else
    i=cenet_readProcessMemory(fd, pHandle, 0x601068, &hp, 4);
#endif
    debug_log("CESERVERTEST_RPMTHREAD:");

    debug_log("i=%d\n", i);
    debug_log("hp=%d\n", hp);
    sleep(1);
  }
}

void *CESERVERTEST(int pid )
{
  int fd;

  pthread_t pth;
  debug_log("CESERVERTEST: running\n");

  //sleep(2);
  debug_log("connecting...\n");

  fd=cenet_connect();
  debug_log("fd=%d\n", fd);
  debug_log("pid=%d\n", pid);

  pHandle=cenet_OpenProcess(fd, pid);

  debug_log("pHandle=%d\n", pHandle);

  //cenet_VirtualQueryExFull(fd, pHandle,  VQE_DIRTYONLY | VQE_PAGEDONLY);

 // CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, pid);



  //launch the debuggerthread
  //pthread_create(&pth, NULL, CESERVERTEST_DEBUGGERTHREAD, NULL);
  CESERVERTEST_DEBUGGERTHREAD(NULL);

  //launch the rpmthread
 // sleep(1);
  //pthread_create(&pth, NULL, CESERVERTEST_RPMTHREAD, NULL);

 // while (1);


  fflush(stdout);

  return NULL;

}
