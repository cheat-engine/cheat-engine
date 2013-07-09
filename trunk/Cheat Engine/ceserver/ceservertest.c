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


  memset(&addr, sizeof(addr), 0);
  addr.sin_family=AF_INET;
  addr.sin_port=htons(PORT);
  addr.sin_addr.s_addr=htonl(INADDR_LOOPBACK); //0x1600a8c0; //INADDR_LOOPBACK;

  printf("calling connect...\n");
  i=connect(fd, (struct sockaddr *)&addr, sizeof(addr));

  printf("after connect. %d\n", i);


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

  printf("cenet_OpenProcess(%d,%d)\n", fd, pid);



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

  printf(">>>>>>>>>>>>>>>>>>cenet_waitForDebugEvent returned<<<<<<<<<<<<<<<<\n");

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

  printf("cenet_readProcessMemory(%d, %d, %llx, %p, %d)", fd, pHandle, address, dest, size);

  rpm.command=CMD_READPROCESSMEMORY;
  rpm.pHandle=pHandle;
  rpm.address=address;
  rpm.size=size;

  sendall(fd, &rpm, sizeof(rpm), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);

  printf("result=%d\n", result);
  recv(fd, dest, result, MSG_WAITALL);


  return result;

}

int cenet_setBreakpoint(int fd, int pHandle, int tid, void *Address, int bptype, int bpsize)
{
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
    int result;

  printf("cenet_setBreakpoint\n");
  sb.command=CMD_SETBREAKPOINT;
  sb.hProcess=pHandle;
  sb.tid=tid;
  sb.Address=(uintptr_t)Address;
  sb.bptype=bptype;
  sb.bpsize=bpsize;

  sendall(fd, &sb, sizeof(sb), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);

  return result;
}

int cenet_removeBreakpoint(int fd, int pHandle, int tid)
{
#pragma pack(1)
    struct
    {
      char command;
      HANDLE hProcess;
      int tid;
    } rb;
#pragma pack()
    int result;

  printf("cenet_removeBreakpoint\n");
  rb.command=CMD_REMOVEBREAKPOINT;
  rb.hProcess=pHandle;
  rb.tid=tid;

  sendall(fd, &rb, sizeof(rb), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);

  return result;
}

void *CESERVERTEST_DEBUGGERTHREAD(void *arg)
{
  int count=0;
  int fd=cenet_connect();

  if (cenet_startDebugger(fd, pHandle))
  {
    int i;
    DebugEvent devent;

    printf("cenet_startDebugger=true\n");

    while (1)
    {
      count++;
      printf("count=%d\n", count);

      if (count==4)
      {
        printf("going to set breakpoint\n");
#ifdef __arm__
        cenet_setBreakpoint(fd, pHandle, -1, 0x8374, 0, 1);
#else
        cenet_setBreakpoint(fd, pHandle, -1, 0x4007ad, 0, 1);
#endif

        printf("cenet_setBreakpoint returned\n");
      }

      i=cenet_waitForDebugEvent(fd, pHandle, &devent, 2000);
      if (i)
      {

        if (devent.debugevent==5)
        {
          printf("TRAP (thread %d)\n", devent.threadid);
          printf("Going to remove breakpoint\n");
          cenet_removeBreakpoint(fd, pHandle, devent.threadid);

          printf("cenet_removeBreakpoint returned\n");
          cenet_continueFromDebugEvent(fd, pHandle, devent.threadid, 1); //continue unhandled/single step
        }
        else
          cenet_continueFromDebugEvent(fd, pHandle, devent.threadid, 0);
      }
    }

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
    printf("CESERVERTEST_RPMTHREAD:");

    printf("i=%d\n", i);
    printf("hp=%d\n", hp);
    sleep(1);
  }
}

void *CESERVERTEST(void *argv[])
{
  int fd;

  int pid=atoi(argv[2]);
  pthread_t pth;
  printf("CESERVERTEST: running\n");

  //sleep(2);
  printf("connecting...\n");

  fd=cenet_connect();
  printf("fd=%d\n", fd);
  printf("pid=%d\n", pid);

  pHandle=cenet_OpenProcess(fd, pid);

  printf("pHandle=%d\n", pHandle);

  //launch the debuggerthread
  pthread_create(&pth, NULL, CESERVERTEST_DEBUGGERTHREAD, NULL);

  //launch the rpmthread
 // sleep(1);
  //pthread_create(&pth, NULL, CESERVERTEST_RPMTHREAD, NULL);

 // while (1);


  fflush(stdout);

  return NULL;

}
