/*
 * server.c
 *
 *  Created on: Aug 25, 2013
 *      Author: eric
 *
 * The communication layer between the process and ceserver
 * This module will be loaded into the game by either a PRELOAD like LD_PRELOAD or a config, or injected using ceserver's module injector
 *
 * When launched the constructor will spawn a thread and open a named shared object/pipe (does linux support that) for communication
 */

#include <errno.h>
#include <stdio.h>
#include <pthread.h>
#include <stdint.h>
#include <unistd.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <stdarg.h>
#ifdef ANDROID
#include <android/log.h>
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/mman.h>



#include "server.h"
#include "speedhack.h"

int done=0;

#ifndef SUN_LEN //missing in android (copy from linux sys/un.h)
# include <string.h>    /* For prototype of `strlen'.  */

/* Evaluate to actual length of the `sockaddr_un' structure.  */
# define SUN_LEN(ptr) ((size_t) (((struct sockaddr_un *) 0)->sun_path)        \
          + strlen ((ptr)->sun_path))
#endif

void dvmJitStats();


typedef struct
{
  uint64_t address;
  uint32_t size;
} AllocEntry, *PAllocEntry;

PAllocEntry allocList;
int allocList_Max;
int allocList_Pos;



#ifdef __ANDROID__
  #define LOG_TAG "CESERVER_EXTENSION"
  #define LOGD(fmt, args...) __android_log_vprint(ANDROID_LOG_DEBUG, LOG_TAG, fmt, ##args)
#endif

int debug_log(const char * format , ...)
{
  va_list list;
  va_start(list,format);
  int ret = vprintf(format,list);

  #ifdef __ANDROID__
    LOGD(format,list);
  #endif
  va_end(list);
  return ret;
}

void allocListAdd(uint64_t address, uint32_t size)
{
  if (allocList==NULL)
  {
    allocList=calloc(16, sizeof(AllocEntry));
    allocList_Max=16;
  }

  allocList[allocList_Pos].address=address;
  allocList[allocList_Pos].size=size;

  allocList_Pos++;
  if (allocList_Pos==allocList_Max)
  {
    allocList_Max*=2;
    allocList=realloc(allocList, allocList_Max*sizeof(AllocEntry));
  }
}

void allocListRemove(uint64_t address)
{
  int i;
  for (i=0; i<allocList_Pos; i++)
  {
    if (allocList[i].address==address)
    {
      int j;
      for (j=i; j<allocList_Pos-1; j++)
        allocList[j]=allocList[j+1];

      allocList_Pos--;
      break;
    }
  }

}

int allocListFind(uint64_t address)
/*
 * Return the size. (0 if not found)
 */
{
  int i;
  for (i=0; i<allocList_Pos; i++)
  {
    if (allocList[i].address==address)
    {
      return allocList[i].size;
    }
  }

  return 0;

}



ssize_t recvall (int s, void *buf, size_t size, int flags)
{
  ssize_t totalreceived=0;
  ssize_t sizeleft=size;
  unsigned char *buffer=(unsigned char*)buf;

  //printf("enter recvall\n");

  flags=flags | MSG_WAITALL;

  while (sizeleft>0)
  {
    ssize_t i=recv(s, &buffer[totalreceived], sizeleft, flags);

    if (i==0)
    {
      debug_log("recv returned 0\n");
      return i;
    }

    if (i==-1)
    {
      debug_log("recv returned -1\n");
      if (errno==EINTR)
      {
        debug_log("errno = EINTR\n");
        i=0;
      }
      else
      {
        debug_log("Error during recvall: %d. errno=%d\n",(int)i, errno);
        return i; //read error, or disconnected
      }

    }

    totalreceived+=i;
    sizeleft-=i;
  }

  //printf("leave recvall\n");
  return totalreceived;
}

ssize_t sendall (int s, void *buf, size_t size, int flags)
{
  ssize_t totalsent=0;
  ssize_t sizeleft=size;
  unsigned char *buffer=(unsigned char*)buf;

  while (sizeleft>0)
  {
    ssize_t i=send(s, &buffer[totalsent], sizeleft, flags);

    if (i==0)
    {
      return i;
    }

    if (i==-1)
    {
      if (errno==EINTR)
        i=0;
      else
      {
        debug_log("Error during sendall: %d. errno=%d\n",(int)i, errno);
        return i;
      }
    }

    totalsent+=i;
    sizeleft-=i;
  }

  return totalsent;
}

int DispatchCommand(int currentsocket, unsigned char command)
{
  //printf("Handling command %d\n", command);

  switch (command)
  {
    case EXTCMD_ALLOC:
    {
#pragma pack(1)
      struct {
        uint64_t preferedAddress;
        uint32_t size;
      } params;
#pragma pack()
      //printf("EXTCMD_ALLOC. Receiving params:\n");

      if (recvall(currentsocket, &params, sizeof(params), 0)>0)
      {

       // debug_log("params.preferedAddress=%lx\n", params.preferedAddress);
        //printf("params.size=%d\n", params.size);

        uint64_t address=(uint64_t)mmap((void *)params.preferedAddress, params.size, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

        //printf("Actually allocated at %lx\n", address);
        if (address)
          allocListAdd(address, params.size);

        sendall(currentsocket, &address, sizeof(address), 0);


      }


      break;
    }

    case EXTCMD_FREE:
    {
#pragma pack(1)
      struct {
        uint64_t address;
        uint32_t size;
      } params;
#pragma pack()

      uint32_t result;

      if (recvall(currentsocket, &params, sizeof(params), 0)>0)
      {
        //printf("EXTCMD_FREE\n");
        //printf("params.address=%lx\n", params.address);
        //printf("1: params.size=%d\n", params.size);

        if (params.size==0)
          params.size=allocListFind(params.address);



        //printf("2: params.size=%d\n", params.size);

        if (params.size)
        {
          //printf("Calling munmap\n");
          result=munmap((void *)params.address, params.size);
          if (result==-1)
            result=0;
          else
            result=1;
        }
        else
          result=0;

        //printf("Removing alloc from list\n");
        allocListRemove(params.address);

        //printf("Returning result\n");
        sendall(currentsocket, &result, sizeof(result), 0);


      }
      break;
    }

    case EXTCMD_CREATETHREAD:
    {
#pragma pack(1)
      struct {
        uint64_t startaddress;
        uint64_t parameter;
      } params;
#pragma pack()

      if (recvall(currentsocket, &params, sizeof(params), 0)>0)
      {
        //printf("EXTCMD_CREATETHREAD\n");
        //printf("params.startaddress=%lx\n", params.startaddress);
        //printf("params.parameter=%ld\n", params.parameter);

        uint64_t threadhandle=0;

        pthread_create((pthread_t *)&threadhandle, NULL, (void *)params.startaddress, (void *)params.parameter);

        sendall(currentsocket, &threadhandle, sizeof(threadhandle), 0);
      }


      break;
    }

    case EXTCMD_LOADMODULE:
    {
      uint32_t modulepathlength;
      if (recvall(currentsocket, &modulepathlength, sizeof(modulepathlength), 0)>0)
      {
        char *modulepath[modulepathlength+1];
        if (recvall(currentsocket, modulepath, modulepathlength, 0)>0)
        {
          modulepath[modulepathlength]=0;
          uint64_t result;
          result=(dlopen((const char *)modulepath, RTLD_NOW)!=NULL);

          sendall(currentsocket, &result, sizeof(result), 0);

        }


      }

      break;
    }

    case EXTCMD_SPEEDHACK_SETSPEED:
    {
#pragma pack(1)
      struct {
        float speed;
      } params;
#pragma pack()

      uint32_t result;

      if (recvall(currentsocket, &params, sizeof(params), 0)>0)
      {
        //printf("EXTCMD_SPEEDHACK_SETSPEED(%d)\n", params.speed);

        result=speedhack_initializeSpeed(params.speed);
        sendall(currentsocket, &result, sizeof(result), 0);
      }
      break;
    }

  }
  return 1;

}

void *newconnection(void *arg)
{
  int sockethandle=(uintptr_t)arg;
  unsigned char command;
  int r;
  //printf("Hello!\n");

  //wait for a command and dispatch it

  r=1;
  while (r>=0)
  {
    r=recvall(sockethandle, &command, 1, MSG_WAITALL);
    if (r==1)
      DispatchCommand(sockethandle, command);
    else
    {
      //printf("Peer has disconnected");
      //if (r==-1)
      //  debug_log(" due to an error");

      //printf("\n");

      //fflush(stdout);
      close(sockethandle);
    }
  }

  debug_log("Bye\n");
  return NULL;
}

void *ServerThread(void *arg)
{
  int s=(uintptr_t)arg;

  while (!done)
  {
    struct sockaddr_un addr_client;
    socklen_t clisize;
    int a;
    a=accept(s, (struct sockaddr *)&addr_client, &clisize);

    //printf("accept returned %d\n", a);
    if (a==-1)
    {
      debug_log("accept failed: %d\n", a);
      break;
    }
    else
    {
      pthread_t pth;
      pthread_create(&pth, NULL, (void *)newconnection, (void *)(uintptr_t)a);
    }


  }

  return NULL;
}

__attribute__((destructor)) void term(void)
{
  //printf("X_X\n");
  done=1;

}

__attribute__((constructor)) void moduleinit(void)
{
  char name[256];
  int s;

  int i;
  //printf("\nServerthread active\n");

  speedhack_initializeSpeed(1.0f);

  void *X;
  void *module=NULL;

/*
  __android_log_print(ANDROID_LOG_DEBUG, "CHEATENGING", "Hello \n");

  __android_log_print(ANDROID_LOG_DEBUG, "CHEATENGING", "module1=%p\n", module);

  module=dlopen("libdvm.so", module);
  __android_log_print(ANDROID_LOG_DEBUG, "CHEATENGING", "module=%p\n", module);


  X=dlsym(module, "dvmJitStats");
  __android_log_print(ANDROID_LOG_DEBUG, "CHEATENGING", "After dlsym\n");

  __android_log_print(ANDROID_LOG_DEBUG, "CHEATENGING", "X=%p\n", X);

 // dvmJitStats();

  //if (dvmJitStats)
 // {
 //   debug_log("Calling dvmJitStats\n");
    //dvmJitStats();
 // }
*/
  s=socket(AF_UNIX, SOCK_STREAM, 0);

  sprintf(name, " ceserver_extension%d", getpid());

  //printf("s=%d\n", s);


  struct sockaddr_un address;
  address.sun_family=AF_UNIX;
  strcpy(address.sun_path,name);

  int al=SUN_LEN(&address);

  address.sun_path[0]=0;


  int optval = 1;
  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &optval, optval);

  i=bind(s, (struct sockaddr *)&address, al);
  //printf("bind returned %d\n", i);
  if (i==0)
  {

    int l;
    l=listen(s, 32);
    debug_log("listen=%d\n",l);

    if (l==0)
    {
      //listen successful, launch the serverthread
      pthread_t pth;
      pthread_create(&pth, NULL, (void *)ServerThread, (void *)(uintptr_t)s);
    }
    else debug_log("listen failed: %d\n", errno);
  }
  else
    debug_log("bind failed: %d\n", errno);
}

