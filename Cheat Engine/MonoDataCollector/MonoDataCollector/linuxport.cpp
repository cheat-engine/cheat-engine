/*
 * linuxport.cpp
 *
 *  Created on: Nov 5, 2022
 *      Author: eric heijnen
 *
 *
 */

#include <syslog.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/types.h>
#include <unistd.h>


#include <chrono>
#include <thread>

#include <stdarg.h>

#include <pthread.h>
#include <signal.h>

#include <string.h>






#ifdef __ANDROID__
#include <android/log.h>

# define SUN_LEN(ptr) ((size_t) (((struct sockaddr_un *) 0)->sun_path)        \
          + strlen ((ptr)->sun_path))


#elif __linux__
#include <syslog.h>
#endif

#include "linuxport.h"


#include "MonoDataCollector.h"


char lastpath[255];

#ifdef __ANDROID__
  #define LOG_TAG "MonoDataCollector"
  #define LOGD(fmt, args...) __android_log_vprint(ANDROID_LOG_DEBUG, LOG_TAG, fmt, ##args)
#endif




void InitializeCriticalSection(CRITICAL_SECTION *cs)
{
    pthread_mutexattr_t attribs;


    pthread_mutexattr_settype(&attribs, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(cs, &attribs);
}

void EnterCriticalSection(CRITICAL_SECTION *cs)
{
    pthread_mutex_lock(cs);

}

void LeaveCriticalSection(CRITICAL_SECTION *cs)
{
    pthread_mutex_unlock(cs);
}


BOOL TerminateThread(HANDLE hThread, DWORD dwExitCode)
{
    return pthread_kill((pthread_t)hThread, 0)==0;
}

int GetCurrentProcessId(void)
{
    return getpid();
}

int Sleep(int ms)
{

    std::this_thread::sleep_for(std::chrono::milliseconds(ms));

    return 0;

}

int OutputDebugString(const char * format , ...)
{
  va_list list;
  va_start(list,format);
  int ret = vprintf(format,list);
  va_end(list);


  #ifdef __ANDROID__
  va_start(list,format);
  LOGD(format,list);
  va_end(list);
  #endif

  return ret;
}

void ClosePipe(HANDLE h)
{
    close((int)h);
}


#define debug_log OutputDebugString

ssize_t recvall (int s, void *buf, size_t size, int flags)
{
  ssize_t totalreceived=0;
  ssize_t sizeleft=size;
  unsigned char *buffer=(unsigned char*)buf;

  //printf("enter recvall\n");
  #ifdef _WINDOWS
    flags=flags | MSG_WAITALL;
#endif



  while (sizeleft>0)
  {
    ssize_t i=recv(s, &buffer[totalreceived], sizeleft, flags);

    if (i==0)
    {
      debug_log((char *)"recv returned 0\n");
      return i;
    }

    if (i==-1)
    {
      debug_log((char *)"recv returned -1\n");
      if (errno==EINTR)
      {
        debug_log((char *)"errno = EINTR\n");
        i=0;
      }
      else
      {
        debug_log((char *)"Error during recvall ( %s ) ", strerror(errno));
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
        debug_log((char *)"Error during sendall ( %s )\n", strerror(errno));
        return i;
      }
    }

    totalsent+=i;
    sizeleft-=i;
  }

  return totalsent;
}


BOOL ReadFilePipeWrapper(HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, LPDWORD lpNumberOfBytesRead, LPOVERLAPPED lpOverlapped)
{
    int r=(int)recvall((int)hFile, (void *)lpBuffer, (size_t)nNumberOfBytesToRead,0);
    if (r>0)
    {
        *lpNumberOfBytesRead=r;
        return TRUE;
    }
    else
        return FALSE;
}

BOOL WriteFilePipeWrapper(HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToWrite, LPDWORD lpNumberOfBytesWriten, LPOVERLAPPED lpOverlapped)
{
    int r=(int)sendall((int)hFile, (void *)lpBuffer, (size_t)nNumberOfBytesToWrite,0);
    if (r>0)
    {
        *lpNumberOfBytesWriten=r;
        return TRUE;
    }
    else
        return FALSE;
}



HANDLE CreateNamedPipe(char *name) //createNamedPipe and wait
{
    //single connect system
    int s;
    OutputDebugString("linuxport: CreateNamedPipeAndWait(\"%s\")\n",name);
    s=socket(AF_UNIX, SOCK_STREAM,0);
    if (s)
    {
        char path[100];

        sprintf(path," %s", name);



        struct sockaddr_un address;
        address.sun_family=AF_UNIX;
        strcpy(address.sun_path, path);

        int al=(int)SUN_LEN(&address);

        address.sun_path[0]=0;

        int optval=1;
        setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &optval, optval);

        int i;
        i=bind(s,(struct sockaddr *)&address, al);
        if (i==0)
        {
            OutputDebugString((char*)"MDC Starting to listen\n");
            i=listen(s,32);

            OutputDebugString("Listen returned %d\n",i);
            if (i==0)
            {
                OutputDebugString((char*)"Connection attempt\n");
                struct sockaddr_un addr_client;
                socklen_t clisize;
                int a=-1;


                while (a==-1)
                {
                  OutputDebugString((char*)"Calling accept\n");
                  a=accept(s, (struct sockaddr *)&addr_client, &clisize);

                  if (a!=-1)
                  {
                      OutputDebugString((char*)"Connection accepted\n");
                      OutputDebugString((char*)"Closing the listener\n");
                      close(s); //stop listening
                      return a;
                  }
                  else
                  {
                    OutputDebugString((char*)"accept returned -1  ( %s )\n",a, strerror(errno));

                  }
                }

            }
            else
                OutputDebugString((char*)"Listen failed");

        }
        else
          OutputDebugString((char*)"Bind failed\n");
    }
    else
        OutputDebugString((char*)"Failure creating socket\n");

    return INVALID_HANDLE_VALUE;
}

__attribute__((constructor)) void moduleinit(void)
{

  pthread_t lpep;

#ifdef __linux__
  openlog((char*)"Cheat Engine MDC", 0, LOG_USER);
#endif

  OutputDebugString("monodatacollector moduleinit");

  pthread_create(&lpep, NULL, (void* (*)(void*)) LinuxPortEntryPoint,  NULL);
}

