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

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>


int done=0;

#ifndef SUN_LEN //missing in android (copy from linux sys/un.h)
# include <string.h>    /* For prototype of `strlen'.  */

/* Evaluate to actual length of the `sockaddr_un' structure.  */
# define SUN_LEN(ptr) ((size_t) (((struct sockaddr_un *) 0)->sun_path)        \
          + strlen ((ptr)->sun_path))
#endif


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
      printf("recv returned 0\n");
      return i;
    }

    if (i==-1)
    {
      printf("recv returned -1\n");
      if (errno==EINTR)
      {
        printf("errno = EINTR\n");
        i=0;
      }
      else
      {
        printf("Error during recvall: %d. errno=%d\n",(int)i, errno);
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
        printf("Error during sendall: %d. errno=%d\n",(int)i, errno);
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
  printf("Handling command %d\n", command);

  return 1;

}

void *newconnection(void *arg)
{
  int sockethandle=(uintptr_t)arg;
  unsigned char command;
  int r;
  printf("Hello!\n");

  //wait for a command and dispatch it

  r=recvall(sockethandle, &command, 1, MSG_WAITALL);
  if (r==1)
    DispatchCommand(sockethandle, command);
  else
  {
    printf("Peer has disconnected");
    if (r==-1)
      printf(" due to an error");

    printf("\n");

    fflush(stdout);
    close(sockethandle);
  }

  return NULL;
}

void *ServerThread(void *arg)
{
  int s=(uintptr_t)arg;

  while (!done)
  {
    struct sockaddr_un addr_client;
    int clisize;
    int a;
    a=accept(s, (struct sockaddr *)&addr_client, &clisize);

    printf("accept returned %d\n", a);
    if (a==-1)
    {
      printf("accept failed: %d\n", a);
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
  printf("X_X\n");
  done=1;

}

__attribute__((constructor)) void moduleinit(void)
{
  char name[256];
  int s;

  int i;
  printf("\nServerthread active\n");

  s=socket(AF_UNIX, SOCK_STREAM, 0);

  sprintf(name, " ceserver_extension%d", getpid());

  printf("s=%d\n", s);


  struct sockaddr_un address;
  address.sun_family=AF_UNIX;
  strcpy(address.sun_path,name);

  int al=SUN_LEN(&address);

  address.sun_path[0]=0;


  int optval = 1;
  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &optval, optval);

  i=bind(s, (struct sockaddr *)&address, al);
  printf("bind returned %d\n", i);
  if (i==0)
  {

    int l;
    l=listen(s, 32);
    printf("listen=%d\n",l);

    if (l==0)
    {
      //listen successful, launch the serverthread
      pthread_t pth;
      pthread_create(&pth, NULL, (void *)ServerThread, (void *)(uintptr_t)s);
    }
    else printf("listen failed: %d\n", errno);
  }
  else
    printf("bind failed: %d\n", errno);
}

