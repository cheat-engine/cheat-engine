#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <string.h>
#include <netinet/in.h>
#include <unistd.h>
#include <sys/select.h>

#include <errno.h>


#include "ceserver.h"
#include "porthelp.h"
#include "api.h"

pthread_t pth;
pthread_t identifierthread;
volatile int done;
int PORT;

char versionstring[]="CHEATENGINE Network 1.0";

ssize_t sendall (int s, void *buf, size_t size, int flags)
{
  ssize_t totalsent=0;
  ssize_t sizeleft=size;
  unsigned char *buffer=(unsigned char*)buf;

  while (sizeleft>0)
  {
    ssize_t i=send(s, &buffer[totalsent], sizeleft, flags);

    if ((i==-1) || (i==0))
    {
      printf("Error during sendall: %d. errno=%d\n",i, errno);
        return i; //read error, or disconnected
    }

    totalsent+=i;
    sizeleft-=i;
  }

  return totalsent;
}


void *newconnection(void *arg)
{
  int s=(int)arg;
  unsigned char command;

  //printf("new connection. Using socket %d\n", s);


  while (done==0)
  {


    int r=recv(s, &command, 1, MSG_WAITALL);

    //printf("s=%d  r=%d  command=%d\n", s, r, command);
    //fflush(stdout);

    if (r>0)
    {
      switch (command)
      {
        case CMD_GETVERSION:
        {
          PCeVersion v;
          int versionsize=strlen(versionstring);
          v=(PCeVersion)malloc(sizeof(CeVersion)+versionsize);
          v->stringsize=versionsize;
          v->version=1;

          memcpy((char *)v+sizeof(CeVersion), versionstring, versionsize);

          //version request
          sendall(s, v, sizeof(CeVersion)+versionsize, 0);

          free(v);

          break;
        }

        case CMD_CLOSECONNECTION:
        {
          printf("Connection %d closed properly\n", s);
          fflush(stdout);
          close(s);

          return NULL;
        }

        case CMD_TERMINATESERVER:
        {
          printf("Command to terminate the server received\n");
          fflush(stdout);
          close(s);
          exit(0);
        }

        case CMD_CLOSEHANDLE:
        {
          HANDLE h;

          if (recv(s, &h, sizeof(h), MSG_WAITALL)>0)
          {
            CloseHandle(h);
          }
          else
          {
            printf("Error during read for CMD_CLOSEHANDLE\n");
            close(s);
            fflush(stdout);
            return 0;
          }
          break;
        }

        case CMD_CREATETOOLHELP32SNAPSHOT:
        {
          CeCreateToolhelp32Snapshot params;
          HANDLE result;

          if (recv(s, &params, sizeof(CeCreateToolhelp32Snapshot), MSG_WAITALL) > 0)
          {
            result=CreateToolhelp32Snapshot(params.dwFlags, params.th32ProcessID);
            //printf("result of CreateToolhelp32Snapshot=%d\n", result);
            sendall(s, &result, sizeof(HANDLE), 0);
          }
          else
          {
            printf("Error during read for CMD_CREATETOOLHELP32SNAPSHOT\n");
            fflush(stdout);
            close(s);
            return 0;
          }

          break;
        }

        case CMD_PROCESS32FIRST:
        case CMD_PROCESS32NEXT:
        {
          HANDLE toolhelpsnapshot;
          if (recv(s, &toolhelpsnapshot, sizeof(toolhelpsnapshot), MSG_WAITALL) >0)
          {
            ProcessListEntry pe;
            BOOL result;
            CeProcessEntry *r;
            int size;

            if (command==CMD_PROCESS32FIRST)
              result=Process32First(toolhelpsnapshot, &pe);
            else
              result=Process32Next(toolhelpsnapshot, &pe);

          //  printf("result=%d\n", result);


            if (result)
            {
              size=sizeof(CeProcessEntry)+ strlen(pe.ProcessName);
              r=(PCeProcessEntry)malloc(size);
              r->processnamesize=strlen(pe.ProcessName);
              r->pid=pe.PID;
              memcpy((char *)r+sizeof(CeProcessEntry), pe.ProcessName, r->processnamesize);
            }
            else
            {
              size=sizeof(CeProcessEntry);
              r=(PCeProcessEntry)malloc(size);
              r->processnamesize=0;
              r->pid=0;
            }

            r->result=result;

            sendall(s, r, size, 0);

            free(r);

          }
          break;
        }

        case CMD_READPROCESSMEMORY:
        {
          CeReadProcessMemoryInput c;

          r=recv(s, &c, sizeof(c), MSG_WAITALL);
          if (r>0)
          {
            PCeReadProcessMemoryOutput o=NULL;

            //if (c.size>200000)
            //printf("ReadProcessMemory. Address: %llx - Size=%d\n", c.address, c.size);

            o=(PCeReadProcessMemoryOutput)malloc(sizeof(CeReadProcessMemoryOutput)+c.size);

            if (o==NULL)
            {
              printf("Failure allocating memory\n");
              while (1);
            }


            o->read=ReadProcessMemory(c.handle, (void *)(uintptr_t)c.address, &o[1], c.size);

         //   printf("ReadProcessMemory returned %d\n", o->read);


            if (o->read > 500000)
            {
            	//printf("going to send %u bytes\n", sizeof(CeReadProcessMemoryOutput)+o->read);
            }

            int i=sendall(s, o, sizeof(CeReadProcessMemoryOutput)+o->read, 0);

            //if (o->read > 500000)
            //	printf("sent %d bytes. Wanted to send %u\n", i, sizeof(c.size)+o->read);


            if ((signed int)i!=sizeof(CeReadProcessMemoryOutput)+o->read)
            {
            	printf("READ INTERUPTION: %d out of %d\n",i,sizeof(CeReadProcessMemoryOutput)+o->read);

            	if (i==-1)
            	{
            		int e=errno;
            		char *error=strerror(e);
            		printf("Error: %d: %s\n", e, error);
            		fflush(stdout);

            		while (1) sleep(10);


            	}
            }

          //  printf("send %d bytes. Wanted to send %lu\n", i, sizeof(c.size)+o->read);

            if (o)
              free(o);

           // printf("+");
           // fflush(stdout);

          }

          break;
        }

        case CMD_WRITEPROCESSMEMORY:
        {
          CeWriteProcessMemoryInput c;

          printf("CMD_WRITEPROCESSMEMORY:\n");


          r=recv(s, &c, sizeof(c), MSG_WAITALL);
          if (r>0)
          {

            CeWriteProcessMemoryOutput o;
            unsigned char *buf;

            printf("recv returned %d bytes\n", r);
            printf("c.size=%d\n", c.size);

            buf=(unsigned char *)malloc(c.size);

            r=recv(s, buf, c.size, MSG_WAITALL);
            if (r>0)
            {
              printf("received %d bytes for the buffer. Wanted %d\n", r, c.size);
              o.written=WriteProcessMemory(c.handle, (void *)(uintptr_t)c.address, buf, c.size);

              r=sendall(s, &o, sizeof(CeWriteProcessMemoryOutput), 0);
              printf("wpm: returned %d bytes to caller\n", r);

            }
            else
            	printf("wpm recv error while reading the data\n");

            free(buf);

          }
          else
          {
        	  printf("RPM: recv failed\n");
          }

          break;

        }

        case CMD_VIRTUALQUERYEX:
        {
          CeVirtualQueryExInput c;


          r=recv(s, &c, sizeof(c), MSG_WAITALL);
          if (r>0)
          {
            RegionInfo rinfo;
            CeVirtualQueryExOutput o;



            o.result=VirtualQueryEx(c.handle, (void *)(uintptr_t)c.baseaddress, &rinfo);
            o.protection=rinfo.protection;
            o.baseaddress=rinfo.baseaddress;
            o.size=rinfo.size;

            sendall(s, &o, sizeof(o), 0);


          }

          break;
        }



        case CMD_OPENPROCESS:
        {
          int pid;

         // printf("OpenProcess:");


          r=recv(s, &pid, sizeof(int), MSG_WAITALL);
          if (r>0)
          {
            int processhandle=OpenProcess(pid);
            sendall(s, &processhandle, sizeof(int), 0);
          }
          else
          {
                  printf("Error\n");
                  fflush(stdout);
                  close(s);
                  return NULL;
          }

          break;
        }

        /*readprocessmemory:
         * Note: Preferably used with a modified kernel that allows direct access to /proc/pid/mem
         * Alternatively, preferred to use a kernel with CONFIG_STRICT_DEVMEM OFF,
         * If neither situations are available, PTRACE will be used to read, and that is sloooooooooooow
         *
         */


      }
    }
    else
    if (r==-1)
    {
      printf("read error on socket %d (%d)\n", s, errno);
      fflush(stdout);
      close(s);
      return NULL;
    }
    else
    if (r==0)
    {
      printf("Peer has disconnected\n");
      fflush(stdout);
      close(s);
      return NULL;
    }
  }

  close(s);

  return NULL;
}

void *IdentifierThread(void *arg)
{
  int i;
  int s;
  int v=1;
#pragma pack(1)
  struct
  {
    uint32_t checksum;
    uint16_t port;
  } packet;
#pragma pack()

  socklen_t clisize;
  struct sockaddr_in addr, addr_client;

  printf("IdentifierThread active\n");
  s=socket(PF_INET, SOCK_DGRAM, 0);
  i=setsockopt(s, SOL_SOCKET, SO_BROADCAST, &v, sizeof(v));

  memset(&addr, 0, sizeof(addr));

  addr.sin_family=PF_INET;
  addr.sin_addr.s_addr=INADDR_ANY;
  addr.sin_port=htons(3296);
  i=bind(s, (struct sockaddr *)&addr, sizeof(addr));

  if (i>=0)
  {
    while (1)
    {
      memset(&addr_client, 0, sizeof(addr_client));
      addr_client.sin_family=PF_INET;
      addr_client.sin_addr.s_addr=INADDR_ANY;
      addr_client.sin_port=htons(3296);

      clisize=sizeof(addr_client);

      i=recvfrom(s, &packet, sizeof(packet), 0, (struct sockaddr *)&addr_client, &clisize);

      //i=recv(s, &v, sizeof(v), 0);
      if (i>=0)
      {

        printf("Identifier thread received a message :%d\n",v);
        printf("sizeof(packet)=%d\n", sizeof(packet));

        printf("packet.checksum=%x\n", packet.checksum);
        packet.checksum*=0xce;
        packet.port=PORT;
        printf("packet.checksum=%x\n", packet.checksum);

//        packet.checksum=00AE98E7 - y=8C7F09E2


        i=sendto(s, &packet, sizeof(packet), 0, (struct sockaddr *)&addr_client, clisize);
        printf("sendto returned %d\n",i);
      }
      else
    	  printf("recvfrom failed\n");
    }


  }
  else
	  printf("bind failed\n");

  printf("IdentifierThread exit\n");

  return 0;
}

int main(int argc, char *argv[])
{
        int s;
        int b;
        int l;
        int a;


        if (argc>1)
        {
          if (strcmp(argv[1], "TEST")==0)
          {
            printf("Test mode\n");
            if (argc>2)
            {
              int pid;
              HANDLE h;
              pid=atoi(argv[2]);


              h=OpenProcess(pid);
              if (h)
              {
                printf("Opened the process\n");
                if (StartDebug(h))
                {
                  printf("Debugging the process\n");

                  while (1)
                  {
                    if (WaitForDebugEvent(h))
                    {
                      printf("Got a debug event\n");
                      if (ContinueFromDebugEvent(h, 0)==FALSE)
                      {
                        printf("Could not continue...\n");
                        //StopDebug(h);
                        //break;
                      }
                    }

                  }
                }
                else
                  printf("Debug failed\n");
              }
              else
                printf("Failed opening the process\n");


              exit(1);
            }

          }
        }

        socklen_t clisize;
        struct sockaddr_in addr, addr_client;

        PORT=52736;

        done=0;
        //printf("WEEEEE\n");

        printf("&s=%p\n", &s);
        printf("main=%p\n", main);

        printf("CEServer. Waiting for client connection\n");

        //if (broadcast)
        pthread_create(&identifierthread, NULL, IdentifierThread, NULL);



        s=socket(AF_INET, SOCK_STREAM, 0);
        printf("socket=%d\n", s);



        memset(&addr, sizeof(addr), 0);
        addr.sin_family=AF_INET;
        addr.sin_port=htons(PORT);
        addr.sin_addr.s_addr=INADDR_ANY;

        b=bind(s, (struct sockaddr *)&addr, sizeof(addr));
        printf("bind=%d\n", b);

        if (b!=-1)
        {

                l=listen(s, 32);

                printf("listen=%d\n", l);

                clisize=sizeof(addr_client);
                memset(&addr_client, sizeof(addr_client), 0);

                while (done==0)
                {
                        a=accept(s, (struct sockaddr *)&addr_client, &clisize);

                        printf("accept=%d\n", a);

                        if (a != -1)
                        {
                          pthread_create(&pth, NULL, newconnection, (void *)a);

                        }
                }


        }

        printf("Terminate server\n");



        close(s);

        return 0;
}
