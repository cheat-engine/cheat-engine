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
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <strings.h>

#include <sys/mman.h>
#include <sys/ptrace.h>
#include <sys/wait.h>




#include "api.h"
#include "porthelp.h"


pthread_mutex_t memorymutex;


typedef struct
{
  int processListIterator;
  int processCount;
  PProcessListEntry processList;
} ProcessList, *PProcessList;


int WriteProcessMemory(HANDLE hProcess, void *lpAddress, void *buffer, int size)
{
  int written=0;

  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=GetPointerFromHandle(hProcess);


    if (pthread_mutex_lock(&memorymutex) == 0)
    {
      if (ptrace(PTRACE_ATTACH, p->pid,0,0)==0)
      {
        int status;
        pid_t pid=wait(&status);
        int offset=0;
        int max=size-sizeof(long int);

        long int *address=buffer;


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

      pthread_mutex_unlock(&memorymutex);
    }



  }

  return written;
}

int ReadProcessMemory(HANDLE hProcess, void *lpAddress, void *buffer, int size)
{
  //idea in case this is too slow. always read a full page and keep the last 16 accessed pages.
  //only on cache miss, or if the cache is older than 1000 milliseconds fetch the page.
  //keep in mind that this routine can get called by multiple threads at the same time


  int read=0;
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=GetPointerFromHandle(hProcess);

    //printf("hProcess=%d, lpAddress=%p, buffer=%p, size=%d\n", hProcess, lpAddress, buffer, size);

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
         // printf("pread error\n");
        }

        ptrace(PTRACE_DETACH, pid,0,0);
      }
      else
        printf("ptrace attach failed\n");

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
    PProcessData p=GetPointerFromHandle(hProcess);
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

            int r,w,x;

            if (index(protectionstring, 'x'))
              x=1;
            else
              x=0;

            if (index(protectionstring, 'r'))
              r=1;
            else
              r=0;

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
    PProcessData p=malloc(sizeof(ProcessData));

    p->pid=pid;
    p->path=strdup(processpath);

    sprintf(processpath,"/proc/%d/maps", pid);
    p->maps=strdup(processpath);

    sprintf(processpath,"/proc/%d/mem", pid);
    p->mem=open(processpath, O_RDONLY);



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
    PProcessList pl=GetPointerFromHandle(hSnapshot);

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
    PProcessList pl=GetPointerFromHandle(hSnapshot);
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
    PProcessList pl=malloc(sizeof(ProcessList));

    //printf("Creating processlist\n");

    pl->processCount=0;
    pl->processList=malloc(sizeof(ProcessListEntry)* max);

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
            pl->processList=realloc(pl->processList, max);
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

}

void CloseHandle(HANDLE h)
{
  handleType ht=GetHandleType(h);

 // printf("CloseHandle(%d)\n", h);
  if (ht==htTHSProcess)
  {
    ProcessList *pl=GetPointerFromHandle(h);
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
    PProcessData pd=GetPointerFromHandle(h);
    free(pd->maps);
    free(pd->path);
    close(pd->mem);
    free(pd);
  }

  RemoveHandle(h);
}




