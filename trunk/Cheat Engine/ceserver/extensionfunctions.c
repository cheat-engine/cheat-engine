/*
 * extensionfunctions.c
 *
 *  Created on: Aug 25, 2013
 *      Author: eric
 */


#include <stdio.h>
#include <pthread.h>

#include "extensionloader.h"
#include "extensionfunctions.h"
#include "api.h"


uint64_t ext_alloc(HANDLE hProcess, uint64_t preferedBase, int size)
{
  uint64_t result=0;
  printf("ext_alloc(%d, %llx, %d\n", hProcess, preferedBase, size);

  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);

    struct {
      uint8_t command;
      uint64_t preferedAddress;
      uint32_t size;
    } allocCommand;

    if (p->hasLoadedExtension==FALSE)
    {
      printf("hasLoadedExtension == FALSE");
      if (loadCEServerExtension(hProcess)==FALSE)
      {
        printf("Failure to load the extension\n");
        return 0;
      }
    }


    pthread_mutex_lock(&p->extensionMutex);

    allocCommand.command=CMD_ALLOC;
    allocCommand.preferedAddress=preferedBase;
    allocCommand.size=size;

    if (sendall(p->extensionFD, &allocCommand, sizeof(allocCommand), 0)>0)
      recvall(p->extensionFD, &result, sizeof(result), 0);

    pthread_mutex_unlock(&p->extensionMutex);


  }


  return result;
}
