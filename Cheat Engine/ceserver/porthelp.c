/*
 * porthelp.c
 *
 *  Created on: Jul 21, 2011
 *      Author: erich
 * Description: Implements some tools that might come in handy when writing a port
 */

#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include "porthelp.h"

typedef struct
{
  handleType type;
  void *pointer;

} HandleListEntry, *PHandleListEntry;

volatile HandleListEntry *HandleList;
int HandleList_max;

int CreateHandleFromPointer(void *p, handleType type)
{
  if (HandleList==NULL)
  {
    //Initialize the handlelist
    HandleList_max=4096 / sizeof(HandleListEntry); //allocate around the size of 4KB
    HandleList=(PHandleListEntry)malloc(HandleList_max*4096);

    memset((void *)HandleList, 0, sizeof(HandleListEntry)*256);
  }

  //find a empty handle spot, if none are found, relocate (shouldn't happen since ce doesn't open that many handles (of the type provided here), and tends to close them)
  int i;
  for (i=1; i<HandleList_max; i++) //start from 1, just sacrifice 0
  {
    if (HandleList[i].type==htEmpty)
    {
      HandleList[i].pointer=p;
      HandleList[i].type=type;
      return i;
    }
  }

  //still here so not a single spot was free (wtf?)
  i=HandleList_max;

  HandleList_max=HandleList_max * 2;

  HandleList=(PHandleListEntry)realloc((void *)HandleList, HandleList_max * sizeof(HandleListEntry));
  memset((void *)&HandleList[i], 0, i); //zero the new block (i contains the old size which is the appended new size)

  HandleList[i].pointer=p;
  HandleList[i].type=type;
  return i;
}


void *GetPointerFromHandle(int handle)
{
  if ((handle<HandleList_max) && (HandleList[handle].type != htEmpty))
    return HandleList[handle].pointer;
  else
    return NULL;
}

handleType GetHandleType(int handle)
{
  if (handle<HandleList_max)
    return HandleList[handle].type;
  else
    return htEmpty;
}

void RemoveHandle(int handle)
{
  if ((handle>0) && (handle<HandleList_max) && (HandleList[handle].type != htEmpty))
    HandleList[handle].type=htEmpty;
}

int SearchHandleList(int type, HANDLESEARCHCALLBACK cb, void *searchdata)
/*
 * go through the handle list and call cb(data, searchdata) for each handle of the specified type
 * if cb(data,searchdata) returns true then return that handle, else return 0
 */
{
  int i;

  for (i=1; i<HandleList_max; i++)
  {
    if (HandleList[i].type==type)
    {
      if (cb(HandleList[i].pointer, searchdata))
        return i;
    }
  }

  return 0;
}
