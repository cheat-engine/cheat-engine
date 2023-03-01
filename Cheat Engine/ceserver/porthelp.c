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
  int Handle;

} HandleListEntry, *PHandleListEntry;

volatile HandleListEntry *HandleList;
int HandleList_max;

int CreateHandleFromPointer(void *p, handleType type)
{
  if (HandleList==NULL)
  {
    //Initialize the handlelist
    HandleList_max=256;
    HandleList=(PHandleListEntry)malloc(256*sizeof(HandleListEntry));

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

  debug_log("Reached max amount of handles (%d). Leaking some memory to make a larger list (hope for the best, and fix this handle leak that is going on)\n",HandleList_max);
  HandleListEntry *NewHandleList=malloc(HandleList_max * 2 * sizeof(HandleListEntry));
  memset((void *)NewHandleList, 0, HandleList_max * 2 * sizeof(HandleListEntry));
  memcpy(NewHandleList,HandleList, sizeof(HandleList_max * sizeof(HandleListEntry)));

  HandleList=NewHandleList;
  i=HandleList_max;
  HandleList_max=HandleList_max * 2;

  HandleList[i].pointer=p;
  HandleList[i].type=type;
  return i;
}


void *GetPointerFromHandle(int handle)
{
  if ((handle>0) && (handle<HandleList_max) && (HandleList[handle].type != htEmpty))
    return HandleList[handle].pointer;
  else
    return NULL;
}

handleType GetHandleType(int handle)
{
  if ((handle>0) && (handle<HandleList_max))
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
