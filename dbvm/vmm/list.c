/*
 * list.c
 *
 *  Created on: Jun 25, 2019
 *      Author: eric
 *
 *  Implements a sorted address<>pointerlist 'class'

 */

#include "list.h"
#include "displaydebug.h"


int addresslist_findclosestIndex(PAddressList l, QWORD address) //internal function for add
{
  int begin = -1, end = -1;

  if (l->size)
  {
    begin = 0;
    end = l->size - 1;

    while (begin < end)
    {
      int d = (end - begin) / 2;
      int i = begin + d;
      QWORD v = l->list[i].address;

      if (v == address)
        return i;

      if (address < v)
        end = i;
      else
        begin = i+1;
    }
  }

  return begin;
}

void addresslist_add(PAddressList l, QWORD address, void *data)
{
  int s = addresslist_findclosestIndex(l, address);

  if (l->size >= l->capacity) //first make sure there is enough space to add
  {
    l->list = (PAddressListEntry)realloc(l->list, l->size + 32);
    l->capacity = l->size + 32;
  }

  if (s < 0)
  {
    l->list[0].data = data;
    l->list[0].address = address;
  }
  else
  {
    int i;
    if (l->list[s].address == address)
    {
      //error, already in the list
      nosendchar[getAPICID()]=0;
      sendstringf("addresslist_add: already in the list");
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
      while (1) outportb(0x80,0xc4);

    }

    while ((l->list[s].address < address) && (s < l->size))
      s++;

    for (i = l->size; i > s; i--) //move everything to the right
      l->list[i] = l->list[i - 1];

    l->list[s].data = data;
    l->list[s].address = address;
  }

  l->size++;


}

void *addresslist_find(PAddressList l, QWORD address)
/*
 * Searches the list for the address and returns the data field.  NULL if not found
 */
{

  int begin, end;

  if (l->size)
  {
    begin = 0;
    end = l->size;

    while (begin < end)
    {
      int d = end - begin;
      int i = begin + (d / 2);
      QWORD v = l->list[i].address;

      if (v == address)
        return l->list[i].data;

      if (address < v)
        end = i;
      else
        begin = i+1;
    }
  }

  return NULL;

}

void addresslist_remove(PAddressList l, QWORD address)
{
  int begin, end;

  if (l->size)
  {
    begin = 0;
    end = l->size;

    while (begin < end)
    {
      int d = end - begin;
      int i = begin + (d / 2);
      QWORD v = l->list[i].address;

      if (v == address)
      {
        int j;
        for (j = i; j < l->size-1; j++)
          l->list[j] = l->list[j + 1];

        l->size--;

        return;
      }

      if (address < v)
        end = i;
      else
        begin = i + 1;
    }
  }
}



PAddressList addresslist_create()
{
  PAddressList self = (PAddressList)malloc(sizeof(AddressList));
  self->list = (PAddressListEntry)malloc(sizeof(AddressListEntry) * 32);
  self->capacity = 32;
  self->size = 0;
  return self;
}

void addresslist_destroy(PAddressList l)
{
  free(l->list);
  free(l);
}
