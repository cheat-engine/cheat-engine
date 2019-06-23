/*
 * maps.c
 *
 *  Created on: Jun 23, 2019
 *      Author: eric
 */

//a rudimentary pointer map implementation for storing 64-bit pointers


#include "maps.h"

PMapInfo createMap(int maxlevel)
{
  PMapInfo r=NULL;
  if (maxlevel<=4) return NULL;

  r=malloc(sizeof(MapInfo));

  zeromemory(r,sizeof(MapInfo));
  r->maxlevel=maxlevel;

  r->top=malloc(4096);
  zeromemory(r->top,4096);

  return r;
}

PMapInfo createPhysicalMemoryMap()
{
  /* calculate the maxlevel needed for MAXPHYADDR
   * 32: (32-12)/9=2.2222 (3)
   * 36 :(36-12)/9=2,666666667 (3)
   * 48: (48-12)/9=4
   * 56: (56-12)/9=5
   * 64: (64-12)/9=4.333 (5)
   */


  int neededbits=MAXPHYADDR-12;
  int levelneeded=neededbits / 9;
  if (neededbits % 12)
    levelneeded++;

  return createMap(levelneeded);
}

int map_setEntry(PMapInfo map, QWORD address, void *data)
{
  PMapData m=map->top;
  int level=0;
  int entrynr;
  address=address >> 12;

  //address: XXXXXXXXXXXXX(XXX)
  //binary XXXXXXX XXXXXXXXX XXXXXXXXX XXXXXXXXX XXXXXXXXX XXXXXXXXX (XXXXXXXXXXXX)
  //binary: XXXXXXXXX XXXXXXXXX XXXXXXXXX XXXXXXXXX (XXXXXXXXXXXX)


  while (level<map->maxlevel)
  {
    entrynr=(address >> (level *9)) & 0x1ff;

    if (m[entrynr].Map==NULL)
    {
      //create the map entry
      void *nm=malloc(4096);
      if (nm==NULL)
        return 1;

      zeromemory(nm,4096);
      m[entrynr].Map=(PMapData)nm;
    }

    m=m[entrynr].Map;
    level++;
  }

  entrynr=(address >> (level*9)) & 0x1ff;
  m[entrynr].Data=data;

  return 0;
}

void *map_getEntry(PMapInfo map, QWORD address)
{
  PMapData m=map->top;
  int level=0;
  int entrynr;

  address=address >> 12;
  while (level<map->maxlevel)
  {
    entrynr=(address >> (level*9)) & 0x1ff;

    m=m[entrynr].Map;
    if (m==NULL)
      return NULL;

    level++;
  }

  return m[entrynr].Data;
}

