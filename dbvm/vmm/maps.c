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
  PMapInfo r = NULL;
  if (maxlevel==0) return NULL;

  r = (PMapInfo)malloc(sizeof(MapInfo));

  zeromemory(r, sizeof(MapInfo));
  r->maxlevel = maxlevel;

  r->top = (PMapData)malloc(4096);
  zeromemory(r->top, 4096);

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


  int neededbits = MAXPHYADDR - 12;
  int levelneeded = neededbits / 9;
  if (neededbits % 12)
    levelneeded++;

  return createMap(levelneeded);
}

int map_setEntry(PMapInfo map, QWORD address, void *data)
{
  PMapData m = map->top;
  int level = map->maxlevel;
  int entrynr;
  address = address >> 12;

  //address: XXXXXXXXXXXXX(XXX)
  //binary XXXXXXX XXXXXXXXX XXXXXXXXX XXXXXXXXX XXXXXXXXX XXXXXXXXX (XXXXXXXXXXXX)
  //binary: XXXXXXXXX XXXXXXXXX XXXXXXXXX XXXXXXXXX (XXXXXXXXXXXX)


  while (level > 0)
  {
    entrynr = (address >> (level * 9)) & 0x1ff;

    if (m[entrynr].Map == NULL)
    {
      //create the map entry
      void *nm = malloc(4096);
      if (nm == NULL)
        return 1;

      zeromemory(nm, 4096);
      m[entrynr].Map = (PMapData)nm;
    }

    m = m[entrynr].Map;
    level--;
  }

  entrynr = address & 0x1ff;
  m[entrynr].Data = data;  //EPT cloak: data is a pointer to an array of cpu's

  return 0;
}

void *map_getEntry(PMapInfo map, QWORD address)
{
  PMapData m = map->top;
  int level = map->maxlevel;
  int entrynr;

  address = address >> 12;
  while (level > 0 )
  {
    entrynr = (address >> (level * 9)) & 0x1ff;

    m = m[entrynr].Map;
    if (m == NULL)
      return NULL;

    level--;
  }

  entrynr = address & 0x1ff;
  return m[entrynr].Data;
}



void map_iterate(PMapData mapdata, QWORD baseaddress, int level, MAPCALLBACK cb)
{
  int i;

  int addresspart = level * 9;


  for (i = 0; i < 512; i++)
  {
    if (mapdata[i].Map)
    {
      QWORD newaddress = baseaddress + (QWORD)((QWORD)i << addresspart);

      if (level==0)
        cb(newaddress, mapdata[i].Data);
      else
        map_iterate(mapdata[i].Map, newaddress, level - 1, cb);
    }
  }
}

void map_foreach(PMapInfo map, MAPCALLBACK cb)
{
  map_iterate(map->top, 0, map->maxlevel, cb);
}
