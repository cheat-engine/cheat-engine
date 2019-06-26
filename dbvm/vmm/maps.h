/*
 * maps.h
 *
 *  Created on: Jun 23, 2019
 *      Author: eric
 */

#ifndef VMM_MAPS_H_
#define VMM_MAPS_H_

#include "common.h"
#include "mm.h"

typedef struct _MapData
{
  union
  {
    void *Data;
    struct _MapData *Map;
  };
} MapData, *PMapData;


typedef struct
{
  int maxlevel;
  PMapData top;
} *PMapInfo, MapInfo;

PMapInfo createPhysicalMemoryMap();
int map_setEntry(PMapInfo map, QWORD address, void *data);
void *map_getEntry(PMapInfo map, QWORD address);

typedef void(*MAPCALLBACK)(QWORD address, void *data);
void map_foreach(PMapInfo map, MAPCALLBACK cb);

#endif /* VMM_MAPS_H_ */
