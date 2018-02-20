/* This will contain all routines to handle os specific routines, like getPID, 
 * 
 */


#include "osspecific.h"
#include "main.h"
#include "common.h"
#include "mm.h"
#include "vmpaging.h"

int getpid(pcpuinfo currentcpuinfo)
{
  
  //if xp sp2-32bit
  //[[kernel fs:124]+1ec]=pid

  ULONG fsbase=vmread(0x680e);  

  ULONG *part1=mapVMmemory(currentcpuinfo, fsbase+0x124, 4, NULL, NULL);

  if (part1)
  {
    ULONG *part2=mapVMmemory(currentcpuinfo, *part1+0x1ec,4, NULL, NULL);

    int pid=*part2;

    unmapVMmemory(part1,4);
    unmapVMmemory(part2,4);

    return pid;
  }

  return 0;
}

