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
  
  //if (os==0)
  
  int notpaged=0;
  
  ULONG fsbase=vmread(0x680e);  
  ULONG *part1=(ULONG *)(UINT64)MapPhysicalMemory(getPhysicalAddressVM(currentcpuinfo, fsbase+0x124, &notpaged), currentcpuinfo->AvailableVirtualAddress);
  ULONG *part2;
  
  if (notpaged)
  {
    part2=(ULONG *)(UINT64)MapPhysicalMemory(getPhysicalAddressVM(currentcpuinfo, *part1+0x1ec, &notpaged), currentcpuinfo->AvailableVirtualAddress);
    if (notpaged)
      return 0;
    else
      return *part2;      
  }
  else   
    return 0;  
}

