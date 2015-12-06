/*
mm.c:
This will manage the memory allocs and free's
Just used for basic initialization allocation, frees shouldn't happen too often

*/

#include "mm.h"
#include "main.h"
#include "multicore.h"
#include "common.h"


//#define sendstringf(s,x...)
//#define sendstring(s)


/*
a sorted linked list of allocated memory

*/
typedef struct _memorylistitem
{
  unsigned long long base;
	ULONG size;
	ULONG type; //0=free, 1=used
	struct _memorylistitem *next;
	struct _memorylistitem *previous;
} MemlistItem, *PMemlistItem;

PMemlistItem memorylist=NULL;
PMemlistItem firstfreeregion; //speeds it up
PMemlistItem first4Kfreeregion;

criticalSection mallocCS;

PMemlistItem findFreeRegion(unsigned int size)
{
  PMemlistItem temp=memorylist;

  //sendstringf("Scanning for free memory of size %d\n\r",size);

  while (temp)
  {
    //if free and size is bigger or equal to needed size
    if ((temp->type==0) && (temp->size>=size))
    {
      //we got a candidate
      //sendstringf("Candidate found :base=%8 size=%d\n\r",temp->base,temp->size);
      if (size>=4096)
      {

          unsigned int regionsize=temp->size;
          unsigned long long regionbase=temp->base;

          unsigned int difference=(4096-(regionbase % 4096)) % 4096;

          if (difference<sizeof(MemlistItem)+16)
          {
            //the difference isn't big enough for a new memory identifier
            //adjust by taking another 4K
            //sendstringf("Difference after base adjustment isn't big enough, taking 4K more\n\r");
            difference+=4096;
          }

          //decrease the size of the region needed for it to be alligned on 4kb
          regionsize-=difference;
          if (regionsize<size)
          {
            //NEXT
            //sendstringf("candidate not good enough, try again\n\r");
            temp=temp->next;
            continue;
          }
       }

       //sendstringf("Returned this candidate for usage\n\r");

       return temp;
    }
    else temp=temp->next;
  }

  return NULL;
}


void *malloc(unsigned int size)
{
/*
find a block of memory big enough to hold this data
if equal or bigger than 4096 bytes align on a page boundary
*/
  PMemlistItem newdescriptor;
  PMemlistItem freememory;

  unsigned int totalsize;

  sendstringf("------------>malloc(0x%x)<------------\n", size);

  if (mallocCS.locked)
  {
   // sendstringf("mallocCS.locked=%d\n\r",mallocCS.locked);
  }

  csEnter(&mallocCS);

  //find a region of memory where I can put a mem descriptor
  freememory=findFreeRegion(sizeof(MemlistItem));
  if (freememory==NULL)
  {
    csLeave(&mallocCS);
    return NULL;
  }

	size+=(4 - (size % 4)) % 4;
  if (size==0)
    size=4;

	totalsize=size;


  newdescriptor=(PMemlistItem)freememory->base; //place the new descriptor in the found memory
	newdescriptor->previous=freememory->previous;
	newdescriptor->next=freememory; //put it before the free memory
	newdescriptor->type=1; //in use

	if (freememory->previous==NULL)
		memorylist=newdescriptor;		//start of the memorylist
	else
		newdescriptor->previous->next=newdescriptor;

	//adjust the found free region to start later with a smaller free size
	freememory->previous=newdescriptor;
	freememory->base+=sizeof(MemlistItem);
	freememory->size-=sizeof(MemlistItem);



  freememory=findFreeRegion(size);

	if (freememory)
  {
		//now we can fill in the descriptor info
		newdescriptor->base=freememory->base;
    newdescriptor->size=size;
    newdescriptor->type=1; //in use now

		if (size>=4096)
		{
      /* adjust nextitem to have a 4K alligned base and give the unused stuff
			 * to the previous item */
			unsigned int difference=(4096-(newdescriptor->base % 4096)) % 4096;

		//	sendstringf("size>=4096: base=%8 difference=%x",nextitem->base,difference);

			if (difference>=4+sizeof(MemlistItem))
			{
				//this memory is too big to be wasted away, let's save it by making a memory descriptor here as well
				PMemlistItem temp2=(PMemlistItem)newdescriptor->base;
				temp2->base=newdescriptor->base+sizeof(MemlistItem);
				temp2->size=difference-sizeof(MemlistItem);
				temp2->type=0; //free to use
				temp2->previous=newdescriptor->previous;
				temp2->next=newdescriptor;

				//sendstringf("Creating a extra memlist item for the skipped mem(B=%8, s=%x)\n\r",temp2->base,temp2->size);

				if (newdescriptor->previous==NULL)
					memorylist=temp2;		//start of the memorylist
				else
				{
					//sendstringf("Adjusting previous memitem (B=%8) to point to the new one (B=8)\n\r",newdescriptor->base,temp2->base);
					newdescriptor->previous->next=temp2;
				}

				newdescriptor->previous=temp2;


			}
			else
			{
				//adjusted the size of the previous item to take the skipped memory in
				if (newdescriptor->previous)
				{
					newdescriptor->previous->size+=difference;
				}
				else
				{
				//	sendstringf("Allocation weirdness. selected item has no previous\n\r");
				}
				totalsize+=difference;
			}

			newdescriptor->base+=difference; //set the base to 4k alligned

		}



    freememory->base=newdescriptor->base+size;
    freememory->type=0; //stays 0, free
    freememory->size-=totalsize; //decrease free memory with size

    csLeave(&mallocCS);
    return (void *)(newdescriptor->base);
  }

 // sendstringf("Failed allocating memory\n\r");
  csLeave(&mallocCS);

  sendstring("!!!!!!!!!!!!!!!!!!!!!!!OUT OF MEMORY!!!!!!!!!!!!!!!!!!!!!!!\n");


  return NULL; //no memory
}

PMemlistItem getRegion(void *pointer)
{
  PMemlistItem region=memorylist;
  unsigned long long address=(unsigned long long)pointer;

  while (region)
  {
    //sendstring("Scanning for region %8. This region=%8 and size=%x\n\r",address,region->base,region->size);
    if (region->base==address)
      return region;

    //not in this region, so check next
    region=region->next;
  }

  //nothing found
  return NULL;
}

void free(void* pointer)
{
  /* don't call too often, it'll eventually mess up your memory */
  PMemlistItem region=getRegion(pointer);

  //find this buffer and mark that region as free
  if (region)
  {
    region->type=0; //free
  }
  else
  {
    //sendstringf("Failed freeing memory\n\r");
  }

}

UINT64 MapPhysicalMemory(UINT64 address, UINT64 VirtualAddress)
{
  return MapPhysicalMemoryEx(address,VirtualAddress,0);
}

UINT64 MapPhysicalMemoryEx(UINT64 address, UINT64 VirtualAddress, int writable)
{
  /* This will map the physical address specified at the virtual address specified, with at least minsize (in chunks of 2MB)
		 return value will be the virtual address the specified address is located
	*/

  //sendstringf("Mapping physical address %6 at base VirtualAddress %8\n\r",address,VirtualAddress);
	int Dirptr=VirtualAddress >> 30;
	int Dir=(VirtualAddress >> 21) & 0x1ff;
	PPDE_PAE usedpagedir=(PPDE_PAE)((unsigned long long)pagedirvirtual+(unsigned long long)Dirptr*0x1000);

  int Dirptr2=(VirtualAddress+0x00200000) >> 30;
  int Dir2=((VirtualAddress+0x00200000) >> 21) & 0x1ff;
  PPDE_PAE usedpagedir2=(PPDE_PAE)((unsigned long long)pagedirvirtual+(unsigned long long)Dirptr2*0x1000);

  *(unsigned long long*)&usedpagedir[Dir]=(address & 0xFFFFFFFFFFE00000ULL);
  usedpagedir[Dir].P=1;
	usedpagedir[Dir].PS=1;
	usedpagedir[Dir].RW=writable;
	usedpagedir[Dir].US=1;
	_invlpg(VirtualAddress);


  //could add a extra check, but that is slower than just executing this
  *(unsigned long long*)&usedpagedir2[Dir2]=((address+0x00200000) & 0xFFFFFFFFFFE00000ULL);
  usedpagedir2[Dir2].P=1;
  usedpagedir2[Dir2].PS=1;
  usedpagedir2[Dir2].RW=writable;
  usedpagedir2[Dir2].US=1;
  _invlpg(VirtualAddress+0x00200000);

  unsigned long long result=VirtualAddress+(address % 0x00200000);
  //sendstringf("Mapped memory, result=%8\n\r",result);

  return result;
}


unsigned int maxAllocatableMemory(void)
/* will return the maximum available memory that can be allocated
   Note: this is NOT the total ammount of free memory */
{
	PMemlistItem region=memorylist;
  PMemlistItem maxsizeregion=NULL;
	while (region)
	{
		if (region->type==0)
		{
			if ((maxsizeregion==NULL) || (maxsizeregion->size<region->size))
				maxsizeregion=region;
		}
    region=region->next;
	}
	return maxsizeregion->size;
}




void InitializeMM(UINT64 BaseVirtualAddress)
{
  /* memorylist contains the virtual address that is freely accessible */

  sendstringf("Initializing Memory Manager and keeping %d bytes reserved for the stack of %d cpu's\n",cpucount*MAX_STACK_SIZE, cpucount);
  sendstringf("&memorylist=%6\n\r", (UINT64)&memorylist);
  sendstringf("memorylist=%6\n\r", (UINT64)memorylist);

  memorylist->base=BaseVirtualAddress+sizeof(MemlistItem);
  memorylist->size=(0x007fffff-(cpucount*MAX_STACK_SIZE))-memorylist->base; //make room for the stack of the cpucores (each cpu will get 128KB stack)
  memorylist->type=0; //free
  memorylist->previous=NULL;
  memorylist->next=NULL;

  sendstringf("Available memory ranges from %6 to %6", memorylist->base, memorylist->base+memorylist->size);
  //sendstringf("memorylist->size=%d\n",memorylist->size);

  firstfreeregion=memorylist;
  first4Kfreeregion=memorylist;
}

void printMMregions()
{
	PMemlistItem mi=memorylist;
	int i=0;
	int j;
	int overlap=0;
	//sendstringf("The following regions of memory are described:\n\r");
	while (mi)
	{
    unsigned int b=NULL;
		if (mi->previous)
			b=mi->previous->base;

		//sendstringf("%d: Base=%8 Size=%x Previous_base=%8 ",i,mi->base,mi->size,b);
		if (mi->type==0)
		{
			//sendstringf("Free");
		}

		//sendstringf("\n\r");

		mi=mi->next;
		i++;
	}

//one more safety check: overlapping of memory...
	mi=memorylist;
	i=0;
	j=0;
	while (mi)
	{
		PMemlistItem temp=mi->next;
		while (temp)
		{
      if (
				((temp->base>=mi->base) && ((temp->base+temp->size)<(mi->base+mi->size))) ||
				 ((mi->base>=temp->base) && ((mi->base+mi->size)<(temp->base+temp->size)))
				)
			{
				overlap=1;
				break;

			}

			if (overlap)
				break;

			temp=temp->next;
			j++;
		}

		mi=mi->next;
		i++;
	}

	if (overlap)
	{
		//sendstringf("OVERLAP DETECTED:   %d and %d !!!\n\r",i,j);
	}
	else
	{
		//sendstringf("Seems to be ok\n\r");
	}

}

void SetPageToWriteThrough(UINT64 address)
{
  /* pagedirvirtual contains the virtual address where the pagetable is stored */
  PPDE2MB_PAE usedpagedir;
  UINT64 Dirptr=(UINT64)(address >> 30) & 0x1ff;
  UINT64 Dir=(UINT64)(address >> 21) & 0x1ff;
  UINT64 Offset=address & 0x1fffff;
  UINT64 result=0xffffffffffffffffULL;

  sendstringf("Marking %6 as WriteThrough\n", address);

  usedpagedir=(PPDE2MB_PAE)((UINT64)pagedirvirtual+(UINT64)Dirptr*0x1000);

  //this design doesn't use more than 4GB VIRTUAL ram addressing, even though it is 64, bit, so only the level0 pagedirptr is enough

  if (usedpagedir[Dir].P)
  {
    if (usedpagedir[Dir].PS==1)
    {
      sendstring("This pagedir is a BIG page (bad idea)\n");
      usedpagedir[Dir].PWT=1;
      _invlpg(address);

    }
    else
    {
      //this is a pagedir without PS bit, pfn is 12 bits shifted now (or just clear first 12 bits)
      PPDE_PAE usedpagedirNOPS=(PPDE_PAE)((UINT64)pagedirvirtual+(UINT64)Dirptr*0x1000);

      //it has a pagetable (loadedOS?)
      UINT64 Offset2=address & 0xfff;
      UINT64 Table=(address >> 12) & 0x1ff;

      PPTE_PAE usedpagetable=(PPTE_PAE)MapPhysicalMemory((UINT64)(usedpagedirNOPS[Dir].PFN) << 12, 0x0fc00000);

      if (usedpagetable[Table].P)
      {
        sendstring("Marking the pagetable entry (good)\n");
        usedpagedir[Dir].PWT=1;
        _invlpg(address);
      }
      else
      {
        sendstring("Not present pagetable entry\n");
      }

    }
  }
  else
  {
    sendstring("pagedir is NOT present\n");
    //sendstringf("Dirptr=%d Dir=%d Offset=%x\n\r", Dirptr, Dir, Offset);
    //sendstringf("pagedirvirtual=%6\n\r",(UINT64)pagedirvirtual);
    //sendstringf("usedpagedir=%6\n\r",(UINT64)usedpagedir);
    //sendstringf("usedpagedir[Dir].P==0\n\r");
    //sendstringf("&usedpagedir[Dir]==%6\n\r",(UINT64)&usedpagedir[Dir]);


  }
}



UINT64 VirtualToPhysical(UINT64 address)
{
  /* pagedirvirtual contains the virtual address where the pagetable is stored */
  PPDE2MB_PAE usedpagedir;
  UINT64 Dirptr=(UINT64)(address >> 30) & 0x1ff;
  UINT64 Dir=(UINT64)(address >> 21) & 0x1ff;
  UINT64 Offset=address & 0x1fffff;
  UINT64 result=0xffffffffffffffffULL;


 // sendstringf("VirtualToPhysical %6\n",address);
  //sendstringf("Dirptr=%d, Dir=%d\n", Dirptr, Dir);


  usedpagedir=(PPDE2MB_PAE)((UINT64)pagedirvirtual+(UINT64)Dirptr*0x1000);


  //sendstringf("usedpagedir=%6\n",usedpagedir);


  //this design doesn't use more than 4GB VIRTUAL ram addressing, even though it is 64, bit, so only the level0 pagedirptr is enough

  if (usedpagedir[Dir].P)
  {
    //sendstring("pagedir is present\n");
    if (usedpagedir[Dir].PS==1)
    {
      //sendstring("This pagedir is a BIG page\n");
      result=(UINT64)((UINT64)usedpagedir[Dir].PFN << 13)+Offset;
    }
    else
    {
      //this is a pagedir without PS bit, pfn is 12 bits shifted now (or just clear first 12 bits)
      PPDE_PAE usedpagedirNOPS=(PPDE_PAE)((UINT64)pagedirvirtual+(UINT64)Dirptr*0x1000);
      //sendstringf("This pagedir(%6) has a pagetable\n", *(UINT64 *)(&usedpagedir[Dir]));

      //it has a pagetable (loadedOS?)
      UINT64 Offset2=address & 0xfff;
      UINT64 Table=(address >> 12) & 0x1ff;

//tip: Improve this by caching the page in a non changing local page
      PPTE_PAE usedpagetable=(PPTE_PAE)MapPhysicalMemory((UINT64)(usedpagedirNOPS[Dir].PFN) << 12, 0x0fc00000);

      //sendstringf("Mapped the usedpagetable at %6\n", (UINT64)usedpagetable);

      //sendstringf("Table=%d\n",Table);

      if (usedpagetable[Table].P)
      {
        result=(UINT64)((UINT64)usedpagetable[Table].PFN << 12)+Offset2;
      }
      else
      {
        //sendstring("Not present pagetable entry\n");
      }

    }
  }
  else
  {
    //sendstring("pagedir is NOT present\n");
    //sendstringf("Dirptr=%d Dir=%d Offset=%x\n\r", Dirptr, Dir, Offset);
    //sendstringf("pagedirvirtual=%6\n\r",(UINT64)pagedirvirtual);
    //sendstringf("usedpagedir=%6\n\r",(UINT64)usedpagedir);
    //sendstringf("usedpagedir[Dir].P==0\n\r");
    //sendstringf("&usedpagedir[Dir]==%6\n\r",(UINT64)&usedpagedir[Dir]);


  }

  return result;
}

