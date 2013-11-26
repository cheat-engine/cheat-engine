#include <stdio.h>

//This file is used when a lot of memory needs to be allocated in small chunks
//Note: this is NOT thread safe, only let 1 thread access this code
#pragma pack 16
typedef __declspec(align(16)) struct _BigAllocs
{
	int totalsize;
	int pos;
	unsigned char *buffer;
} BigAllocs, *PBigAllocs;

__device__ BigAllocs *allocs=NULL;
__device__ int allocsPos=0;
__device__ int allocsMax=0;

__device__ void *balloc(int size)
{
	void *result=NULL;
    
	//printf("allocating %d bytes\n", size);
	
	
	if (allocs==NULL) //allocate a big amoung of memory first
	{
		printf("balloc first init\n");
		allocsMax=16;
		allocsPos=0;
		allocs=(PBigAllocs)malloc(allocsMax*sizeof(BigAllocs));		
		allocs[allocsPos].pos=0;
		allocs[allocsPos].totalsize=2*1024*1024;
		allocs[allocsPos].buffer=(unsigned char *)malloc(allocs[allocsPos].totalsize); //16MB	+1mb for each new pos	
		printf("allocs[allocsPos].buffer=%p\n", allocs[allocsPos].buffer);
		memset(allocs[allocsPos].buffer,0, allocs[allocsPos].totalsize);
	}	
	
	if (allocs[allocsPos].totalsize-allocs[allocsPos].pos<size) 
	{
		printf("balloc reinit\n");
		

		
		//a new BigAllocs object is needed
		allocsPos++;
		if (allocsPos>=allocsMax)
		{		
			PBigAllocs old=allocs;
			printf("Reallocating\n");
			
			allocs=(PBigAllocs)malloc(allocsMax*sizeof(BigAllocs)*2);
			memcpy(allocs, old, allocsMax*sizeof(BigAllocs));			
			
		    allocsMax*=2; //allocate more blocks			
		}
		
		allocs[allocsPos].pos=0;
		allocs[allocsPos].totalsize=2*1024*1024;
		allocs[allocsPos].buffer=(unsigned char *)malloc(allocs[allocsPos].totalsize);	
		memset(allocs[allocsPos].buffer,0, allocs[allocsPos].totalsize);
		
		printf("buffer=%p\n", allocs[allocsPos].buffer);
		
		
	}
	
	result=&allocs[allocsPos].buffer[allocs[allocsPos].pos];
	allocs[allocsPos].pos+=size;
	if (allocs[allocsPos].pos & 0xf) //make sure the next one is aligned
		allocs[allocsPos].pos=(allocs[allocsPos].pos + 0x10) & ~(0xf);
		

	//printf("allocs[allocsPos].pos=%x\n", allocs[allocsPos].pos);
		
	return result;	
	
}
