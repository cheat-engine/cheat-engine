#include "cuda_runtime.h"

#include <windows.h>
#include <stdio.h>

#include "pscanfileaccess.h"
#include "cudapointervaluelist.cuh"

typedef struct _rcaller //recursion replacement
{
	UINT_PTR startvalue;  //0 when not used
	UINT_PTR stopvalue;  //
	PPointerList plist;
	int plistIndex;  
} rcaller, *prcaller;

typedef struct _continueData
{	
	int level;  //the current level	(-1 if not running)
	prcaller caller; //
	int *offsets; //temp offset list
} ContinueData, *PContinueData;

typedef struct _workcommand  //same as continuedata but no plist data
{
	UINT_PTR startvalue;  
	UINT_PTR stopvalue;  
	int level;  
	int *offsets; 
} WorkCommand, *PWorkCommand;

typedef struct _workCommandList
{
	unsigned int count;
	WorkCommand list[16];
} WorkCommandList, *PWorkCommandList;

__device__ WorkCommandList SavedWorkCommandList;
__device__ WorkCommandList SavedWorkCommandList2;


__global__ void pscan(PContinueData cd, int structsize, int maxlevel, PWorkCommandList PreviousSavedCommandList, PWorkCommandList CurrentSavedCommandList)
/*
The pointerscanner loop
*/
{
  int index = blockIdx.x * blockDim.x + threadIdx.x;
  int level;

  printf("pscan index=%d\n", index);

  //check if the current thread should continue from the last spot, or fetch a new work command
  if (cd[index].level==-1)
  {
    //fetch the state from the previous saved commad list
    unsigned int i=atomicDec(&(PreviousSavedCommandList->count), 0); //it never goes below 0
    
    if (i>0)
    {  
      //something was in it
      int level;
      i--;
      level=PreviousSavedCommandList->list[i].level;
      cd[index].level=level;
      
      cd[index].caller[cd[index].level].startvalue=PreviousSavedCommandList->list[i].startvalue;
      cd[index].caller[cd[index].level].stopvalue=PreviousSavedCommandList->list[i].stopvalue;      
      
      //memcpy(cd[index].offsets, PreviousSavedCommandList->list[i].offsets, sizeof(int)*PreviousSavedCommandList->list[i].level); 
            
      printf("Has data: PreviousSavedCommandList->count=%d (i=%d)\n", PreviousSavedCommandList->count, i);
    };    
      
  }
  
  
  level=cd[index].level;
  
  while (level>=0)
  {  
	//continue from the current state
	
	UINT_PTR startvalue=cd[index].caller[level].startvalue;
	UINT_PTR stopvalue=cd[index].caller[level].stopvalue;
	PPointerList plist=NULL;
	
	printf("%d: Processing\n", index);
	printf("%d: StartValue:%x\n", index, (unsigned int)startvalue);
	printf("%d: StopValue: %x\n", index, (unsigned int)cd[index].caller[level].stopvalue);
	
	plist=findPointerValue(startvalue, &stopvalue);
	printf("plist=%p\n", plist);
	
	printf("stopvalue=%x\n", (unsigned int)stopvalue);
	
	
	//.....
	
	level--;
	if (level>=0)
	{
		if (cd[index].caller[level].startvalue)
		{
		  //it has data, set it up
		  cd[index].level=level;
		}
		else
		{
		  cd[index].level=-1; //end of recursive call reached
		}		
	}
  }





}

__global__ void initpscan(PContinueData cd, UINT_PTR address, int structsize, int maxlevel, UINT_PTR valuetofind)
{
	int index = blockIdx.x * blockDim.x + threadIdx.x;
	
	cd[index].caller=(prcaller)malloc(sizeof(rcaller)*maxlevel);
	if (index==0)
	{
	    int i;
		printf("initpscan %d\n", index);
	
		cd[index].level=0;
		
		cd[index].caller[0].startvalue=address-structsize;
		cd[index].caller[0].stopvalue=address; 
		
		SavedWorkCommandList.count=0;
		SavedWorkCommandList2.count=0;	
		
		for (i=0; i<16; i++)
		{
			SavedWorkCommandList.list[i].offsets=(int *)malloc(sizeof(int)*maxlevel);
			SavedWorkCommandList2.list[i].offsets=(int *)malloc(sizeof(int)*maxlevel);
		}
	}
	else
	{	
		cd[index].level=-1;		
	}

}


int pointerscan(UINT_PTR address, int structsize, int maxlevel)
{
  PContinueData cd;
  
  PWorkCommandList l1, l2;
  
  
  cudaMalloc(&cd, sizeof(ContinueData)*1024);
  initpscan<<<1,1024>>>(cd, address, structsize, maxlevel, address);

  cudaDeviceSynchronize(); 
  printf("CUDA error: %s\n", cudaGetErrorString(cudaGetLastError()));     


  //loop till all are done
  cudaGetSymbolAddress((void **)&l1, SavedWorkCommandList);
  cudaGetSymbolAddress((void **)&l2, SavedWorkCommandList2);
  
  pscan<<<1,1>>>(cd, structsize, 5, l1, l2); 
  
  //loop
  
  cudaDeviceSynchronize(); 
  printf("CUDA error: %s\n", cudaGetErrorString(cudaGetLastError()));    
  
   
    
  
  
  return 0;
}