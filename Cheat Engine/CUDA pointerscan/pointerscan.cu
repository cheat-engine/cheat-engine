/*
Pointerscan core
While the original pointerscan can run forever using recursive loops for each thread, the cuda implementation can not do that.
If a kernel does not exit within 2 seconds, it will crash. So, in order to deal with this, the recusrsive loop hase been replaced with a recursion replacement which
allows resume capability
*/
#include "cuda_runtime.h"

#include <windows.h>
#include <stdio.h>

#include "pscanfileaccess.h"
#include "cudapointervaluelist.cuh"

#define MAXCOMMANDLISTSIZE	2048
#pragma pack(16)

typedef __declspec(align(16)) struct _rcaller //recursion replacement
{
	UINT_PTR valueToFind;
	UINT_PTR startvalue;  
	UINT_PTR stopvalue; 
	PPointerList plist;
	int plistIndex;  //index in the plist to start off with
} rcaller, *prcaller;

typedef __declspec(align(16)) struct _workcommand  //same as continuedata but no plist data
{
	UINT_PTR valueToFind;  
	int level;  
	int *offsets; 
} WorkCommand, *PWorkCommand;


__global__ void pscan(PWorkCommand queueElements, PWorkCommand staticoutputqueue, int staticoutputquesize, PWorkCommand *allocatedoutputqueue, int *allocatedoutputsize)
/*
The pointerscanner iteration
*/
{
	int index = threadIdx.x;// 0; // blockIdx.x * blockDim.x + threadIdx.x;
	UINT_PTR stopValue = queueElements[index].valueToFind + 4096;
	PPointerList pl;
	int i;

	pl = findPointerValue(queueElements[index].valueToFind, &stopValue);

	if (pl == NULL)
	{
		staticoutputqueue[index].level = 666;
		staticoutputqueue[index].offsets = 0;
	}
	else
	{
		staticoutputqueue[index].level = 777;
		staticoutputqueue[index].offsets = 0;
	}
}



int pointerscan(UINT_PTR address, int structsize, int maxlevel)
{
	PWorkCommand wc = (PWorkCommand)malloc(sizeof(WorkCommand) * 1024);
	int wcsize = 1;

  int i=0;
  BOOL done=FALSE;  
  cudaError_t err;

  //loop till all are done
  /*
  
  while (!done)
  {	  
	  if (i%10==0)
	  {
	    int r=0;	  
		err=cudaMemcpyToSymbol(didWork, &r, sizeof(r));	
		if (err!=cudaSuccess)  
		{
		  printf("CUDA error: %s\n", cudaGetErrorString(err));  
	 	  break;
		}
		
		r=12;
		err=cudaMemcpyFromSymbol(&r, didWork, sizeof(r));
		if (err!=cudaSuccess)  
		{
		  printf("CUDA error: %s\n", cudaGetErrorString(err));  
	 	  break;
		}
		
		if (r!=0)
		{
			printf("FAIL\n");
			break;
		}		
	  }
	  
	//  printf("------------SCAN %d------------------\n", i);  
	  pscan<<<1,1024>>>(cd, structsize, 5); 
	  cudaDeviceSynchronize(); 
	  
	  err=cudaGetLastError();
	
	
	
	  if (err!=cudaSuccess)  
	  {
		printf("CUDA error: %s\n", cudaGetErrorString(err));  
		break;
	  }
	  
	  if (i%10==0)
	  {
	    int r=0;	  
		err=cudaMemcpyFromSymbol(&r, didWork, sizeof(r));	
		if (err!=cudaSuccess)  
		{
		  printf("CUDA error: %s\n", cudaGetErrorString(err));  
	 	  break;
		}
				
		if (r==0)
		  done=TRUE;
	  }	  
	  
	  i++;
	  

	  


  }
  
  //loop
  

  */
   
    
  
  
  return 0;
}