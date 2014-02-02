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

typedef __declspec(align(16)) struct _continueData
{	
	int level;  //the current level	(-1 if not running)
	prcaller caller; //
	int *offsets; //temp offset list
} ContinueData, *PContinueData;

typedef __declspec(align(16)) struct _workcommand  //same as continuedata but no plist data
{
	UINT_PTR valueToFind;  
	int level;  
	int *offsets; 
} WorkCommand, *PWorkCommand;

typedef __declspec(align(16)) struct _workCommandList
{
	unsigned int lock; //not 0 when locked
	unsigned int count;
	WorkCommand list[MAXCOMMANDLISTSIZE];
} WorkCommandList, *PWorkCommandList;

__device__ WorkCommandList SavedWorkCommandList;


__device__ int didWork;


__device__ int pscan2(PContinueData cd, int structsize, int maxlevel)
/*
The pointerscanner loop

When loading take from PreviousSavedCommandList, or CurrentSavedCommandList

Thing I learned after trying to debug from 10PM to 5:30AM:  atomic functions do not work in __global__ functions
*/
{
  int timeout=4096;
  int index = blockIdx.x * blockDim.x + threadIdx.x;
  int level=-1;
 
 
 //printf("blockIdx.x=%d blockDim.x=%d threadIdx.x=%d\n", blockIdx.x, blockDim.x, threadIdx.x);
//return;
 
  /*if (SavedWorkCommandList.count>MAXCOMMANDLISTSIZE)
  {
	printf("SavedWorkCommandList.count invalid at entry: %d\n", SavedWorkCommandList.count);
	return;
  }*/
  	

  //printf("pscan index=%d\n", index);

  //check if the current thread should continue from the last spot, or fetch a new work command
  if (cd[index].level==-1)
  {
    //This tread was idle. Fetch the state from the previous saved command list
    int i;/*=SavedWorkCommandList.count;
    
    if (i>MAXCOMMANDLISTSIZE)
    {
		printf("SavedWorkCommandList.count corrupted.  %d\n", i);
		
		return;
    }*/
    
    if (SavedWorkCommandList.count>0)
		i=atomicSub((unsigned int*)&SavedWorkCommandList.count, 1);
	else
		i=0;
      
   
    
    if (i>0)  //count==1 means index 0
    {  
      //something was in it
      int level;
      
       //printf("idle thread.  Fetched item %d from the SavedWorkCommandList\n", i);
      
     // printf("i>0\n");
      
      i--;
      
      /*
      if (i<0)
      {
		printf("i<0\n");
		

		return;
      }
      */
      
      level=SavedWorkCommandList.list[i].level;
      
      //debug
      /*
      if (level<0)
      {
		printf("Level(%d)<0 ERROR!\n", level);
	
		return;
      }
      
      if (level>=maxlevel)
      {
		printf("level(%d)>maxlevel(%d)\n", level, maxlevel);
		printf("i=%d\n", i);
		printf("valuetofind=%x\n", (unsigned int)SavedWorkCommandList.list[i].valueToFind);
		
		return;
      }*/
      
      cd[index].level=level;      
      
      cd[index].caller[level].valueToFind=SavedWorkCommandList.list[i].valueToFind;
      cd[index].caller[level].startvalue=SavedWorkCommandList.list[i].valueToFind-structsize;
      cd[index].caller[level].stopvalue=SavedWorkCommandList.list[i].valueToFind;      
      cd[index].caller[level].plistIndex=0;
      cd[index].caller[level].plist=NULL;
      
      if (cd[index].caller[level].valueToFind==0)
      {
		printf("cd[index].caller[level].valueToFind set to 0 by SavedWorkCommandList\n");
      }
      
     // printf("%d, %d, %p  (%d)\n", index, level, &cd[index].caller[level].valueToFind, i); 
      
      if (level)
      {
		memcpy(cd[index].offsets, SavedWorkCommandList.list[i].offsets, sizeof(int)*level); 
		cd[index].caller[level-1].valueToFind=0; //mark the previous caller as invalid (exit point)
		cd[index].caller[level-1].startvalue=0;
	  }
    /*        
      printf("Has data: SavedWorkCommandList.count=%d (i=%d)\n", SavedWorkCommandList.count, i);
      printf("cd[%d].level = %d\n", index, cd[index].level);
	  printf("cd[%d].caller[%d].valueToFind = %x\n", index, cd[index].level, (unsigned int)cd[index].caller[cd[index].level].valueToFind);      
      printf("cd[%d].caller[%d].startvalue = %x\n", index, cd[index].level, (unsigned int)cd[index].caller[cd[index].level].startvalue);
      printf("cd[%d].caller[%d].stopvalue = %x\n", index, cd[index].level, (unsigned int)cd[index].caller[cd[index].level].stopvalue);
      */
    }
    else
    {
	//  printf("No data in the list\n");	 
	  atomicExch(&SavedWorkCommandList.count, 0);
    }
      
  }
  
  
  level=cd[index].level;
  if ((level>=0) && (didWork==0))
  {
    didWork=1;
   // printf("Going to do work. Level=%d\n", level);
  }
    
  //wait till all threads are here  
  

  
  __syncthreads(); 
  
 // printf("after __syncthreads()\n");
 // printf("SavedWorkCommandList.count=%d\n", SavedWorkCommandList.count);
  
  //from now on SavedWorkCommandList will be used for output only. (First increase the count, and then use the previous value as input
  while (level>=0)
  {  
	//continue from the current state
	
	UINT_PTR valueToFind=cd[index].caller[level].valueToFind;
	UINT_PTR startvalue=cd[index].caller[level].startvalue;
	UINT_PTR stopvalue=cd[index].caller[level].stopvalue;
	PPointerList plist=cd[index].caller[level].plist;
	int plistIndex=cd[index].caller[level].plistIndex;  
	BOOL levelChanged=FALSE;
	
	
	/*if (valueToFind==0)
	{
		printf("valueTofind==0  (startvalue=%x stopvalue=%x)\n", (unsigned int)startvalue, (unsigned int)stopvalue);
		break;
	}*/
	
//	printf("level %d\n", level);
	
//	printf("%d: Processing\n", index);
//	printf("%d: valueToFind:%x\n", index, (unsigned int)valueToFind);
//	printf("%d: StartValue:%x\n", index, (unsigned int)startvalue);
//	printf("%d: StopValue: %x\n", index, (unsigned int)stopvalue);
//	printf("%d: plistIndex: %d\n", index,  plistIndex);


	
	while (stopvalue>=startvalue) 
	{	
		int currentOffset=valueToFind-stopvalue;
	
		if (plist==NULL)
		{		
			plist=findPointerValue(startvalue, &stopvalue);
			plistIndex=0;
		}
			
	//	printf("%d:plist=%p  (stopvalue=%x startvalue=%x\n", index, plist, (unsigned int)stopvalue, (unsigned int)startvalue);	
		
		if (plist)
		{
			int i;
			cd[index].offsets[level]=currentOffset;
			
			for (i=plistIndex; i<plist->pos; i++)
			{
			
				timeout--;
				if (timeout<0)
				{
				//	printf("timeout. Saving state:\n");
			/*
					if (level>=maxlevel)
					{
					   printf("level is fucked\n");
					}
					*/
								
					cd[index].level=level;
					cd[index].caller[level].valueToFind=valueToFind;
					cd[index].caller[level].startvalue=startvalue;
					cd[index].caller[level].stopvalue=stopvalue;
					cd[index].caller[level].plist=plist;
					cd[index].caller[level].plistIndex=i; //this one
					
					if (cd[index].caller[level].valueToFind==0)
					{
						printf("cd[%d].level=%d\n", index, cd[index].level);
						printf("cd[%d].caller[%d].valueToFind=%x\n", index, cd[index].level, (unsigned int)cd[index].caller[level].valueToFind);
						printf("cd[%d].caller[%d].startvalue=%x\n", index, cd[index].level, (unsigned int)cd[index].caller[level].startvalue);
						printf("cd[%d].caller[%d].stopvalue=%x\n", index, cd[index].level, (unsigned int)cd[index].caller[level].stopvalue);
						printf("cd[%d].caller[%d].plist=%x\n", index, cd[index].level, (unsigned int)cd[index].caller[level].plist);
						printf("cd[%d].caller[%d].plistIndex=%x\n", index, cd[index].level, (unsigned int)cd[index].caller[level].plistIndex);
					}
					return timeout;
				}
							
				
	//			printf("%d: offset: %x\n",i, currentOffset);
				
				
				if (plist->list[i].staticdata)
				{
			//	   printf("%d: FOUND A STATIC\n", index);
				   int k;
				   
				 //  printf("plist->list[%d].address=%x\nsd=%p  offset=%x\n", i, (unsigned int)plist->list[i].address, plist->list[i].staticdata, plist->list[i].staticdata->offset);
		 
		 /*
				   printf("%x ", plist->list[i].staticdata->offset);
				   
				   for (k=level; k>=0; k--)
				   {
				     printf("- %x", cd[index].offsets[k]); 
				   }
				   
				   printf("\n");
				   */
				   
				  // timeout=timeout-1000;
				   
				}
				else
				{				
					if ((level+1)<maxlevel)
					{
						//add this path to the commandlist if possible
						int j=MAXCOMMANDLISTSIZE-10;  //Here, J is used as a countdown
						
						if (SavedWorkCommandList.count<MAXCOMMANDLISTSIZE-128)
						{
							//I could add it. Let's see if I can acuire a lock
							while (j<MAXCOMMANDLISTSIZE) //try to lock it multiple times
							{
								if (atomicExch(&SavedWorkCommandList.lock, 1)==0)
								{							
									//Acuired a lock
									
									//printf("Acquired lock after %d tries\n", MAXCOMMANDLISTSIZE-j);
									j=SavedWorkCommandList.count;
									
									if (j<MAXCOMMANDLISTSIZE) //if it's valid (there's room)
									  SavedWorkCommandList.count++;  //increase the counter
									
									atomicExch(&SavedWorkCommandList.lock, 0); //unlock		
									break;						
								}
								else
								{									
									//printf("Writer: Failure to acquire lock (%d)\n", MAXCOMMANDLISTSIZE-j);									
									j++;
								}								
							}
						}
						else
						{
							//printf("Didn't bother to add: %d\n", SavedWorkCommandList.count);
							j=MAXCOMMANDLISTSIZE;
						}
						
						//here J is used as an index into the list
						
						
			//			printf("Entering this offset. Maxlevel not reached. Got commandlist slot %d\n", j);
						
						if (j<MAXCOMMANDLISTSIZE) //we got a valid block
						{
							//add to the commandlist
			//				printf("Filling in the commandlist entry\n");
							SavedWorkCommandList.list[j].valueToFind=plist->list[i].address;
							
							if (SavedWorkCommandList.list[j].valueToFind==0)
							{
								printf("SavedWorkCommandList valueToFind was set to 0\n");								
							}
							SavedWorkCommandList.list[j].level=level+1;			
							memcpy(SavedWorkCommandList.list[j].offsets, cd[index].offsets, sizeof(int)*(level+1)); 
						
						}
						
						else
						{
						    //lock failed, or full
						    
							//do this myself
							
							
			//				printf("Entering this entry myself\n");		
							//store the current state
							
							cd[index].caller[level].valueToFind=valueToFind;
							cd[index].caller[level].startvalue=startvalue;
							cd[index].caller[level].stopvalue=stopvalue;
							cd[index].caller[level].plist=plist;
							cd[index].caller[level].plistIndex=i+1; //next one
							
							level++;						
							cd[index].caller[level].valueToFind=plist->list[i].address;
							cd[index].caller[level].startvalue=plist->list[i].address-structsize;
							cd[index].caller[level].stopvalue=plist->list[i].address;
							cd[index].caller[level].plist=NULL;
							cd[index].caller[level].plistIndex=0;
							
							if (cd[index].caller[level].valueToFind==0)
							{
								printf("cd[index].caller[level].valueToFind was set to 0\n");								
							}
														
							
			//				printf("Changed the level\n");
							levelChanged=true;
							break;											
						}
						
						
					}
				}
				
				//TIMEOUT CHECK
				
				//check if this kernel has ran long enough
				//timeout--;
				
				if (timeout<0)
				{
					//printf("timeout. Saving state:\n");
			/*
					if (level>=maxlevel)
					{
					   printf("level is fucked\n");
					}
					*/
					
					cd[index].level=level;
					cd[index].caller[level].valueToFind=valueToFind;
					cd[index].caller[level].startvalue=startvalue;
					cd[index].caller[level].stopvalue=stopvalue;
					cd[index].caller[level].plist=plist;
					cd[index].caller[level].plistIndex=i+1; //next one
					
					/*
					if (cd[index].caller[level].valueToFind==0)
					{					
						printf("cd[%d].level=%d\n", index, cd[index].level);
						printf("cd[%d].caller[%d].valueToFind=%x\n", index, cd[index].level, (unsigned int)cd[index].caller[level].valueToFind);
						printf("cd[%d].caller[%d].startvalue=%x\n", index, cd[index].level, (unsigned int)cd[index].caller[level].startvalue);
						printf("cd[%d].caller[%d].stopvalue=%x\n", index, cd[index].level, (unsigned int)cd[index].caller[level].stopvalue);
						printf("cd[%d].caller[%d].plist=%x\n", index, cd[index].level, (unsigned int)cd[index].caller[level].plist);
						printf("cd[%d].caller[%d].plistIndex=%x\n", index, cd[index].level, (unsigned int)cd[index].caller[level].plistIndex);
					}*/
					return timeout;
				}
				
				
				
			}
			
			if (levelChanged)
			{
	//			printf("Level changed. Exiting for loop\n");
			    break;
			}
			
			plistIndex=0;
			
			plist=plist->Previous;
		    if (plist)
				stopvalue=plist->PointerValue;
		    else
		    {
				break; //nothing else to be found    		
			}
			
		
		}
		else
		  break;
		  
		  
	}
	
	if (!levelChanged)
	{
		//go back if possible
		level--;
		if (level>=0)
		{
	//		printf("going up a level if possible\n");
			if (cd[index].caller[level].valueToFind)
			{
			  //it has data, select it
	//		  printf("possible\n");
			  cd[index].level=level;
			}
			else
			{
	//		  printf("Not possible. Exiting...\n");
	
			  //try to get an entry from the worklist
			  
			  //experimental:
			  {
				int i=10;
				BOOL canContinue=FALSE;
				
				if (SavedWorkCommandList.count>0) //there's something in the list
				{
					
					while (i>0) //try to acquire a lock multiple times
					{
						if (atomicExch(&SavedWorkCommandList.lock, 1)==0)
						{							
							//Acuired a lock
							
							
							
							if (SavedWorkCommandList.count) //there's still something in the list
							{			
								//printf("Acquired lock after %d tries\n", 10-i);
							
								i=SavedWorkCommandList.count-1;
								level=SavedWorkCommandList.list[i].level;			
								cd[index].level=level;					
								cd[index].caller[level].valueToFind=SavedWorkCommandList.list[i].valueToFind;								
								cd[index].caller[level].startvalue=SavedWorkCommandList.list[i].valueToFind-structsize;
								cd[index].caller[level].stopvalue=SavedWorkCommandList.list[i].valueToFind;      
      								
								cd[index].caller[level].plist=NULL;
								cd[index].caller[level].plistIndex=0;
								if (level)
								{
								    memcpy(cd[index].offsets, SavedWorkCommandList.list[i].offsets, sizeof(int)*level); 
									cd[index].caller[level-1].valueToFind=0; //mark the previous level as invalid
									cd[index].caller[level-1].startvalue=0;
								}
								
								canContinue=TRUE;
					
								SavedWorkCommandList.count--; //take it from the list
							}
							
							atomicExch(&SavedWorkCommandList.lock, 0); //unlock		
							break;						
						}
						else
						{									
							//printf("Writer: Failure to acquire lock (%d)\n", MAXCOMMANDLISTSIZE-j);									
							i--;
						}								
					}
				}
				
			    if (canContinue==FALSE)
			    {	
			      cd[index].level=-1; //end of recursive call reached, mark this thread as inactive

				  return timeout;
			    }
			  				
			  }
			  //Experimental
						
									  
		
			}		
		}
	}
//	else
//	  printf("Level just changed. Not going up\n");
  }
  
  
  if (SavedWorkCommandList.count)
	printf("waste %d\n", SavedWorkCommandList.count);
	
 // printf("Exit. Level=%d\n", level);
  cd[index].level=level;
  return timeout;
  
  
 

}

__global__ void pscan(PContinueData cd, int structsize, int maxlevel)
{
	int t;
	t=pscan2(cd, structsize, maxlevel);
//	printf("t=%d\n", t);
	
}

__global__ void initpscan(PContinueData cd, UINT_PTR address, int structsize, int maxlevel, UINT_PTR valuetofind)
{
	int index = blockIdx.x * blockDim.x + threadIdx.x;
	int i;
	
	cd[index].caller=(prcaller)malloc(sizeof(rcaller)*maxlevel);
	cd[index].offsets=(int *)malloc(sizeof(int)*maxlevel);	
	cd[index].level=-1;	
	
	for (i=0; i<maxlevel; i++)
	{
	  cd[index].caller[i].valueToFind=0;
	  cd[index].caller[i].startvalue=0;
	  cd[index].caller[i].stopvalue=0;
	  cd[index].caller[i].plist=NULL;
	  cd[index].caller[i].plistIndex=0;
	  
	  cd[index].offsets[i]=0;
	}
	
	if (index==1)
	{
	  printf("sizeof(rcaller)=%d  (%x)\n", sizeof(rcaller), sizeof(rcaller));	
	
	  printf("\n%d : %p %p\n", index, cd[index].caller, cd[index].offsets );
	  
	  printf("c1: %p\n", &cd[index].caller[0].valueToFind); 
	  printf("c2: %p\n", &cd[index].caller[1].valueToFind); 
	  printf("c3: %p\n", &cd[index].caller[2].valueToFind);	  
	}
	
	
	
	if (cd[index].offsets==NULL)
	{
		printf("Alloc offsets failed %d\n", index);
	}
	
	if (cd[index].caller==NULL)
	{
		printf("Alloc caller failed %d\n", index);
	}
	
	if (index==0)
	{
	    int i;
		printf("initpscan %d\n", index);
		
		SavedWorkCommandList.count=0;
		
		for (i=0; i<MAXCOMMANDLISTSIZE; i++)
		{
			SavedWorkCommandList.list[i].offsets=(int *)malloc(sizeof(int)*maxlevel);
		}
		
		//setup the initial work command
		SavedWorkCommandList.list[0].level=0;
		SavedWorkCommandList.list[0].valueToFind=address;		
		SavedWorkCommandList.count=1;
		
	}


}


int pointerscan(UINT_PTR address, int structsize, int maxlevel)
{
  PContinueData cd;
  int i=0;
  BOOL done=FALSE;  
  cudaError_t err;
  
  
  cudaMalloc(&cd, sizeof(ContinueData)*1024);
  initpscan<<<1,1024>>>(cd, address, structsize, maxlevel+2, address);

  cudaDeviceSynchronize(); 
  printf("CUDA error: %s\n", cudaGetErrorString(cudaGetLastError()));     


  //loop till all are done
  
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
  

  
   
    
  
  
  return 0;
}