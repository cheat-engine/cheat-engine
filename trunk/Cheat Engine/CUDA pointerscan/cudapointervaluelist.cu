#include <windows.h>
#include <stdio.h>


#include "cudapointervaluelist.cuh"
#include "bigalloc.cuh"
 


__device__ PReversePointerList Level0List=NULL;
__device__ int maxlevel;

__device__ PPointerList firstPointerValue=NULL;
__device__ PPointerList lastPointerValue=NULL;
    
__device__ PPointerList findClosestPointer(PReversePointerList addresslist, int entrynr, int level, UINT_PTR maxvalue)
/*
The pointer was not found exactly, but we are in an addresslist that has been allocated, so something is filled in at least
*/
{
	int i;
	PPointerList result=NULL;
	
	for (i=entrynr+1; i<=0xF; i++)
	{		
		if (addresslist[i].u.ReversePointerList)
		{
		  if (level==maxlevel)
		  {
			result=addresslist[i].u.PointerList;
			while ((result) && (result->PointerValue>maxvalue)) //should only run one time
			  result=result->Previous;

			if (result==NULL)
			  result=firstPointerValue;

			return result;
		  }
		  else //dig deeper
		  {
			result=findClosestPointer(addresslist[i].u.ReversePointerList, -1, level+1, maxvalue); //so it will be found by the next top scan
			if (result) 
			  return result;
		  }
		}	
	
	}


	//nothing at the top, try the bottom
	for (i=entrynr-1; i>=0; i--)
	{
	    if (addresslist[i].u.ReversePointerList)
		{
		  if (level==maxlevel) 
		  {
			result=addresslist[i].u.PointerList;
			while ((result) && (result->PointerValue>maxvalue)) //should never happen
			  result=result->Previous;

			if (result==NULL) 
			  result=firstPointerValue;

			return result;				
			
		  }
		  else //dig deeper
		  {
			result=findClosestPointer(addresslist[i].u.ReversePointerList,0x10, level+1, maxvalue); //F downto 0
			if (result) 
			  return result;
		  }
		}
	}
	return result;
	

}    

__device__ PPointerList findPointerValue(UINT_PTR startvalue, UINT_PTR *stopvalue)
/*
find a node that falls in the region of stopvalue and startvalue
*/
{
  PPointerList result=NULL;
  int level;
  PReversePointerList currentarray;
  int entrynr;  
  UINT_PTR _stopvalue; 

  _stopvalue=*stopvalue;
  currentarray=Level0List;

//  printf("findPointerValue for %x\n", (unsigned int)startvalue);
//  printf("maxlevel is %d\n", (unsigned int)maxlevel);

  for (level=0; level<=maxlevel; level++)
  {
    entrynr=((UINT64)_stopvalue >> (UINT64)(((maxlevel-level)*4))) & 0xf;
    

    if (currentarray[entrynr].u.ReversePointerList==NULL) //not found
    {
      result=findClosestPointer(currentarray, entrynr, level, _stopvalue);
      break;
    }
    else
    {
      if (level==maxlevel)
      {
        result=currentarray[entrynr].u.PointerList;
        break;
      }
    }
    currentarray=currentarray[entrynr].u.ReversePointerList;
  }

  *stopvalue=result->PointerValue;

  //clean up bad results
  
 // printf("result=%p\n", result);
 // printf("result->PointerValue=%x\n", (unsigned int)result->PointerValue);
  
 // printf("result->Next->PointerValue=%x\n", (unsigned int)result->Next->PointerValue);
  
  
  
  if (result->PointerValue<startvalue)
    result=NULL;
    
  return result;
}

 
    
    
__device__ void fillList(PReversePointerList addresslist, int level, PPointerList *prev)
/*
Fills in the linked list of the reverse pointer list
*/
{
  int i;
  if (level==maxlevel)
  {
    for (i=0; i<=0xf; i++)
    {
      if (addresslist[i].u.PointerList)
      {
        if (*prev)
          (*prev)->Next=addresslist[i].u.PointerList;
        else
          firstPointerValue=addresslist[i].u.PointerList;

        addresslist[i].u.PointerList->Previous=*prev;
        *prev=addresslist[i].u.PointerList;
      }
    }
  }
  else
  {
    for (i=0; i<=0xf; i++)
    {    
      if (addresslist[i].u.ReversePointerList)
        fillList(addresslist[i].u.ReversePointerList,level+1, prev);
    }
  }

}

__global__ void generateLinkedList(void)
{
  lastPointerValue=NULL;
  fillList(Level0List,0,&lastPointerValue); 
}

__global__ void findoraddpointervalue(unsigned char *bla, int max)
/*
Go through the data and add the pointervalue. (port from ce's pascal source, with some improvements since no dynamic loading is necesary)
*/
{
  int i;
  int loopnr;
  int pd=0;
  UINT64 pointervalue;
  DWORD pointercount;
  int level, entrynr, size;
  PReversePointerList currentarray, temp;
  PPointerList plist;

  if (Level0List==NULL) //first time init
  {
    Level0List=(PReversePointerList)malloc(16*sizeof(PReversePointerList));
    memset(Level0List, 0, 16*sizeof(PReversePointerList));
  }
 
  // printf("this one will crash\n");
  loopnr=1;
   
  while (pd<max)
  {
	 // printf("loopnr %d\n", loopnr);

	  memcpy(&pointervalue, &bla[pd], sizeof(pointervalue));
	  pd+=sizeof(pointervalue);
	  
	 // printf("pointervalue=%x\n", (unsigned int)pointervalue);
		
		   	 
	  currentarray=Level0List;
	  level=0;
	  

	  while (level<maxlevel)
	  {
	 
		//add the path if needed
		entrynr=((UINT64)pointervalue >> (UINT64)(((maxlevel-level)*4))) & 0xf;
	    
	     
		if (currentarray[entrynr].u.ReversePointerList==NULL) //allocate
		{   
		  size=16*sizeof(PReversePointerList);
		 
		  temp=(PReversePointerList)balloc(size);   
		 // memset(temp, 0, size);
	        
		  currentarray[entrynr].u.ReversePointerList=temp;      
		  
		}
		currentarray=currentarray[entrynr].u.ReversePointerList;
	    
		level++;
	  }
	  
	
	
	 
	  entrynr=((UINT64)pointervalue >> (UINT64)(((maxlevel-level)*4))) & 0xf;
	  plist=currentarray[entrynr].u.PointerList;   
	  
	  if (plist==NULL) //allocate one 
	  {
	 
		currentarray[entrynr].u.PointerList=(TPointerList*)balloc(sizeof(TPointerList));
		plist=currentarray[entrynr].u.PointerList;
		plist->PointerValue=pointervalue;

		plist->list=NULL;
		plist->pos=0;
		
	  }

	  //use the current plist
	  
	  
	  memcpy(&pointercount, &bla[pd], sizeof(pointercount));
	  pd+=sizeof(pointercount);
	  	  
   
	   
	  plist->pos=pointercount;
	  plist->list=(TPointerData *)balloc(sizeof(TPointerData)*pointercount); 
	  
	  //printf("plist->list=%p\n", plist->list);
	  
 
	  for (i=0; i<pointercount; i++)
	  {
	 
		UINT64 address;	
		memcpy(&address, &bla[pd], sizeof(address));
		pd+=sizeof(address);
		plist->list[i].address=address;
			

		
		if (bla[pd]==1)
		{
		  DWORD moduleindex;	  
		  DWORD offset;
		  pd+=1;
		  
		  memcpy(&moduleindex, &bla[pd], sizeof(moduleindex));
		  pd+=sizeof(moduleindex);
		  
		  memcpy(&offset, &bla[pd], sizeof(offset));
		  pd+=sizeof(offset);  
		  
	
		  plist->list[i].staticdata=(TStaticData *)balloc(sizeof(TStaticData));
		  plist->list[i].staticdata->moduleindex=moduleindex;
		  plist->list[i].staticdata->offset=offset;	  	  
		  
		  //printf("plist->list[i].staticdata=%p\n", plist->list[i].staticdata);
		 
		}
		else
		{
		  pd+=1;
		  plist->list[i].staticdata=NULL;
	
		}
	  } //for 

	  
	  
	 // printf("pd=%d  max=%d\n", pd, max);
	  loopnr++;
  }
 
}


void setMaxLevel(int count)
{	
	cudaMemcpyToSymbol(maxlevel, &count, sizeof(count));	
}