#ifndef CUDAPOINTERVALUELIST_H
#define CUDAPOINTERVALUELIST_H

#include <windows.h>
//#pragma pack(16)

typedef  struct
{
  DWORD moduleindex;
  int offset;
} TStaticData, *PStaticData;

typedef   struct
{
  UINT_PTR address;
  PStaticData staticdata;
} TPointerData, *PPointerData;

typedef   struct _PointerList
{
   // int maxsize;  //not needed for preloaded scandata (saves some space)
    //int expectedsize;
    int pos;
    PPointerData list;

    //Linked list
    UINT_PTR PointerValue;
    _PointerList *Previous;
    _PointerList *Next;
    
} TPointerList, *PPointerList;
 
typedef   struct _ReversePointerList
{
	union
	{
		PPointerList PointerList;
		_ReversePointerList *ReversePointerList;
	} u;
} TReversePointerList, *PReversePointerList;



extern __device__ PPointerList findPointerValue(UINT_PTR startvalue, UINT_PTR *stopvalue);
__global__ void findoraddpointervalue(unsigned char *bla, int max);
__global__ void generateLinkedList(void);
void setMaxLevel(int count);



#endif//CUDAPOINTERVALUELIST_H