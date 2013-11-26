#ifndef CUDAPOINTERVALUELIST_H
#define CUDAPOINTERVALUELIST_H

#include <windows.h>
#pragma pack(16)

typedef __declspec(align(16)) struct
{
  DWORD moduleindex;
  int offset;
} TStaticData, *PStaticData;

typedef __declspec(align(16)) struct
{
  UINT_PTR address;
  PStaticData staticdata;
} TPointerData, *PPointerData;

typedef __declspec(align(16)) struct _PointerList
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
 
typedef __declspec(align(16)) struct _ReversePointerList
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