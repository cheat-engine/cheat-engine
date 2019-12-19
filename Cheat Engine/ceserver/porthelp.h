/*
 * porthelp.h
 *
 *  Created on: Jul 21, 2011
 *      Author: erich
 */

#ifndef PORTHELP_H_
#define PORTHELP_H_

#include <stdint.h>

typedef uint32_t HANDLE; //just an int, in case of a 32-bit ce version and a 64-bit linux version I can not give pointers, so use ID's for handles
typedef uint32_t DWORD;

#define TH32CS_SNAPPROCESS  0x2
#define TH32CS_SNAPMODULE   0x8

#define PAGE_NOACCESS 1
#define PAGE_READONLY 2
#define PAGE_READWRITE 4
#define PAGE_WRITECOPY 8
#define PAGE_EXECUTE 16
#define PAGE_EXECUTE_READ 32
#define PAGE_EXECUTE_READWRITE 64

#define MEM_MAPPED 262144
#define MEM_PRIVATE 131072

typedef enum {htEmpty=0, htProcesHandle, htThreadHandle, htTHSProcess, htTHSModule, htNativeThreadHandle} handleType; //The difference between ThreadHandle and NativeThreadHandle is that threadhandle is based on the processid of the thread, the NativeThreadHandle is in linux usually the pthread_t handle
typedef int BOOL;

typedef int (*HANDLESEARCHCALLBACK) (void *data, void *searchdata);

#define TRUE 1
#define FALSE 0


int CreateHandleFromPointer(void *p, handleType type);
void *GetPointerFromHandle(int handle);
handleType GetHandleType(int handle);
void RemoveHandle(int handle);
int SearchHandleList(int type, HANDLESEARCHCALLBACK cb, void *searchdata);

#endif /* PORTHELP_H_ */
