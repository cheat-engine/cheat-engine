/*
 * linuxport.h
 *
 *  Created on: Nov 5, 2022
 *      Author: eric
 */

#ifndef MONODATACOLLECTOR_LINUXPORT_H_
#define MONODATACOLLECTOR_LINUXPORT_H_


#include <cstdint>
#include <vector>
#include <string>
#include <iostream>
#include <sstream>
#include <pthread.h>
#include <stdlib.h>
#include <strings.h>
#include <stdio.h>
#include <dlfcn.h>



typedef int BOOL;
typedef intptr_t HANDLE;


typedef uint8_t BYTE;
typedef uint16_t WORD;
typedef uint32_t DWORD, *LPDWORD;
typedef uint32_t UINT32;
typedef uint64_t QWORD;
typedef uint64_t UINT64, *PUINT64;


typedef void* PVOID;
typedef void* LPVOID, *LPOVERLAPPED;


typedef intptr_t HINSTANCE;
typedef size_t SIZE_T;

typedef uintptr_t UINT_PTR, *PUINT_PTR;

typedef pthread_mutex_t CRITICAL_SECTION;

//__darwin_intptr_t

#define FALSE 0
#define TRUE 1
#define INVALID_HANDLE_VALUE (int)-1

#define WINAPI
#define _cdecl
#define __cdecl

BOOL TerminateThread(HANDLE hThread, DWORD dwExitCode);
int OutputDebugString(const char * format , ...);
#define OutputDebugStringA OutputDebugString

#define GetProcAddress dlsym

HANDLE CreateNamedPipe(char *name);
void ClosePipe(HANDLE h);

int GetCurrentProcessId();
int Sleep(int ms);

void InitializeCriticalSection(CRITICAL_SECTION *cs);
void EnterCriticalSection(CRITICAL_SECTION *cs);
void LeaveCriticalSection(CRITICAL_SECTION *cs);

BOOL ReadFilePipeWrapper(HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, LPDWORD lpNumberOfBytesRead, LPOVERLAPPED lpOverlapped);
BOOL WriteFilePipeWrapper(HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToWrite, LPDWORD lpNumberOfBytesWrite, LPOVERLAPPED lpOverlapped);


#define sprintf_s snprintf



#endif /* MONODATACOLLECTOR_LINUXPORT_H_ */
