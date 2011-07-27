/*
 * ceserver.h
 *
 *  Created on: Jul 18, 2011
 *      Author: erich
 */

#ifndef CESERVER_H_
#define CESERVER_H_

#include "porthelp.h"

#define CMD_GETVERSION 0
#define CMD_CLOSECONNECTION 1
#define CMD_TERMINATESERVER 2
#define CMD_OPENPROCESS 3
#define CMD_CREATETOOLHELP32SNAPSHOT 4
#define CMD_PROCESS32FIRST 5
#define CMD_PROCESS32NEXT 6
#define CMD_CLOSEHANDLE 7
#define CMD_VIRTUALQUERYEX 8
#define CMD_READPROCESSMEMORY 9
#define CMD_WRITEPROCESSMEMORY 10


//extern char *versionstring;

#pragma pack(1)
typedef struct {
	int version;
	unsigned char stringsize;
	//append the versionstring
} CeVersion, *PCeVersion;

typedef struct {
    DWORD dwFlags;
    DWORD th32ProcessID;
} CeCreateToolhelp32Snapshot, *PCeCreateToolhelp32Snapshot;

typedef struct {
    int result;
    int pid;
    int processnamesize;
    //processname
} CeProcessEntry, *PCeProcessEntry;

typedef struct {
  int handle;
  unsigned long long baseaddress;
} CeVirtualQueryExInput, *PCeVirtualQueryExInput;

typedef struct {
  int result;
  int protection;
  unsigned long long baseaddress;
  unsigned long long size;
} CeVirtualQueryExOutput, *PCeVirtualQueryExOutput;

typedef struct {
  int handle;
  unsigned long long address;
  int size;
} CeReadProcessMemoryInput, *PCeReadProcessMemoryInput;

typedef struct {
  int read;
} CeReadProcessMemoryOutput, *PCeReadProcessMemoryOutput;

typedef struct {
  int32_t handle;
  int64_t address;
  int32_t size;
} CeWriteProcessMemoryInput, *PCeWriteProcessMemoryInput;


typedef struct {
  int32_t written;
} CeWriteProcessMemoryOutput, *PCeWriteProcessMemoryOutput;


#pragma pack()


#endif /* CESERVER_H_ */
