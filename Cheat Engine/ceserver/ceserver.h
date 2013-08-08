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
#define CMD_STARTDEBUG 11
#define CMD_STOPDEBUG 12
#define CMD_WAITFORDEBUGEVENT 13
#define CMD_CONTINUEFROMDEBUGEVENT 14
#define CMD_SETBREAKPOINT 15
#define CMD_REMOVEBREAKPOINT 16
#define CMD_SUSPENDTHREAD 17
#define CMD_RESUMETHREAD 18
#define CMD_GETTHREADCONTEXT 19
#define CMD_SETTHREADCONTEXT 20
#define CMD_GETARCHITECTURE 21
#define CMD_MODULE32FIRST 22
#define CMD_MODULE32NEXT 23

#define CMD_GETSYMBOLLISTFROMFILE 24



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
    int result;
    int64_t modulebase;
    int modulesize;
    int modulenamesize;
    //modulename

} CeModuleEntry, *PCeModuleEntry;

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
  uint32_t handle;
  uint64_t address;
  uint32_t size;
  uint8_t  compress;
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


typedef struct {
  HANDLE hProcess;
  int tid;
  unsigned long long Address;
  int bptype;
  int bpsize;
} CeSetBreapointInput, *PCeSetBreakpointInput;


typedef struct {
  int result;
} CeSetBreapointOutput, *PCeSetBreakpointOutput;

typedef struct {
  HANDLE hProcess;
  int tid;
} CeRemoveBreapointInput, *PCeRemoveBreakpointInput;


typedef struct {
  int result;
} CeRemoveBreapointOutput, *PCeRemoveBreakpointOutput;

typedef struct {
  HANDLE hProcess;
  int tid;
} CeSuspendThreadInput, *PCeSuspendThreadInput;


typedef struct {
  int result;
} CeSuspendThreadOutput, *PCeSuspendThreadOutput;

typedef struct {
  HANDLE hProcess;
  int tid;
} CeResumeThreadInput, *PCeResumeThreadInput;


typedef struct {
  int result;
} CeResumeThreadOutput, *PCeResumeThreadOutput;


#pragma pack()

ssize_t sendall (int s, void *buf, size_t size, int flags);
ssize_t recvall (int s, void *buf, size_t size, int flags);
int DispatchCommand(int currentsocket, unsigned char command);
int CheckForAndDispatchCommand(int currentsocket);

#endif /* CESERVER_H_ */
