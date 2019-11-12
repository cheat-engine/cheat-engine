/*
 * ceserver.h
 *
 *  Created on: Jul 18, 2011
 *      Author: erich
 */

#ifndef CESERVER_H_
#define CESERVER_H_

#include <stdint.h>
#include <sys/types.h>
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
#define CMD_LOADEXTENSION         25

#define CMD_ALLOC                   26
#define CMD_FREE                    27
#define CMD_CREATETHREAD            28
#define CMD_LOADMODULE              29
#define CMD_SPEEDHACK_SETSPEED      30

#define CMD_VIRTUALQUERYEXFULL      31
#define CMD_GETREGIONINFO           32

#define CMD_AOBSCAN					200

//just in case I ever get over 255 commands this value will be reserved for a secondary command list (FF 00 -  FF 01 - ... - FF FE - FF FF 01 - FF FF 02 - .....
#define CMD_COMMANDLIST2            255





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
  uint64_t baseaddress;
} CeVirtualQueryExInput, *PCeVirtualQueryExInput;

typedef struct {
  uint8_t result;
  uint32_t protection;
  uint32_t type;
  uint64_t baseaddress;
  uint64_t size;
} CeVirtualQueryExOutput, *PCeVirtualQueryExOutput;

typedef struct {
  int handle;
  uint8_t flags;
} CeVirtualQueryExFullInput, *PCeVirtualQueryExFullInput;

typedef struct {
  uint32_t protection;
  uint32_t type;
  uint64_t baseaddress;
  uint64_t size;
} CeVirtualQueryExFullOutput, *PCeVirtualQueryExFullOutput;

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
  int debugreg;
  uint64_t Address;
  int bptype;
  int bpsize;
} CeSetBreapointInput, *PCeSetBreakpointInput;


typedef struct {
  int result;
} CeSetBreapointOutput, *PCeSetBreakpointOutput;

typedef struct {
  HANDLE hProcess;
  uint32_t tid;
  uint32_t debugreg;
  uint32_t wasWatchpoint;
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

typedef struct {
  HANDLE hProcess;
  uint64_t preferedBase;
  uint32_t size;
} CeAllocInput, *PCeAllocInput;


typedef struct {
  uint64_t address; //0=fail
} CeAllocOutput, *PCeAllocOutput;

typedef struct {
  HANDLE hProcess;
  uint64_t address;
  uint32_t size;
} CeFreeInput, *PCeFreeInput;


typedef struct {
  uint32_t result;
} CeFreeOutput, *PCeFreeOutput;

typedef struct {
  HANDLE hProcess;
  uint64_t startaddress;
  uint64_t parameter;
} CeCreateThreadInput, *PCeCreateThreadInput;


typedef struct {
  HANDLE threadhandle;
} CeCreateThreadOutput, *PCeCreateThreadOutput;

typedef struct {
  HANDLE hProcess;
  uint32_t modulepathlength;
  //modulepath
} CeLoadModuleInput, *PCeLoadModuleInput;


typedef struct {
  uint32_t result;
} CeLoadModuleOutput, *PCeLoadModuleOutput;


typedef struct {
  HANDLE hProcess;
  float speed;
} CeSpeedhackSetSpeedInput, *PCeSpeedhackSetSpeedInput;


typedef struct {
  uint32_t result;
} CeSpeedhackSetSpeedOutput, *PCeSpeedhackSetSpeedOutput;

typedef struct {
	HANDLE hProcess;
	uint64_t start;
	uint64_t end;
	int inc;
	int protection;
	int scansize;
} CeAobScanInput, * PCeAobScanInput;
#pragma pack()

ssize_t sendall (int s, void *buf, size_t size, int flags);
ssize_t recvall (int s, void *buf, size_t size, int flags);
int DispatchCommand(int currentsocket, unsigned char command);
int CheckForAndDispatchCommand(int currentsocket);

#if BUILD_OPTION == 1
  #define SHARED_LIBRARY
#endif

#endif /* CESERVER_H_ */
