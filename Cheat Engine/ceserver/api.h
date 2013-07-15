/*
 * api.h
 *
 *  Created on: Jul 21, 2011
 *      Author: erich
 */

#ifndef API_H_
#define API_H_

#include <pthread.h>
#include <sys/queue.h>

#include "porthelp.h"


typedef struct
{
  int PID;
  char *ProcessName;

} ProcessListEntry, *PProcessListEntry;

#pragma pack(1)
typedef struct {
  unsigned int debugevent;
  pthread_t threadid;
//other data
} DebugEvent, *PDebugEvent;

struct DebugEventQueueElement {
  TAILQ_ENTRY(DebugEventQueueElement) entries;
  DebugEvent de;
};

#pragma pack()

TAILQ_HEAD(debugEventQueueHead, DebugEventQueueElement);


typedef struct {
  int tid;
  int isPaused;
  int suspendCount;
  DebugEvent suspendedDevent; //debug event to be injected when resumed
} ThreadData, *PThreadData;

typedef struct {
  int pid;
  int mapfd; //file descriptor for /proc/pid/maps
  char *path;
  char *maps;
  int mem;
  int isDebugged; //if this is true no need to attach/detach constantly, BUT make sure the debugger thread does do it's job
  pthread_t debuggerThreadID;

  PThreadData threadlist;
  int threadlistmax;
  int threadlistpos;

  DebugEvent debuggedThreadEvent;

  int debuggerServer; //sockets for communicating with the debugger thread by local threads
  int debuggerClient;


  pthread_mutex_t debugEventQueueMutex; //probably not necessary as all queue operations are all done in the debuggerthread of the process

  struct debugEventQueueHead debugEventQueue;
} ProcessData, *PProcessData;



#pragma pack(1)
typedef struct {
  unsigned long long baseaddress;
  unsigned long long size;
  DWORD protection;
} RegionInfo, *PRegionInfo;
#pragma pack()





void CloseHandle(HANDLE h);
BOOL Process32Next(HANDLE hSnapshot, PProcessListEntry processentry);
BOOL Process32First(HANDLE hSnapshot, PProcessListEntry processentry);
HANDLE CreateToolhelp32Snapshot(DWORD dwFlags, DWORD th32ProcessID);
HANDLE OpenProcess(DWORD pid);
int VirtualQueryEx(HANDLE hProcess, void *lpAddress, PRegionInfo rinfo);
int ReadProcessMemory(HANDLE hProcess, void *lpAddress, void *buffer, int size);
int WriteProcessMemory(HANDLE hProcess, void *lpAddress, void *buffer, int size);

int StartDebug(HANDLE hProcess);
int StopDebug(HANDLE hProcess);

int WaitForDebugEventNative(PProcessData p, PDebugEvent devent, int tid, int timeout);
int WaitForDebugEvent(HANDLE hProcess, PDebugEvent devent, int timeout);
int ContinueFromDebugEvent(HANDLE hProcess, int tid, int ignoresignal);
int GetDebugPort(HANDLE hProcess);

int SetBreakpoint(HANDLE hProcess, int tid, void *Address, int bptype, int bpsize);

int SuspendThread(HANDLE hProcess, int tid);
int ResumeThread(HANDLE hProcess, int tid);

PDebugEvent FindThreadDebugEventInQueue(PProcessData p, int tid);
void AddDebugEventToQueue(PProcessData p, PDebugEvent devent);
int RemoveThreadDebugEventFromQueue(PProcessData p, int tid);

void initAPI();

#endif /* API_H_ */
