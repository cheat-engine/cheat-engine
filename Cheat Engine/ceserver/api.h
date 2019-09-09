/*
 * api.h
 *
 *  Created on: Jul 21, 2011
 *      Author: erich
 */

#ifndef API_H_
#define API_H_

#include <stdint.h>
#include <pthread.h>
#include <sys/queue.h>

#include "porthelp.h"
#include "context.h"

#include <sys/ptrace.h>

#ifdef __ANDROID__
  #include<android/log.h>
#endif
/*

#if defined(__arm__) || defined(__ANDROID__)
#include <linux/user.h>
#else
#include <sys/user.h>
#endif
*/

#ifdef HAS_LINUX_USER_H
  #include <linux/user.h>
#else
  #include <sys/user.h>
#endif



#define VQE_PAGEDONLY 1
#define VQE_DIRTYONLY 2
#define VQE_NOSHARED 4


typedef struct
{
  unsigned long long baseAddress;
  int moduleSize;
  char *moduleName;

} ModuleListEntry, *PModuleListEntry;

typedef struct
{
  int PID;
  char *ProcessName;

} ProcessListEntry, *PProcessListEntry;

#pragma pack(1)

typedef struct
{
  uint8_t num_brps;   //number of instruction breakpoints
  uint8_t num_wrps;   //number of data breakpoints
  uint8_t wp_len;     //max length of a data breakpoint
  uint8_t debug_arch; //debug architecture

} HBP_RESOURCE_INFO, *PHBP_RESOURCE_INFO;

#ifdef __arm__
/*  struct user_pt_regs
  {
    long regs[18];
  };

  struct user_hwdebug_state {
   __u32 dbg_info;
   struct {
   __u32 addr;
   __u32 ctrl;
   } dbg_regs[16];
  };

#define NT_ARM_HW_WATCH 0x403
#define PTRACE_GETREGSET 0x4204
#define PTRACE_SETREGSET 0x4205
*/
#endif


typedef struct {
  int debugevent;
  int64_t threadid;
  union
  {
    uint64_t address; //TRAP: Address that caused trap
    struct {
      uint8_t maxBreakpointCount; //Number of execute breakpoints this system supports at most
      uint8_t maxWatchpointCount;
      uint8_t maxSharedBreakpoints; //If the system uses the same kind of breakpoints for execute and watchpoints. 0 otherwise
    }; //CreateProcess
  };
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
  int ReferenceCount;
  int pid;
  int mapfd; //file descriptor for /proc/pid/maps
  char *path;
  char *maps;
  int mem;
  int hasLoadedExtension; //set to true if the ceserver extension has been loaded in this process
  int neverForceLoadExtension; //set to true if you don't want to force load the module (if it's loaded, use it, but don't use the injection method)
  pthread_mutex_t extensionMutex;
  int extensionFD; //socket to communicate with the target

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
  uint64_t baseaddress;
  uint64_t size;
  uint32_t protection;
  uint32_t type;
} RegionInfo, *PRegionInfo;
#pragma pack()

#if defined __i386__ || defined __x86_64__
typedef struct _regDR6
{
  union{
    uintptr_t value;
    struct {
      unsigned B0        :1;
      unsigned B1        :1;
      unsigned B2        :1;
      unsigned B3        :1;
      unsigned Reserved  :9;
      unsigned BD        :1;
      unsigned BS        :1;
      unsigned BT        :1;
    };
  };
} __attribute__((__packed__)) regDR6,*PregDR6;
#endif





void CloseHandle(HANDLE h);
BOOL Process32Next(HANDLE hSnapshot, PProcessListEntry processentry);
BOOL Process32First(HANDLE hSnapshot, PProcessListEntry processentry);
HANDLE CreateToolhelp32Snapshot(DWORD dwFlags, DWORD th32ProcessID);
HANDLE OpenProcess(DWORD pid);
int VirtualQueryEx(HANDLE hProcess, void *lpAddress, PRegionInfo rinfo, char *mapsline);
int VirtualQueryExFull(HANDLE hProcess, uint32_t flags, RegionInfo **rinfo, uint32_t *count);
int ReadProcessMemory(HANDLE hProcess, void *lpAddress, void *buffer, int size);
int WriteProcessMemory(HANDLE hProcess, void *lpAddress, void *buffer, int size);

int StartDebug(HANDLE hProcess);
int StopDebug(HANDLE hProcess);

int WaitForDebugEventNative(PProcessData p, PDebugEvent devent, int tid, int timeout);
int WaitForDebugEvent(HANDLE hProcess, PDebugEvent devent, int timeout);
int ContinueFromDebugEvent(HANDLE hProcess, int tid, int ignoresignal);
int GetDebugPort(HANDLE hProcess);

int SetBreakpoint(HANDLE hProcess, int tid, int debugreg, void *address, int bptype, int bpsize);
int RemoveBreakpoint(HANDLE hProcess, int tid, int debugreg, int wasWatchpoint);

int SuspendThread(HANDLE hProcess, int tid);
int ResumeThread(HANDLE hProcess, int tid);

int GetThreadContext(HANDLE hProcess, int tid, PCONTEXT Context, int type);
int SetThreadContext(HANDLE hProcess, int tid, PCONTEXT Context, int type);

PDebugEvent FindThreadDebugEventInQueue(PProcessData p, int tid);
void AddDebugEventToQueue(PProcessData p, PDebugEvent devent);
int RemoveThreadDebugEventFromQueue(PProcessData p, int tid);

void initAPI();

extern pthread_mutex_t debugsocketmutex;

#ifdef __ANDROID__
  #define LOG_TAG "CESERVER"
  #define LOGD(fmt, args...) __android_log_vprint(ANDROID_LOG_DEBUG, LOG_TAG, fmt, ##args)
#endif

int debug_log(const char * format , ...); 
long safe_ptrace(int request, pid_t pid, void * addr, void * data);
extern int MEMORY_SEARCH_OPTION;
extern int ATTACH_PID;
extern unsigned char SPECIFIED_ARCH;
#endif /* API_H_ */
