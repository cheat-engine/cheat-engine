#ifndef CEWINDOWS_H
#define CEWINDOWS_H

#ifndef _WIN32
#error Only load windowslite.h in windows systems
#endif
typedef void* HANDLE;
typedef void* LPVOID;
typedef uint32_t DWORD;
typedef uint32_t *LPDWORD;
typedef void* LPOVERLAPPED;
typedef unsigned char* LPCSTR;
typedef void* LPSECURITY_ATTRIBUTES;
typedef int BOOL;
typedef int64_t LONG_PTR;
typedef uint64_t ULONG_PTR;

typedef struct _RTL_CRITICAL_SECTION {
    void* DebugInfo;
    long LockCount;
    long RecursionCount;
    HANDLE OwningThread;       
    HANDLE LockSemaphore;
    ULONG_PTR SpinCount; 
} RTL_CRITICAL_SECTION, *PRTL_CRITICAL_SECTION;
typedef RTL_CRITICAL_SECTION CRITICAL_SECTION;
typedef CRITICAL_SECTION* LPCRITICAL_SECTION;


void InitializeCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
void EnterCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
void LeaveCriticalSection(LPCRITICAL_SECTION lpCriticalSection);

HANDLE ReadFile(HANDLE  hFile, LPVOID lpBuffer,  DWORD nNumberOfBytesToRead,  LPDWORD lpNumberOfBytesRead,     LPOVERLAPPED lpOverlapped);
HANDLE WriteFile(HANDLE hFile, LPVOID lpBuffer,  DWORD nNumberOfBytesToWrite, LPDWORD lpNumberOfBytesWritten,  LPOVERLAPPED lpOverlapped);
HANDLE CreateFileA(LPCSTR lpFileName, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes, DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes, HANDLE hTemplateFile);
HANDLE CreateNamedPipeA(LPCSTR lpName, DWORD dwOpenMode, DWORD dwPipeMode, DWORD nMaxInstances, DWORD nOutBufferSize, DWORD nInBufferSize, DWORD nDefaultTimeOut, LPSECURITY_ATTRIBUTES lpSecurityAttributes);
BOOL ConnectNamedPipe(HANDLE hNamedPipe, LPOVERLAPPED lpOverlapped);
BOOL CloseHandle(HANDLE hObject);
BOOL DisconnectNamedPipe(HANDLE hNamedPipe);



    
#define PIPE_ACCESS_DUPLEX 0x00000003   
#define PIPE_TYPE_BYTE     0x00000000 
#define PIPE_READMODE_BYTE 0x00000000
#define PIPE_WAIT          0x00000000

#define TRUE 1
#define FALSE 0
#define INFINITE           0xFFFFFFFF

#define GENERIC_READ 0x80000000
#define GENERIC_WRITE 0x40000000
#define OPEN_EXISTING 3

#define INVALID_HANDLE_VALUE ((HANDLE)(LONG_PTR)-1)

#endif