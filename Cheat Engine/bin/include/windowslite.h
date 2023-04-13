#ifndef CEWINDOWS_H
#define CEWINDOWS_H

typedef void* HANDLE;
typedef void* LPVOID;
typedef uint32_t DWORD;
typedef uint32_t *LPDWORD;
typedef void* LPOVERLAPPED;
typedef unsigned char* LPCSTR;
typedef void* LPSECURITY_ATTRIBUTES;
typedef int BOOL;
typedef int64_t LONG_PTR;


HANDLE ReadFile(HANDLE  hFile, LPVOID lpBuffer,  DWORD nNumberOfBytesToRead,  LPDWORD lpNumberOfBytesRead,     LPOVERLAPPED lpOverlapped);
HANDLE WriteFile(HANDLE hFile, LPVOID lpBuffer,  DWORD nNumberOfBytesToWrite, LPDWORD lpNumberOfBytesWritten,  LPOVERLAPPED lpOverlapped);
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

#define INVALID_HANDLE_VALUE ((HANDLE)(LONG_PTR)-1)

#endif