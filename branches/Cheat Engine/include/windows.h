// UnderC Development Project, 2001
#ifndef _WINDOWS_H
#define _WINDOWS_H
typedef unsigned long DWORD, *PDWORD, *LPDWORD;
typedef char *LPSTR;
typedef const char *LPCSTR;
typedef void *PVOID;
typedef void *LPCVOID;
typedef void *LPVOID;
typedef int BOOL;
typedef unsigned short WORD;
typedef void* HWND;
#define WINAPI __API

#include <winuser.h>

#endif
