#ifndef __WINUSER_H
#define __WINUSER_H
#include <windows.h>

struct RECT {
  int left;
  int top;
  int right;
  int bottom;
};
typedef RECT* LPRECT;
typedef int (*WNDENUMPROC) (void*,long);

#lib user32.dll
extern "C" {
 WINAPI HWND GetActiveWindow();
 WINAPI BOOL GetWindowRect(HWND,LPRECT);
 WINAPI BOOL MoveWindow(HWND,int,int,int,int,BOOL);
 WINAPI int  GetWindowTextA(HWND,LPSTR,int);
 WINAPI BOOL SetWindowTextA(HWND,LPSTR);
 WINAPI HWND FindWindowA(LPSTR,LPSTR);
 WINAPI BOOL EnumWindows(WNDENUMPROC callback, int lparam);
 WINAPI  int  SendMessageA(HWND,int,int,int);
 WINAPI BOOL InvalidateRect(HWND,LPRECT,BOOL);
}

#lib
#endif


