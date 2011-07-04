//winuser.h
#ifndef __WINUSER_H
#define __WINUSER_H

#lib USER32.DLL
#define API __API
API int SetWindowTextA(int hw, char *text);
API int GetDesktopWindow();
API int  FindWindowA(char *,char *);
API int MoveWindow(int,int,int,int,int,int);
API int GetWindowTextA(int,char *,int);
API int SendMessageA(int,int,int,int);
#define WM_COMMAND 0x0111

#define setw SetWindowTextA
int hw;
void find()
{
 hw = FindWindowA(0,"squeeze.cpp - Notepad");
}

#define GW_HWNDFIRST        0
#define GW_HWNDLAST         1
#define GW_HWNDNEXT         2
#define GW_HWNDPREV         3
#define GW_OWNER            4
#define GW_CHILD            5

API int GetWindow(int hw, int cmd);

#lib
#endif

