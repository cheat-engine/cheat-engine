/*
 * api.h
 *
 *  Created on: Jul 21, 2011
 *      Author: erich
 */

#ifndef API_H_
#define API_H_

#include "porthelp.h"


typedef struct
{
  int PID;
  char *ProcessName;

} ProcessListEntry, *PProcessListEntry;

typedef struct {
  int pid;
  int mapfd; //file descriptor for /proc/pid/maps
  char *path;
  char *maps;
  int mem;
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

void initAPI();

#endif /* API_H_ */
