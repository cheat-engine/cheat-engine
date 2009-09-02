#ifndef STEALTHEDIT_H
#define STEALTHEDIT_H

void stealthedit_initialize(void);
int stealthedit_initStealthEditHooksForCurrentCPU(void);

int stealthedit_RemoveCloakedSection(DWORD ProcessID, DWORD pagebase);
int stealthedit_AddCloakedSection(DWORD ProcessID, DWORD pagebase, DWORD relocatedpagebase, int size);


#endif