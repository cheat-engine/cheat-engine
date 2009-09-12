#ifndef STEALTHEDIT_H
#define STEALTHEDIT_H

void stealthedit_initialize(void);
int stealthedit_initStealthEditHooksForCurrentCPU(void);

int stealthedit_RemoveCloakedSection(DWORD ProcessID, UINT_PTR pagebase);
int stealthedit_AddCloakedSection(DWORD ProcessID, UINT_PTR pagebase, UINT_PTR relocatedpagebase, int size);


#endif