#ifndef NEWARD_H_
#define NEWARD_H_


#include "common.h"

extern PARD fakeARD;
int ARDcount;

void sendARD(void);
void initARDcount(void);
void fakeARD_InsertRange(QWORD address, QWORD size, DWORD type);
int fakeARD_getConventionalMemory();
#endif
