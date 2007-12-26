#ifndef NEWKERNEL_H
#define NEWKERNEL_H

#include "ntifs.h"

NTSTATUS makeKernelCopy(ULONG KernelBase, ULONG KernelSize);

void* KernelCopy;

#endif
