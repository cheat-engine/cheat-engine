#ifndef THREADS_H
#define THREADS_H


#include <ntifs.h>
#include <windef.h>
#include "DBKFunc.h"


void Ignore(PKAPC Apc, PKNORMAL_ROUTINE NormalRoutine, PVOID NormalContext, PVOID SystemArgument1, PVOID SystemArgument2);
void SuspendThreadAPCRoutine(PVOID arg1, PVOID arg2, PVOID arg3);

void DBKSuspendThread(ULONG ThreadID);
void DBKResumeThread(ULONG ThreadID);

void DBKResumeProcess(ULONG ProcessID);
void DBKSuspendProcess(ULONG ProcessID);



//NTSTATUS NTAPI DBKGetContextThread(IN PETHREAD Thread, IN OUT PCONTEXT ThreadContext);

#endif