#ifndef SIGCHECK_H
#define SIGCHECK_H

NTSTATUS SecurityCheck(void);
NTSTATUS CheckSignatureOfFile(PUNICODE_STRING originalpath, BOOL isProcess);
NTSTATUS CheckSignature(PVOID buffer, DWORD buffersize, PVOID sig, DWORD sigsize);

#endif