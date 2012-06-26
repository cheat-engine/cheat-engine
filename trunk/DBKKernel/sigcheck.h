#ifndef SIGCHECK_H
#define SIGCHECK_H

NTSTATUS SecurityCheck(void);
NTSTATUS CheckSignatureOfFile(PUNICODE_STRING originalpath);
NTSTATUS CheckSignature(PVOID buffer, DWORD buffersize, PVOID sig, DWORD sigsize);

#endif