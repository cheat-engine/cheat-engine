#ifndef NativeAPI_H_
#define NativeAPI_H_

#define MAX_HIT_COUNT  5000000
DWORD AOBScan(HANDLE hProcess, const char* pattern, const char* mask, uint64_t start, uint64_t end, int inc, int protection, uint64_t* match_addr);

#endif