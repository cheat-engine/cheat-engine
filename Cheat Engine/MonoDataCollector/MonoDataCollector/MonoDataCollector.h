#pragma once

extern HANDLE DataCollectorThread;
extern HANDLE SuicideThread;
extern HINSTANCE g_hInstance;
DWORD WINAPI DataCollectorEntry(LPVOID lpThreadParameter);
DWORD WINAPI SuicideCheck(LPVOID lpThreadParameter);

#ifdef __APPLE__
void MacPortEntryPoint(void *param);
#endif

#ifdef __linux__
void LinuxPortEntryPoint(void *param);
#endif
