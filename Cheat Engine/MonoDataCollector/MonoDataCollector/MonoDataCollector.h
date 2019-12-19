#pragma once

extern HANDLE DataCollectorThread;
extern HANDLE SuicideThread;
extern HINSTANCE g_hInstance;
DWORD WINAPI DataCollectorEntry(LPVOID lpThreadParameter);
DWORD WINAPI SuicideCheck(LPVOID lpThreadParameter);
