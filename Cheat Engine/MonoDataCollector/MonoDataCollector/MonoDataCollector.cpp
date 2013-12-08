#include "stdafx.h"
#include "pipeserver.h"


DWORD WINAPI DataCollectorEntry(LPVOID lpThreadParameter)
{
	CPipeServer *pw;
	pw=new CPipeServer();
	pw->Start();

	return 0;
}