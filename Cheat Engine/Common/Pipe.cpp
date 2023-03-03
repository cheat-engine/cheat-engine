

#ifdef _WINDOWS
#include <Windows.h>
#else
  #ifdef _APPLE_
    #include "macport.h"
  #else
    #include "linuxport.h"
  #endif

#define ReadFile ReadFilePipeWrapper
#define WriteFile WriteFilePipeWrapper
#endif

#include "Pipe.h"

//superclass to make pipe handling easier to work with

Pipe::Pipe(void)
{
	pipehandle=0;
	InitializeCriticalSection(&cs);
}

Pipe::~Pipe(void)
{
	//check if someone forgot to clean it up
	if ((pipehandle!=0) && (pipehandle!=INVALID_HANDLE_VALUE))
	{
#ifdef _WINDOWS
		CloseHandle(pipehandle);
#else
        ClosePipe(pipehandle);
#endif
		pipehandle=0;
	}
	
    
}

void Pipe::Lock(void)
{
	EnterCriticalSection(&cs);
}

void Pipe::Unlock(void)
{
	LeaveCriticalSection(&cs);
}

void Pipe::Read(PVOID buf, unsigned int count)
{
	DWORD br;
	if (count==0) return;
	DWORD totalread = 0;

	while (totalread < count)
	{
		if (ReadFile(pipehandle, buf, count, &br, NULL) == FALSE)
			throw("Read Error");

		totalread += br;
		buf = &((char *)buf)[br];
	}
}

void Pipe::Write(PVOID buf, unsigned int count)
{
	DWORD bw;
	if (count==0) return;
	DWORD totalwritten = 0;

	while (totalwritten < count)
	{
		if (WriteFile(pipehandle, buf, count, &bw, NULL) == FALSE)
			throw("Write Error");

		totalwritten += bw;
		buf = &((char *)buf)[bw];
	}
}

BYTE Pipe::ReadByte()
{
	BYTE b;
	Read(&b, 1);
	return b;
}

WORD Pipe::ReadWord()
{
	WORD w;
	Read(&w, 2);
	return w;
}

DWORD Pipe::ReadDword()
{
	DWORD dw;
	Read(&dw, 4);
	return dw;
}

UINT64 Pipe::ReadQword()
{
	UINT64 q;
	Read(&q, 8);
	return q;
}

void Pipe::WriteByte(BYTE b)
{
	Write(&b, 1);
}

void Pipe::WriteWord(WORD w)
{
	Write(&w, 2);
}

void Pipe::WriteDword(DWORD dw)
{
	Write(&dw, 4);
}

void Pipe::WriteQword(UINT64 q)
{
	Write(&q, 8);
}
