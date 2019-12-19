#include "StdAfx.h"
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
		CloseHandle(pipehandle);
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

void Pipe::Read(PVOID buf, int count)
{
	DWORD br;
	if (count==0) return;
	if (ReadFile(pipehandle, buf, count, &br, NULL)==FALSE)
		throw("Read Error");
}

void Pipe::Write(PVOID buf, int count)
{
	DWORD bw;
	if (count==0) return;
	if (WriteFile(pipehandle, buf, count, &bw, NULL)==FALSE)
		throw("Write Error");
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
