#pragma once

//I couldn't quickly find a TMemoryStream equivalent in tyhe c++ stds library that wasn't marked as deprecated, so here's my version of the wheel
#include <stdlib.h>

#ifdef _WINDOWS
#include <Windows.h>
#endif

#ifndef _WINDOWS
#ifdef _APPLE_
#include "macport.h"
#else
#include "linuxport.h"
#endif //_APPLE_
#endif //_WINDOWS

class CMemStream
{
	
private:
	char *memory;
	unsigned int memorysize;
	unsigned int position;
	unsigned int size;
	void growTo(unsigned int newsize);
public:
	void* GetMemory();
	unsigned int GetSize();
	unsigned int GetPosition();
	unsigned int SetPosition(unsigned int newpos);
	void Clear();

	void Read(PVOID buf, unsigned int count);
	void Write(PVOID buf, unsigned int count);
	BYTE ReadByte();
	WORD ReadWord();
	DWORD ReadDword();
	UINT64 ReadQword();
	void WriteByte(BYTE b);
	void WriteWord(WORD w);
	void WriteDword(DWORD d);
	void WriteQword(UINT64 q);

	CMemStream();
	~CMemStream();
};

