#include "CMemStream.h"

void CMemStream::Read(PVOID buf, unsigned int count)
{
	if ((position + count) > size)
		throw ("CMemStream read beyond boundary");

	memcpy(buf, &memory[position], count);
	position += count;
}

void CMemStream::Write(PVOID buf, unsigned int count)
{
	while ((position + count) > memorysize)
	{			
		if (memorysize > 2*1024 * 1024)
			growTo(memorysize + 2*1024 * 1024);
		else
			growTo(memorysize * 2);
	}

	memcpy(&memory[position], buf, count);
	position += count;
	if (position > size)
		size = position;
}

BYTE CMemStream::ReadByte()
{
	BYTE r;
	Read(&r, sizeof(r));

	return r;
}

WORD CMemStream::ReadWord()
{
	WORD r;
	Read(&r, sizeof(r));

	return r;
}

DWORD CMemStream::ReadDword()
{
	DWORD r;
	Read(&r, sizeof(r));

	return r;
}

UINT64 CMemStream::ReadQword()
{
	UINT64 r;
	Read(&r, sizeof(r));

	return r;
}

void CMemStream::WriteByte(BYTE b)
{	
	Write(&b, sizeof(b));
}

void CMemStream::WriteWord(WORD w)
{
	Write(&w, sizeof(w));
}

void CMemStream::WriteDword(DWORD d)
{
	Write(&d, sizeof(d));

}

void CMemStream::WriteQword(UINT64 q)
{
	Write(&q, sizeof(q));
}

void* CMemStream::GetMemory()
{
	return (void*)memory;
}

unsigned int CMemStream::GetSize()
{
	return size;
}

unsigned int CMemStream::GetPosition()
{
	return position;
}

unsigned int CMemStream::SetPosition(unsigned int newpos)
{
	position = newpos;
	if (position > size)
	{
		size = position;
		if (size > memorysize)
			growTo(size);
	}

	return position;
}

void CMemStream::Clear()
{
	size = 0;
	position = 0;
}

void CMemStream::growTo(unsigned int newsize)
{
	if (newsize > memorysize)
	{
		memory = (char*)realloc(memory, newsize);
		memorysize = newsize;
	}
}

CMemStream::CMemStream()
{	
	size = 0;
	position = 0;
	memory = (char*)malloc(4096);
	memorysize = 4096;
}


CMemStream::~CMemStream()
{
	if (memory)
		free(memory);
}
