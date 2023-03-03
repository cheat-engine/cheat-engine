#pragma once

#ifndef _WINDOWS
  #ifdef _APPLE_
    #include "macport.h"
  #else
    #include "linuxport.h"
  #endif //_APPLE_
#endif //_WINDOWS


class Pipe
{
private:
    CRITICAL_SECTION cs;
protected:
	HANDLE pipehandle;
public:
	void Read(PVOID buf, unsigned int count);
	void Write(PVOID buf, unsigned int count);
	BYTE ReadByte();
	WORD ReadWord();
	DWORD ReadDword();
	UINT64 ReadQword();
	void WriteByte(BYTE b);
	void WriteWord(WORD b);
	void WriteDword(DWORD b);
	void WriteQword(UINT64 b);

	void Lock();
	void Unlock();

	Pipe(void);
	~Pipe(void);
};
