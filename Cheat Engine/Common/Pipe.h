#pragma once

class Pipe
{
protected:
	HANDLE pipehandle;
	void Read(PVOID buf, int count);
	void Write(PVOID buf, int count);
	BYTE ReadByte();
	WORD ReadWord();
	DWORD ReadDword();
	UINT64 ReadQword();
	void WriteByte(BYTE b);
	void WriteWord(WORD b);
	void WriteDword(DWORD b);
	void WriteQword(UINT64 b);
public:
	Pipe(void);
	~Pipe(void);
};
