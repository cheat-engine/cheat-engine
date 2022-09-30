#define NOREDIRECT
#ifdef _WINDOWS
#include <io.h>
#else
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#endif
#include "tcc.h"
#undef NOREDIRECT

typedef int(__stdcall *OPENFILE_OVERRIDE)(const char *filename, int OpenFlag);
typedef int(__stdcall *READFILE_OVERRIDE)(int FileHandle, void *DstBuf, int MaxCharCount);
typedef int(__stdcall *CLOSEFILE_OVERRIDE)(int FileHandle);


OPENFILE_OVERRIDE openfile_override;
READFILE_OVERRIDE readfile_override;
CLOSEFILE_OVERRIDE closefile_override;


LIBTCCAPI void tcc_install_filehook(OPENFILE_OVERRIDE newopenfile_override, READFILE_OVERRIDE newreadfile_override, CLOSEFILE_OVERRIDE newclosefile_override)
{
	openfile_override = newopenfile_override;
	readfile_override = newreadfile_override;
	closefile_override = newclosefile_override;	
}

int __cdecl redirectedopen(char const* _FileName, int _OpenFlag, ...)
{
	int r=-1;
	if (openfile_override)
		r = openfile_override(_FileName, _OpenFlag);

	if (r == -1)
		r = open(_FileName, _OpenFlag);


	return r;
}

int __cdecl redirectedclose(int _FileHandle)
{
	int r=-1;
	if (closefile_override)
		r = closefile_override(_FileHandle);

	if (r == -1)
		r = close(_FileHandle);

	return r;
}


int __cdecl redirectedread(int _FileHandle, void* _DstBuf, unsigned int _MaxCharCount)
{	
	int r = -1;
	if (readfile_override)
		r = readfile_override(_FileHandle, _DstBuf, _MaxCharCount);

	if (r==-1)
		r=read(_FileHandle, _DstBuf, _MaxCharCount);

	return r;
}
