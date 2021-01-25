//+---------------------------------------------------------------------------
//
//  dll.c - Windows DLL example - dynamically linked part
//

#include <windows.h>

__declspec(dllexport) const char *hello_data = "(not set)";

__declspec(dllexport) void hello_func (void)
{
    MessageBox (0, hello_data, "From DLL", MB_ICONINFORMATION);
}
