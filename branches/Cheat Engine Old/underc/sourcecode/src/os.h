// os.h
// *ch 1.2.9 patch (argues with standard BeOS header)
#ifndef _UNDERC_OS_H
#define _UNDERC_OS_H

typedef void *Handle;
long get_file_time(const char *file);
Handle load_library(const char *name);
void free_library(Handle h);
void *get_proc_address(Handle h, const char *name);
Handle get_process_handle();

#endif

