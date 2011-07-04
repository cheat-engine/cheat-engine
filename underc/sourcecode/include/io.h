#ifndef _IO_H
#define _IO_H

#ifndef _TIME_T_DEFINED
typedef long time_t;             /* time value */
typedef unsigned long _fsize_t;
#define _TIME_T_DEFINED          /* avoid multiple def's of time_t */
#endif
struct _finddata_t {
    unsigned int  attrib;
    time_t      time_create;    /* -1 for FAT file systems */
    time_t      time_access;    /* -1 for FAT file systems */
    time_t      time_write;
    _fsize_t    size;
    char        name[260];
};

/* File attribute constants for _findfirst() */

#define _A_NORMAL       0x00    /* Normal file - No read/write restrictions */
#define _A_RDONLY       0x01    /* Read only file */
#define _A_HIDDEN       0x02    /* Hidden file */
#define _A_SYSTEM       0x04    /* System file */
#define _A_SUBDIR       0x10    /* Subdirectory */
#define _A_ARCH         0x20    /* Archive file */

#lib msvcrt40.dll
extern "C" {
  long _findfirst(char* path, _finddata_t* info);
  int  _findnext(long handle, _finddata_t* info);
  int  _findclose(long handle);
  int  _access(const char *file, int mode);
}
#lib

#endif