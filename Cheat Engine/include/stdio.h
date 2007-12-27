/* 
 this file is deliberately empty (the stdio functions are built-in)
 * but FILE is often needed...
*/
#ifndef __stdio_h
#define __stdio_h
typedef int FILE;
FILE *stdin = _get_std_stream(1);
FILE *stdout = _get_std_stream(2);
FILE *stderr = _get_std_stream(3);

/* Seek method constants */

#define SEEK_CUR    1
#define SEEK_END    2
#define SEEK_SET    0

#endif

