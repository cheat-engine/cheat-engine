#ifndef _STRING_H
#define _STRING_H
#include <_shared_lib.h>
/* From Appendix B3, String Functions, of K&R2 */
/* Commented functions are implemented as built-ins */
#include <stddef.h>
extern "C" {
  //char *strcpy(char *, const char *);
  //char *strncpy(char *, const char *, size_t);
  //char *strcat(char *, const char *);
  char *strncat(char *, const char *, size_t);
  //int strcmp(const char *, const char *);
  int strncmp(const char *, const char *, size_t);
  //char *strchr(const char *, int);
  //char *strrchr(const char *, int);
  size_t strspn(const char *, const char *);
  size_t strcspn(const char *, const char *);
  char *strpbrk(const char *, const char *);
  //char *strstr(const char *, const char *);
  //size_t strlen(const char *);
  char *strerror(size_t);
  //char *strtok(char *, const char *);
  void memcpy(char *, const char *, size_t);
  void memmove(char *, const char *, size_t);
  int memcmp(const char *, const char *, size_t);
  void *memchr(const char *, int, size_t);
  void memset(char *, int, size_t);
}
#lib
#endif

