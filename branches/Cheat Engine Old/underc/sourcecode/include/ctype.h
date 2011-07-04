// UnderC Development Project, 2001
#ifndef __cctype_H
#define __cctype_H
#include <_shared_lib.h>
extern "C" {
  int isalpha(int);
  int isalnum(int);
  int isdigit(int);
  int isspace(int);
  int isprint(int);
  char toupper(int);
  char tolower(int);
  int ispunct(int);
  int isupper(int);
  int isxdigit(int);
  int islower(int);
  int iscntrl(int);
  int isgraph(int);
}
#include <_end_shared.h>
#endif
