#ifndef CELOG_H
#define CELOG_H

#include <stdarg.h>

int __android_log_vprint(int prio, const char* tag, const char* fmt, __builtin_va_list ap)
    __attribute__((__format__(printf, 3, 0)));

#ifndef LOG_TAG
#define LOG_TAG "CELOG"
#endif

#define LOGD(fmt, args...) __android_log_vprint(3, LOG_TAG, fmt, ##args)


void debug_log(const char * format , ...)
{
#ifndef _WIN64
  __builtin_va_list list;

  __builtin_va_start(list,format);
  LOGD(format,list);
  __builtin_va_end(list);
#else
  #error NYI
#endif

  return;
}


#endif //CELOG_H