#ifndef CELOG_H
#define CELOG_H

#include <stdarg.h>

#ifdef ANDROID
int __android_log_vprint(int prio, const char* tag, const char* fmt, __builtin_va_list ap)
    __attribute__((__format__(printf, 3, 0)));

#ifndef LOG_TAG
#define LOG_TAG "CELOG"
#endif

#define LOGD(fmt, args...) __android_log_vprint(3, LOG_TAG, fmt, ##args)
#endif

#ifdef _WIN32
void OutputDebugStringA(char* msg);
int vsnprintf(char *str, size_t size, const char *format, __builtin_va_list ap);

#endif


void debug_log(const char * format , ...)
{

  __builtin_va_list list;

  __builtin_va_start(list,format);
#ifndef _WIN64  
  LOGD(format,list);
#else
  char log[256];
  vsnprintf(log, 255, format, list);
  log[256]=0;
  OutputDebugStringA(log);  
#endif  
  __builtin_va_end(list);


  return;
}


#endif //CELOG_H