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

#endif

#ifdef __APPLE__
void openlog(char *ident, int logopt, int facility);
int setlogmask(int mskptr);
//int vsyslog(int priority, char* message, __builtin_va_list args);
void syslog(int priority, const char *message, ...);

#define LOG_USER    1 << 3
#define LOG_DEBUG   7
#define LOG_UPTO(pri) ((1 << (pri+1))-1)
#define LOG_NOTICE  5

int openedlog;
#endif

int vsnprintf(char *str, int size,  const char restrict *format, __builtin_va_list ap);


void debug_log(const char restrict * format , ...)
{

  __builtin_va_list list;

  __builtin_va_start(list,format);
#ifdef ANDROID
  LOGD(format,list);
#elif __APPLE__
  if (!openedlog)
  {
      openlog("CELOG",0,LOG_USER);
      setlogmask(LOG_UPTO(LOG_DEBUG));
      openedlog=1;
  }
    
  char log[256];
  vsnprintf(log, 255, format, list);
  log[255]=0;
    
  syslog(LOG_NOTICE, log); //vsyslog didn't work as planned
    
#elif _WIN32
  char log[256];
  vsnprintf(log, 255, format, list);
  log[256]=0;
  OutputDebugStringA(log);  
#endif  
  __builtin_va_end(list);


  return;
}


#endif //CELOG_H
