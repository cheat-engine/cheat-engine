// UC_TIMER.H

#ifndef __UC_TIMER_H
#define __UC_TIMER_H

// import GetTickCount()  (no of msec since begining of session)
#lib kernel32.dll
extern "C" {
__API unsigned long GetTickCount();
}
#lib

class Timer {
  unsigned long m_start;
  char* m_msg;
public:
   Timer() { }
   void start(char* msg) { 
     m_msg = msg;
     m_start = GetTickCount();
   }
   void stop() {
     unsigned long elapsed = GetTickCount() - m_start;
     printf("%s: time %lf sec\n",m_msg,elapsed/1000.0);
   }
 };

#endif


  