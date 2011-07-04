/* threads.h
 * Simple wrappers for Win32 threads, mutexes, etc.
 * Steve Donovan, 1999.
 */

#ifndef __THREADS_H
#define __THREADS_H

typedef void *Handle;
#ifndef NULL
#define NULL 0
#endif

int tick_count();
int win_exec(char *prog);
void kill_process(int retcode);

class KernelObject {
protected:
  Handle m_handle;
  bool m_own;
public:
  ~KernelObject();
  void *handle();
  int wait();
};

typedef unsigned long ulong;
  
class Thread: public KernelObject {
 
 public:
   enum Piority { Lowest, BelowNormal,Normal,AboveNormal,Highest };
   Thread(bool owner = true);
   void piority (Piority p);
   void resume();
   void suspend();
   void start_time();
   long elapsed_time();
   void kill();
   static void sleep(int msec);
   static ulong local();
   static void local(ulong);
   virtual int execute();
   virtual void cleanup() {}
};

typedef Thread *PThread;

class Mutex: public KernelObject {
  public:
   Mutex(char *name=NULL);
   int release();
};

// *add 1.2.4
class Event: public KernelObject {
    bool m_set;
public:
    Event(char *name=NULL);
    void reset();
    void set();
    bool is_set();
};

class Lock {
private:
  void *m_data;
public:
  Lock();
  ~Lock();
};


#endif
   

