/* threads.cpp
 * Simple wrappers for Win32 threads, mutexes, etc.
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 */

#include <windows.h>
#include "threads.h"

static DWORD WINAPI ThreadFunction(LPVOID parm);

int KernelObject::wait()
{
 DWORD dw = WaitForSingleObject(m_handle, INFINITE);
 if (dw == WAIT_OBJECT_0) { // Mutex became signalled!
    return 1;
 }
 else throw("abandoned mutex???"); 
 return 0;
}

void *KernelObject::handle()
{
 return (void *)m_handle;
}

KernelObject::~KernelObject()
{
    if (m_own) CloseHandle(m_handle);
}

Thread::Thread(bool owner)
{
  DWORD thread_id;
  m_own = owner;
  if (m_own) {
    m_handle = CreateThread(NULL,0,ThreadFunction,
                  this,CREATE_SUSPENDED,&thread_id);
  } else
    m_handle = GetCurrentThread();
}


void  Thread::piority(Thread::Piority p)
{
//  int pr;
  /*
  switch(p) {
    case Thread::Lowest: pr = THREAD_PIORITY_LOWEST; break;
    case Thread::BelowNormal: pr = THREAD_PIORITY_BELOW_NORMAL; break;
    case Thread::Normal: pr = THREAD_PIORITY_NORMAL; break;
    case Thread::AboveNormal: pr = THREAD_PIORITY_ABOVE_NORMAL; break;
    case Thread::Highest:  pr = THREAD_PIORITY_HIGEST; break;
   }
   SetThreadPiority(m_handle,pr);
   */
}

void Thread::resume()
{
  ResumeThread(m_handle);
}

void Thread::suspend()
{
  SuspendThread(m_handle);
}

void Thread::sleep(int msec)
{
 Sleep(msec);
}

static long gTime;
// *add 0.9.5 Elapsed time - *NOTE* should be thread-specific!!
void Thread::start_time()
{
  gTime = GetTickCount();
}

long Thread::elapsed_time()
{
  return GetTickCount() - gTime;
}

__declspec(thread) bool mtLocal = true;

ulong Thread::local()
{ return mtLocal; }

void Thread::local(ulong n)
{ mtLocal = n ? true : false; }


void Thread::kill()
{
  TerminateThread(m_handle,0);
}

int Thread::execute()
{
 return 1;
}

static DWORD WINAPI ThreadFunction(LPVOID parm)
{
 PThread(parm)->execute(); 
 return 1;
}

//----------------------------- Mutex ---------------------------
Mutex::Mutex(char *name)
{
  m_handle = CreateMutex(NULL,FALSE,name);
  m_own = true;
}

int Mutex::release()
{
 ReleaseMutex(m_handle);
 return 1;
}

Lock::Lock()
{
 m_data = new CRITICAL_SECTION;
 InitializeCriticalSection((LPCRITICAL_SECTION)m_data);
}

Lock::~Lock()
{
 DeleteCriticalSection((LPCRITICAL_SECTION)m_data); 
 delete (LPCRITICAL_SECTION)m_data;
}

int tick_count()
{ return GetCurrentTime(); }

int win_exec(char *prog)
{
 int res = WinExec(prog, SW_SHOW);
 if (res > 1) return 1; 
 else return 0;
}

void kill_process(int retcode)
{
  ExitProcess(retcode);
}  

// *add 1.2.4

Event::Event(char *name)
{
    m_handle = CreateEvent(NULL,FALSE,FALSE,name);
    reset();
    m_own = true;
}

void Event::reset()
{
    ResetEvent(m_handle);
    m_set = false;
}

void Event::set()
{
    SetEvent(m_handle);
    m_set = true;
}

bool Event::is_set()
{
    return m_set;
}

