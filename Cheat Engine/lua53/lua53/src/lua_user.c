#ifdef WIN32   
  #include <windows.h>
#else
  #include <pthread.h>
#endif
  #include "lua.h"
  #include "lua_user.h"
  #include "lstate.h"

//CRITICAL_SECTION b;
//BOOL cscreated = FALSE;

  void LuaLockInitial(lua_State * L) 
  { 
	  if (!L->l_G->lock_init)	  
	  {
#ifdef WIN32
		  InitializeCriticalSection((CRITICAL_SECTION*)(&L->l_G->lock));
#else
 		  pthread_mutexattr_t attr;
          pthread_mutexattr_init(&attr);
		  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);

		  pthread_mutex_init(&L->l_G->lock, &attr);
#endif
		  L->l_G->lock_init = 1;
	  }
  }

  void LuaLockFinal(lua_State * L) /* Not called by Lua. */
  {
#ifdef WIN32 
	  DeleteCriticalSection((CRITICAL_SECTION*)(&L->l_G->lock));
#endif

  }

  void LuaLock(lua_State * L)
  {	  
	  if (!L->l_G->lock_init)
		  LuaLockInitial(L);

#ifdef WIN32
	  EnterCriticalSection((CRITICAL_SECTION*)(&L->l_G->lock));
#else
	  pthread_mutex_lock(&L->l_G->lock);
#endif
  }

  void LuaUnlock(lua_State * L)
  { 
	//  MessageBox(0, "LuaUnlock","LuaUnlock", MB_OK);
    /* Release control of mutex */
	  if (!L->l_G->lock_init)
	    LuaLockInitial(L);

#ifdef WIN32
	  LeaveCriticalSection((CRITICAL_SECTION*)(&L->l_G->lock));
#else
	  pthread_mutex_unlock(&L->l_G->lock);
#endif
  }
