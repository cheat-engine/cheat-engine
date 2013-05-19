  #include <windows.h>
  #include "lua.h"
  #include "lua_user.h"

//todo: lock specific lua state, but then again, I usually only have one state...

  static struct {
    CRITICAL_SECTION LockSct;
	HANDLE e;
    BOOL Init;
	int tid;
	int lockcount;
  } Gl;

  void LuaLockInitial(lua_State * L) 
  { 
	 // MessageBox(0, "LuaLockInitial","LuaLockInitial", MB_OK);
    if (! Gl.Init) 
    {
      /* Create a mutex */
	//	Gl.e=CreateEvent(NULL,FALSE, TRUE,NULL);
      InitializeCriticalSection(&Gl.LockSct);
      Gl.Init = TRUE;
    }
  }

  void LuaLockFinal(lua_State * L) /* Not called by Lua. */
  { 

	MessageBox(0, "LuaLockFinal","LuaLockFinal", MB_OK);
    /* Destroy a mutex. */
    if (Gl.Init)
    {
	//	CloseHandle(Gl.e);
      DeleteCriticalSection(&Gl.LockSct);
      Gl.Init = FALSE;
    }
	
  }

  void LuaLock(lua_State * L)
  {
	  if (! Gl.Init) 
		  LuaLockInitial(L);


	//  MessageBox(0, "LuaLock","LuaLock", MB_OK);
    /* Wait for control of mutex */
    EnterCriticalSection(&Gl.LockSct);

	  
  }

  void LuaUnlock(lua_State * L)
  { 
	//  MessageBox(0, "LuaUnlock","LuaUnlock", MB_OK);
    /* Release control of mutex */
    LeaveCriticalSection(&Gl.LockSct);
  }