  #include <windows.h>
  #include "lua.h"
  #include "lua_user.h"
  #include "lstate.h"

CRITICAL_SECTION b;

  void LuaLockInitial(lua_State * L) 
  { 

	  if (sizeof(b) != sizeof(L->lock))
	  {
		  MessageBoxA(0, "Not the same", "FUUUCK", MB_OK);
	  }
    if (!L->lock_init) 
    {
		InitializeCriticalSection((LPCRITICAL_SECTION)&L->lock);
		L->lock_init = 1;
    }
  }

  void LuaLockFinal(lua_State * L) /* Not called by Lua. */
  { 
    /* Destroy a mutex. */
	if (L->lock_init)
    {
      DeleteCriticalSection(&L->lock);
	  L->lock_init = 0;
    }
	
  }

  void LuaLock(lua_State * L)
  {	  
	  if (!L->lock_init)
		  LuaLockInitial(L);

	  EnterCriticalSection((LPCRITICAL_SECTION)&L->lock);	  
  }

  void LuaUnlock(lua_State * L)
  { 
	//  MessageBox(0, "LuaUnlock","LuaUnlock", MB_OK);
    /* Release control of mutex */
	  LeaveCriticalSection((LPCRITICAL_SECTION)&L->lock);
  }