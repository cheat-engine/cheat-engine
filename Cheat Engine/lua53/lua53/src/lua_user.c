  #include <windows.h>
  #include "lua.h"
  #include "lua_user.h"
  #include "lstate.h"

CRITICAL_SECTION b;
BOOL cscreated = FALSE;

  void LuaLockInitial(lua_State * L) 
  { 
	  if (!cscreated)
	  {
		  InitializeCriticalSection(&b);
		  cscreated = TRUE;
	  }
  }

  void LuaLockFinal(lua_State * L) /* Not called by Lua. */
  { 
	  DeleteCriticalSection(&b);

  }

  void LuaLock(lua_State * L)
  {	  
	  if (!cscreated)
		  LuaLockInitial(L);

	  EnterCriticalSection(&b);	  
  }

  void LuaUnlock(lua_State * L)
  { 
	//  MessageBox(0, "LuaUnlock","LuaUnlock", MB_OK);
    /* Release control of mutex */

	  if (!cscreated)
		  LuaLockInitial(L);


	  LeaveCriticalSection(&b);
  }