  #include <windows.h>
  #include "lua.h"
  #include "lua_user.h"
  #include "lstate.h"

//CRITICAL_SECTION b;
//BOOL cscreated = FALSE;

  void LuaLockInitial(lua_State * L) 
  { 
	  if (!L->l_G->lock_init)	  
	  {
		  InitializeCriticalSection((CRITICAL_SECTION*)(&L->l_G->lock));
		  L->l_G->lock_init = 1;
	  }
  }

  void LuaLockFinal(lua_State * L) /* Not called by Lua. */
  { 
	  DeleteCriticalSection((CRITICAL_SECTION*)(&L->l_G->lock));

  }

  void LuaLock(lua_State * L)
  {	  
	  if (!L->l_G->lock_init)
		  LuaLockInitial(L);

	  EnterCriticalSection((CRITICAL_SECTION*)(&L->l_G->lock));
  }

  void LuaUnlock(lua_State * L)
  { 
	//  MessageBox(0, "LuaUnlock","LuaUnlock", MB_OK);
    /* Release control of mutex */
	  if (!L->l_G->lock_init)
	    LuaLockInitial(L);

	  LeaveCriticalSection((CRITICAL_SECTION*)(&L->l_G->lock));
  }