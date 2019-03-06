/*
 * luahandler.h
 *
 *  Created on: Feb 28, 2019
 *      Author: eric
 */

#ifndef VMM_LUAHANDLER_H_
#define VMM_LUAHANDLER_H_

#include "lua/src/lua.h"

lua_State *initializeLua(void);
void enterLuaConsole(void);

#endif /* VMM_LUAHANDLER_H_ */
