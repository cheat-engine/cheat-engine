unit luaJit;

{$mode delphi}

{*
** LuaJIT -- a Just-In-Time Compiler for Lua. http://luajit.org/
**
** Copyright (C) 2005-2010 Mike Pall. All rights reserved.
**
** Permission is hereby granted, free of charge, to any person obtaining
** a copy of this software and associated documentation files (the
** "Software"), to deal in the Software without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Software, and to
** permit persons to whom the Software is furnished to do so, subject to
** the following conditions:
**
** The above copyright notice and this permission notice shall be
** included in all copies or substantial portions of the Software.
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**
** [ MIT license: http://www.opensource.org/licenses/mit-license.php ]
*}

interface

{$IFDEF windows}
uses
  windows, Classes, SysUtils, lua;

const LUAJIT_VERSION = 'LuaJIT 2.0.0-beta5';
const LUAJIT_VERSION_NUM = 20000; //Version 2.0.0 = 02.00.00.
//const LUAJIT_VERSION_SYM = luaJIT_version_2_0_0_beta5
const LUAJIT_COPYRIGHT = 'Copyright (C) 2005-2010 Mike Pall';
const LUAJIT_URL = 'http://luajit.org/';

// Modes for luaJIT_setmode.
const LUAJIT_MODE_MASK = $00ff;

const
  LUAJIT_MODE_ENGINE=0; // Set mode for whole JIT engine.
  LUAJIT_MODE_DEBUG=1;		// Set debug mode (idx = level).

  LUAJIT_MODE_FUNC=2;		// Change mode for a function.
  LUAJIT_MODE_ALLFUNC=3;		// Recurse into subroutine protos.
  LUAJIT_MODE_ALLSUBFUNC=4;	// Change only the subroutines.

  LUAJIT_MODE_TRACE=5;		// Flush a compiled trace.

  LUAJIT_MODE_WRAPCFUNC = $10;	// Set wrapper mode for C function calls.

  LUAJIT_MODE_MAX=$11;


// Flags or'ed in to the mode.
const LUAJIT_MODE_OFF = $0000;	 // Turn feature off.
const LUAJIT_MODE_ON = $0100;	 // Turn feature on.
const LUAJIT_MODE_FLUSH = $0200; // Flush JIT-compiled code.

//LuaJIT public C API.
function luaJIT_setmode(L: Plua_State; idx: integer; mode: integer): integer;
{$ENDIF}


implementation

{$IFDEF windows}
var
  _luaJIT_setmode : function(L: Plua_State; idx: integer; mode: integer): integer; cdecl;

function luaJIT_setmode(L: Plua_State; idx: integer; mode: integer): integer;
begin
  if assigned(_luaJIT_setmode) then
    result:=_luaJIT_setmode(L, idx, mode)
  else
    result:=0;
end;

procedure InitializeLuaJIT;
var h: THandle;
begin
  h:=LoadLibrary(LUA_LIB_NAME);
  if h<>0 then
    _luaJIT_setmode:=getProcAddress(h,'luaJIT_setmode');
end;


initialization
  InitializeLuaJIT;
{$ENDIF}




end.

