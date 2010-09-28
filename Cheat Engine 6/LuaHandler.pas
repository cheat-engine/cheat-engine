unit LuaHandler;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, lua, lualib, lauxlib, syncobjs, cefuncproc, newkernelhandler;

var
  LuaVM: Plua_State;
  LuaCS: Tcriticalsection;


function CheckIfConditionIsMetContext(context: PContext; script: string): boolean;
procedure LUA_SetCurrentContextState(context: PContext);
procedure InitializeLuaScripts;

implementation

procedure InitializeLuaScripts;
begin

end;

procedure LUA_SetCurrentContextState(context: PContext);
begin
  LuaCS.Enter;
  try
    {$ifdef cpu64}
    lua_pushinteger(luavm, context.{$ifdef cpu64}Rax{$else}eax{$endif});
    lua_setglobal(luavm, 'RAX');
    {$endif}
    lua_pushinteger(luavm, context.{$ifdef cpu64}Rax{$else}eax{$endif} and $ffffffff);
    lua_setglobal(luavm, 'EAX');

    {$ifdef cpu64}
    lua_pushinteger(luavm, context.{$ifdef cpu64}Rbx{$else}ebx{$endif});
    lua_setglobal(luavm, 'RBX');
    {$endif}
    lua_pushinteger(luavm, context.{$ifdef cpu64}Rbx{$else}ebx{$endif} and $ffffffff);
    lua_setglobal(luavm, 'EBX');


    {$ifdef cpu64}
    lua_pushinteger(luavm, context.{$ifdef cpu64}Rcx{$else}ecx{$endif});
    lua_setglobal(luavm, 'RCX');
    {$endif}
    lua_pushinteger(luavm, context.{$ifdef cpu64}Rcx{$else}ecx{$endif} and $ffffffff);
    lua_setglobal(luavm, 'ECX');

    {$ifdef cpu64}
    lua_pushinteger(luavm, context.{$ifdef cpu64}Rdx{$else}edx{$endif});
    lua_setglobal(luavm, 'RDX');
    {$endif}
    lua_pushinteger(luavm, context.{$ifdef cpu64}Rdx{$else}edx{$endif} and $ffffffff);
    lua_setglobal(luavm, 'EDX');


    {$ifdef cpu64}
    lua_pushinteger(luavm, context.{$ifdef cpu64}Rsi{$else}esi{$endif});
    lua_setglobal(luavm, 'RSI');
    {$endif}
    lua_pushinteger(luavm, context.{$ifdef cpu64}Rsi{$else}esi{$endif} and $ffffffff);
    lua_setglobal(luavm, 'ESI');


    {$ifdef cpu64}
    lua_pushinteger(luavm, context.{$ifdef cpu64}Rdi{$else}edi{$endif});
    lua_setglobal(luavm, 'RDI');
    {$endif}
    lua_pushinteger(luavm, context.{$ifdef cpu64}Rdi{$else}edi{$endif} and $ffffffff);
    lua_setglobal(luavm, 'EDI');


    {$ifdef cpu64}
    lua_pushinteger(luavm, context.{$ifdef cpu64}Rbp{$else}ebp{$endif});
    lua_setglobal(luavm, 'RBP');
    {$endif}
    lua_pushinteger(luavm, context.{$ifdef cpu64}RBP{$else}eBP{$endif} and $ffffffff);
    lua_setglobal(luavm, 'EBP');


    {$ifdef cpu64}
    lua_pushinteger(luavm, context.{$ifdef cpu64}RSP{$else}eSP{$endif});
    lua_setglobal(luavm, 'RSP');
    {$endif}
    lua_pushinteger(luavm, context.{$ifdef cpu64}RSP{$else}eSP{$endif} and $ffffffff);
    lua_setglobal(luavm, 'ESP');

    {$ifdef cpu64}
    lua_pushinteger(luavm, context.{$ifdef cpu64}RIP{$else}eIP{$endif});
    lua_setglobal(luavm, 'RIP');
    {$endif}
    lua_pushinteger(luavm, context.{$ifdef cpu64}RIP{$else}eIP{$endif} and $ffffffff);
    lua_setglobal(luavm, 'EIP');

    lua_pushinteger(luavm, context.EFlags);
    lua_setglobal(luavm, 'EFLAGS');



    {$ifdef cpu64}
    lua_pushinteger(luavm, context.r8);
    lua_setglobal(luavm, 'R8');

    lua_pushinteger(luavm, context.r9);
    lua_setglobal(luavm, 'R9');

    lua_pushinteger(luavm, context.r10);
    lua_setglobal(luavm, 'R10');

    lua_pushinteger(luavm, context.r11);
    lua_setglobal(luavm, 'R11');

    lua_pushinteger(luavm, context.r12);
    lua_setglobal(luavm, 'R12');

    lua_pushinteger(luavm, context.r13);
    lua_setglobal(luavm, 'R13');

    lua_pushinteger(luavm, context.r14);
    lua_setglobal(luavm, 'R14');

    lua_pushinteger(luavm, context.r15);
    lua_setglobal(luavm, 'R15');
    {$endif}


  finally
    LuaCS.Leave;
  end;
end;

function CheckIfConditionIsMetContext(context: PContext; script: string): boolean;
{
precondition: script returns a value (so already has the 'return ' part appended for single line scripts)
}
var i: integer;
begin
  LuaCS.Enter;
  try
    LUA_SetCurrentContextState(context);

    if lua_dostring(luavm, pchar(script))=0 then
    begin
      i:=lua_gettop(LuaVM);
      if i=1 then //valid return
      begin
        result:=lua_toboolean(LuaVM, -1);

        lua_pop(LuaVM, lua_gettop(luavm));
        //and now set the register values
      end;
    end else lua_pop(LuaVM, lua_gettop(luavm)); //balance the stack
  finally
    LuaCS.Leave;
  end;
end;

function LuaPanic(L: Plua_State): Integer; cdecl;
begin
  raise exception.create('LUA panic!');
end;


function readBytes(processhandle: dword; L: PLua_State): integer; cdecl;
var paramcount: integer;
  addresstoread: ptruint;
  bytestoread: integer;
  i: integer;
  bytes: array of byte;
  x: dword;
begin
  paramcount:=lua_gettop(L);


  addresstoread:=lua_tointeger(L,-paramcount);

  if paramcount>1 then
    bytestoread:=lua_tointeger(L,-paramcount+1)
  else
    bytestoread:=1;

  lua_pop(L, paramcount);

  setlength(bytes,bytestoread);
  ZeroMemory(@bytes[0], bytestoread);
  if ReadProcessMemory(processhandle, pointer(addresstoread), @bytes[0], bytestoread, x) then
    for i:=0 to x-1 do
      lua_pushinteger(L,bytes[i]);

  result:=x;
end;


function writeBytes(processhandle: dword; L: PLua_State): integer;
var
  paramcount: integer;
  bytes: array of byte;
  i: integer;
  address: ptruint;
  x: dword;
  oldprotect: dword;
begin
  paramcount:=lua_gettop(L);
  if paramcount=0 then exit;

  setlength(bytes,paramcount-1);

  address:=lua_tointeger(L, -paramcount);


  for i:=-paramcount+1 to -1 do
   bytes[i]:=lua_tointeger(L,i);

  x:=0;
  VirtualProtectEx(processhandle, pointer(address), paramcount-1, PAGE_EXECUTE_READWRITE, oldprotect);
  WriteProcessMemory(processhandle, pointer(address), @bytes[0], paramcount-1, x);
  VirtualProtectEx(processhandle, pointer(address), paramcount-1, oldprotect, oldprotect);


  lua_pop(L, paramcount);
  lua_pushinteger(L, x);    //return the number of bytes written

  result:=1;  //return 1 value
end;

function writeBytes_fromlua(L: PLua_state): integer; cdecl;
begin
  result:=writeBytes(processhandle, L);
end;

function readBytes_fromlua(L: PLua_State): integer; cdecl;
begin
  result:=readBytes(processhandle, L);
end;

function writeBytesLocal_fromlua(L: PLua_state): integer; cdecl;
begin
  result:=writebytes(getcurrentprocess, L);
end;

function readBytesLocal_fromlua(L: PLua_State): integer; cdecl;
begin
  result:=readbytes(getcurrentprocess, L);
end;


initialization
  LuaCS:=TCriticalSection.create;
  LuaVM:=lua_open();

  if LuaVM<>nil then
  begin
    luaL_openlibs(LuaVM);

    lua_atpanic(LuaVM, LuaPanic);
    lua_register(LuaVM, 'readBytes', readbytes_fromlua);
    lua_register(LuaVM, 'writeBytes', writebytes_fromlua);

    lua_register(LuaVM, 'readBytesLocal', readbyteslocal_fromlua);
    lua_register(LuaVM, 'writeBytesLocal', writebyteslocal_fromlua);
  end;

finalization
  if LuaCS<>nil then
    LuaCS.free;

  if LuaVM<>nil then
    lua_close(LuaVM);

end.

