unit LuaHandler;

{todo: Split up into smaller units. 7800 lines is becomming too big}

{$mode delphi}

interface

uses
  windows, Classes, dialogs, SysUtils, lua, lualib, lauxlib, syncobjs, cefuncproc,
  newkernelhandler, autoassembler, Graphics, controls, LuaCaller, forms, ExtCtrls,
  StdCtrls, comctrls, ceguicomponents, generichotkey, luafile, xmplayer_server,
  ExtraTrainerComponents, customtimer, menus;

var
  LuaVM: Plua_State;
  LuaCS: Tcriticalsection;


function lua_strtofloat(s: string): double;
function lua_strtoint(s: string): integer;

procedure Lua_RegisterObject(name: string; o: TObject);
function CheckIfConditionIsMetContext(context: PContext; script: string): boolean;
procedure LUA_DoScript(s: string);
function LUA_functioncall(routinetocall: string; parameters: array of const): integer;
procedure LUA_memrec_callback(memrec: pointer; routine: string);
procedure LUA_SetCurrentContextState(context: PContext);
function LUA_onBreakpoint(context: PContext): boolean;
procedure LUA_onNotify(functionid: integer; sender: tobject);
function Lua_ToString(L: Plua_State; i: Integer): string;
procedure InitializeLuaScripts;
procedure InitializeLua;


implementation

uses mainunit, frmluaengineunit, pluginexports, MemoryRecordUnit, debuggertypedefinitions,
  symbolhandler, frmautoinjectunit, simpleaobscanner, addresslist, memscan, foundlisthelper,
  cesupport, DBK32functions, sharedMemory, disassembler, LuaCanvas, LuaPen, LuaFont, LuaBrush,
  LuaPicture, LuaMenu, MemoryBrowserFormUnit, disassemblerviewunit, hexviewunit;

resourcestring
  rsLUA_DoScriptWasNotCalledRomTheMainThread = 'LUA_DoScript was not called '
    +'rom the main thread';
  rsUndefinedLuaError = 'Undefined lua error';
  rsCheatengineIsBeingAFag = 'Cheatengine is being a fag';
  rsPluginAddress = 'Plugin Address';
  rsInvalidFloat = 'Invalid floating point string:%s';
  rsInvalidInt = 'Invalid integer:%s';


function lua_isstring(L: PLua_state; i: integer): boolean;
begin
  result := lua_type(L,i)=LUA_TSTRING;
end;

function Lua_ToString(L: Plua_State; i: Integer): string;
var r: pchar;
begin
  if lua_islightuserdata(L, i) then
    Result := inttohex(ptruint(lua_touserdata(L, i)),1)
  else
  begin
    r := lua.lua_tostring(l,i);
    if r<>nil then
      result:=r
    else
      result:='';
  end;
end;

procedure InitializeLuaScripts;
var f: string;
  i: integer;
  pc: pchar;
begin
  f:='main.lua';
  if not FileExists(f) then //perhaps in the cedir
  begin
    f:=CheatEngineDir+'main.lua';
    if not FileExists(f) then
    begin
      //try the defines only then
      f:='defines.lua';
      if not FileExists(f) then
      begin
        f:=CheatEngineDir+'defines.lua';
        if not FileExists(f) then
          exit;
      end;
    end;
  end;

  //file exists

  LuaCS.Enter;
  try
    if lua_dofile(luavm, pchar(f))<>0 then
    begin
      i:=lua_gettop(luavm);
      if i>0 then
      begin
        pc:=lua_tolstring(luavm, -1,nil);
        if pc<>nil then
          showmessage('main.lua error:'+pc)
        else
          showmessage('main.lua error');
      end
      else showmessage('main.lua error');

    end;

    lua_pop(LuaVM, lua_gettop(luavm)); //reset stack
  finally
    LuaCS.Leave;
  end;

end;

function lua_strtofloat(s: string): double;
var stackpos: integer;
  s2: integer;
begin
  LuaCS.enter;
  try
    stackpos:=lua_gettop(luavm);
    if lua_dostring(luavm, pchar('return '+s) )=0 then
    begin
      s2:=lua_gettop(luavm);
      if (s2-stackpos)>0 then
        result:=lua_tonumber(luavm, stackpos-s2)
      else
        raise exception.create(Format(rsInvalidFloat, [s]));
    end
    else
      raise exception.create(Format(rsInvalidFloat, [s]));

  finally
    lua_settop(luavm, stackpos);
    LuaCS.leave;
  end;
end;

function lua_strtoint(s: string): integer;
var stackpos: integer;
  s2: integer;
begin
  LuaCS.enter;
  try
    stackpos:=lua_gettop(luavm);
    if lua_dostring(luavm, pchar('return '+s) )=0 then
    begin
      s2:=lua_gettop(luavm);
      if (s2-stackpos)>0 then
        result:=lua_tointeger(luavm, stackpos-s2)
      else
        raise exception.create(Format(rsInvalidInt, [s]));
    end
    else
      raise exception.create(Format(rsInvalidInt, [s]));

  finally
    lua_settop(luavm, stackpos);
    LuaCS.leave;
  end;

end;

procedure Lua_RegisterObject(name: string; o: TObject);
begin
  LuaCS.enter;
  try
    lua_pop(LuaVM, lua_gettop(luavm));
    lua_pushlightuserdata(LuaVM, o);
    lua_setglobal(LuaVM, pchar(name));
  finally
    LuaCS.Leave;
  end;
end;

function LUA_onBreakpoint(context: PContext): boolean;
var p: integer;
begin
  result:=false;
  LuaCS.enter;
  try
    lua_pop(LuaVM, lua_gettop(luavm)); //clear it just to be sure

    lua_getfield(luavm, LUA_GLOBALSINDEX, pchar('debugger_onBreakpoint'));

    p:=lua_gettop(luavm);
    if p<>0 then //debugger_onBreakpoint is defined
    begin
      if lua_isfunction(luavm, -1) then //it's a function, yeeeeeeeeh
      begin
        LUA_SetCurrentContextState(context);


       // lua_pop(LuaVM, lua_gettop(luavm));
        p:=lua_gettop(luavm);

        if lua_pcall(LuaVM, 0, 1, 0)=0 then
        begin
          p:=lua_gettop(luavm);

          if (p=1) then //only 1 parameter returned
            result:=lua_tointeger(luavm, -1)<>0;  //return the result is not 0


          lua_pop(LuaVM, lua_gettop(luavm)); //clear stack

          //set new state if changes where made

         //if p<>0 then
          begin
          //  if lua_toboolean(luavm, -1) then
            begin
              lua_getglobal(luavm, 'EFLAGS');
              context.EFLAGS:=lua_tointeger(luavm, -1);

              if not processhandler.is64bit then
              begin
                lua_getglobal(luavm, 'EAX');
                context.{$ifdef cpu64}rax{$else}eax{$endif}:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'EBX');
                context.{$ifdef cpu64}rbx{$else}ebx{$endif}:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'ECX');
                context.{$ifdef cpu64}rcx{$else}ecx{$endif}:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'EDX');
                context.{$ifdef cpu64}rdx{$else}edx{$endif}:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'ESI');
                context.{$ifdef cpu64}rsi{$else}esi{$endif}:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'EDI');
                context.{$ifdef cpu64}rdi{$else}edi{$endif}:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'EBP');
                context.{$ifdef cpu64}rbp{$else}ebp{$endif}:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'EIP');
                context.{$ifdef cpu64}rip{$else}eip{$endif}:=lua_tointeger(luavm, -1);


              end
              else
              begin



              {$ifdef cpu64}
                lua_getglobal(luavm, 'RAX');
                context.RAX:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'RBX');
                context.RBX:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'RCX');
                context.RCX:=lua_tointeger(luavm, -1);


                lua_getglobal(luavm, 'RDX');
                context.RDX:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'RSI');
                context.RSI:=lua_tointeger(luavm, -1);


                lua_getglobal(luavm, 'RDI');
                context.RDI:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'RBP');
                context.RBP:=lua_tointeger(luavm, -1);


                lua_getglobal(luavm, 'RSP');
                context.RSP:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'RIP');
                context.RIP:=lua_tointeger(luavm, -1);


                lua_getglobal(luavm, 'R8');
                context.R8:=lua_tointeger(luavm, -1);


                lua_getglobal(luavm, 'R9');
                context.R9:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'R10');
                context.R10:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'R11');
                context.R11:=lua_tointeger(luavm, -1);


                lua_getglobal(luavm, 'R12');
                context.R12:=lua_tointeger(luavm, -1);


                lua_getglobal(luavm, 'R13');
                context.R13:=lua_tointeger(luavm, -1);


                lua_getglobal(luavm, 'R14');
                context.R14:=lua_tointeger(luavm, -1);

                lua_getglobal(luavm, 'R15');
                context.R15:=lua_tointeger(luavm, -1);
              {$endif}

              end;

              //lua_pop(luavm, lua_gettop(luavm));

            end;
          end;

        end;
      end;
    end;



  finally
    lua_pop(LuaVM, lua_gettop(luavm));
    LuaCS.leave;
  end;
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

procedure LUA_DoScript(s: string);
var i: integer;
  pc: pchar;
begin
  if GetCurrentThreadId<>MainThreadID then raise exception.create(rsLUA_DoScriptWasNotCalledRomTheMainThread);

  LUACS.Enter;
  try
    i:=lua_dostring(luavm, pchar(s));
    if i<>0 then
    begin
      pc:=lua.lua_tostring(luavm, -1);
      if pc<>nil then
        raise Exception.Create(pc)
      else
        raise exception.create(rsUndefinedLuaError);

    end;
  finally
    lua_pop(luavm, lua_gettop(luavm)); //clear the stack
    LUACS.Leave;
  end;
end;

procedure LUA_onNotify(functionid: integer; sender: tobject);
begin
  LUACS.enter;
  try
    lua_rawgeti(Luavm, LUA_REGISTRYINDEX, functionid);

    lua_pushlightuserdata(Luavm, sender);
    lua_pcall(Luavm, 1, 0, 0);
  finally
    lua_pop(luavm, lua_gettop(luavm)); //clear the stack
    LUACS.Leave;
  end;
end;

procedure LUA_memrec_callback(memrec: pointer; routine: string);
var m: TMemoryrecord;
  p: integer;
begin
  LuaCS.Enter;
  try
    m:=memrec;

    lua_getfield(luavm, LUA_GLOBALSINDEX, pchar(routine));

    p:=lua_gettop(luavm);
    if p<>0 then
    begin
      if lua_isfunction(luavm, -1) then
      begin
        lua_pushlightuserdata(luavm, memrec);
        lua_pcall(luavm, 1, 0, 0);
      end;


    end;
  finally
    lua_pop(luavm,lua_gettop(luavm));
    luacs.Leave;
  end;
end;

function LUA_functioncall(routinetocall: string; parameters: array of const): integer;
var i: integer;
  c: string;
  p: integer;
  oldstack: integer;
begin
  result:=-1;
  oldstack:=lua_gettop(luavm);

 // if luacs.TryEnter then
  begin
    try
      //check if the routine exists
      lua_getfield(luavm, LUA_GLOBALSINDEX, pchar(routinetocall));

      p:=lua_gettop(luavm);
      if p<>0 then
      begin
        if lua_isfunction(luavm, -1) then
        begin
          //routine exists, fill in the parameters
          for i:=0 to length(parameters)-1 do
          begin
            case parameters[i].VType of
              system.vtInteger : lua_pushinteger(LUAVM, parameters[i].VInteger);
              system.vtBoolean: lua_pushboolean(LUAVM, parameters[i].VBoolean);
              system.vtChar:
              begin
                c:=parameters[i].VChar;
                lua_pushstring(LUAVM, c);
              end;
              system.vtExtended: lua_pushnumber(LUAVM, parameters[i].VExtended^);
              system.vtString: lua_pushstring(LUAVM, pchar(parameters[i].VString));
              system.vtPointer: lua_pushlightuserdata(LUAVM, parameters[i].VPointer);
              system.vtPChar: lua_pushstring(LUAVM, parameters[i].VPChar);
              system.vtObject: lua_pushlightuserdata(LUAVM, pointer(parameters[i].VObject));
              system.vtClass: lua_pushlightuserdata(LUAVM, pointer(parameters[i].VClass));
              system.vtWideChar, vtPWideChar, vtVariant, vtInterface,
                vtWideString: lua_pushstring(LUAVM, rsCheatengineIsBeingAFag);
              system.vtAnsiString: lua_pushstring(LUAVM, pchar(parameters[i].VAnsiString));
              system.vtCurrency: lua_pushnumber(LUAVM, parameters[i].VCurrency^);
              system.vtInt64:
              begin
                if (parameters[i].VInt64^<=$ffffffff) then
                  lua_pushinteger(LUAVM, parameters[i].VInt64^)
                else
                  lua_pushlightuserdata(LUAVM, pointer(parameters[i].VInt64^));
              end;
              system.vtQWord:
              begin
                if (parameters[i].VQWord^<=$ffffffff) then
                  lua_pushinteger(LUAVM, parameters[i].VQWord^)
                else
                  lua_pushlightuserdata(LUAVM, pointer(parameters[i].VQWord^));
              end;
            end;

          end;

          lua_pcall(luavm, length(parameters), 1, 0);
          i:=lua_gettop(luavm);
          if i>0 then //it has a parameter
            result:=lua_tointeger(luavm, -1);
        end;


      end;


    finally
      lua_pop(luavm,oldstack-lua_gettop(luavm));
 //     luacs.leave;
    end;

  end;
end;

 {
procedure LUA_callback(routine: string; parameters: tvararray);
var m: TMemoryrecord;
  p: integer;
begin
  LuaCS.Enter;
  try
    m:=memrec;

    lua_getfield(luavm, LUA_GLOBALSINDEX, pchar(routine));

    p:=lua_gettop(luavm);
    if p<>0 then
    begin
      if lua_isfunction(luavm, -1) then
      begin
        lua_pushlightuserdata(luavm, memrec);
        lua_pcall(luavm, 1, 0, 0);
      end;


    end;
  finally
    lua_pop(luavm,lua_gettop(luavm));
    luacs.Leave;
  end;
end; }

function CheckIfConditionIsMetContext(context: PContext; script: string): boolean;
{
precondition: script returns a value (so already has the 'return ' part appended for single line scripts)
}
var i: integer;
begin
  result:=false;
  LuaCS.Enter;
  try
    LUA_SetCurrentContextState(context);

    if lua_dostring(luavm, pchar(script))=0 then
    begin
      i:=lua_gettop(LuaVM);
      if i=1 then //valid return
        result:=lua_toboolean(LuaVM, -1);
    end;
  finally
    lua_pop(LuaVM, lua_gettop(luavm));
    LuaCS.Leave;
  end;
end;

function LuaPanic(L: Plua_State): Integer; cdecl;
begin
  result:=0;
  lua_pop(LuaVM, lua_gettop(luavm));
  raise exception.create('LUA panic!');
end;


function sleep(L: PLua_State): integer; cdecl;
var
  parameters: integer;
begin
  parameters:=lua_gettop(L);

  result:=0;

  if parameters=1 then
    windows.sleep(lua_tointeger(L, -1));

  lua_pop(L, parameters);
end;

function print2(param: pointer): pointer;
begin
  if frmLuaEngine=nil then
    frmLuaEngine:=TfrmLuaEngine.Create(nil);

  frmLuaEngine.mOutput.Lines.add(pchar(param));

  if frmLuaEngine.cbShowOnPrint.checked then
    frmLuaEngine.show;

  result:=nil;
end;

function print(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  s: string;

  str: string;
  i: integer;
begin
  parameters:=lua_gettop(L);
  if parameters=0 then exit;

  str:='';
  for i:=-parameters to -1 do
  begin
    if lua_islightuserdata(L,i) then
      s:=inttohex(ptruint(lua_touserdata(L, i)),8)
    else
      s:=lua_tostring(L, i);

    str:=str+s+' ';
  end;

  if str<>'' then
    pluginsync(print2, @str[1]);

  lua_pop(L, parameters);
  lua_pushstring(L, str);
  result:=1;
end;

function showMessage_lua(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  s: string;
begin
  parameters:=lua_gettop(L);
  if parameters=0 then exit;

  if lua_islightuserdata(l,-1) then
    s:=inttohex(ptruint(lua_touserdata(L, -1)),8)
  else
    s:=lua_tostring(L, -1);

  ce_showmessage(pchar(s));

  lua_pop(L, parameters);
  result:=0;
end;

function readInteger(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: integer;
  r: dword;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=1 then
    begin
      //ShowMessage(inttostr(lua_type(L, -1)));

      if lua_isstring(L, -1) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-1))
      else
        address:=lua_tointeger(L,-1);


      lua_pop(L, parameters);

      v:=0;
      if ReadProcessMemory(processhandle, pointer(address), @v, sizeof(v), r) then
      begin
        lua_pushinteger(L, v);
        result:=1;
      end;

    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function readFloat(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: single;
  r: dword;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=1 then
    begin
      if lua_isstring(L, -1) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-1))
      else
        address:=lua_tointeger(L,-1);

      lua_pop(L, parameters);

      v:=0;
      if ReadProcessMemory(processhandle, pointer(address), @v, sizeof(v), r) then
      begin
        lua_pushnumber(L, v);
        result:=1;
      end;

    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function readDouble(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: double;
  r: dword;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=1 then
    begin
      if lua_isstring(L, -1) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-1))
      else
        address:=lua_tointeger(L,-1);

      lua_pop(L, parameters);

      v:=0;
      if ReadProcessMemory(processhandle, pointer(address), @v, sizeof(v), r) then
      begin
        lua_pushnumber(L, v);
        result:=1;
      end;

    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function readString(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: pchar;
  r: dword;
  maxsize: integer;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters>=1 then
    begin
      if lua_isstring(L, -parameters) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-2))
      else
        address:=lua_tointeger(L,-parameters);

      if parameters=2 then
        maxsize:=lua_tointeger(L,-1)
      else
        maxsize:=50;

      lua_pop(L, parameters);

      getmem(v,maxsize);
      try
        if ReadProcessMemory(processhandle, pointer(address), v, maxsize, r) then
        begin
          v[maxsize-1]:=#0;
          lua_pushstring(L, v);
          result:=1;
        end;


      finally
        freemem(v);
      end;

    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeInteger(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: integer;
  r: dword;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=2 then
    begin
      if lua_isstring(L, -2) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-2))
      else
        address:=lua_tointeger(L,-2);

      v:=lua_tointeger(L, -1);

      lua_pop(L, parameters);
      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), @v, sizeof(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeFloat(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: single;
  r: dword;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=2 then
    begin
      if lua_isstring(L, -2) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-2))
      else
        address:=lua_tointeger(L,-2);

      v:=lua_tonumber(L, -1);

      lua_pop(L, parameters);


      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), @v, sizeof(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeDouble(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: double;
  r: dword;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=2 then
    begin
      if lua_isstring(L, -2) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-2))
      else
        address:=lua_tointeger(L,-2);

      v:=lua_tonumber(L, -1);

      lua_pop(L, parameters);

      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), @v, sizeof(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeString(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: pchar;
  r: dword;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=2 then
    begin
      if lua_isstring(L, -2) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-2))
      else
        address:=lua_tointeger(L,-2);

      v:=lua.lua_tostring(L, -1);

      lua_pop(L, parameters);

      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), v, length(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function readBytesEx(processhandle: dword; L: PLua_State): integer; cdecl;
var parameters: integer;
  addresstoread: ptruint;
  bytestoread: integer;
  i: integer;
  bytes: array of byte;
  x: dword;
  tableversion: boolean;
begin
  tableversion:=false;
  result:=0;
  parameters:=lua_gettop(L);

  if lua_isstring(L, -parameters) then
    addresstoread:=symhandler.getAddressFromName(lua_tostring(L,-parameters))
  else
    addresstoread:=lua_tointeger(L,-parameters);

  if parameters>1 then
  begin
    bytestoread:=lua_tointeger(L,-parameters+1);

    if parameters>2 then
      tableversion:=lua_toboolean(L, -parameters+2);

  end
  else
    bytestoread:=1;

  lua_pop(L, parameters);

  setlength(bytes,bytestoread);
  ZeroMemory(@bytes[0], bytestoread);
  if ReadProcessMemory(processhandle, pointer(addresstoread), @bytes[0], bytestoread, x) then
  begin
    if tableversion then
    begin
      lua_newtable(L);
      for i:=0 to x-1 do
      begin
        lua_pushinteger(L, i);
        lua_pushinteger(L, bytes[i]);
        lua_settable(L, -3);
      end;
      result:=1;
    end
    else
    begin
      for i:=0 to x-1 do
        lua_pushinteger(L,bytes[i]);
      result:=x;
    end;
  end;


end;


function writeBytesEx(processhandle: dword; L: PLua_State): integer;
var
  parameters: integer;
  bytes: array of byte;
  i,j: integer;
  address: ptruint;
  x: dword;
  oldprotect: dword;
  b: byte;
begin
  parameters:=lua_gettop(L);
  if parameters=0 then exit;



  if lua_isstring(L, -parameters) then
    address:=symhandler.getAddressFromName(lua_tostring(L,-parameters))
  else
    address:=lua_tointeger(L,-parameters);

  if lua_istable(L, -parameters+1) then
  begin
    parameters:=lua_tointeger(L, -parameters+2);


    setlength(bytes,parameters-1);

    for i:=0 to parameters-1 do
    begin
      lua_pushinteger(L,i);
      lua_gettable(L, -3);

      j:=lua_tointeger(L,-1);
      showmessage(inttostr(i)+','+inttostr(j));
      lua_pop(L,1);
    end;
  end
  else
  begin
    setlength(bytes,parameters-1);

    j:=0;
    for i:=(-parameters)+1 to -1 do
    begin
      b:=lua_tointeger(L,i);
      bytes[j]:=b;
      inc(j);
    end;

  end;

  x:=0;
  VirtualProtectEx(processhandle, pointer(address), parameters-1, PAGE_EXECUTE_READWRITE, oldprotect);
  WriteProcessMemory(processhandle, pointer(address), @bytes[0], parameters-1, x);
  VirtualProtectEx(processhandle, pointer(address), parameters-1, oldprotect, oldprotect);


  lua_pop(L, parameters);
  lua_pushinteger(L, x);    //return the number of bytes written

  result:=1;  //return 1 value
end;

function writeBytes(L: PLua_state): integer; cdecl;
begin
  result:=writeBytesEx(processhandle, L);
end;

function readBytes(L: PLua_State): integer; cdecl;
begin
  result:=readBytesEx(processhandle, L);
end;

function writeBytesLocal(L: PLua_state): integer; cdecl;
begin
  result:=writebytesEx(getcurrentprocess, L);
end;

function readBytesLocal(L: PLua_State): integer; cdecl;
begin
  result:=readbytesEx(getcurrentprocess, L);
end;

function autoAssemble_lua(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  code: TStringlist;
  r: boolean;
begin
  parameters:=lua_gettop(L);
  if parameters=0 then exit;

  code:=tstringlist.create;
  try
    code.text:=lua_tostring(L, -1);

    try
      r:=autoassemble(code, false);
    except
      r:=false;
    end;

    lua_pop(L, parameters);
    lua_pushboolean(L, r);
  finally
    code.free;
  end;

  result:=1;
end;

function getPixel(L: PLua_State): integer; cdecl;
var t:TCanvas;
  parameters: integer;
  r: dword;
  x,y: integer;
begin
  result:=0; //return 0 paramaters
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    x:=lua_tointeger(L, -2); //x
    y:=lua_tointeger(L, -1); //y
    lua_pop(L, 2);

    try
      t:=TCanvas.create;
      try
        t.Handle:=getdc(0);
        r:=t.Pixels[x,y];

        lua_pushinteger(L,r); //push the color on the stack
        result:=1; //tell lue I put 1 parameter on the stack

        ReleaseDC(0,t.handle);
      finally
        t.free;
      end;
    except
    end;
  end else lua_pop(L, parameters);
end;

function getMousePos(L: PLua_State): integer; cdecl;
var t:TCanvas;
  parameters: integer;
  cp: Tpoint;
begin
  result:=0; //return 0 parameters
  parameters:=lua_gettop(L);
  if parameters=0 then
  begin
    cp:=mouse.CursorPos;
    lua_pushinteger(L, cp.x);
    lua_pushinteger(L, cp.y);
    result:=2;   //return 2 parameters
  end else lua_pop(L, parameters);
end;

function setMousePos(L: PLua_State): integer; cdecl;
var t:TCanvas;
  parameters: integer;
  cp: Tpoint;
begin
  result:=0; //return 0 parameters
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    cp.x:=lua_tointeger(L, -2); //x
    cp.y:=lua_tointeger(L, -1); //y
    lua_pop(L, 2);

    mouse.CursorPos:=cp;
  end else lua_pop(L, parameters);
end;

function createTableEntry(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: pointer;
begin
  lua_pop(L, lua_gettop(L)); //clear the stack

  r:=ce_createTableEntry;
  lua_pushlightuserdata(L, r);
  result:=1;
end;

function getTableEntry(L: PLua_State): integer; cdecl;
var parameters: integer;
  description: pchar;
  r: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    description:=lua.lua_tostring(L,-1); //description

    lua_pop(L, parameters);  //clear stack

    r:=ce_getTableEntry(description);
    if r<>nil then
    begin
      lua_pushlightuserdata(L,r); //return the pointer
      result:=1;
    end;
  end else lua_pop(L, parameters);
end;

function memoryrecord_setDescription(L: PLUA_State): integer; cdecl;
var
  parameters: integer;
  description: pchar;
  memrec: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memrec:=lua_touserdata(L,-2); //memrec
    description:=lua.lua_tostring(L,-1); //description

    lua_pop(L, parameters);  //clear stack

    ce_memrec_setDescription(memrec, description);
  end;

  lua_pop(L, parameters);  //clear stack

end;

function memoryrecord_getDescription(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: pchar;
  memrec: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L,-1);
    d:=ce_memrec_getDescription(memrec);
    if d<>nil then
    begin
      lua_pushstring(L, d);
      result:=1;
    end;
  end else lua_pop(L, parameters);
end;

function memoryrecord_getAddress(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  memrec: pointer;
  address: ptruint;
  offsets: array of dword;
  offsetcount: integer;
  i: integer;
begin
  result:=0;
  offsetcount:=0;
  setlength(offsets,0);

  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    if ce_memrec_getAddress(memrec, @address, nil, 0, @offsetcount) then
    begin
      if offsetcount>0 then
      begin
        setlength(offsets,offsetcount);
        ce_memrec_getAddress(memrec, @address, @offsets[0], length(offsets), @offsetcount);
      end;


      lua_pushinteger(L,address);

      for i:=0 to offsetcount-1 do
        lua_pushinteger(L, offsets[i]);

      result:=1+offsetcount;


    end;

  end else lua_pop(L, parameters);
end;

function memoryrecord_setAddress(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;
  address: pchar;

  s: string;
  offsets: array of dword;
  i,j: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=2 then
  begin
    memrec:=lua_touserdata(L, (-parameters));

    if lua_isstring(L, (-parameters)+1) then
      address:=lua.lua_tostring(L, (-parameters)+1)
    else //convert it to a hexadecimal value first
    begin
      s:=inttohex(lua_tointeger(L, (-parameters)+1),8);
      address:=pchar(s);
    end;

    setlength(offsets,parameters-2);
    j:=0;
    for i:=(-parameters)+2 to -1 do
    begin
      offsets[j]:=lua_tointeger(L, i);
      inc(j);
    end;

    lua_pop(L, parameters);

    ce_memrec_setAddress(memrec, address, @offsets[0], length(offsets))
  end else
    lua_pop(L, parameters);
end;

function memoryrecord_getType(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L, (-parameters));
    lua_pop(L, parameters);

    lua_pushinteger(L, ce_memrec_getType(memrec));
    result:=1;

  end;
end;

function memoryrecord_setType(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  vtype: integer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memrec:=lua_touserdata(L, -2);
    vtype:=lua_tointeger(L, -1);
    lua_pop(L, parameters);

    ce_memrec_setType(memrec, vtype);
  end
  else
    lua_pop(L, parameters);

end;

function memoryrecord_getValue(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;

  v: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L, (-parameters));
    lua_pop(L, parameters);


    getmem(v,255);
    try
      if ce_memrec_getValue(memrec, v, 255) then
      begin
        lua_pushstring(L, v);
        result:=1;
      end;

    finally
      freemem(v);
    end;
  end else lua_pop(L, parameters);
end;


function memoryrecord_setValue(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;

  v: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memrec:=lua_touserdata(L, -2);
    v:=lua.lua_tostring(L, -1);


    ce_memrec_setValue(memrec, v);
  end;
  lua_pop(L, parameters);
end;

function memoryrecord_getScript(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;

  v: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L, -1);
    v:=ce_memrec_getScript(memrec);
    lua_pop(L, parameters);

    if v<>nil then
    begin
      lua_pushstring(L, v);
      result:=1;
    end;

  end else lua_pop(L, parameters);


end;


function memoryrecord_setScript(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;

  v: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memrec:=lua_touserdata(L, -2);
    v:=lua.lua_tostring(L, -1);

    ce_memrec_setScript(memrec, v);
  end;


  lua_pop(L, parameters);
end;


function memoryrecord_isActive(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  direction: integer;
  memrec: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L, -parameters);
    lua_pop(L, parameters);

    lua_pushboolean(L, ce_memrec_isFrozen(memrec));
    result:=1;
  end
  else lua_pop(L, parameters);
end;

function memoryrecord_freeze(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;
  direction: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    memrec:=lua_touserdata(L, -parameters);


    if parameters=2 then
      direction:=lua_tointeger(L, -1)
    else
      direction:=0;

    ce_memrec_freeze(memrec, direction);
  end;

  lua_pop(L, parameters);
end;

function memoryrecord_unfreeze(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;
  direction: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L, -parameters);
    ce_memrec_unfreeze(memrec);
  end;

  lua_pop(L, parameters);
end;

function memoryrecord_setColor(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;
  color: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memrec:=lua_touserdata(L,-2);
    color:=lua_tointeger(L,-1);
    ce_memrec_setColor(memrec,color);
  end;

  lua_pop(L, parameters);
end;

function memoryrecord_appendToEntry(L: PLua_State): integer; cdecl;
var
  memrec1,memrec2: pointer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memrec1:=lua_touserdata(L,-2);
    memrec2:=lua_touserdata(L,-1);
    ce_memrec_appendtoentry(memrec1,memrec2);
  end;

  lua_pop(L, parameters);
end;

function memoryrecord_delete(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L,-2);
    ce_memrec_delete(memrec);
  end;

  lua_pop(L, parameters);
end;

function memoryrecord_getID(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memoryrecord:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, memoryrecord.id);
    result:=1;

  end else lua_pop(L, parameters);
end;

function memoryrecord_getHotkeyCount(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memoryrecord:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, memoryrecord.HotkeyCount);
    result:=1;

  end else lua_pop(L, parameters);
end;

function memoryrecord_getHotkey(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memoryrecord:=lua_touserdata(L,-2);
    index:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, memoryrecord.Hotkey[index]);
    result:=1;

  end else lua_pop(L, parameters);
end;

function memoryrecord_getHotkeyByID(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
  id: integer;
  i: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memoryrecord:=lua_touserdata(L,-2);
    id:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    for i:=0 to memoryrecord.Hotkeycount-1 do
      if memoryrecord.Hotkey[i].id=id then
      begin
        lua_pushlightuserdata(L, memoryrecord.Hotkey[i]);
        result:=1;
      end;

  end else lua_pop(L, parameters);
end;


function memoryrecord_onActivate(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memoryrecord:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(memoryrecord.onActivate));
    memoryrecord.onActivate:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      memoryrecord.onActivate:=lc.ActivateEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      memoryrecord.onActivate:=lc.ActivateEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function memoryrecord_onDeactivate(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memoryrecord:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(memoryrecord.onDeactivate));
    memoryrecord.onDeactivate:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      memoryrecord.onDeactivate:=lc.ActivateEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      memoryrecord.onDeactivate:=lc.ActivateEvent;
    end;


  end;

  lua_pop(L, parameters);
end;

function isKeyPressed(L: PLua_State): integer; cdecl;
var parameters: integer;
  keyinput: pchar;
  key: integer;
  w: word;
  r: boolean;
begin
  result:=0;
  r:=false;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L,-1) then  //char given instead of keycode
    begin
      keyinput:=lua.lua_tostring(L,-1);
      if keyinput<>nil then
        key:=ord(keyinput[0]);
    end
    else

    if lua_isnumber(L,-1) then //keycode
      key:=lua_tointeger(L,-1);

    lua_pop(L, parameters); //parameters have been fetched, clear stack

    if key<>0 then
    begin
      w:=GetAsyncKeyState(key);
      r:=(w and 1)=1;

      if not r then
        r:=((w shr 15) and 1)=1;

      lua_pushboolean(L, r);
      result:=1;
    end;

  end else lua_pop(L, parameters);

end;


function keyDown(L: PLua_State): integer; cdecl;
var parameters: integer;
  keyinput: pchar;
  key: integer;
  w: word;
  r: boolean;
begin
  result:=0;
  r:=false;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L,-1) then  //char given isntead of keycode
    begin
      keyinput:=lua.lua_tostring(L,-1);
      if keyinput<>nil then
        key:=ord(keyinput[0]);
    end
    else
    if lua_isnumber(L,-1) then //keycode
      key:=lua_tointeger(L,-1);


    if key<>0 then
      keybd_event(key, 0,0,0);

  end;
  lua_pop(L, parameters);
end;


function keyUp(L: PLua_State): integer; cdecl;
var parameters: integer;
  keyinput: pchar;
  key: integer;
  w: word;
  r: boolean;
begin
  result:=0;
  r:=false;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L,-1) then  //char given isntead of keycode
    begin
      keyinput:=lua.lua_tostring(L,-1);
      if keyinput<>nil then
        key:=ord(keyinput[0]);
    end
    else
    if lua_isnumber(L,-1) then //keycode
      key:=lua_tointeger(L,-1);


    if key<>0 then
      keybd_event(key, 0,KEYEVENTF_KEYUP,0);

  end;
  lua_pop(L, parameters);
end;

function doKeyPress(L: PLua_State): integer; cdecl;
var parameters: integer;
  keyinput: pchar;
  key: integer;
  w: word;
  r: boolean;
begin
  result:=0;
  r:=false;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L,-1) then  //char given isntead of keycode
    begin
      keyinput:=lua.lua_tostring(L,-1);
      if keyinput<>nil then
        key:=ord(keyinput[0]);
    end
    else
    if lua_isnumber(L,-1) then //keycode
      key:=lua_tointeger(L,-1);


    if key<>0 then
    begin
      keybd_event(key, 0, 0, 0);
      sleep(110);
      keybd_event(key, 0, KEYEVENTF_KEYUP, 0);
    end;

  end;
  lua_pop(L, parameters);
end;

function getProcessIDFromProcessName(L: PLua_state): integer; cdecl;
var parameters: integer;
  pname: pchar;
  pid: dword;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    pname:=lua.lua_tostring(L, -1);
    lua_pop(L, parameters);

    pid:=ce_getProcessIDFromProcessName(pname);
    if pid<>0 then
    begin
      lua_pushinteger(L, pid);
      result:=1;
    end;


  end else lua_pop(L, parameters);
end;

function openProcess(L: PLua_state): integer; cdecl;
var parameters: integer;
  pname: pchar;
  pid: dword;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L,-1) then
    begin
      pname:=lua.lua_tostring(L,-1);
      pid:=ce_getProcessIDFromProcessName(pname);
    end
    else
      pid:=lua_tointeger(L,-1);

    lua_pop(L, parameters);

    if pid<>0 then
      ce_openProcess(pid);

    if (ProcessHandle<>0) and (processid=pid) then
    begin
      lua_pushboolean(L, true);
      result:=1;
    end;

  end else lua_pop(L, parameters);
end;

function beep(L: PLua_state): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L)); //clear the stack
  SysUtils.Beep;
  result:=0;
end;

function pause(L: PLua_state): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L)); //clear the stack
  ce_pause;
  result:=0;
end;

function unpause(L: PLua_state): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L)); //clear the stack
  ce_unpause;
  result:=0;
end;


function debugProcess(L: PLua_state): integer; cdecl;
var parameters: integer;
  debuggerinterface: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
    debuggerinterface:=lua_tointeger(L, -1)
  else
    debuggerinterface:=0;

  lua_pop(L, lua_gettop(L)); //clear the stack

  ce_debugProcess(debuggerinterface);
end;

function debug_setBreakpoint(L: Plua_State): integer; cdecl;
var parameters: integer;
  i,j: integer;

  address: ptruint;
  size: integer;
  trigger: integer;
  e: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    j:=0;
    address:=0;
    size:=1;
    trigger:=integer(bptexecute);
    for i:=-parameters to -1 do
    begin
      case j of
        0:
        begin
          if lua_isstring(L, i) then
          begin
            e:=false;
            address:=symhandler.getAddressFromName(lua_tostring(L, i), false,e);
            if e then //unrecognizable address
            begin
              lua_pop(L, lua_gettop(L));
              exit;
            end;
          end
          else
            address:=lua_tointeger(L, i);

        end;
        1: size:=lua_tointeger(L,i);
        2: trigger:=lua_tointeger(L,i);
      end;
      inc(j);
    end;

    try
      ce_debug_setBreakpoint(address,size,TBreakpointTrigger(trigger));
    except
    end;
  end;

  lua_pop(L, lua_gettop(L)); //clear the stack
end;

function debug_removeBreakpoint(L: Plua_State): integer; cdecl;
var parameters: integer;
  address: ptruint;
  e: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L, -1) then
    begin
      e:=false;
      address:=symhandler.getAddressFromName(lua_tostring(L, -1), false,e);
      if e then //unrecognizable address
      begin
        lua_pop(L, lua_gettop(L));
        exit;
      end;
    end
    else
      address:=lua_tointeger(L, -1);

    ce_debug_removeBreakpoint(address);
  end;

  lua_pop(L, lua_gettop(L)); //clear the stack
end;

function debug_continueFromBreakpoint(L: Plua_State): integer; cdecl;
var parameters: integer;
  method: TContinueOption;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    method:=TContinueOption(lua_tointeger(L, -1));
    ce_debug_continuefrombreakpoint(method);
  end;

  lua_pop(L, lua_gettop(L)); //clear the stack
end;

function closeCE(L: Plua_state): integer; cdecl;
begin
  ce_closeCE;
  result:=0;
end;

function hideAllCEWindows(L: Plua_State): integer; cdecl;
begin
  result:=0;
  lua_pop(L, lua_gettop(L)); //clear the stack

  ce_hideAllCEWindows;
end;

function unhideMainCEwindow(L: Plua_State): integer; cdecl;
begin
  result:=0;
  lua_pop(L, lua_gettop(L)); //clear the stack

  ce_unhideMainCEwindow;
end;

function createForm(L: Plua_State): integer; cdecl;
var f: pointer;
  parameters: integer;
  visible: boolean;
begin
  result:=1;
  parameters:=lua_gettop(L);

  if parameters=1 then
    visible:=lua_toboolean(L,-1)
  else
    visible:=true;

  lua_pop(L, lua_gettop(L));


  f:=ce_createForm(visible);
  lua_pushlightuserdata(L, f);
  result:=1;
end;

function form_onClose(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TCustomForm;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(control.onClose));
    control.onClose:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      control.OnClose:=lc.CloseEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnClose:=lc.CloseEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function form_centerScreen(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    ce_form_centerScreen(f);
  end;
  lua_pop(L, lua_gettop(L));
end;

function form_hide(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    ce_form_hide(f);
  end;
  lua_pop(L, lua_gettop(L));
end;

function form_showModal(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: tcustomform;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(L));
    lua_pushinteger(L, f.ShowModal);
  end
  else
   lua_pop(L, lua_gettop(L));

end;


function form_isForegroundWindow(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: tcustomform;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(L));
    lua_pushboolean(L, GetForegroundWindow()=f.Handle);
    result:=1;
  end
  else
   lua_pop(L, lua_gettop(L));

end;

function form_getMenu(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  form: TCustomForm;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    form:=lua_touserdata(L,-1);
    lua_pop(L, parameters);


    lua_pushlightuserdata(L, form.menu);
    result:=1;

  end else lua_pop(L, parameters);
end;

function form_setMenu(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  form: TCustomForm;
  menu: TMainmenu;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    form:=lua_touserdata(L,-2);
    menu:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    form.Menu:=menu;

  end else lua_pop(L, parameters);
end;



function form_show(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    ce_form_show(f);
  end;
  lua_pop(L, lua_gettop(L));
end;

function listView_getCanvas(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TlistView;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, c.Canvas);
    result:=1;

  end else lua_pop(L, parameters);
end;

function comboBox_getCanvas(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TcomboBox;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, c.Canvas);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listbox_getCanvas(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TListBox;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, c.Canvas);
    result:=1;

  end else lua_pop(L, parameters);

end;



function graphicControl_getCanvas(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TGraphicControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, c.Canvas);
    result:=1;

  end else lua_pop(L, parameters);
end;


function customControl_getCanvas(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TCustomControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, c.Canvas);
    result:=1;

  end else lua_pop(L, parameters);
end;

function createPanel(L: Plua_State): integer; cdecl;
var parameters: integer;
  f,p: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    p:=ce_createPanel(f);

    lua_pop(L, lua_gettop(L));

    lua_pushlightuserdata(L, p);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function createButton(L: Plua_State): integer; cdecl;
var parameters: integer;
  f,p: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    p:=ce_createButton(f);

    lua_pop(L, lua_gettop(L));

    lua_pushlightuserdata(L, p);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function createGroupBox(L: Plua_State): integer; cdecl;
var parameters: integer;
  f,p: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    p:=ce_createGroupBox(f);

    lua_pop(L, lua_gettop(L));

    lua_pushlightuserdata(L, p);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function createImage(L: Plua_State): integer; cdecl;
var parameters: integer;
  f,p: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    p:=ce_createImage(f);

    lua_pop(L, lua_gettop(L));

    lua_pushlightuserdata(L, p);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function image_loadImageFromFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  i: pointer;
  filename: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    i:=lua_touserdata(L, -2);
    filename:=lua.lua_tostring(L, -1);
    ce_image_loadImageFromFile(i,filename);
  end;

  lua_pop(L, lua_gettop(L));
end;

function image_stretch(L: Plua_State): integer; cdecl;
var parameters: integer;
  i: pointer;
  state: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    i:=lua_touserdata(L, -2);
    state:=lua_toboolean(L, -1);
    ce_image_stretch(i,state);
  end;

  lua_pop(L, lua_gettop(L));
end;

function image_transparent(L: Plua_State): integer; cdecl;
var parameters: integer;
  i: pointer;
  state: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    i:=lua_touserdata(L, -2);
    state:=lua_toboolean(L, -1);
    ce_image_transparent(i,state);
  end;

  lua_pop(L, lua_gettop(L));
end;

function Image_getCanvas(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TImage;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, c.Canvas);
    result:=1;

  end else lua_pop(L, parameters);
end;

function Image_getPicture(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TCustomImage;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L,-1);
    lua_pop(L, parameters);


    lua_pushlightuserdata(L, c.Picture);
    result:=1;

  end else lua_pop(L, parameters);
end;

function createHotkey(L: Plua_State): integer; cdecl;
var parameters: integer;
  h: TGenericHotkey;
  routine: string;

  lc: TLuaCaller;

  i: integer;
  keys: TKeycombo;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=2 then //(function, key, ...)
  begin
    lc:=TLuaCaller.create;

    if lua_isfunction(L, -parameters) then
      lc.luaroutineindex:=luaL_ref(L,LUA_REGISTRYINDEX)
    else
      lc.luaroutine:=lua_tostring(L,-parameters);

    zeromemory(@keys,sizeof(keys));
    for i:=-parameters+1 to -1 do
      keys[i+parameters-1]:=lua_tointeger(L, i);

    h:=TGenericHotkey.create(lc.NotifyEvent, keys);


    lua_pushlightuserdata(L, h);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function GenericHotkey_setKeys(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  GenericHotkey: TGenericHotkey;
  i: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=2 then
  begin
    Generichotkey:=lua_touserdata(L,-parameters);

    zeromemory(@GenericHotkey.keys,sizeof(GenericHotkey.keys));
    for i:=-parameters+1 to -1 do
      GenericHotkey.keys[i+parameters-1]:=lua_tointeger(L, i);
  end;

  lua_pop(L, parameters);
end;

function generichotkey_onHotkey(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  GenericHotkey: TGenericHotkey;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    GenericHotkey:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(GenericHotkey.onNotify));
    GenericHotkey.onNotify:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      GenericHotkey.onNotify:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      GenericHotkey.onNotify:=lc.NotifyEvent;
    end;

  end;

  lua_pop(L, parameters);
end;


function createLabel(L: Plua_State): integer; cdecl;
var parameters: integer;
  f,p: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    p:=ce_createLabel(f);

    lua_pop(L, lua_gettop(L));

    lua_pushlightuserdata(L, p);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function createEdit(L: Plua_State): integer; cdecl;
var parameters: integer;
  f,p: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    p:=ce_createEdit(f);

    lua_pop(L, lua_gettop(L));

    lua_pushlightuserdata(L, p);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function createMemo(L: Plua_State): integer; cdecl;
var parameters: integer;
  f,p: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    p:=ce_createMemo(f);

    lua_pop(L, lua_gettop(L));

    lua_pushlightuserdata(L, p);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function createTimer(L: Plua_State): integer; cdecl;
var parameters: integer;
  f,p: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    f:=lua_touserdata(L, -parameters);
    p:=ce_createTimer(f);

    if parameters>=2 then
      tcustomtimer(p).Enabled:=lua_toboolean(L, -parameters+1)
    else
      tcustomtimer(p).enabled:=true;

    lua_pop(L, lua_gettop(L));

    lua_pushlightuserdata(L, p);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function timer_setInterval(L: Plua_State): integer; cdecl;
var parameters: integer;
  t: TCustomTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    t:=lua_touserdata(L, -2);
    t.Interval:=lua_tointeger(L, -1);
  end;

  lua_pop(L, lua_gettop(L));
end;

function timer_onTimer(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  timer: TCustomTimer;
  f: integer;
  routine: string;

  lc: TLuaCaller;
  oldroutine: TNotifyEvent;
begin

  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    timer:=lua_touserdata(L,-2);
    oldroutine:=timer.OnTimer;

    CleanupLuaCall(TMethod(timer.OnTimer));
    timer.ontimer:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      timer.OnTimer:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      timer.OnTimer:=lc.NotifyEvent;
    end;


  end;

  lua_pop(L, parameters);
end;

function timer_setEnabled(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  Timer: TCustomTimer;
  Enabled: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    Timer:=lua_touserdata(L,-2);
    Timer.Enabled:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;

function timer_getEnabled(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  Timer: TCustomTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    Timer:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, Timer.Enabled);
    result:=1;

  end else lua_pop(L, parameters);
end;


function control_setCaption(L: Plua_State): integer; cdecl;
var parameters: integer;
  c: pointer;
  caption: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L, -2);
    caption:=lua.lua_tostring(L, -1);
    ce_control_setCaption(c,caption);
  end;

  lua_pop(L, lua_gettop(L));
end;

function control_getCaption(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: pchar;
  control: pointer;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    getmem(d,255);
    try
      if ce_control_getCaption(control, d, 255) then
      begin
        lua_pushstring(L, d);
        result:=1;
      end;


    finally
      freemem(d);
    end;
  end else lua_pop(L, parameters);
end;


function control_setPosition(L: Plua_State): integer; cdecl;
var parameters: integer;
  c: pointer;
  x,y: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    c:=lua_touserdata(L, -3);
    x:=lua_tointeger(L, -2);
    y:=lua_tointeger(L, -1);

    ce_control_setPosition(c,x,y);
  end;

  lua_pop(L, lua_gettop(L));
end;


function control_getPosition(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: pointer;
  x,y: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    x:=ce_control_getX(control);
    y:=ce_control_getY(control);

    lua_pushinteger(L, x);
    lua_pushinteger(L, y);
    result:=2;

  end else lua_pop(L, parameters);
end;

function control_setSize(L: Plua_State): integer; cdecl;
var parameters: integer;
  c: pointer;
  width,height: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    c:=lua_touserdata(L, -3);
    width:=lua_tointeger(L, -2);
    height:=lua_tointeger(L, -1);

    ce_control_setSize(c,width,height);
  end;

  lua_pop(L, lua_gettop(L));
end;

function control_getSize(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: pointer;
  width,height: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    width:=ce_control_getWidth(control);
    height:=ce_control_getHeight(control);

    lua_pushinteger(L, width);
    lua_pushinteger(L, height);
    result:=2;

  end else lua_pop(L, parameters);
end;


function control_setAlign(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: pointer;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);
    a:=lua_tointeger(L,-1);
    ce_control_setAlign(control,a);
  end;

  lua_pop(L, parameters);
end;

function control_getAlign(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  align: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, integer(control.Align));
    result:=1;

  end else lua_pop(L, parameters);
end;

function control_setEnabled(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  Enabled: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);
    control.Enabled:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;

function control_getEnabled(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, control.Enabled);
    result:=1;

  end else lua_pop(L, parameters);
end;


function control_setVisible(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  visible: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);
    control.visible:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;

function control_getVisible(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, control.Visible);
    result:=1;

  end else lua_pop(L, parameters);
end;

function control_setColor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  Color: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);
    control.Color:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

function control_getColor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, control.color);
    result:=1;

  end else lua_pop(L, parameters);
end;


function control_setParent(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  Parent: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);
    control.Parent:=TWinControl(lua_touserdata(L,-1));
  end;

  lua_pop(L, parameters);
end;

function control_getParent(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, control.Parent);
    result:=1;

  end else lua_pop(L, parameters);
end;

function control_setPopupMenu(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  PopupMenu: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);
    control.PopupMenu:=TPopupMenu(lua_touserdata(L,-1));
  end;

  lua_pop(L, parameters);
end;

function control_getPopupMenu(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, control.PopupMenu);
    result:=1;

  end else lua_pop(L, parameters);
end;

function wincontrol_getControlCount(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    wincontrol:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, wincontrol.ControlCount);
    result:=1;

  end else lua_pop(L, parameters);
end;

function wincontrol_getControl(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    wincontrol:=lua_touserdata(L,-2);
    index:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, wincontrol.Controls[index]);
    result:=1;

  end else lua_pop(L, parameters);
end;

function wincontrol_onEnter(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    wincontrol:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(wincontrol.OnEnter));
    wincontrol.OnEnter:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      wincontrol.OnEnter:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      wincontrol.OnEnter:=lc.NotifyEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function wincontrol_onExit(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    wincontrol:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(wincontrol.onExit));
    wincontrol.onExit:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      wincontrol.OnExit:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      wincontrol.OnExit:=lc.NotifyEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function wincontrol_canFocus(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    wincontrol:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, wincontrol.CanFocus);
    result:=1;

  end else lua_pop(L, parameters);
end;

function wincontrol_focused(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    wincontrol:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, wincontrol.Focused);
    result:=1;

  end else lua_pop(L, parameters);
end;

function wincontrol_setFocus(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    wincontrol:=lua_touserdata(L,-1);
    wincontrol.SetFocus;
  end;

  lua_pop(L, parameters);
end;

function strings_add(L: Plua_State): integer; cdecl;
var parameters: integer;
  strings: TStrings;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    strings:=lua_touserdata(L, -2);
    strings.Add(lua_tostring(L, -1));
  end;

  lua_pop(L, lua_gettop(L));
end;

function strings_clear(L: Plua_State): integer; cdecl;
var parameters: integer;
  strings: TStrings;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    strings:=lua_touserdata(L, -1);
    strings.Clear;
  end;

  lua_pop(L, lua_gettop(L));
end;

function strings_remove(L: Plua_State): integer; cdecl;  //compat with ce 6
var parameters: integer;
  strings: TStrings;
  s: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    strings:=lua_touserdata(L, -2);
    s:=lua_tostring(L, -1);
    ce_stringlist_remove(strings,pchar(s));
  end;

  lua_pop(L, lua_gettop(L));
end;

function strings_getString(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  strings: TStrings;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    strings:=lua_touserdata(L,-2);
    index:=lua_toInteger(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, strings[index]);
    result:=1;

  end else lua_pop(L, parameters);
end;

function strings_setString(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  strings: TStrings;
  index: integer;
  s: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    strings:=lua_touserdata(L,-3);
    index:=lua_toInteger(L,-2);
    s:=lua_tostring(l,-1);

    strings[index]:=s;
  end;
  lua_pop(L, parameters);
end;

function strings_delete(L: Plua_State): integer; cdecl;
var parameters: integer;
  strings: TStrings;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    strings:=lua_touserdata(L, -2);
    index:=lua_tointeger(L,-1);
    strings.Delete(index);
  end;

  lua_pop(L, lua_gettop(L));
end;


function strings_getText(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  strings: TStrings;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    strings:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, strings.Text);
    result:=1;

  end else lua_pop(L, parameters);
end;

function strings_setText(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  strings: TStrings;
  text: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    strings:=lua_touserdata(L,-2);
    text:=Lua_ToString(L, -1);
    lua_pop(L, parameters);

    strings.Text:=text;


  end else lua_pop(L, parameters);
end;

function strings_indexOf(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  strings: TStrings;
  s: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    strings:=lua_touserdata(L,-2);
    s:=Lua_ToString(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, strings.IndexOf(s));
    result:=1;

  end else lua_pop(L, parameters);
end;

function strings_insert(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  strings: TStrings;
  index: integer;
  s: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    strings:=lua_touserdata(L,-3);
    index:=lua_tointeger(L,-2);
    s:=Lua_ToString(L,-1);

    strings.Insert(index,s);
  end;
  lua_pop(L, parameters);
end;


function strings_getCount(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  strings: TStrings;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    strings:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, strings.Count);
    result:=1;

  end else lua_pop(L, parameters);
end;

function strings_loadFromFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  strings: TStrings;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    strings:=lua_touserdata(L, -2);
    try
      strings.LoadFromFile(lua_tostring(L, -1));
    except
    end;
  end;

  lua_pop(L, lua_gettop(L));
end;

function strings_saveToFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  strings: TStrings;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    strings:=lua_touserdata(L, -2);
    try
      strings.SaveToFile(lua_tostring(L, -1));
    except
    end;
  end;

  lua_pop(L, lua_gettop(L));
end;

function createStringlist(L: Plua_State): integer; cdecl;
var
  stringlist: TStringlist;
begin
  result:=0;
  lua_pop(L, lua_gettop(L));

  stringlist:=TStringList.Create;

  lua_pushlightuserdata(L, stringlist);
  result:=1;
end;

function stringlist_getDuplicates(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  stringlist: TStringlist;
  align: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    stringlist:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, integer(stringlist.Duplicates));
    result:=1;

  end else lua_pop(L, parameters);
end;

function stringlist_setDuplicates(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  stringlist: TStringlist;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    stringlist:=lua_touserdata(L,-2);
    stringlist.Duplicates:=TDuplicates(lua_tointeger(L,-1));
  end;

  lua_pop(L, parameters);
end;


function stringlist_getSorted(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  stringlist: TStringlist;
  align: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    stringlist:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, stringlist.Sorted);
    result:=1;

  end else lua_pop(L, parameters);
end;

function stringlist_setSorted(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  stringlist: TStringlist;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    stringlist:=lua_touserdata(L,-2);
    stringlist.Sorted:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;

function stringlist_getCaseSensitive(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  stringlist: TStringlist;
  align: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    stringlist:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, stringlist.CaseSensitive);
    result:=1;

  end else lua_pop(L, parameters);
end;

function stringlist_setCaseSensitive(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  stringlist: TStringlist;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    stringlist:=lua_touserdata(L,-2);
    stringlist.CaseSensitive:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;


function control_onClick(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(control.onClick));
    control.onClick:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      control.OnClick:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnClick:=lc.NotifyEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function object_destroy(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  component: pointer;
  x,y: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    component:=lua_touserdata(L,-1);

    ce_object_destroy(component);
  end;

  lua_pop(L, parameters);
end;

function messageDialog(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  message: pchar;
  dialogtype: integer;
  buttontype: integer;

  r: integer;

  i: integer;
  b: TMsgDlgButtons;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=3 then
  begin
    message:=lua.lua_tostring(L,-parameters);
    dialogtype:=lua_tointeger(L,-parameters+1);
    b:=[];
    for i:=-parameters+2 to -1 do
    begin
      buttontype:=lua_tointeger(L,i);
      case buttontype of
        0:  b:=b+[mbYes];
        1:  b:=b+[mbNo];
        2:  b:=b+[mbOK];
        3:  b:=b+[mbCancel];
        4:  b:=b+[mbAbort];
        5:  b:=b+[mbRetry];
        6:  b:=b+[mbIgnore];
        7:  b:=b+[mbAll];
        8:  b:=b+[mbNoToAll];
        9:  b:=b+[mbYesToAll];
        10: b:=b+[mbHelp];
        11: b:=b+[mbClose];
        else b:=b+[mbyes];
      end;
    end;
    lua_pop(L, parameters);

    r:=ce_messageDialog_lua(message, dialogtype, b);
    lua_pushinteger(L,r);
    result:=1;

  end else lua_pop(L, parameters);
end;

function speedhack_setSpeed(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  speed: single;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    speed:=lua_tonumber(L,-1);
    ce_speedhack_setSpeed(speed);
  end;
  lua_pop(L, parameters);
end;

function injectDLL(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  filename: pchar;
  r: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    filename:=lua.lua_tostring(L,-1);
    r:=false;
    try
      r:=ce_InjectDLL(filename,pchar(''));
    except
    end;

    result:=1;
    lua_pushboolean(L, r);
  end;
  lua_pop(L, parameters);
end;




function getAutoAttachList(L: Plua_State): integer; cdecl;
var f: pointer;
  parameters: integer;
  visible: boolean;
begin
  result:=1;
  lua_pop(L, lua_gettop(L));

  f:=ce_getAutoAttachList();
  lua_pushlightuserdata(L, f);
  result:=1;
end;



function generateAPIHookScript_lua(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  address: string;
  addressTo: string;
  addresstogetnewcalladdress: string;
  script: tstringlist;
begin
  address:='';
  addressTo:='';
  addresstogetnewcalladdress:='';

  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=2 then
  begin
    address:=lua_tostring(L, -parameters);
    addressTo:=lua_tostring(L, (-parameters)+1);

    if parameters=3 then
      addresstogetnewcalladdress:=lua_tostring(L, (-parameters)+2);

    lua_pop(L, lua_gettop(L));

    script:=tstringlist.create;
    try
      generateAPIHookScript(script, address, addressto, addresstogetnewcalladdress);
      lua_pushstring(L, pchar(script.text));
      result:=1;
    finally
      script.free;
    end;
  end;

end;

function createProcess(L: PLua_state): integer; cdecl;
var parameters: integer;
  path,params: string;
  debug: boolean;
  breakonentrypoint: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  path:='';
  params:='';
  debug:=false;
  breakonentrypoint:=false;

  if parameters>0 then
    path:=lua_tostring(L, -parameters);

  if parameters>1 then
    params:=lua_tostring(L, (-parameters)+1);

  if parameters>2 then
    debug:=lua_toboolean(L, (-parameters)+2);

  if parameters>3 then
    breakonentrypoint:=lua_toboolean(L, (-parameters)+3);

  if path<>'' then
    ce_createProcess(pchar(path), pchar(params), debug, breakonentrypoint);

  lua_pop(L, lua_gettop(L));
end;

function AOBScan(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  i,b: integer;
  scanstring: string;
  protectionflags: string;
  alignmentparam: string;
  alignmenttype: TFastScanMethod;
  list: tstringlist;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters=0 then exit;

  protectionflags:='';
  alignmenttype:=fsmNotAligned;
  alignmentparam:='1';

  if (parameters>=1) and (lua_isstring(L,-parameters)) then
  begin
    //it's a scanstring, optional call
    scanstring:=Lua_ToString(L, -parameters);
    if parameters>=2 then
      protectionflags:=Lua_ToString(L, -parameters+1);

    if parameters>=3 then
      alignmenttype:=TFastScanMethod(lua_tointeger(L, -parameters+2));


    if parameters>=4 then
      alignmentparam:=Lua_ToString(L, -parameters+3);


  end
  else
  begin
    //buildup the scanstring
    scanstring:='';
    for i:=-parameters to -1 do
    begin
      b:=lua_tointeger(L,i);

      if (b>255) then scanstring:=scanstring+'* '
      else
      if b=0 then
      begin
        if not lua_isnumber(L,i) then
          scanstring:=scanstring+'* '
        else
          scanstring:=scanstring+'00 '
      end
      else scanstring:=scanstring+inttohex(b,2)+' ';
    end;
  end;
  lua_pop(L, lua_gettop(L));


  list:=tstringlist.create;
  if getaoblist(scanstring, list, protectionflags, alignmenttype, alignmentparam) then
  begin
    result:=1;
    lua_pushlightuserdata(L, list);
  end
  else
    list.free;

end;




function getOpenedProcessID(L: PLua_state): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));
  result:=1;
  lua_pushinteger(L, processid);
end;

function getAddress(L: PLua_state): integer; cdecl;
var parameters: integer;
  s: string;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    s:=Lua_ToString(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushinteger(L,symhandler.getAddressFromName(s));
    result:=1;
  end
  else
  lua_pop(L, lua_gettop(l));

end;

function getNameFromAddress(L: PLua_state): integer; cdecl;
var parameters: integer;
  s: string;
  address: ptruint;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L, -1) then
      address:=symhandler.getAddressFromName(lua_tostring(L,-1))
    else
      address:=lua_tointeger(L,-1);

    lua_pop(L, lua_gettop(l));

    lua_pushstring(L,symhandler.getNameFromAddress(address));
    result:=1;
  end
  else lua_pop(L, lua_gettop(l));
end;

function inModule(L: PLua_state): integer; cdecl;
var parameters: integer;
  s: string;
  address: ptruint;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L, -1) then
      address:=symhandler.getAddressFromName(lua_tostring(L,-1))
    else
      address:=lua_tointeger(L,-1);

    lua_pop(L, lua_gettop(l));

    lua_pushboolean(L,symhandler.inModule(address));
    result:=1;
  end
  else lua_pop(L, lua_gettop(l));
end;

function inSystemModule(L: PLua_state): integer; cdecl;
var parameters: integer;
  s: string;
  address: ptruint;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L, -1) then
      address:=symhandler.getAddressFromName(lua_tostring(L,-1))
    else
      address:=lua_tointeger(L,-1);

    lua_pop(L, lua_gettop(l));

    lua_pushboolean(L,symhandler.inSystemModule(address));
    result:=1;
  end;
end;

function getCommonModuleList(L: PLua_state): integer; cdecl;
begin
  result:=0;
  lua_pop(L, lua_gettop(l));

  lua_pushlightuserdata(L,symhandler.getCommonModuleList);
  result:=1;
end;

function reinitializeSymbolhandler(L: PLua_state): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));
  result:=0;
  symhandler.reinitialize;
  symhandler.waitforsymbolsloaded;
end;

function getPropertyList(L: PLua_state): integer; cdecl;
var parameters: integer;
  c: tobject;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushlightuserdata(L, ce_getPropertylist(c));
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function getProperty(L: PLua_state): integer; cdecl;
var parameters: integer;
  c: tobject;
  p: string;
  buf: pchar;

  size: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L, -2);
    p:=Lua_ToString(L, -1);

    lua_pop(L, lua_gettop(l));

    size:=ce_getProperty(c,pchar(p),buf,0);

    if size=0 then exit; //invalid property

    getmem(buf,size);
    if ce_getProperty(c,pchar(p),buf,size)<=size then
    begin
      lua_pushstring(L, buf);
      result:=1;
    end;
    freemem(buf);


  end else lua_pop(L, lua_gettop(l));
end;

function setProperty(L: PLua_state): integer; cdecl;
var parameters: integer;
  c: tobject;
  p,v: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    c:=lua_touserdata(L, -3);
    p:=Lua_ToString(L, -2);
    v:=Lua_ToString(L, -1);

    ce_setProperty(c,pchar(p),pchar(v));
  end;

  lua_pop(L, lua_gettop(l));
end;

function object_getClassName(L: PLua_state): integer; cdecl;
var c: TObject;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushstring(L, c.ClassName);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;



function getMemoryViewForm(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  lua_pushlightuserdata(l, MemoryBrowser);
end;

function getMainForm(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  lua_pushlightuserdata(l, mainform);
end;

function memoryview_getDisassemblerView(L: PLua_state): integer; cdecl;
var m: TMemoryBrowser;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    m:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushlightuserdata(L, m.disassemblerview);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function memoryview_getHexadecimalView(L: PLua_state): integer; cdecl;
var m: TMemoryBrowser;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    m:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushlightuserdata(L, m.hexview);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;


function getAddressList(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  lua_pushlightuserdata(l, mainform.addresslist);
end;

function getFreezeTimer(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  lua_pushlightuserdata(l, mainform.FreezeTimer);
end;

function getUpdateTimer(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  lua_pushlightuserdata(l, mainform.UpdateTimer);
end;



function inheritsFromObject(L: PLua_state): integer; cdecl;
var x: TObject;
begin
  result:=0;
  if lua_gettop(L)>1 then
  begin
    x:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    if x<>nil then
    begin
      result:=1;
      lua_pushboolean(l, (x is TObject));
    end;

  end;
end;

function inheritsFromComponent(L: PLua_state): integer; cdecl;
var x: TObject;
begin
  result:=0;
  if lua_gettop(L)>1 then
  begin
    x:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    if x<>nil then
    begin
      result:=1;
      lua_pushboolean(l, (x is TComponent));
    end;

  end;
end;

function inheritsFromControl(L: PLua_state): integer; cdecl;
var x: TObject;
begin
  result:=0;
  if lua_gettop(L)>1 then
  begin
    x:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    if x<>nil then
    begin
      result:=1;
      lua_pushboolean(l, (x is TControl));
    end;

  end;
end;

function inheritsFromWinControl(L: PLua_state): integer; cdecl;
var x: TObject;
begin
  result:=0;
  if lua_gettop(L)>1 then
  begin
    x:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    if x<>nil then
    begin
      result:=1;
      lua_pushboolean(l, (x is TWinControl));
    end;

  end;
end;



function component_getComponentCount(L: PLua_state): integer; cdecl;
var c: TComponent;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushinteger(L, c.ComponentCount);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function component_findComponentByName(L: PLua_state): integer; cdecl;
var c: TComponent;
  n: string;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L, -2);
    n:=Lua_ToString(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushlightuserdata(L, c.FindComponent(n));
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function component_getComponent(L: PLua_state): integer; cdecl;
var c: TComponent;
  i: integer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L, -2);
    i:=lua_tointeger(L,-1);
    lua_pop(L, lua_gettop(l));

    lua_pushlightuserdata(L, c.Components[i]);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function component_getName(L: PLua_state): integer; cdecl;
var c: TComponent;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushstring(L, c.Name);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function component_setName(L: PLua_state): integer; cdecl;
var c: TComponent;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L, -2);
    c.Name:=lua_tostring(L,-1);
  end;
  lua_pop(L, lua_gettop(l));
end;

function component_getTag(L: PLua_state): integer; cdecl;
var c: TComponent;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushinteger(L, c.Tag);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function component_setTag(L: PLua_state): integer; cdecl;
var c: TComponent;
  t: integer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L, -2);
    c.Tag:=lua_tointeger(L, -1);
  end;
  lua_pop(L, lua_gettop(l));
end;


function component_getOwner(L: PLua_state): integer; cdecl;
var c: TComponent;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushlightuserdata(L, c.Owner);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function panel_getAlignment(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  panel: Tcustompanel;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    panel:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, integer(panel.Alignment));
    result:=1;

  end else lua_pop(L, parameters);
end;

function panel_setAlignment(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  panel: Tcustompanel;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    panel:=lua_touserdata(L,-2);
    panel.Alignment:=TAlignment(lua_tointeger(L,-1));
  end;

  lua_pop(L, parameters);
end;

function panel_getBevelInner(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  panel: Tcustompanel;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    panel:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, integer(panel.BevelInner));
    result:=1;

  end else lua_pop(L, parameters);
end;

function panel_setBevelInner(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  panel: Tcustompanel;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    panel:=lua_touserdata(L,-2);
    panel.BevelInner:=TPanelBevel(lua_tointeger(L,-1));
  end;

  lua_pop(L, parameters);
end;

function panel_getBevelOuter(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  panel: Tcustompanel;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    panel:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, integer(panel.BevelOuter));
    result:=1;

  end else lua_pop(L, parameters);
end;

function panel_setBevelOuter(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  panel: Tcustompanel;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    panel:=lua_touserdata(L,-2);
    panel.BevelOuter:=TPanelBevel(lua_tointeger(L,-1));
  end;

  lua_pop(L, parameters);
end;

function panel_getBevelWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  panel: Tcustompanel;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    panel:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, panel.BevelWidth);
    result:=1;

  end else lua_pop(L, parameters);
end;

function panel_setBevelWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  panel: Tcustompanel;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    panel:=lua_touserdata(L,-2);
    panel.BevelWidth:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

function panel_getFullRepaint(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  panel: Tcustompanel;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    panel:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, panel.FullRepaint);
    result:=1;

  end else lua_pop(L, parameters);
end;

function panel_setFullRepaint(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  panel: Tcustompanel;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    panel:=lua_touserdata(L,-2);
    panel.FullRepaint:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;

function edit_clear(L: Plua_State): integer; cdecl;
var parameters: integer;
  e: tcustomedit;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    e:=lua_touserdata(L, -1);
    e.clear;
  end;
  lua_pop(L, lua_gettop(L));
end;

function edit_selectAll(L: Plua_State): integer; cdecl;
var parameters: integer;
  e: tcustomedit;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    e:=lua_touserdata(L, -1);
    e.SelectAll;
  end;
  lua_pop(L, lua_gettop(L));
end;

function edit_clearSelection(L: Plua_State): integer; cdecl;
var parameters: integer;
  e: tcustomedit;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    e:=lua_touserdata(L, -1);
    e.ClearSelection;
  end;
  lua_pop(L, lua_gettop(L));
end;

function edit_copyToClipboard(L: Plua_State): integer; cdecl;
var parameters: integer;
  e: tcustomedit;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    e:=lua_touserdata(L, -1);
    e.CopyToClipboard;
  end;
  lua_pop(L, lua_gettop(L));
end;

function edit_cutToClipboard(L: Plua_State): integer; cdecl;
var parameters: integer;
  e: tcustomedit;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    e:=lua_touserdata(L, -1);
    e.CutToClipboard;
  end;
  lua_pop(L, lua_gettop(L));
end;

function edit_pasteFromClipboard(L: Plua_State): integer; cdecl;
var parameters: integer;
  e: tcustomedit;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    e:=lua_touserdata(L, -1);
    e.PasteFromClipboard;
  end;
  lua_pop(L, lua_gettop(L));
end;

function edit_onChange(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TCustomEdit;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(control.onChange));
    control.onChange:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      control.OnChange:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnChange:=lc.NotifyEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function memo_append(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memo: TCustomMemo;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memo:=lua_touserdata(L,-2);
    memo.Append(Lua_ToString(L,-1));
  end;

  lua_pop(L, parameters);
end;

function memo_getLines(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memo: TCustomMemo;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memo:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, memo.Lines);
    result:=1;

  end else lua_pop(L, parameters);
end;

function memo_getWordWrap(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memo: Tcustommemo;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memo:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, memo.WordWrap);
    result:=1;

  end else lua_pop(L, parameters);
end;

function memo_setWordWrap(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memo: Tcustommemo;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memo:=lua_touserdata(L,-2);
    memo.WordWrap:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;

function memo_getWantTabs(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memo: Tcustommemo;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memo:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, memo.WantTabs);
    result:=1;

  end else lua_pop(L, parameters);
end;

function memo_setWantTabs(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memo: Tcustommemo;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memo:=lua_touserdata(L,-2);
    memo.WantTabs:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;

function memo_getWantReturns(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memo: Tcustommemo;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memo:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, memo.WantReturns);
    result:=1;

  end else lua_pop(L, parameters);
end;

function memo_setWantReturns(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memo: Tcustommemo;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memo:=lua_touserdata(L,-2);
    memo.WantReturns:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;

function memo_getScrollbars(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memo: Tcustommemo;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memo:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, integer(memo.Scrollbars));
    result:=1;

  end else lua_pop(L, parameters);
end;

function memo_setScrollbars(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memo: Tcustommemo;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memo:=lua_touserdata(L,-2);
    memo.Scrollbars:=TScrollStyle(lua_tointeger(L,-1));
  end;

  lua_pop(L, parameters);
end;

function button_getModalResult(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  button: Tcustombutton;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    button:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, integer(button.ModalResult));
    result:=1;

  end else lua_pop(L, parameters);
end;

function button_setModalResult(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  button: Tcustombutton;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    button:=lua_touserdata(L,-2);
    button.ModalResult:=TModalResult(lua_tointeger(L,-1));
  end;

  lua_pop(L, parameters);
end;



function createToggleBox(L: Plua_State): integer; cdecl;
var
  ToggleBox: TCEToggleBox;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_touserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  ToggleBox:=TCEToggleBox.Create(owner);
  if owner<>nil then
    ToggleBox.Parent:=owner;

  lua_pushlightuserdata(L, ToggleBox);
  result:=1;
end;

function createCheckBox(L: Plua_State): integer; cdecl;
var
  CheckBox: TCECheckBox;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_touserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  CheckBox:=TCECheckBox.Create(owner);
  if owner<>nil then
    CheckBox.Parent:=owner;

  lua_pushlightuserdata(L, CheckBox);
  result:=1;
end;

function checkbox_getAllowGrayed(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  checkbox: Tcustomcheckbox;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    checkbox:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, checkbox.AllowGrayed);
    result:=1;

  end else lua_pop(L, parameters);
end;

function checkbox_setAllowGrayed(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  checkbox: Tcustomcheckbox;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    checkbox:=lua_touserdata(L,-2);
    checkbox.AllowGrayed:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;

function checkbox_getState(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  checkbox: Tcustomcheckbox;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    checkbox:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, integer(checkbox.State));
    result:=1;

  end else lua_pop(L, parameters);
end;

function checkbox_setState(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  checkbox: Tcustomcheckbox;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    checkbox:=lua_touserdata(L,-2);
    checkbox.State:=TCheckBoxState(lua_tointeger(L,-1));
  end;

  lua_pop(L, parameters);
end;

function checkbox_onChange(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TCustomCheckBox;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(control.onChange));
    control.onChange:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      control.OnChange:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnChange:=lc.NotifyEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function createRadioGroup(L: Plua_State): integer; cdecl;
var
  RadioGroup: TCERadioGroup;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_touserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  RadioGroup:=TCERadioGroup.Create(owner);
  if owner<>nil then
    RadioGroup.Parent:=owner;

  lua_pushlightuserdata(L, RadioGroup);
  result:=1;
end;

function radiogroup_getRows(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  radiogroup: TCustomRadioGroup;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    radiogroup:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, radiogroup.Rows);
    result:=1;

  end else lua_pop(L, parameters);
end;

function radiogroup_getItems(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  radiogroup: TCustomRadioGroup;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    radiogroup:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, radiogroup.items);
    result:=1;

  end else lua_pop(L, parameters);
end;

function radiogroup_getColumns(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  radiogroup: Tcustomradiogroup;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    radiogroup:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, radiogroup.Columns);
    result:=1;

  end else lua_pop(L, parameters);
end;

function radiogroup_setColumns(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  radiogroup: Tcustomradiogroup;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    radiogroup:=lua_touserdata(L,-2);
    radiogroup.Columns:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

function radiogroup_onClick(L: PLua_State): integer; cdecl; //for some reason the radiogroup has it's own fonclick variable
var
  parameters: integer;
  control: TCustomRadioGroup;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(control.onClick));
    control.onClick:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      control.OnClick:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnClick:=lc.NotifyEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function createListBox(L: Plua_State): integer; cdecl;
var
  ListBox: TCEListBox;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_touserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  ListBox:=TCEListBox.Create(owner);
  if owner<>nil then
    ListBox.Parent:=owner;

  lua_pushlightuserdata(L, ListBox);
  result:=1;
end;

function listbox_clear(L: Plua_State): integer; cdecl;
var parameters: integer;
  listbox: tcustomlistbox;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listbox:=lua_touserdata(L, -1);
    listbox.clear;
  end;
  lua_pop(L, lua_gettop(L));
end;


function listbox_getItems(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listbox: TCustomlistbox;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listbox:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, listbox.items);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listbox_getItemIndex(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listbox: Tcustomlistbox;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listbox:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, listbox.ItemIndex);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listbox_setItemIndex(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listbox: Tcustomlistbox;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    listbox:=lua_touserdata(L,-2);
    listbox.itemindex:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

//combobox
function createComboBox(L: Plua_State): integer; cdecl;
var
  ComboBox: TCEComboBox;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_touserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  ComboBox:=TCEComboBox.Create(owner);
  if owner<>nil then
    ComboBox.Parent:=owner;

  lua_pushlightuserdata(L, ComboBox);
  result:=1;
end;

function combobox_clear(L: Plua_State): integer; cdecl;
var parameters: integer;
  combobox: tcustomcombobox;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    combobox:=lua_touserdata(L, -1);
    combobox.clear;
  end;
  lua_pop(L, lua_gettop(L));
end;


function combobox_getItems(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  combobox: TCustomcombobox;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    combobox:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, combobox.items);
    result:=1;

  end else lua_pop(L, parameters);
end;

function combobox_getItemIndex(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  combobox: Tcustomcombobox;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    combobox:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, combobox.ItemIndex);
    result:=1;

  end else lua_pop(L, parameters);
end;

function combobox_setItemIndex(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  combobox: Tcustomcombobox;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    combobox:=lua_touserdata(L,-2);
    combobox.itemindex:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;


function createProgressBar(L: Plua_State): integer; cdecl;
var
  ProgressBar: TCEProgressBar;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_touserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  ProgressBar:=TCEProgressBar.Create(owner);
  if owner<>nil then
    ProgressBar.Parent:=owner;

  lua_pushlightuserdata(L, ProgressBar);
  result:=1;
end;

function progressbar_stepIt(L: Plua_State): integer; cdecl;
var parameters: integer;
    progressbar: TCustomProgressBar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    progressbar:=lua_touserdata(L, -1);
    progressbar.StepIt;
  end;
  lua_pop(L, lua_gettop(L));
end;

function progressbar_stepBy(L: Plua_State): integer; cdecl;
var parameters: integer;
    progressbar: TCustomProgressBar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    progressbar:=lua_touserdata(L, -2);
    progressbar.StepBy(lua_tointeger(L, -1));
  end;
  lua_pop(L, lua_gettop(L));
end;


function progressbar_getMax(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  progressbar: Tcustomprogressbar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    progressbar:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, progressbar.max);
    result:=1;

  end else lua_pop(L, parameters);
end;

function progressbar_setMax(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  progressbar: Tcustomprogressbar;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    progressbar:=lua_touserdata(L,-2);
    progressbar.max:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

function progressbar_getMin(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  progressbar: Tcustomprogressbar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    progressbar:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, progressbar.Min);
    result:=1;

  end else lua_pop(L, parameters);
end;

function progressbar_setMin(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  progressbar: Tcustomprogressbar;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    progressbar:=lua_touserdata(L,-2);
    progressbar.Min:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;


function progressbar_getPosition(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  progressbar: Tcustomprogressbar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    progressbar:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, progressbar.Position);
    result:=1;

  end else lua_pop(L, parameters);
end;

function progressbar_setPosition(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  progressbar: Tcustomprogressbar;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    progressbar:=lua_touserdata(L,-2);
    progressbar.Position:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

//trackbar
function createTrackBar(L: Plua_State): integer; cdecl;
var
  TrackBar: TCETrackBar;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_touserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  TrackBar:=TCETrackBar.Create(owner);
  if owner<>nil then
    TrackBar.Parent:=owner;

  lua_pushlightuserdata(L, TrackBar);
  result:=1;
end;

function trackbar_getMax(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  trackbar: Tcustomtrackbar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    trackbar:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, trackbar.max);
    result:=1;

  end else lua_pop(L, parameters);
end;

function trackbar_setMax(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  trackbar: Tcustomtrackbar;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    trackbar:=lua_touserdata(L,-2);
    trackbar.max:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

function trackbar_getMin(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  trackbar: Tcustomtrackbar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    trackbar:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, trackbar.Min);
    result:=1;

  end else lua_pop(L, parameters);
end;

function trackbar_setMin(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  trackbar: Tcustomtrackbar;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    trackbar:=lua_touserdata(L,-2);
    trackbar.Min:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;


function trackbar_getPosition(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  trackbar: Tcustomtrackbar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    trackbar:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, trackbar.Position);
    result:=1;

  end else lua_pop(L, parameters);
end;

function trackbar_setPosition(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  trackbar: Tcustomtrackbar;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    trackbar:=lua_touserdata(L,-2);
    trackbar.Position:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

function trackbar_onChange(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TCustomTrackBar;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(control.onChange));
    control.onChange:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      control.OnChange:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnChange:=lc.NotifyEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function listcolumn_setAutosize(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listcolumns: TListColumn;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    listcolumns:=lua_touserdata(L,-2);
    listcolumns.AutoSize:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;


function listcolumn_getCaption(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listcolumn: Tlistcolumn;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listcolumn:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, listcolumn.caption);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listcolumn_setCaption(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listcolumn: Tlistcolumn;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    listcolumn:=lua_touserdata(L,-2);
    listcolumn.caption:=Lua_ToString(L,-1);
  end;

  lua_pop(L, parameters);
end;

function listcolumn_getMaxWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listcolumn: Tlistcolumn;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listcolumn:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, listcolumn.maxwidth);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listcolumn_setMaxWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listcolumn: Tlistcolumn;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    listcolumn:=lua_touserdata(L,-2);
    listcolumn.maxwidth:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

function listcolumn_getMinWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listcolumn: Tlistcolumn;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listcolumn:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, listcolumn.Minwidth);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listcolumn_setMinWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listcolumn: Tlistcolumn;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    listcolumn:=lua_touserdata(L,-2);
    listcolumn.Minwidth:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

function listcolumn_getWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listcolumn: Tlistcolumn;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listcolumn:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, listcolumn.width);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listcolumn_setWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listcolumn: Tlistcolumn;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    listcolumn:=lua_touserdata(L,-2);
    listcolumn.width:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

function collection_clear(L: Plua_State): integer; cdecl;
var parameters: integer;
  collection: TCollection;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    collection:=lua_touserdata(L, -1);
    collection.clear;
  end;
  lua_pop(L, lua_gettop(L));
end;

function collection_getCount(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  collection: Tcollection;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    collection:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, collection.Count);
    result:=1;

  end else lua_pop(L, parameters);
end;

function collection_delete(L: Plua_State): integer; cdecl;
var parameters: integer;
  collection: Tcollection;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    collection:=lua_touserdata(L, -2);
    index:=lua_tointeger(L,-1);
    collection.Delete(index);
  end;

  lua_pop(L, lua_gettop(L));
end;

function listcolumns_add(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listcolumns: TListColumns;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listcolumns:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, listcolumns.Add);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listcolumns_getColumn(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listcolumns: TListcolumns;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    listcolumns:=lua_touserdata(L,-2);
    index:=lua_toInteger(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, listcolumns[index]);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listitem_delete(L: Plua_State): integer; cdecl;
var parameters: integer;
  listitem: Tlistitem;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listitem:=lua_touserdata(L, -1);
    listitem.Delete;
  end;

  lua_pop(L, lua_gettop(L));
end;

function listitem_getCaption(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listitem: Tlistitem;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listitem:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, listitem.caption);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listitem_setCaption(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listitem: Tlistitem;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    listitem:=lua_touserdata(L,-2);
    listitem.Caption:=Lua_ToString(L,-1);
  end;

  lua_pop(L, parameters);
end;

function listitem_getSubItems(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listitem: Tlistitem;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listitem:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, listitem.SubItems);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listitems_clear(L: Plua_State): integer; cdecl;
var parameters: integer;
  listitems: Tlistitems;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listitems:=lua_touserdata(L, -1);
    listitems.clear;
  end;
  lua_pop(L, lua_gettop(L));
end;

function listitems_getCount(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listitems: Tlistitems;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listitems:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, listitems.Count);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listitems_add(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listitems: Tlistitems;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listitems:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, listitems.Add);
    result:=1;

  end else lua_pop(L, parameters);
end;


//listview
function createListView(L: Plua_State): integer; cdecl;
var
  ListView: TCEListView;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_touserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  ListView:=TCEListView.Create(owner);
  if owner<>nil then
    ListView.Parent:=owner;

  lua_pushlightuserdata(L, ListView);
  result:=1;
end;

function listview_clear(L: Plua_State): integer; cdecl;
var parameters: integer;
  listview: Tcustomlistview;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listview:=lua_touserdata(L, -1);
    listview.Clear;
  end;
  lua_pop(L, lua_gettop(L));
end;

function listview_getColumns(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listview: TCEListView;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listview:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, listview.Columns);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listview_getItems(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listview: TCustomListView;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listview:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, listview.Items);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listview_getItemIndex(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listview: Tcustomlistview;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    listview:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, listview.ItemIndex);
    result:=1;

  end else lua_pop(L, parameters);
end;

function listview_setItemIndex(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  listview: Tcustomlistview;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    listview:=lua_touserdata(L,-2);
    listview.itemindex:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

function opendialog_execute(L: Plua_State): integer; cdecl;
var parameters: integer;
  opendialog: TOpenDialog;
  r: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    opendialog:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(L));

    if opendialog.Execute then
      r:=opendialog.filename
    else
      r:='';

    lua_pushstring(L, r);
    result:=1;

  end
  else
    lua_pop(L, lua_gettop(L));
end;


function findTableFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: string;
  i: integer;

  s: tmemorystream;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=Lua_ToString(L, -1);
    lua_pop(L, lua_gettop(L));
    for i:=0 to mainform.LuaFiles.count-1 do
      if TLuafile(mainform.Luafiles[i]).name=f then
      begin
        s:=TLuafile(mainform.Luafiles[i]).stream;
        lua_pushlightuserdata(L, TLuafile(s));
        result:=1;
      end;

  end
  else
    lua_pop(L, lua_gettop(L));
end;


function tablefile_saveToFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  lf: TLuaFile;
  f: string;
  i: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    lf:=lua_touserdata(L, -2);
    f:=Lua_ToString(L, -1);
    lf.stream.SaveToFile(f);
  end;

  lua_pop(L, lua_gettop(L));
end;

function tablefile_getData(L: Plua_State): integer; cdecl;
var parameters: integer;
  lf: TLuaFile;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    lf:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(L));
    lua_pushlightuserdata(L, lf.stream);
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function xmplayer_playXM(L: Plua_State): integer; cdecl;
var parameters: integer;
  lf: TLuaFile;
  f: string;
  i: integer;
begin
  if xmplayer=nil then
    xmplayer:=TXMPlayer.create;

  result:=0;
  parameters:=lua_gettop(L);
  if (xmplayer<>nil) and (parameters=1) then
  begin
    if lua_islightuserdata(L,-1) then //stream
      xmplayer.playXM(tstream(lua_touserdata(L,-1)))
    else
      xmplayer.playXM(Lua_ToString(L,-1))
  end;

  lua_pop(L, lua_gettop(L));
end;

function xmplayer_pause(L: Plua_State): integer; cdecl;
var parameters: integer;
begin
  if xmplayer=nil then
    xmplayer:=TXMPlayer.create;

  result:=0;
  lua_pop(L, lua_gettop(L));

  if xmplayer<>nil then
     xmplayer.pause;
end;

function xmplayer_resume(L: Plua_State): integer; cdecl;
var parameters: integer;
begin
  if xmplayer=nil then
    xmplayer:=TXMPlayer.create;

  result:=0;
  lua_pop(L, lua_gettop(L));

  if xmplayer<>nil then
     xmplayer.resume;
end;

function xmplayer_stop(L: Plua_State): integer; cdecl;
var parameters: integer;
begin
  if xmplayer=nil then
    xmplayer:=TXMPlayer.create;

  result:=0;
  lua_pop(L, lua_gettop(L));

  if xmplayer<>nil then
     xmplayer.stop;
end;

function xmplayer_isPlaying(L: Plua_State): integer; cdecl;
var parameters: integer;
begin
  if xmplayer=nil then
    xmplayer:=TXMPlayer.create;

  result:=0;
  lua_pop(L, lua_gettop(L));

  if xmplayer<>nil then
  begin
    result:=1;
    lua_pushboolean(L, xmplayer.isPlaying);
  end;
end;

function readRegionFromFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  filename: string;
  address: ptruint;
  size: integer;
  x: dword;
  buf: pointer;
  f: tmemorystream;
begin
  result:=0;
  x:=0;
  f:=nil;

  parameters:=lua_gettop(L);
  if (parameters=2) then
  begin
    filename:=Lua_ToString(L, -2);
    if lua_isstring(L, -1) then
      address:=symhandler.getAddressFromName(lua_tostring(L,-1))
    else
      address:=lua_tointeger(L,-1);

    lua_pop(L, lua_gettop(L));

    f:=tmemorystream.create;
    try
      f.LoadFromFile(filename);
      writeprocessmemory(processhandle, pointer(address), f.memory, f.size, x);

    finally
      freemem(f);
    end;


    result:=1;
    lua_pushinteger(L,x);
  end else lua_pop(L, lua_gettop(L));
end;


function writeRegionToFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  filename: string;
  address: ptruint;
  size: integer;
  x: dword;
  buf: pointer;
  f: tfilestream;
begin
  result:=0;
  x:=0;
  f:=nil;

  parameters:=lua_gettop(L);
  if (parameters=3) then
  begin
    filename:=Lua_ToString(L, -3);
    if lua_isstring(L, -2) then
      address:=symhandler.getAddressFromName(lua_tostring(L,-2))
    else
      address:=lua_tointeger(L,-2);

    size:=lua_tointeger(L,-1);
    lua_pop(L, lua_gettop(L));

    getmem(buf,size);
    try

      readprocessmemory(processhandle, pointer(address), buf, size, x);

      f:=tfilestream.create(filename, fmCreate);
      f.WriteBuffer(buf^, x);

    finally
      if f<>nil then
        freemem(f);

      freemem(buf);
    end;


    result:=1;
    lua_pushinteger(L,x);
  end else lua_pop(L, lua_gettop(L));
end;


function registerSymbol(L: Plua_State): integer; cdecl;
var parameters: integer;
  symbolname: string;
  address: string;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if (parameters=2) then
  begin
    symbolname:=Lua_ToString(L, -2);
    if lua_isstring(L, -1) then
      address:=lua_tostring(L,-1)
    else
      address:=IntToHex(lua_tointeger(L,-1),1);

    symhandler.AddUserdefinedSymbol(address, symbolname);
  end;

  lua_pop(L, lua_gettop(L));
end;

function unregisterSymbol(L: Plua_State): integer; cdecl;
var parameters: integer;
  symbolname: string;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if (parameters=1) then
  begin
    symbolname:=Lua_ToString(L, -1);
    symhandler.DeleteUserdefinedSymbol(symbolname);
  end;

  lua_pop(L, lua_gettop(L));
end;

function resetLuaState(L: Plua_State): integer; cdecl;
begin
  result:=0;
  lua_pop(L, lua_gettop(L));
  InitializeLua; //this creates a NEW lua state (cut doesn't destroy the current one)

end;

function cheatcomponent_getActive(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    cheatcomponent:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, cheatcomponent.activated);
    result:=1;

  end else lua_pop(L, parameters);
end;


function cheatcomponent_setActive(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=2 then
  begin
    cheatcomponent:=lua_touserdata(L,-parameters);
    cheatcomponent.activated:=lua_toboolean(L,-parameters+1);

    if parameters=3 then
    begin
      deactivatetime:=lua_tointeger(L,-parameters+2);
      if cheatcomponent.activated then
        cheatcomponent.setDeactivateTimer(deactivatetime);

    end;
  end;


  lua_pop(L, parameters);
end;

function cheatcomponent_getDescription(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    cheatcomponent:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, cheatcomponent.Description);
    result:=1;

  end else lua_pop(L, parameters);
end;


function cheatcomponent_setDescription(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    cheatcomponent:=lua_touserdata(L,-2);
    cheatcomponent.Description:=Lua_ToString(L,-1);
  end;
  lua_pop(L, parameters);
end;

function cheatcomponent_getHotkey(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    cheatcomponent:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, cheatcomponent.Hotkey);
    result:=1;

  end else lua_pop(L, parameters);
end;


function cheatcomponent_setHotkey(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    cheatcomponent:=lua_touserdata(L,-2);
    cheatcomponent.Hotkey:=Lua_ToString(L,-1);
  end;
  lua_pop(L, parameters);
end;

function cheatcomponent_getDescriptionLeft(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    cheatcomponent:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, cheatcomponent.DescriptionLeft);
    result:=1;

  end else lua_pop(L, parameters);
end;


function cheatcomponent_setDescriptionLeft(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    cheatcomponent:=lua_touserdata(L,-2);
    cheatcomponent.DescriptionLeft:=lua_tointeger(L,-1);
  end;
  lua_pop(L, parameters);
end;

function cheatcomponent_getHotkeyLeft(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    cheatcomponent:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, cheatcomponent.HotkeyLeft);
    result:=1;

  end else lua_pop(L, parameters);
end;


function cheatcomponent_setHotkeyLeft(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    cheatcomponent:=lua_touserdata(L,-2);
    cheatcomponent.HotkeyLeft:=lua_tointeger(L,-1);
  end;
  lua_pop(L, parameters);
end;

function cheatcomponent_getEditValue(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    cheatcomponent:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, cheatcomponent.EditValue);
    result:=1;

  end else lua_pop(L, parameters);
end;


function cheatcomponent_setEditValue(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    cheatcomponent:=lua_touserdata(L,-2);
    cheatcomponent.EditValue:=Lua_ToString(L,-1);
  end;
  lua_pop(L, parameters);
end;


function memoryrecordhotkey_getDescription(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecordhotkey: TMemoryRecordHotkey;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memoryrecordhotkey:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, memoryrecordhotkey.description);
    result:=1;

  end else lua_pop(L, parameters);
end;

function memoryrecordhotkey_getHotkeyString(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecordhotkey: TMemoryRecordHotkey;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memoryrecordhotkey:=lua_touserdata(L,-1);
    lua_pop(L, parameters);


    lua_pushstring(L, ConvertKeyComboToString(memoryrecordhotkey.keys));
    result:=1;

  end else lua_pop(L, parameters);
end;

function memoryrecordhotkey_getID(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecordhotkey: TMemoryRecordHotkey;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memoryrecordhotkey:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, memoryrecordhotkey.id);
    result:=1;

  end else lua_pop(L, parameters);
end;

function memoryrecordhotkey_onHotkey(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecordhotkey: Tmemoryrecordhotkey;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memoryrecordhotkey:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(memoryrecordhotkey.onHotkey));
    memoryrecordhotkey.onHotkey:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      memoryrecordhotkey.onHotkey:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      memoryrecordhotkey.onHotkey:=lc.NotifyEvent;
    end;

  end;

  lua_pop(L, parameters);
end;


function memoryrecordhotkey_onPostHotkey(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecordhotkey: Tmemoryrecordhotkey;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memoryrecordhotkey:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(memoryrecordhotkey.onPostHotkey));
    memoryrecordhotkey.onPostHotkey:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      memoryrecordhotkey.onPostHotkey:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      memoryrecordhotkey.onPostHotkey:=lc.NotifyEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function memoryrecordhotkey_getOwner(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecordhotkey: TMemoryRecordHotkey;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memoryrecordhotkey:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, memoryrecordhotkey.owner);
    result:=1;

  end else lua_pop(L, parameters);
end;

function memoryrecordhotkey_doHotkey(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecordhotkey: TMemoryRecordHotkey;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memoryrecordhotkey:=lua_touserdata(L,-1);
    memoryrecordhotkey.doHotkey;
  end;
  lua_pop(L, parameters);
end;

function addresslist_getCount(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  addresslist: TAddresslist;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    addresslist:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, addresslist.Count);
    result:=1;

  end else lua_pop(L, parameters);
end;

function addresslist_getMemoryRecord(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  addresslist: TAddresslist;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    addresslist:=lua_touserdata(L,-2);
    index:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, addresslist.MemRecItems[index]);
    result:=1;

  end else lua_pop(L, parameters);
end;

function addresslist_getMemoryRecordByDescription(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  addresslist: TAddresslist;
  description: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    addresslist:=lua_touserdata(L,-2);
    description:=Lua_ToString(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, addresslist.getRecordWithDescription(description));
    result:=1;

  end else lua_pop(L, parameters);
end;

function addresslist_getMemoryRecordByID(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  addresslist: TAddresslist;
  id: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    addresslist:=lua_touserdata(L,-2);
    id:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, addresslist.getRecordWithID(id));
    result:=1;

  end else lua_pop(L, parameters);
end;

function addresslist_createMemoryRecord(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  addresslist: TAddresslist;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    addresslist:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L,   MainForm.addresslist.addaddress(rsPluginAddress, '0', [], 0, vtDword));
    result:=1;

  end else lua_pop(L, parameters);
end;


function createMemScan(L: Plua_State): integer; cdecl;
var
  progressbar: TCustomProgressbar;
  memscan: TMemScan;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
    progressbar:=lua_touserdata(L, -1);

  lua_pop(L, lua_gettop(L));

  memscan:=TMemscan.create(progressbar);

  lua_pushlightuserdata(L, memscan);
  result:=1;
end;

function getCurrentMemscan(L: Plua_State): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));

  lua_pushlightuserdata(L, mainform.memscan);
  result:=1;
end;



//memscan_firstScan(memscan, scanOption, vartype, roundingtype, input1, input2, startAddress,
//                  stopAddress, protectionflags, alignmenttype, "alignmentparam", isHexadecimalInput,
//                  isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan);
function memscan_firstScan(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  memscan: Tmemscan;
  scanOption: TScanOption;
  vartype: TVariableType;
  roundingtype: TRoundingType;
  input1: string;
  input2: string;
  startaddress: ptruint;
  stopaddress: ptruint;
  protectionflags: string;
  alignmenttype: TFastScanMethod;
  alignmentparam: String;
  isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=15 then
  begin
    memscan:=lua_touserdata(L, -parameters);
    scanOption:=TScanOption(lua_tointeger(L, -parameters+1));
    vartype:=TVariableType(lua_tointeger(L, -parameters+2));
    roundingtype:=TRoundingType(lua_tointeger(L, -parameters+3));
    input1:=Lua_ToString(L, -parameters+4);
    input2:=Lua_ToString(L, -parameters+5);

    if lua_isstring(L, -parameters+6) then
      startaddress:=symhandler.getAddressFromName(Lua_ToString(L, -parameters+6))
    else
      startaddress:=lua_tointeger(L, -parameters+6);

    if lua_isstring(L, -parameters+7) then
      stopaddress:=symhandler.getAddressFromName(Lua_ToString(L, -parameters+7))
    else
      stopaddress:=lua_tointeger(L, -parameters+7);

    protectionflags:=Lua_ToString(L, -parameters+8);
    alignmenttype:=TFastScanMethod(lua_tointeger(L, -parameters+9));
    alignmentparam:=lua_tostring(L, -parameters+10);

    isHexadecimalInput:=lua_toboolean(L, -parameters+11);
    isNotABinaryString:=lua_toboolean(L, -parameters+12);
    isunicodescan:=lua_toboolean(L, -parameters+13);
    iscasesensitive:=lua_toboolean(L, -parameters+14);

    lua_pop(L, lua_gettop(L));
    memscan.firstscan(scanoption, vartype, roundingtype, input1,input2, startaddress,stopaddress, isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, alignmenttype, alignmentparam, nil );
  end
  else
    lua_pop(L, lua_gettop(L));


end;

function memscan_nextScan(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  memscan: Tmemscan;
  scanOption: TScanOption;
  roundingtype: TRoundingType;
  input1: string;
  input2: string;
  isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan: boolean;
  savedscanname: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=10 then
  begin
    memscan:=lua_touserdata(L, -parameters);
    scanOption:=TScanOption(lua_tointeger(L, -parameters+1));
    roundingtype:=TRoundingType(lua_tointeger(L, -parameters+2));
    input1:=Lua_ToString(L, -parameters+3);
    input2:=Lua_ToString(L, -parameters+4);

    isHexadecimalInput:=lua_toboolean(L, -parameters+5);
    isNotABinaryString:=lua_toboolean(L, -parameters+6);
    isunicodescan:=lua_toboolean(L, -parameters+7);
    iscasesensitive:=lua_toboolean(L, -parameters+8);
    ispercentagescan:=lua_toboolean(L, -parameters+9);

    if parameters=11 then
      savedscanname:=Lua_ToString(L, -parameters+10)
    else
      savedscanname:='';

    lua_pop(L, lua_gettop(L));

    memscan.nextscan(scanoption, roundingtype, input1,input2, isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan, savedscanname<>'', savedscanname );
  end else lua_pop(L, lua_gettop(L));
end;

function memscan_waitTillDone(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  memscan: Tmemscan;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memscan:=lua_touserdata(L, -parameters);
    lua_pop(L, lua_gettop(L));

    memscan.waittilldone;
  end else lua_pop(L, lua_gettop(L));
end;

function memscan_getAttachedFoundlist(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  memscan: Tmemscan;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memscan:=lua_touserdata(L, -parameters);
    lua_pop(L, lua_gettop(L));

    result:=1;
    lua_pushlightuserdata(L,memscan.attachedFoundlist);
  end else lua_pop(L, lua_gettop(L));
end;


function memscan_saveCurrentResults(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  memscan: Tmemscan;
  name: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memscan:=lua_touserdata(L, -parameters);
    name:=Lua_ToString(L, -parameters+1);
    lua_pop(L, lua_gettop(L));

    memscan.saveresults(name);
  end else lua_pop(L, lua_gettop(L));
end;

function createFoundList(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  foundlist: TFoundlist;
  memscan: TMemScan;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
    memscan:=lua_touserdata(L, -1);

  lua_pop(L, lua_gettop(L));

  foundlist:=TFoundList.create(nil, memscan);

  lua_pushlightuserdata(L, foundlist);
  result:=1;
end;

function foundlist_initialize(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  foundlist: Tfoundlist;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    foundlist:=lua_touserdata(L, -parameters);
    lua_pop(L, lua_gettop(L));

    foundlist.Initialize;
  end else lua_pop(L, lua_gettop(L));
end;

function foundlist_deinitialize(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  foundlist: Tfoundlist;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    foundlist:=lua_touserdata(L, -parameters);
    lua_pop(L, lua_gettop(L));

    foundlist.deinitialize;
  end else lua_pop(L, lua_gettop(L));
end;

function foundlist_getCount(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  foundlist: Tfoundlist;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    foundlist:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, foundlist.Count);
    result:=1;
  end else lua_pop(L, parameters);
end;

function foundlist_getAddress(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  foundlist: Tfoundlist;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    foundlist:=lua_touserdata(L,-2);
    index:=lua_tointeger(L,-1);
    lua_pop(L, parameters);


    lua_pushstring(L, inttohex(foundlist.GetAddress(index),8));
    result:=1;
  end else lua_pop(L, parameters);
end;

function foundlist_getValue(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  foundlist: Tfoundlist;
  b: dword;
  value: string;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    foundlist:=lua_touserdata(L,-2);
    index:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    foundlist.GetAddress(index, b, value);

    lua_pushstring(L, value);
    result:=1;
  end else lua_pop(L, parameters);
end;

function supportCheatEngine(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  //attachwindow, hasclosebutton, width, height, position ,yoururl OPTIONAL, extraparameters OPTIONAL, percentageshown OPTIONAL
  attachwindow: TCustomForm;
  hasCloseButton: boolean;
  width: integer;
  height: integer;
  position: integer;
  yoururl: string;
  extraparameters: string;
  percentageshown: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=5 then
  begin
    attachwindow:=lua_touserdata(L, -parameters);
    hasCloseButton:=lua_toboolean(L, -parameters+1);
    width:=lua_tointeger(L, -parameters+2);
    height:=lua_tointeger(L, -parameters+3);
    position:=lua_tointeger(L, -parameters+4);

    if parameters>=6 then
      yoururl:=Lua_ToString(L, -parameters+5)
    else
      yoururl:='';

    if parameters>=7 then
      extraparameters:=Lua_ToString(L, -parameters+6)
    else
      extraparameters:='';

    if parameters>=8 then
      percentageshown:=lua_tointeger(L, -parameters+7)
    else
      percentageshown:=0;

    lua_pop(L, lua_gettop(L));

    if adwindow=nil then
      adwindow:=TADWindow.CreateNew(nil, hasclosebutton);

    adwindow.clientWidth:=width;
    adwindow.clientheight:=height;
    adwindow.show;
    adwindow.AttachToForm(attachwindow);
    case position of
      0: adwindow.setPosition(akTop);
      1: adwindow.setPosition(akRight);
      2: adwindow.setPosition(akBottom);
      3: adwindow.setPosition(akLeft);
    end;

    adwindow.setUserUrl(yoururl);
    adwindow.setUserPercentage(percentageshown);
    adwindow.optional:=extraparameters;

    adwindow.LoadAd;


  end else lua_pop(L, lua_gettop(L));
end;

function fuckCheatEngine(L: Plua_State): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));
  if adwindow<>nil then
    adWindow.visible:=false;

  result:=0;
end;

function dbk_initialize(L: Plua_State): integer; cdecl;
var x: bool;
begin
  LoadDBK32;
  lua_pushboolean(L, isDriverLoaded(@x));
  result:=1;
end;

function dbk_useKernelmodeOpenProcess(L: Plua_State): integer; cdecl;
begin
  UseDBKOpenProcess;

  result:=0;
end;

function dbk_useKernelmodeProcessMemoryAccess(L: Plua_State): integer; cdecl;
begin
  UseDBKReadWriteMemory;
  result:=0;
end;

function dbk_useKernelmodeQueryMemoryRegions(L: Plua_State): integer; cdecl;
begin
  UseDBKQueryMemoryRegion;
  result:=0;
end;

function dbk_getPEProcess(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  pid: dword;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    pid:=lua_tointeger(L,-1);

    lua_pushinteger(L, GetPEProcess(pid));
    result:=1;
  end else lua_pop(L, parameters);
end;

function dbk_getPEThread(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  pid: dword;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    pid:=lua_tointeger(L,-1);

    lua_pushinteger(L, GetPEThread(pid));
    result:=1;
  end else lua_pop(L, parameters);
end;

function dbk_executeKernelMemory(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  address: ptruint;
  parameter: ptruint;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    if lua_isstring(L, -parameters) then
      address:=symhandler.getAddressFromName(Lua_ToString(L,-parameters))
    else
      address:=lua_tointeger(L, -parameters);

    if parameters>=2 then
    begin
      if lua_isstring(L, -parameters+1) then
        parameter:=symhandler.getAddressFromName(Lua_ToString(L,-parameters+1))
      else
        parameter:=lua_tointeger(L, -parameters+1);
    end
    else
      parameter:=0;

    lua_pop(L, parameters);


    executeKernelCode(address,parameter);

    result:=0;
  end else lua_pop(L, parameters);
end;

function createSplitter(L: Plua_State): integer; cdecl;
var
  Splitter: TCESplitter;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_touserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  Splitter:=TCESplitter.Create(owner);
  if owner<>nil then
    Splitter.Parent:=owner;

  lua_pushlightuserdata(L, splitter);
  result:=1;
end;

function allocateSharedMemory(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  sharedmemoryname: string;
  size: ptruint;
  address: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    sharedmemoryname:=Lua_ToString(L,-parameters);

    if parameters>=2 then
      size:=lua_tointeger(L, -parameters+1)
    else
      size:=4096;

    lua_pop(L, parameters);

    address:=allocateSharedMemoryIntoTargetProcess(sharedmemoryname, size);
    if address<>nil then
    begin
      lua_pushinteger(L, ptruint(address));
      result:=1;
    end;
  end else lua_pop(L, parameters);
end;

function deallocateSharedMemory(L: PLua_State): integer; cdecl;
var parameters: integer;
  address: ptruint;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
    lua_pop(L, parameters);

  lua_pushstring(L, 'deallocateSharedMemory is not implemented (It''s not even in the list of available functions)');
  lua_error(L);
end;

function getCheatEngineDir(L: PLua_State): integer; cdecl;
begin
  lua_pop(L, lua_gettop(l));
  lua_pushstring(L, CheatEngineDir);
  result:=1;
end;

function disassemble_lua(L: PLua_State): integer; cdecl;
var parameters: integer;
  address: ptruint;
  d: TDisassembler;
  x: string;
  s: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L, -parameters) then
      address:=symhandler.getAddressFromName(lua_tostring(L,-parameters))
    else
      address:=lua_tointeger(L,-parameters);

    lua_pop(L, parameters);

    d:=TDisassembler.Create;
    try
      d.showmodules:=false;
      d.showsymbols:=false;
    finally
      d.free;
    end;

    s:=d.disassemble(address, x);
    result:=1;
    lua_pushstring(L, s);
  end else lua_pop(L, parameters);
end;

function splitDisassembledString(L: PLua_State): integer; cdecl;
var parameters: integer;
  disassembledstring: string;

  address, bytes, opcode, special: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    disassembledstring:=lua_tostring(L,-parameters);
    lua_pop(L, parameters);

    disassembler.splitDisassembledString(disassembledstring, true, address, bytes, opcode, special);

    result:=4;
    lua_pushstring(L, special);
    lua_pushstring(L, opcode);
    lua_pushstring(L, bytes);
    lua_pushstring(L, address);
  end
  else
    lua_pop(L, parameters);
end;

function hexadecimalview_getTopAddress(L: PLua_State): integer; cdecl;
var parameters: integer;
  hv: THexView;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    hv:=lua_touserdata(L, -1);
    lua_pop(L, parameters);

    result:=1;
    lua_pushinteger(L, hv.address);
  end
  else
    lua_pop(L, parameters);
end;

function hexadecimalview_setTopAddress(L: PLua_State): integer; cdecl;
var parameters: integer;
  hv: THexView;
  address: ptruint;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    hv:=lua_touserdata(L, -2);
    if lua_isstring(L, -1) then
      address:=symhandler.getAddressFromName(Lua_ToString(L, -1))
    else
      address:=lua_tointeger(L, -1);


    lua_pop(L, parameters);

    hv.address:=address;
  end
  else
    lua_pop(L, parameters);
end;

function hexadecimalview_onAddressChange(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  hexadecimalview: THexView;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    hexadecimalview:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(hexadecimalview.onAddressChange));
    hexadecimalview.onAddressChange:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      hexadecimalview.onAddressChange:=lc.AddressChangeEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      hexadecimalview.onAddressChange:=lc.AddressChangeEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function hexadecimalview_onByteSelect(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  hexadecimalview: THexView;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    hexadecimalview:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(hexadecimalview.onByteSelect));
    hexadecimalview.onByteSelect:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      hexadecimalview.onByteSelect:=lc.ByteSelectEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      hexadecimalview.onByteSelect:=lc.ByteSelectEvent;
    end;

  end;

  lua_pop(L, parameters);
end;


function disassemblerview_getSelectedAddress(L: PLua_State): integer; cdecl;
var parameters: integer;
  dv: TDisassemblerview;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    dv:=lua_touserdata(L, -1);
    lua_pop(L, parameters);

    result:=1;
    lua_pushinteger(L, dv.SelectedAddress);
  end
  else
    lua_pop(L, parameters);
end;

function disassemblerview_setSelectedAddress(L: PLua_State): integer; cdecl;
var parameters: integer;
  dv: TDisassemblerview;
  address: ptruint;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    dv:=lua_touserdata(L, -2);
    if lua_isstring(L, -1) then
      address:=symhandler.getAddressFromName(Lua_ToString(L, -1))
    else
      address:=lua_tointeger(L, -1);


    lua_pop(L, parameters);

    dv.SelectedAddress:=address;
  end
  else
    lua_pop(L, parameters);
end;

function disassemblerview_onSelectionChange(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  disassemblerview: TDisassemblerview;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    disassemblerview:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(disassemblerview.onselectionchange));
    disassemblerview.onselectionchange:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      disassemblerview.onselectionchange:=lc.DisassemblerSelectionChangeEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      disassemblerview.onselectionchange:=lc.DisassemblerSelectionChangeEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

procedure InitializeLua;
var s: tstringlist;
begin
  LuaVM:=lua_open();
  if LuaVM<>nil then
  begin
    luaL_openlibs(LuaVM);

    lua_atpanic(LuaVM, LuaPanic);
    lua_register(LuaVM, 'print', print);
    lua_register(LuaVM, 'sleep', sleep);
    lua_register(LuaVM, 'pause', pause);
    lua_register(LuaVM, 'unpause', unpause);
    lua_register(LuaVM, 'readBytes', readbytes);
    lua_register(LuaVM, 'writeBytes', writebytes);
    lua_register(LuaVM, 'readInteger', readInteger);
    lua_register(LuaVM, 'readFloat', readFloat);
    lua_register(LuaVM, 'readDouble', readDouble);
    lua_register(LuaVM, 'readString', readString);
    lua_register(LuaVM, 'writeInteger', writeInteger);
    lua_register(LuaVM, 'writeFloat', writeFloat);
    lua_register(LuaVM, 'writeDouble', writeDouble);
    lua_register(LuaVM, 'writeString', writeString);

    lua_register(LuaVM, 'readBytesLocal', readbyteslocal);
    lua_register(LuaVM, 'writeBytesLocal', writebyteslocal);
    lua_register(LuaVM, 'autoAssemble', autoAssemble_lua);
    lua_register(LuaVM, 'showMessage', showMessage_lua);
    lua_register(LuaVM, 'getPixel', getPixel);
    lua_register(LuaVM, 'getMousePos', getMousePos);
    lua_register(LuaVM, 'setMousePos', setMousePos);
    lua_register(LuaVM, 'createTableEntry', createTableEntry);
    lua_register(LuaVM, 'getTableEntry', getTableEntry);
    lua_register(LuaVM, 'memoryrecord_setDescription', memoryrecord_setDescription);
    lua_register(LuaVM, 'memoryrecord_getDescription', memoryrecord_getDescription);
    lua_register(LuaVM, 'memoryrecord_getAddress', memoryrecord_getAddress);
    lua_register(LuaVM, 'memoryrecord_setAddress', memoryrecord_setAddress);
    lua_register(LuaVM, 'memoryrecord_getType', memoryrecord_getType);
    lua_register(LuaVM, 'memoryrecord_setType', memoryrecord_setType);
    lua_register(LuaVM, 'memoryrecord_getValue', memoryrecord_getValue);
    lua_register(LuaVM, 'memoryrecord_setValue', memoryrecord_setValue);
    lua_register(LuaVM, 'memoryrecord_getScript', memoryrecord_getScript);
    lua_register(LuaVM, 'memoryrecord_setScript', memoryrecord_setScript);
    lua_register(LuaVM, 'memoryrecord_isActive', memoryrecord_isActive);
    lua_register(LuaVM, 'memoryrecord_freeze', memoryrecord_freeze);
    lua_register(LuaVM, 'memoryrecord_unfreeze', memoryrecord_unfreeze);
    lua_register(LuaVM, 'memoryrecord_setColor', memoryrecord_setColor);
    lua_register(LuaVM, 'memoryrecord_appendToEntry', memoryrecord_appendToEntry);
    lua_register(LuaVM, 'memoryrecord_delete', memoryrecord_delete);

    lua_register(LuaVM, 'memoryrecord_getID', memoryrecord_getID);
    lua_register(LuaVM, 'memoryrecord_getHotkeyCount', memoryrecord_getHotkeyCount);
    lua_register(LuaVM, 'memoryrecord_getHotkey', memoryrecord_getHotkey);
    lua_register(LuaVM, 'memoryrecord_getHotkeyByID', memoryrecord_getHotkeyByID);
    lua_register(LuaVM, 'memoryrecord_onActivate', memoryrecord_onActivate);
    lua_register(LuaVM, 'memoryrecord_onDeactivate', memoryrecord_onDeactivate);



    lua_register(LuaVM, 'isKeyPressed', isKeyPressed);
    lua_register(LuaVM, 'keyDown', keyDown);
    lua_register(LuaVM, 'keyUp', keyUp);
    lua_register(LuaVM, 'doKeyPress', doKeyPress);
    lua_register(LuaVM, 'getProcessIDFromProcessName', getProcessIDFromProcessName);
    lua_register(LuaVM, 'openProcess', openProcess);
    lua_register(LuaVM, 'debugProcess', debugProcess);
    lua_register(LuaVM, 'debug_setBreakpoint', debug_setBreakpoint);
    lua_register(LuaVM, 'debug_removeBreakpoint', debug_removeBreakpoint);
    lua_register(LuaVM, 'debug_continueFromBreakpoint', debug_continueFromBreakpoint);
    lua_register(LuaVM, 'closeCE', closeCE);
    lua_register(LuaVM, 'hideAllCEWindows', hideAllCEWindows);
    lua_register(LuaVM, 'unhideMainCEwindow', unhideMainCEwindow);


    lua_register(LuaVM, 'createGroupBox', createGroupBox);


    lua_register(LuaVM, 'createLabel', createLabel);
    lua_register(LuaVM, 'createSplitter', createSplitter);

    lua_register(LuaVM, 'messageDialog', messageDialog);
    lua_register(LuaVM, 'speedhack_setSpeed', speedhack_setSpeed);
    lua_register(LuaVM, 'injectDLL', injectDLL);
    lua_register(LuaVM, 'getAutoAttachList', getAutoAttachList);


    lua_register(LuaVM, 'generateAPIHookScript', generateAPIHookScript_lua);
    lua_register(LuaVM, 'createProcess', createProcess);
    lua_register(LuaVM, 'AOBScan', AOBScan);
    lua_register(LuaVM, 'getOpenedProcessID', getOpenedProcessID);
    lua_register(LuaVM, 'getAddress', getAddress);
    lua_register(LuaVM, 'reinitializeSymbolhandler', reinitializeSymbolhandler);

    //ce6.1
    lua_register(LuaVM, 'getNameFromAddress', getNameFromAddress);
    lua_register(LuaVM, 'inModule', inModule);
    lua_register(LuaVM, 'inSystemModule', inSystemModule);
    lua_register(LuaVM, 'getCommonModuleList', getCommonModuleList);

    lua_register(LuaVM, 'createImage', createImage);
    lua_register(LuaVM, 'image_loadImageFromFile', image_loadImageFromFile);
    lua_register(LuaVM, 'image_transparent', image_transparent);
    lua_register(LuaVM, 'image_stretch', image_stretch);
    lua_register(LuaVM, 'image_getCanvas', image_getCanvas);
    lua_register(LuaVM, 'image_getPicture', Image_getPicture);


    lua_register(LuaVM, 'createHotkey', createHotkey);
    lua_register(LuaVM, 'generichotkey_setKeys', generichotkey_setKeys);
    lua_register(LuaVM, 'generichotkey_onHotkey', generichotkey_onHotkey);

    lua_register(LuaVM, 'getPropertyList', getPropertyList);
    lua_register(LuaVM, 'setProperty', setProperty);
    lua_register(LuaVM, 'getProperty', getProperty);

    lua_register(LuaVM, 'object_getClassName', object_getClassName);
    lua_register(LuaVM, 'object_destroy', object_destroy);

    lua_register(LuaVM, 'component_getComponentCount', component_getComponentCount);
    lua_register(LuaVM, 'component_getComponent', component_getComponent);
    lua_register(LuaVM, 'component_findComponentByName', component_findComponentByName);
    lua_register(LuaVM, 'component_getName', component_getName);
    lua_register(LuaVM, 'component_setName', component_setName);
    lua_register(LuaVM, 'component_getTag', component_getTag);
    lua_register(LuaVM, 'component_setTag', component_setTag);
    lua_register(LuaVM, 'component_getOwner', component_getOwner);

    lua_register(LuaVM, 'control_setCaption', control_setCaption);
    lua_register(LuaVM, 'control_getCaption', control_getCaption);
    lua_register(LuaVM, 'control_setPosition', control_setPosition);
    lua_register(LuaVM, 'control_getPosition', control_getPosition);
    lua_register(LuaVM, 'control_setSize', control_setSize);
    lua_register(LuaVM, 'control_getSize', control_getSize);
    lua_register(LuaVM, 'control_setAlign', control_setAlign);
    lua_register(LuaVM, 'control_getAlign', control_getAlign);
    lua_register(LuaVM, 'control_onClick', control_onClick);
    lua_register(LuaVM, 'control_setEnabled', control_setEnabled);
    lua_register(LuaVM, 'control_getEnabled', control_getEnabled);
    lua_register(LuaVM, 'control_setVisible', control_setVisible);
    lua_register(LuaVM, 'control_getVisible', control_getVisible);
    lua_register(LuaVM, 'control_setColor', control_setColor);
    lua_register(LuaVM, 'control_getColor', control_getColor);
    lua_register(LuaVM, 'control_setParent', control_setParent);
    lua_register(LuaVM, 'control_getParent', control_getParent);
    lua_register(LuaVM, 'control_setPopupMenu', control_setPopupMenu);
    lua_register(LuaVM, 'control_getPopupMenu', control_getPopupMenu);



    lua_register(LuaVM, 'wincontrol_getControlCount', wincontrol_getControlCount);
    lua_register(LuaVM, 'wincontrol_getControl', wincontrol_getControl);
    lua_register(LuaVM, 'wincontrol_onEnter', wincontrol_OnEnter);
    lua_register(LuaVM, 'wincontrol_onExit', wincontrol_OnExit);
    lua_register(LuaVM, 'wincontrol_canFocus', wincontrol_canFocus);
    lua_register(LuaVM, 'wincontrol_focused', wincontrol_focused);
    lua_register(LuaVM, 'wincontrol_setFocus', wincontrol_setFocus);

    lua_register(LuaVM, 'strings_add', strings_add);
    lua_register(LuaVM, 'strings_clear', strings_clear);
    lua_register(LuaVM, 'strings_delete', strings_delete);
    lua_register(LuaVM, 'strings_getText', strings_getText);
    lua_register(LuaVM, 'strings_indexOf', strings_indexOf);
    lua_register(LuaVM, 'strings_insert', strings_insert);
    lua_register(LuaVM, 'strings_getCount', strings_getCount);
    lua_register(LuaVM, 'strings_remove', strings_remove);
    lua_register(LuaVM, 'strings_getString', strings_getString);
    lua_register(LuaVM, 'strings_setString', strings_setString);


    lua_register(LuaVM, 'createStringlist', createStringlist);
    lua_register(LuaVM, 'stringlist_getDuplicates', stringlist_getDuplicates);
    lua_register(LuaVM, 'stringlist_setDuplicates', stringlist_setDuplicates);
    lua_register(LuaVM, 'stringlist_getSorted', stringlist_getSorted);
    lua_register(LuaVM, 'stringlist_setSorted', stringlist_setSorted);
    lua_register(LuaVM, 'stringlist_getCaseSensitive', stringlist_getCaseSensitive);
    lua_register(LuaVM, 'stringlist_setCaseSensitive', stringlist_setCaseSensitive);

    lua_register(LuaVM, 'createForm', createForm);
    lua_register(LuaVM, 'form_centerScreen', form_centerScreen);
    lua_register(LuaVM, 'form_onClose', form_onClose);
    lua_register(LuaVM, 'form_show', form_show);
    lua_register(LuaVM, 'form_hide', form_hide);
    lua_register(LuaVM, 'form_showModal', form_showModal);
    lua_register(LuaVM, 'form_isForegroundWindow', form_isForegroundWindow);
    lua_register(LuaVM, 'form_getMenu', form_getMenu);
    lua_register(LuaVM, 'form_setMenu', form_setMenu);

    lua_register(LuaVM, 'createPanel', createPanel);
    lua_register(LuaVM, 'panel_getAlignment', panel_getAlignment);
    lua_register(LuaVM, 'panel_setAlignment', panel_setAlignment);
    lua_register(LuaVM, 'panel_getBevelInner', panel_getBevelInner);
    lua_register(LuaVM, 'panel_setBevelInner', panel_setBevelInner);
    lua_register(LuaVM, 'panel_getBevelOuter', panel_getBevelOuter);
    lua_register(LuaVM, 'panel_setBevelOuter', panel_setBevelOuter);
    lua_register(LuaVM, 'panel_getBevelWidth', panel_getBevelWidth);
    lua_register(LuaVM, 'panel_setBevelWidth', panel_setBevelWidth);
    lua_register(LuaVM, 'panel_getFullRepaint', panel_getFullRepaint);
    lua_register(LuaVM, 'panel_setFullRepaint', panel_setFullRepaint);



    lua_register(LuaVM, 'createEdit', createEdit);
    lua_register(LuaVM, 'edit_clear', edit_clear);
    lua_register(LuaVM, 'edit_selectAll', edit_selectAll);
    lua_register(LuaVM, 'edit_clearSelection', edit_clearSelection);
    lua_register(LuaVM, 'edit_copyToClipboard', edit_copyToClipboard);
    lua_register(LuaVM, 'edit_cutToClipboard', edit_cutToClipboard);
    lua_register(LuaVM, 'edit_pasteFromClipboard', edit_pasteFromClipboard);
    lua_register(LuaVM, 'edit_onChange', edit_onChange);

    lua_register(LuaVM, 'createMemo', createMemo);
    lua_register(LuaVM, 'memo_append', memo_append);
    lua_register(LuaVM, 'memo_getLines', memo_getLines);
    lua_register(LuaVM, 'memo_getWordWrap', memo_getWordWrap);
    lua_register(LuaVM, 'memo_setWordWrap', memo_setWordWrap);
    lua_register(LuaVM, 'memo_getWantTabs', memo_getWantTabs);
    lua_register(LuaVM, 'memo_setWantTabs', memo_setWantTabs);
    lua_register(LuaVM, 'memo_getWantReturns', memo_getWantReturns);
    lua_register(LuaVM, 'memo_setWantReturns', memo_setWantReturns);
    lua_register(LuaVM, 'memo_getScrollbars', memo_getScrollbars);
    lua_register(LuaVM, 'memo_setScrollbars', memo_setScrollbars);

    lua_register(LuaVM, 'createButton', createButton);
    lua_register(LuaVM, 'button_getModalResult', button_getModalResult);
    lua_register(LuaVM, 'button_setModalResult', button_setModalResult);

    lua_register(LuaVM, 'createToggleBox', createToggleBox);
    lua_register(LuaVM, 'createCheckBox', createCheckBox);
    lua_register(LuaVM, 'checkbox_getAllowGrayed', checkbox_getAllowGrayed);
    lua_register(LuaVM, 'checkbox_setAllowGrayed', checkbox_setAllowGrayed);
    lua_register(LuaVM, 'checkbox_getState', checkbox_getState);
    lua_register(LuaVM, 'checkbox_setState', checkbox_setState);
    lua_register(LuaVM, 'checkbox_onChange', checkbox_onChange);


    lua_register(LuaVM, 'createRadioGroup', createRadioGroup);
    lua_register(LuaVM, 'radiogroup_getRows', radiogroup_getRows);
    lua_register(LuaVM, 'radiogroup_getItems', radioGroup_getItems);
    lua_register(LuaVM, 'radiogroup_getColumns', radiogroup_getColumns);
    lua_register(LuaVM, 'radiogroup_setColumns', radiogroup_setColumns);
    lua_register(LuaVM,' radiogroup_onClick', radiogroup_onClick);

    lua_register(LuaVM, 'createListBox', createListBox);
    lua_register(LuaVM, 'listbox_clear', listbox_clear);
    lua_register(LuaVM, 'listbox_getItems', listbox_getItems);
    lua_register(LuaVM, 'listbox_getItemIndex', listbox_getItemIndex);
    lua_register(LuaVM, 'listbox_setItemIndex', listbox_setItemIndex);
    lua_register(LuaVM, 'listbox_getCanvas', listbox_getCanvas);

    lua_register(LuaVM, 'createComboBox', createComboBox);
    lua_register(LuaVM, 'combobox_clear', combobox_clear);
    lua_register(LuaVM, 'combobox_getItems', combobox_getItems);
    lua_register(LuaVM, 'combobox_getItemIndex', combobox_getItemIndex);
    lua_register(LuaVM, 'combobox_setItemIndex', combobox_setItemIndex);
    lua_register(LuaVM, 'combobox_getCanvas', combobox_getCanvas);


    lua_register(LuaVM, 'createProgressBar', createProgressBar);
    lua_register(LuaVM, 'progressbar_stepIt', progressbar_stepIt);
    lua_register(LuaVM, 'progressbar_stepBy', progressbar_stepBy);
    lua_register(LuaVM, 'progressbar_getMax', progressbar_getMax);
    lua_register(LuaVM, 'progressbar_setMax', progressbar_setMax);
    lua_register(LuaVM, 'progressbar_getMin', progressbar_getMin);
    lua_register(LuaVM, 'progressbar_setMin', progressbar_setMin);
    lua_register(LuaVM, 'progressbar_getPosition', progressbar_getPosition);
    lua_register(LuaVM, 'progressbar_setPosition', progressbar_setPosition);

    lua_register(LuaVM, 'createTrackBar', createTrackBar);
    lua_register(LuaVM, 'trackbar_getMax', trackbar_getMax);
    lua_register(LuaVM, 'trackbar_setMax', trackbar_setMax);
    lua_register(LuaVM, 'trackbar_getMin', trackbar_getMin);
    lua_register(LuaVM, 'trackbar_setMin', trackbar_setMin);
    lua_register(LuaVM, 'trackbar_getPosition', trackbar_getPosition);
    lua_register(LuaVM, 'trackbar_setPosition', trackbar_setPosition);
    lua_register(LuaVM, 'trackbar_onChange', trackbar_onChange);


    lua_register(LuaVM, 'listcolumn_setAutosize', listcolumn_setAutosize);
    lua_register(LuaVM, 'listcolumn_getCaption', listcolumn_getCaption);
    lua_register(LuaVM, 'listcolumn_setCaption', listcolumn_setCaption);
    lua_register(LuaVM, 'listcolumn_getMaxWidth', listcolumn_getMaxWidth);
    lua_register(LuaVM, 'listcolumn_setMaxWidth', listcolumn_setMaxWidth);
    lua_register(LuaVM, 'listcolumn_getMinWidth', listcolumn_getMinWidth);
    lua_register(LuaVM, 'listcolumn_setMinWidth', listcolumn_setMinWidth);
    lua_register(LuaVM, 'listcolumn_getWidth', listcolumn_getWidth);
    lua_register(LuaVM, 'listcolumn_setWidth', listcolumn_setWidth);


    lua_register(LuaVM, 'collection_clear', collection_clear);
    lua_register(LuaVM, 'collection_getCount', collection_getCount);
    lua_register(LuaVM, 'collection_delete', collection_delete);

    lua_register(LuaVM, 'listcolumns_add', listcolumns_add);
    lua_register(LuaVM, 'listcolumns_getColumn', listcolumns_getColumn);

    lua_register(LuaVM, 'listitem_delete', listitem_delete);
    lua_register(LuaVM, 'listitem_getCaption', listitem_getCaption);
    lua_register(LuaVM, 'listitem_setCaption', listitem_setCaption);
    lua_register(LuaVM, 'listitem_getSubItems', listitem_getSubItems);

    lua_register(LuaVM, 'listitems_clear', listitems_clear);
    lua_register(LuaVM, 'listitems_getCount', listitems_getCount);
    lua_register(LuaVM, 'listitems_add', listitems_add);


    lua_register(LuaVM, 'createListView', createListView);
    lua_register(LuaVM, 'listview_clear', listview_clear);
    lua_register(LuaVM, 'listview_getColumns', listview_getColumns);
    lua_register(LuaVM, 'listview_getItems', listview_getItems);
    lua_register(LuaVM, 'listview_getItemIndex', listview_getItemIndex);
    lua_register(LuaVM, 'listview_setItemIndex', listview_setItemIndex);
    lua_register(LuaVM, 'listview_getCanvas', listview_getCanvas);


    lua_register(LuaVM, 'createTimer', createTimer);
    lua_register(LuaVM, 'timer_setInterval', timer_setInterval);
    lua_register(LuaVM, 'timer_onTimer', timer_onTimer);
    lua_register(LuaVM, 'timer_setEnabled', timer_setEnabled);
    lua_register(LuaVM, 'timer_getEnabled', timer_getEnabled);

    lua_register(LuaVM, 'openDialog_execute', openDialog_execute);

    Lua_register(LuaVM, 'getMemoryViewForm', getMainForm);
    lua_register(LuaVM, 'memoryview_getDisassemblerView', memoryview_getDisassemblerView);
    lua_register(LuaVM, 'memoryview_getHexadecimalView', memoryview_getHexadecimalView);


    Lua_register(LuaVM, 'getMainForm', getMainForm);
    Lua_register(LuaVM, 'getAddressList', getAddressList);
    Lua_register(LuaVM, 'getFreezeTimer', getFreezeTimer);
    Lua_register(LuaVM, 'getUpdateTimer', getUpdateTimer);

    Lua_register(LuaVM, 'findTableFile', findTableFile);
    Lua_register(LuaVM, 'tablefile_saveToFile', tablefile_saveToFile);
    Lua_register(LuaVM, 'tablefile_getData', tablefile_getData);

    Lua_register(LuaVM, 'xmplayer_playXM', xmplayer_playXM);
    Lua_register(LuaVM, 'xmplayer_pause', xmplayer_pause);
    Lua_register(LuaVM, 'xmplayer_resume', xmplayer_resume);
    Lua_register(LuaVM, 'xmplayer_stop', xmplayer_stop);
    Lua_register(LuaVM, 'xmplayer_isPlaying', xmplayer_isPlaying);

    Lua_register(LuaVM, 'writeRegionToFile', writeRegionToFile);
    Lua_register(LuaVM, 'readRegionFromFile', readRegionFromFile);

    Lua_register(LuaVM, 'registerSymbol', registersymbol);
    Lua_register(LuaVM, 'unregisterSymbol', unregistersymbol);

    Lua_register(LuaVM, 'resetLuaState', resetLuaState);

    Lua_register(LuaVM, 'cheatcomponent_setActive', cheatcomponent_setActive);
    Lua_register(LuaVM, 'cheatcomponent_getActive', cheatcomponent_getActive);
    Lua_register(LuaVM, 'cheatcomponent_setDescription', cheatcomponent_setDescription);
    Lua_register(LuaVM, 'cheatcomponent_getDescription', cheatcomponent_getDescription);
    Lua_register(LuaVM, 'cheatcomponent_setHotkey', cheatcomponent_setHotkey);
    Lua_register(LuaVM, 'cheatcomponent_getHotkey', cheatcomponent_getHotkey);
    Lua_register(LuaVM, 'cheatcomponent_setDescriptionLeft', cheatcomponent_setDescriptionLeft);
    Lua_register(LuaVM, 'cheatcomponent_getDescriptionLeft', cheatcomponent_getDescriptionLeft);
    Lua_register(LuaVM, 'cheatcomponent_setHotkeyLeft', cheatcomponent_setHotkeyLeft);
    Lua_register(LuaVM, 'cheatcomponent_getHotkeyLeft', cheatcomponent_getHotkeyLeft);
    Lua_register(LuaVM, 'cheatcomponent_setEditValue', cheatcomponent_setEditValue);
    Lua_register(LuaVM, 'cheatcomponent_getEditValue', cheatcomponent_getEditValue);


    Lua_register(LuaVM, 'memoryrecordhotkey_getDescription', memoryrecordhotkey_getDescription);
    Lua_register(LuaVM, 'memoryrecordhotkey_getHotkeyString', memoryrecordhotkey_getHotkeyString);
    Lua_register(LuaVM, 'memoryrecordhotkey_getID', memoryrecordhotkey_getID);
    Lua_register(LuaVM, 'memoryrecordhotkey_onHotkey', memoryrecordhotkey_onHotkey);
    Lua_register(LuaVM, 'memoryrecordhotkey_onPostHotkey', memoryrecordhotkey_onPostHotkey);
    Lua_register(LuaVM, 'memoryrecordhotkey_getOwner', memoryrecordhotkey_getOwner);
    Lua_register(LuaVM, 'memoryrecordhotkey_doHotkey', memoryrecordhotkey_doHotkey);

    Lua_register(LuaVM, 'addresslist_getCount', addresslist_getCount);
    Lua_register(LuaVM, 'addresslist_getMemoryRecord', addresslist_getMemoryRecord);
    Lua_register(LuaVM, 'addresslist_getMemoryRecordByDescription', addresslist_getMemoryRecordByDescription);
    Lua_register(LuaVM, 'addresslist_getMemoryRecordByID', addresslist_getMemoryRecordByID);
    Lua_register(LuaVM, 'addresslist_createMemoryRecord', addresslist_createMemoryRecord);

    Lua_register(LuaVM, 'createMemScan', createMemScan);
    Lua_register(LuaVM, 'getCurrentMemscan', getCurrentMemscan);
    Lua_register(LuaVM, 'memscan_firstScan', memscan_firstScan);
    Lua_register(LuaVM, 'memscan_nextScan', memscan_nextScan);
    Lua_register(LuaVM, 'memscan_waitTillDone', memscan_waitTillDone);
    Lua_register(LuaVM, 'memscan_saveCurrentResults', memscan_saveCurrentResults);
    Lua_register(LuaVM, 'memscan_getAttachedFoundlist', memscan_getAttachedFoundlist);


    Lua_register(LuaVM, 'createFoundList', createFoundList);
    Lua_register(LuaVM, 'foundlist_initialize', foundlist_initialize);
    Lua_register(LuaVM, 'foundlist_deinitialize', foundlist_deinitialize);
    Lua_register(LuaVM, 'foundlist_getCount', foundlist_getCount);
    Lua_register(LuaVM, 'foundlist_getAddress', foundlist_getAddress);
    Lua_register(LuaVM, 'foundlist_getValue', foundlist_getValue);

    Lua_register(LuaVM, 'supportCheatEngine', supportCheatEngine);
    Lua_register(LuaVM, 'fuckCheatEngine', fuckCheatEngine);



    lua_register(LuaVM, 'inheritsFromObject', inheritsFromObject);
    lua_register(LuaVM, 'inheritsFromComponent', inheritsFromComponent);
    lua_register(LuaVM, 'inheritsFromControl', inheritsFromControl);
    lua_register(LuaVM, 'inheritsFromWinControl', inheritsFromWinControl);

    Lua_register(LuaVM, 'beep', beep);


    lua_register(LuaVM, 'dbk_initialize', dbk_initialize);
    lua_register(LuaVM, 'dbk_useKernelmodeOpenProcess', dbk_useKernelmodeOpenProcess);
    lua_register(LuaVM, 'dbk_useKernelmodeProcessMemoryAccess', dbk_useKernelmodeProcessMemoryAccess);
    lua_register(LuaVM, 'dbk_useKernelmodeQueryMemoryRegions', dbk_useKernelmodeQueryMemoryRegions);
    lua_register(LuaVM, 'dbk_getPEProcess', dbk_getPEProcess);
    lua_register(LuaVM, 'dbk_getPEThread', dbk_getPEThread);
    lua_register(LuaVM, 'dbk_executeKernelMemory', dbk_executeKernelMemory);

    lua_register(LuaVM, 'allocateSharedMemory', allocateSharedMemory);
    lua_register(LuaVM, 'deallocateSharedMemory', deallocateSharedMemory);
    lua_register(LuaVM, 'getCheatEngineDir', getCheatEngineDir);

    lua_register(LuaVM, 'disassemble', disassemble_lua);
    lua_register(LuaVM, 'splitDisassembledString', splitDisassembledString);

    lua_register(LuaVM, 'customControl_getCanvas', customControl_getCanvas);
    lua_register(LuaVM, 'graphicControl_getCanvas', graphicControl_getCanvas);


    lua_register(LuaVM, 'disassemblerview_getSelectedAddress', disassemblerview_getSelectedAddress);
    lua_register(LuaVM, 'disassemblerview_setSelectedAddress', disassemblerview_setSelectedAddress);
    lua_register(LuaVM, 'disassemblerview_onSelectionChange', disassemblerview_onSelectionChange);

    lua_register(LuaVM, 'hexadecimalview_getTopAddress', hexadecimalview_getTopAddress);
    lua_register(LuaVM, 'hexadecimalview_setTopAddress', hexadecimalview_setTopAddress);
    lua_register(LuaVM, 'hexadecimalview_onAddressChange', hexadecimalview_onAddressChange);
    lua_register(LuaVM, 'hexadecimalview_onByteSelect', hexadecimalview_onByteSelect);


    initializeLuaPicture;
    initializeLuaPen;
    initializeLuaBrush;
    initializeLuaFont;
    initializeLuaCanvas;
    initializeLuaMenu;


    s:=tstringlist.create;
    try
      //ce 6.0 compatibility. 6.0 has these methods in the stringlist instead of the strings class
      s.add('package.path = package.path .. ";?.lua";');
      s.add('stringlist_getCount=strings_getCount');
      s.add('stringlist_getString=strings_getString');
      s.add('stringlist_add=strings_add');
      s.add('stringlist_remove=strings_remove');

      //same for the rename of memrec to memoryrecord
      s.add('memrec_setDescription = memoryrecord_setDescription');
      s.add('memrec_getDescription = memoryrecord_getDescription');
      s.add('memrec_getAddress = memoryrecord_getAddress');
      s.add('memrec_setAddress = memoryrecord_setAddress');
      s.add('memrec_getType = memoryrecord_getType');
      s.add('memrec_setType = memoryrecord_setType');
      s.add('memrec_getValue = memoryrecord_getValue');
      s.add('memrec_setValue = memoryrecord_setValue');
      s.add('memrec_getScript = memoryrecord_getScript');
      s.add('memrec_isActive = memoryrecord_isActive');
      s.add('memrec_freeze = memoryrecord_freeze');
      s.add('memrec_unfreeze = memoryrecord_unfreeze');
      s.add('memrec_setColor = memoryrecord_setColor');
      s.add('memrec_appendToEntry = memoryrecord_appendToEntry');
      s.add('memrec_delete = memoryrecord_delete');
      s.add('getAddressFromName = getAddress');

      //timer onInterval has been renamed to timer onTimer
      s.add('timer_onInterval = timer_onTimer');

      lua_doscript(s.text);

    finally
      s.free;
    end;
  end;
end;

initialization
  LuaCS:=TCriticalSection.create;


  InitializeLua;



finalization
  if LuaCS<>nil then
    LuaCS.free;

  if LuaVM<>nil then
    lua_close(LuaVM);

end.

