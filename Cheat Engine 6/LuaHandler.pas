unit LuaHandler;

{$mode delphi}

interface

uses
  windows, Classes, dialogs, SysUtils, lua, lualib, lauxlib, syncobjs, cefuncproc,
  newkernelhandler, autoassembler, Graphics, controls;

var
  LuaVM: Plua_State;
  LuaCS: Tcriticalsection;


function CheckIfConditionIsMetContext(context: PContext; script: string): boolean;
procedure LUA_DoScript(s: string);
function LUA_functioncall(routinetocall: string; parameters: array of const): integer;
procedure LUA_memrec_callback(memrec: pointer; routine: string);
procedure LUA_SetCurrentContextState(context: PContext);
function LUA_onBreakpoint(context: PContext): boolean;
procedure LUA_onNotify(functionid: integer; sender: tobject);
function Lua_ToString(L: Plua_State; i: Integer): string;
procedure InitializeLuaScripts;


implementation

uses frmluaengineunit, pluginexports, MemoryRecordUnit, debuggertypedefinitions,
  symbolhandler, frmautoinjectunit, simpleaobscanner;

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
      exit;
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

        //set the "changedREG" variables
        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'hasChangedARegister');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedEAX');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedEBX');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedECX');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedEDX');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedESI');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedEDI');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedEBP');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedESP');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedEIP');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedEFLAGS');

        {$ifdef cpu64}
        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedRAX');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedRBX');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedRCX');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedRDX');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedRSI');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedRDI');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedRBP');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedRSP');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedRIP');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedR8');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedR9');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedR10');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedR11');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedR12');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedR13');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedR14');

        lua_pushboolean(luavm, false);
        lua_setglobal(luavm, 'changedR15');
        {$endif}



        if lua_pcall(LuaVM, 0, 1, 0)=0 then
        begin
          if (lua_gettop(luavm)=1) then //only 1 parameter returned
            result:=lua_tointeger(luavm, -1)<>0;  //return the result is not 0


          lua_pop(LuaVM, lua_gettop(luavm)); //clear stack

          //set new state if changes where made

          lua_getglobal(luavm, 'hasChangedARegister');
          p:=lua_gettop(luavm);
          if p<>0 then
          begin
            if lua_toboolean(luavm, -1) then
            begin
              //hasChangedARegister is true, check which ones...
              lua_settop(luavm, lua_gettop(luavm));

              lua_getglobal(luavm, 'changedEAX');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'EAX');
                context.{$ifdef cpu64}Rax{$else}eax{$endif}:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedEBX');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'EBX');
                context.{$ifdef cpu64}RBX{$else}EBX{$endif}:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedECX');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'ECX');
                context.{$ifdef cpu64}RCX{$else}ECX{$endif}:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedEDX');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'EDX');
                context.{$ifdef cpu64}RDX{$else}EDX{$endif}:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedESI');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'ESI');
                context.{$ifdef cpu64}RSI{$else}ESI{$endif}:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedEDI');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'EDI');
                context.{$ifdef cpu64}RDI{$else}EDI{$endif}:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedEBP');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'EBP');
                context.{$ifdef cpu64}RBP{$else}EBP{$endif}:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedEIP');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'EIP');
                context.{$ifdef cpu64}RIP{$else}EIP{$endif}:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedEFLAGS');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'EFLAGS');
                context.EFLAGS:=lua_tointeger(luavm, -1);
              end;


              lua_pop(LuaVM, lua_gettop(luavm)); //clear stack (just to make sure no overflow happens, not even sure if it's needed)

              {$ifdef cpu64}
              lua_getglobal(luavm, 'changedRAX');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'RAX');
                context.RAX:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedRBX');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'RBX');
                context.RBX:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedRCX');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'RCX');
                context.RCX:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedRDX');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'RDX');
                context.RDX:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedRSI');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'RSI');
                context.RSI:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedRDI');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'RDI');
                context.RDI:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedRBP');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'RBP');
                context.RBP:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedRSP');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'RSP');
                context.RSP:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedRIP');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'RIP');
                context.RIP:=lua_tointeger(luavm, -1);
              end;

              lua_pop(LuaVM, lua_gettop(luavm)); //clear stack (just to make sure no overflow happens, not even sure if it's needed)

              lua_getglobal(luavm, 'changedR8');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'R8');
                context.R8:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedR9');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'R9');
                context.R9:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedR10');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'R10');
                context.R10:=lua_tointeger(luavm, -1);
              end;


              lua_getglobal(luavm, 'changedR11');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'R11');
                context.R11:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedR12');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'R12');
                context.R12:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedR13');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'R13');
                context.R13:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedR14');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'R14');
                context.R14:=lua_tointeger(luavm, -1);
              end;

              lua_getglobal(luavm, 'changedR15');
              if lua_toboolean(luavm, -1) then
              begin
                lua_getglobal(luavm, 'R15');
                context.R15:=lua_tointeger(luavm, -1);
              end;


              {$endif}

              lua_pop(luavm, lua_gettop(luavm));

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




    lua_pop(LuaVM, lua_gettop(luavm));
  finally
    LuaCS.Leave;
  end;
end;

procedure LUA_DoScript(s: string);
var i: integer;
  pc: pchar;
begin
  if GetCurrentThreadId<>MainThreadID then raise exception.create('LUA_DoScript was not called rom the main thread');

  LUACS.Enter;
  try
    i:=lua_dostring(luavm, pchar(s));
    if i<>0 then
    begin
      pc:=lua.lua_tostring(luavm, -1);
      if pc<>nil then
        raise Exception.Create(pc)
      else
        raise exception.create('Undefined lua error');

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
begin
  result:=-1;
  LuaCS.Enter;
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
            system.vtWideChar, vtPWideChar, vtVariant, vtInterface, vtWideString: lua_pushstring(LUAVM, 'Cheatengine is being a fag');
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
    lua_pop(luavm,lua_gettop(luavm));
    luacs.leave;
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
  raise exception.create('LUA panic!');
end;


function sleep_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
begin
  paramcount:=lua_gettop(L);

  result:=0;
  if paramcount=1 then
    sleep(lua_tointeger(L, -1));

  lua_pop(L, paramcount);
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

function print_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  s: string;

  str: string;
  i: integer;
begin
  paramcount:=lua_gettop(L);
  if paramcount=0 then exit;

  str:='';
  for i:=-paramcount to -1 do
  begin
    if lua_islightuserdata(L,i) then
      s:=inttohex(ptruint(lua_touserdata(L, i)),8)
    else
      s:=lua_tostring(L, i);

    str:=str+s+' ';
  end;

  if str<>'' then
    pluginsync(print2, @str[1]);

  lua_pop(L, paramcount);
  lua_pushstring(L, str);
  result:=1;
end;

function showMessage_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  s: string;
begin
  paramcount:=lua_gettop(L);
  if paramcount=0 then exit;

  if lua_islightuserdata(l,-1) then
    s:=inttohex(ptruint(lua_touserdata(L, -1)),8)
  else
    s:=lua_tostring(L, -1);

  ce_showmessage(pchar(s));

  lua_pop(L, paramcount);
  result:=0;
end;

function readInteger_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  address: ptruint;

  v: integer;
  r: dword;
begin
  result:=0;
  try
    paramcount:=lua_gettop(L);
    if paramcount=1 then
    begin
      if lua_isstring(L, -1) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-1))
      else
        address:=lua_tointeger(L,-1);

      lua_pop(L, paramcount);

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

function readFloat_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  address: ptruint;

  v: single;
  r: dword;
begin
  result:=0;
  try
    paramcount:=lua_gettop(L);
    if paramcount=1 then
    begin
      if lua_isstring(L, -1) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-1))
      else
        address:=lua_tointeger(L,-1);

      lua_pop(L, paramcount);

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

function readDouble_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  address: ptruint;

  v: double;
  r: dword;
begin
  result:=0;
  try
    paramcount:=lua_gettop(L);
    if paramcount=1 then
    begin
      if lua_isstring(L, -1) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-1))
      else
        address:=lua_tointeger(L,-1);

      lua_pop(L, paramcount);

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

function readString_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  address: ptruint;

  v: pchar;
  r: dword;
  maxsize: integer;
begin
  result:=0;
  try
    paramcount:=lua_gettop(L);
    if paramcount>=1 then
    begin
      if lua_isstring(L, -paramcount) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-2))
      else
        address:=lua_tointeger(L,-paramcount);

      if paramcount=2 then
        maxsize:=lua_tointeger(L,-1)
      else
        maxsize:=50;

      lua_pop(L, paramcount);

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

function writeInteger_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  address: ptruint;

  v: integer;
  r: dword;
begin
  result:=0;
  try
    paramcount:=lua_gettop(L);
    if paramcount=2 then
    begin
      if lua_isstring(L, -2) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-2))
      else
        address:=lua_tointeger(L,-2);

      v:=lua_tointeger(L, -2);

      lua_pop(L, paramcount);
      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), @v, sizeof(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeFloat_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  address: ptruint;

  v: single;
  r: dword;
begin
  result:=0;
  try
    paramcount:=lua_gettop(L);
    if paramcount=2 then
    begin
      if lua_isstring(L, -2) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-2))
      else
        address:=lua_tointeger(L,-2);

      v:=lua_tonumber(L, -2);

      lua_pop(L, paramcount);


      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), @v, sizeof(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeDouble_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  address: ptruint;

  v: double;
  r: dword;
begin
  result:=0;
  try
    paramcount:=lua_gettop(L);
    if paramcount=2 then
    begin
      if lua_isstring(L, -2) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-2))
      else
        address:=lua_tointeger(L,-2);

      v:=lua_tonumber(L, -2);

      lua_pop(L, paramcount);

      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), @v, sizeof(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeString_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  address: ptruint;

  v: pchar;
  r: dword;
begin
  result:=0;
  try
    paramcount:=lua_gettop(L);
    if paramcount=2 then
    begin
      if lua_isstring(L, -2) then
        address:=symhandler.getAddressFromName(lua_tostring(L,-2))
      else
        address:=lua_tointeger(L,-2);

      v:=lua.lua_tostring(L, -2);

      lua_pop(L, paramcount);

      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), v, length(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
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

function autoAssemble_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  code: TStringlist;
  r: boolean;
begin
  paramcount:=lua_gettop(L);
  if paramcount=0 then exit;

  code:=tstringlist.create;
  try
    code.text:=lua_tostring(L, -1);

    try
      r:=autoassemble(code, false);
    except
      r:=false;
    end;

    lua_pop(L, paramcount);
    lua_pushboolean(L, r);
  finally
    code.free;
  end;

  result:=1;
end;

function getPixel_fromlua(L: PLua_State): integer; cdecl;
var t:TCanvas;
  paramcount: integer;
  r: dword;
  x,y: integer;
begin
  result:=0; //return 0 paramaters
  paramcount:=lua_gettop(L);
  if paramcount=2 then
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
  end else lua_pop(L, paramcount);
end;

function getMousePos_fromlua(L: PLua_State): integer; cdecl;
var t:TCanvas;
  paramcount: integer;
  cp: Tpoint;
begin
  result:=0; //return 0 parameters
  paramcount:=lua_gettop(L);
  if paramcount=0 then
  begin
    cp:=mouse.CursorPos;
    lua_pushinteger(L, cp.x);
    lua_pushinteger(L, cp.y);
    result:=2;   //return 2 parameters
  end else lua_pop(L, paramcount);
end;

function setMousePos_fromlua(L: PLua_State): integer; cdecl;
var t:TCanvas;
  paramcount: integer;
  cp: Tpoint;
begin
  result:=0; //return 0 parameters
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    cp.x:=lua_tointeger(L, -2); //x
    cp.y:=lua_tointeger(L, -1); //y
    lua_pop(L, 2);

    mouse.CursorPos:=cp;
  end else lua_pop(L, paramcount);
end;

function createTableEntry_fromlua(L: PLua_State): integer; cdecl;
var paramcount: integer;
  r: pointer;
begin
  lua_pop(L, lua_gettop(L)); //clear the stack

  r:=ce_createTableEntry;
  lua_pushlightuserdata(L, r);
  result:=1;
end;

function getTableEntry_fromlua(L: PLua_State): integer; cdecl;
var paramcount: integer;
  description: pchar;
  r: pointer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    description:=lua.lua_tostring(L,-1); //description

    lua_pop(L, paramcount);  //clear stack

    r:=ce_getTableEntry(description);
    if r<>nil then
    begin
      lua_pushlightuserdata(L,r); //return the pointer
      result:=1;
    end;
  end else lua_pop(L, paramcount);
end;

function memrec_setDescription_fromlua(L: PLUA_State): integer; cdecl;
var
  paramcount: integer;
  description: pchar;
  memrec: pointer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    memrec:=lua_touserdata(L,-2); //memrec
    description:=lua.lua_tostring(L,-1); //description

    lua_pop(L, paramcount);  //clear stack

    ce_memrec_setDescription(memrec, description);
  end;

  lua_pop(L, paramcount);  //clear stack

end;

function memrec_getDescription_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  d: pchar;
  memrec: pointer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    memrec:=lua_touserdata(L,-1);
    d:=ce_memrec_getDescription(memrec);
    if d<>nil then
    begin
      lua_pushstring(L, d);
      result:=1;
    end;
  end else lua_pop(L, paramcount);
end;

function memrec_getAddress_fromlua(L: PLua_state): integer; cdecl;
var
  paramcount: integer;
  memrec: pointer;
  address: ptruint;
  offsets: array of dword;
  offsetcount: integer;
  i: integer;
begin
  result:=0;
  offsetcount:=0;
  setlength(offsets,0);

  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    memrec:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

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

  end else lua_pop(L, paramcount);
end;

function memrec_setAddress_fromlua(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  paramcount: integer;
  address: pchar;
  offsets: array of dword;
  i,j: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount>=2 then
  begin
    memrec:=lua_touserdata(L, (-paramcount));
    address:=lua.lua_tostring(L, (-paramcount)+1);

    setlength(offsets,paramcount-2);
    j:=0;
    for i:=(-paramcount)+2 to -1 do
    begin
      offsets[j]:=lua_tointeger(L, i);
      inc(j);
    end;

    lua_pop(L, paramcount);

    ce_memrec_setAddress(memrec, address, @offsets[0], length(offsets))
  end else
    lua_pop(L, paramcount);
end;

function memrec_getType_fromlua(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  paramcount: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    memrec:=lua_touserdata(L, (-paramcount));
    lua_pop(L, paramcount);

    lua_pushinteger(L, ce_memrec_getType(memrec));
    result:=1;

  end;
end;

function memrec_setType_fromlua(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  vtype: integer;
  paramcount: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    memrec:=lua_touserdata(L, -2);
    vtype:=lua_tointeger(L, -1);
    lua_pop(L, paramcount);

    ce_memrec_setType(memrec, vtype);
  end
  else
    lua_pop(L, paramcount);

end;

function memrec_getValue_fromlua(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  paramcount: integer;

  v: pchar;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    memrec:=lua_touserdata(L, (-paramcount));
    lua_pop(L, paramcount);


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
  end;
end;


function memrec_setValue_fromlua(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  paramcount: integer;

  v: pchar;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    memrec:=lua_touserdata(L, -2);
    v:=lua.lua_tostring(L, -1);


    ce_memrec_setValue(memrec, v);
  end;
  lua_pop(L, paramcount);
end;

function memrec_getScript_fromlua(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  paramcount: integer;

  v: pchar;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    memrec:=lua_touserdata(L, -1);
    v:=ce_memrec_getScript(memrec);
    lua_pop(L, paramcount);

    if v<>nil then
    begin
      lua_pushstring(L, v);
      result:=1;
    end;

  end else lua_pop(L, paramcount);


end;


function memrec_setScript_fromlua(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  paramcount: integer;

  v: pchar;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    memrec:=lua_touserdata(L, -2);
    v:=lua.lua_tostring(L, -1);

    ce_memrec_setScript(memrec, v);
  end;


  lua_pop(L, paramcount);
end;


function memrec_isActive_fromlua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  direction: integer;
  memrec: pointer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    memrec:=lua_touserdata(L, -paramcount);


    lua_pushboolean(L, ce_memrec_isFrozen(memrec));
    result:=1;
  end;

  lua_pop(L, paramcount);
end;

function memrec_freeze_fromlua(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  paramcount: integer;
  direction: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount>=1 then
  begin
    memrec:=lua_touserdata(L, -paramcount);


    if paramcount=2 then
      direction:=lua_tointeger(L, -1)
    else
      direction:=0;

    ce_memrec_freeze(memrec, direction);
  end;

  lua_pop(L, paramcount);
end;

function memrec_unfreeze_fromlua(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  paramcount: integer;
  direction: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    memrec:=lua_touserdata(L, -paramcount);
    ce_memrec_unfreeze(memrec);
  end;

  lua_pop(L, paramcount);
end;

function memrec_setColor_fromlua(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  paramcount: integer;
  color: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    memrec:=lua_touserdata(L,-2);
    color:=lua_tointeger(L,-1);
    ce_memrec_setColor(memrec,color);
  end;

  lua_pop(L, paramcount);
end;

function memrec_appendToEntry_fromlua(L: PLua_State): integer; cdecl;
var
  memrec1,memrec2: pointer;
  paramcount: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    memrec1:=lua_touserdata(L,-2);
    memrec2:=lua_touserdata(L,-1);
    ce_memrec_appendtoentry(memrec1,memrec2);
  end;

  lua_pop(L, paramcount);
end;

function memrec_delete_fromlua(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  paramcount: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    memrec:=lua_touserdata(L,-2);
    ce_memrec_delete(memrec);
  end;

  lua_pop(L, paramcount);
end;

function isKeyPressed_fromLua(L: PLua_State): integer; cdecl;
var paramcount: integer;
  keyinput: pchar;
  key: integer;
  w: word;
  r: boolean;
begin
  result:=0;
  r:=false;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
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

    lua_pop(L, paramcount); //parameters have been fetched, clear stack

    if key<>0 then
    begin
      w:=GetAsyncKeyState(key);
      r:=(w and 1)=1;

      if not r then
        r:=((w shr 15) and 1)=1;

      lua_pushboolean(L, r);
      result:=1;
    end;

  end else lua_pop(L, paramcount);

end;


function keyDown_fromLua(L: PLua_State): integer; cdecl;
var paramcount: integer;
  keyinput: pchar;
  key: integer;
  w: word;
  r: boolean;
begin
  result:=0;
  r:=false;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
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
  lua_pop(L, paramcount);
end;


function keyUp_fromLua(L: PLua_State): integer; cdecl;
var paramcount: integer;
  keyinput: pchar;
  key: integer;
  w: word;
  r: boolean;
begin
  result:=0;
  r:=false;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
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
  lua_pop(L, paramcount);
end;

function doKeyPress_fromLua(L: PLua_State): integer; cdecl;
var paramcount: integer;
  keyinput: pchar;
  key: integer;
  w: word;
  r: boolean;
begin
  result:=0;
  r:=false;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
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
  lua_pop(L, paramcount);
end;

function getProcessIDFromProcessName_fromLua(L: PLua_state): integer; cdecl;
var paramcount: integer;
  pname: pchar;
  pid: dword;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    pname:=lua.lua_tostring(L, -1);
    lua_pop(L, paramcount);

    pid:=ce_getProcessIDFromProcessName(pname);
    if pid<>0 then
    begin
      lua_pushinteger(L, pid);
      result:=1;
    end;


  end else lua_pop(L, paramcount);
end;

function openProcess_fromLua(L: PLua_state): integer; cdecl;
var paramcount: integer;
  pname: pchar;
  pid: dword;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    if lua_isstring(L,-1) then
    begin
      pname:=lua.lua_tostring(L,-1);
      pid:=ce_getProcessIDFromProcessName(pname);
    end
    else
      pid:=lua_tointeger(L,-1);

    lua_pop(L, paramcount);

    if pid<>0 then
      ce_openProcess(pid);

  end else lua_pop(L, paramcount);
end;

function pause_fromLua(L: PLua_state): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L)); //clear the stack
  ce_pause;
  result:=0;
end;

function unpause_fromLua(L: PLua_state): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L)); //clear the stack
  ce_unpause;
  result:=0;
end;


function debugProcess_fromLua(L: PLua_state): integer; cdecl;
var paramcount: integer;
  debuggerinterface: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
    debuggerinterface:=lua_tointeger(L, -1)
  else
    debuggerinterface:=0;

  lua_pop(L, lua_gettop(L)); //clear the stack

  ce_debugProcess(debuggerinterface);
end;

function debug_setBreakpoint_fromLua(L: Plua_State): integer; cdecl;
var paramcount: integer;
  i,j: integer;

  address: ptruint;
  size: integer;
  trigger: integer;
  e: boolean;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount>=1 then
  begin
    j:=0;
    address:=0;
    size:=1;
    trigger:=integer(bptexecute);
    for i:=-paramcount to -1 do
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

    ce_debug_setBreakpoint(address,size,TBreakpointTrigger(trigger));
  end;

  lua_pop(L, lua_gettop(L)); //clear the stack
end;

function debug_removeBreakpoint_fromLua(L: Plua_State): integer; cdecl;
var paramcount: integer;
  address: ptruint;
  e: boolean;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
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

function debug_continueFromBreakpoint_fromLua(L: Plua_State): integer; cdecl;
var paramcount: integer;
  method: TContinueOption;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    method:=TContinueOption(lua_tointeger(L, -1));
    ce_debug_continuefrombreakpoint(method);
  end;

  lua_pop(L, lua_gettop(L)); //clear the stack
end;

function closeCE_fromLua(L: Plua_state): integer; cdecl;
begin
  ce_closeCE;
  result:=0;
end;

function hideAllCEWindows_fromLua(L: Plua_State): integer; cdecl;
begin
  result:=0;
  lua_pop(L, lua_gettop(L)); //clear the stack

  ce_hideAllCEWindows;
end;

function unhideMainCEwindow_fromLua(L: Plua_State): integer; cdecl;
begin
  result:=0;
  lua_pop(L, lua_gettop(L)); //clear the stack

  ce_unhideMainCEwindow;
end;

function createForm_fromLua(L: Plua_State): integer; cdecl;
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

function form_centerScreen_fromLua(L: Plua_State): integer; cdecl;
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

function form_hide_fromLua(L: Plua_State): integer; cdecl;
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

function form_show_fromLua(L: Plua_State): integer; cdecl;
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

function createPanel_fromLua(L: Plua_State): integer; cdecl;
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

function createButton_fromLua(L: Plua_State): integer; cdecl;
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

function createGroupBox_fromLua(L: Plua_State): integer; cdecl;
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

function createImage_fromLua(L: Plua_State): integer; cdecl;
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

function image_loadImageFromFile_fromLua(L: Plua_State): integer; cdecl;
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

function image_stretch_fromLua(L: Plua_State): integer; cdecl;
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

function image_transparent_fromLua(L: Plua_State): integer; cdecl;
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



function createLabel_fromLua(L: Plua_State): integer; cdecl;
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

function createEdit_fromLua(L: Plua_State): integer; cdecl;
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

function createMemo_fromLua(L: Plua_State): integer; cdecl;
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

function createTimer_fromLua(L: Plua_State): integer; cdecl;
var parameters: integer;
  f,p: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    p:=ce_createTimer(f);

    lua_pop(L, lua_gettop(L));

    lua_pushlightuserdata(L, p);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function timer_setInterval_fromLua(L: Plua_State): integer; cdecl;
var parameters: integer;
  t: pointer;
  interval: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    t:=lua_touserdata(L, -2);
    interval:=lua_tointeger(L, -1);
    ce_timer_setInterval(t,interval);
  end;

  lua_pop(L, lua_gettop(L));
end;

function timer_onTimer_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: pointer;
  f: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);
    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX);
      ce_timer_onTimerLua(control, f);
    end;
  end;

  lua_pop(L, paramcount);
end;


function control_setCaption_fromLua(L: Plua_State): integer; cdecl;
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

function control_getCaption_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  d: pchar;
  control: pointer;

begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

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
  end else lua_pop(L, paramcount);
end;


function control_setPosition_fromLua(L: Plua_State): integer; cdecl;
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


function control_getPosition_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: pointer;
  x,y: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    x:=ce_control_getX(control);
    y:=ce_control_getY(control);

    lua_pushinteger(L, x);
    lua_pushinteger(L, y);
    result:=2;

  end else lua_pop(L, paramcount);
end;

function control_setSize_fromLua(L: Plua_State): integer; cdecl;
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

function control_getSize_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: pointer;
  width,height: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    width:=ce_control_getWidth(control);
    height:=ce_control_getHeight(control);

    lua_pushinteger(L, width);
    lua_pushinteger(L, height);
    result:=2;

  end else lua_pop(L, paramcount);
end;

function control_setAlign_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: pointer;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);
    a:=lua_tointeger(L,-1);
    ce_control_setAlign(control,a);
  end;

  lua_pop(L, paramcount);
end;


function form_onClose_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: pointer;
  f: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);
    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX);
      ce_form_onCloseLua(control, f);
    end;
  end;

  lua_pop(L, paramcount);
end;

function control_onClick_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: pointer;
  f: integer;


//  clickroutine: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);
    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX);
      ce_control_onClickLua(control, f);
    end;
  end;

  lua_pop(L, paramcount);
end;

function object_destroy_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  component: pointer;
  x,y: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    component:=lua_touserdata(L,-1);

    ce_object_destroy(component);
  end;

  lua_pop(L, paramcount);
end;

function messageDialog_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  message: pchar;
  dialogtype: integer;
  buttontype: integer;

  r: integer;

  i: integer;
  b: TMsgDlgButtons;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount>=3 then
  begin
    message:=lua.lua_tostring(L,-paramcount);
    dialogtype:=lua_tointeger(L,-paramcount+1);
    b:=[];
    for i:=-paramcount+2 to -1 do
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
    lua_pop(L, paramcount);

    r:=ce_messageDialog_lua(message, dialogtype, b);
    lua_pushinteger(L,r);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function speedhack_setSpeed_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  speed: single;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    speed:=lua_tonumber(L,-1);
    ce_speedhack_setSpeed(speed);
  end;
  lua_pop(L, paramcount);
end;

function injectDLL_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  filename: pchar;
  r: boolean;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
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
  lua_pop(L, paramcount);
end;




function getAutoAttachList_fromLua(L: Plua_State): integer; cdecl;
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

function stringlist_getCount_fromLua(L: PLua_state): integer; cdecl;
var parameters: integer;
  c: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(L));

    lua_pushinteger(L, ce_stringlist_getCount(c));
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function stringlist_getString_fromLua(L: Plua_State): integer; cdecl;
var parameters: integer;
  c: pointer;
  index: integer;
  r: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L, -2);
    index:=lua.lua_tointeger(L, -1);

    lua_pop(L, lua_gettop(L));


    lua_pushstring(L, tstringlist(c)[index]);
    result:=1;
  end else lua_pop(L, lua_gettop(L));

end;

function stringlist_add_fromLua(L: Plua_State): integer; cdecl;
var parameters: integer;
  c: pointer;
  s: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L, -2);
    s:=lua.lua_tostring(L, -1);
    ce_stringlist_add(c,s);
  end;

  lua_pop(L, lua_gettop(L));
end;

function stringlist_remove_fromLua(L: Plua_State): integer; cdecl;
var parameters: integer;
  c: pointer;
  s: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L, -2);
    s:=lua.lua_tostring(L, -1);
    ce_stringlist_remove(c,s);
  end;

  lua_pop(L, lua_gettop(L));
end;

function generateAPIHookScript_fromLua(L: PLua_state): integer; cdecl;
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

function createProcess_fromLua(L: PLua_state): integer; cdecl;
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

function AOBScan_fromLua(L: PLua_state): integer; cdecl;
var
  paramcount: integer;
  i,b: integer;
  scanstring: string;
  list: tstringlist;
begin
  result:=0;

  paramcount:=lua_gettop(L);
  if paramcount=0 then exit;

  if (paramcount=1) and (lua_isstring(L,-1)) then
    scanstring:=Lua_ToString(L, -1)
  else
  begin
    //buildup the scanstring
    scanstring:='';
    for i:=-paramcount to -1 do
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
  if getaoblist(scanstring, list) then
  begin
    result:=1;
    lua_pushlightuserdata(L, list);
  end
  else
    list.free;

end;

initialization
  LuaCS:=TCriticalSection.create;
  LuaVM:=lua_open();

  if LuaVM<>nil then
  begin
    luaL_openlibs(LuaVM);

    lua_atpanic(LuaVM, LuaPanic);
    lua_register(LuaVM, 'print', print_fromlua);
    lua_register(LuaVM, 'sleep', sleep_fromlua);
    lua_register(LuaVM, 'pause', pause_fromlua);
    lua_register(LuaVM, 'unpause', unpause_fromlua);
    lua_register(LuaVM, 'readBytes', readbytes_fromlua);
    lua_register(LuaVM, 'writeBytes', writebytes_fromlua);
    lua_register(LuaVM, 'readInteger', readInteger_fromlua);
    lua_register(LuaVM, 'readFloat', readFloat_fromlua);
    lua_register(LuaVM, 'readDouble', readDouble_fromlua);
    lua_register(LuaVM, 'readString', readString_fromlua);
    lua_register(LuaVM, 'writeInteger', writeInteger_fromlua);
    lua_register(LuaVM, 'writeFloat', writeFloat_fromlua);
    lua_register(LuaVM, 'writeDouble', writeDouble_fromlua);
    lua_register(LuaVM, 'writeString', writeString_fromlua);

    lua_register(LuaVM, 'readBytesLocal', readbyteslocal_fromlua);
    lua_register(LuaVM, 'writeBytesLocal', writebyteslocal_fromlua);
    lua_register(LuaVM, 'autoAssemble', autoAssemble_fromlua);
    lua_register(LuaVM, 'showMessage', showMessage_fromlua);
    lua_register(LuaVM, 'getPixel', getPixel_fromlua);
    lua_register(LuaVM, 'getMousePos', getMousePos_fromlua);
    lua_register(LuaVM, 'setMousePos', setMousePos_fromlua);
    lua_register(LuaVM, 'createTableEntry', createTableEntry_fromlua);
    lua_register(LuaVM, 'getTableEntry', getTableEntry_fromlua);
    lua_register(LuaVM, 'memrec_setDescription', memrec_setDescription_fromlua);
    lua_register(LuaVM, 'memrec_getDescription', memrec_getDescription_fromlua);
    lua_register(LuaVM, 'memrec_getAddress', memrec_getAddress_fromlua);
    lua_register(LuaVM, 'memrec_getType', memrec_getType_fromlua);
    lua_register(LuaVM, 'memrec_setType', memrec_setType_fromlua);
    lua_register(LuaVM, 'memrec_getValue', memrec_getValue_fromlua);
    lua_register(LuaVM, 'memrec_setValue', memrec_setValue_fromlua);
    lua_register(LuaVM, 'memrec_getScript', memrec_getScript_fromlua);
    lua_register(LuaVM, 'memrec_isActive', memrec_isActive_fromlua);
    lua_register(LuaVM, 'memrec_freeze', memrec_freeze_fromlua);
    lua_register(LuaVM, 'memrec_unfreeze', memrec_unfreeze_fromlua);
    lua_register(LuaVM, 'memrec_setColor', memrec_setColor_fromlua);
    lua_register(LuaVM, 'memrec_appendToEntry', memrec_appendToEntry_fromlua);
    lua_register(LuaVM, 'memrec_delete', memrec_delete_fromlua);
    lua_register(LuaVM, 'isKeyPressed', isKeyPressed_fromlua);
    lua_register(LuaVM, 'keyDown', keyDown_fromLua);
    lua_register(LuaVM, 'keyUp', keyUp_fromLua);
    lua_register(LuaVM, 'doKeyPress', doKeyPress_fromLua);
    lua_register(LuaVM, 'getProcessIDFromProcessName', getProcessIDFromProcessName_fromLua);
    lua_register(LuaVM, 'openProcess', openProcess_fromLua);
    lua_register(LuaVM, 'debugProcess', debugProcess_fromLua);
    lua_register(LuaVM, 'debug_setBreakpoint', debug_setBreakpoint_fromLua);
    lua_register(LuaVM, 'debug_removeBreakpoint', debug_removeBreakpoint_fromLua);
    lua_register(LuaVM, 'debug_continueFromBreakpoint', debug_continueFromBreakpoint_fromLua);
    lua_register(LuaVM, 'closeCE', closeCE_fromLua);
    lua_register(LuaVM, 'hideAllCEWindows', hideAllCEWindows_fromLua);
    lua_register(LuaVM, 'unhideMainCEwindow', unhideMainCEwindow_fromLua);
    lua_register(LuaVM, 'createForm', createForm_fromLua);
    lua_register(LuaVM, 'form_centerScreen', form_centerScreen_fromLua);
    lua_register(LuaVM, 'form_onClose', form_onClose_fromLua);
    lua_register(LuaVM, 'form_show', form_show_fromLua);
    lua_register(LuaVM, 'form_hide', form_hide_fromLua);
    lua_register(LuaVM, 'createPanel', createPanel_fromLua);
    lua_register(LuaVM, 'createGroupBox', createPanel_fromLua);
    lua_register(LuaVM, 'createButton', createButton_fromLua);
    lua_register(LuaVM, 'createImage', createImage_fromLua);
    lua_register(LuaVM, 'image_loadImageFromFile', image_loadImageFromFile_fromLua);
    lua_register(LuaVM, 'image_transparent', image_transparent_fromLua);
    lua_register(LuaVM, 'image_stretch', image_stretch_fromLua);
    lua_register(LuaVM, 'createLabel', createLabel_fromLua);
    lua_register(LuaVM, 'createEdit', createEdit_fromLua);
    lua_register(LuaVM, 'createMemo', createMemo_fromLua);
    lua_register(LuaVM, 'createTimer', createTimer_fromLua);
    lua_register(LuaVM, 'timer_setInterval', timer_setInterval_fromLua);
    lua_register(LuaVM, 'timer_onTimer', timer_onTimer_fromLua);
    lua_register(LuaVM, 'control_setCaption', control_setCaption_fromLua);
    lua_register(LuaVM, 'control_getCaption', control_getCaption_fromLua);
    lua_register(LuaVM, 'control_setPosition', control_setPosition_fromLua);
    lua_register(LuaVM, 'control_getPosition', control_getPosition_fromLua);
    lua_register(LuaVM, 'control_setSize', control_setSize_fromLua);
    lua_register(LuaVM, 'control_getSize', control_getSize_fromLua);
    lua_register(LuaVM, 'control_setAlign', control_setAlign_fromLua);
    lua_register(LuaVM, 'control_onClick', control_onClick_fromLua);
    lua_register(LuaVM, 'object_destroy', object_destroy_fromLua);
    lua_register(LuaVM, 'messageDialog', messageDialog_fromLua);
    lua_register(LuaVM, 'speedhack_setSpeed', speedhack_setSpeed_fromLua);
    lua_register(LuaVM, 'injectDLL', injectDLL_fromLua);
    lua_register(LuaVM, 'getAutoAttachList', getAutoAttachList_fromLua);
    lua_register(LuaVM, 'stringlist_getCount', stringlist_getCount_fromLua);
    lua_register(LuaVM, 'stringlist_getString', stringlist_getString_fromLua);
    lua_register(LuaVM, 'stringlist_add', stringlist_add_fromLua);
    lua_register(LuaVM, 'stringlist_remove', stringlist_remove_fromLua);
    lua_register(LuaVM, 'generateAPIHookScript', generateAPIHookScript_fromLua);
    lua_register(LuaVM, 'createProcess', createProcess_fromLua);
    lua_register(LuaVM, 'AOBScan', AOBScan_fromLua);

    LUA_DoScript('os=nil');

  end;

finalization
  if LuaCS<>nil then
    LuaCS.free;

  if LuaVM<>nil then
    lua_close(LuaVM);

end.

