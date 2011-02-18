unit LuaHandler;

{$mode delphi}

interface

uses
  windows, Classes, dialogs, SysUtils, lua, lualib, lauxlib, syncobjs, cefuncproc,
  newkernelhandler, autoassembler, Graphics, controls, LuaCaller, forms, ExtCtrls,
  StdCtrls, comctrls, ceguicomponents, generichotkey;

var
  LuaVM: Plua_State;
  LuaCS: Tcriticalsection;


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


implementation

uses mainunit, frmluaengineunit, pluginexports, MemoryRecordUnit, debuggertypedefinitions,
  symbolhandler, frmautoinjectunit, simpleaobscanner;

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


       // lua_pop(LuaVM, lua_gettop(luavm));
        p:=lua_gettop(luavm);

        if lua_pcall(LuaVM, 0, 1, 0)=0 then
        begin
          p:=lua_gettop(luavm);

          if (p=1) then //only 1 parameter returned
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
      //ShowMessage(inttostr(lua_type(L, -1)));

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

      v:=lua_tointeger(L, -1);

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

      v:=lua_tonumber(L, -1);

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

      v:=lua_tonumber(L, -1);

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

      v:=lua.lua_tostring(L, -1);

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

  if lua_isstring(L, -paramcount) then
    addresstoread:=symhandler.getAddressFromName(lua_tostring(L,-paramcount))
  else
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
  i,j: integer;
  address: ptruint;
  x: dword;
  oldprotect: dword;
  b: byte;
begin
  paramcount:=lua_gettop(L);
  if paramcount=0 then exit;

  setlength(bytes,paramcount-1);

  if lua_isstring(L, -paramcount) then
    address:=symhandler.getAddressFromName(lua_tostring(L,-paramcount))
  else
    address:=lua_tointeger(L,-paramcount);


  j:=0;
  for i:=(-paramcount)+1 to -1 do
  begin
    b:=lua_tointeger(L,i);
    bytes[j]:=b;
    inc(j);
  end;

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
  end else lua_pop(L, paramcount);
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
    lua_pop(L, paramcount);

    lua_pushboolean(L, ce_memrec_isFrozen(memrec));
    result:=1;
  end
  else lua_pop(L, paramcount);
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

function beep_fromLua(L: PLua_state): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L)); //clear the stack
  beep;
  result:=0;
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

    try
      ce_debug_setBreakpoint(address,size,TBreakpointTrigger(trigger));
    except
    end;
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

function form_showModal_fromLua(L: Plua_State): integer; cdecl;
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

function createHotkey_fromLua(L: Plua_State): integer; cdecl;
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
  timer: TTimer;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    timer:=lua_touserdata(L,-2);


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

function control_getAlign_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TControl;
  align: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, integer(control.Align));
    result:=1;

  end else lua_pop(L, paramcount);
end;

function control_setEnabled_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TControl;
  Enabled: boolean;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);
    control.Enabled:=lua_toboolean(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function control_getEnabled_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TControl;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushboolean(L, control.Enabled);
    result:=1;

  end else lua_pop(L, paramcount);
end;


function control_setVisible_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TControl;
  visible: boolean;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);
    control.visible:=lua_toboolean(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function control_getVisible_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TControl;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushboolean(L, control.Visible);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function control_setColor_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TControl;
  Color: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);
    control.Color:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function control_getColor_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TControl;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, control.color);
    result:=1;

  end else lua_pop(L, paramcount);
end;


function control_setParent_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TControl;
  Parent: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);
    control.Parent:=TWinControl(lua_touserdata(L,-1));
  end;

  lua_pop(L, paramcount);
end;

function control_getParent_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TControl;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    control:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushlightuserdata(L, control.Parent);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function wincontrol_getControlCount_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  wincontrol: TWinControl;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    wincontrol:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, wincontrol.ControlCount);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function wincontrol_getControl_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  wincontrol: TWinControl;
  index: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    wincontrol:=lua_touserdata(L,-2);
    index:=lua_tointeger(L,-1);
    lua_pop(L, paramcount);

    lua_pushlightuserdata(L, wincontrol.Controls[index]);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function wincontrol_onEnter_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  wincontrol: TWinControl;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    wincontrol:=lua_touserdata(L,-2);


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

  lua_pop(L, paramcount);
end;

function wincontrol_onExit_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  wincontrol: TWinControl;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    wincontrol:=lua_touserdata(L,-2);


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

  lua_pop(L, paramcount);
end;

function wincontrol_canFocus_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  wincontrol: TWinControl;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    wincontrol:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushboolean(L, wincontrol.CanFocus);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function wincontrol_focused_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  wincontrol: TWinControl;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    wincontrol:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushboolean(L, wincontrol.Focused);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function wincontrol_setFocus_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  wincontrol: TWinControl;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    wincontrol:=lua_touserdata(L,-1);
    wincontrol.SetFocus;
  end;

  lua_pop(L, paramcount);
end;

function strings_add_fromLua(L: Plua_State): integer; cdecl;
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

function strings_clear_fromLua(L: Plua_State): integer; cdecl;
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

function strings_remove_fromLua(L: Plua_State): integer; cdecl;  //compat with ce 6
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

function strings_getString_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  strings: TStrings;
  index: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    strings:=lua_touserdata(L,-2);
    index:=lua_toInteger(L,-1);
    lua_pop(L, paramcount);

    lua_pushstring(L, strings[index]);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function strings_setString_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  strings: TStrings;
  index: integer;
  s: string;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=3 then
  begin
    strings:=lua_touserdata(L,-3);
    index:=lua_toInteger(L,-2);
    s:=lua_tostring(l,-1);

    strings[index]:=s;
  end;
  lua_pop(L, paramcount);
end;

function strings_delete_fromLua(L: Plua_State): integer; cdecl;
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

function strings_append_fromLua(L: Plua_State): integer; cdecl;
var parameters: integer;
  strings: TStrings;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    strings:=lua_touserdata(L, -2);
    strings.Append(lua_tostring(L, -1));
  end;

  lua_pop(L, lua_gettop(L));
end;

function strings_getText_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  strings: TStrings;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    strings:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushstring(L, strings.Text);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function strings_indexOf_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  strings: TStrings;
  s: string;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    strings:=lua_touserdata(L,-2);
    s:=Lua_ToString(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, strings.IndexOf(s));
    result:=1;

  end else lua_pop(L, paramcount);
end;

function strings_insert_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  strings: TStrings;
  index: integer;
  s: string;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=3 then
  begin
    strings:=lua_touserdata(L,-3);
    index:=lua_tointeger(L,-2);
    s:=Lua_ToString(L,-1);

    strings.Insert(index,s);
  end;
  lua_pop(L, paramcount);
end;


function strings_getCount_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  strings: TStrings;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    strings:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, strings.Count);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function strings_loadFromFile_fromLua(L: Plua_State): integer; cdecl;
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

function strings_saveToFile_fromLua(L: Plua_State): integer; cdecl;
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



function stringlist_getDuplicates_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  stringlist: TStringlist;
  align: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    stringlist:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, integer(stringlist.Duplicates));
    result:=1;

  end else lua_pop(L, paramcount);
end;

function stringlist_setDuplicates_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  stringlist: TStringlist;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    stringlist:=lua_touserdata(L,-2);
    stringlist.Duplicates:=TDuplicates(lua_tointeger(L,-1));
  end;

  lua_pop(L, paramcount);
end;


function stringlist_getSorted_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  stringlist: TStringlist;
  align: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    stringlist:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushboolean(L, stringlist.Sorted);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function stringlist_setSorted_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  stringlist: TStringlist;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    stringlist:=lua_touserdata(L,-2);
    stringlist.Sorted:=lua_toboolean(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function stringlist_getCaseSensitive_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  stringlist: TStringlist;
  align: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    stringlist:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushboolean(L, stringlist.CaseSensitive);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function stringlist_setCaseSensitive_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  stringlist: TStringlist;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    stringlist:=lua_touserdata(L,-2);
    stringlist.CaseSensitive:=lua_toboolean(L,-1);
  end;

  lua_pop(L, paramcount);
end;


function form_onClose_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TCustomForm;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);


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

  lua_pop(L, paramcount);
end;

function control_onClick_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TControl;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);


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




function getOpenedProcessID_fromLua(L: PLua_state): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));
  result:=1;
  lua_pushinteger(L, processid);
end;

function getAddress_fromLua(L: PLua_state): integer; cdecl;
var paramcount: integer;
  s: string;

begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount>=1 then
  begin
    s:=Lua_ToString(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushnumber(L,symhandler.getAddressFromName(s));
    result:=1;
  end;

end;

function reinitializeSymbolhandler_fromLua(L: PLua_state): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));
  result:=0;
  symhandler.reinitialize;
  symhandler.waitforsymbolsloaded;
end;

function getPropertyList_fromLua(L: PLua_state): integer; cdecl;
var paramcount: integer;
  c: tobject;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    c:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushlightuserdata(L, ce_getPropertylist(c));
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function getProperty_fromLua(L: PLua_state): integer; cdecl;
var paramcount: integer;
  c: tobject;
  p: string;
  buf: pchar;

  size: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
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

function setProperty_fromLua(L: PLua_state): integer; cdecl;
var paramcount: integer;
  c: tobject;
  p,v: string;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=3 then
  begin
    c:=lua_touserdata(L, -3);
    p:=Lua_ToString(L, -2);
    v:=Lua_ToString(L, -1);

    ce_setProperty(c,pchar(p),pchar(v));
  end;

  lua_pop(L, lua_gettop(l));
end;

function object_getClassName_fromLua(L: PLua_state): integer; cdecl;
var c: TObject;
  paramcount: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    c:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushstring(L, c.ClassName);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;




function getMainForm_fromLua(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  lua_pushlightuserdata(l, mainform);
end;

function getAddressList_fromLua(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  lua_pushlightuserdata(l, mainform.addresslist);
end;

function getFreezeTimer_fromLua(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  lua_pushlightuserdata(l, mainform.FreezeTimer);
end;

function getUpdateTimer_fromLua(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  lua_pushlightuserdata(l, mainform.UpdateTimer);
end;



function inheritsFromObject_fromLua(L: PLua_state): integer; cdecl;
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

function inheritsFromComponent_fromLua(L: PLua_state): integer; cdecl;
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

function inheritsFromControl_fromLua(L: PLua_state): integer; cdecl;
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

function inheritsFromWinControl_fromLua(L: PLua_state): integer; cdecl;
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

function component_getComponentCount_fromLua(L: PLua_state): integer; cdecl;
var c: TComponent;
  paramcount: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    c:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushinteger(L, c.ComponentCount);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function component_getComponent_fromLua(L: PLua_state): integer; cdecl;
var c: TComponent;
  i: integer;
  paramcount: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    c:=lua_touserdata(L, -2);
    i:=lua_tointeger(L,-1);
    lua_pop(L, lua_gettop(l));

    lua_pushlightuserdata(L, c.Components[i]);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function component_getName_fromLua(L: PLua_state): integer; cdecl;
var c: TComponent;
  paramcount: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    c:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushstring(L, c.Name);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function component_setName_fromLua(L: PLua_state): integer; cdecl;
var c: TComponent;
  paramcount: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    c:=lua_touserdata(L, -2);
    c.Name:=lua_tostring(L,-1);
  end;
  lua_pop(L, lua_gettop(l));
end;

function component_getTag_fromLua(L: PLua_state): integer; cdecl;
var c: TComponent;
  paramcount: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    c:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushinteger(L, c.Tag);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function component_setTag_fromLua(L: PLua_state): integer; cdecl;
var c: TComponent;
  t: integer;
  paramcount: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    c:=lua_touserdata(L, -2);
    c.Tag:=lua_tointeger(L, -1);
  end;
  lua_pop(L, lua_gettop(l));
end;


function component_getOwner_fromLua(L: PLua_state): integer; cdecl;
var c: TComponent;
  paramcount: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    c:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushlightuserdata(L, c.Owner);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function panel_getAlignment_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  panel: Tcustompanel;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    panel:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, integer(panel.Alignment));
    result:=1;

  end else lua_pop(L, paramcount);
end;

function panel_setAlignment_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  panel: Tcustompanel;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    panel:=lua_touserdata(L,-2);
    panel.Alignment:=TAlignment(lua_tointeger(L,-1));
  end;

  lua_pop(L, paramcount);
end;

function panel_getBevelInner_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  panel: Tcustompanel;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    panel:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, integer(panel.BevelInner));
    result:=1;

  end else lua_pop(L, paramcount);
end;

function panel_setBevelInner_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  panel: Tcustompanel;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    panel:=lua_touserdata(L,-2);
    panel.BevelInner:=TPanelBevel(lua_tointeger(L,-1));
  end;

  lua_pop(L, paramcount);
end;

function panel_getBevelOuter_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  panel: Tcustompanel;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    panel:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, integer(panel.BevelOuter));
    result:=1;

  end else lua_pop(L, paramcount);
end;

function panel_setBevelOuter_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  panel: Tcustompanel;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    panel:=lua_touserdata(L,-2);
    panel.BevelOuter:=TPanelBevel(lua_tointeger(L,-1));
  end;

  lua_pop(L, paramcount);
end;

function panel_getBevelWidth_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  panel: Tcustompanel;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    panel:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, panel.BevelWidth);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function panel_setBevelWidth_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  panel: Tcustompanel;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    panel:=lua_touserdata(L,-2);
    panel.BevelWidth:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function panel_getFullRepaint_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  panel: Tcustompanel;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    panel:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushboolean(L, panel.FullRepaint);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function panel_setFullRepaint_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  panel: Tcustompanel;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    panel:=lua_touserdata(L,-2);
    panel.FullRepaint:=lua_toboolean(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function edit_clear_fromLua(L: Plua_State): integer; cdecl;
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

function edit_selectAll_fromLua(L: Plua_State): integer; cdecl;
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

function edit_clearSelection_fromLua(L: Plua_State): integer; cdecl;
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

function edit_copyToClipboard_fromLua(L: Plua_State): integer; cdecl;
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

function edit_cutToClipboard_fromLua(L: Plua_State): integer; cdecl;
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

function edit_pasteFromClipboard_fromLua(L: Plua_State): integer; cdecl;
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

function edit_onChange_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TCustomEdit;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);


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

  lua_pop(L, paramcount);
end;

function memo_append_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  memo: TCustomMemo;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    memo:=lua_touserdata(L,-2);
    memo.Append(Lua_ToString(L,-1));
  end;

  lua_pop(L, paramcount);
end;

function memo_getLines_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  memo: TCustomMemo;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    memo:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushlightuserdata(L, memo.Lines);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function memo_getWordWrap_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  memo: Tcustommemo;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    memo:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushboolean(L, memo.WordWrap);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function memo_setWordWrap_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  memo: Tcustommemo;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    memo:=lua_touserdata(L,-2);
    memo.WordWrap:=lua_toboolean(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function memo_getWantTabs_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  memo: Tcustommemo;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    memo:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushboolean(L, memo.WantTabs);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function memo_setWantTabs_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  memo: Tcustommemo;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    memo:=lua_touserdata(L,-2);
    memo.WantTabs:=lua_toboolean(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function memo_getWantReturns_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  memo: Tcustommemo;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    memo:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushboolean(L, memo.WantReturns);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function memo_setWantReturns_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  memo: Tcustommemo;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    memo:=lua_touserdata(L,-2);
    memo.WantReturns:=lua_toboolean(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function memo_getScrollbars_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  memo: Tcustommemo;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    memo:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, integer(memo.Scrollbars));
    result:=1;

  end else lua_pop(L, paramcount);
end;

function memo_setScrollbars_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  memo: Tcustommemo;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    memo:=lua_touserdata(L,-2);
    memo.Scrollbars:=TScrollStyle(lua_tointeger(L,-1));
  end;

  lua_pop(L, paramcount);
end;

function button_getModalResult_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  button: Tcustombutton;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    button:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, integer(button.ModalResult));
    result:=1;

  end else lua_pop(L, paramcount);
end;

function button_setModalResult_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  button: Tcustombutton;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    button:=lua_touserdata(L,-2);
    button.ModalResult:=TModalResult(lua_tointeger(L,-1));
  end;

  lua_pop(L, paramcount);
end;


function checkbox_getAllowGrayed_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  checkbox: Tcustomcheckbox;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    checkbox:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushboolean(L, checkbox.AllowGrayed);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function checkbox_setAllowGrayed_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  checkbox: Tcustomcheckbox;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    checkbox:=lua_touserdata(L,-2);
    checkbox.AllowGrayed:=lua_toboolean(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function checkbox_getState_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  checkbox: Tcustomcheckbox;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    checkbox:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, integer(checkbox.State));
    result:=1;

  end else lua_pop(L, paramcount);
end;

function checkbox_setState_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  checkbox: Tcustomcheckbox;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    checkbox:=lua_touserdata(L,-2);
    checkbox.State:=TCheckBoxState(lua_tointeger(L,-1));
  end;

  lua_pop(L, paramcount);
end;

function checkbox_onChange_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TCustomCheckBox;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);


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

  lua_pop(L, paramcount);
end;

function radiogroup_getRows_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  radiogroup: TCustomRadioGroup;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    radiogroup:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, radiogroup.Rows);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function radiogroup_getItems_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  radiogroup: TCustomRadioGroup;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    radiogroup:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushlightuserdata(L, radiogroup.items);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function radiogroup_getColumns_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  radiogroup: Tcustomradiogroup;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    radiogroup:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, radiogroup.Columns);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function radiogroup_setColumns_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  radiogroup: Tcustomradiogroup;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    radiogroup:=lua_touserdata(L,-2);
    radiogroup.Columns:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function radiogroup_onClick_fromLua(L: PLua_State): integer; cdecl; //for some reason the radiogroup has it's own fonclick variable
var
  paramcount: integer;
  control: TCustomRadioGroup;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);


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

  lua_pop(L, paramcount);
end;

function listbox_clear_fromLua(L: Plua_State): integer; cdecl;
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


function listbox_getItems_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listbox: TCustomlistbox;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listbox:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushlightuserdata(L, listbox.items);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listbox_getItemIndex_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listbox: Tcustomlistbox;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listbox:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, listbox.ItemIndex);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listbox_setItemIndex_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listbox: Tcustomlistbox;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    listbox:=lua_touserdata(L,-2);
    listbox.itemindex:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;

//combobox
function combobox_clear_fromLua(L: Plua_State): integer; cdecl;
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


function combobox_getItems_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  combobox: TCustomcombobox;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    combobox:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushlightuserdata(L, combobox.items);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function combobox_getItemIndex_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  combobox: Tcustomcombobox;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    combobox:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, combobox.ItemIndex);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function combobox_setItemIndex_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  combobox: Tcustomcombobox;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    combobox:=lua_touserdata(L,-2);
    combobox.itemindex:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;


function progressbar_stepIt_fromLua(L: Plua_State): integer; cdecl;
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

function progressbar_stepBy_fromLua(L: Plua_State): integer; cdecl;
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


function progressbar_getMax_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  progressbar: Tcustomprogressbar;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    progressbar:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, progressbar.max);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function progressbar_setMax_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  progressbar: Tcustomprogressbar;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    progressbar:=lua_touserdata(L,-2);
    progressbar.max:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function progressbar_getMin_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  progressbar: Tcustomprogressbar;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    progressbar:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, progressbar.Min);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function progressbar_setMin_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  progressbar: Tcustomprogressbar;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    progressbar:=lua_touserdata(L,-2);
    progressbar.Min:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;


function progressbar_getPosition_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  progressbar: Tcustomprogressbar;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    progressbar:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, progressbar.Position);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function progressbar_setPosition_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  progressbar: Tcustomprogressbar;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    progressbar:=lua_touserdata(L,-2);
    progressbar.Position:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;

//trackbar
function trackbar_getMax_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  trackbar: Tcustomtrackbar;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    trackbar:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, trackbar.max);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function trackbar_setMax_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  trackbar: Tcustomtrackbar;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    trackbar:=lua_touserdata(L,-2);
    trackbar.max:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function trackbar_getMin_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  trackbar: Tcustomtrackbar;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    trackbar:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, trackbar.Min);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function trackbar_setMin_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  trackbar: Tcustomtrackbar;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    trackbar:=lua_touserdata(L,-2);
    trackbar.Min:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;


function trackbar_getPosition_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  trackbar: Tcustomtrackbar;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    trackbar:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, trackbar.Position);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function trackbar_setPosition_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  trackbar: Tcustomtrackbar;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    trackbar:=lua_touserdata(L,-2);
    trackbar.Position:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function trackbar_onChange_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  control: TCustomTrackBar;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    control:=lua_touserdata(L,-2);


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

  lua_pop(L, paramcount);
end;

function listcolumn_setAutosize_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listcolumns: TListColumn;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    listcolumns:=lua_touserdata(L,-2);
    listcolumns.AutoSize:=lua_toboolean(L,-1);
  end;

  lua_pop(L, paramcount);
end;


function listcolumn_getCaption_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listcolumn: Tlistcolumn;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listcolumn:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushstring(L, listcolumn.caption);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listcolumn_setCaption_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listcolumn: Tlistcolumn;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    listcolumn:=lua_touserdata(L,-2);
    listcolumn.caption:=Lua_ToString(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function listcolumn_getMaxWidth_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listcolumn: Tlistcolumn;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listcolumn:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, listcolumn.maxwidth);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listcolumn_setMaxWidth_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listcolumn: Tlistcolumn;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    listcolumn:=lua_touserdata(L,-2);
    listcolumn.maxwidth:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function listcolumn_getMinWidth_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listcolumn: Tlistcolumn;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listcolumn:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, listcolumn.Minwidth);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listcolumn_setMinWidth_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listcolumn: Tlistcolumn;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    listcolumn:=lua_touserdata(L,-2);
    listcolumn.Minwidth:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function listcolumn_getWidth_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listcolumn: Tlistcolumn;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listcolumn:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, listcolumn.width);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listcolumn_setWidth_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listcolumn: Tlistcolumn;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    listcolumn:=lua_touserdata(L,-2);
    listcolumn.width:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function collection_clear_fromLua(L: Plua_State): integer; cdecl;
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

function collection_getCount_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  collection: Tcollection;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    collection:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, collection.Count);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function collection_delete_fromLua(L: Plua_State): integer; cdecl;
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

function listcolumns_add_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listcolumns: TListColumns;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listcolumns:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushlightuserdata(L, listcolumns.Add);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listcolumns_getColumn_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listcolumns: TListcolumns;
  index: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    listcolumns:=lua_touserdata(L,-2);
    index:=lua_toInteger(L,-1);
    lua_pop(L, paramcount);

    lua_pushlightuserdata(L, listcolumns[index]);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listitem_delete_fromLua(L: Plua_State): integer; cdecl;
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

function listitem_getCaption_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listitem: Tlistitem;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listitem:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushstring(L, listitem.caption);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listitem_setCaption_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listitem: Tlistitem;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    listitem:=lua_touserdata(L,-2);
    listitem.Caption:=Lua_ToString(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function listitem_getSubItems_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listitem: Tlistitem;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listitem:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushlightuserdata(L, listitem.SubItems);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listitems_clear_fromLua(L: Plua_State): integer; cdecl;
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

function listitems_getCount_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listitems: Tlistitems;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listitems:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, listitems.Count);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listitems_add_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listitems: Tlistitems;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listitems:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushlightuserdata(L, listitems.Add);
    result:=1;

  end else lua_pop(L, paramcount);
end;


function listview_clear_fromLua(L: Plua_State): integer; cdecl;
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

function listview_getColumns_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listview: TCEListView;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listview:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushlightuserdata(L, listview.Columns);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listview_getItems_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listview: TCustomListView;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listview:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushlightuserdata(L, listview.Items);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listview_getItemIndex_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listview: Tcustomlistview;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    listview:=lua_touserdata(L,-1);
    lua_pop(L, paramcount);

    lua_pushinteger(L, listview.ItemIndex);
    result:=1;

  end else lua_pop(L, paramcount);
end;

function listview_setItemIndex_fromLua(L: PLua_State): integer; cdecl;
var
  paramcount: integer;
  listview: Tcustomlistview;
  a: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount=2 then
  begin
    listview:=lua_touserdata(L,-2);
    listview.itemindex:=lua_tointeger(L,-1);
  end;

  lua_pop(L, paramcount);
end;

function opendialog_execute_fromLua(L: Plua_State): integer; cdecl;
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
    lua_register(LuaVM, 'memrec_setAddress', memrec_setAddress_fromlua);
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


    lua_register(LuaVM, 'messageDialog', messageDialog_fromLua);
    lua_register(LuaVM, 'speedhack_setSpeed', speedhack_setSpeed_fromLua);
    lua_register(LuaVM, 'injectDLL', injectDLL_fromLua);
    lua_register(LuaVM, 'getAutoAttachList', getAutoAttachList_fromLua);
    {
    lua_register(LuaVM, 'stringlist_getCount', stringlist_getCount_fromLua);
    lua_register(LuaVM, 'stringlist_getString', stringlist_getString_fromLua);
    lua_register(LuaVM, 'stringlist_getFullText', stringlist_getFullText_fromLua);  //6.1
    lua_register(LuaVM, 'stringlist_add', stringlist_add_fromLua);
    lua_register(LuaVM, 'stringlist_remove', stringlist_remove_fromLua);   }

    lua_register(LuaVM, 'generateAPIHookScript', generateAPIHookScript_fromLua);
    lua_register(LuaVM, 'createProcess', createProcess_fromLua);
    lua_register(LuaVM, 'AOBScan', AOBScan_fromLua);
    lua_register(LuaVM, 'getOpenedProcessID', getOpenedProcessID_fromLua);
    lua_register(LuaVM, 'getAddress', getAddress_fromLua);
    lua_register(LuaVM, 'reinitializeSymbolhandler', reinitializeSymbolhandler_fromLua);

    //ce6.1
    lua_register(LuaVM, 'createHotkey', createHotkey_fromLua);

    lua_register(LuaVM, 'getPropertyList', getPropertyList_fromLua);
    lua_register(LuaVM, 'setProperty', setProperty_fromLua);
    lua_register(LuaVM, 'getProperty', getProperty_fromLua);

    lua_register(LuaVM, 'object_getClassName', object_getClassName_fromLua);
    lua_register(LuaVM, 'object_destroy', object_destroy_fromLua);

    lua_register(LuaVM, 'component_getComponentCount', component_getComponentCount_fromLua);
    lua_register(LuaVM, 'component_getComponent', component_getComponent_fromLua);
    lua_register(LuaVM, 'component_getName', component_getName_fromLua);
    lua_register(LuaVM, 'component_setName', component_setName_fromLua);
    lua_register(LuaVM, 'component_getTag', component_getTag_fromLua);
    lua_register(LuaVM, 'component_setTag', component_setTag_fromLua);
    lua_register(LuaVM, 'component_getOwner', component_getOwner_fromLua);

    lua_register(LuaVM, 'control_setCaption', control_setCaption_fromLua);
    lua_register(LuaVM, 'control_getCaption', control_getCaption_fromLua);
    lua_register(LuaVM, 'control_setPosition', control_setPosition_fromLua);
    lua_register(LuaVM, 'control_getPosition', control_getPosition_fromLua);
    lua_register(LuaVM, 'control_setSize', control_setSize_fromLua);
    lua_register(LuaVM, 'control_getSize', control_getSize_fromLua);
    lua_register(LuaVM, 'control_setAlign', control_setAlign_fromLua);
    lua_register(LuaVM, 'control_getAlign', control_getAlign_fromLua);
    lua_register(LuaVM, 'control_onClick', control_onClick_fromLua);
    lua_register(LuaVM, 'control_setEnabled', control_setEnabled_fromLua);
    lua_register(LuaVM, 'control_getEnabled', control_getEnabled_fromLua);
    lua_register(LuaVM, 'control_setVisible', control_setVisible_fromLua);
    lua_register(LuaVM, 'control_getVisible', control_getVisible_fromLua);
    lua_register(LuaVM, 'control_setColor', control_setColor_fromLua);
    lua_register(LuaVM, 'control_getColor', control_getColor_fromLua);
    lua_register(LuaVM, 'control_setParent', control_setParent_fromLua);
    lua_register(LuaVM, 'control_getParent', control_getParent_fromLua);

    lua_register(LuaVM, 'wincontrol_getControlCount', wincontrol_getControlCount_fromLua);
    lua_register(LuaVM, 'wincontrol_getControl', wincontrol_getControl_fromLua);
    lua_register(LuaVM, 'wincontrol_OnEnter', wincontrol_OnEnter_fromLua);
    lua_register(LuaVM, 'wincontrol_OnExit', wincontrol_OnExit_fromLua);
    lua_register(LuaVM, 'wincontrol_canFocus', wincontrol_canFocus_fromLua);
    lua_register(LuaVM, 'wincontrol_focused', wincontrol_focused_fromLua);
    lua_register(LuaVM, 'wincontrol_setFocus', wincontrol_setFocus_fromLua);

    lua_register(LuaVM, 'strings_add', strings_add_fromLua);
    lua_register(LuaVM, 'strings_clear', strings_clear_fromLua);
    lua_register(LuaVM, 'strings_delete', strings_delete_fromLua);
    lua_register(LuaVM, 'strings_append', strings_append_fromLua);
    lua_register(LuaVM, 'strings_getText', strings_getText_fromLua);
    lua_register(LuaVM, 'strings_indexOf', strings_indexOf_fromLua);
    lua_register(LuaVM, 'strings_insert', strings_insert_fromLua);
    lua_register(LuaVM, 'strings_getCount', strings_getCount_fromLua);
    lua_register(LuaVM, 'strings_remove', strings_remove_fromLua);
    lua_register(LuaVM, 'strings_getString', strings_getString_fromLua);
    lua_register(LuaVM, 'strings_setString', strings_setString_fromLua);

    lua_register(LuaVM, 'stringlist_getDuplicates', stringlist_getDuplicates_fromLua);
    lua_register(LuaVM, 'stringlist_setDuplicates', stringlist_setDuplicates_fromLua);
    lua_register(LuaVM, 'stringlist_getSorted', stringlist_getSorted_fromLua);
    lua_register(LuaVM, 'stringlist_getSorted', stringlist_setSorted_fromLua);
    lua_register(LuaVM, 'stringlist_getCaseSensitive', stringlist_getCaseSensitive_fromLua);
    lua_register(LuaVM, 'stringlist_getCaseSensitive', stringlist_setCaseSensitive_fromLua);

    lua_register(LuaVM, 'form_centerScreen', form_centerScreen_fromLua);
    lua_register(LuaVM, 'form_onClose', form_onClose_fromLua);
    lua_register(LuaVM, 'form_show', form_show_fromLua);
    lua_register(LuaVM, 'form_hide', form_hide_fromLua);
    lua_register(LuaVM, 'form_showModal', form_showModal_fromLua);


    lua_register(LuaVM, 'panel_getAlignment', panel_getAlignment_fromLua);
    lua_register(LuaVM, 'panel_setAlignment', panel_setAlignment_fromLua);
    lua_register(LuaVM, 'panel_getBevelInner', panel_getBevelInner_fromLua);
    lua_register(LuaVM, 'panel_setBevelInner', panel_setBevelInner_fromLua);
    lua_register(LuaVM, 'panel_getBevelOuter', panel_getBevelOuter_fromLua);
    lua_register(LuaVM, 'panel_setBevelOuter', panel_setBevelOuter_fromLua);
    lua_register(LuaVM, 'panel_getBevelWidth', panel_getBevelWidth_fromLua);
    lua_register(LuaVM, 'panel_setBevelWidth', panel_setBevelWidth_fromLua);
    lua_register(LuaVM, 'panel_getFullRepaint', panel_getFullRepaint_fromLua);
    lua_register(LuaVM, 'panel_setFullRepaint', panel_setFullRepaint_fromLua);

    lua_register(LuaVM, 'edit_clear', edit_clear_fromLua);
    lua_register(LuaVM, 'edit_selectAll', edit_selectAll_fromLua);
    lua_register(LuaVM, 'edit_clearSelection', edit_clearSelection_fromLua);
    lua_register(LuaVM, 'edit_copyToClipboard', edit_copyToClipboard_fromLua);
    lua_register(LuaVM, 'edit_cutToClipboard', edit_cutToClipboard_fromLua);
    lua_register(LuaVM, 'edit_pasteFromClipboard', edit_pasteFromClipboard_fromLua);
    lua_register(LuaVM, 'edit_onChange', edit_onChange_fromLua);

    lua_register(LuaVM, 'memo_append', memo_append_fromLua);
    lua_register(LuaVM, 'memo_getLines', memo_getLines_fromLua);
    lua_register(LuaVM, 'memo_getWordWrap', memo_getWordWrap_fromLua);
    lua_register(LuaVM, 'memo_setWordWrap', memo_setWordWrap_fromLua);
    lua_register(LuaVM, 'memo_getWantTabs', memo_getWantTabs_fromLua);
    lua_register(LuaVM, 'memo_setWantTabs', memo_setWantTabs_fromLua);
    lua_register(LuaVM, 'memo_getWantReturns', memo_getWantReturns_fromLua);
    lua_register(LuaVM, 'memo_setWantReturns', memo_setWantReturns_fromLua);
    lua_register(LuaVM, 'memo_getScrollbars', memo_getScrollbars_fromLua);
    lua_register(LuaVM, 'memo_setScrollbars', memo_setScrollbars_fromLua);

    lua_register(LuaVM, 'button_getModalResult', button_getModalResult_fromLua);
    lua_register(LuaVM, 'button_setModalResult', button_setModalResult_fromLua);


    lua_register(LuaVM, 'checkbox_getAllowGrayed', checkbox_getAllowGrayed_fromLua);
    lua_register(LuaVM, 'checkbox_setAllowGrayed', checkbox_setAllowGrayed_fromLua);
    lua_register(LuaVM, 'checkbox_getState', checkbox_getState_fromLua);
    lua_register(LuaVM, 'checkbox_setState', checkbox_setState_fromLua);
    lua_register(LuaVM, 'checkbox_onChange', checkbox_onChange_fromLua);

    lua_register(LuaVM, 'radiogroup_getRows', radiogroup_getRows_fromLua);
    lua_register(LuaVM, 'radiogroup_getItems', radioGroup_getItems_fromLua);
    lua_register(LuaVM, 'radiogroup_getColumns', radiogroup_getColumns_fromLua);
    lua_register(LuaVM, 'radiogroup_setColumns', radiogroup_setColumns_fromLua);
    lua_register(LuaVM,' radiogroup_onClick', radiogroup_onClick_fromLua);

    lua_register(LuaVM, 'listbox_clear', listbox_clear_fromLua);
    lua_register(LuaVM, 'listbox_getItems', listbox_getItems_fromLua);
    lua_register(LuaVM, 'listbox_getItemIndex', listbox_getItemIndex_fromLua);
    lua_register(LuaVM, 'listbox_setItemIndex', listbox_setItemIndex_fromLua);

    lua_register(LuaVM, 'combobox_clear', combobox_clear_fromLua);
    lua_register(LuaVM, 'combobox_getItems', combobox_getItems_fromLua);
    lua_register(LuaVM, 'combobox_getItemIndex', combobox_getItemIndex_fromLua);
    lua_register(LuaVM, 'combobox_setItemIndex', combobox_setItemIndex_fromLua);

    lua_register(LuaVM, 'progressbar_stepIt', progressbar_stepIt_fromLua);
    lua_register(LuaVM, 'progressbar_stepBy', progressbar_stepBy_fromLua);
    lua_register(LuaVM, 'progressbar_getMax', progressbar_getMax_fromLua);
    lua_register(LuaVM, 'progressbar_setMax', progressbar_setMax_fromLua);
    lua_register(LuaVM, 'progressbar_getMin', progressbar_getMin_fromLua);
    lua_register(LuaVM, 'progressbar_setMin', progressbar_setMin_fromLua);
    lua_register(LuaVM, 'progressbar_getPosition', progressbar_getPosition_fromLua);
    lua_register(LuaVM, 'progressbar_setPosition', progressbar_setPosition_fromLua);

    lua_register(LuaVM, 'trackbar_getMax', trackbar_getMax_fromLua);
    lua_register(LuaVM, 'trackbar_setMax', trackbar_setMax_fromLua);
    lua_register(LuaVM, 'trackbar_getMin', trackbar_getMin_fromLua);
    lua_register(LuaVM, 'trackbar_setMin', trackbar_setMin_fromLua);
    lua_register(LuaVM, 'trackbar_getPosition', trackbar_getPosition_fromLua);
    lua_register(LuaVM, 'trackbar_setPosition', trackbar_setPosition_fromLua);
    lua_register(LuaVM, 'trackbar_onChange', trackbar_onChange_fromLua);


    lua_register(LuaVM, 'listcolumn_setAutosize', listcolumn_setAutosize_fromLua);
    lua_register(LuaVM, 'listcolumn_getCaption', listcolumn_getCaption_fromLua);
    lua_register(LuaVM, 'listcolumn_setCaption', listcolumn_setCaption_fromLua);
    lua_register(LuaVM, 'listcolumn_getMaxWidth', listcolumn_getMaxWidth_fromLua);
    lua_register(LuaVM, 'listcolumn_setMaxWidth', listcolumn_setMaxWidth_fromLua);
    lua_register(LuaVM, 'listcolumn_getMinWidth', listcolumn_getMinWidth_fromLua);
    lua_register(LuaVM, 'listcolumn_setMinWidth', listcolumn_setMinWidth_fromLua);
    lua_register(LuaVM, 'listcolumn_getWidth', listcolumn_getWidth_fromLua);
    lua_register(LuaVM, 'listcolumn_setWidth', listcolumn_setWidth_fromLua);


    lua_register(LuaVM, 'collection_clear', collection_clear_fromLua);
    lua_register(LuaVM, 'collection_getCount', collection_getCount_fromLua);
    lua_register(LuaVM, 'collection_delete', collection_delete_fromLua);

    lua_register(LuaVM, 'listcolumns_add', listcolumns_add_fromLua);
    lua_register(LuaVM, 'listcolumns_getColumn', listcolumns_getColumn_fromLua);

    lua_register(LuaVM, 'listitem_delete', listitem_delete_fromLua);
    lua_register(LuaVM, 'listitem_getCaption', listitem_getCaption_fromLua);
    lua_register(LuaVM, 'listitem_setCaption', listitem_setCaption_fromLua);
    lua_register(LuaVM, 'listitem_getSubItems', listitem_getSubItems_fromLua);

    lua_register(LuaVM, 'listitems_clear', listitems_clear_fromLua);
    lua_register(LuaVM, 'listitems_getCount', listitems_getCount_fromLua);
    lua_register(LuaVM, 'listitems_add', listitems_add_fromLua);

    lua_register(LuaVM, 'listview_clear', listview_clear_fromLua);
    lua_register(LuaVM, 'listview_getColumns', listview_getColumns_fromLua);
    lua_register(LuaVM, 'listview_getItems', listview_getItems_fromLua);
    lua_register(LuaVM, 'listview_getItemIndex', listview_getItemIndex_fromLua);
    lua_register(LuaVM, 'listview_setItemIndex', listview_setItemIndex_fromLua);


    lua_register(LuaVM, 'timer_setInterval', timer_setInterval_fromLua);
    lua_register(LuaVM, 'timer_onTimer', timer_onTimer_fromLua);

    lua_register(LuaVM, 'openDialog_execute', openDialog_execute_fromLua);




    Lua_register(LuaVM, 'getMainForm', getMainForm_fromLua);
    Lua_register(LuaVM, 'getAddressList', getAddressList_fromLua);
    Lua_register(LuaVM, 'getFreezeTimer', getFreezeTimer_fromLua);
    Lua_register(LuaVM, 'getUpdateTimer', getUpdateTimer_fromLua);


    lua_register(LuaVM, 'inheritsFromObject', inheritsFromObject_fromLua);
    lua_register(LuaVM, 'inheritsFromComponent', inheritsFromComponent_fromLua);
    lua_register(LuaVM, 'inheritsFromControl', inheritsFromControl_fromLua);
    lua_register(LuaVM, 'inheritsFromWinControl', inheritsFromWinControl_fromLua);

    Lua_register(LuaVM, 'beep', beep_fromLua);

    LUA_DoScript('os=nil');


    //ce 6.0 compatibility. 6.0 has these methods in the stringlist instead of the strings class
    LUA_DoScript('stringlist_getCount=strings_getCount');
    LUA_DoScript('stringlist_getString=strings_getString');
    LUA_DoScript('stringlist_add=strings_add');
    LUA_DoScript('stringlist_remove=strings_remove');
  end;



finalization
  if LuaCS<>nil then
    LuaCS.free;

  if LuaVM<>nil then
    lua_close(LuaVM);

end.

