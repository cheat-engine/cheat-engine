--Routine that scans and hooks lua functions

--return debug.getinfo(function).source



--luaTypes-- 
LUA_TNONE          = -1
LUA_TNIL           = 0
LUA_TBOOLEAN       = 1
LUA_TLIGHTUSERDATA = 2
LUA_TNUMBER        = 3
LUA_TSTRING        = 4
LUA_TTABLE         = 5
LUA_TFUNCTION      = 6
LUA_TUSERDATA      = 7
LUA_TTHREAD        = 8
LUA_NUMTAGS        = 9
  

function hookLua()
  --get the most commonly used lua functions
  
  --create an executor
  LuaExecutor=createRemoteExecutor()
  
  
  GetCurrentThreadIDStub=createExecuteCodeExStub(0,'GetCurrentThreadID') --todo, add an Executor.ThreadID...
  LuaExecutorThreadID=LuaExecutor.executeStub(GetCurrentThreadIDStub)
  
  
  LuaMutexHandle=executeCodeLocalEx('CreateMutexA',0,0,0)
  LuaRemoteMutexHandle=duplicateHandle(LuaMutexHandle)
  
  LuaNotifyCEEvent=executeCodeLocalEx('CreateEventA',0,0,0,0)
  LuaCEDoneEvent=executeCodeLocalEx('CreateEventA',0,1,0,0)
  
  LuaRemoteNotifyCEEvent=duplicateHandle(LuaNotifyCEEvent)
  LuaRemoteCEDoneEvent=duplicateHandle(LuaCEDoneEvent)

  --create a lua hook callroutine (todo: 32-bit)
success, aainfo=autoAssemble(string.format([[
alloc(new_lua_gettop,256)
alloc(original_lua_gettop,8)
alloc(last_lua_state,8)
alloc(mutex,8)
alloc(ceevent,8) //notify CE that lua has paused for it to start executing
alloc(cedone,8) //wait for this. CE sets it when done
alloc(executorthreadid,4) //the executor threadid just goes, it doesn't wait

alloc(hascommands,1)
label(afterwait)

new_lua_gettop:

push rcx
sub rsp,20



call GetCurrentThreadID
cmp eax,[executorthreadid]
je afterwait


//obtain the mutex
mov rcx,[mutex] //only for the process, not for CE (even though CE created it...)
mov rdx,ffffffff
call waitForSingleObject

//mutex obtained

cmp byte [hascommands],0
je nocommands

mov rcx,[rsp+20] //remember the push rcx ?
mov [last_lua_state],rcx
//signal CE that lua is frozen

mov rcx,[ceevent]
call setEvent

mov rcx,[cedone]
mov rdx,ffffffff
call waitForSingleObject

//ce is done with the lua state, continue the normal program


nocommands:
mov rcx,[mutex]
call ReleaseMutex

afterwait:
add rsp,20
pop rcx
jmp [original_lua_gettop]

original_lua_gettop:
dq 0

mutex:
dq %x

ceevent:
dq %x

cedone:
dq %x

executorthreadid:
dd %x

]],LuaRemoteMutexHandle, LuaRemoteNotifyCEEvent, LuaRemoteCEDoneEvent, LuaExecutorThreadID))



  s=generateAPIHookScript('lua_gettop', string.format("%x",aainfo.allocs.new_lua_gettop.address), string.format("%x",aainfo.allocs.original_lua_gettop.address)  )
  return autoAssemble(s)  
end

function lua_gettop()
  if luastubs and lua_state then
    return LuaExecutor.executeStub(luastubs.lua_gettop,{lua_state})
  end
end

function lua_settop(index)
  if luastubs and lua_state then
    return LuaExecutor.executeStub(luastubs.lua_settop,{lua_state, index})
  end
end

function lua_pop(n)
  return lua_settop(-n-1)
end

function lua_pushinteger(value)
  if luastubs and lua_state then
    return LuaExecutor.executeStub(luastubs.lua_pushinteger,{lua_state, value})
  end  
end

function lua_tointegerx(index)
  if luastubs and lua_state and luastubs.lua_tointegerx then
    local isvalid={0,0,0,0}
    local r,r2=LuaExecutor.executeStub(luastubs.lua_tointegerx,{lua_state, index,isvalid})
    
    if r2.Data[1]~=0 then
      return r
    else
      return nil
    end
  end  
end

function lua_tointeger(index)
  if luastubs and lua_state then
    if luastubs.lua_tointeger then
      return LuaExecutor.executeStub(luastubs.lua_tointeger,{lua_state, index})    
    else      
      return lua_tointegerx(index)
    end
  end  
end

function lua_touserdata(index)
  if luastubs and lua_state and luastubs.lua_touserdata then    
    return LuaExecutor.executeStub(luastubs.lua_touserdata,{lua_state, index})    
  end    
end

function lua_tostring(index)
  if luastubs and lua_state and luastubs.lua_tolstring then
    local stringLength={0,0,0,0,0,0,0,0}
    local stringAddress,l=LuaExecutor.executeStub(luastubs.lua_tolstring,{lua_state, index,stringLength})   
    stringLength=byteTableToQword(l.Data)
    
    local result=readString(stringAddress, stringLength)    
    return result
  end
end

function lua_gettable(index)
  if luastubs and lua_state and luastubs.lua_gettable then
    return LuaExecutor.executeStub(luastubs.lua_gettable,{lua_state, index})    
  end
end

function lua_settable(index)
  if luastubs and lua_state and luastubs.lua_settable then
    return LuaExecutor.executeStub(luastubs.lua_settable,{lua_state, index})    
  end
end


function lua_type(index)
  if luastubs and lua_state and luastubs.lua_type then
    return LuaExecutor.executeStub(luastubs.lua_type,{lua_state, index})    
  end
end

function lua_istable(index)
  return lua_type(index)==LUA_TTABLE; 
end

function lua_isnil(index)
   return lua_type(index)==LUA_TNIL
end

function lua_isstring(index)
  if luastubs and lua_state and luastubs.lua_isstring then
    return LuaExecutor.executeStub(luastubs.lua_isstring,{lua_state, index})    
  end
end

function lua_isinteger(index)
  if luastubs and lua_state and luastubs.lua_isinteger then
    return LuaExecutor.executeStub(luastubs.lua_isinteger,{lua_state, index})    
  end
end

function lua_isnumber(index)
  if luastubs and lua_state and luastubs.lua_isnumber then
    return LuaExecutor.executeStub(luastubs.lua_isnumber,{lua_state, index})    
  end
end

function lua_iscfunction(index)
  if luastubs and lua_state and luastubs.lua_iscfunction then
    return LuaExecutor.executeStub(luastubs.lua_iscfunction,{lua_state, index})    
  end
end

function lua_isuserdata(index)
  if luastubs and lua_state and luastubs.lua_isuserdata then
    return LuaExecutor.executeStub(luastubs.lua_isuserdata,{lua_state, index})    
  end
end

function lua_rawlen(index)
  if luastubs and lua_state and luastubs.lua_rawlen then
    return LuaExecutor.executeStub(luastubs.lua_rawlen,{lua_state, index})    
  end
end

function lua_objlen(index)
  return lua_rawlen(index);
end;   



function lual_loadstring(script)
  if luastubs and lua_state and luastubs.lual_loadstring then
    return LuaExecutor.executeStub(luastubs.lual_loadstring,{lua_state, script})
  end
end

function lua_pcallUnlocked(lua_state, argcount, resultcount, errfunc)  
  if luastubs and lua_state then
    if luastubs.lua_pcall then
      return LuaExecutor.executeStub(luastubs.lua_pcall,{lua_state, argcount, resultcount, errfunc})
    elseif luastubs.lua_pcallk then
      return LuaExecutor.executeStub(luastubs.lua_pcallk,{lua_state, argcount, resultcount, errfunc,0,0})
    end
  end
end

function lua_pcall(argcount, resultcount, errfunc)
  --warning: Do not call functions that need to synchronize with the main thread
  --use lua_pcallUnlocked if you're sure the lua build you're working with is thread safe 
  return lua_pcallUnlocked(lua_state, argcount, resultcount, errfunc)
end


function lua_dostring(script)
  if lual_loadstring(script)==0 then
    return lua_pcall(0,0,0)  
  end  
end


function lua_pushcclosure(functionaddress, n)
  if luastubs and lua_state and luastubs.lua_pushcclosure then
    return LuaExecutor.executeStub(luastubs.lua_pushcclosure,{lua_state, functionaddress,n})
  end
end

function lua_pushcfunction(functionaddress)
  return lua_pushcclosure(functionaddress,0)
end

function lua_setglobal(name)
  if luastubs and lua_state and luastubs.lua_setglobal then
    return LuaExecutor.executeStub(luastubs.lua_setglobal,{lua_state, name})
  end
end

function lua_getglobal(name)
  if luastubs and lua_state and luastubs.lua_getglobal then
    return LuaExecutor.executeStub(luastubs.lua_getglobal,{lua_state, name})
  end
end

function luaopen_debug()
  if luastubs and lua_state and luastubs.luaopen_debug then
    return LuaExecutor.executeStub(luastubs.luaopen_debug,{lua_state})
  end  
end

function lua_registerdebug()
  local oldstack=lua_gettop()
  luaopen_debug()
  if oldstack~=lua_gettop() then  
    lua_setglobal('debug')
  end
end



function lua_register(name, functionaddress)
  lua_pushcfunction(functionaddress)
  lua_setglobal(name)
end


function lua_ceprint(stringAddress)
  --called when ceprint is executed by the target. stringAddress is
  if stringAddress~=0 then  
    print(process..'>'..readString(stringAddress))
  end
  return 0
end

function lua_registerceprint()
  --injects a print function into the target which on execution sends a string to CE's lua engine window.
  --and registers it as 'ceprint'
  r,r2=autoAssemble([[
alloc(ceprint,1024)
ceprint:
loadlibrary(luaclient-x86_64.dll)
luacall(openLuaServer('CELUASERVER'))

alloc(CELUA_ServerName,16)
alloc(ceprintStringName,16)
alloc(ceprintFunctionID,4)


ceprintStringName:
db 'lua_ceprint',0

ceprintFunctionID:
dd 0

CELUA_ServerName:
db 'CELUASERVER',0

ceprint:
push rcx
sub rsp,30

mov rcx,[rsp+30]
mov rdx,1
mov r8,0
call lua_tolstring

//rax should now be 0 or point to a string
mov [rsp+20],rax

mov ecx,[ceprintFunctionID]
test ecx,ecx
jne short hasrefid

mov rcx,ceprintStringName
call CELUA_GetFunctionReferenceFromName  //Basically calls createRef(functionname) and returns the value
mov [ceprintFunctionID],eax
mov ecx,eax

hasrefid:
//ecx contains the ceprintFunctionID
mov edx,1
lea r8,[rsp+20]
mov r9,1
call CELUA_ExecuteFunctionByReference

add rsp,38
mov rax,0 //no return
ret
  ]])
  
  if r then
    lua_register('ceprint',r2.allocs.ceprint.address)  
    return true
  else
    return r,r2
  end   
end

function lua_startrecordcalls()
--[[
celog={}
celogging=true
debug.sethook(function(event)
  table.insert(celog,debug.getinfo(2))

  count=count-1
  if celogging==false then
    debug.sethook()
  end
end,'c')
--]]
  local oldstack=lua_gettop()
  lua_dostring([[
celog={}
celogging=true
debug.sethook(function(event)
  table.insert(celog,debug.getinfo(2))

  if celogging==false then
    debug.sethook()
  end
end,'c')  
  ]])
  
  if lua_gettop()~=oldstack then
    print("Error:"..lua_tostring(-1))
    lua_pop(1)
  end

end

function lua_stoprecordcalls()
  lua_dostring('debug.sethook()')
end

function lua_getcalllog()
end


function setLuaState(ls)
  lua_state=ls  
end

function getLuaState(ls)
  return lua_state
end

function LockLuaState(timeout)

  --waits for a lock on lua and then get the state
  if timeout==nil then
    timeout=0xffffffff
  end 
  
  executeCodeLocalEx('ResetEvent',LuaNotifyCEEvent)   
  executeCodeLocalEx('ResetEvent',LuaCEDoneEvent)
  writeBytes(aainfo.allocs.hascommands.address,1)
  local result=executeCodeLocalEx('WaitForSingleObject',LuaNotifyCEEvent,timeout)==0
  
  if result then
    lua_state=readQword(aainfo.allocs.last_lua_state.address)     
    
    
    if luastubs~=nil then
      if luastubs.processid~=getOpenedProcessID() then
        luastubs=nil
      end
    end
    
    if luastubs==nil then
      --create lua function call stubs
      luastubs={}
      luastubs.processid=getOpenedProcessID()
      luastubs.lua_gettop=createExecuteCodeExStub(1,'lua_gettop',0)
      luastubs.lua_settop=createExecuteCodeExStub(1,'lua_settop',0,0)      
            
      luastubs.lua_pushinteger=createExecuteCodeExStub(1,'lua_pushinteger',0,0)
      luastubs.lua_tointegerx=createExecuteCodeExStub(1,'lua_tointegerx',0,0,{type=5, size=4})      
      if getAddressSafe('lua_tointeger') then
        luastubs.lua_tointegerx=createExecuteCodeExStub(1,'lua_tointeger',0,0)    
      end
      
      luastubs.lua_touserdata=createExecuteCodeExStub(1,'lua_touserdata',0,0)    
      
      luastubs.lua_type=createExecuteCodeExStub(1,'lua_type',0,0);
 
      luastubs.lua_gettable=createExecuteCodeExStub(1,'lua_gettable',0,0)      
      luastubs.lua_settable=createExecuteCodeExStub(1,'lua_settable',0,0) 
      
      luastubs.lua_rawlen=createExecuteCodeExStub(1,'lua_rawlen',0,0) 
      
      
      
      luastubs.lua_isstring=createExecuteCodeExStub(1,'lua_isstring',0,0) 
      luastubs.lua_isinteger=createExecuteCodeExStub(1,'lua_isinteger',0,0) 
      luastubs.lua_isnumber=createExecuteCodeExStub(1,'lua_isnumber',0,0) 
      luastubs.lua_iscfunction=createExecuteCodeExStub(1,'lua_iscfunction',0,0) 
      luastubs.lua_isuserdata=createExecuteCodeExStub(1,'lua_isuserdata',0,0) 
      
      luastubs.lual_loadstring=createExecuteCodeExStub(1,'lual_loadstring',0,3)   
      luastubs.lua_pcallk=createExecuteCodeExStub(1,'lua_pcallk',0,0,0,0,0,0) 
     
      if getAddressSafe('lua_pcall') then
        luastubs.lua_pcall=createExecuteCodeExStub(1,'lua_pcall',0,0,0,0)    
      end

      luastubs.lua_pcallk=createExecuteCodeExStub(1,'lua_pcallk',0,0,0,0,0,0) 
      
      luastubs.lua_pushcclosure=createExecuteCodeExStub(1,'lua_pushcclosure',0,0,0) 
      luastubs.lua_setglobal=createExecuteCodeExStub(1,'lua_setglobal',0,3)       
      luastubs.lua_getglobal=createExecuteCodeExStub(1,'lua_getglobal',0,3)   
      luastubs.lua_tolstring=createExecuteCodeExStub(1,'lua_tolstring',0,0,{type=5, size=8})   
      luastubs.luaopen_debug=createExecuteCodeExStub(1,'luaopen_debug',0)  
    end
    
    return true
  else 
    ReleaseLuaState()
  end
end

function ReleaseLuaState()
  --resumes the game
  writeBytes(aainfo.allocs.hascommands.address,0)
  executeCodeLocalEx('SetEvent',LuaCEDoneEvent)
  lua_state=nil
end