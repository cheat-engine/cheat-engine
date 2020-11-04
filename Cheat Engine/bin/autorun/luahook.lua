--Routine that scans and hooks lua functions

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
      luastubs.lua_pushinteger=createExecuteCodeExStub(1,'lua_pushinteger',0,0)
      luastubs.lua_tointegerx=createExecuteCodeExStub(1,'lua_tointegerx',0,0,{type=5, size=4})      
      if getAddressSafe('lua_tointeger') then
        luastubs.lua_tointegerx=createExecuteCodeExStub(1,'lua_tointeger',0,0)    
      end
      luastubs.lua_settop=createExecuteCodeExStub(1,'lua_settop',0,0)      
      
      luastubs.lual_loadstring=createExecuteCodeExStub(1,'lual_loadstring',0,3)   
      luastubs.lua_pcallk=createExecuteCodeExStub(1,'lua_pcallk',0,0,0,0,0,0) 
     
      if getAddressSafe('lua_pcall') then
        luastubs.lua_pcall=createExecuteCodeExStub(1,'lua_pcall',0,0,0,0)    
      end

      luastubs.lua_pcallk=createExecuteCodeExStub(1,'lua_pcallk',0,0,0,0,0,0) 
      
      luastubs.lua_pushcfunction=createExecuteCodeExStub(1,'lua_pushcfunction',0,0) 
      luastubs.lua_setglobal=createExecuteCodeExStub(1,'lua_setglobal',0,3)       
      luastubs.lua_getglobal=createExecuteCodeExStub(1,'lua_getglobal',0,3)   
      
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