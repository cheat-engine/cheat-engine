--same as monodatacollector but for .net and .netcore
--can theoretically be used on mono as well

if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'dotnetforceddatacollector.po')
end


local pathsep
local libfolder

if getOperatingSystem()==0 then
  pathsep=[[\]]
  libfolder='dlls'
else
  pathsep='/'
  libfolder='dylibs'
end

dotnet_timeout=3000

DOTNETCMD_TEST=0
DOTNETCMD_INITMODULELIST=1
DOTNETCMD_GETMETHODENTRYPOINT=2
DOTNETCMD_GETFIELDTYPENAME=3
DOTNETCMD_GETFIELDVALUE=4
DOTNETCMD_SETFIELDVALUE=5
DOTNETCMD_LOADMODULE=6
DOTNETCMD_GETMETHODPARAMETERS=7


DOTNETCMD_EXIT=255


dotnetmodulelist={}

function dotnet_findDotNetMethodAddress(namespace, classname, methodname, modulename)
  print(string.format("dotnet_findDotNetMethodAddress('%s','%s','%s','%s')",namespace,classname, methodname, modulename))

  local fcn

  if namespace==nil then namespace='' end
  if modulename then modulename=modulename:lower() end

  if namespace~='' then
    fcn=namespace..'.'..classname
  else
    fcn=classname
  end

  local dc=getDotNetDataCollector()

  local moduleid


  local domains=dc.enumDomains()
  local i
  for i=1,#domains do
    local modules=dc.enumModuleList(domains[i].DomainHandle)
    local j
    for j=1,#modules do
      if (modulename==nil) or (modulename==extractFileName(modules[j].Name):lower()) then
        local classes=dc.enumTypeDefs(modules[j].ModuleHandle)
        local k
        t={}

        for k=1,#classes do
          --printf("%d: %s (%d) <=>%s (%d)",k, classes[k].Name, #classes[k].Name, fcn, #fcn)
          t[k]=classes[k].Name

          if classes[k].Name==fcn then
            --found the class
            --print("yes")
            local ml=dc.getTypeDefMethods(modules[j].ModuleHandle, classes[k].TypeDefToken)
            local l
            for l=1,#ml do
              if ml[l].Name==methodname then
                print("Found method. Calling dotnet_getMethodEntryPoint")
                local r=dotnet_getMethodEntryPoint(dotnet_getModuleID(extractFileName(modules[j].Name)), ml[l].MethodToken)
                
                if r then
                  printf("%s at address %8x", methodname, r)
                  return r
                else
                  print("failure")
                end
              end
            end
          end
        end
      end
    end
  end
end

function dotnet_loadModule(path)
  if path==nil then
    print(debug.traceback())
    error('dotnet_loadModule: path is nil')
  end
  local r
  dotnetpipe.lock()
  dotnetpipe.writeByte(DOTNETCMD_LOADMODULE) 
  dotnetpipe.writeDword(#path);
  dotnetpipe.writeString(path)
  r=dotnetpipe.readDword()
  if dotnetpipe==nil then return false end
  
  dotnetpipe.unlock()
  
  if r==1 then  
    dotnet_initModuleList()
    reinitializeDotNetSymbolhandler(extractFileName(path))
    return true
  else
    return false
  end
end

function dotnet_initModuleList()
  --load the modulelist from the injected dll.  Watch the order
  dotnetmodulelist={}
  dotnetpipe.lock()
  dotnetpipe.writeByte(DOTNETCMD_INITMODULELIST)   
  local count=dotnetpipe.readDword()
  
  if not (count or dotnetpipe) then return end
    
  local i
  for i=1,count do
    local stringsize=dotnetpipe.readDword()
    local s=dotnetpipe.readString(stringsize)
    
    
    dotnetmodulelist[i]={}
    dotnetmodulelist[i].Name=s
    dotnetmodulelist[i].Index=i
    dotnetmodulelist[s]=dotnetmodulelist[i]
  end
  dotnetpipe.unlock()
end

function dotnet_setFieldValue(moduleid, fielddef, instanceaddress, value)
  local result
  dotnetpipe.lock()
  dotnetpipe.writeByte(DOTNETCMD_SETFIELDVALUE)
  dotnetpipe.writeDword(moduleid)
  dotnetpipe.writeDword(fielddef)
  dotnetpipe.writeQword(instanceaddress)
  dotnetpipe.writeDword(#value)
  dotnetpipe.writeString(value)
  dotnetpipe.unlock()
  
  return result
end


function dotnet_getFieldValue(moduleid, fielddef, instanceaddress)
  local result
  dotnetpipe.lock()
  dotnetpipe.writeByte(DOTNETCMD_GETFIELDVALUE)
  dotnetpipe.writeDword(moduleid)
  dotnetpipe.writeDword(fielddef)
  dotnetpipe.writeQword(instanceaddress)
  local stringsize=dotnetpipe.readDword()  
  local result=dotnetpipe.readString(stringsize)
  dotnetpipe.unlock()
  
  return result
end


function dotnet_getFieldTypeName(moduleid, fielddef)
  local result
  dotnetpipe.lock()
  dotnetpipe.writeByte(DOTNETCMD_GETFIELDTYPENAME)
  dotnetpipe.writeDword(moduleid)
  dotnetpipe.writeDword(fielddef)
  local stringsize=dotnetpipe.readDword()  
  local result=dotnetpipe.readString(stringsize)
  dotnetpipe.unlock()
  
  return result
end

function dotnet_getMethodEntryPoint(moduleid, methoddef)
  dotnetpipe.lock()
  dotnetpipe.writeByte(DOTNETCMD_GETMETHODENTRYPOINT)
  dotnetpipe.writeDword(moduleid)
  dotnetpipe.writeDword(methoddef)
  local result=dotnetpipe.readQword()
  dotnetpipe.unlock()
  
  return result
end

function dotnet_getMethodParameters(moduleid, methoddef)
  if moduleid==nil then error('moduleid is nil') end
  if methoddef==nil then error('methoddef is nil') end
  
  local returntype,parameters,sz
  dotnetpipe.lock()
  dotnetpipe.writeByte(DOTNETCMD_GETMETHODPARAMETERS)
  dotnetpipe.writeDword(moduleid)
  dotnetpipe.writeDword(methoddef)
  sz=dotnetpipe.readDword()  
  returntype=dotnetpipe.readString(sz)
  sz=dotnetpipe.readDword()  
  parameters=dotnetpipe.readString(sz)  
  dotnetpipe.unlock()
  
  return returntype, parameters  
end

function dotnet_getModuleID(modulename)
  if dotnetmodulelist==nil then
    dotnet_initModuleList()
    
    if dotnetmodulelist==nil then return end
  end
  
  local m=dotnetmodulelist[modulename] 
  if m then
    return m.Index-1
  end
end

function dotnet_disconnect()
print("dotnet_disconnect")
  if dotnetpipe then
    dotnetpipe.lock()
    dotnetpipe.writeByte(DOTNETCMD_EXIT) 
    dotnetpipe.unlock()
    
    dotnetpipe.destroy()
  end
  dotnetpipe=nil
end

function LaunchDotNetInterface()
  local dllname
  local remotePipeHandle
  
  if dotnetpipe and (tonumber(dotnetpipe.processid)==getOpenedProcessID()) then
    return true
  end
  
  if dotnetpipe then
    dotnet_disconnect()
    if dotnetpipe~=nil then
      dotnetpipe.destroy()
      dotnetpipe=nil
    end
  end
 
  dllname="DotNetInterface.dll"     --it's an "Any CPU" library    
 
  
  --create a pipe and duplicate it's handle to the target process
  local serverpipe=createPipe('cedotnetpipe_pid'..getOpenedProcessID(), 256*1024,1024)    
  remotePipeHandle=duplicateHandle(serverpipe.Handle)
  
  serverpipe.destroy()
  
  --print(string.format("new handle=%d", remotePipeHandle)) 
  
  local injectResult, injectError=injectDotNetLibrary(getAutorunPath()..libfolder..pathsep..dllname, 'DotNetInterface.PipeServer','Init',string.format("%d",remotePipeHandle), dotnet_timeout) 

  if (injectResult==nil) or (injectResult==false )then
    if injectError then
      messageDialog(translate('Inject error : ')..string.format("%x",injectError), mtError, mbOK);
    else
      messageDialog(translate('Inject error : ')..translate('Unknown'), mtError, mbOK);
    end
    return  
  end
  
  dotnetpipe=connectToPipe('cedotnetpipe_pid'..getOpenedProcessID() ,dotnet_timeout)    
  
  if (dotnetpipe==nil) then
    print("dotnetpipe is nil. Failed to connect")
    return --failure
  end  
  
  
  --print("connected")
  
  
  dotnetpipe.OnError=function(self)
    print("dotnetpipe error")
    dotnetpipe=nil
  end 

  dotnetpipe.OnTimeout=function(self)  
    dotnetpipe=nil
    print("dotnet timeout")
  end 
  

  local val=math.random(1,255)
  --printf("val=%x",val)
  
  dotnetpipe.lock()
  dotnetpipe.writeByte(DOTNETCMD_TEST)
  dotnetpipe.writeByte(val)  
  local result=dotnetpipe.readByte() 
  --if result then
    --printf("result=%x", result)
  --end
  if dotnetpipe==nil then return false end
  dotnetpipe.unlock()
  
  local resultcalc=val ~ 0xce
  
  if result~=resultcalc then
    
    messageDialog('pipe connection error (invalid value returned)', mtError, mbOK);
    if dotnetpipe~=nil then

      dotnetpipe.destroy()
      dotnetpipe=nil
    end
    return      
  end
  
 -- print("calling dotnet_initModuleList")
  
  dotnet_initModuleList()
  
  dotnetpipe.processid=getOpenedProcessID()
  dotnetpipe.isValid=function() return getOpenedProcessID()==tonumber(dotnetpipe.processid) end
  --still here, return true   
  return true
end