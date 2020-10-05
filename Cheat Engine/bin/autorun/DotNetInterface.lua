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

dotnet_timeout=300000

DOTNETCMD_TEST=0
DOTNETCMD_INITMODULELIST=1
DOTNETCMD_GETMETHODENTRYPOINT=2
DOTNETCMD_EXIT=255


dotnetmodulelist={}

function dotnet_initModuleList()
  --load the modulelist from the injected dll.  Watch the order
  dotnetmodulelist={}
  dotnetpipe.lock()
  dotnetpipe.writeByte(DOTNETCMD_INITMODULELIST)   
  local count=dotnetpipe.readDword()
  local i
  for i=1,count do
    local stringsize=dotnetpipe.readDword()
    local s=dotnetpipe.readString(stringsize)
    print(s)
    
    dotnetmodulelist[i]={}
    dotnetmodulelist[i].Name=s
    dotnetmodulelist[i].Index=i
    dotnetmodulelist[s]=dotnetmodulelist[i]
  end
  dotnetpipe.unlock()
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

function LaunchDotNetInterface()
  local dllname
  local remotePipeHandle
  
  if dotnetpipe and (tonumber(dotnetpipe.processid)==getOpenedProcessID()) then
    return true
  end
  
  if dotnetpipe then
    dotnetpipe.destroy()
    dotnetpipe=nil
  end
 
  dllname="DotNetInterface.dll"     --it's an "Any CPU" library    
 
  
  --create a pipe and duplicate it's handle to the target process
  local serverpipe=createPipe('cedotnetpipe_pid'..getOpenedProcessID(), 256*1024,1024)    
  remotePipeHandle=duplicateHandle(serverpipe.Handle)
  
  print(string.format("new handle=%d", remotePipeHandle)) 
  
  local injectResult, injectError=injectDotNetLibrary(getAutorunPath()..libfolder..pathsep..dllname, 'DotNetInterface.PipeServer','Init',string.format("%d",remotePipeHandle), dotnet_timeout) 
 -- local injectResult, injectError=injectDotNetLibrary([[D:\git\cheat-engine\Cheat Engine\DotNetInvasiveDataCollector\DotNetInvasiveDataCollector\bin\Debug\netstandard2.0\DotNetInterface.dll]], 'DotNetInterface.PipeServer','Init',string.format("%d",remotePipeHandle)) 


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
  
  
  print("connected")
  
  
  dotnetpipe.OnError=function(self)
    print("dotnetpipe error")
  end 

  dotnetpipe.OnTimeout=function(self)  
    dotnetpipe=nil
    print("dotnet timeout")
  end 
  

  local val=math.random(1,255)
  printf("val=%x",val)
  
  dotnetpipe.lock()
  dotnetpipe.writeByte(DOTNETCMD_TEST)
  dotnetpipe.writeByte(val)  
  local result=dotnetpipe.readByte() 
  if result then
    printf("result=%x", result)
  end
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
  
  print("calling dotnet_initModuleList")
  
  dotnet_initModuleList()
  
  dotnetpipe.processid=getOpenedProcessID()
  --still here, return true   
  return true
end