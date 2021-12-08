    
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
DOTNETCMD_WRAPOBJECT=8
DOTNETCMD_UNWRAPOBJECT=9
DOTNETCMD_INVOKEMETHOD=10


DOTNETCMD_EXIT=255


dotnetmodulelist={}

function dotnet_findDotNetMethodAddress(namespace, classname, methodname, modulename)
  --print(string.format("dotnet_findDotNetMethodAddress('%s','%s','%s','%s')",namespace,classname, methodname, modulename))

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
  if dotnetpipe==nil then return nil end
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

  local count=dotnetpipe.readByte()
  local TypeCodes={}
  for i=1,count do
    TypeCodes[i]=dotnetpipe.readByte()
  end
    
  dotnetpipe.unlock()
  
  return returntype, parameters, TypeCodes
end

function dotnet_wrapobject(objectaddress)
  if dotnetpipe then
    local result
    dotnetpipe.lock() 
    dotnetpipe.writeByte(DOTNETCMD_WRAPOBJECT)
    dotnetpipe.writeQword(objectaddress)
    result=dotnetpipe.readQword()  
    dotnetpipe.unlock()
    
    return result
  end
end

function dotnet_unwrapobject(wrappedobjecthandle)
  local result
  dotnetpipe.lock() 
  dotnetpipe.writeByte(DOTNETCMD_UNWRAPOBJECT)
  dotnetpipe.writeQword(wrappedobjecthandle)
  dotnetpipe.unlock()
  
  return result
end

TypeCode={}
TypeCode.Empty=0
TypeCode.Object=1
TypeCode.DBNull=2
TypeCode.Boolean=3
TypeCode.Char=4
TypeCode.SByte=5
TypeCode.Byte=6
TypeCode.Int16=7
TypeCode.UInt16=8
TypeCode.Int32=9
TypeCode.UInt32=10
TypeCode.Int64=11
TypeCode.UInt64=12
TypeCode.Single=13
TypeCode.Double=14
TypeCode.Decimal=15
TypeCode.DateTime=16
TypeCode.String=18 --handle as utf8string

dotnet_writeObjectValue={}
dotnet_writeObjectValue[TypeCode.Empty]=function() end
dotnet_writeObjectValue[TypeCode.Object]=function(wrappedObjectHandle) dotnetpipe.writeQword(wrappedObjectHandle) end
dotnet_writeObjectValue[TypeCode.DBNull]=dotnet_writeObjectValue[TypeCode.Empty]
dotnet_writeObjectValue[TypeCode.Boolean]=function(flag) dotnetpipe.writeByte(flag and 1 or 0) end
dotnet_writeObjectValue[TypeCode.Char]=function(c)  
  c=string.sub(c,1,1)
  dotnetpipe.writeWideString(c,false)  
end

dotnet_writeObjectValue[TypeCode.SByte]=function(v) dotnetpipe.writeByte(v) end
dotnet_writeObjectValue[TypeCode.Byte]=dotnet_writeObjectValue[TypeCode.SByte]
dotnet_writeObjectValue[TypeCode.Int16]=function(v) dotnetpipe.writeWord(v) end
dotnet_writeObjectValue[TypeCode.UInt16]=dotnet_writeObjectValue[TypeCode.Int16]
dotnet_writeObjectValue[TypeCode.Int32]=function(v) dotnetpipe.writeDword(v) end
dotnet_writeObjectValue[TypeCode.UInt32]=dotnet_writeObjectValue[TypeCode.Int32]
dotnet_writeObjectValue[TypeCode.Int64]=function(v) dotnetpipe.writeQword(v) end
dotnet_writeObjectValue[TypeCode.UInt64]=dotnet_writeObjectValue[TypeCode.Int64]
dotnet_writeObjectValue[TypeCode.Single]=function(v) dotnetpipe.writeFloat(v) end
dotnet_writeObjectValue[TypeCode.Double]=function(v) dotnetpipe.writeDouble(v) end
dotnet_writeObjectValue[TypeCode.Decimal]=dotnet_writeObjectValue[TypeCode.Double] --not a perfect match, but will do
dotnet_writeObjectValue[TypeCode.DateTime]=dotnet_writeObjectValue[TypeCode.UInt64]
dotnet_writeObjectValue[TypeCode.String]=function(s)   
  dotnetpipe.writeDword(#s)
  dotnetpipe.writeString(s)
end



dotnet_StringToTypeCodeValue={}
dotnet_StringToTypeCodeValue[TypeCode.Empty]=function(s) return 0 end
dotnet_StringToTypeCodeValue[TypeCode.Object]=function(s) 
  if s=='' then return 0 end --void gets handled as an object with address 0
  if s:sub(1,2)=='0x' then 
    return tonumber(s) 
  else 
    return tonumber(s,16) 
  end
end
dotnet_StringToTypeCodeValue[TypeCode.DBNull]=dotnet_StringToTypeCodeValue[TypeCode.Object]
dotnet_StringToTypeCodeValue[TypeCode.Boolean]=function(s) 
  local s=s:upper()
  if s=='1' or s=='TRUE' then return true end
  if s=='0' or s=='FALSE' then return false end
  return nil  
end
dotnet_StringToTypeCodeValue[TypeCode.Char]=function(s) return s:sub(1,1) end
dotnet_StringToTypeCodeValue[TypeCode.SByte]=function(s) return tonumber(s) end
dotnet_StringToTypeCodeValue[TypeCode.Byte]=dotnet_StringToTypeCodeValue[TypeCode.SByte]
dotnet_StringToTypeCodeValue[TypeCode.Int16]=dotnet_StringToTypeCodeValue[TypeCode.SByte]
dotnet_StringToTypeCodeValue[TypeCode.UInt16]=dotnet_StringToTypeCodeValue[TypeCode.SByte]
dotnet_StringToTypeCodeValue[TypeCode.Int32]=dotnet_StringToTypeCodeValue[TypeCode.SByte]
dotnet_StringToTypeCodeValue[TypeCode.UInt32]=dotnet_StringToTypeCodeValue[TypeCode.SByte]
dotnet_StringToTypeCodeValue[TypeCode.Int64]=dotnet_StringToTypeCodeValue[TypeCode.SByte]
dotnet_StringToTypeCodeValue[TypeCode.UInt64]=dotnet_StringToTypeCodeValue[TypeCode.SByte]
dotnet_StringToTypeCodeValue[TypeCode.Single]=dotnet_StringToTypeCodeValue[TypeCode.SByte]
dotnet_StringToTypeCodeValue[TypeCode.Double]=dotnet_StringToTypeCodeValue[TypeCode.SByte]
dotnet_StringToTypeCodeValue[TypeCode.Decimal]=dotnet_StringToTypeCodeValue[TypeCode.SByte]
dotnet_StringToTypeCodeValue[TypeCode.DateTime]=dotnet_StringToTypeCodeValue[TypeCode.SByte]
dotnet_StringToTypeCodeValue[TypeCode.String]=function(s) return s end


dotnet_readObjectValue={}
dotnet_readObjectValue[TypeCode.Empty]=function() return nil end
dotnet_readObjectValue[TypeCode.Object]=function() return dotnetpipe.readQword() end
dotnet_readObjectValue[TypeCode.DBNull]=dotnet_readObjectValue[TypeCode.Empty]
dotnet_readObjectValue[TypeCode.Boolean]=function() return dotnetpipe.readByte()~=0 end
dotnet_readObjectValue[TypeCode.Char]=function() return dotnetpipe.readWideString(1) end
dotnet_readObjectValue[TypeCode.SByte]=function() return signExtend(dotnetpipe.readByte(),7) end
dotnet_readObjectValue[TypeCode.Byte]=function() return dotnetpipe.readByte() end
dotnet_readObjectValue[TypeCode.Int16]=function() return signExtend(dotnetpipe.readWord(),15) end
dotnet_readObjectValue[TypeCode.UInt16]=function() return dotnetpipe.readWord() end
dotnet_readObjectValue[TypeCode.Int32]=function() return signExtend(dotnetpipe.readDWord(),31) end
dotnet_readObjectValue[TypeCode.UInt32]=function() return dotnetpipe.readDWord() end
dotnet_readObjectValue[TypeCode.Int64]=function() return dotnetpipe.readQword(v) end
dotnet_readObjectValue[TypeCode.UInt64]=dotnet_readObjectValue[TypeCode.UInt64]
dotnet_readObjectValue[TypeCode.Single]=function() return dotnetpipe.readFloat(v) end
dotnet_readObjectValue[TypeCode.Double]=function() return dotnetpipe.readDouble(v) end
dotnet_readObjectValue[TypeCode.Decimal]=dotnet_readObjectValue[TypeCode.Double] --not a perfect match, but will do
dotnet_readObjectValue[TypeCode.DateTime]=dotnet_readObjectValue[TypeCode.UInt64]
dotnet_readObjectValue[TypeCode.String]=function() 
  local length=dotnetpipe.readDword()
  return dotnetpipe.readString(length) 
end 

dotnet_readObjectValue[255]=function()
  local msg=dotnet_readObjectValue[TypeCode.String]
  print("Invoke Error: "..msg)
  return nil
end


function dotnet_writeObject(parameter)
  --pre: dotnetpipe lock is obtained
  printf("dotnet_writeObject parameter.Type=%d parameter.Value="..parameter.Value, parameter.Type)
  
  dotnetpipe.writeByte(parameter.Type)
  local handler=dotnet_writeObjectValue[parameter.Type]
  if handler then
    handler(parameter.Value)
  else   
    print("Unknown write handler:"..parameter.Type)
    print(debug.traceback())
    return false
  end
  
  return true  
end

function dotnet_readObject()
  printf("dotnet_readObject")
  local result
  local Type=dotnetpipe.readByte()
  local handler=dotnet_readObjectValue[Type]
  if handler then    
    return handler()
  else
    if (Type~=nil) then
      print("Unknown read handler:"..Type)
    else
      print("Unknown read handler:"..Type)          
    end
      
    print(debug.traceback())
    return nil  
  end  
end

function dotnet_invoke_method(moduleid, methoddef, wrappedObjectHandle, parameters)
  local result
  dotnetpipe.lock() 
  dotnetpipe.writeByte(DOTNETCMD_INVOKEMETHOD)
  dotnetpipe.writeDword(moduleid)
  dotnetpipe.writeDword(methoddef)
  dotnetpipe.writeQword(wrappedObjectHandle)  
  
  --print("writing parameters")
  if parameters then
    dotnetpipe.writeByte(#parameters)
    local i
    for i=1,#parameters do  
      dotnet_writeObject(parameters[i])      
    end
  else
    dotnetpipe.writeByte(0)
  end
  
 -- print("Reading result")

  result=dotnet_readObject()  
  dotnetpipe.unlock()  
  return result
end

function dotnet_invoke_method_dialog(name, moduleid, methoddef, wrappedAddress)
  if wrappedAddress==nil then
    return nil,'no wrapped object address provided'
  end
  if not inMainThread() then
    return nil,'dotnet_invoke_method_dialog must run in the main thread' 
  end
  

  local returntype, parameters, typeCodes=dotnet_getMethodParameters(moduleid, methoddef)
  if returntype==nil then
    return nil,'dotnet_getMethodParameters failed'
  end
  
  parameters=parameters:sub(2,#parameters-1) --strip the braces
  
  local paramlist=table.pack(parameters:split(','))
  
  if #paramlist~=#typeCodes-1 then --bug?
    paramlist={} 
    local i
    for i=1,#typeCodes do
      paramlist[i]='tc'..typecodes[i]..' param'..i;
    end
  end
  
  --still here, so all checks are ok, spawn the dialog
  local mifinfo
  mifinfo=createMethodInvokedialog(name, paramlist, function()
    local instance=wrappedAddress  
  end)
  
  
  --print("createMethodInvokedialog returned")
  
  
  mifinfo.btnOk.OnClick=function(b)
    --use typeCodes to fill the paramlist  
    print("DotnetInterface: mifinfo.btnOk.OnClick ")
    
    local parameters={}
    for i=2,#typeCodes do
      local e={}
      e.Type=typeCodes[i]
      
      local valuehandler=dotnet_StringToTypeCodeValue[e.Type]
      if valuehandler then
        e.Value=valuehandler(mifinfo.parameters[i-1].edtVarText.Text)        
        if e.Value==nil then --nil is error
          messageDialog('The input :"'..mifinfo.parameters[i].edtVarText.Text..'" could not be parsed for '..mifinfo.parameters[i].lblVarName, mtError,mbOK);        
          return
        end
      else  
        messageDialog('TypeCodes '..valuehandler..' is not known', mtError,mbOK);
        return
      end
      
      
      table.insert(parameters,e)
    end
    
    local result=dotnet_invoke_method(moduleid, methoddef, wrappedAddress, parameters)
    
    

  end
  
 -- print("showing mifinfo.mif")
  
  mifinfo.mif.show()
  
  return mifinfo
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
    print(debug.traceback())    
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