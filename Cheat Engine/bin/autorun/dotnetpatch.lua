--patches a dotnet method.  Prerequisite: Must not be inlined or generic, or anything complex

function ParseScriptTokens(script,values)
  --parses the script for <> entries and looks up the value in the values table
  if script==nil then 
    print(debug.traceback())
    error('ParseScriptTokens: script is nil') 
  end
  if values==nil then 
    print(debug.traceback())
    error('ParseScriptTokens: values is nil') 
  end
  
  return string.gsub(script,"<(.-)>",function(v) 
    local r=values[v]
    if r then return r else return x end
  end)
end

function dotnetpatch_getAllReferences()
  --gets a list of all assemblies
  --todo: if they are in-memory only, export them to a file first (create the mz/pe manually, just the metadata)
  local r={}
  local sysfile

  if monopipe then
    mono_enumImages(function(img)
      local n=mono_image_get_filename(img)
      local ln=extractFileName(n:lower())
      --not removing default core assembly from referrences  
	  table.insert(r,n)
	  if ln=='mscorlib.dll' or ln=='netstandard.dll' or ln=='system.runtime.dll' then
	    if sysfile==nil then
		  sysfile=n
		end
	  end
    end)
    return r,sysfile
  end


  local dc=getDotNetDataCollector()
  local d=dc.enumDomains()
  local i
  for i=1,#d do
    local ml=dc.enumModuleList(d[i].DomainHandle)
    local j
    for j=1,#ml do
      local ln=extractFileName(ml[j].Name):lower()
      --not removing default core assembly from referrences  
	  r[#r+1]=ml[j].Name
	  printf("%d %s", j, ml[j].Name)
	  if ln=='mscorlib.dll' or ln=='netstandard.dll' or ln=='system.runtime.dll' then
	    if sysfile==nil then
		  sysfile=ml[j].Name
		end
	  end
    end
  end

  return r,sysfile
end

function SplitDotNetName(dotnetname)
  local r=mono_splitSymbol(dotnetname) --can handle both
  return r.namespace, r.classname, r.methodname
end

function findDotNetMethodAddress(name, modulename)
  --print(string.format("findDotNetMethodAddress('%s','%s')", name, modulename));

  local result
  local namespace, classname, methodname=SplitDotNetName(name)
  if namespace==nil then namespace='' end
  if classname==nil or classname=='' then return getAddressSafe(name) end --hexadecimal ?
  if methodname==nil or methodname=='' then return getAddressSafe(name) end
    
  if monopipe then
    --mono  
    local dllmodulelower=modulename:lower()
    
    --first try to get it manually using the .net interface currently used, if that fails, go for the symbolhandler method

    --find the image      
    local assemblies=mono_enumAssemblies() 
    for i=1,#assemblies do
      local img=mono_getImageFromAssembly(assemblies[i])
      local imagename=mono_image_get_filename(img):lower()
      local ln=extractFileName(imagename)
	  
      if ln==dllmodulelower then
        --find the class and method
        local class=mono_image_findClass(img, namespace, classname)          
        
        if class then
          local method=mono_class_findMethod(class, methodname)
          if method then
            return mono_compile_method(method)
          end
          
        end
        break
      end
    end
    
    --still here
    result=getAddressSafe(monoformat) --monoscript's symbolhandler will cause this method to get compiled
    
    if result==nil then
      return nil,name..' could not be resolved'
    end
  
  else
    --ms dotnet
    local dnformat='' 
    if namespace~='' then dnformat=namespace..'.' end
    dnformat=dnformat..classname..'::'..methodname
    
    result=getAddressSafe(dnformat) --the .net module injector tries to prelink all types in the module and reload the symbolhandler for that module 
    if result==nil then
      --try to find the stub
      result=dotnet_findDotNetMethodAddress(namespace, classname, methodname, modulename)
      
      if result==nil then 
        return nil,name..' could not be resolved'
      end
    end
  end
  
  return result  
end

function detourdotnet(oldmethodaddress,newmethodaddress,oldmethodcalleraddress)
  --write jmp newmethod at the compiled address of oldmethod and if oldmethodcaller is provided write a jmp <trampolinetoold> at oldmethodcaller
  --print(string.format("detourdotnet(%08x,%08x,%08x)",oldmethodaddress, newmethodaddress, oldmethodcalleraddress))
  

  local ahe,ahd=generateAPIHookScript(string.format("%.8x",oldmethodaddress), string.format("%.8x",newmethodaddress))
  script=ahe..string.format([[
%.8x:
jmp originalcall
]],oldmethodcalleraddress)
  
  
  --print('-------ENABLE-------')
  --print(script)
  --print('--------------------')
  --print('');
  --print('------DISABLE------')
  --print(ahd)
  --print('-------------------')

  local aaresult,disableinfo=autoAssemble(script)  
  if aaresult then
    return aaresult,disableinfo,ahd
  else
    return aaresult, disableinfo  
  end
end


function InjectDotNetDetour(dllmodule, oldmethodname, newmethodname, oldmethodcaller)
  --load the given dll, find and compile the methods, and call detourdotnet
  if dllmodule==nil then
    print(debug.traceback())
    print('InjectDotNetDetour: dllmodule is nil')
    error('InjectDotNetDetour: dllmodule is nil')
  end  
  
  --print(string.format("InjectDotNetDetour(%s, %s, %s, %s)", dllmodule, oldmethodname, newmethodname, oldmethodcaller))
  

  if monopipe then
    if mono_loadAssemblyFromFile(dllmodule)==nil then
      return false,'loading '+dllmodule+' failed'
    end

    --get the address of oldmethodname, newmethodname, and optionally oldmethodcaller

    
    local oldmethodaddress=getAddressSafe(oldmethodname)
    local oldmethodcalleraddress=getAddressSafe(oldmethodcaller)     
    local newmethodaddress=findDotNetMethodAddress(newmethodname, extractFileName(dllmodule)) --get that from the injected dll, not the symhandler (in case there's a previously injected dll)
    
    if oldmethodcaller and newmethodaddress and oldmethodcalleraddress then
      return detourdotnet(oldmethodaddress,newmethodaddress,oldmethodcalleraddress)
    else
      error("not all addresses found")
    end
     
  else
    LaunchDotNetInterface()
    --print("injecting module")
    if dotnet_loadModule(dllmodule)==false then
      return false,'loading '..dllmodule..' failed'
    end
    
    --print("Getting oldmethod address "..oldmethodname);
    local oldmethodAddress=getAddressSafe(oldmethodname)
    if oldmethodAddress==nil then
      --print(oldmethodname.." not perfect")
      oldmethodaddress=findDotNetMethodAddress(oldmethodname)
      
      if oldmethodaddress==nil then error('Failure getting '..oldmethodname) end
    end  
    --printf("oldmethodaddress=%.8x",oldmethodaddress)
    --print("--------------")
    
    
    --print("Getting newmethod address "..newmethodname);
    --local newmethodaddress=getAddressSafe(newmethodname)
    --if newmethodaddress==nil then     
      --print(newmethodname.." not perfect")
    local newmethodaddress=findDotNetMethodAddress(newmethodname, extractFileName(dllmodule))
     
    --  if newmethodaddress==nil then error('Failure getting '..newmethodname) end      
    --end  
    --printf("newmethodaddress=%.8x",newmethodaddress)
    --print("--------------")
   
    --print("Getting oldmethodcaller address "..oldmethodcaller);
    local oldmethodcalleraddress=getAddressSafe(oldmethodcaller)
    if oldmethodcalleraddress==nil then 
      --print(oldmethodcaller.." not perfect")
      oldmethodcalleraddress=findDotNetMethodAddress(oldmethodcaller, extractFileName(dllmodule))
      
      if oldmethodcalleraddress==nil then error('Failure getting '..oldmethodcalleraddress) end  
    end       
    --printf("oldmethodcalleraddress=%.8x",oldmethodcalleraddress)
   -- print("--------------")

    
    if oldmethodaddress and newmethodaddress and oldmethodcalleraddress then
      return detourdotnet(oldmethodaddress,newmethodaddress,oldmethodcalleraddress)
    else
      error("not all addresses found")
    end    
  end


end
