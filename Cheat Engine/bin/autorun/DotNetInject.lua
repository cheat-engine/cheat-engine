--[[
You have a d:\bla.dll with namespace ClassLibraryX, with a class named "MyClass"
That class contains a function defined as:
public static int MyInitFunctionName(string parameters)

then you do: injectDotNetDLL('d:\\bla.dll','ClassLibraryX.MyClass','MyInitFunctionName','Something')

--]]

local DotNetCoreInjectScript=[[
[enable]
alloc(injectdotnetdll, 2048)
alloc(IID_ICLRRuntimeHost4,16)
alloc(RuntimeHost,8)

alloc(paramstr,256)
alloc(methodname,256)
alloc(classname,256)
alloc(dllpath,512)

alloc(returnvalue,4)
alloc(errorvalue,4)
label(error)

dllpath:
dw '%s',0

classname:
dw '%s',0

methodname:
dw '%s',0

paramstr:
dw '%s',0


IID_ICLRRuntimeHost4:
db 66 d3 f6 64 c2 d7 1f 4f b4 b2 e8 16 0c ac 43 af

injectdotnetdll:
[64-bit]
sub rsp,6*8+8
mov rcx,IID_ICLRRuntimeHost4
mov rdx,RuntimeHost
[/64-bit]

[32-bit]
push RuntimeHost
push IID_ICLRRuntimeHost4
[/32-bit]

call GetCLRRuntimeHost
cmp eax,0
jne error

[64-bit]
mov rcx,[RuntimeHost]
mov rax,[rcx]
mov rdx,dllpath
mov r8,classname
mov r9,methodname
mov rbx,paramstr
mov [rsp+20],rbx
mov rbx,returnvalue
mov [rsp+28],rbx
call [rax+b*8]
[/64-bit]

[32-bit]
mov ecx,[RuntimeHost]
mov eax,[ecx]
push returnvalue
push paramstr
push methodname
push classname
push dllpath
push ecx
call [eax+b*4]
[/32-bit]

mov [errorvalue],eax

error:
[64-bit]
add rsp,6*8+8
ret
[/64-bit]

[32-bit]
ret 4
[/32-bit]

createthreadandwait(injectdotnetdll)

[disable]
dealloc(injectdotnetdll)
dealloc(IID_ICLRRuntimeHost4)
dealloc(RuntimeHost)

dealloc(paramstr)
dealloc(methodname)
dealloc(classname)
dealloc(dllpath)

dealloc(returnvalue)
dealloc(errorvalue)

]]


----------------------dot net---------------------------------------

local DotNetStandardInjectScript=[[
[enable]
alloc(injectdotnetdll,4096)
alloc(metahost,8)
alloc(RuntimeEnum,8)
alloc(RuntimeInfo,8)
alloc(RuntimeHost,8)
alloc(IID_ICLRMetaHost,16)
alloc(CLSID_CLRMetaHost,16)

alloc(Count,4)
alloc(IID_ICLRRuntimeHost,16)
alloc(CLSID_CLRRuntimeHost,16)
alloc(returnvalue,4)
alloc(errorvalue,4)

alloc(rti_flags,4)
alloc(rti_started,4)


alloc(paramstr,256)
alloc(methodname,256)
alloc(classname,256)
alloc(dllpath,512)
label(error)
label(RuntimeEnumLoop)

errorvalue:
dd ffffffff //ffffffff= it never even got to the execute part

dllpath:
dw '%s',0

classname:
dw '%s',0

methodname:
dw '%s',0

paramstr:
dw '%s',0


IID_ICLRMetaHost:
db 9E DB 32 D3 B3 B9 25 41 82 07 A1 48 84 F5 32 16

CLSID_CLRMetaHost:
db 8D 18 80 92 8E 0E 67 48 B3 0C 7F A8 38 84 E8 DE

IID_ICLRRuntimeHost:
db 6C A0 F1 90 12 77 62 47 86 B5 7A 5E BA 6B DB 02

CLSID_CLRRuntimeHost:
db 6E A0 F1 90 12 77 62 47 86 B5 7A 5E BA 6B DB 02

injectdotnetdll:
[64-bit]
sub rsp,6*8+8

mov rcx,CLSID_CLRMetaHost
mov rdx,IID_ICLRMetaHost
mov r8,metahost
[/64-bit]

[32-bit]
push metahost
push IID_ICLRMetaHost
push CLSID_CLRMetaHost
[/32-bit]
call MSCOREE.CLRCreateInstance

cmp eax,0
jne error

[64-bit]
mov rcx,[metahost]
mov rax,[rcx]
mov rdx,-1
mov r8,RuntimeEnum
call [rax+6*8] //EnumerateLoadedRuntimes
[/64-bit]

[32-bit]
mov ecx,[metahost]
mov eax,[ecx]
push RuntimeEnum
push -1
push ecx
call [eax+6*4] //EnumerateLoadedRuntimes
[/32-bit]


cmp eax,0
jne error


RuntimeEnumLoop:
[64-bit]
mov rcx,[RuntimeEnum]
mov rax,[rcx]

mov rdx,1
mov r8,RuntimeInfo
mov r9,Count
call [rax+3*8]  //RuntimeEnum->Next
[/64-bit]

[32-bit]
mov ecx,[RuntimeEnum]
mov eax,[ecx]
push Count
push RuntimeInfo
push 1
push ecx
call [eax+3*4]  //RuntimeEnum->Next
[/32-bit]

cmp eax,0
jne error

[64-bit]
mov rcx,[RuntimeInfo]
mov rax,[rcx]
mov rdx,rti_started
mov r8,rti_flags
call [rax+e*8] //RunTimeInfo->isStarted(started,flags)
[/64-bit]

[32-bit]
mov ecx,[RuntimeInfo]
mov eax,[ecx]
push rti_flags
push rti_started
push ecx
call [eax+e*4] //RunTimeInfo->isStarted(started,flags)
[/32-bit]

cmp eax,0
jne RuntimeEnumLoop

cmp dword [rti_flags],0
je RuntimeEnumLoop

//started
[64-bit]
mov rcx,[RuntimeInfo]
mov rax,[rcx]
mov rdx,CLSID_CLRRuntimeHost
mov r8,IID_ICLRRuntimeHost
mov r9,RuntimeHost
call [rax+9*8] //GetInterface
[/64-bit]

[32-bit]
mov ecx,[RuntimeInfo]
mov eax,[ecx]
push RuntimeHost
push IID_ICLRRuntimeHost
push CLSID_CLRRuntimeHost
push ecx
call [eax+9*4] //GetInterface
[/32-bit]

cmp eax,0
jne RuntimeEnumLoop

[64-bit]
mov rcx,[RuntimeHost]
mov rax,[rcx]
mov rdx,dllpath
mov r8,classname
mov r9,methodname
mov rbx,paramstr
mov [rsp+20],rbx
mov rbx,returnvalue
mov [rsp+28],rbx
call [rax+b*8]
[/64-bit]

[32-bit]
mov ecx,[RuntimeHost]
mov eax,[ecx]
push returnvalue
push paramstr
push methodname
push classname
push dllpath
push ecx
call [eax+b*4]
[/32-bit]

mov [errorvalue],eax

jmp RuntimeEnumLoop

error:
[64-bit]
add rsp,6*8+8
ret
[/64-bit]

[32-bit]
ret 4
[/32-bit]

createthreadandwait(injectdotnetdll)

[disable]
dealloc(injectdotnetdll)
dealloc(metahost)
dealloc(RuntimeEnum)
dealloc(RuntimeInfo)
dealloc(RuntimeHost)
dealloc(IID_ICLRMetaHost)
dealloc(CLSID_CLRMetaHost)

dealloc(Count)
dealloc(IID_ICLRRuntimeHost)
dealloc(CLSID_CLRRuntimeHost)
dealloc(returnvalue)
dealloc(errorvalue)

dealloc(rti_flags)
dealloc(rti_started)


dealloc(paramstr)
dealloc(methodname)
dealloc(classname)
dealloc(dllpath)


]]

function injectDotNetDLL(path, classname, methodname, parameter, timeout)
  if monopipe then
    --inject the dll using mono
    local assembly=mono_loadAssemblyFromFile(path)
    
    if (classname==nil) and (method==nil) then return assembly end
    
    if assembly and classname and methodname then
      local image=mono_getImageFromAssembly(assembly)
      if image then
        --find class
        local c=mono_image_enumClasses(image)
        for i=1,#c do        
          if c[i].classname==classname then
            local m=mono_class_enumMethods(c[i].class)
            if m.name==methodname then
              local result
              if parameter then
                local args={}
                args[1]={}
                args[1].type=vtString
                args[1].value=parameter               
                
                result=mono_invoke_method(nil,m,nil,{})
              else
                result=mono_invoke_method(nil,m,nil,nil)
              end
            
              return result
            end
          end
        end
      end
      
    end
    
    return
  end
  

  local m=enumModules()
  local isDotNetCore=false
  local isDotNetStandard=false
  for i=1,#m do
    local uppername=m[i].Name:upper()
    if uppername=='MSCOREE.DLL' then
      if getAddressSafe('MSCOREE.CLRCreateInstance') then
        isDotNetStandard=true
      end
    end

    if uppername=='CORECLR.DLL' then
      if getAddressSafe('CORECLR.GetCLRRuntimeHost') then
        isDotNetCore=true
      end
    end
  end

  local script
  if isDotNetCore then
    script=DotNetCoreInjectScript
  elseif isDotNetStandard then
    script=DotNetStandardInjectScript
  else
    return nil,-4 --no dotnet architecture detected. Todo: Create a .net host in the target?
  end


  local script=string.format(script,path, classname, methodname, parameter)
  status, disableInfo=autoAssemble(script)

  if status then
    local returnValue=readInteger(disableInfo.allocs.returnvalue.address)
    local errorValue=readInteger(disableInfo.allocs.errorvalue.address)
    autoAssemble(script, disableInfo)

    if errorValue==nil then
      return nil,-3   --target crashed...
    end

    if errorValue~=0 then --not a successful load
      if errorValue==0xffffffff then
        return nil, -2 --failed getting to the execute part
      else
        return nil, errorValue --execution gave this error (ntstatus)
      end
    end

    return returnValue
  else
    return nil,-1
  end

end

injectDotNetLibrary=injectDotNetDLL

