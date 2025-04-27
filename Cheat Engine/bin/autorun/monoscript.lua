if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'monoscript.po')
end

local thread_checkifmonoanyhow=nil
local StructureElementCallbackID=nil
local pathsep
local libfolder

if getOperatingSystem()==0 then
  pathsep=[[\]]
  libfolder='dlls'
else
  pathsep='/'
  libfolder='dylibs'
end

local dpiscale=getScreenDPI()/96

--[[local]] monocache={}

mono_timeout=0 --3000 --change to 0 to never timeout (meaning: 0 will freeze your face off if it breaks on a breakpoint, just saying ...)

MONO_DATACOLLECTORVERSION=20240511

MONOCMD_INITMONO=0
MONOCMD_OBJECT_GETCLASS=1
MONOCMD_ENUMDOMAINS=2
MONOCMD_SETCURRENTDOMAIN=3
MONOCMD_ENUMASSEMBLIES=4
MONOCMD_GETIMAGEFROMASSEMBLY=5
MONOCMD_GETIMAGENAME=6
MONOCMD_ENUMCLASSESINIMAGE=7
MONOCMD_ENUMFIELDSINCLASS=8
MONOCMD_ENUMMETHODSINCLASS=9
MONOCMD_COMPILEMETHOD=10
MONOCMD_GETMETHODHEADER=11
MONOCMD_GETMETHODHEADER_CODE=12
MONOCMD_LOOKUPRVA=13
MONOCMD_GETJITINFO=14
MONOCMD_FINDCLASS=15
MONOCMD_FINDMETHOD=16
MONOCMD_GETMETHODNAME=17
MONOCMD_GETMETHODCLASS=18
MONOCMD_GETCLASSNAME=19
MONOCMD_GETCLASSNAMESPACE=20
MONOCMD_FREEMETHOD=21
MONOCMD_TERMINATE=22
MONOCMD_DISASSEMBLE=23
MONOCMD_GETMETHODSIGNATURE=24
MONOCMD_GETPARENTCLASS=25
MONOCMD_GETSTATICFIELDADDRESSFROMCLASS=26
MONOCMD_GETFIELDCLASS=27
MONOCMD_GETARRAYELEMENTCLASS=28
MONOCMD_FINDMETHODBYDESC=29
MONOCMD_INVOKEMETHOD=30
MONOCMD_LOADASSEMBLY=31
MONOCMD_GETFULLTYPENAME=32

MONOCMD_OBJECT_NEW=33
MONOCMD_OBJECT_INIT=34
MONOCMD_GETVTABLEFROMCLASS=35
MONOCMD_GETMETHODPARAMETERS=36
MONOCMD_ISCLASSGENERIC=37
MONOCMD_ISIL2CPP=38

MONOCMD_FILLOPTIONALFUNCTIONLIST=39
MONOCMD_GETSTATICFIELDVALUE=40 --fallback for il2cpp which doesn't expose what's needed
MONOCMD_SETSTATICFIELDVALUE=41
MONOCMD_GETCLASSIMAGE=42
MONOCMD_FREE=43
MONOCMD_GETIMAGEFILENAME=44
MONOCMD_GETCLASSNESTINGTYPE=45
MONOCMD_LIMITEDCONNECTION=46
MONOCMD_GETMONODATACOLLECTORVERSION=47
MONOCMD_NEWSTRING=48

MONOCMD_ENUMIMAGES=49 
MONOCMD_ENUMCLASSESINIMAGEEX=50
MONOCMD_ISCLASSENUM = 51
MONOCMD_ISCLASSVALUETYPE = 52
MONOCMD_ISCLASSISSUBCLASSOF = 53
MONOCMD_ARRAYELEMENTSIZE = 54
MONOCMD_GETCLASSTYPE = 55
MONOCMD_GETCLASSOFTYPE = 56
MONOCMD_GETTYPEOFMONOTYPE = 57
MONOCMD_GETREFLECTIONTYPEOFCLASSTYPE = 58
MONOCMD_GETREFLECTIONMETHODOFMONOMETHOD = 59
MONOCMD_MONOOBJECTUNBOX = 60
MONOCMD_MONOARRAYNEW = 61
MONOCMD_ENUMINTERFACESOFCLASS = 62
MONOCMD_GETMETHODFULLNAME = 63
MONOCMD_TYPEISBYREF = 64
MONOCMD_GETPTRTYPECLASS = 65
MONOCMD_GETFIELDTYPE = 66
MONOCMD_GETTYPEPTRTYPE = 67
MONOCMD_GETCLASSNESTEDTYPES = 68
MONOCMD_COLLECTGARBAGE = 69
MONOCMD_GETMETHODFLAGS = 70

MONO_TYPE_END        = 0x00       -- End of List
MONO_TYPE_VOID       = 0x01
MONO_TYPE_BOOLEAN    = 0x02
MONO_TYPE_CHAR       = 0x03
MONO_TYPE_I1         = 0x04
MONO_TYPE_U1         = 0x05
MONO_TYPE_I2         = 0x06
MONO_TYPE_U2         = 0x07
MONO_TYPE_I4         = 0x08
MONO_TYPE_U4         = 0x09
MONO_TYPE_I8         = 0x0a
MONO_TYPE_U8         = 0x0b
MONO_TYPE_R4         = 0x0c
MONO_TYPE_R8         = 0x0d
MONO_TYPE_STRING     = 0x0e
MONO_TYPE_PTR        = 0x0f       -- arg: <type> token
MONO_TYPE_BYREF      = 0x10       -- arg: <type> token
MONO_TYPE_VALUETYPE  = 0x11       -- arg: <type> token
MONO_TYPE_CLASS      = 0x12       -- arg: <type> token
MONO_TYPE_VAR         = 0x13          -- number
MONO_TYPE_ARRAY      = 0x14       -- type, rank, boundsCount, bound1, loCount, lo1
MONO_TYPE_GENERICINST= 0x15          -- <type> <type-arg-count> <type-1> \x{2026} <type-n> */
MONO_TYPE_TYPEDBYREF = 0x16
MONO_TYPE_I          = 0x18
MONO_TYPE_U          = 0x19
MONO_TYPE_FNPTR      = 0x1b          -- arg: full method signature */
MONO_TYPE_OBJECT     = 0x1c
MONO_TYPE_SZARRAY    = 0x1d       -- 0-based one-dim-array */
MONO_TYPE_MVAR       = 0x1e       -- number */
MONO_TYPE_CMOD_REQD  = 0x1f       -- arg: typedef or typeref token */
MONO_TYPE_CMOD_OPT   = 0x20       -- optional arg: typedef or typref token */
MONO_TYPE_INTERNAL   = 0x21       -- CLR internal type */

MONO_TYPE_MODIFIER   = 0x40       -- Or with the following types */
MONO_TYPE_SENTINEL   = 0x41       -- Sentinel for varargs method signature */
MONO_TYPE_PINNED     = 0x45       -- Local var that points to pinned object */

MONO_TYPE_ENUM       = 0x55        -- an enumeration */

monoTypeToVartypeLookup={} --for dissect data
monoTypeToVartypeLookup[MONO_TYPE_BOOLEAN]=vtByte 
monoTypeToVartypeLookup[MONO_TYPE_CHAR]=vtUnicodeString --the actual chars...
monoTypeToVartypeLookup[MONO_TYPE_I1]=vtByte
monoTypeToVartypeLookup[MONO_TYPE_U1]=vtByte
monoTypeToVartypeLookup[MONO_TYPE_I2]=vtWord
monoTypeToVartypeLookup[MONO_TYPE_U2]=vtWord
monoTypeToVartypeLookup[MONO_TYPE_I4]=vtDword
monoTypeToVartypeLookup[MONO_TYPE_U4]=vtDword
monoTypeToVartypeLookup[MONO_TYPE_I8]=vtQword
monoTypeToVartypeLookup[MONO_TYPE_U8]=vtQword
monoTypeToVartypeLookup[MONO_TYPE_R4]=vtSingle
monoTypeToVartypeLookup[MONO_TYPE_R8]=vtDouble
monoTypeToVartypeLookup[MONO_TYPE_STRING]=vtPointer --pointer to a string object
monoTypeToVartypeLookup[MONO_TYPE_PTR]=vtPointer
monoTypeToVartypeLookup[MONO_TYPE_I]=vtPointer --IntPtr
monoTypeToVartypeLookup[MONO_TYPE_U]=vtPointer
monoTypeToVartypeLookup[MONO_TYPE_OBJECT]=vtPointer --object
monoTypeToVartypeLookup[MONO_TYPE_BYREF]=vtPointer
monoTypeToVartypeLookup[MONO_TYPE_CLASS]=vtPointer
monoTypeToVartypeLookup[MONO_TYPE_FNPTR]=vtPointer
monoTypeToVartypeLookup[MONO_TYPE_GENERICINST]=vtPointer
monoTypeToVartypeLookup[MONO_TYPE_ARRAY]=vtPointer
monoTypeToVartypeLookup[MONO_TYPE_SZARRAY]=vtPointer
monoTypeToVartypeLookup[MONO_TYPE_VALUETYPE]=vtPointer --needed for structs when returned by invoking a method( even though they are not qwords)

monoTypeToCStringLookup={}
monoTypeToCStringLookup[MONO_TYPE_END]='void'
monoTypeToCStringLookup[MONO_TYPE_BOOLEAN]='boolean'
monoTypeToCStringLookup[MONO_TYPE_CHAR]='char'
monoTypeToCStringLookup[MONO_TYPE_I1]='char'
monoTypeToCStringLookup[MONO_TYPE_U1]='unsigned char'
monoTypeToCStringLookup[MONO_TYPE_I2]='short'
monoTypeToCStringLookup[MONO_TYPE_U2]='unsigned short'
monoTypeToCStringLookup[MONO_TYPE_I4]='int'
monoTypeToCStringLookup[MONO_TYPE_U4]='unsigned int'
monoTypeToCStringLookup[MONO_TYPE_I8]='int64'
monoTypeToCStringLookup[MONO_TYPE_U8]='unsigned int 64'
monoTypeToCStringLookup[MONO_TYPE_R4]='single'
monoTypeToCStringLookup[MONO_TYPE_R8]='double'
monoTypeToCStringLookup[MONO_TYPE_STRING]='String'
monoTypeToCStringLookup[MONO_TYPE_PTR]='Pointer'
monoTypeToCStringLookup[MONO_TYPE_BYREF]='Object'
monoTypeToCStringLookup[MONO_TYPE_CLASS]='Object'
monoTypeToCStringLookup[MONO_TYPE_FNPTR]='Function'
monoTypeToCStringLookup[MONO_TYPE_GENERICINST]='<Generic>'
monoTypeToCStringLookup[MONO_TYPE_ARRAY]='Array[]'
monoTypeToCStringLookup[MONO_TYPE_SZARRAY]='String[]'


FIELD_ATTRIBUTE_FIELD_ACCESS_MASK=0x0007
FIELD_ATTRIBUTE_COMPILER_CONTROLLED=0x0000
FIELD_ATTRIBUTE_PRIVATE=0x0001
FIELD_ATTRIBUTE_FAM_AND_ASSEM=0x0002
FIELD_ATTRIBUTE_ASSEMBLY=0x0003
FIELD_ATTRIBUTE_FAMILY=0x0004
FIELD_ATTRIBUTE_FAM_OR_ASSEM=0x0005
FIELD_ATTRIBUTE_PUBLIC=0x0006
FIELD_ATTRIBUTE_STATIC=0x0010
FIELD_ATTRIBUTE_INIT_ONLY=0x0020
FIELD_ATTRIBUTE_LITERAL=0x0040
FIELD_ATTRIBUTE_NOT_SERIALIZED=0x0080
FIELD_ATTRIBUTE_SPECIAL_NAME=0x0200
FIELD_ATTRIBUTE_PINVOKE_IMPL=0x2000
FIELD_ATTRIBUTE_RESERVED_MASK=0x9500
FIELD_ATTRIBUTE_RT_SPECIAL_NAME=0x0400
FIELD_ATTRIBUTE_HAS_FIELD_MARSHAL=0x1000
FIELD_ATTRIBUTE_HAS_DEFAULT=0x8000
FIELD_ATTRIBUTE_HAS_FIELD_RVA=0x0100

METHOD_ATTRIBUTE_MEMBER_ACCESS_MASK      =0x0007
METHOD_ATTRIBUTE_COMPILER_CONTROLLED     =0x0000
METHOD_ATTRIBUTE_PRIVATE                 =0x0001
METHOD_ATTRIBUTE_FAM_AND_ASSEM           =0x0002
METHOD_ATTRIBUTE_ASSEM                   =0x0003
METHOD_ATTRIBUTE_FAMILY                  =0x0004
METHOD_ATTRIBUTE_FAM_OR_ASSEM            =0x0005
METHOD_ATTRIBUTE_PUBLIC                  =0x0006

METHOD_ATTRIBUTE_STATIC                  =0x0010
METHOD_ATTRIBUTE_FINAL                   =0x0020
METHOD_ATTRIBUTE_VIRTUAL                 =0x0040
METHOD_ATTRIBUTE_HIDE_BY_SIG             =0x0080

METHOD_ATTRIBUTE_VTABLE_LAYOUT_MASK      =0x0100
METHOD_ATTRIBUTE_REUSE_SLOT              =0x0000
METHOD_ATTRIBUTE_NEW_SLOT                =0x0100

METHOD_ATTRIBUTE_STRICT                  =0x0200
METHOD_ATTRIBUTE_ABSTRACT                =0x0400
METHOD_ATTRIBUTE_SPECIAL_NAME            =0x0800

METHOD_ATTRIBUTE_PINVOKE_IMPL            =0x2000
METHOD_ATTRIBUTE_UNMANAGED_EXPORT        =0x0008
 
    

MONO_TYPE_NAME_FORMAT_IL=0
MONO_TYPE_NAME_FORMAT_REFLECTION=1
MONO_TYPE_NAME_FORMAT_FULL_NAME=2
MONO_TYPE_NAME_FORMAT_ASSEMBLY_QUALIFIED=3

function mono_clearcache()
  monocache={}
  monocache.processid=getOpenedProcessID()
end




function monoTypeToVarType(monoType)
--MonoTypeEnum
  local result=monoTypeToVartypeLookup[monoType]

  if result==nil then
    result=vtDword --just give it something
  end

  return result
end

function parseImage(t, image)
  if image.parsed then return end
  
 
  local classes=mono_image_enumClasses(image.handle)
  if t.Terminated then return end
  
  if classes then
    --monoSymbolList.addSymbol('','Pen15',address,1)
    local i
    for i=1,#classes do
      local classname=classes[i].classname
      local namespace=classes[i].namespace
      local methods=mono_class_enumMethods(classes[i].class)
      
      if methods then
        local j
        for j=1,#methods do
          local address=readPointer(methods[j].method) --first pointer is a pointer to the code
          if address and address~=0 then
            local sname=classname..'.'..methods[j].name
            
            if namespace and namespace~='' then
              sname=namespace..'.'..sname
            end

            monoSymbolList.addSymbol('',sname,address,1)
          end
        end      
      end
      
      if t.Terminated then return end
    end  
  end 
  
end

function monoIL2CPPSymbolEnum(t)
  t.freeOnTerminate(false)
  t.Name='monoIL2CPPSymbolEnum'
  
 -- print("monoIL2CPPSymbolEnum");
  
  local priority=nil  
  --first enum all images
  local images={}
  local assemblies=mono_enumAssemblies() 
  
  monoSymbolList.IL2CPPSymbolEnumProgress=0
  
  
  if assemblies then
    for i=1,#assemblies do      
      images[i]={}
      images[i].handle=mono_getImageFromAssembly(assemblies[i])
      images[i].name=mono_image_get_name(images[i].handle)
      images[i].parsed=false
      if images[i].name=='Assembly-CSharp.dll' then
        priority=i
      end
    end
  end
      
  if monopipe==nil or t.Terminated then return end
  
  if priority then
    parseImage(t, images[priority])
    monoSymbolList.IL2CPPSymbolEnumProgress=(1/#assemblies) * 100
  end
  if monopipe==nil or t.Terminated then return end
  
  for i=1,#images do
    local x=i
    
    if i~=priority then        
      parseImage(t, images[i])
    end
    
    if priority then
      monoSymbolList.IL2CPPSymbolEnumProgress=((i-1)/#assemblies) * 100    
    else
      monoSymbolList.IL2CPPSymbolEnumProgress=(i/#assemblies) * 100
    end
    
    if monopipe==nil or t.Terminated then return end
  end

  --print("all symbols loaded") --print is threadsafe
  monoSymbolList.FullyLoaded=true
end

function mono_StructureListCallback()
  local r={}
  local ri=1;
  if monopipe then
    --return a list of all classes
    --print("Getting classlist")
    mono_enumImages(
      function(image)
        --enum classes
        --print("Getting classes for ".. mono_image_get_name(image))
        local classlist=mono_image_enumClasses(image)
        if classlist then
          local i
          for i=1,#classlist do            
            r[ri]={}
            r[ri].name=classlist[i].classname
            r[ri].id1=classlist[i].class
            ri=ri+1
          end
        end
      end
    )
  
    
  end
  
  return r
end

function mono_ElementListCallback(class) --2nd param ignored
  local r={}
  --print("Getting class fields for "..class..",",extra)
  if monopipe~=nil then
    --enumerate the fields in the class and return it
    local fields=mono_class_enumFields(class, true) 
    if fields then    
      for i=1, #fields do
        if fields[i].isStatic==false then
          r[i]={}
          r[i].name=fields[i].name
          r[i].offset=fields[i].offset
          r[i].vartype=monoTypeToVarType(fields[i].monotype)                    
        end
      end
    end
    
  end
  
  return r
end


function fillMissingFunctions()
  local result
  
  outputDebugString('fillMissingFunctions')
  local mono_type_get_name_full
  if getOperatingSystem()==0 then  --windows
    waitForExports()
    mono_type_get_name_full=getAddressSafe("mono_type_get_name_full") --it's defined as a symbol, but not public on some games
  else
    outputDebugString('get address of mono_type_get_name_full')
    mono_type_get_name_full=getAddressSafe("mono_type_get_name_full")
  end
  
  
  local cmd=MONOCMD_FILLOPTIONALFUNCTIONLIST
  monopipe.lock()
  monopipe.writeByte(MONOCMD_FILLOPTIONALFUNCTIONLIST)
  monopipe.writeQword(mono_type_get_name_full)
  result=monopipe.readByte()
  monopipe.unlock()
  
  return result
end

local lastMonoError

function mono_connectionmode2()
  monopipe.lock()
  monopipe.writeByte(MONOCMD_LIMITEDCONNECTION)
  monopipe.writeByte(1);
  monopipe.unlock()
end

function mono_connectionmode1()
  monopipe.lock()
  monopipe.writeByte(MONOCMD_LIMITEDCONNECTION)
  monopipe.writeByte(0);
  monopipe.unlock()
end



function LaunchMonoDataCollector(internalReconnectDisconnectEachTime)
  if debug_isBroken() then
    if inMainThread() then   
      messageDialog(translate('You can not use this while the process is frozen'), mtError, mbOK)
    end
    return nil
  end
  
  --if debug_canBreak() then return 0 end
  
  if monoSymbolEnum then
    monoSymbolEnum.terminate()
    monoSymbolEnum.waitfor()
    --print("bye monoSymbolEnum")
    monoSymbolEnum.destroy()
    monoSymbolEnum=nil
  end
  
  if monoSymbolList then  
    --print("monoSymbolList exists");
    if tonumber(monoSymbolList.ProcessID)~=getOpenedProcessID() or (monoSymbolList.FullyLoaded==false) then     
      --print("new il2cpp SymbolList")
      monoSymbolList.destroy()
      monoSymbolList=nil
    end
  end

  if (monopipe~=nil) then
    if (mono_AttachedProcess==getOpenedProcessID()) then
      return monoBase --already attached to this process
    end
    monopipe.destroy()
    monopipe=nil
  end


  if (monoeventpipe~=nil) then
    monoeventpipe.destroy()
    monoeventpipe=nil
  end


  local dllname
  local dllpath
  
  local skipsymbols=true
  
  
  if isConnectedToCEServer() then
    local basename='libMonoDataCollector'
    if targetIsAndroid() then
      dllname=basename    
    else    
      --assume linux
      dllname=basename..'-linux'     
    end
    
    if targetIsArm() then
      if targetIs64Bit() then
        dllname=dllname..'-aarch64.so'
      else
        dllname=dllname..'-arm.so'
      end      
    else
      if targetIs64Bit() then
        dllname=dllname..'-x86_64.so'
      else
        dllname=dllname..'-i386.so'
      end
    end
    
    dllpath=getCEServerPath()..dllname
  else
    if getOperatingSystem()==0 then
      skipsymbols=false --for the alternative (can not create pipes) situation
      dllname="MonoDataCollector"
      if targetIs64Bit() then
        dllname=dllname.."64.dll"
      else
        dllname=dllname.."32.dll"
      end
    
    
      autoAssemble([[
        mono-2.0-bdwgc.mono_error_ok:
        mov eax,1
        ret
      ]]) --don't care if it fails


    else
      dllname='libMonoDataCollectorMac.dylib'
    end  
    
    dllpath=getAutorunPath()..libfolder..pathsep..dllname   
  end
  
 -- printf("Injecting %s\n", dllpath);


  local injectResult, injectError=injectLibrary(dllpath, skipsymbols)
  if (not injectResult) and isConnectedToCEServer() then --try the searchpath
    outputDebugString('mdc lua: calling injectLibrary')
    injectResult, injectError=injectLibrary(dllname, skipsymbols)
    outputDebugString('mdc lua: after calling injectLibrary')
  end
    
  if (skipsymbols==false) and (getAddressSafe("MDC_ServerPipe")==nil) then
    outputDebugString('mdc lua: calling waitForExports')
    waitForExports()
    if getAddressSafe("MDC_ServerPipe")==nil then
      print("DLL Injection failed or invalid DLL version")
      return 0
    end
  end

  --wait till attached
  local timeout=getTickCount()+5000
  while (monopipe==nil) and (getTickCount()<timeout) do
    if (skipsymbols==false) and (readInteger(getAddressSafe("MDC_ServerPipe"))==0xdeadbeef) then
      --likely an UWP target which can not create a named pipe
      --print("UWP situation")
      local serverpipe=createPipe('cemonodc_pid'..getOpenedProcessID(), 256*1024,1024)      
      local newhandle=duplicateHandle(serverpipe.Handle)
      serverpipe.destroy() --the old handle is not needed anymore
      --print("New pipe handle is "..newhandle)
      
      writeInteger(getAddressSafe("MDC_ServerPipe"), newhandle)      
    end
    
    monopipe=connectToPipe('cemonodc_pid'..getOpenedProcessID() ,mono_timeout)
  end

  if (monopipe==nil) then
    return 0 --failure
  end
  
  local v=mono_getMonoDatacollectorDLLVersion();
  if (v==nil) or (v~=MONO_DATACOLLECTORVERSION) then
    local s='There is an inconsistency with the monodatacollector dll and monoscript.lua . Unexpected behaviour and crashes are to be expected'
    if inMainThread then
      messageDialog(s, mtWarning)
    else
      print('Warning:'..s)
    end
  end
    
  
  monopipe.OnError=function(self)
    print("monopipe error")
    monopipe.OnTimeout(self)
  end

  monopipe.OnTimeout=function(self)  
    print("monopipe disconnected")
    
    local oldmonopipe=monopipe
    monopipe=nil
    mono_AttachedProcess=0
    monoBase=0
    
    if self then
      oldmonopipe.unlock()
    end
      
    if inMainThread() and monoSymbolEnum then
      monoSymbolEnum.terminate()    
      monoSymbolEnum.waitfor()
      print("terminating monoSymbolEnum due to timeout or error")
      monoSymbolEnum.destroy()
      monoSymbolEnum=nil
    end
    
    oldmonopipe.destroy()


    if StructureElementCallbackID then 
      unregisterStructureAndElementListCallback(StructureElementCallbackID)
      StructureElementCallbackID=nil
    end
    
    if (lastMonoError==nil) or (getTickCount()>lastMonoError+200) then  
      --print("monopipe error. Reattaching in 500 ms")
      --auto re-attach after error
      --createTimer(200,function()
        --print("Reattaching")
     --   LaunchMonoDataCollector()    
      --end)      
    --else
    --  print("monopipe error. Last reattach too soon. Giving up")
    end
    
    

    
    lastMonoError=getTickCount()
    
  end

  --in case you implement the profiling tools use a secondary pipe to receive profiler events
 -- while (monoeventpipe==nil) do
 --   monoeventpipe=connectToPipe('cemonodc_pid'..getOpenedProcessID()..'_events')
 -- end

  mono_AttachedProcess=getOpenedProcessID()

  monopipe.writeByte(CMD_INITMONO)
  monopipe.ProcessID=getOpenedProcessID()
  monoBase=monopipe.readQword()
  
  if (monoBase==nil) or (monoBase==0) then
    print("Mono not usable in target")
    return 0
  end


  if monoBase~=0 then
    if mono_AddressLookupID==nil then
      mono_AddressLookupID=registerAddressLookupCallback(mono_addressLookupCallback)
    end

    if mono_SymbolLookupID==nil then
      mono_SymbolLookupID=registerSymbolLookupCallback(mono_symbolLookupCallback, slNotSymbol)
    end

    if mono_StructureNameLookupID==nil then
      mono_StructureNameLookupID=registerStructureNameLookup(mono_structureNameLookupCallback)
    end

    if mono_StructureDissectOverrideID==nil then
      mono_StructureDissectOverrideID=registerStructureDissectOverride(mono_structureDissectOverrideCallback)
    end

  end


  
  StructureElementCallbackID=registerStructureAndElementListCallback(mono_StructureListCallback, mono_ElementListCallback)

  monopipe.IL2CPP=mono_isil2cpp()
  
  if monopipe.IL2CPP then
    if monoSymbolList==nil then
      monoSymbolList=createSymbolList()
      monoSymbolList.register() 
      monoSymbolList.ProcessID=getOpenedProcessID()
      monoSymbolList.FullyLoaded=false   
      monoSymbolList.IL2CPPSymbolEnumProgress=0
      monoSymbolEnum=createThread(monoIL2CPPSymbolEnum)
      
      createTimer(500,function()
        --print("0.5 second delayed timer running now")
        if monoSymbolList.FullyLoaded==false then
          --show a progressbar in CE                  
          if monoSymbolList.progressbar then         
            monoSymbolList.progressbar.destroy()
            monoSymbolList.progressbar=nil
          end
          
          local pb=monoSymbolList.progressbar
          
          pb=createProgressBar(MainForm.Panel4)
          pb.Align=alBottom
          pb.Max=100
          
          local pmCancelEnum=createPopupMenu(pb)
          local miCancelEnum=createMenuItem(pmCancelEnum)
          miCancelEnum.Caption=translate('Cancel symbol enum')          
          pb.PopupMenu=pmCancelEnum

          local pbl=createLabel(pb)
          pbl.Caption=translate('IL2CPP symbol enum: 0%')
          pbl.AnchorSideLeft.Control=pb
          pbl.AnchorSideLeft.Side=asrCenter

          pbl.AnchorSideTop.Control=pb
          pbl.AnchorSideTop.Side=asrCenter

          pb.Height=pbl.Height 
          monoSymbolList.progressbar=pb
          local t=createTimer(pb)
          t.enabled=true         
          t.interval=250        
          t.OnTimer=function()
            --print("Check progress")
            pb.Position=math.ceil(monoSymbolList.IL2CPPSymbolEnumProgress)
    
            pbl.Caption=string.format("IL2CPP symbol enum: %.f%%",monoSymbolList.IL2CPPSymbolEnumProgress)
            if monoSymbolList.FullyLoaded then
              --print("done. Turning off check timer, and starting cleanup timer in 1.5 seconds")
              t.enabled=false
              
              pb.Position=100
              pbl.Caption=string.format("IL2CPP symbol enum: Done"); --enum done. Now wait 1.5 seconds and then delete the bar 
              
              createTimer(1500,function()
                --print("cleanup timer that runs after 1.5 seconds. destroying progressbar")                             
                pb.destroy() --also destroys t
              end)
            end
          end
        end
      end)
    end
  end
  
  if getOperatingSystem()==1 then
    --mac sometimes doesn't export mono_type_get_name_full but the symbol is defined. CE can help with this
    fillMissingFunctions()
    
    
    monopipe.AntiIdleThread=createTimer()
      --in some games on the mac version the mainthread freezes when the thread is suspended/idle and not really sure why. fetching the domains resumes the game
      if monopipe then
        monopipe.AntiIdleThread.Interval=50
        monopipe.AntiIdleThread.OnTimer=function(t)
          mono_enumDomains()
        end
      else
        t.destroy()
      end
  end
  
  if internalReconnectDisconnectEachTime==nil then --old scripts don't give the parameter
    internalReconnectDisconnectEachTime=false     
  end
  
  if internalReconnectDisconnectEachTime then
    mono_connectionmode2() --Change the behaviour from always connected to the mono runtime to only issuing a command, attach the handler thread to the mono runtime, and afterwards disconnect the thread from the mono runtime
  end 

  mono_clearcache()  
  
  if miMonoTopMenuItem==nil then --launched mono with lua before it was detected
    mono_setMonoMenuItem(true,false) 
  end
  
  return monoBase
end

function mono_structureDissectOverrideCallback(structure, baseaddress)
--  print("oc")
  if monopipe==nil then return nil end
  
  local realaddress, classaddress=mono_object_findRealStartOfObject(baseaddress)
  if (realaddress==baseaddress) then
    local smap = {}
    local s = monoform_exportStructInternal(structure, classaddress, true, false, smap, false)
    return s~=nil
  else
    return nil
  end
end


function mono_structureNameLookupCallback(address)
  local currentaddress, classaddress, classname

  if monopipe==nil then return nil end
  
  local always=monoSettings.Value["AlwaysUseForDissect"]
  local r
  if (always==nil) or (always=="") then
    r=messageDialog(translate("Do you wish to let the mono extention figure out the name and start address? If it's not a proper object this may crash the target."), mtConfirmation, mbYes, mbNo, mbYesToAll, mbNoToAll)    
  else
    if (always=="1") then
      r=mrYes
    else
      r=mrNo
    end
  end
  
  
  if (r==mrYes) or (r==mbYesToAll) then
    currentaddress, classaddress, classname=mono_object_findRealStartOfObject(address)

    if (currentaddress~=nil) then
      -- print("currentaddress~=nil : "..currentaddress)
      return classname,currentaddress
    else
      --  print("currentaddress==nil")
      return nil
    end
  end

  --still alive, so the user made a good choice
  if (r==mrYesToAll) then
    monoSettings.Value["AlwaysUseForDissect"]="1"
  elseif (r==mrNoToAll) then
    monoSettings.Value["AlwaysUseForDissect"]="0"
  end
end

function mono_splitSymbol(symbol)
  local result=nil
  
  local parts={}
  local x
  for x in string.gmatch(symbol, "[^:.]+") do
    table.insert(parts, x)
  end

  local methodname=''
  local classname=''
  local namespace=''

  if (#parts>0) then
    methodname=(symbol:find("[:.]%.cc?tor$") ~= nil and '.' or '')..parts[#parts] --methodname=parts[#parts]    
    if (#parts>1) then
      classname=parts[#parts-1]
      if (#parts>2) then
        for x=1,#parts-2 do
          if x==1 then
            namespace=parts[x]
          else
            namespace=namespace..'.'..parts[x]
          end
        end
      end
    end
  end
  --[[
  if (methodname=='ctor' and symbol.endswith('.ctor')) or
     (methodname=='cctor' and symbol.endswith('.cctor')) then
     methodname='.'..methodname
  end--]]
  
  
  result={}
  result.methodname=methodname
  result.classname=classname
  result.namespace=namespace  
  
  return result
end

function mono_symbolLookupCallback(symbol)
  --if debug_canBreak() then return nil end

  if monopipe == nil then return nil end  
  if monopipe.IL2CPP then return nil end

  if symbol:match('[()%[%]]')~=nil then return nil end --no formulas/indexer

  
  local methodname=''
  local classname=''
  local namespace=''
  
  local ss=mono_splitSymbol(symbol)
  methodname=ss.methodname
  classname=ss.classname
  namespace=ss.namespace

  if (methodname~='') and (classname~='') then
    local method=mono_findMethod(namespace, classname, methodname)
    if (method==0) then
      return nil
    end

    local methodaddress=mono_compile_method(method)
    if (methodaddress~=0) then
      return methodaddress
    end

  end

  --still here,
  return nil

end


function mono_addressLookupCallback(address)
  --if (inMainThread()==false) or (debug_canBreak()) then --the debugger thread might call this
  --  return nil
  --end
  if monopipe==nil then return nil end
  if monopipe.IL2CPP then return nil end
  
  if debug_isBroken() then return nil end
  if tonumber(monopipe.ProcessID)~=getOpenedProcessID() then return nil end

  local ji=mono_getJitInfo(address)
  local result=''
  if ji~=nil then
--[[
        ji.jitinfo;
        ji.method
        ji.code_start
        ji.code_size
--]]
    if (ji.method~=0) then
      local class=mono_method_getClass(ji.method)

      if class==nil then return nil end


      local classname=mono_class_getName(class)
      local namespace=mono_class_getNamespace(class)
      if (classname==nil) or (namespace==nil) then return nil end

      if namespace~='' then
        namespace=namespace..':'
      end
      
      if mono_class_getNestingType(class) then
        result=mono_class_getFullName(class)..":"..mono_method_getName(ji.method)            
      else
        result=namespace..classname..":"..mono_method_getName(ji.method)      
      end      
      
      if address~=ji.code_start then
        result=result..string.format("+%x",address-ji.code_start)
      end
    end

  end

  return result
end

function mono_object_getClass(address)
  --if debug_canBreak() then return nil end
  if monopipe==nil then return nil end

  monopipe.lock()
  monopipe.writeByte(MONOCMD_OBJECT_GETCLASS)
  monopipe.writeQword(address)

  local classaddress=monopipe.readQword()
  if (classaddress~=nil) and (classaddress~=0) then
    if monopipe==nil then return nil end
    local stringlength=monopipe.readWord()
    local classname

    if stringlength>0 then
      classname=monopipe.readString(stringlength)
    end
    monopipe.unlock()

    return classaddress, classname
  else
    if monopipe then
      monopipe.unlock()
    end
    
    return nil
  end
end

function mono_image_enumClassesEx(image)

  --printf("mono_image_enumClassesEx(%.8x)", image)
  local result=nil
  
  if monopipe then
    m=createMemoryStream()      
    m.writeByte(MONOCMD_ENUMCLASSESINIMAGEEX)
    m.writeQword(image)
    m.Position=0    
    
    monopipe.lock()   
   -- printf("calling writeFromStream with %d bytes", m.size)
    monopipe.writeFromStream(m,m.size)    
    
   -- printf("after write")
    m.clear()
    
    local datasize=monopipe.readDword()
    
  --  printf("datasize=%d", datasize);
    
    monopipe.readIntoStream(m, datasize)    
    monopipe.unlock()
    
    result={}
    --parse the received data
    m.Position=0
    local count=m.readDword()
    for i=1,count do    
      local Class={}
      local l
      Class.Handle=m.readQword()
      Class.ParentHandle=m.readQword()
      Class.NestingTypeHandle=m.readQword()
      l=m.readWord()      
      Class.Name=m.readString(l)
      l=m.readWord()
      Class.NameSpace=m.readString(l)
      l=m.readWord()
      Class.FullName=m.readString(l)
      
      if Class.NestingTypeHandle==0 then
        if Class.NameSpace~='' then
          Class.FullName=Class.NameSpace..'.'..Class.Name
        else
          Class.FullName=Class.Name
        end
      end

      
      table.insert(result,Class)
    end    
    
    m.destroy()   
    
  end
  
  return result

end

function mono_enumImagesEx(domain) 
  --returns all the image object and the full paths to the images in one go

  local result=nil
  --if debug_canBreak() then return nil end
  if monopipe then
    monopipe.lock()
    monopipe.writeByte(MONOCMD_ENUMIMAGES)
    local datasize=monopipe.readDword()
    
    local m=createMemoryStream()    
    monopipe.readIntoStream(m, datasize)
    monopipe.unlock()
    
    result={}
    --parse the received data
    m.Position=0
    while m.Position<m.Size do
      local img={}
      img.Image=m.readQword()
      
      local sl=m.readWord()
      img.Path=m.readString(sl)
      
      table.insert(result,img)
    end    
    
    m.destroy()   
    

    
  end
  return result
end


function mono_enumImages(onImage)
  local assemblies=mono_enumAssemblies()
  if assemblies then
    for i=1,#assemblies do
      local image=mono_getImageFromAssembly(assemblies[i])
      if image and (image~=0) then
        onImage(image)      
      end
    end
  end
end

function mono_enumDomains()
  --if debug_canBreak() then return nil end

  if monopipe==nil then return nil end


  monopipe.lock()
  if monopipe==nil then return nil end
  
  monopipe.writeByte(MONOCMD_ENUMDOMAINS)  
  if monopipe==nil then return nil end
  
  local count=monopipe.readDword()
  if monopipe==nil then return nil end
  
  local result={}
  local i
  if (count~=nil) then
    for i=1, count do      
      result[i]=monopipe.readQword()
      if monopipe==nil then return nil end
    end
  end
  
  if monopipe==nil then return nil end
  monopipe.unlock()

  return result
end

function mono_getMonoDatacollectorDLLVersion()
  local r=nil
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMONODATACOLLECTORVERSION)
  r=monopipe.readDword()  
  if monopipe then
    monopipe.unlock()
  end
  
  return r
end


function mono_setCurrentDomain(domain)
  --if debug_canBreak() then return nil end

  monopipe.lock()
  monopipe.writeByte(MONOCMD_SETCURRENTDOMAIN)
  monopipe.writeQword(domain)

  local result=monopipe.readDword()
  monopipe.unlock()
  return result;
end

function mono_enumAssembliesOld()
  local result=nil
  --if debug_canBreak() then return nil end
  if monopipe then
    monopipe.lock()
    monopipe.writeByte(MONOCMD_ENUMASSEMBLIES)
    local count=monopipe.readDword()
    if count~=nil then
      result={}
      local i
      for i=1, count do
        result[i]=monopipe.readQword()
      end
    end

    monopipe.unlock()
  end
  return result
end



function mono_enumAssemblies()
  if 1 then return mono_enumAssembliesOld() end --please remove it only when launching the next update
  local result=nil
  --if debug_canBreak() then return nil end
  if monopipe then
    monopipe.lock()
   -- print("calling mono_enumAssemblies")
    monopipe.writeByte(MONOCMD_ENUMASSEMBLIES)
    local count=monopipe.readDword()
    
  --  printf("mono_enumAssemblies: count=%d", count)
    
    if count~=nil then
      result=monopipe.readQwords(count)      
    end

    
    monopipe.unlock()
  --  print("after mono_enumAssemblies")
  end
  return result
end

function mono_getImageFromAssembly(assembly)
  --if debug_canBreak() then return nil end
  if assembly==nil then error('mono_getImageFromAssembly: assembly is nil') end
  if assembly==0 then error('mono_getImageFromAssembly: assembly is 0') end  
  
  if monopipe==nil then return nil end
  monopipe.lock()  
  if monopipe==nil then return nil end
  
  monopipe.writeByte(MONOCMD_GETIMAGEFROMASSEMBLY)
  monopipe.writeQword(assembly)
  monopipe.unlock()
  return monopipe.readQword()
end

function mono_image_get_name(image)
  if image==nil then return nil,'invalid image' end
  --if debug_canBreak() then return nil end

  if monopipe==nil then return nil end  
  monopipe.lock()
  if monopipe==nil then return nil end
  
  monopipe.writeByte(MONOCMD_GETIMAGENAME)
  monopipe.writeQword(image)
  local namelength=monopipe.readWord()
  local name
  
  if monopipe then
    name=monopipe.readString(namelength)
    monopipe.unlock()
  end

  return name
end

function mono_image_get_filename(image)
  --if debug_canBreak() then return nil end

  if monopipe==nil then return nil end  
  monopipe.lock()
  if monopipe==nil then return nil end
  
  monopipe.writeByte(MONOCMD_GETIMAGEFILENAME)
  monopipe.writeQword(image)
  local namelength=monopipe.readWord()
  local name=monopipe.readString(namelength)

  monopipe.unlock()
  return name
end



function mono_isValidName(str)
  local r=string.find(str, "[^%a%d_.]", 1)
  return (r==nil) or (r>=5)
end

function mono_isValidName(str)
  if str then
    local r=string.find(str, "[^%a%d_.]", 1)
    return (r==nil) or (r>=5)
  else
    return false
  end
end

function mono_image_enumClasses_il2cppfallback(image)
  --all classes have the image as first field.
  --Classes are aligned on a 16 byte boundary
  --offset 0x10 of the class has a pointer to the string

  --first find all possible classes for this image (can contain a few wrong ones)
  local ms=createMemScan()
  local scantype=vtDword
  local pointersize=4
  if targetIs64Bit() then
    scantype=vtQword
    pointersize=8
  end

  ms.firstScan(soExactValue,scantype,rtRounded,string.format('%x',image),'', 0,0x7ffffffffffffffff, '', fsmAligned, "10",true, true,false,false)
  ms.waitTillDone()

  local fl=createFoundList(ms)
  fl.initialize()

  local result={}
  for i=0,fl.Count-1 do
    local e={}
    e.class=tonumber('0x'..fl[i])
    e.classname=readString(readPointer(e.class+pointersize*2),200)
    e.namespace=readString(readPointer(e.class+pointersize*3),200)
    if (e.classname==nil) or (e.classname=='') or (mono_isValidName(e.classname)==false) then e=nil end
    if e and ((e.namespace~='') and (mono_isValidName(e.namespace)==false)) then e=nil end

    if e then
      table.insert(result,e)
    end
  end

  fl.destroy()
  ms.destroy()

  return result
end


function mono_image_enumClasses(image)
  --if debug_canBreak() then return nil end
  if monopipe==nil then return nil end

  monopipe.lock()
  monopipe.writeByte(MONOCMD_ENUMCLASSESINIMAGE)
  monopipe.writeQword(image)
  local classcount=monopipe.readDword()
  if (classcount==nil) or (classcount==0) then
    if monopipe then
      monopipe.unlock()
    end
    
    if monopipe.IL2CPP then
      return mono_image_enumClasses_il2cppfallback(image)
    end
    
    return nil
  end

  local classes={}
  local i,j
  j=1
  for i=1, classcount do
    local c=monopipe.readQword()

    if (c==nil) then break end

    if (c~=0) then
      classes[j]={}
      classes[j].class=c 
      local classnamelength=monopipe.readWord()
      if classnamelength>0 then
        local n=monopipe.readString(classnamelength)
        classes[j].classname=n
      else
        classes[j].classname=''
      end

      local namespacelength=monopipe.readWord()
      if namespacelength==nil then break end
      
      if namespacelength>0 then
        classes[j].namespace=monopipe.readString(namespacelength)
      else
        classes[j].namespace=''
      end
      j=j+1
    end
    
  end

  if monopipe then
    monopipe.unlock()
  end

  return classes;
end

function mono_class_isgeneric(class)
  if class==nil then
    print("mono_class_isgeneric with null pointer: ")
    print(debug.traceback())
    return nil
  end
  local result=''
  monopipe.lock()
  monopipe.writeByte(MONOCMD_ISCLASSGENERIC)
  monopipe.writeQword(class)

  result=monopipe.readByte()~=0 

  monopipe.unlock()
  return result;
end

function mono_class_isEnum(klass)
 if not klass or klass==0 then return false end
 monopipe.lock()
 monopipe.writeByte(MONOCMD_ISCLASSENUM)
 monopipe.writeQword(klass)
 local retv = monopipe.readByte()
 monopipe.unlock()
 return retv==1
end
local function mono_classFindMethodByParameterCount(kls,mthdName,prmCount,prmName)
  for k,v in pairs(mono_class_enumMethods(kls,1)) do
    if v.name==mthdName then
      local prms = mono_method_get_parameters(v.method)
      if prmCount then
        if #prms.parameters==prmCount then
          if prmName then
             for kk,vv in pairs(prms.parameters) do
               if vv.name==prmName then return v end
             end
          else
            return v
          end
        end
      else
        return v
      end
    end
  end
end

function mono_class_IsPrimitive(klass)
  local tp = mono_class_get_type(klass)
  local rtp = mono_classtype_get_reflectiontype(tp)
  if not rtp then LaunchMonoDataCollector(); error('Reflection type not found') end
  local kls = mono_object_getClass(rtp)
  local mtd = mono_classFindMethodByParameterCount(kls,'get_IsPrimitive')
  assert(mtd and mtd.method~=0,'Error: method "Type.get_IsPrimitive" was nil')
  mtd = mtd.method
  local v = mono_invoke_method(nil,mtd,rtp,{})
  return v==1
end

function mono_class_isValueType(klass)
 if not klass or klass==0 then return false end
 monopipe.lock()
 monopipe.writeByte(MONOCMD_ISCLASSVALUETYPE)
 monopipe.writeQword(klass)
 local retv = monopipe.readByte()
 monopipe.unlock()
 return retv==1
end

function mono_class_isStruct(klass)
 return mono_class_isValueType(klass) and not(mono_class_isEnum(klass)) and not(mono_class_IsPrimitive(klass))
end

function mono_class_isSubClassOf(klass,parentklass,checkInterfaces)
 checkInterfaces = checkInterfaces and 1 or 0
 if not klass or klass==0 then return false end
 monopipe.lock()
 monopipe.writeByte(MONOCMD_ISCLASSISSUBCLASSOF)
 monopipe.writeQword(klass)
 monopipe.writeQword(parentklass)
 monopipe.writeByte(checkInterfaces)
 local retv = monopipe.readByte()
 monopipe.unlock()
 return retv==1
end

function mono_isil2cpp(class)
  local result=false
  monopipe.lock()
  monopipe.writeByte(MONOCMD_ISIL2CPP)
  result=monopipe.readByte()==1

  monopipe.unlock()
  return result
end

function mono_class_getNestedTypes(class)
  local result={}
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETCLASSNESTEDTYPES)  
  monopipe.writeQword(class)
  local count=monopipe.readDword()
 
  for i=1,count do
    result[i]=monopipe.readQword()
  end

  monopipe.unlock()
  
  return result
  
end


function mono_class_getNestingType(class)
  --returns the parent class if nested. 0 if not nested
  
  if (class==nil) or (class==0) then 
    print("mono_class_getNestingType received an invalid class")
    print(debug.traceback())
    return nil
  end
  
  local result
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETCLASSNESTINGTYPE)  
  monopipe.writeQword(class)
  result=monopipe.readQword()
  if monopipe==nil then
    print("mono_class_getNestingType crashed:")
    print(debug.traceback())
    return nil
  end
  monopipe.unlock()
  
  return result
end

function mono_class_getName(class)
  --if debug_canBreak() then return nil end\
  if (class==nil) or (class==0) then     
    return ''
  end

  local result=''
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETCLASSNAME)
  monopipe.writeQword(class)

  local namelength=monopipe.readWord();
  if monopipe then  
    result=monopipe.readString(namelength);

    monopipe.unlock()
  end
  return result
end


function mono_class_getNamespace(class)
  --if debug_canBreak() then return nil end
  if (class==nil) or (class==0) then     
    return ''
  end  

  local result=''
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETCLASSNAMESPACE)
  monopipe.writeQword(class)

  local namelength=monopipe.readWord();
  result=monopipe.readString(namelength);

  monopipe.unlock()
  return result;
end


function mono_class_getFullName(typeptr, isclass, nameformat)
  --if debug_canBreak() then return nil end
  if isclass==nil then isclass=1 end
  if nameformat==nil then nameformat=MONO_TYPE_NAME_FORMAT_REFLECTION end

  --print("mono_class_getFullName");
  local result=''
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETFULLTYPENAME)
  monopipe.writeQword(typeptr)
  monopipe.writeByte(isclass)
  monopipe.writeDword(nameformat)

  local namelength=monopipe.readWord();
  result=monopipe.readString(namelength);

  monopipe.unlock()
  return result;
end


function mono_class_getParent(class)
  --if debug_canBreak() then return nil end

  local result=0
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETPARENTCLASS)
  monopipe.writeQword(class)  

  result=monopipe.readQword()

  monopipe.unlock()
  return result;
end

function mono_class_getImage(class)
  local result=0
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETCLASSIMAGE)
  monopipe.writeQword(class)  
  result=monopipe.readQword()
  monopipe.unlock()
  return result;   
end

function mono_ptr_class_get(fieldtype_or_ptrtype)
--returns the MonoType* object which is a pointer to the given type. Use "mono_class_getFullName" on the returned value to see the difference.
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETPTRTYPECLASS)
  monopipe.writeQword(fieldtype_or_ptrtype)
  local val = monopipe.readQword()
  monopipe.unlock()
  return val
end

function mono_field_get_type(monofield)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETFIELDTYPE)
  monopipe.writeQword(monofield)
  local val = monopipe.readQword()
  monopipe.unlock()
  return val
end

function mono_type_get_ptr_type(ptrtype)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETTYPEPTRTYPE)
  monopipe.writeQword(ptrtype)
  local val = monopipe.readQword()
  monopipe.unlock()
  return val
end

function mono_field_getClass(field)
  --if debug_canBreak() then return nil end

  local result=0
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETFIELDCLASS)
  monopipe.writeQword(field)  

  result=monopipe.readQword()

  monopipe.unlock()
  return result;
end

function mono_type_getClass(field)
  --ce <7.5.2
  return mono_field_getClass(field)
end

function mono_class_get_type(kls)
  if not kls or kls==0 then return nil end
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETCLASSTYPE)
  monopipe.writeQword(kls)
  local retv = monopipe.readQword()
  monopipe.unlock()
 return retv
end

function mono_type_get_class(monotype)
  if not monotype or monotype==0 then return nil end
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETCLASSOFTYPE)
  monopipe.writeQword(monotype)
  local retv = monopipe.readQword()
  monopipe.unlock()
 return retv
end

function mono_type_get_type(monotype)
  if not monotype or monotype==0 then return nil end
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETTYPEOFMONOTYPE)
  monopipe.writeQword(monotype)
  local retv = monopipe.readDword()
  monopipe.unlock()
 return retv
end

function mono_type_is_byref(monotype)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_TYPEISBYREF)
  monopipe.writeQword(monotype)
  local val = monopipe.readByte()
  monopipe.unlock()
  return val == 1
end

function mono_classtype_get_reflectiontype(monotype)
  if not monotype or monotype==0 then return end
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETREFLECTIONTYPEOFCLASSTYPE)
  monopipe.writeQword(monotype)
  local retv = monopipe.readQword()
  monopipe.unlock()
  return retv
end

function mono_method_get_reflectiontype(method,klass)
  assert(method,'Error: "method" was nil. It is supposed to be a MonoMethod*')
  assert(klass,'Error: "klass" was nil. It is supposed to be a MonoClass*')
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETREFLECTIONMETHODOFMONOMETHOD)
  monopipe.writeQword(method)
  monopipe.writeQword(klass)
  local retv = monopipe.readQword()
  monopipe.unlock()
  return retv
end

function mono_object_unbox(monoobject)
  assert(monoobject,'Error: "monoobject" was nil. It is supposed to be a MonoObject*')
  monopipe.lock()
  monopipe.writeByte(MONOCMD_MONOOBJECTUNBOX)
  monopipe.writeQword(monoobject)
  local retv = monopipe.readQword()
  monopipe.unlock()
  return retv
end

function mono_class_getArrayElementClass(klass)
  --if debug_canBreak() then return nil end

  local result=0
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETARRAYELEMENTCLASS)
  monopipe.writeQword(klass)

  result=monopipe.readQword()

  monopipe.unlock()
  return result;
end

function mono_class_getVTable(domain, klass)
  --if debug_canBreak() then return nil end
  if domain and klass==nil then
    klass=domain
    domain=nil
  end
  
  if not monopipe then return nil end
  
  if monopipe.IL2CPP then
    return klass
  end
  
  if klass==nil then
    return nil,"No class provided"
  end
  
  
  local result=0
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETVTABLEFROMCLASS)
  monopipe.writeQword(domain)
  monopipe.writeQword(klass)
  
  result=monopipe.readQword()
  if monopipe then
    monopipe.unlock()
  end
  return result  
end

local function GetInstancesOfClass(kls)
  if getOperatingSystem()==0 then
    local reskls = mono_findClass("UnityEngine","Resources")
    local mthds = mono_class_enumMethods(reskls)
    local fn
    for k,v in pairs(mthds) do
      if v.name == 'FindObjectsOfTypeAll' then
        local prms = mono_method_get_parameters(v.method)
        if #prms.parameters == 1 and prms.parameters[1].name=="type" then fn = v.method break end
      end
    end
    if not fn then
      reskls = mono_findClass("UnityEngine","Object")
      mthds = mono_class_enumMethods(reskls)
      for k,v in pairs(mthds) do
        if v.name == 'FindObjectsOfType' then
          local prms = mono_method_get_parameters(v.method)
          if #prms.parameters == 1 and prms.parameters[1].name=="type" then fn = v.method break end
        end
      end
      if not fn then return end
    end
    local sig = mono_method_getSignature(fn)
    local klstype = mono_class_get_type(kls)
    local reftype = mono_classtype_get_reflectiontype(klstype)
    if not reftype or reftype==0 then return end
    return mono_invoke_method(nil,fn,0,{{type=vtPointer,value=reftype}})
  end

end


--todo for the instance scanner: Get the fields and check that pointers are either nil or point to a valid address
function mono_class_findInstancesOfClassListOnly(domain, klass, progressBar)
  local inst = GetInstancesOfClass(klass)
  if inst and readPointer(inst) and readPointer(inst)~=0 then
     local countoff =  targetIs64Bit() and 0x18 or 0xC
     local elementsoff = targetIs64Bit() and 0x20 or 0x10
     local elesize = targetIs64Bit() and 8 or 4
     local arr = inst--readPointer(inst)
     local count =readInteger(arr+countoff)
     local result = {}
     for i=0,count-1 do
      result[#result+1] = readPointer(inst+i*elesize+elementsoff)
     end
     return result
  end
  
  if debugInstanceLookup then 
    if progressBar then
      printf("progressBar is set. progressBar.ClassName=%s", progressBar.ClassName)
    end
  
    print("mono_class_findInstancesOfClassListOnly")     
  end

  local vtable=mono_class_getVTable(domain, klass)
  if debugInstanceLookup then 
    if vtable then
      printf("vtable is %x", vtable)
    else
      print("vtable is nil") 
    end
  end

  
  if (vtable) and (vtable~=0) then
    local ms=createMemScan(progressBar)  
    local scantype=vtDword
    if targetIs64Bit() then
      scantype=vtQword
    end
    
    ms.firstScan(soExactValue,scantype,rtRounded,string.format('%x',vtable),'', 0,0x7ffffffffffffffff, '', fsmAligned, "8",true, true,false,false)

    ms.waitTillDone() 
    if debugInstanceLookup then     
      print("after ms.waitTillDone")
    end
    
    local fl=createFoundList(ms)
    fl.initialize()
    
    local result={}
    local i
    for i=0,fl.Count-1 do
      result[i+1]=tonumber('0x'..fl[i])
    end
    
    if debugInstanceLookup then print("Destroying fl and ms") end
    
    fl.destroy()    
    ms.destroy()  
    if debugInstanceLookup then 
      printf("end of mono_class_findInstancesOfClassListOnly with valid vtable. #result=%d", #result)    
    end
    
    return result
  end
end


function mono_class_findInstancesOfClass(domain, klass, OnScanDone, ProgressBar)
  --find all instances of this class
  
  --get the fields of this class and get their value
  local struct=createStructure(mono_class_getFullName(klass))
  monoform_exportStructInternal(struct, klass, true, false, smap, false)
  
  local pointeroffsets={}
  
  local i
  for i=0,struct.Count-1 do
    if struct.Element[i].Vartype==vtPointer then 
      table.insert(pointeroffsets,struct.Element[i].Offset)
    end
  end
  
  
  
  local vtable=mono_class_getVTable(domain, klass)
  if (vtable) and (vtable~=0) then
    --do a memory scan for this vtable, align on ending with 8/0 (fastscan 8) (64-bit can probably do fastscan 10)    
    
    local ms
    
    
    
    if OnScanDone~=nil then
      ms=createMemScan(ProgressBar)      
      ms.OnScanDone=OnScanDone
    else
      ms=createMemScan(MainForm.Progressbar)  
      ms.OnScanDone=function(m)
        local fl=createFoundList(m)
        MainForm.Progressbar.Position=0

        fl.initialize()

        local r=createForm(false)
        r.caption=translate('Instances of ')..mono_class_getName(klass)

        local tv=createTreeView(r)
        tv.ReadOnly=true
        tv.MultiSelect=true
        
        
        local w=createLabel(r)
        w.Caption=translate('Warning: These are just guesses. Validate them yourself')
        w.Align=alTop
        tv.align=alClient
        tv.OnDblClick=function(sender)
          local n=sender.Selected
          
          if n then
            local entrynr=n.Index;
            local offset=struct.Element[entrynr].Offset
            
            while n.level>0 do --get the parent~
              n=n.parent
            end            
            getMemoryViewForm().HexadecimalView.Address=tonumber('0x'..n.text)+offset
            getMemoryViewForm().Show()
          end         
        end
        
        local pm=createPopupMenu(r)
        local miCopyToClipboard=createMenuItem(pm)
        miCopyToClipboard.Caption='Copy selection to clipboard'
        miCopyToClipboard.Shortcut='Ctrl+C'
        miCopyToClipboard.OnClick=function(m)
          local i
          local sl=createStringlist()
          for i=0,tv.Items.Count-1 do
            if tv.Items[i].Selected then 
              sl.add(tv.Items[i].Text)
            end
          end  

          if (sl.Count>0) then
            writeToClipboard(sl.Text) 
          end
          
          sl.destroy();          
        end     
        pm.Items.add(miCopyToClipboard)
        
        
        local miRescan=createMenuItem(pm)
        miRescan.Caption='Rescan'
        miRescan.Shortcut='Ctrl+R'
        miRescan.OnClick=function(m)
          --don't do this too often...
          --print("Rescan")          
          mono_class_findInstancesOfClass(domain, klass, OnScanDone, ProgressBar)
          r.close()
        end
        
        pm.Items.add(miRescan)
        
      
        
        local miDissectStruct=createMenuItem(pm)
        miDissectStruct.Caption='Dissect struct'
        miDissectStruct.Shortcut='Ctrl+D'
        miDissectStruct.OnClick=function(m)
          --
          local n=tv.Selected
          
          if n then
            while n.level>0 do --get the parent
              n=n.parent
            end
            
            local address=tonumber('0x'..n.text)
            
            local s=monoform_exportStruct(address, nil,false, true, smap, true, false)
            monoform_miDissectShowStruct(s, address)             
          end          
        end        
        pm.Items.add(miDissectStruct)
        
        
        local miInvokeMethod=createMenuItem(pm)
        miInvokeMethod.Caption='Invoke method of class'
        miInvokeMethod.Shortcut='Ctrl+I'
        miInvokeMethod.OnClick=function(m)        
          --show the methodlist
          local n=tv.Selected
          
          if n then
            while n.level>0 do --get the parent
              n=n.parent
            end
            
            local address=tonumber('0x'..n.text)            
          
            local list=createStringlist()
            local m=mono_class_enumMethods(klass, true)
            local i
            for i=1,#m do
              list.add(m[i].name)
            end
            
            i=showSelectionList('Invoke Method of Instance','Select a method to execute',list)
            list.destroy()
            
            if i==-1 then return end
            
            mono_invoke_method_dialog(nil, m[i+1].method, address)       
          end
        end
          
        pm.Items.add(miInvokeMethod)
        
        
        tv.PopupMenu=pm
        

        r.OnClose=function(f)             
          return caFree
        end

        r.OnDestroy=function(f)
          if struct then
            struct.destroy()
            struct=nil
          end
          tv.OnDblClick=nil    
          
          if miCopyToClipboard then
            miCopyToClipboard.OnClick=nil
          end
          
          if miRescan then
            miRescan.OnClick=nil
          end
          
          if miDissectStruct then
            miDissectStruct.OnClick=nil
          end
          
        end

        local i
        for i=0, fl.Count-1 do
          --check if the address is valid
          local address=tonumber('0x'..fl[i])
          local j
          local valid=true
          for j=1,#pointeroffsets do 
            local v=readPointer(address+pointeroffsets[j])
            
            if (v==nil) then
              valid=false
            else
              if (v~=0) then  --0 is valid (nil)
                v=readBytes(v,1)
                valid=v~=nil;
              end
            end
          end
        
          if valid then
            local tn=tv.Items.Add(fl[i])          
            tn.hasChildren=true
          end
        end
        
        tv.OnExpanding=function(sender, node)                 
          --delete all children if it has any and then fill them in again
          local address=tonumber('0x'..node.Text)
          
          node.deleteChildren()
          local i
          if struct then
            for i=0, struct.Count-1 do
              --add to this node
              local e=struct.Element[i]
              node.add(e.Name..' - '..e.getValueFromBase(address))
            end   
            return true            
          else
            return false
          end
          
          
        end

        r.position=poScreenCenter
        r.borderStyle=bsSizeable
        r.show()

        fl.destroy()
        m.destroy()
      end
    end
    
    local scantype=vtDword
    if targetIs64Bit() then
      scantype=vtQword
    end
    
    ms.firstScan(soExactValue,scantype,rtRounded,string.format('%x',vtable),'', 0,0x7ffffffffffffffff, '', fsmAligned, "8",true, true,false,false)
  end
  
end




function mono_class_getStaticFieldAddress(domain, class)
  --if debug_canBreak() then return nil end
  if not monopipe then return nil end
  

  if (class==nil)  and domain then
    class=domain
    domain=0
  end
  
  local result=0
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETSTATICFIELDADDRESSFROMCLASS)
  monopipe.writeQword(domain)  
  monopipe.writeQword(class)  

  result=monopipe.readQword()
  
  if monopipe then
    monopipe.unlock()
  end
  return result;
end

function mono_object_enumValues(object)
--same as mono_class_enumFields but each field has a   a list of fields of the class that belongs to the class, and their value
  local r={}
  local c=mono_object_getClass(object)
  if c then
    local fields=mono_class_enumFields(c)
    if fields then
      local i
      for i=1,#fields do
        if not (fields[i].isStatic or fields[i].isConst) then
          local reader=getDotNetValueReader(fields[i].monotype)
          if reader then
            local address=object+fields[i].offset
            r[fields[i].name]=reader(address)
          end          
        end      
      end 
      return r  
    end
  end 
 
end

function mono_class_enumFields(class, includeParents, expandedStructs)
  local function GetFields(class, includeParents, expandedStructs, staticnoinclude)
    local classfield;
    local index=1;
    local fields={}

    if monopipe==nil then return fields end


    if includeParents then
      local parent=mono_class_getParent(class)
      if (parent) and (parent~=0) then
        fields=GetFields(parent, includeParents, expandedStructs);
        index=#fields+1;
      end
    end


    monopipe.lock()


    --mono_class_getParent

    monopipe.writeByte(MONOCMD_ENUMFIELDSINCLASS)
    monopipe.writeQword(class)

    repeat
      classfield=monopipe.readQword()
      if (classfield~=nil) and (classfield~=0) then
        local namelength;
        fields[index]={}
        fields[index].field=classfield
        fields[index].type=monopipe.readQword()
        fields[index].monotype=monopipe.readDword()

        fields[index].parent=monopipe.readQword()
        fields[index].offset=monopipe.readDword()
        fields[index].flags=monopipe.readDword()

        fields[index].isStatic=(bAnd(fields[index].flags, bOr(FIELD_ATTRIBUTE_STATIC, FIELD_ATTRIBUTE_HAS_FIELD_RVA))) ~= 0 --check mono for other fields you'd like to test
        fields[index].isConst=(bAnd(fields[index].flags, FIELD_ATTRIBUTE_LITERAL)) ~= 0

        namelength=monopipe.readWord();
        fields[index].name=monopipe.readString(namelength);

        namelength=monopipe.readWord();
        fields[index].typename=monopipe.readString(namelength);
        if (staticnoinclude and fields[index].isStatic) then
          fields[index] = nil
        else
          index=index+1
        end
        
      end

    until (classfield==nil) or (classfield==0)

    if monopipe then
      monopipe.unlock()
    end

    return fields
  end
  local mainFields = GetFields(class, includeParents, expandedStructs)
  if expandedStructs then
    for k,v in pairs(mainFields) do
      local lockls = mono_field_getClass(v.field)
      if not(v.isStatic or v.isConst) and mono_class_isStruct(lockls) and not(mono_class_isSubClassOf(lockls,class)) then --does not want to infinitely loop if the struct has some static member of the same class
         local subFields = GetFields(lockls, includeParents, expandedStructs, true)
         --print(v.name, v.typename, fu(v.monotype))
         if #subFields >0 then
            if subFields[1].offset == 0x10 then  --Not sure if also in 32 bit...
               for kk,vv in pairs(subFields) do
                   vv.offset = vv.offset-0x10+v.offset
               end
            end
            subFields[1].name = mainFields[k].name..'.'..subFields[1].name
            for i=2, #subFields do
                subFields[i].name = mainFields[k].name..'.'..subFields[i].name
                mainFields[#mainFields+1] = subFields[i]
            end
            mainFields[k] = subFields[1]
         end
      end
    end
  end

  return mainFields
end

function mono_class_enumMethods(class, includeParents)
  --if debug_canBreak() then return nil end
  
  --print("mono_class_enumMethods")

  local method
  local index=1
  local methods={}

  if includeParents then
    --print("includeParents")
    local parent=mono_class_getParent(class)
    if (parent) and (parent~=0) then
      methods=mono_class_enumMethods(parent, includeParents);
      index=#methods+1;
      
     -- print("#methods="..#methods)
      
      local i
      for i=1,#methods do
        if methods[i].parent==nil then
          methods[i].parent=parent
        end
      end
    end
  end
  
  
  
  monopipe.lock()

  monopipe.writeByte(MONOCMD_ENUMMETHODSINCLASS)
  monopipe.writeQword(class)

  repeat
    method=monopipe.readQword()
    if (method~=nil) and (method~=0) then
      local namelength;
      methods[index]={}
      methods[index].method=method
      namelength=monopipe.readWord()
      methods[index].name=monopipe.readString(namelength)
      methods[index].flags=monopipe.readDword()
      index=index+1
    end

  until (method==nil) or (method==0)

  monopipe.unlock()
  
  local temp={}
  local i
  for i=1,#methods do
    temp[i]={methods[i].name, methods[i]}
  end
  table.sort(temp, function(e1,e2) return e1[1] < e2[1] end)
  
  methods={}
  for i=1,#temp do
    methods[i]=temp[i][2]
  end  

  return methods
end

function mono_class_enumInterfaces(MonoClass)
  if not MonoClass or MonoClass==0 then return {} end
  monopipe.lock()
  monopipe.writeByte(MONOCMD_ENUMINTERFACESOFCLASS)
  monopipe.writeQword(MonoClass)
  local retv = {}
  local klass
  repeat
    klass = monopipe.readQword()
    retv[#retv+1] = (klass and klass~=0) and klass or nil
  until(not klass or klass==0)
  monopipe.unlock()
  return retv
end

function mono_getJitInfo(address)
  --if debug_canBreak() then return nil end
  
  if monopipe==nil then return nil end

  local d=mono_enumDomains()
  if (d~=nil) then
    monopipe.lock()

    for i=1, #d do
      monopipe.writeByte(MONOCMD_GETJITINFO)
      monopipe.writeQword(d[i])
      monopipe.writeQword(address)

      local jitinfo=monopipe.readQword()

      if (jitinfo~=nil) and (jitinfo~=0) then
        local result={}
        result.jitinfo=jitinfo;
        result.method=monopipe.readQword();
        result.code_start=monopipe.readQword();
        result.code_size=monopipe.readDword();

        monopipe.unlock() --found something
        return result
      end

    end

    monopipe.unlock()
  end
  return nil
end

function mono_getStaticFieldValue(vtable, field)
  if not monopipe then return nil end
  
  local r
  monopipe.lock()
  if monopipe then
    monopipe.writeByte(MONOCMD_GETSTATICFIELDVALUE)
    monopipe.writeQword(vtable)
    monopipe.writeQword(field)
    r=monopipe.readQword()
    
    if monopipe then
      monopipe.unlock() 
    end
  end

  return r    
end

function mono_setStaticFieldValue(vtable, field, value)
  local r
  monopipe.lock()
  monopipe.writeByte(MONOCMD_SETSTATICFIELDVALUE)
  monopipe.writeQword(vtable)
  monopipe.writeQword(field)
  monopipe.writeQword(value)
  monopipe.unlock() 
  
  return r    
end


function mono_class_getStaticFieldValue(class, field)
  local vtable=mono_class_getVTable(0,class)
  return mono_getStaticFieldValue(vtable, field)
end

function mono_class_setStaticFieldValue(class, field, value)
  local vtable=mono_class_getVTable(0,class)
  return mono_setStaticFieldValue(vtable, field, value)
end




function mono_object_findRealStartOfObject(address, maxsize)
  --if debug_canBreak() then return nil end

  if maxsize==nil then
    maxsize=4096
  end

  if address==nil then
    error(translate("address==nil"))
  end

  local currentaddress=bAnd(address, 0xfffffffffffffffc)


  while (currentaddress>address-maxsize) do
    if monopipe==nil then return nil end
    
    local classaddress,classname=mono_object_getClass(currentaddress)

    if (classaddress~=nil) and (classname~=nil) then
      classname=classname:match "^%s*(.-)%s*$" --trim
      if (classname~='') then
        local r=string.find(classname, "[^%a%d_.]", 1)  --scan for characters that are not decimal or characters, or have a _ or . in the name


        if (r==nil) or (r>=5) then
          return currentaddress, classaddress, classname --good enough
        end
      end
    end

    currentaddress=currentaddress-4
  end

  --still here
  return nil

end



--function mono_findReferencesToObject(class) --scan the memory for objects with a vtable to a specific class
--end

function mono_image_findClass(image, namespace, classname)
  --if debug_canBreak() then return nil end

--find a class in a specific image
  if monopipe==nil then return 0 end

  monopipe.lock()

  monopipe.writeByte(MONOCMD_FINDCLASS)
  monopipe.writeQword(image)
  monopipe.writeWord(#classname)
  monopipe.writeString(classname)
  if (namespace~=nil) then
    monopipe.writeWord(#namespace)
    monopipe.writeString(namespace)
  else
    monopipe.writeWord(0)
  end

  result=monopipe.readQword()
  monopipe.unlock()
  
  return result
end

function mono_image_findClassSlow(image, namespace, classname)
  --if debug_canBreak() then return nil end

--find a class in a specific image
  local result=0

  if monopipe==nil then return 0 end

  monopipe.lock()

  local fullnamerequested=classname:find("+") ~= nil

  local c=mono_image_enumClasses(image)
  if c then
    local i
    for i=1, #c do
      --check that classname is in c[i].classname
      if fullnamerequested then
        local cname=mono_class_getFullName(c[i].class)
        local r=mono_splitSymbol(cname)

        if r.methodname==classname then --methodname is classname in this context
          result=c[i].class
          break
        end
      else
        if c[i].classname==classname then
          result=c[i].class
          break;
        end
      end
    end

  end
  monopipe.unlock()

  return result
end

function mono_splitClassAndNestedTypeNames(classname)
  --takes a clasname formatted as xxxx+yyyy+zzzz and splits it into xxxx and yyyy
end

function mono_findClass(namespace, classname)
  --if debug_canBreak() then return nil end

--searches all images for a specific class
 -- print(string.format("mono_findClass: namespace=%s classname=%s", namespace, classname))
  
  local i
  if namespace and classname==nil then  --user forgot namespace    
    classname=namespace
    namespace=''
  end
  
  if namespace==nil or classname==nil then
    return nil,'invalid parameters'
  end
  
  if monocache and (monocache.processid==getOpenedProcessID()) and monocache.nonfoundclasses and monocache.nonfoundclasses[namespace..'.'..classname] then
    return nil
  end
  

  local ass=mono_enumAssemblies()
  local result
  
  if ass==nil then return nil end

  local fullnamerequested=classname:find("+") ~= nil
  
  if fullnamerequested then
    --there's a nested type specified
  end
  
  if fullnamerequested==false then
    for i=1, #ass do
      result=mono_image_findClass(mono_getImageFromAssembly(ass[i]), namespace, classname)
      if (result~=0) then
        return result;
      end
    end
  end

  --still here:

  for i=1, #ass do
    result=mono_image_findClassSlow(mono_getImageFromAssembly(ass[i]), namespace, classname)
    if (result~=0) then
      return result;
    end
  end  
  
  --not found
  if monocache==nil then mono_clearcache() end  
  if monocache.nonfoundclasses==nil then
    monocache.nonfoundclasses={}
  end
  monocache.nonfoundclasses[namespace..'.'..classname]=true
  return nil
end

function mono_class_findMethod(class, methodname)
  --if debug_canBreak() then return nil end

  if methodname==nil then return nil end
  if monopipe==nil then return nil end

  monopipe.lock()
  monopipe.writeByte(MONOCMD_FINDMETHOD)
  monopipe.writeQword(class)

  monopipe.writeWord(#methodname)
  monopipe.writeString(methodname)

  local result=monopipe.readQword()


  monopipe.unlock()

  return result
end

function mono_findMethod(namespace, classname, methodname)
  --if debug_canBreak() then return nil end
  if methodname==nil then
    if namespace and classname then
      methodname=classname
      classname=namespace
      namespace=''
    end  
  end
  
  if namespace==nil or classname==nil or methodname==nil then
    return nil,'invalid parameters'
  end

  local class=mono_findClass(namespace, classname)
  local result=0
  if class and (class~=0) then
    result=mono_class_findMethod(class, methodname)
  end

  return result
end


function mono_image_findMethodByDesc(image, methoddesc)
  --if debug_canBreak() then return nil end

  if image==nil then return 0 end
  if methoddesc==nil then return 0 end

  monopipe.lock()
  monopipe.writeByte(MONOCMD_FINDMETHODBYDESC)
  monopipe.writeQword(image)

  monopipe.writeWord(#methoddesc)
  monopipe.writeString(methoddesc)

  local result=monopipe.readQword()

  monopipe.unlock()

  return result
end
mono_class_findMethodByDesc=mono_image_findMethodByDesc --for old scripts that use this when it was wrongly named


function mono_findMethodByDesc(assemblyname, methoddesc)
  --if debug_canBreak() then return nil end
  if assemblyname==nil then return nil,'assemblyname is nil' end  
  if methoddesc==nil then return nil,'methoddesc is nil' end

  local assemblies = mono_enumAssemblies()
  if assemblies==nil then return nil, 'no assemblies' end
  local i

  for i=1, #assemblies do
    local image = mono_getImageFromAssembly(assemblies[i])
    local imagename = mono_image_get_name(image)
    if imagename == assemblyname then
      return mono_image_findMethodByDesc(image, methoddesc)  
    end      
  end
  
  --still here, try case insensitive assembly names
  assemblyname=assemblyname:lower()
  for i=1, #assemblies do
    local image = mono_getImageFromAssembly(assemblies[i])
    local imagename = mono_image_get_name(image):lower()
    if imagename == assemblyname then
      return mono_image_findMethodByDesc(image, methoddesc)  
    end      
  end  
  return nil
  
end



--idea for the future:
--function mono_invokeMethod()
--  print("Not yet implemented")
--end

function mono_method_getName(method)
  --if debug_canBreak() then return nil end

  local result=''
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMETHODNAME)
  monopipe.writeQword(method)

  local namelength=monopipe.readWord();
  result=monopipe.readString(namelength);

  monopipe.unlock()
  return result;
end

function mono_method_getFullName(monomethod)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMETHODFULLNAME)
  monopipe.writeQword(monomethod)
  local namelength=monopipe.readWord()
  local result=monopipe.readString(namelength)
  monopipe.unlock()
  return result or ''
end


function mono_method_getHeader(method)
  --if debug_canBreak() then return nil end
  if method==nil then return nil end

  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMETHODHEADER)
  monopipe.writeQword(method)
  local result=monopipe.readQword()

  monopipe.unlock()

  return result;
end

function mono_method_get_parameters(method)
--like mono_method_getSignature but returns it in a more raw format (no need to string parse)
  --if debug_canBreak() then return nil end
  if monopipe==nil then return nil end
  
  if method==nil then return nil end
  local result={}
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMETHODPARAMETERS)
  monopipe.writeQword(method)  
  
  local paramcount=monopipe.readByte()
  if paramcount==nil then return nil end
  
  local i
  
  result.parameters={}
  
  --names
  for i=1, paramcount do  
    local namelength=monopipe.readByte()
    
    if namelength==nil then return nil end
    
    result.parameters[i]={}
    
    if namelength>0 then
      result.parameters[i].name=monopipe.readString(namelength)
    else
      result.parameters[i].name='param '..i
    end
  end
  
  --types
  for i=1, paramcount do  
    result.parameters[i].monotype=monopipe.readQword();
    result.parameters[i].type=monopipe.readDword(); 
  end
  
  --result  
  result.returnmonotype = monopipe.readQword()
  result.returntype=monopipe.readDword()  
  
  monopipe.unlock()
  return result  
end

function mono_method_getSignature(method)
--Gets the method 'signature', the corresponding parameter names, and the returntype
  --if debug_canBreak() then return nil end
  
  if method==nil then return nil end
  if monopipe==nil then return nil end

  local result=''
  local parameternames={}
  local returntype=''
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMETHODSIGNATURE)
  monopipe.writeQword(method)

  local paramcount=monopipe.readByte()
  if paramcount==nil then return nil end --invalid method (monopipe is likely dead now)
  
  local i
  
  for i=1, paramcount do
    local namelength=monopipe.readByte()
    if namelength>0 then
      parameternames[i]=monopipe.readString(namelength)
    else
      parameternames[i]='param'..i
    end
  end
  
  if monopipe.IL2CPP then
    result=''
    
    for i=1,paramcount do
      local typenamelength=monopipe.readWord()
      local typename
      if typenamelength>0 then
        typename=monopipe.readString(typenamelength)
      else
        typename='<undefined>'
      end  

      result=result..typename
      if i<paramcount then
        result=result..','
      end      
    end  
    
   
    
    --build a string with these typenames
  else
    local resultlength=monopipe.readWord();
    result=monopipe.readString(resultlength);
  end

  local returntypelength=monopipe.readByte()
  returntype=monopipe.readString(returntypelength)  
  

  monopipe.unlock()
  return result, parameternames, returntype;
end

function mono_method_disassemble(method)
  --if debug_canBreak() then return nil end

  local result=''
  monopipe.lock()
  monopipe.writeByte(MONOCMD_DISASSEMBLE)
  monopipe.writeQword(method)

  local resultlength=monopipe.readWord();
  result=monopipe.readString(resultlength);

  monopipe.unlock()
  return result;
end

function mono_method_getClass(method)
  --if debug_canBreak() then return nil end

  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMETHODCLASS)
  monopipe.writeQword(method)
  local result=monopipe.readQword()

  if monopipe then
    monopipe.unlock()
  end

  return result;
end

function mono_method_getFlags(method)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMETHODFLAGS)
  monopipe.writeQword(method)
  local result=monopipe.readDword()

  if monopipe then
    monopipe.unlock()
  end

  return result;
end


function mono_compile_method(method) --Jit a method if it wasn't jitted yet
  --if debug_canBreak() then return nil end

  --print(string.format("mono_compile_method. Method=%x", method))
  if monopipe then
    monopipe.lock()

    monopipe.writeByte(MONOCMD_COMPILEMETHOD)
    monopipe.writeQword(method)
    local result=monopipe.readQword()
    if monopipe then
      monopipe.unlock()
    end
    return result
  end
end

--note: does not work while the profiler is active (Current implementation doesn't use the profiler, so we're good to go)
function mono_free_method(method) --unjit the method. Only works on dynamic methods. (most are not)
  --if debug_canBreak() then return nil end

  monopipe.lock()

  monopipe.writeByte(MONOCMD_FREEMETHOD)
  monopipe.writeQword(method)
  monopipe.unlock()
end

--note: does not work while the profiler is active (Current implementation doesn't use the profiler, so we're good to go)
function mono_free(object) --unjit the method. Only works on dynamic methods. (most are not)
  --if debug_canBreak() then return nil end

  monopipe.lock()

  monopipe.writeByte(MONOCMD_FREE)
  monopipe.writeQword(object)
  monopipe.unlock()
end

function mono_methodheader_getILCode(methodheader)
  --if debug_canBreak() then return nil end

  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMETHODHEADER_CODE)
  monopipe.writeQword(methodheader)
  local address=monopipe.readQword()
  local size=monopipe.readDword()

  monopipe.unlock()

  return address, size;
end

function mono_getILCodeFromMethod(method)
  if monopipe.IL2CPP then return nil end
  
  local hdr=mono_method_getHeader(method)
  return mono_methodheader_getILCode(hdr)
end


function mono_image_rva_map(image, offset)
  --if debug_canBreak() then return nil end

  monopipe.lock()
  monopipe.writeByte(MONOCMD_LOOKUPRVA)
  monopipe.writeQword(image)
  monopipe.writeDword(offset)
  local address=monopipe.readQword()
  monopipe.unlock()
  return address;
end

function mono_string_readString(stringobject)
  if stringobject==nil then
    return nil,'invalid parameter'
  end

  local length,stringstart
  --printf("mono_string_readString(%x)",stringobject)
  if targetIs64Bit() then
    length=readInteger(stringobject+0x10)
    stringstart=stringobject+0x14
    
    --printf("length=%d",length)
    --printf("stringstart=%x",stringstart)
  else
    length=readInteger(stringobject+0x8)
    stringstart=stringobject+0x10
  end
  
  if length==nil then
    return string.format(translate('<Invalid string at %.8x>'), stringobject)
  else  
    return readString(stringstart,length*2,true)
  end    
end

function mono_readObject()
  local vtype = monopipe.readByte()
  --print(vtype)
  if vtype == MONO_TYPE_VOID then
    return monopipe.readQword()
  elseif vtype == MONO_TYPE_STRING then
    local resultlength=monopipe.readWord();
    return monopipe.readString(resultlength);
  end
  
  local vartype = monoTypeToVartypeLookup[vtype]
  if vartype == vtByte then
    return monopipe.readByte(),vtype
  elseif vartype == vtWord then
    return monopipe.readWord(),vtype
  elseif vartype == vtDword then
    return monopipe.readDword(),vtype
  elseif vartype == vtQword then
    return monopipe.readQword(),vtype
  elseif vartype == vtSingle then
    return monopipe.readFloat(),vtype
  elseif vartype == vtDouble then
    return monopipe.readDouble(),vtype
  elseif vartype == vtPointer then
    return monopipe.readQword(),vtype
  else
    if targetIs64Bit() then
      return monopipe.readQword(),vtype
    else
      return monopipe.readDword(),vtype
    end
  end  
  return nil
end

--[[function mono_readObject()
  local vtype = monopipe.readByte()
  if vtype == MONO_TYPE_VOID then
    return nil
  elseif vtype == MONO_TYPE_STRING then
    local resultlength=monopipe.readWord();
    return monopipe.readString(resultlength);
  end
  
  local vartype = monoTypeToVartypeLookup[vtype]
  if vartype == vtByte then
    return monopipe.readByte()
  elseif vartype == vtWord then
    return monopipe.readWord()
  elseif vartype == vtDword then
    return monopipe.readDword()
  elseif vartype == vtQword then
    return monopipe.readQword()
  elseif vartype == vtSingle then
    return monopipe.readFloat()
  elseif vartype == vtDouble then
    return monopipe.readDouble()
  elseif vartype == vtPointer then
    return monopipe.readQword()
  end  
  return nil
end]]

function mono_writeObject(vartype, value)
  if vartype == vtString then
    monopipe.writeByte(MONO_TYPE_STRING)
    monopipe.writeWord(#value);
    monopipe.writeString(value);
  elseif vartype == vtByte then
    monopipe.writeByte(MONO_TYPE_I1)
    monopipe.writeByte(value)
  elseif vartype == vtWord then
    monopipe.writeByte(MONO_TYPE_I2)
    monopipe.writeWord(value)
  elseif vartype == vtDword then
    monopipe.writeByte(MONO_TYPE_I4)
    monopipe.writeDword(value)
  elseif vartype == vtPointer then
    monopipe.writeByte(MONO_TYPE_PTR)
    monopipe.writeQword(value)
  elseif vartype == vtQword then
    monopipe.writeByte(MONO_TYPE_I8)
    monopipe.writeQword(value)
  elseif vartype == vtSingle then
    monopipe.writeByte(MONO_TYPE_R4)
    monopipe.writeFloat(value)
  elseif vartype == vtDouble then
    monopipe.writeByte(MONO_TYPE_R8)
    monopipe.writeDouble(value)
  else
    monopipe.writeByte(MONO_TYPE_VOID)
    monopipe.writeQword(value)
  end
  return nil
  
end

--[[function mono_writeObject(vartype, value)
  if vartype == vtString then
   -- monopipe.writeByte(MONO_TYPE_STRING)
    monopipe.writeWord(#value);
    monopipe.writeString(value);
  elseif vartype == vtByte then
   -- monopipe.writeByte(MONO_TYPE_I1)
    monopipe.writeByte(value)
  elseif vartype == vtWord then
   -- monopipe.writeByte(MONO_TYPE_I2)
    monopipe.writeWord(value)
  elseif vartype == vtDword then
   -- monopipe.writeByte(MONO_TYPE_I4)
    monopipe.writeDword(value)
  elseif vartype == vtPointer then
  -- monopipe.writeByte(MONO_TYPE_PTR)
    monopipe.writeQword(value)
  elseif vartype == vtQword then
 -- monopipe.writeByte(MONO_TYPE_I8)
    monopipe.writeQword(value)
  elseif vartype == vtSingle then
   -- monopipe.writeByte(MONO_TYPE_R4)
    monopipe.writeFloat(value)
  elseif vartype == vtDouble then
   -- monopipe.writeByte(MONO_TYPE_R8)
    monopipe.writeDouble(value)
  else
    --monopipe.writeByte(MONO_TYPE_VOID)
  end
  return nil
  
end
]]
function mono_writeVarType(vartype)
  if vartype == vtString then
    monopipe.writeByte(MONO_TYPE_STRING)
  elseif vartype == vtByte then
    monopipe.writeByte(MONO_TYPE_I1)
  elseif vartype == vtWord then
    monopipe.writeByte(MONO_TYPE_I2)
  elseif vartype == vtDword then
    monopipe.writeByte(MONO_TYPE_I4)
  elseif vartype == vtPointer then
    monopipe.writeByte(MONO_TYPE_PTR)
  elseif vartype == vtQword then
    monopipe.writeByte(MONO_TYPE_I8)
  elseif vartype == vtSingle then
    monopipe.writeByte(MONO_TYPE_R4)
  elseif vartype == vtDouble then
    monopipe.writeByte(MONO_TYPE_R8)
  else
    monopipe.writeByte(MONO_TYPE_VOID)
  end
end

function mono_invoke_method_dialog(domain, method, address, OnResult, OnCreateInstance)
  --spawn a dialog where the user can fill in fields like: instance and parameter values
  --parameter fields will be of the proper type

  --the instance field may be a dropdown dialog which gets populated by mono_class_findInstancesOfClass* or a <new instance> button where the user can choose which constructor etc...
  if method==nil then return nil,'Method==nil' end
  
  local types, paramnames, returntype=mono_method_getSignature(method)

  if types==nil then return nil,'types==nil' end
  
  local parameters=mono_method_get_parameters(method)
  
  if parameters==nil then return nil,'invalid method. has no param info' end
  

  local flags=mono_method_getFlags(method)
  local static=(flags & METHOD_ATTRIBUTE_STATIC) == METHOD_ATTRIBUTE_STATIC
    

  local mifinfo

  local typenames={}
  local tn
  for tn in string.gmatch(types, '([^,]+)') do
    table.insert(typenames, tn)
  end

  if #typenames~=#paramnames then return nil end


  local c=mono_method_getClass(method)
  local classname=''
  if c and (c~=0) then
    classname=mono_class_getName(c)..'.'
  end
  local methodname=classname..mono_method_getName(method)

  
  paramstrings={}
  for i=1,#parameters.parameters do
    paramstrings[i]={}
    paramstrings[i].varname=typenames[i]..' '..paramnames[i]
    paramstrings[i].isObject=parameters.parameters[i].type==MONO_TYPE_VALUETYPE or parameters.parameters[i].type==MONO_TYPE_BYREF or parameters.parameters[i].type==MONO_TYPE_OBJECT
  end

  

  local invokeDialogParams={}
  invokeDialogParams.name=methodname
  invokeDialogParams.isStatic=static
  invokeDialogParams.address=address
  invokeDialogParams.allowCustomAddress=true
  invokeDialogParams.parameters=paramstrings  
  invokeDialogParams.nonmodal=OnResult~=nil  
  
  
  local function OkClickHandler(dialog, idp, output)     
    --ok button clicked (called by the dialog on OK, or when the dialog closes with OK and it's modal)
    
    local instance=invokeDialogParams.address
    local params=parameters    

    --use monoTypeToVartypeLookup to convert it to the type mono_method_invoke likes it
    local args={}
    for i=1, #parameters.parameters do
    
      args[i]={}
      args[i].type=monoTypeToVartypeLookup[parameters.parameters[i].type]
      if parameters.parameters[i].type==MONO_TYPE_STRING then
        args[i].type=vtString
        args[i].value=invokeDialogParams.parameters[i].value --handle strings (which are actually pointers to string objects) specially
      elseif args[i].type==vtPointer then
       --accept hexadecimal strings and strings like '{xxx=123,yyy=456}'
        local input=invokeDialogParams.parameters[i].value
        if input:startsWith('0x') then input=input:sub(3) end
     
        local v=tonumber(input,16)
        if v==nil then
          --not a number
          local valf=loadstring('return '..invokeDialogParams.parameters[i].value)
          if valf==nil then --perhaps the user forgot the { }
            valf=loadstring('return {'..invokeDialogParams.parameters[i].value..'}')
          end
          if valf then
            args[i].value=valf()
          end
        else
          args[i].value=v
        end
      else
        args[i].value=tonumber(invokeDialogParams.parameters[i].value)
      end
    


      if args[i].value==nil then
        messageDialog(translate('parameter ')..i..': "'..invokeDialogParams.parameters[i].value..'" '..translate('is not a valid value'), mtError, mbOK)
        return
      end
    end
    
    --DEBUG: global vars for debug
    _d,_m,_i,_args=domain, method, instance, args
    r,_secondary, vtype=mono_invoke_method(domain, method, instance, args)
    local hrs=nil --human readable string
    if r then
      if type(r)=='table' then --it returned a table instead of value
        --it's for human eyes so sort by varname
        local sorted={}
        for varname, value in pairs(r) do
          local e={}
          e.varname=varname
          e.value=value
          table.insert(sorted, e)
        end
        table.sort(sorted,function(a,b) return a.varname<b.varname end)
        --construct a string readable for the user
        
        local s
        s='{'
        for i=1,#sorted do          
          if i~=1 then s=s..', ' end            
          s=s..string.format("%s=%s", sorted[i].varname, sorted[i].value)
        end

        s=s..'}'
        hrs=s   
      else      
        if readByte(r) then          
          hrs=string.format('%s returned: 0x%x', methodname, r)
        else
          hrs=string.format('%s returned: %s', methodname, r)
        end
      end
    end     
      
    if OnResult then   
      --print("OnResult is set")    
      if r and vtype and hrs and hrs~='' then
       -- print("r and vtype and hrs and hrs~='' ")
        --add the result to the output object
        if output then         
          if getTimeStamp then --being nice to older CE users...
            hrs=getTimeStamp()..' - '..hrs
          else
            hrs=os.date("%H:%M:%S")..' - '..hrs          
          end
          output.insert(0,hrs)
          
        else
         -- print("output==nil")
        end
      end
      OnResult(r, _secondary, vtype, hrs)
    else            
      return r,_secondary, vtype, hrs      
    end   
  end
  
  if invokeDialogParams.nonmodal then
    invokeDialogParams.onOKClick=OkClickHandler    
  end
  
  if OnCreateInstance then
    invokeDialogParams.onCreateInstanceClick=function(dialog, idp, paramindex)
      printf("creating instance of param "..paramindex)
       
      local classhandle=mono_type_get_class(parameters.parameters[paramindex].monotype)
      local r=OnCreateInstance(classhandle)
      
      printf("OnCreateInstance returned %s", r)
      return r
    end
  end
  
  local r
  r=createMethodInvokeDialog(invokeDialogParams)
  if OnResult then return end
  
  if r then
    return OkClickHandler()
  end
end


function mono_invoke_method(domain, method, object, args)

  local parameters=mono_method_get_parameters(method).parameters
  if parameters==nil then    
    return nil,'Parameter lookup failed'
  end  
  
  if object and object~=0 then
    local class=mono_method_getClass(method)
    if mono_type_get_type(mono_class_get_type(class))==MONO_TYPE_VALUETYPE then
      --the object needs to be unboxed to be able to be used
      object=mono_object_unbox(object)
    end
  end

  
  
  for i=1, #args do
    if type(args[i])~='table' or args[i].type==nil or args[i].value==nil then
      --argument isn't in the {type,value} format  
      --Try figuring out what the type is by looking at the method info

   
      local newarg={}
    
      newarg.type=monoTypeToVarType(parameters[i].type)
      newarg.value=args[i]
    
      if newarg.type==vtPointer and type(newarg.value)=='table' then
        local class=mono_type_get_class(parameters[i].monotype)   
        if class then
          --create an instance of this class with the fields setup as in the given table  (e.g input is {x=12,y=13,z=14}
          local o=mono_object_new(class)
          --todo: if o then
          -- call the constructor without params, and fill in the fields
          
          newarg.value=o
        end
        
        args[i]=newarg
      end
    end 

    --if type is MONO_TYPE_VALUETYPE then unbox the object
    if parameters[i].type==MONO_TYPE_VALUETYPE then
      args[i].value=mono_object_unbox(args[i].value)
    end
          
  end

  monopipe.lock()
  monopipe.writeByte(MONOCMD_INVOKEMETHOD)
  
  monopipe.writeQword(method)
  monopipe.writeQword(object)
  
  for i=1, #args do 
    mono_writeObject(args[i].type, args[i].value)
  end
  
  local result, vtype =mono_readObject()
  --print(type(result),result)
  if monopipe then
    local exception = nil
    if monopipe.readByte() == 1 then
      if monopipe.readByte() == 1 then
        local excplen = monopipe.readWord()
        exception = monopipe.readString(excplen)
      end
    end
    monopipe.unlock()
    
    if vtype==MONO_TYPE_VALUETYPE then
      --read the fields from this type and return that instead
      local f=mono_object_enumValues(result)
 
      if f then
        return f, exception, vtype
      end
    --  local 
      
      
    end
    return result, exception, vtype      
  else
   
    --something bad happened
    LaunchMonoDataCollector()
    return nil
  end
  
end

--[[function mono_invoke_method(domain, method, object, args)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_INVOKEMETHOD)
  
  monopipe.writeQword(domain)
  monopipe.writeQword(method)
  monopipe.writeQword(object)
  monopipe.writeWord(#args)
  for i=1, #args do
    mono_writeVarType(args[i].type)
  end
  for i=1, #args do
    mono_writeObject(args[i].type, args[i].value)
  end
  
  local result=mono_readObject()
  
  if monopipe then
    monopipe.unlock()
    return result      
  else
    --something bad happened
    LaunchMonoDataCollector()
    return nil
  end
  
end]]

function mono_loadAssemblyFromFile(fname)
  --if debug_canBreak() then return nil end

  monopipe.lock()
  monopipe.writeByte(MONOCMD_LOADASSEMBLY)
  monopipe.writeWord(#fname)
  monopipe.writeString(fname)
  local result = monopipe.readQword()
  monopipe.unlock()
  
  monocache.nonfoundclasses={}  --reset just the nonfound classes
  return result;  
end

function mono_object_new(klass)
  --if debug_canBreak() then return nil end

  monopipe.lock()
  monopipe.writeByte(MONOCMD_OBJECT_NEW)
  monopipe.writeQword(klass)
  local result = monopipe.readQword()
  monopipe.unlock()
  return result;  
end

function mono_object_init(object)
  --if debug_canBreak() then return nil end

  monopipe.lock()
  monopipe.writeByte(MONOCMD_OBJECT_INIT)
  monopipe.writeQword(object)
  local result = monopipe.readByte()==1
  monopipe.unlock()
  return result;  
end

function mono_new_string(domain, utf8str)
  if type(domain)=='string' and utf8str==nil then
    utf8str=domain
    domain=nil
  end
  
  monopipe.lock()
  monopipe.writeByte(MONOCMD_NEWSTRING)
  monopipe.writeQword(domain)
  monopipe.writeWord(#utf8str)
  monopipe.writeString(utf8str)
  local result = monopipe.readQword()
  monopipe.unlock()
  return result;    
end

--[[

--------code belonging to the mono dissector form---------

--]]

function monoform_miShowMethodParametersClick(sender)  
  monoSettings.Value["ShowMethodParameters"]=sender.checked  
end


function monoform_miShowILDisassemblyClick(sender)
  if (monoForm.TV.Selected~=nil) then
    local node=monoForm.TV.Selected
    if (node~=nil) and (node.Level==4) and (node.Parent.Text=='methods') then
      local f=createForm()
      f.BorderStyle=bsSizeable
      f.centerScreen()
      f.Caption=node.Text
      f.OnClose=function(sender) return caFree end
      local m=createMemo(f)
      m.Align=alClient
      m.ScrollBars=ssBoth

      m.Lines.Text=mono_method_disassemble(node.Data)
    end
  end

end

function monoform_miInvokeMethodClick(sender)
  local node=monoForm.TV.Selected

  if (node~=nil) and (node.Level==4) and (node.Parent.Text=='methods') then
    mono_invoke_method_dialog(nil, node.data)
  end

  
end

function monoform_miRejitClick(sender)
  if (monoForm.TV.Selected~=nil) then
    local node=monoForm.TV.Selected
    if (node~=nil) and (node.Level==4) and (node.Parent.Text=='methods') then
      local r=mono_compile_method(node.Data)
      getMemoryViewForm().DisassemblerView.SelectedAddress=r
      getMemoryViewForm().show()
--      print(string.format("Method at %x", r))
    end
  end
end

function monoform_miGetILCodeClick(sender)
  if monopipe.IL2CPP then return end
  
  if (monoForm.TV.Selected~=nil) then
    local node=monoForm.TV.Selected
    if (node~=nil) and (node.Level==4) and (node.Parent.Text=='methods') then
      local r,s=mono_getILCodeFromMethod(node.Data)
      if r~=nil then
        print(string.format(translate("ILCode from %x to %x"), r,r+s))
      end
    end
  end
end

function mono_array_element_size(arrayKlass)
  if not arrayKlass or arrayKlass==0 then return 0 end
  monopipe.lock()
  monopipe.writeByte(MONOCMD_ARRAYELEMENTSIZE)
  monopipe.writeQword(arrayKlass)
  local retv = monopipe.readQword()
  if monopipe then
    monopipe.unlock()
  else
    return 0
  end
  return retv
end

function mono_array_new(klass,count)
  count = count and count or 0
  assert(klass and klass~=0,'Error: The Element class for array must be defined')
  monopipe.lock()
  monopipe.writeByte(MONOCMD_MONOARRAYNEW)
  monopipe.writeQword(klass)
  monopipe.writeDword(count)
  local retv = monopipe.readQword()
  monopipe.unlock()
  return retv
end


function monoform_miDissectShowStruct(s, address)
  if s then
    --show it
    --print("showing "..s.Name)
    
    f=enumStructureForms()
    local i
    for i=1,#f do
      if (f[i].MainStruct==s) then
        if address then
          --add it to the window
          local j
          local found=false
          for j=0, f.ColumnCount-1 do
            if f.Column[j].Address==address then
              found=true
              break;
            end
          end
          
          if not found then
            local c=f.addColumn()
            c.Address=address
          end
        end
        f[i].show()
        return
      end
    end
    
    --still here
    --print('new one')
    
    if address==nil then address=0 end
    f=createStructureForm(address,'Group 1',s.Name)        
    f.show()    
  end
end

function monoform_miDissectStaticStructureClick(sender)
  -- combine adding static to dissect and to table
  if (monoForm.TV.Selected~=nil) then
    local node=monoForm.TV.Selected
    if (node~=nil) and (node.Data~=nil) and (node.Level==2) then
      monoform_miAddStaticFieldAddressClick(sender) 
      local smap = monoform_getStructMap()
      local s = monoform_exportStruct(node.Data, nil, true, true, smap, true, false)
      
      monoform_miDissectShowStruct(s) 
    end
  end
end

function monoform_miAddStructureClick(sender)
  if (monoForm.TV.Selected~=nil) then
    local node=monoForm.TV.Selected
    if (node~=nil) and (node.Data~=nil) and (node.Level==2) then
      local smap = monoform_getStructMap()
      local s = monoform_exportStruct(node.Data, nil, false, false, smap, true, false)
      monoform_exportStruct(node.Data, nil, false, true, smap, true, false)
      
      monoform_miDissectShowStruct(s)
      
    end
  end
end

function monoform_miAddStructureRecursiveClick(sender)
  if (monoForm.TV.Selected~=nil) then
    local node=monoForm.TV.Selected
    if (node~=nil) and (node.Data~=nil) and (node.Level==2) then
      local smap = monoform_getStructMap()
      local s = monoform_exportStruct(node.Data, nil, true, false, smap, true, false)
      s = monoform_exportStruct(node.Data, nil, true, true, smap, true, false)
    end
  end
end

function monoform_miFindInstancesOfClass(sender)
  local node=monoForm.TV.Selected
  if (node~=nil) then    
    if (node.Data~=nil) and (node.Level==2) then     
      mono_class_findInstancesOfClass(nil, node.data) 
    end
  end
end

function monoform_createInstanceOfClass(sender)
  local node=monoForm.TV.Selected
  if (node~=nil) then    
    if (node.Data~=nil) and (node.Level==2) then     
      local r=mono_object_new(node.data)
      if r then
        print(string.format("mono_object_new returned %x",r))
        if r and (r~=0) then
          r=mono_object_init(r);
          if r then
            print(string.format("mono_object_init returned success"))
          else
            print(string.format("mono_object_init returned false"))
          end        
        end
      end
      
    end  
  end  
end

--[[
function monoform_miCreateObject(sender)
  if (monoForm.TV.Selected~=nil) then
    local node=monoForm.TV.Selected
    if (node~=nil) and (node.Data~=nil) and (node.Level==2) then
      --create this class object and call the .ctor if it has one
      --todo: implement this 
      
    end
  end
end
--]]


-- Add the script for locating static data pointer for a class and adding records
function monoform_AddStaticClass(domain, image, class)
  if domain==nil or image==nil or class==nil then
    return
  end
  
  local addrs = getAddressList()
  local classname=mono_class_getName(class)
  local namespace=mono_class_getNamespace(class)
  local assemblyname=mono_image_get_name(image)

  local prefix, rootmr, mr
  prefix = ''
  rootmr=addresslist_createMemoryRecord(addrs)
  rootmr.Description = translate("Resolve ")..classname
  rootmr.Type = vtAutoAssembler

  local symclassname = classname:gsub("([^A-Za-z0-9%.,_$`<>%[%]])", "")
  local script = {}
  script[#script+1] = '[ENABLE]'
  script[#script+1] = monoAA_GETMONOSTATICDATA(assemblyname, namespace, classname, symclassname, true)
  script[#script+1] = '[DISABLE]'
  script[#script+1] = monoAA_GETMONOSTATICDATA(assemblyname, namespace, classname, symclassname, false)
  rootmr.Script = table.concat(script,"\n")
  memoryrecord_setColor(rootmr, 0xFF0000)
  --local data = mono_class_getStaticFieldAddress(domain, class)
  --rootmr.Address = string.format("%08X",data)
  --rootmr.Type = vtPointer
  mr=addresslist_createMemoryRecord(addrs)
  mr.Description=classname..'.Static'
  mr.Address='['..symclassname..".Static]"
  mr.Type=vtPointer
  mr.appendToEntry(rootmr)

  mr=addresslist_createMemoryRecord(addrs)
  mr.Description=classname..'.Class'
  mr.Address='['..symclassname..".Class]"
  mr.Type=vtPointer
  mr.appendToEntry(rootmr)

  local i
  local fields=mono_class_enumFields(class)
  for i=1, #fields do
    if fields[i].isStatic and not fields[i].isConst and (field==nil or fields[i].field==field) then
      local fieldName = fields[i].name:gsub("([^A-Za-z0-9%.,_$`<>%[%]])", "")
      local offset = fields[i].offset
      if fieldName==nil or fieldName:len()==0 then
        fieldName = string.format(translate("Offset %x"), offset)
      end
      mr=addresslist_createMemoryRecord(addrs)
      mr.Description=prefix..fieldName

      if fields[i].monotype==MONO_TYPE_STRING then
        -- mr.Address=string.format("[[%s.Static]+%X]+C",symclassname,offset)
        mr.Address=symclassname..'.Static'
        mr.OffsetCount=2
        mr.Offset[0]=0xC
        mr.Offset[1]=offset
        mr.Type=vtString
        memoryrecord_string_setUnicode(mr, true)
        memoryrecord_string_setSize(mr, 80)
      else
        mr.Address=symclassname..'.Static'
        mr.OffsetCount=1
        mr.Offset[0]=offset
        mr.Type=monoTypeToVarType(fields[i].monotype)
      end
      if rootmr~=nil then
         mr.appendToEntry(rootmr)
      else
          break
      end
    end
  end
end

function monoform_AddStaticClassField(domain, image, class, fieldclass, field)
  if domain==nil or image==nil or class==nil or fieldclass==nil or field==nil then
    return
  end
  local i
  local fields=mono_class_enumFields(fieldclass)
  for i=1, #fields do
    if fields[i].field==field then
      local fieldname = fields[i].name
      local offset = fields[i].offset
      if fieldname==nil or fieldname:len()==0 then
        fieldname = string.format(translate("Offset %x"), offset)
      end
      
      local addrs = getAddressList()
      local classname=mono_class_getName(class)
      local namespace=mono_class_getNamespace(class)
      local assemblyname=mono_image_get_name(image)

      local rootmr, mr
      rootmr=addresslist_createMemoryRecord(addrs)
      rootmr.Description = translate("Resolve ")..classname.."."..fieldname
      rootmr.Type = vtAutoAssembler

      local symclassname = classname:gsub("[^A-Za-z0-9._]", "")
      local symfieldname = fieldname:gsub("[^A-Za-z0-9._]", "")
      local script = {}
      script[#script+1] = '[ENABLE]'
      script[#script+1] = monoAA_GETMONOSTATICFIELDDATA(assemblyname, namespace, classname, fieldname, symclassname, true)
      script[#script+1] = '[DISABLE]'
      script[#script+1] = monoAA_GETMONOSTATICFIELDDATA(assemblyname, namespace, classname, fieldname, symclassname, false)
      rootmr.Script = table.concat(script,"\n")
      memoryrecord_setColor(rootmr, 0xFF0000)
      
      mr=addresslist_createMemoryRecord(addrs)
      mr.Description=classname..'.'..fieldname
      mr.appendToEntry(rootmr)

      if fields[i].monotype==MONO_TYPE_STRING then
        mr.Address=symclassname..'.'..symfieldname
        mr.OffsetCount=1
        mr.Offset[0]=0xC
        mr.Type=vtString
        memoryrecord_string_setUnicode(mr, true)
        memoryrecord_string_setSize(mr, 80)
      else
        mr.Address="["..symclassname..'.'..symfieldname.."]"
        mr.Type=monoTypeToVarType(fields[i].monotype)
      end      
      break
    end
  end
end

function monoform_miAddStaticFieldAddressClick(sender)
  if (monoForm.TV.Selected~=nil) then
    local node=monoForm.TV.Selected
    local domain, image, class, field
    if (node~=nil) and (node.Data~=nil) then
      if (node.Level>=4) and (node.Parent.Text=='static fields') then
        local inode = node.Parent.Parent.Parent
        local cnode = node.Parent.Parent
        local fieldclass = cnode.Data
        while inode.Text == 'base class' do
          cnode = inode.Parent
          inode = cnode.Parent
        end        
        domain = inode.Parent.Data
        image = inode.Data
        class = cnode.Data
        field = node.Data
        monoform_AddStaticClassField(domain, image, class, fieldclass, field)
      elseif (node~=nil) and (node.Data~=nil) and (node.Level==2) then
        domain = node.Parent.Parent.Data
        image = node.Parent.Data
        class = node.Data
        monoform_AddStaticClass(domain, image, class)
      elseif (node~=nil) and (node.Data~=nil) and (node.Level==3) then
        domain = node.Parent.Parent.Parent.Data
        image = node.Parent.Parent.Data
        class = node.Parent.Data
        monoform_AddStaticClass(domain, image, class)
      end
    end

  end
end


function monoform_context_onpopup(sender)
  if monopipe==nil then return end  
  if tonumber(monopipe.ProcessID)~=getOpenedProcessID() then return end
  
  local node=monoForm.TV.Selected

  local methodsEnabled = (node~=nil) and (node.Level==4) and (node.Parent.Text=='methods')
  monoForm.miRejit.Enabled = methodsEnabled
  monoForm.miInvokeMethod.Enabled = methodsEnabled
  monoForm.miGetILCode.Enabled = methodsEnabled and (monopipe.IL2CPP==false)
  monoForm.miShowILDisassembly.Enabled = methodsEnabled and (monopipe.IL2CPP==false)
  local structuresEnabled = (node~=nil) and (node.Data~=nil) and (node.Level==2)
  monoForm.miExportStructure.Enabled = structuresEnabled
  local fieldsEnabled = (node~=nil) and (node.Data~=nil)
    and ( (node.Level==2)
      or ((node.Level>=3) and (node.Text=='static fields'))
      or ((node.Level>=4) and (node.Parent.Text=='static fields')))
  monoForm.miFieldsMenu.Enabled = fieldsEnabled
  monoForm.miAddStaticFieldAddress.Enabled = fieldsEnabled
  
  monoForm.miFindInstancesOfClass.Enabled=structuresEnabled
  monoForm.miCreateClassInstance.Enabled=structuresEnabled
end



function monoform_EnumImages(node)
  --print("monoform_EnumImages")
  local i

  local assemblies=mono_enumAssemblies()
  local images={}
  
  mono_enumImages(
    function(image)
      local imagename=mono_image_get_name(image)
      
      if imagename then       
        local e={}
        e.image=image
        e.imagename=imagename  

        table.insert(images,e)        
      end
    end
  )
  
  table.sort(images,function(e1,e2)
    return e1.imagename < e2.imagename
  end)
  
  for i=1,#images do    
    local n=node.add(string.format("%x : %s", images[i].image, images[i].imagename))          n.HasChildren=true
    n.Data=images[i].image
    n.HasChildren=true
  end
end

function monoform_AddClass(node, klass, namespace, classname, fqname)
  local desc=string.format("%x : %s", klass, fqname)
  local n=node.add(desc)
  n.Data=klass
  
  local nf=n.add("static fields")
  nf.Data=klass
  nf.HasChildren=true
  
  local nf=n.add("fields")
  nf.Data=klass
  nf.HasChildren=true

  local nm=n.add("methods")
  nm.Data=klass
  nm.HasChildren=true
  
  local p = mono_class_getParent(klass)
  if p~=nil then
    local np=n.add("base class")
    np.Data=p
    np.HasChildren=true
  end
end

function monoform_EnumClasses(node)
  --print("monoform_EnumClasses")
  local image=node.Data
  local classes=mono_image_enumClasses(image)
  local i
  if classes~=nil then
    for i=1, #classes do
      classes[i].fqname = mono_class_getFullName(classes[i].class)
      if classes[i].fqname==nil or classes[i].fqname=='' then        
        classes[i].fqname=classes[i].classname
        
        if classes[i].fqname==nil or classes[i].fqname=='' then  
          classes[i].fqname='<unnamed>'
        end
      end
    end
  
    local monoform_class_compare = function (a,b)
      if a.namespace < b.namespace then
        return true
      elseif b.namespace < a.namespace then
        return false
      end
      if a.fqname < b.fqname then
        return true
      elseif b.fqname < a.fqname then
        return false
      end
      return a.class < b.class
    end
  
    table.sort(classes, monoform_class_compare)

    for i=1, #classes do
      monoform_AddClass(node, classes[i].class, classes[i].namespace, classes[i].classname, classes[i].fqname)
    end
  end

end;

function monoform_EnumFields(node, static)
 -- print("monoform_EnumFields")
  local i
  local class=node.Data;
  local fields=mono_class_enumFields(class)
  for i=1, #fields do
    if fields[i].isStatic == static and not fields[i].isConst then
      local n=node.add(string.format(translate("%x : %s (type: %s)"), fields[i].offset, fields[i].name,  fields[i].typename))
      n.Data=fields[i].field
    end
  end
end

function getParameterFromMethod(method)
  if method==nil then return ' ERR:method==nil' end
  
  local types,paramnames,returntype=mono_method_getSignature(method)
  
  if types==nil then return ' ERR:types==nil' end

  local typenames={}
  local tn
  for tn in string.gmatch(types, '([^,]+)') do
    table.insert(typenames, tn)
  end

  if #typenames==#paramnames then
    local r='('
    local i
    local c=#paramnames

    for i=1,c do
      r=r..paramnames[i]..': '..typenames[i]
      if i<c then
        r=r..'; '
      end
    end

    r=r..'):'..returntype
    return r

  else
    return '? - ('..types..'):'..returntype
  end
end


function monoform_EnumMethods(node)
  --print("monoform_EnumMethods")
  local i
  local class=node.Data;


  local methods=mono_class_enumMethods(class,monoForm.miShowParentMethods.Checked)
  for i=1, #methods do
    local parameters=''
    if monoForm.miShowMethodParameters.Checked then
      parameters=getParameterFromMethod(methods[i].method)
      if parameters==nil then parameters='' end
    end
    
    local n=node.add(string.format("%x : %s %s", methods[i].method, methods[i].name, parameters))
    n.Data=methods[i].method
  end
end


function mono_TVExpanding(sender, node)
  --print("mono_TVExpanding")
  --print("node.Count="..node.Count)
  --print("node.Level="..node.Level)

  local allow=true
  if (node.Count==0) then
    if (node.Level==0) then  --images
      monoform_EnumImages(node)
    elseif (node.Level==1) then --classes
      monoform_EnumClasses(node)
    elseif (node.Level>=3) and (node.Text=='static fields') then --static fields
      monoform_EnumFields(node, true)
    elseif (node.Level>=3) and (node.Text=='fields') then --fields
      monoform_EnumFields(node, false)
    elseif (node.Level>=3) and (node.Text=='methods') then --methods
      monoform_EnumMethods(node)
    elseif (node.Level>=3) and (node.Text=='base class') then 
      if (monoForm.autoExpanding==nil) or (monoForm.autoExpanding==false) then
        local klass = node.Data
        if (klass ~= 0) then
          local classname=mono_class_getName(klass)
          local namespace=mono_class_getNamespace(klass)
          local fqname=mono_class_getFullName(klass)
          monoform_AddClass(node, klass, namespace, classname, fqname)
        end
      else
        allow=false --don't auto expand the base classes
      end
    end

  end

  return allow
end


function mono_TVCollapsing(sender, node)
  local allow=true

  return allow
end

function monoform_miFindNextClick(sender)
  --repeat the last scan
  monoForm.FindDialog.OnFind(sender)
end


function monoform_FindDialogFindClass(sender)
  local texttofind=string.lower(monoForm.FindDialog.FindText)
  local tv=monoForm.TV
  local i=0
  local startindex=0
  local expandnodes=string.find(monoForm.FindDialog.Options, 'frEntireScope')
  
  if tv.Selected~=nil then
    startindex=tv.Selected.AbsoluteIndex+1
  end
  
  i=startindex
  
  tv.beginUpdate()
  while i<tv.Items.Count do
    local node=tv.Items[i]
    if (node.Level==2) then
      local text=string.lower(node.Text)
      if text:find(texttofind) then
        tv.Selected=node  
        monoForm.miFindNext.Enabled=true
        break;
      end
    end

    if expandnodes and (node.Level<2) then
      node.Expand(false)
    end
    
    i=i+1    
  end
  
  tv.endUpdate()

  
  
end

function monoform_FindDialogFind(sender)
  local texttofind=string.lower(monoForm.FindDialog.FindText)
  local tv=monoForm.TV
  local startindex=0

  if tv.Selected~=nil then
    startindex=tv.Selected.AbsoluteIndex+1
  end


  local i


  if string.find(monoForm.FindDialog.Options, 'frEntireScope') then
    --deep scan
    tv.beginUpdate()
    i=startindex
    while i<tv.Items.Count do
      local node=monoForm.TV.items[i]
      local text=string.lower(node.Text)

      if string.find(text, texttofind)~=nil then
          --found it
        tv.Selected=node
        monoForm.miFindNext.Enabled=true
        break
      end
      
      if node.HasChildren then
        node.Expand(false)
      end

      i=i+1
    end

    tv.endUpdate()
  else
    --just the already scanned stuff
    for i=startindex, tv.Items.Count-1 do
      local node=monoForm.TV.items[i]
      local text=string.lower(node.Text)

      if string.find(text, texttofind)~=nil then
          --found it
        tv.Selected=node
        monoForm.miFindNext.Enabled=true
        return
      end
    end
  end



end

function monoform_miFindClassClick(sender)
  --print("findclass click");
  monoForm.FindDialog.OnFind=monoform_FindDialogFindClass
  monoForm.FindDialog.execute()
end


function monoform_miFindClick(sender)
  --print("find click");
  monoForm.FindDialog.OnFind=monoform_FindDialogFind
  monoForm.FindDialog.execute()
end


function monoform_miExpandAllClick(sender)
  if messageDialog(translate("Are you sure you wish to expand the whole tree? This can take a while and Cheat Engine may look like it has crashed (It has not)"), mtConfirmation, mbYes, mbNo)==mrYes then
    monoForm.TV.beginUpdate()
    monoForm.autoExpanding=true --special feature where a base object can contain extra lua variables
    monoForm.TV.fullExpand()
    monoForm.autoExpanding=false
    monoForm.TV.endUpdate()
  end
end

function monoform_miSaveClick(sender)
  if monoForm.SaveDialog.execute() then
    monoForm.TV.saveToFile(monoForm.SaveDialog.Filename)
  end
end



function mono_dissect()
  --shows a form with a treeview that holds all the data nicely formatted.
  --only fetches the data when requested
  if (monopipe==nil)  then
    LaunchMonoDataCollector()
  end
  
  if (monoForm==nil) then
    monoForm=createFormFromFile(getAutorunPath()..'forms'..pathsep..'MonoDataCollector.frm')
    if monoSettings.Value["ShowMethodParameters"]~=nil then
      if monoSettings.Value["ShowMethodParameters"]=='' then
        monoForm.miShowMethodParameters.Checked=true
      else
        monoForm.miShowMethodParameters.Checked=monoSettings.Value["ShowMethodParameters"]=='1'
      end
    else
      monoForm.miShowMethodParameters.Checked=true
    end
  end

  monoForm.OnDestroy=function(s)
    if monoSettings then
      monoSettings.Value['monoform.x']=s.left
      monoSettings.Value['monoform.y']=s.top
      monoSettings.Value['monoform.width']=s.width
      monoSettings.Value['monoform.height']=s.height  
    end
  end

  local newx=tonumber(monoSettings.Value['monoform.x'])
  local newy=tonumber(monoSettings.Value['monoform.y'])
  local newwidth=tonumber(monoSettings.Value['monoform.width'])
  local newheight=tonumber(monoSettings.Value['monoform.height']) 
  
  if (newx and newx>getWorkAreaWidth()) then newx=nil end --make sure it stays within the workable area
  if (newy and newy>getWorkAreaHeight()) then newy=nil end
  
  if newx and newy then monoForm.left=newx end
  if newx and newy then monoForm.top=newy end
  if newx and newy and newwidth then monoForm.width=newwidth end
  if newx and newy and newheight then monoForm.height=newheight end
 
  
  monoForm.show()

  monoForm.TV.Items.clear()

  local domains=mono_enumDomains()
  local i

  if (domains~=nil) then
    for i=1, #domains do
      n=monoForm.TV.Items.add(string.format("%x", domains[i]))
      n.Data=domains[i]
      monoForm.TV.Items[i-1].HasChildren=true
    end
  end

end

function miMonoActivateClick(sender)
  if monopipe then
    if isKeyPressed(VK_CONTROL) then
    monopipe.lock()
    monopipe.writeByte(MONOCMD_TERMINATE)
    monopipe.unlock()
    monopipe.OnTimeout()
    print('dll ejected')
    else
    monopipe.OnTimeout()
    end
  else  
    if LaunchMonoDataCollector()==0 then
      showMessage(translate("Failure to launch  "))
    end
  end
end

function miMonoDissectClick(sender)
  mono_dissect()
end





function mono_setMonoMenuItem(usesmono, usesdotnet)
 
  --print("mono_setMonoMenuItem ")
  --if usesmono then print("usesmono") end
  --if usesdotnet then print("usesdotnet") end
  
  if usesmono or usesdotnet then
    --create a menu item if needed
    
    
    if (miMonoTopMenuItem==nil) then
      local mfm=MainForm.Menu
      
      if mfm then
        local mi
        miMonoTopMenuItem=createMenuItem(MainForm)
       
        mfm.Items.insert(mfm.Items.Count-1, miMonoTopMenuItem) --add it before help

        mi=createMenuItem(MainForm)
        mi.Caption=translate("Activate mono features")
        mi.OnClick=miMonoActivateClick
        mi.Name='miMonoActivate'
        miMonoTopMenuItem.Add(mi)

        mi=createMenuItem(MainForm)
        mi.Caption=translate("Dissect mono")
        mi.Shortcut="Ctrl+Alt+M"
        mi.OnClick=miMonoDissectClick
        mi.Name='miMonoDissect'
        miMonoTopMenuItem.Add(mi)
        
        mi=createMenuItem(MainForm)
        mi.Caption="-" 
        mi.Name="miDotNetSeperator"
        miMonoTopMenuItem.Add(mi)        
        
        mi=createMenuItem(MainForm)
        mi.Caption=translate(".Net Info")
        mi.Shortcut="Ctrl+Alt+N"
        mi.OnClick=miDotNetInfoClick
        mi.Name='miDotNetInfo'
        miMonoTopMenuItem.Add(mi)        
        
        
        miMonoTopMenuItem.OnClick=function(s)
          MainForm.miMonoActivate.Checked=monopipe~=nil
        end
      end
    end
    
    if miMonoTopMenuItem then
      MainForm.miMonoActivate.Visible=true
      MainForm.miMonoDissect.Visible=true
      MainForm.miDotNetSeperator.Visible=true
        
      if usesmono and not usesdotnet then
        miMonoTopMenuItem.Caption=translate("Mono")
      elseif usesdotnet and not usesmono then  
        miMonoTopMenuItem.Caption=translate(".Net")
        MainForm.miMonoActivate.Visible=false
        MainForm.miMonoDissect.Visible=false      
        MainForm.miDotNetSeperator.Visible=false
      else
        miMonoTopMenuItem.Caption=translate("Mono/.Net")     
      end
    end

    
  end
  
  if (not usesmono) and (not usesdotnet) then  
    --destroy the menu item if needed
    if miMonoTopMenuItem~=nil then
      MainForm.miMonoDissect.destroy() --clean up the onclick handler
      MainForm.miMonoActivate.destroy()  --clean up the onclick handler
      
      miMonoTopMenuItem.destroy() --also destroys the subitems as they are owned by this menuitem
      miMonoTopMenuItem=nil
    end

    if monopipe~=nil then
      monopipe.destroy()
      monopipe=nil

      if mono_AddressLookupID~=nil then
        unregisterAddressLookupCallback(mono_AddressLookupID)
        mono_AddressLookupID=nil
      end


      if mono_SymbolLookupID~=nil then
        unregisterSymbolLookupCallback(mono_SymbolLookupID)
        mono_SymbolLookupID=nil
      end

    end
  else
    --update the menu visibility
    
    
  end
end

function mono_checkifmonoanyhow(t)
  while t.Terminated==false do
    local r=getAddressSafe('mono_thread_attach',false,true)
    local r2=getAddressSafe('il2cpp_thread_attach',false,true)
    
    if (r~=nil) or (r2~=nil) then
      --print("thread_checkifmonoanyhow found the mono_thread_attach export")
      thread_checkifmonoanyhow=nil
      synchronize(mono_setMonoMenuItem, true)
      return
    end
    sleep(2000)
  end
end




function mono_OpenProcessMT()
 -- print("mono_OpenProcessMT")
  --enumModules is faster than getAddress at OpenProcess time (No waiting for all symbols to be loaded first)
  local usesmono=false
  local usesdotnet=false
  local m=enumModules()
  local i
  for i=1, #m do
   -- print(m[i].Name)
    if (m[i].Name=='mono.dll') or (string.sub(m[i].Name,1,5)=='mono-') or (string.sub(m[i].Name,1,7)=='libmono') or (string.sub(m[i].Name,1,9)=='libil2cpp') or (m[i].Name=='GameAssembly.dll') or (m[i].Name=='UnityPlayer.dll')  then
      usesmono=true
    end   
    
    
    if (m[i].Name=='clr.dll') or (m[i].Name=='coreclr.dll') or (m[i].Name=='clrjit.dll') then
      usesdotnet=true
    end    
  end
  
  synchronize(function()
    mono_setMonoMenuItem(usesmono, usesdotnet)
  end)
  
  if (usesmono==false) and (getOperatingSystem()==1) and (thread_checkifmonoanyhow==nil) then
    thread_checkifmonoanyhow=createThread(mono_checkifmonoanyhow)
  end  
  

  if (monopipe~=nil) and (monopipe.ProcessID~=getOpenedProcessID()) then
    --different process
    synchronize(function()
      monopipe.destroy()
      monopipe=nil

      if mono_AddressLookupID~=nil then
        unregisterAddressLookupCallback(mono_AddressLookupID)
        mono_AddressLookupID=nil
      end


      if mono_SymbolLookupID~=nil then
        unregisterSymbolLookupCallback(mono_SymbolLookupID)
        mono_SymbolLookupID=nil
      end

      if mono_StructureNameLookupID~=nil then
        unregisterStructureNameLookup(mono_StructureNameLookupID)
        mono_StructureNameLookupID=nil
      end

      if mono_StructureDissectOverrideID~=nil then
        unregisterStructureDissectOverride(mono_StructureDissectOverrideID)
        mono_StructureDissectOverrideID=nil
      end
    end)
  end

end

function mono_OnProcessOpened(processid, processhandle, caption)
  --call the original onOpenProcess if there was one
  if mono_OldOnProcessOpened~=nil then
    mono_OldOnProcessOpened(processid, processhandle, caption)
  end
  
  if mono_OpenProcessMTThread==nil then --don't bother if it exists
    mono_OpenProcessMTThread=createThread(function(t)       
      t.Name='mono_OpenProcessMT'
      --print("mono_OpenProcessMTThread")
      mono_OpenProcessMT(t)
      mono_OpenProcessMTThread=nil
      --print("mono_OpenProcessMTThread finished")
    end)  
  end

  
end

function monoAA_USEMONO(parameters, syntaxcheckonly)
  --called whenever an auto assembler script encounters the USEMONO() line
  --the value you return will be placed instead of the given line
  --In this case, returning a empty string is fine
  --Special behaviour: Returning nil, with a secondary parameter being a string, will raise an exception on the auto assembler with that string

  --another example:
  --return parameters..":\nnop\nnop\nnop\n"
  --you'd then call it using usemono(00400500) for example

  if (syntaxcheckonly==false) and (LaunchMonoDataCollector()==0) then
    return nil,translate("The mono handler failed to initialize")
  end

  return "" --return an empty string (removes it from the internal aa assemble list)
end

function monoAA_FINDMONOMETHOD(parameters, syntaxcheckonly)
  --called whenever an auto assembler script encounters the MONOMETHOD() line

  --parameters: name, fullmethodnamestring
  --turns into a define that sets up name as an address to this method

  local name, fullmethodnamestring, namespace, classname, methodname, methodaddress
  local c,d,e

  --parse the parameters
  c=string.find(parameters,",")
  if c~=nil then
    name=string.sub(parameters, 1,c-1)

    fullmethodnamestring=string.sub(parameters, c+1, #parameters)
    c=string.find(fullmethodnamestring,":")
    if (c~=nil) then
      namespace=string.sub(fullmethodnamestring, 1,c-1)
    else
      namespace='';
    end

    d=string.find(fullmethodnamestring,":",c)
    if (d~=nil) then
      e=string.find(fullmethodnamestring,":",d+1)
      if e~=nil then
        classname=string.sub(fullmethodnamestring, c+1, e-1)
        methodname=string.sub(fullmethodnamestring, e+1, #fullmethodnamestring)
      else
        return nil,translate("Invalid parameters (Methodname could not be determined)")
      end
    else
      return nil,translate("Invalid parameters (Classname could not be determined)")
    end
  else
    return nil,translate("Invalid parameters (name could not be determined)")
  end


  classname=classname:match "^%s*(.-)%s*$" --trim
  methodname=methodname:match "^%s*(.-)%s*$" --trim


  if syntaxcheckonly then
    return "define("..name..",00000000)"
  end

  if (monopipe==nil) or (monopipe.Connected==false) then
    LaunchMonoDataCollector()
  end

  if (monopipe==nil) or (monopipe.Connected==false) then
    return nil,translate("The mono handler failed to initialize")
  end


  local method=mono_findMethod(namespace, classname, methodname)
  if (method==0) then
    return nil,fullmethodnamestring..translate(" could not be found")
  end

  methodaddress=mono_compile_method(method)
  if (methodaddress==0) then
    return nil,fullmethodnamestring..translate(" could not be jitted")
  end


  local result="define("..name..","..string.format("%x", methodaddress)..")"

 -- showMessage(result)

  return result
end


function monoAA_FINDMONOMETHODWITHPARAMS(parameters, syntaxcheckonly)
  --called whenever an auto assembler script encounters the FINDMONOMETHODWITHPARAMS() line

  --parameters: name, fullmethodnamestring, (OPTIONAL)method param types
  --turns into a define that sets up name as an address to this method
  --example for Underminer - no namespace, class Inventory method TryRemoveItem
  --has three instances, we want one with 'ItemType' and 'int' parameters:
  --NOTE: cannot be any spaces inside parameters with comma (ItemType,int)!
  --FINDMONOMETHODWITHPARAMS(tryremoveitem, Inventory:TryRemoveItem, ItemType,int)

  local trim = function(s)
    return s:match"^%s*(.*)":match"(.-)%s*$"
  end

  local take = function(s, sep)
    sep = sep or ","
    local position = string.find(s, sep)
    if position then
      local first = string.sub(s, 1, position - 1)
      local remainder = string.sub(s, position + 1, #s)
      return first, remainder
    end
    return s, nil
  end

  local take_and_trim = function(s, sep)
    local a, b = take(s, sep)
    return trim(a), b
  end

  local name, r = take_and_trim(parameters)
  local full_method_name, r = take_and_trim(r)
  local signature = nil
  if r then signature = trim(r) end

  --parse the parameters
  local namespace, r1 = take_and_trim(full_method_name, ":")
  local class_name, r2 = take_and_trim(r1, ":")
  local method_name = nil
  if r2 == nil then
    -- just Class:Method if no namespace
    method_name = class_name
    class_name = namespace
    namespace = ""
  else
    method_name, r3 = take_and_trim(r2, ":")
  end

  if syntaxcheckonly then
    return "define("..name..",00000000)"
  end

  if (monopipe==nil) or (monopipe.Connected==false) then
    LaunchMonoDataCollector()
  end

  if (monopipe==nil) or (monopipe.Connected==false) then
    return nil,translate("The mono handler failed to initialize")
  end

  local class_id = mono_findClass(namespace, class_name)
  local foundMethods = mono_class_enumMethods(class_id)
  local methods = {}
  local exact = {}
  for k,v in pairs(foundMethods) do
    if v.name == method_name then
      v.parameters = mono_method_get_parameters(v.method)
      v.signature = mono_method_getSignature(v.method)
      if signature ~= nil and v.signature == signature then exact = v end
      table.insert(methods, v)
    end
  end
  if #methods == 1 then exact = methods[1] end -- no need for params if only 1
  if not exact then
     if #methods > 1 then
        return nil, full_method_name..translate(" has overrides, specify parameters")
     end
     return nil, full_method_name..translate(" not found")
  end

  local method_address = mono_compile_method(exact.method)
  if (method_address == 0) then
    return nil, full_method_name..translate(" could not be jitted")
  end

  local result="define("..name..","..string.format("%x", method_address)..")"
  return result
end

function monoform_getStructMap()
  -- TODO: bug check for getStructureCount which does not return value correctly in older CE
  local structmap={}
  local n=getStructureCount()
  if n==nil then
    showMessage(translate("Sorry this feature does not work yet.  getStructureCount needs patching first."))
    return nil
  end
  local fillChildStruct = function (struct, structmap) 
    local i, e, s
    if struct==nil then return end
    for i=0, struct.Count-1 do
      e = struct.Element
      if e.Vartype == vtPointer then
        s = e.ChildStruct
        if s~=nil then fillChildStruct(s, structmap) end
      end      
    end
  end
  for i=0, n-1 do
    local s = getStructure(i)
    structmap[s.Name]=s
    fillChildStruct(s, structmap)
  end
  return structmap
end

function mono_purgeDuplicateGlobalStructures()
  local smap = monoform_getStructMap()
  local n=getStructureCount()
  local slist = {}
  for i=0, n-1 do
    local s1 = getStructure(i)
    local s2 = smap[s1.Name]
    if s1 ~= s2 then
       slist[s1.Name] = s1
    end
  end
  local name
  local s
  for name, s in pairs(slist) do
    print(translate("Removing ")..name)
    structure_removeFromGlobalStructureList(s)
  end
end

function mono_reloadGlobalStructures(imagename)
  local smap = monoform_getStructMap()
  local classmap = {}
  local staticmap = {}
  local arraymap = {}
  local imageclasses = {}
  
  local i, j
  local fqclass, caddr
  local assemblies=mono_enumAssemblies()
  for i=1, #assemblies do
    local image=mono_getImageFromAssembly(assemblies[i])
    local iname=mono_image_get_name(image)
    if imagename==nil or imagename==iname then
      local classes=mono_image_enumClasses(image)
      
      -- purge classes
      for j=1, #classes do
        local fqclass = monoform_getfqclassname(classes[j].class, false)
        local s = smap[fqclass]
        if s ~= nil then
          structure_removeFromGlobalStructureList(s)
          classmap[fqclass] = classes[j].class
        end
        s = smap[fqclass..'[]']
        if s ~= nil then
          structure_removeFromGlobalStructureList(s)
          arraymap[fqclass..'[]'] = classes[j].class
        end
        -- check for static section
        fqclass = fqclass..'.Static'
        s = smap[fqclass]
        if s ~= nil then
          structure_removeFromGlobalStructureList(s)
          staticmap[fqclass] = classes[j].class
        end
      end
      
      -- if order function given, sort by it by passing the table and keys a, b, otherwise just sort the keys 
      local spairs = function(t, order)
          local keys = {}
          for k in pairs(t) do keys[#keys+1] = k end
          if order then
              table.sort(keys, function(a,b) return order(t, a, b) end)
          else
              table.sort(keys)
          end
          local i = 0
          return function() -- return the iterator function
              i = i + 1
              if keys[i] then
                  return keys[i], t[keys[i]]
              end
          end
      end
      local merge=function(...)
          local i,k,v
          local result={}
          i=1
          while true do
              local args = select(i,...)
              if args==nil then break end
              for k,v in pairs(args) do result[k]=v end
              i=i+1
          end
          return result
      end
      for fqclass, caddr in spairs(merge(classmap, arraymap, staticmap)) do
        s = createStructure(fqclass)
        structure_addToGlobalStructureList(s)
        smap[fqclass] = s
      end
    end
  end
  for fqclass, caddr in pairs(classmap) do
    print(translate("Reloading Structure ")..fqclass)
    monoform_exportStruct(caddr, fqclass, true, false, smap, false, true)
  end
  for fqclass, caddr in pairs(arraymap) do
    print(translate("Reloading Structure ")..fqclass)
    monoform_exportArrayStruct(nil, caddr, fqclass, true, false, smap, false, true)
  end
  for fqclass, caddr in pairs(staticmap) do
    print(translate("Reloading Structure ")..fqclass)
    monoform_exportStruct(caddr, fqclass, true, true, smap, false, true)
  end
end


function monoform_escapename(value)
  if value~=nil then
    return value:gsub("([^A-Za-z0-9%+%.,_$`<>%[%]])", "")
  end
  return nil
end

function monoform_getfqclassname(caddr, static)
  if (caddr==nil or caddr==0) then return nil end
  --local classname=mono_class_getName(caddr)
  --local namespace=mono_class_getNamespace(caddr)
  local classname=mono_class_getFullName(caddr)
  local namespace=nil
  local fqclass = monoform_escapename(classname)
  if fqclass==nil or string.len(fqclass) == 0 then
    return nil
  end
  if namespace~=nil and string.len(namespace) ~= 0 then
    fqclass = namespace.."."..fqclass
  end
  if static then
     fqclass = fqclass..".Static"
  end
  return fqclass
end

function monoform_exportStruct(caddr, typename, recursive, static, structmap, makeglobal, reload)
  --print('monoform_exportStruct')
  local fqclass = monoform_getfqclassname(caddr, static)
  if typename==nil then
    typename = fqclass
  end
  if typename == nil then
    return nil
  end
  -- check if existing. exit early if already present
  local s = structmap[typename]
  if s == nil then
    -- print("Creating Structure "..typename)
    s = createStructure(typename)
    structmap[typename] = s  
    if makeglobal then 
      structure_addToGlobalStructureList(s)
    end
  else
    if not reload==true then 
      return s
    end
    -- TODO: cannot clear fields here but would like to
  end
  makeglobal = false
  return monoform_exportStructInternal(s, caddr, recursive, static, structmap, makeglobal)
end

mono_StringStruct=nil
  
function monofrom_addPointerStructure(parentstructure, thiselement, field, recursive, static, structmap, loopnumber)
  --This function will add sub-strutures to the fields that are c Pointers
  --to disable, set " monoSettings.Value["MaxPointerChildStructs"] = "" "
  assert(field.monotype==MONO_TYPE_PTR, 'Error: WAIT! How did I end up here!?')
  local kls = mono_field_getClass(field.field)
  if not kls or not readPointer(kls) then return end
  kls = mono_class_get_type(kls)
  kls = mono_type_get_ptr_type(kls)
  if not kls or not readPointer(kls) then return end
  local pat = mono_type_get_type(kls)
  kls = mono_type_get_class(kls)
  kls = pat==MONO_TYPE_GENERICINST and mono_class_getParent(kls) or kls
  if not kls or not readPointer(kls) then return end
  local subflds = mono_class_enumFields(kls,1)
  if #subflds==0 then return end
  local subofst; -- the offset of very first non-static, non-const field needs to be subtracted from similar fields
  for k,v in ipairs(subflds) do
    if not v.isStatic and not v.isConst then
      subofst = subofst or v.offset
      break
    end
  end
  local structure = createStructure("")
  monoform_exportStructInternal(structure, kls, recursive, static, structmap, nil, subofst, loopnumber-1)
  print(structure.Count, field.name)
  if structure.Count > 0 then
    thiselement.ChildStruct = structure
    thiselement.ChildStructStart = 0
  else
    structure.destroy()
  end
  --print(#subflds, subflds[1].offset,mono_class_getFullName(kls), mono_class_getFullName(mono_field_getClass(field.field)))
end

function monoform_exportStructInternal(s, caddr, recursive, static, structmap, makeglobal, minusoffset, loopnumber)
  --print("monoform_exportStructInternal")
  if not(tonumber(monoSettings.Value["MaxPointerChildStructs"])) then
    monoSettings.Value["MaxPointerChildStructs"] = "2"
  end

  if (monopipe==nil) or (caddr==0) or (caddr==nil) then return nil end

  local className = mono_class_getFullName(caddr)
  --print('Populating '..className)

  if string.sub(className,-2)=='[]' then
    local elemtype = mono_class_getArrayElementClass(caddr)
    return monoform_exportArrayStructInternal(s, caddr, elemtype, recursive, structmap, makeglobal, true)
  end
  minusoffset = minusoffset or 0

  local hasStatic = false
  structure_beginUpdate(s)

  local fields=mono_class_enumFields(caddr,true,true)
  local str -- string struct
  local childstructs = {}
  local i
  --print(#fields)
  for i=1, #fields do
    hasStatic = hasStatic or fields[i].isStatic

    if fields[i].isStatic==static and not fields[i].isConst then
      local e=s.addElement()
      local ft = fields[i].monotype
      local fieldname = monoform_escapename(fields[i].name)
      if fieldname~=nil then
        e.Name=fieldname
      end
      e.Offset=fields[i].offset - minusoffset
      e.Vartype=mono_class_isEnum(mono_field_getClass( fields[i].field )) and vtDword or monoTypeToVarType(ft)
      --print(string.format("  Field: %d: %d: %d: %s", e.Offset, e.Vartype, ft, fieldname))

      loopnumber = loopnumber or tonumber(monoSettings.Value["MaxPointerChildStructs"])
      if ft==MONO_TYPE_STRING or ft==MONO_TYPE_CHAR then
        --e.Vartype=vtUnicodeString
        e.Bytesize = 999
      elseif ft == MONO_TYPE_PTR and loopnumber > 0 then
        monofrom_addPointerStructure(s, e, fields[i], recursive, static, structmap, loopnumber)
--[[        e.Vartype=vtPointer
--print(string.format("  Field: %d: %d: %d: %s", e.Offset, e.Vartype, ft, fieldname))

         if mono_StringStruct==nil then
         --  print("Creating string object")

           mono_StringStruct = createStructure("String")

           mono_StringStruct.beginUpdate()
           local ce=mono_StringStruct.addElement()
           ce.Name="Length"
           if targetIs64Bit() then
             ce.Offset=0x10
       else
       ce.Offset=0x8
       end

           ce.Vartype=vtDword
           ce=mono_StringStruct.addElement()
           ce.Name="Value"
           if targetIs64Bit() then
             ce.Offset=0x14
           else
             ce.Offset=0xC
           end
           ce.Vartype=vtUnicodeString
           ce.Bytesize=128
           mono_StringStruct.endUpdate()
           --mono_StringStruct.addToGlobalStructureList()
         end
         e.setChildStruct(mono_StringStruct)

      elseif ft == MONO_TYPE_PTR or ft == MONO_TYPE_CLASS or ft == MONO_TYPE_BYREF
          or ft == MONO_TYPE_GENERICINST then
        --print("bla")
        local typename = monoform_escapename(fields[i].typename)
        if typename ~= nil then
          local typeval = mono_field_getClass(fields[i].field)
          --print(string.format("PTR: %X: %s", typeval, typename))
          cs = monoform_exportStruct(typeval, typename, recursive, false, structmap, makeglobal)
          if cs~=nil then e.setChildStruct(cs) end
        end
      elseif ft == MONO_TYPE_SZARRAY then
        --print("bla2")
        local typename = monoform_escapename(fields[i].typename)
        local arraytype = mono_field_getClass(fields[i].field)
        local elemtype = mono_class_getArrayElementClass(arraytype)
  --print(typename)

        --local acs = monoform_exportArrayStruct(arraytype, elemtype, typename, recursive, static, structmap, makeglobal, false)
        --if acs~=nil then e.setChildStruct(acs) end --]]
      end

    end
  end

  structure_endUpdate(s)
  return s
end

function monoform_exportArrayStruct(arraytype, elemtype, typename, recursive, static, structmap, makeglobal, reload)
  local acs=nil
  if typename~=nil then
    acs = structmap[typename]
    if acs==nil and arraytype~=nil then
      acs = monoform_exportStruct(arraytype, typename, recursive, false, structmap, makeglobal)
      reload = true
    end
  end
  return monoform_exportArrayStructInternal(acs, arraytype, elemtype, recursive, structmap, makeglobal, reload)  
end

function mono_structfields_getStartOffset(fields)
  --this function get the first non-static, non-const field and gets its offset to subtract from all the offsets of fields
  --this is done since structs as a memeber element in a class are not pointers, rather simple values!
  for k,v in pairs(fields) do
    if not(v.isConst) and not(v.isStatic) then
      return v.offset
    end
  end
end

function monoform_addCSStructElements(structure, klass, parentstructname, offsetInStructure, prename, postname, preklassName, postClassName)
  parentstructname = type(parentstructname)=='string' and #parentstructname>0 and parentstructname..'.' or ''
  offsetInStructure = tonumber(offsetInStructure) or 0 --for arrays of the same struct
  prename = prename or "" --the text to add before the name of the element
  postname = postname or "" --text to add after the name of the element
  preklassName = preklassName or "" --the text to add before the klassname (in paranthesis) of the element
  postklassName = postklassName or "" --text to add after the klassname (in paranthesis) of the element

  local subfield = mono_class_enumFields(klass)
  local suboffset = mono_structfields_getStartOffset(subfield)
  if not suboffset then
    suboffset = targetIs64Bit() and 0x10 or 0x8
  end
  for k,v in pairs(subfield) do
    if not(v.isConst) and not(v.isStatic) then
      local fieldClass = mono_field_getClass( v.field )
      local klsname = mono_class_getName(fieldClass)
      local eloffset = offsetInStructure+v.offset-suboffset
      if mono_class_isStruct(fieldClass) then
        monoform_addCSStructElements(structure, fieldClass, v.name, eloffset, prename, postname, klsname..'.')
      else
        local nm = v.name..'('..preklassName..klsname..postklassName..')'
        local ce=structure.addElement()
        ce.Name=string.format("%s%s%s",prename,parentstructname..nm,postname)
        ce.Offset=eloffset
        ce.Vartype= mono_class_isEnum(fieldClass) and vtDword or monoTypeToVarType( v.monotype ) --vtPointer
        if ce.Vartype == vtDword then
          ce.DisplayMethod = 'dtSignedInteger'
        end
      end
    end
  end
end

function monoform_exportArrayStructInternal(acs, arraytype, elemtype, recursive, structmap, makeglobal, reload)
  --print("monoform_exportArrayStructInternal")
  --print(fu(arraytype),mono_class_getFullName(arraytype))
  if acs~=nil then
    cs = monoform_exportStruct(elemtype, nil, recursive, false, structmap, makeglobal)
    if cs~=nil and reload then
      structure_beginUpdate(acs)
      local ce=acs.addElement()
      ce.Name='Count'
      if targetIs64Bit() then
        ce.Offset=0x18
      else
        ce.Offset=0xC
      end
      ce.Vartype=vtDword
      ce.setChildStruct(cs)

      local j
      local psize = arraytype and mono_array_element_size(arraytype) or nil
      psize = psize and psize or (targetIs64Bit() and 8 or 4)

      local start
      if targetIs64Bit() then
        start=0x20
      else
        start=0x10
      end
      local elementkls = mono_class_getArrayElementClass(arraytype)
      local elementmonotype = mono_type_get_type(mono_class_get_type(elementkls))
      local isStruct = mono_class_isStruct(elementkls)--mono_class_isValueType(elementkls) and not(mono_class_IsPrimitive(elementkls)) and not(mono_class_isEnum(elementkls))
      --print(fu(elementkls),mono_class_getFullName(elementkls),fu(elementmonotype))
      if isStruct  then
         --print("yep, a struct")
         for j=0, 9 do -- Arbitrarily add 10 elements
           monoform_addCSStructElements(acs, elementkls, "", j*psize+start, '['..j..']', "")
         end
      else
        for j=0, 9 do -- Arbitrarily add 10 elements
          ce=acs.addElement()
          ce.Name=string.format("[%d]%s",j,mono_class_getName(elementkls))
          ce.Offset=j*psize+start
          ce.Vartype= monoTypeToVarType( elementmonotype ) --vtPointer
    if ce.Vartype == vtDword then
      ce.DisplayMethod = 'dtSignedInteger'
    end
        end
      end
      structure_endUpdate(acs)
    end
  end
  return acs
end

function monoAA_GETMONOSTRUCT(parameters, syntaxcheckonly)
  --called whenever an auto assembler script encounters the GETMONOSTRUCT() line

  --parameters: classname or classname,namespace:classname  (or classname,classname)

  --turns into a struct define

  local c,name,classname,namespace

  c=string.find(parameters,",")
  if c==nil then
    --just find this class
    name=parameters
    classname=parameters
    namespace=''
    --print("Format 1")
    --print("name="..name)
    --print("classname="..classname)
    --print("namespace="..namespace)

  else
    --this is a name,namespace:classname notation
    --print("Format 2")

    name=string.sub(parameters, 1, c-1)
    parameters=string.sub(parameters, c+1, #parameters)


    c=string.find(parameters,":")
    if (c~=nil) then
      namespace=string.sub(parameters, 1,c-1)

      classname=string.sub(parameters, c+1, #parameters)
    else
      namespace='';
      classname=parameters
    end

    --print("name="..name)
    --print("classname="..classname)
    --print("namespace="..namespace)

  end

  name=name:match "^%s*(.-)%s*$"
  classname=classname:match "^%s*(.-)%s*$"
  namespace=namespace:match "^%s*(.-)%s*$"

  local class=mono_findClass(namespace, classname)
  if (class==nil) or (class==0) then
    return nil,translate("The class ")..namespace..":"..classname..translate(" could not be found")
  end

  local fields=mono_class_enumFields(class)
  if (fields==nil) or (#fields==0) then
    return nil,namespace..":"..classname..translate(" has no fields")
  end


  local offsets={}
  local i
  for i=1, #fields do
    if (fields[i].offset~=0) and (not fields[i].isStatic) then
      offsets[fields[i].offset]=fields[i].name
    end
  end

  local sortedindex={}
  for c in pairs(offsets) do
    table.insert(sortedindex, c)
  end
  table.sort(sortedindex)

  local result="struct "..name.."\n"
  local fieldsize

  if #sortedindex>0 then
    fieldsize=sortedindex[1]-0;

    result=result.."vtable: resb "..fieldsize
  end

  result=result.."\n"


  for i=1, #sortedindex do
    local offset=sortedindex[i]



    local name=offsets[offset]
    result=result..name..": "
    if sortedindex[i+1]~=nil then
      fieldsize=sortedindex[i+1]-offset
    else
      --print("last one")
      fieldsize=1 --last one
    end

    result=result.." resb "..fieldsize.."\n"

  end  

  result=result.."ends\n"

  --showMessage(result)

  return result
end

function monoAA_GETMONOSTATICDATA(assemblyname, namespace, classname, symbolprefix, enable)
  --parameters: assemblyname = partial name match of assembly
  --            namespace = namespace of class (empty string if no namespace)
  --            classname = name of class
  --            symbolprefix = name of symbol prefix (sanitized classname used if nil)

  -- returns AA script for locating static data location for given structure
  if monopipe.il2cpp then return end
  
  local SYMCLASSNAME
  if assemblyname==nil or namespace==nil or classname==nil then
    return ''
  end
  if symbolprefix~=nil then
    SYMCLASSNAME = symbolprefix:gsub("[^A-Za-z0-9._]", "")
  else
    SYMCLASSNAME = classname:gsub("[^A-Za-z0-9._]", "")
  end
  -- Populates ###.Static and ###.Class where ### the symbol prefix
  local script_tmpl
  if enable then
    if targetIs64Bit() then
      script_tmpl = [===[
label($SYMCLASSNAME$.threadexit)
label(classname)
label(namespace)
label(assemblyname)
label(status)
label(domain)
label(assembly)
label($SYMCLASSNAME$.Static)
label($SYMCLASSNAME$.Class)
alloc($SYMCLASSNAME$.threadstart, 2048, mono.mono_thread_attach)

registersymbol($SYMCLASSNAME$.Static)
registersymbol($SYMCLASSNAME$.Class)

$SYMCLASSNAME$.threadstart:
sub rsp,28

xor rax,rax
mov [$SYMCLASSNAME$.Class],rax
mov [$SYMCLASSNAME$.Static],rax

call mono.mono_get_root_domain
cmp rax,0
je $SYMCLASSNAME$.threadexit
mov [domain],rax

mov rcx,[domain]
call mono.mono_thread_attach

mov rcx,assemblyname
mov rdx,status
call mono.mono_assembly_load_with_partial_name
cmp rax,0
je $SYMCLASSNAME$.threadexit

mov rcx,rax
call mono.mono_assembly_get_image
cmp rax,0
je $SYMCLASSNAME$.threadexit
mov [assembly], rax

mov rcx,eax
mov rdx,namespace
mov r8,classname

call mono.mono_class_from_name_case
cmp rax,0
je $SYMCLASSNAME$.threadexit
mov [$SYMCLASSNAME$.Class],rax

mov rcx,[domain]
mov rdx,rax
call mono.mono_class_vtable
cmp rax,0
je $SYMCLASSNAME$.threadexit

mov rcx,rax
call mono.mono_vtable_get_static_field_data

mov [$SYMCLASSNAME$.Static],rax
jmp $SYMCLASSNAME$.threadexit
///////////////////////////////////////////////////////
// Data section
$SYMCLASSNAME$.Static:
dq 0
$SYMCLASSNAME$.Class:
dq 0
assemblyname:
db '$ASSEMBLYNAME$',0
namespace:
db '$NAMESPACE$',0
classname:
db '$CLASSNAME$',0
status:
dq 0
domain:
dq 0
assembly:
dq 0

$SYMCLASSNAME$.threadexit:
add rsp,28
ret
createthread($SYMCLASSNAME$.threadstart)
]===]
    else
      script_tmpl = [===[
label($SYMCLASSNAME$.threadexit)
label(classname)
label(namespace)
label(assemblyname)
label(status)
label(domain)
label(assembly)
label($SYMCLASSNAME$.Static)
label($SYMCLASSNAME$.Class)
alloc($SYMCLASSNAME$.threadstart, 2048)

registersymbol($SYMCLASSNAME$.Static)
registersymbol($SYMCLASSNAME$.Class)

$SYMCLASSNAME$.threadstart:
mov [$SYMCLASSNAME$.Class],0
mov [$SYMCLASSNAME$.Static],0

call mono.mono_get_root_domain
cmp eax,0
je $SYMCLASSNAME$.threadexit
mov [domain],eax

push [domain]
call mono.mono_thread_attach
add esp,4

push status
push assemblyname
call mono.mono_assembly_load_with_partial_name
add esp,8
cmp eax,0
je $SYMCLASSNAME$.threadexit

push eax
call mono.mono_assembly_get_image
add esp,4
cmp eax,0
je $SYMCLASSNAME$.threadexit
mov [assembly], eax

push classname
push namespace
push eax
call mono.mono_class_from_name_case
add esp,C
cmp eax,0
je $SYMCLASSNAME$.threadexit
mov [$SYMCLASSNAME$.Class],eax

push eax
push [domain]
call mono.mono_class_vtable
add esp,8
cmp eax,0
je $SYMCLASSNAME$.threadexit

push eax
call mono.mono_vtable_get_static_field_data
add esp,4
mov [$SYMCLASSNAME$.Static],eax
jmp $SYMCLASSNAME$.threadexit
///////////////////////////////////////////////////////
// Data section
$SYMCLASSNAME$.Static:
dd 0
$SYMCLASSNAME$.Class:
dd 0
assemblyname:
db '$ASSEMBLYNAME$',0
namespace:
db '$NAMESPACE$',0
classname:
db '$CLASSNAME$',0
status:
dd 0
domain:
dd 0
assembly:
dd 0
$SYMCLASSNAME$.threadexit:
ret
createthread($SYMCLASSNAME$.threadstart)
]===]
    end
  else
    script_tmpl = [===[
unregistersymbol($SYMCLASSNAME$.Static)
unregistersymbol($SYMCLASSNAME$.Class)
dealloc($SYMCLASSNAME$.threadstart)
]===]
  end
  return script_tmpl
         :gsub('($CLASSNAME$)', classname)
         :gsub('($SYMCLASSNAME$)', SYMCLASSNAME)
         :gsub('($NAMESPACE$)', namespace)
         :gsub('($ASSEMBLYNAME$)', assemblyname)
end

function monoAA_GETMONOSTATICFIELDDATA(assemblyname, namespace, classname, fieldname, symbolprefix, enable)
  --parameters: assemblyname = partial name match of assembly
  --            namespace = namespace of class (empty string if no namespace)
  --            classname = name of class
  --            fieldname = name of field
  --            symbolprefix = name of symbol prefix (sanitized classname used if nil)

  -- returns AA script for locating static data location for given structure
  if monopipe.il2cpp then return end  
  
  local SYMCLASSNAME
  if assemblyname==nil or namespace==nil or classname==nil or fieldname==nil then
    return ''
  end
  if symbolprefix~=nil then
    SYMCLASSNAME = symbolprefix:gsub("[^A-Za-z0-9._]", "")
  else
    SYMCLASSNAME = classname:gsub("[^A-Za-z0-9._]", "")
  end
  local SYMFIELDNAME = fieldname:gsub("[^A-Za-z0-9._]", "")
  
  -- Populates ###.Static and ###.Class where ### the symbol prefix
  local script_tmpl
  if enable then
    script_tmpl = [===[
label(classname)
label(namespace)
label(assemblyname)
label(fieldname)
label(status)
label(domain)
label(assembly)
label(field)
label($SYMCLASSNAME$.$SYMFIELDNAME$)
label($SYMCLASSNAME$.$SYMFIELDNAME$.threadexit)
alloc($SYMCLASSNAME$.$SYMFIELDNAME$.threadstart, 2048)

registersymbol($SYMCLASSNAME$.$SYMFIELDNAME$)

$SYMCLASSNAME$.$SYMFIELDNAME$.threadstart:
mov [$SYMCLASSNAME$.$SYMFIELDNAME$],0

call mono.mono_get_root_domain
cmp eax,0
je $SYMCLASSNAME$.$SYMFIELDNAME$.threadexit
mov [domain],eax

push [domain]
call mono.mono_thread_attach
add esp,4

push status
push assemblyname
call mono.mono_assembly_load_with_partial_name
add esp,8
cmp eax,0
je $SYMCLASSNAME$.$SYMFIELDNAME$.threadexit

push eax
call mono.mono_assembly_get_image
add esp,4
cmp eax,0
je $SYMCLASSNAME$.$SYMFIELDNAME$.threadexit
mov [assembly], eax

push classname
push namespace
push eax
call mono.mono_class_from_name_case
add esp,C
cmp eax,0
je $SYMCLASSNAME$.$SYMFIELDNAME$.threadexit
push fieldname
push eax
call mono.mono_class_get_field_from_name
add esp,8
cmp eax,0
je $SYMCLASSNAME$.$SYMFIELDNAME$.threadexit
mov [field], eax
push eax
call mono.mono_field_get_parent
add esp,4
cmp eax,0
je $SYMCLASSNAME$.$SYMFIELDNAME$.threadexit
push eax
push [domain]
call mono.mono_class_vtable
add esp,8
cmp eax,0
je $SYMCLASSNAME$.$SYMFIELDNAME$.threadexit
push eax
call mono.mono_vtable_get_static_field_data
add esp,4
cmp eax,0
je $SYMCLASSNAME$.$SYMFIELDNAME$.threadexit
push eax // save data on stack
push [field]
call mono.mono_field_get_offset
add esp,4
pop ebx // restore data
add eax,ebx
mov [$SYMCLASSNAME$.$SYMFIELDNAME$],eax
jmp $SYMCLASSNAME$.$SYMFIELDNAME$.threadexit
///////////////////////////////////////////////////////
// Data section
$SYMCLASSNAME$.$SYMFIELDNAME$:
dd 0
assemblyname:
db '$ASSEMBLYNAME$',0
namespace:
db '$NAMESPACE$',0
classname:
db '$CLASSNAME$',0
fieldname:
db '$FIELDNAME$',0
status:
dd 0
domain:
dd 0
assembly:
dd 0
field:
dd 0
$SYMCLASSNAME$.$SYMFIELDNAME$.threadexit:
ret
createthread($SYMCLASSNAME$.$SYMFIELDNAME$.threadstart)
]===]
  else
    script_tmpl = [===[
unregistersymbol($SYMCLASSNAME$.$SYMFIELDNAME$)
dealloc($SYMCLASSNAME$.$SYMFIELDNAME$.threadstart)
]===]
  end
  return script_tmpl
         :gsub('($CLASSNAME$)', classname)
         :gsub('($SYMCLASSNAME$)', SYMCLASSNAME)
         :gsub('($FIELDNAME$)', fieldname)
         :gsub('($SYMFIELDNAME$)', SYMFIELDNAME)
         :gsub('($NAMESPACE$)', namespace)
         :gsub('($ASSEMBLYNAME$)', assemblyname)
end

function mono_initialize()
  --register a function to be called when a process is opened
  monoSettings=getSettings("MonoExtension")  
  
  if (mono_init1==nil) then
    mono_init1=true
    mono_OldOnProcessOpened=MainForm.OnProcessOpened
    MainForm.OnProcessOpened=mono_OnProcessOpened

    registerAutoAssemblerCommand("USEMONO", monoAA_USEMONO)
    registerAutoAssemblerCommand("FINDMONOMETHOD", monoAA_FINDMONOMETHOD)
    registerAutoAssemblerCommand("GETMONOSTRUCT", monoAA_GETMONOSTRUCT)
    registerAutoAssemblerCommand("FINDMONOMETHODWITHPARAMS", monoAA_FINDMONOMETHODWITHPARAMS)

    registerEXETrainerFeature('Mono', function()
      local r={}
      r[1]={}
      r[1].PathToFile=getAutorunPath()..'monoscript.lua'
      r[1].RelativePath=[[autorun\]]

      r[2]={}
      r[2].PathToFile=getAutorunPath()..'forms'..pathsep..'MonoDataCollector.frm'
      r[2].RelativePath=[[autorun\]]..'forms'..pathsep

      if getOperatingSystem()==0 then
        r[3]={}
        r[3].PathToFile=getAutorunPath()..libfolder..pathsep..'MonoDataCollector32.dll'
        r[3].RelativePath=[[autorun\]]..libfolder..pathsep

        r[4]={}
        r[4].PathToFile=getAutorunPath()..libfolder..pathsep..'MonoDataCollector64.dll'
        r[4].RelativePath=[[autorun\]]..libfolder..pathsep
      else
        r[3]={}
        r[3].PathToFile=getAutorunPath()..libfolder..pathsep..'libMonoDataCollectorMac.dylib'
        r[3].RelativePath=[[autorun\]]..libfolder..pathsep
      end

      return r
    end)


  end
end


mono_initialize()



