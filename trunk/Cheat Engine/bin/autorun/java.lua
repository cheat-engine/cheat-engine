JAVACMD_STARTCODECALLBACKS=0
JAVACMD_STOPCODECALLBACKS=1
JAVACMD_GETLOADEDCLASSES=2
JAVACMD_DEREFERENCELOCALOBJECT=3
JAVACMD_GETCLASSMETHODS=4
JAVACMD_GETCLASSFIELDS=5
JAVACMD_GETIMPLEMENTEDINTERFACES=6
JAVAVMD_FINDREFERENCESTOOBJECT=7
JAVACMD_FINDJOBJECT=8
JAVACMD_GETCLASSSIGNATURE=9
JAVACMD_GETSUPERCLASS=10
JAVACMD_GETOBJECTCLASS=11
JAVACMD_GETCLASSDATA=12
JAVACMD_REDEFINECLASS=13
JAVACMD_FINDCLASS=14
JAVACMD_GETCAPABILITIES=15


JAVACODECMD_METHODLOAD=0
JAVACODECMD_METHODUNLOAD=1
JAVACODECMD_DYNAMICCODEGENERATED=2
JAVACODECMD_TERMINATED=255



JAVA_TIMEOUT=500000 --500 seconds



function getFieldFromType(type, field, infloopprotection)
  if type==nil then return nil end

  if infloopprotection==nil then
  infloopprotection=0
  else
    infloopprotection=infloopprotection+1
    if infloopprotection>20 then
      return nil
    end
  end

  type=type:gsub("<.->(.-)","<%1>") --replace the <xxx> part with <>
  local Struct=JavaStructs[type]

  if (Struct==nil) or (Struct[field]==nil) then
    return getFieldFromType(JavaTypes[type].Alternate, field, infloopprotection) --check the super type if that one has fields
  else
    return Struct[field]
  end

end

function getKlassFromObject(object)
  return readQword(object+getFieldFromType("oopDesc","_metadata._klass").Offset)+JavaTypes["oopDesc"].Size
end

function CollectJavaSymbolsNonInjected(thread)
  if thread~=nil then
    thread.name="CollectJavaSymbolsNonInjected"
  end

  JavaStructs={}
  JavaTypes={}

  local s,s2


  s=readPointer("jvm.gHotSpotVMStructs")
  if (s==nil) or (s==0) then
    return --invalid JVM
  end

  VMStructEntryTypeNameOffset=readInteger("jvm.gHotSpotVMStructEntryTypeNameOffset")
  VMStructEntryFieldNameOffset=readInteger("jvm.gHotSpotVMStructEntryFieldNameOffset")
  VMStructEntryTypestringOffset=readInteger("jvm.gHotSpotVMStructEntryTypestringOffset")
  VMStructEntryIsStaticOffset=readInteger("jvm.gHotSpotVMStructEntryIsStaticOffset")
  VMStructEntryOffsetOffset=readInteger("jvm.gHotSpotVMStructEntryOffsetOffset")
  VMStructEntryAddressOffset=readInteger("jvm.gHotSpotVMStructEntryAddressOffset")
  VMStructEntryArrayStride=readInteger("jvm.gHotSpotVMStructEntryArrayStride")







--[[
  const char* typeName;            // The type name containing the given field (example: "Klass")
  const char* fieldName;           // The field name within the type           (example: "_name")
  const char* Typestring;          // Quoted name of the type of this field (example: "Symbol*";
                                   // parsed in Java to ensure type correctness
  int32_t  isStatic;               // Indicates whether following field is an offset or an address
  uint64_t offset;                 // Offset of field within structure; only used for nonstatic fields
  void* address;                   // Address of field; only used for static fields
                                   // ("offset" can not be reused because of apparent SparcWorks compiler bug
                                   // in generation of initializer data)
--]]




  while readString(readQword(s+VMStructEntryTypeNameOffset))~=nil do
    local a,b,c,d;
    a=readString(readPointer(s+VMStructEntryTypeNameOffset),255)
    b=readString(readPointer(s+VMStructEntryFieldNameOffset),255)
    c=readString(readPointer(s+VMStructEntryTypestringOffset),255)

    d=readPointer(s+VMStructEntryIsStaticOffset)

    if a and b and c then
      if JavaStructs[a]==nil then
        JavaStructs[a]={}
      end

      JavaStructs[a][b]={}
      JavaStructs[a][b].Typestring=c

      if d==0 then
        JavaStructs[a][b].Offset=readPointer(s+VMStructEntryOffsetOffset)
      else
        JavaStructs[a][b].Address=readPointer(s+VMStructEntryAddressOffset)
      end



      if d~=0 then
        print(a.."  -  "..b.."  -  "..c.."  :  "..string.format("%x  ( %x )",readPointer(s+VMStructEntryAddressOffset), readPointer(readPointer(s+VMStructEntryAddressOffset)) ))
      else
        print(a.."  -  "..b.."  -  "..c.."  :  "..string.format("%x",readPointer(s+VMStructEntryOffsetOffset)))
      end
    end
    s=s+VMStructEntryArrayStride
  end



  --print("--------------------------------------------------------------------------------")


  s2=readPointer("jvm.gHotSpotVMTypes")
  VMTypeEntryTypeNameOffset=readInteger("jvm.gHotSpotVMTypeEntryTypeNameOffset")
  VMTypeEntrySuperclassNameOffset=readInteger("jvm.gHotSpotVMTypeEntrySuperclassNameOffset")
  VMTypeEntryIsOopTypeOffset=readInteger("jvm.gHotSpotVMTypeEntryIsOopTypeOffset")
  VMTypeEntryIsIntegerTypeOffset=readInteger("jvm.gHotSpotVMTypeEntryIsIntegerTypeOffset")
  VMTypeEntryIsUnsignedOffset=readInteger("jvm.gHotSpotVMTypeEntryIsUnsignedOffset")
  VMTypeEntrySizeOffset=readInteger("jvm.gHotSpotVMTypeEntrySizeOffset")
  VMTypeEntryArrayStride=readInteger("jvm.gHotSpotVMTypeEntryArrayStride")





  while readString(readPointer(s2+VMTypeEntryTypeNameOffset))~=nil do
    local a,b,isInteger, isOop, size;
    a=readString(readPointer(s2+VMTypeEntryTypeNameOffset),255)
    b=readString(readPointer(s2+VMTypeEntrySuperclassNameOffset),255)


    isOop=readInteger(s2+VMTypeEntryIsOopTypeOffset)
    isInteger=readInteger(s2+VMTypeEntryIsIntegerTypeOffset)
    size=readInteger(s2+VMTypeEntrySizeOffset)


    if a then
      local _a,_b
      _a=a:gsub("<.->(.-)","<%1>")
      JavaTypes[_a]={}
      JavaTypes[_a].Size=size

      if b then
        _b=b:gsub("<.->(.-)","<%1>")
        JavaTypes[_a].Alternate=_b
      end

    end



    local r=''
    if a then
      r=r..a
    end

    if b then
      r=r.."  -  "..b
    end


    --print(r.."  (size="..size..")")
    s2=s2+VMTypeEntryArrayStride
  end


 -- print("-------------------------------------------------")
  s=readPointer("jvm.gHotSpotVMIntConstants")
  VMIntConstantEntryNameOffset=readInteger("jvm.gHotSpotVMIntConstantEntryNameOffset")
  VMIntConstantEntryValueOffset=readInteger("jvm.gHotSpotVMIntConstantEntryValueOffset")
  VMIntConstantEntryArrayStride=readInteger("jvm.gHotSpotVMIntConstantEntryArrayStride")


  while readString(readPointer(s+VMIntConstantEntryNameOffset))~=nil do
    local name,value
    name=readString(readPointer(s+VMIntConstantEntryNameOffset))
    value=readInteger(s+VMIntConstantEntryValueOffset)

    --print(name.."="..string.format("%x",value))

    s=s+VMIntConstantEntryArrayStride
  end

  --print("-------------------------------------------------")
  s=readPointer("jvm.gHotSpotVMLongConstants")
  VMLongConstantEntryNameOffset=readInteger("jvm.gHotSpotVMLongConstantEntryNameOffset")
  VMLongConstantEntryValueOffset=readInteger("jvm.gHotSpotVMLongConstantEntryValueOffset")
  VMLongConstantEntryArrayStride=readInteger("jvm.gHotSpotVMLongConstantEntryArrayStride")

  while readString(readPointer(s+VMLongConstantEntryNameOffset))~=nil do
    local name,value
    name=readString(readPointer(s+VMLongConstantEntryNameOffset))
    value=readQword(s+VMLongConstantEntryValueOffset)

    --print(name.."="..string.format("%x",value))

    s=s+VMLongConstantEntryArrayStride
  end


  --Fetch the interpreter functions
  local InterpreterFunctionList=getFieldFromType('AbstractInterpreter', '_code').Address
  local BufferOffset=getFieldFromType('StubQueue', '_stub_buffer').Offset
  local QueueEndOffset=getFieldFromType('StubQueue', '_queue_end').Offset

  local InterpreterCodeletSizeOffset=getFieldFromType('InterpreterCodelet', '_size').Offset
  local InterpreterCodeletDescriptionOffset=getFieldFromType('InterpreterCodelet', '_description').Offset
  local InterpreterCodeletHeaderSize=JavaTypes['InterpreterCodelet'].Size
  local InterpreterCodeletHeaderSizeAligned

  InterpreterCodeletHeaderSizeAligned=InterpreterCodeletHeaderSize

  if targetIs64Bit() then
    --increase InterpreterCodeletHeaderSizeAligned so it's dividable by 32
    if (InterpreterCodeletHeaderSizeAligned % 32)~=0 then
      InterpreterCodeletHeaderSizeAligned=(InterpreterCodeletHeaderSizeAligned+32) - (InterpreterCodeletHeaderSizeAligned % 32)
    end
  else
    --increase InterpreterCodeletHeaderSizeAligned so it's dividable by 16
    if (InterpreterCodeletHeaderSizeAligned % 16)~=0 then
      InterpreterCodeletHeaderSizeAligned=(InterpreterCodeletHeaderSizeAligned+16) - (InterpreterCodeletHeaderSizeAligned % 16)
    end
  end

  StubQueueAddress=readPointer(InterpreterFunctionList)
  BufferStart=readPointer(StubQueueAddress+BufferOffset)
  BufferEnd=BufferStart+readInteger(StubQueueAddress+QueueEndOffset)

  CurrentPos=BufferStart
  while (CurrentPos<BufferEnd) do
    local CodeletSize=readInteger(CurrentPos+InterpreterCodeletSizeOffset)
    local Description=readString(readInteger(CurrentPos+InterpreterCodeletDescriptionOffset))
    local Codestart=CurrentPos+InterpreterCodeletHeaderSizeAligned

    --print(string.format("%x = %s", Codestart, Description))

    JavaSymbols.addSymbol("","jInterpreter_"..Description,Codestart, CodeletSize-InterpreterCodeletHeaderSizeAligned)



    CurrentPos=CurrentPos+CodeletSize
  end

  JavaHotSpotFieldsLoaded=true
end


function javaInjectAgent()
  if (JavaSymbols==nil) then
    JavaSymbols=createSymbolList()
  else
    JavaSymbols.clear()
  end


  createNativeThread(CollectJavaSymbolsNonInjected)

  if (javapipe ~= nil) then
    javapipe.destroy()  --this will cause the pipe listener to destroy the java event server, which will stop the javaeventthread (so no need to wait for that)
	javapipe=nil
  end



  local alreadyinjected=false

  if javaInjectedProcesses==nil then
    javaInjectedProcesses={}

	local oldstate=errorOnLookupFailure(false)
	local address=getAddress('CEJVMTI64.dll')
	if (address~=nil) and (address~=0) then
	  javaInjectedProcesses[getOpenedProcessID()]=true
	  alreadyinjected=true
	  --opened a process with the JVMTI agent already running

	end

	errorOnLookupFailure(oldstate)

  else
    --check if already injected
	alreadyinjected=javaInjectedProcesses[getOpenedProcessID()]==true
  end





  if (alreadyinjected==false) then
	  autoAssemble([[
		globalalloc(bla,1024)

		globalalloc(cmd,16)
		globalalloc(arg0,256)
		globalalloc(arg1,256)
		globalalloc(arg2,256)
		globalalloc(result,4)

		globalalloc(pipename,256)

		cmd:
		db 'load',0

		arg0:
		db 'F:\svn\Cheat Engine\bin\autorun\dlls\CEJVMTI64',0

		arg1:
		db 0

		arg2:
		db 0

		pipename:
		db '\\.\pipe\cejavapipe',0


		bla:
		sub rsp,8
		sub rsp,30

		mov rcx,cmd
		mov rdx,arg0
		mov r8,arg1
		mov r9,arg2

		mov [rsp],cmd
		mov [rsp+8],arg0
		mov [rsp+10],arg1
		mov [rsp+18],arg2
		mov [rsp+20],pipename

		call jvm.JVM_EnqueueOperation
		mov [result],eax

		add rsp,38
		ret

		createthread(bla)
		]])


	javaInjectedProcesses[getOpenedProcessID()]=true

  end




  --wait till attached

  local timeout=getTickCount()+JAVA_TIMEOUT
  while (javapipe==nil) and (getTickCount()<timeout) do
    javapipe=connectToPipe('cejavadc_pid'..getOpenedProcessID())
  end

  if (javapipe==nil) then
    return 0 --failure
  end

  java_StartListeneningForEvents()

  JavaSymbols.register() --make these symbols available to all of cheat engine


  java.capabilities=java_getCapabilities()

  return 1;
end

function JavaEventListener(thread)
  if thread~=nil then
    thread.name="JavaEventListener"
  end

  --this code runs in another thread
  local EVENTCMD_METHODLOAD=0
  local EVENTCMD_METHODUNLOAD=1
  local EVENTCMD_DYNAMICCODEGENERATED=2

  local JavaEventPipe


  local timeout=getTickCount()+JAVA_TIMEOUT --5 seconds
  while (JavaEventPipe==nil) and (getTickCount()<timeout) do
    JavaEventPipe=connectToPipe('cejavaevents_pid'..getOpenedProcessID())
  end

  if (JavaEventPipe==nil) then
    return  --failure
  end


  while true do
    local command=JavaEventPipe.readByte()
	if command==EVENTCMD_METHODLOAD then  --methodload

	  local size1, size2, size3,ssize,classname, methodname, methodsig

	  local method=JavaEventPipe.readQword()
	  local code_size=JavaEventPipe.readDword()
	  local code_addr=JavaEventPipe.readQword()
	  size1=JavaEventPipe.readWord()
	  if (size1>0) then
	    classname=JavaEventPipe.readString(size1)
	  else
	    classname=''
	  end

	  size2=JavaEventPipe.readWord()
	  if (size2>0) then
	    methodname=JavaEventPipe.readString(size2)
	  else
	    methodname=''
	  end

	  size3=JavaEventPipe.readWord()
	  if (size3>0) then
        methodsig=JavaEventPipe.readString(size3)
	  else
	    methodsig=''
	  end

	  local endpos=classname:match'^.*();'
	  if endpos~=nil then
  	    classname=string.sub(classname,1,endpos-1)
	  end
	  local name=classname.."::"..methodname..methodsig


	  JavaSymbols.addSymbol("",classname.."::"..methodname,code_addr,code_size)

	  --print(string.format("s1=%d s2=%d s3=%d  (cn=%s  mn=%s  ms=%s)", size1,size2,size3, classname, methodname, methodsig))

	  --print(string.format("Methodload: %s -  (%x) %x-%x", name, method, code_addr, code_addr+code_size))


	  --
	elseif command==EVENTCMD_METHODUNLOAD then --methodunload
	  local method=JavaEventPipe.readQword()
	  local code_addr=JavaEventPipe.readQword()

	  print("EVENTCMD_METHODUNLOAD")
	  JavaSymbols.deleteSymbol(code_addr)
	  --
	elseif command==EVENTCMD_DYNAMICCODEGENERATED then --DynamicCodeGenerated
	  local ssize
	  local address=JavaEventPipe.readQword()
	  local length=JavaEventPipe.readDword()
	  ssize=JavaEventPipe.readWord()
	  local name=JavaEventPipe.readString(ssize)

	  --print(string.format("DynamicCode: %s  -  %x-%x", name, address, address+length))
	  JavaSymbols.addSymbol("",name,address,length)


	  --
	elseif command==JAVACODECMD_TERMINATED then
	  --print("eventserver terminated")
	  break
	elseif command==nil then
	  --print("Disconnected")
	  break
    else
	  print("Unexpected event received")  --synchronize isn't necesary for print as that function is designed to synchronize internally
      break --unknown command
	end
  end

  JavaEventPipe.destroy();
end

function java_getCapabilities()
  result={}
  javapipe.lock()
  javapipe.writeByte(JAVACMD_GETCAPABILITIES)

  result.can_access_local_variables=javapipe.readByte()
  result.can_generate_all_class_hook_events=javapipe.readByte()
  result.can_generate_breakpoint_events=javapipe.readByte()
  result.can_generate_compiled_method_load_events=javapipe.readByte()
  result.can_generate_field_access_events=javapipe.readByte()
  result.can_generate_field_modification_events=javapipe.readByte()
  result.can_generate_single_step_events=javapipe.readByte()
  result.can_get_bytecodes=javapipe.readByte()
  result.can_get_constant_pool=javapipe.readByte()
  result.can_maintain_original_method_order=javapipe.readByte()
  result.can_redefine_any_class=javapipe.readByte()
  result.can_redefine_classes=javapipe.readByte()
  result.can_retransform_any_class=javapipe.readByte()
  result.can_retransform_classes=javapipe.readByte()
  result.can_tag_objects=javapipe.readByte()

  javapipe.unlock()

  return result;
end


function java_StartListeneningForEvents()
  javapipe.lock();
  javapipe.writeByte(JAVACMD_STARTCODECALLBACKS)


  --the javapipe will now be frozen until a javaeventpipe makes an connection
  createNativeThread(JavaEventListener);

  javapipe.unlock();
end

function java_getLoadedClasses()
  javapipe.lock()
  javapipe.writeByte(JAVACMD_GETLOADEDCLASSES)

  local classcount=javapipe.readDword()
  local classes={}

  if (classcount==nil) then
    return nil
  end

  if classcount>0 then
    local i=0
	local length
	for i=1,classcount do
	  classes[i]={}
	  classes[i].jclass=javapipe.readQword()  --this is a pointer to a pointer to java.lang.class. To get the offset where klass is stored use getFieldFromType("java_lang_Class", "_klass_offset")  (The klass contains a _fields field which points to a array which contains the offset of the fields. Might be useful)
	  length=javapipe.readWord()
	  classes[i].signature=javapipe.readString(length);

	  length=javapipe.readWord()
	  classes[i].generic=javapipe.readString(length);
	end

  end


  javapipe.unlock()

  return classes
end

function java_dereferenceLocalObject(object)
  javapipe.lock()
  javapipe.writeByte(JAVACMD_DEREFERENCELOCALOBJECT)
  javapipe.writeQword(object)
  javapipe.unlock()
end

function java_cleanClasslist(classlist)
  local i
  for i=1, #classlist do
    java_dereferenceLocalObject(classlist[i].jclass)
  end
end

function java_getClassMethods(class)
  javapipe.lock()
  javapipe.writeByte(JAVACMD_GETCLASSMETHODS)
  javapipe.writeQword(class)
  local count=javapipe.readDword()
  local i
  local result={}
  local length
  for i=1,count do
    result[i]={}
    result[i].jmethodid=javapipe.readQword()

	length=javapipe.readWord()
	result[i].name=javapipe.readString(length)

	length=javapipe.readWord()
	result[i].signature=javapipe.readString(length)

	length=javapipe.readWord()
	result[i].generic=javapipe.readString(length)
  end
  javapipe.unlock()
  return result
end

function java_getClassFields(class)
  javapipe.lock()
  javapipe.writeByte(JAVACMD_GETCLASSFIELDS)
  javapipe.writeQword(class)
  local count=javapipe.readDword()
  local i
  local result={}
  local length
  for i=1,count do
    result[i]={}
    result[i].jfieldid=javapipe.readQword()

	length=javapipe.readWord()
	result[i].name=javapipe.readString(length)

	length=javapipe.readWord()
	result[i].signature=javapipe.readString(length)

	length=javapipe.readWord()
	result[i].generic=javapipe.readString(length)
  end
  javapipe.unlock()
  return result
end

function java_getImplementedInterfaces(class)
  result={}
  javapipe.lock()
  javapipe.writeByte(JAVACMD_GETIMPLEMENTEDINTERFACES)
  javapipe.writeQword(class)
  local count=javapipe.readDword()
  for i=1,count do
    result[i]=javapipe.readQword()
  end

  javapipe.unlock()
  return result
end



function java_findReferencesToObject(jObject)
  result={}
  local count=0
  javapipe.lock()
  javapipe.writeByte(JAVAVMD_FINDREFERENCESTOOBJECT)
  javapipe.writeQword(jObject)

  count=javapipe.readDword()
  local i
  for i=1, count do
    result[i]=javapipe.readQword();
  end


  javapipe.unlock()

  return result
end


function java_redefineClassWithCustomData(class, memory)
  javapipe.lock()
  javapipe.writeByte(JAVACMD_REDEFINECLASS)
  javapipe.writeQword(class)
  javapipe.writeDword(#memory)
  javapipe.writeString(memory)
  javapipe.unlock()
end

function java_redefineClassWithCustomClassFile(class, filename)
  local f=assert(io.open(filename,"rb"))
  local data = f:read("*all")
  f:close()
  java_redefineClassWithCustomData(class, data)
end

function java_getClassData(class)
  --gets the .class binary data (tip: Write a .class parser/editor so you can modify attributes and method bodies)
  local result={}
  javapipe.lock()
  javapipe.writeByte(JAVACMD_GETCLASSDATA)
  javapipe.writeQword(class)

  result.size=javapipe.readDword()
  if (result.size > 0) then
    result.data=javapipe.readString(result.size)
  end
  javapipe.unlock()

  return result.data
end

function java_writeClassToDisk(class, filename)
  local data=java_getClassData(class)
  local f=assert(io.open(filename,"wb"))
  f:write(data)
  f:close()
end



function java_invokeMethod()
end

function java_findClass(signature)
  local result=nil
  javapipe.lock()
  javapipe.writeByte(JAVACMD_FINDCLASS)
  javapipe.writeWord(#signature)
  javapipe.writeString(signature)
  result=javapipe.readQword()

  javapipe.unlock()
  return result
end

function java_setClassSearchPath()
end

function java_findAllObjectsFromClass(jClass)

end


function java_GetField(jObject, fieldid)
end

function java_SetField(jObject, fieldid, value)
end

function java_search_start()
  --tag all known objects and set a variable to let some functions know they can not function until the scan has finished (they can't set tags)

end

function java_search_getResults()
end

function java_search_findObjectsWithValue(value)
end

function java_search_stop()
end



function java_getObjectHandleToAddress(address)
  local result=0
  javapipe.lock()
  javapipe.writeByte(JAVACMD_FINDJOBJECT)
  javapipe.writeQword(address)

  result=javapipe.readQword()
  javapipe.unlock()


  return result
end


function java_getObjectClass(jObject)
  local result
  javapipe.lock()

  javapipe.writeByte(JAVACMD_GETOBJECTCLASS);
  javapipe.writeQword(jObject)
  result=javapipe.readQword()


  javapipe.unlock()
  return result
end

function java_getClassSignature(jClass)
  local length
  local result=''
  javapipe.lock()
  javapipe.writeByte(JAVACMD_GETCLASSSIGNATURE)
  javapipe.writeQword(jClass)

  length=javapipe.readWord()
  result=javapipe.readString(length);

  length=javapipe.readWord()
  if (length>0) then
    result=result..'  Generic='..javapipe.readString(length);
  end

  javapipe.unlock()

  return result
end

function java_getSuperClass(jClass)
  local result=nil
  javapipe.lock()
  javapipe.writeByte(JAVACMD_GETSUPERCLASS)
  javapipe.writeQword(jClass)

  result=javapipe.readQword()
  javapipe.unlock()

  return result
end



function miJavaActivateClick(sender)
  javaInjectAgent()
end


function javaForm_treeviewExpanding(sender, node)
  local allow=true

  --outputDebugString("Expanding "..node.Text)

  --print("javaForm_treeviewExpanding "..node.level)
  if node.Level==0 then
    if node.Count==0 then
	  --expand the class this node describes
	  local jklass=node.Data
	  local methods=java_getClassMethods(jklass)
	  local fields=java_getClassFields(jklass)
	  local interfaces=java_getImplementedInterfaces(jklass)
	  local superclass=java_getSuperClass(jklass)

	  local i

	  if superclass~=0 then
	    node.add('superclass='..java_getClassSignature(superclass))
		java_dereferenceLocalObject(superclass)
	  end



	  node.add('---Implemented interfaces---');
	  for i=1, #interfaces do
	    local name
		if interfaces[i]>0 then
		  name=java_getClassSignature(interfaces[i])
		else
		  name='???'
		end

	    node.add(string.format("%x : %s", interfaces[i], name))
	  end

	  node.add('---Fields---');
	  for i=1, #fields do
	    node.add(string.format("%x: %s: %s (%s)", fields[i].jfieldid, fields[i].name, fields[i].signature,fields[i].generic))
	  end

	  node.add('---Methods---');


	  for i=1, #methods do
	    node.add(string.format("%x: %s%s           %s", methods[i].jmethodid, methods[i].name, methods[i].signature, methods[i].generic))
	  end


	  --java_getClassFields(jklass);
    end
  end

  return allow
end

function javaForm_searchClass(sender)
  javaForm.findAll=false --classes only
  javaForm.findDialog.Title="Search for class..."
  javaForm.findDialog.execute()
end

function javaForm_searchAll(sender)
  javaForm.findAll=true --everything
  javaForm.findDialog.Title="Search for..."
  javaForm.findDialog.execute()
end

function javaForm_doSearch(sender)
  --search for javaForm.findDialog.FindText
  local currentindex=1
  local findall=javaForm.findAll
  local searchstring=javaForm.findDialog.FindText

  if javaForm.treeview.Selected ~= nil then
    currentindex=javaForm.treeview.Selected.AbsoluteIndex+1 --start at the next one
  end

  while currentindex<javaForm.treeview.Items.Count do
    local node=javaForm.treeview.Items[currentindex]

	if (node.level==0) or findall then
	  --check if node.Text contains the searchstring
	  if string.find(node.Text,searchstring) ~= nil then
	    --found one
		node.Selected=true
		node.makeVisible()
		return
	  end
	end

	if findall and node.HasChildren then
	  node.expand()
	end
	currentindex=currentindex+1
  end

end

function miJavaReferencesClick(sender)
  r=java_followReferences()
end

function miJavaDissectClick(sender)
  --I could also implement the same method as mono, but as an example I'll be creating the form with code only
  if (javaForm==nil) then
    javaForm={}
    javaForm.form=createForm()
	javaForm.form.Borderstyle=bsSizeable
	javaForm.form.Width=640
	javaForm.form.Height=480
	javaForm.treeview=createTreeview(javaForm.form)
	javaForm.treeview.align=alClient
	javaForm.treeview.OnExpanding=javaForm_treeviewExpanding


	javaForm.menu=createMainMenu(javaForm.form)

	local searchmenu=createMenuItem(javaForm.menu)
	searchmenu.caption="Search"

	javaForm.menu.items.add(searchmenu)


	local searchClass=createMenuItem(javaForm.menu)
	searchClass.caption="Find Class"
	searchClass.Shortcut="Ctrl+F"
	searchClass.OnClick=javaForm_searchClass
	searchmenu.add(searchClass)


    local searchAll=createMenuItem(javaForm.menu)
	searchAll.caption="Find..."
	searchAll.Shortcut="Ctrl+Alt+F"
	searchAll.OnClick=javaForm_searchAll
	searchmenu.add(searchAll)

	javaForm.findDialog=createFindDialog(javaForm.form)
	javaForm.findDialog.Options="[frHideEntireScope, frHideWholeWord, frDown, frDisableUpDown, frMatchCase, frDisableMatchCase]"
	javaForm.findDialog.OnFind=javaForm_doSearch
	javaForm.form.position=poScreenCenter


  end

  if (java_classlist~=nil) then
    java_cleanClasslist(java_classlist) --prevent a memory leak
  end
  java_classlist=java_getLoadedClasses()

  if (java_classlist~=nil) then
	local i
	for i=1,#java_classlist do
	  local node=javaForm.treeview.Items.Add(string.format("%d(%x) : %s (%s)", i, java_classlist[i].jclass, java_classlist[i].signature, java_classlist[i].generic	))

	  node.Data=java_classlist[i].jclass
	  node.HasChildren=true
    end
  end




  javaForm.form.show()

end


function miJavaSetEnvironmentClick(sender)
  if targetIs64Bit() then
  autoAssemble([[
alloc(newenv, 32768)

alloc(sev, 2048)
alloc(path, 512)

alloc(pathstr,5)
alloc(JTOstr, 18)
alloc(JTO, 19)
label(end)
label(hasnosemicolon)
label(copyoption)


path:
{$lua}
return "db ';"..getCheatEngineDir().."autorun\\dlls\\32;"..getCheatEngineDir().."autorun\\dlls\\64',0"
{$asm}

pathstr:
db 'PATH',0

JTOstr:
db 'JAVA_TOOL_OPTIONS',0

JTO:
db ' -agentlib:cejvmti',0

sev:

//sub rsp,8 //align the stack
//sub rsp,20 //allocate scratchspace for function calls
sub rsp,28 //using magic to compine those two

//set the path
mov rcx,pathstr
mov rdx,newenv
mov r8,8000

call GetEnvironmentVariableA


mov rdx,path

cmp byte [newenv+rax],';'
jne hasnosemicolon

add rdx,1 //it already has a semicolon so skip it

hasnosemicolon:

mov rcx,newenv
//rdx=path(+1)
call ntdll.strcat

mov rcx,pathstr
mov rdx,newenv
call SetEnvironmentVariableA


//set the java tool options
mov byte [newenv],0


mov rcx,JTOstr
mov rdx,newenv
mov r8,8000
call GetEnvironmentVariableA

mov rdx, JTO

cmp rax,0 //not yet defined
jne copyoption

//it hasn't been defined yet
add rdx,1 //no space

copyoption:

mov rcx,newenv
//rdx=rdx
call ntdll.strcat

mov rcx,JTOstr
mov rdx,newenv
call SetEnvironmentVariableA


end:

add rsp,28
ret

createthread(sev)
]])

  else
  autoAssemble([[
alloc(newenv, 32768)

alloc(sev, 2048)
alloc(path, 512)

alloc(pathstr,5)
alloc(JTOstr, 18)
alloc(JTO, 19)
label(end)
label(hasnosemicolon)
label(copyoption)


path:
{$lua}
return "db ';"..getCheatEngineDir().."autorun\\dlls\\32;"..getCheatEngineDir().."autorun\\dlls\\64',0"
{$asm}

pathstr:
db 'PATH',0

JTOstr:
db 'JAVA_TOOL_OPTIONS',0

JTO:
db ' -agentlib:cejvmti',0

sev:

//set the path
push 8000
push newenv
push pathstr
call GetEnvironmentVariableA


mov esi,path


cmp byte [newenv+eax],';'
jne hasnosemicolon

add esi,1 //it already has a semicolon so skip it

hasnosemicolon:

push esi
push newenv
call ntdll.strcat
add esp,8


push newenv
push pathstr
call SetEnvironmentVariableA


//set the java tool options
mov byte [newenv],0

push 8000
push newenv
push JTOstr
call GetEnvironmentVariableA

mov esi, JTO

cmp eax,0 //not yet defined
jne copyoption

//it hasn't been defined yet
add esi,1 //no space

copyoption:

push esi
push newenv
call ntdll.strcat
add esp,8

push newenv
push JTOstr
call SetEnvironmentVariableA


end:
ret

createthread(sev)

  ]]
  )
  end
end

function java_OpenProcessAfterwards()
  local usesjava=false
  local m=enumModules()
  local i

  java_classlist=nil

  for i=1, #m do
    if m[i].Name=='jvm.dll' then
      usesjava=true
      break
    end
  end

  if usesjava or java.settings.cbAlwaysShowMenu.Checked then
    if (miJavaTopMenuItem==nil) then
      local mfm=getMainForm().Menu
      local mi

      miJavaTopMenuItem=createMenuItem(mfm)
      miJavaTopMenuItem.Caption="Java"
      mfm.Items.insert(mfm.Items.Count-1, miJavaTopMenuItem) --add it before help


      mi=createMenuItem(miJavaTopMenuItem)
      mi.Caption="Configure process environment to launch the ce java agent in spawned processes"
      mi.OnClick=miJavaSetEnvironmentClick
      miJavaTopMenuItem.Add(mi)

      mi=createMenuItem(miJavaTopMenuItem)
      mi.Caption="Activate java features"
      mi.OnClick=miJavaActivateClick
	  mi.Enabled=usesjava
      miJavaTopMenuItem.Add(mi)

      mi=createMenuItem(miJavaTopMenuItem)
      mi.Caption="Dissect java"
      mi.Shortcut="Ctrl+Alt+J"
      mi.OnClick=miJavaDissectClick
	  mi.Enabled=usesjava
      miJavaTopMenuItem.Add(mi)

      mi=createMenuItem(miJavaTopMenuItem)
      mi.Caption="Follow all java references"
      mi.Shortcut="Ctrl+Alt+R"
      mi.OnClick=miJavaReferencesClick
	  mi.Enabled=usesjava
      miJavaTopMenuItem.Add(mi)
    end
  end
end

function java_OpenProcess(processid)
  if java.oldOnOpenProcess~=nil then
    java.oldOnOpenProcess(processid)
  end

  synchronize(java_OpenProcessAfterwards) --call this function when the whole OpenProcess routine is done (next sync check)
end

function javaAA_USEJAVA(parameters, syntaxcheckonly)
  --called whenever an auto assembler script encounters the USEMONO() line
  --the value you return will be placed instead of the given line
  --In this case, returning a empty string is fine
  --Special behaviour: Returning nil, with a secondary parameter being a string, will raise an exception on the auto assembler with that string


  if (syntaxcheckonly==false) and (javaInjectAgent()==0) then
	return nil,"The java handler failed to initialize"
  end

  return "" --return an empty string (removes it from the internal aa assemble list)
end


function java_settingsClose(sender)
  local result=caHide
  if java.settingsOnClose~=nil then
    result=java.settingsOnClose(sender)
  end

  if (result==caHide) and (sender.ModalResult==mrOK) then
    --Apply changes

	--if there is an error return caNone (and show a message preferably)
	if java.settings.cbAlwaysShowMenu.Checked then
	  java.settings.registry.Value["Always Show Menu"]=1
	else
	  java.settings.registry.Value["Always Show Menu"]=0
	end

	if java.settings.cbGlobalHook.Checked then
      if (java.settings.registry.Value["Global Hook"]=='') or (java.settings.registry.Value["Global Hook"]==0) then
	    --it got selected
	  end

	  java.settings.registry.Value["Global Hook"]=1
	else
      if java.settings.registry.Value["Global Hook"]==1 then
	    --it got deselected
	  end
      java.settings.registry.Value["Global Hook"]=0
	end

  end
  return result
end

function java_settingsShow(sender)
  if java.settingsOnShow~=nil then
    result=java.settingsOnShow(sender)
  end

  --update the controls based on the registry
  java.settings.cbAlwaysShowMenu.Checked=java.settings.registry.Value["Always Show Menu"]=='1'
  java.settings.cbGlobalHook.Checked=java.settings.registry.Value["Global Hook"]=='1'

end

function java_initialize()
  --register a function to be called when a process is opened
  if (java==nil) then
    java={}
    java.oldOnOpenProcess=onOpenProcess
	onOpenProcess=java_OpenProcess

	registerAutoAssemblerCommand("USEJAVA", javaAA_USEJAVA)


	local sf=getSettingsForm()
	java.settingsTab=sf.SettingsPageControl.addTab()

	local node=sf.SettingsTreeView.Items.add("Java")
	node.data=userDataToInteger(java.settingsTab)

	java.settingsOnClose=sf.onClose
	sf.onClose=java_settingsClose

	java.settingsOnShow=sf.onShow
	sf.onShow=java_settingsShow


	java.settings={}

	local cbAlwaysShowMenu=createCheckBox(java.settingsTab)
	cbAlwaysShowMenu.Caption="Show java menu item even if the target process hasn't loaded jvm.dll (Used for the local setEnvironment option)"
	cbAlwaysShowMenu.AnchorSideLeft.Control=java.settingsTab
	cbAlwaysShowMenu.AnchorSideLeft.Side="asrLeft"

	cbAlwaysShowMenu.AnchorSideTop.Control=java.settingsTab
	cbAlwaysShowMenu.AnchorSideTop.Side="asrTop"

	cbAlwaysShowMenu.Anchors="[akTop, akLeft]"

	java.settings.cbAlwaysShowMenu=cbAlwaysShowMenu

	local cbGlobalHook=createCheckBox(java.settingsTab)
	cbGlobalHook.Caption="Systemwide java agent injection. (Loads the java agent even when CE isn't running. Reboot recommended)"
	cbGlobalHook.AnchorSideLeft.Control=java.settingsTab
	cbGlobalHook.AnchorSideLeft.Side="asrLeft"

	cbGlobalHook.AnchorSideTop.Control=cbAlwaysShowMenu
	cbGlobalHook.AnchorSideTop.Side="asrBottom"
	cbGlobalHook.Anchors="[akTop, akLeft]"

	java.settings.cbGlobalHook=cbGlobalHook
	java.settings.registry=getSettings("Java")

    --initialize the settings based on the registry
    java.settings.cbAlwaysShowMenu.Checked=java.settings.registry.Value["Always Show Menu"]=='1'
    java.settings.cbGlobalHook.Checked=java.settings.registry.Value["Global Hook"]=='1'



  end
end


java_initialize()
