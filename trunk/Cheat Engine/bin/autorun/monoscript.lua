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


function LaunchMonoDataCollector()
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

  if injectDLL(getCheatEngineDir()..[[\autorun\dlls\MonoDataCollector.dll]])==false then
    print("Failure injecting the MonoDatacollector dll")
	return 0
  end

  --wait till attached
  local timeout=getTickCount()+5000;
  while (monopipe==nil) and (getTickCount()<timeout) do
    monopipe=connectToPipe('cemonodc_pid'..getOpenedProcessID())
  end

  if (monopipe==nil) then
    return 0 --failure
  end

 -- while (monoeventpipe==nil) do
 --   monoeventpipe=connectToPipe('cemonodc_pid'..getOpenedProcessID()..'_events')
 -- end

  mono_AttachedProcess=getOpenedProcessID()

  monopipe.writeByte(CMD_INITMONO)
  monoBase=monopipe.readQword()

  if (monoBase~=0) then
    if mono_AddressLookupID==nil then
      mono_AddressLookupID=registerAddressLookupCallback(mono_addressLookupCallback)
	end
  end

  return monoBase
end

function mono_addressLookupCallback(address)
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
	  result=mono_class_getName(mono_method_getClass(ji.method))..":"..mono_method_getName(ji.method)
	  if address~=ji.code_start then
	    result=result..string.format("+%x",address-ji.code_start)
	  end
	end

  end

  return result
end

function mono_object_getClass(address)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_OBJECT_GETCLASS)
  monopipe.writeQword(address)

  local classaddress=monopipe.readQword()
  if (classaddress~=0) then
    local stringlength=monopipe.readWord()
    local classname=monopipe.readString(stringlength)
    monopipe.unlock()

    return classaddress, classname
  else
    monopipe.unlock()
    return nil
  end
end

function mono_enumDomains()
  monopipe.lock()
  monopipe.writeByte(MONOCMD_ENUMDOMAINS)
  local count=monopipe.readDword()
  local result={}
  local i
  for i=1, count do
    result[i]=monopipe.readQword()
  end

  monopipe.unlock()

  return result
end

function mono_setCurrentDomain(domain)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_SETCURRENTDOMAIN)
  monopipe.writeQword(domain)

  local result=monopipe.readDword()
  monopipe.unlock()
  return result;
end

function mono_enumAssemblies()
  monopipe.lock()
  monopipe.writeByte(MONOCMD_ENUMASSEMBLIES)
  local count=monopipe.readDword()
  local result={}
  local i
  for i=1, count do
    result[i]=monopipe.readQword()
  end

  monopipe.unlock()
  return result
end

function mono_getImageFromAssembly(assembly)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETIMAGEFROMASSEMBLY)
  monopipe.writeQword(assembly)
  monopipe.unlock()
  return monopipe.readQword()
end

function mono_image_get_name(image)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETIMAGENAME)
  monopipe.writeQword(image)
  local namelength=monopipe.readWord()
  local name=monopipe.readString(namelength)

  monopipe.unlock()
  return name
end

function mono_image_enumClasses(image)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_ENUMCLASSESINIMAGE)
  monopipe.writeQword(image)
  local classcount=monopipe.readDword()

  local classes={}
  local i;
  for i=1, classcount do
    classes[i]={}
    classes[i].class=monopipe.readQword()

    local classnamelength=monopipe.readWord()
	if classnamelength>0 then
      classes[i].classname=monopipe.readString(classnamelength)
	else
	  classes[i].classname=''
	end

    local namespacelength=monopipe.readWord()
	if namespacelength>0 then
      classes[i].namespace=monopipe.readString(namespacelength)
	else
	  classes[i].namespace=''
	end
  end

  monopipe.unlock()

  return classes;
end

function mono_class_getName(clasS)
  local result=''
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETCLASSNAME)
  monopipe.writeQword(clasS)

  local namelength=monopipe.readWord();
  result=monopipe.readString(namelength);

  monopipe.unlock()
  return result;
end


function mono_class_enumFields(class)
  local classfield;
  local index=1;
  local fields={}

  monopipe.lock()

  monopipe.writeByte(MONOCMD_ENUMFIELDSINCLASS)
  monopipe.writeQword(class)

  repeat
    classfield=monopipe.readQword()
    if (classfield~=nil) and (classfield~=0) then
      local namelength;
      fields[index]={}
      fields[index].field=classfield
      fields[index].type=monopipe.readQword()
      fields[index].parent=monopipe.readQword()
      fields[index].offset=monopipe.readDword()
      namelength=monopipe.readWord();
      fields[index].name=monopipe.readString(namelength);

      namelength=monopipe.readWord();
      fields[index].typename=monopipe.readString(namelength);
      index=index+1
    end

  until (classfield==nil) or (classfield==0)

  monopipe.unlock()

  return fields

end

function mono_class_enumMethods(class)
  local method
  local index=1
  local methods={}

  monopipe.lock()

  monopipe.writeByte(MONOCMD_ENUMMETHODSINCLASS)
  monopipe.writeQword(class)

  repeat
    method=monopipe.readQword()
    if (method~=nil) and (method~=0) then
      local namelength;
      methods[index]={}
      methods[index].method=method
      namelength=monopipe.readWord();
      methods[index].name=monopipe.readString(namelength);
      index=index+1
    end

  until (method==nil) or (method==0)

  monopipe.unlock()

  return methods
end

function mono_getJitInfo(address)
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



function mono_object_findRealStartOfObject(address, maxsize)
  if maxsize==nil then
    maxsize=4096
  end

  if address==nil then
    error("address==nil")
  end

  local currentaddress=bAnd(address, 0xfffffffffffffffc)


  while (currentaddress>address-maxsize) do
    local classaddress,classname=mono_object_getClass(currentaddress)

    if (classaddress~=nil) and (classname~=nil) then
      local r=string.find(classname, "[^%a%d_.]", 1)  --scan for characters that are not decimal or characters, or have a _ or . in the name

      if (r==nil) or (r>=5) then
        return currentaddress, classaddress, classname --good enough
      end
    end

    currentaddress=currentaddress-4
  end

  --still here
  return nil

end



function mono_findReferencesToObject(class) --scan the memory for objects with a vtable to a specific class
end

function mono_image_findClass(image, namespace, classname)
--find a class in a specific image
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

function mono_findClass(namespace, classname)
--searches all images for a specific class
  local ass=mono_enumAssemblies()
  local result=0

  for i=1, #ass do

    result=mono_image_findClass(mono_getImageFromAssembly(ass[i]), namespace, classname)
	if (result~=0) then
	  return result;
	end


  end

  --still here:
  return 0
end

function mono_class_findMethod(class, methodname)
  if methodname==nil then return 0 end

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
  local class=mono_findClass(namespace, classname)
  local result=0
  if class~=0 then
    result=mono_class_findMethod(class, methodname)
  end

  return result
end

function mono_invokeMethod()
  print("Not yet implemented")
end

function mono_method_getName(method)
  local result=''
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMETHODNAME)
  monopipe.writeQword(method)

  local namelength=monopipe.readWord();
  result=monopipe.readString(namelength);

  monopipe.unlock()
  return result;
end

function mono_method_getHeader(method)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMETHODHEADER)
  monopipe.writeQword(method)
  local result=monopipe.readQword()

  monopipe.unlock()

  return result;
end

function mono_method_getClass(method)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMETHODCLASS)
  monopipe.writeQword(method)
  local result=monopipe.readQword()

  monopipe.unlock()

  return result;
end


function mono_compile_method(method) --Jit a method if it wasn't jitted yet
  monopipe.lock()

  monopipe.writeByte(MONOCMD_COMPILEMETHOD)
  monopipe.writeQword(method)
  local result=monopipe.readQword()
  monopipe.unlock()
  return result
end

--only if the profiler is active:
function mono_free_method() --unjit the method ?
  monopipe.lock()
  --initialize the profiler if needed
  monopipe.unlock()
end

function mono_methodheader_getILCode(methodheader)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMETHODHEADER_CODE)
  monopipe.writeQword(methodheader)
  local address=monopipe.readQword()
  local size=monopipe.readDword()

  monopipe.unlock()

  return address, size;
end

function mono_getILCodeFromMethod(method)
  local hdr=mono_method_getHeader(method)
  return mono_methodheader_getILCode(hdr)
end


function mono_image_rva_map(image, offset)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_LOOKUPRVA)
  monopipe.writeQword(image)
  monopipe.writeDword(offset)
  local address=monopipe.readQword()
  monopipe.unlock()
  return address;
end



--[[

--------code belonging to the mono dissector form---------

--]]
function monoform_miRejitClick(sender)
  if (monoForm.TV.Selected~=nil) then
    local node=monoForm.TV.Selected
	if (node~=nil) and (node.Level==4) and (node.Parent.Index==1) then
	  local r=mono_compile_method(node.Data)
	  print(string.format("Method at %x", r))
	end
  end
end

function monoform_miGetILCodeClick(sender)
  if (monoForm.TV.Selected~=nil) then
    local node=monoForm.TV.Selected
	if (node~=nil) and (node.Level==4) and (node.Parent.Index==1) then
	  local r,s=mono_getILCodeFromMethod(node.Data)
	  print(string.format("ILCode from %x to %x", r,r+s))
	end
  end
end


function monoform_EnumImages(node)
  --print("monoform_EnumImages")
  local i
  local domain=node.Data
  --mono_setCurrentDomain(domain)
  local assemblies=mono_enumAssemblies()

  for i=1, #assemblies do
    local image=mono_getImageFromAssembly(assemblies[i])
    local imagename=mono_image_get_name(image)
    local n=node.add(string.format("%x : %s", image, imagename))
    n.HasChildren=true;
    n.Data=image

  end
end

function monoform_EnumClasses(node)
  --print("monoform_EnumClasses")
  local image=node.Data
  local classes=mono_image_enumClasses(image)
  local i
  for i=1, #classes do
    local n=node.add(string.format("%x : %s:%s", classes[i].class, classes[i].namespace, classes[i].classname))

    local nf=n.add("fields");
    nf.Data=classes[i].class;
    nf.HasChildren=true

    local nm=n.add("methods");
    nm.Data=classes[i].class;
    nm.HasChildren=true
  end

end;

function monoform_EnumFields(node)
 -- print("monoform_EnumFields")
  local i
  local class=node.Data;
  local fields=mono_class_enumFields(class)
  for i=1, #fields do
    local n=node.add(string.format("%x : %s (type: %s)", fields[i].offset, fields[i].name,  fields[i].typename))
    n.Data=fields[i].field
  end
end

function monoform_EnumMethods(node)
  --print("monoform_EnumMethods")
  local i
  local class=node.Data;


  local methods=mono_class_enumMethods(class)
  for i=1, #methods do
    local n=node.add(string.format("%x : %s", methods[i].method, methods[i].name))
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
    elseif (node.Level==3) and (node.Index==0) then --fields
      monoform_EnumFields(node)
    elseif (node.Level==3) and (node.Index==1) then --methods
      monoform_EnumMethods(node)
    end

  end

  return allow
end


function mono_TVCollapsing(sender, node)
  local allow=true

  return allow
end

function monoform_miExpandAllClick(sender)
  if messageDialog("Are you sure you wish to expand the whole tree? This can take a while and Cheat Engine may look like it has crashed (It has not)", mtConfirmation, mbYes, mbNo)==mrYes then
    monoForm.TV.beginUpdate()
    monoForm.TV.fullExpand()
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
    monoForm=createFormFromFile(getCheatEngineDir()..[[\autorun\forms\MonoDataCollector.frm]])
  end

  monoForm.show()

  monoForm.TV.Items.clear()

  local domains=mono_enumDomains()
  local i
  for i=1, #domains do
    n=monoForm.TV.Items.add(string.format("%x", domains[i]))
    n.Data=domains[i]
    monoForm.TV.Items[i-1].HasChildren=true
  end

end

function miMonoActivateClick(sender)
  if LaunchMonoDataCollector()==0 then
    showMessage("Failure to launch")
  end
end

function miMonoDissectClick(sender)
  mono_dissect()
end

function mono_OpenProcess(processid)
  --call the original onOpenProcess if there was one
  if mono_oldOnOpenProcess~=nil then
    mono_oldOnOpenProcess(processid)
  end

  --enumModules is faster than getAddress at OpenProcess time (No waiting for all symbols to be loaded first)

  local usesmono=false
  local m=enumModules()
  local i
  for i=1, #m do
    if m[i].Name=='mono.dll' then
	  usesmono=true
	  break
	end
  end

  if usesmono then
    --create a menu item if needed
	if (miMonoTopMenuItem==nil) then
	  local mfm=getMainForm().Menu
	  local mi
	  miMonoTopMenuItem=createMenuItem(mfm)
	  miMonoTopMenuItem.Caption="Mono"
	  mfm.Items.insert(mfm.Items.Count-1, miMonoTopMenuItem) --add it before help

	  mi=createMenuItem(miMonoTopMenuItem)
	  mi.Caption="Activate mono features"
	  mi.OnClick=miMonoActivateClick
	  miMonoTopMenuItem.Add(mi)

	  mi=createMenuItem(miMonoTopMenuItem)
	  mi.Caption="Dissect mono"
	  mi.Shortcut="Ctrl+Alt+M"
	  mi.OnClick=miMonoDissectClick
	  miMonoTopMenuItem.Add(mi)
	end


  else
    --destroy the menu item if needed
	if miMonoTopMenuItem~=nil then
	  miMonoTopMenuItem.destroy() --also destroys the subitems as they are owned by this menuitem
	  miMonoTopMenuItem=nil
	end

	if monopipe~=nil then
	  monopipe.destroy()
	  monopipe=nil
	end
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
	return nil,"The mono handler failed to initialize"
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
        return nil,"Invalid parameters (Methodname could not be determined)"
      end
    else
      return nil,"Invalid parameters (Classname could not be determined)"
    end
  else
    return nil,"Invalid parameters (name could not be determined)"
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
    return nil,"The mono handler failed to initialize"
  end


  local method=mono_findMethod(namespace, classname, methodname)
  if (method==0) then
    return nil,fullmethodnamestring.." could not be found"
  end

  methodaddress=mono_compile_method(method)
  if (methodaddress==0) then
    return nil,fullmethodnamestring.." could not be jitted"
  end


  local result="define("..name..","..string.format("%x", methodaddress)..")"

 -- showMessage(result)

  return result
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
	print("Format 2")

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
  if (class==0) then
    return nil,"The class "..namespace..":"..classname.." could not be found"
  end

  local fields=mono_class_enumFields(class)
  if (fields==nil) or (#fields==0) then
    return nil,namespace..":"..classname.." has no fields"
  end


  local offsets={}
  local i
  for i=1, #fields do
    if fields[i].offset~=0 then
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


  for i=2, #sortedindex do
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

function mono_initialize()
  --register a function to be called when a process is opened
  if (mono_init1==nil) then
    mono_init1=true
    mono_oldOnOpenProcess=onOpenProcess
	onOpenProcess=mono_OpenProcess

	registerAutoAssemblerCommand("USEMONO", monoAA_USEMONO)
	registerAutoAssemblerCommand("FINDMONOMETHOD", monoAA_FINDMONOMETHOD)
	registerAutoAssemblerCommand("GETMONOSTRUCT", monoAA_GETMONOSTRUCT)
  end
end


mono_initialize()

