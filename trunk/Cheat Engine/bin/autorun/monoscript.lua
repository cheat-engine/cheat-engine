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



function LaunchMonoDataCollector()
  if injectDLL(getCheatEngineDir()..[[\autorun\dlls\MonoDataCollector.dll]])==false then
    print("Failure injecting the MonoDatacollector dll")
	return 0
  end




  if (monopipe~=nil) then
    monopipe.destroy()
    monopipe=nil
  end

  if (monoeventpipe~=nil) then
    monoeventpipe.destroy()
    monoeventpipe=nil
  end

  while (monopipe==nil) do
    monopipe=connectToPipe('cemonodc_pid'..getOpenedProcessID())
  end

 -- while (monoeventpipe==nil) do
 --   monoeventpipe=connectToPipe('cemonodc_pid'..getOpenedProcessID()..'_events')
 -- end

  monopipe.writeByte(CMD_INITMONO)
  return monopipe.readQword()
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
    classes[i].classname=monopipe.readString(classnamelength)
  end

  monopipe.unlock()

  return classes;
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


function mono_findReferencesToObject() --scan the memory for objects with a vtable to a specific class
end

function mono_findClass()
end

function mono_findMethod()
end

function mono_invokeMethod()
end

function mono_method_getHeader(method)
  monopipe.lock()
  monopipe.writeByte(MONOCMD_GETMETHODHEADER)
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


--code belonging to the mono dissector form--
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
    local n=node.add(string.format("%x : %s", classes[i].class, classes[i].classname))

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


--to launch execute:
--mono_dissect()
