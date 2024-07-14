if dotNetInfo==nil then
  dotNetInfo={}
end

function dotNetInfo.createInstanceOfClassDialog(Class)
  if Class==nil then return nil,'No class provided' end 
  

  if (Class.Image.Domain.Control~=0) then
    return nil,'Only mono/il2pp is supported' --not currently supported
  end


  --find .ctor's
  local ctorlist={}
  for i=1,#Class.Methods do
    if Class.Methods[i].Name=='.ctor' then
      table.insert(ctorlist, Class.Methods[i])
    end
  end

  if #ctorlist==0 then return nil,'No .ctor' end

  local instanceAddress=mono_object_new(Class.Handle)

  if instanceAddress==nil or instanceAddress==0 then
    return nil,'Failure creating instance of this class'
  end
  
 -- printf("New instanceAddress=%x", instanceAddress)


  local Method
  if #ctorlist>1 then
    --ask which one to use
    local sl=createStringList()
    for i=1,#ctorlist do
      sl.add(Class.Methods[i].ReturnType..' '..Class.Methods[i].Parameters)
    end

    local i,r=showSelectionList(Class.FullName..' .ctors', "Select the constructor you'd like to use", sl, false)
    if i==-1 then return nil end

    Method=Class.Methods[i+1]

    sl.destroy()
  else
    Method=ctorlist[1]
  end
  
  if Method==nil then
    return nil,'No method selected'
  end
  
  
  if #Method.Parameters==0 then
    --no parameters needed, call the constructor directly
    mono_invoke_method(Method.Class.Image.Domain.Handle, Method.Handle, instanceAddress,{})
    if result==nil then
      messageDialog('Failure to call constructor:'.._secondary,mtWarning)
    end    
  else
    --constructor has parameters, ask to fill them in
    --printf("Calling dotNetInfo.SpawnInvokeMethodDialog with address %x", instanceAddress)
    
    local r,err=dotNetInfo.SpawnInvokeMethodDialog(Method, instanceAddress)
    if r==nil and err then    
      messageDialog('Failure to call constructor:'..err,mtWarning)     
    end
  end
  
  return instanceAddress
end