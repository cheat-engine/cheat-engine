if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'dotnetinfo.po')
end

if getOperatingSystem()==0 then
  pathsep=[[\]]
else
  pathsep='/'
end

local DPIMultiplier=(getScreenDPI()/96)
local CONTROL_MONO=0
local CONTROL_DOTNET=1

--[[local]] DataSource={} --All collected data about the current process. From domains, to images, to classes, to fields and methods. Saves on queries and multiple windows can use it
local CurrentProcess

local frmDotNetInfos={} --keep track of the windows


local function CTypeToString(ctype)
  local r=monoTypeToCStringLookup[ctype]
  if r==nil then
    r='Object '
  end
  
  return r
end


local function getClassMethods(Class)
  Class.Methods={}
  

  
  local i
  if Class.Image.Domain.Control==CONTROL_MONO then
    local methods=mono_class_enumMethods(Class.Handle)
    if methods then
      for i=1,#methods do
        local e={}
        e.Handle=methods[i].method
        e.Name=methods[i].name
        
        local params=mono_method_get_parameters(e.Handle)
                
        e.Parameters=params.returntype..' ('
        local j
        for j=1, #params.parameters do
          local p=params.parameters[i].type..' '..params.parameters[i].name
          
          e.Parameters=e.Parameters..p
          if j~=#params.parameters then
            e.Parameters=e.Parameters..', '
          end
        end
        
        table.insert(Class.Methods, e)
      end
    end
  else
    local methods=DataSource.DotNetDataCollector.GetTypeDefMethods(Class.Image.Handle, Class.Handle)
         
    if methods then
      --MethodToken, Name, Attributes, ImplementationFlags, ILCode, NativeCode, SecondaryNativeCode[]: Integer
      for i=1,#methods do
        local e={}
        --_G.Methods=methods
        --_G.Class=Class
        e.Handle=methods[i].MethodToken
        e.Name=methods[i].Name
        
        print("Method "..e.Name)
        --build parameter list
        local plist=DataSource.DotNetDataCollector.GetMethodParameters(Class.Image.Handle, methods[i].MethodToken)
        if plist then
          --print("Plist is valid.  #plist="..#plist) 
          e.Parameters="("
          for i=1,#plist do
            --print("plist var1 = "..plist[i].CType)
            e.Parameters=e.Parameters..CTypeToString(plist[i].CType).." "..plist[i].Name
            if i<#plist then
              e.Parameters=e.Parameters..', '
            end
          end
          
          e.Parameters=e.Parameters..")"
        else
          --print("plist failed")
          e.Parameters='(...)'        
        end
      
        table.insert(Class.Methods, e)
      end
    end
  end
end

local function getClassFields(Class)
  Class.Fields={}
  local i
  if Class.Image.Domain.Control==CONTROL_MONO then
    local fields=mono_class_enumFields(Class.Handle)
    for i=1,#fields do
      local e={}
      e.Handle=fields[i].type
      e.Name=fields[i].name
      e.VarType=fields[i].monotype
      e.VarTypeName=fields[i].typename
      e.Offset=fields[i].offset
      e.Static=fields[i].isStatic
      e.Const=fields[i].isConst
      
      e.Class=Class
      
      table.insert(Class.Fields,e)
    end
   
  else
    --mono
    local classdata=DataSource.DotNetDataCollector.GetTypeDefData(Class.Image.Handle, Class.Handle)
    if classdata and classdata.Fields then
      for i=1,#classdata.Fields do
        --Offset, FieldType, Name
        local e={}
        e.Handle=classdata.Fields[i].Token
        e.Name=classdata.Fields[i].Name
        e.VarType=classdata.Fields[i].FieldType
        e.VarTypeName=classdata.Fields[i].FieldTypeClassName
        e.Offset=classdata.Fields[i].Offset
        e.Static=classdata.Fields[i].IsStatic
        e.Class=Class
        
        
        table.insert(Class.Fields,e)
      end
    end
    
  end
end

local function getClasses(Image)
  if Image.Classes==nil then
    print("getClasses Error: Image.Classes was not initialized")
    return
  end
  
  if (not Image.Classes.Busy) then
    print("getClasses Error: Image.Classes was not busy")
    return
  end 

  --get the classlist
  local i
  if (Image.Domain.Control==CONTROL_MONO) then
    local classlist=mono_image_enumClasses(Image.Handle)        
    if classlist then         
      for i=1,#classlist do
        local e={}        
        e.Name=classlist[i].classname
        e.NameSpace=classlist[i].namespace        
        e.Handle=classlist[i].class
        e.ParentToken=mono_class_getParent(e.Handle)         
        e.Image=Image
        
        table.insert(Image.Classes,e)
      end     
    end
  else
    local classlist=DataSource.DotNetDataCollector.EnumTypeDefs(Image.Handle)  
    for i=1,#classlist do
     --TypeDefToken, Name, Flags, Extends
      local e={}
      e.Name=classlist[i].Name
      e.NameSpace='' --nyi
      e.Handle=classlist[i].TypeDefToken
      e.ParentToken=DataSource.DotNetDataCollector.GetTypeDefParent(Image.Handle, e.Handle) --classlist[i].Extends
      e.Image=Image
      
      table.insert(Image.Classes,e)
    end
  end

  table.sort(Image.Classes, function(e1,e2) return e1.Name < e2.Name end)
  
end

local function getImages(Domain)
  local i
  
  Domain.Images={}
  if Domain.Control==CONTROL_MONO then
    --mono
    local a=mono_enumAssemblies(Domain.DomainHandle)
    for i=1,#a do
      local image=mono_getImageFromAssembly(a[i])
      local e={}
      e.Handle=image
      e.Name=mono_image_get_name(image)
      e.FileName=extractFileName(e.Name)
      e.Domain=Domain
      table.insert(Domain.Images,e)
    end
  else
    --dotnet
    local modules=DataSource.DotNetDataCollector.EnumModuleList(Domain.Handle)  
    --ModuleHandle, BaseAddress, Name
    for i=1,#modules do
      local e={}
      e.Handle=modules[i].ModuleHandle
      e.Name=modules[i].Name
      e.FileName=extractFileName(e.Name)
      e.BaseAddress=modules[i].BaseAddress
      e.Domain=Domain
      
      table.insert(Domain.Images,e)
      
      
    end
  end
  
  --sort the list
  table.sort(Domain.Images, function(e1,e2) return e1.FileName < e2.FileName end)
      
end

local function getDomains()  
  DataSource.Domains={}
  
  if monopipe then
    local mr=mono_enumDomains()  
    local i
    for i=1,#mr do
      local e={}
      e.Handle=mr[i]
      e.Control=CONTROL_MONO
      table.insert(DataSource.Domains,e)
    end
  end
  
  local mr=DataSource.DotNetDataCollector.EnumDomains()  
  for i=1,#mr do
    local e={}
    e.Handle=mr[i].DomainHandle
    e.Name=mr[i].Name
    e.Control=CONTROL_DOTNET
    table.insert(DataSource.Domains,e)
  end  
  
  return DataSource.Domains
end

local function clearClassInformation(frmDotNetInfo)
  frmDotNetInfo.lvStaticFields.Items.clear()
  frmDotNetInfo.lvFields.Items.clear()
  frmDotNetInfo.lvMethods.Items.clear()
  frmDotNetInfo.gbClassInformation.Caption='Class Information'
end

local function ClassFetchWaitTillReadyAndSendData(thread, frmDotNetInfo, Image, OnDataFunction, FetchDoneFunction)
  --print("ClassFetchWaitTillReadyAndSendData")
  if Image.Classes==nil then 
    print("ClassFetchWaitTillReadyAndSendData Error: Image.Classes is nil")
    return
  end
  
  while Image.Classes.Busy and (not thread.Terminated) do 
    sleep(100)    
  end
  
 -- print("after wait")
  if thread.Terminated or Image.Classes.Busy then return end
  
  local i
  local block={}
  for i=1,#Image.Classes do
    local j=1+((i-1) % 10)
    block[j]=Image.Classes[i]
    
    if j==10 then
      synchronize(OnDataFunction,thread, block)
      block={}      
    end
  end
  
  if #block>0 then
    synchronize(OnDataFunction,thread, block)
  end
  
  synchronize(FetchDoneFunction)  
end

local function StartClassFetch(frmDotNetInfo, Image, OnDataFunction, FetchDoneFunction)
  --create a thread that fetches the full classlist if needed and then calls the OnDataFunction untill full 
  --StartClassFetch called while frmDotNetInfo.ClassFetchThread is not nil. Destroy the old one first  
  if frmDotNetInfo.ClassFetchThread then
    --print("killing old thread")
    frmDotNetInfo.ClassFetchThread.terminate()
    frmDotNetInfo.ClassFetchThread.waitfor()
    frmDotNetInfo.ClassFetchThread.destroy()
    frmDotNetInfo.ClassFetchThread=nil
    
    
  end
  
  if Image.Classes==nil then
    Image.Classes={}
    Image.Classes.Busy=true
    
    --create a thread that fetches the classes
    frmDotNetInfo.ClassFetchThread=createThread(function(t)
      --get the classess
      t.FreeOnTerminate(false)
      getClasses(Image)      
      Image.Classes.Busy=nil
      
      if t.Terminated then
        Image.Classes=nil
        return
      end
      
      --all classes are fetched, send to the waiting form
      ClassFetchWaitTillReadyAndSendData(t, frmDotNetInfo, Image, OnDataFunction, FetchDoneFunction)      
    end)  
  else
    --create a thread that waits until Busy is false/gone and fill the list normally
    --print("Already has list")
    frmDotNetInfo.ClassFetchThread=createThread(function(t)
      t.FreeOnTerminate(false)
      ClassFetchWaitTillReadyAndSendData(t, frmDotNetInfo, Image, OnDataFunction, FetchDoneFunction) 
    end)
  end
  

end

local function CancelClassFetch(frmDotNetInfo)
  --interrupts the current classfetch
  if frmDotNetInfo.ClassFetchThread then
    frmDotNetInfo.ClassFetchThread.terminate()
    frmDotNetInfo.ClassFetchThread.waitfor()
    frmDotNetInfo.ClassFetchThread.destroy()
    frmDotNetInfo.ClassFetchThread=nil
  end
end

local function ClassSelectionChange(sender)
  local frmDotNetInfo=frmDotNetInfos[sender.owner.Tag]
  clearClassInformation(frmDotNetInfo)
  
  if sender.ItemIndex>=0 then
    local Domain=DataSource.Domains[frmDotNetInfo.lbDomains.ItemIndex+1]
    local Image=Domain.Images[frmDotNetInfo.lbImages.ItemIndex+1] 
    local Class=Image.Classes[frmDotNetInfo.lbClasses.ItemIndex+1]    
    
    
    frmDotNetInfo.gbClassInformation.Caption='Class Information ('..Class.Name..')'
  
    if Class.Fields==nil then
      getClassFields(Class)
    end
    
    if Class.Methods==nil then
      getClassMethods(Class)
    end
    
    if Class.Fields then  
      for i=1,#Class.Fields do
        if Class.Fields[i].Static then
          local li=frmDotNetInfo.lvStaticFields.Items.add()                    
          li.Caption=Class.Fields[i].Name
          
          if Class.Fields[i].VarTypeName and Class.Fields[i].VarTypeName~='' then
            li.SubItems.add(Class.Fields[i].VarTypeName)              
          else
            li.SubItems.add(Class.Fields[i].VarType)    
          end
        else       
          local li=frmDotNetInfo.lvFields.Items.add()
        
          li.Caption=string.format("%.3x", Class.Fields[i].Offset)
          li.SubItems.add(Class.Fields[i].Name)
          if Class.Fields[i].VarTypeName and Class.Fields[i].VarTypeName~='' then
            li.SubItems.add(Class.Fields[i].VarTypeName)              
          else
            li.SubItems.add(Class.Fields[i].VarType)    
          end               
        end
      end
    end
    
    if Class.Methods then
      for i=1,#Class.Methods do
        local li=frmDotNetInfo.lvMethods.Items.add()   
        li.Caption=Class.Methods[i].Name
        li.SubItems.add(Class.Methods[i].Parameters)
      end
    end
    
    
  end
end


local function ImageSelectionChange(sender)
  local frmDotNetInfo=frmDotNetInfos[sender.owner.Tag]

  frmDotNetInfo.lbClasses.Items.clear()
  clearClassInformation(frmDotNetInfo)
  
  if sender.ItemIndex>=0 then   
    frmDotNetInfo.lbClasses.Enabled=false
    frmDotNetInfo.lbClasses.Cursor=crHourGlass
    
    local Domain=DataSource.Domains[frmDotNetInfo.lbDomains.ItemIndex+1]
    local Image=Domain.Images[frmDotNetInfo.lbImages.ItemIndex+1]
    StartClassFetch(frmDotNetInfo, Image, function(thread, classlistchunk)
      --executed every 10 lines or so, in the main thread
      local i
      if frmDotNetInfo.lbClasses.Enabled==false then
        --there is something to select already
        frmDotNetInfo.lbClasses.Enabled=true
        frmDotNetInfo.lbClasses.Cursor=crDefault      
      end
      
      if thread.Terminated then return end
      
      for i=1,#classlistchunk do
        frmDotNetInfo.lbClasses.Items.add(string.format('%d: (%.8x) - %s', frmDotNetInfo.lbClasses.Items.Count+1, classlistchunk[i].Handle, classlistchunk[i].Name))
      end
      

    end,
    function()
      --called when done
      frmDotNetInfo.lbClasses.Enabled=true
      frmDotNetInfo.lbClasses.Cursor=crDefault
    end)
    
    
  end
end


local function DomainSelectionChange(sender)
  local frmDotNetInfo=frmDotNetInfos[sender.owner.Tag]
  frmDotNetInfo.lbImages.Items.clear()
  frmDotNetInfo.lbClasses.Items.clear()
  clearClassInformation(frmDotNetInfo)
  
  local frmDotNetInfo=frmDotNetInfos[sender.owner.Tag]
  if sender.ItemIndex>=0 then
    local Domain=DataSource.Domains[sender.ItemIndex+1]
    --get the Images list
    if Domain.Images==nil then
      --fill the images list
      getImages(Domain)   
    end
    
    local i
    local ImageList=Domain.Images
    
    
    for i=1,#ImageList do      
      s=extractFileName(ImageList[i].Name)
      frmDotNetInfo.lbImages.Items.Add(s)
    end
  end
end




function miDotNetInfoClick(sender)
  
  --in case of a double case scenario where there's both .net and mono, go for net first if the user did not activate mono explicitly 
  if (DataSource.DotNetDataCollector==nil) or (CurrentProcess~=getOpenedProcessID) then  
    DataSource={}  
    DataSource.DotNetDataCollector=getDotNetDataCollector()    
  end
  
  if miMonoTopMenuItem and miMonoTopMenuItem.miMonoActivate.Visible and (monopipe==nil)  then
    if getDotNetDataCollector().Attached==false then --no .net and the user hasn't activated mono features. Do it for the user
      LaunchMonoDataCollector()  
    end
  end
  

  local frmDotNetInfo=createFormFromFile(getAutorunPath()..'forms'..pathsep..'DotNetInfo.frm')  
  local i
 
  --find an unused entry
  i=1
  while frmDotNetInfos[i] do i=i+1 end   

  frmDotNetInfos[i]=frmDotNetInfo
  frmDotNetInfo.Name="frmDotNetInfo"..i   
  frmDotNetInfo.Tag=i
  
  frmDotNetInfo.OnDestroy=function(f)
    f.SaveFormPosition({
      f.gbDomains.Width,    
      f.gbImages.Width,
      f.gbClasses.Width,
      f.gbStaticFields.Height,
      f.gbFields.Height,
    })
    
    CancelClassFetch(frmDotNetInfos[f.Tag])
    frmDotNetInfos[f.Tag]=nil
  end
  
  local formdata={}
  frmDotNetInfo.loadedFormPosition,formdata=frmDotNetInfo.LoadFormPosition()
  if frmDotNetInfo.loadedFormPosition then
    if #formdata>=5 then
      frmDotNetInfo.gbDomains.Width=formdata[1]
      frmDotNetInfo.gbImages.Width=formdata[2]
      frmDotNetInfo.gbClasses.Width=formdata[3]
      frmDotNetInfo.gbStaticFields.Height=formdata[4]
      frmDotNetInfo.gbFields.Height=formdata[5]
    end   
  else
    --first run. 
    frmDotNetInfo.gbDomains.Width=DPIMultiplier*100
    frmDotNetInfo.gbImages.Width=DPIMultiplier*300
    frmDotNetInfo.gbClasses.Width=DPIMultiplier*200
    frmDotNetInfo.gbStaticFields.Height=DPIMultiplier*100
    frmDotNetInfo.gbFields.Height=DPIMultiplier*100
    frmDotNetInfo.Width=DPIMultiplier*1300
    frmDotNetInfo.Height=DPIMultiplier*700
    frmDotNetInfo.Position='poScreenCenter'
  end
  
  frmDotNetInfo.OnClose=function()
    return caFree 
  end
  
  --Domain box setup
  frmDotNetInfo.lbDomains.OnSelectionChange=DomainSelectionChange
  
  --Images box setup
  frmDotNetInfo.lbImages.OnSelectionChange=ImageSelectionChange
  
  --Classes box setup
  frmDotNetInfo.lbClasses.OnSelectionChange=ClassSelectionChange
  
  --Class info setup
  
  
  --Init
  
  
  if DataSource.Domains==nil then
    getDomains()
  end
  
  for i=1,#DataSource.Domains do
    local s=string.format("%x",DataSource.Domains[i].Handle)
    if DataSource.Domains[i].Name then
      s=s..' ( '..DataSource.Domains[i].Name..' ) '
    end
    
    frmDotNetInfo.lbDomains.Items.add(s)
  end
  
  if #DataSource.Domains==1 then
    frmDotNetInfo.lbDomains.ItemIndex=0
    frmDotNetInfo.gbDomains.Visible=false
    frmDotNetInfo.split1.Visible=false
  end
  
  
  if #DataSource.Domains==0 then
    DataSource={}
    CurrentProcess=nil --maybe later
    messageDialog(translate('No .NET info found'),mtError,mbOK)
    frmDotNetInfo.destroy()
  else
    frmDotNetInfo.show()  
  end
  
  _G.xxx=frmDotNetInfo
end