--java info


jDataSource={} 
jDataSource.globalsToRelease={}
local frmJavaInfos={} 

--[[

nodeinfo contains:
self - nodeinfo
parent - nodeinfo
selfnode - stringtreenode
parentnode - stringtreenode
class
field
valuestring
object --for InterpretMode 1
objectclass: string -- for InterpretMode 1 (on fetching values, and the class doesn't match the objectclass then delete all childnodes first)
address --for InterpretMode 0

jDataSource.globalsToRelease is a table of jobjects that are not needed anymore.
when a node gets destroyed, and the node has a object reference, add it to that list
    
--]]

function lvClassesOnData(frmJavaInfo, sender, listitem)
  local src=frmJavaInfo.filteredClassList or jDataSource.Classes
 
  local data=src and src[listitem.Index+1]
  if data then
    listitem.Caption=data.signature
  end
end

function stFieldsGetText_Offset(nodeinfo)
  if nodeinfo.field then
    if nodeinfo.field.offset then
      return string.format("%.2x", nodeinfo.field.offset)
    else  
      return '<no offset>'
    end
  else
    return '<nodeinfo.field=nil>'
  end
end

function stFieldsGetText_Name(nodeinfo)
  if nodeinfo.field then
    return nodeinfo.field.name
  end
end

function stFieldsGetText_Type(nodeinfo)
  if nodeinfo.field then
    return nodeinfo.field.signature
  end
end

function stFieldsGetText_Value(nodeinfo)
  if nodeinfo.valuestring then
    return nodeinfo.valuestring
  else      
    return ''
  end
end

stFieldsGetTextLookupTable={}
stFieldsGetTextLookupTable[0]=stFieldsGetText_Offset
stFieldsGetTextLookupTable[1]=stFieldsGetText_Name
stFieldsGetTextLookupTable[2]=stFieldsGetText_Type
stFieldsGetTextLookupTable[3]=stFieldsGetText_Value

function stFieldsGetText(frmJavaInfo, sender, nodeindex, columnindex, node, texttype)
  local nodeinfo=getNodeInfoFromNode(sender, node)
  
  if nodeinfo==nil then
    return '<nodeinfo=nil>'
  end
  
  local lf=stFieldsGetTextLookupTable[columnindex]
  if lf then
    return lf(nodeinfo)
  else
    return 'wtf: '..nodeindex..','..columnindex
  end
end



function stFieldsDblClick(frmJavaInfo, sender)
  --get the selected node (if there is any)
  local node=sender.FocusedNode
  if node==nil then return end
  local nd=getNodeInfoFromNode(sender, node)
  if nd==nil then return end
  if nd.parent==nil then return end
  
  if sender.HasChildren[node] then
    if nd.field.InternalType~=11 then
      return
    end
  end

  local oldvalue=stFieldsGetText(frmJavaInfo, sender, sender.NodeIndex[node], 3, node, 0)
  if nd.field.InternalType==11 then  --remove doublequotes
    oldvalue=oldvalue:sub(2,-2)
  end
  
  if oldvalue==nil then return end
  if oldvalue=='' then return end

  local newvalue=inputQuery('Field Edit', 'New value for this field:', oldvalue)

  if newvalue==nil then return end

  --printf("Changing the value to %s", newvalue)
  local newValues={}
  newValues[1]={}
  if nd.field.static then
    newValues[1].ObjectOrClass=nd.class.jclass 
  else
    newValues[1].ObjectOrClass=nd.parent.object
  end
  newValues[1].Field=nd.field
  newValues[1].Value=newvalue
  java_setFieldValues(newValues)
  
  --_G.newValues=newValues

  buildChildNodes(frmJavaInfo, nd.parent, sender==frmJavaInfo.stStaticFields)
  

  sender.refresh()
end



function stFieldsFreeNode(frmJavaInfo, sender, node)
  local d=sender.getNodeDataAsInteger(node)
  if d then  
    local nodeinfo=getRef(d)
    if nodeinfo then
      if nodeinfo.object then
        
        table.insert(jDataSource.globalsToRelease, nodeinfo.object)
      end
    else
      print("freeing a node that had an empty ref set")
    end
     
    destroyRef(d)
  else
    print("freeing a node that had no luaref set")
  end
end

function getNodeInfoFromNode(stringtreeview, node)
  local ndi=stringtreeview.getNodeDataAsInteger(node)
  if ndi==nil or ndi==0 then return nil end
  return getRef(ndi)
end



function getClassFieldsAndMethods(Class)
  if Class.Fields==nil then
    --fill the fieldlist
    Class.Fields, Class.StaticFields=java_getClassFields(Class.jclass)
    if Class.Fields then
      Class.Fields.Class=Class
      Class.StaticFields.Class=Class
    else
      Class.StaticFields=nil      
    end
  end
  
  if Class.Methods==nil then
    Class.Methods=java_getClassMethods(Class.jclass)
  end
end

function getAllClassFieldsAndMethods(Class)

  if (Class.AllStaticFields==nil) or (Class.AllFields==nil) or (Class.AllMethods==nil) then
    local StaticFieldList={}
    local FieldList={}
    local MethodList={}
    local ClassList={}
    
    local c=Class
    
    while c do
      getClassFieldsAndMethods(c) 
      table.insert(ClassList, c)
      c=jDataSource.Classes.signatureLookup[c.superclass_signature]    
    end    
    
    for i=#ClassList,1,-1 do
      if ClassList[i].StaticFields then
        for j=1,#ClassList[i].StaticFields do
          table.insert(StaticFieldList, ClassList[i].StaticFields[j])
        end
      end
      
      if ClassList[i].Fields then
        for j=1,#ClassList[i].Fields do
          table.insert(FieldList, ClassList[i].Fields[j])
        end
      end
      
      if ClassList[i].Methods then
        for j=1,#ClassList[i].Methods do
          table.insert(MethodList, ClassList[i].Methods[j])
        end      
      end

    end
    
    StaticFieldList.Class=Class
    FieldList.Class=Class
    MethodList.Class=Class
    
    Class.AllStaticFields=StaticFieldList
    Class.AllFields=FieldList
    Class.AllMethods=MethodList
    
    
  end
end

function buildChildNodes(frmJavaInfo, nodeinfo, staticfieldlist)
  local sig
  local class
  
  local stv
  
  if staticfieldlist then
    stv=frmJavaInfo.stStaticFields
  else
    stv=frmJavaInfo.stFields
  end

  if nodeinfo==nil then
    if staticfieldlist then
      nodeinfo=frmJavaInfo.rootnodestatic
    else
      nodeinfo=frmJavaInfo.rootnode
    end
  end
  
  if nodeinfo.object then
    sig=java_getObjectClassName(nodeinfo.object)
    class=jDataSource.Classes.signatureLookup[sig]
  else
    if nodeinfo.objectclass then
      sig=nodeinfo.objectclass
      class=jDataSource.Classes.signatureLookup[sig]
    else
      if nodeinfo.parent==nil then
        sig=frmJavaInfo.CurrentlySelectedClass.signature
        class=frmJavaInfo.CurrentlySelectedClass
      else
        if nodeinfo.field then       
          sig=nodeinfo.field.signature
          class=jDataSource.Classes.signatureLookup[sig]
        end
        
        if class==nil then 
          return
          --error('buildChildNodes called with an invalid node (not the root, no object and no objectclass)')
        end
      end
    end
  end
  

  if class then
    getClassFieldsAndMethods(class) --fills them if needed
    
   
    
    local Fields    
    local Values
    
    if frmJavaInfo.cbShowInherited.checked then
      getAllClassFieldsAndMethods(class) --initializes the fields if not yet initialized    
      
      --find the inherited fields and add them to the fields 
      if staticfieldlist and (nodeinfo.parent==nil) then          
        Fields=class.AllStaticFields
      else
        Fields=class.AllFields
      end
    else
      --no need to copy, just use as it is
      if staticfieldlist and (nodeinfo.parent==nil) then  
        Fields=class.StaticFields    
      else    
        Fields=class.Fields    
      end
    end
    
    
    if staticfieldlist and (nodeinfo.parent==nil) then  
      Values=java_getFieldValuesFromObject(0, Fields)    
    else
      if nodeinfo.object then
        Values=java_getFieldValuesFromObject(nodeinfo.object, Fields)      
      end    
    end
    
    local valid=(nodeinfo.objectclass==sig)

    if valid then
      valid=#Fields==stv.NodeChildCount[nodeinfo.node]
    end

    if not valid then
      frmJavaInfo.stFields.HasChildren[nodeinfo.node]=false --deletes all children
    end
    

    local n=stv.getFirstChild(nodeinfo.node)  
    if Fields then
      for i=1,#Fields do
        local currentnodeinfo
        if n==nil then
          n=stv.addChild(nodeinfo.node)
          currentnodeinfo={}
          currentnodeinfo.self=currentnodeinfo
          currentnodeinfo.parent=nodeinfo
          currentnodeinfo.node=n
          currentnodeinfo.parentnode=nodeinfo.node
          stv.setNodeDataAsInteger(n, createRef(currentnodeinfo))
        else
          currentnodeinfo=getNodeInfoFromNode(stv, n)
        end

        currentnodeinfo.class=class
        currentnodeinfo.field=Fields[i]

        if currentnodeinfo.object then
          table.insert(jDataSource.globalsToRelease,currentnodeinfo.object) --mark for deletion, it's going to get overwritten
        end


        if Values then
          currentnodeinfo.valuestring=Values[i].Value
          currentnodeinfo.object=Values[i].Object
        else
          currentnodeinfo.valuestring=''
          currentnodeinfo.object=nil
          
        end


        if currentnodeinfo.object and  (stv.NodeChildCount[n]>0) then  --this node was already extracted
          buildChildNodes(frmJavaInfo, currentnodeinfo, staticfieldlist)
        else
          stv.HasChildren[currentnodeinfo.node]=false --deletes any nodes if they where present
          stv.HasChildren[currentnodeinfo.node]=Fields[i].signature:startsWith('L') and (Fields[i].signature~='Ljava/lang/String;')
        end

        n=stv.getNextSibling(n)
      end
    end
   
    if nodeinfo.object then
      nodeinfo.objectclass=sig
    end
  else
    print("unknown class:"..sig)
  end
end

function stFieldsExpanding(frmJavaInfo, sender, node)
  buildChildNodes(frmJavaInfo, getNodeInfoFromNode(sender, node))
  return true  
end

function stStaticFieldsExpanding(frmJavaInfo, sender, node)
  buildChildNodes(frmJavaInfo, getNodeInfoFromNode(sender, node),true)
  return true  
end

local delayedResize

function InheritanceResize(gbInheritance, now)
  local oldresize=gbInheritance.OnResize
  gbInheritance.OnResize=nil
  
  if delayedResize==nil then
    local f=function()
      local i,x,y
      local width=gbInheritance.ClientWidth
      
      x=jf.lblSuperclasses.Width+4
      y=0
      
      for i=2 , gbInheritance.ControlCount-1 do
        --the labels are in the order they get added to the groupbox
        local c=gbInheritance.Control[i]
        if (x~=0) and (x+c.Width>width) then --next line
          x=0
          y=y+c.height
        end
        
        c.Left=2+x
        c.Top=y
        
        
        x=x+c.Width+1
      end      
      delayedResize=nil      
    end
    
    if now then 
      f()
    else
      delayedResize=createTimer(100,f)
    end
  else
    --reset timer
    delayedResize.Enabled=false
    delayedResize.Enabled=true
  end  
  
  gbInheritance.OnResize=oldresize
  

end

function buildInheritanceList(frmJavaInfo)
  while frmJavaInfo.gbInheritance.ControlCount>2 do
    frmJavaInfo.gbInheritance.Control[2].destroy()   
  end
  
  local class
  if frmJavaInfo.rootnode.objectclass then
    class=jDataSource.Classes.signatureLookup[frmJavaInfo.rootnode.objectclass]
  else
    class=jf.CurrentlySelectedClass
  end
  
  if class==nil then
    class=jf.CurrentlySelectedClass
  end
  
  local ClassList={}
  ClassList[1]={}
  ClassList[1].Class=class
  while class.superclass_signature do
    local e={}    
      
    class=jDataSource.Classes.signatureLookup[class.superclass_signature]
    if class==nil then      
      class={}
      class.signature=class.superclass_signature
    end
    e.Class=class
    table.insert(ClassList,e)        
  end
  
  for i=1,#ClassList do
    local l=createLabel(frmJavaInfo.gbInheritance)
    ClassList[i].Label=l
    l.caption=ClassList[i].Class.signature
    if i==1 then
      l.Font.Style="[fsBold]"  
    else
      if ClassList[i].Class.jclass then      
        l.Font.Style="[fsUnderline]"
        l.Font.Color=clBlue       
      end
    end
    
    if ClassList[i].Class.jclass  then
      l.Cursor=crHandPoint
      l.OnMouseDown=function(s)
        --show class state
        --print("Showing state "..ClassList[i].Class.Name)    
        frmJavaInfo.stFields.clear()
        frmJavaInfo.stStaticFields.clear()
        
        frmJavaInfo.rootnode.objectclass=ClassList[i].Class.signature
        frmJavaInfo.rootnodestatic.objectclass=ClassList[i].Class.signature
        
        frmJavaInfo.CurrentlySelectedClass=ClassList[i].Class
    
        buildChildNodes(frmJavaInfo, nil)    
        buildChildNodes(frmJavaInfo, nil, true) --static fields
  
        
        local j
        for j=1,#ClassList do
          if j==i then --the currently selected index (this is why I like lua)
            ClassList[j].Label.Font.Color=clWindowText
            ClassList[j].Label.Font.Style="[fsBold]" 
          else
            ClassList[j].Label.Font.Style="[fsUnderline]"
            ClassList[j].Label.Font.Color=clBlue   
          end
        end
      end      
      
    end
    
    if i~=#ClassList then --not the last item
      l=createLabel(frmJavaInfo.gbInheritance)          
      l.Caption="->"    
    end    
  end
    
  
  InheritanceResize(frmJavaInfo.gbInheritance, true)
end

function lvClassesSelectionChange(frmJavaInfo, sender)
  if frmJavaInfo.dontSwitchClass then return end

  frmJavaInfo.rootnode.object=nil
   
  for i=0,frmJavaInfo.lvMethods.items.Count-1 do
    destroyRef(frmJavaInfo.lvMethods.items[i].Data)
  end
  frmJavaInfo.lvMethods.items.clear()
  

  frmJavaInfo.comboFieldBaseAddress.text=''
  frmJavaInfo.comboFieldBaseAddress.Items.clear()
  
  frmJavaInfo.stFields.clear()
  frmJavaInfo.stStaticFields.clear()
  
  frmJavaInfo_clearJavaInstances(frmJavaInfo)

  
  if sender.ItemIndex>=0 then
    frmJavaInfo.btnLookupInstances.Enabled=true
  
    local src=frmJavaInfo.filteredClassList or jDataSource.Classes    
    local class=src[sender.ItemIndex+1]
    
    if class==nil then
      error('lvClassesSelectionChange: error: class==nil  sender.ItemIndex+1=='..sender.ItemIndex+1)
    end
    
    frmJavaInfo.CurrentlySelectedClass=class    
    
    getClassFieldsAndMethods(class)
    local methodList=class.Methods
    
    if frmJavaInfo.cbShowInherited.checked then     
      getAllClassFieldsAndMethods(class)
      methodList=class.AllMethods
    end
    
    frmJavaInfo.lvMethods.beginUpdate()
    
    if methodList then
      for i=1,#methodList do
        local li=frmJavaInfo.lvMethods.items.add()
        li.caption=methodList[i].name
        li.subItems.add(methodList[i].signature);
        li.data=createRef(methodList[i])
      end
    end
    frmJavaInfo.lvMethods.endUpdate()

    frmJavaInfo.rootnode.object=nil     
    frmJavaInfo.rootnode.objectclass=nil
    frmJavaInfo.rootnodestatic.object=nil     
    frmJavaInfo.rootnodestatic.objectclass=nil

     
   
    buildChildNodes(frmJavaInfo, nil)    
    buildChildNodes(frmJavaInfo, nil, true) --static fields

    
    if class.Fields then    
      local hasoffsets=false

      for i=1,#class.Fields do 
        if class.Fields[i].offset then 
          hasoffsets=true
          break
        end      
      end

      if hasoffsets then
        frmJavaInfo.stFields.Header.MainColumn=0
        frmJavaInfo.stFields.Header.Columns[0].Visible=true
      else
        frmJavaInfo.stFields.Header.MainColumn=1
        frmJavaInfo.stFields.Header.Columns[0].Visible=false
      end
    end
    
    buildInheritanceList(frmJavaInfo)
  end
end

function edtClassFilterChange(frmJavaInfo, sender)
  frmJavaInfo.dontSwitchClass=true
  frmJavaInfo.lvClasses.ItemIndex=-1
  frmJavaInfo.dontSwitchClass=false
  
  if frmJavaInfo.edtClassFilter.Text=='' then
    frmJavaInfo.filteredClassList=nil
    frmJavaInfo.lvClasses.Items.Count=#jDataSource.Classes
  else
    local casesensitive=frmJavaInfo.cbCaseSensitive.Checked
    local filter=frmJavaInfo.edtClassFilter.Text
    if not casesensitive then
      filter=filter:upper()
    end      
    local r={}
    
    if casesensitive then
      printf("case sensitive scan")
      for i=1,#jDataSource.Classes do
        if jDataSource.Classes[i].signature:find(filter,1,true) then          
          table.insert(r,jDataSource.Classes[i])        
        end      
      end
    else
      for i=1,#jDataSource.Classes do
        if jDataSource.Classes[i].signature:upper():find(filter,1,true) then
          table.insert(r,jDataSource.Classes[i])        
        end      
      end    
    end
    
    frmJavaInfo.filteredClassList=r    
    frmJavaInfo.lvClasses.Items.Count=#r
  end
end

function frmJavaInfo_clearJavaInstances(frmJavaInfo)
  if frmJavaInfo.instancelist then
    local objects={}
    
    for i=1,#frmJavaInfo.instancelist do
      objects[i]=frmJavaInfo.instancelist[i].jobject
    end 
    
    for i=1,#jDataSource.globalsToRelease do --add other globals to release as well
      table.insert(objects, jDataSource.globalsToRelease[i])
    end
    
    --printf("Dereferencing %d objects", #objects)
    java_dereferenceGlobalObjects(objects)
    
    jDataSource.globalsToRelease={}
  end
  frmJavaInfo.instancelist=nil
  
  frmJavaInfo.comboFieldBaseAddress.Items.clear()
end

function btnLookupInstancesClick(frmJavaInfo, sender)
  --looking up instances
  local lookupmethod=frmJavaInfo.InstanceLookupMethod
  
  if frmJavaInfo.InstanceLookup then
    frmJavaInfo.InstanceLookup.destroy()
    frmJavaInfo.InstanceLookup=nil  
  end
  frmJavaInfo.btnLookupInstances.Enabled=false  
  
  frmJavaInfo_clearJavaInstances(frmJavaInfo)
  
  frmJavaInfo.InstanceLookupActive=true  
  createThread(function(t)  
    local crashed=false
    t.freeOnTerminate(false)
    t.Name='JavaInstanceLookup'
    
   -- frmJavaInfo.btnLookupInstances.Caption='Looking up instances'
    
    
    
    --do stuff
    
    --printf("Finding instances of jclass %d", frmJavaInfo.CurrentlySelectedClass.jclass)
    local results=java_findAllInstancesFromClass(frmJavaInfo.CurrentlySelectedClass.jclass, lookupmethod)
    
    if (results==nil) or (#results==0) and (readInteger(process)==nil) then      
      crashed=true     
    end 
    frmJavaInfo.instancelist=results;
    
    
    if t.terminated==false then
      t.synchronize(function()  
        --fill frmJavaInfo.comboFieldBaseAddress.Items with the results
        local j
        
        frmJavaInfo.comboFieldBaseAddress.Items.clear()
        if crashed then
          messageDialog('It looks like the target has crashed. Try one of the other lookup methods. (Rightclick on "Lookup Instances" and pick an alternate method next time)', mtError, mbOK)
          return
        end
 
        for i=1, #results do
          if results[i].address==0 then
            j=frmJavaInfo.comboFieldBaseAddress.Items.add(string.format('jobject(%x)', results[i].jobject))          
          else
            j=frmJavaInfo.comboFieldBaseAddress.Items.add(string.format('jobject(%x) //(%x)', results[i].jobject, results[i].address))          
          end
          frmJavaInfo.comboFieldBaseAddress.Items.Data[j]=results[i].jobject
        end

        
        frmJavaInfo.btnLookupInstances.Enabled=true      
        
        frmJavaInfo.comboFieldBaseAddress.DroppedDown=true
      end)     
    end
  end)
end

function comboFieldBaseAddressOnChange(frmJavaInfo, sender)
  local s=sender.Text:trim()
  
  frmJavaInfo.rootnode.InterpretMode=-1 --invalid, don't try
  
  if s:startsWith('jobject(') then  
    --must be a selected entry
    local index=sender.ItemIndex
    if index==-1 then
      index=sender.Items.IndexOf(s)

      if index==-1 then
        return
      end      
    end
    
    --interpret as jobject (use the selection, not the text)
    frmJavaInfo.rootnode.InterpretMode=1
    frmJavaInfo.rootnode.object=frmJavaInfo.comboFieldBaseAddress.Items.Data[index]
  else
    --interpret as address   
    frmJavaInfo.rootnode.InterpretMode=0
    frmJavaInfo.rootnode.address=getAddressSafe(s)    
  end
  
  refreshFieldListValues(frmJavaInfo)
end

function refreshFieldListValues(frmJavaInfo)
  if frmJavaInfo.rootnode.InterpretMode==-1 then return end
  buildChildNodes(frmJavaInfo, frmJavaInfo.rootnode) 
  frmJavaInfo.stFields.refresh()
end

function miInvokeMethodClick(frmJavaInfo, sender)
  if frmJavaInfo.rootnode.object==nil then
    messageDialog('Please first select a proper instance', mtError,mbOK)
    return
  end

  if frmJavaInfo.lvMethods.Selected==nil then return end

  local Method=getRef(frmJavaInfo.lvMethods.Selected.Data)
  local name=frmJavaInfo.CurrentlySelectedClass.signature:sub(2,-2)..':'..Method.name
  local params={}
  --parse the java signature and split it into parameters:
  local parsedsig=java_parseSignature(Method.signature)

  if parsedsig.parameters then
    for i=1,#parsedsig.parameters do
      params[i]=java_convertTypeStrToReadableString(parsedsig.parameters[i])
    end
  end


  --frmJavaInfo.currentlySelectedClasss

  --title name:
  --local name=
  local resultpanel
  local resultlb
  local midinfo
  local results={}  

  midinfo=createMethodInvokeDialog(name, params, function(form, values)
    

    local parameters={}
    for i=1,#parsedsig.parameters do
      parameters[i]={}
      parameters.value=values[i]
      parameters.type=parsedsig.parameters[i]
    end

    if resultpanel==nil then
      resultpanel=createForm()
      --resultpanel.PopupMode='pmNone'
      resultpanel.BorderStyle=bsSizeable

      function setPos()
        resultpanel.left=4+midinfo.mid.left+midinfo.mid.width
        resultpanel.top=midinfo.mid.top
        resultpanel.height=midinfo.mid.height
      end

      midinfo.mid.OnChangeBounds=function(s)
        setPos()
      end

      setPos()

      resultlb=createListBox(resultpanel) --todo: virtualstringtree
      resultlb.Align='alClient'

      resultpanel.OnClose=function()
        resultpanel=nil
        return caFree
      end
    end

    local r=java_invokeMethodEx(frmJavaInfo.rootnode.object, Method.jmethodid, parsedsig.returntype, parameters)

    if r then      
      local i=resultlb.items.insert(0,r.Value)
      resultlb.itemindex=0
      if r.Object then
        table.insert(results, r.Object)
      end
    end

  end)

  midinfo.mid.OnClose=function()
    if resultpanel then
      resultpanel.destroy()
    end
    
    for i=1,#results do
      if results[i] then
        table.insert(jDataSource.globalsToRelease, results[i])
      end
    end
    
    results=nil
    
    return caFree
  end

  midinfo.mid.show()


end


function miJavaInfoClick(sender)
  
 -- printf("miJavaDissectClick")
  local r,err=javaInjectAgent();
  if not r then
    if err==nil then err='<no reason>' end
    messageDialog('No java agent injected:'..err, mtError, mbOK);
    return 
  end
  
  local frmJavaInfo=createFormFromFile(getAutorunPath()..'forms'..pathsep..'JavaInfo.frm')
  
  local i
  _G.jf=frmJavaInfo
  
  i=0
  while frmJavaInfos[i] do i=i+1 end
  frmJavaInfos[i]=frmJavaInfo
  frmJavaInfos.Name="frmJavaInfo"..i
  frmJavaInfos.Tag=i
  frmJavaInfo.FieldAddress=nil --init this explicitly to nil, as there is a method that exists
  frmJavaInfo.InstanceLookupMethod=1 --0=VMDebug getallClassInstances C++ call, 1=enum all objects and filter the specific class type
  
  frmJavaInfo.OnDestroy=function(f)
    --print("saving javaform position")
    local dataToSave= {
        1, --version
        f.gbClasses.Width,
        f.gbStaticFields.Height,
        f.gbFields.Height,
        
        --save the column widths
        f.stStaticFields.Header.Columns[0].Width,
        f.stStaticFields.Header.Columns[1].Width,
        f.stStaticFields.Header.Columns[2].Width,
        
        f.stFields.Header.Columns[0].Width,
        f.stFields.Header.Columns[1].Width,
        f.stFields.Header.Columns[2].Width,
        f.stFields.Header.Columns[3].Width,
    
        f.lvMethods.Columns[0].Width,
        f.lvMethods.Columns[1].Width,
        
        frmJavaInfo.InstanceLookupMethod
        
        }
      
    --for i=1,#12 do
    --  printf("%d=%d", dataToSave[i])
    --end    
    f.SaveFormPosition(dataToSave)   

    frmJavaInfos[f.tag]=nil
  end
  
  frmJavaInfo.lvClasses.OnData=function(sender, listitem) lvClassesOnData(frmJavaInfo, sender, listitem) end
  frmJavaInfo.edtClassFilter.OnChange=function(sender) edtClassFilterChange(frmJavaInfo, sender) end
  frmJavaInfo.cbCaseSensitive.OnChange=function(sender) edtClassFilterChange(frmJavaInfo, sender) end

  frmJavaInfo.lvClasses.OnSelectItem=function(sender, listitem, selected) lvClassesSelectionChange(frmJavaInfo, sender) end
  
  frmJavaInfo.btnLookupInstances.OnClick=function(sender) btnLookupInstancesClick(frmJavaInfo, sender) end
  frmJavaInfo.btnLookupInstances.Enabled=false
  
  frmJavaInfo.comboFieldBaseAddress.OnChange=function(sender) comboFieldBaseAddressOnChange(frmJavaInfo, sender) end
  
  frmJavaInfo.stFields.OnGetText=function(sender, nodeindex, columnindex, node, texttype) return stFieldsGetText(frmJavaInfo, sender, nodeindex, columnindex, node, texttype) end
  frmJavaInfo.stFields.OnFreeNode=function(sender, node) stFieldsFreeNode(frmJavaInfo, sender, node) end
  frmJavaInfo.stFields.OnExpanding=function(sender, node) return stFieldsExpanding(frmJavaInfo, sender, node) end
  frmJavaInfo.stFields.OnDblClick=function(sender) stFieldsDblClick(frmJavaInfo, sender) end 


  frmJavaInfo.stStaticFields.OnGetText=function(sender, nodeindex, columnindex, node, texttype) return stFieldsGetText(frmJavaInfo, sender, nodeindex, columnindex+1, node, texttype) end --+1 as no offset is given (0:name, 1:type, 2:value -> 1:name, 2:type, 3: value)
  frmJavaInfo.stStaticFields.OnFreeNode=function(sender, node) stFieldsFreeNode(frmJavaInfo, sender, node) end
  frmJavaInfo.stStaticFields.OnExpanding=function(sender, node) return stStaticFieldsExpanding(frmJavaInfo, sender, node) end
  frmJavaInfo.stStaticFields.OnDblClick=function(sender) stFieldsDblClick(frmJavaInfo, sender) end 
  
  frmJavaInfo.gbInheritance.OnResize=InheritanceResize
  
  frmJavaInfo.cbShowInherited.OnChange=function(sender) lvClassesSelectionChange(frmJavaInfo, frmJavaInfo.lvClasses) end
  
  frmJavaInfo.miInvokeMethod.OnClick=function(sender) miInvokeMethodClick(frmJavaInfo, sender) end
  
  
  frmJavaInfo.miFindField.OnClick=function(sender) spawnJavaSearchDialog(frmJavaInfo, 0) end
  frmJavaInfo.miFindMethod.OnClick=function(sender) spawnJavaSearchDialog(frmJavaInfo, 1) end
  
  
  frmJavaInfo.SearchPanel=createFormFromFile(getAutorunPath()..'forms'..pathsep..'JavaSearch.frm')
    
  frmJavaInfo.rootnode={} --root node    
  frmJavaInfo.rootnode.node=frmJavaInfo.stFields.getRootNode()
  
  frmJavaInfo.rootnodestatic={} --root node    
  frmJavaInfo.rootnodestatic.node=frmJavaInfo.stStaticFields.getRootNode()
  
  
  local pmLookupInstances=createPopupMenu(frmJavaInfo)
  local LookupMethod0=createMenuItem(frmJavaInfo)
  LookupMethod0.Caption='Native Dalvik VMDebug getInstancesOfClasses call'
  LookupMethod0.RadioItem=true  
  LookupMethod0.AutoCheck=true
  LookupMethod0.OnClick=function() frmJavaInfo.InstanceLookupMethod=0 end
  
  local LookupMethod1=createMenuItem(frmJavaInfo)
  LookupMethod1.Caption='JVMTI Object Tagging'
  LookupMethod1.RadioItem=true
  LookupMethod1.Checked=true
  LookupMethod1.AutoCheck=true
  LookupMethod1.OnClick=function() frmJavaInfo.InstanceLookupMethod=1 end
  
  pmLookupInstances.Items.add(LookupMethod0)
  pmLookupInstances.Items.add(LookupMethod1)
  
  frmJavaInfo.btnLookupInstances.PopupMenu=pmLookupInstances
  
    
 -- print("A")
  if jDataSource.Classes==nil then
    frmJavaInfo.caption='Java Info: Loading classes. Please wait'
    frmJavaInfo.lvClasses.enabled=false
    frmJavaInfo.edtClassFilter.enabled=false
    
    frmJavaInfo.onCloseQuery=function(s) 
      getLuaEngine().show()
      return false 
    end --prevent closing the form
    createThread(function(t)

      jDataSource.Classes=java_getLoadedClasses()    
      

      t.synchronize(function()
        frmJavaInfo.lvClasses.Items.Count=#jDataSource.Classes
        frmJavaInfo.lvClasses.enabled=true
        frmJavaInfo.edtClassFilter.enabled=true   
        frmJavaInfo.caption='Java Info'
        frmJavaInfo.onCloseQuery=nil
      end)

     -- print("after t.synchronize")      
      
    end)    
  else
    frmJavaInfo.lvClasses.Items.Count=#jDataSource.Classes
  end
  
  frmJavaInfo.loadedFormPosition,formdata=frmJavaInfo.LoadFormPosition() 
 
  if frmJavaInfo.loadedFormPosition then
    if frmJavaInfo.width>getScreenWidth() then
      frmJavaInfo.Width=getScreenWidth() * 0.9
    end  

    if formdata[1]==1 then
      frmJavaInfo.gbClasses.Width=formdata[2]
      frmJavaInfo.gbStaticFields.Height=formdata[3]
      frmJavaInfo.gbFields.Height=formdata[4]  
      frmJavaInfo.stStaticFields.Header.Columns[0].Width=formdata[5]
      frmJavaInfo.stStaticFields.Header.Columns[1].Width=formdata[6]
      frmJavaInfo.stStaticFields.Header.Columns[2].Width=formdata[7]
      
      frmJavaInfo.stFields.Header.Columns[0].Width=formdata[8]
      frmJavaInfo.stFields.Header.Columns[1].Width=formdata[9]
      frmJavaInfo.stFields.Header.Columns[2].Width=formdata[10]
      frmJavaInfo.stFields.Header.Columns[3].Width=formdata[11]

      frmJavaInfo.lvMethods.Columns[0].Width=formdata[12]
      frmJavaInfo.lvMethods.Columns[1].Width=formdata[13]
      
      frmJavaInfo.InstanceLookupMethod=formdata[14]
    end
  end 
  
  frmJavaInfo.OnClose=function()
    --print("Closing javaform")
    return caFree 
  end  
  
  frmJavaInfo.show()
end
