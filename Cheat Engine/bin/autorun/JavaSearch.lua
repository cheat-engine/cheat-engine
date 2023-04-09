--javaSearch

function spawnJavaSearchDialog(frmJavaInfo, searchtype)  
  local currentScan --rule: only writable in mainthread
  local searchresults={}
  
  local frmSearch=createFormFromFile(getAutorunPath()..'forms'..pathsep..'JavaSearch.frm')  
  _G.frmJavaSearch=frmSearch  
   

  if searchtype==0 then
    frmSearch.Caption=translate('Find Field')    
    frmSearch.lvResults.Columns[1].Caption='Field'
  elseif searchtype==1 then
    frmSearch.Caption=translate('Find Method')  
    frmSearch.lvResults.Columns[1].Caption='Method'    
  else 
    return nil,'no'
  end
  
  frmSearch.lvResults.OnDblClick=function()
    -- select the class, and go to the result
    local index=frmSearch.lvResults.ItemIndex+1
    if index==0 then return end    
    if index>#searchresults then return end
    local r=searchresults[index]
    
    local jclass=r.jclass
    local Class=jDataSource.Classes.jClassLookup[jclass]
    frmJavaInfo.edtClassFilter.Text=Class.signature    
    
    if frmJavaInfo.lvClasses.Items.Count>0 then
      frmJavaInfo.lvClasses.ItemIndex=0
    end
    
    --highlight the field/method
    frmJavaInfo.show()
    
    if searchresults[index].fieldid then
      --field
      local fieldid=searchresults[index].fieldid
      local Field=Class.Fields.jfieldidLookup[fieldid] 
      local st
      if Field.static then
        --search for it in the static list
        st=frmJavaInfo.stStaticFields
      else
        --search for it in the field list
        st=frmJavaInfo.stFields
      end      
      
      local n=st.getFirstChild(nil)
      while n do
        local nd=getRef(st.getNodeDataAsInteger(n))
        
        if nd.field.jfieldid==fieldid then
          
          st.setFocus()
          st.Selected[n]=true
          st.FocusedNode=n          
          return
        end
      
        n=st.getNextSibling(n)      
      end
      
    elseif searchresults[index].methodid then
      local Method=Class.Methods.jmethodidLookup[searchresults[index].methodid ]
      local methodid=searchresults[index].methodid
      
      
      frmJavaInfo.lvMethods.setFocus()
      for i=1,frmJavaInfo.lvMethods.Items.Count-1 do
        if frmJavaInfo.CurrentlySelectedClass.Methods[i].jmethodid==methodid then
          frmJavaInfo.lvMethods.itemIndex=i-1
          return
        end       
      end
    end
    
  end
  
  
  frmSearch.btnScan.OnClick=function()
    --do the scan (todo: threaded)    
    if searchtype==0 then
      searchresults=java_findFields(frmSearch.edtSearchName.Text, frmSearch.edtSearchSignature.Text, frmSearch.cbCaseSensitive.Checked)    
    elseif searchtype==1 then
      searchresults=java_findMethods(frmSearch.edtSearchName.Text, frmSearch.edtSearchSignature.Text, frmSearch.cbCaseSensitive.Checked)        
    end
    
    frmSearch.lvResults.Items.Count=#searchresults
  end
  
  frmSearch.lvResults.OnData=function(sender, listitem)
    local index=listitem.index+1
    if searchresults and searchresults[index] then
      if index>#searchresults then 
        listitem.caption='count bug'
        return
      end
      local jclass=searchresults[index].jclass
      local Class=jDataSource.Classes.jClassLookup[jclass]
      if Class then      
        listitem.caption=Class.signature
        
        if Class.Fields==nil then
          Class.Fields, Class.StaticFields=java_getClassFields(jclass)
        end
        
        if Class.Methods==nil then
          Class.Methods=java_getClassMethods(jclass)
        end
        
        if searchresults[index].fieldid then        
          local fieldid=searchresults[index].fieldid 
          local Field=Class.Fields.jfieldidLookup[fieldid]     

          
          if Field then            
            listitem.SubItems.add(Field.name..' : '..Field.signature);
          else
            listitem.SubItems.add('field bug')
          end
        elseif searchresults[index].methodid then
          local methodid=searchresults[index].fieldid 
          local Method=Class.Methods.jmethodidLookup[methodid]
          if Method then          
            listitem.SubItems.add(Method.name..' : '..Method.signature);            
          else
            listitem.SubItems.add('method bug')
          end
        end
      else
        listitem.caption='class bug'
      end     
    else
      listitem.caption='bug'
    end
    
  end
  
  frmSearch.OnClose=function(f)
    frmSearch=nil
    return caFree
  end
  
  frmSearch.OnDestroy=function(f)
    --print("saving frmSearch named "..f.name)
    local dataToSave= {
        1, --version of the saved data (I should do this everywhere)
        f.lvResults.Columns[0].Width,
        f.lvResults.Columns[1].Width
    }         
    f.SaveFormPosition(dataToSave)         
  end
  
  local formdata
  --print("loading data for frmSearch named "..frmSearch.Name)
  frmSearch.loadedFormPosition,formdata=frmSearch.LoadFormPosition()  
  if frmSearch.loadedFormPosition then
    if formdata[1]==1 then
      frmSearch.lvResults.Columns[0].Width=formdata[2]
      frmSearch.lvResults.Columns[1].Width=formdata[3]
    end
  end
  
  frmSearch.show()
  
  frmSearch.edtSearchName.setFocus()

end