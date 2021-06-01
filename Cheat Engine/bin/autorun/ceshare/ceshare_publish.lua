local function isWindowVisible(winhandle)
  return executeCodeLocal('IsWindowVisible',winhandle)~=0
end

local function getBaseParentFromWindowHandle(winhandle)
  local i=0
  local last=winhandle

  while winhandle and (winhandle~=0) and (i<10000) do
    last=winhandle
    winhandle=getWindow(winhandle, GW_HWNDOWNER)
    i=i+1
  end;

  return last
end

function ceshare.getProcessTitle(pid)
  local w=getWindow(getForegroundWindow(), GW_HWNDFIRST)

  local bases={}

  while w and (w~=0) do
    if getWindowProcessID(w)==pid then
      if isWindowVisible(w) then
        local h=getBaseParentFromWindowHandle(w)
        local c=getWindowCaption(h)
        if isWindowVisible(h) and (c~='') then
          bases[h]=c
        end
      end
    end
    w=getWindow(w,GW_HWNDNEXT)
  end


  for h,n in pairs(bases) do
    return n --just hope for the best...
  end
end

function ceshare.getCurrentProcessTitle()
  return ceshare.getProcessTitle(getOpenedProcessID())
end



function ceshare.Delete(entry)
  if entry then
    if messageDialog(translate('Are you sure you wish to delete this table?'),mtWarning,mbYes,mbNo)==mrYes then
      local r=ceshare.QueryXURL('DeleteTable.php','id='..entry.ID)  
      if r then
        if ceshare.CheatBrowserFrm and ceshare.CheatBrowserFrm.Visible then
          ceshare.CheckForCheatsClick()
        end
        
        if ceshare.UpdateOrNewFrm and ceshare.UpdateOrNewFrm.Visible then
          ceshare.PublishCheatClick()
        end      
        showMessage(translate('Table successfuly deleted')) --meanie
      end
    end
  end
end

function ceshare.PublishCheat(data,title,processname, headermd5, versionindependent, description, public, fullfilehash, secondarymodulename, secondaryfullfilehashmd5, url)


  local parameters=''
   
  if (processname==nil) or (processname=='') then
    ceshare.showError(translate('processname is empty'))
    return  
  end
  
  if (title==nil) or (title=='') then
    ceshare.showError(translate('title is empty'))
    return  
  end

  if (data==nil) or (data=='') then
    ceshare.showError(translate('data is empty'))
    return
  end
  
  if (description==nil) or (description=='') then
    ceshare.showError(translate('description is empty'))
    return
  end
  


  parameters='data='..ceshare.url_encode(data)
  parameters=parameters..'&title='..ceshare.url_encode(title)
  parameters=parameters..'&processname='..ceshare.url_encode(processname);
  parameters=parameters..'&description='..ceshare.url_encode(description)   
  if headermd5~=nil then parameters=parameters..'&headermd5='..ceshare.url_encode(headermd5) end    
  if public~=nil then parameters=parameters..'&public='..ceshare.url_encode(public) end  
  if versionindependent~=nil then parameters=parameters..'&versionindependent='..ceshare.url_encode(versionindependent) end
  if fullfilehash~=nil then parameters=parameters..'&fullfilehash='..ceshare.url_encode(fullfilehash) end
  if secondarymodulename~=nil then parameters=parameters..'&secondarymodulename='..ceshare.url_encode(secondarymodulename) end
  if secondaryfullfilehashmd5~=nil then parameters=parameters..'&secondaryfullfilehashmd5='..ceshare.url_encode(secondaryfullfilehashmd5) end
  if url~=nil then parameters=parameters..'&url='..ceshare.url_encode(url) end
 
  if isKeyPressed(VK_CONTROL)==false then  --control lets you get a new script if needed
    local secondaryIdentifierCode=ceshare.secondaryIdentifierCode.Value[processname:lower()]
    if secondaryIdentifierCode and secondaryIdentifierCode~='' then
      local value,param=loadstring(secondaryIdentifierCode)()
      if value and param then
        parameters=parameters..'&secondaryidentifier='..ceshare.url_encode(param)
      end
    end
  end    
  
 
  local r=ceshare.QueryXURL('PublishTable.php',parameters)
  
  if r then
    showMessage(translate('Thank you, your table has been published'));
    return true
  end
end

function ceshare.UpdateCheat(id,data,title,headermd5, versionindependent, description, public, fullfilehash, secondarymodulename, secondaryfullfilehashmd5, url)
  local parameters=''
  
  if id==nil then
    ceshare.showError(translate('No id given'))
    return
  end
    
  if (title==nil) or (title=='') then
    ceshare.showError(translate('title is empty'))
    return  
  end

  if (data==nil) or (data=='') then
    ceshare.showError(translate('data is empty'))
    return
  end
  
  if (description==nil) or (description=='') then
    ceshare.showError(translate('description is empty'))
    return
  end
  
  if ceshare.LoggedIn==nil then
    if not ceshare.spawnLoginDialog() then 
      return
    end
  end
  

  parameters=parameters..'id='..id
  parameters=parameters..'&data='..ceshare.url_encode(data)
  parameters=parameters..'&title='..ceshare.url_encode(title)
  parameters=parameters..'&description='..ceshare.url_encode(description)   
  if headermd5~=nil then parameters=parameters..'&headermd5='..ceshare.url_encode(headermd5) end    
  if public~=nil then parameters=parameters..'&public='..ceshare.url_encode(public) end  
  if versionindependent~=nil then parameters=parameters..'&versionindependent='..ceshare.url_encode(versionindependent) end
  if fullfilehash~=nil then parameters=parameters..'&fullfilehash='..ceshare.url_encode(fullfilehash) end
  if secondarymodulename~=nil then parameters=parameters..'&secondarymodulename='..ceshare.url_encode(secondarymodulename) end
  if secondaryfullfilehashmd5~=nil then parameters=parameters..'&secondaryfullfilehashmd5='..ceshare.url_encode(secondaryfullfilehashmd5) end
  if url~=nil then parameters=parameters..'&url='..ceshare.url_encode(url) end
  
  
  local r=ceshare.QueryXURL('EditTable.php',parameters)
  
  if r then
    showMessage(translate('Thank you, your table has been updated'));
    return true
  end

end

function ceshare.PublishCheatClick(sender, cheatinfo) 
  local loggedin=ceshare.LoggedIn or false
  --if not logged in, log in now
  if not loggedin then
    if not ceshare.spawnLoginDialog() then 
      return
    end
  end
  
  if cheatinfo then
    ceshare.publishOrUpdate(cheatinfo)  
    return    
  end
  
  --spawn a window that shows all tables with this processname that the current user has modify rights to
 
  if ceshare.UpdateOrNewFrm==nil then
    local f=createFormFromFile(ceshare.formpath..'UpdateOrNew.FRM') 
    
    f.AutoSize=true
    f.AutoSize=false
    
    local h=f.lvCheats.Canvas.getTextHeight('XXX')*10
    local hdelta=h-f.lvCheats.Height
    
    if hdelta>0 then
      f.height=f.height+hdelta      
    end
    
    local headerwidth=0
    local i
    for i=0,f.lvCheats.Columns.Count-1 do
      local w=f.lvCheats.Columns[i].Width
      local neededw=f.Canvas.getTextWidth(' '..f.lvCheats.Columns[i].Caption..' ')
      if w<neededw then
        f.lvCheats.Columns[i].Autosize=false
        f.lvCheats.Columns[i].Width=neededw
        w=neededw
      end
      headerwidth=headerwidth+w    
      
    end
    f.ClientWidth=headerwidth+10
    
    f.lvCheats.OnDblClick=function(s)
      f.btnChoose.doClick()
    end
    
    f.rbUpdate.OnChange=function(s)
      f.lvCheats.Enabled=s.Checked
      f.btnChoose.Caption=translate('Update table')
    end
    
    f.rbPublish.OnChange=function(s)      
      f.btnChoose.Caption=translate('Publish new table')
    end    
   
    f.btnChoose.OnClick=function(s)
      local cheatinfo
      
      if f.rbUpdate.checked then
        local itemindex=f.lvCheats.ItemIndex
            
        if itemindex==-1 and f.rbUpdate.checked then
          messageDialog(translate('Please select a cheattable to update'), mtError, mbOK);
          return
        end
        
        if itemindex<#ceshare.CurrentUpdateQuery then
          cheatinfo=ceshare.CurrentUpdateQuery[itemindex+1]                
          ceshare.publishOrUpdate(cheatinfo)          
        else
          messageDialog(translate('Invalid background update query list'), mtError, mbOK);
        end
      else
        ceshare.publishOrUpdate()
      end     
      
      
      f.hide()
    end;

    
    ceshare.UpdateOrNewFrm=f
  end;
  
  
  
  --get the table list of entries the user can change
  
  ceshare.CurrentUpdateQuery=ceshare.QueryCurrentProcess(true)
  ceshare.UpdateOrNewFrm.rbUpdate.checked=true
  ceshare.UpdateOrNewFrm.rbUpdate.OnChange(ceshare.UpdateOrNewFrm.rbUpdate)
  
  
  if ceshare.CurrentUpdateQuery==nil or #ceshare.CurrentUpdateQuery==0 then
    --skip to publish instantly
    ceshare.UpdateOrNewFrm.rbPublish.Checked=true
    ceshare.UpdateOrNewFrm.btnChoose.doClick()
  else
    ceshare.UpdateOrNewFrm.lvCheats.clear()
    local i
    for i=1,#ceshare.CurrentUpdateQuery do 
      local li=ceshare.UpdateOrNewFrm.lvCheats.Items.add()
      li.Caption=ceshare.CurrentUpdateQuery[i].Title
      local owner=ceshare.CurrentUpdateQuery[i].Owner
      local editor=ceshare.CurrentUpdateQuery[i].LastEditor
      if editor==owner then 
        li.SubItems.add(owner)
      else
        li.SubItems.add(editor..' (owner:'..owner..')')
      end
      
      if ceshare.CurrentUpdateQuery[i].Public then
        li.SubItems.add('     yes     ')
      else
        li.SubItems.add('     ')
      end
      
      if ceshare.CurrentUpdateQuery[i].VersionIndependent then
        li.SubItems.add('      yes      ')
      else
        li.SubItems.add('             ')
      end 

      if ceshare.CurrentUpdateQuery[i].Signed then
        li.SubItems.add('     yes     ')
      else    
        li.SubItems.add('           ')  --signed
      end
      
      if (ceshare.LoadedTable) and (ceshare.LoadedTable.ID==ceshare.CurrentUpdateQuery[i].ID) then
        --select the table if a table was loaded and you have access to update
        li.Selected=true
        ceshare.UpdateOrNewFrm.lvCheats.ItemIndex=li.Index
      end         
    end
    ceshare.UpdateOrNewFrm.show()    
  end
  
end

function ceshare.publishOrUpdate(cheatinfo) --cheatinfo is a set if an update
  if ceshare.PublishCheatFrm==nil then
    ceshare.PublishCheatFrm=createFormFromFile(ceshare.formpath..'PublishCheat.frm')    
    --configure base state and add events
    
    
    ceshare.PublishCheatFrm.cbVersionIndependent.OnChange=function(s)
      ceshare.PublishCheatFrm.lblHeaderMD5Text.visible=true --not s.Checked
      ceshare.PublishCheatFrm.lblHeaderMD5.visible=true --not s.Checked
      ceshare.PublishCheatFrm.pnlFullFileHash.visible=not s.Checked
      ceshare.PublishCheatFrm.cbUseSecondaryModule.visible=not s.checked
      if s.checked then      
        ceshare.PublishCheatFrm.cbUseSecondaryModule.checked=false
        ceshare.PublishCheatFrm.cbNeedsFullFileHash.checked=false      
      end
    end
    
    
    
    ceshare.PublishCheatFrm.cbNeedsFullFileHash.OnChange=function(s)
      local ml=enumModules()
      ceshare.PublishCheatFrm.lblFullHeaderMD5Text.visible=s.Checked
      ceshare.PublishCheatFrm.lblFullHeaderMD5.visible=s.Checked
      ceshare.PublishCheatFrm.lblFullHeaderMD5.Caption=md5file(ml[1].PathToFile)
    end
    
    ceshare.PublishCheatFrm.cbUseSecondaryModule.OnChange=function(s)
      --when the 'Use secondary module' checkbox is ticked
      ceshare.PublishCheatFrm.lblModulename.visible=s.checked
      ceshare.PublishCheatFrm.cbModuleName.visible=s.checked      
      ceshare.PublishCheatFrm.pnlModuleFullHash.visible=s.checked
    end
    
    ceshare.PublishCheatFrm.cbModuleNeedsFullFileHash.OnChange=function(s)
      ceshare.PublishCheatFrm.lblFullModuleHeaderMD5Text.visible=s.checked
      ceshare.PublishCheatFrm.lblFullModuleHeaderMD5.visible=s.checked
      
      if s.checked then
        ceshare.PublishCheatFrm.cbModuleName.OnChange(s)
      end
    end
    
    ceshare.PublishCheatFrm.cbModuleName.OnChange=function(s)
      if ceshare.PublishCheatFrm.cbModuleNeedsFullFileHash.Checked then      
        if ceshare.PublishCheatFrm.cbModuleName.Text=='' then
          ceshare.PublishCheatFrm.lblFullModuleHeaderMD5.caption='<Select a module>'
        else
          local ml=enumModules()
          for i=1,#ml do
            if ml[i].Name:lower()==ceshare.PublishCheatFrm.cbModuleName.Text:lower() then
              ceshare.PublishCheatFrm.lblFullModuleHeaderMD5.caption=md5file(ml[i].PathToFile)
              ceshare.PublishCheatFrm.lblFullModuleHeaderMD5.Font.Color=ceshare.PublishCheatFrm.cbModuleNeedsFullFileHash.Font.Color            
              ceshare.PublishCheatFrm.cbModuleName.Font.Color=ceshare.PublishCheatFrm.cbModuleNeedsFullFileHash.Font.Color            
              return
            end
          end

          ceshare.PublishCheatFrm.lblFullModuleHeaderMD5.Caption='<Module not found>'
          ceshare.PublishCheatFrm.lblFullModuleHeaderMD5.Font.Color=0x0000ff
          ceshare.PublishCheatFrm.cbModuleName.Font.Color=0x0000ff    
        end 
      end      
       
    end
    
    
    
    ceshare.PublishCheatFrm.btnCancel.OnClick=function(s)
      ceshare.PublishCheatFrm.close()    
    end
   
    ceshare.PublishCheatFrm.lblHeaderMD5Text.visible=false
    ceshare.PublishCheatFrm.lblHeaderMD5.visible=false
    ceshare.PublishCheatFrm.pnlFullFileHash.visible=false    
    ceshare.PublishCheatFrm.lblFullHeaderMD5Text.visible=false
    ceshare.PublishCheatFrm.lblFullHeaderMD5.visible=false

    ceshare.PublishCheatFrm.lblModulename.visible=false
    ceshare.PublishCheatFrm.cbModuleName.visible=false

    ceshare.PublishCheatFrm.pnlModuleFullHash.visible=false
    ceshare.PublishCheatFrm.lblFullModuleHeaderMD5Text.visible=false
    ceshare.PublishCheatFrm.lblFullModuleHeaderMD5.visible=false    

    ceshare.PublishCheatFrm.cbPublic.checked=false

    ceshare.Position='poScreenCenter'
    
    --position and size saving
    ceshare.PublishCheatFrm.OnDestroy=function(s)
      ceshare.settings.Value['PublishCheatFrm.x']=s.left
      ceshare.settings.Value['PublishCheatFrm.y']=s.top
      ceshare.settings.Value['PublishCheatFrm.width']=s.width
      ceshare.settings.Value['PublishCheatFrm.height']=s.height
    end
    
    
    local newx=ceshare.settings.Value['PublishCheatFrm.x']
    local newy=ceshare.settings.Value['PublishCheatFrm.y']
    local newwidth=ceshare.settings.Value['PublishCheatFrm.width']
    local newheight=ceshare.settings.Value['PublishCheatFrm.height']
    
    if (newwidth~='') or (newheight~='') then
      ceshare.PublishCheatFrm.AutoSize=false
    end
    
    if newx~='' then ceshare.PublishCheatFrm.Left=newx end
    if newy~='' then ceshare.PublishCheatFrm.Top=newy end
    if newwidth~='' then ceshare.PublishCheatFrm.Width=newwidth end
    if newheight~='' then ceshare.PublishCheatFrm.Height=newheight end    
    
    ceshare.PublishCheatFrm.lblFullModuleHeaderMD5.caption=''
    ceshare.PublishCheatFrm.lblFullHeaderMD5.Caption=''
  end
  
  ceshare.PublishCheatFrm.btnPublish.OnClick=function(sender)   
    if ceshare.PublishCheatFrm.cbUseSecondaryModule.Checked then
      if ceshare.PublishCheatFrm.cbModuleName.Text=='' then
        messageDialog('Missing module',mtError,mbOK)
        return
      end    
    end
    
    if (AddressList.Count==0) and 
       (getApplication().AdvancedOptions.CodeList2.Items.Count==0) and
       (getApplication().Comments.Memo1.Lines.Count==0) then
      if messageDialog(translate('This looks like an empty table. Are you sure?'),mtWarning,mbYes,mbNo)~=mrYes then return end
    end

    if ceshare.PublishCheatFrm.cbPublic.Enabled and ceshare.PublishCheatFrm.cbPublic.Checked then
      if messageDialog(translate('Are you sure you wish to let \'Everyone\' overwrite your table in the ceshare system ?'),mtWarning,mbYes,mbNo)~=mrYes then return end
    end
       
    local temptablepath=ceshare.path..'temptable.ct'
    saveTable(temptablepath)
    
    if MainForm.miSignTable.Visible then
      if messageDialog(translate('Do you wish to sign this table?'),mtConfirmation,mbYes,mbNo)==mrYes then
        local r,msg=signTable(ceshare.path..'temptable.ct')
        if not r then
          messageDialog(msg,mtError,mbOK)
          return
        end
      end
    end    
    
    local s=createStringList()
    s.loadFromFile(temptablepath)
    
        
    local fullfilehash,secondarymodulename,secondaryfullfilehash
    
    if ceshare.PublishCheatFrm.cbNeedsFullFileHash.Checked then
      fullfilehash=ceshare.PublishCheatFrm.lblFullHeaderMD5.Caption
    end
    
    if ceshare.PublishCheatFrm.cbUseSecondaryModule.Checked then
      secondarymodulename=ceshare.PublishCheatFrm.cbModuleName.Text 
      secondaryfullfilehash=ceshare.PublishCheatFrm.lblFullModuleHeaderMD5.caption
    end
    

    

    if cheatinfo then
      if ceshare.UpdateCheat(cheatinfo.ID,
                            s.Text, --data
                            ceshare.PublishCheatFrm.edtTitle.Text,                              
                            ceshare.PublishCheatFrm.lblHeaderMD5.Caption, --headermd5
                            ceshare.PublishCheatFrm.cbVersionIndependent.checked, --versionindependent
                            ceshare.PublishCheatFrm.mDescription.Lines.Text, --description
                            ceshare.PublishCheatFrm.cbPublic.Checked,                                
                            fullfilehash,
                            secondarymodulename,
                            secondaryfullfilehash,
                            ceshare.PublishCheatFrm.edtURL.Text
                            ) then
        ceshare.PublishCheatFrm.close() 
      end          
    else
      if ceshare.PublishCheat(s.Text, --data
                            ceshare.PublishCheatFrm.edtTitle.Text,
                            ceshare.PublishCheatFrm.edtProcessName.Text, --processname
                            ceshare.PublishCheatFrm.lblHeaderMD5.Caption, --headermd5
                            ceshare.PublishCheatFrm.cbVersionIndependent.checked, --versionindependent
                            ceshare.PublishCheatFrm.mDescription.Lines.Text, --description
                            ceshare.PublishCheatFrm.cbPublic.Checked,                                
                            fullfilehash,
                            secondarymodulename,
                            secondaryfullfilehash,
                            ceshare.PublishCheatFrm.edtURL.Text
                            ) then
        ceshare.PublishCheatFrm.close()                            
      end
    end
       
    s.destroy()
  
  end      

  
  
  --fill in header and processname
  local headermd5
  headermd5=ceshare.getCurrentProcessHeaderMD5()
  
  local ml=enumModules()
  
  ceshare.PublishCheatFrm.cbModuleName.Items.clear()
  for i=1,#ml do    
    ceshare.PublishCheatFrm.cbModuleName.Items.add(ml[i].Name)
  end


      
  if cheatinfo then
    ceshare.PublishCheatFrm.Caption=translate('Update table')
    ceshare.PublishCheatFrm.edtTitle.Text=cheatinfo.Title
    
    if cheatinfo.public then
      ceshare.PublishCheatFrm.cbPublic.checked=true
      ceshare.PublishCheatFrm.cbPublic.enabled=false
    else
      ceshare.PublishCheatFrm.cbPublic.checked=false
      ceshare.PublishCheatFrm.cbPublic.enabled=true    
    end    
  else
    ceshare.PublishCheatFrm.cbPublic.checked=false
    ceshare.PublishCheatFrm.cbPublic.enabled=true
  
    ceshare.PublishCheatFrm.Caption=translate('Publish new table')
    local pt=ceshare.getCurrentProcessTitle()
    if pt then
      ceshare.PublishCheatFrm.edtTitle.Text=pt
    end
  end

  ceshare.PublishCheatFrm.edtProcessName.text=process
  ceshare.PublishCheatFrm.edtProcessName.Enabled=cheatinfo==nil
  ceshare.PublishCheatFrm.cbPublic.Enabled=(cheatinfo==nil) or (cheatinfo.Public==false)
  if cheatinfo then
    ceshare.PublishCheatFrm.mDescription.Lines.Text=cheatinfo.Description
    ceshare.PublishCheatFrm.cbVersionIndependent.Checked=cheatinfo.VersionIndependent
    ceshare.PublishCheatFrm.cbPublic.Checked=cheatinfo.Public
    
    ceshare.PublishCheatFrm.cbNeedsFullFileHash.Checked=(cheatinfo.FullFileHash~=nil) and (cheatinfo.FullFileHash~='')
    ceshare.PublishCheatFrm.cbUseSecondaryModule.Checked=(cheatinfo.SecondaryModuleName~=nil) and (cheatinfo.SecondaryModuleName~='')
    ceshare.PublishCheatFrm.cbModuleName.Text=cheatinfo.SecondaryModuleName     
    ceshare.PublishCheatFrm.cbModuleName.OnChange(ceshare.PublishCheatFrm.cbModuleName)
    
    ceshare.PublishCheatFrm.cbVersionIndependent.Checked=cheatinfo.VersionIndependent
   
  end
  ceshare.PublishCheatFrm.cbVersionIndependent.OnChange(ceshare.PublishCheatFrm.cbVersionIndependent)  
 
  if headermd5==nil then
    ceshare.PublishCheatFrm.lblHeaderMD5.Caption=''
    ceshare.PublishCheatFrm.cbNeedsFullFileHash.enabled=false    
  else
    ceshare.PublishCheatFrm.lblHeaderMD5.Caption=headermd5
    ceshare.PublishCheatFrm.cbNeedsFullFileHash.enabled=true
  end

  ceshare.PublishCheatFrm.show() --clicking publish will do the rest
end
