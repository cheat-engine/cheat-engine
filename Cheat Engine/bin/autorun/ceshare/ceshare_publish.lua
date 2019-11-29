function ceshare.Delete(entry)
  if entry then
    local r=ceshare.QueryXURL('DeleteTable.php','id='..entry.ID)  
    if r then
      showMessage('Table successfuly deleted') --meanie
    end
  end
end

function ceshare.PublishCheat(data,title,processname, headermd5, versionindependent, description, public, fullfilehash, secondarymodulename, secondaryfullfilehashmd5)
  local parameters=''
   
  if (processname==nil) or (processname=='') then
    ceshare.showError('processname is empty')
    return  
  end
  
  if (title==nil) or (title=='') then
    ceshare.showError('title is empty')
    return  
  end

  if (data==nil) or (data=='') then
    ceshare.showError('data is empty')
    return
  end
  
  if (description==nil) or (description=='') then
    ceshare.showError('description is empty')
    return
  end
  
  if ceshare.LoggedIn==nil then
    if not ceshare.spawnLoginDialog() then 
      return
    end
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
    showMessage('Thank you, your table has been published');
    return true
  end
  
  ceshare.ClearCredentialsIfNeeded()  
end

function ceshare.UpdateCheat(id,data,title,headermd5, versionindependent, description, public, fullfilehash, secondarymodulename, secondaryfullfilehashmd5)
  local parameters=''
  
  if id==nil then
    ceshare.showError('No id given')
    return
  end
    
  if (title==nil) or (title=='') then
    ceshare.showError('title is empty')
    return  
  end

  if (data==nil) or (data=='') then
    ceshare.showError('data is empty')
    return
  end
  
  if (description==nil) or (description=='') then
    ceshare.showError('description is empty')
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
  
  local r=ceshare.QueryXURL('EditTable.php',parameters)
  
  if r then
    showMessage('Thank you, your table has been updated');
    return true
  end

end

function ceshare.UpdateCheatClick(sender)
  ceshare.PublishCheatClick(sender,ceshare.LoadedTable )
end

function ceshare.PublishCheatClick(sender, cheatinfo) 
  if (cheatinfo==nil) and ceshare.LoadedTable and (messageDialog('Are you sure you wish to publish a new table instead of updating the existing one?', mtConfirmation, mbYes, mbNo)~=mrYes) then
    return
  end
  
  --if (cheatinfo~=nil) and (cheatinfo.HeaderMD5:lower()~=ceshare.PublishCheatFrm.lblHeaderMD5.Text.lower()) then
  --  if messageDialog('The header has changed. Isn\'t it better to do a new table?', mtConfirmation,mbYes,mbNo)~=mrYes then return end
  --end

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
    
    ceshare.PublishCheatFrm.cbPublic.checked=true
    
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
  end

  ceshare.PublishCheatFrm.btnPublish.OnClick=function(sender)   
    if (AddressList.Count==0) and 
       (getApplication().AdvancedOptions.CodeList2.Items.Count==0) and
       (getApplication().Comments.Memo1.Lines.Count==0) then
      if messageDialog('This looks like an empty table. Are you sure?',mtWarning,mbYes,mbNo)~=mrYes then return end
    end

    if ceshare.PublishCheatFrm.cbPublic.Checked then
      if messageDialog('Are you sure you wish to let \'Everyone\' overwrite your table in the ceshare system ?',mtWarning,mbYes,mbNo)~=mrYes then return end
    end
       
    local temptablepath=ceshare.path..'temptable.ct'
    saveTable(temptablepath)
    
    if MainForm.miSignTable.Visible then
      if messageDialog('Do you wish to sign this table?',mtConfirmation,mbYes,mbNo)==mrYes then
        local originalFile=MainForm.OpenDialog1.FileName
        local originalDir=MainForm.OpenDialog1.InitialDir
        MainForm.OpenDialog1.FileName=ceshare.path..'temptable.ct'
        MainForm.OpenDialog1.InitialDir=ceshare.path
        MainForm.miSignTable.doClick()
        
        MainForm.OpenDialog1.InitialDir=originalDir
        MainForm.OpenDialog1.FileName=originalFile
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
                            secondaryfullfilehash) then
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
                            secondaryfullfilehash) then
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
    ceshare.PublishCheatFrm.Caption='Update table'
    ceshare.PublishCheatFrm.edtTitle.Text=cheatinfo.Title
  else
    ceshare.PublishCheatFrm.Caption='Publish new table'
    local wl=getWindowlist()[getOpenedProcessID()]
    if wl and wl[1] then
      ceshare.PublishCheatFrm.edtTitle.Text=wl[1]
    end
  end

  ceshare.PublishCheatFrm.edtProcessName.text=process
  ceshare.PublishCheatFrm.edtProcessName.Enabled=cheatinfo==nil
  ceshare.PublishCheatFrm.cbPublic.Enabled=(cheatinfo==nil) or (cheatinfo.Public==false)
  if cheatinfo then
    ceshare.PublishCheatFrm.mDescription.Lines.Text=cheatinfo.Description
    ceshare.PublishCheatFrm.cbVersionIndependent.Checked=cheatinfo.VersionIndependent
    ceshare.PublishCheatFrm.cbPublic.Checked=cheatinfo.Public
    
    ceshare.PublishCheatFrm.cbNeedsFullFileHash.Checked=cheatinfo.FullFileHash~=nil
    ceshare.PublishCheatFrm.cbUseSecondaryModule.Checked=cheatinfo.SecondaryModuleName~=nil
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