--Responsible for editing permissions on tables and changing the owner

function ceshare.ManageAccessList(entry)
  if entry then
   
    if ceshare.Permissions==nil then
      local f=createFormFromFile(ceshare.formpath..'Permissions.FRM')      
      ceshare.PermissionsFrm=f
      
      f.OnDestroy=function(s)
        ceshare.settings.Value['PermissionsFrm.x']=s.left
        ceshare.settings.Value['PermissionsFrm.y']=s.top
      end
      
      f.lbUserNames.Width=f.canvas.getTextWidth('this is a very long username wtf')
      f.lbUserNames.Height=f.canvas.getTextHeight('QWERTYjkl')*10
      
      f.lbUsernames.OnDblClick=function(s)
        if s.ItemIndex~=-1 then
          s.Items.delete(s.ItemIndex)
        end      
      end
      
      f.btnAddUSer.OnClick=function(s)
        local name=f.EdtUsername.Text
        if name~='' then
          s.Items.add(name)        
        end
      end
      

      local newx=ceshare.settings.Value['PermissionsFrm.x']
      local newy=ceshare.settings.Value['PermissionsFrm.y']
      
      if newx~='' then f.left=newx end
      if newy~='' then f.top=newy end
    end
    
    r=ceshare.QueryXURL('QueryAccessList.php','id='..entry.ID)
    
    ceshare.PermissionsFrm.lblId.Caption=entry.ID
    ceshare.PermissionsFrm.lbltitle.Caption=entry.Title
    
    
    
    if r and r.Access then 
      entry.access={}
      entry.access.Owner=r.Access.Owner["@Name"]  
      ceshare.PermissionsFrm.edtOwner.Text=entry.access.Owner      
      entry.access.List={}
      
      local i
      ceshare.PermissionsFrm.lbUsernames.Items.clear()
      for i=1, r.Access.AccessList:numChildren() do
        local e=r.Access.AccessList:children()[i]    
        if e then
          local name=e["@Name"]
          
          table.insert(entry.access.List,name)          
          ceshare.PermissionsFrm.lbUsernames.Items.add(name)
        end
      end     
      
      if ceshare.PermissionsFrm.showModal()==mrOK then
        --apply the changes
        local parameters='id='..entry.ID
        if entry.access.Owner~=ceshare.PermissionsFrm.edtOwner.Text then
          if messageDialog('Are you sure you wish to change the owner?', mtConfirmation,mbYes,mbNo)~=mrYes then return end
          parameters=parameters..'&owner='..ceshare.url_encode(ceshare.PermissionsFrm.edtOwner.Text)
        end
        
        for i=1,ceshare.PermissionsFrm.lbUsernames.Items.Count do
          parameters=parameters..'&users['..i..']='..ceshare.url_encode(ceshare.PermissionsFrm.lbUsernames.Items[i-1])
        end
        
        r=ceshare.QueryXURL('SetAccessList.php',parameters)
        if r then
          showMessage('Access has been updated');
        end
        
      end
    end
  end
end

function ceshare.getPermissions(entry, skipError)
--gets the permissions the current user has for this entry
  if entry==nil then return end
  entry.Permissions=nil
  
  r=ceshare.QueryXURL('CheckPermissions.php','id='..entry.ID, skipError)
  if r then
    if r.Permissions then
      entry.Permissions={}
      entry.Permissions.canUpdate=false      
      if r.Permissions["@Update"] then
        entry.Permissions.canUpdate=r.Permissions["@Update"]=='1'
      end
      
      entry.Permissions.canDelete=false      
      if r.Permissions["@Delete"] then
        entry.Permissions.canDelete=r.Permissions["@Delete"]=='1'
      end

      entry.Permissions.canManage=false      
      if r.Permissions["@Manage"] then
        entry.Permissions.canManage=r.Permissions["@Manage"]=='1'
      end
    end
  end
end