function ceshare.enumModules2()
  local m=enumModules()
  local r={}
  
  for i=1,#m do
    r[m[i].Name:lower()]=m[i]
  end
  
  return r
end

function ceshare.QueryProcessCheats(processname, headermd5, updatableOnly)
  local modulelist=ceshare.enumModules2()
  local result=nil
  local parameters='processname='..ceshare.url_encode(processname)
  
  if isKeyPressed(VK_CONTROL)==false then  --control lets you get a new script if needed
    local secondaryIdentifierCode=ceshare.secondaryIdentifierCode.Value[processname:lower()]
    if secondaryIdentifierCode and secondaryIdentifierCode~='' then
      local value,param=loadstring(secondaryIdentifierCode)()
      if value and param then
        parameters=parameters..'&secondaryidentifier='..ceshare.url_encode(param)
      end
    end
  end
  
  if updatableOnly then
    parameters=parameters..'&updatableOnly=1';
  end

  
  
  
  --local r=ceshare.getInternet().postURL(url,parameters)
    --local s=ceshare.xmlParser:ParseXmlText(r)
    local s=ceshare.QueryXURL('QueryProcessTables.php', parameters)
    if s then
      if s.CheatList then
        --parse the list
        local i
        --if s.CheatList.Cheat then
          --there are results
          result={}
          for i=1, s.CheatList:numChildren() do
            local CheatEntry=s.CheatList:children()[i]
            if CheatEntry then
              local entry={}
              entry.ID=tonumber(CheatEntry["@ID"])
              entry.Title=CheatEntry["@title"]
              entry.HeaderMD5=CheatEntry["@headermd5"]
              entry.Owner=CheatEntry["@username"]
              entry.LastEditor=CheatEntry["@editorname"]
              entry.VersionIndependent=CheatEntry["@versionIndependent"]=='1'
              entry.Public=CheatEntry["@public"]=='1'
              entry.Rating=tonumber(CheatEntry["@ratingtotal"]) or 0
              entry.RatingCount=tonumber(CheatEntry["@ratingcount"]) or 0
              entry.AccessCount=tonumber(CheatEntry["@accesscount"]) or 0
              entry.LastEditTime=CheatEntry["@lastEditTime"]
              entry.FullFileHash=CheatEntry["@fullfilehashmd5"]
              entry.SecondaryModuleName=CheatEntry["@secondarymodulename"]
              entry.SecondaryFullFileHash=CheatEntry["@secondaryfullfilehashmd5"]
              entry.CheckCode=CheatEntry["@luaScriptToCheckForMatch"]
              entry.Description=CheatEntry["@description"]
              entry.Url=CheatEntry["@url"]
              entry.DataType=CheatEntry["@datatype"]
              entry.Signed=CheatEntry["@signed"]=='1'
              entry.YourRating=tonumber(ceshare.settings.Value['voted'..entry.ID])
              
              entry.Match=1;
              
    --calculate the match
    --versionIndependant with matching header is always 100%
    --versionIndependant with nonmatching header is 80%
    --non versionIndependent with matching header and fullfilehash is 100%
    --non versionIndependent with matching header is 90%
    --non versionIndependant and non matching header is 1%
    
              
              local matchingHeader=string.lower(entry.HeaderMD5)==string.lower(headermd5)
                           
              if entry.VersionIndependent then  
                if matchingHeader then
                  entry.Match=100
                else
                  entry.Match=80
                end                               
              else
                --not versionIndependent
                if matchingHeader then                 
                  if entry.FullFileHash and entry.FullFileHash~='' then
                    local me=modulelist[processname:lower()]
                    if me then
                      if not me.md5 then
                        me.md5=md5file(me.PathToFile)
                      end
                      
                      if me.md5 then                      
                        if me.md5:lower()==entry.FullFileHash:lower() then
                          entry.Match=100
                        else
                          entry.Match=50                          
                        end                      
                      else 
                        entry.Match=0 --file gone...
                      end
                    else
                      entry.Match=0 --the process exe can not be found? Why am I even here?
                    end
                    
                  else
                    entry.Match=100
                  end
                  
                end
                
                if entry.SecondaryModuleName and entry.SecondaryModuleName~='' then
                  local me=modulelist[entry.SecondaryModuleName:lower()]
                  if me then
                    if not me.md5 then
                      me.md5=md5file(me.PathToFile)
                    end 
                    if me.md5 then
                      if me.md5:lower()==entry.SecondaryModuleName:lower() then
                        entry.Match=100
                      else
                        entry.Match=50
                      end  
                    else
                      entry.Match=0
                    end
                  else
                    entry.Match=0 --module not loaded
                  end
                end                
              end
              
              local signedvalue=0;
              if entry.Signed then signedvalue=1 end
                            
              entry.sortscore=entry.Match*1000+signedvalue*10
              if entry.RatingCount>0 then
                entry.sortscore=entry.sortscore+(entry.Rating/(entry.RatingCount*0.9)) --total votes does count as well
              end

              table.insert(result, entry)
            end
          end
          
         
        --end
      end
    end


  --Sort the list by Match , Signed, and Score (in that order)
  if result then
    table.sort(result, function(a,b)
      return a.sortscore>b.sortscore
    end)
  end
  
  return result
end



function ceshare.getCurrentProcessHeaderMD5()
  local pid=getOpenedProcessID()
  if (pid) and (pid~=0) then
    local modulelist=enumModules()
    if (modulelist) and (#modulelist>0) then
      local headermd5=md5memory(modulelist[1].Address,4096)
      return headermd5
    end
  end

end

function ceshare.QueryCurrentProcess(updatableOnly)
  local processnamemd5
  local headermd5
  headermd5=ceshare.getCurrentProcessHeaderMD5()

  if headermd5 then
    return ceshare.QueryProcessCheats(process, headermd5, updatableOnly)
  end
end

function ceshare.CheckForCheatsClick(s, overrideProcess)
  --spawn the cheatbrowser
  
  if ceshare.CheatBrowserFrm==nil then
    local f=createFormFromFile(ceshare.formpath..'BrowseCheats.FRM')
    f.lblProcessName.Caption=process
    f.Name='ceshare_CheatBrowserFrm'

    ceshare.CheatBrowserFrm=f

    local h=f.lvCheats.Canvas.getTextHeight('XXX')*10
    f.lvCheats.Constraints.MinHeight=h
    f.pnlDescription.Constraints.MinHeight=h
    
    --configure base state and add events
    
    ceshare.CheatBrowserFrm.OnDestroy=function(f)
      f.saveFormPosition({ceshare.CheatBrowserFrm.CEPanel1.Height})
    end

    ceshare.CheatBrowserFrm.lvCheats.Font.Size=12
    ceshare.CheatBrowserFrm.lvCheats.OnSelectItem=function(sender, listitem, selected)
      if selected and listitem.index then
        local desc=ceshare.CurrentQuery[listitem.index+1].Description
        
        ceshare.CheatBrowserFrm.mDescription.Lines.Text=desc
        
        
        --[[local rating=ceshare.CurrentQuery[listitem.index+1].YourRating    
        if rating then
          --trigger a refresh
          ceshare.RateStars[rating].img.OnMouseLeave(ceshare.RateStars[rating]) 
        end--]]
        
        ceshare.CheatBrowserFrm.lblContact.Visible=true
        ceshare.CheatBrowserFrm.lblContact.Font.Size=7
      end
      
      ceshare.RateStars[1].img.OnMouseLeave(ceshare.RateStars[1]) 
  

    end
    
    ceshare.CheatBrowserFrm.lvCheats.OnAdvancedCustomDrawSubItem=function(Sender, Item, SubItemIndex, State, Stage)
      if SubItemIndex==7 then --link subitem
        if ceshare.CurrentQuery[Item.index+1].Url and (ceshare.CurrentQuery[Item.index+1].Url~='') then
          if Stage==cdPostPaint then
            local rect=Item.DisplayRectSubItem(7,drBounds)  
            Sender.Canvas.stretchDraw(rect, ceshare.linkButton)
          end
        
        end
      end      

      return true --return true for DefaultDraw
    end
    
    ceshare.CheatBrowserFrm.lvCheats.OnMouseMove=function(sender, x, y)
      local item=sender.getItemAt(x,y)
      if item then
        if ceshare.CurrentQuery[item.index+1].Url and (ceshare.CurrentQuery[item.index+1].Url~='') then
          local linkrect=item.DisplayRectSubItem(7,drBounds)
        
          if (x>linkrect.Left) and (x<linkrect.Right) and
             (y>linkrect.Top) and (y<linkrect.Bottom) then
            ceshare.CheatBrowserFrm.lvCheats.Cursor=crHandPoint
            return 
          end 
        end        
      end
      if ceshare.CheatBrowserFrm.lvCheats.Cursor==crHandPoint then
        ceshare.CheatBrowserFrm.lvCheats.Cursor=crDefault  
      end
    end

    ceshare.CheatBrowserFrm.lvCheats.OnMouseDown=function(sender, button, x, y)
      local item=sender.getItemAt(x,y)
      if item then      
        local url=ceshare.CurrentQuery[item.index+1].Url
        if url and (url~='') then   
          local linkrect=item.DisplayRectSubItem(7,drBounds)  
          if (x>linkrect.Left) and (x<linkrect.Right) and
             (y>linkrect.Top) and (y<linkrect.Bottom) then             
            if (string.sub(url,1,7)=='http://') or string.sub(url,1,8)=='https://' then
              shellExecute(url)
            end
          end
        end  
      end
    end
    
    
    ceshare.CheatBrowserFrm.btnLoadTable.OnClick=function(s)
      local index=ceshare.CheatBrowserFrm.lvCheats.ItemIndex
      if index~=-1 then
        --load the selected table
        local url=ceshare.base..'GetTable.php'
        local parameters='id='..ceshare.CurrentQuery[index+1].ID        
        local cheattable=ceshare.getInternet().postURL(url,parameters) --not an xml result, so don't use QueryXURL
        local cheattabless=createStringStream(cheattable)
        
        if ceshare.ceversion<7.1 then
          while AddressList.Count>0 do
            AddressList[0].destroy()
          end
        end
        
        if not ceshare.decodeFunctionHooked then
          local originalDecode=decodeFunction
          local noToAll=false
          local yesToAll=false
          decodeFunction=function(data)
            local decodeIt=yesToAll

            if (noToAll or yesToAll) == false then 
              local r=messageDialog(translate('The current table is trying to load obfuscated code. This often means mallicious intent as tables are supposed to be public. Do you wish to execute this lua code anyhow?'), mtWarning, mbYes,  mbNo, mbYesToAll, mbNoToAll)
              if r==mrYes then
                decodeIt=true
              elseif r==mrYesToAll then
                decodeIt=true
                yesToAll=true
              elseif r==mrNoToAll then
                decodeIt=false
                noToAll=true
              end              
            end
            if decodeIt==false then 
              return function() return nil end --dummy function
            else 
              return originalDecode(data)
            end            
          end
          ceshare.decodeFunctionHooked=true
        end
        
        loadTable(cheattabless)        
        cheattabless.destroy()
      
        ceshare.CheatBrowserFrm.close()
        ceshare.LoadedTable=ceshare.CurrentQuery[index+1]
      end
    end
    
    ceshare.CheatBrowserFrm.lvCheats.OnDblClick=ceshare.CheatBrowserFrm.btnLoadTable.OnClick
    ceshare.CheatBrowserFrm.miLoad.OnClick=ceshare.CheatBrowserFrm.btnLoadTable.OnClick
    
    --create the 'Rate Table' stars
    ceshare.picFullStar=ceshare.CheatBrowserFrm.imgStarFilled.Picture
    ceshare.picEmptyStar=ceshare.CheatBrowserFrm.imgStarNotFilled.Picture
  
    ceshare.RateStars={}
    
    local currentLeftControl=ceshare.CheatBrowserFrm.lblRate
    local i
    for i=1,5 do
      ceshare.RateStars[i]={}
      ceshare.RateStars[i].state=false
      ceshare.RateStars[i].img=createImage(ceshare.CheatBrowserFrm.pnlControls)
      ceshare.RateStars[i].img.Stretch=true
      ceshare.RateStars[i].img.Picture=ceshare.picEmptyStar
      ceshare.RateStars[i].img.AnchorSideLeft.Control=currentLeftControl
      ceshare.RateStars[i].img.AnchorSideLeft.Side=asrRight
      ceshare.RateStars[i].img.AnchorSideTop.Control=ceshare.CheatBrowserFrm.lblRate
      ceshare.RateStars[i].img.AnchorSideTop.Side=asrCenter
      ceshare.RateStars[i].img.BorderSpacing.Left=4
      
      ceshare.RateStars[i].img.Cursor=-21
      ceshare.RateStars[i].img.Tag=i
      
      ceshare.RateStars[i].img.OnMouseEnter=function(star)
        local i
        local index=star.Tag
        local cindex=ceshare.CheatBrowserFrm.lvCheats.ItemIndex
        for i=1,5 do                  
          if (i<=index) and (cindex~=-1) then
            ceshare.RateStars[i].img.Picture=ceshare.picFullStar            
          else
            ceshare.RateStars[i].img.Picture=ceshare.picEmptyStar
          end
        end
      end
      
      ceshare.RateStars[i].img.OnMouseLeave=function(star)
        local cindex=ceshare.CheatBrowserFrm.lvCheats.ItemIndex
        local yourRating
        if cindex~=-1 then
          yourRating=ceshare.CurrentQuery[cindex+1].YourRating or 0        
        else
          yourRating=0
        end
        
        for i=1,5 do  
          if (yourRating>=i) then
            ceshare.RateStars[i].img.Picture=ceshare.picFullStar           
          else
            ceshare.RateStars[i].img.Picture=ceshare.picEmptyStar           
          end
        end
      end
      
      ceshare.RateStars[i].img.OnClick=function(star)
        local index=ceshare.CheatBrowserFrm.lvCheats.ItemIndex   
        if index~=-1 then
          local cheatid=ceshare.CurrentQuery[index+1].ID  
          local rating=star.tag        
          local parameters='id='..cheatid
          parameters=parameters..'&rating='..rating
          xml=ceshare.QueryXURL('RateTable.php',parameters)
          if xml then
            --just a cache so the database doesn't have to be asked and shows even when  not logged in
            ceshare.settings.Value['voted'..cheatid]=rating
            ceshare.CurrentQuery[index+1].YourRating=rating 
            
            ceshare.RateStars[star.tag].img.OnMouseLeave(ceshare.RateStars[star.tag])
          end
        end        
      end
      
      ceshare.CheatBrowserFrm.lblContact.OnClick=function(l)
        --clicked on the Contact link bitton right
        local index=ceshare.CheatBrowserFrm.lvCheats.ItemIndex  
        if index~=-1 then
          local cheatid=ceshare.CurrentQuery[index+1].ID  
          local parameters='id='..cheatid
          if ceshare.CurrentQuery[index+1].Public==false then
            --ask if the owner of the table or admin should be contacted
            local f=createForm(false)
            f.Caption=translate('Contact')
            local l=createLabel(f)
            l.Caption=translate('Contact who?')
            l.Align=alTop
            l.Alignment='taCenter'

            local buttonPanel=createPanel(f)
            buttonPanel.BevelOuter='bvNone'
            buttonPanel.Align='alClient'
            buttonPanel.ChildSizing.Layout='cclLeftToRightThenTopToBottom'
            buttonPanel.ChildSizing.ControlsPerLine=2
            buttonPanel.ChildSizing.HorizontalSpacing=(getScreenDPI()/96)*3
            buttonPanel.BorderSpacing.Around=(getScreenDPI()/96)*3



            local btnAdmin=createButton(buttonPanel)
            btnAdmin.Caption=translate('Site Admin')
            btnAdmin.ModalResult=1000
            local btnOwner=createButton(buttonPanel)
            btnOwner.Caption=translate('Table Owner')
            btnOwner.ModalResult=1001

            btnAdmin.AutoSize=true
            btnOwner.AutoSize=true

            buttonPanel.AutoSize=true
            f.AutoSize=true

            f.BorderIcons="[biSystemMenu]"
            f.Position="poScreenCenter"

            local r=f.showModal()
            if r>=1000 then
              if r==1000 then --admin
                parameters=parameters..'&admin=1' 
              end
            else
              return
            end

          end 

          xml=ceshare.QueryXURL('Contact.php',parameters)
          if xml then
            --just a cache so the database doesn't have to be asked and shows even when  not logged in
            if xml.Url then
              local url=xml.Url:value()
              --[[
              validate the result.
              allowed: 
                mailto:
                http:
                https:              
              --]]
              
              local valid=false
              start,stop=string.find(url, "https:")
              if start==1 then valid=true end
              
              if not valid then
                start,stop=string.find(url, "http:")
                if start==1 then valid=true end
              end
              
              if not valid then
                start,stop=string.find(url, "mailto:")
                if start==1 then valid=true end
              end
              
              if valid then
                shellExecute(url)
              end
            end
          end
          
          
        end
      end
      
      currentLeftControl=ceshare.RateStars[i].img
    end
    local RateStars={}


    ceshare.CheatBrowserFrm.miViewComments.OnClick=function(s)
      local index=ceshare.CheatBrowserFrm.lvCheats.ItemIndex
      if index~=-1 then
        ceshare.ViewComments(ceshare.CurrentQuery[index+1])
      end
    end
    
    ceshare.CheatBrowserFrm.btnAddViewComments.OnClick=ceshare.CheatBrowserFrm.miViewComments.OnClick

    ceshare.CheatBrowserFrm.miLoginToSeeMoreOptions.OnClick=function(s)
      ceshare.spawnLoginDialog()
    end

    ceshare.CheatBrowserFrm.miUpdateTable.OnClick=function(s)
      local index=ceshare.CheatBrowserFrm.lvCheats.ItemIndex
      if index~=-1 then
        ceshare.PublishCheatClick(nil,ceshare.CurrentQuery[index+1])
      end    
    end
    
    ceshare.CheatBrowserFrm.miDeleteTable.OnClick=function(s)
      local index=ceshare.CheatBrowserFrm.lvCheats.ItemIndex
      if index~=-1 then
        local entry=ceshare.CurrentQuery[index+1]
        ceshare.Delete(entry)
      end    
    end    
    
    ceshare.CheatBrowserFrm.miManageAccessList.OnClick=function(s)
      local index=ceshare.CheatBrowserFrm.lvCheats.ItemIndex
      if index~=-1 then
        local entry=ceshare.CurrentQuery[index+1]
        ceshare.ManageAccessList(entry)
      end
    end
    
    ceshare.CheatBrowserFrm.pmList.OnPopup=function(s)
      if ceshare.LoggedIn==nil then ceshare.LoggedIn=false end
      local index=ceshare.CheatBrowserFrm.lvCheats.ItemIndex
      
      local canUpdate=false
      local canDelete=false
      local canManage=false
      
      if index~=-1 then
        ceshare.CheatBrowserFrm.miLoad.Visible=true
        ceshare.CheatBrowserFrm.miViewComments.Visible=true
        ceshare.CheatBrowserFrm.miViewHistory.Visible=false --to be implemented later
        ceshare.CheatBrowserFrm.sep.Visible=true
        
        local entry=ceshare.CurrentQuery[index+1]
        if ceshare.LoggedIn then
          if (entry.Permissions==nil) or isKeyPressed(VK_CONTROL) then
            ceshare.getPermissions(entry, true) --don't show errors
          end
          
          if entry.Permissions then
            canUpdate=entry.Permissions.canUpdate
            canDelete=entry.Permissions.canDelete
            canManage=entry.Permissions.canManage
          end
        end
      
      else
        ceshare.CheatBrowserFrm.miLoad.Visible=false
        ceshare.CheatBrowserFrm.miViewComments.Visible=false
        ceshare.CheatBrowserFrm.miViewHistory.Visible=false
        ceshare.CheatBrowserFrm.sep.Visible=false      
      end
      
      ceshare.CheatBrowserFrm.miLoginToSeeMoreOptions.Visible=ceshare.LoggedIn==false

      
      ceshare.CheatBrowserFrm.miUpdateTable.Visible=canUpdate
      ceshare.CheatBrowserFrm.miDeleteTable.Visible=canDelete
      ceshare.CheatBrowserFrm.miManageAccessList.Visible=canManage
      
    end
    
    ceshare.linkButton=createPNG()
    ceshare.linkButton.LoadFromFile(ceshare.imagepath..'link.png')
    
    local formdata
    ceshare.CheatBrowserFrm.loadedFormPosition, formdata=ceshare.CheatBrowserFrm.loadFormPosition()
    if ceshare.CheatBrowserFrm.loadedFormPosition then
      if #formdata>=1 then    
        ceshare.CheatBrowserFrm.CEPanel1.Height=formdata[1]   
      end
    else    
      ceshare.CheatBrowserFrm.Position='poScreenCenter'
      ceshare.CheatBrowserFrm.CEPanel1.Height=(getScreenDPI()/96)*100
    end
  
  end

  --get the table list
  ceshare.CheatBrowserFrm.lvCheats.clear()
  
  if overrideProcess==nil then
    ceshare.CurrentQuery=ceshare.QueryCurrentProcess()
  else  
    local headermd5=ceshare.getCurrentProcessHeaderMD5() or '00000000000000000000000000000000'         
    ceshare.CurrentQuery=ceshare.QueryProcessCheats(overrideProcess, headermd5)
  end

  if ceshare.CurrentQuery==nil or #ceshare.CurrentQuery==0 then
    messageDialog(translate('Sorry, but there are currently no tables for this target. Perhaps you can be the first'),mtError,mbOK)
    return
  end

  for i=1,#ceshare.CurrentQuery do
    local versionIndependent=false
  
    local li=ceshare.CheatBrowserFrm.lvCheats.Items.add()
    li.Caption=ceshare.CurrentQuery[i].Title
    local owner=ceshare.CurrentQuery[i].Owner
    local editor=ceshare.CurrentQuery[i].LastEditor
    if editor==owner then 
      li.SubItems.add(owner)
    else
      li.SubItems.add(editor..' (owner='..owner..')')
    end
      
    if ceshare.CurrentQuery[i].Public then
      li.SubItems.add('     yes     ')
    else
      li.SubItems.add('     ')
    end

    --todo 7.1+: imagelist and stars
    if ceshare.CurrentQuery[i].RatingCount==0 then
      li.SubItems.add('Unrated')
    else
      li.SubItems.add(ceshare.CurrentQuery[i].Rating/ceshare.CurrentQuery[i].RatingCount..' out of 5 ('..ceshare.CurrentQuery[i].RatingCount..' votes)')
    end

    if ceshare.CurrentQuery[i].VersionIndependent then
      li.SubItems.add('      yes      ')
    else
      li.SubItems.add('             ')
    end

    if ceshare.CurrentQuery[i].Signed then
      li.SubItems.add('     yes     ')
    else    
      li.SubItems.add('           ')  --signed
    end


    li.SubItems.add('  '..ceshare.CurrentQuery[i].Match..'%  ')
    
    if ceshare.LoadedTable==tonumber(ceshare.CurrentQuery[i].ID) then
      ceshare.CheatBrowserFrm.lvCheats.ItemIndex=i-1
      --select
    end
    
    li.SubItems.add(' ') --url

  end
  

  ceshare.CheatBrowserFrm.btnViewHistory.Visible=false --later

  ceshare.CheatBrowserFrm.show()
  ceshare.CheatBrowserFrm.AutoSize=false
  ceshare.CheatBrowserFrm.lvCheats.Constraints.MinHeight=0
  ceshare.CheatBrowserFrm.pnlDescription.Constraints.MinHeight=0
  
  --adjust starsize
  local dim=ceshare.CheatBrowserFrm.btnAddViewComments.Height
  for i=1,5 do  
    ceshare.RateStars[i].img.Width=dim
    ceshare.RateStars[i].img.Height=dim
  end
  
  if not ceshare.CheatBrowserFrmShownBefore then
    --adjust the size
    local headerwidth=0
    local i
    for i=0,ceshare.CheatBrowserFrm.lvCheats.Columns.Count-1 do
      local w=ceshare.CheatBrowserFrm.lvCheats.Columns[i].Width
      local neededw=ceshare.CheatBrowserFrm.Canvas.getTextWidth(' '..ceshare.CheatBrowserFrm.lvCheats.Columns[i].Caption..' ')
      
      if w<neededw then
        ceshare.CheatBrowserFrm.lvCheats.Columns[i].Autosize=false
        ceshare.CheatBrowserFrm.lvCheats.Columns[i].Width=neededw        
        w=neededw
      end
        
      headerwidth=headerwidth+w    
    end
    
    
    
    if not ceshare.CheatBrowserFrm.loadedFormPosition then
      ceshare.CheatBrowserFrm.ClientWidth=headerwidth+10      
    end
    
    
    ceshare.CheatBrowserFrmShownBefore=true
  end


end

