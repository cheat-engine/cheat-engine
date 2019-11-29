function ceshare.enumModules2()
  local m=enumModules()
  local r={}
  
  for i=1,#m do
    r[m[i].Name:lower()]=m[i]
  end
  
  return r
end

function ceshare.QueryProcessCheats(processname, headermd5)
  local modulelist=ceshare.enumModules2()
  local result=nil
  --local url=ceshare.base..'QueryProcessCheats.php'
  local parameters='processname='..ceshare.url_encode(processname)
  --print(url..'?'..parameters)
  
  if isKeyPressed(VK_CONTROL)==false then  --control lets you get a new script if needed
    local secondaryIdentifierCode=ceshare.secondaryIdentifierCode.Value[processname:lower()]
    if secondaryIdentifierCode and secondaryIdentifierCode~='' then
      local value,param=loadstring(secondaryIdentifierCode)()
      if value and param then
        parameters=parameters..'&secondaryidentifier='..ceshare.url_encode(param)
      end
    end
  end

  
  
  
  --local r=ceshare.getInternet().postURL(url,parameters)
    --local s=xmlParser:ParseXmlText(r)
    s=ceshare.QueryXURL('QueryProcessTables.php', parameters)
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
              entry.VersionIndependent=CheatEntry["@versionIndependent"]=='1'
              entry.Public=CheatEntry["@public"]=='1'
              entry.Rating=tonumber(CheatEntry["@ratingtotal"]) or 0
              entry.RatingCount=tonumber(CheatEntry["@ratingcount"]) or 0
              entry.LastEditTime=CheatEntry["@lastEditTime"]
              entry.FullFileHash=CheatEntry["@fullfilehashmd5"]
              entry.SecondaryModuleName=CheatEntry["@secondarymodulename"]
              entry.SecondaryFullFileHash=CheatEntry["@secondaryfullfilehashmd5"]
              entry.CheckCode=CheatEntry["@luaScriptToCheckForMatch"]
              entry.Description=CheatEntry["@description"]
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
                  local me=modulelist[SecondaryModuleName:lower()]
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

function ceshare.QueryCurrentProcess()
  local processnamemd5
  local headermd5
  headermd5=ceshare.getCurrentProcessHeaderMD5()

  if headermd5 then
    return ceshare.QueryProcessCheats(process, headermd5)
  end
end

function ceshare.CheckForCheatsClick(s)
  --spawn the cheatbrowser
  
  if ceshare.CheatBrowserFrm==nil then
    local f=createFormFromFile(ceshare.formpath..'BrowseCheats.FRM')
    f.lblProcessName.Caption=process

    ceshare.CheatBrowserFrm=f

    local h=f.lvCheats.Canvas.getTextHeight('XXX')*10
    f.lvCheats.Constraints.MinHeight=h
    f.pnlDescription.Constraints.MinHeight=h
    
    --configure base state and add events

    ceshare.CheatBrowserFrm.lvCheats.OnSelectItem=function(sender, listitem, selected)
      if selected and listitem.index then
        local desc=ceshare.CurrentQuery[listitem.index+1].Description
        
        ceshare.CheatBrowserFrm.imgDescription.Width=ceshare.CheatBrowserFrm.ScrollBox1.ClientWidth
        ceshare.CheatBrowserFrm.imgDescription.Height=ceshare.CheatBrowserFrm.ScrollBox1.ClientHeight
        
        ceshare.CheatBrowserFrm.imgDescription.Picture.Bitmap.Canvas.Brush.Color=0xffffff
        ceshare.CheatBrowserFrm.imgDescription.Picture.Bitmap.Width=ceshare.CheatBrowserFrm.ScrollBox1.Width
        ceshare.CheatBrowserFrm.imgDescription.Picture.Bitmap.Height=ceshare.CheatBrowserFrm.ScrollBox1.Height

        ceshare.CheatBrowserFrm.imgDescription.Picture.Bitmap.Canvas.fillRect(0,0,ceshare.CheatBrowserFrm.ScrollBox1.Width,ceshare.CheatBrowserFrm.ScrollBox1.Height)

        local imgrect={Left=0,Top=0,Right=ceshare.CheatBrowserFrm.imgDescription.Width, Bottom=ceshare.CheatBrowserFrm.imgDescription.Height}

        

        local r=ceshare.CheatBrowserFrm.imgDescription.Picture.Bitmap.Canvas.textRect(imgrect,0,0,desc);
        if r then --ce 7.1+ gets the actually needed rect

        else

        end
        
        --[[local rating=ceshare.CurrentQuery[listitem.index+1].YourRating    
        if rating then
          --trigger a refresh
          ceshare.RateStars[rating].img.OnMouseLeave(ceshare.RateStars[rating]) 
        end--]]
      end
      
      ceshare.RateStars[1].img.OnMouseLeave(ceshare.RateStars[1]) 
  

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
        
        loadTable(cheattabless)        
        cheattabless.destroy()
      
        ceshare.CheatBrowserFrm.close()
        ceshare.LoadedTable=ceshare.CurrentQuery[index+1]
        
        MainForm.miCEShareUpdatePublicSeperator.visible=true
        MainForm.miUpdateCheat.Visible=true        
      end
    end
    
    ceshare.CheatBrowserFrm.lvCheats.OnDblClick=ceshare.CheatBrowserFrm.btnLoadTable.OnClick
    ceshare.CheatBrowserFrm.miLoad.OnClick=ceshare.CheatBrowserFrm.btnLoadTable.OnClick
    
    --create the 'Rate Table' stars
    ceshare.picFullStar=ceshare.CheatBrowserFrm.imgStarFilled.Picture
    ceshare.picEmptyStar=ceshare.CheatBrowserFrm.imgStarNotFilled.Picture
  
    ceshare.RateStars={}
    local dim=ceshare.CheatBrowserFrm.btnAddViewComments.Height+2
    local currentLeftControl=ceshare.CheatBrowserFrm.lblRate
    local i
    for i=1,5 do
      ceshare.RateStars[i]={}
      ceshare.RateStars[i].state=false
      ceshare.RateStars[i].img=createImage(ceshare.CheatBrowserFrm.pnlControls)
      ceshare.RateStars[i].img.Width=dim
      ceshare.RateStars[i].img.Height=dim      
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
        ceshare.DeleteTable(entry)
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
        ceshare.CheatBrowserFrm.miViewHistory.Visible=false --to be implemented later true
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
    
  end

  ceshare.CheatBrowserFrm.lvCheats.clear()
  ceshare.CurrentQuery=ceshare.QueryCurrentProcess()

  if ceshare.CurrentQuery==nil then
    messageDialog('Sorry, but there are currently no tables for this process. Perhaps you can be the first',mtError,mbOK)
    return
  end

  for i=1,#ceshare.CurrentQuery do
    local versionIndependent=false
  
    local li=ceshare.CheatBrowserFrm.lvCheats.Items.add()
    li.Caption=ceshare.CurrentQuery[i].Title
    li.SubItems.add(ceshare.CurrentQuery[i].Owner);
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

  end
  

  ceshare.CheatBrowserFrm.btnViewHistory.Visible=false --later

  ceshare.CheatBrowserFrm.show()
  ceshare.CheatBrowserFrm.AutoSize=false
  ceshare.CheatBrowserFrm.lvCheats.Constraints.MinHeight=0
  ceshare.CheatBrowserFrm.pnlDescription.Constraints.MinHeight=0
  
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
    
    ceshare.CheatBrowserFrm.ClientWidth=headerwidth+10
    ceshare.CheatBrowserFrmShownBefore=true
  end

  ceshare.CheatBrowserFrm.Position='poScreenCenter'
end

