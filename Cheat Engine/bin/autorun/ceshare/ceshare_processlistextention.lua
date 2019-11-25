function ceshare.GetCurrentProcessList()
  ceshare.currentprocesslist={}
  
  for pid,name in pairs(getProcessList()) do
    local md5name=stringToMD5String(string.lower(name))
    --search processlist for this
    if ceshare.processlist[md5name] then
      local e={}
      e.pid=pid
      e.name=name
      e.md5=md5name
      table.insert(ceshare.currentprocesslist,e)
    end
  end
  
  return ceshare.currentprocesslist
end

function ceshare.DownloadProcessList()
  --Downloads the processlist 
  local i=ceshare.getInternet()
  local processlist=i.getURL(ceshare.base..'processlist.txt')
  
  if processlist==nil then
    return
  end
  
  if processlist:sub(1,1)=='<' then
    return --it returned html code instead of a md5 list
  end
  
  local f=io.open(ceshare.path..[[processlist.txt]],'wb')
  if f then
    f:write(processlist)
    f:close()

    ceshare.settings.Value.LastProcessListDownload=os.time()
  end
end

function ceshare.LoadProcessList()
  --checks if processlist.txt exists, and if not, call DownloadProcessList
  --returns true on load
  local lastdownload=ceshare.settings.Value.LastProcessListDownload  
  
  if (lastdownload==nil) or (lastdownload=='') or (os.time()>(tonumber(lastdownload)+3600))  then   
    ceshare.DownloadProcessList()
  end  
  
  local processlist
  local f=io.open(ceshare.path..[[processlist.txt]],'rb')
  if not f then
    ceshare.DownloadProcessList()
    f=io.open(ceshare.path..[[processlist.txt]],'rb') --try again
  end
  
  if f then
    processlist=f:read("*all")
    f:close()
  else 
    return false
  end
  
  --still here so processlist is valid
  
  local sl=createStringList()
  sl.Text=processlist  
  ceshare.processlist={}
  for i=0,sl.Count-1 do 
    local s=sl[i]
    if #s==32 then    
      ceshare.processlist[sl[i]]=true
    end
  end
  
  sl.destroy()
end




z=registerFormAddNotification(function(s)
  --watches for the ProcessWindow form
  if s.ClassName=='TProcessWindow' then
  --[[ --on hold while waiting for tabs to support images or ownerdraw, which neither is the case
    s.registerCreateCallback(function(s2)    
      if ceshare.ceversion>=7.1 then --can show icons in the tab
        if s2.TabHeader.Images==nil then
          --this ce version does not have images yet
          s2.TabHeader.Images=MainForm.mfImageList --use the mainform imagelist.  ImageIndex11 is useful
          
          
          local OriginalOnShow=s2.OnShow
          s2.OnShow=function(s)
            OriginalOnShow()
            ceshare.GetCurrentProcessList()                    
          end
          
          s2.TabHeader.OnGetImageIndex=function(sender, tabindex)
            print("fart")
            return 11;
          end
          
        end
      end    
    end)
    
    --]]
    
    s.registerFirstShowCallback(function(s2)
      local ci
      local OriginalProcessListDrawItem=s2.ProcessList.OnDrawItem
      
      ceshare.GetCurrentProcessList()  
      
      ceshare.ProcessListWindow=s2
      
      s2.TabHeader.Tabs.add('CEShare')
      ci=s2.TabHeader.Tabs.Count-1
      

      
      ceshare.ProcessWindowCEShareTabIndex=ci
      
      local OriginalOnDestroy=s2.OnDestroy
      s2.OnDestroy=function(sender)
        if OriginalOnDestroy then
          ceshare.settings.Value.ProcessListLastTab=s2.TabHeader.TabIndex
          OriginalOnDestroy()
        end
      end
      
      local OriginalProcessListOnDblClick=s2.ProcessList.OnDblClick
      

      s2.ProcessList.OnDrawItem=function(sender, index, rect, state)      
        if ceshare.ceversion<7.1 then                  
          --a bug in 7.0 and earlier makes state the wrong type. so first convert it to the proper names, or just empty it as CE doesn't make use of it
          state=''
        end
        
        local r=OriginalProcessListDrawItem(sender, index, rect, state)      
        if s2.TabHeader.TabIndex==ci then
          --draw the icon for this process
          
          if ceshare.processiconcache then
            local iconhandle=ceshare.processiconcache[ceshare.currentprocesslist[index+1].pid]            
            if iconhandle then                      
             
              local senderdc=sender.Canvas.Handle
              if senderdc==nil then --7.1 doesn't have Canvas.Handle
                senderdc=readPointerLocal(userDataToInteger(sender.Canvas)+0xc8)  --fHandle offset in the Canvas object  
              end
            
              local ih=sender.ItemHeight                    
            
              executeCodeLocalEx('DrawIconEx',senderdc,0,rect.Top,iconhandle,ih,ih,0,0,3)  
              
            end
          end
          
        end  
        
        return r
      end

      
    
      
      local oldTabChange=s2.TabHeader.OnChange

      s2.TabHeader.OnChange=function(th)
        if (s2.TabHeader.TabIndex==ci)  then
          s2.ProcessList.Items.clear()
          --fill the list with known processes
            
          if ceshare.processlist==nil then return end
          
          ceshare.currentprocesslist={}
          
          
          for pid,name in pairs(getProcessList()) do
            local md5name=stringToMD5String(string.lower(name))
            --search processlist for this
            if ceshare.processlist[md5name] then
              local e={}
              e.pid=pid
              e.name=name
              e.md5=md5name
              table.insert(ceshare.currentprocesslist,e)
            
              s2.ProcessList.Items.add(string.format('%.8x-%s',pid,name))
            
              if ceshare.processiconcache==nil then
                ceshare.processiconcache={}
              end
            
              if ceshare.processiconcache[pid]==nil then
                local mi=enumModules(pid)
                if mi then
                  local mainmodule=mi[1]
                  if mainmodule then
                    local path=mainmodule.PathToFile
                    if path then
                      ceshare.processiconcache[pid]=executeCodeLocalEx('ExtractIconA',0x00400000,path,0)  --hinstance is 00400000 even for the 64-bit version                  
                    end                    
                  end
                end                
              end 
            end
          end
        else
          oldTabChange(th)         
        end          
      end
      
      --load the new tab (CE doesn't know about this tab so doesn't save it)      
      local ProcessListLastTab=tonumber(ceshare.settings.Value.ProcessListLastTab)
      if ProcessListLastTab==nil then --if never picked a tab, go to ceshare first to show it
        ProcessListLastTab=ci
      end

      s2.TabHeader.TabIndex=ProcessListLastTab
      s2.TabHeader.OnChange(s2.TabHeader)           
    end) --s.registerFirstShowCallback(function(s2)
  end --if s.ClassName=='TProcessWindow' then
end) --registerFormAddNotification(function(s)


ceshare.originalOnProcessOpened=MainForm.OnProcessOpened
MainForm.OnProcessOpened=function(processid, processhandle, caption)
  if ceshare.originalOnProcessOpened then --call the original hook if there is one
    ceshare.originalOnProcessOpened(processid, processhandle, caption)
  end

  if ceshare.ProcessListWindow and (ceshare.ProcessListWindow.TabHeader.TabIndex==ceshare.ProcessWindowCEShareTabIndex) then
    --Opened by the ceshare tab. Check that the user didn't load their own table  (OnProcessOpened comes after being asked to load a local table)   
    local app=getApplication()
    if (AddressList.Count==0) and 
       (app.AdvancedOptions.CodeList2.Items.Count==0) and 
       (app.Comments.Memo1.Lines.Count==0) then
         MainForm.miCheckForCheats.DoClick()       
    end
  end
end


ceshare.LoadProcessList()
