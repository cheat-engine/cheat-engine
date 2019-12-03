if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'CeShare.po')
end

ceshare={}

function ceshare.getInternet()
  if ceshare.internet==nil then
    ceshare.internet=getInternet('ceshare')
  end
  return ceshare.internet
end

ceshare.version=-1
ceshare.path=getCheatEngineDir()..[[autorun\ceshare\]]
ceshare.formpath=ceshare.path..[[\forms\]]

if package.loaded.xmlSimple==nil then
  package.path=package.path..';'..getCheatEngineDir()..[[autorun\xml\?.lua]]
else
  package.loaded.xmlSimple=nil
end
xmlParser = require("xmlSimple").newParser()


package.path=package.path..';'..ceshare.path..[[?.lua]]

function loadCEShare()
  ceshare.settings=getSettings('ceshare')
  ceshare.secondaryIdentifierCode=getSettings('ceshare\\secondaryIdenfierCode')

  require("ceshare_account")
  require("ceshare_publish")
  require("ceshare_querycheats")
  require("ceshare_processlistextention")
  require("ceshare_permissions")
  require("ceshare_comments")
  require("ceshare_requests")
  
  --add "CE Share menu items"
  local miCESHARETopMenuItem=createMenuItem(MainForm)
  miCESHARETopMenuItem.Caption='CE Share'
  MainForm.Menu.Items.insert(2,miCESHARETopMenuItem) --in front of table

  local miCheckForCheats=createMenuItem(MainForm)
  miCheckForCheats.Caption='Check for mods/cheats for current process'
  miCheckForCheats.OnClick=ceshare.CheckForCheatsClick
  miCheckForCheats.Default=true
  miCheckForCheats.Name='miCheckForCheats'
  miCESHARETopMenuItem.add(miCheckForCheats)
  
  local miRequestCheats=createMenuItem(MainForm)
  miRequestCheats.Caption='Request and check other requests for mods/cheats for current process'
  miRequestCheats.OnClick=ceshare.RequestForCheatsClick
  miRequestCheats.Name='miRequestCheats'
  miCESHARETopMenuItem.add(miRequestCheats)  

  local miLoginToSeeMoreOptions=createMenuItem(MainForm)
  miLoginToSeeMoreOptions.Caption='Login to see more options (Like publish Table)'
  miLoginToSeeMoreOptions.OnClick=ceshare.spawnLoginDialog
  miLoginToSeeMoreOptions.Name='miLoginToSeeMoreOptions';
  miLoginToSeeMoreOptions.Visible=true
  miCESHARETopMenuItem.add(miLoginToSeeMoreOptions)

  local miPublishCheat=createMenuItem(MainForm)
  miPublishCheat.Caption='Publish table'
  miPublishCheat.OnClick=ceshare.PublishCheatClick
  miPublishCheat.Name='miPublishCheat';
  miPublishCheat.Visible=false
  miCESHARETopMenuItem.add(miPublishCheat)
  
  local miSeperator=createMenuItem(MainForm)
  miSeperator.Caption='-'
  miSeperator.Name='miCEShareUpdatePublicSeperator'
  miSeperator.Visible=false
  miCESHARETopMenuItem.add(miSeperator)
  
  local miUpdateCheat=createMenuItem(MainForm)
  miUpdateCheat.Caption='Update table'
  miUpdateCheat.OnClick=ceshare.UpdateCheatClick
  miUpdateCheat.Name='miUpdateCheat'
  miCESHARETopMenuItem.add(miUpdateCheat)
  miUpdateCheat.Visible=false

  --check requestsc
  
  miCESHARETopMenuItem.OnClick=function(s)      
    loggedin=ceshare.LoggedIn or false
    miLoginToSeeMoreOptions.Visible=not loggedin
    miPublishCheat.Visible=loggedin
    
    local canUpdate=false    
    if loggedin and ceshare.LoadedTable then
      
      if (ceshare.LoadedTable.Permissions==nil) or isKeyPressed(VK_CONTROL) then
        ceshare.getPermissions(ceshare.LoadedTable, true) --don't show errors
      end
      
      if ceshare.LoadedTable.Permissions then     
        canUpdate=ceshare.LoadedTable.Permissions.canUpdate
      end
    end
    miSeperator.Visible=canUpdate
    miUpdateCheat.Visible=canUpdate
  end

  local originalLoadTable=loadTable
  loadTable=function(streamorfilename,merge,ignoreluascriptdialog)
    ceshare.ClearCredentials() --stupid if a user loads an unknown table with credentials stored but whatever
    return originalLoadTable(streamorfilename,merge,ignoreluascriptdialog) 
  end
  
  
  
end

function loadCEShareServerListInCombobox(cb)
--[[
fills in ceshare.ceshareserverlist if needed and puts the list into the combobox
--]]
  
  if ceshare.ceshareserverlist==nil then
    local list=ceshare.getInternet().getURL('https://cheatengine.org/cesharelist.txt') --AKA: Eggbaskets
    local sl=createStringList()
    sl.Text=list
    
    
    --parse the list and build a table
    ceshare.ceshareserverlist={}
    
    local i
    for i=0,sl.Count-1 do
      if sl[i]:sub(1,1)~='#' then
        local server=sl[i]
        local sep,sep2=server:find(' #%-# ')
        if sep then
          local e={}
          e.full=server
          e.base=server:sub(1,sep-1)
          e.description=server:sub(sep2+1)        
          table.insert(ceshare.ceshareserverlist,e)
        end
      end
    end    
    sl.destroy()
  end
  
  cb.Items.clear()
  for i=1,#ceshare.ceshareserverlist do
    cb.Items.add(ceshare.ceshareserverlist[i].description)
  end  
end

local f=io.open(ceshare.path..[[server.txt]],'rb')
if f then
  ceshare.base=f:read("*all")
  f:close()
else
  --first time setup, ask the user for a ceshare base url
  ceshare.initialSetup=createFormFromFile(ceshare.formpath..'InitialSetup.FRM')
  loadCEShareServerListInCombobox(ceshare.initialSetup.cbCEShareURL)
 
  
  ceshare.initialSetup.Position='poScreenCenter'
  ceshare.initialSetup.AutoSize=true
  if ceshare.initialSetup.showModal()==mrOK then
    if ceshare.initialSetup.cbCEShareURL.ItemIndex==-1 then
      ceshare.base=ceshare.initialSetup.cbCEShareURL.Text
    else
      ceshare.base=ceshare.ceshareserverlist[ceshare.initialSetup.cbCEShareURL.ItemIndex+1].base
    end   
  else
    ceshare.base=''  
  end
  
  f=io.open(ceshare.path..[[server.txt]],'wb')
  f:write(ceshare.base)
  f:close()  
end

ceshare.ceversion=getCEVersion()

--add a ceshare config option to settings

local sf=getSettingsForm()
ceshare.settingsTab=sf.SettingsPageControl.addTab()


ceshare.settingsCBBase=createComboBox(ceshare.settingsTab)
ceshare.settingsCBBase.Text=ceshare.base
ceshare.settingsCBBase.Align='alTop'

local lblCEShareLabel=createLabel(ceshare.settingsTab)
lblCEShareLabel.Caption='CEShare community URL'
lblCEShareLabel.Align='alTop'



ceshare.settingsTab.OnShow=function()
  --fill the list
  if ceshare.settingsCBBase.Items.Count==0 then
    loadCEShareServerListInCombobox(ceshare.settingsCBBase)
  end  
end

local insertNode=sf.SettingsTreeView.Items[3]  --insert it near the unrandomizer since it'd be used as often as that setting
local node=sf.SettingsTreeView.Items.insert(insertNode, "CEShare")
node.data=userDataToInteger(ceshare.settingsTab)

local originalSettingsCloseEvent=sf.OnClose
sf.OnClose=function(s)
  local r=caHide
  if originalSettingsCloseEvent then
    r=originalSettingsCloseEvent(s)
  end
  
  if s.ModalResult==mrOK then
    --apply change
    local newbase=''
    if ceshare.settingsCBBase.ItemIndex==-1 then
      newbase=ceshare.settingsCBBase.Text
    else
      newbase=ceshare.ceshareserverlist[ceshare.settingsCBBase.ItemIndex+1].base
    end  
    
    if newbase~=ceshare.base then    
      f=io.open(ceshare.path..[[server.txt]],'wb')
      f:write(newbase)
      f:close()   
      
      local needsload=(ceshare.base==nil) or (ceshare.base=='')
      ceshare.base=newbase  

      if needsload then   
        loadCEShare()
      end
    end
  end
  
  
  return r  
end






function ceshare.showError(msg)
  messageDialog(msg,mtError, mbOK)  
end

function ceshare.url_encode(str)
  if type(str)=='boolean' then
    if str then 
      return '1'
    else
      return '0'
    end
  end
 
  
  if (str) then      
    --str = string.gsub (str, "\n", "\r\n")
    str = string.gsub (str, "([^%w %-%_%.%~])",
        function (c) return string.format ("%%%02X", string.byte(c)) end)
    str = string.gsub (str, " ", "+")
  end
  return str
end

function ceshare.parseResult(r, skipErrorDialog)
--parses the xml string and returns the xml object, or nil with a true/false.
--true means, try again, false means give up
  local xml
  
  
  if r then
    if (r:sub(1,2)=='<?') then
      xml=xmlParser:ParseXmlText(r)
      if xml then
        if xml.invalidsession then --This requires a valid session. Spawn a login screen
          if ceshare.spawnLoginDialog() then --try again after logging in
            return nil,true
          else 
            return nil,false
          end  
        end
        
        if xml.extraparamneeded then
          local valid, functionresult
          
          local f=loadstring(xml.extraparamneeded:value())
          
          if f then
            --check registry if it's ok to run this script, or ask if it should be loaded
            local d=createForm(false)
            d.Caption='Secondary identifier code'
            d.BorderStyle='bsSizeable'
            
            local pnlBtns=createPanel(d)
            btnAllow=createButton(pnlBtns)
            btnCancel=createButton(pnlBtns)
            
            btnAllow.AutoSize=true
            btnCancel.AutoSide=true
            
            btnAllow.Caption='Allow'
            btnAllow.Default=true
            btnAllow.ModalResult=mrOK
            
            btnCancel.Caption='Cancel'
            btnCancel.Cancel=true
            btnCancel.ModalResult=mrCancel
            
            pnlBtns.ChildSizing.ControlsPerLine=2
            pnlBtns.ChildSizing.HorizontalSpacing=4
            pnlBtns.ChildSizing.EnlargeHorizontal='crsHomogenousChildResize'
            pnlBtns.ChildSizing.Layout='cclLeftToRightThenTopToBottom'
            pnlBtns.AutoSize=true                        
            pnlBtns.Align='alBottom'
            
            pnlHeaderAndEditor=createPanel(d)
            pnlHeaderAndEditor.align='alClient'
            
            
            local header=createLabel(pnlHeaderAndEditor)
            header.caption='This process needs a special identifier to set it apart from others with the same name. To generate this identifier the below code is needed. Do you agree with the execution of this code to generate the identifier?'
            header.align='alTop'
            header.WordWrap=true
            
            local scriptviewer
            if createSynEdit then
              scriptviewer=createSynEdit(pnlHeaderAndEditor,0) --7.1+  ,0 means lua syntax
              scriptviewer.Gutter.Parts.SynGutterMarks1.Visible=false
              scriptviewer.Gutter.Parts.SynGutterChanges1.Visible=false
              scriptviewer.Gutter.Parts.SynGutterSeparator1.Visible=true
              scriptviewer.Gutter.Parts.SynGutterCodeFolding1.Visible=false
              scriptviewer.Font.Size=10              
            else
              scriptviewer=createMemo(pnlHeaderAndEditor)              
              scriptviewer.ScrollBars='ssAutoBoth'
            end
            scriptviewer.ReadOnly=true
            scriptviewer.Lines.Text=xml.extraparamneeded:value()
            scriptviewer.align='alClient'
            
            d.Width=d.Canvas.getTextWidth(header.Caption)/3
            d.Height=d.Canvas.getTextHeight('Qwertyuiopjkl')*18            
            d.position='poScreenCenter'
            
            if d.showModal()~=mrOK then return nil,false end
          end
          
          --still here so ok
          valid, functionresult=f()
          
          if valid then
            ceshare.secondaryIdentifierCode.Value[process:lower()]=xml.extraparamneeded:value()
          end
          
          return nil, valid, functionresult --try again with this extra parameter
        
        end
        --[[ perhaps someday if there is a really trusted server
        if xml.executeLua then
          loadstring(xml.executeLua:value())()
          return xml
        end
        --]]      
        
        
        if xml.error then
          if (skipErrorDialog==nil) or (skipErrorDialog==false) then
            ceshare.showError(xml.error:value())        
          end
          
          return nil,false
        else
          return xml
        end

      else
        if (skipErrorDialog==nil) or (skipErrorDialog==false) then
          ceshare.showError('Error:'..r)  
        end
        return nil,false     
      end 
    else
      if (skipErrorDialog==nil) or (skipErrorDialog==false) then
        ceshare.showError("Invalid reply from server:"..r)  
      end
      return nil,false
    end
  else
    if (skipErrorDialog==nil) or (skipErrorDialog==false) then
      ceshare.showError("Server did not respond") --..ceshare.debug)
    end
    return nil,false
  end
end

function ceshare.QueryXURL(filename,parameters, skipErrorDialog)  
  local tryagain=true
  local xml
  local extraparam
  while tryagain==true do
    local rawdata=ceshare.getInternet().postURL(ceshare.base..filename,parameters)
    xml,tryagain,extraparam=ceshare.parseResult(rawdata, skipErrorDialog)
    if xml then
      return xml
    end  

    if tryagain and extraparam then   
      local newparam='secondaryidentifier='..ceshare.url_encode(extraparam)
      if parameters and parameters~='' then
        parameters=parameters..'&'..newparam
      else
        parameters=extraparam
      end
    end
  end
  
  return nil
end

if (ceshare.base) and (ceshare.base~='') then
  loadCEShare() 
end 






