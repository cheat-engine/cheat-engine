--version check update script for cheat engine
--Don't like it? Just delete this file. Easy as that

--For the translators:
if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'VersionCheck.po')
end

vsettings=getSettings("VersionCheck")

local VersionCheckThread

function CheckVersion(automatic)
  --create a thread that will get the latest version and buildnumber
  if versionCheckThread==nil then
    versionCheckThread=createThread(function(t)
        local i=getInternet('CEVersionCheck')
        local r=i.getURL('http://cheatengine.org/latestversion.txt')
        
        if r then        
          local sl=createStringlist()
          local newerVersion=false
          local latestVersionCompleteBuildNumber
          local latestVersionNumber
          local latestVersionString --seperate for crap like 6.5.1 (can't show 6.51 to the user)
          sl.Text=r          
          
          if sl.Count<3 then
            t.synchronize(function()
              if automatic then
                messageDialog(translate('Unable to check version (Invalid content, not enough lines)'), mtError,mbOK)
              end
              versionCheckThread=nil
            end)
            sl.destroy()  
            i.destroy()            
            return
          end
          
          latestVersionCompleteBuildNumber=tonumber(sl[0])
          latestVersionNumber=tonumber(sl[1])
          latestVersionString=sl[2]
          sl.destroy()
          
          if (latestVersionCompleteBuildNumber==nil) or (latestVersionNumber==nil) then
            t.synchronize(function()
              if automatic then
                messageDialog(translate('Unable to check version (Invalid content)'), mtError, mbOK)
              end
              versionCheckThread=nil
            end) 
            i.destroy()
            return
          end             
          
          local fv=getCheatEngineFileVersion()
          
          if fv then          
            if latestVersionCompleteBuildNumber>fv then
              newerVersion=true
            end
          else
            --failed getting the file version (filesystem issues...)
            if latestVersionNumber>getCEVersion() then
              newerVersion=true
            end              
          end   
        
          
          t.synchronize(function()
            if newerVersion then
              if messageDialog(string.format(translate('Cheat Engine %s is available at www.cheatengine.org. Go there now?'),latestVersionString), mtConfirmation, mbYes, mbNo)==mrYes then
                shellExecute('http://cheatengine.org/')
              end
            else
              if not automatic then
                showMessage(string.format(translate('You are up to date. The latest version is %s'),latestVersionString))
              end
            end
            
            versionCheckThread=nil
          end)
        else
          t.synchronize(function()
            if not automatic then
              messageDialog(translate("Unable to check version (Can't connect)"), mtError, mbOK)
            end
            versionCheckThread=nil
          end)
        end
        
        i.destroy()
    end)
  end
  
  
end


if vsettings then
  if vsettings.Value['CheckOnLaunch']=='1' then
    CheckVersion(true)
  end
end


local mi=createMenuItem(MainForm.MainMenu)
mi.Caption=translate('Check for new version')
-- also works: mi.Caption=translateID('VC-CFNV')
mi.OnClick=function(mi) CheckVersion(false) end

MainForm.miHelp.insert(MainForm.miAbout.MenuIndex,mi)

--add setting to the main ce config screen
local sf=getSettingsForm()
--I want it in front of the show undo button
--that means, take on the top and left anchor of the undo button, and change the anchor of the undo button to my item

local cbCheckForUpdatesOnLaunch=createCheckBox(sf)
cbCheckForUpdatesOnLaunch.checked=vsettings.Value['CheckOnLaunch']=='1'
cbCheckForUpdatesOnLaunch.Caption=translate('Check for updates when Cheat Engine starts')

cbCheckForUpdatesOnLaunch.Parent=sf.cbShowUndo.Parent --put it inside the same control as the undo button (the scrollbox)

cbCheckForUpdatesOnLaunch.AnchorSideTop=sf.cbShowUndo.AnchorSideTop
cbCheckForUpdatesOnLaunch.AnchorSideLeft=sf.cbShowUndo.AnchorSideLeft

sf.cbShowUndo.AnchorSideTop.Control=cbCheckForUpdatesOnLaunch
sf.cbShowUndo.AnchorSideTop.Side=asrBottom --put the top of the undo button to the bottom of the new checkbox (so below it)

sf.cbShowUndo.AnchorSideLeft.Control=cbCheckForUpdatesOnLaunch
sf.cbShowUndo.AnchorSideLeft.Side=asrLeft --put the left of the undo button to the left side of the new checkbox (so same start)




--now capture when the action is applied. I could hijack the button, but exceptions can sometimes cause issues. (though this button should not give exceptions in 6.7+ anymore)
local oldSettingsFormClose=sf.OnClose

sf.OnClose=function(f)
  if sf.ModalResult==mrOK then --the user clicked OK and all checks passed
    vsettings.Value['CheckOnLaunch']=cbCheckForUpdatesOnLaunch.Checked
  end
  
  --call the original OnClose of the settings form
  if oldSettingsFormClose then
    return oldSettingsFormClose(f);
  end
end

