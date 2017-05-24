if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'VersionCheck.po')
end

local vsettings=getSettings("VersionCheck")

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
            queue(function()
              print(translate('Unable to check version (Invalid content, not enough lines)'))
              versionCheckThread=nil
            end)
            sl.destroy()            
            return
          end
          
          latestVersionCompleteBuildNumber=tonumber(sl[0])
          latestVersionNumber=tonumber(sl[1])
          latestVersionString=sl[2]
          sl.destroy()
          
          if (latestVersionCompleteBuildNumber==nil) or (latestVersionNumber==nil) then
            queue(function()
              print(translate('Unable to check version (Invalid content)'))
              versionCheckThread=nil
            end) 
            return
          end             
          
          local fv=getCheatEngineFileVersion()
          
          if fv then          
            if latestVersionCompleteBuildNumber>fv then
              print('bigger')
              newerVersion=true
            else
              print('smaller or equal')
            end
          else
            --failed getting the file version (filesystem issues...)
            if latestVersionNumber>getCEVersion() then
              newerVersion=true
            end              
          end         
          
          queue(function()
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
          queue(function()
            print(translate("Unable to check version (Can't connect)"))
            versionCheckThread=nil
          end)
        end
    end)
  end
  
  
end


if vsettings then
  if vsettings['CheckOnLaunch']=='1' then
    CheckVersion(true)
  end
end


local mi=createMenuItem(MainForm.MainMenu)
mi.Caption=translate('Check for new version')
-- also works: mi.Caption=translateID('VC-CFNV')
mi.OnClick=function(mi) CheckVersion(false) end

MainForm.miHelp.insert(MainForm.miAbout.MenuIndex,mi)


