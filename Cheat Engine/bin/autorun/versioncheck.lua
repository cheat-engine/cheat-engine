--version check update script for cheat engine
--Don't like it? Just delete this file. Easy as that

--For the translators:
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
        local r=i.getURL('https://cheatengine.org/latestversion.txt')

        if r then
          local sl=createStringlist()
          local newerVersion=false
          local latestVersionCompleteBuildNumber
          local latestVersionNumber
          local latestVersionString --separate for crap like 6.5.1 (can't show 6.51 to the user)
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
          sl=nil

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
                shellExecute('https://cheatengine.org/?versioncheck=1')
              else
                if automatic then --the user clicked away, so probably not interested
                  local NewInterval=(tonumber(vsettings.Value['CheckInterval']) or 1)*2 --just show a default of two times the current skip time
                  local r=inputQuery(string.format(translate('Update to %s'),latestVersionString),translate('In how many days should I notify you again?'), NewInterval)
                  NewInterval=tonumber(r)
                  if NewInterval then
                    vsettings.Value['CheckInterval']=NewInterval
                  else
                    --the user clicked cancel, so seems to be in a hurry. Let's ask in 2*days again
                    vsettings.Value['CheckInterval']=(tonumber(vsettings.Value['CheckInterval']) or 1)*2
                  end
                end
              end
            else
              if not automatic then
                showMessage(string.format(translate('You are up to date. The latest version is %s'),latestVersionString))
              end
            end

            versionCheckThread=nil
          end)

          vsettings.Value['LastCheck']=os.time() --last successful check
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
    local LastCheck=tonumber(vsettings.Value['LastCheck']) or 0 --get the time of the last successful check
    local CheckInterval=tonumber(vsettings.Value['CheckInterval']) or 1
    if (LastCheck+CheckInterval*60*60*24)<os.time() then
      CheckVersion(true)
    end
  end
end


local mi=createMenuItem(MainForm.MainMenu)
mi.Caption=translate('Check for new version')
mi.ImageIndex=15
-- also works: mi.Caption=translateID('VC-CFNV')
mi.OnClick=function(mi) CheckVersion(false) end

MainForm.miHelp.insert(MainForm.miAbout.MenuIndex,mi)

--add setting to the main ce config screen
local sf=getSettingsForm()
--I want it in front of the show undo button
--that means, take on the top and left anchor of the undo button, and change the anchor of the undo button to my item

local cbCheckForUpdatesOnLaunch=createCheckBox(sf)
local lblInterval=createLabel(sf)
local edtInterval=createEdit(sf)
local parent=sf.cbShowUndo.Parent --put it inside the same control as the "undo button checkbox" (the scrollbox)

cbCheckForUpdatesOnLaunch.Checked=vsettings.Value['CheckOnLaunch']=='1'
cbCheckForUpdatesOnLaunch.Caption=translate('Check for updates when Cheat Engine starts')..'.'
cbCheckForUpdatesOnLaunch.Parent=parent
cbCheckForUpdatesOnLaunch.AnchorSideTop.Control=edtInterval
cbCheckForUpdatesOnLaunch.AnchorSideTop.Side=asrCenter
cbCheckForUpdatesOnLaunch.AnchorSideLeft=sf.cbShowUndo.AnchorSideLeft

cbCheckForUpdatesOnLaunch.OnChange=function()
  lblInterval.Enabled=cbCheckForUpdatesOnLaunch.Checked
  edtInterval.Enabled=cbCheckForUpdatesOnLaunch.Checked
end

cbCheckForUpdatesOnLaunch.OnChange(cbCheckForUpdatesOnLaunch)

lblInterval.Caption=translate('Interval(days):')
lblInterval.Parent=parent
lblInterval.AnchorSideTop.Control=cbCheckForUpdatesOnLaunch
lblInterval.AnchorSideTop.Side=asrCenter
lblInterval.AnchorSideLeft.Control=cbCheckForUpdatesOnLaunch
lblInterval.AnchorSideLeft.Side=asrRight


if vsettings then
  edtInterval.Text=tonumber(vsettings.Value['CheckInterval']) or '1'
else
  edtInterval.Text='1'
end

edtInterval.ClientWidth=sf.Canvas.getTextWidth(' XX ');
edtInterval.Parent=parent
edtInterval.AnchorSideTop = sf.cbShowUndo.AnchorSideTop
edtInterval.AnchorSideLeft.Control=lblInterval
edtInterval.AnchorSideLeft.Side=asrRight


sf.cbShowUndo.AnchorSideTop.Control=edtInterval
sf.cbShowUndo.AnchorSideTop.Side=asrBottom --put the top of the "undo button checkbox" to the bottom of the new edtInterval (so below it)

sf.cbShowUndo.AnchorSideLeft.Control=cbCheckForUpdatesOnLaunch
sf.cbShowUndo.AnchorSideLeft.Side=asrLeft --put the left of the "undo button checkbox" to the left side of the new checkbox (so same start)




--now capture when the action is applied. I could hijack the button, but exceptions can sometimes cause issues. (though this button should not give exceptions in 6.7+ anymore)
local oldSettingsFormClose=sf.OnClose

sf.OnClose=function(f, closeAction)
  local result=closeAction
  if sf.ModalResult==mrOK then --the user clicked OK and all checks passed
    vsettings.Value['CheckOnLaunch']=cbCheckForUpdatesOnLaunch.Checked
    vsettings.Value['CheckInterval']=edtInterval.Text
  end

  --call the original OnClose of the settings form
  if oldSettingsFormClose then
    return oldSettingsFormClose(f, closeAction);
  end
end

