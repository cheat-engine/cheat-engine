--version check update script for cheat engine
--Don't like it? Just delete this file. Easy as that

--For the translators:
if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'VersionCheck.po')
end

local VersionCheckThread

local vsettings = getSettings("VersionCheck")
local checkOnLaunch -- checking for updates enabled?
local option        -- 0=onLaunch, 1=1day, 2=2days, ...
local lastCheck     -- last check time (lastCheck will be written back to registry only on successful check)

if vsettings then
  checkOnLaunch = vsettings.Value['CheckOnLaunch']=='1'
  option = tonumber(vsettings.Value['Option']) or 1 -- default is "once a day"
  lastCheck = tonumber(vsettings.Value['LastCheck']) or 0
end

local function CheckVersion(automatic)
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
            local nextCheckDateInfo = ''

            if checkOnLaunch and (option>0) then
              local today = os.date('*t'); today.hour=0; today.min=0; today.sec=0
              lastCheck = os.time(today)
              vsettings.Value['LastCheck'] = lastCheck
              nextCheckDateInfo = '\n'..string.format(translate('(next check after %s)'),os.date('%c',lastCheck+option*3600*24))
            end

            if newerVersion then
              if messageDialog(string.format(translate('Cheat Engine %s is available at www.cheatengine.org. Go there now?'),latestVersionString), mtConfirmation, mbYes, mbNo)==mrYes then
                shellExecute('http://cheatengine.org/')
              end
            else
              if not automatic then
                showMessage(string.format(translate('You are up to date. The latest version is %s'),latestVersionString)..nextCheckDateInfo)
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

if checkOnLaunch then
  if option==0 then -- on launch
    CheckVersion(true)
  elseif (lastCheck+option*3600*24)<=os.time() then -- once per interval
    CheckVersion(true)
  end
end


local mi=createMenuItem(MainForm.MainMenu)
mi.Caption=translate('Check for new version')
-- also works: mi.Caption=translateID('VC-CFNV')
mi.OnClick=function() CheckVersion(false) end

MainForm.miHelp.insert(MainForm.miAbout.MenuIndex,mi)

--add setting to the main ce config screen
local sf=getSettingsForm()
--I want it in front of the show undo button
--that means, take on the top and left anchor of the undo button, and change the anchor of the undo button to my item

local cbCheckForUpdatesOnLaunch=createCheckBox(sf)
cbCheckForUpdatesOnLaunch.checked=checkOnLaunch
cbCheckForUpdatesOnLaunch.Caption=translate('Check for updates when Cheat Engine starts')

cbCheckForUpdatesOnLaunch.Parent=sf.cbShowUndo.Parent --put it inside the same control as the undo button (the scrollbox)

cbCheckForUpdatesOnLaunch.AnchorSideTop=sf.cbShowUndo.AnchorSideTop
cbCheckForUpdatesOnLaunch.AnchorSideLeft=sf.cbShowUndo.AnchorSideLeft

sf.cbShowUndo.AnchorSideTop.Control=cbCheckForUpdatesOnLaunch
sf.cbShowUndo.AnchorSideTop.Side=asrBottom --put the top of the undo button to the bottom of the new checkbox (so below it)

sf.cbShowUndo.AnchorSideLeft.Control=cbCheckForUpdatesOnLaunch
sf.cbShowUndo.AnchorSideLeft.Side=asrLeft --put the left of the undo button to the left side of the new checkbox (so same start)

-- also add combobox with options
local cbCheckForUpdatesOptions=createComboBox(sf)
cbCheckForUpdatesOptions.ReadOnly=true

cbCheckForUpdatesOptions.Parent=sf.cbShowUndo.Parent

cbCheckForUpdatesOptions.AnchorSideTop.Control=cbCheckForUpdatesOnLaunch
cbCheckForUpdatesOptions.AnchorSideLeft.Control=cbCheckForUpdatesOnLaunch
cbCheckForUpdatesOptions.AnchorSideLeft.Side=asrRight

cbCheckForUpdatesOptions.Items.Text = [[every CE launch
once a day
once every 2 days
once every 3 days
once every 4 days
once every 5 days
once every 6 days
once every week
once every 7 days
once every 8 days
once every 9 days
once every 10 days
once every 11 days
once every 12 days
once every 13 days
once every 2 weeks]]

cbCheckForUpdatesOptions.ItemIndex = option

local width = sf.Canvas.getTextWidth('once every 2 weeks')
width = width + executeCodeLocal('user32.GetSystemMetrics', 2) + executeCodeLocal('user32.GetSystemMetrics', 9) --SM_CXVSCROLL = 2, SM_CYVTHUMB = 9
cbCheckForUpdatesOptions.Width = width

--now capture when the action is applied. I could hijack the button, but exceptions can sometimes cause issues. (though this button should not give exceptions in 6.7+ anymore)
local oldSettingsFormClose=sf.OnClose

sf.OnClose=function(f)
  if sf.ModalResult==mrOK then --the user clicked OK and all checks passed
    checkOnLaunch = cbCheckForUpdatesOnLaunch.Checked
    option = cbCheckForUpdatesOptions.ItemIndex
    vsettings.Value['CheckOnLaunch'] = checkOnLaunch
    vsettings.Value['Option'] = option
  end

  --call the original OnClose of the settings form
  if oldSettingsFormClose then
    return oldSettingsFormClose(f);
  end
end

