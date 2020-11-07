--saves the current scan
--creates a new tab
--loads the scan into this tab

function splitScanIntoNewTab()
  local filename=getCurrentMemscan().ScanresultFolder..'.splitscan.savestate' --files starting with a . do not get copied so this is safe
 
  saveMemoryScan_internal(filename)
  
  MainForm.miAddTab.doClick()
  
  loadMemoryScan_internal(filename)  
end

local mi=createMenuItem(MainForm.Menu)
mi.Caption=translate('Split scan into new tab')
mi.ShortCut=textToShortCut('Ctrl+Alt+T')
mi.OnClick=splitScanIntoNewTab
mi.ImageIndex=MainForm.miAddTab.ImageIndex
MainForm.Menu.Items[0].Insert(MainForm.miAddTab.MenuIndex+1,mi)
