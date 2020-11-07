if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'SaveSessions.po')
end


if cheatEngineIs64Bit() then
  if string.find(package.cpath, 'clibs64')==nil then
    package.cpath=package.cpath..[[;.\clibs64\?.dll]]
  end
else
  if string.find(package.cpath, 'clibs32')==nil then
    package.cpath=package.cpath..[[;.\clibs32\?.dll]]
  end
end

require("lfs")

function loadMemoryScan_internal(filename)
  --print("loadMemoryScan")
  
  --the thread is used to bypasses a bug in 6.3

	local ms=getCurrentMemscan()
	local mf=getMainForm()




	local input,err=createFileStream(filename,fmOpenRead or fmShareDenyNone)
  if input==nil then
    MessageDialog(err, mtError,mbOK)
    return
  end

	local scanvalue=input.readAnsiString()
  local originalFromAddress=input.readAnsiString()  
  local originalToAddress=input.readAnsiString()  
	local scantype=input.readByte()
	local vartype=input.readByte()
  

	local savedscancount=input.readByte()

	local savedscans={}
	for i=1,savedscancount do	 
		savedscans[i]=input.readAnsiString()
	end

  --initial data has been read, now setup the scan state to be compatible with the saved state
	--easiest is just do a small scan

	--ms.newscan()
	--since ms.newScan was never implemented in ce 6.3 click on new scan if needed
	if ms.LastScanType~="stNewScan" then
    mf.btnNewScan.doClick() --new scan
  end


	mf.scanvalue.Text='123' --nice number
	mf.vartype.itemindex=vartype --while I could have used ms.firstScan this is easier since I don't need to convert itemindex to vartype
	mf.scantype.itemindex=0 --exact value

	mf.cbWritable.setState(cbGrayed)
	mf.cbExecutable.setState(cbGrayed)
	mf.cbCopyOnWrite.setState(cbGrayed)

  mf.FromAddress.Text=string.format("%x", getAddress("kernel32.dll"))    
  mf.ToAddress.Text=string.format("%x", getAddress("kernel32.dll")+1)        

	--first scan
  local oldOnScanDone=ms.OnScanDone
  local oldOnInitialScanDone=ms.OnInitialScanDone
  ms.OnScanDone=function(m)
    ms.OnScanDone=oldOnScanDone
    ms.OnInitialScanDone=oldOnInitialScanDone
      
    --tell the memscan that there are saved scans
    for i=1, savedscancount do
      ms.saveCurrentResults(savedscans[i])
    end

    local fl=ms.FoundList;

    fl.deinitialize() --release the file handles



    --overwrite the files with the ones in this archive

    local filecount=input.readByte() --nr of files
    --print("filecount="..filecount)

    for i=1, filecount do  --for each file
      --get the filename
      local name=input.readAnsiString()
      
     -- print("loading "..name)

      --get the filesize
      local filesize=input.readQword()
      local output,err=createFileStream(getCurrentMemscan().ScanresultFolder..name, fmCreate)
      if not output then
        MessageDialog(err, mtError,mbOK)
        input.destroy()    
        return      
      end
      
      output.CopyFrom(input, filesize)     
      output.destroy()
    end

    input.destroy()
    
    if oldOnInitialScanDone then
      oldOnInitialScanDone(m)
    end
    
    if oldOnScanDone then
      oldOnScanDone(m)
    end
    

    fl.initialize() --reopen the files

    mf.scanvalue.Text=scanvalue --nice number
    mf.vartype.itemindex=vartype --while I could have used ms.firstScan this is easier since I don't need to convert itemindex to vartype
    mf.scantype.itemindex=scantype --exact value

    mf.FromAddress.Text=originalFromAddress
    mf.ToAddress.Text=originalToAddress  
    mf.foundcountlabel.Caption=fl.Count
  
  end
  ms.OnInitialScanDone=nil
  
  mf.btnNewScan.doClick() --new scan
  
  ms.waitTillDone() 
end

function loadMemoryScan()

	if getOpenedProcessID()==0 then
    messageDialog(translate("Open a process first"), mtError, mbOK)
    return
	end


	local dialog=createOpenDialog()
	dialog.DefaultExt=".CS"
	dialog.Filter=translate("Cheat Engine Scan files").." (*.CS)|*.CS"
	dialog.FilterIndex=1
  dialog.Options="[ofEnableSizing]"    

	if dialog.execute()==false then return end
	
  loadMemoryScan_internal(dialog.Filename)
	dialog.destroy()
end


function saveMemoryScan_internal(filename)
  local i,j


  --6.3 doesn't have a folder picker, so create one file that holds all data
  local output,err=createFileStream(filename,fmCreate)
  if output==nil then
    MessageDialog(err, mtError,mbOK)
    return
  end
  
  

  --save some settings
  local mf=getMainForm()

  --current scanvalue
  output.writeAnsiString(mf.scanvalue.Text)
  output.writeAnsiString(mf.fromAddress.Text)  
  output.writeAnsiString(mf.toAddress.Text)    
  output.writeByte(mf.scantype.ItemIndex)
  output.writeByte(mf.VarType.ItemIndex)

  --get the filelist
  local files={}

  local olddir=lfs.currentdir()
  lfs.chdir(getCurrentMemscan().ScanresultFolder) --undocumented features are fun....


  for file in lfs.dir('.') do
    if string.sub(file,1,1)~='.' then
      local f={}
      f.name=file
      f.size=lfs.attributes(file).size

      table.insert(files, f)
    end
  end

  --check the extensions for other things than first, undo or tmp
  local savedscans={}
  for i=1,#files do
    local ext=files[i].name:match("%.([^%.]+)$")

    if (string.upper(ext)~='FIRST') and
       (string.upper(ext)~='TMP')   and
       (string.upper(ext)~='UNDO')  then
      --check if it's already in the list
      local found=false

      for j=1,#savedscans do
        if savedscans[j]==ext then
          found=true
          break
        end
      end

      if found==false then
        table.insert(savedscans, found)
      end
    end
  end

  output.writeByte(#savedscans)

  for i=1,#savedscans do
    output.writeAnsiString(savedscans[i])  
  end

  --now save the files
  output.writeByte(#files) --number of files
  for i=1, #files do
    --write the filename
    output.writeAnsiString(files[i].name)
    
    --print("saving "..files[i].name)

    local input,err=createFileStream(getCurrentMemscan().ScanresultFolder..files[i].name,fmOpenRead | fmShareDenyNone)
    
    if input==nil then
      MessageDialog(err, mtError,mbOK)
      output.destroy()
      return
    else    
      --write the filesize (qword)
      output.writeQword(input.Size)
      output.CopyFrom(input,input.Size)    
      input.destroy()
    end
  end

  lfs.chdir(olddir)


  output.destroy()
  
  --print("done")
end

function saveMemoryScan()
	if getOpenedProcessID()==0 then
    messageDialog(translate("Open a process first"), mtError, mbOK)
    return
	end


  local dialog=createSaveDialog()
  dialog.DefaultExt=".CS"
  dialog.Filter=translate("Cheat Engine Scan files").." (*.CS)|*.CS"
  dialog.FilterIndex=1
  dialog.Options="[ofEnableSizing,ofOVerwritePrompt]"

  if dialog.execute()==false then return nil end

  saveMemoryScan_internal(dialog.Filename)
  dialog.destroy()
end

local mf=getMainForm()

SaveScanSession={}


SaveScanSession.miSaveScanSession=createMenuItem(mf.Menu)
SaveScanSession.miSaveScanSession.caption=translate('Save scan session')
--SaveScanSession.miSaveScanSession.ImageIndex=39
SaveScanSession.miSaveScanSession.Shortcut='Ctrl+Alt+Shift+S'
SaveScanSession.miSaveScanSession.OnClick=saveMemoryScan
SaveScanSession.miSaveScanSession.Enabled=false

local s=createPicture()
s.LoadFromFile(getCheatEngineDir()..[[autorun\images\export128x128.png]])
local ii=MainForm.mfImageList.add(s.Bitmap)
SaveScanSession.miSaveScanSession.ImageIndex=ii
s.destroy()

mf.Menu.Items[0].insert(9, SaveScanSession.miSaveScanSession)


SaveScanSession.miLoadScanSession=createMenuItem(mf.Menu)
SaveScanSession.miLoadScanSession.caption=translate('Load scan session')
--SaveScanSession.miLoadScanSession.ImageIndex=38
SaveScanSession.miLoadScanSession.Shortcut='Ctrl+Alt+Shift+O'
SaveScanSession.miLoadScanSession.OnClick=loadMemoryScan
SaveScanSession.miLoadScanSession.Enabled=false

local s=createPicture()
s.LoadFromFile(getCheatEngineDir()..[[autorun\images\import128x128.png]])
local ii=MainForm.mfImageList.add(s.Bitmap)
SaveScanSession.miLoadScanSession.ImageIndex=ii
s.destroy()

mf.Menu.Items[0].insert(10, SaveScanSession.miLoadScanSession)

local mi=createMenuItem(mf.Menu) --seperator
mi.caption='-'
mf.Menu.Items[0].insert(11, mi)

mi.Visible=MainForm.miSignTable.Visible

local oldFileMenuClick=mf.Menu.Items[0].OnClick

mf.Menu.Items[0].OnClick=function(sender)
  if (oldFileMenuClick) then
    oldFileMenuClick(sender)
  end

  --check that it isn't a first scan
  local enable=getCurrentMemscan().lastScanWasRegionScan==false

  SaveScanSession.miSaveScanSession.Enabled=enable
  SaveScanSession.miLoadScanSession.Enabled=true
end
