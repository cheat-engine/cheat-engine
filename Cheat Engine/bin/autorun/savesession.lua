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

function loadMemoryScan_thread(t)
  --the thread is used to bypasses a bug in 6.3
  t.synchronize(function(t)
	  ms=getCurrentMemscan()
	  mf=getMainForm()

	  if getOpenedProcessID()==0 then
		messageDialog(translate("Open a process first"), mtError, mbOK)
		return
	  end


	  dialog=createOpenDialog()
	  dialog.DefaultExt=".CS"
	  dialog.Filter=translate("Cheat Engine Scan files").." (*.CS)|*.CS"
	  dialog.FilterIndex=1

	  if dialog.execute()==false then return end
	   filename=dialog.Filename
	  dialog.destroy()


	   input=io.open(filename,"rb")

	   scanvaluelength=string.byte(input:read(1))
	   scanvalue=input:read(scanvaluelength)



	   scantype=string.byte(input:read(1))
	   vartype=string.byte(input:read(1))

	   savedscancount=string.byte(input:read(1))

	  savedscans={}
	  for i=1,savedscancount do
		 length=string.byte(input:read(1))
		savedscans[i]=input:read(length)
	  end

	  --initial data has been read, now setup the scan state to be compatible with the saved state
	  --easiest is just do a small scan

	  --ms.newscan()
	  --since ms.newScan was never implemented in ce 6.3 click on new scan if needed
	  if ms.LastScanType~="stNewScan" then

		if mf.btnNewScan==nil then --ce 6.4 uses this name, 6.3 still uses the not so normal name
		  mf.button2.doClick() --new scan
		else
		  mf.btnNewScan.doClick() --new scan
		end
	  end


	  mf.scanvalue.Text='982451653' --nice number
	  mf.vartype.itemindex=vartype --while I could have used ms.firstScan this is easier since I don't need to convert itemindex to vartype
	  mf.scantype.itemindex=0 --exact value

	  mf.cbWritable.setState(cbGrayed)
	  mf.cbExecutable.setState(cbGrayed)
	  mf.cbCopyOnWrite.setState(cbGrayed)

    if mf.FromAddress.Lines==nil then
      mf.FromAddress.Text=string.format("%x", getAddress("kernel32.dll"))    
    else
	    mf.FromAddress.Lines.Text=string.format("%x", getAddress("kernel32.dll"))
    end
    
    if mf.ToAddress.Lines==nil then
      mf.ToAddress.Text=string.format("%x", getAddress("kernel32.dll")+1)        
    else
      mf.ToAddress.Lines.Text=string.format("%x", getAddress("kernel32.dll")+1)    
    end
    
	  

	  --first scan
	  if mf.btnNewScan==nil then --ce 6.4 uses this name, 6.3 still uses the not so normal name
		mf.button2.doClick() --new scan
	  else
		mf.btnNewScan.doClick() --new scan
	  end


  end)


  if (savedscancount==nil) then
    return
  end
  


  ms.waitTillDone() --this would freeze in the main thread in 6.3


  
  
  
  
  t.synchronize(function(t)

	  --tell the memscan that there are saved scans
	  for i=1, savedscancount do
		ms.saveCurrentResults(savedscans[i])
	  end



	  local fl=ms.FoundList;

	  fl.deinitialize() --release the file handles



	  --overwrite the files with the ones in this archive
	  local olddir=lfs.currentdir()
	  lfs.chdir(getCurrentMemscan().ScanresultFolder)


	  local filecount=string.byte(input:read(1)) --nr of files

	  for i=1, filecount do  --for each file
		--get the filename
		local filenamelength=string.byte(input:read(1))
		local name=input:read(filenamelength)

		--get the filesize
		local filesize=byteTableToQword({string.byte(input:read(8),1,8)})
		local output=io.open(name,"wb")

		while output==nil do --it's possible that the first scan save is in progress, so keep trying
		  sleep(40)
		  output=io.open(name,"wb")
		end

		local data

		j=filesize
		while j>0 do
		  local block=512*1024
		  if block>j then
			block=j
		  end

		  data=input:read(block)
		  output:write(data)

		  j=j-block
		end

		output:close()
	  end

	  input:close()


	  lfs.chdir(olddir)

	  fl.initialize() --reopen the files

	  mf.scanvalue.Text=scanvalue --nice number
	  mf.vartype.itemindex=vartype --while I could have used ms.firstScan this is easier since I don't need to convert itemindex to vartype
	  mf.scantype.itemindex=scantype --exact value


	  mf.foundcountlabel.Caption=fl.Count

	  mf.miResetRange.doClick()

  end)
end

function loadMemoryScan()


  createNativeThread(loadMemoryScan_thread)


end


function saveMemoryScan()
  local i,j

  if getOpenedProcessID()==0 then
    messageDialog(translate("Open a process first and do a scan"), mtError, mbOk)
    return
  end


  local dialog=createSaveDialog()
  dialog.DefaultExt=".CS"
  dialog.Filter=translate("Cheat Engine Scan files").." (*.CS)|*.CS"
  dialog.FilterIndex=1

  if dialog.execute()==false then return nil end

  local filename=dialog.Filename
  dialog.destroy()



  --6.3 doesn't have a folder picker, so create one file that holds all data
  local output=io.open(filename,"wb")

  --save some settings
  local mf=getMainForm()

  --current scanvalue
  output:write(string.char(#mf.scanvalue.Text))
  output:write(mf.scanvalue.Text)

  output:write(string.char(mf.scantype.ItemIndex))
  output:write(string.char(mf.VarType.ItemIndex))


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
     (string.upper(ext)~='TMP') and
     (string.upper(ext)~='UNDO') then

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

  output:write(string.char(#savedscans))

  for i=1,#savedscans do
    output:write(string.char(#savedscans[i]))
    output:write(savedscans[i])
  end

  --now save the files
  output:write(string.char(#files)) --number of files
  for i=1, #files do
    --write the filename
    output:write(string.char(#files[i].name))
    output:write(files[i].name)

    local input=io.open(files[i].name,"rb")
  local data

  --write the filesize (qword)
  j=files[i].size

  output:write(string.char(unpack(qwordToByteTable(files[i].size))))


  --write the file
  while j>0 do
    local block=512*1024
    if block>j then
      block=j
    end

    data=input:read(block)
    output:write(data)

    j=j-block
  end




  input:close()
  end

  lfs.chdir(olddir)


  output:close()

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

mi=createMenuItem(mf.Menu) --seperator
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
