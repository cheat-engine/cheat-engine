if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'patchscan.po')
end

local IMAGE_SCN_CNT_CODE=0x20
local IMAGE_SCN_MEM_EXECUTE=0x20000000

function byteTableToHexString(bt)
  local i
  local r=''

  if bt then
    for i=1,#bt do
      r=r..string.format("%.2x ",bt[i])
    end
  end
  return r
end


function scanModuleForPatches(modulepath, loadedModuleBase)

  local original=createMemoryStream()
  r,e=original.loadFromFileNoError(modulepath)
  if not r then
    original.destroy()
    return false,e
  end
  original.Position=0


  if (byteTableToString(original.read(2))~='MZ') then
    original.destroy()
    return nil,translate('Not a valid executable')
  end

  original.Position=60;
  local lfanew=original.readDword();
  original.Position=lfanew;

  if (byteTableToString(original.read(2))~='PE') then
    original.destroy()
    return nil,translate('Not a valid windows executable')
  end

  original.position=original.Position+2
  local Machine=original.readWord()
  local SectionCount=original.readWord()
  original.Position=original.Position+12
  local OptHeaderSize=original.readWord()
  original.position=original.Position+2

  local OptHeaderStart=original.Position
  local Magic=original.readWord()
  local MajorLinkerVersion=original.readByte()
  local MinorLinkerVersion=original.readByte()

  local SizeOfCode=original.readDword()
  original.Position=original.Position+8
  local EntryPoint=original.readDword()
  local BaseOfCode=original.readDword()

  local BaseOfData
  local ImageBase
  --scan the reloc table
  if Machine==0x8664 then
    BaseOfData=nil
    ImageBase=original.readQword()
    original.Position=original.Position+4+4+2+2+2+2+2+2+4+4+4+4+2+2+8+8+8+8
  else
    BaseOfData=original.readDword()
    ImageBase=original.readDword()
    original.Position=original.Position+4+4+2+2+2+2+2+2+4+4+4+4+2+2+4+4+4+4
  end

  local RelocDistance=loadedModuleBase-ImageBase;


  local LoaderFlags=original.readDword()
  local RVACount=original.readDword()

  if RVACount~=16 then
    original.destroy()
    return nil,translate('This type of module is currently not supported')
  end

  --DataDirectory follows
  local DataDirPosition=original.Position
  local ImageSectionHeaderPosition=OptHeaderStart+OptHeaderSize

  --parse the sections so VirtualToFile can function
  local sections={}
  original.Position=ImageSectionHeaderPosition

  local i
  for i=1,SectionCount do
    sections[i]={}
    sections[i].name=byteTableToString(original.read(8))
    sections[i].misc=original.readDword()
    sections[i].virtualAddress=original.readDword()
    sections[i].sizeOfRawData=original.readDword()
    sections[i].PointerToRawData=original.readDword()
    original.position=original.position+12;
    sections[i].Characteristics=original.readDword()
    sections[i].Executable=sections[i].Characteristics & IMAGE_SCN_CNT_CODE == IMAGE_SCN_CNT_CODE

  end

  local function VirtualToFile(VA)
    --scan the address in the sections list
    local i
    local offset
    for i=1,#sections do
      if (VA>=sections[i].virtualAddress) and (VA<sections[i].virtualAddress+sections[i].sizeOfRawData) then
        return sections[i].PointerToRawData+(VA-sections[i].virtualAddress)
      end
    end
  end


  local ImportTableRVAInfo=DataDirPosition+1*8
  local RelocationTableRVAInfo=DataDirPosition+5*8


  original.Position=ImportTableRVAInfo
  local ImportTableAddress=original.readDword()
  local ImportTableSize=original.readDword()
  local ImportTablePosition=VirtualToFile(ImportTableAddress)

  original.Position=RelocationTableRVAInfo
  local RelocationTableAddress=original.readDword()
  local RelocationTableSize=original.readDword()
  local RelocationTablePosition=VirtualToFile(RelocationTableAddress)


  local relocs={}
  if RelocationTablePosition then

    original.position=RelocationTablePosition
    repeat
      local oldpos=original.position
      local VABase=original.readDword()
      local Size=original.readDword()

      local Entries=math.floor((Size - 8) / 2)
      --print(string.format("VABase=%x (Size=%d Entries=%d)", VABase, Size, Entries))

      local i
      for i=1,Entries do
        local relinfo=original.readWord()
        local reltype=relinfo >> 12
        local VA=VABase+(relinfo & 0xfff);
        if (reltype == 3) then
          writeIntegerLocal(original.Memory+VirtualToFile(VA),readIntegerLocal(original.Memory+VirtualToFile(VA))+RelocDistance)
        else
          if (reltype == 10) then
            writeQwordLocal(original.Memory+VirtualToFile(VA),readQwordLocal(original.Memory+VirtualToFile(VA))+RelocDistance)
          else
            if reltype ~= 0 then
              relocs[VA]=true --just mark it as a "I don't know"
            end
          end
        end
      end

      oldpos=oldpos+Size
    until original.Position>=RelocationTablePosition+RelocationTableSize
  else

  end

  --all information has been gathered. Now scan the code sections and compare against the target process
  --print("Scanning for differences...")
  local results={}

  for i=1 , #sections do
    if sections[i].Executable then
      local VA=loadedModuleBase+sections[i].virtualAddress
      local FA=original.Memory+sections[i].PointerToRawData
      local bytesLeft=sections[i].sizeOfRawData
      local bytesOK
      local result=false

      --print(string.format("Checking section %s ranging from %x to %x", sections[i].name, VA,VA+sections[i].sizeOfRawData))

      while (result==false) and (bytesLeft>0) do
        result,bytesOK=compareMemory(VA,FA,bytesLeft,1) --VA in target, FA in CE, so method 1
        if (result==false) then
          --local addressString=getNameFromAddress(VA+bytesOK)
          local entrynr=#results+1
          if (entrynr==1) or ((results[entrynr-1].Address+8)~=(VA+bytesOK)) then
            results[entrynr]={}
            results[entrynr].Address=VA+bytesOK
            results[entrynr].FileAddress=FA+bytesOK
            results[entrynr].Size=8
          else
            results[entrynr-1].Size=results[entrynr-1].Size+8
          end

          VA=VA+bytesOK+8
          FA=FA+bytesOK+8
          bytesLeft=bytesLeft-bytesOK-8
        end

        if result==nil then
          return nil, translate("Compare error. ")
        end
      end
    end
  end

  --get the bytes
  for i=1,#results do
    results[i].OriginalBytes=readBytesLocal(results[i].FileAddress, results[i].Size, true) --original.read(results[i].Size)
    results[i].PatchedBytes=readBytes(results[i].Address, results[i].Size, true)
  end


  original.destroy()

  return results
end

function startPatchScan()
  if getOpenedProcessID()==0 then
    openProcess(getCheatEngineProcessID())
  end
  local sl=createStringlist()
  local l=enumModules()
  for i=1,#l do
    sl.add(l[i].Name)
  end

  local msf=createForm(false)
  msf.Caption=translate('Module List')
  local label=createLabel(msf)
  label.Align='alTop'
  label.WordWrap=false
  label.Caption=translate('Select the modules to scan for patches. Hold shift/ctrl to select multiple modules')

  local btnPanel=createPanel(msf)
  btnPanel.ChildSizing.ControlsPerLine=2
  btnPanel.ChildSizing.Layout='cclLeftToRightThenTopToBottom'
  btnPanel.ChildSizing.TopBottomSpacing=5
  btnPanel.ChildSizing.EnlargeHorizontal='crsHomogenousSpaceResize'

  local btnOk=createButton(btnPanel)
  local btnCancel=createButton(btnPanel)
  btnOk.Caption=translate('  OK  ')
  btnOk.Default=true
  btnOk.ModalResult=mrOK
  btnCancel.Caption=translate('Cancel')
  btnCancel.Cancel=true
  btnCancel.ModalResult=mrCancel


  btnPanel.Align='alBottom'
  btnPanel.AutoSize=true
  btnPanel.BevelOuter='bvNone'

  local listbox=createListBox(msf)
  listbox.Items=sl
  listbox.MultiSelect=true
  listbox.Align='alClient'
  listbox.OnDblClick=function(l) msf.ModalResult=mrOK end

  msf.BorderStyle='bsSizeable'
  msf.Position='poScreenCenter'
  msf.autosize=true
  msf.OnShow=function(f)
    f.autosize=false
    msf.ClientHeight=msf.Canvas.getTextHeight('XGgxj')*10
  end
  if msf.showModal()==mrOK then
    local allpatches={}
    local i

    --todo: use a thread to do the scan


    --progressbar + currently scanned module
    local pform=createForm(false)
    pform.position='poScreenCenter'
    pform.ClientWidth = 600
    pform.ClientHeight = 30
    local psprogress = createProgressBar(pform)
    psprogress.ClientWidth = 600
    psprogress.ClientHeight = 30
    psprogress.Max = listbox.Items.Count-1
    psprogress.Min = 0
    psprogress.Position = 1
    pform.show()
    for i=0,listbox.Items.Count-1 do
      psprogress.position = i
      pform.Caption=string.format(translate("Scanning: %s"), l[i+1].Name)
      if listbox.Selected[i] then
        local modulepatches,emsg=scanModuleForPatches(l[i+1].PathToFile, l[i+1].Address)

        if modulepatches then
          local j
          for j=1,#modulepatches do
            local c=#allpatches+1
            allpatches[c]=modulepatches[j]
            allpatches[c].Modulename=l[i+1].Name --add the modulename (scanModuleForPatches doesn't add that)
          end
        else
          messageDialog(translate('Error in ')..l[i].name..':'..emsg, mtError, mbOK)
        end
      end
    end
    pform.close()
    pform.destroy()
    ---build a gui with the information in allpatches
    -- _G.dbg=allpatches

    local rform=createForm(false)
    local lv=createListView(rform)

    rform.Caption=translate('Patch list')

    lv.Align='alClient'
    lv.ViewStyle='vsReport'
    lv.ReadOnly=true
    lv.MultiSelect=true
    lv.RowSelect=true
    lv.HideSelection=false
    local caddress=lv.Columns.add()
    local coriginal=lv.Columns.add()
    local cpatched=lv.Columns.add()

    caddress.Width=rform.Canvas.GetTextWidth('XXXXXXXXXXXXXXXXXXXXXXXX')
    caddress.Caption=translate('Address')
    coriginal.Width=rform.Canvas.GetTextWidth('XX XX XX XX XX XX XX XX XX')
    coriginal.Caption=translate('Original')
    cpatched.Width=coriginal.Width
    cpatched.Caption=translate('Patched')

    for i=1,#allpatches do
      local li=lv.Items.add()
      local s=allpatches[i]
      li.Caption=getNameFromAddress(s.Address)
      li.SubItems.Add(byteTableToHexString(s.OriginalBytes))
      li.SubItems.Add(byteTableToHexString(s.PatchedBytes))

      li.Data=createRef(s)
    end

    lv.OnDblClick=function(s)
      --_G.dbglv=lv
      if lv.Selected then
        local ref=getRef(lv.Selected.Data)

        getMemoryViewForm().DisassemblerView.SelectedAddress=ref.Address
      end
    end

    local pm=createPopupMenu(rform)
    local miRestore=createMenuItem(pm)
    local miPatch=createMenuItem(pm)

    pm.Images=getMemoryViewForm().mvImageList

    miRestore.Caption=translate('Restore with original')
    miRestore.ImageIndex=44
    miPatch.Caption=translate('Reapply patch')
    miPatch.ImageIndex=49
    pm.Items.add(miRestore)
    pm.Items.add(miPatch)

    miRestore.OnClick=function(s)
      local i
      for i=0, lv.Items.Count-1 do
        if lv.Items[i].Selected then
          local ref=getRef(lv.Items[i].Data)

          writeBytes(ref.Address, ref.OriginalBytes)
        end
      end
    end

    miPatch.OnClick=function(s)
      local i
      for i=0, lv.Items.Count-1 do
        if lv.Items[i].Selected then
          local ref=getRef(lv.Items[i].Data)

          writeBytes(ref.Address, ref.PatchedBytes)
        end
      end
    end


    lv.PopupMenu=pm

    rform.position='poScreenCenter'
    rform.ClientWidth=caddress.Width+coriginal.Width+cpatched.Width
    rform.ClientHeight=MainForm.Canvas.getTextHeight('XGgxj')*10
    rform.BorderStyle='bsSizeable'

    rform.show()

    rform.OnClose=function(f)
      local i
      for i=0,lv.Items.Count-1 do
        local ref=lv.Items[i].Data
        destroyRef(ref)
      end

      rform=nil
      return caFree
    end
  end

  msf.destroy()

  if inMainThread() then processMessages() end

end

local mv=getMemoryViewForm()
local mi=createMenuItem(mv.Menu)
mi.Caption=translate('Scan for patches')
mi.ImageIndex=10
mi.Shortcut='Ctrl+Shift+P'
mi.OnClick=startPatchScan
mv.Extra1.insert(mv.DissectPEheaders1.MenuIndex+1, mi)