if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'pseudocodediagram.po')
end


--[[pseudocodediagram.lua]]--

local DPIAdjust=getScreenDPI()/96

--Global
diagramstyle = {}
diagramstyle.instruction_registerstyle       = '[31;1m' --red + bold
diagramstyle.instruction_hexstyle            = '[34;1m' --blue + bold
diagramstyle.instruction_symbolstyle         = '[32;1m' --green + bold
diagramstyle.instruction_opcodestyle         = '[1m' --bold
diagramstyle.link_defaultcolor               = 0x00FF00FF --fuchsia
diagramstyle.link_nottakencolor              = 0x000000FF --red
diagramstyle.link_takencolor                 = 0x00FF0000 --blue
diagramstyle.link_linethickness              = 3*DPIAdjust
diagramstyle.link_arrowsize                  = math.ceil(5*DPIAdjust)
diagramstyle.link_pointdepth                 = 20*DPIAdjust --distance between links
diagramstyle.block_headershowsymbol          = true
diagramstyle.block_bodyshowaddresses         = false
diagramstyle.block_bodyshowaddressesassymbol = true
diagramstyle.block_bodyshowbytes             = false
diagramstyle.block_backgroundcolor           = 0x00FFFFFF --white
diagramstyle.diagram_backgroundcolor         = 0x0099FFCC --light green
diagramstyle.zoom_min                        = 0.25
diagramstyle.zoom_max                        = 16.0
diagramstyle.zoom_step                       = 0.25
diagramstyle.highlight_linktakencolor        = 0x000000FF --red
diagramstyle.highlight_linknottakencolor     = diagramstyle.diagram_backgroundcolor  --invisible
diagramstyle.highlight_blockexecutedcolor    = diagramstyle.block_backgroundcolor 
diagramstyle.highlight_blocknotexecutedcolor = 0 --black   



local function disassembleDecoratedInstruction(address)
  local disassembler, result, bytes, temp = getVisibleDisassembler(), ' '
  temp = disassembler.disassemble(address)
  temp, temp, bytes, temp = splitDisassembledString(temp)
  if (diagramstyle.block_bodyshowaddresses and diagramstyle.block_bodyshowaddressesassymbol and inModule(address)) then 
    result = result .. 
             string.char(27) .. diagramstyle.instruction_symbolstyle ..
             getNameFromAddress(disassembler.LastDisassembleData.address) .. 
             string.char(27) .. '[0m' ..  ' - '
  elseif (diagramstyle.block_bodyshowaddresses) then 
    result = result .. 
             string.format('%X', disassembler.LastDisassembleData.address) .. ' - ' 
  end
  if (diagramstyle.block_bodyshowbytes) then result = result .. bytes .. ' - '  end
  result =  result .. 
            string.char(27).. diagramstyle.instruction_opcodestyle .. 
            disassembler.LastDisassembleData.opcode ..
            string.char(27) .. '[0m' .. ' '
  for word in string.gmatch(disassembler.LastDisassembleData.parameters,'[^{*}]*') do
    if result then
       if word == 'R' then --{R}=Register
          result = result .. string.char(27) .. diagramstyle.instruction_registerstyle
       elseif word == 'H' then --{H}=Hex value
          result = result .. string.char(27) .. diagramstyle.instruction_hexstyle
       elseif word == 'S' then --{S}=Symbol
          result = result .. string.char(27) .. diagramstyle.instruction_symbolstyle
       elseif word == 'N' then --{N}=Nothing special
          result = result .. string.char(27) .. 'c' --nothing
       else
          result = result .. word
       end
    else
       result = word
    end
  end
  return result
end

local function createDiagramForm(diagram, name)
  diagram.form=createForm(false)
  diagram.form.PopupMode='pmNone'
  diagram.form.BorderStyle='bsSizeable'
  diagram.form.Caption=name
  diagram.form.Width=getScreenWidth()-(getScreenWidth()/6)
  diagram.form.Height=getScreenHeight()-(getScreenHeight()/6)
end

local function calculateInbetweenColor(c1,c2,v)
  --v=0 : c1
  --v=1 : c2
  if (c1==nil) or (c2==nil) or (v==nil) then
    print("calculateInbetweenColor invalid paramer")
    return nil  
  end
  --print(string.format('calculateInbetweenColor(%x,%x,%f)',c1,c2,v))

  local r1=c1 & 0xFF
  local g1=c1 >> 8 & 0xFF
  local b1=c1 >> 16 & 0xFF

  local r2=c2 & 0xFF
  local g2=c2 >> 8 & 0xFF
  local b2=c2 >> 16 & 0xFF


  local rr=((r2-r1)/255 * v)*255
  local rg=((g2-g1)/255 * v)*255
  local rb=((b2-b1)/255 * v)*255

  local newr=r1+rr
  local newg=g1+rg
  local newb=b1+rb

 
  local newv=math.floor(newr)+(math.floor(newg)<<8)+(math.floor(newb)<<16)

  return newv
end


local function showTracerPath(diagram, tracerform)
  --go through the blocklist and see if the start/stop address is in there
  local blockheat={}
  local linkheat={}
  local linkstaken={} 
  local maxblockheat=1
  local maxlinkheat=1
  local i
  local acl={} --address count list
  local selectedonly=tracerform.SelectionCount>1
  
  local blockaddresses={}
  for i=1,#diagram.blocks do
    blockaddresses[diagram.blocks[i].stop]=diagram.blocks[i]
  end
  
  for i=0,tracerform.Count-1 do
    local e=tracerform.Entry[i]
    
    
    if e and ((selectedonly==false) or e.selected) then            
      --if diagram.state.branchOrigins[e.address] then
      if blockaddresses[e.address] then
        --branching address, check the next address
        
        if i<tracerform.Count-1 then        
          local e2=tracerform.Entry[i+1]
          
          if linkstaken[e.address]==nil then --new originaddress
            linkstaken[e.address]={} 
          end
          
          if linkstaken[e.address][e2.address]==nil then --new destinationaddress
            linkstaken[e.address][e2.address]=1 --start at 0
          else
            linkstaken[e.address][e2.address]=linkstaken[e.address][e2.address]+1
          end
        end
      end
    
      if acl[e.address] == nil then
        acl[e.address] = 1
      else
        acl[e.address] = acl[e.address]+1
      end
    end
  end
  
  for i=1,#diagram.dblocks do
    local blockdata=diagram.blocks[diagram.dblocks[i].tag] --diagram.dblocks[i].tag 'should' be i, but in case that ever changes use the tag
    local count=acl[blockdata.start] or acl[blockdata.stop] --in case the trace starts from the center of a function    

    blockheat[i]={}
    blockheat[i].Block=diagram.dblocks[i]
      
    if count then
      blockheat[i].Count=count                  
    else
      blockheat[i].Count=0
    end
    
    maxblockheat=math.max(maxblockheat, blockheat[i].Count) 
  end
  
  for i=0,diagram.diagram.LinkCount-1 do
    local l=diagram.diagram.Link[i]    
    local obt=l.OriginBlock.Tag
    local dbt=l.DestinationBlock.Tag
    
    local originaddress=diagram.blocks[obt].stop
    local destinationaddress=diagram.blocks[dbt].start
    
    
    linkheat[i+1]={}
    linkheat[i+1].Link=l        
    
    if linkstaken[originaddress] and linkstaken[originaddress][destinationaddress] then
      linkheat[i+1].Count=linkstaken[originaddress][destinationaddress]      
    else
      linkheat[i+1].Count=0      
    end  
    
    maxlinkheat=math.max(maxlinkheat, linkheat[i+1].Count)    
  end
  
  --[[
  local origin,destination
  for origin,destination in pairs(linkstaken) do
    for d2,count in pairs(destination) do
      print(origin..'->'..destination..' '..count..' times')
    end
  end
  --]]  
  
  --blockheat and linkheat are set
  
  --now apply colors to the links and blocks

  
  local v
  for i=1,#blockheat do
    v=blockheat[i].Count/maxblockheat --v now contains a value between 0 and 1, 0 is absolutely not executed, 1 is fully executed
    blockheat[i].Block.BackgroundColor=calculateInbetweenColor(diagramstyle.highlight_blocknotexecutedcolor, diagramstyle.highlight_blockexecutedcolor, v)
  end
    
  for i=1,#linkheat do
    v=linkheat[i].Count/maxlinkheat --v now contains a value between 0 and 1, 0 is absolutely not executed, 1 is fully executed
    linkheat[i].Link.LineColor=calculateInbetweenColor(diagramstyle.highlight_linknottakencolor, diagramstyle.highlight_linktakencolor, v)
  end  
  
  diagram.diagram.DrawPlotPoints=false
  
  diagram.diagram.repaint()
  return blockheat,linkheat
end

local function showUltimap2Path(diagram, ultimapform)
  --no link data, it's non sequential data (yet, until I add seperate tracefile parsing which does contains sequential TNT packets)
  local blockheat={}
  local maxblockheat=1
  
  for i=1,#diagram.dblocks do    
    local blockdata=diagram.blocks[diagram.dblocks[i].tag]
    local isin,count=ultimapform.isInList(blockdata.start)
    
    blockheat[i]={} 
    blockheat[i].Block=diagram.dblocks[i]
    
    if isin then      
      blockheat[i].Count=count or 1
    else
      blockheat[i].Count=0
    end
    
    maxblockheat=math.max(maxblockheat, blockheat[i].Count) 
  end
  
  local v
  for i=1,#blockheat do
    v=blockheat[i].Count/maxblockheat --v now contains a value between 0 and 1, 0 is absolutely not executed, 1 is fully executed
    blockheat[i].Block.BackgroundColor=calculateInbetweenColor(diagramstyle.highlight_blocknotexecutedcolor, diagramstyle.highlight_blockexecutedcolor, v)
  end 
  
  diagram.diagram.repaint()
  return blockheat
end

local function showCodeFilterPath(diagram, codefilterform)
  --no link data 
  return showUltimap2Path(diagram, codefilerform) --codefilterform also has isInList just count is nil
end

local function createMenu(diagram)
  local mv=getMemoryViewForm()
  local mm=createMainMenu(diagram.form)
  local FileMenu=createMenuItem(mm)
  mm.Images = mv.mvImageList --icons from the memoryviewer's form
  FileMenu.Caption=translate('File')
  FileMenu.Name='miFile'
  
  local miLoad=createMenuItem(mm)
  miLoad.Caption=translate('Load from file')
  miLoad.Name='miLoad'
  miLoad.ImageIndex=23
  miLoad.Shortcut='Ctrl+O'
  
  miLoad.OnClick=function(s)
    local fd=createOpenDialog()
    fd.Title=translate('Select the file you wish to open')
    fd.DefaultExt='CEDIAG'
    fd.Filter=translate('Diagram files (*.CEDIAG )|*.CEDIAG')
    if fd.Execute() then
      local fs=createFileStream(fd.FileName,fmOpenRead)
      
      diagram.diagram.loadFromStream(fs)   
      
      diagram.dblocks={}
      
      for i=0,diagram.diagram.BlockCount-1 do
        diagram.dblocks[i+1]=diagram.diagram.Block[i]
        diagram.dblocks[i+1].OnDoubleClickHeader=function(bl)
          local mwform = getMemoryViewForm()
          local dview = mwform.DisassemblerView
          dview.SelectedAddress = diagram.blocks[bl.Tag].start
          mwform.show()
        end
      end
      
      --Load sorted addresslist
      diagram.sortedAddressList={}
      local i
      local count=fs.readDword()
      --print("sortedAddressList count = "..count)
      for i=1,count do
        diagram.sortedAddressList[i]=fs.readQword() 
      end
      
      --Load diagram.blocks info
      diagram.blocks={}
      count=fs.readDword()
      --print("diagram.blocks count="..count)
      for i=1,count do
        --print("loading block "..i)
        diagram.blocks[i]={}
        diagram.blocks[i].start=fs.readQword()
        diagram.blocks[i].stop=fs.readQword()
        diagram.blocks[i].salstopindex=fs.readDword()
        local jc=fs.readWord()
        if jc>0 then
          diagram.blocks[i].getsJumpedToBy={}
          local j
          for j=1,jc do
            diagram.blocks[i].getsJumpedToBy[j]=fs.readQword()
          end          
        end
        
        local jtc=fs.readByte()
        if jtc>0 then
          diagram.blocks[i].jumpsTo={}
          diagram.blocks[i].jumpsTo.destinationtaken=fs.readQword()
          diagram.blocks[i].jumpsTo.destinationtaken=fs.readByte()==1          
        end        
      end 

      --print("reached end of load file")      
      
      fs.destroy()
    end
    

    
    
    fd.destroy()    
  end

  local miSave=createMenuItem(mm)
  miSave.Caption=translate('Save to file')
  miSave.Name='miSave'
  miSave.ImageIndex=22
  miSave.Shortcut='Ctrl+S'
  miSave.OnClick=function(s)
    
    local fd=createSaveDialog()
    fd.Title=translate('Fill in the filename you wish to save this diagram as')
    fd.DefaultExt='CEDIAG'
    fd.Filter=translate('Diagram files (*.CEDIAG )|*.CEDIAG')
    fd.Options='['..string.sub(string.sub(fd.Options,2),1,#fd.Options-2)..',ofOverwritePrompt'..']'
    if fd.Execute() then
      local fs=createFileStream(fd.FileName,fmCreate)
      diagram.diagram.saveToStream(fs)
      
      local i
      --save sorted addresslist

      fs.writeDword(#diagram.sortedAddressList)
      for i=1,#diagram.sortedAddressList do
        fs.WriteQword(diagram.sortedAddressList[i])
      end      
      
      
      --save diagram.blocks info
      
      fs.writeDword(#diagram.blocks)
      for i=1,#diagram.blocks do
        --print("Saving block "..i)
        fs.writeQword(diagram.blocks[i].start)
        fs.writeQword(diagram.blocks[i].stop)
        fs.writeDword(diagram.blocks[i].salstopindex)
        if diagram.blocks[i].getsJumpedToBy then
          fs.writeWord(#diagram.blocks[i].getsJumpedToBy)
          local j          
          for j=1,#diagram.blocks[i].getsJumpedToBy do
            fs.writeQword(diagram.blocks[i].getsJumpedToBy[j])          
          end
          
          
        else 
          fs.writeWord(0)
        end
        
        if diagram.blocks[i].jumpsTo then
          fs.writeByte(1)
          fs.writeQword(diagram.blocks[i].jumpsTo.destinationtaken or 0)
          if diagram.blocks[i].jumpsTo.destinationtaken then
            fs.writeByte(1)
          else
            fs.writeByte(0)
          end             

        else 
          fs.writeByte(0)
        end        
        
        
        
      end
      
      
      fs.destroy() 
    end
    fd.destroy()    
  end
  
  local miSaveAsImage=createMenuItem(mm)
  miSaveAsImage.Caption=translate('Save diagram to image')
  miSaveAsImage.Name='miSaveAsImage'
  miSaveAsImage.ImageIndex=47
  miSaveAsImage.OnClick=function(s)
    local fd=createSaveDialog()
    fd.Title=translate('Fill in the filename you wish to save this diagram image')
    fd.DefaultExt='PNG'
    fd.Filter=translate('PNG files (*.PNG )|*.PNG')
    if fd.Execute() then
      diagram.diagram.saveAsImage(fd.FileName)    
    end
    fd.destroy()    
  end
  
  local miClose=createMenuItem(mm)
  miClose.Caption=translate('Close')
  miClose.OnClick=function(s) diagram.form.close() end
  miClose.Name='miClose'
  miClose.ImageIndex=37
  
  local miSep=createMenuItem(mm)
  miSep.Caption='-'
  

  FileMenu.add(miLoad)
  FileMenu.add(miSave)
  FileMenu.add(miSaveAsImage)
  FileMenu.add(miSep)
  FileMenu.add(miClose)
  
  
  local DisplayMenu=createMenuItem(mm)  
  DisplayMenu.Caption=translate('Display')
  DisplayMenu.Name='miDisplay'
  
  local miPaths=createMenuItem(mm)
  miPaths.Caption=translate('Show path from Ultimap1/2 or Codefilter')  
  miPaths.ImageIndex=67
  miPaths.Name='miPaths'
  miPaths.OnClick=function(sender)
    local um=getUltimap2()
    if um and (um.Count>0) then
      showUltimap2Path(diagram, um)
      return
    end
    
    local cf=getCodeFilter()
    if cf then
      showCodeFilterPath(diagram, cf)
    end
  end
  
  local miTracerPaths=createMenuItem(mm)
  miTracerPaths.Caption=translate('Show path from tracer window') --if more than 1 show a list of tracer windows and pick one 
  miTracerPaths.ImageIndex=67
  miTracerPaths.Name='miTracerPaths'
  miTracerPaths.OnClick=function(sender)
    local i
    local tracers={}
    for i=0,getFormCount()-1 do
      local f=getForm(i)
      if f and f.ClassName=='TfrmTracer' then
        if f.Count>0 then
          tracers[#tracers+1]=f
        end
      end      
    end
    
    if #tracers>0 then
      local tracer=tracers[1]
      if #tracers>1 then      
        local list=createStringlist()
        --create a list
        
        for i=1,#tracers do
          if tracers[i].Count>0 then
            local first
            first=tracers[i].Entry[0].address
          
            list.add(string.format(translate('Tracer starting at %8x (%s)'), first, getNameFromAddress(first)))
          end
        end
        
        local r=showSelectionList(translate('Tracer paths'), translate('Which tracer window shall be used?'), list, false)
        if r then
          tracer=tracers[r+1]
        end        
        
        list.destroy()
      end
      
      if tracer then
        --print("Showing path")
        showTracerPath(diagram, tracer)
      end 
    else      
      showMessage(translate('No tracerform with results visible'));
    end
  end
  
  DisplayMenu.add(miPaths)
  DisplayMenu.add(miTracerPaths)
  

  local ViewMenu=createMenuItem(mm)  
  ViewMenu.Caption=translate('View')
  ViewMenu.Name='miView'

  local miZoom=createMenuItem(mm)
  miZoom.Caption=translate('Zoom')
  miZoom.ImageIndex=63
  miZoom.Name='miZoom'

  local miZoom100=createMenuItem(mm)
  miZoom100.Caption=translate('100%')  
  miZoom100.ImageIndex=63
  miZoom100.Name='miZoom100'
  miZoom100.OnClick=function()
    local newX=diagram.diagram.ScrollX/diagram.diagram.MaxScrollX
    local newY=diagram.diagram.ScrollY/diagram.diagram.MaxScrollY
    diagram.diagram.Zoom=1
    diagram.diagram.ScrollX=newX*diagram.diagram.MaxScrollX
    diagram.diagram.ScrollY=newY*diagram.diagram.MaxScrollY     
  end

  local miZoomIn=createMenuItem(mm)
  miZoomIn.Caption=translate('Zoom in')  
  miZoomIn.ImageIndex=61
  miZoomIn.Name='miZoomIn'
  miZoomIn.OnClick=function()
    if (diagram.diagram.Zoom < diagramstyle.zoom_max) then
      local newX=diagram.diagram.ScrollX/diagram.diagram.MaxScrollX
      local newY=diagram.diagram.ScrollY/diagram.diagram.MaxScrollY  
      diagram.diagram.Zoom = diagram.diagram.Zoom + diagramstyle.zoom_step
      diagram.diagram.ScrollX=newX*diagram.diagram.MaxScrollX
      diagram.diagram.ScrollY=newY*diagram.diagram.MaxScrollY  
    end  
  end

  local miZoomOut=createMenuItem(mm)
  miZoomOut.Caption=translate('Zoom out')  
  miZoomOut.ImageIndex=62
  miZoomOut.Name='miZoomOut'
  miZoomOut.OnClick=function()
    if (diagram.diagram.Zoom > diagramstyle.zoom_min) then
      local newX=diagram.diagram.ScrollX/diagram.diagram.MaxScrollX
      local newY=diagram.diagram.ScrollY/diagram.diagram.MaxScrollY  
      diagram.diagram.Zoom = diagram.diagram.Zoom - diagramstyle.zoom_step
      diagram.diagram.ScrollX=newX*diagram.diagram.MaxScrollX
      diagram.diagram.ScrollY=newY*diagram.diagram.MaxScrollY  
    end
  end

  ViewMenu.add(miZoom)
  miZoom.add(miZoom100)
  miZoom.add(miZoomIn)
  miZoom.add(miZoomOut)
  
  mm.Items.add(FileMenu)
  mm.Items.add(DisplayMenu)
  mm.Items.add(ViewMenu)
  
  --todo, add code to the menu items
end

local function DiagramContextPopup(sender, mousepos)
  --sender is the diagram object
  --showMessage('weee '..mousepos.x..','..mousepos.y)
  local diagram=getRef(sender.Tag) --diagram has in it's tag value a reference to the diagram table

  local islink=false
  local isblock=false
  local isheaderlessblock=false
  
  mousepos.x=(mousepos.x+diagram.diagram.ScrollX)/diagram.diagram.Zoom
  mousepos.y=(mousepos.y+diagram.diagram.ScrollY)/diagram.diagram.Zoom

    
  local obj=sender.getObjectAt(mousepos)
  if (obj) then
    islink=obj.ClassName=='TDiagramLink'
    isblock=(obj.ClassName=='TDiagramBlock') and (obj.ShowHeader)
    isheaderlessblock=(obj.ClassName=='TDiagramBlock') and (not obj.ShowHeader)
  end
  
  local i
  
  for i=1,#diagram.popup.LinkItems do diagram.popup.LinkItems[i].visible=islink end
  for i=1,#diagram.popup.BlockItems do diagram.popup.BlockItems[i].visible=isblock end
  for i=1,#diagram.popup.HeaderlessBlockItems do diagram.popup.HeaderlessBlockItems[i].visible=isheaderlessblock end

  diagram.popup.All[1].Visible=islink or isblock


  diagram.popup.lastobject=obj
  diagram.popup.lastpos=mousepos

  return false
end

local function scrollToDiagramBlock(diagram, dblock)
  diagram.diagram.ScrollX = dblock.x - math.abs((diagram.form.width / 2) - ((dblock.width) / 2))
  diagram.diagram.ScrollY = dblock.y - math.abs((diagram.form.height / 2) - ((dblock.height) / 2))
end

local function PopupMenuGoToSourceClick(sender)
  local diagram=getRef(sender.Owner.Owner.Tag) --the owner of the menuitem is the popupmenu, and the owner of that is the diagram (alternatively, the menuitem tag could be set to the diagram table as well)
  local sourceblock = diagram.popup.lastobject.OriginBlock
  scrollToDiagramBlock(diagram, sourceblock)
end

local function PopupMenuGoToDestinationClick(sender)
  local diagram=getRef(sender.Owner.Owner.Tag)
  local destinationblock = diagram.popup.lastobject.DestinationBlock
  scrollToDiagramBlock(diagram, destinationblock)
end

local function PopupMenuRemoveAllPointsClick(sender)
  local diagram=getRef(sender.Owner.Owner.Tag)
  diagram.popup.lastobject.removeAllPoints()
  diagram.diagram.repaint()
end

local function PopupMenuEditBlockHeaderClick(sender)
  local diagram=getRef(sender.Owner.Owner.Tag)
  local newheader = inputQuery(translate("Edit"), translate("new header"), diagram.popup.lastobject.caption)
  if newheader ~= nil then diagram.popup.lastobject.caption = newheader end
end

local function PopupMenuEditBlockBackgroundColorClick(sender) --a color dialog would be better
  local diagram=getRef(sender.Owner.Owner.Tag)
  local colordialog=createColorDialog()
  if colordialog.execute() then
    local newcolor = colordialog.Color
    diagram.popup.lastobject.BackgroundColor=newcolor
    diagram.diagram.repaint()
  end
  colordialog.destroy()
end

local function PopupMenuListSourcesClick(sender)
  local diagram=getRef(sender.Owner.Owner.Tag)
  local linkz = diagram.popup.lastobject.getLinks()
  local stringlist = createStringlist()
  for i=1, #linkz.asDestination do
    
    if linkz.asDestination[i].OriginBlock==nil then
      error('linkz.asDestination[i].OriginBlock is nil')
    end
    stringlist.add(getNameFromAddress(diagram.blocks[linkz.asDestination[i].OriginBlock.tag].start))
  end
  local index = showSelectionList(translate("Sources list"), "", stringlist)
  if linkz.asDestination[index+1] ~= nil then
    sourceblock = linkz.asDestination[index+1].OriginBlock
    scrollToDiagramBlock(diagram, sourceblock)
  end
end

local function PopupMenuListDestinationsClick(sender)
  local diagram=getRef(sender.Owner.Owner.Tag)
  local linkz = diagram.popup.lastobject.getLinks()
  local stringlist = createStringlist()
  for i=1, #linkz.asSource do
    stringlist.add(getNameFromAddress(diagram.blocks[linkz.asSource[i].DestinationBlock.tag].start))
  end
  local index = showSelectionList(translate("Destinations list"), "", stringlist)
  if linkz.asSource[index+1] ~= nil then
    destinationblock = linkz.asSource[index+1].DestinationBlock
    scrollToDiagramBlock(diagram, destinationblock)
  end
end

local function editBlockStrings(b)
  local result=false
  local mf=createForm(false)
  mf.Caption=translate('Block editor')
  mf.PopupMode='pmNone'
  mf.BorderStyle=bsSizeable
  local m=createMemo(mf)
  local bh=createPanel(mf)
  local cbh=createPanel(bh)
  cbh.BevelOuter='bvNone'
  bh.Align=alBottom
  m.Align=alClient
  m.ScrollBars=ssAutoBoth
  m.WordWrap=false
  
      
  local btnOK=createButton(cbh)
  local btnCancel=createButton(cbh)
  
  btnOK.Caption=translate('OK')
  btnOK.Default=true
  btnOK.ModalResult=mrOK
  btnCancel.Caption=translate('Cancel')
  btnCancel.Cancel=true
  btnCancel.ModalResult=mrCancel  
  
  btnOK.AnchorSideLeft.Control=cbh
  btnOK.AnchorSideLeft.Side=asrLeft
  btnOK.AnchorSideTop.Control=cbh
  btnOK.AnchorSideTop.Side=asrTop
  
  btnCancel.AnchorSideLeft.Control=btnOK
  btnCancel.AnchorSideLeft.Side=asrRight
  btnCancel.BorderSpacing.Left=DPIAdjust*5
  
  btnOK.AutoSize=true
  btnCancel.AutoSize=true
  cbh.AutoSize=true
  bh.AutoSize=true
  
  cbh.AnchorSideLeft.Control=bh
  cbh.AnchorSideLeft.Side=asrCenter
  cbh.BorderSpacing.Top=3*DPIAdjust
  cbh.BorderSpacing.Bottom=3*DPIAdjust
  
  
  m.Lines.Text=b.Strings.Text
  
  mf.Position=poScreenCenter
  if mf.showModal()==mrOK then
    b.Strings.Text=m.Lines.Text
    b.owner.repaint()
    result=true
  end   
  mf.destroy()   

  return result  
end 

local function PopupMenuCreateAnnotationClick(sender)
  --create a new dialog at the popup location
  local diagram=getRef(sender.Owner.Owner.Tag)
  local b=diagram.diagram.createBlock()
  b.ShowHeader=false
  b.DragBody=true
  b.X=diagram.popup.lastpos.x
  b.Y=diagram.popup.lastpos.y
  b.Width=100*DPIAdjust
  b.Height=100*DPIAdjust
  
  diagram.diagram.repaint() --show the new block
  
  b.OnDoubleClickBody=function(s)
    return editBlockStrings(b)
  end
  local r=b.OnDoubleClickBody(b)
  if r then
    b.AutoSize=true
  else
    b.destroy()    
  end
  
  diagram.diagram.repaint()
end

local function PopupMenuDeleteAnnotationClick(sender)
  local diagram=getRef(sender.Owner.Owner.Tag)
  
  if diagram.popup.lastobject and diagram.popup.lastobject.ShowHeader==false then
    diagram.popup.lastobject.destroy()
    diagram.diagram.repaint()
    diagram.popup.lastobject=nil
  end
end


local function createDiagramPopupMenu(diagram)
  local pm=createPopupMenu(diagram.diagram)
  pm.Images = getMemoryViewForm().mvImageList --icons from the memoryviewer's form
  diagram.diagram.PopupMenu=pm
  diagram.diagram.OnContextPopup=DiagramContextPopup
  
  diagram.popup={}
  diagram.popup.Menu=pm
  diagram.popup.LinkItems={}
  diagram.popup.LinkItems[1]=CreateMenuItem(pm)
  diagram.popup.LinkItems[1].Caption=translate('Go to source')
  diagram.popup.LinkItems[1].ImageIndex=34
  diagram.popup.LinkItems[1].OnClick=PopupMenuGoToSourceClick

  diagram.popup.LinkItems[2]=CreateMenuItem(pm)
  diagram.popup.LinkItems[2].Caption=translate('Go to destination')
  diagram.popup.LinkItems[2].ImageIndex=34
  diagram.popup.LinkItems[2].OnClick=PopupMenuGoToDestinationClick

  diagram.popup.LinkItems[3]=CreateMenuItem(pm)
  diagram.popup.LinkItems[3].Caption='-' --separator
  
  diagram.popup.LinkItems[4]=CreateMenuItem(pm)
  diagram.popup.LinkItems[4].Caption=translate('Remove all points')      
  diagram.popup.LinkItems[4].ImageIndex=32
  diagram.popup.LinkItems[4].OnClick=PopupMenuRemoveAllPointsClick
  
  pm.Items.add(diagram.popup.LinkItems[1])
  pm.Items.add(diagram.popup.LinkItems[2])
  pm.Items.add(diagram.popup.LinkItems[3])
  pm.Items.add(diagram.popup.LinkItems[4])
  
  diagram.popup.BlockItems={}
  diagram.popup.BlockItems[1]=CreateMenuItem(pm)
  diagram.popup.BlockItems[1].Caption=translate('Edit block header')
  diagram.popup.BlockItems[1].ImageIndex=6
  diagram.popup.BlockItems[1].OnClick=PopupMenuEditBlockHeaderClick

  diagram.popup.BlockItems[2]=CreateMenuItem(pm)
  diagram.popup.BlockItems[2].Caption=translate('Edit block color')
  diagram.popup.BlockItems[2].ImageIndex=45
  diagram.popup.BlockItems[2].OnClick=PopupMenuEditBlockBackgroundColorClick

  diagram.popup.BlockItems[3]=CreateMenuItem(pm)
  diagram.popup.BlockItems[3].Caption='-' --separator
  
  diagram.popup.BlockItems[4]=CreateMenuItem(pm)
  diagram.popup.BlockItems[4].Caption=translate('List sources')      
  diagram.popup.BlockItems[4].ImageIndex=36
  diagram.popup.BlockItems[4].OnClick=PopupMenuListSourcesClick  
  
  diagram.popup.BlockItems[5]=CreateMenuItem(pm)
  diagram.popup.BlockItems[5].Caption=translate('List destinations')  
  diagram.popup.BlockItems[5].ImageIndex=36    
  diagram.popup.BlockItems[5].OnClick=PopupMenuListDestinationsClick    
  
  
  pm.Items.add(diagram.popup.BlockItems[1])
  pm.Items.add(diagram.popup.BlockItems[2])
  pm.Items.add(diagram.popup.BlockItems[3])   
  pm.Items.add(diagram.popup.BlockItems[4])
  pm.Items.add(diagram.popup.BlockItems[5])
  

  
  diagram.popup.HeaderlessBlockItems={}
  diagram.popup.HeaderlessBlockItems[1]=CreateMenuItem(pm)
  diagram.popup.HeaderlessBlockItems[1].Caption=translate('Edit annotation color')
  diagram.popup.HeaderlessBlockItems[1].ImageIndex=45
  diagram.popup.HeaderlessBlockItems[1].OnClick=PopupMenuEditBlockBackgroundColorClick

  diagram.popup.HeaderlessBlockItems[2]=CreateMenuItem(pm)
  diagram.popup.HeaderlessBlockItems[2].Caption=translate('Delete annotation')
  diagram.popup.HeaderlessBlockItems[2].ImageIndex=32
  diagram.popup.HeaderlessBlockItems[2].OnClick=PopupMenuDeleteAnnotationClick

  pm.Items.add(diagram.popup.HeaderlessBlockItems[1])
  pm.Items.add(diagram.popup.HeaderlessBlockItems[2])
  
  diagram.popup.All={}
  diagram.popup.All[1]=CreateMenuItem(pm)
  diagram.popup.All[1].Caption='-'
  
  diagram.popup.All[2]=CreateMenuItem(pm)
  diagram.popup.All[2].Caption=translate('Create annotation') 
  diagram.popup.All[2].ImageIndex=65
  diagram.popup.All[2].OnClick=PopupMenuCreateAnnotationClick    
  pm.Items.add(diagram.popup.All[1])
  pm.Items.add(diagram.popup.All[2])    
end

local function createDiagramDiagram(diagram)
  diagram.diagram = createDiagram(diagram.form)
  diagram.diagram.Align='alClient'
  diagram.diagram.ArrowStyles='[asDestination,asOrigin]'
  diagram.diagram.BackgroundColor=diagramstyle.diagram_backgroundcolor
  diagram.diagram.BlockBackground=diagramstyle.block_backgroundcolor
  diagram.diagram.LineThickness=diagramstyle.link_linethickness
  diagram.diagram.ArrowSize=diagramstyle.link_arrowsize
  diagram.diagram.Tag=createRef(diagram)
  --diagram.diagram.AllowUserToCreatePlotPoints = false
  --diagram.diagram.AllowUserToMovePlotPoints = false
  --diagram.diagram.AllowUserToChangeAttachPoints = false
end

local function onBlockDrag(dblock)
  local linkz, point = dblock.getLinks(), {}
  for i=1, #linkz.asDestination do
    if (linkz.asDestination[i].Points ~= nil) and (linkz.asDestination[i].PointCount >= 1) then
      point.x = dblock.x + (dblock.width / 2) + linkz.asDestination[i].DestinationDescriptor.Position
      point.y = linkz.asDestination[i].Points[linkz.asDestination[i].PointCount-1].y
      linkz.asDestination[i].Points[linkz.asDestination[i].PointCount-1] = point
    end
  end
  for i=1, #linkz.asSource do
    if (linkz.asSource[i].Points ~= nil) and (linkz.asSource[i].PointCount >= 1) then
      point.x = dblock.x + (dblock.width / 2) + linkz.asSource[i].OriginDescriptor.Position
      point.y = linkz.asSource[i].Points[0].y
      linkz.asSource[i].Points[0] = point
    end
  end
end

local function createDiagramBlock(diagram, name)
  local diagramblock = diagram.diagram.createBlock()
  diagramblock.Caption=name
  diagramblock.OnDoubleClickHeader = function(bl)
    local mwform = getMemoryViewForm()
    local dview = mwform.DisassemblerView
    dview.SelectedAddress = diagram.blocks[bl.Tag].start
    mwform.show()
  end
  diagramblock.OnDrag=onBlockDrag
  return diagramblock
end

local function createDiagramHeaderlessBlock(diagram)
  local diagramheaderlessblock = diagram.diagram.createBlock()
  diagramheaderlessblock.ShowHeader=false
  diagramheaderlessblock.DragBody=true
  return diagramheaderlessblock
end

local function createDiagramLink(diagram, sourceblock, destinationblock, color, offset)
  local sourceBSD={}
  sourceBSD.Block=diagram.dblocks[sourceblock]
  sourceBSD.Side=dbsBottom
  sourceBSD.Position=offset 

  local offset2 = 0
  local linkz = diagram.dblocks[destinationblock].getLinks()
  if #linkz.asDestination > 0 then
    if math.fmod(#linkz.asDestination + 1,2) == 0 then offset2=(diagramstyle.link_pointdepth/2)*(#linkz.asDestination+1)
    else offset2=-(diagramstyle.link_pointdepth/2)*(#linkz.asDestination) end
  end

  if (diagram.dblocks[destinationblock].x+(diagram.dblocks[destinationblock].width/2)+offset2) >= (diagram.dblocks[destinationblock].x+diagram.dblocks[destinationblock].width) then
    diagram.dblocks[destinationblock].width=diagram.dblocks[destinationblock].width+math.abs((diagram.dblocks[destinationblock].x+diagram.dblocks[destinationblock].width)-(diagram.dblocks[destinationblock].x+(diagram.dblocks[destinationblock].width/2)+offset2))+40*DPIAdjust
  elseif (diagram.dblocks[destinationblock].x+(diagram.dblocks[destinationblock].width / 2) + offset2) <= diagram.dblocks[destinationblock].x then
    diagram.dblocks[destinationblock].width=diagram.dblocks[destinationblock].width+math.abs(diagram.dblocks[destinationblock].x-(diagram.dblocks[destinationblock].x+(diagram.dblocks[destinationblock].width / 2)+offset2))+40*DPIAdjust
  end
  
  local destinationBSD={}
  destinationBSD.Block=diagram.dblocks[destinationblock]
  destinationBSD.Side=dbsTop
  destinationBSD.Position=offset2
  
  local diagramlink = diagram.diagram.addConnection(sourceBSD, destinationBSD)
  
  diagramlink.LineColor=color

  return diagramlink
end

local function createDiagramBlocks(diagram)
  -- _G.d=diagram
  diagram.dblocks = {}
  for i=1, #diagram.blocks do
    if diagram.state.parsed[diagram.blocks[i].start] then
      if (diagramstyle.block_headershowsymbol and inModule(diagram.blocks[i].start)) then
        diagram.dblocks[i] = createDiagramBlock(diagram, ' ' .. string.char(27) .. diagramstyle.instruction_symbolstyle .. 
                                                        getNameFromAddress(diagram.blocks[i].start))
      else
        diagram.dblocks[i] = createDiagramBlock(diagram, ' ' .. string.format('%X', diagram.blocks[i].start))
      end

      diagram.dblocks[i].Tag = i

      local current = diagram.blocks[i].start
      while (current <= diagram.blocks[i].stop) do
        diagram.dblocks[i].Strings.add(disassembleDecoratedInstruction(current))
        if diagram.state.parsed[current] ~= nil and diagram.state.parsed[current].bytesize ~= 0 then 
          current = current + diagram.state.parsed[current].bytesize
        else break end  
      end
      diagram.dblocks[i].AutoSize = true
    end
  end
end

local function blockAddressToBlockIndex(diagram, address)
  for i=1, #diagram.blocks do
    if (diagram.blocks[i].start == address) or (diagram.blocks[i].stop == address) then
      return i
    end
  end
  return nil
end

local function diagramBlockToDiagramBlockIndex(diagram, dblock)
  for i=1, #diagram.dblocks do
    if diagram.dblocks[i] == dblock then
      return i
    end
  end
  return nil
end

local function diagramBlockInputToInputIndex(dblock, idblock)
  local linkz = dblock.getLinks()
  for i=1, #linkz.asDestination do
    if linkz.asDestination[i].OriginBlock == idblock then 
      return i 
    end
  end
  return 0
end

local function linkDiagramBlocks(diagram)
  for i=1, #diagram.dblocks do
    if i > 1 then --skip starting block
      for j=1, #diagram.blocks[i].getsJumpedToBy do
        if diagram.blocks[i].getsJumpedToBy[j] == diagram.blocks[i-1].stop then
          local link=createDiagramLink(diagram, i-1, i, diagramstyle.link_nottakencolor,10*DPIAdjust) --not taken branches
          local linkdata={}
          linkdata.isTaken=true          
          link.Tag=createRef(linkdata)
        end
      end
    end
    if (diagram.blocks[i].jumpsTo) then --skip leaf blocks
      local destinationblock = blockAddressToBlockIndex(diagram, diagram.blocks[i].jumpsTo.destinationtaken)

      if destinationblock then
        local linkdata={}
        local color=diagramstyle.link_takencolor
        local offset=-10*DPIAdjust
  
        if diagram.blocks[i].jumpsTo.destinationnottaken==nil then
          linkdata.unconditional=true  --also true for logicalFollow, but those where logicalFollow is false are jmp's
          color=diagramstyle.link_defaultcolor
          offset=0          
        end
        
        if diagram.blocks[i].jumpsTo.logicalFollow then          
          linkdata.logicalFollow=true
          color=diagramstyle.link_defaultcolor
          offset=0
        end        
                
        local link=createDiagramLink(diagram, i, destinationblock, color, offset) --taken branches
          
        linkdata.isTaken=false
        link.Tag=createRef(linkdata)
      end

    end
  end
end

local function createDiagramPseudoBlocks(diagram)
  diagram.dpblocks = {}
  for i=1, #diagram.dblocks do
    local linkz = diagram.dblocks[i].getLinks()
    diagram.dpblocks[i] = {}
    diagram.dpblocks[i].input_count_extra = #linkz.asDestination
    diagram.dpblocks[i].input_count = #linkz.asDestination
    diagram.dpblocks[i].input = {}
    diagram.dpblocks[i].output_count = #linkz.asSource
    diagram.dpblocks[i].output = {}
    diagram.dpblocks[i].betteroutput_count = 0
    diagram.dpblocks[i].betteroutput = {}
    diagram.dpblocks[i].column_count = 0
    diagram.dpblocks[i].column = 0
    diagram.dpblocks[i].row_count = 0
    diagram.dpblocks[i].row = 0
    diagram.dpblocks[i].odescriptor = {}
    diagram.dpblocks[i].ddescriptor = {}
    diagram.dpblocks[i].link = {}
    for j=1, #linkz.asDestination do 
      diagram.dpblocks[i].input[j] = diagramBlockToDiagramBlockIndex(diagram, linkz.asDestination[j].OriginBlock)
    end
    for j=1, #linkz.asSource do 
      diagram.dpblocks[i].output[j] = diagramBlockToDiagramBlockIndex(diagram, linkz.asSource[j].DestinationBlock)
      diagram.dpblocks[i].odescriptor[j] = linkz.asSource[j].OriginDescriptor
      diagram.dpblocks[i].ddescriptor[j] = linkz.asSource[j].DestinationDescriptor
      diagram.dpblocks[i].link[j] = linkz.asSource[j]
    end
  end
end

local function initDiagramVisitedBlocks(diagram, dvblocks)
  for i=1, #diagram.dblocks do
    dvblocks[i] = {}
    dvblocks[i].visited = false
  end
end

local function createQueue()
  return {first = 0, last = -1}
end

local function pushLeft (queue, value)
  local first = queue.first - 1
  queue.first = first
  queue[first] = value
end

local function popRight (queue)
  local last = queue.last
  if queue.first > last then 
    return nil 
  end
  local value = queue[last]
  queue[last] = nil
  queue.last = last - 1
  return value
end

--[[
  makes all the multiple inputs blocks, single input blocks
  the goal is to obtain a better block arrangement
--]]
local function ComputeBetterArrangement(diagram)
  local dvblocks, more, branchqueue, nextbranch = {}, true, createQueue(), {}
  initDiagramVisitedBlocks(diagram, dvblocks)
  dvblocks[1].visited = true
  pushLeft(branchqueue, 1) --starting block

  while (more) do 
    more=false
    while branchqueue.first <= branchqueue.last do
      local nextbranch = popRight(branchqueue)
      for j=1, diagram.dpblocks[nextbranch].output_count do
        if not dvblocks[diagram.dpblocks[nextbranch].output[j]].visited then
          if diagram.dpblocks[diagram.dpblocks[nextbranch].output[j]].input_count_extra == 1 then
            diagram.dpblocks[nextbranch].betteroutput[#diagram.dpblocks[nextbranch].betteroutput+1]=diagram.dpblocks[nextbranch].output[j]
            diagram.dpblocks[nextbranch].betteroutput_count = #diagram.dpblocks[nextbranch].betteroutput
            dvblocks[diagram.dpblocks[nextbranch].output[j]].visited=true
            pushLeft(branchqueue, diagram.dpblocks[nextbranch].output[j])
            more = true
          end
          diagram.dpblocks[diagram.dpblocks[nextbranch].output[j]].input_count_extra = diagram.dpblocks[diagram.dpblocks[nextbranch].output[j]].input_count_extra-1
        end
      end
    end

    nextbranch.branch_min=nil
    nextbranch.input_count_min=nil
    nextbranch.input_min=nil
    
    for i=1, #diagram.dblocks do
      if dvblocks[i].visited then
        for j=1, diagram.dpblocks[i].output_count do
          if not dvblocks[diagram.dpblocks[i].output[j]].visited then
            if (nextbranch.branch_min == nil) or (diagram.dpblocks[diagram.dpblocks[i].output[j]].input_count_extra == nextbranch.input_count_min) or 
            ((diagram.dpblocks[diagram.dpblocks[i].output[j]].input_count_extra == nextbranch.input_count_min) and (diagram.dpblocks[i].output[j] < nextbranch.branch_min)) then
              nextbranch.branch_min=diagram.dpblocks[i].output[j]
              nextbranch.input_count_min=diagram.dpblocks[diagram.dpblocks[i].output[j]].input_count_extra
              nextbranch.input_min=i
            end
          end
        end
      end
    end
    if nextbranch.branch_min ~= nil then
      diagram.dpblocks[nextbranch.input_min].betteroutput[#diagram.dpblocks[nextbranch.input_min].betteroutput+1]=nextbranch.branch_min
      diagram.dpblocks[nextbranch.input_min].betteroutput_count=#diagram.dpblocks[nextbranch.input_min].betteroutput
      dvblocks[nextbranch.branch_min].visited=true
      pushLeft(branchqueue, nextbranch.branch_min)
      diagram.dpblocks[nextbranch.branch_min].input_count_extra=diagram.dpblocks[nextbranch.branch_min].input_count_extra-1
      more = true
    end
  end
end

local function adjustEverything(diagram, dpblock, column, row)
  diagram.dpblocks[dpblock].column = diagram.dpblocks[dpblock].column + column
  diagram.dpblocks[dpblock].row= diagram.dpblocks[dpblock].row + row
  for i=1, diagram.dpblocks[dpblock].betteroutput_count do
    local edge = diagram.dpblocks[dpblock].betteroutput[i]
    adjustEverything(diagram, edge, column, row)
  end
end

--[[
  computes the diagram's rows and columns
  we'll need them in order to arrange blocks, links and points
--]]
local function computeLayers(diagram, dpblock)
  local column, row_count, c_column = 0, 0, 0
  for i=1, diagram.dpblocks[dpblock].betteroutput_count do
    computeLayers(diagram, diagram.dpblocks[dpblock].betteroutput[i])
    if (diagram.dpblocks[diagram.dpblocks[dpblock].betteroutput[i]].row_count+1) > row_count then
      row_count = diagram.dpblocks[diagram.dpblocks[dpblock].betteroutput[i]].row_count+1
    end
    c_column = diagram.dpblocks[diagram.dpblocks[dpblock].betteroutput[i]].column
  end
  if diagram.dpblocks[dpblock].betteroutput_count == 2 then
    local better1, better2, offset = diagram.dpblocks[dpblock].betteroutput[1], diagram.dpblocks[dpblock].betteroutput[2]
    if (diagram.dpblocks[better1].betteroutput_count == 0) then
      diagram.dpblocks[better1].column = diagram.dpblocks[better2].column - 2
      if diagram.dpblocks[better1].column < 0 then offset = -diagram.dpblocks[better1].column else offset = 0 end
      adjustEverything(diagram, better1, offset, 1)
      adjustEverything(diagram, better2, offset, 1)
      column = diagram.dpblocks[better2].column_count + offset
    elseif (diagram.dpblocks[better2].betteroutput_count == 0) then
      adjustEverything(diagram, better1, 0, 1)
      adjustEverything(diagram, better2, diagram.dpblocks[better1].column + 2, 1)
      column = math.max(diagram.dpblocks[better1].column_count, diagram.dpblocks[better2].column + 2)
    else
      adjustEverything(diagram, better1, 0, 1)
      adjustEverything(diagram, better2, diagram.dpblocks[better1].column_count, 1)
      column = diagram.dpblocks[better1].column_count + diagram.dpblocks[better1].column_count
    end
    diagram.dpblocks[dpblock].column_count = math.max(2, column)
    diagram.dpblocks[dpblock].column = math.floor((diagram.dpblocks[better1].column + diagram.dpblocks[better2].column) / 2)
  else
    if diagram.dpblocks[dpblock].betteroutput_count == 1 then
      local edge = diagram.dpblocks[dpblock].betteroutput[1]
      adjustEverything(diagram, edge, column, 1)
      column = column + diagram.dpblocks[edge].column_count
    end
    if column >= 2 then
      if diagram.dpblocks[dpblock].betteroutput_count == 1 then diagram.dpblocks[dpblock].column = c_column
      else diagram.dpblocks[dpblock].column = math.floor((column - 2) / 2) end
      diagram.dpblocks[dpblock].column_count = column
    else
      diagram.dpblocks[dpblock].column, diagram.dpblocks[dpblock].column_count = 0, 2
    end
  end
  diagram.dpblocks[dpblock].row, diagram.dpblocks[dpblock].row_count = 0, row_count
end

local function initLayerRelatedStuff(diagram)
  diagram.points = {}
  diagram.paths = {}
  diagram.column_count = 0
  diagram.row_count = 0
  diagram.column = {}
  diagram.row = {}
  diagram.links_column = {}
  diagram.links_row = {}
  diagram.column_links_count = {}
  diagram.row_max_depth = {}
  for i=1, #diagram.dpblocks do
    diagram.column_count = math.max(diagram.dpblocks[i].column, diagram.column_count)
    diagram.row_count = math.max(diagram.dpblocks[i].row, diagram.row_count)
    diagram.points[i] = {}
    diagram.points[i].output_input = {}
    diagram.points[i].column = {}
    for j=1, #diagram.dpblocks[i].output do
      diagram.points[i].output_input[j] = {}
      diagram.points[i].output_input[j].point = 0
      diagram.points[i].column[j] = {}
      diagram.points[i].column[j].point = 0
    end
    for j=#diagram.dpblocks[i].output+1, #diagram.dpblocks[i].output+#diagram.dpblocks[i].input do
      diagram.points[i].output_input[j] = {}
      diagram.points[i].output_input[j].point = 0
    end
  end
  for i=-1, diagram.row_count do
    diagram.row[i] = {}
    diagram.links_row[i] = {}
    diagram.row[i].height = 0
    diagram.row[i].y = 0
    diagram.links_row[i].y = 0
    diagram.column_links_count[i] = {}
    diagram.column_links_count[i].count = 0
  end
  for i=0, diagram.column_count do
    diagram.column[i] = {}
    diagram.links_column[i] = {}
    diagram.column[i].width = 0
    diagram.column[i].x = 0
    diagram.links_column[i].x = 0
    diagram.row_max_depth[i] = {}
    diagram.row_max_depth[i].count = 0
  end
  for i=-1, diagram.row_count do
    diagram.paths[i] = {}
    for j=0, diagram.column_count do
      diagram.paths[i][j] = {}
      diagram.paths[i][j].path = {}
    end
  end
  for i=1, #diagram.dpblocks do
    diagram.column[diagram.dpblocks[i].column].width = math.max(diagram.dblocks[i].width, diagram.column[diagram.dpblocks[i].column].width)
    diagram.row[diagram.dpblocks[i].row].height = math.max(diagram.dblocks[i].height, diagram.row[diagram.dpblocks[i].row].height)
  end
end

local function computePoints(diagram)
  --output vertical points
  for i=1, #diagram.dpblocks do
    local origin = i
    for j=1, diagram.dpblocks[i].output_count do
      diagram.column_links_count[diagram.dpblocks[origin].row].count = diagram.column_links_count[diagram.dpblocks[origin].row].count+1
      diagram.points[i].output_input[j].point = diagram.column_links_count[diagram.dpblocks[origin].row].count
    end
  end
  --input vertical points
  for i=1, #diagram.dpblocks do
    local destination = i
    for j=1, diagram.dpblocks[i].input_count do
      diagram.column_links_count[diagram.dpblocks[destination].row-1].count = diagram.column_links_count[diagram.dpblocks[destination].row-1].count+1
      diagram.points[i].output_input[j+#diagram.dpblocks[i].output].point = diagram.column_links_count[diagram.dpblocks[destination].row-1].count
    end
  end
  --remaining horizontal points
  for i=1, #diagram.dpblocks do
    for j=1, diagram.dpblocks[i].output_count do
      local destination, path = diagram.dpblocks[i].output[j], 0
      local rowto, rowfrom = math.max(diagram.dpblocks[destination].row, diagram.dpblocks[i].row), math.min(diagram.dpblocks[destination].row, diagram.dpblocks[i].row)
      for i=rowfrom, rowto do path = math.max(#diagram.paths[i][diagram.dpblocks[destination].column].path, path) end
      for i=rowfrom, rowto do diagram.paths[i][diagram.dpblocks[destination].column].path[path+1]=true end
      diagram.row_max_depth[diagram.dpblocks[destination].column].count = math.max(diagram.row_max_depth[diagram.dpblocks[destination].column].count, path+1)
      diagram.points[i].column[j].point=path+1
    end
  end
end

local function arrangeDiagramLayers(diagram)
  local x = 20*DPIAdjust
  for i=0, diagram.column_count-1 do
    diagram.column[i].x=x
    x=x+diagram.column[i].width
    diagram.links_column[i].x=x
    x=x+(diagramstyle.link_pointdepth*diagram.row_max_depth[i].count)+diagramstyle.link_pointdepth
  end
  diagram.row[-1].y=0 --extra row
  diagram.row[-1].height=diagramstyle.link_pointdepth * (diagram.column_links_count[-1].count) --size = inputs * pointdepth
  local y=diagram.row[-1].height+diagramstyle.link_pointdepth
  for i=0, diagram.row_count-1 do
    diagram.row[i].y=y 
    y=y+diagram.row[i].height
    diagram.links_row[i].y=y
    y=y+(diagramstyle.link_pointdepth*diagram.column_links_count[i].count)+diagramstyle.link_pointdepth
  end
  diagram.column[diagram.column_count].x=x
  diagram.row[diagram.row_count].y=y
  diagram.links_column[diagram.column_count].x=x+diagram.column[diagram.column_count].width
  diagram.links_row[diagram.row_count].y=y+diagram.row[diagram.row_count].height
end

local function arrangeDiagramBlocks(diagram)
  for i=1, #diagram.dblocks do
    diagram.dblocks[i].x=diagram.column[diagram.dpblocks[i].column].x+(diagram.column[diagram.dpblocks[i].column].width/2)-(diagram.dblocks[i].width/2)
    diagram.dblocks[i].y=diagram.row[diagram.dpblocks[i].row].y
  end
end

local function arrangeDiagramLinks(diagram)
  for i=1, #diagram.dblocks do
    for j=1, diagram.dpblocks[i].output_count do
      local origin_row=diagram.dpblocks[i].row
      local destination_row=diagram.dpblocks[diagram.dpblocks[i].output[j]].row
      local link=diagram.dpblocks[i].link[j]
      local origin_index=i
      local destination_index=diagram.dpblocks[i].output[j]
      local input_index=diagramBlockInputToInputIndex(link.DestinationBlock, link.OriginBlock)+#diagram.dpblocks[destination_index].output    
      link.addPoint(link.OriginBlock.X+(link.OriginBlock.Width/2)+diagram.dpblocks[origin_index].odescriptor[j].Position, diagram.links_row[origin_row].y+diagramstyle.link_pointdepth*diagram.points[origin_index].output_input[j].point, 0)
      if (origin_row+1 == destination_row) then
        link.addPoint(link.DestinationBlock.X+(link.DestinationBlock.Width/2)+diagram.dpblocks[origin_index].ddescriptor[j].Position, diagram.links_row[origin_row].y+diagramstyle.link_pointdepth*diagram.points[origin_index].output_input[j].point, 1)
      else
        link.addPoint(diagram.links_column[diagram.dpblocks[destination_index].column].x+diagramstyle.link_pointdepth*diagram.points[origin_index].column[j].point, diagram.links_row[origin_row].y+diagramstyle.link_pointdepth*diagram.points[origin_index].output_input[j].point, 1)
        link.addPoint(diagram.links_column[diagram.dpblocks[destination_index].column].x+diagramstyle.link_pointdepth*diagram.points[origin_index].column[j].point, diagram.links_row[destination_row-1].y+diagramstyle.link_pointdepth*diagram.points[destination_index].output_input[input_index].point, 2)
        link.addPoint(link.DestinationBlock.X+(link.DestinationBlock.Width/2)+diagram.dpblocks[origin_index].ddescriptor[j].Position, diagram.links_row[destination_row-1].y+diagramstyle.link_pointdepth*diagram.points[destination_index].output_input[input_index].point, 3)
      end
    end
  end
end

local function centerDiagramBlock(diagram, dblock)
  diagram.dblocks[dblock].x=(diagram.form.width/2)-(diagram.dblocks[dblock].width/2)
  diagram.dblocks[dblock].y=(diagram.form.height/2)-(diagram.dblocks[dblock].height/2)
end

local function moveEverything(diagram, offset)
  for i=1,#diagram.dblocks do
    diagram.dblocks[i].x=diagram.dblocks[i].x+offset
  end
  for i=0, diagram.diagram.LinkCount-1 do
    local link = diagram.diagram.Link[i]
    if link.Points ~= nil then
      for j=0, link.PointCount do
        if link.Points[j] ~= nil then
          local point = {}
          point.x=link.Points[j].x+offset
          point.y=link.Points[j].y
          link.Points[j]=point
        end
      end
    end
  end
end

local function createDiagramInfoBlock(diagram)
  local dinfoblock = createDiagramHeaderlessBlock(diagram)
  dinfoblock.Strings.add(" " .. string.char(27) .. string.format("[1m"..translate("[Diagram info]"), diagram.blocks[1].start) .. string.char(27) .."[0m")
  dinfoblock.Strings.add(string.format(translate(" Function start: 0x%X"), diagram.blocks[1].start))
  dinfoblock.Strings.add(string.format(translate(" Function stop: 0x%X"), diagram.blocks[#diagram.blocks].stop))
  dinfoblock.Strings.add(string.format(translate(" Diagram blocks count: %d"), #diagram.dblocks))
  dinfoblock.Strings.add(string.format(translate(" Diagram links count: %d"), diagram.diagram.LinkCount))
  dinfoblock.AutoSize=true
  dinfoblock.x=0
  dinfoblock.y=0
  if #diagram.blocks > 1 then moveEverything(diagram, dinfoblock.width + diagramstyle.link_pointdepth) end
end

local function spawnDiagram(start, limit)
  local diagram = {}
  diagram.state=pseudocode.parseFunction(start, limit)
  diagram.blocks,diagram.sortedAddressList=pseudocode.createBlocks(diagram.state)
  createDiagramForm(diagram, translate('Diagram'))
  createMenu(diagram)   
  createDiagramDiagram(diagram)
  createDiagramPopupMenu(diagram)
  createDiagramBlocks(diagram)
  if #diagram.dblocks > 1 then
    linkDiagramBlocks(diagram)
    createDiagramPseudoBlocks(diagram)
    ComputeBetterArrangement (diagram)
    computeLayers(diagram, 1)
    initLayerRelatedStuff(diagram)
    computePoints(diagram)
    arrangeDiagramLayers(diagram)
    arrangeDiagramBlocks(diagram)    
    arrangeDiagramLinks(diagram)
  elseif #diagram.dblocks > 0 then centerDiagramBlock(diagram, 1) end
  createDiagramInfoBlock(diagram)
  diagram.form.Visible=true
  diagram.diagram.repaint()
  --focus on first block
  if #diagram.dblocks > 0 then scrollToDiagramBlock(diagram, diagram.dblocks[1]) end
  return diagram
end

local function MenuSpawnDiagram()
  local mv=getMemoryViewForm()
  local a=mv.DisassemblerView.SelectedAddress
  local b=mv.DisassemblerView.SelectedAddress2 or a
  a=math.min(a,b);
  spawnDiagram(a,100000)
end

local mv=getMemoryViewForm()
local mi=createMenuItem(mv.Menu)
mi.Caption=translate('Spawn diagram')
mi.Shortcut='Ctrl+Shift+D'
mi.ImageIndex=33
mi.OnClick=MenuSpawnDiagram
mv.debuggerpopup.Items.insert(mv.MenuItem2.MenuIndex+1, mi)


registerFormAddNotification(function(f)
  --watch for tracerforms and add a 'Spawn Diagram' option there as well
  if f.ClassName=='TfrmTracer' then
    local callback=123
    callback=f.registerFirstShowCallback(function(f)
      --print("calling unregisterFirstShowCallback using callback. ClassName="..callback.ClassName)
      f.unregisterFirstShowCallback(callback) --cleans up memory

      --add a spawn diagram option
      local mi=createMenuItem(f.pmTracer)
      mi.Caption=translate('Spawn diagram')    
      mi.Shortcut='Ctrl+Shift+D'
      mi.ImageIndex=4
      mi.OnClick=function(s)
        local entrynr
        local i
        
        --print("Spawn Diagram menuitem")
        --print("f classname="..f.ClassName)
        --print("f.Count="..f.Count)
        
        if f.Count>0 then
          local entrynr
        
          if f.lvTracer.Selected then 
            --find the first selected entry
            for i=0,f.Count-1 do
              --print("Getting entry "..i)
              local e=f.Entry[i]
              if e then
                if entrynr==nil then --get first valid entry
                  entrynr=i
                end
                
                if e.selected then --selections take precedence               
                  entrynr=i        
                  break            
                end
              end
            end       
          end
          
          if entrynr then          
            --print("calling spawnDiagram with entrynr "..entrynr..' which has address '..string.format('%x',f.Entry[entrynr].address ))
            spawnDiagram(f.Entry[entrynr].address,100000)   
          else
            return
          end
        end        
      end
      
      f.pmTracer.Items.add(mi)
    
    end)
    
    --print("Registered FirstShowCallback c.classname="..callback.ClassName)
    --callback.OnDestroy=function(sender) print("I got destroyed") end
  end
end)


--[[
diagram structure:
diagram = {}
diagram.form
diagram.diagram
diagram.popup = {}
diagram.blocks = {}
diagram.sortedAddressList = {} --array sorted from 1..#sortedAddressList containing addresses
diagram.dblocks = {}
diagram.dpblocks = {}
diagram.points = {}
diagram.column_count
diagram.row_count
diagram.column = {}
diagram.row = {}
diagram.paths = {}
diagram.links_column = {}
diagram.links_row = {}
diagram.column_links_count = {}
diagram.row_max_depth = {}
]]

--_G.diagramstyle.diagram_backgroundcolor = 0xFFFFFF (edit style from the outside example)

--[[todolist]]
--have a rightclick on an address function, then find the start of the function and then parse and display the diagram