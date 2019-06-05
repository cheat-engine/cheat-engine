--[[pseudocodediagram.lua]]--
local DPIAdjust=getScreenDPI()/96

local diagramstyle = {}

diagramstyle.instruction_registerstyle = '[31;1m' --red + bold
diagramstyle.instruction_hexstyle = '[34;1m' --blue + bold
diagramstyle.instruction_symbolstyle = '[32;1m' --green + bold
diagramstyle.instruction_opcodestyle = '[1m' --bold

diagramstyle.link_defaultcolor = 0x00FF00FF --fuchsia
diagramstyle.link_nottakencolor = 0x000000FF --red
diagramstyle.link_takencolor = 0x00FF0000 --blue
diagramstyle.link_linethickness = 3*DPIAdjust
diagramstyle.link_arrowsize = math.ceil(5*DPIAdjust)
diagramstyle.link_pointdepth = 20*DPIAdjust --distance between links

diagramstyle.block_headershowsymbol = true
diagramstyle.block_bodyshowaddresses = false
diagramstyle.block_bodyshowaddressesassymbol = true
diagramstyle.block_bodyshowbytes = false
diagramstyle.block_backgroundcolor = 0x00FFFFFF --white

diagramstyle.diagram_blackgroundcolor = 0x0099FFCC --light green



function editDiagramStyle(new_diagramstyle)
  if (new_diagramstyle) then
    if (new_diagramstyle.instruction_registerstyle ~= nil) then 
      diagramstyle.instruction_registerstyle = new_diagramstyle.instruction_registerstyle end
    if (new_diagramstyle.instruction_hexstyle ~= nil) then 
      diagramstyle.instruction_hexstyle = new_diagramstyle.instruction_hexstyle end
    if (new_diagramstyle.instruction_symbolstyle ~= nil) then 
      diagramstyle.instruction_symbolstyle = new_diagramstyle.instruction_symbolstyle end
    if (new_diagramstyle.instruction_opcodestyle ~= nil) then 
      diagramstyle.instruction_opcodestyle = new_diagramstyle.instruction_opcodestyle end

    if (new_diagramstyle.link_defaultcolor ~= nil) then
      diagramstyle.link_defaultcolor = new_diagramstyle.link_defaultcolor end
    if (new_diagramstyle.link_nottakencolor ~= nil) then
      diagramstyle.link_nottakencolor = new_diagramstyle.link_nottakencolor end
    if (new_diagramstyle.link_takencolor ~= nil) then
      diagramstyle.link_takencolor = new_diagramstyle.link_takencolor end
    if (new_diagramstyle.link_linethickness ~= nil) then
      diagramstyle.link_linethickness = new_diagramstyle.link_linethickness end
    if (new_diagramstyle.link_arrowsize ~= nil) then
      diagramstyle.link_arrowsize = new_diagramstyle.link_arrowsize end
    if (new_diagramstyle.link_pointdepth ~= nil) then
      diagramstyle.link_pointdepth = new_diagramstyle.link_pointdepth end


    if (new_diagramstyle.block_headershowsymbol ~= nil) then
      diagramstyle.block_headershowsymbol = new_diagramstyle.block_headershowsymbol end
    if (new_diagramstyle.block_bodyshowaddresses ~= nil) then
      diagramstyle.block_bodyshowaddresses = new_diagramstyle.block_bodyshowaddresses end
    if (new_diagramstyle.block_backgroundcolor ~= nil) then
      diagramstyle.block_backgroundcolor = new_diagramstyle.block_backgroundcolor end
    if (new_diagramstyle.block_bodyshowaddressesassymbol ~= nil) then
      diagramstyle.block_bodyshowaddressesassymbol = new_diagramstyle.block_bodyshowaddressesassymbol end
    if (new_diagramstyle.block_bodyshowbytes ~= nil) then
      diagramstyle.block_bodyshowbytes = new_diagramstyle.block_bodyshowbytes end

    if (new_diagramstyle.diagram_blackgroundcolor ~= nil) then
      diagramstyle.diagram_blackgroundcolor = new_diagramstyle.diagram_blackgroundcolor end
  end
end

function disassembleDecoratedInstruction(address)
  local disassembler, result, bytes, temp = getVisibleDisassembler(), ' '
  temp = disassembler.disassemble(address)
  temp, temp, bytes, temp = splitDisassembledString(temp)
  if (diagramstyle.block_bodyshowaddresses and diagramstyle.block_bodyshowaddressesassymbol and inModule(address)) then result = result .. 
                                                          string.char(27) .. diagramstyle.instruction_symbolstyle ..
                                                          getNameFromAddress(disassembler.LastDisassembleData.address) .. 
                                                          string.char(27) .. '[0m' ..  ' - '

  elseif (diagramstyle.block_bodyshowaddresses) then result = result .. 
                                                          string.format('%X', disassembler.LastDisassembleData.address) .. ' - ' end
  if (diagramstyle.block_bodyshowbytes) then result = result .. bytes .. ' - '  end
  result =  result .. string.char(27).. diagramstyle.instruction_opcodestyle .. 
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

function createDiagramForm(diagram, name)
  diagram.form = createForm(false)
  diagram.form.BorderStyle='bsSizeable'
  diagram.form.Caption=name
  diagram.form.Width=getScreenWidth() - (getScreenWidth() / 6)
  diagram.form.Height=getScreenHeight() - (getScreenHeight() / 6)
end

function createMenu(diagram)
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

  local miSave=createMenuItem(mm)
  miSave.Caption=translate('Save to file')
  miSave.Name='miSave'
  miSave.ImageIndex=22
  
  local miSaveAsImage=createMenuItem(mm)
  miSaveAsImage.Caption=translate('Save diagram to image')
  miSaveAsImage.Name='miSaveAsImage'
  
  local miClose=createMenuItem(mm)
  miClose.Caption=translate('Close')
  miClose.OnClick=function(s) diagram.form.close() end
  miClose.Name='miClose'
  
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
  miPaths.AutoCheck=true
  miPaths.Name='miPaths'
  
  local miTracerPaths=createMenuItem(mm)
  miTracerPaths.Caption=translate('Show path from tracer window') --if more than 1 show a list of tracer windows and pick one 
  miTracerPaths.AutoCheck=true    
  miTracerPaths.Name='miTracerPaths'
  
  DisplayMenu.add(miPaths)
  DisplayMenu.add(miTracerPaths)
  
  
  mm.Items.add(FileMenu)
  mm.Items.add(DisplayMenu)
  
  --todo, add code to the menu items
end


function DiagramContextPopup(sender, mousepos)
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

  diagram.popup.lastobject=obj

  return false
end

function scrollToDiagramBlock(diagram, dblock)
  diagram.diagram.ScrollX = dblock.x - math.abs((diagram.form.width / 2) - ((dblock.width) / 2))
  diagram.diagram.ScrollY = dblock.y - math.abs((diagram.form.height / 2) - ((dblock.height) / 2))
end

function PopupMenuLink1Click(sender)
  local diagram=getRef(sender.Owner.Owner.Tag) --the owner of the menuitem is the popupmenu, and the owner of that is the diagram (alternatively, the menuitem tag could be set to the diagram table as well)
  local sourceblock = diagram.popup.lastobject.OriginBlock
  scrollToDiagramBlock(diagram, sourceblock)
end

function PopupMenuLink2Click(sender)
  local diagram=getRef(sender.Owner.Owner.Tag)
  local destinationblock = diagram.popup.lastobject.DestinationBlock
  scrollToDiagramBlock(diagram, destinationblock)
end

function PopupMenuLink3Click(sender)
  local diagram=getRef(sender.Owner.Owner.Tag)
  diagram.popup.lastobject.removeAllPoints()
  diagram.diagram.repaint()
end

function PopupMenuBlock1Click(sender)
  local diagram=getRef(sender.Owner.Owner.Tag)
  newheader = inputQuery("Edit", "new header", diagram.popup.lastobject.caption)
  if newheader ~= nil then diagram.popup.lastobject.caption = newheader end
end

function PopupMenuBlock2Click(sender) --to implement

end

function PopupMenuBlock3Click(sender)
  local diagram=getRef(sender.Owner.Owner.Tag)
  local linkz = diagram.popup.lastobject.getLinks()
  local stringlist = createStringlist()
  for i=1, #linkz.asDestination do
    stringlist.add(getNameFromAddress(getRef(linkz.asDestination[i].OriginBlock.tag)))
  end
  local index = showSelectionList("Source list", "", stringlist)
  if linkz.asDestination[index+1] ~= nil then
    sourceblock = linkz.asDestination[index+1].OriginBlock
    scrollToDiagramBlock(diagram, sourceblock)
  end
end

function PopupMenuBlock4Click(sender)
  local diagram=getRef(sender.Owner.Owner.Tag)
  local linkz = diagram.popup.lastobject.getLinks()
  local stringlist = createStringlist()
  for i=1, #linkz.asSource do
    stringlist.add(getNameFromAddress(getRef(linkz.asSource[i].DestinationBlock.tag)))
  end
  local index = showSelectionList("Destination list", "", stringlist)
  if linkz.asSource[index+1] ~= nil then
    destinationblock = linkz.asSource[index+1].DestinationBlock
    scrollToDiagramBlock(diagram, destinationblock)
  end
end

function createDiagramPopupMenu(diagram)
  local mv=getMemoryViewForm()
  local pm=createPopupMenu(diagram.diagram)
  pm.Images = mv.mvImageList --icons from the memoryviewer's form
  diagram.diagram.PopupMenu=pm
  diagram.diagram.OnContextPopup=DiagramContextPopup
  
  diagram.popup={}
  diagram.popup.Menu=pm
  diagram.popup.LinkItems={}
  diagram.popup.LinkItems[1]=CreateMenuItem(pm)
  diagram.popup.LinkItems[1].Caption=translate('Go to source')
  diagram.popup.LinkItems[1].OnClick=PopupMenuLink1Click

  diagram.popup.LinkItems[2]=CreateMenuItem(pm)
  diagram.popup.LinkItems[2].Caption=translate('Go to destination')
  diagram.popup.LinkItems[2].OnClick=PopupMenuLink2Click
  
  diagram.popup.LinkItems[3]=CreateMenuItem(pm)
  diagram.popup.LinkItems[3].Caption=translate('Remove all points')      
  diagram.popup.LinkItems[3].ImageIndex=32
  diagram.popup.LinkItems[3].OnClick=PopupMenuLink3Click
  
  pm.Items.add(diagram.popup.LinkItems[1])
  pm.Items.add(diagram.popup.LinkItems[2])
  pm.Items.add(diagram.popup.LinkItems[3])
  
  diagram.popup.BlockItems={}
  diagram.popup.BlockItems[1]=CreateMenuItem(pm)
  diagram.popup.BlockItems[1].Caption=translate('Edit block header')
  diagram.popup.BlockItems[1].ImageIndex=6
  diagram.popup.BlockItems[1].OnClick=PopupMenuBlock1Click

  diagram.popup.BlockItems[2]=CreateMenuItem(pm)
  diagram.popup.BlockItems[2].Caption=translate('Edit block color') --to implement
  diagram.popup.BlockItems[2].ImageIndex=6
  --diagram.popup.BlockItems[2].OnClick=PopupMenuBlock2Click  
  
  diagram.popup.BlockItems[3]=CreateMenuItem(pm)
  diagram.popup.BlockItems[3].Caption=translate('List sources')      
  diagram.popup.BlockItems[3].OnClick=PopupMenuBlock3Click  
  
  diagram.popup.BlockItems[4]=CreateMenuItem(pm)
  diagram.popup.BlockItems[4].Caption=translate('List destinations')      
  diagram.popup.BlockItems[4].OnClick=PopupMenuBlock4Click    
  
  pm.Items.add(diagram.popup.BlockItems[1])
  pm.Items.add(diagram.popup.BlockItems[2])
  pm.Items.add(diagram.popup.BlockItems[3])   
  pm.Items.add(diagram.popup.BlockItems[4])

  diagram.popup.HeaderlessBlockItems={}
  diagram.popup.HeaderlessBlockItems[1]=CreateMenuItem(pm)
  diagram.popup.HeaderlessBlockItems[1].Caption=translate('Edit block color') --to implement
  diagram.popup.HeaderlessBlockItems[1].ImageIndex=6
  --diagram.popup.HeaderlessBlockItems[1].OnClick=PopupMenuBlock2Click

  pm.Items.add(diagram.popup.HeaderlessBlockItems[1])
end


function createDiagramDiagram(diagram)
  diagram.diagram = createDiagram(diagram.form)
  diagram.diagram.Align='alClient'
  diagram.diagram.ArrowStyles='[asDestination,asOrigin]'
  diagram.diagram.BackgroundColor=diagramstyle.diagram_blackgroundcolor
  diagram.diagram.BlockBackground=diagramstyle.block_backgroundcolor
  diagram.diagram.LineThickness=diagramstyle.link_linethickness
  diagram.diagram.ArrowSize=diagramstyle.link_arrowsize
  diagram.diagram.Tag=createRef(diagram)
  --diagram.diagram.AllowUserToCreatePlotPoints = false
  --diagram.diagram.AllowUserToMovePlotPoints = false
  --diagram.diagram.AllowUserToChangeAttachPoints = false
end

function onBlockDrag(dblock)
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

function createDiagramBlock(diagram, name)
  local diagramblock = diagram.diagram.createBlock()
  diagramblock.Caption=name
  diagramblock.OnDoubleClickHeader = function()
    local mwform = getMemoryViewForm()
    local dview = mwform.DisassemblerView
    dview.SelectedAddress = getAddressSafe(getRef(diagramblock.Tag))
    mwform.show()
  end
  diagramblock.OnDrag = onBlockDrag
  return diagramblock
end

function createDiagramHeaderlessBlock(diagram)
  local diagramheaderlessblock = diagram.diagram.createBlock()
  diagramheaderlessblock.ShowHeader = false
  diagramheaderlessblock.DragBody = true
  return diagramheaderlessblock
end

function createDiagramLink(diagram, sourceblock, destinationblock, color, offset)
  local sourceBSD={}
  sourceBSD.Block=diagram.dblocks[sourceblock]
  sourceBSD.Side=dbsBottom
  sourceBSD.Position=offset 

  local offset2 = 0
  local linkz = diagram.dblocks[destinationblock].getLinks()
  if #linkz.asDestination > 0 then
    if math.fmod(#linkz.asDestination + 1,2) == 0 then
      offset2 = (diagramstyle.link_pointdepth/2)*(#linkz.asDestination + 1)
    else
      offset2 = -(diagramstyle.link_pointdepth/2)*(#linkz.asDestination)
    end
  end

  if (diagram.dblocks[destinationblock].x + (diagram.dblocks[destinationblock].width / 2) + offset2) >= (diagram.dblocks[destinationblock].x + diagram.dblocks[destinationblock].width) then
    diagram.dblocks[destinationblock].width = diagram.dblocks[destinationblock].width + math.abs((diagram.dblocks[destinationblock].x + diagram.dblocks[destinationblock].width) - (diagram.dblocks[destinationblock].x + (diagram.dblocks[destinationblock].width / 2) + offset2)) + 40*DPIAdjust
  elseif (diagram.dblocks[destinationblock].x + (diagram.dblocks[destinationblock].width / 2) + offset2) <= diagram.dblocks[destinationblock].x then
    diagram.dblocks[destinationblock].width = diagram.dblocks[destinationblock].width + math.abs(diagram.dblocks[destinationblock].x - (diagram.dblocks[destinationblock].x + (diagram.dblocks[destinationblock].width / 2) + offset2)) + 40*DPIAdjust
  end
  
  local destinationBSD={}
  destinationBSD.Block=diagram.dblocks[destinationblock]
  destinationBSD.Side=dbsTop
  destinationBSD.Position=offset2
  
  local diagramlink = diagram.diagram.addConnection(sourceBSD, destinationBSD)
  
  diagramlink.LineColor=color

  return diagramlink
end

function createDiagramBlocks(diagram)
  diagram.dblocks = {}
  for i=1, #diagram.blocks do
    if diagram.state.parsed[diagram.blocks[i].start] then
      if (diagramstyle.block_headershowsymbol and inModule(diagram.blocks[i].start)) then
        diagram.dblocks[i] = createDiagramBlock(diagram, ' ' .. string.char(27) .. diagramstyle.instruction_symbolstyle .. 
                                                        getNameFromAddress(diagram.blocks[i].start))
      else
        diagram.dblocks[i] = createDiagramBlock(diagram, ' ' .. string.format('%X', diagram.blocks[i].start))
      end

      diagram.dblocks[i].Tag = createRef(diagram.blocks[i].start)

      local current = diagram.blocks[i].start
      while (current <= diagram.blocks[i].stop) do
        diagram.dblocks[i].Strings.add(disassembleDecoratedInstruction(current))
        if diagram.state.parsed[current] ~= nil and diagram.state.parsed[current].bytesize ~= 0 then current = current + diagram.state.parsed[current].bytesize
        else break end  
      end
      diagram.dblocks[i].AutoSize = true
    end
  end
end

function blockAddressToBlockIndex(diagram, address)
  for i=1, #diagram.blocks do
    if (diagram.blocks[i].start == address) or (diagram.blocks[i].stop == address) then
      return i
    end
  end
  return nil
end

function diagramBlockToDiagramBlockIndex(diagram, dblock)
  for i=1, #diagram.dblocks do
    if diagram.dblocks[i] == dblock then
      return i
    end
  end
  return nil
end

function diagramBlockInputToInputIndex(dblock, idblock)
  local linkz = dblock.getLinks()
  for i=1, #linkz.asDestination do
    if linkz.asDestination[i].OriginBlock == idblock then 
      return i 
    end
  end
  return 0
end

function linkDiagramBlocks(diagram)
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

function createDiagramPseudoBlocks(diagram)
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
    diagram.dpblocks[i].v_layer_count = 0
    diagram.dpblocks[i].v_layer = 0
    diagram.dpblocks[i].layer_count = 0
    diagram.dpblocks[i].layer = 0
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

function initDiagramVisitedBlocks(diagram, dvblocks)
  for i=1, #diagram.dblocks do
    dvblocks[i] = {}
    dvblocks[i].visited = false
  end
end

function createQueue()
  return {first = 0, last = -1}
end

function pushLeft (queue, value)
  local first = queue.first - 1
  queue.first = first
  queue[first] = value
end

function popRight (queue)
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
function computeBetterEdges(diagram)
  local dvblocks, more, branchqueue = {}, true, createQueue()
  initDiagramVisitedBlocks(diagram, dvblocks)
  dvblocks[1].visited = true
  pushLeft(branchqueue, 1) --starting block

  while (more) do 
    more = false
    while branchqueue.first <= branchqueue.last do
      local nextbranch = popRight(branchqueue)
      for j=1, diagram.dpblocks[nextbranch].output_count do
        if not dvblocks[diagram.dpblocks[nextbranch].output[j]].visited then
          if diagram.dpblocks[diagram.dpblocks[nextbranch].output[j]].input_count_extra == 1 then
            diagram.dpblocks[nextbranch].betteroutput[#diagram.dpblocks[nextbranch].betteroutput+1] = diagram.dpblocks[nextbranch].output[j]
            diagram.dpblocks[nextbranch].betteroutput_count = #diagram.dpblocks[nextbranch].betteroutput
            dvblocks[diagram.dpblocks[nextbranch].output[j]].visited = true
            pushLeft(branchqueue, diagram.dpblocks[nextbranch].output[j])
            more = true
          end
          diagram.dpblocks[diagram.dpblocks[nextbranch].output[j]].input_count_extra = diagram.dpblocks[diagram.dpblocks[nextbranch].output[j]].input_count_extra - 1
        end
      end
    end

    local nextbranch = {}
    nextbranch.branch_min = nil
    nextbranch.inputs_min = nil
    nextbranch.input_min = nil
    
    for i=1, #diagram.dblocks do
      if dvblocks[i].visited then
        for j=1, diagram.dpblocks[i].output_count do
          if not dvblocks[diagram.dpblocks[i].output[j]].visited then
            if (nextbranch.branch_min == nil) or (diagram.dpblocks[diagram.dpblocks[i].output[j]].input_count_extra == nextbranch.inputs_min) or 
            ((diagram.dpblocks[diagram.dpblocks[i].output[j]].input_count_extra == nextbranch.inputs_min) and (diagram.dpblocks[i].output[j] < nextbranch.branch_min)) then
              nextbranch.branch_min = diagram.dpblocks[i].output[j]
              nextbranch.inputs_min = diagram.dpblocks[diagram.dpblocks[i].output[j]].input_count_extra
              nextbranch.input_min = i
            end
          end
        end
      end
    end
    if nextbranch.branch_min ~= nil then
      diagram.dpblocks[nextbranch.input_min].betteroutput[#diagram.dpblocks[nextbranch.input_min].betteroutput+1] = nextbranch.branch_min
      diagram.dpblocks[nextbranch.input_min].betteroutput_count = #diagram.dpblocks[nextbranch.input_min].betteroutput
      dvblocks[nextbranch.branch_min].visited = true
      pushLeft(branchqueue, nextbranch.branch_min)
      diagram.dpblocks[nextbranch.branch_min].input_count_extra = diagram.dpblocks[nextbranch.branch_min].input_count_extra - 1
      more = true
    end
  end
end

function adjustEverything(diagram, dpblock, v_layer, layer)
  diagram.dpblocks[dpblock].v_layer = diagram.dpblocks[dpblock].v_layer + v_layer
  diagram.dpblocks[dpblock].layer = diagram.dpblocks[dpblock].layer + layer
  for i=1, diagram.dpblocks[dpblock].betteroutput_count do
    local edge = diagram.dpblocks[dpblock].betteroutput[i]
    adjustEverything(diagram, edge, v_layer, layer)
  end
end

--[[
  computes the diagram's rows and columns
  we'll need them in order to arrange blocks, links and points
--]]
function computeLayers(diagram, dpblock)
  local v_layer, layer_count, child_v_layer = 0, 0, 0

  for i=1, diagram.dpblocks[dpblock].betteroutput_count do
    local edge = diagram.dpblocks[dpblock].betteroutput[i] 
    computeLayers(diagram, edge)
    if (diagram.dpblocks[edge].layer_count+1) > layer_count then
      layer_count = diagram.dpblocks[edge].layer_count+1
    end
    child_v_layer = diagram.dpblocks[edge].v_layer
  end

  if diagram.dpblocks[dpblock].betteroutput_count == 2 then
    local better1, better2, offset = diagram.dpblocks[dpblock].betteroutput[1], diagram.dpblocks[dpblock].betteroutput[2]

    if (diagram.dpblocks[better1].betteroutput_count == 0) then
      diagram.dpblocks[better1].v_layer = diagram.dpblocks[better2].v_layer - 2
      if diagram.dpblocks[better1].v_layer < 0 then offset = -diagram.dpblocks[better1].v_layer else offset = 0 end
      adjustEverything(diagram, better1, offset, 1)
      adjustEverything(diagram, better2, offset, 1)
      v_layer = diagram.dpblocks[better2].v_layer_count + offset
    elseif (diagram.dpblocks[better2].betteroutput_count == 0) then
      adjustEverything(diagram, better1, 0, 1)
      adjustEverything(diagram, better2, diagram.dpblocks[better1].v_layer + 2, 1)
      v_layer = math.max(diagram.dpblocks[better1].v_layer_count, diagram.dpblocks[better2].v_layer + 2)
    else
      adjustEverything(diagram, better1, 0, 1)
      adjustEverything(diagram, better2, diagram.dpblocks[better1].v_layer_count, 1)
      v_layer = diagram.dpblocks[better1].v_layer_count + diagram.dpblocks[better1].v_layer_count
    end
    diagram.dpblocks[dpblock].v_layer_count = math.max(2, v_layer)
    diagram.dpblocks[dpblock].v_layer = math.floor((diagram.dpblocks[better1].v_layer + diagram.dpblocks[better2].v_layer) / 2)
  else
    for i=1, diagram.dpblocks[dpblock].betteroutput_count do
      local edge = diagram.dpblocks[dpblock].betteroutput[i]
      adjustEverything(diagram, edge, v_layer, 1)
      v_layer = v_layer + diagram.dpblocks[edge].v_layer_count
    end
    if v_layer >= 2 then
      if diagram.dpblocks[dpblock].betteroutput_count == 1 then
        diagram.dpblocks[dpblock].v_layer = child_v_layer
      else
        diagram.dpblocks[dpblock].v_layer = math.floor((v_layer - 2) / 2)
      end
      diagram.dpblocks[dpblock].v_layer_count = v_layer
    else
      diagram.dpblocks[dpblock].v_layer, diagram.dpblocks[dpblock].v_layer_count = 0, 2
    end
  end
  diagram.dpblocks[dpblock].layer, diagram.dpblocks[dpblock].layer_count = 0, layer_count
end

function initPoints(diagram)
  diagram.points = {}
  for i=1, #diagram.dpblocks do
    diagram.points[i] = {}
    diagram.points[i].output_input = {}
    diagram.points[i].v_layer = {}
    for j=1, #diagram.dpblocks[i].output do
      diagram.points[i].output_input[j] = {}
      diagram.points[i].output_input[j].point = 0
      diagram.points[i].v_layer[j] = {}
      diagram.points[i].v_layer[j].point = 0
    end
    for j=#diagram.dpblocks[i].output+1, #diagram.dpblocks[i].output+#diagram.dpblocks[i].input do
      diagram.points[i].output_input[j] = {}
      diagram.points[i].output_input[j].point = 0
    end
  end
end

function initLayerRelatedStuff(diagram)
  diagram.v_layer_count = 0
  diagram.layer_count = 0
  diagram.v_layer = {}
  diagram.v_layer_links = {}
  diagram.layer = {}
  diagram.layer_links = {}
  diagram.v_layer_links_count = {}
  diagram.layer_links_count = {}
  for i=1, #diagram.dpblocks do
    diagram.v_layer_count = math.max(diagram.dpblocks[i].v_layer, diagram.v_layer_count)
    diagram.layer_count = math.max(diagram.dpblocks[i].layer, diagram.layer_count)
  end
  for i=-1, diagram.layer_count do
    diagram.layer[i] = {}
    diagram.layer_links[i] = {}
    diagram.layer[i].height = 0
    diagram.layer[i].y = 0
    diagram.layer_links[i].y = 0
    diagram.v_layer_links_count[i] = {}
    diagram.v_layer_links_count[i].count = 0
  end
  for i=0, diagram.v_layer_count do
    diagram.v_layer[i] = {}
    diagram.v_layer_links[i] = {}
    diagram.v_layer[i].width = 0
    diagram.v_layer[i].x = 0
    diagram.v_layer_links[i].x = 0
    diagram.layer_links_count[i] = {}
    diagram.layer_links_count[i].count = 0
  end
end

function computePoints(diagram)
  --output vertical points
  for i=1, #diagram.dpblocks do
    local origin = i
    for j=1, diagram.dpblocks[i].output_count do
      diagram.v_layer_links_count[diagram.dpblocks[origin].layer].count = diagram.v_layer_links_count[diagram.dpblocks[origin].layer].count + 1
      diagram.points[i].output_input[j].point = diagram.v_layer_links_count[diagram.dpblocks[origin].layer].count
    end
  end
  --input vertical points
  for i=1, #diagram.dpblocks do
    local destination = i
    for j=1, diagram.dpblocks[i].input_count do
      diagram.v_layer_links_count[diagram.dpblocks[destination].layer-1].count = diagram.v_layer_links_count[diagram.dpblocks[destination].layer-1].count + 1
      diagram.points[i].output_input[j+#diagram.dpblocks[i].output].point = diagram.v_layer_links_count[diagram.dpblocks[destination].layer-1].count
    end
  end
  --remaining horizontal points
  for i=1, #diagram.dpblocks do
    for j=1, diagram.dpblocks[i].output_count do
      local destination = diagram.dpblocks[i].output[j]
      diagram.layer_links_count[diagram.dpblocks[destination].v_layer].count = diagram.layer_links_count[diagram.dpblocks[destination].v_layer].count + 1
      diagram.points[i].v_layer[j].point = diagram.layer_links_count[diagram.dpblocks[destination].v_layer].count
    end
  end
end

function arrangeDiagramLayers(diagram)
  for i=1, #diagram.dpblocks do
    diagram.v_layer[diagram.dpblocks[i].v_layer].width = math.max(diagram.dblocks[i].width, diagram.v_layer[diagram.dpblocks[i].v_layer].width)
    diagram.layer[diagram.dpblocks[i].layer].height = math.max(diagram.dblocks[i].height, diagram.layer[diagram.dpblocks[i].layer].height)
  end
  local x = 20*DPIAdjust
  for i=0, diagram.v_layer_count-1 do
    diagram.v_layer[i].x = x
    x = x + diagram.v_layer[i].width
    diagram.v_layer_links[i].x = x
    x = x + (diagramstyle.link_pointdepth * (diagram.layer_links_count[i].count)) + diagramstyle.link_pointdepth
  end
  diagram.layer[-1].y = 0 --extra layer
  diagram.layer[-1].height = diagramstyle.link_pointdepth * (diagram.v_layer_links_count[-1].count) --size = inputs * pointdepth
  local y = diagram.layer[-1].height + diagramstyle.link_pointdepth
  for i=0, diagram.layer_count-1 do
    diagram.layer[i].y = y 
    y = y + diagram.layer[i].height
    diagram.layer_links[i].y = y
    y = y + (diagramstyle.link_pointdepth * (diagram.v_layer_links_count[i].count)) + diagramstyle.link_pointdepth
  end
  diagram.v_layer[diagram.v_layer_count].x = x
  diagram.layer[diagram.layer_count].y = y
  diagram.v_layer_links[diagram.v_layer_count].x = x + diagram.v_layer[diagram.v_layer_count].width
  diagram.layer_links[diagram.layer_count].y = y + diagram.layer[diagram.layer_count].height
end

function arrangeDiagramBlocks(diagram)
  for i=1, #diagram.dblocks do
    diagram.dblocks[i].x = diagram.v_layer[diagram.dpblocks[i].v_layer].x
    diagram.dblocks[i].y = diagram.layer[diagram.dpblocks[i].layer].y
  end
end

function arrangeDiagramLinks(diagram)
  for i=1, #diagram.dblocks do
    for j=1, diagram.dpblocks[i].output_count do
      local origin_layer=diagram.dpblocks[i].layer
      local destination_layer=diagram.dpblocks[diagram.dpblocks[i].output[j]].layer
      local link=diagram.dpblocks[i].link[j]
      local origin_index = i
      local destination_index = diagram.dpblocks[i].output[j]
      local input_index = diagramBlockInputToInputIndex(link.DestinationBlock, link.OriginBlock) + #diagram.dpblocks[destination_index].output
      
      link.addPoint(link.OriginBlock.X + (link.OriginBlock.Width / 2) + diagram.dpblocks[origin_index].odescriptor[j].Position, diagram.layer_links[origin_layer].y + diagramstyle.link_pointdepth * diagram.points[origin_index].output_input[j].point, 0)

      if (origin_layer + 1 == destination_layer) then
        link.addPoint(link.DestinationBlock.X + (link.DestinationBlock.Width / 2) + diagram.dpblocks[origin_index].ddescriptor[j].Position, diagram.layer_links[origin_layer].y + diagramstyle.link_pointdepth * diagram.points[origin_index].output_input[j].point, 1)
      else
        link.addPoint(diagram.v_layer_links[diagram.dpblocks[destination_index].v_layer].x + diagramstyle.link_pointdepth * diagram.points[origin_index].v_layer[j].point, diagram.layer_links[origin_layer].y + diagramstyle.link_pointdepth * diagram.points[origin_index].output_input[j].point, 1)
        link.addPoint(diagram.v_layer_links[diagram.dpblocks[destination_index].v_layer].x + diagramstyle.link_pointdepth * diagram.points[origin_index].v_layer[j].point, diagram.layer_links[destination_layer-1].y + diagramstyle.link_pointdepth * diagram.points[destination_index].output_input[input_index].point, 2)
        link.addPoint(link.DestinationBlock.X + (link.DestinationBlock.Width / 2) + diagram.dpblocks[origin_index].ddescriptor[j].Position, diagram.layer_links[destination_layer-1].y + diagramstyle.link_pointdepth * diagram.points[destination_index].output_input[input_index].point, 3)
      end
    end
  end
end

function centerDiagramBlock(diagram, dblock)
  diagram.dblocks[dblock].x = (diagram.form.width / 2) - (diagram.dblocks[dblock].width / 2)
  diagram.dblocks[dblock].y = (diagram.form.height / 2) - (diagram.dblocks[dblock].height / 2)
end

function moveEverything(diagram, offset)
  for i=1,#diagram.dblocks do
    diagram.dblocks[i].x=diagram.dblocks[i].x+offset
  end
  for i=0, diagram.diagram.LinkCount-1 do
    local link = diagram.diagram.Link[i]
    if link.Points ~= nil then
      for j=0, link.PointCount do
        if link.Points[j] ~= nil then
          local point = {}
          point.x = link.Points[j].x+offset
          point.y = link.Points[j].y
          link.Points[j] = point
        end
      end
    end
  end
end

function createDiagramInfoBlock(diagram)
  local dinfoblock = createDiagramHeaderlessBlock(diagram)
  dinfoblock.Strings.add(" " .. string.char(27) .. string.format("[1m[Diagram info]", diagram.blocks[1].start) .. string.char(27) .."[0m")
  dinfoblock.Strings.add(string.format(" Function start: 0x%X", diagram.blocks[1].start))
  dinfoblock.Strings.add(string.format(" Function stop: 0x%X", diagram.blocks[#diagram.blocks].stop))
  dinfoblock.Strings.add(string.format(" Diagram blocks count: %d", #diagram.dblocks))
  dinfoblock.Strings.add(string.format(" Diagram links count: %d", diagram.diagram.LinkCount))
  dinfoblock.AutoSize = true
  dinfoblock.x=0
  dinfoblock.y=0
  if #diagram.blocks > 1 then moveEverything(diagram, dinfoblock.width + 20*DPIAdjust) end
end

function spawnDiagram(start, limit)
  local diagram = {}
  diagram.state = parseFunction(start, limit)
  diagram.blocks = createBlocks(diagram.state)
  createDiagramForm(diagram, 'Diagram')
  createMenu(diagram)   
  createDiagramDiagram(diagram)
  createDiagramPopupMenu(diagram)
  createDiagramBlocks(diagram)
  if #diagram.dblocks > 1 then
    linkDiagramBlocks(diagram)
    createDiagramPseudoBlocks(diagram)
    computeBetterEdges (diagram)
    computeLayers(diagram, 1)
    initLayerRelatedStuff(diagram)
    initPoints(diagram)
    computePoints(diagram)
    arrangeDiagramLayers(diagram)
    arrangeDiagramBlocks(diagram)
    arrangeDiagramLinks(diagram)
  else
    if #diagram.dblocks > 0 then centerDiagramBlock(diagram, 1) end
  end
  createDiagramInfoBlock(diagram)
  diagram.form.Visible = true
  diagram.diagram.repaint()
  return diagram
end

function MenuSpawnDiagram()
  local mv=getMemoryViewForm()
  local a=mv.DisassemblerView.SelectedAddress
  local b=mv.DisassemblerView.SelectedAddress2 or a
  a=math.min(a,b);
  spawnDiagram(a,100000)
end

local mv=getMemoryViewForm()
local mi=createMenuItem(mv.Menu)
mi.Caption='Spawn diagram'
mi.Shortcut='Ctrl+Shift+D'
mi.ImageIndex=33
mi.OnClick=MenuSpawnDiagram
mv.debuggerpopup.Items.insert(mv.MenuItem2.MenuIndex+1, mi)

--[[
diagram structure:

diagram = {}
diagram.form
diagram.diagram
diagram.popup = {}
diagram.blocks = {}
diagram.dblocks = {}
diagram.dpblocks = {}
diagram.points = {}
diagram.v_layer_count
diagram.layer_count
diagram.v_layer = {}
diagram.layer = {}
diagram.v_layer_links = {}
diagram.layer_links = {}
diagram.v_layer_links_count = {}
diagram.layer_links_count = {}
]]

--[[
local new_diagramstyle = {}
new_diagramstyle.block_bodyshowaddresses = true
new_diagramstyle.block_bodyshowaddressesassymbol = true
new_diagramstyle.block_bodyshowbytes = true
editDiagramStyle(new_diagramstyle)
spawnDiagram(0x100016914, 50)
]]--

--[[todolist]]
--have a rightclick on an address function, then find the start of the function and then parse and display the diagram