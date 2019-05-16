--[[pseudocodediagram.lua]]--
local DPIAdjust=getScreenDPI()/96

local diagramstyle = {}

diagramstyle.instruction_registerstyle = '[31;1m' --red + bold
diagramstyle.instruction_hexstyle = '[34;1m' --blue + bold
diagramstyle.instruction_symbolstyle = '[32;1m' --green + bold
diagramstyle.instruction_opcodestyle = '[1m' --bold


diagramstyle.link_defaultcolor = 0x00FF00FF 
diagramstyle.link_nottakencolor = 0x000000FF --red
diagramstyle.link_takencolor = 0x00FF0000 --blue
diagramstyle.link_linethickness = 3*DPIAdjust
diagramstyle.link_arrowsize = math.ceil(5*DPIAdjust)
diagramstyle.link_pointdepth = {}
diagramstyle.link_pointdepth[true] = 50*DPIAdjust --taken
diagramstyle.link_pointdepth[false] = 60*DPIAdjust --not taken
diagramstyle.link_pointdepth['backward'] = 40*DPIAdjust --backward

diagramstyle.block_headershowsymbol = true
diagramstyle.block_bodyshowaddresses = false
diagramstyle.block_bodyshowaddressesassymbol = true
diagramstyle.block_bodyshowbytes = false
diagramstyle.block_backgroundcolor = 0x00FFFFFF --white

diagramstyle.diagram_blackgroundcolor = 0x00808080 --grey



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

function blockAddressToBlockIndex(blocks, address)
  for i,block in pairs(blocks) do
    if (block.start == address or block.stop == address) then
      return i
    end
  end
  return nil
end

function diagramBlockToDiagramBlockIndex(dblocks, dblock)
  for i,dblockA in pairs(dblocks) do
    if (dblockA == dblock) then
      return i
    end
  end
  return nil
end

function diagramLayerBlockToDiagramLayer(dlayers, dlblock)
  for i=1, #dlayers.layer do
    for j=1, #dlayers.layer[i] do
      if (dlayers.layer[i][j] == dlblock) then
        return i
      end
    end
  end
  return nil
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

function createDiagramForm(name)
  local form = createForm()
  --form.AutoSize=true
  form.BorderStyle='bsSizeable'
  form.Caption=name
  form.width=getScreenWidth() - (getScreenWidth() / 6)  --1000
  form.height=getScreenHeight() - (getScreenHeight() / 6) --800
  return form
end

function createDiagramDiagram(form)
  local diagram = createDiagram(form)
  diagram.Align='alClient'
  diagram.ArrowStyles='[asDestination,asPoints,asCenter]'
  diagram.BackgroundColor=diagramstyle.diagram_blackgroundcolor
  diagram.BlockBackground=diagramstyle.block_backgroundcolor
  diagram.LineThickness=diagramstyle.link_linethickness
  diagram.ArrowSize=diagramstyle.link_arrowsize
  return diagram
end

function createDiagramBlock(diagram, name)
  local diagramblock = diagram.createBlock()
  diagramblock.Caption=name
  return diagramblock
end

function createDiagramLink(diagram, sourceblock, destinationblock, color,offset)
  --local diagramlink = diagram.addConnection(sourceblock, destinationblock)
  local sourceBSD={}
  sourceBSD.Block=sourceblock
  sourceBSD.Side=dbsBottom
  sourceBSD.Position=offset 
  
  local destinationBSD={}
  destinationBSD.Block=destinationblock
  destinationBSD.Side=dbsTop
  destinationBSD.Position=0
  
  local diagramlink = diagram.addConnection(sourceBSD, destinationBSD)
  
  diagramlink.LineColor=color
  return diagramlink
end

function createDiagramBlocks(diagram, state, blocks)
  local dblocks = {}
  for i,block in pairs(blocks) do
    if state.parsed[block.start] then
      --create block
      if (diagramstyle.block_headershowsymbol and inModule(block.start)) then
        dblocks[i] = createDiagramBlock(diagram, ' ' .. string.char(27) .. diagramstyle.instruction_symbolstyle .. 
                                                        getNameFromAddress(block.start))
      else
        dblocks[i] = createDiagramBlock(diagram, ' ' .. string.format('%X', block.start))
      end
      --fill block
      local current = block.start
      while (current <= block.stop) do
        dblocks[i].Strings.add(disassembleDecoratedInstruction(current))
        if state.parsed[current] ~= nil and state.parsed[current].bytesize ~= 0 then current = current + state.parsed[current].bytesize
        else break end  
      end
      dblocks[i].AutoSize = true
    end
  end
  
  --_G.blocks=dblocks
  
  return dblocks
  
end

function linkDiagramBlocks(diagram, state, dblocks, blocks)
  local istaken = {}
  for i,diagramblock in pairs(dblocks) do
    if (i > 1) then --skip starting block
      for j,source in pairs(blocks[i].getsJumpedToBy) do
        if (source == blocks[i-1].stop) then
          local link=createDiagramLink(diagram, dblocks[i-1], diagramblock, diagramstyle.link_nottakencolor,10*DPIAdjust) --not taken branches
          local linkdata={}
          linkdata.isTaken=true          
          link.Tag=createRef(linkdata)
          
          
          istaken[i] = false
        end
      end
    end
    if (blocks[i].jumpsTo) then --skip leaf blocks
      local destinationblock_index = blockAddressToBlockIndex(blocks, blocks[i].jumpsTo.destinationtaken)
      

      
      if (destinationblock_index) then
        local linkdata={}
        local color=diagramstyle.link_takencolor
        local offset=-10*DPIAdjust
        
        if blocks[i].jumpsTo.destinationnottaken==nil then
          linkdata.unconditional=true  --also true for logicalFollow, but those where logicalFollow is false are jmp's
          color=diagramstyle.link_defaultcolor
          offset=0          
        end
      
      
        
        if blocks[i].jumpsTo.logicalFollow then          
          linkdata.logicalFollow=true
          color=diagramstyle.link_defaultcolor
          offset=0
        end        
          
          
        local link=createDiagramLink(diagram, diagramblock, dblocks[destinationblock_index], color,offset) --taken branches
        
        linkdata.isTaken=false
        link.Tag=createRef(linkdata)
        
        istaken[destinationblock_index] = true
      end
    end
  end
  return istaken
end

function fetchLinks(dblocks)
  local dlinks, k = {}, 1
  for i=1, #dblocks do
    local links = dblocks[i].getLinks()
    for j=1, #links.asDestination do
      dlinks[k] = links.asDestination[j]
      k = k + 1
    end
  end
  return dlinks
end

function generateLayers(dblocks)
  local dlayers = {}
  dlayers.layer = {}
  dlayers.height = {}

  dlayers.layer[1] = {}
  dlayers.layer[1][1] = dblocks[1] --layer 1 = starting block
  for i=2, #dblocks do --create subsequent layers
    local links = dblocks[i].getLinks()
    --get the previous layer index (new layer = previous + 1)
    local index = diagramLayerBlockToDiagramLayer(dlayers, links.asDestination[1].OriginBlock) 
    k = 1
    if (dlayers.layer[index + 1] ~= nil) then --first block in the current layer?
      while dlayers.layer[index + 1][k] ~= nil do k = k + 1 end 
      dlayers.layer[index + 1][k] = dblocks[i]
    else
      dlayers.layer[index + 1] = {}
      dlayers.layer[index + 1][k] = dblocks[i]
    end
  end
  local max
  for i=1, #dlayers.layer do --get layers height
    max = dlayers.layer[i][1].height
    for j=2, #dlayers.layer[i] do
      max = math.max(max, dlayers.layer[i][j].height)
    end
    dlayers.height[i] = max
  end
  return dlayers
end

function adjustLayerBlocks(dlayer, newdblock, overlapdblock)
  --to fix/finish
  local leftblocks = {}
  local rightblocks = {}
  local r = 1
  local l = 1

  --move the layer blocks at the right/left of the new block based on the conflict
  for i=1, #dlayer do
    if (dlayer[i].x >= newdblock.x) then
      if dlayer[i] ~= newdblock then
        rightblocks[r] = dlayer[i]
        r = r + 1
      end
    elseif (dlayer[i].x < newdblock.x) then
      leftblocks[l] = dlayer[i]
      l = l + 1
    else
      --nothing
    end
  end
  
  if (overlapdblock.x >= newdblock.x) then
    for i=1, #rightblocks do
      rightblocks[i].x =  rightblocks[i].x + newdblock.width
    end
  else
    for i=1, #leftblocks do
      leftblocks[i].x =  leftblocks[i].x - newdblock.width
    end
  end
end

function arrangeDiagramBlocks(dform, dblocks, istaken, dlayers)
  dblocks[1].x = dform.width / 2 - dblocks[1].width / 2
  for i,dblock in pairs(dblocks) do
   
    if (i > 1) then      
      local links = dblock.getLinks()
      if (istaken[i]) then 
        dblock.x = links.asDestination[1].OriginBlock.x - dblock.width - 50*DPIAdjust
      else
        dblock.x = links.asDestination[1].OriginBlock.x + links.asDestination[1].OriginBlock.width + 50*DPIAdjust
      end

      --fetch the previous layer in order to evaluate where the current layer starts
      local previous_layer_index = diagramLayerBlockToDiagramLayer(dlayers, links.asDestination[1].OriginBlock)
      local previous_layer_start = links.asDestination[1].OriginBlock.y --fetch the previous layer start
      local previous_layer_stop = dlayers.height[previous_layer_index] --fetch the previous layer stop
      --previous_layer_start + previous_layer_stop + 100*DPIAdjust leads to the current layer start 
      local current_layer_start = previous_layer_start + previous_layer_stop + 100*DPIAdjust
      dblock.y = current_layer_start --insert the block into the current layer

      for j=1, #dblocks do --check for eventual overlaps
        if (dblock.overlapsWith(dblocks[j])) then
          index = diagramLayerBlockToDiagramLayer(dlayers, dblocks[j])
          adjustLayerBlocks(dlayers.layer[index], dblock, dblocks[j]) --adjust all the blocks of the current layer when inserting a new one (in case of overlap)
        end
      end
      
      if dblock.x<0 then --too far too the left, move everything
        local j
        local offset=-dblock.x
        for j=1,i do
          dblocks[j].x=dblocks[j].x+offset
        end
      end
    end
  end
end

function arrangeDiagramLinks(dblocks, istaken)
  local dlinks, k = fetchLinks(dblocks)

  for i,dlink in pairs(dlinks) do
    local odesc=dlink.OriginDescriptor
    local ddesc=dlink.DestinationDescriptor

    local index = diagramBlockToDiagramBlockIndex(dblocks, dlink.DestinationBlock)
    
    if (dlink.DestinationBlock.Y > dlink.OriginBlock.Y) then --branching forward
      dlink.addPoint(dlink.OriginBlock.X + (dlink.OriginBlock.Width / 2)+odesc.Position, dlink.OriginBlock.Y + dlink.OriginBlock.Height + diagramstyle.link_pointdepth[istaken[index]], 1)       
      dlink.addPoint(dlink.DestinationBlock.X + (dlink.DestinationBlock.Width / 2), dlink.OriginBlock.Y + dlink.OriginBlock.Height + diagramstyle.link_pointdepth[istaken[index]], 2)
      k = 3
    else --branching backward
      dlink.addPoint(dlink.OriginBlock.X + (dlink.OriginBlock.Width / 2)+odesc.Position, dlink.OriginBlock.Y + dlink.OriginBlock.Height + diagramstyle.link_pointdepth["backward"], 1)
      if (dlink.DestinationBlock.X < dlink.OriginBlock.X) then
        dlink.addPoint(dlink.DestinationBlock.X + dlink.DestinationBlock.Width + 50*DPIAdjust, dlink.OriginBlock.Y + dlink.OriginBlock.Height + diagramstyle.link_pointdepth["backward"], 2)
        dlink.addPoint(dlink.DestinationBlock.X + dlink.DestinationBlock.Width + 50*DPIAdjust, dlink.DestinationBlock.Y - diagramstyle.link_pointdepth["backward"], 3)
      else
        dlink.addPoint(dlink.DestinationBlock.X - 50*DPIAdjust, dlink.OriginBlock.Y + dlink.OriginBlock.Height + diagramstyle.link_pointdepth["backward"], 2)
        dlink.addPoint(dlink.DestinationBlock.X - 50*DPIAdjust, dlink.DestinationBlock.Y - diagramstyle.link_pointdepth["backward"], 3)
      end
      dlink.addPoint(dlink.DestinationBlock.X + (dlink.DestinationBlock.Width / 2), dlink.DestinationBlock.Y - diagramstyle.link_pointdepth["backward"], 4)
      k = 4
    end

    --wrap
  end
end

function spawnDiagram(start, limit)
  local dform = createDiagramForm('Diagram')
  local ddiagram = createDiagramDiagram(dform)
  local state = parseFunction(start, limit)
  local blocks = createBlocks(state)
  local dblocks = createDiagramBlocks(ddiagram, state, blocks)
  local istaken = linkDiagramBlocks(ddiagram, state, dblocks, blocks)
  local dlayers = generateLayers(dblocks)
  maxx,maxy=arrangeDiagramBlocks(dform, dblocks, istaken, dlayers)

  arrangeDiagramLinks(dblocks, istaken)

  ddiagram.repaint()
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
mi.OnClick=MenuSpawnDiagram
mv.debuggerpopup.Items.insert(mv.MenuItem2.MenuIndex+1, mi)

--[[
local new_diagramstyle = {}
new_diagramstyle.block_bodyshowaddresses = true
new_diagramstyle.block_bodyshowaddressesassymbol = true
new_diagramstyle.block_bodyshowbytes = true
editDiagramStyle(new_diagramstyle)
spawnDiagram(0x100016914, 50)
]]--

  --[[
  print("--debug--")
  for i=1, #dlayers.layer do
    print(string.format("(layer #%d) height: %d", i, dlayers.height[i]))
    print(string.format("(layer #%d) blocks:", i))
    for j=1, #dlayers.layer[i] do
      print(string.format("%s", dlayers.layer[i][j].Caption))
    end
  end
  ]]--

--[[todolist]]
--put incoming lines in the top and outgoing lines in the bottom, add points to lines, etc...
--have a rightclick on an address function, then find the start of the function and then parse and display the diagram
--incorporate frmtracer results in it, or ultimap traces