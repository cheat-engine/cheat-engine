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
diagramstyle.link_pointdepth = 10*DPIAdjust

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
  local diagramform = createForm()
  diagramform.BorderStyle='bsSizeable'
  diagramform.Caption=name
  diagramform.width=getScreenWidth() - (getScreenWidth() / 6)
  diagramform.height=getScreenHeight() - (getScreenHeight() / 6)
  return diagramform
end

function createDiagramDiagram(form)
  local diagramdiagram = createDiagram(form)
  diagramdiagram.Align='alClient'
  diagramdiagram.ArrowStyles='[asDestination,asPoints,asCenter]'
  diagramdiagram.BackgroundColor=diagramstyle.diagram_blackgroundcolor
  diagramdiagram.BlockBackground=diagramstyle.block_backgroundcolor
  diagramdiagram.LineThickness=diagramstyle.link_linethickness
  diagramdiagram.ArrowSize=diagramstyle.link_arrowsize
  return diagramdiagram
end

function createDiagramBlock(diagram, name)
  local diagramblock = diagram.createBlock()
  diagramblock.Caption=name
  return diagramblock
end

function createDiagramLink(diagram, sourceblock, destinationblock, color,offset)
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
  local diagramblocks = {}
  for i,block in pairs(blocks) do
    if state.parsed[block.start] then
      if (diagramstyle.block_headershowsymbol and inModule(block.start)) then
        diagramblocks[i] = createDiagramBlock(diagram, ' ' .. string.char(27) .. diagramstyle.instruction_symbolstyle .. 
                                                        getNameFromAddress(block.start))
      else
        diagramblocks[i] = createDiagramBlock(diagram, ' ' .. string.format('%X', block.start))
      end
      local current = block.start
      while (current <= block.stop) do
        diagramblocks[i].Strings.add(disassembleDecoratedInstruction(current))
        if state.parsed[current] ~= nil and state.parsed[current].bytesize ~= 0 then current = current + state.parsed[current].bytesize
        else break end  
      end
      diagramblocks[i].AutoSize = true
    end
  end
  
  return diagramblocks
end

function blockAddressToBlockIndex(blocks, address)
  for i,block in pairs(blocks) do
    if (block.start == address) or (block.stop == address) then
      return i
    end
  end
  return nil
end

function diagramBlockToDiagramBlockIndex(dblocks, dblock)
  for i=1, #dblocks do
    if dblocks[i] == dblock then
      return i
    end
  end
  return nil
end

function linkDiagramBlocks(diagram, dblocks, blocks)
  for i,diagramblock in pairs(dblocks) do
    if (i > 1) then --skip starting block
      for j,source in pairs(blocks[i].getsJumpedToBy) do
        if (source == blocks[i-1].stop) then
          local link=createDiagramLink(diagram, dblocks[i-1], diagramblock, diagramstyle.link_nottakencolor,10*DPIAdjust) --not taken branches
          local linkdata={}
          linkdata.isTaken=true          
          link.Tag=createRef(linkdata)
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
      end
    end
  end
end

function initDiagramPseudoBlocks(dblocks, dpblocks)
  for i=1, #dblocks do
    local linkz = dblocks[i].getLinks()
    dpblocks[i] = {}
    dpblocks[i].width = dblocks[i].width
    dpblocks[i].height = dblocks[i].height
    dpblocks[i].input_count = #linkz.asDestination
    dpblocks[i].output_count = #linkz.asSource
    dpblocks[i].betteroutput_count = 0
    dpblocks[i].input = {}
    dpblocks[i].betteroutput = {}
    dpblocks[i].v_layer = 0
    dpblocks[i].v_layer_count = 0
    dpblocks[i].layer = 0
    dpblocks[i].layer_count = 0
    for j=1, #linkz.asDestination do 
      dpblocks[i].input[j] = diagramBlockToDiagramBlockIndex(dblocks, linkz.asDestination[j].OriginBlock)
    end
    dpblocks[i].output = {}
    for j=1, #linkz.asSource do 
      dpblocks[i].output[j] = diagramBlockToDiagramBlockIndex(dblocks, linkz.asSource[j].DestinationBlock)
    end
  end
end

function initDiagramVisitedBlocks(dblocks, dvblocks)
  for i=1, #dblocks do
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
  if queue.first > last then return nil end
  local value = queue[last]
  queue[last] = nil
  queue.last = last - 1
  return value
end

function computeBetterEdges(dblocks, dpblocks)
  local dvblocks = {}
  initDiagramVisitedBlocks(dblocks, dvblocks)

  local more = true
  dvblocks[1].visited = true

  branchqueue = createQueue()
  pushLeft(branchqueue, 1) --starting blocks

  while (more) do 
    more = false
    while branchqueue.first <= branchqueue.last do
      local nextbranch = popRight(branchqueue)
      for j=1, dpblocks[nextbranch].output_count do
        if not dvblocks[dpblocks[nextbranch].output[j]].visited then
          if dpblocks[dpblocks[nextbranch].output[j]].input_count == 1 then
            dpblocks[nextbranch].betteroutput[#dpblocks[nextbranch].betteroutput+1] = dpblocks[nextbranch].output[j]
            dpblocks[nextbranch].betteroutput_count = #dpblocks[nextbranch].betteroutput
            dvblocks[dpblocks[nextbranch].output[j]].visited = true
            pushLeft(branchqueue, dpblocks[nextbranch].output[j])
            more = true
          end
          dpblocks[dpblocks[nextbranch].output[j]].input_count = dpblocks[dpblocks[nextbranch].output[j]].input_count - 1
        end
      end
    end

    local nextbranch = {}
    nextbranch.branch_min = nil
    nextbranch.inputs_min = nil
    nextbranch.input_min = nil
    
    for i=1, #dblocks do
      if dvblocks[i].visited then
        for j=1, dpblocks[i].output_count do
          if not dvblocks[dpblocks[i].output[j]].visited then
            if (nextbranch.branch_min == nil) or (dpblocks[dpblocks[i].output[j]].input_count == nextbranch.inputs_min) or 
            ((dpblocks[dpblocks[i].output[j]].input_count == nextbranch.inputs_min) and (dpblocks[i].output[j] < nextbranch.branch_min)) then
              nextbranch.branch_min = dpblocks[i].output[j]
              nextbranch.inputs_min = dpblocks[dpblocks[i].output[j]].input_count
              nextbranch.input_min = i
            end
          end
        end
      end
    end
    if nextbranch.branch_min ~= nil then
      dpblocks[nextbranch.input_min].betteroutput[#dpblocks[nextbranch.input_min].betteroutput+1] = nextbranch.branch_min
      dpblocks[nextbranch.input_min].betteroutput_count = #dpblocks[nextbranch.input_min].betteroutput
      dvblocks[nextbranch.branch_min].visited = true
      pushLeft(branchqueue, nextbranch.branch_min)
      dpblocks[nextbranch.branch_min].input_count = dpblocks[nextbranch.branch_min].input_count - 1
      more = true
    end
  end
end


function printBetterEdges(dblocks, dpblocks, dpblock) --debug purposes
  for i=1, dpblocks[dpblock].betteroutput_count do
    print(string.format("%s --> %s", dblocks[dpblock].caption, dblocks[dpblocks[dpblock].betteroutput[i]].caption))
    printBetterEdges(dblocks, dpblocks, dpblocks[dpblock].betteroutput[i])
  end
end

function adjustEverything(dpblocks, dpblock, v_layer, layer)
  dpblocks[dpblock].v_layer = dpblocks[dpblock].v_layer + v_layer
  dpblocks[dpblock].layer = dpblocks[dpblock].layer + layer
  for i=1, dpblocks[dpblock].betteroutput_count do
    local edge = dpblocks[dpblock].betteroutput[i]
    adjustEverything(dpblocks, edge, v_layer, layer)
  end
end

function computeLayers(dblocks, dpblocks, dpblock)
  local v_layer, layer_count, child_v_layer = 0, 0, 0

  for i=1, dpblocks[dpblock].betteroutput_count do
    local edge = dpblocks[dpblock].betteroutput[i] 
    computeLayers(dblocks, dpblocks, edge)
    if (dpblocks[edge].layer_count+1) > layer_count then
      layer_count = dpblocks[edge].layer_count+1
    end
    child_v_layer = dpblocks[edge].v_layer
  end

  if dpblocks[dpblock].betteroutput_count == 2 then
    local better1 = dpblocks[dpblock].betteroutput[1]
    local better2 = dpblocks[dpblock].betteroutput[2]
    local v_layer_offset = {}

    if (dpblocks[better1].betteroutput_count == 0) then
      dpblocks[better1].v_layer = dpblocks[better2].v_layer - 2
      if dpblocks[better1].v_layer < 0 then v_layer_offset[1] = -dpblocks[better1].v_layer else v_layer_offset[1] = 0 end
      adjustEverything(dpblocks, better1, v_layer_offset[1], 1)
      adjustEverything(dpblocks, better2, v_layer_offset[1], 1)
      v_layer = dpblocks[better2].v_layer_count + v_layer_offset[1]
    elseif (dpblocks[better2].betteroutput_count == 0) then
      adjustEverything(dpblocks, better1, 0, 1)
      adjustEverything(dpblocks, better2, dpblocks[better1].v_layer + 2, 1)
      v_layer = math.max(dpblocks[better1].v_layer_count, dpblocks[better2].v_layer + 2)
    else
      adjustEverything(dpblocks, better1, 0, 1)
      adjustEverything(dpblocks, better2, dpblocks[better1].v_layer_count, 1)
      v_layer = dpblocks[better1].v_layer_count + dpblocks[better1].v_layer_count
    end
    dpblocks[dpblock].v_layer_count = math.max(2, v_layer)
    dpblocks[dpblock].v_layer = math.ceil((dpblocks[better1].v_layer + dpblocks[better2].v_layer) / 2)
  else
    for i=1, dpblocks[dpblock].betteroutput_count do
      local edge = dpblocks[dpblock].betteroutput[i]
      adjustEverything(dpblocks, edge, v_layer, 1)
      v_layer = v_layer + dpblocks[edge].v_layer_count
    end
    if v_layer >= 2 then
      if dpblocks[dpblock].betteroutput_count == 1 then
        dpblocks[dpblock].v_layer = child_v_layer
      else
        dpblocks[dpblock].v_layer = math.ceil((v_layer - 2) / 2)
      end
      dpblocks[dpblock].v_layer_count = v_layer
    else
      dpblocks[dpblock].v_layer = 0
      dpblocks[dpblock].v_layer_count = 2
    end
  end
  dpblocks[dpblock].layer = 0
  dpblocks[dpblock].layer_count = layer_count
end

function arrangeDiagramBlocks(dblocks, dpblocks)
  local v_layer, layer = {}, {}

  for i=1, #dpblocks do
    dpblocks[1].v_layer_count = math.max(dpblocks[i].v_layer, dpblocks[1].v_layer_count)
    dpblocks[1].layer_count = math.max(dpblocks[i].layer, dpblocks[1].layer_count)
  end

  for i=0, dpblocks[1].v_layer_count+1 do
    v_layer[i] = {}
    v_layer[i].width = 0
    v_layer[i].x = 0
  end
  for i=0, dpblocks[1].layer_count+1 do
    layer[i] = {}
    layer[i].height = 0
    layer[i].y = 0
  end

  for i=1, #dpblocks do
    if math.ceil(dpblocks[i].width / 2) > v_layer[dpblocks[i].v_layer].width then 
      v_layer[dpblocks[i].v_layer].width = math.ceil(dpblocks[i].width / 2)
    end
    if math.ceil(dpblocks[i].width / 2) > v_layer[dpblocks[i].v_layer+1].width then 
      v_layer[dpblocks[i].v_layer+1].width = math.ceil(dpblocks[i].width / 2)
    end
    if dpblocks[i].height > layer[dpblocks[i].layer].height then 
      layer[dpblocks[i].layer].height = dpblocks[i].height
    end
  end

  v_layer[0].x = 20*DPIAdjust
  for i=1, dpblocks[1].v_layer_count do
    v_layer[i].x = v_layer[i-1].x + v_layer[i-1].width + 50*DPIAdjust
  end
  layer[0].y = 20*DPIAdjust
  for i=1, dpblocks[1].layer_count do
    layer[i].y = layer[i-1].y + layer[i-1].height + 50*DPIAdjust
  end

  for i=1, #dblocks do
    dblocks[i].x = (v_layer[dpblocks[i].v_layer].x + v_layer[dpblocks[i].v_layer].width + 50*DPIAdjust) - (dblocks[i].width / 2)
    if (dblocks[i].x + dblocks[i].width) > (v_layer[dpblocks[i].v_layer].x + v_layer[dpblocks[i].v_layer].width + v_layer[dpblocks[i].v_layer+1].width + 50*DPIAdjust) then
      dblocks[i].x = (v_layer[dpblocks[i].v_layer].x + v_layer[dpblocks[i].v_layer].width + v_layer[dpblocks[i].v_layer+1].width + 50*DPIAdjust) - (dblocks[i].width)
    end
    dblocks[i].y = layer[dpblocks[i].layer].y
  end

  --to finish
end

function computeOutputPointDepth(dpblocks, origin, destination)
  for i=1, dpblocks[origin].betteroutput_count do
    if dpblocks[origin].output[i] == dpblocks[destination] then
      return diagramstyle.link_pointdepth * i
    end
  end
  return 0
end

function spawnDiagram(start, limit)
  local dform = createDiagramForm('Diagram')
  local ddiagram = createDiagramDiagram(dform)
  local state = parseFunction(start, limit)
  local blocks = createBlocks(state)
  local dblocks = createDiagramBlocks(ddiagram, state, blocks)
  linkDiagramBlocks(ddiagram, dblocks, blocks)
  local dpblocks = {}
  initDiagramPseudoBlocks(dblocks, dpblocks)
  computeBetterEdges (dblocks, dpblocks)
  --printBetterEdges(dblocks, dpblocks, 1) --debug
  computeLayers(dblocks, dpblocks, 1)

  for i=1, #dpblocks do
    dblocks[i].caption = string.format("v_layer:%d--v_layer_count:%d--layer:%d--layer_count:%d",dpblocks[i].v_layer, dpblocks[i].v_layer_count, dpblocks[i].layer, dpblocks[i].layer_count)
    dblocks[i].caption = string.format("v_layer:%d--v_layer_count:%d--layer:%d--layer_count:%d",dpblocks[i].v_layer, dpblocks[i].v_layer_count, dpblocks[i].layer, dpblocks[i].layer_count)
  end

  arrangeDiagramBlocks(dblocks, dpblocks)
  --arrangeDiagramLinks(ddiagram, dblocks, istaken, dlayers)
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

    --[[
  for i=1, #dpblocks do
    dblocks[i].caption = string.format("v_layer:%d--v_layer_count:%d--layer:%d--layer_count:%d",dpblocks[i].v_layer, dpblocks[i].v_layer_count, dpblocks[i].layer, dpblocks[i].layer_count)
    dblocks[i].caption = string.format("v_layer:%d--v_layer_count:%d--layer:%d--layer_count:%d",dpblocks[i].v_layer, dpblocks[i].v_layer_count, dpblocks[i].layer, dpblocks[i].layer_count)
  end
  ]]

--[[todolist]]
--have a rightclick on an address function, then find the start of the function and then parse and display the diagram
--incorporate frmtracer results in it, or ultimap traces