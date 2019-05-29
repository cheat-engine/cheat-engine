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
diagramstyle.link_pointdepth = 20*DPIAdjust --distance between links

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

function createDiagramForm(name)
  local diagramform = createForm()
  diagramform.BorderStyle='bsSizeable'
  diagramform.Caption=name
  diagramform.Width=getScreenWidth() - (getScreenWidth() / 6)
  diagramform.Height=getScreenHeight() - (getScreenHeight() / 6)
  diagramform.Visible=false
  return diagramform
end

function createDiagramDiagram(form)
  local diagramdiagram = createDiagram(form)
  diagramdiagram.Align='alClient'
  diagramdiagram.ArrowStyles='[asDestination,asOrigin]'
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

function createDiagramLink(diagram, sourceblock, destinationblock, color, offset)
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

function diagramBlockInputToInputIndex(dblock, idblock)
  local linkz = dblock.getLinks()
  for i=1, #linkz.asDestination do
    if (linkz.asDestination[i].OriginBlock == idblock) then 
      return i 
    end
  end
  return 0
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
              
        local link=createDiagramLink(diagram, diagramblock, dblocks[destinationblock_index], color, offset) --taken branches
        
        linkdata.isTaken=false
        link.Tag=createRef(linkdata)
      end
    end
  end
end

function createDiagramPseudoBlocks(dblocks)
  local dpblocks = {}
  for i=1, #dblocks do
    local linkz = dblocks[i].getLinks()
    dpblocks.v_layer_count = 0
    dpblocks.layer_count = 0
    dpblocks[i] = {}
    dpblocks[i].input_count_extra = #linkz.asDestination
    dpblocks[i].input_count = #linkz.asDestination
    dpblocks[i].input = {}
    dpblocks[i].output_count = #linkz.asSource
    dpblocks[i].output = {}
    dpblocks[i].betteroutput_count = 0
    dpblocks[i].betteroutput = {}
    dpblocks[i].v_layer_count = 0
    dpblocks[i].v_layer = 0
    dpblocks[i].layer_count = 0
    dpblocks[i].layer = 0
    dpblocks[i].odescriptor = {}
    dpblocks[i].ddescriptor = {}
    dpblocks[i].link = {}
    for j=1, #linkz.asDestination do 
      dpblocks[i].input[j] = diagramBlockToDiagramBlockIndex(dblocks, linkz.asDestination[j].OriginBlock)
    end
    for j=1, #linkz.asSource do 
      dpblocks[i].output[j] = diagramBlockToDiagramBlockIndex(dblocks, linkz.asSource[j].DestinationBlock)
      dpblocks[i].odescriptor[j] = linkz.asSource[j].OriginDescriptor
      dpblocks[i].ddescriptor[j] = linkz.asSource[j].DestinationDescriptor
      dpblocks[i].link[j] = linkz.asSource[j]
    end
  end
  return dpblocks
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
  if queue.first > last then 
    return nil 
  end
  local value = queue[last]
  queue[last] = nil
  queue.last = last - 1
  return value
end

function computeBetterEdges(dblocks, dpblocks)
  local dvblocks = {}
  local more = true
  local branchqueue = createQueue()
  initDiagramVisitedBlocks(dblocks, dvblocks)
  dvblocks[1].visited = true
  pushLeft(branchqueue, 1) --starting block

  while (more) do 
    more = false
    while branchqueue.first <= branchqueue.last do
      local nextbranch = popRight(branchqueue)
      for j=1, dpblocks[nextbranch].output_count do
        if not dvblocks[dpblocks[nextbranch].output[j]].visited then
          if dpblocks[dpblocks[nextbranch].output[j]].input_count_extra == 1 then
            dpblocks[nextbranch].betteroutput[#dpblocks[nextbranch].betteroutput+1] = dpblocks[nextbranch].output[j]
            dpblocks[nextbranch].betteroutput_count = #dpblocks[nextbranch].betteroutput
            dvblocks[dpblocks[nextbranch].output[j]].visited = true
            pushLeft(branchqueue, dpblocks[nextbranch].output[j])
            more = true
          end
          dpblocks[dpblocks[nextbranch].output[j]].input_count_extra = dpblocks[dpblocks[nextbranch].output[j]].input_count_extra - 1
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
            if (nextbranch.branch_min == nil) or (dpblocks[dpblocks[i].output[j]].input_count_extra == nextbranch.inputs_min) or 
            ((dpblocks[dpblocks[i].output[j]].input_count_extra == nextbranch.inputs_min) and (dpblocks[i].output[j] < nextbranch.branch_min)) then
              nextbranch.branch_min = dpblocks[i].output[j]
              nextbranch.inputs_min = dpblocks[dpblocks[i].output[j]].input_count_extra
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
      dpblocks[nextbranch.branch_min].input_count_extra = dpblocks[nextbranch.branch_min].input_count_extra - 1
      more = true
    end
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
    local offset

    if (dpblocks[better1].betteroutput_count == 0) then
      dpblocks[better1].v_layer = dpblocks[better2].v_layer - 2
      if dpblocks[better1].v_layer < 0 then offset = -dpblocks[better1].v_layer else offset = 0 end
      adjustEverything(dpblocks, better1, offset, 1)
      adjustEverything(dpblocks, better2, offset, 1)
      v_layer = dpblocks[better2].v_layer_count + offset
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
    dpblocks[dpblock].v_layer = math.floor((dpblocks[better1].v_layer + dpblocks[better2].v_layer) / 2)
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
        dpblocks[dpblock].v_layer = math.floor((v_layer - 2) / 2)
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

function initPoints(dpblocks, points)
  for i=1, #dpblocks do
    points[i] = {}
    points[i].output_input = {}
    points[i].v_layer = {}
    for j=1, #dpblocks[i].output do
      points[i].output_input[j] = {}
      points[i].output_input[j].point = 0
      points[i].v_layer[j] = {}
      points[i].v_layer[j].point = 0
    end
    for j=#dpblocks[i].output+1, #dpblocks[i].output+#dpblocks[i].input do
      points[i].output_input[j] = {}
      points[i].output_input[j].point = 0
    end
  end
end

function initLinkCounts(dpblocks, h_links_count, v_links_count)
  for i=0, dpblocks.layer_count do
    v_links_count[i] = {}
    v_links_count[i].link = {}
  end
  for i=0, dpblocks.v_layer_count do
    h_links_count[i] = {}
    h_links_count[i].link = {}
  end
end

function computePoints(dblocks, dpblocks)
  local v_links_count, h_links_count, points = {}, {}, {}
  initLinkCounts(dpblocks, h_links_count, v_links_count)
  initPoints(dpblocks, points)
  --output vertical points
  for i=1, #dpblocks do
    local origin = i
    for j=1, dpblocks[i].output_count do
      v_links_count[dpblocks[origin].layer].link[#v_links_count[dpblocks[origin].layer].link+1] = i
      points[i].output_input[j].point = #v_links_count[dpblocks[origin].layer].link
    end
  end
  --input vertical points
  for i=1, #dpblocks do
    local destination = i
    for j=1, dpblocks[i].input_count do
      v_links_count[dpblocks[destination].layer-1].link[#v_links_count[dpblocks[destination].layer-1].link+1] = i
      points[i].output_input[j+#dpblocks[i].output].point = #v_links_count[dpblocks[destination].layer-1].link
    end
  end
  --remaining horizontal points
  for i=1, #dpblocks do
    for j=1, dpblocks[i].output_count do
      local destination = dpblocks[i].output[j]
      h_links_count[dpblocks[destination].v_layer].link[#h_links_count[dpblocks[destination].v_layer].link+1] = i
      points[i].v_layer[j].point = #h_links_count[dpblocks[destination].v_layer].link
    end
  end
  return v_links_count, h_links_count, points
end

function getLayerCounts(dpblocks)
  for i=1, #dpblocks do
    dpblocks.v_layer_count = math.max(dpblocks[i].v_layer, dpblocks.v_layer_count)
    dpblocks.layer_count = math.max(dpblocks[i].layer, dpblocks.layer_count)
  end
end

function initHorizontalAndVerticalLayers(dpblocks, v_layer, layer, links_v_layer, links_layer)
  for i=0, dpblocks.v_layer_count do
    v_layer[i] = {}
    links_v_layer[i] = {}
    v_layer[i].width = 0
    v_layer[i].x = 0
    links_v_layer.x = 0
  end
  for i=0, dpblocks.layer_count do
    layer[i] = {}
    links_layer[i] = {}
    layer[i].height = 0
    layer[i].y = 0
    links_layer.y = 0
  end
end

function arrangeDiagramLayers(dblocks, dpblocks, v_links_count, h_links_count)
  local v_layer, layer, links_v_layer, links_layer = {}, {}, {}, {}
  initHorizontalAndVerticalLayers(dpblocks, v_layer, layer, links_v_layer, links_layer)
  for i=1, #dpblocks do
    v_layer[dpblocks[i].v_layer].width = math.max(dblocks[i].width, v_layer[dpblocks[i].v_layer].width)
    layer[dpblocks[i].layer].height = math.max(dblocks[i].height, layer[dpblocks[i].layer].height)
  end
  local x = 20*DPIAdjust
  for i=0, dpblocks.v_layer_count-1 do
    v_layer[i].x = x
    x = x + v_layer[i].width
    links_v_layer[i].x = x
    x = x + (diagramstyle.link_pointdepth * (#h_links_count[i].link)) + diagramstyle.link_pointdepth
  end
  local y = 20*DPIAdjust
  for i=0, dpblocks.layer_count-1 do
    layer[i].y = y 
    y = y + layer[i].height
    links_layer[i].y = y
    y = y + (diagramstyle.link_pointdepth * (#v_links_count[i].link)) + diagramstyle.link_pointdepth
  end
  v_layer[dpblocks.v_layer_count].x = x
  layer[dpblocks.layer_count].y = y
  links_v_layer[dpblocks.v_layer_count].x = x + v_layer[dpblocks.v_layer_count].width
  links_layer[dpblocks.layer_count].y = y + layer[dpblocks.layer_count].height
  return v_layer, layer, links_v_layer, links_layer
end

function arrangeDiagramBlocks(dblocks, dpblocks, v_layer, layer)
  for i=1, #dblocks do
    dblocks[i].x = v_layer[dpblocks[i].v_layer].x
    dblocks[i].y = layer[dpblocks[i].layer].y
  end
end

function arrangeDiagramLinks(dblocks, dpblocks, links_v_layer, links_layer, points)
  for i=1, #dblocks do
    for j=1, dpblocks[i].output_count do
      local origin_layer=dpblocks[i].layer
      local destination_layer=dpblocks[dpblocks[i].output[j]].layer
      local link=dpblocks[i].link[j]
      local origin_index = i
      local destination_index = dpblocks[i].output[j]
      local input_index = diagramBlockInputToInputIndex(link.DestinationBlock, link.OriginBlock) + #dpblocks[destination_index].output
      
      link.addPoint(link.OriginBlock.X + (link.OriginBlock.Width / 2) + dpblocks[origin_index].odescriptor[j].Position, links_layer[origin_layer].y + diagramstyle.link_pointdepth * points[origin_index].output_input[j].point, 0)

      if (origin_layer + 1 == destination_layer) then
        link.addPoint(link.DestinationBlock.X + (link.DestinationBlock.Width / 2), links_layer[origin_layer].y + diagramstyle.link_pointdepth * points[origin_index].output_input[j].point, 1)
      else
        link.addPoint(links_v_layer[dpblocks[destination_index].v_layer].x + diagramstyle.link_pointdepth * points[origin_index].v_layer[j].point, links_layer[origin_layer].y + diagramstyle.link_pointdepth * points[origin_index].output_input[j].point, 1)
        link.addPoint(links_v_layer[dpblocks[destination_index].v_layer].x + diagramstyle.link_pointdepth * points[origin_index].v_layer[j].point, links_layer[destination_layer-1].y + diagramstyle.link_pointdepth * points[destination_index].output_input[input_index].point, 2)
        link.addPoint(link.DestinationBlock.X + (link.DestinationBlock.Width / 2), links_layer[destination_layer-1].y + diagramstyle.link_pointdepth * points[destination_index].output_input[input_index].point, 3)
      end
    end
  end
end

function centerDiagramBlock(dform, dblock)
  dblock.x = (dform.width / 2) - (dblock.width / 2)
  dblock.y = (dform.height / 2) - (dblock.height / 2)
end

function spawnDiagram(start, limit)
  local dform = createDiagramForm('Diagram')
  local ddiagram = createDiagramDiagram(dform)
  local state = parseFunction(start, limit)
  local blocks = createBlocks(state)
  local dblocks = createDiagramBlocks(ddiagram, state, blocks)
  if #dblocks > 1 then
    linkDiagramBlocks(ddiagram, dblocks, blocks)
    local dpblocks = createDiagramPseudoBlocks(dblocks)
    computeBetterEdges (dblocks, dpblocks)
    computeLayers(dblocks, dpblocks, 1)
    getLayerCounts(dpblocks)
    local v_links_count, h_links_count, points = computePoints(dblocks, dpblocks)
    local v_layer, layer, links_v_layer, links_layer = arrangeDiagramLayers(dblocks, dpblocks, v_links_count, h_links_count)
    arrangeDiagramBlocks(dblocks, dpblocks, v_layer, layer)
    arrangeDiagramLinks(dblocks, dpblocks, links_v_layer, links_layer, points)
  else
    if #dblocks > 0 then centerDiagramBlock(dform, dblocks[1]) end
  end
  dform.Visible = true
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
mi.ImageIndex=33
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

--[[todolist]]
--have a rightclick on an address function, then find the start of the function and then parse and display the diagram
--incorporate frmtracer results in it, or ultimap traces