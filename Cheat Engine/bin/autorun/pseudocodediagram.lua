--[[pseudocodediagram.lua]]--

local diagramstyle = {}
diagramstyle.instruction_registerstyle = '[31;1m' --red + bold
diagramstyle.instruction_hexstyle = '[34;1m' --blue + bold
diagramstyle.instruction_symbolstyle = '[32;1m' --green + bold
diagramstyle.instruction_opcodestyle = '[1m' --bold
diagramstyle.link_nottakencolor = 0x000000FF --red
diagramstyle.link_takencolor = 0x00FF0000 --blue
diagramstyle.link_linethickness = 2
diagramstyle.block_headershowsymbol = true
diagramstyle.block_bodyshowaddresses = false
diagramstyle.block_backgroundcolor = 0x00FFFFFF
diagramstyle.diagram_blackgroundcolor = 0x00808080

function editDiagramStyle(new_diagramstyle)
  if (new_diagramstyle) then
    if (new_diagramstyle.instruction_registerstyle) then 
      diagramstyle.instruction_registerstyle = new_diagramstyle.instruction_registerstyle end
    if (new_diagramstyle.instruction_hexstyle) then 
      diagramstyle.instruction_hexstyle = new_diagramstyle.instruction_hexstyle end
    if (new_diagramstyle.instruction_symbolstyle) then 
      diagramstyle.instruction_symbolstyle = new_diagramstyle.instruction_symbolstyle end
    if (new_diagramstyle.instruction_opcodestyle) then 
      diagramstyle.instruction_opcodestyle = new_diagramstyle.instruction_opcodestyle end
    if (new_diagramstyle.link_nottakencolor) then
      diagramstyle.link_nottakencolor = new_diagramstyle.link_nottakencolor end
    if (new_diagramstyle.link_takencolor) then
      diagramstyle.link_takencolor = new_diagramstyle.link_takencolor end
    if (new_diagramstyle.block_headershowsymbol) then
      diagramstyle.block_headershowsymbol = new_diagramstyle.block_headershowsymbol end
    if (new_diagramstyle.block_bodyshowaddresses) then
      diagramstyle.block_bodyshowaddresses = new_diagramstyle.block_bodyshowaddresses end
    if (new_diagramstyle.block_backgroundcolor) then
      diagramstyle.block_backgroundcolor = new_diagramstyle.block_backgroundcolor end
    if (new_diagramstyle.diagram_blackgroundcolor) then
      diagramstyle.diagram_blackgroundcolor = new_diagramstyle.diagram_blackgroundcolor end
    if (new_diagramstyle.link_linethickness) then
      diagramstyle.link_linethickness = new_diagramstyle.link_linethickness end
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

function decorateBlockInstruction(instruction)
  local i, result, temp = 0, ' '

  for word in string.gmatch(instruction,'[^-]*') do
    if (i == 2 and word ~= '') then temp = word
    else
      if (word ~= '' and temp ~= nil) then temp = temp .. '-' .. word
      elseif (word ~= '' and diagramstyle.block_bodyshowaddresses) then result = result .. word .. '-' end
    end
    if (word ~= '') then i = i + 1 end
  end

  i = 0
  result =  result .. ' ' .. string.char(27).. diagramstyle.instruction_opcodestyle --bold

  for word in string.gmatch(temp,'[^ ]*') do
    if (i == 0 and word ~= '') then
      result = result .. ' ' .. word .. string.char(27) .. '[0m' .. ' ' --terminator
    else
      if (word ~= '') then result = result .. word .. ' ' end
    end
    if (word ~= '') then  i = i + 1 end
  end

  instruction = result
  result = nil

  for word in string.gmatch(instruction,'[^{*}]*') do
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
  form.BorderStyle='bsSizeable'
  form.Caption=name
  return form
end

function createDiagramDiagram(form)
  local diagram = createDiagram(form)
  diagram.Align='alClient'
  diagram.ArrowStyles='[asDestination,asPoints,asCenter]'
  diagram.BackgroundColor=diagramstyle.diagram_blackgroundcolor
  diagram.BlockBackground=diagramstyle.block_backgroundcolor
  diagram.LineThickness=diagramstyle.link_linethickness
  return diagram
end

function createDiagramBlock(diagram, name)
  local diagramblock = diagram.createBlock()
  diagramblock.Caption=name
  return diagramblock
end

function createDiagramLink(diagram, sourceblock, destinationblock, color)
  local diagramlink = diagram.addConnection(sourceblock, destinationblock)
  diagramlink.LineColor = color
  return diagramlink
end

function createDiagramBlocks(diagram, state, blocks)
  local disassembler, temp = getVisibleDisassembler()
  local dblocks = {}
  for i,block in pairs(blocks) do
    if state.parsed[block.start] then
      --create block
      if (diagramstyle.block_headershowsymbol) then
        dblocks[i] = createDiagramBlock(diagram, ' ' .. string.char(27) .. diagramstyle.instruction_symbolstyle .. getNameFromAddress(block.start))
      else
        dblocks[i] = createDiagramBlock(diagram, ' ' .. string.format('0x%X', block.start))
      end
      --fill block
      local current = block.start
      while (current <= block.stop) do
        temp = decorateBlockInstruction(disassembler.disassemble(current))
        dblocks[i].Strings.add(temp)
        if state.parsed[current].bytesize ~= 0 then current = current + state.parsed[current].bytesize
        else break end  
      end
      dblocks[i].AutoSize = true
    end
  end
  return dblocks
end

function linkDiagramBlocks(diagram, state, dblocks, blocks)
  local destinationblock_index
  local istaken = {}
  for i,diagramblock in pairs(dblocks) do
    if (i > 1) then --skip starting block
      for j,source in pairs(blocks[i].getsJumpedToBy) do
        if (source == blocks[i-1].stop) then
          createDiagramLink(diagram, dblocks[i-1], diagramblock, diagramstyle.link_nottakencolor) --not taken branches
          istaken[i] = false
        end
      end
    end
    if (blocks[i].jumpsTo) then --skip leaf blocks
      destinationblock_index = blockAddressToBlockIndex(blocks, blocks[i].jumpsTo.destinationtaken)
      if (destinationblock_index) then
        createDiagramLink(diagram, diagramblock, dblocks[destinationblock_index], diagramstyle.link_takencolor) --taken branches
        istaken[destinationblock_index] = true
      end
    end
  end
  return istaken
end

function arrangeDiagramBlocks(dblocks, istaken)
  for i,dblock in pairs(dblocks) do
    if (i > 1) then
      if (istaken[i]) then
        dblock.y = dblocks[i-1].y + dblocks[i-1].height + 50
        else
        dblock.x = dblocks[i-1].x + dblocks[i-1].width + 50
        dblock.y = dblocks[i-1].y + dblocks[i-1].height + 50
      end
    end
  end
end

function spawnDiagram(start, limit)
  local dform = createDiagramForm('Diagram')
  local ddiagram = createDiagramDiagram(dform)
  local state = parseFunction(start, limit)
  local blocks = createBlocks(state)
  local dblocks = {}
  local istaken = {}
  dblocks = createDiagramBlocks(ddiagram, state, blocks)
  istaken = linkDiagramBlocks(ddiagram, state, dblocks, blocks)
  arrangeDiagramBlocks(dblocks, istaken)
end


--[[todolist]]
--put incoming lines in the top and outgoing lines in the bottom, add points to lines, etc...
--have a rightclick on an address function, then find the start of the function and then parse and display the diagram
--incorporate frmtracer results in it, or ultimap traces