--[[pseudocodediagram.lua]]--

local diagramstyle = {}
diagramstyle.instruction_registerstyle = '[31;1m' --red + bold
diagramstyle.instruction_hexstyle = '[34;1m' --blue + bold
diagramstyle.instruction_symbolstyle = '[32;1m' --green + bold
diagramstyle.instruction_opcodestyle = '[1m' --bold
diagramstyle.link_nottakencolor = 0x000000FF --red
diagramstyle.link_takencolor = 0x00FF0000 --blue
diagramstyle.block_headershowsymbol = true
diagramstyle.block_bodyshowaddresses = false

function editDiagramStyle(table_diagramstyle)
  if (table_diagramstyle) then
    if (table_diagramstyle.instruction_registerstyle) then 
      diagramstyle.instruction_registerstyle = table_diagramstyle.instruction_registerstyle end
    if (table_diagramstyle.instruction_hexstyle) then 
      diagramstyle.instruction_hexstyle = table_diagramstyle.instruction_hexstyle end
    if (table_diagramstyle.instruction_symbolstyle) then 
      diagramstyle.instruction_symbolstyle = table_diagramstyle.instruction_symbolstyle end
    if (table_diagramstyle.instruction_opcodestyle) then 
      diagramstyle.instruction_opcodestyle = table_diagramstyle.instruction_opcodestyle end
    if (table_diagramstyle.link_nottakencolor) then
      diagramstyle.link_nottakencolor = table_diagramstyle.link_nottakencolor end
    if (table_diagramstyle.link_takencolor) then
      diagramstyle.link_takencolor = table_diagramstyle.link_takencolor end
    if (table_diagramstyle.block_headershowsymbol) then
      diagramstyle.block_headershowsymbol = table_diagramstyle.block_headershowsymbol end
    if (table_diagramstyle.block_bodyshowaddresses) then
      diagramstyle.block_bodyshowaddresses = table_diagramstyle.block_bodyshowaddresses end
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

function decorateBlockInstruction(instruction) --todo: customizable
  local i, j, result = 0, 0, ' '
  for word in string.gmatch(instruction,'[^-]*') do
      if (i == 2 and word ~= '') then --=Opcode
        result =  result .. ' ' .. string.char(27).. diagramstyle.instruction_opcodestyle --bold
        for ward in string.gmatch(word,'[^ ]*') do
          if (j == 2) then
            result = result .. ' ' .. ward .. string.char(27) .. '[0m' --terminator
          else
            if (ward ~= '') then result = result .. ward .. ' ' end
          end
          j = j + 1
        end
      else
        if (word ~= '' and diagramstyle.block_bodyshowaddresses) then result = result .. word .. '-' end
      end
      if (j ~= 0) then break end
      if (word ~= '') then i = i + 1 end
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

function fillDiagramBlocks(diagram, state, diagramblocks, blocks)
  local disassembler, temp = getVisibleDisassembler()
  for i,block in pairs(blocks) do
    if state.parsed[block.start] then
      --create block
      if (diagramstyle.block_headershowsymbol) then
        diagramblocks[i] = createDiagramBlock(diagram, ' ' .. string.char(27) .. diagramstyle.instruction_symbolstyle .. getNameFromAddress(block.start))
      else
        diagramblocks[i] = createDiagramBlock(diagram, ' ' .. string.format('0x%X', block.start))
      end
      --fill block
      local current = block.start
      while (current <= block.stop) do
        temp = decorateBlockInstruction(disassembler.disassemble(current))
        diagramblocks[i].Strings.add(temp)
        if state.parsed[current].bytesize ~= 0 then current = current + state.parsed[current].bytesize
        else break end  
      end
      diagramblocks[i].AutoSize = true
    end
  end
end

function linkDiagramBlocks(diagram, state, diagramblocks, blocks)
  local destinationblock_index
  local istaken = {}
  for i,diagramblock in pairs(diagramblocks) do
    if (i > 1) then --skip starting block
      for j,source in pairs(blocks[i].getsJumpedToBy) do
        if (source == blocks[i-1].stop) then
          createDiagramLink(diagram, diagramblocks[i-1], diagramblock, diagramstyle.link_nottakencolor) --not taken branches
          istaken[i] = false
        end
      end
    end
    if (blocks[i].jumpsTo) then --skip leaf blocks
      destinationblock_index = blockAddressToBlockIndex(blocks, blocks[i].jumpsTo.destinationtaken)
      if (destinationblock_index) then
        createDiagramLink(diagram, diagramblock, diagramblocks[destinationblock_index], diagramstyle.link_takencolor) --taken branches
        istaken[destinationblock_index] = true
      end
    end
  end
  return istaken
end

function arrangeDiagramBlocks(diagram, state, diagramblocks, blocks, istaken)
  for i,diagramblock in pairs(diagramblocks) do
    if (i > 1) then
      if (istaken[i]) then
        diagramblock.y = diagramblocks[i-1].y + diagramblocks[i-1].height + 50
        else
        diagramblock.x = diagramblocks[i-1].x + diagramblocks[i-1].width + 50
        diagramblock.y = diagramblocks[i-1].y + diagramblocks[i-1].height + 50
      end
    end
  end
end

function spawnDiagram(start, limit)
  local dform = createDiagramForm('Diagram')
  local ddiagram = createDiagramDiagram(dform)
  local state = parseFunction(start, limit)
  local blocks = createBlocks(state)
  local diagramblocks = {}
  local istaken = {}
  fillDiagramBlocks(ddiagram, state, diagramblocks, blocks)
  istaken = linkDiagramBlocks(ddiagram, state, diagramblocks, blocks)
  arrangeDiagramBlocks(ddiagram, state, diagramblocks, blocks, istaken)
end



--[[todolist]]
--blocks auto position
--put incoming lines in the top and outgoing lines in the bottom, add points to lines, etc...
--have a rightclick on an address function, then find the start of the function and then parse and display the diagram
--incorporate frmtracer results in it, or ultimap traces