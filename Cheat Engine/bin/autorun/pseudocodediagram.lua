--[[pseudocodediagram.lua]]--

local registerstyle = '[31;1m' --red + bold
local hexstyle = '[34;1m' --blue + bold
local symbolstyle = '[32;1m' --green + bold
local opcodestyle = '[1m' --bold
local link
local nottakencolor = 0x000000FF --red
local takencolor = 0x00FF0000 --blue
local blockstyle = {}
blockstyle.headerShowSymbol = true
blockstyle.bodyShowAddresses = false


function editDiagramStyle(table_blockstyle, table_linkstyle, table_instructionstyle)
  if (table_blockstyle) then
     blockstyle.headerShowSymbol = table_blockstyle.headerShowSymbol
     blockstyle.bodyShowAddresses = table_blockstyle.bodyShowAddresses
  end

  if (table_linkstyle) then
   --in development
  end

  if (table_instructionstyle) then
   --in development
  end 
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

function adjustBlockHeightWidth(diagram, diagramblock, blockline, string)
  diagramblock.width = math.max(diagramblock.width, diagram.Canvas.getTextWidth(string))
  diagramblock.height = (blockline+2)*diagram.Canvas.getTextHeight("gjaGWqQ")
end

function decorateBlockInstruction(instruction) --todo: customizable
  local i, j, result = 0, 0, ' '
  for word in string.gmatch(instruction,'[^-]*') do
      if (i == 2 and word ~= '') then --=Opcode
        result =  result .. ' ' .. string.char(27).. opcodestyle --bold
        for ward in string.gmatch(word,'[^ ]*') do
          if (j == 2) then
            result = result .. ' ' .. ward .. string.char(27) .. '[0m' --terminator
          else
            if (ward ~= '') then result = result .. ward .. ' ' end
          end
          j = j + 1
        end
      else
        if (word ~= '' and blockstyle.bodyShowAddresses) then result = result .. word .. '-' end
      end
      if (j ~= 0) then break end
      if (word ~= '') then i = i + 1 end
  end

  instruction = result
  result = nil

  for word in string.gmatch(instruction,'[^{*}]*') do
    if result then
       if word == 'R' then --{R}=Register
          result = result .. string.char(27) .. registerstyle
       elseif word == 'H' then --{H}=Hex value
          result = result .. string.char(27) .. hexstyle
       elseif word == 'S' then --{S}=Symbol
          result = result .. string.char(27) .. symbolstyle
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

function createDiagramBlock(diagram, name)
  local diagramblock = diagram.createBlock()
  diagramblock.Caption = name
  return diagramblock
end

function createDiagramLink(diagram, sourceblock, destinationblock, color)
  local diagramlink = diagram.addConnection(sourceblock, destinationblock)
  diagramlink.LineColor = color
  return diagramlink
end

function blockAddressToBlockIndex(blocks, address)
  for i,block in pairs(blocks) do
    if (block.start == address or block.stop == address) then
      return i
    end
  end
  return nil
end

function fillDiagramBlocks(diagram, state, diagramblocks, blocks)
  local disassembler, temp = getVisibleDisassembler()
  for i,block in pairs(blocks) do
    if state.parsed[block.start] then
      --create block
      if (blockstyle.headerShowSymbol) then
        diagramblocks[i] = createDiagramBlock(diagram, ' ' .. string.char(27) .. symbolstyle .. getNameFromAddress(block.start))
      else
        diagramblocks[i] = createDiagramBlock(diagram, ' ' .. string.format('0x%X', block.start))
      end
      --fill block
      local current = block.start
      local line = 1
      while (current <= block.stop) do
        temp = decorateBlockInstruction(disassembler.disassemble(current))
        diagramblocks[i].Strings.add(temp)
        adjustBlockHeightWidth(diagram, diagramblocks[i], line, temp)
        current = current + state.parsed[current].bytesize
        line = line + 1
      end
    end
  end
end

function linkDiagramBlocks(diagram, state, diagramblocks, blocks)
  local destinationblock_index
  for i,diagramblock in pairs(diagramblocks) do
    --first wave
    if (i > 1) then --skip starting block
      for j,source in pairs(blocks[i].getsJumpedToBy) do
        if (source == blocks[i-1].stop) then
          createDiagramLink(diagram, diagramblocks[i-1], diagramblock, nottakencolor) --not taken branches
        end
      end
    end
    --second wave
    if (blocks[i].jumpsTo) then --skip leaf blocks
      destinationblock_index = blockAddressToBlockIndex(blocks, blocks[i].jumpsTo.destinationtaken)
      if (destinationblock_index) then
        createDiagramLink(diagram, diagramblock, diagramblocks[destinationblock_index], takencolor) --taken branches
      end
    end
  end
end

function spawnDiagram(start, limit)
  local dForm = createDiagramForm('Diagram')
  local dDiagram = createDiagramDiagram(dForm)
  local state = parseFunction(start, limit)
  local blocks = createBlocks(state)
  local diagramblocks = {}
  fillDiagramBlocks(dDiagram, state, diagramblocks, blocks)
  linkDiagramBlocks(dDiagram, state, diagramblocks, blocks)
end



--[[todolist]]
--blocks auto position
--put incoming lines in the top and outgoing lines in the bottom, add points to lines, etc...
--have a rightclick on an address function, then find the start of the function and then parse and display the diagram
--incorporate frmtracer results in it, or ultimap traces