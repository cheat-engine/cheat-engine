local function genericJumpHandler(state, alwaystaken)
  local origin=state.address
  local addressString=string.gsub(state.ldd.parameters,"qword ptr ","")
  local addressString=string.gsub(addressString,"dword ptr ","")
  local destination=getAddressSafe(addressString) --find out the destination
  local destination2

  if destination==nil then
    --in case of registers
    return
  end

  if not alwaystaken then
    destination2=origin+state.parsed[origin].bytesize
  end;


  state.branchOrigins[origin]={}
  state.branchOrigins[origin].destinationtaken=destination
  state.branchOrigins[origin].destinationnottaken=destination2

  if state.branchDestinations[destination]==nil then --list of destinations and their origin(s)
    state.branchDestinations[destination]={}
  end

  table.insert(state.branchDestinations[destination], origin)

  if not alwaystaken then
    if state.branchDestinations[destination2]==nil then --list of destinations and their origin(s)
      state.branchDestinations[destination2]={}
    end

    table.insert(state.branchDestinations[destination2], origin)
  end

  return destination
end

local function pfindNextSpot(state)
  --check if there are unreached branch destinations, if so, change the address to one of those
  --print('pfindNextSpot')

  for destination,origins in pairs(state.branchDestinations) do
    if state.parsed[destination]==nil then
      state.address=destination
      return true
    end
  end

  --if not, return false,true to exit
  --print("no new point found")
  return false,true
end


local plookup={}
plookup['jo']=function(state)
  genericJumpHandler(state) --the conditional ones do not change the state, but will store the destination
end

plookup['jno']=function(state)
  genericJumpHandler(state)
end

plookup['jb']=function(state)
  genericJumpHandler(state) --the conditional ones do not change the state, but will store the destination
end

plookup['jnb']=function(state)
  genericJumpHandler(state) --the conditional ones do not change the state, but will store the destination
end

plookup['jae']=function(state)
  genericJumpHandler(state)
end

plookup['je']=function(state)
  genericJumpHandler(state)
end

plookup['jne']=function(state)
  genericJumpHandler(state)
end

plookup['ja']=function(state)
  genericJumpHandler(state)
end

plookup['jna']=function(state)
  genericJumpHandler(state)
end

plookup['js']=function(state)
  genericJumpHandler(state)
end

plookup['jns']=function(state)
  genericJumpHandler(state)
end

plookup['jp']=function(state)
  genericJumpHandler(state)
end

plookup['jnp']=function(state)
  genericJumpHandler(state)
end

plookup['jl']=function(state)
  genericJumpHandler(state)
end

plookup['jnl']=function(state)
  genericJumpHandler(state)
end

plookup['jle']=function(state)
  genericJumpHandler(state)
end

plookup['jg']=function(state)
  genericJumpHandler(state)
end

plookup['jng']=function(state)
  genericJumpHandler(state)
end


plookup['jmp']=function(state)
  local newa
  newa=genericJumpHandler(state, true)
  if newa==nil then --register based jmp
    state.branchOrigins[state.address]={} --jumps to 'nowhere and everywhere'
  else
    state.address=newa
    return true
  end
end


plookup['loop']=function(state)
  genericJumpHandler(state)
end

plookup['ret']=function(state)
  --end of the function (or some weird trick)

  --print('ret')
  
  state.branchOrigins[state.address]={}
  
  return pfindNextSpot(state)
end


function parseFunction(startaddress, limit)
  local state={}
  --print('parseFunction')
  state.branchDestinations={} --list of branch destinations
  state.branchOrigins={} --list of branch origins

  state.parsed={} --list of addresses parsed (to track holes, etc...)

  local currentpath={}
  currentpath.asm={}

  state.address=startaddress

  local d=createDisassembler()
  local done=false
  if limit==nil then limit=1000 end

  local statechange, shouldexit

  while (not done) and (limit>0) do
    local a=state.address

   -- print(string.format('%x',a))

    if state.parsed[a] then
      --already parsed
      statechange, shouldexit=pfindNextSpot(state)
      if shouldexit then break end
      if not statechange then break end

      a=state.address
    end

    local s=d.disassemble(a)

    state.parsed[a]={}
    state.parsed[a].instruction=s



    local ldd=d.getLastDisassembleData()
    table.insert(currentpath.asm, ldd)
    state.ldd=ldd

    state.parsed[a].bytesize=#ldd.bytes


    local statechange
    local f=plookup[ldd.opcode]
    if f then
      statechange,shouldexit=f(state)
    end



    if (not statechange) then
      if #ldd.bytes==0 then
        statechange,shouldexit=pfindNextSpot(state)
      else
        state.address=state.address+#ldd.bytes
      end
    end


    limit=limit-1

    if shouldexit then break end
  end


  return state
end

local function createBlocks(state)
  local blocks={}

  --first sort the parsed instruction list
  local sal={} --sorted address list
  for n in pairs(state.parsed) do
    table.insert(sal,n);
  end

  table.sort(sal)

  blocks[1]={}
  blocks[1].start=sal[1]
  blocks[1].salstartindex=1

  blocks[1].getsJumpedToBy=state.branchDestinations[sal[1]]

  for i=2,#sal do
    local address=sal[i]
    if state.branchDestinations[address] then --first check this
      --this is a destination, so cut off at the previous one
      blocks[#blocks].stop=sal[i-1]
      blocks[#blocks].salstopindex=i-1
      blocks[#blocks].jumpsTo=state.branchOrigins[blocks[#blocks].stop]

      if blocks[#blocks].jumpsTo==nil then
        blocks[#blocks].jumpsTo={}
        blocks[#blocks].jumpsTo.destinationtaken=address
        blocks[#blocks].jumpsTo.logicalFollow=true
        
        
      end

      blocks[#blocks+1]={}
      blocks[#blocks].start=address
      blocks[#blocks].salstartindex=i
      blocks[#blocks].getsJumpedToBy=state.branchDestinations[address]
    end
    
    if (i==#sal) and (state.branchOrigins[address]) then
      blocks[#blocks].jumpsTo=state.branchOrigins[address]
    end    
  end

  blocks[#blocks].stop=sal[#sal]
  blocks[#blocks].salstopindex=#sal

  return blocks,sal
end

pseudocode = {}
pseudocode.parseFunction = parseFunction
pseudocode.createBlocks = createBlocks

--[[
z=parseFunction('GetModuleHandleA')

b=createBlocks(z)
for i=1,#b do
  local from='               '
  local to=''

  if b[i].getsJumpedToBy then
    from=''
    for j=1,#b[i].getsJumpedToBy do
      from=from..string.format("%.8x ",b[i].getsJumpedToBy[j])
    end
  end

  if b[i].jumpsTo then
    if b[i].jumpsTo.destinationtaken then
      to=string.format("%.8x ",b[i].jumpsTo.destinationtaken)
    end

    if b[i].jumpsTo.destinationnottaken then
      to=to..string.format("else %.8x",b[i].jumpsTo.destinationnottaken)
    end
  end

  print(string.format("%.8x-%.8x  Linked From: %s Links To: %s", b[i].start, b[i].stop, from,to))
end
--]]

--return z

--even better: parsefunction(ntoskrnl.PspSetContextThreadInternal) it has a conditional codecave jmp way outside it's function range

