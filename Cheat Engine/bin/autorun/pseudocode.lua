--in development

function genericJumpHandler(state)
  local origin=state.address
  local addressString=string.gsub(state.ldd.parameters,"qword ptr ","")
  local addressString=string.gsub(addressString,"dword ptr ","")
  local destination=getAddressSafe(addressString) --find out the destination

  if destination==nil then
    --in case of registers
    return
  end




  state.branchOrigins[origin]=destination  --just for extra info, not that useful
  if state.branchDestinations[destination]==nil then --list of destinations and their origin(s)
    state.branchDestinations[destination]={}
  end

  table.insert(state.branchDestinations[destination], origin)

  return destination
end

function pfindNextSpot(state)
  --check if there are unreached branch destinations, if so, change the address to one of those
  print('pfindNextSpot')

  for destination,origins in pairs(state.branchDestinations) do
    if state.parsed[destination]==nil then
      state.address=destination
      return true
    end
  end

  --if not, return false,true to exit
  print("no new point found")
  return false,true
end


plookup={}
plookup['je']=function(state)
  genericJumpHandler(state) --the conditional ones do not change the state, but will store the destination
end

plookup['jne']=function(state)
  genericJumpHandler(state)
end

plookup['jo']=function(state)
  genericJumpHandler(state)
end

plookup['ja']=function(state)
  genericJumpHandler(state)
end


plookup['jmp']=function(state)
  state.address=genericJumpHandler(state)
  return true
end

plookup['loop']=function(state)
  genericJumpHandler(state)
end

plookup['ret']=function(state)
  --end of the function (or some weird trick)

  print('ret')
  return pfindNextSpot(state)
end


function parseFunction(startaddress, limit)
  local state={}
  print('parseFunction')
  state.branchDestinations={} --list of branch destinations
  state.branchOrigins={} --list of branch origins

  state.parsed={} --list of addresses parsed (to track holes, etc...)

  state.paths={}
  local currentpath={}
  currentpath.asm={}

  state.address=startaddress

  local d=createDisassembler()
  local done=false
  if limit==nil then limit=1000 end

  local statechange, shouldexit

  while (not done) and (limit>0) do
    local a=state.address

    print(string.format('%x',a))

    if state.parsed[a] then
      --already parsed
      statechange, shouldexit=pfindNextSpot(state)
      if shouldexit then break end
      if not statechange then break end

      a=state.address
    end



    d.disassemble(a)

    state.parsed[a]=true

    local ldd=d.getLastDisassembleData()
    table.insert(currentpath.asm, ldd)
    state.ldd=ldd

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

--parseFunction(0x00413190)

--even better: parsefunction(ntoskrnl.PspSetContextThreadInternal) it has a conditional codecave jmp way outside it's function range
