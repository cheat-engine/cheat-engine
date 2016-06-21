--make sure the cpu affinity of ce is set to only 1 cpu
function Ultimap2Check()
  check1=CPUID(0)
  if (check1.ebx~=1970169159) or  (check1.ecx ~= 1818588270) or (check1.edx ~= 1231384169) then
    print("I do recommend you get a real CPU")
    return
  end 

  if ((CPUID(0x7,0).ebx >> 25) & 1)==0 then  --check cpu feature flag if it supports trace
    print("Error: Your cpu sucks")
    return  
  end

  CPUID_14_0=CPUID(0x14,0)

  if (CPUID_14_0.ecx & (1 << 1)==0) and (CPUID_14_0.ecx & (1 << 0)==0) then
    print("Error: Single Output Region ToPA Only CPU. Not supported. Sorry!")
    return
  end

  if (CPUID_14_0.ebx & 1)==0 then
    print("Error: No CR3 Filter support for this CPU")
    return
  end

  print("Your cpu passed the tests. It should support Ultimap V2.0")
  return true
end


--MSR's
IA32_PERF_GLOBAL_STATUS=0x38e  --bit 55=Trace_ToPA_PMI

IA32_RTIT_CTL=0x570
IA32_RTIT_STATUS=0x571
IA32_RTIT_CR3_MATCH=0x572



--ToPA
IA32_RTIT_OUTPUT_BASE=0x560
IA32_RTIT_OUTPUT_MASK_PTRS=0x561

function ToPA_Table_Entry_To_Value(e)
  local v=e.PhysicalAddress -- << 12;

  v=v | (e.Size << 6)  
  
  if e.STOP then v=v | (1 << 4) end 
 -- if e.INT then v=v | (1 << 2) end  --I don't recommend using this in lua
  if e.END then v=v | 1 end 

  return v
end

function Value_To_ToPA_Table_Entry(v)
  local e={}
  e.PhysicalAddress=(v & 0xfffffffffffff000)-- >> 12;
  e.Size = (v << 6) & 0xf
  e.STOP = (v & (1 << 4))~=0
  e.INT = (v & (1 << 2))~=0
  e.END = (v & 1)~=0
  return e
end

function RTIT_CTL_To_Value(e)
  local v=0
  if e.TraceEn then v=v | 1 end
  if e.OS then v=v | (1 << 2) end
  if e.USER then v=v | (1 << 3) end
  if e.CR3Filter then v=v | (1 << 7) end
  if e.ToPA then v=v | (1 << 8) end
  if e.TSCEn then v=v | (1 << 10) end
  if e.DisRETC then v=v | (1 << 11) end
  if e.Bit13 then v=v | (1 << 13) end
  return v
end

function Value_To_RTIT_CTL(v)
  local e={}
  e.TraceEn = (v & 1)~=0
  e.OS = (v & (1 << 2))~=0
  e.USER = (v & (1 << 3))~=0
  e.CR3Filter = (v & (1 << 7))~=0
  e.ToPA = (v & (1 << 8))~=0
  e.TSCEn = (v & (1 << 10))~=0
  e.DisRETC = (v & (1 << 11))~=0
  e.Bit13 = (v & (1 << 13))~=0

  return e
end

function setRTIT_CTL(e)
  return dbk_writeMSR(IA32_RTIT_CTL, RTIT_CTL_To_Value(e))
end

function getRTIT_CTL()
  return Value_To_RTIT_CTL(dbk_readMSR(IA32_RTIT_CTL))
end


function Value_To_RTIT_STATUS(v)
  local e={}
  e.ContextEn = (v & (1 << 1))~=0
  e.TriggerEn = (v & (1 << 2))~=0
  e.Error = (v & (1 << 4))~=0
  e.Stopped = (v & (1 << 5))~=0

  return e
end

function getRTIT_STATUS()
  return Value_To_RTIT_STATUS(dbk_readMSR(IA32_RTIT_STATUS))
end

function setupToPA(size)
  dbk_initialize()
  dbk_useKernelmodeOpenProcess()
  dbk_useKernelmodeProcessMemoryAccess() 


  if getOpenedProcessID()==0 then error('First open a process') end

  if (size<12288) then error('setupToPA: size<12288') end --at least one page for data
  
  if ToPAMemory~=nil then
    print("Free old ToPA memory block")
    freeKernelMemory(ToPAMemory)
    ToPAMemory=nil
  end

  ToPAMemory=allocateKernelMemory(size)
  if (ToPAMemory~=0) then
    --configure it
    --zero the memory
    print(string.format("ToPA allocated at %x", ToPAMemory))

    for i=0, size-7, 8 do
      writeQword(ToPAMemory+i,0)
    end

    dbk_writeMSR(IA32_RTIT_OUTPUT_BASE, dbk_getPhysicalAddress(ToPAMemory))
    dbk_writeMSR(IA32_RTIT_OUTPUT_MASK_PTRS,0)

    local maxAddress=ToPAMemory+size-1
    local currentOutput=ToPAMemory+4096
    local currentToPA=ToPAMemory
    local maxToPA=currentToPA+4096-9
    

    local e={}
    local lastToPA=currentToPA
    while (currentOutput<maxAddress) do
      lastToPA=currentToPA
      e.PhysicalAddress= dbk_getPhysicalAddress(currentOutput)
      e.Size = 0  --todo: group together if on a proper alignment
      e.STOP = false
      e.INT = false
      
      if currentToPA>=maxToPA then
        --this block becomes a new ToPA
        print("new ToPA list")
        e.END = true          

        currentToPA=currentOutput
        maxToPA=currentToPA+4096-9
      else
        e.END = false
        currentToPA=currentToPA+8
      end

      writeQword(lastToPA, ToPA_Table_Entry_To_Value(e))

      currentOutput=currentOutput+4096        
    end

    --end of memory
    e.STOP = true
    e.INT = false
    e.END = false
    e.Size = 0
    writeQword(lastToPA, ToPA_Table_Entry_To_Value(e))  --mark it as stop

    
  end
end

function launchRTIT()
  local e=getRTIT_CTL()
  e.TraceEn = true
  e.OS = false
  e.USER = true
  e.CR3Filter = false --for now
  e.ToPA = true
  e.TSCEn = true
  e.DisRETC = true

  setRTIT_CTL(e)
end


--dbk_writeMSR(IA32_RTIT_CTL,0)
--dbk_writeMSR(IA32_RTIT_STATUS,0)
--dbk_writeMSR(IA32_RTIT_OUTPUT_BASE, 0)
--dbk_writeMSR(IA32_RTIT_OUTPUT_MASK_PTRS,0)


--setupToPA(4194304)
--launchRTIT()
--sleep(1000)

--if (getRTIT_STATUS().Error) then
--  print("fuck")
--else
--  print("Yeeeh. ")
--end

ultimap2={}

function BufferPanelResize(sender)
  --resize all the groupboxes so they fix the current width/height
  local i

  if (ultimap2.gbBuffer) and (#ultimap2.gbBuffer>0) then
    local bwidth=sender.ClientWidth / #ultimap2.gbBuffer
    for i=1, #ultimap2.gbBuffer do
      ultimap2.gbBuffer[i].gb.Width=bwidth
	  ultimap2.gbBuffer[i].gb.Height=sender.ClientHeight
	  ultimap2.gbBuffer[i].gb.Top=0
	  ultimap2.gbBuffer[i].gb.Left=bwidth*(i-1)	  
	end
  end
end

function RTIT_WatcherThread(t, cpunr)
  print("Thread for cpu "..cpunr)
  SetAffinityThread(1 << cpunr)
  sleep(1000)
  
  --if (getCPUNR()~=cpunr) then print("Error for thread "..cpunr) else print("Success for thread"..cpunr) end
  
  
  while not t.Terminated do
    --get the status and update ultimap2.gbBuffer[cpunr+1].progress 
    sleep(1000);
  end
end


function LaunchUltimap2()
  if true then --Ultimap2Check() then
    --create a form with options and state of ultimap2
    if ultimap2.Form==nil then
      ultimap2.Form=createFormFromFile(getCheatEngineDir()..[[autorun\forms\frmUltimap2.FRM]])
      
     
      ultimap2.Form.Show()

      --setup the trace buffer display.
      local cpucount=getCPUCount()
	  ultimap2.gbBuffer={}
	  
	  
	  local i
	  for i=1, cpucount do
		ultimap2.gbBuffer[i]={}
		ultimap2.gbBuffer[i].gb=createGroupBox(ultimap2.Form.gbCpuTraceBuffers)
		ultimap2.gbBuffer[i].gb.Caption="CPU "..i-1
		
		ultimap2.gbBuffer[i].ab=createCheckBox(ultimap2.gbBuffer[i].gb)
		ultimap2.gbBuffer[i].ab.Caption="Active"
		ultimap2.gbBuffer[i].ab.AnchorSideLeft.Control = ultimap2.gbBuffer[i].gb
		ultimap2.gbBuffer[i].ab.AnchorSideLeft.Side = asrCenter
		ultimap2.gbBuffer[i].ab.AnchorSideBottom.Control = ultimap2.gbBuffer[i].gb
		ultimap2.gbBuffer[i].ab.AnchorSideBottom.Side = asrBottom		
		ultimap2.gbBuffer[i].ab.Anchors="[akLeft, akBottom]"
		
		
		ultimap2.gbBuffer[i].progress=createImage(ultimap2.gbBuffer[i].gb)
		ultimap2.gbBuffer[i].progress.Align=alClient
		ultimap2.gbBuffer[i].progress.Stretch=true
		ultimap2.gbBuffer[i].progress.Picture.Bitmap.Width=1
		ultimap2.gbBuffer[i].progress.Picture.Bitmap.Height=100		
		ultimap2.gbBuffer[i].progress.Picture.Bitmap.Canvas.gradientFill(0,0,1,100,0x0000ff,0x00ff00,0)
		
		--launch a monitor thread for this cpu (even runs when not active)
		createNativeThread(RTIT_WatcherThread,i-1)
	  end
	  
	  ultimap2.Form.gbCpuTraceBuffers.OnResize=BufferPanelResize
	  BufferPanelResize(ultimap2.Form.gbCpuTraceBuffers)
    end
  end
end
