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

ultimap2={}

CMD_None=0
CMD_LoadToPA=1
CMD_StartRecording=2
CMD_StopRecording=3

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
  
  --count the active cpu's
  local i
  local count=0
  for i=1, #ultimap2.gbBuffer do
    if ultimap2.gbBuffer[i].ab.Checked then count=count+1 end
  end
  
  if count==0 then error('no active cpu') end

  if getOpenedProcessID()==0 then error('First open a process') end
  
 
  local sizePerCPU=math.floor(size / count) & ~0xfff;
  local left=size-(sizePerCPU*count) & ~0xfff;
  
  if (sizePerCPU<8192) then error('setupToPA: sizePerCPU<8192') end  --ToPA table and first output page

  if (ultimap2.ToPABuffers~=nil) then
    for k,v in pairs(ultimap2.ToPABuffers) do
	  freeKernelMemory(v.Memory)
	end
  end
  
  ultimap2.ToPABuffers={}
 

  local extra=0
  for i=1, #ultimap2.gbBuffer do
    if ultimap2.gbBuffer[i].ab.Checked then
	  
	  if left>=4096 then
	    extra=4096
		left=left-4096
	  else
	    extra=0
	  end
	  local size=sizePerCPU+extra;
	  ultimap2.ToPABuffers[i]={}
	  ultimap2.ToPABuffers[i].Memory=allocateKernelMemory(size)
	  ultimap2.ToPABuffers[i].Layout={} --list that contains the physical addresses sorted in the order they will get filled
	  local ToPAMemory=ultimap2.ToPABuffers[i].Memory	  
	  
	  ultimap2.Command[i]=CMD_LoadToPA
	  print(string.format("ToPA for cpu %d allocated at %x", i-1, ToPAMemory))	

	  if (ToPAMemory~=0) then
		--configure it
		--zero the memory (not really needed, but do this while testing)

		for i=0, size-7, 8 do
		  writeQword(ToPAMemory+i,0)
		end

		local maxAddress=ToPAMemory+size-1
		local currentOutput=ToPAMemory+4096
		local currentToPA=ToPAMemory
		local maxToPA=currentToPA+4096-9
		

		local e={}
		local lastToPA=currentToPA
		while (currentOutput<maxAddress) do
		  lastToPA=currentToPA
		  e.PhysicalAddress= dbk_getPhysicalAddress(currentOutput)
		  e.Size = 0  --todo: group together if on a proper alignment and contiguous
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
			
			table.insert(ultimap2.ToPABuffers[i].Layout, e.PhysicalAddress)
			--TODO: If support for size>0 then for each page call a table insert
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
  --print("Thread for cpu "..cpunr-1)
  local progresscheck={}
  SetAffinityThread(1 << (cpunr-1))
  sleep(1000)
  
  --if (getCPUNR()~=cpunr-1) then print("Error for thread "..cpunr) else print("Success for thread"..cpunr) end
  
  dbk_writeMSR(IA32_RTIT_CTL,0)
  dbk_writeMSR(IA32_RTIT_STATUS,0)  
  
  while not t.Terminated do
    
    --get the status and update ultimap2.gbBuffer[cpunr+1].progress 
	if ultimap2.Command[cpunr]==CMD_LoadToPA then	
	  dbk_writeMSR(IA32_RTIT_OUTPUT_BASE, dbk_getPhysicalAddress(ToPABuffers[cpunr].Memory))
	  dbk_writeMSR(IA32_RTIT_OUTPUT_MASK_PTRS,0)	
	  ultimap2.Command[cpunr]=0
	  
	  progresscheck={} --calculate the percentage for progress	  
	  local maxindex=#ultimap2.ToPABuffers;
	  for k,v in pairs(ultimap2.ToPABuffers) do
	    progresscheck[v]=(k/maxindex)*100;
	  end	  
	end
	
	if ultimap2.Command[cpunr]==CMD_StartRecording then
	  dbk_writeMSR(IA32_RTIT_OUTPUT_BASE, dbk_getPhysicalAddress(ToPABuffers[cpunr].Memory))
	  dbk_writeMSR(IA32_RTIT_OUTPUT_MASK_PTRS,0)		
	  launchRTIT()
	  ultimap2.Command[cpunr]=0
	  
	  progresscheck={} --calculate the percentage for progress
	  local maxindex=#ultimap2.ToPABuffers;
	  for k,v in pairs(ultimap2.ToPABuffers) do
	    progresscheck[v]=(k/maxindex)*100;	  	
      end		
	end
	
	if ultimap2.Command[cpunr]==CMD_StopRecording then
	  dbk_writeMSR(IA32_RTIT_CTL,0)
	  ultimap2.Command[cpunr]=0
	end	
	
	
	
    sleep(100);
	if ultimap2.Active then
  
	
	  --collect data
	  local status=getRTIT_STATUS()
	  local OutputBase=dbk_readMSR(IA32_RTIT_OUTPUT_BASE)
	  local OutputMask=dbk_readMSR(IA32_RTIT_OUTPUT_MASK_PTRS)
	  
	  t.synchronize(function(t, cpunr, Base, Mask, Status)
	    if ultimap2.gbBuffer[cpunr].ab.Checked then
	      print("cpu"..(cpunr-1)..":")
		  if Status.Error then print("cpu"..(cpunr-1)..":Error") end
		  if Status.Stopped then print("cpu"..(cpunr-1)..":Stopped") end
		  print(string.format("Base=%x MaskPTR=%x", Base, Mask))  
		  
		  --update the progress percentage
		end
	  end, cpunr, OutputBase, OutputMask, status)
	end
  end
end

function waitForWatcherThreads() --only from mainthread
  --TODO: Add event and other sync methods to lua
  local starttime=GetTickCount();
  for i=1, #ultimap2.gbBuffer do
    if ultimap2.Command[i]~=CMD_None then
	  if GetTickCount()>starttime+5000 then --5 seconds
	    print("Thread "..i.." has stopped responding");
		return
      end
	  sleep(10)
	  checkSynchronize()
	end
  end
end

function RecordClick(sender)
  if (sender.Checked) then
    for i=1, #ultimap2.gbBuffer do
      if ultimap2.gbBuffer[i].ab.Checked then
	    ultimap2.Command[i].CMD_StartRecording=true
	  end
	  
	  ultimap2.gbBuffer[i].ab.Enabled=false
	end
	
	
  else 
    for i=1, #ultimap2.gbBuffer do
      ultimap2.Command[i].CMD_StopRecording=true	  
	  ultimap2.gbBuffer[i].ab.Enabled=true
	end
  end
  
  waitForWatcherThreads()
end;

function LaunchUltimap2()
  if true then --Ultimap2Check() then
    --create a form with options and state of ultimap2
    if ultimap2.Form==nil then
	  ultimap2.Active=false
      ultimap2.Form=createFormFromFile(getCheatEngineDir()..[[autorun\forms\frmUltimap2.FRM]])
      
     
      

      --setup the trace buffer display.
      local cpucount=getCPUCount()
	  ultimap2.gbBuffer={}
	  ultimap2.Command={}
	  
	  
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
		ultimap2.gbBuffer[i].ab.Checked=true
		
		
		ultimap2.gbBuffer[i].progress=createImage(ultimap2.gbBuffer[i].gb)
		ultimap2.gbBuffer[i].progress.Align=alClient
		ultimap2.gbBuffer[i].progress.Stretch=true
		ultimap2.gbBuffer[i].progress.Picture.Bitmap.Width=1
		ultimap2.gbBuffer[i].progress.Picture.Bitmap.Height=100		
		ultimap2.gbBuffer[i].progress.Picture.Bitmap.Canvas.gradientFill(0,0,1,100,0x0000ff,0x00ff00,0)
		
		--launch a monitor thread for this cpu (even runs when not active)
		createNativeThread(RTIT_WatcherThread,i)
	  end
	  
	  ultimap2.Form.gbCpuTraceBuffers.OnResize=BufferPanelResize
	  ultimap2.Form.Show()
	  BufferPanelResize(ultimap2.Form.gbCpuTraceBuffers)
	 
	  ultimap2.Form.tbRecord.OnClick=RecordClick
    end
  end
end
