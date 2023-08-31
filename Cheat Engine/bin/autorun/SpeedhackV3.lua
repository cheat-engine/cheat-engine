--Copyright Cheat Engine


function getOriginalCodeAndFiller(address)
  local original,filler

  if type(address)~='number' then
    address=getAddressSafe(address)
  end

  if address==nil then
    return nil, 'invalid address'
  end

  local sl=createStringList()
  local d=createDisassembler()
  local size=0
  while size<5 do
    d.disassemble(address)
    local ldd=d.LastDisassembleData
    local inst=ldd.opcode..' '..ldd.parameters
    sl.add(inst)
    size=size+#ldd.bytes
    address=address+#ldd.bytes
  end

  original=sl.Text
  if size-5>0 then
    filler=string.format("nop %x", size-5)
  else
    filler=''
  end

  sl.destroy()
  d.destroy()
  return original,filler
end


function hookSpeedFunctions()
  --print("hookSpeedFunctions")
  if getAddressSafe("new_gettickcount")~=nil and getAddressSafe("speedhack_wantedspeed")~=nil then
    return true
  end
  
  local r,r2=injectCEHelperLib()
  
  if not r then
    messageDialog('error in injectCEHelperLib(): '..r2, mtError,mbOK)
    return false
  end
  
  local result, data=autoAssemble([[
    globalalloc(speedhack_wantedspeed,4)
    speedhack_wantedspeed:
    dd (float)1


{$asm}

  ]])

  if not result then
    messageDialog(data)
    return
  end

  local gtcaddress=getAddressSafe('kernel32.gettickcount64')
  if gtcaddress==nil then
    waitforExports()
    gtcaddress=getAddressSafe('kernel32.gettickcount64')

    if (gtcaddress==nil) then
      reinitializeSymbolhandler()
      gtcaddress=getAddressSafe('kernel32.gettickcount64')
      if (gtcaddress==nil) then
        messageDialog('Failure finding kernel32.gettickcount64', mtError, mbOK)
        return false
      end
    end
  end


  local originalcode,filler=getOriginalCodeAndFiller(gtcaddress)

  if originalcode then


  --speedhack does not disable. Just sets speed to 1 when done

    local s=string.format([[
alloc(gtc_originalcode,64,"kernel32.gettickcount64")
label(gtc_returnhere)
label(gtchook_exit)

{$c}
#include <stddef.h>
#include <celib.h>


__stdcall uint64_t gtc_originalcode(void);
float gtc_speed=1.0f;
uint64_t gtc_initialtime=0;
uint64_t gtc_initialoffset=0;
cecs gtc_cs;

extern float speedhack_wantedspeed;
extern void csenter(cecs *cs);


__stdcall uint64_t new_gettickcount(void)
{
  uint64_t newtime;

  uint64_t currenttime;
  float wantedspeed; //small issue with tcc where you can not compare against extern directly

  csenter(&gtc_cs);
  
  currenttime=gtc_originalcode();
  
  wantedspeed=speedhack_wantedspeed;

  if (gtc_initialtime==0)
  {
    gtc_initialtime=currenttime;
    gtc_initialoffset=currenttime;
  }

  newtime=(currenttime-gtc_initialtime)*gtc_speed;
  newtime=newtime+gtc_initialoffset; //don't put in in the calculation above, as it gets converted to float, and truncated

  if (gtc_speed!=wantedspeed)
  {
    //the user wants to change the speed
    gtc_initialoffset=newtime;
    gtc_initialtime=currenttime;
    gtc_speed=speedhack_wantedspeed;
  }



  csleave(&gtc_cs);


  return newtime;

}
{$asm}


gtc_originalcode:
%s

gtchook_exit:
jmp gtc_returnhere

kernel32.gettickcount64:
jmp new_gettickcount
%s

gtc_returnhere:


kernel32.timeGetTime:
jmp new_gettickcount

kernel32.getTickCount:
jmp new_gettickcount

]],originalcode, filler)

    local result, data=autoAssemble(s)
  end;


--queryPerformanceCounter
  local qpcaddress=getAddressSafe('ntdll.RtlQueryPerformanceCounter')
  if qpcaddress==nil then
    waitforExports()
    qpcaddress=getAddressSafe('ntdll.RtlQueryPerformanceCounter')

    if (qpcaddress==nil) then
      reinitializeSymbolhandler()
      qpcaddress=getAddressSafe('ntdll.RtlQueryPerformanceCounter')
      if (qpcaddress==nil) then
        messageDialog('Failure finding kernel32.gettickcount64', mtError, mbOK)
        return false
      end
    end
  end


  local originalcode,filler=getOriginalCodeAndFiller(qpcaddress)

  if originalcode then


  --speedhack does not disable. Just sets speed to 1 when done

    local s=string.format([[
alloc(qpc_originalcode,64,"ntdll.RtlQueryPerformanceCounter")
label(qpc_returnhere)
label(qpchook_exit)

{$c}
#include <stddef.h>
#include <celib.h>

__stdcall int  qpc_originalcode(uint64_t *count);
float qpc_speed=1.0f;
uint64_t qpc_initialtime=0;
uint64_t qpc_initialoffset=0;
cecs qpc_cs;

uint64_t qpc_lastresult=0;

extern float speedhack_wantedspeed;
extern void csenter(cecs *cs);


__stdcall int  new_RtlQueryPerformanceCounter(uint64_t *count)
{
  uint64_t newtime;

  uint64_t currenttime;
  uint64_t newwantedspeed;


  float wantedspeed; //small issue with tcc where you can not compare against extern directly

  csenter(&qpc_cs);

  int result=qpc_originalcode(&currenttime);


  
  wantedspeed=speedhack_wantedspeed;

  if (qpc_initialtime==0)
  {
    qpc_initialtime=currenttime;
    qpc_initialoffset=currenttime;
  }

  newtime=(currenttime-qpc_initialtime)*qpc_speed;

  newtime=newtime+qpc_initialoffset;
  if (qpc_speed!=wantedspeed)
  {
    //the user wants to change the speed
    qpc_initialoffset=newtime;
    qpc_initialtime=currenttime;
    qpc_speed=speedhack_wantedspeed;
  }
  


  csleave(&qpc_cs); 
  

  *count=newtime;

  return result;

}
{$asm}


qpc_originalcode:
%s

qpchook_exit:
jmp qpc_returnhere

ntdll.RtlQueryPerformanceCounter:
jmp new_RtlQueryPerformanceCounter
%s

qpc_returnhere:


]],originalcode, filler)

    local result2, data2=autoAssemble(s)
  end;

  return result or result2
end



registerSpeedhackCallbacks(function() --OnActivate
  if (not isConnectedToCEServer()) and targetIsX86() then
    local result, errormsg
    
    if getAddressSafe("new_gettickcount")==nil or getAddressSafe("speedhack_wantedspeed")==nil then
      --still needs hooking
      result,errormsg=hookSpeedFunctions()
    else
      result=true
    end
        
    return true, result, errormsg
  else
    return false
  end
end,

function(speed) --OnSetSpeed(speed)
  if (not isConnectedToCEServer()) and targetIsX86() then
    local result, errormsg
    if getAddressSafe("new_gettickcount")==nil or getAddressSafe("speedhack_wantedspeed")==nil then
      print("not yet hooked yet")
      result,errormsg=hookSpeedFunctions()
      if not result then return true, false, errormsg end
    end

    writeFloat("speedhack_wantedspeed", speed)
    result=true      
    
    return true, true
  else
    return false
  end
end)


