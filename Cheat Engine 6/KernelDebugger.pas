unit KernelDebugger;

{$MODE Delphi}

interface

uses jwawindows, windows, LCLIntf, classes, sysutils, syncobjs,dialogs,{classes,}CEDebugger,disassembler,NewKernelHandler,FoundCodeUnit,
     {tlhelp32,}ComCtrls,addressparser, graphics, CEFuncProc, debughelper, debuggertypedefinitions;

type

  TKDebugger=class;
  TKDebuggerThread=class(TThread)
  private
    addressfound: ptrUint;
    currentdebuggerstate: TDebuggerstate; //when a debug event has occured this will be set to the current state
    threadlistCS: TCriticalSection;
    threadlist: array of THandle;

    owner: TKDebugger;
    stepping: boolean; //is true if the previous event was a break or a single step and the user hasn't stopped single stepping yet

    tempaddressspecifier: string;

    continueEvent: Tevent;
    continueOption: TcontinueOption;
    runtilladdress: ptrUint;

    procedure ConvertDebuggerStateToContext(debuggerstate: TDebuggerstate; var context: _CONTEXT);

    //mainthread:
    procedure foundone;
    procedure AddToChangesList;
    procedure UpdateGui;

    //thread:
    function HandleBreak:boolean;
    function HandleChangeRegister(breakreason: integer): boolean;
    function HandleFindCode: boolean;
    function HandleFindWhatCodeAccesses: boolean;

    function getDebugReason: integer;


  public
    active: boolean;
    tempcontext: _CONTEXT;

    procedure Continue(continueOption: TContinueOption; runtilladdress: ptrUint=0);
    procedure execute; override;
    constructor create(owner: TKDebugger; suspended:boolean);
//    destructor destroy;  override;
  end;


  TKDebugger=class
  private
    DebuggerThread: TKDebuggerThread;

    breakpointCS: TCriticalSection;
    breakpoint: array [0..3] of record
      active: boolean;
      Address: DWORD;
      BreakType: TBreakType;
      BreakLength: TBreakLength;
      BreakOption: TBreakOption;
      ChangeRegisterData: TRegistermodificationBP;
      BreakOnce: boolean; //if set it gets deleted when broken on once
      ThreadID: dword; //threadid determines wwhich thread it should break on.
    end;

    generaldebugregistercontext: TContext;
    fGlobalDebug: boolean;
    procedure setGlobalDebug(x: boolean);
    function breaklengthToByteLength(breakLength: TBreakLength): integer;
    function getNumberOfBreakpoints: integer;
  public
    procedure AddThread(ThreadID: Dword);
    procedure ApplyDebugRegistersForThread(threadhandle: DWORD);
    procedure ApplyDebugRegisters;
    procedure StartDebugger;
    procedure StopDebugger;
    procedure SetBreakpoint(address: ptrUint; BreakType: TBreakType; BreakLength: integer; BreakOption: TBreakOption=bo_break; ChangeReg: PRegistermodificationBP=nil; threadid: dword=0; breakOnce: boolean=false); overload;
    procedure SetBreakpoint(address: ptrUint; BreakType: TBreakType; BreakLength: TBreakLength; BreakOption: TBreakOption=bo_break; ChangeReg: PRegistermodificationBP=nil; threadid: dword=0; breakOnce: boolean=false); overload;

    procedure DisableBreakpoint(bp: integer);
    procedure DisableAllBreakpoints;


    procedure GetContext(var context: _CONTEXT);
    procedure Continue(continueOption: TContinueOption; runtilladdress: ptrUint=0);

    //address specific toggle bp
    procedure ToggleBreakpoint(address: ptrUint);

    function isActive: boolean;

    function isExecutableBreakpoint(a: ptrUint): boolean;
    property GlobalDebug: boolean read fGlobalDebug write setGlobalDebug;
    constructor create;
    property nrofbreakpoints: integer read getNumberOfBreakpoints;
  end;

var KDebugger: TKDebugger;

implementation

uses frmProcessWatcherUnit,formchangedaddresses,memorybrowserformunit, frmstacktraceunit;



Procedure TKDebugger.StartDebugger;
begin
  if processid=0 then raise exception.Create('Please open a process first');

  if not loaddbvmifneeded then raise exception.Create('You can''t currently use the kernel debugger');


  if Debuggerthread=nil then
    Debuggerthread:=TKDebuggerThread.create(self,false);
end;

Procedure TKDebugger.StopDebugger;
begin
  if (DebuggerThread<>nil) then
  begin
    Debuggerthread.Terminate;
    Debuggerthread.WaitFor;
    FreeAndNil(Debuggerthread);
  end;
end;

function TKDebugger.breaklengthToByteLength(breakLength: TBreakLength): integer;
begin
  case breaklength of
    bl_1byte: result:=1;
    bl_2byte: result:=2;
    bl_4byte: result:=4;
    bl_8byte: result:=8;
    else result:=1; //(error)
  end;

end;

function TKDebugger.isExecutableBreakpoint(a: ptrUint): boolean;
var i: integer;
begin
  result:=false;
  breakpointcs.enter;
  try
    for i:=0 to 3 do
      if breakpoint[i].active then
      begin
        if (breakpoint[i].BreakType=bt_OnInstruction) and (breakpoint[i].address=a) then
        begin
          result:=true;
          exit;
        end;

      end;

  finally
    breakpointcs.leave;
  end;

end;

function TKDebugger.getNumberOfBreakpoints: integer;
var i: integer;
begin
  result:=0;
  breakpointcs.enter;
  for i:=0 to 3 do
    if breakpoint[i].active then
      inc(result);

  breakpointcs.leave;
end;

procedure TKDebugger.SetBreakpoint(address: ptrUint; BreakType: TBreakType; BreakLength: integer; BreakOption: TBreakOption=bo_Break; ChangeReg: PRegistermodificationBP=nil; threadid: dword=0; breakOnce: boolean=false);
//split up into seperate SetBreakpoint calls
var atleastone: boolean;
begin
  OutputDebugString(format('SetBreakpoint integerlength (%d)',[breaklength]));
  if not isactive then
    StartDebugger;

  atleastone:=false;
  try
    while (breaklength>0) do
    begin
      Outputdebugstring(format('address=%x breaklength=%d',[address,breaklength]));
      if (breaklength=1) or (address mod 2 > 0) then
      begin
        atleastone:=true;
        SetBreakpoint(address, BreakType, bl_1byte, BreakOption, ChangeReg, threadid, breakonce);
        inc(address,1);
        dec(BreakLength,1);
      end
      else
      if (breaklength=2) or (address mod 4 > 0) then
      begin
        atleastone:=true;
        SetBreakpoint(address, BreakType, bl_2byte, BreakOption, ChangeReg, threadid, breakonce);
        inc(address,2);
        dec(BreakLength,2);
      end else
      if (breaklength>=4) then
      begin
        atleastone:=true;
        SetBreakpoint(address, BreakType, bl_4byte, BreakOption, ChangeReg, threadid, breakonce);
        inc(address,4);
        dec(breaklength,4);
      end;
    end;
  except
    on e:Exception do
      if not atleastone then
        raise e;
  end;

end;

procedure TKDebugger.SetBreakpoint(address: ptrUint; BreakType: TBreakType; BreakLength: TBreakLength; BreakOption: TBreakOption=bo_break; ChangeReg: PRegistermodificationBP=nil; threadid: dword=0; breakOnce: boolean=false);
//only call this from the main thread
var debugreg: integer;
    i: integer;
begin
  OutputDebugString('SetBreakpoint predefinedlength for address:'+inttohex(address,8));

  //find a debugreg spot not used yet
  if not isactive then
    StartDebugger;
    
  debugreg:=-1;

  
  breakpointCS.Enter;
  for i:=0 to 3 do
    if not breakpoint[i].active then
    begin
      breakpoint[i].Address:=address;
      breakpoint[i].BreakType:=BreakType;
      breakpoint[i].BreakLength:=BreakLength;
      breakpoint[i].BreakOption:=breakoption;
      if changereg<>nil then
        breakpoint[i].ChangeRegisterData:=changereg^;

      breakpoint[i].threadid:=threadid;
      breakpoint[i].BreakOnce:=breakonce;
      breakpoint[i].active:=true;
      

      outputdebugstring(format('using debug reg %d with BreakOption %d',[i,integer(breakoption)]));
      debugreg:=i;
      break;
    end;
  breakpointCS.Leave;


  if debugreg=-1 then
    raise exception.Create('Out of debug registers');

  //apply the breakpoint
  if fGlobalDebug then
  begin
    //don't set the debugregs manually, let the taskswitching do the work for us
    //(for global debug the debugreg is just a recommendation, so don't watch for a dr6 result with this exit)\
    OutputDebugString('fGlobalDebug=true, Setting breakpoint using global debug');
    outputdebugstring('extra:');
    OutputDebugString(format('DBKDebug_GD_SetBreakpoint(true, %d, %x, %d, %d)',[debugreg,address, integer(breaktype),integer(breaklength)]));
    DBKDebug_GD_SetBreakpoint(true,debugreg,address,breaktype,breaklength);
  end
  else
  begin
    //manually set the breakpoints in the global debug register context
    OutputDebugString('fGlobalDebug=false, Setting breakpoint manually');
    OutputDebugString(pchar(format('debugreg=%d breaktype=%d breaklength=%d',[debugreg, integer(breaktype), integer(breaklength)])));

    generaldebugregistercontext.Dr7:=generaldebugregistercontext.Dr7 or (1 shl 10); //just to make it look nicer set the flag that will always be 1

    //unset the bits for the given debugreg
    generaldebugregistercontext.Dr7:=generaldebugregistercontext.Dr7 and (not ((1 shl (debugreg*2)) or ($f shl 16+debugreg*2))); // disable local bp and unset the 4 bits for LEN and R/W 

    //set the specific bits
    generaldebugregistercontext.Dr7:=generaldebugregistercontext.Dr7 or (integer(breaklength) shl (16+debugreg*2+2)) or (integer(breaktype) shl (16+debugreg*2));

    //enable local breakpoint
    generaldebugregistercontext.Dr7:=generaldebugregistercontext.Dr7 or (1 shl (debugreg*2));
    OutputDebugString(pchar(format('Setting DR7 to %x',[generaldebugregistercontext.Dr7])));

    case debugreg of
      0: generaldebugregistercontext.Dr0:=address;
      1: generaldebugregistercontext.Dr1:=address;
      2: generaldebugregistercontext.Dr2:=address;
      3: generaldebugregistercontext.Dr3:=address;
    end;

    //and apply
    ApplyDebugRegisters;
  end;
end;

procedure TKDebugger.ToggleBreakpoint(address: ptrUint);
var i: integer;
    found :boolean;
begin
  outputdebugstring(format('ToggleBreakpoint(%x)',[address]));
  found:=false;
  breakpointcs.Enter;
  for i:=0 to 3 do
  begin
    //only affect breakpoints of type instruction
    if (breakpoint[i].active) and (breakpoint[i].Address=address) and (breakpoint[i].BreakType=bt_onInstruction) then
    begin
      outputdebugstring('Found breakpoint. Disabling it');
      //found, so disable
      DisableBreakpoint(i);
      found:=true; //let's not exit just yet...
    end;
  end;

  breakpointcs.Leave;

  if not found then
  begin
    outputdebugstring(format('Not found. Calling SetBreakpoint(%x, bt_OnInstruction, bl_1byte, bo_Break)',[address]));
    SetBreakpoint(address, bt_OnInstruction, bl_1byte, bo_Break);
  end;
end;

procedure TKDebugger.DisableAllBreakpoints;
var i: integer;
begin
  for i:=0 to 3 do
    DisableBreakpoint(i);
end;

procedure TKDebugger.DisableBreakpoint(bp: integer);
begin
  if fGlobalDebug then
  begin
    DBKDebug_GD_SetBreakpoint(false, bp, 0, bt_OnInstruction, bl_1byte);
  end
  else
  begin
    generaldebugregistercontext.Dr7:=generaldebugregistercontext.Dr7 and (not ((1 shl bp) or ($f shl 16+bp*2)));

    case bp of
      0: generaldebugregistercontext.Dr0:=0;
      1: generaldebugregistercontext.Dr1:=0;
      2: generaldebugregistercontext.Dr2:=0;
      3: generaldebugregistercontext.Dr3:=0;
    end;
    ApplyDebugRegisters;
  end;
  breakpoint[bp].active:=false;
end;

procedure TKDebugger.AddThread(ThreadID: Dword);
var Threadhandle: thandle;
begin
  if not GlobalDebug then
  begin
    Debuggerthread.threadlistCS.Enter;
    try
      setlength(Debuggerthread.threadlist,length(Debuggerthread.threadlist)+1);
      threadhandle:=Openthread(STANDARD_RIGHTS_REQUIRED or windows.synchronize or $3ff,true,ThreadID);
      Debuggerthread.threadlist[length(Debuggerthread.threadlist)-1]:=threadhandle;
      ApplyDebugRegistersForThread(threadhandle);
    finally
      Debuggerthread.threadlistCS.Leave;
    end;
  end;
end;

procedure TKDebugger.ApplyDebugRegistersForThread(threadhandle: DWORD);
begin
  OutputDebugString(format('Calling TKDebugger.ApplyDebugRegistersForThread with handle %x',[threadhandle]));
  if not globaldebug then
  begin
    Debuggerthread.threadlistCS.Enter;
    try
      if debuggerthread.stepping then
      begin
        OutputDebugString('debuggerthread.stepping is TRUE');
        debuggerthread.currentdebuggerstate.dr0:=generaldebugregistercontext.Dr0;
        debuggerthread.currentdebuggerstate.dr1:=generaldebugregistercontext.Dr1;
        debuggerthread.currentdebuggerstate.dr2:=generaldebugregistercontext.Dr2;
        debuggerthread.currentdebuggerstate.dr3:=generaldebugregistercontext.Dr3;
        debuggerthread.currentdebuggerstate.dr6:=generaldebugregistercontext.Dr6;
        debuggerthread.currentdebuggerstate.dr7:=generaldebugregistercontext.Dr7;
      end
      else
      begin
        OutputDebugString('debuggerthread.stepping is FALSE');      
        if not setthreadcontext(threadhandle, generaldebugregistercontext) then
          OutputDebugString(format('Failed setting debug registers on thread %x with error %d',[threadhandle, GetLastError]));
      end;
    finally
      Debuggerthread.threadlistCS.Leave;
    end;
  end;
end;

procedure TKDebugger.ApplyDebugRegisters;
var i: integer;
begin
  if not globaldebug then
  begin
    Debuggerthread.threadlistCS.Enter;
    try
      for i:=0 to length(Debuggerthread.threadlist)-1 do
        ApplyDebugRegistersForThread(Debuggerthread.threadlist[i]);
    finally
      Debuggerthread.threadlistCS.Leave;
    end;
  end;
end;

procedure TKDebugger.setGlobalDebug(x: boolean);
begin
  fGlobalDebug:=x;

  if isActive then
  begin
    LoadDBK32;
    if x then
      OutputDebugString('setGlobalDebug(true)')
    else
      OutputDebugString('setGlobalDebug(false)');


    if assigned(newkernelhandler.DBKDebug_SetGlobalDebugState) then
      DBKDebug_SetGlobalDebugState(x);
  end;
end;

function TKDebugger.isActive: boolean;
begin
  result:=DebuggerThread <> nil;
end;

procedure TKDebugger.GetContext(var context: _CONTEXT);
begin
  if DebuggerThread<>nil then
    context:=DebuggerThread.tempcontext;
end;

procedure TKDebugger.Continue(continueOption: TContinueOption; runtilladdress: ptrUint=0);
begin
  if debuggerthread<>nil then
    debuggerthread.Continue(continueoption, runtilladdress);
end;

constructor TKDebugger.create;
begin
  breakpointCS:=TCriticalSection.Create;
  generaldebugregistercontext.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
  
end;

//---------------------------------------

constructor TKDebuggerThread.create(owner: TKDebugger; suspended:boolean);
var ths: thandle;
    tE: threadentry32;
    i,j: integer;
    found: boolean;
    temp: thandle;
begin
  OutputDebugString('TKDebuggerThread.create');
  continueEvent:=Tevent.Create(nil, false, false, '');

  DBKDebug_StartDebugging(ProcessID);
  active:=true;
  self.owner:=owner;

  threadlistCS:=TCriticalSection.Create;
  inherited create(true);


  if not owner.GlobalDebug then
  begin

    //try to find this process in the processwatch window.
    found:=false;
    for i:=0 to length(frmprocesswatcher.processes)-1 do
    begin
      if frmprocesswatcher.processes[i].processid=processid then
      begin
        //open the threads
        for j:=0 to length(frmprocesswatcher.processes[i].threadlist)-1 do
        begin

          temp:=Openthread(STANDARD_RIGHTS_REQUIRED or windows.synchronize or $3ff,true,frmprocesswatcher.processes[i].threadlist[j].threadid);
          if temp<>0 then
          begin        
            setlength(threadlist,length(threadlist)+1);
            threadlist[length(threadlist)-1]:=temp;
          end;
        end;

        found:=true;
        break;

      end;
    end;

    if not found then
    begin
      //if it wasn't found try to add it (and tell the user it's best to start the process after ce has started)
      ths:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,processid);
      try
        if ths<>0 then
        begin
          te.dwSize:=sizeof(te);
          if Thread32First(ths,te) then
          begin
            repeat
              if te.th32OwnerProcessID=processid then
              begin
                setlength(threadlist,length(threadlist)+1);
                threadlist[length(threadlist)-1]:=Openthread(STANDARD_RIGHTS_REQUIRED or windows.synchronize or $3ff,true,te.th32ThreadID);
              end;
            until not thread32Next(ths,te);
          end;
        end;
      finally
        closehandle(ths);
      end;
    end;
  end;

  if not suspended then resume;
end;

procedure TKDebuggerThread.ConvertDebuggerStateToContext(debuggerstate: TDebuggerstate; var context: _CONTEXT);
begin
  (*
  ZeroMemory(@context,sizeof(_CONTEXT));
  context.Eax:=debuggerstate.Eax;
  context.Ebx:=debuggerstate.Ebx;
  context.Ecx:=debuggerstate.Ecx;
  context.Edx:=debuggerstate.Edx;
  context.Esi:=debuggerstate.Esi;
  context.Edi:=debuggerstate.Esi;
  context.Ebp:=debuggerstate.Ebp;
  context.Esp:=debuggerstate.Esp;
  context.Eip:=debuggerstate.Eip;
  context.Dr0:=debuggerstate.dr0;
  context.Dr1:=debuggerstate.dr1;
  context.Dr2:=debuggerstate.dr2;
  context.Dr3:=debuggerstate.dr3;
  context.Dr6:=debuggerstate.dr6;
  context.Dr7:=debuggerstate.dr7;  *)
end;

procedure TKDebuggerThread.Continue(continueOption: TContinueOption; runtilladdress: ptrUint=0);
begin
  self.continueOption:=continueOption;
  self.runtilladdress:=runtilladdress;
  continueEvent.SetEvent;
end;

procedure TKDebuggerThread.UpdateGui;
var
  temp: string;
begin
  (*
  with memorybrowser do
  begin
    //enable debug mode
    run1.Enabled:=true;
    step1.Enabled:=true;
    stepover1.Enabled:=true;
    runtill1.Enabled:=true;
    stacktrace1.Enabled:=true;
    Executetillreturn1.Enabled:=true;
    if stepping then
      caption:='Memory Viewer - Currently debugging thread'
    else
      caption:='Memory Viewer - Running...';

    if frmstacktrace<>nil then
    begin
      ConvertDebuggerStateToContext(currentdebuggerstate, tempcontext);
      frmstacktrace.stacktrace(currentdebuggerstate.threadid,tempcontext);
    end;

    disassemblerview.SelectedAddress:=currentdebuggerstate.Eip;

    temp:='EAX '+IntToHex(currentdebuggerstate.Eax,8);
    if temp<>eaxlabel.Caption then
    begin
      eaxlabel.Font.Color:=clred;
      eaxlabel.Caption:=temp;
    end else eaxlabel.Font.Color:=clWindowText;

    temp:='EBX '+IntToHex(currentdebuggerstate.Ebx,8);
    if temp<>ebxlabel.Caption then
    begin
      ebxlabel.Font.Color:=clred;
      ebxlabel.Caption:=temp;
    end else ebxlabel.Font.Color:=clWindowText;

    temp:='ECX '+IntToHex(currentdebuggerstate.ECx,8);
    if temp<>eCxlabel.Caption then
    begin
      eCXlabel.Font.Color:=clred;
      eCXlabel.Caption:=temp;
    end else eCXlabel.Font.Color:=clWindowText;

    temp:='EDX '+IntToHex(currentdebuggerstate.EDx,8);
    if temp<>eDxlabel.Caption then
    begin
      eDxlabel.Font.Color:=clred;
      eDxlabel.Caption:=temp;
    end else eDxlabel.Font.Color:=clWindowText;

    temp:='ESI '+IntToHex(currentdebuggerstate.ESI,8);
    if temp<>eSIlabel.Caption then
    begin
      eSIlabel.Font.Color:=clred;
      eSIlabel.Caption:=temp;
    end else eSIlabel.Font.Color:=clWindowText;

    temp:='EDI '+IntToHex(currentdebuggerstate.EDI,8);
    if temp<>eDIlabel.Caption then
    begin
      eDIlabel.Font.Color:=clred;
      eDIlabel.Caption:=temp;
    end else eDIlabel.Font.Color:=clWindowText;

    temp:='EBP '+IntToHex(currentdebuggerstate.EBP,8);
    if temp<>eBPlabel.Caption then
    begin
      eBPlabel.Font.Color:=clred;
      eBPlabel.Caption:=temp;
    end else eBPlabel.Font.Color:=clWindowText;

    temp:='ESP '+IntToHex(currentdebuggerstate.ESP,8);
    if temp<>eSPlabel.Caption then
    begin
      eSPlabel.Font.Color:=clred;
      eSPlabel.Caption:=temp;
    end else eSPlabel.Font.Color:=clWindowText;

    temp:='EIP '+IntToHex(currentdebuggerstate.EIP,8);
    if temp<>eIPlabel.Caption then
    begin
      eIPlabel.Font.Color:=clred;
      eIPlabel.Caption:=temp;
    end else eIPlabel.Font.Color:=clWindowText;

    temp:='CS '+IntToHex(currentdebuggerstate.cs,4);
    if temp<>CSlabel.Caption then
    begin
      CSlabel.Font.Color:=clred;
      CSlabel.Caption:=temp;
    end else CSlabel.Font.Color:=clWindowText;

    temp:='DS '+IntToHex(currentdebuggerstate.ds,4);
    if temp<>DSlabel.Caption then
    begin
      DSlabel.Font.Color:=clred;
      DSlabel.Caption:=temp;
    end else DSLabel.Font.Color:=clWindowText;

    temp:='SS '+IntToHex(currentdebuggerstate.ss,4);
    if temp<>SSlabel.Caption then
    begin
      SSlabel.Font.Color:=clred;
      SSlabel.Caption:=temp;
    end else SSlabel.Font.Color:=clWindowText;

    temp:='ES '+IntToHex(currentdebuggerstate.es,4);
    if temp<>ESlabel.Caption then
    begin
      ESlabel.Font.Color:=clred;
      ESlabel.Caption:=temp;
    end else ESlabel.Font.Color:=clWindowText;

    temp:='FS '+IntToHex(currentdebuggerstate.fs,4);
    if temp<>FSlabel.Caption then
    begin
      FSlabel.Font.Color:=clred;
      FSlabel.Caption:=temp;
    end else FSlabel.Font.Color:=clWindowText;

    temp:='GS '+IntToHex(currentdebuggerstate.gs,4);
    if temp<>GSlabel.Caption then
    begin
      GSlabel.Font.Color:=clred;
      GSlabel.Caption:=temp;
    end else GSlabel.Font.Color:=clWindowText;

    temp:='CF '+IntToStr(GetBitOf(currentdebuggerstate.EFLAgs,0));
    if temp<>cflabel.Caption then
    begin
      CFlabel.Font.Color:=clred;
      CFlabel.caption:=temp;
    end else cflabel.Font.Color:=clWindowText;

    temp:='PF '+IntToStr(GetBitOf(currentdebuggerstate.EFlags,2));
    if temp<>Pflabel.Caption then
    begin
      Pflabel.Font.Color:=clred;
      Pflabel.caption:=temp;
    end else Pflabel.Font.Color:=clWindowText;

    temp:='AF '+IntToStr(GetBitOf(currentdebuggerstate.EFlags,4));
    if temp<>Aflabel.Caption then
    begin
      Aflabel.Font.Color:=clred;
      Aflabel.caption:=temp;
    end else Aflabel.Font.Color:=clWindowText;

    temp:='ZF '+IntToStr(GetBitOf(currentdebuggerstate.EFlags,6));
    if temp<>Zflabel.Caption then
    begin
      Zflabel.Font.Color:=clred;
      Zflabel.caption:=temp;
    end else Zflabel.Font.Color:=clWindowText;

    temp:='SF '+IntToStr(GetBitOf(currentdebuggerstate.EFlags,7));
    if temp<>Sflabel.Caption then
    begin
      Sflabel.Font.Color:=clred;
      Sflabel.caption:=temp;
    end else Sflabel.Font.Color:=clWindowText;

    temp:='DF '+IntToStr(GetBitOf(currentdebuggerstate.EFlags,10));
    if temp<>Dflabel.Caption then
    begin
      Dflabel.Font.Color:=clred;
      Dflabel.caption:=temp;
    end else Dflabel.Font.Color:=clWindowText;

    temp:='OF '+IntToStr(GetBitOf(currentdebuggerstate.EFlags,11));
    if temp<>Oflabel.Caption then
    begin
      Oflabel.Font.Color:=clred;
      Oflabel.caption:=temp;
    end else Oflabel.Font.Color:=clWindowText;


    ConvertDebuggerStateToContext(currentdebuggerstate, lastdebugcontext);
 {
    EAXv:=currentdebuggerstate.Eax;
    EBXv:=currentdebuggerstate.Ebx;
    ECXv:=currentdebuggerstate.Ecx;
    EDXv:=currentdebuggerstate.Edx;
    ESIv:=currentdebuggerstate.ESi;
    EDIv:=currentdebuggerstate.Edi;
    EBPv:=currentdebuggerstate.Ebp;
    ESPv:=currentdebuggerstate.Esp;
    EIPv:=currentdebuggerstate.Eip;  }

    showDebugPanels:=true;
    reloadStacktrace;
  end;*)
end;

procedure TKDebuggerThread.AddToChangesList;
var i: integer;
    lbs: string;
    newitem: TListItem;
    x: PContext;
    bpa: ptrUint;
begin
   (*try
    bpa:=getaddress(tempaddressspecifier);
    lbs:=inttohex(bpa,8);
    for i:=0 to frmchangedaddresses.Changedlist.Items.Count-1 do
      if frmchangedaddresses.Changedlist.Items[i].Caption=lbs then exit;

    newitem:=frmchangedaddresses.Changedlist.Items.Add;
    getmem(x,sizeof(_CONTEXT));
    ConvertDebuggerStateToContext(currentdebuggerstate, x^);
    newitem.Data:=x;
    newitem.Caption:=lbs;

    //enable the timer if needed, there's data to be handled
    if not frmchangedaddresses.Timer1.Enabled then
      frmchangedaddresses.Timer1.Enabled:=true;
  except
    //
  end;
*)
end;

procedure TKDebuggerThread.foundone;
var desc,opcode: string;
    address: ptrUint;
    coderecord: Tcoderecord;
begin
  with foundcodedialog do
  begin
    address:=addressfound;
    opcode:=disassemble(address,desc);

    coderecord:=TCoderecord.create;

    coderecord.address:=addressfound;
    coderecord.size:=address-addressfound;
    coderecord.opcode:=opcode;
    coderecord.description:=desc;

    coderecord.eax:=currentdebuggerstate.EAX;
    coderecord.ebx:=currentdebuggerstate.EBX;
    coderecord.ecx:=currentdebuggerstate.ECX;
    coderecord.edx:=currentdebuggerstate.EDX;
    coderecord.esi:=currentdebuggerstate.Esi;
    coderecord.edi:=currentdebuggerstate.Edi;
    coderecord.ebp:=currentdebuggerstate.Ebp;
    coderecord.esp:=currentdebuggerstate.Esp;
    coderecord.eip:=currentdebuggerstate.Eip;
    coderecord.context.ContextFlags:=0;
    Foundcodelist.Items.AddObject(opcode,coderecord);
  end;
end;

function TKDebuggerThread.getDebugReason: integer;
//breakreason -2 = error
//breakreason -1 = single step
//breakreason x = used breakpoint
var i,j: integer;
    bsize: integer;
    address: ptrUint;
begin

  outputdebugstring('EIP='+inttohex(currentdebuggerstate.eip,8));
  outputdebugstring('DR6='+inttohex(currentdebuggerstate.dr6,8));
  outputdebugstring('DR7='+inttohex(currentdebuggerstate.dr7,8));
    
  result:=-2;
  owner.breakpointCS.Enter;
  try
    for i:=0 to 3 do
    begin
      if getbit(i, currentdebuggerstate.dr6)=1 then
      begin
        //find which debug breakpoint it actually is, and that it didn't get overwritten
        outputdebugstring(format('bit %d in DR6 is 1',[i]));

        case i of
          0: address:=currentdebuggerstate.dr0;
          1: address:=currentdebuggerstate.dr1;
          2: address:=currentdebuggerstate.dr2;
          3: address:=currentdebuggerstate.dr3;
        end;

        for j:=0 to 3 do
        begin
          if owner.breakpoint[j].active then
          begin
            outputdebugstring(format('bp %d: is %x the same as %x ?',[j, owner.breakpoint[j].address, address]));


            if owner.breakpoint[j].address=address then
            begin
              outputdebugstring('yes it is');
              result:=j;
              exit;
            end else outputdebugstring('nope');
          end
          else
            outputdebugstring(format('breakpoint %d is not active',[j]));
        end;
      end
      else
        outputdebugstring(format('bit %d in DR6 is 0',[i]));
    end;
  finally
    owner.breakpointCS.Leave;
  end;

  if result=-2 then
  begin
    OutputDebugString('Result = -2, is it a single step?');
    //single step then ?
    if getbit(14,currentdebuggerstate.dr6)=1 then //check if single step bit is 1
    begin
      OutputDebugString('Yes, it is a single step');
      //yes, it's a single step
      //is the user single stepping ?
      if Stepping then
      begin
        OutputDebugString('Single step while stepping is on. So break');
        result:=-1;
      end
      else
      begin
        OutputDebugString('Single step while stepping is off, caused by program itself ?');
        result:=-1; //remove this if you want to cause an exception
      end;
    end;
  end;
end;

function TKDebuggerThread.HandleBreak:boolean;
var wr: TWaitResult;
    address: ptrUint;
begin
  OutputDebugString('HandleBreak');

  outputdebugstring(format('initial eflags=%x',[currentdebuggerstate.eflags]));

  result:=true; //unless the single step isn't intended, but that check should have happened in the kernel
  stepping:=true;

  //update gui state
  synchronize(updategui);

  //sleep until the user sets the continue event
  continueEvent.ResetEvent; //make sure it's unset so unwanted usercommands don't cause a continue
  continueEvent.WaitFor(INFINITE);

  case continueoption of
    co_run:
    begin
      OutputDebugString('run');
      currentdebuggerstate.eflags:=eflags_setRF(currentdebuggerstate.eflags,1); //skip current instruction bp
      currentdebuggerstate.eflags:=eflags_setTF(currentdebuggerstate.eflags,0);
      stepping:=false;
    end;

    co_stepinto:
    begin
      OutputDebugString('step into');
      currentdebuggerstate.eflags:=eflags_setRF(currentdebuggerstate.eflags,1); //skip current instruction bp
      currentdebuggerstate.eflags:=eflags_setTF(currentdebuggerstate.eflags,1); //trap to execute on next instruction
      stepping:=true;
    end;

    co_stepover:
    begin
      OutputDebugString('step over. Stepping='+booltostr(stepping,true));
      currentdebuggerstate.eflags:=eflags_setRF(currentdebuggerstate.eflags,1); //skip current instruction bp
      currentdebuggerstate.eflags:=eflags_setTF(currentdebuggerstate.eflags,0);
      //find next instruction address
      address:=currentdebuggerstate.eip;
      disassemble(address);

      //set breakpoint here. (one time only bp for this specific threadid)
      KDebugger.SetBreakpoint(address, bt_OnInstruction, 1, bo_Break, nil, currentdebuggerstate.threadid, true);
      stepping:=false;
    end;

    co_runtill:
    begin
      OutputDebugString('run till');    
      currentdebuggerstate.eflags:=eflags_setRF(currentdebuggerstate.eflags,1); //skip current instruction bp
      currentdebuggerstate.eflags:=eflags_setTF(currentdebuggerstate.eflags,0);
      OutputDebugString('Setting breakpoint to '+inttohex(runtilladdress,8));
      KDebugger.SetBreakpoint(runtilladdress, bt_OnInstruction, 1, bo_Break, nil, currentdebuggerstate.threadid, true);
      stepping:=false;
    end;

  end;

  DBKDebug_SetDebuggerState(@currentdebuggerstate);

  //continue debugged thread

end;

function TKDebuggerThread.HandleChangeRegister(breakreason: integer): boolean;
begin
  OutputDebugString('HandleChangeRegister');

  owner.breakpointCS.Enter;
  if owner.breakpoint[breakreason].ChangeRegisterData.change_eax then currentdebuggerstate.eax:=owner.breakpoint[breakreason].ChangeRegisterData.new_eax;
  if owner.breakpoint[breakreason].ChangeRegisterData.change_ebx then currentdebuggerstate.ebx:=owner.breakpoint[breakreason].ChangeRegisterData.new_ebx;
  if owner.breakpoint[breakreason].ChangeRegisterData.change_ecx then currentdebuggerstate.ecx:=owner.breakpoint[breakreason].ChangeRegisterData.new_ecx;
  if owner.breakpoint[breakreason].ChangeRegisterData.change_edx then currentdebuggerstate.edx:=owner.breakpoint[breakreason].ChangeRegisterData.new_edx;
  if owner.breakpoint[breakreason].ChangeRegisterData.change_esi then currentdebuggerstate.esi:=owner.breakpoint[breakreason].ChangeRegisterData.new_esi;
  if owner.breakpoint[breakreason].ChangeRegisterData.change_edi then currentdebuggerstate.edi:=owner.breakpoint[breakreason].ChangeRegisterData.new_edi;
  if owner.breakpoint[breakreason].ChangeRegisterData.change_ebp then currentdebuggerstate.ebp:=owner.breakpoint[breakreason].ChangeRegisterData.new_ebp;
  if owner.breakpoint[breakreason].ChangeRegisterData.change_esp then currentdebuggerstate.esp:=owner.breakpoint[breakreason].ChangeRegisterData.new_esp;
  if owner.breakpoint[breakreason].ChangeRegisterData.change_eip then currentdebuggerstate.eip:=owner.breakpoint[breakreason].ChangeRegisterData.new_eip;

  if owner.breakpoint[breakreason].ChangeRegisterData.change_cf then currentdebuggerstate.eflags:=eflags_setCF(currentdebuggerstate.eflags, integer(owner.breakpoint[breakreason].ChangeRegisterData.new_cf));
  if owner.breakpoint[breakreason].ChangeRegisterData.change_pf then currentdebuggerstate.eflags:=eflags_setPF(currentdebuggerstate.eflags, integer(owner.breakpoint[breakreason].ChangeRegisterData.new_pf));
  if owner.breakpoint[breakreason].ChangeRegisterData.change_af then currentdebuggerstate.eflags:=eflags_setAF(currentdebuggerstate.eflags, integer(owner.breakpoint[breakreason].ChangeRegisterData.new_af));
  if owner.breakpoint[breakreason].ChangeRegisterData.change_zf then currentdebuggerstate.eflags:=eflags_setZF(currentdebuggerstate.eflags, integer(owner.breakpoint[breakreason].ChangeRegisterData.new_zf));
  if owner.breakpoint[breakreason].ChangeRegisterData.change_sf then currentdebuggerstate.eflags:=eflags_setSF(currentdebuggerstate.eflags, integer(owner.breakpoint[breakreason].ChangeRegisterData.new_sf));
  if owner.breakpoint[breakreason].ChangeRegisterData.change_of then currentdebuggerstate.eflags:=eflags_setOF(currentdebuggerstate.eflags, integer(owner.breakpoint[breakreason].ChangeRegisterData.new_of));

  owner.breakpointCS.Leave;


  //set resume flag so the next time this instruction is executed it won't break (yes, eip could be changed, but let's do it anyhow)
  currentdebuggerstate.eflags:=eflags_setRF(currentdebuggerstate.eflags,1);
  DBKDebug_SetDebuggerState(@currentdebuggerstate);
  result:=true;
end;

function TKDebuggerThread.HandleFindCode: boolean;
var
  i: integer;
//  temp: dword;
  opcode,desc: string;

begin
  //update gui with debugevent data
  //continue debugged thread
  OutputDebugString('HandleFindCode');

  result:=true;
  i:=0;
  if (foundcodedialog<>nil) then
  begin
    addressfound:=currentdebuggerstate.eip;
    opcode:=disassemble(addressfound,desc);

    if (pos('REP',opcode)=0) then
    begin
      addressfound:=previousopcode(currentdebuggerstate.eip);
    end
    else
    begin
      if (currentdebuggerstate.ecx=0) then
        addressfound:=previousopcode(currentdebuggerstate.eip)
      else
        addressfound:=currentdebuggerstate.eip;
    end;


    for i:=0 to foundcodedialog.foundcodelist.count-1 do
      if (TcodeRecord(foundcodedialog.foundcodelist.items.objects[i]).address=addressfound) then exit; //already in the list, handled, and continue


    //still here so not in the list
    synchronize(foundone);

  end;
  //else no handler, let's try to continue...

end;

function TKDebuggerThread.HandleFindWhatCodeAccesses: boolean;
//find window
//evaluate code between brackets
//store address
//continue
var opcode,desc: string;
    offset: ptrUint;
    fb,nb: integer;
begin
  offset:=currentdebuggerstate.eip;
  opcode:=disassemble(offset,desc);

  fb:=pos('[',opcode);
  if fb>0 then
    nb:=pos(']',opcode);

  if (fb>0) and (nb>0) then //instruction has brackets
  begin
    tempaddressspecifier:=copy(opcode,fb+1,nb-fb-1);
    synchronize(addtochangeslist);
  end;

  //and continue
  currentdebuggerstate.eflags:=eflags_setRF(currentdebuggerstate.eflags,1);
  DBKDebug_SetDebuggerState(@currentdebuggerstate);
  result:=true;
end;

procedure TKDebuggerThread.execute;
var
  breakreason: integer;
  breakoption: TBreakOption;
  handled: boolean;
begin
  active:=true;
  KDebugger.setGlobalDebug(KDebugger.GlobalDebug); //actually set it now

  try

    while not terminated do
    begin
      if DBKDebug_WaitForDebugEvent(1000) then
      begin
        OutputDebugString('KDebug event');

        DBKDebug_GetDebuggerState(@currentdebuggerstate);
        breakreason:=GetDebugReason;

        OutputDebugString(format('breakreason=%d',[breakreason]));

        if breakreason>=-1 then
        begin
          //it has been determined this is a break caused by ce

          breakoption:=bo_Break;
          if breakreason<>-1 then //no single step (if it is, bo_break)
          begin
            //breakpoint triggered
            //fetch bp data
            owner.breakpointcs.Enter;
            try
              breakoption:=owner.breakpoint[breakreason].BreakOption;

              if (owner.breakpoint[breakreason].ThreadID=0) or (owner.breakpoint[breakreason].ThreadID=currentdebuggerstate.threadid) then
              begin
                //delete if it belongs to this thread and it's a one time only break
                if owner.breakpoint[breakreason].BreakOnce then
                begin
                  stepping:=true;
                  KDebugger.DisableBreakpoint(breakreason);
                end;
              end
              else
              begin
                //it's a breakpoint that's not designed for this thread, skip it, but don't tell windows it happened
                OutputDebugString('Thread specific breakpoint. Break didn''t happen in target thread. Skipping breakpoint');
                currentdebuggerstate.eflags:=eflags_setRF(currentdebuggerstate.eflags,1);
                DBKDebug_SetDebuggerState(@currentdebuggerstate);
                DBKDebug_ContinueDebugEvent(true);
                System.Continue;
              end;
            finally
              owner.breakpointCS.Leave;
            end;
            
          end;



          case breakoption of
            bo_break:           handled:=HandleBreak;
            bo_ChangeRegister:  handled:=HandleChangeRegister(breakreason);
            bo_FindCode:        handled:=HandleFindCode;
            bo_FindWhatCodeAccesses: handled:=HandleFindWhatCodeAccesses;
            else
            begin
              OutputDebugString('Invalid breakoption');
              handled:=false;
            end;
          end;

          DBKDebug_ContinueDebugEvent(handled);
        end
        else DBKDebug_ContinueDebugEvent(false);  //not handled
      end;
      //timeout

    end;

    //terminated

  except

  end;

  //tell the kerneldriver to whipe out the debuggeerdprocesslist
  DBKDebug_Stopdebugging;

  active:=false;
end;

initialization
  KDebugger:=TKDebugger.create;


end.
