unit KernelDebugger;

interface

uses windows,sysutils,SyncObjs, dialogs,classes,debugger,disassembler,newkernelhandler,foundcodeunit,
     tlhelp32,cefuncproc;

type
  TKDebugger=class;
  TKDebuggerThread=class(TThread)
  private
    addressfound: dword;
    currentdebuggerstate: TDebuggerstate; //when a debug event has occured this will be set to the current state
    threadlistCS: TCriticalSection;
    threadlist: array of dword;

    owner: TKDebugger;
    stepping: boolean; //is true if the previous event was a break or a single step and the user hasn't stopped single stepping yet

    procedure foundone;
    function HandleBreak: boolean;
    function HandleChangeRegister(breakreason: integer): boolean;
    function HandleFindCode: boolean;

    function getDebugReason: integer;
  public
    active: boolean;

    procedure execute; override;
    constructor create(owner: TKDebugger; suspended:boolean);
//    destructor destroy;  override;
  end;

  TBreakOption = (bo_Break=0, bo_ChangeRegister=1, bo_FindCode=2);
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
    end;

    generaldebugregistercontext: TContext;
    fGlobalDebug: boolean;
    procedure setGlobalDebug(x: boolean);
  public
    procedure AddThread(ThreadID: Dword);
    procedure ApplyDebugRegistersForThread(threadhandle: DWORD);
    procedure ApplyDebugRegisters;
    procedure StartDebugger;
    procedure StopDebugger;
    procedure SetBreakpoint(address: dword; BreakType: TBreakType; BreakLength: integer; BreakOption: TBreakOption=bo_break); overload;
    procedure SetBreakpoint(address: dword; BreakType: TBreakType; BreakLength: TBreakLength; BreakOption: TBreakOption=bo_break); overload;

    procedure DisableBreakpoint(bp: integer);
    procedure DisableAllBreakpoints;

    function isActive: boolean;
    property GlobalDebug: boolean read fGlobalDebug write setGlobalDebug;
    constructor create;
  end;
  
var KDebugger: TKDebugger;

implementation

uses frmProcessWatcherUnit,memorybrowserformunit;

Procedure TKDebugger.StartDebugger;
begin
  if processid=0 then raise exception.Create('Please open a process first');
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

procedure TKDebugger.SetBreakpoint(address: dword; BreakType: TBreakType; BreakLength: integer; BreakOption: TBreakOption=bo_Break);
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
        SetBreakpoint(address, BreakType, bl_1byte, BreakOption);
        inc(address,1);
        dec(BreakLength,1);
      end;

      if (breaklength=2) or (address mod 4 > 0) then
      begin
        atleastone:=true;
        SetBreakpoint(address, BreakType, bl_2byte, BreakOption);
        inc(address,2);
        dec(BreakLength,2);
      end;

      if (breaklength=4) then
      begin
        atleastone:=true;
        SetBreakpoint(address, BreakType, bl_4byte, BreakOption);
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

procedure TKDebugger.SetBreakpoint(address: dword; BreakType: TBreakType; BreakLength: TBreakLength; BreakOption: TBreakOption=bo_break);
//only call this from the main thread
var debugreg: integer;
    i: integer;
begin
  OutputDebugString('SetBreakpoint predefinedlength');

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
    DBKDebug_GD_SetBreakpoint(true,debugreg,address,breaktype,breaklength);
  end
  else
  begin
    //manually set the breakpoints in the global debug register context
    OutputDebugString('fGlobalDebug=false, Setting breakpoint manually');

    generaldebugregistercontext.Dr7:=generaldebugregistercontext.Dr7 and (not ((1 shl debugreg) or (3 shl 16+debugreg*2))) or (integer(breaktype) shl debugreg) or (integeR(breaklength) shl 16+debugreg*2);
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
    generaldebugregistercontext.Dr7:=generaldebugregistercontext.Dr7 and (not ((1 shl bp) or (3 shl 16+bp*2)));

    case bp of
      0: generaldebugregistercontext.Dr0:=0;
      1: generaldebugregistercontext.Dr1:=0;
      2: generaldebugregistercontext.Dr2:=0;
      3: generaldebugregistercontext.Dr3:=0;
    end;
    ApplyDebugRegisters;
  end;
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
  OutputDebugString(format('Calling TKDebugger.ApplyDebugRegistersForThread(%x)',[threadhandle]));
  if not globaldebug then
  begin
    Debuggerthread.threadlistCS.Enter;
    try
      if not setthreadcontext(threadhandle, generaldebugregistercontext) then
        OutputDebugString(format('Failed setting debug registers on thread %x with error %d',[threadhandle, GetLastError]));
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
  LoadDBK32;
  fGlobalDebug:=x;
  if x then
    OutputDebugString('setGlobalDebug(true)')
  else
    OutputDebugString('setGlobalDebug(false)');

  
  if assigned(newkernelhandler.DBKDebug_SetGlobalDebugState) then
    DBKDebug_SetGlobalDebugState(x);
end;

function TKDebugger.isActive: boolean;
begin
  result:=DebuggerThread <> nil;
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



procedure TKDebuggerThread.foundone;
var desc,opcode: string;
    address: dword;
begin
  with foundcodedialog do
  begin
    address:=addressfound;
    opcode:=disassemble(address,desc);

    setlength(coderecords,length(coderecords)+1);
    coderecords[length(coderecords)-1].address:=addressfound;
    coderecords[length(coderecords)-1].size:=address-addressfound;
    coderecords[length(coderecords)-1].opcode:=opcode;
    coderecords[length(coderecords)-1].description:=desc;

    coderecords[length(coderecords)-1].eax:=currentdebuggerstate.EAX;
    coderecords[length(coderecords)-1].ebx:=currentdebuggerstate.EBX;
    coderecords[length(coderecords)-1].ecx:=currentdebuggerstate.ECX;
    coderecords[length(coderecords)-1].edx:=currentdebuggerstate.EDX;
    coderecords[length(coderecords)-1].esi:=currentdebuggerstate.Esi;
    coderecords[length(coderecords)-1].edi:=currentdebuggerstate.Edi;
    coderecords[length(coderecords)-1].ebp:=currentdebuggerstate.Ebp;
    coderecords[length(coderecords)-1].esp:=currentdebuggerstate.Esp;
    coderecords[length(coderecords)-1].eip:=currentdebuggerstate.Eip;
    Foundcodelist.Items.Add(opcode);
  end;
end;


function TKDebuggerThread.getDebugReason: integer;
//breakreason -2 = error
//breakreason -1 = single step
//breakreason x = used breakpoint
var i,j: integer;
    bsize: integer;
    address: dword;
begin
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
    OutputDebugString('Result = -2');
    //single step then ?
    if getbit(14,currentdebuggerstate.dr6)=1 then
    begin
      //yes, it's a single step
      //is the user single stepping ?
      if Stepping then
        result:=-1;
    end;
  end;
end;

function TKDebuggerThread.HandleBreak:boolean;
begin
  OutputDebugString('HandleBreak');

  result:=true; //unless the single step isn't intended

  //update gui state
  //sleep until the user sets the continue event

  stepping:=true; //set stepping to false if the user continues with the state set to continue

  //continue debugged thread

end;

function TKDebuggerThread.HandleChangeRegister(breakreason: integer): boolean;
begin
  OutputDebugString('HandleChangeRegister');
  result:=true;
  //apply update according to the given breakpoint
  //continue debugged thread
end;

function TKDebuggerThread.HandleFindCode: boolean;
var
  i: integer;
  temp: dword;
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
      if (currentdebuggerstate.ecx=0) then addressfound:=previousopcode(currentdebuggerstate.eip);
    end
    else
      addressfound:=previousopcode(currentdebuggerstate.eip);


    for i:=0 to length(foundcodedialog.coderecords)-1 do
      if (foundcodedialog.coderecords[i].address=addressfound) then exit; //already in the list, handled, and continue

    //still here so not in the list
    synchronize(foundone);

  end;
  //else no handler, let's try to continue...

end;

procedure TKDebuggerThread.execute;
var
  breakreason: integer;
  breakoption: TBreakOption;

begin
  active:=true;
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
            breakoption:=owner.breakpoint[breakreason].BreakOption;
            owner.breakpointCS.Leave;
          end;

          case breakoption of
            bo_break:           DBKDebug_ContinueDebugEvent(HandleBreak);
            bo_ChangeRegister:  DBKDebug_ContinueDebugEvent(HandleChangeRegister(breakreason));
            bo_FindCode:        DBKDebug_ContinueDebugEvent(HandleFindCode);
            else
            begin
              OutputDebugString('Invalid breakoption');
              DBKDebug_ContinueDebugEvent(false); //weird bug if it happens...
            end;
          end;

        end
        else DBKDebug_ContinueDebugEvent(false);  //not handled
      end;
      //timeout

    end;

    //terminated

  except

  end;

  crdebugging.Enter;
{
  //disable the debugregs
  zeromemory(@debugregs,sizeof(debugregs));
  debugregs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
  debugregs.Dr7:=reg0set or reg1set or reg2set or reg3set;
  for i:=0 to length(threadlist)-1 do
  begin
    suspendthread(threadlist[i]);
    SetThreadContext(threadlist[i],Debugregs);
    resumethread(threadlist[i]);
  end;
         }
  //tell the kerneldriver to whipe out the debuggeerdprocesslist
  DBKDebug_Stopdebugging;
  
  crdebugging.Leave;
  active:=false;
end;

initialization
  KDebugger:=TKDebugger.create;


end.
