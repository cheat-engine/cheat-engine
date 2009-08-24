unit debug;

interface

uses windows, sysutils, dbk32functions;

type TDebuggerstate=record
	eflags : DWORD;
	eax : DWORD;
	ebx : DWORD;
	ecx : DWORD;
	edx : DWORD;
	esi : DWORD;
	edi : DWORD;
	ebp : DWORD;
	esp : DWORD;
	eip : DWORD;
	cs  : DWORD;
	ds  : DWORD;
	es  : DWORD;
	fs  : DWORD;
	gs  : DWORD;
	ss  : DWORD;
  dr0 : DWORD;
  dr1 : DWORD;
  dr2 : DWORD;
  dr3 : DWORD;
  dr6 : DWORD;
  dr7 : DWORD;
end;
type PDebuggerstate=^TDebuggerstate;

type TBreakType=(bt_OnInstruction=0,bt_OnWrites=1, bt_OnIOAccess=2, bt_OnReadsAndWrites=3);
type TBreakLength=(bl_1byte=0, bl_2byte=1, bl_8byte=2{Only when in 64-bit}, bl_4byte=3);

function DBKDebug_ContinueDebugEvent(handled: BOOL): boolean; stdcall;
function DBKDebug_WaitForDebugEvent(timeout: dword): boolean; stdcall;
function DBKDebug_GetDebuggerState(state: PDebuggerstate): boolean; stdcall;
function DBKDebug_SetDebuggerState(state: PDebuggerstate): boolean; stdcall;

function DBKDebug_SetGlobalDebugState(state: BOOL): BOOL; stdcall;
function DBKDebug_StartDebugging(processid:dword):BOOL; stdcall;
function DBKDebug_StopDebugging:BOOL; stdcall;
function DBKDebug_GD_SetBreakpoint(active: BOOL; debugregspot: integer; Address: dword; breakType: TBreakType; breakLength: TBreakLength): BOOL; stdcall;


implementation

function StartCEKernelDebug:BOOL; stdcall;
var
    br,cc: dword;
    i:integer;
    cpunr,PA,SA:Dword;
    cpunr2:byte;
begin
  result:=false;
  outputdebugstring('DebugProcess function');

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_HOOKINTS;

    GetProcessAffinityMask(getcurrentprocess,PA,SA);

    //first hook the interrupts if needed
    cpunr2:=0;
    cpunr:=1;
    while (cpunr<=PA) do
    begin
      if ((cpunr) and PA)>0 then
      begin
        SetProcessAffinityMask(getcurrentprocess,cpunr);
        //create a new thread. (Gues on what cpu it will run at...)

        with THookIDTThread.Create(true) do
        begin
          try
            cpunr:=cpunr2;
            resume;

            while not done do sleep(10); //the sleep should also cause a taskswitch but I'm not 100% sure

            if not succeeded then
            begin
              SetProcessAffinityMask(getcurrentprocess,PA);
              messagebox(0,pchar('Failure when changing the interrupt handler on CPU '+inttostr(cpunr)),'',mb_ok);
              exit;
            end;
          finally
            free;
          end;
        end;

      end;
      if cpunr=$80000000 then break;
      inc(cpunr,cpunr);
      inc(cpunr2);
    end;

    SetProcessAffinityMask(getcurrentprocess,PA); //multi processors are so fun. It'd be a waste not to use it
    outputdebugstring('going to start the hooker');
    hooker:=thookidtconstantly.Create(false);

    result:=true;
  end;
end;

function DBKDebug_SetGlobalDebugState(state: BOOL): BOOL; stdcall;
var
  x: BOOL;
  br,cc: dword;
begin
  outputdebugstring('SetGlobalDebugState');
  x:=state;

  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_SETGLOBALDEBUGSTATE;
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,br,nil);
  end else result:=false;
end;

function DBKDebug_GD_SetBreakpoint(active: BOOL; debugregspot: integer; Address: dword; breakType: TBreakType; breakLength: TBreakLength): BOOL; stdcall;
var
  input: record
    active: BOOL;
    debugregspot: integer;
    address: DWORD;
    breaktype: TBreakType;
    breakLength: TBreakLength;
  end;

  br,cc: dword;
begin
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    result:=StartCEKernelDebug;
    input.active:=active;
    input.debugregspot:=debugregspot;
    input.address:=address;
    input.breaktype:=breaktype;
    input.breakLength:=breaklength;
    
    cc:=IOCTL_CE_GD_SETBREAKPOINT;
    result:=result and deviceiocontrol(hdevice,cc,@input,sizeof(input),@input,0,br,nil);
  end;
end;

function DBKDebug_StartDebugging(processid:dword):BOOL; stdcall;
type Tinput=record
  ProcessID:DWORD;
end;
var input:TInput;
    br,cc: dword;
begin
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    result:=StartCEKernelDebug;
    input.Processid:=processid;
    cc:=IOCTL_CE_DEBUGPROCESS;
    result:=result and deviceiocontrol(hdevice,cc,@input,sizeof(input),@input,0,br,nil);
  end;
end;


function DBKDebug_StopDebugging:BOOL; stdcall;
var x,cc: dword;
begin
  outputdebugstring('DBK32: StopDebugging called');
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_STOPDEBUGGING;
    result:=deviceiocontrol(hdevice,cc,nil,0,nil,0,x,nil);
  end;
end;


function DBKDebug_GetDebuggerState(state: PDebuggerstate): boolean; stdcall;
var
  Output: TDebuggerstate;
  cc: dword;
begin
  OutputDebugString('DBKDebug_GetDebuggerState');
  result:=false;
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_GETDEBUGGERSTATE;
    result:=deviceiocontrol(hdevice,cc,nil,0,@output,sizeof(output),cc,nil);
    if result then
    begin
      OutputDebugString('result = true');
      state^:=output;
    end;
  end;
end;

function DBKDebug_SetDebuggerState(state: PDebuggerstate): boolean; stdcall;
var
  input: TDebuggerstate;
  cc: dword;
begin
  OutputDebugString('DBKDebug_SetDebuggerState');
  result:=false;
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_SETDEBUGGERSTATE;
    input:=state^;
    result:=deviceiocontrol(hdevice,cc,@input,sizeof(Input),nil,0,cc,nil);
  end;
end;


function DBKDebug_ContinueDebugEvent(handled: BOOL): boolean; stdcall;
var
  cc: dword;
  Input: record
    handled: BOOL;
  end;
begin
  result:=false;
  input.handled:=handled;
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_CONTINUEDEBUGEVENT;
    result:=deviceiocontrol(hdevice,cc,@Input,sizeof(input),nil,0,cc,nil);
  end;
end;

function DBKDebug_WaitForDebugEvent(timeout: dword): boolean; stdcall;
type TInput=record
  Timeout: DWORD;
end;
var cc: dword;
    x: TInput;
begin
  result:=false;
  x.timeout:=timeout;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_WAITFORDEBUGEVENT;
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,cc,nil);
  end;
end;


end.
