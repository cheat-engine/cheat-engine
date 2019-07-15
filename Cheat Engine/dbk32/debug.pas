unit debug;

{$MODE Delphi}

interface

uses windows, sysutils, dbk32functions, classes, multicpuexecution;

type TDebuggerstate=packed record
  threadid: uint64;
  causedbydbvm: uint64;
	eflags : uint64;
	eax : uint64;
	ebx : uint64;
	ecx : uint64;
	edx : uint64;
	esi : uint64;
	edi : uint64;
	ebp : uint64;
	esp : uint64;
	eip : uint64;
	r8  : uint64;
	r9  : uint64;
	r10 : uint64;
	r11 : uint64;
	r12 : uint64;
	r13 : uint64;
	r14 : uint64;
	r15 : uint64;
	cs  : uint64;
	ds  : uint64;
	es  : uint64;
	fs  : uint64;
	gs  : uint64;
	ss  : uint64;
  dr0 : uint64;
  dr1 : uint64;
  dr2 : uint64;
  dr3 : uint64;
  dr6 : uint64;
  dr7 : uint64;
  fxstate: array[0..511] of byte;
  LBR_Count: uint64;
  LBR: array [0..15] of UINT64;

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
function DBKDebug_GD_SetBreakpoint(active: BOOL; debugregspot: integer; Address: ptruint; breakType: TBreakType; breakLength: TBreakLength): BOOL; stdcall;
function DBKDebug_SetAbilityToStepKernelCode(state: boolean):BOOL; stdcall;
procedure DBKDebug_SetStoreLBR(state: BOOL);

implementation



function hookints: BOOL; stdcall;
var cc,br: dword;
begin
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_HOOKINTS;
    result:=deviceiocontrol(hdevice,cc,nil,0,nil,0,br,nil);
  end else result:=false;
  
end;

var KernelDebugStarted: boolean;
function StartCEKernelDebug:BOOL; stdcall;
begin
  outputdebugstring('StartCEKernelDebug');
  if not KernelDebugStarted then
    KernelDebugStarted:=hookints;

  result:=KernelDebugStarted;
end;

function internal_SetGlobalDebugState(state: pointer): BOOL; stdcall;
var
  x: BOOL;
  br,cc: dword;
begin
  outputdebugstring('SetGlobalDebugState');
  x:=PBOOL(state)^;

  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_SETGLOBALDEBUGSTATE;
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,br,nil);
  end;
end;

function DBKDebug_SetGlobalDebugState(state: BOOL): BOOL; stdcall;
begin
  result:=foreachcpu(internal_SetGlobalDebugState, @state);
end;

function internal_touchdebugregister(parameters: pointer): BOOL; stdcall;
var
  br,cc: dword;
begin
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_TOUCHDEBUGREGISTER;
    result:=deviceiocontrol(hdevice,cc,nil,0,nil,0,br,nil);
  end;
end;

procedure DBKDebug_TouchDebugRegister;
//this routine touches the debug registers on each cpu
//when global debug is enabled this facilitates in setting or unsetting changes in the breakpoint list
//this way when a breakpoint is set, it actually gets set, or unset the same
//just make sure to disable the breakpoint before removing the handler
begin
  OutputDebugString('DBKDebug_TouchDebugRegister');
  foreachcpu(internal_touchdebugregister,nil);
end;

function DBKDebug_GD_SetBreakpoint(active: BOOL; debugregspot: integer; Address: ptruint; breakType: TBreakType; breakLength: TBreakLength): BOOL; stdcall;
var
  input: record
    active: BOOL;
    debugregspot: integer;
    address: QWORD;
    breaktype: DWORD;
    breakLength: DWORD;
  end;

  br,cc: dword;
begin
  OutputDebugString('DBKDebug_GD_SetBreakpoint');
  if not active then OutputDebugString('Deactivating breakpoint');

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    result:=StartCEKernelDebug;
    input.active:=active;
    input.debugregspot:=debugregspot;
    input.address:=address;
    input.breaktype:=dword(breaktype);
    input.breakLength:=dword(breaklength);

    outputdebugstring(pchar(format('sizeof(input)=%d, breaktype=%d breaklength=%d',[sizeof(input), integer(input.breaktype), integer(input.breakLength)])));

    cc:=IOCTL_CE_GD_SETBREAKPOINT;
    result:=result and deviceiocontrol(hdevice,cc,@input,sizeof(input),@input,0,br,nil);
    DBKDebug_TouchDebugRegister; //update the system state
  end else result:=false;
end;


function DBKDebug_StartDebuggingInternal(processid: pointer):BOOL; stdcall;
type Tinput=record
  ProcessID:DWORD;
end;
var input:TInput;
    br,cc: dword;
begin
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    result:=StartCEKernelDebug;
    input.Processid:=PDWORD(processid)^;
    cc:=IOCTL_CE_DEBUGPROCESS;
    result:=result and deviceiocontrol(hdevice,cc,@input,sizeof(input),@input,0,br,nil);
  end else result:=false;
end;





function DBKDebug_StartDebugging(processid:dword):BOOL; stdcall;
begin
  result:=foreachcpu(DBKDebug_StartDebuggingInternal, @processid);
end;

function internal_StopDebugging(parameters: pointer):BOOL; stdcall;
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

function DBKDebug_StopDebugging:BOOL; stdcall;
begin
  result:=foreachcpu(internal_StopDebugging,nil);
end;

function DBKDebug_GetDebuggerState(state: PDebuggerstate): boolean; stdcall;
var
  Output: TDebuggerstate;
  cc: dword;
begin
  FillMemory(state,sizeof(TDebuggerState),1);
  
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

//    state.fxstate
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

function DBKDebug_SetAbilityToStepKernelCode(state: boolean):BOOL; stdcall;
type Tinput=record
  state: integer;
end;
var input:TInput;
    br,cc: dword;
begin

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    result:=false;
    if state then input.state:=1 else input.state:=0;


    cc:=IOCTL_CE_SETKERNELSTEPABILITY;
    result:=deviceiocontrol(hdevice,cc,@input,sizeof(input),nil,0,br,nil);
  end else result:=false;
end;

procedure DBKDebug_SetStoreLBR(state: BOOL);
var br,cc: dword;
begin
  outputdebugstring(pchar('DBKDebug_SetStoreLBR('+BoolToStr(state,'true','false')+')'));
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_SETSTORELBR;
    deviceiocontrol(hdevice,cc,@state,sizeof(state),nil,0,br,nil);
  end;
end;

end.
