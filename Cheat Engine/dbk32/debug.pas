unit debug;

interface

uses windows, dbk32functions;

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

function DBKDebug_ContinueDebugEvent(handled: BOOL): boolean; stdcall;
function DBKDebug_WaitForDebugEvent(timeout: dword): boolean; stdcall;
function DBKDebug_GetDebuggerState(state: PDebuggerstate): boolean; stdcall;
function DBKDebug_SetDebuggerState(state: PDebuggerstate): boolean; stdcall;


implementation

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
