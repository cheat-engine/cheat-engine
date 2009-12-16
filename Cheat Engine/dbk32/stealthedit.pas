unit stealthedit;

interface

uses windows, sysutils, dbk32functions, classes, multicpuexecution;

function stealthedit_InitializeHooks: bool; stdcall;
function stealthedit_AddCloakedSection(processid: DWORD; pagebase: dword; relocatedpagebase: dword; size: integer ):BOOl; stdcall;
function stealthedit_RemoveCloakedSection(processid: DWORD; pagebase: dword):BOOl; stdcall;

implementation

function internal_stealthedit_InitializeHooks(parameters: pointer):BOOL; stdcall;
var cc,br: dword;
begin
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_HOOKSTEALTHEDITINTS;
    outputdebugstring('Calling IOCTL_CE_HOOKSTEALTHEDITINTS');
    result:=deviceiocontrol(hdevice,cc,nil,0,nil,0,br,nil);
  end else result:=false;
end;

function stealthedit_InitializeHooks: bool; stdcall;
begin
  result:=foreachcpu(internal_stealthedit_InitializeHooks, nil);
end;


function stealthedit_AddCloakedSection(processid: DWORD; pagebase: dword; relocatedpagebase: dword; size: integer ):BOOl; stdcall;
var
  input: record
    ProcessID: uint64;
		pagebase:  uint64;
	  relocatedpagebase: uint64;
		size: uint64;
  end;
  cc,br: dword;
begin
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    input.ProcessID:=processid;
    input.pagebase:=pagebase;
    input.relocatedpagebase:=relocatedpagebase;
    input.size:=size;
    cc:=IOCTL_CE_ADDCLOAKEDSECTION;
    outputdebugstring('Calling IOCTL_CE_HOOKSTEALTHEDITINTS');
    result:=deviceiocontrol(hdevice,cc,@input,sizeof(input),nil,0,br,nil);
  end else result:=false;
end;

function stealthedit_RemoveCloakedSection(processid: DWORD; pagebase: dword):BOOl; stdcall;
var
  input: record
    ProcessID: uint64;
		pagebase:  uint64;
  end;
  cc,br: dword;
begin
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    input.ProcessID:=processid;
    input.pagebase:=pagebase;
    cc:=IOCTL_CE_REMOVECLOAKEDSECTION;
    outputdebugstring('Calling IOCTL_CE_HOOKSTEALTHEDITINTS');
    result:=deviceiocontrol(hdevice,cc,@input,sizeof(input),nil,0,br,nil);
  end else result:=false;
end;

end.

