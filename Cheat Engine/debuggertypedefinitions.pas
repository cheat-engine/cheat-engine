unit debuggertypedefinitions;

{$mode delphi}

interface

uses
  Classes, SysUtils, Windows,
  cefuncproc, NewKernelHandler, commonTypeDefs;

type
  TNewProcedureData = record
    processid: dword;
    processhandle: dword;
    filehandle: dword;
    EntryPoint: dword;
    OriginalEntryByte: byte;
    DotNet: boolean;
    FirstBreak: boolean;
  end;



type
  TDebugRegisterType = (drtAny, drtExecute, drtWatch);

type
  TContinueOption = (co_run=0, co_stepinto=1, co_stepover=2, co_runtill=3);

type
  TBreakpointMethod = (bpmInt3=0, bpmDebugRegister=1, bpmException=2, bpmDBVM=3);

type
  TBreakOption = (bo_Break = 0, bo_ChangeRegister = 1, bo_FindCode = 2, bo_FindWhatCodeAccesses = 3, bo_BreakAndTrace=4, bo_OnBreakpoint=5);
  TBreakPointAction = TBreakOption;

type
  TBreakpointTrigger = (bptExecute=0, bptAccess=1, bptWrite=2);

type
  tregistermodificationBP32 = record
    address: uint_ptr; //addres to break on
    change_eax: BOOL;
    change_ebx: BOOL;
    change_ecx: BOOL;
    change_edx: BOOL;
    change_esi: BOOL;
    change_edi: BOOL;
    change_ebp: BOOL;
    change_esp: BOOL;
    change_eip: BOOL;
    change_cf: BOOL;
    change_pf: BOOL;
    change_af: BOOL;
    change_zf: BOOL;
    change_sf: BOOL;
    change_of: BOOL;
    new_eax: dword;
    new_ebx: dword;
    new_ecx: dword;
    new_edx: dword;
    new_esi: dword;
    new_edi: dword;
    new_ebp: dword;
    new_esp: dword;
    new_eip: dword;
    new_cf: BOOL;
    new_pf: BOOL;
    new_af: BOOL;
    new_zf: BOOL;
    new_sf: BOOL;
    new_of: BOOL;
  end;

type
  PRegisterModificationBP32 = ^TRegisterModificationBP32;

type
  tregistermodificationBP64 = record
    address: uint_ptr; //addres to break on
    change_eax: BOOL;
    change_ebx: BOOL;
    change_ecx: BOOL;
    change_edx: BOOL;
    change_esi: BOOL;
    change_edi: BOOL;
    change_ebp: BOOL;
    change_esp: BOOL;
    change_eip: BOOL;
    change_r8: BOOL;
    change_r9: BOOL;
    change_r10: BOOL;
    change_r11: BOOL;
    change_r12: BOOL;
    change_r13: BOOL;
    change_r14: BOOL;
    change_r15: BOOL;
    change_cf: BOOL;
    change_pf: BOOL;
    change_af: BOOL;
    change_zf: BOOL;
    change_sf: BOOL;
    change_of: BOOL;
    new_eax: ptrUint;
    new_ebx: ptrUint;
    new_ecx: ptrUint;
    new_edx: ptrUint;
    new_esi: ptrUint;
    new_edi: ptrUint;
    new_ebp: ptrUint;
    new_esp: ptrUint;
    new_eip: ptrUint;
    new_r8: ptrUint;
    new_r9: ptrUint;
    new_r10: ptrUint;
    new_r11: ptrUint;
    new_r12: ptrUint;
    new_r13: ptrUint;
    new_r14: ptrUint;
    new_r15: ptrUint;
    new_cf: BOOL;
    new_pf: BOOL;
    new_af: BOOL;
    new_zf: BOOL;
    new_sf: BOOL;
    new_of: BOOL;
  end;

type
  PRegisterModificationBP64 = ^TRegisterModificationBP64;

{$ifdef CPU64}
type
  TRegisterModificationBP = TRegisterModificationBP64;

type
  PRegisterModificationBP = PRegisterModificationBP64;
{$else}
type
  TRegisterModificationBP = TRegisterModificationBP32;

type
  PRegisterModificationBP = PRegisterModificationBP32;
{$endif}



 // TBreakpointList=TFPGList<PBreakpoint>;

function BreakPointTriggerIsWatchpoint(bpt: TBreakpointTrigger): boolean; inline;

function breakpointTriggerToString(bpt: TBreakpointTrigger): string;
function breakpointMethodToString(bpm: TBreakpointMethod): string;
function breakpointActionToString(bpa: TBreakpointAction): string;

//kernel debug
function SizeToBreakLength(size: integer): TBreakLength;
function BreakPointTriggerToBreakType(bpt: TBreakpointTrigger): TBreakType;



var int3byte: byte = $cc;

implementation

//this is a new one... (rebuild as usual)
resourcestring
  rsOnExecute = 'On Execute';
  rsOnWrite = 'On Write';
  rsOnReadWrite = 'On Read/Write';
  rsSoftwareBreakpoint = 'Software Breakpoint';
  rsHardwareBreakpoint = 'Hardware Breakpoint';
  rsExceptionBreakpoint = 'Exception Breakpoint';
  rsDBVMBreakpoint = 'DBVM Breakpoint';
  rsBreak = 'Break';
  rsChangeReg = 'Change reg';
  rsFindCode = 'Find code';
  rsFindCodeAccess = 'Find code access';
  rsBreakAndTrace = 'Break and trace';

function SizeToBreakLength(size: integer): TBreakLength;
begin
  case size of
    1: result:=bl_1byte;
    2: result:=bl_2byte;
    4: result:=bl_4byte;
    8: result:=bl_8byte;
    else
       result:=bl_1byte;
  end;
end;

function BreakPointTriggerIsWatchpoint(bpt: TBreakpointTrigger): boolean; inline;
begin
  result:=bpt in [bptAccess, bptWrite];
end;

function BreakPointTriggerToBreakType(bpt: TBreakpointTrigger): TBreakType;
begin
  case bpt of
    bptExecute: result:=bt_OnInstruction;
    bptAccess: result:=bt_OnReadsAndWrites;
    bptWrite: result:=bt_OnWrites;
    else
       result:=bt_OnInstruction;
  end;
end;


function breakpointTriggerToString(bpt: TBreakpointTrigger): string;
begin
  case bpt of
    bptExecute:  result:=rsOnExecute;
    bptWrite:    result:=rsOnWrite;
    bptAccess:   result:=rsOnReadWrite;
    else
       result:='Error';
  end;
end;

function BreakpointMethodToString(bpm: TBreakpointMethod): string;
begin
  case bpm of
    bpmInt3:           result:=rsSoftwareBreakpoint;
    bpmDebugRegister:  result:=rsHardwareBreakpoint;
    bpmException:      result:=rsExceptionBreakpoint;
    bpmDBVM:           result:=rsDBVMBreakpoint;
    else
       result:='Error';
  end;
end;

function breakpointActionToString(bpa: TBreakpointAction): string;
begin
  case bpa of
    bo_Break: result:=rsBreak;
    bo_ChangeRegister: result:=rsChangeReg;
    bo_FindCode: result:=rsFindCode;
    bo_FindWhatCodeAccesses: result:=rsFindCodeAccess;
    bo_BreakAndTrace: result:=rsBreakAndTrace;
    else
       result:='Error';
  end;
end;

end.

