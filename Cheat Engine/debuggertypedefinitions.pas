unit debuggertypedefinitions;

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  Windows,
  {$endif}
  Classes, SysUtils,
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

  {$ifdef cpu32}
  M128A= record
    low,high: qword;
  end;
  {$endif}

  TXMMFIELDS=array [0..3] of DWORD;
  PXMMFIELDS=^TXMMFIELDS;

 TRegisterModificationFloatList=bitpacked record
   change_fp0: 0..1;
   change_fp1: 0..1;
   change_fp2: 0..1;
   change_fp3: 0..1;
   change_fp4: 0..1;
   change_fp5: 0..1;
   change_fp6: 0..1;
   change_fp7: 0..1;
 end;
 PRegisterModificationFloatList=^TRegisterModificationFloatList;

 TRegisterModificationXMMListSingleEntry=bitpacked record
   change_part1: 0..1;
   change_part2: 0..1;
   change_part3: 0..1;
   change_part4: 0..1;
 end;
 PRegisterModificationXMMListSingleEntry=^TRegisterModificationXMMListSingleEntry;

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

    change_FP: BYTE; //binary, each bit is a new FP
    new_FP0: double;
    new_FP1: double;
    new_FP2: double;
    new_FP3: double;
    new_FP4: double;
    new_FP5: double;
    new_FP6: double;
    new_FP7: double;

    change_XMM: DWORD; //binary, each nibble is an XMM register, and each bit of the nibble is a dword part of the xmm reg
    new_XMM0: TXMMFIELDS;
    new_XMM1: TXMMFIELDS;
    new_XMM2: TXMMFIELDS;
    new_XMM3: TXMMFIELDS;
    new_XMM4: TXMMFIELDS;
    new_XMM5: TXMMFIELDS;
    new_XMM6: TXMMFIELDS;
    new_XMM7: TXMMFIELDS;

    usesDouble: BYTE; //each bit specifies an xmm field what weas edited using a double specifier
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

    change_FP: BYTE; //binary, each bit is a new FP
    new_FP0: M128A; //only need the 10 bytes
    new_FP1: M128A;
    new_FP2: M128A;
    new_FP3: M128A;
    new_FP4: M128A;
    new_FP5: M128A;
    new_FP6: M128A;
    new_FP7: M128A;

    change_XMM: QWORD; //binary, each nibble is an XMM register, and each bit of the nibble is a dword part of the xmm reg
    new_XMM0: TXMMFIELDS;
    new_XMM1: TXMMFIELDS;
    new_XMM2: TXMMFIELDS;
    new_XMM3: TXMMFIELDS;
    new_XMM4: TXMMFIELDS;
    new_XMM5: TXMMFIELDS;
    new_XMM6: TXMMFIELDS;
    new_XMM7: TXMMFIELDS;
    new_XMM8: TXMMFIELDS;
    new_XMM9: TXMMFIELDS;
    new_XMM10: TXMMFIELDS;
    new_XMM11: TXMMFIELDS;
    new_XMM12: TXMMFIELDS;
    new_XMM13: TXMMFIELDS;
    new_XMM14: TXMMFIELDS;
    new_XMM15: TXMMFIELDS;

    //for gui purposes only:
    usesDouble: WORD; //each bit specifies an xmm field what weas edited using a double specifier

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

