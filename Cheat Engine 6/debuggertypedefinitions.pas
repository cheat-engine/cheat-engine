unit debuggertypedefinitions;

{$mode delphi}

interface

uses
  Classes, SysUtils, Windows, FoundCodeUnit, formchangedaddresses, frmTracerUnit, NewKernelHandler;

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
  TContinueOption = (co_run=0, co_stepinto=1, co_stepover=2, co_runtill=3);

type
  TBreakpointMethod = (bpmInt3, bpmDebugRegister);

type
  TBreakOption = (bo_Break = 0, bo_ChangeRegister = 1, bo_FindCode = 2, bo_FindWhatCodeAccesses = 3, bo_BreakAndTrace=4);
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

type
  PBreakpoint = ^TBreakPoint;

  TBreakpoint = record
    {
    the following 2 items: active and markedfordeletion handle the case when a
    breakpoint has been removed right at the same moment it has fired and the user
    thread managed to get to the critical section first
    }
    active: boolean;
    //set if the debugger should handle it fully, or just skip it (but no dbg_nothandled)
    markedfordeletion: boolean;
    deletecountdown: integer; //when markedfordeletion is set and deletecountdown hits 0, it'll get deleted
    referencecount: integer; //the number of windows this bp is currently being edited in (mainly used in 'set/change condition')

    //If set the next time no events take place this breakpoint will be removed

    //    condition: TCondition;

    owner: PBreakpoint;
    //in case of a multi DR/address breakpoint, removing one of these or the owner, affects the others
    address: uint_ptr;
    originalbyte: byte;
    breakpointMethod: TBreakpointMethod;
    breakpointAction: TBreakOption;
    breakpointTrigger: TBreakpointTrigger;
    debugRegister: integer;
    //if debugRegister bp this will hold which debug register is used for it
    size: integer; //if and on access or onwrite this defines the region
    FoundcodeDialog: TFoundcodedialog;
    frmchangedaddresses: Tfrmchangedaddresses;
    frmTracer: TfrmTracer;
    tracecount: integer;
    traceendcondition: pchar;

    //set if it's a bpaFetchRegistersandcontinue set on memory access
    //ChangedAddresses: TfrmChangedAddresses; //set if it's a bpaFetchRegistersandcontinue set on execute
    ThreadID: DWORD;
    //set if the breakpoint is for one specific thread, ignore breaks if it happens on other threads

    OneTimeOnly: boolean; //true if the breakpoint should be removed after the first time it is hit

    changereg: tregistermodificationBP;

    conditonalbreakpoint: record
      script: pchar;
      easymode: boolean;
    end;
  end;


  TBreakpointSplit = record //the breakpointsplit type is used by GetBreakpointList
    address: uint_ptr;      //address alligned on size
    size: integer;       //1, 2 or 4
  end;
  TBreakpointSplitArray = array of TBreakpointSplit;



function breakpointTriggerToString(bpt: TBreakpointTrigger): string;
function breakpointMethodToString(bpm: TBreakpointMethod): string;
function breakpointActionToString(bpa: TBreakpointAction): string;

//kernel debug
function SizeToBreakLength(size: integer): TBreakLength;
function BreakPointTriggerToBreakType(bpt: TBreakpointTrigger): TBreakType;


var int3byte: byte = $cc;

implementation


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
    bptExecute: result:='On Execute';
    bptWrite:   result:='On Write';
    bptAccess:  result:='On Read/Write';
  end;
end;

function BreakpointMethodToString(bpm: TBreakpointMethod): string;
begin
  case bpm of
    bpmInt3:          result:='Software Breakpoint';
    bpmDebugRegister: result:='Hardware Breakpoint';
  end;
end;

function breakpointActionToString(bpa: TBreakpointAction): string;
begin
  case bpa of
    bo_Break: result:='Break';
    bo_ChangeRegister: result:='Change reg';
    bo_FindCode: result:='Find code';
    bo_FindWhatCodeAccesses: result:='Find code access';
    bo_BreakAndTrace: result:='Break and trace';
  end;
end;

end.

