unit BreakpointTypeDef;

{$mode delphi}

interface

uses
  Classes, SysUtils, windows, FoundCodeUnit, formchangedaddresses, frmTracerUnit,
  commonTypeDefs, debuggertypedefinitions;

type

  PBreakpoint = ^TBreakPoint;

  TBreakpointEvent=function(bp: pointer; OnBreakpointContext: pointer):boolean of object;

  TBreakpoint = record
    {
    the following 2 items: active and markedfordeletion handle the case when a
    breakpoint has been removed right at the same moment it has fired and the user
    thread managed to get to the critical section first
    }
    active: boolean;  //todo: Perhaps a per/threadid activate field
    //set if the debugger should handle it fully, or just skip it (but no dbg_nothandled)
    markedfordeletion: boolean;
    deletetickcount: dword;
    deletecountdown: integer; //when markedfordeletion is set and deletecountdown hits 0, it'll get deleted
    referencecount: integer; //the number of windows this bp is currently being edited in (mainly used in 'set/change condition')

    //If set the next time no events take place this breakpoint will be removed

    //    condition: TCondition;

    owner: PBreakpoint;
    //in case of a multi DR/address breakpoint, removing one of these or the owner, affects the others
    address: uint_ptr;
    size: integer; //size of this breakpoint (can be any size in case of exception bp)
    originalbyte: byte;

    originalaccessrights: TAccessRights;

    breakpointMethod: TBreakpointMethod;
    breakpointAction: TBreakOption;
    breakpointTrigger: TBreakpointTrigger;
    debugRegister: integer;  //if debugRegister bp this will hold which debug register is used for it

    dbvmwatchid: integer; //DBVM watch id in case of a bpmDBVM



    FoundcodeDialog: TFoundcodedialog;
    frmchangedaddresses: Tfrmchangedaddresses;
    frmTracer: TfrmTracer;
    tracecount: integer;
    traceendcondition: pchar;
    tracestepOver: boolean; //when set the tracer will step over instead of single step
    traceNoSystem: boolean; //when set the tracer will step over system module addresses

    isTracerStepOver: boolean; //

    //set if it's a bpaFetchRegistersandcontinue set on memory access
    //ChangedAddresses: TfrmChangedAddresses; //set if it's a bpaFetchRegistersandcontinue set on execute
    ThreadID: DWORD;
    //set if the breakpoint is for one specific thread, ignore breaks if it happens on other threads

    OneTimeOnly: boolean; //true if the breakpoint should be removed after the first time it is hit
    StepOverBp: boolean;

    changereg: tregistermodificationBP;

    conditonalbreakpoint: record
      script: pchar;
      easymode: boolean;
    end;

    OnBreakpoint: TBreakpointEvent; //method to be called by the debuggerthread when this breakpoint triggers
    OnBreakpointContext: pointer;
  end;


  TBreakpointSplit = record //the breakpointsplit type is used by GetBreakpointList
    address: uint_ptr;      //address alligned on size
    size: integer;       //1, 2 or 4
  end;
  TBreakpointSplitArray = array of TBreakpointSplit;

implementation

end.

