unit KernelDebugger;

{obsolete}

{$MODE Delphi}

interface

uses jwawindows, windows, LCLIntf, classes, sysutils, syncobjs,dialogs,{classes,}CEDebugger,disassembler,NewKernelHandler,FoundCodeUnit,
     {tlhelp32,}ComCtrls,addressparser, graphics, CEFuncProc, debughelper, debuggertypedefinitions;


implementation

uses frmProcessWatcherUnit,formchangedaddresses,memorybrowserformunit, frmstacktraceunit;


end.
