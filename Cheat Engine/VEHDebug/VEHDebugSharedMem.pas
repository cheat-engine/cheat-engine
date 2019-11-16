unit VEHDebugSharedMem;
{
This unit it used by both CE and the VEHDebug project
}

{$mode delphi}

interface

uses
  windows, Classes, SysUtils;


const
  TPOLL_TCREATEREALCONTEXT=$00000001;
  VEHVERSION=4;

type TVEHDebugSharedMem=packed record
  CurrentContext: array [0..8191] of byte; //should be enough for a context...


  //Events created by CE, but duplicated for rights for the target process
  HasDebugEvent: QWORD; //set by the dll (qword so the allignment isn't broken when used in 32-bit)
  HasHandledDebugEvent: QWORD; //set by the debugger
  ContinueMethod: QWORD;
  ProcessID: DWORD;
  ThreadID: DWORD;
  ThreadWatchMethod: QWORD;
  ThreadWatchMethodConfig: QWORD; //each bit contains an boolean option (for threadpoll, the only one implemented, bit 0 means simulate thread create contexts)
  VEHVersion: DWORD; //0xCECE####  , set by the DLL
  HeartBeat: DWORD; //value that constantly changes as long as CE is alive

  NoBreakListSize: QWORD;   //number of entries in the nobreaklist
  NoBreakList: array [0..63] of QWORD;

  case integer of
    1: (Exception32: EXCEPTION_RECORD32);
    2: (Exception64: EXCEPTION_RECORD64);
  //end;







end;
type PVEHDebugSharedMem=^TVEHDebugSharedMem;



implementation

end.

