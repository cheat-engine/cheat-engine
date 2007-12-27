library dbk32;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  DBK32functions in 'DBK32functions.pas',
  vmxfunctions in 'vmxfunctions.pas';

{$R *.res}


exports VQE;//VirtualQueryEx;
exports OP;//OpenProcess;
exports OT;//OpenThread
exports NOP;//NtOpenProcess;
exports RPM;//ReadProcessMemory;
exports WPM;//WriteProcessMemory;
exports VAE; //VirtualAllocEx
exports CreateRemoteAPC;
exports ReadPhysicalMemory;
exports WritePhysicalMemory;
exports GetPhysicalAddress;
exports GetPEProcess;
exports GetPEThread;
exports ProtectMe;
exports UnprotectMe;
exports IsValidHandle;
exports GetCR4;
exports GetCR3;
exports SetCR3;
exports GetCR0;
exports GetSDT;
exports GetSDTShadow;
exports setAlternateDebugMethod;
exports getAlternateDebugMethod;
exports DebugProcess;
exports StopDebugging;
exports StopRegisterChange;
exports RetrieveDebugData;
exports GetThreadsProcessOffset;
exports GetThreadListEntryOffset;
exports GetDebugportOffset;
exports GetProcessnameOffset;
exports StartProcessWatch;
exports WaitForProcessListData;
exports GetProcessNameFromID;
exports GetProcessNameFromPEProcess;
exports GetIDTCurrentThread;
exports GetIDTs;
exports MakeWritable;
exports GetLoadedState;
exports ChangeRegOnBP;
exports SetGlobalDebugState;

exports DBKSuspendThread;
exports DBKResumeThread;
exports DBKSuspendProcess;
exports DBKResumeProcess;

exports KernelAlloc;
exports GetKProcAddress;

exports GetSDTEntry;
exports SetSDTEntry;
exports GetSSDTEntry;
exports SetSSDTEntry;

exports GetGDT;

exports test;
exports useIOCTL;
exports MakeKernelCopy;


//vmx
exports dbvm_version;
exports dbvm_changeselectors;
exports dbvm_restore_interrupts;
exports dbvm_block_interrupts;
exports dbvm_redirect_interrupt1;

begin
  randomize;
end.
