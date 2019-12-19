unit exceptionhandler;

{$mode delphi}

interface

uses
  jwawindows, //I use jwawindows only for the purpose of the structure declaration of PVECTORED_EXCEPTION_HANDLER, I declare and assign AddVectoredExceptionHandler myself
  windows, Classes, SysUtils;

var
  exceptrsp,exceptrbp, tryBegin, tryExcept: ptrUint; //globally accessible, set each time when a thread is about to raise an exception that needs to be handled

implementation

type TAddVectoredExceptionHandler=function(FirstHandler: ULONG; VectoredHandler: PVECTORED_EXCEPTION_HANDLER): PVOID; stdcall;
type TRemoveVectoredExceptionHandler=function(VectoredHandlerHandle: PVOID): ULONG; stdcall;
type TStackWalk64=function(MachineType: DWORD; hProcess: HANDLE; hThread: HANDLE; var StackFrame: STACKFRAME64; ContextRecord: PVOID; ReadMemoryRoutine: PREAD_PROCESS_MEMORY_ROUTINE64; FunctionTableAccessRoutine: PFUNCTION_TABLE_ACCESS_ROUTINE64; GetModuleBaseRoutine: PGET_MODULE_BASE_ROUTINE64; TranslateAddress: PTRANSLATE_ADDRESS_ROUTINE64): BOOL; stdcall;
type TSymFunctionTableAccess64=function(hProcess: HANDLE; AddrBase: Int64): PVOID; stdcall;
type TSymGetModuleBase64=function(hProcess: HANDLE; qwAddr: Int64): Int64; stdcall;
type TSymInitialize=function(hProcess: HANDLE; UserSearchPath: PSTR; fInvadeProcess: BOOL): BOOL; stdcall;



var k,d: THandle;
    veh: PVOID;
    AddVectoredExceptionHandler:    TAddVectoredExceptionHandler;
    RemoveVectoredExceptionHandler: TRemoveVectoredExceptionHandler;
    StackWalk64: TStackWalk64;
    SymFunctionTableAccess64: TSymFunctionTableAccess64;
    SymGetModuleBase64:TSymGetModuleBase64;
    SymInitialize:TSymInitialize;

function MyVectoredHandler(ExceptionInfo: PEXCEPTION_POINTERS): DWORD; stdcall;
var sf: STACKFRAME64;
    tempcontext: CONTEXT;
begin
  //check if the exception falls within the region of the try block

(*
 // else
  begin
    //secondary check



    //do a stacktrace
    tempcontext:=ExceptionInfo.ContextRecord^; //copy over the details
    ZeroMemory(@sf,sizeof(sf));

    sf.AddrPC.Offset:=tempcontext.{$ifdef cpu64}Rip{$else}eip{$endif};
    sf.AddrPC.Mode:=AddrModeFlat;
    sf.AddrFrame.Offset:=tempcontext.{$ifdef cpu64}rbp{$else}ebp{$endif};
    sf.AddrFrame.Mode:=AddrModeFlat;
    sf.AddrStack.Offset:=tempcontext.{$ifdef cpu64}rsp{$else}esp{$endif};
    sf.AddrStack.Mode:=AddrModeFlat;

    while StackWalk64(IMAGE_FILE_MACHINE_AMD64, GetCurrentProcess, GetCurrentThread, sf, @tempcontext, nil, SymFunctionTableAccess64, SymGetModuleBase64, nil) do
    begin
      if (sf.AddrPC.Offset>=tryBegin) and (sf.AddrPC.Offset<tryExcept) then
      begin
        //if so, adjust eip to go to the except block
        ExceptionInfo.ContextRecord^.Rip:=tryExcept;
        ExceptionInfo.ContextRecord^.rbp:=exceptrbp;
        ExceptionInfo.ContextRecord^.rsp:=exceptrsp;
        result:=EXCEPTION_CONTINUE_EXECUTION;
        exit;
      end;

      sf.AddrPC.Offset:=tempcontext.Rip;
      sf.AddrPC.Mode:=AddrModeFlat;
      sf.AddrFrame.Offset:=tempcontext.rbp;
      sf.AddrFrame.Mode:=AddrModeFlat;
      sf.AddrStack.Offset:=tempcontext.rsp;
      sf.AddrStack.Mode:=AddrModeFlat;

    end;

    result:=EXCEPTION_CONTINUE_SEARCH; //else let the other exception handlers deal with it
  end;     *)
end;


initialization
(*
k:=LoadLibrary('kernel32.dll');
  AddVectoredExceptionHandler:=GetProcAddress(k,'AddVectoredExceptionHandler');
  RemoveVectoredExceptionHandler:=GetProcAddress(k,'RemoveVectoredExceptionHandler');

  d:=LoadLibrary('Dbghelp.dll');
  StackWalk64:=GetProcAddress(d,'StackWalk64');
  SymFunctionTableAccess64:=GetProcAddress(d,'SymFunctionTableAccess64');
  SymGetModuleBase64:=GetProcAddress(d,'SymGetModuleBase64');
  SymInitialize:=GetProcAddress(d,'SymInitialize');

  if assigned(SymInitialize) then
    if SymInitialize(GetCurrentProcess, nil, true) then
    begin
      beep;
    end;

  if assigned(AddVectoredExceptionHandler) then
    veh:=AddVectoredExceptionHandler(1, @MyVectoredHandler); //1 = Be the first handler to get the exception. (Note: Even an outputdebugstring raises an exception)
*)
finalization
(*  if veh<>nil then
    RemoveVectoredExceptionHandler(veh);

  FreeLibrary(k); //never forget to unload kernel32.dll, it might not be needed anymore.... yeah....
  *)
end.

