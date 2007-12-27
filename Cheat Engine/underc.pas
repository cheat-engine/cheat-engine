unit underc;

{
Problem: The underc engine is ABSOLUTLY NOT thread safe. It uses static variables
to store the current script's information. As you may have guessed, this will
cause problems when another thread starts a script itself. (basicly the 2 scripts
will turn into one big garbled mess...)
So, this class is used to let other threads wait for their turn with the script
engine. Not very efficient, but let's assume not too many scrips get executed by
different threads. (so makes it unusable if I wanted to use it for scanning)

}

interface

uses windows, SyncObjs, SysUtils{$ifndef isdll},pluginexports, forms, dialogs,cefuncproc,newkernelhandler{$endif};

type tuc_init=function(defs_file: pchar; use_defs: BOOL): integer; stdcall;
type tuc_finis=procedure; stdcall;
type tuc_exec=function(command: pchar):BOOL; stdcall;
type tuc_result=procedure(buff: pchar; maxsize: integer); stdcall;
type tuc_error=procedure(buff: pchar; maxsize: integer); stdcall;
type tuc_import=function(declaration: pchar; routine: pointer):BOOL; stdcall;
type tuc_main=function(argc: integer; arguments: pchar): integer; stdcall;
type tuc_compile=function(result: pchar; code: pchar): pointer; stdcall;
type tuc_eval=function(expr: pchar; res: pchar; sz: integer): integer; stdcall;
type tuc_eval_args=function(func: pointer; result: pointer): integer; cdecl varargs;
type tuc_get_function=function(functionname: pchar): pointer; stdcall;


type TScriptengine=class
  private
    underclibrary: thandle;
    currentthreadid: dword;
    scriptEngineCS: TCriticalSection;

    uc_init: tuc_init;
    uc_finis: tuc_finis; //yes, it really is called finis...
    uc_exec: tuc_exec;
    uc_result: tuc_result;
    uc_error: tuc_error;
    uc_import: tuc_import;
    uc_main: tuc_main;
    uc_compile: tuc_compile;
    uc_eval: tuc_eval;
    uc_eval_args: tuc_eval_args;
    uc_get_function: tuc_get_function;
    function CheckThread: boolean;
  public
    constructor create;
    function beginScript: boolean;
    function endScript: boolean;
    function execute_command(command: string): boolean;
    function getError: string;
    function getResult: string;
end;

var scriptengine: TScriptengine;

implementation
//all dll implementations are done outside of the interface
//other units can only use the wrapper i'm providing. (more thread safe)

{$ifndef isdll}
function xplusone(x: integer):integer; stdcall;
begin
  result:=x+1;
end;

function ce_GetSelectedProcessHandle: THandle; stdcall;
begin
  result:=processhandle;
end;

function ce_ReadProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;
{Calls the ReadProcessMemory version the rest of CE uses right now}
begin
  result:=ReadProcessMemory(hProcess,lpBaseAddress,lpBuffer,nSize,lpNumberOfBytesRead);
end;

function ce_WriteProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;
{Calls the WriteProcessMemory version the rest of CE uses right now}
begin
  result:=WriteProcessMemory(hProcess,lpBaseAddress,lpBuffer,nSize,lpNumberOfBytesWritten);
end;



{$endif}

constructor TScriptengine.create;
var s: string;
begin
{$ifndef isdll}
  s:=extractfilepath(application.ExeName);
  SetEnvironmentVariable('UC_HOME',pchar(s));
{$endif}
//dll is already done by the injector

  currentthreadid:=0;
  //moved loading of the dll to first usage


  scriptEngineCS:=tcriticalsection.Create;
end;

function TScriptEngine.checkthread: boolean;
//This function will return true if the correct thread is using a function. (Used to make sure the correct thread uses the class at a time)
begin
  if currentthreadid<>0 then
    if getcurrentthreadid<>currentthreadid then
    begin
      result:=false;
      exit; //This is not the thread that started it
    end;

  result:=true;
end;


function TScriptEngine.beginScript:boolean;
var thisdll: dword;
    mfilename: pchar;
    dllpath: string;
begin
  if currentthreadid=getcurrentthreadid then
  begin
    result:=false;
    exit; //don't let a thread enter twice without a end\
  end;

  scriptEngineCS.enter; //will block other threads trying to enter
  currentthreadid:=getcurrentthreadid;

  if underclibrary=0 then //this way it gets loaded when it's actually used (saves some mem)
  begin
{$ifdef isdll}
    thisdll:=GetModuleHandle('undercdll.dll');
    getmem(mfilename,256);
    GetModuleFileName(thisdll,mfilename,256);
    dllpath:=mfilename;
    dllpath:=extractfilepath(dllpath);
    freemem(mfilename);

    underclibrary:=loadlibrary(pchar(dllpath+'\ucc12.dll'));

    if underclibrary=0 then
      messagebox(0,pchar(dllpath+'\ucc12.dll'),'error dllpath=',mb_ok);
{$else}
//    underclibrary:=loadlibrary('C:\Cheat Engine\underc\sourcecode\src\ddebug\ucc12.dll');
//    underclibrary:=loadlibrary('C:\Cheat Engine\underc\sourcecode\ucc12.dll');
    underclibrary:=loadlibrary('ucc12.dll');
{$endif}


    if (underclibrary=0) then
    begin
      messagebox(0,'ucc12.dll was not found. No c-scripting abilities','ucc12 not found',MB_ICONERROR or mb_ok);
      exit;
    end;

    uc_init:=getprocaddress(underclibrary,'_uc_init@8');
    uc_finis:=getprocaddress(underclibrary,'_uc_finis@0');
    uc_exec:=getprocaddress(underclibrary,'_uc_exec@4');
    uc_result:=getprocaddress(underclibrary,'_uc_result@8');
    uc_error:=getprocaddress(underclibrary,'_uc_error@8');
    uc_import:=getprocaddress(underclibrary,'_uc_import@8');
    uc_main:=getprocaddress(underclibrary,'_uc_main@8');
    uc_compile:=getprocaddress(underclibrary,'_uc_compile@8');
    uc_eval:=getprocaddress(underclibrary,'_uc_eval@12');
    uc_eval_args:=getprocaddress(underclibrary,'uc_eval_args');
    uc_get_function:=getprocaddress(underclibrary,'_uc_get_function@4');

    uc_init(nil,false);


{$ifndef isdll}
    //make some ce specific functions.
    uc_import('int xplusone(int)',@xplusone);
    uc_import('void ce_showmessage(char*)',@ce_showmessage);
    uc_import('int ce_ChangeRegistersAtAddress(unsigned long, void *)',@ce_ChangeRegistersAtAddress);
    uc_import('int ce_AutoAssemble(char *)',@ce_AutoAssemble);
    uc_import('int ce_Assembler(unsigned int, char *, unsigned char *, int, int *)',@ce_assembler);
    uc_import('int ce_Disassembler(unsigned int, char *, int)',@ce_Disassembler);
    uc_import('int ce_InjectDLL(char *, char *)',@ce_InjectDLL);
    uc_import('unsigned int ce_GetAddressFromPointer(unsigned int, int, unsigned int *)',@ce_GetaddressFromPointer);
    uc_import('int ce_GetSelectedProcessHandle(void)',@ce_GetSelectedProcessHandle);
    uc_import('int ce_ReadProcessMemory(unsigned int, void *, void *, unsigned long, void *)',@ce_ReadProcessMemory);
    uc_import('int ce_WriteProcessMemory(unsigned int, void *, void *, unsigned long, void *)',@ce_WriteProcessMemory);
{$endif}

//    scriptengine.uc_import('void abracadabra(int)',pointer($7c802442));
  end;




//  uc_init(nil,false);



  result:=true;
end;

function TScriptEngine.endScript:boolean;
{
release the critical section so another thread can use it if it likes
}
begin
  result:=false;
  if not checkthread then
    exit;

 // uc_finis;
//  freelibrary(underclibrary);
  currentthreadid:=0;
  scriptEngineCS.leave;

  result:=true;
end;

function TScriptEngine.execute_command(command: string): boolean;
begin
  result:=false;
  if not checkthread then
    exit;

  result:=uc_exec(pchar(command));
end;

function TScriptEngine.getResult: string;
var r: pchar;
    s: string;
begin
  r:=nil;
  if not checkthread then
    exit;

  getmem(r,128);
  uc_result(r,128);

  s:=r;

  freemem(r);

  result:=s;
end;

function TScriptEngine.getError: string;
var r: pchar;
    s: string;
begin
  r:=nil;
  if not checkthread then
    exit;

  getmem(r,128);
  uc_error(r,128);
  s:=r;

  freemem(r);

  result:=s;
end;


initialization
  scriptengine:=TScriptengine.create;

finalization
  //scriptengine.free;

end.




