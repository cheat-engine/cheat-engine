unit networkInterface;

{$mode objfpc}{$H+}

//playing arround with the objfpc notation. The pointer based arrays look interesting

interface

uses
  {$ifdef JNI}
  Classes, SysUtils, Sockets, resolve, ctypes,syncobjs, math, zstream,
  newkernelhandler, unixporthelper, processhandlerunit, gutil, gmap,VirtualQueryExCache;
  {$else}
  {$ifdef darwin}
  lcltype, MacTypes,macport,
  {$endif}
  {$ifdef windows}
  jwawindows, windows,
  {$endif}
  Classes, SysUtils, Sockets, resolve, ctypes, networkconfig,
  cefuncproc, newkernelhandler, math, zstream, syncobjs, ProcessHandlerUnit,
  VirtualQueryExCache, gutil, gmap;
  {$endif}



{$ifdef jni}
const networkcompression=0;
{$endif}


type

  TNetworkDebugEvent=packed record
    signal: integer;
    threadid: qword;
    case integer of
      -2: (//create process
      createProcess: packed record
        maxBreakpointCount: uint8;
        maxWatchpointCount: uint8;
        maxSharedBreakpoints: uint8;
      end; );
      5: (address: qword;  );
  end;

  TNetworkEnumSymCallback=function(modulename: string; symbolname: string; address: ptruint; size: integer; secondary: boolean ): boolean of object;

  TVQEMapCmp = specialize TLess<PtrUInt>;
  TVQEMap = specialize TMap<PtrUInt, TVirtualQueryExCache, TVQEMapCmp>;

  TCEServerOptionType=(netParent=0, //just a piece of text with child options under it
                       netBoolean=1, netInteger=2, netFloat=3, netDouble=4, netText=5);

  TCEServerOption=record
    optname: string;  //this is the internal name of the option
    parentoptname: string; //if this is a sibling, this holds the parent optname, else empty
    optdescription: string; //text shown to the user
    acceptablevalues: string; //allowed values (when set)
    currentvalue: string; //the current value (1 for true in boolean)
    optiontype: TCEServerOptionType;
  end;

  TCEServerOptions=array of TCEServerOption;


  TCEConnection=class
  private
    socket: cint;
    fConnected: boolean;

    version: integer;

    executesInsideTarget: boolean; //if true, ptrace won't work.  Use sigtrap for debugging

    //todo: change rpmcache to a map
    rpmcache: array [0..15] of record //every connection is thread specific, so each thread has it's own rpmcache
        lastupdate: TLargeInteger; //contains the last time this page was updated
        baseaddress: PtrUInt;
        memory: array [0..4095] of byte;
      end;

    WriteProcessMemoryBufferCount: integer; //to deal with recursive calls
    WriteProcessMemoryBuffer: array of record
        processhandle: thandle;
        baseaddress: PtrUInt;
        memory: array of byte;
    end;
    WriteProcessMemoryBufferCS: TCriticalSection;


    VirtualQueryExCacheMap: TVQEMap;
    VirtualQueryExCacheMapCS: TCriticalSection;


    function receiveString16: string; //read a string that is preceded by a 16 bit length indicator

    function receive(buffer: pointer; size: integer): integer;
    function send(buffer: pointer; size: integer): integer;

    function CReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: PTRUINT): BOOL;
    function NReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: PTRUINT): BOOL;



  public
    function isNetworkHandle(handle: THandle): boolean;

    function Module32Next(hSnapshot: HANDLE; var lpme: MODULEENTRY32; isfirst: boolean=false): BOOL;
    function Module32First(hSnapshot: HANDLE; var lpme: MODULEENTRY32): BOOL;

    function Thread32Next(hSnapshot: HANDLE; var lpte: THREADENTRY32; isfirst: boolean=false): BOOL;
    function Thread32First(hSnapshot: HANDLE; var lpte: THREADENTRY32): BOOL;

    function Process32Next(hSnapshot: HANDLE; var lppe: PROCESSENTRY32; isfirst: boolean=false): BOOL;
    function Process32First(hSnapshot: HANDLE; var lppe: PROCESSENTRY32): BOOL;
    function CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): HANDLE;
    function CloseHandle(handle: THandle):WINBOOL;
    function OpenProcess(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; dwProcessId:DWORD):HANDLE;
    function CreateRemoteThread(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer;  dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle;
    function VirtualAllocEx(hProcess: THandle; lpAddress: Pointer; dwSize, flAllocationType: DWORD; flProtect: DWORD): Pointer;
    function VirtualFreeEx(hProcess: HANDLE; lpAddress: LPVOID; dwSize: SIZE_T; dwFreeType: DWORD): BOOL;
    function VirtualQueryEx(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD;
    function VirtualQueryEx_StartCache(hProcess: THandle; flags: DWORD): boolean;
    procedure VirtualQueryEx_EndCache(hProcess: THandle);
    function VirtualProtectEx(hProcess: THandle; lpAddress: Pointer; dwSize, flNewProtect: DWORD; var OldProtect: DWORD): BOOL;

    function GetRegionInfo(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD; var mapsline: string): DWORD;

    function ReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: PTRUINT): BOOL;
    function WriteProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: PTRUINT): BOOL;
    procedure beginWriteProcessMemory;
    function endWriteProcessMemory: boolean;

    function StartDebug(hProcess: THandle): BOOL;
    function WaitForDebugEvent(hProcess: THandle; timeout: integer; var devent: TNetworkDebugEvent):BOOL;
    function ContinueDebugEvent(hProcess: THandle; threadid: dword; continuemethod: integer): BOOL;
    function SetBreakpoint(hProcess: THandle; threadid: integer; debugregister: integer; address: PtrUInt; bptype: integer; bpsize: integer): boolean;
    function RemoveBreakpoint(hProcess: THandle; threadid: integer; debugregister: integer; wasWatchpoint: boolean): boolean;
    function AllocateAndGetContext(hProcess: Thandle; threadid: integer): Pointer;
    function setContext(hProcess: Thandle; threadid: integer; context: pointer; contextsize: integer): boolean;
    function getVersion(var name: string): integer;
    function getArchitecture(hProcess: THandle): integer;
    function getABI: integer;
    function enumSymbolsFromFile(modulepath: string; fileoffset: dword; modulebase: ptruint; callback: TNetworkEnumSymCallback; nameaddendum: string=''): boolean;
    function loadModule(hProcess: THandle; modulepath: string): boolean;
    function loadModuleEx(hProcess: THandle; dlopenaddress: ptruint; modulepath: string): boolean;
    function loadExtension(hProcess: Thandle): boolean;
    function speedhack_setSpeed(hProcess: THandle; speed: single): boolean;
    procedure setConnectionName(name: string);

    procedure getOptions(var options: TCEServerOptions);
    function getOption(name: string):string;
    procedure setOption(name: string; value: string);

    function connectNamedPipe(name: string; timeout:integer=0): HANDLE;
    function readPipe(h: THandle; destination: pointer; size: integer; timeout:integer=0): boolean;
    function writePipe(h: THandle; source: pointer; size: integer; timeout:integer=0): boolean;

    function getServerPath: string;

    function isAndroid: boolean;

    function setCurrentPath(path: string): boolean;
    function getCurrentPath: string;
    procedure enumfiles(path: string; list: tstrings);
    function getFilePermissions(path: string; out perms: UINT32): boolean;
    function setFilePermissions(path: string; perms: UINT32): boolean;

    function getFile(path: string; s: tstream): boolean;
    function putFile(path: string; s: tstream): boolean;

    function createDir(path: string): boolean;
    function deleteFile(path: string): boolean;

    procedure TerminateServer;

    constructor create;
    destructor destroy; override;
  published
    property connected: boolean read fConnected;
    property path: string read getServerPath;
  end;


{$ifdef jni}
var
  host: THostAddr;
  port: integer;

{$endif}

implementation

uses elfsymbols, Globals, maps;

const
  CMD_GETVERSION =0;
  CMD_CLOSECONNECTION= 1;
  CMD_TERMINATESERVER= 2;
  CMD_OPENPROCESS= 3;
  CMD_CREATETOOLHELP32SNAPSHOT =4;
  CMD_PROCESS32FIRST= 5;
  CMD_PROCESS32NEXT= 6;
  CMD_CLOSEHANDLE=7;
  CMD_VIRTUALQUERYEX=8;
  CMD_READPROCESSMEMORY=9;
  CMD_WRITEPROCESSMEMORY=10;
  CMD_STARTDEBUG=11;
  CMD_STOPDEBUG=12;
  CMD_WAITFORDEBUGEVENT=13;
  CMD_CONTINUEFROMDEBUGEVENT=14;
  CMD_SETBREAKPOINT=15;
  CMD_REMOVEBREAKPOINT=16;
  CMD_SUSPENDTHREAD=17;
  CMD_RESUMETHREAD=18;
  CMD_GETTHREADCONTEXT=19;
  CMD_SETTHREADCONTEXT=20;
  CMD_GETARCHITECTURE=21;
  CMD_MODULE32FIRST=22;
  CMD_MODULE32NEXT=23;

  CMD_GETSYMBOLLISTFROMFILE=24;

  CMD_LOADEXTENSION=25;
  CMD_ALLOC=26;
  CMD_FREE=27;
  CMD_CREATETHREAD=28;
  CMD_LOADMODULE=29;
  CMD_SPEEDHACK_SETSPEED=30;

  //
  CMD_VIRTUALQUERYEXFULL=31;
  CMD_GETREGIONINFO=32; //extended version of VirtualQueryEx which also get the full string
  CMD_GETABI=33;  //for c-code compilation

  //4
  CMD_SET_CONNECTION_NAME=34;
  CMD_CREATETOOLHELP32SNAPSHOTEX =35;

  CMD_CHANGEMEMORYPROTECTION=36;
  CMD_GETOPTIONS=37;
  CMD_GETOPTION=38;
  CMD_SETOPTION=39;

  CMD_PTRACE_MMAP=40;

  CMD_OPENNAMEDPIPE=41;
  CMD_PIPEREAD=42;
  CMD_PIPEWRITE=43;


  CMD_GETCESERVERPATH=44;
  CMD_ISANDROID=45;

  CMD_LOADMODULEEX=46;

  CMD_SETCURRENTPATH=47;
  CMD_GETCURRENTPATH=48;
  CMD_ENUMFILES=49;
  CMD_GETFILEPERMISSIONS=50;
  CMD_SETFILEPERMISSIONS=51;
  CMD_GETFILE=52;
  CMD_PUTFILE=53;
  CMD_CREATEDIR=54;
  CMD_DELETEFILE=55;




type
  TLocalModuleListEntry=class
    baseaddress: ptruint;
    fileoffset: dword;
    size: dword;
    part: integer;
    name: string;
  end;

  TLocalThreadListEntry=class
    threadid: dword;
    ownerprocessid: dword;
  end;


{  TLocalProcessListEntry=class

  end;}

  TToolhelp32SnapshotInfo=class
    handle: dword;
    snapshottype: dword;
    list: tfplist;

  public
    constructor create;
    destructor destroy; override;
  end;


var
  LocalToolhelpSnapshotsCS: TCriticalSection;
  LocalToolhelpSnapshots: array of TToolhelp32SnapshotInfo;

constructor TToolhelp32SnapshotInfo.create;
begin
  list:=tfplist.create;
end;

destructor TToolhelp32SnapshotInfo.destroy;
var i: integer;
begin
  for i:=0 to list.count-1 do
    tobject(list[i]).destroy;

  list.free;
  list:=nil;

  inherited destroy;
end;

procedure TCEConnection.TerminateServer;
var command: byte;
begin
  command:=CMD_TERMINATESERVER;
  send(@command, sizeof(command));
end;




function TCEConnection.CloseHandle(handle: THandle):WINBOOL;
var CloseHandleCommand: packed record
    command: byte;
    handle: dword;
  end;

  r: integer;
  ths: TToolhelp32SnapshotInfo;
begin
  if ((handle shr 24) and $ff)= $cd then  //local fake handle
  begin
    try
      LocalToolhelpSnapshotsCS.enter;
      try
        ths:=LocalToolhelpSnapshots[handle and $ffffff];
        LocalToolhelpSnapshots[handle and $ffffff]:=nil;
      finally
        LocalToolhelpSnapshotsCS.Leave;
      end;

      if ths=nil then
      begin
        OutputDebugString('Tried to close invalid fake handle');
        exit(false);
      end;

      ths.free;
      result:=true;

    except
      result:=false;
    end;
  end
  else
  if ((handle shr 24) and $ff)= $ce then
  begin
    CloseHandleCommand.command:=CMD_CLOSEHANDLE;
    CloseHandleCommand.handle:=handle and $ffffff;
    send(@CloseHandleCommand, sizeof(CloseHandleCommand));

    receive(@r,sizeof(r));
    result:=true;
  end
  {$ifdef windows}
  else //not a network handle
    result:=windows.CloseHandle(handle)
  {$endif};
end;

function TCEConnection.Module32Next(hSnapshot: HANDLE; var lpme: MODULEENTRY32; isfirst: boolean=false): BOOL;
var ModulelistCommand: packed record
    command: byte;
    handle: dword;
  end;

  r: packed record
    result: integer;
    modulebase: qword;
    modulepart: dword;
    modulesize: dword;
    modulefileoffset: dword;
    stringlength: dword;
  end;

  mname: pchar;
  mnames: string;
  ths: TToolhelp32SnapshotInfo;
  index: integer;
  mle: TLocalModuleListEntry;
begin

  result:=false;

  if ((hSnapshot shr 24) and $ff)= $cd then
  begin
    //local snapshot
    LocalToolhelpSnapshotsCS.Enter;
    try
      ths:=LocalToolhelpSnapshots[hSnapshot and $ffffff];
    finally
      LocalToolhelpSnapshotsCS.Leave;
    end;

    if ths=nil then
    begin
      OutputDebugString('Module32First/Next on an invalid toolhelp handle');
      exit;
    end;

    if ths.snapshottype<>TH32CS_SNAPMODULE then exit;


    if isfirst then
      lpme.th32ModuleID:=0;

    if lpme.th32ModuleID>=ths.list.Count then exit(false);

    mle:=TLocalModuleListEntry(ths.list[lpme.th32ModuleID]);
    lpme.hModule:=mle.baseaddress;
    lpme.modBaseAddr:=pointer(mle.baseaddress);
    lpme.modBaseSize:=mle.size;
    lpme.GlblcntUsage:=mle.part;
    lpme.ProccntUsage:=mle.fileoffset;

    mnames:=mle.name;

    if mle.part>0 then
    asm
    nop
    end;

    copymemory(@lpme.szExePath[0], @mnames[1], min(length(mnames)+1, MAX_PATH));
    lpme.szExePath[MAX_PATH-1]:=#0;

    if mle.part<>0 then
      mnames:=mnames+'.'+inttostr(mle.part);

    copymemory(@lpme.szModule[0], @mnames[1], min(length(mnames)+1, MAX_MODULE_NAME32));
    lpme.szModule[MAX_MODULE_NAME32-1]:=#0;



    inc(lpme.th32ModuleID);
    exit(true);
  end;


  if ((hSnapshot shr 24) and $ff)= $ce then
  begin
    if isfirst then
      ModulelistCommand.command:=CMD_MODULE32FIRST
    else
      ModulelistCommand.command:=CMD_MODULE32NEXT;

    ModulelistCommand.handle:=hSnapshot and $ffffff;
    if send(@ModulelistCommand, sizeof(ModulelistCommand)) > 0 then
    begin
      ZeroMemory(@r,sizeof(r));
      if receive(@r, sizeof(r))>0 then
      begin
        result:=r.result<>0;

        if result then
        begin //it has a string
          getmem(mname, r.stringlength+1);
          receive(mname, r.stringlength);
          mname[r.stringlength]:=#0;

          mnames:=mname;

          if mname<>nil then
            FreeMemAndNil(mname);

          ZeroMemory(@lpme, sizeof(lpme));
          lpme.hModule:=r.modulebase;
          lpme.modBaseAddr:=pointer(r.modulebase);
          lpme.modBaseSize:=r.modulesize;
          lpme.GlblcntUsage:=r.modulepart;
          lpme.ProccntUsage:=r.modulefileoffset;
          {$ifdef darwin}
          lpme.is64bit:=processhandler.is64Bit;
          {$endif}

          if r.modulepart>0 then
          asm
          nop
          end;

          copymemory(@lpme.szExePath[0], @mnames[1], min(length(mnames)+1, MAX_PATH));
          lpme.szExePath[MAX_PATH-1]:=#0;

          if r.modulepart<>0 then
            mnames:=mnames+'.'+inttostr(mle.part);

          copymemory(@lpme.szModule[0], @mnames[1], min(length(mnames)+1, MAX_MODULE_NAME32));
          lpme.szModule[MAX_MODULE_NAME32-1]:=#0;
        end;

      end;
    end;
  end;

end;

function TCEConnection.Module32First(hSnapshot: HANDLE; var lpme: MODULEENTRY32): BOOL;
begin
  result:=module32next(hSnapshot, lpme, true);
end;

function TCEConnection.Thread32Next(hSnapshot: HANDLE; var lpte: THREADENTRY32; isfirst: boolean=false): BOOL;
var
  ths: TToolhelp32SnapshotInfo;
  index: integer;
  tle: TLocalThreadListEntry;
begin
  result:=false;

  if ((hSnapshot shr 24) and $ff)= $cd then
  begin
    //local snapshot
    LocalToolhelpSnapshotsCS.Enter;
    try
      ths:=LocalToolhelpSnapshots[hSnapshot and $ffffff];
    finally
      LocalToolhelpSnapshotsCS.Leave;
    end;

    if ths=nil then
    begin
      OutputDebugString('Module32First/Next on an invalid toolhelp handle');
      exit;
    end;

    if ths.snapshottype<>TH32CS_SNAPTHREAD then exit;


    if isfirst then
      lpte.dwFlags:=0;  //use dwFlags as counter

    if lpte.dwFlags>=ths.list.Count then exit(false);

    tle:=TLocalThreadListEntry(ths.list[lpte.dwFlags]);
    lpte.cntUsage:=0;
    lpte.tpDeltaPri:=0;
    lpte.tpBasePri:=0;
    lpte.th32OwnerProcessID:=tle.ownerprocessid;
    lpte.th32ThreadID:=tle.ThreadID;
    inc(lpte.dwFlags);
    exit(true);
  end;
  //else unhandled.

end;

function TCEConnection.Thread32First(hSnapshot: HANDLE; var lpte: THREADENTRY32): BOOL;
begin
  exit(thread32next(hSnapshot, lpte, true));
end;


function TCEConnection.Process32Next(hSnapshot: HANDLE; var lppe: PROCESSENTRY32; isfirst: boolean=false): BOOL;
var ProcesslistCommand: packed record
    command: byte;
    handle: dword;
  end;

  r: packed record
    result: integer;
    pid: dword;
    stringlength: dword;
  end;

  pname: pchar;

begin
  result:=false;

  //OutputDebugString('TCEConnection.Process32Next');
  if ((hSnapshot shr 24) and $ff)= $ce then
  begin
    //OutputDebugString('Valid network handle');

    if isfirst then
      ProcesslistCommand.command:=CMD_PROCESS32FIRST
    else
      ProcesslistCommand.command:=CMD_PROCESS32NEXT;

    ProcesslistCommand.handle:=hSnapshot and $ffffff;
    if send(@ProcesslistCommand, sizeof(ProcesslistCommand)) > 0 then
    begin
      if receive(@r, sizeof(r))>0 then
      begin
        result:=r.result<>0;

        if result then
        begin //it has a string
          getmem(pname, r.stringlength+1);
          receive(pname, r.stringlength);
          pname[r.stringlength]:=#0;

          ZeroMemory(@lppe, sizeof(lppe));
          lppe.th32ProcessID:=r.pid;


          CopyMemory(@lppe.szExeFile[0], pname, min(r.stringlength+1, MAX_PATH));
          lppe.szExeFile[MAX_PATH-1]:=#0;

          FreeMemAndNil(pname);
        end;

      end;
    end;
  end;
end;

function TCEConnection.Process32First(hSnapshot: HANDLE; var lppe: PROCESSENTRY32): BOOL;
begin
  OutputDebugString('TCEConnection.Process32First');
  result:=process32next(hSnapshot, lppe, true);
end;

function TCEConnection.CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): HANDLE;
var CTSCommand: packed record
    command: byte;
    dwFlags: dword;
    th32ProcessID: dword;
  end;

  r: integer;
  r2: packed record
        result: integer;
        modulebase: qword;
        modulepart: dword;
        modulesize: dword;
        modulefileoffset: dword;
        stringlength: dword;
    end;

  eol: boolean;

  mname: pchar;
  mnamesize: integer;

  ths: TToolhelp32SnapshotInfo;
  mle: TLocalModuleListEntry;
  tle: TLocalThreadListEntry;
  i: integer;

  threadlist: array of integer;

begin
  result:=0;

  OutputDebugString('TCEConnection.CreateToolhelp32Snapshot()');
  CTSCommand.command:=CMD_CREATETOOLHELP32SNAPSHOTEX;
  CTSCommand.dwFlags:=dwFlags;
  CTSCommand.th32ProcessID:=th32ProcessID;

  r:=0;
  if send(@CTSCommand, sizeof(CTSCommand))>0 then
  begin
    if (dwFlags and TH32CS_SNAPTHREAD)=TH32CS_SNAPTHREAD then
    begin
      //it'll send a list of threadid's now
      ths:=TToolhelp32SnapshotInfo.create;
      ths.handle:=0;
      ths.snapshottype:=TH32CS_SNAPTHREAD;

      LocalToolhelpSnapshotsCS.enter;
      try
        for i:=0 to length(LocalToolhelpSnapshots)-1 do
        begin
          if LocalToolhelpSnapshots[i]=nil then
          begin
            LocalToolhelpSnapshots[i]:=ths;
            ths.handle:=$cd000000 or i;
            break;
          end;
        end;

        if ths.handle=0 then
        begin
          i:=length(LocalToolhelpSnapshots);

          setlength(LocalToolhelpSnapshots,length(LocalToolhelpSnapshots)+1);
          LocalToolhelpSnapshots[i]:=ths;
          ths.handle:=$cd000000 or i;
        end;
      finally
        LocalToolhelpSnapshotsCS.leave;
      end;

      result:=ths.handle;

      if receive(@r, sizeof(r))>0 then
      begin
        setlength(threadlist, r);
        if receive(@threadlist[0],sizeof(integer)*r)>0 then
        begin
          for i:=0 to length(threadlist)-1 do
          begin
            tle:=TLocalThreadListEntry.create;
            tle.ownerprocessid:=th32processid;
            tle.threadid:=threadlist[i];
            ths.list.add(tle);
          end;
        end;
      end;


    end
    else
    if (dwFlags and TH32CS_SNAPMODULE)=TH32CS_SNAPMODULE then
    begin
      //it'll send me a list of modules now



      ths:=TToolhelp32SnapshotInfo.create;
      ths.handle:=0;
      ths.snapshottype:=TH32CS_SNAPMODULE;

      LocalToolhelpSnapshotsCS.enter;
      try
        for i:=0 to length(LocalToolhelpSnapshots)-1 do
        begin
          if LocalToolhelpSnapshots[i]=nil then
          begin
            LocalToolhelpSnapshots[i]:=ths;
            ths.handle:=$cd000000 or i;
            break;
          end;
        end;

        if ths.handle=0 then
        begin
          i:=length(LocalToolhelpSnapshots);

          setlength(LocalToolhelpSnapshots,length(LocalToolhelpSnapshots)+1);
          LocalToolhelpSnapshots[i]:=ths;
          ths.handle:=$cd000000 or i;
        end;
      finally
        LocalToolhelpSnapshotsCS.leave;
      end;

      result:=ths.handle;

      eol:=false;
      mnamesize:=512;
      getmem(mname, mnamesize);
      while not eol do
      begin
        if receive(@r2, sizeof(r2))>0 then
        begin
          if r2.stringlength>mnamesize then //needs more memory
          begin
            freemem(mname);
            mnamesize:=r2.stringlength;
            getmem(mname, mnamesize);
          end;


          receive(mname, r2.stringlength);
          mname[r2.stringlength]:=#0;

          eol:=r2.result<>1;

          if not eol then
          begin
            mle:=TLocalModuleListEntry.create;
            mle.baseaddress:=r2.modulebase;
            mle.part:=r2.modulepart;
            mle.size:=r2.modulesize;
            mle.fileoffset:=r2.modulefileoffset;
            mle.name:=mname;
            ths.list.add(mle);
          end;
        end;


      end;
    end
    else //not handled yet
    if receive(@r, sizeof(r))>0 then
    begin
      if (r>0) then
        r:=r or $ce000000;

      result:=r;
    end;
  end;
end;

function TCEConnection.CReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: PTRUINT): BOOL;
//cached read process memory. Split up into pagechunks and check which pages need to be cached, and then read from the caches
type TPageInfo=record
    startaddress: ptruint;
    memory: pbyte;
  end;

var pages: array of TPageInfo;
  pagecount: ptruint;
  i,j: integer;

  oldest: integer;

  freq: tlargeinteger;
  currenttime: tlargeinteger;

  x: ptruint;
  blockoffset, blocksize: dword;
  currentbase: ptruint;
  currenttarget: ptruint;

  m: PByte;
begin
  //log(format('TCEConnection.CReadProcessMemory: Read %d bytes from %p into %p',[nsize, lpBaseAddress, lpBuffer]));


  result:=false;
  lpNumberOfBytesRead:=0;

  currenttarget:=ptruint(lpBaseAddress)+nsize-1;
  if currenttarget<ptruint(lpBaseAddress) then //overflow
  begin
    pagecount:=2+(currenttarget shr 12);
  end
  else
    pagecount:=1+((ptruint(lpBaseAddress)+nSize-1) shr 12) - (ptruint(lpBaseAddress) shr 12);

  setlength(pages, pagecount);

  {$ifdef windows}
  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(currenttime);
  {$else}
  currenttime:=gettickcount64;
  {$endif}

  //if ptruint(lpBaseAddress)=$40000c then


  for i:=0 to pagecount-1 do
  begin
    pages[i].startaddress:=(ptruint(lpBaseAddress) and ptruint(not $fff)) + (i*4096);
    pages[i].memory:=nil;
    //find the mapped page, if not found, map it

    oldest:=0;




    for j:=0 to 15 do
    begin
      if rpmcache[j].baseaddress=pages[i].startaddress then
      begin
        //check if the page is too old

        if ((currenttime-rpmcache[j].lastupdate) {$ifdef windows}/ freq{$endif}) > networkRPMCacheTimeout then //too old, refetch
          oldest:=i //so it gets reused
        else //not too old, can still be used
          pages[i].memory:=@rpmcache[j].memory[0];

        break;
      end;

      if (rpmcache[j].lastupdate<rpmcache[oldest].lastupdate) then
        oldest:=j;
    end;

    if pages[i].memory=nil then
    begin
      //map this page to the oldest entry
      pages[i].memory:=@(rpmcache[oldest].memory[0]);
      if not NReadProcessMemory(hProcess, pointer(pages[i].startaddress), pages[i].memory, 4096, x) then
      begin
        if i=0 then exit; //no need to continue, the start is unreadable
        pages[i].memory:=nil; //mark as unreadable, perhaps a few bytes can still be read
      end
      else
      begin
        rpmcache[oldest].lastupdate:=currenttime; //successful read
        rpmcache[oldest].baseaddress:=pages[i].startaddress;
      end;
    end;
  end;

  //all pages should be mapped now, so start copying till the end or till a nil map is encountered

  currentbase:=ptruint(lpbaseaddress);
  currenttarget:=ptruint(lpBuffer);
  for i:=0 to pagecount-1 do
  begin
    if pages[i].memory=nil then exit; //done

    m:=pbyte(pages[i].memory);

    //check what part of this page can be copied
    blockoffset:=currentbase and $fff; //start in this block
    blocksize:=min(nsize, 4096-blockoffset);

    CopyMemory(pointer(currenttarget), @m[blockoffset], blocksize);

    currentbase:=currentbase+blocksize; //next page
    currenttarget:=currenttarget+blocksize;
    lpNumberOfBytesRead:=lpNumberOfBytesRead+blocksize;

    nsize:=nsize-blocksize;
  end;

  result:=true; //everything got copied


end;

function TCEConnection.NReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: PTRUINT): BOOL;
//Network read process memory
var
  input: packed record
    command: byte;
    handle: UINT32;
    baseaddress: UINT64;
    size: UINT32;
    compressed: UINT8;
  end;

  output: packed record
    bytesread: integer;
    //followed by the bytes
  end;

  compressedresult: packed record
    uncompressedsize: uint32;
    compressedsize: uint32;
  end;

  compressedbuffer: tmemorystream;
  d: Tdecompressionstream;
begin
  result:=false;
  lpNumberOfBytesRead:=0;

  //still here so not everything was cached
  input.command:=CMD_READPROCESSMEMORY;
  input.handle:=hProcess;
  input.baseaddress:=ptruint(lpBaseAddress);
  input.size:=nSize;
  if nsize>128 then
    input.compressed:=networkcompression
  else
    input.compressed:=0;

  if send(@input, sizeof(input))>0 then
  begin
    if input.compressed<>0 then
    begin
      if receive(@compressedresult, sizeof(compressedresult))>0 then
      begin
        compressedbuffer:=tmemorystream.create;
        try
          compressedbuffer.Size:=compressedresult.compressedsize;
          if receive(compressedbuffer.Memory, compressedresult.compressedsize)>0 then
          begin
            //decompress this
            d:=Tdecompressionstream.create(compressedbuffer, false);
            try
              d.ReadBuffer(lpbuffer^, compressedresult.uncompressedsize);
              result:=compressedresult.uncompressedsize>0;
              lpNumberOfBytesRead:=compressedresult.uncompressedsize;
            finally
              d.free;
            end;

          end;


        finally
          compressedbuffer.free;
        end;
      end;
    end
    else
    begin
      //not compressed
      if receive(@output, sizeof(output))>0 then
      begin
        if output.bytesread>0 then
        begin
          if receive(lpBuffer, output.bytesread)>0 then
          begin
            result:=true;
            lpNumberOfBytesRead:=output.bytesread;
          end;

        end;
      end;//else connection error
    end;
  end;
end;

function TCEConnection.ReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: PTRUINT): BOOL;
begin
 // log(format('TCEConnection.ReadProcessMemory: Read %d bytes from %p into %p',[nsize, lpBaseAddress, lpBuffer]));


  if ((hProcess shr 24) and $ff)= $ce then
  begin
   // Log('hProcess is valid');

    result:=false;
    lpNumberOfBytesRead:=0;

    hProcess:=hProcess and $ffffff;

    if nsize=0 then exit;

    if (nsize<=8192) then
    begin
      //log('nsize<=8192. Calling CReadProcessMemory');
      result:=CReadProcessMemory(hProcess, lpBaseAddress, lpBuffer, nsize, lpNumberOfBytesRead);
    end
    else //just fetch it all from the net , ce usually does not fetch more than 8KB for random accesses, so would be a waste of time
    begin
     // log('nsize>8192. Calling NReadProcessMemory');
      result:=NReadProcessMemory(hProcess, lpBaseaddress, lpbuffer, nsize, lpNumberOfBytesRead);
    end;



  end
  {$ifdef windows}
  else
    result:=windows.ReadProcessMemory(hProcess, lpBaseAddress, lpBuffer, nSize, lpNumberOfBytesRead)
  {$endif};
end;

function TCEConnection.WriteProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: PTRUINT): BOOL;
type TWPMrecord=packed record
      command: byte;
      handle: integer;
      baseaddress: qword;
      size: integer;
      //buffer
    end;
    PWPMRecord=^TWPMRecord;
var
  input: PWPMrecord;
  i: pbyte;

  output: packed record
    byteswritten: integer;
    //followed by the bytes
  end;

  j,k: integer;

  b: ptruint;

begin
  WriteProcessMemoryBufferCS.enter;
  try

    if WriteProcessMemoryBufferCount=0 then
    begin
      if ((hProcess shr 24) and $ff)= $ce then
      begin
        result:=false;
        lpNumberOfBytesWritten:=0;

        input:=getmem(sizeof(TWPMRecord)+nSize);

        input^.command:=CMD_WRITEPROCESSMEMORY;
        input^.handle:=hProcess and $ffffff;
        input^.baseaddress:=ptruint(lpBaseAddress);
        input^.size:=nSize;

        CopyMemory(@input[1], lpBuffer, nSize);


        if send(input, sizeof(TWPMRecord)+nSize)>0 then
        begin
          if receive(@output, sizeof(output))>0 then
          begin
            if output.byteswritten>0 then
            begin
              result:=true;
              lpNumberOfBytesWritten:=output.byteswritten;

              if (lpNumberOfBytesWritten>0) then //for 1 byte changes
              begin
                //clear rpm cache for this entry if there is one

                b:=ptruint(lpBaseAddress) and (not $fff);
                for j:=0 to 15 do
                  if rpmcache[j].baseaddress=b then
                    rpmcache[j].lastupdate:=0; //set to outdated
              end;
            end;
          end;
        end;

        FreeMemAndNil(input);
      end
      {$ifdef windows}
      else
        result:=windows.WriteProcessMemory(hProcess, lpBaseAddress, lpBuffer, nSize, lpNumberOfBytesWritten)
      {$endif};

    end
    else
    begin
      //add it to the buffer
      result:=true;
      lpNumberOfBytesWritten:=nsize;

      k:=length(WriteProcessMemoryBuffer);
      setlength(WriteProcessMemoryBuffer, k+1);

      WriteProcessMemoryBuffer[k].processhandle:=ProcessHandle;
      WriteProcessMemoryBuffer[k].baseaddress:=ptruint(lpBaseAddress);
      setlength(WriteProcessMemoryBuffer[k].memory, nsize);
      CopyMemory(@WriteProcessMemoryBuffer[k].memory[0], lpBuffer, nsize);
    end;

  finally
    WriteProcessMemoryBufferCS.leave;
  end;
end;

procedure TCEConnection.beginWriteProcessMemory;
begin
  WriteProcessMemoryBufferCS.Enter;
  inc(WriteProcessMemoryBufferCount);

  //todo: Change the network interface to actually send ALL writes in one command
end;

function TCEConnection.endWriteProcessMemory: boolean;
var
  i,j,k: integer;
  x: ptruint;

  grouped: boolean;
begin
  result:=true;
  dec(WriteProcessMemoryBufferCount);

  if WriteProcessMemoryBufferCount=0 then
  begin
    //group the blocks
    i:=0;

    while i<length(WriteProcessMemoryBuffer)-1 do
    begin
      grouped:=false;
      //find a block that overlaps

      j:=i+1;

      while j<=length(WriteProcessMemoryBuffer)-1 do
      begin
        if InRangeX(WriteProcessMemoryBuffer[i].baseaddress, WriteProcessMemoryBuffer[j].baseaddress, WriteProcessMemoryBuffer[j].baseaddress+length(WriteProcessMemoryBuffer[j].memory)) or
           InRangeX(WriteProcessMemoryBuffer[j].baseaddress, WriteProcessMemoryBuffer[i].baseaddress, WriteProcessMemoryBuffer[i].baseaddress+length(WriteProcessMemoryBuffer[i].memory)) then
        begin
          k:=WriteProcessMemoryBuffer[i].baseaddress-WriteProcessMemoryBuffer[j].baseaddress;
          if k>0 then
          begin
            setlength(WriteProcessMemoryBuffer[i].memory, length(WriteProcessMemoryBuffer[i].memory)+k);
            MoveMemory(@WriteProcessMemoryBuffer[i].memory[k], @WriteProcessMemoryBuffer[i].memory[0], k );

            WriteProcessMemoryBuffer[i].baseaddress:=WriteProcessMemoryBuffer[j].baseaddress;
          end;

          //set the end
          k:=(WriteProcessMemoryBuffer[j].baseaddress+length(WriteProcessMemoryBuffer[j].memory))-(WriteProcessMemoryBuffer[i].baseaddress+length(WriteProcessMemoryBuffer[i].memory));  //get rhe bytes that need to be added
          if k>0 then //increase the size
            setlength(WriteProcessMemoryBuffer[i].memory, length(WriteProcessMemoryBuffer[i].memory)+k);

          //copy the bytes
          k:=WriteProcessMemoryBuffer[j].baseaddress-WriteProcessMemoryBuffer[i].baseaddress;
          copymemory(@WriteProcessMemoryBuffer[i].memory[k], @WriteProcessMemoryBuffer[j].memory[0], length(WriteProcessMemoryBuffer[j].memory));


          grouped:=true;

          //delete this from the list
          for k:=j to length(WriteProcessMemoryBuffer)-2 do
            WriteProcessMemoryBuffer[k]:=WriteProcessMemoryBuffer[k+1];

          setlength(WriteProcessMemoryBuffer, length(WriteProcessMemoryBuffer)-1);
        end
        else
          inc(j);
      end;

      if not grouped then //next one
        inc(i);
    end;

    //write
    for i:=0 to length(WriteProcessMemoryBuffer)-1 do
    begin
      if not WriteProcessMemory(WriteProcessMemoryBuffer[i].processhandle, pointer(WriteProcessMemoryBuffer[i].baseaddress), @WriteProcessMemoryBuffer[i].memory[0], length(WriteProcessMemoryBuffer[i].memory), x) then
        result:=false;
    end;


  end;

  WriteProcessMemoryBufferCS.Leave;
end;

function TCEConnection.CreateRemoteThread(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer;  dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle;
var
  input: packed record
    command: byte;
    hProcess: integer;
    startaddress: qword;
    parameter: qword;
  end;
  output: integer;
begin
  if isNetworkHandle(hProcess) then
  begin
    result:=0;
    input.command:=CMD_CREATETHREAD;
    input.hProcess:=hProcess and $ffffff;
    input.startaddress:=ptruint(lpStartAddress);
    input.parameter:=ptruint(lpParameter);

    if send(@input, sizeof(input))>0 then
    begin
      output:=0;
      receive(@output, sizeof(output));
      result:=output;

      if (result>0) then //mark it as a network handle
        result:=result or $ce000000;

      lpThreadId:=result; //for now
    end;

  end
  {$ifdef windows}
  else
    result:=windows.CreateRemoteThread(hProcess, lpThreadAttributes, dwStackSize, lpStartAddress, lpParameter, dwCreationFlags, lpThreadId);
  {$endif}
end;

function TCEConnection.VirtualAllocEx(hProcess: THandle; lpAddress: Pointer; dwSize, flAllocationType: DWORD; flProtect: DWORD): pointer;
var
  input: packed record
    command: byte;
    hProcess: integer;
    preferedBase: qword;
    size: integer;
    windowsprotection: integer;
  end;

  output: UINT64;
begin
  result:=nil;

  if isNetworkHandle(hProcess) then
  begin
    input.command:=CMD_ALLOC;
    input.hProcess:=hProcess and $ffffff;
    input.preferedBase:=ptruint(lpAddress);
    input.windowsprotection:=flProtect;
    input.size:=dwsize;

    if send(@input, sizeof(input))>0 then
    begin
      output:=0;
      receive(@output, sizeof(output));
      result:=pointer(output);
    end;

  end
  {$ifdef windows}
  else
    result:=windows.VirtualAllocEx(hProcess, lpAddress, dwSize, flAllocationType, flProtect);
  {$endif}
end;

function TCEConnection.VirtualFreeEx(hProcess: HANDLE; lpAddress: LPVOID; dwSize: SIZE_T; dwFreeType: DWORD): BOOL;
var
  input: packed record
    command: byte;
    hProcess: integer;
    address: qword;
    size: integer;
  end;

  r: UINT32;
begin
  r:=0;

  if isNetworkHandle(hProcess) then
  begin
    result:=false;
    input.command:=CMD_FREE;
    input.hProcess:=hProcess and $ffffff;
    input.address:=ptruint(lpAddress);
    input.size:=dwsize;

    if send(@input, sizeof(input))>0 then
    begin
      r:=0;
      receive(@r, sizeof(r));
      result:=r<>0;
    end;

  end
  {$ifdef windows}
  else
    result:=windows.VirtualFreeEx(hProcess, lpAddress, dwSize, dwFreeType);
  {$endif}
end;

function TCEConnection.VirtualQueryEx_StartCache(hProcess: THandle; flags: DWORD): boolean;
var
  vqec: TVirtualQueryExCache;
  input: packed record
    command: byte;
    handle: integer;
    flags: byte;
  end;

  vqe_entry: packed record
    baseaddress: qword;
    size: qword;
    protection: dword;
    _type: dword;
  end;

  mbi: TMEMORYBASICINFORMATION;

  count: UINT32;
  i: UINT32;

  nextAddress: qword;
begin
  result:=false;

  //check if this processhandle is already cached, and if so, first call endcache on it
  VirtualQueryEx_EndCache(hProcess); //clean up if there is already one

  if isNetworkHandle(hProcess) then
  begin
    vqec:=TVirtualQueryExCache.create(hProcess);

    //fill it

    input.command:=CMD_VIRTUALQUERYEXFULL;
    input.handle:=hProcess and $ffffff;
    input.flags:=flags;

    if send(@input, sizeof(input))>0 then
    begin
      //
      //ceserver will now send the number of entries followed by the entries themself
      nextAddress:=0;

      receive(@count, sizeof(count));

      for i:=0 to count-1 do
      begin
        receive(@vqe_entry, sizeof(vqe_entry));

        if (vqe_entry.baseaddress<>nextAddress) then
        begin
          mbi.baseaddress:=pointer(nextAddress);
          mbi.allocationbase:=mbi.BaseAddress;
          mbi.AllocationProtect:=PAGE_NOACCESS;
          mbi.protect:=PAGE_NOACCESS;
          mbi.State:=MEM_FREE;
          mbi._Type:=0;
          mbi.RegionSize:=vqe_entry.baseaddress-nextaddress;
          vqec.AddRegion(mbi);
        end;

        mbi.BaseAddress:=pointer(vqe_entry.baseaddress);
        mbi.AllocationBase:=mbi.BaseAddress;
        mbi.AllocationProtect:=PAGE_EXECUTE_READWRITE;
        mbi.Protect:=vqe_entry.protection;

        if vqe_entry.protection=PAGE_NOACCESS then
        begin
          mbi.State:=MEM_FREE;
          mbi._Type:=0;
        end
        else
        begin
          mbi.State:=MEM_COMMIT;
          mbi._Type:=vqe_entry._type;
        end;

        mbi.RegionSize:=vqe_entry.size;
        vqec.AddRegion(mbi);

        nextaddress:=vqe_entry.baseaddress+vqe_entry.size;
      end;


    end
    else
      exit; //fail to cache

    //and add it to the map
    VirtualQueryExCacheMapCS.enter;
    try
      VirtualQueryExCacheMap.insert(hProcess, vqec);
    finally
      VirtualQueryExCacheMapCS.Leave;
    end;
  end; //don't do anything

end;

procedure TCEConnection.VirtualQueryEx_EndCache(hProcess: THandle);
var vqecache: TVirtualQueryExCache;
begin
  //find the current cache of this hProcess
  VirtualQueryExCacheMapCS.enter;
  try
    if VirtualQueryExCacheMap.TryGetValue(hProcess, vqecache) then
    begin
      //cleanup
      VirtualQueryExCacheMap.Delete(hProcess);
      vqecache.free;
    end;
  finally
    VirtualQueryExCacheMapCS.leave;
  end;
end;

function TCEConnection.VirtualQueryEx(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD;
var
  input: packed record
    command: byte;
    handle: integer;
    baseaddress: qword;
  end;

  output: packed record
    result: byte;
    protection: dword;
    _type: dword;
    baseaddress: qword;
    size: qword;
  end;

  vqecache: TVirtualQueryExCache;

begin
  result:=0;

  if VirtualQueryExCacheMap.TryGetValue(hProcess, vqecache) then  //check if there is a cache going on for this handle
  begin
    //yes, get it from the cache
    if vqecache.getRegion(ptruint(lpAddress), lpBuffer) then
      result:=sizeof(lpBuffer)
    else
      result:=0;
  end
  else
  begin
    //no, use the slow method instead
    if isNetworkHandle(hProcess) then
    begin
      result:=0;
      input.command:=CMD_VIRTUALQUERYEX;
      input.handle:=hProcess and $ffffff;
      input.baseaddress:=qword(lpAddress);
      if send(@input, sizeof(input))>0 then
      begin
        if receive(@output, sizeof(output))>0 then
        begin
          if output.result>0 then
          begin
            lpBuffer.BaseAddress:=pointer(output.baseaddress);
            lpBuffer.AllocationBase:=lpBuffer.BaseAddress;
            lpbuffer.AllocationProtect:=PAGE_NOACCESS;
            lpbuffer.Protect:=output.protection;

            if output.protection=PAGE_NOACCESS then
            begin
              lpbuffer.State:=MEM_FREE;
              lpbuffer._Type:=0;
            end
            else
            begin
              lpbuffer.State:=MEM_COMMIT;
              lpbuffer._Type:=output._type;
            end;



            lpbuffer.RegionSize:=output.size;

            result:=dwlength;
          end
          else
            result:=0;
        end;
      end;

    end
    {$ifdef windows}
    else
      result:=windows.VirtualQueryEx(hProcess, lpAddress, lpBuffer, dwLength)
    {$endif};
  end;
end;

function TCEConnection.VirtualProtectEx(hProcess: THandle; lpAddress: Pointer; dwSize, flNewProtect: DWORD; var OldProtect: DWORD): BOOL;
var
  input: packed record
    command: byte;
    handle: integer;
    address: qword;
    size: UINT32;
    newprotect: uint32;

  end;

  output: packed record
    result: uint32;
    oldprotection: uint32;
  end;
begin
  result:=false;

  if isNetworkHandle(hProcess) then
  begin
    input.command:=CMD_CHANGEMEMORYPROTECTION;
    input.handle:=hProcess and $ffffff;
    input.address:=qword(lpAddress);
    input.size:=dwsize;
    input.newprotect:=flNewProtect;
    if send(@input, sizeof(input))>0 then
    begin
      if receive(@output, sizeof(output))>0 then
      begin
        if output.result=0 then
        begin
          oldprotect:=output.oldprotection;
          exit(true);
        end
        else
          exit(false);
      end
      else
      begin
        OutputDebugString('CMD_CHANGEMEMORYPROTECTION failed');
      end;
    end;

  end
  {$ifdef windows}
  else
    result:=windows.VirtualProtectEx(hProcess, lpAddress, dwSize, flNewProtect, @OldProtect);
  {$endif};

end;

function TCEConnection.GetRegionInfo(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD; var mapsline: string): DWORD;
var
  input: packed record
    command: byte;
    handle: integer;
    baseaddress: qword;
  end;

  output: packed record
    result: byte;
    protection: dword;
    _type: dword;
    baseaddress: qword;
    size: qword;
  end;

  mapslinesize: byte;

  ml: pchar;
begin
  result:=0;

  log('TCEConnection.GetRegionInfo');

  if isNetworkHandle(hProcess) then
  begin
    log('valid handle');
    result:=0;
    input.command:=CMD_GETREGIONINFO;
    input.handle:=hProcess and $ffffff;
    input.baseaddress:=qword(lpAddress);
    if send(@input, sizeof(input))>0 then
    begin
      if receive(@output, sizeof(output))>0 then
      begin
        if output.result>0 then
        begin
          lpBuffer.BaseAddress:=pointer(output.baseaddress);
          lpBuffer.AllocationBase:=lpBuffer.BaseAddress;
          lpbuffer.AllocationProtect:=PAGE_NOACCESS;
          lpbuffer.Protect:=output.protection;

          if output.protection=PAGE_NOACCESS then
          begin
            lpbuffer.State:=MEM_FREE;
            lpbuffer._Type:=0;
          end
          else
          begin
            lpbuffer.State:=MEM_COMMIT;
            lpbuffer._Type:=output._type;
          end;



          lpbuffer.RegionSize:=output.size;

          result:=dwlength;
        end
        else
          result:=0;
      end;

      //extended part of CMD_GETREGIONINFO;
      log('receiving extended state');
      if receive(@mapslinesize, sizeof(mapslinesize))>0 then
      begin
        log('received extended state');
        log('mapelinesize='+inttostr(mapslinesize));

        getmem(ml, mapslinesize+1);
        if (ml<>nil) then
        begin
          receive(ml, mapslinesize);
          ml[mapslinesize]:=#0;

          mapsline:=ml;
          FreeMemAndNil(ml);
        end;
      end;


    end;

  end;
end;

function TCEConnection.OpenProcess(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; dwProcessId:DWORD):HANDLE;
var OpenProcessCommand: packed record
    command: byte;
    pid: integer;
  end;

  var h: integer;
begin
  result:=0;
  h:=0;
  OpenProcessCommand.command:=CMD_OPENPROCESS;
  OpenProcessCommand.pid:=dwProcessID;
  if send(@OpenProcessCommand, sizeof(OpenProcessCommand))>0 then
    if receive(@h, sizeof(h))>0 then
    begin
      if (h>0) then
        h:=h or $ce000000;
      result:=h;
    end;
end;

function TCEConnection.StartDebug(hProcess: THandle): BOOL;
var Input: packed record
    command: byte;
    handle: integer;
  end;

var Output: packed record
    result: integer;
  end;
begin
  result:=false;
  if ((hProcess shr 24) and $ff)= $ce then
  begin
    input.command:=CMD_STARTDEBUG;
    input.handle:=hProcess and $ffffff;
    if send(@input, sizeof(input))>0 then
    begin
      if receive(@output, sizeof(output))>0 then
      begin
        result:=output.result<>0;
      end;
    end;
  end;

end;

function TCEConnection.ContinueDebugEvent(hProcess: THandle; threadid: dword; continuemethod: integer): BOOL;
var
  input: packed record
    command: byte;
    handle: integer;
    threadid: dword;
    continuemethod: integer;
  end;
  r: integer;
begin

  result:=false;

  if ((hProcess shr 24) and $ff)= $ce then
  begin
    input.command:=CMD_CONTINUEFROMDEBUGEVENT;
    input.handle:=hProcess and $ffffff;
    input.threadid:=threadid;
    input.continuemethod:=continuemethod;
    if send(@input, sizeof(input))>0 then
    begin
      if receive(@r, sizeof(r))>0 then
        result:=r<>0;
    end;

  end;



end;

function TCEConnection.WaitForDebugEvent(hProcess: THandle; timeout: integer; var devent: TNetworkDebugEvent):BOOL;
var
  Input: packed record
    command: byte;
    handle: integer;
    timeout: integer;
  end;

  r: integer;

begin
  result:=false;

  if ((hProcess shr 24) and $ff)= $ce then
  begin
    input.command:=CMD_WAITFORDEBUGEVENT;
    input.handle:=hProcess and $ffffff;
    input.timeout:=timeout;
    if send(@input, sizeof(input))>0 then
    begin
      if receive(@r, sizeof(r))>0 then
      begin
        result:=r<>0;
        if result then
          result:=receive(@devent, sizeof(TNetworkDebugEvent))>0;

      end;



    end;
  end;




end;


function TCEConnection.SetBreakpoint(hProcess: THandle; threadid: integer; debugregister: integer; address: PtrUInt; bptype: integer; bpsize: integer): boolean;
var
  input: packed record
    command: byte;
    handle: integer;
    tid: integer;
    debugregister: integer;
    address: qword;
    bptype: integer;
    bpsize: integer;
  end;

  r: integer;

begin
  result:=false;

  if ((hProcess shr 24) and $ff)= $ce then
  begin
    input.command:=CMD_SETBREAKPOINT;
    input.handle:=hProcess and $ffffff;
    input.tid:=threadid;
    input.debugregister:=debugregister;

    input.address:=address;
    input.bptype:=bptype;
    input.bpsize:=bpsize;

    if send(@input, sizeof(input))>0 then
    begin
      if receive(@r, sizeof(r))>0 then
        result:=r<>0;
    end;
  end;

end;

function TCEConnection.RemoveBreakpoint(hProcess: THandle; threadid: integer; debugregister: integer; wasWatchpoint: boolean): boolean;
var
  input: packed record
    command: byte;
    handle: integer;
    tid: integer;
    debugregister: integer;
    wasWatchpoint: integer;
  end;

  r: integer;

begin
  result:=false;

  if ((hProcess shr 24) and $ff)= $ce then
  begin
    input.command:=CMD_REMOVEBREAKPOINT;
    input.handle:=hProcess and $ffffff;
    input.tid:=threadid;
    input.debugregister:=debugregister;
    if wasWatchpoint then
      input.wasWatchpoint:=1
    else
      input.wasWatchpoint:=0;


    if send(@input, sizeof(input))>0 then
    begin
      if receive(@r, sizeof(r))>0 then
        result:=r<>0;
    end;
  end;

end;

function TCEConnection.setContext(hProcess: Thandle; threadid: integer; context: pointer; contextsize: integer): boolean;
var
  input: packed record
    command: UINT8;
    hProcess: uint32;
    threadid: uint32;
    contextsize: uint32;
  end;
  r: uint32;
begin
  result:=false;
  input.command:=CMD_SETTHREADCONTEXT;
  input.hProcess:=hProcess and $ffffff;
  input.threadid:=threadid;
  input.contextsize:=ContextSize;

  if send(@input, sizeof(input))>0 then
  begin
    if send(context, contextsize)>0 then
    begin
      if receive(@r,sizeof(r))>0 then
        result:=r<>0;
    end;
  end;
end;

function TCEConnection.AllocateAndGetContext(hProcess: Thandle; threadid: integer): pointer;
//get he context and save it in an allocated memory block of variable size. The caller is responsible for freeing this block

var
  Input: packed record
    command: UINT8;
    hprocess: uint32;
    threadid: uint32;
  end;

  contextsize: UINT32;
  r: integer;

begin
  result:=nil;
  input.command:=CMD_GETTHREADCONTEXT;
  input.hprocess:=hProcess and $ffffff;
  input.threadid:=threadid;


  if send(@input, sizeof(input))>0 then
  begin
    if receive(@r, sizeof(r))>0 then
    begin
      if (r<>0) and (receive(@contextsize, sizeof(contextsize))>0) then
      begin
        getmem(result,  contextsize);
        if receive(result, contextsize)=0 then
        begin
          FreeMemAndNil(result);
          result:=nil;
        end;
      end;
    end;
  end;
end;

function TCEConnection.getVersion(var name: string): integer;
var CeVersion: packed record
  version: integer;
  stringsize: byte;
end;
  _name: pchar;

  command: byte;
begin
  result:=0;
  command:=CMD_GETVERSION;
  if send(@command, 1)>0 then
  begin
    if receive(@CeVersion, sizeof(CeVersion))>0 then
    begin
      getmem(_name, CeVersion.stringsize+1);
      receive(_name, CeVersion.stringsize);
      _name[CeVersion.stringsize]:=#0;

      name:=_name;
      FreeMemAndNil(_name);

      result:=CeVersion.version;

      if copy(_name,1,3)='lib' then
        executesInsideTarget:=true;


      self.version:=result;
    end;
  end;
end;

function TCEConnection.getArchitecture(hProcess: THandle): integer;
var input: packed record
    command: byte;
    hprocess: uint32;
  end;
  r: byte;
begin
  result:=0;
  input.command:=CMD_GETARCHITECTURE;
  input.hprocess:=hProcess and $ffffff;
  if send(@input, sizeof(input))>0 then
    if receive(@r, 1)>0 then
      result:=r;
end;

function TCEConnection.getABI: integer;
var command: byte;
  r: byte;
begin
  result:=0;
  command:=CMD_GETABI;
  if send(@command, 1)>0 then
    if receive(@r, 1)>0 then
      result:=r;
end;


function ShortenLinuxModuleName(modulename: string): string;
var i: integer;
begin
  //build the shortenedmodulename
  //parse the modulepath and strip the version and .so part and everything after it
  //formats: libxxx-#.#.so.#.#.#
  //keep in mind names like :libdbusmenu-gtk.so.4.0.12 and libdbusmenu-glib.so.4.0.12 should become libdbusmenu-gtk and libdbusmenu-glib respectively



  for i:=1 to length(modulename)-1 do
  begin
    case modulename[i] of
      '-':
      begin
        //check if modulename[i+1] is a number, if so, cut from here
        if (length(modulename)>=i+1) and (modulename[i+1] in ['0'..'9']) then
        begin
          result:=copy(modulename, 1, i-1);
          exit;
        end;


      end;

      '.':
      begin
        //check if it is .so
        if (length(modulename)>=i+2) and (uppercase(modulename[i+1])='S') and (uppercase(modulename[i+2])='O') then
        begin
          result:=copy(modulename, 1, i-1);
          exit;
        end;
      end;
    end;
  end;

  //still here
  result:=modulename;


end;

function TCEConnection.enumSymbolsFromFile(modulepath: string; fileoffset: dword; modulebase: ptruint; callback: TNetworkEnumSymCallback; nameaddendum: string=''): boolean;
type
  TCeGetSymbolList=packed record
    command: byte;
    fileoffset: uint32;
    symbolpathsize: uint32;
    path: array [0..0] of char;
  end;

  PCeGetSymbolList=^TCeGetSymbolList;

  TNetworkSymbolInfo=packed record
    address: uint64;
    size: int32;
    _type: int32;
    namelength: uint8;
  end;

  PNetworkSymbolInfo=^TNetworkSymbolInfo;



var
  msg: PCeGetSymbolList;
  msgsize: integer;

  compressedsize: uint32;
  decompressedsize: uint32;

  d: Tdecompressionstream;
  compressedbuffer: TMemorystream;

  decompressed: PByte;

  currentsymbol: PNetworkSymbolInfo;


  modulename: string;
  pos: integer;

  symname: pchar;
  maxsymname: integer;

  isexe: uint32;
  shortenedmodulename: string=''; //the name of the module with nothing after .so
  i: integer;
begin
  result:=true;

  if modulepath='[vdso]' then
  begin
    //special module with no clear filepath
    result:=EnumElfSymbols('vdso', modulebase, callback);
    exit;
  end;



  msgsize:=1+4+4+length(modulepath);
  getmem(msg, msgsize);

  msg^.command:=CMD_GETSYMBOLLISTFROMFILE;
  msg^.fileoffset:=fileoffset;
  msg^.symbolpathsize:=length(modulepath);
  CopyMemory(@msg^.path, @modulepath[1], length(modulepath));

  if fileoffset<>0 then
  asm
  nop
  end;

  if send(msg,  msgsize)>0 then
  begin
    if receive(@isexe, sizeof(isexe))>0 then
    begin
      if receive(@compressedsize, sizeof(compressedsize))>0 then
      begin
        if compressedsize>0 then
        begin
          if receive(@decompressedsize, sizeof(decompressedsize))>0 then
          begin
            compressedbuffer:=tmemorystream.create;
            compressedbuffer.Size:=compressedsize-3*sizeof(uint32);

            if receive(compressedbuffer.Memory, compressedbuffer.size)>0 then
            begin
              //decompress it
              d:=Tdecompressionstream.Create(compressedbuffer, false);
              getmem(decompressed, decompressedsize);
              d.ReadBuffer(decompressed^, decompressedsize);
              d.free;

              //parse through the decompressed block and fill in the results

              if copy(modulepath,1,1)<>'[' then
              begin
                modulename:=extractfilename(modulepath);

                shortenedmodulename:=ShortenLinuxModuleName(modulename);

              end
              else
                modulename:=modulepath;

              pos:=0;

              maxsymname:=256;
              getmem(symname, maxsymname);


              while pos<decompressedsize do
              begin
                currentsymbol:=@decompressed[pos];
                inc(pos, sizeof(TNetworkSymbolInfo));

                if currentsymbol^.namelength>=maxsymname then
                begin
                  //need more memory
                  maxsymname:=currentsymbol^.namelength+1;

                  FreeMemAndNil(symname);
                  getmem(symname, maxsymname);
                end;

                CopyMemory(symname, @decompressed[pos], currentsymbol^.namelength);
                symname[currentsymbol^.namelength]:=#0;

                inc(pos, currentsymbol^.namelength);


                if currentsymbol^.namelength>0 then
                begin
                  if isexe<>0 then
                  begin
                    if callback(modulename, symname, currentsymbol^.address, currentsymbol^.size,false)=false then
                      break;
                  end
                  else
                  begin
                    if (callback(shortenedmodulename+nameaddendum, symname, modulebase+currentsymbol^.address, currentsymbol^.size, false) and
                        callback(modulename+nameaddendum, symname, modulebase+currentsymbol^.address, currentsymbol^.size, true))=false then
                      break;
                  end;
                end;
              end;


              FreeMemAndNil(symname);

              FreeMemAndNil(decompressed);

            end;

            compressedbuffer.free;
          end;
        end;
      end;

    end;
  end;

end;

function TCEConnection.loadModuleEx(hProcess: THandle; dlopenaddress: ptruint; modulepath: string): boolean;
type
  TInput=packed record
    command: uint8;
    handle: uint32;
    dlopenaddress: uint64;
    modulepathlength: uint32;
    modulename: packed record end;
  end;
  PInput=^TInput;

var
  input: Pinput;
  r:uint64;
begin
  result:=false;
  if isNetworkHandle(hProcess) then
  begin
    getmem(input, sizeof(TInput)+length(modulepath));

    input^.command:=CMD_LOADMODULEEX;
    input^.handle:=hProcess and $ffffff;
    input^.dlopenaddress:=dlopenaddress;
    input^.modulepathlength:=Length(modulepath);
    CopyMemory(@input^.modulename, @modulepath[1], length(modulepath));

    if send(input,  sizeof(TInput)+length(modulepath))>0 then
    begin
      receive(@r, sizeof(r));
      result:=r<>0;
    end;

    FreeMemAndNil(input);

  end;
end;

function TCEConnection.loadModule(hProcess: THandle; modulepath: string): boolean;
type
  TInput=packed record
    command: uint8;
    handle: uint32;
    modulepathlength: uint32;
    modulename: packed record end;
  end;
  PInput=^TInput;

var
  input: Pinput;
  r:uint64;
begin
  OutputDebugString('TCEConnection.loadModule('+modulepath+')');
  result:=false;
   if isNetworkHandle(hProcess) then
  begin
    getmem(input, sizeof(TInput)+length(modulepath)+1);

    input^.command:=CMD_LOADMODULE;
    input^.handle:=hProcess and $ffffff;
    input^.modulepathlength:=Length(modulepath)+1;
    CopyMemory(@input^.modulename, @modulepath[1], length(modulepath)+1); //also include 0 terminator

    if send(input,  sizeof(TInput)+length(modulepath)+1)>0 then
    begin
      receive(@r, sizeof(r));

      OutputDebugString('CMD_LOADMODULE returned '+r.ToString);
      result:=r<>0;
    end;

    FreeMemAndNil(input);

  end;
end;

function TCEConnection.loadExtension(hProcess: THandle): boolean;
var
  loadExtensionCommand: packed record
    command: byte;
    handle: uint32;
  end;

  r:uint32;
begin
  result:=false;

  if isNetworkHandle(hProcess) then
  begin
    loadExtensionCommand.command:=CMD_LOADEXTENSION;
    loadExtensionCommand.handle:=hProcess and $ffffff;

    if send(@loadExtensionCommand,  sizeof(loadExtensionCommand))>0 then
    begin
      receive(@r, sizeof(r));
      result:=r<>0;
    end;
  end;

end;

function TCEConnection.speedhack_setSpeed(hProcess: THandle; speed: single): boolean;
var
  speedhackSetSpeedCommand: packed record
    command: byte;
    handle: uint32;
    speed: single;
  end;

  r:uint32;
begin
  result:=false;

  if isNetworkHandle(hProcess) then
  begin
    speedhackSetSpeedCommand.command:=CMD_SPEEDHACK_SETSPEED;
    speedhackSetSpeedCommand.handle:=hProcess and $ffffff;
    speedhackSetSpeedCommand.speed:=speed;

    if send(@speedhackSetSpeedCommand,  sizeof(speedhackSetSpeedCommand))>0 then
    begin
      receive(@r, sizeof(r));
      result:=r<>0;
    end;
  end;
end;

procedure TCEConnection.setConnectionName(name: string);
type
  TInput=packed record
    command: uint8;
    namelength: uint32;
    name: packed record end;
  end;
  PInput=^TInput;

var
  input: Pinput;
  ignored: string;
begin
  if (getVersion(ignored)>=4) then
  begin
    getmem(input, sizeof(TInput)+length(name));
    input^.command:=CMD_SET_CONNECTION_NAME;
    input^.namelength:=length(name);
    copymemory(@input^.name, @name[1], length(name));

    send(input, sizeof(TInput)+length(name));
  end;
end;

procedure TCEConnection.getOptions(var options: TCEServerOptions);
var
  command: uint8;
  optioncount: UINT16;
  i: integer;

  b: byte;
  t: integer;

begin
  command:=CMD_GETOPTIONS;
  send(@command,1);
  receive(@optioncount,sizeof(optioncount));

  setlength(options, optioncount);
  for i:=0 to optioncount-1 do
  begin
    options[i].optname:=receiveString16;
    options[i].parentoptname:=receiveString16;
    options[i].optdescription:=receiveString16;
    options[i].acceptablevalues:=receiveString16;
    options[i].currentvalue:=receiveString16;

    receive(@t, sizeof(t));
    options[i].optiontype:=TCEServerOptionType(t);
  end;
end;

function TCEConnection.getOption(name: string): string;
var
  buf: tmemorystream;
begin
  buf:=tmemorystream.create;
  buf.WriteByte(CMD_GETOPTION);
  buf.WriteWord(length(name));
  buf.WriteBuffer(name[1],length(name));

  send(buf.Memory, buf.Size);
  buf.free;

  result:=receiveString16;
end;

procedure TCEConnection.setOption(name: string; value: string);
var
  buf: tmemorystream;
begin
  buf:=tmemorystream.create;
  buf.WriteByte(CMD_SETOPTION);
  buf.WriteWord(length(name));
  buf.WriteBuffer(name[1],length(name));
  buf.WriteWord(length(value));
  if length(value)>0 then
    buf.WriteBuffer(value[1], length(value));

  send(buf.Memory, buf.Size);
  buf.free;
end;

function TCEConnection.connectNamedPipe(name: string; timeout:integer=0): HANDLE;
var
  buf: tmemorystream;
  r: uint32;
begin
  buf:=tmemorystream.Create;
  buf.WriteByte(CMD_OPENNAMEDPIPE);
  buf.WriteWord(length(name));
  buf.WriteBuffer(name[1],length(name));
  buf.WriteDWord(timeout);
  send(buf.memory, buf.size);
  buf.free;

  receive(@r,sizeof(r));

  if r<>0 then
    result:=$ce000000 or r
  else
    result:=INVALID_HANDLE_VALUE;
end;

function TCEConnection.readPipe(h: THandle; destination: pointer; size: integer; timeout:integer=0): boolean;
var
  input: packed record
    command: byte;
    h: uint32;
    size: uint32;
    timeout: uint32;
  end;

  actualsize: int32;
begin
  if isNetworkHandle(h) then
  begin
    h:=h and $ffffff;

    input.command:=CMD_PIPEREAD;
    input.h:=h;
    input.size:=size;
    input.timeout:=timeout;
    send(@input, sizeof(input));

    receive(@actualsize, sizeof(actualsize));
    if actualsize>size then
    begin
      //oh no....
      fConnected:=false;
      if socket<>0 then
        CloseSocket(socket);

      socket:=0;

      exit(false);
    end;

    receive(destination, actualsize);

    result:=actualsize=size;
  end
  else
    result:=false;
end;

function TCEConnection.writePipe(h: THandle; source: pointer; size: integer; timeout:integer=0): boolean;
var
  buf: tmemorystream;
  c: int32;
begin
  if isNetworkHandle(h) then
  begin
    h:=h and $ffffff;

    buf:=tmemorystream.create;
    buf.WriteByte(CMD_PIPEWRITE);
    buf.WriteDWord(h);
    buf.writeDword(size);
    buf.writeDword(timeout);
    buf.WriteBuffer(source^,size);
    send(buf.memory, buf.size);
    buf.free;

    receive(@c,sizeof(c));

    result:=c=size;

  end
  else
    result:=false;
end;

function TCEConnection.getServerPath: string;
var c: byte;
begin
  c:=CMD_GETCESERVERPATH;
  send(@c,1);
  result:=receiveString16;
end;

function TCEConnection.isAndroid: boolean;
var command, r: byte;
begin
  command:=CMD_ISANDROID;
  send(@command,1);
  receive(@r,1);
  result:=r<>0;
end;

function TCEConnection.setCurrentPath(path: string): boolean;
var buf: Tmemorystream;
  r: byte;
begin
  buf:=tmemorystream.create;
  buf.writebyte(CMD_SETCURRENTPATH);
  buf.WriteWord(length(path));
  buf.WriteBuffer(path[1],word(length(path)));
  send(buf.Memory, buf.Size);

  buf.free;
  receive(@r,1);

  result:=r<>0;
end;

function TCEConnection.getCurrentPath: string;
var command: byte;
begin
  command:=CMD_GETCURRENTPATH;
  send(@command,1);
  result:=receiveString16;
end;

procedure TCEConnection.enumfiles(path: string; list: tstrings);
var buf: tmemorystream;
  filecount: uint32;
  i: uint32;
  s: string;
  t: byte;
begin
  buf:=tmemorystream.create;
  buf.WriteByte(CMD_ENUMFILES);
  buf.writeword(length(path));
  buf.writebuffer(path[1],word(length(path)));
  send(buf.memory,buf.size);
  buf.free;

  list.clear;
  repeat
    s:=receivestring16;

    if s<>'' then
    begin
      receive(@t,1);
      list.AddObject(s, tobject(pointer(t)));
    end;
  until s='';
end;

function TCEConnection.getFilePermissions(path: string; out perms: UINT32): boolean;
var
  buf: tmemorystream;
  r: byte;
begin
  buf:=tmemorystream.create;
  buf.Writebyte(CMD_GETFILEPERMISSIONS);
  buf.writeword(length(path));
  buf.writebuffer(path[1],word(length(path)));
  send(buf.memory,buf.size);
  buf.free;

  receive(@r,1);
  if r<>0 then
    receive(@perms, sizeof(perms));

  result:=r<>0;
end;

function TCEConnection.setFilePermissions(path: string; perms: UINT32): boolean;
var
  buf: tmemorystream;
  r: byte;
begin
  buf:=tmemorystream.create;
  buf.Writebyte(CMD_SETFILEPERMISSIONS);
  buf.writeword(length(path));
  buf.writebuffer(path[1],word(length(path)));
  buf.writedword(perms);
  send(buf.memory,buf.size);
  buf.free;

  receive(@r,1);
  result:=r<>0;
end;


function TCEConnection.getFile(path: string; s: tstream): boolean;
var
  buf: tmemorystream;
  filelength: uint32; //do not bother with 4GB+ files...
  f: pointer;
begin
  buf:=tmemorystream.create;
  buf.writeByte(CMD_GETFILE);
  buf.writeword(length(path));
  buf.writebuffer(path[1],word(length(path)));
  send(buf.memory,buf.size);
  buf.free;

  receive(@filelength,sizeof(filelength));
  if filelength<>$ffffffff then
  begin
    getmem(f,filelength);
    receive(f,filelength);
    s.Position:=0;
    s.Size:=0;
    s.WriteBuffer(f^,filelength);
    freemem(f);
  end;

  result:=filelength<>$ffffffff;
end;

function TCEConnection.putFile(path: string; s: tstream): boolean;
var
  buf: tmemorystream;
  r: byte;
begin
  buf:=tmemorystream.create;
  buf.WriteByte(CMD_PUTFILE);
  buf.WriteWord(length(path));
  buf.writebuffer(path[1],word(length(path)));
  buf.WriteDWord(s.Size);
  buf.CopyFrom(s,0);
  send(buf.Memory,buf.size);

  receive(@r,1);
  result:=r<>0;
end;

function TCEConnection.createDir(path: string): boolean;
var
  buf: tmemorystream;
  r: byte;
begin
  buf:=tmemorystream.create;
  buf.WriteByte(CMD_CREATEDIR);
  buf.WriteWord(length(path));
  buf.writebuffer(path[1],word(length(path)));
  send(buf.Memory,buf.size);
  receive(@r,1);
  result:=r<>0;
end;

function TCEConnection.deleteFile(path: string): boolean;
var
  buf: tmemorystream;
  r: byte;
begin
  buf:=tmemorystream.create;
  buf.WriteByte(CMD_DELETEFILE);
  buf.WriteWord(length(path));
  buf.writebuffer(path[1],word(length(path)));
  send(buf.Memory,buf.size);
  receive(@r,1);
  result:=r<>0;
end;

function TCEConnection.isNetworkHandle(handle: THandle): boolean;
begin
  result:=((handle shr 24) and $ff)= $ce;
end;

function TCEConnection.send(buffer: pointer; size: integer): integer;
var i: integer;
  B: BOOL=TRUE;
begin
  result:=0;


  while (result<size) do
  begin
    i:=fpsend(socket, pointer(ptruint(buffer)+result), size, 0);
    if i<=0 then
    begin
      OutputDebugString('Error during send');
      fConnected:=false;
      if socket<>0 then
        CloseSocket(socket);

      socket:=0;
      result:=i; //error
      exit;
    end;

    inc(result, i);
  end;
end;

function TCEConnection.receive(buffer: pointer; size: integer): integer;
var
  i: integer;
begin
  //{$ifdef windows}
    //xp doesn't support MSG_WAITALL

    result:=0;
    while (result<size) do
    begin
      i:=fprecv(socket, pointer(ptruint(buffer)+result), size-result, 0);
      if i<=0 then
      begin
        fConnected:=false;
        if socket<>0 then
          CloseSocket(socket);

        socket:=0;
        result:=i; //error

        OutputDebugString('Error during receive');
        exit;
      end;

      inc(result, i);

    end;

 // {$else}
  //  result:=fprecv(socket, buffer, size, MSG_WAITALL);
  //{$endif}
end;

function TCEConnection.receiveString16: string;
var
  l: uint16;
  r: pchar;
begin
  receive(@l,sizeof(l));
  if l=0 then exit('');

  getmem(r,l);
  receive(r,l);
  r[l]:=#0;
  result:=r;
  freemem(r);
end;

constructor TCEConnection.create;
var SockAddr: TInetSockAddr;
  retry: integer;
  B: BOOL;
begin
  OutputDebugString('Inside TCEConnection.create');
  WriteProcessMemoryBufferCS:=TCriticalSection.create;

  VirtualQueryExCacheMapCS:=TCriticalSection.create;
  VirtualQueryExCacheMap:=TVQEMap.Create;

  socket:=cint(INVALID_SOCKET);

  if (host.s_addr=0) or (port=0) then exit;

  //connect
  socket:=FPSocket(AF_INET, SOCK_STREAM, 0);
  if (socket=cint(INVALID_SOCKET)) then
  begin
    OutputDebugString('Socket creation failed. Check permissions');
    exit;
  end;



  OutputDebugString('socket='+inttostr(socket));

  SockAddr.sin_family := AF_INET;
  SockAddr.sin_port := port;
  SockAddr.sin_addr.s_addr := host.s_addr;

  B:=TRUE;

  fpsetsockopt(socket, IPPROTO_TCP, TCP_NODELAY, @B, sizeof(B));

  retry:=0;
  while not (fConnected) and (retry<5) do
  begin
    if fpconnect(socket, @SockAddr, sizeof(SockAddr)) >=0 then
    begin
      b:=TRUE;
      fpsetsockopt(socket, IPPROTO_TCP, TCP_NODELAY, @B, sizeof(B)); //just to be sure
      fConnected:=true;
    end
    else
    begin
      inc(retry);
     // OutputDebugString('fail '+inttostr(retry));
    end;
  end;

  if not fconnected then
    OutputDebugString('Connection failure');
end;

destructor TCEConnection.destroy;
begin
  if not ((socket=cint(INVALID_SOCKET)) or (socket=0)) then
    CloseSocket(socket);

  if VirtualQueryExCacheMap<>nil then
    VirtualQueryExCacheMap.free;

  if VirtualQueryExCacheMapCS<>nil then
    VirtualQueryExCacheMapCS.free;

  if WriteProcessMemoryBufferCS<>nil then
    WriteProcessMemoryBufferCS.free;

end;

initialization
  LocalToolhelpSnapshotsCS:=TCriticalSection.create;

finalization
  LocalToolhelpSnapshotsCS.free;

end.

