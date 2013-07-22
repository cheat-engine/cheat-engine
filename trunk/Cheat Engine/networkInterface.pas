unit networkInterface;

{$mode objfpc}{$H+}

//playing arround with the objfpc notation. The pointer based arrays look interesting

interface

uses
  jwawindows, windows, Classes, SysUtils, Sockets, resolve, ctypes, networkconfig,
  cefuncproc, newkernelhandler, math;

type
  TNetworkDebugEvent=packed record
    signal: integer;
    threadid: qword;
  end;

  TCEConnection=class
  private
    socket: cint;
    fConnected: boolean;

    rpmcache: array [0..15] of record //every connection is thread specific, so each thread has it's own rpmcache
        lastupdate: TLargeInteger; //contains the last time this page was updated
        baseaddress: PtrUInt;
        memory: array [0..4095] of byte;
      end;

    function receive(buffer: pointer; size: integer): integer;
    function send(buffer: pointer; size: integer): integer;

    function CReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL;
    function NReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL;

  public

    function Process32Next(hSnapshot: HANDLE; var lppe: PROCESSENTRY32; isfirst: boolean=false): BOOL;
    function Process32First(hSnapshot: HANDLE; var lppe: PROCESSENTRY32): BOOL;
    function CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): HANDLE;
    function CloseHandle(handle: THandle):WINBOOL;
    function OpenProcess(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; dwProcessId:DWORD):HANDLE;
    function VirtualQueryEx(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD;
    function ReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL;
    function WriteProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL;
    function StartDebug(hProcess: THandle): BOOL;
    function WaitForDebugEvent(hProcess: THandle; timeout: integer; var devent: TNetworkDebugEvent):BOOL;
    function ContinueDebugEvent(hProcess: THandle; threadid: dword; continuemethod: integer): BOOL;
    function getVersion(var name: string): integer;
    property connected: boolean read fConnected;

    constructor create;
    destructor destroy; override;
  end;


implementation

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


function TCEConnection.CloseHandle(handle: THandle):WINBOOL;
var CloseHandleCommand: packed record
    command: byte;
    handle: dword;
  end;
begin
  if ((handle shr 24) and $ff)= $ce then
  begin
    CloseHandleCommand.command:=CMD_CLOSEHANDLE;
    CloseHandleCommand.handle:=handle and $ffffff;
    send(@CloseHandleCommand, sizeof(CloseHandleCommand));
    result:=true;
  end
  else //not a network handle
    result:=windows.CloseHandle(handle);
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
  if ((hSnapshot shr 24) and $ff)= $ce then
  begin
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
          strcopy(@lppe.szExeFile[0], pname);
          freemem(pname);

        end;

      end;
    end;
  end
  else
    result:=false;
end;

function TCEConnection.Process32First(hSnapshot: HANDLE; var lppe: PROCESSENTRY32): BOOL;
begin
  result:=process32next(hSnapshot, lppe, true);
end;

function TCEConnection.CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): HANDLE;
var CTSCommand: packed record
    command: byte;
    dwFlags: dword;
    th32ProcessID: dword;
  end;

var r: integer;
begin
  CTSCommand.command:=CMD_CREATETOOLHELP32SNAPSHOT;
  CTSCommand.dwFlags:=dwFlags;
  CTSCommand.th32ProcessID:=th32ProcessID;

  r:=0;
  if send(@CTSCommand, sizeof(CTSCommand))>0 then
    if receive(@r, sizeof(r))>0 then
    begin
      if (r>0) then
        r:=r or $ce000000;

      result:=r;
    end;
end;

function TCEConnection.CReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL;
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

  x: dword;
  blockoffset, blocksize: dword;
  currentbase: ptruint;
  currenttarget: ptruint;

  m: PByte;
begin
  result:=false;
  lpNumberOfBytesRead:=0;

  pagecount:=1+((ptruint(lpBaseAddress)+nSize-1) shr 12) - (ptruint(lpBaseAddress) shr 12);
  setlength(pages, pagecount);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(currenttime);

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
        if ((currenttime-rpmcache[j].lastupdate) / freq) > 1.0 then //too old, refetch
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

function TCEConnection.NReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL;
//Network read process memory
var
  input: packed record
    command: byte;
    handle: integer;
    baseaddress: qword;
    size: integer;
  end;

  output: packed record
    bytesread: integer;
    //followed by the bytes
  end;
begin
  result:=false;
  lpNumberOfBytesRead:=0;

  {
  if ptruint(lpBaseAddress)<$00400000 then
  asm
  nop
  nop
  nop
  end;
  }

  //still here so not everything was cached
  input.command:=CMD_READPROCESSMEMORY;
  input.handle:=hProcess;
  input.baseaddress:=ptruint(lpBaseAddress);
  input.size:=nSize;
  if send(@input, sizeof(input))>0 then
  begin
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

function TCEConnection.ReadProcessMemory(hProcess: THandle; lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL;
begin
  if ((hProcess shr 24) and $ff)= $ce then
  begin
    result:=false;
    lpNumberOfBytesRead:=0;

    hProcess:=hProcess and $ffffff;

    if nsize=0 then exit;

    if (nsize<=8192) then
    begin
      result:=CReadProcessMemory(hProcess, lpBaseAddress, lpBuffer, nsize, lpNumberOfBytesRead);
    end
    else //just fetch it all from the net , ce usually does not fetch more than 8KB for random accesses, so would be a waste of time
      result:=NReadProcessMemory(hProcess, lpBaseaddress, lpbuffer, nsize, lpNumberOfBytesRead);



  end
  else
    result:=windows.ReadProcessMemory(hProcess, lpBaseAddress, lpBuffer, nSize, lpNumberOfBytesRead);
end;

function TCEConnection.WriteProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL;
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

  j: integer;

  b: ptruint;

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

    freemem(input);
  end
  else
    result:=windows.WriteProcessMemory(hProcess, lpBaseAddress, lpBuffer, nSize, lpNumberOfBytesWritten);
end;

function TCEConnection.VirtualQueryEx(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD;
var
  input: packed record
    command: byte;
    handle: integer;
    baseaddress: qword;
  end;

  output: packed record
    result: integer;
    protection: integer;
    baseaddress: qword;
    size: qword;
  end;

begin

  if ((hProcess shr 24) and $ff)= $ce then
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
            lpbuffer._Type:=MEM_PRIVATE;
          end;



          lpbuffer.RegionSize:=output.size;

          result:=dwlength;
        end
        else
          result:=0;
      end;
    end;
  end
  else
    result:=windows.VirtualQueryEx(hProcess, lpAddress, lpBuffer, dwLength);
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
  if fpsend(socket, @command, 1,0)>0 then
  begin
    if receive(@CeVersion, sizeof(CeVersion))>0 then
    begin
      getmem(_name, CeVersion.stringsize);
      receive(_name, CeVersion.stringsize);

      name:=_name;
      freemem(_name);

      result:=length(name);
    end;
  end;
end;

function TCEConnection.send(buffer: pointer; size: integer): integer;
var i: integer;
begin
  result:=0;
  while (result<size) do
  begin
    i:=fpsend(socket, pointer(ptruint(buffer)+result), size, 0);
    if i<=0 then
    begin
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
  {$ifdef windows}
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
        exit;
      end;

      inc(result, i);

    end;

  {$else}
    result:=fprecv(socket, buffer, size, MSG_WAITALL);
  {$endif}
end;

constructor TCEConnection.create;
var SockAddr: TInetSockAddr;
  retry: integer;
begin

  socket:=cint(INVALID_SOCKET);

  if (host.s_addr=0) or (port=0) then exit;

  //connect
  socket:=FPSocket(AF_INET, SOCK_STREAM, 0);

  SockAddr.Family := AF_INET;
  SockAddr.Port := port;
  SockAddr.Addr := host.s_addr;

  retry:=0;
  while not (fConnected) and (retry<5) do
  begin
    if fpconnect(socket, @SockAddr, sizeof(SockAddr)) >=0 then
      fConnected:=true
    else
      inc(retry);
  end;
end;

destructor TCEConnection.destroy;
begin
  if socket<>INVALID_SOCKET then
    CloseSocket(socket);
end;

end.

