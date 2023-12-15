unit GDBServerDebuggerInterface;

//https://sourceware.org/gdb/current/onlinedocs/gdb.html/Remote-Protocol.html#Remote-Protocol
//https://github.com/llvm/llvm-project/blob/main/lldb/docs/lldb-gdb-remote.txt

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DebuggerInterface, contexthandler, ctypes, forms, resolve,
  XMLRead, XMLwrite, DOM {$ifdef windows}, windows, WinSock2, Clipbrd{$endif}
  {$ifdef darwin} , macport,macportdefines,BaseUnix,Unix{$endif}
  ,math, syncobjs, StringHashList, Maps, sockets;

type
  TGDBServerContextHandler=class(TContextInfo)
  private
    _general: array of PContextElement_register;
    _mainfpu: array of PContextElement_register;

    __general: array of TContextElement_register;
    __mainfpu: array of TContextElement_register;

    procedure SetInstructionPointerRegister(reg: PContextElement_register);
    procedure SetStackPointerRegister(reg: PContextElement_register);
    procedure SetFramePointerRegister(reg: PContextElement_register);
    procedure addGeneralPurposeRegister(reg: PContextElement_register);
    procedure addMainFPURegister(reg: PContextElement_register);
    procedure finalizeLists;
  public
    destructor destroy; override;
    constructor create;
  end;

  TGdbserverExecutor=class(tthread)
    command: string;
    errormessage: string;
    procedure Execute; override;
  end;

  TDwordArray=array of dword;


  TGDBBreakpoint=record
    address: ptruint;
    bptype: integer;
    bpkind: integer;
  end;
  PGDBBreakpoint=^TGDBBreakpoint;

  //stepaction is a type that determines what to do on the next step
  TStepActionType=(saIgnore, saReactivateBreakpoint); //param: bpnr

  TStepAction=record
    action: TStepActionType;
    param: integer;
  end;

  TQWordArray=array of QWord;

  TGDBServerDebuggerInterface=class(TDebuggerInterface)
  private
    socket: cint;
    host: THostAddr;
    port: integer;
    maxpacketsize: integer;
    packetsizeinhex: boolean;
    gdbserverprocess: thandle; //if it's locally launched
    gdbserverlaunchcommand: string;
    gdbcontextHandler: TGDBServerContextHandler;

    pointersize: integer;
    pagesize: integer;
    //watchpointsBreakBeforeOperation: boolean;
    bigEndian: boolean;

    //stashedPackets: tstringlist; //ignored commands that some functions didn't want to handle yet (e.g threadlist at the same time as a break happening)
    receiveBuffer: string;
    binaryReceiveBuffer: pchar;
    binaryReceiveBufferMax: integer;
    binaryReceiveBufferPos: integer;
    usesack: boolean;
    features: TStringArray;

    canvCont: boolean;
    canContinue: boolean;
    canContinueWithSig: boolean;
    canStep, canStepWithSig: boolean;
    canStop: boolean;
    canStepRange: boolean;
    canUseContextCommands: boolean;

    cannotuseQSaveRegisterState: boolean;
    ThreadSuffixSupported: boolean; //
    ThreadEvents: boolean; //
    ThreadListWhenStopped: boolean;

    gdbserverExecutor: TGdbserverExecutor;

   // lastStopMessage: string;



    stopped: boolean;
    stoptargetused: boolean; //if a ctrl+c (3) was sent to the server
    stoppacket: string;
    stoppackethl: TStringHashList;
    pid: dword;
    currenttid: dword;
    currentpc: qword;

    signal: byte;
    lock: TCriticalSection;

    breakpoints: TList;

    context: Pointer;

    nextStepActions: array of TStepAction;

    canReadBytesFast: boolean;
    supportsnonstop: boolean;

    QueriedHasToBreakFirst: boolean;
    hasToBreakFirst: boolean;

    isStepping: boolean; //when the break is a trace
    addressThatCausedBreakpoint: qword;

    oldcontexts: TMap; //map that contains all seen contexts during a stop. Gets cleared after a call to ContinueDebugEvent

    breakIfNeededCount: integer; //in case breakIfNeeded is called multiple times


    savedStateIndex: integer;
    savedState: pointer; //in case the index method doesn't work

    procedure clearOldContexts;
    procedure ack;
    procedure nack;
    function waitForData(timeout: integer): boolean;

    function sendPacket(s: string; timeout: integer=2000): boolean;

    procedure addStepAction(satype: TStepActionType; param: integer);
    function getThreadFromStopPacket(p: string): dword;

    function disableBreakpoint(index: integer):boolean;
    function activateBreakpoint(index: integer): boolean;

    function stoppedDueToManualBreak: boolean;
    function breakIfNeeded: boolean;
    procedure continueAfterManualStop; //to continue after a breakIfNeeded

    function getcurrentpc: qword;
    function findExecuteBreakpoint(address:qword): integer;
    function findWatchpointBreakpoint(address:qword): integer;

    function getCurrentProgramcounter :ptruint;
    procedure setCurrentProgramcounter(newpc: ptruint);

    function ApplyRegisterChanges(hThread: THandle; reglist: PContextElementRegisterList; oldc, newc: pointer): boolean;
  public
    lastreceivedpacket: string; //debug string (delete)
    function isStopPacket(p: string): boolean;
    function receivePacket(timeout: integer=2000; watchForStopPacketsAsWell:boolean=false): string; //move to private
    function receiveBinaryPacket(out data: pchar; out datalen: integer; timeout: integer=2000): boolean;

    procedure ObtainLock;
    procedure ReleaseLock;

    procedure getProcessList(list: tstrings);
    procedure getThreadList(var tl: TDwordArray);

    function getCurrentStopPacket: string;
    function debugPacket(data: string; timeout: integer=2000): string;
    function writeBytes(address: ptruint; data: pointer; size: integer): boolean;
    procedure readBytes(address: ptruint; data: pointer; size: integer; out actualread: integer);
    procedure readBytesFast(address: ptruint; data: pointer; size: integer; out actualread: integer);

    function saveState: boolean;
    function restoreState: boolean;

    function allocateMemory(size: integer; protections: string): ptruint;
    function deallocateMemory(address: ptruint): boolean;

    function isInjectedEvent: boolean; override;
    function stoptarget(out wasStopped: boolean):boolean;
    function WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL; override;
    function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL; override;
  //  function SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL; override;
  //  function GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false):  BOOL; override;

    function GetThreadContext(hThread: THandle; lpContext: pointer; isFrozenThread: Boolean=false): BOOL; overload; override;
    function SetThreadContext(hThread: THandle; lpContext: pointer; isFrozenThread: Boolean=false): BOOL; overload; override;

    function suspendProcess: boolean;
    procedure resumeProcess;

    function canUseIPT: boolean; override;

    function setExecuteBP(address: ptruint; preferedbptype: integer=1): integer;
    function setWriteBP(address: ptruint; size: integer=1): integer;
    function setAccessBP(address: ptruint; size: integer=1): integer;

    procedure deleteBreakpoint(index: integer);

    function needsToAttach: boolean; override;
    function controlsTheThreadList: boolean; override;
    function canReportExactDebugRegisterTrigger: boolean; override;
    function usesDebugRegisters: boolean; override;

    function isConnected: boolean;

    function DebugActiveProcess(dwProcessId: DWORD): WINBOOL; override;
    function DebugActiveProcessStop(dwProcessID: DWORD): WINBOOL; override;
    procedure AddToNoBreakList(threadid: integer); override;
    procedure RemoveFromNoBreakList(threadid: integer); override;
    procedure connect(ip: string; _port: word);

    procedure OnApiPointerChange(sender: Tobject);
    procedure ClearRPMCache;
    procedure ClearRPMCachePage(baseaddress: qword);

    destructor destroy; override;
    constructor createAndConnect(_gdbserverlaunchcommand: string; ip: string; _port: word);
    constructor connectToExistingServer(ip: string; _port: word);

    property isStopped: boolean read stopped;
    property currentprogramcounter: ptruint read getCurrentProgramcounter write setCurrentProgramcounter;

  end;

const EXCEPTION_GDB_BREAKPOINT=$CEDB0002;


implementation

uses process, controls, StrUtils{$ifndef STANDALONEDEBUG}, symbolhandler,
  CEFuncProc, ProcessHandlerUnit, newkernelhandler, globals,
  DebuggerInterfaceAPIWrapper, PageMap, dialogs{$endif};
//TGDBServerContextHandler

threadvar haslock: integer;

procedure TGDBServerContextHandler.finalizeLists;
var i: integer;
  maxoffset: dword;
begin
  //build the normal register lists
  maxoffset:=0;
  fGeneralPurposeRegisterMaxCharCount:=0;

  setlength(__general, length(_general));
  for i:=0 to length(_general)-1 do
  begin
    __general[i]:=_general[i]^;
    maxoffset:=max(maxoffset, __general[i].ContextOffset+__general[i].size);

    nameToEntryLookup.Add(__general[i].name, @__general[i]);

    if fFramePointerRegister=_general[i] then fFramePointerRegister:=@__general[i];
    if fStackPointerRegister=_general[i] then fStackPointerRegister:=@__general[i];
    if fInstructionPointerRegister=_general[i] then fInstructionPointerRegister:=@__general[i];

    _general[i]^.name:='';
    freemem(_general[i]);

    fGeneralPurposeRegisterMaxCharCount:=max(length(__general[i].name),fGeneralPurposeRegisterMaxCharCount);
  end;
  setlength(_general,0);

  setlength(__mainfpu, length(_mainfpu));
  for i:=0 to length(__mainfpu)-1 do
  begin
    __mainfpu[i]:=_mainfpu[i]^;
    maxoffset:=max(maxoffset, __mainfpu[i].ContextOffset+__mainfpu[i].size);
    nameToEntryLookup.Add(__mainfpu[i].name, @__mainfpu[i]);
    freemem(_mainfpu[i]);
  end;
  setlength(_mainfpu,0);

  general:=@__general;
  mainfpu:=@__mainfpu;

  fcontextsize:=maxoffset;
end;

procedure TGDBServerContextHandler.SetInstructionPointerRegister(reg: PContextElement_register);
begin
  fInstructionPointerRegister:=reg;
end;

procedure TGDBServerContextHandler.SetStackPointerRegister(reg: PContextElement_register);
begin
  fStackPointerRegister:=reg;
end;

procedure TGDBServerContextHandler.SetFramePointerRegister(reg: PContextElement_register);
begin
  fFramePointerRegister:=reg;
end;

procedure TGDBServerContextHandler.addGeneralPurposeRegister(reg: PContextElement_register);
begin
  setlength(_general, length(_general)+1);
  _general[length(_general)-1]:=reg;
end;

procedure TGDBServerContextHandler.addMainFPURegister(reg: PContextElement_register);
begin
  setlength(_mainfpu, length(_mainfpu)+1);
  _mainfpu[length(_mainfpu)-1]:=reg;
end;


//TGdbserverExecutor

procedure TGdbserverExecutor.Execute;
var p: TProcess;
  c: integer;
begin
  while not terminated do
  begin

    try
      p:=TProcess.Create(nil);
      p.CommandLine:=command;
     // p.ShowWindow:=swoHIDE;
      p.Execute;

      while (not terminated) and (not p.WaitOnExit(1000)) do ;

      if terminated then
        p.Terminate(1);

      p.free;
    except
      on e: exception do
      begin
        errormessage:=e.message;
        exit;
      end;
    end;
    sleep(10);
  end;

end;


destructor TGDBServerContextHandler.destroy;
begin

  setSpecializedContexHandler(nil);
  inherited destroy;
end;

constructor TGDBServerContextHandler.create;
begin
  inherited create;
  general:=@_general;
  setSpecializedContexHandler(self);
end;

//--------TGDBServerDebuggerInterface--------

function checkPacket(s: string): boolean;
var
  checksumstr: string;
  checksum: byte;
  currentchecksum: byte;
  i: integer;
begin
  if length(s)<4 then exit(false); //"$#00" is the smallest
  if s[1]<>'$' then exit(false);
  if s[length(s)-2]<>'#' then exit(false);

  checksumstr:=copy(s,length(s)-1);
  try
    checksum:=StrToInt('$'+checksumstr);
  except
    exit(false);
  end;

  currentchecksum:=0;
  for i:=2 to length(s)-3 do
    currentchecksum:=currentchecksum+ord(s[i]);

  exit(currentchecksum=checksum);
end;

function makepacket(s: string): string;
var
  sum: byte;
  i: integer;
begin
  sum:=0;
  for i:=1 to length(s) do
    sum:=sum+ord(s[i]);

  result:='$'+s+'#'+inttohex(sum,2);
end;


function TGDBServerDebuggerInterface.waitForData(timeout: integer): boolean;
var
  sl: TFDSet;
  tv: Timeval;
  i: integer;
begin
  {$ifdef unix}fpFD_ZERO{$else}FD_ZERO{$endif}(sl);
  {$ifdef unix}fpFD_SET{$else}FD_SET{$endif}(socket, sl);



  tv.tv_sec:=timeout div 1000;
  tv.tv_usec:=(timeout mod 1000)*1000;

  i:={$ifdef windows}winsock2.select{$else}fpselect{$endif}(socket+1, @sl,nil,nil,@tv);
  result:=i>=1;
end;

procedure TGDBServerDebuggerInterface.nack;
var c: char;
begin
  c:='-';
  fpsend(socket, @c,1,0);
end;

procedure TGDBServerDebuggerInterface.ack;
var c: char;
begin
  c:='+';
  fpsend(socket, @c,1,0);
end;
  {
procedure TGDBServerDebuggerInterface.stashPacket(packet: string; front: boolean=false);
begin
  if packet='' then exit;

  if front then
    stashedPackets.Insert(0, packet)
  else
    stashedPackets.Add(packet);
end;   }

function TGDBServerDebuggerInterface.receivePacket(timeout: integer=2000; watchForStopPacketsAsWell: boolean=false): string;
//ONLY CALL WHEN LOCK IS OBTAINED
//wait for a packet. If it's a stop packet record the stop reason and continue waiting (unless watchForStopPacketsAsWell is true)
var r: string;
  i: integer;
  buf: pchar;

  sl: TFDSet;
  seta,setb: TFDSet;
  tv: Timeval;
  packetstart: integer;

  receivedData: boolean;
  fullpacket: string;
  packetdata: string;

  foundend: boolean;

  receivecount: integer;
begin

  if haslock<=0 then raise exception.create('ReceivePacket without lock');

 { if (skipStashedPackets=false) and (stashedPackets.count>0) then
  begin
    result:=stashedPackets[0];
    stashedPackets.Delete(0);
    exit;
  end; }

  getmem(buf,4096);
  try
    result:='';
    receivecount:=0;

    while receivecount<10 do
    begin
      receivedData:=false;
      inc(receivecount);
      while true do
      begin
        if not waitForData(0) then break;
        i:=fprecv(socket, buf,4095,0);
        buf[i]:=#0;

        receiveBuffer:=receiveBuffer+buf;

        if i>0 then
          receivedData:=true;
        //find a buffer inside the receiveBuffer and if found, return it as a result (and ack)
      end;



      packetstart:=0;
      for i:=1 to length(receiveBuffer) do
        if receiveBuffer[i]='$' then
        begin
          packetstart:=i;
          break;
        end;

      if packetstart=0 then
      begin
        if waitForData(timeout)=false then exit;
        continue;  //read more
      end;

      //find the end
      foundend:=false;
      for i:=packetstart to length(receiveBuffer) do
      begin
        if receiveBuffer[i]='#' then
        begin
          foundend:=true;
          fullpacket:=copy(receiveBuffer,packetstart, i+2);
          receiveBuffer:=copy(receivebuffer,i+3);

          result:=copy(fullpacket,2,length(fullpacket)-4);
          lastreceivedpacket:=result;

          if (lastreceivedpacket<>'') and (lastreceivedpacket[1]='O') and (lastreceivedpacket<>'OK') then
          begin
            OutputDebugString(pchar('O->'+copy(lastreceivedpacket,2)));
            break; //it's a 'stop' packet that doesn't cause a stop (nothing really waits for this)
          end;

          if isStopPacket(lastreceivedpacket) then
          begin
            stopped:=true;
            stoppacket:=lastreceivedpacket;
            if watchForStopPacketsAsWell=false then //just a stop packet.
            begin
              receivecount:=0;
              break;
            end;
          end;

          //outputdebugstring('gdb <- '+result);

          if not usesAck then exit; //got the data

          if checkPacket(fullpacket)=false then
          begin
            nack; //request retransmission
            break;
          end
          else
          begin
            ack; //everything ok
            exit;
          end;
        end;
      end;

      //still here,
      if not foundend then //if foundend is true then invalid packet, nack has been sent, read again
      begin
        if WaitForData(timeout)=false then
        begin
          nack; //request retransmission
          if WaitForData(timeout)=false then
            exit; //give up
        end;
      end;



    end;
  finally
    freemem(buf);
  end;
end;

function TGDBServerDebuggerInterface.receiveBinaryPacket(out data: pchar; out datalen: integer; timeout: integer=2000): boolean;
//ONLY CALL WHEN LOCK IS OBTAINED
var r: string;
  i,j, tpos: integer;
  buf: pchar;

  sl: TFDSet;
  seta,setb: TFDSet;
  tv: Timeval;
  packetstart: integer;

  receivedData: boolean;
  fullpacket: string;
  packetdata: string;

  foundend: boolean;

  receivecount: integer;
  temp: pchar;
  ps: integer;
begin
  result:=false;
  if haslock<=0 then raise exception.create('ReceivePacket (binary) without lock');

  if binaryReceiveBuffer=nil then
  begin
    getmem(binaryReceiveBuffer, 4);
    binaryReceiveBufferMax:=4;
    binaryReceiveBufferPos:=0;
  end;

  getmem(buf,4096);
  try
    receivecount:=0;

    while receivecount<10 do
    begin
      receivedData:=false;
      inc(receivecount);
      while true do
      begin
        if not waitForData(0) then break; //nothing left to read at the moment (might have more later)
        i:=fprecv(socket, buf,4096,0);

        if binaryReceiveBufferPos+i>=binaryReceiveBufferMax then
        begin
          binaryReceiveBufferMax:=binaryReceiveBufferPos+i+4096;
          ReAllocMem(binaryReceiveBuffer, binaryReceiveBufferMax);
        end;

        copymemory(@binaryReceiveBuffer[binaryReceiveBufferPos], buf, i);
        inc(binaryReceiveBufferPos,i);

        if i>0 then
          receivedData:=true;
        //find a buffer inside the receiveBuffer and if found, return it as a result (and ack)
      end;



      packetstart:=-1;
      for i:=0 to binaryReceiveBufferPos-1 do
        if binaryreceiveBuffer[i]='$' then
        begin
          packetstart:=i;
          break;
        end;

      if packetstart=-1 then
      begin
        if waitForData(timeout)=false then exit;
        continue;  //read more
      end;

      //find the end
      foundend:=false;
      for i:=packetstart to binaryreceiveBufferPos-1 do
      begin
        if binaryreceiveBuffer[i]='#' then
        begin
          foundend:=true;

          ps:=i+2-packetstart;
          getmem(temp, ps);

          tpos:=0;
          j:=packetstart+1;
          while j<i do
          begin
            if (j<i+1) and (binaryReceiveBuffer[j]='}') then
            begin
              temp[tpos]:=char(pbyte(@binaryReceiveBuffer[j+1])^ xor $20);
              inc(tpos);
              inc(j,2);
            end
            else
            begin
              temp[tpos]:=binaryReceiveBuffer[j];
              inc(tpos);
              inc(j,1);
            end;
          end;

          data:=temp;
          datalen:=tpos;

          if (datalen>1) and (data[0]='O') and (data[1]<>'K') then
          begin
            OutputDebugString(pchar('O->'+pchar(@data[1])));
            break;
          end;

          if isStopPacket(data) then
          begin
            stopped:=true;
            stoppacket:=data;
            break;
          end;

          result:=true;

          if not usesAck then exit; //got the data

          if checkPacket(pchar(@binaryReceiveBuffer[packetstart]))=false then
          begin
            nack; //request retransmission
            break;
          end
          else
          begin
            ack; //everything ok
            exit;
          end;
        end;
      end;

      //still here,
      if not foundend then //if foundend is true then invalid packet, nack has been sent, read again
      begin
        if WaitForData(timeout)=false then
        begin
          nack; //request retransmission
          if WaitForData(timeout)=false then
            exit; //give up
        end;
      end;



    end;
  finally
    freemem(buf);
  end;
end;

function TGDBServerDebuggerInterface.sendPacket(s: string; timeout: integer=2000): boolean;
var
  i: integer;
  sent: integer;
  sl: TFDSet;
  tv: Timeval;
  ra: char;
  transmitcount: integer;
begin
  if haslock<=0 then raise exception.create('sendPacket without lock');


 // outputdebugstring('ce -> gdb: '+s);
  if (s='c') or (s.StartsWith('vCont;')) then
  begin
    //OutputDebugString('GDB continued:'+s);
    stopped:=false;
    stoptargetused:=false;
    stoppacket:='';
  end;

  s:=makepacket(s);



  result:=false;

  transmitcount:=0;
  while not result and (transmitcount<10) do
  begin
    inc(transmitcount);
    sent:=0;

    while sent<length(s) do
    begin
      i:=fpsend(socket, @s[1+sent],length(s)-sent,0);
      if i<=0 then
      begin
        if socket<>0 then
        begin
          closesocket(socket);
          socket:=0;
          exit(false);
        end;
      end
      else
        inc(sent,i);
    end;

    if usesAck then //wait for the ack
    begin
      if waitForData(timeout)=false then continue;
      if fprecv(socket, @ra,1,0)=1 then
        result:=ra='+';

    end else exit(true);

  end;
end;

  {
function TGDBServerDebuggerInterface.AllocateMemory(read, write, execute: boolean): pointer;
begin

end; }


function TGDBServerDebuggerInterface.isInjectedEvent: boolean;
begin
  result:=false;
end;

function TGDBServerDebuggerInterface.getCurrentStopPacket: string;
begin
  if stopped then
    result:=stoppacket
  else
    result:='';
end;

function TGDBServerDebuggerInterface.debugPacket(data: string; timeout: integer=2000): string;
begin
  obtainlock;
  try
    breakifneeded;

    if data<>'' then
      sendPacket(data);

    result:=receivePacket(timeout,true);


    continueAfterManualStop;
  finally
    ReleaseLock;
  end;

end;

function TGDBServerDebuggerInterface.writeBytes(address: ptruint; data: pointer; size: integer): boolean;
var
  s,r: string;
  wasstopped: boolean;
  hexstr: pchar;

  bytesleft: integer;
  blocksize: integer;
begin
  result:=false;
  ObtainLock;
  try
    breakIfNeeded;

    //e.g: M10002C7DD,6:909090909090


   // outputdebugstring(format('writeBytes(%x)',[address]));

    bytesleft:=size;
    result:=true;
    while result and (bytesleft>0) do
    begin
      blocksize:=min(maxpacketsize, bytesleft);
      s:='M'+inttohex(address,1)+','+inttohex(blocksize,1)+':';
      getmem(hexstr,blocksize*2+1);
      BinToHex(data, hexstr,size);
      hexstr[blocksize*2]:=#0;
      s:=s+hexstr;

      sendPacket(s);
      r:=receivePacket(1000);
      result:=r='OK';

      freemem(hexstr);

      inc(address, blocksize);
      dec(bytesleft, blocksize);
    end;



    continueAfterManualStop;

  finally
    ReleaseLock;
  end;
end;

procedure TGDBServerDebuggerInterface.readBytes(address: ptruint; data: pointer; size: integer; out actualread: integer);
var
  s: string;
  wasstopped: boolean;

  bytesleft: integer;
  blocksize: integer;
begin
  ObtainLock;
  try
    breakIfNeeded;

    actualread:=0;

    bytesleft:=size;
    while bytesleft>0 do
    begin
      blocksize:=min(maxpacketsize, bytesleft);
      sendPacket('m'+inttohex(address,1)+','+inttohex(blocksize,1));
      s:=receivePacket(1000);
      size:=length(s) div 2;
      if size<>blocksize then break;

      HexToBin(pchar(s),data, size);

      inc(actualread, size);
      inc(data,size);
      dec(bytesleft, size);
    end;

    continueAfterManualStop;

  finally
    ReleaseLock;
  end;
end;

procedure TGDBServerDebuggerInterface.readBytesFast(address: ptruint; data: pointer; size: integer; out actualread: integer);
var
  wasstopped: boolean;
  d: pointer;
begin
  ObtainLock;
  try
    breakIfNeeded;

    sendPacket('x'+inttohex(address,1)+','+inttohex(size,1));
    actualread:=size;
    receiveBinaryPacket(d,actualread);
    copymemory(data,d,actualread);

    continueAfterManualStop;
  finally
    ReleaseLock;
  end;

end;

procedure TGDBServerDebuggerInterface.ObtainLock;
begin

  lock.enter;
  inc(haslock);
end;

procedure TGDBServerDebuggerInterface.ReleaseLock;
begin
  dec(haslock);
  lock.leave;
end;

function TGDBServerDebuggerInterface.isStopPacket(p: string): boolean;
begin
  result:=(p<>'') and (p[1] in ['S','T','W','X','w','N']); //note: O is not a 'stop'
end;

function TGDBServerDebuggerInterface.stoppedDueToManualBreak: boolean;
begin
  result:=false;
  if stopped and stoptargetused then
  begin
    //check the stoppacket to see if it was due to a manual stop or if a breakpoint triggered before it stopped
    exit(stoppacket.Contains('metype:5') or stoppacket.Contains('reason:exception'));

  end;
end;

function TGDBServerDebuggerInterface.suspendProcess:boolean;
var wasstopped: boolean;
begin
  result:=stopped;
  if not stopped then
    result:=stoptarget(wasstopped);

  inc(breakIfNeededCount);
end;

procedure TGDBServerDebuggerInterface.resumeProcess;
begin
  continueAfterManualStop;
end;

function TGDBServerDebuggerInterface.breakIfNeeded: boolean;
var wasstopped: boolean;
begin
  result:=stopped;
  if not stopped and hasToBreakFirst then
    result:=stoptarget(wasstopped);

  inc(breakIfNeededCount);
end;

procedure TGDBServerDebuggerInterface.continueAfterManualStop;
begin
  //outputdebugstring('continueAfterManualStop: breakIfNeededCount='+breakIfNeededCount.ToString);
  ObtainLock;
  if breakIfNeededCount>0 then
    dec(breakIfNeededCount);

  if stoppedDueToManualBreak and (breakIfNeededCount=0) then
  begin
    //outputdebugstring('continueAfterManualStop: Continueing');
    sendPacket('c');
  end;

  ReleaseLock;
end;

function TGDBServerDebuggerInterface.setWriteBP(address: ptruint; size: integer=1):integer;
var
  s: string;
  bp: PGDBBreakpoint=nil;
  i: integer;
begin
  result:=-1;
  obtainlock;
  try
    breakIfNeeded;

    sendPacket('Z2,'+inttohex(address)+','+inttostr(size));
    s:=receivePacket(1000);
    if s<>'OK' then
      raise exception.create('Failure setting write watch');

    getmem(bp, sizeof(TGDBBreakpoint));
    bp^.address:=address;
    bp^.bptype:=2;
    bp^.bpkind:=size;

    for i:=0 to breakpoints.count-1 do
      if breakpoints[i]=nil then
      begin
        breakpoints[i]:=bp;
        result:=i;
      end;

    if result=-1 then
      result:=breakpoints.add(bp);

    continueAfterManualStop;
  finally
    ReleaseLock;
  end;

end;

function TGDBServerDebuggerInterface.setAccessBP(address: ptruint; size: integer=1): integer;
var
  s: string;
  bp: PGDBBreakpoint=nil;
  i: integer;
begin
  result:=-1;
  obtainlock;
  try
    breakIfNeeded;

    sendPacket('Z4,'+inttohex(address)+','+inttostr(size));
    s:=receivePacket(1000);
    if s<>'OK' then
      raise exception.create('Failure setting writye watch');

    getmem(bp, sizeof(TGDBBreakpoint));
    bp^.address:=address;
    bp^.bptype:=4;
    bp^.bpkind:=size;

    for i:=0 to breakpoints.count-1 do
      if breakpoints[i]=nil then
      begin
        breakpoints[i]:=bp;
        result:=i;
      end;

    if result=-1 then
      result:=breakpoints.add(bp);

    continueAfterManualStop;

  finally
    ReleaseLock;
  end;

end;

function TGDBServerDebuggerInterface.setExecuteBP(address: ptruint; preferedbptype: integer=1): integer;
var
  s: string;
  bp: PGDBBreakpoint=nil;
  i: integer;
  foundslot: boolean;
begin
  result:=-1;
  preferedbptype:=preferedbptype mod 2;   //0(software) or 1(hardware exec)

  obtainlock;
  try
    breakIfNeeded;

    sendPacket('Z'+preferedbptype.ToString+','+inttohex(address)+',1');
    s:=receivePacket(1000);
    if (s<>'OK') then
    begin
      preferedbptype:=(preferedbptype+1) mod 2;
      sendPacket('Z'+preferedbptype.ToString+','+inttohex(address)+',1');
      s:=receivePacket(1000);
      if s<>'OK' then
        raise exception.create('Failure setting breakpoint');
    end;

    getmem(bp, sizeof(TGDBBreakpoint));
    bp^.address:=address;
    bp^.bptype:=preferedbptype;
    bp^.bpkind:=1;

    for i:=0 to breakpoints.count-1 do
      if breakpoints[i]=nil then
      begin
        breakpoints[i]:=bp;
        result:=i;
      end;

    if result=-1 then
      result:=breakpoints.add(bp);


    continueAfterManualStop;
  finally
    ReleaseLock;
  end;
end;

function TGDBServerDebuggerInterface.findExecuteBreakpoint(address: qword): integer;
var i: integer;
begin
  result:=-1;
  for i:=0 to breakpoints.count-1 do
    if (breakpoints[i]<>nil) and (PGDBBreakpoint(breakpoints[i])^.address=address) and (PGDBBreakpoint(breakpoints[i])^.bptype in [0,1]) then //found the BP
      exit(i);
end;

function TGDBServerDebuggerInterface.findWatchpointBreakpoint(address:qword): integer;
var i: integer;
begin
  result:=-1;
  for i:=0 to breakpoints.count-1 do
  begin
    if (breakpoints[i]<>nil) and (PGDBBreakpoint(breakpoints[i])^.address=address) and (PGDBBreakpoint(breakpoints[i])^.bptype in [2..4]) then //found the BP
      exit(i);
  end;
end;

procedure TGDBServerDebuggerInterface.deleteBreakpoint(index: integer);
var bp: PGDBBreakpoint;
  i: integer;
begin
  disableBreakpoint(index);
  bp:=PGDBBreakpoint(breakpoints[index]);
  freemem(bp);
  breakpoints[index]:=nil;

  for i:=0 to length(nextStepActions)-1 do
  begin
    if (nextStepActions[i].action=saReactivateBreakpoint) and
       (nextStepActions[i].param=index)
    then
      nextStepActions[i].action:=saIgnore;
  end;

end;

function TGDBServerDebuggerInterface.disableBreakpoint(index: integer):boolean;
var
  s: string;
  bp: PGDBBreakpoint;
begin
  if (index<0) or (index>=breakpoints.Count) then raise exception.create('disableBreakpoint: invalid index');

  bp:=PGDBBreakpoint(breakpoints[index]);

  if bp=nil then
  begin
    exit;
  end;

  ObtainLock;
  try
    sendPacket('z'+inttostr(bp^.bptype)+','+inttohex(bp^.address)+','+inttostr(bp^.bpkind));
    s:=receivePacket;
    result:=s='OK';
  finally
    ReleaseLock;
  end;
end;

function TGDBServerDebuggerInterface.activateBreakpoint(index: integer): boolean;
var
  s: string;
  bp: PGDBBreakpoint;
begin
  if (index<0) or (index>=breakpoints.Count) then raise exception.create('activateBreakpoint: invalid index');

  bp:=PGDBBreakpoint(breakpoints[index]);

  if bp=nil then raise exception.create('Tried activating a deleted breakpoint');

  obtainlock;
  try
    sendPacket('Z'+inttostr(bp^.bptype)+','+inttohex(bp^.address)+','+inttostr(bp^.bpkind));
    s:=receivePacket;
    result:=s='OK';

  finally
    ReleaseLock;
  end;
end;

function TGDBServerDebuggerInterface.getcurrentpc: qword;
begin
  if currentpc=0 then
  begin
    ZeroMemory(context, gdbcontextHandler.ContextSize);
    GetThreadContext(currenttid, context);

    currentpc:=gdbcontextHandler.InstructionPointerRegister^.getValue(context);
  end;

  result:=currentpc;
end;

procedure TGDBServerDebuggerInterface.getProcessList(list: tstrings); //some GDB instances support a processlist
var
  i: integer;
  r: string;
  sa: TStringArray;
  pidstring: string;
  pname: string;
  s: string;
begin
  list.clear;
  obtainlock;
  try
    breakIfNeeded;

    sendPacket('qfProcessInfoInfo');
    r:=receivePacket;

    if (r='') or (r[1]='E') then exit;

    repeat
      sa:=r.split([';']);
      pidstring:='';
      pname:='';
      for i:=0 to length(sa)-1 do
      begin
        if sa[i].StartsWith('pid:') then
          pidstring:=copy(sa[i], 5);

        if sa[i].StartsWith('name:') then
          name:=copy(sa[i],6);
      end;

      if (pidstring<>'') and (name<>'') and (pidstring<>'0') then
      begin
        setlength(s,length(name) div 2);
        HexToBin(pchar(name), pchar(@s[1]), length(s));
        list.add(inttohex(strtoint(pidstring),8)+'-'+s);
      end;



      sendPacket('qsProcessInfo');
      r:=receivePacket;
    until (r='') or (r[1]='E');

    continueAfterManualStop;
  finally
    releaselock;
  end;
end;

procedure TGDBServerDebuggerInterface.getThreadList(var tl: TDwordArray);
var
  s: string;
  gotstopmessage: boolean;
  timeout: integer;
  wasstopped: boolean;
  tls: TStringArray;
  oldpos: integer;
  i: integer;
begin
  tl:=[];
  obtainlock;
  try

    timeout:=0;

    breakIfNeeded;

    sendPacket('qfThreadInfo');
    s:=receivePacket(1000);
    while (s<>'') and (s<>'l') do
    begin
      if s[1]='m' then
      begin
        tls:=copy(s,2).Split([',']);
        oldpos:=length(tl);
        setlength(tl, length(tl)+length(tls));
        for i:=0 to length(tls)-1 do
        begin
          tl[oldpos+i]:=strtoint('$'+tls[i]);
        end;
        sendPacket('qsThreadInfo');
        s:=receivePacket(1000);
      end
      else break;
    end;

    continueAfterManualStop;
  finally
    releaselock;
  end;

end;

function TGDBServerDebuggerInterface.needsToAttach: boolean;
begin
  result:=false;
end;

function TGDBServerDebuggerInterface.controlsTheThreadList: boolean;
begin
  result:=true;
end;

function TGDBServerDebuggerInterface.canReportExactDebugRegisterTrigger: boolean;
begin
  result:=false;
end;

function TGDBServerDebuggerInterface.usesDebugRegisters: boolean;
begin
  result:=false; //gdbserver owns them, do not touch
end;

function TGDBServerDebuggerInterface.stoptarget(out WasStopped: boolean): boolean;
//Lock must have been obtained before calling this
var
  c: byte;
  i: integer;
  s: string;
  timeout: integer;
begin
  wasStopped:=stopped;
  result:=stopped;

  if not stopped then
  begin
    ObtainLock;
    c:=3;
    timeout:=10;

    repeat
      fpsend(socket, @c,1,0);
      stoptargetused:=true;
      s:=receivePacket(1000,true);
      dec(timeout);
    until stopped or (timeout=0);

    ReleaseLock;
    result:=stopped;
  end;
end;

procedure TGDBServerDebuggerInterface.addStepAction(satype: TStepActionType; param: integer);
var e: integer;
begin
  e:=length(nextStepActions);
  setlength(nextStepActions, e+1);
  nextStepActions[e].action:=satype;
  nextStepActions[e].param:=param;;
end;


function TGDBServerDebuggerInterface.getThreadFromStopPacket(p: string): dword;
var
  s: string;
  params: TStringArray;
  i: integer;
  kv: TStringArray;
begin
  result:=0;
  if p[1] in ['S','T'] then
  begin
    s:=copy(p,4);
    params:=s.Split([';']);

    for i:=0 to length(params)-1 do
    begin
      kv:=params[i].split([':']);
      if kv[0]='thread' then
        exit(strtoint('$'+kv[1]));
    end;
  end;
end;

function TGDBServerDebuggerInterface.WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL;
var s: string;
  s2: string;
  params: TStringArray;
  signals: string;
  keyvalue: TStringArray;
  v: string;
  i,j: integer;

  sa: TStringArray;

  currentthreadlist: array of record
    threadid: integer;
    pc: qword;
  end;

  medata: array of qword;

  handleEvent: boolean;
  breakpointid: integer;

  v2: pchar;

begin
  //example:
  //T05thread:306c;hexname:7475746f7269616c2d7838365f36342e657865;00:0060240000000000;01:0000000000000000;02:0000000000000000;03:e0c9319af87f0000;04:0000000000000000;05:0000000000000000;06:0000000000000000;07:f8fe580700000000;08:0000000000000000;09:e0c9319af87f0000;0a:0000000000000000;0b:0000000000000000;0c:0000000000000000;0d:0000000000000000;0e:0000000000000000;0f:0000000000000000;10:81092f9af87f0000;11:4602000000000000;12:3300;13:5300;14:2b00;15:2b00;16:2b00;17:2b00;reason:breakpoint;


  {
  //windows:

  int3 breakpoint at : 10002C7DD = T05thread:81d4;hexname:7475746f7269616c2d7838365f36342e657865;threads:81d4,3df0,4a74,8890,5ebc,6e84;jstopinfo:5b7b226e616d65223a227475746f7269616c2d7838365f36342e657865222c22726561736f6e223a22627265616b706f696e74222c227369676e616c223a352c22746964223a33333233367d2c7b226e616d65223a227475746f7269616c2d7838365f36342e657865222c22746964223a31353835367d2c7b226e616d65223a227475746f7269616c2d7838365f36342e657865222c22746964223a31393036307d2c7b226e616d65223a227475746f7269616c2d7838365f36342e657865222c22746964223a33343936307d2c7b226e616d65223a227475746f7269616c2d7838365f36342e657865222c22746964223a32343235327d2c7b226e616d65223a227475746f7269616c2d7838365f36342e657865222c22746964223a32383239327d5d;thread-pcs:000000010002c7dd,00007ff89a2f08e4,00007ff89a2f08e4,00007ff89a2f08e4,00007ff89a2f08e4,00007ff89a2f08e4;00:0400000000000000;01:a05c620100000000;02:eccfeedf00000000;03:0500000000000000;04:b075280001000000;05:0000000000000000;06:70e93f0100000000;07:40e83f0100000000;08:9046300001000000;09:ea1c170000000000;0a:58189f0000000000;0b:4602000000000000;0c:0076620100000000;0d:0047160001000000;0e:706c280001000000;0f:a875280001000000;10:ddc7020001000000;11:0202000000000000;12:3300;13:5300;14:2b00;15:2b00;16:2b00;17:2b00;reason:breakpoint;
  write watch at 0x01556558 (22373720):         T05thread:2e58;hexname:7475746f7269616c2d7838365f36342e657865;threads:2e58;thread-pcs:000000010002c7e3;00:0000000000000000;01:605d550100000000;02:b1f8921100000000;03:0100000000000000;04:b075280001000000;05:0000000000000000;06:70e93f0100000000;07:40e83f0100000000;08:a2213b0a00000000;09:a2213b0a00000000;0a:60213c0100000000;0b:4602000000000000;0c:c076550100000000;0d:0047160001000000;0e:706c280001000000;0f:a875280001000000;10:e3c7020001000000;11:0602000000000000;12:3300;13:5300;14:2b00;15:2b00;16:2b00;17:2b00;reason:watchpoint;description:32323337333732302030203138343436373434303733373039353531363135;
        description=22373720 0 18446744073709551615




  stop: T05thread:8b00;hexname:7475746f7269616c2d7838365f36342e657865;threads:2e58,8b00;jstopinfo:5b7b226465736372697074696f6e223a2232323337333732302030203138343436373434303733373039353531363135222c226e616d65223a227475746f7269616c2d7838365f36342e657865222c22726561736f6e223a227761746368706f696e74222c227369676e616c223a352c22746964223a31313836347d2c7b226465736372697074696f6e223a22457863657074696f6e203078383030303030303320656e636f756e74657265642061742061646472657373203078376666383961326630393830222c226e616d65223a227475746f7269616c2d7838365f36342e657865222c22726561736f6e223a22657863657074696f6e222c227369676e616c223a352c22746964223a33353538347d5d;thread-pcs:00007ff897aea104,00007ff89a2f0981;00:00a0240000000000;01:0000000000000000;02:0000000000000000;03:e0c9319af87f0000;04:0000000000000000;05:0000000000000000;06:0000000000000000;07:f8fe5f0200000000;08:0000000000000000;09:e0c9319af87f0000;0a:0000000000000000;0b:0000000000000000;0c:0000000000000000;0d:0000000000000000;0e:0000000000000000;0f:0000000000000000;10:81092f9af87f0000;11:4602000000000000;12:3300;13:5300;14:2b00;15:2b00;16:2b00;17:2b00;reason:exception;description:457863657074696f6e203078383030303030303320656e636f756e74657265642061742061646472657373203078376666383961326630393830;
          description=Exception 0x80000003 encountered at address 0x7ff89a2f0980
                      0x7ff89a2f0980 = ntdll.DbgBreakpoint

  So: a stop is an int3 bp but not handled as breakpoint


  81d4=33236
  10002C7DD=4295149533

  jstopinfo=[{"name":"tutorial-x86_64.exe","reason":"breakpoint","signal":5,"tid":33236},{"name":"tutorial-x86_64.exe","tid":15856},{"name":"tutorial-x86_64.exe","tid":19060},{"name":"tutorial-x86_64.exe","tid":34960},{"name":"tutorial-x86_64.exe","tid":24252},{"name":"tutorial-x86_64.exe","tid":28292}]

  g;thread:81d4=0400000000000000eccfeedf000000000500000000000000a05c62010000000040e83f010000000070e93f01000000000000000000000000b0752800010000009046300001000000ea1c17000000000058189f0000000000460200000000000000766201000000000047160001000000706c280001000000a875280001000000ddc70200010000000202000000000000330053002b002b002b002b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000


  ---
  hw bp on mac (rosetta):
  bp at 104bc55a0: T05thread:7408fb;threads:7408fb,743c9d,740935;thread-pcs:104bc55a0,7ff8016c9cae,7ff8016c85c2;jstopinfo:5b7b22746964223a373630343437352c226d6574797065223a362c226d6564617461223a5b322c305d2c22726561736f6e223a22657863657074696f6e227d5d;                      00:0000000000000000;01:0000000000000000;02:48fa6f0e03000000;03:0000000000000000;04:a8f66f0e03000000;05:a8f66f0e03000000;06:10f76f0e03000000;07:88f56f0e03000000;08:f8ffffffff7f0000;09:a900000000000000;0a:3852e144f87f0000;0b:7bb88004f87f0000;0c:0000000000000000;0d:e85a160501000000;0e:50c5b40e01000000;0f:d0c9e50401000000;10:a055bc0401000000;11:4602000000000000;12:2b00000000000000;13:0000000000000000;14:0000000000000000;metype:6;mecount:2;medata:2;medata:0;                 memory:0x30e6ff710=30f76f0e03000000a73fdb0401000000;memory:0x30e6ff730=50f76f0e0300000039cbe50401000000;

  sw bp on mac (rosetta):
  bp at 104bc55a0: T05thread:7408fb;threads:7408fb,743c9d,740935;thread-pcs:104bc55a0,7ff8016c9cae,7ff8016c85c2;jstopinfo:5b7b22746964223a373630343437352c226d6574797065223a362c226d6564617461223a5b322c305d2c22726561736f6e223a22657863657074696f6e227d5d;                      00:0000000000000000;01:0000000000000000;02:48fa6f0e03000000;03:0000000000000000;04:a8f66f0e03000000;05:a8f66f0e03000000;06:10f76f0e03000000;07:88f56f0e03000000;08:f8ffffffff7f0000;09:a900000000000000;0a:3852e144f87f0000;0b:7bb88004f87f0000;0c:0000000000000000;0d:e85a160501000000;0e:50c5b40e01000000;0f:d0c9e50401000000;10:a055bc0401000000;11:4602000000000000;12:2b00000000000000;13:0000000000000000;14:0000000000000000;metype:6;mecount:2;medata:2;medata:0;                 memory:0x30e6ff710=30f76f0e03000000a73fdb0401000000;memory:0x30e6ff730=50f76f0e0300000039cbe50401000000;

  watch write on mac (rosetta):  (watchpointsBreakBeforeOperation=true)
  watch 1051d27e0: T05thread:7408fb;threads:7408fb,745323,740935;thread-pcs:104bc55a9,7ff8016c9cae,7ff8016c85c2;jstopinfo:5b7b22746964223a373630343437352c226d6574797065223a362c226d6564617461223a5b312c343338303736343132382c305d2c22726561736f6e223a22657863657074696f6e227d5d;00:0000000000000000;01:0000000000000000;02:48fa6f0e03000000;03:0000000000000000;04:a8f66f0e03000000;05:a8f66f0e03000000;06:80f56f0e03000000;07:70f56f0e03000000;08:f8ffffffff7f0000;09:a900000000000000;0a:3852e144f87f0000;0b:7bb88004f87f0000;0c:0000000000000000;0d:e85a160501000000;0e:50c5b40e01000000;0f:d0c9e50401000000;10:a955bc0401000000;11:4602000000000000;12:2b00000000000000;13:0000000000000000;14:0000000000000000;metype:6;mecount:3;medata:1;medata:1051d27e0;medata:0;memory:0x30e6ff580=10f76f0e030000002656bc0401000000;memory:0x30e6ff710=30f76f0e03000000a73fdb0401000000;

  stop:  T11thread:7a5ed8;threads:7a5ed8,7e6cf8,7a5f05;thread-pcs:7ff8016c85c2,7ff8016c9cae,7ff8016c85c2;jstopinfo:5b7b22746964223a383031393637322c226d6574797065223a352c226d6564617461223a5b36353533392c31375d2c22726561736f6e223a22657863657074696f6e227d5d;00:2f00000100000000;01:5000480d03000000;02:0000000003500000;03:0000000002000000;04:5000480d03000000;05:0608000705000000;06:b0ff470d03000000;07:58ff470d03000000;08:0000000000000000;09:0000000003500000;0a:0000000003500000;0b:0000000000000000;0c:0000000003500000;0d:0608000705000000;0e:0000000003500000;0f:0000000000000000;10:c2856c01f87f0000;11:4602000000000000;12:2b00000000000000;13:0000000000000000;14:0000000000000000;metype:5;mecount:2;medata:10003;medata:11;memory:0x30d47ffb0=d000480d03000000f4f56c01f87f0000;memory:0x30d4800d0=0001480d03000000aa886c01f87f0000;

  single step trace:
        T05thread:7a5ed8;threads:7a5ed8,80d4a9,7a5f05,80d09c;thread-pcs:10411d2f1,7ff801702bb0,7ff8016c85c2,7ff8016c9cae;jstopinfo:5b7b22746964223a383031393637322c226d6574797065223a362c226d6564617461223a5b312c305d2c22726561736f6e223a22657863657074696f6e227d5d;00:0000000000000000;01:0000000000000000;02:480a480d03000000;03:0000000000000000;04:a806480d03000000;05:a806480d03000000;06:1007480d03000000;07:8005480d03000000;08:f8ffffffff7f0000;09:a900000000000000;0a:3852e144f87f0000;0b:7bb88004f87f0000;0c:0000000000000000;0d:48db6b0401000000;0e:5095070e01000000;0f:f0483b0401000000;10:f1d2110401000000;11:4602000000000000;12:2b00000000000000;13:0000000000000000;14:0000000000000000;metype:6;mecount:2;medata:1;medata:0;memory:0x30d480710=3007480d03000000c7be300401000000;memory:0x30d480730=5007480d03000000594a3b0401000000;


  stop=       metype:5;mecount:2;medata:10003;medata:11
  step=       metype:6;mecount:2;medata:1;medata:0;
  watch write=metype:6;mecount:3;medata:1;medata:1051d27e0;medata:0;
  bp=         metype:6;mecount:2;medata:2;medata:0

  -----



  watchpoint at 01588338: T05thread:7ac0;hexname:7475746f7269616c2d7838365f36342e657865;threads:7ac0,7a0c,838c,3a20,3584,5978;jstopinfo:5b7b226465736372697074696f6e223a2232323537373937362030203138343436373434303733373039353531363135222c226e616d65223a227475746f7269616c2d7838365f36342e657865222c22726561736f6e223a227761746368706f696e74222c227369676e616c223a352c22746964223a33313432347d2c7b226e616d65223a227475746f7269616c2d7838365f36342e657865222c22746964223a33313234347d2c7b226e616d65223a227475746f7269616c2d7838365f36342e657865222c22746964223a33333637367d2c7b226e616d65223a227475746f7269616c2d7838365f36342e657865222c22746964223a31343838307d2c7b226e616d65223a227475746f7269616c2d7838365f36342e657865222c22746964223a31333730307d2c7b226e616d65223a227475746f7269616c2d7838365f36342e657865222c22746964223a32323930347d5d;thread-pcs:000000010002c7e3,00007ff89a2f08e4,00007ff89a2f08e4,00007ff89a2f08e4,00007ff89a2f08e4,00007ff89a2f08e4;00:0100000000000000;01:407b580100000000;02:5bdaf66400000000;03:0200000000000000;04:b075280001000000;05:0000000000000000;06:70e93f0100000000;07:40e83f0100000000;08:4c1b0a0000000000;09:4c1b0a0000000000;0a:8016690200000000;0b:4602000000000000;0c:a094580100000000;0d:0047160001000000;0e:706c280001000000;0f:a875280001000000;10:e3c7020001000000;11:0602000000000000;12:3300;13:5300;14:2b00;15:2b00;16:2b00;17:2b00;reason:watchpoint;description:32323537373937362030203138343436373434303733373039353531363135;

  jstopinfo=[{"description":"22577976 0 18446744073709551615","name":"tutorial-x86_64.exe","reason":"watchpoint","signal":5,"tid":31424},{"name":"tutorial-x86_64.exe","tid":31244},{"name":"tutorial-x86_64.exe","tid":33676},{"name":"tutorial-x86_64.exe","tid":14880},{"name":"tutorial-x86_64.exe","tid":13700},{"name":"tutorial-x86_64.exe","tid":22904}]
  description=22577976 0 18446744073709551615

  22577976=0x1588338

  on macos a watchpoint has metype:6 , medata:1, medata:addresswatched

  }
  currentthreadlist:=[];
  currentpc:=0;
  ObtainLock;
  result:=false;

  try
    if not stopped then
    begin
      s:=receivePacket(dwMilliseconds,true);
      if not stopped then
      begin
        if s<>'' then
        begin
          if s[1]='O' then
            OutputDebugString(pchar(copy(s,2)))
          else
            OutputDebugString(pchar('Unexpected packet received:'+s));

          exit(false);
        end
        else
        begin
          //nothing received
          if QueriedHasToBreakFirst=false then
          begin
            sendPacket('qfThreadInfo');
            s:=receivePacket(dwMilliseconds);
            if (not stopped) then
            begin
              QueriedHasToBreakFirst:=true;
              if s<>'' then
                hasToBreakFirst:=false;

              exit(false);
            end;
            //it got stopped, the query was invalid
          end
          else
            exit(false);

        end;


      end;
    end;

    lpDebugEvent.dwProcessId:=pid;

    if stoppacket<>'' then
    begin
      {$ifdef darwin}

      outputdebugstring('waitfordebugevent received stoppacket: '+stoppacket);

      {$endif}


      result:=true;
      addressThatCausedBreakpoint:=$ffffffffffffffff;
      //wasSoftwareBreakpoint:=false;

      case stoppacket[1] of
        'S','T':
        begin
          //stopped due to a signal (in case of T, more info is provided)
          handleEvent:=false;

          s2:=copy(stoppacket,2);
          signals:=copy(s2,1,2);
          signal:=strtoint('$'+signals);
          s2:=copy(s2,3);
          params:=s2.split(';');

          if stoppackethl=nil then
            stoppackethl:=TStringHashList.Create(true);

          for i:=0 to stoppackethl.Count-1 do
            StrDispose(stoppackethl.list[i]^.Data);

          stoppackethl.Clear;

          medata:=[];
          for i:=0 to length(params)-1 do
          begin
            keyvalue:=params[i].Split([':']);
            if keyvalue[0]='medata' then //for some reason this data is sent using the same name mecount times...
            begin
              keyvalue[0]:='medata'+inttostr(length(medata));
              setlength(medata, length(medata)+1);
              medata[length(medata)-1]:=strtoint64('$'+keyvalue[1]);
            end;

            stoppackethl.Add(keyvalue[0],strnew(pchar(keyvalue[1])));
          end;

          v:=pchar(stoppackethl['thread']);
          if v<>'' then
          begin
            lpDebugEvent.dwThreadId:=strtoint('$'+v);
            currenttid:=lpDebugEvent.dwThreadId;
          end;

          v:=pchar(stoppackethl['threads']);
          if v<>'' then
          begin
            //e.g: v='7408fb,740b10,740b11,740ab7,740935,740b12'
            sa:=v.Split([',']);
            setlength(currentthreadlist, length(sa));
            for j:=0 to length(sa)-1 do
            begin
              currentthreadlist[j].threadid:=strtoint64('$'+sa[j]);
              currentthreadlist[j].pc:=0;
            end;
          end;

          v:=pchar(stoppackethl['thread-pcs']);
          if v<>'' then
          begin
            sa:=v.Split([',']);
            if length(sa)=length(currentthreadlist) then
            begin
              for j:=0 to length(sa)-1 do
              begin
                currentthreadlist[j].pc:=StrToInt64('$'+sa[j]);
                if currenttid=currentthreadlist[j].threadid then
                  currentpc:=currentthreadlist[j].pc;
              end;
            end
            else
              setlength(currentthreadlist,0);
          end;

          v:=pchar(stoppackethl['metype']);
          if v='6' then
          begin
            if length(medata)>=2 then
            begin

              if (length(medata)=2) and (medata[0]=1) and (medata[1]=0) then
              begin
                handleEvent:=true;
                breakpointid:=-1;
                isStepping:=true;
              end
              else
              if medata[0]=1 then //watchpoint hit
              begin
                //medata[1] is the address
                addressThatCausedBreakpoint:=medata[1];

                breakpointid:=findWatchpointBreakpoint(addressThatCausedBreakpoint);
                if breakpointid<>-1 then
                begin
                  handleEvent:=true;
                  breakpointid:=breakpointid;

                  disableBreakpoint(breakpointid);
                  addStepAction(saReactivateBreakpoint, breakpointid);
                end;
              end
              else if medata[0]=2 then //execute bp hit
              begin
                currentpc:=0;
                getcurrentpc;
                addressThatCausedBreakpoint:=currentpc;

                breakpointid:=findExecuteBreakpoint(currentpc);
                if breakpointid<>-1 then
                begin
                  handleEvent:=true;
                  disableBreakpoint(breakpointid);
                  addStepAction(saReactivateBreakpoint, breakpointid);
                end;
              end;
            end;
          end
          else
          begin
            v:=pchar(stoppackethl['reason']);

            if v='breakpoint' then //exec bp
            begin
              getcurrentpc;
              addressThatCausedBreakpoint:=currentpc;

              breakpointid:=findExecuteBreakpoint(currentpc);
              if breakpointid<>-1 then
              begin
                handleEvent:=true;
                disableBreakpoint(breakpointid);
                addStepAction(saReactivateBreakpoint, breakpointid);
              end;
            end
            else
            if v='watchpoint' then
            begin
              v:=pchar(stoppackethl['description']);
              i:=(length(v) div 2)+1;

              getmem(v2, i);
              i:=HexToBin(pchar(v), v2,i);
              v2[i]:=#0;
              v:=v2;
              freemem(v2);
              //e.g: 22373720 0 18446744073709551615
              sa:=v.Split([' ']);
              if length(sa)>=1 then
              begin
                addressThatCausedBreakpoint:=strtoint64(sa[0]);
                breakpointid:=findWatchpointBreakpoint(addressThatCausedBreakpoint);
                if breakpointid<>-1 then
                begin
                  handleEvent:=true;

                  if watchpointsTriggerAfterExecution=false then
                  begin
                    disableBreakpoint(breakpointid);
                    addStepAction(saReactivateBreakpoint, breakpointid);
                  end;
                end;
              end;
            end
            else
            if v='trace' then
            begin
              breakpointid:=-1;
              addressThatCausedBreakpoint:=getcurrentpc;
              isStepping:=true;
              handleEvent:=true;
            end;
          end;

          if handleEvent then
          begin
            lpDebugEvent.dwDebugEventCode:=EXCEPTION_DEBUG_EVENT;
            lpDebugEvent.Exception.dwFirstChance:=1;
            lpDebugEvent.Exception.ExceptionRecord.ExceptionAddress:=pointer(getcurrentpc);
            lpDebugEvent.Exception.ExceptionRecord.ExceptionCode:=EXCEPTION_GDB_BREAKPOINT;
            lpDebugEvent.Exception.ExceptionRecord.ExceptionFlags:=breakpointid; //-1 on step
            lpDebugEvent.Exception.ExceptionRecord.NumberParameters:=1;
            lpDebugEvent.Exception.ExceptionRecord.ExceptionInformation[0]:=addressThatCausedBreakpoint;
            exit(true);
          end
          else
          begin
            //nothing I can handle, continue
            sendPacket('c');
            exit(false);
          end;
        end;

        'W','X': //process exit
        begin
          lpDebugEvent.dwDebugEventCode:=EXIT_PROCESS_DEBUG_EVENT;
          s2:=copy(stoppacket,2);
          s2:=s2.Split([';'])[1];
          lpDebugEvent.ExitProcess.dwExitCode:=Swap(strtoint(copy(s2,2)));
          exit(true);
        end;
      {
        'w': //thread exit
        begin
          signals:=copy(stoppacket,2,2);
          signal:=strtoint('$'+signals);

          s2:=copy(stoppacket,5);

          lpDebugEvent.dwThreadId:=strtoint(s2);
          sendPacket('c');
          exit(false);  //not handled atm
        end; }

       { 'N': //no threads existing
        begin
         // raise exception.create('Invalid no-threads-existing event received. A thread may have been lost');
        end;  }

        else
        begin
          //unhandled
          OutputDebugString('Unhandled packet');
          sendPacket('c');
          exit(false);
        end;


      end;

      stopped:=true;
    end
    else
    begin
      //messed up state
      OutputDebugString('Stopped was true but no stoppacket set... Continuing');
      stopped:=false;
      sendPacket('c');
      result:=false;
    end;


  finally
    ReleaseLock;
  end;
end;

function TGDBServerDebuggerInterface.ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL;
var
  c: string;
  s: string;
  tid: dword;
  i: integer;
  didstepalready: boolean;
  timeout: integer;
  ws: boolean;
begin
  ObtainLock;
  try
    didstepalready:=false;
    if stopped then
    begin

      if length(nextStepActions)>0 then
      begin
        //step this thread once and apply the step actions
        c:='vCont;s:'+currenttid.ToHexString(1);
        sendPacket(c);

        OutputDebugString('stepactionhandler: single stepping one time to do a step action');
        stoppacket:='';
        timeout:=0;
        repeat
          s:=receivePacket(1000,true);
          inc(timeout);
        until isStopPacket(s) or (timeout>5);

        if (timeout>5) and (isStopPacket(s)=false) then
        begin
          outputdebugstring('stepactionhandler: Failure waiting for break after single step. Giving up');
          stoptarget(ws);
          sendPacket('c');
          exit(true);
        end;

        tid:=getThreadFromStopPacket(s);
        if tid<>currenttid then
        begin
          //wtf?
          exit(true); //give up for now (nextStepActions is still set, try next continue)
        end;
        didstepalready:=true;

        for i:=0 to length(nextStepActions)-1 do
        begin
          if nextStepActions[i].action=saReactivateBreakpoint then
            activateBreakpoint(nextStepActions[i].param);
        end;

        setlength(nextStepActions,0);
      end;

      case dwContinueStatus of
        DBG_CONTINUE:
        begin
          c:='c';
        end;

        DBG_EXCEPTION_NOT_HANDLED:
        begin
          c:='c'; //C'+signal.ToHexString(2);
        end;

        DBG_CONTINUE_SINGLESTEP: if didstepalready then exit(true) else
        begin
          if canvCont then
            c:='vCont;s'
          else
            c:='s';
        end;
        DBG_CONTINUE_SINGLESTEP_UNHANDLED: if didstepalready then exit(true) else
        begin
          if canvCont then
            c:='vCont;S'+signal.ToHexString(2)
          else
            c:='S'+signal.ToHexString(2);
        end;
      end;

      if c.StartsWith('vCont') then
      begin
        if dwThreadID<>$ffffffff then
        begin
          c:=c+':'+dwThreadID.ToHexString(1);
        end;
      end;

      sendPacket(c);
      stopped:=false;
      stoppacket:='';
      result:=true;
    end
    else
      result:=false;

  finally
    clearOldContexts;
    ReleaseLock;
  end;
end;

procedure TGDBServerDebuggerInterface.clearOldContexts;
var
  mi:TMapIterator;
  oldcontext: Pointer;
  k: dword;
begin
  mi:=TMapIterator.Create(oldcontexts);
  mi.First;
  while not mi.EOM do
  begin
    mi.GetData(oldcontext);
    freemem(oldcontext);
    mi.Next;
  end;
  mi.free;

  oldcontexts.Clear;
end;


function TGDBServerDebuggerInterface.ApplyRegisterChanges(hThread: THandle; reglist: PContextElementRegisterList; oldc, newc: pointer): boolean;
var
  i,j: integer;
  s,r: string;
  hexstring: pchar;
  hexstringsize: integer;
begin
  if reglist=nil then exit(false);
  hexstringsize:=2;
  getmem(hexstring,hexstringsize+1);

  result:=true;
  for i:=0 to length(reglist^)-1 do
  begin
    if reglist^[i].getValue(oldc)<>reglist^[i].getValue(newc) then
    begin
      if reglist^[i].size*2>hexstringsize then
      begin
        hexstringsize:=reglist^[i].size*2;
        ReAllocMem(hexstring, hexstringsize+1);
      end;
      BinToHex(reglist^[i].getPointer(newc), hexstring, reglist^[i].size);
      hexstring[reglist^[i].size*2]:=#0;

      s:='P'+reglist^[i].internalidentifier.ToHexString(1)+'='+hexstring+';thread:'+inttohex(hthread,1);
      sendPacket(s);
      r:=receivePacket;
      if r<>'OK' then
        result:=false;
    end;

  end;
end;

function TGDBServerDebuggerInterface.SetThreadContext(hThread: THandle; lpContext: pointer; isFrozenThread: Boolean=false): BOOL;
var
  r1,r2: boolean;
  oldc: pointer;
  d: dword;

begin
  result:=true;
  ObtainLock;
  breakIfNeeded;
  try
    d:=hThread;
    if oldcontexts.GetData(d,oldc) then
    begin
      //figure out the differences and apply the changes reg by reg
      if gdbcontextHandler.isDifferent(lpContext, oldc) then
      begin
        //sendPacket('Gxxxxxx;thread:'+hThread.ToHexString); doesn't seem to work


        //apply the differences
        r1:=ApplyRegisterChanges(hThread, gdbcontextHandler.getGeneralPurposeRegisters, oldc, lpContext);
        r2:=ApplyRegisterChanges(hThread, gdbcontextHandler.getFloatingPointRegisters,  oldc, lpContext);
        result:=r1 and r2;

        if result then
          copymemory(oldc, lpContext, gdbcontextHandler.ContextSize); //apply the changes to the local copy so they don't get written again
      end;
    end;



  finally
    continueAfterManualStop;
    ReleaseLock;
  end;
end;

function TGDBServerDebuggerInterface.GetThreadContext(hThread: THandle; lpContext: pointer; isFrozenThread: Boolean=false): BOOL;
var
  r: string;

  i: integer;
  oldcontext: pbyte;
  d: dword;
  rl: PContextElementRegisterList;
begin
  ObtainLock;
  breakIfNeeded;
  try
    if canUseContextCommands then
    begin
      sendPacket('g;thread:'+hThread.ToHexString);
      r:=receivePacket;
    end
    else
    begin
      //get each register seperately
      sendPacket('Hg'+hThread.ToHexString);
      r:=receivePacket;

      if r<>'OK' then
        OutputDebugString(pchar('GetThreadContext: Setting thread to '+hThread.ToHexString+' failed'));

      ZeroMemory(lpContext, gdbcontextHandler.ContextSize);
      rl:=gdbcontextHandler.getGeneralPurposeRegisters;
      for i:=0 to length(rl^)-1 do
      begin
        sendPacket('p'+inttohex(rl^[i].internalidentifier,2));
        r:=receivePacket;
        if r<>'' then
          HexToBin(pchar(r), pchar(@rl^[i].ContextOffset), rl^[i].size);
      end;

      rl:=gdbcontextHandler.getFloatingPointRegisters;
      for i:=0 to length(rl^)-1 do
      begin
        sendPacket('p'+inttohex(rl^[i].internalidentifier,2));
        r:=receivePacket;
        if r<>'' then
          HexToBin(pchar(r), pchar(@rl^[i].ContextOffset), rl^[i].size);
      end;
    end;

  finally
    continueAfterManualStop;
    ReleaseLock;
  end;

  r:=StringReplace(r,'x','0',[rfReplaceAll]);
  i:=HexToBin(pchar(r),pchar(lpcontext),gdbcontextHandler.ContextSize);

  if oldcontexts.HasId(hthread)=false then
  begin
    getmem(oldcontext, gdbcontextHandler.ContextSize);
    copymemory(oldcontext,lpContext,gdbcontextHandler.ContextSize);
    d:=hthread;
    oldcontexts.Add(d,oldcontext);
  end;

  result:=i=gdbcontextHandler.ContextSize;
end;

function TGDBServerDebuggerInterface.allocateMemory(size: integer; protections: string): ptruint;
var
  r: string;
begin
  result:=0;
  ObtainLock;
  breakIfNeeded;
  try
    sendPacket('_M'+size.ToHexString+','+protections);
    r:=receivePacket;
    if (r<>'') and (r[1]<>'E') then
    begin
      try
        result:=StrToInt64('$'+r);
      except
      end;
    end;
  finally
    continueAfterManualStop;
    ReleaseLock;
  end;
end;

function TGDBServerDebuggerInterface.deallocateMemory(address: ptruint): boolean;
var
  r: string;
begin
  result:=false;
  ObtainLock;
  breakIfNeeded;
  try
    sendpacket('_m'+address.ToHexString);
    r:=receivePacket;
    result:=r='OK';
  finally
    continueAfterManualStop;
    ReleaseLock;
  end;

end;

function TGDBServerDebuggerInterface.saveState: boolean;
//only call when actually stopped
var
  tid: dword;
  r: string;
begin

  tid:=getThreadFromStopPacket(stoppacket);
  if cannotuseQSaveRegisterState=false then
  begin
    sendPacket('QSaveRegisterState;thread:'+tid.ToHexString);
    r:=receivePacket;
    outputdebugstring('QSaveRegisterState returned '+r);

    if (r<>'') and (r[1]<>'E') then
    begin
      try
        savedStateIndex:=strtoint(r);

        exit(true);
      except
        cannotuseQSaveRegisterState:=true;
      end;
    end;
  end;

  if savedState<>nil then
    freememandnil(savedState);

  getmem(savedState, gdbcontexthandler.ContextSize);
  result:=GetThreadContext(tid,savedstate);

end;

function TGDBServerDebuggerInterface.restoreState: boolean;
var
  tid: dword;
begin
  tid:=getThreadFromStopPacket(stoppacket);
  if cannotuseQSaveRegisterState=false then
  begin
    sendpacket('QRestoreRegisterState:'+savedstateindex.ToString+';thread:'+tid.ToHexString);
    exit(receivePacket='OK');
  end;


  if savedstate<>nil then
    result:=SetThreadContext(tid,savedstate);
end;

function TGDBServerDebuggerInterface.canUseIPT: boolean;
begin
  result:=false;

end;


function TGDBServerDebuggerInterface.getCurrentProgramcounter :ptruint;
var s: array of byte;
   thread: dword;
   r: string;
begin
  if not stopped then raise exception.create('ProgramCounter read while not suspended');
  ObtainLock;
  try
    setlength(s, gdbcontextHandler.InstructionPointerRegister^.size);

    thread:=getThreadFromStopPacket(stoppacket);

    sendpacket('p'+inttohex(gdbcontextHandler.InstructionPointerRegister^.internalidentifier,1)+';thread:'+thread.tohexstring);
    r:=receivePacket;
    if r<>'' then
      HexToBin(pchar(r), @s[0], gdbcontextHandler.InstructionPointerRegister^.size);

    result:=0;
    copymemory(@result,@s[0],min(sizeof(ptruint), gdbcontextHandler.InstructionPointerRegister^.size));

  finally
    ReleaseLock;
  end;
end;

procedure TGDBServerDebuggerInterface.setCurrentProgramcounter(newpc: ptruint);
var s: string;
  thread: dword;
  r: string;
  buf: array of byte;
begin
  if not stopped then raise exception.create('ProgramCounter write while not suspended');
  ObtainLock;
  try

    setlength(buf, gdbcontextHandler.InstructionPointerRegister^.size);
    zeromemory(@buf[0], gdbcontextHandler.InstructionPointerRegister^.size);

    copymemory(@buf[0],@newpc, min(sizeof(newpc), gdbcontextHandler.InstructionPointerRegister^.size) );

    setlength(s, gdbcontextHandler.InstructionPointerRegister^.size*2);

    BinToHex(@buf[0], @s[1], gdbcontextHandler.InstructionPointerRegister^.size);


    thread:=getThreadFromStopPacket(stoppacket);
    sendpacket('P'+inttohex(gdbcontextHandler.InstructionPointerRegister^.internalidentifier,1)+'='+s+';thread:'+thread.tohexstring);
    r:=receivePacket;
    if r<>'OK' then
      raise exception.create('Failure setting programcounter');


  finally
    ReleaseLock;
  end;
end;

function TGDBServerDebuggerInterface.DebugActiveProcess(dwProcessId: DWORD): WINBOOL;
type TRegList=(rlUnkown, rlGPR, rlFloats);
var
  i,j: integer;
  r,t: string;
  d: TXMLDocument;
  ss: tstringstream;

  target: string;
  position: integer;
  blocksize: integer;
  reading: boolean;
  target_xml: string;

  key,value: string;
  reginfo: TStringArray;
  reg: PContextElement_register;
  regnr: integer;
  v: integer;

  addToList: Treglist;
  formatguess: Treglist;
  n: TDOMElement;
  nreg: TDOMElement;
  wasstopped: boolean;
  nextoffset: dword;
begin
  {$ifndef STANDALONEDEBUG}
  if (processhandler.processid=0) or (processhandler.processid<>dwProcessId) then
  begin
    debuggerAttachStatus:='DebugActiveProcess: Opening process';
    processhandler.processid:=dwProcessID;
    Open_Process;
    symhandler.kernelsymbols:=true;
    symhandler.reinitialize;
  end;
  {$endif}

  ObtainLock;
  try
    stopped:=false;
    pid:=dwProcessId;

    sendPacket('vAttach;'+dwProcessId.ToHexString);
    r:=receivePacket(2000,true);
    if (r<>'') and (r[1]='E') then exit(false);

    if r='OK' then
      exit(true);

    if r='' then //no error, also no stop
    begin
      sendPacket('?'); //what state are we in ?
      r:=receivePacket(2000,true);
      if (r='') or (r[1]='E') then
      begin
        if not stoptarget(wasstopped) then exit(false); //can't stop the target either

      end;
    end;

    position:=0;
    blocksize:=0;
    if maxpacketsize<>0 then
      blocksize:=maxpacketsize-1
    else
      blocksize:=256;

    reading:=true;
    target_xml:='';

    while reading do
    begin
      sendPacket('qXfer:features:read:target.xml:'+position.ToHexString+','+inttohex(blocksize));
      r:=receivePacket(2000);
      if r='' then break; //not supported at all

      t:=copy(r,2);


      case r[1] of
        'm': target_xml:=target_xml+t;
        'l':
        begin
          target_xml:=target_xml+t;
          break;
        end;
        else break;
      end;

      inc(position,length(t));
    end;


    nextoffset:=0;

    if target_xml<>'' then
    begin
      try
        ss:=tstringstream.create(target_xml);
        {$ifdef CPUFEATURESDEBUG}
        ss.SaveToFile('d:\cpufeatures-orig.xml');
        {$endif}
        ReadXMLFile(d, ss);
        ss.free;

        n:=TDOMElement(d.findnode('target'));
        if n<>nil then
          n:=TDOMElement(n.FindNode('feature'));

        if n<>nil then
        begin
          if gdbcontextHandler=nil then //at least one register was found
            gdbcontextHandler:=TGDBServerContextHandler.create;

          for regnr:=0 to n.ChildNodes.Count-1 do
          begin
            nreg:=TDOMElement(n.ChildNodes[regnr]);
            if nreg<>nil then
            begin
              if nreg.TagName='reg' then
              begin
                addToList:=rlUnkown;
                formatguess:=rlUnkown;
                getmem(reg, sizeof(TContextElement_register));
                ZeroMemory(reg,sizeof(TContextElement_register));

                reg^.name:=nreg.GetAttribute('name');

                value:=nreg.GetAttribute('bitsize');
                reg^.size:=strtoint(value) div 8;

                value:=nreg.GetAttribute('offset');
                if value='' then
                begin
                  canUseContextCommands:=false;
                  reg^.ContextOffset:=nextoffset;
                  nextoffset:=nextoffset+reg^.size;
                end
                else
                  reg^.ContextOffset:=strtoint(value);





                value:=nreg.GetAttribute('regnum');
                if value<>'' then
                  reg^.internalidentifier:=strtoint(value);

                value:=nreg.GetAttribute('group');

                if (value='General Purpose Registers') or (value='general') then
                  addToList:=rlGPR //0=gpr
                else if value='Floating Point Registers' then
                  addToList:=rlFloats
                else
                begin
                  asm
                    nop
                  end;
                end;

                value:=nreg.GetAttribute('format');
                if (value='float') or value.StartsWith('vector-') then
                   formatguess:=rlFloats
                 else
                   formatguess:=rlGPR;

                value:=nreg.GetAttribute('generic');
                if value='pc' then gdbcontextHandler.SetInstructionPointerRegister(reg) else
                if value='sp' then gdbcontextHandler.SetStackPointerRegister(reg) else
                if value='fp' then gdbcontextHandler.SetFramePointerRegister(reg);

                if addToList=rlUnkown then  //guess based on the format
                  addToList:=formatguess;

                case addToList of
                  rlGPR:  gdbcontextHandler.addGeneralPurposeRegister(reg);
                  rlFloats: gdbcontextHandler.addMainFPURegister(reg);
                end;

              end;
            end;
          end;
        end;

        {$ifdef CPUFEATURESDEBUG}
        WriteXMLFile(d,'d:\cpufeatures.xml');
        {$endif}

        d.free;
      except
      end;
    end;

    if gdbcontextHandler=nil then
    begin
      //use qRegisterInfo
      gdbcontextHandler:=TGDBServerContextHandler.create;

      for regnr:=0 to 255 do
      begin
        addToList:=rlUnkown;
        formatguess:=rlUnkown;

        sendPacket('qRegisterInfo'+inttohex(regnr,2));
        r:=receivePacket(2000);
        if (r='') or (r[1]='E') then break;

        getmem(reg, sizeof(TContextElement_register));
        ZeroMemory(reg,sizeof(TContextElement_register));

        reg^.internalidentifier:=regnr;

        reginfo:=r.Split([';']);
        for i:=0 to length(reginfo)-1 do
        begin

          j:=pos(':',reginfo[i]);
          if j<>0 then
          begin
            key:=copy(reginfo[i],1,j-1);
            value:=copy(reginfo[i],j+1);

            case key of
              'name':
              begin
                reg^.name:=value;
              end;

              'offset':
              begin
                v:=strtoint(value);
                reg^.ContextOffset:=v;
              end;

              'bitsize':
              begin
                v:=strtoint(value);
               { if v<8 then //edit: apparently GDB doesnb't do flags...
                begin
                  //this is a flags register   (should never happen)
                  reg^.entrytype:=1;
                  reg^.size:=v;
                end
                else }
                begin
                  reg^.entrytype:=0;
                  reg^.size:=v div 8;

                end;

              end;

              'set':
              begin
                if value='General Purpose Registers' then
                  addToList:=rlGPR //0=gpr
                else if value='Floating Point Registers' then
                  addToList:=rlFloats
                else
                  begin
                    asm
                    nop
                    end;
                  end;
              end;

              'format':
              begin
                if (value='float') or value.StartsWith('vector-') then
                  formatguess:=rlFloats
                else
                  formatguess:=rlGPR;
              end;



              'generic':
              begin
                if value='pc' then
                begin
                  gdbcontextHandler.SetInstructionPointerRegister(reg);
                end;
                if value='sp' then gdbcontextHandler.SetStackPointerRegister(reg);

                if value='fp' then gdbcontextHandler.SetFramePointerRegister(reg);
              end;
            end;

          end;
        end;


        if addToList=rlUnkown then  //guess based on the format
          addToList:=formatguess;

        case addToList of
          rlGPR:  gdbcontextHandler.addGeneralPurposeRegister(reg);
          rlFloats: gdbcontextHandler.addMainFPURegister(reg);
        end;
      end;


    end;


    if gdbcontextHandler<>nil then
    begin
      if gdbcontextHandler.InstructionPointerRegister=nil then
      begin
        //try to find it manually:
        for i:=0 to length(gdbcontextHandler._general)-1 do
        begin
          if (gdbcontextHandler._general[i]^.name='rip') or
             (gdbcontextHandler._general[i]^.name='eip') or
             (gdbcontextHandler._general[i]^.name='ip') then
          begin
            gdbcontextHandler.SetInstructionPointerRegister(gdbcontextHandler._general[i]);
            break;
          end;
        end;

        if gdbcontextHandler.InstructionPointerRegister=nil then
          raise exception.create('No instruction pointer register found');
      end;

      if gdbcontextHandler.StackPointerRegister=nil then
      begin
        for i:=0 to length(gdbcontextHandler._general)-1 do
        begin
          if (gdbcontextHandler._general[i]^.name='rsp') or
             (gdbcontextHandler._general[i]^.name='esp') or
             (gdbcontextHandler._general[i]^.name='sp') then
          begin
            gdbcontextHandler.SetStackPointerRegister(gdbcontextHandler._general[i]);
            break;
          end;
        end;
        if gdbcontextHandler.StackPointerRegister=nil then
          raise exception.create('No stack register found');
      end;

      if gdbcontextHandler.FramePointerRegister=nil then
      begin
        for i:=0 to length(gdbcontextHandler._general)-1 do
        begin
          if (gdbcontextHandler._general[i]^.name='rbp') or
             (gdbcontextHandler._general[i]^.name='ebp') or
             (gdbcontextHandler._general[i]^.name='bp') then
          begin
            gdbcontextHandler.SetFramePointerRegister(gdbcontextHandler._general[i]);
            break;
          end;
        end;

        if gdbcontextHandler.FramePointerRegister=nil then
          raise exception.create('No framepointer register found');
      end;

      gdbcontextHandler.finalizeLists;

      getmem(context, gdbcontextHandler.ContextSize);
    end;

    hasToBreakFirst:=true;

{$ifndef STANDALONEDEBUG}
    if lowercase(gdbcontextHandler.InstructionPointerRegister^.name).EndsWith('ip') then   //todo: if more architectures get added, change this   (e.g read the actual architecture field)
      processhandler.SystemArchitecture:=archX86
    else
      processhandler.SystemArchitecture:=archArm;
{$endif}

    exit(true);
    //fAttach:PIDHEX
    //once successfull do a qXfer:features:read:target.xml:0,maxpacketsize-1 and fill in a contexthandler
  finally
    ReleaseLock;
  end;
end;

function TGDBServerDebuggerInterface.DebugActiveProcessStop(dwProcessID: DWORD): WINBOOL;
var
  s: string;
  wasstopped: boolean;
begin
  OutputDebugString('TGDBServerDebuggerInterface.DebugActiveProcessStop');

  ObtainLock;
  OutputDebugString('got lock');

  stoptarget(wasstopped);

  OutputDebugString('stopped target. Sending ''D''');
  sendPacket('D');

  ReleaseLock;
  OutputDebugString('released lock');
  {$ifdef windows}
  closehandle(socket);
  {$else}
  fpclose(socket);
  {$endif}

  if gdbserverExecutor<>nil then
  begin
    OutputDebugString('terminating gdbserverExecutor');
    gdbserverExecutor.Terminate;
  end;

end;

procedure TGDBServerDebuggerInterface.AddToNoBreakList(threadid: integer);
begin

end;

procedure TGDBServerDebuggerInterface.RemoveFromNoBreakList(threadid: integer);
begin

end;

{$ifndef STANDALONEDEBUG}
var
  RPMPageMap: TPageMap;
  RPMPageMapMREW: TMultiReadExclusiveWriteSynchronizer;

procedure _ClearRPMCachePage(baseaddress: qword);
var
  pageinfo: PPageInfo;
begin
  if RPMPageMap<>nil then
  begin
    RPMPageMapMREW.Beginwrite;
    RPMPageMap.Remove(baseaddress shr 12);
    RPMPageMapMREW.Endwrite;
  end;
end;



function GDB_ReadProcessMemory(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: size_t; var lpNumberOfBytesRead: PTRUINT): BOOL; stdcall;
var
  s: integer;
  pageinfo: PPageInfo;
  p: ptruint;
  stopp: ptruint;
  data: pbyte;
  op: string;
  str: string;
begin
  result:=false;
  p:=ptruint(lpBaseAddress);
  stopp:=ptruint(lpBaseAddress)+nsize;
  lpNumberOfBytesRead:=0;

  while p<stopp do
  begin
    RPMPageMapMREW.Beginread;
    pageinfo:=RPMPageMap.GetPageInfo(p shr 12);
    RPMPageMapMREW.Endread;

    if pageinfo=nil then
    begin
      RPMPageMapMREW.Beginwrite;
      pageinfo:=RPMPageMap.GetPageInfo(p shr 12);

      if pageinfo=nil then //still not read
      begin
        getmem(data,4096);
        TGDBServerDebuggerInterface(CurrentDebuggerInterface).readBytes(ptruint(p and qword($fffffffffffff000)), data, 4096, s);
        if s=4096 then
        begin
          pageinfo:=RPMPageMap.Add(p shr 12, data);
        end
        else
        begin
          freemem(data);
          data:=nil;
          pageinfo:=RPMPageMap.Add(p shr 12, nil);
        end;
      end;
      RPMPageMapMREW.Endwrite;
    end;

    if pageinfo=nil then
    begin
      exit(lpNumberOfBytesRead<>0);
    end;

    if pageinfo^.data=nil then
    begin
      exit(lpNumberOfBytesRead<>0);
    end;

    //pageinfo is valid
    if (stopp and qword($fffffffffffff000))=(p and qword($fffffffffffff000)) then
    begin
      s:=stopp-p //from the current p to stopp
    end
    else
    begin
      s:=$1000-(p and $fff); //rest of the page starting from p
    end;

    //try
      data:=pbyte(pageinfo^.data);
      copymemory(lpbuffer, @data[p and $fff], s);
   { except
      on e: EAccessViolation do
      begin
        case e.ExceptionRecord^.ExceptionInformation[0] of
          0: op:='read';
          1: op:='write to';
          8: op:='execute';
          else op:='do something weird with';
        end;

        str:=str+' (tried to '+op+' address '+inttohex(qword(e.ExceptionRecord^.ExceptionInformation[1]),8)+')';

        OutputDebugString(pchar('p='+inttohex(p,8)+' stopp='+inttohex(stopp,8)+' s='+inttohex(s)+':'+str));
      end;
    end; }
    inc(lpbuffer,s);
    inc(lpNumberOfBytesRead, s);

    p:=(p+4096) and qword($fffffffffffff000);
  end;

  result:=lpNumberOfBytesRead>0;
end;

function GDB_WriteProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: size_t; var lpNumberOfBytesWritten: PTRUINT): BOOL; stdcall;
var i, last: ptruint;
begin
  result:=TGDBServerDebuggerInterface(CurrentDebuggerInterface).writeBytes(ptruint(lpBaseAddress), lpBuffer, nsize);

  i:=ptruint(lpBaseAddress);
  last:=i+nsize;

  while i<last do
  begin
    _ClearRPMCachePage(i);
    inc(i,4096);
  end;
end;
{$endif}

procedure TGDBServerDebuggerInterface.ClearRPMCache;
begin
  {$ifndef STANDALONEDEBUG}
  if RPMPageMap<>nil then
  begin
    RPMPageMapMREW.Beginwrite;
    RPMPageMap.free;
    RPMPageMap:=TPageMap.create;
    RPMPageMapMREW.Endwrite;
  end;
  {$endif}
end;


procedure TGDBServerDebuggerInterface.ClearRPMCachePage(baseaddress: qword);
begin
  {$ifndef STANDALONEDEBUG}
  _ClearRPMCachePage(baseaddress);
  {$endif}
end;

procedure TGDBServerDebuggerInterface.OnApiPointerChange(sender: Tobject);
begin
  {$ifndef STANDALONEDEBUG}
  if RPMPageMap=nil then
  begin
    RPMPageMap:=TPageMap.create;
    RPMPageMapMREW:=TMultiReadExclusiveWriteSynchronizer.Create;
  end;


  if GDBReadProcessMemory then ReadProcessMemoryActual:=TReadProcessMemory(@GDB_ReadProcessMemory) else ReadProcessMemoryActual:=TReadProcessMemory(defaultRPM);
  if GDBWriteProcessMemory then WriteProcessMemoryActual:=TWriteProcessMemory(@GDB_WriteProcessMemory) else WriteProcessMemoryActual:=TWriteProcessMemory(defaultWPM);
  {$endif}
end;

destructor TGDBServerDebuggerInterface.destroy;
var i: integer;
begin
  if gdbserverExecutor<>nil then
  begin
    gdbserverExecutor.Terminate;
    gdbserverExecutor.WaitFor;
    gdbserverExecutor.free;
    gdbserverExecutor:=nil;
  end;

  if lock<>nil then
    lock.Free;

 { if stashedPackets<>nil then
    stashedPackets.free;   }

  if breakpoints<>nil then
  begin
    for i:=0 to breakpoints.Count-1 do
      freemem(PGDBBreakpoint(breakpoints[i]));

    breakpoints.Free;
  end;

  if context<>nil then
    freeandnil(context);
end;

function TGDBServerDebuggerInterface.isConnected: boolean;
begin
  result:=socket<>INVALID_SOCKET;
end;

procedure TGDBServerDebuggerInterface.connect(ip: string; _port: word);
var r: string;
  hr: THostResolver;
  sa: sockaddr;
  b: BOOL;
  i,j: integer;

  hostinfo: TStringArray;
  continueOptions: TStringArray;
  starttime: qword;
  connectresult: integer;

begin
  canUseContextCommands:=true; //assume the best

  ObtainLock;
  try
    socket:=cint(INVALID_SOCKET);
    host:=StrToNetAddr(ip);

    if host.s_bytes[4]=0 then
    begin
      hr:=THostResolver.Create(nil);
      try
        if hr.NameLookup(ip) then
          host:=hr.NetHostAddress
        else
          raise exception.create(ip+' in not found');

      finally
        hr.free;
      end;
    end;


    port:=ShortHostToNet(_port);


    starttime:=gettickcount64;



    connectresult:=-1;
    while (connectresult<>0) do
    begin
      socket:=FPSocket(AF_INET, SOCK_STREAM, 0);
      if (socket=cint(INVALID_SOCKET)) then
        raise exception.create('Socket creation failed. Check permissions');



      ZeroMemory(@sa,sizeof(sa));
      sa.sin_family := AF_INET;
      sa.sin_port := port;
      sa.sin_addr.s_addr := host.s_addr;
      B:=TRUE;
      fpsetsockopt(socket, IPPROTO_TCP, TCP_NODELAY, @B, sizeof(B));
      connectresult:=fpconnect(socket, @sa, sizeof(sa));



      if (connectresult=0) or (gettickcount64>starttime+10000) then break;

      {$ifdef windows}
      closehandle(socket);
      {$else}
      FpClose(socket);
      {$endif}
      socket:=INVALID_SOCKET;
      sleep(10);
    end;

    if connectresult=0 then
    begin
      usesAck:=true; //initially it starts as true
      b:=TRUE;
      fpsetsockopt(socket, IPPROTO_TCP, TCP_NODELAY, @B, sizeof(B)); //just to be sure
      sendPacket('QStartNoAckMode'); //currently only tcp connection is supported, so lets get rid of these extra acks
      r:=receivePacket;
      if r='OK' then
        UsesAck:=false;

      sendPacket('qSupported:xmlRegisters=i386,arm;multiprocess+');
      r:=receivePacket;

      maxpacketsize:=512;
      if length(r)>0 then
      begin
        features:=r.Split([';']);


        for i:=0 to length(features)-1 do
        begin
          if features[i].StartsWith('PacketSize=') then
          begin
            r:=copy(features[i],12);
            if r.EndsWith('000') then //sometimes it's in hex... (apple)
            begin
              try
                maxpacketsize:=strtoint('$'+r);
                packetsizeinhex:=true;
              finally
              end;
            end
            else
            begin
              try
                maxpacketsize:=strtoint(r);
              except
              end;
            end;

            break;
          end;
        end;
      end;



      sendPacket('qHostInfo');
      r:=receivePacket;

      //windows:         triple:7838365f36342d70632d77696e646f77732d6d737663;ptrsize:8; watchpoint_exceptions_received:after;endian:little;os_version:6.2.0;os_build:57696e646f7773204e5420362e322e30;os_kernel:57696e646f7773204e5420362e322e30;hostname:534543524554;
      //macos(roesetta): cputype:16777228;cpusubtype:2;addressing_bits:47;ostype:macosx;watchpoint_exceptions_received:before;vendor:apple;os_version:13.5.0;maccatalyst_version:16.6;endian:little;ptrsize:8;vm-page-size:16384;

      pointersize:=8;
      pagesize:=4096;
      if length(r)>0 then
      begin
        hostinfo:=r.split([';']);
        for i:=0 to length(hostinfo)-1 do
        begin
          if hostinfo[i].StartsWith('ptrsize:') then
          begin
            r:=copy(hostinfo[i],9);
            try
              pointersize:=strtoint(r);
            except
            end;
          end;

          if hostinfo[i].StartsWith('watchpoint_exceptions_received:') then
          begin
            r:=copy(hostinfo[i],32);
            fwatchpointsTriggerAfterExecution:=r<>'before';
          end;

          if hostinfo[i].StartsWith('endian:') then
          begin
            r:=copy(hostinfo[i],8);
            bigEndian:=r='big';
          end;

          if hostinfo[i].StartsWith('vm-page-size:') then
          begin
            r:=copy(hostinfo[i], 14);
            pagesize:=strtoint(r);
          end;
        end;
      end;

      sendPacket('vCont?');
      r:=receivePacket;
      if r.StartsWith('vCont') then canvCont:=true;

      if canvCont then
      begin
        continueOptions:=r.split(';');

        for i:=1 to length(continueOptions)-1 do
        begin
          case continueOptions[i][1] of
            'c': canContinue:=true;
            'C': canContinueWithSig:=true;
            's': canStep:=true;
            'S': canStepWithSig:=true;
            't': canStop:=true;
            'r':  canStepRange:=true;
          end;
        end;
      end;

      sendPacket('QNonStop:0');
      r:=receivepacket;
      if r='OK' then supportsnonstop:=true;

      sendPacket('QSetDetachOnError:1');
      r:=receivePacket;
      if r<>'OK' then
        OutputDebugString('QSetDetachOnError not supported');

      sendPacket('QThreadSuffixSupported');
      r:=receivePacket;
      if r='OK' then
        ThreadSuffixSupported:=true;

      sendPacket('QThreadEvents:1');
      r:=receivePacket;
      if r<>'OK' then
      begin


        OutputDebugString('QThreadEvents not supported');
        sendPacket('QListThreadsInStopReply:1');
        r:=receivePacket;
        if r='OK' then
          ThreadListWhenStopped:=true
        else
        begin
          sendPacket('QListThreadsInStopReply');
          r:=receivePacket;
          if r='OK' then
            ThreadListWhenStopped:=true
        end;


      end
      else
        ThreadEvents:=true;




    end
    else raise exception.create('Failure to connect');

  finally
    ReleaseLock;
  end;
end;



constructor TGDBServerDebuggerInterface.connectToExistingServer(ip: string; _port: word);
begin
  inherited create;
  oldcontexts:=TMap.Create(itu4,sizeof(pointer));
  fDebuggerCapabilities:=fDebuggerCapabilities-[dbcCanUseInt1BasedBreakpoints];
  fDebuggerCapabilities:=fDebuggerCapabilities+[dbcDealsWithBreakpointsInternally,dbcSoftwareBreakpoint];


  lock:=TCriticalSection.Create;
//  stashedPackets:=tstringlist.create;
  breakpoints:=tlist.create;

  connect(ip,_port);
end;



constructor TGDBServerDebuggerInterface.createAndConnect(_gdbserverlaunchcommand: string; ip: string; _port: word);
begin
  inherited create;
  oldcontexts:=TMap.Create(itu4,sizeof(pointer));
  fDebuggerCapabilities:=fDebuggerCapabilities-[dbcCanUseInt1BasedBreakpoints];
  fDebuggerCapabilities:=fDebuggerCapabilities+[dbcDealsWithBreakpointsInternally,dbcSoftwareBreakpoint];

  breakpoints:=tlist.create;
  lock:=TCriticalSection.Create;

  gdbserverlaunchcommand:=_gdbserverlaunchcommand;
  gdbserverExecutor:=TGdbserverExecutor.Create(true);
  gdbserverExecutor.command:=gdbserverlaunchcommand;
  gdbserverExecutor.Start;
  //if ip is empty then use a local socket
  sleep(10);
  try
    connect(ip, _port);
  except
    if gdbserverExecutor.Finished then
    begin
      raise exception.Create(gdbserverExecutor.errormessage);
    end
    else
      raise;
  end;

end;


end.

