unit debugger2;

interface

uses windows,sysutils,dialogs,classes,debugger,disassembler,newkernelhandler,foundcodeunit,
     tlhelp32,cefuncproc;

type Tdebugevent =record
  EAX,EBX,ECX,EDX,ESI,EDI,EBP,ESP,EIP:DWORD;
end;

type TDebugEvents=class(TThread)
  private
    addressfound: dword;
    currentDebugEvent: TDebugEvent;
    procedure foundone;
  public
    debugregs: _context;
    threadlist: array of dword;

    breakpoints: array [0..3] of dword;
    breakpointchanges: array [0..3] of tregistermodificationBP;
    function nrofbreakpoints:integer;
    procedure setbreakpoints;
    procedure execute; override;
    constructor create(suspended:boolean);
    destructor destroy;  override;
end;

var DebuggerThread2: TDebugEvents;

implementation

uses frmProcessWatcherUnit,memorybrowserformunit;

destructor TDebugEvents.destroy;
var i: integer;
begin
  crdebugging.Enter;
  for i:=0 to 3 do
    breakpoints[i]:=0;
  crdebugging.Leave;

  setbreakpoints;
  memorybrowser.updatebplist;

  Terminate;
  waitfor;
  inherited destroy;
end;

constructor TDebugevents.create(suspended:boolean);
var ths: thandle;
    tE: threadentry32;
    i,j: integer;
    found: boolean;
    temp: thandle;
begin
  inherited create(true);
  debugregs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;

  //try to find this process in the processwatch window.
  found:=false;
  for i:=0 to length(frmprocesswatcher.processes)-1 do
  begin
    if frmprocesswatcher.processes[i].processid=processid then
    begin
      //open the threads
      for j:=0 to length(frmprocesswatcher.processes[i].threadlist)-1 do
      begin
        temp:=Openthread(STANDARD_RIGHTS_REQUIRED or windows.synchronize or $3ff,true,frmprocesswatcher.processes[i].threadlist[j].threadid);
        if temp<>0 then
        begin
          setlength(threadlist,length(threadlist)+1);
          threadlist[length(threadlist)-1]:=temp;
        end;
      end;

      found:=true;
      break;

    end;
  end;

  if not found then
  begin
    //if it wasn't found try to add it (and tell the user it's best to start the process after ce has started)
    ths:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,processid);
    try
      if ths<>0 then
      begin
        te.dwSize:=sizeof(te);
        if Thread32First(ths,te) then
        begin
          repeat
            if te.th32OwnerProcessID=processid then
            begin
              setlength(threadlist,length(threadlist)+1);
              threadlist[length(threadlist)-1]:=Openthread(STANDARD_RIGHTS_REQUIRED or windows.synchronize or $3ff,true,te.th32ThreadID);
            end;
          until not thread32Next(ths,te);
        end;
      end;
    finally
      closehandle(ths);
    end;
  end;

  if not suspended then resume;
end;

procedure TDebugevents.setbreakpoints;
var i: integer;
begin
  debugregs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
  debugregs.Dr7:=reg0set or reg1set or reg2set or reg3set;

  debugregs.Dr0:=breakpoints[0];
  debugregs.dr1:=breakpoints[1];
  debugregs.dr2:=breakpoints[2];
  debugregs.Dr3:=breakpoints[3];

  for i:=0 to length(threadlist)-1 do
  begin
    suspendthread(threadlist[i]);
    setthreadcontext(threadlist[i],debugregs);
    resumethread(threadlist[i]);
  end;
end;

function TDebugEvents.nrofbreakpoints: integer;
var i,nr: integer;
begin
  //count the filled in debugregs
  nr:=0;
  for i:=0 to 3 do
    if breakpoints[i]<>0 then inc(nr);

  result:=nr;
end;


procedure TDebugEvents.foundone;
var desc,opcode: string;
    address: dword;
begin
  with foundcodedialog do
  begin
    address:=addressfound;
    opcode:=disassemble(address,desc);

    setlength(coderecords,length(coderecords)+1);
    coderecords[length(coderecords)-1].address:=addressfound;
    coderecords[length(coderecords)-1].size:=address-addressfound;
    coderecords[length(coderecords)-1].opcode:=opcode;
    coderecords[length(coderecords)-1].desciption:=desc;

    coderecords[length(coderecords)-1].eax:=currentdebugevent.EAX;
    coderecords[length(coderecords)-1].ebx:=currentdebugevent.EBX;
    coderecords[length(coderecords)-1].ecx:=currentdebugevent.ECX;
    coderecords[length(coderecords)-1].edx:=currentdebugevent.EDX;
    coderecords[length(coderecords)-1].esi:=currentdebugevent.Esi;
    coderecords[length(coderecords)-1].edi:=currentdebugevent.Edi;
    coderecords[length(coderecords)-1].ebp:=currentdebugevent.Ebp;
    coderecords[length(coderecords)-1].esp:=currentdebugevent.Esp;
    coderecords[length(coderecords)-1].eip:=currentdebugevent.Eip;
    Foundcodelist.Items.Add(opcode);
  end;
end;

procedure TDebugEvents.execute;
var DebugEvent:array [0..49] of TDebugEvent;
    i,j,events: integer;
    offset: dword;
    opcode,desc: string;
    notinlist: boolean;
begin
  try
    while not terminated do
    begin
      if foundcodedialog=nil then
      begin
        sleep(1000);
        continue;
      end;

      crdebugging.Enter;
      try
        //poll the debugevents
        events:=RetrieveDebugData(@DebugEvent);
        for i:=0 to events-1 do
        begin
          currentdebugevent:=DebugEvent[i];
          addressfound:=debugevent[i].EIP;
          offset:=addressfound;
          opcode:=disassemble(offset,desc);

          if pos('REP',opcode)=0 then
            addressfound:=previousopcode(addressfound)
          else
            if debugevent[i].Ecx=0 then addressfound:=previousopcode(addressfound);

          //check if the address is in the list
          notinlist:=true;
          try
            for j:=0 to length(foundcodedialog.coderecords)-1 do
              if foundcodedialog.coderecords[j].address=addressfound then //if it is in the list then set notinlist to false and go out of the loop
              begin
                notinlist:=false;
                break;
              end;
          except
            //list got shortened or invalid (or whatever weird bug)
          end;

          if notinlist then synchronize(foundone); //add this memory address to the foundcode window.
        end;


      finally
        crdebugging.Leave;
      end;
      sleep(250);
      //check for new threads and set their debug registers
    end;
  except

  end;

  crdebugging.Enter;
  //disable the debugregs
  zeromemory(@debugregs,sizeof(debugregs));
  debugregs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
  debugregs.Dr7:=reg0set or reg1set or reg2set or reg3set;
  for i:=0 to length(threadlist)-1 do
  begin
    suspendthread(threadlist[i]);
    SetThreadContext(threadlist[i],Debugregs);
    resumethread(threadlist[i]);
  end;

  //tell the kerneldriver to whipe out the debuggeerdprocesslist
  Stopdebugging;
  
  crdebugging.Leave;
end;

end.
