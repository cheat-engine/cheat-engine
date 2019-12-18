unit frmMemoryAllocHandlerUnit;

{$MODE Delphi}

{
ZwVirtualAllocEx
}
interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, symbolhandler, symbolhandlerstructs, CEFuncProc,NewKernelHandler,
  autoassembler, ExtCtrls, ComCtrls, stacktrace2, math, Menus, syncobjs, Contnrs, circularbuffer,
  LResources, commonTypeDefs;


const
  HOOKEVENT_ALLOC=0;
  HOOKEVENT_FREE=1;
  HOOKEVENT_HEAPALLOC=2;
  HOOKEVENT_HEAPFREE=3;
  HOOKEVENT_HEAPDESTROY=4;

type
  TAllocData=record
    handle: qword;
    baseaddress: qword;
    allocationType: dword;
    protect: dword;
    size: dword;
    esp: qword;
  end;

  TFreeData=record
    handle: qword;
    baseaddress: qword;
    size: dword;
    FreeType: dword;
  end;

  THeapAllocData=record
    HeapHandle: qword;
    Flags: DWORD;
    Size: dword;
    esp:qword;
    address: qword;
  end;

  THeapFreeData=record
    HeapHandle: qword;
    Flags: dword;
    HeapBase: qword;
  end;

  THeapDestroyData=record
    HeapHandle: qword;
  end;

  THookEvent = record
    eventtype: integer;
    case integer of
      HOOKEVENT_ALLOC: (AllocEvent: TAllocData);
      HOOKEVENT_FREE: (FreeEvent: TFreeData);
      HOOKEVENT_HEAPALLOC: (HeapAllocEvent: THeapAllocData);
      HOOKEVENT_HEAPFREE: (HeapFreeEvent: THeapFreeData);
      HOOKEVENT_HEAPDESTROY: (HeapDestroyEvent: THeapDestroyData);
  end;

type TmemoryAllocevent=class
  private
  public
    HookEvent: THookEvent;

    BaseAddress: ptrUint;
    //stack: array [0..4095] of byte;
    //stacksize: dword;

    procedure assign(o: TMemoryAllocEvent);
end;

type
  PMemRecTable=^TMemRecTable;
  PMemRecTableArray=^TMemRecTableArray;
  TMemRecTable=record
    case integer of
      1: (memallocevent: TmemoryAllocevent); //if this is the last level (7) this is a memallocevent pointer
      2: (MemrecArray: PMemRecTableArray);   //else it's a PMemrectablearray
  end;
  TMemRecTableArray=array [0..15] of TMemRecTable;

type TDisplayThread=class(tthread)
  private
    maxlevel: integer;
    procedure AddAddress(addresslist: PMemrecTableArray; memallocevent: TmemoryAllocevent );
    procedure removeaddress(addresslist: PMemrecTableArray; memallocevent: TmemoryAllocevent );
  public
    csObjectList: TCriticalSection;
    ObjectQueue: TCircularObjectBuffer;
    ListContainsEntries: TEvent;

    errorcount: dword; //debug variable
    heapcount: integer;
    //-----
    procedure AddObject_Free(o: TMemoryAllocEvent);
    procedure AddObject_Alloc(o: TMemoryAllocEvent);
    procedure AddObject_HeapAlloc(o: TMemoryAllocEvent);
    procedure AddObject_HeapFree(o: TMemoryAllocEvent);
    procedure AddObject_HeapDestroy(o: TMemoryAllocEvent);
    procedure AddObject_HeapDestroy2(a: PMemrectableArray; level: integer; heaphandle: qword);
    procedure addobject(objecttoadd: TMemoryAllocEvent);
    procedure execute; override;
    constructor create(suspended: boolean);
    destructor destroy; override;
end;
  
type TAllocWatcher=class(TThread)
  private
    HasSetupDataEvent: THandle;
    CEHasHandledItEvent: THandle;
    HookEventDataAddress: ptrUint;

    errorcount: dword; //debug variable

    HookEvent: THookEvent;
    maxlevel: integer;
    procedure allocevent;
    procedure freeEvent;
    procedure heapAllocEvent;
    procedure heapFreeEvent;
    procedure headpDestroyEvent;
   //  procedure addObject;
    procedure addObjectToList(o: TmemoryAllocevent);
  public
    procedure execute; override;
    constructor create(suspended: boolean; HasSetupDataEvent: THandle; CEHasHandledItEvent: THandle; HookEventDataAddress: ptrUint);
end;





type

  { TfrmMemoryAllocHandler }

  TfrmMemoryAllocHandler = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblBaseAddress: TLabel;
    lblFlags: TLabel;
    lblHeapHandle: TLabel;
    lblSize: TLabel;
    Panel2: TPanel;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    lblErr: TLabel;
    btnReload: TButton;
    cbHookAllocs: TCheckBox;
    procedure cbHookAllocsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
    procedure cbHookAllocsClick(Sender: TObject);
  private
    { Private declarations }
    HasSetupDataEvent: THandle;
    CEHasHandledItEvent: THandle;
    CEInitializationFinished: THandle;
    watcher: TAllocWatcher;
    HookEventDataAddress: ptrUint;

    hookscript: tstringlist;
    hookallocarray: TCEAllocArray;
    exceptionlist: TCEExceptionListArray;

    maxlevel: integer;
    pointermask: integer;
  public
    { Public declarations }

//level0 (0000000-10000000-20000000-30000000-40000000-50000000-60000000-70000000-80000000-90000000-a0000000-b0000000-c0000000-d0000000-e0000000-f0000000
//level1.0 (01000000-02000000-03000000-04000000-....
//level1.1 (11000000-12000000-13000000-14000000-....
//max level (7/15) is the actual record
    memrecCS: TCriticalSection;
    HeapBaselevel: TMemRecTableArray;
    AllocBaseLevel: TMemRecTableArray;

    hookedprocessid: dword;
    displaythread: TDisplayThread;

    function FindAddress(addresslist: PMemrecTableArray; address: ptrUint): TMemoryAllocEvent;
    function WaitForInitializationToFinish: boolean;
  end;

var
  frmMemoryAllocHandler: TfrmMemoryAllocHandler;

implementation


uses frmautoinjectunit, MemoryBrowserFormUnit, processhandlerunit, Globals;

resourcestring
  rsQueuedMemoryEventsWaiting = 'Queued memory events waiting: %s';
  rsHeapcount = 'Heapcount=%s';
  rsFailureToInitialize = 'Failure to initialize';
  rsFailureToHook = 'Failure to hook';
  rsEvent1Failure = 'Event1 failure:';
  rsEvent2Failure = 'Event2 failure:';
  rsEvent3Failure = 'Event3 failure:';

procedure TDisplayThread.removeaddress(addresslist: PMemrecTableArray; memallocevent: TmemoryAllocevent );
var
  level: integer;
  entrynr: integer;
  currentarray: PMemrectablearray;
  i: integer;
  temp: TmemoryAllocevent;
begin
  level:=0;
  currentarray:=addresslist;
  while level<maxlevel do
  begin
    entrynr:=memallocevent.baseaddress shr ((maxlevel-level)*4) and $f;
    if currentarray[entrynr].MemrecArray=nil then
      exit; //not found

    currentarray:=currentarray[entrynr].MemrecArray;

    inc(level);
  end;

  entrynr:=memallocevent.baseaddress shr ((maxlevel-level)*4) and $f;
  if currentarray[entrynr].memallocevent<>nil then
  begin
    //remove it
    temp:=currentarray[entrynr].memallocevent;
    currentarray[entrynr].memallocevent:=nil;

    if addresslist=@frmMemoryAllocHandler.HeapBaselevel then
      dec(heapcount);
     
    temp.Free;
  end;
end;

procedure TDisplayThread.AddAddress(addresslist: PMemrecTableArray; memallocevent: TmemoryAllocevent );
var
  level: integer;
  entrynr: integer;
  temp,currentarray: PMemrectablearray;
  temp2: TmemoryAllocevent;
  i: integer;
begin
  level:=0;
  currentarray:=addresslist;
  while level<maxlevel do
  begin

    entrynr:=memallocevent.baseaddress shr ((maxlevel-level)*4) and $f;
    if currentarray[entrynr].MemrecArray=nil then //allocate the array
    begin
      GetMem(temp, sizeof(TMemRecTableArray));
      ZeroMemory(temp, sizeof(TMemRecTableArray));
      currentarray[entrynr].MemrecArray:=temp;
    end;

    currentarray:=currentarray[entrynr].MemrecArray;

    inc(level);
  end;

  entrynr:=memallocevent.baseaddress shr ((maxlevel-level)*4) and $f;
  if currentarray[entrynr].memallocevent<>nil then
  begin
    temp2:=currentarray[entrynr].memallocevent;
    currentarray[entrynr].memallocevent:=nil;
    temp2.free;
  end;

  currentarray[entrynr].memallocevent:=memallocevent;

  if addresslist=@frmMemoryAllocHandler.HeapBaselevel then
     inc(heapcount);

end;




procedure TDisplayThread.AddObject_HeapAlloc(o: TMemoryAllocEvent);
begin
  addaddress(@frmMemoryAllocHandler.HeapBaseLevel, o);
end;

procedure TDisplayThread.AddObject_HeapFree(o: TMemoryAllocEvent);
var
  first,last: integer;
  j,k: integer;
  x: integer;
  temp: dword;
begin
  removeaddress(@frmMemoryAllocHandler.HeapBaseLevel, o);
  o.Free;
end;

procedure TDisplayThread.AddObject_HeapDestroy2(a: PMemrectableArray; level: integer; heaphandle: qword);
var i: integer;
    temp: TMemoryAllocEvent;
begin
  if level=maxlevel then
  begin
    for i:=0 to 15 do
      if (a[i].memallocevent<>nil) and (a[i].memallocevent.HookEvent.HeapAllocEvent.HeapHandle=heaphandle) then
      begin
        //delete this one
        temp:=a[i].memallocevent;
        a[i].memallocevent:=nil;
        dec(heapcount);
        temp.free;
      end;
  end
  else
  for i:=0 to 15 do
  begin
    if a[i].MemrecArray<>nil then
      AddObject_HeapDestroy2(a[i].MemrecArray,level+1,heaphandle);
  end;

end;

procedure TDisplayThread.AddObject_HeapDestroy(o: TMemoryAllocEvent);
var
  i: integer;
  level: integer;
  destroyedHandle: pointer;
begin
  AddObject_HeapDestroy2(@frmMemoryAllocHandler.HeapBaseLevel,0,o.HookEvent.HeapDestroyEvent.HeapHandle);
end;

procedure TDisplayThread.AddObject_Alloc(o: TMemoryAllocEvent);
var
  s: string;
  first,last: integer;
  x,i,j,k,l: integer;
  temp: dword;
  insert: boolean;
begin
  addaddress(@frmMemoryAllocHandler.AllocBaseLevel, o);
end;

procedure TDisplayThread.AddObject_Free(o: TMemoryAllocEvent);
var
  first,last: integer;
  x,j,k: integer;
  temp: dword;
begin
  removeaddress(@frmMemoryAllocHandler.AllocBaseLevel, o);
  o.Free;
end;

procedure TDisplayThread.addobject(objecttoadd: TMemoryAllocEvent);
//Handles the current object with the gui (add, delete, etc...)
begin
  try
    case objecttoadd.HookEvent.eventtype of
      HOOKEVENT_FREE:       AddObject_Free(objecttoadd);
      HOOKEVENT_ALLOC:      AddObject_Alloc(objecttoadd);
      HOOKEVENT_HEAPALLOC:  AddObject_HeapAlloc(objecttoadd);
      HOOKEVENT_HEAPFREE:   AddObject_HeapFree(objecttoadd);
      HOOKEVENT_HEAPDESTROY:AddObject_HeapDestroy(objecttoadd);
    end;
  except
    inc(errorcount);
    OutputDebugString('Exception in TDisplayThread.addobject2. objecttoadd='+inttohex(ptrUint(objecttoadd),8));
  end;
end;

procedure TDisplayThread.execute;
var o: TMemoryAllocEvent;
begin
  while not terminated do
  begin
    try
      o:=nil;
      csObjectList.Enter;
      try
        if ObjectQueue.Count>0 then
          o:=TMemoryAllocEvent(ObjectQueue.Read);
      finally
        csObjectList.Leave;
      end;

      if o<>nil then
      begin
        frmMemoryAllocHandler.memrecCS.enter;
        try
          addobject(o)
        finally
          frmMemoryAllocHandler.memrecCS.leave;
        end;
      end
      else
        ListContainsEntries.WaitFor(1000); //nothing to add to the list, wait a second and check again

    except
      inc(errorcount);
      OutputDebugString('Error in TDisplayThread.execute');
    end; 
  end;
end;

constructor TDisplayThread.create(suspended: boolean);
begin
  csObjectList:=TCriticalSection.Create;
  ObjectQueue:=TCircularObjectBuffer.Create(5000000, 1000); //arround 20MB of of pointe storage   

  ListContainsEntries:=TEvent.Create(nil,false,false,'');

  if processhandler.is64Bit then
    maxlevel:=15
  else
    maxlevel:=7;

  inherited create(suspended);
end;

destructor TDisplayThread.destroy;
begin
  if csObjectList<>nil then
    freeandnil(csObjectList);

  if ListContainsEntries<>nil then
    freeandnil(ListContainsEntries);

  if ObjectQueue<>nil then
    freeandnil(ObjectQueue);
    
  inherited destroy;
end;

procedure TAllocWatcher.addObjectToList(o: TmemoryAllocevent);
//Will add this object to a list that will later on be read out by the display thread
begin
  frmMemoryAllocHandler.displaythread.csObjectList.Enter;
  try
    frmMemoryAllocHandler.displaythread.ObjectQueue.Write(o)
  finally
    frmMemoryAllocHandler.displaythread.csObjectList.Leave;
  end;
  frmMemoryAllocHandler.displaythread.ListContainsEntries.SetEvent;
end;

procedure TmemoryAllocevent.assign(o: TMemoryAllocEvent);
var i: integer;
begin
  self.HookEvent:=o.HookEvent;

 { for i:=0 to o.stacksize-1 do
    self.stack[i]:=o.stack[i];

  self.stacksize:=o.stacksize; }
end;


            {
procedure TAllocWatcher.addObject;

end;  }

procedure TAllocWatcher.allocEvent;
var s: integer;
    o: TMemoryAllocEvent;
begin
  {$IFDEF windows}
  o:=TmemoryAllocevent.create;
  o.HookEvent:=hookevent;
  o.BaseAddress:=hookevent.AllocEvent.baseaddress;

   {

  if not ReadProcessMemory(processhandle,pointer(o.HookEvent.AllocEvent.esp),@o.stack[0],4096,o.stacksize) then
  begin
    //probably couldn't read because of pageerror
    //read the remaining of the page
    s:=4096-(o.HookEvent.AllocEvent.esp mod 4096);
    ReadProcessMemory(processhandle,pointer(o.HookEvent.AllocEvent.esp),@o.stack[0],s,o.stacksize);
  end; }

  setevent(CEHasHandledItEvent); //tell the hooked app to continue
  addObjectToList(o);
  {$ENDIF}
end;

procedure TAllocWatcher.freeEvent;
var o: TMemoryAllocEvent;
begin
  {$IFDEF windows}
  o:=TmemoryAllocevent.create;
  o.HookEvent:=hookevent;
  o.BaseAddress:=hookevent.FreeEvent.baseaddress;

  setevent(CEHasHandledItEvent);
  addObjectToList(o);
  {$ENDIF}
end;

procedure TAllocWatcher.heapAllocEvent;
var o: TMemoryAllocEvent;
var s: integer;
begin
  {$IFDEF windows}
  o:=TmemoryAllocevent.create;
  o.HookEvent:=hookevent;
  o.BaseAddress:=ptrUint(hookevent.HeapAllocEvent.address);
{
  if not ReadProcessMemory(processhandle,pointer(o.HookEvent.HeapAllocEvent.esp),@o.stack[0],4096,o.stacksize) then
  begin
    //probably couldn't read because of pageerror
    //read the remaining of the page
    s:=4096-(o.HookEvent.AllocEvent.esp mod 4096);
    ReadProcessMemory(processhandle,pointer(o.HookEvent.HeapAllocEvent.esp),@o.stack[0],s,o.stacksize);
  end;     }

  setevent(CEHasHandledItEvent);
  addObjectToList(o);
  {$ENDIF}
end;

procedure TAllocWatcher.heapFreeEvent;
var o: TMemoryAllocEvent;
begin
  {$IFDEF windows}
  o:=TmemoryAllocevent.create;
  o.HookEvent:=hookevent;
  o.BaseAddress:=ptrUint(hookevent.HeapFreeEvent.HeapBase);
  setevent(CEHasHandledItEvent);
  addObjectToList(o);
  {$ENDIF}
end;

procedure TAllocWatcher.headpDestroyEvent;
var o: TMemoryAllocEvent;
begin
  {$IFDEF windows}
  o:=TmemoryAllocevent.create;
  o.HookEvent:=hookevent;
  o.BaseAddress:=$0;

  setevent(CEHasHandledItEvent);
  addObjectToList(o);
  {$ENDIF}
end;

procedure TAllocWatcher.execute;
var x: PtrUInt;
begin
  {$IFDEF windows}
  while not terminated do
  begin
    try
      if waitforsingleobject(HasSetupDataEvent,500)=WAIT_OBJECT_0 then
      begin
        if readprocessmemory(processhandle, pointer(HookEventDataAddress),@HookEvent,sizeof(HookEvent),x) then
        begin
          case HookEvent.eventtype of
            HOOKEVENT_ALLOC : allocEvent;
            HOOKEVENT_FREE  : freeEvent;
            HOOKEVENT_HEAPALLOC : heapAllocEvent;
            HOOKEVENT_HEAPFREE  : heapFreeEvent;
            HOOKEVENT_HEAPDESTROY : headpDestroyEvent;
          end;
        end;

      end;
    except
      inc(errorcount);
      OutputDebugString('Error in TAllocWatcher.execute');
    end;
  end;
  {$ENDIF}
end;

constructor TAllocWatcher.create(suspended: boolean; HasSetupDataEvent: THandle; CEHasHandledItEvent: THandle; HookEventDataAddress: ptrUint);
begin
  self.HasSetupDataEvent:=HasSetupDataEvent;
  self.CeHasHandledItEvent:=CeHasHandledItEvent;

  self.HookEventDataAddress:=HookEventDataAddress;

  if processhandler.is64Bit then
    maxlevel:=15
  else
    maxlevel:=7;

  inherited create(suspended);
end;



procedure TfrmMemoryAllocHandler.FormCreate(Sender: TObject);
var
  injectionscript: TStringlist;
  x,y,z: THandle;
  mi: tmoduleinfo;
  mname: string;
begin
  {$ifdef windows}

  if processhandler.is64Bit then
  begin
    maxlevel:=15;
    pointermask:=7; //AND the value/address with this value. If the result=0 it's aligned
  end
  else
  begin
    maxlevel:=7;
    pointermask:=3;
  end;


  memrecCS:=TCriticalSection.Create;
  displaythread:=TDisplayThread.create(false);
  injectionscript:=tstringlist.Create;
  try
    //inject allochook.dll

    if processhandler.is64bit then
      mname:='allochook-x86_64.dll'
    else
      mname:='allochook-i386.dll';


    if not symhandler.getmodulebyname(mname,mi) then
    begin
      injectdll(CheatEngineDir+mname);
      symhandler.reinitialize(True);
    end;
      



    HasSetupDataEvent:=CreateEvent(nil, false, false, nil);
    CEHasHandledItEvent:=CreateEvent(nil,false,false,nil);
    CEInitializationFinished:=CreateEvent(nil,true,false,nil);

    x:=0;
    if not DuplicateHandle(GetCurrentProcess, HasSetupDataEvent, processhandle, @x, 0, false, DUPLICATE_SAME_ACCESS	) then
      raise exception.Create(rsEvent1Failure+inttostr(getlasterror));

    y:=0;
    if not DuplicateHandle(GetCurrentProcess, CEHasHandledItEvent, processhandle, @y, 0, false, DUPLICATE_SAME_ACCESS	) then
      raise exception.Create(rsEvent2Failure+inttostr(getlasterror));

    z:=0;
    if not DuplicateHandle(GetCurrentProcess, CEInitializationFinished, processhandle, @z, 0, false, DUPLICATE_SAME_ACCESS	) then
      raise exception.Create(rsEvent3Failure+inttostr(getlasterror));

    //set event handles
    injectionscript.Add('HasSetupDataEvent:');
    injectionscript.Add('DQ '+inttohex(x,8));
    injectionscript.Add('CEHasHandledItEvent:');
    injectionscript.Add('DQ '+inttohex(y,8));
    injectionscript.Add('CEInitializationFinished:');
    injectionscript.Add('DQ '+inttohex(z,8));


    if not autoassemble(injectionscript,false) then raise exception.Create(rsFailureToHook);

    HookEventDataAddress:=symhandler.getAddressFromName('HookEventData');

    //everything configured successful, start thread that watches for HasSetupDataEvent events
    watcher:=TAllocWatcher.create(false, HasSetupDataEvent, CEHasHandledItEvent, HookEventDataAddress);

    hookedprocessid:=processid;

    injectionscript.Clear;
    injectionscript.Add('CreateThread(CeInitializeAllocHook)');
    if not autoassemble(injectionscript,false) then raise exception.Create(rsFailureToInitialize);

    
  finally
    injectionscript.Free;
  end;

  {$endif}
end;

procedure TfrmMemoryAllocHandler.cbHookAllocsChange(Sender: TObject);
begin

end;


procedure TfrmMemoryAllocHandler.FormDestroy(Sender: TObject);
begin
  if watcher<>nil then
    freeandnil(watcher);

  if HasSetupDataEvent>0 then
    closehandle(HasSetupDataEvent);

  if CEHasHandledItEvent>0 then
    closehandle(CEHasHandledItEvent);

  //cleanup memory allocs
end;

procedure TfrmMemoryAllocHandler.FormShow(Sender: TObject);
begin
  if processhandler.is64Bit then
    edit1.ClientWidth:=canvas.TextWidth(' DDDDDDDD ')
  else
    edit1.ClientWidth:=canvas.TextWidth(' DDDDDDDDDDDDD ')
end;

procedure TfrmMemoryAllocHandler.GroupBox1Click(Sender: TObject);
begin

end;

procedure TfrmMemoryAllocHandler.Timer1Timer(Sender: TObject);
begin
  StatusBar1.Panels[0].Text:=Format(rsQueuedMemoryEventsWaiting, [inttostr(displaythread.ObjectQueue.Count)]);
  Statusbar1.Panels[1].Text:=Format(rsHeapcount, [inttostr(displaythread.heapcount)]);
end;

procedure TfrmMemoryAllocHandler.Button1Click(Sender: TObject);
var result:  TMemoryAllocEvent;
begin
  result:=FindAddress(@HeapBaselevel, strToInt('$'+edit1.Text));

  if result<>nil then
  begin
    lblHeapHandle.caption:=inttohex(ptrUint(result.HookEvent.HeapAllocEvent.HeapHandle),8);
    lblflags.Caption:=heapflagstostring(result.HookEvent.HeapAllocEvent.Flags);
    lblbaseaddress.caption:=inttohex(ptrUint(result.HookEvent.HeapAllocEvent.address),8);
    lblsize.Caption:=inttostr(result.HookEvent.HeapAllocEvent.Size);
    if lblErr.Visible then
      lblErr.Visible:=false;
  end else
  begin
    lblHeapHandle.caption:='-';
    lblflags.Caption:='-';


    if FindAddress(@AllocBaseLevel,strtoint('$'+edit1.Text))<>nil then
    begin
      lblbaseaddress.caption:=inttohex(ptrUint(result.HookEvent.HeapAllocEvent.address),8);
      lblsize.Caption:=inttostr(result.HookEvent.HeapAllocEvent.Size);
      lblErr.Visible:=true;
    end
    else
    begin
      lblbaseaddress.caption:='-';
      lblsize.Caption:='-';
      if lblErr.Visible then
        lblErr.Visible:=false;
    end;
  end;
end;

type TMemrectablearraylist = array [0..15] of record
 arr:PMemrectablearray;
 entrynr: integer;
 end;

function findMaxOfPath(a: PMemrectablearray; level: integer; maxlevel: integer):TMemoryAllocEvent;
var
  i: integer;
begin
  result:=nil;
  if level=maxlevel then
  begin
    for i:=15 downto 0 do
    begin
      if a[i].memallocevent<>nil then
      begin
        result:=a[i].memallocevent;
        exit;
      end;
    end;
  end
  else
  begin
    for i:=15 downto 0 do
    begin
      if a[i].MemrecArray<>nil then
      begin
        result:=findMaxOfPath(a[i].MemrecArray,level+1, maxlevel);
        if result<>nil then exit;
      end;
    end;
  end;
end;

function findprevious(lvl: TMemrectablearraylist; level: integer; maxlevel: integer):TMemoryAllocEvent;
var
  i: integer;
  currentarray: PMemrectablearray;
begin
  result:=nil;
  if level=maxlevel then
  begin
    currentarray:=lvl[level].arr;
    for i:=lvl[level].entrynr-1 downto 0 do
    begin
      if currentarray[i].memallocevent<>nil then
      begin
        result:=currentarray[i].memallocevent;
        exit;
      end;
    end;

  end
  else
  begin
    currentarray:=lvl[level].arr;
    for i:=lvl[level].entrynr-1 downto 0 do
    begin
      if currentarray[i].MemrecArray<>nil then
      begin
        result:=findMaxOfPath(currentarray[i].MemrecArray,level+1, maxlevel);
        if result<>nil then exit;
      end;
    end;

  end;
  //still here
  if level>0 then
  begin
    lvl[level].entrynr:=$f;
    result:=findprevious(lvl,level-1, maxlevel);
  end;
end;

function TfrmMemoryAllocHandler.WaitForInitializationToFinish: boolean;
begin
  {$IFDEF windows}
    result:=WaitForSingleObject(CEInitializationFinished,5000)=WAIT_OBJECT_0;
  {$ENDIF}
end;

function TfrmMemoryAllocHandler.FindAddress(addresslist: PMemrecTableArray; address: ptrUint): TMemoryAllocEvent;
  //only call this when displaythread is suspended
var
  level: integer;
  entrynr: integer;
  currentarray: PMemrectablearray;

  lvl: TMemrectablearraylist;
  i: integer;
begin
  zeromemory(@lvl,sizeof(TMemrectablearraylist));

  result:=nil;

  //memrecCS.Enter;
 // try
    level:=0;
    currentarray:=addresslist;

    while level<maxlevel do
    begin
      entrynr:=address shr ((7-level)*4) and $f;
      lvl[level].arr:=currentarray;
      lvl[level].entrynr:=entrynr;

      if currentarray[entrynr].MemrecArray=nil then
      begin


        //not a direct match (this will happen almost every time)
        //try to find a previous entry
        result:=findprevious(lvl, level, maxlevel);
        if (result<>nil) and (not InRangeX(address,result.BaseAddress,result.BaseAddress+result.HookEvent.HeapAllocEvent.Size)) then
          result:=nil;

        exit;
      end;

      currentarray:=currentarray[entrynr].MemrecArray;

      inc(level);
    end;

    entrynr:=address shr ((maxlevel-level)*4) and $f;
    lvl[level].arr:=currentarray;
    lvl[level].entrynr:=entrynr;

    if currentarray[entrynr].memallocevent<>nil then
      result:=currentarray[entrynr].memallocevent
    else
    begin
      //not a direct match
      //try to find a previous entry
      result:=findprevious(lvl,level, maxlevel);
    end;


    if (result<>nil) and (not InRangeX(address,result.BaseAddress,result.BaseAddress+result.HookEvent.HeapAllocEvent.Size)) then
      result:=nil;
  {
  finally
    //memrecCS.Leave;
  end;}
end;

procedure DeletePath(addresslist: PMemrecTableArray; level: integer; maxlevel: integer);
var
  i: integer;
begin
  if level=maxlevel then
  begin
    for i:=0 to 15 do
    begin
      if addresslist[i].memallocevent<>nil then
        freeandnil(addresslist[i].memallocevent);
    end;
  end
  else
  begin
    for i:=0 to 15 do
    begin
      if addresslist[i].MemrecArray<>nil then
      begin
        deletepath(addresslist[i].MemrecArray,level+1, maxlevel);
        freememandnil(addresslist[i].MemrecArray);

      end;
    end;



  end;



end;

procedure TfrmMemoryAllocHandler.btnReloadClick(Sender: TObject);
var
  injectionscript: TStringlist;
  i: integer;
begin
  {$IFDEF windows}
  WaitForInitializationToFinish;

  injectionscript:=TStringList.Create;
  memrecCS.Enter;
  displaythread.csObjectList.Enter;
  try
    //whipe the old list
    DeletePath(@HeapBaselevel, 0, maxlevel);
    displaythread.heapcount:=0;


    injectionscript.Add('CreateThread(CeInitializeAllocHook)');
    ResetEvent(CEInitializationFinished);
    if not autoassemble(injectionscript, false) then raise exception.Create(rsFailureToInitialize);
  finally
    displaythread.csObjectList.Leave;
    memrecCS.Leave;
    injectionscript.Free;
  end;
  {$ENDIF}

end;

procedure TfrmMemoryAllocHandler.cbHookAllocsClick(Sender: TObject);
begin
  {$ifdef windows}
  if cbHookAllocs.Checked then
  begin

    if hookscript=nil then
      hookscript:=tstringlist.Create;

    hookscript.Clear;
    hookscript.Add('[Enable]');
    hookscript.Add('');
    hookscript.Add('[Disable]');
    hookscript.Add('');    


    //hook apis

    generateAPIHookScript(hookscript,'NtAllocateVirtualMemory','CeAllocateVirtualMemory', 'NtAllocateVirtualMemoryOrig','0');
    generateAPIHookScript(hookscript,'NtFreeVirtualMemory','CeFreeVirtualMemory', 'NtFreeVirtualMemoryOrig','1');
    generateAPIHookScript(hookscript,'RtlAllocateHeap','CeRtlAllocateHeap', 'RtlAllocateHeapOrig','2');
    generateAPIHookScript(hookscript,'RtlFreeHeap','CeRtlFreeHeap', 'RtlFreeHeapOrig','3');
    generateAPIHookScript(hookscript,'RtlDestroyHeap','CeRtlDestroyHeap', 'RtlDestroyHeapOrig','4');

    if not autoassemble(hookscript,false,true,false,false,hookallocarray, exceptionlist) then raise exception.Create(rsFailureToHook);
  end
  else
  begin
    //unload
    if hookscript=nil then exit; //should never happen

    if not autoassemble(hookscript,false,false,false,false,hookallocarray, exceptionlist) then raise exception.Create(rsFailureToHook);

    freeandnil(hookscript);
  end;
  {$endif}



end;

initialization
  {$i frmMemoryAllocHandlerUnit.lrs}

end.

