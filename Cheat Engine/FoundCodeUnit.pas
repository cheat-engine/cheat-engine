unit FoundCodeUnit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport, math,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  LCLIntf, LResources, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, disassembler, ExtCtrls, Menus,
  NewKernelHandler, clipbrd, ComCtrls, fgl, formChangedAddresses, LastDisassembleData,
  vmxfunctions, betterControls,  Maps, syncobjs, contexthandler;

type
  Tcoderecord = class
  private
    fhitcount: integer;
    fcontext: pointer;
    contexthandler: TContextInfo;
    function getContext: pointer;
    procedure setContext(ctx: Pointer);

    procedure setHitcount(c: integer);
  public
    firstSeen: TDateTime;
    lastSeen: TDateTime;
    addressString: string;
    address: ptrUint;
    size: integer;
    opcode: string;
    description: string;

    stack: record
      savedsize: ptruint;
      stack: pbyte;
    end;

    ipt: record
      log: pointer;
      size: integer;
    end;

    dbvmcontextbasic:    PPageEventBasic;


    diffcount: integer;
    LastDisassembleData: TLastDisassembleData;

    formChangedAddresses: TfrmChangedAddresses;

    property hitcount: integer read fhitcount write setHitcount;
    property context: pointer read getContext write setContext;
    procedure savestack;
    constructor create;
    destructor destroy; override;
end;

type

  { TFoundCodeDialog }
  TFoundCodeDialog=class;

  {$ifdef windows}
  TDBVMWatchPollThread=class(TThread)
  private
    results: PPageEventListDescriptor;
    resultsize: integer;

    cr3disassembler: Tcr3Disassembler;
    procedure addEntriesToList;
  public
    id: integer;
    fcd: TFoundCodeDialog;
    procedure execute; override;
  end;
  {$endif}


  TFoundCodeDialog = class(TForm)
    FoundCodeList: TListView;
    fcdImageList: TImageList;
    dbvmMissedEntries: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miFindWhatCodeAccesses: TMenuItem;
    MenuItem4: TMenuItem;
    miFindWhatAccesses: TMenuItem;
    miSaveTofile: TMenuItem;
    mInfo: TMemo;
    Panel1: TPanel;
    Description: TLabel;
    Panel4: TPanel;
    pmOptions: TPopupMenu;
    ReplacewithcodethatdoesnothingNOP1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Showthisaddressinthedisassembler1: TMenuItem;
    Addtothecodelist1: TMenuItem;
    MoreInfo1: TMenuItem;
    Panel2: TPanel;
    btnOK: TButton;
    Panel3: TPanel;
    btnExtraInfo: TButton;
    btnAddToCodeList: TButton;
    btnOpenDisassembler: TButton;
    btnReplacewithnops: TButton;
    N1: TMenuItem;
    Copyselectiontoclipboard1: TMenuItem;
    Splitter1: TSplitter;
    timerEntryInfoUpdate: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FoundCodeListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FoundcodeListClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnReplacewithnopsClick(Sender: TObject);
    procedure btnOpenDisassemblerClick(Sender: TObject);
    procedure btnAddToCodeListClick(Sender: TObject);
    procedure FoundCodeListColumnClick(Sender: TObject; Column: TListColumn);
    procedure FoundcodeListDblClick(Sender: TObject);
    procedure btnExtraInfoClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FoundCodeListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure MenuItem1Click(Sender: TObject);
    procedure miFindWhatAccessesClick(Sender: TObject);
    procedure miFindWhatCodeAccessesClick(Sender: TObject);
    procedure miSaveTofileClick(Sender: TObject);
    procedure pmOptionsPopup(Sender: TObject);
    procedure Copyselectiontoclipboard1Click(Sender: TObject);
    procedure timerEntryInfoUpdateTimer(Sender: TObject);
  private
    { Private declarations }
    loadedPosition: boolean;
    setcountwidth: boolean;
    fdbvmwatchid: integer;
    {$ifdef windows}
    dbvmwatchpollthread: TDBVMWatchPollThread;
    {$endif}

    usedmiFindWhatAccesses: boolean; //debug

    countsortdirection: integer;
    addresssortdirection: integer;
    procedure stopdbvmwatch;
    procedure addInfo(Coderecord: TCoderecord);
    procedure moreinfo;
    function getSelection: string;
    procedure setdbvmwatchid(id: integer);
    procedure ChangedAddressClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    { Public declarations }
    addresswatched: ptruint;
    useexceptions: boolean;
    usesdebugregs: boolean;
    multiplerip: boolean;

    dbvmwatch_unlock: qword;


    addRecord_Address: ptruint;
    iptlog: pointer; //do not free this.  owned by the debugthreadhandler
    iptlogsize: integer;
    breakpoint: pointer;

    seenAddressListCS: TCriticalSection; //should only be accessed by the debugger thread, but just in case...
    seenAddressList: TMap; //list for the debugger thread to determine if it should be added to the list

    procedure AddRecord;
    procedure setChangedAddressCount(address :ptruint);
    property dbvmwatchid: integer read fdbvmwatchid write setdbvmwatchid;
  end;


resourcestring
  strClose='Close';
  rsFCThesWillSetASiftwareBreakpointInt3OnEverySingleOpcodeEtc = 'This will set a Software Breakpoint (int3) on every single opcode that will be reported here. Are you sure ?';

//var
//  FoundCodeDialog: TFoundCodeDialog;

implementation

uses CEFuncProc, CEDebugger,debughelper, debugeventhandler, MemoryBrowserFormUnit,
     MainUnit,kerneldebugger, AdvancedOptionsUnit ,formFoundcodeListExtraUnit,
     MainUnit2, ProcessHandlerUnit, Globals, Parsers, DBK32functions, symbolhandler,
     DebuggerInterfaceAPIWrapper, DBVMDebuggerInterface, breakpointtypedef;



{$IFDEF windows}
procedure TDBVMWatchPollThread.execute;
var
  i: integer;
  size: integer;
begin
  cr3disassembler:=TCR3Disassembler.create;
  getmem(results,4096);
  resultsize:=4096;
  zeromemory(results,resultsize);

  try
    while not terminated do
    begin
      size:=resultsize;
      i:=dbvm_watch_retrievelog(id, results, size);
      if i=0 then
      begin
      //  OutputDebugString('TDBVMWatchPollThread returned 0');
      //  OutputDebugString('results^.numberOfEntries='+inttostr(results^.numberOfEntries));
      //  OutputDebugString('results^.maxSize='+inttostr(results^.maxSize));

        //process data
        if results^.numberOfEntries>0 then
        begin
      //    OutputDebugString('calling addEntriesToList');
          synchronize(addEntriesToList);
          sleep(10);
        end
        else
          sleep(50);
      end
      else
      if i=2 then
      begin
        //not enough memory. Allocate twice the needed amount
     //   outputdebugstring(inttostr(resultsize)+' is too small for the buffer. It needs to be at least '+inttostr(size));
        freememandnil(results);


        resultsize:=size*2;
        getmem(results, resultsize);
        zeromemory(results,resultsize);

        continue; //try again, no sleep
      end else
      begin
        outputdebugstring('dbvm_watch_retrievelog returned '+inttostr(i)+' which is not supported');
        exit;
      end;
    end;


  except
    on e: exception do
      outputdebugstring('TDBVMWatchPollThread crash:'+e.Message);
  end;

  freememandnil(results);
  freeandnil(cr3disassembler);
end;
{$ENDIF}

destructor TCodeRecord.Destroy;
begin
  if stack.stack<>nil then
    freememandnil(stack.stack);

  if ipt.log<>nil then
    freememandnil(ipt.log);

  if fcontext<>nil then
    freememandnil(fcontext);

  inherited destroy;
end;

constructor TCodeRecord.create;
begin
  formChangedAddresses:=nil;
  firstseen:=now;
end;


{$IFDEF windows}
procedure TDBVMWatchPollThread.addEntriesToList;
var
  coderecord: TCodeRecord;
  c: pcontext;
  i,j: integer;
  opcode,desc: string;
  li: TListItem;
  ldi: TLastDisassembleData;

  basicinfo: TPageEventBasic;

  basic: PPageEventBasicArray;
  extended: PPageEventExtendedArray absolute basic;
  basics: PPageEventBasicWithStackArray absolute basic;
  extendeds: PPageEventExtendedWithStackArray absolute basic;

  address, address2: ptruint;

  skip: boolean;

  debug,debug2: pointer;


begin
  outputdebugstring('addEntriesToList');

  if results^.missedEntries>0 then
  begin
    fcd.dbvmMissedEntries.caption:=string.format(rsDBVMMissedEntries, [results^.missedEntries]);
    if fcd.dbvmMissedEntries.visible=false then
      fcd.dbvmMissedEntries.visible:=true;
  end;


  try
    basic:=PPageEventBasicArray(ptruint(results)+sizeof(TPageEventListDescriptor));

    for i:=0 to results^.numberOfEntries-1 do
    begin

      case results^.entryType of
        0: basicinfo:=basic^[i];
        1: basicinfo:=extended^[i].basic;
        2: basicinfo:=basics^[i].basic;
        3: basicinfo:=extendeds^[i].basic;
      end;
      address:=basicinfo.RIP;
      outputdebugstring('testing entry '+inttostr(i)+'('+inttohex(address,8)+') - basicinfo.Count='+inttostr(basicinfo.Count+1));

      //check if this address is inside the list
      skip:=false;
      for j:=0 to fcd.foundcodelist.Items.Count-1 do
      begin
        if TCodeRecord(fcd.foundcodelist.Items[j].data).address=address then
        begin
          //it's already in the list
          if fcd.multiplerip=false then
          begin
            OutputDebugString(inttostr(j)+':Already in the list');
            TCodeRecord(fcd.foundcodelist.Items[j].data).hitcount:=TCodeRecord(fcd.foundcodelist.Items[j].data).hitcount+(basicinfo.Count+1);
            skip:=true;

            fcd.foundcodelist.Items[j].caption:=inttostr(TCodeRecord(fcd.foundcodelist.Items[j].data).hitcount);
            break;
          end
          else
          begin
            //check the other registers
            if (basicinfo.RAX=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.RAX) and
               (basicinfo.RBX=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.RBX) and
               (basicinfo.RCX=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.RCX) and
               (basicinfo.RDX=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.RDX) and
               (basicinfo.RSP=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.RSP) and
               (basicinfo.RBP=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.RBP) and
               (basicinfo.RSI=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.RSI) and
               (basicinfo.RDI=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.RDI) and
               (basicinfo.R8=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.R8) and
               (basicinfo.R9=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.R9) and
               (basicinfo.R10=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.R10) and
               (basicinfo.R11=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.R11) and
               (basicinfo.R12=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.R12) and
               (basicinfo.R13=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.R13) and
               (basicinfo.R14=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.R14) and
               (basicinfo.R15=TCodeRecord(fcd.foundcodelist.Items[j].data).dbvmcontextbasic^.R15) THEN
            begin
              TCodeRecord(fcd.foundcodelist.Items[j].data).hitcount:=TCodeRecord(fcd.foundcodelist.Items[j].data).hitcount+basicinfo.count;

              skip:=true;
              break;
            end;
          end;
        end;
      end;

      if skip then
      begin
        OutputDebugString('Skipping entry. it''s in the list');
        continue;
      end
      else
        OutputDebugString('Not in the list yet. Adding it');

      coderecord:=TCoderecord.create;
      getmem(coderecord.dbvmcontextbasic, sizeof(TPageEventBasicArray));
      c:=coderecord.context;

      coderecord.dbvmcontextbasic^:=basicinfo;



      outputdebugstring('created coderecord and  dbvmcontextbasic');

      case results^.entryType of

        0: ; //already done

        1: //extended
        begin
          copymemory(@c^.{$ifdef cpu64}FltSave{$else}ext{$endif}, @extended^[i].fpudata, sizeof(c^.{$ifdef cpu64}FltSave{$else}ext{$endif}));

        end;

        2: //basic with stack
        begin
          coderecord.stack.savedsize:=4096;
          getmem(coderecord.stack.stack, 4096);

          copymemory(coderecord.stack.stack, @basics^[i].stack[0], 4096);
        end;

        3: //extended with stack
        begin
          copymemory(@c^.{$ifdef cpu64}FltSave{$else}ext{$endif}, @extendeds^[i].fpudata, sizeof(c^.{$ifdef cpu64}FltSave{$else}ext{$endif}));
          coderecord.stack.savedsize:=4096;
          getmem(coderecord.stack.stack, 4096);

          copymemory(coderecord.stack.stack, @basics^[i].stack[0], 4096);
        end;

        else
        begin
          //error
          outputdebugstring('unexpected type');
          freememandnil(coderecord.dbvmcontextbasic);
          freeandnil(coderecord);
          exit;
        end;
      end;

      address:=coderecord.dbvmcontextbasic^.RIP;
      address2:=address;

      cr3disassembler.cr3:=coderecord.dbvmcontextbasic^.CR3;
      outputdebugstring(format('disassembling %.8x with CR3 %.8x: ',[address, cr3disassembler.cr3]));

      opcode:=cr3disassembler.disassemble(address2,desc);
      ldi:=cr3disassembler.LastDisassembleData;

      outputdebugstring('opcode='+opcode);


      coderecord.address:=address;
      coderecord.size:=address2-address;
      coderecord.opcode:=opcode;
      coderecord.description:=desc;
      coderecord.LastDisassembleData:=ldi;
      coderecord.hitcount:=coderecord.dbvmcontextbasic^.Count;

      //make compatible with older code:
      OutputDebugString('setting up compatibility context');

      c^.{$ifdef cpu64}Rax{$else}Eax{$endif}:=coderecord.dbvmcontextbasic^.RAX;
      c^.{$ifdef cpu64}Rbx{$else}Ebx{$endif}:=coderecord.dbvmcontextbasic^.RBX;
      c^.{$ifdef cpu64}Rcx{$else}Ecx{$endif}:=coderecord.dbvmcontextbasic^.RCX;
      c^.{$ifdef cpu64}Rdx{$else}Edx{$endif}:=coderecord.dbvmcontextbasic^.RDX;
      c^.{$ifdef cpu64}Rsi{$else}Esi{$endif}:=coderecord.dbvmcontextbasic^.RSI;
      c^.{$ifdef cpu64}Rdi{$else}Edi{$endif}:=coderecord.dbvmcontextbasic^.RDI;
      c^.{$ifdef cpu64}Rbp{$else}Ebp{$endif}:=coderecord.dbvmcontextbasic^.RBP;
      c^.{$ifdef cpu64}Rsp{$else}Esp{$endif}:=coderecord.dbvmcontextbasic^.Rsp;
      c^.{$ifdef cpu64}Rip{$else}Eip{$endif}:=coderecord.dbvmcontextbasic^.Rip;
      {$ifdef cpu64}
      c^.R8:=coderecord.dbvmcontextbasic^.R8;
      c^.R9:=coderecord.dbvmcontextbasic^.R9;
      c^.R10:=coderecord.dbvmcontextbasic^.R10;
      c^.R11:=coderecord.dbvmcontextbasic^.R11;
      c^.R12:=coderecord.dbvmcontextbasic^.R12;
      c^.R13:=coderecord.dbvmcontextbasic^.R13;
      c^.R14:=coderecord.dbvmcontextbasic^.R14;
      c^.R15:=coderecord.dbvmcontextbasic^.R15;
      {$endif}
      c^.EFlags:=coderecord.dbvmcontextbasic^.FLAGS;
      c^.SegCs:=coderecord.dbvmcontextbasic^.CS;
      c^.SegSs:=coderecord.dbvmcontextbasic^.SS;
      c^.SegDs:=coderecord.dbvmcontextbasic^.DS;
      c^.SegEs:=coderecord.dbvmcontextbasic^.ES;
      c^.SegFs:=coderecord.dbvmcontextbasic^.FS;
      c^.SegGs:=coderecord.dbvmcontextbasic^.GS;

      coderecord.hitcount:=coderecord.dbvmcontextbasic^.Count+1;
      coderecord.diffcount:=0;

      OutputDebugString('adding to the foundlist');
      li:=fcd.FoundCodeList.Items.Add;
      li.caption:='1';
      li.SubItems.add(opcode);
      li.data:=coderecord;

      OutputDebugString('done with no errors');
    end;

  except
    on e: exception do
      outputdebugstring('TDBVMWatchPollThread.addEntriesToList error: '+e.message);
  end;
end;
{$ENDIF}

procedure TCodeRecord.setHitcount(c: integer);
begin
  fHitcount:=c;
  lastSeen:=now;
end;

function TCodeRecord.getContext: pointer;
begin
  if fcontext=nil then
  begin
    outputdebugstring('TCodeRecord.getContext: fContext was nil. Allocating it');
    contexthandler:=getBestContextHandler;
    fcontext:=getmem(contexthandler.ContextSize);
    ZeroMemory(fcontext, contexthandler.ContextSize);
  end;

  result:=fcontext;
end;

procedure TCodeRecord.setContext(ctx: Pointer);
begin
  contexthandler:=getBestContextHandler;
  fcontext:=contexthandler.getCopy(ctx);
end;

procedure TCodeRecord.savestack;
var base: qword;
begin
  getmem(stack.stack, savedStackSize);
  base:=contexthandler.StackPointerRegister^.getValue(context);

  if ReadProcessMemory(processhandle, pointer(base), stack.stack, savedStackSize, stack.savedsize)=false then
  begin
    //for some reason this sometimes returns 0 bytes read even if some of the bytes are readable.
    stack.savedsize:=4096-(base mod 4096);
    ReadProcessMemory(processhandle, pointer(base), stack.stack, stack.savedsize, stack.savedsize);
  end;
end;

procedure TFoundCodeDialog.AddRecord;
{
Invoked by the debugger thread
It takes the data from the current thread and stores it in the processlist
}
var currentthread: TDebugThreadHandler;
  opcode: string;
  ldi: TLastDisassembleData;
  address,address2: ptrUint;
  desc: string;

  coderecord: TCodeRecord;
  i: integer;

  li: tlistitem;
  f: TfrmChangedAddresses;

  d: TDisassembler;
begin
  currentthread:=debuggerthread.CurrentThread;
  address:=addRecord_Address;

  //disassemble to get the opcode and size
  address2:=address;
  d:=TDisassembler.Create;
  opcode:=d.disassemble(address2,desc);
  ldi:=d.LastDisassembleData;
  freeandnil(d);


  coderecord:=TCoderecord.create;
  coderecord.address:=address;
  coderecord.size:=address2-address;
  coderecord.opcode:=opcode;
  coderecord.description:=desc;
  coderecord.context:=currentthread.context;
  coderecord.LastDisassembleData:=ldi;
  coderecord.savestack;
  coderecord.hitcount:=1;
  coderecord.diffcount:=0;

  getmem(coderecord.ipt.log, iptlogsize);
  copymemory(coderecord.ipt.log, iptlog, iptlogsize);
  coderecord.ipt.size:=iptlogsize;


  seenAddressListCS.Enter;
  try
    seenAddressList.Add(address, coderecord);
  except
    //should NEVER happen as AddRecord is only called when it's not in the list, but just to be sure...
  end;
  seenAddressListCS.Leave;

  li:=FoundCodeList.Items.Add;
  li.caption:='1';
  li.SubItems.add(opcode);
  li.data:=coderecord;

  if miFindWhatAccesses.Checked then //add it
    coderecord.formChangedAddresses:=debuggerthread.FindWhatCodeAccesses(address, self);  //ffffffuuuuuuuuuu. Rebuild again

  debuggerthread.guiupdate:=true;
end;

procedure TFoundCodeDialog.stopdbvmwatch;
begin
  {$IFDEF windows}
  if dbvmwatchpollthread<>nil then
  begin
    dbvmwatchpollthread.Terminate;
    dbvmwatchpollthread.WaitFor;
    freeandnil(dbvmwatchpollthread);
  end;


  if dbvmwatchid<>-1 then
  begin
    dbvm_watch_delete(dbvmwatchid);
    dbvmwatchid:=-1;
  end;

  if dbvmwatch_unlock<>0 then
  begin
    UnlockMemory(dbvmwatch_unlock);
    dbvmwatch_unlock:=0;
  end;
  {$endif}
end;

procedure TFoundCodeDialog.setdbvmwatchid(id: integer);
begin
  {$IFDEF windows}
  fdbvmwatchid:=id;

  if id<>-1 then
  begin
    if dbvmwatchpollthread<>nil then
      freeandnil(dbvmwatchpollthread);

    dbvmwatchpollthread:=TDBVMWatchPollThread.Create(true);
    dbvmwatchpollthread.id:=id;
    dbvmwatchpollthread.fcd:=self;
    dbvmwatchpollthread.Start;

    useexceptions:=true;
  end;
  {$ENDIF}
end;


procedure TFoundCodeDialog.setChangedAddressCount(address :ptruint);
var i: integer;
begin
  for i:=0 to foundcodelist.Items.Count-1 do
    if TCodeRecord(foundcodelist.Items[i].data).address=address then
    begin
      //increase the counter
      inc(TCodeRecord(foundcodelist.Items[i].data).diffcount);
      FoundcodeList.items[i].caption:=inttostr(TCodeRecord(foundcodelist.Items[i].data).hitcount)+' ('+inttostr(TCodeRecord(foundcodelist.Items[i].data).diffcount)+')';
      exit;
    end;
end;

procedure TFoundCodeDialog.addInfo(Coderecord: TCoderecord);
var
  address: ptruint;
  disassembled: array[1..5] of string;
  firstchar: char;
  hexcount: integer;
  temp: string;
  i: integer;

  gplist: PContextElementRegisterList;
begin
  if processhandler.is64Bit then
    hexcount:=16
  else
    hexcount:=8;

  address:=Coderecord.address;
  address:=previousopcode(address);
  address:=previousopcode(address);

  disassembled[1]:=disassemble(address,temp);
  disassembled[2]:=disassemble(address,temp);

  if address<>coderecord.address then
  begin
    disassembled[1]:='';
    disassembled[2]:='';
    disassembled[3]:=coderecord.opcode;
    disassembled[4]:='';
    disassembled[5]:='';
  end
  else
  begin
    disassembled[3]:=disassemble(address,temp);
    disassembled[4]:=disassemble(address,temp);
    disassembled[5]:=disassemble(address,temp);
  end;

  minfo.Lines.BeginUpdate;
  try
    if coderecord.addressString<>'' then minfo.Lines.add(coderecord.addressString+':');

    minfo.Lines.Add(disassembled[1]);
    minfo.Lines.Add(disassembled[2]);
    minfo.Lines.Add(disassembled[3]+' <<');
    minfo.Lines.Add(disassembled[4]);
    minfo.Lines.Add(disassembled[5]);
    minfo.Lines.Add('');


    gplist:=coderecord.contexthandler.getGeneralPurposeRegisters;
    if gplist<>nil then
      for i:=0 to length(gplist^)-1 do
        minfo.Lines.Add(gplist^[i].name+'='+gplist^[i].getFullValueString(coderecord.context));

    minfo.lines.add('');
    minfo.lines.add('');

    minfo.lines.add('First seen:'+TimeToStr(coderecord.firstSeen));
    minfo.lines.add('Last seen:'+TimeToStr(coderecord.lastSeen));



  finally
    minfo.lines.EndUpdate;
  end;
end;

procedure TFoundCodeDialog.ChangedAddressClose(Sender: TObject; var CloseAction: TCloseAction);
var i: integer;
  coderecord: Tcoderecord;
begin
  for i:=0 to FoundCodeList.Items.Count-1 do
  begin
    coderecord:=TCodeRecord(foundcodelist.items[i].data);
    if coderecord.formChangedAddresses=sender then
      coderecord.formChangedAddresses:=nil;
  end;

end;

procedure TFoundCodeDialog.moreinfo;
var
  disassembled: array[1..5] of record
    s: string;
    a: ptruint;
  end;

    address: ptrUint;
    itemindex: integer;
    temp,temp2: string;
    maxregistervalue: ptrUint;
    p: ptrUint;
    i: integer;
    coderecord: TCodeRecord;
    firstchar: string;



    w: integer;

    FormFoundCodeListExtra: TFormFoundCodeListExtra;

    ew: trect; //extra window rect
    cw: trect; //changed address window rect

    offset: integer;

    lbl: TLabel;

  d: TDisassembler;

  gplist: PContextElementRegisterList;
begin
  if processhandler.is64bit then
    firstchar:='R' else firstchar:='E';

  itemindex:=foundcodelist.ItemIndex;
  if itemindex<>-1 then
  begin
    FormFoundCodeListExtra:=TFormFoundCodeListExtra.Create(application);
    if useexceptions or (currentdebuggerinterface is TDBVMDebugInterface) then
      FormFoundCodeListExtra.Label18.Visible:=false
    else
      FormFoundCodeListExtra.Label18.Visible:=true;

    coderecord:=TCodeRecord(foundcodelist.items[itemindex].data);
    if coderecord.formChangedAddresses<>nil then
    begin
      if not coderecord.formChangedAddresses.visible then //override userdefined positioning
      begin
        //LCLIntf.GetWindowRect(coderecord.formChangedAddresses.handle, cw);
        //LCLIntf.GetWindowRect(FormFoundCodeListExtra.handle, ew);

        //coderecord.formChangedAddresses.left:=(ew.Left-(cw.Right-cw.left));
        //coderecord.formChangedAddresses.top:=FormFoundCodeListExtra.top;

      end;

      coderecord.formChangedAddresses.show;
      coderecord.formChangedAddresses.AddHandlerClose(ChangedAddressClose);
    end;


    address:=coderecord.address;
    {$IFDEF windows}
    if CurrentDebuggerInterface is TDBVMDebugInterface then
    begin
      {$ifdef cpu64}
      if pcontext(coderecord.context)^.P2Home<>0 then
      begin
        d:=TCR3Disassembler.Create;
        TCR3Disassembler(d).CR3:=pcontext(coderecord.context)^.P2Home;
      end
      else
      {$endif}
        d:=TDisassembler.Create;
    end
    else
    if coderecord.dbvmcontextbasic<>nil then
    begin
      d:=TCR3Disassembler.Create;
      TCR3Disassembler(d).CR3:=coderecord.dbvmcontextbasic^.CR3;
    end
    else
    {$ENDIF}
      d:=TDisassembler.Create;


    address:=previousopcode(address, d);
    address:=previousopcode(address, d);

    disassembled[1].a:=address;
    disassembled[1].s:=d.disassemble(address,temp);

    disassembled[2].a:=address;
    disassembled[2].s:=d.disassemble(address,temp);

    if address<>coderecord.address then
    begin
      disassembled[1].s:='';
      disassembled[2].s:='';
      disassembled[3].s:=coderecord.opcode;
      disassembled[4].s:='';
      disassembled[5].s:='';
    end
    else
    begin
      disassembled[3].a:=address;
      disassembled[3].s:=d.disassemble(address,temp);

      disassembled[4].a:=address;
      disassembled[4].s:=d.disassemble(address,temp);

      disassembled[5].a:=address;
      disassembled[5].s:=d.disassemble(address,temp);
    end;

    freeandnil(d);



    //convert disassembled strings to address+opcode only (no bytes)
    //xxxxxxxx - xx xx xx - opcode
    temp:=copy(disassembled[1].s,pos('-',disassembled[1].s)+2,length(disassembled[1].s));
    temp:=copy(temp,pos('-',temp)+2,length(temp));
    disassembled[1].s:=copy(disassembled[1].s,1,pos('-',disassembled[1].s))+' '+temp;

    temp:=copy(disassembled[2].s,pos('-',disassembled[2].s)+2,length(disassembled[2].s));
    temp:=copy(temp,pos('-',temp)+2,length(temp));
    disassembled[2].s:=copy(disassembled[2].s,1,pos('-',disassembled[2].s))+' '+temp;

    temp:=copy(disassembled[3].s,pos('-',disassembled[3].s)+2,length(disassembled[3].s));
    temp:=copy(temp,pos('-',temp)+2,length(temp));
    disassembled[3].s:=copy(disassembled[3].s,1,pos('-',disassembled[3].s))+' '+temp;

    temp:=copy(disassembled[4].s,pos('-',disassembled[4].s)+2,length(disassembled[4].s));
    temp:=copy(temp,pos('-',temp)+2,length(temp));
    disassembled[4].s:=copy(disassembled[4].s,1,pos('-',disassembled[4].s))+' '+temp;

    temp:=copy(disassembled[5].s,pos('-',disassembled[5].s)+2,length(disassembled[5].s));
    temp:=copy(temp,pos('-',temp)+2,length(temp));
    disassembled[5].s:=copy(disassembled[5].s,1,pos('-',disassembled[5].s))+' '+temp;



    with FormFoundCodeListExtra do
    begin
      pnlEPTWatch.visible:=false;

      label1.Caption:=disassembled[1].s;
      label1.tag:=disassembled[1].a;

      Label2.Caption:=disassembled[2].s;
      Label2.tag:=disassembled[2].a;

      Label3.Caption:=disassembled[3].s;
      Label3.tag:=disassembled[3].a;

      Label4.Caption:=disassembled[4].s;
      Label4.tag:=disassembled[4].a;

      Label5.Caption:=disassembled[5].s;
      Label5.tag:=disassembled[5].a;


      BeginFormUpdate;
      while pnlRegisters.ControlCount>0 do
        pnlRegisters.Controls[0].Destroy;

      //controlsPerLine:
      //9 - 8 digit registers = 3 controls/line  (3 rows)  (3x3)      (24 digits/row)
      //16 - 16 digit registers = registers = 5

      gplist:=coderecord.contexthandler.getGeneralPurposeRegisters;
      if gplist<>nil then
      begin
        pnlRegisters.ChildSizing.ControlsPerLine:=length(gplist^) div 3;
        for i:=0 to length(gplist^)-1 do
        begin
          lbl:=tlabel.create(FormFoundCodeListExtra);
          lbl.Caption:=gplist^[i].name+'='+gplist^[i].getValueString(coderecord.context);
          lbl.tag:=ptruint(@gplist^[i]);
          lbl.parent:=pnlRegisters;
          lbl.OnMouseDown:=registerMouseDown;
          lbl.OnDblClick:=RegisterDblClick;
        end;
      end;
      EndFormUpdate;
     (*
      if processhandler.SystemArchitecture=archarm then
      begin
        formfoundcodelistextra.label17.caption:='';

        if processhandler.is64Bit then
        begin
          //just start from scratch
          while pnlRegisters.ControlCount>0 do
            pnlRegisters.Controls[0].Destroy;

          pnlRegisters.ChildSizing.ControlsPerLine:=6;

          for i:=0 to 30 do
          begin
            lbl:=tlabel.create(FormFoundCodeListExtra);
            lbl.Caption:=format('X%d=%.8x',[i,coderecord.arm64context.regs.X[i]]);
            lbl.parent:=pnlRegisters;
            lbl.OnMouseDown:=registerMouseDown;
            lbl.OnDblClick:=RegisterDblClick;
          end;

          lbl:=tlabel.create(FormFoundCodeListExtra);
          lbl.Caption:=format('SP=%.8x',[coderecord.arm64context.SP]);
          lbl.parent:=pnlRegisters;
          lbl.OnMouseDown:=registerMouseDown;
          lbl.OnDblClick:=RegisterDblClick;

          lbl:=tlabel.create(FormFoundCodeListExtra);
          lbl.Caption:=format('PC=%.8x',[coderecord.arm64context.PC]);
          lbl.parent:=pnlRegisters;
          lbl.OnMouseDown:=registerMouseDown;
          lbl.OnDblClick:=RegisterDblClick;

          lbl:=tlabel.create(FormFoundCodeListExtra);
          lbl.Caption:=format('PSTATE=%.8x',[coderecord.arm64context.PSTATE]);
          lbl.parent:=pnlRegisters;
          lbl.OnMouseDown:=registerMouseDown;
          lbl.OnDblClick:=RegisterDblClick;
        end
        else
        begin
          //repurpose the x86 32-bit labels
          lblRAX.caption:=' R0='+IntToHex(coderecord.armcontext.R0,8);
          lblRDX.caption:=' R1='+IntToHex(coderecord.armcontext.R1,8);
          lblRBP.caption:=' R2='+IntToHex(coderecord.armcontext.R2,8);
          lblRBX.caption:=' R3='+IntToHex(coderecord.armcontext.R3,8);
          lblRSI.caption:=' R4='+IntToHex(coderecord.armcontext.R4,8);
          lblRSP.caption:=' R5='+IntToHex(coderecord.armcontext.R5,8);
          lblRCX.caption:=' R6='+IntToHex(coderecord.armcontext.R6,8);
          lblRDI.caption:=' R7='+IntToHex(coderecord.armcontext.R7,8);
          lblRIP.caption:=' R8='+IntToHex(coderecord.armcontext.R8,8);

          lblR9:=tlabel.Create(FormFoundCodeListExtra);
          lblR9.caption:=' R9='+IntToHex(coderecord.armcontext.R9,8);
          lblR9.parent:=pnlRegisters;
          lblR9.OnMouseDown:=registerMouseDown;
          lblR9.OnDblClick:=RegisterDblClick;

          lblR10:=tlabel.Create(FormFoundCodeListExtra);
          lblR10.caption:='R10='+IntToHex(coderecord.armcontext.R10,8);
          lblR10.parent:=pnlRegisters;
          lblR10.OnMouseDown:=registerMouseDown;
          lblR10.OnDblClick:=RegisterDblClick;

          lblR11:=tlabel.Create(FormFoundCodeListExtra);
          lblR11.caption:=' FP='+IntToHex(coderecord.armcontext.FP,8);
          lblR11.parent:=pnlRegisters;
          lblR11.OnMouseDown:=registerMouseDown;
          lblR11.OnDblClick:=RegisterDblClick;
                  //new row
          lblR12:=tlabel.Create(FormFoundCodeListExtra);
          lblR12.caption:=' IP='+IntToHex(coderecord.armcontext.IP,8);
          lblR12.parent:=pnlRegisters;
          lblR12.OnMouseDown:=registerMouseDown;
          lblR12.OnDblClick:=RegisterDblClick;

          lblR13:=tlabel.Create(FormFoundCodeListExtra);
          lblR13.caption:=' SP='+IntToHex(coderecord.armcontext.SP,8);
          lblR13.parent:=pnlRegisters;
          lblR13.OnMouseDown:=registerMouseDown;
          lblR13.OnDblClick:=RegisterDblClick;

          lblR14:=tlabel.Create(FormFoundCodeListExtra);
          lblR14.caption:=' LR='+IntToHex(coderecord.armcontext.LR,8);
          lblR14.parent:=pnlRegisters;
          lblR14.OnMouseDown:=registerMouseDown;
          lblR14.OnDblClick:=RegisterDblClick;
          //new line

          lblR15:=tlabel.Create(FormFoundCodeListExtra);
          lblR15.caption:=' PC='+IntToHex(coderecord.armcontext.PC,8);
          lblR15.parent:=pnlRegisters;
          lblR15.OnMouseDown:=registerMouseDown;
          lblR15.OnDblClick:=RegisterDblClick;

          lblR16:=tlabel.Create(FormFoundCodeListExtra);
          lblR16.caption:=' CPSR='+IntToHex(coderecord.armcontext.CPSR,8);
          lblR16.parent:=pnlRegisters;
          lblR16.OnMouseDown:=registerMouseDown;
          lblR16.OnDblClick:=RegisterDblClick;

          lblR17:=tlabel.Create(FormFoundCodeListExtra);
          lblR17.caption:=' ORIG_R0='+IntToHex(coderecord.armcontext.ORIG_R0,8);
          lblR17.parent:=pnlRegisters;
          lblR17.OnMouseDown:=registerMouseDown;
          lblR17.OnDblClick:=RegisterDblClick;
        end;
        {Constraints.MinHeight:=panel6.top+(lblR17.top+lblR17.height)+16+panel5.height;
        if height<Constraints.MinHeight then
          height:=Constraints.MinHeight;   }
      end
      else *)
      begin


        (*

        lblRAX.caption:=firstchar+'AX='+IntToHex(coderecord.context.{$ifdef cpu64}Rax{$else}Eax{$endif},8);
        lblRBX.caption:=firstchar+'BX='+IntToHex(coderecord.context.{$ifdef cpu64}Rbx{$else}Ebx{$endif},8);
        lblRCX.caption:=firstchar+'CX='+IntToHex(coderecord.context.{$ifdef cpu64}Rcx{$else}Ecx{$endif},8);
        lblRDX.caption:=firstchar+'DX='+IntToHex(coderecord.context.{$ifdef cpu64}Rdx{$else}Edx{$endif},8);
        lblRSI.caption:=firstchar+'SI='+IntToHex(coderecord.context.{$ifdef cpu64}Rsi{$else}Esi{$endif},8);
        lblRDI.caption:=firstchar+'DI='+IntToHex(coderecord.context.{$ifdef cpu64}Rdi{$else}Edi{$endif},8);
        lblRSP.caption:=firstchar+'SP='+IntToHex(coderecord.context.{$ifdef cpu64}Rsp{$else}Esp{$endif},8);
        lblRBP.caption:=firstchar+'BP='+IntToHex(coderecord.context.{$ifdef cpu64}Rbp{$else}Ebp{$endif},8);
        lblRIP.caption:=firstchar+'IP='+IntToHex(coderecord.context.{$ifdef cpu64}Rip{$else}Eip{$endif},8);

        {$ifdef cpu64}
        if processhandler.is64bit then
        begin
          pnlRegisters.ChildSizing.ControlsPerLine:=5;

          lblR8:=tlabel.Create(FormFoundCodeListExtra);
          lblR8.caption:=' R8='+IntToHex(coderecord.context.r8,8);
          lblR8.parent:=FormFoundCodeListExtra.pnlRegisters;
          lblR8.OnMouseDown:=registerMouseDown;
          lblR8.OnDblClick:=RegisterDblClick;
          lblR8.Align:=lblrcx.Align;
          lblR8.Color:=lblRAX.Color;
          lblR8.Name:='lblR8';



          lblR9:=tlabel.Create(FormFoundCodeListExtra);
          lblR9.caption:=' R9='+IntToHex(coderecord.context.r9,8);
          lblR9.parent:=FormFoundCodeListExtra.pnlRegisters;
          lblR9.OnMouseDown:=registerMouseDown;
          lblR9.OnDblClick:=RegisterDblClick;
          lblR9.Align:=lblrcx.Align;
          lblR9.Color:=lblRAX.Color;


          lblR10:=tlabel.Create(FormFoundCodeListExtra);
          lblR10.caption:='R10='+IntToHex(coderecord.context.r10,8);
          lblR10.parent:=FormFoundCodeListExtra.pnlRegisters;
          lblR10.OnMouseDown:=registerMouseDown;
          lblR10.OnDblClick:=RegisterDblClick;
          lblR10.Align:=lblrcx.Align;
          lblR10.Color:=lblRAX.Color;


          lblR11:=tlabel.Create(FormFoundCodeListExtra);
          lblR11.caption:='R11='+IntToHex(coderecord.context.r11,8);
          lblR11.parent:=FormFoundCodeListExtra.pnlRegisters;
          lblR11.OnMouseDown:=registerMouseDown;
          lblR11.OnDblClick:=RegisterDblClick;
          lblR11.Align:=lblrcx.Align;
          lblR11.Color:=lblRAX.Color;


          lblR12:=tlabel.Create(FormFoundCodeListExtra);
          lblR12.caption:='R12='+IntToHex(coderecord.context.r12,8);
          lblR12.parent:=FormFoundCodeListExtra.pnlRegisters;
          lblR12.OnMouseDown:=registerMouseDown;
          lblR12.OnDblClick:=RegisterDblClick;
          lblR12.Align:=lblrcx.Align;
          lblR12.Color:=lblRAX.Color;


          lblR13:=tlabel.Create(FormFoundCodeListExtra);
          lblR13.caption:='R13='+IntToHex(coderecord.context.r13,8);
          lblR13.parent:=FormFoundCodeListExtra.pnlRegisters;
          lblR13.OnMouseDown:=registerMouseDown;
          lblR13.OnDblClick:=RegisterDblClick;
          lblR13.Align:=lblrcx.Align;
          lblR13.Color:=lblRAX.Color;


          lblR14:=tlabel.Create(FormFoundCodeListExtra);
          lblR14.caption:='R14='+IntToHex(coderecord.context.r14,8);
          lblR14.parent:=FormFoundCodeListExtra.pnlRegisters;
          lblR14.OnMouseDown:=registerMouseDown;
          lblR14.OnDblClick:=RegisterDblClick;
          lblR14.Align:=lblrcx.Align;
          lblR14.Color:=lblRAX.Color;



          lblR15:=tlabel.Create(FormFoundCodeListExtra);
          lblR15.caption:='R15='+IntToHex(coderecord.context.r15,8);
          lblR15.parent:=FormFoundCodeListExtra.pnlRegisters;
          lblR15.OnMouseDown:=registerMouseDown;
          lblR15.OnDblClick:=RegisterDblClick;
          lblR15.Align:=lblrcx.Align;
          lblR15.Color:=lblRAX.Color;

          if (currentdebuggerinterface is TDBVMDebugInterface) and (coderecord.context.P2Home<>0) then
          begin
            lblR16:=tlabel.Create(FormFoundCodeListExtra);
            lblR16.caption:=' CR3='+IntToHex(coderecord.context.P2Home,8);
            lblR16.parent:=FormFoundCodeListExtra.pnlRegisters;
            lblR16.OnMouseDown:=registerMouseDown;
            lblR16.OnDblClick:=RegisterDblClick;
            lblR16.Align:=lblrcx.Align;
            lblR16.Color:=lblRAX.Color;
          end;

          lblRBP.BringToFront;
          lblRSP.BringToFront;
          lblRIP.BringToFront;
        end;
        {$endif}
        *)

        if coderecord.dbvmcontextbasic<>nil then
        begin
          pnlEPTWatch.visible:=true;
          lblPhysicalAddress.caption:=format('Physical Address=%.8x',[coderecord.dbvmcontextbasic^.PhysicalAddress]);
          lblVirtualAddress.caption:=format('Virtual Address=%.8x',[coderecord.dbvmcontextbasic^.VirtualAddress]);
          lblFSBase.caption:=format('FS Base=%.8x',[coderecord.dbvmcontextbasic^.FSBASE]);
          lblGSBase.caption:=format('GS Base=%.8x',[coderecord.dbvmcontextbasic^.GSBASE]);
          lblGSBaseKernel.caption:=format('GS Base Kernel=%.8x',[coderecord.dbvmcontextbasic^.GSBASE_KERNEL]);
          lblCR3.caption:=format('CR3=%.8x',[coderecord.dbvmcontextbasic^.CR3]);
        end
        else
          pnlEPTWatch.visible:=false;
      end;
      label6.Caption:=coderecord.description;
    end;

    //parse the disassembled[3] string to help the user find the pointer
    //first find the [xxx]

    temp:=lowercase(copy(disassembled[3].s,pos('[',disassembled[3].s)+1,(pos(']',disassembled[3].s)-1)-(pos('[',disassembled[3].s))));
    firstchar:=lowercase(firstchar);

    maxregistervalue:=0;

    if temp<>'' then
    begin
      //parse
      //find the biggest value, registers or exact value

      if ((coderecord.LastDisassembleData.modrmValueType=dvtValue) and (coderecord.LastDisassembleData.hasSib=false)) then
      begin
        //only a value and no sib (the sib part can get messed up if it's a read)

        offset:=integer(coderecord.LastDisassembleData.modrmValue);

        if processhandler.SystemArchitecture=archX86 then
          formfoundcodelistextra.probably:=addresswatched-offset;
      end
      else
      begin
        if processhandler.SystemArchitecture=archX86 then
        begin
          //todo: contexthandler lookups
          //try the old code (coderecord.context is
          if pos(firstchar+'ax',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.{$ifdef cpu64}Rax{$else}Eax{$endif});
          if pos(firstchar+'bx',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.{$ifdef cpu64}Rbx{$else}Ebx{$endif});
          if pos(firstchar+'cx',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.{$ifdef cpu64}Rcx{$else}Ecx{$endif});
          if pos(firstchar+'dx',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.{$ifdef cpu64}Rdx{$else}Edx{$endif});
          if pos(firstchar+'di',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.{$ifdef cpu64}Rdi{$else}Edi{$endif});
          if pos(firstchar+'si',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.{$ifdef cpu64}Rsi{$else}Esi{$endif});
          if pos(firstchar+'bp',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.{$ifdef cpu64}Rbp{$else}Ebp{$endif});
          if pos(firstchar+'sp',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.{$ifdef cpu64}Rsp{$else}Esp{$endif});
          {$ifdef cpu64}
          if processhandler.is64Bit then
          begin
            if pos('r8',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.r8);
            if pos('r9',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.r9);
            if pos('r0',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.r10);
            if pos('r1',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.r11);
            if pos('r2',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.r12);
            if pos('r3',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.r13);
            if pos('r4',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.r14);
            if pos('r5',temp)>0 then maxregistervalue:=MaxX(maxregistervalue, pcontext(coderecord.context)^.r15);
          end;
          {$endif}

          //the offset is always at the end, so read from back to front
          //todo: use addresswatched



          temp2:='';
          for i:=length(temp) downto 1 do
            if temp[i] in ['0'..'9','a'..'f'] then temp2:=temp[i]+temp2 else break;

          if temp2<>'' then //I know this isn't completely correct e.g: [eax*4] but even then the 4 will NEVER be bigger than eax (unless it's to cause a crash)
          begin
            p:=StrToQWordEx('$'+temp2);
            if p>maxregistervalue then maxregistervalue:=p;
          end;

          formfoundcodelistextra.probably:=maxregistervalue;
        end;  //todo: arm
      end;
    end else formfoundcodelistextra.label17.caption:='';


    w:=formfoundcodelistextra.width;
    w:=max(w,formfoundcodelistextra.label1.width);
    w:=max(w,formfoundcodelistextra.label2.width);
    w:=max(w,formfoundcodelistextra.label3.width);
    w:=max(w,formfoundcodelistextra.label4.width);
    w:=max(w,formfoundcodelistextra.label5.width);

    if w<>formfoundcodelistextra.Width then
      formfoundcodelistextra.Width:=w+5;


    formfoundcodelistextra.context:=coderecord.contexthandler.getCopy(coderecord.context);

    if coderecord.stack.stack<>nil then
    begin
      getmem(formfoundcodelistextra.stack.stack, coderecord.stack.savedsize);
      formfoundcodelistextra.stack.savedsize:=coderecord.stack.savedsize;
      CopyMemory(formfoundcodelistextra.stack.stack, coderecord.stack.stack, coderecord.stack.savedsize);
    end;

    if coderecord.ipt.log<>nil then
    begin
      getmem(formfoundcodelistextra.ipt.log, coderecord.ipt.size);
      formfoundcodelistextra.ipt.size:=coderecord.ipt.size;
      copymemory(FormFoundCodeListExtra.ipt.log, coderecord.ipt.log, coderecord.ipt.size);
    end;

    FormFoundCodeListExtra.Show;
  end;
end;

procedure TFoundCodeDialog.FoundcodeListClick(Sender: TObject);
begin
end;

procedure TFoundCodeDialog.FormCreate(Sender: TObject);
var x: array of integer;
begin
  btnOk.caption:=strStop;

  setlength(x,0);
  if loadformposition(self,x) then
    loadedPosition:=true;

  if length(x)>=1 then
  begin
    foundcodelist.Columns[0].Width:=x[0];
    setcountwidth:=true;
  end;

  fdbvmwatchid:=-1;

  countsortdirection:=1;
  addresssortdirection:=1;

  seenAddressList:=tmap.Create(ituPtrSize,sizeof(pointer));
  seenAddressListCS:=TCriticalSection.Create;
end;

procedure TFoundCodeDialog.FormDeactivate(Sender: TObject);
begin

end;

procedure TFoundCodeDialog.FormDestroy(Sender: TObject);
var i: integer;
    cr: Tcoderecord;
    s: string;
begin
  stopDBVMWatch();

  for i:=0 to FoundCodeList.Items.count-1 do
  begin
    cr:=Tcoderecord(FoundCodeList.Items[i].data);

    if cr.formChangedAddresses<>nil then
    begin
      if usedmiFindWhatAccesses=false then
      begin
        MessageDlg('cr.formChangedAddresses is not nil but the function was never used. Fuck! cr.formChangedAddresses='+inttohex(ptruint(cr.formChangedAddresses),8), mtError,[mbok],0);
        exit;
      end;
      cr.formChangedAddresses.Close;
    end;

    FoundCodeList.Items[i].data:=nil;
    freeandnil(cr);
  end;



  FoundCodeList.Clear;
  timerEntryInfoUpdate.Enabled:=false;

  freeandnil(seenAddressList);
  freeandnil(seenAddressListCS);
end;

procedure TFoundCodeDialog.FormShow(Sender: TObject);
begin
  if not setcountwidth then
  begin
    FoundCodeList.Column[0].Width:=max(FoundCodeList.Column[0].Width, FoundCodeList.Canvas.TextWidth('9999'));
    setcountwidth:=true;
  end;

  if not loadedPosition then
  begin
    width:=canvas.TextWidth('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX');
    height:=canvas.TextHeight('Q')*20;
    FoundCodeList.Columns[0].Width:=canvas.TextWidth(' '+FoundCodeList.Columns[0].Caption+' ');
  end;
end;

procedure TFoundCodeDialog.FoundCodeListChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin

end;

procedure TFoundCodeDialog.btnOKClick(Sender: TObject);
var original: dword;
    i: integer;
begin
  if btnOK.caption=strStop then
  begin
    if debuggerthread<>nil then
      debuggerthread.CodeFinderStop(self);

    btnOK.caption:=strClose;

    stopdbvmwatch;
  end
  else close;


end;

procedure TFoundCodeDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  x: array of integer;
begin
  if miFindWhatAccesses.Checked then
    miFindWhatAccesses.Click; //unchecks it


  if btnOK.caption=strStop then
    if debuggerthread<>nil then
      debuggerthread.CodeFinderStop(self);



  if breakpoint<>nil then
  begin
    action:=caHide; //don't free. Only when the breakpoint is fully gone free it
    dec(pbreakpoint(breakpoint)^.referencecount);
  end
  else
    action:=caFree;

  setlength(x,1);
  x[0]:=FoundCodeList.Columns[0].Width;
  saveformposition(self,x);

  try
    //rename so that the next created dialog can have this name
    self.name:=self.name+'_tobedeleted'+inttohex(random(65535),4)+inttohex(random(65535),4)+inttohex(random(65535),4)+inttohex(random(65535),4)+'_'+inttohex(GetTickCount64,1);
  except
  end;
end;

procedure TFoundCodeDialog.btnReplacewithnopsClick(Sender: TObject);
var codelength: dword;
    written: dword;
    i,j: integer;
    nops: array of byte;
    a: dword;
    original: dword;

    mbi : _MEMORY_BASIC_INFORMATION;

    coderecord: TCodeRecord;
begin

  with foundcodelist do
  begin
    for j:=0 to foundcodelist.items.Count-1 do
    begin
      if foundcodelist.items[j].Selected then
      begin
        coderecord:=TcodeRecord(foundcodelist.items[j].data);
        codelength:=coderecord.size;
        //add it to the codelist

        try
          advancedoptions.AddToCodeList(coderecord.address,coderecord.size,false, foundcodelist.SelCount>1);
        except
          //already in the list, undo
        end;

        for i:=0 to AdvancedOptions.count-1 do
        begin
          advancedoptions.lvCodelist.ClearSelection;
          if symhandler.getAddressFromName(AdvancedOptions.entries[i].code.symbolname,false)=coderecord.address then
          begin
            advancedoptions.lvCodelist.selected:=advancedoptions.lvCodelist.Items[i];
            advancedoptions.lvCodelist.Items[i].Selected:=true;
            if AdvancedOptions.entries[i].code.changed then
              advancedoptions.miRestoreWithOriginalClick(advancedoptions.miRestoreWithOriginal)
            else
              advancedoptions.miReplaceWithNopsClick(advancedoptions.miReplaceWithNops);
          end;
        end;

      end;
    end;
  end;


end;

procedure TFoundCodeDialog.btnOpenDisassemblerClick(Sender: TObject);
var coderecord: TCodeRecord;
begin
  if foundcodelist.itemindex<>-1 then
  begin
    coderecord:=TcodeRecord(foundcodelist.items[foundcodelist.itemindex].data);
    memorybrowser.disassemblerview.SelectedAddress:=coderecord.address;
    memorybrowser.panel1.visible:=true;
    memorybrowser.show;
  end;
end;

procedure TFoundCodeDialog.btnAddToCodeListClick(Sender: TObject);
var i: integer;
    coderecord: TCodeRecord;
    added: boolean=false;
begin

  for i:=0 to foundcodelist.items.count-1 do
  begin
    if foundcodelist.items[i].Selected then
    begin
      coderecord:=TcodeRecord(foundcodelist.items[i].data);
      if advancedoptions.AddToCodeList(coderecord.address,coderecord.size,false, foundcodelist.SelCount>1) then
        added:=true;
    end;
  end;

  if added then
    advancedoptions.Show;

end;

function SortByAddress(Item1, Item2: TListItem; AOptionalParam: PtrInt): Integer stdcall;
begin
  result:=AOptionalParam*(TcodeRecord(Item1.data).address-TcodeRecord(Item2.data).address);

end;

function SortByCount(Item1, Item2: TListItem; AOptionalParam: PtrInt): Integer stdcall;
begin
  result:=AOptionalParam*(TcodeRecord(Item2.data).hitcount-TcodeRecord(Item1.data).hitcount);
end;

procedure TFoundCodeDialog.FoundCodeListColumnClick(Sender: TObject; Column: TListColumn);
begin
  case column.index of
    0:
    begin
      FoundcodeList.CustomSort(SortByCount,countsortdirection);
      countsortdirection:=countsortdirection*-1;
    end;

    1:
    begin
      FoundcodeList.CustomSort(SortByAddress,addresssortdirection);
      addresssortdirection:=addresssortdirection*-1;
    end;
  end;
end;

procedure TFoundCodeDialog.FoundcodeListDblClick(Sender: TObject);
begin
  MoreInfo;
end;

procedure TFoundCodeDialog.btnExtraInfoClick(Sender: TObject);
begin
  moreinfo;
end;

procedure TFoundCodeDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if btnOK.caption=strStop then btnOK.Click;
  CanClose:=true;
end;


procedure TFoundCodeDialog.FoundCodeListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var coderecord: TCodeRecord;
  selectedRecord: integer;
  i: integer;
begin
  minfo.clear;

  if foundcodelist.Selected<>nil then
  begin
    btnReplacewithnops.enabled:=true;
    btnOpenDisassembler.enabled:=true;
    btnAddToCodeList.enabled:=true;
    btnExtraInfo.Enabled:=true;

    coderecord:=TCodeRecord(foundcodelist.Selected.data);
    description.Caption:=coderecord.description;

    addinfo(coderecord);

    {
    for i:=0 to FoundCodeList.Items.Count-1 do
    begin
      if foundcodelist.items[i].Selected then
      begin
        coderecord:=TCodeRecord(foundcodelist.items[i].data);
        addinfo(coderecord);
      end;
    end;   }

    //minfo.VertScrollBar.Position:=0;
    minfo.SelStart:=0;
    minfo.SelLength:=0;
  end
  else
  begin
    btnReplacewithnops.enabled:=false;
    btnOpenDisassembler.enabled:=false;
    btnAddToCodeList.enabled:=false;
    btnExtraInfo.Enabled:=false;
    if foundcodelist.Items.Count=0 then
      description.caption:=rsUseTheGameApplicationForAWhile
    else
      description.caption:=rsSelectAnItemFromTheListForASmallDescription;


  end;
end;

procedure TFoundCodeDialog.MenuItem1Click(Sender: TObject);
var i: integer;
begin
  FoundCodeList.OnSelectItem:=nil;
  for i:=0 to foundcodelist.items.count-1 do
    FoundCodeList.Items[i].Selected:=true;

  FoundCodeList.OnSelectItem:=FoundCodeListSelectItem;
end;

procedure TFoundCodeDialog.miFindWhatAccessesClick(Sender: TObject);
var i: integer;
  coderecord: Tcoderecord;
  f: TfrmChangedAddresses;

begin
  usedmiFindWhatAccesses:=true;

  if miFindWhatAccesses.checked then
  begin
    if MessageDlg(rsFCThesWillSetASiftwareBreakpointInt3OnEverySingleOpcodeEtc,mtWarning, [mbyes,mbno],0)=mryes then
    begin
      //go through all existing entries and set a FindWhatCodeAccesses bp
      for i:=0 to FoundCodeList.Items.Count-1 do
      begin
        coderecord:=TCodeRecord(foundcodelist.items[i].data);
        coderecord.diffcount:=0;
        coderecord.formChangedAddresses:=debuggerthread.FindWhatCodeAccesses(coderecord.address, self);
      end;

      if FoundCodeList.Column[0].Width<FoundCodeList.Canvas.TextWidth('9999 (8)') then //make sure it's displayed
        FoundCodeList.Column[0].Width:=FoundCodeList.Canvas.TextWidth('9999 (8)');
    end
    else
      miFindWhatAccesses.checked:=false;
  end
  else
  begin
    //deactivate all bp's
    //go through bplist and remove all bp's
    for i:=0 to FoundCodeList.Items.Count-1 do
    begin
      coderecord:=TCodeRecord(foundcodelist.items[i].data);
      if coderecord.formChangedAddresses<>nil then
      begin
        coderecord.formChangedAddresses.Close; //brings reference count to 0, so will autodestroy
        coderecord.formChangedAddresses:=nil;
      end;
    end;

  end;
end;

procedure TFoundCodeDialog.miFindWhatCodeAccessesClick(Sender: TObject);
var a: ptruint;
begin
  if foundcodelist.ItemIndex<>-1 then
    MemoryBrowser.findWhatthisCodeAccesses(TcodeRecord(foundcodelist.items[foundcodelist.itemindex].data).address);
end;

function TFoundCodeDialog.getSelection:string;
var
  i: integer;
begin
  result:='';
  for i:=0 to FoundcodeList.Items.count-1 do
    if FoundcodeList.items[i].selected then
      result:=result+FoundcodeList.Items[i].subitems[0]+#13#10;
end;

procedure TFoundCodeDialog.miSaveTofileClick(Sender: TObject);
var
  s: TStringList;
begin
  if savedialog1.execute then
  begin
    s:=tstringlist.create;
    s.text:=getSelection;
    try
      s.SaveToFile(savedialog1.filename);
    finally
      freeandnil(s);
    end;
  end;
end;

procedure TFoundCodeDialog.pmOptionsPopup(Sender: TObject);
begin
  ReplacewithcodethatdoesnothingNOP1.Enabled:=foundcodelist.selcount>0;
  Showthisaddressinthedisassembler1.enabled:=foundcodelist.itemindex<>-1;
  Addtothecodelist1.enabled:=foundcodelist.selcount>0;
  MoreInfo1.Enabled:=foundcodelist.itemindex<>-1;
  miFindWhatCodeAccesses.enabled:=foundcodelist.itemindex<>-1;

  Copyselectiontoclipboard1.enabled:=foundcodelist.selcount>0;
  miSaveTofile.enabled:=Copyselectiontoclipboard1.Enabled;
end;

procedure TFoundCodeDialog.Copyselectiontoclipboard1Click(Sender: TObject);
begin
  clipboard.AsText:=getSelection;
end;

procedure TFoundCodeDialog.timerEntryInfoUpdateTimer(Sender: TObject);
var
  i: integer;
  starttime: qword;
  c: FoundCodeUnit.TCodeRecord;
  updateAddressString: boolean;

  news: string;
begin
  starttime:=GetTickCount64;
  updateAddressString:=true;
  for i:=0 to foundcodelist.Items.Count-1 do
  begin
    c:=FoundCodeUnit.TCodeRecord(foundcodelist.items[i].data);
    if c<>nil then
    begin
      if updateAddressString and (c.addressString='') then
        c.addressString:=symhandler.getNameFromAddress(c.address);

      if gettickcount64-starttime>250 then updateAddressString:=false; //next time better

      if miFindWhatAccesses.checked or (TCodeRecord(foundcodelist.Items[i].data).diffcount<>0) then
        news:=inttostr(c.hitcount)+' ('+inttostr(TCodeRecord(foundcodelist.Items[i].data).diffcount)+')'
      else
        news:=inttostr(c.hitcount);

      if FoundcodeList.items[i].caption<>news then
        FoundcodeList.items[i].caption:=news;

    end;
  end;

end;

initialization
  {$i FoundCodeUnit.lrs}

end.
