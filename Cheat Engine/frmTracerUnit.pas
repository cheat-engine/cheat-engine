unit frmTracerUnit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport, macportdefines,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  NewKernelHandler, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, LResources, commonTypeDefs, frmFindDialogUnit,
  Menus, ComCtrls, frmStackviewunit, frmFloatingPointPanelUnit, disassembler,
  debuggertypedefinitions, betterControls, LastDisassembleData;

type
  TTraceDebugInfo=class
  private
  public
    cr3:qword;
    instruction: string;
    instructionsize: integer;
    referencedAddress: ptrUint;
    c: _CONTEXT;
    bytes: pbytearray;
    bytesize: PtrUInt;
    isfloat: boolean;

    stack: record
      savedsize: PtrUInt;
      stack: pbyte;
    end;

    compareindex: integer;



    function datatype: TVariableType;
    procedure fillbytes(datasize: integer);
    procedure savestack;
    procedure saveToStream(s: tstream);
    constructor createFromStream(s: tstream);
    destructor destroy; override;
  end;

  TDBVMStatusUpdater=class(TPanel)
  private
    found: TLabel;
    progressbar: TProgressbar;
    cancelButton: TButton;
    timer: TTimer;
    procedure CancelAndGetResultClick(sender: TObject);
    procedure checkDBVMTracerStatus(sender: TObject);
  public
    OnTraceDone: TNotifyEvent;
    constructor Create(TheOwner: TComponent); override;
  end;

  { TfrmTracer }

  TfrmTracer = class(TForm)
    aflabel: TLabel;
    Button1: TButton;
    btnStopSearch: TButton;
    cflabel: TLabel;
    cbFPUXMM: TCheckBox;
    cbIgnoreStackPointers: TCheckBox;
    CSLabel: TLabel;
    dflabel: TLabel;
    DSLabel: TLabel;
    EAXLabel: TLabel;
    EBPlabel: TLabel;
    EBXlabel: TLabel;
    ECXlabel: TLabel;
    EDIlabel: TLabel;
    EDXlabel: TLabel;
    EIPlabel: TLabel;
    ESIlabel: TLabel;
    ESlabel: TLabel;
    ESPlabel: TLabel;
    FontDialog1: TFontDialog;
    FSlabel: TLabel;
    GSlabel: TLabel;
    ftImageList: TImageList;
    lblAddressed: TLabel;
    lblInstruction: TLabel;
    lvTracer: TTreeView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem7: TMenuItem;
    N1: TMenuItem;
    miRealignCompare: TMenuItem;
    miNewTrace: TMenuItem;
    miOpenTraceForCompare: TMenuItem;
    miSave: TMenuItem;
    miLoad: TMenuItem;
    miSaveToDisk: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    miSearchNext: TMenuItem;
    oflabel: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pnlRegisters: TPanel;
    pnlSegments: TPanel;
    pnlFlags: TPanel;
    Panel7: TPanel;
    pnlTracer: TPanel;
    pflabel: TLabel;
    pnlSearch: TPanel;
    pmTracer: TPopupMenu;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    SaveDialogText: TSaveDialog;
    sbShowFloats: TSpeedButton;
    sbShowstack: TSpeedButton;
    sflabel: TLabel;
    SSlabel: TLabel;
    Splitter1: TSplitter;
    zflabel: TLabel;
    procedure btnStopSearchClick(Sender: TObject);
    procedure cbFPUXMMChange(Sender: TObject);
    procedure cbIgnoreStackPointersChange(Sender: TObject);
    procedure lvTracerAdvancedCustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure lvTracerAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure lvTracerCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvTracerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure miNewTraceClick(Sender: TObject);
    procedure miOpenTraceForCompareClick(Sender: TObject);
    procedure miLoadClick(Sender: TObject);
    procedure miRealignCompareClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miSaveToDiskClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure RegisterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvTracerClick(Sender: TObject);
    procedure EAXLabelDblClick(Sender: TObject);
    procedure lvTracerDblClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure sbShowFloatsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbShowstackClick(Sender: TObject);
  private
    { Private declarations }
    Stackview: TfrmStackView;

    comparetv: TTreeView;

   // traceaddress: dword;
    fpp: TfrmFloatingPointPanel;
    isConfigured: boolean;
    dereference: boolean;
    fsavestack: boolean;

    RXlabels: array of TLabel;

    lastsearchstring: string;
    stopsearch: boolean;

    currentAppendage: TTreenode;


    fDataTrace: boolean;
    fSkipconfig: boolean;

    stepover: boolean;
    stepoverrep: boolean;
    nosystem: boolean;

    finddialog: TfrmFindDialog;

    loadedformpos: boolean;

    l1,l2: TTreeView;
    reallignscanaddress: ptruint;

    registerCompareIgnore: array [0..16] of boolean;

    da: TDisassembler;
    {$ifdef windows}
    dacr3: TCR3Disassembler;
    {$endif}

    defaultBreakpointMethod: TBreakpointmethod;

    physicaladdress: qword;
    DBVMStatusUpdater: TDBVMStatusUpdater;

    procedure configuredisplay;
    procedure setSavestack(x: boolean);
    procedure updatestackview;
    procedure cleanuptv(tv: TTreeview);
    procedure RealignTVSelectionChanged(sender: TObject);
    procedure RealignTVAddressScan(Sender: TCustomTreeView;
              Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
              var PaintImages, DefaultDraw: Boolean);

    function getEntry(index: integer): TTraceDebugInfo;
    function getCount: integer;
    function getSelectionCount: integer;
    procedure DBVMTraceDone(sender: TObject);
  public
    { Public declarations }
    returnfromignore: boolean;

    isdbvminterface: boolean; //faster than xxx is tdbvmdebuggerinterface
    LastDisassembleData: TLastDisassembleData;
    procedure setDataTrace(state: boolean);
    procedure addRecord;
    procedure finish;
    property savestack: boolean read fsavestack write setSavestack;
    property Entry[index: integer]: TTraceDebugInfo read getEntry;
    constructor create(Owner: TComponent; DataTrace: boolean=false; skipconfig: boolean=false); overload;
    constructor createWithBreakpointMethodSet(Owner: TComponent; DataTrace: boolean=false; skipconfig: boolean=false; breakpointmethod: tbreakpointmethod=bpmDebugRegister); overload;
  published
    property count: integer read getCount;
    property selectionCount: integer read getSelectionCount;
  end;

implementation


uses  LuaByteTable, clipbrd, CEDebugger, debughelper, MemoryBrowserFormUnit, frmTracerConfigUnit,
  ProcessHandlerUnit, Globals, Parsers, strutils, CEFuncProc,
  LuaHandler, symbolhandler, byteinterpreter,
  tracerIgnore, LuaForm, lua, lualib,lauxlib, LuaClass,vmxfunctions, DBK32functions,
  DebuggerInterfaceAPIWrapper, DBVMDebuggerInterface, mainunit2, fontSaveLoadRegistry,
  Registry;

resourcestring
  rsSearch = 'Search';
  rsTypeTheLUAConditionYouWantToSearchForExampleEAX0x1 = 'Type the (LUA) condition you want to search for (Example: EAX==0x1234)    '#13#10'Also available: referencedAddress (integer), referencedBytes (bytetable), instruction (string)';
  rsWaitingForTraceToStart = 'Waiting for trace to start';
  rsDBVMBreakAndTraceNeedsDBVM = 'DBVM Break and Trace needs DBVM. Loading '
    +'DBVM can potentially cause a system freeze. Are you sure?';

destructor TTraceDebugInfo.destroy;
begin
  if bytes<>nil then
    freememandnil(bytes);

  if stack.stack<>nil then
    freememandnil(stack.stack);

  inherited destroy;
end;

function TTraceDebugInfo.datatype: TVariableType;
begin
  if isfloat then
  begin
    case bytesize of
      4: exit(vtSingle);
      8: exit(vtDouble);
      10: ;//exit(vtExtended);
    end;
  end;

  case bytesize of
    1: result:=vtByte;
    2: result:=vtWord;
    else
      result:=FindTypeOfData(referencedAddress, bytes, bytesize);

  end;
end;

procedure TTraceDebugInfo.fillbytes(datasize: integer);
begin
  getmem(bytes, datasize);
  bytesize:=0;
  if cr3=0 then
    ReadProcessMemory(processhandle, pointer(referencedaddress), bytes, datasize, bytesize)
  {$ifdef windows}
  else
    ReadProcessMemoryCR3(cr3, pointer(referencedaddress), bytes, datasize, bytesize){$endif};
end;

procedure TTraceDebugInfo.SaveStack;
begin
  getmem(stack.stack, savedStackSize);

  if cr3=0 then
  begin
    if ReadProcessMemory(processhandle, pointer(c.{$ifdef cpu64}Rsp{$else}esp{$endif}), stack.stack, savedStackSize, stack.savedsize)=false then
    begin
      stack.savedsize:=4096-(c.{$ifdef cpu64}Rsp{$else}esp{$endif} mod 4096);
      ReadProcessMemory(processhandle, pointer(c.{$ifdef cpu64}Rsp{$else}esp{$endif}), stack.stack, stack.savedsize, stack.savedsize);
    end;
  end
  {$ifdef windows}
  else
    ReadProcessMemoryCR3(cr3, pointer(c.{$ifdef cpu64}Rsp{$else}esp{$endif}), stack.stack, savedStackSize, stack.savedsize){$endif};
end;

constructor TTraceDebugInfo.createFromStream(s: tstream);
var
  temp: dword;
  x: pchar;
begin
  s.ReadBuffer(temp, sizeof(temp));

  getmem(x, temp+1);
  s.readbuffer(x^, temp);
  x[temp]:=#0;
  instruction:=x;
  freememandnil(x);

  s.ReadBuffer(instructionsize, sizeof(instructionsize));
  s.ReadBuffer(referencedAddress, sizeof(referencedAddress));
  s.readBuffer(c, sizeof(c));
  s.ReadBuffer(bytesize, sizeof(bytesize));
  getmem(bytes, bytesize);
  s.readbuffer(bytes[0], bytesize);

  s.readbuffer(stack.savedsize, sizeof(stack.savedsize));
  getmem(stack.stack, stack.savedsize);
  s.readbuffer(stack.stack^, stack.savedsize);
end;

procedure TTraceDebugInfo.saveToStream(s: tstream);
var temp: dword;
begin
  //instruction: string;
  temp:=length(instruction);
  s.writebuffer(temp, sizeof(temp));
  s.WriteBuffer(instruction[1], temp);

  //instructionsize: integer;
  s.writebuffer(instructionsize, sizeof(instructionsize));

  //referencedAddress: ptrUint;
  s.WriteBuffer(referencedAddress, sizeof(referencedAddress));

  //c: _CONTEXT;
  s.WriteBuffer(c, sizeof(c));

  //bytesize: dword;
  s.WriteBuffer(bytesize, sizeof(bytesize));

  //bytes: pbytearray;
  s.writebuffer(bytes[0], bytesize);

  {stack: record
    savedsize: dword;
    stack: pbyte;
  end;}

  s.writebuffer(stack.savedsize, sizeof(stack.savedsize));
  s.writebuffer(stack.stack^, stack.savedsize);
end;


//----------------------TDBVMStatusUpdater-------------------------
procedure TDBVMStatusUpdater.CancelAndGetResultClick(sender: TObject);
begin
  dbvm_cloak_traceonbp_stoptrace;
  if assigned(OnTraceDone) then
    OnTraceDone(self);
end;

procedure TDBVMStatusUpdater.checkDBVMTracerStatus(sender: TObject);
var
  status: integer;
  count, max: dword;
  s: string;
begin
  OutputDebugString('checkDBVMTracerStatus');
  status:=dbvm_cloak_traceonbp_getstatus(count,max);

  OutputDebugString(format('status=%d count=%d max=%d',[status, count, max]));

  case status of
    0: s:='No trace active';
    1: s:='Trace ready to spring';
    2: s:='Trace activated and recording';
    3: s:='Trace finished recording';
    else s:='Unknown status';
  end;
  if status<>0 then
    s:=s+#13#10+format('%d/%d',[count, max]);

  if status=2 then
  begin
    if progressbar.style<>pbstNormal then
      progressbar.Style:=pbstNormal;

    if cancelbutton=nil then
    begin
      cancelButton:=tbutton.create(self);
      cancelbutton.caption:='Cancel and get results';
      cancelbutton.autosize:=true;
      cancelbutton.parent:=self;
      cancelbutton.AnchorSideTop.Control:=progressbar;
      cancelbutton.AnchorSideTop.side:=asrBottom;
      cancelbutton.AnchorSideLeft.control:=self;
      cancelbutton.AnchorSideLeft.side:=asrCenter;

      cancelbutton.BorderSpacing.top:=4;
      cancelbutton.BorderSpacing.bottom:=4;
      cancelbutton.OnClick:=CancelAndGetResultClick;
    end;

    if max<>0 then
    begin
      progressbar.position:=trunc((count/max)*100);
      found.caption:=format('%d/%d  (%d %%)', [count, max, progressbar.position]);
    end;
  end;


  if status=3 then //time to close
  begin
    progressbar.Position:=100;
    if assigned(OnTraceDone) then
      OnTraceDone(self);
  end;
end;

constructor TDBVMStatusUpdater.Create(TheOwner: TComponent);
begin
  inherited create(TheOwner);
  OutputDebugString('TDBVMStatusUpdater.create');
  OutputDebugString('create progressbar');
  progressbar:=TProgressBar.create(self);
  progressbar.parent:=self;
  progressbar.style:=pbstMarquee;

  OutputDebugString('create found label');
  found:=TLabel.create(self);
  found.caption:=rsWaitingForTraceToStart;
  found.parent:=self;
  found.WordWrap:=true;

  found.AnchorSideTop.control:=self;
  found.anchorsideTop.side:=asrTop;
  found.anchorsideleft.control:=self;
  found.anchorsideleft.side:=asrCenter;
  found.borderspacing.Top:=4;
  found.anchors:=[akleft,aktop,akright];

  progressbar.AnchorSideTop.control:=found;
  progressbar.anchorsideTop.side:=asrBottom;
  progressbar.anchorsideleft.control:=self;
  progressbar.anchorsideleft.side:=asrLeft;
  progressbar.anchorsideright.control:=self;
  progressbar.anchorsideright.side:=asrright;
  progressbar.borderspacing.Top:=4;
  progressbar.borderspacing.Bottom:=4;
  progressbar.anchors:=[akleft,aktop,akright];



  autosize:=true;

  OutputDebugString('create timer');
  Timer:=TTimer.create(self);
  Timer.Interval:=250;
  Timer.OnTimer:=checkDBVMTracerStatus;
  Timer.Enabled:=true;

  OutputDebugString('TDBVMStatusUpdater.create returned');
end;

//--------------------------TfrmTracer------------------------

constructor TfrmTracer.createWithBreakpointMethodSet(Owner: TComponent; DataTrace: boolean=false; skipconfig: boolean=false; breakpointmethod: tbreakpointmethod=bpmDebugRegister); overload;
begin
  inherited create(owner);
  fDataTrace:=Datatrace;
  fSkipConfig:=skipconfig;
  defaultBreakpointMethod:=breakpointmethod;
end;

constructor TfrmTracer.create(Owner: TComponent; DataTrace: boolean=false; skipconfig: boolean=false);
begin
  inherited create(owner);
  fDataTrace:=Datatrace;
  fSkipConfig:=skipconfig;
  defaultBreakpointMethod:=preferedBreakpointMethod;
end;

procedure TfrmTracer.finish;
begin
  //found a reason
  if lvtracer.items.count>0 then
  begin
    miOpenTraceForCompare.visible:=true;
    miOpenTraceForCompare.Enabled:=true;
  end;
end;

procedure TfrmTracer.addRecord;
var s,s2: string;
    i: integer;
    d: TTraceDebugInfo;

    a,address: ptrUint;
    referencedAddress: ptrUint;
    haserror: boolean;
    thisnode, thatnode,x: TTreenode;

    datasize: integer;
    isfloat: boolean;


    currentda: TDisassembler;
    cr3: qword;
begin
  //the debuggerthread is now paused so get the context and add it to the list

  try

    address:=debuggerthread.CurrentThread.context.{$ifdef CPU64}rip{$else}eip{$endif};

    a:=address;
    if da=nil then
    begin
      da:=tdisassembler.Create;
      da.showsymbols:=symhandler.showsymbols;
      da.showmodules:=symhandler.showmodules;
      da.showsections:=symhandler.showsections;
    end;

    {$ifdef windows}
    if isdbvminterface and (dacr3=nil) then
    begin
      dacr3:=tcr3disassembler.Create;
      dacr3.showsymbols:=symhandler.showsymbols;
      dacr3.showmodules:=symhandler.showmodules;
      dacr3.showsections:=symhandler.showsections;
    end;



    {$ifdef cpu64}
    if isdbvminterface and (debuggerthread.CurrentThread.context.P2Home<>0) then
    begin
      cr3:=debuggerthread.CurrentThread.context.P2Home;
      currentda:=dacr3;
      dacr3.CR3:=cr3;
    end
    else
    {$endif}
    {$endif}
    begin
      currentda:=da;
      cr3:=0;
    end;

    s:=currentda.disassemble(a, s2);

    lastDisassembleData:=currentda.LastDisassembleData;

    datasize:=currentda.LastDisassembleData.datasize;
    if datasize=0 then
      datasize:=4;

    isfloat:=currentda.LastDisassembleData.isfloat;

    referencedAddress:=0;
    if dereference then
    begin
      i:=pos('[',s)+1;
      if i>0 then
      begin
        s2:=copy(s,i,pos(']',s)-i);
        referencedAddress:=symhandler.getAddressFromName(s2, false, haserror, debuggerthread.CurrentThread.context);
      end;
    end;


    i:=posex('-',s);
    i:=posex('-',s,i+1);
    s:=copy(s,i+2,length(s));


    d:=TTraceDebugInfo.Create;
    d.cr3:=cr3;
    d.instructionsize:=a-address;
    d.c:=debuggerthread.CurrentThread.context^;
    d.instruction:=s;
    d.referencedAddress:=referencedAddress;
    d.isfloat:=isfloat;
    d.fillbytes(datasize);

    if savestack then
      d.savestack;

    s:=symhandler.getNameFromAddress(address)+' - '+s;

    if returnfromignore then
    begin
      //00500DD9
      returnfromignore:=false;
      if (currentAppendage<>nil) then
        currentAppendage:=currentAppendage.Parent;
    end;

    if currentAppendage<>nil then
      thisnode:=lvTracer.Items.AddChildObject(currentAppendage,s,d)
    else
      thisnode:=lvTracer.Items.AddObject(nil,s,d);

    if not stepover and currentda.LastDisassembleData.iscall then
      currentAppendage:=thisnode;

    if (currentda.LastDisassembleData.isret) {or returnfromignore} then
    begin
      returnfromignore:=false;
      if currentAppendage<>nil then
      begin
        currentAppendage:=currentAppendage.Parent;

        if currentAppendage<>nil then
        begin
          //check if the return is valid, could be it's a parent jump
          d:=TTraceDebugInfo(currentAppendage.Data);

          if d=nil then exit; //cleanup underway

          if (d.c.{$ifdef cpu64}Rip{$else}eip{$endif}+d.instructionsize<>a) then
          begin
            //see if a parent can be found that does match
            x:=currentappendage.Parent;
            while x<>nil do
            begin
              d:=TTraceDebugInfo(x.Data);

              if d=nil then exit; //cleanup underway

              if (d.c.{$ifdef cpu64}Rip{$else}eip{$endif}+d.instructionsize=a) then
              begin
                //match found
                currentAppendage:=x;
                exit;
              end;

              x:=x.parent;
            end;
          end;

        end;
      end
      else
      begin
        //create a node at the top and append the current top node to it
        thisnode:=lvTracer.items.AddFirst(nil,'');

        thatnode:=thisnode.GetNextSibling;
        while thatnode<>nil do
        begin
          thatnode.MoveTo(thisnode, naAddChild);
          thatnode:=thisnode.GetNextSibling;
        end;
      end;
    end;


  except
    on e: exception do
      OutputDebugString('tracer addRecord failed with error:'+e.Message);
  end;

end;

procedure TfrmTracer.setDataTrace(state: boolean);
begin
  fDataTrace:=state;
end;

procedure TfrmTracer.setSavestack(x: boolean);
begin
  fsavestack:=x;
  sbShowstack.visible:=x;
end;

function TfrmTracer.getEntry(index: integer): TTraceDebugInfo;
begin
  result:=nil;
  if (index>=0) and (index<count) then
    result:=TTraceDebugInfo(lvTracer.Items[index].Data);
end;

function TfrmTracer.getCount: integer;
begin
  result:=lvTracer.Items.count;
end;

function TfrmTracer.getSelectionCount: integer;
begin
  result:=lvTracer.Items.SelectionCount;
end;

procedure TfrmTracer.FormCreate(Sender: TObject);
var
  x: array of integer;
  reg: TRegistry;
begin
  //set a breakpoint and when that breakpoint gets hit trace a number of instructions
  setlength(x,0);
  loadedformpos:=loadformposition(self,x);

  if length(x)>1 then
    panel1.Width:=x[0];


  reg:=Tregistry.Create;
  try
    if reg.OpenKey('\Software\'+strCheatEngine+'\TracerTree '+inttostr(screen.PixelsPerInch)+'\Font'+darkmodestring,false) then
      LoadFontFromRegistry(lvtracer.Font, reg);
  except
  end;

  reg.free;

end;

procedure TfrmTracer.RegisterMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  s: string;
  i: integer;
  l: tlabel;
begin
  if button = mbright then
  begin
    if (sender is TLabel) then
    begin
      s:=tlabel(sender).Caption;
      i:=pos(' ',s);
      if i>0 then //should always be true
      begin
        s:=copy(s,i+1,length(s));
        clipboard.AsText:=s;
      end;
    end;
  end;

  if button = mbMiddle then
  begin
    if (sender is tlabel) then
    begin
      if tlabel(sender).tag=8 then exit; //not for eip/rip

      if fsItalic in tlabel(sender).font.style then
      begin
        tlabel(sender).font.style:=tlabel(sender).font.style-[fsItalic];
        registerCompareIgnore[tlabel(sender).tag]:=false;
      end
      else
      begin
        tlabel(sender).font.style:=tlabel(sender).font.style+[fsItalic];
        registerCompareIgnore[tlabel(sender).tag]:=true;
      end;

      lvTracer.refresh;
    end;
  end;
end;

procedure TfrmTracer.miSaveToDiskClick(Sender: TObject);
var
  z: Tstringlist;
  i: integer;
  t: TTraceDebugInfo;
  c: PContext;
  pref: string;
begin
  //save the results of the trace to disk
  if SaveDialogText.Execute then
  begin
    z:=tstringlist.create;
    try

      for i:=0 to lvTracer.Items.Count-1 do
      begin

        z.add(lvTracer.Items[i].Text);
        t:=TTraceDebugInfo(lvTracer.Items[i].data);
        if t<>nil then
        begin
          c:=@t.c;
          if processhandler.is64Bit then
            pref:='R'
          else
            pref:='E';


          if dereference then
          begin
            if t.referencedAddress<>0 then
              z.add(inttohex(t.referencedAddress,8)+' = '+DataToString(t.bytes, t.bytesize, t.datatype));
          end;




          z.add(pref+'AX='+inttohex(c.{$ifdef cpu64}Rax{$else}Eax{$endif},processhandler.hexdigitpreference));
          z.add(pref+'BX='+inttohex(c.{$ifdef cpu64}Rbx{$else}Ebx{$endif},processhandler.hexdigitpreference));
          z.add(pref+'CX='+inttohex(c.{$ifdef cpu64}Rcx{$else}Ecx{$endif},processhandler.hexdigitpreference));
          z.add(pref+'DX='+inttohex(c.{$ifdef cpu64}Rdx{$else}Edx{$endif},processhandler.hexdigitpreference));
          z.add(pref+'SI='+inttohex(c.{$ifdef cpu64}Rsi{$else}Esi{$endif},processhandler.hexdigitpreference));
          z.add(pref+'DI='+inttohex(c.{$ifdef cpu64}Rdi{$else}Edi{$endif},processhandler.hexdigitpreference));
          z.add(pref+'BP='+inttohex(c.{$ifdef cpu64}Rbp{$else}Ebp{$endif},processhandler.hexdigitpreference));
          z.add(pref+'SP='+inttohex(c.{$ifdef cpu64}Rsp{$else}Esp{$endif},processhandler.hexdigitpreference));
          z.add(pref+'IP='+inttohex(c.{$ifdef cpu64}Rip{$else}Eip{$endif},processhandler.hexdigitpreference));

          {$ifdef cpu64}
          if processhandler.is64bit then
          begin
            z.add('R8='+inttohex(c.r8,processhandler.hexdigitpreference));
            z.add('R9='+inttohex(c.r9,processhandler.hexdigitpreference));
            z.add('R10='+inttohex(c.r10,processhandler.hexdigitpreference));
            z.add('R11='+inttohex(c.r11,processhandler.hexdigitpreference));
            z.add('R12='+inttohex(c.r12,processhandler.hexdigitpreference));
            z.add('R13='+inttohex(c.r13,processhandler.hexdigitpreference));
            z.add('R14='+inttohex(c.r14,processhandler.hexdigitpreference));
            z.add('R15='+inttohex(c.r15,processhandler.hexdigitpreference));
          end;
          {$endif}

          z.add('');
          z.add('EFLAGS='+inttohex(c.EFlags,8));
          z.add('');
          z.add('-');


        end;
      end;

      z.SaveToFile(SaveDialogText.filename);
    finally
      z.free;
    end;
  end;
end;

procedure TfrmTracer.btnStopSearchClick(Sender: TObject);
begin
  stopsearch:=true;
end;

procedure TfrmTracer.cbFPUXMMChange(Sender: TObject);
begin
  lvtracer.Refresh;
end;

procedure TfrmTracer.cbIgnoreStackPointersChange(Sender: TObject);
begin
  lvTracer.Refresh;
end;

procedure TfrmTracer.lvTracerAdvancedCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin

end;

procedure TfrmTracer.lvTracerAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  thisinfo: TTraceDebugInfo;
  compareinfo: TTraceDebugInfo;

  ignorestack: boolean;

  different: boolean;
  xmmcount: integer;
begin
  if comparetv=nil then
  begin
    sender.BackgroundColor:=clWindow;
    sender.canvas.font.color:=clWindowText;
  end
  else
  begin
    thisinfo:=TTraceDebugInfo(node.data);
    compareinfo:=nil;

    if (thisinfo.compareindex>=0) and (thisinfo.compareindex<comparetv.Items.count) then
      compareinfo:=TTraceDebugInfo(comparetv.Items[thisinfo.compareindex].data);

    if compareinfo<>nil then
    begin
      if thisinfo.c.{$ifdef cpu64}Rip{$else}eip{$endif}<>compareinfo.c.{$ifdef cpu64}Rip{$else}eip{$endif} then
      begin
        sender.canvas.font.color:=clWindowText;
        sender.BackgroundColor:=clRed;
      end
      else
      begin
        ignorestack:=cbIgnoreStackPointers.checked;

        sender.BackgroundColor:=clWindow;
        //check the registers and flags

        different:=((not registerCompareIgnore[0]) and (compareinfo.c.{$ifdef cpu64}rax{$else}eax{$endif}<>thisinfo.c.{$ifdef cpu64}rax{$else}eax{$endif}))
        or ((not registerCompareIgnore[1]) and (compareinfo.c.{$ifdef cpu64}rbx{$else}ebx{$endif}<>thisinfo.c.{$ifdef cpu64}rbx{$else}ebx{$endif}))
        or ((not registerCompareIgnore[2]) and (compareinfo.c.{$ifdef cpu64}rcx{$else}ecx{$endif}<>thisinfo.c.{$ifdef cpu64}rcx{$else}ecx{$endif}))
        or ((not registerCompareIgnore[3]) and (compareinfo.c.{$ifdef cpu64}rdx{$else}edx{$endif}<>thisinfo.c.{$ifdef cpu64}rdx{$else}edx{$endif}))
        or ((not registerCompareIgnore[4]) and (ignorestack=false) and (compareinfo.c.{$ifdef cpu64}rbp{$else}ebp{$endif}<>thisinfo.c.{$ifdef cpu64}rbp{$else}ebp{$endif}))
        or ((not registerCompareIgnore[5]) and (ignorestack=false) and (compareinfo.c.{$ifdef cpu64}rsp{$else}esp{$endif}<>thisinfo.c.{$ifdef cpu64}rsp{$else}esp{$endif}))
        or ((not registerCompareIgnore[6]) and (compareinfo.c.{$ifdef cpu64}rsi{$else}esi{$endif}<>thisinfo.c.{$ifdef cpu64}rsi{$else}esi{$endif}))
        or ((not registerCompareIgnore[7]) and (compareinfo.c.{$ifdef cpu64}rdi{$else}edi{$endif}<>thisinfo.c.{$ifdef cpu64}rdi{$else}edi{$endif}))
        or ((compareinfo.c.EFlags and $cd5)<>(thisinfo.c.EFlags and $cd5)) //CD5 for the bits I am checking
        {$ifdef cpu64}
        or ((not registerCompareIgnore[9]) and (compareinfo.c.r8<>thisinfo.c.r8))
        or ((not registerCompareIgnore[10]) and (compareinfo.c.r9<>thisinfo.c.r9))
        or ((not registerCompareIgnore[11]) and (compareinfo.c.r10<>thisinfo.c.r10))
        or ((not registerCompareIgnore[12]) and (compareinfo.c.r11<>thisinfo.c.r11))
        or ((not registerCompareIgnore[13]) and (compareinfo.c.r12<>thisinfo.c.r12))
        or ((not registerCompareIgnore[14]) and (compareinfo.c.r13<>thisinfo.c.r13))
        or ((not registerCompareIgnore[15]) and (compareinfo.c.r14<>thisinfo.c.r14))
        or ((not registerCompareIgnore[16]) and (compareinfo.c.r15<>thisinfo.c.r15))
        {$endif};

        if (not different) and cbFPUXMM.checked then
        begin
          if processhandler.is64Bit then
            xmmcount:=16
          else
            xmmcount:=8;




          {$ifdef cpu64}
          different:=CompareMem(@compareinfo.c.FltSave.XmmRegisters[0], @thisinfo.c.FltSave.XmmRegisters[0], xmmcount*sizeof(M128A));
          {$else}
          different:=CompareMem(@compareinfo.c.ext.XMMRegisters[0], @thisinfo.c.ext.XMMRegisters[0], xmmcount*sizeof(M128A));
          {$endif}

          if not different then
          begin
            {$ifdef cpu64}
            different:=CompareMem(@compareinfo.c.FltSave.FloatRegisters[0], @thisinfo.c.FltSave.FloatRegisters[0], 8*sizeof(M128A));
            {$else}
            different:=CompareMem(@compareinfo.c.FloatSave.RegisterArea[0], @thisinfo.c.FloatSave.RegisterArea[0], 80);
            {$endif}
          end;

        end;

        if different then
          sender.canvas.font.color:=clRed
        else
          sender.canvas.font.color:=clWindowText;

      end;
    end
    else
    begin
      sender.BackgroundColor:=clWindow;
      sender.canvas.font.color:=clWindowText;
    end;
  end;


  DefaultDraw:=true;

end;

procedure TfrmTracer.lvTracerCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin

end;

procedure TfrmTracer.lvTracerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  //update the help based on what register is focused
end;

procedure TfrmTracer.MenuItem5Click(Sender: TObject);
var t: ttreenode;
begin
  t:=lvTracer.Items.GetFirstNode;
  while t<>nil do
  begin
    t.Expand(true);
    t:=t.GetNextSibling;
  end;
end;

procedure TfrmTracer.MenuItem6Click(Sender: TObject);
var t: ttreenode;
begin
  t:=lvTracer.Items.GetFirstNode;
  while t<>nil do
  begin
    t.Collapse(true);
    t:=t.GetNextSibling;
  end;
end;

procedure TfrmTracer.MenuItem7Click(Sender: TObject);
var reg: TRegistry;
begin
  fontdialog1.Font.assign(lvtracer.font);
  if fontdialog1.execute then
  begin
    lvtracer.font.assign(fontdialog1.font);

    reg:=Tregistry.Create;
    try
      if reg.OpenKey('\Software\'+strCheatEngine+'\TracerTree '+inttostr(screen.PixelsPerInch)+'\Font'+darkmodestring,true) then
        SaveFontToRegistry(lvTracer.Font, reg);
    except
    end;

    reg.free;
  end;
end;

procedure TfrmTracer.DBVMTraceDone(sender: TObject);
var
  desc: PTracerListDescriptor;
  listsize: integer;
  r: integer;
  count,max: dword;
  startwait: qword;

  list: PPageEventExtendedArray;
  stacklist: PPageEventExtendedWithStackArray absolute list;

  err: string;

  i: integer;
  desciterator: integer;
  s,s2: string;

  basic: PPageEventBasic;
  fpu: PFXSAVE64;
  stack: PByteArray;

  a: ptruint;
  d: TTraceDebugInfo;

  thisnode, thatnode, x: TTreenode;

begin
  OutputDebugString('DBVMTraceDone');
  dbvm_cloak_traceonbp_stoptrace;
  freeandnil(DBVMStatusUpdater);


  startwait:=gettickcount64; //in case the user canceled the trace
  while (dbvm_cloak_traceonbp_getstatus(count,max)<>3) and (gettickcount64<startwait+3000) do
    sleep(50);


  listsize:=0;
  r:=dbvm_cloak_traceonbp_readlog(nil,listsize);
  if r=2 then
  begin
    getmem(desc, listsize*2);
    lvTracer.Items.BeginUpdate;
    try
      r:=dbvm_cloak_traceonbp_readlog(desc, listsize);

      if r=0 then
      begin
        list:=PPageEventExtendedArray(qword(desc)+sizeof(TTracerListDescriptor));

        if da=nil then
        begin
          da:=tdisassembler.Create;
          da.showsymbols:=symhandler.showsymbols;
          da.showmodules:=symhandler.showmodules;
          da.showsections:=symhandler.showsections;
        end;

        for desciterator:=0 to desc.count-1 do
        begin
          if desc.datatype=1 then
          begin
            //extended
            basic:=@list[desciterator].basic;
            fpu:=@list[desciterator].fpudata;
            stack:=nil;
          end
          else
          begin
            //list with stack
            basic:=@stacklist[desciterator].basic;
            fpu:=@stacklist[desciterator].fpudata;
            stack:=@stacklist[desciterator].stack;
          end;

          //same as addrecord, but limited by what dbvm can do
          a:=basic^.RIP;
          s:=da.disassemble(a,s2);
          i:=posex('-',s);
          i:=posex('-',s,i+1);
          s:=copy(s,i+2,length(s));

          d:=TTraceDebugInfo.Create;
          d.instructionsize:=a-basic^.RIP;
          {$ifdef cpu64}
          d.c.P1Home:=basic^.FSBASE; //just using these field for storage
          d.c.p2home:=basic^.GSBASE;
          d.c.p3home:=basic^.CR3;
          {$endif}
          d.c.EFlags:=basic^.FLAGS;
          d.c.{$ifdef cpu32}Eax{$else}Rax{$endif}:=basic^.RAX;
          d.c.{$ifdef cpu32}Ebx{$else}Rbx{$endif}:=basic^.RBX;
          d.c.{$ifdef cpu32}Ecx{$else}Rcx{$endif}:=basic^.RCX;
          d.c.{$ifdef cpu32}Edx{$else}Rdx{$endif}:=basic^.RDX;
          d.c.{$ifdef cpu32}Esi{$else}Rsi{$endif}:=basic^.RSI;
          d.c.{$ifdef cpu32}Edi{$else}Rdi{$endif}:=basic^.RDI;
          {$ifdef cpu64}
          d.c.R8:=basic^.R8;
          d.c.R9:=basic^.R9;
          d.c.R10:=basic^.R10;
          d.c.R11:=basic^.R11;
          d.c.R12:=basic^.R12;
          d.c.R13:=basic^.R13;
          d.c.R14:=basic^.R14;
          d.c.R15:=basic^.R15;
          {$endif}
          d.c.{$ifdef cpu32}Ebp{$else}Rbp{$endif}:=basic^.RBP;
          d.c.{$ifdef cpu32}Esp{$else}Rsp{$endif}:=basic^.RSP;
          d.c.{$ifdef cpu32}Eip{$else}Rip{$endif}:=basic^.RIP;
          d.c.SegCs:=basic^.CS;
          d.c.SegDs:=basic^.DS;
          d.c.SegEs:=basic^.ES;
          d.c.SegSs:=basic^.SS;
          d.c.SegFs:=basic^.FS;
          d.c.SegGs:=basic^.GS;

          {$ifdef cpu64}
          copymemory(@d.c.FltSave, fpu,512);
          {$else}
          copymemory(@d.c.ext, fpu,512);
          {$endif}

          d.instruction:=s;
          d.referencedAddress:=0;
          d.isfloat:=false;
          d.bytes:=nil;
          d.bytesize:=0;

          if stack<>nil then
          begin
            getmem(d.stack.stack,4096);
            copymemory(d.stack.stack,stack,4096);
            d.stack.savedsize:=4096;
          end;

          s:=symhandler.getNameFromAddress(basic^.rip)+' - '+s;

          if returnfromignore then
          begin
            //00500DD9
            returnfromignore:=false;
            if (currentAppendage<>nil) then
              currentAppendage:=currentAppendage.Parent;
          end;

          if currentAppendage<>nil then
            thisnode:=lvTracer.Items.AddChildObject(currentAppendage,s,d)
          else
            thisnode:=lvTracer.Items.AddObject(nil,s,d);

          if not stepover and da.LastDisassembleData.iscall then
             currentAppendage:=thisnode;

          if (da.LastDisassembleData.isret) then
          begin
            returnfromignore:=false;
            if currentAppendage<>nil then
            begin
              currentAppendage:=currentAppendage.Parent;

              if currentAppendage<>nil then
              begin
                //check if the return is valid, could be it's a parent jump
                d:=TTraceDebugInfo(currentAppendage.Data);
                if (d.c.{$ifdef cpu64}Rip{$else}eip{$endif}+d.instructionsize<>a) then
                begin
                  //see if a parent can be found that does match
                  x:=currentappendage.Parent;
                  while x<>nil do
                  begin
                    d:=TTraceDebugInfo(x.Data);

                    if (d.c.{$ifdef cpu64}Rip{$else}eip{$endif}+d.instructionsize=a) then
                    begin
                      //match found
                      currentAppendage:=x;
                      break;
                    end;

                    x:=x.parent;
                  end;
                end;

              end;
            end
            else
            begin
              //create a node at the top and append the current top node to it
              thisnode:=lvTracer.items.AddFirst(nil,'');

              thatnode:=thisnode.GetNextSibling;
              while thatnode<>nil do
              begin
                thatnode.MoveTo(thisnode, naAddChild);
                thatnode:=thisnode.GetNextSibling;
              end;
            end;
          end;

        end;

      end
      else
      begin
        case r of
          4: err:='invalid address for buffer';
          6: err:='offset too high';
          else err:='unknown error '+inttostr(r);
        end;
        MessageDlg('Failure getting DBVM trace. : '+err, mtError,[mbok],0);
      end;
    finally
      freemem(desc);
      lvTracer.Items.EndUpdate;
    end;
  end
  else
    MessageDlg('Unexpected result from DBVM. dbvm_cloak_traceonbp_readlog returned '+inttostr(r), mtError,[mbok],0);

  //load the list
end;

procedure TfrmTracer.miNewTraceClick(Sender: TObject);
var tcount: integer;
    startcondition,stopcondition: string;
    testcontext: TContext;

    fromaddress: ptruint;
    toaddress: ptruint;

    bpTrigger: TBreakpointTrigger;
    b: byte;
    actual: SIZE_T;
    oldprotect: dword;
    v: boolean;
    r: integer;

    options: dword;
    count: integer;
    i: integer;

    oldpages: qword;
    newpages: qword;

    memneeded: integer;
    StayInsideModule: boolean;
begin
  if (owner is TMemoryBrowser) then
    fromaddress:=(owner as TMemoryBrowser).disassemblerview.SelectedAddress
  else
    fromaddress:=memorybrowser.disassemblerview.SelectedAddress;

  if frmTracerConfig=nil then
    frmTracerConfig:=TfrmTracerConfig.create(application);

  with frmTracerConfig do
  begin
    DataTrace:=fDataTrace;
    breakpointmethod:=defaultBreakpointMethod;

    cbStayInsideInitialModule.enabled:=symhandler.inModule(fromaddress);
    if cbStayInsideInitialModule.enabled=false then
      cbStayInsideInitialModule.checked:=false;

    if showmodal=mrok then
    begin

      currentAppendage:=nil;
      stopsearch:=false;

      if comparetv<>nil then
      begin
        cleanuptv(comparetv);
        freeandnil(comparetv);
      end;
      cleanuptv(lvtracer);
      lvTracer.items.Clear;


      ignoredModuleListHandler.LoadIgnoredModules;

      dereference:= cbDereferenceAddresses.checked;
      savestack:= cbSaveStack.checked;

      tcount:=strtoint(edtMaxTrace.text);
      startcondition:=edtStartCondition.text;
      stopcondition:=edtStopCondition.text;
      stepover:=cbStepOver.checked;
      stepoverrep:=cbStepOverRep.checked;
      nosystem:=cbSkipSystemModules.checked;

      StayInsideModule:=cbStayInsideInitialModule.checked;



      {$ifdef windows}
      if cbDBVMBreakAndTrace.checked then
      begin
        if loaddbvmifneeded(rsDBVMBreakAndTraceNeedsDBVM)=false then exit;


        //setup dbvm trace


        if cbDBVMTriggerCOW.checked then
        begin
          if ReadProcessMemory(processhandle, pointer(fromaddress), @b,1,actual) then
          begin
            v:=(SkipVirtualProtectEx=false) and VirtualProtectEx(processhandle, pointer(fromaddress),1, PAGE_EXECUTE_READWRITE, oldprotect);
            WriteProcessMemory(processhandle,pointer(fromaddress),@b,1,actual);
            if v then
              VirtualProtectEx(processhandle,pointer(fromaddress),1,oldprotect,oldprotect);
          end;
        end;

        options:=1;
        if cbSaveStack.checked then options:=3;

        if GetPhysicalAddress(processhandle,pointer(fromaddress), physicaladdress)=false then
          raise exception.create('Failure getting the physical address of this ');

        outputdebugstring(format('Calling dbvm_cloak_traceonbp(0x%x,%d,0x%x,0x%x)',[physicaladdress, tcount, options, fromaddress]));


        r:=dbvm_cloak_traceonbp(physicalAddress, tcount,options,fromaddress);
        count:=0;
        while (count<20) and (r<>0) do
        begin
          outputdebugstring('r='+inttostr(r)+' count='+inttostr(count));
          case r of
            1: raise exception.create('BP Cloak error');
            2:
            begin
              dbvm_cloak_traceonbp_stoptrace;

              if count>15 then  //takes too long, force it
                dbvm_cloak_traceonbp_remove(0,true);
            end;
            3:
            begin
              //need to allocate more memory
              if options=1 then memneeded:=sizeof(TTracerListDescriptor)+sizeof(TPageEventExtended)*tcount
              else memneeded:=sizeof(TTracerListDescriptor)+sizeof(TPageEventExtendedWithStack)*tcount;

              outputdebugstring('DBVM needs '+inttostr(memneeded)+' bytes free');

              memneeded:=1+(memneeded div 4096);
              dbvm_getMemory(oldpages);
              allocateMemoryForDBVM(memneeded);
              dbvm_getMemory(newpages);

              if newpages<=oldpages then raise exception.create('Failure allocating '+inttostr(memneeded)+' pages of physcal memory to DBVM');
            end;
          end;

          sleep(250);
          r:=dbvm_cloak_traceonbp(physicalAddress, tcount,options,fromaddress);
          inc(count);
        end;

        if r<>0 then
        begin
          outputdebugstring('r is still not 0. Error out');
          case r of
            1: raise exception.create('BP Cloak error');
            2: raise exception.create('Failure to kill previous trace');
            3: raise exception.create('Not enough DBVM memory free');
          end;
        end;

        //still here, trace activation was succesful

        OutputDebugString('after TDBVMStatusUpdater');
        DBVMStatusUpdater:=TDBVMStatusUpdater.create(Self);
        DBVMStatusUpdater.parent:=self;
        DBVMStatusUpdater.AnchorSideTop.control:=self;
        DBVMStatusUpdater.AnchorSideTop.side:=asrCenter;
        DBVMStatusUpdater.AnchorSideLeft.Control:=self;
        DBVMStatusUpdater.AnchorSideLeft.Side:=asrLeft;
        DBVMStatusUpdater.AnchorSideRight.Control:=self;
        DBVMStatusUpdater.AnchorSideRight.Side:=asrRight;
        DBVMStatusUpdater.anchors:=[aktop, akright, akLeft];
        DBVMStatusUpdater.OnTraceDone:=DBVMTraceDone;

        OutputDebugString('after config of DBVMStatusUpdater');

        OutputDebugString('calling checkDBVMTracerStatus');
        DBVMStatusUpdater.checkDBVMTracerStatus(nil);
        OutputDebugString('after checkDBVMTracerStatus');

      end
      else
      {$endif}
      if startdebuggerifneeded then
      begin
        isdbvminterface:=CurrentDebuggerInterface is TDBVMDebugInterface;

        if fDataTrace then
        begin
          //get breakpoint trigger
          if rbBreakOnAccess.checked then
            bpTrigger:=bptAccess
          else
            bpTrigger:=bptWrite;

          //get address to break on
          if (owner is TMemoryBrowser) then
            (owner as TMemoryBrowser).hexview.GetSelectionRange(fromaddress,toaddress)
          else
            memorybrowser.hexview.GetSelectionRange(fromaddress,toaddress);

          //set the breakpoint
          debuggerthread.setBreakAndTraceBreakpoint(self, fromaddress, bpTrigger, breakpointmethod, 1+(toaddress-fromaddress), tcount, startcondition, stopcondition, stepover, stepoverrep, nosystem);
        end
        else
        begin
          if (owner is TMemoryBrowser) then
            debuggerthread.setBreakAndTraceBreakpoint(self, (owner as TMemoryBrowser).disassemblerview.SelectedAddress, bptExecute, breakpointmethod, 1, tcount, startcondition, stopcondition, StepOver, stepoverrep, Nosystem, stayinsidemodule)
          else
            debuggerthread.setBreakAndTraceBreakpoint(self, memorybrowser.disassemblerview.SelectedAddress, bptExecute, breakpointmethod, 1, tcount, startcondition, stopcondition, StepOver, stepoverrep, nosystem, StayInsideModule);
        end;
      end;
    end;
  end;

  OutputDebugString('reached end of miNewTraceClick');
end;

procedure TfrmTracer.cleanuptv(tv: TTreeview);
//deletes the tracedebuginfo for all the lines
var
  i: integer;
  d: TTraceDebugInfo;

begin
  for i:=0 to tv.Items.Count-1 do
  begin
    if tv.Items[i].Data<>nil then
    begin
      d:=TTraceDebugInfo(tv.Items[i].Data);
      if d<>nil then
      begin
        tv.Items[i].Data:=nil;
        d.Free;
      end;
    end;
  end;
end;


procedure TfrmTracer.miOpenTraceForCompareClick(Sender: TObject);
var
  f: tfilestream;
  temp: dword;
  version: integer;
  m: TMemoryStream;
  i,j: integer;
begin
  if opendialog1.Execute then
  begin
    f:=TFileStream.create(opendialog1.filename, fmOpenRead or fmShareDenyNone);
    try
      f.ReadBuffer(temp, sizeof(temp));
      version:=temp;

      f.readbuffer(temp, sizeof(temp));

      m:=TMemoryStream.create;
      if temp>0 then
        m.CopyFrom(f, temp);

      m.Position:=0;

      if comparetv<>nil then
      begin
        cleanuptv(comparetv);
        freeandnil(comparetv);
      end;

      comparetv:=ttreeview.create(self);

      comparetv.LoadFromStream(m); //load the texttrace regardless of version
      m.free;

      if version<>{$ifdef cpu64}1{$else}0{$endif} then
        raise exception.create('This trace was made with the '+{$ifdef cpu64}'32'{$else}'64'{$endif}+'-bit version of '+strCheatEngine+'. You need to use that version to see the register values and stacktrace');

      for i:=0 to comparetv.Items.Count-1 do
      begin
        comparetv.Items[i].Data:=TTraceDebugInfo.createFromStream(f);
        if i<lvtracer.items.Count then
          TTraceDebugInfo(lvTracer.items[i].data).compareindex:=i;
      end;

      miRealignCompare.enabled:=true;
      miRealignCompare.visible:=true;

      {
      //nope
      width:=width+lvTracer.width;

      lvTracer.Align:=alNone;
      comparetv.parent:=pnlTracer;
      pnlTracer.ChildSizing.ControlsPerLine:=2;
      pnlTracer.ChildSizing.EnlargeHorizontal:=crsHomogenousChildResize;
      pnlTracer.ChildSizing.EnlargeVertical:=crsScaleChilds;
      pnlTracer.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
      cbLockScroll.visible:=true;
      }

      lvTracer.Refresh;
    finally
      f.free;
    end;
  end;
end;

procedure TfrmTracer.miLoadClick(Sender: TObject);
var
  f: tfilestream;
  temp: dword;
  version: integer;
  i: integer;
  m: TMemoryStream;
begin
  if opendialog1.execute then
  begin
    savestack:=true;

    f:=TFileStream.create(opendialog1.filename, fmOpenRead or fmShareDenyNone);
    try
      f.ReadBuffer(temp, sizeof(temp));
      version:=temp;
      if not (version in [0,1]) then
        raise exception.create('Unsupported tracefile');


      f.readbuffer(temp, sizeof(temp));

      m:=TMemoryStream.create;
      if temp>0 then
        m.CopyFrom(f, temp);

      m.Position:=0;
      cleanuptv(lvtracer);

      lvTracer.LoadFromStream(m); //load the texttrace regardless of version
      m.free;

      if version<>{$ifdef cpu64}1{$else}0{$endif} then
        raise exception.create('This trace was made with the '+{$ifdef cpu64}'32'{$else}'64'{$endif}+'-bit version of '+strCheatEngine+'. You need to use that version to see the register values and stacktrace');

      for i:=0 to lvTracer.Items.Count-1 do
        lvTracer.Items[i].Data:=TTraceDebugInfo.createFromStream(f);

      miOpenTraceForCompare.Enabled:=true;
      miOpenTraceForCompare.Visible:=true;
    finally
      f.free;
    end;
  end;
end;


procedure TfrmTracer.RealignTVSelectionChanged(sender: TObject);
var tdi, tdi2: TTraceDebugInfo;
  i: integer;
  index: integer;
begin
  if l1.Selected<>nil then
  begin
    tdi:=TTraceDebugInfo(l1.selected.data);
    reallignscanaddress:=tdi.c.{$ifdef cpu64}Rip{$else}eip{$endif};
    l2.refresh;

    if (tdi.compareindex<>-1) and (tdi.compareindex<l2.items.count) then
    begin
      tdi2:=TTraceDebugInfo(l2.Items[tdi.compareindex].data);
      if reallignscanaddress=tdi2.c.{$ifdef cpu64}Rip{$else}eip{$endif} then
      begin
        l2.Items[tdi.compareindex].MakeVisible;
        l2.refresh;
      end;
    end;
  end;
end;

procedure TfrmTracer.RealignTVAddressScan(Sender: TCustomTreeView;
              Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
              var PaintImages, DefaultDraw: Boolean);
begin
  defaultdraw:=true;
  if TTraceDebugInfo(node.data).c.{$ifdef cpu64}Rip{$else}eip{$endif}=reallignscanaddress then
  begin
    sender.BackgroundColor:=clGreen;
    sender.canvas.font.color:=clWhite;
  end
  else
  begin
    sender.BackgroundColor:=clWindow;
    sender.canvas.font.color:=clWindowText;
  end;
end;

procedure TfrmTracer.miRealignCompareClick(Sender: TObject);
var
  frmrealign: TCustomForm;
  description: TLabel;
  tracerpanel: TPanel;
  btnpanel: TPanel;
  select: tbutton;


  i: integer;
  newindex: integer;
  adjustment: integer;
begin
  //show the two treeviews side by side and let the user pick parts that match (also use color highlighting to show inconsistencies)
  frmRealign:=TCustomForm.CreateNew(self);
  frmrealign.caption:='Realign compare';
  frmrealign.position:=poScreenCenter;

  description:=tlabel.create(frmrealign);
  description.caption:='Select two spots that are the same address (It is possible the address does not exist. In that case, pick another. Tip: function end)';
  description.WordWrap:=true;
  description.Align:=alTop;
  description.parent:=frmrealign;

  tracerpanel:=tpanel.create(frmrealign);
  tracerpanel.align:=alClient;
  tracerpanel.parent:=frmrealign;

  tracerpanel.ChildSizing.ControlsPerLine:=2;
  tracerpanel.ChildSizing.layout:=cclLeftToRightThenTopToBottom;
  tracerpanel.ChildSizing.EnlargeVertical:=crsScaleChilds;
  tracerpanel.ChildSizing.EnlargeHorizontal:=crsHomogenousChildResize;

  l1:=ttreeview.Create(frmrealign);
  l1.readonly:=true;
  l1.parent:=tracerpanel;
  l1.Items.Assign(lvTracer.Items);
  for i:=0 to l1.items.count-1 do
  begin
    l1.items[i].Data:=lvtracer.Items[i].data;
    if l1.Items[i].HasChildren then
      l1.items[i].Expand(false);
  end;

  l1.Options:=l1.Options-[tvoThemedDraw];
  l1.OnAdvancedCustomDrawItem:=lvTracerAdvancedCustomDrawItem;
  l1.OnSelectionChanged:=RealignTVSelectionChanged;

  l2:=ttreeview.Create(frmrealign);
  l2.readonly:=true;
  l2.parent:=tracerpanel;
  l2.items.assign(comparetv.items);
  l2.Options:=l2.Options-[tvoThemedDraw];
  l2.OnAdvancedCustomDrawItem:=RealignTVAddressScan;

  for i:=0 to l2.items.count-1 do
  begin
    l2.items[i].data:=comparetv.items[i].data;
    if l2.Items[i].HasChildren then
      l2.items[i].Expand(false);
  end;

  btnpanel:=tpanel.create(frmrealign);
  btnpanel.align:=albottom;
  btnpanel.parent:=frmrealign;
  btnpanel.autosize:=true;
  btnpanel.BorderSpacing.Top:=4;
  btnpanel.BorderSpacing.Bottom:=4;


  select:=tbutton.create(frmrealign);
  select.parent:=btnpanel;
  select.AnchorSideLeft.Control:=btnpanel;
  select.AnchorSideLeft.Side:=asrCenter;
  select.modalresult:=mrok;
  select.caption:='Match up';
  select.autosize:=true;

  //todo: Try to select an initial guess
  //frmrealign;

  frmrealign.width:=frmrealign.canvas.TextWidth('XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXX');

  if frmrealign.ShowModal=mrok then
  begin
    if (l1.Selected=nil) or (l2.selected=nil) then
    begin
      errorbeep;
      freeandnil(l1);
      freeandnil(l2);
      exit;
    end;

    //shift the compareindex from l1 to the selected entry in l2 and further
    newindex:=l2.selected.AbsoluteIndex;
    adjustment:=newindex-l1.selected.AbsoluteIndex;
    for i:=l1.selected.AbsoluteIndex to l1.items.count-1 do
      inc(TTraceDebugInfo(l1.items[i].data).compareindex, adjustment);

    lvtracer.Refresh;

    freeandnil(l1);
    freeandnil(l2);
  end;
end;

procedure TfrmTracer.miSaveClick(Sender: TObject);
var
  i: integer;
  t: TTraceDebugInfo;
  c: PContext;
  pref: string;

  f: tfilestream;

  m: tmemorystream;
  temp: dword;
  emptytracedebug: TTraceDebugInfo;
begin
  //save the results of the trace to disk
  if savedialog1.Execute then
  begin
    emptytracedebug:=TTraceDebugInfo.Create;
    f:=tfilestream.create(savedialog1.filename, fmCreate);
    try
      //save if it's made with the 32 or 64-bit ce version (the 32-bit ce version can not load the 64-bit version and vice versa)
      {$ifdef cpu64}
      temp:=1;
      {$else}
      temp:=0;
      {$endif}
      f.writebuffer(temp, sizeof(temp));

      m:=tmemorystream.create;
      lvTracer.SaveToStream(m);

      temp:=m.Size;
      f.writebuffer(temp, sizeof(temp));
      m.SaveToStream(f);
      m.free;



      for i:=0 to lvTracer.Items.Count-1 do
      begin
        t:=TTraceDebugInfo(lvTracer.Items[i].data);
        if t<>nil then
          t.saveToStream(f)
        else
          emptytracedebug.saveToStream(f);
      end;

      beep;
    finally
      f.free;
      emptytracedebug.free;
    end;
  end;
end;

function bytesToLuaByteTableString(bytes: PByteArray; size: integer): string;
var i: integer;
begin
  result:='{';
  for i:=0 to size-1 do
  begin
    if i>0 then
      result:=result+','+inttostr(bytes[i])
    else
      result:=result+inttostr(bytes[i]);
  end;

  result:=result+'}';
end;

procedure TfrmTracer.MenuItem4Click(Sender: TObject);
var
  i: integer;
  c: PContext;
  check: boolean;
  searchstring: string;

  script: tstringlist=nil;
  usesReferencedAddress: boolean=false;
  usesReferencedBytes: boolean=false;
  usesInstruction: boolean=false;
begin
  if finddialog=nil then
  begin
    finddialog:=TfrmFindDialog.create(Self);
    finddialog.ShowCaseSensitive:=false;
  end;

  if (sender = miSearchNext) then
    check:=true
  else
  begin
    finddialog.FindText:=lastsearchstring;
    finddialog.Description:=rsTypeTheLUAConditionYouWantToSearchForExampleEAX0x1;
    check:=finddialog.Execute;
//    check:=InputQuery(rsSearch, rsTypeTheLUAConditionYouWantToSearchForExampleEAX0x1, lastsearchstring);
    lastsearchstring:=finddialog.FindText;
  end;


  if check then
  begin
    searchstring:='return '+lastsearchstring;
    usesReferencedAddress:=searchstring.Contains('referencedAddress'); //screw the user if he uses comments
    usesReferencedBytes:=searchstring.Contains('referencedBytes');
    usesInstruction:=searchstring.Contains('instruction');

    script:=tstringlist.Create;
    script.Text:=searchstring;

    script.Insert(0,''); //space for referencedAddress
    script.Insert(1,''); //space for referencedBytes
    script.Insert(2,''); //space for instruction



    stopsearch:=false;
    progressbar1.Position:=0;
    progressbar1.Max:=lvTracer.items.Count;
    pnlSearch.visible:=true;

    if finddialog.direction=fdDown then
    begin
      if lvTracer.Selected=nil then
        i:=0
      else
        i:=lvTracer.Selected.AbsoluteIndex+1;



      while (i<lvTracer.items.count) and (not stopsearch) do
      begin
        c:=@TTraceDebugInfo(lvTracer.Items[i].data).c;
        if c<>nil then
        begin
          if usesReferencedAddress then
            script[0]:='local referencedAddress=0x'+inttohex(TTraceDebugInfo(lvTracer.Items[i].data).referencedAddress,8);

          if usesReferencedBytes then
            script[1]:='local referencedBytes='+bytesToLuaByteTableString(TTraceDebugInfo(lvTracer.Items[i].data).bytes, TTraceDebugInfo(lvTracer.Items[i].data).bytesize);

          if usesInstruction then
            script[2]:='local instruction=[['+TTraceDebugInfo(lvTracer.Items[i].data).instruction+' ]]';

          if CheckIfConditionIsMetContext(0, c, script.text) then
          begin
            lvTracer.Items[i].Selected:=true;
            lvTracer.MakeSelectionVisible;
            lvTracerClick(lvTracer);
            break;
          end;


          inc(i);
          if (i mod 50)=0 then application.ProcessMessages;

        end;
      end;
    end
    else
    begin
      if lvTracer.Selected=nil then
        i:=lvtracer.items.count-1
      else
        i:=lvTracer.Selected.AbsoluteIndex-1;

      while (i>=0) and (not stopsearch) do
      begin
        c:=@TTraceDebugInfo(lvTracer.Items[i].data).c;
        if c<>nil then
        begin
          if usesReferencedAddress then
            script[0]:='local referencedAddress=0x'+inttohex(TTraceDebugInfo(lvTracer.Items[i].data).referencedAddress,8);

          if usesReferencedBytes then
            script[1]:='local referencedBytes='+bytesToLuaByteTableString(TTraceDebugInfo(lvTracer.Items[i].data).bytes, TTraceDebugInfo(lvTracer.Items[i].data).bytesize);

          if usesInstruction then
            script[2]:='local instruction=[['+TTraceDebugInfo(lvTracer.Items[i].data).instruction+']]';

          if CheckIfConditionIsMetContext(0, c, script.text) then
          begin
            lvTracer.Items[i].Selected:=true;
            lvTracer.MakeSelectionVisible;
            lvTracerClick(lvTracer);
            break;
          end;


          dec(i);
          if (i mod 50)=0 then application.ProcessMessages;

        end;
      end;
    end;

    pnlSearch.visible:=false;
  end;

  if script<>nil then
    freeandnil(script);
end;

procedure TfrmTracer.Panel1Click(Sender: TObject);
begin

end;



procedure TfrmTracer.FormClose(Sender: TObject; var Action: TCloseAction);
var i: integer;
begin

  if debuggerthread<>nil then
    debuggerthread.stopBreakAndTrace(self);

  if comparetv<>nil then
  begin
    cleanuptv(comparetv);
    freeandnil(comparetv);
  end;

  cleanuptv(lvTracer);

  action:=cafree; //if still buggy, change to cahide

  if DBVMStatusUpdater<>nil then
  begin
    dbvm_cloak_traceonbp_stoptrace;
    freeandnil(DBVMStatusUpdater);
  end;

  if physicaladdress<>0 then
    dbvm_cloak_traceonbp_remove(physicaladdress);
end;

procedure TfrmTracer.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmTracer.FormDestroy(Sender: TObject);
var x: array of integer;
begin

  if da<>nil then
    da.free;

  {$ifdef windows}
  if dacr3<>nil then
    dacr3.free;
  {$endif}

  setlength(x,1);
  x[0]:=panel1.width;

  saveformposition(self,x);
end;

procedure TfrmTracer.configuredisplay;
{
if the process is 64-bit create r8-r15 and move all objects closer
}
var
  i: integer;
  l: tlabel;
begin
  if not isConfigured then
  begin
    if processhandler.is64Bit then
    begin
      setlength(RXlabels,8);

      for i:=8 to 15 do
      begin
        l:=tlabel.create(self);
        RXlabels[i-8]:=l;

        with l do
        begin
          parent:=pnlRegisters;
          font:=eaxlabel.font;
          parentfont:=true;
          cursor:=eaxlabel.cursor;

          tag:=i+1;
          OnDblClick:=EAXLabelDblClick;
          OnMouseDown:=RegisterMouseDown;
        end;
      end;

      eiplabel.BringToFront;

    end;

    isConfigured:=true;
  end;
end;

procedure TfrmTracer.lvTracerClick(Sender: TObject);
var temp: string;
    context: _context;
    t: TTraceDebugInfo;
    prefix: char;

    t2: TTraceDebugInfo;
begin
  configuredisplay;

  if lvTracer.selected<>nil then
  begin
    t:=TTraceDebugInfo(lvTracer.selected.data);
    if t<>nil then
    begin
      t2:=nil;
      if (comparetv<>nil) and (t.compareindex<>-1) and (t.compareindex<comparetv.Items.count) then
        t2:=TTraceDebugInfo(comparetv.items[t.compareindex].Data);

      lblinstruction.caption:=t.instruction;
      if dereference then
      begin
        if t.referencedAddress<>0 then
          lblAddressed.caption:=inttohex(t.referencedAddress,8)+' = '+DataToString(t.bytes, t.bytesize, t.datatype)
        else
          lblAddressed.caption:=' ';
      end else lblAddressed.Caption:=' ';


      context:=t.c;

      if processhandler.is64bit then
        prefix:='R'
      else
        prefix:='E';

      temp:=prefix+'AX '+IntToHex(context.{$ifdef cpu64}rax{$else}Eax{$endif},processhandler.hexdigitpreference);
      if (t2<>nil) and (t.c.{$ifdef cpu64}rax{$else}Eax{$endif}<>t2.c.{$ifdef cpu64}rax{$else}Eax{$endif}) then temp:=temp+' <> '+IntToHex(t2.c.{$ifdef cpu64}rax{$else}Eax{$endif},processhandler.hexdigitpreference);
      if temp<>eaxlabel.Caption then
      begin
        eaxlabel.Font.Color:=clred;
        eaxlabel.Caption:=temp;
      end else eaxlabel.Font.Color:=clWindowText;

      temp:=prefix+'BX '+IntToHex(context.{$ifdef cpu64}rbx{$else}ebx{$endif},processhandler.hexdigitpreference);
      if (t2<>nil) and (t.c.{$ifdef cpu64}rbx{$else}Ebx{$endif}<>t2.c.{$ifdef cpu64}rbx{$else}Ebx{$endif}) then temp:=temp+' <> '+IntToHex(t2.c.{$ifdef cpu64}rbx{$else}Ebx{$endif},processhandler.hexdigitpreference);
      if temp<>ebxlabel.Caption then
      begin
        ebxlabel.Font.Color:=clred;
        ebxlabel.Caption:=temp;
      end else ebxlabel.Font.Color:=clWindowText;

      temp:=prefix+'CX '+IntToHex(context.{$ifdef cpu64}rcx{$else}ecx{$endif},processhandler.hexdigitpreference);
      if (t2<>nil) and (t.c.{$ifdef cpu64}rcx{$else}Ecx{$endif}<>t2.c.{$ifdef cpu64}rcx{$else}Ecx{$endif}) then temp:=temp+' <> '+IntToHex(t2.c.{$ifdef cpu64}rcx{$else}Ecx{$endif},processhandler.hexdigitpreference);
      if temp<>eCxlabel.Caption then
      begin
        eCXlabel.Font.Color:=clred;
        eCXlabel.Caption:=temp;
      end else eCXlabel.Font.Color:=clWindowText;

      temp:=prefix+'DX '+IntToHex(context.{$ifdef cpu64}rdx{$else}edx{$endif},processhandler.hexdigitpreference);
      if (t2<>nil) and (t.c.{$ifdef cpu64}rdx{$else}Edx{$endif}<>t2.c.{$ifdef cpu64}rdx{$else}Edx{$endif}) then temp:=temp+' <> '+IntToHex(t2.c.{$ifdef cpu64}rdx{$else}Edx{$endif},processhandler.hexdigitpreference);
      if temp<>eDxlabel.Caption then
      begin
        eDxlabel.Font.Color:=clred;
        eDxlabel.Caption:=temp;
      end else eDxlabel.Font.Color:=clWindowText;

      temp:=prefix+'SI '+IntToHex(context.{$ifdef cpu64}rsi{$else}esi{$endif},processhandler.hexdigitpreference);
      if (t2<>nil) and (t.c.{$ifdef cpu64}rsi{$else}esi{$endif}<>t2.c.{$ifdef cpu64}rsi{$else}Esi{$endif}) then temp:=temp+' <> '+IntToHex(t2.c.{$ifdef cpu64}rsi{$else}Esi{$endif},processhandler.hexdigitpreference);
      if temp<>eSIlabel.Caption then
      begin
        eSIlabel.Font.Color:=clred;
        eSIlabel.Caption:=temp;
      end else eSIlabel.Font.Color:=clWindowText;

      temp:=prefix+'DI '+IntToHex(context.{$ifdef cpu64}rdi{$else}edi{$endif},processhandler.hexdigitpreference);
      if (t2<>nil) and (t.c.{$ifdef cpu64}rdi{$else}edi{$endif}<>t2.c.{$ifdef cpu64}rdi{$else}Edi{$endif}) then temp:=temp+' <> '+IntToHex(t2.c.{$ifdef cpu64}rdi{$else}Edi{$endif},processhandler.hexdigitpreference);
      if temp<>eDIlabel.Caption then
      begin
        eDIlabel.Font.Color:=clred;
        eDIlabel.Caption:=temp;
      end else eDIlabel.Font.Color:=clWindowText;

      temp:=prefix+'BP '+IntToHex(context.{$ifdef cpu64}rbp{$else}ebp{$endif},processhandler.hexdigitpreference);
      if (t2<>nil) and (t.c.{$ifdef cpu64}rbp{$else}ebp{$endif}<>t2.c.{$ifdef cpu64}rbp{$else}Ebp{$endif}) then temp:=temp+' <> '+IntToHex(t2.c.{$ifdef cpu64}rbp{$else}Ebp{$endif},processhandler.hexdigitpreference);
      if temp<>eBPlabel.Caption then
      begin
        eBPlabel.Font.Color:=clred;
        eBPlabel.Caption:=temp;
      end else eBPlabel.Font.Color:=clWindowText;

      temp:=prefix+'SP '+IntToHex(context.{$ifdef cpu64}rsp{$else}esp{$endif},processhandler.hexdigitpreference);
      if (t2<>nil) and (t.c.{$ifdef cpu64}rsp{$else}esp{$endif}<>t2.c.{$ifdef cpu64}rsp{$else}Esp{$endif}) then temp:=temp+' <> '+IntToHex(t2.c.{$ifdef cpu64}rsp{$else}Esp{$endif},processhandler.hexdigitpreference);
      if temp<>eSPlabel.Caption then
      begin
        eSPlabel.Font.Color:=clred;
        eSPlabel.Caption:=temp;
      end else eSPlabel.Font.Color:=clWindowText;

      temp:=prefix+'IP '+IntToHex(context.{$ifdef cpu64}rip{$else}eip{$endif},processhandler.hexdigitpreference);
      if (t2<>nil) and (t.c.{$ifdef cpu64}rip{$else}eip{$endif}<>t2.c.{$ifdef cpu64}rip{$else}Eip{$endif}) then temp:=temp+' <> '+IntToHex(t2.c.{$ifdef cpu64}rip{$else}Eip{$endif},processhandler.hexdigitpreference);
      if temp<>eIPlabel.Caption then
      begin
        eIPlabel.Font.Color:=clred;
        eIPlabel.Caption:=temp;
      end else eIPlabel.Font.Color:=clWindowText;

      {$ifdef cpu64}

      if length(rxlabels)>0 then
      begin

        temp:='R8  '+IntToHex(context.r8,processhandler.hexdigitpreference);
        if (t2<>nil) and (t.c.r8<>t2.c.r8) then temp:=temp+' <> '+IntToHex(t2.c.r8,processhandler.hexdigitpreference);
        if temp<>RXlabels[0].Caption then
        begin
          RXlabels[0].Font.Color:=clred;
          RXlabels[0].Caption:=temp;
        end else RXlabels[0].Font.Color:=clWindowText;

        temp:='R9  '+IntToHex(context.r9,processhandler.hexdigitpreference);
        if (t2<>nil) and (t.c.r9<>t2.c.r9) then temp:=temp+' <> '+IntToHex(t2.c.r9,processhandler.hexdigitpreference);
        if temp<>RXlabels[1].Caption then
        begin
          RXlabels[1].Font.Color:=clred;
          RXlabels[1].Caption:=temp;
        end else RXlabels[1].Font.Color:=clWindowText;

        temp:='R10 '+IntToHex(context.r10,processhandler.hexdigitpreference);
        if (t2<>nil) and (t.c.r10<>t2.c.r10) then temp:=temp+' <> '+IntToHex(t2.c.r10,processhandler.hexdigitpreference);
        if temp<>RXlabels[2].Caption then
        begin
          RXlabels[2].Font.Color:=clred;
          RXlabels[2].Caption:=temp;
        end else RXlabels[2].Font.Color:=clWindowText;

        temp:='R11 '+IntToHex(context.r11,processhandler.hexdigitpreference);
        if (t2<>nil) and (t.c.r11<>t2.c.r11) then temp:=temp+' <> '+IntToHex(t2.c.r11,processhandler.hexdigitpreference);
        if temp<>RXlabels[3].Caption then
        begin
          RXlabels[3].Font.Color:=clred;
          RXlabels[3].Caption:=temp;
        end else RXlabels[3].Font.Color:=clWindowText;

        temp:='R12 '+IntToHex(context.r12,processhandler.hexdigitpreference);
        if (t2<>nil) and (t.c.r12<>t2.c.r12) then temp:=temp+' <> '+IntToHex(t2.c.r12,processhandler.hexdigitpreference);
        if temp<>RXlabels[4].Caption then
        begin
          RXlabels[4].Font.Color:=clred;
          RXlabels[4].Caption:=temp;
        end else RXlabels[4].Font.Color:=clWindowText;

        temp:='R13 '+IntToHex(context.r13,processhandler.hexdigitpreference);
        if (t2<>nil) and (t.c.r13<>t2.c.r13) then temp:=temp+' <> '+IntToHex(t2.c.r13,processhandler.hexdigitpreference);
        if temp<>RXlabels[5].Caption then
        begin
          RXlabels[5].Font.Color:=clred;
          RXlabels[5].Caption:=temp;
        end else RXlabels[5].Font.Color:=clWindowText;

        temp:='R14 '+IntToHex(context.r14,processhandler.hexdigitpreference);
        if (t2<>nil) and (t.c.r14<>t2.c.r14) then temp:=temp+' <> '+IntToHex(t2.c.r14,processhandler.hexdigitpreference);
        if temp<>RXlabels[6].Caption then
        begin
          RXlabels[6].Font.Color:=clred;
          RXlabels[6].Caption:=temp;
        end else RXlabels[6].Font.Color:=clWindowText;

        temp:='R15 '+IntToHex(context.r15,processhandler.hexdigitpreference);
        if (t2<>nil) and (t.c.r15<>t2.c.r15) then temp:=temp+' <> '+IntToHex(t2.c.r15,processhandler.hexdigitpreference);
        if temp<>RXlabels[7].Caption then
        begin
          RXlabels[7].Font.Color:=clred;
          RXlabels[7].Caption:=temp;
        end else RXlabels[7].Font.Color:=clWindowText;
      end;

      {$endif}


      temp:='CS '+IntToHex(context.SEGCS,4);
      if temp<>CSlabel.Caption then
      begin
        CSlabel.Font.Color:=clred;
        CSlabel.Caption:=temp;
      end else CSlabel.Font.Color:=clWindowText;

      temp:='DS '+IntToHex(context.SEGDS,4);
      if temp<>DSlabel.Caption then
      begin
        DSlabel.Font.Color:=clred;
        DSlabel.Caption:=temp;
      end else DSLabel.Font.Color:=clWindowText;

      temp:='SS '+IntToHex(context.SEGSS,4);
      if temp<>SSlabel.Caption then
      begin
        SSlabel.Font.Color:=clred;
        SSlabel.Caption:=temp;
      end else SSlabel.Font.Color:=clWindowText;

      temp:='ES '+IntToHex(context.SEGES,4);
      if temp<>ESlabel.Caption then
      begin
        ESlabel.Font.Color:=clred;
        ESlabel.Caption:=temp;
      end else ESlabel.Font.Color:=clWindowText;

      temp:='FS '+IntToHex(context.SEGFS,4);
      if temp<>FSlabel.Caption then
      begin
        FSlabel.Font.Color:=clred;
        FSlabel.Caption:=temp;
      end else FSlabel.Font.Color:=clWindowText;

      temp:='GS '+IntToHex(context.SEGGS,4);
      if temp<>GSlabel.Caption then
      begin
        GSlabel.Font.Color:=clred;
        GSlabel.Caption:=temp;
      end else GSlabel.Font.Color:=clWindowText;

      temp:='CF '+IntToStr(GetBitOf(context.EFLAgs,0));
      if (t2<>nil) and (GetBitOf(t.c.EFlags,0)<>GetBitOf(t2.c.EFlags,0)) then temp:=temp+' <> '+IntToStr(GetBitOf(t2.c.EFlags,0));
      if temp<>cflabel.Caption then
      begin
        CFlabel.Font.Color:=clred;
        CFlabel.caption:=temp;
      end else cflabel.Font.Color:=clWindowText;

      temp:='PF '+IntToStr(GetBitOf(context.EFlags,2));
      if (t2<>nil) and (GetBitOf(t.c.EFlags,2)<>GetBitOf(t2.c.EFlags,2)) then temp:=temp+' <> '+IntToStr(GetBitOf(t2.c.EFlags,2));
      if temp<>Pflabel.Caption then
      begin
        Pflabel.Font.Color:=clred;
        Pflabel.caption:=temp;
      end else Pflabel.Font.Color:=clWindowText;

      temp:='AF '+IntToStr(GetBitOf(context.EFlags,4));
      if (t2<>nil) and (GetBitOf(t.c.EFlags,4)<>GetBitOf(t2.c.EFlags,4)) then temp:=temp+' <> '+IntToStr(GetBitOf(t2.c.EFlags,4));
      if temp<>Aflabel.Caption then
      begin
        Aflabel.Font.Color:=clred;
        Aflabel.caption:=temp;
      end else Aflabel.Font.Color:=clWindowText;

      temp:='ZF '+IntToStr(GetBitOf(context.EFlags,6));
      if (t2<>nil) and (GetBitOf(t.c.EFlags,6)<>GetBitOf(t2.c.EFlags,6)) then temp:=temp+' <> '+IntToStr(GetBitOf(t2.c.EFlags,6));
      if temp<>Zflabel.Caption then
      begin
        Zflabel.Font.Color:=clred;
        Zflabel.caption:=temp;
      end else Zflabel.Font.Color:=clWindowText;

      temp:='SF '+IntToStr(GetBitOf(context.EFlags,7));
      if (t2<>nil) and (GetBitOf(t.c.EFlags,7)<>GetBitOf(t2.c.EFlags,7)) then temp:=temp+' <> '+IntToStr(GetBitOf(t2.c.EFlags,7));
      if temp<>Sflabel.Caption then
      begin
        Sflabel.Font.Color:=clred;
        Sflabel.caption:=temp;
      end else Sflabel.Font.Color:=clWindowText;

      temp:='DF '+IntToStr(GetBitOf(context.EFlags,10));
      if (t2<>nil) and (GetBitOf(t.c.EFlags,10)<>GetBitOf(t2.c.EFlags,10)) then temp:=temp+' <> '+IntToStr(GetBitOf(t2.c.EFlags,10));
      if temp<>Dflabel.Caption then
      begin
        Dflabel.Font.Color:=clred;
        Dflabel.caption:=temp;
      end else Dflabel.Font.Color:=clWindowText;

      temp:='OF '+IntToStr(GetBitOf(context.EFlags,11));
      if (t2<>nil) and (GetBitOf(t.c.EFlags,11)<>GetBitOf(t2.c.EFlags,11)) then temp:=temp+' <> '+IntToStr(GetBitOf(t2.c.EFlags,11));
      if temp<>Oflabel.Caption then
      begin
        Oflabel.Font.Color:=clred;
        Oflabel.caption:=temp;
      end else Oflabel.Font.Color:=clWindowText;

      if fpp<>nil then
        fpp.SetContextPointer(@TTraceDebugInfo(lvTracer.selected.data).c);

      if Stackview<>nil then
        updatestackview;

    end;

  end;
end;

procedure TfrmTracer.updatestackview;
var di: TTraceDebugInfo;
begin
  if (Stackview<>nil) and (lvTracer.selected<>nil) and (Stackview.Visible) then
  begin
    //get stack
    di:=TTraceDebugInfo(lvTracer.selected.data);
    if (di<>nil) and (di.stack.stack<>nil) then
      StackView.SetContextPointer(@di.c, di.stack.stack, di.stack.savedsize);
  end;
end;

procedure TfrmTracer.EAXLabelDblClick(Sender: TObject);
var s: string;
begin
  s:=tlabel(sender).Caption;
  s:=copy(s,5,length(s)-4);

  memorybrowser.memoryaddress:=StrToQWordEx('$'+s);
end;

procedure TfrmTracer.lvTracerDblClick(Sender: TObject);
var
  a: ptruint;
  syma: ptruint;
  sym: string;
  i: integer;
  e: boolean;
begin

  if (lvTracer.selected<>nil) and (lvTracer.selected.data<>nil) then
  begin
    syma:=0;
    a:=TTraceDebugInfo(lvTracer.selected.data).c.{$ifdef cpu64}rip{$else}Eip{$endif};
    e:=true;
    i:=RPos(' - ', lvTracer.Selected.Text);
    if i>0 then
    begin
      sym:=copy(lvTracer.Selected.Text,1,i);
      sym:=trim(sym);

      if sym<>'' then
        syma:=symhandler.getAddressFromName(sym,false,e);
    end;

    if not e then
      memorybrowser.disassemblerview.SelectedAddress:=syma
    else
      memorybrowser.disassemblerview.SelectedAddress:=a
  end;
end;

procedure TfrmTracer.Panel1Resize(Sender: TObject);
begin
  panel7.Top:=(panel1.ClientHeight div 2)-panel7.Height;
  if panel7.top<(pnlFlags.top+pnlFlags.height) then
    panel7.top:=pnlFlags.top+pnlFlags.height;
end;

procedure TfrmTracer.sbShowFloatsClick(Sender: TObject);
begin
  if (lvTracer.selected=nil) or (lvTracer.selected.data=nil) then exit;

  if fpp=nil then
  begin
    fpp:=frmFloatingPointPanelUnit.TfrmFloatingPointPanel.create(self); //fpc complains it can't find it, so added the unit in front
  end;

  fpp.Left:=self.left+self.Width;
  fpp.Top:=self.top;
  fpp.SetContextPointer(@TTraceDebugInfo(lvTracer.selected.data).c);
  fpp.show;//pop to foreground
end;

procedure ApplyFontColor(control: TControl; color: TColor);
var
  i: integer;
  wc: twincontrol;
begin
  control.font.color:=color;
  if control is TWinControl then
  begin
    wc:=TWinControl(control);
    for i:=0 to wc.ControlCount-1 do
      applyFontcolor(wc.Controls[i],color);
  end;


end;

procedure TfrmTracer.FormShow(Sender: TObject);
var minwidth: integer;
begin
  panel1.Font.Size:=10;


  lblInstruction.font.size:=10;
  sbShowFloats.font.size:=10;
  sbShowstack.font.size:=10;
  button1.Font.size:=10;
  pnlRegisters.font.size:=10;
  pnlFlags.Font.size:=10;
  pnlSegments.font.size:=10;

  ApplyFontColor(Panel1, font.color);

  Panel1Resize(panel1);


  if loadedformpos=false then
  begin
    minwidth:=panel1.Canvas.TextWidth(' XXX XXXXXXXXXXXXXXXX   XX X ');
    if panel1.width<minwidth then
      panel1.width:=minwidth+4;

    minwidth:=lvtracer.canvas.textwidth('XXXXXXXX - XXX XXX XXXXXXXXXXXXX');
    if lvtracer.width<minwidth then
      width:=width+(minwidth-lvtracer.width);

  end;
end;

procedure TfrmTracer.sbShowstackClick(Sender: TObject);
begin
  if Stackview=nil then
    stackview:=TfrmStackView.create(self);

  stackview.show;

  updatestackview;
end;

function frmTracer_getEntry(L: PLua_state): integer; cdecl;
var
  f: TfrmTracer;
  i: integer;

  e: TTraceDebugInfo;
  t: integer;

  ct: integer;
begin
  f:=luaclass_getClassObject(L);
  i:=lua_tointeger(L,1);

  result:=0;
  e:=f.Entry[i];
  if e=nil then
    exit(0);

  lua_newtable(L);
  t:=lua_gettop(L);

  lua_pushstring(L,'address');
  lua_pushinteger(L,{$ifdef cpu64}e.c.RIP{$else}e.c.EIP{$endif});
  lua_settable(L,t);

  lua_pushstring(L,'selected');
  if (i>=0) and (i<f.lvTracer.Items.Count) then
    lua_pushboolean(L, f.lvTracer.Items[i].Selected)
  else
    lua_pushboolean(L, false);
  lua_settable(L,t);

  lua_pushstring(L,'instruction');
  lua_pushstring(L,e.instruction);
  lua_settable(L,t);

  lua_pushstring(L,'instructionSize');
  lua_pushinteger(L,e.instructionsize);
  lua_settable(L,t);

  lua_pushstring(L,'referencedAddress');
  lua_pushinteger(L,e.referencedAddress);
  lua_settable(L,t);

  lua_pushstring(L,'context');
  lua_pushcontext(L,@e.c);
  lua_settable(L,t);

  lua_pushstring(L,'referencedData');
  CreateByteTableFromPointer(L,e.bytes,e.bytesize);
  lua_settable(L,t);

  lua_pushstring(L,'hasStackSnapshot');
  lua_pushboolean(L, e.stack.stack<>nil);
  lua_settable(L,t);

  result:=1;
end;

function frmTracer_getStack(L: PLua_state): integer; cdecl;
var
  f: TfrmTracer;
  i: integer;

  e: TTraceDebugInfo;
begin
  f:=luaclass_getClassObject(L);
  i:=lua_tointeger(L,1);

  result:=0;
  e:=f.Entry[i];
  if (e=nil) or (e.stack.stack=nil) then
    exit(0);

  CreateByteTableFromPointer(L,pbytearray(e.stack.stack),e.stack.savedsize);
  result:=1;
end;

procedure frmTracer_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customform_addMetaData(L, metatable, userdata);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Entry', frmTracer_getEntry, nil);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'StackEntry', frmTracer_getStack, nil);

end;

initialization
  registerclass(TfrmTracer);
  luaclass_register(TfrmTracer, frmTracer_addMetaData);

  {$i frmTracerUnit.lrs}

end.
