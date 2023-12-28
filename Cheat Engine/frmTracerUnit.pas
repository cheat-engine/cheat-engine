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
  debuggertypedefinitions, betterControls, LastDisassembleData, contexthandler;

type
  TTraceDebugInfo=class
  private
  public
    cr3:qword;
    instruction: string;
    instructionsize: integer;
    referencedAddress: ptrUint;
    context: pointer; //was c
    contexthandler: TContextInfo;

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
    constructor createFromStream(s: tstream; ch: TContextInfo);
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

    GPRlabels: array of TLabel;
    Specializedlabels: array of TLabel;
    Flagslabels: array of tlabel;

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

    registerCompareIgnore: array of boolean;

    da: TDisassembler;
    {$ifdef windows}
    dacr3: TCR3Disassembler;
    {$endif}

    defaultBreakpointMethod: TBreakpointmethod;

    physicaladdress: qword;
    DBVMStatusUpdater: TDBVMStatusUpdater;

    currentTracefilename: string;

    contexthandler: TContextInfo;

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
  Registry, GDBServerDebuggerInterface;

resourcestring
  rsSearch = 'Search';
  rsTypeTheLUAConditionYouWantToSearchForExampleEAX0x1 = 'Type the (LUA) condition you want to search for (Example: EAX==0x1234)    '#13#10'Also available: referencedAddress (integer), referencedBytes (bytetable), instruction (string)';
  rsWaitingForTraceToStart = 'Waiting for trace to start';
  rsDBVMBreakAndTraceNeedsDBVM = 'DBVM Break and Trace needs DBVM. Loading '
    +'DBVM can potentially cause a system freeze. Are you sure?';
  rsTracer = 'Tracer';

destructor TTraceDebugInfo.destroy;
begin
  if bytes<>nil then
    freememandnil(bytes);

  if stack.stack<>nil then
    freememandnil(stack.stack);

  if context<>nil then
    freememandnil(context);

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
var sp: qword;
begin
  getmem(stack.stack, savedStackSize);
  sp:=contexthandler.StackPointerRegister^.getValue(context);

  if cr3=0 then
  begin
    if ReadProcessMemory(processhandle, pointer(sp), stack.stack, savedStackSize, stack.savedsize)=false then
    begin
      stack.savedsize:=4096-(sp mod 4096);
      ReadProcessMemory(processhandle, pointer(sp), stack.stack, stack.savedsize, stack.savedsize);
    end;
  end
  {$ifdef windows}
  else
    ReadProcessMemoryCR3(cr3, pointer(sp), stack.stack, savedStackSize, stack.savedsize){$endif};
end;

constructor TTraceDebugInfo.createFromStream(s: tstream; ch: TContextInfo);
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
  getmem(context, ch.ContextSize);
  s.readBuffer(context, ch.ContextSize);
  contexthandler:=ch;
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
  s.WriteBuffer(context^, contexthandler.ContextSize);

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
    ch: TContextInfo;
begin
  //the debuggerthread is now paused so get the context and add it to the list

  try
    ch:=debuggerthread.CurrentThread.contexthandler ;
    address:=ch.InstructionPointerRegister^.getValue(debuggerthread.CurrentThread.context);

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
    d.context:=ch.getCopy(debuggerthread.CurrentThread.context);
    d.contexthandler:=ch;
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



          if (ch.InstructionPointerRegister.getValue(d.context)+d.instructionsize<>a) then
          begin
            //see if a parent can be found that does match
            x:=currentappendage.Parent;
            while x<>nil do
            begin
              d:=TTraceDebugInfo(x.Data);

              if d=nil then exit; //cleanup underway

              if (ch.InstructionPointerRegister.getValue(d.context)+d.instructionsize=a) then
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
  contexthandler:=getBestContextHandler;

  setlength(x,0);
  loadedformpos:=loadformposition(self,x);

  if length(x)>=1 then
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
  i,j: integer;
  t: TTraceDebugInfo;
  c: Pointer;
  pref: string;
  ch: TContextInfo;

  gp: PContextElementRegisterList;
  flags: PContextElementRegisterList;
begin
  //save the results of the trace to disk
  if SaveDialogText.Execute then
  begin
    ch:=getBestContextHandler;
    z:=tstringlist.create;
    try

      for i:=0 to lvTracer.Items.Count-1 do
      begin

        z.add(lvTracer.Items[i].Text);
        t:=TTraceDebugInfo(lvTracer.Items[i].data);
        if t<>nil then
        begin
          c:=t.context;

          gp:=ch.getGeneralPurposeRegisters;

          if dereference then
          begin
            if t.referencedAddress<>0 then
              z.add(inttohex(t.referencedAddress,8)+' = '+DataToString(t.bytes, t.bytesize, t.datatype));
          end;

          for j:=0 to length(gp^)-1 do
            z.add(uppercase(gp^[j].name)+'='+gp^[j].getFullValueString(c));

          flags:=ch.getGeneralPurposeFlags;
          if flags<>nil then
          begin
            z.add('');
            for j:=0 to length(flags^)-1 do
              z.add(uppercase(flags^[j].name)+'='+flags^[j].getValueString(c));
          end;
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
  gpr: PContextElementRegisterList;
  fpu: PContextElementRegisterList;
  i: integer;
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

      if contexthandler.InstructionPointerRegister^.getValue(thisinfo.context)<>contexthandler.InstructionPointerRegister^.getValue(compareinfo.context) then
      begin
        sender.canvas.font.color:=clWindowText;
        sender.BackgroundColor:=clRed;
      end
      else
      begin
        ignorestack:=cbIgnoreStackPointers.checked;

        sender.BackgroundColor:=clWindow;
        //check the registers and flags

        gpr:=contexthandler.getGeneralPurposeRegisters;

        different:=false;
        if length(registerCompareIgnore)<>length(gpr^) then raise exception.create('registercompare doesn''t match gpr');
        for i:=0 to length(gpr^)-1 do
        begin
          if contexthandler.InstructionPointerRegister=@gpr^[i] then continue; //skip instruction pointer

          if (not registerCompareIgnore[i]) and (gpr^[i].getValue(compareinfo.context) <> gpr^[i].getValue(thisinfo.context)) then
          begin
            if ignorestack and ( (contexthandler.FramePointerRegister=@gpr^[i]) or (contexthandler.StackPointerRegister=@gpr^[i])) then
              continue;

            different:=true;
            break;
          end;
        end;

        if (not different) and cbFPUXMM.checked then
        begin
          fpu:=contexthandler.getFloatingPointRegisters;
          for i:=0 to length(fpu^)-1 do
          begin
            if comparemem(fpu^[i].getPointer(compareinfo.context),  fpu^[i].getPointer(compareinfo.context), fpu^[i].size)=false then
            begin
              different:=false;
              break;
            end;
          end;
          if not different then
          begin
            fpu:=contexthandler.getAlternateFloatingPointRegisters;
            if fpu<>nil then
            begin
              for i:=0 to length(fpu^)-1 do
              begin
                if comparemem(fpu^[i].getPointer(compareinfo.context),  fpu^[i].getPointer(compareinfo.context), fpu^[i].size)=false then
                begin
                  different:=false;
                  break;
                end;
              end;
            end;
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
          PContext(d.context)^.P1Home:=basic^.FSBASE; //just using these field for storage
          PContext(d.context)^.p2home:=basic^.GSBASE;
          PContext(d.context)^.p3home:=basic^.CR3;
          {$endif}
          PContext(d.context)^.EFlags:=basic^.FLAGS;
          PContext(d.context)^.{$ifdef cpu32}Eax{$else}Rax{$endif}:=basic^.RAX;
          PContext(d.context)^.{$ifdef cpu32}Ebx{$else}Rbx{$endif}:=basic^.RBX;
          PContext(d.context)^.{$ifdef cpu32}Ecx{$else}Rcx{$endif}:=basic^.RCX;
          PContext(d.context)^.{$ifdef cpu32}Edx{$else}Rdx{$endif}:=basic^.RDX;
          PContext(d.context)^.{$ifdef cpu32}Esi{$else}Rsi{$endif}:=basic^.RSI;
          PContext(d.context)^.{$ifdef cpu32}Edi{$else}Rdi{$endif}:=basic^.RDI;
          {$ifdef cpu64}
          PContext(d.context)^.R8:=basic^.R8;
          PContext(d.context)^.R9:=basic^.R9;
          PContext(d.context)^.R10:=basic^.R10;
          PContext(d.context)^.R11:=basic^.R11;
          PContext(d.context)^.R12:=basic^.R12;
          PContext(d.context)^.R13:=basic^.R13;
          PContext(d.context)^.R14:=basic^.R14;
          PContext(d.context)^.R15:=basic^.R15;
          {$endif}
          PContext(d.context)^.{$ifdef cpu32}Ebp{$else}Rbp{$endif}:=basic^.RBP;
          PContext(d.context)^.{$ifdef cpu32}Esp{$else}Rsp{$endif}:=basic^.RSP;
          PContext(d.context)^.{$ifdef cpu32}Eip{$else}Rip{$endif}:=basic^.RIP;
          PContext(d.context)^.SegCs:=basic^.CS;
          PContext(d.context)^.SegDs:=basic^.DS;
          PContext(d.context)^.SegEs:=basic^.ES;
          PContext(d.context)^.SegSs:=basic^.SS;
          PContext(d.context)^.SegFs:=basic^.FS;
          PContext(d.context)^.SegGs:=basic^.GS;

          {$ifdef cpu64}
          copymemory(@PContext(d.context)^.FltSave, fpu,512);
          {$else}
          copymemory(@PContext(d.context)^.ext, fpu,512);
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

                if (contexthandler.InstructionPointerRegister^.getValue(d.context)+d.instructionsize<>a) then
                begin
                  //see if a parent can be found that does match
                  x:=currentappendage.Parent;
                  while x<>nil do
                  begin
                    d:=TTraceDebugInfo(x.Data);

                    if (contexthandler.InstructionPointerRegister^.getValue(d.context)+d.instructionsize=a) then
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
      contexthandler:=getBestContextHandler;


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

        if CurrentDebuggerInterface is TGDBServerDebuggerInterface then
          breakpointmethod:=bpmGDB;

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
  ch: TContextInfo;
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

      //todo: save/load the contextinfo from the stream
      if version<>{$ifdef cpu64}1{$else}0{$endif} then
        raise exception.create('This trace was made with the '+{$ifdef cpu64}'32'{$else}'64'{$endif}+'-bit version of '+strCheatEngine+'. You need to use that version to see the register values and stacktrace');

      ch:=getBestContextHandler;
      for i:=0 to comparetv.Items.Count-1 do
      begin
        comparetv.Items[i].Data:=TTraceDebugInfo.createFromStream(f, ch);
        if i<lvtracer.items.Count then
          TTraceDebugInfo(lvTracer.items[i].data).compareindex:=i;
      end;

      miRealignCompare.enabled:=true;
      miRealignCompare.visible:=true;

      caption:=rstracer+':'+currenttracefilename+' - '+extractfilename(opendialog1.filename);



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
  ch: TContextInfo;
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

      //todo: save/load the contextinfo from the stream
      if version<>{$ifdef cpu64}1{$else}0{$endif} then
        raise exception.create('This trace was made with the '+{$ifdef cpu64}'32'{$else}'64'{$endif}+'-bit version of '+strCheatEngine+'. You need to use that version to see the register values and stacktrace');

      dereference:=false;
      ch:=getBestContextHandler;

      for i:=0 to lvTracer.Items.Count-1 do
      begin
        lvTracer.Items[i].Data:=TTraceDebugInfo.createFromStream(f,ch);
        if not dereference and (TTraceDebugInfo(lvTracer.Items[i].Data).bytesize>0) then
          dereference:=true;
      end;



      miOpenTraceForCompare.Enabled:=true;
      miOpenTraceForCompare.Visible:=true;

      currentTracefilename:=extractfilename(opendialog1.filename);

      caption:=rsTracer+':'+currentTracefilename;
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

    reallignscanaddress:=contexthandler.InstructionPointerRegister^.getValue(tdi.context);
    l2.refresh;

    if (tdi.compareindex<>-1) and (tdi.compareindex<l2.items.count) then
    begin
      tdi2:=TTraceDebugInfo(l2.Items[tdi.compareindex].data);
      if reallignscanaddress=contexthandler.InstructionPointerRegister^.getValue(tdi2.context) then
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

  if contexthandler.InstructionPointerRegister^.getValue(TTraceDebugInfo(node.data).context)=reallignscanaddress then
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

      currenttracefilename:=extractfilename(savedialog1.filename);

      caption:=rsTracer+':'+currenttracefilename;
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
  c: pointer;
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
        c:=TTraceDebugInfo(lvTracer.Items[i].data).context;
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
        c:=TTraceDebugInfo(lvTracer.Items[i].data).context;
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
  gpr: PContextElementRegisterList;
  specialized:  PContextElementRegisterList;
  flags:  PContextElementRegisterList;
  pcindex: integer;
begin
  pcindex:=-1;
  if not isConfigured then
  begin
    //build the registerlists
    while pnlRegisters.ControlCount>0 do
      pnlRegisters.Controls[0].Free;

    while pnlFlags.ControlCount>0 do
      pnlFlags.Controls[0].Free;

    while pnlSegments.ControlCount>0 do
      pnlSegments.Controls[0].Free;

    gpr:=contexthandler.getGeneralPurposeRegisters;
    setlength(gprlabels, length(gpr^));
    for i:=0 to length(gpr^)-1 do
    begin
      l:=tlabel.create(self);
      gprlabels[i]:=l;
      l.parent:=pnlRegisters;
      l.parentfont:=true;
      l.parentcolor:=false;
      l.cursor:=crHandPoint;
      l.tag:=ptruint(@gpr^[i]);
      l.OnDblClick:=EAXLabelDblClick;
      l.OnMouseDown:=RegisterMouseDown;

      if contexthandler.InstructionPointerRegister=@gpr^[i] then
        pcindex:=i;
    end;

  {  if pcindex<>-1 then
      gprlabels[pcindex].BringToFront;  }

    specialized:=contexthandler.getSpecializedRegisters;
    if specialized<>nil then
    begin
      setlength(Specializedlabels, length(specialized^));
      for i:=0 to length(specialized^)-1 do
      begin
        l:=tlabel.create(self);
        Specializedlabels[i]:=l;
        l.parent:=pnlSegments;
        l.parentfont:=true;
        l.parentcolor:=false;
        l.tag:=ptruint(@specialized^[i]);
      end;
    end;

    flags:=contexthandler.getGeneralPurposeFlags;
    if flags<>nil then
    begin
      setlength(Flagslabels, length(flags^));
      for i:=0 to length(flags^)-1 do
      begin
        l:=tlabel.create(self);
        Flagslabels[i]:=l;
        l.parent:=pnlFlags;
        l.parentfont:=true;
        l.parentcolor:=false;
        l.tag:=ptruint(@flags^[i]);
      end;
    end;


    isConfigured:=true;
  end;
end;

procedure TfrmTracer.lvTracerClick(Sender: TObject);
var temp: string;
    context: pointer;
    t: TTraceDebugInfo;
    prefix: char;

    t2: TTraceDebugInfo;
    c: tcontrol;
    r: PContextElement_register;
    i: integer;
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



      context:=t.context;
      for i:=0 to pnlRegisters.ControlCount-1 do
      begin
        c:=pnlRegisters.Controls[i];

        if c is TLabel then
        begin
          r:=PContextElement_register(c.Tag);
          temp:=padleft(r^.name, contexthandler.GeneralPurposeRegisterMaxCharCount)+' '+r^.getFullValueString(context);
          if (t2<>nil) and (r^.getValue(t2.context)<>r^.getValue(t.context)) then
            temp:=temp+' <> '+r^.getFullValueString(t2.context);

          if temp<>c.caption then
          begin
            c.font.color:=clred;
            c.caption:=temp;
          end
          else
            c.font.color:=clWindowtext;
        end;
      end;

      for i:=0 to pnlFlags.ControlCount-1 do
      begin
        c:=pnlFlags.Controls[i];

        if c is TLabel then
        begin
          r:=PContextElement_register(c.Tag);
          temp:=padleft(r^.name, contexthandler.GeneralPurposeFlagMaxCharCount)+' '+r^.getFullValueString(context);
          if (t2<>nil) and (r^.getValue(t2.context)<>r^.getValue(t.context)) then
            temp:=temp+' <> '+r^.getFullValueString(t2.context);

          if temp<>c.caption then
          begin
            c.font.color:=clred;
            c.caption:=temp;
          end
          else
            c.font.color:=clWindowtext;
        end;
      end;

      for i:=0 to pnlSegments.ControlCount-1 do
      begin
        c:=pnlSegments.Controls[i];

        if c is TLabel then
        begin
          r:=PContextElement_register(c.Tag);
          temp:=r^.name+' '+r^.getFullValueString(context);
          if temp<>c.caption then
          begin
            c.font.color:=clred;
            c.caption:=temp;
          end
          else
            c.font.color:=clWindowtext;
        end;
      end;

      if fpp<>nil then
        fpp.SetContextPointer(context);

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
      StackView.SetContextPointer(di.context, di.stack.stack, di.stack.savedsize);
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
    a:=contexthandler.InstructionPointerRegister^.getValue(TTraceDebugInfo(lvTracer.selected.data).context);
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
  fpp.SetContextPointer(TTraceDebugInfo(lvTracer.selected.data).context);
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
  ch: TContextInfo;
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

  lua_pushinteger(L,e.contexthandler.InstructionPointerRegister^.getValue(e.context));
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
  lua_pushcontext(L,e.context);
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
