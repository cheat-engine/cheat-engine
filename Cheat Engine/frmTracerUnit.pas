unit frmTracerUnit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, disassembler, NewKernelHandler, ExtCtrls, Buttons,
  LResources, frmFloatingPointPanelUnit, strutils, cefuncproc, clipbrd, Menus,
  ComCtrls, luahandler, symbolhandler, byteinterpreter, frmStackviewunit,
  tracerIgnore, commonTypeDefs;

type TTraceDebugInfo=class
  private
  public
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

    function datatype: TVariableType;
    procedure fillbytes(datasize: integer);
    procedure savestack;
    procedure saveToStream(s: tstream);
    constructor createFromStream(s: tstream);
    destructor destroy; override;
end;

type

  { TfrmTracer }

  TfrmTracer = class(TForm)
    aflabel: TLabel;
    Button1: TButton;
    btnStopSearch: TButton;
    cflabel: TLabel;
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
    FSlabel: TLabel;
    GSlabel: TLabel;
    lblInstruction: TLabel;
    lblAddressed: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
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
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    pflabel: TLabel;
    pnlSearch: TPanel;
    pmTracer: TPopupMenu;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    lvTracer: TTreeView;
    sbShowFloats: TSpeedButton;
    sbShowstack: TSpeedButton;
    sflabel: TLabel;
    SSlabel: TLabel;
    Splitter1: TSplitter;
    zflabel: TLabel;
    procedure btnStopSearchClick(Sender: TObject);
    procedure lvTracerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure miLoadClick(Sender: TObject);
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

    traceaddress: dword;
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
    nosystem: boolean;

    procedure configuredisplay;
    procedure setSavestack(x: boolean);
    procedure updatestackview;
  public
    { Public declarations }
    returnfromignore: boolean;

    procedure setDataTrace(state: boolean);
    procedure addRecord;
    procedure finish;
    property savestack: boolean read fsavestack write setSavestack;
    constructor create(Owner: TComponent; DataTrace: boolean=false; skipconfig: boolean=false); overload;
  end;

implementation


uses cedebugger, debughelper, MemoryBrowserFormUnit, frmTracerConfigUnit,
  debuggertypedefinitions, processhandlerunit, Globals, Parsers;

resourcestring
  rsSearch = 'Search';
  rsTypeTheLUAConditionYouWantToSearchForExampleEAX0x1 = 'Type the (LUA) condition you want to search for (Example: EAX==0x1234)';

destructor TTraceDebugInfo.destroy;
begin
  if bytes<>nil then
    freemem(bytes);

  if stack.stack<>nil then
    freemem(stack.stack);

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
  ReadProcessMemory(processhandle, pointer(referencedaddress), bytes, datasize, bytesize);
end;

procedure TTraceDebugInfo.SaveStack;
begin
  getmem(stack.stack, savedStackSize);
  if ReadProcessMemory(processhandle, pointer(c.{$ifdef cpu64}Rsp{$else}esp{$endif}), stack.stack, savedStackSize, stack.savedsize)=false then
  begin
    stack.savedsize:=4096-(c.{$ifdef cpu64}Rsp{$else}esp{$endif} mod 4096);
    ReadProcessMemory(processhandle, pointer(c.{$ifdef cpu64}Rsp{$else}esp{$endif}), stack.stack, stack.savedsize, stack.savedsize);
  end;
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
  freemem(x);

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


constructor TfrmTracer.create(Owner: TComponent; DataTrace: boolean=false; skipconfig: boolean=false);
begin
  inherited create(owner);
  fDataTrace:=Datatrace;
  fSkipConfig:=skipconfig;
end;

procedure TfrmTracer.finish;
begin
  //for whatever I could use it...

end;

procedure TfrmTracer.addRecord;
var s,s2: string;
    i: integer;
    d: TTraceDebugInfo;

    a,address: ptrUint;
    referencedAddress: ptrUint;
    haserror: boolean;
    thisnode, thatnode,x: TTreenode;

    da: TDisassembler;

    datasize: integer;
    isfloat: boolean;
begin
  //the debuggerthread is now paused so get the context and add it to the list

  try

    address:=debuggerthread.CurrentThread.context.{$ifdef CPU64}rip{$else}eip{$endif};
    a:=address;
    s:=disassemble(a);

    a:=address;
    da:=tdisassembler.Create;
    s:=da.disassemble(a, s2);

    datasize:=da.LastDisassembleData.datasize;
    if datasize=0 then
      datasize:=4;

    isfloat:=da.LastDisassembleData.isfloat;

    da.free;



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
    d.instructionsize:=a-address;
    d.c:=debuggerthread.CurrentThread.context^;
    d.instruction:=s;
    d.referencedAddress:=referencedAddress;
    d.isfloat:=isfloat;
    d.fillbytes(datasize);

    if savestack then
      d.savestack;

    s:=inttohex(address,8)+' - '+s;

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

    if not stepover and defaultDisassembler.LastDisassembleData.iscall then
      currentAppendage:=thisnode;

    if (defaultDisassembler.LastDisassembleData.isret) {or returnfromignore} then
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

procedure TfrmTracer.FormCreate(Sender: TObject);
var tcount: integer;
    condition: string;
    x: array of integer;
    testcontext: TContext;

    fromaddress: ptruint;
    toaddress: ptruint;

    bpTrigger: TBreakpointTrigger;
begin
  //set a breakpoint and when that breakpoint gets hit trace a number of instructions
  if fskipconfig=false then
  begin
    if frmTracerConfig=nil then
      frmTracerConfig:=TfrmTracerConfig.create(application);

    with frmTracerConfig do
    begin
      DataTrace:=fDataTrace;
      if showmodal=mrok then
      begin
        ignoredModuleListHandler.LoadIgnoredModules;

        dereference:= cbDereferenceAddresses.checked;
        savestack:= cbSaveStack.checked;

        tcount:=strtoint(edtMaxTrace.text);
        condition:=edtCondition.text;
        stepover:=cbStepOver.checked;
        nosystem:=cbSkipSystemModules.checked;

        if startdebuggerifneeded then
        begin
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
            debuggerthread.setBreakAndTraceBreakpoint(self, fromaddress, bpTrigger, 1+(toaddress-fromaddress), tcount, condition, stepover, nosystem);
          end
          else
          begin
            if (owner is TMemoryBrowser) then
              debuggerthread.setBreakAndTraceBreakpoint(self, (owner as TMemoryBrowser).disassemblerview.SelectedAddress, bptExecute, 1, tcount, condition, StepOver, Nosystem)
            else
              debuggerthread.setBreakAndTraceBreakpoint(self, memorybrowser.disassemblerview.SelectedAddress, bptExecute,1, tcount, condition, StepOver, nosystem);
          end;
        end;


      end;
    end;

  end;


  setlength(x,0);
  loadformposition(self,x);
end;

procedure TfrmTracer.RegisterMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var s: string;
i: integer;
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
  if savedialog1.Execute then
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




          z.add(pref+'AX='+inttohex(c.{$ifdef cpu64}Rax{$else}Eax{$endif},8));
          z.add(pref+'BX='+inttohex(c.{$ifdef cpu64}Rbx{$else}Ebx{$endif},8));
          z.add(pref+'CX='+inttohex(c.{$ifdef cpu64}Rcx{$else}Ecx{$endif},8));
          z.add(pref+'DX='+inttohex(c.{$ifdef cpu64}Rdx{$else}Edx{$endif},8));
          z.add(pref+'SI='+inttohex(c.{$ifdef cpu64}Rsi{$else}Esi{$endif},8));
          z.add(pref+'DI='+inttohex(c.{$ifdef cpu64}Rdi{$else}Edi{$endif},8));
          z.add(pref+'BP='+inttohex(c.{$ifdef cpu64}Rbp{$else}Ebp{$endif},8));
          z.add(pref+'SP='+inttohex(c.{$ifdef cpu64}Rsp{$else}Esp{$endif},8));
          z.add(pref+'IP='+inttohex(c.{$ifdef cpu64}Rip{$else}Eip{$endif},8));

          {$ifdef cpu64}
          if processhandler.is64bit then
          begin
            z.add('R8='+inttohex(c.r8,8));
            z.add('R9='+inttohex(c.r9,8));
            z.add('R10='+inttohex(c.r10,8));
            z.add('R11='+inttohex(c.r11,8));
            z.add('R12='+inttohex(c.r12,8));
            z.add('R13='+inttohex(c.r13,8));
            z.add('R14='+inttohex(c.r14,8));
            z.add('R15='+inttohex(c.r15,8));
          end;
          {$endif}

          z.add('');
          z.add('EFLAGS='+inttohex(c.EFlags,8));
          z.add('');
          z.add('-');


        end;
      end;

      z.SaveToFile(savedialog1.filename);
    finally
      z.free;
    end;
  end;
end;

procedure TfrmTracer.btnStopSearchClick(Sender: TObject);
begin
  stopsearch:=true;
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

      f.readbuffer(temp, sizeof(temp));

      m:=TMemoryStream.create;
      if temp>0 then
        m.CopyFrom(f, temp);

      m.Position:=0;
      lvTracer.LoadFromStream(m); //load the texttrace regardless of version
      m.free;

      if version<>{$ifdef cpu64}1{$else}0{$endif} then
        raise exception.create('This trace was made with the '+{$ifdef cpu64}'32'{$else}'64'{$endif}+'-bit version of cheat engine. You need to use that version to see the register values and stacktrace');

      for i:=0 to lvTracer.Items.Count-1 do
        lvTracer.Items[i].Data:=TTraceDebugInfo.createFromStream(f);

    finally
      f.free;
    end;
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


procedure TfrmTracer.MenuItem4Click(Sender: TObject);
var
  i: integer;
  c: PContext;
  check: boolean;
  searchstring: string;
begin
  if (sender = miSearchNext) then
    check:=true
  else
  begin
    check:=InputQuery(rsSearch, rsTypeTheLUAConditionYouWantToSearchForExampleEAX0x1, lastsearchstring);
    lastsearchstring:=lastsearchstring;
  end;

  searchstring:='return '+lastsearchstring;

  if check then
  begin
    stopsearch:=false;
    progressbar1.Position:=0;
    progressbar1.Max:=lvTracer.items.Count;
    pnlSearch.visible:=true;

    if lvTracer.Selected=nil then
      i:=0
    else
      i:=lvTracer.Selected.AbsoluteIndex+1;

    while (i<lvTracer.items.count) and (not stopsearch) do
    begin
      c:=@TTraceDebugInfo(lvTracer.Items[i].data).c;
      if c<>nil then
      begin
        if CheckIfConditionIsMetContext(c, searchstring) then
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

    pnlSearch.visible:=false;
  end;
end;

procedure TfrmTracer.Panel1Click(Sender: TObject);
begin

end;



procedure TfrmTracer.FormClose(Sender: TObject; var Action: TCloseAction);
var i: integer;
begin

  if debuggerthread<>nil then
    debuggerthread.stopBreakAndTrace(self);

  for i:=0 to lvTracer.Items.Count-1 do
  begin
    if lvTracer.Items[i].data<>nil then
    begin
      TTraceDebugInfo(lvTracer.Items[i].data).Free;
      lvTracer.Items[i].data:=nil;
    end;
  end;

  action:=cafree; //if still buggy, change to cahide
end;

procedure TfrmTracer.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmTracer.FormDestroy(Sender: TObject);
begin
  saveformposition(self,[]);
end;

procedure TfrmTracer.configuredisplay;
{
if the process is 64-bit create r8-r15 and move all objects closer
}
var
  i: integer;
  p,l: tlabel;
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
          parent:=panel4;
          font:=eaxlabel.font;
          cursor:=eaxlabel.cursor;

          tag:=i;
          onclick:=p.onclick;
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
begin
  configuredisplay;

  if lvTracer.selected<>nil then
  begin
    t:=TTraceDebugInfo(lvTracer.selected.data);
    if t<>nil then
    begin

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

      temp:=prefix+'AX '+IntToHex(context.{$ifdef cpu64}rax{$else}Eax{$endif},8);
      if temp<>eaxlabel.Caption then
      begin
        eaxlabel.Font.Color:=clred;
        eaxlabel.Caption:=temp;
      end else eaxlabel.Font.Color:=clWindowText;

      temp:=prefix+'BX '+IntToHex(context.{$ifdef cpu64}rbx{$else}ebx{$endif},8);
      if temp<>ebxlabel.Caption then
      begin
        ebxlabel.Font.Color:=clred;
        ebxlabel.Caption:=temp;
      end else ebxlabel.Font.Color:=clWindowText;

      temp:=prefix+'CX '+IntToHex(context.{$ifdef cpu64}rcx{$else}ecx{$endif},8);
      if temp<>eCxlabel.Caption then
      begin
        eCXlabel.Font.Color:=clred;
        eCXlabel.Caption:=temp;
      end else eCXlabel.Font.Color:=clWindowText;

      temp:=prefix+'DX '+IntToHex(context.{$ifdef cpu64}rdx{$else}edx{$endif},8);
      if temp<>eDxlabel.Caption then
      begin
        eDxlabel.Font.Color:=clred;
        eDxlabel.Caption:=temp;
      end else eDxlabel.Font.Color:=clWindowText;

      temp:=prefix+'SI '+IntToHex(context.{$ifdef cpu64}rsi{$else}esi{$endif},8);
      if temp<>eSIlabel.Caption then
      begin
        eSIlabel.Font.Color:=clred;
        eSIlabel.Caption:=temp;
      end else eSIlabel.Font.Color:=clWindowText;

      temp:=prefix+'DI '+IntToHex(context.{$ifdef cpu64}rdi{$else}edi{$endif},8);
      if temp<>eDIlabel.Caption then
      begin
        eDIlabel.Font.Color:=clred;
        eDIlabel.Caption:=temp;
      end else eDIlabel.Font.Color:=clWindowText;

      temp:=prefix+'BP '+IntToHex(context.{$ifdef cpu64}rbp{$else}ebp{$endif},8);
      if temp<>eBPlabel.Caption then
      begin
        eBPlabel.Font.Color:=clred;
        eBPlabel.Caption:=temp;
      end else eBPlabel.Font.Color:=clWindowText;

      temp:=prefix+'SP '+IntToHex(context.{$ifdef cpu64}rsp{$else}esp{$endif},8);
      if temp<>eSPlabel.Caption then
      begin
        eSPlabel.Font.Color:=clred;
        eSPlabel.Caption:=temp;
      end else eSPlabel.Font.Color:=clWindowText;

      temp:=prefix+'IP '+IntToHex(context.{$ifdef cpu64}rip{$else}eip{$endif},8);
      if temp<>eIPlabel.Caption then
      begin
        eIPlabel.Font.Color:=clred;
        eIPlabel.Caption:=temp;
      end else eIPlabel.Font.Color:=clWindowText;

      {$ifdef cpu64}

      if length(rxlabels)>0 then
      begin

        temp:='R8  '+IntToHex(context.r8,8);
        if temp<>RXlabels[0].Caption then
        begin
          RXlabels[0].Font.Color:=clred;
          RXlabels[0].Caption:=temp;
        end else RXlabels[0].Font.Color:=clWindowText;

        temp:='R9  '+IntToHex(context.r9,8);
        if temp<>RXlabels[1].Caption then
        begin
          RXlabels[1].Font.Color:=clred;
          RXlabels[1].Caption:=temp;
        end else RXlabels[1].Font.Color:=clWindowText;

        temp:='R10 '+IntToHex(context.r10,8);
        if temp<>RXlabels[2].Caption then
        begin
          RXlabels[2].Font.Color:=clred;
          RXlabels[2].Caption:=temp;
        end else RXlabels[2].Font.Color:=clWindowText;

        temp:='R11 '+IntToHex(context.r11,8);
        if temp<>RXlabels[3].Caption then
        begin
          RXlabels[3].Font.Color:=clred;
          RXlabels[3].Caption:=temp;
        end else RXlabels[3].Font.Color:=clWindowText;

        temp:='R12 '+IntToHex(context.r12,8);
        if temp<>RXlabels[4].Caption then
        begin
          RXlabels[4].Font.Color:=clred;
          RXlabels[4].Caption:=temp;
        end else RXlabels[4].Font.Color:=clWindowText;

        temp:='R13 '+IntToHex(context.r13,8);
        if temp<>RXlabels[5].Caption then
        begin
          RXlabels[5].Font.Color:=clred;
          RXlabels[5].Caption:=temp;
        end else RXlabels[5].Font.Color:=clWindowText;

        temp:='R14 '+IntToHex(context.r14,8);
        if temp<>RXlabels[6].Caption then
        begin
          RXlabels[6].Font.Color:=clred;
          RXlabels[6].Caption:=temp;
        end else RXlabels[6].Font.Color:=clWindowText;

        temp:='R15 '+IntToHex(context.r15,8);
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
      if temp<>cflabel.Caption then
      begin
        CFlabel.Font.Color:=clred;
        CFlabel.caption:=temp;
      end else cflabel.Font.Color:=clWindowText;

      temp:='PF '+IntToStr(GetBitOf(context.EFlags,2));
      if temp<>Pflabel.Caption then
      begin
        Pflabel.Font.Color:=clred;
        Pflabel.caption:=temp;
      end else Pflabel.Font.Color:=clWindowText;

      temp:='AF '+IntToStr(GetBitOf(context.EFlags,4));
      if temp<>Aflabel.Caption then
      begin
        Aflabel.Font.Color:=clred;
        Aflabel.caption:=temp;
      end else Aflabel.Font.Color:=clWindowText;

      temp:='ZF '+IntToStr(GetBitOf(context.EFlags,6));
      if temp<>Zflabel.Caption then
      begin
        Zflabel.Font.Color:=clred;
        Zflabel.caption:=temp;
      end else Zflabel.Font.Color:=clWindowText;

      temp:='SF '+IntToStr(GetBitOf(context.EFlags,7));
      if temp<>Sflabel.Caption then
      begin
        Sflabel.Font.Color:=clred;
        Sflabel.caption:=temp;
      end else Sflabel.Font.Color:=clWindowText;

      temp:='DF '+IntToStr(GetBitOf(context.EFlags,10));
      if temp<>Dflabel.Caption then
      begin
        Dflabel.Font.Color:=clred;
        Dflabel.caption:=temp;
      end else Dflabel.Font.Color:=clWindowText;

      temp:='OF '+IntToStr(GetBitOf(context.EFlags,11));
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
  s:=copy(s,5,8);

  memorybrowser.memoryaddress:=StrToQWordEx('$'+s);
end;

procedure TfrmTracer.lvTracerDblClick(Sender: TObject);
begin
  if (lvTracer.selected<>nil) and (lvTracer.selected.data<>nil) then
    memorybrowser.disassemblerview.SelectedAddress:=TTraceDebugInfo(lvTracer.selected.data).c.{$ifdef cpu64}rip{$else}Eip{$endif};
end;

procedure TfrmTracer.Panel1Resize(Sender: TObject);
begin
  panel7.Top:=(panel1.ClientHeight div 2)-panel7.Height;
  if panel7.top<(panel6.top+panel6.height) then
    panel7.top:=panel6.top+panel6.height;
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

procedure TfrmTracer.FormShow(Sender: TObject);
begin
  panel1.Font.Height:=GetFontData(font.Handle).Height;
  if panel1.Font.Height>-13 then
    panel1.Font.Height:=-13;

  lblInstruction.font:=font;
  sbShowFloats.font:=font;
  sbShowstack.font:=font;
  button1.Font:=font;;
  Panel1Resize(panel1);
end;

procedure TfrmTracer.sbShowstackClick(Sender: TObject);
begin
  if Stackview=nil then
    stackview:=TfrmStackView.create(self);

  stackview.show;

  updatestackview;
end;

initialization
  registerclass(TfrmTracer);
  {$i frmTracerUnit.lrs}

end.
