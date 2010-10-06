unit frmTracerUnit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, disassembler, NewKernelHandler, ExtCtrls, Buttons,
  LResources, frmFloatingPointPanelUnit, strutils, cefuncproc, clipbrd, Menus,
  ComCtrls, luahandler, symbolhandler, byteinterpreter, frmStackviewunit;

type TTraceDebugInfo=class
  private
  public
    instruction: string;
    referencedAddress: ptrUint;
    c: _CONTEXT;
    bytes: pbytearray;
    bytesize: dword;

    stack: record
      savedsize: dword;
      stack: pbyte;
    end;

    function datatype: TVariableType;
    procedure fillbytes;
    procedure savestack;
    destructor destroy; override;
end;

type

  { TfrmTracer }

  TfrmTracer = class(TForm)
    Button1: TButton;
    Button2: TButton;
    lblInstruction: TLabel;
    lblAddressed: TLabel;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miSearchNext: TMenuItem;
    Panel1: TPanel;
    EAXLabel: TLabel;
    EBXlabel: TLabel;
    ECXlabel: TLabel;
    EDXlabel: TLabel;
    ESIlabel: TLabel;
    EDIlabel: TLabel;
    EBPlabel: TLabel;
    ESPlabel: TLabel;
    EIPlabel: TLabel;
    cflabel: TLabel;
    Panel2: TPanel;
    pnlSearch: TPanel;
    pflabel: TLabel;
    aflabel: TLabel;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    sbShowstack: TSpeedButton;
    zflabel: TLabel;
    sflabel: TLabel;
    oflabel: TLabel;
    CSLabel: TLabel;
    SSlabel: TLabel;
    DSLabel: TLabel;
    ESlabel: TLabel;
    FSlabel: TLabel;
    GSlabel: TLabel;
    Splitter1: TSplitter;
    dflabel: TLabel;
    sbShowFloats: TSpeedButton;
    procedure Button2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure RegisterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure EAXLabelDblClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
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

    procedure configuredisplay;
    procedure setSavestack(x: boolean);
    procedure updatestackview;
  public
    { Public declarations }
    procedure addRecord;
    property savestack: boolean read fsavestack write setSavestack;
  end;

implementation


uses cedebugger, debughelper, MemoryBrowserFormUnit, frmTracerConfigUnit;

destructor TTraceDebugInfo.destroy;
begin
  if bytes<>nil then
    freemem(bytes);

  if stack.stack<>nil then
    freemem(stack.stack);
end;

function TTraceDebugInfo.datatype: TVariableType;
begin
  result:=FindTypeOfData(referencedAddress, bytes, bytesize);
end;

procedure TTraceDebugInfo.fillbytes;
begin
  getmem(bytes, 64);
  bytesize:=0;
  ReadProcessMemory(processhandle, pointer(referencedaddress), bytes, 64, bytesize);
end;

procedure TTraceDebugInfo.SaveStack;
begin
  getmem(stack.stack, 4096);
  ReadProcessMemory(processhandle, pointer(c.{$ifdef cpu64}Rsp{$else}esp{$endif}), stack.stack, 4096, stack.savedsize);
end;

procedure TfrmTracer.addRecord;
var s,s2: string;
    i: integer;
    d: TTraceDebugInfo;

    a,address: ptrUint;
    referencedAddress: ptrUint;
    haserror: boolean;
begin
  //the debuggerthread is now paused so get the context and add it to the list

  address:=debuggerthread.CurrentThread.context.{$ifdef CPU64}rip{$else}eip{$endif};
  a:=address;
  s:=disassemble(a);

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
  d.c:=debuggerthread.CurrentThread.context^;
  d.instruction:=s;
  d.referencedAddress:=referencedAddress;
  d.fillbytes;

  if savestack then
    d.savestack;

  s:=inttohex(address,8)+' - '+s;

  ListBox1.Items.AddObject(s,d);
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
begin
  //set a breakpoint and when that breakpoint gets hit trace a number of instructions
  with TfrmTracerConfig.create(self) do
  begin
    if showmodal=mrok then
    begin
      dereference:= cbDereferenceAddresses.checked;
      savestack:= cbSaveStack.checked;

      tcount:=strtoint(edtMaxTrace.text);
      condition:=edtCondition.text;

      if startdebuggerifneeded then
        debuggerthread.setBreakAndTraceBreakpoint(self, memorybrowser.disassemblerview.SelectedAddress, tcount, condition);


    end;
    free;
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

procedure TfrmTracer.MenuItem3Click(Sender: TObject);
var z: Tstringlist;
i: integer;
t: TTraceDebugInfo;
c: PContext;
pref: string;
begin
  if savedialog1.Execute then
  begin
    z:=tstringlist.create;
    try
      for i:=0 to listbox1.count-1 do
      begin
        z.add(listbox1.Items[i]);
        t:=TTraceDebugInfo(listbox1.Items.Objects[i]);
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

procedure TfrmTracer.Button2Click(Sender: TObject);
begin
  stopsearch:=true;
end;

procedure TfrmTracer.MenuItem4Click(Sender: TObject);
var
i: integer;
c: PContext;
check: boolean;
begin
  if (sender = miSearchNext) then
    check:=true
  else
  begin
    check:=InputQuery('Search','Type the (LUA) condition you want to search for (Example: EAX==0x1234)', lastsearchstring);
    lastsearchstring:='return '+lastsearchstring;
  end;

  if check then
  begin
    stopsearch:=false;
    progressbar1.Position:=0;
    progressbar1.Max:=listbox1.Count;
    pnlSearch.visible:=true;

    i:=listbox1.itemindex+1;

    while (i<listbox1.count) and (not stopsearch) do
    begin
      c:=@TTraceDebugInfo(listbox1.Items.Objects[i]).c;
      if CheckIfConditionIsMetContext(c, lastsearchstring) then
      begin
        listbox1.ItemIndex:=i;
        listbox1.MakeCurrentVisible;
        listbox1Click(listbox1);
        break;
      end;


      inc(i);
      if (i mod 50)=0 then application.ProcessMessages;
    end;

    pnlSearch.visible:=false;
  end;
end;



procedure TfrmTracer.FormClose(Sender: TObject; var Action: TCloseAction);
var i: integer;
begin

  if debuggerthread<>nil then
    debuggerthread.stopBreakAndTrace(self);

  for i:=0 to listbox1.Items.Count-1 do
    TTraceDebugInfo(listbox1.Items.Objects[i]).Free;

  action:=cafree;
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
      ebxlabel.top:=eaxlabel.top+eaxlabel.height;
      ecxlabel.top:=ebxlabel.top+ebxlabel.height;
      edxlabel.top:=ecxlabel.top+ecxlabel.height;
      esilabel.top:=edxlabel.top+edxlabel.height;
      edilabel.top:=esilabel.top+esilabel.height;
      ebplabel.top:=edilabel.top+edilabel.height;
      esplabel.top:=ebplabel.top+ebplabel.height;

      p:=esplabel;

      setlength(RXlabels,8);

      for i:=8 to 15 do
      begin
        l:=tlabel.create(self);
        RXlabels[i-8]:=l;

        with l do
        begin
          parent:=p.parent;
          font:=p.font;
          left:=p.left;
          top:=p.top+p.height;
          cursor:=p.cursor;


          tag:=i;
          onclick:=p.onclick;
          OnMouseDown:=RegisterMouseDown;

        end;
        p:=l;
      end;

      eiplabel.top:=l.top+l.height;
      cslabel.top:=eiplabel.top+eiplabel.height+20;
      sslabel.top:=cslabel.top+cslabel.height;
      dslabel.top:=sslabel.top+sslabel.height;
      eslabel.top:=dslabel.top+dslabel.height;
      fslabel.top:=eslabel.top+eslabel.height;
      gslabel.top:=fslabel.top+fslabel.height;

      if clientheight<(gslabel.top+gslabel.height+button1.height+4) then
        clientheight:=gslabel.top+gslabel.height+button1.height+4;

    end;
  end;
end;

procedure TfrmTracer.ListBox1Click(Sender: TObject);
var temp: string;
    context: _context;
    t: TTraceDebugInfo;
    prefix: char;
begin
  configuredisplay;

  if listbox1.ItemIndex<>-1 then
  begin
    t:=TTraceDebugInfo(listbox1.Items.Objects[listbox1.ItemIndex]);

    lblinstruction.caption:=t.instruction;
    if dereference then
    begin
      if t.referencedAddress<>0 then
        lblAddressed.caption:=inttohex(t.referencedAddress,8)+' = '+DataToString(t.bytes, t.bytesize, t.datatype)
      else
        lblAddressed.caption:='';
    end else lblAddressed.Caption:='';


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

      temp:='R8 '+IntToHex(context.r8,8);
      if temp<>RXlabels[0].Caption then
      begin
        RXlabels[0].Font.Color:=clred;
        RXlabels[0].Caption:=temp;
      end else RXlabels[0].Font.Color:=clWindowText;

      temp:='R9 '+IntToHex(context.r9,8);
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
      fpp.SetContextPointer(@TTraceDebugInfo(listbox1.Items.Objects[listbox1.ItemIndex]).c);

    if Stackview<>nil then
      updatestackview;

  end;
end;

procedure TfrmTracer.updatestackview;
var di: TTraceDebugInfo;
begin
  if (Stackview<>nil) and (listbox1.ItemIndex<>-1) then
  begin
    //get stack
    di:=TTraceDebugInfo(listbox1.Items.Objects[listbox1.ItemIndex]);
    StackView.SetContextPointer(@di.c, di.stack.stack, di.stack.savedsize);
  end;
end;

procedure TfrmTracer.EAXLabelDblClick(Sender: TObject);
var s: string;
begin
  s:=tlabel(sender).Caption;
  s:=copy(s,5,8);

  memorybrowser.memoryaddress:=strtoint64('$'+s);
end;

procedure TfrmTracer.ListBox1DblClick(Sender: TObject);
begin
  memorybrowser.disassemblerview.SelectedAddress:=TTraceDebugInfo(listbox1.Items.Objects[listbox1.ItemIndex]).c.{$ifdef cpu64}rip{$else}Eip{$endif};
end;

procedure TfrmTracer.Panel1Resize(Sender: TObject);
begin
  button1.Top:=clientheight-button1.height-3;

  if sbShowstack.visible then
  begin
    sbShowFloats.top:=(clientheight div 2)-(sbshowFloats.height);
    sbShowstack.top:=(clientheight div 2);
    sbShowFloats.Left:=panel1.ClientWidth-sbshowfloats.width-2;
    sbShowstack.left:=sbshowfloats.left;
  end
  else
  begin
    sbShowFloats.top:=(clientheight div 2)-(sbshowFloats.height div 2);
    sbShowFloats.Left:=panel1.ClientWidth-sbshowfloats.width-2;
  end;
end;

procedure TfrmTracer.sbShowFloatsClick(Sender: TObject);
begin
  if listbox1.ItemIndex=-1 then exit;

  if fpp=nil then
  begin
    fpp:=frmFloatingPointPanelUnit.TfrmFloatingPointPanel.create(self); //fpc complains it can't find it, so added the unit in front
  end;

  fpp.Left:=self.left+self.Width;
  fpp.Top:=self.top;
  fpp.SetContextPointer(@TTraceDebugInfo(listbox1.Items.Objects[listbox1.ItemIndex]).c);
  fpp.show;//pop to foreground
end;

procedure TfrmTracer.FormShow(Sender: TObject);
begin
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
  {$i frmTracerUnit.lrs}

end.
