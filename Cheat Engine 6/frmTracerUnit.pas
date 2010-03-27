unit frmTracerUnit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,disassembler, NewKernelHandler, ExtCtrls,
  Buttons, LResources, frmFloatingPointPanelUnit, strutils, cefuncproc;

type TTraceDebugInfo=class
  private
  public
    c: _CONTEXT;
end;

type

  { TfrmTracer }

  TfrmTracer = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
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
    pflabel: TLabel;
    aflabel: TLabel;
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
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure EAXLabelDblClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure sbShowFloatsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    traceaddress: dword;
    fpp: TfrmFloatingPointPanel;
    isConfigured: boolean;

    RXlabels: array of TLabel;
    procedure configuredisplay;
  public
    { Public declarations }
    procedure addRecord;
  end;

implementation


uses cedebugger, debughelper, MemoryBrowserFormUnit;

procedure TfrmTracer.addRecord;
var s: string;
    i: integer;
    d: TTraceDebugInfo;

    a,prev: ptrUint;
begin
  //the debuggerthread is now paused so get the context and add it to the list

  a:=debuggerthread.CurrentThread.context.{$ifdef CPU64}rip{$else}eip{$endif};
  prev:=disassembler.previousopcode(a);

  s:=disassemble(prev);
  i:=posex('-',s);
  i:=posex('-',s,i+1);
  s:=copy(s,i+2,length(s));
  s:=inttohex(prev,8)+' - '+s;

  d:=TTraceDebugInfo.Create;
  d.c:=debuggerthread.CurrentThread.context^;
  ListBox1.Items.AddObject(s,d);
end;

procedure TfrmTracer.FormCreate(Sender: TObject);
var tcount: integer;
    tcounts: string;
begin
  //set a breakpoint and when that breakpoint gets hit trace a number of instructions
  tcounts:='1000';

  if inputquery('Cheat Engine Trace','Trace count',tcounts) then
  begin
    try
      tcount:=strtoint(tcounts);
    except
      raise exception.Create('That isn''t a valid count');
    end;

    if startdebuggerifneeded then
      debuggerthread.setBreakAndTraceBreakpoint(self, memorybrowser.disassemblerview.SelectedAddress, tcount);
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
begin
  configuredisplay;

  if listbox1.ItemIndex<>-1 then
  begin
    context:=TTraceDebugInfo(listbox1.Items.Objects[listbox1.ItemIndex]).c;

    temp:='EAX '+IntToHex(context.{$ifdef cpu64}rax{$else}Eax{$endif},8);
    if temp<>eaxlabel.Caption then
    begin
      eaxlabel.Font.Color:=clred;
      eaxlabel.Caption:=temp;
    end else eaxlabel.Font.Color:=clWindowText;

    temp:='EBX '+IntToHex(context.{$ifdef cpu64}rbx{$else}ebx{$endif},8);
    if temp<>ebxlabel.Caption then
    begin
      ebxlabel.Font.Color:=clred;
      ebxlabel.Caption:=temp;
    end else ebxlabel.Font.Color:=clWindowText;

    temp:='ECX '+IntToHex(context.{$ifdef cpu64}rcx{$else}ecx{$endif},8);
    if temp<>eCxlabel.Caption then
    begin
      eCXlabel.Font.Color:=clred;
      eCXlabel.Caption:=temp;
    end else eCXlabel.Font.Color:=clWindowText;

    temp:='EDX '+IntToHex(context.{$ifdef cpu64}rdx{$else}edx{$endif},8);
    if temp<>eDxlabel.Caption then
    begin
      eDxlabel.Font.Color:=clred;
      eDxlabel.Caption:=temp;
    end else eDxlabel.Font.Color:=clWindowText;

    temp:='ESI '+IntToHex(context.{$ifdef cpu64}rsi{$else}esi{$endif},8);
    if temp<>eSIlabel.Caption then
    begin
      eSIlabel.Font.Color:=clred;
      eSIlabel.Caption:=temp;
    end else eSIlabel.Font.Color:=clWindowText;

    temp:='EDI '+IntToHex(context.{$ifdef cpu64}rdi{$else}edi{$endif},8);
    if temp<>eDIlabel.Caption then
    begin
      eDIlabel.Font.Color:=clred;
      eDIlabel.Caption:=temp;
    end else eDIlabel.Font.Color:=clWindowText;

    temp:='EBP '+IntToHex(context.{$ifdef cpu64}rbp{$else}ebp{$endif},8);
    if temp<>eBPlabel.Caption then
    begin
      eBPlabel.Font.Color:=clred;
      eBPlabel.Caption:=temp;
    end else eBPlabel.Font.Color:=clWindowText;

    temp:='ESP '+IntToHex(context.{$ifdef cpu64}rsp{$else}esp{$endif},8);
    if temp<>eSPlabel.Caption then
    begin
      eSPlabel.Font.Color:=clred;
      eSPlabel.Caption:=temp;
    end else eSPlabel.Font.Color:=clWindowText;

    temp:='EIP '+IntToHex(context.{$ifdef cpu64}rip{$else}eip{$endif},8);
    if temp<>eIPlabel.Caption then
    begin
      eIPlabel.Font.Color:=clred;
      eIPlabel.Caption:=temp;
    end else eIPlabel.Font.Color:=clWindowText;

    {$ifdef cpu64}


    temp:='R8 '+IntToHex(context.r8,8);
    if temp<>RXlabels[0].Caption then
    begin
      RXlabels[0].Font.Color:=clred;
      RXlabels[0].Caption:=temp;
    end else RXlabels[0].Font.Color:=clWindowText;

    temp:='R9 '+IntToHex(context.r9,8);
    if temp<>RXlabels[9].Caption then
    begin
      RXlabels[9].Font.Color:=clred;
      RXlabels[9].Caption:=temp;
    end else RXlabels[9].Font.Color:=clWindowText;

    temp:='R10 '+IntToHex(context.r10,8);
    if temp<>RXlabels[10].Caption then
    begin
      RXlabels[10].Font.Color:=clred;
      RXlabels[10].Caption:=temp;
    end else RXlabels[10].Font.Color:=clWindowText;

    temp:='R11 '+IntToHex(context.r11,8);
    if temp<>RXlabels[11].Caption then
    begin
      RXlabels[11].Font.Color:=clred;
      RXlabels[11].Caption:=temp;
    end else RXlabels[11].Font.Color:=clWindowText;

    temp:='R12 '+IntToHex(context.r12,8);
    if temp<>RXlabels[12].Caption then
    begin
      RXlabels[12].Font.Color:=clred;
      RXlabels[12].Caption:=temp;
    end else RXlabels[12].Font.Color:=clWindowText;

    temp:='R13 '+IntToHex(context.r13,8);
    if temp<>RXlabels[13].Caption then
    begin
      RXlabels[13].Font.Color:=clred;
      RXlabels[13].Caption:=temp;
    end else RXlabels[13].Font.Color:=clWindowText;

    temp:='R14 '+IntToHex(context.r14,8);
    if temp<>RXlabels[14].Caption then
    begin
      RXlabels[14].Font.Color:=clred;
      RXlabels[14].Caption:=temp;
    end else RXlabels[14].Font.Color:=clWindowText;

    temp:='R15 '+IntToHex(context.r15,8);
    if temp<>RXlabels[15].Caption then
    begin
      RXlabels[15].Font.Color:=clred;
      RXlabels[15].Caption:=temp;
    end else RXlabels[15].Font.Color:=clWindowText;

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
  end;
end;

procedure TfrmTracer.EAXLabelDblClick(Sender: TObject);
var s: string;
begin
  s:=tlabel(sender).Caption;
  s:=copy(s,5,8);

  memorybrowser.memoryaddress:=strtoint('$'+s);
end;

procedure TfrmTracer.ListBox1DblClick(Sender: TObject);
begin
  memorybrowser.disassemblerview.SelectedAddress:=TTraceDebugInfo(listbox1.Items.Objects[listbox1.ItemIndex]).c.{$ifdef cpu64}rip{$else}Eip{$endif};
end;

procedure TfrmTracer.Panel1Resize(Sender: TObject);
begin
  button1.Top:=clientheight-button1.height-3;
  sbShowFloats.top:=(clientheight div 2)-(sbshowFloats.height div 2);
end;

procedure TfrmTracer.sbShowFloatsClick(Sender: TObject);
begin
  if listbox1.ItemIndex=-1 then exit;
  
  if fpp=nil then
    fpp:=TfrmFloatingPointPanel.create(self);

  fpp.Left:=self.left+self.Width;
  fpp.Top:=self.top;
  fpp.SetContextPointer(@TTraceDebugInfo(listbox1.Items.Objects[listbox1.ItemIndex]).c);
  fpp.show;//pop to foreground
end;

procedure TfrmTracer.FormShow(Sender: TObject);
begin
  sbShowFloats.Top:=(clientheight div 2)-(sbShowFloats.Height div 2);
end;

initialization
  {$i frmTracerUnit.lrs}

end.
