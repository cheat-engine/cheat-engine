unit frmTracerUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,disassembler,debugger,newkernelhandler, ExtCtrls;

type TTraceDebugInfo=class
  private
  public
    c: _CONTEXT;
end;

type
  TfrmTracer = class(TForm)
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
    Button1: TButton;
    Splitter1: TSplitter;
    dflabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure EAXLabelDblClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
    traceaddress: dword;
  public
    { Public declarations }
  end;

var
  frmTracer: TfrmTracer;

implementation

{$R *.dfm}

uses MemoryBrowserFormUnit;

//procedure TfrmTracer.

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
    begin
      debuggerthread.userisdebugging:=true;
      debuggerthread.traceaddress:=memorybrowser.dselected;
      debuggerthread.tracecount:=tcount;

      traceaddresS:=memorybrowser.dselected;

      togglebreakpoint(memorybrowser.dselected);
      memorybrowser.updatedisassemblerview;
    end;
  end;
end;

procedure TfrmTracer.FormClose(Sender: TObject; var Action: TCloseAction);
var i: integer;
begin
  if debuggerthread<>nil then
  begin
    debuggerthread.traceaddress:=0;
    debuggerthread.tracecount:=0;
  end;

  for i:=0 to listbox1.Items.Count-1 do
    TTraceDebugInfo(listbox1.Items.Objects[i]).Free;

  action:=cafree;

  if debuggerthread<>nil then
  begin
    ToggleBreakpoint(traceaddress);
    debuggerthread.traceaddress:=0;
  end;
  
  frmtracer:=nil;
end;

procedure TfrmTracer.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmTracer.ListBox1Click(Sender: TObject);
var temp: string;
    context: _context;
begin
  if listbox1.ItemIndex<>-1 then
  begin
    context:=TTraceDebugInfo(listbox1.Items.Objects[listbox1.ItemIndex]).c;

    temp:='EAX '+IntToHex(context.Eax,8);
    if temp<>eaxlabel.Caption then
    begin
      eaxlabel.Font.Color:=clred;
      eaxlabel.Caption:=temp;
    end else eaxlabel.Font.Color:=clWindowText;

    temp:='EBX '+IntToHex(context.Ebx,8);
    if temp<>ebxlabel.Caption then
    begin
      ebxlabel.Font.Color:=clred;
      ebxlabel.Caption:=temp;
    end else ebxlabel.Font.Color:=clWindowText;

    temp:='ECX '+IntToHex(context.ECx,8);
    if temp<>eCxlabel.Caption then
    begin
      eCXlabel.Font.Color:=clred;
      eCXlabel.Caption:=temp;
    end else eCXlabel.Font.Color:=clWindowText;

    temp:='EDX '+IntToHex(context.EDx,8);
    if temp<>eDxlabel.Caption then
    begin
      eDxlabel.Font.Color:=clred;
      eDxlabel.Caption:=temp;
    end else eDxlabel.Font.Color:=clWindowText;

    temp:='ESI '+IntToHex(context.ESI,8);
    if temp<>eSIlabel.Caption then
    begin
      eSIlabel.Font.Color:=clred;
      eSIlabel.Caption:=temp;
    end else eSIlabel.Font.Color:=clWindowText;

    temp:='EDI '+IntToHex(context.EDI,8);
    if temp<>eDIlabel.Caption then
    begin
      eDIlabel.Font.Color:=clred;
      eDIlabel.Caption:=temp;
    end else eDIlabel.Font.Color:=clWindowText;

    temp:='EBP '+IntToHex(context.EBP,8);
    if temp<>eBPlabel.Caption then
    begin
      eBPlabel.Font.Color:=clred;
      eBPlabel.Caption:=temp;
    end else eBPlabel.Font.Color:=clWindowText;

    temp:='ESP '+IntToHex(context.ESP,8);
    if temp<>eSPlabel.Caption then
    begin
      eSPlabel.Font.Color:=clred;
      eSPlabel.Caption:=temp;
    end else eSPlabel.Font.Color:=clWindowText;

    temp:='EIP '+IntToHex(context.EIP,8);
    if temp<>eIPlabel.Caption then
    begin
      eIPlabel.Font.Color:=clred;
      eIPlabel.Caption:=temp;
    end else eIPlabel.Font.Color:=clWindowText;

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
  end;
end;

procedure TfrmTracer.EAXLabelDblClick(Sender: TObject);
var s: string;
begin
  s:=tlabel(sender).Caption;
  s:=copy(s,5,8);

  memorybrowser.memoryaddress:=strtoint('$'+s);
  memorybrowser.refreshmb;
end;

procedure TfrmTracer.ListBox1DblClick(Sender: TObject);
begin
  memorybrowser.dselected:=TTraceDebugInfo(listbox1.Items.Objects[listbox1.ItemIndex]).c.Eip;
  memorybrowser.dselected2:=memorybrowser.dselected;
  memorybrowser.updatedisassemblerview;
end;

end.
