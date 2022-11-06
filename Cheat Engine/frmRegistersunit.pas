unit frmRegistersunit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport, LCLProc, math,
  {$endif}
  {$ifdef windows}
  win32proc, jwawindows, windows,
  {$endif}
  LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, StdCtrls, frmFloatingPointPanelUnit, NewKernelHandler,
  cefuncproc, LResources,Clipbrd, Menus, frmStackViewunit, betterControls, strutils, contexthandler;

type

{%region TRegisters }

  { TRegisters }

  TRegisters = class(TForm)
    EAXLabel: TLabel;
    lblCF: TLabel;
    lblPF: TLabel;
    lblAF: TLabel;
    lblZF: TLabel;
    lblSF: TLabel;
    lblDF: TLabel;
    lblOF: TLabel;
    EBPlabel: TLabel;
    EBXlabel: TLabel;
    ECXlabel: TLabel;
    EDIlabel: TLabel;
    EDXlabel: TLabel;
    EIPlabel: TLabel;
    ESIlabel: TLabel;
    ESPlabel: TLabel;
    miDisassembleMemoryRegion: TMenuItem;
    miBrowseMemoryRegion: TMenuItem;
    miCopyRegisterValueToClipboard: TMenuItem;
    miSeparator1: TMenuItem;
    pnlFlags: TPanel;
    rImageList: TImageList;
    miCopyAllRegistersToClipboard: TMenuItem;
    Panel1: TPanel;
    LabelRegisters: TLabel;
    PanelRegistersList: TPanel;
    Panel4: TPanel;
    registerLabelContextMenu: TPopupMenu;
    sbShowFloats: TSpeedButton;
    sbShowStack: TSpeedButton;
    Shape1: TShape;
    Panel2: TPanel;
    procedure FormShow(Sender: TObject);
    procedure miBrowseMemoryRegionClick(Sender: TObject);
    procedure miCopyAllRegistersToClipboardClick(Sender: TObject);
    procedure CopyRegisterValueToClipboardMouseRightClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miCopyRegisterValueToClipboardClick(Sender: TObject);
    procedure miDisassembleMemoryRegionClick(Sender: TObject);
    procedure sbShowFloatsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BrowseMemoryRegionClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbShowStackClick(Sender: TObject);
  private
    { Private declarations }
    r8label: TLabel;
    r9label: TLabel;
    r10label: TLabel;
    r11label: TLabel;
    r12label: TLabel;
    r13label: TLabel;
    r14label: TLabel;
    r15label: TLabel;

    context: PContext;
    contexthandler: TContextInfo;

    fpp: TfrmFloatingPointPanel;
    stack: record
      savedsize: dword;
      stack: pbyte;
    end;

    stackview: TfrmStackView;

  public
    { Public declarations }
    procedure SetContextPointer(newcontext: PContext; _stack: PByte; stacksize: integer);
  end;

{%endregion TRegisters }

implementation

uses MemoryBrowserFormUnit, ProcessHandlerUnit;

resourcestring
  rsLabelRegisterHint = 'DoubleClick: Browse this memory region.' + LineEnding + 'Right Mouse Click: Register Value will be copied to Clipboard and Context Menu will open.';
  rsNoDescription = 'No Description';

procedure TRegisters.SetContextPointer(newcontext: PContext; _stack: PByte; stacksize: integer);
var
  pre,f: string;
  sizeNeeded: integer;

  gpRegList, gpFlagList: PContextElementRegisterList;
  r: PContextElement_register;

  i: integer;
  oldContextHandler:TContextInfo;
  c: TControl;
  l: TLabel absolute c;
begin
  oldContextHandler:=contexthandler;

  self.context:=newcontext;
  contexthandler:=getBestContextHandler;



  if oldContextHandler<>contexthandler then
  begin
    //clear the old screen
    BeginFormUpdate;
    while PanelRegistersList.controlcount>0 do
      PanelRegistersList.controls[0].Free;

    while pnlFlags.ControlCount>0 do
      pnlFlags.controls[0].Free;

    //fill the fields according to the contexthandler general and flags list

    gpRegList:=contexthandler.getGeneralPurposeRegisters;
    gpFlagList:=contexthandler.getGeneralPurposeFlags;

    for i:=0 to length(gpRegList^)-1 do
    begin
      l:=TLabel.create(self);
      l.OnDblClick:=BrowseMemoryRegionClick;
      l.OnMouseDown:=CopyRegisterValueToClipboardMouseRightClick;
      l.Cursor:=crHandPoint;
      l.tag:=PtrInt(@gpRegList^[i]);
      l.parent:=PanelRegistersList;
      l.PopupMenu:=registerLabelContextMenu;
      l.ShowHint:=true;
      l.Hint:=rsLabelRegisterHint;
    end;

    for i:=0 to length(gpFlagList^)-1 do
    begin
      l:=TLabel.create(self);
      l.OnMouseDown:=CopyRegisterValueToClipboardMouseRightClick;
      l.Cursor:=crHandPoint;
      l.tag:=PtrInt(@gpFlagList^[i]);
      l.parent:=pnlFlags;
      l.PopupMenu:=registerLabelContextMenu;
    end;

    EndFormUpdate;
  end;

  for i:=0 to PanelRegistersList.ControlCount-1 do
  begin
    c:=PanelRegistersList.Controls[i];
    if c is tlabel then
    begin
      r:=PContextElement_register(c.tag);
      if r<>nil then
      begin
        if r.size=4 then
          c.Caption:=format(padright(r.name, contexthandler.GeneralPurposeRegisterMaxCharCount)+' %.8x',[r.getDword(context)])
        else if r.size=8 then
          c.Caption:=format(padright(r.name, contexthandler.GeneralPurposeRegisterMaxCharCount)+' %.16x',[r.getQword(context)])
        else
          c.caption:=padright(r.name, contexthandler.GeneralPurposeRegisterMaxCharCount)+' NYI!';
      end;
    end;
  end;

  for i:=0 to pnlFlags.ControlCount-1 do
  begin
    c:=pnlFlags.Controls[i];
    if c is tlabel then
    begin
      r:=PContextElement_register(c.tag);
      if r<>nil then
        c.Caption:=format(padright(r.name, contexthandler.GeneralPurposeFlagMaxCharCount)+' %d',[r.getFlag(context)]);
    end;
  end;





  if _stack<>nil then
  begin
    if stack.stack<>nil then //free old stack copy
      freememandnil(stack.stack);

    getmem(stack.stack, stacksize);
    stack.savedsize:=stacksize;

    copymemory(stack.stack, _stack, stacksize);
  end;

  panel1.autosize:=true;


  autosize:=false;

  autosize:=true;
  DoAutoSize;

  autosize:=false;
  if panel1.clientwidth>panel2.width then
    width:=width+panel2.width+8;
end;

procedure TRegisters.sbShowFloatsClick(Sender: TObject);
begin
  if fpp=nil then
    fpp:=TfrmFloatingPointPanel.create(self);


  fpp.Left:=self.left+self.Width;
  fpp.Top:=self.top;
  fpp.SetContextPointer(context);
  fpp.show;//pop to foreground
end;

procedure TRegisters.BrowseMemoryRegionClick(Sender: TObject);
var e: PContextElement_register;
begin
  e:=PContextElement_register(tlabel(sender).tag);
  if e^.entrytype=0 then
  begin
    memorybrowser.memoryaddress:=e^.getValue(context);
    memorybrowser.Show;
  end;
end;

procedure TRegisters.CopyRegisterValueToClipboardMouseRightClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var s: string;
i: integer;
begin
  if button = mbright then
  begin
    if (sender is TLabel) then
    begin
      s:=trim(tlabel(sender).Caption);
      i:=pos(' ',s);
      if i>0 then //should always be true
      begin
        s:=copy(s,i+1,length(s));
        clipboard.AsText:=s;
      end;
    end;
  end;
end;

procedure TRegisters.FormShow(Sender: TObject);
const CCHILDREN_TITLEBAR=5;
type
  TTitleBarInfoEx=record
    cbSize: DWORD;
    rcTitleBar: TRECT;
    rgstate: array [0..CCHILDREN_TITLEBAR] of DWORD;
    rgrect: array [0..CCHILDREN_TITLEBAR] of TRECT;
  end;
var
  tbi: TTITLEBARINFOEX;
  i: integer;
  widthNeeded: integer;
begin
  i:=GetFontData(font.Handle).Height;
  if i>-13 then
    i:=-13;

  PanelRegistersList.Font.Height:=i;
  PanelRegistersList.Font.Color:=Font.Color;

  pnlFlags.Font.Height:=i;
  pnlFlags.Font.Color:=Font.Color;

  shape1.brush.color:=clWindowtext;
  shape1.pen.color:=clWindowtext;

  widthNeeded:=canvas.TextWidth('XX  '+caption+'  XX');





  {$ifdef windows}
  if WindowsVersion>=wvVista then
  begin
    tbi.cbSize:=sizeof(tbi);
    sendmessage(handle, WM_GETTITLEBARINFOEX, 0, ptruint(@tbi));

    DoAutoSize;

    autosize:=false;
    i:=tbi.rcTitleBar.Right-tbi.rcTitleBar.Left;
    dec(i,tbi.rgrect[5].Right-tbi.rgrect[5].left);
    dec(i,tbi.rgrect[3].Right-tbi.rgrect[3].left);
    dec(i,tbi.rgrect[2].Right-tbi.rgrect[2].left);
    dec(i, GetSystemMetrics(SM_CXSIZE));
    dec(i, GetSystemMetrics(SM_CXPADDEDBORDER));
    dec(i, GetSystemMetrics(SM_CXBORDER));

    widthNeeded:=max(panel1.width+panel2.width+4, widthneeded);

    //if i<widthNeeded then
    Width:=widthNeeded;
  end;
  {$endif}


end;

{%region registerLabelContextMenu }
  
procedure TRegisters.miCopyRegisterValueToClipboardClick(Sender: TObject);
var
  digits: integer;
  registerValue: ptrUint;
  registerStringValue, f: string;
begin

  if not (registerLabelContextMenu.PopupComponent is TLabel) then
    exit;

  clipboard.Clear;

  if processhandler.is64bit then
    digits:=16
  else
    digits:=8;


  registerValue:=PContextElement_register(tlabel(registerLabelContextMenu.PopupComponent).tag)^.getValue(context);

  registerStringValue:=inttohex(registerValue,digits);

  clipboard.astext:=registerStringValue;

end;

procedure TRegisters.miBrowseMemoryRegionClick(Sender: TObject);

begin

  if not (registerLabelContextMenu.PopupComponent is TLabel) then
    exit;

  memorybrowser.memoryaddress:=PContextElement_register(tlabel(registerLabelContextMenu.PopupComponent).tag)^.getValue(context);
  memorybrowser.Show;

end;

procedure TRegisters.miDisassembleMemoryRegionClick(Sender: TObject);
begin

  if not (registerLabelContextMenu.PopupComponent is TLabel) then
    exit;

  memorybrowser.disassemblerview.SelectedAddress:=PContextElement_register(tlabel(registerLabelContextMenu.PopupComponent).tag)^.getValue(context);
  memorybrowser.Show;

end;

procedure TRegisters.miCopyAllRegistersToClipboardClick(Sender: TObject);
var
  i: integer;
  s: tstringlist;
  pref: string;
  digits: integer;

  reglist: PContextElementRegisterList;
begin

  s:=TStringList.create;

  if processhandler.is64bit then
    digits:=16
  else
    digits:=8;

  reglist:=contexthandler.getGeneralPurposeRegisters;
  for i:=0 to length(reglist^)-1 do
    s.add(reglist^[i].name+' = '+inttohex(reglist^[i].getValue(context),digits));


  clipboard.astext:=s.text;
  s.free;
end;

{%endregion registerLabelContextMenu }

procedure TRegisters.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if fpp<>nil then
  begin
    fpp.close;
    fpp.free;
  end;


  if stackview<>nil then
  begin
    stackview.close;
    stackview.close;
  end;

  if stack.stack<>nil then
    freememandnil(stack.stack);

  action:=cafree;
end;

procedure TRegisters.FormResize(Sender: TObject);
begin

end;

procedure TRegisters.sbShowStackClick(Sender: TObject);
begin
  if stack.stack=nil then exit;

  if Stackview=nil then
    stackview:=TfrmStackView.create(self);

  stackview.SetContextPointer(context, stack.stack, stack.savedsize);
  stackview.show;
end;

initialization
  {$i frmRegistersunit.lrs}

end.
