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
  cefuncproc, LResources,Clipbrd, Menus, frmStackViewunit, betterControls;

type

{%region TRegisters }

  TRegisters = class(TForm)
    EAXLabel: TLabel;
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
    fpp: TfrmFloatingPointPanel;
    stack: record
      savedsize: dword;
      stack: pbyte;
    end;

    stackview: TfrmStackView;

    function ConvertTagIndexToValue(tag: integer): ptrUint;
  public
    { Public declarations }
    procedure SetContextPointer(context: PContext; _stack: PByte; stacksize: integer);
  end;

{%endregion TRegisters }

implementation

uses MemoryBrowserFormUnit, ProcessHandlerUnit;

resourcestring
  rsLabelRegisterHint = 'DoubleClick: Browse this memory region.' + LineEnding + 'Right Mouse Click: Register Value will be copied to Clipboard and Context Menu will open.';
  rsNoDescription = 'No Description';

procedure TRegisters.SetContextPointer(context: PContext; _stack: PByte; stacksize: integer);
var
  pre,f: string;
  sizeNeeded: integer;
begin
  self.context:=context;

  //ebxlabel.top:=eaxlabel.top+eaxlabel.height;
  //ecxlabel.top:=ebxlabel.top+ebxlabel.height;
  //edxlabel.top:=ecxlabel.top+ecxlabel.height;
  //esilabel.top:=edxlabel.top+edxlabel.height;
  //edilabel.top:=esilabel.top+esilabel.height;
  //ebplabel.top:=edilabel.top+edilabel.height;
  //esplabel.top:=ebplabel.top+ebplabel.height;

  EAXLabel.ShowHint:=true;
  EAXLabel.Hint:=rsLabelRegisterHint;
  EBXlabel.ShowHint:=true;
  EBXlabel.Hint:=rsLabelRegisterHint;
  ECXlabel.ShowHint:=true;
  ECXlabel.Hint:=rsLabelRegisterHint;
  EDXlabel.ShowHint:=true;
  EDXlabel.Hint:=rsLabelRegisterHint;
  ESIlabel.ShowHint:=true;
  ESIlabel.Hint:=rsLabelRegisterHint;
  EDIlabel.ShowHint:=true;
  EDIlabel.Hint:=rsLabelRegisterHint;
  ESPlabel.ShowHint:=true;
  ESPlabel.Hint:=rsLabelRegisterHint;
  EBPlabel.ShowHint:=true;
  EBPlabel.Hint:=rsLabelRegisterHint;
  EIPlabel.ShowHint:=true;
  EIPlabel.Hint:=rsLabelRegisterHint;


  if processhandler.is64Bit then
  begin
    pre:='R';
    f:='%0.16x';

    if R8Label=nil then
    begin
      r8label:=TLabel.create(self);
      r8label.OnDblClick:=BrowseMemoryRegionClick;
      r8label.OnMouseDown:=CopyRegisterValueToClipboardMouseRightClick;
      r8label.Cursor:=crHandPoint;
      r8label.tag:=9;
      r8label.parent:=PanelRegistersList;
      r8label.PopupMenu:=registerLabelContextMenu;
      r8label.ShowHint:=true;
      r8label.Hint:=rsLabelRegisterHint;

      r8label.AnchorSideLeft.Control:=EIPlabel;
      r8label.AnchorSideTop.Control:=EIPlabel;
      r8label.AnchorSideTop.Side:=asrBottom;
      r8label.AnchorSideRight.Control:=EIPlabel;
      r8label.AnchorSideRight.Side:=asrBottom;
    end;

    if R9Label=nil then
    begin
      r9label:=TLabel.create(self);
      r9label.OnDblClick:=BrowseMemoryRegionClick;
      r9label.OnMouseDown:=CopyRegisterValueToClipboardMouseRightClick;
      r9label.Cursor:=crHandPoint;
      r9label.tag:=10;
      r9label.parent:=PanelRegistersList;
      r9label.PopupMenu:=registerLabelContextMenu;
      r9label.ShowHint:=true;
      r9label.Hint:=rsLabelRegisterHint;

      r9label.AnchorSideLeft.Control:=r8label;
      r9label.AnchorSideTop.Control:=r8label;
      r9label.AnchorSideTop.Side:=asrBottom;
      r9label.AnchorSideRight.Control:=r8label;
      r9label.AnchorSideRight.Side:=asrBottom;
    end;

    if R10Label=nil then
    begin
      r10label:=TLabel.create(self);
      r10label.OnDblClick:=BrowseMemoryRegionClick;
      r10label.OnMouseDown:=CopyRegisterValueToClipboardMouseRightClick;
      r10label.Cursor:=crHandPoint;
      r10label.tag:=11;
      r10label.parent:=PanelRegistersList;
      r10label.PopupMenu:=registerLabelContextMenu;
      r10label.ShowHint:=true;
      r10label.Hint:=rsLabelRegisterHint;

      r10label.AnchorSideLeft.Control:=r9label;
      r10label.AnchorSideTop.Control:=r9label;
      r10label.AnchorSideTop.Side:=asrBottom;
      r10label.AnchorSideRight.Control:=r9label;
      r10label.AnchorSideRight.Side:=asrBottom;
    end;

    if R11Label=nil then
    begin
      r11label:=TLabel.create(self);
      r11label.OnDblClick:=BrowseMemoryRegionClick;
      r11label.OnMouseDown:=CopyRegisterValueToClipboardMouseRightClick;
      r11label.Cursor:=crHandPoint;
      r11label.tag:=12;
      r11label.parent:=PanelRegistersList;
      r11label.PopupMenu:=registerLabelContextMenu;
      r11label.ShowHint:=true;
      r11label.Hint:=rsLabelRegisterHint;

      r11label.AnchorSideLeft.Control:=r10label;
      r11label.AnchorSideTop.Control:=r10label;
      r11label.AnchorSideTop.Side:=asrBottom;
      r11label.AnchorSideRight.Control:=r10label;
      r11label.AnchorSideRight.Side:=asrBottom;
    end;

    if R12Label=nil then
    begin
      r12label:=TLabel.create(self);
      r12label.OnDblClick:=BrowseMemoryRegionClick;
      r12label.OnMouseDown:=CopyRegisterValueToClipboardMouseRightClick;
      r12label.Cursor:=crHandPoint;
      r12label.tag:=13;
      r12label.parent:=PanelRegistersList;
      r12label.PopupMenu:=registerLabelContextMenu;
      r12label.ShowHint:=true;
      r12label.Hint:=rsLabelRegisterHint;

      r12label.AnchorSideLeft.Control:=r11label;
      r12label.AnchorSideTop.Control:=r11label;
      r12label.AnchorSideTop.Side:=asrBottom;
      r12label.AnchorSideRight.Control:=r11label;
      r12label.AnchorSideRight.Side:=asrBottom;
    end;

    if R13Label=nil then
    begin
      r13label:=TLabel.create(self);
      r13label.OnDblClick:=BrowseMemoryRegionClick;
      r13label.OnMouseDown:=CopyRegisterValueToClipboardMouseRightClick;
      r13label.Cursor:=crHandPoint;
      r13label.tag:=14;
      r13label.parent:=PanelRegistersList;
      r13label.PopupMenu:=registerLabelContextMenu;
      r13label.ShowHint:=true;
      r13label.Hint:=rsLabelRegisterHint;

      r13label.AnchorSideLeft.Control:=r12label;
      r13label.AnchorSideTop.Control:=r12label;
      r13label.AnchorSideTop.Side:=asrBottom;
      r13label.AnchorSideRight.Control:=r12label;
      r13label.AnchorSideRight.Side:=asrBottom;
    end;

    if R14Label=nil then
    begin
      r14label:=TLabel.create(self);
      r14label.OnDblClick:=BrowseMemoryRegionClick;
      r14label.OnMouseDown:=CopyRegisterValueToClipboardMouseRightClick;
      r14label.Cursor:=crHandPoint;
      r14label.tag:=15;
      r14label.parent:=PanelRegistersList;
      r14label.PopupMenu:=registerLabelContextMenu;
      r14label.ShowHint:=true;
      r14label.Hint:=rsLabelRegisterHint;

      r14label.AnchorSideLeft.Control:=r13label;
      r14label.AnchorSideTop.Control:=r13label;
      r14label.AnchorSideTop.Side:=asrBottom;
      r14label.AnchorSideRight.Control:=r13label;
      r14label.AnchorSideRight.Side:=asrBottom;
    end;

    if R15Label=nil then
    begin
      r15label:=TLabel.create(self);
      r15label.OnDblClick:=BrowseMemoryRegionClick;
      r15label.OnMouseDown:=CopyRegisterValueToClipboardMouseRightClick;
      r15label.Cursor:=crHandPoint;
      r15label.tag:=16;
      r15label.parent:=PanelRegistersList;
      r15label.PopupMenu:=registerLabelContextMenu;
      r15label.ShowHint:=true;
      r15label.Hint:=rsLabelRegisterHint;

      r15label.AnchorSideLeft.Control:=r14label;
      r15label.AnchorSideTop.Control:=r14label;
      r15label.AnchorSideTop.Side:=asrBottom;
      r15label.AnchorSideRight.Control:=r14label;
      r15label.AnchorSideRight.Side:=asrBottom;
    end;


    eiplabel.BringToFront;
  end
  else
  begin
    pre:='E';
    f:='%0.8x';
    //eiplabel.top:=esplabel.top+ebxlabel.top-eaxlabel.top;
  end;


  EAXLabel.Caption:=format(pre+'AX '+f,[context.{$ifdef cpu64}Rax{$else}Eax{$endif}]);
  EBXLabel.Caption:=format(pre+'BX '+f,[context.{$ifdef cpu64}Rbx{$else}Ebx{$endif}]);
  ECXLabel.Caption:=format(pre+'CX '+f,[context.{$ifdef cpu64}Rcx{$else}Ecx{$endif}]);
  EDXLabel.Caption:=format(pre+'DX '+f,[context.{$ifdef cpu64}Rdx{$else}Edx{$endif}]);
  ESILabel.Caption:=format(pre+'SI '+f,[context.{$ifdef cpu64}Rsi{$else}Esi{$endif}]);
  EDILabel.Caption:=format(pre+'DI '+f,[context.{$ifdef cpu64}Rdi{$else}Edi{$endif}]);
  EBPLabel.Caption:=format(pre+'BP '+f,[context.{$ifdef cpu64}Rbp{$else}Ebp{$endif}]);
  ESPLabel.Caption:=format(pre+'SP '+f,[context.{$ifdef cpu64}Rsp{$else}Esp{$endif}]);
  EIPLabel.Caption:=format(pre+'IP '+f,[context.{$ifdef cpu64}Rip{$else}Eip{$endif}]);

  {$ifdef cpu64}

  if processhandler.is64Bit then
  begin
    R8Label.Caption:=format(' R8 '+f,[context.R8]);
    R9Label.Caption:=format(' R9 '+f,[context.R9]);
    R10Label.Caption:=format('R10 '+f,[context.R10]);
    R11Label.Caption:=format('R11 '+f,[context.R11]);
    R12Label.Caption:=format('R12 '+f,[context.R12]);
    R13Label.Caption:=format('R13 '+f,[context.R13]);
    R14Label.Caption:=format('R14 '+f,[context.R14]);
    R15Label.Caption:=format('R15 '+f,[context.R15]);
  end;
  {$endif}

  if r8label<>nil then r8label.visible:=processhandler.is64Bit;
  if r9label<>nil then r9label.visible:=processhandler.is64Bit;
  if r10label<>nil then r10label.visible:=processhandler.is64Bit;
  if r11label<>nil then r11label.visible:=processhandler.is64Bit;
  if r12label<>nil then r12label.visible:=processhandler.is64Bit;
  if r13label<>nil then r13label.visible:=processhandler.is64Bit;
  if r14label<>nil then r14label.visible:=processhandler.is64Bit;
  if r15label<>nil then r15label.visible:=processhandler.is64Bit;


  sizeNeeded:=PanelRegistersList.top+eiplabel.top+eiplabel.height+20;

  if r15label<>nil then
  begin
    sizeNeeded+= 10 * eiplabel.height;
  end;

  sizeNeeded:=max(sizeNeeded, self.constraints.MinHeight);
  self.constraints.MinHeight:=sizeNeeded;

  sizeNeeded:=eaxlabel.left+eaxlabel.Canvas.TextWidth(eaxlabel.caption)+panel2.width+10;
  sizeNeeded:=max(sizeNeeded, self.constraints.MinWidth);
  self.constraints.MinWidth:=sizeNeeded;

  if _stack<>nil then
  begin
    if stack.stack<>nil then //free old stack copy
      freememandnil(stack.stack);

    getmem(stack.stack, stacksize);
    stack.savedsize:=stacksize;

    copymemory(stack.stack, _stack, stacksize);
  end;
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
begin
  memorybrowser.memoryaddress:=ConvertTagIndexToValue(tlabel(sender).tag);
  memorybrowser.Show;
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
  shape1.color:=clWindowtext;

  widthNeeded:=canvas.TextWidth('XX  '+caption+'  XX');

  constraints.MinWidth:=max(constraints.MinWidth, widthNeeded);

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

    if i<widthNeeded then
      Width:=max(Width, width+(widthNeeded-i));
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
  begin
    digits:=16;
  end
  else
  begin
    digits:=8;
  end;

  registerValue:=ConvertTagIndexToValue(tlabel(registerLabelContextMenu.PopupComponent).tag);

  registerStringValue:=inttohex(registerValue,digits);

  clipboard.astext:=registerStringValue;

end;

procedure TRegisters.miBrowseMemoryRegionClick(Sender: TObject);

begin

  if not (registerLabelContextMenu.PopupComponent is TLabel) then
    exit;

  memorybrowser.memoryaddress:=ConvertTagIndexToValue(tlabel(registerLabelContextMenu.PopupComponent).tag);
  memorybrowser.Show;

end;

procedure TRegisters.miDisassembleMemoryRegionClick(Sender: TObject);
begin

  if not (registerLabelContextMenu.PopupComponent is TLabel) then
    exit;

  memorybrowser.disassemblerview.SelectedAddress:=ConvertTagIndexToValue(tlabel(registerLabelContextMenu.PopupComponent).tag);
  memorybrowser.Show;

end;

procedure TRegisters.miCopyAllRegistersToClipboardClick(Sender: TObject);
var
  i: integer;
  s: tstringlist;
  pref: string;
  digits: integer;
begin

  s:=TStringList.create;

  if processhandler.is64bit then
  begin
    pref:='R';
    digits:=16;
  end
  else
  begin
    pref:='E';
    digits:=8;
  end;

  s.add(pref+'AX = '+inttohex(context.{$ifdef cpu64}Rax{$else}Eax{$endif},digits));
  s.add(pref+'BX = '+inttohex(context.{$ifdef cpu64}Rbx{$else}Ebx{$endif},digits));
  s.add(pref+'CX = '+inttohex(context.{$ifdef cpu64}Rcx{$else}Ecx{$endif},digits));
  s.add(pref+'DX = '+inttohex(context.{$ifdef cpu64}Rdx{$else}Edx{$endif},digits));
  s.add(pref+'SI = '+inttohex(context.{$ifdef cpu64}Rsi{$else}Esi{$endif},digits));
  s.add(pref+'DI = '+inttohex(context.{$ifdef cpu64}Rdi{$else}Edi{$endif},digits));
  s.add(pref+'BP = '+inttohex(context.{$ifdef cpu64}Rbp{$else}Ebp{$endif},digits));
  s.add(pref+'SP = '+inttohex(context.{$ifdef cpu64}Rsp{$else}Esp{$endif},digits));
  s.add(pref+'IP = '+inttohex(context.{$ifdef cpu64}Rip{$else}Eip{$endif},digits));

  {$ifdef cpu64}
  if processhandler.is64Bit then
  begin
    s.add('R8  = '+inttohex(context.R8,16));
    s.add('R9  = '+inttohex(context.R9,16));
    s.add('R10 = '+inttohex(context.R10,16));
    s.add('R11 = '+inttohex(context.R11,16));
    s.add('R12 = '+inttohex(context.R12,16));
    s.add('R13 = '+inttohex(context.R13,16));
    s.add('R14 = '+inttohex(context.R14,16));
    s.add('R15 = '+inttohex(context.R15,16));
  end;
  {$endif}

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

function TRegisters.ConvertTagIndexToValue(tag: integer): ptrUint;
begin
  result:=0;
  case tag of
    0: result:=context.{$ifdef cpu64}Rax{$else}Eax{$endif};
    1: result:=context.{$ifdef cpu64}Rbx{$else}Ebx{$endif};
    2: result:=context.{$ifdef cpu64}Rcx{$else}Ecx{$endif};
    3: result:=context.{$ifdef cpu64}Rdx{$else}Edx{$endif};
    4: result:=context.{$ifdef cpu64}Rsi{$else}Esi{$endif};
    5: result:=context.{$ifdef cpu64}Rdi{$else}Edi{$endif};
    6: result:=context.{$ifdef cpu64}Rbp{$else}Ebp{$endif};
    7: result:=context.{$ifdef cpu64}Rsp{$else}Esp{$endif};
    8: result:=context.{$ifdef cpu64}Rip{$else}Eip{$endif};
    {$ifdef cpu64}
    9: result:=context.r8;
    10: result:=context.r9;
    11: result:=context.r10;
    12: result:=context.r11;
    13: result:=context.r12;
    14: result:=context.r13;
    15: result:=context.r14;
    16: result:=context.r15;
    {$endif}
  end;
end;

procedure TRegisters.FormResize(Sender: TObject);
begin
  sbShowFloats.Top:=(clientheight div 2)-sbShowFloats.Height;
  sbShowStack.top:=sbShowFloats.top+sbShowFloats.height;
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
