unit frmRegistersunit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, StdCtrls, frmFloatingPointPanelUnit, NewKernelHandler,
  cefuncproc, LResources,Clipbrd, frmStackViewunit;

type

  { TRegisters }

  TRegisters = class(TForm)
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
    Label14: TLabel;
    sbShowStack: TSpeedButton;
    Shape1: TShape;
    Panel2: TPanel;
    sbShowFloats: TSpeedButton;
    procedure RegisterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbShowFloatsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EAXLabelDblClick(Sender: TObject);
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

    function TagToValue(tag: integer): ptrUint;
  public
    { Public declarations }
    procedure SetContextPointer(context: PContext; _stack: PByte; stacksize: integer);
  end;

implementation


uses MemoryBrowserFormUnit;


procedure TRegisters.SetContextPointer(context: PContext; _stack: PByte; stacksize: integer);
var pre,f: string;
begin
  self.context:=context;

  ebxlabel.top:=eaxlabel.top+eaxlabel.height;
  ecxlabel.top:=ebxlabel.top+ebxlabel.height;
  edxlabel.top:=ecxlabel.top+ecxlabel.height;
  esilabel.top:=edxlabel.top+edxlabel.height;
  edilabel.top:=esilabel.top+esilabel.height;
  ebplabel.top:=edilabel.top+edilabel.height;
  esplabel.top:=ebplabel.top+ebplabel.height;


  if processhandler.is64Bit then
  begin
    pre:='R';
    f:='%0.16x';

    if R8Label=nil then
    begin
      r8label:=TLabel.create(self);
      r8label.font:=eaxlabel.Font;
      r8label.left:=eaxlabel.left;
      r8label.top:=esplabel.Top+esplabel.Height;
      r8label.height:=eaxlabel.height;
      r8label.OnDblClick:=EAXLabelDblClick;
      r8label.OnMouseDown:=RegisterMouseDown;
      r8label.tag:=9;
      r8label.parent:=panel1;
    end;

    if R9Label=nil then
    begin
      r9label:=TLabel.create(self);
      r9label.font:=eaxlabel.Font;
      r9label.left:=eaxlabel.left;
      r9label.top:=r8label.Top+r8label.Height;
      r9label.height:=eaxlabel.height;
      r9label.OnDblClick:=EAXLabelDblClick;
      r9label.OnMouseDown:=RegisterMouseDown;
      r9label.tag:=10;
      r9label.parent:=panel1;
    end;

    if R10Label=nil then
    begin
      r10label:=TLabel.create(self);
      r10label.font:=eaxlabel.Font;
      r10label.left:=eaxlabel.left;
      r10label.top:=r9label.Top+r9label.Height;
      r10label.height:=eaxlabel.height;
      r10label.OnDblClick:=EAXLabelDblClick;
      r10label.OnMouseDown:=RegisterMouseDown;
      r10label.tag:=11;
      r10label.parent:=panel1;
    end;

    if R11Label=nil then
    begin
      r11label:=TLabel.create(self);
      r11label.font:=eaxlabel.Font;
      r11label.left:=eaxlabel.left;
      r11label.top:=r10label.Top+r10label.Height;
      r11label.height:=eaxlabel.height;
      r11label.OnDblClick:=EAXLabelDblClick;
      r11label.OnMouseDown:=RegisterMouseDown;
      r11label.tag:=12;
      r11label.parent:=panel1;
    end;

    if R12Label=nil then
    begin
      r12label:=TLabel.create(self);
      r12label.font:=eaxlabel.Font;
      r12label.left:=eaxlabel.left;
      r12label.top:=r11label.Top+r11label.Height;
      r12label.height:=eaxlabel.height;
      r12label.OnDblClick:=EAXLabelDblClick;
      r12label.OnMouseDown:=RegisterMouseDown;
      r12label.tag:=13;
      r12label.parent:=panel1;
    end;

    if R13Label=nil then
    begin
      r13label:=TLabel.create(self);
      r13label.font:=eaxlabel.Font;
      r13label.left:=eaxlabel.left;
      r13label.top:=r12label.Top+r12label.Height;
      r13label.height:=eaxlabel.height;
      r13label.OnDblClick:=EAXLabelDblClick;
      r13label.OnMouseDown:=RegisterMouseDown;
      r13label.tag:=14;
      r13label.parent:=panel1;
    end;

    if R14Label=nil then
    begin
      r14label:=TLabel.create(self);
      r14label.font:=eaxlabel.Font;
      r14label.left:=eaxlabel.left;
      r14label.top:=r13label.Top+r13label.Height;
      r14label.height:=eaxlabel.height;
      r14label.OnDblClick:=EAXLabelDblClick;
      r14label.OnMouseDown:=RegisterMouseDown;
      r14label.tag:=15;
      r14label.parent:=panel1;
    end;

    if R15Label=nil then
    begin
      r15label:=TLabel.create(self);
      r15label.font:=eaxlabel.Font;
      r15label.left:=eaxlabel.left;
      r15label.top:=r14label.Top+r14label.Height;
      r15label.height:=eaxlabel.height;
      r15label.OnDblClick:=EAXLabelDblClick;
      r15label.OnMouseDown:=RegisterMouseDown;
      r15label.tag:=16;
      r15label.parent:=panel1;
    end;

    eiplabel.top:=r15label.top+r15label.height;
  end else
  begin
    pre:='E';
    f:='%0.8x';
    eiplabel.top:=esplabel.top+ebxlabel.top-eaxlabel.top;
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

  clientheight:=max(clientheight, eiplabel.top+eiplabel.height+8);
  clientwidth:=max(clientwidth, eaxlabel.left+eaxlabel.Canvas.TextWidth(eaxlabel.caption)+ panel2.width+8);

  if _stack<>nil then
  begin
    if stack.stack<>nil then //free old stack copy
      freemem(stack.stack);

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

procedure TRegisters.RegisterMouseDown(Sender: TObject; Button: TMouseButton;
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
    freemem(stack.stack);

  action:=cafree;
end;

function TRegisters.TagToValue(tag: integer): ptrUint;
begin
  case tag of
    0: result:=context.{$ifdef cpu64}Rax{$else}Eax{$endif};
    1: result:=context.{$ifdef cpu64}Rbx{$else}Ebx{$endif};
    2: result:=context.{$ifdef cpu64}rcx{$else}Ecx{$endif};
    3: result:=context.{$ifdef cpu64}rdx{$else}Edx{$endif};
    4: result:=context.{$ifdef cpu64}rsi{$else}Esi{$endif};
    5: result:=context.{$ifdef cpu64}rdi{$else}Edi{$endif};
    6: result:=context.{$ifdef cpu64}rbp{$else}Ebp{$endif};
    7: result:=context.{$ifdef cpu64}rsp{$else}Esp{$endif};
    8: result:=context.{$ifdef cpu64}rip{$else}Eip{$endif};
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

procedure TRegisters.EAXLabelDblClick(Sender: TObject);
begin
  memorybrowser.memoryaddress:=TagToValue(tlabel(sender).tag);
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
