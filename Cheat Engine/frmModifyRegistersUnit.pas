unit frmModifyRegistersUnit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows, DBK32functions, vmxfunctions,
  {$endif}
  LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CEDebugger, debughelper, KernelDebugger, CEFuncProc,
  NewKernelHandler, symbolhandler, LResources, ExtCtrls, ComCtrls,  math,
  BreakpointTypeDef;

type

  { TfrmModifyRegisters }
  TChangeRegFloatPanel=class(TPanel)
  private
    id: integer;
    lbl: tlabel;
    edt: Tedit;
  public
    constructor Create(AOwner: TComponent; id: integer); overload;
  end;

  TChangeRegXMMPanelEdit=class(TPanel)
  private
    lblstart: tlabel;
    lblstop: tlabel;
    edt: tedit;
  public
    procedure setrange(start, stop: integer);
    constructor Create(AOwner: TComponent; initialid: integer); overload;
  end;

  TChangeRegXMMPanel=class(TGroupbox)
  private
    id: integer;
    tc: TTabControl;
    pnl: TPanel;
    Edits: array [0..3] of TChangeRegXMMPanelEdit;
    procedure tabchange(Sender: TObject);
    function getField(index: integer): dword;
    function usesDouble: boolean;
    procedure setDouble(state: boolean);
  public
    function getEditMask: qword;
    procedure fixdimensions;
    property field[index: integer]: dword read getField;
    property Double: boolean read usesDouble write setDouble;
    constructor Create(AOwner: TComponent; id: integer);  overload;
  end;


  TfrmModifyRegisters = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbAF: TCheckBox;
    cbCF: TCheckBox;
    cbOF: TCheckBox;
    cbPF: TCheckBox;
    cbSF: TCheckBox;
    cbZF: TCheckBox;
    cbUseDBVM: TCheckBox;
    cbChangeExt: TCheckBox;
    edtPA: TEdit;
    edtEAX: TEdit;
    edtEBP: TEdit;
    edtEBX: TEdit;
    edtECX: TEdit;
    edtEDI: TEdit;
    edtEDX: TEdit;
    edtEIP: TEdit;
    edtESI: TEdit;
    edtESP: TEdit;
    edtR10: TEdit;
    edtR11: TEdit;
    edtR12: TEdit;
    edtR13: TEdit;
    edtR14: TEdit;
    edtR15: TEdit;
    edtR8: TEdit;
    edtR9: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure cbChangeExtChange(Sender: TObject);
    procedure cbUseDBVMChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ScrollBox1Click(Sender: TObject);
  private
    { Private declarations }
    address:ptrUint;
    ExtScrollbox: TScrollbox;

    FloatPanel: TPanel;
    XMMPanel: TPanel;

    floats: array [0..7] of TChangeRegFloatPanel;
    xmms: array [0..15] of TChangeRegXMMPanel;

    hasExistingBP: boolean;
    currentbp: TBreakpoint;
  public
    { Public declarations }
    constructor create(AOwner:tcomponent;address:ptrUint); overload;
  end;

var
  frmModifyRegisters: TfrmModifyRegisters;

implementation

uses formsettingsunit, MemoryBrowserFormUnit, debuggertypedefinitions,
  ProcessHandlerUnit, DPIHelper, frmFloatingPointPanelUnit;

resourcestring
  rsModifyRegistersSAt = 'Modify registers(s) at %s';
  rsPleaseFillInAValidValueFor = 'Please fill in a valid value for';

constructor TfrmModifyRegisters.create(AOwner:tcomponent;address:ptrUint);
var x: pbreakpoint;
begin

  inherited create(Aowner);

  self.address:=address;
  caption:=Format(rsModifyRegistersSAt, [IntToHex(address, 8)]);

  if debuggerthread<>nil then
  begin
    x:=debuggerthread.isBreakpoint(address);

    if (x<>nil) and (x.breakpointAction=bo_ChangeRegister) then
    begin
      hasExistingBP:=true; //so onshow can create the fpu screen
      currentbp:=x^;

      if x.changereg.change_eax then
        edtEAX.Text:=inttohex(x.changereg.new_eax,8);

      if x.changereg.change_ebx then
        edtEBX.Text:=inttohex(x.changereg.new_ebx,8);

      if x.changereg.change_ecx then
        edtECX.Text:=inttohex(x.changereg.new_ecx,8);

      if x.changereg.change_edx then
        edtEDX.Text:=inttohex(x.changereg.new_edx,8);

      if x.changereg.change_esi then
        edtESI.Text:=inttohex(x.changereg.new_esi,8);

      if x.changereg.change_edi then
        edtEDI.Text:=inttohex(x.changereg.new_edi,8);

      if x.changereg.change_ebp then
        edtEBP.Text:=inttohex(x.changereg.new_ebp,8);

      if x.changereg.change_esp then
        edtESP.Text:=inttohex(x.changereg.new_esp,8);

      if x.changereg.change_eip then
        edtEIP.Text:=inttohex(x.changereg.new_eip,8);

      {$ifdef cpu64}
      if x.changereg.change_r8 then
        edtR8.Text:=inttohex(x.changereg.new_r8,8);

      if x.changereg.change_r9 then
        edtR9.Text:=inttohex(x.changereg.new_r9,8);

      if x.changereg.change_r10 then
        edtR10.Text:=inttohex(x.changereg.new_r10,8);

      if x.changereg.change_r11 then
        edtR11.Text:=inttohex(x.changereg.new_r11,8);

      if x.changereg.change_r12 then
        edtR12.Text:=inttohex(x.changereg.new_r12,8);

      if x.changereg.change_r13 then
        edtR13.Text:=inttohex(x.changereg.new_r13,8);

      if x.changereg.change_r14 then
        edtR14.Text:=inttohex(x.changereg.new_r14,8);

      if x.changereg.change_r15 then
        edtR15.Text:=inttohex(x.changereg.new_r15,8);

      {$endif}

      if x.changereg.change_cf then
        cbCF.checked:=x.changereg.new_cf;

      if x.changereg.change_pf then
        cbPF.checked:=x.changereg.new_pf;

      if x.changereg.change_af then
        cbAF.checked:=x.changereg.new_af;

      if x.changereg.change_zf then
        cbZF.checked:=x.changereg.new_zf;

      if x.changereg.change_sf then
        cbSF.checked:=x.changereg.new_sf;

      if x.changereg.change_of then
        cbOF.checked:=x.changereg.new_of;

    end;


  end;
end;


procedure TfrmModifyRegisters.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmModifyRegisters.cbUseDBVMChange(Sender: TObject);
var
  pa: int64;
  oldaz: boolean;
begin
  {$ifdef windows}
  if cbUseDBVM.checked then
  begin
    if GetPhysicalAddress(processhandle, pointer(address), PA) then
      edtPA.Text:=inttohex(pa,8);
  end;

  label10.Visible:=cbUseDBVM.checked;
  edtPA.visible:=cbUseDBVM.checked;

  if cbChangeExt.checked=false then
  begin
    oldaz:=autosize;
    autosize:=true;
    DoAutoSize;

    autosize:=oldaz;
  end
  else
  begin
    if cbChangeExt.top+cbChangeExt.height>panel1.top then
      clientheight:=clientheight+((cbChangeExt.top+cbChangeExt.height)-panel1.top);
  end;
  {$endif}
end;

constructor TChangeRegFloatPanel.Create(AOwner: TComponent; id: integer);
begin
  inherited create(Aowner);

  self.id:=id;
  lbl:=tlabel.Create(self);
  edt:=tedit.create(self);
  lbl.parent:=self;
  edt.parent:=self;

  lbl.anchorsideTop.Control:=self;
  lbl.anchorsidetop.Side:=asrTop;
  lbl.AnchorSideLeft.Control:=self;
  lbl.AnchorSideLeft.Side:=asrLeft;
  lbl.Caption:='FP('+inttostr(id)+')';

  edt.anchorsideTop.Control:=lbl;
  edt.anchorsidetop.side:=asrBottom;
  edt.AnchorSideLeft.Control:=self;
  edt.AnchorSideLeft.side:=asrLeft;

  BevelOuter:=bvnone;

  autosize:=true;
end;

procedure TChangeRegXMMPanelEdit.setrange(start,stop: integer);
begin
  lblstart.caption:=inttostr(start);
  lblstop.caption:=inttostr(stop);
end;

constructor TChangeRegXMMPanelEdit.create(AOwner: TComponent; initialid: integer);
begin
  inherited create(AOwner);

  lblstart:=tlabel.create(self);
  lblstop:=tlabel.create(self);
  edt:=tedit.create(self);;

  lblstart.anchorsidetop.control:=self;
  lblstart.anchorsidetop.side:=asrTop;
  lblstart.anchorsideleft.control:=self;
  lblstart.anchorsideleft.side:=asrLeft;

  edt.anchorsidetop.control:=lblstart;
  edt.anchorsidetop.side:=asrBottom;
  edt.anchorsideleft.control:=self;
  edt.anchorsideleft.side:=asrLeft;

  lblstop.anchorsidetop.control:=self;
  lblstop.anchorsidetop.side:=asrTop;
  lblstop.anchorsideright.control:=edt;
  lblstop.anchorsideright.side:=asrRight;

  lblstop.Anchors:=[akTop, akRight];

  lblstart.parent:=self;
  lblstop.parent:=self;
  edt.parent:=self;

  //color:=clblue;

  BevelOuter:=bvnone;

  autosize:=true;

  setrange(initialid*32,(initialid+1)*32-1);
end;

function TChangeRegXMMPanel.getField(index: integer): dword;
var
  d: dword;
  f: single absolute d;

  q: qword;
  dbl: double absolute q;
begin
  if tc.TabIndex=0 then
  begin
    f:=StrToFloat(trim(edits[index].edt.Text));
    result:=d;
  end
  else
  begin
    if index<=1 then
    begin
      dbl:=StrToFloat(trim(edits[0].edt.Text));
      if index=0 then
        result:=q
      else
        result:=q shr 32;
    end
    else
    begin
      dbl:=StrToFloat(trim(edits[1].edt.Text));
      if index=2 then
        result:=q
      else
        result:=q shr 32;
    end;


  end;
end;

function TChangeRegXMMPanel.usesDouble;
begin
  result:=tc.tabindex=1;
end;

procedure TChangeRegXMMPanel.setDouble(state: boolean);
begin
  if state then
    tc.tabindex:=1
  else
    tc.tabindex:=0;

  tc.onchange(tc);
end;

function TChangeRegXMMPanel.getEditMask;
var
  mask: qword;
begin
  mask:=0;
  if tc.tabindex=0 then
  begin
    if trim(edits[0].edt.Text)<>'' then mask:=mask or 1;
    if trim(edits[1].edt.Text)<>'' then mask:=mask or 2;
    if trim(edits[2].edt.Text)<>'' then mask:=mask or 4;
    if trim(edits[3].edt.Text)<>'' then mask:=mask or 8;
  end
  else
  begin
    if trim(edits[0].edt.Text)<>'' then mask:=mask or 1 or 2;
    if trim(edits[1].edt.Text)<>'' then mask:=mask or 4 or 8;
  end;

  mask:=mask shl (4*id);

  result:=mask;
end;

procedure TChangeRegXMMPanel.fixdimensions;
var i: integer;
begin
  DoAutoSize;
  for i:=0 to 3 do
    edits[i].DoAutoSize;

  i:=edits[3].left+edits[3].Width;
  tc.ClientWidth:=i+4;
  pnl.Width:=i;

  width:=i;
  i:=edits[0].height;
  tc.clientheight:=pnl.top+i+3;
  height:=i;
end;

procedure TChangeRegXMMPanel.tabchange(sender: tobject);
begin
  edits[2].Visible:=tc.tabindex=0;
  edits[3].Visible:=tc.tabindex=0;
  if tc.tabindex=0 then
  begin
    edits[0].setrange(0,31);
    edits[1].setrange(32,63);
  end
  else
  begin
    edits[0].setrange(0,63);
    edits[1].setrange(63,127);
  end;
end;

constructor TChangeRegXMMPanel.Create(AOwner: TComponent; id: integer);
var
  i: integer;
begin
  inherited create(AOwner);
  self.id:=id;
  caption:='XMM'+inttostr(id);

  tc:=ttabcontrol.Create(self);
  tc.parent:=self;
  tc.Tabs.add('Float');
  tc.Tabs.add('Double');
  tc.TabIndex:=0;

  tc.OnChange:=tabchange;

  pnl:=tpanel.Create(tc);
  pnl.parent:=tc;
  //pnl.color:=clAqua;
  pnl.align:=alClient;

  for i:=0 to 3 do
  begin
    edits[i]:=TChangeRegXMMPanelEdit.Create(pnl,i);
    edits[i].parent:=pnl;
  end;


  pnl.ChildSizing.ControlsPerLine:=4;
  pnl.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
  pnl.ChildSizing.HorizontalSpacing:=3;
end;

procedure TfrmModifyRegisters.cbChangeExtChange(Sender: TObject);
var i: integer;
  is64: boolean;
begin
  if ExtScrollbox=nil then
  begin
    ExtScrollbox:=TScrollbox.create(self);
    ExtScrollbox.Parent:=self;
    ExtScrollbox.Width:=400;
    ExtScrollbox.Height:=200;

    FloatPanel:=TPanel.Create(ExtScrollbox);
    FloatPanel.parent:=ExtScrollBox;

    FloatPanel.AnchorSideTop.Control:=extScrollbox;
    FloatPanel.AnchorSideTop.Side:=asrTop;
    FloatPanel.AnchorSideLeft.Control:=ExtScrollbox;
    FloatPanel.AnchorSideLeft.Side:=asrLeft;
    FloatPanel.BevelOuter:=bvNone;
    //FloatPanel.color:=clred;


    XMMPanel:=TPanel.create(ExtScrollbox);
    XMMPanel.parent:=ExtScrollBox;
    //XMMPanel.color:=clgreen;
    XMMPanel.BevelOuter:=bvNone;

    for i:=0 to 7 do
    begin
      floats[i]:=TChangeRegFloatPanel.Create(FloatPanel, i);
      floats[i].parent:=floatpanel;
    end;

    floatpanel.ChildSizing.ControlsPerLine:=4;
    floatpanel.ChildSizing.HorizontalSpacing:=3;
    floatpanel.ChildSizing.VerticalSpacing:=3;
    floatpanel.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;

    FloatPanel.autosize:=true;


    XMMPanel.anchorsidetop.control:=FloatPanel;
    XMMPanel.anchorsidetop.side:=asrBottom;

    for i:=0 to 15 do
    begin
      xmms[i]:=TChangeRegXMMPanel.Create(XMMPanel, i);
      xmms[i].parent:=XMMPanel;
      xmms[i].fixdimensions;
    end;

    XMMPanel.ChildSizing.ControlsPerLine:=1;
    XMMPanel.ChildSizing.VerticalSpacing:=1;
    XMMPanel.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
    XMMPanel.AutoSize:=true;

    ExtScrollbox.AnchorSideTop.Control:=self;
    ExtScrollbox.AnchorSideTop.Side:=asrTop;
    ExtScrollbox.AnchorSideRight.Control:=self;
    ExtScrollbox.AnchorSideRight.Side:=asrRight;
    ExtScrollbox.AnchorSideBottom.Control:=panel1;
    ExtScrollbox.AnchorSideBottom.Side:=asrTop;
    ExtScrollbox.Anchors:=[akTop, akRight, akBottom];

    ExtScrollbox.ClientWidth:=max(FloatPanel.Width, xmmpanel.width);

    ExtScrollbox.VertScrollBar.Tracking:=true;
    //ExtScrollbox.AutoSize:=true;

    width:=width+ExtScrollbox.Width;

  end;

  if cbChangeExt.Checked then
  begin
    panel1.AnchorSideTop.control:=nil;
    panel1.AnchorSideBottom.control:=self;
    panel1.AnchorSideBottom.Side:=asrBottom;

    panel1.Anchors:=[akLeft, akBottom];
    ExtScrollbox.visible:=true;

    is64:=processhandler.is64Bit;
    for i:=8 to 15 do
      xmms[i].Visible:=is64;
  end
  else
  begin
    ExtScrollbox.visible:=false;
    panel1.AnchorSideTop.control:=cbChangeExt;
    panel1.AnchorSideBottom.control:=nil;
    panel1.Anchors:=[akLeft, akTop];


  end;

  OnResize(self);
  //DoAutoSize;
end;


procedure TfrmModifyRegisters.Button1Click(Sender: TObject);
var
  tempregedit:tregistermodificationBP;

  {$ifdef windows}
  changereginfo: tchangeregonbpinfo;
  {$endif}
  PA: qword;
  bpid: integer;
  d: double;
  e: extended;
  mask: qword;

  xfields: TXMMFIELDS;
  i,j: integer;

  bp: PBreakpoint;
  bo: integer;
  ob: byte;
begin
  tempregedit.address:=address;
  tempregedit.change_eax:=edtEAX.text<>'';
  tempregedit.change_ebx:=edtEBX.text<>'';
  tempregedit.change_ecx:=edtECX.text<>'';
  tempregedit.change_edx:=edtEDX.text<>'';
  tempregedit.change_esi:=edtESI.text<>'';
  tempregedit.change_edi:=edtEDI.text<>'';
  tempregedit.change_ebp:=edtEBP.text<>'';
  tempregedit.change_esp:=edtESP.text<>'';
  tempregedit.change_eip:=edtEIP.text<>'';
  {$ifdef cpu64}
  tempregedit.change_r8:=edtR8.text<>'';
  tempregedit.change_r9:=edtR9.text<>'';
  tempregedit.change_r10:=edtR10.text<>'';
  tempregedit.change_r11:=edtR11.text<>'';
  tempregedit.change_r12:=edtR12.text<>'';
  tempregedit.change_r13:=edtR13.text<>'';
  tempregedit.change_r14:=edtR14.text<>'';
  tempregedit.change_r15:=edtR15.text<>'';
  {$endif}

  tempregedit.change_cf:=cbCF.State<>cbGrayed;
  tempregedit.change_pf:=cbPF.State<>cbGrayed;
  tempregedit.change_af:=cbAF.State<>cbGrayed;
  tempregedit.change_zf:=cbZF.State<>cbGrayed;
  tempregedit.change_sf:=cbSF.State<>cbGrayed;
  tempregedit.change_of:=cbOF.State<>cbGrayed;

  if tempregedit.change_eax then try tempregedit.new_eax:=symhandler.getaddressfromname(edtEAX.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' EAX'); end;
  if tempregedit.change_ebx then try tempregedit.new_ebx:=symhandler.getaddressfromname(edtEBX.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' EBX'); end;
  if tempregedit.change_ecx then try tempregedit.new_ecx:=symhandler.getaddressfromname(edtECX.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' ECX'); end;
  if tempregedit.change_edx then try tempregedit.new_edx:=symhandler.getaddressfromname(edtEDX.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' EDX'); end;
  if tempregedit.change_esi then try tempregedit.new_esi:=symhandler.getaddressfromname(edtESI.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' ESI'); end;
  if tempregedit.change_edi then try tempregedit.new_edi:=symhandler.getaddressfromname(edtEDI.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' EDI'); end;
  if tempregedit.change_ebp then try tempregedit.new_ebp:=symhandler.getaddressfromname(edtEBP.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' EBP'); end;
  if tempregedit.change_esp then try tempregedit.new_esp:=symhandler.getaddressfromname(edtESP.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' ESP'); end;
  if tempregedit.change_eip then try tempregedit.new_eip:=symhandler.getaddressfromname(edtEIP.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' EIP'); end;
  {$ifdef cpu64}
  if tempregedit.change_r8 then try tempregedit.new_r8:=symhandler.getaddressfromname(edtR8.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' R8'); end;
  if tempregedit.change_r9 then try tempregedit.new_r9:=symhandler.getaddressfromname(edtR9.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' R9'); end;
  if tempregedit.change_r10 then try tempregedit.new_r10:=symhandler.getaddressfromname(edtR10.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' R10'); end;
  if tempregedit.change_r11 then try tempregedit.new_r11:=symhandler.getaddressfromname(edtR11.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' R11'); end;
  if tempregedit.change_r12 then try tempregedit.new_r12:=symhandler.getaddressfromname(edtR12.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' R12'); end;
  if tempregedit.change_r13 then try tempregedit.new_r13:=symhandler.getaddressfromname(edtR13.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' R13'); end;
  if tempregedit.change_r14 then try tempregedit.new_r14:=symhandler.getaddressfromname(edtR14.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' R14'); end;
  if tempregedit.change_r15 then try tempregedit.new_r15:=symhandler.getaddressfromname(edtR15.text) except raise exception.Create(rsPleaseFillInAValidValueFor+' R15'); end;
  {$endif}
  if tempregedit.change_cf then tempregedit.new_cf:=cbCF.checked;
  if tempregedit.change_pf then tempregedit.new_pf:=cbPF.checked;
  if tempregedit.change_af then tempregedit.new_af:=cbAF.checked;
  if tempregedit.change_zf then tempregedit.new_zf:=cbZF.checked;
  if tempregedit.change_sf then tempregedit.new_sf:=cbSF.checked;
  if tempregedit.change_of then tempregedit.new_of:=cbOF.checked;

  tempregedit.change_FP:=0;
  if cbChangeExt.checked then
    for i:=0 to 7 do
    begin
      if trim(floats[i].edt.Text)<>'' then
      begin
        tempregedit.change_FP:=tempregedit.change_FP or (1 shl i);
        {$ifdef cpu64}
        d:=StrToFloat(trim(floats[i].edt.Text));
        doubletoextended(@d,pointer(ptruint(@tempregedit.new_FP0)+16*i));
        {$else}
        e:=StrToFloat(trim(floats[i].edt.Text));
        copymemory(pointer(ptruint(@tempregedit.new_FP0)+16*i),@e,10);
        {$endif}
      end;
    end;

  tempregedit.change_XMM:=0;
  tempregedit.usesDouble:=0;

  if cbChangeExt.checked then
    for i:=0 to {$ifdef cpu64}15{$else}7{$endif} do
    begin
      if xmms[i].Double then
        tempregedit.usesDouble:=tempregedit.usesDouble or (1 shl i);

      mask:=xmms[i].getEditMask;
      tempregedit.change_XMM:=tempregedit.change_XMM or mask;

      if mask<>0 then
      begin
        for j:=0 to 3 do
        begin
          if ((mask shr (i*4)) and (1 shl j))>0 then
            xfields[j]:=xmms[i].field[j]
          else
            xfields[j]:=0;
        end;


        copymemory(pointer(ptruint(@tempregedit.new_XMM0)+sizeof(TXMMFIELDS)*i), @xfields[0],sizeof(TXMMFIELDS));
      end;


    end;
  {$ifdef windows}


  if debuggerthread<>nil then
  begin
    //remove the old one
    debuggerthread.lockbplist;

    bp:=debuggerthread.isBreakpoint(address);
    if bp<>nil then
      debuggerthread.RemoveBreakpoint(bp);

    debuggerthread.unlockbplist;
  end;

  if cbUseDBVM.checked then
  begin
    if loaddbvmifneeded('Launch DBVM?') then
    begin
      if dbvm_isBreakpoint(address,pa, bo,ob) then
        dbvm_cloak_removechangeregonbp(pa);

      pa:=strtoint64('$'+edtPA.text);


      //convert to a changereginfo
      changereginfo.Flags.changeRAX:=ifthen(tempregedit.change_eax,1,0);
      changereginfo.Flags.changeRBX:=ifthen(tempregedit.change_ebx,1,0);
      changereginfo.Flags.changeRCX:=ifthen(tempregedit.change_ecx,1,0);
      changereginfo.Flags.changeRDX:=ifthen(tempregedit.change_edx,1,0);
      changereginfo.Flags.changeRSI:=ifthen(tempregedit.change_esi,1,0);
      changereginfo.Flags.changeRDI:=ifthen(tempregedit.change_edi,1,0);
      changereginfo.Flags.changeRBP:=ifthen(tempregedit.change_ebp,1,0);
      changereginfo.Flags.changeRSP:=ifthen(tempregedit.change_esp,1,0);
      changereginfo.Flags.changeRIP:=ifthen(tempregedit.change_eip,1,0);
{$ifdef cpu64}
      changereginfo.Flags.changeR8:=ifthen(tempregedit.change_r8,1,0);
      changereginfo.Flags.changeR9:=ifthen(tempregedit.change_r9,1,0);
      changereginfo.Flags.changeR10:=ifthen(tempregedit.change_r10,1,0);
      changereginfo.Flags.changeR11:=ifthen(tempregedit.change_r11,1,0);
      changereginfo.Flags.changeR12:=ifthen(tempregedit.change_r12,1,0);
      changereginfo.Flags.changeR13:=ifthen(tempregedit.change_r13,1,0);
      changereginfo.Flags.changeR14:=ifthen(tempregedit.change_r14,1,0);
      changereginfo.Flags.changeR15:=ifthen(tempregedit.change_r15,1,0);
{$else}
      changereginfo.Flags.changeR8:=0;
      changereginfo.Flags.changeR9:=0;
      changereginfo.Flags.changeR10:=0;
      changereginfo.Flags.changeR11:=0;
      changereginfo.Flags.changeR12:=0;
      changereginfo.Flags.changeR13:=0;
      changereginfo.Flags.changeR14:=0;
      changereginfo.Flags.changeR15:=0;
{$endif}
      changereginfo.Flags.changeCF:=ifthen(tempregedit.change_cf,1,0);
      changereginfo.Flags.changePF:=ifthen(tempregedit.change_pf,1,0);
      changereginfo.Flags.changeAF:=ifthen(tempregedit.change_af,1,0);
      changereginfo.Flags.changeZF:=ifthen(tempregedit.change_zf,1,0);
      changereginfo.Flags.changeSF:=ifthen(tempregedit.change_sf,1,0);
      changereginfo.Flags.changeOF:=ifthen(tempregedit.change_of,1,0);

      changereginfo.Flags.newCF:=ifthen(tempregedit.new_cf,1,0);
      changereginfo.Flags.newPF:=ifthen(tempregedit.new_pf,1,0);
      changereginfo.Flags.newAF:=ifthen(tempregedit.new_af,1,0);
      changereginfo.Flags.newZF:=ifthen(tempregedit.new_zf,1,0);
      changereginfo.Flags.newSF:=ifthen(tempregedit.new_sf,1,0);
      changereginfo.Flags.newOF:=ifthen(tempregedit.new_of,1,0);

      changereginfo.newRAX:=tempregedit.new_eax;
      changereginfo.newRBX:=tempregedit.new_ebx;
      changereginfo.newRCX:=tempregedit.new_ecx;
      changereginfo.newRDX:=tempregedit.new_edx;
      changereginfo.newRSI:=tempregedit.new_esi;
      changereginfo.newRDI:=tempregedit.new_edi;
      changereginfo.newRBP:=tempregedit.new_ebp;
      changereginfo.newRSP:=tempregedit.new_esp;
      changereginfo.newRIP:=tempregedit.new_eip;
{$ifdef cpu64}
      changereginfo.newR8:=tempregedit.new_r8;
      changereginfo.newR9:=tempregedit.new_r9;
      changereginfo.newR10:=tempregedit.new_r10;
      changereginfo.newR11:=tempregedit.new_r11;
      changereginfo.newR12:=tempregedit.new_r12;
      changereginfo.newR13:=tempregedit.new_r13;
      changereginfo.newR14:=tempregedit.new_r14;
      changereginfo.newR15:=tempregedit.new_r15;
{$endif}

      changereginfo.changeXMM:=tempregedit.change_XMM; //16 nibbles, each bit is one dword
      changereginfo.changeFP:=tempregedit.change_FP;


      copymemory(@changereginfo.newFP0, @tempregedit.new_FP0, 16);
      copymemory(@changereginfo.newFP1, @tempregedit.new_FP1, 16);
      copymemory(@changereginfo.newFP2, @tempregedit.new_FP2, 16);
      copymemory(@changereginfo.newFP3, @tempregedit.new_FP3, 16);
      copymemory(@changereginfo.newFP4, @tempregedit.new_FP4, 16);
      copymemory(@changereginfo.newFP5, @tempregedit.new_FP5, 16);
      copymemory(@changereginfo.newFP6, @tempregedit.new_FP6, 16);
      copymemory(@changereginfo.newFP7, @tempregedit.new_FP7, 16);

      copymemory(@changereginfo.XMM0, @tempregedit.new_XMM0[0], 16);
      copymemory(@changereginfo.XMM1, @tempregedit.new_XMM1[0], 16);
      copymemory(@changereginfo.XMM2, @tempregedit.new_XMM2[0], 16);
      copymemory(@changereginfo.XMM3, @tempregedit.new_XMM3[0], 16);
      copymemory(@changereginfo.XMM4, @tempregedit.new_XMM4[0], 16);
      copymemory(@changereginfo.XMM5, @tempregedit.new_XMM5[0], 16);
      copymemory(@changereginfo.XMM6, @tempregedit.new_XMM6[0], 16);
      copymemory(@changereginfo.XMM7, @tempregedit.new_XMM7[0], 16);

      {$ifdef cpu64}
      copymemory(@changereginfo.XMM8, @tempregedit.new_XMM8[0], 16);
      copymemory(@changereginfo.XMM9, @tempregedit.new_XMM9[0], 16);
      copymemory(@changereginfo.XMM10, @tempregedit.new_XMM10[0], 16);
      copymemory(@changereginfo.XMM11, @tempregedit.new_XMM11[0], 16);
      copymemory(@changereginfo.XMM12, @tempregedit.new_XMM12[0], 16);
      copymemory(@changereginfo.XMM13, @tempregedit.new_XMM13[0], 16);
      copymemory(@changereginfo.XMM14, @tempregedit.new_XMM14[0], 16);
      copymemory(@changereginfo.XMM15, @tempregedit.new_XMM15[0], 16);
      {$endif}

      log('Calling dbvm_cloak_changeregonbp');
      if dbvm_cloak_changeregonbp(PA, changereginfo, address)<>0 then
        MessageDlg('Failure setting a DBVM ChangeRegOnBP breakpoint', mtError,[mbok],0);

      memorybrowser.disassemblerview.Update;
      modalresult:=mrok;
      exit;
    end;
  end;
  {$endif}

  //set a breakpoint at this spot
  if startdebuggerifneeded then
  begin
    if debuggerthread<>nil then
      debuggerthread.setChangeRegBreakpoint(@tempregedit);


  end;

  memorybrowser.disassemblerview.Update;
  modalresult:=mrok;
end;

procedure TfrmModifyRegisters.FormCreate(Sender: TObject);
var pref: string;
begin
  {$ifdef windows}
  cbUseDBVM.visible:=isDBVMCapable and hasEPTSupport;
  if isRunningDBVM and (debuggerthread=nil) then
    cbUseDBVM.checked:=true;
  {$else}
  cbUseDBVM.visible:=false;

  {$endif}


  label10.visible:=cbUseDBVM.checked;
  edtPA.visible:=cbUseDBVM.checked;

  if processhandler.is64bit then
  begin
    pref:='R'
  end
  else
  begin
    pref:='E'
  end;

  label1.Caption:=pref+'AX';
  label2.Caption:=pref+'BX';
  label3.Caption:=pref+'CX';
  label4.Caption:=pref+'DX';
  label5.Caption:=pref+'SI';
  label6.Caption:=pref+'DI';
  label7.Caption:=pref+'BP';
  label8.Caption:=pref+'SP';
  label9.Caption:=pref+'IP';
end;

procedure TfrmModifyRegisters.FormResize(Sender: TObject);
var d: integer;
begin
  BeginUpdateBounds;
  LockRealizeBounds;

  if cbChangeExt.checked then
    d:=clientwidth-ExtScrollbox.Width-(panel3.Left+panel3.width)
  else
    d:=clientwidth-(panel3.Left+panel3.width);

  edtEAX.Width:=edtEAX.Width+d;


  UnlockRealizeBounds;
  EndUpdateBounds;
end;

procedure TfrmModifyRegisters.FormShow(Sender: TObject);
var
  i,j: integer;
  pex: PExtended;
  d: double;
  n: byte;

  xmmp: PXMMFIELDS;

  pd: pdouble;
begin
  if not processhandler.is64Bit then
  begin
    label17.visible:=false;
    label18.visible:=false;
    label19.visible:=false;
    label20.visible:=false;
    label21.visible:=false;
    label22.visible:=false;
    label23.visible:=false;
    label24.visible:=false;

    edtR8.visible:=false;
    edtR9.visible:=false;
    edtR10.visible:=false;
    edtR11.visible:=false;
    edtR12.visible:=false;
    edtR13.visible:=false;
    edtR14.visible:=false;
    edtR15.visible:=false;



    DoAutoSize;
  end;

  autosize:=false;

  cbChangeExt.left:=edtR15.left;

 
  Constraints.MinHeight:=cbChangeExt.top+cbChangeExt.height+panel1.height+6;


  if hasExistingBP then
  begin
    if (currentbp.changereg.change_FP or currentbp.changereg.change_XMM)>0 then
      cbChangeExt.Checked:=true;

    if cbChangeExt.checked then
    begin
      for i:=0 to 7 do
      begin
        if (currentbp.changereg.change_FP and (1 shl i))>0 then
        begin
          pex:=pextended(ptruint(@currentbp.changereg.new_FP0)+16*i);
          {$ifdef cpu32}
          floats[i].edt.Text:=floattostr(pex^);
          {$else}
          extendedtodouble(pex,d);
          floats[i].edt.Text:=floattostr(d);
          {$endif}
        end;
      end;

      for i:=0 to {$ifdef cpu32}7{$else}15{$endif} do
      begin
        n:=(currentbp.changereg.change_XMM shr (i*4)) and $f;
        if n>0 then
        begin
          xmmp:=PXMMFIELDS(ptruint(@currentbp.changereg.new_XMM0)+i*16);
          xmms[i].Double:=currentbp.changereg.usesDouble and (1 shl i)>0;
          if xmms[i].Double then
          begin
            //double
            for j:=0 to 1 do
            begin
              if (n and (1 shl (j*2)))>0 then
              begin
                pd:=@xmmp[j*2];
                xmms[i].Edits[j].edt.text:=floattostr(pd^);
              end;
            end;
          end
          else
          begin
            //float
            for j:=0 to 3 do
              if (n and (1 shl j))>0 then
                xmms[i].Edits[j].edt.text:=floattostr(psingle(@xmmp[j])^);

          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmModifyRegisters.ScrollBox1Click(Sender: TObject);
begin

end;

initialization
  {$i frmModifyRegistersUnit.lrs}

end.





