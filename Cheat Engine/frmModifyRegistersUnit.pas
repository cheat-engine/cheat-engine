unit frmModifyRegistersUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CEDebugger, debughelper, KernelDebugger, CEFuncProc,
  NewKernelHandler, symbolhandler, LResources, ExtCtrls;

type

  { TfrmModifyRegisters }

  TfrmModifyRegisters = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbAF: TCheckBox;
    cbCF: TCheckBox;
    cbOF: TCheckBox;
    cbPF: TCheckBox;
    cbSF: TCheckBox;
    cbZF: TCheckBox;
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    address:ptrUint;
  public
    { Public declarations }
    constructor create(AOwner:tcomponent;address:ptrUint); overload;
  end;

var
  frmModifyRegisters: TfrmModifyRegisters;

implementation

uses formsettingsunit, MemoryBrowserFormUnit, debuggertypedefinitions, ProcessHandlerUnit;

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

    if x<>nil then
    begin


      //find the address in debuggerthread.registermodificationBPs

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


procedure TfrmModifyRegisters.Button1Click(Sender: TObject);
var
    tempregedit:tregistermodificationBP;
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
begin

end;

procedure TfrmModifyRegisters.FormShow(Sender: TObject);
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
end;

initialization
  {$i frmModifyRegistersUnit.lrs}

end.





