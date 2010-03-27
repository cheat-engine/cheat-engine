unit frmModifyRegistersUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEDebugger,debughelper, KernelDebugger,CEFuncProc,NewKernelHandler,symbolhandler, LResources;

type

  { TfrmModifyRegisters }

  TfrmModifyRegisters = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox22: TCheckBox;
    CheckBox23: TCheckBox;
    CheckBox24: TCheckBox;
    CheckBox25: TCheckBox;
    CheckBox26: TCheckBox;
    CheckBox27: TCheckBox;
    CheckBox28: TCheckBox;
    CheckBox29: TCheckBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Label1: TLabel;
    CheckBox2: TCheckBox;
    Edit2: TEdit;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    CheckBox3: TCheckBox;
    Edit3: TEdit;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label3: TLabel;
    CheckBox4: TCheckBox;
    Edit4: TEdit;
    Label4: TLabel;
    CheckBox5: TCheckBox;
    Edit5: TEdit;
    Label5: TLabel;
    CheckBox6: TCheckBox;
    Edit6: TEdit;
    Label6: TLabel;
    CheckBox7: TCheckBox;
    Edit7: TEdit;
    Label7: TLabel;
    CheckBox8: TCheckBox;
    Edit8: TEdit;
    Label8: TLabel;
    CheckBox9: TCheckBox;
    Edit9: TEdit;
    Label9: TLabel;
    CheckBox10: TCheckBox;
    Label10: TLabel;
    CheckBox11: TCheckBox;
    Label11: TLabel;
    CheckBox12: TCheckBox;
    Label12: TLabel;
    CheckBox13: TCheckBox;
    Label13: TLabel;
    CheckBox14: TCheckBox;
    Label14: TLabel;
    CheckBox15: TCheckBox;
    Label15: TLabel;
    CheckBox16: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox20: TCheckBox;
    CheckBox21: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Label16: TLabel;
    procedure CheckBox22Change(Sender: TObject);
    procedure CheckBox23Change(Sender: TObject);
    procedure CheckBox24Change(Sender: TObject);
    procedure CheckBox25Change(Sender: TObject);
    procedure CheckBox26Change(Sender: TObject);
    procedure CheckBox27Change(Sender: TObject);
    procedure CheckBox28Change(Sender: TObject);
    procedure CheckBox29Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure CheckBox11Click(Sender: TObject);
    procedure CheckBox12Click(Sender: TObject);
    procedure CheckBox13Click(Sender: TObject);
    procedure CheckBox14Click(Sender: TObject);
    procedure CheckBox15Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    address:dword;
  public
    { Public declarations }
    constructor create(AOwner:tcomponent;address:ptrUint);
  end;

var
  frmModifyRegisters: TfrmModifyRegisters;

implementation

uses formsettingsunit, MemoryBrowserFormUnit, debuggertypedefinitions;

constructor TfrmModifyRegisters.create(AOwner:tcomponent;address:ptrUint);
var i: integer;
var x: pbreakpoint;
begin

  inherited create(Aowner);

  self.address:=address;
  caption:='Modify registers(s) at '+IntToHex(address,8);

  if debuggerthread<>nil then
  begin
    x:=debuggerthread.isBreakpoint(address);
    if x<>nil then
    begin


      //find the address in debuggerthread.registermodificationBPs

      if x.changereg.change_eax then
      begin
        checkbox1.checked:=true;
        edit1.Text:=inttohex(x.changereg.new_eax,8);
      end;

      if x.changereg.change_ebx then
      begin
        checkbox2.checked:=true;
        edit2.Text:=inttohex(x.changereg.new_ebx,8);
      end;

      if x.changereg.change_ecx then
      begin
        checkbox3.checked:=true;
        edit3.Text:=inttohex(x.changereg.new_ecx,8);
      end;
      if x.changereg.change_edx then
      begin
        checkbox4.checked:=true;
        edit4.Text:=inttohex(x.changereg.new_edx,8);
      end;
      if x.changereg.change_esi then
      begin
        checkbox5.checked:=true;
        edit5.Text:=inttohex(x.changereg.new_esi,8);
      end;
      if x.changereg.change_edi then
      begin
        checkbox6.checked:=true;
        edit6.Text:=inttohex(x.changereg.new_edi,8);
      end;
      if x.changereg.change_ebp then
      begin
        checkbox7.checked:=true;
        edit7.Text:=inttohex(x.changereg.new_ebp,8);
      end;
      if x.changereg.change_esp then
      begin
        checkbox8.checked:=true;
        edit8.Text:=inttohex(x.changereg.new_esp,8);
      end;
      if x.changereg.change_eip then
      begin
        checkbox9.checked:=true;
        edit9.Text:=inttohex(x.changereg.new_eip,8);
      end;
      {$ifdef cpu64}
      if x.changereg.change_r8 then
      begin
        checkbox22.checked:=true;
        edit10.Text:=inttohex(x.changereg.new_r8,8);
      end;
      if x.changereg.change_r9 then
      begin
        checkbox23.checked:=true;
        edit11.Text:=inttohex(x.changereg.new_r9,8);
      end;
      if x.changereg.change_r10 then
      begin
        checkbox24.checked:=true;
        edit12.Text:=inttohex(x.changereg.new_r10,8);
      end;
      if x.changereg.change_r11 then
      begin
        checkbox25.checked:=true;
        edit13.Text:=inttohex(x.changereg.new_r11,8);
      end;
      if x.changereg.change_r12 then
      begin
        checkbox26.checked:=true;
        edit14.Text:=inttohex(x.changereg.new_r12,8);
      end;
      if x.changereg.change_r13 then
      begin
        checkbox27.checked:=true;
        edit15.Text:=inttohex(x.changereg.new_r13,8);
      end;
      if x.changereg.change_r14 then
      begin
        checkbox28.checked:=true;
        edit16.Text:=inttohex(x.changereg.new_r14,8);
      end;
      if x.changereg.change_r15 then
      begin
        checkbox29.checked:=true;
        edit17.Text:=inttohex(x.changereg.new_r15,8);
      end;
      {$endif}

      if x.changereg.change_cf then
      begin
        checkbox10.checked:=true;
        checkbox16.checked:=x.changereg.new_cf;
      end;
      if x.changereg.change_pf then
      begin
        checkbox11.checked:=true;
        checkbox17.checked:=x.changereg.new_pf;
      end;
      if x.changereg.change_af then
      begin
        checkbox12.checked:=true;
        checkbox18.checked:=x.changereg.new_af;
      end;
      if x.changereg.change_zf then
      begin
        checkbox13.checked:=true;
        checkbox19.checked:=x.changereg.new_zf;
      end;
      if x.changereg.change_sf then
      begin
        checkbox14.checked:=true;
        checkbox20.checked:=x.changereg.new_sf;
      end;
      if x.changereg.change_of then
      begin
        checkbox15.checked:=true;
        checkbox21.checked:=x.changereg.new_of;
      end;

    end;


  end;

  if KDebugger.isActive then
  begin
  {
    //find the address in debuggerthread.registermodificationBPs
    for i:=0 to 3 do
      if debuggerthread2.breakpoints[i]=address then
      begin
        if debuggerthread2.breakpointchanges[i].change_eax then
        begin
          checkbox1.checked:=true;
          edit1.Text:=inttohex(debuggerthread2.breakpointchanges[i].new_eax,8);
        end;

        if debuggerthread2.breakpointchanges[i].change_ebx then
        begin
          checkbox2.checked:=true;
          edit2.Text:=inttohex(debuggerthread2.breakpointchanges[i].new_ebx,8);
        end;

        if debuggerthread2.breakpointchanges[i].change_ecx then
        begin
          checkbox3.checked:=true;
          edit3.Text:=inttohex(debuggerthread2.breakpointchanges[i].new_ecx,8);
        end;
        if debuggerthread2.breakpointchanges[i].change_edx then
        begin
          checkbox4.checked:=true;
          edit4.Text:=inttohex(debuggerthread2.breakpointchanges[i].new_edx,8);
        end;
        if debuggerthread2.breakpointchanges[i].change_esi then
        begin
          checkbox5.checked:=true;
          edit5.Text:=inttohex(debuggerthread2.breakpointchanges[i].new_esi,8);
        end;
        if debuggerthread2.breakpointchanges[i].change_edi then
        begin
          checkbox6.checked:=true;
          edit6.Text:=inttohex(debuggerthread2.breakpointchanges[i].new_edi,8);
        end;
        if debuggerthread2.breakpointchanges[i].change_ebp then
        begin
          checkbox7.checked:=true;
          edit7.Text:=inttohex(debuggerthread2.breakpointchanges[i].new_ebp,8);
        end;
        if debuggerthread2.breakpointchanges[i].change_esp then
        begin
          checkbox8.checked:=true;
          edit8.Text:=inttohex(debuggerthread2.breakpointchanges[i].new_esp,8);
        end;
        if debuggerthread2.breakpointchanges[i].change_eip then
        begin
          checkbox9.checked:=true;
          edit9.Text:=inttohex(debuggerthread2.breakpointchanges[i].new_eip,8);
        end;

        if debuggerthread2.breakpointchanges[i].change_cf then
        begin
          checkbox10.checked:=true;
          checkbox16.checked:=debuggerthread2.breakpointchanges[i].new_cf;
        end;
        if debuggerthread2.breakpointchanges[i].change_pf then
        begin
          checkbox11.checked:=true;
          checkbox17.checked:=debuggerthread2.breakpointchanges[i].new_pf;
        end;
        if debuggerthread2.breakpointchanges[i].change_af then
        begin
          checkbox12.checked:=true;
          checkbox18.checked:=debuggerthread2.breakpointchanges[i].new_af;
        end;
        if debuggerthread2.breakpointchanges[i].change_zf then
        begin
          checkbox13.checked:=true;
          checkbox19.checked:=debuggerthread2.breakpointchanges[i].new_zf;
        end;
        if debuggerthread2.breakpointchanges[i].change_sf then
        begin
          checkbox14.checked:=true;
          checkbox20.checked:=debuggerthread2.breakpointchanges[i].new_sf;
        end;
        if debuggerthread2.breakpointchanges[i].change_of then
        begin
          checkbox15.checked:=true;
          checkbox21.checked:=debuggerthread2.breakpointchanges[i].new_of;
        end;

      end;
      }
  end;
end;


procedure TfrmModifyRegisters.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmModifyRegisters.CheckBox22Change(Sender: TObject);
begin
  edit10.enabled:=checkbox22.checked;
end;

procedure TfrmModifyRegisters.CheckBox23Change(Sender: TObject);
begin
  edit11.enabled:=checkbox23.checked;
end;

procedure TfrmModifyRegisters.CheckBox24Change(Sender: TObject);
begin
  edit12.enabled:=checkbox24.checked;
end;

procedure TfrmModifyRegisters.CheckBox25Change(Sender: TObject);
begin
  edit13.enabled:=checkbox25.checked;
end;

procedure TfrmModifyRegisters.CheckBox26Change(Sender: TObject);
begin
  edit14.enabled:=checkbox26.checked;
end;

procedure TfrmModifyRegisters.CheckBox27Change(Sender: TObject);
begin
  edit15.enabled:=checkbox27.checked;
end;

procedure TfrmModifyRegisters.CheckBox28Change(Sender: TObject);
begin
  edit16.enabled:=checkbox28.checked;
end;

procedure TfrmModifyRegisters.CheckBox29Change(Sender: TObject);
begin
  edit17.enabled:=checkbox29.checked;
end;

procedure TfrmModifyRegisters.CheckBox1Click(Sender: TObject);
begin
  edit1.Enabled:=checkbox1.checked;
end;

procedure TfrmModifyRegisters.CheckBox2Click(Sender: TObject);
begin
  edit2.Enabled:=checkbox2.checked;
end;

procedure TfrmModifyRegisters.CheckBox3Click(Sender: TObject);
begin
  edit3.Enabled:=checkbox3.checked;
end;

procedure TfrmModifyRegisters.CheckBox4Click(Sender: TObject);
begin
  edit4.Enabled:=checkbox4.checked;
end;

procedure TfrmModifyRegisters.CheckBox5Click(Sender: TObject);
begin
  edit5.Enabled:=checkbox5.checked;
end;

procedure TfrmModifyRegisters.CheckBox6Click(Sender: TObject);
begin
  edit6.Enabled:=checkbox6.checked;
end;

procedure TfrmModifyRegisters.CheckBox7Click(Sender: TObject);
begin
  edit7.Enabled:=checkbox7.checked;
end;

procedure TfrmModifyRegisters.CheckBox8Click(Sender: TObject);
begin
  edit8.Enabled:=checkbox8.checked;
end;

procedure TfrmModifyRegisters.CheckBox9Click(Sender: TObject);
begin
  edit9.Enabled:=checkbox9.checked;
end;

procedure TfrmModifyRegisters.CheckBox10Click(Sender: TObject);
begin
  checkbox16.Enabled:=checkbox10.checked;
end;

procedure TfrmModifyRegisters.CheckBox11Click(Sender: TObject);
begin
  checkbox17.Enabled:=checkbox11.checked;
end;

procedure TfrmModifyRegisters.CheckBox12Click(Sender: TObject);
begin
  checkbox18.Enabled:=checkbox12.checked;
end;

procedure TfrmModifyRegisters.CheckBox13Click(Sender: TObject);
begin
  checkbox19.Enabled:=checkbox13.checked;
end;

procedure TfrmModifyRegisters.CheckBox14Click(Sender: TObject);
begin
  checkbox20.Enabled:=checkbox14.checked;
end;

procedure TfrmModifyRegisters.CheckBox15Click(Sender: TObject);
begin
  checkbox21.Enabled:=checkbox15.checked;
end;

procedure TfrmModifyRegisters.Button1Click(Sender: TObject);
var itsin:boolean;
    foundit: boolean;
    i,drnr:integer;
    tempregedit:tregistermodificationBP;
begin

  tempregedit.address:=address;
  tempregedit.change_eax:=checkbox1.checked;
  tempregedit.change_ebx:=checkbox2.checked;
  tempregedit.change_ecx:=checkbox3.checked;
  tempregedit.change_edx:=checkbox4.checked;
  tempregedit.change_esi:=checkbox5.checked;
  tempregedit.change_edi:=checkbox6.checked;
  tempregedit.change_ebp:=checkbox7.checked;
  tempregedit.change_esp:=checkbox8.checked;
  tempregedit.change_eip:=checkbox9.checked;
  tempregedit.change_cf:=checkbox10.checked;
  tempregedit.change_pf:=checkbox11.checked;
  tempregedit.change_af:=checkbox12.checked;
  tempregedit.change_zf:=checkbox13.checked;
  tempregedit.change_sf:=checkbox14.checked;
  tempregedit.change_of:=checkbox15.checked;

  if tempregedit.change_eax then try tempregedit.new_eax:=symhandler.getaddressfromname(edit1.text) except raise exception.Create('Please fill in a valid value for EAX'); end;
  if tempregedit.change_ebx then try tempregedit.new_ebx:=symhandler.getaddressfromname(edit2.text) except raise exception.Create('Please fill in a valid value for EBX'); end;
  if tempregedit.change_ecx then try tempregedit.new_ecx:=symhandler.getaddressfromname(edit3.text) except raise exception.Create('Please fill in a valid value for ECX'); end;
  if tempregedit.change_edx then try tempregedit.new_edx:=symhandler.getaddressfromname(edit4.text) except raise exception.Create('Please fill in a valid value for EDX'); end;
  if tempregedit.change_esi then try tempregedit.new_esi:=symhandler.getaddressfromname(edit5.text) except raise exception.Create('Please fill in a valid value for ESI'); end;
  if tempregedit.change_edi then try tempregedit.new_edi:=symhandler.getaddressfromname(edit6.text) except raise exception.Create('Please fill in a valid value for EDI'); end;
  if tempregedit.change_ebp then try tempregedit.new_ebp:=symhandler.getaddressfromname(edit7.text) except raise exception.Create('Please fill in a valid value for EBP'); end;
  if tempregedit.change_esp then try tempregedit.new_esp:=symhandler.getaddressfromname(edit8.text) except raise exception.Create('Please fill in a valid value for ESP'); end;
  if tempregedit.change_eip then try tempregedit.new_eip:=symhandler.getaddressfromname(edit9.text) except raise exception.Create('Please fill in a valid value for EIP'); end;
  {$ifdef cpu64}
  if tempregedit.change_r8 then try tempregedit.new_r8:=symhandler.getaddressfromname(edit10.text) except raise exception.Create('Please fill in a valid value for R8'); end;
  if tempregedit.change_r9 then try tempregedit.new_r9:=symhandler.getaddressfromname(edit11.text) except raise exception.Create('Please fill in a valid value for R9'); end;
  if tempregedit.change_r10 then try tempregedit.new_r10:=symhandler.getaddressfromname(edit12.text) except raise exception.Create('Please fill in a valid value for R10'); end;
  if tempregedit.change_r11 then try tempregedit.new_r11:=symhandler.getaddressfromname(edit13.text) except raise exception.Create('Please fill in a valid value for R11'); end;
  if tempregedit.change_r12 then try tempregedit.new_r12:=symhandler.getaddressfromname(edit14.text) except raise exception.Create('Please fill in a valid value for R12'); end;
  if tempregedit.change_r13 then try tempregedit.new_r13:=symhandler.getaddressfromname(edit15.text) except raise exception.Create('Please fill in a valid value for R13'); end;
  if tempregedit.change_r14 then try tempregedit.new_r14:=symhandler.getaddressfromname(edit16.text) except raise exception.Create('Please fill in a valid value for R14'); end;
  if tempregedit.change_r15 then try tempregedit.new_r15:=symhandler.getaddressfromname(edit17.text) except raise exception.Create('Please fill in a valid value for R15'); end;
  {$endif}
  if tempregedit.change_cf then tempregedit.new_cf:=checkbox16.checked;
  if tempregedit.change_pf then tempregedit.new_pf:=checkbox17.checked;
  if tempregedit.change_af then tempregedit.new_af:=checkbox18.checked;
  if tempregedit.change_zf then tempregedit.new_zf:=checkbox19.checked;
  if tempregedit.change_sf then tempregedit.new_sf:=checkbox20.checked;
  if tempregedit.change_of then tempregedit.new_of:=checkbox21.checked;

  //set a breakpoint at this spot
  if (formsettings.cbKdebug.checked) and (debuggerthread=nil) then
  begin
    KDebugger.StartDebugger;  //if it wasn't enabled yet
    KDebugger.SetBreakpoint(address, bt_OnInstruction, 1, bo_ChangeRegister, @tempregedit);
  end
  else
  begin
    if debuggerthread<>nil then
      debuggerthread.setChangeRegBreakpoint(@tempregedit);

  end;

  memorybrowser.disassemblerview.Update;
  modalresult:=mrok;
end;

procedure TfrmModifyRegisters.FormShow(Sender: TObject);
begin
  if not processhandler.is64Bit then
  begin
    clientheight:=label9.top+label9.height+8+button1.height;
    label17.visible:=false;
    label18.visible:=false;
    label19.visible:=false;
    label20.visible:=false;
    label21.visible:=false;
    label22.visible:=false;
    label23.visible:=false;
    label24.visible:=false;
  end;
end;

initialization
  {$i frmModifyRegistersUnit.lrs}

end.





