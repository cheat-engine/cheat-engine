unit frmModifyRegistersUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,debugger,debugger2,cefuncproc,newkernelhandler,symbolhandler;

type
  TfrmModifyRegisters = class(TForm)
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Label1: TLabel;
    CheckBox2: TCheckBox;
    Edit2: TEdit;
    Label2: TLabel;
    CheckBox3: TCheckBox;
    Edit3: TEdit;
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
  private
    { Private declarations }
    address:dword;
  public
    { Public declarations }
    constructor create(AOwner:tcomponent;address:dword);
  end;

var
  frmModifyRegisters: TfrmModifyRegisters;

implementation

uses formsettingsunit, MemoryBrowserFormUnit;

constructor TfrmModifyRegisters.create(AOwner:tcomponent;address:dword);
var i: integer;
begin
  inherited create(Aowner);

  self.address:=address;
  caption:='Modify registers(s) at '+IntToHex(address,8);

  if debuggerthread<>nil then
  begin
    //find the address in debuggerthread.registermodificationBPs
    for i:=0 to length(debuggerthread.registermodificationBPs)-1 do
      if debuggerthread.registermodificationBPs[i].address=address then
      begin
        if debuggerthread.registermodificationBPs[i].change_eax then
        begin
          checkbox1.checked:=true;
          edit1.Text:=inttohex(debuggerthread.registermodificationBPs[i].new_eax,8);
        end;

        if debuggerthread.registermodificationBPs[i].change_ebx then
        begin
          checkbox2.checked:=true;
          edit2.Text:=inttohex(debuggerthread.registermodificationBPs[i].new_ebx,8);
        end;

        if debuggerthread.registermodificationBPs[i].change_ecx then
        begin
          checkbox3.checked:=true;
          edit3.Text:=inttohex(debuggerthread.registermodificationBPs[i].new_ecx,8);
        end;
        if debuggerthread.registermodificationBPs[i].change_edx then
        begin
          checkbox4.checked:=true;
          edit4.Text:=inttohex(debuggerthread.registermodificationBPs[i].new_edx,8);
        end;
        if debuggerthread.registermodificationBPs[i].change_esi then
        begin
          checkbox5.checked:=true;
          edit5.Text:=inttohex(debuggerthread.registermodificationBPs[i].new_esi,8);
        end;
        if debuggerthread.registermodificationBPs[i].change_edi then
        begin
          checkbox6.checked:=true;
          edit6.Text:=inttohex(debuggerthread.registermodificationBPs[i].new_edi,8);
        end;
        if debuggerthread.registermodificationBPs[i].change_ebp then
        begin
          checkbox7.checked:=true;
          edit7.Text:=inttohex(debuggerthread.registermodificationBPs[i].new_ebp,8);
        end;
        if debuggerthread.registermodificationBPs[i].change_esp then
        begin
          checkbox8.checked:=true;
          edit8.Text:=inttohex(debuggerthread.registermodificationBPs[i].new_esp,8);
        end;
        if debuggerthread.registermodificationBPs[i].change_eip then
        begin
          checkbox9.checked:=true;
          edit9.Text:=inttohex(debuggerthread.registermodificationBPs[i].new_eip,8);
        end;

        if debuggerthread.registermodificationBPs[i].change_cf then
        begin
          checkbox10.checked:=true;
          checkbox16.checked:=debuggerthread.registermodificationBPs[i].new_cf;
        end;
        if debuggerthread.registermodificationBPs[i].change_pf then
        begin
          checkbox11.checked:=true;
          checkbox17.checked:=debuggerthread.registermodificationBPs[i].new_pf;
        end;
        if debuggerthread.registermodificationBPs[i].change_af then
        begin
          checkbox12.checked:=true;
          checkbox18.checked:=debuggerthread.registermodificationBPs[i].new_af;
        end;
        if debuggerthread.registermodificationBPs[i].change_zf then
        begin
          checkbox13.checked:=true;
          checkbox19.checked:=debuggerthread.registermodificationBPs[i].new_zf;
        end;
        if debuggerthread.registermodificationBPs[i].change_sf then
        begin
          checkbox14.checked:=true;
          checkbox20.checked:=debuggerthread.registermodificationBPs[i].new_sf;
        end;
        if debuggerthread.registermodificationBPs[i].change_of then
        begin
          checkbox15.checked:=true;
          checkbox21.checked:=debuggerthread.registermodificationBPs[i].new_of;
        end;

      end;
  end;

  if debuggerthread2<>nil then
  begin
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
  end;
end;

{$R *.dfm}

procedure TfrmModifyRegisters.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
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
  if tempregedit.change_cf then tempregedit.new_cf:=checkbox16.checked;
  if tempregedit.change_pf then tempregedit.new_pf:=checkbox17.checked;
  if tempregedit.change_af then tempregedit.new_af:=checkbox18.checked;
  if tempregedit.change_zf then tempregedit.new_zf:=checkbox19.checked;
  if tempregedit.change_sf then tempregedit.new_sf:=checkbox20.checked;
  if tempregedit.change_of then tempregedit.new_of:=checkbox21.checked;

  //set a breakpoint at this spot

  if (formsettings.cbKdebug.checked) and (debuggerthread=nil) then
  begin
    if DebuggerThread2=nil then
      DebuggerThread2:=TDebugEvents.Create(false);

    crdebugging.Enter;

    try
      //first check if it's already in the list, if they are in, delete it and readd the new entry
      for i:=0 to 3 do
        if debuggerthread2.breakpoints[i]=address then
          debuggerthread2.breakpoints[i]:=0;
      debuggerthread2.setbreakpoints;

      drnr:=-1;
      for i:=0 to 3 do
        if debuggerthread2.breakpoints[i]=0 then   //free to use
        begin
          debuggerthread2.breakpoints[i]:=address;

          outputdebugstring('Legal call to ChangeRegOnBP');


          ChangeRegOnBP(processid,address,i,tempregedit.change_eax,tempregedit.change_ebx,tempregedit.change_ecx,tempregedit.change_edx,tempregedit.change_esi,tempregedit.change_edi,tempregedit.change_ebp,tempregedit.change_esp,tempregedit.change_eip,tempregedit.change_cf,tempregedit.change_pf,tempregedit.change_af,tempregedit.change_zf,tempregedit.change_sf,tempregedit.change_of,
                                            tempregedit.new_eax,tempregedit.new_ebx,tempregedit.new_ecx,tempregedit.new_edx,tempregedit.new_esi,tempregedit.new_edi,tempregedit.new_ebp,tempregedit.new_esp,tempregedit.new_eip,tempregedit.new_cf,tempregedit.new_pf,tempregedit.new_af,tempregedit.new_zf,tempregedit.new_sf,tempregedit.new_of);
          debuggerthread2.breakpointchanges[i]:=tempregedit;
          debuggerthread2.setbreakpoints;
          drnr:=i;
          break;
        end;

      if drnr=-1 then raise exception.Create('Max of 4 Debugregs has been reached');

      memorybrowser.updatebplist;
    finally
      crdebugging.Leave;
    end;

  end
  else
  begin

    crdebugging.Enter;
    try
      itsin:=false;
      //check if it's already a breakpoint. in that case just add it to mod register array
      if formsettings.rbDebugAsBreakpoint.checked then
      begin
        //check the debugregs
        for i:=0 to length(debuggerthread.userbreakpoints)-1 do
          if debuggerthread.userbreakpoints[i]=address then
          begin
            itsin:=true;
            break;
          end;
      end
      else
      begin
        //check the default bp's
        for i:=0 to length(debuggerthread.int3userbreakpoints)-1 do
          if debuggerthread.int3userbreakpoints[i].address=address then
          begin
            itsin:=true;
            break;
          end;

      end;

    finally
      crdebugging.Leave;
    end;

    //add this entry to the registermodificationBPs array in debugger
    if not itsin then togglebreakpoint(address);
    //add the address to the array of bp's

    //find it, if found, change it. else add it
    foundit:=false;
    for i:=0 to length(debuggerthread.registermodificationBPs)-1 do
      if debuggerthread.registermodificationBPs[i].address=address then
      begin
        //found it, so edit it
        debuggerthread.registermodificationBPs[i]:=tempregedit;
        foundit:=true;
      end;

    if not foundit then
    begin
      //add a new one
      setlength(debuggerthread.registermodificationBPs,length(debuggerthread.registermodificationBPs)+1);
      debuggerthread.registermodificationBPs[length(debuggerthread.registermodificationBPs)-1]:=tempregedit;
    end;
  end;

  memorybrowser.updatedisassemblerview;
  modalresult:=mrok;
end;

end.




