unit frmDBVMWatchConfigUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmDBVMWatchConfig }

  TfrmDBVMWatchConfig = class(TForm)
    btnOK: TButton;
    Button2: TButton;
    cbLockPage: TCheckBox;
    cbMultipleRIP: TCheckBox;
    cbWholePage: TCheckBox;
    cbSaveFPU: TCheckBox;
    cbSaveStack: TCheckBox;
    edtPhysicalAddress: TEdit;
    edtMaxEntries: TEdit;
    gbAccessType: TGroupBox;
    lblPhysicalAddress: TLabel;
    lblVirtualAddress: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    rbExecuteAccess: TRadioButton;
    rbWriteAccess: TRadioButton;
    rbReadAccess: TRadioButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblPhysicalAddressClick(Sender: TObject);
  private
    { private declarations }
    fAddress: qword;
    fPhysicalAddress: qword;
    procedure setAddress(a: qword);
    function getWatchType: integer;
    procedure setWatchType(t:integer);
    function getOptions: DWORD;
    procedure setOptions(o: DWORD);
    function getLockPage: boolean;
    procedure setLockPage(o: boolean);
    function getMaxEntries: integer;
    procedure setMaxEntries(m:integer);
  public
    property Address: qword read fAddress write setAddress;
    property PhysicalAddress: qword read fPhysicalAddress;
    property Watchtype: integer read getWatchType write setWatchType;
    property Options: DWORD read getOptions write setOptions;
    property LockPage: boolean read getLockPage write setLockPage;
    property MaxEntries: integer read getMaxEntries write setMaxEntries;
    { public declarations }
  end;

var
  frmDBVMWatchConfig: TfrmDBVMWatchConfig;

implementation

{$R *.lfm}

uses math,NewKernelHandler, ProcessHandlerUnit, vmxfunctions, registry{$ifdef darwin},macport{$endif};

function TfrmDBVMWatchConfig.getMaxEntries: integer;
begin
  result:=StrToInt(edtMaxEntries.text);
end;

procedure TfrmDBVMWatchConfig.setMaxEntries(m:integer);
begin
  edtMaxEntries.text:=inttostr(m);
end;

function TfrmDBVMWatchConfig.getLockPage: boolean;
begin
  result:=cbLockPage.checked;
end;

procedure TfrmDBVMWatchConfig.setLockPage(o: boolean);
begin
  cblockpage.checked:=o;
end;

function TfrmDBVMWatchConfig.getOptions: DWORD;
begin
  result:=0;
  if cbSaveFPU.checked then result:=result or EPTO_SAVE_FXSAVE;
  if cbSaveStack.checked then result:=result or EPTO_SAVE_STACK;
  if cbMultipleRIP.checked then result:=result or EPTO_MULTIPLERIP;
  if cbWholePage.checked then result:=result or EPTO_LOG_ALL;
end;

procedure TfrmDBVMWatchConfig.setOptions(o: DWORD);
begin
  cbSaveFPU.checked:=(o and EPTO_SAVE_FXSAVE)>0;
  cbSaveStack.checked:=(o and EPTO_SAVE_STACK)>0;
  cbMultipleRIP.checked:=(o and EPTO_MULTIPLERIP)>0;
  cbWholePage.checked:=(o and EPTO_LOG_ALL)>0;
end;

function TfrmDBVMWatchConfig.getWatchType: integer;
begin
  result:=0;

  if rbWriteAccess.checked then result:=0 else
  if rbReadAccess.checked then result:=1 else
  if rbExecuteAccess.checked then result:=2;
end;

procedure TfrmDBVMWatchConfig.setWatchType(t:integer);
begin
  case t of
    0: rbWriteAccess.checked:=true;
    1: rbReadAccess.checked:=true;
    2: rbExecuteAccess.Checked:=true;
  end;
end;

procedure TfrmDBVMWatchConfig.btnOKClick(Sender: TObject);
var i: integer;
begin
  if edtPhysicalAddress.visible then
  begin
    try
      fPhysicalAddress:=strtoint64('$'+edtPhysicalAddress.Text);
    except
      messagedlg('The provided physical address '+edtPhysicalAddress.text+' is invalid', mtError,[mbok],0);
      exit;
    end;
  end;

  if TryStrToInt(edtMaxEntries.text,i) then
    modalresult:=mrok
  else
  begin
    beep;
    edtMaxEntries.SetFocus;
    edtMaxEntries.SelectAll;
  end;

end;

procedure TfrmDBVMWatchConfig.FormCreate(Sender: TObject);
var
  reg: Tregistry;
begin
  //load default options
  reg:=TRegistry.create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey('\Software\Cheat Engine\DBVMWatch', false) then
    begin
      if reg.ValueExists('Lock Page') then cbLockPage.checked:=reg.ReadBool('Lock Page');
      if reg.ValueExists('Log FPU') then cbSaveFPU.checked:=reg.ReadBool('Log FPU');
      if reg.ValueExists('Log Stack') then cbSaveStack.checked:=reg.ReadBool('Log Stack');
      if reg.ValueExists('Multiple matching RIP') then cbMultipleRIP.checked:=reg.ReadBool('Multiple matching RIP');
      if reg.ValueExists('Max number of entries') then edtMaxEntries.text:=inttostr(reg.ReadInteger('Max number of entries'));
      if reg.ValueExists('Log whole page') then cbWholePage.checked:=reg.ReadBool('Log whole page');

      if reg.ValueExists('Watchtype') then Watchtype:=reg.ReadInteger('Watchtype');
    end;


  finally
    reg.free;
  end;
end;

procedure TfrmDBVMWatchConfig.FormDestroy(Sender: TObject);
var
  reg: tregistry;
  max: integer;
begin
  //save default options
  reg:=TRegistry.create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey('\Software\Cheat Engine\DBVMWatch', true) then
    begin
      reg.writeBool('Lock Page', cbLockPage.checked);
      reg.writeBool('Log FPU', cbSaveFPU.checked);
      reg.writeBool('Log Stack', cbSaveStack.checked);
      reg.writeBool('Multiple matching RIP', cbMultipleRIP.checked);
      reg.writeBool('Log whole page', cbWholePage.checked);

      if TryStrToInt(edtMaxEntries.text, max) then
         reg.WriteInteger('Max number of entries', max);

      reg.writeInteger('Watchtype', WatchType);
    end;
  finally
    reg.free;
  end;
end;

procedure TfrmDBVMWatchConfig.lblPhysicalAddressClick(Sender: TObject);
begin

end;

procedure TfrmDBVMWatchConfig.setAddress(a: qword);
var
  x: ptruint;
  temp: dword;
  s: string;
begin
  {$ifdef windows}
  faddress:=a;
  lblVirtualAddress.caption:=format('Virtual Address=%.8x',[a]);


  if ReadProcessMemory(processhandle, pointer(a),@temp,1,x) then
  begin
    if GetPhysicalAddress(processhandle, pointer(a), fPhysicalAddress) then
      lblPhysicalAddress.caption:=format('Physical Address=%.8x',[fPhysicalAddress])
    else
    begin
      edtPhysicalAddress.visible:=true;
      cbLockPage.enabled:=false;
    end;

    btnOK.Enabled:=true;
  end;

  if btnok.enabled=false then
  begin
    lblPhysicalAddress.caption:='Physical Address=invalid';
    lblPhysicalAddress.font.color:=clRed;
  end;
  {$endif}
end;

end.

