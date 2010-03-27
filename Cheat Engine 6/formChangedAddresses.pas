unit formChangedAddresses;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEFuncProc, ExtCtrls, ComCtrls, Menus, NewKernelHandler, LResources,
  disassembler, symbolhandler;

type
  TfrmChangedAddresses = class(TForm)
    lblInfo: TLabel;
    Panel1: TPanel;
    OKButton: TButton;
    Changedlist: TListView;
    cbDisplayType: TComboBox;
    Timer1: TTimer;
    PopupMenu1: TPopupMenu;
    Showregisterstates1: TMenuItem;
    Browsethismemoryregion1: TMenuItem;
    procedure OKButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ChangedlistDblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Showregisterstates1Click(Sender: TObject);
    procedure Browsethismemoryregion1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure refetchValues;
  public
    { Public declarations }
    procedure AddRecord;
  end;


implementation


uses CEDebugger, MainUnit, frmRegistersunit, MemoryBrowserFormUnit, debughelper, debugeventhandler;

procedure TfrmChangedAddresses.AddRecord;
var s: string;
haserror: boolean;
address: ptrUint;
 i: integer;
 li: tlistitem;
 currentthread: TDebugThreadHandler;
begin
  //the debuggerthread is idle at this point
  currentThread:=debuggerthread.CurrentThread;
  if currentthread<>nil then
  begin
    //get the instruction address
    address:=currentThread.context.{$ifdef cpu64}Rip{$else}eip{$endif};
    s:=disassemble(address);
    i:=pos('[',s)+1;
    s:=copy(s,i,pos(']',s)-i);

    address:=symhandler.getAddressFromName(s, false, haserror, @currentthread.context);

    if not hasError then
    begin
      //check if this address is already in the list
      s:=inttohex(address,8);
      for i:=0 to changedlist.Items.Count-1 do
        if changedlist.items[i].caption=s then exit;

      //and if not, add it
      li:=changedlist.Items.add;
      li.caption:=s;
      li.SubItems.Add('');
    end;
  end;
end;

procedure TfrmChangedAddresses.OKButtonClick(Sender: TObject);
var temp: dword;
    i: integer;
begin

  if OKButton.caption='Stop' then
  begin
    debuggerthread.FindWhatCodeAccessesStop(self);
    okButton.Caption:='Close';
  end
  else
    close;

end;

procedure TfrmChangedAddresses.FormClose(Sender: TObject;
  var Action: TCloseAction);
var temp:dword;
    i: integer;
begin
  action:=caFree;
  for i:=0 to changedlist.Items.Count-1 do
    freemem(changedlist.Items[i].Data);

  if OKButton.caption='Stop' then
    OKButton.Click;
end;

procedure TfrmChangedAddresses.FormShow(Sender: TObject);
begin
  OKButton.Caption:='Stop';
end;

procedure TfrmChangedAddresses.ChangedlistDblClick(Sender: TObject);
var i: integer;
    ad: dword;
begin
  if changedlist.Selected<>nil then
    mainform.addresslist.addaddress('No Description', changedlist.selected.caption, [],0, OldVarTypeToNewVarType(cbDisplayType.itemindex));
end;

procedure TfrmChangedAddresses.refetchValues;
var i: integer;
    s: string;
begin
  if changedlist.Items.Count>0 then
  begin
    if not timer1.Enabled then
      timer1.Enabled:=true;

    for i:=0 to changedlist.Items.Count-1 do
    begin
      case cbDisplayType.ItemIndex of
        0: s:=ReadAndParseAddress(strtoint64('$'+changedlist.items[i].caption), vtByte);
        1: s:=ReadAndParseAddress(strtoint64('$'+changedlist.items[i].caption), vtWord);
        2: s:=ReadAndParseAddress(strtoint64('$'+changedlist.items[i].caption), vtDWord);
        3: s:=ReadAndParseAddress(strtoint64('$'+changedlist.items[i].caption), vtSingle);
        4: s:=ReadAndParseAddress(strtoint64('$'+changedlist.items[i].caption), vtDouble);
      end;

      if Changedlist.Items[i].SubItems.Count=0 then
        Changedlist.Items[i].SubItems.Add('');

      Changedlist.Items[i].SubItems[0]:=s;
    end;
  end else timer1.Enabled:=false;
end;

procedure TfrmChangedAddresses.Timer1Timer(Sender: TObject);
begin
  refetchValues;
end;

procedure TfrmChangedAddresses.Showregisterstates1Click(Sender: TObject);
begin
  if changedlist.Selected<>nil then
  begin
    with TRegisters.create(self) do
    begin
      borderstyle:=bsSingle;
      SetContextPointer(changedlist.Selected.Data);
      show;
    end;
  end;

  //for frmfloatingpointpanel: SetContextPointer(changedlist.Selected.Data);
end;

procedure TfrmChangedAddresses.Browsethismemoryregion1Click(
  Sender: TObject);
begin
  if changedlist.Selected<>nil then
    memorybrowser.memoryaddress:=strtoint('$'+changedlist.Selected.Caption);

end;

procedure TfrmChangedAddresses.FormDestroy(Sender: TObject);
begin
  saveformposition(self,[]);
end;

procedure TfrmChangedAddresses.FormCreate(Sender: TObject);
var x: array of integer;
begin
  setlength(x, 0);
  loadformposition(self,x);
end;

initialization
  {$i formChangedAddresses.lrs}

end.

