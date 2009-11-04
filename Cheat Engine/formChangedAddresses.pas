unit formChangedAddresses;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,cefuncproc, ExtCtrls, ComCtrls, Menus;

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
  end;

var
  frmChangedAddresses: TfrmChangedAddresses;

implementation

{$R *.dfm}

uses debugger, MainUnit, frmRegistersunit, MemoryBrowserFormUnit;

procedure TfrmChangedAddresses.OKButtonClick(Sender: TObject);
var temp: dword;
    i: integer;
begin
  if OKButton.caption='Stop' then
  begin
    if WaitForSingleObject(semaphore,10000)=WAIT_FAILED then
      raise exception.Create('OMG! I can''t stop it!');

    try
      if debuggerthread=nil then exit;

      debuggerthread.Suspend;
      debuggerthread.breakpointset:=false;

      zeromemory(@debuggerthread.DRRegs,sizeof(debuggerthread.DRRegs));
      debuggerthread.DRRegs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
      debuggerthread.DRRegs.Dr7:=reg0set or reg1set or reg2set or reg3set;

      with debuggerthread do
      for i:=0 to length(threadlist)-1 do
      begin
        suspendthread(threadlist[i,1]);
        setthreadcontext(threadlist[i,1],DRRegs);
        resumethread(threadlist[i,1]);
      end;

      debuggerthread.Resume;

    finally
      releasesemaphore(semaphore,1,nil);
    end;

    okButton.Caption:='Close';
  end else close;


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

  if frmChangedAddresses=self then frmChangedAddresses:=nil;
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
  begin
    ad:=strtoint('$'+changedlist.Selected.Caption);
    with mainform do
    begin
      i:=0;
      while (i<numberofrecords) and
            (
              (memrec[i].Address<>ad) or
              (memrec[i].VarType<>2)
            )  do
            inc(i);
      //find out of this record is in the list
      if i=NumberOfRecords then
      begin
        inc(NumberOfRecords);
        ReserveMem;

        memrec[NumberOfRecords-1].Description:='No Description';
        memrec[NumberOfRecords-1].Address:=ad;

        memrec[NumberOfRecords-1].VarType:=cbDisplayType.itemindex;

        memrec[NumberOfRecords-1].Frozen:=false;
        memrec[NumberOfRecords-1].FrozenValue:=0;

        memrec[NumberOfRecords-1].Bit:=0;
        hotkeys[NumberOfRecords-1]:=-1;

        updatescreen;
        updatelist;
      end
      else raise Exception.Create(IntToHex(ad,8)+' is already in the list!');


    end;
  end;
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
        0: s:=ReadAndParseAddress(strtoint('$'+changedlist.items[i].caption), vtByte);
        1: s:=ReadAndParseAddress(strtoint('$'+changedlist.items[i].caption), vtWord);
        2: s:=ReadAndParseAddress(strtoint('$'+changedlist.items[i].caption), vtDWord);
        3: s:=ReadAndParseAddress(strtoint('$'+changedlist.items[i].caption), vtSingle);
        4: s:=ReadAndParseAddress(strtoint('$'+changedlist.items[i].caption), vtDouble);
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
  begin
    memorybrowser.memoryaddress:=strtoint('$'+changedlist.Selected.Caption);
    memorybrowser.refreshmb;
  end;
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

end.

