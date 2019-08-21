unit frmExceptionRegionListUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus,commonTypeDefs;

type

  { TfrmExceptionRegionList }

  TfrmExceptionRegionList = class(TForm)
    btnAdd: TButton;
    cbAutoAddAllocs: TCheckBox;
    edtStart: TEdit;
    edtStop: TEdit;
    erlImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbRegionList: TListBox;
    MainMenu1: TMainMenu;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miClear: TMenuItem;
    miFile: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveDialog1: TSaveDialog;
    procedure btnAddClick(Sender: TObject);
    procedure cbAutoAddAllocsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbRegionListDblClick(Sender: TObject);
    procedure miClearClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
  private
    { private declarations }
    function getRegionFromString(s: string): TMemoryRegionSmall;
  public
    { public declarations }
    procedure updateList;
  end;

var
  frmExceptionRegionList: TfrmExceptionRegionList;

implementation

{$R *.lfm}

uses UnexpectedExceptionsHelper, MemoryBrowserFormUnit, CEFuncProc,
  symbolhandler, globals;

{ TfrmExceptionRegionList }

resourcestring
  rsConfirmDeleteRegion = 'Are you sure you wish to remove the range %s?';
  rsEraseListQuestion = 'Are you sure the erase the list?';

procedure TfrmExceptionRegionList.btnAddClick(Sender: TObject);
var start, stop: ptruint;

  size: integer;
begin
  start:=symhandler.getAddressFromName(edtStart.text);
  stop:=symhandler.getAddressFromName(edtStop.text);

  size:=stop-start+1;

  AddUnexpectedExceptionRegion(start, size);
end;

procedure TfrmExceptionRegionList.cbAutoAddAllocsChange(Sender: TObject);
var n: TNotifyEvent;
begin
  n:=MemoryBrowser.miExceptionRegionAutoAddAllocs.OnClick;
  MemoryBrowser.miExceptionRegionAutoAddAllocs.OnClick:=nil;
  MemoryBrowser.miExceptionRegionAutoAddAllocs.checked:=cbAutoAddAllocs.Checked;
  MemoryBrowser.miExceptionRegionAutoAddAllocs.OnClick:=n;

  allocsAddToUnexpectedExceptionList:=cbAutoAddAllocs.Checked;
end;

procedure TfrmExceptionRegionList.FormCreate(Sender: TObject);
begin
  LoadFormPosition(self);
end;

procedure TfrmExceptionRegionList.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TfrmExceptionRegionList.FormShow(Sender: TObject);
var n: TNotifyEvent;
begin
  updatelist;
  n:=cbAutoAddAllocs.OnChange;
  cbAutoAddAllocs.OnChange:=nil;
  cbAutoAddAllocs.Checked:=MemoryBrowser.miExceptionRegionAutoAddAllocs.checked;
  cbAutoAddAllocs.OnChange:=n;

  edtstart.Constraints.MinWidth:=canvas.TextWidth(' DDDDDDDDDDDDDDDD ');
end;

function TfrmExceptionRegionList.getRegionFromString(s: string): TMemoryRegionSmall;
var
  i: integer;
  start,stop: string;
  a,b: ptruint;
  size: integer;
begin
  i:=pos('-',s);
  if i>0 then
  begin
    start:=trim(copy(s,1,i));
    stop:=trim(copy(s,i+1,length(s)));

    a:=strtoint64(start);
    b:=strtoint64(stop);
    size:=b-a-1;

    result.Address:=a;
    result.size:=size;
  end
  else
    raise exception.create('Invalid string passed to getRegionFromString');
end;

procedure TfrmExceptionRegionList.lbRegionListDblClick(Sender: TObject);
var
  r: TMemoryRegionSmall;
  s: string;
begin
  if lbregionlist.itemindex<>-1 then
  begin
    s:=lbRegionList.items[lbRegionList.itemindex];
    if messagedlg(format(rsConfirmDeleteRegion, [s]),mtConfirmation,[mbyes,mbno],0)=mryes then
    begin
      r:=getRegionFromString(s);
      RemoveUnexpectedExceptionRegion(r.Address,r.size);
    end;
  end;
end;

procedure TfrmExceptionRegionList.miClearClick(Sender: TObject);
begin
  if messagedlg(rsEraseListQuestion, mtConfirmation, [mbyes, mbno], 0)=mryes then
    ClearUnexpectedExceptionRegions;
end;

procedure TfrmExceptionRegionList.miOpenClick(Sender: TObject);
var
  s: TStringList;
  i: integer;
  r: TMemoryRegionSmall;
begin
  if OpenDialog1.execute then
  begin
    s:=tstringlist.create;
    s.LoadFromFile(opendialog1.FileName, true);

    ClearUnexpectedExceptionRegions;
    for i:=0 to s.Count-1 do
    begin
      r:=getRegionFromString(s[i]);

      AddUnexpectedExceptionRegion(r.Address, r.size);
    end;
  end;
end;

procedure TfrmExceptionRegionList.miSaveClick(Sender: TObject);
begin
  if SaveDialog1.execute then
    lbregionlist.Items.SaveToFile(opendialog1.FileName);
end;

procedure TfrmExceptionRegionList.updateList;
begin
  getUnexpectedExceptionRegionList(lbRegionList.Items);
end;

end.

