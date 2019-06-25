unit AccessedMemory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, ComCtrls, genericHotkey, DBK32functions, commonTypeDefs;

resourcestring
  rsAMError = 'Error';
  rsAMYouCantSaveAnEmptyList = 'You can''t save an empty list';

type

  { TfrmAccessedMemory }

  TfrmAccessedMemory = class(TForm)
    btnClearSmallSnapshot: TButton;
    btnClearSmallSnapshot1: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    famrImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    procedure btnClearSmallSnapshot1Click(Sender: TObject);
    procedure btnClearSmallSnapshotClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Edit2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);

    procedure startMonitor(sender: TObject);
    procedure stopMonitor(sender: TObject);

  private
    { private declarations }
    startk, stopk: TKeyCombo;

    hkStart: TGenericHotkey;
    hkStop: TGenericHotkey;

    ranges: TPRangeDynArray;

  public
    { public declarations }


  end;

var
  frmAccessedMemory: TfrmAccessedMemory;

implementation

{$R *.lfm}

uses ProcessHandlerUnit, CEFuncProc;

{ TfrmAccessedMemory }

procedure TfrmAccessedMemory.startMonitor(sender: TObject);
begin
  DBK32Initialize;
  MarkAllPagesAsNonAccessed(ProcessHandle);
  button3.enabled:=true;
end;

procedure TfrmAccessedMemory.stopMonitor(sender: TObject);
begin
  if button3.enabled then
  begin
    EnumAndGetAccessedPages(processhandle, ranges);
    listview1.items.count:=length(ranges);

    button3.enabled:=false;
  end;
end;

procedure TfrmAccessedMemory.FormCreate(Sender: TObject);
begin

end;

procedure TfrmAccessedMemory.FormShow(Sender: TObject);
begin
  btnClearSmallSnapshot.autosize:=true;
  btnClearSmallSnapshot1.autosize:=true;
  btnClearSmallSnapshot.autosize:=false;
  btnClearSmallSnapshot1.autosize:=false;

  btnClearSmallSnapshot.ClientHeight:=canvas.TextHeight(btnClearSmallSnapshot.caption)+3;
  btnClearSmallSnapshot1.ClientHeight:=canvas.TextHeight(btnClearSmallSnapshot.caption)+3;

  button2.autosize:=true;
  button3.autosize:=true;
  button2.autosize:=false;
  button3.autosize:=false;

  if button2.width>button3.width then button3.width:=button2.width else button2.width:=button3.width;

  panel1.autosize:=false;
  autosize:=false;
end;

procedure TfrmAccessedMemory.ListView1Data(Sender: TObject; Item: TListItem);
begin
  if (item.index>=0) and (item.index<length(ranges)) then
  begin
    item.caption:=inttostr(item.index)+':'+inttohex(ranges[item.index].startAddress,8);
    item.SubItems.Add(inttohex(ranges[item.index].endaddress,8));
  end
  else
    item.Caption:=rsAMError;
end;

procedure TfrmAccessedMemory.MenuItem2Click(Sender: TObject);
var f: tfilestream;
begin
  if OpenDialog1.Execute then
  begin
    f:=tfilestream.Create(opendialog1.filename, fmOpenRead);
    setlength(ranges, f.Size div sizeof(TPRange));
    f.ReadBuffer(ranges[0], f.size);
    f.free;

    listview1.Items.count:=length(ranges);
  end;
end;

procedure TfrmAccessedMemory.MenuItem3Click(Sender: TObject);
var f:tfilestream;
begin
  if length(ranges)=0 then
    MessageDlg(rsAMYouCantSaveAnEmptyList, mtError, [mbok], 0)
  else
  if savedialog1.execute then
  begin
    f:=tfilestream.Create(savedialog1.filename, fmcreate);
    f.WriteBuffer(ranges[0], length(ranges)*sizeof(TPRange));
    f.free;
  end;
end;

procedure TfrmAccessedMemory.MenuItem4Click(Sender: TObject);
begin
  listview1.Items.Count:=0;
  listview1.Clear;

  setlength(ranges,0);
end;

procedure TfrmAccessedMemory.Button2Click(Sender: TObject);
begin

end;

procedure TfrmAccessedMemory.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if startk[4]=0 then
  begin
    for i:=0 to 4 do
      if startk[i]=0 then
      begin
        startk[i]:=key;
        break;
      end else
      if startk[i]=key then break;
  end;

  edit1.Text:=ConvertKeyComboToString(startk);
end;

procedure TfrmAccessedMemory.Edit2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if stopk[4]=0 then
  begin
    for i:=0 to 4 do
      if stopk[i]=0 then
      begin
        stopk[i]:=key;
        break;
      end else
      if startk[i]=key then break;
  end;

  edit2.Text:=ConvertKeyComboToString(stopk);
end;


procedure TfrmAccessedMemory.Button1Click(Sender: TObject);
begin
  if hkstart<>nil then
    freeandnil(hkstart);

  if hkstop<>nil then
    freeandnil(hkstop);

  if startk[0]<>0 then
    hkStart:=TGenericHotkey.create(@startMonitor, startk);

  if stopk[0]<>0 then
    hkStop:=TGenericHotkey.create(@stopMonitor, stopk);


end;

procedure TfrmAccessedMemory.btnClearSmallSnapshotClick(Sender: TObject);
begin
  FillWord(startk, 5,0);
  edit1.text:='';
  edit1.SetFocus;
end;

procedure TfrmAccessedMemory.btnClearSmallSnapshot1Click(Sender: TObject);
begin
  FillWord(stopk, 5,0);
  edit2.text:='';
  edit2.SetFocus;
end;


end.

