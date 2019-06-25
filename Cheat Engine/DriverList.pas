unit DriverList;

{$MODE Delphi}

interface

uses
  jwaWindows, windows, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Menus, imagehlp, CEFuncProc,
  NewKernelHandler, LREsources, ComCtrls, registry;

resourcestring
  rsDLNothingFound = 'nothing found';

type

  { TfrmDriverlist }

  TfrmDriverlist = class(TForm)
    Button1: TButton;
    FindDialog1: TFindDialog;
    FindDialog2: TFindDialog;
    dlImageList: TImageList;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Find1: TMenuItem;
    tvDriverList: TTreeView;
    procedure FindDialog2Find(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure tvDriverListDblClick(Sender: TObject);
    procedure tvDriverListExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  private
    { Private declarations }

  public
    { Public declarations }
  end;


function systemroot: string;

var
  frmDriverlist: TfrmDriverlist;


implementation


uses MemoryBrowserFormUnit, PEInfoFunctions;


var _systemroot: string;

function systemroot: string;
var reg: tregistry;
begin
  if _systemroot='' then
  begin
    reg:=tregistry.Create;
    reg.RootKey:=HKEY_LOCAL_MACHINE;
    if reg.OpenKey('\SOFTWARE\Microsoft\Windows NT\CurrentVersion',false) then
    begin
      if reg.ValueExists('SystemRoot') then
        _systemroot:=reg.ReadString('SystemRoot');
    end;

    if _systemroot='' then _systemroot:='c:\windows\';

    if _systemroot[length(systemroot)]<>'\' then
      _systemroot:=systemroot+'\';

    reg.free;
  end;

  result:=_systemroot;
end;


procedure TfrmDriverlist.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;





procedure TfrmDriverlist.FormCreate(Sender: TObject);
var
  list: TStringlist;
  i: integer;
  tn: TTreeNode;
begin
  list:=TStringlist.create;
  getDriverList(list);
  list.Sorted:=true;
  for i:=0 to list.count-1 do
  begin
    tn:=tvDriverList.Items.add(nil,list[i]);
    tn.Data:=pointer(list.Objects[i]);
    tn.HasChildren:=true;
  end;

end;

procedure TfrmDriverlist.FindDialog2Find(Sender: TObject);
var
  i,j: integer;
  n: TTreenode;
  wasexpanded: boolean;
begin
  tvDriverList.BeginUpdate;
  try
    n:=tvDriverList.Selected;
    if n=nil then
      n:=tvDriverList.Items.GetFirstNode
    else
    begin
      if pos(uppercase(finddialog2.FindText),uppercase(n.text))>0 then
        n:=n.GetNext;
    end;




    while n<>nil do
    begin
      if n.level=1 then
      begin
        if pos(uppercase(finddialog2.FindText),uppercase(n.text))>0 then
        begin
          n.Selected:=true;
          n.MakeVisible;
          exit;
        end;
      end;

      if n.HasChildren then
      begin
        if n.count=0 then
        begin
          n.Expand(false);
          n.Collapse(false);
        end;
      end;

      n:=n.GetNext;
    end;
    showmessage(rsDLNothingFound);

  finally
    tvDriverList.EndUpdate;
  end;
end;

procedure TfrmDriverlist.FindDialog1Find(Sender: TObject);
var
  i,j: integer;
  n: TTreenode;

begin
  n:=tvDriverList.Selected;
  if n=nil then
    n:=tvDriverList.Items.GetFirstNode;

  while n.Parent<>nil do
    n:=n.parent;

  while n<>nil do
  begin
    if pos(uppercase(finddialog1.FindText),uppercase(n.text))>0 then
    begin
      n.Selected:=true;
      n.MakeVisible;
      exit;
    end;
    n:=n.GetNextSibling;
  end;

  showmessage(rsDLNothingFound);
end;

procedure TfrmDriverlist.Find1Click(Sender: TObject);
begin
  finddialog1.Execute;
end;

procedure TfrmDriverlist.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmDriverlist.MenuItem1Click(Sender: TObject);
begin
  finddialog2.Execute;
end;

procedure TfrmDriverlist.tvDriverListDblClick(Sender: TObject);
begin
  if (tvDriverList.Selected<>nil) and (tvDriverList.Selected.data<>nil) then
    MemoryBrowser.disassemblerview.SelectedAddress:=ptruint(tvDriverList.Selected.data);
end;



procedure TfrmDriverlist.tvDriverListExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  exportlist: tstringlist;
  p: pchar;
  driverpath: string;
  r: integer;
  i: integer;
  tn: TTreenode;
begin


  if (node.level=0) and (node.count=0) then
  begin
    //get the exportlist
    getmem(p,256);
    r:=GetDeviceDriverFileNameA(node.data, p,255);
    p[r]:=#0;

    driverpath:=p;
    driverpath:=StringReplace(driverpath,'\??\','',[]);
    driverpath:=StringReplace(driverpath,'\SystemRoot\',systemroot,[rfIgnoreCase]);

    if r<>0 then
    begin
      tvDriverList.Items.AddChild(node,driverpath+':');
      exportlist:=TStringList.create;
      try
        peinfo_getExportList(driverpath,exportlist);
      except
      end;
      for i:=0 to exportlist.count-1 do
      begin
        tn:=tvDriverList.Items.AddChild(node, inttohex(ptruint(node.data)+ptruint(exportlist.Objects[i]),8)+' - '+exportlist[i]);
        tn.data:=pointer(ptruint(node.data)+ptruint(exportlist.Objects[i]));
      end;

      exportlist.free;
    end
    else
      node.HasChildren:=false;

    freemem(p);
  end;
end;

initialization
  {$i DriverList.lrs}


finalization

end.


