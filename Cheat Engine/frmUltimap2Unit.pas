unit frmUltimap2Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, EditBtn, Menus, libipt;

type

  { TfrmUltimap2 }

  TfrmUltimap2 = class(TForm)
    btnExecuted: TButton;
    btnFilterCallCount: TButton;
    btnFilterModule: TButton;
    btnNotCalled: TButton;
    btnNotExecuted: TButton;
    btnRecordPause: TButton;
    btnResetCount: TButton;
    Button1: TButton;
    Button5: TButton;
    Button6: TButton;
    cbFilterFuturePaths: TCheckBox;
    cbfilterOutNewEntries: TCheckBox;
    gbRange: TGroupBox;
    lbRange: TListBox;
    miRangeDeleteSelected: TMenuItem;
    miRangeDeleteAll: TMenuItem;
    pmRangeOptions: TPopupMenu;
    rbLogToFolder: TRadioButton;
    rbRuntimeParsing: TRadioButton;
    deTargetFolder: TDirectoryEdit;
    edtCallCount: TEdit;
    edtBufSize: TEdit;
    lblBuffersPerCPU: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblIPCount: TLabel;
    lblLastfilterresult: TLabel;
    ListView1: TListView;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure miRangeDeleteSelectedClick(Sender: TObject);
    procedure miRangeDeleteAllClick(Sender: TObject);
  private
    { private declarations }
    l: tstringlist;
    function ModuleSelectEvent(index: integer; listText: string): string;
  public
    { public declarations }
  end;

var
  frmUltimap2: TfrmUltimap2;

implementation

{$R *.lfm}

uses symbolhandler, frmSelectionlistunit;

{ TfrmUltimap2 }

procedure TfrmUltimap2.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  canclose:=MessageDlg('Closing will free all collected data. Continue? (Tip: You can minimize this window instead)', mtConfirmation,[mbyes,mbno], 0, mbno)=mryes;
end;

function TfrmUltimap2.ModuleSelectEvent(index: integer; listText: string): string;
var
  mi: TModuleInfo;
  address: ptruint;
begin
  if (index<>-1) and (l<>nil) then
  begin
    address:=ptruint(l.Objects[index]);
    if symhandler.getmodulebyaddress(address, mi) then
      exit(inttohex(mi.baseaddress,8)+'-'+inttohex(mi.baseaddress+mi.basesize,8));
  end;

  result:=listText+' -error';
end;

procedure TfrmUltimap2.Button1Click(Sender: TObject);
var
  r: string;
  output: string;
  start, stop: ptruint;
begin
  if l=nil then
    l:=tstringlist.create;

  symhandler.getModuleList(l);
  output:='';
  ShowSelectionList(self, 'Module list', 'Select a module or give your own range', l, output, true, @ModuleSelectEvent);
  if output<>'' then
  begin
    //check that output can be parsed

    if symhandler.parseRange(output, start, stop) then
      lbrange.Items.Add(inttohex(start,8)+'-'+inttohex(stop,8));
  end;

  freeandnil(l);
end;

procedure TfrmUltimap2.FormDestroy(Sender: TObject);
begin
  //free the process image

  //free the buffers
  frmUltimap2:=nil;
end;

procedure TfrmUltimap2.miRangeDeleteSelectedClick(Sender: TObject);
var i: integer;
begin
  for i:=lbrange.Items.Count-1 downto 0 do
    if lbrange.Selected[i] then
      lbRange.Items.Delete(i);
end;

procedure TfrmUltimap2.miRangeDeleteAllClick(Sender: TObject);
begin
  lbRange.clear;
end;

end.

