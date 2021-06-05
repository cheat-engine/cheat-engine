unit frmRearrangeStructureListUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Menus, betterControls;

type

  { TfrmRearrangeStructureList }

  TfrmRearrangeStructureList = class(TForm)
    lbStructlist: TListBox;
    MenuItem1: TMenuItem;
    Panel4: TPanel;
    PopupMenu1: TPopupMenu;
    spbDown: TSpeedButton;
    spbUp: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure spbDownClick(Sender: TObject);
    procedure spbUpClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure updateList;
  end;

implementation

{$R *.lfm}

uses StructuresFrm2;

{ TfrmRearrangeStructureList }


procedure TfrmRearrangeStructureList.updatelist;
var i: integer;
begin
  for i:=0 to DissectedStructs.Count-1 do
    lbStructlist.Items.AddObject(TDissectedStruct(DissectedStructs[i]).name, DissectedStructs[i]);
end;

procedure TfrmRearrangeStructureList.FormShow(Sender: TObject);
begin
  updatelist;
end;

procedure TfrmRearrangeStructureList.MenuItem1Click(Sender: TObject);
var
  s: TDissectedStruct;
  i: integer;
begin
  if MessageDlg('are you sure you wish to delete the selection?',mtConfirmation,[mbyes,mbno],0)=mryes then
  begin
    for i:=DissectedStructs.count-1 downto 0 do
      if lbStructlist.Selected[i] then
      begin
        s:=TDissectedStruct(lbStructlist.Items.Objects[i]);
        s.free;
      end;

    lbStructlist.DeleteSelected;
  end;
end;

procedure TfrmRearrangeStructureList.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var i: integer;
begin
  if lbStructlist.count<>DissectedStructs.count then exit;

  for i:=0 to DissectedStructs.count-1 do
    dissectedstructs[i]:=lbStructlist.Items.Objects[i];

  for i:=0 to frmStructures2.count-1 do
    TfrmStructures2(frmStructures2[i]).onStructListChange;
end;



procedure TfrmRearrangeStructureList.spbDownClick(Sender: TObject);
var i: integer;
begin
  for i:=lbStructlist.items.count-2 downto 0 do
  begin
    if lbStructlist.Selected[i] then
    begin
      lbStructlist.selected[i]:=false;
      lbStructlist.Items.Exchange(i, i+1);
      lbStructlist.selected[i+1]:=true;
    end;

  end;

end;

procedure TfrmRearrangeStructureList.spbUpClick(Sender: TObject);
var i: integer;
begin
  for i:=1 to lbStructlist.items.count-1 do
  begin
    if lbStructlist.Selected[i] then
    begin
      lbStructlist.selected[i]:=false;
      lbStructlist.Items.Exchange(i, i-1);
      lbStructlist.selected[i-1]:=true;
    end;

  end;

end;

end.

