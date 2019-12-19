unit frmRearrangeStructureListUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TfrmRearrangeStructureList }

  TfrmRearrangeStructureList = class(TForm)
    lbStructlist: TListBox;
    Panel4: TPanel;
    spbDown: TSpeedButton;
    spbUp: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure lbStructlistSelectionChange(Sender: TObject; User: boolean);
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

procedure TfrmRearrangeStructureList.lbStructlistSelectionChange(
  Sender: TObject; User: boolean);
var index: integer;
begin
  index:=lbStructlist.itemindex;
  spbDown.enabled:=(index>=0) and (index<lbStructlist.count-1);
  spbUp.enabled:=(index>=1);
end;

procedure TfrmRearrangeStructureList.spbDownClick(Sender: TObject);
begin
  if (lbStructlist.itemindex>=0) and (lbStructlist.Count>lbStructlist.itemindex+1) then
  begin
    lbStructlist.Items.Exchange(lbStructlist.itemindex, lbStructlist.ItemIndex+1);
    lbStructlist.itemindex:=lbStructlist.itemindex+1;
  end;

  lbStructlistSelectionChange(lbStructlist,false);
end;

procedure TfrmRearrangeStructureList.spbUpClick(Sender: TObject);
begin
  if lbStructlist.itemindex>=1 then
  begin
    lbStructlist.Items.Exchange(lbStructlist.itemindex, lbStructlist.ItemIndex-1);
    lbStructlist.itemindex:=lbStructlist.itemindex-1;
  end;

  lbStructlistSelectionChange(lbStructlist,false);
end;

end.

