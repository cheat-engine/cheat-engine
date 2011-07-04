unit style1unit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls;

type
  TStyle1 = class(TForm)
    Label1: TLabel;
    Image1: TImage;
    CheckListBox1: TCheckListBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Style1: TStyle1;

implementation

uses Unit1;

{$R *.dfm}

procedure TStyle1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  application.Terminate;
end;

procedure TStyle1.FormShow(Sender: TObject);
var i: Integer;
    ingroups: grouptype;
begin
  style1.Caption:=form1.TrainerTitle;
  label1.Caption:=form1.usercomments;
  if form1.UsePicture then
  begin
    form1.imagestream.Seek(0,sofrombeginning);
    image1.Picture.Bitmap.LoadFromStream(form1.imagestream);
  end;


  for i:=0 to form1.numberofrecords-1 do
  begin
    if (form1.memrec[i].Group>0) and (form1.groups[form1.memrec[i].Group]) then
    begin
      if (not ingroups[form1.memrec[i].Group]) then
      begin
        form1.MemrecToListbox[i]:=checklistbox1.Items.Count;
        ingroups[form1.memrec[i].Group]:=true;
        checklistbox1.Items.Add(form1.groupnames[form1.memrec[i].Group]);
      end else form1.MemrecToListbox[i]:=checklistbox1.Items.Count-1;

    end else
    begin
      form1.MemrecToListbox[i]:=checklistbox1.Items.Count;
      checklistbox1.Items.Add(form1.memrec[i].description);
    end;
  end;
end;

procedure TStyle1.CheckListBox1ClickCheck(Sender: TObject);
begin
  if not checklistbox1.Checked[checklistbox1.ItemIndex] then
    form1.uncheckitem(checklistbox1.itemindex);

  if checklistbox1.Checked[checklistbox1.ItemIndex] then
    if form1.checkitem(checklistbox1.itemindex)=false then checklistbox1.Checked[checklistbox1.ItemIndex]:=false;

end;

end.
