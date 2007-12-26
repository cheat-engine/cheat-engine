unit style3unit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls;

type
  TStyle3 = class(TForm)
    Label1: TLabel;
    CheckListBox1: TCheckListBox;
    Image1: TImage;
    Image3: TImage;
    Image4: TImage;
    Image6: TImage;
    Image2: TImage;
    Image5: TImage;
    procedure Image2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure Image3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Style3: TStyle3;

implementation

uses Unit1;

{$R *.dfm}

procedure TStyle3.Image2Click(Sender: TObject);
begin
  application.Terminate;
end;

procedure TStyle3.FormShow(Sender: TObject);
var i: Integer;
    ingroups: grouptype;
begin
  style3.Caption:=form1.TrainerTitle;
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

procedure TStyle3.CheckListBox1ClickCheck(Sender: TObject);
begin
  if not checklistbox1.Checked[checklistbox1.ItemIndex] then
    form1.uncheckitem(checklistbox1.itemindex);

  if checklistbox1.Checked[checklistbox1.ItemIndex] then
    if form1.checkitem(checklistbox1.itemindex)=false then checklistbox1.Checked[checklistbox1.ItemIndex]:=false;

end;

procedure TStyle3.Image3MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (ssLeft in Shift) then
  begin
      ReleaseCapture;
      SendMessage(Handle,WM_NCLBUTTONDOWN,HTCAPTION,0);
  end;
end;

procedure TStyle3.Image5Click(Sender: TObject);
begin
  Application.Terminate;
end;

end.
