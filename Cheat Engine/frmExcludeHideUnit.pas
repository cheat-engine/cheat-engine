unit frmExcludeHideUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,cefuncproc;

type
  TfrmExcludeHide = class(TForm)
    Label1: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    RadioButton1: TRadioButton;
    Label4: TLabel;
    RadioButton2: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox2DblClick(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmExcludeHide: TfrmExcludeHide;

implementation

{$R *.dfm}
uses formsettingsunit;

procedure TfrmExcludeHide.FormCreate(Sender: TObject);
var i:integer;
begin
  //get processlist
  GetProcessListSmall(listbox1);
  //and fill listbox2 with the items from the donthidelist;
  for i:=0 to length(formsettings.tempdonthidelist)-1 do
    listbox2.Items.Add(formsettings.tempdonthidelist[i]);

  if not formsettings.tempHideAll then radiobutton1.Checked:=true else radiobutton2.checked:=true; 
end;

procedure TfrmExcludeHide.ListBox1DblClick(Sender: TObject);
begin
  if (listbox1.ItemIndex<>-1) and (listbox2.Items.IndexOf(listbox1.Items[listbox1.itemindex])=-1) then
    listbox2.Items.Add(lowercase(listbox1.Items[listbox1.itemindex]));
end;

procedure TfrmExcludeHide.Button1Click(Sender: TObject);
var i: integer;
begin
  setlength(formsettings.tempdonthidelist,listbox2.items.count);
  for i:=0 to listbox2.items.count-1 do
    formsettings.tempdonthidelist[i]:=listbox2.Items[i];

  formsettings.tempHideAll:=radiobutton2.checked;

  modalresult:=mrok;
end;

procedure TfrmExcludeHide.ListBox2DblClick(Sender: TObject);
begin
  if listbox2.ItemIndex<>-1 then
    listbox2.DeleteSelected;
end;

procedure TfrmExcludeHide.RadioButton1Click(Sender: TObject);
var rb1check: boolean;
begin
  rb1check:=not radiobutton1.checked;
  label1.enabled:=rb1check;
  label2.Enabled:=rb1check;
  label3.Enabled:=rb1check;
  listbox1.Enabled:=rb1check;
  listbox2.Enabled:=rb1check;
end;

end.
