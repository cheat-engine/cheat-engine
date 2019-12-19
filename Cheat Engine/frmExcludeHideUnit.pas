unit frmExcludeHideUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEFuncProc, ExtCtrls, LResources;

type

  { TfrmExcludeHide }

  TfrmExcludeHide = class(TForm)
    Label4: TLabel;
    Panel1: TPanel;
    Label1: TLabel;
    Panel5: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Panel2: TPanel;
    Label2: TLabel;
    ListBox1: TListBox;
    Panel3: TPanel;
    Label3: TLabel;
    ListBox2: TListBox;
    Panel4: TPanel;
    Button2: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox2DblClick(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmExcludeHide: TfrmExcludeHide;

implementation

uses formsettingsunit, processlist;

procedure TfrmExcludeHide.FormCreate(Sender: TObject);
var i:integer;
begin

  //get processlist
  GetProcessList(listbox1);
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
var i: integer;
begin

  if listbox2.selcount>0 then
  begin
    i:=0;
    while i<listbox2.Count do
    begin
      if listbox2.Selected[i] then
        listbox2.Items.Delete(i)
      else
        inc(i);
    end;
  end;
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

procedure TfrmExcludeHide.Panel1Resize(Sender: TObject);
begin
  panel2.Left:=0;
  panel2.Width:=(panel1.ClientWidth div 2)-2;
  panel3.Left:=panel2.Width+2;
  panel3.Width:=(panel1.ClientWidth div 2)-2;

  panel4.Left:=(panel1.ClientWidth div 2) - (panel4.Width div 2);
end;

initialization
  {$i frmExcludeHideUnit.lrs}

end.
