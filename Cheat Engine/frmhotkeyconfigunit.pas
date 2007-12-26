unit frmhotkeyconfigunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,hotkeyhandler,cefuncproc,mainunit2;

type
  TfrmHotkeyConfig = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    ListBox1: TListBox;
    Panel2: TPanel;
    Edit1: TEdit;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Panel3: TPanel;
    Label52: TLabel;
    Edit2: TEdit;
    Label51: TLabel;
    Edit3: TEdit;
    Panel4: TPanel;
    Edit4: TEdit;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    currentspeed: integer;
    increasespeed: boolean;
    procedure updatehotkey;
    procedure updatespeed;
  public
    { Public declarations }
    newhotkeys: array [0..28] of tkeycombo;
    newspeedhackspeed1: tspeedhackspeed;
    newspeedhackspeed2: tspeedhackspeed;
    newspeedhackspeed3: tspeedhackspeed;
    newspeedhackspeed4: tspeedhackspeed;
    newspeedhackspeed5: tspeedhackspeed;
    speedupdelta:       single;
    slowdowndelta:      single;
  end;

var
  frmHotkeyConfig: TfrmHotkeyConfig;

implementation

{$R *.dfm}


procedure TFrmHotkeyConfig.UpdateSpeed;
begin
  if panel4.Visible then
  begin
    if increasespeed then
    begin
      speedupdelta:=strtofloat(edit4.Text);
    end
    else
    begin
      slowdowndelta:=strtofloat(edit4.Text);
    end
  end;

  if panel3.Visible then
  begin
    if currentspeed=1 then
    begin
      newspeedhackspeed1.speed:=StrToFloat(edit2.Text);
      newspeedhackspeed1.sleeptime:=strtoint(edit3.Text);
    end else
    if currentspeed=2 then
    begin
      newspeedhackspeed2.speed:=StrToFloat(edit2.Text);
      newspeedhackspeed2.sleeptime:=strtoint(edit3.Text);
    end else
    if currentspeed=3 then
    begin
      newspeedhackspeed3.speed:=StrToFloat(edit2.Text);
      newspeedhackspeed3.sleeptime:=strtoint(edit3.Text);
    end else
    if currentspeed=4 then
    begin
      newspeedhackspeed4.speed:=StrToFloat(edit2.Text);
      newspeedhackspeed4.sleeptime:=strtoint(edit3.Text);
    end else
    if currentspeed=5 then
    begin
      newspeedhackspeed5.speed:=StrToFloat(edit2.Text);
      newspeedhackspeed5.sleeptime:=strtoint(edit3.Text);
    end;
  end;

  if (listbox1.ItemIndex>=3) and (listbox1.itemindex<=7) then
  begin
    currentspeed:=listbox1.ItemIndex-2;
    case currentspeed of
      1:
      begin
        edit2.text:=format('%.2f',[newspeedhackspeed1.speed]);
        edit3.text:=inttostr(newspeedhackspeed1.sleeptime);
      end;

      2:
      begin
        edit2.text:=format('%.2f',[newspeedhackspeed2.speed]);
        edit3.text:=inttostr(newspeedhackspeed2.sleeptime);
      end;

      3:
      begin
        edit2.text:=format('%.2f',[newspeedhackspeed3.speed]);
        edit3.text:=inttostr(newspeedhackspeed3.sleeptime);
      end;

      4:
      begin
        edit2.text:=format('%.2f',[newspeedhackspeed4.speed]);
        edit3.text:=inttostr(newspeedhackspeed4.sleeptime);
      end;

      5:
      begin
        edit2.text:=format('%.2f',[newspeedhackspeed5.speed]);
        edit3.text:=inttostr(newspeedhackspeed5.sleeptime);
      end;
    end;


    panel3.Visible:=true;
    panel4.Visible:=false;
  end else panel3.Visible:=false;

  if (listbox1.ItemIndex=8) or (listbox1.ItemIndex=9) then
  begin
    increasespeed:=listbox1.itemindex=8;
    if increasespeed then
      edit4.Text:=format('%.2f',[speedupdelta])
    else
      edit4.Text:=format('%.2f',[slowdowndelta]);

    panel4.visible:=true;
    panel3.Visible:=false;
  end else panel4.Visible:=false;

end;

procedure Tfrmhotkeyconfig.updatehotkey;
begin
  edit1.Text:=ConvertKeyComboToString(newhotkeys[listbox1.ItemIndex]);
  updatespeed;
end;

procedure TfrmHotkeyConfig.Button1Click(Sender: TObject);
begin
  updatespeed;
  modalresult:=mrok;
end;

procedure TfrmHotkeyConfig.FormShow(Sender: TObject);
begin
  listbox1.ItemIndex:=0;
  UpdateHotkey;
end;

procedure TfrmHotkeyConfig.ListBox1Click(Sender: TObject);
begin
  updatehotkey;
end;

procedure TfrmHotkeyConfig.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if newhotkeys[listbox1.ItemIndex][4]=0 then
  begin
    for i:=0 to 4 do
      if newhotkeys[listbox1.ItemIndex][i]=0 then
      begin
        newhotkeys[listbox1.ItemIndex][i]:=key;
        break;
      end else
      if newhotkeys[listbox1.ItemIndex][i]=key then break;
  end;

  edit1.Text:=ConvertKeyComboToString(newhotkeys[listbox1.ItemIndex]);
end;

procedure TfrmHotkeyConfig.Button3Click(Sender: TObject);
begin
  zeromemory(@newhotkeys[listbox1.ItemIndex][0],10);
  edit1.Text:=ConvertKeyComboToString(newhotkeys[listbox1.ItemIndex]);
  edit1.SetFocus;
end;

end.
