unit frameHotkeyConfigUnit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources, CEFuncProc, commonTypeDefs;

type
  TframeHotkeyConfig = class(TFrame)
    Panel1: TPanel;
    Label1: TLabel;
    ListBox1: TListBox;
    Panel2: TPanel;
    Label2: TLabel;
    Edit1: TEdit;
    Button3: TButton;
    Panel3: TPanel;
    Label52: TLabel;
    edtSHSpeed: TEdit;
    Panel4: TPanel;
    Label3: TLabel;
    Edit4: TEdit;
    Panel5: TPanel;
    edtKeypollInterval: TEdit;
    edtHotkeyDelay: TEdit;
    Label4: TLabel;
    Label5: TLabel;
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
    newhotkeys: array [0..30] of tkeycombo;
    newspeedhackspeed1: tspeedhackspeed;
    newspeedhackspeed2: tspeedhackspeed;
    newspeedhackspeed3: tspeedhackspeed;
    newspeedhackspeed4: tspeedhackspeed;
    newspeedhackspeed5: tspeedhackspeed;
    speedupdelta:       single;
    slowdowndelta:      single;    
  end;

implementation


procedure TFrameHotkeyConfig.UpdateSpeed;
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
      newspeedhackspeed1.speed:=StrToFloat(edtSHspeed.Text);
    end else
    if currentspeed=2 then
    begin
      newspeedhackspeed2.speed:=StrToFloat(edtSHspeed.Text);
    end else
    if currentspeed=3 then
    begin
      newspeedhackspeed3.speed:=StrToFloat(edtSHspeed.Text);
    end else
    if currentspeed=4 then
    begin
      newspeedhackspeed4.speed:=StrToFloat(edtSHspeed.Text);
    end else
    if currentspeed=5 then
    begin
      newspeedhackspeed5.speed:=StrToFloat(edtSHspeed.Text);
    end;
  end;

  if (listbox1.ItemIndex>=3) and (listbox1.itemindex<=7) then
  begin
    currentspeed:=listbox1.ItemIndex-2;
    case currentspeed of
      1:
      begin
        edtSHSpeed.text:=format('%.2f',[newspeedhackspeed1.speed]);
      end;

      2:
      begin
        edtSHSpeed.text:=format('%.2f',[newspeedhackspeed2.speed]);
      end;

      3:
      begin
        edtSHSpeed.text:=format('%.2f',[newspeedhackspeed3.speed]);
      end;

      4:
      begin
        edtSHSpeed.text:=format('%.2f',[newspeedhackspeed4.speed]);
      end;

      5:
      begin
        edtSHSpeed.text:=format('%.2f',[newspeedhackspeed5.speed]);
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

procedure TFrameHotkeyConfig.updatehotkey;
begin
  edit1.Text:=ConvertKeyComboToString(newhotkeys[listbox1.ItemIndex]);
  updatespeed;  
end;

procedure TframeHotkeyConfig.ListBox1Click(Sender: TObject);
begin
  updatehotkey;
end;

procedure TframeHotkeyConfig.Edit1KeyDown(Sender: TObject; var Key: Word;
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

procedure TframeHotkeyConfig.Button3Click(Sender: TObject);
begin
  zeromemory(@newhotkeys[listbox1.ItemIndex][0],10);
  edit1.Text:=ConvertKeyComboToString(newhotkeys[listbox1.ItemIndex]);
  edit1.SetFocus;
end;

initialization
  {$i frameHotkeyConfigUnit.lrs}

end.
