unit frameHotkeyConfigUnit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources, Menus, Buttons, CEFuncProc,
  commonTypeDefs;

const cehotkeycount=32;

type

  { TframeHotkeyConfig }

  TframeHotkeyConfig = class(TFrame)
    cbStopOnRelease: TCheckBox;
    fhcImageList: TImageList;
    MenuItem1: TMenuItem;
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
    PopupMenu1: TPopupMenu;
    procedure Edit1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edtSHSpeedChange(Sender: TObject);
    procedure edtSHSpeedExit(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button3Click(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure MenuItem1Click(Sender: TObject);
  private
    { Private declarations }
    currentspeed: integer;
    increasespeed: boolean;
    procedure updatehotkey;
    procedure updatespeed;
  public
    { Public declarations }
    newhotkeys: array [0..cehotkeycount-1] of tkeycombo;
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
      newspeedhackspeed1.disablewhenreleased:=cbStopOnRelease.checked;
      newspeedhackspeed1.keycombo:=newhotkeys[currentspeed+3];
    end else
    if currentspeed=2 then
    begin
      newspeedhackspeed2.speed:=StrToFloat(edtSHspeed.Text);
      newspeedhackspeed2.disablewhenreleased:=cbStopOnRelease.checked;
      newspeedhackspeed2.keycombo:=newhotkeys[currentspeed+3];
    end else
    if currentspeed=3 then
    begin
      newspeedhackspeed3.speed:=StrToFloat(edtSHspeed.Text);
      newspeedhackspeed3.disablewhenreleased:=cbStopOnRelease.checked;
      newspeedhackspeed3.keycombo:=newhotkeys[currentspeed+3];
    end else
    if currentspeed=4 then
    begin
      newspeedhackspeed4.speed:=StrToFloat(edtSHspeed.Text);
      newspeedhackspeed4.disablewhenreleased:=cbStopOnRelease.checked;
      newspeedhackspeed4.keycombo:=newhotkeys[currentspeed+3];
    end else
    if currentspeed=5 then
    begin
      newspeedhackspeed5.speed:=StrToFloat(edtSHspeed.Text);
      newspeedhackspeed5.disablewhenreleased:=cbStopOnRelease.checked;
      newspeedhackspeed5.keycombo:=newhotkeys[currentspeed+3];
    end;
  end;

  if (listbox1.ItemIndex>=4) and (listbox1.itemindex<=8) then
  begin
    currentspeed:=listbox1.ItemIndex-3;
    case currentspeed of
      1:
      begin
        edtSHSpeed.text:=format('%.3f',[newspeedhackspeed1.speed]);
        cbStopOnRelease.checked:=newspeedhackspeed1.disablewhenreleased;
      end;

      2:
      begin
        edtSHSpeed.text:=format('%.3f',[newspeedhackspeed2.speed]);
        cbStopOnRelease.checked:=newspeedhackspeed2.disablewhenreleased;
      end;

      3:
      begin
        edtSHSpeed.text:=format('%.3f',[newspeedhackspeed3.speed]);
        cbStopOnRelease.checked:=newspeedhackspeed3.disablewhenreleased;
      end;

      4:
      begin
        edtSHSpeed.text:=format('%.3f',[newspeedhackspeed4.speed]);
        cbStopOnRelease.checked:=newspeedhackspeed4.disablewhenreleased;
      end;

      5:
      begin
        edtSHSpeed.text:=format('%.3f',[newspeedhackspeed5.speed]);
        cbStopOnRelease.checked:=newspeedhackspeed5.disablewhenreleased;
      end;
    end;


    panel3.Visible:=true;
    panel4.Visible:=false;
  end else panel3.Visible:=false;

  if (listbox1.ItemIndex=9) or (listbox1.ItemIndex=10) then
  begin
    increasespeed:=listbox1.itemindex=9;
    if increasespeed then
      edit4.Text:=format('%.3f',[speedupdelta])
    else
      edit4.Text:=format('%.3f',[slowdowndelta]);

    panel4.visible:=true;
    panel3.Visible:=false;
  end else panel4.Visible:=false;

end;

procedure TFrameHotkeyConfig.updatehotkey;
begin
  if (listbox1.ItemIndex>=0) and (listbox1.ItemIndex<listbox1.Items.Count) then
    edit1.Text:=ConvertKeyComboToString(newhotkeys[listbox1.ItemIndex]);

  updatespeed;

end;

procedure TframeHotkeyConfig.ListBox1Click(Sender: TObject);
begin
  updatehotkey;
end;

procedure TframeHotkeyConfig.edtSHSpeedChange(Sender: TObject);
begin

end;

procedure TframeHotkeyConfig.edtSHSpeedExit(Sender: TObject);
begin
  try
    updatespeed;
  except
  end;
end;

procedure TframeHotkeyConfig.Edit1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var key: word;
begin
  key:=0;
  case button of
    mbMiddle: key:=VK_MBUTTON;
    mbExtra1: key:=VK_XBUTTON1;
    mbExtra2: key:=VK_XBUTTON2;
  end;

  if key<>0 then
    Edit1KeyDown(edit1, key, shift);
end;

procedure TframeHotkeyConfig.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if listbox1.ItemIndex<>-1 then
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
end;

procedure TframeHotkeyConfig.Button3Click(Sender: TObject);
begin
  if listbox1.ItemIndex<>-1 then
  begin
    zeromemory(@newhotkeys[listbox1.ItemIndex][0],10);
    edit1.Text:=ConvertKeyComboToString(newhotkeys[listbox1.ItemIndex]);
    edit1.SetFocus;
  end;
end;

procedure TframeHotkeyConfig.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  if user then
    updatehotkey;
end;

procedure TframeHotkeyConfig.MenuItem1Click(Sender: TObject);
var i: integer;
begin
  for i:=0 to cehotkeycount-1 do
    newhotkeys[i][0]:=0;

  updatehotkey;
end;

initialization
  {$i frameHotkeyConfigUnit.lrs}

end.
