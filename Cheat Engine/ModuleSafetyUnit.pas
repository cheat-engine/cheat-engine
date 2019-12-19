unit ModuleSafetyUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, LResources;

type

  { TfrmModuleSafety }

  TfrmModuleSafety = class(TForm)
    msImageList: TImageList;
    ListBox1: TListBox;
    Edit1: TEdit;
    Button1: TButton;
    rbAllowList: TRadioButton;
    rbDenyList: TRadioButton;
    cbGlobalDeny: TCheckBox;
    PopupMenu1: TPopupMenu;
    Remove1: TMenuItem;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure rbAllowListClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmModuleSafety: TfrmModuleSafety;

implementation


procedure TfrmModuleSafety.Button1Click(Sender: TObject);
begin
  listbox1.items.Add(uppercase(edit1.Text));
end;

procedure TfrmModuleSafety.Remove1Click(Sender: TObject);
var i: integer;
begin

  if listbox1.selcount>0 then
  begin
    i:=0;
    while i<listbox1.Count do
    begin
      if listbox1.Selected[i] then
        listbox1.Items.Delete(i)
      else
        inc(i);
    end;
  end;
end;

procedure TfrmModuleSafety.rbAllowListClick(Sender: TObject);
begin
  cbglobaldeny.enabled:=rbdenylist.Checked;
end;

initialization
  {$i ModuleSafetyUnit.lrs}

end.
