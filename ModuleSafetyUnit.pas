unit ModuleSafetyUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls;

type
  TfrmModuleSafety = class(TForm)
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

{$R *.dfm}

procedure TfrmModuleSafety.Button1Click(Sender: TObject);
begin
  listbox1.items.Add(uppercase(edit1.Text));
end;

procedure TfrmModuleSafety.Remove1Click(Sender: TObject);
begin
  if listbox1.itemindex<>-1 then
    listbox1.DeleteSelected;
end;

procedure TfrmModuleSafety.rbAllowListClick(Sender: TObject);
begin
  cbglobaldeny.enabled:=rbdenylist.Checked;
end;

end.
