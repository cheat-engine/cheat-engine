unit formAddToCodeList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmAddToCodeList = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Edit2: TEdit;
    Label2: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    fromaddress,toaddress: dword;
    addtocodelist: boolean;
  end;

var
  frmAddToCodeList: TfrmAddToCodeList;

implementation

{$R *.dfm}

uses advancedoptionsunit;

procedure TfrmAddToCodeList.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if addtocodelist then action:=cafree;
end;

procedure TfrmAddToCodeList.FormShow(Sender: TObject);
begin
  //fill in the edit fields
  edit1.Text:=IntToHex(fromaddress,8);
  edit2.Text:=IntToHex(toaddress,8);

  if addtocodelist then caption:='Region to add' else caption:='Region';
end;

procedure TfrmAddToCodeList.Button1Click(Sender: TObject);
begin
  try
    fromaddress:=strtoint('$'+edit1.Text);
  except
    raise exception.Create('Please fill in a valid ''From'' address');
  end;

  try
    toaddress:=strToInt('$'+edit2.Text);
  except
    raise exception.Create('Please fill in a valid ''To'' address');
  end;

  if addtocodelist then
    advancedoptions.AddToCodeList(fromaddress,1+toaddress-fromaddress,false);
  modalresult:=mrOk;
end;

end.
