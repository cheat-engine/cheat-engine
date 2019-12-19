unit formAddToCodeList;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources, ExtCtrls, cefuncproc, newkernelhandler;

type

  { TfrmAddToCodeList }

  TfrmAddToCodeList = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Edit2: TEdit;
    Label2: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    fromaddress,toaddress: ptrUint;
    addtocodelist: boolean;
  end;

var
  frmAddToCodeList: TfrmAddToCodeList;

implementation


uses AdvancedOptionsUnit, Parsers;

resourcestring
  rsRegionToAdd = 'Region to add';
  rsRegion = 'Region';
  rsPleaseFillInAValidFromAddress = 'Please fill in a valid ''From'' address';
  rsPleaseFillInAValidToAddress = 'Please fill in a valid ''To'' address';

procedure TfrmAddToCodeList.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if addtocodelist then action:=cafree;
end;

procedure TfrmAddToCodeList.FormCreate(Sender: TObject);
begin

end;

procedure TfrmAddToCodeList.FormShow(Sender: TObject);
begin
  //fill in the edit fields
  edit1.Text:=IntToHex(fromaddress,8);
  edit2.Text:=IntToHex(toaddress,8);

  if addtocodelist then caption:=rsRegionToAdd else caption:=rsRegion;
end;

procedure TfrmAddToCodeList.Button1Click(Sender: TObject);
begin
  try
    fromaddress:=StrToQWordEx('$'+edit1.Text);
  except
    raise exception.Create(rsPleaseFillInAValidFromAddress);
  end;

  try
    toaddress:=StrToQWordEx('$'+edit2.Text);
  except
    raise exception.Create(rsPleaseFillInAValidToAddress);
  end;

  if addtocodelist then
    advancedoptions.AddToCodeList(fromaddress,1+toaddress-fromaddress,false);

  modalresult:=mrOk;
end;

initialization
  {$i formAddToCodeList.lrs}

end.
