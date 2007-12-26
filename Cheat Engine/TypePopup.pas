unit TypePopup;  //norm

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TTypeForm = class(TForm)
    Label1: TLabel;
    VarType: TComboBox;
    Button1: TButton;
    Button2: TButton;
    LengthPanel: TPanel;
    Edit1: TEdit;
    lengthlabel: TLabel;
    BitPanel: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    Edit2: TEdit;
    cbunicode: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure VarTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    Procedure UpdateTypeForm;
  public
    { Public declarations }
    nrofrecord: Integer;
  end;

var
  TypeForm: TTypeForm;

implementation

{$R *.DFM}

{$ifdef net}
uses Unit2;
{$else}
uses MainUnit;
{$endif}

Procedure TTypeForm.UpdateTypeForm;
begin
  if Vartype.itemindex=0 then
  begin
    bitpanel.visible:=true;
    lengthpanel.visible:=false;
    lengthlabel.visible:=false;
    label2.Visible:=true;
    edit2.Visible:=true;
    TypeForm.width:=390;
  end
  else
  if vartype.itemindex>=7 then
  begin
    bitpanel.visible:=false;
    lengthpanel.visible:=true;
    lengthlabel.visible:=true;
    typeform.width:=295;

  end else
  begin
    bitpanel.visible:=false;
    lengthpanel.visible:=false;
    lengthlabel.visible:=false;

    TypeForm.width:=182;
  end;
end;

procedure TTypeForm.Button2Click(Sender: TObject);
begin
  modalresult:=mrno;
end;

procedure TTypeForm.Button1Click(Sender: TObject);
var bit,bitl: Byte;
    err: integer;
begin
  err:=0;
  bitl:=0;

  If Vartype.ItemIndex=0 then MainForm.memrec[NrOfRecord].VarType:=5 else //bit
  if Vartype.ItemIndex=1 then Mainform.memrec[NrOfRecord].VarType:=0 else //byte
  if Vartype.ItemIndex=2 then Mainform.memrec[NrOfRecord].VarType:=1 else //word
  if Vartype.ItemIndex=3 then Mainform.memrec[NrOfRecord].VarType:=2 else //dword
  if Vartype.ItemIndex=4 then Mainform.memrec[NrOfRecord].VarType:=6 else //8-bytes
  if Vartype.ItemIndex=5 then Mainform.memrec[NrOfRecord].VarType:=3 else //float
  if Vartype.ItemIndex=6 then Mainform.memrec[NrOfRecord].VarType:=4 else //double
  if Vartype.ItemIndex=7 then Mainform.memrec[NrOfRecord].VarType:=7 else  //text
  if vartype.itemindex=8 then Mainform.memrec[NrOfRecord].VarType:=8;

  if Vartype.ItemIndex<7 then
  begin
    if RadioButton1.checked then bit:=0 else
    if RadioButton2.checked then bit:=1 else
    if RadioButton3.checked then Bit:=2 else
    if RadioButton4.checked then Bit:=3 else
    if RadioButton5.checked then Bit:=4 else
    if RadioButton6.checked then Bit:=5 else
    if RadioButton7.checked then Bit:=6 else
                                 Bit:=7;
  end
  else
    val(edit1.Text,bit,err);

  if vartype.ItemIndex=0 then
    val(edit2.Text,bitl,err);

  MainForm.memrec[NrOfRecord].Bit:=bit;
  mainform.memrec[NrOfRecord].bitlength:=bitl;
  mainform.memrec[nrofrecord].Frozen:=false;

  mainform.memrec[nrofrecord].unicode:=cbunicode.Checked;


  modalresult:=mryes;
end;

procedure TTypeForm.VarTypeChange(Sender: TObject);
begin
  UpdateTypeform;
end;

procedure TTypeForm.FormShow(Sender: TObject);
begin
  UpdateTypeform;
end;

end.
