unit TypePopup;

{$MODE Delphi}

  //norm

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, LResources, MemoryRecordUnit, cefuncproc, customtypehandler;

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
    MemoryRecord: TMemoryRecord;
    procedure RefreshCustomTypes;
  end;

var
  TypeForm: TTypeForm;

implementation


{$ifdef net}
uses Unit2;
{$else}
uses MainUnit;
{$endif}

Procedure TTypeForm.RefreshCustomTypes;
var old:  TNotifyEvent;
    i: integer;

    oldtype: string;
begin
  old:=VarType.OnChange;
  VarType.OnChange:=nil;

  oldtype:=vartype.text;

  i:=0;
  //first clear all custom types
  while i<vartype.Items.Count do
  begin
    if vartype.Items.Objects[i]<>nil then
      vartype.Items.Delete(i)
    else
      inc(i);
  end;

  //now add the custom types back
  for i:=0 to customtypes.Count-1 do
    vartype.Items.AddObject(TcustomType(customtypes[i]).name,customtypes[i]);



  //set the selected index back if possible
  i:=vartype.Items.IndexOf(oldtype);
  if i<>-1 then
    vartype.ItemIndex:=i
  else
    vartype.itemindex:=3; //4 byte

  VarType.OnChange:=old;
end;


Procedure TTypeForm.UpdateTypeForm;
begin
  if Vartype.itemindex=0 then
  begin
    bitpanel.visible:=true;
    lengthpanel.visible:=false;
    lengthlabel.visible:=false;
    label2.Visible:=true;
    edit2.Visible:=true;
    clientwidth:=bitpanel.left+bitpanel.Width+vartype.left;
  end
  else
  if vartype.itemindex>=7 then
  begin
    bitpanel.visible:=false;
    lengthpanel.visible:=true;
    lengthlabel.visible:=true;
    clientwidth:=lengthpanel.left+lengthpanel.Width;

  end else
  begin
    bitpanel.visible:=false;
    lengthpanel.visible:=false;
    lengthlabel.visible:=false;

    TypeForm.width:=vartype.Left+vartype.Width+vartype.left;
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

  case Vartype.ItemIndex of
    0: MemoryRecord.VarType:=vtBinary;
    1: MemoryRecord.Vartype:=vtByte;
    2: MemoryRecord.Vartype:=vtWord;
    3: MemoryRecord.Vartype:=vtDword;
    4: MemoryRecord.Vartype:=vtQword;
    5: MemoryRecord.Vartype:=vtSingle;
    6: MemoryRecord.VarType:=vtDouble;
    7: MemoryRecord.Vartype:=vtString;
    8: MemoryRecord.VarType:=vtByteArray;
  end;

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


  if memoryrecord.vartype=vtBinary then
  begin
    MemoryRecord.Extra.bitData.Bit:=bit;
    MemoryRecord.Extra.bitData.bitlength:=bitl;
  end;

  if memoryrecord.vartype=vtString then
    MemoryRecord.Extra.stringData.length:=bit;

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

initialization
  {$i TypePopup.lrs}

end.
