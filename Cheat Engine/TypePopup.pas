unit TypePopup;

{$MODE Delphi}

  //norm

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, LResources, MemoryRecordUnit, cefuncproc, customtypehandler, commonTypeDefs;

type

  { TTypeForm }

  TTypeForm = class(TForm)
    BitPanel: TPanel;
    Button1: TButton;
    Button2: TButton;
    cbunicode: TCheckBox;
    cbCodePage: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lengthlabel: TLabel;
    LengthPanel: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    VarType: TComboBox;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbCodePageChange(Sender: TObject);
    procedure cbunicodeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
uses MainUnit, vartypestrings;
{$endif}

resourcestring
  rsInvalidLength = 'Invalid length';

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
  if vartype.itemindex in [7,8] then
  begin
    bitpanel.visible:=false;
    lengthpanel.visible:=true;
    lengthlabel.visible:=true;
    clientwidth:=lengthpanel.left+lengthpanel.Width;
    cbunicode.visible:=vartype.itemindex=7;
    cbCodePage.visible:=cbunicode.Visible;
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
    ct: TCustomType;
    wasNotAOB: boolean;
begin
  err:=0;
  bitl:=0;

  wasNotAOB:=MemoryRecord.Vartype<>vtByteArray;



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
    else
    begin
      if Vartype.ItemIndex<>-1 then
      begin
        ct:=TCustomType(vartype.Items.Objects[Vartype.ItemIndex]);
        if ct<>nil then
        begin
          MemoryRecord.VarType:=vtCustom;
          MemoryRecord.CustomTypeName:=ct.name;
        end;
      end;
    end;
  end;

  err:=0;
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

  if err>0 then
    raise exception.create(rsInvalidLength);



  if memoryrecord.vartype=vtBinary then
  begin
    MemoryRecord.Extra.bitData.Bit:=bit;
    MemoryRecord.Extra.bitData.bitlength:=bitl;
  end;

  if memoryrecord.vartype=vtString then
  begin
    val(edit1.Text,MemoryRecord.Extra.stringData.length,err);

    MemoryRecord.Extra.stringData.unicode:=cbunicode.checked;
    MemoryRecord.Extra.stringData.codepage:=cbCodePage.checked;
  end;

  if memoryrecord.vartype=vtByteArray then
  begin
    val(edit1.Text,MemoryRecord.Extra.byteData.bytelength,err);

    if wasNotAOB then //it wasn't an aob before, set the hexadecimal value
      MemoryRecord.showAsHex:=true;
  end;

  modalresult:=mryes;
end;

procedure TTypeForm.cbCodePageChange(Sender: TObject);
begin
  if cbCodePage.checked then cbunicode.checked:=false;
end;

procedure TTypeForm.cbunicodeChange(Sender: TObject);
begin
  if cbunicode.checked then cbCodePage.checked:=false;
end;

procedure TTypeForm.FormCreate(Sender: TObject);
begin
  VarType.Items.Clear;
  vartype.items.add(rs_vtBinary);
  vartype.items.add(rs_vtByte);
  vartype.items.add(rs_vtWord);
  vartype.items.add(rs_vtDword);
  vartype.items.add(rs_vtQword);
  vartype.items.add(rs_vtSingle);
  vartype.items.add(rs_vtDouble);
  vartype.items.add(rs_vtString);
  vartype.items.add(rs_vtByteArray);
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
