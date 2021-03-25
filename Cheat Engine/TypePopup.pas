unit TypePopup;

{$MODE Delphi}

  //norm

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, LResources, MemoryRecordUnit, CEFuncProc, CustomTypeHandler, commonTypeDefs, betterControls;

type

  { TTypeForm }

  TTypeForm = class(TForm)
    bitPanel: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    cbHex: TCheckBox;
    cbSigned: TCheckBox;
    cbUnicode: TCheckBox;
    cbCodePage: TCheckBox;
    lengthEdit: TEdit;
    bitLengthEdit: TEdit;
    Label1: TLabel;
    labelBit6: TLabel;
    labelBit7: TLabel;
    labelBitLength: TLabel;
    labelBit0: TLabel;
    labelBit1: TLabel;
    labelBit2: TLabel;
    labelBit3: TLabel;
    labelBit4: TLabel;
    labelBit5: TLabel;
    labelLength: TLabel;
    lengthPanel: TPanel;
    HexAndSignedPanel: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    bitRadioButton0: TRadioButton;
    bitRadioButton1: TRadioButton;
    bitRadioButton2: TRadioButton;
    bitRadioButton3: TRadioButton;
    bitRadioButton4: TRadioButton;
    bitRadioButton5: TRadioButton;
    bitRadioButton6: TRadioButton;
    bitRadioButton7: TRadioButton;
    VarType: TComboBox;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure cbCodePageChange(Sender: TObject);
    procedure cbUnicodeChange(Sender: TObject);
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
    procedure RefreshFieldsByMemoryRecord(memrec: TMemoryRecord);
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


procedure TTypeForm.RefreshFieldsByMemoryRecord(memrec: TMemoryRecord);
begin

  case memrec.vartype of
    vtCustom:  VarType.itemindex:=VarType.Items.IndexOf(memrec.CustomTypeName);

    vtBinary:
    begin
      TypeForm.VarType.itemindex:=0;
      TypeForm.bitLengthEdit.text:=IntToStr(memrec.extra.bitData.bitlength);

      case memrec.extra.bitData.Bit of
        0     :       bitRadioButton0.checked:=true;
        1     :       bitRadioButton1.checked:=true;
        2     :       bitRadioButton2.checked:=true;
        3     :       bitRadioButton3.checked:=true;
        4     :       bitRadioButton4.checked:=true;
        5     :       bitRadioButton5.checked:=true;
        6     :       bitRadioButton6.checked:=true;
        7     :       bitRadioButton7.checked:=true;
      end;
    end;

    vtByte:   VarType.itemindex:=1;
    vtWord:   VarType.itemindex:=2;
    vtDword:  VarType.itemindex:=3;
    vtQword:  VarType.itemindex:=4;
    vtSingle: VarType.itemindex:=5;
    vtDouble: VarType.itemindex:=6;

    vtString:
    begin
      VarType.itemindex:=7;
      lengthEdit.text:=inttostr(memrec.Extra.stringData.length);
      cbUnicode.checked:=memrec.Extra.stringData.unicode;
      cbCodePage.checked:=memrec.Extra.stringData.codepage;
    end;

    vtByteArray:
    begin
      lengthEdit.text:=inttostr(memrec.Extra.byteData.bytelength);
      VarType.itemindex:=8;
      cbUnicode.visible:=false;
      cbCodePage.visible:=false;
    end;
  end;

  if (memrec.vartype = vtByte)
     or (memrec.vartype = vtWord)
     or (memrec.vartype = vtDword)
     or (memrec.vartype = vtQword)
     or (memrec.vartype = vtSingle)
     or (memrec.vartype = vtDouble)
     or (memrec.vartype = vtByteArray)
     then
  begin
    cbHex.checked:=memrec.ShowAsHex;
    cbSigned.checked:=memrec.ShowAsSigned;
  end
  else
  begin
    cbHex.checked:=false;
    cbSigned.checked:=false;
  end;

end;

Procedure TTypeForm.UpdateTypeForm;
begin

  cbHex.Enabled:=false;
  cbSigned.Enabled:=false;

  // Binary
  if Vartype.itemindex=0 then
  begin
    bitPanel.visible:=true;
    lengthPanel.visible:=false;
    labelLength.visible:=false;
    labelBitLength.Visible:=true;
    bitLengthEdit.Visible:=true;
    clientwidth:=bitPanel.left+bitPanel.Width+vartype.left;
  end
  // String ByteArray
  else if vartype.itemindex in [7,8] then
  begin
    bitPanel.visible:=false;
    lengthPanel.visible:=true;
    labelLength.visible:=true;
    clientwidth:=lengthPanel.left+lengthPanel.Width;
    cbUnicode.visible:=vartype.itemindex=7;
    cbCodePage.visible:=cbUnicode.Visible;
    cbHex.enabled:=vartype.itemindex=8;
  end
  // Other Types
  else
  begin
    bitPanel.visible:=false;
    lengthPanel.visible:=false;
    labelLength.visible:=false;

    TypeForm.width:=vartype.Left+vartype.Width+vartype.left;

    cbHex.Enabled:=true;
    cbSigned.Enabled:=true;

  end;
end;

procedure TTypeForm.ButtonCancelClick(Sender: TObject);
begin
  modalresult:=mrno;
end;

procedure TTypeForm.ButtonOKClick(Sender: TObject);
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
    if bitRadioButton0.checked then bit:=0 else
    if bitRadioButton1.checked then bit:=1 else
    if bitRadioButton2.checked then Bit:=2 else
    if bitRadioButton3.checked then Bit:=3 else
    if bitRadioButton4.checked then Bit:=4 else
    if bitRadioButton5.checked then Bit:=5 else
    if bitRadioButton6.checked then Bit:=6 else
                                 Bit:=7;
  end
  else
    val(lengthEdit.Text,bit,err);

  if vartype.ItemIndex=0 then
    val(bitLengthEdit.Text,bitl,err);

  if err>0 then
    raise exception.create(rsInvalidLength);

  if (memoryrecord.vartype<>vtString)
     and (memoryrecord.vartype<>vtBinary)
     then
  begin
    MemoryRecord.ShowAsHex:=cbHex.Checked;
    MemoryRecord.ShowAsSigned:=cbSigned.Checked;
  end
  else
  begin
    MemoryRecord.ShowAsHex:=false;
    MemoryRecord.ShowAsSigned:=false;
  end;

  if memoryrecord.vartype=vtBinary then
  begin
    MemoryRecord.Extra.bitData.Bit:=bit;
    MemoryRecord.Extra.bitData.bitlength:=bitl;
  end;

  if memoryrecord.vartype=vtString then
  begin
    val(lengthEdit.Text,MemoryRecord.Extra.stringData.length,err);

    MemoryRecord.Extra.stringData.unicode:=cbUnicode.checked;
    MemoryRecord.Extra.stringData.codepage:=cbCodePage.checked;
  end;

  if memoryrecord.vartype=vtByteArray then
  begin
    val(lengthEdit.Text,MemoryRecord.Extra.byteData.bytelength,err);

    if wasNotAOB then //it wasn't an aob before, set the hexadecimal value
      MemoryRecord.showAsHex:=true;
  end;

  modalresult:=mryes;
end;

procedure TTypeForm.cbCodePageChange(Sender: TObject);
begin
  if cbCodePage.checked then cbUnicode.checked:=false;
end;

procedure TTypeForm.cbUnicodeChange(Sender: TObject);
begin
  if cbUnicode.checked then cbCodePage.checked:=false;
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
  lengthEdit.Constraints.MinWidth:=labellength.Width;
  bitLengthEdit.Width:=lengthEdit.Width;
  UpdateTypeform;
end;

initialization
  {$i TypePopup.lrs}

end.
