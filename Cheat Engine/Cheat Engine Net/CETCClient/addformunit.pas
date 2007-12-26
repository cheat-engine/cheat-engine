unit addformunit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TAddform = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ValuePanel: TPanel;
    Label12: TLabel;
    Edit1: TEdit;
    VarType: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Description: TEdit;
    BitPanel: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    NewAddress: TEdit;
    Edit2: TEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VarTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Addform: TAddform;

implementation

uses CEClient, Unit2;

{$R *.dfm}

procedure TAddform.FormClose(Sender: TObject; var Action: TCloseAction);
begin                                                 
  action:=cafree;
  addform:=nil;
end;

procedure TAddform.Button1Click(Sender: TObject);
var error: integer;
    address: dword;
    bit: byte;
    nrofbits: byte;
    vartype2: byte;
    i,j: integer;
begin
  val('$'+NewAddress.text,address,error);

  if RadioButton1.checked then bit:=0 else
  if RadioButton2.checked then bit:=1 else
  if RadioButton3.checked then Bit:=2 else
  if RadioButton4.checked then Bit:=3 else
  if RadioButton5.checked then Bit:=4 else
  if RadioButton6.checked then Bit:=5 else
  if RadioButton7.checked then Bit:=6 else
                               Bit:=7;

  try
    nrofbits:=StrToInt(edit2.Text);
  except
    raise exception.Create(edit2.Text+' is not a valid number');
  end;

  if valuepanel.visible then
  begin
    try
      bit:=strtoint(edit1.Text);
    except
      raise exception.Create(edit1.text+' is not a valid value!');
    end;
  end;

  case vartype.Itemindex of
    0  :  VarType2:=5;
    1  :  VarType2:=0;
    2  :  VarType2:=1;
    3  :  VarType2:=2;
    4  :  VarType2:=6;
    5  :  VarType2:=3;
    6  :  VarType2:=4;
    7  :  Vartype2:=7;
    8  :  VarType2:=8;
    else vartype2:=0;
  end;

  i:=0;
  for j:=0 to mainform.NumberOfRecords-1 do
  begin
    if (mainform.memrec[j].Address=address) then
    begin
      if vartype2<>mainform.memrec[j].vartype then inc(i)
      else
      begin
        if bit<>mainform.memrec[j].Bit then inc(i);
      end;
    end else inc(i);
  end;

  if i=MainForm.NumberOfRecords then
  begin
    inc(MainForm.NumberOfRecords);

    mainform.reserveMem;

    if description.text='' then
    mainform.memrec[MainForm.NumberOfRecords-1].Description:='No description!' else
    mainform.memrec[MainForm.NumberOfRecords-1].Description:=description.text;
    mainform.memrec[MainForm.NumberOfRecords-1].Address:=address;

 // mainform.memrec[MainForm.NumberOfRecords-1].VarType:=Vartype.itemindex;
  //
    mainform.memrec[MainForm.NumberOfRecords-1].VarType:=vartype2;

    mainform.memrec[MainForm.NumberOfRecords-1].Bit:=bit;
    mainform.memrec[MainForm.NumberOfRecords-1].bitlength:=nrofbits;


    mainform.memrec[MainForm.NumberOfRecords-1].Frozen:=false;
    mainform.Hotkeystrings[Mainform.NumberOfRecords-1]:='';
    mainform.Hotkeys[MainForm.NumberOfRecords-1]:=-1;

    output[0]:=3; //add address (address:dword ,valuetype:byte ,bitnr:byte,length:byte )


    pdword(@output[1])^:=address;
    case vartype2 of
      0: output[5]:=1;
      1: output[5]:=2;
      2: output[5]:=4;
      3: output[5]:=4;
      4: output[5]:=8;
      5: output[5]:=nrofbits div 8+1;
      6: output[5]:=8;
      7: output[5]:=bit;
      8: output[5]:=bit;
    end;

    sendbuf(6);

    MainForm.UpdateScreen;
    MainForm.updatelist;
    
    modalresult:=mrok;
  end else showmessage('This address is already in the list!');
end;

procedure TAddform.FormCreate(Sender: TObject);
begin
  vartype.ItemIndex:=3;
end;

procedure TAddform.VarTypeChange(Sender: TObject);
begin
  bitpanel.Visible:=Vartype.itemindex=0;
  if vartype.text='Text' then
  begin
    valuepanel.visible:=true;
    valuepanel.BringToFront;
    label12.Caption:='Nr. of Characters';
  end
  else if vartype.text='Array of Byte' then
  begin
    valuepanel.visible:=true;
    label12.Caption:='Nr. of Bytes';
  end
  else valuepanel.visible:=false;
end;

procedure TAddform.FormShow(Sender: TObject);
begin
  NewAddress.SetFocus;
  NewAddress.SelectAll;
  description.Text:='No description!';
end;

end.
