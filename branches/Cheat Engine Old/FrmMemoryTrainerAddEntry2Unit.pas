unit FrmMemoryTrainerAddEntry2Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFrmMemoryTrainerAddEntry2 = class(TForm)
    Recordlist: TListBox;
    RadioButton1: TRadioButton;
    Label1: TLabel;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    EditValue: TEdit;
    Label2: TLabel;
    btnAdd: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    FreezePanel: TPanel;
    NormalFreeze: TRadioButton;
    DecreaseFreeze: TRadioButton;
    IncreaseFreeze: TRadioButton;
    procedure RadioButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure RecordlistClick(Sender: TObject);
    procedure RecordlistDblClick(Sender: TObject);
  private
    { Private declarations }
    cb2enabled: boolean;
  public
    { Public declarations }
  end;

var
  FrmMemoryTrainerAddEntry2: TFrmMemoryTrainerAddEntry2;

implementation

uses MainUnit, formMemoryTrainerAddEntry;

{$R *.dfm}

procedure TFrmMemoryTrainerAddEntry2.RadioButtonClick(Sender: TObject);
begin
  if (sender as TRadiobutton).tag>2 then
  begin
    label2.Enabled:=false;
    EditValue.Enabled:=false;
    checkbox1.enabled:=false;
    checkbox1.checked:=false;
  end
  else
  begin
    label2.enableD:=true;
    EditValue.enabled:=true;
    checkbox1.enabled:=cb2enabled;
  end;

  if (sender as TRadiobutton).tag in [1,3] then
  begin
    //enable
    normalfreeze.Enabled:=true;
    DecreaseFreeze.Enabled:=true;
    IncreaseFreeze.Enabled:=true;
  end
  else
  begin
    //disable
    normalfreeze.Enabled:=false;
    DecreaseFreeze.Enabled:=false;
    IncreaseFreeze.Enabled:=false;
  end;
end;

procedure TFrmMemoryTrainerAddEntry2.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TFrmMemoryTrainerAddEntry2.FormCreate(Sender: TObject);
var i: integer;
begin
  for i:=0 to mainform.NumberOfRecords-1 do
    recordlist.Items.Add(mainform.memrec[i].Description);

  //search other already existing memorystuff entrys
  cb2enabled:=true;
  with frmMemoryTrainerAddEntry do
    for i:=0 to length(memorystuff)-1 do
      if memorystuff[i].userinput then cb2enabled:=false;

  checkbox1.Enabled:=cb2enabled;
end;

procedure TFrmMemoryTrainerAddEntry2.btnAddClick(Sender: TObject);
var testvalue: int64;
    testvalue2: double;
    i,error: integer;
begin

  with frmMemoryTrainerAddEntry do
  begin
    setlength(memorystuff,length(memorystuff)+1);
    memorystuff[length(memorystuff)-1].memrecnr:=recordlist.itemindex;
    memorystuff[length(memorystuff)-1].frozen:=(radiobutton1.checked or radiobutton3.checked);
    memorystuff[length(memorystuff)-1].setvalue:=(radiobutton1.checked or radiobutton2.checked);
    memorystuff[length(memorystuff)-1].userinput:=checkbox1.Checked;

    if normalfreeze.checked then frmMemoryTrainerAddEntry.memorystuff[length(memorystuff)-1].frozendirection:=0;
    if decreasefreeze.checked then frmMemoryTrainerAddEntry.memorystuff[length(memorystuff)-1].frozendirection:=1;
    if increasefreeze.checked then frmMemoryTrainerAddEntry.memorystuff[length(memorystuff)-1].frozendirection:=2;

    //check if the value is a valid value and then copy the STRING to the value entry
    if editvalue.enabled then
    begin

      if length(editvalue.text)=0 then
      begin
        setlength(memorystuff,length(memorystuff)-1);   //undo add
        raise exception.Create('You havn''t filled in a value in the value field.');
      end;

      case mainform.memrec[recordlist.ItemIndex].VarType of
      0,1,2,6:   begin
                    val(editvalue.text,testvalue,error);
                    if error<>0 then
                    begin
                      setlength(memorystuff,length(memorystuff)-1);
                      raise exception.Create('The value you entered is not a valid notation for the record you selected');
                    end;
                 end;

      5:         begin
                   for i:=1 to length(editvalue.text) do
                     if not (editvalue.Text[i] in ['0','1','?','*','x']) then
                     begin
                       setlength(memorystuff,length(memorystuff)-1);
                       raise exception.create('This value has to be in binary notation(0,1,?,x,*)');
                     end;
                 end;

      3,4:       begin
                    val(editvalue.text,testvalue2,error);
                    if error<>0 then
                    begin
                      setlength(memorystuff,length(memorystuff)-1);
                      raise exception.Create('The value you entered is not a valid notation for the record you selected');
                    end;
                  end;
     //no check for 7

      8:          begin
                    for i:=1 to length(editvalue.text) do
                    case editvalue.text[i] of
                      '0'..'9' : ;
                      'a'..'f' : ;
                      'A'..'F' : ;
                      ' ','-' : ;
                      else
                      begin
                        setlength(memorystuff,length(memorystuff)-1);
                        raise exception.create('The value you entered is not a valid notation for the record you selected');
                      end;
                    end;
                  end;
      end;

      //if we still havnt crashed after this then:
      memorystuff[length(memorystuff)-1].valuest:=editvalue.text;

    end;




    listview.Items.Add.Caption:=recordlist.Items[recordlist.itemindex];
    if frmMemoryTrainerAddEntry.memorystuff[length(memorystuff)-1].frozen then
    begin
      case frmMemoryTrainerAddEntry.memorystuff[length(memorystuff)-1].frozendirection of
        0: listview.Items[listview.Items.Count-1].SubItems.add('x');
        1: listview.Items[listview.Items.Count-1].SubItems.add('x-');
        2: listview.Items[listview.Items.Count-1].SubItems.add('x+');
      end;


    end else listview.Items[listview.Items.Count-1].SubItems.add('');

    if editvalue.enabled then
      listview.Items[listview.Items.Count-1].SubItems.add(memorystuff[length(memorystuff)-1].valuest) else
      listview.Items[listview.Items.Count-1].SubItems.add('');

    if checkbox1.Checked then
      listview.Items[listview.Items.Count-1].SubItems.add('x') else
      listview.Items[listview.Items.Count-1].SubItems.add('');
  end;



  modalresult:=mrok;
end;

procedure TFrmMemoryTrainerAddEntry2.RecordlistClick(Sender: TObject);
begin
  btnAdd.enabled:=recordlist.itemindex>-1;

  if btnadd.enabled then
  begin
    if mainform.memrec[recordlist.itemindex].VarType=255 then
    begin
      radiobutton1.Enabled:=false;
      radiobutton2.Enabled:=false;
      radiobutton3.Checked:=true;
      radiobutton3.OnClick(radiobutton3);
      normalfreeze.Checked:=true;
      increasefreeze.Enabled:=false;
      decreasefreeze.Enabled:=false;
    end
    else
    begin
      radiobutton1.Enabled:=true;
      radiobutton2.Enabled:=true;

      if radiobutton1.checked then
        radiobutton1.OnClick(radiobutton1);

      if radiobutton2.checked then
        radiobutton2.OnClick(radiobutton2);

      if radiobutton3.checked then
        radiobutton3.OnClick(radiobutton3);


    end;
  end;
end;

procedure TFrmMemoryTrainerAddEntry2.RecordlistDblClick(Sender: TObject);
begin
  btnAdd.Click;
end;

end.

