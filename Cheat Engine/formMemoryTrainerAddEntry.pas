unit formMemoryTrainerAddEntry;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,cefuncproc,symbolhandler;

type TMemoryentry = record
  memrecnr: Integer;  //contains already all the stuff needed
  frozen: boolean;
  frozendirection: integer;
  setvalue: boolean;
  userinput:boolean;
  valuest: string;
end;

type
  TFrmMemoryTrainerAddEntry = class(TForm)
    Button1: TButton;
    Button2: TButton;
    editDescription: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    editHotkey: TEdit;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ListBox1: TListBox;
    Label2: TLabel;
    btnDelete: TButton;
    Button3: TButton;
    Label5: TLabel;
    Listview: TListView;
    Button4: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ListviewClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure editHotkeyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure editHotkeyKeyPress(Sender: TObject; var Key: Char);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    editmode: boolean;
    memorystuff: array of Tmemoryentry;

    hotkey: tkeycombo;
  end;

var
  FrmMemoryTrainerAddEntry: TFrmMemoryTrainerAddEntry;

implementation

uses FrmMemoryTrainerAddEntry2Unit, AdvancedOptionsUnit,
  formMemoryModifier, MainUnit;

{$R *.dfm}

procedure TFrmMemoryTrainerAddEntry.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:=CaFree;
end;

procedure TFrmMemoryTrainerAddEntry.FormCreate(Sender: TObject);
begin
  listbox1.Items:=advancedoptions.codelist.Items;
end;

procedure TFrmMemoryTrainerAddEntry.Button3Click(Sender: TObject);
begin
  FrmMemoryTrainerAddEntry2:=TFrmMemoryTrainerAddEntry2.create(self);
  FrmMemoryTrainerAddEntry2.showmodal;
end;

procedure TFrmMemoryTrainerAddEntry.ListviewClick(Sender: TObject);
begin
  btndelete.enableD:=listview.Itemindex>-1;
end;

procedure TFrmMemoryTrainerAddEntry.btnDeleteClick(Sender: TObject);
var i:integer;
begin
  for i:=listview.ItemIndex to length(memorystuff)-2 do
    memorystuff[i]:=memorystuff[i+1];

  setlength(memorystuff,length(memorystuff)-1);

  listview.DeleteSelected;

  if (listview.ItemIndex=-1) then btnDelete.Enabled:=false;
  if (listview.Items.Count=0) then
    btnDelete.Enabled:=false;
end;

procedure TFrmMemoryTrainerAddEntry.Button1Click(Sender: TObject);
var i,j,k,index: integer;
    keymod: word;
begin
{  keymod:=0;
  if ssctrl in lastshiftstate then keymod:=keymod or MOD_CONTROL;
  if ssAlt in LastShiftState then keymod:=keymod or MOD_ALT;
  if ssShift in LastShiftState then keymod:=keymod or MOD_Shift;

  if keymod=0 then
    raise Exception.create('At least control, alt or shift have to be pressed to be a valid hotkey!');

  if (LastState=VK_SHIFT) or (LastState=VK_CONTROL) or (LastState=VK_MENU) then
    raise Exception.create('One other key has to be pressed!(except ctrl, alt or shift!)');
    }
    
  if editmode then
  begin
    showmessage('nyi');
  end
  else
  begin
    index:=length(frmMemoryModifier.trainerdata);
    setlength(frmMemoryModifier.TrainerData,index+1);

    if listbox1.SelCount>0 then
    begin //code
      setlength(frmMemoryModifier.trainerdata[index].codeentrys,listbox1.SelCount);

      j:=0;
      for i:=0 to listbox1.count-1 do
      begin
        if listbox1.Selected[i] then
        begin
          frmMemoryModifier.trainerdata[index].codeentrys[j].address:=advancedoptions.code[i].Address;
          setlength(frmMemoryModifier.trainerdata[index].codeentrys[j].originalopcode,length(advancedoptions.code[i].actualopcode));
          for k:=0 to length(frmMemoryModifier.trainerdata[index].codeentrys[j].originalopcode)-1 do
            frmMemoryModifier.trainerdata[index].codeentrys[j].originalopcode[k]:=advancedoptions.code[i].actualopcode[k];

          inc(j);
        end;
      end;
    end;


    frmMemoryModifier.trainerdata[index].hasedit:=false;
    if listview.items.count>0 then
    begin //addresses
      setlength(frmMemoryModifier.trainerdata[index].addressentrys,length(memorystuff));

      for i:=0 to length(memorystuff)-1 do
      begin
        if memorystuff[i].userinput then
        begin
          frmMemoryModifier.trainerdata[index].hasedit:=true;
          frmMemoryModifier.trainerdata[index].editvalue:=memorystuff[i].valuest;
        end;

        frmMemoryModifier.trainerdata[index].addressentrys[i].address:=mainform.memrec[memorystuff[i].memrecnr].address;
        frmMemoryModifier.trainerdata[index].addressentrys[i].interpretableaddress:=mainform.memrec[memorystuff[i].memrecnr].interpretableaddress;
        frmMemoryModifier.trainerdata[index].addressentrys[i].ispointer:=mainform.memrec[memorystuff[i].memrecnr].IsPointer;

        setlength(frmMemoryModifier.trainerdata[index].addressentrys[i].pointers,length(mainform.memrec[memorystuff[i].memrecnr].pointers));
        for j:=0 to length(mainform.memrec[memorystuff[i].memrecnr].pointers)-1 do
        begin
          frmMemoryModifier.trainerdata[index].addressentrys[i].pointers[j].Address:=mainform.memrec[memorystuff[i].memrecnr].pointers[j].Address;
          frmMemoryModifier.trainerdata[index].addressentrys[i].pointers[j].Interpretableaddress:=mainform.memrec[memorystuff[i].memrecnr].pointers[j].Interpretableaddress;
          frmMemoryModifier.trainerdata[index].addressentrys[i].pointers[j].offset:=mainform.memrec[memorystuff[i].memrecnr].pointers[j].offset;
        end;

        frmMemoryModifier.trainerdata[index].addressentrys[i].memtyp:=mainform.memrec[memorystuff[i].memrecnr].VarType;
        frmMemoryModifier.trainerdata[index].addressentrys[i].frozen:=memorystuff[i].frozen;
        frmMemoryModifier.trainerdata[index].addressentrys[i].frozendirection:=memorystuff[i].frozendirection;
        frmMemoryModifier.trainerdata[index].addressentrys[i].setvalue:=memorystuff[i].setvalue;
        frmMemoryModifier.trainerdata[index].addressentrys[i].userinput:=memorystuff[i].userinput;


       // frmMemoryModifier.trainerdata[index].addressentrys[i]

        frmMemoryModifier.trainerdata[index].addressentrys[i].value:=memorystuff[i].valuest;
        frmMemoryModifier.trainerdata[index].addressentrys[i].autoassemblescript:=mainform.memrec[memorystuff[i].memrecnr].autoassemblescript;
      end;
    end;

    //all the entrys have been added
    frmMemoryModifier.trainerdata[index].description:=editDescription.Text;
    frmMemoryModifier.trainerdata[index].hotkeytext:=editHotkey.Text;
    frmMemoryModifier.trainerdata[index].hotkey:=hotkey;

    //now add this to the list
    frmMemoryModifier.recordview.Items.Add.caption:=frmMemoryModifier.trainerdata[index].description;
    frmMemoryModifier.recordview.Items[frmMemoryModifier.recordview.Items.count-1].SubItems.add(frmMemoryModifier.trainerdata[index].hotkeytext);
  end;

  modalresult:=mrok;
end;


procedure TFrmMemoryTrainerAddEntry.editHotkeyKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var i: integer;
begin
  if hotkey[4]=0 then
  begin
    for i:=0 to 4 do
      if hotkey[i]=0 then
      begin
        hotkey[i]:=key;
        break;
      end else
      if hotkey[i]=key then break;  //already in list
  end;

  editHotkey.Text:=ConvertKeyComboToString(hotkey);
end;

procedure TFrmMemoryTrainerAddEntry.editHotkeyKeyPress(Sender: TObject;
  var Key: Char);
begin
  key:=chr(0);
end;

procedure TFrmMemoryTrainerAddEntry.Button4Click(Sender: TObject);
begin
  zeromemory(@hotkey[0],10);
  editHotkey.Text:=ConvertKeyComboToString(hotkey);
  edithotkey.SetFocus;
end;

end.
