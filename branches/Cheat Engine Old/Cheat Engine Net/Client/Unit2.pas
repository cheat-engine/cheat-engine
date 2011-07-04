unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,math,
  StdCtrls,ceclient, ComCtrls, ExtCtrls, Menus, Buttons,clipbrd, XPMan,registry,debugger,cefuncproc,netapis;

resourcestring
  CEnorm = 'Cheat Engine 5.2 client';
  CERegion = 'Cheat Engine 5.2 client - Please wait!';
  CESearch = 'Cheat Engine 5.2 client - Please wait!';
  CERegionSearch = 'Cheat Engine 5.2 client - Please wait!';
  CEWait= 'Cheat Engine 5.2 client - Please wait!';

  strNoDescription='No description';
  strClose='Close';
  strStop='Stop';
  strOk='OK';
//scanoptions
const
  SO_FASTSCAN=$1;
  SO_HEXADECIMAL=$2;
  SO_READONLY=$4;
  SO_FINDONLYONE=$8;
  SO_ISBINARY=$10;
  SO_UNICODE=$20;


type grouptype = array[1..4] of boolean;

type TSkin = record
    Marker:             String[6];
    backgroundimage:    Boolean;
    ProcessTextColor:   TColor;
    Normaltextcolor:    TColor;
    InvertedNormalTextColor: TColor;
    Groupboxcolor:      TColor;
    Backgroundcolor:    TColor;
    Selectedrecordcolor: TColor;
    Selectedrecordcolor2: TColor;
    Textfieldcolor:     TColor;
    Textfieldbackgroundcolor: TColor;

    showHelp:           Boolean;
end;

type
  TMainForm = class(TForm)
    Splitter1: TSplitter;
    Panel1: TPanel;
    Panel2: TPanel;
    SortByFrozenButton: TButton;
    SortByDescriptionButton: TButton;
    SortByAddressButton: TButton;
    SortByTypeButton: TButton;
    SortByValueButton: TButton;
    Panel3: TPanel;
    Label30: TLabel;
    Label29: TLabel;
    Label12: TLabel;
    Label27: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label13: TLabel;
    Label18: TLabel;
    Label23: TLabel;
    Label28: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label14: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    ScrollBar1: TScrollBar;
    Panel4: TPanel;
    advancedbutton: TSpeedButton;
    CommentButton: TSpeedButton;
    Panel5: TPanel;
    ProcessLabel: TLabel;
    FoundLabel: TLabel;
    ScanText: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    LoadButton: TSpeedButton;
    SaveButton: TSpeedButton;
    SpeedButton1: TSpeedButton;
    Logo: TImage;
    Label6: TLabel;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    FControl: TEdit;
    FoundList: TListBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Dos: TRadioButton;
    Windows: TRadioButton;
    Readonly: TCheckBox;
    FromAddress: TMemo;
    ToAddress: TMemo;
    cbFastScan: TCheckBox;
    NewScan: TButton;
    NextScanButton: TButton;
    ScanType: TComboBox;
    VarType: TComboBox;
    Button3: TButton;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    HexadecimalCheckbox: TCheckBox;
    UndoScan: TButton;
    rbBit: TRadioButton;
    rbDec: TRadioButton;
    scanvalue: TEdit;
    cbCaseSensitive: TCheckBox;
    PopupMenu2: TPopupMenu;
    Deletethisrecord1: TMenuItem;
    Browsethismemoryregion1: TMenuItem;
    SetHotkey1: TMenuItem;
    Freezealladdresses2: TMenuItem;
    Unfreezealladdresses1: TMenuItem;
    N5: TMenuItem;
    Setbreakpoint1: TMenuItem;
    Findoutwhatreadsfromthisaddress1: TMenuItem;
    sep1: TMenuItem;
    Calculatenewvaluepart21: TMenuItem;
    N4: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N1: TMenuItem;
    Groupoption1: TMenuItem;
    Settonogroup1: TMenuItem;
    N2: TMenuItem;
    Settogroup11: TMenuItem;
    Settogroup21: TMenuItem;
    Settogroup31: TMenuItem;
    Settogroup41: TMenuItem;
    N3: TMenuItem;
    Sortaddressesbygroup1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Browsethismemoryarrea1: TMenuItem;
    Removeselectedaddresses1: TMenuItem;
    Selectallitems1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    TopDisabler: TTimer;
    emptypopup: TPopupMenu;
    ccpmenu: TPopupMenu;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    StatusBar1: TStatusBar;
    cbSpeedhack: TCheckBox;
    Label52: TLabel;
    Edit2: TEdit;
    Edit1: TEdit;
    Label51: TLabel;
    btnSetSpeedhack: TButton;
    CheckBox8: TCheckBox;
    RadioButton3: TRadioButton;
    Findoutwhataccessesthisaddress1: TMenuItem;
    FreezeTimer: TTimer;
    UpdateTimer: TTimer;
    UpdateFoundlisttimer: TTimer;
    cbUnicode: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
    procedure DosClick(Sender: TObject);
    procedure WindowsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VarTypeChange(Sender: TObject);
    procedure ScanTypeChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Deletethisrecord1Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure NewScanClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Layoutsettings1Click(Sender: TObject);
    procedure NextScanButtonClick(Sender: TObject);
    procedure ValueClick(Sender: TObject);
    procedure CommentButtonMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SpeedButton2Click(Sender: TObject);
    procedure CommentButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure FControlEnter(Sender: TObject);
    procedure FControlExit(Sender: TObject);
    procedure Label28Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AddressClick(Sender: TObject);
    procedure Typeclick(Sender: TObject);
    procedure FControlKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FControlKeyPress(Sender: TObject; var Key: Char);
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Calculatenewvaluepart21Click(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure SettogroupXClick(Sender: TObject);
    procedure Settonogroup1Click(Sender: TObject);
    procedure Freezealladdresses2Click(Sender: TObject);
    procedure Unfreezealladdresses1Click(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure slectItem(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBox2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrollBar1Enter(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Browsethismemoryarrea1Click(Sender: TObject);
    procedure Browsethismemoryregion1Click(Sender: TObject);
    procedure scanvalueKeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox8Click(Sender: TObject);
    procedure cbSpeedhackClick(Sender: TObject);
    procedure btnSetSpeedhackClick(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
    procedure Setbreakpoint1Click(Sender: TObject);
    procedure Findoutwhatreadsfromthisaddress1Click(Sender: TObject);
    procedure Findoutwhataccessesthisaddress1Click(Sender: TObject);
    procedure advancedbuttonClick(Sender: TObject);
  private
    { Private declarations }
    numberoflines: integer;
    frozenbox:   array of TCheckbox;
    description: array of TLabel;
    address:     array of TLabel;
    ValType:     array of TLabel;
    Value:       array of TLabel;
    Select:      array of TLabel;

    oldwidth,oldheight: integer;
    newaddress: Dword;
    isbit: boolean;

    old8087CW: word;

    procedure LoadDefaultSkin;
    procedure copySelectedRecords;
    procedure Paste;
    procedure setfoundlisthorizontal;
    procedure WMGetMinMaxInfo(var Message: TMessage); message WM_GETMINMAXINFO;
    procedure SetWriteBreakpoint(address:dword;size: integer);
    procedure SetReadBreakpoint(address:dword;size: integer);
    procedure SetReadWriteBreakpoint(address:dword;size: integer);
    
  public
    { Public declarations }
    Advanced: boolean;
    ProcessOpened: boolean;

    max: integer;
    buffersize: dword;


    opening: string;

    numberofrecords,oldNumberOfRecords,oldcodelistcount: Integer;
    memrec: array of Memoryrecord;

    oldmemrec: array of Memoryrecord;
    oldcomments: string;

    values: array of String[255];
    Hotkeystrings: Array of string;
    Hotkeys: array of integer;


    selected: array of Boolean;
    lastselected: Integer;
    LastLastSelected: Integer;
    FirstShiftSelected: Integer;

    lag: Integer;

    skin:       TSkin;
    MemImage:   TMemoryStream;
    CheatEngineDir: String;
    updatetimerspeed,freezetimerspeed: integer;

    Procedure AddToRecord(line:Integer);
    procedure updatescreen;
    procedure Updateskin;
    procedure Deleterecords;
    procedure Deletegroups(groups: grouptype);
    procedure UpdateScantype;
    procedure UpdateList;
    procedure ShowValues;
    function CheckIfSaved: boolean;
    procedure Resync;
    procedure ReserveMem;
  end;

var
  MainForm: TMainForm;
  Skip_PAGE_NOCACHE: Boolean;

implementation

uses processwindow, Unit1, changetimerunit, TypePopup, addformunit,
  commentsunit, layoutunit, Unit3, Standaloneunit, Changeoffsetunit,
  formsettingsunit, MemoryBrowserFormUnit,formscanningunit,opensave,
  FoundCodeUnit, AdvancedOptionsUnit,mainunit2;

{$R *.DFM}


procedure TMainForm.WMGetMinMaxInfo(var Message: TMessage);
var MMInfo: ^MINMAXINFO;
begin //the constraint function of the form behaves weird when draging from the top or left side, so I have to do this myself.
  MMInfo:=ptr(message.LParam);
  MMInfo.ptMinTrackSize:=point(515,501);
end;

procedure TMainform.ShowValues;
var i,rec: integer;
begin
  for i:=0 to numberoflines-1 do
  begin
    rec:=scrollbar1.Position+i;
    if rec<numberofrecords then
    begin
      value[i].Caption:=values[rec];
    end;
  end;
end;

procedure TMainform.setfoundlisthorizontal;
var i,maxwidth,chars,maxchars: integer;
begin
  maxwidth:=0;
  maxchars:=0;

  for i:=0 to foundlist.Items.Count-1 do
  begin
    chars:=length(foundlist.Items[i]); //faster than 2 time retrieving
    if chars>maxchars then
    begin
      maxchars:=chars;
    end;
  end;

  foundlist.ScrollWidth:=8*maxchars;
end;

procedure TMainForm.ReserveMem;
begin
  setlength(memrec,numberofrecords);
  setlength(values,NumberOfRecords);
  setlength(selected,numberofrecords);
  setlength(hotkeystrings,NumberOfRecords);
  setlength(Hotkeys,numberofrecords);
end;

procedure TMainForm.resync;
var i,j,datasend: integer;
begin
  output[0]:=6;   //clear record list
  sendbuf(1);


  //put all adresses back
  for i:=0 to numberofrecords-1 do
  begin
    //if (i mod 2000)=0 then sleep(lag*4);  //resync should be made safe so a lof of sleeping...
    output[0]:=CS_AddAddress;
    pdword(@output[1])^:=memrec[i].Address;
    output[5]:=memrec[i].VarType;
    output[6]:=memrec[i].Bit;                              
    output[7]:=memrec[i].BitLength;
    sendbuf(8);
  end;

  //and freeze the frozen ones
  //the client should have received the values before the desync

  for i:=0 to numberofrecords-1 do
  begin
    if memrec[i].Frozen then
    begin
      if (values[i]<>'NYA') and (values[i]<>'??') then
      begin
        output[0]:=CS_FREEZEADDRESS;  //freeze address
        pword(@output[1])^:=i;
        sendbuf(3);
      end;
    end;
  end;

end;

procedure TMainForm.copySelectedRecords;
var clip: TClipboard;
    textform: string;
    i:  Integer;
begin
  //
  clip:=TClipboard.Create;
  textform:='CETables-';

//  textform:='aaa'+chr(13)+'FFFFFF'+chr(13)+'DWORD'+chr(13); there can only be an CR when hex editing the savetable.
  for i:=0 to numberofrecords-1 do
  begin
    if selected[i] then
      textform:=textform+memrec[i].Description+chr(13)+'$'+IntTohex(memrec[i].Address,8)+chr(13)+IntToStr(memrec[i].VarType)+chr(13);
  end;
  clip.SetTextBuf(pchar(textform));  //text address type

end;

procedure TMainform.paste;
var clip: TClipboard;
    textform: string;
    textform2: Pchar;
    i,j,k,l: Integer;
    last: Integer;
    temprec: array of memoryRecord;
begin
  //paste
  k:=0;
  j:=0;
  last:=10;
  getmem(textform2,10240);
  clip:=TClipboard.create;
  if clip.HasFormat(CF_TEXT) then
    l:=clip.GetTextBuf(textform2,10240)
  else
  begin
    clip.free;
    exit;
  end;

  textform:=textform2;

 // 'CETables-'

  //now extract the info.
  for i:=1 to l do
  begin
    if textform[i]=chr(13) then
    begin
      if j=0 then //decription
      begin
        setlength(temprec,k+1);
        temprec[k].Description:=copy(textform,last,i-last);
        inc(j);
      end else
      if j=1 then //address
      begin
        temprec[k].Address:=StrToInt(copy(textform,last,i-last));
        inc(j);
      end else
      if j=2 then //type
      begin
        temprec[k].VarType:=StrToInt(copy(textform,last,i-last));
        j:=0;
        inc(k);
      end;
      last:=i+1;
    end;
  end;

  inc(NumberOfRecords,k);
  setlength(memrec,mainform.NumberOfRecords);
  setlength(selected,mainform.NumberOfRecords);


  if lastselected=-1 then lastselected:=numberofrecords-k-1;
  if lastselected=-1 then lastselected:=0;
  for i:=numberofrecords-k-1 downto lastselected do
  begin
    memrec[i+k]:=memrec[i];
    selected[i+k]:=selected[i];
  end;

  for i:=lastselected to lastselected+k-1 do
  begin
    memrec[i]:=temprec[i-lastselected];
    selected[i]:=false;
  end;

  if numberofrecords>6 then scrollbar1.max:=numberofrecords-6;

  if lastselected<scrollbar1.position then scrollbar1.Position:=lastselected;
  if lastselected>scrollbar1.position+5 then scrollbar1.Position:=lastselected-5;


  mainform.updatelist;
  mainform.UpdateScreen;


  freemem(textform2);

  resync;  //wtf, i'm just to lazy to make an insert record on the server
  
end;

function TMainForm.CheckIfSaved: boolean;
var help :word;
    i: Integer;
begin
  if numberofrecords=0 then
  begin
    result:=true;
    exit;
  end;

  result:=true;

  if (OldNumberOfRecords=NumberOfRecords) then
  begin
    i:=0;
    while (result) and (i<numberofrecords) do
    begin
      if oldmemrec[i].Description<>memrec[i].Description then result:=false;
      if oldmemrec[i].Address<>memrec[i].Address then result:=false;
      if oldmemrec[i].VarType<>memrec[i].VarType then result:=false;
      if oldmemrec[i].Bit<>memrec[i].Bit then result:=false;
      if oldmemrec[i].Group<>memrec[i].Group then result:=false;
      inc(i);
    end;
  end else result:=false;

  if result=false then
  begin
    help:=messagedlg('You havn''t saved your last changes yet. Save Now?',mtConfirmation,mbYesNoCancel,0);
    case help of
      mrCancel      : result:=false;
      mrYes         : begin
                        result:=true;
                        SaveButton.click;
                      end;
    else result:=true;
    end;
  end;
end;


procedure TMainform.UpdateList;
begin
  //request a updated list for the current records
  //  scrollbar1.Position to scrollbar1.Position+numberoflines
  output[0]:=4;
  pword(@output[1])^:=scrollbar1.Position;
  pword(@output[3])^:=scrollbar1.Position+numberoflines;
  sendbuf(5);
end;

procedure TMainform.Deletegroups(groups: grouptype);
var i: integer;
begin
  for i:=0 to numberofrecords-1 do
  begin
    if groups[1] and (memrec[i].Group=1) then selected[i]:=true else
    if groups[2] and (memrec[i].Group=2) then selected[i]:=true else
    if groups[3] and (memrec[i].Group=3) then selected[i]:=true else
    if groups[4] and (memrec[i].Group=4) then selected[i]:=true else selected[i]:=false;
  end;

  deleterecords;
end;


procedure TMainForm.DeleteRecords;
var i,j: Integer;
    count: integer;
begin
  //fird out if there are more than 1 selected items
  i:=0;
  count:=0;
  while (i<numberofrecords) and (count<2) do
  begin
    if selected[i] then inc(count);
    inc(count);
  end;


  i:=0;
  while i<numberofrecords do
  begin
    if selected[i] then
    begin
      for j:=i to numberofrecords-2 do
      begin
        memrec[j]:=memrec[j+1];
        selected[j]:=selected[j+1];
      end;

      output[0]:=CS_DELETEADDRESS;
      pword(@output[1])^:=i;
      sendbuf(3);

      inc(count);
     // if count=70 then sleep(lag*3);

      dec(numberofrecords);
      dec(i);
      setlength(memrec,numberofrecords);
      setlength(selected,numberofrecords);

      if firstshiftselected>numberofrecords-1 then firstshiftselected:=-1;
    end;

    inc(i);
  end;

  If lastselected>numberofrecords-1 then lastselected:=numberofrecords-1;
  if lastselected>-1 then selected[lastselected]:=true;
  Updatescreen;

//  Updatelist;
end;

procedure TMainform.LoadDefaultSkin;
var default: Tfilestream;
    i: Integer;
begin

end;

procedure TMainForm.UpdateSkin;
begin
end;

procedure TMainform.Updatescreen;
var rec,i: Integer;
begin
  if numberofrecords>=numberoflines then
  begin
    scrollbar1.enabled:=true;
    scrollbar1.max:=numberofrecords-1;
    scrollbar1.pagesize:=numberoflines-1;
    scrollbar1.LargeChange:=numberoflines-1;
  end else
  begin
    scrollbar1.enabled:=false;
    scrollbar1.position:=0;
  end;

  for i:=0 to numberoflines-1 do
  begin
    rec:=scrollbar1.Position+i;
    if rec<numberofrecords then
    begin
      if hotkeys[rec]<>-1 then
      begin
        frozenbox[i].Checked:=false;
        frozenbox[i].Enabled:=false;
      end
      else
      begin
        frozenbox[i].checked:=memrec[rec].frozen;
        frozenbox[i].Enabled:=true;
      end;

      description[i].Caption:=memrec[rec].Description;
      address[i].caption:=IntTohex(memrec[rec].address,8);


      if not frozenbox[i].visible then  //if it's not yet visible, make it visible
      begin
        frozenbox[i].visible:=true;
        description[i].visible:=true;
        address[i].visible:=true;
        valtype[i].visible:=true;
        value[i].visible:=true;
      end;

      case memrec[rec].vartype of
      0         : valtype[i].Caption:='Byte';
      1         : valtype[i].Caption:='2 Bytes';
      2         : valtype[i].Caption:='4 Bytes';
      3         : valtype[i].caption:='Float';
      4         : valtype[i].caption:='Double';
      5         : begin
                    valtype[i].Caption:='Binary['+IntToStr(memrec[rec].Bitlength)+']';
                    address[i].caption:=address[i].caption+'^'+IntToStr(memrec[rec].Bit);
                  end;
      6         : valtype[i].caption:='8 Bytes';
      7         : valtype[i].Caption:='Text['+inttostr(memrec[rec].Bit)+']';
      8         : valtype[i].Caption:='Bytes['+inttostr(memrec[rec].Bit)+']';

      else      valtype[i].caption:='BUG!';
      end;

      if selected[rec] then
      begin
        if lastselected=rec then
        begin
          select[i].Color:=clActiveCaption;
          select[i].Font.Color:=clActiveCaption;
          Frozenbox[i].Color:=clActiveCaption;
        end
        else
        begin
          select[i].Color:=clGradientActiveCaption;
          select[i].Font.Color:=clGradientActiveCaption;
          Frozenbox[i].Color:=clGradientActiveCaption;
        end;
        description[i].Font.Color:=clHighlightText;
        address[i].Font.Color:=clHighlightText;
        valtype[i].Font.Color:=clHighlightText;
        value[i].Font.Color:=clHighlightText;
      end
      else
      begin
        case memrec[rec].Group of
          0  :  begin
                  description[i].Font.Color:=clWindowText;
                  address[i].Font.Color:=clWindowText;
                  valtype[i].Font.Color:=clWindowText;
                  value[i].Font.Color:=clWindowText;
                end;

          1  :  begin
                  description[i].Font.Color:=clRed;
                  address[i].Font.Color:=clRed;
                  valtype[i].Font.Color:=clRed;
                  value[i].Font.Color:=clRed;
                end;

          2  :  begin
                  description[i].Font.Color:=clBlue;
                  address[i].Font.Color:=clBlue;
                  valtype[i].Font.Color:=clBlue;
                  value[i].Font.Color:=clBlue;
                end;

          3  :  begin
                  description[i].Font.Color:=clYellow;
                  address[i].Font.Color:=clYellow;
                  valtype[i].Font.Color:=clYellow;
                  value[i].Font.Color:=clYellow;
                end;

          4  :  begin
                  description[i].Font.Color:=clGreen;
                  address[i].Font.Color:=clGreen;
                  valtype[i].Font.Color:=clGreen;
                  value[i].Font.Color:=clGreen;
                end;

        end;

        select[i].Color:=clBtnFace;
        select[i].Font.Color:=clBtnFace;
        Frozenbox[i].Color:=clBtnFace;
      end;
    end else
    begin
      description[i].Font.Color:=clBtnFace;
      select[i].Color:=clBtnFace;
      select[i].Font.Color:=clBtnFace;
      description[i].Font.Color:=clBtnFace;
      Frozenbox[i].Color:=clBtnFace;
      address[i].Font.Color:=clBtnFace;
      valtype[i].Font.Color:=clBtnFace;
      value[i].Font.Color:=clBtnFace;

      if frozenbox[i].visible then
      begin
        frozenbox[i].visible:=false;
        description[i].visible:=false;
        address[i].visible:=false;
        valtype[i].visible:=false;
        value[i].visible:=false;
      end;

    end;
  end;
end;


procedure TMainForm.UpdateScanType;
var OldText: String;
    OldIndex: Integer;
    hexvis: boolean;
resourcestring
  strexact='Exact';
  strexactvalue='Exact Value';
  strbiggerThan='Bigger than...';
  strSmallerThan='Smaller than...';
  strIncreasedValue='Increased value';
  strIncreasedValueBy='Increased value by ...';
  strDecreasedValue='Decreased value';
  strDecreasedValueBy='Decreased value by ...';
  strChangedValue='Changed value';
  strUnchangedValue='Unchanged value';
  strUnknownInitialValue='Unknown initial value';

  strScantextcaptiontotext='Text:';
  strScantextcaptiontoValue='Value:';
  strsearchForText='Search for text';
  strSearchForArray='Search for this array';
begin


  OldIndex:=Scantype.itemindex;
  OldText:=Scantype.text;
  hexvis:=true;

  ScanType.Items.Clear;

  ScanText.Caption:=strScantextcaptiontoValue;
  case varType.ItemIndex of
    0   :     begin
                ScanType.Items.Add(strExact);
                ScanType.DropDownCount:=1;
              end;

  1,2,3,4,5,6:begin  //byte-word-dword--8bytes-float-double
                if vartype.itemindex in [5,6] then
                  hexvis:=false;

                ScanType.Items.Add(strExactValue);
                ScanType.Items.Add(strBiggerThan);
                ScanType.Items.Add(strsmallerThan);

                if NextScanbutton.Enabled then
                begin
                  scantype.Items.Add(strIncreasedValue);
                  Scantype.Items.Add(strIncreasedValueBy);
                  ScanType.Items.Add(strDecreasedValue);
                  ScanType.Items.Add(strDecreasedValueBy);
                  ScanType.Items.add(strChangedValue);
                  ScanType.Items.Add(strUnchangedValue);
                  Scantype.DropDownCount:=9;
                end else
                begin
                  ScanType.Items.Add(strUnknownInitialValue);
                  ScanType.DropDownCount:=4;
                end;
              end;

  7:          begin  //text
                ScanText.caption:=strScanTextCaptionToText;
                ScanType.Items.Add(strSearchForText);
                //perhaps also a changed value and unchanged value scan
                ScanType.DropDownCount:=1;
                hexvis:=false;
              end;

  8:          begin  //array of bytes
                ScanText.caption:=vartype.Items[8];
                ScanType.Items.Add(strSearchforarray);
                ScanType.DropDownCount:=1;
              end;

  end;

  if (oldtext=strUnknownInitialValue) and (NextScanButton.enabled) then scantype.itemindex:=0 else scantype.itemindex:=oldindex;


  if (scantype.Text=strIncreasedValue) or
     (scantype.text=strDecreasedValue) or
     (scantype.Text=strChangedValue) or
     (scantype.Text=strUnchangedValue) or
     (scantype.Text=strUnknownInitialValue) then
     begin
       Scantext.Visible:=false;
       Scanvalue.visible:=false;
       HexadecimalCheckbox.visible:=false;
     end else
     begin
       Scantext.Visible:=true;
       Scanvalue.visible:=true;
       HexadecimalCheckbox.visible:=hexvis;
     end;

end;




procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Connectform.close;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
var FName: String;
    expectedFilename: String;
    i: integer;
begin
  proceswindow:=TProceswindow.Create(self);
  if ProcesWindow.ShowModal=mrOk then
  begin
    if numberofrecords>0 then
    begin
      if messagedlg('Keep the current address list?', mtConfirmation, [mbYes, mbNo], 0)=mrNo then
      begin
        Comments.Memo1.Lines.Text:='';
        Numberofrecords:=0;
        Updatescreen;
      end else updatelist;
    end;

    if (copy(processlabel.caption,length(processlabel.caption)-11,12)='WINOA386.MOD') or
       (copy(processlabel.caption,pos('-',processlabel.caption)+1,13)='MS-DOS Prompt') then
    begin
      fromaddress.text:='80000000';
      toaddress.text:='BFFFFFFF';
      dos.checked:=true;
    end else
    begin
      FromAddress.text:='00400000';
      ToAddress.text:='7FFFFFFF';
      windows.checked:=true;
    end;

    //-------------NEW SCAN


    foundlabel.Caption:='0';
    foundlist.Clear;

    newscan.Caption:='First Scan';

    Groupbox1.Enabled:=true;
    for i:=0 to groupbox1.ControlCount-1 do
      groupbox1.Controls[i].Enabled:=true;

    scanvalue.Enabled:=true;
    newscan.Enabled:=true;
    nextscanbutton.Enabled:=false;
    vartype.Enabled:=true;
    scantype.Enabled:=true;
    scantext.enabled:=true;
    label4.enabled:=true;
    label8.Enabled:=true;
    HexadecimalCheckbox.Enabled:=true;

    scanvalue.visible:=true;
    scantext.Visible:=true;
    scanvalue.text:='';
    checkbox8.Enabled:=true;
    cbspeedhack.Enabled:=true;


    Updatescantype;
    Scantype.ItemIndex:=0;



    //-----------------------
    //Get the expectedFilename
    //-----------------------
    if savedialog1.Filename<>'' then exit;
    if opendialog1.Filename<>'' then exit;

    Fname:=copy(processlabel.caption,pos('-',processlabel.caption)+1,length(processLabel.caption));

  if FName[length(FName)-3]='.' then  //it's a filename
    expectedFilename:=copy(FName,1,length(FName)-4)
  else //it's a normal title;
    expectedFilename:=FName;


    savedialog1.FileName:=expectedFilename;
    Opendialog1.FileName:=expectedFilename;
  end;
end;

procedure TMainForm.DosClick(Sender: TObject);
begin
  FromAddress.text:='80000000';
  ToAddress.text:='BFFFFFFF';
end;

procedure TMainForm.WindowsClick(Sender: TObject);
begin
  FromAddress.text:='00400000';
  ToAddress.text:='7FFFFFFF';
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  scanvalue.text:='';
  fromaddress.Text:='00400000';
  toaddress.Text:='7FFFFFFF';
  isbit:=false;

  panel3.DoubleBuffered:=true;

  old8087CW:=Get8087CW;
  Set8087CW($133f);  //for those changingmy code: 133f is NOT meant to be 1337

  Memimage:=TMemorystream.Create;
  FirstShiftSelected:=-1;

  oldwidth:=screen.width;
  oldheight:=screen.height;

  oldnumberofrecords:=0;

  logo.Hint:='Left-click to go to the Cheat Engine Homepage'#13#10+
             'Right-click to access the menu';

  logo.ShowHint:=true;

  numberoflines:=7;
  setlength(frozenbox,7);
  frozenbox[0]:=checkbox1;
  frozenbox[1]:=checkbox2;
  frozenbox[2]:=checkbox3;
  frozenbox[3]:=checkbox4;
  frozenbox[4]:=checkbox5;
  frozenbox[5]:=checkbox6;
  frozenbox[6]:=checkbox7;

  setlength(description,7);
  description[0]:=label28;
  description[1]:=label9;
  description[2]:=label17;
  description[3]:=label19;
  description[4]:=label33;
  description[5]:=label32;
  description[6]:=label31;

  setlength(address,7);
  address[0]:=label23;
  address[1]:=label10;
  address[2]:=label20;
  address[3]:=label21;
  address[4]:=label34;
  address[5]:=label35;
  address[6]:=label36;

  setlength(valtype,7);
  valtype[0]:=label18;
  valtype[1]:=label11;
  valtype[2]:=label22;
  valtype[3]:=label24;
  valtype[4]:=label40;
  valtype[5]:=label39;
  valtype[6]:=label37;

  setlength(value,7);
  value[0]:=label13;
  value[1]:=label14;
  value[2]:=label25;
  value[3]:=label26;
  value[4]:=label41;
  value[5]:=label42;
  value[6]:=label43;

  setlength(select,7);
  select[0]:=label12;
  select[1]:=label3;
  select[2]:=label16;
  select[3]:=label15;
  select[4]:=label27;
  select[5]:=label29;
  select[6]:=label30;

  updatescreen;

  newaddress:=0;

  VarType.ItemIndex:=3;
  UpdateScantype;
  ScanType.ItemIndex:=0;


end;

procedure TMainForm.ScanTypeChange(Sender: TObject);
begin
  updatescantype;
end;

procedure TMainForm.VarTypeChange(Sender: TObject);
var a: int64;
    b: double;
    c: Integer;
    d: single;
    hexvis: boolean;
    decbitvis: boolean;
    hexleft: integer;
    hextext: string;
    hexwidth: integer;
    casevis: boolean;

    oldscantype: integer;
    error: integer;
    temp:string;
begin
  hexvis:=true;
  hexleft:=187;
  hexwidth:=38;
  hextext:='Hex';
  casevis:=false;

  decbitvis:=false;


  if not (oldscantype in [smallerthan,biggerthan,exact_value,Advanced_Scan]) then
    scantype.itemindex:=0;
  //Scantype.itemindex:=0;
  case vartype.itemindex of
  0: begin //binary
       if (hexadecimalcheckbox.visible) then
       begin
         //convert it was type 0,1,2 or 6
         if hexadecimalcheckbox.Checked then
         begin
           //convert from hex to dec
           if length(scanvalue.Text)>0 then
           begin
             if scanvalue.Text[1]='-' then
               val('-$'+copy(scanvalue.text,2,length(scanvalue.Text)),a,error)
             else
               val('$'+scanvalue.Text,a,error);
           end;
         end else val(scanvalue.text,a,error);
       end;

       hexadecimalcheckbox.Checked:=true;
       rbdec.checked:=true;
       scanvalue.Text:=IntToStr(a);

       HexadecimalCheckbox.visible:=false;
       decbitvis:=true;
       Scantype.itemindex:=0;

     end;

   1,2,3,4:
     begin
       casevis:=false;


       scanvalue.MaxLength:=0;

       if rbdec.visible then
       begin
         if rbbit.checked then
           scanvalue.text:=IntToStr(binToInt(scanvalue.text))
         //else keep it the same
       end
       else
       begin
         if hexadecimalcheckbox.Checked and hexadecimalcheckbox.Visible then
           val('$'+copy(scanvalue.text,1,length(scanvalue.Text)),a,c)
         else
           val(scanvalue.text,a,c);

         scanvalue.Text:=IntToStr(a);
       end;

       hexadecimalcheckbox.enableD:=true;
       hexadecimalcheckbox.Checked:=false;
     end;

  5: begin //float;
       hexvis:=false;
       temp:=scanvalue.text;

       if rbdec.visible then
       begin
         //it was a bit
         if rbbit.checked then
           temp:=IntToStr(binToInt(scanvalue.text))
         //else keep it the same
       end
       else
       begin
         if hexadecimalcheckbox.visible then
         begin
           if hexadecimalcheckbox.Checked then
             val('$'+copy(scanvalue.text,1,length(scanvalue.Text)),a,c)
           else
             val(scanvalue.text,a,c);

           temp:=IntToStr(a);
         end;
       end;


       hexadecimalcheckbox.Checked:=false;
       HexadecimalCheckbox.enabled:=false;
       scanvalue.MaxLength:=0;

       val(temp,d,c);
       Scanvalue.Text:=FloatToStr(d);
     end;

  6: begin //double
       hexvis:=false;
       temp:=scanvalue.text;

       if rbdec.visible then
       begin
         //it was a bit
         if rbbit.checked then
           temp:=IntToStr(binToInt(scanvalue.text))
         //else keep it the same
       end
       else
       begin
         if hexadecimalcheckbox.visible then
         begin
           if hexadecimalcheckbox.Checked then
             val('$'+copy(scanvalue.text,1,length(scanvalue.Text)),a,c)
           else
             val(scanvalue.text,a,c);

           temp:=IntToStr(a);
         end;
       end;


       hexadecimalcheckbox.Checked:=false;
       HexadecimalCheckbox.enabled:=false;
       scanvalue.MaxLength:=0;

       val(temp,b,c);
       Scanvalue.Text:=FloatToStr(b);
     end;

  7: begin //text
       scantype.itemindex:=0;
       casevis:=true;
       cbCasesensitive.Checked:=true;
       cbCasesensitive.ShowHint:=false;


       hexadecimalcheckbox.enabled:=true;
       hexadecimalcheckbox.checkeD:=cbCaseSensitive.checked;
       hexvis:=false;
       hextext:='Unicode';
       hexleft:=170;
       hexwidth:=61;
     end;

  8: begin  //array of byte
       scantype.itemindex:=0;
       scanvalue.MaxLength:=0;
       hexadecimalcheckbox.enableD:=true;
       hexadecimalcheckbox.Checked:=true;
       for c:=1 to length(scanvalue.Text) do
         case scanvalue.text[c] of
           '-',' '  :  ;
           '0'..'9' :  ;
           'A'..'F' :  ;
           else     begin
                      scanvalue.text:='';
                      break;
                    end;
         end;
     end;
  end;

  hexadecimalcheckbox.Caption:=hextext;
  hexadecimalcheckbox.left:=hexleft;
  hexadecimalcheckbox.Width:=hexwidth;
  HexadecimalCheckbox.visible:=hexvis;
  rbdec.visible:=decbitvis;
  rbbit.Visible:=decbitvis;

  cbCaseSensitive.visible:=casevis;

  cbfastscan.Enabled:=vartype.ItemIndex in [2..6];

  UpdateScanType;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if addform=nil then addform:=TAddform.Create(self);
  addform.showmodal;
end;


Procedure TMainForm.AddToRecord(line:Integer);
var error: Integer;
    Address: Dword;
    i,j:  Integer;
    bit: Integer;
    tmp: string;
    pc: ^char;

    found: boolean;
    bitl,nrofbits: integer;
begin
//first check if this address is already in the list!
  i:=getvartype;
  nrofbits:=length(foundlist.Items[line])-11;

  if i=5 then //binary
  begin
    val(FoundList.Items[line][10],bit,error);
    bitl:=nrofbits;
  end
  else
  begin
    bit:=0;
    bitl:=0;
  end;

  if i=7 then //text
    bit:=length(FoundList.Items[line])-8;  //in case of text use the bit record space for length

  if i=8 then //array of bytes
  begin
    //bit will get the number of spaces +1
    tmp:=foundlist.Items[line];
    pc:=@tmp[9];

    bit:=0;
    while pc^<>chr(0) do
    begin
      if pc^=' ' then inc(bit);
      inc(pc);
    end;

  end;

  val('$'+FoundList.Items[line],address,error);

  found:=false;
  for j:=0 to NumberOfRecords-1 do
    if (memrec[j].Address=address) and
       (i=memrec[j].vartype) and
       (bit=memrec[j].Bit) then
    begin
      if memrec[j].vartype=5 then
      begin
        if memrec[i].bitlength=bitl then
        begin
          found:=true;
          break;
        end;
      end
      else
      begin
        found:=true;
        break;
      end;
    end;

  if not found then
  begin
    inc(NumberOfRecords);
    ReserveMem;

    memrec[NumberOfRecords-1].Description:='No Description';
    memrec[NumberOfRecords-1].Address:=address;

    if getscantype=string_scan then memrec[NumberOfRecords-1].VarType:=0 else
                                    memrec[NumberOfRecords-1].VarType:=getvartype;

    memrec[NumberOfRecords-1].Frozen:=false;
    memrec[NumberOfRecords-1].FrozenValue:=0;

    memrec[NumberOfRecords-1].Bit:=bit;
    memrec[NumberOfRecords-1].bitlength:=bitl;
    hotkeys[NumberOfRecords-1]:=-1;

    //add
    output[0]:=CS_ADDADDRESS;
    pdword(@output[1])^:=memrec[NumberOfRecords-1].Address;
    output[5]:=memrec[NumberOfRecords-1].VarType;
    output[6]:=memrec[NumberOfRecords-1].Bit;
    output[7]:=memrec[NumberOfRecords-1].Bitlength;
    sendbuf(8);


  end
  else if memrec[j].VarType=5 then raise Exception.Create(IntToHex(address,8)+'^'+IntToStr(memrec[j].bit)+' with length of '+IntToStr(nrofbits)+' bits, is already in the list!') else
                                   raise Exception.Create(IntToHex(address,8)+' is already in the list!');




  updatescreen;
  updatelist;

  scrollbar1.Position:=scrollbar1.Max-numberoflines+2;
end;



procedure TMainForm.SpeedButton3Click(Sender: TObject);
var i: Integer;
    morein: boolean;
begin
  morein:=false;
  for i:=0 to foundlist.Items.Count-1 do
    try
      if foundlist.Selected[i] then AddToRecord(i);
    except
      if foundlist.SelCount=1 then raise exception.Create('This address is already in the list') else morein:=true;
    end;

  if morein then showmessage('One or more addresses where already in the list');
  FControl.SetFocus;
end;

procedure TMainForm.Deletethisrecord1Click(Sender: TObject);
var i,j: Integer;
    comfirm: Word;
    deletegroup: grouptype;
    IsGroup: boolean;
    multiplegroups: boolean;
begin
  //delete selectedrecord
  isgroup:=false;
  multiplegroups:=false;
  for i:=1 to 4 do deletegroup[i]:=false;
  i:=0;
  j:=0;

  while (i<numberofrecords) do
  begin
    if selected[i] then
    begin
      inc(j);
      if memrec[i].Group>0 then
      begin
        if isgroup and (deletegroup[memrec[i].Group]) then multiplegroups:=true;
        deletegroup[memrec[i].Group]:=true;
        isgroup:=true;
      end;
    end;

    inc(i);
  end;


  comfirm:=MrNo;
  if j>1 then comfirm:=Messagedlg('Delete these addresses?', mtWarning, [mbYes, mbNo], 0);
  if j=1 then comfirm:=Messagedlg('Delete this address?', mtWarning, [mbYes, mbNo], 0);
  if j=0 then exit;

  if comfirm=mrNo then exit;

  if isgroup and not multiplegroups then comfirm:=Messagedlg('Also delete the group this address is a member of?', mtConfirmation, [mbYes, mbNo], 0);
  if isgroup and multiplegroups then comfirm:=Messagedlg('Also delete the groups of the addresses you selected?', mtConfirmation, [mbYes, mbNo], 0);

  deleterecords;

  if comfirm=mrYes then deletegroups(deletegroup);
end;

procedure TMainForm.ScrollBar1Change(Sender: TObject);
begin
  updatescreen;
  updatelist;
  
end;

procedure TMainForm.NewScanClick(Sender: TObject);
var FromAdd,ToAdd:Dword;
    error: Integer;
//    Tim,Tim2: TTimeStamp;
    Vtype,SType: Integer;
    Processhandle2: Thandle;
    i: integer;
    value: string;
    templist: TStringlist;

    res: word;
    extra: boolean;

    lastscantype: integer;

begin
  if not processopened then exit;
  try

  screen.Cursor:=crhourglass;

//first makesure all files used are closed (normaly not needed but there is this weird bug files are not closed...)
//  addressfile, memoryfile: File;
//  newAddressfile,newmemoryfile: File;
  advanced:=false;
  undoscan.enabled:=false;

  lastscantype:=scantype.ItemIndex;


  VType:=GetVarType;
  SType:=GetScanType;

  foundlist.Clear;
  if not nextscanbutton.Enabled then //it's a first scan
  begin
    if scanvalue.visible then
    if scanvalue.text='' then raise Exception.Create('Please fill in something!');

    val('$'+FromAddress.text,FromAdd,error);
    val('$'+ToAddress.text,ToAdd,error);

    foundlabel.visible:=true;
    if max>32000 then raise Exception.Create('The max number of addresses to show may not be set higher than 32000');

    mainform.caption:=CERegionSearch;
    application.ProcessMessages;
    foundlabel.caption:='';
    foundlist.items.clear;



    //send scan
    output[0]:=CS_FirstScan;
    pdword(@output[1])^:=FromAdd;
    pdword(@output[5])^:=ToAdd;
    output[9]:=vtype;
    output[10]:=stype;

    output[11]:=


    length(scanvalue.text);


    if output[11]=0 then
    begin
      output[11]:=1;
      output[12]:=0;
    end
    else copymemory(@output[12],@scanvalue.text[1],output[11]);

    output[12+output[11]]:=0;
    if cbFastscan.checked then output[12+output[11]]:=output[12+output[11]] or SO_FASTSCAN;
    if HexaDecimalCheckbox.checked then output[12+output[11]]:=output[12+output[11]] or SO_HEXADECIMAL;
    if readonly.checked then output[12+output[11]]:=output[12+output[11]] or SO_READONLY;
    if cbunicode.checked then output[12+output[11]]:=output[12+output[11]] or SO_UNICODE;

    sendbuf(12+output[11]+1);
    formscanning:=TFormscanning.create(self);
    res:=formscanning.showmodal;

    setfoundlisthorizontal;

    progressbar1.Position:=0;
    mainform.Caption:=CEnorm;

    if res=mrCancel then
    begin
      exit;
    end;

    Groupbox1.Enabled:=false;
    for i:=0 to groupbox1.ControlCount-1 do
      groupbox1.Controls[i].Enabled:=false;

    vartype.Enabled:=false;
    nextscanbutton.enabled:=true;
    newscan.Caption:='New Scan';
    beep;

    UpdateScanType;

    Scantype.ItemIndex:=lastscantype;
    screen.Cursor:=crdefault;
  end
  else
  begin
    //close files in case of a bug i might have missed...
    //send newscan command
    output[0]:=CS_NEWSCAN;
    sendbuf(1);


    vartype.visible:=true;
    foundlabel.Caption:='0';
    foundlist.Clear;

    newscan.Caption:='First Scan';

    nextscanbutton.Enabled:=false;
    vartype.Enabled:=true;

    scanvalue.visible:=true;
    scantext.Visible:=true;

    Updatescantype;
    Scantype.ItemIndex:=0;
    Groupbox1.Enabled:=true;
    for i:=0 to groupbox1.ControlCount-1 do
      groupbox1.Controls[i].Enabled:=true;

    case vtype of
     0,1,2  : begin
                val(scanvalue.Text,i,error);
                if error>0 then scanvalue.text:=IntToStr(i);
              end;
    end;
    setfoundlisthorizontal;

  end;

  if stype=string_scan then nextscanbutton.enabled:=false;

  if (foundlabel.caption='0') and (SType<>Advanced_scan) then NextScanButton.enabled:=false;

  progressbar1.max:=10;
  progressbar1.Position:=0;
  UpdateScanType;


  finally
    screen.Cursor:=crdefault;
  end;
end;


procedure TMainForm.CheckBox1Click(Sender: TObject);
begin
  UpdateScreen;
end;

procedure TMainForm.Layoutsettings1Click(Sender: TObject);
begin
  layout.show;
end;

procedure TMainForm.NextScanButtonClick(Sender: TObject);
var error: Integer;
    Vtype,SType: Integer;
    hexvalue: string;
    templist: tstringlist;
    i: integer;
    res: word;

    temp,temp2: dword;
begin
  try
  screen.Cursor:=crhourglass;
  foundlabel.visible:=true;
  VType:=GetVarType;
  SType:=GetScanType;

  mainform.caption:=CESearch;
  application.ProcessMessages;

  if not((Stype=Advanced_scan) or (Stype=string_scan)) then
  begin
    vartype.Enabled:=false;
    foundlist.clear;
    foundlabel.caption:='';

    //send scan
    output[0]:=CS_NextScan;
    output[1]:=stype;
    output[2]:=length(scanvalue.text);
    if output[2]=0 then
    begin
      output[2]:=1;
      output[3]:=0;
    end
    else copymemory(@output[3],@scanvalue.text[1],output[2]);

    output[3+output[2]]:=0;
    if cbFastscan.checked then output[3+output[2]]:=output[3+output[2]] or SO_FASTSCAN;
    if HexaDecimalCheckbox.checked then output[3+output[2]]:=output[3+output[2]] or SO_HEXADECIMAL;

    sendbuf(3+output[2]+1);

    formscanning:=TFormscanning.create(self);
    formscanning.showmodal;

    setfoundlisthorizontal;

    Beep;
    undoscan.Enabled:=true;
  end else showmessage('You can''t do a nextscan with the current selected way of scanning!');

  mainform.caption:=CENorm;
  progressbar1.max:=10;
  progressbar1.Position:=0;
  screen.Cursor:=crdefault;

  finally
    screen.Cursor:=crdefault;
    if nextscanbutton.Enabled then
    begin
      newscan.Default:=false;
      nextscanbutton.Default:=true
    end
    else
    begin
      nextscanbutton.Default:=false;
      newscan.Default:=true;
    end;
  end;
end;


procedure TMainForm.ValueClick(Sender: TObject);
var oldvaluest: string;
    newvalueSt: String;
    error,i: Integer;

begin
  if (value[(sender as TLabel).tag].Caption='NYA') or (value[(sender as TLabel).tag].Caption='??') or (value[(sender as TLabel).tag].Caption='NAN') or (value[(sender as TLabel).tag].Caption='INF') then
  begin
    beep;
    exit;
  end;

  oldvaluest:=value[(sender as TLabel).tag].Caption;
  newvaluest:=InputBox('Value','Change the value to:',value[(sender as TLabel).tag].Caption);

  if oldvaluest=newvaluest then exit;

  error:=0;

  case memrec[scrollbar1.position+(sender as TLabel).tag].VarType of
    0,1,2,5,6: begin
                  try
                    StrtoInt(newvaluest);
                  except
                    error:=1;
                  end;
//                  val(newvaluest,newvalue6,error);    //ok, newvalue6 isnt uded, but I need to see if there's an error
                end;

    3,4:        begin
                  try
                    StrTofloat(newvaluest);
                  except
                    error:=1;
                  end;
//                  val(newvaluest,newvalue5,error);
                end;
  end;


  if error=0 then
  begin
    if length(newvaluest)>250 then raise exception.create('A value can''t be longer than 250 characters');
    //newvaluest contains a valid variable
    values[scrollbar1.position+(sender as TLabel).tag]:='NYA';

    output[0]:=7;
    pword(@output[1])^:=scrollbar1.position+(sender as TLabel).tag;
    output[3]:=length(newvaluest);

    for i:=1 to length(newvaluest) do
      output[3+i]:=byte(newvaluest[i]);

    sendbuf(4+output[3]);
  end else raise Exception.create('This is not a valid value!');

  updatelist;
  updatescreen;
end;

procedure TMainForm.CommentButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if comments.Memo1.Lines.Count=0 then
  begin
    Commentbutton.Hint:='No Comments!';
    exit;
  end;

  Commentbutton.Hint:=copy(comments.Memo1.Text,1,20);
  if length(comments.Memo1.Text)>20 then Commentbutton.Hint:=Commentbutton.Hint+'...';
end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
var i: integer;
begin
  if numberofrecords>0 then
  begin
    if messagedlg('Are you sure you want to delete all addresses?',mtWarning,[mbYes,mbNo],0)=mrNo then exit;

    for i:=0 to numberofrecords-1 do
    begin
      output[0]:=15;
      pword(@output[1])^:=i;
      sendbuf(3);
    end;

    numberofRecords:=0;
    updatelist;  //make sure the wipeout is successfull
    updatescreen;
  end;
end;

procedure TMainForm.CommentButtonClick(Sender: TObject);
begin
  comments.showmodal;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
var savefile: File;
    actualwritten: Integer;
    Controle: String[6];
    records: dword;
    x: Pchar;

begin
  if (savedialog1.FileName='') and (opendialog1.filename<>'') then
  begin
    //set the filename the table was opened with to the filename you save as default to
    //and dont forget to change the extension to .CT3
    savedialog1.FileName:=ChangeFileExt(opendialog1.FileName,'');
  end;

  SaveDialog1.InitialDir:=cheatenginedir;
  if Savedialog1.Execute then
  begin
    savetable(savedialog1.FileName);
  end;
end;

procedure TMainForm.LoadButtonClick(Sender: TObject);
var merge: boolean;
    app: word;
    Extension: string;
begin
  merge:=false;
  if CheckIfSaved=false then exit;

  OpenDialog1.InitialDir:=cheatenginedir;

  if Opendialog1.Execute then
  begin
    Extension:=uppercase(extractfileext(opendialog1.filename));
    if (Extension<>'.GH') and
       (Extension<>'.CET') and
       (Extension<>'.CT2') and
       (Extension<>'.CT3') and
       (Extension<>'.CT') and
       (Extension<>'.EXE') then raise exception.create('Unknown extention');


    if ((numberofrecords>0)) and (Extension<>'.EXE') then app:=messagedlg('You wish to merge the current table with this table?',mtConfirmation,mbYesNoCancel,0);
    case app of
      mrCancel: exit;
      mrYes: merge:=true;
      mrNo: merge:=false;
    end;

    LoadTable(Opendialog1.filename,merge);

  end;

  resync;

  Updatescreen;
  updatelist;
end;


procedure TMainForm.FControlEnter(Sender: TObject);
begin
  LastSelected:=LastLastSelected;
  Updatescreen;
end;

procedure TMainForm.FControlExit(Sender: TObject);
begin
  LastLastSelected:=LastSelected;
  Lastselected:=-1;
  Updatescreen;
end;

procedure TMainForm.Label28Click(Sender: TObject);
var newdesc: string;
begin
  if lastselected<>scrollbar1.position+(sender as TLabel).tag then exit;

  newdesc:=InputBox('Description','Change the description to:',memrec[scrollbar1.position+(sender as TLabel).tag].Description);
  if newdesc='' then newdesc:='No description!';
  memrec[scrollbar1.position+(sender as TLabel).tag].Description:=newdesc;
  updatescreen;

  //updatelist not needed
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=CheckIfSaved;
end;

procedure TMainForm.AddressClick(Sender: TObject);
var adr: String;
    newadr: dword;
    error: Integer;
    bit: Byte;
begin
  if memrec[scrollbar1.position+(sender as TLabel).tag].Frozen then
    raise Exception.Create('You first have to unfreeze this address before you can change it');

  if memrec[scrollbar1.position+(sender as TLabel).tag].VarType=5 then //bit
    adr:=inputbox('Address','Give the new address: (use ^ to specify the startbit)',address[(sender as TLabel).tag].caption) else
    adr:=inputbox('Address','Give the new address:',address[(sender as TLabel).tag].caption);

  if adr='DEBUG' then
  begin
    showmessage(IntToStr(scrollbar1.position)+' - '+IntToStr((Sender As TLabel).tag));
    showmessage(IntToStr(scrollbar1.position+(sender as TLabel).tag)+' - '+address[scrollbar1.position+(sender as TLabel).tag].caption);
  end;

  if memrec[scrollbar1.position+(sender as TLabel).tag].VarType=5 then
    if pos('^',adr)=0 then
      raise exception.Create('A binary type needs a startbit!  (notation:xxxxxxxx^x)');

  if adr[1]<>'$' then adr:='$'+adr;
  val(adr,newadr,error);
  if error=0 then memrec[scrollbar1.position+(sender as TLabel).tag].Address:=newadr else
    if adr[error]='^' then
    begin
      //newadr contains the right address
      //now just set the right bit

      if length(adr)-error>1 then
        raise exception.Create(adr+' is not an valid address!');

      try
        Bit:=StrToInt(adr[length(adr)]);
      except
        raise exception.Create(adr+' is not an valid address!');
      end;

      if bit>7 then
        raise exception.Create(adr+' is not an valid address!');

      //we made it to here so I assume this address is valid ;-)
      memrec[scrollbar1.position+(sender as TLabel).tag].Address:=newadr;
      memrec[scrollbar1.position+(sender as TLabel).tag].Bit:=Bit;
    end else
    raise exception.Create(adr+' is not an valid address!');

  updatescreen;
  updatelist;
end;

procedure TMainForm.Typeclick(Sender: TObject);
var i,j: Integer;
    oldType: Integer;

begin
  OldType:=memrec[scrollbar1.position+(sender as TLabel).tag].VarType;

  if memrec[scrollbar1.position+(sender as TLabel).tag].Frozen then
    Raise Exception.Create('You can''t change the type of an frozen value. First unfreeze it!');
  TypeForm.NrOfRecord:=scrollbar1.position+(sender as TLabel).tag;


  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=0 then TypeForm.VarType.itemindex:=1 else  //byte
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=1 then TypeForm.VarType.itemindex:=2 else  //word
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=2 then TypeForm.VarType.itemindex:=3 else  //dword
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=3 then TypeForm.VarType.itemindex:=5 else  //float
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=4 then TypeForm.VarType.itemindex:=6 else  //double
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=5 then TypeForm.VarType.itemindex:=0 else  //bit
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=6 then TypeForm.VarType.itemindex:=4 else //int64
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=7 then TypeForm.VarType.itemindex:=7 else //text
  if memrec[scrollbar1.position+(sender as TLabel).Tag].VarType=8 then Typeform.VarType.ItemIndex:=8;


  case memrec[scrollbar1.position+(sender as TLabel).tag].Bit of
  0     :       TypeForm.RadioButton1.checked:=true;
  1     :       TypeForm.RadioButton2.checked:=true;
  2     :       TypeForm.RadioButton3.checked:=true;
  3     :       TypeForm.RadioButton4.checked:=true;
  4     :       TypeForm.RadioButton5.checked:=true;
  5     :       TypeForm.RadioButton6.checked:=true;
  6     :       TypeForm.RadioButton7.checked:=true;
  7     :       TypeForm.RadioButton8.checked:=true;
  end;

  TypeForm.Showmodal;

  if oldtype=memrec[scrollbar1.position+(sender as TLabel).tag].VarType then exit;


  i:=0;
  j:=0;
  while (i<numberofrecords-1) and (j<1) do
  begin
    if selected[i] then inc(j);
    inc(i);
  end;

  if (i>1) then
  begin
    j:=memrec[scrollbar1.position+(sender as TLabel).tag].VarType;
    for i:=0 to numberofrecords-1 do
    begin
      if (selected[i]) and (not memrec[i].Frozen) then
        memrec[i].VarType:=j;
    end;
  end;

  resync;
  
  updatescreen;
  updatelist;
end;

procedure TMainForm.FControlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i,j: Integer;
    comfirm: Dword;
    doit: Boolean;
    freeze: Boolean;
    db: Byte;
    dw: Word;
    dd: dword;
    di64: Int64;
    a: single;
    b: double;
    error: Dword;

    controle: String;

    select: Integer;
    count: integer;

begin
  if ((key=ord('C')) and (ssCtrl in Shift) and not (ssAlt in Shift)) then copyselectedrecords;
  if ((key=ord('V')) and (ssCtrl in Shift) and not (ssAlt in Shift)) then Paste;
  if ((key=ord('X')) and (ssCtrl in Shift) and not (ssAlt in Shift)) then begin
                                                                            copyselectedrecords;
                                                                            deleterecords;
                                                                          end;
  case key of
    ord('A')..ord('Z'),ord('0')..ord('9'): begin
                          if not (ssCtrl in Shift) then
                          begin
                            select:=-1;
                            for i:=lastselected+1 to numberofrecords-1 do
                            begin
                              if (length(memrec[i].description[1])>0) and (uppercase(memrec[i].description[1])=uppercase(chr(key))) then
                              begin
                                select:=i;
                                break;
                              end;
                            end;

                            if select=-1 then exit;

                            if not ((ssShift) in (Shift)) then FirstShiftSelected:=-1;

                            if ssShift in Shift then  //ctrl does nothing
                            begin
                              if lastselected<numberofRecords-1 then
                              begin
                                if FirstShiftSelected=-1 then FirstShiftSelected:=lastselected;

                                //select everything from FirstShiftSelected to sel
                                for i:=0 to numberofrecords-1 do selected[i]:=false;

                                Lastselected:=select;

                                if FirstShiftSelected<lastselected then
                                  for i:=FirstShiftselected to lastselected do selected[i]:=true
                                else
                                  for i:=FirstShiftSelected downto lastselected do selected[i]:=true;
                              end;
                            end
                            else
                            begin
                              if lastselected<numberofRecords-1 then
                              begin
                                for i:=0 to numberofRecords-1 do selected[i]:=false;
                                Lastselected:=select;
                                selected[LastSelected]:=true;
                              end;
                            end;

                            if lastselected<scrollbar1.position then scrollbar1.Position:=lastselected;
                            if lastselected>scrollbar1.position+5 then scrollbar1.Position:=lastselected-5;

                           Updatescreen;
                          end;
                        end;

    VK_RETURN:begin
                value[lastselected-scrollbar1.position].OnDblClick(value[lastselected-scrollbar1.position]);
              end;

    VK_HOME:  begin
                if not ((ssShift) in (Shift)) then FirstShiftSelected:=-1;

                if ssShift in Shift then  //ctrl does nothing
                begin
                  if lastselected>0 then
                  begin
                    if FirstShiftSelected=-1 then FirstShiftSelected:=lastselected;

                    //select everything from FirstShiftSelected to sel
                    for i:=0 to numberofrecords-1 do selected[i]:=false;

                    Lastselected:=0;


                    if FirstShiftSelected<lastselected then
                      for i:=FirstShiftselected to lastselected do selected[i]:=true
                    else
                      for i:=FirstShiftSelected downto lastselected do selected[i]:=true;

                  end;

                  scrollbar1.position:=0;
                end
                else
                begin
                  if lastselected>0 then
                  begin
                    for i:=0 to numberofRecords-1 do selected[i]:=false;
                    Lastselected:=0;
                    selected[lastselected]:=true;
                    scrollbar1.Position:=0;
                  end;
                end;
                Updatescreen;
              end;

    VK_END:   begin
                 //
                if not ((ssShift) in (Shift)) then FirstShiftSelected:=-1;

                if ssShift in Shift then  //ctrl does nothing
                begin
                  if lastselected<numberofRecords-1 then
                  begin
                    if FirstShiftSelected=-1 then FirstShiftSelected:=lastselected;

                    //select everything from FirstShiftSelected to sel
                    for i:=0 to numberofrecords-1 do selected[i]:=false;

                    Lastselected:=numberofrecords-1;

                    if FirstShiftSelected<lastselected then
                      for i:=FirstShiftselected to lastselected do selected[i]:=true
                    else
                      for i:=FirstShiftSelected downto lastselected do selected[i]:=true;
                  end;
                end
                else
                begin
                  if lastselected<numberofRecords-1 then
                  begin
                    for i:=0 to numberofRecords-1 do selected[i]:=false;
                    Lastselected:=numberofrecords-1;
                    selected[LastSelected]:=true;
                  end;
                end;

                if lastselected<scrollbar1.position then scrollbar1.Position:=lastselected;
                if lastselected>scrollbar1.position+5 then scrollbar1.Position:=lastselected-5;

                Updatescreen;
              end;

    VK_UP:    begin
                if not ((ssShift) in (Shift)) then FirstShiftSelected:=-1;

                if ssShift in Shift then  //ctrl does nothing
                begin
                  if lastselected>0 then
                  begin
                    if FirstShiftSelected=-1 then FirstShiftSelected:=lastselected;

                    //select everything from FirstShiftSelected to sel
                    for i:=0 to numberofrecords-1 do selected[i]:=false;

                    Lastselected:=lastselected-1;


                    if FirstShiftSelected<lastselected then
                      for i:=FirstShiftselected to lastselected do selected[i]:=true
                    else
                      for i:=FirstShiftSelected downto lastselected do selected[i]:=true;

                  end;
                end
                else
                begin
                  if lastselected>0 then
                  begin
                    for i:=0 to numberofRecords-1 do selected[i]:=false;
                    Lastselected:=lastselected-1;
                    selected[lastselected]:=true;
                  end;
                end;

                if lastselected>scrollbar1.position+5 then scrollbar1.Position:=lastselected-5;
                if lastselected<scrollbar1.position then scrollbar1.Position:=lastselected;
                Updatescreen;

              end;

    VK_DOWN:  begin
                if not ((ssShift) in (Shift)) then FirstShiftSelected:=-1;

                if ssShift in Shift then  //ctrl does nothing
                begin
                  if lastselected<numberofRecords-1 then
                  begin
                    if FirstShiftSelected=-1 then FirstShiftSelected:=lastselected;

                    //select everything from FirstShiftSelected to sel
                    for i:=0 to numberofrecords-1 do selected[i]:=false;

                    Lastselected:=lastselected+1;

                    if FirstShiftSelected<lastselected then
                      for i:=FirstShiftselected to lastselected do selected[i]:=true
                    else
                      for i:=FirstShiftSelected downto lastselected do selected[i]:=true;
                  end;
                end
                else
                begin
                  if lastselected<numberofRecords-1 then
                  begin
                    for i:=0 to numberofRecords-1 do selected[i]:=false;
                    Lastselected:=Lastselected+1;
                    selected[LastSelected]:=true;
                  end;
                end;

                if lastselected<scrollbar1.position then scrollbar1.Position:=lastselected;
                if lastselected>scrollbar1.position+5 then scrollbar1.Position:=lastselected-5;

                Updatescreen;

              end;

    vk_prior:  begin  //page up
                if not ((ssShift) in (Shift)) then FirstShiftSelected:=-1;

                if ssShift in Shift then  //ctrl does nothing
                begin
                  if lastselected>0 then
                  begin
                    if FirstShiftSelected=-1 then FirstShiftSelected:=lastselected;

                    //select everything from FirstShiftSelected to sel
                    for i:=0 to numberofrecords-1 do selected[i]:=false;

                    if LastSelected>5 then
                      Lastselected:=lastselected-6
                    else
                      LastSelected:=0;



                    if FirstShiftSelected<lastselected then
                      for i:=FirstShiftselected to lastselected do selected[i]:=true
                    else
                      for i:=FirstShiftSelected downto lastselected do selected[i]:=true;

                  end;
                end
                else
                begin
                  if lastselected>0 then
                  begin
                    for i:=0 to numberofRecords-1 do selected[i]:=false;
                    if lastselected>5 then
                      Lastselected:=lastselected-6
                    else
                      LastSelected:=0;
                    selected[lastselected]:=true;
                  end;
                end;

                if lastselected<scrollbar1.position then scrollbar1.Position:=lastselected;
                if lastselected>scrollbar1.position+5 then scrollbar1.Position:=lastselected-5;

                Updatescreen;

               end;

    vk_next:   begin  //page down
                if not ((ssShift) in (Shift)) then FirstShiftSelected:=-1;

                if ssShift in Shift then  //ctrl does nothing
                begin
                  if lastselected<numberofRecords-1 then
                  begin
                    if FirstShiftSelected=-1 then FirstShiftSelected:=lastselected;

                    //select everything from FirstShiftSelected to sel
                    for i:=0 to numberofrecords-1 do selected[i]:=false;

                    if Lastselected+5<numberofrecords-1 then
                      Lastselected:=lastselected+5
                    else
                      Lastselected:=Numberofrecords-1;


                    if FirstShiftSelected<lastselected then
                      for i:=FirstShiftselected to lastselected do selected[i]:=true
                    else
                      for i:=FirstShiftSelected downto lastselected do selected[i]:=true;
                  end;
                end
                else
                begin
                  if lastselected<numberofRecords-1 then
                  begin
                    for i:=0 to numberofRecords-1 do selected[i]:=false;

                    if Lastselected+5>NumberOfRecords-1 then
                      Lastselected:=Numberofrecords-1
                    else
                      LastSelected:=Lastselected+5;

                    selected[LastSelected]:=true;
                  end;
                end;

                if lastselected<scrollbar1.position then scrollbar1.Position:=lastselected;
                if lastselected>scrollbar1.position+5 then scrollbar1.Position:=lastselected-5;

                Updatescreen;
               end;

    vk_delete: begin
                 Deletethisrecord1.Click;
               end;

    vk_space:  begin
                 //(un)freeze all selected addresses
                 //according to the lastselected one, AND if shift/ctrl is presses

                 freeze:=not memrec[lastselected].frozen;

                 count:=0;
                 for i:=0 to numberofrecords-1 do
                 begin
                   if selected[i] then
                   begin
                     doit:=((ssCtrl in shift) or (ssShift in shift)) or (lastselected=i);
                     if doit then
                     begin
                       inc(count,sizeof(send));
                       //if count>7000 then sleep(lag*3);
                       if not freeze then
                       begin
                         //unfreeze this address;
                         memrec[i].Frozen:=false;
                         output[0]:=CS_UNFREEZEADDRESS;
                         pword(@output[1])^:=i;
                         sendbuf(3);
                       end
                       else
                       begin  //freeze
                         output[0]:=CS_FREEZEADDRESS;
                         pword(@output[1])^:=i;
                         sendbuf(3);
                       end;
                     end else
                     begin
                       selected[i]:=false;
                       FirstShiftSelected:=lastselected;
                     end;




                   end;
                 end;

                 updatescreen;
                 updatelist;
               end;

  end;
  key:=0;

end;

procedure TMainForm.FControlKeyPress(Sender: TObject; var Key: Char);
begin
  key:=chr(0);
end;

procedure TMainForm.Copy1Click(Sender: TObject);
begin
  copyselectedrecords;
end;

procedure TMainForm.Cut1Click(Sender: TObject);
begin
  copyselectedrecords;
  deleterecords;
  updatescreen;
  updatelist;
end;

procedure TMainForm.Paste1Click(Sender: TObject);
begin
  Paste;
  updatescreen;
  updatelist;
end;

procedure TMainForm.Calculatenewvaluepart21Click(Sender: TObject);
var newaddress:dword;
    calculate: Integer;
    i: Integer;
    selectedi: Integer;
begin
  newaddress:=memrec[lastselected].address;
  if (foundlist.SelCount>0) then
    val('$'+foundlist.Items[foundlist.itemindex],newaddress,i);


  changeoffset:=TChangeOffset.create(self);
  changeoffset.FromAddress:=memrec[lastselected].address;
  changeoffset.toAddress:=NewAddress;
  if changeoffset.showmodal=mrCancel then exit;

  if changeoffset.error>0 then raise exception.Create('This is not an valid value');
  calculate:=changeoffset.offset;

  //first find out how many where selected.

  i:=0;
  selectedi:=0;
  while (i<numberofrecords) and (selectedi<2) do
  begin
    if selected[i] then inc(selectedi);
    inc(i);
  end;

  if selectedi>1 then
  begin  //change all selected items
    for i:=0 to Numberofrecords-1 do
      if selected[i] then inc(memrec[i].Address,calculate);

  end else
  begin //change all items
    for i:=0 to NumberOfRecords-1 do
      inc(memrec[i].Address,calculate);
  end;


  resync;
  updatescreen;
end;

procedure TMainForm.PopupMenu2Popup(Sender: TObject);
var i: Integer;
    selectedi: Integer;
    number: Integer;
    clip: TClipboard;
    inclipboard: boolean;
    temp: pchar;
begin
  inclipboard:=false;
  clip:=TClipboard.Create;
  if clip.HasFormat(cf_text) then
  begin
    getmem(temp,200);
    clip.GetTextBuf(temp,200);
    temp[9]:=chr(0);
    inclipboard:=(temp='CETables-');
    freemem(temp);
  end;

  clip.free;

  number:=0;
  i:=0;
  selectedi:=0;
  while (selectedi<2) and (i<numberofrecords) do
  begin
    if selected[i] then inc(selectedi);
    inc(i);
  end;

  if (numberofrecords=0) then
  begin
    DeleteThisRecord1.visible:=false;
    FreezeallAddresses2.visible:=false;
    UnfreezeAllAddresses1.visible:=false;
    sep1.Visible:=false;
    CalculateNewValuePart21.visible:=false;
    n4.visible:=false;
    cut1.Enabled:=false;
    copy1.Enabled:=false;
    paste1.Enabled:=inclipboard;

    groupoption1.Visible:=false;
  end;

  if (selectedi=0) and (numberofrecords=1) then
  begin
    DeleteThisRecord1.Enabled:=false;
    FreezeAllAddresses2.Visible:=true;
    FreezeAllAddresses2.Caption:='Freeze the address in this list!';
    UnfreezeAllAddresses1.Visible:=true;
    UnfreezeAllAddresses1.Caption:='Unfreeze the address in this list!';
    if memrec[0].Frozen then
    begin
      UnfreezeAllAddresses1.Enabled:=true;
      FreezeAllAddresses2.Enabled:=false;
    end
    else
    begin
      UnfreezeAllAddresses1.Enabled:=false;
      FreezeAllAddresses2.Enabled:=true;
    end;

    sep1.Visible:=true;
    CalculateNewValuePart21.Visible:=false;
    n4.Visible:=false;

    cut1.Enabled:=false;
    copy1.Enabled:=false;
    paste1.enabled:=inclipboard;

    N1.Visible:=false;
    groupoption1.visible:=false;


  end;

  if (selectedi=1) and (numberofrecords=1) then
  begin
    DeleteThisRecord1.Visible:=true;
    DeleteThisRecord1.Caption:='Delete this address';
    FreezeAllAddresses2.Visible:=true;
    FreezeAllAddresses2.Caption:='Freeze this address';
    UnfreezeAllAddresses1.Visible:=true;
    UnfreezeAllAddresses1.Caption:='Unfreeze this address';
    if memrec[0].Frozen then
    begin
      UnfreezeAllAddresses1.Enabled:=true;
      FreezeAllAddresses2.Enabled:=false;
    end
    else
    begin
      UnfreezeAllAddresses1.Enabled:=false;
      FreezeAllAddresses2.Enabled:=true;
    end;
    sep1.Visible:=true;
    CalculateNewValuePart21.Caption:='Change this address to the new address';
    CalculateNewValuePart21.Visible:=true;
    CalculateNewValuePart21.Enabled:=(foundlist.ItemIndex<>-1);
    N4.visible:=true;
    cut1.enabled:=true;
    copy1.enabled:=true;
    paste1.Enabled:=true;
    n1.visible:=true;
    groupoption1.visible:=true;

    Settonogroup1.Caption:='Remove from group '+IntToStr(memrec[lastselected].Group);
  end;


  if (selectedi=0) and (numberofrecords>1) then
  begin
    DeleteThisRecord1.Visible:=false;
    FreezeAllAddresses2.Visible:=true;
    FreezeAllAddresses2.Caption:='Freeze all addresses in this list!';
    UnfreezeAllAddresses1.Visible:=true;
    UnfreezeAllAddresses1.Caption:='Unfreeze all addresses in this list!';

    for i:=0 to numberofrecords-1 do
      if memrec[i].Frozen then inc(number);

    if number=0 then  //there is no frozen value so unfreeze is impossible
    begin
      UnfreezeAllAddresses1.Enabled:=false;
      FreezeAllAddresses2.Enabled:=true;
    end;

    if number=numberofrecords then //all addresses have been frozen
    begin
      UnfreezeAllAddresses1.Enabled:=true;
      FreezeAllAddresses2.Enabled:=false;
    end;

    if (number>0) and (number<numberofrecords) then
    begin
      UnfreezeAllAddresses1.Enabled:=true;
      freezeallAddresses2.Enabled:=true;
    end;


    sep1.Visible:=true;
    CalculateNewValuePart21.Visible:=false;
    n4.visible:=false;
    cut1.Enabled:=false;
    copy1.enabled:=false;
    paste1.enabled:=inclipboard;
    N1.enabled:=false;
    groupoption1.enabled:=false;

  end;

  if (selectedi=1) and (numberofrecords>1) then
  begin
    DeleteThisRecord1.Visible:=true;
    DeleteThisRecord1.Caption:='Delete this address';
    FreezeAllAddresses2.Visible:=true;
    FreezeAllAddresses2.Caption:='Freeze all addresses in this list';
    UnfreezeAllAddresses1.Visible:=true;
    unFreezeallAddresses1.Caption:='Unfreeze all addresses in this list';

    for i:=0 to numberofrecords-1 do
      if memrec[i].Frozen then inc(number);

    if number=0 then  //there is no frozen value so unfreeze is impossible
    begin
      UnfreezeAllAddresses1.Enabled:=false;
      FreezeAllAddresses2.Enabled:=true;
    end;

    if number=numberofrecords then //all addresses have been frozen
    begin
      UnfreezeAllAddresses1.Enabled:=true;
      FreezeAllAddresses2.Enabled:=false;
    end;

    if (number>0) and (number<numberofrecords) then
    begin
      UnfreezeAllAddresses1.Enabled:=true;
      freezeallAddresses2.Enabled:=true;
    end;

    sep1.Visible:=true;
    CalculateNewValuePart21.Caption:='Recalculate all addresses';
    CalculateNewValuePart21.Visible:=true;
    CalculateNewValuePart21.Enabled:=(foundlist.ItemIndex<>-1);

    n4.visible:=true;
    cut1.enabled:=true;
    copy1.Enabled:=true;
    paste1.Enabled:=inclipboard;
    N1.visible:=true;
    groupoption1.visible:=true;
  end;


  if (selectedi>1) and (numberofrecords>1) then
  begin
    DeleteThisRecord1.Visible:=true;
    DeleteThisRecord1.Caption:='Delete these addresses';
    FreezeAllAddresses2.Visible:=true;
    FreezeAllAddresses2.Caption:='Freeze all addresses in this list';
    UnFreezeAllAddresses1.Visible:=true;
    Unfreezealladdresses1.Caption:='Unfreeze all addresses in this list';
    for i:=0 to numberofrecords-1 do
      if memrec[i].Frozen then inc(number);

    if number=0 then  //there is no frozen value so unfreeze is impossible
    begin
      UnfreezeAllAddresses1.Enabled:=false;
      FreezeAllAddresses2.Enabled:=true;
    end;

    if number=numberofrecords then //all addresses have been frozen
    begin
      UnfreezeAllAddresses1.Enabled:=true;
      FreezeAllAddresses2.Enabled:=false;
    end;

    if (number>0) and (number<numberofrecords) then
    begin
      UnfreezeAllAddresses1.Enabled:=true;
      freezeallAddresses2.Enabled:=true;
    end;


    sep1.Visible:=true;
    calculatenewvaluepart21.Caption:='Recalculate selected addresses!';
    CalculateNewValuePart21.Visible:=true;
    CalculateNewValuePart21.Enabled:=(foundlist.ItemIndex<>-1);
    N4.Visible:=true;
    cut1.enabled:=true;
    copy1.enabled:=true;
    paste1.enabled:=inclipboard;
    N1.visible:=true;
    groupoption1.visible:=true;
  end;

end;

procedure TMainForm.SettogroupXClick(Sender: TObject);
var groupnr: Integer;
    i: Integer;
begin
  //set all selected items to group (Sender as TmenuItem).tag
  GroupNr:=(Sender as TMenuItem).Tag;
  for i:=0 to numberofrecords-1 do if selected[i] then memrec[i].group:=Groupnr;
end;


procedure TMainForm.Settonogroup1Click(Sender: TObject);
begin
  memrec[lastselected].Group:=0;
end;

procedure TMainForm.Freezealladdresses2Click(Sender: TObject);
var i,j: Integer;
    multiple: Boolean;
    count: integer;
begin
  j:=0;
  i:=0;
  while (j<2) and (i<numberofrecords) do
  begin
    if selected[i] then inc(j);
    inc(i);
  end;

  multiple:=(j>1);

  count:=0;
  for i:=0 to numberofrecords-1 do
  begin
    if not memrec[i].Frozen then
    begin
      //
      if (not multiple) or (multiple and selected[i]) then
      begin
        output[0]:=CS_FreezeAddress;
        pword(@output[1])^:=i;
        sendbuf(3);

      end;
    end;
  end;

  updatescreen;
end;


procedure TMainForm.Unfreezealladdresses1Click(Sender: TObject);
var i: Integer;
begin
  for i:=0 to numberofrecords-1 do
    memrec[i].Frozen:=false;

  resync;
  updatelist;
  updatescreen;
end;

procedure TMainForm.Panel1Resize(Sender: TObject);
var i: integer;
    previouscount: integer;
    changed:boolean;
begin

  //now addjust the numberoflines and create rows if needed
  //every row is 16 pixels so just do
  previouscount:=numberoflines;
  numberoflines:=(panel3.height div 16) +1;

  if length(select) < numberoflines then
  begin
    //make new lines
    setlength(frozenbox,numberoflines);
    setlength(description,numberoflines);
    setlength(address,numberoflines);
    setlength(ValType,numberoflines);
    setlength(Value,numberoflines);
    setlength(Select,numberoflines);

    for i:=previouscount to numberoflines-1 do
    begin
      //checkbox
      select[i]:=Tlabel.Create(self);

      select[i].AutoSize:=false;
      select[i].Color:=select[0].Color;
      select[i].Top:=i*16;
      select[i].Left:=0;
      select[i].Width:=panel3.Width;
      select[i].Height:=16;
      select[i].Anchors:=[akLeft,akTop,akRight];

      select[i].Caption:=select[0].Caption;
      select[i].Transparent:=false;
      select[i].Visible:=true;
      select[i].tag:=i;
      select[i].SendToBack;
      select[i].Font:=select[0].Font;

      select[i].ParentColor:=false;
      select[i].ParentFont:=false;
      select[i].OnDragDrop:=Panel1DragDrop;
      select[i].OnDragOver:=panel1DragOver;
      select[i].OnMouseDown:=SlectItem; //damn spell error
      select[i].PopupMenu:=Popupmenu2;
      select[i].Parent:=panel3;


      frozenbox[i]:=tcheckbox.Create(self);
      frozenbox[i].Caption:='';
      frozenbox[i].Top:=i*16;
      frozenbox[i].Left:=24;
      frozenbox[i].Width:=17;
      frozenbox[i].Visible:=false;
      frozenbox[i].Tag:=i;
      frozenbox[i].Cursor:=crHandPoint;
      frozenbox[i].OnClick:=CheckBox1Click;
      frozenbox[i].OnDragDrop:=Panel1DragDrop;
      frozenbox[i].OnDragOver:=Panel1DragOver;
      frozenbox[i].OnMouseDown:=CheckBox2MouseDown;
      frozenbox[i].PopupMenu:=Popupmenu2;
      frozenbox[i].Parent:=panel3;

      description[i]:=Tlabel.Create(self);
      description[i].Font:=description[0].Font;
      description[i].Caption:='You shouldn''t be able to see this';
      description[i].Top:=i*16;
      description[i].Left:=64;
      description[i].Visible:=false;
      description[i].Tag:=i;
      description[i].transparent:=true;
      description[i].Cursor:=crHandPoint;
      description[i].OnDblClick:=Label28Click;
      description[i].OnDragDrop:=Panel1DragDrop;
      description[i].OnDragOver:=panel1DragOver;
      description[i].OnMouseDown:=Slectitem;

      description[i].PopupMenu:=popupmenu2;
      description[i].Parent:=panel3;

      address[i]:=Tlabel.Create(self);
      address[i].Top:=i*16;
      address[i].Left:=226;
      address[i].Visible:=false;
      address[i].Tag:=i;
      address[i].Transparent:=true;
      address[i].Cursor:=crHandPoint;
      address[i].OnDblClick:=Addressclick;
      address[i].OnDragDrop:=Panel1DragDrop;
      address[i].OnDragOver:=panel1DragOver;
      address[i].OnMouseDown:=Slectitem;

      address[i].PopupMenu:=popupmenu2;
      address[i].Parent:=panel3;

      ValType[i]:=Tlabel.Create(self);
      ValType[i].Top:=i*16;
      ValType[i].Left:=293;
      valtype[i].Alignment:=taCenter;
      valtype[i].AutoSize:=false;
      valtype[i].Width:=54;
      ValType[i].Visible:=false;
      valtype[i].Tag:=i;
      valtype[i].Transparent:=true;
      valtype[i].Cursor:=crHandPoint;
      valtype[i].OnDblClick:=TypeClick;
      valtype[i].OnDragDrop:=Panel1DragDrop;
      valtype[i].OnDragOver:=panel1DragOver;
      valtype[i].OnMouseDown:=Slectitem;
      valtype[i].PopupMenu:=popupmenu2;
      ValType[i].Parent:=panel3;


      Value[i]:=Tlabel.Create(self);
      Value[i].Top:=i*16;
      Value[i].Left:=351;
      Value[i].Visible:=false;
      value[i].Tag:=i;
      value[i].Transparent:=true;
      value[i].Cursor:=crHandPoint;
      value[i].OnDblClick:=valueclick;
      value[i].OnDragDrop:=Panel1DragDrop;
      value[i].OnDragOver:=panel1DragOver;
      value[i].OnMouseDown:=Slectitem;
      value[i].PopupMenu:=popupmenu2;
      Value[i].Parent:=panel3;
    end;
  end;

  updatescreen;
end;

procedure TMainForm.Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  accept:=(source is TListbox);
end;

procedure TMainForm.Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  speedbutton3.click;
end;

procedure TMainForm.slectItem(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var sel: Integer;
    i: Integer;
begin
  FControl.SetFocus;

//only react to one click
 // if sender=Label38 then exit;


  sel:=0;
  if sender is TLabel then
  begin
    sel:=(Sender as TLabel).Tag+scrollbar1.Position;
    if sel>numberofrecords-1 then
    begin
      if not ((ssSHift in Shift) or (ssCtrl in Shift) or (ssRight in Shift)) then
      begin
        for i:=0 to numberofrecords-1 do
        begin
          selected[i]:=false;
        end;
        updatescreen;
      end;

      exit;
    end;
  end;

  if sender is TCheckBox then
    sel:=(Sender as TCheckBox).Tag+scrollbar1.Position;

  if not ((ssShift) in (Shift)) then FirstShiftSelected:=-1;


  if ssLeft in Shift then
  begin
    if not (ssShift in Shift) and not (ssCtrl in Shift) then
    begin
      for i:=0 to numberofrecords-1 do
        selected[i]:=false;
    end;

    if (sel>=numberofrecords) then
    begin
      updatelist;
      updatescreen;
      exit;
    end;

    if ssShift in shift then
    begin

      if FirstShiftSelected=-1 then FirstShiftSelected:=lastselected;

      //select everything from FirstShiftSelected to sel
      for i:=0 to numberofrecords-1 do selected[i]:=false;

      if FirstShiftSelected<sel then
        for i:=FirstShiftselected to sel do selected[i]:=true
      else
        for i:=FirstShiftSelected downto sel do selected[i]:=true;

      lastselected:=sel;

      updatelist;
      updatescreen;
      exit;
    end;


    if ssCtrl in shift then
    begin
      selected[sel]:=not selected[sel];
      if selected[sel] then lastselected:=sel;
      updatelist;
      updatescreen;
      exit;
    end;


    selected[sel]:=true;

  end;

  if ssRight in shift then
  begin
    if (sel>=numberofrecords) then
    begin
      updatelist;
      updatescreen;
      exit;
    end;

    if not selected[sel] then //clear all other selected items except this one
    begin
      for i:=0 to numberofrecords-1 do selected[i]:=false;
    end;

    selected[sel]:=true;

  end;

  lastselected:=sel;

  Updatescreen;
end;

procedure TMainForm.CheckBox2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var error: dword;

    a: single;
    b: double;
    controle: String;

    i,j,k,sel: Integer;
    db: Byte;
    dw: Word;
    dd: dword;
    di64: Int64;

    read8: array of byte;
    read9: pbyte;

    freeze: boolean;
    freezegroup: array [1..4] of boolean;
    temps: pchar;

    temp: string;
begin
  FControl.SetFocus;

  for i:=1 to 4 do
    freezegroup[i]:=false;

  if ssRight in Shift then
  begin
    if not selected[(sender as TCheckBox).Tag+scrollbar1.Position] then //clear all other selected items except this one
    begin
      for i:=0 to numberofrecords-1 do
        selected[i]:=false;
    end;
  end;

  if not((ssShift in Shift) or (ssCtrl in Shift)) then
  begin
    for i:=0 to numberofrecords-1 do
      selected[i]:=false;
  end;

  if not ((ssShift) in (Shift)) then FirstShiftSelected:=-1;

  sel:=(sender as TCheckBox).Tag+scrollbar1.Position;

  if ssShift in shift then
  begin
    if FirstShiftSelected=-1 then FirstShiftSelected:=lastselected;

    //select everything from FirstShiftSelected to sel
    for i:=0 to numberofrecords-1 do selected[i]:=false;

    if FirstShiftSelected<sel then
      for i:=FirstShiftselected to sel do selected[i]:=true
    else
      for i:=FirstShiftSelected downto sel do selected[i]:=true;

    lastselected:=sel;
  end;



  selected[(sender as TCheckBox).Tag+scrollbar1.Position]:=true;
  lastselected:=(sender as TCheckBox).Tag+scrollbar1.Position;

  freeze:=not frozenbox[(sender as TCheckbox).Tag].checked;

  //find groups to be frozen/unfrozen
  for i:=0 to numberofrecords-1 do
  begin
    if (memrec[i].Group>0) and (selected[i]) then
      freezegroup[memrec[i].Group]:=true;  //freezegroup can also be used to unfreeze
  end;

  for i:=0 to numberofrecords-1 do
  begin
    if freeze then //freeze all selected items
    begin
      if (selected[i] and not memrec[i].Frozen) or
         ( freezegroup[(memrec[i].Group)] and (memrec[i].group>0))
      then //if it's selected and not yet frozen freeze it.
      begin
        //freeze that address
        output[0]:=CS_FreezeAddress;
        pword(@output[1])^:=i;
        sendbuf(3);

        memrec[i].Frozen:=true;
      end;
    end
    else
    begin
      {freeze is false, so UNFREEZE all selected items and groups}
      if selected[i] then
      begin
        output[0]:=CS_UnfreezeAddress;
        pword(@output[1])^:=i;
        sendbuf(3);

        memrec[i].frozen:=false;
      end;

      if (memrec[i].group>0) and (freezegroup[memrec[i].group]) then
      begin
        output[0]:=CS_UnfreezeAddress;
        pword(@output[1])^:=i;
        sendbuf(3);

        memrec[i].frozen:=false;
      end;
    end;
  end;

  UpdateScreen;
end;


procedure TMainForm.ScrollBar1Enter(Sender: TObject);
begin
  fcontrol.SetFocus;
end;

procedure TMainForm.SpeedButton4Click(Sender: TObject);
begin
  if formsettings.showmodal<>mrok then
  begin
    LoadSettingsFromRegistry;
    exit;
  end;
  //send new options to the server
  output[0]:=CS_SetTimerSpeed;
  pword(@output[1])^:=updatetimer.Interval;
  pword(@output[3])^:=freezetimer.Interval;
  sendbuf(5);

  output[0]:=CS_SetConfig; //ShowAsSigned:byte BinariesAsDecimal:byte max:word; buffersize:dword;skip_page_no_cache: byte;UseDebugRegs:byte;stealthusermode:byte;stealthkernelmode:byte
  if formsettings.cbShowAsSigned.checked then output[1]:=1 else output[1]:=0;
  if formsettings.cbBinariesAsDecimal.checked then output[2]:=1 else output[2]:=0;

  pword(@output[3])^:=mainform.max;
  pdword(@output[5])^:=mainform.buffersize;

  if Skip_PAGE_NOCACHE then output[9]:=1 else output[9]:=0;
  if formsettings.rbDebugRegisters.checked then output[10]:=1 else output[10]:=0;


  if formsettings.cbKernelQueryMemoryRegion.checked then output[11]:=1 else output[11]:=0;
  if formsettings.cbKernelReadWriteProcessMemory.checked then output[12]:=1 else output[12]:=0;
  if formsettings.cbKernelOpenProcess.Checked then output[13]:=1 else output[13]:=0;

  if formsettings.cbstealth.checked then output[14]:=1 else output[14]:=0;
  if formsettings.cbprotectme.checked then output[15]:=1 else output[15]:=0;


  sendbuf(16);
end;

procedure TMainForm.FormShow(Sender: TObject);
var reg: TRegistry;
    i: integer;
    go:boolean;
begin
  LoadSettingsFromRegistry;

  output[0]:=CS_SETCONFIG;

  if formsettings.cbShowAsSigned.checked then output[1]:=1 else output[1]:=0;
  if formsettings.cbBinariesAsDecimal.checked then output[2]:=1 else output[2]:=0;

  pword(@output[3])^:=max;
  pdword(@output[5])^:=buffersize;
  if unit2.Skip_PAGE_NOCACHE then output[9]:=1 else output[9]:=0;
  if formsettings.rbDebugRegisters.checked then output[10]:=1 else output[10]:=0;

  if formsettings.cbKernelQueryMemoryRegion.checked then output[11]:=1 else output[11]:=0;
  if formsettings.cbKernelReadWriteProcessMemory.checked then output[12]:=1 else output[12]:=0;
  if formsettings.cbKernelOpenProcess.Checked then output[13]:=1 else output[13]:=0;
  if formsettings.cbstealth.checked then output[14]:=1 else output[14]:=0;
  if formsettings.cbprotectme.checked then output[15]:=1 else output[15]:=0;

  sendbuf(16);

  //send update and freeze speed
  output[0]:=CS_SetTimerSpeed;
  pword(@output[1])^:=updatetimer.Interval;
  pword(@output[3])^:=freezetimer.Interval;
  sendbuf(5);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  if memorybrowser=nil then memorybrowser:=TMemorybrowser.Create(self);
  memorybrowser.show;
end;

procedure TMainForm.Browsethismemoryarrea1Click(Sender: TObject);
var a: dword;
    b: integer;
begin
  val('$'+Foundlist.items[Foundlist.itemindex],a,b);

  if memorybrowser=nil then memorybrowser:=tmemorybrowser.Create(self);

  MemoryBrowser.memoryaddress:=a;
  memorybrowser.show;
end;

procedure TMainForm.Browsethismemoryregion1Click(Sender: TObject);
begin
  if memorybrowser=nil then memorybrowser:=tmemorybrowser.Create(self);
  MemoryBrowser.memoryaddress:=memrec[lastselected].address;
  memorybrowser.show;
end;

procedure TMainForm.scanvalueKeyPress(Sender: TObject; var Key: Char);
var correct: boolean;
    becomes: string;
    i,j: integer;
begin
  correct:=false;

  if key=chr(13) then
  begin
    if nextscanbutton.Enabled then nextscanbutton.Click else newscan.Click;
    key:=#0;
    exit;
  end;

  if key=chr(8) then correct:=true else
  if key=chr(16) then correct:=true else
  case vartype.ItemIndex of
  0:   begin  //bit
         if rbbit.checked then
         begin
           if key='0' then correct:=true else
           if key='1' then correct:=true else
           if key='*' then correct:=true else
           if key='?' then correct:=true;
         end else
         begin
           case key of
             '0'..'9' : correct:=true;
           end;
         end;
       end;

  1,2,3,4: begin
           if HexadecimalCheckbox.checked then
             case key of
               '0'..'9' : correct:=true;
               'A'..'F' : correct:=true;
               'a'..'f' : correct:=true;
               '-' : correct:=true;
             end else
             case key of
               '0'..'9' : correct:=true;
               '-' : correct:=true;
             end;
           end;

  5,6,7:   correct:=true; //stupid language settings....

  8:       begin
             becomes:=scanvalue.text;
             if HexadecimalCheckbox.checked then
             begin
               case key of
                 '0'..'9' : correct:=true;
                 'A'..'F' : correct:=true;
                 'a'..'f' : correct:=true;
                 '?','*' : correct:=true;
                 ' ','-'  : correct:=true;  //space and - are the seperators
               end;
             end
             else
             begin
               case key of
                 '0'..'9' : correct:=true;
                 '?','*' : correct:=true;
                 ' ','-'  : correct:=true;
               end;
             end;
           end;

  end;

  if (key='-') and ((scantype.itemindex=1) or (scantype.itemindex=3)) then correct:=false;
  if not correct then key:=chr(0);
end;

procedure TMainForm.CheckBox8Click(Sender: TObject);
begin
  output[0]:=CS_SetHyperscanState;
  if checkbox8.Checked then output[1]:=1 else output[1]:=0;
  sendbuf(2);
end;

procedure TMainForm.cbSpeedhackClick(Sender: TObject);
var speed: dword;
    sleeptime: dword;
begin
//  CS_EnableSpeedhack=20; //(direction:byte;speed:dword;sleeptime:dword)
//  CS_DisableSpeedhack=21; //
  if cbspeedhack.checked then
  begin
    cbspeedhack.Enabled:=false;
    cbspeedhack.Cursor:=crhourglass;
    btnSetSpeedhack.Click;
  end
  else
  begin
    output[0]:=cs_disablespeedhack;
    sendbuf(1);
  end;
end;

procedure TMainForm.btnSetSpeedhackClick(Sender: TObject);
var speedf: single;
    speed: dword absolute speedf;
    sleeptime: dword;
    speedtext: string;
    i: integer;
begin
  try
    speedtext:=edit2.Text;
    speedf:=strtofloat(speedtext);
  except
    try
      if pos(',',speedtext)<>0 then
      begin
        //replace the , with a .
        i:=pos(',',speedtext);
        while i<>0 do
        begin
          speedtext[i]:='.';
          i:=pos(',',speedtext);
        end;
      end
      else
      begin
        //replace the . with a ,
        i:=pos('.',speedtext);
        while i<>0 do
        begin
          speedtext[i]:=',';
          i:=pos('.',speedtext);
        end;
      end;

      speedf:=strtofloat(speedtext);
    except
      raise exception.Create('The speed value is incorrect');
    end;
  end;

  sleeptime:=strtoint(edit1.text);

  if speedf<=0 then raise exception.Create('I can''t set this speed. (must be bigger than 0)');
  if speedf*sleeptime<1 then
  begin
    try
      sleeptime:=trunc(roundto(1/speedf,-1));
      if sleeptime=0 then sleeptime:=1;

      edit1.Text:=IntToStr(sleeptime);
    except
      exception.Create('I can''t set the speed');
    end;
  end;


  if cbspeedhack.checked then
  begin
    output[0]:=CS_EnableSpeedhack;
    psingle(@output[1])^:=speedf;
    pdword(@output[5])^:=sleeptime;

    sendbuf(9);
  end
end;

procedure TMainForm.RadioButton3Click(Sender: TObject);
begin
  FromAddress.text:='00000000';
  ToAddress.text:='FFFFFFFF';
end;

procedure TMainForm.Setbreakpoint1Click(Sender: TObject);
var address: dword;
begin
  if not startdebuggerifneeded then exit;
  address:=memrec[lastselected].Address;

  if memrec[lastselected].VarType=0 then SetWriteBreakpoint(address,1); //byte
  if memrec[lastselected].VarType=1 then SetWriteBreakpoint(address,2); //word
  if memrec[lastselected].VarType=2 then SetWriteBreakpoint(address,4); //dword
  if memrec[lastselected].VarType=3 then SetWriteBreakpoint(address,4); //single
  if memrec[lastselected].VarType=4 then SetWriteBreakpoint(address,8); //double
  if memrec[lastselected].VarType=5 then SetWriteBreakpoint(address,1+(memrec[lastselected].Bit+memrec[lastselected].bitlength) div 8); //bit(will be handled as a byte)
  if memrec[lastselected].VarType=6 then SetWriteBreakpoint(address,8); //8-bytes (qword)
  if memrec[lastselected].VarType=7 then SetWriteBreakpoint(address,memrec[lastselected].Bit); //length of the text
  if memrec[lastselected].VarType=8 then SetWriteBreakpoint(address,memrec[lastselected].Bit); //length of the array
end;

procedure TMainForm.Findoutwhatreadsfromthisaddress1Click(Sender: TObject);
var address: dword;
begin
  if not startdebuggerifneeded then exit;
  address:=memrec[lastselected].Address;

  if memrec[lastselected].VarType=0 then SetReadBreakpoint(address,1); //byte
  if memrec[lastselected].VarType=1 then SetReadBreakpoint(address,2); //word
  if memrec[lastselected].VarType=2 then SetReadBreakpoint(address,4); //dword
  if memrec[lastselected].VarType=3 then SetReadBreakpoint(address,4); //single
  if memrec[lastselected].VarType=4 then SetReadBreakpoint(address,8); //double
  if memrec[lastselected].VarType=5 then SetReadBreakpoint(address,1+(memrec[lastselected].Bit+memrec[lastselected].bitlength) div 8); //bit(will be handled as a byte)
  if memrec[lastselected].VarType=6 then SetReadBreakpoint(address,8); //8-bytes (qword)
  if memrec[lastselected].VarType=7 then SetReadBreakpoint(address,memrec[lastselected].Bit); //length of the text
  if memrec[lastselected].VarType=8 then SetReadBreakpoint(address,memrec[lastselected].Bit); //length of the array
end;


procedure TMainForm.Findoutwhataccessesthisaddress1Click(Sender: TObject);
var address: dword;
begin
  if not startdebuggerifneeded then exit;
  address:=memrec[lastselected].Address;

  if memrec[lastselected].VarType=0 then SetReadWriteBreakpoint(address,1); //byte
  if memrec[lastselected].VarType=1 then SetReadWriteBreakpoint(address,2); //word
  if memrec[lastselected].VarType=2 then SetReadWriteBreakpoint(address,4); //dword
  if memrec[lastselected].VarType=3 then SetReadWriteBreakpoint(address,4); //single
  if memrec[lastselected].VarType=4 then SetReadWriteBreakpoint(address,8); //double
  if memrec[lastselected].VarType=5 then SetReadWriteBreakpoint(address,1+(memrec[lastselected].Bit+memrec[lastselected].bitlength) div 8); //bit(will be handled as a byte)
  if memrec[lastselected].VarType=6 then SetReadWriteBreakpoint(address,8); //8-bytes (qword)
  if memrec[lastselected].VarType=7 then SetReadWriteBreakpoint(address,memrec[lastselected].Bit); //length of the text
  if memrec[lastselected].VarType=8 then SetReadWriteBreakpoint(address,memrec[lastselected].Bit); //length of the array
end;


procedure TMainForm.SetWriteBreakpoint(address:dword;size: integer);
var ar: dword;
    buf: pointer;
    output: array [0..6] of byte;
resourcestring strOpcodeChanged='The following opcodes changed the selected address';
begin
  ar:=0;
  getmem(buf,size);
  try
    ReadProcessmemory(processhandle,pointer(address),buf,size,ar);

    if ar<size then raise exception.Create('Not all bytes of this address are readable');

    //no problems, so lets do it
    output[0]:=CS_FindWhatWrites;
    pdword(@output[1])^:=address;
    pword(@output[5])^:=size;

    foundcodedialog:=Tfoundcodedialog.Create(self);
    foundcodedialog.Caption:=strOpcodeChanged;
    foundcodedialog.btnOK.caption:='Stop';

    connectform.IdTCPClient1.WriteBuffer(output[0],7);
    foundcodedialog.showmodal;
  finally
    freemem(buf);
  end;
end;

procedure TMainForm.SetReadBreakpoint(address:dword;size: integer);
var ar: dword;
    buf: pointer;
    output: array [0..6] of byte;
resourcestring strOpcodeRead='The following opcodes read the selected address';
begin
  ar:=0;
  getmem(buf,size);
  try
    ReadProcessmemory(processhandle,pointer(address),buf,size,ar);

    if ar<size then raise exception.Create('Not all bytes of this address are readable');

    //no problems, so lets do it
    output[0]:=CS_FindWhatReads;
    pdword(@output[1])^:=address;
    pword(@output[5])^:=size;

    foundcodedialog:=Tfoundcodedialog.Create(self);
    foundcodedialog.Caption:=strOpcodeRead;
    foundcodedialog.btnOK.caption:='Stop';

    connectform.IdTCPClient1.WriteBuffer(output[0],7);
    foundcodedialog.showmodal;
  finally
    freemem(buf);
  end;
end;

procedure TMainForm.SetReadWriteBreakpoint(address:dword;size: integer);
var ar: dword;
    buf: pointer;
    output: array [0..6] of byte;
resourcestring stropcodeaccessed='The following opcodes accessed the selected address';
begin
  ar:=0;
  getmem(buf,size);
  try
    ReadProcessmemory(processhandle,pointer(address),buf,size,ar);

    if ar<size then raise exception.Create('Not all bytes of this address are readable');

    //no problems, so lets do it
    output[0]:=CS_FindWhatAccesses;
    pdword(@output[1])^:=address;
    pword(@output[5])^:=size;

    foundcodedialog:=Tfoundcodedialog.Create(self);
    foundcodedialog.Caption:=strOpcodeAccessed;
    foundcodedialog.btnOK.caption:='Stop';

    connectform.IdTCPClient1.WriteBuffer(output[0],7);
    foundcodedialog.showmodal;
  finally
    freemem(buf);
  end;
end;



procedure TMainForm.advancedbuttonClick(Sender: TObject);
begin
  advancedoptions.show;
end;

end.
