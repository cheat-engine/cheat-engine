unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ComCtrls, StdCtrls,  Menus, CEFuncproc, Buttons,shellapi,
  ExtCtrls, Dialogs, Clipbrd,debugger,debugger2, assemblerunit,
  registry,{xpman,}math,hexeditor, Gauges, ImgList,commctrl,NewKernelHandler,
  hotkeyhandler,tlhelp32,undochanges,winsvc,imagehlp,unrandomizer,symbolhandler,
  ActnList,hypermode,autoassembler,injectedpointerscanunit,plugin,savefirstscan,
  foundlisthelper,disassembler, underc, psapi, peinfounit, PEInfoFunctions, memscan;

  //the following are just for compatibility

 

const copypasteversion=4;
const wm_freedebugger=WM_USER+1;
const wm_scandone=WM_USER+2;


type TFlash = class (TThread)
  public
    procedure Execute; override;
    procedure col;
end;


type TToggleWindows = class(TThread)
  private
  public
    constructor Create(CreateSuspended: Boolean);
    procedure Execute; override;
  end;


type grouptype = array[1..6] of boolean;

type TFcontrol= class( TEdit)
  private
  public
    procedure wmMouseWheel (var Msg : TWMMouseWheel); message wm_MouseWheel;

  end;


type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    SortByFrozenButton: TButton;
    SortByDescriptionButton: TButton;
    SortByAddressButton: TButton;
    SortByTypeButton: TButton;
    SortByValueButton: TButton;
    UpdateTimer: TTimer;
    FreezeTimer: TTimer;
    PopupMenu2: TPopupMenu;
    Deletethisrecord1: TMenuItem;
    Browsethismemoryregion1: TMenuItem;
    Calculatenewvaluepart21: TMenuItem;
    Freezealladdresses2: TMenuItem;
    sep1: TMenuItem;
    Unfreezealladdresses1: TMenuItem;
    N1: TMenuItem;
    Groupoption1: TMenuItem;
    Settogroup11: TMenuItem;
    Settogroup21: TMenuItem;
    Settogroup31: TMenuItem;
    Settogroup41: TMenuItem;
    Settonogroup1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    Sortaddressesbygroup1: TMenuItem;
    N4: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Cut1: TMenuItem;
    Setbreakpoint1: TMenuItem;
    SetHotkey1: TMenuItem;
    Findoutwhatreadsfromthisaddress1: TMenuItem;
    N5: TMenuItem;
    Panel4: TPanel;
    advancedbutton: TSpeedButton;
    Label7: TLabel;
    CommentButton: TSpeedButton;
    Panel5: TPanel;
    ProcessLabel: TLabel;
    foundcountlabel: TLabel;
    ScanText: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    LoadButton: TSpeedButton;
    SaveButton: TSpeedButton;
    Label6: TLabel;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Dos: TRadioButton;
    Windows: TRadioButton;
    Readonly: TCheckBox;
    FromAddress: TMemo;
    ToAddress: TMemo;
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
    Splitter1: TSplitter;
    SpeedButton4: TSpeedButton;
    cbCaseSensitive: TCheckBox;
    cbFastScan: TCheckBox;
    Timer1: TTimer;
    btnShowRegions: TButton;
    Foundlist3: TListView;
    Findoutwhataccessesthisaddress1: TMenuItem;
    Showashexadecimal1: TMenuItem;
    Trytofindbasepointer1: TMenuItem;
    Settogroup42: TMenuItem;
    Settogroup51: TMenuItem;
    cbFasterScan: TCheckBox;
    AllClick: TRadioButton;
    Panel7: TPanel;
    SpeedButton1: TSpeedButton;
    cbPauseWhileScanning: TCheckBox;
    btnSetSpeedhack: TButton;
    Label51: TLabel;
    Label52: TLabel;
    Edit2: TEdit;
    Edit1: TEdit;
    cbSpeedhack: TCheckBox;
    Change1: TMenuItem;
    Description1: TMenuItem;
    Address1: TMenuItem;
    ype1: TMenuItem;
    Value1: TMenuItem;
    LabelModifiedmem: TLabel;
    Timer4: TTimer;
    pnlFloat: TPanel;
    rt3: TRadioButton;
    rt1: TRadioButton;
    rt2: TRadioButton;
    cbUnicode: TCheckBox;
    cbUnrandomizer: TCheckBox;
    Changescript1: TMenuItem;
    ActionList1: TActionList;
    actSave: TAction;
    actOpen: TAction;
    ImageList1: TImageList;
    actAutoAssemble: TAction;
    Forcerechecksymbols1: TMenuItem;
    Label5: TLabel;
    Label38: TLabel;
    Smarteditaddresses1: TMenuItem;
    Label53: TLabel;
    Pointerscanforthisaddress1: TMenuItem;
    Label55: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Plugins1: TMenuItem;
    Label59: TLabel;
    UpdateFoundlisttimer: TTimer;
    mode16: TCheckBox;
    Browsethismemoryregioninthedisassembler1: TMenuItem;
    AutoAttachTimer: TTimer;
    Button2: TButton;
    Button4: TButton;
    cbNewscanroutine: TCheckBox;
    LogoPanel: TPanel;
    Logo: TImage;
    ScrollBox1: TScrollBox;
    HeaderControl1: THeaderControl;
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
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    vscrollpanel: TPanel;
    ScrollBar1: TScrollBar;
    procedure ShowProcessListButtonClick(Sender: TObject);
    procedure NewScanClick(Sender: TObject);
    procedure NextScanButtonClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AddressKeyPress(Sender: TObject; var Key: Char);
    procedure Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FoundListDblClick(Sender: TObject);
    procedure Browsethismemoryarrea1Click(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure FreezeTimerTimer(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure Label28Click(Sender: TObject);
    procedure AddressClick(Sender: TObject);
    procedure TypeClick(Sender: TObject);
    procedure ValueClick(Sender: TObject);
    procedure Browsethismemoryregion1Click(Sender: TObject);
    procedure Label24ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure Deletethisrecord1Click(Sender: TObject);
    procedure SortByFrozenButtonClick(Sender: TObject);
    procedure SortByDescriptionButtonClick(Sender: TObject);
    procedure SortByAddressButtonClick(Sender: TObject);
    procedure SortByTypeButtonClick(Sender: TObject);
    procedure SortByValueButtonClick(Sender: TObject);
    procedure ScanvalueoldKeyPress(Sender: TObject; var Key: Char);
    procedure Calculatenewvaluepart21Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ScanTypeChange(Sender: TObject);
    procedure VarTypeChange(Sender: TObject);
    procedure LogoClick(Sender: TObject);
    procedure DosClick(Sender: TObject);
    procedure WindowsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Selectallitems1Click(Sender: TObject);
    procedure Label37Click(Sender: TObject);
    procedure Freezealladdresses2Click(Sender: TObject);
    procedure SlectItem(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure CheckBox2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBox1Click(Sender: TObject);
    procedure Unfreezealladdresses1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Removeselectedaddresses1Click(Sender: TObject);
    procedure FControlKeyPress(Sender: TObject; var Key: Char);
    procedure FControlKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ScrollBar1Enter(Sender: TObject);
    procedure FControlExit(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure FControlEnter(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CommentButtonClick(Sender: TObject);
    procedure CommentButtonMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SettogroupXClick(Sender: TObject);
    procedure Settonogroup1Click(Sender: TObject);
    procedure SetPrioritys1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Sortaddressesbygroup1Click(Sender: TObject);
    procedure Setbreakpoint1Click(Sender: TObject);
    procedure TopDisablerTimer(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure advancedbuttonClick(Sender: TObject);
    procedure HexadecimalCheckboxClick(Sender: TObject);
    procedure SetHotkey1Click(Sender: TObject);
    procedure Findoutwhatreadsfromthisaddress1Click(Sender: TObject);
    procedure UndoScanClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbBitClick(Sender: TObject);
    procedure rbDecClick(Sender: TObject);
    procedure Cut2Click(Sender: TObject);
    procedure Copy2Click(Sender: TObject);
    procedure Paste2Click(Sender: TObject);
    procedure ccpmenuPopup(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure Splitter1Moved(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure cbCaseSensitiveClick(Sender: TObject);
    procedure LogoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure directionclick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnShowRegionsClick(Sender: TObject);
    procedure Findoutwhataccessesthisaddress1Click(Sender: TObject);
    procedure OpenProcesslist1Click(Sender: TObject);
    procedure CloseCheatEngine1Click(Sender: TObject);
    procedure Showashexadecimal1Click(Sender: TObject);
    procedure OpenMemorybrowser1Click(Sender: TObject);
    procedure Trytofindbasepointer1Click(Sender: TObject);
    procedure cbFastScanClick(Sender: TObject);
    procedure AllClickClick(Sender: TObject);
    procedure cbPauseWhileScanningClick(Sender: TObject);
    procedure btnSetSpeedhackClick(Sender: TObject);
    procedure Description1Click(Sender: TObject);
    procedure Address1Click(Sender: TObject);
    procedure ype1Click(Sender: TObject);
    procedure Value1Click(Sender: TObject);
    procedure ProcessLabelDblClick(Sender: TObject);
    procedure Timer4Timer(Sender: TObject);
    procedure ProcessLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbUnrandomizerClick(Sender: TObject);
    procedure cbUnrandomizerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Foundlist3CustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actAutoAssembleExecute(Sender: TObject);
    procedure Changescript1Click(Sender: TObject);
    procedure Forcerechecksymbols1Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Smarteditaddresses1Click(Sender: TObject);
    procedure Pointerscanforthisaddress1Click(Sender: TObject);
    procedure Label53Click(Sender: TObject);
    procedure Label57Click(Sender: TObject);
    procedure Foundlist3Data(Sender: TObject; Item: TListItem);
    procedure UpdateFoundlisttimerTimer(Sender: TObject);
    procedure Foundlist3KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mode16Click(Sender: TObject);
    procedure Label59Click(Sender: TObject);
    procedure Label38Click(Sender: TObject);
    procedure Browsethismemoryregioninthedisassembler1Click(
      Sender: TObject);
    procedure AutoAttachTimerTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ScanTypeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HeaderControl1SectionClick(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure HeaderControl1SectionResize(HeaderControl: THeaderControl;
      Section: THeaderSection);
  private
    fcontrol: tfcontrol;
    aaa:single;
    hotkeypressed: integer;

    cancelbutton: Tbutton;

    CreateCustomScanButton: TButton;
    EditCustomScanButton: TButton;
    CustomScanScripts: array of record
                         name: string;
                         data: TStringlist;
                         CustomScanType: TCustomScanType;
                       end;



    oldwidth,oldheight: integer;
    newaddress: Dword;
    isbit: boolean;
    tempbitmap: Tbitmap;
    dontrunshow:boolean;

    LastWasHex: boolean;
    dontconvert:boolean;
    FlashProcessButton: TFlash;
    oldvartype: integer;

    unrandomize: Tunrandomize;


    scantext2: tlabel;
    andlabel: tlabel;
    scanvalue2: tedit;
    cbpercentage: tcheckbox;

    ceclipboardformat: THandle;
    reinterpretcheck: integer;

    ffoundcount: int64;

    SaveFirstScanThread: TSaveFirstScanThread;

    foundlist: Tfoundlist;
    lastscantype: integer;

    editingscript: boolean;
    editedscript: integer;

    procedure doNewScan;
    procedure SetExpectedTableName;
    procedure autoattachcheck;
    procedure aprilfoolsscan;
    function CheckIfSaved: Boolean;
    procedure checkpaste;
    procedure hotkey(var Message: TMessage); message WM_HOTKEY;
    procedure WMGetMinMaxInfo(var Message: TMessage); message WM_GETMINMAXINFO;
    procedure freedebugger(var Message: TMessage); message WM_FREEDEBUGGER;
    procedure pointerscanner_addaddress(var Message: TMessage); message WM_COPYDATA;
    procedure Hotkey2(var Message:TMessage); message wm_hotkey2;
    procedure ScanDone(var message: TMessage); message WM_SCANDONE;
    procedure FreezeThem;
    Procedure Edit;
    function paste(simplecopypaste: boolean): integer;
    procedure CopySelectedRecords;
    procedure DeleteRecords;
    procedure Deletegroups(groups: grouptype);
    procedure exceptionhandler(Sender: TObject; E: Exception);
    procedure ResizeScreen;
    procedure SetReadWriteBreakpoint(address: dword; size: dword);
    procedure toggleWindow;
    procedure adjustbringtofronttext;

    procedure scanEpilogue(canceled: boolean);
    procedure CancelbuttonClick(sender: TObject);
    procedure CreateCustomScanButtonClick(sender: TObject);
    procedure EditCustomScanButtonClick(sender: TObject);

    procedure changeScriptCallback(script: string; changed: boolean);

    //property functions
    function GetRoundingType: TRoundingType;
    procedure SetRoundingType(rt: TRoundingType);
    function getScanStart: Dword;
    procedure setScanStart(newscanstart: dword);
    function getScanStop: Dword;
    procedure setScanStop(newscanstop: dword);
    function getFastscan: boolean;
    procedure setFastScan(state: boolean);
    function getScanReadonly: boolean;
    procedure setScanReadOnly(state: boolean);

    function getSelectedCustomScanData: TStringlist;
    function getSelectedCustomScanType: TCustomScanType;
    function getSelectedVariableType: TVariableType;
    procedure setfoundcount(x: int64);


  public
    { Public declarations }
   
    test: single;

    debugproc: boolean;

    oldNumberOfRecords: Integer;
    oldMemrec: array of memoryrecord;
    OldComments: String;
    oldcodelistcount: integer;

    NumberOfRecords: Integer;
    memrec: array of memoryrecord;
    FrozenFvalue: Array of Double;
    FrozenStrings: Array of string;
    FrozenBytes: array of array of byte;

    Hotkeystrings: Array of string;
    Hotkeys: array of integer;

    Selected: Array of Boolean;
    lastselected: Integer;
    LastLastSelected: Integer;
    FirstShiftSelected: Integer; //-1 = no shift pressed before else it contains the record that has been selected


    lastsave: array of memoryrecord;
    lastsavecount: Integer;

    NumberofLines: integer;
    freezedirection: array of TLabel;
    frozenbox:   array of TCheckbox;
    description: array of TLabel;
    address:     array of TLabel;
    ValType:     array of TLabel;
    Value:       array of TLabel;
    Select:      array of TLabel;

    Priority: Dword;
    memimage: TMemorystream;

    canceled: boolean;

    originalheight:integer;
    originalwidth:integer;

    fronttext: string;


    aprilfools: boolean;
    editedsincelastsave: boolean;

    autoattachlist: tstringlist;

    memscan: tmemscan;

    procedure plugintype1click(Sender:tObject);
    procedure AddToRecord(Line: Integer);
    procedure AddAutoAssembleScript(script:string);
    procedure reinterpretaddresses;
    
    procedure Updatelist;
    Procedure UpdateScreen;
    procedure SetWriteBreakpoint(address: dword; size: dword);
    procedure SetReadBreakpoint(address: dword; size: dword);
    procedure setfoundlisthorizontal;
    procedure reserveMem;
    procedure ChangeValue(Itemnr: integer; NewValue:String);
    function  getStringFromRecord(itemid: Integer):string;
    procedure EnableHypermode;
    procedure disableHypermode;
    Procedure UpdateScanType;
    procedure enableGui(isnextscan: boolean);
    procedure disableGui;
    procedure SpawnCancelButton;
    procedure DestroyCancelButton;
    procedure disableautoassemblecheat(i: integer);
    procedure enableautoassemblecheat(i: integer);
    procedure addaddress(description: string; address:dword; const offsets: array of dword; offsetcount: integer; ispointer: boolean; vartype: integer; length: integer; startbit:integer; unicode: boolean); overload;
    procedure addaddress(description: string; address:dword; const offsets: array of dword; offsetcount: integer; ispointer: boolean; vartype: integer; length: integer; startbit:integer; unicode,showashex: boolean); overload;
    property foundcount: int64 read ffoundcount write setfoundcount;
    property RoundingType: TRoundingType read GetRoundingType write SetRoundingType;
    property ScanStart: dword read getScanStart write setScanStart;
    property ScanStop: dword read getScanStop write setScanStop;
    property FastScan: boolean read getFastscan write setFastscan;
    property ScanReadOnly: boolean read getScanReadonly write setScanReadonly;
    property SelectedCustomScanData: Tstringlist read getSelectedCustomScanData;
    property SelectedCustomScanType: TCustomScanType read getSelectedCustomScanType;
    property SelectedVariableType: TVariableType read getSelectedVariableType;
  end;

var
  MainForm: TMainForm;
  ToggleWindows: TTogglewindows;

implementation

uses mainunit2,ProcessWindowUnit, MemoryBrowserFormUnit, TypePopup,
  AddAddress, HotKeys, standaloneunit, aboutunit,
  CommentsUnit, formsettingsunit, Changeoffsetunit, FoundCodeUnit, advancedoptionsunit,
  formScanningUnit, formDifferentBitSizeUnit,OpenSave,
  formMemoryTrainerUnit, MemoryTrainerDesignUnit, formhotkeyunit,
  formAddressChangeUnit, formmemoryregionsunit,formPointerOrPointeeUnit,
  frmhotkeyconfigunit, frmProcessWatcherUnit, formProcessInfo, frmautoinjectunit,
  PasteTableentryFRM, pointerscannerfrm, PointerscannerSettingsFrm,
  InjectedpointerscanornotFRM, frmGDTunit, frmFunctionlistUnit;

{$R *.DFM}

var ncol: TColor;
procedure TFlash.Col;
begin
  mainform.panel7.Color:=ncol;
end;

procedure TFlash.Execute;
var red,green: byte;
    decreasered: boolean;
begin
{$ifndef ceasinjecteddll}
  freeonterminate:=true; 
  decreasered:=true;
  red:=254;
  green:=0;
  while not terminated do
  begin
    if decreasered then
    begin
      dec(red,2);
      inc(green,2);
      if green>=250 then decreasered:=false;
    end
    else
    begin
      inc(red,2);
      dec(green,2);
      if red>=254 then
        decreasered:=true;
    end;
    ncol:=(green shl 8)+red;
    synchronize(col); //sigh (without it works too, but at least with this I know for sure it works.

    sleep(10);
  end;

{$endif}
  mainform.Panel7.Color:=clBtnFace;
end;

constructor TToggleWindows.Create(CreateSuspended: Boolean);
begin
  freeonterminate:=true;
  inherited Create(CreateSuspended);
end;

procedure TToggleWindows.Execute;
begin
  toggleotherwindows;
  togglewindows:=nil;
end;

procedure TMainForm.WMGetMinMaxInfo(var Message: TMessage);
var MMInfo: ^MINMAXINFO;
begin //the constraint function of the form behaves weird when draging from the top or left side, so I have to do this myself.
//mainform.pix
  MMInfo:=ptr(message.LParam);
  if pixelsperinch=96 then
  begin
    MMInfo.ptMinTrackSize:=point(490,460);
  end;
end;

procedure TFcontrol.wmMouseWheel(var msg: TWMMousewheel);
var k: word;

begin
  //move down message

  if msg.WheelDelta>0 then
    k:=vk_up
  else
    k:=vk_down;

  mainform.FControlKeyDown(self, k,[]);
end;

procedure TMainform.Hotkey2(var Message:TMessage);
var i:integer;
    a,b: single;
    s: string;
begin
  case message.WParam of
    0:
    begin
      //popup/hide CE
      if advancedoptions.Pausebutton.Down then
      begin
        errorbeep;
        exit;
      end;

      beep;

      if formsettings.cbHideAllWindows.checked then
      begin
        ToggleWindow;

        if formsettings.cbCenterOnPopup.checked then
          if not allwindowsareback then
            setwindowpos(mainform.Handle,HWND_NOTOPMOST,(screen.Width div 2)-(mainform.Width div 2),(screen.Height div 2)-(mainform.height div 2),mainform.Width,mainform.Height,SWP_NOZORDER or SWP_NOACTIVATE);

        if not allwindowsareback then application.BringToFront else
          setforegroundwindow(lastforeground);

        adjustbringtofronttext;
        exit;
      end;

      application.BringToFront;
      SetForegroundWindow(mainform.Handle);

      mainform.SetFocus;

      if formsettings.cbCenterOnPopup.checked then
        setwindowpos(mainform.Handle,HWND_NOTOPMOST,(screen.Width div 2)-(mainform.Width div 2),(screen.Height div 2)-(mainform.height div 2),mainform.Width,mainform.Height,SWP_NOZORDER or SWP_NOACTIVATE);

      formstyle:=fsStayOnTop;
    end;

    1: //Pause
    begin
      with advancedoptions do
      begin
        pausedbyhotkey:=true;
        pausebutton.down:=not pausebutton.down;
        pausebutton.Click;
        pausedbyhotkey:=false;
      end;
    end;

    2: //speedhack
    begin
      if cbSpeedhack.Enabled then
      begin
        beep;
        cbSpeedhack.checked:=not cbSpeedhack.Checked;
      end;
    end;

    //3..7=set speedhack speed
    3:
    begin
      if cbspeedhack.enabled then
      begin
        cbspeedhack.Checked:=true;
        if cbspeedhack.checked then
        begin
          edit2.Text:=format('%.2f',[speedhackspeed1.speed]);
          edit1.Text:=inttostr(speedhackspeed1.sleeptime);
          btnSetSpeedhack.Click;
        end;
      end;
    end;

    4:
    begin
      if cbspeedhack.enabled then
      begin
        cbspeedhack.Checked:=true;
        if cbspeedhack.checked then
        begin
          edit2.Text:=format('%.2f',[speedhackspeed2.speed]);
          edit1.Text:=inttostr(speedhackspeed2.sleeptime);
          btnSetSpeedhack.Click;
        end;
      end;
    end;

    5:
    begin
      if cbspeedhack.enabled then
      begin
        cbspeedhack.Checked:=true;
        if cbspeedhack.checked then
        begin
          edit2.Text:=format('%.2f',[speedhackspeed3.speed]);
          edit1.Text:=inttostr(speedhackspeed3.sleeptime);
          btnSetSpeedhack.Click;
        end;
      end;
    end;

    6:
    begin
      if cbspeedhack.enabled then
      begin
        cbspeedhack.Checked:=true;
        if cbspeedhack.checked then
        begin
          edit2.Text:=format('%.2f',[speedhackspeed4.speed]);
          edit1.Text:=inttostr(speedhackspeed4.sleeptime);
          btnSetSpeedhack.Click;
        end;
      end;
    end;

    7:
    begin
      if cbspeedhack.enabled then
      begin
        cbspeedhack.Checked:=true;
        if cbspeedhack.checked then
        begin
          edit2.Text:=format('%.2f',[speedhackspeed5.speed]);
          edit1.Text:=inttostr(speedhackspeed5.sleeptime);
          btnSetSpeedhack.Click;
        end;
      end;
    end;

    8:
    begin
      //increase speed
      try
        if cbspeedhack.Enabled then
        begin
          cbspeedhack.Checked:=true;
          if cbspeedhack.Checked then
          begin
            a:=strtofloat(edit2.text);
            a:=a+speedupdelta;
            edit2.Text:=format('%.2f',[a]);
            btnSetSpeedhack.Click;
          END;
        end;
      except

      end;
    end;


    9:
    begin
      //decrease speed
      try
        if cbspeedhack.Enabled then
        begin
          cbspeedhack.Checked:=true;
          if cbspeedhack.Checked then
          begin
            b:=strtofloat(edit2.text);
            b:=b-slowdowndelta;
            edit2.Text:=format('%.2f',[b]);
            btnSetSpeedhack.Click;
          end;
        end;
      except

      end;
    end;

    10..18: //Change type (if possible)
    begin
      if vartype.Enabled then
        vartype.itemindex:=message.WParam-3
      else
      begin
        errorbeep;
      end;
    end;

    19://new scan
    begin
      if not newscan.Enabled then exit; //only when no process is opened
      if (formscanning<>nil) and (formscanning.Visible) then exit; //it's scanning

      i:=vartype.ItemIndex;

      if newscan.Caption=strNewScan then
        newscan.Click; //start new scan

      vartype.ItemIndex:=i;
      vartype.OnChange(vartype); //set previous type
    end;

    20: //new scan Exact value
    begin

      if not newscan.Enabled then exit;
      if (formscanning<>nil) and (formscanning.Visible) then exit; //it's scanning
      i:=vartype.itemindex;
      s:=scanvalue.Text;
      if s='' then exit;      

      if newscan.Caption=strNewScan then
        newscan.Click; //start new scan

      vartype.ItemIndex:=i;
      vartype.OnChange(vartype); //set previous type

      scanvalue.Text:=s;
      newscan.Click;
    end;

    21: //new scan unknown initial value
    begin
      if not newscan.Enabled then exit;
      if (formscanning<>nil) and (formscanning.Visible) then exit; //it's scanning

      i:=vartype.ItemIndex;

      if newscan.Caption=strNewScan then
        newscan.Click; //start new scan

      vartype.ItemIndex:=i;
      vartype.OnChange(vartype);

      scantype.ItemIndex:=scantype.Items.IndexOf(StrUnknownInitialValue);
      scantype.OnChange(scantype);

      newscan.Click;
    end;

    22: //next scan Exact value
    begin
      if not newscan.Enabled then exit;
      if (formscanning<>nil) and (formscanning.Visible) then exit; //it's scanning

      if nextscanbutton.Enabled then
      begin
        scantype.ItemIndex:=scantype.Items.IndexOf(StrExactValue);
        scantype.OnChange(scantype);

        nextscanbutton.click;
      end
      else Errorbeep;
    end;

    23: //next scan IncreasedValue
    begin
      if not newscan.Enabled then exit;
      if (formscanning<>nil) and (formscanning.Visible) then exit; //it's scanning

      if nextscanbutton.Enabled then
      begin
        scantype.ItemIndex:=scantype.Items.IndexOf(StrIncreasedValue);
        scantype.OnChange(scantype);

        nextscanbutton.click;
      end
      else Errorbeep;
    end;

    24: //next scan DecreasedValue
    begin
      if not newscan.Enabled then exit;
      if (formscanning<>nil) and (formscanning.Visible) then exit; //it's scanning

      if nextscanbutton.Enabled then
      begin
        scantype.ItemIndex:=scantype.Items.IndexOf(StrDecreasedValue);
        scantype.OnChange(scantype);

        nextscanbutton.click;
      end
      else Errorbeep;
    end;

    25: //next scan ChangedValue
    begin
      if not newscan.Enabled then exit;
      if (formscanning<>nil) and (formscanning.Visible) then exit; //it's scanning

      if nextscanbutton.Enabled then
      begin
        scantype.ItemIndex:=scantype.Items.IndexOf(StrChangedValue);
        scantype.OnChange(scantype);

        nextscanbutton.click;
      end
      else Errorbeep;
    end;

    26: //next scan unchangedValue
    begin
      if not newscan.Enabled then exit;
      if (formscanning<>nil) and (formscanning.Visible) then exit; //it's scanning

      if nextscanbutton.Enabled then
      begin
        scantype.ItemIndex:=scantype.Items.IndexOf(StrUnchangedValue);
        scantype.OnChange(scantype);

        nextscanbutton.click;
      end
      else Errorbeep;
    end;

    27: //undo lastscan
    begin
      if not newscan.Enabled then exit;
      if (formscanning<>nil) and (formscanning.Visible) then exit; //it's scanning

      if undoscan.Enabled then
        undoscan.Click
      else
        Errorbeep;
    end;

    28: //cancel current scan
    begin
      if (formscanning<>nil) and (formscanning.btnCancel.Enabled) then
        formscanning.btnCancel.Click;
    end;

  end;

end;

procedure TMainForm.hotkey(var Message: TMessage); //stays because the old hotkeyhandler is still used in some places
var x: TDevMode;
    i: integer;
    c: integer;
begin
  if ((hotkeyform<>nil) and (hotkeyform.Visible)) or ((formhotkey<>nil) and (formhotkey.visible)) then exit;

  if Message.wparam=0 then
  begin
    //bring to front
    try
      unregisterhotkey(mainform.handle,0);

    if advancedoptions.Pausebutton.Down then
    begin
      beep;
      sleep(100);
      beep;
      sleep(100);
      beep;
      sleep(100);
      exit;
    end;

    beep;

    if formsettings.cbHideAllWindows.checked then
    begin
      ToggleWindow;

//      ToggleOtherWindows;

      if formsettings.cbCenterOnPopup.checked then
        if not allwindowsareback then
          setwindowpos(mainform.Handle,HWND_NOTOPMOST,(screen.Width div 2)-(mainform.Width div 2),(screen.Height div 2)-(mainform.height div 2),mainform.Width,mainform.Height,SWP_NOZORDER or SWP_NOACTIVATE);

      if not allwindowsareback then application.BringToFront else
      begin
        setforegroundwindow(lastforeground);
     //   setactivewindow(lastactive);
      end;

      adjustbringtofronttext;
      exit;
    end;


   // if length(windowlist)<>0 then
    application.BringToFront;

    if formsettings.cbCenterOnPopup.checked then
      setwindowpos(mainform.Handle,HWND_NOTOPMOST,(screen.Width div 2)-(mainform.Width div 2),(screen.Height div 2)-(mainform.height div 2),mainform.Width,mainform.Height,SWP_NOZORDER or SWP_NOACTIVATE);

    formstyle:=fsStayOnTop;

    finally
      registerhotkey(mainform.handle,0,message.lparamlo,message.LParamHi); //restore the hotkey
    end;
  end;


  if message.WParam=2 then //toggle speedhack
  begin
    try
      unregisterhotkey(mainform.handle,2);
      if cbSpeedhack.Enabled then
      begin
        beep;
        cbSpeedhack.checked:=not cbSpeedhack.Checked;
      end;
    finally
      registerhotkey(mainform.handle,2,message.lparamlo,message.LParamHi); //restore the hotkey
    end;

  end;

  if message.WParam>=10 then //it's a record with a hotkey
  begin
    for i:=0 to numberofrecords-1 do
    begin //find the record
      if message.WParam=hotkeys[i] then
      begin
        if memrec[i].VarType=255 then
        begin
          if memrec[i].Frozen then
            disableautoassemblecheat(i)
          else
            enableautoassemblecheat(i);
            
          updatescreen;
          exit;
        end;

        //set the value to what it was when the hotkey got set
        hotkeypressed:=i;
        memrec[i].Frozen:=true;
        FreezeThem;
        memrec[i].Frozen:=false;
        hotkeypressed:=-1;
        exit;
      end;
    end;
  end;
end;
//----------------------------------

function TMainform.getSelectedCustomScanData: TStringlist;
var index: integer;
begin

  index:=scantype.itemindex;
  if (getSelectedVariableType=vtCustom) and (index>=0) then
    result:=CustomScanScripts[index].data
  else
    result:=nil;
end;

function TMainform.getSelectedCustomScanType: TCustomScanType;
var index: integer;
begin
  index:=scantype.itemindex;
  if (getSelectedVariableType=vtCustom) and (index>=0) then
    result:=CustomScanScripts[index].CustomScanType
  else
    result:=cstNone;
end;

function TMainform.getSelectedVariableType: TVariableType;
{wrapper for the new getVarType2 in the new scanroutine}
begin
  result:=getVarType2;
end;

function TMainform.getScanStart: Dword;
begin
  try
    result:=strtoint('$'+FromAddress.text);
  except
    raise exception.Create('Invalid start address: '+FromAddress.text);
  end;
end;

procedure TMainform.setScanStart(newscanstart: dword);
begin
  FromAddress.text:=inttohex(newscanstart,8);
end;

function TMainform.getScanStop: Dword;
begin
  try
    result:=strtoint('$'+ToAddress.text);
  except
    raise exception.Create('Invalid start address: '+FromAddress.text);
  end;
end;

procedure TMainform.setScanStop(newscanstop: dword);
begin
  ToAddress.text:=inttohex(newscanstop,8);
end;


function TMainform.getFastscan: boolean;
begin
  result:=cbFastscan.checked;
end;

procedure TMainform.setFastScan(state: boolean);
begin
  cbFastscan.checked:=state;
end;

function TMainform.getScanReadonly: boolean;
begin
  result:=Readonly.checked;
end;

procedure TMainform.setScanReadOnly(state: boolean);
begin
  readonly.checked:=state;
end;


function TMainform.GetRoundingType: TRoundingType;
{Property function to get the current rounding type}
begin
  if rt1.checked then result:=rtRounded else
  if rt2.checked then result:=rtExtremerounded else
  if rt3.Checked then result:=rtTruncated;
end;

procedure TMainform.SetRoundingType(rt: TRoundingType);
{Property function to set the current rounding type}
begin
  case rt of
    rtRounded:        rt1.checked;
    rtExtremerounded: rt2.checked;
    rtTruncated:      rt3.checked;
  end;
end;


procedure TMainform.setfoundcount(x: int64);
begin
  ffoundcount:=x;
  foundcountlabel.Caption:=inttostr(x);
end;

procedure TMainform.DestroyCancelButton;
begin
  if cancelbutton<>nil then
    freeandnil(cancelbutton);
end;

procedure TMainform.SpawnCancelButton;
begin
  cancelbutton:=TButton.create(self);
  with cancelbutton do
  begin
    top:=newscan.top;
    left:=newscan.left;
    width:=(nextscanbutton.left+nextscanbutton.width)-left;
    height:=newscan.height;
    caption:='Cancel';
    onclick:=cancelbuttonclick;
    parent:=self;
  end;
end;


procedure TMainform.DisableGui;
{
This procedure will disable the gui. E.g while scanning the memory with no wait
screen.
}
var i: integer;
begin
  Groupbox1.Enabled:=false;
  for i:=0 to groupbox1.ControlCount-1 do
    groupbox1.Controls[i].Enabled:=false;

  scanvalue.Enabled:=false;
  vartype.Enabled:=false;
  scantype.Enabled:=false;
  scantext.enabled:=false;
  label4.enabled:=false;
  label8.Enabled:=false;
  HexadecimalCheckbox.Enabled:=false;
  cbCaseSensitive.Enabled:=false;

  newscan.enabled:=false;
  nextscanbutton.enabled:=false;
  undoscan.enabled:=false;
end;

procedure TMainform.EnableGui(isnextscan: boolean);
{
Enables the gui options according to what type of scan is currently used
no scan, enable everything
already scanning, disable the group and type
}
var i: integer;
    fname,expectedfilename: string;
    scanstarted: boolean;
begin
  scanstarted:=newscan.caption=strnewscan;

  if not scanstarted then
  begin
    Groupbox1.Enabled:=true;
    for i:=0 to groupbox1.ControlCount-1 do
      groupbox1.Controls[i].Enabled:=true;
  end;

  scanvalue.Enabled:=true;
  newscan.Enabled:=true;

  undoscan.Enabled:=isnextscan; //nextascan was already enabled
  nextscanbutton.Enabled:=scanstarted;
  vartype.Enabled:=not scanstarted;
  scantype.Enabled:=true;
  scantext.enabled:=true;
  label4.enabled:=true;
  label8.Enabled:=true;
  HexadecimalCheckbox.Enabled:=true;
  cbCaseSensitive.Enabled:=true;

  scanvalue.visible:=true;
  scantext.Visible:=true;

  Updatescantype;
  Scantype.ItemIndex:=0;

  //-----------------------
  //Get the expectedFilename
  //-----------------------
  SetExpectedTableName;

  cbspeedhack.enabled:=true;
  cbunrandomizer.Enabled:=true;
end;

procedure TMainform.toggleWindow;
resourcestring
  strWindowFailedToHide='A window failed to hide';
var c: integer;
begin
  togglewindows:=TTogglewindows.Create(false);
  c:=0;
  while togglewindows<>nil do
  begin
    if c=500 then
    begin
      togglewindows.free;
      raise exception.Create(strWindowFailedToHide);
    end;
    sleep(10);
    inc(c);
  end;
end;

procedure TMainForm.ReserveMem;
var before: integer;
    i: integer;
begin
  before:=length(memrec);

  if numberofrecords<before then
  begin
    //cleanup some extra memory
    for i:=numberofrecords to before-1 do
    begin
      setlength(memrec[i].pointers,0);
      setlength(memrec[i].allocs,0);
    end;
  end;

  setlength(memrec,numberofrecords);
  setlength(frozenfvalue,numberofrecords);
  setlength(frozenStrings,numberofrecords);
  setlength(frozenbytes,NumberOfRecords);
  setlength(selected,numberofrecords);
  setlength(hotkeystrings,NumberOfRecords);
  setlength(Hotkeys,numberofrecords);

  if numberofrecords>before then
  begin
    //initialize the new memory
    for i:=before to numberofrecords-1 do
    begin
      zeromemory(@memrec[i],sizeof(memrec[i]));
      zeromemory(@frozenfvalue[i],sizeof(frozenfvalue[i]));
      zeromemory(@frozenStrings[i],sizeof(frozenStrings[i]));
      zeromemory(@frozenbytes[i],sizeof(frozenbytes[i]));
      zeromemory(@selected[i],sizeof(selected[i]));
      zeromemory(@hotkeystrings[i],sizeof(hotkeystrings[i]));
      zeromemory(@Hotkeys[i],sizeof(Hotkeys[i]));
    end;
  end;
end;


procedure TMainform.setfoundlisthorizontal;
begin
//removed
end;

procedure TMainform.exceptionhandler(Sender: TObject; E: Exception);
begin
  if (E.Message='Error creating window device context') then exit;
  screen.Cursor:=crdefault;
  closefile(addressfile);
  closefile(memoryfile);
  closefile(newAddressfile);
  closefile(newmemoryfile);
  Application.ShowException(E);
end;

procedure TMainForm.freedebugger;
begin
  //the debugger says the process has ended, or the thread has stopped
  freeandnil(debuggerthread);
  advancedoptions.Pausebutton.Down:=false;
end;


procedure TMainform.SetReadWriteBreakpoint(address: dword; size: dword);
var mbi: _Memory_Basic_Information;
    i: integer;
    ct: _context;
    regsinuse: integer;
    olda,olds: dword;
    dr: dword;

procedure Set4bytebreak;
begin
  case regsinuse of
    0: begin
         ct.dr0:=address;
         ct.dr7:=ct.dr7 or reg0set or reg0rw or debugexact or reg0len4;
       end;

    1: begin
         ct.dr1:=address;
         ct.dr7:=ct.Dr7 or reg1set or reg1rw or debugexact or reg1len4;
       end;

    2: begin
         ct.Dr2:=address;
         ct.dr7:=ct.dr7 or reg2set or reg2rw or debugexact or reg2len4;
       end;

    3: begin
         ct.Dr3:=address;
         ct.dr7:=ct.dr7 or reg3set or reg3rw or debugexact or reg3len4;
       end;
  end;

  inc(address,4);
  dec(size,4);
  inc(regsinuse);
end;

procedure Set2bytebreak;
begin
  case regsinuse of
    0: begin
         ct.dr0:=address;
         ct.dr7:=ct.dr7 or reg0set or reg0rw or debugexact or reg0len2;
       end;

    1: begin
         ct.dr1:=address;
         ct.dr7:=ct.Dr7 or reg1set or reg1rw or debugexact or reg1len2;
       end;

    2: begin
         ct.Dr2:=address;
         ct.dr7:=ct.dr7 or reg2set or reg2rw or debugexact or reg2len2;
       end;

    3: begin
         ct.Dr3:=address;
         ct.dr7:=ct.dr7 or reg3set or reg3rw or debugexact or reg3len2;
       end;
  end;

  inc(address,2);
  dec(size,2);
  inc(regsinuse);
end;

procedure Set1bytebreak;
begin
  case regsinuse of
    0: begin
         ct.dr0:=address;
         ct.dr7:=ct.dr7 or reg0set or reg0rw or debugexact;
       end;

    1: begin
         ct.dr1:=address;
         ct.dr7:=ct.Dr7 or reg1set or reg1rw or debugexact;
       end;

    2: begin
         ct.Dr2:=address;
         ct.dr7:=ct.dr7 or reg2set or reg2rw or debugexact;
       end;

    3: begin
         ct.Dr3:=address;
         ct.dr7:=ct.dr7 or reg3set or reg3rw or debugexact;
       end;
  end;

  inc(address);
  dec(size);
  inc(regsinuse);
end;

resourcestring
  strAccessed='The following opcodes accessed the selected address';

var rd: dword;
    tmp: byte;
    ths: thandle;
    th: thandle;
    tE: threadentry32;
begin
  //check if you can read address to address+size
  if not (formsettings.rbDebugRegisters.checked or formsettings.cbKDebug.checked) then
  begin
    readprocessmemory(processhandle,pointer(address),@tmp,1,rd);
    if rd<>1 then raise exception.Create(strAddressHasToBeReadable);
  end;

  if foundcodedialog<>nil then raise exception.Create('The debugger is already trying to find out what reads,writes or accesses a certain address. First close the other window');
  if debuggerthread2<>nil then raise exception.create('Please stop any other debugger option before enabling this option');

  foundcodedialog:=TFoundcodedialog.create(self);
  foundcodedialog.Caption:=strAccessed;
  foundcodedialog.btnOK.caption:=strStop;

  if (formsettings.cbKdebug.checked) and (debuggerthread=nil) then
    if not DebugProcess(processid,address,size,3) then raise exception.Create(strFailedToInitialize);

  olda:=address;
  olds:=size;
  zeromemory(@ct,sizeof(ct));
  ct.ContextFlags:=CONTEXT_DEBUG_REGISTERS;

  //configure the debugregs
  if formsettings.rbDebugRegisters.checked or formsettings.cbKDebug.checked then
  begin
    foundcodedialog.useexceptions:=false;
    regsinuse:=0;
    ct.dr7:=0;
    while (regsinuse<4) and (size>0) do
    begin
      if size>=4 then
      begin
        if (address mod 4)>0 then
        begin
          if (address mod 2)>0 then
          begin
            set1bytebreak; //watch on a byte
            continue;
          end
          else
          begin
            set2bytebreak;
            continue;
          end;
        end
        else
        begin
          set4bytebreak;
          continue;
        end;
      end;

      if size>=2 then
      begin
        if (address mod 2)>0 then
        begin
          set1bytebreak; //watch on a byte
          continue;
        end
        else
        begin
          set2bytebreak;
          continue;
        end;
      end;


      if size=1 then
        set1bytebreak;
    end;

    //ct.dr7:=$D0303;
    if formsettings.cbKdebug.Checked and (debuggerthread=nil) then
    begin
      if DebuggerThread2<>nil then
      begin
        debuggerthread2.Terminate;
        debuggerthread2.WaitFor;
        freeandnil(debuggerthread2);
      end;

      DebuggerThread2:=TDebugEvents.Create(true);
      debuggerthread2.debugregs:=ct;

      for i:=0 to length(debuggerthread2.threadlist)-1 do
      begin
        suspendthread(debuggerthread2.threadlist[i]);
        SetThreadContext(debuggerthread2.threadlist[i],debuggerthread2.debugregs);
        resumethread(debuggerthread2.threadlist[i]);
      end;

      debuggerthread2.Resume;
    end
    else
    begin
      debuggerthread.DRRegs:=ct;

      debuggerthread.Suspend;
      for i:=0 to length(debuggerthread.threadlist)-1 do
      begin
        suspendthread(debuggerthread.threadlist[i][1]);
        SetThreadContext(debuggerthread.threadlist[i][1],debuggerthread.DRRegs);
        resumethread(debuggerthread.threadlist[i][1]);
      end;

      debuggerthread.FindWriter2:=true;
      debuggerthread.Resume;
    end;
  end
  else
  begin
   //dont use debug regs
    foundcodedialog.useexceptions:=true;
    virtualqueryEx(processhandle,pointer(address),mbi,sizeof(mbi));

    debugger.DebuggerThread.findreader.pagebase:=dword(mbi.BaseAddress);
    debugger.DebuggerThread.findreader.pagesize:=dword(mbi.RegionSize);
    debugger.DebuggerThread.findreader.Address:=address;
    debugger.DebuggerThread.findreader.size:=size;
    DebuggerThread.findreaderset:=true;
    DebuggerThread.alsowrites:=true;

    VirtualProtectEx(processhandle,pointer(address),size,PAGE_NOACCESS,debugger.DebuggerThread.findreader.originalprotection);
  end;

  foundcodedialog.show;
end;


procedure TMainform.SetReadBreakpoint(address: dword; size: dword);
var mbi: _Memory_Basic_Information;
    i: integer;
    tmp:byte;
    rD:dword;
resourcestring strOpcodeRead='The following opcodes read from the selected address';
begin
  readprocessmemory(processhandle,pointer(address),@tmp,1,rd);
  if rd<>1 then raise exception.Create(strAddressHasToBeReadable);

  if foundcodedialog<>nil then raise exception.Create('The debugger is already trying to find out what reads,writes or accesses a certain address. First close the other window');

  foundcodedialog:=TFoundcodedialog.create(self);
  foundcodedialog.Caption:=strOpcodeRead;
  foundcodedialog.useexceptions:=true;
  foundcodedialog.btnOK.caption:=strStop;

  virtualqueryEx(processhandle,pointer(address),mbi,sizeof(mbi));

  debugger.DebuggerThread.findreader.pagebase:=dword(mbi.BaseAddress);
  debugger.DebuggerThread.findreader.pagesize:=dword(mbi.RegionSize);
  debugger.DebuggerThread.findreader.Address:=address;
  debugger.DebuggerThread.findreader.size:=size;
  DebuggerThread.findreaderset:=true;

  VirtualProtectEx(processhandle,pointer(address),size,PAGE_NOACCESS,debugger.DebuggerThread.findreader.originalprotection);
  foundcodedialog.show;
end;

procedure TMainform.SetWriteBreakpoint(address: dword; size: dword);
var mbi: _Memory_Basic_Information;
    i: integer;
    ct: _context;
    regsinuse: integer;
    olda,olds: dword;
    dr: dword;

procedure Set4bytebreak;
begin
  case regsinuse of
    0: begin
         ct.dr0:=address;
         ct.dr7:=ct.dr7 or reg0set or reg0w or debugexact or reg0len4;
       end;

    1: begin
         ct.dr1:=address;
         ct.dr7:=ct.Dr7 or reg1set or reg1w or debugexact or reg1len4;
       end;

    2: begin
         ct.Dr2:=address;
         ct.dr7:=ct.dr7 or reg2set or reg2w or debugexact or reg2len4;
       end;

    3: begin
         ct.Dr3:=address;
         ct.dr7:=ct.dr7 or reg3set or reg3w or debugexact or reg3len4;
       end;
  end;

  inc(address,4);
  dec(size,4);
  inc(regsinuse);
end;

procedure Set2bytebreak;
begin
  case regsinuse of
    0: begin
         ct.dr0:=address;
         ct.dr7:=ct.dr7 or reg0set or reg0w or debugexact or reg0len2;
       end;

    1: begin
         ct.dr1:=address;
         ct.dr7:=ct.Dr7 or reg1set or reg1w or debugexact or reg1len2;
       end;

    2: begin
         ct.Dr2:=address;
         ct.dr7:=ct.dr7 or reg2set or reg2w or debugexact or reg2len2;
       end;

    3: begin
         ct.Dr3:=address;
         ct.dr7:=ct.dr7 or reg3set or reg3w or debugexact or reg3len2;
       end;
  end;

  inc(address,2);
  dec(size,2);
  inc(regsinuse);
end;

procedure Set1bytebreak;
begin
  case regsinuse of
    0: begin
         ct.dr0:=address;
         ct.dr7:=ct.dr7 or reg0set or reg0w or debugexact;
       end;

    1: begin
         ct.dr1:=address;
         ct.dr7:=ct.Dr7 or reg1set or reg1w or debugexact;
       end;

    2: begin
         ct.Dr2:=address;
         ct.dr7:=ct.dr7 or reg2set or reg2w or debugexact;
       end;

    3: begin
         ct.Dr3:=address;
         ct.dr7:=ct.dr7 or reg3set or reg3w or debugexact;
       end;
  end;

  inc(address);
  dec(size);
  inc(regsinuse);
end;

var rd: dword;
    tmp: byte;
resourcestring strOpcodeChanged='The following opcodes changed the selected address';
begin
  //check if you can read address to address+size
  if not (formsettings.rbDebugRegisters.checked or formsettings.cbKDebug.checked) then
  begin
    readprocessmemory(processhandle,pointer(address),@tmp,1,rd);
    if rd<>1 then raise exception.Create(strAddressHasToBeReadable);
  end;

  if foundcodedialog<>nil then raise exception.Create('The debugger is already trying to find out what reads,writes or accesses a certain address. First close the other window');
  if debuggerthread2<>nil then raise exception.create('Please stop any other debugger option before enabling this option');

  foundcodedialog:=TFoundcodedialog.create(self);
  foundcodedialog.Caption:=strOpcodeChanged;
  foundcodedialog.btnOK.caption:=strstop;

  if formsettings.cbKdebug.checked and (debuggerthread=nil) then
    if not DebugProcess(processid,address,size,2) then raise exception.Create(strFailedToInitialize);


  olda:=address;
  olds:=size;
  zeromemory(@ct,sizeof(ct));
  ct.ContextFlags:=CONTEXT_DEBUG_REGISTERS;

  if formsettings.rbDebugRegisters.checked then
  begin
    foundcodedialog.useexceptions:=false;
    regsinuse:=0;
    ct.dr7:=0;
    while (regsinuse<4) and (size>0) do
    begin
      if size>=4 then
      begin
        if (address mod 4)>0 then
        begin
          if (address mod 2)>0 then
          begin
            set1bytebreak; //watch on a byte
            continue;
          end
          else
          begin
            set2bytebreak;
            continue;
          end;
        end
        else
        begin
          set4bytebreak;
          continue;
        end;
      end;

      if size>=2 then
      begin
        if (address mod 2)>0 then
        begin
          set1bytebreak; //watch on a byte
          continue;
        end
        else
        begin
          set2bytebreak;
          continue;
        end;
      end;


      if size=1 then
        set1bytebreak;
    end;

   // ct.dr7:=$D0303;
    if formsettings.cbKdebug.Checked and (debuggerthread=nil) then
    begin
      if DebuggerThread2<>nil then
      begin
        debuggerthread2.Terminate;
        debuggerthread2.WaitFor;
        freeandnil(debuggerthread2);
      end;
          
      DebuggerThread2:=TDebugEvents.Create(true);
      debuggerthread2.debugregs:=ct;

      for i:=0 to length(debuggerthread2.threadlist)-1 do
      begin
        suspendthread(debuggerthread2.threadlist[i]);
        SetThreadContext(debuggerthread2.threadlist[i],debuggerthread2.debugregs);
        resumethread(debuggerthread2.threadlist[i]);
      end;

      debuggerthread2.Resume;
    end
    else
    begin
      debuggerthread.DRRegs:=ct;

      debuggerthread.Suspend;
      for i:=0 to length(debuggerthread.threadlist)-1 do
      begin
        suspendthread(debuggerthread.threadlist[i][1]);
        SetThreadContext(debuggerthread.threadlist[i][1],debuggerthread.DRRegs);
        resumethread(debuggerthread.threadlist[i][1]);
      end;

      debuggerthread.FindWriter2:=true;
      debuggerthread.Resume;
    end;
  end
  else
  begin
    foundcodedialog.useexceptions:=true;
    virtualqueryEx(processhandle,pointer(olda),mbi,sizeof(mbi));

    debugger.DebuggerThread.readonly.pagebase:=dword(mbi.BaseAddress);
    debugger.DebuggerThread.readonly.pagesize:=dword(mbi.RegionSize);
    debugger.DebuggerThread.readonly.Address:=olda;
    debugger.DebuggerThread.readonly.size:=olds;
    DebuggerThread.readonlyset:=true;

    VirtualProtectEx(processhandle,pointer(olda),olds,PAGE_EXECUTE_READ,debugger.DebuggerThread.readonly.originalprotection);
  end;



  foundcodedialog.show;
end;

procedure TMainform.Deletegroups(groups: grouptype);
var i: integer;
begin
  for i:=0 to numberofrecords-1 do
  begin
    if groups[1] and (memrec[i].Group=1) then selected[i]:=true else
    if groups[2] and (memrec[i].Group=2) then selected[i]:=true else
    if groups[3] and (memrec[i].Group=3) then selected[i]:=true else
    if groups[4] and (memrec[i].Group=4) then selected[i]:=true else
    if groups[5] and (memrec[i].Group=5) then selected[i]:=true else
    if groups[6] and (memrec[i].Group=6) then selected[i]:=true else

    selected[i]:=false;
  end;

  deleterecords;
end;


procedure TMainForm.DeleteRecords;
var i,j: Integer;
begin
  if editingscript then exit; //don't do it when editing a script

  i:=0;
  while i<numberofrecords do
  begin
    if selected[i] then
    begin
      unregisterhotkey(handle,hotkeys[i]);
      hotkeys[i]:=-1;
      setlength(frozenbytes[i],0);

      for j:=i to numberofrecords-2 do
      begin
        memrec[j]:=memrec[j+1];
        frozenfvalue[j]:=frozenfvalue[j+1];
        frozenstrings[j]:=frozenstrings[j+1];
        frozenbytes[j]:=frozenbytes[j+1];
        selected[j]:=selected[j+1];
        hotkeystrings[j]:=hotkeystrings[j+1];
        hotkeys[j]:=hotkeys[j+1];
      end;

      dec(numberofrecords);
      dec(i);
      reservemem;

      if firstshiftselected>numberofrecords-1 then firstshiftselected:=-1;
    end;

    inc(i);
  end;

  If lastselected>numberofrecords-1 then lastselected:=numberofrecords-1;
  if lastselected>-1 then selected[lastselected]:=true;
  Updatescreen;
  Updatelist;
end;


Procedure TMainForm.UpdateScreen;
var rec,i,j: Integer;
    temp:string;
    col: tcolor;
    realaddress,realaddress2,count:dword;
    error: boolean;
    check: boolean;

resourcestring
  strAllowNegative='Allow negative changes';
  strAllowPositive='Allow positive changes';
  strNormalFreeze='Don''t allow any change';


begin
  if numberofrecords>=numberoflines then
  begin
    if not scrollbar1.Enabled then
      scrollbar1.enabled:=true;

    if scrollbar1.max<>numberofrecords-1 then
      scrollbar1.max:=numberofrecords-1;

    if scrollbar1.PageSize<>numberoflines-1 then
      scrollbar1.pagesize:=numberoflines-1;

    if scrollbar1.LargeChange<>numberoflines-1 then
      scrollbar1.LargeChange:=numberoflines-1;
  end else
  begin
    if scrollbar1.Enabled then
      scrollbar1.enabled:=false;

    if scrollbar1.Position<>0 then
      scrollbar1.position:=0;
  end;

  for i:=0 to numberoflines-1 do
  begin
    rec:=scrollbar1.Position+i;
    if rec<numberofrecords then
    begin
      if ((memrec[rec].Frozen) or (hotkeys[rec]<>-1)) and (memrec[rec].VarType in [0,1,2,3,4,6]) then
      begin
        case memrec[rec].Frozendirection of
          0:
          begin
            col:=clBlue;
            temp:='=';
          end;

          1:
          begin
            temp:='-';
            col:=clRed;
          end;

          2:
          begin
            temp:='+';
            col:=clGreen;
          end;
        end;

        if selected[rec] then col:=ClWhite;

        if freezedirection[i].Font.Color<>col then
          freezedirection[i].Font.Color:=col;

        if temp<>freezedirection[i].Caption then
        begin
          freezedirection[i].Caption:=temp;

          if temp='-' then freezedirection[i].Hint:=strAllowNegative;//'Allow negative changes';
          if temp='+' then freezedirection[i].Hint:=strAllowPositive;//'Allow positive changes';
          if temp='=' then freezedirection[i].Hint:=strNormalFreeze; //'Don''t allow any change';
          freezedirection[i].ShowHint:=true;
        end;



        freezedirection[i].visible:=true;
      end
      else
      begin
        freezedirection[i].visible:=false;
      end;

      frozenbox[i].checked:=memrec[rec].Frozen;
      if hotkeys[rec]<>-1 then
        frozenbox[i].Enabled:=false
      else
        frozenbox[i].Enabled:=true;

      description[i].Caption:=memrec[rec].Description;


      if memrec[rec].IsPointer then
      begin
        error:=false;

        //find the real address
        realaddress2:=memrec[rec].pointers[length(memrec[rec].pointers)-1].Address;
        for j:=length(memrec[rec].pointers)-1 downto 0 do
        begin
          check:=(readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count));
          if check and (count=4) then
            realaddress2:=realaddress+memrec[rec].pointers[j].offset
          else
          begin
            error:=true;
            break;
          end;
        end;
        realaddress:=realaddress2;

        if not error then
          address[i].Caption:='P->'+IntToHex(realaddress,8)
        else
          address[i].Caption:='P->????????';
      end
      else
        address[i].caption:=IntTohex(memrec[rec].address,8);


     // if not frozenbox[i].visible then  //if it's not yet visible, make it visible
      begin
        frozenbox[i].visible:=true;
        description[i].visible:=true;

        if memrec[rec].VarType<>255 then
        begin
          address[i].visible:=true;
          valtype[i].visible:=true;
          value[i].visible:=true;
        end
        else
        begin
          address[i].visible:=false;
          valtype[i].visible:=false;
          value[i].visible:=false;
        end;
      end;

      case memrec[rec].vartype of
      0         : valtype[i].Caption:=vartype.Items[1]; //byte
      1         : valtype[i].Caption:=vartype.Items[2];
      2         : valtype[i].Caption:=vartype.Items[3];
      3         : valtype[i].caption:=vartype.Items[5];
      4         : valtype[i].caption:=vartype.Items[6];
      5         : begin
                    valtype[i].Caption:=vartype.Items[0]+'['+IntToStr(memrec[rec].Bitlength)+']';
                    address[i].caption:=address[i].caption+'^'+IntToStr(memrec[rec].Bit);
                  end;
      6         : valtype[i].caption:=vartype.Items[4];
      7         :
                  begin
                    if memrec[rec].unicode then
                      valtype[i].Caption:='U'+vartype.Items[7]+'['+inttostr(memrec[rec].Bit)+']'
                    else
                      valtype[i].Caption:=vartype.Items[7]+'['+inttostr(memrec[rec].Bit)+']';
                  end;
      8         : valtype[i].Caption:=vartype.Items[8]+'['+inttostr(memrec[rec].Bit)+']';

      255       : valtype[i].Caption:=strAutoAssemble;
      else      valtype[i].caption:=strBUG;
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

          5  :  begin
                  description[i].Font.Color:=clPurple;
                  address[i].Font.Color:=clPurple;
                  valtype[i].Font.Color:=clPurple;
                  value[i].Font.Color:=clPurple;
                end;

          6  :  begin
                  description[i].Font.Color:=clWhite;
                  address[i].Font.Color:=clWhite;
                  valtype[i].Font.Color:=clWhite;
                  value[i].Font.Color:=clWhite;
                end;

          else  begin
                  description[i].Font.Color:=clWindowText;
                  address[i].Font.Color:=clWindowText;
                  valtype[i].Font.Color:=clWindowText;
                  value[i].Font.Color:=clWindowText;
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
        freezedirection[i].Visible:=false;
      end;

    end;
  end;

end;

function TMainForm.CheckIfSaved: boolean;
resourcestring strAskToSave='You haven''t saved your last changes yet. Save Now?';
var help :word;
    i,j: Integer;
begin
  //result:=true;
  result:=not editedsincelastsave;

  if advancedoptions.codelist.Count=oldcodelistcount then
  begin
    

  end else result:=false;

  if (OldNumberOfRecords=NumberOfRecords) then
  begin
    i:=0;
    while (result) and (i<numberofrecords) do
    begin
      if oldmemrec[i].Description<>memrec[i].Description then result:=false;
      if oldmemrec[i].Address<>memrec[i].Address then result:=false;
      if oldmemrec[i].VarType<>memrec[i].VarType then result:=false;
      if oldmemrec[i].Bit<>memrec[i].Bit then result:=false;
      if oldmemrec[i].bitlength<>memrec[i].bitlength then result:=false;
      if length(oldmemrec[i].pointers)<>length(memrec[i].pointers) then result:=false else
      begin
        for j:=0 to length(oldmemrec[i].pointers)-1 do
        begin
          if oldmemrec[i].pointers[j].Address<>memrec[i].pointers[j].Address then result:=false;
          if oldmemrec[i].pointers[j].offset<>memrec[i].pointers[j].Offset then result:=false;
        end;
      end;
      if oldmemrec[i].Group<>memrec[i].Group then result:=false;

      inc(i);
    end;
  end else result:=false;

  if not (oldcomments=comments.memo1.Text) then result:=false;

  if result=false then
  begin
    help:=messagedlg(strAskToSave,mtConfirmation,mbYesNoCancel,0);
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

procedure TMainForm.UpdateScanType;
var OldText: String;
    OldIndex: Integer;
    hexvis: boolean;
    floatvis: boolean;
    oldwidth: integer;
    i: integer;
resourcestring
  strScantextcaptiontotext='Text:';
  strScantextcaptiontoValue='Value:';
  strsearchForText='Search for text';
  strSearchForArray='Search for this array';
begin


  OldIndex:=Scantype.itemindex;
  OldText:=Scantype.text;
  hexvis:=true;
  floatvis:=false;

  ScanType.Items.Clear;

  ScanText.Caption:=strScantextcaptiontoValue;
  case varType.ItemIndex of
    0   :     begin
                ScanType.Items.Add(strExact);
                ScanType.DropDownCount:=1;
              end;

  1,2,3,4,5,6,9:begin  //byte-word-dword--8bytes-float-double-all
                if vartype.itemindex in [5,6,9] then
                begin
                  if oldindex=0 then
                    floatvis:=true;

                  if vartype.itemindex<>9 then
                    hexvis:=false;
                end;

                ScanType.Items.Add(strExactValue);
                ScanType.Items.Add(strBiggerThan);
                ScanType.Items.Add(strsmallerThan);
                ScanType.Items.Add(strValueBetween);

                if NextScanbutton.Enabled then
                begin
                  scantype.Items.Add(strIncreasedValue);
                  Scantype.Items.Add(strIncreasedValueBy);
                  ScanType.Items.Add(strDecreasedValue);
                  ScanType.Items.Add(strDecreasedValueBy);
                  ScanType.Items.add(strChangedValue);
                  ScanType.Items.Add(strUnchangedValue);

                  if (not advanced) and (not cbfasterscan.checked) then
                  begin
                    ScanType.Items.Add(strSameAsFirstScan);
                    Scantype.DropDownCount:=11;
                  end
                  else
                    Scantype.DropDownCount:=10;

                end else
                begin
                  ScanType.Items.Add(strUnknownInitialValue);
                  ScanType.DropDownCount:=5;
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

  10:         begin
                //custom
                //go through the list of custom scan's
                for i:=0 to length(CustomScanScripts)-1 do
                  scantype.Items.Add(CustomScanScripts[i].name);

                ScanType.DropDownCount:=length(CustomScanScripts);
              end;
  end;

  if varType.ItemIndex = 10 then  //custom scan
  begin
    //CreateCustomScanButton: TButton;
    //EditCustomScanButton: TButton;

    if CreateCustomScanButton=nil then
    begin
      //spawn it
      CreateCustomScanButton:=TButton.Create(self);
      with CreateCustomScanButton do
      begin
        width:=50;
        height:=19;
        left:=ScanType.left+ScanType.Width+3;
        top:=ScanType.Top+(scantype.Height div 2)-(height div 2);

        anchors:=[akTop,akRight];
        caption:='New';
        parent:=self;
        onclick:=CreateCustomScanButtonClick;
      end;

      EditCustomScanButton:=TButton.Create(self);
      with EditCustomScanButton do
      begin
        width:=50;
        height:=19;
        left:=CreateCustomScanButton.left+CreateCustomScanButton.Width+3;
        top:=CreateCustomScanButton.Top;

        anchors:=[akTop,akRight];
        caption:='Edit';
        parent:=self;
        onclick:=EditCustomScanButtonClick;
      end;
    end;
  end
  else
  begin

    if CreateCustomScanButton<>nil then //free it
    begin

      freeandnil(CreateCustomScanButton);

      if EditCustomScanButton<>nil then
        freeandnil(EditCustomScanButton);
    end;


  end;

  if (oldtext=strUnknownInitialValue) and (NextScanButton.enabled) then scantype.itemindex:=0 else scantype.itemindex:=oldindex;

  if (scantype.text=strIncreasedValueBy) or (scantype.text=strDecreasedValueBy) then
  begin
    if cbpercentage=nil then
    begin
      cbpercentage:=tcheckbox.create(self);
      cbpercentage.Left:=scantype.Left+scantype.Width+5;
      cbpercentage.Top:=scantype.Top+2;
      cbpercentage.Caption:='at least xx%';
      cbpercentage.Anchors:=[akTop,akRight];
      cbpercentage.Width:=80;
      cbpercentage.Parent:=scantype.Parent;
    end;
  end
  else
  begin
    if cbpercentage<>nil then
      freeandnil(cbpercentage);
  end;

  if scantype.Text=strValueBetween then
  begin
    if scanvalue2=nil then
    begin
      //decrease the width of the scanvalue editbox
      oldwidth:=scanvalue.width;
      scanvalue.Width:=(scanvalue.Width div 2)-20;

      //create a 2nd editbox
      scanvalue2:=tedit.create(self);
      //scanvalue2.onkeydown:=scanvalueKeyDown;
      scanvalue2.OnKeyPress:=ScanvalueoldKeyPress;
      scanvalue2.PopupMenu:=ccpmenu;
      scanvalue2.Left:=scanvalue.left+scanvalue.Width+20;
      scanvalue2.Width:=oldwidth-scanvalue.width-20;
      scanvalue2.Top:=scanvalue.top;
      scanvalue2.Parent:=scanvalue.Parent;
      scanvalue2.Anchors:=scanvalue.Anchors;

      scantext2:=tlabel.create(self);
      scantext2.caption:=scantext.caption;
      scantext2.Left:=scanvalue2.Left;
      scantext2.Top:=scantext.top;
      scantext2.Parent:=scantext.parent;

      andlabel:=tlabel.Create(self);
      andlabel.Caption:='and';
      andlabel.Left:=scanvalue2.Left-20;
      andlabel.Top:=scanvalue2.Top+2;
      andlabel.Parent:=scanvalue2.Parent;
    end
  end else
  begin
    if scanvalue2<>nil then
    begin
      scanvalue.Width:=scanvalue.width+20+scanvalue2.width;
      freeandnil(scanvalue2);
      freeandnil(scantext2);
      freeandnil(andlabel);
    end;
  end;


  if (scantype.Text=strIncreasedValue) or
     (scantype.text=strDecreasedValue) or
     (scantype.Text=strChangedValue) or
     (scantype.Text=strUnchangedValue) or
     (scantype.Text=strUnknownInitialValue) or
     (scantype.Text=strSameAsFirstScan)
     then
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

  pnlfloat.Visible:=floatvis;

end;


function TMainform.getStringFromRecord(itemid: Integer):string;
var i,j,k: Integer;
    read1: byte;
    read2: word;
    read3: dword;
    read4: single;
    read5: double;
    read6: Int64;
    read7: pchar;
    read72: pwidechar;
    read8: array of byte;
    read9: pbyte;

    count: dword;
    rec: Integer;
    temp,temp2: string;

    nrofbytes: integer;

    realaddress,realaddress2: dword;
    stopnow: boolean;
    check: boolean;

begin
  rec:=itemid;
  if not memrec[rec].frozen then
  begin
    if memrec[rec].IsPointer then
    begin
      //find the real address
      realaddress2:=memrec[rec].pointers[length(memrec[rec].pointers)-1].Address;
      for j:=length(memrec[rec].pointers)-1 downto 0 do
      begin

        check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
        if check and (count=4) then
          realaddress2:=realaddress+memrec[rec].pointers[j].offset
        else
        begin
          result:='??';
          exit;
        end;

      end;

      realaddress:=realaddress2;
    end else realaddress:=memrec[rec].Address;


  //get the value!!!


    case memrec[rec].VarType of
    0:            begin //byte
                    check:=readprocessmemory(processhandle,pointer(realaddress),addr(read1),1,count);
                    if (not check) or (count=0) then result:='??' else
                      begin
                        if memrec[rec].ShowAsHex then
                          result:='0x'+IntToHex(read1,2)
                        else
                        if formsettings.cbShowAsSigned.checked then result:=IntToStr(ShortInt(read1))
                        else result:=IntToStr(read1);
                      end;
                  end;

    1:            begin //word
                    check:=readprocessmemory(processhandle,pointer(realaddress),addr(read2),2,count);
                    if (not check) or (count=0) then result:='??' else
                      begin
                        if memrec[rec].ShowAsHex then
                          result:='0x'+IntToHex(read2,4)
                        else
                        if formsettings.cbShowAsSigned.checked then result:=IntToStr(SmallInt(read2))
                        else result:=IntToStr(read2);
                      end;
                  end;

    2:            begin //dword
                    read3:=0;
                    check:=readprocessmemory(processhandle,pointer(realaddress),addr(read3),4,count);

                    if (not check) or (count=0) then result:='??' else
                      begin
                        if memrec[rec].ShowAsHex then
                          result:='0x'+IntToHex(read3,8)
                        else
                        if formsettings.cbShowAsSigned.checked then result:=IntToStr(Longint(read3))
                        else result:=IntToStr(read3);
                      end;
                  end;

    3:            begin //float
                    check:=readprocessmemory(processhandle,pointer(realaddress),addr(read4),4,count);
                    if (not check) or (count=0) then result:='??' else
                      result:=FloatToStr(read4);
                  end;

    4:            begin  //double
                    check:=readprocessmemory(processhandle,pointer(realaddress),addr(read5),8,count);
                    if (not check) or (count=0) then result:='??' else
                      result:=FloatToStr(read5);
                  end;

    5:            begin //binary
                    //read the bytes

                    nrofbytes:=1+((memrec[rec].Bit+memrec[rec].bitlength) div 8);
                    setlength(read8,nrofbytes);

                    check:=readprocessmemory(processhandle,pointer(realaddress),addr(read8[0]),nrofbytes,count);
                    if (not check) or (count=0) then result:='??' else
                    begin
                      //convert what i need to a string of bits
                      temp:='';
                      j:=memrec[rec].Bit;
                      read9:=@read8[0];
                      for k:=1 to memrec[rec].bitlength do
                      begin
                        temp:=temp+IntToStr(getbit(j,read9^));
                        inc(j);
                        if j>=8 then
                        begin
                          j:=0;
                          inc(read9);
                        end;
                      end;

                      temp2:='';
                      for k:=length(temp) downto 1 do
                        temp2:=temp2+temp[k];

                      if formsettings.cbBinariesAsDecimal.checked then
                      begin
                        try
                          result:=IntToStr(bintoint(temp2));
                        except
                          result:=strtoolong;
                        end;
                      end else result:=temp2;

                      //and convert them to a decimal value
                    end;
                  end;

    6:            begin //Int64
                    check:=readprocessmemory(processhandle,pointer(realaddress),addr(read6),8,count);
                    if (not check) or (count=0) then result:='??' else
                      begin
                        if memrec[rec].ShowAsHex then
                          result:='0x'+IntToHex(read6,16)
                        else
                          result:=IntToStr(read6);
                      end;
                  end;

    7:            begin  //text
                    if memrec[rec].unicode then
                    begin
                      getmem(read72,memrec[rec].Bit*2+2);
                      check:=readprocessmemory(processhandle,pointer(realaddress),read72,memrec[rec].Bit*2,count);
                      if (not check) or (count<memrec[rec].Bit) then result:='??' else
                      begin
                        read72[memrec[rec].Bit]:=chr(0);
                        result:=read72;
                      end;
                      freemem(read72);
                    end
                    else
                    begin
                      getmem(read7,memrec[rec].Bit+1);
                      check:=readprocessmemory(processhandle,pointer(realaddress),read7,memrec[rec].Bit,count);
                      if (not check) or (count<memrec[rec].Bit) then result:='??' else
                      begin
                        read7[memrec[rec].Bit]:=chr(0);
                        result:=read7;
                      end;
                      freemem(read7);
                    end;
                  end;

    8:            begin //array of byte
                    setlength(read8,memrec[rec].Bit);
                    check:=readprocessmemory(processhandle,pointer(realaddress),read8,memrec[rec].Bit,count);

                    if (not check) or (count<memrec[rec].Bit) then result:='??' else
                    begin
                      temp:='';
                      for j:=0 to memrec[rec].Bit-1 do
                        temp:=temp+IntToHex(read8[j],2)+' ';

                      result:=temp;
                    end;

                    setlength(read8,0);
                  end;

    end;
  end else
  begin
    if memrec[rec].ShowAsHex then
    begin
      if memrec[rec].VarType=0 then result:=IntToHex(memrec[rec].frozenvalue,2) else
      if memrec[rec].VarType=1 then result:=IntToHex(memrec[rec].frozenvalue,4) else
      if memrec[rec].VarType=2 then result:=IntToHex(memrec[rec].frozenvalue,8);
    end
    else
    begin
      if formsettings.cbShowAsSigned.checked then
      begin
        if memrec[rec].VarType=0 then result:=IntToStr(ShortInt(memrec[rec].frozenvalue)) else
        if memrec[rec].VarType=1 then result:=IntToStr(SmallInt(memrec[rec].frozenvalue)) else
        if memrec[rec].VarType=2 then result:=IntToStr(LongInt(memrec[rec].frozenvalue));
      end else
      begin
        if memrec[rec].VarType=0 then result:=IntToStr(byte(memrec[rec].frozenvalue)) else
        if memrec[rec].VarType=1 then result:=IntToStr(word(memrec[rec].frozenvalue)) else
        if memrec[rec].VarType=2 then result:=IntToStr(dword(memrec[rec].frozenvalue));
      end;
    end;



    if memrec[rec].VarType=3 then  //signle
    begin
      read4:=FrozenFvalue[rec];
      result:=FloatToStr(read4);
    end;

    if memrec[rec].VarType=4 then  //double
    begin
      read5:=FrozenFvalue[rec];
      result:=FloatToStr(read5);
    end;

    if memrec[rec].Vartype=5 then //binary
    begin
      //turn arround
      temp2:=frozenstrings[rec];

      temp:='';
      for j:=length(temp2) downto 1 do
        temp:=temp+temp2[j];

      if formsettings.cbBinariesAsDecimal.checked then
        result:=IntToStr(bintoint(temp))
      else
        result:=temp;
    end;

    if memrec[rec].VarType=6 then  //int64
    begin
      if memrec[rec].ShowAsHex then
        result:=IntToHex(memrec[rec].frozenvalue,1)
      else
        result:=IntToStr((memrec[rec].frozenvalue));
    end;

    if memrec[rec].VarType=7 then //text
    begin
      result:=frozenstrings[rec];
    end;

    if memrec[rec].VarType=8 then //array of byte
    begin
      temp:='';
      for j:=0 to length(frozenbytes[rec])-1 do
        temp:=temp+IntToHex(FrozenBytes[rec][j],2)+' ';
      result:=temp;
    end;

  end;

end;

procedure TMainform.reinterpretaddresses;
var i: integer;
    a: dword;
begin
  //a:=gettickcount;

  try
    if assigned(formscanning) and formscanning.Visible then exit; //dont do it during a scan (slow)
  except

  end;

  //update reinterpetable addresses
  for i:=0 to numberofrecords-1 do
  begin
    if memrec[i].interpretableaddress<>'' then
    begin
      try
        memrec[i].address:=symhandler.getAddressFromName(memrec[i].interpretableaddress,false); //don't wait for symbols here
      except

      end;
    end;

    if memrec[i].IsPointer and (memrec[i].pointers[length(memrec[i].pointers)-1].interpretableaddress<>'') then
    begin
      try
        memrec[i].pointers[length(memrec[i].pointers)-1].Address:=symhandler.getAddressFromName(memrec[i].pointers[length(memrec[i].pointers)-1].interpretableaddress,false);
      except

      end;
    end;
  end;


  //showmessage(inttostr(gettickcount-a));  //always 0 ticks...
end;

procedure TMainForm.UpdateList;
var i,j,k: Integer;
    rec: Integer;
    count: dword;

    realaddress,realaddress2: dword;
    stopnow: boolean;
    check: boolean;

begin
//

  for i:=0 to (numberoflines-1) do
  begin
    stopnow:=false;

    rec:=scrollbar1.Position+i;
    if (rec)<numberofrecords then
    begin
      if memrec[rec].IsPointer then
      begin
        //find the real address
        realaddress2:=memrec[rec].pointers[length(memrec[rec].pointers)-1].Address;
        for j:=length(memrec[rec].pointers)-1 downto 0 do
        begin
          count:=0;
          check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
          if check and (count=4) then
            realaddress2:=realaddress+memrec[rec].pointers[j].offset
          else
          begin
            value[i].Caption:='??';
            address[i].Caption:='P->????????';
            stopnow:=true;
            break;
          end;

        end;
        if stopnow then continue;

        realaddress:=realaddress2;
        address[i].Caption:='P->'+IntToHex(realaddress,8);
      end else realaddress:=memrec[rec].Address;


    //get the value!!!
      if memrec[rec].VarType=255 then
        value[i].Caption:=''
      else
        value[i].Caption:=getStringFromRecord(rec)


    end;
  end;
end;

procedure TMainForm.FreezeThem;
var i,j,k: Integer;
    nrofbytes: integer;
    write: dword;
    write1: byte;
    write2: word;
    write3: dword;
    write4: single;
    write5: double;
    write6: Int64;

    write8: array of byte;
    write9: pbyte;

    writes: pchar;
    error: boolean;

    realaddress,realaddress2: dword;
    count:dword;
    check:boolean;

    originalprotection: dword;
begin
  for i:=0 to numberofrecords-1 do
    if memrec[i].frozen then
    begin
      if memrec[i].IsPointer then
      begin
        //find the real address
        realaddress2:=memrec[i].pointers[length(memrec[i].pointers)-1].Address;
        for j:=length(memrec[i].pointers)-1 downto 0 do
        begin
          check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
          if check and (count=4) then
            realaddress2:=realaddress+memrec[i].pointers[j].offset
          else
            continue;
        end;
        realaddress:=realaddress2;
      end else realaddress:=memrec[i].Address;

      error:=false;

      if memrec[i].vartype<255 then //make writable
        VirtualProtectEx(Processhandle,pointer(realaddress),8,PAGE_EXECUTE_READWRITE, originalprotection);

      case memrec[i].VarType of
        0:      begin
                  if memrec[i].Frozendirection=0 then
                  begin
                    write1:=byte(memrec[i].FrozenValue);
                    writeprocessmemory(processhandle,pointer(realaddress),addr(write1),1,write);
                  end
                  else
                  begin
                    readprocessmemory(processhandle,pointer(realaddress),@write1,1,write);
                    if write=1 then
                    if memrec[i].Frozendirection=1 then
                    begin
                      if hotkeypressed=i then
                      begin
                        write1:=write1-memrec[i].FrozenValue; //decrease with frozenvalue
                        writeprocessmemory(processhandle,pointer(realaddress),addr(write1),1,write);
                      end
                      else
                      begin
                        //allow decreased values
                        if write1<=byte(memrec[i].FrozenValue) then memrec[i].FrozenValue:=write1
                        else
                        begin
                          write1:=byte(memrec[i].FrozenValue);
                          writeprocessmemory(processhandle,pointer(realaddress),addr(write1),1,write);
                        end;
                      end
                    end
                    else
                    begin
                      //allow increased values
                      if hotkeypressed=i then
                      begin
                        write1:=write1+memrec[i].FrozenValue; //increase with frozenvalue
                        writeprocessmemory(processhandle,pointer(realaddress),addr(write1),1,write);
                      end
                      else
                      begin
                        if write1>=byte(memrec[i].FrozenValue) then memrec[i].FrozenValue:=write1
                        else
                        begin
                          write1:=byte(memrec[i].FrozenValue);
                          writeprocessmemory(processhandle,pointer(realaddress),addr(write1),1,write);
                        end;
                      end;
                    end;
                  end;
                  error:=write<>1;
                end;

        1:      begin
                  if memrec[i].Frozendirection=0 then
                  begin
                    write2:=word(memrec[i].FrozenValue);
                    writeprocessmemory(processhandle,pointer(realaddress),addr(write2),2,write);
                  end
                  else
                  begin
                    readprocessmemory(processhandle,pointer(realaddress),@write2,2,write);
                    if write=2 then
                    if memrec[i].Frozendirection=1 then
                    begin
                      //allow decreased values
                      if hotkeypressed=i then
                      begin
                        write2:=write2-memrec[i].FrozenValue; //decrease with frozenvalue
                        writeprocessmemory(processhandle,pointer(realaddress),addr(write2),2,write);
                      end
                      else
                      begin
                        if write2<=word(memrec[i].FrozenValue) then memrec[i].FrozenValue:=write2
                        else
                        begin
                          write2:=word(memrec[i].FrozenValue);
                          writeprocessmemory(processhandle,pointer(realaddress),addr(write2),2,write);
                        end;
                      end;
                    end
                    else
                    begin

                      if hotkeypressed=i then
                      begin
                        write2:=write2+memrec[i].FrozenValue;
                        writeprocessmemory(processhandle,pointer(realaddress),addr(write2),2,write);
                      end
                      else
                      begin
                        //allow increased values
                        if write2>=word(memrec[i].FrozenValue) then memrec[i].FrozenValue:=write2
                        else
                        begin
                          write2:=word(memrec[i].FrozenValue);
                          writeprocessmemory(processhandle,pointer(realaddress),addr(write2),2,write);
                        end;
                      end;
                    end;
                  end;
                  error:=write<>2;
                end;

        2:      begin
                  if memrec[i].Frozendirection=0 then
                  begin
                    write3:=memrec[i].FrozenValue;
                    writeprocessmemory(processhandle,pointer(realaddress),addr(write3),4,write);
                  end
                  else
                  begin
                    readprocessmemory(processhandle,pointer(realaddress),@write3,4,write);
                    if write=4 then
                    if memrec[i].Frozendirection=1 then
                    begin
                      if hotkeypressed=i then
                      begin
                        write3:=write3-memrec[i].FrozenValue;
                        writeprocessmemory(processhandle,pointer(realaddress),addr(write3),4,write);
                      end
                      else
                      begin
                        //allow decreased values
                        if write3<=dword(memrec[i].FrozenValue) then memrec[i].FrozenValue:=write3
                        else
                        begin
                          write3:=dword(memrec[i].FrozenValue);
                          writeprocessmemory(processhandle,pointer(realaddress),addr(write3),4,write);
                        end;
                      end;
                    end
                    else
                    begin
                      //allow increased values
                      if hotkeypressed=i then
                      begin
                        write3:=write3+memrec[i].FrozenValue;
                        writeprocessmemory(processhandle,pointer(realaddress),addr(write3),4,write);
                      end
                      else
                      begin

                        if write3>=dword(memrec[i].FrozenValue) then memrec[i].FrozenValue:=write3
                        else
                        begin
                          write3:=dword(memrec[i].FrozenValue);
                          writeprocessmemory(processhandle,pointer(realaddress),addr(write3),4,write);
                        end;
                      end;
                    end;
                  end;
                  error:=write<>4;
                end;

        3:      begin  //float
                  if memrec[i].Frozendirection=0 then
                  begin
                    write4:=FrozenFValue[i];
                    writeprocessmemory(processhandle,pointer(realaddress),addr(write4),4,write);
                  end
                  else
                  begin
                    readprocessmemory(processhandle,pointer(realaddress),@write4,4,write);
                    if write=4 then
                    if memrec[i].Frozendirection=1 then
                    begin
                      if hotkeypressed=i then
                      begin
                        write4:=write4-FrozenFValue[i];
                        writeprocessmemory(processhandle,pointer(realaddress),addr(write4),4,write);
                      end
                      else
                      begin
                        //allow decreased values
                        if write4<=FrozenFValue[i] then FrozenFValue[i]:=write4
                        else
                        begin
                          write4:=FrozenFValue[i];
                          writeprocessmemory(processhandle,pointer(realaddress),addr(write4),4,write);
                        end;
                      end;
                    end
                    else
                    begin
                      if hotkeypressed=i then
                      begin
                        write4:=write4+FrozenFValue[i];
                        writeprocessmemory(processhandle,pointer(realaddress),addr(write4),4,write);
                      end
                      else
                      begin

                        //allow increased values
                        if write4>=FrozenFValue[i] then FrozenFValue[i]:=write4
                        else
                        begin
                          write4:=FrozenFValue[i];
                          writeprocessmemory(processhandle,pointer(realaddress),addr(write4),4,write);
                        end;
                      end;
                    end;
                  end;
                  error:=write<>4;
                end;

        4:      begin //double
                  if memrec[i].Frozendirection=0 then
                  begin
                    write5:=FrozenFValue[i];
                    writeprocessmemory(processhandle,pointer(realaddress),addr(write5),8,write);
                  end
                  else
                  begin
                    readprocessmemory(processhandle,pointer(realaddress),@write5,8,write);
                    if write=8 then
                    if memrec[i].Frozendirection=1 then
                    begin
                      if hotkeypressed=i then
                      begin
                        write5:=write5-FrozenFValue[i];
                        writeprocessmemory(processhandle,pointer(realaddress),addr(write5),8,write);
                      end
                      else
                      begin
                        //allow decreased values
                        if write5<=FrozenFValue[i] then FrozenFValue[i]:=write5
                        else
                        begin
                          write5:=FrozenFValue[i];
                          writeprocessmemory(processhandle,pointer(realaddress),addr(write5),8,write);
                          error:=write<>8;
                        end;
                      end;
                    end
                    else
                    begin
                      //allow increased values
                      if hotkeypressed=i then
                      begin
                        write5:=write5+FrozenFValue[i];
                        writeprocessmemory(processhandle,pointer(realaddress),addr(write5),8,write);
                      end
                      else
                      begin
                        if write5>=FrozenFValue[i] then FrozenFValue[i]:=write5
                        else
                        begin
                          write5:=FrozenFValue[i];
                          writeprocessmemory(processhandle,pointer(realaddress),addr(write5),8,write);
                          error:=write<>8;
                        end;
                      end;
                    end;
                  end;
                  error:=write<>8;
                end;

        5:      begin //bit  (no up or down)
                  nrofbytes:=1+((memrec[i].Bit+memrec[i].bitlength) div 8);
                  setlength(write8,nrofbytes);
                  readprocessmemory(processhandle,pointer(realaddress),addr(write8[0]),nrofbytes,write);

                  j:=memrec[i].Bit;
                  write9:=@write8[0];
                  for k:=1 to memrec[i].bitlength do
                  begin
                    setbit(j,write9^,StrToInt(frozenstrings[i][k]));
                    inc(j);
                    if j>=8 then
                    begin
                      j:=0;
                      inc(write9);
                    end;
                  end;


                  writeprocessmemory(processhandle,pointer(realaddress),addr(write8[0]),nrofbytes,write);
                  error:=write<>nrofbytes;
                end;

        6:      begin  //int64
                  if memrec[i].Frozendirection=0 then
                  begin
                    write6:=memrec[i].FrozenValue;
                    writeprocessmemory(processhandle,pointer(realaddress),addr(write6),8,write);
                  end
                  else
                  begin
                    readprocessmemory(processhandle,pointer(realaddress),@write6,8,write);
                    if write=8 then
                    if memrec[i].Frozendirection=1 then
                    begin
                      if hotkeypressed=i then
                      begin
                        write6:=write6-memrec[i].FrozenValue;
                        writeprocessmemory(processhandle,pointer(realaddress),addr(write6),8,write);
                      end
                      else
                      begin
                        //allow decreased values
                        if write6<=memrec[i].FrozenValue then memrec[i].FrozenValue:=write6
                        else
                        begin
                          write6:=memrec[i].FrozenValue;
                          writeprocessmemory(processhandle,pointer(realaddress),addr(write6),8,write);
                        end;
                      end;
                    end
                    else
                    begin
                      if hotkeypressed=i then
                      begin
                        write6:=write6-memrec[i].FrozenValue;
                        writeprocessmemory(processhandle,pointer(realaddress),addr(write6),8,write);
                      end
                      else
                      begin
                        //allow increased values
                        if write6>=memrec[i].FrozenValue then memrec[i].FrozenValue:=write6
                        else
                        begin
                          write6:=memrec[i].FrozenValue;
                          writeprocessmemory(processhandle,pointer(realaddress),addr(write6),8,write);
                        end;
                      end;
                    end;
                  end;
                  error:=write<>8;
                end;

        7:      begin  //text (no up or down)
                  getmem(writes,memrec[i].Bit+1);

                  StrCopy(writes, PChar(frozenstrings[i]));

                  writeprocessmemory(processhandle,pointer(realaddress),writes,memrec[i].bit,write);

                  freemem(writes);      //if this was heroes 4 v1.0 I would have to remove all freemem statements ;)
                  error:=write<>memrec[i].bit;
                end;

        8:      begin  //array of bytes (no up or down)
                  writeprocessmemory(processhandle,pointer(realaddress),@frozenbytes[i][0],memrec[i].bit,write);
                  error:=write<>memrec[i].bit;
                end;

      end;

      if memrec[i].vartype<255 then //restore
        VirtualProtectEx(Processhandle, pointer(realaddress), 8, originalprotection, originalprotection);

      if error then
      begin
        if not memrec[i].IsPointer then
        begin
          memrec[i].Frozen:=false;
          if (i>=scrollbar1.Position) and (i<=scrollbar1.position+(numberoflines-1)) then updatescreen;
        end; //else keep on freezng as if nothing happened
      end;

    end;
end;

procedure TMainform.AddAutoAssembleScript(script:string);
var error: Integer;
    Address: Dword;
    i,j:  Integer;
    bit: Integer;
    tmp: string;
    pc: ^char;

    found: boolean;
    bitl: integer;

begin
//first check if this address is already in the list!
  inc(NumberOfRecords);
  ReserveMem;

  memrec[NumberOfRecords-1].Description:='Auto assemble cheat';
  memrec[NumberOfRecords-1].Address:=0;
  memrec[NumberOfRecords-1].VarType:=255;

  memrec[NumberOfRecords-1].Frozen:=false;
  memrec[NumberOfRecords-1].FrozenValue:=0;

  memrec[NumberOfRecords-1].Bit:=0;
  memrec[NumberOfRecords-1].bitlength:=0;
  memrec[NumberOfRecords-1].showAsHex:=false;
  memrec[NumberOfRecords-1].autoassemblescript:=script;

  hotkeys[NumberOfRecords-1]:=-1;
  memrec[NumberOfRecords-1].unicode:=false;

  updatescreen;
  updatelist;

  scrollbar1.Position:=scrollbar1.Max-numberoflines+2;
end;

Procedure TMainForm.AddToRecord(line:Integer);
var error: Integer;
    Address: Dword;
    i,j:  Integer;
    bit: Integer;
    tmp: string;
    pc: ^char;

    found: boolean;
    bitl: integer;

    realvartype: integer;
    tempvartype: TVariableType;
begin
//first check if this address is already in the list!

  realvartype:=getvartype;
  if realvartype=5 then //binary
  begin
    bit:=foundlist.getstartbit(line);
    bitl:=foundlist.GetVarLength;
  end
  else
  if realvartype=9 then //all
  begin
    bit:=0;
    bitl:=0;  
    tempvartype:=TVariableType(foundlist.getstartbit(line));
    case tempvartype of
      vtByte: realvartype:=0;
      vtWord: realvartype:=1;
      vtDWord: realvartype:=2;
      vtQWord: realvartype:=6;
      vtSingle: realvartype:=3;
      vtDouble: realvartype:=4;
    end;

  end else
  if realvartype=10 then //custom
  begin
    realvartype:=2;
  end
  else
  begin
    bit:=0;
    bitl:=0;
    bit:=foundlist.GetVarLength;
  end;
  address:=foundlist.GetAddress(line);

  inc(NumberOfRecords);
  ReserveMem;

  memrec[NumberOfRecords-1].Description:=strNoDescription;
  memrec[NumberOfRecords-1].Address:=address;
  memrec[NumberOfRecords-1].IsPointer:=false;
  setlength(memrec[NumberOfRecords-1].pointers,0);

  if getscantype=string_scan then memrec[NumberOfRecords-1].VarType:=0 else
                                  memrec[NumberOfRecords-1].VarType:=realvartype;

  memrec[NumberOfRecords-1].Frozen:=false;
  memrec[NumberOfRecords-1].FrozenValue:=0;

  memrec[NumberOfRecords-1].Bit:=bit;
  memrec[NumberOfRecords-1].bitlength:=bitl;
  memrec[NumberOfRecords-1].showAsHex:=hexadecimalcheckbox.checked;

  hotkeys[NumberOfRecords-1]:=-1;

  if memrec[NumberOfRecords-1].VarType=7 then //text
    memrec[NumberOfRecords-1].unicode:=cbunicode.Checked;

  if foundlist.inmodule(line) then
    memrec[NumberOfRecords-1].interpretableaddress:=foundlist.GetModuleNamePlusOffset(line)
  else
    memrec[NumberOfRecords-1].interpretableaddress:='';

  updatescreen;
  updatelist;

  scrollbar1.Position:=scrollbar1.Max-numberoflines+2;
end;

procedure TMainForm.SetExpectedTableName;
var fname: string;
    expectedfilename: string;
begin
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


procedure TMainForm.ShowProcessListButtonClick(Sender: TObject);
var oldprocess: Dword;
    resu: integer;
    i,j: integer;

    startnewscan: boolean;
    oldprocesshandle: thandle;

    oldprocessname: string;
    newprocessname: string;

    modulelist: tstringlist;
resourcestring
  strConfirmProcessTermination='This will close the current process. Are you sure you want to do this?';
  strError='Error';
  strErrorwhileOpeningProcess='Error while opening this process';
  strWantANewScan='Do you want to start a new scan?';
  strKeepList='Keep the current address list/code list?';
  strInfoAboutTable='Info about this table:';
  strPhysicalMemory='Physical Memory';


begin
  if formsettings.cbUndoMemoryChanges.checked then CheckForChanges; //place this line at important places

  if flashprocessbutton<>nil then
  begin
    flashprocessbutton.Terminate;
    flashprocessbutton:=nil;
  end;

  oldprocessname:=copy(mainform.ProcessLabel.Caption,pos('-',mainform.ProcessLabel.Caption)+1,length(mainform.ProcessLabel.Caption));

  if (debuggerthread<>nil) and (debuggerthread.attached) then
  begin
    if @DebugActiveProcessStop=@DebugActiveProcessStopProstitute then
    begin
      if messagedlg(strConfirmProcessTermination,mtWarning,[mbYes ,MbNo],0)=mrNo then exit else
      begin
        //whipe out the watchlist
        debuggerthread.debugging:=false;
        terminateprocess(processhandle,0);
      end;
    end;
  end;

  canceled:=false;

  oldprocess:=processID;
  ProcessWindow:=TProcessWindow.Create(nil);
  //for the people that play arround with my source, this causes a external exception while running inside the delphi IDE, outside it gives no problems...

  oldprocesshandle:=processhandle;
  resu:=ProcessWindow.ShowModal;
  ProcessWindow.free;
  ProcessWindow:=nil;

  newprocessname:=copy(mainform.ProcessLabel.Caption,pos('-',mainform.ProcessLabel.Caption)+1,length(mainform.ProcessLabel.Caption));

  if (debuggerthread<>nil) and (debuggerthread.attached) then
  begin
    memorybrowser.updateregisterview;
    AdvancedOptions.UpdateAdvancedOptions;
  end;


  if resu=mrCancel then exit;

  symhandler.reinitialize;
  reinterpretaddresses;

  if oldprocess=0 then //set disassembler and hexview of membrowser to what the main header says
    memorybrowser.setcodeanddatabase;
  
  if resu=mryes then
  begin
    try
      cbfasterscan.Checked:=true;
      foundcount:=0;
      foundlist.Clear;

      newscan.Caption:=strFirstScan;

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
      cbCaseSensitive.Enabled:=true;

      scanvalue.visible:=true;
      scantext.Visible:=true;
      scanvalue.text:='';


      Updatescantype;
      Scantype.ItemIndex:=0;

      //-----------------------
      //Get the expectedFilename
      //-----------------------
      SetExpectedTableName;



    except
      processlabel.Caption:=strError;
      raise exception.Create(strErrorWhileOpeningProcess);
    end;

    exit;
  end;


  if (processhandle=0)then
  begin
    if newscan.Caption=strNewScan then newscan.click;

    //disable everything
    foundcount:=0;
    foundlist.Clear;

    newscan.Caption:=strFirstScan;

    Groupbox1.Enabled:=false;
    for i:=0 to groupbox1.ControlCount-1 do
      groupbox1.Controls[i].Enabled:=false;


    scanvalue.Enabled:=false;
    newscan.Enabled:=false;
    nextscanbutton.Enabled:=false;
    vartype.Enabled:=false;
    scantype.Enabled:=false;
    scantext.enabled:=false;
    label4.enabled:=false;
    label8.Enabled:=false;

    scanvalue.visible:=false;
    scantext.Visible:=false;
    scanvalue.text:='';
    HexadecimalCheckbox.Enabled:=false;
    cbCaseSensitive.Enabled:=false;

    Updatescantype;
    Scantype.ItemIndex:=0;

    if cefuncproc.hypermode<>nil then
      freeandnil(cefuncproc.hypermode);

    cbSpeedhack.Enabled:=false;
    cbUnrandomizer.Enabled:=false;



    if processid<>$FFFFFFFF then
    begin
      processlabel.Caption:=strError;
      raise exception.Create(strErrorWhileOpeningProcess);
    end else
    begin
      processlabel.Caption:=strPhysicalMemory;
    end;

    UpdateScanType;
  end;

  if (processID=oldProcess) then exit;



  //a new process has been selected
  cbspeedhack.enabled:=true;
  cbunrandomizer.Enabled:=true;


  startnewscan:=true;
  if (newscan.Caption=strNewScan) and (newprocessname=oldprocessname) then
    startnewscan:=messagedlg(strWantANewScan,mtConfirmation,[mbyes,mbno],0)=mryes;

  if (numberofrecords>0) or (advancedoptions.codelist.Count>0) then
  begin
    if (messagedlg(strKeepList, mtConfirmation, [mbYes, mbNo], 0)=mrNo) then
    begin
      //no, clear the list
      Comments.Memo1.Clear;
      comments.Memo1.Lines.Add(strInfoAboutTable);
      Numberofrecords:=0;
      advancedoptions.codelist.clear;
      advancedoptions.numberofcodes:=0;
      Updatescreen;
      Updatelist;

    end
    else
    begin
      //yes, so keep the list
      //go through the list and chek for auto assemble entries, and check if one is enabled. If so, ask to disable (withotu actually disabling)
      for i:=0 to numberofrecords-1 do
        if (memrec[i].VarType=255) and (memrec[i].Frozen) then
        begin
          if (messagedlg('There are one or more auto assembler entries enabled in this table. Do you want them disabled? (without executing the disable part)', mtConfirmation, [mbYes, mbNo], 0)=mryes) then
          begin
            for j:=0 to numberofrecords-1 do
              if (memrec[j].VarType=255) and (memrec[j].Frozen) then
                memrec[j].Frozen:=false;

            Updatescreen;
          end;
          break;
        end;
    end;

  end;

  if startnewscan then
  begin
    if (copy(processlabel.caption,length(processlabel.caption)-11,12)='WINOA386.MOD') or
       (copy(processlabel.caption,pos('-',processlabel.caption)+1,13)='MS-DOS Prompt') then
    begin
      fromaddress.text:='80000000';
      toaddress.text:='BFFFFFFF';
      dos.checked:=true;
    end else
    begin
      if processid=$FFFFFFFF then
      begin
        allclick.Checked:=true;
        FromAddress.Text:='00000000';
        ToAddress.Text:='FFFFFFFF';
      end
      else
      begin
        FromAddress.text:='00400000';
        ToAddress.text:='7FFFFFFF';
        windows.checked:=true;
      end;
    end;

    enablegui(false);
  end;

  UpdateScanType;  
end;

procedure TMainform.aprilfoolsscan;
begin
  if aprilfools then
  begin
    if messagedlg('Thank you for trying out Cheat Engine. Because it has expired Cheat Engine will now close. Is that ok with you?',mtInformation,[mbyes,mbno],0)=mryes then
    begin
      showmessage('April fools!!!!');


    end
    else
    begin
      if messagedlg('WHAT!!! Are you saying you''re going to continue using CE ILLEGALLY??? If you say yes, i''m going to mail the cops to get you and send you to jail!!!',mtwarning,[mbyes,mbno],0)=mryes then
        showmessage('Hrmpf... Because I''m in a good mood i''ll let you go this time. But don''t do it again you filthy pirate')
      else
        showmessage('April fools!!!!');
    end;

    caption:=cenorm;
    aprilfools:=false;
  end;
end;

procedure TMainform.donewscan;
var i: integer;
begin
  if SaveFirstScanThread<>nil then //stop saving the results of the fist scan
  begin
    SaveFirstScanThread.Terminate;
    SaveFirstScanThread.WaitFor;
    freeandnil(SaveFirstScanThread);
  end;


  if cbfasterscan.checked then //tell hyperscan it is a new scan
  begin
    if (cefuncproc.hypermode<>nil) and (cefuncproc.hypermode.HyperscanWindow<>0) then
      sendmessage(cefuncproc.hypermode.HyperscanWindow,WM_USER+1,0,0);
  end;

  fastscan:=formsettings.cbFastscan.checked;
  //close files in case of a bug i might have missed...
  closefiles;

  freememory;
  vartype.visible:=true;

  foundcount:=0;
  foundlist.Clear;

  newscan.Caption:=strFirstScan;

  nextscanbutton.Enabled:=false;
  vartype.Enabled:=true;

  scanvalue.visible:=true;
  scantext.Visible:=true;

  Updatescantype;
  Scantype.ItemIndex:=0;

  //enable the memory scan groupbox 
  Groupbox1.Enabled:=true;
  for i:=0 to groupbox1.ControlCount-1 do
    groupbox1.Controls[i].Enabled:=true;


  VartypeChange(vartype);
  setfoundlisthorizontal;

  foundlist.deleteresults;

  if scanvalue.Visible and scanvalue.Enabled then
  begin
    scanvalue.SetFocus;
    scanvalue.SelectAll;
  end
end;


procedure TMainForm.NewScanClick(Sender: TObject);
resourcestring strfillInSomething='Please fill in something!';
               strFirstSelectAProcess='First select a process';

var FromAdd,ToAdd:Dword;
    error: Integer;
//    Tim,Tim2: TTimeStamp;
    Vtype,SType: Integer;
    Processhandle2: Thandle;
    i,j: integer;
    value: string;


    res: word;
    extra: boolean;

    a,b: string;
    winhandle:thandle;
    winprocess: dword;
    roundingtype: tfloatscan;

    value2:string;
    mi: tmoduleinfo;
    fr: dword;

    bytes: tbytes;
begin
  if (not cbfasterscan.checked) and cbnewscanroutine.checked then
  begin
    button2.click;
    exit;
  end;
  
  
  if (cefuncproc.hypermode<>nil) and advancedoptions.Pausebutton.down then
    raise exception.Create('Please unpause the game first');

  res:=mrcancel;

  VType:=GetVarType;
  SType:=GetScanType;


  foundlist.Deinitialize;
  
  try
    if scanvalue2<>nil then value2:=scanvalue2.Text;

    if formsettings.cbUndoMemoryChanges.checked then CheckForChanges; //place this line at important places

    screen.Cursor:=crhourglass;

    advanced:=false;
    undoscan.enabled:=false;

    lastscantype:=scantype.ItemIndex;

    case self.roundingtype of
      rtrounded:        roundingtype:=rounded;
      rtExtremerounded: roundingtype:=extremerounded;
      rtTruncated:      roundingtype:=truncated;
    end;

    foundlist.Clear;

    if not nextscanbutton.Enabled then //it's a first scan
    begin
      try
        if (not cbfasterscan.Checked) and (cbPauseWhileScanning.checked) then
        begin
          advancedoptions.Pausebutton.down:=true;
          advancedoptions.Pausebutton.Click;
        end;

      if (scanvalue.visible) and (scanvalue.text='') then raise Exception.Create(strFillInSomething);
      if (scanvalue2<>nil) and (value2='') then raise exception.Create(strFillInSomething);

      if processid<>0 then
      begin
        FromAdd:=ScanStart;
        ToAdd:=ScanStop;


        //--------------------------------------------------

        if (SType=Exact_value) or (SType=String_scan) or (SType=BiggerThan) or (SType=SmallerThan) or (Stype=ValueBetween) then
        begin
          foundcountlabel.visible:=true;

          mainform.caption:=CERegionSearch;
          application.ProcessMessages;
          foundcountlabel.caption:='';

          Processhandle2:=OpenProcess(PROCESS_ALL_ACCESS,false,GetCurrentProcessId);

          try
            if formsettings.checkThread.checked or cbFasterScan.checked then
            begin
              formscanning:=TFormscanning.create(self);
              formscanning.button:=0;
              formscanning.scan:=1;
              formscanning.fromadd:=fromadd;
              formscanning.toadd:=toadd;
              formscanning.readonly:=readonly.checked;
              formscanning.stype:=stype;
              formscanning.vtype:=vtype;
              formscanning.fastscan:=fastscan;
              formscanning.scanvalue:=scanvalue.Text;
              formscanning.scanvalue2:=value2;

              formscanning.hexadecimal:=hexadecimalcheckbox.checked;
              formscanning.unicode:=cbunicode.checked;
              formscanning.LowMemoryUsage:=formsettings.cbLowMemoryUsage.checked;
              formscanning.Skip_PAGE_NOCACHE:=Skip_PAGE_NOCACHE;
              formscanning.ExtremeScan:=cbFasterscan.Checked;
              formscanning.roundingtype:=roundingtype;


              res:=formscanning.showmodal;

            end else
            begin
              foundcount:=GetMemoryRangesAndScanValue2(fr,FromAdd,ToAdd,readonly.checked,false,SType,vtype,scanvalue.text,value2,roundingtype,hexadecimalcheckbox.checked,progressbar1,fastscan,cbunicode.checked);
            end;

            if aprilfools then aprilfoolsscan;


            if vtype=5 then
              foundlist3.Columns[0].Width:=90
            else
              foundlist3.Columns[0].Width:=82;

            symhandler.loadmodulelist; //reload this to be sure

            lastwashex:=HexadecimalCheckbox.checked;

          finally

            screen.Cursor:=crdefault;
            progressbar1.Position:=0;
            mainform.Caption:=CEnorm;
            UpdateFoundlisttimer.Enabled:=true;
          end;

          if res=mrcancel then
          begin
            screen.Cursor:=crdefault;
            exit;
          end;

          Groupbox1.Enabled:=false;
          for i:=0 to groupbox1.ControlCount-1 do
            groupbox1.Controls[i].Enabled:=false;

          vartype.Enabled:=false;
          nextscanbutton.enabled:=true;
          newscan.Caption:=strNewScan;
          beep;

          UpdateScanType;

          Scantype.ItemIndex:=lastscantype;
          screen.Cursor:=crdefault;


          if SaveFirstScanThread<>nil then
          begin
            SaveFirstScanThread.Terminate;
            SaveFirstScanThread.WaitFor;
            freeandnil(SaveFirstScanThread);
          end;
          SaveFirstScanThread:=TSaveFirstScanThread.Create(false);

          exit;
        end;

        //---------------------------------------------------------

        mainform.Caption:=CEregion;
        application.ProcessMessages;



        try
          UpdateFoundlisttimer.Enabled:=false;

          if formsettings.checkThread.checked or cbFasterScan.checked then
          begin
            formscanning:=TFormscanning.create(self);
            formscanning.button:=0;
            formscanning.scan:=0;
            formscanning.fromadd:=fromadd;
            formscanning.toadd:=toadd;
            formscanning.readonly:=readonly.checked;
            formscanning.stype:=stype;
            formscanning.vtype:=vtype;
            formscanning.scanvalue:=scanvalue.Text;
            formscanning.scanvalue2:=value2;
            formscanning.fastscan:=fastscan;


            formscanning.LowMemoryUsage:=formsettings.cbLowMemoryUsage.checked;
            formscanning.Skip_PAGE_NOCACHE:=Skip_PAGE_NOCACHE;
            formscanning.ExtremeScan:=cbFasterscan.Checked;
            res:=formscanning.showmodal;


          end else
          foundcount:=GetMemoryRanges2(FromAdd,ToAdd,readonly.checked,progressbar1,vtype,fastscan);

          if aprilfools then aprilfoolsscan;
        
          //foundlabel.visible:=false;
          setfoundlisthorizontal;


        finally
          progressbar1.Position:=0;
          mainform.Caption:=CEnorm;
          UpdateFoundlisttimer.Enabled:=true;

        end;

        if res=mrcancel then
        begin
          screen.Cursor:=crdefault;
          exit;
        end;

        application.ProcessMessages;

        // if Scantype.ItemIndex=String_Scan then foundlabel.caption:=IntToStr(TextSearch(scanvalue.text,foundlist));

        //main datafiles have been generated. Now let Next Scan do the remaining work.
        if not ((Stype=Advanced_scan) or (SType=string_scan)) then NextScanbutton.click else
        begin
          Beep;
        end;

        nextscanbutton.enabled:=true;
        newscan.Caption:=strNewScan;
        vartype.enabled:=false;

        Groupbox1.Enabled:=false;
        for i:=0 to groupbox1.ControlCount-1 do
          groupbox1.Controls[i].Enabled:=false;

        scantype.ItemIndex:=lastscantype;

        //save the results of the fist scan
        if SaveFirstScanThread<>nil then
        begin
          SaveFirstScanThread.Terminate;
          SaveFirstScanThread.WaitFor;
          freeandnil(SaveFirstScanThread);
        end;
        SaveFirstScanThread:=TSaveFirstScanThread.Create(false);


          
      end else showmessage(strFirstSelectaProcess);

      finally
        if (not cbfasterscan.Checked) and (cbPauseWhileScanning.checked) then
        begin
          advancedoptions.Pausebutton.down:=false; //resume
          advancedoptions.Pausebutton.Click;
        end;
      end;



    end else  //it's a new scan
      DoNewScan;

    if stype=string_scan then nextscanbutton.enabled:=false;

    if (foundcount=0) and (SType<>Advanced_scan) then NextScanButton.enabled:=false;

    progressbar1.max:=10;
    progressbar1.Position:=0;
    UpdateScanType;
    UpdateScanType; //second time to force the repaint



    screen.Cursor:=crdefault;


  finally
    scanEpilogue(res=mrcancel);


  end;


end;

procedure TMainForm.NextScanButtonClick(Sender: TObject);
resourcestring strCantdoNextScan='You can''t do a Next Scan with the current selected way of scanning!';
var error: Integer;
    Vtype,SType: Integer;
    hexvalue: string;
    i,j: integer;

    temp,temp2: dword;
    a,b:string;
    FromAdd,ToAdd:Dword;

    roundingtype:tfloatscan;
    value2: string;
    percentage: boolean;

    mi: tmoduleinfo;

    res: word;
    bytes: tbytes;
begin
  if (not cbfasterscan.checked) and cbnewscanroutine.checked then
  begin
    button4.click;
    exit;
  end;

  if (cefuncproc.hypermode<>nil) and advancedoptions.Pausebutton.down then
    raise exception.Create('Please unpause the game first');

  if SaveFirstScanThread<>nil then
  begin
    SaveFirstScanThread.WaitFor; //wait till it's done
    freeandnil(SaveFirstScanThread);
  end;

   
  if scanvalue2<>nil then value2:=scanvalue2.Text;
  if cbpercentage<>nil then percentage:=cbpercentage.Checked;


  VType:=GetVarType;
  SType:=GetScanType;

  foundlist.Deinitialize;

  try
    if formsettings.cbUndoMemoryChanges.checked then CheckForChanges; //place this line at important places

    screen.Cursor:=crhourglass;


    if (not cbfasterscan.Checked) and (cbPauseWhileScanning.checked) then
    begin
      advancedoptions.Pausebutton.down:=true;
      advancedoptions.Pausebutton.Click;
    end;

    foundcountlabel.visible:=true;
    VType:=GetVarType;
    SType:=GetScanType;

    if rt1.checked then roundingtype:=rounded;
    if rt2.checked then roundingtype:=extremerounded;
    if rt3.Checked then roundingtype:=truncated;

    mainform.caption:=CESearch;
    application.ProcessMessages;

    try
      UpdateFoundlisttimer.Enabled:=false;

    if not((Stype=Advanced_scan) or (Stype=string_scan)) then
    begin
      if vtype=5 then
      begin
        bitoffsetchange:=0;

        //open the memoryfile end check to see if the length of the bits are what they need to be
        assignfile(Memoryfile,cheatenginedir+'MEMORY.TMP');

        reset(memoryfile,1);
        blockread(memoryfile,temp,4,temp2);
        setlength(bitscan,temp);
        blockread(memoryfile,pointer(bitscan)^,temp,temp2);
        closefile(memoryfile);

        if rbbit.Checked then
          if temp<>length(scanvalue.Text) then
          begin
            formDifferentBitSize:=tformDifferentBitSize.create(self);
            formDifferentBitSize.newbit:=scanvalue.Text;
            formDifferentBitSize.showmodal;
            bitoffsetchange:=formdifferentbitsize.delta;
            formdifferentbitsize.free;
          end;

        if rbdec.checked then
          if temp<>length(inttobin(strtoint(scanvalue.Text))) then
          begin
            formDifferentBitSize:=tformDifferentBitSize.create(self);
            formDifferentBitSize.newbit:=inttobin(strtoint(scanvalue.Text));
            formDifferentBitSize.showmodal;
            bitoffsetchange:=formdifferentbitsize.delta;
            formdifferentbitsize.free;
          end;

        if cbfasterscan.checked then
          hyperscanview.bitoffsetchange:=bitoffsetchange;

      end;


      vartype.Enabled:=false;

      foundlist.Clear;
      foundcountlabel.caption:='';

      if formsettings.checkThread.checked or cbfasterscan.checked then
      begin
        val('$'+FromAddress.text,FromAdd,error);
        val('$'+ToAddress.text,ToAdd,error);

        formscanning:=TFormscanning.create(self);
        formscanning.button:=1;
        formscanning.scan:=2;
        formscanning.stype:=stype;
        formscanning.vtype:=vtype;
        formscanning.fromadd:=fromadd;
        formscanning.toadd:=toadd;
        formscanning.fastscan:=fastscan;
        formscanning.scanvalue:=scanvalue.Text;
        formscanning.scanvalue2:=value2;
        formscanning.hexadecimal:=hexadecimalcheckbox.checked;
        formscanning.unicode:=cbunicode.Checked;
        formscanning.percentage:=percentage;

        formscanning.LowMemoryUsage:=formsettings.cbLowMemoryUsage.checked;
        formscanning.Skip_PAGE_NOCACHE:=Skip_PAGE_NOCACHE;
        formscanning.ExtremeScan:=cbFasterscan.Checked;
        formscanning.roundingtype:=roundingtype;
        res:=formscanning.showmodal;
      end else
      foundcount:=nextscan2(scanvalue.text,value2,stype,vtype,roundingtype,hexadecimalcheckbox.Checked,progressbar1,fastscan,cbunicode.checked,percentage);

      foundlist.Clear;


      setfoundlisthorizontal;

      Beep;



    end else showmessage(strCantDoNextScan);


    finally
      UpdateFoundlisttimer.Enabled:=true;
      mainform.caption:=CENorm;
      progressbar1.max:=10;
      progressbar1.Position:=0;
      screen.Cursor:=crdefault;

      if scanvalue.Enabled then scanvalue.SelectAll;

    end;

  finally
    scanepilogue(res=mrcancel);

    if (not cbfasterscan.Checked) and (cbPauseWhileScanning.checked) then
    begin
      advancedoptions.Pausebutton.down:=false; //resume
      advancedoptions.Pausebutton.Click;
    end;

    UpdateScanType;
  end;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  memorybrowser.show;
end;

procedure TMainForm.FormCreate(Sender: TObject);
resourcestring strClickToGoHome='Click here to go to the Cheat Engine homepage';
var pid: dword;
    tokenhandle: thandle;
    tp:TTokenPrivileges;
    prev: TTokenPrivileges;

    ReturnLength: Dword;

    reg: tregistry;
    differentWidth: integer;
begin
  foundlist:=tfoundlist.create(foundlist3,foundcountlabel);

  memscan:=tmemscan.create(progressbar1,mainform.Handle, wm_scandone);
  

  hotkeypressed:=-1;

  ceclipboardformat:=RegisterClipboardFormat('CE_ENTRY');

  HyperscanFileMapping:=CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,sizeof(tscansettings),'CEHYPERSCANSETTINGS');
  if HyperscanFileMapping<>0 then
  begin
    HyperscanView:=MapViewOfFile(HyperscanFileMapping,FILE_MAP_ALL_ACCESS,0,0,0);
    if hyperscanview=nil then
      closehandle(HyperscanFileMapping);

    zeromemory(hyperscanview,sizeof(tscansettings));
  end;


  if hyperscanview<>nil then
  begin
    hyperscanview.mainformHandle:=handle;
    hyperscanview.applicantionhandle:=application.Handle;
    hyperscanview.CheatEngineDir:=CheatEngineDir;
    hyperscanview.CEProcessID:=GetCurrentProcessId;
    hyperscanview.CEMainThreadID:=getcurrentthreadid;
  end;



  pid:=GetCurrentProcessID;

  ownprocesshandle:=OpenProcess(PROCESS_ALL_ACCESS,true,pid);
  tokenhandle:=0;

  if ownprocesshandle<>0 then
  begin
    if OpenProcessToken(ownprocesshandle,TOKEN_QUERY or TOKEN_ADJUST_PRIVILEGES	,tokenhandle) then
    begin
      if lookupPrivilegeValue(nil, 'SeDebugPrivilege' ,tp.Privileges[0].Luid) then
      begin
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        tp.PrivilegeCount := 1; // One privilege to set
        AdjustTokenPrivileges(tokenhandle,false,tp,sizeof(tp),@prev,returnlength);
      end;
    end;
  end;


  fcontrol:=tfcontrol.Create(self);
  with fcontrol do
  begin
    parent:=panel5;
    OnEnter:=FControlEnter;
    OnExit:=FcontrolExit;
    OnKeyDown:=FControlKeyDown;
    OnKeyPress:=FControlKeyPress;
    helpcontext:=10;
    left:=0;
    top:=57;
    height:=21;
    width:=81;
    taborder:=12;
    sendtoback;

  end;

  tempbitmap:=TBitmap.create;

  scanvalue.text:='';
  fromaddress.Text:='00400000';
  toaddress.Text:='7FFFFFFF';
  isbit:=false;


  debugger.Semaphore:=createsemaphore(nil,1,1,nil);

  panel3.DoubleBuffered:=true;

  old8087CW:=Get8087CW;
  Set8087CW($133f);


  application.OnException:=exceptionhandler;
  debugproc:=false;

  //get current screen resolution (when switching back for debug)
  originalwidth:=screen.Width;
  originalheight:=screen.Height;

  Memimage:=TMemorystream.Create;
  FirstShiftSelected:=-1;

  oldwidth:=screen.width;
  oldheight:=screen.height;

  oldnumberofrecords:=0;
  processhandle:=0;

  logo.Hint:=strClickToGoHome;

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

  setlength(freezedirection,7);
  freezedirection[0]:=label44;
  freezedirection[1]:=label45;
  freezedirection[2]:=label46;
  freezedirection[3]:=label47;
  freezedirection[4]:=label48;
  freezedirection[5]:=label49;
  freezedirection[6]:=label50;

  updatescreen;

  newaddress:=0;

  VarType.ItemIndex:=3;
  oldvartype:=3;

  UpdateScantype;
  ScanType.ItemIndex:=0;

  newscan.caption:=strFirstScan;
  Updatelist;

  scrollbox1.DoubleBuffered:=true; //remove flickering
  headercontrol1.DoubleBuffered:=true;


  hookedin:=false;

  //allignment fixes for some window style's that mess up with thick borders (like vista)
  differentWidth:=logopanel.left-(clientwidth-logopanel.width);
  button1.Left:=clientwidth-button1.width;
  commentbutton.left:=clientwidth-commentbutton.width;
  logopanel.left:=clientwidth-logopanel.width;
  speedbutton4.Left:=clientwidth-speedbutton4.width;
  progressbar1.Width:=progressbar1.width-differentwidth;
  undoscan.left:=undoscan.left-differentwidth;

  //create object for the auto attach list
  autoattachlist:=tstringlist.create;
  autoattachlist.CaseSensitive:=false; //set it up as not case sensitive

  randomize;

  pluginhandler:=TPluginhandler.create;


{$ifdef ceasinjectabledll}
  //panel7.Visible:=false;
  speedbutton1.enabled:=false;
  processid:=getcurrentprocessid;
  processhandle:=getcurrentprocess;
  enableGui;
  processlabel.Caption:=Inttohex(processid,8)+' : '+'Current process';
{$endif}

end;

procedure TMainForm.AddressKeyPress(Sender: TObject; var Key: Char);
begin
  hexadecimal(key);

  windows.checked:=false;
  dos.checked:=false;
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

procedure TMainForm.FoundListDblClick(Sender: TObject);
begin
  if foundList3.SelCount>0 then
    AddToRecord(FoundList3.ItemIndex);

  FControl.SetFocus;
end;

procedure TMainForm.Browsethismemoryarrea1Click(Sender: TObject);
var a,b: dword;
    s: string;
begin
  if (foundlist3.ItemIndex<>-1) then
  begin
    MemoryBrowser.memoryaddress:=foundlist.GetAddress(foundlist3.itemindex,b,s);
    memorybrowser.show;
  end;
end;

procedure TMainForm.UpdateTimerTimer(Sender: TObject);
begin
  updatetimer.Enabled:=false;

  inc(reinterpretcheck);
  if reinterpretcheck mod 15=0 then reinterpretaddresses;

  UpdateList;
  updatetimer.Enabled:=true;
end;

procedure TMainForm.FreezeTimerTimer(Sender: TObject);
begin

  freezetimer.enabled:=false;
  FreezeThem;
  freezetimer.enabled:=true;
end;

procedure TMainForm.ScrollBar1Change(Sender: TObject);
begin
  updatescreen;
  updatelist;
end;

procedure TMainForm.Label28Click(Sender: TObject);
var newdesc: string;
resourcestring
  strChangeDescription1='Description';
  strChangeDescription2='Change the description to:';
begin
  if lastselected<>scrollbar1.position+(sender as TLabel).tag then exit;

  newdesc:=InputBox(strChangeDescription1,strChangeDescription2,memrec[scrollbar1.position+(sender as TLabel).tag].Description);
  if newdesc='' then newdesc:=strNoDescription;
  memrec[scrollbar1.position+(sender as TLabel).tag].Description:=newdesc;
  updatescreen;
end;

procedure TMainForm.AddressClick(Sender: TObject);
var adr: String;
    newadr: dword;
    error: Integer;
    bit: Byte;
begin

//  if memrec[scrollbar1.position+(sender as TLabel).tag].Frozen then
//    raise Exception.Create('You first have to unfreeze this address before you can change it');

  formaddresschange:=TFormaddresschange.Create(self);
  formaddresschange.index:=scrollbar1.position+(sender as TLabel).tag;
  formaddresschange.index2:=(sender as TLabel).tag;
  formaddresschange.showmodal;
//address popup


  updatescreen;
  updatelist;
end;

procedure TMainForm.TypeClick(Sender: TObject);
var i,j: Integer;
    oldType: Integer;

begin
  OldType:=memrec[scrollbar1.position+(sender as TLabel).tag].VarType;

//  if memrec[scrollbar1.position+(sender as TLabel).tag].Frozen then
//    Raise Exception.Create('You can''t change the type of an frozen value. First unfreeze it!');
  TypeForm.NrOfRecord:=scrollbar1.position+(sender as TLabel).tag;


  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=0 then TypeForm.VarType.itemindex:=1 else  //byte
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=1 then TypeForm.VarType.itemindex:=2 else  //word
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=2 then TypeForm.VarType.itemindex:=3 else  //dword
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=3 then TypeForm.VarType.itemindex:=5 else  //float
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=4 then TypeForm.VarType.itemindex:=6 else  //double
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=5 then
  begin
    TypeForm.VarType.itemindex:=0;  //bit
    TypeForm.Edit2.text:=IntToStr(memrec[scrollbar1.position+(sender as TLabel).tag].bitlength);
  end else
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=6 then TypeForm.VarType.itemindex:=4 else //int64
  If memrec[scrollbar1.position+(sender as TLabel).tag].VarType=7 then
  begin
    TypeForm.VarType.itemindex:=7;
    typeform.cbunicode.checked:=memrec[scrollbar1.position+(sender as TLabel).tag].unicode;
    TypeForm.Edit1.text:=IntToStr(memrec[scrollbar1.position+(sender as TLabel).tag].bit);
  end else

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

  if TypeForm.Showmodal=mrNo then exit;

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
      if (selected[i]) then
      begin
        memrec[i].VarType:=j;
        memrec[i].Frozen:=false;
      end;
    end;
  end;

  updatescreen;
  updatelist;
end;

//vars for changevalue
var
  differentsizesanswer:boolean;
  terminatewith0answer:boolean;
  askfordifferentsizesonce:boolean;
  asked:boolean;

procedure TMainform.ChangeValue(Itemnr: integer; NewValue:String);
var newvalue1: Byte;
    oldvalue1: byte;
    newvalue2: word;
    newvalue3: dword;
    newvalue4: Single;
    newvalue5: Double;
    newvalue6: Int64;
    newvalueSt: String;
    newvaluest2: widestring;
    newvalue8: TBytes;

    newbytes: array of byte;

    text: pchar;
    addzero: boolean;
    write: dword;
    error,i,j,k,l: Integer;
    bl:integer;

    original: dword;
    thistype: integer;

    nrselected: integer;
    realaddress,realaddress2,count:dword;
    check: boolean;


resourcestring
  strNotTheSameSize1='The text you entered isn''t the same size as the original. Continue?';
  strNotTheSameSize2='Not the same size!';
  strAdd0='Do you want to add a ''0''-terminator at the end?';
  strNotAValidNotation='This is not a valid notation';
  strNotSameAmmountofBytes='The number of bytes you typed is not the same as the previous ammount. Continue?';
  strNotAValidBinaryNotation=' is not a valid binary notation!';
begin
  thistype:=memrec[itemnr].VarType;
  newvaluest:=newvalue;

  error:=0;
  if memrec[itemnr].IsPointer then
  begin
    //find the real address
    realaddress2:=memrec[itemnr].pointers[length(memrec[itemnr].pointers)-1].Address;
    for j:=length(memrec[itemnr].pointers)-1 downto 0 do
    begin
      check:=(realaddress2<($7fffffff-4)) and readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
      if check and (count=4) then
        realaddress2:=realaddress+memrec[itemnr].pointers[j].offset
      else
      begin
        beep;
        exit;
      end;


    end;
    realaddress:=realaddress2;
  end else realaddress:=memrec[itemnr].Address;



  case thistype of
    0,1,2,6: begin
                  val(newvaluest,newvalue6,error);
                  if error=0 then
                  begin
                    newvalue1:=byte(newvalue6);
                    newvalue2:=word(newvalue6);
                    newvalue3:=dword(newvalue6);
                  end;
                end;

    3,4:        begin
                  val(newvaluest,newvalue5,error);
                  if error<>0 then
                  begin
                    if newvaluest[error]=',' then newvaluest[error]:='.'
                    else
                    if newvaluest[error]='.' then newvaluest[error]:=',';

                    error:=0;
                    val(newvaluest,newvalue5,error);
                  end;

                  newvalue4:=newvalue5;
                end;

    7:          begin
                  addzero:=false;
                  if length(newvaluest)<>memrec[itemnr].Bit then
                  begin
                    if askfordifferentsizesonce and (not asked) then
                    begin
                      asked:=true;
                      differentsizesanswer:=false;
                      if messagebox(mainform.handle,pchar(strNotTheSameSize1),pchar(strNotTheSameSize2),MB_YESNO or MB_ICONWARNING)=IDNO then exit;
                      differentsizesanswer:=true; //continue
                      //no way back
                      memrec[itemnr].OldValue:=getStringFromRecord(itemnr);
                      addzero:=(messagebox(mainform.handle,pchar(strAdd0),pchar(CEnorm),MB_YESNO or MB_ICONQUESTION) = IDYES);
                      terminatewith0answer:=addzero;
                      memrec[itemnr].Bit:=length(newvaluest);
                    end;

                    if differentsizesanswer then
                    begin
                      //continue
                      addzero:=terminatewith0answer;
                      memrec[itemnr].OldValue:=getStringFromRecord(itemnr);
                      memrec[itemnr].Bit:=length(newvaluest);
                    end;
                  end;

                  newvalue1:=0;
                  newvalue2:=0;

                  if memrec[itemnr].unicode then
                  begin
                    newvaluest2:=newvaluest;


                    VirtualProtectEx(processhandle,  pointer(realaddress),1,PAGE_EXECUTE_READWRITE,original);
                    writeprocessmemory(processhandle,pointer(realaddress),@newvaluest2[1],length(newvaluest2)*2,write);
                    if addzero then writeprocessmemory(processhandle,pointer(realaddress+length(newvaluest)*2),addr(newvalue2),2,write);
                    VirtualProtectEx(processhandle,  pointer(realaddress),1,original,write);

                    frozenstrings[itemnr]:=newvaluest;
                    freemem(text);
                  end
                  else
                  begin
                    getmem(text,length(newvaluest));
                    StrCopy(text, PChar(newvaluest));

                    VirtualProtectEx(processhandle,  pointer(realaddress),1,PAGE_EXECUTE_READWRITE,original);
                    writeprocessmemory(processhandle,pointer(realaddress),text,length(newvaluest),write);
                    if addzero then writeprocessmemory(processhandle,pointer(realaddress+length(newvaluest)),addr(newvalue1),1,write);
                    VirtualProtectEx(processhandle,  pointer(realaddress),1,original,write);

                    frozenstrings[itemnr]:=newvaluest;
                    freemem(text);
                  end;
                  exit;
                end;

    8:          begin
                  for i:=1 to length(newvaluest) do
                  case newvaluest[i] of
                    '0'..'9' : ;
                    'a'..'f' : ;
                    'A'..'F' : ;
                    ' ','-' : ;
                    else raise exception.create(strNotAValidNotation);
                  end;

                  ConvertStringToBytes(newvaluest,true,newvalue8);

                  if length(newvalue8)<>memrec[itemnr].Bit then
                    if messagebox(mainform.handle,pchar(strNotSameAmmountOfBytes),pchar(StrNotTheSameSize2),MB_YESNO or MB_ICONWARNING)=IDNO then exit;

                  memrec[itemnr].OldValue:=getStringFromRecord(itemnr);

                  memrec[itemnr].Bit:=length(newvalue8);
                  setlength(frozenbytes[itemnr],length(newvalue8));

                  for i:=0 to length(newvalue8)-1 do
                    frozenbytes[itemnr][i]:=newvalue8[i];


                  RewriteData(processhandle,realaddress,@frozenbytes[itemnr][0],length(newvalue8));
                  setlength(newvalue8,0);
                  exit;
                end;
  end;


  if error>0 then raise Exception.Create(strNotAValidNotation);

  memrec[itemnr].OldValue:=getStringFromRecord(itemnr);

  i:=itemnr;
  VirtualProtectEx(processhandle,  pointer(realaddress),1,PAGE_EXECUTE_READWRITE,original);

  case memrec[i].VarType of
    0: //byte
    begin
      writeprocessmemory(processhandle,pointer(realaddress),addr(newvalue1),1,write);
      if (hotkeys[i]=-1) or (memrec[i].Frozendirection=0) then memrec[i].FrozenValue:=newvalue1;
    end;

    1: //word
    begin
      writeprocessmemory(processhandle,pointer(realaddress),addr(newvalue2),2,write);
      if (hotkeys[i]=-1) or (memrec[i].Frozendirection=0) then memrec[i].FrozenValue:=newvalue2;
    end;

    2: //dword
    begin
      writeprocessmemory(processhandle,pointer(realaddress),addr(newvalue3),4,write);
      if (hotkeys[i]=-1) or (memrec[i].Frozendirection=0) then memrec[i].FrozenValue:=newvalue3;
    end;

    3: //single
    begin
      writeprocessmemory(processhandle,pointer(realaddress),addr(newvalue4),4,write);
      if (hotkeys[i]=-1) or (memrec[i].Frozendirection=0) then FrozenFValue[i]:=newvalue4
    end;

    4: //double
    begin
      writeprocessmemory(processhandle,pointer(realaddress),addr(newvalue5),8,write);
      if (hotkeys[i]=-1) or (memrec[i].Frozendirection=0) then FrozenFValue[i]:=newvalue5
    end;

    5: //binary
    begin
      if formsettings.cbBinariesAsDecimal.checked then
        newvaluest:=inttobin(abs(newvalue6));

      bl:=1+((length(newvaluest)-1) div 8);

      setlength(newbytes,bl);
      ReadProcessMemory(processhandle,pointer(realaddress),@newbytes[0],bl,write);

      if formsettings.cbBinariesAsDecimal.checked then
        newvaluest:=inttobin(abs(newvalue6));


      j:=0;
      k:=memrec[i].bit;
      for l:=length(newvaluest) downto 1 do
      begin
        case newvaluest[l] of
         '0' : setbit(k,newbytes[j],0);
         '1' : setbit(k,newbytes[j],1);
         '*','?': ;
         else raise exception.Create(newvaluest+strNotAValidBinaryNotation);
        end;
        inc(k);
        if k>=8 then
        begin
          inc(j);
          k:=0;
        end;
      end;

      writeprocessmemory(processhandle,pointer(realaddress),@newbytes[0],bl,write);
      frozenstrings[i]:=newvaluest;
      setlength(newbytes,0);
    end;

    6: //Int64
    begin
      writeprocessmemory(processhandle,pointer(realaddress),addr(newvalue6),8,write);
      if (hotkeys[i]=-1) or (memrec[i].Frozendirection=0) then memrec[i].FrozenValue:=newvalue6;
    end;
  end;

  //set old security back
  VirtualProtectEx(processhandle,  pointer(realaddress),1,original,write);
  updatelist;
end;

procedure TMainForm.ValueClick(Sender: TObject);
var newvalue1: Byte;
    oldvalue1: byte;
    newvalue2: word;
    newvalue3: dword;
    newvalue4: Single;
    newvalue5: Double;
    newvalue6: Int64;
    newvalueSt: String;
    newvalue8: TBytes;

    newbytes: array of byte;

    text: pchar;
    addzero: boolean;
    write: dword;
    error,i,j,k,l: Integer;
    bl:integer;

    original: dword;
    thistype: integer;

    nrselected: integer;
    realaddress,realaddress2,count:dword;

    stringsize: integer;


resourcestring strValue='Value';
               strChange1Value='Change this value to:';
               strChangeMoreValues='Change these values to:';

begin
  differentsizesanswer:=false;
  terminatewith0answer:=false;
  askfordifferentsizesonce:=false;
  asked:=false;

  if memrec[scrollbar1.position+(sender as TLabel).tag].VarType=255 then
  begin
    //auto assembler script
    Changescript1.Click;
    exit;
  end;


  if (value[(sender as TLabel).tag].Caption='??') or (value[(sender as TLabel).tag].Caption='NAN') or (value[(sender as TLabel).tag].Caption='INF') then
  begin
    beep;
    exit;
  end;

  nrselected:=0;
  thistype:=memrec[scrollbar1.position+(sender as TLabel).tag].VarType;
  stringsize:=memrec[scrollbar1.position+(sender as TLabel).tag].Bit;
  askfordifferentsizesonce:=true;

  for i:=0 to numberofrecords-1 do
  begin
    if selected[i] then
    begin
      if stringsize<>memrec[i].Bit then askfordifferentsizesonce:=false;


      inc(nrselected);
      if memrec[i].VarType<>thistype then
      begin
        //check for compatibility
        if ((thistype in [0,1,2,6]) and not (memrec[i].VarType in [0,1,2,6])) or
           ((thistype in [3,4]) and not (memrec[i].VarType in [3,4])) or
           (thistype=5) or
           (thistype=7) or
           (thistype=8) then
        begin
          beep; //stupid lame beepsound
          exit;
        end;
      end;
    end;
  end;



  newvaluest:=value[(sender as TLabel).tag].Caption;

  if thistype in [7,8] then
  begin
    if not InputQuery(strValue,strChange1Value,newvaluest) then exit;
  end
  else
  begin
    if nrselected>1 then
    begin
      if not InputQuery(strValue,strchangeMoreValues,newvaluest) then exit;
    end
    else
    begin
      if not InputQuery(strValue,strChange1Value,newvaluest) then exit;
    end;
  end;


  for i:=0 to numberofrecords-1 do
  begin
    if selected[i] then
      ChangeValue(i,newvaluest);
  end;

  updatelist;
end;

procedure TMainForm.Browsethismemoryregion1Click(Sender: TObject);
var address: dword;
    realaddress,realaddress2: dword;
    j: integer;
    check: boolean;

    count: dword;
begin
  try
    if memrec[lastselected].IsPointer then
    begin
      realaddress2:=memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Address;
      for j:=length(memrec[lastselected].pointers)-1 downto 0 do
      begin
        check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
        if check and (count=4) then
          realaddress2:=realaddress+memrec[lastselected].pointers[j].offset
        else
          raise exception.Create('The pointer can''t be completly read');
      end;
      address:=realaddress2;

    end
    else
      address:=memrec[lastselected].address;
  except
    address:=memrec[lastselected].address;
  end;
  MemoryBrowser.memoryaddress:=address;

  memorybrowser.show;
end;

procedure TMainForm.Label24ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
beep;
end;

procedure TMainForm.Deletethisrecord1Click(Sender: TObject);
var i,j: Integer;
    comfirm: Word;
    deletegroup: grouptype;
    IsGroup: boolean;
    multiplegroups: boolean;

resourcestring
  strDelete1Address='Delete this address?';
  strDeleteMultipleAddresses='Delete these addresses?';
  strDelete1Group='Also delete the group this address is a member of?';
  strDeleteMoreGroups='Also delete the groups of the addresses you selected?';
begin
  //delete selectedrecord
  isgroup:=false;
  multiplegroups:=false;
  for i:=1 to 6 do deletegroup[i]:=false;
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
  if j>1 then comfirm:=Messagedlg(strDeleteMultipleAddresses, mtWarning, [mbYes, mbNo], 0);
  if j=1 then comfirm:=Messagedlg(strDelete1Address, mtWarning, [mbYes, mbNo], 0);
  if j=0 then exit;

  if comfirm=mrNo then exit;

  if isgroup and not multiplegroups then comfirm:=Messagedlg(strDelete1group, mtConfirmation, [mbYes, mbNo], 0);
  if isgroup and multiplegroups then comfirm:=Messagedlg(strDeleteMoreGroups, mtConfirmation, [mbYes, mbNo], 0);

  deleterecords;

  if comfirm=mrYes then deletegroups(deletegroup);

end;

procedure TMainForm.SortByFrozenButtonClick(Sender: TObject);
var temprec: memoryrecord;
    tempFvalue: double;
    tempstring: string;
    i,j:       Integer;
    lastfrozen: Integer;
    TempSelected: boolean;
    temphotkey: integer;
    tempbytes: array of byte;
begin
//sort memrec by frozen


  i:=0;
  lastfrozen:=-1;

  while (i<numberofrecords) do
  begin
    if memrec[i].frozen then
    begin
      inc(lastfrozen);
      temprec:=memrec[i];
      memrec[i]:=memrec[lastfrozen];
      memrec[lastfrozen]:=temprec;

      tempfvalue:=FrozenFvalue[i];
      FrozenFvalue[i]:=FrozenFvalue[lastfrozen];
      FrozenFvalue[lastfrozen]:=tempFvalue;

      tempstring:=FrozenStrings[i];
      Frozenstrings[i]:=frozenstrings[lastfrozen];
      frozenstrings[lastfrozen]:=tempstring;

      temphotkey:=hotkeys[i];
      hotkeys[i]:=hotkeys[lastfrozen];
      hotkeys[lastfrozen]:=temphotkey;

      setlength(tempbytes,length(frozenbytes[i]));
      for j:=0 to length(tempbytes)-1 do
        tempbytes[j]:=frozenbytes[i][j];

      setlength(Frozenbytes[i],length(frozenbytes[lastfrozen]));
      for j:=0 to length(Frozenbytes[i])-1 do
        Frozenbytes[i][j]:=FrozenBytes[lastfrozen][j];

      setlength(frozenbytes[lastfrozen],length(tempbytes));
      for j:=0 to length(Tempbytes)-1 do
        Frozenbytes[lastfrozen][j]:=Tempbytes[j];


      if (selected[i]) or (selected[lastfrozen]) then
      begin
        tempselected:=selected[i];
        selected[i]:=selected[lastfrozen];
        selected[lastfrozen]:=tempselected;
      end;

      if (i=lastselected) then LastSelected:=lastfrozen;
      If (lastfrozen=lastselected) then lastselected:=i;
    end;

    inc(i);
  end;

  updatescreen;
  setlength(tempbytes,0);
  FControl.SetFocus;
end;

procedure TMainForm.SortByDescriptionButtonClick(Sender: TObject);
var temprec: memoryrecord;
    i,j:       Integer;
    smallest: Integer;
    tempFvalue: double;
    TempSelected: Boolean;
    tempstring: string;
    temphotkey: integer;
    tempbytes: array of byte;
begin
//sort memrec by frozen

  for i:=0 to numberofrecords-1 do
    begin
      smallest:=i;
      for j:=i to numberofrecords-1 do  //find here the smallest
        if memrec[j].description<memrec[smallest].Description then smallest:=j;
      temprec:=memrec[i];
      memrec[i]:=memrec[smallest];
      memrec[smallest]:=temprec;

      tempfvalue:=FrozenFvalue[i];
      FrozenFvalue[i]:=FrozenFvalue[smallest];
      FrozenFvalue[smallest]:=tempFvalue;

      tempstring:=FrozenStrings[i];
      Frozenstrings[i]:=frozenstrings[smallest];
      frozenstrings[smallest]:=tempstring;

      temphotkey:=hotkeys[i];
      hotkeys[i]:=hotkeys[smallest];
      hotkeys[smallest]:=temphotkey;

      setlength(tempbytes,length(frozenbytes[i]));
      for j:=0 to length(tempbytes)-1 do
        tempbytes[j]:=frozenbytes[i][j];

      setlength(Frozenbytes[i],length(frozenbytes[smallest]));
      for j:=0 to length(Frozenbytes[i])-1 do
        Frozenbytes[i][j]:=FrozenBytes[smallest][j];

      setlength(frozenbytes[smallest],length(tempbytes));
      for j:=0 to length(Tempbytes)-1 do
        Frozenbytes[smallest][j]:=Tempbytes[j];



      if (selected[i]) or (selected[smallest]) then
      begin
        tempselected:=selected[i];
        selected[i]:=selected[smallest];
        selected[smallest]:=tempselected;
      end;

      if (i=lastselected) then LastSelected:=smallest;
      If (smallest=lastselected) then lastselected:=i;

    end;

  updatescreen;
  FControl.SetFocus;
end;

procedure TMainForm.SortByAddressButtonClick(Sender: TObject);
var temprec: memoryrecord;
    i,j:       Integer;
    smallest: Integer;
    TempSelected: boolean;
    TempFvalue: double;
    tempstring: string;
    temphotkey: integer;
    tempbytes: array of byte;
begin
//sort memrec by frozen

  for i:=0 to numberofrecords-1 do
    begin
      smallest:=i;
      for j:=i to numberofrecords-1 do  //find here the smallest
        if memrec[j].address<memrec[smallest].address then smallest:=j;
      temprec:=memrec[i];
      memrec[i]:=memrec[smallest];
      memrec[smallest]:=temprec;

      tempfvalue:=FrozenFvalue[i];
      FrozenFvalue[i]:=FrozenFvalue[smallest];
      FrozenFvalue[smallest]:=tempFvalue;

      tempstring:=FrozenStrings[i];
      Frozenstrings[i]:=frozenstrings[smallest];
      frozenstrings[smallest]:=tempstring;

      temphotkey:=hotkeys[i];
      hotkeys[i]:=hotkeys[smallest];
      hotkeys[smallest]:=temphotkey;

      setlength(tempbytes,length(frozenbytes[i]));
      for j:=0 to length(tempbytes)-1 do
        tempbytes[j]:=frozenbytes[i][j];

      setlength(Frozenbytes[i],length(frozenbytes[smallest]));
      for j:=0 to length(Frozenbytes[i])-1 do
        Frozenbytes[i][j]:=FrozenBytes[smallest][j];

      setlength(frozenbytes[smallest],length(tempbytes));
      for j:=0 to length(Tempbytes)-1 do
        Frozenbytes[smallest][j]:=Tempbytes[j];


      if (selected[i]) or (selected[smallest]) then
      begin
        tempselected:=selected[i];
        selected[i]:=selected[smallest];
        selected[smallest]:=tempselected;
      end;

      if (i=lastselected) then LastSelected:=smallest;
      If (smallest=lastselected) then lastselected:=i;

    end;

  updatescreen;
  FControl.SetFocus;
end;

procedure TMainForm.SortByTypeButtonClick(Sender: TObject);
var temprec: memoryrecord;
    i,j:       Integer;
    smallest: Integer;
    TempFvalue: Double;
    TempSelected: Boolean;
    TempString: string;
    temphotkey: integer;
    tempbytes: array of byte;
begin
//sort memrec by frozen

  for i:=0 to numberofrecords-1 do
    begin
      smallest:=i;
      for j:=i to numberofrecords-1 do  //find here the smallest
        if memrec[j].vartype<memrec[smallest].vartype then smallest:=j;
      temprec:=memrec[i];
      memrec[i]:=memrec[smallest];
      memrec[smallest]:=temprec;

      tempfvalue:=FrozenFvalue[i];
      FrozenFvalue[i]:=FrozenFvalue[smallest];
      FrozenFvalue[smallest]:=tempFvalue;

      tempstring:=FrozenStrings[i];
      Frozenstrings[i]:=frozenstrings[smallest];
      frozenstrings[smallest]:=tempstring;

      temphotkey:=hotkeys[i];
      hotkeys[i]:=hotkeys[smallest];
      hotkeys[smallest]:=temphotkey;

      setlength(tempbytes,length(frozenbytes[i]));
      for j:=0 to length(tempbytes)-1 do
        tempbytes[j]:=frozenbytes[i][j];

      setlength(Frozenbytes[i],length(frozenbytes[smallest]));
      for j:=0 to length(Frozenbytes[i])-1 do
        Frozenbytes[i][j]:=FrozenBytes[smallest][j];

      setlength(frozenbytes[smallest],length(tempbytes));
      for j:=0 to length(Tempbytes)-1 do
        Frozenbytes[smallest][j]:=Tempbytes[j];


      if (selected[i]) or (selected[smallest]) then
      begin
        tempselected:=selected[i];
        selected[i]:=selected[smallest];
        selected[smallest]:=tempselected;
      end;

      if (i=lastselected) then LastSelected:=smallest;
      If (smallest=lastselected) then lastselected:=i;

    end;

  updatescreen;
  FControl.SetFocus;
end;

procedure TMainForm.SortByValueButtonClick(Sender: TObject);
{var temprec: memoryrecord;
    tempammount: int64;
    i,j:       Integer;
    smallest: Integer;
    ammount: array of int64;
    tempammount1: byte;
    tempammount2: word;
    tempammount3: dword;
    tempammount4: int64;
    count: dword;
    TempFvalue: Double;
    TempSelected: Boolean;
    TempString: String;
    TempHotkey: integer;
    TempBytes: array of byte;}
begin
{//sort memrec by frozen
  setlength(ammount,numberofrecords);

  for i:=0 to numberofrecords-1 do
  begin
    if memrec[i].VarType=0 then //byte
    begin
      readprocessmemory(processhandle,pointer(memrec[i].address),addr(tempammount1),1,count);
      if count<1 then ammount[i]:=-1 else ammount[i]:=tempammount1;
    end;

    if memrec[i].VarType=1 then //word
    begin
      readprocessmemory(processhandle,pointer(memrec[i].address),addr(tempammount2),2,count);
      if count<2 then ammount[i]:=-1 else ammount[i]:=tempammount2;
    end;

    if memrec[i].VarType=2 then //dword
    begin
      readprocessmemory(processhandle,pointer(memrec[i].address),addr(tempammount3),4,count);
      if count<4 then ammount[i]:=-1 else ammount[i]:=tempammount3;
    end;

    if (memrec[i].VarType=3) or (memrec[i].vartype=4) then
      ammount[i]:=-1;


    if memrec[i].VarType=6 then //int64
    begin
      readprocessmemory(processhandle,pointer(memrec[i].address),addr(tempammount4),8,count);
      if count<8 then ammount[i]:=-1 else ammount[i]:=tempammount4;
    end;

    if memrec[i].vartype=5 then
    begin
      readprocessmemory(processhandle,pointer(memrec[i].address),addr(tempammount1),1,count);
      if count<1 then ammount[i]:=dword(-1) else
      begin
        case memrec[i].Bit of
          0       :       if (tempammount1 and 1)=0 then ammount[i]:=0 else ammount[i]:=1;
          1       :       if (tempammount1 and 2)=0 then ammount[i]:=0 else ammount[i]:=1;
          2       :       if (tempammount1 and 4)=0 then ammount[i]:=0 else ammount[i]:=1;
          3       :       if (tempammount1 and 8)=0 then ammount[i]:=0 else ammount[i]:=1;
          4       :       if (tempammount1 and 16)=0 then ammount[i]:=0 else ammount[i]:=1;
          5       :       if (tempammount1 and 32)=0 then ammount[i]:=0 else ammount[i]:=1;
          6       :       if (tempammount1 and 64)=0 then ammount[i]:=0 else ammount[i]:=1;
          7       :       if (tempammount1 and 128)=0 then ammount[i]:=0 else ammount[i]:=1;
        end;
      end;
    end;



  end;

  for i:=0 to numberofrecords-1 do
    begin
      smallest:=i;
      for j:=i to numberofrecords-1 do  //find here the smallest
        if ammount[j]<ammount[smallest] then smallest:=j;
      temprec:=memrec[i];
      tempammount:=ammount[i];
      memrec[i]:=memrec[smallest];
      memrec[smallest]:=temprec;
      ammount[smallest]:=tempammount;

      tempfvalue:=FrozenFvalue[i];
      FrozenFvalue[i]:=FrozenFvalue[smallest];
      FrozenFvalue[smallest]:=tempFvalue;

      tempstring:=FrozenStrings[i];
      Frozenstrings[i]:=frozenstrings[smallest];
      frozenstrings[smallest]:=tempstring;

      temphotkey:=hotkeys[i];
      hotkeys[i]:=hotkeys[smallest];
      hotkeys[smallest]:=temphotkey;

      setlength(tempbytes,length(frozenbytes[i]));
      for j:=0 to length(tempbytes)-1 do
        tempbytes[j]:=frozenbytes[i][j];

      setlength(Frozenbytes[i],length(frozenbytes[smallest]));
      for j:=0 to length(Frozenbytes[i])-1 do
        Frozenbytes[i][j]:=FrozenBytes[smallest][j];

      setlength(frozenbytes[smallest],length(tempbytes));
      for j:=0 to length(Tempbytes)-1 do
        Frozenbytes[smallest][j]:=Tempbytes[j];


      if (selected[i]) or (selected[smallest]) then
      begin
        tempselected:=selected[i];
        selected[i]:=selected[smallest];
        selected[smallest]:=tempselected;
      end;

      if (i=lastselected) then LastSelected:=smallest;
      If (smallest=lastselected) then lastselected:=i;

    end;

  updatescreen;
  FControl.SetFocus;}
end;

procedure TMainForm.ScanvalueoldKeyPress(Sender: TObject; var Key: Char);
var correct: boolean;
    becomes: string;
    i,j: integer;
begin
  checkpaste;
  correct:=false;

  if key=chr(13) then
  begin
    if nextscanbutton.Enabled then nextscanbutton.Click else newscan.Click;

    key:=#0;
    exit;
  end;

{  if key=chr(8) then correct:=true else
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
  if not correct then key:=chr(0);   }
end;

procedure TMainForm.Calculatenewvaluepart21Click(Sender: TObject);
var newaddress:dword;
    calculate: Integer;
    i,j,err: Integer;
    selectedi: Integer;

    firstispointer,dontdopointers: boolean;
    re: string;
    ok: boolean;

    res: integer;
resourcestring
  strSelectedAddressIsAPointer='The selected address is a pointer. Are you sure? (the base pointer will get the address)';
  strMorePointers='There are more pointers selected. Do you want to change them as well?';
  strMorePointers2='You have selected one or more pointers. Do you want to change them as well?';
  strNotAValidValue='This is not an valid value';
begin
  res:=-1;

  //first find out how many where selected.
  i:=0;
  selectedi:=0;
  while (i<numberofrecords) and (selectedi<2) do
  begin
    if selected[i] then inc(selectedi);
    inc(i);
  end;


  dontdopointers:=false;
  firstispointer:=false;
  if memrec[lastselected].IsPointer then
    if messagedlg(strSelectedAddressIsAPointer,mtConfirmation,[mbyes,mbno],0)<>mryes then exit else firstispointer:=true;

  if selectedi>1 then
  begin
    //see if there are (other) pointers selected
    for i:=0 to numberofrecords-1 do
      if (i<>lastselected) and (selected[i]) and (memrec[i].IsPointer) then
      begin
        if firstispointer then
          re:=strMorePointers
        else
          re:=strMorePointers2;

        if messagedlg(re,mtConfirmation,[mbyes,mbno],0)=mrno then dontdopointers:=true;
        break;
      end;
  end else
  begin
    //all addresses
    for i:=0 to numberofrecords-1 do
      if (i<>lastselected) and (memrec[i].IsPointer) then
      begin
        if firstispointer then
          re:=strMorePointers
        else
          re:=strMorePointers2;

        if messagedlg(re,mtConfirmation,[mbyes,mbno],0)=mrno then dontdopointers:=true;
        break;
      end;
  end;



  newaddress:=memrec[lastselected].address;
  if (foundlist3.SelCount>0) then
    newaddress:=foundlist.GetAddress(foundlist3.ItemIndex);
    

  changeoffset:=TChangeOffset.create(self);

  if memrec[lastselected].IsPointer then
    changeoffset.FromAddress:=memrec[i].pointers[length(memrec[i].pointers)-1].Address
  else
    changeoffset.FromAddress:=memrec[lastselected].address;

  changeoffset.toAddress:=NewAddress;
  if changeoffset.showmodal=mrCancel then exit;

  if changeoffset.error>0 then raise exception.Create(strNotAValidValue);
  calculate:=changeoffset.offset;

  for i:=0 to Numberofrecords-1 do
    if ((selectedi>1) and selected[i]) or (selectedi=1) then
    begin
      if (memrec[i].interpretableaddress<>'') and (not memrec[i].IsPointer) then
      begin
        //check if it's a normal address or a interpretable address
        val('$'+memrec[i].interpretableaddress,j,err);

        if err>0 then
        begin
          if res=-1 then
          begin
            res:=messagedlg('The record with description '''+memrec[i].Description+''' has as interpretable address '''+memrec[i].interpretableaddress+'''. The recalculation will change it to '+symhandler.getNameFromAddress(memrec[i].Address+calculate,true,true)+'. Do you want to edit it to the new address?',mtconfirmation,[mbyes,mbno,mbNoToAll,mbYesToAll,mbCancel],0);
            if res=mrcancel then exit;
          end;

          ok:=res=mryes;

          if (res=mryes) or (res=mrno) then
            res:=-1; //reset
        end
        else
          ok:=true;


      end else ok:=true;

      if ok then
      begin
        inc(memrec[i].Address,calculate);
        if memrec[i].interpretableaddress<>'' then memrec[i].interpretableaddress:=symhandler.getNameFromAddress(memrec[i].Address,true,true);

        if memrec[i].IsPointer then
        begin
          if not dontdopointers then
          begin
            if memrec[i].pointers[length(memrec[i].pointers)-1].Interpretableaddress<>'' then
            begin
              val('$'+memrec[i].pointers[length(memrec[i].pointers)-1].Interpretableaddress,j,err);
              if err>0 then
              begin
                if res=-1 then
                begin
                  res:=messagedlg('The record with description '''+memrec[i].Description+''' has as interpretable base pointer:'''+memrec[i].pointers[length(memrec[i].pointers)-1].Interpretableaddress+'''. The recalculation will make it '+symhandler.getNameFromAddress((memrec[i].pointers[length(memrec[i].pointers)-1].Address+calculate),true,true)+'. Do you want to edit it to the new address?',mtconfirmation,[mbyes,mbno,mbNoToAll,mbYesToAll,mbCancel],0);
                  if res=mrcancel then exit;
                end;

                ok:=res=mryes;

                if (res=mryes) or (res=mrno) then
                  res:=-1; //reset
              end
              else
                ok:=true;
            end;

            if ok then
            begin
              inc(memrec[i].pointers[length(memrec[i].pointers)-1].Address,calculate);
              if memrec[i].pointers[length(memrec[i].pointers)-1].Interpretableaddress<>'' then
                memrec[i].pointers[length(memrec[i].pointers)-1].Interpretableaddress:=symhandler.getNameFromAddress(memrec[i].pointers[length(memrec[i].pointers)-1].Address,true,true);
            end;
          end;
        end;
      end;
    end;

  updatescreen;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if addform=nil then addform:=Taddform.create(self);
  
  addform.showmodal
end;

procedure TMainForm.ScanTypeChange(Sender: TObject);
begin
  updatescantype;

end;

procedure TMainForm.VarTypeChange(Sender: TObject);
var a: int64;
    pa: ^int64;
    pb: ^dword;
    b: double;
    c: Integer;
    d: single;
    i: integer;
    hexvis: boolean;
    decbitvis: boolean;
    hexleft: integer;
    hextext: string;
    hexwidth: integer;
    casevis: boolean;

    oldscantype: integer;
    error: integer;
    temp:string;

    newvartype: integer;
    unicodevis: boolean;
    tc:tbitmap;
begin
  newvartype:=vartype.ItemIndex;

  dontconvert:=true;

  hexvis:=true;
  unicodevis:=false;
  hexleft:=rbbit.Left;
  hexwidth:=50;

  hextext:='Hex';
  casevis:=false;

  decbitvis:=false;

  //convertroutine:

  case oldvartype of
    0:  //it was a bit
    begin
      case newvartype of
        1,2,3,4,5,6:
        begin
          //convert it to a normal value
          if rbbit.checked then
          begin
            //convert it to a decimal value
            rbdec.Checked:=true;
            rbdec.OnClick(rbdec);
          end;
        end;

        7: ; //just leave it

        8:  //array of byte
        begin
          //first convert it to a array of byte
          if rbbit.checked then
          begin
            //convert it to a decimal value
            rbdec.Checked:=true;
            rbdec.OnClick(rbdec);
          end;

          //now convert it to a array of byte
          try
            a:=strtoint(scanvalue.text);
            scanvalue.text:=vartobytes(@a,8);
          except
            scanvalue.text:='';
          end;

        end;
      end;
    end;

    1,2,3,4,5,6,9:
    begin
      //it was one of the normal values
      case newvartype of
        0: //it gets converted to a binary
        begin
          //set

          if oldvartype in [1,2,3,4] then
          begin
            try
              if hexadecimalcheckbox.Checked then
                a:=strtoint('$'+scanvalue.text)
              else
                a:=strtoint(scanvalue.text);
              scanvalue.Text:=inttostr(a);
            except
              scanvalue.text:='';
            end;
          end
          else
          if oldvartype=5 then
          begin
            try
              d:=strtofloat(scanvalue.Text);
              pb:=@d;
              scanvalue.Text:=inttostr(pb^);
            except
              scanvalue.text:='';
            end;
          end else
          if oldvartype=6 then
          begin
            try
              b:=strtofloat(scanvalue.Text);
              pa:=@b;
              scanvalue.Text:=inttostr(pa^);
            except
              scanvalue.text:='';
            end;
          end;

          isbit:=false;
          rbdec.checked:=true;
          rbdec.OnClick(rbdec);
        end;

        1,2,3,4:
        begin
           //converted to a normal type
          if oldvartype in [5,6] then
          begin
            try
              scanvalue.text:=inttostr(trunc(strtofloat(scanvalue.text)));
            except
              scanvalue.Text:='';
            end;

          end;

        end;

        5,6: ; //converted to a float, leave it the same

        7: ;//converted to a string , leave it

        8: //converted to a array of byte
        begin
          if oldvartype in [1,2,3,4] then
          begin
            //now convert it to a array of byte
            if oldvartype in [1,2,3,4] then
            begin
              try
                a:=strtoint(scanvalue.text);
                scanvalue.text:=vartobytes(@a,8);
              except
                scanvalue.text:='';
              end;
            end
            else
            if oldvartype=5 then
            begin
              try
                d:=strtofloat(scanvalue.Text);
                scanvalue.Text:=vartobytes(@d,4);
              except
                scanvalue.text:='';
              end;
            end else
            if oldvartype=6 then
            begin
              try
                b:=strtofloat(scanvalue.Text);
                scanvalue.Text:=vartobytes(@b,8);
              except
                scanvalue.text:='';
              end;
            end;

          end;
        end;


      end;
    end;


    7: //it was text
    begin
      case newvartype of
        0: //it becomes a binary
        begin
          //convert the text to to a decimal value if possible
          try
            a:=strtoint(scanvalue.text);
            scanvalue.Text:=inttostr(a);
          except
            scanvalue.text:='';
          end;
        end;

        1,2,3,4:
        begin
          //convert it to a decimal value
          //allow all characters from '0' to '9' and break if it doesn't
          val(scanvalue.text,a,i);
          scanvalue.Text:=inttostr(a);
        end;

        5,6:
        begin
          //convert the string to a float
          //same as dec, but allow . and , and E and -
          val(scanvalue.text,b,i);
          scanvalue.Text:=format('%.2f',[b]);
        end;

        8:
        begin
          //convert the string to a array of bytes
          temp:='';
          for i:=1 to length(scanvalue.text) do
            temp:=temp+inttohex(ord(scanvalue.Text[i]),2)+' ';

          scanvalue.text:=copy(temp,1,length(temp)-1);


        end;

      end;
    end;


    8: //it was a array of bytes
    begin
      case newvartype of
        0: //it becomes a binary
        begin
          //convert it to a decimal value
          rbdec.checked:=true;
          isbit:=false;
          scanvalue.text:=IntToStr(ByteStringToInt(scanvalue.Text,HexadecimalCheckbox.checked));
        end;

        1,2,3,4: //it becomes a normal decimal value
        begin
          scanvalue.text:=IntToStr(ByteStringToInt(scanvalue.Text,HexadecimalCheckbox.checked));
        end;

        5: //it becomes a float/single
        begin
          scanvalue.Text:=floattostr(bytestringtosingle(scanvalue.text,hexadecimalcheckbox.checked));
        end;

        6: //it becomes a double
        begin
          scanvalue.Text:=floattostr(bytestringtodouble(scanvalue.text,hexadecimalcheckbox.checked));
        end;

        7: //convert it to a string
        begin
          scanvalue.text:=bytestringtotext(scanvalue.text,hexadecimalcheckbox.checked);
        end;

      end;

    end;
  end;

  oldvartype:=vartype.itemindex;

  if not (oldscantype in [smallerthan,biggerthan,valueBetween,exact_value,Advanced_Scan]) then
    scantype.itemindex:=0;

  case newvartype of
  0: begin //binary
       if not cbNewscanroutine.checked then
         hexadecimalcheckbox.Checked:=true;
       rbdec.checked:=true;
       HexadecimalCheckbox.visible:=false;
       decbitvis:=true;
       Scantype.itemindex:=0;
     end;

   1,2,3,4,9,10:
     begin
       casevis:=false;
       hexvis:=true;
       scanvalue.MaxLength:=0;
       hexadecimalcheckbox.enabled:=newscan.enabled;
       //hexadecimalcheckbox.Checked:=false;
     end;

  5: begin //float;
       casevis:=false;
       hexvis:=false;
       hexadecimalcheckbox.Checked:=false;
       HexadecimalCheckbox.enabled:=false;
       scanvalue.MaxLength:=0;
     end;

  6: begin //double
       hexvis:=false;
       temp:=scanvalue.text;


       hexadecimalcheckbox.Checked:=false;
       HexadecimalCheckbox.enabled:=false;
       scanvalue.MaxLength:=0;
     end;

  7: begin //text
       scantype.itemindex:=0;
       casevis:=true;
       cbCasesensitive.Checked:=true;
       cbCasesensitive.ShowHint:=false;
       unicodevis:=true;


       hexadecimalcheckbox.enabled:=newscan.enabled;
       hexadecimalcheckbox.checkeD:=cbCaseSensitive.checked;
       hexvis:=false;
       hextext:='Unicode';
       hexleft:=170;
       hexwidth:=61;
     end;

  8: begin  //array of byte
       scantype.itemindex:=0;
       scanvalue.MaxLength:=0;
       hexadecimalcheckbox.enableD:=newscan.enabled;
       hexadecimalcheckbox.Checked:=true;
     end;

  end;

  hexadecimalcheckbox.Caption:=hextext;

  tc:=tbitmap.Create;
  tc.canvas.Font:=hexadecimalcheckbox.Font;
  hexwidth:=tc.canvas.TextWidth(hextext)+22;
  tc.free;
  hexadecimalcheckbox.width:=hexwidth;
  hexadecimalcheckbox.left:=scanvalue.Left-hexwidth;
  HexadecimalCheckbox.visible:=hexvis;
  rbdec.visible:=decbitvis;
  rbbit.Visible:=decbitvis;
  cbunicode.Visible:=unicodevis;

  cbCaseSensitive.visible:=casevis;

  cbfastscan.Enabled:=(vartype.ItemIndex in [2..6]) and newscan.enabled;

  UpdateScanType;
  dontconvert:=false;

  //set the old vartype
  oldvartype:=vartype.itemindex;
end;

procedure TMainForm.LogoClick(Sender: TObject);
begin
  if messagedlg('Do you want to go to the Cheat Engine website?',mtconfirmation,[mbyes,mbno],0)=mryes then
  begin
    case random(5) of
      0: ShellExecute(0, pchar('open'),pchar('http://www.syndiv.com/ce/'), pchar(''),pchar(''), SW_MAXIMIZE	);
      1: ShellExecute(0, pchar('open'),pchar('http://wWw.cheatengine.tk/'), pchar(''),pchar(''), SW_MAXIMIZE	);
      2: ShellExecute(0, pchar('open'),pchar('http://www.heijnen1.demon.nl/'), pchar(''),pchar(''), SW_MAXIMIZE	);
      3,4: ShellExecute(0, pchar('open'),pchar('http://www.cheatengine.org/'), pchar(''),pchar(''), SW_MAXIMIZE	);
    end;
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

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=CheckIfSaved;
end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
resourcestring
  strdeleteall='Are you sure you want to delete all addresses?';
var i: integer;
begin
  if editingscript then exit; //don't do it when editing a script

  if numberofrecords>0 then
  begin
    if messagedlg(strdeleteall,mtWarning,[mbYes,mbNo],0)=mrNo then exit;
    numberofRecords:=0;

    for i:=0 to numberofrecords-1 do
    begin
      if hotkeys[i]<>-1 then
        unregisterhotkey(handle,hotkeys[i]);
      hotkeys[i]:=-1;
      hotkeystrings[i]:='';
    end;

    updatelist;
    updatescreen;
  end;

  reservemem;
end;

procedure TMainForm.SpeedButton3Click(Sender: TObject);
resourcestring
  stralreadyin='This address is already in the list';
  stralreadyinlistmultiple='One or more addresses where already in the list';
var i: Integer;
    morein: boolean;
begin
  morein:=false;
  for i:=0 to foundlist3.Items.Count-1 do
    try
      if foundlist3.Items[i].Selected then AddToRecord(i);
    except
      if foundlist3.SelCount=1 then raise exception.Create(stralreadyin) else morein:=true;
    end;

  if morein then showmessage(stralreadyinlistmultiple);
  FControl.SetFocus;
end;

procedure TMainForm.Selectallitems1Click(Sender: TObject);
var i: Integer;
begin
  foundlist3.SelectAll;
end;

procedure TMainForm.Label37Click(Sender: TObject);
begin
beep;
end;

procedure TMainForm.Freezealladdresses2Click(Sender: TObject);
{ scan each item in memrec, if it's not frozen freeze it by getting the current
 value of the address and putting it in the right value type
 Then set frozen to true and update the visible list}
var i,j,k: Integer;
    db: Byte;
    dw: Word;
    dd: Dword;
    a: Single;
    b: double;
    dI64: int64;
    temps: pchar;

    multiple,check: Boolean;
    error: dword;
    controle:string;

    temp: string;
    read8: array of byte;
    read9: pbyte;

    realaddress,realaddress2,count:dword;

begin

  if memrec[lastselected].vartype=255 then
  begin
    enableautoassemblecheat(lastselected);
    exit;
  end;

  j:=0;
  i:=0;
  while (j<2) and (i<numberofrecords) do
  begin
    if selected[i] then inc(j);
    inc(i);
  end;

  multiple:=(j>1);

  for i:=0 to numberofrecords-1 do
  begin

    if not memrec[i].Frozen then
    begin
      if memrec[i].IsPointer then
      begin
        //find the real address
        realaddress2:=memrec[i].pointers[length(memrec[i].pointers)-1].Address;
        for j:=length(memrec[i].pointers)-1 downto 0 do
        begin
          check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
          if check and (count=4) then
            realaddress2:=realaddress+memrec[i].pointers[j].offset;
        end;
        realaddress:=realaddress2;
      end else realaddress:=memrec[i].Address;


      memrec[i].Frozendirection:=0;
      //
      if (not multiple) or (multiple and selected[i]) then
        case memrec[i].VarType of
              0       :       begin  //byte
                                readprocessmemory(processhandle,pointer(realaddress),addr(db),1,error);
                                if error=1 then
                                begin
                                  memrec[i].FrozenValue:=db;
                                  memrec[i].Frozen:=true;
                                end;
                              end;

              1       :      begin  //word
                               readprocessmemory(processhandle,pointer(realaddress),addr(dw),2,error);
                               if error=2 then
                               begin
                                 memrec[i].FrozenValue:=dw;
                                 memrec[i].Frozen:=true;
                               end;
                             end;

              2       :      begin  //dword
                               readprocessmemory(processhandle,pointer(realaddress),addr(dd),4,error);
                               if error=4 then
                               begin
                                 memrec[i].FrozenValue:=dd;
                                 memrec[i].Frozen:=true;
                               end;
                             end;

              3       :      begin
                               readprocessmemory(processhandle,pointer(realaddress),addr(a),4,error);
                               if error=4 then
                               begin
                                 controle:=FloatToStr(a);
                                 if system.pos('NAT',controle)>0 then exit;
                                 if system.pos('INF',controle)>0 then exit;
                                 frozenfvalue[i]:=a;
                                 memrec[i].Frozen:=true;
                               end;
                             end;

              4       :      begin
                               readprocessmemory(processhandle,pointer(realaddress),addr(b),8,error);
                               if error=8 then
                               begin
                                 controle:=FloatToStr(b);
                                 if system.pos('NAT',controle)>0 then exit;
                                 if system.pos('INF',controle)>0 then exit;
                                 frozenfvalue[i]:=b;
                                 memrec[i].Frozen:=true;
                               end;
                             end;

              5       :      begin  //bit
                               k:=1+((memrec[i].Bit+memrec[i].bitlength) div 8);
                               setlength(read8,k);

                               readprocessmemory(processhandle,pointer(realaddress),addr(read8[0]),k,error);
                               if error=k then
                               begin
                                 //find out the current bit combination (use frozenstrings to store the value)

                                 //convert what i need to a string of bits
                                 temp:='';
                                 j:=memrec[i].Bit;
                                 read9:=@read8[0];
                                 for k:=1 to memrec[i].bitlength do
                                 begin
                                   temp:=temp+IntToStr(getbit(j,read9^));
                                   inc(j);
                                   if j>=8 then
                                   begin
                                     j:=0;
                                     inc(read9);
                                   end;
                                 end;

                                 //the tempstring now contaisn the bits (bit0 is first char...)
                                 frozenstrings[i]:=temp;
                                 memrec[i].Frozen:=true;
                               end;
                             end;

              6       :      begin  //int64
                               readprocessmemory(processhandle,pointer(realaddress),addr(dI64),8,error);
                               if error=8 then
                               begin
                                 memrec[i].FrozenValue:=di64;
                                 memrec[i].Frozen:=true;
                               end;
                             end;

              7       :      begin  //text
                               getmem(temps,memrec[i].bit+1);
                               readprocessmemory(processhandle,pointer(realaddress),temps,memrec[i].Bit,error);
                               if error=memrec[i].bit then
                               begin
                                 temps[memrec[i].bit]:=#0;
                                 FrozenStrings[i]:=temps;
                                 memrec[i].Frozen:=true;
                               end;
                               freemem(temps);
                             end;

              8       :      begin  //array of byte
                               setlength(FrozenBytes[i],memrec[i].bit);
                               readprocessmemory(processhandle,pointer(realaddress),FrozenBytes[i],memrec[i].Bit,error);
                               if error=memrec[i].bit then
                                 memrec[i].Frozen:=true;
                              end;

              {255     :      begin
                               enableautoassemblecheat(i);

                             end; }



        end;

    end;
  end;

  updatelist;
  updatescreen;
end;


procedure TMainForm.SlectItem(Sender: TObject; Button: TMouseButton;
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

  updatelist;
  Updatescreen;
end;

procedure TMainForm.PopupMenu2Popup(Sender: TObject);
resourcestring
  strsethotkey='Set a hotkey';
  strshowasdecimal='Show as decimal value';
  strshowashex='Show as hexadecimal value';
  strFreezeAddressInList='Freeze the address in this list';
  strFreezeAllAddresses='Freeze all addresses in this list';
  strUnfreezeAllAddresses='Unfreeze all addresses in this list';
  strUnfreezeAddressInList='Unfreeze the address in this list';
  strDeleteAddress='Delete this address';
  strDeleteTheseAddresses='Delete these addresses';
  strRecalculateAddress='Recalculate address';
  strRecalculateSelectedAddresses='Recalculate selected addresses';
  strRecalculateAllAddresses='Recalculate all addresses';

  strRemoveFromGroup='Remove from group ';

  strChangeScript='Change script';
  strEnableCheat='Enable cheat';
  strDisableCheat='Disable cheat';

  strForceRecheck='Force recheck symbols';

var i: Integer;
    selectedi: Integer;
    number: Integer;
    clip: TClipboard;
    inclipboard: boolean;
    temp: pchar;
begin

  sethotkey1.Caption:=strsethotkey;

  if (lastselected<>-1) and (lastselected<numberofrecords) then
  begin
    if hotkeys[lastselected]<>-1 then sethotkey1.Caption:='Remove the hotkey (currently:'+hotkeystrings[lastselected]+')';

    Trytofindbasepointer1.Visible:=memrec[lastselected].IsPointer;
    if memrec[lastselected].VarType in [0,1,2,6] then
      Showashexadecimal1.Visible:=true;

    if memrec[lastselected].ShowAsHex then
      Showashexadecimal1.Caption:=strshowasdecimal
    else
      Showashexadecimal1.Caption:=strshowashex;
  end else Showashexadecimal1.Visible:=false;

  inclipboard:=false;
  clip:=TClipboard.Create;
  if clip.HasFormat(ceclipboardformat) then
  begin
    inclipboard:=true;
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


  Forcerechecksymbols1.visible:=numberofrecords>0;

  Pointerscanforthisaddress1.Visible:=(numberofrecords>0) and (selectedi>=1);
  change1.Visible:=(numberofrecords>0) and (selectedi>=1);


  if (numberofrecords=0) then //it's a empty list
  begin
    DeleteThisRecord1.visible:=false;
    BrowseThisMemoryRegion1.visible:=false;
    ShowAsHexadecimal1.Visible:=false;
    SetHotkey1.Visible:=false;
    FreezeallAddresses2.visible:=false;
    UnfreezeAllAddresses1.visible:=false;

    n5.visible:=false;
    Setbreakpoint1.visible:=false;
    Findoutwhatreadsfromthisaddress1.visible:=false;
    Findoutwhataccessesthisaddress1.Visible:=false;

    sep1.Visible:=false;
    CalculateNewValuePart21.visible:=false;
    n4.visible:=false;
    cut1.Enabled:=false;
    copy1.Enabled:=false;
    paste1.Enabled:=inclipboard;

    groupoption1.Visible:=false;
  end;

  if (selectedi=0) and (numberofrecords=1) then //nothing selected, but there is 1 address
  begin
    DeleteThisRecord1.Visible:=false;
    Browsethismemoryregion1.visible:=false;
    ShowAsHexadecimal1.Visible:=false;
    SetHotkey1.Visible:=false;
    FreezeAllAddresses2.Caption:=strfreezeaddressinlist;
    UnfreezeAllAddresses1.Caption:=strunfreezeaddressinlist;
    if memrec[0].Frozen then
    begin
      UnfreezeAllAddresses1.visible:=true;
      FreezeAllAddresses2.visible:=false;
    end
    else
    begin
      UnfreezeAllAddresses1.visible:=false;
      FreezeAllAddresses2.visible:=true;
    end;
    n5.visible:=false;
    Setbreakpoint1.visible:=false;
    Findoutwhatreadsfromthisaddress1.visible:=false;
    Findoutwhataccessesthisaddress1.Visible:=false;

    sep1.Visible:=false;
    CalculateNewValuePart21.Visible:=false;
    n4.Visible:=true;

    cut1.Enabled:=false;
    copy1.Enabled:=false;
    paste1.enabled:=inclipboard;

    N1.Visible:=false;
    groupoption1.visible:=false;
  end;

  if (selectedi=1) and (numberofrecords=1) then //1 selected and 1 record
  begin
    DeleteThisRecord1.Visible:=true;
    DeleteThisRecord1.Caption:=strdeleteaddress;
    Browsethismemoryregion1.Visible:=true;
    ShowAsHexadecimal1.Visible:=true;
    SetHotkey1.Visible:=true;
    FreezeAllAddresses2.Caption:=strfreezeaddressinlist;
    UnfreezeAllAddresses1.Caption:=strunfreezeaddressinlist;
    if memrec[0].Frozen then
    begin
      UnfreezeAllAddresses1.visible:=true;
      FreezeAllAddresses2.visible:=false;
    end
    else
    begin
      UnfreezeAllAddresses1.visible:=false;
      FreezeAllAddresses2.visible:=true;
    end;

    n5.visible:=formsettings.cbShowAdvanced.Checked;
    Setbreakpoint1.visible:=formsettings.cbShowAdvanced.Checked;
    Findoutwhatreadsfromthisaddress1.visible:=formsettings.cbShowAdvanced.Checked;
    Findoutwhataccessesthisaddress1.Visible:=formsettings.cbShowAdvanced.Checked;


    sep1.Visible:=true;
    CalculateNewValuePart21.Caption:=strRecalculateAddress;
    CalculateNewValuePart21.Visible:=true;
//    CalculateNewValuePart21.Enabled:=(foundlist2.ItemIndex<>-1);
    N4.visible:=true;
    cut1.enabled:=true;
    copy1.enabled:=true;
    paste1.Enabled:=true;
    n1.visible:=true;
    groupoption1.visible:=true;

    Settonogroup1.Caption:=strRemoveFromGroup+IntToStr(memrec[lastselected].Group);
  end;


  if (selectedi=0) and (numberofrecords>1) then //nothing selected but there are more records
  begin
    DeleteThisRecord1.Visible:=false;
    Browsethismemoryregion1.Visible:=false;
    SetHotkey1.Visible:=false;
    FreezeAllAddresses2.Caption:=strFreezeAllAddresses;
    UnfreezeAllAddresses1.Caption:=strUnfreezeAllAddresses;

    for i:=0 to numberofrecords-1 do
      if memrec[i].Frozen then inc(number);

    if number=0 then  //there is no frozen value so unfreeze is impossible
    begin
      UnfreezeAllAddresses1.visible:=false;
      FreezeAllAddresses2.visible:=true;
    end;

    if number=numberofrecords then //all addresses have been frozen
    begin
      UnfreezeAllAddresses1.visible:=true;
      FreezeAllAddresses2.visible:=false;
    end;

    if (number>0) and (number<numberofrecords) then
    begin
      UnfreezeAllAddresses1.visible:=true;
      freezeallAddresses2.visible:=true;
    end;

    n5.Visible:=false;
    Setbreakpoint1.Visible:=false;
    Findoutwhatreadsfromthisaddress1.visible:=false;
    Findoutwhataccessesthisaddress1.Visible:=false;

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
    DeleteThisRecord1.Caption:=strDeleteAddress;
    Browsethismemoryregion1.Visible:=true;
    ShowAsHexadecimal1.Visible:=true;

    SetHotkey1.Visible:=true;
    FreezeAllAddresses2.Caption:=strfreezealladdresses;
    unFreezeallAddresses1.Caption:=strUnfreezeAllAddresses;

    for i:=0 to numberofrecords-1 do
      if memrec[i].Frozen then inc(number);

    if number=0 then  //there is no frozen value so unfreeze is impossible
    begin
      UnfreezeAllAddresses1.visible:=false;
      FreezeAllAddresses2.visible:=true;
    end;

    if number=numberofrecords then //all addresses have been frozen
    begin
      UnfreezeAllAddresses1.visible:=true;
      FreezeAllAddresses2.visible:=false;
    end;

    if (number>0) and (number<numberofrecords) then
    begin
      UnfreezeAllAddresses1.visible:=true;
      freezeallAddresses2.visible:=true;
    end;

    n5.visible:=formsettings.cbShowAdvanced.Checked;
    Setbreakpoint1.visible:=formsettings.cbShowAdvanced.Checked;
    Findoutwhatreadsfromthisaddress1.visible:=formsettings.cbShowAdvanced.Checked;
    Findoutwhataccessesthisaddress1.Visible:=formsettings.cbShowAdvanced.Checked;

    sep1.Visible:=true;
    CalculateNewValuePart21.Caption:=strRecalculateAllAddresses;
    CalculateNewValuePart21.Visible:=true;
//    CalculateNewValuePart21.Enabled:=(foundlist2.ItemIndex<>-1);

    n4.visible:=true;
    cut1.enabled:=true;
    copy1.Enabled:=true;
    paste1.Enabled:=inclipboard;
    N1.visible:=true;
    groupoption1.visible:=true;
    Settonogroup1.Caption:=strremovefromgroup+IntToStr(memrec[lastselected].Group);
  end;


  if (selectedi>1) and (numberofrecords>1) then
  begin
    DeleteThisRecord1.Visible:=true;
    DeleteThisRecord1.Enabled:=true;
    DeleteThisRecord1.Caption:=strdeleteaddress;
    Browsethismemoryregion1.Visible:=true;
    ShowAsHexadecimal1.Visible:=true;

    SetHotkey1.Visible:=true;
    FreezeAllAddresses2.Caption:=strfreezealladdresses;
    Unfreezealladdresses1.Caption:=strunfreezealladdresses;
    for i:=0 to numberofrecords-1 do
      if memrec[i].Frozen then inc(number);

    if number=0 then  //there is no frozen value so unfreeze is impossible
    begin
      UnfreezeAllAddresses1.visible:=false;
      FreezeAllAddresses2.visible:=true;
    end;

    if number=numberofrecords then //all addresses have been frozen
    begin
      UnfreezeAllAddresses1.visible:=true;
      FreezeAllAddresses2.visible:=false;
    end;

    if (number>0) and (number<numberofrecords) then
    begin
      UnfreezeAllAddresses1.visible:=true;
      freezeallAddresses2.visible:=true;
    end;

    n5.visible:=formsettings.cbShowAdvanced.Checked;
    Setbreakpoint1.visible:=formsettings.cbShowAdvanced.Checked;
    Findoutwhatreadsfromthisaddress1.visible:=formsettings.cbShowAdvanced.Checked;
    Findoutwhataccessesthisaddress1.Visible:=formsettings.cbShowAdvanced.Checked;


    sep1.Visible:=true;
    calculatenewvaluepart21.Caption:=strrecalculateselectedaddresses;
    CalculateNewValuePart21.Visible:=true;
//    CalculateNewValuePart21.Enabled:=(foundlist2.ItemIndex<>-1);
    N4.Visible:=true;
    cut1.enabled:=true;
    copy1.enabled:=true;
    paste1.enabled:=inclipboard;
    N1.visible:=true;
    groupoption1.visible:=true;
    Setbreakpoint1.Visible:=true;

    Settonogroup1.Caption:=strRemoveFromGroup;
  end;

  address1.enabled:=selectedi=1;
  ype1.Enabled:=selectedi=1;

  if (lastselected<>-1) and (numberofrecords>0) and (memrec[lastselected].VarType=255) then
  begin
    Browsethismemoryregion1.visible:=false;
    Showashexadecimal1.Visible:=false;
    SetHotkey1.Visible:=true;
    sethotkey1.Enabled:=true;
    changescript1.Visible:=true;

    UnfreezeAllAddresses1.caption:=strdisablecheat;
    FreezeAllAddresses2.caption:=strenablecheat;

    n5.visible:=false;
    Trytofindbasepointer1.Visible:=false;
    Findoutwhataccessesthisaddress1.visible:=false;
    Setbreakpoint1.visible:=false;
    Findoutwhatreadsfromthisaddress1.Visible:=false;
    sep1.visible:=false;
    Calculatenewvaluepart21.visible:=false;
    N4.visible:=true;
    n1.Visible:=false;
  end else changescript1.Visible:=false;

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

    freeze,check: boolean;
    freezegroup: array [1..6] of boolean;
    temps: pchar;

    temp: string;
    realaddress,realaddress2,count: dword;
begin

  FControl.SetFocus;

  for i:=1 to 6 do
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
      if memrec[i].IsPointer then
      begin
        //find the real address
        realaddress2:=memrec[i].pointers[length(memrec[i].pointers)-1].Address;
        for j:=length(memrec[i].pointers)-1 downto 0 do
        begin
          check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
          if check and (count=4) then
            realaddress2:=realaddress+memrec[i].pointers[j].offset;
        end;
        realaddress:=realaddress2;
      end else realaddress:=memrec[i].Address;


      if (selected[i] and not memrec[i].Frozen) or
         ((memrec[i].group>0) and freezegroup[(memrec[i].Group)])
      then //if it's selected and not yet frozen freeze it.
      begin
        //freeze that address
        memrec[i].Frozendirection:=0;
        case memrec[i].VarType of
              0       :       begin  //byte
                                readprocessmemory(processhandle,pointer(realaddress),addr(db),1,error);
                                if error=1 then
                                begin
                                  memrec[i].FrozenValue:=db;
                                  memrec[i].Frozen:=true;
                                end;
                              end;

              1       :      begin  //word
                               readprocessmemory(processhandle,pointer(realaddress),addr(dw),2,error);
                               if error=2 then
                               begin
                                 memrec[i].FrozenValue:=dw;
                                 memrec[i].Frozen:=true;
                               end;
                             end;

              2       :      begin  //dword
                               readprocessmemory(processhandle,pointer(realaddress),addr(dd),4,error);
                               if error=4 then
                               begin
                                 memrec[i].FrozenValue:=dd;
                                 memrec[i].Frozen:=true;
                               end;
                             end;

              3       :      begin
                               readprocessmemory(processhandle,pointer(realaddress),addr(a),4,error);
                               if error=4 then
                               begin
                                 controle:=FloatToStr(a);
                                 if system.pos('NAT',controle)>0 then exit;
                                 if system.pos('INF',controle)>0 then exit;
                                 frozenfvalue[i]:=a;
                                 memrec[i].Frozen:=true;
                               end;
                             end;

              4       :      begin
                               readprocessmemory(processhandle,pointer(realaddress),addr(b),8,error);
                               if error=8 then
                               begin
                                 controle:=FloatToStr(b);
                                 if system.pos('NAT',controle)>0 then exit;
                                 if system.pos('INF',controle)>0 then exit;
                                 frozenfvalue[i]:=b;
                                 memrec[i].Frozen:=true;
                               end;
                             end;

              5       :      begin  //binary
                               k:=1+((memrec[i].Bit+memrec[i].bitlength) div 8);
                               setlength(read8,k);

                               readprocessmemory(processhandle,pointer(realaddress),addr(read8[0]),k,error);
                               if error=k then
                               begin
                                 //find out the current bit combination (use frozenstrings to store the value)

                                 //convert what i need to a string of bits
                                 temp:='';
                                 j:=memrec[i].Bit;
                                 read9:=@read8[0];
                                 for k:=1 to memrec[i].bitlength do
                                 begin
                                   temp:=temp+IntToStr(getbit(j,read9^));
                                   inc(j);
                                   if j>=8 then
                                   begin
                                     j:=0;
                                     inc(read9);
                                   end;
                                 end;

                                 //the tempstring now contaisn the bits (bit0 is first char...)
                                 frozenstrings[i]:=temp;
                                 memrec[i].Frozen:=true;
                               end;
                             end;

              6       :      begin  //int64
                               readprocessmemory(processhandle,pointer(realaddress),addr(dI64),8,error);
                               if error=8 then
                               begin
                                 memrec[i].FrozenValue:=di64;
                                 memrec[i].Frozen:=true;
                               end;
                             end;

              7       :      begin  //text
                               getmem(temps,memrec[i].bit+1);
                               readprocessmemory(processhandle,pointer(realaddress),temps,memrec[i].Bit,error);
                               if error=memrec[i].bit then
                               begin
                                 temps[memrec[i].bit]:=#0;
                                 FrozenStrings[i]:=temps;
                                 memrec[i].Frozen:=true;
                               end;
                               freemem(temps);
                             end;

              8       :      begin  //array of byte
                               setlength(FrozenBytes[i],memrec[i].bit);
                               readprocessmemory(processhandle,pointer(realaddress),FrozenBytes[i],memrec[i].Bit,error);
                               if error=memrec[i].bit then
                                 memrec[i].Frozen:=true;
                             end;

              255     :      enableautoassemblecheat(i); //auto assemble script

        end;
      end;
    end
    else
    begin
      {freeze is false, so UNFREEZE all selected items and groups}
      if selected[i] then
      begin
        if memrec[i].VarType=255 then
          disableautoassemblecheat(i)
        else
          memrec[i].frozen:=false;
      end;

      if (memrec[i].group>0) and (freezegroup[memrec[i].group]) then
      begin
        if memrec[i].VarType=255 then
          disableautoassemblecheat(i)
        else
          memrec[i].frozen:=false;
      end;
    end;
  end;

  sethotkey1.Enabled:=memrec[lastselected].VarType<>255;

  UpdateScreen;
end;

procedure TMainForm.CheckBox1Click(Sender: TObject);
begin
  UpdateScreen;
end;

procedure TMainForm.Unfreezealladdresses1Click(Sender: TObject);
var i: Integer;
begin
  if memrec[lastselected].vartype=255 then
  begin
    disableautoassemblecheat(lastselected);
    exit;
  end;           

  for i:=0 to numberofrecords-1 do
  begin
    if memrec[i].VarType=255 then
      disableautoassemblecheat(i)
    else
      memrec[i].Frozen:=false;
  end;

  updatelist;
  updatescreen;
end;

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  if foundlist3.Items.Count=0 then
  begin
    Removeselectedaddresses1.enabled:=false;
    Browsethismemoryarrea1.enabled:=false;
    Selectallitems1.enabled:=false;
  end else
  begin
    Removeselectedaddresses1.enabled:=true;
    Browsethismemoryarrea1.enabled:=True;

    Selectallitems1.enabled:=foundlist3.Items.Count<5000;
  end;

  if Foundlist3.SelCount>1 then
    Removeselectedaddresses1.caption:='Remove selected addresses'
  else
    Removeselectedaddresses1.caption:='Remove selected address';

  if Foundlist3.selcount=0 then
  begin
    Removeselectedaddresses1.enabled:=false;
    Browsethismemoryarrea1.enabled:=false;
  end else
  begin
    Removeselectedaddresses1.enabled:=true;
    Browsethismemoryarrea1.enabled:=true;
  end;

  Removeselectedaddresses1.Visible:=not (GetVarType in [5,8]);

end;

procedure TMainForm.Removeselectedaddresses1Click(Sender: TObject);
var address: Dword;
    e,i,j: Integer;
    bit: Byte;
    selected: array of integer;
begin
  if SaveFirstScanThread<>nil then
  begin
    SaveFirstScanThread.WaitFor; //wait till it's done
    freeandnil(SaveFirstScanThread);
  end;

  if foundlist3.selcount=1 then //use itemindex (faster)
  begin
		foundlist.deleteaddress(foundlist3.itemindex);
	end
	else if foundlist3.selcount>1 then
	begin
    if foundlist3.items.count>100000 then
      if messagedlg('This list is huge and deleting multiple items will require CE to traverse the whole list and can take a while. Are you sure?',mtconfirmation,[mbyes,mbno],0)<>mryes then exit;

    screen.Cursor:=crhourglass;

   	MainForm.Caption:=CEWait;
    Mainform.Refresh;
    try
      j:=0;
      setlength(selected, foundlist3.selcount);
      for i:=0 to foundlist3.items.count-1 do
      begin
        if foundlist3.items[i].Selected then
        begin
          selected[j]:=i;
          inc(j);
        end;
      end;

      for i:=length(selected)-1 downto 0 do
        foundlist.deleteaddress(selected[i]);

		finally
	    Mainform.caption:=CENorm;
   		screen.Cursor:=crDefault;
		end;
	end;

	foundlist.deinitialize;
	foundlist.initialize(getvartype);
end;


procedure TMainForm.FControlKeyPress(Sender: TObject; var Key: Char);
begin
  key:=chr(0);

end;

procedure TMainForm.FControlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i,j,k: Integer;
    comfirm: Dword;
    doit,check: Boolean;
    freeze: Boolean;
    db: Byte;
    dw: Word;
    dd: dword;
    di64: Int64;
    a: single;
    b: double;
    error: Dword;
    temps: pchar;

    read8: array of byte;
    read9: pbyte;
    temp: string;

    controle: String;

    select: Integer;
    realaddress,realaddress2,count:dword;

procedure FreezeOrUnfreezeSelected;
var i,j,k: integer;
begin
   freeze:=not memrec[lastselected].frozen;

   for i:=0 to numberofrecords-1 do
   begin
     if memrec[i].ispointer then
     begin
       //find the real address
       realaddress2:=memrec[i].pointers[length(memrec[i].pointers)-1].address;
       for j:=length(memrec[i].pointers)-1 downto 0 do
       begin
         check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
         if check and (count=4) then
           realaddress2:=realaddress+memrec[i].pointers[j].offset;
       end;
       realaddress:=realaddress2;
     end else realaddress:=memrec[i].address;

     if (hotkeys[i]=-1) and (selected[i]) then
     begin
         if not freeze then
         begin
           if memrec[i].VarType=255 then
             disableautoassemblecheat(i)
           else
             memrec[i].frozen:=false;
         end
         else
         begin  //freeze
           memrec[i].frozendirection:=0;

           case memrec[i].vartype of
             0       :       begin  //byte
                               readprocessmemory(processhandle,pointer(realaddress),addr(db),1,error);
                               if error=1 then
                               begin
                                 memrec[i].frozenvalue:=db;
                                 memrec[i].frozen:=true;
                               end;
                             end;

              1       :      begin  //word
                               readprocessmemory(processhandle,pointer(realaddress),addr(dw),2,error);
                               if error=2 then
                               begin
                                 memrec[i].frozenvalue:=dw;
                                 memrec[i].frozen:=true;
                               end;
                             end;

              2       :      begin  //dword
                               readprocessmemory(processhandle,pointer(realaddress),addr(dd),4,error);
                               if error=4 then
                               begin
                                 memrec[i].frozenvalue:=dd;
                                 memrec[i].frozen:=true;
                               end;
                             end;

              3       :      begin
                               readprocessmemory(processhandle,pointer(realaddress),addr(a),4,error);
                               if error=4 then
                               begin
                                 controle:=floattostr(a);
                                 if system.pos('nat',controle)>0 then exit;
                                 if system.pos('inf',controle)>0 then exit;
                                 frozenfvalue[i]:=a;
                                 memrec[i].frozen:=true;
                               end;
                             end;

              4       :      begin
                               readprocessmemory(processhandle,pointer(realaddress),addr(b),8,error);
                               if error=8 then
                               begin
                                 controle:=floattostr(a);
                                 if system.pos('nat',controle)>0 then exit;
                                 if system.pos('inf',controle)>0 then exit;
                                 frozenfvalue[i]:=b;
                                 memrec[i].frozen:=true;
                               end;
                             end;

             5       :      begin  //bit
                              k:=1+((memrec[i].bit+memrec[i].bitlength) div 8);
                              setlength(read8,k);

                              readprocessmemory(processhandle,pointer(realaddress),addr(read8[0]),k,error);
                              if error=k then
                              begin
                                //find out the current bit combination (use frozenstrings to store the value)

                                //convert what i need to a string of bits
                                temp:='';
                                j:=memrec[i].bit;
                                read9:=@read8[0];
                                for k:=1 to memrec[i].bitlength do
                                begin
                                  temp:=temp+inttostr(getbit(j,read9^));
                                  inc(j);
                                  if j>=8 then
                                  begin
                                    j:=0;
                                    inc(read9);
                                  end;
                                end;

                                //the tempstring now contaisn the bits (bit0 is first char...)
                                frozenstrings[i]:=temp;
                                memrec[i].frozen:=true;
                              end;
                            end;

             6       :      begin  //int64
                              readprocessmemory(processhandle,pointer(realaddress),addr(di64),8,error);
                              if error=8 then
                              begin
                                memrec[i].frozenvalue:=di64;
                                memrec[i].frozen:=true;
                              end;
                            end;

             7       :      begin  //text
                              getmem(temps,memrec[i].bit+1);
                              readprocessmemory(processhandle,pointer(realaddress),temps,memrec[i].bit,error);
                              if error=memrec[i].bit then
                              begin
                                temps[memrec[i].Bit]:=#0;
                                frozenstrings[i]:=temps;
                                memrec[i].frozen:=true;
                              end;
                              freemem(temps);
                            end;

             8       :      begin  //text
                              setlength(frozenbytes[i],memrec[i].bit);
                              readprocessmemory(processhandle,pointer(realaddress),frozenbytes[i],memrec[i].bit,error);
                              if error=memrec[i].bit then
                                memrec[i].frozen:=true;
                            end;

             255      :     begin
                              enableautoassemblecheat(i);
                            end;

           end;


         end;

     end;
   end;

   updatescreen;
   updatelist;

end;

var tempmemrec: memoryrecord;
    tempfvalue: double;
    tempstring: string;
    TempSelected: boolean;
    temphotkey: integer;
    tempbytes: array of byte;
        
    sel1,sel2: integer;
    nrselected: integer;
begin
  if ((key=ord('S')) and (ssctrl in shift)) then
  begin
    if editingscript then exit;

    //swap
    nrselected:=0;
    for i:=0 to NumberOfRecords-1 do
    begin
      if selected[i] then
      begin
        inc(nrselected);
        if nrselected=1 then sel1:=i;
        if nrselected=2 then sel2:=i;
        if nrselected=3 then
        begin
          beep;
          exit;
        end;
      end;

    end;
    if nrselected<>2 then
    begin
      beep;
      exit;
    end;

    tempmemrec:=memrec[sel1];
    memrec[sel1]:=memrec[sel2];
    memrec[sel2]:=tempmemrec;

    tempfvalue:=FrozenFvalue[sel2];
    FrozenFvalue[sel2]:=FrozenFvalue[sel1];
    FrozenFvalue[sel1]:=tempFvalue;

    tempstring:=FrozenStrings[sel2];
    Frozenstrings[sel2]:=frozenstrings[sel1];
    frozenstrings[sel1]:=tempstring;

    temphotkey:=hotkeys[sel2];
    hotkeys[sel2]:=hotkeys[sel1];
    hotkeys[sel1]:=temphotkey;

    setlength(tempbytes,length(frozenbytes[sel2]));
    for j:=0 to length(tempbytes)-1 do
      tempbytes[j]:=frozenbytes[sel2][j];

    setlength(Frozenbytes[sel2],length(frozenbytes[sel1]));
    for j:=0 to length(Frozenbytes[sel2])-1 do
      Frozenbytes[sel2][j]:=FrozenBytes[sel1][j];

    setlength(frozenbytes[sel1],length(tempbytes));
    for j:=0 to length(Tempbytes)-1 do
      Frozenbytes[sel1][j]:=Tempbytes[j];



    updatescreen;
    updatelist;
    exit;
  end;

  if ((key=ord('E')) and (ssCtrl in Shift) and not (ssAlt in Shift)) then
  begin
    edit;
    exit;
  end;

  if ((key=ord('C')) and (ssCtrl in Shift) and not (ssAlt in Shift)) then
  begin
    copyselectedrecords;
    exit;
  end;

  if ((key=ord('V')) and (ssCtrl in Shift) and not (ssAlt in Shift)) then
  begin
    Paste(formsettings.cbsimplecopypaste.checked);
    exit;
  end;

  if ((key=ord('X')) and (ssCtrl in Shift) and not (ssAlt in Shift)) then
  begin
    copyselectedrecords;
    deleterecords;
    exit;
  end;

  if ((key=ord('Z')) and (ssCtrl in Shift) and not (ssAlt in Shift)) then
  begin
    //toggle the selected items
    for i:=0 to numberofrecords-1 do
      if selected[i] and (memrec[i].OldValue<>'') then
        ChangeValue(i,memrec[i].OldValue);
    exit;
  end;

  if ((key=ord('A')) and (ssctrl in Shift) and not (ssalt in Shift)) then
  begin
    //select all
    for i:=0 to numberofrecords-1 do
      selected[i]:=true;

    updatescreen;
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
                            if lastselected>scrollbar1.position+(numberoflines-1) then scrollbar1.Position:=lastselected-(numberoflines-1);

                           Updatescreen;
                          end;
                        end;

    VK_RETURN:begin
                if ssCtrl in shift then
                  description[lastselected-scrollbar1.position].OnDblClick(value[lastselected-scrollbar1.position])
                else if ssAlt in shift then
                  address[lastselected-scrollbar1.position].OnDblClick(value[lastselected-scrollbar1.position])
                else
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
                if lastselected>scrollbar1.position+(numberoflines-1) then scrollbar1.Position:=lastselected-(numberoflines-1);

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

                if lastselected>scrollbar1.position+(numberoflines-1) then scrollbar1.Position:=lastselected-(numberoflines-1);
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
                if lastselected>=scrollbar1.position+(numberoflines-1) then
                begin
                  //if it it more then 2/3th visible select it, else increase the scroller
                  //every line is 16 pixels
                  if (panel3.height mod 16)<10 then
                    scrollbar1.Position:=lastselected-(numberoflines-2)
                  else
                    scrollbar1.Position:=lastselected-(numberoflines-1);

                end;

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

                    if LastSelected>(numberoflines-1) then
                      Lastselected:=lastselected-numberoflines
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
                    if lastselected>(numberoflines-1) then
                      Lastselected:=lastselected-numberoflines
                    else
                      LastSelected:=0;
                    selected[lastselected]:=true;
                  end;
                end;

                if lastselected<scrollbar1.position then scrollbar1.Position:=lastselected;
                if lastselected>scrollbar1.position+(numberoflines-1) then scrollbar1.Position:=lastselected-(numberoflines-1);

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

                    if Lastselected+(numberoflines-1)<numberofrecords-1 then
                      Lastselected:=lastselected+(numberoflines-1)
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

                    if Lastselected+(numberoflines-1)>NumberOfRecords-1 then
                      Lastselected:=Numberofrecords-1
                    else
                      LastSelected:=Lastselected+(numberoflines-1);

                    selected[LastSelected]:=true;
                  end;
                end;

                if lastselected<scrollbar1.position then scrollbar1.Position:=lastselected;
                if lastselected>scrollbar1.position+(numberoflines-1) then scrollbar1.Position:=lastselected-(numberoflines-1);

                Updatescreen;
               end;

    vk_delete: begin
                 if editingscript then exit;

                 Deletethisrecord1.Click;
               end;

    vk_space:  begin
                 //(un)freeze all selected addresses
                 //according to the lastselected one, AND if shift/ctrl is presses
                 FreezeOrUnfreezeSelected;

               end;

    vk_add:    begin
                 if [ssctrl] = shift then
                 begin
                   //move selected records one pos down
                   if (lastselected<>-1) and (lastselected<numberofrecords) then
                   begin
                     if not selected[lastselected] then exit;
                     
                     for i:=0 to numberofrecords-1 do
                       selected[i]:=false;

                     selected[lastselected]:=true;
                     CopySelectedRecords;
                     DeleteRecords;

                     lastselected:=paste(true);

                     for i:=0 to numberofrecords-1 do
                       selected[i]:=false;

                     selected[lastselected]:=true;
                     updatescreen;
                   end;

                 end
                 else
                 begin
                   FreezeOrUnfreezeSelected;

                   for i:=0 to numberofrecords-1 do
                   begin
                     if (hotkeys[i]=-1) and (selected[i]) then
                     begin
                       memrec[i].Frozendirection:=2;
                       memrec[i].Frozen:=true;
                     end;
                   end;

                   updatescreen;
                   updatelist;
                 end;



               end;

    vk_subtract:
               begin
                 if [ssctrl] = shift then
                 begin
                   //move selected records one pos up
                   if (lastselected<>-1) and (lastselected<numberofrecords) then
                   begin
                     if (lastselected=0) or (not selected[lastselected]) then exit;

                     for i:=0 to numberofrecords-1 do
                       selected[i]:=false;

                     dec(lastselected); //cheat, just move the previous one up
                     
                     selected[lastselected]:=true;
                     CopySelectedRecords;
                     DeleteRecords;

                     lastselected:=paste(true)-1;

                     for i:=0 to numberofrecords-1 do
                       selected[i]:=false;

                     selected[lastselected]:=true;
                     updatescreen;
                   end;

                 end
                 else
                 begin
                   FreezeOrUnfreezeSelected;

                   for i:=0 to numberofrecords-1 do
                   begin
                     if (hotkeys[i]=-1) and (selected[i]) then
                     begin
                       memrec[i].Frozendirection:=1;
                       memrec[i].Frozen:=true;
                     end;
                   end;

                   updatescreen;
                   updatelist;
                 end;
               end;


  end;
  key:=0;

end;

procedure TMainForm.ScrollBar1Enter(Sender: TObject);
begin
  FControl.SetFocus;
end;

procedure TMainForm.FControlExit(Sender: TObject);
begin
  LastLastSelected:=LastSelected;
  Lastselected:=-1;
  Updatescreen;
end;

procedure TMainForm.Panel1Click(Sender: TObject);
begin
  FControl.SetFocus;
end;

procedure TMainForm.FControlEnter(Sender: TObject);
begin
  LastSelected:=LastLastSelected;
  Updatescreen;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var i: integer;
   reg: Tregistry;
   crashcounter: integer;
begin
  //close codefinder
  if foundcodedialog<>nil then
  begin
    foundcodedialog.btnOK.Click;  //first or 2nd click
    if foundcodedialog<>nil then
      foundcodedialog.btnOK.Click; //2nd click
  end;

  //undo unrandomize
  if unrandomize<>nil then
    freeandnil(unrandomize);
    
  disablestealth;

  Freememory;
  numberofrecords:=0;
  reservemem;

  cbSpeedhack.Checked:=false;
  cbFasterscan.Checked:=false;

  if flashprocessbutton<>nil then
    flashprocessbutton.Terminate;

  if debuggerthread2<>nil then
    freeandnil(debuggerthread2);

  try
  if @DebugActiveProcessStop<>@DebugActiveProcessStopProstitute then
  begin
    //detach the debugger
    hide;
    crashcounter:=0;
    if advancedoptions.Pausebutton.Down then
    begin
      advancedoptions.Pausebutton.Down:=false;
      advancedoptions.Pausebutton.Click;
    end;


    if debuggerthread<>nil then debuggerthread.Terminate;
    while (debuggerthread<>nil) and (debuggerthread.attached) and (crashcounter<10) do
    begin
      inc(crashcounter);
      sleep(1000);
    end;
  end;
  except
    //
  end;

  try
  tempbitmap.free;
  shutdown;
  closehandle(debugger.semaphore);
  unregisterhotkey(handle,0);


  for i:=0 to numberofrecords-1 do
    if hotkeys[i]<>-1 then unregisterhotkey(handle,hotkeys[i]);
  except
    //
  end;

  if advancedoptions.Pausebutton.Down then
  begin
    with advancedoptions do
    begin
      pausebutton.down:=not pausebutton.down;
      pausebutton.Click;
    end;
  end;

  if length(windowlist)>0 then
  begin
    toggleWindow;
    setforegroundwindow(lastforeground);
    setactivewindow(lastactive);
  end;

  autoattachlist.free;
 // EExternalException
end;


procedure TMainForm.CommentButtonClick(Sender: TObject);
begin
  comments.Show;
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

procedure TMainForm.SettogroupXClick(Sender: TObject);
var groupnr: Integer;
    i: Integer;
begin
  //set all selected items to group (Sender as TmenuItem).tag
  GroupNr:=(Sender as TMenuItem).Tag;
  for i:=0 to numberofrecords-1 do
    if selected[i] then memrec[i].group:=Groupnr;

  updatescreen;
end;

procedure TMainForm.Settonogroup1Click(Sender: TObject);
begin
  memrec[lastselected].Group:=0;
end;

procedure TMainForm.SetPrioritys1Click(Sender: TObject);
begin
  formsettings.showmodal;
end;

procedure TMainForm.copySelectedRecords;
var clip: TClipboard;
    textform: string;
    i,j:  Integer;
    nrselected: integer;

    ms: tmemorystream;

    identifier: string;
    x: dword;
    hentries: thandle;

    targetbuffer: pointer;
begin
  if editingscript then exit; //don't do it when editing a script

  if openclipboard(handle) then
  begin
    emptyclipboard;
    //fill the memorystream with the required data
    ms:=tmemorystream.Create;

    try
      ms.WriteBuffer('CETables',8);
      x:=copypasteversion;
      ms.WriteBuffer(x,sizeof(x));

      nrselected:=0;
      for i:=0 to numberofrecords-1 do
        if selected[i] then inc(nrselected);
      ms.WriteBuffer(nrselected,sizeof(nrselected));

      for i:=0 to numberofrecords-1 do
      begin
        if selected[i] then
        begin
          //description
          x:=length(memrec[i].description);
          ms.WriteBuffer(x,sizeof(x));
          ms.WriteBuffer(memrec[i].description[1],x);

          //address
          ms.WriteBuffer(memrec[i].address,sizeof(memrec[i].address));

          //interpretable address
          x:=length(memrec[i].interpretableaddress);
          ms.writebuffer(x,sizeof(x));
          ms.writebuffer(memrec[i].interpretableaddress[1],x);

          //vartype
          ms.WriteBuffer(memrec[i].VarType,sizeof(memrec[i].VarType));

          //unicode
          ms.WriteBuffer(memrec[i].unicode,sizeof(memrec[i].unicode));

          //IsPointer
          ms.writebuffer(memrec[i].IsPointer,sizeof(memrec[i].IsPointer));

          //pointers
          x:=length(memrec[i].pointers);
          ms.WriteBuffer(x,sizeof(x));
          for j:=0 to x-1 do
          begin
            //address
            ms.writebuffer(memrec[i].pointers[j].Address,sizeof(memrec[i].pointers[j].Address));

            //offset
            ms.writebuffer(memrec[i].pointers[j].offset,sizeof(memrec[i].pointers[j].offset));

            //interpretable address
            x:=length(memrec[i].pointers[j].Interpretableaddress);
            ms.writebuffer(x,sizeof(x));
            ms.writebuffer(memrec[i].pointers[j].interpretableaddress[1],x);
          end;

          //bit
          ms.WriteBuffer(memrec[i].Bit,sizeof(memrec[i].bit));

          //bitlength
          ms.writebuffer(memrec[i].bitlength,sizeof(memrec[i].bitlength));

          //skip frozen (always false)

          //showashex
          ms.writebuffer(memrec[i].ShowAsHex,sizeof(memrec[i].ShowAsHex));

          //autoassemble script
          x:=length(memrec[i].autoassemblescript);
          ms.writebuffer(x,sizeof(x));
          ms.writebuffer(memrec[i].autoassemblescript[1],x);
        end;
      end;

      //the stream has been filled, now globalalloc with ms.size as size
      hentries := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE,ms.Size);
      targetbuffer:=globallock(hentries);
      copymemory(targetbuffer,ms.Memory,ms.size);
      globalunlock(hentries);

      SetclipboardData(ceclipboardformat,hentries);

    finally
      ms.free;
      closeclipboard;
    end;
  end;
end;

function TMainform.paste(simplecopypaste: boolean): integer;
{
this routine will paste a entry from the cplipboard into the addresslist of CE
If simplecopypaste is false frmPasteTableentry is shown to let the user change
some stuff before adding the new entry

returns the entry number of the new addresses (first one)
}
var clip: TClipboard;
    textform,y: string;
    textform2: Pchar;
    i,j,k,l: Integer;
    last: Integer;
    temprec: array of memoryRecord;

    first:boolean;
    lines:array of string;
    linenr: integer;
    temp: dword;

    records: integer;

    stringnr: integer;

    texthandle: thandle;
    textformpos: integer;

    ms: tmemorystream;
    hentries: thandle;
    sourcebuffer: pointer;

    p: pchar;
    x: dword;

    replace_find: string;
    replace_with: string;
    changeoffsetstring: string;
    changeoffset: dword;
begin
  if editingscript then exit; //don't do it when editing a script

  //paste
  k:=0;
  j:=0;
  last:=11;

  ms:=tmemorystream.create;
  clip:=TClipboard.create;
  try
    if clip.HasFormat(ceclipboardformat) then
    begin
      frmPasteTableentry:=TfrmPasteTableentry.create(self);
      try
        if not simplecopypaste then
          if frmpastetableentry.showmodal=mrcancel then exit;

        replace_find:=frmpastetableentry.edtFind.text;
        replace_with:=frmpastetableentry.edtReplace.text;

        changeoffsetstring:='$'+stringreplace(frmpastetableentry.edtOffset.Text,'-','-$',[rfReplaceAll]);
        changeoffsetstring:=stringreplace(changeoffsetstring,'$-','-',[rfReplaceAll]);

        try
          changeoffset:=strtoint(changeoffsetstring);
        except
          changeoffset:=0;
        end;
      finally
        frmPasteTableentry.free;
      end;

      openclipboard(handle);
      try
        hentries:=GetClipboarddata(ceclipboardformat);
        sourcebuffer:=Globallock(hentries);

        ms.writebuffer(sourcebuffer^,globalsize(hentries));
        globalunlock(hentries);
      finally
        closeclipboard;
      end;

      ms.Position:=0;
      getmem(p,9);
      ms.Read(p^,8);
      p[8]:=#0;
      y:=p;
      freemem(p);

      if y<>'CETables' then exit;

      ms.readbuffer(x,sizeof(x));
      if x<>copypasteversion then exit;

      ms.readbuffer(k,sizeof(k));
      setlength(temprec,k);

      for i:=0 to k-1 do
      begin
        //description
        ms.ReadBuffer(x,sizeof(x));
        getmem(p,x+1);
        ms.ReadBuffer(p^,x);
        p[x]:=#0;
        temprec[i].Description:=p;
        freemem(p);

        if replace_find<>'' then
          temprec[i].Description:=stringreplace(temprec[i].Description,replace_find,replace_with,[rfReplaceAll,rfIgnoreCase]);

        //address
        ms.ReadBuffer(temprec[i].address,sizeof(temprec[i].address));
        temprec[i].Address:=temprec[i].address+changeoffset;

        //interpretable address
        ms.ReadBuffer(x,sizeof(x));
        getmem(p,x+1);
        ms.ReadBuffer(p^,x);
        p[x]:=#0;
        temprec[i].interpretableaddress:=p;
        freemem(p);

        if temprec[i].interpretableaddress<>'' then
        begin
          try
            x:=symhandler.getAddressFromName(temprec[i].interpretableaddress);
            x:=x+changeoffset;
            temprec[i].interpretableaddress:=symhandler.getNameFromAddress(x,true,true)
          except
            temprec[i].interpretableaddress:=inttohex(temprec[i].Address,8);
          end;
        end;

        //vartype
        ms.ReadBuffer(temprec[i].VarType,sizeof(temprec[i].VarType));

        //unicode
        ms.ReadBuffer(temprec[i].unicode,sizeof(temprec[i].unicode));

        //IsPointer
        ms.ReadBuffer(temprec[i].IsPointer,sizeof(temprec[i].IsPointer));

        ms.ReadBuffer(x,sizeof(x));
        setlength(temprec[i].pointers,x);
        for j:=0 to x-1 do
        begin
          //address
          ms.ReadBuffer(temprec[i].pointers[j].Address,sizeof(temprec[i].pointers[j].Address));
          temprec[i].pointers[j].Address:=temprec[i].pointers[j].Address+changeoffset;

          //offset
          ms.ReadBuffer(temprec[i].pointers[j].offset,sizeof(temprec[i].pointers[j].offset));

          //interpretable address
          ms.ReadBuffer(x,sizeof(x));
          getmem(p,x+1);
          ms.ReadBuffer(p^,x);
          p[x]:=#0;
          temprec[i].pointers[j].interpretableaddress:=p;
          freemem(p);

          if temprec[i].pointers[j].interpretableaddress<>'' then
          begin
            try
              x:=symhandler.getAddressFromName(temprec[i].pointers[j].interpretableaddress);
              x:=x+changeoffset;
              temprec[i].pointers[j].interpretableaddress:=symhandler.getNameFromAddress(x,true,true)
            except
              temprec[i].pointers[j].interpretableaddress:=inttohex(temprec[i].Address,8);
            end;
          end;

        end;

        //bit
        ms.ReadBuffer(temprec[i].Bit,sizeof(temprec[i].bit));

        //bitlength
        ms.ReadBuffer(temprec[i].bitlength,sizeof(temprec[i].bitlength));

        //skip frozen (always false)

        //showashex
        ms.ReadBuffer(temprec[i].ShowAsHex,sizeof(temprec[i].ShowAsHex));

        //auto assembler
        ms.ReadBuffer(x,sizeof(x));
        getmem(p,x+1);
        ms.readbuffer(p^,x);
        p[x]:=#0;
        temprec[i].autoassemblescript:=p;
        freemem(p);
      end;


    end else exit;

    inc(NumberOfRecords,k);
    ReserveMem;

    //lastselected:=-1;

//    if lastselected=-1 then lastselected:=numberofrecords-k-1;
//    if lastselected=-1 then lastselected:=0;

    result:=lastselected+1;

    for i:=numberofrecords-k-1 downto lastselected+1 do
    begin
      memrec[i+k]:=memrec[i];
      selected[i+k]:=selected[i];
      frozenfvalue[i+k]:=frozenfvalue[i];
      frozenstrings[i+k]:=frozenstrings[i];
      frozenbytes[i+k]:=frozenbytes[i];
      hotkeystrings[i+k]:=hotkeystrings[i];
      hotkeys[i+k]:=hotkeys[i];

    end;

    if numberofrecords-k=0 then
    for i:=0 to k-1 do
    begin
      memrec[i]:=temprec[i];
      hotkeys[i]:=-1;
      selected[i]:=false;
    end
    else
    for i:=lastselected+1 to lastselected+k do
    begin
      memrec[i]:=temprec[i-lastselected-1];
      hotkeys[i]:=-1;
      selected[i]:=false;
    end;

  finally
    mainform.UpdateScreen;
    mainform.Updatelist;
    ms.free;
    clip.free;
  end;
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
  Paste(formsettings.cbsimplecopypaste.checked);
  updatescreen;
  updatelist;
end;

procedure TMainForm.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i: Integer;
begin
  if not ((ssSHift in Shift) or (ssCtrl in Shift)) then
  begin
    for i:=0 to numberofrecords-1 do
    begin
      selected[i]:=false;
    end;
    updatescreen;
  end;
end;

procedure TMainForm.Sortaddressesbygroup1Click(Sender: TObject);
var temprec: memoryrecord;
    i,j:       Integer;
    smallest: Integer;
    TempSelected: boolean;
    TempFvalue: double;
    TempString: string;
begin
//sort memrec by frozen

  for i:=0 to numberofrecords-1 do
    begin
      smallest:=i;
      for j:=i to numberofrecords-1 do  //find here the smallest
        if memrec[j].group<memrec[smallest].group then smallest:=j;
      temprec:=memrec[i];
      memrec[i]:=memrec[smallest];
      memrec[smallest]:=temprec;

      tempfvalue:=FrozenFvalue[i];
      FrozenFvalue[i]:=FrozenFvalue[smallest];
      FrozenFvalue[smallest]:=tempFvalue;

      tempstring:=frozenstrings[i];
      frozenstrings[i]:=frozenstrings[smallest];
      frozenstrings[smallest]:=tempstring;

      if (selected[i]) or (selected[smallest]) then
      begin
        tempselected:=selected[i];
        selected[i]:=selected[smallest];
        selected[smallest]:=tempselected;
      end;

      if (i=lastselected) then LastSelected:=smallest;
      If (smallest=lastselected) then lastselected:=i;

    end;

  updatescreen;
  FControl.SetFocus;

end;

procedure TMainForm.Setbreakpoint1Click(Sender: TObject);
resourcestring
  strFindWhatWritesToPointer='Find out what writes to this pointer';
  strFindWhatWritesToPointer2='Find what reads from the address pointed at by this pointer';
  strcantdebugnow='You can''t use this function while you are debugging the application yourself. (Close the memory view window)';
  strpointercantberead='The pointer can''t be completly read';
var address: dword;
    realaddress,realaddress2,count: dword;
    j: integer;
    res:word;
    check: boolean;
begin
  if (not formsettings.cbKdebug.checked) then
  begin
    if (not startdebuggerifneeded) then exit;
    if debuggerthread.userisdebugging then raise exception.create(strcantdebugnow);
  end;

  address:=memrec[lastselected].Address;

  if memrec[lastselected].IsPointer then
  begin
    address:=memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Address;
    with TformPointerOrPointee.create(self) do
    begin
      button1.Caption:=strfindwhatwritestopointer;
      button2.Caption:=strfindwhatwritestopointer2;
      res:=showmodal;
      if res=mrno then
      begin
        //find what writes to the address pointer at by this pointer
        realaddress2:=memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Address;
        for j:=length(memrec[lastselected].pointers)-1 downto 0 do
        begin
          check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
          if check and (count=4) then
            realaddress2:=realaddress+memrec[lastselected].pointers[j].offset
          else
            raise exception.Create(strpointercantberead);
        end;
        address:=realaddress2;

      end
      else if res=mryes then address:=memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Address else exit;
    end;
  end;

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

procedure TMainForm.TopDisablerTimer(Sender: TObject);
begin
  setwindowpos(mainform.Handle,HWND_NOTOPMOST,mainform.left,mainform.top,mainform.width,mainform.height,SWP_SHOWWINDOW);
  TopDisabler.Enabled:=false;
end;

procedure Tmainform.ResizeScreen;
var i: integer;
    previouscount: integer;
    changed:boolean;
    FrozenWidth, DescriptionWidth, AddressWidth, typeWidth, ValueWidth: integer;
    FrozenLeft, DescriptionLeft, AddressLeft, typeLeft, ValueLeft: integer;
begin
  FrozenWidth:=headercontrol1.sections[0].width;
  descriptionwidth:=headercontrol1.sections[1].width;
  addresswidth:=headercontrol1.sections[2].width;
  typewidth:=headercontrol1.sections[3].width;
  valuewidth:=headercontrol1.sections[4].width;

  FrozenLeft:=headercontrol1.sections[0].left+2;
  descriptionleft:=headercontrol1.sections[1].left+2;
  addressleft:=headercontrol1.sections[2].left+2;
  typeleft:=headercontrol1.sections[3].left+2;
  valueleft:=headercontrol1.sections[4].left+2;

  //now addjust the numberoflines and create rows if needed
  //every row is 16 pixels so just do
  previouscount:=numberoflines;

  for i:=0 to previouscount-1 do
  begin
    if description[i].Left<>descriptionleft then
      description[i].Left:=descriptionleft;

    if description[i].Width<>descriptionwidth then
      description[i].Width:=descriptionwidth;

    if address[i].Left<>addressleft then
      address[i].Left:=addressleft;

    if address[i].Width<>addresswidth then
      address[i].Width:=addresswidth;


    if valtype[i].Left<>typeleft  then
      ValType[i].Left:=typeleft;

    if ValType[i].Width<>Typewidth then
      ValType[i].Width:=Typewidth;


    if value[i].Left<>valueleft then
      Value[i].Left:=valueleft;

    if value[i].Width<>valuewidth then
      value[i].Width:=valuewidth;

   // freezedirection[i].Left:=frozenoffset+2;
  end;



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
    setlength(freezedirection,numberoflines);


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
      frozenbox[i].Left:=frozenleft+22;
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
      description[i].Left:=descriptionleft;
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
      address[i].Left:=addressleft;
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
      ValType[i].Left:=typeleft;
      valtype[i].Width:=typewidth;
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
      Value[i].Left:=valueleft;
      value[i].Width:=valuewidth;
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

      freezedirection[i]:=TLabel.create(self);
      freezedirection[i].Caption:='';
      freezedirection[i].Font.Color:=clRed;
      freezedirection[i].Font.Style:=[fsBold];
      freezedirection[i].Font.Size:=12;

      freezedirection[i].Top:=-3+i*16;
      freezedirection[i].Left:=frozenleft+7;
      freezedirection[i].Visible:=false;
      freezedirection[i].ParentColor:=false;
      freezedirection[i].Tag:=i;
      freezedirection[i].Transparent:=true;
      freezedirection[i].OnMouseDown:=directionclick;
      freezedirection[i].OnDragDrop:=Panel1DragDrop;
      freezedirection[i].OnDragOver:=panel1DragOver;
      freezedirection[i].PopupMenu:=popupmenu2;
      freezedirection[i].Cursor:=crHandPoint;
      freezedirection[i].Parent:=panel3;

    end;
  end;
          
  updatescreen;
end;

procedure TMainForm.Panel1Resize(Sender: TObject);
begin
  resizescreen;
end;

procedure TMainForm.ScrollBar1Scroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  //bugfix? (I cant find a property in the scrollbar to do this correctly!)
//  if scrollpos>numberofrecords-(numberoflines-1) then scrollpos:=numberofrecords-(numberoflines-1);
end;

procedure TMainForm.advancedbuttonClick(Sender: TObject);
begin

  advancedoptions.Show;
end;




procedure TMainForm.HexadecimalCheckboxClick(Sender: TObject);
var x: int64;
    i: integer;
begin
  if dontconvert then exit;
{  if VarType.Text='Byte' then    getVarType:=0 else
  if VarType.Text='2 Bytes' then getVarType:=1 else
  if VarType.Text='4 Bytes' then getVarType:=2 else
  if VarType.Text='8 Bytes' then getvarType:=6 else
  if Vartype.Text='Float' then   getVarType:=3 else
  if Vartype.Text='Double' then  getVarType:=4 else
  if VarType.Text='Bit' then     getVarType:=5 else
  if vartype.Text='Text'then     getVarType:=7 else
  if vartype.Text='Array of Bytes' then getVarType:=8 else}

  if HexadecimalCheckbox.Checked then
  begin
    //convert what is in scanvalue to hexadecimal notation
    val(scanvalue.Text,x,i);
    case GetVarType of
      0: scanvalue.text:=IntToHex(byte(x),2);
      1: scanvalue.Text:=inttohex(word(x),4);
      2: scanvalue.Text:=inttohex(dword(x),8);
      6: scanvalue.Text:=inttohex(int64(x),16);
    end;

  end
  else
  begin
    //convert to decimal noatation
    case GetVarType of
      0,1,2,6: begin
                 if length(scanvalue.Text)>0 then
                 begin
                   if scanvalue.Text[1]='-' then
                     val('-$'+copy(scanvalue.Text,2,length(scanvalue.text)),x,i)
                   else
                     val('$'+scanvalue.Text,x,i);

                   scanvalue.Text:=inttostr(x);
                 end;
               end;
    end;
  end;
end;

procedure TMainForm.SetHotkey1Click(Sender: TObject);
var db: Byte;
    dw: Word;
    dd: dword;
    di64: Int64;
    a: single;
    b: double;
    error: Dword;
    controle: string;
    temps: pchar;
    i,j,k: integer;
    read8: array of byte;
    read9: pbyte;
    temp: string;
    realaddress,realaddress2,count:dword;
    check: boolean;

    hotkeyvalue: string;
    hotkeydirection: integer;
    
begin
  HotKeyForm.recnr:=lastselected;


  if hotkeys[lastselected]<>-1 then //remove this hotkey
  begin
    memrec[lastselected].Frozendirection:=0;
    oldunregisterhotkey(handle,hotkeys[lastselected]);
    hotkeys[lastselected]:=-1;
    updatescreen;
    exit;
  end;

  if memrec[lastselected].IsPointer then
  begin
    //find the real address
    realaddress2:=memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Address;
    for j:=length(memrec[lastselected].pointers)-1 downto 0 do
    begin
      check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
      if check and (count=4) then
        realaddress2:=realaddress+memrec[lastselected].pointers[j].offset;
    end;
    realaddress:=realaddress2;
  end else realaddress:=memrec[lastselected].Address;


  hotKeyForm.hotkeyvalue:=value[lastselected].Caption;

  if memrec[lastselected].VarType=255 then
    hotkeyform.autoassemble:=true
  else
    hotkeyform.autoassemble:=false;

  error:=0;
  if HotKeyForm.showModal=mrok then
  begin
    memrec[lastselected].Frozen:=false;

    //hotkeyform2
    memrec[lastselected].Frozendirection:=hotkeyform.freezedirection;

    case memrec[lastselected].VarType of
      0:      begin  //byte
                readprocessmemory(processhandle,pointer(realaddress),addr(db),1,error);
                if error=1 then
                begin
                  db:=strtoint(hotkeyform.hotkeyvalue);
                  memrec[lastselected].FrozenValue:=db;
                end
                else
                begin
                  unregisterhotkey(Handle,hotkeys[lastselected]);
                  hotkeys[lastselected]:=-1;
                end;
              end;

      1:      begin  //word
                readprocessmemory(processhandle,pointer(realaddress),addr(dw),2,error);
                if error=2 then
                begin
                  dw:=strtoint(hotkeyform.hotkeyvalue);
                  memrec[lastselected].FrozenValue:=dw
                end
                else
                begin
                  unregisterhotkey(Handle,hotkeys[lastselected]);
                  hotkeys[lastselected]:=-1;
                end;
              end;

      2:      begin  //dword
                readprocessmemory(processhandle,pointer(realaddress),addr(dd),4,error);
                if error=4 then
                begin
                  dd:=strtoint(hotkeyform.hotkeyvalue);
                  memrec[lastselected].FrozenValue:=dd
                end
                else
                begin
                  unregisterhotkey(Handle,hotkeys[lastselected]);
                  hotkeys[lastselected]:=-1;
                end;
              end;

      3:      begin
                readprocessmemory(processhandle,pointer(realaddress),addr(a),4,error);
                if error=4 then
                begin
                  controle:=FloatToStr(a);
                  if (system.pos('NAT',controle)>0) or (system.pos('INF',controle)>0) then
                  begin
                    unregisterhotkey(Handle,hotkeys[lastselected]);
                    hotkeys[lastselected]:=-1;
                  end;

                  a:=strtofloat(hotkeyform.hotkeyvalue);
                  frozenfvalue[lastselected]:=a;
                end
                else
                begin
                  unregisterhotkey(Handle,hotkeys[lastselected]);
                  hotkeys[lastselected]:=-1;
                end;
              end;

      4:      begin
                readprocessmemory(processhandle,pointer(realaddress),addr(b),8,error);
                if error=8 then
                begin
                  controle:=FloatToStr(b);
                  if (system.pos('NAT',controle)>0) or (system.pos('INF',controle)>0) then
                  begin
                    unregisterhotkey(Handle,hotkeys[lastselected]);
                    hotkeys[lastselected]:=-1;
                  end;

                  b:=strtofloat(hotkeyform.hotkeyvalue);
                  frozenfvalue[lastselected]:=b;
                end
                else
                begin
                  unregisterhotkey(Handle,hotkeys[lastselected]);
                  hotkeys[lastselected]:=-1;
                end;
              end;

      5:      begin  //binary
                k:=1+((memrec[i].Bit+memrec[i].bitlength) div 8);
                setlength(read8,k);

                readprocessmemory(processhandle,pointer(realaddress),addr(read8[0]),k,error);
                if error=k then
                begin
                  //find out the current bit combination (use frozenstrings to store the value)
                  //convert what i need to a string of bits
                  temp:='';
                  j:=memrec[i].Bit;
                  read9:=@read8[0];
                  for k:=1 to memrec[lastselected].bitlength do
                  begin
                    temp:=temp+IntToStr(getbit(j,read9^));
                    inc(j);
                    if j>=8 then
                    begin
                      j:=0;
                      inc(read9);
                    end;
                  end;

                  //the tempstring now contaisn the bits (bit0 is first char...)
                  frozenstrings[lastselected]:=temp;
                  memrec[lastselected].Frozen:=true;
                end;
              end;

      6:      begin  //int64
                readprocessmemory(processhandle,pointer(realaddress),addr(dI64),8,error);
                if error=8 then
                begin
                  di64:=strtoint64(hotkeyform.hotkeyvalue);
                  memrec[lastselected].FrozenValue:=di64
                end
                else
                begin
                  unregisterhotkey(Handle,hotkeys[lastselected]);
                  hotkeys[lastselected]:=-1;
                end;
              end;

      7:      begin  //text
                getmem(temps,memrec[lastselected].bit);
                readprocessmemory(processhandle,pointer(realaddress),temps,memrec[lastselected].Bit,error);
                if error=memrec[lastselected].bit then FrozenStrings[lastselected]:=temps
                else
                begin
                  unregisterhotkey(Handle,hotkeys[lastselected]);
                  hotkeys[lastselected]:=-1;
                end;
                freemem(temps);
              end;

      8:      begin  //array of byte
                setlength(FrozenBytes[lastselected],memrec[lastselected].bit);
                readprocessmemory(processhandle,pointer(realaddress),FrozenBytes[lastselected],memrec[lastselected].Bit,error);
                if error<>memrec[lastselected].bit then
                begin
                  unregisterhotkey(Handle,hotkeys[lastselected]);
                  hotkeys[lastselected]:=-1;
                end;
              end;

    end;

    updatescreen;
  end;
end;

procedure TMainForm.Findoutwhatreadsfromthisaddress1Click(Sender: TObject);
var address: dword;
    realaddress,realaddress2,count: dword;
    j: integer;
    res:word;
    check: boolean;
begin
  if not startdebuggerifneeded then exit;
  if debuggerthread.userisdebugging then raise exception.create('You can''t use this function while you are debugging the application yourself. (Close the memory view window)');

  address:=memrec[lastselected].Address;

  if memrec[lastselected].IsPointer then
  begin
    address:=memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Address;
    with TformPointerOrPointee.create(self) do
    begin
      res:=showmodal;
      if res=mrno then
      begin
        //find what writes to the address pointer at by this pointer
        realaddress2:=memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Address;
        for j:=length(memrec[lastselected].pointers)-1 downto 0 do
        begin
          check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
          if check and (count=4) then
            realaddress2:=realaddress+memrec[lastselected].pointers[j].offset;
        end;
        address:=realaddress2;

      end
      else if res=mryes then address:=memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Address else exit;
    end;
  end;

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

procedure TMainForm.UndoScanClick(Sender: TObject);
var i,j: integer;
    error: integer;
    a,b: string;
resourcestring
  strconfirmUndo='Do you really want to go back to the results of the previous scan?';
begin
  if messagedlg(strConfirmUndo,mtConfirmation	,[mbyes,mbno],0)=mryes then
  begin
    foundlist.Deinitialize;
    undolastscan(GetVarType,hexadecimalcheckbox.checked);
    foundcount:=foundlist.Initialize(getvartype);
    undoscan.Enabled:=false;
  end;
end;

procedure TMainform.adjustbringtofronttext;
var hotkey:string;
    reg : TRegistry;
resourcestring
  strHideForeground='will hide the foreground window';
  strHideAll='will hide all windows';
  strUnHideForeground='will bring the foreground window back';
  strUnhideAll='will bring all windows back';

begin
  if formsettings.cbHideAllWindows.Checked then
  begin
    if allwindowsareback then
    begin
      if onlyfront then fronttext:=strHideForeground
                   else fronttext:=strHideAll;
    end
    else
    begin
      if onlyfront then fronttext:=strUnhideForeground
                   else fronttext:=strUnhideAll;
    end;
  //
  end else fronttext:='brings Cheat Engine to front';



  try
    hotkey:='';
    reg:=TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\Cheat Engine',false) then
        hotkey:=reg.ReadString('BringToFrontHotkey');
    except
    end;
  finally
    reg.Free;
  end;

  if hotkey='' then label7.caption:='';
//  fronthotkey:=hotkey;
end;

procedure TMainForm.FormShow(Sender: TObject);
resourcestring
  strhappybirthday='Let''s sing Happy Birthday for Dark Byte today!';
  strXMess='Merry christmas and happy new year';
  strNewyear='And what are your good intentions for this year? ;-)';
  strfuture='Wow,I never imagined people would use Cheat Engine up to today';


  procedure Associate(ext: string);
  var  reg: TRegistry;
       preCE: string;
  begin
    reg := TRegistry.Create;
    reg.RootKey := HKEY_CLASSES_ROOT;
    reg.LazyWrite := false;

    reg.OpenKey(ext+'\shell\open\command',true);

    preCE:=reg.ReadString('preCE');
    if preCE='' then //no preCE item yet
      preCE:=reg.ReadString('');
    reg.writestring('preCE',preCE);

    reg.WriteString('',application.ExeName+' "%1"');
    reg.CloseKey;

    reg.OpenKey(ext+'\DefaultIcon', true);

    preCE:=reg.ReadString('preCE');
    if preCE='' then //no preCE item yet
      preCE:=reg.ReadString('');
    reg.writestring('preCE',preCE);

    reg.WriteString('',application.ExeName+',0');
    reg.CloseKey;

    reg.free;
  end;
var reg: tregistry;
    modifier: dword;
    key: dword;
    hotkey: string;
    year,month,day: word;
    temp:string;

    i: integer;
    outputfile: textfile;
    go:boolean;
    loadt: boolean;

    firsttime: boolean;
begin

  loadt:=false;
  edit2.Text:=format('%.1f',[1.0]);

  reg:=Tregistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if not Reg.OpenKey('\Software\Cheat Engine',false) then
    begin
      if Reg.OpenKey('\Software\Cheat Engine',true) then
      begin
        //write some default data into the register
        reg.WriteBool('Undo',true);
        reg.writeBool('Advanced',true);
        reg.writeBool('SeperateThread',true);
        reg.writebool('Use Hyperscan if posible',false);

        reg.WriteInteger('ScanThreadpriority',formsettings.combothreadpriority.itemindex);
      end;
    end;
  except

  end;


  try
    firsttime:=reg.ReadBool('First Time User');
  except
    firsttime:=true;
  end;

  if firsttime then
  begin
    reg.WriteBool('First Time User',false);

    if messagedlg('This is the first time Cheat Engine is executed. Do you want to associate files with extension .CT with Cheat Engine?',mtConfirmation,[mbyes,mbno],0)=mryes then
      associate('.CT');

    if messagedlg('Do you want to try out the tutorial?',mtconfirmation,[mbyes,mbno],0)=mryes then
      shellexecute(0,'open','Tutorial.exe','','',sw_show);
  end;


//  animatewindow(mainform.Handle,10000,AW_CENTER);
  //mainform.repaint;
  fronttext:='brings Cheat engine to front';

  if dontrunshow then exit;

  panel7.DoubleBuffered:=true;
  flashprocessbutton:=tflash.create(false);



  dontrunshow:=true;
  decodedate(now,year,month,day);
  if (month=7) and (day=1) then showmessage(strhappybirthday);
  if (month=12) and ((day=25) or (day=26)) then Showmessage(strXmess);
  if (month=1) and (day=1) then showmessage(strnewyear );
  if (month=1) and (day=1) and (year>=2010) then showmessage(strFuture);

  if (month=4) and (day=1) then aprilfools:=true;

  if aprilfools=true then
    Messagedlg('Your license to use Cheat Engine has expired. You can buy a license to use cheat engine for 1 month for $200, 6 months for only $1000 and for 1 year for only $1800.'+' If you don''t renew your license Cheat Engine will be severely limited in it''s abilities. (e.g: Next scan has been disabled)',mtwarning,[mbok],0);

  LoadSettingsFromRegistry;

  //Load the table if one was suplied
  overridedebug:=false;
  if paramcount>=1 then
  begin
    loadt:=true;

    if not (uppercase(paramstr(1))='/NOSTEALTH') then
    begin

      //moved to oncreate
      reg:=tregistry.Create;
      try
        Reg.RootKey := HKEY_CURRENT_USER;
        if Reg.OpenKey('\Software\Cheat Engine',false) then
        begin
          if reg.ReadBool('Protect CE') then
          begin
            //open kerneldata.dat and check the version
            ProtectCE;
          end;

          if reg.ReadBool('StealthOnExecute') then EnableStealth;
        end;
      finally
        reg.Free;
      end;
    end else loadt:=false;

    if uppercase(paramstr(1))='-O' then
    begin
      OverrideDebug:=true;
      loadt:=false;
    end;


    if paramcount>1 then
    begin
      //extra param
      temp:=paramstr(2);
      if length(temp)>4 then
      if temp[1]+temp[2]+temp[3]='/o=' then
      begin
        //output to:
        temp:=copy(temp,4,length(temp)-3);
        assignfile(outputfile,   temp);
        rewrite(outputfile);
        for i:=0 to numberofrecords-1 do
          writeln(outputfile,memrec[i].description+'   '+IntToHex(memrec[i].Address,8));

        closefile(outputfile);

        showmessage(temp+' has been created');
      end;
    end;
  end else
  begin
    if formsettings.cbStealth.checked then EnableStealth;
    if formsettings.cbProtectme.Checked then protectce;
  end;

  if loadt then
  begin
    LoadTable(paramstr(1),false);
    updatescreen;
    updatelist;
  end;


  if GetSystemType<3 then //not a supported os for hardware breakpoints
  with formsettings do
  begin
    rdWriteExceptions.Checked:=true;
    rbDebugRegisters.Enabled:=false;
    label6.Enabled:=false;
    label7.Enabled:=false;

    rbDebugAsBreakpoint.Enabled:=false;
    rbInt3AsBreakpoint.Checked:=true;

  end;

  if (GetSystemType<4) or (is64bitos) then  //not nt or later
  begin
    with formsettings do
    begin
      cbKernelQueryMemoryRegion.enabled:=false;
      cbKernelReadWriteProcessMemory.enabled:=false;
      cbKernelOpenProcess.enabled:=false;
      cbStealth.enabled:=false;
      cbprotectme.Enabled:=false;
      cbUndoMemoryChanges.Enabled:=false;
      cbForceUndo.enabled:=false;
      cbProcessWatcher.Enabled:=false;
      cbKDebug.enabled:=false;
      cbGlobalDebug.enabled:=false;

      TauntOldOsUser.Visible:=true;
      panel1.Enabled:=false;
      label25.Enabled:=false;
    end;
  end;



  vartypechange(vartype);
  adjustbringtofronttext;


  if aprilfools then
    caption:=cenorm+' EXPIRED!';

  if autoattachtimer.enabled then autoattachcheck;
end;



procedure TMainForm.rbBitClick(Sender: TObject);
begin
  if not cbNewscanroutine.checked then
    HexadecimalCheckbox.checked:=rbdec.checked;
    
  if not isbit then
  begin
    isbit:=true;
    //convert the value to a binary value
    try
      if scanvalue.text='' then scanvalue.text:='0' else
        scanvalue.text:=inttobin(strtoint(scanvalue.Text));
      if scanvalue.text='' then scanvalue.text:='0';
    except
     //
    end;
  end;

end;

procedure TMainForm.rbDecClick(Sender: TObject);
begin
  if not cbNewscanroutine.checked then
    HexadecimalCheckbox.checked:=rbdec.checked;

  if isbit then
  begin
    isbit:=false;
    //convert the binary text to a decimal representation
    scanvalue.Text:=IntToStr(BinToInt(scanvalue.Text));
  end;
end;

procedure TMainForm.Cut2Click(Sender: TObject);
begin
  if scanvalue.SelLength>0 then
    scanvalue.CutToClipboard;
end;

procedure TMainForm.Copy2Click(Sender: TObject);
begin
  if scanvalue.SelLength>0 then
    scanvalue.CopyToClipboard;
end;

procedure TMainForm.Paste2Click(Sender: TObject);
var cb: TClipboard;
    text: string;
    i: integer;
    allow: boolean;
begin
  cb:=tclipboard.Create;
  if cb.HasFormat(CF_TEXT) then
  begin
    if scanvalue.Focused then
      scanvalue.PasteFromClipboard;

    if (scanvalue2<>nil) and (scanvalue2.Focused) then
      scanvalue2.PasteFromClipboard;
  end;

  cb.free;
end;

procedure TMainform.checkpaste;
var cb: TClipboard;
    text: string;
    i: integer;
    allow: boolean;
begin
  cb:=tclipboard.Create;
  Paste2.enabled:=cb.HasFormat(CF_TEXT);
  cb.free;
end;

procedure TMainForm.ccpmenuPopup(Sender: TObject);
begin
  checkpaste;
end;

procedure TMainForm.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  if newsize<305 then
  begin
    newsize:=305;
    accept:=false;
  end; 
end;

procedure TMainForm.Splitter1Moved(Sender: TObject);
begin
  panel5.Repaint;
end;

procedure TMainForm.SpeedButton4Click(Sender: TObject);
var tlhlp: thandle;
    m:tagMODULEENTRY32;
    mbi: _MEMORY_BASIC_INFORMATION;
    x:dword;
    i:integer;
    Savedata: tfilestream;

    KernelBase:DWORD;
    NTDLLBase: DWORD;
    User32Base:DWORD;
    KernelDLLRegions: array of tregiondata;
    NTDLLRegions: array of tregiondata;
    User32DLLRegions: Array of tregiondata;

    oldstealthhook: thandle;
    stealth1,stealth2: boolean;
    showstealthanim: boolean;
    ceprotect: boolean;

    ntdll,kernel32,user32:thandle;

    oldmodulelist:pointer;
begin
  suspendhotkeyhandler;

  stealth1:=formsettings.cbStealth.Checked;
  stealth2:=formsettings.cbProtectMe.checked;
  ceprotect:=formsettings.cbUndoMemoryChanges.Checked;
  oldmodulelist:=modulelist;

  if formsettings.ShowModal<>mrok then
  begin
    resumehotkeyhandler;
    LoadSettingsFromRegistry;
    exit;
  end;


  resumehotkeyhandler;

  //save the memory (if needed)
  if ceprotect and (not formsettings.cbUndoMemoryChanges.Checked) then
  begin
    //stop the memory restore

  end;

  if (not ceprotect) and (formsettings.cbUndoMemoryChanges.Checked) then
  begin
    //start the memory restore
    setlength(kerneldllregions,0);
    setlength(ntdllregions,0);
    setlength(user32dllregions,0);

    //locate kernel32.dll and ntdll.dll and save them to file (I could of course read the files from harddisk, but I like this better)
    tlhlp:=createtoolhelp32snapshot(TH32CS_SNAPMODULE, getcurrentprocessid);
    m.dwSize:=sizeof(tagModuleentry32);

    DONTUseDBKQueryMemoryRegion; //I need to find out the executable locations. (and since the user told me the memory was unchanged this will not cause any problems, unless the user was braindead and clicked yes, in wich case the "Kill User" command has to be activated)
    savedata:=tfilestream.Create(cheatenginedir+'CEProtect.dat',fmCreate);
    try
      if module32first(tlhlp,m) then
      begin
        repeat
          if uppercase(pchar(@m.szModule[0]))='NTDLL.DLL' then
          begin
            ntdllbase:=dword(m.modBaseAddr);
            x:=dword(m.modBaseAddr);

            while (virtualqueryex(getcurrentprocess,pointer(x),mbi,sizeof(mbi))>0) and (x<(dword(m.modbaseaddr)+m.modBaseSize)) do
            begin
              if (mbi.Protect and (page_execute or page_execute_read or page_execute_readwrite))>0 then
              begin
                //it's executable, so save it
                setlength(NTDLLRegions,length(ntdllregions)+1);
                NTDLLRegions[length(NTDLLRegions)-1].dllnr:=1;
                NTDLLRegions[length(NTDLLRegions)-1].address:=x;
                NTDLLRegions[length(NTDLLRegions)-1].size:=mbi.RegionSize;
              end;
              inc(x,mbi.RegionSize);
            end;
          end;

          if uppercase(pchar(@m.szModule[0]))='KERNEL32.DLL' then
          begin
            kernelbase:=dword(m.modBaseAddr);
            x:=dword(m.modBaseAddr);

            while (virtualqueryex(getcurrentprocess,pointer(x),mbi,sizeof(mbi))>0) and (x<(dword(m.modbaseaddr)+m.modBaseSize)) do
            begin
              if (mbi.Protect and (page_execute or page_execute_read or page_execute_readwrite))>0 then
              begin
                //it's executable, so save it
                setlength(KERNELDLLRegions,length(KERNELDLLRegions)+1);
                KERNELDLLRegions[length(KERNELDLLRegions)-1].dllnr:=1;
                KERNELDLLRegions[length(KERNELDLLRegions)-1].address:=x;
                KERNELDLLRegions[length(KERNELDLLRegions)-1].size:=mbi.RegionSize;
              end;
              inc(x,mbi.RegionSize);
            end;
          end;

          if uppercase(pchar(@m.szModule[0]))='USER32.DLL' then
          begin
            user32base:=dword(m.modBaseAddr); //can change
            x:=dword(m.modBaseAddr);

            while (virtualqueryex(getcurrentprocess,pointer(x),mbi,sizeof(mbi))>0) and (x<(dword(m.modbaseaddr)+m.modBaseSize)) do
            begin
              if (mbi.Protect and (page_execute or page_execute_read or page_execute_readwrite))>0 then
              begin
                //it's executable, so save it
                setlength(user32DLLRegions,length(user32DLLRegions)+1);
                user32DLLRegions[length(user32DLLRegions)-1].dllnr:=1;
                user32DLLRegions[length(user32DLLRegions)-1].address:=x;
                user32DLLRegions[length(user32DLLRegions)-1].size:=mbi.RegionSize;
              end;
              inc(x,mbi.RegionSize);
            end;
          end;



        until not module32next(tlhlp,m);

      end;

      x:=undoversion;
      savedata.WriteBuffer(x,4); //version (let's just say I don't want to load the data of a older/newer version of CE)
      savedata.WriteBuffer(Kernelbase,4);
      savedata.WriteBuffer(ntdllbase,4);
      savedata.WriteBuffer(user32base,4);

      x:=length(KERNELDLLRegions); //shuld be 1 for both, but let's be safe
      savedata.WriteBuffer(x,4);
      savedata.Writebuffer(KERNELDLLRegions[0],x*sizeof(tregiondata));

      //save the memory
      for i:=0 to x-1 do
        savedata.WriteBuffer(pbyte(KERNELDLLRegions[i].address)^,KERNELDLLRegions[i].size);

      x:=length(ntdllRegions);
      savedata.WriteBuffer(x,4);
      savedata.Writebuffer(ntdllRegions[0],x*sizeof(tregiondata));
      //save the memory
      for i:=0 to x-1 do
        savedata.WriteBuffer(pbyte(NTDLLRegions[i].address)^,NTDLLRegions[i].size);

      x:=length(user32dllRegions);
      savedata.WriteBuffer(x,4);
      savedata.Writebuffer(user32dllregions[0],x*sizeof(tregiondata));
      //save the memory
      for i:=0 to x-1 do
        savedata.WriteBuffer(pbyte(user32DLLRegions[i].address)^,user32DLLRegions[i].size);


      //save the location of the api's to check. (I doubt they'd hook getprocaddress to point to a unhooked version, but let's take the safe side.....)
      //the user told us the memory is clean so getprocaddress gives us the right addresses
      kernel32:=loadlibrary('kernel32.dll');
      x:=dword(GetProcAddress(kernel32,'OpenProcess'));
      savedata.WriteBuffer(x,4);
      x:=dword(GetProcAddress(kernel32,'DebugActiveProcess'));
      savedata.WriteBuffer(x,4);
      x:=dword(GetProcAddress(kernel32,'SuspendThread'));
      savedata.WriteBuffer(x,4);
      x:=dword(GetProcAddress(kernel32,'ResumeThread'));
      savedata.WriteBuffer(x,4);
      x:=dword(GetProcAddress(kernel32,'ReadProcessMemory'));
      savedata.WriteBuffer(x,4);
      x:=dword(GetProcAddress(kernel32,'WriteProcessMemory'));
      savedata.WriteBuffer(x,4);
      FreeLibrary(kernel32);

      ntdll:=loadlibrary('ntdll.dll');
      x:=dword(GetProcAddress(ntdll,'NtOpenProcess'));
      savedata.WriteBuffer(x,4);
      freelibrary(ntdll);

      user32:=loadlibrary('user32.dll');
      x:=dworD(GetProcAddresS(user32,'SetWindowsHookExA'));
      savedata.WriteBuffer(x,4);
      freelibrary(user32);
      

      firstrun:=true;
    finally
      savedata.Free;
    end;
  end;


  //reset the hotkeys
  

  //set the memorybrowser settings if they changed
  if formsettings.cbShowDisassembler.checked then
  begin
    memorybrowser.splitter1.Visible:=true;
    memorybrowser.panel1.Visible:=true;
  end
  else
  begin
  //  panel1.Visible:=false;
    memorybrowser.Panel1.Visible:=false;
    memorybrowser.splitter1.Visible:=false;
  end;

  if formsettings.cbShowDebugoptions.Checked then
  begin
    with memorybrowser do
    begin
      panel5.visible:=false;
      splitter2.visible:=false;
      registerview.visible:=false;

      panel5.Visible:=true;
      splitter2.Visible:=true;
      registerview.Visible:=true;
    end;


    memorybrowser.view1.Visible:=true;
    memorybrowser.Debug1.Visible:=true;
    memorybrowser.Splitter2.Visible:=true;
    memorybrowser.RegisterView.Visible:=true;
    memorybrowser.Splitter2.Visible:=true;
  end
  else
  begin
    memorybrowser.RegisterView.Visible:=false;
    memorybrowser.Splitter2.Visible:=false;
    memorybrowser.Debug1.Visible:=false;
    memorybrowser.view1.Visible:=false;
  end;

  if (formsettings.cbStealth.checked) or (formsettings.cbProtectme.Checked) then
  begin
    showstealthanim:=false;
    if (not stealth1) and (formsettings.cbStealth.checked) then
      showstealthanim:=true;

    if (not stealth2) and (formsettings.cbprotectme.checked) then
      showstealthanim:=true;

    if showstealthanim then
      animatewindow(mainform.Handle,1500,AW_BLEND or AW_HIDE);


    if formsettings.cbStealth.checked then EnableStealth;
    if formsettings.cbProtectme.Checked then protectce;

    if showstealthanim then
      showwindow(mainform.Handle,sw_show);
  end;

  if not formsettings.cbStealth.checked then disablestealth;



  if formsettings.cbKernelQueryMemoryRegion.checked then UseDBKQueryMemoryRegion else DontUseDBKQueryMemoryRegion;
  if formsettings.cbKernelReadWriteProcessMemory.checked then UseDBKReadWriteMemory else DontUseDBKReadWriteMemory;
  if formsettings.cbKernelOpenProcess.Checked then UseDBKOpenProcess else DontUseDBKOpenProcess;

  adjustbringtofronttext;
end;

procedure TMainForm.cbCaseSensitiveClick(Sender: TObject);
begin
  HexadecimalCheckbox.checked:=cbcasesensitive.Checked;
end;

procedure TMainForm.LogoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button=mbright then
  begin
    About:=TAbout.create(self);
    About.showmodal;
  end;
end;

var strt: integer=0;


procedure TMainForm.directionclick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var sel: Integer;
    i: Integer;
begin
  FControl.SetFocus;


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

    if memrec[sel].Frozen then inc(memrec[sel].Frozendirection);
      
    if memrec[sel].Frozendirection=3 then memrec[sel].Frozendirection:=0;
  end;

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

  updatelist;
  Updatescreen;
end;

procedure TMainForm.btnShowRegionsClick(Sender: TObject);
begin
  formmemoryregions:=tformmemoryregions.Create(self);
  formmemoryregions.showmodal;
end;

procedure TMainForm.Findoutwhataccessesthisaddress1Click(Sender: TObject);
var address: dword;
    realaddress,realaddress2,count: dword;
    j: integer;
    res: word;
    check: boolean;
begin
  if (not formsettings.cbKdebug.checked) then
  begin
    if (not startdebuggerifneeded) then exit;
    if debuggerthread.userisdebugging then raise exception.create('You can''t use this function while you are debugging the application yourself. (Close the memory view window)');
  end;

  address:=memrec[lastselected].Address;

  if memrec[lastselected].IsPointer then
  begin
    address:=memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Address;
    with TformPointerOrPointee.create(self) do
    begin
      button1.Caption:='Find out what accesses this pointer';
      button2.Caption:='Find what accesses the address pointed at by this pointer';

      res:=showmodal;
      if res=mrno then
      begin
        //find what writes to the address pointer at by this pointer
        realaddress2:=memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Address;
        for j:=length(memrec[lastselected].pointers)-1 downto 0 do
        begin
          check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
          if check and (count=4) then
            realaddress2:=realaddress+memrec[lastselected].pointers[j].offset
          else
            raise exception.Create('The pointer can''t be completly read');
        end;
        address:=realaddress2;

      end
      else if res=mryes then address:=memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Address else exit;
    end;
  end;

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

procedure TMainForm.OpenProcesslist1Click(Sender: TObject);
begin
  speedbutton1.Click;
end;

procedure TMainForm.CloseCheatEngine1Click(Sender: TObject);
begin
  close;
end;

procedure TMainForm.Showashexadecimal1Click(Sender: TObject);
var i: integer;
begin
  for i:=0 to numberofrecords-1 do
    if selected[i] then
      memrec[i].ShowAsHex:=not memrec[lastselected].ShowAsHex;
  updatelist;
end;

procedure TMainForm.OpenMemorybrowser1Click(Sender: TObject);
begin
  Button3.click;
end;


procedure TMainForm.Trytofindbasepointer1Click(Sender: TObject);
var value:string;
    Vtype,SType: Integer;
    FromAdd,ToAdd:Dword;

    x: TBytes;

    i,j,error: integer;
    res:word;
    atf: dword;
begin
  if processhandle=0 then raise exception.Create('First open a process to be scanned');

  res:=mrcancel;
  VType:=GetVarType;
  SType:=GetScanType;

  if not (vtype in [0,1,2,3,4,6]) then raise exception.create('This function can''t work with the type you''ve selected');
  value:='';
  if InputQuery('Find base address','Please give the address the pointer should point to and I''ll try to find it. (Warning! This will take a long time, and it''s only recommended if nothing else works)',value) then
  begin
    if value='' then raise exception.Create('Please fill in an address');

    try
      atf:=StrToInt('$'+value);
    except

    end;

    //fill x
    setlength(x,length(memrec[lastselected].pointers));
    for i:=0 to length(x)-1 do
      x[length(x)-1-i]:=memrec[lastselected].pointers[i].offset;     //inverse, last first


    foundlist.Deinitialize;
    try
      screen.Cursor:=crHourglass;
      advanced:=false;
      undoscan.enabled:=false;

      foundlist.Clear;

      val('$'+FromAddress.text,FromAdd,error);
      val('$'+ToAddress.text,ToAdd,error);
      foundcountlabel.visible:=true;

      mainform.caption:=CERegionSearch;
      foundcountlabel.caption:='';

      UpdateFoundlisttimer.Enabled:=false;

      if formsettings.checkThread.checked or cbfasterscan.checked then
      begin
        formscanning:=TFormscanning.create(self);
        formscanning.button:=0;
        formscanning.scan:=3;
        formscanning.fromadd:=fromadd;
        formscanning.toadd:=toadd;
        formscanning.readonly:=readonly.checked;
        formscanning.stype:=stype;
        formscanning.vtype:=vtype;
        formscanning.fastscan:=fastscan;
        formscanning.scanvalue:=scanvalue.Text;
        formscanning.hexadecimal:=hexadecimalcheckbox.checked;
        formscanning.addresstofind:=atf;
        formscanning.LowMemoryUsage:=formsettings.cbLowMemoryUsage.checked;
        formscanning.Skip_PAGE_NOCACHE:=Skip_PAGE_NOCACHE;
        formscanning.ExtremeScan:=cbFasterscan.Checked;
        
        setlength(formscanning.pointerinfo,length(x));
        for i:=0 to length(x)-1 do
          formscanning.pointerinfo[i]:=x[i];
          
        res:=formscanning.showmodal;
      end else
      begin
        foundcount:=FindPointer(FromAdd,ToAdd,atf,progressbar1,x);
      end;

      foundlist.Clear;

      progressbar1.Position:=0;
      mainform.Caption:=CEnorm;
    finally
      screen.Cursor:=crDefault;
      setlength(x,0);
    end;

    foundlist.Initialize(2,0,true,false,false,false);
  end;
end;

procedure TMainForm.cbFastScanClick(Sender: TObject);
var i: integer;
begin
  if cbfasterscan.Checked then
  begin
    {hyperscan does not support this type}
    i:=vartype.Items.IndexOf('All (Byte to Double)');
    if i>=0 then
      vartype.Items.Delete(i);

    i:=vartype.Items.IndexOf('Custom');
    if i>=0 then
      vartype.Items.Delete(i);
  end
  else
  begin
    i:=vartype.Items.IndexOf('All (Byte to Double)');
    if i=-1 then //it isn't in the list (anymore)
      vartype.Items.add('All (Byte to Double)'); //add it back

    i:=vartype.Items.IndexOf('Custom');
    if i=-1 then
      vartype.Items.add('Custom');

  end;

  if formsettings.cbUndoMemoryChanges.checked then CheckForChanges; //place this line at important places

  if (sender=cbspeedhack) then
  begin
    if GetSystemType<4 then
      raise exception.Create(strNeedNewerWindowsVersion);
  end;

  if cbfasterscan.Checked or cbSpeedhack.Checked then
  begin
    if cefuncproc.hypermode=nil then
    begin
      try
        cefuncproc.hypermode:=THypermode.create;
      except
        cefuncproc.hypermode:=nil;
      end;
    end;
  end
  else if cefuncproc.hypermode<>nil then
  begin
    freeandnil(cefuncproc.hypermode);
  end;

  label51.visible:=cbspeedhack.checked;
  edit1.visible:=cbspeedhack.checked;
  label52.visible:=cbspeedhack.checked;
  edit2.visible:=cbspeedhack.checked;
  btnSetSpeedhack.visible:=cbspeedhack.checked;

  if cefuncproc.hypermode=nil then
  begin
    cbFasterscan.checked:=false;
    cbSpeedhack.Checked:=false;
    exit;
  end else cefuncproc.hypermode.WaitFor;

  if cefuncproc.hypermode=nil then exit; //self destruct

  if (cefuncproc.hypermode.speedhackenabled) and (not cbspeedhack.checked) then cefuncproc.hypermode.DisableSpeedhack;

  if (cefuncproc.hypermode.hyperscanwindow<>0) and
     (cbSpeedhack.checked) and
     (sender=cbSpeedhack) then
     begin
       if sendmessage(cefuncproc.hypermode.hyperscanwindow,wm_user+4,0,0)=$11223344 then
       begin
         cbspeedhack.Enabled:=true;
         cbunrandomizer.Enabled:=true;
         label52.Enabled:=true;
         label51.Enabled:=true;
         edit2.Enabled:=true;
         edit1.Enabled:=true;
         btnsetspeedhack.Enabled:=true;
         cefuncproc.hypermode.speedhackenabled:=true;
         btnSetSpeedhack.Click;
       end;
     end;
end;

procedure TMainForm.AllClickClick(Sender: TObject);
begin
  FromAddress.text:='00000000';
  ToAddress.text:='FFFFFFFF';
end;

procedure TMainForm.cbPauseWhileScanningClick(Sender: TObject);
resourcestring
  strdontbother='Don''t even bother. Cheat Engine uses the main thread to receive messages when the scan is done, freeze it and CE will crash!';
  strneeddebugger='To use this option the debugger must be attached to the game';
begin
  if (cbPauseWhileScanning.checked) and (processid=getcurrentprocessid) then
  begin
    cbPauseWhileScanning.Checked:=false;
    messagedlg(strdontbother,mtError,[mbok],0);
  end;

  if (cbPauseWhileScanning.checked) and (not startdebuggerifneeded) then
  begin
    cbPauseWhileScanning.Checked:=false;
    messagedlg(strneeddebugger,mtInformation,[mbok],0);
  end;
end;

procedure TMainForm.btnSetSpeedhackClick(Sender: TObject);
resourcestring
  strfailuretosetspeed='Failure to set the speed';
  strIncorrectspeed='The speed value is incorrect';
  strCantSetSpeed='I can''t set this speed. (must be bigger than 0)';
  strHyperscanFailed='The hyperthread failed to respond with the right answer';

var x: dword;
    speedf: single;
    speed: dword absolute speedf;
    sleeptime: integer;
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
      raise exception.Create(strincorrectspeed);
    end;
  end;

  sleeptime:=strtoint(edit1.text);

  if speedf<=0 then raise exception.Create(strcantsetspeed);
  if speedf*sleeptime<1 then
  begin
    try
      sleeptime:=trunc(roundto(1/speedf,-1));
      if sleeptime=0 then sleeptime:=1;

      edit1.Text:=IntToStr(sleeptime);
    except
      exception.Create(strfailuretosetspeed);
    end;
  end;

  x:=sendmessage(cefuncproc.hypermode.hyperscanwindow,wm_user+6,sleeptime,speed);

  if x<>12345 then raise exception.Create(strHyperscanfailed);
end;



procedure TMainform.disableHypermode;
var i: integer;
begin
  if cefuncproc.hypermode<>nil then
  begin
    cefuncproc.hypermode.WaitFor;
    if cefuncproc.hypermode.speedhackenabled then cefuncproc.hypermode.disablespeedhack;
    freeandnil(cefuncproc.hypermode);
  end;
end;

procedure TMainForm.enableHypermode;
resourcestring
  strHyperscanfailed='Hypermode didn''t respond';
  strhookfailed='I can''t set the hook required to get into the process. Check if there isn''t any anti-cheat protection running.';
var
  CEScanProcAddress:pointer;

  winhandle,possiblewinhandle: thandle;
  winprocess: dword;
  winthreadid: dword;

  x: ^byte;
  i,j: integer;
  s: string;
begin
  if cefuncproc.hypermode<>nil then exit;
  cefuncproc.hypermode:=thypermode.create;
end;

procedure TMainForm.Description1Click(Sender: TObject);
begin
  description[lastselected-scrollbar1.position].OnDblClick(value[lastselected-scrollbar1.position]);
end;

procedure TMainForm.Address1Click(Sender: TObject);
begin
  address[lastselected-scrollbar1.position].OnDblClick(value[lastselected-scrollbar1.position]);
end;

procedure TMainForm.ype1Click(Sender: TObject);
begin
  valtype[lastselected-scrollbar1.position].OnDblClick(value[lastselected-scrollbar1.position])
end;

procedure TMainForm.Value1Click(Sender: TObject);
begin
  value[lastselected-scrollbar1.position].OnDblClick(value[lastselected-scrollbar1.position])
end;

procedure TMainForm.ProcessLabelDblClick(Sender: TObject);
var peprocesS: dword;
    needed:dword;
    x: dword;
    processInfo:TProcessBasicInformation;

    buf:pchar;
begin
  if formsettings.cbKernelOpenProcess.Checked then
  begin
    if processid=0 then exit;
    
    if not IsValidHandle(processhandle) then
      if messagedlg('The process isn''t fully opened. Indicating a invalid ProcessID. You still want to find out the EPROCESS? (BSOD is possible)',mtwarning,[mbyes,mbno],0)<>mryes then exit;

    peprocess:=GetPEProcess(processid);
    showmessage('PEProcess='+IntToHex(peprocess,8));
    memorybrowser.memoryaddress:=peprocess;

  end;
end;

procedure TMainForm.Timer4Timer(Sender: TObject);
begin
  LabelModifiedmem.visible:=false;
  timer4.Enabled:=false;
end;


procedure TMainForm.ProcessLabelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (button = mbright) and (darkbytekernel<>0) and IsValidHandle(processhandle) then
    tfrmProcessInfo.Create(self).Show;
end;

procedure TMainForm.cbUnrandomizerClick(Sender: TObject);
begin
  if cbunrandomizer.checked then
  begin
    if(messagedlg('This will scan for and change some routines that are commonly used to'+' generate a random value so they always return the same. Please be aware that there is a chance it overwrites the wrong routines causing the program to crash, or that the program uses an unknown random generator. Continue?',mtwarning,[mbyes,mbno],0)=mryes) then
    begin
      unrandomize:=tunrandomize.create(true);
      with unrandomize do
      begin

        progressbar:=tprogressbar.create(self);
        progressbar.left:=twincontrol(sender).Left;
        progressbar.top:=twincontrol(sender).top;
        progressbar.Width:=twincontrol(sender).width;
        progressbar.height:=twincontrol(sender).height;

        progressbar.parent:=self;
        cbunrandomizer.enabled:=false;
        resume;
      end;
    end else cbUnrandomizer.Checked:=false;
  end
  else
  begin
    if unrandomize<>nil then
      freeandnil(unrandomize);
  end;
end;

procedure TMainForm.cbUnrandomizerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if cbunrandomizer.Checked and (button=mbright) then
  begin
    //show unrandimized addresses
    unrandomize.showaddresses;
  end;
end;

procedure TMainForm.disableautoassemblecheat(i: integer);
var code: tstringlist;
begin
  code:=tstringlist.Create;
  code.Text:=memrec[i].autoassemblescript;
  try
    memrec[i].Frozen:=not autoassemble(code,false,false,false,false,memrec[i].allocs);
    if memrec[i].Frozen then //failed, lets say if reloading the symbols can help
    begin
      symhandler.reinitialize;
      memrec[i].Frozen:=not autoassemble(code,false,false,false,false,memrec[i].allocs);
    end;
  except

  end;

  code.free;
end;

procedure TMainForm.enableautoassemblecheat(i: integer);
var code: tstringlist;
begin
  code:=tstringlist.Create;
  code.Text:=memrec[i].autoassemblescript;
  try
    setlength(memrec[i].allocs,1);
    memrec[i].Frozen:=autoassemble(code,false,true,false,false,memrec[i].allocs);

    if not memrec[i].frozen then
    begin
      symhandler.reinitialize;
      memrec[i].Frozen:=autoassemble(code,false,true,false,false,memrec[i].allocs);
    end;

  except

  end;

  code.free;
end;


procedure TMainForm.addaddress(description: string; address:dword; const offsets: array of dword; offsetcount: integer; ispointer: boolean; vartype: integer; length: integer; startbit:integer; unicode,showashex: boolean);
var i: integer;
begin
  if offsetcount=0 then ispointer:=false; //safeguard

  inc(NumberOfRecords);
  reserveMem;

  memrec[numberofrecords-1].Description:=description;
  memrec[numberofrecords-1].Address:=address;
  memrec[NumberOfRecords-1].VarType:=vartype;
  memrec[numberofrecords-1].unicode:=unicode;
  memrec[numberofrecords-1].IsPointer:=ispointer;
  memrec[numberofrecords-1].Bit:=startbit; //startbit is also used for size for strings
  memrec[numberofrecords-1].bitlength:=length;
  memrec[numberofrecords-1].showashex:=showashex;

  if ispointer then
  begin
    setlength(memrec[numberofrecords-1].pointers,offsetcount);

    for i:=0 to offsetcount-1 do
    begin
      memrec[numberofrecords-1].pointers[i].Address:=address;
      memrec[numberofrecords-1].pointers[i].offset:=offsets[i];
    end;
  end;

  memrec[NumberOfRecords-1].Frozen:=false;
  memrec[NumberOfRecords-1].FrozenValue:=0;
  FrozenFvalue[NumberOfRecords-1]:=0;
  FrozenStrings[NumberOfRecords-1]:='';
  Hotkeystrings[NumberOfRecords-1]:='';
  Hotkeys[NumberOfRecords-1]:=-1;

  UpdateScreen;
  updatelist;

end;

procedure TMainForm.addaddress(description: string; address:dword; const offsets: array of dword; offsetcount: integer; ispointer: boolean; vartype: integer; length: integer; startbit:integer; unicode: boolean);
begin
  addaddress(description,address,offsets,offsetcount,ispointer,vartype,length,startbit,unicode,false);
end;

procedure TMainForm.Foundlist3CustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if foundlist.inmodule(item.index) then foundlist3.Canvas.Font.Color:=clgreen;
end;

procedure TMainForm.actOpenExecute(Sender: TObject);
var merge: boolean;
    app: word;
    Extension: string;
resourcestring
  strUnknownExtension='Unknown extension';
begin
  if editingscript then raise exception.create('First close your script edit window before opening a new table');

  merge:=false;
  if CheckIfSaved=false then exit;

  OpenDialog1.InitialDir:=cheatenginedir;

  if Opendialog1.Execute then
  begin
    Extension:=uppercase(extractfileext(opendialog1.filename));
    if (Extension<>'.PTR') and
       (Extension<>'.AMT') and
       (Extension<>'.GH') and
       (Extension<>'.CET') and
       (Extension<>'.CT2') and
       (Extension<>'.CT3') and
       (Extension<>'.CT') and
       (Extension<>'.EXE') then raise exception.create(strUnknownExtension);


    if ((numberofrecords>0) or (advancedoptions.numberofcodes>0)) and (Extension<>'.EXE') then app:=messagedlg('You wish to merge the current table with this table?',mtConfirmation,mbYesNoCancel,0);
    case app of
      mrCancel: exit;
      mrYes: merge:=true;
      mrNo: merge:=false;
    end;

    LoadTable(Opendialog1.filename,merge);
    reinterpretaddresses;

  end;

  if advancedoptions.codelist.Count>0 then
  begin
    advancedoptions.Position:=podesigned;
    advancedoptions.Left:=mainform.left-advancedoptions.Width;
    advancedoptions.Top:=mainform.Top+mainform.Height-advancedoptions.Height;

    if (advancedoptions.Left<0) or (advancedoptions.Top+advancedoptions.Height>screen.height) then
    begin
      advancedoptions.left:=0;
      advancedoptions.Top:=screen.Height-advancedoptions.Height;

    end;
    advancedoptions.show;
  end;

  Updatescreen;
  updatelist;
end;

procedure TMainForm.actSaveExecute(Sender: TObject);
begin
  if (savedialog1.FileName='') and (opendialog1.filename<>'') then
  begin
    //set the filename the table was opened with to the filename you save as default to
    //and dont forget to change the extension to .CT
    savedialog1.FileName:=ChangeFileExt(opendialog1.FileName,'');
  end;

  SaveDialog1.InitialDir:=cheatenginedir;
  if Savedialog1.Execute then
    savetable(savedialog1.FileName);


end;

procedure TMainForm.actAutoAssembleExecute(Sender: TObject);
begin
  tfrmautoinject.create(self).show;
end;

procedure TMainform.changeScriptCallback(script: string; changed: boolean);
{
Gets called when a edit script is done
}
begin
  if editingscript then
  begin
    editingscript:=false;
    if changed then
      memrec[editedscript].autoassemblescript:=script;
    
  end else raise exception.Create('Unexpected scriptchange');
end;

procedure TMainForm.Changescript1Click(Sender: TObject);
begin
  if editingscript then raise exception.Create('First finish editing the other script');

  if lastselected<>-1 then
  begin
    with tfrmautoinject.Create(self) do
    begin

      new1.Enabled:=false;

      editscript:=true;
      editscript2:=true;
      callbackroutine:=changeScriptCallback;

      mainform.editingscript:=true;
      mainform.editedscript:=lastselected;


      assemblescreen.Text:=memrec[lastselected].autoassemblescript;

      {
      if showmodal=mrok then
      begin
        if assemblescreen.text<>memrec[lastselected].autoassemblescript then
          editedsincelastsave:=true;
          
        memrec[lastselected].autoassemblescript:=assemblescreen.text;
        free; //needed for editscript
      end; }

      formstyle:=fsStayOnTop;
      show;

    end;
  end;

end;

procedure TMainForm.Forcerechecksymbols1Click(Sender: TObject);
begin
  symhandler.reinitialize;
  symhandler.waitforsymbolsloaded;
  reinterpretaddresses;
  updatescreen;
end;

procedure TMainForm.Label5Click(Sender: TObject);
begin
{  SetProcessAffinityMask(getcurrentprocess,1);
  sleep(500);
  dbktest;
  SetProcessAffinityMask(getcurrentprocess,2);
  sleep(500);
  dbktest;
  SetProcessAffinityMask(getcurrentprocess,3);
                                                 }
end;

procedure TMainform.edit;
var frmPasteTableentry: TfrmPasteTableentry;
    replace_find: string;
    replace_with: string;
    changeoffsetstring: string;
    changeoffset,x: dword;
    i,j: integer;
    hasselected: boolean;
begin
  if NumberOfRecords=0 then exit;
  
  frmPasteTableentry:=TfrmPasteTableentry.create(self);
  try
    frmPasteTableentry.caption:='Edit addresses';
    frmPasteTableentry.Button1.Caption:='Edit';

    if frmpastetableentry.showmodal=mrcancel then exit;
    replace_find:=frmpastetableentry.edtFind.text;
    replace_with:=frmpastetableentry.edtReplace.text;

    changeoffsetstring:='$'+stringreplace(frmpastetableentry.edtOffset.Text,'-','-$',[rfReplaceAll]);
    changeoffsetstring:=stringreplace(changeoffsetstring,'$-','-',[rfReplaceAll]);

    try
      changeoffset:=strtoint(changeoffsetstring);
    except
      changeoffset:=0;
    end;
  finally
    frmPasteTableentry.free;
  end;


  hasselected:=false;
  for i:=0 to numberofrecords-1 do
  begin
    if Selected[i] then
    begin
      hasselected:=true;
      break;
    end;
  end;

  for i:=0 to numberofrecords-1 do
  begin
    if (hasselected and selected[i]) or (not hasselected) then
    begin
      memrec[i].Description:=StringReplace(memrec[i].Description,replace_find,replace_with,[rfReplaceAll,rfIgnoreCase]);
      memrec[i].Address:=memrec[i].Address+changeoffset;

      if memrec[i].interpretableaddress<>'' then
      begin
        try
          x:=symhandler.getAddressFromName(memrec[i].interpretableaddress);
          x:=x+changeoffset;
          memrec[i].interpretableaddress:=symhandler.getNameFromAddress(x,true,true)
        except
          memrec[i].interpretableaddress:=inttohex(memrec[i].Address,8);
        end;
      end;




      for j:=0 to length(memrec[i].pointers)-1 do
      begin
        //address
        memrec[i].pointers[j].Address:=memrec[i].pointers[j].Address+changeoffset;

        if memrec[i].pointers[j].interpretableaddress<>'' then
        begin
          try
            x:=symhandler.getAddressFromName(memrec[i].pointers[j].interpretableaddress);
            x:=x+changeoffset;
            memrec[i].pointers[j].interpretableaddress:=symhandler.getNameFromAddress(x,true,true)
          except
            memrec[i].pointers[j].interpretableaddress:=inttohex(memrec[i].Address,8);
          end;
        end;
      end;
    end;
  end;

  updatescreen;
  Updatelist;
end;

procedure TMainForm.Smarteditaddresses1Click(Sender: TObject);
begin
  edit;
end;

procedure TMainForm.Pointerscanforthisaddress1Click(Sender: TObject);
var res: integer;
    realaddress2,realaddress,address,count: dword;
    j: integer;
    check: boolean;
    i: integer;
begin
  try
    if memrec[lastselected].IsPointer then
    begin
      realaddress2:=memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Address;
      for j:=length(memrec[lastselected].pointers)-1 downto 0 do
      begin
        check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
        if check and (count=4) then
          realaddress2:=realaddress+memrec[lastselected].pointers[j].offset
        else
          raise exception.Create('The pointer can''t be completly read');
      end;
      address:=realaddress2;

    end
    else
      address:=memrec[lastselected].address;
  except
    address:=memrec[lastselected].address;
  end;


  with tfrmInjectedpointerscanornot.create(self) do
  begin
    res:=showmodal;
    free;
  end;

  if res=mrcancel then exit;

  if res=mryes then
  begin
    //default
    if frmpointerscanner=nil then
      frmpointerscanner:=tfrmpointerscanner.create(self);

    frmpointerscanner.show;

    if frmpointerscannersettings=nil then
      frmpointerscannersettings:=tfrmpointerscannersettings.create(self);

    frmpointerscannersettings.edtAddress.text:=inttohex(address,8);
    frmPointerScanner.Method3Fastspeedandaveragememoryusage1.Click;
  end
  else if res=mrno then
  begin
    //injected scan
    injectpointerscan(address);
  end;
end;

procedure testx(arg1:pointer; arg2:pointer; arg3:pointer); stdcall;
begin
asm
  pushad
  pushad
  popad
  popad
end;

end;

procedure TMainForm.Label53Click(Sender: TObject);
var x: integer;
begin
  asm
    mov ax,$40
    mov es,ax

  end;
  showmessage(inttohex(x,8));
end;

procedure TMainForm.pointerscanner_addaddress(var Message: TMessage);
var p: thandle;
    sourcebuffer: pointer;
    ms: tmemorystream;
    x: dword;
    address: dword;
    offsets: array of cardinal;
    i: integer;

    cds: PCOPYDATASTRUCT;
    st: pchar;
    s: string;
begin
  cds:=pointeR(message.LParam);
  if cds=nil then exit;

  try
    if cds.dwData=$ce then
    begin
      ms:=tmemorystream.Create;
      try
        getmem(sourcebuffer,cds.cbData);
        copymemory(sourcebuffer,cds.lpData,cds.cbData);

        ms.writebuffer(sourcebuffer^,cds.cbData);
        freemem(sourcebuffer);

        ms.Position:=0;
        ms.ReadBuffer(x,sizeof(x));
        getmem(st,x+1);
        ms.readbuffer(st^,x);
        st[x]:=#0;
        s:=st;
        freemem(st);

        address:=symhandler.getAddressFromName(s);

        setlength(offsets,0);

        while ms.Position<ms.Size do
        begin
          ms.ReadBuffer(x,4);
          setlength(offsets,length(offsets)+1);
          offsets[length(offsets)-1]:=x;
        end;

        addaddress('Pointerscan address',address,offsets,length(offsets),length(offsets)>0,2,0,0,false,false);
        if memrec[numberofrecords-1].IsPointer then //yes, but check anyhow
        begin
          memrec[numberofrecords-1].pointers[length(memrec[numberofrecords-1].pointers)-1].Interpretableaddress:=s;
        end;
      finally
        ms.Free;
      end;

    end;
  except

  end;
end;

procedure TMainForm.Label57Click(Sender: TObject);
{var i: dword;   }
begin
 { i:=newkernelhandler.dbvm_version;
  showmessage('dbvm version='+inttohex(i,8));  }
end;

Procedure TMainForm.plugintype1click(sender:tobject);
var selectedrecord: PPlugin1_SelectedRecord;
var x: TPluginfunctionType1;
    interpretableaddress: string[255];
    description: string[255];
    i: integer;
    offsets: PDwordArray;
begin
  interpretableaddress:=' ';
  description:=' ';

  if (lastselected<>-1) and (lastselected<numberofrecords) and (selected[lastselected]) then
  begin
    getmem(selectedrecord,sizeof(TPlugin1_SelectedRecord));
    //fill it with data

    if memrec[lastselected].IsPointer then
      interpretableaddress:=memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Interpretableaddress
    else
      interpretableaddress:=memrec[lastselected].interpretableaddress;

    selectedrecord.interpretedaddress:=@interpretableaddress[1];

    selectedrecord.address:=memrec[lastselected].Address;
    selectedrecord.ispointer:=memrec[lastselected].IsPointer;
    selectedrecord.countoffsets:=length(memrec[lastselected].pointers);

    getmem(offsets,selectedrecord.countoffsets*4); //don't forget to free
    selectedrecord.offsets:=offsets;
    for i:=0 to selectedrecord.countoffsets-1 do
      selectedrecord.offsets[i]:=memrec[lastselected].pointers[i].offset;

    description:=memrec[lastselected].Description;
    selectedrecord.description:=@description[1];

    selectedrecord.valuetype:=memrec[lastselected].VarType;
    selectedrecord.size:=memrec[lastselected].bit;    
  end
  else
    selectedrecord:=nil;

  x:=TPluginfunctionType1(tmenuitem(sender).Tag);
  if x<>nil then
  begin

    interpretableaddress[length(interpretableaddress)+1]:=#0;
    description[length(description)+1]:=#0;

    if x.callback(selectedrecord) then
    begin
      if selectedrecord<>nil then
      begin
        interpretableaddress[255]:=#0;
        description[255]:=#0;

        pbyte(@interpretableaddress[0])^:=StrLen(@interpretableaddress[1]);
        pbyte(@description[0])^:=StrLen(@description[1]);

        if memrec[lastselected].IsPointer then
          memrec[lastselected].pointers[length(memrec[lastselected].pointers)-1].Interpretableaddress:=interpretableaddress
        else
          memrec[lastselected].interpretableaddress:=interpretableaddress;

        memrec[lastselected].Description:=description;
        memrec[lastselected].VarType:=selectedrecord.valuetype;

        //load back and free memory
        freemem(offsets); //using my own var instead the user is lame enough to mess up the pointer
        reinterpretaddresses;
      end;
    end;
    //showmessage(inttohex(dword(@x.callback),8));
  end;
end;


//------------------foundlist------------------

procedure TMainForm.Foundlist3Data(Sender: TObject; Item: TListItem);
var extra: dword;
    value: string;
    address: string;
    valuetype: TVariableType;
begin
  //put in data
  try
    address:=inttohex(foundlist.GetAddress(item.Index,extra,value),8);

    if foundlist.vartype = 5 then //binary
    begin
      address:=address+'^'+inttostr(extra);
    end
    else
    if foundlist.vartype = 9 then //all
    begin
      valuetype:=TVariableType(extra);
      
      //here valuetype is stored using the new method
      case valuetype of
        vtByte: address:=address+':1';
        vtWord: address:=address+':2';
        vtDword: address:=address+':4';
        vtQword: address:=address+':8';
        vtSingle: address:=address+':s';
        vtDouble: address:=address+':d';
      end;
    end
    else //normal
      address:=inttohex(foundlist.GetAddress(item.Index,extra,value),8);

    item.Caption:=address;
    item.subitems.add(value);
  except
    showmessage(inttostr(item.index));
  end;
end;

procedure TMainForm.UpdateFoundlisttimerTimer(Sender: TObject);
begin
  foundlist.RefetchValueList;
end;

procedure TMainForm.Foundlist3KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((key=ord('A')) and (ssctrl in Shift) and not (ssalt in Shift)) then
  begin
    //select all
    if foundlist3.Items.Count<5000 then
      foundlist3.SelectAll;

  end;
end;

procedure TMainForm.mode16Click(Sender: TObject);
begin
{  disassembler.mode16:=mode16.checked; }
end;

procedure TMainForm.Label59Click(Sender: TObject);
{type TKeGetCurrentIrql=function: dword; stdcall;
     TExAllocatePool=function(Pooltype: dword; NumberOfBytes: dword):pointer; stdcall;

var new_cs,new_ss,new_ds,new_es,new_fs,new_gs: word;
    i,j: integer;
    cr0_value: dword;

    KeGetCurrentIrql: TKeGetCurrentIrql;
    ExAllocatePool:   TExAllocatePool;
    z: pointer;
label lp;    }
begin
{  //get the original segment selectors, just to be sure
  asm
    mov ax,cs
    mov [new_cs],ax
    mov ax,ss
    mov [new_ss],ax
    mov ax,ds
    mov [new_ds],ax
    mov ax,es
    mov [new_es],ax
    mov ax,fs
    mov [new_fs],ax
    mov ax,gs
    mov [new_gs],ax
  end;

  i:=0;

  KeGetCurrentIrql:=TKeGetCurrentIrql(GetKProcAddress('KeGetCurrentIrql'));
  ExAllocatePool:=TExAllocatePool(GetKProcAddress('ExAllocatePool'));


  showmessage('CS='+inttohex(new_cs,2)+' SS='+inttohex(new_ss,2)+' DS='+inttohex(new_ds,2)+' ES='+inttohex(new_es,2)+' FS='+inttohex(new_fs,2)+' GS='+inttohex(new_gs,2));
  dbvm_block_interrupts;
  dbvm_changeselectors($8,$10,$23,$23,$30,0);



  //mainform.Caption:='blaaaaaaa';

  i:=KeGetCurrentIrql;
  z:=ExAllocatePool(0,4096);

  dbvm_changeselectors(new_cs,new_ss,new_ds,new_es,new_fs,new_gs);

  dbvm_restore_interrupts;



  showmessage('Still alive! IRQL='+inttohex(i,8)+' Allocated nonpaged memory at '+inttohex(dword(z),8));
  }
end;

{
var usedkernelbase: dword=0;
    usedkernelstring: string;
function FindUsedKernel: string;
//Will return the string of the currently used kernelfile
var need: dword;
    x: PPointerArray;
    count: integer;
    drivername: pchar;
    i: integer;
begin

  if usedkernelbase=0 then
  begin
    EnumDevicedrivers(nil,0,need);
    getmem(x,need);
    try
      if enumDevicedrivers(@x[0],need,need) then
      begin
        count:=need div 8;
        getmem(drivername,200);

        try
          for i:=0 to count-1 do
          begin
            GetDevicedriverBaseName(x[i],drivername,200);
            if usedkernelbase=0 then //not found so keep looking
            begin
              if pos('krnl',drivername)>0 then  //first one to encounter is it. (and can't pick the first one sicne thats ntdll.dll)
              begin
                usedkernelbase:=dword(x[i]);
                usedkernelstring:=drivername;
                result:=drivername;
                break;
              end;
            end;

            
          end;
        finally
          freemem(drivername);
        end;
      end;
    finally
      freemem(x);
    end;
  end
  else result:=usedkernelstring;
end;

}
{
type Ttestfunc=function(a: dword): boolean; stdcall;
   }
function x(a: dword):BOOL; stdcall;
begin
  result:=(a<100);
end;
     {
var v1: real48;
    v2: Single;
    v3: double;
    v4: Extended;
    v5: Comp;
    v6: Currency;
                     }


procedure TMainForm.Label38Click(Sender: TObject);
const cnt=500000;
var
    buf: pchar;
    actualread: dword;
    a: int64;
    b: int64;
    c,d: int64;

    total: uint64;
    i,j: integer;
    fc: integer;
    tc1: dword;
    tc2: dword;

    bytes: tbytes;

    offsets: array of dword;

    xa,xb: dword;
    pr: dword;

    ba: array of boolean;
    lb: tlistbox;

    xxx: dword;
begin
{  if x(12) then exit;

  scriptengine.beginScript;
  try
    tc1:=gettickcount;
    scriptengine.x;
    tc2:=gettickcount;
  finally
    scriptengine.endScript;
  end;

  showmessage(inttostr(tc2-tc1));

  exit;
  
   {
  getmem(buf,$9001);
  total:=0;
  for i:=0 to cnt do
  begin
    QueryPerformanceCounter(c);
    readprocessmemory(processhandle,pointer($00400000),buf,$200,xa);
    QueryPerformanceCounter(d);
    total:=total+(d-c);
  end;

  d:=total;
  c:=trunc(d / cnt);

  showmessage('t='+inttostr(c ));
  exit;

  asm
    mov [0],1223
  end;

//  showmessage(inttobin(123));
  exit;

  pr:=memscan.GetProgress(xa,xb);
  showmessage(format('%d : %d - %d',[pr,xa,xb]));

  exit;

  v1:=2;
  v2:=2;
  v3:=2;
  v4:=2;
  v5:=2;
  v6:=2;

  if v1=0 then exit;
  if v2=0 then exit;
  if v3=0 then exit;
  if v4=0 then exit;
  if v5=0 then exit;
  if v6=0 then exit;


 // addaddress();
  addaddress('v1',dword(@v1),offsets,0,false,2,0,0,false,false);
  addaddress('v2',dword(@v2),offsets,0,false,2,0,0,false,false);
  addaddress('v3',dword(@v3),offsets,0,false,2,0,0,false,false);
  addaddress('v4',dword(@v4),offsets,0,false,2,0,0,false,false);
  addaddress('v5',dword(@v5),offsets,0,false,2,0,0,false,false);
  addaddress('v6',dword(@v6),offsets,0,false,2,0,0,false,false);

  beep;
exit;
  functocall:=x;

  getmem(buf,32*1024*1024);
  actualread:=0;
  total:=0;
  fc:=0;

  tc1:=gettickcount;
  QueryPerformanceCounter(c);
  for i:=1 to cnt do
  begin
    QueryPerformanceCounter(a);
    asm
      pushad

      popad
    end;

    if functocall(i) then
    //if (i mod 666)=123 then
    begin
      inc(fc);
    end;

    QueryPerformanceCounter(b);
    total:=total+(b-a);
  end;

  QueryPerformanceCounter(d);
  tc2:=gettickcount;


//  if c then showmessage('success') else showmessage('fail');
//  showmessage('wanted to read:'+inttohex(32*1024*1024,8)+' didread='+inttohex(actualread,8));
    showmessage('time='+inttostr(total div cnt)+' d-c='+inttostr((d-c) div cnt)+' '+inttostr((tc2-tc1) ));


  freemem(buf);
  exit;
      }
{  //find the current kernel, and get the base of it
  //
  findusedkernel;
  getmem(header,4096*4);
  try
    ph:=newkernelhandler.KernelOpenProcess(PROCESS_ALL_ACCESS	,true,getcurrentprocessid);
    if newkernelhandler.KernelReadProcessMemory(ph,pointer(usedkernelbase),header,4096*4,bytesread) then
    begin
      if MakeKernelCopy(usedkernelbase,peinfo_getcodebase(header)+peinfo_getcodesize(header)) then
      begin


      end else showmessage('MakeKernelCopy failed');
    end
    else
      showmessage('bla');
  finally
    freemem(header);
  end;  }
end;

procedure TMainForm.Browsethismemoryregioninthedisassembler1Click(
  Sender: TObject);
var a,b: dword;
    s: string;
begin
  if (foundlist3.ItemIndex<>-1) then
  begin
    memorybrowser.DisassemblerAddress:=foundlist.GetAddress(foundlist3.itemindex,b,s);
    memorybrowser.dselected:=memorybrowser.DisassemblerAddress;
    memorybrowser.dselected2:=$ffffffff;
    memorybrowser.updatedisassemblerview;
    memorybrowser.show;
  end;
end;

procedure TMainform.autoattachcheck;
var pl: TStringlist;
    i,j,k: integer;
    newPID: dword;
begin
  //in case there is no processwatcher this timer will be used to enumare the processlist every 2 seconds
  if (not formsettings.cbAlwaysAutoAttach.checked) and ((processhandle<>0) or (processid<>0)) then
    exit;

  pl:=tstringlist.Create;
  getprocesslist(pl);

  for i:=0 to autoattachlist.Count-1 do
  begin
    for j:=0 to pl.Count-1 do //can't do indexof
    begin
      if pos(autoattachlist.Strings[i],pl.strings[j])=10 then
      begin
        //the process is found

        val('$'+pl.strings[j],newPID,k);
        if processid=newPID then exit; //already attached to this one

        ProcessID:=newPID;
        unpause;
        DetachIfPossible;

        MainForm.ProcessLabel.caption:=pl.strings[j];
        Open_Process;
        enablegui(false);

        symhandler.reinitialize;
        reinterpretaddresses;
      end;
    end;

  end;
//  pl.IndexOf(autoattachlist.items[i]);
  pl.free;
end;

procedure TMainForm.AutoAttachTimerTimer(Sender: TObject);
begin
  autoattachcheck;
end;



procedure TMainForm.Button2Click(Sender: TObject);
var svalue2: string;
begin
  foundlist.Deinitialize; //unlock file handles

  if button2.tag=0 then
  begin
    progressbar1.min:=0;
    progressbar1.max:=1000;
    progressbar1.position:=0;

    if scanvalue2<>nil then
      svalue2:=scanvalue2.Text
    else
      svalue2:='';

    lastscantype:=scantype.ItemIndex; 

    if cbPauseWhileScanning.checked then
    begin
      advancedoptions.Pausebutton.down:=true;
      advancedoptions.Pausebutton.Click;
    end;


    memscan.firstscan(GetScanType2, getVarType2, roundingtype, scanvalue.text, svalue2, scanStart, scanStop, fastscan, scanreadonly, HexadecimalCheckbox.checked, rbdec.checked, cbunicode.checked, cbCaseSensitive.checked, SelectedCustomScanData, SelectedCustomScanType);
    DisableGui;
    SpawnCancelButton;
  end
  else if button2.tag=2 then
  begin
    //newscan
    button2.Tag:=0;
    donewscan;
    memscan.newscan; //cleanup memory and terminate all background threads    
  end;
end;

procedure TMainForm.ScanDone(var message: TMessage);
var i: integer;
    canceled: boolean;
begin
  canceled:=false;

  button2.Tag:=2;
  button2.Caption:='Scan';
  button4.tag:=0;
  progressbar1.Position:=0;

  foundcount:=memscan.GetFoundCount;


  if message.wparam>0 then
  begin
    messagedlg('Scan error:'+memscan.GetErrorString, mtError,[mbok],0);
  end;       

{  else}
//  showmessage('SCAN SUCCES. time='+inttostr(after-before));


  enablegui(memscan.LastScanType=stNextScan);
  destroyCancelButton;


  foundlist.Initialize(getvartype,i,hexadecimalcheckbox.checked,formsettings.cbShowAsSigned.Checked,formsettings.cbBinariesAsDecimal.Checked,cbunicode.checked);


  if memscan.lastscantype=stFirstScan then
  begin
    //firstscan Epilogue
    Groupbox1.Enabled:=false;
    for i:=0 to groupbox1.ControlCount-1 do
      groupbox1.Controls[i].Enabled:=false;

    vartype.Enabled:=false;
    nextscanbutton.enabled:=true;
    newscan.Caption:=strNewScan;
  end;

  beep; //let the blind user know the scan has finished (See, I'm thinking about the visually impeared users...)

  progressbar1.Position:=0;
  UpdateFoundlisttimer.Enabled:=true;
  
  Scantype.ItemIndex:=lastscantype;
  UpdateScanType;

  scanepilogue(canceled);

end;

procedure Tmainform.CancelbuttonClick(sender: TObject);
begin
  tbutton(sender).Caption:='Terminating scan...';
  memscan.terminatescan;
end;

procedure TMainForm.Button4Click(Sender: TObject);
var svalue2: string;
begin
  foundlist.Deinitialize; //unlock file handles

  if cbPauseWhileScanning.checked then
  begin
    advancedoptions.Pausebutton.down:=true;
    advancedoptions.Pausebutton.Click;
  end;

  progressbar1.min:=0;
  progressbar1.max:=1000;
  progressbar1.position:=0;

  
  if scanvalue2<>nil then
    svalue2:=scanvalue2.Text
  else
    svalue2:='';

  lastscantype:=scantype.ItemIndex;
    
  memscan.nextscan(GetScanType2, roundingtype, scanvalue.text, svalue2, scanStart, scanStop, fastscan, scanreadonly, HexadecimalCheckbox.checked, rbdec.checked, cbunicode.checked, cbCaseSensitive.checked, SelectedCustomScanData, SelectedCustomScanType);
  DisableGui;
  SpawnCancelButton;
end;

procedure TMainform.scanepilogue(canceled: boolean);
var vtype: integer;
    i: integer;
    bytes: tbytes;
begin
  vtype:=getvartype;
  if not canceled then
  begin
    case vtype of
      5: if cbNewscanroutine.checked then i:=memscan.getbinarysize else i:=nrofbits;
      7: i:=length(scanvalue.Text);
      8: //array of byte
      begin
        setlength(bytes,0);
        try
          ConvertStringToBytes(scanvalue.Text,hexadecimalcheckbox.checked,bytes );
          i:=length(bytes);
        except
          i:=1;
        end;
        setlength(bytes,0);
      end;
    end;
    foundlist.Initialize(vtype,i,hexadecimalcheckbox.checked,formsettings.cbShowAsSigned.Checked,formsettings.cbBinariesAsDecimal.Checked,cbunicode.checked);
  end
  else foundlist.Initialize(vtype); //failed scan, just reopen the addressfile

  try
    if scanvalue.Visible and scanvalue.Enabled then
    begin
      scanvalue.SetFocus;
      scanvalue.SelectAll;
    end
    else
    if not canceled then
    begin
      NextScanButton.SetFocus;
    end;
  except

  end;



  if cbPauseWhileScanning.checked then
  begin
    advancedoptions.Pausebutton.down:=false; //resume
    advancedoptions.Pausebutton.Click;
  end;

end;

//-----------------custom scan-----------------//
resourcestring strCustomScanConfig='Custom scan config';
procedure TMainform.EditCustomScanButtonClick(sender: TObject);
var autoinjectform: TFrmAutoInject;
    scandata: tstringlist;
    scriptname: string;
begin
  scandata:=SelectedCustomScanData;
  if scandata=nil then exit;

  autoinjectform:=TFrmAutoInject.Create(self);
  try
    with autoinjectform do
    begin
      caption:=caption+' '+strCustomScanConfig;

      assemblescreen.Text:=scandata.text;
      new1.Enabled:=false;

      editscript:=true;
      injectintomyself:=true;      

      if showmodal=mrok then
      begin
        scriptname:=customscanscripts[scantype.itemindex].name;
        if not inputquery(strCustomScanConfig,'The script seems to be valid. What is the name of this script?',scriptname) then exit;

        scandata.Text:=assemblescreen.Text;
        customscanscripts[scantype.itemindex].name:=scriptname;
      end;
      

    end;
  finally
    autoinjectform.free;
  end;
end;

procedure TMainform.CreateCustomScanButtonClick(sender: TObject);
{
Open a autoassembler window and fill it in.
Give hints that people can use loadlibratry and functions from dll's
}

var scriptname: string;
    i: integer;
begin

  with tfrmautoinject.Create(self) do
  begin
    try
      caption:=caption+' '+strCustomScanConfig;
      with assemblescreen.lines do
      begin
        add('[enable]');
        add('{do not change the allocnames of the following code, you are free to add new allocs though');
        add('of course then don''t forget to dealloc them at [disable] as well}');
        add('alloc(checkroutine,2048)');
        add('alloc(prologue,2048)');
        add('alloc(epilogue,2048)');
        add('alloc(fastscanstepsize,4)');
        add('alloc(variablesize,4)');
        add('alloc(firstscan,4)');
        add('alloc(scantext,4) //will get the pointer to the given string');
        add('alloc(scanvalue,8) //will get the value of the input string converted to an 8-byte value');
        add('alloc(singlescanvalue,4) //will get the float type of the input');
        add('alloc(doublescanvalue,8) //will get the double type of the input');
        add('');
        add('variablesize:');
        add('dd 4 //defines how many bytes get saved for each found result');
        add('');
        add('fastscanstepsize:');
        add('dd 1 //defines the stepsize when using fastscan (1=no difference)');
        add('');
        add('firstscan:');
        add('dd 0 //set to 1 if you want the old value to be that of the first scan');

        add('');
        add('/* routines: ');
        add('Hint: You can write these routines in any language you like and export them as dll''s. ');
        add('Then use loadlibraty and call exportfunction to use them*/');
        add('');
        add('checkroutine:');
        add('/*');
        add('edx=pointer to new value');
        add('ecx=pointer to old value');
        add('*/');
        add('');
        add('//example of 4-byte "exact value" scan for the value 100:');
        add('mov eax,[edx]  //eax gets the new value');
        add('cmp eax,#100  //compare eax with #100, # tells the assembler to read it as a decimal instead of hex');
        add('setz al //sets al to 1 if match, 0 if false (upper bits of eax are ignored)');
        add('ret');
        add('');

        add('prologue:');
        add('//You can put some code here that gets executed BEFORE the scan starts');
        add('ret');
        add('');
        add('epilogue:');
        add('//You can put some code here that gets executed AFTER the scan finishes');
        add('ret');
        add('');


        add('[disable]');
        add('dealloc(checkroutine)');
        add('dealloc(prologue,2048)');
        add('dealloc(epilogue,2048)');
        add('dealloc(fastscanstepsize)');
        add('dealloc(variablesize)');
        add('dealloc(scantext)');
        add('dealloc(scanvalue)');
        add('dealloc(singlescanvalue)');
        add('dealloc(doublescanvalue)');
      end;

      new1.Enabled:=false;

      editscript:=true;
      injectintomyself:=true;

      if showmodal=mrok then
      begin
        scriptname:='script '+inttostr(length(customscanscripts)+1);
        if not inputquery(strCustomScanConfig,'The script seems to be valid. What is the name of this script?',scriptname) then exit;

        //create a new entry in the custom list
        i:=length(customscanscripts);
        setlength(CustomScanScripts,i+1);

        CustomScanScripts[i].name:=scriptname;
        CustomScanScripts[i].data:=tstringlist.create;
        CustomScanScripts[i].data.Text:=assemblescreen.Text;
        CustomScanScripts[i].CustomScanType:=cstAutoAssembler;

        UpdateScanType; //reload the custom script list

        scantype.ItemIndex:=i;
      end;
    finally
      free; //clean up
    end;
  end;


end;

procedure TMainForm.ScanTypeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
    index: integer;
begin
  index:=scantype.itemindex;

  if (key=vk_delete) and (getSelectedVariableType=vtCustom) and (index>=0) then //custom scan and something is selected
    if  messagedlg('Delete this custom script?',mtConfirmation,[mbyes,mbno],0)=mryes then
    begin
      CustomScanScripts[index].data.free;
      for i:=index to length(customscanscripts)-2 do
        CustomScanScripts[i]:=CustomScanScripts[i+1];

      setlength(CustomScanScripts,length(CustomScanScripts)-1);
      scantype.Items.Delete(index);

      if length(CustomScanScripts)>=index then
        scantype.ItemIndex:=length(CustomScanScripts)-1;

      scantype.Refresh;
    end;

end;

procedure TMainForm.HeaderControl1SectionClick(
  HeaderControl: THeaderControl; Section: THeaderSection);
begin
  if editingscript then exit; //no sorting till the edit is done

  case section.Index of
    0: sortbyfrozenbutton.Click;
    1: sortbydescriptionbutton.Click;
    2: sortbyaddressbutton.Click;
    3: sortbytypebutton.Click;
    4: sortbyvaluebutton.Click;
  end;
end;

procedure TMainForm.HeaderControl1SectionResize(
  HeaderControl: THeaderControl; Section: THeaderSection);
var x: integer;
begin
  x:=(headercontrol1.Sections[headercontrol1.Sections.Count-1].Left+headercontrol1.Sections[headercontrol1.Sections.Count-1].Width);
  scrollbox1.HorzScrollBar.Range:=x;
  resizescreen;

end;

end.














