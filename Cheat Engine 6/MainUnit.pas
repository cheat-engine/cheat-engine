unit MainUnit;

{$MODE Delphi}

interface

uses
  jwaWindows, Windows, LCLIntf, LCLProc, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, ComCtrls, StdCtrls, Menus, CEFuncproc, Buttons, shellapi,
  imagehlp, ExtCtrls, Dialogs, Clipbrd, CEDebugger,kerneldebugger, assemblerunit,
  hotkeyhandler, registry, Math, ImgList, commctrl, NewKernelHandler,
  unrandomizer, symbolhandler, ActnList, LResources, hypermode, memscan,
  autoassembler, plugin, savefirstscan, menuitemExtra, speedhack2,AccessCheck,
  frmCScriptUnit, foundlisthelper, disassembler,peinfounit, PEInfoFunctions,
  simpleaobscanner, pointervaluelist, ManualModuleLoader, underc, debughelper,
  frmRegistersunit,ctypes, addresslist,addresslisthandlerunit, memoryrecordunit,
  windows7taskbar


  {, , formsextra ,KIcon, windows7taskbar};

//the following are just for compatibility



const
  copypasteversion = 4;

const
  wm_freedebugger = WM_USER + 1;

const
  wm_scandone = WM_USER + 2;


type
  TFlash = class(TThread)
  public
    procedure Execute; override;
    procedure col;
  end;


type
  TToggleWindows = class(TThread)
  private
  public
    constructor Create(CreateSuspended: boolean);
    procedure Execute; override;
  end;


type
  grouptype = array[1..6] of boolean;

type

  { TMainForm }

  TMainForm = class(TForm)
    CreateGroup: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miFreezePositive: TMenuItem;
    miFreezeNegative: TMenuItem;
    Panel1: TPanel;
    UpdateTimer: TTimer;
    FreezeTimer: TTimer;
    PopupMenu2: TPopupMenu;
    Deletethisrecord1: TMenuItem;
    Browsethismemoryregion1: TMenuItem;
    Calculatenewvaluepart21: TMenuItem;
    Freezealladdresses2: TMenuItem;
    sep1: TMenuItem;
    N1: TMenuItem;
    N4: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Cut1: TMenuItem;
    Setbreakpoint1: TMenuItem;
    SetHotkey1: TMenuItem;
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
    Windows: TRadioButton;
    ReadOnly: TCheckBox;
    FromAddress: TMemo;
    ToAddress: TMemo;
    NewScan: TButton;
    NextScanButton: TButton;
    ScanType: TComboBox;
    VarType: TComboBox;
    btnMemoryView: TButton;
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
    btnShowRegions: TButton;
    Foundlist3: TListView;
    Findoutwhataccessesthisaddress1: TMenuItem;
    Showashexadecimal1: TMenuItem;
    rbAllMemory: TRadioButton;
    Panel7: TPanel;
    SpeedButton1: TSpeedButton;
    cbPauseWhileScanning: TCheckBox;
    Change1: TMenuItem;
    Description1: TMenuItem;
    Address1: TMenuItem;
    ype1: TMenuItem;
    Value1: TMenuItem;
    LabelModifiedmem: TLabel;
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
    LogoPanel: TPanel;
    Logo: TImage;
    Panel14: TPanel;
    btnSetSpeedhack2: TButton;
    editSH2: TEdit;
    Label54: TLabel;
    tbSpeed: TTrackBar;
    Label56: TLabel;
    Label60: TLabel;
    cbSpeedhack: TCheckBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Process1: TMenuItem;
    Help1: TMenuItem;
    Edit3: TMenuItem;
    About1: TMenuItem;
    OpenProcess1: TMenuItem;
    Save1: TMenuItem;
    Load1: TMenuItem;
    Settings1: TMenuItem;
    N6: TMenuItem;
    a1: TMenuItem;
    b1: TMenuItem;
    c1: TMenuItem;
    d1: TMenuItem;
    e1: TMenuItem;
    CreateProcess1: TMenuItem;
    New1: TMenuItem;
    N7: TMenuItem;
    ools1: TMenuItem;
    N8: TMenuItem;
    Helpindex1: TMenuItem;
    actScriptEngine: TAction;
    Plugins2: TMenuItem;
    actMemoryView: TAction;
    Label61: TLabel;
    actOpenProcesslist: TAction;
    procedure Address1Click(Sender: TObject);
    procedure Description1Click(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure Foundlist3Resize(Sender: TObject);
    procedure CreateGroupClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure miFreezeNegativeClick(Sender: TObject);
    procedure miFreezePositiveClick(Sender: TObject);
    procedure Panel5Resize(Sender: TObject);
    procedure rbAllMemoryChange(Sender: TObject);
    procedure ShowProcessListButtonClick(Sender: TObject);
    procedure NewScanClick(Sender: TObject);
    procedure NextScanButtonClick(Sender: TObject);
    procedure btnMemoryViewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AddressKeyPress(Sender: TObject; var Key: char);
    procedure FoundListDblClick(Sender: TObject);
    procedure Browsethismemoryarrea1Click(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure FreezeTimerTimer(Sender: TObject);
    procedure Browsethismemoryregion1Click(Sender: TObject);
    procedure Deletethisrecord1Click(Sender: TObject);
    procedure ScanvalueoldKeyPress(Sender: TObject; var Key: char);
    procedure Calculatenewvaluepart21Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ScanTypeChange(Sender: TObject);
    procedure Value1Click(Sender: TObject);
    procedure VarTypeChange(Sender: TObject);
    procedure LogoClick(Sender: TObject);
    procedure WindowsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Selectallitems1Click(Sender: TObject);
    procedure Label37Click(Sender: TObject);
    procedure Freezealladdresses2Click(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure Unfreezealladdresses1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Removeselectedaddresses1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CommentButtonClick(Sender: TObject);
    procedure CommentButtonMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure Copy1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Setbreakpoint1Click(Sender: TObject);
    procedure TopDisablerTimer(Sender: TObject);
    procedure advancedbuttonClick(Sender: TObject);
    procedure HexadecimalCheckboxClick(Sender: TObject);
    procedure SetHotkey1Click(Sender: TObject);
    procedure UndoScanClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbBitClick(Sender: TObject);
    procedure rbDecClick(Sender: TObject);
    procedure Cut2Click(Sender: TObject);
    procedure Copy2Click(Sender: TObject);
    procedure Paste2Click(Sender: TObject);
    procedure ccpmenuPopup(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: integer;
      var Accept: boolean);
    procedure Splitter1Moved(Sender: TObject);
    procedure SettingsClick(Sender: TObject);
    procedure cbCaseSensitiveClick(Sender: TObject);
    procedure LogoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure btnShowRegionsClick(Sender: TObject);
    procedure Findoutwhataccessesthisaddress1Click(Sender: TObject);
    procedure OpenProcesslist1Click(Sender: TObject);
    procedure CloseCheatEngine1Click(Sender: TObject);
    procedure Showashexadecimal1Click(Sender: TObject);
    procedure OpenMemorybrowser1Click(Sender: TObject);
    procedure cbFastScanClick(Sender: TObject);
    procedure rbAllMemoryClick(Sender: TObject);
    procedure cbPauseWhileScanningClick(Sender: TObject);
    procedure ProcessLabelDblClick(Sender: TObject);
    procedure ProcessLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure cbUnrandomizerClick(Sender: TObject);
    procedure cbUnrandomizerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Foundlist3CustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: boolean);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actAutoAssembleExecute(Sender: TObject);
    procedure Changescript1Click(Sender: TObject);
    procedure Forcerechecksymbols1Click(Sender: TObject);
    procedure Smarteditaddresses1Click(Sender: TObject);
    procedure Pointerscanforthisaddress1Click(Sender: TObject);
    procedure Label53Click(Sender: TObject);
    procedure Foundlist3Data(Sender: TObject; Item: TListItem);
    procedure UpdateFoundlisttimerTimer(Sender: TObject);
    procedure Foundlist3KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure Label59Click(Sender: TObject);
    procedure Browsethismemoryregioninthedisassembler1Click(Sender: TObject);
    procedure AutoAttachTimerTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ScanTypeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure tbSpeedChange(Sender: TObject);
    procedure btnSetSpeedhack2Click(Sender: TObject);
    procedure cbSpeedhackClick(Sender: TObject);
    procedure Process1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure CreateProcess1Click(Sender: TObject);
    procedure Helpindex1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure actScriptEngineExecute(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure Label61Click(Sender: TObject);
    procedure actOpenProcesslistExecute(Sender: TObject);
    procedure ype1Click(Sender: TObject);
  private
    oldscanvalue2text: string;
    aaa: single;
    hotkeypressed: integer;

    cancelbutton: TButton;
    //cancel button that spawns during a scan, disabled initially to prevent doubleclick accidents
    cancelbuttonenabler: TTimer;
    //timer that will enable the cancelbutton after 3 seconds

    CreateCustomScanButton: TButton;
    EditCustomScanButton: TButton;
    CustomScanScripts: array of record
      Name: string;
      Data: TStringList;
      CustomScanType: TCustomScanType;
    end;



    oldwidth, oldheight: integer;
    newaddress: ptrUint;
    isbit: boolean;
    tempbitmap: Tbitmap;
    dontrunshow: boolean;

    LastWasHex: boolean;
    dontconvert: boolean;
    FlashProcessButton: TFlash;
    oldvartype: integer;

    unrandomize: Tunrandomize;


    scantext2: tlabel;
    andlabel: tlabel;
    scanvalue2: tedit;
    cbpercentage: tcheckbox;

    reinterpretcheck: integer;

    ffoundcount: int64;

    SaveFirstScanThread: TSaveFirstScanThread;

    foundlist: Tfoundlist;
    lastscantype: integer;

    oldhandle: thandle;

    hexstateForIntTypes: boolean;

    function openprocessPrologue: boolean;
    procedure openProcessEpilogue(oldprocessname: string; oldprocess: dword; oldprocesshandle: dword);
    procedure doNewScan;
    procedure SetExpectedTableName;
    procedure autoattachcheck;
    procedure aprilfoolsscan;
    function CheckIfSaved: boolean;
    procedure checkpaste;
    procedure hotkey(var Message: TMessage); message WM_HOTKEY;
    procedure WMGetMinMaxInfo(var Message: TMessage); message WM_GETMINMAXINFO;
    procedure Hotkey2(var Message: TMessage); message wm_hotkey2;
    procedure ScanDone(var message: TMessage); message WM_SCANDONE;
    procedure Edit;
    function paste(simplecopypaste: boolean): integer;
    procedure CopySelectedRecords;


    procedure exceptionhandler(Sender: TObject; E: Exception);
    procedure toggleWindow;
    procedure adjustbringtofronttext;

    procedure scanEpilogue(canceled: boolean);
    procedure CancelbuttonClick(Sender: TObject);
    procedure CancelbuttonenablerInterval(Sender: TObject);
    procedure CreateCustomScanButtonClick(Sender: TObject);
    procedure EditCustomScanButtonClick(Sender: TObject);

    procedure changeScriptCallback(memrec: TMemoryRecord; script: string; changed: boolean);

    //processlist
    procedure ProcessItemClick(Sender: TObject);

    //property functions
    function GetRoundingType: TRoundingType;
    procedure SetRoundingType(rt: TRoundingType);
    function getScanStart: ptruint;
    procedure setScanStart(newscanstart: ptruint);
    function getScanStop: ptruint;
    procedure setScanStop(newscanstop: ptruint);
    function getFastscan: boolean;
    procedure setFastScan(state: boolean);
    function getScanReadonly: boolean;
    procedure setScanReadOnly(state: boolean);

    function getSelectedCustomScanData: TStringList;
    function getSelectedCustomScanType: TCustomScanType;
    function getSelectedVariableType: TVariableType;
    procedure setfoundcount(x: int64);


    procedure AddresslistDropByListview(sender: TObject; node: TTreenode; attachmode: TNodeAttachMode);
  public
    { Public declarations }
    addresslist: TAddresslist;

    test: single;
    itemshavechanged: boolean;

    debugproc: boolean;
    autoopen: boolean;
    //boolean set when table is opened by other means than user file picker

    Priority: Dword;
    memimage: TMemorystream;

    canceled: boolean;

    originalheight: integer;
    originalwidth: integer;

    fronttext: string;


    aprilfools: boolean;
    editedsincelastsave: boolean;

    autoattachlist: TStringList;
    oldcodelistcount: integer;

    memscan: tmemscan;

    procedure ChangedHandle(Sender: TObject);
    procedure plugintype0click(Sender: TObject);
    procedure plugintype5click(Sender: TObject);
    procedure OnToolsClick(Sender: TObject);
    procedure AddToRecord(Line: integer; node: TTreenode=nil; attachmode: TNodeAttachMode=naAdd);
    procedure AddAutoAssembleScript(script: string);
    procedure reinterpretaddresses;


    procedure ClearList;

    procedure UpdateScanType;
    procedure enableGui(isnextscan: boolean);
    procedure disableGui;
    procedure SpawnCancelButton;
    procedure DestroyCancelButton;

    property foundcount: int64 read ffoundcount write setfoundcount;
    property RoundingType: TRoundingType read GetRoundingType write SetRoundingType;
    property ScanStart: ptruint read getScanStart write setScanStart;
    property ScanStop: ptruint read getScanStop write setScanStop;
    property FastScan: boolean read getFastscan write setFastscan;
    property ScanReadOnly: boolean read getScanReadonly write setScanReadonly;
    property SelectedCustomScanData: TStringList read getSelectedCustomScanData;
    property SelectedCustomScanType: TCustomScanType read getSelectedCustomScanType;
    property SelectedVariableType: TVariableType read getSelectedVariableType;
  end;

var
  MainForm: TMainForm;
  ToggleWindows: TTogglewindows;

implementation


uses mainunit2, AddAddress, ProcessWindowUnit, MemoryBrowserFormUnit, TypePopup
  , HotKeys{, standaloneunit}, aboutunit,  formScanningUnit,  formhotkeyunit, formDifferentBitSizeUnit,
  CommentsUnit, formsettingsunit, formAddressChangeUnit, Changeoffsetunit, FoundCodeUnit, advancedoptionsunit,
  frmProcessWatcherUnit,formPointerOrPointeeUnit,OpenSave, formmemoryregionsunit, formProcessInfo
  , frmautoinjectunit,PasteTableentryFRM,pointerscannerfrm,PointerscannerSettingsFrm,frmFloatingPointPanelUnit{,
  , ,
  formMemoryTrainerUnit, MemoryTrainerDesignUnit, ,
  ,,
  ,
  , , ,
  , frmGDTunit, frmFunctionlistUnit, };

var
  ncol: TColor;

procedure TFlash.Col;
begin
  mainform.panel7.Color := ncol;
end;

procedure TFlash.Execute;
var
  red, green: byte;
  decreasered: boolean;
begin

{$ifndef ceasinjecteddll}
  freeonterminate := True;
  decreasered := True;
  red := 254;
  green := 0;
  while not terminated do
  begin
    if decreasered then
    begin
      Dec(red, 2);
      Inc(green, 2);
      if green >= 250 then
        decreasered := False;
    end
    else
    begin
      Inc(red, 2);
      Dec(green, 2);
      if red >= 254 then
        decreasered := True;
    end;
    ncol := (green shl 8) + red;
    synchronize(col);
    //sigh (without it works too, but at least with this I know for sure it works.

    sleep(10);
  end;

{$endif}
  mainform.Panel7.Color := clBtnFace;
end;

constructor TToggleWindows.Create(CreateSuspended: boolean);
begin
  freeonterminate := True;
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
  MMInfo:=pointer(message.LParam);
  if pixelsperinch=96 then
  begin
    MMInfo.ptMinTrackSize:=point(490,460);
  end;
end;




procedure TMainform.Hotkey2(var Message: TMessage);
var
  i: integer;
  a, b: single;
  s: string;

  memrec: TMemoryRecord;
begin
  if message.LParam <>0 then
  begin
    memrec:=TMemoryRecord(message.LParam);
    if memrec<>nil then //just be safe (e.g other app sending message)
      memrec.DoHotkey(message.WParam);

    //param contains the hotkey number
  end
  else
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

      if formsettings.cbHideAllWindows.Checked then
      begin
        ToggleWindow;

        if formsettings.cbCenterOnPopup.Checked then
          if not allwindowsareback then
            setwindowpos(mainform.Handle, HWND_NOTOPMOST, (screen.Width div 2) -
              (mainform.Width div 2), (screen.Height div 2) - (mainform.Height div
              2), mainform.Width, mainform.Height, SWP_NOZORDER or SWP_NOACTIVATE);

        if not allwindowsareback then
          application.BringToFront
        else
          setforegroundwindow(lastforeground);

        adjustbringtofronttext;
        exit;
      end;

      application.BringToFront;
      SetForegroundWindow(mainform.Handle);

      mainform.SetFocus;

      if formsettings.cbCenterOnPopup.Checked then
        setwindowpos(mainform.Handle, HWND_NOTOPMOST, (screen.Width div 2) -
          (mainform.Width div 2), (screen.Height div 2) - (mainform.Height div
          2), mainform.Width, mainform.Height, SWP_NOZORDER or SWP_NOACTIVATE);

      formstyle := fsStayOnTop;
    end;

    1: //Pause
    begin
      with advancedoptions do
      begin
        pausedbyhotkey := True;
        pausebutton.down := not pausebutton.down;
        pausebutton.Click;
        pausedbyhotkey := False;
      end;
    end;

    2: //speedhack
    begin
      if cbSpeedhack.Enabled then
      begin
        beep;
        cbSpeedhack.Checked := not cbSpeedhack.Checked;
      end;
    end;

    //3..7=set speedhack speed
    3:
    begin
      if cbspeedhack.Enabled then
      begin
        cbspeedhack.Checked := True;
        if cbspeedhack.Checked then
        begin
          editsh2.Text:=format('%.2f', [speedhackspeed1.speed]);
          btnSetSpeedhack2.Click;
        end;
      end;
    end;

    4:
    begin
      if cbspeedhack.Enabled then
      begin
        cbspeedhack.Checked := True;
        if cbspeedhack.Checked then
        begin
          editsh2.Text:=format('%.2f', [speedhackspeed2.speed]);
          btnSetSpeedhack2.Click;
        end;
      end;
    end;

    5:
    begin
      if cbspeedhack.Enabled then
      begin
        cbspeedhack.Checked := True;
        if cbspeedhack.Checked then
        begin
          editsh2.Text:=format('%.2f', [speedhackspeed3.speed]);
          btnSetSpeedhack2.Click;
        end;
      end;
    end;

    6:
    begin
      if cbspeedhack.Enabled then
      begin
        cbspeedhack.Checked := True;
        if cbspeedhack.Checked then
        begin
          editsh2.Text:=format('%.2f', [speedhackspeed4.speed]);
          btnSetSpeedhack2.Click;
        end;
      end;
    end;

    7:
    begin
      if cbspeedhack.Enabled then
      begin
        cbspeedhack.Checked := True;
        if cbspeedhack.Checked then
        begin
          editsh2.Text:=format('%.2f', [speedhackspeed5.speed]);
          btnSetSpeedhack2.Click;
        end;
      end;
    end;

    8:
    begin
      //increase speed
      try
        if cbspeedhack.Enabled then
        begin
          cbspeedhack.Checked := True;
          if cbspeedhack.Checked then
          begin
            a := strtofloat(editsh2.Text);
            a := a + speedupdelta;
            editsh2.Text:=format('%.2f', [a]);
            btnSetSpeedhack2.Click;
          end;
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
          cbspeedhack.Checked := True;
          if cbspeedhack.Checked then
          begin
            b := strtofloat(editsh2.Text);
            b := b - slowdowndelta;
            editsh2.Text:=format('%.2f', [b]);
            btnSetSpeedhack2.Click;
          end;
        end;
      except

      end;
    end;

    10..18: //Change type (if possible)
    begin
      if vartype.Enabled then
        vartype.ItemIndex := message.WParam - 3
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
      if cancelbutton<>nil then
        cancelbutton.Click;
    end;

    29: //debug->run
    begin
      MemoryBrowser.Run1.Click;
    end;

  end;

end;

procedure TMainForm.hotkey(var Message: TMessage);
//stays because the old hotkeyhandler is still used in some places
var
  x: TDevMode;
  i: integer;
  c: integer;
begin

  if (formhotkey<>nil) and (formhotkey.visible) then exit;

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


end;
//----------------------------------

function TMainform.getSelectedCustomScanData: TStringList;
var
  index: integer;
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

function TMainform.getScanStart: ptruint;
begin
  try

    Result := StrToInt64('$' + FromAddress.Text);
  except
    raise Exception.Create('Invalid start address: ' + FromAddress.Text);
  end;
end;

procedure TMainform.setScanStart(newscanstart: ptruint);
begin
  FromAddress.Text := inttohex(newscanstart, 8);
end;

function TMainform.getScanStop: ptruint;
begin
  try
    Result := StrToInt64('$' + ToAddress.Text);
  except
    raise Exception.Create('Invalid stop address: ' + ToAddress.Text);
  end;
end;

procedure TMainform.setScanStop(newscanstop: ptruint);
begin
  ToAddress.Text := inttohex(newscanstop, 8);
end;


function TMainform.getFastscan: boolean;
begin
  Result := cbFastscan.Checked;
end;

procedure TMainform.setFastScan(state: boolean);
begin
  cbFastscan.Checked := state;
end;

function TMainform.getScanReadonly: boolean;
begin
  Result := ReadOnly.Checked;
end;

procedure TMainform.setScanReadOnly(state: boolean);
begin
  ReadOnly.Checked := state;
end;



function TMainform.GetRoundingType: TRoundingType;
  {Property function to get the current rounding type}
begin
  Result := rtTruncated;
  if rt1.Checked then
    Result := rtRounded
  else
  if rt2.Checked then
    Result := rtExtremerounded
  else
  if rt3.Checked then
    Result := rtTruncated;
end;

procedure TMainform.SetRoundingType(rt: TRoundingType);
{Property function to set the current rounding type}
begin
  case rt of
    rtRounded: rt1.Checked;
    rtExtremerounded: rt2.Checked;
    rtTruncated: rt3.Checked;
  end;
end;


procedure TMainform.setfoundcount(x: int64);
var
  xdouble: double;
begin
  ffoundcount := x;
  xdouble := x;
  //  foundcountlabel.Caption:=inttostr(x);
  foundcountlabel.Caption := Format('%.0n', [xdouble]);
end;

procedure TMainform.DestroyCancelButton;
begin
  if cancelbutton <> nil then
    FreeAndNil(cancelbutton);
  if cancelbuttonenabler <> nil then
    FreeAndNil(cancelbuttonenabler);
end;

procedure TMainform.SpawnCancelButton;
begin
  cancelbutton := TButton.Create(self);
  with cancelbutton do
  begin
    top := newscan.top;
    left := newscan.left;
    Width := (nextscanbutton.left + nextscanbutton.Width) - left;
    Height := newscan.Height;
    Caption := 'Cancel';
    onclick := cancelbuttonclick;
    Enabled := False;
    tag := 0; //0=normal 1=force

    Hint := 'This button will try to cancel the current scan. Click twice to force an exit';
    ParentShowHint := False;
    ShowHint := True;

    parent := panel5;
  end;

  cancelbuttonenabler := TTimer.Create(self);

  with cancelbuttonenabler do
  begin
    interval := 2000; //2 seconds
    OnTimer := cancelbuttonenablerinterval;
    Enabled := True;
  end;
end;


procedure TMainform.DisableGui;
{
This procedure will disable the gui. E.g while scanning the memory with no wait
screen.
}
var
  i: integer;
begin
  Groupbox1.Enabled := False;
  for i := 0 to groupbox1.ControlCount - 1 do
    groupbox1.Controls[i].Enabled := False;

  scanvalue.Enabled := False;
  if scanvalue2 <> nil then
  begin
    scanvalue2.Enabled := False;
    andlabel.Enabled := False;
    scantext2.Enabled := False;
  end;

  vartype.Enabled := False;
  scantype.Enabled := False;
  scantext.Enabled := False;
  label4.Enabled := False;
  label8.Enabled := False;
  HexadecimalCheckbox.Enabled := False;
  cbCaseSensitive.Enabled := False;

  newscan.Enabled := False;
  nextscanbutton.Enabled := False;
  undoscan.Enabled := False;
end;

procedure TMainform.EnableGui(isnextscan: boolean);
{
Enables the gui options according to what type of scan is currently used
no scan, enable everything
already scanning, disable the group and type
}
var
  i: integer;
  fname, expectedfilename: string;
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
  if scanvalue2<>nil then
  begin
    scanvalue2.Enabled:=true;
    andlabel.Enabled:=true;
    scantext2.Enabled:=true;
  end;
  newscan.Enabled:=true;

  undoscan.Enabled:=isnextscan; //nextscan was already enabled
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

resourcestring
  strWindowFailedToHide = 'A window failed to hide';

procedure TMainform.toggleWindow;
var
  c: integer;
begin
  togglewindows := TTogglewindows.Create(False);
  c := 0;
  while togglewindows <> nil do
  begin
    if c = 500 then
    begin
      togglewindows.Free;
      raise Exception.Create(strWindowFailedToHide);
    end;
    sleep(10);
    Inc(c);
  end;
end;


procedure TMainform.exceptionhandler(Sender: TObject; E: Exception);
begin
  if (E.Message = 'Error creating window device context') then
    exit;
  screen.Cursor := crdefault;
  MessageDlg(E.message, mtError, [mbOK], 0);
end;

resourcestring
  strAccessed='The following opcodes accessed the selected address';


resourcestring
  strOpcodeRead = 'The following opcodes read from the selected address';



resourcestring strOpcodeChanged='The following opcodes changed the selected address';



resourcestring strAskToSave='You haven''t saved your last changes yet. Save Now?';
function TMainForm.CheckIfSaved: boolean;
var help :word;
    i,j: Integer;
begin
  //result:=true;
  result:=not editedsincelastsave;


  if itemshavechanged then result:=false;

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

resourcestring
  strScantextcaptiontotext = 'Text:';
  strScantextcaptiontoValue = 'Value:';
  strsearchForText = 'Search for text';
  strSearchForArray = 'Search for this array';

procedure TMainForm.UpdateScanType;
var
  OldText: string;
  OldIndex: integer;
  hexvis: boolean;
  floatvis: boolean;
  oldwidth: integer;
  i: integer;

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
                  ScanType.Items.Add(strSameAsFirstScan);
                  Scantype.DropDownCount:=11;

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
      scanvalue2.TabOrder:=scanvalue.TabOrder+1;
      scanvalue2.Text:=oldscanvalue2text;

      scantext2:=tlabel.create(self);
      scantext2.caption:=scantext.caption;
      scantext2.Left:=scanvalue2.Left;
      scantext2.Top:=scantext.top;
      scantext2.Parent:=scantext.parent;
      scantext2.Anchors:=scantext.Anchors;

      andlabel:=tlabel.Create(self);
      andlabel.Caption:='and';
      andlabel.Left:=scanvalue2.Left-20;
      andlabel.Top:=scanvalue2.Top+2;
      andlabel.Parent:=scanvalue2.Parent;
      andlabel.Anchors:=scantext.Anchors;
    end
  end else
  begin
    if scanvalue2<>nil then
    begin
      scanvalue.Width:=scanvalue.width+20+scanvalue2.width;
      oldscanvalue2text:=scanvalue2.Text;
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


procedure TMainform.reinterpretaddresses;
begin
  addresslist.ReinterpretAddresses;
end;



procedure TMainform.AddAutoAssembleScript(script: string);
begin
  addresslist.addAutoAssembleScript(script);
end;

procedure TMainForm.AddToRecord(Line: integer; node: TTreenode=nil; attachmode: TNodeAttachMode=naAdd);
var error: Integer;
    Address: ptrUint;
    i,j:  Integer;
    bit: Integer;
    tmp: string;
    pc: ^char;

    found: boolean;
    bitl: integer;

    realvartype: integer;
    tempvartype: TVariableType;
    addressstring: string;
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
    case scandisplayroutinetype of
      0: realvartype:=0;
      1: realvartype:=1;
      2: realvartype:=2;
      3: realvartype:=6;
      4: realvartype:=3;
      5: realvartype:=4;
      6: realvartype:=8;
      7,8 : realvartype:=7;
      else realvartype:=2;
    end;
  end
  else
  begin
    bit:=0;
    bitl:=0;
    bit:=foundlist.GetVarLength;
  end;
  address:=foundlist.GetAddress(line);

  if foundlist.inmodule(line) then
    addressString:=foundlist.GetModuleNamePlusOffset(line)
  else
    addressstring:=inttohex(address,8);


  addresslist.addaddress(strNoDescription, addressString, [], 0, OldVarTypeToNewVarType(realvartype), bit,bitl , scandisplayroutinetype=8,node,attachmode);
end;

procedure TMainForm.SetExpectedTableName;
var
  fname: string;
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

resourcestring
  strConfirmProcessTermination =
    'This will close the current process. Are you sure you want to do this?';
  strError = 'Error';
  strErrorwhileOpeningProcess = 'Error while opening this process';
  strKeepList = 'Keep the current address list/code list?';
  strInfoAboutTable = 'Info about this table:';
  strPhysicalMemory = 'Physical Memory';

function TMainform.openprocessPrologue: boolean;
begin

  Result := False;

  if flashprocessbutton <> nil then
  begin
    flashprocessbutton.Terminate;
    flashprocessbutton := nil;
  end;

  if (debuggerthread<>nil) then
  begin
    debuggerthread.Terminate;
    debuggerthread.WaitFor;
    freeandnil(debuggerthread);
  end;

  canceled := False;
  Result := True;
end;

procedure TMainform.openProcessEpilogue(oldprocessname: string;
  oldprocess: dword; oldprocesshandle: dword);
var
  newprocessname: string;
  i, j: integer;
  fname, expectedfilename: string;
begin

  newprocessname := copy(mainform.ProcessLabel.Caption, pos(
    '-', mainform.ProcessLabel.Caption) + 1, length(mainform.ProcessLabel.Caption));

  symhandler.reinitialize;
  reinterpretaddresses;

  if oldprocess=0 then //set disassembler and hexview of membrowser to what the main header says
    memorybrowser.setcodeanddatabase;


  if (processhandle = 0) then
  begin
    if newscan.Caption = strNewScan then
      newscan.click;

    //disable everything

    foundcount := 0;
    foundlist.Clear;

    newscan.Caption := strFirstScan;

    Groupbox1.Enabled := False;
    for i := 0 to groupbox1.ControlCount - 1 do
      groupbox1.Controls[i].Enabled := False;


    scanvalue.Enabled := False;
    newscan.Enabled := False;
    nextscanbutton.Enabled := False;
    vartype.Enabled := False;
    scantype.Enabled := False;
    scantext.Enabled := False;
    label4.Enabled := False;
    label8.Enabled := False;

    scanvalue.Visible := False;
    scantext.Visible := False;
    scanvalue.Text := '';
    HexadecimalCheckbox.Enabled := False;
    cbCaseSensitive.Enabled := False;

    Updatescantype;
    Scantype.ItemIndex := 0;

    cbSpeedhack.Enabled := False;
    cbUnrandomizer.Enabled := False;



    if processid <> $FFFFFFFF then
    begin
      processlabel.Caption := strError;
      raise Exception.Create(strErrorWhileOpeningProcess);
    end
    else
    begin
      processlabel.Caption := strPhysicalMemory;
    end;

    UpdateScanType;
  end;

  if (processID = oldProcess) then
    exit;



  //a new process has been selected
  cbspeedhack.Enabled := True;
  cbunrandomizer.Enabled := True;


  if (addresslist.count > 0) or (advancedoptions.codelist2.items.Count > 0) then
  begin
    if (messagedlg(strKeepList, mtConfirmation, [mbYes, mbNo], 0) = mrNo) then
    begin

      ClearList;
    end
    else
    begin
      //yes, so keep the list
      //go through the list and chek for auto assemble entries, and check if one is enabled. If so, ask to disable (withotu actually disabling)
      for i := 0 to addresslist.count - 1 do
        if (addresslist[i].VarType = vtCustom) and (addresslist[i].active) then
        begin
          if (messagedlg(
            'There are one or more auto assembler entries enabled in this table. Do you want them disabled? (without executing the disable part)',
            mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
          begin
            for j := 0 to addresslist.count - 1 do
              if (addresslist[j].VarType = vtCustom) and (addresslist[j].active) then
                addresslist[j].disablewithoutexecute;
          end;
          break;
        end;
    end;

  end;

  enablegui(nextscanbutton.Enabled);

  Fname := copy(processlabel.Caption, pos('-', processlabel.Caption) + 1,
    length(processLabel.Caption));

  if FName[length(FName) - 3] = '.' then  //it's a filename
    expectedFilename := copy(FName, 1, length(FName) - 4) + '.ct'
  else //it's a normal title;
    expectedFilename := FName + '.ct';



  if fileexists(expectedfilename) or fileexists(cheatenginedir + expectedfilename) then
  begin
    if messagedlg('Load the associated table? (' + expectedFilename + ')', mtConfirmation,
      [mbYes, mbNo], 0) = mrYes then
    begin
      autoopen := True;
      if fileexists(expectedfilename) then
        opendialog1.FileName := expectedfilename
      else
        opendialog1.FileName := cheatenginedir + expectedfilename;

      LoadButton.Click;
    end;
  end;

  UpdateScanType;
end;

procedure TMainForm.ShowProcessListButtonClick(Sender: TObject);
var
  oldprocess: Dword;
  resu: integer;
  i, j: integer;
  oldprocesshandle: thandle;
  oldprocessname: string;
  modulelist: TStringList;
begin
  if not openprocessPrologue then
    exit;

  oldprocessname := copy(mainform.ProcessLabel.Caption, pos(
    '-', mainform.ProcessLabel.Caption) + 1, length(mainform.ProcessLabel.Caption));

  oldprocess := processID;
  oldprocesshandle := processhandle;

  if Processwindow = nil then
    ProcessWindow := TProcessWindow.Create(nil);

  resu := ProcessWindow.ShowModal;

  if resu = mrCancel then
    exit;

  openProcessEpilogue(oldprocessname, oldprocess, oldprocesshandle);
end;

procedure TMainForm.rbAllMemoryChange(Sender: TObject);
begin

end;

procedure TMainForm.Foundlist3Resize(Sender: TObject);
begin
  foundlist3.Columns[1].width:=foundlist3.ClientWidth-foundlist3.Columns[0].Width;
end;

procedure TMainForm.Description1Click(Sender: TObject);
begin
  addresslist.doDescriptionChange;
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  if length(filenames)>0 then
  begin
    if CheckIfSaved then
      LoadTable(filenames[0],false);
  end;
end;

procedure TMainForm.Address1Click(Sender: TObject);
begin
  addresslist.doAddressChange;
end;

procedure TMainForm.CreateGroupClick(Sender: TObject);
var
  groupname: string;
  i: integer;
  count: integer;
begin
  //in rare cases you can use the treeview data data if you request so
  count:=0;
  for i:=0 to addresslist.Items.count-1 do
    if TMemoryRecord(addresslist.Items[i].data).isGroupHeader then inc(count);

  groupname:='Group '+inttostr(count+1);

  if InputQuery('Groups','What do you want the groupname to be?',groupname) then
    addresslist.CreateGroup(groupname);
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
var i: integer;
begin
  addresslist.SelectAll;
end;

procedure TMainForm.miFreezeNegativeClick(Sender: TObject);
begin
  if addresslist.selectedRecord<>nil then
  begin
    addresslist.selectedRecord.allowDecrease:=true;
    addresslist.selectedRecord.active:=true;
  end;
end;

procedure TMainForm.miFreezePositiveClick(Sender: TObject);
begin
  if addresslist.selectedRecord<>nil then
  begin
    addresslist.selectedRecord.allowIncrease:=true;
    addresslist.selectedRecord.active:=true;
  end;
end;

procedure TMainForm.Panel5Resize(Sender: TObject);
begin
  speedbutton3.top:=foundlist3.top+foundlist3.height-speedbutton3.Height;
  speedbutton3.left:=foundlist3.left+foundlist3.width+2;
end;

procedure TMainform.aprilfoolsscan;
begin

  if aprilfools then
  begin
    if messagedlg(
      'Thank you for trying out Cheat Engine. Because it has expired Cheat Engine will now close. Is that ok with you?',
      mtInformation, [mbYes, mbNo], 0) = mrYes then
    begin
      ShowMessage('April fools!!!!');

    end
    else
    begin
      if messagedlg(
        'WHAT!!! Are you saying you''re going to continue using CE ILLEGALLY??? If you say yes, i''m going to mail the cops to get you and send you to jail!!!', mtWarning, [mbYes, mbNo], 0) = mrYes then
        ShowMessage(
          'Hrmpf... Because I''m in a good mood i''ll let you go this time. But don''t do it again you filthy pirate')
      else
        ShowMessage('April fools!!!!');
    end;

    Caption := cenorm;
    aprilfools := False;
  end;
end;

procedure TMainform.donewscan;
var
  i: integer;
begin
  if SaveFirstScanThread<>nil then //stop saving the results of the fist scan
  begin
    SaveFirstScanThread.Terminate;
    SaveFirstScanThread.WaitFor;
    freeandnil(SaveFirstScanThread);
  end;

  fastscan:=formsettings.cbFastscan.checked;
  //close files in case of a bug i might have missed...

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
  foundlist.deleteresults;

  if scanvalue.Visible and scanvalue.Enabled then
  begin
    scanvalue.SetFocus;
    scanvalue.SelectAll;
  end
end;

procedure TMainForm.NewScanClick(Sender: TObject);
begin
  button2.click; //now completly replaced
end;

procedure TMainForm.NextScanButtonClick(Sender: TObject);
begin
  button4.click;
end;

procedure TMainForm.btnMemoryViewClick(Sender: TObject);
begin
   memorybrowser.show;
end;

resourcestring
  strClickToGoHome = 'Click here to go to the Cheat Engine homepage';

procedure TMainForm.FormCreate(Sender: TObject);
var
  pid: dword;
  tokenhandle: thandle;
  tp: TTokenPrivileges;
  prev: TTokenPrivileges;

  ReturnLength: Dword;

  reg: tregistry;
  differentWidth: integer;
  x: array of integer;

  errormode: dword;
begin
  forms.Application.ShowButtonGlyphs:=sbgNever;
  application.OnException := exceptionhandler;
  errormode := SetErrorMode(0);
  setErrorMode(errormode or SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);


  foundlist:=tfoundlist.create(foundlist3,foundcountlabel);

  actScriptEngine.ShortCut := TextToShortCut('Ctrl+Shift+C');


  hotkeypressed := -1;

  pid := GetCurrentProcessID;

  ownprocesshandle := OpenProcess(PROCESS_ALL_ACCESS, True, pid);
  tokenhandle := 0;

  if ownprocesshandle <> 0 then
  begin
    if OpenProcessToken(ownprocesshandle, TOKEN_QUERY or
      TOKEN_ADJUST_PRIVILEGES, tokenhandle) then
    begin
      if lookupPrivilegeValue(nil, 'SeDebugPrivilege', tp.Privileges[0].Luid) then
      begin
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        tp.PrivilegeCount := 1; // One privilege to set
        if not AdjustTokenPrivileges(tokenhandle, False, tp, sizeof(tp),
          prev, returnlength) then
          ShowMessage('Failure setting the debug privilege. Debugging may be limited.');
      end;
    end;
  end;


  tempbitmap := TBitmap.Create;

  scanvalue.Text := '';
  Windows.OnClick(Windows);
  isbit := False;

  old8087CW := Get8087CW;
  Set8087CW($133f);
  SetSSECSR($1f80);



  debugproc := False;

  //get current screen resolution (when switching back for debug)
  originalwidth := screen.Width;
  originalheight := screen.Height;

  Memimage := TMemorystream.Create;

  oldwidth := screen.Width;
  oldheight := screen.Height;

  ProcessHandler.ProcessHandle := 0;

  logo.Hint := strClickToGoHome;

  logo.ShowHint := True;

  newaddress := 0;

  VarType.ItemIndex := 3;
  oldvartype := 3;

  UpdateScantype;
  ScanType.ItemIndex := 0;

  newscan.Caption := strFirstScan;
  hookedin := False;

  //allignment fixes for some window style's that mess up with thick borders (like vista)
  differentWidth := logopanel.left - (clientwidth - logopanel.Width);
  button1.Left := clientwidth - button1.Width;
  commentbutton.left := clientwidth - commentbutton.Width;
  logopanel.left := clientwidth - logopanel.Width;
  speedbutton4.Left := clientwidth - speedbutton4.Width;
  progressbar1.Width := progressbar1.Width - differentwidth;
  undoscan.left := undoscan.left - differentwidth;

  //create object for the auto attach list
  autoattachlist := TStringList.Create;
  autoattachlist.CaseSensitive := False; //set it up as not case sensitive

  randomize;

  pluginhandler := TPluginhandler.Create;


{$ifdef ceasinjectabledll}
  //panel7.Visible:=false;
  speedbutton1.Enabled := False;
  processid := getcurrentprocessid;
  processhandle := getcurrentprocess;
  enableGui;
  processlabel.Caption := Inttohex(processid, 8) + ' : ' + 'Current process';
{$endif}


  setlength(x, 7);
  if loadformposition(self, x) then
  begin

   (* headercontrol1.Sections[0].Width := x[0];
    headercontrol1.Sections[1].Width := x[1];
    headercontrol1.Sections[2].Width := x[2];
    headercontrol1.Sections[3].Width := x[3];
    headercontrol1.Sections[4].Width := x[4];   *)
    panel5.Height := x[5];
    foundlist3.columns[0].Width := x[6];
  end;

  oldhandle := mainform.handle;

  panel5.Constraints.MinHeight:=groupbox1.top+groupbox1.height+speedbutton2.height+3;
  mainform.Constraints.MinWidth:=400;
  mainform.Constraints.MinHeight:=panel5.height+150;

  addresslist:=TAddresslist.create(self);
  addresslist.width:=500;
  addresslist.height:=150;
  addresslist.top:=50;
  addresslist.parent:=panel1;
  addresslist.PopupMenu:=popupmenu2;
  addresslist.OnDropByListview:=AddresslistDropByListview;
  addresslist.Align:=alClient;


  symhandler.loadCommonModuleList;
end;

procedure TMainForm.ChangedHandle(Sender: TObject);
begin
  memscan.setScanDoneCallback(mainform.handle,wm_scandone);

  //reset the hotkeys
  hotkeyTargetWindowHandleChanged(oldhandle,mainform.handle);
  oldhandle:=mainform.handle;
end;

procedure TMainForm.AddressKeyPress(Sender: TObject; var Key: char);
begin
  hexadecimal(key);
end;

procedure TMainForm.FoundListDblClick(Sender: TObject);
begin
  if foundList3.SelCount > 0 then
    AddToRecord(FoundList3.ItemIndex);
end;

procedure TMainForm.Browsethismemoryarrea1Click(Sender: TObject);
var
  a, b: dword;
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
  addresslist.Refresh;

  updatetimer.Enabled:=false;

  inc(reinterpretcheck);
  if reinterpretcheck mod 15=0 then reinterpretaddresses;

  updatetimer.Enabled:=true;
end;

procedure TMainForm.FreezeTimerTimer(Sender: TObject);
begin
  freezetimer.enabled:=false;
  addresslist.ApplyFreeze;
  freezetimer.enabled:=true;
end;

resourcestring
  strChangeDescription1='Description';
  strChangeDescription2='Change the description to:';


//vars for changevalue
var
  differentsizesanswer: boolean;
  terminatewith0answer: boolean;
  askfordifferentsizesonce: boolean;
  asked: boolean;

resourcestring
  strNotTheSameSize1='The text you entered isn''t the same size as the original. Continue?';
  strNotTheSameSize2='Not the same size!';
  strAdd0='Do you want to add a ''0''-terminator at the end?';
  strNotAValidNotation='This is not a valid notation';
  strNotSameAmmountofBytes='The number of bytes you typed is not the same as the previous ammount. Continue?';
  strNotAValidBinaryNotation=' is not a valid binary notation!';


resourcestring strValue='Value';
               strChange1Value='Change this value to:';
               strChangeMoreValues='Change these values to:';


procedure TMainForm.Browsethismemoryregion1Click(Sender: TObject);
begin
  if addresslist.selectedrecord<>nil then
  begin
    memorybrowser.hexview.address:=addresslist.selectedrecord.GetRealAddress;
    memorybrowser.show;
  end;
end;

procedure TMainForm.Deletethisrecord1Click(Sender: TObject);
begin
  addresslist.DeleteSelected;
end;

procedure TMainForm.ScanvalueoldKeyPress(Sender: TObject; var Key: char);
var
  correct: boolean;
  becomes: string;
  i, j: integer;
begin
  checkpaste;
  correct := False;

  if key = chr(13) then
  begin
    if nextscanbutton.Enabled then
      nextscanbutton.Click
    else
      newscan.Click;

    key := #0;
    exit;
  end;
end;

resourcestring
  strSelectedAddressIsAPointer =
    'The selected address is a pointer. Are you sure? (the base pointer will get the address)';
  strMorePointers = 'There are more pointers selected. Do you want to change them as well?';
  strMorePointers2 = 'You have selected one or more pointers. Do you want to change them as well?';
  strNotAValidValue = 'This is not an valid value';

procedure TMainForm.Calculatenewvaluepart21Click(Sender: TObject);
var
  newaddress: ptrUint;
  calculate: integer;
  i, j, err: integer;
  selectedi: integer;

  firstispointer, dontdopointers: boolean;
  re: string;
  ok: boolean;

  res: integer;

  sel: TMemoryRecord;
  tempaddress: ptrUint;

begin
  if addresslist.count=0 then exit;

  res:=-1;

  //first find out how many where selected.
  i:=0;
  selectedi:=0;
  while (i<addresslist.count) and (selectedi<2) do
  begin
    if addresslist[i].isSelected then inc(selectedi);
    inc(i);
  end;


  dontdopointers:=false;
  firstispointer:=false;

  sel:=addresslist.selectedRecord;
  if sel=nil then
    sel:=addresslist[0];


  if sel.IsPointer then
    if messagedlg(strSelectedAddressIsAPointer,mtConfirmation,[mbyes,mbno],0)<>mryes then exit else firstispointer:=true;


  if selectedi>1 then
  begin
    //see if there are (other) pointers selected
    for i:=0 to addresslist.count-1 do
      if (addresslist[i]<>sel) and (addresslist[i].isselected) and (addresslist[i].IsPointer) then
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
    for i:=0 to addresslist.count-1 do
      if (addresslist[i]<>sel) and (addresslist[i].IsPointer) then
      begin
        if firstispointer then
          re:=strMorePointers
        else
          re:=strMorePointers2;

        if messagedlg(re,mtConfirmation,[mbyes,mbno],0)=mrno then dontdopointers:=true;
        break;
      end;
  end;



  newaddress:=sel.GetRealAddress;
  if (foundlist3.SelCount>0) then
    newaddress:=foundlist.GetAddress(foundlist3.ItemIndex);


  changeoffset:=TChangeOffset.create(self);

  changeoffset.FromAddress:=sel.getBaseAddress;

  changeoffset.toAddress:=NewAddress;
  if changeoffset.showmodal=mrCancel then exit;

  if changeoffset.error>0 then raise exception.Create(strNotAValidValue);
  calculate:=changeoffset.offset;

  for i:=0 to addresslist.count-1 do
  begin
    if ((selectedi>1) and addresslist[i].isSelected) or (selectedi=1) then
    begin
      if (not addresslist[i].IsPointer) then
      begin
        //check if it's a normal address or a interpretable address
        val('$'+addresslist[i].interpretableaddress,tempaddress,err);

        if err>0 then
        begin
          if res=-1 then
          begin
            res:=messagedlg('The record with description '''+addresslist[i].Description+''' has as interpretable address '''+addresslist[i].interpretableaddress+'''. The recalculation will change it to '+symhandler.getNameFromAddress(addresslist[i].getBaseAddress+calculate,true,true)+'. Do you want to edit it to the new address?',mtconfirmation,[mbyes,mbno,mbNoToAll,mbYesToAll,mbCancel],0);
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
        tempaddress:=addresslist[i].getBaseAddress;

        inc(tempaddress,calculate);
        addresslist[i].interpretableaddress:=symhandler.getNameFromAddress(tempaddress,true,true);
      end;
    end;

  end;

  addresslist.ReinterpretAddresses;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if addform = nil then
    addform := Taddform.Create(self);

  addform.showmodal;
end;

procedure TMainForm.ScanTypeChange(Sender: TObject);
begin
  updatescantype;

end;

procedure TMainForm.Value1Click(Sender: TObject);
begin
  addresslist.doValueChange;
end;

procedure TMainForm.VarTypeChange(Sender: TObject);
var
  a: int64;
  pa: ^int64;
  pb: ^dword;
  b: double;
  c: integer;
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
  temp: string;

  newvartype: integer;
  unicodevis: boolean;
  tc: tbitmap;
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
      hexstateForIntTypes:=hexadecimalcheckbox.Checked;

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
       rbdec.checked:=true;
       hexadecimalcheckbox.Checked:=false;
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
       hexadecimalcheckbox.Checked:=hexstateForIntTypes;
     end;

  5: begin //float;
       casevis:=false;

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
       hexadecimalcheckbox.checked:=cbCaseSensitive.checked;
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

  for i:=0 to panel5.ControlCount-1 do
  begin
    panel5.Controls[i].Refresh;
    panel5.Controls[i].Repaint;
    panel5.Controls[i].Invalidate;
  end;

end;

procedure TMainForm.LogoClick(Sender: TObject);
begin
  if messagedlg('Do you want to go to the Cheat Engine website?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ShellExecute(0, PChar('open'), PChar('http://www.cheatengine.org/?referedby=CE56'),
      PChar(''), PChar(''), SW_MAXIMIZE);

end;

procedure TMainForm.WindowsClick(Sender: TObject);
begin
  if Is64BitProcess(getcurrentprocess) then
  begin
    FromAddress.Text := '0000000000000000';
    ToAddress.Text := '7FFFFFFFFFFFFFFF';
  end
  else
  begin
    FromAddress.Text := '00000000';
    ToAddress.Text := '7FFFFFFF';
  end;
end;

procedure TMainForm.rbAllMemoryClick(Sender: TObject);
begin
  if Is64BitProcess(getcurrentprocess) then
  begin
    FromAddress.Text := '0000000000000000';
    ToAddress.Text := 'FFFFFFFFFFFFFFFF';
  end
  else
  begin
    FromAddress.Text := '00000000';
    ToAddress.Text := 'FFFFFFFF';
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CheckIfSaved;
end;

resourcestring
  strdeleteall = 'Are you sure you want to delete all addresses?';

procedure TMainForm.SpeedButton2Click(Sender: TObject);
begin
  addresslist.clear;
end;

resourcestring
  stralreadyin = 'This address is already in the list';
  stralreadyinlistmultiple = 'One or more addresses where already in the list';


procedure TMainForm.AddresslistDropByListview(sender: TObject; node: TTreenode; attachmode: TNodeAttachMode);
var
  i: integer;
begin
  for i := 0 to foundlist3.Items.Count - 1 do
    try
      if foundlist3.Items[i].Selected then
        AddToRecord(i,node,attachmode);
    except
    end;
end;

procedure TMainForm.SpeedButton3Click(Sender: TObject);
begin
  AddresslistDropByListview(addresslist, nil, naAdd);
end;



procedure TMainForm.Selectallitems1Click(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to foundlist3.Items.Count - 1 do
    foundlist3.items[i].Selected := True;

end;

procedure TMainForm.Label37Click(Sender: TObject);
begin
  beep;
end;

procedure TMainForm.Freezealladdresses2Click(Sender: TObject);
begin
  if addresslist.selectedRecord<>nil then
  begin
    if addresslist.selectedRecord.active then
      addresslist.DeactivateSelected
    else
      addresslist.ActivateSelected;
  end;
end;



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
procedure TMainForm.PopupMenu2Popup(Sender: TObject);
var i: Integer;
    selectedi: Integer;
    number: Integer;
    clip: TClipboard;
    inclipboard: boolean;
    temp: pchar;
    s: string;

    //6.0
    selectionCount: integer;
    selectedrecord: TMemoryRecord;
begin
  sethotkey1.Caption:=strsethotkey;

  selectedrecord:=addresslist.selectedRecord;
  selectionCount:=0;
  for i:=0 to addresslist.count-1 do
    if addresslist.MemRecItems[i].isSelected then
      inc(selectioncount);


  DeleteThisRecord1.visible:=(addresslist.selectedRecord<>nil);
  Change1.visible:=(addresslist.selectedrecord<>nil);
  address1.enabled:=(addresslist.selectedrecord<>nil) and (not addresslist.selectedRecord.isGroupHeader);
  ype1.enabled:=address1.enabled;
  Value1.enabled:=address1.enabled;
  Smarteditaddresses1.enabled:=true;

  BrowseThisMemoryRegion1.visible:=(addresslist.selectedRecord<>nil) and (not addresslist.selectedRecord.isGroupHeader);
  ShowAsHexadecimal1.visible:=(addresslist.selectedRecord<>nil) and (addresslist.selectedRecord.VarType in [vtByte, vtWord, vtDword, vtQword, vtSingle, vtDouble]) and (not addresslist.selectedRecord.isGroupHeader);
  SetHotkey1.visible:=(addresslist.selectedRecord<>nil) and (not addresslist.selectedRecord.isGroupHeader);

  Freezealladdresses2.visible:=(addresslist.selectedRecord<>nil);

  Changescript1.visible:=(addresslist.selectedRecord<>nil) and (addresslist.selectedrecord.VarType=vtCustom);

  n5.visible:=(addresslist.selectedRecord<>nil);

  Pointerscanforthisaddress1.visible:=(addresslist.selectedRecord<>nil) and (not addresslist.selectedRecord.isGroupHeader);
  Findoutwhataccessesthisaddress1.visible:=(addresslist.selectedRecord<>nil) and (not addresslist.selectedRecord.isGroupHeader);
  Setbreakpoint1.visible:=(addresslist.selectedRecord<>nil) and (not addresslist.selectedRecord.isGroupHeader);

  sep1.visible:=(addresslist.selectedRecord<>nil) and (not addresslist.selectedRecord.isGroupHeader);
  Calculatenewvaluepart21.visible:=addresslist.count>0;
  Forcerechecksymbols1.visible:=addresslist.count>0;

  n4.visible:=addresslist.count>0;

  n1.visible:=true;
  CreateGroup.visible:=true;

end;

procedure TMainForm.Unfreezealladdresses1Click(Sender: TObject);
begin

end;

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin

  Browsethismemoryregioninthedisassembler1.Enabled:=Foundlist3.SelCount>=1;

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
var
  address: Dword;
  e, i, j: integer;
  bit: byte;
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

    foundlist3.Items.BeginUpdate;
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
      foundlist3.Items.EndUpdate;
    end;
  end;

  foundlist.deinitialize;
  foundlist.initialize(getvartype);
end;



procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;
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

  cbSpeedhack.Checked:=false;

  if flashprocessbutton<>nil then
  begin
    flashprocessbutton.Terminate;
    flashprocessbutton.WaitFor;
    flashprocessbutton:=nil;
  end;

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


      if debuggerthread<>nil then
      begin
        debuggerthread.Terminate;
        debuggerthread.WaitFor;
        debuggerthread.free;
        debuggerthread:=nil;
      end;
    end;
  except
    //
  end;

  if frmProcessWatcher<>nil then
  begin
    frmProcessWatcher.Free;
    frmProcessWatcher:=nil;
  end;


  try
    tempbitmap.free;
    shutdown;

    unregisterhotkey(handle,0);
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

  if speedhack<>nil then speedhack.free;
 // EExternalException
end;


procedure TMainForm.CommentButtonClick(Sender: TObject);
begin
  comments.Show;
end;

procedure TMainForm.CommentButtonMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if comments.Memo1.Lines.Count=0 then
  begin
    Commentbutton.Hint:='No Comments';
    exit;
  end;

  Commentbutton.Hint:=copy(comments.Memo1.Text,1,20);
  if length(comments.Memo1.Text)>20 then Commentbutton.Hint:=Commentbutton.Hint+'...';
end;

procedure TMainForm.copySelectedRecords;
begin
  clipboard.astext:=addresslist.GetTableXMLAsText(true);
end;

function TMainform.paste(simplecopypaste: boolean): integer;
{
this routine will paste a entry from the cplipboard into the addresslist of CE
If simplecopypaste is false frmPasteTableentry is shown to let the user change
some stuff before adding the new entry

returns the entry number of the new addresses (first one)
}
var s: string;
begin
  s:=clipboard.AsText;
  addresslist.AddTableXMLAsText(s);
end;

procedure TMainForm.Copy1Click(Sender: TObject);
begin

  copyselectedrecords;
end;

procedure TMainForm.Cut1Click(Sender: TObject);
begin
  copyselectedrecords;
  addresslist.DeleteSelected(false);
end;

procedure TMainForm.Paste1Click(Sender: TObject);
begin
  Paste(formsettings.cbsimplecopypaste.checked);
end;

procedure TMainForm.Findoutwhataccessesthisaddress1Click(Sender: TObject);
var address: ptrUint;
    res: word;
begin
  if addresslist.selectedRecord<>nil then
  begin
    address:=addresslist.selectedRecord.GetRealAddress;

    if (not formsettings.cbKdebug.checked) then
      if (not startdebuggerifneeded) then exit;

    if addresslist.selectedRecord.IsPointer then
    begin
      with TformPointerOrPointee.create(self) do
      begin
        button1.Caption:='Find out what accesses this pointer';
        button2.Caption:='Find what accesses the address pointed at by this pointer';

        res:=showmodal;
        if res=mrno then //find what writes to the address pointer at by this pointer
          address:=addresslist.selectedRecord.GetRealAddress
        else
        if res=mryes then
          address:=symhandler.getAddressFromName(addresslist.selectedRecord.interpretableaddress)
        else
          exit;
      end;
    end;

    DebuggerThread.FindWhatAccesses(address,addresslist.selectedRecord.bytesize); //byte

  end;
end;

procedure TMainForm.Setbreakpoint1Click(Sender: TObject);
var address: ptrUint;
    res: word;
begin
  if addresslist.selectedRecord<>nil then
  begin
    address:=addresslist.selectedRecord.GetRealAddress;

    if (not formsettings.cbKdebug.checked) then
      if (not startdebuggerifneeded) then exit;

    if addresslist.selectedRecord.IsPointer then
    begin
      with TformPointerOrPointee.create(self) do
      begin
        button1.Caption:='Find out what writes this pointer';
        button2.Caption:='Find what writes the address pointed at by this pointer';

        res:=showmodal;
        if res=mrno then //find what writes to the address pointer at by this pointer
          address:=addresslist.selectedRecord.GetRealAddress
        else
        if res=mryes then
          address:=symhandler.getAddressFromName(addresslist.selectedRecord.interpretableaddress)
        else
          exit;
      end;
    end;

    DebuggerThread.FindWhatWrites(address,addresslist.selectedRecord.bytesize); //byte

  end;
end;

procedure TMainForm.TopDisablerTimer(Sender: TObject);
begin
  setwindowpos(mainform.Handle,HWND_NOTOPMOST,mainform.left,mainform.top,mainform.width,mainform.height,SWP_SHOWWINDOW);
  TopDisabler.Enabled:=false;
end;

procedure TMainForm.advancedbuttonClick(Sender: TObject);
begin
  advancedoptions.Show;
end;

procedure TMainForm.HexadecimalCheckboxClick(Sender: TObject);
var
  x: int64;
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
begin
{  HotKeyForm.recnr:=lastselected;}
  with THotKeyForm.Create(self) do
  begin
    memrec:=addresslist.selectedRecord;
    memrec.beginEdit;
    show;
  end;
end;

resourcestring
  strconfirmUndo = 'Do you really want to go back to the results of the previous scan?';

procedure TMainForm.UndoScanClick(Sender: TObject);
var
  i, j: integer;
  error: integer;
  a, b: string;

begin

  if messagedlg(strConfirmUndo,mtConfirmation  ,[mbyes,mbno],0)=mryes then
  begin
    foundlist.Deinitialize;
    undolastscan(GetVarType,hexadecimalcheckbox.checked);
    foundcount:=foundlist.Initialize(getvartype);
    undoscan.Enabled:=false;
  end;
end;

resourcestring
  strHideForeground = 'will hide the foreground window';
  strHideAll = 'will hide all windows';
  strUnHideForeground = 'will bring the foreground window back';
  strUnhideAll = 'will bring all windows back';

procedure TMainform.adjustbringtofronttext;
var
  hotkey: string;
  reg: TRegistry;

begin
  if formsettings.cbHideAllWindows.Checked then
  begin
    if allwindowsareback then
    begin
      if onlyfront then
        fronttext := strHideForeground
      else
        fronttext := strHideAll;
    end
    else
    begin
      if onlyfront then
        fronttext := strUnhideForeground
      else
        fronttext := strUnhideAll;
    end;

  end
  else
    fronttext := 'brings Cheat Engine to front';



  try
    hotkey := '';
    reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\Cheat Engine', False) then
        hotkey := reg.ReadString('BringToFrontHotkey');
    except
    end;
  finally
    reg.Free;
  end;

  if hotkey = '' then
    label7.Caption := '';
  //  fronthotkey:=hotkey;
end;

resourcestring
  strhappybirthday = 'Let''s sing Happy Birthday for Dark Byte today!';
  strXMess = 'Merry christmas and happy new year';
  strNewyear = 'And what are your good intentions for this year? ;-)';
  strfuture = 'Wow,I never imagined people would use Cheat Engine up to today';

procedure TMainForm.FormShow(Sender: TObject);
var
  reg: tregistry;
  modifier: dword;
  key: dword;
  hotkey: string;
  year, month, day: word;
  temp: string;

  i: integer;
  outputfile: textfile;
  go: boolean;
  loadt: boolean;

  firsttime: boolean;
  x: array of integer;
begin
  Set8087CW($133f);
  SetSSECSR($1f80);

  loadt := False;
  editsh2.Text := format('%.1f', [1.0]);

  reg := Tregistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if not Reg.OpenKey('\Software\Cheat Engine', False) then
    begin
      if Reg.OpenKey('\Software\Cheat Engine', True) then
      begin
        //write some default data into the register
        reg.WriteBool('Undo', True);
        reg.writeBool('Advanced', True);

        reg.WriteInteger('ScanThreadpriority',
          formsettings.combothreadpriority.ItemIndex);
      end;
    end;
  except

  end;

  if reg.ValueExists('First Time User') then
    firsttime := reg.ReadBool('First Time User')
  else
    firsttime := True;

  if firsttime then
  begin
    reg.WriteBool('First Time User', False);

    if messagedlg('Do you want to try out the tutorial?', mtConfirmation,
      [mbYes, mbNo], 0) = mrYes then
      shellexecute(0, 'open', 'Tutorial.exe', nil, nil, sw_show);
  end;


  //  animatewindow(mainform.Handle,10000,AW_CENTER);
  //mainform.repaint;
  fronttext := 'brings Cheat engine to front';

  if dontrunshow then
    exit;

  panel7.DoubleBuffered := True;
  flashprocessbutton := tflash.Create(False);



  dontrunshow := True;
  decodedate(now, year, month, day);
  if (month = 7) and (day = 1) then
    ShowMessage(strhappybirthday);
  if (month = 12) and ((day = 25) or (day = 26)) then
    ShowMessage(strXmess);
  if (month = 1) and (day = 1) then
    ShowMessage(strnewyear);
  if (month = 1) and (day = 1) and (year >= 2010) then
    ShowMessage(strFuture);

  if (month = 4) and (day = 1) then
    aprilfools := True;

  if aprilfools = True then
    Messagedlg(
      'Your license to use Cheat Engine has expired. You can buy a license to use cheat engine for 1 month for $200, 6 months for only $1000 and for 1 year for only $1800.' + ' If you don''t renew your license Cheat Engine will be severely limited in it''s abilities. (e.g: Next scan has been disabled)', mtWarning, [mbOK], 0);


  LoadSettingsFromRegistry;

  //Load the table if one was suplied
  overridedebug := False;
  if paramcount >= 1 then
    LoadTable(paramstr(1),false);

  if GetSystemType < 3 then //not a supported os for hardware breakpoints
    with formsettings do
    begin
      rdWriteExceptions.Checked := True;
      rbDebugRegisters.Enabled := False;
      label6.Enabled := False;
      label7.Enabled := False;

      rbDebugAsBreakpoint.Enabled := False;
      rbInt3AsBreakpoint.Checked := True;

    end;

  if (GetSystemType < 4) {or (is64bitos)} then  //not nt or later
  begin
    with formsettings do
    begin
      cbKernelQueryMemoryRegion.Enabled := False;
      cbKernelReadWriteProcessMemory.Enabled := False;
      cbKernelOpenProcess.Enabled := False;
      cbProcessWatcher.Enabled := False;
      cbKDebug.Enabled := False;
      cbGlobalDebug.Enabled := False;

      TauntOldOsUser.Visible := True;
    end;
  end;



  vartypechange(vartype);
  adjustbringtofronttext;


  if aprilfools then
    Caption := cenorm + ' EXPIRED!';

  if autoattachtimer.Enabled then
    autoattachcheck;




  //SMenu:=GetSystemMenu(handle,false);

  //don't put this in oncreate, just don't

  if memscan=nil then
    memscan:=tmemscan.create(progressbar1);

  memscan.setScanDoneCallback(mainform.handle,wm_scandone);

  FileAccessTest;

end;



procedure TMainForm.rbBitClick(Sender: TObject);
begin

  if not isbit then
  begin
    isbit:=true;
    //convert the value to a binary value
    try
      if scanvalue.text='' then scanvalue.text:='0' else
        scanvalue.text:=inttobin(strtoint64(scanvalue.Text));
      if scanvalue.text='' then scanvalue.text:='0';
    except
     //
    end;
  end;

end;

procedure TMainForm.rbDecClick(Sender: TObject);
begin
  if isbit then
  begin
    isbit := False;
    //convert the binary text to a decimal representation
        scanvalue.Text:=IntToStr(cefuncproc.BinToInt(scanvalue.Text));
  end;
end;

procedure TMainForm.Cut2Click(Sender: TObject);
begin
  if scanvalue.SelLength > 0 then
    scanvalue.CutToClipboard;
end;

procedure TMainForm.Copy2Click(Sender: TObject);
begin
  if scanvalue.SelLength > 0 then
    scanvalue.CopyToClipboard;
end;

procedure TMainForm.Paste2Click(Sender: TObject);
var
  cb: TClipboard;
  Text: string;
  i: integer;
  allow: boolean;
begin
  cb := tclipboard.Create;
  if cb.HasFormat(CF_TEXT) then
  begin
    if scanvalue.Focused then
      scanvalue.PasteFromClipboard;

    if (scanvalue2 <> nil) and (scanvalue2.Focused) then
      scanvalue2.PasteFromClipboard;
  end;

  cb.Free;
end;

procedure TMainform.checkpaste;
var
  cb: TClipboard;
  Text: string;
  i: integer;
  allow: boolean;
begin
  cb := tclipboard.Create;
  Paste2.Enabled := cb.HasFormat(CF_TEXT);
  cb.Free;
end;

procedure TMainForm.ccpmenuPopup(Sender: TObject);
begin
  checkpaste;
end;

procedure TMainForm.Splitter1CanResize(Sender: TObject; var NewSize: integer;
  var Accept: boolean);
begin
  if newsize < 305 then
  begin
    newsize := 305;
    accept := False;
  end;
end;

procedure TMainForm.Splitter1Moved(Sender: TObject);
begin
  panel5.Repaint;
end;

procedure TMainForm.SettingsClick(Sender: TObject);
var
  oldmodulelist:pointer;
begin

  suspendhotkeyhandler;

  oldmodulelist:=modulelist;

  if formsettings.ShowModal<>mrok then
  begin
    resumehotkeyhandler;
    LoadSettingsFromRegistry;
    exit;
  end;


  resumehotkeyhandler;


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


  if formsettings.cbKernelQueryMemoryRegion.checked then UseDBKQueryMemoryRegion else DontUseDBKQueryMemoryRegion;
  if formsettings.cbKernelReadWriteProcessMemory.checked then UseDBKReadWriteMemory else DontUseDBKReadWriteMemory;
  if formsettings.cbKernelOpenProcess.Checked then UseDBKOpenProcess else DontUseDBKOpenProcess;

  adjustbringtofronttext;
end;

procedure TMainForm.cbCaseSensitiveClick(Sender: TObject);
begin
  HexadecimalCheckbox.Checked := cbcasesensitive.Checked;
end;

procedure TMainForm.LogoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if button = mbright then
    about1.click;
end;

procedure TMainForm.btnShowRegionsClick(Sender: TObject);
begin
  formmemoryregions:=tformmemoryregions.Create(self);
  formmemoryregions.showmodal;
end;



procedure TMainForm.OpenProcesslist1Click(Sender: TObject);
begin
  speedbutton1.Click;
end;

procedure TMainForm.CloseCheatEngine1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Showashexadecimal1Click(Sender: TObject);
begin
  if addresslist.selectedRecord<>nil then
    addresslist.selectedRecord.showAsHex:=not addresslist.selectedRecord.showAsHex;
end;

procedure TMainForm.OpenMemorybrowser1Click(Sender: TObject);
begin
  btnMemoryView.click;
end;


procedure TMainForm.cbFastScanClick(Sender: TObject);
begin
end;



resourcestring
  strdontbother =
    'Don''t even bother. Cheat Engine uses the main thread to receive messages when the scan is done, freeze it and CE will crash!';
  strneeddebugger = 'To use this option the debugger must be attached to the game';

procedure TMainForm.cbPauseWhileScanningClick(Sender: TObject);

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

procedure TMainForm.ProcessLabelDblClick(Sender: TObject);
var peprocess: dword;
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

procedure TMainForm.ProcessLabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
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
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if cbunrandomizer.Checked and (button=mbright) then
  begin
    //show unrandimized addresses
    unrandomize.showaddresses;
  end;
end;

procedure TMainForm.Foundlist3CustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: boolean);
begin
  if foundlist.inmodule(item.index) then foundlist3.Canvas.Font.Color:=clgreen;
end;

resourcestring
  strUnknownExtension = 'Unknown extension';

procedure TMainForm.actOpenExecute(Sender: TObject);
var
  merge: boolean;
  app: word;
  Extension: string;

begin
  merge:=false;
  if not autoopen then
    if CheckIfSaved=false then exit;

  OpenDialog1.InitialDir:=cheatenginedir;

  if autoopen or Opendialog1.Execute then
  begin
    autoopen:=false;
    Extension:=uppercase(extractfileext(opendialog1.filename));
    if (Extension<>'.XML') and
       (Extension<>'.PTR') and
       (Extension<>'.AMT') and
       (Extension<>'.GH') and
       (Extension<>'.CET') and
       (Extension<>'.CT2') and
       (Extension<>'.CT3') and
       (Extension<>'.CT') and
       (Extension<>'.EXE') then raise exception.create(strUnknownExtension);


    if ((addresslist.count>0) or (advancedoptions.numberofcodes>0)) and (Extension<>'.EXE') then app:=messagedlg('Do you wish to merge the current table with this table?',mtConfirmation,mbYesNoCancel,0);
    case app of
      mrCancel: exit;
      mrYes: merge:=true;
      mrNo: merge:=false;
    end;

    LoadTable(Opendialog1.filename,merge);
    reinterpretaddresses;

  end;

  if advancedoptions.codelist2.items.Count>0 then
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

procedure TMainform.changeScriptCallback(memrec: TMemoryRecord; script: string; changed: boolean);
{
Gets called when a edit script is done
}
begin
  if changed then
    memrec.AutoAssemblerData.script.Text:=script;

  memrec.endEdit; //release it so the user can delete it if he/she wants to
end;

procedure TMainForm.Changescript1Click(Sender: TObject);
var x: TFrmAutoInject;
    y: array of integer;
begin
  if (addresslist.selectedRecord<>nil) and (addresslist.selectedRecord.VarType=vtCustom) then
  begin

    x:=tfrmautoinject.Create(self);
    with x do
    begin
      name:='AAEditScript';
      new1.Enabled:=false;

      editscript:=true;
      editscript2:=true;

      memrec:=addresslist.selectedRecord;
      memrec.beginEdit;
      callbackroutine:=changeScriptCallback;

      assemblescreen.text:=memrec.AutoAssemblerData.script.text;


      loadformposition(x,y);
      show;


    end;
  end;
end;

procedure TMainForm.Forcerechecksymbols1Click(Sender: TObject);
begin
  symhandler.reinitialize;
  symhandler.waitforsymbolsloaded;
  addresslist.reinterpretAddresses;
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
  if addresslist.count=0 then exit;

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
  for i:=0 to addresslist.count-1 do
  begin
    if addresslist[i].isselected then
    begin
      hasselected:=true;
      break;
    end;
  end;

  for i:=0 to addresslist.count-1 do
  begin
    if (hasselected and addresslist[i].isSelected) or (not hasselected) then
    begin
      addresslist[i].Description:=StringReplace(addresslist[i].Description,replace_find,replace_with,[rfReplaceAll,rfIgnoreCase]);

      try
        x:=symhandler.getAddressFromName(addresslist[i].interpretableaddress);
        x:=x+changeoffset;
        addresslist[i].interpretableaddress:=symhandler.getNameFromAddress(x,true,true)
      except

      end;
    end;
  end;
end;

procedure TMainForm.Smarteditaddresses1Click(Sender: TObject);
begin
  edit;
end;

procedure TMainForm.Pointerscanforthisaddress1Click(Sender: TObject);
var address: ptrUint;
    count: dword;
    j: integer;
    check: boolean;
    i: integer;
    findpointeroffsets: boolean;

    frmPointerScanner: TfrmPointerScanner;
    memrec: TMemoryRecord;
begin
  if addresslist.selectedRecord <> nil then
  begin
    memrec:=addresslist.selectedRecord;
    findpointeroffsets:=false;


    address:=memrec.GetRealAddress;

    begin
      //default
      frmPointerScanner:=tfrmpointerscanner.create(self);
      frmPointerScanner.show;

      if frmpointerscannersettings=nil then //used over and over
        frmpointerscannersettings:=tfrmpointerscannersettings.create(self);

      frmpointerscannersettings.edtAddress.text:=inttohex(address,8);

      if findpointeroffsets then
      begin
        //create and fill in the offset list

        frmpointerscannersettings.cbMustEndWithSpecificOffset.checked:=true;
        TOffsetEntry(frmpointerscannersettings.offsetlist[0]).offset:=memrec.pointeroffsets[0];

        for i:=1 to length(memrec.pointeroffsets)-1 do
        begin
          frmpointerscannersettings.btnAddOffset.Click;
          TOffsetEntry(frmpointerscannersettings.offsetlist[i]).offset:=memrec.pointeroffsets[i];
        end;
      end;

      frmPointerScanner.Method3Fastspeedandaveragememoryusage1.Click;
    end;


  end;
end;

procedure testx(arg1: pointer; arg2: pointer; arg3: pointer); stdcall;
begin


end;

procedure TMainForm.Label53Click(Sender: TObject);
begin

end;

procedure TMainform.OnToolsClick(Sender: TObject);
begin
    shellexecute(0,'open',pchar(formsettings.lvTools.Items[TMenuItem(sender).Tag].SubItems[0]),nil,nil,SW_SHOW);
end;

procedure TMainForm.plugintype5click(Sender: TObject);
var x: TPluginfunctionType5;
begin
  x:=TPluginfunctionType5(tmenuitem(sender).Tag);
  if x<>nil then
    x.callback();
end;

procedure TMainForm.plugintype0click(Sender: TObject);
var selectedrecord: PPlugin0_SelectedRecord;
var x: TPluginfunctionType0;
    interpretableaddress: string[255];
    description: string[255];
    i: integer;
    offsets: PDwordArray;

    a,b,c,d,e,f,g,h,j: dword;
begin
  if addresslist.selectedRecord=nil then exit;

  interpretableaddress:=' ';
  description:=' ';


  getmem(selectedrecord,sizeof(TPlugin0_SelectedRecord));
  //fill it with data

  interpretableaddress:=addresslist.selectedRecord.interpretableaddress;

  selectedrecord.interpretedaddress:=@interpretableaddress[1];

  selectedrecord.address:=addresslist.selectedRecord.getrealAddress;
  selectedrecord.ispointer:=addresslist.selectedRecord.IsPointer;
  selectedrecord.countoffsets:=length(addresslist.selectedRecord.pointeroffsets);

  getmem(offsets,selectedrecord.countoffsets*4); //don't forget to free
  selectedrecord.offsets:=offsets;
  for i:=0 to selectedrecord.countoffsets-1 do
    selectedrecord.offsets[i]:=addresslist.selectedRecord.pointeroffsets[i];

  description:=addresslist.selectedRecord.Description;
  selectedrecord.description:=@description[1];

  selectedrecord.valuetype:=integer(addresslist.selectedRecord.VarType);
  selectedrecord.size:=addresslist.selectedRecord.bytesize;



  x:=TPluginfunctionType0(tmenuitem(sender).Tag);
  if x<>nil then
  begin

    interpretableaddress[length(interpretableaddress)+1]:=#0;
    description[length(description)+1]:=#0;

    if x.callback(selectedrecord) then
    begin

        interpretableaddress[255]:=#0;
        description[255]:=#0;

        pbyte(@interpretableaddress[0])^:=StrLen(@interpretableaddress[1]);
        pbyte(@description[0])^:=StrLen(@description[1]);

        addresslist.selectedRecord.interpretableaddress:=interpretableaddress;

        addresslist.selectedRecord.Description:=description;
        byte(addresslist.selectedRecord.VarType):=selectedrecord.valuetype;

        //load back and free memory
        freemem(offsets); //using my own var instead the user is lame enough to mess up the pointer
        addresslist.selectedRecord.ReinterpretAddress;
    end;
    //showmessage(inttohex(dword(@x.callback),8));
  end;


  addresslist.selectedRecord.refresh;

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

procedure TMainForm.Foundlist3KeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
var i: integer;
begin
  if ((key=ord('A')) and (ssctrl in Shift) and not (ssalt in Shift)) then
  begin
    //select all
    if foundlist3.Items.Count<5000 then
    begin
      for i:=0 to foundlist3.items.count-1 do
        foundlist3.Items[i].Selected:=true;

//      foundlist3.SelectAll;
    end;
  end;
end;


procedure TMainForm.Label59Click(Sender: TObject);
var l: tstringlist;
begin
  SetProgressValue(50,100);
  SetProgressState(tbpsError);

//  l:=tstringlist.create;
//  symhandler.getModuleList(l);
//  showmessage(l.text);
end;


procedure ChangeIcon(hModule: HModule; restype: PChar; resname: PChar;
  lparam: thandle); stdcall;
var
  iconRSRC: HRSRC;
  iconG: HGLOBAL;
  resSize: integer;
  p: pointer;
begin
  iconRSRC := FindResource(hModule, resName, resType);
  resSize := SizeofResource(hModule, iconRSRC);
  iconG := LoadResource(hModule, iconRSRC);
  p := LockResource(iconG);

  //  nvoke UpdateResource,hUpdate,lpszType,lpszName,ecx,pData,nSizeOfRes
  //;invoke FreeResource,hResLoaded
  //mov eax,TRUE

  UpdateResource(lParam, restype, resName, 1030, p, resSize);

end;


procedure TMainForm.Browsethismemoryregioninthedisassembler1Click(Sender: TObject);
var
  a, b: dword;
  s: string;
begin
  if (foundlist3.ItemIndex<>-1) then
  begin
    memorybrowser.disassemblerview.SelectedAddress:=foundlist.GetAddress(foundlist3.itemindex,b,s);
    memorybrowser.show;
  end;
end;

procedure TMainform.autoattachcheck;
var pl: TStringlist;
    i,j,k: integer;
    newPID: dword;
    pli: PProcessListInfo;
begin
  //in case there is no processwatcher this timer will be used to enumare the processlist every 2 seconds
  if (not formsettings.cbAlwaysAutoAttach.checked) and ((processhandle<>0) or (processid<>0)) then
    exit;

  pl:=tstringlist.Create;
  getprocesslist(pl);

  try

    for i:=0 to autoattachlist.Count-1 do
    begin
      for j:=0 to pl.Count-1 do //can't do indexof
      begin
        if pos(uppercase(autoattachlist.Strings[i]),uppercase(pl.strings[j]))=10 then
        begin
          //the process is found

          val('$'+pl.strings[j],newPID,k);
          if processid=newPID then exit; //already attached to this one

          ProcessHandler.processid:=newPID;
          unpause;
          DetachIfPossible;

          MainForm.ProcessLabel.caption:=pl.strings[j];
          Open_Process;
          enablegui(false);

          openProcessEpilogue('',0,0);

          symhandler.reinitialize;
          reinterpretaddresses;
        end;
      end;

    end;
  //  pl.IndexOf(autoattachlist.items[i]);

  finally
    for i:=0 to pl.count-1 do
      if pl.Objects[i]<>nil then
      begin
        pli:=pointer(pl.Objects[i]);
        if pli.processIcon>0 then
          DestroyIcon(pli.processIcon);
        freemem(pli);
      end;

    pl.free;
  end;
end;

procedure TMainForm.AutoAttachTimerTimer(Sender: TObject);
begin
  autoattachcheck;
end;



procedure TMainForm.Button2Click(Sender: TObject);
var
  svalue2: string;
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
var
  i: integer;
  canceled: boolean;
begin
  i:=0;
  canceled:=false;

  button2.Tag:=2;
  button2.Caption:='Scan';
  button4.tag:=0;
  progressbar1.Position:=0;


  SetProgressState(tbpsNone);


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

procedure Tmainform.CancelbuttonClick(Sender: TObject);
begin
  if cancelbutton.tag=0 then
  begin
    cancelbutton.Caption:='Terminating scan...';
    cancelbutton.Enabled:=false;
    cancelbutton.Tag:=1; //force termination
    cancelbutton.Hint:='This button will force cancel a scan. Expect memory leaks';
    cancelbutton.ParentShowHint:=false;
    cancelbutton.ShowHint:=true;
    memscan.terminatescan(false);

    cancelbuttonenabler.Enabled:=false;
    cancelbuttonenabler.interval:=8000; //8 seconds
    cancelbuttonenabler.tag:=1;
    cancelbuttonenabler.enabled:=true;
  end
  else
  begin
    //force it. It took too long
    memscan.TerminateScan(true);
  end;
end;

procedure Tmainform.CancelbuttonenablerInterval(Sender: TObject);
begin
  if cancelbutton <> nil then
    cancelbutton.Enabled := True;

  if cancelbutton.Tag = 1 then
    cancelbutton.Caption := 'Force termination';
  TTimer(Sender).Enabled := False;
end;

procedure TMainForm.Button4Click(Sender: TObject);
var
  svalue2: string;
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
var
  vtype: integer;
  i: integer;
  bytes: tbytes;
begin
  vtype:=getvartype;
  if not canceled then
  begin
    case vtype of
      5: i:=memscan.getbinarysize;
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

      10: i:=memscan.Getcustomvariablesize;
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
resourcestring
  strCustomScanConfig = 'Custom scan config';

procedure TMainform.EditCustomScanButtonClick(Sender: TObject);
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

procedure TMainform.CreateCustomScanButtonClick(Sender: TObject);
{
Open a autoassembler window and fill it in.
Give hints that people can use loadlibratry and functions from dll's
}

var
  scriptname: string;
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
        add('scandisplayroutinetype:');
        add('/*');
        add('displayroutinetype is a ''special'' globally registered symbol (No need to alloc)');
        add('The byte at this address specifies how the values are shown');
        add('0=1 byte notation');
        add('1=2 byte notation');
        add('2=4 byte notation');
        add('3=8 byte notation');
        add('4=float notation');
        add('5=double notation');
        add('6=array of bytes');
        add('7=string ascii');
        add('8=string unicode');
        add('ff=use ''scandisplayroutine:'' to convert the data to a string');
        add('*/');
        add('db 2 //2=4 byte notation');
        add('');
        add('scandisplayroutine:');
        add('/*');
        add('displayroutine is a ''special'' globally registered symbol (No need to alloc)');
        add('if ''scandisplayroutinetype:'' is set to 255 then this routine will be called to');
        add('convert the value at the address specified to a ascii-string');
        add('eax=pointer to bytes at the address');
        add('edx=pointer to destination string (max 50 chars)');
        add('');
        add('note: scandisplayroutine is only 16KB big');
        add('*/');
        add('mov [edx],"N" //NYI, not yet implemented');
        add('mov [edx+1],"Y"');
        add('mov [edx+2],"I"');
        add('mov [edx+3],0');
        add('ret');

        add('');
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

procedure TMainForm.ScanTypeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  i: integer;
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

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  saveformposition(self,[
                        0, //headercontrol1.Sections[0].Width,
                        0, //headercontrol1.Sections[1].Width,
                        0, //headercontrol1.Sections[2].Width,
                        0, //headercontrol1.Sections[3].Width,
                        0, //headercontrol1.Sections[4].Width,
                        panel5.height,
                        foundlist3.columns[0].width
                        ]);

  if addresslist<>nil then
    freeandnil(addresslist);
end;

procedure TMainForm.tbSpeedChange(Sender: TObject);
var
  x: integer;
  y: single;
begin
  x := tbSpeed.position;
  case x of
    0: y := 0;
    1: y := 0.5;
    2: y := 1;
    3: y := 2;
    4: y := 5;
    5: y := 10;
    6: y := 20;
    7: y := 50;
    8: y := 100;
    9: y := 200;
    10: y := 500;
    else
      y := 1;
  end;
  editSH2.Text := format('%.1f', [y]);
end;

procedure TMainForm.btnSetSpeedhack2Click(Sender: TObject);
var
  newspeed: single;
  fs: Tformatsettings;
  error: boolean;
begin
  error:=false;
  try
    newspeed:=StrToFloat(editsh2.Text);
  except
    fs:=DefaultFormatSettings;
    try
      newspeed:=StrToFloat(editsh2.Text, fs);
    except
      error:=true;
    end;
  end;

  if error or IsInfinite(newspeed) or IsNan(newspeed) then
    raise exception.Create(editSH2.text+' is not a valid speed');

  if speedHack<>nil then
    speedhack.setSpeed(newspeed);
end;

procedure TMainForm.cbSpeedhackClick(Sender: TObject);
begin
  if cbSpeedhack.Checked then
  begin
    try
      if speedhack<>nil then
        freeandnil(speedhack);

      speedhack:=TSpeedhack.create;
    except
      on e: exception do
      begin
        cbSpeedhack.checked:=false;
        raise exception.create(e.Message);
      end;
    end;
  end
  else
  begin
    if speedhack<>nil then
      freeandnil(speedhack);
  end;

  panel14.Visible:=cbSpeedhack.Checked;
end;

{--------Processlist menuitem--------}
var
  il: TImageList;

procedure TMainForm.Process1Click(Sender: TObject);

var sl: tstringlist;
    mi: array of TMenuItem;
    currentmi: TMenuItemExtra;
    i,j: integer;

    tempicon: graphics.TIcon;

begin

  //fill with processlist
  if il=nil then
    il:=TImageList.Create(self);


  il.Clear;

  Menu.Images:=il;

  sl:=tstringlist.Create;

  try
    GetProcessList(sl);
    for i:=process1.Count-1 downto 3 do
      process1.Items[i].Free;

    setlength(mi,sl.count);
    for i:=0 to sl.count-1 do
    begin
      j:=sl.count-1-i;
      currentmi:=TMenuItemExtra.Create(self);
      currentmi.Caption:=sl[i];
      currentmi.Default:=dword(sl.Objects[i])=ProcessID;
      currentmi.data:=pointer(PProcessListInfo(sl.Objects[i])^.processid);
      currentmi.OnClick:=ProcessItemClick;

      if PProcessListInfo(sl.Objects[i])^.processIcon>0 then
      begin
        tempicon:=graphics.TIcon.Create;
        tempicon.handle:=PProcessListInfo(sl.Objects[i])^.processIcon;
        il.AddIcon(tempicon);
        tempicon.Free;

        currentmi.ImageIndex:=il.Count-1;
      end else
        currentmi.ImageIndex:=-1;

      mi[j]:=currentmi;
      process1.Add(currentmi);
    end;


  finally
    for i:=0 to sl.Count-1 do
      if sl.Objects[i]<>nil then
         freemem(pointer(sl.Objects[i]));
    sl.free;
  end;
end;

procedure TMainForm.ProcessItemClick(Sender: TObject);
var
  pid: dword;
  oldprocess: Dword;
  oldprocesshandle: thandle;
  oldprocessname: string;
begin

  if openprocessPrologue then
  begin
    oldprocessname:=copy(mainform.ProcessLabel.Caption,pos('-',mainform.ProcessLabel.Caption)+1,length(mainform.ProcessLabel.Caption));
    oldprocess:=processID;
    oldprocesshandle:=processhandle;
    if (sender is TMenuItemExtra) then
    begin
      pid:=dword(TMenuItemExtra(sender).data);

      unpause;
      DetachIfPossible;

      with TProcessWindow.Create(self) do
      begin
        pwop(inttohex(pid,8));
        ProcessLabel.caption:=TMenuItemExtra(sender).Caption;
        free;
      end;

      openprocessepilogue(oldprocessname,oldprocess,oldprocesshandle);

    end;
  end;
end;


{^^^^^^^^Processlist menuitem^^^^^^^^}
procedure TMainForm.About1Click(Sender: TObject);
begin
  About := TAbout.Create(self);
  About.showmodal;
end;

procedure TMainForm.CreateProcess1Click(Sender: TObject);
var
  x: dword;
  oldprocess: Dword;
  oldprocesshandle: thandle;
  oldprocessname: string;
begin
  if openprocessPrologue then
  begin

    oldprocessname := copy(mainform.ProcessLabel.Caption, pos(
      '-', mainform.ProcessLabel.Caption) + 1, length(mainform.ProcessLabel.Caption));
    oldprocess := processID;
    oldprocesshandle := processhandle;
    with TProcessWindow.Create(self) do
    begin
      btnCreateThread.Click;
      Free;
    end;

    if processid <> oldprocess then
      openprocessepilogue(oldprocessname, oldprocess, oldprocesshandle);

  end;
end;

procedure TMainForm.Helpindex1Click(Sender: TObject);
begin
  Application.HelpContext(1);
end;

procedure TMainForm.New1Click(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to erase the data in the current table?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    clearlist;
end;

procedure TMainForm.ClearList;
{
Will remove all entries from the cheattable, comments, and advanced options window
}
begin
  Comments.Memo1.Clear;
  comments.Memo1.Lines.Add(strInfoAboutTable);
  advancedoptions.codelist2.items.Clear;
  advancedoptions.numberofcodes := 0;

  addresslist.clear;
end;

procedure TMainForm.actScriptEngineExecute(Sender: TObject);
begin
  with TfrmCScript.create(self) do
    show;
end;

procedure TMainForm.File1Click(Sender: TObject);
begin
  menu.Images := imagelist1;
end;

procedure TMainForm.Label61Click(Sender: TObject);
begin

end;

procedure TMainForm.actOpenProcesslistExecute(Sender: TObject);
begin
  speedbutton1.Click;
end;

procedure TMainForm.ype1Click(Sender: TObject);
begin
  addresslist.doTypeChange;
end;

initialization
  {$i MainUnit.lrs}

end.

