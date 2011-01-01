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
  foundlisthelper, disassembler,peinfounit, PEInfoFunctions,
  simpleaobscanner, pointervaluelist, ManualModuleLoader, debughelper,
  frmRegistersunit,ctypes, addresslist,addresslisthandlerunit, memoryrecordunit,
  windows7taskbar,tablist,DebuggerInterface,vehdebugger, tableconverter,
  customtypehandler, lua,luahandler, lauxlib, lualib, frmSelectionlistunit,   htmlhelp, win32int;

//the following are just for compatibility



const
  copypasteversion = 4;

const
  wm_freedebugger = WM_USER + 1;

const
  wm_scandone = WM_USER + 2;
  wm_pluginsync = WM_USER + 3;

//scantabs
type
  TScanState=record
    alignsizechangedbyuser: boolean;
    compareToSavedScan: boolean;
    currentlySelectedSavedResultname: string; //I love long variable names

    lblcompareToSavedScan:record
      caption: string;
      visible: boolean;
      left: integer;
    end;


    FromAddress: record
      text: string;
    end;

    ToAddress: record
      text: string;
    end;

    ReadOnly: record
      checked: boolean;
    end;

    cbfastscan: record
      checked: boolean;
    end;

    edtAlignment: record
      text: string;
    end;


    cbpercentage: record
      exists: boolean;
      checked: boolean;
    end;


    floatpanel: record
      visible: boolean;
      rounded: boolean;
      roundedextreme: boolean;
      truncated: boolean;
    end;

    rbbit: record
      visible: boolean;
      enabled: boolean;
      checked: boolean;
    end;

    rbdec: record
      visible: boolean;
      enabled: boolean;
      checked: boolean;
    end;

    hexadecimalcheckbox: record
      visible: boolean;
      checked: boolean;
    end;

    gbScanOptionsEnabled: boolean;

    scantype: record
      options: string;
      itemindex: integer;
      enabled: boolean;
      dropdowncount: integer;
    end;

    vartype: record
      options: string;
      itemindex: integer;
      enabled: boolean;
    end;


    memscan: TMemscan;
    foundlist: TFoundList;

    scanvalue: record
      visible: boolean;
      text: string;
    end;

    scanvalue2: record
      exists: boolean;
      text: string;
    end;

    firstscanstate: record
      caption: string;
      enabled: boolean;
    end;

    nextscanstate: record
      enabled: boolean;
    end;

    button2: record
      tag: integer;
    end;

    foundlist3: record
      itemindex: integer;
    end;

  end;
  PScanState=^TScanState;


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
    ColorDialog1: TColorDialog;
    CreateGroup: TMenuItem;
    edtAlignment: TEdit;
    FromAddress: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    lblcompareToSavedScan: TLabel;
    Label53: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miSaveScanresults: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    miShowAsBinary: TMenuItem;
    miZeroTerminate: TMenuItem;
    miResetRange: TMenuItem;
    miChangeColor: TMenuItem;
    miGroupconfig: TMenuItem;
    miDefineNewCustomTypeLua: TMenuItem;
    miDeleteCustomType: TMenuItem;
    miHideChildren: TMenuItem;
    miBindActivation: TMenuItem;
    miRecursiveSetValue: TMenuItem;
    miDefineNewCustomType: TMenuItem;
    miEditCustomType: TMenuItem;
    miRenameTab: TMenuItem;
    miTablistSeperator: TMenuItem;
    miCloseTab: TMenuItem;
    miAddTab: TMenuItem;
    miFreezePositive: TMenuItem;
    miFreezeNegative: TMenuItem;
    Panel1: TPanel;
    pmTablist: TPopupMenu;
    pmValueType: TPopupMenu;
    pmResetRange: TPopupMenu;
    rbFsmAligned: TRadioButton;
    rbfsmLastDigts: TRadioButton;
    SettingsButton: TSpeedButton;
    ToAddress: TMemo;
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
    gbScanOptions: TGroupBox;
    ReadOnly: TCheckBox;
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
    cbCaseSensitive: TCheckBox;
    cbFastScan: TCheckBox;
    Foundlist3: TListView;
    Findoutwhataccessesthisaddress1: TMenuItem;
    Showashexadecimal1: TMenuItem;
    Panel7: TPanel;
    SpeedButton1: TSpeedButton;
    cbPauseWhileScanning: TCheckBox;
    Change1: TMenuItem;
    Description1: TMenuItem;
    Address1: TMenuItem;
    ype1: TMenuItem;
    Value1: TMenuItem;
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
    Pointerscanforthisaddress1: TMenuItem;
    Label55: TLabel;
    Label57: TLabel;
    Plugins1: TMenuItem;
    Label59: TLabel;
    UpdateFoundlisttimer: TTimer;
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
    procedure cbFastScanChange(Sender: TObject);
    procedure Description1Click(Sender: TObject);
    procedure edtAlignmentKeyPress(Sender: TObject; var Key: char);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure Foundlist3Resize(Sender: TObject);
    procedure CreateGroupClick(Sender: TObject);
    procedure Foundlist3SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lblcompareToSavedScanClick(Sender: TObject);
    procedure Label58Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure miResetRangeClick(Sender: TObject);
    procedure miChangeColorClick(Sender: TObject);
    procedure miDefineNewCustomTypeLuaClick(Sender: TObject);
    procedure miDeleteCustomTypeClick(Sender: TObject);
    procedure miBindActivationClick(Sender: TObject);
    procedure miEditCustomTypeClick(Sender: TObject);
    procedure miHideChildrenClick(Sender: TObject);
    procedure miDefineNewCustomTypeClick(Sender: TObject);
    procedure miRecursiveSetValueClick(Sender: TObject);
    procedure miRenameTabClick(Sender: TObject);
    procedure miAddTabClick(Sender: TObject);
    procedure miCloseTabClick(Sender: TObject);
    procedure miFreezeNegativeClick(Sender: TObject);
    procedure miFreezePositiveClick(Sender: TObject);
    procedure miSaveScanresultsClick(Sender: TObject);
    procedure miShowAsBinaryClick(Sender: TObject);
    procedure miZeroTerminateClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel5Resize(Sender: TObject);
    procedure pmTablistPopup(Sender: TObject);
    procedure pmValueTypePopup(Sender: TObject);
    procedure rbAllMemoryChange(Sender: TObject);
    procedure rbFsmAlignedChange(Sender: TObject);
    procedure ScanTypeSelect(Sender: TObject);
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
    tabcounter: integer; //variable that only goes up, doesn't go down when a tab is deleted
    scantablist: TTablist;

    oldscanvalue2text: string;
    aaa: single;
    hotkeypressed: integer;

    cancelbutton: TButton;
    //cancel button that spawns during a scan, disabled initially to prevent doubleclick accidents
    cancelbuttonenabler: TTimer;
    //timer that will enable the cancelbutton after 3 seconds

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
    compareToSavedScan: boolean;
    currentlySelectedSavedResultname: string; //I love long variable names

    alignsizechangedbyuser: boolean;
    scantypechangedbyhotkey: boolean;

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
    procedure PluginSync(var m: TMessage); message wm_pluginsync;
    procedure Edit;
    procedure paste(simplecopypaste: boolean);
    procedure CopySelectedRecords;


    procedure exceptionhandler(Sender: TObject; E: Exception);
    procedure toggleWindow;
    procedure adjustbringtofronttext;

    procedure scanEpilogue(canceled: boolean);
    procedure CancelbuttonClick(Sender: TObject);
    procedure CancelbuttonenablerInterval(Sender: TObject);

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

    function getSelectedVariableType: TVariableType;
    procedure setfoundcount(x: int64);


    procedure AddresslistDropByListview(sender: TObject; node: TTreenode; attachmode: TNodeAttachMode);

    procedure SaveCurrentState(scanstate: PScanState);
    procedure SetupInitialScanTabState(scanstate: PScanState; IsFirstEntry: boolean);
    procedure ScanTabListTabChange(sender: TObject; oldselection: integer);

    //custom type:
    procedure CreateCustomType(customtype: TCustomtype; script:string; changed: boolean; lua: boolean=false);

    procedure RefreshCustomTypes;
    procedure LoadCustomTypesFromRegistry;

    procedure setGbScanOptionsEnabled(state: boolean);


    function onhelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
    procedure SaveIntialTablesDir(dir: string);
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
    extraautoattachlist: TStringList; //modifed by plugins and scripts, not affected by settings changes
    oldcodelistcount: integer;

    memscan: tmemscan;

    function openprocessPrologue: boolean;
    procedure openProcessEpilogue(oldprocessname: string; oldprocess: dword; oldprocesshandle: dword;autoattachopen: boolean=false);


    procedure ChangedHandle(Sender: TObject);
    procedure plugintype0click(Sender: TObject);
    procedure plugintype5click(Sender: TObject);
    procedure OnToolsClick(Sender: TObject);
    procedure AddToRecord(Line: integer; node: TTreenode=nil; attachmode: TNodeAttachMode=naAdd);
    procedure AddAutoAssembleScript(script: string);
    procedure reinterpretaddresses;


    procedure ClearList;

    procedure CreateScanValue2;
    procedure DestroyScanValue2;

    procedure cbPercentageOnChange(sender: TObject);
    procedure CreateCbPercentage;
    procedure DestroyCbPercentage;

    procedure UpdateScanType;
    procedure enableGui(isnextscan: boolean);
    procedure disableGui;
    procedure SpawnCancelButton;
    procedure DestroyCancelButton;

    procedure AddressListAutoAssemblerEdit(sender: TObject; memrec: TMemoryRecord);

    property foundcount: int64 read ffoundcount write setfoundcount;
    property RoundingType: TRoundingType read GetRoundingType write SetRoundingType;
    property ScanStart: ptruint read getScanStart write setScanStart;
    property ScanStop: ptruint read getScanStop write setScanStop;
    property FastScan: boolean read getFastscan write setFastscan;
    property ScanReadOnly: boolean read getScanReadonly write setScanReadonly;

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
  , frmautoinjectunit,PasteTableentryFRM,pointerscannerfrm,PointerscannerSettingsFrm,frmFloatingPointPanelUnit,
  pluginexports;

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

    if not terminated then
      synchronize(col);

    sleep(10);
  end;

{$endif}
  ncol:=clBtnFace;
  synchronize(col);
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

    27: //next scan same as first
    begin
      if not newscan.Enabled then exit;
      if (formscanning<>nil) and (formscanning.Visible) then exit; //it's scanning

      if nextscanbutton.Enabled then
      begin
        scantypechangedbyhotkey:=true;
        scantype.ItemIndex:=scantype.Items.Count-1;
        scantype.OnChange(scantype);
        scantypechangedbyhotkey:=false;
      end
      else Errorbeep;
    end;

    28: //undo lastscan
    begin

      if not newscan.Enabled then exit;
      if (formscanning<>nil) and (formscanning.Visible) then exit; //it's scanning

      if undoscan.Enabled then
        undoscan.Click
      else
        Errorbeep;
    end;

    29: //cancel current scan
    begin
      if cancelbutton<>nil then
        cancelbutton.Click;
    end;

    30: //debug->run
    begin
      MemoryBrowser.Run1.Click;
    end;



  end;

end;

procedure TMainForm.hotkey(var Message: TMessage);
//stays because the old hotkeyhandler is still used in some places
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


procedure TMainform.PluginSync(var m: TMessage);
var func: TPluginFunc;
  params: pointer;
begin
  func:=pointer(m.wparam);
  params:=pointer(m.lparam);


  m.result:=ptruint(func(params));
end;

//----------------------------------

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
  Result := cbFastscan.enabled and cbFastscan.Checked;
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
begin
  setGbScanOptionsEnabled(false);

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
  scanstarted: boolean;
begin

  scanstarted:=newscan.caption=strnewscan;

  if not scanstarted then
  begin
    setGbScanOptionsEnabled(true);
    cbFastScanClick(cbfastscan);
  end;

  scanvalue.Enabled:=true;
  if scanvalue2<>nil then
  begin
    scanvalue2.Enabled:=true;
    andlabel.Enabled:=true;
    scantext2.Enabled:=true;
  end;
  newscan.Enabled:=true;

  undoscan.Enabled:=isnextscan and memscan.canUndo; //nextscan was already enabled
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


  //--------------------------cbpercentage--------------
procedure TMainForm.cbPercentageOnChange(sender: TObject);
begin
  if cbpercentage.Checked then
  begin
    //turn this into a double value scan like "value between"
    CreateScanValue2;
    ScanText.caption:='Value %';
    ScanText2.caption:='Value %';
  end
  else
  begin
    if ScanType.text<>strValueBetween then
    begin
      //single value scan
      ScanText.caption:=strScantextcaptiontoValue;
      DestroyScanValue2;
    end
    else
    begin
      ScanText.caption:=strScantextcaptiontoValue;
      ScanText2.caption:=strScantextcaptiontoValue;
    end;
  end;
end;

procedure TMainForm.CreateCbPercentage;
begin
  if cbpercentage=nil then
  begin
    cbpercentage:=tcheckbox.create(self);
    cbpercentage.AutoSize:=true;
    cbpercentage.Left:=scantype.Left+scantype.Width+5;
    cbpercentage.Top:=scantype.Top+2;

    cbpercentage.Parent:=scantype.Parent;
    cbpercentage.OnChange:=cbPercentageOnChange;
  end;

  if ScanType.text=strValueBetween then
    cbpercentage.Caption:='between %'
  else
    cbpercentage.Caption:='at least xx%';

end;

procedure TMainForm.DestroyCbPercentage;
begin
  if cbpercentage<>nil then
  begin
    cbpercentage.Checked:=false;
    freeandnil(cbpercentage);
  end;
end;
//------------------

procedure TMainForm.CreateScanValue2;
var oldwidth: integer;
begin
  if scanvalue2=nil then
  begin
    //decrease the width of the scanvalue editbox
    andlabel:=tlabel.Create(self);
    andlabel.Parent:=scanvalue.Parent;
    andlabel.Caption:='and';

    oldwidth:=scanvalue.width;
    scanvalue.Width:=(scanvalue.Width div 2)-(andlabel.width div 2)-3;


    andlabel.Left:=scanvalue.Left+scanvalue.width+3;
    andlabel.Top:=scanvalue.Top+(scanvalue.height div 2)-(andlabel.height div 2);

    andlabel.Anchors:=scantext.Anchors;



    //create a 2nd editbox
    scanvalue2:=tedit.create(self);
    //scanvalue2.onkeydown:=scanvalueKeyDown;
    scanvalue2.OnKeyPress:=ScanvalueoldKeyPress;
    scanvalue2.PopupMenu:=ccpmenu;
    scanvalue2.Left:=andlabel.left+andlabel.width+3;
    scanvalue2.Width:=oldwidth-(scanvalue2.left-scanvalue.left);
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



  end;
end;

procedure TMainForm.DestroyScanValue2;
begin
  if scanvalue2<>nil then
  begin
    scanvalue.Width:=(scanvalue2.left+scanvalue2.width)-scanvalue.left;
    oldscanvalue2text:=scanvalue2.Text;
    freeandnil(scanvalue2);
    freeandnil(scantext2);
    freeandnil(andlabel);
  end;
end;

procedure TMainForm.UpdateScanType;
var
  OldText: string;
  OldIndex: integer;
  hexvis: boolean;
  floatvis: boolean;
  t: tstringlist;
begin


  OldIndex:=Scantype.itemindex;
  OldText:=Scantype.text;
  hexvis:=true;
  floatvis:=false;

  ScanType.Items.Clear;

  ScanText.Caption:=strScantextcaptiontoValue;

  if (varType.ItemIndex in [1,2,3,4,5,6,9]) or (vartype.itemindex>=10) then  //byte-word-dword--8bytes-float-double-all   - custom
  begin

    if vartype.itemindex in [5,6,9] then //float/all
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

      if compareToSavedScan then
        ScanType.Items.Add(strCompareToLastScan)
      else
      begin
        t:=tstringlist.create;
        if memscan.getsavedresults(t)>1 then
          ScanType.Items.Add(strcompareToSavedScan)
        else
          ScanType.Items.Add(strCompareToFirstScan);

        t.free;



      end;

      Scantype.DropDownCount:=11;

    end else
    begin
      ScanType.Items.Add(strUnknownInitialValue);
      ScanType.DropDownCount:=5;
    end;

  end
  else
  case varType.ItemIndex of
    0   :     begin
                ScanType.Items.Add(strExact);
                ScanType.DropDownCount:=1;
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

  if (scantype.text=strIncreasedValueBy) or (scantype.text=strDecreasedValueBy) or (scantype.text=strValueBetween) then
  begin
    if NextScanButton.enabled then
      createCbPercentage;

  end
  else
  begin
    destroyCbPercentage;


  end;

  if scantype.Text=strValueBetween then
    CreateScanValue2
  else
    DestroyScanValue2;


  if (scantype.Text=strIncreasedValue) or
     (scantype.text=strDecreasedValue) or
     (scantype.Text=strChangedValue) or
     (scantype.Text=strUnchangedValue) or
     (scantype.Text=strUnknownInitialValue)
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

  if rbBit.visible then
    HexadecimalCheckbox.visible:=false;

  //save the last scantype (if it wasn't the option to change between first/last)
  if (scantype.ItemIndex<>-1) and (scantype.ItemIndex<scantype.Items.Count) then
  begin
    if not ((scantype.items[scantype.ItemIndex]=strcompareToSavedScan) or (scantype.items[scantype.ItemIndex]=strCompareToLastScan)) then
      lastscantype:=scantype.ItemIndex;
  end;
end;


procedure TMainform.reinterpretaddresses;
begin
  if addresslist<>nil then
    addresslist.ReinterpretAddresses;
end;



procedure TMainform.AddAutoAssembleScript(script: string);
begin
  addresslist.addAutoAssembleScript(script);
end;

procedure TMainForm.AddToRecord(Line: integer; node: TTreenode=nil; attachmode: TNodeAttachMode=naAdd);
var Address: ptrUint;
    startbit: Integer;
    l: integer;

    realvartype: integer;
    tempvartype: TVariableType;
    addressstring: string;

    ct: TCustomType;
    customname: string;
    m: tmemoryrecord;
begin

  //first check if this address is already in the list!
  customname:='';

  realvartype:=getvartype;
  if realvartype=5 then //binary
  begin
    startbit:=foundlist.getstartbit(line);
    l:=memscan.Getbinarysize;
  end
  else
  if realvartype=9 then //all
  begin
    l:=0;
    startbit:=0;
    tempvartype:=TVariableType(foundlist.getstartbit(line));
    case tempvartype of
      vtByte: realvartype:=0;
      vtWord: realvartype:=1;
      vtDWord: realvartype:=2;
      vtQWord: realvartype:=6;
      vtSingle: realvartype:=3;
      vtDouble: realvartype:=4;
    end;

  end
  else
  if realvartype=10 then //custom
  begin
    ct:=TCustomType(vartype.items.objects[vartype.itemindex]);
    customname:=ct.name;
  end
  else
  begin
    startbit:=0;
    l:=foundlist.GetVarLength;
  end;
  address:=foundlist.GetAddress(line);

  if foundlist.inmodule(line) then
    addressString:=foundlist.GetModuleNamePlusOffset(line)
  else
    addressstring:=inttohex(address,8);


  m:=addresslist.addaddress(strNoDescription, addressString, [], 0, OldVarTypeToNewVarType(realvartype), customname, l,startbit , false,node,attachmode);
  if m.VarType=vtBinary then
    m.Extra.bitData.showasbinary:=rbBit.checked;
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
    freeandnil(flashprocessbutton);
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

procedure TMainform.openProcessEpilogue(oldprocessname: string; oldprocess: dword; oldprocesshandle: dword;autoattachopen: boolean=false);
var
  i, j: integer;
  fname, expectedfilename: string;

  wasActive: boolean; //set to true if the table had AA scripts enabled or the code list had nopped instruction
begin
  outputdebugstring('openProcessEpilogue called');
  symhandler.reinitialize;
  symhandler.waitforsymbolsloaded;
  reinterpretaddresses;

  if oldprocess=0 then //set disassembler and hexview of membrowser to what the main header says
    memorybrowser.setcodeanddatabase;

  outputdebugstring('After setcodeanddatabase');


  if (processhandle = 0) then
  begin
    outputdebugstring('processhandle is 0, so disabling gui');

    if newscan.Caption = strNewScan then
      newscan.click;

    //disable everything

    foundcount := 0;
    foundlist.Clear;

    newscan.Caption := strFirstScan;

    setGbScanOptionsEnabled(false);


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


    //apply this for all tabs
    if scantablist<>nil then
      for i:=0 to scantablist.Count-1 do
        SaveCurrentState(PScanState(scantablist.TabData[i]));

  end;

  if (processID = oldProcess) then
    exit;

  outputdebugstring('oldprocessid != processid');

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
      wasActive:=false;
      for i := 0 to addresslist.count - 1 do
        if (addresslist[i].VarType = vtAutoAssembler) and (addresslist[i].active) then
        begin
          wasActive:=true;
          break;
        end;

      if not wasActive then
      begin
        for i:=0 to length(AdvancedOptions.code)-1 do
          if AdvancedOptions.code[i].changed then
          begin
            wasActive:=true;
            break;
          end;
      end;

      if wasactive then
      begin
        if (messagedlg(
          'There are one or more auto assembler entries or code changes enabled in this table. Do you want them disabled? (without executing the disable part)',
          mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
        begin
          for j := 0 to addresslist.count - 1 do
            if (addresslist[j].VarType = vtAutoAssembler) and (addresslist[j].active) then
              addresslist[j].disablewithoutexecute;

          for i:=0 to length(AdvancedOptions.code)-1 do
            AdvancedOptions.code[i].changed:=false;
        end;

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


  if not autoattachopen then
  begin
    if fileexists(TablesDir+'\'+expectedfilename) or fileexists(expectedfilename) or fileexists(cheatenginedir + expectedfilename) then
    begin
      if messagedlg('Load the associated table? (' + expectedFilename + ')', mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then
      begin
        autoopen := True;
        if fileexists(TablesDir+'\'+expectedfilename) then
          opendialog1.FileName:= TablesDir+'\'+expectedfilename
        else
        if fileexists(expectedfilename) then
          opendialog1.FileName := expectedfilename
        else
          opendialog1.FileName := cheatenginedir + expectedfilename;

        LoadButton.Click;
      end;
    end;
  end;
  UpdateScanType;

  if scantablist<>nil then
    for i:=0 to scantablist.Count-1 do
      SaveCurrentState(PScanState(scantablist.TabData[i]));


  outputdebugstring('openProcessEpilogue exit');
end;

procedure TMainForm.ShowProcessListButtonClick(Sender: TObject);
var
  oldprocess: Dword;
  resu: integer;
  oldprocesshandle: thandle;
  oldprocessname: string;
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

procedure TMainForm.rbFsmAlignedChange(Sender: TObject);
begin
  if rbfsmLastDigts.checked then
    alignsizechangedbyuser:=false;

  VarType.OnChange(vartype);
end;

procedure TMainForm.ScanTypeSelect(Sender: TObject);
begin

end;

procedure TMainForm.Foundlist3Resize(Sender: TObject);
begin

end;

procedure TMainForm.Description1Click(Sender: TObject);
begin
  addresslist.doDescriptionChange;
end;

procedure TMainForm.edtAlignmentKeyPress(Sender: TObject; var Key: char);
begin
  if rbFsmAligned.checked then
    alignsizechangedbyuser:=true;
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

procedure TMainForm.cbFastScanChange(Sender: TObject);
begin
  edtAlignment.enabled:=cbFastScan.checked and cbfastscan.enabled;
  rbFsmAligned.enabled:=edtAlignment.enabled;
  rbfsmLastDigts.enabled:=edtAlignment.enabled;

  alignsizechangedbyuser:=false;
  VarType.OnChange(vartype);
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

procedure TMainForm.Foundlist3SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin

end;



procedure TMainForm.MenuItem1Click(Sender: TObject);
begin
  addresslist.SelectAll;
end;

procedure TMainForm.MenuItem7Click(Sender: TObject);
begin
  close;
end;

procedure TMainForm.miResetRangeClick(Sender: TObject);
begin
  {$ifdef cpu64}
    FromAddress.text:='0000000000000000';
    ToAddress.text:='7fffffffffffffff';
  {$else}
    FromAddress.text:='00000000';
    if Is64bitOS then
      ToAddress.text:='ffffffff' //games with 3GB aware will use this in 64-bit os's
    else
      ToAddress.text:='7fffffff';
  {$endif}
end;

procedure TMainForm.miChangeColorClick(Sender: TObject);
var i: integer;
begin
  if (addresslist.SelCount>0) and (colordialog1.execute) then
  begin
    for i:=0 to addresslist.Count-1 do
      if addresslist[i].isSelected then
        addresslist[i].color:=colordialog1.Color;
  end;
end;




procedure TMainForm.miBindActivationClick(Sender: TObject);
begin
  miBindActivation.Checked:=not miBindActivation.Checked;

  if addresslist.selectedRecord<>nil then
  begin
    if miBindActivation.Checked then
      addresslist.selectedRecord.options:=addresslist.selectedRecord.options+[moBindActivation]
    else
      addresslist.selectedRecord.options:=addresslist.selectedRecord.options-[moBindActivation];
  end;
end;



procedure TMainForm.miHideChildrenClick(Sender: TObject);
begin
  miHideChildren.Checked:=not miHideChildren.Checked;

  if addresslist.selectedRecord<>nil then
  begin
    if miHideChildren.Checked then
      addresslist.selectedRecord.options:=addresslist.selectedRecord.options+[moHideChildren]
    else
      addresslist.selectedRecord.options:=addresslist.selectedRecord.options-[moHideChildren]
  end;
end;

procedure TMainForm.setGbScanOptionsEnabled(state: boolean);
var i: integer;
begin
  gbScanOptions.Enabled := state;
  for i := 0 to gbScanOptions.ControlCount - 1 do
    gbScanOptions.Controls[i].Enabled := state;
end;

procedure TMainForm.LoadCustomTypesFromRegistry;
var
  reg: TRegistry;
  customtypes: TStringlist;
  i: integer;
  islua: boolean;
begin
  reg:=tregistry.create;
  vartype.OnChange:=nil; //disable the onchange event so CreateCustomType doesn't keep setting it
  try
    if reg.OpenKey('\Software\Cheat Engine\CustomTypes\',false) then
    begin
      CustomTypes:=TStringlist.create;
      reg.GetKeyNames(CustomTypes);
      for i:=0 to CustomTypes.count-1 do
      begin
        if reg.OpenKey('\Software\Cheat Engine\CustomTypes\'+CustomTypes[i],false) then
        begin
          try
            islua:=false;
            if reg.ValueExists('lua') then
              islua:=reg.ReadBool('lua');

            CreateCustomType(nil, reg.ReadString('Script'),true, islua);
          except
            outputdebugstring('The custom type script '''+CustomTypes[i]+''' could not be loaded');
          end;
        end;
      end;

      CustomTypes.free;
    end;
    reg.free;
    RefreshCustomTypes;
  finally
    vartype.OnChange:=VarTypeChange;   //set the onchange event back
  end;
end;

procedure TMainForm.RefreshCustomTypes;
{
In short: remove all custom scan types and add them back
}
var i: integer;
begin
  vartype.items.BeginUpdate;
  try
    while VarType.Items.Count>10 do
      vartype.items.Delete(10);


    for i:=0 to customTypes.Count-1 do
      vartype.Items.AddObject(TCustomType(customTypes[i]).name, customTypes[i]);

    //set to default (4 bytes) if not selected anything anymore
    if (vartype.ItemIndex=-1) or (vartype.ItemIndex>=VarType.Items.Count) then
      vartype.itemindex:=3;

    vartype.DropDownCount:=vartype.items.count;
  finally
    vartype.items.EndUpdate;
  end;

  addresslist.refreshcustomtypes;
end;

procedure TMainForm.miDeleteCustomTypeClick(Sender: TObject);
var reg : TRegistry;
  ct: TCustomType;
begin
  ct:=TCustomType(vartype.Items.Objects[vartype.ItemIndex]);
  if (ct<>nil) and ((ct.CustomTypeType=cttAutoAssembler) or (ct.CustomTypeType=cttLuaScript)) then
  begin
    if messagedlg('Are you sure you want to delete '+ct.name+'?',mtconfirmation, [mbno,mbyes],0)=mryes then
    begin
      reg:=tregistry.create;
      reg.DeleteKey('\Software\Cheat Engine\CustomTypes\'+ct.name);
      ct.remove;
      RefreshCustomTypes;
    end;
  end;
end;

procedure TMainForm.CreateCustomType(customtype: TCustomtype; script:string; changed: boolean; lua: boolean=false);
var
  reg : TRegistry;
  ct: TCustomType;
  oldname: string;
  i: integer;
begin

  ct:=nil;
  if changed then
  begin
    if customtype=nil then
    begin
      if not lua then
        ct:=TCustomType.CreateTypeFromAutoAssemblerScript(script)
      else
        ct:=TcustomType.CreateTypeFromLuaScript(script);
    end
    else
    begin
      //edited script

      ct:=customtype;
      oldname:=ct.name;
      ct.setScript(script,lua);

      //if the new script has a different name then delete the old one
      if oldname<>ct.name then
      begin
        //delete the old one
        reg:=Tregistry.Create;
        reg.DeleteKey('\Software\Cheat Engine\CustomTypes\'+oldname);
        reg.free;
      end;
    end;



    //Add/change this to the registry
    reg:=Tregistry.Create;
    if Reg.OpenKey('\Software\Cheat Engine\CustomTypes\'+ct.name,true) then
    begin
      reg.WriteString('Script',script);
      if lua then
        reg.WriteBool('lua',true);

    end;

    reg.free;

    RefreshCustomTypes;

    //now set the type to the current type
    if (ct<>nil) then
    begin
      for i:=0 to vartype.Items.Count-1 do
        if TCustomType(vartype.items.objects[i])=ct then
        begin
          vartype.itemindex:=i;
          if assigned(vartype.OnChange) then  //force an onchange (lazarus bug)
            vartype.OnChange(vartype);
          break;
        end;
    end;


  end;
end;



procedure TMainForm.miEditCustomTypeClick(Sender: TObject);
var ct: TCustomType;
begin
  ct:=TCustomType(vartype.Items.Objects[vartype.itemindex]);
  if (ct<>nil) and ((ct.CustomTypeType=cttAutoAssembler) or (ct.CustomTypeType=cttLuaScript))then
  begin

    with TfrmAutoInject.create(self) do
    begin
      injectintomyself:=true;
      CustomTypeScript:=true;
      CustomTypeCallback:=CreateCustomType;
      CustomType:=ct;
      if ct.CustomTypeType=cttLuaScript then
        luamode:=true;

      assemblescreen.Lines.Text:=CustomType.script;

      show;
    end;

  end;
end;


procedure TMainForm.miDefineNewCustomTypeLuaClick(Sender: TObject);
var fbn,n: string;
begin
  n:='Custom LUA type';
  fbn:='customvaluetype';
  if customTypes.count>0 then
  begin
    n:=n+' '+inttostr(customtypes.count+1);
    fbn:=fbn+' '+inttostr(customtypes.count+1);
  end;

  with TfrmAutoInject.create(self) do
  begin
    injectintomyself:=true;
    CustomTypeScript:=true;
    CustomTypeCallback:=CreateCustomType;
    CustomType:=nil;
    luamode:=true;

    with assemblescreen.Lines do
    begin
      Add('Note: keep the function base name unique.');
      Add('typename="'+n+'" --shown as the typename in ce');
      Add('bytecount=4  --number of bytes of this type');
      Add('functionbasename="'+fbn+'"');
      Add('');
      Add('function '+fbn+'_bytestovalue(b1,b2,b3,b4)');
      Add('--Add extra byte parameters as required');
      Add('return 123');
      Add('');
      Add('end');
      Add('');
      Add('function '+fbn+'_valuetobytes(i)');
      Add('');
      Add('--return the bytes to write (usually only used when you change the value)');
      Add('return 0,0,0,0');
      Add('');
      Add('end');
      Add('return typename,bytecount,functionbasename');
    end;
    show;

  end;
end;


procedure TMainForm.miDefineNewCustomTypeClick(Sender: TObject);
begin


  with TfrmAutoInject.create(self) do
  begin
    injectintomyself:=true;
    CustomTypeScript:=true;
    CustomTypeCallback:=CreateCustomType;
    CustomType:=nil;

    with assemblescreen.Lines do
    begin
      Add('alloc(TypeName,256)');
      Add('alloc(ByteSize,4)');
      Add('alloc(ConvertRoutine,1024)');
      Add('alloc(ConvertBackRoutine,1024)');
      Add('');
      Add('TypeName:');
      Add('db ''Custom Type Name'',0');
      Add('');
      Add('ByteSize:');
      Add('dd 4');
      Add('');
      Add('//The convert routine should hold a routine that converts the data to an integer (in eax)');
      Add('//function declared as: stdcall int ConvertRoutine(unsigned char *input);');
      Add('//Note: Keep in mind that this routine can be called by multiple threads at the same time.');
      Add('ConvertRoutine:');
      Add('//jmp dllname.functionname');
      add('[64-bit]');
      Add('//or manual:');
      Add('//parameters: (64-bit)');
      Add('//rcx=address of input');
      Add('mov eax,[rcx] //eax now contains the bytes ''input'' pointed to');
      Add('');
      Add('ret');
      add('[/64-bit]');
      add('');
      add('[32-bit]');
      Add('//jmp dllname.functionname');
      Add('//or manual:');
      Add('//parameters: (32-bit)'); //[esp]=return [esp+4]=input
      Add('push ebp');  //[esp]=ebp , [esp+4]=return [esp+8]=input
      Add('mov ebp,esp');  //[ebp]=ebp , [esp+4]=return [esp+8]=input
      Add('//[ebp+8]=input');
      Add('//example:');
      Add('mov eax,[ebp+8] //place the address that contains the bytes into eax');
      Add('mov eax,[eax] //place the bytes into eax so it''s handled as a normal 4 byte value');
      Add('');
      Add('pop ebp');
      Add('ret 4');
      add('[/32-bit]');

      Add('');
      Add('//The convert back routine should hold a routine that converts the given integer back to a row of bytes (e.g when the user wats to write a new value)');
      Add('//function declared as: stdcall void ConvertBackRoutine(int i, unsigned char *output);');
      Add('ConvertBackRoutine:');
      Add('//jmp dllname.functionname');
      Add('//or manual:');
      Add('[64-bit]');
      Add('//parameters: (64-bit)');
      Add('//ecx=input');
      Add('//rdx=address of output');
      Add('//example:');
      Add('mov [rdx],ecx //place the integer the 4 bytes pointed to by rdx');
      Add('');
      Add('ret');
      Add('[/64-bit]');
      add('');
      Add('[32-bit]');
      Add('//parameters: (32-bit)'); //[esp]=return [esp+4]=input
      Add('push ebp');  //[esp]=ebp , [esp+4]=return [esp+8]=input
      Add('mov ebp,esp');  //[ebp]=ebp , [esp+4]=return [esp+8]=input
      Add('//[ebp+8]=input');
      Add('//[ebp+c]=address of output');
      Add('//example:');
      Add('push eax');
      Add('push ebx');
      Add('mov eax,[ebp+8] //load the value into eax');
      Add('mov ebx,[ebp+c] //load the address into ebx');
      Add('mov [ebx],eax //write the value into the address');
      Add('pop ebx');
      Add('pop eax');

      Add('');
      Add('pop ebp');
      Add('ret 8');
      add('[/32-bit]');
      Add('');
    end;

    show;
  end;




end;

procedure TMainForm.miRecursiveSetValueClick(Sender: TObject);
begin
  miRecursiveSetValue.Checked:=not miRecursiveSetValue.Checked;

  if addresslist.selectedRecord<>nil then
  begin
    if miRecursiveSetValue.Checked then
      addresslist.selectedRecord.options:=addresslist.selectedRecord.options+[moRecursiveSetValue]
    else
      addresslist.selectedRecord.options:=addresslist.selectedRecord.options-[moRecursiveSetValue];
  end;
end;

procedure TMainForm.miRenameTabClick(Sender: TObject);
var s: string;
begin
  s:=scantablist.TabText[scantablist.SelectedTab];
  if InputQuery('Cheat Engine','What will be the new name for this tab?',s) then
    scantablist.TabText[scantablist.SelectedTab]:=s;
end;



procedure TMainForm.SaveCurrentState(scanstate: PScanState);
begin
  //save the current state
  scanstate.alignsizechangedbyuser:=alignsizechangedbyuser;

  scanstate.compareToSavedScan:=comparetosavedscan;
  scanstate.currentlySelectedSavedResultname:=currentlySelectedSavedResultname; //I love long variable names

  scanstate.lblcompareToSavedScan.caption:=lblcompareToSavedScan.caption;
  scanstate.lblcompareToSavedScan.visible:=lblcompareToSavedScan.visible;
  scanstate.lblcompareToSavedScan.left:=lblcompareToSavedScan.left;


  scanstate.FromAddress.text:=fromaddress.text;
  scanstate.ToAddress.text:=toaddress.text;
  scanstate.ReadOnly.checked:=readonly.checked;

  scanstate.cbfastscan.checked:=cbFastScan.checked;
  scanstate.edtAlignment.text:=edtAlignment.Text;

  scanstate.scanvalue.text:=scanvalue.text;
  scanstate.scanvalue.visible:=scanvalue.visible;

  if scanvalue2<>nil then
  begin
    scanstate.scanvalue2.exists:=true;
    scanstate.scanvalue2.text:=scanvalue2.text;
  end else scanstate.scanvalue2.exists:=false;

  scanstate.scantype.options:=scantype.Items.Text;
  scanstate.scantype.enabled:=scantype.enabled;
  scanstate.scantype.itemindex:=scantype.ItemIndex;
  scanstate.scantype.dropdowncount:=scantype.DropDownCount;

  scanstate.vartype.options:=vartype.Items.Text;
  scanstate.vartype.enabled:=vartype.enabled;
  scanstate.vartype.itemindex:=vartype.ItemIndex;


  scanstate.firstscanstate.caption:=newscan.caption;
  scanstate.firstscanstate.enabled:=newscan.enabled;
  scanstate.nextscanstate.enabled:=nextscanbutton.Enabled;


  scanstate.gbScanOptionsEnabled:=gbScanOptions.Enabled;

  scanstate.floatpanel.visible:=pnlfloat.visible;
  scanstate.floatpanel.rounded:=rt1.checked;
  scanstate.floatpanel.roundedextreme:=rt2.checked;
  scanstate.floatpanel.truncated:=rt3.checked;


  scanstate.rbbit.visible:=rbbit.visible;
  scanstate.rbbit.enabled:=rbbit.enabled;
  scanstate.rbbit.checked:=rbbit.checked;

  scanstate.rbdec.visible:=rbdec.visible;
  scanstate.rbdec.enabled:=rbdec.enabled;
  scanstate.rbdec.checked:=rbdec.checked;

  scanstate.HexadecimalCheckbox.visible:=HexadecimalCheckbox.visible;
  scanstate.HexadecimalCheckbox.checked:=HexadecimalCheckbox.checked;

  if cbpercentage<>nil then
  begin
    scanstate.cbpercentage.exists:=false;
    scanstate.cbpercentage.checked:=cbpercentage.Checked;
  end
  else
    scanstate.cbpercentage.exists:=false;

  scanstate.button2.tag:=button2.tag;
  scanstate.foundlist3.itemindex:=foundlist3.itemindex;

{
  if foundlist3.TopItem<>nil then
    scanstate.foundlist3.topitemindex:=foundlist3.topitem.Index
  else
    scanstate.foundlist3.topitemindex:=-1;    }
end;

procedure TMainForm.SetupInitialScanTabState(scanstate: PScanState; IsFirstEntry: boolean);
begin
  ZeroMemory(scanstate,sizeof(TScanState));

  if IsFirstEntry then
  begin
    scanstate.memscan:=memscan;
    scanstate.foundlist:=foundlist;
  end
  else
  begin
    scanstate.memscan:=tmemscan.create(progressbar1);
    scanstate.foundlist:=TFoundList.create(foundlist3, scanstate.memscan);
    scanstate.memscan.setScanDoneCallback(mainform.handle,wm_scandone);
  end;

  savecurrentstate(scanstate);

  //initial scans don't have a previous scan
  scanstate.lblcompareToSavedScan.visible:=false;
  scanstate.compareToSavedScan:=false;



end;

procedure TMainForm.ScanTabListTabChange(sender: TObject; oldselection: integer);
var oldstate,newstate: PScanState;
begin
  oldstate:=scantablist.TabData[oldselection];
  newstate:=scantablist.TabData[scantablist.SelectedTab];

  savecurrentstate(oldstate);

  //load the new state
  if newstate<>nil then
  begin
    //load
    mainform.BeginFormUpdate;

    scantype.OnChange:=nil;
    vartype.onchange:=nil;
    rbbit.OnClick:=nil;
    rbdec.Onclick:=nil;
    HexadecimalCheckbox.OnClick:=nil;

    scanvalue.text:=newstate.scanvalue.text;
    scanvalue.visible:=newstate.scanvalue.visible;

    if newstate.scanvalue2.exists then
    begin
      CreateScanValue2;
      scanvalue2.text:=newstate.scanvalue2.text;
    end
    else
    begin
      //destroy if it exists
      DestroyScanValue2;
    end;

    alignsizechangedbyuser:=newstate.alignsizechangedbyuser;
    comparetosavedscan:=newstate.compareToSavedScan;
    currentlySelectedSavedResultname:=newstate.currentlySelectedSavedResultname; //I love long variable names

    lblcompareToSavedScan.caption:=newstate.lblcompareToSavedScan.caption;
    lblcompareToSavedScan.visible:=newstate.lblcompareToSavedScan.visible;
    lblcompareToSavedScan.left:=newstate.lblcompareToSavedScan.left;



    scantype.items.text:=newstate.scantype.options;
    scantype.enabled:=newstate.scantype.enabled;
    scantype.ItemIndex:=newstate.scantype.itemindex;
    scantype.DropDownCount:=newstate.scantype.dropdowncount;

    vartype.items.text:=newstate.vartype.options;
    vartype.enabled:=newstate.vartype.enabled;
    vartype.ItemIndex:=newstate.vartype.itemindex;


    newscan.caption:=newstate.firstscanstate.caption;
    newscan.enabled:=newstate.firstscanstate.enabled;

    nextscanbutton.Enabled:=newstate.nextscanstate.enabled;

    cbFastScan.Checked:=newstate.cbfastscan.checked;
    edtAlignment.Text:=newstate.edtAlignment.text;

    setGbScanOptionsEnabled(newstate.gbScanOptionsEnabled);

    cbFastScanClick(cbfastscan);    //update the alignment textbox

    pnlfloat.visible:=newstate.floatpanel.visible;
    rt1.checked:=newstate.floatpanel.rounded;
    rt2.checked:=newstate.floatpanel.roundedextreme;
    rt3.checked:=newstate.floatpanel.truncated;

    rbbit.visible:=newstate.rbbit.visible;
    rbbit.enabled:=newstate.rbbit.enabled;
    rbbit.checked:=newstate.rbbit.checked;

    rbdec.visible:=newstate.rbdec.visible;
    rbdec.enabled:=newstate.rbdec.enabled;
    rbdec.checked:=newstate.rbdec.checked;

    HexadecimalCheckbox.visible:=newstate.HexadecimalCheckbox.visible;
    HexadecimalCheckbox.checked:=newstate.HexadecimalCheckbox.checked;

    if newstate.cbpercentage.exists then
    begin
      CreateCbPercentage;
      cbpercentage.Checked:=newstate.cbpercentage.checked;
    end
    else
      DestroyCbPercentage;

    button2.tag:=newstate.button2.tag;

    scantype.OnChange:=ScanTypeChange;
    VarType.OnChange:=VarTypeChange;
    rbbit.OnClick:=rbBitClick;
    rbdec.Onclick:=rbDecClick;
    HexadecimalCheckbox.OnClick:=HexadecimalCheckboxClick;

    mainform.EndFormUpdate;


    foundlist3.beginupdate;

    foundlist.Deinitialize;

    memscan:=newstate.memscan;
    foundlist:=newstate.foundlist;


    UpdateScanType;


    foundcount:=foundlist.Initialize(getvartype,memscan.customtype);

    foundlist3.endupdate;


    if newstate.foundlist3.itemindex<foundcount then
    begin
      foundlist3.ItemIndex:=newstate.foundlist3.itemindex;
      foundlist3.Items[newstate.foundlist3.itemindex].Selected:=true;
      foundlist3.Items[newstate.foundlist3.itemindex].MakeVisible(false);
      foundlist3.Items[newstate.foundlist3.itemindex].Top:=0;
    end;
//    foundlist3.TopItem:=foundlist3.items[newstate.foundlist.itemindex];
  end;
  //else leave empty
end;

procedure TMainForm.miAddTabClick(Sender: TObject);
var
  i: integer;
  c: array of tcontrol;
  foundlistheightdiff: integer;
  newstate: PScanState;
begin
  if scantablist=nil then
  begin
    setlength(c, panel5.ControlCount);
    for i:=0 to panel5.controlcount-1 do
      c[i]:=panel5.controls[i];


    foundlistheightdiff:=btnMemoryView.top-(foundlist3.top+foundlist3.Height);

    scantablist:=TTablist.Create(self);
    scantablist.PopupMenu:=pmTablist;
    scantablist.parent:=panel5;
    scantablist.top:=panel7.top+panel7.height;
    scantablist.Left:=0;
    scantablist.Width:=clientwidth-logopanel.width;
    scantablist.color:=panel5.Color;
    scantablist.height:=20;

    scantablist.Anchors:=scantablist.Anchors+[akRight];

    i:=scantablist.AddTab('Scan 1'); //original scan

    getmem(newstate, sizeof(TScanState));
    SetupInitialScanTabState(newstate,true);
    scantablist.TabData[i]:=newstate;

    i:=scantablist.AddTab('Scan 2'); //first new scan
    getmem(newstate, sizeof(TScanState));
    SetupInitialScanTabState(newstate,false);
    scantablist.TabData[i]:=newstate;

    scantablist.OnTabChange:=ScanTabListTabChange;
    scantablist.SelectedTab:=i;

    tabcounter:=3;


    //p.height:=p.TabHeight;

    //make space for the tabs
    //luckely this routine is not called often

    for i:=0 to length(c)-1 do
    begin
      if (c[i]<>panel7) and
         (c[i]<>LoadButton) and
         (c[i]<>SaveButton) and
         (c[i]<>ProcessLabel) and
         (c[i]<>Progressbar1) and
         (c[i]<>logopanel) and
         (c[i]<>btnMemoryView) and
         (c[i]<>speedbutton2) and
         (c[i]<>button1)
      then
      begin
        panel5.Controls[i].Top:=panel5.Controls[i].Top+20; //p.height;
        //c[i].Parent:=p;
      end;

    end;

    scantablist.Color:=clBtnFace;
    scantablist.Brush.Color:=clBtnFace;


    foundlist3.Height:=btnMemoryView.top-foundlist3.top-foundlistheightdiff;

    panel5.Constraints.MinHeight:=gbScanOptions.top+gbScanOptions.height+speedbutton2.height+3;

    if panel5.Height<panel5.Constraints.MinHeight then
      panel5.Height:=panel5.Constraints.MinHeight;



  end
  else
  begin
    i:=scantablist.addtab('Scan '+inttostr(tabcounter));
    getmem(newstate, sizeof(TScanState));
    SetupInitialScanTabState(newstate,false);
    scantablist.TabData[i]:=newstate;

    scantablist.SelectedTab:=i;
    inc(tabcounter);
  end;

  if NextScanButton.Enabled then
    newScan.click;

end;

procedure TMainForm.miCloseTabClick(Sender: TObject);
var oldscanstate: PScanState;
    oldindex: integer;
begin
  if (scantablist<>nil) and (scantablist.count>1) then
  begin
    //since rightclicking a tab selects it, delete the currently selected tab
    oldindex:=scantablist.SelectedTab;
    oldscanstate:=scantablist.TabData[scantablist.SelectedTab];

    //switch the currently selected tab to the right one if possible, else the lef tone
    if oldindex<scantablist.count-1 then
      scantablist.SelectedTab:=oldindex+1
    else
      scantablist.SelectedTab:=oldindex-1;

    scantablist.RemoveTab(oldindex);

    //now we can delete the tabdata
    oldscanstate.foundlist.Free;
    oldscanstate.memscan.Free;
    freemem(oldscanstate);

  end;
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

procedure TMainForm.miSaveScanresultsClick(Sender: TObject);
var n: string;
begin
  if memscan.nextscanCount>0 then
  begin
    n:='Scanresult '+inttostr(memscan.nextscanCount+1);
    if inputquery('Save scan results', 'What name do you want to give to these scanresults?', n) then
    begin
      memscan.saveresults(n);
      UpdateScanType;
    end;
  end;
end;

procedure TMainForm.miShowAsBinaryClick(Sender: TObject);
begin
  if (addresslist.selectedrecord<>nil) and (addresslist.selectedrecord.vartype=vtbinary) then
    addresslist.selectedrecord.extra.bitData.showasbinary:=not addresslist.selectedrecord.extra.bitData.showasbinary;
end;

procedure TMainForm.miZeroTerminateClick(Sender: TObject);
begin
  if (addresslist.selectedRecord<>nil) and (addresslist.selectedRecord.VarType=vtString) then
    addresslist.selectedRecord.Extra.stringData.ZeroTerminate:=not addresslist.selectedRecord.Extra.stringData.ZeroTerminate;
end;

procedure TMainForm.Panel1Click(Sender: TObject);
begin

end;

procedure TMainForm.Panel5Resize(Sender: TObject);
begin
  speedbutton3.top:=foundlist3.top+foundlist3.height-speedbutton3.Height;
  speedbutton3.left:=foundlist3.left+foundlist3.width+2;
  foundlist3.Columns[1].width:=foundlist3.ClientWidth-foundlist3.Columns[0].Width;

  ScanText.left:=scanvalue.left; //lazarus rev  25348 32-bit fix
  if ScanText2<>nil then
    scantext2.left:=scanvalue2.Left;

  if andlabel<>nil then
    andlabel.Left:=scanvalue2.Left-20;


  lblcompareToSavedScan.left:=newscan.left + ((((nextscanbutton.left+nextscanbutton.width) - newscan.left) div 2) - (lblcompareToSavedScan.width div 2));

  if cbpercentage<>nil then
    cbpercentage.left:=scantype.left+scantype.width+5;
end;

procedure TMainForm.pmTablistPopup(Sender: TObject);
var x,y: integer;
begin
  if scantablist<>nil then //should always be true
  begin
    x:=scantablist.ScreenToClient(mouse.cursorpos).x;
    y:=scantablist.ScreenToClient(mouse.cursorpos).y;
    miCloseTab.visible:=scantablist.GetTabIndexAt(x,y)<>-1;
    miTablistSeperator.Visible:=miCloseTab.visible;
    miRenameTab.Visible:=miCloseTab.visible;
  end;
end;

procedure TMainForm.pmValueTypePopup(Sender: TObject);
begin
  miEditCustomType.visible:=(vartype.itemindex<>-1) and (vartype.items.objects[vartype.itemindex]<>nil);
  miDeleteCustomType.visible:=miEditCustomType.visible;
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
  setGbScanOptionsEnabled(true);

  cbFastScanClick(cbfastscan);


  VartypeChange(vartype);
  foundlist.deleteresults;

  if scanvalue.Visible and scanvalue.Enabled then
  begin
    scanvalue.SetFocus;
    scanvalue.SelectAll;
  end;

  compareToSavedScan:=false;
  lblcompareToSavedScan.visible:=false;

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
//   ExtractFilePath();
end;

resourcestring
  strClickToGoHome = 'Click here to go to the Cheat Engine homepage';

function TMainform.onhelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
begin
  callhelp:=false;
  result:=true;

  if command=HELP_CONTEXT then
    HtmlHelpA(Win32WidgetSet.AppHandle,pchar(cheatenginedir+'cheatengine.chm'),HH_HELP_CONTEXT,data);
end;


procedure TMainForm.FormCreate(Sender: TObject);
var
  pid: dword;
  tokenhandle: thandle;
  tp: TTokenPrivileges;
  prev: TTokenPrivileges;

  ReturnLength: Dword;

  differentWidth: integer;
  x: array of integer;

  errormode: dword;
  minworkingsize, maxworkingsize: size_t;
  reg: tregistry;
begin

  reg:=Tregistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine',false) then
    begin
      if reg.ValueExists('Initial tables dir') then
      begin
        SaveDialog1.InitialDir:=reg.Readstring('Initial tables dir');
        opendialog1.InitialDir:=SaveDialog1.initialdir;
      end
      else
      begin
        SaveDialog1.InitialDir:=tablesdir;
        opendialog1.InitialDir:=tablesdir;
      end;
    end;

  finally
    reg.free;
  end;

  application.OnHelp:=onhelp;



  forms.Application.ShowButtonGlyphs:=sbgNever;
  application.OnException := exceptionhandler;
  errormode := SetErrorMode(0);
  setErrorMode(errormode or SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);




  actScriptEngine.ShortCut := TextToShortCut('Ctrl+Shift+C');


  hotkeypressed := -1;

  pid := GetCurrentProcessID;

  ownprocesshandle := OpenProcess(PROCESS_ALL_ACCESS, True, pid);




  tokenhandle := 0;

  if ownprocesshandle <> 0 then
  begin
    if OpenProcessToken(ownprocesshandle, TOKEN_QUERY or TOKEN_ADJUST_PRIVILEGES, tokenhandle) then
    begin
      ZeroMemory(@tp,sizeof(tp));
      if lookupPrivilegeValue(nil, 'SeDebugPrivilege', tp.Privileges[0].Luid) then
      begin
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        tp.PrivilegeCount := 1; // One privilege to set
        if not AdjustTokenPrivileges(tokenhandle, False, tp, sizeof(tp),
          prev, returnlength) then
          ShowMessage('Failure setting the debug privilege. Debugging may be limited.');
      end;


      ZeroMemory(@tp,sizeof(tp));
      if lookupPrivilegeValue(nil, SE_LOAD_DRIVER_NAME, tp.Privileges[0].Luid) then
      begin
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        tp.PrivilegeCount := 1; // One privilege to set
        if not AdjustTokenPrivileges(tokenhandle, False, tp, sizeof(tp),
          prev, returnlength) then
          ShowMessage('Failure setting the load driver privilege. Debugging may be limited.');
      end;




      if GetSystemType>=7 then
      begin
        ZeroMemory(@tp,sizeof(tp));
        if lookupPrivilegeValue(nil, 'SeCreateGlobalPrivilege', tp.Privileges[0].Luid) then
        begin
          tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
          tp.PrivilegeCount := 1; // One privilege to set
          if not AdjustTokenPrivileges(tokenhandle, False, tp, sizeof(tp),
            prev, returnlength) then
            ShowMessage('Failure setting the CreateGlobal privilege.');
        end;



        {$ifdef cpu64}
        ZeroMemory(@tp,sizeof(tp));
        ZeroMemory(@prev, sizeof(prev));
        if lookupPrivilegeValue(nil, 'SeLockMemoryPrivilege', tp.Privileges[0].Luid) then
        begin
          tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
          tp.PrivilegeCount := 1; // One privilege to set
          AdjustTokenPrivileges(tokenhandle, False, tp, sizeof(tp), prev, returnlength);
        end;

        {$endif}
      end;


      ZeroMemory(@tp,sizeof(tp));
      ZeroMemory(@prev, sizeof(prev));
      if lookupPrivilegeValue(nil, 'SeIncreaseWorkingSetPrivilege', tp.Privileges[0].Luid) then
      begin
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        tp.PrivilegeCount := 1; // One privilege to set
        AdjustTokenPrivileges(tokenhandle, False, tp, sizeof(tp), prev, returnlength);
      end;



    end;

    if GetProcessWorkingSetSize(ownprocesshandle, minworkingsize, maxworkingsize) then
      SetProcessWorkingSetSize(ownprocesshandle, 16*1024*1024, 64*1024*1024);


  end;


  tempbitmap := TBitmap.Create;

  scanvalue.Text := '';

  {$ifdef cpu64}
  fromaddress.MaxLength:=16;
  toaddress.MaxLength:=16;
  {$else}
  fromaddress.MaxLength:=8;
  toaddress.MaxLength:=8;
  {$endif}

  miResetRange.click;

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
  progressbar1.Width := progressbar1.Width - differentwidth;
  undoscan.left := undoscan.left - differentwidth;

  //create object for the auto attach list
  autoattachlist := TStringList.Create;
  autoattachlist.CaseSensitive := False; //set it up as not case sensitive
  autoattachlist.Delimiter:=';';

  extraautoattachlist := TStringlist.create;
  extraautoattachlist.CaseSensitive := False; //set it up as not case sensitive
  extraautoattachlist.Delimiter:=';';

  randomize;



{$ifdef ceasinjectabledll}
  //panel7.Visible:=false;
  speedbutton1.Enabled := False;
  processid := getcurrentprocessid;
  processhandle := getcurrentprocess;
  enableGui;
  processlabel.Caption := Inttohex(processid, 8) + ' : ' + 'Current process';
{$endif}



  oldhandle := mainform.handle;

  panel5.Constraints.MinHeight:=gbScanOptions.top+gbScanOptions.height+speedbutton2.height+3;
  mainform.Constraints.MinWidth:=400;
  mainform.Constraints.MinHeight:=panel5.height+150;

  addresslist:=TAddresslist.create(self);
  addresslist.width:=500;
  addresslist.height:=150;
  addresslist.top:=50;
  addresslist.parent:=panel1;
  addresslist.PopupMenu:=popupmenu2;
  addresslist.OnDropByListview:=AddresslistDropByListview;
  addresslist.OnAutoAssemblerEdit:=AddressListAutoAssemblerEdit;
  addresslist.Align:=alClient;


  symhandler.loadCommonModuleList;

  setlength(x, 7);
  if loadformposition(self, x) then
  begin
    addresslist.headers.Sections[0].Width := x[0];
    addresslist.headers.Sections[1].Width := x[1];
    addresslist.headers.Sections[2].Width := x[2];
    addresslist.headers.Sections[3].Width := x[3];
    addresslist.headers.Sections[4].Width := x[4];
    panel5.Height := x[5];
    foundlist3.columns[0].Width := x[6];
  end;

  pluginhandler := TPluginhandler.Create;

  //custom types
  LoadCustomTypesFromRegistry;


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
  b: dword;
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
  if addresslist<>nil then
    addresslist.Refresh;

  updatetimer.Enabled:=false;

  inc(reinterpretcheck);
  if reinterpretcheck mod 15=0 then reinterpretaddresses;

  updatetimer.Enabled:=true;
end;

procedure TMainForm.FreezeTimerTimer(Sender: TObject);
begin
  freezetimer.enabled:=false;
  if addresslist<>nil then
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
begin
  checkpaste;

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

  firstispointer: boolean;
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

          ok:=(res=mryes) or (res=mrYesToAll);

          if (res=mryes) or (res=mrno) then
            res:=-1; //reset (not an xxx to all)
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
var old: TNotifyevent;
  s: tstringlist;
  l: TfrmSelectionList;
begin
  old:=scantype.OnChange;
  try
    if (scantype.ItemIndex<>-1) then
    begin
      //currentlySelectedSavedResultname
      if (scantype.Items[scantype.itemindex]=strcompareToSavedScan) or (scantype.Items[scantype.itemindex]=strCompareToFirstScan) then
      begin
        s:=tstringlist.Create;
        try
          if (not scantypechangedbyhotkey) or (memscan.getsavedresults(s)>1) then
          begin
            //popup a window where the user can select the scanresults
            //currentlySelectedSavedResultname
            l:=TfrmSelectionList.create(self,s);
            l.Caption:='Saved scan results';
            l.label1.caption:='Select the saved scan result from the list below';
            l.itemindex:=0;

            if (l.showmodal=mrok) and (l.itemindex<>-1) then
            begin
              currentlySelectedSavedResultname:=l.selected;
              if l.itemindex=0 then
                lblcompareToSavedScan.caption:='Comparing to first scan results'
              else
                lblcompareToSavedScan.caption:='Comparing to '+currentlySelectedSavedResultname;
            end
            else
            begin
              scantype.itemindex:=lastscantype;
              exit;
            end;
          end
          else
          begin
            currentlySelectedSavedResultname:='FIRST';
            lblcompareToSavedScan.caption:='Comparing to first scan results';
          end;
        finally
          s.free;
        end;

        scantype.Items[scantype.itemindex]:=strCompareToLastScan;
        scantype.itemindex:=lastscantype;
        compareToSavedScan:=true;

        lblcompareToSavedScan.visible:=true;
        lblcompareToSavedScan.left:=newscan.left + ((((nextscanbutton.left+nextscanbutton.width) - newscan.left) div 2) - (lblcompareToSavedScan.width div 2));

      end
      else
      if scantype.Items[scantype.itemindex]=strCompareToLastScan then
      begin
        scantype.Items[scantype.itemindex]:=strcompareToSavedScan;
        scantype.itemindex:=lastscantype;
        compareToSavedScan:=false;
        lblcompareToSavedScan.visible:=false;
      end;
    end;

    updatescantype;
  finally
    scantype.OnChange:=old;
  end;
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
  d: single;
  i: integer;
  hexvis: boolean;
  decbitvis: boolean;
  hextext: string;
  hexwidth: integer;
  casevis: boolean;

  oldscantype: integer;
  temp: string;

  newvartype: integer;
  unicodevis: boolean;
  tc: tbitmap;

  alignsize: integer;
begin
  //todo: rewrite this
  oldscantype:=scantype.ItemIndex;
  newvartype:=vartype.ItemIndex;

  dontconvert:=true;

  hexvis:=true;
  unicodevis:=false;
  hexwidth:=50;

  hextext:='Hex';
  casevis:=false;

  decbitvis:=false;

  if rbFsmAligned.checked and (not alignsizechangedbyuser) then
  begin
    if vartype.Items.Objects[vartype.ItemIndex]<>nil then
    begin
      //custom type is ALWAYS the decider
      if rbFsmAligned.checked then
        edtAlignment.text:=inttohex(TCustomType(vartype.Items.Objects[vartype.ItemIndex]).bytesize,1);
    end
    else
    begin
      try
        case newvartype of
          0,1,7,8,9: alignsize:=1; //byte, aob, string
          2: alignsize:=2; //word
          else
            alignsize:=4; //dword, float, single, etc...
        end;

        if rbFsmAligned.checked then
          edtAlignment.text:=inttohex(alignsize,1);
      except
      end;
    end;

  end;



  //convertroutine:
  if (oldvartype in [1,2,3,4,5,6,9]) or (oldvartype>10) then
  begin
    //it was one of the normal values
    hexstateForIntTypes:=hexadecimalcheckbox.Checked;

    if (newvartype in [1,2,3,4]) or (oldvartype>10) then
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

    end
    else
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

  if (newvartype in [1,2,3,4,9]) or (newvartype>=10) then //if normal or custom type
  begin
    casevis:=false;
    hexvis:=true;
    scanvalue.MaxLength:=0;
    hexadecimalcheckbox.enabled:=newscan.enabled;
    hexadecimalcheckbox.Checked:=hexstateForIntTypes;
  end
  else
  case newvartype of
  0: begin //binary
       rbdec.checked:=true;
       hexadecimalcheckbox.Checked:=false;
       HexadecimalCheckbox.visible:=false;
       decbitvis:=true;
       Scantype.itemindex:=0;
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

  cbfastscan.Enabled:=NewScan.enabled and (not nextscanbutton.enabled);   //only enabled when newscan is enabled and nextscan not



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


  if decbitvis then
    HexadecimalCheckbox.visible:=false;

  panel5.OnResize(panel5); //lazarus, force the scantext left

end;

procedure TMainForm.LogoClick(Sender: TObject);
begin
  if messagedlg('Do you want to go to the Cheat Engine website?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ShellExecute(0, PChar('open'), PChar('http://www.cheatengine.org/?referredby=CE60'),
      PChar(''), PChar(''), SW_MAXIMIZE);

end;

procedure TMainForm.WindowsClick(Sender: TObject);
begin

end;

procedure TMainForm.rbAllMemoryClick(Sender: TObject);
begin

end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CheckIfSaved;
end;

resourcestring
  strdeleteall = 'Are you sure you want to delete all addresses?';

procedure TMainForm.SpeedButton2Click(Sender: TObject);
begin
  if messagedlg(strdeleteall, mtConfirmation, [mbno,mbyes],0)=mryes then
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
  foundlist3.BeginUpdate;
  try

    for i := 0 to foundlist3.Items.Count - 1 do
    begin
      //foundlist3.items[i].Selected := True;
      foundlist3.Items[i].Selected:=true;
    end;
  finally
    foundlist3.EndUpdate;
  end;


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

    //6.0
    selectionCount: integer;
    selectedrecord: TMemoryRecord;
begin
  sethotkey1.Caption:='Set/Change hotkeys';

  selectedrecord:=addresslist.selectedRecord;
  selectionCount:=0;
  for i:=0 to addresslist.count-1 do
    if addresslist.MemRecItems[i].isSelected then
      inc(selectioncount);


  miZeroTerminate.visible:=(selectedrecord<>nil) and (selectedrecord.VarType=vtString);
  miZeroTerminate.Checked:=(miZeroTerminate.visible) and (selectedrecord.Extra.stringData.ZeroTerminate);

  DeleteThisRecord1.visible:=(addresslist.selectedRecord<>nil);
  Change1.visible:=(addresslist.selectedrecord<>nil) and (not (addresslist.selectedRecord.vartype=vtAutoAssembler));
  address1.enabled:=(addresslist.selectedrecord<>nil) and (not addresslist.selectedRecord.isGroupHeader);
  ype1.enabled:=address1.enabled;
  Value1.enabled:=address1.enabled;
  Smarteditaddresses1.enabled:=true;

  BrowseThisMemoryRegion1.visible:=(addresslist.selectedRecord<>nil) and (not addresslist.selectedRecord.isGroupHeader) and (not (addresslist.selectedRecord.vartype=vtAutoAssembler));
  ShowAsHexadecimal1.visible:=(addresslist.selectedRecord<>nil) and (addresslist.selectedRecord.VarType in [vtByte, vtWord, vtDword, vtQword, vtSingle, vtDouble, vtCustom, vtByteArray]) and (not addresslist.selectedRecord.isGroupHeader);

  if (addresslist.selectedRecord<>nil) and (addresslist.selectedrecord.VarType = vtBinary) then
  begin
    if addresslist.selectedRecord.Extra.bitData.showasbinary then
      miShowAsBinary.caption:='Show as decimal'
    else
      miShowAsBinary.caption:='Show as binary';

    miShowAsBinary.visible:=true;
  end
  else miShowAsBinary.visible:=false;


  if (addresslist.selectedRecord<>nil) then
  begin
    if not addresslist.selectedRecord.showAsHex then
      ShowAsHexadecimal1.Caption:='Show as hexadecimal'
    else
      ShowAsHexadecimal1.Caption:='Show as decimal';
  end;



  SetHotkey1.visible:=(addresslist.selectedRecord<>nil) and (not addresslist.selectedRecord.isGroupHeader);

  Freezealladdresses2.visible:=(addresslist.selectedRecord<>nil);

  Changescript1.visible:=(addresslist.selectedRecord<>nil) and (addresslist.selectedrecord.VarType=vtAutoAssembler);

  n5.visible:=(addresslist.selectedRecord<>nil);

  Pointerscanforthisaddress1.visible:=(addresslist.selectedRecord<>nil) and (not addresslist.selectedRecord.isGroupHeader) and (not (addresslist.selectedRecord.vartype=vtAutoAssembler));
  Findoutwhataccessesthisaddress1.visible:=(addresslist.selectedRecord<>nil) and (not addresslist.selectedRecord.isGroupHeader) and (not (addresslist.selectedRecord.vartype=vtAutoAssembler));
  Setbreakpoint1.visible:=(addresslist.selectedRecord<>nil) and (not addresslist.selectedRecord.isGroupHeader) and (not (addresslist.selectedRecord.vartype=vtAutoAssembler));

  sep1.visible:=(addresslist.selectedRecord<>nil) and (not addresslist.selectedRecord.isGroupHeader) and (not (addresslist.selectedRecord.vartype=vtAutoAssembler));
  Calculatenewvaluepart21.visible:=(addresslist.count>0);
  Forcerechecksymbols1.visible:=addresslist.count>0;

  //one extra check for recalculate (don't show it when an aa address is selected)
  if (addresslist.selectedRecord<>nil) and (addresslist.selectedRecord.vartype=vtAutoAssembler) then
    Calculatenewvaluepart21.visible:=false;

  n4.visible:=addresslist.count>0;

  n1.visible:=true;
  CreateGroup.visible:=true;

  if (selectedrecord<>nil) and selectedrecord.treenode.HasChildren then
  begin
    miGroupconfig.visible:=true;
    miHideChildren.checked:=moHideChildren in selectedrecord.options;
    miBindActivation.checked:=moBindActivation in selectedrecord.options;
    miRecursiveSetValue.checked:=moRecursiveSetValue in selectedrecord.options;
  end
  else
    miGroupconfig.Visible:=false;

  miChangeColor.Visible:=addresslist.selcount>0;

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
  foundlist.initialize(getvartype,memscan.CustomType);
end;



procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;
  reg: Tregistry;
  crashcounter: integer;

  h: thandle;

begin


  //undo unrandomize
  if unrandomize<>nil then
    freeandnil(unrandomize);

  cbSpeedhack.Checked:=false;

  if flashprocessbutton<>nil then
  begin
    flashprocessbutton.Terminate;
    flashprocessbutton.WaitFor;
    freeandnil(FlashProcessButton);
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

procedure TMainform.paste(simplecopypaste: boolean);
{
this routine will paste a entry from the cplipboard into the addresslist of CE
If simplecopypaste is false frmPasteTableentry is shown to let the user change
some stuff before adding the new entry

returns the entry number of the new addresses (first one)
}
var s: string;
begin
  s:=clipboard.AsText;
  addresslist.AddTableXMLAsText(s,simplecopypaste);
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
  OutputDebugString('Setbreakpoint1Click');

  if addresslist.selectedRecord<>nil then
  begin
    address:=addresslist.selectedRecord.GetRealAddress;

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
    memscan.undolastscan;
    foundcount:=foundlist.Initialize(getvartype,memscan.CustomType);
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
      shellexecute(0, 'open', 'Tutorial-i386.exe', nil, nil, sw_show);
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
  if (month = 1) and (day = 1) then
    ShowMessage(strnewyear);
  if (month = 1) and (day = 1) and (year >= 2015) then
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



  if memscan=nil then
    memscan:=tmemscan.create(progressbar1);

  foundlist:=tfoundlist.create(foundlist3,memscan);

  //don't put this in oncreate, just don't
  memscan.setScanDoneCallback(mainform.handle,wm_scandone);

  InitializeLuaScripts;
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


  if formsettings.cbKernelQueryMemoryRegion.checked then UseDBKQueryMemoryRegion else DontUseDBKQueryMemoryRegion;
  if formsettings.cbKernelReadWriteProcessMemory.checked then UseDBKReadWriteMemory else DontUseDBKReadWriteMemory;
  if formsettings.cbKernelOpenProcess.Checked then UseDBKOpenProcess else DontUseDBKOpenProcess;

  adjustbringtofronttext;

  if not NextScanButton.Enabled then
  begin
    //memscan can be reset
    if memscan<>nil then
      memscan.free;

    memscan:=tmemscan.create(progressbar1);
    memscan.setScanDoneCallback(mainform.handle,wm_scandone);
  end;
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
var
  i: integer;
  newstate: boolean;
begin
  if addresslist.selectedRecord<>nil then
  begin
    newstate:=not addresslist.selectedRecord.showAsHex;

    for i:=0 to addresslist.Count-1 do
      if addresslist[i].isSelected then
        addresslist[i].showAsHex:=newstate;
  end;
end;

procedure TMainForm.OpenMemorybrowser1Click(Sender: TObject);
begin
  btnMemoryView.click;
end;


procedure TMainForm.cbFastScanClick(Sender: TObject);
begin



end;



resourcestring
  strdontbother = 'Don''t even bother. Cheat Engine uses the main thread to receive messages when the scan is done, freeze it and CE will crash!';

procedure TMainForm.cbPauseWhileScanningClick(Sender: TObject);

begin
  if (cbPauseWhileScanning.checked) and (processid=getcurrentprocessid) then
  begin
    cbPauseWhileScanning.Checked:=false;
    messagedlg(strdontbother,mtError,[mbok],0);
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
  if assigned(IsValidHandle) then
  begin
    if (button = mbright) and (DBKLoaded) and IsValidHandle(processhandle) then
    begin
      outputdebugstring('(button = mbright) and (DBKLoaded) and IsValidHandle(processhandle)');
      tfrmProcessInfo.Create(self).Show;
    end;
  end else outputdebugstring('IsValidHandle is unassigned');
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
        start;
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
var s: string;
begin

  defaultdraw:=true;
 // s:=item.Caption;
//  item.SubItems[0]:='';
//  s:=item.SubItems[0];




  if foundlist.inmodule(item.index) then
    foundlist3.Canvas.Font.Color:=clgreen;
end;

resourcestring
  strUnknownExtension = 'Unknown extension';

procedure TMainForm.SaveIntialTablesDir(dir: string);
var reg: tregistry;
begin
  reg:=Tregistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine',true) then
      reg.WriteString('Initial tables dir', dir);

  finally
    reg.free;
  end;
end;

procedure TMainForm.actOpenExecute(Sender: TObject);
var
  merge: boolean;
  app: word;
  Extension: string;

begin
  merge:=false;
  if not autoopen then
    if CheckIfSaved=false then exit;



  if autoopen or Opendialog1.Execute then
  begin
    SaveIntialTablesDir(extractfilepath(Opendialog1.filename));

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


  if Savedialog1.Execute then
    savetable(savedialog1.FileName);

  opendialog1.FileName:=savedialog1.filename;

  SaveIntialTablesDir(extractfilepath(savedialog1.filename));

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

procedure TMainform.AddressListAutoAssemblerEdit(sender: TObject; memrec: TMemoryRecord);
var x: TFrmAutoInject;
    y: array of integer;
begin
  if memrec.isBeingEdited then
  begin
    memrec.autoAssembleWindow.visible:=true;
    memrec.autoAssembleWindow.BringToFront;
  end
  else
  begin
    x:=tfrmautoinject.Create(self);
    with x do
    begin
      //name:='AAEditScript';
      new1.Enabled:=false;

      editscript:=true;
      editscript2:=true;

      memrec:=addresslist.selectedRecord;
      memrec.beginEdit;
      memrec.autoAssembleWindow:=x;
      callbackroutine:=changeScriptCallback;

      assemblescreen.text:=memrec.AutoAssemblerData.script.text;

      setlength(y,0);
      loadformposition(x,y);
      show;
    end;

  end;

end;

procedure TMainForm.Changescript1Click(Sender: TObject);
begin
  if (addresslist.selectedRecord<>nil) and (addresslist.selectedRecord.VarType=vtAutoAssembler) then
    AddressListAutoAssemblerEdit(addresslist, addresslist.selectedRecord);
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

    t: TVariableType;
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
        byte(t):=selectedrecord.valuetype;
        addresslist.selectedRecord.VarType:=t;

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
      if extra>=$1000 then
      begin
        address:=address+':'+TCustomType(customTypes[extra-$1000]).name;
      end
      else
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
      end;
    end;

    item.Caption:=address;
    item.subitems.add(value);
  except
    showmessage(inttostr(item.index));
  end;
end;

procedure TMainForm.UpdateFoundlisttimerTimer(Sender: TObject);
begin
  if foundlist<>nil then
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


procedure TMainForm.lblcompareToSavedScanClick(Sender: TObject);
begin

end;

procedure TMainForm.Label58Click(Sender: TObject);
begin

end;

procedure TMainForm.Label59Click(Sender: TObject);
var l: tstringlist;
begin
  l:=tstringlist.create;
  getaoblist('11 22 33', l);

  showmessage(l.text);


end;


procedure ChangeIcon(hModule: HModule; restype: PChar; resname: PChar;
  lparam: thandle); stdcall;
begin


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
    a: string;
    p: string;


    attachlist: tstringlist;
begin
  if (not formsettings.cbAlwaysAutoAttach.checked) and ((processhandle<>0) or (processid<>0)) then
    exit;

  attachlist:=tstringlist.create;
  try
    attachlist.AddStrings(autoattachlist);
    attachlist.AddStrings(extraautoattachlist);

    if attachlist.Count>0 then
    begin
      //in case there is no processwatcher this timer will be used to enumare the processlist every 2 seconds


      pl:=tstringlist.Create;
      getprocesslist(pl);

      try

        for i:=0 to attachlist.Count-1 do
        begin
          a:=uppercase(trim(attachlist.Strings[i]));
          for j:=pl.Count-1 downto 0 do //can't do indexof
          begin
            p:=uppercase(pl.strings[j]);
            if pos(a,p)=10 then
            begin
              //the process is found
              p:='$'+copy(p,1,8);
              val(p,newPID,k);
              if processid=newPID then exit; //already attached to this one

              ProcessHandler.processid:=newPID;
              unpause;
              DetachIfPossible;

              MainForm.ProcessLabel.caption:=pl.strings[j];
              Open_Process;
              enablegui(false);

              openProcessEpilogue('',0,0,true);

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


  finally
    attachlist.free;
  end;
end;

procedure TMainForm.AutoAttachTimerTimer(Sender: TObject);
begin
  autoattachcheck;
end;



procedure TMainForm.Button2Click(Sender: TObject);
var
  svalue2: string;
  percentage: boolean;
  fastscanmethod: TFastscanmethod;
begin

  foundlist.Deinitialize; //unlock file handles



  if cbpercentage<>nil then
    percentage:=cbpercentage.checked
  else
    percentage:=false;


  if button2.tag=0 then
  begin
    if ScanTabList<>nil then
      ScanTabList.Enabled:=false;

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



    memscan.alignment:=strtoint('$'+edtAlignment.text);
    if rbFsmAligned.checked then
      fastscanmethod:=fsmAligned
    else
      fastscanmethod:=fsmLastDigits;

    memscan.firstscan(GetScanType2, getVarType2, roundingtype, scanvalue.text, svalue2, scanStart, scanStop, fastscan, scanreadonly, HexadecimalCheckbox.checked, rbdec.checked, cbunicode.checked, cbCaseSensitive.checked, percentage, fastscanmethod, length(edtAlignment.text), TCustomType(vartype.items.objects[vartype.itemindex]));

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
  if ScanTabList<>nil then
    ScanTabList.enabled:=true;

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



  foundlist.Initialize(getvartype,memscan.Getbinarysize,hexadecimalcheckbox.checked,formsettings.cbShowAsSigned.Checked,not rbBit.checked,cbunicode.checked, TCustomType(VarType.items.objects[vartype.ItemIndex]));

  if memscan.lastscantype=stFirstScan then
  begin
    //firstscan Epilogue
    setGbScanOptionsEnabled(false);

    vartype.Enabled:=false;
    nextscanbutton.enabled:=true;
    newscan.Caption:=strNewScan;
  end;

  beep; //let the blind user know the scan has finished (See, I'm thinking about the visually impeared users...)

  progressbar1.Position:=0;
  UpdateFoundlisttimer.Enabled:=true;

  Scantype.ItemIndex:=lastscantype;
  UpdateScanType;

  if cbpercentage<>nil then
    cbPercentageOnChange(cbpercentage);


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
  estimateddiskspaceneeded: qword;
  diskspacefree, totaldiskspace: int64;
  totaldiskspacefree: LARGE_INTEGER;
  percentage: boolean;
begin
  estimateddiskspaceneeded:=foundcount*8*3;
  GetDiskFreeSpaceEx(pchar(memscan.ScanresultFolder), diskspacefree, totaldiskspace,@totaldiskspacefree);

  if estimateddiskspaceneeded>diskspacefree then
    if MessageDlg('You are low on diskspace on the folder where the scanresults are stored. Scanning might fail. Are you sure you want to continue?',mtwarning,[mbyes,mbno],0)<>mryes then exit;

  if cbpercentage<>nil then
    percentage:=cbPercentage.checked
  else
    percentage:=false;


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

  memscan.nextscan(GetScanType2, roundingtype, scanvalue.text, svalue2, scanStart, scanStop, scanreadonly, HexadecimalCheckbox.checked, rbdec.checked, cbunicode.checked, cbCaseSensitive.checked, percentage, compareToSavedScan, currentlySelectedSavedResultname);
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
    end;
    foundlist.Initialize(vtype,i,hexadecimalcheckbox.checked,formsettings.cbShowAsSigned.Checked,not rbBit.checked,cbunicode.checked,memscan.CustomType);
  end
  else foundlist.Initialize(vtype,memscan.CustomType); //failed scan, just reopen the addressfile




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


procedure TMainForm.ScanTypeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin


end;

procedure TMainForm.FormDestroy(Sender: TObject);
var i: integer;
    oldscanstate: PScanState;
begin

  saveformposition(self,[
                        addresslist.headers.Sections[0].Width,
                        addresslist.headers.Sections[1].Width,
                        addresslist.headers.Sections[2].Width,
                        addresslist.headers.Sections[3].Width,
                        addresslist.headers.Sections[4].Width,
                        panel5.height,
                        foundlist3.columns[0].width
                        ]);


  if foundlist<>nil then
    foundlist.Deinitialize;

  if addresslist<>nil then
    freeandnil(addresslist);

  if scantablist=nil then
    if memscan<>nil then
      freeandnil(memscan);

  if scantablist<>nil then
  begin
    for i:=0 to scantablist.Count-1 do
    begin
      if scantablist.SelectedTab<>i then
      begin
        oldscanstate:=scantablist.TabData[i];
        oldscanstate.foundlist.free;
        oldscanstate.memscan.free;
        freemem(oldscanstate);
      end;
    end;
    freeandnil(scantablist);
  end;

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
      currentmi.data:=pointer(ptrUint(PProcessListInfo(sl.Objects[i])^.processid));
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
      pid:=dword(ptrUint(TMenuItemExtra(sender).data)); //the menuitem .data field contains the processid (and not some allocated memory)

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

end;

procedure TMainForm.File1Click(Sender: TObject);
begin
  menu.Images := imagelist1;

  miSaveScanresults.Enabled:=memscan.nextscanCount>0;
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
  DecimalSeparator:='.';
  ThousandSeparator:=',';


  {$i MainUnit.lrs}

end.

