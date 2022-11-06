unit formsettingsunit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport, macexceptiondebuggerinterface,
  {$endif}
  {$ifdef windows}
  windows, win32proc,
  {$endif}LCLProc, LCLIntf, LCLType, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,registry, Menus,ComCtrls,CEFuncProc,ExtCtrls,{tlhelp32,}CheckLst,
  Buttons, LResources, frameHotkeyConfigUnit, math,

  KernelDebugger,plugin,NewKernelHandler,CEDebugger,hotkeyhandler, debugHelper,
  formhotkeyunit, debuggertypedefinitions, FileUtil, IniFiles, betterControls;


type Tpathspecifier=class(TObject)
  public
    path: string;
end;

type

  { TformSettings }

  TformSettings = class(TForm)
    askforreplacewithnops: TCheckBox;
    btnCancel: TButton;
    btnExcludeProcesses: TButton;
    btnOK: TButton;
    btnSetFont: TButton;
    btnSelectLanguage: TButton;
    btnMakeKernelDebugPossible: TButton;
    btnRestoreKernelProtection: TButton;
    cbAlwaysAutoAttach: TCheckBox;
    cbCanStepKernelcode: TCheckBox;
    cbCenterOnPopup: TCheckBox;
    cbDontOpenHandle: TCheckBox;
    cbDontusetempdir: TCheckBox;
    cbFastscan: TCheckBox;
    cbGlobalDebug: TCheckBox;
    cbHideAllWindows: TCheckBox;
    cbKDebug: TRadioButton;
    cbMemImage: TCheckBox;
    cbMemMapped: TCheckBox;
    cbMemPrivate: TCheckBox;
    cbOldPointerAddMethod: TCheckBox;
    cbOverrideExistingBPs: TCheckBox;
    cbPauseWhenScanningOnByDefault: TCheckBox;
    cbProcessIcons: TCheckBox;
    cbSaveWindowPos: TCheckBox;
    cbShowallWindows: TCheckBox;
    cbShowAsSigned: TCheckBox;
    cbShowMainMenu: TCheckBox;
    cbShowProcesslist: TCheckBox;
    cbShowUndo: TCheckBox;
    cbsimplecopypaste: TCheckBox;
    cbSkip_PAGE_NOCACHE: TCheckBox;
    cbUpdatefoundList: TCheckBox;
    cbUseVEHDebugger: TRadioButton;
    cbUseWindowsDebugger: TRadioButton;
    cbVEHRealContextOnThreadCreation: TCheckBox;
    cbWaitAfterGuiUpdate: TCheckBox;
    cbWriteLoggingOn: TCheckBox;
    CheckBox1: TCheckBox;
    cbOverrideDefaultFont: TCheckBox;
    cbDPIAware: TCheckBox;
    cbShowLanguageMenuItem: TCheckBox;
    cbProcessWatcherOpensHandles: TCheckBox;
    cbAlwaysSignTable: TCheckBox;
    cbAlwaysAskForPassword: TCheckBox;
    cbLuaPassiveGarbageCollection: TCheckBox;
    cbLuaGarbageCollectAll: TCheckBox;
    cbLuaOnlyCollectWhenLarger: TCheckBox;
    cbNeverChangeProtection: TCheckBox;
    cbAlwaysForceLoad: TCheckBox;
    cbAllocsAddToWatchedRegions: TCheckBox;
    cbSkip_PAGE_WRITECOMBINE: TCheckBox;
    cbUseThreadForFreeze: TCheckBox;
    cbAllByte: TCheckBox;
    cbAllWord: TCheckBox;
    cbAllDword: TCheckBox;
    cbAllQword: TCheckBox;
    cbAllSingle: TCheckBox;
    cbAllDouble: TCheckBox;
    cbAllCustom: TCheckBox;
    cbDisableDarkModeSupport: TCheckBox;
    cbDBVMDebugTriggerCOW: TCheckBox;
    cbDBVMDebugTargetedProcessOnly: TCheckBox;
    cbDBVMDebugKernelmodeBreaks: TCheckBox;
    cbSaveMemoryregionScanSettings: TCheckBox;
    cbSkipPDB: TCheckBox;
    cbUseIntelPT: TCheckBox;
    cbRecordIPTForFindWhatRoutines: TCheckBox;
    cbIPTTraceSize: TComboBox;
    cbHideIPTCapability: TCheckBox;
    combothreadpriority: TComboBox;
    defaultbuffer: TPopupMenu;
    Default1: TMenuItem;
    edtRepeatDelay: TEdit;
    edtLuaCollectTimer: TEdit;
    edtLuaMinCollectSize: TEdit;
    EditAutoAttach: TEdit;
    EditBufsize: TEdit;
    EditFreezeInterval: TEdit;
    editUpdatefoundInterval: TEdit;
    EditUpdateInterval: TEdit;
    edtStacksize: TEdit;
    edtTempScanFolder: TEdit;
    edtWriteLogSize: TEdit;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    gbUnexpectedExceptionHandling: TGroupBox;
    gbAllTypes: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label20: TLabel;
    Label25: TLabel;
    lblMaxIPTSize: TLabel;
    lblRepeatDelay: TLabel;
    lblCurrentLanguage: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label21: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblThreadFollowing: TLabel;
    lbLanguages: TListBox;
    LoadButton: TSpeedButton;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel9: TPanel;
    pcDebugConfig: TPageControl;
    pnlConfig: TPanel;
    miLanguages: TPopupMenu;
    miLuaExecAlways: TRadioButton;
    miLuaExecSignedOnly: TRadioButton;
    miLuaExecAsk: TRadioButton;
    miLuaExecNever: TRadioButton;
    miUnexpectedBreakpointsIgnore: TRadioButton;
    miUnexpectedBreakpointsBreak: TRadioButton;
    miUnexpectedBreakpointsBreakWhenInsideRegion: TRadioButton;
    cbUseDBVMDebugger: TRadioButton;
    rbMacDebugThreadLevel: TRadioButton;
    cbUseMacDebugger: TRadioButton;
    rbMacDebugTaskLevel: TRadioButton;
    rbDebugAsBreakpoint: TRadioButton;
    gbDebuggerInterface: TGroupBox;
    rbInt3AsBreakpoint: TRadioButton;
    rbPageExceptions: TRadioButton;
    rbVEHHookThreadCreation: TRadioButton;
    rbVEHPollThread: TRadioButton;
    rbVEHUseProcessWatcher: TRadioButton;
    replacewithnops: TCheckBox;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    ScrollBox3: TScrollBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    spbDown: TSpeedButton;
    spbUp: TSpeedButton;
    Languages: TTabSheet;
    TabSheet1: TTabSheet;
    tsMacDebuggerInterface: TTabSheet;
    tsLua: TTabSheet;
    tsSigning: TTabSheet;
    tsKernelDebugConfig: TTabSheet;
    tsVEHDebugConfig: TTabSheet;
    tsWindowsDebuggerConfig: TTabSheet;
    tvMenuSelection: TTreeView;
    pcSetting: TPageControl;
    GeneralSettings: TTabSheet;
    ScanSettings: TTabSheet;
    Plugins: TTabSheet;
    CodeFinder: TTabSheet;
    Assembler: TTabSheet;
    cbHandleBreakpoints: TCheckBox;
    Extra: TTabSheet;
    TauntOldOsUser: TLabel;
    GroupBox3: TGroupBox;
    cbKernelQueryMemoryRegion: TCheckBox;
    cbKernelReadWriteProcessMemory: TCheckBox;
    cbKernelOpenProcess: TCheckBox;
    cbProcessWatcher: TCheckBox;
    cbInjectDLLWithAPC: TCheckBox;
    CheckBox4: TCheckBox;
    tsHotkeys: TTabSheet;
    OpenDialog1: TOpenDialog;
    Unrandomizer: TTabSheet;
    Label5: TLabel;
    edtDefault: TEdit;
    cbIncremental: TCheckBox;
    Panel6: TPanel;
    AboutLabel: TLabel;
    frameHotkeyConfig: TframeHotkeyConfig;
    tsTools: TTabSheet;
    Panel2: TPanel;
    cbShowTools: TCheckBox;
    Panel3: TPanel;
    edtApplicationTool: TEdit;
    btnSetToolShortcut: TButton;
    Panel5: TPanel;
    Panel4: TPanel;
    btnToolNew: TButton;
    btnToolDelete: TButton;
    lvTools: TListView;
    lblApplicationTool: TLabel;
    lblShortcut: TLabel;
    lblShortcutText: TLabel;
    lblToolsName: TLabel;
    edtToolsName: TEdit;
    OpenButton: TSpeedButton;
    OpenDialog2: TOpenDialog;
    Panel7: TPanel;
    Button5: TButton;
    Button4: TButton;
    Panel8: TPanel;
    Label22: TLabel;
    clbPlugins: TCheckListBox;
    procedure btnMakeKernelDebugPossibleClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnRestoreKernelProtectionClick(Sender: TObject);
    procedure btnSetFontClick(Sender: TObject);
    procedure btnSelectLanguageClick(Sender: TObject);
    procedure cbAskIfTableHasLuascriptChange(Sender: TObject);
    procedure cbDontusetempdirChange(Sender: TObject);
    procedure cbDebuggerInterfaceChange(Sender: TObject);
    procedure cbKernelOpenProcessChange(Sender: TObject);
    procedure cbKernelQueryMemoryRegionChange(Sender: TObject);
    procedure cbOverrideDefaultFontChange(Sender: TObject);
    procedure cbProcessWatcherChange(Sender: TObject);
    procedure cbUseIntelPTChange(Sender: TObject);
    procedure cbWriteLoggingOnChange(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure cbRecordIPTForFindWhatRoutinesChange(Sender: TObject);
    procedure EditBufSizeKeyPress(Sender: TObject; var Key: Char);
    procedure Default1Click(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbShowDisassemblerClick(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure miUnexpectedBreakpointsOptionChange(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure Panel3Resize(Sender: TObject);
    procedure pcSettingChange(Sender: TObject);
    procedure rbInt3AsBreakpointChange(Sender: TObject);
    procedure replacewithnopsClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure cbUpdatefoundListClick(Sender: TObject);
    procedure AboutLabelClick(Sender: TObject);
    procedure cbHideAllWindowsClick(Sender: TObject);
    procedure btnExcludeProcessesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbKernelQueryMemoryRegionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ScrollBox1Click(Sender: TObject);
    procedure spbDownClick(Sender: TObject);
    procedure spbUpClick(Sender: TObject);
    procedure tvMenuSelectionChange(Sender: TObject; Node: TTreeNode);
    procedure Panel6Resize(Sender: TObject);
    procedure cbProcessIconsClick(Sender: TObject);
    procedure tvMenuSelectionCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure btnSetToolShortcutClick(Sender: TObject);
    procedure cbShowToolsClick(Sender: TObject);
    procedure btnToolNewClick(Sender: TObject);
    procedure lvToolsClick(Sender: TObject);
    procedure edtApplicationToolChange(Sender: TObject);
    procedure btnToolDeleteClick(Sender: TObject);
    procedure edtToolsNameChange(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
  private
    { Private declarations }
    hasBeenShown: boolean;

    tempstatePopupHide:word;
    temppopupmodifier:dword;
    tempstatePause:word;
    tempPausemodifier:dword;
    tempstateSpeedhack:word;
    tempSpeedhackmodifier:dword;


    tempmodulelist: pchar;
    tempmodulelistsize: integer;
    tempdenylist: boolean;
    tempdenylistglobal: boolean;

    deletedmodules: tstringlist;

    hasSetNewLanguage: boolean;
    newLanguage: string;

    unexpectedExceptionHandlerChanged: boolean;

    hasNoteAboutDebuggerInterfaces: boolean;

    procedure SetAssociations;
    procedure LanguageMenuItemClick(Sender: TObject);
    {$ifdef darwin}
    procedure FixUpsideDownTreeview;
    procedure FixUpsideDownTreeviewTimer(sender: TObject);
    {$endif}
  public
    { Public declarations }
    

    tempdonthidelist: array of string;
    temphideall: boolean;
    laststatePopupHide:word;
    lastpopupmodifier:dword;
    laststatePause:word;
    lastPausemodifier:dword;
    laststateSpeedhack:word;
    lastSpeedhackmodifier:dword;
    Loadingsettingsfromregistry: boolean;

    unrandomizersettings: record
                            defaultreturn: integer;
                            incremental: boolean;
                          end;

    procedure cleanupLanguageList;
    procedure setNoteAboutDebuggerInterfaces;
    procedure ScanForLanguages;

  published
    property SettingsTreeView: TTreeView read tvMenuSelection;   //just some stuff to make things look nicer. You're not required to use them
    property SettingsPageControl: TPageControl read pcSetting;

  end;

var
  formSettings: TformSettings;



  {$ifdef net}
  IsDebuggerPresentLocation: integer=0;
  {$endif}

implementation

uses
  aboutunit, MainUnit, MainUnit2, frmExcludeHideUnit, ModuleSafetyUnit,
  frmProcessWatcherUnit, CustomTypeHandler, processlist, commonTypeDefs,
  frmEditHistoryUnit, Globals, fontSaveLoadRegistry, CETranslator,
  MemoryBrowserFormUnit, DBK32functions, feces, UnexpectedExceptionsHelper,
  cpuidUnit, DPIHelper;


type TLanguageEntry=class
  foldername: string;
end;



procedure decimal(var key: char);
begin
  case key of
    chr(8)   : ;
    chr(16)  : ;
    '0'..'9' : ;
    else key:=chr(0);
  end;
end;


procedure TFormSettings.setNoteAboutDebuggerInterfaces;
begin
  if hasNoteAboutDebuggerInterfaces=false then
  begin
    gbDebuggerInterface.caption:=gbDebuggerInterface.caption+' '+ rsWontHaveAnyEffectUntilYouOpenANewProcess;
    hasNoteAboutDebuggerInterfaces:=true;
  end;
end;

procedure TFormSettings.SetAssociations; //obsolete, done from installer
begin

end;


resourcestring
  strProcessWatcherWillPreventUnloader='Enabling the process watcher will prevent the unloader from working';
  rsYouHavenTSelectedAnyMemoryTypeThisWillResultInChea = 'You haven''t selected any memory type. This will result in '+strCheatEngine+' finding NO memory! Are you stupid?';
  rsIsNotAValidInterval = '%s is not a valid interval';
  rsTheScanbufferSizeHasToBeGreaterThan0 = 'The scanbuffer size has to be greater than 0';
  rsTheValueForTheKeypollIntervalIsInvalid = 'the value for the keypoll interval (%s is invalid';
  rsTheValueForTheWaitBetweenHotkeyPressesIsInvalid = 'the value for the wait between hotkey presses (%s is invalid';
  rsPleaseBootWithUnsignedDriversAllowedF8DuringBootOr = 'Please boot with unsigned drivers allowed(F8 during boot), or sign the driver yourself';
  rsRequiresDBVM = '(Requires DBVM)';
  rsThisPluginIsAlreadyLoaded = 'This plugin is already loaded';
  rsIdle = 'Idle';
  rsLowest = 'Lowest';
  rsLower = 'Lower';
  rsNormal = 'Normal';
  rsHigher = 'Higher';
  rsHighest = 'Highest';
  rsTimeCritical = 'TimeCritical';
  rsGeneralSettings = 'General Settings';
  rsTools = 'Tools';
  rsHotkeys = 'Hotkeys';
  rsUnrandomizer = 'Unrandomizer';
  rsScanSettings = 'Scan Settings';
  rsPlugins = 'Plugins';
  rsLanguages = 'Languages';
  rsDebuggerOptions = 'Debugger Options';
  rsLuaOptions = 'Lua';
  rsExtra = 'Extra';
  rsSigning = 'Signing';
  rsNoName = 'No Name';
  rsAttachToForegroundProcess = 'Attach to current foreground process';
  rsPopupHideCheatEngine = 'Popup/Hide '+strCheatEngine;
  rsPauseTheSelectedProcess = 'Pause the selected process';
  rsToggleTheSpeedhack = 'Toggle the speedhack';
  rsSpeedhackSpeed = 'Speedhack speed';
  rsChangeTypeTo = 'Change type to';
  rsBinary = 'Binary';
  rsByte = 'Byte';
  rs2Bytes = '2 Bytes';
  rs4Bytes = '4 Bytes';
  rs8Bytes = '8 Bytes';
  rsFloat = 'Float';
  rsDouble = 'Double';
  rsText = 'Text';
  rsArrayOfByte = 'Array of byte';
  rsNextScan = 'Next Scan';
  rsToggleBetweenFirstLastScanCompare =
    'Toggle between first/last scan compare';
  rsUndoLastScan = 'Undo last scan';
  rsCancelTheCurrentScan = 'Cancel the current scan';
  rsDebugRun = 'Debug->Run';
  rsUnknownInitialValue = 'Unknown Initial Value';
  rsIncreasedValue = 'Increased Value';
  rsDecreasedValue = 'Decreased Value';
  rsChangedValue = 'Changed Value';
  rsUnchangedValue = 'Unchanged Value';
  rsNewLanguageSet = 'New language set';
  rsRestartCE = 'It is recommended to restart '+strCheatEngine+' for this change to take effect';
  rsFailureToOpenRegistry = 'Failure to open the registry entry';
  rsSpectreWarning = 'WARNING! Making kernelmode possible will slightly increase the speed of your system, BUT it will make you vulnerable to Spectre attacks'#13#10'Are you ok with this? (You can later re-enable this protection)';
  rsSpectreRestore = 'Your protection has been restored. Please restart your '
    +'system to make it take effect';
  rsFailMMRegistry = 'Failure getting the Memory Management registry key';
  rsSpectreRegistryChanged = 'The registry keys has been changed accordingly. '
    +' Reboot your system to make it take effect';
  rsAllCustomTypes = 'All Custom Types';


procedure TformSettings.btnOKClick(Sender: TObject);
var processhandle2: Thandle;
    reg: TRegistry;
    bufsize: integer;
    i,j,error: integer;
    ec:dword;
    found:boolean;

    networkupdateinterval,updateinterval,freezeinterval,FoundInterval: integer;
    collectgarbagetimer, collectgarbageminimumsize: integer;
    repeatDelay: integer;

    stacksize: integer;

    dllpath: Tpathspecifier;

    cpu: string;
    WriteLogSize: integer;
    s: string;

begin
  try
    {$ifdef cpu64}
    cpu:='64';
    {$else}
    cpu:='32';
    {$endif}

  {$ifndef net}

    {$ifdef windows}
    if cbProcessWatcher.checked and (frmprocesswatcher=nil) then
    begin
      loaddbk32;
      frmprocesswatcher:=tfrmprocesswatcher.Create(mainform); //start the process watcher
    end;
    {$endif}


  {$endif}

    if not ((cbMemPrivate.checked) or (cbMemImage.Checked) or (cbMemMapped.Checked)) then
      if messagedlg(rsYouHavenTSelectedAnyMemoryTypeThisWillResultInChea, mtWarning, [mbyes, mbno], 0)<>mryes then exit;

    WriteLogSize:=strtoint(edtWriteLogSize.text);

    val(edtStacksize.text, stacksize, error);
    if (error<>0) or (stacksize<=0) then raise exception.Create(Format(rsIsNotAValidInterval, [edtStacksize.text]));

    val(edtLuaCollectTimer.text, collectgarbagetimer, error);
    if (error<>0) or (collectgarbagetimer<=0) then raise exception.Create(Format(rsIsNotAValidInterval, [edtLuaCollectTimer.text]));

    val(edtLuaMinCollectSize.text, collectgarbageminimumsize, error);
    if (error<>0) or (collectgarbageminimumsize<=0) then raise exception.Create(Format(rsIsNotAValidInterval, [edtLuaMinCollectSize.text]));



    val(editUpdatefoundInterval.Text,foundinterval,error);
    if (error<>0) or (foundinterval<=0) then raise exception.Create(Format(rsIsNotAValidInterval, [editUpdatefoundInterval.Text]));

    val(editupdateinterval.text,updateinterval,error);
    if (error<>0) or (updateinterval<=0) then raise exception.Create(Format(rsIsNotAValidInterval, [editupdateinterval.text]));

    val(editfreezeinterval.text,freezeinterval,error);
    if (error<>0) or (updateinterval<=0) then raise exception.Create(Format(rsIsNotAValidInterval, [editfreezeinterval.text]));

    try bufsize:=StrToInt(editbufsize.text); except bufsize:=1024; end;

    if bufsize=0 then raise exception.create(rsTheScanbufferSizeHasToBeGreaterThan0);


    val(edtRepeatDelay.text,repeatDelay,error);
    if (error<>0) or (repeatDelay<0) then raise exception.Create(Format(rsIsNotAValidInterval, [edtRepeatDelay.text]));




    buffersize:=bufsize*1024;

    mainform.UndoScan.visible:={$ifdef net}false{$else}cbshowundo.checked{$endif};



    //save to the registry
    reg:=Tregistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\'+strCheatEngine,true) then
      begin
        //write the settings
        reg.WriteInteger('Saved Stacksize', stacksize);

        reg.writebool('Show processlist in mainmenu', cbShowProcesslist.checked);
        mainform.Process1.Visible:=cbShowProcesslist.checked;

        reg.WriteBool('Disable DarkMode Support', cbDisableDarkModeSupport.checked);
        reg.WriteBool('Undo',cbshowundo.checked);
        reg.WriteInteger('ScanThreadpriority',combothreadpriority.itemindex);
        case combothreadpriority.itemindex of
          0: scanpriority:=tpIdle;
          1: scanpriority:=tpLowest;
          2: scanpriority:=tpLower;
          3: scanpriority:=tpLower;
          4: scanpriority:=tpNormal;
          5: scanpriority:=tpHigher;
          6: scanpriority:=tpHighest;
          7: scanpriority:=tpTimeCritical;
        end;






        reg.WriteBool('Show all windows on taskbar',cbShowallWindows.checked);
        if cbShowallWindows.checked then
          Application.TaskBarBehavior:=tbMultiButton
        else
          Application.TaskBarBehavior:=tbSingleButton;

        ScanAllTypes:=[];
        if cbAllByte.checked then ScanAllTypes:=ScanAllTypes+[vtByte];
        if cbAllWord.checked then ScanAllTypes:=ScanAllTypes+[vtWord];
        if cbAllDword.checked then ScanAllTypes:=ScanAllTypes+[vtDword];
        if cbAllQword.checked then ScanAllTypes:=ScanAllTypes+[vtQword];
        if cbAllSingle.checked then ScanAllTypes:=ScanAllTypes+[vtSingle];
        if cbAllDouble.checked then ScanAllTypes:=ScanAllTypes+[vtDouble];
        if cbAllCustom.checked then ScanAllTypes:=ScanAllTypes+[vtCustom];

        reg.writebool('AllByte',cbAllByte.checked);
        reg.writebool('AllWord',cbAllWord.checked);
        reg.writebool('AllDWord',cbAllDword.checked);
        reg.writebool('AllQWord',cbAllQword.checked);
        reg.writebool('AllFloat',cbAllSingle.checked);
        reg.writebool('AllDouble',cbAllDouble.checked);
        reg.writebool('AllCustom',cbAllCustom.checked);


        reg.writebool('Can Step Kernelcode',cbCanStepKernelcode.checked);

        reg.WriteInteger('Buffersize',bufsize);
        reg.WriteBool('Center on popup',cbCenterOnPopup.checked);
        reg.WriteInteger('Update interval',updateinterval);
        reg.WriteInteger('Freeze interval',freezeinterval);
        reg.writebool('Show values as signed',cbShowAsSigned.checked);

        reg.WriteBool('Skip PDB', cbSkipPDB.checked);
        skippdb:=cbSkipPDB.checked;

        reg.WriteBool('Use Intel PT For Debug', cbUseIntelPT.Checked);
        useintelptfordebug:=cbUseIntelPT.Checked;

        reg.writebool('Hide IPT Capability', cbHideIPTCapability.checked);
        hideiptcapability:=cbHideIPTCapability.checked;

        reg.WriteBool('Log IPT buffers inside FindWhat results', cbRecordIPTForFindWhatRoutines.Checked);
        inteliptlogfindwhatroutines:=cbRecordIPTForFindWhatRoutines.Checked;

        reg.writeInteger('Max IPT Size', cbIPTTraceSize.ItemIndex);
        maxiptconfigsize:=cbIPTTraceSize.ItemIndex;

        reg.WriteBool('Replace incomplete opcodes with NOPS',replacewithnops.checked);
        reg.WriteBool('Ask for replace with NOPS',askforreplacewithnops.checked);
        reg.WriteBool('Use Anti-debugdetection',checkbox1.checked);
        reg.WriteBool('Override existing bp''s',cbOverrideExistingBPs.checked);
        BPOverride:=cbOverrideExistingBPs.checked;


        reg.WriteBool('Handle unhandled breakpoints',cbhandlebreakpoints.Checked);
        reg.WriteBool('Fastscan on by default',cbFastscan.checked);

        reg.WriteBool('Hardware breakpoints', rbDebugAsBreakpoint.checked);
        reg.WriteBool('Software breakpoints', rbInt3AsBreakpoint.checked);
        reg.Writebool('Exception breakpoints', rbPageExceptions.checked);

        reg.WriteBool('Update Foundaddress list',cbUpdatefoundList.checked);
        reg.WriteInteger('Update Foundaddress list Interval',foundinterval);

        reg.WriteBool('Simple copy/paste',cbsimplecopypaste.checked);
        reg.WriteString('AutoAttach',EditAutoAttach.text);
        reg.writebool('Always AutoAttach', cbAlwaysAutoAttach.checked);

        i:=1;
        if miLuaExecAlways.checked then i:=0 else
        if miLuaExecSignedOnly.checked then i:=1 else
        if miLuaExecAsk.checked then i:=2 else
        if miLuaExecNever.checked then i:=3;

        reg.WriteInteger('LuaScriptAction', i);


        {$ifndef net}
        mainform.UpdateFoundlisttimer.Interval:=foundinterval;
        {$endif}

        reg.WriteBool('Save window positions',cbSaveWindowPos.checked);
        reg.WriteBool('Show main menu',cbShowMainMenu.Checked);
        reg.WriteBool('Get process icons',cbProcessIcons.Checked);
        GetProcessIcons:=cbProcessIcons.Checked;

        reg.WriteBool('Pointer appending', cbOldPointerAddMethod.checked);

        reg.writebool('skip PAGE_NOCACHE',cbSkip_PAGE_NOCACHE.Checked);
        reg.writebool('skip PAGE_WRITECOMBINE',cbSkip_PAGE_WRITECOMBINE.Checked);
        reg.writebool('Pause when scanning on by default',cbPauseWhenScanningOnByDefault.Checked);
        reg.writebool('Save memoryregion scansettings', cbSaveMemoryregionScanSettings.checked);




        reg.WriteInteger('Repeat Delay',repeatDelay);
        Globals.repeatDelay:=repeatDelay;


        reg.WriteBool('Hide all windows',cbHideAllWindows.checked);
        reg.WriteBool('Really hide all windows',temphideall);


        //save donthidelist
        {$ifndef net}
        setlength(donthidelist,length(tempdonthidelist));
        for i:=0 to length(tempdonthidelist)-1 do
        begin
          donthidelist[i]:=tempdonthidelist[i];
          reg.writestring('Do not hide '+IntToStr(i),tempdonthidelist[i]);
        end;

        //end
        reg.writestring('Do not hide '+IntToStr(length(tempdonthidelist)),'');
        reg.WriteBool('MEM_PRIVATE',cbMemPrivate.checked);
        reg.WriteBool('MEM_IMAGE',cbMemImage.Checked);
        reg.WriteBool('MEM_MAPPED',cbMemMapped.Checked);
        onlyfront:=not temphideall;




        //check the module list

        if frmModuleSafety<>nil then //modified
        begin
          if modulelist<>nil then
            freememandnil(modulelist);

          modulelist:=tempmodulelist;
          modulelistsize:=tempmodulelistsize;
          tempmodulelist:=nil;
          denylist:=tempdenylist;
          denylistglobal:=tempdenylistglobal;

          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Module List',{$ifndef windows}bintohexs({$endif}ModuleList^,modulelistsize){$ifndef windows}){$endif};

          reg.writeInteger('modulelistsize',modulelistsize);
          reg.WriteBool('Global Denylist',DenyListGlobal);
          reg.WriteBool('ModuleList as Denylist',DenyList);
        end;


        try
          reg.WriteInteger('hotkey poll interval',strtoint(frameHotkeyConfig.edtKeypollInterval.text));
          hotkeyPollInterval:=strtoint(frameHotkeyConfig.edtKeypollInterval.text);
        except
          raise exception.Create(Format(rsTheValueForTheKeypollIntervalIsInvalid, [frameHotkeyConfig.edtKeypollInterval.text]));
        end;

        try
          reg.WriteInteger('Time between hotkeypress',strtoint(frameHotkeyConfig.edtHotkeyDelay.text));
          hotkeyIdletime:=strtoint(frameHotkeyConfig.edtHotkeyDelay.text);
        except
          raise exception.Create(Format(rsTheValueForTheWaitBetweenHotkeyPressesIsInvalid, [frameHotkeyConfig.edtHotkeyDelay.text]));
        end;






          //save the hotkeylist
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Attach to foregroundprocess Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[0][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Show '+strCheatEngine+' Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[1][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Pause process Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[2][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Toggle speedhack Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[3][0],10){$ifndef windows}){$endif};

          reg.{$ifdef windows}WriteFloat{$else}WriteString{$endif}('Speedhack 1 speed',{$ifndef windows}FloatToStr({$endif}frameHotkeyConfig.newspeedhackspeed1.speed{$ifndef windows}){$endif});
          reg.WriteBool('Speedhack 1 disablewhenreleased',frameHotkeyConfig.newspeedhackspeed1.disablewhenreleased);
          reg.{$ifdef windows}WriteFloat{$else}WriteString{$endif}('Speedhack 2 speed',{$ifndef windows}FloatToStr({$endif}frameHotkeyConfig.newspeedhackspeed2.speed{$ifndef windows}){$endif});
          reg.WriteBool('Speedhack 2 disablewhenreleased',frameHotkeyConfig.newspeedhackspeed2.disablewhenreleased);
          reg.{$ifdef windows}WriteFloat{$else}WriteString{$endif}('Speedhack 3 speed',{$ifndef windows}FloatToStr({$endif}frameHotkeyConfig.newspeedhackspeed3.speed{$ifndef windows}){$endif});
          reg.WriteBool('Speedhack 3 disablewhenreleased',frameHotkeyConfig.newspeedhackspeed3.disablewhenreleased);
          reg.{$ifdef windows}WriteFloat{$else}WriteString{$endif}('Speedhack 4 speed',{$ifndef windows}FloatToStr({$endif}frameHotkeyConfig.newspeedhackspeed4.speed{$ifndef windows}){$endif});
          reg.WriteBool('Speedhack 4 disablewhenreleased',frameHotkeyConfig.newspeedhackspeed4.disablewhenreleased);
          reg.{$ifdef windows}WriteFloat{$else}WriteString{$endif}('Speedhack 5 speed',{$ifndef windows}FloatToStr({$endif}frameHotkeyConfig.newspeedhackspeed5.speed{$ifndef windows}){$endif});
          reg.WriteBool('Speedhack 5 disablewhenreleased',frameHotkeyConfig.newspeedhackspeed5.disablewhenreleased);

          mainunit2.speedhackspeed1:=frameHotkeyConfig.newspeedhackspeed1;
          mainunit2.speedhackspeed2:=frameHotkeyConfig.newspeedhackspeed2;
          mainunit2.speedhackspeed3:=frameHotkeyConfig.newspeedhackspeed3;
          mainunit2.speedhackspeed4:=frameHotkeyConfig.newspeedhackspeed4;
          mainunit2.speedhackspeed5:=frameHotkeyConfig.newspeedhackspeed5;

          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Set Speedhack speed 1 Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[4][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Set Speedhack speed 2 Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[5][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Set Speedhack speed 3 Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[6][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Set Speedhack speed 4 Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[7][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Set Speedhack speed 5 Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[8][0],10){$ifndef windows}){$endif};



          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Increase Speedhack speed',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[9][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteFloat{$else}WriteString{$endif}('Increase Speedhack delta',{$ifndef windows}FloatToStr({$endif}frameHotkeyConfig.speedupdelta{$ifndef windows}){$endif});

          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Decrease Speedhack speed',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[10][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteFloat{$else}WriteString{$endif}('Decrease Speedhack delta',{$ifndef windows}FloatToStr({$endif}frameHotkeyConfig.slowdowndelta{$ifndef windows}){$endif});

          mainunit2.speedupdelta:=frameHotkeyConfig.speedupdelta;
          mainunit2.slowdowndelta:=frameHotkeyConfig.slowdowndelta;

          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Binary Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[11][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Byte Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[12][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('2 Bytes Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[13][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('4 Bytes Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[14][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('8 Bytes Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[15][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Float Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[16][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Double Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[17][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Text Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[18][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Array of Byte Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[19][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('New Scan Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[20][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('New Scan-Exact Value',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[21][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Unknown Initial Value Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[22][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Next Scan-Exact Value',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[23][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Increased Value Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[24][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Decreased Value Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[25][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Changed Value Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[26][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Unchanged Value Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[27][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Same as first scan Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[28][0],10){$ifndef windows}){$endif};

          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Undo Last scan Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[29][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Cancel scan Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[30][0],10){$ifndef windows}){$endif};
          reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('Debug->Run Hotkey',{$ifndef windows}bintohexs({$endif}frameHotkeyConfig.newhotkeys[31][0],10){$ifndef windows}){$endif};


          //apply these hotkey changes
          for i:=0 to cehotkeycount-1 do
          begin
            found:=false;

            for j:=0 to length(hotkeythread.hotkeylist)-1 do
            begin
              if (hotkeythread.hotkeylist[j].id=i) and (hotkeythread.hotkeylist[j].handler2) then
              begin
                //found it
                hotkeythread.hotkeylist[j].keys:=frameHotkeyConfig.newhotkeys[i];
                found:=true;
                break;
              end;
            end;

            if not found then //add it
            begin
              j:=length(hotkeythread.hotkeylist);
              setlength(hotkeythread.hotkeylist,j+1);
              hotkeythread.hotkeylist[j].keys:=frameHotkeyConfig.newhotkeys[i];
              hotkeythread.hotkeylist[j].windowtonotify:=mainform.Handle;
              hotkeythread.hotkeylist[j].id:=i;
              hotkeythread.hotkeylist[j].handler2:=true;
            end;

            checkkeycombo(frameHotkeyConfig.newhotkeys[i]);
          end;





        {$endif}
        dontusetempdir:=cbDontusetempdir.checked;
        tempdiralternative:=trim(edtTempScanFolder.text);

        tempdiralternative:=IncludeTrailingPathDelimiter(tempdiralternative);


        reg.WriteBool('Don''t use tempdir',dontusetempdir);
        reg.WriteString('Scanfolder',tempdiralternative);


        reg.WriteBool('Use dbk32 QueryMemoryRegionEx',cbKernelQueryMemoryRegion.checked);
        reg.WriteBool('Use dbk32 ReadWriteProcessMemory',cbKernelReadWriteProcessMemory.checked);
        reg.WriteBool('Use dbk32 OpenProcess',cbKernelOpenProcess.checked);

        reg.WriteBool('Use Processwatcher',cbProcessWatcher.checked);
        reg.WriteBool('Use VEH Debugger',cbUseVEHDebugger.checked);
        reg.WriteBool('VEH Real context on thread creation event', cbVEHRealContextOnThreadCreation.checked);
        VEHRealContextOnThreadCreation:=cbVEHRealContextOnThreadCreation.checked;


        reg.WriteBool('Use Windows Debugger',cbUseWindowsDebugger.checked);
        reg.WriteBool('Use Kernel Debugger',cbKdebug.checked);
        reg.WriteBool('Use Global Debug Routines',cbGlobalDebug.checked);
        reg.WriteBool('Use DBVM Debugger', cbUseDBVMDebugger.checked);

        reg.writeBool('DBVMBP Trigger COW', cbDBVMDebugTriggerCOW.checked);
        reg.writeBool('DBVMBP This Process Only', cbDBVMDebugTargetedProcessOnly.checked);
        reg.writeBool('DBVMBP Kernelmode', cbDBVMDebugKernelmodeBreaks.checked);

        dbvmbp_options.TriggerCOW:=cbDBVMDebugTriggerCOW.checked;
        dbvmbp_options.TargetedProcessOnly:=cbDBVMDebugTargetedProcessOnly.checked;
        dbvmbp_options.KernelmodeBreaks:=cbDBVMDebugKernelmodeBreaks.checked;



        waitafterguiupdate:=cbWaitAfterGuiUpdate.checked;
        reg.WriteBool('Wait After Gui Update', waitafterguiupdate);

        {$ifdef darwin}
        reg.WriteBool('Use TaskLevel debugger', rbMacDebugTaskLevel.checked);

        useTaskLevelDebug:=rbMacDebugTaskLevel.checked;
        {$endif}


        if miUnexpectedBreakpointsIgnore.checked then i:=0;
        if miUnexpectedBreakpointsBreak.checked then i:=1;
        if miUnexpectedBreakpointsBreakWhenInsideRegion.checked then i:=2;


        reg.WriteInteger('Unexpected Breakpoint Behaviour', i);

        if unexpectedExceptionHandlerChanged then //apply it
        begin
          case i of
            0: UnexpectedExceptionAction:=ueaIgnore;
            1: UnexpectedExceptionAction:=ueaBreak;
            2: UnexpectedExceptionAction:=ueaBreakIfInRegion;
          end;
        end;



        if (reg.ValueExists('Add Allocated Memory As Watched')=false) or (reg.ReadBool('Add Allocated Memory As Watched')<>cbAllocsAddToWatchedRegions.checked) then
          allocsAddToUnexpectedExceptionList:=cbAllocsAddToWatchedRegions.Checked;

        reg.WriteBool('Add Allocated Memory As Watched', cbAllocsAddToWatchedRegions.checked);



        unrandomizersettings.defaultreturn:=strtoint(edtdefault.Text);
        unrandomizersettings.incremental:=cbincremental.Checked;
        reg.WriteInteger('Unrandomizer: default value',unrandomizersettings.defaultreturn);
        reg.WriteBool('Unrandomizer: incremental',unrandomizersettings.incremental);

        reg.writebool('Show tools menu', cbShowTools.checked);
        mainform.ools1.Visible:=cbShowTools.checked;

        reg.writebool('WriteLogging', cbWriteLoggingOn.checked);
        reg.WriteInteger('WriteLoggingSize', WriteLogSize);

        logWrites:=cbWriteLoggingOn.checked;
        setMaxWriteLogSize(writelogsize);

        reg.WriteBool('Show Language MenuItem', cbShowLanguageMenuItem.checked);
        MainForm.miLanguages.visible:=cbShowLanguageMenuItem.checked and (lbLanguages.Count>1);


        reg.WriteBool('DPI Aware', cbDPIAware.Checked);
        reg.writebool('Override Default Font', cbOverrideDefaultFont.Checked);

        reg.writebool('Never Change Protection', cbNeverChangeProtection.checked);
        SkipVirtualProtectEx:=cbNeverChangeProtection.checked;

        reg.writebool('Always Force Load', cbAlwaysForceLoad.checked);
        alwaysforceload:=cbAlwaysForceLoad.checked;

        {$ifdef privatebuild}
        reg.WriteBool('DoNotOpenProcessHandles', cbDontOpenHandle.Checked);
        DoNotOpenProcessHandles:=cbDontOpenHandle.Checked;

        reg.WriteBool('ProcessWatcherOpensHandles', cbProcessWatcherOpensHandles.Checked);
        ProcessWatcherOpensHandles:=cbProcessWatcherOpensHandles.Checked;

        reg.WriteBool('ProcessWatcherOpensHandles', cbProcessWatcherOpensHandles.Checked);

        reg.WriteBool('useapctoinjectdll', cbInjectDLLWithAPC.Checked);
        useapctoinjectdll:=cbInjectDLLWithAPC.checked;
        {$else}
        useapctoinjectdll:=false;
        {$endif}

        reg.WriteBool('Always Sign Table', cbAlwaysSignTable.Checked);
        reg.WriteBool('Always Ask For Password', cbAlwaysAskForPassword.Checked);

        reg.WriteBool('collectgarbage passive', cbLuaPassiveGarbageCollection.checked);
        reg.WriteBool('collectgarbage active', cbLuaGarbageCollectAll.checked);
        reg.WriteInteger('collectgarbage timer', collectgarbagetimer);
        reg.WriteBool('collectgarbage only when bigger', cbLuaOnlyCollectWhenLarger.checked);
        reg.WriteInteger('collectgarbage minsize', collectgarbageminimumsize);

        reg.WriteBool('use thread to freeze', cbUseThreadForFreeze.checked);

        mainform.FreezeTimer.Interval:=freezeinterval;
        mainForm.UseThreadToFreeze:=cbUseThreadForFreeze.checked;

        if cbOverrideDefaultFont.checked then
        begin
          if reg.OpenKey('\Software\'+strCheatEngine+'\Font', true) then
            SaveFontToRegistry(fontdialog1.Font, reg);
        end;


      end
      else
        messagedlg(rsFailureToOpenRegistry, mtError, [mbok], 0);


      if cbLuaGarbageCollectAll.checked then
      begin
        mainform.tLuaGCActive.interval:=collectgarbagetimer*1000;

        if cbLuaOnlyCollectWhenLarger.checked then
          luagc_MinSize:=collectgarbageminimumsize
        else
          luagc_MinSize:=0;
      end;
      mainform.tLuaGCActive.enabled:=cbLuaGarbageCollectAll.checked;
      mainform.tLuaGCPassive.enabled:=cbLuaPassiveGarbageCollection.checked;

  {$ifndef net}

      //save the tools hotkeys
      reg.DeleteKey('\Software\'+strCheatEngine+'\Tools');
      if Reg.OpenKey('\Software\'+strCheatEngine+'\Tools',true) then
      begin
        for i:=0 to lvTools.Items.Count-1 do
        begin
          reg.WriteString(format('%.8x A',[i]),lvTools.Items[i].caption);
          reg.WriteString(format('%.8x B',[i]),lvTools.Items[i].subitems[0]);
          reg.WriteInteger(format('%.8x C',[i]),ptrUint(lvTools.Items[i].data));
        end;
      end;
      UpdateToolsMenu;

      for i:=0 to deletedmodules.Count-1 do
      begin
        j:=pluginhandler.GetPluginID(deletedmodules[i]);
        if j<>-1 then
          pluginhandler.DisablePlugin(j);
      end;

      //save the plugins
      reg.DeleteKey('\Software\'+strCheatEngine+'\Plugins'+cpu);
      if Reg.OpenKey('\Software\'+strCheatEngine+'\Plugins'+cpu,true) then
      begin
        for i:=0 to clbplugins.Count-1 do
        begin
          dllpath:=Tpathspecifier(clbplugins.Items.Objects[i]);

          reg.WriteString(format('%.8x A',[i]),dllpath.path);
          reg.WriteBool(format('%.8x B',[i]),clbplugins.Checked[i]);
        end;
      end;



      for i:=0 to clbplugins.Count-1 do
      begin
        dllpath:=Tpathspecifier(clbplugins.Items.Objects[i]);
        if dllpath<>nil then
        begin

          j:=pluginhandler.GetPluginID(dllpath.path);

          if j=-1 then //not loaded yet
            j:=pluginhandler.LoadPlugin(dllpath.path);

          if clbplugins.Checked[i] then
          begin
            //at least load it if it is loadable

            pluginhandler.EnablePlugin(j);
          end
          else
            pluginhandler.DisablePlugin(j);
        end;
      end;
  {$endif}



    finally
      reg.CloseKey;
      reg.free;
    end;



    SetAssociations;


    {$ifndef net}
    mainform.UpdateTimer.Interval:=updateinterval;
    {$else}
    mainform.UpdateTimer.Interval:=networkupdateinterval;
    {$endif}

    savedStackSize:=stacksize;

    Skip_PAGE_NOCACHE:=cbSkip_PAGE_NOCACHE.Checked;
    Skip_PAGE_WRITECOMBINE:=cbSkip_PAGE_WRITECOMBINE.checked;

    {$ifndef net}
    Scan_MEM_PRIVATE:=cbMemPrivate.checked;
    Scan_MEM_IMAGE:=cbMemImage.Checked;
    Scan_MEM_MAPPED:=cbMemMapped.Checked;
    {$endif}


    if rbDebugAsBreakpoint.checked then
      preferedBreakpointMethod:=bpmDebugRegister
    else
    if rbInt3AsBreakpoint.checked then
      preferedBreakpointMethod:=bpmInt3
    else
    if rbPageExceptions.checked then
      preferedBreakpointMethod:=bpmException;

    laststatePopupHide:=tempstatepopuphide;
    lastpopupmodifier:=temppopupmodifier;
    laststatePause:=tempstatepause;
    lastPausemodifier:=temppausemodifier;
    laststateSpeedhack:=tempstatespeedhack;
    lastSpeedhackmodifier:=tempspeedhackmodifier;

    mainform.autoattachlist.DelimitedText:=formsettings.EditAutoAttach.Text;

    if cbShowMainMenu.Checked then
      mainform.Menu:=mainform.MainMenu1
    else
      mainform.Menu:=nil;

    modalresult:=mrok;

  except
    on e: exception do
      MessageDlg(e.message, mtError,[mbOK],0);
  end;

end;

procedure TformSettings.btnMakeKernelDebugPossibleClick(Sender: TObject);
var reg: TRegistry;
begin
  if messagedlg(rsSpectreWarning,mtWarning,[mbYes,mbNo],0,mbNo)=mrYes then
  begin
    reg:=tregistry.create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey('\SYSTEM\CurrentControlSet\Control\Session Manager\Memory Management',false) then
      begin
        reg.WriteInteger('FeatureSettingsOverride',3);
        reg.WriteInteger('FeatureSettingsOverrideMask',3);

        messagedlg(rsSpectreRegistryChanged, mtInformation, [mbOK], 0);
      end
      else
        messagedlg(rsFailMMRegistry, mtError, [mbok], 0);

    finally
      reg.free;
    end;

  end;
end;

procedure TformSettings.btnCancelClick(Sender: TObject);
begin

end;

procedure TformSettings.btnRestoreKernelProtectionClick(Sender: TObject);
var reg: TRegistry;
begin
  reg:=tregistry.create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Control\Session Manager\Memory Management',false) then
    begin
      if reg.ValueExists('FeatureSettingsOverride') then
        reg.DeleteValue('FeatureSettingsOverride');

      if reg.ValueExists('FeatureSettingsOverrideMask') then
        reg.DeleteValue('FeatureSettingsOverrideMask');

      messagedlg(rsSpectreRestore, mtInformation, [mbok], 0);
    end
    else
      messagedlg(rsFailMMRegistry, mtError, [mbok], 0);
  finally
    reg.free;
  end;
end;

procedure TformSettings.btnSetFontClick(Sender: TObject);
begin
  if fontdialog1.Execute then
  begin
    cbOverrideDefaultFont.Font.assign(fontdialog1.Font);
    btnSetFont.Font.assign(fontdialog1.Font);
  end;
end;

procedure TformSettings.btnSelectLanguageClick(Sender: TObject);
var
  l: TLanguageEntry;
  preferedLanguage: string;
  ini: TIniFile;
  old: string;

  settingsvis: boolean;
  index: integer;
begin
  index:=lbLanguages.ItemIndex;

  if index<>-1 then
  begin
    l:=TLanguageEntry(lbLanguages.Items.Objects[index]);
    if l<>nil then
      preferedLanguage:=l.foldername
    else
      preferedLanguage:='*';

    try
      ini:=TIniFile.Create(cheatenginedir+{$ifdef darwin}'..'+DirectorySeparator+{$endif}'Languages' + DirectorySeparator+'language.ini');
      try
        old:=ini.ReadString('Language','PreferedLanguage','');
        ini.WriteString('Language','PreferedLanguage',preferedLanguage);
        hasSetNewLanguage:=true;
        newLanguage:=preferedLanguage;

        ScanForLanguages;

        doTranslation;

        if uppercase(old)<>uppercase(preferedLanguage) then
          MessageDlg(rsNewLanguageSet, rsRestartCE, mtInformation, [mbok], 0);

      finally
        ini.free;
      end;
    except
    end;
  end;
end;

procedure TformSettings.cbAskIfTableHasLuascriptChange(Sender: TObject);
begin

end;

procedure TformSettings.cbDontusetempdirChange(Sender: TObject);
begin
  label2.enabled:=cbDontusetempdir.checked;
  edtTempScanFolder.enabled:=cbDontusetempdir.checked;
  loadButton.enabled:=cbDontusetempdir.checked;
end;

procedure TformSettings.cbDebuggerInterfaceChange(Sender: TObject);
begin
  rbInt3AsBreakpoint.enabled:=not cbKDebug.checked;

  if cbUseVEHDebugger.Checked then
    pcDebugConfig.ActivePageIndex:=0
  else
  if cbUseWindowsDebugger.checked then
    pcDebugConfig.ActivePageIndex:=1
  else
  if cbKDebug.checked then
  begin
    rbDebugAsBreakpoint.checked:=true;
    pcDebugConfig.ActivePageIndex:=2;
  end
  else
  if cbUseMacDebugger.checked then
  begin
    pcDebugConfig.ActivePageIndex:=3;
    pcDebugConfig.TabIndex:=3;
  end
  else
  if cbUseDBVMDebugger.checked then
  begin
    pcDebugConfig.ActivePageIndex:=4;
    pcDebugConfig.TabIndex:=4;
  end;

  rbPageExceptions.enabled:=not cbKDebug.checked; //currently the kerneldebugger doesn't handle pageexceptions yet (can be added, but not right now)
  if rbPageExceptions.checked and not rbPageExceptions.enabled then
    rbDebugAsBreakpoint.checked:=true;
end;

procedure TformSettings.cbKernelOpenProcessChange(Sender: TObject);
begin
  cbDontOpenHandle.enabled:=cbKernelOpenProcess.Checked;
end;

procedure TformSettings.cbKernelQueryMemoryRegionChange(Sender: TObject);
begin

end;

procedure TformSettings.cbOverrideDefaultFontChange(Sender: TObject);
begin
  btnSetFont.enabled:=cbOverrideDefaultFont.Checked;
end;

procedure TformSettings.cbProcessWatcherChange(Sender: TObject);
begin
  cbProcessWatcherOpensHandles.enabled:=cbProcessWatcher.Checked;
end;

procedure TformSettings.cbUseIntelPTChange(Sender: TObject);
begin
  cbRecordIPTForFindWhatRoutines.visible:=cbUseIntelPT.checked;
  lblMaxIPTSize.visible:=cbUseIntelPT.checked;
  cbIPTTraceSize.visible:=cbUseIntelPT.checked;

  cbHideIPTCapability.visible:=not cbUseIntelPT.checked;
end;

procedure TformSettings.cbWriteLoggingOnChange(Sender: TObject);
begin
  label8.enabled:=cbWriteLoggingOn.checked;
  edtWriteLogSize.enabled:=cbWriteLoggingOn.checked;
end;

procedure TformSettings.CheckBox1Change(Sender: TObject);
begin
  PreventDebuggerDetection:=checkbox1.checked;
end;

procedure TformSettings.cbRecordIPTForFindWhatRoutinesChange(Sender: TObject);
begin

end;



procedure TformSettings.EditBufSizeKeyPress(Sender: TObject;
  var Key: Char);
begin
  decimal(key);
end;

procedure TformSettings.Default1Click(Sender: TObject);
begin
  editbufsize.Text:='512';
end;

procedure TformSettings.FormChangeBounds(Sender: TObject);
var off: integer;
begin
  OnChangeBounds:=nil;
  if top<0 then
  begin
    position:=poDesigned;
    if height>screen.WorkAreaHeight then
      height:=screen.WorkAreaHeight;

    top:=0;
  end;
  OnChangeBounds:=FormChangeBounds;
end;



procedure TformSettings.FormDestroy(Sender: TObject);
begin
  formSettings:=nil;
  if hasBeenShown then
    SaveFormPosition(self);
end;

{$ifdef darwin}
procedure TformSettings.FixUpsideDownTreeviewTimer(sender: TObject);
begin
  //trigger a gui update so it stops being upside down
  tvMenuSelection.Showroot:=true;
  tvMenuSelection.Showroot:=false;

  ttimer(sender).free;
end;

procedure TformSettings.FixUpsideDownTreeview;
var t: TTimer;
begin
  t:=ttimer.create(self);
  t.Interval:=1;
  t.OnTimer:=FixUpsideDownTreeviewTimer;
end;

{$endif}


procedure TformSettings.FormShow(Sender: TObject);
  function CheckAssociation(ext: string):boolean;
  var  reg: TRegistry;
       temp: string;
  begin
    reg := TRegistry.Create;
    reg.RootKey := HKEY_CLASSES_ROOT;
    reg.LazyWrite := false;

    try
      reg.OpenKey(ext+'\shell\open\command',false);
    except
      result:=false;
      reg.free;
      exit;
    end;

    temp:=reg.ReadString('');
    if temp<>application.ExeName+' "%1"' then
    begin
      result:=false;
      reg.CloseKey;
      reg.free;
      exit;
    end;

    result:=true;
  end;
  var reg: TRegistry;
  i,j: integer;
  m: dword;

  fd: TFontData;
begin
  hasBeenShown:=true;
  {$ifdef darwin}
  FixUpsideDownTreeview;
  {$endif}


  tempstatepopuphide:=laststatePopupHide;
  temppopupmodifier:=lastpopupmodifier;
  tempstatepause:=laststatePause;
  temppausemodifier:=lastPausemodifier;
  tempstatespeedhack:=laststateSpeedhack;
  tempspeedhackmodifier:=lastSpeedhackmodifier;

  {$ifndef net}
  setlength(tempdonthidelist,length(donthidelist));
  for i:=0 to length(donthidelist)-1 do
    tempdonthidelist[i]:=donthidelist[i];
  {$endif net}


  label1.Enabled:=not mainform.btnNextScan.enabled;
  editbufsize.enabled:=not mainform.btnNextScan.enabled;

  //load the settings from the register and apply them to this window


  //fill hotkey list
  ZeroMemory(@framehotkeyconfig.newhotkeys, cehotkeycount*sizeof(tkeycombo));

  for i:=0 to length(hotkeythread.hotkeylist)-1 do
    if hotkeythread.hotkeylist[i].handler2 and inrange(hotkeythread.hotkeylist[i].id, 0, cehotkeycount-1) then
    begin
      if hotkeythread.hotkeylist[i].keys[0]<>0 then
        framehotkeyconfig.newhotkeys[hotkeythread.hotkeylist[i].id]:=hotkeythread.hotkeylist[i].keys;
    end;

  framehotkeyconfig.newspeedhackspeed1:=speedhackspeed1;
  framehotkeyconfig.newspeedhackspeed2:=speedhackspeed2;
  framehotkeyconfig.newspeedhackspeed3:=speedhackspeed3;
  framehotkeyconfig.newspeedhackspeed4:=speedhackspeed4;
  framehotkeyconfig.newspeedhackspeed5:=speedhackspeed5;

  framehotkeyconfig.speedupdelta:=speedupdelta;
  framehotkeyconfig.slowdowndelta:=slowdowndelta;


  cbDebuggerInterfaceChange(nil);

  cbVEHRealContextOnThreadCreation.AutoSize:=false;
  cbVEHRealContextOnThreadCreation.AutoSize:=true;


  j:=tvMenuSelection.Width;
  for i:=0 to tvMenuSelection.Items.Count-1 do
    j:=max(j,tvMenuSelection.Canvas.TextWidth(' '+tvMenuSelection.Items[i].Text+' ')+tvMenuSelection.BorderWidth+tvMenuSelection.Indent*2);


  tvMenuSelection.Width:=j;


  {$ifdef windows}
  if WindowsVersion>=wvVista then
    m:=sendmessage(edtStacksize.Handle, EM_GETMARGINS, 0,0)
  else
  {$endif}
    m:=0;


  i:=max(edtStacksize.ClientWidth, canvas.TextWidth('4096')+(m shr 16)+(m and $ffff));
  edtStacksize.clientwidth:=i;

  i:=max(edtLuaCollectTimer.ClientWidth, canvas.TextWidth('64 ')+(m shr 16)+(m and $ffff));
  edtLuaCollectTimer.clientWidth:=i;

  i:=max(edtLuaCollectTimer.ClientWidth, canvas.TextWidth('256 ')+(m shr 16)+(m and $ffff));
  edtLuaMinCollectSize.clientWidth:=i;

  autosize:=false;

  if FontDialog1.Font.Height=0 then
  begin
    //first time init
    fd:=GetFontData(font.handle);

    FontDialog1.Font.Height:=fd.Height;
    FontDialog1.Font.Pitch:=fd.Pitch;
    FontDialog1.Font.Style:=fd.Style;
    FontDialog1.Font.CharSet:=fd.CharSet;
    FontDialog1.Font.Quality:=fd.Quality;
    FontDialog1.Font.Name:=fd.Name;
    FontDialog1.Font.Orientation:=fd.Orientation;
    FontDialog1.Font.color:=font.color;

  end;

 // GroupBox2.top:=gbDebuggerInterface.top+gbDebuggerInterface.height+4;

  unexpectedExceptionHandlerChanged:=false;

  DPIHelper.AdjustComboboxSize(cbIPTTraceSize,canvas);

end;

procedure TformSettings.cbShowDisassemblerClick(Sender: TObject);
begin

end;

procedure TformSettings.Label3Click(Sender: TObject);
begin

end;

procedure TformSettings.LoadButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    edtTempScanFolder.text:=SelectDirectoryDialog1.FileName;
end;

procedure TformSettings.MenuItem1Click(Sender: TObject);
begin
  ScanForLanguages;
end;

procedure TformSettings.miUnexpectedBreakpointsOptionChange(Sender: TObject);
begin
  unexpectedExceptionHandlerChanged:=true;
end;



procedure TformSettings.Panel3Click(Sender: TObject);
begin

end;

procedure TformSettings.Panel3Resize(Sender: TObject);
begin
  spbDown.top:=panel4.top-spbDown.height;
end;

procedure TformSettings.pcSettingChange(Sender: TObject);
begin

end;

procedure TformSettings.rbInt3AsBreakpointChange(Sender: TObject);
begin

end;

procedure TformSettings.replacewithnopsClick(Sender: TObject);
begin
  askforreplacewithnops.Enabled:=replacewithnops.Checked;
end;

procedure TformSettings.CheckBox1Click(Sender: TObject);
begin

end;

procedure TformSettings.CheckBox2Click(Sender: TObject);
begin

end;

procedure TformSettings.cbUpdatefoundListClick(Sender: TObject);
begin
  if cbUpdatefoundList.Checked then
  begin
    label18.Enabled:=true;
    editUpdatefoundInterval.Enabled:=true;
    Label19.Enabled:=true;
  end
  else
  begin
    label18.Enabled:=false;
    editUpdatefoundInterval.Enabled:=false;
    Label19.Enabled:=false;
  end;
end;

procedure TformSettings.AboutLabelClick(Sender: TObject);
begin
  with tabout.create(self) do
  begin
    showmodal;
    free;
  end;

end;

procedure TformSettings.cbHideAllWindowsClick(Sender: TObject);
begin
  btnExcludeProcesses.enabled:=cbHideallWindows.Checked;
end;

procedure TformSettings.btnExcludeProcessesClick(Sender: TObject);
begin
  {$ifndef net}

  with tfrmExcludeHide.create(self) do
  begin
    showmodal;
    free;
  end;
  {$endif}
end;

procedure TformSettings.cleanupLanguageList;
var
  i:integer;
  e:TLanguageEntry;

begin
  for i:=0 to lbLanguages.Count-1 do
  begin
    e:=TLanguageEntry(lbLanguages.Items.Objects[i]);
    if e<>nil then
      e.free;
    lbLanguages.Items.Objects[i]:=nil;
  end;

  lbLanguages.Clear;

  for i:=mainform.miLanguages.Count-1 downto 0 do
    mainform.miLanguages.Items[i].Free;
end;

procedure TformSettings.ScanForLanguages;
var
  i: integer;
  index: integer;
  f: TStringList;
  n: string;
  e: TLanguageEntry;
  ini: TIniFile;

  curr: string;
  mi: TMenuItem;
begin
  n:='';
  cleanupLanguageList;

  curr:=currentTranslation;
  if hasSetNewLanguage then
  begin
    if newlanguage<>'*' then
      curr:=newlanguage
    else
      curr:='';
  end;

  if curr='' then
  begin
    lbLanguages.Items.Add('>>English');
    lblCurrentLanguage.Caption:='English';
  end
  else
    lbLanguages.Items.Add('English');

  mi:=TMenuItem.Create(mainform.MainMenu1);
  mi.Caption:='English';
  mi.Tag:=0;
  mi.RadioItem:=true;
  if curr='' then
    mi.Checked:=true;
  mi.OnClick:=LanguageMenuItemClick;

  mainform.miLanguages.Add(mi);

  f:=TStringList.Create;
  {$ifdef darwin}
  OutputDebugString('ScanForLanguages: Looking in '+CheatEngineDir+{$ifdef darwin}PathDelim+'..'+{$endif}PathDelim+'Languages');
  {$endif}
  FindAllDirectories(f,CheatEngineDir+{$ifdef darwin}PathDelim+'..'+{$endif}PathDelim+'Languages',false);

  index:=1;
  for i:=0 to f.Count-1 do
  begin
    n:=f[i];
    if not (fileexists(n+pathsep+'cheatengine.po') or fileexists(n+PathDelim+'cheatengine-x86_64.po') or fileexists(n+PathDelim+'cheatengine-i386.po')) then
      continue;


    e:=TLanguageEntry.Create;
    e.foldername:=ExtractFileName(n);

    if FileExists(f[i]+PathDelim+'name.txt') then
      n:=ReadFileToString(f[i]+PathDelim+'name.txt')
    else
      n:=e.foldername;

    mi:=TMenuItem.Create(mainform.MainMenu1);
    mi.Caption:=n;
    mi.Tag:=index;
    inc(index);
    mi.RadioItem:=true;
    if uppercase(e.foldername)=uppercase(curr) then
    begin
      if (self<>nil) and (lblCurrentLanguage<>nil) then //should always be the case
        lblCurrentLanguage.Caption:=n;

      n:='>>'+n;
      mi.Checked:=true;
    end;

    mi.OnClick:=LanguageMenuItemClick;
    lbLanguages.Items.AddObject(n,e);
    mainform.miLanguages.Add(mi);

  end;



  if tvMenuSelection.Items[6].Visible=false then
    tvMenuSelection.Items[6].Visible:=lbLanguages.count>1;

  f.free;
end;

procedure TformSettings.LanguageMenuItemClick(Sender: TObject);
var mi: TMenuItem;
begin
  if sender is TMenuItem then
  begin
    mi:=TMenuItem(sender);
    lbLanguages.ItemIndex:=mi.Tag;
    btnSelectLanguage.Click;
  end;
end;

procedure TformSettings.FormCreate(Sender: TObject);
var i: integer;
  {$ifdef windows}
  osVerInfo: TOSVersionInfo;
  {$endif}

  reg: Tregistry;
  v,m: integer;

  KVAShadowInfo: dword;
  rl: DWORD;
begin
  tvMenuSelection.Items[0].Data:=GeneralSettings;
  tvMenuSelection.Items[1].Data:=tsTools;
  tvMenuSelection.Items[2].Data:=tsHotkeys;
  tvMenuSelection.Items[3].Data:=Unrandomizer;
  tvMenuSelection.Items[4].Data:=ScanSettings;
  tvMenuSelection.Items[5].Data:=Plugins;
  tvMenuSelection.Items[6].Data:=Languages;
  tvMenuSelection.Items[7].Data:=self.Assembler;
  tvMenuSelection.Items[8].Data:=tsLua;
  tvMenuSelection.Items[9].Data:=Extra;
  tvMenuSelection.Items[10].Data:=tsSigning;

  tvMenuSelection.Items[6].Visible:=false;
  tvMenuSelection.Items[10].Visible:={$ifdef windows}cansigntables{$else}false{$endif};

  {$ifdef altname}
  tvMenuSelection.Items[9].Visible:=false; //the pussy version does not have kernelmode tools
  {$endif}

  pcSetting.ShowTabs:=false;

  ScanForLanguages;

  combothreadpriority.Items.Clear;
  with combothreadpriority.items do
  begin
    add(rsIdle);
    add(rsLowest);
    add(rsLower);
    add(rsNormal);
    add(rsHigher);
    add(rsHighest);
    add(rsTimeCritical);
  end;
  combothreadpriority.ItemIndex:=4;

  cbAllDword.Checked:=true;
  cbAllSingle.Checked:=true;
  cbAllDouble.Checked:=true;


  with frameHotkeyConfig.ListBox1.items do
  begin
    clear;
    add(rsAttachToForegroundProcess);
    add(rsPopupHideCheatEngine);
    add(rsPauseTheSelectedProcess);
    add(rsToggleTheSpeedhack);
    add(rsSpeedhackSpeed+' 1');
    add(rsSpeedhackSpeed+' 2');
    add(rsSpeedhackSpeed+' 3');
    add(rsSpeedhackSpeed+' 4');
    add(rsSpeedhackSpeed+' 5');
    add(rsSpeedhackSpeed+' +');
    add(rsSpeedhackSpeed+' -');
    add(rsChangeTypeTo+' '+rsBinary);
    add(rsChangeTypeTo+' '+rsByte);
    add(rsChangeTypeTo+' '+rs2Bytes);
    add(rsChangeTypeTo+' '+rs4Bytes);
    add(rsChangeTypeTo+' '+rs8Bytes);
    add(rsChangeTypeTo+' '+rsFloat);
    add(rsChangeTypeTo+' '+rsDouble);
    add(rsChangeTypeTo+' '+rsText);
    add(rsChangeTypeTo+' '+rsArrayOfByte);
    add(strNewScan);
    add(strNewScan+'-'+strexactvalue);
    add(strNewScan+'-'+rsUnknownInitialValue);
    add(rsNextScan+'-'+strexactvalue);
    add(rsNextScan+'-'+rsIncreasedValue);
    add(rsNextScan+'-'+rsDecreasedValue);
    add(rsNextScan+'-'+rsChangedValue);
    add(rsNextScan+'-'+rsUnchangedValue);
    add(rsToggleBetweenFirstLastScanCompare);
    add(rsUndoLastScan);
    add(rsCancelTheCurrentScan);
    add(rsDebugRun);
  end;

  tvMenuSelection.Items[0].Text:=rsGeneralSettings;
  tvMenuSelection.Items[1].Text:=rsTools;
  tvMenuSelection.Items[2].Text:=rsHotkeys;
  tvMenuSelection.Items[3].Text:=rsUnrandomizer;
  tvMenuSelection.Items[4].Text:=rsScanSettings;
  tvMenuSelection.Items[5].Text:=rsPlugins;
  tvMenuSelection.Items[6].Text:=rsLanguages;
  tvMenuSelection.Items[7].Text:=rsDebuggerOptions;
  tvMenuSelection.Items[8].Text:=rsLuaOptions;
  tvMenuSelection.Items[9].Text:=rsExtra;
  tvMenuSelection.Items[10].Text:=rsSigning;



  aboutlabel.left:=aboutlabel.parent.ClientWidth-aboutlabel.width;
  aboutlabel.top:=aboutlabel.parent.clientheight-aboutlabel.height;

  //set the default popup
  laststatePopupHide:=vk_next;
  lastpopupmodifier:=MOD_CONTROL or MOD_ALT;

  laststatepause:=ord('P');
  lastpausemodifier:=MOD_CONTROL or MOD_ALT;

  laststateSpeedhack:=ord('S');
  laststateSpeedhack:=MOD_CONTROL or MOD_ALT;

  deletedmodules:=TStringlist.Create;


  //64-bit check
  if is64bitos then
  begin

    {TauntOldOsUser.Visible:=true;
    TauntOldOsUser.Caption:=rsPleaseBootWithUnsignedDriversAllowedF8DuringBootOr;   }



    cbKdebug.Enabled:={$ifdef windows}isRunningDBVM or isDBVMCapable{$else}false{$endif};

    cbKdebug.Caption:=cbKdebug.Caption+' '+rsRequiresDBVM;
    if not cbKdebug.Enabled then
      cbKdebug.checked:=false;
  end;


  {$ifdef windows}
  if NtQuerySystemInformation(196, @KVAShadowInfo,4,@rl)=0 then
  begin
    //it knows this classID..
    if (KVAShadowInfo and 1)=1 then
    begin
      cbKDebug.enabled:=false;
      btnMakeKernelDebugPossible.visible:=true;
    end;
  end;



  cbUseIntelPT.enabled:=systemSupportsIntelPT;
  cbHideIPTCapability.visible:=systemSupportsIntelPT;
  {$else}
  cbUseIntelPT.visible:=false;
  cbHideIPTCapability.visible:=false;
  {$endif}


  //check if it should be disabled
  reg:=tregistry.create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Control\Session Manager\Memory Management',false) then
    begin
      if reg.ValueExists('FeatureSettingsOverride') and reg.ValueExists('FeatureSettingsOverrideMask') then
      begin
        //good indication KVAShadow is disabled, but let's check the values
        v:=reg.ReadInteger('FeatureSettingsOverride');
        m:=reg.ReadInteger('FeatureSettingsOverrideMask');

        if (v=3) and (m=3) then //it's set
        begin
          if btnMakeKernelDebugPossible.visible then
            btnMakeKernelDebugPossible.enabled:=false;

          //show button to remove this registry key
          btnRestoreKernelProtection.visible:=true;
        end;
      end;
    end;

  finally
    reg.free;
  end;

  if cbKDebug.enabled=false then
  begin
    //still disabled
    btnMakeKernelDebugPossible.Visible:=true;
  end;

  //make the tabs invisible
  {$ifdef windows}
  for i:=0 to pcSetting.PageCount-1 do
    pcSetting.Pages[i].TabVisible:=false;

  pcSetting.ActivePageIndex:=0;

  for i:=0 to pcDebugConfig.PageCount-1 do
    pcDebugConfig.Pages[i].TabVisible:=false;
  {$else}
  pcDebugConfig.ShowTabs:=false;
  {$endif}




  tvMenuSelection.FullExpand;

  {$ifdef privatebuild}
  cbDontOpenHandle.visible:=true;
  cbProcessWatcherOpensHandles.visible:=true;
  cbInjectDLLWithAPC.visible:=true;
  {$endif}


  if LoadFormPosition(self) then
    autosize:=false;



  {$ifdef darwin}
  cbUseVEHDebugger.enabled:=false;
  cbUseVEHDebugger.visible:=false;
  cbUseWindowsDebugger.enabled:=false;
  cbUseWindowsDebugger.visible:=false;
  cbKDebug.enabled:=false;
  cbKDebug.visible:=false;
  cbUseDBVMDebugger.enabled:=false;
  cbUseDBVMDebugger.visible:=false;
  panel11.visible:=false;

  cbUseMacDebugger.checked:=true;

  {$else}
  cbUseMacDebugger.visible:=false;
  {$endif}

  pcSetting.ActivePageIndex:=0;
end;

procedure TformSettings.cbKernelQueryMemoryRegionClick(Sender: TObject);
begin
  if (cbKernelQueryMemoryRegion.Checked) or (cbKernelReadWriteProcessMemory.Checked) then
  begin
    cbKernelOpenProcess.Checked:=true;
    cbKernelOpenProcess.Enabled:=false;
  end
  else cbKernelOpenProcess.Enabled:=true;


end;

procedure TformSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  deletedmodules.Clear;
end;




procedure TformSettings.Button4Click(Sender: TObject);
var pluginname: string;
    fullpath: Tpathspecifier;
    i: integer;
    s: string;
begin
{$ifndef net}

  if opendialog1.Execute then
  begin
    s:=uppercase(ExtractFileName(opendialog1.FileName));
    for i:=0 to clbplugins.count-1 do
      if uppercase(extractfilename(Tpathspecifier(clbplugins.Items.Objects[i]).path))=s then
        raise exception.Create(rsThisPluginIsAlreadyLoaded);

    pluginname:=pluginhandler.GetPluginName(opendialog1.FileName);
    fullpath:=Tpathspecifier.Create;
    fullpath.path:=opendialog1.filename;
    clbPlugins.Items.AddObject(extractfilename(opendialog1.FileName)+':'+pluginname,fullpath);
  end;
{$endif}
end;

procedure TformSettings.Button5Click(Sender: TObject);
var modulename: string;
  dllpath: Tpathspecifier;
  pluginid: integer;
begin

  if clbplugins.ItemIndex<>-1 then
  begin
    dllpath:=Tpathspecifier(clbplugins.Items.Objects[clbplugins.ItemIndex]);
    modulename:=extractfilename(dllpath.path);
    deletedmodules.add(modulename);

    clbPlugins.Items.Delete(clbplugins.ItemIndex);




    pluginid:=pluginhandler.GetPluginID(dllpath.path);
    pluginhandler.UnloadPlugin(pluginid);

    dllpath.Free;
  end;

end;

procedure TformSettings.ScrollBox1Click(Sender: TObject);
begin

end;


procedure TformSettings.spbUpClick(Sender: TObject);
var
  i: integer;
  li: TListItem;
begin
  //move up
  if lvtools.ItemIndex>=1 then
    lvtools.items.Move(lvtools.ItemIndex, lvtools.ItemIndex-1);
end;

procedure TformSettings.spbDownClick(Sender: TObject);
begin
  if (lvtools.ItemIndex<>-1) and (lvtools.ItemIndex<lvtools.items.Count-1) then
    lvtools.items.Move(lvtools.ItemIndex, lvtools.ItemIndex+1);
end;


procedure TformSettings.tvMenuSelectionChange(Sender: TObject;
  Node: TTreeNode);
var w,h: integer;
begin
  if node.Data<>nil then
    pcSetting.ActivePage:=TTabSheet(node.data);

  if pcSetting.ActivePage=self.Assembler then
  begin
    groupbox2.AutoSize:=true;

    pcDebugConfig.PageIndex:=0;
    w:=groupbox2.Width;
    h:=groupbox2.Height;

    pcDebugConfig.PageIndex:=1;
    w:=max(groupbox2.Width, w);
    h:=max(groupbox2.Height, h);

    pcDebugConfig.PageIndex:=2;
    w:=max(groupbox2.Width, w);
    h:=max(groupbox2.Height, h);

    {$ifdef darwin}
    pcDebugConfig.PageIndex:=3;
    w:=max(groupbox2.Width, w);
    h:=max(groupbox2.Height, h);

    {$endif}

    groupbox2.AutoSize:=false;

    cbDebuggerInterfaceChange(nil);

    groupbox2.Width:=w;
    groupbox2.Height:=h;

  end;
end;

procedure TformSettings.Panel6Resize(Sender: TObject);
begin

end;

procedure TformSettings.cbProcessIconsClick(Sender: TObject);
begin

end;

procedure TformSettings.tvMenuSelectionCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse:=false;
end;

procedure TformSettings.btnSetToolShortcutClick(Sender: TObject);
var x: tshortcut;
begin

  if lvtools.Selected=nil then exit;

  with TFormHotkey.Create(self) do
  begin
    if ShowModal=mrok then
    begin
      x:=key;

      if (modifier and MOD_ALT)>0 then
        x:=x or scAlt;

      if (modifier and MOD_CONTROL)>0 then
        x:=x or scCtrl;

      if (modifier and MOD_SHIFT)>0 then
        x:=x or scShift;

      lblShortcutText.caption:=ShortCutToText(x);
      lvtools.Selected.Data:=pointer(ptrUint(x));
      lvtools.Selected.SubItems[1]:=lblShortcutText.caption;
    end;

    free;
  end;
end;

procedure TformSettings.cbShowToolsClick(Sender: TObject);
begin
  lvTools.enabled:=cbShowTools.Checked;
  lblToolsName.enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  edtToolsName.enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  lblApplicationTool.enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  edtApplicationTool.enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  OpenButton.Enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  lblShortcut.enabled:= cbShowTools.Checked and (lvtools.Selected<>nil);
  lblShortcutText.enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  btnSetToolShortcut.enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  btnToolNew.enabled:=cbShowTools.Checked;
  btnToolDelete.Enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);

  if (lvtools.Selected<>nil) then
  begin
    edtToolsName.Text:=lvtools.Selected.Caption;
    edtApplicationTool.Text:=lvtools.Selected.SubItems[0];
    lblShortcutText.caption:=lvtools.Selected.SubItems[1];
  end;

end;

procedure TformSettings.btnToolNewClick(Sender: TObject);
var li:tlistitem;
begin
  li:=lvTools.Items.Add;
  li.Data:=nil;
  li.Caption:=rsNoName;
  li.SubItems.Add('');
  li.SubItems.Add('');
  li.Selected:=true;
  lvTools.OnClick(lvTools);


  edtToolsName.SetFocus;
  edtToolsName.SelectAll;
end;

procedure TformSettings.lvToolsClick(Sender: TObject);
begin
  lblToolsName.enabled:=lvtools.Selected<>nil;
  edtToolsName.enabled:=lvtools.Selected<>nil;
  lblApplicationTool.enabled:=lvtools.Selected<>nil;
  edtApplicationTool.enabled:=lvtools.Selected<>nil;
  lblShortcut.enabled:= lvtools.Selected<>nil;
  lblShortcutText.enabled:=lvtools.Selected<>nil;
  btnSetToolShortcut.enabled:=lvtools.Selected<>nil;
  btnToolDelete.Enabled:=lvtools.Selected<>nil;
  OpenButton.Enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);

  if lvtools.Selected<>nil then
  begin
    edtToolsName.Text:=lvtools.Selected.Caption;
    edtApplicationTool.Text:=lvtools.Selected.SubItems[0];
    lblShortcutText.caption:=lvtools.Selected.SubItems[1];
    edtToolsName.SetFocus;
  end;

  spbDown.enabled:=lvTools.selected<>nil;
  spbUp.Enabled:=spbDown.enabled;

end;

procedure TformSettings.edtApplicationToolChange(Sender: TObject);
begin
  lvtools.Selected.subitems[0]:=edtApplicationTool.text;
end;

procedure TformSettings.btnToolDeleteClick(Sender: TObject);
begin
  if lvTools.Selected<>nil then
    lvTools.Selected.Delete;

  lvTools.OnClick(lvTools); //refresh
end;

procedure TformSettings.edtToolsNameChange(Sender: TObject);
begin
  lvtools.Selected.Caption:=edtToolsName.text;
end;

procedure TformSettings.OpenButtonClick(Sender: TObject);
begin
  if opendialog2.Execute then
    edtApplicationTool.Text:=opendialog2.FileName;


end;

initialization
  {$i formsettingsunit.lrs}

end.


