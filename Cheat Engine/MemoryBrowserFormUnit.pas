unit MemoryBrowserFormUnit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  LCLType,
  {$endif}
  {$ifdef windows}
  jwawindows, windows,imagehlp,
  {$endif}
  LCLProc, LCLIntf, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, frmMemoryAllocHandlerUnit, math, StdCtrls, Spin,
  ExtCtrls,CEFuncProc,symbolhandler,Clipbrd, Menus,plugin,CEDebugger,KernelDebugger,
  Assemblerunit,disassembler,addressparser, Buttons, Contnrs,
  disassemblerviewunit, PEInfoFunctions ,DissectCodeThread,stacktrace2,
  NewKernelHandler, ComCtrls, LResources, byteinterpreter, StrUtils, hexviewunit,
  debughelper, debuggertypedefinitions,frmMemviewPreferencesUnit, registry,
  disassemblerComments, multilineinputqueryunit, frmMemoryViewExUnit,
  LastDisassembleData, ProcessHandlerUnit, commonTypeDefs, binutils,
  fontSaveLoadRegistry, LazFileUtils, ceregistry, frmCR3SwitcherUnit,
  betterControls, ScrollBoxEx, contexthandler,iptlogdisplay
  {$ifdef darwin}, macport, macportdefines{$endif} ;


type

  { TMemoryBrowser }

  TMemoryBrowser = class(TForm)
    aflabel: TLabel;
    cflabel: TLabel;
    CSLabel: TLabel;
    dflabel: TLabel;
    dispQwords: TMenuItem;
    DSLabel: TLabel;
    EAXLabel: TLabel;
    EBPlabel: TLabel;
    EBXlabel: TLabel;
    ECXlabel: TLabel;
    EDIlabel: TLabel;
    EDXlabel: TLabel;
    EIPlabel: TLabel;
    ESIlabel: TLabel;
    ESlabel: TLabel;
    ESPlabel: TLabel;
    FSlabel: TLabel;
    GSlabel: TLabel;
    miIPTLog: TMenuItem;
    miShowRelativeDisassembler: TMenuItem;
    miArchX86: TMenuItem;
    miArchArm: TMenuItem;
    miArchAutodetect: TMenuItem;
    MenuItem4: TMenuItem;
    copyBytesAndOpcodesAndComments: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    miArchitecture: TMenuItem;
    miCR3Switcher: TMenuItem;
    miShowSectionAddresses: TMenuItem;
    miOpenInDissectData: TMenuItem;
    miCopyOpcodesOnly: TMenuItem;
    miUndoLastEdit: TMenuItem;
    miFollowInHexview: TMenuItem;
    miSetSpecificBreakpoint: TMenuItem;
    miWatchBPHardware: TMenuItem;
    miWatchBPException: TMenuItem;
    miWatchBPDBVM: TMenuItem;
    N9: TMenuItem;
    miSetBreakpointSW: TMenuItem;
    miSetBreakpointHW: TMenuItem;
    miSetBreakpointPE: TMenuItem;
    miSetBreakpointDBVMExec: TMenuItem;
    mvImageList: TImageList;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    miSearchForAccessibleStrings: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    miDBVMFindoutwhataddressesthisinstructionaccesses: TMenuItem;
    Showdebugtoolbar1: TMenuItem;
    miCopyAddressesOnly: TMenuItem;
    miHideToolbar: TMenuItem;
    miDBVMActivateCloak: TMenuItem;
    miDBVMDisableCloak: TMenuItem;
    miUltimap: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    miLoadTrace: TMenuItem;
    dispChar: TMenuItem;
    dispShorts: TMenuItem;
    DispLongs: TMenuItem;
    miTextEncoding: TMenuItem;
    miWatchMemoryPageAccess: TMenuItem;
    miSetUnsetBookmark: TMenuItem;
    miGotoBookmark: TMenuItem;
    miUserWriteInteger: TMenuItem;
    miUltimap2: TMenuItem;
    miWatchList: TMenuItem;
    miStackTraceSep: TMenuItem;
    miCompareStructures: TMenuItem;
    miCodeFilter: TMenuItem;
    miExceptionIgnoreList: TMenuItem;
    N19: TMenuItem;
    miUnexpectedExceptionBreakNever: TMenuItem;
    miUnexpectedExceptionBreakAlways: TMenuItem;
    miUnexpectedExceptionBreakIfInRegion: TMenuItem;
    miExceptionRegionSeperator: TMenuItem;
    miExceptionRegionManageList: TMenuItem;
    miExceptionRegionAutoAddAllocs: TMenuItem;
    miBreakOnExceptions: TMenuItem;
    miRunUnhandled: TMenuItem;
    miChangeProtectionRWE: TMenuItem;
    miChangeProtectionRE: TMenuItem;
    miChangeProtectionRW: TMenuItem;
    miChangeProtectionR: TMenuItem;
    miTextEncodingCodePage: TMenuItem;
    miSVCopy: TMenuItem;
    miShowRelative: TMenuItem;
    miHVBack: TMenuItem;
    miHVFollow: TMenuItem;
    miAddRef: TMenuItem;
    miTextEncodingUTF8: TMenuItem;
    miDebugSetAddress: TMenuItem;
    miGNUAssembler: TMenuItem;
    miBinutilsSelect: TMenuItem;
    miBinUtils: TMenuItem;
    miSetBookmark0: TMenuItem;
    miGotoBookmark0: TMenuItem;
    miSetBookmark1: TMenuItem;
    miSetBookmark2: TMenuItem;
    miSetBookmark3: TMenuItem;
    miSetBookmark4: TMenuItem;
    miSetBookmark5: TMenuItem;
    miSetBookmark6: TMenuItem;
    miSetBookmark7: TMenuItem;
    miSetBookmark8: TMenuItem;
    miSetBookmark9: TMenuItem;
    miGotoBookmark1: TMenuItem;
    miGotoBookmark2: TMenuItem;
    miGotoBookmark3: TMenuItem;
    miGotoBookmark4: TMenuItem;
    miGotoBookmark5: TMenuItem;
    miGotoBookmark6: TMenuItem;
    miGotoBookmark7: TMenuItem;
    miGotoBookmark8: TMenuItem;
    miGotoBookmark9: TMenuItem;
    miTextEncodingAscii: TMenuItem;
    miTextEncoding16: TMenuItem;
    miReferencedFunctions: TMenuItem;
    miUserDefinedHeader: TMenuItem;
    miShowIndisassembler: TMenuItem;
    miShowInHexview: TMenuItem;
    miCopyBytesOnly: TMenuItem;
    miDissectData2: TMenuItem;
    miPointerSpider: TMenuItem;
    MenuItem9: TMenuItem;
    miDisassembly32: TMenuItem;
    miDisassembly64: TMenuItem;
    miDisassemblyAutodetect: TMenuItem;
    miDisassemblerType: TMenuItem;
    miAddEBP: TMenuItem;
    miAddESP: TMenuItem;
    miFindWhatWrites: TMenuItem;
    miSelectCurrentFunction: TMenuItem;
    MenuItem3: TMenuItem;
    miDataBreakPointMenu: TMenuItem;
    miWatchAccess: TMenuItem;
    miWatchWrite: TMenuItem;
    MenuItem7: TMenuItem;
    miBreakAndTrace: TMenuItem;
    miFindWhatAccesses: TMenuItem;
    miDeleteBP: TMenuItem;
    miLock: TMenuItem;
    miShowDifference: TMenuItem;
    miUserdefinedComment: TMenuItem;
    miSepEvery4Bytes: TMenuItem;
    miSepEvery8Bytes: TMenuItem;
    miSepEvery2Bytes: TMenuItem;
    miSeperators: TMenuItem;
    miConditionalBreak: TMenuItem;
    miTextPreferences: TMenuItem;
    miLuaEngine: TMenuItem;
    miPaging: TMenuItem;
    miDebugEvents: TMenuItem;
    miLockRowsize: TMenuItem;
    memorypopup: TPopupMenu;
    Goto1: TMenuItem;
    debuggerpopup: TPopupMenu;
    oflabel: TLabel;
    pnlGeneralRegisters: TPanel;
    pnlExtraRegisters: TPanel;
    pnlFlags: TPanel;
    pflabel: TLabel;
    pmRegisters: TPopupMenu;
    pmDebugToolbar: TPopupMenu;
    sbShowFloats: TButton;
    sflabel: TLabel;
    SSlabel: TLabel;
    Timer2: TTimer;
    Panel1: TPanel;
    Panel4: TPanel;
    miReplacewithnops: TMenuItem;
    Gotoaddress1: TMenuItem;
    Search1: TMenuItem;
    Change1: TMenuItem;
    Addthisaddresstothelist1: TMenuItem;
    miAddToTheCodelist: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Splitter1: TSplitter;
    Panel5: TPanel;
    RegisterView: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Loadsymbolfile1: TMenuItem;
    Debug1: TMenuItem;
    miDebugStep: TMenuItem;
    miDebugStepOver: TMenuItem;
    miDebugRunTill: TMenuItem;
    miDebugToggleBreakpoint: TMenuItem;
    tbDebug: TToolBar;
    tbRun: TToolButton;
    tbStepInto: TToolButton;
    tbStepOver: TToolButton;
    tbSeparator1: TToolButton;
    tbStepOut: TToolButton;
    tbSeparator2: TToolButton;
    tbToggleBreakpoint: TToolButton;
    tbRunTill: TToolButton;
    tbSeparator3: TToolButton;
    tbRunUnhandled: TToolButton;
    View1: TMenuItem;
    Stacktrace1: TMenuItem;
    ScrollBox1: TScrollBox;
    lblRegisters: TLabel;
    sRegisters: TShape;
    lblFlags: TLabel;
    sFlags: TShape;
    lblSpecial: TLabel;
    sSpecial: TShape;
    miDebugRun: TMenuItem;
    Threadlist1: TMenuItem;
    Assemble1: TMenuItem;
    N3: TMenuItem;
    miDebugBreak: TMenuItem;
    Extra1: TMenuItem;
    Reservememory1: TMenuItem;
    Savedisassemledoutput1: TMenuItem;
    Savememoryregion1: TMenuItem;
    Loadmemolryregion1: TMenuItem;
    N4: TMenuItem;
    OpenMemory: TOpenDialog;
    Debugstrings1: TMenuItem;
    CreateThread1: TMenuItem;
    MemoryRegions1: TMenuItem;
    FillMemory1: TMenuItem;
    Disectwindow1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Heaps1: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    EnumeratedllsandSymbols1: TMenuItem;
    InjectDLL1: TMenuItem;
    OpenDllDialog: TOpenDialog;
    AutoInject1: TMenuItem;
    Dissectcode1: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Findstaticpointers1: TMenuItem;
    Scanforcodecaves1: TMenuItem;
    Changestateofregisteratthislocation1: TMenuItem;
    miTogglebreakpoint: TMenuItem;
    Breakpointlist1: TMenuItem;
    Makepagewritable1: TMenuItem;
    Dissectdata1: TMenuItem;
    N10: TMenuItem;
    miShowSymbols: TMenuItem;
    miDissectData: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    miShowModuleAddresses: TMenuItem;
    miUserdefinedSymbols: TMenuItem;
    Kerneltools1: TMenuItem;
    Allocatenonpagedmemory1: TMenuItem;
    Getaddress1: TMenuItem;
    Search2: TMenuItem;
    Assemblycode1: TMenuItem;
    Findmemory1: TMenuItem;
    Driverlist1: TMenuItem;
    Plugins1: TMenuItem;
    Sericedescriptortable1: TMenuItem;
    N13: TMenuItem;
    Cut1: TMenuItem;
    Pastefromclipboard1: TMenuItem;
    N14: TMenuItem;
    Setsymbolsearchpath1: TMenuItem;
    miKernelmodeSymbols: TMenuItem;
    Breakandtraceinstructions1: TMenuItem;
    GDTlist1: TMenuItem;
    IDTlist1: TMenuItem;
    Newwindow1: TMenuItem;
    Follow1: TMenuItem;
    Copytoclipboard1: TMenuItem;
    copyBytes: TMenuItem;
    copyOpcodes: TMenuItem;
    copyBytesAndOpcodes: TMenuItem;
    DissectPEheaders1: TMenuItem;
    Back1: TMenuItem;
    Showvaluesofstaticaddresses1: TMenuItem;
    Findoutwhataddressesthisinstructionaccesses1: TMenuItem;
    DisplayType1: TMenuItem;
    N15: TMenuItem;
    dispBytes: TMenuItem;
    dispWords: TMenuItem;
    dispDwords: TMenuItem;
    dispFloat: TMenuItem;
    dispDouble: TMenuItem;
    dispInts: TMenuItem;
    Jumplines1: TMenuItem;
    Showjumplines1: TMenuItem;
    Onlyshowjumplineswithinrange1: TMenuItem;
    Watchmemoryallocations1: TMenuItem;
    Continueanddetachdebugger1: TMenuItem;
    N16: TMenuItem;
    Panel3: TPanel;
    Splitter3: TSplitter;
    pnlStacktrace: TPanel;
    pmStacktrace: TPopupMenu;
    All1: TMenuItem;
    Modulesonly1: TMenuItem;
    Nonsystemmodulesonly1: TMenuItem;
    lvStacktraceData: TListView;
    N17: TMenuItem;
    Maxstacktracesize1: TMenuItem;
    Splitter2: TSplitter;
    Referencedstrings1: TMenuItem;
    N18: TMenuItem;
    stacktrace2: TMenuItem;
    miDebugExecuteTillReturn: TMenuItem;
    zflabel: TLabel;
    procedure Debug1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure GotoBookmarkClick(Sender: TObject);
    procedure Makepagewritable1Click(Sender: TObject);
    procedure memorypopupPopup(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure miSearchForAccessibleStringsClick(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure miArchChangeClick(Sender: TObject);
    procedure miCR3SwitcherClick(Sender: TObject);
    procedure miDBVMFindoutwhataddressesthisinstructionaccessesClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure miIPTLogClick(Sender: TObject);
    procedure miOpenInDissectDataClick(Sender: TObject);
    procedure miShowRelativeDisassemblerClick(Sender: TObject);
    procedure miShowSectionAddressesClick(Sender: TObject);
    procedure miUndoLastEditClick(Sender: TObject);
    procedure miFollowInHexviewClick(Sender: TObject);
    procedure miSetSpecificBreakpointClick(Sender: TObject);
    procedure miSetBreakpointClick(Sender: TObject);
    procedure miCodeFilterClick(Sender: TObject);
    procedure miDBVMActivateCloakClick(Sender: TObject);
    procedure miDBVMDisableCloakClick(Sender: TObject);
    procedure miHideToolbarClick(Sender: TObject);
    procedure miUltimapClick(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure miLoadTraceClick(Sender: TObject);
    procedure miWatchMemoryPageAccessClick(Sender: TObject);
    procedure miUserWriteIntegerClick(Sender: TObject);
    procedure miUltimap2Click(Sender: TObject);
    procedure miWatchListClick(Sender: TObject);
    procedure miCompareStructuresClick(Sender: TObject);
    procedure miAddRefClick(Sender: TObject);
    procedure miBreakOnExceptionsClick(Sender: TObject);
    procedure miChangeProtectionClick(Sender: TObject);
    procedure miExceptionIgnoreListClick(Sender: TObject);
    procedure miExceptionRegionAutoAddAllocsClick(Sender: TObject);
    procedure miExceptionRegionManageListClick(Sender: TObject);
    procedure miHVBackClick(Sender: TObject);
    procedure miHVFollowClick(Sender: TObject);
    procedure miDebugSetAddressClick(Sender: TObject);
    procedure miGNUAssemblerClick(Sender: TObject);
    procedure miBinutilsSelectClick(Sender: TObject);
    procedure miRunUnhandledClick(Sender: TObject);
    procedure miShowRelativeClick(Sender: TObject);
    procedure miSVCopyClick(Sender: TObject);
    procedure miUnexpectedExceptionBreakOptionClick(Sender: TObject);
    procedure pmRegistersPopup(Sender: TObject);
    procedure pmStacktracePopup(Sender: TObject);
    procedure RegisterViewResize(Sender: TObject);
    procedure SetBookmarkClick(Sender: TObject);
    procedure miTextEncodingClick(Sender: TObject);
    procedure miReferencedFunctionsClick(Sender: TObject);
    procedure miShowIndisassemblerClick(Sender: TObject);
    procedure miCopyBytesOnlyClick(Sender: TObject);
    procedure miDissectData2Click(Sender: TObject);
    procedure miPointerSpiderClick(Sender: TObject);
    procedure miSelectCurrentFunctionClick(Sender: TObject);
    procedure miDataBreakPointMenuClick(Sender: TObject);
    procedure miWatchAccessClick(Sender: TObject);
    procedure miWatchWriteClick(Sender: TObject);
    procedure miBreakAndTraceClick(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure miAddESPClick(Sender: TObject);
    procedure miConditionalBreakClick(Sender: TObject);
    procedure miDeleteBPClick(Sender: TObject);
    procedure miDisassemblyAutodetectClick(Sender: TObject);
    procedure miFindWhatAccessesClick(Sender: TObject);
    procedure miFindWhatWritesClick(Sender: TObject);
    procedure miSepClick(Sender: TObject);
    procedure miShowInHexviewClick(Sender: TObject);
    procedure miTextPreferencesClick(Sender: TObject);
    procedure miDebugEventsClick(Sender: TObject);
    procedure miLuaEngineClick(Sender: TObject);
    procedure miPagingClick(Sender: TObject);
    procedure miUserdefinedCommentClick(Sender: TObject);
    procedure miUserDefinedHeaderClick(Sender: TObject);
    procedure Panel5Click(Sender: TObject);
    procedure RegisterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miLockRowsizeClick(Sender: TObject);
    procedure Showdebugtoolbar1Click(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Goto1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure miReplacewithnopsClick(Sender: TObject);

    procedure ShowDebugToolbar;
    procedure HideDebugToolbar;

    procedure FControl1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FControl1KeyPress(Sender: TObject; var Key: Char);
    procedure Gotoaddress1Click(Sender: TObject);
    procedure Search1Click(Sender: TObject);
    procedure Change1Click(Sender: TObject);
    procedure Addthisaddresstothelist1Click(Sender: TObject);
    procedure miAddToTheCodelistClick(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure ScrollBar2Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure miDebugRunClick(Sender: TObject);
    procedure miDebugStepClick(Sender: TObject);
    procedure miDebugStepOverClick(Sender: TObject);
    procedure miDebugRunTillClick(Sender: TObject);
    procedure Stacktrace1Click(Sender: TObject);
    procedure Threadlist1Click(Sender: TObject);
    procedure Assemble1Click(Sender: TObject);
    procedure HexEditKeyPress(Sender: TObject; var Key: Char);
    procedure HexEditExit(Sender: TObject);
    procedure HexEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RegisterLabelDblClick(Sender: TObject);
    procedure miDebugBreakClick(Sender: TObject);
    procedure Reservememory1Click(Sender: TObject);
    procedure Savememoryregion1Click(Sender: TObject);
    procedure Loadmemolryregion1Click(Sender: TObject);
    procedure HexEditDblClick(Sender: TObject);
    procedure Debugstrings1Click(Sender: TObject);
    procedure TextEditExit(Sender: TObject);
    procedure TextEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CreateThread1Click(Sender: TObject);
    procedure MemoryRegions1Click(Sender: TObject);
    procedure TextEditKeyPress(Sender: TObject; var Key: Char);
    procedure FillMemory1Click(Sender: TObject);
    procedure Disectwindow1Click(Sender: TObject);
    procedure Savedisassemledoutput1Click(Sender: TObject);
    procedure Heaps1Click(Sender: TObject);
    procedure EnumeratedllsandSymbols1Click(Sender: TObject);
    procedure InjectDLL1Click(Sender: TObject);
    procedure AutoInject1Click(Sender: TObject);
    procedure Dissectcode1Click(Sender: TObject);
    procedure Createjumptocodecave1Click(Sender: TObject);
    procedure Findstaticpointers1Click(Sender: TObject);
    procedure Scanforcodecaves1Click(Sender: TObject);
    procedure Changestateofregisteratthislocation1Click(Sender: TObject);
    procedure miTogglebreakpointClick(Sender: TObject);
    procedure Breakpointlist1Click(Sender: TObject);
    procedure Dissectdata1Click(Sender: TObject);
    procedure miShowSymbolsClick(Sender: TObject);
    procedure miDissectDataClick(Sender: TObject);
    procedure miShowModuleAddressesClick(Sender: TObject);
    procedure miUserdefinedSymbolsClick(Sender: TObject);
    procedure Allocatenonpagedmemory1Click(Sender: TObject);
    procedure Getaddress1Click(Sender: TObject);
    procedure Findmemory1Click(Sender: TObject);
    procedure Assemblycode1Click(Sender: TObject);
    procedure Driverlist1Click(Sender: TObject);
    procedure Sericedescriptortable1Click(Sender: TObject);
    procedure MBCanvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Cut1Click(Sender: TObject);
    procedure Pastefromclipboard1Click(Sender: TObject);
    procedure Setsymbolsearchpath1Click(Sender: TObject);
    procedure miKernelmodeSymbolsClick(Sender: TObject);
    procedure Breakandtraceinstructions1Click(Sender: TObject);
    procedure debuggerpopupPopup(Sender: TObject);
    procedure GDTlist1Click(Sender: TObject);
    procedure IDTlist1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Newwindow1Click(Sender: TObject);
    procedure Follow1Click(Sender: TObject);
    procedure copyBytesAndOpcodesClick(Sender: TObject);
    procedure DissectPEheaders1Click(Sender: TObject);
    procedure Back1Click(Sender: TObject);
    procedure Showvaluesofstaticaddresses1Click(Sender: TObject);
    procedure Findoutwhataddressesthisinstructionaccesses1Click(
      Sender: TObject);
    procedure sbShowFloatsClick(Sender: TObject);
    procedure ScriptConsole1Click(Sender: TObject);
    procedure DisplayTypeClick(Sender: TObject);
    procedure Showjumplines1Click(Sender: TObject);
    procedure Onlyshowjumplineswithinrange1Click(Sender: TObject);
    procedure View1Click(Sender: TObject);
    procedure Watchmemoryallocations1Click(Sender: TObject);
    procedure Maxstacktracesize1Click(Sender: TObject);
    procedure All1Click(Sender: TObject);
    procedure Modulesonly1Click(Sender: TObject);
    procedure Nonsystemmodulesonly1Click(Sender: TObject);
    procedure Referencedstrings1Click(Sender: TObject);
    procedure stacktrace2Click(Sender: TObject);
    procedure miDebugExecuteTillReturnClick(Sender: TObject);
    procedure lvStacktraceDataData(Sender: TObject; Item: TListItem);
    procedure lvStacktraceDataDblClick(Sender: TObject);
  private
    { Private declarations }

    hasBeenShown: boolean;

    R8Label: TLabel;
    R9Label: TLabel;
    R10Label: TLabel;
    R11Label: TLabel;
    R12Label: TLabel;
    R13Label: TLabel;
    R14Label: TLabel;
    R15Label: TLabel;

    xlabel: array [16..31] of tlabel;




    posloadedfromreg: boolean;
    displaytype: TDisplayType;


    editing: boolean;
    editing2: boolean;

    srow,scolumn: integer;


    memorylabelcount: integer;
    addresslabelcount: integer;
    addresslabel: array of TLabel;
    memorylabel: array of TLabel; //hex
    memoryLabelA: array of TLabel; //ascii
    MemoryLabelVerticalLines: integer;  //number of rows
    MemoryLabelHorizontalLines: Integer;  //the number of lines

    addressestext: array of string[8];
    memorytext: array of string[2];
    memorystring: array of string;
    lengthof8bytes: Integer;

//        x:tno

    lines: integer;
    oldlines: integer;
    Highlightcolor: Tcolor;

    fAccessedRegisterColor: TColor;
    fChangedRegisterColor: TColor;

    numberofaddresses: integer;


    part: integer;


    lastmodulelistupdate: integer;

    disassemblerHistory: TStringList;
    memorybrowserHistory: TStringList;
    assemblerHistory: TStringList;



    lastspecialwidth: integer;
    FShowValues: boolean;
    FShowDebugPanels: boolean;
    FStacktraceSize: integer;

    strace: Tstringlist;

    lastBreakpointCondition: record
      script: string;
      easy: boolean;
    end;

    bookmarks: array [0..9] of record
      addressString: string;
      lastAddress: ptruint;
      setMi: TMenuItem;
      gotoMi: TMenuItem;
    end;

    currentBinutils: Tbinutils;

    StackReference: ptruint;

    adjustedsize: boolean;
    registerpanelfont: TFont;

    overridebreakpointmethod: boolean;
    preferedF5BreakpointMethod: TBreakpointMethod;

    followRegister: PContextElement_register;

    fcr3: qword;
    fcr3switcher: TfrmCR3Switcher;
    procedure cr3switcherCR3Change(sender: TObject);
    procedure SetStacktraceSize(size: integer);
    procedure setShowDebugPanels(state: boolean);
    function getShowValues: boolean;
    procedure setShowValues(newstate: boolean);


    procedure disassemblerviewDblClick(Sender: TObject);
    procedure setHexviewAddress(a: ptrUint);
    function getHexviewAddress:ptrUint;
    procedure hexviewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ApplyFollowRegister;
    procedure setCR3(newcr3: qword);
    function ReadProcessMemory(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: size_t; var lpNumberOfBytesRead: PTRUINT): BOOL;

    procedure setCaption(c: string);
    function getCaption: string;

  public
    { Public declarations }
    FSymbolsLoaded: Boolean;



    thhandle: Thandle;

    disassemblerview: TDisassemblerview;
    hexview: THexview;

    context: pointer;
    contexthandler: TContextInfo;


    laststack: pbytearray;

    disassembler: boolean;
    cancelsearch: boolean;

    ischild: boolean; //determines if it's the main memorybrowser or a child
//    backlist: TStack;

    frmiptlog: TfrmIPTLogDisplay;




    procedure setRegisterPanelFont(f: TFont);

    procedure FindwhatThiscodeAccesses(address: ptrUint);
    procedure DBVMFindwhatThiscodeAccesses(address: ptrUint);

    procedure AssemblePopup(x: string);

    procedure plugintype1click(sender:tobject);
    procedure plugintype6click(sender:tobject);
    procedure setcodeanddatabase;
    procedure GetEntryPointAndDataBase(var code: ptrUint; var data: ptrUint);
    property showvalues: boolean read getShowValues write setShowValues;
    property showDebugPanels: boolean read fShowDebugPanels write setShowDebugPanels;
    property stacktraceSize: integer read FStacktraceSize write SetStacktraceSize;
    property memoryaddress: ptrUint read getHexviewAddress write setHexviewAddress;
    procedure reloadStacktrace;
    function GetReturnaddress: ptrUint;

    procedure OnMemoryViewerRunning;

    procedure UpdateDebugContext(threadhandle: THandle; threadid: dword; changeSelection: boolean=true; _debuggerthread: TDebuggerthread=nil);
    procedure miLockOnClick(Sender: TObject);
    procedure miLockMemviewClick(sender: TObject);

    procedure miDifferenceClick(Sender: TObject);
    procedure miStopDifferenceClick(Sender: TObject);
    procedure Scrollboxscroll(sender: TObject);

    procedure createcr3switcher;
    property cr3switcher: TfrmCR3Switcher read fcr3switcher;
  published
    //support for old scripts that reference these
    property Run1: TMenuItem read miDebugRun;
    property Step1: TMenuItem read miDebugStep;
    property StepOver1: TMenuItem read miDebugStepOver;
    property Executetillreturn1: TMenuItem read miDebugExecuteTillReturn;
    property RunTill1: TMenuItem read miDebugRunTill;
    property miSetAddress: TMenuItem read miDebugSetAddress;
    property Setbreakpoint1: TMenuItem read miDebugToggleBreakpoint;
    property Showsymbols1: TMenuItem read miShowSymbols;
    property Kernelmodesymbols1: TMenuItem read miKernelmodeSymbols;
    property Showmoduleaddresses1: TMenuItem read miShowModuleAddresses;
    property Symbolhandler1: TMenuItem read miUserdefinedSymbols;
    property AccessedRegisterColor: TColor read faccessedRegisterColor write faccessedRegisterColor;
    property ChangedRegisterColor: TColor read fChangedRegisterColor write fChangedRegisterColor;
    property CR3: QWORD read fCR3 write setCR3;
    property Caption: string read getCaption write setCaption;
  end;

var
  MemoryBrowser: TMemoryBrowser;
  mbchildcount: integer; //global so all other children can increase it as well


  MemoryBrowsers: TList; //contains a list of all the memorybrowsers


  
implementation

uses Valuechange, MainUnit, debugeventhandler, findwindowunit,
  frmstacktraceunit, frmBreakThreadUnit, FormDebugStringsUnit,
  frmDissectWindowUnit, frmEnumerateDLLsUnit, frmThreadlistunit,
  formmemoryregionsunit, frmHeapsUnit, frmFindstaticsUnit,
  frmModifyRegistersUnit, savedisassemblyfrm, frmBreakpointlistunit,
  AdvancedOptionsUnit, frmautoinjectunit, formsettingsunit,
  frmSaveMemoryRegionUnit, frmLoadMemoryunit, inputboxtopunit,
  formAddToCodeList, frmFillMemoryUnit, frmCodecaveScannerUnit, FoundCodeUnit,
  frmSelectionlistunit, symbolconfigunit, frmFloatingPointPanelUnit,
  frmTracerUnit, dissectcodeunit, driverlist, formChangedAddresses, peINFOunit,
  frmGDTunit, frmIDTunit, frmDisassemblyscanunit, ServiceDescriptorTables,
  frmReferencedStringsUnit, frmReferencedFunctionsUnit, Structuresfrm,
  Structuresfrm2, pointerscannerfrm, frmDebugEventsUnit, frmPagingUnit,
  frmluaengineunit, disassemblerviewlinesunit, frmBreakpointConditionunit,
  frmStringMapUnit, frmStringpointerscanUnit, frmFilePatcherUnit,
  frmUltimapUnit, frmUltimap2Unit, frmAssemblyScanUnit, MemoryQuery,
  AccessedMemory, Parsers, GnuAssembler, frmEditHistoryUnit, frmWatchlistUnit,
  vmxfunctions, frmstructurecompareunit, globals, UnexpectedExceptionsHelper,
  frmExceptionRegionListUnit, frmExceptionIgnoreListUnit, frmcodefilterunit,
  frmDBVMWatchConfigUnit, DBK32functions, DPIHelper, DebuggerInterface,
  DebuggerInterfaceAPIWrapper, BreakpointTypeDef, CustomTypeHandler,
  frmSourceDisplayUnit, sourcecodehandler, tcclib, mainunit2
  {$ifdef ONEBYTEJUMPS}, autoassemblerexeptionhandler{$endif};


resourcestring
  rsUnlinkFromOtherHexview = 'Unlink from other hexview';
  rsLinkWithOtherHexview = 'Link with other hexview';
  rsStopShowingTheDifference = 'Stop showing the difference';
  rsShowDifference = 'Show difference';
  rsBetween = 'Between %s';
  rsHowLongShouldAChangeBeShown = 'How long should a change be shown?';
  rsChangeDisplayFader = 'Change display fader';
  rsGotoAddress = 'Goto Address';
  rsFillInTheAddressYouWantToGoTo = 'Fill in the address you want to go to';
  rsMemoryViewerRunning = 'Memory Viewer - Running';
  rsCheatEngineSingleLingeAssembler = 'Single-line assembler';
  rsTypeYourAssemblerCodeHereAddress = 'Type your assembler code here: (address=%s)';
  rsTheGeneratedCodeIsByteSLongButTheSelectedOpcodeIsB = 'The generated code is %s byte(s) long, but the selected opcode is %s byte(s) long! Do you want to replace the '
    +'incomplete opcode(s) with NOP''s?';
  rsIDonTUnderstandWhatYouMeanWith = 'I don''t understand what you mean with %s';
  rsChangeRegister = 'Change register';
  rsWhatIsTheNewValueOf = 'What is the new value of %s?';
  rs0Or1 = '(0 or 1)';
  rsIsNotAValidValue = '%s is not a valid value';
  rsAllocateMemory = 'Allocate memory';
  rsHowMuchMemoryDoYouWantToAddToThisProcess = 'How much memory do you want to add to this process?';
  rsHowMuchIs = 'How much is %s?';
  rsErrorAllocatingMemory = 'Error allocating memory!';
  rsAtLeastBytesHaveBeenAllocatedAtDoYouWantToGoThereN = 'At least %s bytes have been allocated at %s%sDo you want to go there now?';
  rsCreateRemoteThread = 'Create remote thread';
  rsWhatWillBeTheStartaddressOfThisThread = 'What will be the startaddress of this thread?';
  rsPleaseEnterAValidHexadecimalAddres = 'Please enter a valid hexadecimal addres';
  rsYouWantToGiveAnAdditional32BitParameterWillShowUpI = 'You want to give an additional 32-bit parameter? (Will show up in (R)/(E)BX)';
  rsPleaseEnterAValidHexadecimalValue = 'Please enter a valid hexadecimal value';
  rsPleaseTargetAProcessFirst = 'Please target a process first';
  rsPleaseTargetAnotherProcess = 'Start another version of '+strCheatEngine+' and attach to that instead';
  rsDoYouWantToExecuteAFunctionOfTheDll = 'Do you want to execute a function of the dll?';
  rsInjectDll = 'Inject dll';
  rsSelectTheFunctionYouWantToExecute = 'Select the function you want to execute';
  rsDLLInjected = 'DLL Injected';
  rsHowMuchMemoryDoYouWishToAllocate = 'How much memory do you wish to allocate?';
  rsAtLeastBytesHaveBeenAllocatedAtGoThereNow = 'At least %s bytes have been allocated at %s. Go there now?';
  rsGetKernelAddress = 'Get kernel address';
  rsGiveTheNameOfTheFunctionYouWantToFindCaseSensitive = 'Give the name of the function you want to find (Case sensitive,certain words can cause blue screens)';
  rsAssemblyScan = 'Assembly scan';
  rsInputTheAssemblyCodeToFindWilcardsSupported = 'Input the assembly code to find. Wildcards( * ) supported.';
  rsSymbolHandler = 'Symbol handler';
  rsPleaseSpecifyTheNewSymbolSearchpathSeperatesPaths = 'Please specify the new symbol searchpath (; separates paths)';
  rsMemoryBrowser = 'MemoryBrowser';
  rsReturnAddress = 'Return Address';
  rsParameters = 'Parameters';
  rsAddress = 'Address';
  rsValue = 'Value';
  rsStacktrace = 'Stacktrace';
  rsNewSize = 'New size';
  rsMaxStacktraceSize = 'Max stacktrace size';
  rsNeedToRunDissectCode = 'You will need to run the dissect code routine first before this window is usable. Run it now?';
  rsMemoryViewerCurrentlyDebuggingThread = 'Memory Viewer - Currently debugging thread %s';
  rsRestoreWithOrginalCode = 'Restore with original code';
  rsReplaceWithCodeThatDoesNothing = 'Replace with code that does nothing';
  rsComment = 'Comment';
  rsCommentFor = 'Comment for %s';
  rsHeaderFor = 'Header for %s';
  rsSShowsTheAutoguessValue = '(%s shows the autoguess value)';
  rsMBBookmark = 'Bookmark %d';
  rsMBBookmark2 = 'Bookmark %d: %s';
  rsMBCreationOfTheRemoteThreadFailed = 'Creation of the remote thread failed';
  rsMBThreadCreated = 'Thread Created';
  rsBecauseOfUnhandledExeption = 'Because of unhandled exception %s';
  rsSomethingHappened = 'something happened';
  rsSetBreakpoint = 'Set breakpoint';
  rsRemoveBreakpoint = 'Remove breakpoint';
  rsInjectDYLIB = 'Inject DYLIB';
  rsSetCustomAlignment = 'Set custom alignment';

//property functions:
function TMemoryBrowser.getShowValues: boolean;
begin
  result:=FShowValues;
end;

procedure TMemoryBrowser.setShowValues(newstate: boolean);
begin
  Showvaluesofstaticaddresses1.checked:=newstate;
  FShowValues:=newstate;
  if disassemblerview<>nil then
    disassemblerview.setCommentsTab(FShowValues);
end;

procedure TMemoryBrowser.setShowDebugPanels(state: boolean);
var
  oldstackwidth: integer;
  oldpanel3width: integer;

  cw: integer;
  ew: integer;
begin
  oldstackwidth:=pnlStacktrace.Width;
  oldpanel3width:=panel3.width;
  {if state then
  begin
    //resizing should change the stack, not the hexview
    panel3.Align:=alLeft;
    splitter3.Align:=alLeft;
    pnlStacktrace.align:=alclient;
    splitter3.ResizeControl:=pnlStacktrace;
  end
  else
  begin
    splitter3.ResizeControl:=panel3;
    pnlStacktrace.align:=alRight;
    splitter3.Align:=alRight;
    panel3.Align:=alclient;
  end; }

  FShowDebugPanels:=state;
  registerview.Visible:=state;
  pnlStacktrace.Visible:=state;
  splitter2.Visible:=state;
  splitter3.Visible:=state;

  pnlStacktrace.width:=oldstackwidth;
end;

procedure TMemoryBrowser.SetStacktraceSize(size: integer);
var x: ptrUint;
begin
  if (context=nil) or (contexthandler=nil) then exit;
  FStacktraceSize:=size;

  if laststack<>nil then
    freememandnil(laststack);

  laststack:=getmem(size);
  readprocessmemory(processhandle, pointer(contexthandler.StackPointerRegister^.getValue(context)), laststack, size, x);

  reloadStacktrace;
end;

procedure TMemoryBrowser.setCaption(c: string);
var cr3pos, cr3posend: integer;
begin
  cr3pos:=pos(' (CR3 ',c);
  if (fcr3<>0) and (cr3pos=0) then //add the statement which CR3 this is
    c:=c+' (CR3 '+inttohex(fcr3,8)+')';


  if (fcr3=0) and (cr3pos<>0) then //delete it
  begin
    cr3posend:=Pos(')',c,cr3pos+1);
    if cr3posend>0 then
      c:=copy(c,1,cr3pos-1)+copy(c,cr3posend+1);
  end;

  inherited caption:=c;
end;

function TMemoryBrowser.getCaption: string;
begin
  result:=inherited caption;
end;

//^^^^


function TMemoryBrowser.ReadProcessMemory(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: size_t; var lpNumberOfBytesRead: PTRUINT): BOOL;
begin
  if fcr3=0 then
    result:={$ifdef windows}newkernelhandler.{$endif}{$ifdef darwin}macport.{$endif}ReadProcessMemory(hProcess, lpBaseAddress, lpBuffer, nsize, lpNumberOfBytesRead)
  {$ifdef windows}
  else
    result:=ReadProcessMemoryCR3(fcr3,lpBaseAddress, lpBuffer, nsize, lpNumberOfBytesRead){$endif};
end;



procedure TMemoryBrowser.Splitter1Moved(Sender: TObject);
begin
  disassemblerview.Update;
end;

procedure TMemoryBrowser.miLockRowsizeClick(Sender: TObject);
var
  rs: string;
  waslocked: boolean;
  newsize: integer;
begin
  waslocked:=hexview.LockedRowSize<>0;


  hexview.LockRowsize; //fills in lockedRowSize
  rs:=inttostr(hexview.LockedRowSize);

  if InputQuery('Alignment','Enter the custom alignment (<=0 is automatic)', rs) then
  begin
    try
      newsize:=StrToInt(rs);
      if newsize<=0 then newsize:=0;

      hexview.LockedRowSize:=newsize;
    except
      if waslocked=false then
        hexview.UnlockRowsize;
    end;
  end
  else
  begin
    if waslocked=false then
      hexview.UnlockRowsize;
  end;

end;

procedure TMemoryBrowser.ShowDebugToolbar;
begin
  tbDebug.Visible:=true;
  tbDebug.Tag:=0;
  Showdebugtoolbar1.Checked:=true;
end;

procedure TMemoryBrowser.HideDebugToolbar;
begin
  tbDebug.Visible:=false;
  Showdebugtoolbar1.Checked:=false;
end;

procedure TMemoryBrowser.Showdebugtoolbar1Click(Sender: TObject);
begin
  if tbDebug.Visible=false then ShowDebugToolbar
  else HideDebugToolbar;
end;

procedure TMemoryBrowser.RegisterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var s: string;
i: integer;
begin
  if button = mbright then
  begin
    if (sender is TLabel) then
    begin
      s:=trim(tlabel(sender).Caption);
      i:=pos(' ',s);
      if i>0 then //should always be true
      begin
        s:=copy(s,i+1,length(s));

        clipboard.AsText:=s;
      end;
    end;
  end;
end;

procedure TMemoryBrowser.miDebugEventsClick(Sender: TObject);
begin
  if frmDebugEvents=nil then
    frmDebugEvents:=tfrmDebugEvents.create(application);

  frmDebugEvents.show;
end;

procedure TMemoryBrowser.miLuaEngineClick(Sender: TObject);
begin
  //start lua engine window
  if frmLuaEngine=nil then
    frmLuaEngine:=TfrmLuaEngine.create(MemoryBrowser); //main mb

  frmLuaEngine.show;

end;

procedure TMemoryBrowser.miPagingClick(Sender: TObject);
begin
  TfrmPaging.create(application).show;
end;

procedure TMemoryBrowser.miUserdefinedCommentClick(Sender: TObject);
var
  s: tstringlist;
begin
  s:=tstringlist.create;
  try
    s.text:=dassemblercomments.comments[disassemblerview.SelectedAddress];
    if multilineinputquery(Format(rsCommentFor, [inttohex(disassemblerview.SelectedAddress, 8)]), Format(rsCommentFor, [inttohex(disassemblerview.SelectedAddress, 8)])+' '+rsSShowsTheAutoguessValue, s) then
      dassemblercomments.comments[disassemblerview.SelectedAddress]:=utf8toansi(s.text);
  finally
    s.free;
  end;

  disassemblerview.Refresh;
end;

procedure TMemoryBrowser.miUserDefinedHeaderClick(Sender: TObject);
var
  s: tstringlist;
begin
  s:=tstringlist.create;
  try
    s.text:=ansitoutf8(dassemblercomments.commentHeader[disassemblerview.SelectedAddress]);
    if multilineinputquery(rsHeaderFor, Format(rsCommentFor, [inttohex(disassemblerview.SelectedAddress, 8)])+' '+rsSShowsTheAutoguessValue, s) then
      dassemblercomments.commentHeader[disassemblerview.SelectedAddress]:=utf8toansi(s.text);
  finally
    s.free;
  end;

  disassemblerview.Refresh;
end;

procedure TMemoryBrowser.Panel5Click(Sender: TObject);
begin

end;

procedure TMemoryBrowser.miLockOnClick(Sender: TObject);
begin
  hexview.unlock;
end;

procedure TMemoryBrowser.miLockMemviewClick(sender: TObject);
begin
  hexview.Lock(TMemoryBrowser(MemoryBrowsers[(sender as TMenuItem).tag]).hexview);
end;

procedure TMemoryBrowser.miDifferenceClick(Sender: TObject);
begin
  hexview.ShowDifference(TMemoryBrowser(MemoryBrowsers[(sender as TMenuItem).tag]).hexview);
end;

procedure TMemoryBrowser.miStopDifferenceClick(Sender: TObject);
begin
  hexview.EndDifferenceView;
end;



procedure TMemoryBrowser.memorypopupPopup(Sender: TObject);
var
  m: TMemorybrowser;
  mi: Menus.TMenuItem;
  i,j: integer;

  islocked: boolean;
  a,a2: ptruint;
  hasbp: boolean;

  mbi: TMEMORYBASICINFORMATION;


begin
  //update customtypes


  for i:=0 to customTypes.Count-1 do
  begin
    j:=dispDouble.MenuIndex+1+i;
    if j<DisplayType1.Count then
      mi:=displayType1.Items[j]
    else
    begin
      mi:=tmenuitem.create(memorypopup);
      mi.OnClick:=DisplayTypeClick;
      mi.AutoCheck:=true;
      mi.RadioItem:=true;
      mi.GroupIndex:=3;
      DisplayType1.Add(mi);
    end;

    mi.caption:=TCustomType(customtypes[i]).name;
    mi.tag:=$1000+i;
  end;

  //delete the entries that are too many
  while DisplayType1.Count>dispDouble.MenuIndex+customTypes.Count+1 do
    DisplayType1.Delete(dispDouble.MenuIndex+customTypes.Count+1);


  miShowDifference.clear;
  miLock.Clear;

  islocked:=hexview.isLocked;
  if isLocked then
  begin
    miLock.OnClick:=miLockOnClick;
    miLock.caption:=rsUnlinkFromOtherHexview;
  end
  else
  begin
    miLock.OnClick:=nil;
    miLock.caption:=rsLinkWithOtherHexview;
  end;

  if hexview.isShowingDifference then
  begin
    miShowDifference.caption:=rsStopShowingTheDifference;
    miShowDifference.onclick:=miStopDifferenceClick;
  end
  else
  begin
    miShowDifference.caption:=rsShowDifference;
    miShowDifference.onclick:=nil;
  end;

  miLock.enabled:=MemoryBrowsers.count>1;

  for i:=0 to MemoryBrowsers.count-1 do
  begin
    m:=Memorybrowsers[i];
    if m<>self then
    begin
      if not hexview.isShowingDifference then
      begin
        mi:=TMenuItem.Create(miShowDifference);
        mi.Caption:=Format(rsBetween, [m.Caption]);
        mi.OnClick:=miDifferenceClick;
        mi.tag:=i;
        miShowDifference.Add(mi);
      end;

      if not islocked then
      begin
        mi:=TMenuItem.Create(miLock);
        mi.caption:=m.caption;
        mi.OnClick:=miLockMemviewClick;
        mi.tag:=i;
        miLock.add(mi);
      end;
    end;

  end;


  if (hexview.hasSelection) then
  begin
    hexview.GetSelectionRange(a,a2);

    hasbp:=(debuggerthread<>nil) and (debuggerthread.isBreakpoint(a,a2)<>nil);
    miDataBreakPointMenu.visible:=(not ischild) and (not hasbp);
    miWatchWrite.visible:=not hasbp;
    miWatchAccess.visible:=not hasbp;
    MenuItem7.visible:=not hasbp;
    miBreakAndTrace.visible:=not hasbp;
    miDeleteBP.visible:=hasbp;

    miHVFollow.Visible:=hexview.CanFollow;
    Addthisaddresstothelist1.visible:=true;
  end
  else
  begin
    miDataBreakPointMenu.visible:=false;
    miWatchWrite.visible:=false;
    miWatchAccess.visible:=false;
    MenuItem7.visible:=false;
    miBreakAndTrace.visible:=false;
    miDeleteBP.visible:=false;

    Addthisaddresstothelist1.visible:=false;
  end;

  miHVBack.visible:=hexview.hasBackList;
  miShowRelative.checked:=hexview.UseRelativeBase;



  miChangeProtectionRWE.checked:=false;
  miChangeProtectionRE.checked:=false;
  miChangeProtectionRW.checked:=false;
  miChangeProtectionR.checked:=false;
  if VirtualQueryEx(processhandle, pointer(hexview.Address), mbi, sizeof(mbi))=sizeof(mbi) then
  begin
    miChangeProtectionRWE.checked:=((mbi.Protect and PAGE_EXECUTE_READWRITE)=PAGE_EXECUTE_READWRITE) or ((mbi.Protect and PAGE_EXECUTE_WRITECOPY)=PAGE_EXECUTE_WRITECOPY);
    miChangeProtectionRE.checked:=((mbi.Protect and PAGE_EXECUTE)=PAGE_EXECUTE) or ((mbi.Protect and PAGE_EXECUTE_READ)=PAGE_EXECUTE_READ);
    miChangeProtectionRW.checked:=((mbi.Protect and PAGE_READWRITE)=PAGE_READWRITE) or ((mbi.Protect and PAGE_WRITECOPY)=PAGE_WRITECOPY);
    miChangeProtectionR.checked:=((mbi.Protect and PAGE_READONLY)=PAGE_READONLY);
  end;

  //
  miWatchBPHardware.enabled:=(CurrentDebuggerInterface=nil) or (dbcHardwareBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities);
  miWatchBPException.enabled:=(CurrentDebuggerInterface=nil) or (dbcSoftwareBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities);
  miWatchBPDBVM.enabled:=(CurrentDebuggerInterface=nil) or (dbcExceptionBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities);
  miWatchBPDBVM.visible:={$ifdef windows}hasEPTSupport{$else}false{$endif};

  if hexview.LockedRowSize<>0 then
    miLockRowsize.caption:=rsSetCustomAlignment+' ('+inttostr(hexview.LockedRowSize)+')'
  else
    miLockRowsize.caption:=rsSetCustomAlignment;

end;


procedure TMemoryBrowser.MenuItem10Click(Sender: TObject);
begin
  if frmStringMap=nil then
    frmStringMap:=TfrmStringMap.Create(application);

  frmStringMap.show;
end;

procedure TMemoryBrowser.miSearchForAccessibleStringsClick(Sender: TObject);
begin
  if frmStringPointerscan=nil then
    frmStringpointerscan:=Tfrmstringpointerscan.create(MemoryBrowser);

  frmStringpointerscan.rbStringscan.Checked:=true;
  frmStringpointerscan.edtBase.text:=inttohex(hexview.address,8);

  frmStringpointerscan.show;
end;

procedure TMemoryBrowser.MenuItem12Click(Sender: TObject);
var f: Tfrmfilepatcher;
begin
  f:=tfrmfilepatcher.create(self);
  f.showmodal;
  f.free;
end;



procedure TMemoryBrowser.MenuItem14Click(Sender: TObject);
begin
  EnableWindowsSymbols(true);
end;

procedure TMemoryBrowser.MenuItem8Click(Sender: TObject);
begin

end;



procedure TMemorybrowser.setCR3(newcr3: qword);
var oldcr3: qword;
begin
  oldcr3:=fcr3;
  if debuggerthread<>nil then debuggerthread.execlocation:=4120;

  fcr3:=newcr3;
  disassemblerview.cr3:=fcr3;
  if debuggerthread<>nil then debuggerthread.execlocation:=4121;
  hexview.cr3:=fcr3;
  if debuggerthread<>nil then debuggerthread.execlocation:=4122;

  if (newcr3<>oldcr3) then
  begin
    if debuggerthread<>nil then debuggerthread.execlocation:=4123;
    createcr3switcher;
    if debuggerthread<>nil then debuggerthread.execlocation:=4124;
    fcr3switcher.addCR3ToList(newcr3);
    if debuggerthread<>nil then debuggerthread.execlocation:=4125;
    fcr3switcher.Show;
    if debuggerthread<>nil then debuggerthread.execlocation:=4126;
  end;

  caption:=caption;
end;

procedure TMemoryBrowser.cr3switcherCR3Change(sender: TObject);
begin
  //cr3 changed, notify the disassembler and hexview
  cr3:=cr3switcher.cr3;
end;

procedure TMemoryBrowser.createcr3switcher;
begin
  if fcr3switcher=nil then
  begin
    fcr3switcher:=TfrmCR3Switcher.Create(self);
    fcr3switcher.OnCR3Change:=cr3switcherCR3Change;
  end;
end;

procedure TMemoryBrowser.miCR3SwitcherClick(Sender: TObject);
begin
  //the cr3 switcher is unique for each memview window
  createcr3switcher;
  fcr3switcher.Show;
end;

procedure TMemoryBrowser.miDBVMFindoutwhataddressesthisinstructionaccessesClick(Sender: TObject);
begin
  DBVMFindwhatThiscodeAccesses(disassemblerview.SelectedAddress);
end;

procedure TMemoryBrowser.MenuItem4Click(Sender: TObject);
begin
  if tbDebug.Visible=true then HideDebugToolbar;
  tbDebug.Tag:=-1;
end;

procedure TMemoryBrowser.miIPTLogClick(Sender: TObject);
var log: pointer;
    logsize: integer;
begin
  {$IFDEF WINDOWS}
  log:=nil;
  if (debuggerthread<>nil) and (debuggerthread.CurrentThread<>nil) then
  begin
    //broken state
    if debuggerthread.CurrentThread.getLastIPTLog(log,logsize)=false then
    begin
      if useintelptfordebug=false then
      begin
        if messagedlg('This will require the IPT feature to be enabled in the debugger. Activate it now? (You will need a new break to record a log)', mtConfirmation, [mbyes,mbno],0)=mryes then
        begin
          useintelptfordebug:=true;
          debuggerthread.initIntelPTTracing;
        end;
      end
      else
      begin
        if debuggerthread.initIntelPTTracing then
          messagedlg('Error retrieving the IPT log. Try again later', mtError, [mbok],0)
        else
          messagedlg('Error activating the IPT log', mtError, [mbok],0)
      end;
      exit;
    end;

    if frmiptlog=nil then
    begin
      frmiptlog:=TfrmIPTLogDisplay.Create(MemoryBrowser);
      frmiptlog.OnClose:=nil;
    end;

    frmiptlog.show;
    if log<>nil then
    begin
      frmiptlog.loadlog(debuggerthread.CurrentThread.ThreadId.ToHexString+'-'+GetTickCount.ToHexString,log,logsize, contexthandler.InstructionPointerRegister^.getValue(context));
      freemem(log);
    end;

  end;
  {$ENDIF}

end;

procedure TMemoryBrowser.miOpenInDissectDataClick(Sender: TObject);
begin
  //create it
  with tfrmstructures2.create(application) do
  begin
    initialaddress:=hexview.SelectionStart;
    show;
  end;

end;

procedure TMemoryBrowser.miShowRelativeDisassemblerClick(Sender: TObject);
begin
  if miShowRelativeDisassembler.checked then
  begin
    if disassemblerview.SelectedAddress<>0 then
    begin
      disassemblerview.RelativeBase:=disassemblerview.SelectedAddress;
      disassemblerview.UseRelativeBase:=true;
    end
    else
    begin
      disassemblerview.RelativeBase:=disassemblerview.TopAddress;
      disassemblerview.UseRelativeBase:=true;
    end;
  end
  else
    disassemblerview.UseRelativeBase:=false;

  disassemblerview.update;
end;



procedure TMemoryBrowser.miUndoLastEditClick(Sender: TObject);
begin
  if logWrites then
  begin
    undowrite(disassemblerview.SelectedAddress);
  end;
end;

procedure TMemoryBrowser.ApplyFollowRegister;
begin
  if (followRegister<>nil) and (context<>nil) then
    hexview.address:=followRegister^.getValue(context);
end;

procedure TMemoryBrowser.miFollowInHexviewClick(Sender: TObject);
begin
  if miFollowInHexview.checked then
    followRegister:=PContextElement_register(pmRegisters.PopupComponent.Tag)
  else
    followRegister:=nil;
end;

procedure TMemoryBrowser.miSetSpecificBreakpointClick(Sender: TObject);
begin

end;

procedure TMemoryBrowser.miSetBreakpointClick(Sender: TObject);
var bpm: TBreakpointMethod;
begin
  if startdebuggerifneeded(true) then
  begin
    case tmenuitem(sender).tag  of
      0: bpm:=bpmDebugRegister;
      1: bpm:=bpmInt3;
      2: bpm:=bpmException;
      3: bpm:=bpmDBVM;
    end;

    overridebreakpointmethod:=true;
    preferedF5BreakpointMethod:=bpm;

    try
      DebuggerThread.SetOnExecuteBreakpoint(disassemblerview.SelectedAddress, bpm);
      disassemblerview.Update;
    except
      on e:exception do MessageDlg(e.message,mtError,[mbok],0);
    end;
  end;
end;

procedure TMemoryBrowser.miCodeFilterClick(Sender: TObject);
begin
  if frmcodefilter=nil then
    frmcodefilter:=tfrmcodefilter.create(application);

  frmcodefilter.show;
end;

procedure TMemoryBrowser.miDBVMActivateCloakClick(Sender: TObject);
var
  PA,VA: qword;
begin
  {$ifdef windows}
  if isRunningDBVM and hasEPTSupport and (not hasCloakedRegionInRange(disassemblerview.SelectedAddress,1,VA,PA)) then
  begin
    VA:=disassemblerview.SelectedAddress;

    if GetPhysicalAddress(processhandle,pointer(VA),PA) then
      dbvm_cloak_activate(PA,VA)
    else
      MessageDlg('Failed obtaining the physical address of this memory', mtError, [mbok],0);
  end;
  {$endif}
end;

procedure TMemoryBrowser.miDBVMDisableCloakClick(Sender: TObject);
var PA,VA: Qword;
begin
  {$ifdef windows}
  if isRunningDBVM and hasEPTSupport and (hasCloakedRegionInRange(disassemblerview.SelectedAddress,1,VA,PA)) then
    dbvm_cloak_deactivate(PA);
  {$endif}
end;

procedure TMemoryBrowser.miHideToolbarClick(Sender: TObject);
begin
  if tbDebug.Visible=true then HideDebugToolbar;
end;

procedure TMemoryBrowser.miUltimapClick(Sender: TObject);
begin
  if frmUltimap=nil then
    frmUltimap:=TfrmUltimap.create(application);

  frmUltimap.show;
end;

procedure TMemoryBrowser.MenuItem17Click(Sender: TObject);
begin
  //build a structure using a registered template and the current data stream
end;

procedure TMemoryBrowser.MenuItem18Click(Sender: TObject);
begin
  TfrmMemoryViewEx.create(self).show;
end;

procedure TMemoryBrowser.miLoadTraceClick(Sender: TObject);
var t: Tfrmtracer;
begin
  t:=TFrmTracer.create(self,false,true);

  try
    t.miLoad.Click;
  except
    on e: exception do
    begin
      t.free; //not needed anymore
      messagedlg(e.Message, mterror, [mbok],0);
      exit;
    end;
  end;

  t.show;

end;

procedure TMemoryBrowser.miWatchMemoryPageAccessClick(Sender: TObject);
begin
  if frmAccessedMemory=nil then
    frmAccessedMemory:=TfrmAccessedMemory.Create(application);

  frmAccessedMemory.Show;
end;

procedure TMemoryBrowser.miUserWriteIntegerClick(Sender: TObject);
begin
  if frmEditHistory=nil then
    frmEditHistory:=tfrmEditHistory.create(application);

  frmEditHistory.show;

end;

procedure TMemoryBrowser.miUltimap2Click(Sender: TObject);
begin
  if frmUltimap2=nil then
    frmUltimap2:=TfrmUltimap2.create(application);

  frmUltimap2.show;
end;

procedure TMemoryBrowser.miWatchListClick(Sender: TObject);
begin
  if frmWatchlist=nil then
    frmWatchlist:=tfrmWatchlist.create(Application);

  frmWatchlist.UpdateContext(context);
  frmWatchlist.show;
end;

procedure TMemoryBrowser.miCompareStructuresClick(Sender: TObject);
begin
  if frmStructureCompare=nil then
    frmStructureCompare:=tfrmStructureCompare.create(application);

  frmStructureCompare.show;
end;



procedure TMemoryBrowser.miDebugSetAddressClick(Sender: TObject);
begin
  if (debuggerthread<>nil) and (debuggerthread.isWaitingToContinue) then
  begin
    debuggerthread.CurrentThread.context.{$ifdef cpu32}eip{$else}Rip{$endif}:=disassemblerview.SelectedAddress;
    debuggerthread.CurrentThread.setContext;
    debuggerthread.CurrentThread.UpdateMemoryBrowserContext;
    UpdateDebugContext(debuggerthread.CurrentThread.Handle, debuggerthread.CurrentThread.ThreadId);
  end;

end;

procedure TMemoryBrowser.miGNUAssemblerClick(Sender: TObject);
var gnua: TfrmAutoInject;
begin
  gnua:=TfrmAutoInject.Create(self);
  gnua.ScriptMode:=smGnuAssembler;
  gnua.show;
end;

procedure TMemoryBrowser.miBinutilsSelectClick(Sender: TObject);
var id: integer;
begin
  if (sender is TMenuItem) then
  begin
    id:=tmenuitem(sender).tag;
    if (id<0) or (id>=binutilslist.count) then
    begin
      defaultBinutils:=nil;
      miDisassemblerType.Enabled:=true;
    end
    else
    begin
      defaultBinutils:=TBinUtils(binutilslist[id]);
      miDisassemblerType.Enabled:=false;
    end;
  end;
end;

procedure TMemoryBrowser.miRunUnhandledClick(Sender: TObject);
begin
  if debuggerthread<>nil then
    debuggerthread.continueDebugging(co_run, 0, false);

  OnMemoryViewerRunning;
  caption:=rsMemoryViewerRunning;
end;

procedure TMemoryBrowser.miShowRelativeClick(Sender: TObject);
begin
  if miShowRelative.checked then
  begin
    if hexview.HasSelection then
    begin
      hexview.RelativeBase:=hexview.SelectionStart;
      hexview.UseRelativeBase:=true;
    end
    else
    begin
      hexview.RelativeBase:=hexview.TopAddress;
      hexview.UseRelativeBase:=true;
    end;
  end
  else
    hexview.UseRelativeBase:=false;

  hexview.update;
end;

procedure TMemoryBrowser.miSVCopyClick(Sender: TObject);
var
  i,j: integer;
  s: tstringlist;
  str: string;
begin
  s:=tstringlist.create;
  try

    for i:=0 to lvStacktraceData.Items.Count-1 do
    begin
      if lvStacktraceData.Items[i].Selected then
      begin
        str:=PadRight(lvStacktraceData.items[i].caption,20);

        for j:=0 to lvStacktraceData.items[i].SubItems.Count-1 do
          str:=str+' - '+PadRight(lvStacktraceData.items[i].SubItems[j],20);

        s.add(str);
      end;

    end;

    clipboard.AsText:=s.Text;

  finally
    s.free;
  end;

end;

procedure TMemoryBrowser.miUnexpectedExceptionBreakOptionClick(Sender: TObject);
begin
  case TMenuItem(Sender).tag of
    0: UnexpectedExceptionAction:=ueaIgnore;
    1: UnexpectedExceptionAction:=ueaBreak;
    2: UnexpectedExceptionAction:=ueaBreakIfInRegion;
  end;
end;

procedure TMemoryBrowser.pmRegistersPopup(Sender: TObject);
begin
  miFollowInHexview.checked:=(followRegister<>nil) and (pmRegisters.PopupComponent<>nil) and (pmRegisters.PopupComponent.Tag=ptruint(followRegister));
end;

procedure TMemoryBrowser.pmStacktracePopup(Sender: TObject);
var
  i: integer;
  x: ptruint;
  s: string;
  haserror: boolean=true;
begin
  if processhandler.is64Bit then
  begin
    miAddESP.Caption:='(rsp+*)';
    miAddEBP.Caption:='(rbp+*)';
  end else
  begin
    miAddESP.Caption:='(esp+*)';
    miAddEBP.Caption:='(ebp+*)';
  end;

  if lvStacktraceData.Selected<>nil then
  begin
    s:=lvStacktraceData.Selected.Caption;
    i:=pos('(', s);
    if i>0 then
      s:=copy(s,1,i-1);

    x:=symhandler.getAddressFromName(s,false,haserror);

    if not haserror then
      miAddRef.caption:=format('(ref+*) Ref will be %x',[x]);
  end;
end;

procedure TMemoryBrowser.RegisterViewResize(Sender: TObject);
begin
  if scrollbox1.VertScrollBar.IsScrollBarVisible then
    sbShowFloats.Left:=scrollbox1.VertScrollBar.ClientSizeWithBar-sbShowFloats.Width
  else
    sbShowFloats.Left:=scrollbox1.VertScrollBar.ClientSizeWithoutBar-sbShowFloats.Width;
end;

procedure TMemoryBrowser.miAddRefClick(Sender: TObject);
var
  i: integer;
  s: string;
  haserror: boolean=true;
  x: ptruint;
begin
  if lvStacktraceData.Selected<>nil then
  begin
    s:=lvStacktraceData.Selected.Caption;
    i:=pos('(', s);
    if i>0 then
      s:=copy(s,1,i-1);

    x:=symhandler.getAddressFromName(s,false,haserror);

    if not haserror then
    begin
      StackReference:=x;
      reloadStacktrace;
    end;
  end
  else
  begin
    miAddESP.Checked:=true;
    reloadStacktrace;
  end;
end;

procedure TMemoryBrowser.miBreakOnExceptionsClick(Sender: TObject);
var n: TNotifyEvent;
begin
  miExceptionRegionSeperator.Visible:=UnexpectedExceptionAction in [ueaBreak, ueaBreakIfInRegion];
  miExceptionRegionAutoAddAllocs.Visible:=UnexpectedExceptionAction=ueaBreakIfInRegion;
  miExceptionRegionManageList.Visible:=UnexpectedExceptionAction=ueaBreakIfInRegion;
  miExceptionIgnoreList.visible:=UnexpectedExceptionAction in [ueaBreak, ueaBreakIfInRegion];

  n:=miExceptionRegionAutoAddAllocs.OnClick;
  miExceptionRegionAutoAddAllocs.OnClick:=nil;
  miExceptionRegionAutoAddAllocs.checked:=allocsAddToUnexpectedExceptionList;
  miExceptionRegionAutoAddAllocs.OnClick:=n;

  miUnexpectedExceptionBreakNever.checked:=UnexpectedExceptionAction=ueaIgnore;
  miUnexpectedExceptionBreakAlways.checked:=UnexpectedExceptionAction=ueaBreak;
  miUnexpectedExceptionBreakIfInRegion.checked:=UnexpectedExceptionAction=ueaBreakIfInRegion;


end;

procedure TMemoryBrowser.miChangeProtectionClick(Sender: TObject);
var
  protection: dword;
  oldprotect: dword;

  l: integer;
begin

  case (sender as TMenuItem).Tag of
    0: protection:=PAGE_EXECUTE_READWRITE;
    1: protection:=PAGE_EXECUTE_READ;
    2: protection:=PAGE_READWRITE;
    3: protection:=PAGE_READONLY;
    else
       protection:=PAGE_EXECUTE_READWRITE; //never
  end;

  l:=max(1, hexview.SelectionStop-hexview.SelectionStart+1);

  //OutputDebugString(format('start=%d stop=%x l=%d',[hexview.SelectionStart, hexview.SelectionStop, l]));

  if hexview.HasSelection then
    VirtualProtectEx(processhandle, pointer(hexview.SelectionStart),l,protection, oldprotect)
  else
    VirtualProtectEx(processhandle, pointer(hexview.Address),l,protection, oldprotect)

end;

procedure TMemoryBrowser.miExceptionIgnoreListClick(Sender: TObject);
begin
  if frmExceptionIgnoreList=nil then
    frmExceptionIgnoreList:=tfrmExceptionIgnoreList.Create(self);

  frmExceptionIgnoreList.show;
end;

procedure TMemoryBrowser.miExceptionRegionAutoAddAllocsClick(Sender: TObject);
var n: TNotifyEvent;
begin
  if frmExceptionRegionList<>nil then
  begin
    n:=frmExceptionRegionList.cbAutoAddAllocs.OnChange;
    frmExceptionRegionList.cbAutoAddAllocs.OnChange:=nil;
    frmExceptionRegionList.cbAutoAddAllocs.Checked:=miExceptionRegionAutoAddAllocs.checked;
    frmExceptionRegionList.cbAutoAddAllocs.OnChange:=n;
  end;

  allocsAddToUnexpectedExceptionList:=miExceptionRegionAutoAddAllocs.checked;
end;

procedure TMemoryBrowser.miExceptionRegionManageListClick(Sender: TObject);
begin
  if frmExceptionRegionList=nil then
    frmExceptionRegionList:=tfrmExceptionRegionList.Create(self);

  frmExceptionRegionList.show;
end;

procedure TMemoryBrowser.miHVBackClick(Sender: TObject);
begin
  hexview.back;
end;

procedure TMemoryBrowser.miHVFollowClick(Sender: TObject);
begin
  hexview.follow;
end;

procedure TMemoryBrowser.SetBookmarkClick(Sender: TObject);
var
  id: integer;
begin
  if (sender=nil) or (not (sender is TMenuItem)) then exit;
  id:=TMenuItem(Sender).Tag;

  if (id<0) or (id>9) then exit;

  if disassemblerview.SelectedAddress=bookmarks[id].lastAddress then
  begin
    //delete
    bookmarks[id].addressString:='';
    bookmarks[id].lastAddress:=0;
    bookmarks[id].setMi.caption:=format(rsMBBookmark, [id]);
    bookmarks[id].gotoMi.caption:=bookmarks[id].setMi.caption;
  end
  else
  begin
    //update
    bookmarks[id].addressString:=symhandler.getNameFromAddress(disassemblerview.SelectedAddress);
    bookmarks[id].lastAddress:=disassemblerview.SelectedAddress;
    bookmarks[id].setMi.Caption:=format(rsMBBookmark2, [id, bookmarks[id].addressString]);
    bookmarks[id].gotoMi.Caption:=bookmarks[id].setMi.caption;
  end;
end;

procedure TMemoryBrowser.GotoBookmarkClick(Sender: TObject);
var
  err: boolean;
  id: integer;
  newaddress: ptruint;
  s: string;
begin
  if (sender=nil) or (not (sender is TComponent)) then exit;
  id:=TComponent(Sender).Tag;

  if (id<0) or (id>9) then exit;
  if bookmarks[id].addressString='' then exit;

  s:=bookmarks[id].addressString;
  newaddress:=symhandler.getAddressFromName(s, false, err);
  if err then
    newaddress:=bookmarks[id].lastAddress;

  disassemblerview.SelectedAddress:=newaddress;
end;



procedure TMemoryBrowser.Makepagewritable1Click(Sender: TObject);
begin
end;

procedure TMemoryBrowser.FormActivate(Sender: TObject);
begin
  disassemblerview.LastFormActiveEvent:=getTickCount64;
end;

procedure TMemoryBrowser.Debug1Click(Sender: TObject);
begin

end;

procedure TMemoryBrowser.miTextEncodingClick(Sender: TObject);
begin
  if miTextEncodingAscii.checked then
    hexview.CharEncoding:=ceAscii
  else
  if miTextEncodingCodePage.checked then
    hexview.CharEncoding:=ceCodepage
  else
  if miTextEncodingUTF8.checked then
    hexview.CharEncoding:=ceUtf8
  else
    hexview.CharEncoding:=ceUtf16;
end;




procedure TMemoryBrowser.miCopyBytesOnlyClick(Sender: TObject);
var start, stop: ptruint;
   l,i: integer;
   x: string;
   x2: ptrUint;
   buf: pbytearray;

   result: string;
begin
  //Unrelated to the copy bytes. This indicates the user just wants and AOB
  start:=minX(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2);
  stop:=maxX(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2);

  //adjust stop to go AFTER the instruction
  disassemble(stop, x);

  l:=stop-start;

  if l<65535 then //not going to do more than that
  begin
    getmem(buf, l);
    try
      if readprocessmemory(processhandle, pointer(start), buf, l, x2) then
      begin
        result:='';
        for i:=0 to l-1 do
          result:=result+inttohex(buf[i],2)+' ';

        Clipboard.AsText:=copy(result,1,length(result)-1);
      end;
    finally
      freememandnil(buf);

    end;
  end;
end;

procedure TMemoryBrowser.miDissectData2Click(Sender: TObject);
begin
  if frmStructures2.count>0 then
  begin
   // TfrmStructures2(frmStructures2[0]).edtAddress.Text:=inttohex(memorybrowser.memoryaddress,8);
    TfrmStructures2(frmStructures2[0]).show;
  end
  else
  begin
    //create it
    with tfrmstructures2.create(application) do
    begin
      initialaddress:=memoryaddress;
      show;

      if initialaddress=0 then //address 0 would normally NOT add a new address, but this is a user specified address, so do it anyhow
      addColumn;
    end;
  end;
end;

procedure TMemoryBrowser.miPointerSpiderClick(Sender: TObject);
begin
  if frmStringPointerscan=nil then
    frmStringpointerscan:=Tfrmstringpointerscan.create(application);

  frmStringpointerscan.rbDatascan.Checked:=true;
  frmStringpointerscan.edtBase.text:=inttohex(hexview.address,8);

  frmStringpointerscan.show;
end;

procedure TMemoryBrowser.miSelectCurrentFunctionClick(Sender: TObject);
var start,stop: ptrUint;
  pa, a: ptruint;

  d: TDisassembler;
  s: string;
  infloop: integer;
begin
  //disassemble the code and find the last address
//  parametervaluetype:=dvtaddress;
  d:=TDisassembler.create;
  try
    start:=disassemblerview.SelectedAddress;




    //find stop:
    stop:=start;
    a:=stop;

    infloop:=0;
    while infloop<100000 do
    begin
      inc(infloop);

      pa:=a;
      d.disassemble(a,s);

      if (length(d.LastDisassembleData.Bytes)>=2) and (d.LastDisassembleData.Bytes[0]=0) and (d.LastDisassembleData.Bytes[1]=0) then
      begin
        stop:=pa;
        break;
      end;


      if (d.LastDisassembleData.opcode='??') or (d.LastDisassembleData.opcode='ret') or (d.LastDisassembleData.opcode='int 3') or (d.LastDisassembleData.opcode='nop') then //found the end
      begin
        stop:=pa;
        break;
      end;

      if (d.LastDisassembleData.isjump) and (not d.LastDisassembleData.iscall) then
      begin
        //adjust the current address to point
        if d.LastDisassembleData.parameterValueType=dvtAddress then //direct address
        begin
          if isAddress(d.LastDisassembleData.parameterValue) then
          begin
            if d.LastDisassembleData.parameterValue < start then
            begin
              start:=d.LastDisassembleData.parameterValue;
              continue;
            end;

            if d.LastDisassembleData.parameterValue > pa then
            begin
              stop:=d.LastDisassembleData.parameterValue;
              a:=stop;
              continue;
            end;

            if (d.LastDisassembleData.parameterValue <= a) and (d.LastDisassembleData.opcode='jmp') then //an unconditonal jump that jumps back and no conditonal jump encountered that went after this. Meaning: Infinite loop
            begin
              stop:=pa;
              break;
            end;
          end;


        end;
      end;

    end;


    //find start
    infloop:=0;

    pa:=start;
    d.disassemble(pa,s); //start from next one in case it's started from the begin

    while infloop<100000 do
    begin
      inc(infloop);


      a:=previousopcode(pa);
      pa:=a;
      d.disassemble(a,s);

      //look for 00 , 00  (add [eax],al is an 100% completely useless instruction)
      if (length(d.LastDisassembleData.Bytes)>=2) and (d.LastDisassembleData.Bytes[0]=0) and (d.LastDisassembleData.Bytes[1]=0) then
      begin
        start:=a;
        break;
      end;


      //look for ret, int3 or nop
      if (d.LastDisassembleData.opcode='??') or (d.LastDisassembleData.opcode='ret') or (d.LastDisassembleData.opcode='int 3') then
      begin
        start:=a; //start from next instruction
        break;
      end;

      if processhandler.is64Bit then
      begin
        //looks for sub rsp,xxxx on an address that is dividable by 0x10

        if (pa mod $10 = 0) and (d.LastDisassembleData.opcode='sub') then
        begin
          if pos('rsp,', lowercase(d.LastDisassembleData.parameters))>0 then
          begin
            start:=pa;
            break;
          end;
        end;
      end
      else
      begin
        //look for push ebp
        if (d.LastDisassembleData.opcode='push') then
        begin
          if lowercase(d.LastDisassembleData.parameters)='ebp' then
          begin
            start:=pa;
            break;
          end;
        end;
      end;

    end;

  finally
    d.free;
  end;

  disassemblerview.SelectedAddress:=start;
  disassemblerview.SelectedAddress2:=stop;
end;

procedure TMemoryBrowser.miDataBreakPointMenuClick(Sender: TObject);
begin

end;

procedure TMemoryBrowser.miWatchAccessClick(Sender: TObject);
var
  a,a2: ptruint;
  bpm: TBreakpointMethod;
begin
  try
    if (startdebuggerifneeded(true)) and (hexview.hasSelection) then
    begin
      hexview.GetSelectionRange(a,a2);

      bpm:=bpmDebugRegister;
      if miWatchBPException.checked then
        bpm:=bpmException
      else
      if miWatchBPDBVM.checked then
        bpm:=bpmDBVM;

      DebuggerThread.SetOnAccessBreakpoint(a, 1+(a2-a), bpm);
      hexview.Update;
    end;
  except
    on e: exception do
      messagedlg(e.message, mtError, [mbOK], 0);
  end;

end;

procedure TMemoryBrowser.miWatchWriteClick(Sender: TObject);
var
  a,a2: ptruint;
  bpm: TBreakpointMethod;
begin
  try
    if (startdebuggerifneeded(true)) and (hexview.hasSelection) then
    begin
      hexview.GetSelectionRange(a,a2);

      bpm:=bpmDebugRegister;
      if miWatchBPException.checked then
        bpm:=bpmException
      else
      if miWatchBPDBVM.checked then
        bpm:=bpmDBVM;


      DebuggerThread.SetOnWriteBreakpoint(a, 1+(a2-a),bpm);
      hexview.Update;
    end;
  except
    on e: exception do
      messagedlg(e.message, mtError, [mbOK], 0);
  end;


end;

procedure TMemoryBrowser.miBreakAndTraceClick(Sender: TObject);
var
  bpm: TBreakpointMethod;
begin
  bpm:=bpmDebugRegister;
  if miWatchBPException.checked then
    bpm:=bpmException
  else
  if miWatchBPDBVM.checked then
    bpm:=bpmDBVM;

  TFrmTracer.createWithBreakpointMethodSet(self,true,false,bpm).show;
end;

procedure TMemoryBrowser.MenuItem9Click(Sender: TObject);
var stime: string;
begin
  stime:=inttostr(hexview.fadetimer);
  if InputQuery(rsHowLongShouldAChangeBeShown, rsChangeDisplayFader, stime) then
    hexview.fadeTimer:=strtoint(stime);
end;

procedure TMemoryBrowser.miAddESPClick(Sender: TObject);
begin
  reloadStacktrace;
end;



procedure TMemoryBrowser.miConditionalBreakClick(Sender: TObject);
var
  script: string;
  easy: boolean;
  bp: PBreakpoint;
begin
  bp:=debuggerthread.isBreakpoint(disassemblerview.SelectedAddress);
  if bp<>nil then
  begin
    inc(bp.referencecount);
    try
      with TfrmBreakpointCondition.create(self) do
      begin
        script:=debuggerthread.getbreakpointcondition(bp, easy);

        if script='' then
        begin
          script:=lastBreakpointCondition.script;
          easy:=lastBreakpointCondition.easy;

          if script='' then
            easy:=true;
        end;

        if easy then
          edtEasy.text:=script
        else
          mComplex.text:=script;



        rbEasy.Checked:=easy;
        rbComplex.checked:=not easy;

        if showmodal=mrok then
        begin
          easy:=rbEasy.checked;
          if easy then
            script:=edtEasy.text
          else
            script:=mComplex.text;

          lastBreakpointCondition.script:=script;
          lastBreakpointCondition.easy:=easy;

          debuggerthread.setbreakpointcondition(bp, easy, script);
        end;

        free;
      end;

    finally
      dec(bp.referencecount);
    end;

  end;
end;

procedure TMemoryBrowser.miDeleteBPClick(Sender: TObject);
var bp: PBreakpoint;
  a,a2: ptruint;
begin
  if (debuggerthread<>nil) then
  begin
    hexview.GetSelectionRange(a,a2);

    debuggerthread.lockbplist;
    try
      repeat
        bp:=debuggerthread.isBreakpoint(a,a2);
        if bp<>nil then
          debuggerthread.RemoveBreakpoint(bp);
      until bp=nil;

    finally
      debuggerthread.unlockbplist;
    end;
    hexview.update;
  end;
end;

procedure TMemoryBrowser.miArchChangeClick(Sender: TObject);
begin
  if miArchAutodetect.checked then
    visibleDisassembler.architecture:=darchAutoDetect
  else
  if miArchX86.checked then
    visibleDisassembler.architecture:=darchX86
  else
    visibleDisassembler.architecture:=darchArm;

  disassemblerview.Update;

  if ssCtrl in GetKeyShiftState then
  begin
    case visibleDisassembler.architecture of
      darchX86: processhandler.SystemArchitecture:=archX86;
      darchArm: processhandler.SystemArchitecture:=archArm;
    end;
  end;
end;

procedure TMemoryBrowser.miDisassemblyAutodetectClick(Sender: TObject);
begin
  if miDisassemblyAutodetect.checked then
    visibleDisassembler.is64bitOverride:=false
  else
  if miDisassembly32.checked then
  begin
    visibleDisassembler.is64bitOverride:=true;
    visibleDisassembler.is64bitOverridestate:=false
  end
  else
  begin
    visibleDisassembler.is64bitOverride:=true;
    visibleDisassembler.is64bitOverridestate:=true;
  end;

  defaultDisassembler.is64bitOverride:=visibleDisassembler.is64bitoverride;
  defaultDisassembler.is64bitOverridestate:=visibleDisassembler.is64bitOverridestate;

  disassemblerview.update;

  if ssCtrl in GetKeyShiftState then
  begin
    if miDisassembly32.checked then
      processhandler.is64Bit:=false
    else
      processhandler.is64Bit:=true;
  end;

end;

procedure TMemoryBrowser.miFindWhatAccessesClick(Sender: TObject);
var
  a,a2: ptruint;
  bpm: TBreakpointMethod;
begin
  if (startdebuggerifneeded(true)) and (hexview.hasSelection) then
  begin
    hexview.GetSelectionRange(a,a2);

    bpm:=bpmDebugRegister;
    if miWatchBPException.checked then
      bpm:=bpmException
    else
    if miWatchBPDBVM.checked then
      bpm:=bpmDBVM;


    DebuggerThread.FindWhatAccesses(a,1+(a2-a), bpm);
    hexview.Update;
  end;
end;

procedure TMemoryBrowser.miFindWhatWritesClick(Sender: TObject);
var
  a,a2: ptruint;
  bpm: TBreakpointMethod;
begin
  if (startdebuggerifneeded(true)) and (hexview.hasSelection) then
  begin
    hexview.GetSelectionRange(a,a2);

    bpm:=bpmDebugRegister;
    if miWatchBPException.checked then
      bpm:=bpmException
    else
    if miWatchBPDBVM.checked then
      bpm:=bpmDBVM;


    DebuggerThread.FindWhatWrites(a,1+(a2-a), bpm);
    hexview.Update;
  end;

end;

procedure TMemoryBrowser.miSepClick(Sender: TObject);
begin
  hexview.bytesPerSeperator:=TMenuItem(sender).Tag;
end;

procedure TMemoryBrowser.miShowInHexviewClick(Sender: TObject);
begin
  hexview.address:=PContextElement_register(pmRegisters.PopupComponent.Tag)^.getValue(context);
end;

procedure TMemoryBrowser.miTextPreferencesClick(Sender: TObject);
var
  x: TfrmMemviewPreferences;
  i: TDisassemblerViewColorsState;
  reg: tregistry;
  fd: TFontData;
begin

  with TfrmMemviewPreferences.create(self) do
  begin
    fd:=Graphics.GetFontData(disassemblerview.font.handle);
    fontdialog1.font.Name:=fd.Name;
    fontdialog1.font.height:=fd.height;
    fontdialog1.font.Pitch:=fd.Pitch;
    fontdialog1.font.Style:=fd.Style;
    fontdialog1.font.CharSet:=fd.CharSet;
    fontdialog1.font.Quality:=fd.Quality;
    fontdialog1.font.Orientation:=fd.Orientation;

    btnFont.Caption:=fontdialog1.Font.Name+' '+inttostr(fontdialog1.Font.Size);



    //FontDialog2.Font.Assign(hexview.HexFont);
    fd:=Graphics.GetFontData(hexview.HexFont.handle);
    fontdialog2.font.Name:=fd.Name;
    fontdialog2.font.height:=fd.height;
    fontdialog2.font.Pitch:=fd.Pitch;
    fontdialog2.font.Style:=fd.Style;
    fontdialog2.font.CharSet:=fd.CharSet;
    fontdialog2.font.Quality:=hexview.HexFont.Quality;

    fontdialog2.font.Orientation:=fd.Orientation;
    cbFontQuality.ItemIndex:=integer(hexview.HexFont.Quality);
    btnHexFont.Caption:=fontdialog2.Font.Name+' '+inttostr(fontdialog2.Font.Size);

    fd:=Graphics.GetFontData(scrollbox1.Font.Reference.Handle);
    fontdialog3.font.Name:=fd.Name;
    fontdialog3.font.height:=fd.height;
    fontdialog3.font.Pitch:=fd.Pitch;
    fontdialog3.font.Style:=fd.Style;
    fontdialog3.font.CharSet:=fd.CharSet;
    fontdialog3.font.Quality:=scrollbox1.Font.Quality;
    fontdialog3.font.Orientation:=fd.Orientation;
    btnRegisterViewFont.Caption:=fontdialog3.Font.Name+' '+inttostr(fontdialog3.Font.Size);


    //set the current colors
    colors:=disassemblerview.colors;

    //and now apply those colors
    cbColorGroupChange(cbColorGroup);

    lblCall.font.color:=disassemblerview.jlCallColor;
    lblUnconditionalJump.font.color:=disassemblerview.jlUnconditionalJumpColor;
    lblConditionalJump.font.color:=disassemblerview.jlconditionalJumpColor;
    jlThickness:=disassemblerview.jlThickness;
    jlSpacing:=disassemblerview.jlSpacing;
    spaceAboveLines:=disassemblerview.spaceAboveLines;
    spaceBelowLines:=disassemblerview.spaceBelowLines;
    hexSpaceBetweenLines:=hexview.spaceBetweenLines;
    cbShowStatusBar.checked:=hexview.statusbar.Visible;

    {$ifdef USELAZFREETYPE}
    cbOriginalRenderingSystem.checked:=UseOriginalRenderingSystem;
    {$endif}

    cbCenterDisassemblerWhenOutsideView.checked:=disassemblerview.CenterOnAddressChangeOutsideView;


    lblHexNormal.font.color:=hexview.normalFontColor;
    lblHexNormal.color:=hexview.normalBackgroundColor;
    lblHexStatic.font.color:=hexview.staticFontColor;
    lblHexStatic.color:=hexview.staticBackgroundColor;
    lblHexHighlighted.font.color:=hexview.highlightedFontColor;
    lblHexHighlighted.color:=hexview.highlightedBackgroundColor;
    lblHexEditing.font.color:=hexview.EditingFontColor;
    lblHexEditing.color:=hexview.EditingBackgroundColor;
    lblHexSecondaryEditing.font.color:=hexview.SecondaryEditingFontColor;
    lblHexSecondaryEditing.color:=hexview.SecondaryEditingBackgroundColor;
    lblHexBreakpoint.font.color:=hexview.BreakpointFontColor;
    lblHexBreakpoint.color:=hexview.BreakpointBackgroundColor;
    lblHexDifferent.font.color:=hexview.DifferentFontColor;
    lblHexDifferent.color:=hexview.DifferentBackgroundColor;

    lblHexTopLine.Font.color:=hexview.toplinecolor;
    lblHexSeperator.Font.color:=hexview.seperatorColor;
    lblHexCursor.Font.color:=hexview.cursorcolor;
    lblHexFadeColor.font.color:=hexview.normalFontColor;
    lblHexFadeColor.color:=hexview.fadecolor;

    lblRegHighLightChange.color:=fChangedRegisterColor;
    lblRegHighLightAccess.color:=fAccessedRegisterColor;



    if showmodal=mrok then
    begin
      //set the colors and save to registry
      {$ifdef USELAZFREETYPE}
      UseOriginalRenderingSystem:=cbOriginalRenderingSystem.checked;
      {$endif}
      disassemblerview.Font.assign(fontdialog1.Font);
      disassemblerview.Font.style:=[];
      disassemblerview.colors:=colors;
      disassemblerview.jlCallColor:=lblCall.font.color;
      disassemblerview.jlUnconditionalJumpColor:=lblUnconditionalJump.font.color;
      disassemblerview.jlconditionalJumpColor:=lblConditionalJump.font.color;
      disassemblerview.spaceAboveLines:=spaceAboveLines;
      disassemblerview.spaceBelowLines:=spaceBelowLines;

      disassemblerview.jlThickness:=jlThickness;
      disassemblerview.jlSpacing:=jlSpacing;

      disassemblerview.CenterOnAddressChangeOutsideView:=cbCenterDisassemblerWhenOutsideView.checked;



      hexview.HexFont:=fontdialog2.Font;

      hexview.normalFontColor:=lblHexNormal.font.color;
      hexview.normalBackgroundColor:=lblHexNormal.color;
      hexview.staticFontColor:=lblHexStatic.font.color;
      hexview.staticBackgroundColor:=lblHexStatic.color;
      hexview.highlightedFontColor:=lblHexHighlighted.font.color;
      hexview.highlightedBackgroundColor:=lblHexHighlighted.color;
      hexview.EditingFontColor:= lblHexEditing.font.color;
      hexview.EditingBackgroundColor:=lblHexEditing.color;
      hexview.SecondaryEditingFontColor:= lblHexSecondaryEditing.font.color;
      hexview.SecondaryEditingBackgroundColor:=lblHexSecondaryEditing.color;
      hexview.BreakpointFontColor:= lblHexBreakpoint.font.color;
      hexview.BreakpointBackgroundColor:=lblHexBreakpoint.color;
      hexview.DifferentFontColor:=lblHexDifferent.font.color;
      hexview.DifferentBackgroundColor:=lblHexDifferent.color;

      hexview.toplinecolor:=lblHexTopLine.Font.color;
      hexview.seperatorColor:=lblHexSeperator.Font.color;
      hexview.cursorcolor:=lblHexCursor.Font.color;
      hexview.fadecolor:=lblHexFadeColor.color;

      fChangedRegisterColor:=lblRegHighLightChange.color;
      fAccessedRegisterColor:=lblRegHighLightAccess.color;


      hexview.spaceBetweenLines:=hexSpaceBetweenLines;
      hexview.statusbar.Visible:=cbShowStatusBar.checked;
      hexview.OnResize(hexview);

      scrollbox1.Font:=fontdialog3.font;
      setRegisterPanelFont(fontdialog3.font);



      free;
    end
    else
    begin
      free;
      exit;
    end;
  end;

  disassemblerview.reinitialize;

  //save to the registry
  {$ifdef USELAZFREETYPE}
  cereg.writeBool('OriginalRenderingSystem', UseOriginalRenderingSystem);
  {$endif}

  reg:=Tregistry.Create;
  try

    if reg.OpenKey('\Software\'+strCheatEngine+'\Disassemblerview '+inttostr(screen.PixelsPerInch)+darkmodestring+'\',true) then
    begin
      reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('colors', {$ifndef windows}bintohexs({$endif}disassemblerview.colors, sizeof(disassemblerview.colors)){$ifndef windows}){$endif};

      reg.WriteInteger('AccessedRegisterColor', integer(fAccessedRegisterColor));
      reg.WriteInteger('ChangedRegisterColor', integer(fChangedRegisterColor));

      reg.WriteInteger('jlCallColor', integer(disassemblerview.jlCallColor));
      reg.WriteInteger('jlUnconditionalJumpColor', integer(disassemblerview.jlUnconditionalJumpColor));
      reg.WriteInteger('jlConditionalJumpColor', integer(disassemblerview.jlconditionalJumpColor));

      reg.writeInteger('spaceAboveLines', disassemblerview.SpaceAboveLines);
      reg.writeInteger('spaceBelowLines', disassemblerview.SpaceBelowLines);
      reg.writeInteger('jlThickness', disassemblerview.jlThickness);
      reg.writeInteger('jlSpacing', disassemblerview.jlSpacing);


      reg.WriteBool('CenterOnAddressChangeOutsideView', disassemblerview.CenterOnAddressChangeOutsideView);
    end;

    if reg.OpenKey('\Software\'+strCheatEngine+'\Disassemblerview '+inttostr(screen.PixelsPerInch)+'\Font'+darkmodestring,true) then
      SaveFontToRegistry(disassemblerview.font, reg);

    if reg.OpenKey('\Software\'+strCheatEngine+'\Hexview '+inttostr(screen.PixelsPerInch),true) then
    begin
      reg.writeInteger('spaceBetweenLines', hexview.spaceBetweenLines);
      reg.WriteBool('showStatusBar', hexview.statusbar.Visible);
    end;

    if reg.openkey('\Software\'+strCheatEngine+'\Hexview'+darkmodestring,true) then
    begin
      reg.{$ifdef windows}WriteBinaryData{$else}WriteString{$endif}('colors', {$ifndef windows}bintohexs({$endif}hexview.colors, sizeof(hexview.colors)){$ifndef windows}){$endif};
      reg.WriteInteger('SeperaterColor', hexview.fseperatorColor);
      reg.WriteInteger('CursorColor', hexview.fcursorcolor);
      reg.WriteInteger('TopLineColor', hexview.ftoplinecolor);
      reg.WriteInteger('FadeColor', hexview.ffadeColor);
    end;

    if reg.OpenKey('\Software\'+strCheatEngine+'\Hexview '+inttostr(screen.PixelsPerInch)+'\Font'+darkmodestring,true) then
      SaveFontToRegistry(hexview.hexfont, reg);

    if reg.OpenKey('\Software\'+strCheatEngine+'\RegisterView '+inttostr(screen.PixelsPerInch)+'\Font'+darkmodestring,true) then
      SaveFontToRegistry(scrollbox1.Font, reg);

  finally
    reg.free;
  end;
end;

procedure TMemoryBrowser.FormShow(Sender: TObject);
var
  w: integer;
  addressSize: integer;
  byteSize: integer;
  opcodeSize: integer;

  lineheight: integer;
begin
  registerpanelfont.assign(scrollbox1.Font);


  if posloadedfromreg=false then
  begin
    autosize:=false;
    w:=0;
    addressSize:=canvas.TextWidth('XXXXXXXXXXXXXXXX  ');  //address
    byteSize:=canvas.TextWidth('XX XX XX XX XX XX XX XX '); ///bytes
    opcodeSize:=canvas.TextWidth('XXXX     XXX,[XXXXXXXXXXXXXXXX+XXX*X]          ');
    disassemblerview.setheaderWidth(0,addressSize);
    disassemblerview.setheaderWidth(1,byteSize);
    disassemblerview.setheaderWidth(2,opcodeSize);

    lineheight:=canvas.TextHeight('jxZyYqQ');


    width:=max(mainform.width, addressSize+byteSize+opcodeSize+2*addressSize);
    height:=ScaleY(600,96);

    panel1.height:= clientheight div 2;

    disassemblerview.setheaderWidth(3,max(32,disassemblerview.width-(disassemblerview.getheaderWidth(0)+disassemblerview.getheaderWidth(1)+disassemblerview.getheaderWidth(2))-32));

    position:=poDesigned;
    position:=poScreenCenter;

    posloadedfromreg:=true;
  end;

  if disassemblerview<>nil then
    disassemblerview.Update;


  Sericedescriptortable1.visible:=not Is64bitOS;
  GDTlist1.Visible:=not is64bitos;
  IDTlist1.Visible:=not is64bitos;

  scrollbox1.Font.Height:=GetFontData(font.handle).height;
  if scrollbox1.Font.Height>-13 then
    scrollbox1.Font.Height:=-13;

  if hexview<>nil then
    hexview.statusbar.Height:=Canvas.TextHeight('BLAy9qrSTt')+3+hexview.statusbar.BorderWidth;

  if adjustedsize=false then
  begin
    tbDebug.ButtonHeight:=scaley(tbDebug.ButtonHeight, DesignTimePPI);

    //tbDebug.ButtonHeight:=trunc(tbDebug.ButtonHeight*fontmultiplication);
    ////dpihelper.AdjustToolbar(tbDebug);

    adjustedsize:=true;
  end;

  {$ifdef darwin}
  //need to adjust the formsize to trigger a repaint
  BoundsRect:=BoundsRect;
  {$endif}

  HasBeenShown:=true;
end;

procedure TMemoryBrowser.disassemblerviewDblClick(Sender: TObject);
var m: TPoint;
  a: ptruint;
  lni: PLineNumberInfo;
  f: TfrmSourceDisplay;
begin
  //find what column is clicked

  m:=disassemblerview.ScreenToClient(mouse.cursorpos);

  a:=disassemblerview.getReferencedByLineAtPos(m);
  if a<>0 then
  begin
    disassemblerview.SelectedAddress:=a;
    exit;
  end;

  lni:=disassemblerview.getSourceCodeAtPos(m);
  if lni<>nil then
  begin
    f:=getSourceViewForm(lni);
    if f<>nil then
      f.show();
    exit;
  end;

  if m.x>(disassemblerview.getheaderWidth(0)+disassemblerview.getheaderWidth(1)+disassemblerview.getheaderWidth(2)) then
  begin
    miUserdefinedComment.click; //comment click
    exit;
  end;

  assemble1.Click;
end;

procedure TMemoryBrowser.FormCreate(Sender: TObject);
var x: array of integer;
  reg: tregistry;
  f: TFont;
  i: integer;
  c: tcolor;
begin


  registerpanelfont:=tfont.Create;

  MemoryBrowsers.Add(self);

  bookmarks[0].setMi:=miSetBookmark0;
  bookmarks[1].setMi:=miSetBookmark1;
  bookmarks[2].setMi:=miSetBookmark2;
  bookmarks[3].setMi:=miSetBookmark3;
  bookmarks[4].setMi:=miSetBookmark4;
  bookmarks[5].setMi:=miSetBookmark5;
  bookmarks[6].setMi:=miSetBookmark6;
  bookmarks[7].setMi:=miSetBookmark7;
  bookmarks[8].setMi:=miSetBookmark8;
  bookmarks[9].setMi:=miSetBookmark9;

  bookmarks[0].gotoMi:=miGotoBookmark0;
  bookmarks[1].gotoMi:=miGotoBookmark1;
  bookmarks[2].gotoMi:=miGotoBookmark2;
  bookmarks[3].gotoMi:=miGotoBookmark3;
  bookmarks[4].gotoMi:=miGotoBookmark4;
  bookmarks[5].gotoMi:=miGotoBookmark5;
  bookmarks[6].gotoMi:=miGotoBookmark6;
  bookmarks[7].gotoMi:=miGotoBookmark7;
  bookmarks[8].gotoMi:=miGotoBookmark8;
  bookmarks[9].gotoMi:=miGotoBookmark9;


  scrollbox1.Font:=mainform.font;
  scrollbox1.Font.name:='Courier New';
  scrollbox1.Font.Size:=13;


  displaytype:=dtByte;

  strace:=tstringlist.create;

  disassembler:=true;

  disassemblerview:=TDisassemblerview.Create(self);
  disassemblerview.font:=mainform.font;
  disassemblerview.Align:=alClient;
  disassemblerview.Parent:=panel5;
  disassemblerview.PopupMenu:=debuggerpopup;
  disassemblerview.OnKeyDown:=FControl1keydown;
  disassemblerview.OnDblClick:=disassemblerviewDblClick;
  disassemblerview.TopAddress:=$00400000;
  disassemblerview.name:='DisassemblerView';

  if GetFontData(disassemblerview.font.handle).height>-13 then
    disassemblerview.font.height:=-13;

  hexview:=THexview.create(self);
  hexview.Align:=alClient;
  hexview.parent:=panel3;
  hexview.popupmenu:=memorypopup;

  hexview.OnKeyDown:=hexviewKeyDown;
  hexview.Name:='HexadecimalView';



  //load from the registry
  {$ifdef USELAZFREETYPE};
  UseOriginalRenderingSystem:=cereg.readBool('OriginalRenderingSystem');
  {$endif}

  f:=TFont.create;
  reg:=Tregistry.Create;
  try
    if reg.OpenKey('\Software\'+strCheatEngine+'\Disassemblerview '+inttostr(screen.PixelsPerInch)+'\Font'+darkmodestring,false) then
    begin
      LoadFontFromRegistry(f, reg);
      disassemblerview.font:=f;
    end;

    if reg.OpenKey('\Software\'+strCheatEngine+'\Disassemblerview '+inttostr(screen.PixelsPerInch)+darkmodestring+'\',false) then
    begin
      if reg.ValueExists('colors') then
      begin
        {$ifdef windows}
        reg.ReadBinaryData('colors', disassemblerview.colors, sizeof(disassemblerview.colors));
        {$else}
        HexToBin(pchar(reg.ReadString('colors')),pchar(@disassemblerview.colors),sizeof(disassemblerview.colors));
        {$endif}
      end;


      if reg.ValueExists('AccessedRegisterColor') then
        fAccessedRegisterColor:=tcolor(reg.ReadInteger('AccessedRegisterColor'));

      if reg.ValueExists('ChangedRegisterColor') then
        fChangedRegisterColor:=tcolor(reg.ReadInteger('ChangedRegisterColor'));

      if reg.ValueExists('jlCallColor') then
        disassemblerview.jlCallColor:=tcolor(reg.ReadInteger('jlCallColor'));

      if reg.ValueExists('jlUnconditionalJumpColor') then
        disassemblerview.jlUnconditionalJumpColor:=tcolor(reg.ReadInteger('jlUnconditionalJumpColor'));

      if reg.ValueExists('jlConditionalJumpColor') then
        disassemblerview.jlConditionalJumpColor:=tcolor(reg.ReadInteger('jlConditionalJumpColor'));

      if reg.ValueExists('spaceAboveLines') then
        disassemblerview.spaceAboveLines:=reg.ReadInteger('spaceAboveLines');

      if reg.ValueExists('spaceBelowLines') then
        disassemblerview.spaceBelowLines:=reg.ReadInteger('spaceBelowLines');

      if reg.ValueExists('jlThickness') then
        disassemblerview.jlThickness:=reg.ReadInteger('jlThickness');

      if reg.ValueExists('jlSpacing') then
        disassemblerview.jlSpacing:=reg.ReadInteger('jlSpacing');

      if reg.ValueExists('CenterOnAddressChangeOutsideView') then
        disassemblerview.CenterOnAddressChangeOutsideView:=reg.ReadBool('CenterOnAddressChangeOutsideView');

      disassemblerview.reinitialize;
    end;

    if reg.OpenKey('\Software\'+strCheatEngine+'\Hexview '+inttostr(screen.PixelsPerInch),false) then
    begin
      if reg.ValueExists('spaceBetweenLines') then
        hexview.spaceBetweenLines:=reg.ReadInteger('spaceBetweenLines');

      if reg.ValueExists('showStatusBar') then
        hexview.statusbar.Visible:=reg.ReadBool('showStatusBar');

    end;

    if reg.openkey('\Software\'+strCheatEngine+'\Hexview'+darkmodestring,true) then
    begin
      if reg.ValueExists('colors') then
      begin
        {$ifdef windows}
        reg.ReadBinaryData('colors', hexview.colors, sizeof(hexview.colors));
        {$else}
        HexToBin(pchar(reg.ReadString('colors')),pchar(@hexview.colors),sizeof(hexview.colors));
        {$endif}
      end;

      if reg.ValueExists('SeperaterColor') then  hexview.fseperatorColor:=reg.ReadInteger('SeperaterColor');
      if reg.ValueExists('CursorColor') then  hexview.fcursorcolor:=reg.ReadInteger('CursorColor');
      if reg.ValueExists('TopLineColor') then  hexview.ftoplinecolor:=reg.ReadInteger('TopLineColor');
      if reg.ValueExists('FadeColor') then  hexview.ffadeColor:=reg.ReadInteger('FadeColor');
    end;



    if reg.OpenKey('\Software\'+strCheatEngine+'\Hexview '+inttostr(screen.PixelsPerInch)+'\Font'+darkmodestring,false) then
    begin
      LoadFontFromRegistry(f, reg);
      hexview.hexfont:=f;
    end;

    if reg.OpenKey('\Software\'+strCheatEngine+'\RegisterView '+inttostr(screen.PixelsPerInch)+'\Font'+darkmodestring,false) then
    begin
      LoadFontFromRegistry(f, reg);
//      scrollbox1.Font:=f;

      setRegisterPanelFont(f);
    end
    else
    begin
      f.assign(scrollbox1.font);
      f.size:=12;
      setRegisterPanelFont(f);
    end;


  finally
    f.free;
    reg.free;
  end;








  memoryaddress:=$00400000;
  memorylabelcount:=0;

  Highlightcolor:=clHighlight;

  disassemblerHistory:=TStringList.create;
  memorybrowserHistory:=TStringList.create;
  assemblerHistory:=TStringList.create;

  showvalues:=true;
  sbShowFloats.left:=scrollbox1.Clientwidth-sbShowFloats.width;

  FStacktraceSize:=4096;
  


  setlength(x, 0);

  if loadformposition(self,x) then
  begin
    disassemblerview.setheaderWidth(0,x[0]);
    disassemblerview.setheaderWidth(1,x[1]);
    disassemblerview.setheaderWidth(2,x[2]);
    disassemblerview.setheaderWidth(3,x[3]);

    if length(x)>=6 then
    begin
      panel1.height:=x[4];
      registerview.width:=x[5];
    end;

    if length(x)>=8 then
    begin
      miShowSymbols.checked:=x[6]=1;
      miShowModuleAddresses.checked:=(x[7] and 1)=1;
      miShowSectionAddresses.checked:=(x[7] and 2)=2;


      symhandler.showsymbols:=miShowSymbols.Checked;
      symhandler.showmodules:=miShowModuleAddresses.Checked;
      symhandler.showsections:=miShowSectionAddresses.checked;
    end;

    if length(x)>=10 then
    begin
      if x[9]<>0 then
        hexview.LockedRowSize:=x[9];
    end;

    if length(x)>=11 then
    begin
      miKernelmodeSymbols.checked:=x[10]=1;
      symhandler.kernelsymbols:=miKernelmodeSymbols.Checked;
    end;

    if length(x)>=12 then
      pnlStacktrace.Width:=x[11];

    setlength(x,0);
    posloadedfromreg:=true;
  end;


  scrollbox1.OnVScroll:=Scrollboxscroll;

  disassemblerview.reinitialize;
  followRegister:=nil;

  {$ifdef darwin}
  InjectDLL1.Caption:=rsInjectDYLIB;
  miChangeProtectionRWE.enabled:=false; //impossible to set , execute can not go together with write
  miChangeProtectionRWE.visible:=false;
  {$endif}

  if ShouldAppsUseDarkMode() then
  begin
    fAccessedRegisterColor:=clBlue;
    fChangedRegisterColor:=clred;
  end
  else
  begin
    fAccessedRegisterColor:=clAqua;
    fChangedRegisterColor:=clred;
  end;

  lblRegisters.font.color:=clWindowtext;
  sRegisters.pen.color:=clWindowtext;

  lblFlags.font.color:=clWindowtext;
  sFlags.pen.color:=clWindowtext;

  lblSpecial.font.color:=clWindowtext;
  sSpecial.Pen.color:=clWindowtext;


end;

procedure TMemoryBrowser.Scrollboxscroll(sender: TObject);
begin

end;

procedure TMemoryBrowser.Goto1Click(Sender: TObject);
var newaddress: string;
    canceled: boolean;
    old: ptruint;
begin
  old:=hexview.SelectionStart;
  if old=0 then old:=memoryaddress;

  newaddress:=inputboxtop(rsGotoAddress, rsFillInTheAddressYouWantToGoTo, IntTohex(old, 8), true, canceled, memorybrowserHistory);

  if(canceled)then
    exit;

  try
    hexview.address:=getaddress(newaddress);
  except
    try
      hexview.address:=symhandler.getAddressFromName(newaddress);
    except
      on e: exception do
      begin
        MessageDlg(e.message, mtError,[mbok],0);
        exit;
      end
    end;
  end;

  if old<>hexview.address then
    hexview.history.Push(pointer(old));

  hexview.SetFocus;
end;

procedure TMemoryBrowser.FormResize(Sender: TObject);
begin

  if disassemblerview<>nil then
    disassemblerview.Update;

end;

procedure TMemoryBrowser.Splitter2Moved(Sender: TObject);
begin
  //caption:=inttostr(registerview.width);
end;

procedure TMemoryBrowser.Timer2Timer(Sender: TObject);
var
  rollover: integer;
  timetaken: qword;
begin
  if Visible then
  begin
    try

      timetaken:=GetTickCount64;

      if hexview<>nil then hexview.update;
      if disassemblerview<>nil then disassemblerview.Update;

      //refresh the modulelist
      if processhandler.isNetwork then
        rollover:=250
      else
        rollover:=50;

      lastmodulelistupdate:=(lastmodulelistupdate+1) mod rollover;
      if lastmodulelistupdate=0 then
        if symhandler<>nil then symhandler.loadmodulelist;

      timetaken:=GetTickCount64-timetaken;
      if (timetaken>timer2.interval) and (timer2.interval<5000) then
      begin
        timer2.enabled:=false;
        timer2.interval:=timer2.interval+100; //this system can't handle the current speed
        timer2.enabled:=true;
      end;

      if (timetaken<timer2.interval) and (timer2.Interval>200) then
        timer2.interval:=max(250,timer2.interval-100);

    except
      on e:exception do
      begin
        timer2.enabled:=false;
        MessageDlg('Error in memview update timer:'+e.message, mtError,[mbok],0);
      end;
    end;
  end;
end;

procedure TMemoryBrowser.miReplacewithnopsClick(Sender: TObject);
var codelength: dword;
    written: dword;
    bla:string;
    i,j: integer;
    nops: array of byte;
    a,a2: ptrUint;
    original: dword;

    mbi : _MEMORY_BASIC_INFORMATION;
  //set the protectionlabel
  e: boolean;
begin
  a:=disassemblerview.SelectedAddress;

  for i:=0 to AdvancedOptions.count-1 do
  begin
    if AdvancedOptions.code[i]<>nil then
    begin
      a2:=symhandler.getAddressFromName(AdvancedOptions.code[i].symbolname,false,e);
      if not e then
      begin
        if InRangeX(disassemblerview.SelectedAddress, a2, a2+length(AdvancedOptions.code[i].actualopcode)-1 ) then
        begin
          for j:=0 to AdvancedOptions.count-1 do
            AdvancedOptions.Selected[j]:=false;

          AdvancedOptions.Selected[i]:=true;
          AdvancedOptions.lvCodelist.ItemIndex:=i;

          if AdvancedOptions.code[i].changed then
            AdvancedOptions.miRestoreWithOriginal.OnClick(AdvancedOptions.miRestoreWithOriginal)
          else
            AdvancedOptions.miReplaceWithNops.onclick(AdvancedOptions.miReplaceWithNops);

          exit;
        end;
      end;

    end;
  end;

  //still here so add it to the codelist

  disassemble(a,bla);
  codelength:=a-disassemblerview.SelectedAddress;

  if advancedoptions.AddToCodeList(disassemblerview.SelectedAddress,codelength,true) then
  begin
    setlength(nops,codelength);
    for i:=0 to codelength-1 do
      nops[i]:=$90;  // $90=nop

    zeromemory(@mbi,sizeof(mbi));
    Virtualqueryex(processhandle,pointer(disassemblerview.SelectedAddress),mbi,sizeof(mbi));

   // get old security and set new security   (not needed in win9x but nt doesnt even allow writeprocessmemory to do this
    original:=0;

    RewriteCode(processhandle,disassemblerview.SelectedAddress,@nops[0],codelength);
    hexview.update;
    disassemblerview.Update;;
  end;
end;

procedure TMemoryBrowser.hexviewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var a: qword;
  psize: integer;
  gotoaddress: qword;
  x: ptruint;
begin
  if shift=[ssCtrl] then
  begin
    case key of
      vk_space: //ctrl+space
      begin
        if hexview.HasSelection then
        begin
          psize:=1+hexview.SelectionStop-hexview.selectionstart;
          if psize>=4 then
          begin
            key:=0;
            if psize>8 then psize:=8;

            if ReadProcessMemory(processhandle, pointer(hexview.SelectionStart), @gotoaddress, processhandler.pointersize,x) then
              disassemblerview.SelectedAddress:=gotoaddress;

            exit;
          end;
        end;

        disassemblerview.SelectedAddress:=memoryaddress;
        key:=0;
      end;

      ord('G'):
      begin
        goto1.click;    //ctrl+g
        key:=0;
      end;

      ord('C'):
      begin
        Cut1.OnClick(Cut1); //ctrl+c
        key:=0;
      end;

      ord('V'):
      begin
        Pastefromclipboard1.OnClick(Pastefromclipboard1); //ctrl+v
        key:=0;
      end;

      ord('F'):
      begin
        search1.OnClick(self);
      end;
    end;
  end
  else
  if shift=[] then
  begin
    if key=vk_f3 then
    begin
      if findwindow<>nil then //easy way to find out if there was a scan before
      begin
        findwindow.editStart.Text:=inttohex(memoryaddress+1,8);
        findwindow.firstscan:=false;
        findwindow.btnOK.Click;
      end else
      begin
        search1.OnClick(self);
      end;
    end;

  end;
end;

//key control for the disassembler
procedure TMemoryBrowser.FControl1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var rct: trect;
    ignore: string;
    i: integer;

    a,b: ptrUint;
begin
  //if shift is not pressed and it's a up,down,left or right, then disable multiline section
  case key of
    vk_delete:
      miReplacewithnops.Click;


    vk_return:
    begin
      //assembler input
      if shift = [] then
        assemble1.Click;
    end;

    vk_space:
    begin
      key:=0;
      if shift=[] then
        follow1.Click
      else
      if shift = [ssShift] then
        back1.click
      else
      if shift = [ssCtrl] then
        memoryaddress:=disassemblerview.SelectedAddress;
    end;

    VK_BACK:
    begin
      key:=0;
      back1.click; //backspace and shift+space
    end;

    ORD('A')..ORD('Z') , ORD('0')..ORD('9'):
    begin
      if key=ORD('C') then
      begin
        if shift = [ssCtrl] then //ctrl C
        begin

          //open the copy window asking what exactly to copy
          with tfrmSavedisassembly.create(self) do
          begin
            a:=minX(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2);
            b:=maxX(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2);

            
            disassemble(b); //b gets increased with size of selected instruction
            edit1.Text:=inttohex(a,8);
            edit2.Text:=inttohex(b-1,8);
            copymode:=true;
            showmodal;
            free;
            exit;
          end;

        end;
      end;

      if key=ORD('G') then
      begin
        if ssCtrl in shift then
        begin
          gotoaddress1.Click;
          disassemblerview.Update;
          exit;
        end;
      end;

      if (key=ORD('Z')) and (ssCtrl in shift) then
        undoLastWrite;

      if (ssalt in shift) or (ssctrl in shift) then exit; 

      try
        assemblepopup(lowercase(chr(key)));
      except
        on e:exception do
        begin
          MessageDlg(e.message, mtError,[mbok],0);
          exit;
        end;

      end;
    end;
  end;

  disassemblerview.Update;

  //key:=0;
end;

procedure TMemoryBrowser.FControl1KeyPress(Sender: TObject; var Key: Char);
begin
  //key:=chr(0);
end;

procedure TMemoryBrowser.Gotoaddress1Click(Sender: TObject);
var newaddress: string;
    oldoptions: dword;
    canceled: boolean;
    oldAddress: ptrUint;
begin
  newaddress:=InputBoxTop(rsGotoAddress, rsFillInTheAddressYouWantToGoTo, IntTohex(disassemblerview.SelectedAddress, 8), true, canceled, memorybrowserHistory);

  if(canceled)then
    exit;

  try
    try
      disassemblerview.SelectedAddress:=symhandler.getaddressfromname(newaddress);
    except
      disassemblerview.SelectedAddress:=getaddress(newaddress);
    end;
  except
    on e:exception do
      MessageDlg(e.Message,mtError,[mbok],0);
  end;
end;

procedure TMemoryBrowser.Search1Click(Sender: TObject);
begin

  if findwindow=nil then findwindow:=TFindwindow.create(self);
  findwindow.firstscan:=true;
  findwindow.editStart.text:=inttohex(hexview.SelectionStart,8);

  findwindow.ShowModal;
end;

procedure TMemoryBrowser.Change1Click(Sender: TObject);
begin
  hexview.changeSelected;
end;

procedure TMemoryBrowser.Addthisaddresstothelist1Click(Sender: TObject);
var i: integer;
    ad: ptrUint;
begin
//this will add the selected recdord to the list
  hexview.AddSelectedAddressToCheatTable;


end;

procedure TMemoryBrowser.miAddToTheCodelistClick(Sender: TObject);
var {start,stop: string;
    a,b: dword;
    i: integer;}

    desc: string;

begin

  frmAddToCodeList:=TfrmAddToCodeList.create(self);
  frmAddToCodeList.addtocodelist:=true;
  frmAddToCodeList.fromaddress:=disassemblerview.SelectedAddress;
  frmAddToCodeList.toaddress:=disassemblerview.SelectedAddress;
  disassemble(frmAddToCodeList.toaddress,desc);
  dec(frmAddToCodeList.toaddress);
  frmAddToCodeList.showmodal;
end;

procedure TMemoryBrowser.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin

  if newsize<80 then
  begin
    newsize:=80;
    accept:=false;
  end;

  if newsize>memorybrowser.Height-80 then
  begin
    newsize:=memorybrowser.Height-80;
    accept:=false;
  end;
  accept:=true;


end;

procedure TMemoryBrowser.ScrollBar2Scroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin

end;

procedure TMemoryBrowser.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ischild then
  begin
    //do stuff particulary for the children
    action:=cafree;
  end
  else
  begin
    //do stuff for the main debugger
    if (debuggerthread<>nil) and (miDebugRun.enabled) then miDebugRun.click; //run if it was paused


    if frmFloatingPointPanel<>nil then
      frmFloatingPointPanel.Visible:=false;

    if WindowState=wsMinimized then //for an unknown reason, the memoryview window can't be shown again if it was hidden minized
      WindowState:=wsNormal;
  end;
end;

procedure TMemoryBrowser.miDebugRunClick(Sender: TObject);
begin
  if miDebugRun.Enabled then
  begin
    if debuggerthread<>nil then
      debuggerthread.ContinueDebugging(co_run);

    OnMemoryViewerRunning;
    caption:=rsMemoryViewerRunning;

    reloadstacktrace;
  end;
end;

procedure TMemoryBrowser.miDebugStepClick(Sender: TObject);
begin
  begin
    if debuggerthread<>nil then
      debuggerthread.ContinueDebugging(co_stepinto);

    OnMemoryViewerRunning;
    caption:=rsMemoryViewerRunning;

    reloadstacktrace;
  end;
end;

procedure TMemoryBrowser.miDebugStepOverClick(Sender: TObject);
var x: ptrUint;
    i,j: integer;
    s,s1,s2,temp:string;
    int3: byte;
    original,a,written:dword;
begin
  debuggerthread.continueDebugging(co_stepover);
  reloadstacktrace;
  OnMemoryViewerRunning;
  caption:=rsMemoryViewerRunning;
end;

procedure TMemoryBrowser.miDebugRunTillClick(Sender: TObject);
var x: ptrUint;
    i: integer;
    temp:string;
    int3: byte;
    original,a,written:dword;
begin
  begin
    if debuggerthread<>nil then
      debuggerthread.ContinueDebugging(co_runtill, disassemblerview.SelectedAddress);

    OnMemoryViewerRunning;
    caption:=rsMemoryViewerRunning;
  end;

end;

procedure TMemoryBrowser.Stacktrace1Click(Sender: TObject);
begin
  if frmstacktrace=nil then
    frmstacktrace:=tfrmstacktrace.create(application);

  frmstacktrace.Show;
end;

procedure TMemoryBrowser.Threadlist1Click(Sender: TObject);
begin

  if frmThreadlist=nil then
    frmThreadlist:=tfrmthreadlist.create(self);

  frmThreadlist.show;
end;

procedure TMemoryBrowser.AssemblePopup(x:string);
var assemblercode,desc,s: string;
    totalbytes: TAssemblerBytes;
    bytes: tassemblerbytes;
    a,b,original,written:ptrUint;
    originalsize:ptrUint;
    replace: boolean;
    c: word;

    res: word;
    i: integer;
    canceled: boolean;

    localdisassembler: TDisassembler;
    bytelength: dword;

    p: dword;

    gnascript: tstringlist;
    vpe: boolean;

    address: ptruint;

    oldbp: PBreakpoint=nil;

    {$ifdef ONEBYTEJUMPS}
    isjmp1: boolean;
    jmp1target: ptruint;
    {$endif}
begin

  //make sure it doesnt have a breakpoint
  address:=disassemblerview.SelectedAddress;
  try

    if debuggerthread<>nil then
    begin
      debuggerthread.lockbplist;
      oldbp:=debuggerthread.isBreakpoint(Address);

      if (oldbp<>nil) and (oldbp^.breakpointMethod<>bpmInt3) then
        oldbp:=nil;

      if oldbp=nil then
        debuggerthread.unlockbplist; //no need to keep this lock
    end;


    originalsize:=Address;

    localdisassembler:=TDisassembler.Create;
    try
      localdisassembler.disassemble(originalsize,desc);
      assemblercode:=localdisassembler.LastDisassembleData.prefix+localdisassembler.LastDisassembleData.opcode+' '+localdisassembler.LastDisassembleData.parameters;
    finally
      localdisassembler.free;
    end;

    dec(originalsize,Address);


    if x<>'' then assemblercode:=x;

  //  copy

    assemblercode:=InputboxTop(rsCheatEngineSingleLingeAssembler, Format(rsTypeYourAssemblerCodeHereAddress, [inttohex(Address, 8)]), assemblercode, x='', canceled{$ifndef darwin},assemblerHistory{$endif});
    if not canceled then
    begin

      if defaultBinutils<>nil then
      begin
        //use the gnuassembler for this
        gnascript:=TStringList.create;
        try
          gnascript.add('.msection sline 0x'+inttohex(Address,8));
          gnascript.Add(assemblercode);
          gnuassemble(gnascript);
        finally
          gnascript.free;
        end;

        exit;
      end;


      {$ifdef ONEBYTEJUMPS}
      s:=lowercase(trim(assemblercode));
      if s.StartsWith('jmp1 ') then
      begin
        isjmp1:=true;
        s:=copy(s,6);

        jmp1target:=symhandler.getAddressFromName(s);

        //still here, so the address is valid
        assemblercode:='db cc';
      end
      else
        isjmp1:=false;
      {$endif}


      if Assemble(assemblercode,Address,bytes) then
      begin
        if originalsize<>length(bytes) then
        begin
          if formsettings.replacewithnops.checked then
          begin
            if formsettings.askforreplacewithnops.checked then
            begin
              c:=messagedlg(Format(rsTheGeneratedCodeIsByteSLongButTheSelectedOpcodeIsB, [IntToStr(length(bytes)), IntToStr(originalsize)]), mtConfirmation, mbYesNoCancel, 0);
              replace:=c=mryes;
              if c=mrCancel then exit;
            end else replace:=true;

            if replace then
            begin
              while originalsize>length(bytes) do
              begin
                setlength(bytes,length(bytes)+1);
                bytes[length(bytes)-1]:=$90;
              end;

              a:=Address+length(bytes);

              b:=Address;
              while b<a do disassemble(b,desc);

              a:=b-Address;
              while length(bytes)<a do
              begin
                setlength(bytes,length(bytes)+1);
                bytes[length(bytes)-1]:=$90;
              end;
            end;


          end;
        end;

        {$ifdef ONEBYTEJUMPS}
        if isjmp1 then
        begin
          InitializeAutoAssemblerExceptionHandler;
          AutoAssemblerExceptionHandlerAddChangeRIPEntry(address, jmp1target);

          AutoAssemblerExceptionHandlerApplyChanges;
        end;
        {$endif}
        //note to self, check the size of the current opcode and give the option to replace the missing or too many bytes with nops
        //and put in a option to disable showing that message, and use a default action

        // get old security and set new security   (not needed in win9x but nt doesnt even allow writeprocessmemory to do this
        original:=0;

        bytelength:=length(bytes);

        if oldbp<>nil then
          debuggerthread.UnsetBreakpoint(oldbp);

        a:=0;
        if fcr3=0 then
        begin
          if SystemSupportsWritableExecutableMemory or SkipVirtualProtectEx then
            vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(processhandle,  pointer(Address),bytelength,PAGE_EXECUTE_READWRITE,p)
          else
          begin
            outputdebugstring('First making memory writable');
            if processid<>GetCurrentProcessId then
              ntsuspendProcess(processhandle);
            vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(processhandle,  pointer(Address),bytelength,PAGE_READWRITE,p)
          end;
          WriteProcessMemoryWithCloakSupport(processhandle,pointer(Address),@bytes[0],bytelength,a);

          if vpe then
          begin
            VirtualProtectEx(processhandle,pointer(Address),bytelength,p,p);
            outputdebugstring('restoring back to the original protection: '+p.ToString);
          end;

          if (not (SystemSupportsWritableExecutableMemory or SkipVirtualProtectEx)) and (processid<>GetCurrentProcessId) then
            ntresumeProcess(processhandle);
        end
        else
        begin
          {$ifdef windows}
          WriteProcessMemoryCR3(fcr3, pointer(address),@bytes[0], bytelength,a);
          {$endif}
        end;

        if (a>0) and (oldbp<>nil) then
        begin
          oldbp^.originalbyte:=bytes[0];
          debuggerthread.SetBreakpoint(oldbp);
        end;

        hexview.update;
        disassemblerview.Update;

        {$ifdef darwin}
        SetFocus;
        disassemblerview.SetFocus;
        {$endif}
      end else raise exception.create(Format(rsIDonTUnderstandWhatYouMeanWith, [assemblercode]));


    end;


  finally
    if oldbp<>nil then  //still needs to be unlocked
      debuggerthread.unlockbplist;
  end;
end;

procedure TMemoryBrowser.Assemble1Click(Sender: TObject);
begin
  try
    AssemblePopup('');
  except
    on e: exception do
      MessageDlg(e.message, mtError, [mbok],0);
  end;
end;

procedure TMemoryBrowser.HexEditKeyPress(Sender: TObject; var Key: Char);
begin
  case key of
    chr(8)   : ;
    chr(16)  : ;
    'A'..'F' : ;
    'a'..'f' : key:=uppercase(key)[1];
    '0'..'9' : ;
    else key:=chr(0);
  end;

  if editing then key:=#0;
  editing:=false;
end;

procedure TMemoryBrowser.HexEditExit(Sender: TObject);
begin

end;

procedure TMemoryBrowser.HexEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

begin
end;

procedure TMemoryBrowser.miShowIndisassemblerClick(Sender: TObject);
begin
  if context<>nil then
    disassemblerview.SelectedAddress:=PContextElement_register(pmRegisters.PopupComponent.Tag)^.getValue(context);
end;


procedure TMemoryBrowser.RegisterLabelDblClick(Sender: TObject);
var x: dword;
    i: integer;
    regname,input: string;
    value: qword;
    rbase: string;
    context: PContext;
    labeltext: string;

    r: PContextElement_register;

begin
  regname:='';
  labeltext:=tlabel(sender).Caption;

  if (debuggerthread<>nil) and (debuggerthread.isWaitingToContinue) then
  begin
    r:=PContextElement_register(tlabel(Sender).Tag);
    regname:=r^.name;

    input:=r^.getValueString(debuggerthread.CurrentThread.context);
    if inputquery(rsChangeRegister, Format(rsWhatIsTheNewValueOf, [regname]), input) then
    begin
      if debuggerthread.CurrentThread<>nil then
      begin
        r^.setValue(debuggerthread.CurrentThread.context, input);
        tlabel(Sender).caption:=padleft(regname, contexthandler.GeneralPurposeRegisterMaxCharCount)+' '+r^.getFullValueString(debuggerthread.CurrentThread.context);
      end;
    end;
  end;

end;

procedure TMemoryBrowser.miDebugBreakClick(Sender: TObject);
//var threadhandle: thandle;
var
  threadlist: TList;
  needform: boolean;
  threadhandle: thandle;
  i: integer;
begin
  if not startdebuggerifneeded then exit;

  threadhandle:=0;
  needform:=false;
  threadlist:=debuggerthread.lockThreadlist;
  try
    if threadlist.count=1 then
    begin
      threadhandle:=TDebugThreadHandler(threadlist[0]).handle;

    end
    else
    if threadlist.count>1 then
      needform:=true
    else exit; //no threads

  finally
    debuggerthread.unlockThreadlist;
  end;

  if needform then
  begin
    //multiple threads!!! Ask user which one
    frmbreakthread:=tfrmbreakthread.create(self);
    if frmbreakthread.showmodal=mrOK then  threadhandle:=frmbreakthread.threadhandle else
    begin
      frmbreakthread.free;
      frmbreakthread:=nil;
      exit;
    end;

    frmbreakthread.free;
    frmbreakthread:=nil;

  end;

  if threadhandle<>0 then
  begin
    threadlist:=debuggerthread.lockThreadlist;
    try
      //find the thread with this handle and tell it to break
      for i:=0 to threadlist.count-1 do
        if TDebugThreadHandler(threadlist[i]).handle=threadhandle then
        begin
          TDebugThreadHandler(threadlist[i]).BreakThread;
          exit;
        end;
    finally
      debuggerthread.unlockThreadlist;
    end;
  end;
end;

procedure TMemoryBrowser.Reservememory1Click(Sender: TObject);
var count: string;
    memsize: dword;
    baseaddress: pointer;
    x: dword;
    s: string;
begin
  count:='4096';
  if inputquery(rsAllocateMemory, rsHowMuchMemoryDoYouWantToAddToThisProcess, count) then
  begin
    try
      memsize:=StrToInt(count);
    except
      raise exception.Create(Format(rsHowMuchIs, [count]));
    end;

    baseaddress:=nil;

    baseaddress:=VirtualAllocEx(processhandle,nil,memsize,MEM_COMMIT or MEM_RESERVE,PAGE_EXECUTE_READWRITE);
    if baseaddress=nil then
      raise exception.Create(rsErrorAllocatingMemory);

    if allocsAddToUnexpectedExceptionList then
      AddUnexpectedExceptionRegion(ptruint(baseaddress),memsize);

    if (disassemblerview.SelectedAddress<>0) and (memsize>7) and (messagedlg(Format(rsAtLeastBytesHaveBeenAllocatedAtDoYouWantToGoThereN, [IntToStr(memsize), IntToHex(ptrUint(baseaddress), 8), #13
      +#10]), mtConfirmation, [mbyes, mbno], 0)=mryes) then
      disassemblerview.SelectedAddress:=ptrUint(baseaddress);
  end;
end;

procedure TMemoryBrowser.Savememoryregion1Click(Sender: TObject);
{will save a cheat engine memory region file .CEM}
begin
  if frmSaveMemoryRegion=nil then
    frmSaveMemoryRegion:=TFrmSaveMemoryRegion.create(self);

  if (disassemblerview.SelectedAddress <> disassemblerview.SelectedAddress2) then //disassembler view selection takes priority
  begin
     frmSaveMemoryRegion.editFrom.Text:=inttohex(minX(disassemblerview.SelectedAddress,disassemblerview.SelectedAddress2),8);
     frmSaveMemoryRegion.editTo.Text:=inttohex(maxX(disassemblerview.SelectedAddress,disassemblerview.SelectedAddress2),8);
  end
  else if (hexview.SelectionStart <> hexview.SelectionStop) then
  begin
     frmSaveMemoryRegion.editFrom.Text:=inttohex(minX(hexview.SelectionStart,hexview.SelectionStop),8);
     frmSaveMemoryRegion.editTo.Text:=inttohex(maxX(hexview.SelectionStart,hexview.SelectionStop),8);
  end
  else
  begin
     //leave blank
  end;

  frmSaveMemoryRegion.show;
end;

procedure TMemoryBrowser.Loadmemolryregion1Click(Sender: TObject);
begin

  if openmemory.Execute then
  begin
    tfrmloadmemory.Create(self).Showmodal(openmemory.filename);
  end;
end;

procedure TMemoryBrowser.HexEditDblClick(Sender: TObject);
begin
  change1.Click;
end;

procedure TMemoryBrowser.Debugstrings1Click(Sender: TObject);
begin
  {$ifndef net}

  if startdebuggerifneeded then
    formdebugstrings.show;
  {$endif}
end;

procedure TMemoryBrowser.TextEditExit(Sender: TObject);
var bt: byte;
    aw: dword;
begin
end;

procedure TMemoryBrowser.TextEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
end;

procedure TMemoryBrowser.CreateThread1Click(Sender: TObject);
var startaddress: ptrUint;
    parameter: ptrUint;
    ThreadID: dword;
    start:string;
    param:string;
begin
  start:=IntToHex(disassemblerview.SelectedAddress,8);
  param:='0';
  if not InputQuery(rsCreateRemoteThread, rsWhatWillBeTheStartaddressOfThisThread, start) then exit;
  try
    startaddress:=StrToQWordEx('$'+start);
  except
    raise exception.Create(rsPleaseEnterAValidHexadecimalAddres);
  end;

  if not InputQuery(rsCreateRemoteThread, rsYouWantToGiveAnAdditional32BitParameterWillShowUpI, param) then exit;
  try
    parameter:=StrToInt('$'+param);
  except
    raise exception.Create(rsPleaseEnterAValidHexadecimalValue);
  end;

  if CreateRemoteThread(processhandle,nil,0,pointer(startaddress),pointer(parameter),0,threadid)=0 then raise exception.Create(rsMBCreationOfTheRemoteThreadFailed) else showmessage(rsMBThreadCreated);
end;

procedure TMemoryBrowser.MemoryRegions1Click(Sender: TObject);
begin
  formmemoryregions:=tformmemoryregions.Create(self);
  formmemoryregions.show;
end;


procedure TMemoryBrowser.TextEditKeyPress(Sender: TObject; var Key: Char);
begin

end;

procedure TMemoryBrowser.FillMemory1Click(Sender: TObject);
begin

  frmFillMemory:=TFrmFillMemory.create(self);
  with frmFillMemory do
  begin
    edit1.Text:=IntToHex(disassemblerview.SelectedAddress,8);
    edit2.Text:=IntToHex(disassemblerview.SelectedAddress+1,8);
    frmFillMemory.showmodal;
  end;

end;

procedure TMemoryBrowser.Disectwindow1Click(Sender: TObject);
begin

  {$ifndef net}
  frmdissectWindow:=tfrmdissectWindow.create(self);
  frmdissectWindow.showmodal;
  {$endif}
end;

procedure TMemoryBrowser.Savedisassemledoutput1Click(Sender: TObject);
var x,y: string;
    start,stop: ptrUint;
    output: textfile;

begin

  with tfrmSavedisassembly.create(self) do
  begin
    start:=minX(disassemblerview.SelectedAddress,disassemblerview.SelectedAddress2);
    stop:=maxX(disassemblerview.SelectedAddress,disassemblerview.SelectedAddress2);
    disassemble(stop);
    edit1.Text:=inttohex(start,8);
    edit2.Text:=inttohex(stop,8);
    show;
  end;
end;

procedure TMemoryBrowser.Heaps1Click(Sender: TObject);
begin
  if processid=0 then
  begin
    messagedlg(rsPleaseTargetAProcessFirst, mterror, [mbOK], 0);
    exit;
  end;

  if processid=GetCurrentProcessId then
  begin
    messagedlg(rsPleaseTargetAnotherProcess, mterror, [mbOK], 0);
    exit;
  end;


  if (frmMemoryAllocHandler<>nil) and (frmMemoryAllocHandler.hookedprocessid<>processid) then
    freeandnil(frmMemoryAllocHandler);

  if frmheaps=nil then
    frmheaps:=tfrmheaps.create(self);

  frmheaps.show;
end;

procedure TMemoryBrowser.EnumeratedllsandSymbols1Click(Sender: TObject);
begin
  symhandler.reinitialize;
  
  if frmEnumerateDLLs=nil then
    frmEnumerateDLLs:=tfrmEnumerateDLLs.create(application);

  frmEnumerateDLLs.show;
  frmEnumerateDLLs.enumerate;
end;

procedure TMemoryBrowser.InjectDLL1Click(Sender: TObject);
var dll: string;
    functionname: string;
    dllList: tstringlist;
begin

  functionname:='';
  dll:='';

  {$ifdef darwin}
  OpenDllDialog.DefaultExt:='.dylib';
  OpenDllDialog.Filter:='Dylib-files (*.dylib)|*.dylib|All files (*.*)|*.*';
  {$endif}

  if opendlldialog.Execute then
  begin
    dll:=utf8toansi(opendlldialog.Filename);
    {$ifdef windows}
    if MessageDlg(rsDoYouWantToExecuteAFunctionOfTheDll, mtConfirmation	, [mbyes, mbno], 0)=mryes then
    begin
      dllList:=tstringlist.Create;

      try
        try
          peinfo_getExportList(opendlldialog.filename, dllList);

          with TfrmSelectionList.create(self,dllList) do
          begin
            caption:=rsInjectDll;
            label1.caption:=rsSelectTheFunctionYouWantToExecute;
            if showmodal=mrok then
              if itemindex<>-1 then
                functionname:=selected;

            free;
          end;
        except

        end;
      finally
        dllList.free;
      end;
    end;
    {$else}
    functionname:='';
    {$endif}

    try
      InjectDll(dll,functionname);
      symhandler.reinitialize(true);
      showmessage(rsDLLInjected);
    except
      on e:exception do
        MessageDlg(e.message,mtError,[mbok],0);
    end;
  end;

end;

procedure TMemoryBrowser.AutoInject1Click(Sender: TObject);
begin
  tfrmautoinject.create(self).show;
end;

procedure TMemoryBrowser.Dissectcode1Click(Sender: TObject);
begin

  {$ifndef net}
  if frmdissectcode=nil then
    frmdissectcode:=tfrmDissectcode.create(self);

  if disassemblerview.SelectedAddress2<>disassemblerview.SelectedAddress then
  begin
    frmDissectCode.edtCustomRangeStart.text:=inttohex(MinX(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2),8);
    frmDissectCode.edtCustomRangeStop.text:=inttohex(MaxX(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2),8);
  end
  else
  begin
    frmDissectCode.edtCustomRangeStart.text:='';
    frmDissectCode.edtCustomRangeStop.text:='';
  end;

  frmdissectcode.Show;
  {$endif}
end;

procedure TMemoryBrowser.Createjumptocodecave1Click(Sender: TObject);
begin

end;

procedure TMemoryBrowser.Findstaticpointers1Click(Sender: TObject);
begin

{$ifndef net}
  if frmfindstatics=nil then
    frmfindstatics:=tfrmfindstatics.create(self);

  frmfindstatics.show;
{$endif}
end;

procedure TMemoryBrowser.Scanforcodecaves1Click(Sender: TObject);
begin

  if frmcodecavescanner=nil then
    frmcodecavescanner:=tfrmCodecaveScanner.create(self);

  frmCodecavescanner.show;
end;

procedure TMemoryBrowser.Changestateofregisteratthislocation1Click(
  Sender: TObject);
begin
  tfrmModifyRegisters.create(self,disassemblerview.SelectedAddress).showmodal;
end;

procedure TMemoryBrowser.miTogglebreakpointClick(Sender: TObject);
var
  bpm: TBreakpointMethod;
  b: byte;
  PA: qword;
  bo: integer;
begin
  //first check if it's a cloaked dbvm bp, and if so, disable it
  if dbvm_isBreakpoint(disassemblerview.SelectedAddress, PA, BO, b) then
  begin
    if bo=1 then //changeregonbp
    begin
      dbvm_cloak_removechangeregonbp(PA);
      disassemblerview.Update;
      exit;
    end;

    if bo=4 then //break and trace
    begin
      dbvm_cloak_traceonbp_remove(PA);
      disassemblerview.Update;
      exit;
    end;
  end;

  try
    if startdebuggerifneeded(true) then
    begin
      if overridebreakpointmethod then
        bpm:=preferedF5BreakpointMethod
      else
        bpm:=preferedBreakpointMethod;

      DebuggerThread.ToggleOnExecuteBreakpoint(disassemblerview.SelectedAddress,bpm);
      disassemblerview.Update;

      ApplySourceCodeDebugUpdate;

    end;
  except
    on e:exception do MessageDlg(e.message,mtError,[mbok],0);
  end;
end;

procedure TMemoryBrowser.Breakpointlist1Click(Sender: TObject);
begin
  if frmbreakpointlist=nil then
  begin
    frmbreakpointlist:=tfrmBreakpointlist.create(self);
    frmBreakpointlist.show;
  end
  else
    frmbreakpointlist.bringtofront;
end;




procedure TMemoryBrowser.Dissectdata1Click(Sender: TObject);
begin

{$ifndef net}
  with tfrmpointerscanner.create(self) do
    show;
{$endif}
end;

procedure TMemoryBrowser.miShowSymbolsClick(Sender: TObject);
begin
  symhandler.showsymbols:=miShowSymbols.Checked;
  disassemblerview.Update;
end;

procedure TMemoryBrowser.miShowModuleAddressesClick(Sender: TObject);
begin
  symhandler.showmodules:=miShowModuleAddresses.Checked;
  disassemblerview.Update;
end;

procedure TMemoryBrowser.miShowSectionAddressesClick(Sender: TObject);
begin
  symhandler.showsections:=miShowSectionAddresses.checked;
  disassemblerview.Update;
end;


procedure TMemoryBrowser.miDissectDataClick(Sender: TObject);
begin
  {
  if length(frmStructures)>0 then
  begin
    frmStructures[0].edtAddress.Text:=inttohex(memorybrowser.memoryaddress,8);
    frmStructures[0].Show;
  end
  else
  begin
    //create it
    with tfrmstructures.create(self) do
    begin
      edtAddress.Text:=inttohex(memoryaddress,8);
      applychanges(false);
      show;
    end;
  end; }
end;

procedure TMemoryBrowser.miUserdefinedSymbolsClick(Sender: TObject);
begin
  if frmSymbolhandler=nil then
    frmSymbolhandler:=TfrmSymbolhandler.create(self);

  frmSymbolhandler.show;
end;

procedure TMemoryBrowser.Allocatenonpagedmemory1Click(Sender: TObject);
var count: string;
    memsize: integer;
    baseaddress: pointer;
    x: dword;
    s: string;
begin
  {$ifdef windows}
{$ifndef net}
  count:='4096';
  if inputquery(rsAllocateMemory, rsHowMuchMemoryDoYouWishToAllocate, count) then
  begin
    try
      memsize:=StrToInt(count);
    except
      raise exception.Create(Format(rsHowMuchIs, [count]));
    end;

    baseaddress:=nil;
    baseaddress:=KernelAlloc(memsize);
    if baseaddress=nil then
      raise exception.Create(rsErrorAllocatingMemory);

    if messagedlg(Format(rsAtLeastBytesHaveBeenAllocatedAtGoThereNow, [IntToStr(memsize), IntToHex(ptrUint(baseaddress), 8)]), mtinformation, [mbyes, mbno], 0) = mryes then
      disassemblerview.SelectedAddress:=ptrUint(baseaddress);
  end;
  {$endif}
  {$endif}
end;

procedure TMemoryBrowser.Getaddress1Click(Sender: TObject);
var p: pointer;
    s: string;
    ws: widestring;
    pws: pwidechar;
begin
  {$ifdef windows}
  if inputquery(rsGetKernelAddress, rsGiveTheNameOfTheFunctionYouWantToFindCaseSensitive, s) then
  begin
    ws:=s;
    pws:=@ws[1];
    p:=GetKProcAddress(pws);

    disassemblerview.SelectedAddress:=ptrUint(p);
  end;
  {$endif}
end;

procedure TMemoryBrowser.Findmemory1Click(Sender: TObject);
begin
  search1.Click;
end;

procedure TMemoryBrowser.Assemblycode1Click(Sender: TObject);
var
  fromaddress: ptruint;
  toaddress: ptruint;


begin
  if frmAssemblyScan=nil then
    frmAssemblyScan:=tfrmassemblyscan.create(self);

  frmAssemblyScan.edtFrom.text:=inttohex(disassemblerview.TopAddress,8);

  if processhandler.is64Bit then
    frmAssemblyScan.edtTo.text:='FFFFFFFFFFFFFFFF'
  else
    frmAssemblyScan.edtTo.text:='FFFFFFFF';


  if frmAssemblyScan.ShowModal=mrok then
  begin
    if frmAssemblyScan.mAssemblerSearch.lines.Count=0 then exit;
    with TfrmDisassemblyscan.create(self) do
    begin
      startaddress:=frmAssemblyScan.startaddress;
      stopaddress:=frmAssemblyScan.stopaddress;
      stringstofind:=frmAssemblyScan.mAssemblerSearch.lines;
      show;
    end;

  end;
end;

procedure TMemoryBrowser.Driverlist1Click(Sender: TObject);
begin
  {$ifndef net}

  with tfrmdriverlist.create(self) do
    show;
  {$endif}
end;

procedure TMemoryBrowser.plugintype6click(sender:tobject);
var
  x: TPluginfunctionType6;
  selectedaddress: ptrUint;
begin
  x:=TPluginfunctionType6(tmenuitem(sender).Tag);
  if x<>nil then
  begin
    selectedaddress:=disassemblerview.SelectedAddress;
    x.callback(@selectedaddress);
    disassemblerview.SelectedAddress:=selectedaddress;
  end;
end;

procedure TMemoryBrowser.plugintype1click(sender:tobject);
var x: TPluginfunctionType1;
address: ptrUint;
hexviewaddress: ptrUint;
seladdress: ptruint;
begin
  x:=TPluginfunctionType1(tmenuitem(sender).Tag);
  if x<>nil then
  begin
    address:=disassemblerview.TopAddress;
    hexviewaddress:=memoryaddress;
    seladdress:=disassemblerview.SelectedAddress;
    if x.callback(@address,@seladdress,@hexviewaddress) then
    begin
      disassemblerview.TopAddress:=address;
      disassemblerview.selectedaddress:=seladdress;
      memoryaddress:=hexviewaddress;
    end;
  end;
end;

procedure TMemoryBrowser.Sericedescriptortable1Click(Sender: TObject);
begin
{$ifndef net}

  if frmServiceDescriptorTables=nil then
    frmServiceDescriptorTables:=TfrmServiceDescriptorTables.create(self);

  frmServiceDescriptorTables.show;
{$endif}
end;

procedure TMemoryBrowser.MBCanvasMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TMemoryBrowser.Cut1Click(Sender: TObject);
begin
  hexview.CopySelectionToClipboard;
end;

procedure TMemoryBrowser.Pastefromclipboard1Click(Sender: TObject);
begin
  hexview.PasteFromClipboard;
end;

procedure TMemoryBrowser.Setsymbolsearchpath1Click(Sender: TObject);
var searchpath: string;
begin
  searchpath:=symhandler.getsearchpath;
  if inputquery(rsSymbolHandler, rsPleaseSpecifyTheNewSymbolSearchpathSeperatesPaths, searchpath) then
  begin
    symhandler.setsearchpath(searchpath);

    symhandler.reinitialize(true);
  end;
end;

procedure TMemoryBrowser.miKernelmodeSymbolsClick(Sender: TObject);
begin
{$ifndef net}

  miKernelmodeSymbols.Checked:=not miKernelmodeSymbols.Checked;

  symhandler.kernelsymbols:=miKernelmodeSymbols.Checked;
  symhandler.reinitialize(true);
  //symhandler.waitforsymbolsloaded(false);
{$endif}
end;

procedure TMemoryBrowser.Breakandtraceinstructions1Click(Sender: TObject);
var f: TFrmTracer;
begin
  f:=TFrmTracer.create(self);
  f.show;
  f.miNewTrace.Click;
end;

procedure TMemoryBrowser.debuggerpopupPopup(Sender: TObject);
var x: ptrUint;
  i: integer;
  a,a2: ptruint;
  inadvancedoptions: boolean;
  e: boolean;
  VA,PA: QWORD;
  BO: integer;
  bpm: TBreakpointMethod;
  b: byte;
  dbvmbp: boolean;

  bp: PBreakpoint=nil;
begin

  Breakandtraceinstructions1.Enabled:=processhandle<>0;
  miTogglebreakpoint.Enabled:=processhandle<>0;
  Changestateofregisteratthislocation1.Enabled:=processhandle<>0;
  Findoutwhataddressesthisinstructionaccesses1.enabled:=processhandle<>0;

  follow1.visible:=isjumporcall(disassemblerview.SelectedAddress,x);
  back1.Visible:=disassemblerview.hasbacklist;

  pluginhandler.handledisassemblerContextPopup(disassemblerview.SelectedAddress);

  miConditionalBreak.enabled:=(debuggerthread<>nil) and (debuggerthread.isBreakpoint(disassemblerview.SelectedAddress)<>nil);
  miConditionalBreak.visible:=miConditionalBreak.enabled;

  miDBVMActivateCloak.visible:={$ifdef windows}isRunningDBVM and hasEPTSupport and (not hasCloakedRegionInRange(disassemblerview.SelectedAddress, 1, VA,PA)){$else}false{$endif};
  miDBVMActivateCloak.enabled:=miDBVMActivateCloak.visible;

  miDBVMDisableCloak.visible:={$ifdef windows}isRunningDBVM and hasEPTSupport and (hasCloakedRegionInRange(disassemblerview.SelectedAddress, 1, VA,PA)){$else}false{$endif};
  miDBVMDisableCloak.enabled:=miDBVMDisableCloak.visible;

  miTogglebreakpoint.visible:=(not ischild);

  if miTogglebreakpoint.visible then
  begin
    dbvmbp:=dbvm_isBreakpoint(disassemblerview.SelectedAddress,PA,BO,b);
    if dbvmbp then
      outputdebugstring('DBVMBP=true')
    else
      outputdebugstring('DBVMBP=false');

    if (debuggerthread<>nil) then
      bp:=debuggerthread.isBreakpoint(disassemblerview.SelectedAddress);

    if ((debuggerthread=nil) or (bp=nil)) and (not dbvmbp) then
    begin
      if overridebreakpointmethod then
        bpm:=preferedF5BreakpointMethod
      else
        bpm:=preferedBreakpointMethod;

      miTogglebreakpoint.caption:=rsSetBreakpoint+' ('+breakpointMethodToString(bpm)+')';

      Changestateofregisteratthislocation1.visible:=true; //setting of bp's is OK
      miSetSpecificBreakpoint.visible:=true;
      Breakandtraceinstructions1.visible:=true;
      Findoutwhataddressesthisinstructionaccesses1.visible:=true;
    end
    else
    begin
      miTogglebreakpoint.caption:=rsRemoveBreakpoint;
      Changestateofregisteratthislocation1.visible:=(bp<>nil) and (bp^.breakpointAction=bo_ChangeRegister);
      miSetSpecificBreakpoint.visible:=false;
      Breakandtraceinstructions1.visible:=false;
      Findoutwhataddressesthisinstructionaccesses1.visible:=false;
    end;
  end
  else
  begin
    Changestateofregisteratthislocation1.visible:=false;
    miSetSpecificBreakpoint.visible:=false;
    Breakandtraceinstructions1.visible:=false;
    Findoutwhataddressesthisinstructionaccesses1.visible:=false;
  end;



  Changestateofregisteratthislocation1.enabled:=Changestateofregisteratthislocation1.visible;


  inadvancedoptions:=false;

  for i:=0 to AdvancedOptions.count-1 do
  begin
    if AdvancedOptions.code[i]<>nil then
    begin
      a:=symhandler.getAddressFromName(AdvancedOptions.code[i].symbolname,false,e);
      if not e then
      begin
        if InRangeX(disassemblerview.SelectedAddress, a, a+length(AdvancedOptions.code[i].actualopcode)-1 ) then
        begin
          inadvancedoptions:=true;

          if AdvancedOptions.code[i].changed then
            begin
              miReplacewithnops.caption:=rsRestoreWithOrginalCode;
              miReplacewithnops.imageindex:=44;
            end
          else
            begin
              miReplacewithnops.caption:=rsReplaceWithCodeThatDoesNothing;
              miReplacewithnops.imageindex:=42;
            end;

          break;
        end;
      end;
    end;
  end;

  if not inadvancedoptions then
    miReplacewithnops.caption:=rsReplaceWithCodeThatDoesNothing;

  miAddToTheCodelist.visible:=not inadvancedoptions;

  miDBVMFindoutwhataddressesthisinstructionaccesses.visible:={$ifdef windows}isDBVMCapable and miSetSpecificBreakpoint.visible{$else}false{$endif};
  miDBVMFindoutwhataddressesthisinstructionaccesses.enabled:=miDBVMFindoutwhataddressesthisinstructionaccesses.visible;
  menuitem5.visible:=miDBVMFindoutwhataddressesthisinstructionaccesses.visible;
  menuitem6.visible:=miDBVMFindoutwhataddressesthisinstructionaccesses.visible;

  //
  miSetBreakpointHW.enabled:=(CurrentDebuggerInterface=nil) or (dbcHardwareBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities);
  miSetBreakpointSW.enabled:=((CurrentDebuggerInterface=nil) and (not formsettings.cbKDebug.Checked)) or ((CurrentDebuggerInterface<>nil) and (dbcSoftwareBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities));
  miSetBreakpointPE.enabled:=((CurrentDebuggerInterface=nil) and (not formsettings.cbKDebug.Checked)) or ((CurrentDebuggerInterface<>nil) and (dbcExceptionBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities));
  miSetBreakpointDBVMExec.enabled:=((CurrentDebuggerInterface=nil) and (formsettings.cbKDebug.Checked)) or ((CurrentDebuggerInterface<>nil) and (dbcDBVMBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities));

  miSetBreakpointDBVMExec.visible:={$ifdef windows}hasEPTSupport{$else}false{$endif};


  if logWrites then
  begin
    a:=minX(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2);
    a2:=maxX(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2);

    if a2=a then
      disassemble(a2);

    miUndoLastEdit.visible:=hasAddressBeenChangedRange(a,a2);
  end
  else
    miUndoLastEdit.visible:=false;
end;

procedure TMemoryBrowser.GDTlist1Click(Sender: TObject);
begin

  Tfrmgdtinfo.create(self).show;
end;

procedure TMemoryBrowser.IDTlist1Click(Sender: TObject);
begin

  TfrmIDT.create(self).show;
end;



procedure TMemoryBrowser.FormDestroy(Sender: TObject);
var
  h0,h1,h2,h3: integer;
  params: array of integer;

  reg: TRegistry;

  v: integer;

begin
  MemoryBrowsers.Remove(self);

  if strace<>nil then
    strace.free;

  if disassemblerHistory<>nil then
    freeandnil(disassemblerHistory);

  if memorybrowserhistory<>nil then
    freeandnil(memorybrowserHistory);

  if assemblerHistory<>nil then
    freeandnil(assemblerHistory);

  //save position of window and other stuff
  //membrowser comes after formsettings so is destroyed before formsettings, so valid
  if hasBeenShown then
  begin
    if self.disassemblerview<>nil then
    begin
      setlength(params,12);
      //don't use [xx,xx,xx] crash
      params[0]:=self.disassemblerview.getheaderwidth(0);
      params[1]:=self.disassemblerview.getheaderwidth(1);
      params[2]:=self.disassemblerview.getheaderwidth(2);
      params[3]:=self.disassemblerview.getheaderwidth(3);
      params[4]:=self.panel1.height;
      params[5]:=self.registerview.width;
      params[6]:=strtoint(BoolToStr(self.miShowSymbols.checked,'1','0'));

      v:=0;
      if self.miShowModuleAddresses.checked then v:=v or 1;
      if self.miShowSectionAddresses.checked then v:=v or 2;
      params[7]:=v;
      //params[8]:=strtoint(BoolToStr(self.miLockRowsize.Checked,'1','0')); 7.4: obsolete
      params[9]:=self.hexview.LockedRowSize;
      params[10]:=strtoint(BoolToStr(self.Kernelmodesymbols1.checked,'1','0'));

      params[11]:=pnlStacktrace.Width;

      saveformposition(self,params);
    end;
  end;

  if self.disassemblerview<>nil then
    freeandnil(self.disassemblerview);

  if self.hexview<>nil then
    freeandnil(self.hexview);



end;

procedure TMemoryBrowser.Newwindow1Click(Sender: TObject);
var
  s: string;
  ns: string;
  i: integer;
begin
  with tmemorybrowser.create(application) do
  begin
    inc(mbchildcount);
    //name:=rsMemoryBrowser+inttostr(mbchildcount);
    debug1.Visible:=false;
    debug1.enabled:=false;
    //registerview.Visible:=false;
    //splitter2.Visible:=false;
    sbShowFloats.Visible:=false;
    miDebugToggleBreakpoint.visible:=false;

    s:=name;
    ns:='';
    for i:=length(s) downto 1 do
    begin
      if s[i] in ['0'..'9'] then
        ns:=s[i]+ns
      else
        break;
    end;

    caption:=caption+'* ('+ns+')';

    Kerneltools1.enabled:=memorybrowser.Kerneltools1.enabled;
    miCR3Switcher.visible:=Kerneltools1.Enabled;
    ischild:=true;
    show;
  end;
end;


procedure TMemoryBrowser.Follow1Click(Sender: TObject);
{
will change the selected disassembler address to the address this instructions jump so if it is an jump instruction
}
var address: ptrUint;
begin
  if isjumporcall(disassemblerview.SelectedAddress,address) then
    disassemblerview.SelectedAddress:=address;
end;



procedure TMemoryBrowser.copyBytesAndOpcodesClick(Sender: TObject);
var a,b: ptrUint;
    _tag: integer;
begin


  _tag:=(sender as tmenuitem).Tag;
  if _tag=0 then raise exception.create('Build environment corrupted');

  with tfrmSavedisassembly.create(self) do
  begin

    a:=minX(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2);
    b:=maxX(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2);
    
    disassemble(b); //b gets increased with size of selected instruction
    edit1.Text:=inttohex(a,8);
    edit2.Text:=inttohex(b-1,8);
    copymode:=true;

    cbAddress.checked:=not (_tag in [4,7]);
    cbBytes.checked:=_tag in [1,2,4,6];
    cbOpcode.checked:=_tag in [1,3,6,7];
    cbComment.checked:=(_tag=1);
    
    button1.click;
    waittilldone;

    free;
  end;


end;

procedure TMemoryBrowser.DissectPEheaders1Click(Sender: TObject);
begin

  with TfrmPEInfo.create(self) do
    show;
end;

procedure TMemoryBrowser.GetEntryPointAndDataBase(var code: ptrUint; var data: ptrUint);
var modulelist: tstringlist=nil;
    base: ptrUint;
    header: pointer=nil;
    headersize: dword;
    br: ptrUint;
    d: ptruint;
    sectionlist: Tstringlist;
    i: integer;
begin
  code:=$00400000;
  data:=$00400000; //on failure

  modulelist:=tstringlist.Create;
  symhandler.getModuleList(modulelist);
  outputdebugstring('Retrieved the module list');

  if modulelist.Count>0 then
  begin
    base:=ptrUint(modulelist.Objects[0]);
    data:=base;
    code:=base;

    outputdebugstring('Base='+inttohex(base,8));


    getmem(header,4096);
    try
      if readprocessmemory(processhandle,pointer(base),header,4096,br) then
      begin
        headersize:=peinfo_getheadersize(header);

        if headersize>0 then
        begin
          OutputDebugString('headersize='+inttostr(headersize));
          if headersize>1024*512 then exit;

          freememandnil(header);
          getmem(header,headersize);
          if not readprocessmemory(processhandle,pointer(base),header,headersize,br) then exit;

          Outputdebugstring('calling peinfo_getEntryPoint');
          code:=base+peinfo_getEntryPoint(header, headersize);

          OutputDebugString('calling peinfo_getdatabase');
          d:=peinfo_getdatabase(header, headersize);
          if d=0 then
          begin
            {$ifndef darwin}
            sectionlist:=TStringList.Create;
            if peinfo_getSectionList(base,sectionList) then
            begin
              for i:=0 to sectionlist.count-1 do
              begin
                if sectionlist[i]='.data' then
                  data:=TSectionInfo(sectionlist.Objects[i]).virtualAddress;

                TSectionInfo(sectionlist.Objects[i]).free;
              end;
            end;

            sectionlist.free;
            {$endif}
          end
          else
            data:=base+d;
        end;


      end;
    finally
      if header<>nil then
        freememandnil(header);

      header:=nil;
    end;
  end;
  modulelist.free;
end;

procedure TMemoryBrowser.setcodeanddatabase;
var code,data: ptrUint;
begin
  if processid=$ffffffff then  //file instead of process
  begin
    code:=0;
    data:=0;
  end
  else
    GetEntryPointAndDataBase(code,data);

  disassemblerview.SelectedAddress:=code;
  memoryaddress:=data;


end;


procedure TMemoryBrowser.Back1Click(Sender: TObject);
begin
  disassemblerview.GoBack;
end;

procedure TMemoryBrowser.Showvaluesofstaticaddresses1Click(
  Sender: TObject);
begin
  showvalues:=not showvalues;
end;


procedure TMemoryBrowser.FindwhatThiscodeAccesses(address: ptrUint);
begin
  if not startdebuggerifneeded then exit;
  if debuggerthread<>nil then
    debuggerthread.FindWhatCodeAccesses(address);
end;

procedure TMemoryBrowser.DBVMFindwhatThiscodeAccesses(address: ptruint);
const
  IA32_VMX_BASIC_MSR=$480;
  IA32_VMX_TRUE_PROCBASED_CTLS_MSR=$48e;
  IA32_VMX_PROCBASED_CTLS_MSR=$482;
  IA32_VMX_PROCBASED_CTLS2_MSR=$48b;
var
  address2: ptrUint;
  res: word;
  id: integer;

  frmchangedaddresses: tfrmchangedaddresses;
  unlockaddress: qword;
  canuseept: boolean;

  s: string;
  procbased1flags: dword;
  i: integer;
begin
  {$ifdef windows}
  LoadDBK32;
  canuseept:=hasEPTSupport;


  if canuseept=false then
  begin
    messagedlg('This function requires that your CPU supports ''Extended Page Table (EPT)'' which your CPU lacks', mtError,[mbok],0);
    exit;
  end;

  if loaddbvmifneeded('DBVM find routines needs DBVM for EPT page hooking. Loading DBVM can potentially cause a system freeze. Are you sure?') then
  begin
    if not loaddbvmifneeded then exit;

    address2:=address;
    s:=disassemble(address2);

    //spawn a DBVM watch config screen where the user can select options like lock memory
    if frmDBVMWatchConfigFindWhatCodeAccesses=nil then
      frmDBVMWatchConfigFindWhatCodeAccesses:=TfrmDBVMExecuteWatchConfig.create(self);

    frmDBVMWatchConfigFindWhatCodeAccesses.address:=address;
    frmDBVMWatchConfigFindWhatCodeAccesses.rbExecuteAccess.checked:=true;
    frmDBVMWatchConfigFindWhatCodeAccesses.gbAccessType.visible:=false;

    frmDBVMWatchConfigFindWhatCodeAccesses.cbMultipleRIP.checked:=true;
    frmDBVMWatchConfigFindWhatCodeAccesses.cbMultipleRIP.Visible:=false;
    frmDBVMWatchConfigFindWhatCodeAccesses.cbWholePage.Visible:=false;

    if frmDBVMWatchConfigFindWhatCodeAccesses.showmodal=mrok then
    begin
      if frmDBVMWatchConfigFindWhatCodeAccesses.LockPage then
        unlockaddress:=LockMemory(processid, address and QWORD($fffffffffffff000),4096)
      else
        unlockaddress:=0;

      id:=dbvm_watch_executes(frmDBVMWatchConfigFindWhatCodeAccesses.PhysicalAddress, address2-address, frmDBVMWatchConfigFindWhatCodeAccesses.Options, frmDBVMWatchConfigFindWhatCodeAccesses.MaxEntries);

      if (id<>-1) then
      begin
        //spawn a frmchangedaddresses
        frmchangedaddresses:=tfrmChangedAddresses.Create(application);

        if frmDBVMWatchConfigFindWhatCodeAccesses.LockPage then
          unlockaddress:=LockMemory(processid, address and QWORD($fffffffffffff000),4096)
        else
          unlockaddress:=0;

        frmchangedaddresses.address:=address;

        frmchangedaddresses.dbvmwatchid:=id;
        frmchangedaddresses.dbvmwatch_unlock:=unlockaddress;
        if defaultDisassembler.LastDisassembleData.isfloat then
          frmchangedaddresses.cbDisplayType.ItemIndex:=3;

        if uppercase(defaultDisassembler.LastDisassembleData.opcode)='RET' then
        begin
          if processhandler.is64Bit then
            s:='[RSP]'
          else
            s:='[ESP]';
        end
        else
        begin
          i:=pos('[',s)+1;
          if i<>0 then
            s:=copy(s,i,pos(']',s)-i)
          else
          begin
            //no [   ] part
            if processhandler.is64Bit then
              s:='RDI'
            else
              s:='EDI';
          end;
        end;

        frmchangedaddresses.equation:=s;

        frmchangedaddresses.show;
      end
      else
        MessageDlg('dbvm_watch failed', mtError, [mbok],0);

    end;
  end;
  {$endif}
end;

procedure TMemoryBrowser.Findoutwhataddressesthisinstructionaccesses1Click(
  Sender: TObject);
begin
  try
    findWhatthisCodeAccesses(disassemblerview.SelectedAddress);

  except
    on e: Exception do
    begin
      MessageDlg(e.message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMemoryBrowser.sbShowFloatsClick(Sender: TObject);
var
  x: tpoint;
  z: trect;
  newleft: integer;
begin

  if frmFloatingPointPanel=nil then
    frmFloatingPointPanel:=TfrmFloatingPointPanel.create(self);


  newleft:=left+Width;
  if newleft+frmFloatingPointPanel.Width>screen.Width then newleft:=screen.width-frmFloatingPointPanel.width;

  frmFloatingPointPanel.Left:=newleft;
  frmFloatingPointPanel.Top:=self.top+(self.ClientOrigin.y-self.top)-(frmFloatingPointPanel.ClientOrigin.y-frmFloatingPointPanel.top);
  frmFloatingPointPanel.ClientHeight:=scrollbox1.Height;

  frmFloatingPointPanel.SetContextPointer(debuggerthread.CurrentThread.context);

  frmFloatingPointPanel.show;//pop to foreground
end;

procedure TMemoryBrowser.ScriptConsole1Click(Sender: TObject);
begin

end;

procedure TMemoryBrowser.DisplayTypeClick(Sender: TObject);
var x: tmenuitem;
begin
//vtByte, vtWord, vtDword, vtDwordDec, vtSingle, vtDouble
  if (sender is TMenuItem) then
  begin
    x:=TMenuItem(sender);
    case x.tag of
      0: hexview.DisplayType:=dtByte;
      1: hexview.DisplayType:=dtByteDec;
      2: hexview.DisplayType:=dtWord;
      3: hexview.DisplayType:=dtWordDec;
      4: hexview.DisplayType:=dtDword;
      5: hexview.DisplayType:=dtDwordDec;
      6: hexview.DisplayType:=dtQword;
      7: hexview.DisplayType:=dtQwordDec;
      8: hexview.DisplayType:=dtsingle;
      9: hexview.DisplayType:=dtDouble;
      else
      begin
        if x.tag>=$1000 then
        begin
          hexview.CustomType:=TCustomType(customtypes[x.tag-$1000]);  //always set first
          hexview.DisplayType:=dtCustom;
        end;


      end;
    end;
  end;
end;

procedure TMemoryBrowser.Showjumplines1Click(Sender: TObject);
begin
  showjumplines1.checked:=not showjumplines1.checked;
  disassemblerview.showjumplines:=showjumplines1.checked;

  Onlyshowjumplineswithinrange1.Enabled:=showjumplines1.checked;
end;

procedure TMemoryBrowser.Onlyshowjumplineswithinrange1Click(
  Sender: TObject);
begin
  Onlyshowjumplineswithinrange1.checked:=not Onlyshowjumplineswithinrange1.checked;
  if Onlyshowjumplineswithinrange1.checked then
    disassemblerview.ShowJumplineState:=jlsOnlyWithinRange
  else
    disassemblerview.ShowJumplineState:=jlsAll;

end;

procedure TMemoryBrowser.View1Click(Sender: TObject);
begin
  miIPTLog.visible:=systemSupportsIntelPT and not hideiptcapability;
  miIPTLog.enabled:=debuggerthread<>nil;
end;

procedure TMemoryBrowser.Watchmemoryallocations1Click(Sender: TObject);
begin
  if processid=0 then
  begin
    MessageDlg(rsPleaseTargetAProcessFirst, mtError,[mbok], 0);
    exit;
  end;

  if processid=GetCurrentProcessId then
  begin
    MessageDlg(rsPleaseTargetAnotherProcess, mtError,[mbok], 0);
    exit;
  end;


  if (frmMemoryAllocHandler<>nil) and (frmMemoryAllocHandler.hookedprocessid<>processid) then
    freeandnil(frmMemoryAllocHandler);


  if frmMemoryAllocHandler=nil then
    frmMemoryAllocHandler:=TfrmMemoryAllocHandler.Create(self);

  frmMemoryAllocHandler.Show;
end;

procedure TMemoryBrowser.reloadStacktrace;
var s: pptrUintarray;
    x: ptrUint;
    
    i: integer;
    address, bytes, details: string;
    li: tlistitem;
    c: TListcolumn;

    refname: string;
    refaddress: ptruint;
begin

  lvStacktraceData.Items.BeginUpdate;
  try
    if stacktrace2.Checked then
    begin
      //setup view for stacktrace if it isn't setup yet
      if lvStacktraceData.columns.Count=3 then
      begin
        lvstacktracedata.Columns.BeginUpdate;
        try
          lvstacktracedata.Columns.Clear;
          c:=lvstacktracedata.Columns.Add;
          c.Caption:=rsReturnAddress;
          c.Width:=lvstacktracedata.Canvas.TextWidth('DDDDDDDD');
          c.AutoSize:=true;


          c:=lvstacktracedata.Columns.Add;
          c.Caption:=rsParameters;
          c.Width:=200;
          c.AutoSize:=true;

        finally
          lvstacktracedata.Columns.EndUpdate;
        end;
      end;

      if frmstacktrace=nil then
        frmstacktrace:=tfrmstacktrace.create(self);


      lvstacktracedata.Items.Count:=frmstacktrace.ListView1.Items.Count;
    end
    else
    begin
      //setup view for stackview if it isn't setup yet
      if lvStacktraceData.columns.Count<>3 then
      begin
        lvstacktracedata.Columns.BeginUpdate;
        try
          lvstacktracedata.Columns.Clear;
          c:=lvstacktracedata.Columns.Add;
          c.Caption:=rsAddress;
          c.Width:=80;
          c:=lvstacktracedata.Columns.Add;
          if processhandler.is64bit then
          begin
            c.Caption:='QWORD';
            c.width:=160;
          end
          else
          begin
            c.Caption:='DWORD';
            c.Width:=80;
          end;

          c:=lvstacktracedata.Columns.Add;
          c.Caption:=rsValue;
          c.Width:=100;
          c.AutoSize:=true;
        finally
          lvstacktracedata.Columns.EndUpdate;
        end;
      end;

      if all1.checked =false then
      begin
        //just get the list
        getmem(s,FStacktraceSize);
        try

          readprocessmemory(processhandle, pointer(contexthandler.StackPointerRegister^.getValue(context)),s, FStacktraceSize,x);
          strace.Clear;

          if miAddESP.checked then
          begin
            refname:=lowercase(contexthandler.StackPointerRegister^.name);
            refaddress:=contexthandler.StackPointerRegister^.getValue(context);
          end
          else
          if (processhandler.SystemArchitecture=archX86) and miAddEBP.checked then
          begin
            refname:='rbp';
            if not processhandler.is64Bit then
              refname[1]:='e';

            refaddress:=contexthandler.getRegister(refname)^.getValue(context);
          end
          else
          if miAddRef.checked then
          begin
            refname:=' ';
            refaddress:=StackReference;
          end;

          if processhandler.SystemArchitecture=archX86 then
            ce_stacktrace(contexthandler.StackPointerRegister^.getValue(context), pcontext(context)^.{$ifdef cpu64}rbp{$else}ebp{$endif}, contexthandler.InstructionPointerRegister^.getValue(context), pbytearray(s),x, strace,false,Nonsystemmodulesonly1.checked or modulesonly1.Checked,Nonsystemmodulesonly1.checked,0,refaddress,refname);


          lvstacktracedata.Items.Count:=strace.Count;
        finally
          freememandnil(s);

        end;
      end else
      begin
        lvstacktracedata.Items.Count:=(FStacktraceSize div processhandler.pointersize)-1;
      end;

    end;
  finally
    lvStacktraceData.Refresh;
    lvStacktraceData.Items.EndUpdate;
  end;

end;

procedure TMemoryBrowser.Maxstacktracesize1Click(Sender: TObject);
var
  s: string;
begin
  s:=inttostr(stacktraceSize);
  InputQuery(rsStacktrace, rsNewSize+':', s);
  try
    stacktraceSize:=strtoint(s);
    Maxstacktracesize1.Caption:=rsMaxStacktraceSize+': '+inttostr(stacktracesize);
  except
  end;
end;

procedure TMemoryBrowser.All1Click(Sender: TObject);
begin
  all1.checked:=true;
  Modulesonly1.Checked:=false;
  Nonsystemmodulesonly1.Checked:=false;
  stacktrace2.Checked:=false;
  reloadstacktrace;
end;

procedure TMemoryBrowser.Modulesonly1Click(Sender: TObject);
begin
  all1.checked:=false;
  Modulesonly1.Checked:=true;
  Nonsystemmodulesonly1.Checked:=false;
  stacktrace2.Checked:=false;
  reloadstacktrace;
end;

procedure TMemoryBrowser.Nonsystemmodulesonly1Click(Sender: TObject);
begin
  all1.checked:=false;
  Modulesonly1.Checked:=false;
  Nonsystemmodulesonly1.Checked:=true;
  stacktrace2.Checked:=false;
  reloadstacktrace;
end;

procedure TMemoryBrowser.stacktrace2Click(Sender: TObject);
begin
  all1.checked:=false;
  Modulesonly1.Checked:=false;
  Nonsystemmodulesonly1.Checked:=false;
  stacktrace2.Checked:=true;
  reloadstacktrace;
end;

procedure TMemoryBrowser.miReferencedFunctionsClick(Sender: TObject);
begin
  if (dissectcode=nil) then
  begin
    if MessageDlg(rsNeedToRunDissectCode, mtConfirmation, [mbyes, mbno], 0)=mryes then
    begin
      Dissectcode1Click(sender);
      frmDissectCode.ondone:=odOpenReferedFunctionsList;
      frmDissectCode.btnStart.click;
    end;
  end else
  begin
    if frmReferencedFunctions=nil then
      frmReferencedFunctions:=tfrmReferencedFunctions.Create(self);

    frmReferencedFunctions.Show;
  end;
end;

procedure TMemoryBrowser.Referencedstrings1Click(Sender: TObject);
begin

  if (dissectcode=nil) then
  begin
    if MessageDlg(rsNeedToRunDissectCode, mtConfirmation, [mbyes, mbno], 0)=mryes then
    begin
      Dissectcode1Click(sender);
      frmDissectCode.ondone:=odOpenReferedStringList;
      frmDissectCode.btnStart.click;
    end;
  end else
  begin
    if frmReferencedStrings=nil then
      frmReferencedStrings:=tfrmReferencedStrings.Create(self);

    frmReferencedStrings.Show;
  end;
end;


function TMemoryBrowser.GetReturnaddress: ptrUint;
var
  haserror: boolean;
  stack: array [0..1023] of ptrUint;
  x: ptrUint;
  i: integer;
begin

  result:=0;

  //do a stacktrace and find the return address

  if frmstacktrace=nil then
    frmstacktrace:=tfrmstacktrace.create(self);

  if frmStacktrace.ListView1.Items.Count>0 then
  begin
    result:=symhandler.getAddressFromName(frmStacktrace.ListView1.Items[0].SubItems[2], false,haserror);
    if haserror then result:=0;
  end;

  if result=0 then
  begin

    //go through the stack and find a entry that falls in executable memory
    ReadProcessMemory(processhandle, pointer(contexthandler.StackPointerRegister^.getValue(context)), @stack[0], 4096, x);
    for i:=0 to (x div processhandler.pointersize) do
    begin
      if symhandler.inModule(stack[i]) and isExecutableAddress(stack[i]) then
      begin
        result:=stack[i]; //best guess, it's an address specifier, it falls inside a module, and it's executable
        exit;
      end;
    end;
  end;
end;

procedure TMemoryBrowser.miDebugExecuteTillReturnClick(Sender: TObject);
var x: ptrUint;
begin
  begin
    x:=getreturnaddress;
    if x>0 then
    begin
      disassemblerview.SelectedAddress:=x;
      miDebugRunTill.Click;
    end else beep; //not possible
  end;
end;

procedure TMemoryBrowser.lvStacktraceDataData(Sender: TObject; Item: TListItem);
var
  x: dword;
  value: ptrUint;
  a: ptrUint;
  address,bytes,details: string;
  v: TVariableType;
  pref: string;

  offset: ptrint;
  offsetstring: string;

  refname: string;
begin

  if stacktrace2.checked then
  begin

    //show frmstacktrace
    if frmStacktrace=nil then
    begin
      frmstacktrace:=TfrmStacktrace.Create(self); //should never happen
    end;

    if item.Index<frmStacktrace.ListView1.Items.Count then
    begin
      item.data:=frmStacktrace.ListView1.Items[item.index].Data;
      item.Caption:=frmStacktrace.ListView1.Items[item.index].SubItems[2]; //returnaddress
      item.SubItems.Add(frmStacktrace.ListView1.Items[item.index].SubItems[3]); //subitems address
    end;
  end
  else
  if all1.checked then
  begin
    //show for each dword what it is


    a:=contexthandler.StackPointerRegister^.getValue(context)+item.Index*processhandler.pointersize;

    if laststack<>nil then
    begin
      if processhandler.is64bit then
      begin
        pref:='r';
        value:=ptruint(pqword(ptruint(laststack)+item.Index*processhandler.pointersize)^);
        item.SubItems.Add(inttohex(value,16));
      end
      else
      begin
        pref:='e';
        value:=ptruint(pdword(ptruint(laststack)+item.Index*processhandler.pointersize)^);
        item.SubItems.Add(inttohex(value,8));
      end;

      if miAddESP.checked then
        offsetstring:='('+pref+'sp+'+inttohex(item.Index*processhandler.pointersize,1)+')'
      else
      begin


        if miAddRef.checked then
        begin
          offset:=a-StackReference;
          refname:=' ';
        end
        else
        begin
          if processhandler.SystemArchitecture=archX86 then
          begin
            offset:=a-PCONTEXT(context)^.{$ifdef cpu64}rbp{$else}Ebp{$endif};
            refname:=pref+'bp';
          end
          else
          begin
            offset:=a-StackReference;
            refname:=' ';
          end;

        end;

        if offset<0 then
          offsetstring:='('+refname+'-'+inttohex(-offset,1)+')'
        else
          offsetstring:='('+refname+'+'+inttohex(offset,1)+')';
      end;


      item.Caption:=inttohex(a,8)+offsetstring;





      v:=FindTypeOfData(a,pointer(ptruint(laststack)+item.Index*processhandler.pointersize),StacktraceSize-item.Index*processhandler.pointersize);
      if v in [vtbyte..vtQword] then
      begin
        //override into pointersize type
        if processhandler.is64bit then
          v:=vtQword
        else
          v:=vtDword;
      end;

      item.SubItems.Add(DataToString(pbytearray(ptruint(laststack)+item.Index*processhandler.pointersize),processhandler.pointersize, v));
    end;
  end else
  begin
    //show strace

    if strace<>nil then
    begin
      if item.index<strace.count then
      begin
        seperatestacktraceline(strace[item.index], address,bytes,details);
        item.Caption:=address;
        item.SubItems.Add(bytes);
        item.SubItems.Add(details);
      end;
    end;
  end;
end;

procedure TMemoryBrowser.lvStacktraceDataDblClick(Sender: TObject);
var
  hasError: boolean;
  x: ptrUint;

  column : integer;
  cursorpos: tpoint;
  tvrect: trect;
  i: integer;

  currentleft: integer;
  s: string;
  ksh: TShiftState;

  pointed: TListItem;
  mp: tpoint;
begin
  mp:=mouse.CursorPos;
  mp:=lvStacktraceData.ScreenToControl(mp);

  pointed:=lvStacktraceData.GetItemAt(mp.x,mp.y);

  if pointed=nil then
    pointed:=lvStacktraceData.Selected;

  if pointed=nil then exit;

  if stacktrace2.checked then
  begin
    //go to the selected address

    disassemblerview.SelectedAddress:=ptruint(pointed.Data);
  end
  else
  begin
    //depending on what column is selected go to the disassembler/hexview part
    cursorpos:=mp;

    column:=0;

    currentleft:=0;
    for i:=0 to lvStacktraceData.Columns.count-1 do
    begin
      if (cursorpos.X>currentleft) and (cursorpos.X<(currentleft+lvStacktraceData.Columns[i].width)) then
      begin
        column:=i;
        break;
      end;
      inc(currentleft, lvStacktraceData.Columns[i].width);
    end;


    if column=0 then
    begin
      s:=pointed.Caption;
      i:=pos('(', s);
      if i>0 then
        s:=copy(s,1,i-1);

    end
    else
      s:=pointed.SubItems[column-1];

    x:=symhandler.getAddressFromName(s,false,haserror);
    if not haserror then
    begin
      ksh:=GetKeyShiftState;
      if ssShift in ksh then
      begin
        disassemblerview.SelectedAddress:=x
      end
      else
      if ssCtrl in ksh then
      begin
        hexview.AddToBackList(pointer(memoryaddress));
        memoryaddress:=x;
      end
      else
      if isExecutableAddress(x) then
      begin
        disassemblerview.SelectedAddress:=x
      end
      else
      begin
        hexview.AddToBackList(pointer(memoryaddress));
        memoryaddress:=x;
      end;
    end;

  end;
end;

procedure TMemoryBrowser.setHexviewAddress(a: ptrUint);
begin
  if hexview<>nil then
    hexview.address:=a;
end;

function TMemoryBrowser.getHexviewAddress:ptrUint;
begin
  if hexview<>nil then
    result:=hexview.address
  else
    result:=0;
end;

procedure TMemoryBrowser.OnMemoryViewerRunning;
begin
  {Disable debug functions & toolbar}
  //tbDebug.enabled:=false; //disable toolbar
  tbRun.Enabled:=false; //disable toolbar run button
  tbStepInto.Enabled:=false; //disable toolbar step into button
  tbStepOver.Enabled:=false; //disable toolbar step over button
  tbStepOut.Enabled:=false; //disable toolbar step out button
  tbRunTill.Enabled:=false;
  miDebugRun.Enabled:=false;
  miRunUnhandled.Enabled:=false;
  miRunUnhandled.Visible:=false;
  miDebugStep.Enabled:=false;
  miDebugStepOver.Enabled:=false;
  miDebugRunTill.Enabled:=false;
  miDebugSetAddress.enabled:=false;
  stacktrace1.Enabled:=false;
  miDebugExecuteTillReturn.Enabled:=false;

  ApplySourceCodeDebugUpdate;
  {Other tasks}
  //...
end;


procedure setControlFontKeepColor(c: TWinControl; f: TFont);
var
  i: integer;
  oldc: tcolor;
  tc: TControl;
  tw: TWinControl absolute tc;
begin
  oldc:=c.font.color;
  c.Font.assign(f);
  c.font.color:=oldc;

  for i:=0 to c.ControlCount-1 do
  begin
    tc:=c.Controls[i];
    if (tc is TWinControl) and (tw.Controlcount>0) then
      setControlFontKeepColor(tw,f);

    if tc is TControl then
    begin
      oldc:=tc.font.color;
      tc.Font.assign(f);
      tc.font.color:=oldc;
    end;
  end;
end;

procedure TMemoryBrowser.setRegisterPanelFont(f: TFont);
begin
  registerpanelfont.Assign(f);
  scrollbox1.ControlCount;
  setControlFontKeepColor(scrollbox1,f);
end;


procedure TMemoryBrowser.UpdateDebugContext(threadhandle: THandle; threadid: dword; changeselection: boolean=true; _debuggerthread: TDebuggerThread=nil);
var temp: string='';
    temp2: string;
    Regstart: string='';
    charcount: integer=8;
    bs: dword;
    x: ptrUint;
    stackaddress: PtrUInt;
    i: integer=0;
    a: ptruint;
    a64: int64;
    d: TDisassembler;

    params: string;
    accessedreglist: tstringlist=nil;

    oldcontexthandler: TContextInfo;

    l: TLabel;
    gprlist, flaglist, speciallist: PContextElementRegisterList;

    cer: PContextElement_register;

    iptlog: pointer;
    iptlogsize: integer;
begin
  if _debuggerthread<>nil then debuggerthread.execlocation:=41301;


  oldcontexthandler:=contexthandler;
  contexthandler:=getBestContextHandler;

  if processhandler.SystemArchitecture=archX86 then
  begin
    a:=contexthandler.InstructionPointerRegister^.getValue(context);
    d:=TDisassembler.create;

    d.disassemble(a, temp);
    params:=d.LastDisassembleData.parameters;
    accessedreglist:=tstringlist.create;
    accessedreglist.Sorted:=true;
    accessedreglist.Duplicates:=dupIgnore;
    getRegisterListFromParams(params, accessedreglist);  //todo: get more data when disassembling

    d.free;
    d:=nil;
  end;

  if frmThreadlist<>nil then
    frmThreadlist.FillThreadlist;

  if _debuggerthread<>nil then _debuggerthread.execlocation:=41302;



  if contexthandler<>oldcontexthandler then
  begin
    //rebuild the registerlist
    self.BeginFormUpdate;
    try
      while pnlGeneralRegisters.ControlCount>0 do
        pnlGeneralRegisters.Controls[0].Free;

      while pnlFlags.ControlCount>0 do
        pnlFlags.Controls[0].Free;

      while pnlExtraRegisters.ControlCount>0 do
        pnlExtraRegisters.Controls[0].Free;

      gprlist:=contexthandler.getGeneralPurposeRegisters;
      if gprlist<>nil then
      begin
        lblRegisters.visible:=true;
        sRegisters.visible:=true;
        pnlGeneralRegisters.visible:=true;

        for i:=0 to length(gprlist^)-1 do
        begin
          l:=tlabel.create(self);
          l.Caption:=gprlist^[i].name;
          l.parent:=pnlGeneralRegisters;
          l.Cursor:=crHandPoint;
          l.Tag:=ptruint(@gprlist^[i]);
          l.PopupMenu:=pmRegisters;
          l.onclick:=RegisterLabelDblClick;
          l.OnMouseDown:=RegisterMouseDown;
        end;
      end
      else
      begin
        lblRegisters.visible:=false;
        sRegisters.visible:=false;
        pnlGeneralRegisters.visible:=false;
      end;

      flaglist:=contexthandler.getGeneralPurposeFlags;
      if flaglist<>nil then
      begin
        lblFlags.visible:=true;
        sflags.visible:=true;
        pnlFlags.visible:=true;

        for i:=0 to length(flaglist^)-1 do
        begin
          l:=tlabel.create(self);
          l.Caption:=flaglist^[i].name;
          l.parent:=pnlFlags;
          l.Cursor:=crHandPoint;
          l.Tag:=ptruint(@flaglist^[i]);
          l.PopupMenu:=pmRegisters;
          l.onclick:=RegisterLabelDblClick;
          l.OnMouseDown:=RegisterMouseDown;
        end;
      end
      else
      begin
        lblFlags.visible:=false;
        sflags.visible:=false;
        pnlFlags.visible:=false;
      end;

      speciallist:=contexthandler.getSpecializedRegisters;
      if speciallist<>nil then
      begin
        lblSpecial.visible:=true;
        sSpecial.visible:=true;
        pnlExtraRegisters.visible:=true;

        for i:=0 to length(speciallist^)-1 do
        begin
          l:=tlabel.create(self);
          l.Caption:=flaglist^[i].name;
          l.parent:=pnlExtraRegisters;
          l.Cursor:=crHandPoint;
          l.Tag:=ptruint(@speciallist^[i]);
          l.PopupMenu:=pmRegisters;
          l.onclick:=RegisterLabelDblClick;
          l.OnMouseDown:=RegisterMouseDown;
        end;
      end
      else
      begin
        lblSpecial.visible:=false;
        sSpecial.visible:=false;
        pnlExtraRegisters.visible:=false;
      end;

    finally
      EndFormUpdate;
    end;

  end;

  if _debuggerthread<>nil then _debuggerthread.execlocation:=41303;


  if (accessedreglist<>nil) then
  begin
    for i:=0 to pnlGeneralRegisters.ControlCount-1 do
    begin
      l:=tlabel(pnlGeneralRegisters.Controls[i]);
      if accessedreglist.IndexOf(PContextElement_register(l.Tag).name)>=0 then l.color:=fAccessedRegisterColor else l.color:=clNone;
    end;

  end;

  if _debuggerthread<>nil then _debuggerthread.execlocation:=41304;

  //tbDebug.enabled:=true;
  if (tbDebug.visible=false) and (tbDebug.Tag<>-1) then
    ShowDebugToolbar; //show toolbar

  miDebugRun.Enabled:=true;
  tbRun.Enabled:=true; //enable toolbar run button

  if debuggerthread=nil then exit;


  miRunUnhandled.Enabled:=(debuggerthread.CurrentThread<>nil) and debuggerthread.CurrentThread.isUnhandledException;

  if _debuggerthread<>nil then _debuggerthread.execlocation:=41305;
  miRunUnhandled.Visible:=miRunUnhandled.Enabled;
  tbRunUnhandled.enabled:=miRunUnhandled.enabled;
  tbRunUnhandled.visible:=miRunUnhandled.enabled;

  miDebugStep.Enabled:=true;
  tbStepInto.Enabled:=true; //enable toolbar step into button
  miDebugStepOver.Enabled:=true;
  tbStepOver.Enabled:=true; //enable toolbar step over button
  miDebugRunTill.Enabled:=true;
  tbRunTill.Enabled:=true;
  tbStepOut.Enabled:=true; //enable toolbar step out button
  miDebugSetAddress.enabled:=true;
  stacktrace1.Enabled:=true;
  miDebugExecuteTillReturn.Enabled:=true;




  if threadid<>0 then
    caption:=Format(rsMemoryViewerCurrentlyDebuggingThread, [inttohex(threadid, 1)]);

  if (debuggerthread.CurrentThread<>nil) and debuggerthread.CurrentThread.isUnhandledException then
    caption:=caption+' '+format(rsBecauseOfUnhandledExeption, [ExceptionCodeToString(debuggerthread.CurrentThread.lastUnhandledExceptionCode)]);

  if (frmstacktrace<>nil) then
  begin
    if (threadhandle=0) and (debuggerthread<>nil) and (debuggerthread.CurrentThread<>nil) then
      threadhandle:=debuggerthread.CurrentThread.handle;

    frmstacktrace.stacktrace(threadhandle, context);
  end;

  if changeselection then
  begin
    disassemblerview.SelectedAddress:=contexthandler.InstructionPointerRegister^.getValue(context);
  end;


  //apply the values
  if pnlGeneralRegisters.visible then
  begin
    for i:=0 to pnlGeneralRegisters.ControlCount-1 do
    begin
      l:=tlabel(pnlGeneralRegisters.controls[i]);
      cer:=PContextElement_register(l.tag);

      temp:=padleft(cer^.name, contexthandler.GeneralPurposeRegisterMaxCharCount)+' '+cer^.getFullValueString(context);
      if temp<>l.caption then
      begin
        l.Font.Color:=fChangedRegisterColor;
        l.Caption:=temp;
      end
      else
        l.Font.Color:=clWindowText;
    end;
  end;

  if pnlFlags.visible then
  begin
    for i:=0 to pnlFlags.ControlCount-1 do
    begin
      l:=tlabel(pnlFlags.controls[i]);
      cer:=PContextElement_register(l.tag);
      temp:=padleft(cer^.name, contexthandler.GeneralPurposeFlagMaxCharCount)+' '+cer^.getFlag(context).ToString;
      if temp<>l.caption then
      begin
        l.Font.Color:=fChangedRegisterColor;
        l.Caption:=temp;
      end
      else
        l.Font.Color:=clWindowText;
    end;
  end;

  if pnlExtraRegisters.Visible then
  begin
    for i:=0 to pnlExtraRegisters.ControlCount-1 do
    begin
      l:=tlabel(pnlExtraRegisters.controls[i]);
      cer:=PContextElement_register(l.tag);
      temp:=padleft(cer^.name, contexthandler.GeneralPurposeRegisterMaxCharCount)+' '+cer^.getValueString(context);
      if temp<>l.caption then
      begin
        l.Font.Color:=fChangedRegisterColor;
        l.Caption:=temp;
      end
      else
        l.Font.Color:=clWindowText;
    end;
  end;

  if _debuggerthread<>nil then _debuggerthread.execlocation:=41308;

  sbShowFloats.BringToFront;
  //sbShowFloats.visible:=true;

  if not registerview.visible then
  begin
    setShowDebugPanels(true);

    registerview.ClientWidth:=lblFlags.left+lblFlags.width+16+scrollbox1.VertScrollBar.Size;

    scrollbox1.HorzScrollBar.Visible:=false;




    scrollbox1.Invalidate;
  end;

  if _debuggerthread<>nil then _debuggerthread.execlocation:=41309;

  if laststack=nil then
    getmem(laststack,stacktraceSize+64);


  //get a stackview
  i:=0;
  stackaddress:=contexthandler.StackPointerRegister^.getValue(context);
  while i<stacktracesize do
  begin
    bs:=4096-((stackaddress+i) mod 4096);
    bs:=min(stacktraceSize-i, bs);


    readprocessmemory(processhandle, pointer(stackaddress+i), pointer(ptruint(laststack)+i), bs, x);

    inc(i,bs);
  end;

  if _debuggerthread<>nil then _debuggerthread.execlocation:=41310;

  reloadStacktrace;

  if _debuggerthread<>nil then _debuggerthread.execlocation:=41311;

  if frmFloatingPointPanel<>nil then
    frmFloatingPointPanel.SetContextPointer(context);


  if not memorybrowser.Visible then
    memorybrowser.show;

  if _debuggerthread<>nil then _debuggerthread.execlocation:=41312;

  if (frmWatchlist<>nil) and (frmWatchlist.Visible) then
    frmWatchlist.UpdateContext(context);

  if accessedreglist<>nil then
    freeandnil(accessedreglist);

  if _debuggerthread<>nil then _debuggerthread.execlocation:=41313;


  {$IFDEF WINDOWS}
  if (debuggerthread<>nil) and debuggerthread.usingIPT and (frmiptlog<>nil) and (frmiptlog.visible) and (debuggerthread.CurrentThread<>nil) then
  begin
    debuggerthread.CurrentThread.getLastIPTLog(iptlog,iptlogsize);
    frmiptlog.loadlog('',iptlog, iptlogsize);
    freemem(iptlog);
  end;
  {$ENDIF}


  registerview.OnResize(registerview);


  ApplyFollowRegister;
  ApplySourceCodeDebugUpdate;
end;



initialization
  MemoryBrowsers:=TList.Create;

  {$i MemoryBrowserFormUnit.lrs}

finalization
  if MemoryBrowsers<>nil then
    FreeAndNil(MemoryBrowsers);


end.
