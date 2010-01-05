unit MemoryBrowserFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,tlhelp32, frmMemoryAllocHandlerUnit,
  math, StdCtrls, Spin, ExtCtrls,CEFuncProc,symbolhandler,Clipbrd, Menus,plugin,debugger,kerneldebugger, assemblerunit,disassembler,addressparser,
  Buttons,imagehlp, Contnrs, disassemblerviewunit, peinfofunctions ,dissectcodethread
  ,stacktrace2, NewKernelHandler, ComCtrls,FormsExtra, frmCScriptUnit , byteinterpreter, StrUtils;


type
  TEdit2= class( TEdit)
  private
  public
    procedure wmMouseWheel (var Msg : TWMMouseWheel); message wm_MouseWheel;

  end;

  TDisplayType = (dtByte, dtWord, dtDword, dtDwordDec, dtSingle, dtDouble);

  TMemoryBrowser = class(TForm)
    memorypopup: TPopupMenu;
    Goto1: TMenuItem;
    debuggerpopup: TPopupMenu;
    Timer2: TTimer;
    Panel1: TPanel;
    Panel4: TPanel;
    Replacewithnops1: TMenuItem;
    Gotoaddress1: TMenuItem;
    Search1: TMenuItem;
    Change1: TMenuItem;
    Addthisaddresstothelist1: TMenuItem;
    Addthisopcodetothecodelist1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Splitter1: TSplitter;
    Panel5: TPanel;
    RegisterView: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Loadsymbolfile1: TMenuItem;
    Debug1: TMenuItem;
    Step1: TMenuItem;
    StepOver1: TMenuItem;
    Runtill1: TMenuItem;
    Setbreakpoint1: TMenuItem;
    View1: TMenuItem;
    Stacktrace1: TMenuItem;
    ScrollBox1: TScrollBox;
    EAXLabel: TLabel;
    EBXlabel: TLabel;
    ECXlabel: TLabel;
    EDXlabel: TLabel;
    ESIlabel: TLabel;
    EDIlabel: TLabel;
    EBPlabel: TLabel;
    ESPlabel: TLabel;
    EIPlabel: TLabel;
    CSLabel: TLabel;
    DSLabel: TLabel;
    SSlabel: TLabel;
    ESlabel: TLabel;
    FSlabel: TLabel;
    GSlabel: TLabel;
    cflabel: TLabel;
    pflabel: TLabel;
    aflabel: TLabel;
    zflabel: TLabel;
    sflabel: TLabel;
    oflabel: TLabel;
    Label14: TLabel;
    Shape1: TShape;
    Label15: TLabel;
    Shape2: TShape;
    Label16: TLabel;
    Shape3: TShape;
    Run1: TMenuItem;
    Threadlist1: TMenuItem;
    Assemble1: TMenuItem;
    N3: TMenuItem;
    Break1: TMenuItem;
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
    Createjumptocodecave1: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Findstaticpointers1: TMenuItem;
    Scanforcodecaves1: TMenuItem;
    Changestateofregisteratthislocation1: TMenuItem;
    ogglebreakpoint1: TMenuItem;
    N9: TMenuItem;
    Breakpointlist1: TMenuItem;
    Makepagewritable1: TMenuItem;
    Dissectdata1: TMenuItem;
    N10: TMenuItem;
    Showsymbols1: TMenuItem;
    Dissectdata2: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    Showmoduleaddresses1: TMenuItem;
    Symbolhandler1: TMenuItem;
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
    Kernelmodesymbols1: TMenuItem;
    Breakandtraceinstructions1: TMenuItem;
    GDTlist1: TMenuItem;
    IDTlist1: TMenuItem;
    ScriptEngine1: TMenuItem;
    Newwindow1: TMenuItem;
    Follow1: TMenuItem;
    dflabel: TLabel;
    Copytoclipboard1: TMenuItem;
    copyBytes: TMenuItem;
    copyOpcodes: TMenuItem;
    CopyBytesAndOpcodes: TMenuItem;
    DissectPEheaders1: TMenuItem;
    Back1: TMenuItem;
    Showvaluesofstaticaddresses1: TMenuItem;
    Findoutwhataddressesthisinstructionaccesses1: TMenuItem;
    ScriptConsole1: TMenuItem;
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
    Panel2: TPanel;
    Protectlabel: TLabel;
    MBCanvas: TPaintBox;
    HexEdit: TEdit;
    TextEdit: TEdit;
    ScrollBar2: TScrollBar;
    Splitter3: TSplitter;
    pnlStacktrace: TPanel;
    sbShowFloats: TSpeedButton;
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
    Executetillreturn1: TMenuItem;
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Goto1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MemoryLabelClick(Sender: TObject);
    procedure MBCanvasPaint(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure MBCanvasMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MBCanvasMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MBCanvasDblClick(Sender: TObject);
    procedure memorypopupPopup(Sender: TObject);
    procedure Replacewithnops1Click(Sender: TObject);
    procedure FControl2KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FControl2KeyPress(Sender: TObject; var Key: Char);
    procedure FControl2Enter(Sender: TObject);
    procedure FControl2Exit(Sender: TObject);
    procedure FControl1Exit(Sender: TObject);

    procedure FControl1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FControl1KeyPress(Sender: TObject; var Key: Char);
    procedure Gotoaddress1Click(Sender: TObject);
    procedure Search1Click(Sender: TObject);
    procedure Change1Click(Sender: TObject);
    procedure Addthisaddresstothelist1Click(Sender: TObject);
    procedure Addthisopcodetothecodelist1Click(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure ScrollBar2Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Run1Click(Sender: TObject);
    procedure Step1Click(Sender: TObject);
    procedure StepOver1Click(Sender: TObject);
    procedure Runtill1Click(Sender: TObject);
    procedure Stacktrace1Click(Sender: TObject);
    procedure Threadlist1Click(Sender: TObject);
    procedure Assemble1Click(Sender: TObject);
    procedure HexEditKeyPress(Sender: TObject; var Key: Char);
    procedure HexEditExit(Sender: TObject);
    procedure HexEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EAXLabelDblClick(Sender: TObject);
    procedure Break1Click(Sender: TObject);
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
    procedure ogglebreakpoint1Click(Sender: TObject);
    procedure Breakpointlist1Click(Sender: TObject);
    procedure Makepagewritable1Click(Sender: TObject);
    procedure Dissectdata1Click(Sender: TObject);
    procedure Showsymbols1Click(Sender: TObject);
    procedure Dissectdata2Click(Sender: TObject);
    procedure Showmoduleaddresses1Click(Sender: TObject);
    procedure Symbolhandler1Click(Sender: TObject);
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
    procedure Kernelmodesymbols1Click(Sender: TObject);
    procedure Breakandtraceinstructions1Click(Sender: TObject);
    procedure debuggerpopupPopup(Sender: TObject);
    procedure GDTlist1Click(Sender: TObject);
    procedure IDTlist1Click(Sender: TObject);
    procedure ScriptEngine1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Newwindow1Click(Sender: TObject);
    procedure Follow1Click(Sender: TObject);
    procedure CopyBytesAndOpcodesClick(Sender: TObject);
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
    procedure Watchmemoryallocations1Click(Sender: TObject);
    procedure Continueanddetachdebugger1Click(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure Panel2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrollBox1Resize(Sender: TObject);
    procedure Maxstacktracesize1Click(Sender: TObject);
    procedure All1Click(Sender: TObject);
    procedure Modulesonly1Click(Sender: TObject);
    procedure Nonsystemmodulesonly1Click(Sender: TObject);
    procedure Referencedstrings1Click(Sender: TObject);
    procedure stacktrace2Click(Sender: TObject);
    procedure Executetillreturn1Click(Sender: TObject);
    procedure lvStacktraceDataData(Sender: TObject; Item: TListItem);
  private
    { Private declarations }
    posloadedfromreg: boolean;
    displaytype: TDisplayType;


    editing: boolean;
    editing2: boolean;

    srow,scolumn: integer;

    bytelength: integer;
    chrlength: integer;

    MBImage: TBitmap;
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
    textheight: integer;


    lines: integer;
    oldlines: integer;
    Highlightcolor: Tcolor;

    numberofaddresses: integer;


    part: integer;

    {$ifndef net}

    {$endif}


    lastmodulelistupdate: integer;

    disassemblerHistory: TStringList;
    memorybrowserHistory: TStringList;
    assemblerHistory: TStringList;

    backlist: TStack;

    lastspecialwidth: integer;
    FShowValues: boolean;
    FShowDebugPanels: boolean;
    FStacktraceSize: integer;

    strace: Tstringlist;

    procedure SetStacktraceSize(size: integer);
    procedure setShowDebugPanels(state: boolean);
    procedure UpdateRWAddress(disasm: string);
    procedure WMGetMinMaxInfo(var Message: TMessage); message WM_GETMINMAXINFO;
    function getShowValues: boolean;
    procedure setShowValues(newstate: boolean);


    procedure disassemblerviewDblClick(Sender: TObject);
  public
    { Public declarations }
    FSymbolsLoaded: Boolean;
    memoryaddress: dword;
    thhandle: Thandle;

    disassemblerview: TDisassemblerview;

    lastdebugcontext: _Context;
    //EAXv: dword;
    //EBXv: dword;
    //ECXv: dword;
    //EDXv: dword;
    //ESIv: dword;
    //EDIv: dword;
    //EBPv: dword;
    //ESPv: dword;
    //EIPv: dword;
    FControl2:Tedit2;
    rows8: integer;
    disassembler: boolean;
    selecting,selectionmade: boolean;
    selected,selected2: dword;
    cancelsearch: boolean;

    ischild: boolean; //determines if it's the main memorybrowser or a child

    procedure FindwhatThiscodeAccesses(address: dword);
    procedure UpdateBPlist;
    procedure UpdateRegisterview;
    procedure RefreshMB;
    procedure AssemblePopup(x: string);

    procedure plugintype1click(sender:tobject);
    procedure plugintype6click(sender:tobject);
    procedure setcodeanddatabase;
    property showvalues: boolean read getShowValues write setShowValues;
    property showDebugPanels: boolean read fShowDebugPanels write setShowDebugPanels;
    property stacktraceSize: integer read FStacktraceSize write SetStacktraceSize;
    procedure reloadStacktrace;
    function GetReturnaddress: dword;
  end;

var
  MemoryBrowser: TMemoryBrowser;
  mbchildcount: integer; //global so all other children can increase it as well

  
implementation

uses Valuechange,
  {$ifdef net}
  unit2,
  addformunit,
  {$else}
  Mainunit,
  {$endif}

  {$ifndef net}
  AddAddress,
  findwindowunit,
  frmstacktraceunit,
  frmBreakThreadUnit,
  FormDebugStringsUnit,
  frmDissectWindowUnit,
  frmEnumerateDLLsUnit,
  frmThreadlistunit,
  formmemoryregionsunit,
  frmHeapsUnit,
  frmFindstaticsUnit,
  frmModifyRegistersUnit,
  frmBreakpointlistunit,
  savedisassemblyfrm,
  {$endif}
  advancedoptionsunit,
  frmautoinjectunit,
  formsettingsunit,
  frmSaveMemoryRegionUnit,
  frmLoadMemoryunit,
  inputboxtopunit,
  formAddToCodeList,
  frmFillMemoryUnit,
  frmCodecaveScannerUnit,
  FoundCodeUnit,
  frmFunctionListUnit,




  {$ifndef net}symbolconfigunit,frmTracerUnit,Structuresfrm,dissectcodeunit,pointerscannerfrm,driverlist,ServiceDescriptorTables,{$endif}
  frmDisassemblyscanunit, frmGDTunit, frmIDTunit, peINFOunit,
  formChangedAddresses, frmFloatingPointPanelUnit,
  frmReferencedStringsUnit;



{$R *.DFM}


procedure TEdit2.wmMouseWheel (var Msg : TWMMouseWheel);
begin
  with (parent as TMemorybrowser) do
  begin
    if msg.WheelDelta>0 then
      memoryaddress:=memoryaddress-(8*rows8*4)
    else
      memoryaddress:=memoryaddress+(8*rows8*4);

    refreshMB;
  end;
end;

//property functions:
function TMemoryBrowser.getShowValues: boolean;
begin
  result:=FShowValues;
end;

procedure TMemoryBrowser.setShowValues(newstate: boolean);
begin
  Showvaluesofstaticaddresses1.checked:=newstate;
  FShowValues:=newstate;
  disassemblerview.setCommentsTab(FShowValues);
end;

procedure TMemoryBrowser.setShowDebugPanels(state: boolean);
begin
  FShowDebugPanels:=state;
  registerview.Visible:=state;
  pnlStacktrace.Visible:=state;
  splitter2.Visible:=state;
  splitter3.Visible:=state;
end;

procedure TMemoryBrowser.SetStacktraceSize(size: integer);
begin
  FStacktraceSize:=size;
  reloadStacktrace;
end;

//^^^^


procedure TMemoryBrowser.WMGetMinMaxInfo(var Message: TMessage);
var MMInfo: ^MINMAXINFO;
begin
  if panel1.visible then
  begin
    MMInfo:=ptr(message.LParam);
    MMInfo.ptMinTrackSize:=point(340,panel1.Height+100);
  end
  else
  begin
    MMInfo:=ptr(message.LParam);
    MMInfo.ptMinTrackSize:=point(340,100);
  end;
end;

procedure TMemoryBrowser.UpdateBPlist;
begin
  {$ifndef net}
  if frmBreakpointlist<>nil then
    frmBreakpointlist.updatebplist;
  {$endif}
end;


procedure TMemoryBrowser.UpdateRegisterview;
begin
//removed till 3.3
end;



procedure TMemoryBrowser.UpdateRWAddress(disasm: string);
var seperator: integer;
    fb: integer;
    nb: integer;
    address: string;
    offset:dword;
begin
  //temporaryily obsolete
  
{  seperator:=pos(',',disasm);
  if seperator>0 then
  begin
    fb:=pos('[',disasm);
    nb:=pos(']',disasm);

    if nb>fb then
    begin
      //if fb<seperator then label1.Font.Color:=clRed //write
      //                else label1.font.color:=clGreen; //read
      address:=copy(disasm,fb+1,nb-fb-1);

      try
        offset:=getaddress(address);
      except

      end;
      //label1.Caption:=IntToHex(offset,8);
    end;
  end; }
end;

procedure TMemoryBrowser.Button4Click(Sender: TObject);
begin
  {$ifndef net}
  debuggerthread.howtocontinue:=1;
  debuggerthread.stepping:=false;
  memorybrowser.Caption:='Advanced - Debug:Stepping';
  {$endif}
end;

procedure TMemoryBrowser.Button2Click(Sender: TObject);
begin
  {$ifndef net}
  debuggerthread.howtocontinue:=0;
  debuggerthread.stepping:=false;
  memorybrowser.Caption:='Advanced - Debug:Running';
  {$endif}
end;

procedure TMemoryBrowser.Splitter1Moved(Sender: TObject);
begin
  disassemblerview.Update;
end;

procedure TMemoryBrowser.FormShow(Sender: TObject);
var x: array of integer;

begin
  disassemblerview.Update;;
  mbimage.Width:=0;  //clear the image
  mbimage.Width:=clientwidth;

  RefreshMB;

  {$ifdef net}
  registerview.Visible:=false;
  stacktrace1.Visible:=false;
  breakpointlist1.Visible:=false;
  threadlist1.Visible:=false;
  debugstrings1.Visible:=false;
  n5.Visible:=false;
  memoryregions1.Visible:=false;
  heaps1.Visible:=false;
  n6.Visible:=false;
  enumeratedllsandsymbols1.Visible:=false;
  n10.visible:=false;
  showsymbols1.Visible:=false;
  showmoduleaddresses1.Visible:=false;
  symbolhandler1.Visible:=false;
  debug1.Visible:=false;
  view1.Visible:=false;

  scanforcodecaves1.Visible:=false;
  reservememory1.Visible:=false;
  fillmemory1.Visible:=false;
  createthread1.Visible:=false;
  n8.visible:=false;
  injectdll1.visible:=false;

  search1.Visible:=false;
  Changestateofregisteratthislocation1.Visible:=false;
  ogglebreakpoint1.Visible:=false;
  n9.Visible:=false;

  dissectcode1.Visible:=false;
  dissectdata2.visible:=false;
  disectwindow1.visible:=false;

  n12.visible:=false;
  dissectdata1.Visible:=false;
  findstaticpointers1.Visible:=false;
  n11.Visible:=true;
  injectdll1.Visible:=false;
  autoinject1.Visible:=true;
  {$endif}



  Sericedescriptortable1.visible:=not Is64bitOS;
  GDTlist1.Visible:=not is64bitos;
  IDTlist1.Visible:=not is64bitos;


end;

procedure TMemoryBrowser.disassemblerviewDblClick(Sender: TObject);
begin
  assemble1.Click;
end;

procedure TMemoryBrowser.FormCreate(Sender: TObject);
var x: array of integer;
begin
  displaytype:=dtByte;
  scriptconsole1.ShortCut:=TextToShortCut('Ctrl+Shift+C');

  strace:=tstringlist.create;


{
not enough time to add header supports


}
 { disassemblerheader.Visible:=false;
  discanvas.Top:=discanvas.top-disassemblerheader.Height;
  discanvas.Height:=discanvas.Height+disassemblerheader.Height;
  //ronresize isn't repainting correctly   }
{^^^^}

  disassembler:=true;

  disassemblerview:=TDisassemblerview.Create(self);
  disassemblerview.Align:=alClient;
  disassemblerview.Parent:=panel5;
  disassemblerview.PopupMenu:=debuggerpopup;
  disassemblerview.OnKeyDown:=FControl1keydown;
  disassemblerview.OnDblClick:=disassemblerviewDblClick;

  disassemblerview.TopAddress:=$00400000;
    

  fcontrol2:=tedit2.create(self);
  fcontrol2.Width:=0;
  fcontrol2.Height:=0;
  fcontrol2.parent:=self;
  fcontrol2.OnEnter:=FControl2Enter;
  fcontrol2.onexit:=FControl2Exit;
  fcontrol2.onKeydown:=FControl2keydown;
  fcontrol2.onkeypress:=FControl2keypress;
  fcontrol2.PopupMenu:=mainform.emptypopup;
  fcontrol2.SendToBack;


  MBImage:=TBitmap.Create;
  MBImage.Canvas.Brush.Color:=clBtnFace;
  MBImage.Width:=mbcanvas.Width*3;
  MBImage.Height:=MBCanvas.Height*3;
  MBImage.Canvas.Font.Name:='Courier';

  textheight:=MBImage.Canvas.TextHeight('||||');
  bytelength:=MBImage.Canvas.TextWidth('   ');
  chrlength:=MBImage.Canvas.TextWidth(' ');

  hexedit.Height:=textheight;
  hexedit.Width:=chrlength*2;
  textedit.Height:=hexedit.Height;
  textedit.Width:=chrlength;

  memoryaddress:=$00400000;
  memorylabelcount:=0;

  Highlightcolor:=clHighlight;

  disassemblerHistory:=TStringList.create;
  memorybrowserHistory:=TStringList.create;
  assemblerHistory:=TStringList.create;

  backlist:=TStack.create;

  showvalues:=true;
  sbShowFloats.left:=scrollbox1.Clientwidth-sbShowFloats.width;

  FStacktraceSize:=4096;
  

  setlength(x, 6);

  if loadformposition(self,x) then
  begin
    disassemblerview.setheaderWidth(0,x[0]);
    disassemblerview.setheaderWidth(1,x[1]);
    disassemblerview.setheaderWidth(2,x[2]);
    disassemblerview.setheaderWidth(3,x[3]);

    panel1.height:=x[4];
    registerview.width:=x[5];

    setlength(x,0);
    posloadedfromreg:=true;
  end;

end;

procedure TMemoryBrowser.Goto1Click(Sender: TObject);
var newaddress: string;
    canceled: boolean;
begin
  panel4.setfocus;
  newaddress:=inputboxtop('Goto Address','Fill in the address you want to go to',IntTohex(memoryaddress,8),true,canceled,memorybrowserHistory);

  memoryaddress:=getaddress(newaddress);

  fcontrol2.SetFocus;

  RefreshMB;
end;

procedure TMemoryBrowser.FormResize(Sender: TObject);
begin
  mbimage.Width:=0;
  mbimage.height:=0;
  mbimage.Width:=mbcanvas.Width;
  mbimage.Height:=mbcanvas.Height;
  refreshmb;
  disassemblerview.Update;;
end;

procedure TMemoryBrowser.MemoryLabelClick(Sender: TObject);
var address: dword;
begin
  address:=memoryaddress+(sender as TLabel).Tag;
  showmessage(inttohex(address,8));

end;

procedure TMemoryBrowser.MBCanvasPaint(Sender: TObject);
var cr: Trect;
begin
  cr:=mbcanvas.Canvas.ClipRect;
  mbcanvas.Canvas.CopyRect(cr,mbimage.Canvas,cr);
end;


procedure TMemoryBrowser.Timer2Timer(Sender: TObject);
begin
  if Visible then
  begin
    refreshMB;
    disassemblerview.Update;

    //refresh the modulelist
    lastmodulelistupdate:=(lastmodulelistupdate+1) mod 10;
    if lastmodulelistupdate=0 then
      symhandler.loadmodulelist;
  end;
end;


procedure TMemoryBrowser.RefreshMB;
var i: integer;
    j,k: integer;
    currentaddress: string[8];
    bts: string[20];

    start: integer;
    stop: integer;
    needed: integer;
    available: integer;
    rowsof8: integer;
    teststr: string;

    p: pchar;
    a: int64;

    range1ok: boolean;
    range1start: dword;
    range1length: dword;

    range2ok: boolean;
    range2start: dword;
    range2length: dword;


    buffer: array of byte;
    bt: byte;
    bytesread: dword;


    rct: Trect;

    mbi : _MEMORY_BASIC_INFORMATION;

    range1module: tmoduleinfo;
    range2module: tmoduleinfo;
    module1ok,module2ok: boolean;
    s: string;
    selstart,selstop: integer;
  procedure getBTSString(unreadable: boolean);
  {
  Because this same code snippet is used in range1 AND range 2 it's betetr to make it a subfunction
  }
  var x: string;
  begin
    bts:='';
    case displayType of
      dtByte:
      begin
        if unreadable then
          bts:='??'
        else
          bts:=IntToHex(buffer[j+(i*8*rowsof8)],2);
      end;

      dtWord:
      begin
        if j mod 2 = 0 then
        begin
          if unreadable then
            bts:='????'
          else
            bts:=IntToHex(pword(@buffer[j+(i*8*rowsof8)])^,4);
            
          while length(bts)<5 do
            bts:=bts+' ';
        end;
      end;

      dtDWord:
      begin
        if j mod 4 = 0 then
        begin
          if unreadable then
            bts:='????????'
          else
            bts:=IntToHex(pdword(@buffer[j+(i*8*rowsof8)])^,8);
            
          while length(bts)<10 do
            bts:=bts+' ';
        end;
      end;

      dtDwordDec:
      begin
        if j mod 4 = 0 then
        begin
          if unreadable then
            bts:='?????'
          else
            bts:=IntToStr(pinteger(@buffer[j+(i*8*rowsof8)])^);

          if length(bts)>9 then
            bts:=copy(bts,1,6)+'...';
            
          while length(bts)<10 do
            bts:=bts+' ';
        end;
      end;

      dtSingle:
      begin
        if j mod 4 = 0 then
        begin
          if unreadable then
            bts:='???'
          else
            bts:=format('%f',[psingle(@buffer[j+(i*8*rowsof8)])^]);
            
          if length(bts)>9 then
            bts:=copy(bts,1,6)+'...';

          while length(bts)<10 do
            bts:=bts+' ';
        end;
      end;

      dtDouble:
      begin
        if j mod 8 = 0 then
        begin
          if unreadable then
            bts:='???'
          else
            bts:=format('%f',[pDouble(@buffer[j+(i*8*rowsof8)])^]);
            
          if length(bts)>18 then
            bts:=copy(bts,1,16)+'...';

          while length(bts)<19 do
            bts:=bts+' ';
        end;
      end;
    end;
  end;
begin
  //find the address in the module list

  if selected<=selected2 then
  begin
    selstart:=selected;
    selstop:=selected2;
  end
  else
  begin
    selstart:=selected2;
    selstop:=selected;
  end;


  k:=0;
  MBCanvas.Canvas.Font:=MBImage.Canvas.Font;

  if length(addressestext)<((mbcanvas.Height) div (TextHeight))+2 then //resync
  begin
    setlength(addressestext,0);
    setlength(addressestext,((mbcanvas.Height) div (TextHeight))+2);
    setlength(memorystring,0);
    setlength(memorystring,((mbcanvas.Height) div (TextHeight))+2);
  end;

  start:=20+mbimage.Canvas.TextWidth('00400000');
  rowsof8:=0;

  needed:=0;

  available:=mbcanvas.Width-(start+20*rowsof8)-mbimage.Canvas.TextWidth('??');

  while available>(needed) do
  begin
    inc(rowsof8);
    teststr:='';
    for i:=1 to rowsof8 do teststr:=teststr+'????????';
    needed:=10+mbcanvas.Canvas.TextWidth(teststr);

    available:=mbcanvas.Width-(start+20*((8*rowsof8)-1))-mbcanvas.Canvas.TextWidth('??');
  end;

  dec(rowsof8);

  if rowsof8=0 then rowsof8:=1;
  
  rows8:=rowsof8;

  if length(memorytext)<8*rowsof8 then
  begin
    setlength(memorytext,0);
    setlength(memorytext,8*rowsof8+1);
  end;

   //fill local array
  setlength(buffer,(((mbcanvas.Height) div (TextHeight))+1)*(8*rowsof8)+257);

  range1start:=memoryaddress;
  range1length:=2048-(range1start mod 2048);
  if range1length>((((mbcanvas.Height) div (TextHeight))+1)*(8*rowsof8)) then range1length:=((((mbcanvas.Height) div (TextHeight))+1)*(8*rowsof8));

 // range1length:=(range1start+(((mbcanvas.Height-5) div (TextHeight))+1)*(8*rowsof8));// mod 2048;

  range2start:=range1start+range1length;
  range2length:=((((mbcanvas.Height) div (TextHeight))+1)*(8*rowsof8))-range1length;

  //get the modules (if they have any)
  module1ok:=symhandler.getmodulebyaddress(range1start,range1module);
  module2ok:=symhandler.getmodulebyaddress(range2start,range2module);


  zeromemory(@buffer[0],length(buffer));
  range1ok:=readprocessmemory(processhandle,pointer(range1start),@buffer[0],range1length,bytesread);
  if range2length>0 then range2ok:=readprocessmemory(processhandle,pointer(range2start),@buffer[range1length],range2length,bytesread) else range2ok:=false;

  for i:=0 to ((mbcanvas.Height) div (TextHeight))+1 do
  begin
    //addresses
    mbcanvas.Canvas.font.Color:=clwindowtext;
    mbimage.Canvas.font.Color:=clwindowtext;

    currentaddress:=IntToHex(dword(memoryaddress+i*8*rowsof8),8);
    mbcanvas.Canvas.TextOut(10,i*TextHeight+2,currentaddress);
    mbimage.Canvas.TextOut(10,i*TextHeight+2,currentaddress);

    if addressestext[i]<>currentaddress then
    begin

      addressestext[i]:=currentaddress;
    end;

    //bytes

    teststr:='';

    p:=pointer(memoryaddress+i*8*rowsof8);
    a:=memoryaddress+i*8*rowsof8;

    j:=0;
    while j<(8*rowsof8) do
    begin
      if a<int64(int64(range1start)+int64(range1length)) then
      begin
        if range1ok then
        begin
          //readable
          if (selecting or selectionmade) and (selstart<>selstop) and ((((memoryaddress+i*8*rowsof8)+j)>=selstart) and (((memoryaddress+i*8*rowsof8)+j)<=selstop)) then
          begin
            mbcanvas.Canvas.font.Color:=clred;
            mbimage.Canvas.font.Color:=clred;
          end
          else
          begin
            if (module1ok) then
            begin
              mbcanvas.Canvas.font.Color:=clgreen;
              mbimage.Canvas.font.Color:=clgreen;
            end
            else
            begin
              mbcanvas.Canvas.font.Color:=clwindowtext;
              mbimage.Canvas.font.Color:=clwindowtext;
            end;
          end;

          GetBTSstring(false);


          mbcanvas.Canvas.TextOut(start+20*j,i*textHeight+2,bts);
          mbimage.Canvas.TextOut(start+20*j,i*textHeight+2,bts);

          if buffer[j+(i*8*rowsof8)]<$20 then
          begin
            mbcanvas.Canvas.TextOut(start+20+20*((8*rowsof8)-1)+j*chrlength,i*textHeight+2,' ');
            mbimage.Canvas.TextOut(start+20+20*((8*rowsof8)-1)+j*chrlength,i*textHeight+2,' ');
          end else
          begin
            mbcanvas.Canvas.TextOut(start+20+20*((8*rowsof8)-1)+j*chrlength,i*textHeight+2,chr(buffer[j+(i*8*rowsof8)]));
            mbimage.Canvas.TextOut(start+20+20*((8*rowsof8)-1)+j*chrlength,i*textHeight+2,chr(buffer[j+(i*8*rowsof8)]));
          end;
        end
        else
        begin
          //unreadable
          if (selecting or selectionmade) and (selstart<>selstop) and ((((memoryaddress+i*8*rowsof8)+j)>=selstart) and (((memoryaddress+i*8*rowsof8)+j)<=selstop)) then
          begin
            mbcanvas.Canvas.font.Color:=clred;
            mbimage.Canvas.font.Color:=clred;
          end
          else
          begin
            mbcanvas.Canvas.font.Color:=clwindowtext;
            mbimage.Canvas.font.Color:=clwindowtext;
          end;

          GetBTSstring(true);
          mbcanvas.Canvas.TextOut(start+20*j,i*textHeight+2,bts);
          mbimage.Canvas.TextOut(start+20*j,i*textHeight+2,bts);

          mbcanvas.Canvas.TextOut(start+20+20*((8*rowsof8)-1)+j*chrlength,i*textHeight+2,'?');
          mbimage.Canvas.TextOut(start+20+20*((8*rowsof8)-1)+j*chrlength,i*textHeight+2,'?');

//          teststr:=teststr+'?';
        end;
      end
      else
      begin
        //range 2
        if range2ok then
        begin
          if (selecting or selectionmade) and (selstart<>selstop) and ((((memoryaddress+i*8*rowsof8)+j)>=selstart) and (((memoryaddress+i*8*rowsof8)+j)<=selstop)) then
          begin
            mbcanvas.Canvas.font.Color:=clred;
            mbimage.Canvas.font.Color:=clred;
          end
          else
          begin
            if (module2ok) then
            begin
              mbcanvas.Canvas.font.Color:=clgreen;
              mbimage.Canvas.font.Color:=clgreen;
            end
            else
            begin
              mbcanvas.Canvas.font.Color:=clwindowtext;
              mbimage.Canvas.font.Color:=clwindowtext;
            end;
          end;

          //readable
          //bts:=IntToHex(buffer[j+i*8*rowsof8],2);
          getBTSString(false);
          mbcanvas.Canvas.TextOut(start+20*j,i*textHeight+2,bts);
          mbimage.Canvas.TextOut(start+20*j,i*textHeight+2,bts);

          if buffer[j+(i*8*rowsof8)]<$20 then
          begin
            mbcanvas.Canvas.TextOut(start+20+20*((8*rowsof8)-1)+j*chrlength,i*textHeight+2,' ');
            mbimage.Canvas.TextOut(start+20+20*((8*rowsof8)-1)+j*chrlength,i*textHeight+2,' ');
          end else
          begin
            mbcanvas.Canvas.TextOut(start+20+20*((8*rowsof8)-1)+j*chrlength,i*textHeight+2,chr(buffer[j+(i*8*rowsof8)]));
            mbimage.Canvas.TextOut(start+20+20*((8*rowsof8)-1)+j*chrlength,i*textHeight+2,chr(buffer[j+(i*8*rowsof8)]));
          end;
        end
        else
        begin
          //unreadable
          if (selecting or selectionmade) and (selstart<>selstop) and ((((memoryaddress+i*8*rowsof8)+j)>=selstart) and (((memoryaddress+i*8*rowsof8)+j)<=selstop)) then
          begin
            mbcanvas.Canvas.font.Color:=clred;
            mbimage.Canvas.font.Color:=clred;
          end
          else
          begin
            mbcanvas.Canvas.font.Color:=clwindowtext;
            mbimage.Canvas.font.Color:=clwindowtext;
          end;

          GetBTSstring(true);
          mbcanvas.Canvas.TextOut(start+20*j,i*textHeight+2,bts);
          mbimage.Canvas.TextOut(start+20*j,i*textHeight+2,bts);

          mbcanvas.Canvas.TextOut(start+20+20*((8*rowsof8)-1)+j*chrlength,i*textHeight+2,'?');
          mbimage.Canvas.TextOut(start+20+20*((8*rowsof8)-1)+j*chrlength,i*textHeight+2,'?');
        end;
      end;
      inc(a);
      inc(j);
    end;



  end;


  {$ifdef net}
  //no virtualquery, so no protectlabel
  protectlabel.Caption:='';
  {$else}
  //set the protectionlabel
  zeromemory(@mbi,sizeof(mbi));
  Virtualqueryex(processhandle,pointer(memoryaddress),mbi,sizeof(mbi));
  teststr:='AllocationProtect=';

  if (mbi.AllocationProtect and PAGE_NOACCESS)>0 then teststr:=teststr+'No Access ';
  if (mbi.AllocationProtect and PAGE_READONLY)>0 then teststr:=teststr+'Read Only ';
  if (mbi.AllocationProtect and PAGE_READWRITE)>0 then teststr:=teststr+'Read/Write ';
  if (mbi.AllocationProtect and PAGE_WRITECOPY)>0 then teststr:=teststr+'Write Copy ';
  if (mbi.AllocationProtect and PAGE_EXECUTE)>0 then teststr:=teststr+'Execute ';
  if (mbi.AllocationProtect and PAGE_EXECUTE_READ)>0 then teststr:=teststr+'Execute/Read only ';
  if (mbi.AllocationProtect and PAGE_EXECUTE_READWRITE)>0 then teststr:=teststr+'Execute/Read/Write ';
  if (mbi.AllocationProtect and PAGE_EXECUTE_WRITECOPY)>0 then teststr:=teststr+'Execute/Write Copy ';
  if (mbi.AllocationProtect and PAGE_GUARD)>0 then teststr:=teststr+'Guarded ';
  if (mbi.AllocationProtect and PAGE_NOCACHE)>0 then teststr:=teststr+'Not Cached';

  if (formsettings<>nil) and formsettings.cbKernelOpenProcess.checked and assigned(GetPhysicalAddress) and GetPhysicalAddress(processhandle,pointer(memoryaddress),a) then
    s:=teststr+' AllocationBase='+IntToHex(dword(mbi.AllocationBase),8)+' RegionSize='+IntTohex(mbi.RegionSize,1)+' Physical Address='+IntToHex(a,8)
  else
    s:=teststr+' AllocationBase='+IntToHex(dword(mbi.AllocationBase),8)+' RegionSize='+IntTohex(mbi.RegionSize,1);


  if module1ok then
    s:=s+' Module='+range1module.modulename;

  protectlabel.caption:=s;

  protectlabel.repaint;
  {$endif}
end;

procedure TMemoryBrowser.MBCanvasMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var a: integer;
    i,j: integer;
    acr: dword;
    bt:byte;

    temp:string;
begin
  if button=mbright then exit;
   
  selecting:=false;

  if selected2<>selected then selectionmade:=true;
  

  fcontrol2.SetFocus;
  hexedit.Visible:=false;
  textedit.Visible:=false;



//find out if the user clicked on a byte, and if so which one
  a:=20+mbimage.Canvas.TextWidth('00400000');
  part:=0; //address

  if x>a then
  begin  //didnt click on the address
    part:=2; //textfield ad the left side
    if x<(a+20*8*rows8) then //byteclick
    begin
      part:=1; //a byte
      srow:=y div textheight;
      scolumn:=(x-a) div 20;

      i:=(srow*8*rows8)+scolumn;
      //caption:=Inttohex(memoryaddress+i,8);
      selected2:=memoryaddress+i;

      if selected<>selected2 then exit;

      //if ssLeft in shift then
      with HexEdit do
      begin
        if visible then
        begin
          if length(hexedit.text)=2 then
          begin
            bt:=strtoint('$'+hexedit.text);
            writeprocessmemory(processhandle,pointer(selected2),@bt,1,acr);
          end;
        end;

        top:=(srow+1)*(textheight)+5;
        left:=1+a+20*(scolumn);

        acr:=0;
        readprocessmemory(processhandle,pointer(selected2),@bt,1,acr);

        if acr=1 then
        begin
          text:=IntToHex(bt,2);
          visible:=true;
          SelectAll;
          SetFocus;
        end else visible:=false;
      end;
    end
    else
    begin
      //not on a byte click
      for i:=1 to 8*rows8 do
        temp:=temp+'D';

      srow:=((y-7) div textheight);


      i:=(x-(1+a+20*8*rows8));

      scolumn:=i div mbcanvas.Canvas.TextWidth('D');

      if scolumn<8*rows8 then
      begin
        i:=scolumn*mbcanvas.Canvas.TextWidth('D');
        selected2:=scolumn+memoryaddress+(8*rows8)*srow;

        if selected<>selected2 then exit;
        

        //if ssLeft in shift then
        with textedit do
        begin
          if visible then
          begin
            if length(textedit.Text)>0 then
            begin
              bt:=byte(textedit.Text[1]);
              if (editing2) and (bt>32) then
                writeprocessmemory(processhandle,pointer(selected2),@bt,1,acr);
            end;
          end;

          textedit.top:=(srow+1)*(textheight)+3;
          textedit.Left:=1+a+20*8*rows8+i;   //  (1+(20+mbimage.Canvas.TextWidth('00400000'))+20*8*rows8+(scolumn*mbcanvas.Canvas.TextWidth('D'))  )

          acr:=0;
          readprocessmemory(processhandle,pointer(selected2),@bt,1,acr);

          if acr=1 then
          begin
            if bt>32 then text:=chr(bt) else text:='';
            visible:=true;
            editing2:=false;
            SelectAll;
            SetFocus;
          end else visible:=false;
        end;
      end;
    end;
  end;

end;

procedure TMemoryBrowser.MBCanvasMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var a,part,srow,scolumns,i: integer;
    temp: string;
begin
  a:=20+mbimage.Canvas.TextWidth('00400000');

  if x>a then
  begin
    if x<(a+20*8*rows8) then //bytepoint
    begin
      mbcanvas.Cursor:=crHandpoint;

      if selecting then
      begin
        part:=0; //address
        if x>a then
        begin  //didnt klick on the address
          part:=2; //textfield at the left side
          if x<(a+20*8*rows8) then //byteclick
          begin
            part:=1; //a byte
            srow:=((y-7) div textheight);
            scolumn:=(x-a) div 20;

            i:=(srow*8*rows8)+scolumn;

            if selected2<>memoryaddress+i then
            begin
              selected2:=memoryaddress+i;

              //and repaint the selected region
              refreshmb;
            end;
          end;
        end;
      end;
    end else
    begin
      mbcanvas.Cursor:=crDefault;
      if selecting then
      begin
        //textclick?
        for i:=1 to 8*rows8 do
          temp:=temp+'D';

        srow:=((y-7) div textheight);


        i:=(x-(1+a+20*8*rows8));

        scolumn:=i div mbcanvas.Canvas.TextWidth('D');

        if scolumn<8*rows8 then
        begin
          i:=scolumn*mbcanvas.Canvas.TextWidth('D');
          i:=scolumn+memoryaddress+(8*rows8)*srow;
          if selected2<>i then
          begin
            selected2:=i;

            refreshmb;
          end;
        end;
      end;


    end;

  end else mbcanvas.Cursor:=crdefault;
end;

procedure TMemoryBrowser.MBCanvasDblClick(Sender: TObject);
begin
  if mbcanvas.Cursor=crHandpoint then
  begin
    if part=1 then
    begin
     //edit
      with Tvaluechangeform.Create(application) do
      begin
        address:=selected;
        ShowModal;
        refreshmb;
      end;
    end;
  end;
end;

procedure TMemoryBrowser.memorypopupPopup(Sender: TObject);
begin
  goto1.Visible:=true;
  change1.Visible:=(mbcanvas.Cursor=crHandpoint);

//  if part=0 then ...
end;

procedure TMemoryBrowser.Replacewithnops1Click(Sender: TObject);
var codelength: integer;
    written: dword;
    bla:string;
    i: integer;
    nops: array of byte;
    a: dword;
    original: dword;

    mbi : _MEMORY_BASIC_INFORMATION;
  //set the protectionlabel
begin
  //search dselected in the addresslist


  a:=disassemblerview.SelectedAddress;

  disassemble(a,bla);
  codelength:=a-disassemblerview.SelectedAddress;

  if advancedoptions.AddToCodeList(disassemblerview.SelectedAddress,codelength,true) then
  begin
    setlength(nops,codelength);
    for i:=0 to codelength-1 do
      nops[i]:=$90;  //$90=nop

    zeromemory(@mbi,sizeof(mbi));
    Virtualqueryex(processhandle,pointer(disassemblerview.SelectedAddress),mbi,sizeof(mbi));

   // get old security and set new security   (not needed in win9x but nt doesnt even allow writeprocessmemory to do this
    original:=0;

    RewriteCode(processhandle,disassemblerview.SelectedAddress,@nops[0],codelength);
    refreshMB;
    disassemblerview.Update;;
  end;
end;

procedure TMemoryBrowser.FControl2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: short;
begin
  case key of
    vk_up       : begin
                    dec(memoryaddress,8*rows8);
                    refreshmb;
                  end;

    vk_down     : begin
                    inc(memoryaddress,8*rows8);
                    refreshmb;
                  end;

    vk_left     : begin
                    dec(memoryaddress);
                    refreshmb;
                  end;
    vk_right    : begin
                    inc(memoryaddress);
                    refreshmb;
                  end;
    vk_prior    : begin
                    dec(memoryaddress,8*rows8*((mbcanvas.Height) div (TextHeight)-1));
                    refreshMB;
                  end;

    vk_next     : begin
                    inc(memoryaddress,8*rows8*((mbcanvas.Height) div (TextHeight)-1));
                    refreshMB;
                  end;

    vk_space:
    begin
      if shift = [ssCtrl] then
        disassemblerview.SelectedAddress:=memoryaddress;
    end;

    {$ifndef net}
    ord('F')    : begin
                    i:=getkeystate(vk_control);
                    if i<=-127 then search1.OnClick(self);
                  end;
    {$endif}

    ord('1')    : if [ssCtrl] = shift then dispBytes.Click;
    ord('2')    : if [ssCtrl] = shift then dispWords.Click;
    ord('3')    : if [ssCtrl] = shift then dispDwords.Click;
    ord('4')    : if [ssCtrl] = shift then dispints.Click;
    ord('5')    : if [ssCtrl] = shift then dispFloat.Click;
    ord('6')    : if [ssCtrl] = shift then dispDouble.Click;

    ord('G')    : begin
                    if ssCtrl in shift then goto1.click;
                  end;

    ord('C')    : begin
                    if getkeystate(vk_control)<=-127 then
                      Cut1.OnClick(self);
                  end;

    ord('V')    : begin
                    if getkeystate(vk_control)<=-127 then
                      Pastefromclipboard1.OnClick(self);
                  end;


    {$ifndef net}
    vk_f3       : begin
                    if findwindow<>nil then //easy way to find out if there was a scan before
                    begin
                      findwindow.editStart.Text:=inttohex(memoryaddress+1,8);
                      findwindow.firstscan:=false;
                    end;
                    findwindow.showmodal;
                  end;
    {$endif}
  end;

  key:=0;
end;

procedure TMemoryBrowser.FControl2KeyPress(Sender: TObject; var Key: Char);
begin
  key:=chr(0);
end;

procedure TMemoryBrowser.FControl2Enter(Sender: TObject);
begin
//  panel4.bevelinner:=bvLowered;
//  mbcanvas.Canvas.DrawFocusRect(rect(0,0,100,100));

end;

procedure TMemoryBrowser.FControl2Exit(Sender: TObject);
begin
  panel4.BevelInner:=bvNone;
//  RefreshMB;
end;

procedure TMemoryBrowser.FControl1Exit(Sender: TObject);
begin

end;

//key control for the disassembler
procedure TMemoryBrowser.FControl1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var rct: trect;
    ignore: string;
    i: integer;

    a,b: dword;
begin
  //if shift is not pressed and it's a up,down,left or right, then disable multiline section
  case key of
    vk_delete:
      Replacewithnops1.Click;


    vk_return:
    begin
      //assembler input
      assemble1.Click;
    end;

    vk_space:
    begin
      if shift=[] then
        follow1.Click
      else
      if shift = [ssShift] then
        back1.click
      else
      if shift = [ssCtrl] then
      begin
        memoryaddress:=disassemblerview.SelectedAddress;
        RefreshMB;
      end;
    end;

    VK_BACK:
    begin
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
            a:=min(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2);
            b:=max(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2);

            
            disassemble(b); //b gets increased with size of selected instruction
            edit1.Text:=inttohex(a,8);
            edit2.Text:=inttohex(b,8);
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

      if (ssalt in shift) or (ssctrl in shift) then exit; 

      assemblepopup(lowercase(chr(key)));
    end;
  end;

  disassemblerview.Update;

  key:=0;
end;

procedure TMemoryBrowser.FControl1KeyPress(Sender: TObject; var Key: Char);
begin
  key:=chr(0);
end;

procedure TMemoryBrowser.Gotoaddress1Click(Sender: TObject);
var newaddress: string;
    symbol :PImagehlpSymbol;
    oldoptions: dword;
    canceled: boolean;
begin
  newaddress:=InputBoxTop('Goto Address','Fill in the address you want to go to',IntTohex(disassemblerview.SelectedAddress,8),true,canceled,memorybrowserHistory);

  try
    disassemblerview.SelectedAddress:=symhandler.getaddressfromname(newaddress);
  except
    disassemblerview.SelectedAddress:=getaddress(newaddress);
  end;
end;

procedure TMemoryBrowser.Search1Click(Sender: TObject);
begin
  {$ifndef net}
  if findwindow=nil then findwindow:=TFindwindow.create(application);
  findwindow.firstscan:=true;
  findwindow.ShowModal;
  {$endif}
end;

procedure TMemoryBrowser.Change1Click(Sender: TObject);
begin
  hexedit.Visible:=false;
  MBCanvasDblClick(MBCanvas);
end;

procedure TMemoryBrowser.Addthisaddresstothelist1Click(Sender: TObject);
var i: integer;
    ad: dword;
begin
//this will add the selected recdord to the list
  if mbcanvas.Cursor=crHandpoint then
  begin
    //selected
    ad:=selected;
    if addform=nil then addform:=Taddform.create(self);
    addform.NewAddress.text:=inttohex(ad,8);
    addform.showmodal;
    //no free because it can be usefull the next time regarding the bits field
  end;
end;

procedure TMemoryBrowser.Addthisopcodetothecodelist1Click(Sender: TObject);
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

end;

procedure TMemoryBrowser.ScrollBar2Scroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var delta: integer;
begin
  fcontrol2.SetFocus;
  case scrollcode of
    scLineUp:   dec(memoryaddress,8*rows8);
    scLineDown: inc(memoryaddress,8*rows8);
    scPageDown: inc(memoryaddress,8*rows8*((mbcanvas.Height) div (TextHeight)-1));
    scPageUp:   dec(memoryaddress,8*rows8*((mbcanvas.Height) div (TextHeight)-1));
    sctrack:
    begin
      delta:=scrollpos-50;
      memoryaddress:=memoryaddress+(8*rows8)*delta;
     // if not formsettings.cbKernelReadWriteProcessMemory.checked then memoryaddress:=trunc(scrollpos/10000*$FFFFFFFF);
    end;
  end;

  refreshmb;
  scrollpos:=50;
end;

procedure TMemoryBrowser.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  {$ifndef net}
  if (debuggerthread<>nil) and (debuggerthread.userisdebugging) then
  begin
    WaitForSingleObject(semaphore,infinite);
    debuggerthread.removebreakpoint;
    releasesemaphore(semaphore,1,nil);

    debuggerthread.DRRegs.Dr0:=0;
    debuggerthread.DRRegs.Dr1:=0;
    debuggerthread.DRRegs.Dr2:=0;
    debuggerthread.DRRegs.Dr3:=0;

    debuggerthread.continuehow:=wdco_run;

    setlength(debuggerthread.userbreakpoints,0);
    setlength(debuggerthread.int3userbreakpoints,0);
    debuggerthread.int3CEBreakpoint.address:=0;

    debuggerthread.continueprocess:=true;
    debuggerthread.userisdebugging:=false;
  end;
  {$endif}


  if ischild then
  begin
    //do stuff particulary for the children
    action:=cafree;
  end
  else
  begin
    //do stuff for the main debugger
    if frmFloatingPointPanel<>nil then
      frmFloatingPointPanel.Visible:=false;

  end;

end;

procedure TMemoryBrowser.Run1Click(Sender: TObject);
begin
  {$ifndef net}
  if kdebugger.isactive then
  begin
    kdebugger.continue(co_run);
  end
  else
  if debuggerthread<>nil then
  begin
    debuggerthread.continuehow:=wdco_run;   //note: I could also have the debuggerthread suspend itself, and resume it here
    debuggerthread.continueprocess:=true;
    caption:='Memory Viewer - Running';
  end;
  {$endif}
end;

procedure TMemoryBrowser.Step1Click(Sender: TObject);
begin
  {$ifndef net}
  if kdebugger.isactive then
  begin
    kdebugger.continue(co_stepinto);
  end
  else
  if debuggerthread<>nil then
  begin
    debuggerthread.continuehow:=wdco_stepinto; //single step
    debuggerthread.continueprocess:=true;
    caption:='Memory Viewer - Running';
  end;

  {$endif}
end;

procedure TMemoryBrowser.StepOver1Click(Sender: TObject);
var x: dword;
    i,j: integer;
    s,s1,s2,temp:string;
    int3: byte;
    original,a,written:dword;

begin
  {$ifndef net}
  int3:=$cc;
  //place a invisble for the user breakpoint on the following upcode

  x:=lastdebugcontext.Eip;
  s:=disassemble(x,temp);
  i:=posex('-',s);
  i:=posex('-',s,i+1);
  s:=copy(s,i+2,length(s));

  i:=pos(' ',s);
  s1:=copy(s,1,i-1);
  s2:=copy(s,i+1,length(s));

  if not ((s1='call') or (s1='loop')) then //not a call or loop
  begin
    Step1.Click;
    exit;
  end;

  if kdebugger.isactive then
  begin
    kdebugger.continue(co_stepover);
  end
  else
  if debuggerthread<>nil then
  begin
    debuggerthread.continuehow:=wdco_stepOver; //step over



    if formsettings.rbDebugAsBreakpoint.checked then
    begin
      debuggerthread.DRRegs.Dr3:=x;

      with debuggerthread do
      for i:=0 to length(threadlist)-1 do
      begin
        suspendthread(debuggerthread.threadlist[i][1]);
        setthreadcontext(debuggerthread.threadlist[i][1],debuggerthread.DRRegs);
        resumethread(debuggerthread.threadlist[i][1]);
      end;
    end else
    begin
      // use the int3CEBreakpoint
      //see if there is already a Breakpoint with this address
      if debuggerthread.int3CEBreakpoint.address>0 then
        RewriteCode(processhandle,debuggerthread.int3CEBreakpoint.address,@debuggerthread.int3CEBreakpoint.originalbyte,1);

      for i:=0 to length(debuggerthread.int3userbreakpoints)-1 do
        if debuggerthread.int3userbreakpoints[i].address=x then
        begin
          //dont need to set a breakpoint
          debuggerthread.continueprocess:=true;
          caption:='Memory Viewer - Running';
          exit;
        end;

      debuggerthread.int3CEBreakpoint.address:=x;
      readprocessmemory(processhandle,pointer(x),@debuggerthread.int3CEBreakpoint.originalbyte,1,written);

      Rewritecode(processhandle,x,@int3,1);
    end;

    debuggerthread.continueprocess:=true;
    caption:='Memory Viewer - Running';
  end;
  {$endif}
end;

procedure TMemoryBrowser.Runtill1Click(Sender: TObject);
var x: dword;
    i: integer;
    temp:string;
    int3: byte;
    original,a,written:dword;
begin
  {$ifndef net}
  int3:=$cc;
  //place a invisble for the user breakpoint on the following upcode

  if kdebugger.isactive then
  begin
    kdebugger.continue(co_runtill);
  end
  else
  if debuggerthread<>nil then
  begin
    debuggerthread.continuehow:=wdco_stepOver; //step over
    x:=disassemblerview.SelectedAddress;
    disassemble(x,temp);

    if formsettings.rbDebugAsBreakpoint.checked then
    begin
      debuggerthread.DRRegs.Dr3:=x;

      with debuggerthread do
      for i:=0 to length(threadlist)-1 do
      begin
        suspendthread(debuggerthread.threadlist[i][1]);
        setthreadcontext(debuggerthread.threadlist[i][1],debuggerthread.DRRegs);
        resumethread(debuggerthread.threadlist[i][1]);
      end;
    end else
    begin
      if debuggerthread.int3CEBreakpoint.address>0 then
      begin
        //restore with original
        RewriteCode(processhandle,debuggerthread.int3CEBreakpoint.address,@debuggerthread.int3CEBreakpoint.originalbyte,1);
      end;

      for i:=0 to length(debuggerthread.int3userbreakpoints)-1 do
        if debuggerthread.int3userbreakpoints[i].address=x then
        begin
          //dont need to set a breakpoint
          debuggerthread.continueprocess:=true;
          caption:='Memory Viewer - Running';
          exit;
        end;

      debuggerthread.int3CEBreakpoint.address:=x;
      readprocessmemory(processhandle,pointer(x),@debuggerthread.int3CEBreakpoint.originalbyte,1,written);

      Rewritecode(processhandle,x,@int3,1);
    end;

    debuggerthread.continueprocess:=true;
    caption:='Memory Viewer - Running';
  end;

  {$endif}
end;

procedure TMemoryBrowser.Stacktrace1Click(Sender: TObject);
begin
  {$ifndef net}
  frmstacktrace:=tfrmstacktrace.create(self);
  frmstacktrace.Show;
  {$endif}
end;

procedure TMemoryBrowser.Threadlist1Click(Sender: TObject);
begin
  {$ifndef net}
  if frmThreadlist<>nil then exit;

  if not startdebuggerifneeded then exit;
  frmThreadlist:=tfrmthreadlist.create(self);
  frmThreadlist.show;
  {$endif}

end;

procedure TMemoryBrowser.AssemblePopup(x:string);
var assemblercode,desc: string;
    bytes: tassemblerbytes;
    a,b,original,written:dword;
    originalsize:dword;
    replace: boolean;
    c: word;

    res: word;
    i: integer;
    canceled: boolean;
begin
  //make sure it doesnt have a breakpoint
  {$ifndef net}
  if debuggerthread<>nil then
  begin
    for i:=0 to length(debuggerthread.int3userbreakpoints)-1 do
      if debuggerthread.int3userbreakpoints[i].address=disassemblerview.SelectedAddress then
      begin
        beep; //Best sound effect cheat engine has
        exit;
      end;
  end;
  {$endif}

  originalsize:=disassemblerview.SelectedAddress;
  assemblercode:=disassemble(originalsize,desc);
  dec(originalsize,disassemblerview.SelectedAddress);

  if x<>'' then
    assemblercode:=x
  else
  begin
    assemblercode:=copy(assemblercode,pos('-',assemblercode)+2,length(assemblercode));
    assemblercode:=copy(assemblercode,pos('-',assemblercode)+2,length(assemblercode));
  end;



//  copy

  assemblercode:=InputboxTop('Cheat Engine single-linge assembler','Type your assembler code here: (address='+inttohex(disassemblerview.SelectedAddress,8)+')',assemblercode,x='', canceled, assemblerHistory);
  if not canceled then
  begin

    try
      if Assemble(assemblercode,disassemblerview.SelectedAddress,bytes) then
      begin
        if originalsize<>length(bytes) then
        begin
          if formsettings.replacewithnops.checked then
          begin
            if formsettings.askforreplacewithnops.checked then
            begin
              c:=messagedlg('The generated code is '+IntToStr(length(bytes))+' byte(s) long, but the selected opcode is '+IntToStr(originalsize)+' byte(s) long! Do you want to replace the incomplete opcode(s) with NOP''s?',mtConfirmation,mbYesNoCancel,0);
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

              a:=disassemblerview.SelectedAddress+length(bytes);

              b:=disassemblerview.SelectedAddress;
              while b<a do disassemble(b,desc);

              a:=b-disassemblerview.SelectedAddress;
              while length(bytes)<a do
              begin
                setlength(bytes,length(bytes)+1);
                bytes[length(bytes)-1]:=$90;
              end;
            end;


          end;
        end;

        //note to self, check the size of the current opcode and give the option to replace the missing or too many bytes with nops
        //and put in a option to disable showing that message, and use a default action

        // get old security and set new security   (not needed in win9x but nt doesnt even allow writeprocessmemory to do this
        original:=0;

        RewriteCode(processhandle,disassemblerview.SelectedAddress,@bytes[0],length(bytes));
        refreshMB;
        disassemblerview.Update;
      end else raise exception.create('I don''t understand what you mean with '+assemblercode);
    except
      raise exception.create('I don''t understand what you mean with '+assemblercode);
    end;

  end;

end;

procedure TMemoryBrowser.Assemble1Click(Sender: TObject);
begin
  AssemblePopup('');
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
var bt: byte;
    aw: dword;
begin
//change
  if not hexedit.Visible then exit;

  if length(hexedit.Text)<2 then
  begin
    hexedit.Visible:=false;
    beep;
    exit;
  end;

  bt:=strtoint('$'+hexedit.text);

  writeprocessmemory(processhandle,pointer(selected),@bt,1,aw);
  refreshmb;

  hexedit.Visible:=false;
end;

procedure TMemoryBrowser.HexEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var  a: integer;
     bt: byte;
     acr: dword;
procedure handleright;
begin
  if (length(hexedit.text)<2) and (key in [vk_right and vk_space] ) then
  begin
    if key=vk_right then exit;
    beep;
    key:=0;
    exit;
  end;

  if key=vk_right then
    if (hexedit.SelStart<>2) and (hexedit.SelLength<>2) then exit;

  bt:=strtoint('$'+hexedit.text);
  writeprocessmemory(processhandle,pointer(selected),@bt,1,acr);
  refreshmb;

  inc(scolumn);
  scolumn:=scolumn mod (8*rows8);
  if (scolumn)=0 then
  begin
    //move down
    inc(srow);

    if (((srow+1)*(textheight)+3)+hexedit.Height)>panel4.Height then
    begin
      dec(srow);
      hexedit.top:=(srow+1)*(textheight)+3;
      inc(memoryaddress,8*rows8);
      refreshmb;
    end else hexedit.Top:=(srow+1)*(textheight)+3;
  end;

  hexedit.left:=1+a+20*(scolumn);
  inc(selected);

  acr:=0;
  readprocessmemory(processhandle,pointer(selected),@bt,1,acr);

  if acr=1 then
  begin
    hexedit.text:=IntToHex(bt,2);
    hexedit.visible:=true;
    hexedit.SelectAll;
    hexedit.SetFocus;
  end else hexedit.visible:=false;
  key:=0;
  editing:=false;
end;
begin
  selecting:=false;
  selectionmade:=false;
  selected2:=selected;
  
  a:=20+mbimage.Canvas.TextWidth('00400000');
  case key of
    ord('C')    : begin
                    if (getkeystate(vk_control) shr 15) and 1 = 1 then
                    begin
                      editing:=false;
                      key:=0;
                      hexedit.Visible:=false;
                      fcontrol2.SetFocus;
                      Cut1.OnClick(self);
                    end
                    else
                    begin
                      if (length(hexedit.text)=1) and (hexedit.SelLength=0) then
                      begin
                        hexedit.text:=hexedit.Text+char(key);
                        handleright;
                        editing:=true;
                      end;
                    end;
                  end;

    ord('V')    : begin
                    if (getkeystate(vk_control) shr 15) and 1= 1 then
                    begin
                      editing:=false;
                      key:=0;
                      hexedit.Visible:=false;
                      fcontrol2.SetFocus;
                      Pastefromclipboard1.OnClick(self);
                    end;
                  end;
                  

    vk_add:   begin
                 acr:=0;
                 if readprocessmemory(processhandle,pointer(selected),@bt,1,acr) then
                 begin
                   inc(bt);
                   if writeprocessmemory(processhandle,pointeR(selected),@bt,1,acr) then
                   begin
                     hexedit.Text:=IntToHex(bt,2);
                     hexedit.SelectAll;
                   end;
                 end;
                 editing:=true;
                 key:=0;
                 disassemblerview.Update;
               end;

    vk_subtract:begin
                 acr:=0;
                 if readprocessmemory(processhandle,pointer(selected),@bt,1,acr) then
                 begin
                   dec(bt);
                   if writeprocessmemory(processhandle,pointeR(selected),@bt,1,acr) then
                   begin
                     hexedit.Text:=IntToHex(bt,2);
                     hexedit.SelectAll;
                   end;
                 end;
                 editing:=true;
                 key:=0;
                 disassemblerview.Update;
               end;

    vk_escape: begin
                 hexedit.Visible:=false;
                 fcontrol2.SetFocus;
               end;

    vk_return: begin
                 if length(hexedit.Text)<2 then
                 begin
                   beep;
                   key:=0;
                   exit;
                 end;

                 hexedit.Visible:=false;
                 bt:=strtoint('$'+hexedit.text);
                 writeprocessmemory(processhandle,pointer(selected),@bt,1,acr);
                 refreshmb;
                 key:=0;
               end;

    vk_up:     begin
                 if length(hexedit.text)<2 then
                 begin
                   beep;
                   exit;
                 end;

                 dec(srow);
                 if srow<0 then
                 begin
                   srow:=0;
                   hexedit.Top:=(srow+1)*(textheight)+3;

                   dec(memoryaddress,8*rows8);
                   refreshmb;
                 end else hexedit.Top:=(srow+1)*(textheight)+3;

                 bt:=strtoint('$'+hexedit.text);
                 writeprocessmemory(processhandle,pointer(selected),@bt,1,acr);
                 refreshmb;


                 dec(selected,8*rows8);

                 acr:=0;
                 readprocessmemory(processhandle,pointer(selected),@bt,1,acr);

                 if acr=1 then
                 begin
                   hexedit.text:=IntToHex(bt,2);
                   hexedit.visible:=true;
                   hexedit.SelectAll;
                   hexedit.SetFocus;
                 end else hexedit.visible:=false;
                 key:=0;

               end;

    vk_down:   begin
                 if length(hexedit.text)<2 then
                 begin
                   beep;
                   exit;
                 end;

                 inc(srow);
                 if (((srow+1)*(textheight)+3)+hexedit.Height)>panel4.Height then
                 begin
                   dec(srow);
                   hexedit.top:=(srow+1)*(textheight)+3;
                   inc(memoryaddress,8*rows8);
                   refreshmb;
                 end else hexedit.Top:=(srow+1)*(textheight)+3;

                 bt:=strtoint('$'+hexedit.text);
                 writeprocessmemory(processhandle,pointer(selected),@bt,1,acr);
                 refreshmb;


                 inc(selected,8*rows8);

                 acr:=0;
                 readprocessmemory(processhandle,pointer(selected),@bt,1,acr);

                 if acr=1 then
                 begin
                   hexedit.text:=IntToHex(bt,2);
                   hexedit.visible:=true;
                   hexedit.SelectAll;
                   hexedit.SetFocus;
                 end else hexedit.visible:=false;
                 key:=0;

               end;


    vk_left:   begin
                 if length(hexedit.Text)<2 then
                 begin
                   if (hexedit.selstart=0) then beep;
                   exit;
                 end;

                 if hexedit.SelStart>0 then exit;

                 bt:=strtoint('$'+hexedit.text);
                 writeprocessmemory(processhandle,pointer(selected),@bt,1,acr);
                 refreshmb;

                 dec(scolumn);
                 if scolumn=-1 then scolumn:=8*rows8-1;
                 if scolumn=8*rows8-1 then
                 begin
                   //move up
                   dec(srow);
                   if srow<0 then
                   begin
                     srow:=0;
                     hexedit.Top:=(srow+1)*(textheight)+3;

                     dec(memoryaddress,8*rows8);
                     refreshmb;
                   end else hexedit.Top:=(srow+1)*(textheight)+3;

                 end;

                 hexedit.left:=1+a+20*(scolumn);
                 dec(selected);

                 acr:=0;
                 readprocessmemory(processhandle,pointer(selected),@bt,1,acr);

                 if acr=1 then
                 begin
                   hexedit.text:=IntToHex(bt,2);
                   hexedit.visible:=true;
                   hexedit.SelectAll;
                   hexedit.SetFocus;
                 end else hexedit.visible:=false;
                 key:=0;
               end;


    vk_space,vk_right:
    begin
      handleright;
    end;

    else
    begin
      if key in [ord('0')..ord('9'),ord('A')..ord('F'),96..105] then
      begin
        if key in [96..105] then ///numpad fix
          key:=key-96+ord('0');
          
        if (length(hexedit.text)=1) and (hexedit.SelLength=0) then
        begin
          hexedit.text:=hexedit.Text+char(key);
          handleright;
          editing:=true;

        end;
      end
      else
      key:=0;
    end;
  end;
end;

procedure TMemoryBrowser.EAXLabelDblClick(Sender: TObject);
var x: dword;
    i: integer;
    regname,input: string;
    value: dword;

begin
  //edit the selected register
  {$ifndef net}
  if (debuggerthread<>nil) and (not debuggerthread.running) then
  begin
    with debuggerthread do
    begin
      i:=tlabel(sender).Tag;
      case i of
      0: regname:='EAX';
      1: regname:='EBX';
      2: regname:='ECX';
      3: regname:='EDX';
      4: regname:='ESI';
      5: regname:='EDI';
      6: regname:='EBP';
      7: regname:='ESP';
      8: regname:='EIP';
      9: regname:='CS';
      10: regname:='SS';
      11: regname:='DS';
      12: regname:='ES';
      13: regname:='FS';
      14: regname:='GS';
      20: regname:='CF';
      21: regname:='PF';
      22: regname:='AF';
      23: regname:='ZF';
      24: regname:='SF';
      25: regname:='DF';
      end;

      input:=copy(tlabel(sender).Caption,pos(' ',tlabel(sender).Caption)+1,8);
      if i<20 then
      begin
        if not inputquery('Change register','What is the new value of '+regname+'?',input) then exit;
      end
      else
      begin
        if not inputquery('Change register','What is the new value of '+regname+'? (0 or 1)',input) then exit;
        input:=trim(input);
        if (input<>'0') and (input<>'1') then
          raise exception.create(input+' is not a valid value');

      end;

      value:=StrToInt('$'+input);


      context.ContextFlags:=CONTEXT_FULL; //
      getthreadcontext(pausedthreadhandle,context);
      case tlabel(sendeR).Tag of
        0: context.Eax:=value;    //eax
        1: context.Ebx:=value;    //ebx
        2: context.Ecx:=value;    //ecx
        3: context.Edx:=value;    //edx
        4: context.Esi:=value;    //esi
        5: context.Edi:=value;    //edi
        6: context.Ebp:=value;    //ebp
        7: context.Esp:=value;    //esp
        8: context.Eip:=value;    //eip
        9: context.segcs:=value;    //cs
        10: context.segss:=value;    //ss
        11: context.segds:=value;    //ds
        12: context.seges:=value;    //es
        13: context.segfs:=value;    //fs
        14: context.seggs:=value;    //gs

        20: setbit(0,context.eflags,value); //0=cf
        21: setbit(2,context.eflags,value); //2=pf
        22: setbit(4,context.eflags,value); //4=af
        23: setbit(6,context.eflags,value); //6=zf
        24: setbit(7,context.eflags,value); //7=sf
        25: setbit(10,context.eflags,value); //10=df
        26: setbit(11,context.eflags,value); //11=of
      end;

      if setthreadcontext(pausedthreadhandle,context) then
      begin
        if tlabel(sender).Tag>=9 then
          tlabel(sender).Caption:=regname+' '+inttohex(value,4)
        else
          tlabel(sender).Caption:=regname+' '+inttohex(value,8)
      end;
    end;

  end;
  {$endif}
end;

procedure TMemoryBrowser.Break1Click(Sender: TObject);
var threadhandle: thandle;
begin
  {$ifndef net}
  if not startdebuggerifneeded then exit;

  threadhandle:=debuggerthread.threadlist[0,1];

  if length(debuggerthread.threadlist)>=1 then
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

  if not breakthread(threadhandle) then raise exception.Create('Can''t set breakpoint');

  {$endif}
end;

procedure TMemoryBrowser.Reservememory1Click(Sender: TObject);
var count: string;
    memsize: integer;
    baseaddress: pointer;
    x: dword;
    s: string;
begin
{$ifndef net}
  count:='4096';
  if inputquery('Allocate memory','How much memory do you want to add to this process. ',count) then
  begin
    try
      memsize:=StrToInt(count);
    except
      raise exception.Create('How much is '+count+'?');
    end;

    baseaddress:=nil;
    baseaddress:=VirtualAllocEx(processhandle,nil,memsize,MEM_COMMIT,PAGE_EXECUTE_READWRITE);
    if baseaddress<>nil then
    begin


      if (disassemblerview.SelectedAddress<>0) and (memsize>7) and (messagedlg('At least '+IntToStr(memsize)+' bytes have been allocated at '+IntToHex(dword(baseaddress),8)+#13#10+'Do you want replace the currently selected address with a jump to that address, and copy the overwritten instructions to there?',mtConfirmation,[mbyes,mbno],0)=mryes) then
        CreateCodecave(dword(baseaddress),disassemblerview.SelectedAddress,memsize)
      else
        messagedlg('At least '+IntToStr(memsize)+' bytes have been allocated at '+IntToHex(dword(baseaddress),8),mtinformation,[mbok],0);


      if messagedlg('Do you want to go there now?',mtConfirmation,[mbyes,mbno],0) = mryes then
        disassemblerview.SelectedAddress:=dword(baseaddress);


    end else raise exception.Create('Error allocating memory!');


  end;
  {$endif}
end;

procedure TMemoryBrowser.Savememoryregion1Click(Sender: TObject);
begin
//will save a cheat engine memory region file .CEM
//header:
//'CHEATENGINE'
//4 bytes:Version
//4 bytes:beginaddress
//4 bytes:size
  TFrmSaveMemoryRegion.create(self).showmodal;
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
  startdebuggerifneeded;

  formdebugstrings.show;
  {$endif}
end;

procedure TMemoryBrowser.TextEditExit(Sender: TObject);
var bt: byte;
    aw: dword;
begin
//change
  if not textedit.Visible then exit;

  if length(hexedit.Text)<2 then
  begin
    hexedit.Visible:=false;
    beep;
    exit;
  end;

  if length(textedit.text)=1 then
  begin
    bt:=byte(textedit.text[1]);
    if bt>32 then
      writeprocessmemory(processhandle,pointer(selected),@bt,1,aw);
  end;

  refreshmb;

  textedit.Visible:=false;
end;

procedure TMemoryBrowser.TextEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var bt: byte;
    acr: dword;
    a: integer;
procedure handleright;
begin
  if (length(textedit.text)=0) then
  begin
    if key=vk_right then exit;
    beep;
    key:=0;
    exit;
  end;

  if editing2 then
  begin
    bt:=byte(textedit.text[1]);
    if bt>32 then
      writeprocessmemory(processhandle,pointer(selected),@bt,1,acr);
    refreshmb;
  end;

  inc(scolumn);
  scolumn:=scolumn mod (8*rows8);
  if (scolumn)=0 then
  begin
    //move down
    inc(srow);

    if (((srow+1)*(textheight)+3)+textedit.Height)>panel4.Height then
    begin
      dec(srow);
      textedit.top:=(srow+1)*(textheight)+3;
      inc(memoryaddress,8*rows8);
      refreshmb;
    end else textedit.Top:=(srow+1)*(textheight)+3;
  end;

  textedit.left:=(1+a+20*8*rows8+(scolumn*mbcanvas.Canvas.TextWidth('D')));
  inc(selected);

  acr:=0;
  readprocessmemory(processhandle,pointer(selected),@bt,1,acr);

  if acr=1 then
  begin
    if bt>32 then textedit.text:=chr(bt) else textedit.text:=' ';
    editing2:=false;
    textedit.visible:=true;
    textedit.SelectAll;
    textedit.SetFocus;
  end else hexedit.visible:=false;
  key:=0;
end;
begin
  a:=20+mbimage.Canvas.TextWidth('00400000');

  case key of
    ord('C'):
    begin
      if getkeystate(vk_control)<=-127 then
      begin
        editing:=false;
        key:=0;
        hexedit.Visible:=false;
        fcontrol2.SetFocus;

        Cut1.OnClick(self);
      end else editing2:=true;
    end;

    ord('V'):
    begin
      if getkeystate(vk_control)<=-127 then
      begin
        editing:=false;
        key:=0;
        hexedit.Visible:=false;
        fcontrol2.SetFocus;
        Pastefromclipboard1.OnClick(self);
      end else editing2:=true;
    end;

    vk_escape:
    begin
      textedit.Visible:=false;
      fcontrol2.SetFocus;
    end;

    vk_return:
    begin
      if length(textedit.Text)<1 then
      begin
        beep;
        key:=0;
        exit;
      end;

      textedit.Visible:=false;
      bt:=byte(textedit.text[1]);
      writeprocessmemory(processhandle,pointer(selected),@bt,1,acr);
      refreshmb;
      key:=0;
    end;


    vk_up:
    begin
      if length(textedit.text)<1 then
      begin
        beep;
        exit;
      end;

      dec(srow);
      if srow<0 then
      begin
        srow:=0;
        textedit.Top:=(srow+1)*(textheight)+3;
        dec(memoryaddress,8*rows8);
        refreshmb;
      end
      else textedit.Top:=(srow+1)*(textheight)+3;

      if editing2 then
      begin
        bt:=byte(textedit.text[1]);
        if bt>32 then
          writeprocessmemory(processhandle,pointer(selected),@bt,1,acr);
      end;

      refreshmb;

      dec(selected,8*rows8);

      acr:=0;
      readprocessmemory(processhandle,pointer(selected),@bt,1,acr);

      if acr=1 then
      begin
        if bt>32 then textedit.text:=chr(bt) else textedit.text:=' ';
        editing2:=false;
        textedit.visible:=true;
        textedit.SelectAll;
        textedit.SetFocus;
      end else textedit.visible:=false;
      key:=0;
    end;

    vk_down:
    begin
      if length(textedit.text)=0 then
      begin
        beep;
        exit;
      end;

      inc(srow);
      if (((srow+1)*(textheight)+3)+textedit.Height)>panel4.Height then
      begin
        dec(srow);
        textedit.top:=(srow+1)*(textheight)+3;
        inc(memoryaddress,8*rows8);
        refreshmb;
      end else textedit.Top:=(srow+1)*(textheight)+3;

      if editing2 then
      begin
        bt:=byte(textedit.text[1]);
        if bt>32 then
          writeprocessmemory(processhandle,pointer(selected),@bt,1,acr);
        refreshmb;
      end;

      inc(selected,8*rows8);

      acr:=0;
      readprocessmemory(processhandle,pointer(selected),@bt,1,acr);

      if acr=1 then
      begin
        if bt>32 then textedit.text:=chr(bt) else textedit.Text:=' ';
        editing2:=false;
        textedit.visible:=true;
        textedit.SelectAll;
        textedit.SetFocus;
      end else textedit.visible:=false;
      key:=0;

    end;


    vk_left:
    begin
      if length(textedit.Text)<1 then
      begin
        beep;
        exit;
      end;

      bt:=byte(textedit.text[1]);
      if editing2 then
      if bt>32 then
        writeprocessmemory(processhandle,pointer(selected),@bt,1,acr);

      refreshmb;

      dec(scolumn);
      if scolumn=-1 then scolumn:=8*rows8-1;

      if scolumn=8*rows8-1 then
      begin
        //move up
        dec(srow);
        if srow<0 then
        begin
          srow:=0;
          textedit.Top:=(srow+1)*(textheight)+3;

          dec(memoryaddress,8*rows8);
          refreshmb;
        end else textedit.Top:=(srow+1)*(textheight)+3;
      end;

      textedit.left:=(1+a+20*8*rows8+(scolumn*mbcanvas.Canvas.TextWidth('D')));            //1+a+20*(scolumn);
      dec(selected);

      acr:=0;
      readprocessmemory(processhandle,pointer(selected),@bt,1,acr);

      if acr=1 then
      begin
        if bt>32 then textedit.text:=chr(bt) else textedit.text:=' ';
        editing2:=false;
        textedit.visible:=true;
        textedit.SelectAll;
        textedit.SetFocus;
      end else textedit.visible:=false;
      key:=0;
    end;

    vk_right:
    begin
      handleright;
    end;

    else begin
           editing2:=true;
         end;
  end;
end;

procedure TMemoryBrowser.CreateThread1Click(Sender: TObject);
var startaddress,parameter: dword;
    ThreadID: dword;
    start:string;
    param:string;
begin
  start:=IntToHex(disassemblerview.SelectedAddress,8);
  param:='0';
  if not InputQuery('Create remote thread','What will be the startaddress of this thread?',start) then exit;
  try
    startaddress:=StrToInt('$'+start);
  except
    raise exception.Create('Please enter a valid hexadecimal addres');
  end;

  if not InputQuery('Create remote thread','You want to give an additional 32-bit parameter? (Will show up in EBX)',param) then exit;
  try
    parameter:=StrToInt('$'+param);
  except
    raise exception.Create('Please enter a valid hexadecimal value');
  end;

  if CreateRemoteThread(processhandle,nil,0,pointer(startaddress),pointer(parameter),0,threadid)=0 then raise exception.Create('Creation of the remote thread failed') else showmessage('Thread Created');
end;

procedure TMemoryBrowser.MemoryRegions1Click(Sender: TObject);
begin
  {$ifndef net}
  formmemoryregions:=tformmemoryregions.Create(self);
  formmemoryregions.show;
  {$endif}
end;

procedure TMemoryBrowser.TextEditKeyPress(Sender: TObject; var Key: Char);
var bt: byte;
acr:dword;
a:integer;
begin
  a:=20+mbimage.Canvas.TextWidth('00400000');
  if editing2 then
  begin
    bt:=byte(key);
    if bt>=32 then
      writeprocessmemory(processhandle,pointer(selected),@bt,1,acr)
    else
    begin
      key:=#0;
      exit;
    end;

    inc(scolumn);
    scolumn:=scolumn mod (8*rows8);
    if (scolumn)=0 then
    begin
      //move down
      inc(srow);

      if (((srow+1)*(textheight)+3)+textedit.Height)>panel4.Height then
      begin
        dec(srow);
        textedit.top:=(srow+1)*(textheight)+3;
        inc(memoryaddress,8*rows8);
        refreshmb;
      end else textedit.Top:=(srow+1)*(textheight)+3;
    end;

    textedit.left:=(1+a+20*8*rows8+(scolumn*mbcanvas.Canvas.TextWidth('D')));
    inc(selected);

    acr:=0;
    readprocessmemory(processhandle,pointer(selected),@bt,1,acr);

    if acr=1 then
    begin
      if bt>32 then textedit.text:=chr(bt) else textedit.text:=' ';
      editing2:=false;
      textedit.visible:=true;
      textedit.SelectAll;
      textedit.SetFocus;
    end else hexedit.visible:=false;
    key:=#0;
  end;

  refreshmb;
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
    start,stop: dword;
    output: textfile;
begin
{$ifndef net}
  with tfrmSavedisassembly.create(self) do
  begin
    edit1.Text:=inttohex(min(disassemblerview.SelectedAddress,disassemblerview.SelectedAddress2),8);
    edit2.Text:=inttohex(max(disassemblerview.SelectedAddress,disassemblerview.SelectedAddress2),8);
    show;
  end;
{$endif}
end;

procedure TMemoryBrowser.Heaps1Click(Sender: TObject);
begin
  {$ifndef net}
  if processid=0 then raise exception.Create('Please target a process first');
  if (frmMemoryAllocHandler<>nil) and (frmMemoryAllocHandler.hookedprocessid<>processid) then
    freeandnil(frmMemoryAllocHandler);

  if frmheaps=nil then
    frmheaps:=tfrmheaps.create(self);

  frmheaps.show;
  {$endif}

end;

procedure TMemoryBrowser.EnumeratedllsandSymbols1Click(Sender: TObject);
begin
  {$ifndef net}
  symhandler.reinitialize;
  
  if frmEnumerateDLLs=nil then
  begin
    frmEnumerateDLLs:=tfrmEnumerateDLLs.create(self);
    frmEnumerateDLLs.Show;
  end
  else frmEnumerateDLLs.enumerate;
  {$endif}
end;

procedure TMemoryBrowser.InjectDLL1Click(Sender: TObject);
var dll: string;
    functionname: string;
    dllList: tstringlist;
begin
  functionname:='';
  dll:='';

  if opendlldialog.Execute then
  begin
    dll:=opendlldialog.Filename;
    if MessageDlg('Do you want to execute a function of the dll?',mtConfirmation	,[mbyes,mbno],0)=mryes then
    begin
      dllList:=tstringlist.Create;

      try
        peinfo_getExportList(opendlldialog.filename, dllList);
        with TfrmFunctionList.create(self,dllList) do
        begin
          if showmodal=mrok then
            if itemindex<>-1 then
              functionname:=functions[itemindex];

          free;
        end;
      finally
        dllList.free;
      end;
    end;

    InjectDll(dll,functionname);
    symhandler.reinitialize;
    showmessage('DLL Injected');
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

  frmdissectcode.Show;
  {$endif}
end;

procedure TMemoryBrowser.Createjumptocodecave1Click(Sender: TObject);
var x: string;
    codecaveaddress: dword;
    size: dword;
begin
  x:='';
  if inputquery('Create Code-Cave','Give the address of the code-cave you want to use. (Use allocate memory if you couldn''t find one)',x) then
  begin
    try
      codecaveaddress:=StrToInt('$'+x);
    except
      raise exception.Create('Please give me a valid address');
    end;

    x:='40';
    if inputquery('Create Code-Cave','How big is the code-cave (or how much do you think you''ll use?)',x) then
    begin
      try
        size:=StrToInt(x);
      except
        raise exception.Create('And how many bytes are that?');
      end;

      CreateCodecave(codecaveaddress,disassemblerview.SelectedAddress,size);

    end;


  end;
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
{$ifndef net}
  if foundcodedialog<>nil then
    raise exception.Create('I can''t do that! You are currently using one of the code finder options, please, stop it first');

  if (formsettings.cbKdebug.checked) and (kdebugger.isactive) and (kdebugger.nrofbreakpoints=4) then raise exception.Create('You have reached the maximum of 4 debugregs. Disable at least one breakpoint first'); //all spots filled up

  if (not formsettings.cbKdebug.checked) then
    if (not startdebuggerifneeded) then exit;

  tfrmModifyRegisters.create(self,disassemblerview.SelectedAddress).showmodal;
{$endif}
end;

procedure TMemoryBrowser.ogglebreakpoint1Click(Sender: TObject);
begin
{$ifndef net}
  if (formsettings.cbKdebug.checked) and (debuggerthread=nil) then
  begin
    KDebugger.StartDebugger;
    KDebugger.ToggleBreakpoint(disassemblerview.SelectedAddress);
  end
  else
  begin
    //normal debugger
    togglebreakpoint(disassemblerview.SelectedAddress);
    disassemblerview.Update;
  end;


{$endif}
end;

procedure TMemoryBrowser.Breakpointlist1Click(Sender: TObject);
begin
{$ifndef net}
  if frmbreakpointlist=nil then
  begin
    frmbreakpointlist:=tfrmBreakpointlist.create(self);
    frmBreakpointlist.show;
  end
  else
    frmbreakpointlist.bringtofront;
{$endif}
end;


procedure TMemoryBrowser.Makepagewritable1Click(Sender: TObject);
var x: dword;
begin
{$ifndef net}
  VirtualProtectEx(processhandle,pointer(memoryaddress),4096,PAGE_EXECUTE_READWRITE,x);
//  if (memoryaddress>80000000) and (DarkByteKernel<>0) then
//    MakeWritableEx(processhandle,memoryaddress,4096,false);
{$endif}
end;

procedure TMemoryBrowser.Dissectdata1Click(Sender: TObject);
begin
{$ifndef net}
  with tfrmpointerscanner.create(self) do
    show;
{$endif}
end;

procedure TMemoryBrowser.Showsymbols1Click(Sender: TObject);
begin
  showsymbols1.Checked:=not showsymbols1.Checked;
  symhandler.showsymbols:=showsymbols1.Checked;
  disassemblerview.Update;
end;

procedure TMemoryBrowser.Showmoduleaddresses1Click(Sender: TObject);
begin
  Showmoduleaddresses1.Checked:=not Showmoduleaddresses1.checked;
  symhandler.showmodules:=Showmoduleaddresses1.Checked;
  disassemblerview.Update;
end;


procedure TMemoryBrowser.Dissectdata2Click(Sender: TObject);
begin
{$ifndef net}
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
      update(false);
      show;
    end;
  end;



{$endif}
end;

procedure TMemoryBrowser.Symbolhandler1Click(Sender: TObject);
begin
{$ifndef net}
  if frmSymbolhandler=nil then
    frmSymbolhandler:=TfrmSymbolhandler.create(self);

  frmSymbolhandler.show;
{$endif}
end;

procedure TMemoryBrowser.Allocatenonpagedmemory1Click(Sender: TObject);
var count: string;
    memsize: integer;
    baseaddress: pointer;
    x: dword;
    s: string;
begin
{$ifndef net}
  count:='4096';
  if inputquery('Allocate memory','How much memory do you wish to allocate?. ',count) then
  begin
    try
      memsize:=StrToInt(count);
    except
      raise exception.Create('How much is '+count+'?');
    end;

    baseaddress:=nil;
    baseaddress:=KernelAlloc(memsize);
    if baseaddress<>nil then
    begin
      if (disassemblerview.SelectedAddress<>0) and (memsize>7) and (messagedlg('At least '+IntToStr(memsize)+' bytes have been allocated at '+IntToHex(dword(baseaddress),8)+#13#10+'Do you want replace the currently selected address with a jump to that address, and copy the overwritten instructions to there?',mtConfirmation,[mbyes,mbno],0)=mryes) then
        CreateCodecave(dword(baseaddress),disassemblerview.SelectedAddress,memsize)
      else
        messagedlg('At least '+IntToStr(memsize)+' bytes have been allocated at '+IntToHex(dword(baseaddress),8),mtinformation,[mbok],0);


      if messagedlg('Do you want to go there now?',mtConfirmation,[mbyes,mbno],0) = mryes then
        disassemblerview.SelectedAddress:=dword(baseaddress);
    end else raise exception.Create('Error allocating memory!');
  end;
  {$endif}
end;

procedure TMemoryBrowser.Getaddress1Click(Sender: TObject);
var p: pointer;
    s: string;
    ws: widestring;
    pws: pwidechar;
begin
  if inputquery('Get kernel address','Give the name of the function you want to find (Case sensitive,certain words can cause blue screens)',s) then
  begin
    ws:=s;
    pws:=@ws[1];
    p:=GetKProcAddress(pws);

    disassemblerview.SelectedAddress:=dword(p);
  end;
end;

procedure TMemoryBrowser.Findmemory1Click(Sender: TObject);
begin
  search1.Click;
end;

procedure TMemoryBrowser.Assemblycode1Click(Sender: TObject);
var s:string;

begin
  s:='';
  if inputquery('Assembly scan','Input the assembly code to find. wilcards(*) supported.',s) then
  begin
    if s='' then exit;
    with TfrmDisassemblyscan.create(self) do
    begin
      startaddress:=disassemblerview.SelectedAddress;
      stringtofind:=s;
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
  selectedaddress: dword;
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
{$ifndef net}
var x: TPluginfunctionType1;
address: dword;
{$endif}
begin
{$ifndef net}
  x:=TPluginfunctionType1(tmenuitem(sender).Tag);
  if x<>nil then
  begin
    address:=disassemblerview.TopAddress;
    x.callback(@address,@disassemblerview.SelectedAddress,@memoryaddress);
    disassemblerview.TopAddress:=address;
    refreshmb;
  end;
{$endif}
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
var a: integer;
    i,j: integer;
    acr: dword;
    bt:byte;

    temp:string;
    hadselection: boolean;
begin
  if button=mbright then
  begin
    if selecting or selectionmade then exit;
  end;

  selecting:=button=mbleft;

  hadselection:=selectionmade;
  selectionmade:=false;

  fcontrol2.SetFocus;
  hexedit.Visible:=false;
  textedit.Visible:=false;

//find out if the user clicked on a byte, and if so which one
  a:=20+mbimage.Canvas.TextWidth('00400000');
  part:=0; //address

  if x>a then
  begin  //didnt click on the address
    part:=2; //textfield ad the left side
    if x<(a+20*8*rows8) then //byteclick
    begin
      if displaytype<>dtByte then
      begin
        selecting:=false; //not for this type
        exit;
      end;
      
      part:=1; //a byte
      srow:=(y div textheight);
      scolumn:=(x-a) div 20;

      i:=(srow*8*rows8)+scolumn;
      //caption:=Inttohex(memoryaddress+i,8);
      selected:=memoryaddress+i;

    end
    else
    begin
      //not on a byte click
      for i:=1 to 8*rows8 do
        temp:=temp+'D';

      srow:=y div textheight;


      i:=(x-(1+a+20*8*rows8));

      scolumn:=i div mbcanvas.Canvas.TextWidth('D');

      if scolumn<8*rows8 then
      begin
        i:=scolumn*mbcanvas.Canvas.TextWidth('D');
        selected:=scolumn+memoryaddress+(8*rows8)*srow;

      end;
    end;
  end;

  selected2:=selected;

  if hadselection then RefreshMB;
end;

procedure TMemoryBrowser.Cut1Click(Sender: TObject);
var i: dword;
    start,stop: dword;
    x: byte;
    br: dword;
    s: string;

    cl: tclipboard;
begin
  if selected>selected2 then
  begin
    start:=selected2;
    stop:=selected;
  end
  else
  begin
    start:=selected;
    stop:=selected2;
  end;

  s:='';
  for i:=start to stop do
  begin
    if readprocessmemory(processhandle,pointer(i),@x,1,br) then
    begin
      s:=s+inttohex(x,2)+' ';
    end
    else s:=s+'?? ';
  end;

  if s<>'' then
    s:=copy(s,1,length(s)-1);

  cl:=tclipboard.Create;
  cl.AsText:=s;
  cl.Free;
end;

procedure TMemoryBrowser.Pastefromclipboard1Click(Sender: TObject);
var cl: tclipboard;
    s: string;
    i: integer;
    x,bw: dword;
    b: tbytes;
begin
  cl:=tclipboard.Create;
  s:=cl.AsText;
  cl.free;


  setlength(b,0);
  try
    ConvertStringToBytes(s,true,b);
    x:=selected;
    for i:=0 to length(b)-1 do
    begin
      if b[i]<>-1 then
        writeprocessmemory(processhandle,pointer(x),@b[i],1,bw);

      inc(x);
    end;
  except

  end;

  setlength(b,0);
  refreshmb;
end;

procedure TMemoryBrowser.Setsymbolsearchpath1Click(Sender: TObject);
var searchpath: string;
begin
{$ifndef net}
  if symhandler.isloaded then
  begin
    searchpath:=symhandler.getsearchpath;
    if inputquery('Symbol handler','Please specify the new symbol searchpath (; seperates paths)',searchpath) then
    begin
      symhandler.setsearchpath(searchpath);

      symhandler.reinitialize;

      symhandler.waitforsymbolsloaded;

    end;
  end;
{$endif}
end;

procedure TMemoryBrowser.Kernelmodesymbols1Click(Sender: TObject);
begin
{$ifndef net}

  Kernelmodesymbols1.Checked:=not Kernelmodesymbols1.Checked;

  symhandler.kernelsymbols:=Kernelmodesymbols1.Checked;
  symhandler.reinitialize;
  symhandler.waitforsymbolsloaded;
{$endif}
end;

procedure TMemoryBrowser.Breakandtraceinstructions1Click(Sender: TObject);
begin
//  if debugger<>nil then
{$ifndef net}

  frmtracer:=TFrmTracer.create(self);
  frmtracer.show;
{$endif}
end;

procedure TMemoryBrowser.debuggerpopupPopup(Sender: TObject);
var x: dword;
begin
  Breakandtraceinstructions1.Enabled:=processhandle<>0;
  ogglebreakpoint1.Enabled:=processhandle<>0;
  Changestateofregisteratthislocation1.Enabled:=processhandle<>0;
  follow1.visible:=isjumporcall(disassemblerview.SelectedAddress,x);
  back1.Visible:=backlist.Count>0;

  pluginhandler.handledisassemblerContextPopup(disassemblerview.SelectedAddress);
end;

procedure TMemoryBrowser.GDTlist1Click(Sender: TObject);
begin
  Tfrmgdtinfo.create(self).show;
end;

procedure TMemoryBrowser.IDTlist1Click(Sender: TObject);
begin
  TfrmIDT.create(self).show;
end;

procedure TMemoryBrowser.ScriptEngine1Click(Sender: TObject);
var x: tfrmautoinject;
begin
  x:=tfrmautoinject.create(self);
  x.cplusplus:=true;
  x.show;
end;

procedure TMemoryBrowser.FormDestroy(Sender: TObject);
var h0,h1,h2,h3: integer;
begin
  if strace<>nil then
    strace.free;

  disassemblerHistory.free;
  memorybrowserHistory.free;
  assemblerHistory.free;

  //save position of window and other stuff
  //membrowser comes after formsettings so is destroyed before formsettings, so valid
  if (not ischild) then
  begin
    saveformposition(self,[
                            disassemblerview.getheaderwidth(0),
                            disassemblerview.getheaderwidth(1),
                            disassemblerview.getheaderwidth(2),
                            disassemblerview.getheaderwidth(3),
                            panel1.height,
                            registerview.width
                    ]);   
  end;

end;

procedure TMemoryBrowser.Newwindow1Click(Sender: TObject);
begin
  with tmemorybrowser.create(nil) do
  begin
    inc(mbchildcount);
    name:='MemoryBrowser'+inttostr(mbchildcount);
    debug1.Visible:=false;
    //registerview.Visible:=false;
    //splitter2.Visible:=false;
    sbShowFloats.Visible:=false;
    caption:=caption+'* ('+inttostr(mbchildcount)+')';

    ischild:=true;
    show;
  end;
end;


procedure TMemoryBrowser.Follow1Click(Sender: TObject);
{
will change the selected disassembler address to the address this instructions jump so if it is an jump instruction
}
var address: dword;
begin
  if isjumporcall(disassemblerview.SelectedAddress,address) then
  begin
    backlist.Push(pointer(disassemblerview.SelectedAddress));
    disassemblerview.SelectedAddress:=address;
  end;
end;



procedure TMemoryBrowser.CopyBytesAndOpcodesClick(Sender: TObject);
var a,b: dword;
    _tag: integer;
begin
  _tag:=(sender as tmenuitem).Tag;

  with tfrmSavedisassembly.create(self) do
  begin

    a:=min(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2);
    b:=max(disassemblerview.SelectedAddress, disassemblerview.SelectedAddress2);
    
    disassemble(b); //b gets increased with size of selected instruction
    edit1.Text:=inttohex(a,8);
    edit2.Text:=inttohex(b,8);
    copymode:=true;

    checkbox1.checked:=true;
    checkbox2.checked:=(_tag=0) or (_tag=1);
    checkbox3.checked:=(_tag=0) or (_tag=2);
    
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

procedure TMemoryBrowser.SetCodeAndDataBase;
var modulelist: tstringlist;
    base: dword;
    header: pointer;
    headersize: dword;
    br: dword;
begin
  modulelist:=tstringlist.Create;
  symhandler.getModuleList(modulelist);

  if modulelist.Count>0 then
  begin
    base:=dword(modulelist.Objects[0]);
    getmem(header,4096);
    try
      if readprocessmemory(processhandle,pointer(base),header,4096,br) then
      begin
        headersize:=peinfo_getheadersize(header);
        if headersize=0 then exit;
        
        if headersize>4096 then
        begin
          if headersize>1024*512 then exit;

          freemem(header);
          getmem(header,headersize);
          if not readprocessmemory(processhandle,pointer(base),header,headersize,br) then exit;
        end;

        disassemblerview.SelectedAddress:=base+peinfo_getEntryPoint(header);

        memoryaddress:=base+peinfo_getdatabase(header);
      end;
    finally
      freemem(header);
    end;
  end;
  modulelist.free;
end;

procedure TMemoryBrowser.Back1Click(Sender: TObject);
begin
  if backlist.Count>0 then
    disassemblerview.SelectedAddress:=dword(backlist.pop);
end;

procedure TMemoryBrowser.Showvaluesofstaticaddresses1Click(
  Sender: TObject);
begin
  showvalues:=not showvalues;
end;

procedure TMemoryBrowser.FindwhatThiscodeAccesses(address: dword);
var i: integer;
begin
  //check if the old window exists, if so, mark it as deactivated
  if frmChangedAddresses<>nil then
    frmChangedAddresses.changedlist.color:=clGray;

  //create new window, and leave the old one alive
  frmChangedAddresses:=TfrmChangedAddresses.Create(self);
  
  if (formsettings.cbKdebug.checked) and (debuggerthread=nil) then
  begin
    KDebugger.StartDebugger;  //if it wasn't enabled yet
    try
      KDebugger.SetBreakpoint(address, bt_OnInstruction, 1, bo_FindWhatCodeAccesses);
    except
      on e: exception do
      begin
        freeandnil(frmChangedAddresses);
        raise e;
      end;
    end;
  end
  else
  begin
    //New method:
    if not startdebuggerifneeded then exit;
    if debuggerthread.userisdebugging then raise exception.create('You can''t use this function while you are debugging the application yourself. (Close the memory view window forces a close of manual debugging)');

    if frmChangedAddresses<>nil then
      frmChangedAddresses.changedlist.color:=clGray;

    frmChangedAddresses:=TfrmChangedAddresses.Create(self);

    debuggerthread.Suspend;

    debuggerthread.DRRegs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
    debuggerthread.DRRegs.Dr0:=address;
    debuggerthread.DRRegs.Dr7:=reg0set or reg1set or reg2set or reg3set;

    for i:=0 to length(debuggerthread.threadlist)-1 do
    begin
      suspendthread(debuggerthread.threadlist[i][1]);
      if not setthreadcontext(debuggerthread.threadlist[i][1],debuggerthread.DRRegs) then showmessage('failed 1');
      resumethread(debuggerthread.threadlist[i][1]);
    end;


    debuggerthread.breakpointaddress:=address;
    debuggerthread.breakpointset:=true;

    debuggerthread.Resume;
  end;
  frmChangedAddresses.show;
end;

procedure TMemoryBrowser.Findoutwhataddressesthisinstructionaccesses1Click(
  Sender: TObject);
begin
  findWhatthisCodeAccesses(disassemblerview.SelectedAddress);
end;

procedure TMemoryBrowser.sbShowFloatsClick(Sender: TObject);
var x: tpoint;
z: trect;
begin
  if frmFloatingPointPanel=nil then
    frmFloatingPointPanel:=TfrmFloatingPointPanel.create(self);

  frmFloatingPointPanel.Left:=self.left+self.Width;
  frmFloatingPointPanel.Top:=self.top+(self.ClientOrigin.y-self.top)-(frmFloatingPointPanel.ClientOrigin.y-frmFloatingPointPanel.top);
  frmFloatingPointPanel.ClientHeight:=scrollbox1.Height;
  frmFloatingPointPanel.show;//pop to foreground
end;

procedure TMemoryBrowser.ScriptConsole1Click(Sender: TObject);
begin
  with TfrmCScript.create(self) do
    show;
end;

procedure TMemoryBrowser.DisplayTypeClick(Sender: TObject);
var x: tmenuitem;
begin
//vtByte, vtWord, vtDword, vtDwordDec, vtSingle, vtDouble
  if (sender is TMenuItem) then
  begin
    x:=TMenuItem(sender);
    case x.tag of
      0: DisplayType:=dtByte;
      1: DisplayType:=dtWord;
      2: DisplayType:=dtDword;
      3: DisplayType:=dtDwordDec;
      4: DisplayType:=dtsingle;
      5: DisplayType:=dtDouble;
    end;


    mbimage.Canvas.FillRect(rect(0,0,mbimage.Width,mbimage.Height));
    MBCanvas.Invalidate;
    MBCanvas.Repaint;
    refreshMB;
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

procedure TMemoryBrowser.Watchmemoryallocations1Click(Sender: TObject);
begin
  if processid=0 then raise exception.Create('Please target a process first');
  if (frmMemoryAllocHandler<>nil) and (frmMemoryAllocHandler.hookedprocessid<>processid) then
    freeandnil(frmMemoryAllocHandler);


  if frmMemoryAllocHandler=nil then
  begin
    {if MessageDlg('This function will inject a dll into the target process and hook some memory allocation/free routines. Continue?',mtConfirmation, [mbyes,mbno],0)<>mryes then exit;

    }
    frmMemoryAllocHandler:=TfrmMemoryAllocHandler.Create(self);
  end;

  frmMemoryAllocHandler.Show;
end;

procedure TMemoryBrowser.Continueanddetachdebugger1Click(Sender: TObject);
begin
  if debuggerthread<>nil then
  begin
    debuggerthread.Terminate;
    debuggerthread.continuehow:=wdco_run;   //note: I could also have the debuggerthread suspend itself, and resume it here
    debuggerthread.continueprocess:=true;
    caption:='Memory Viewer - Detaching';
    debuggerthread.WaitFor;
    open_process;
    caption:='Memory Viewer - Detached';
  end;
end;

procedure TMemoryBrowser.Panel2Resize(Sender: TObject);
begin
  mbcanvas.Height:=panel4.Height-2-9;
  mbcanvas.Width:=panel4.Width;

  mbimage.Width:=mbcanvas.Width;
  mbimage.Height:=mbcanvas.Height;

  mbimage.Canvas.FillRect(rect(0,0,mbimage.Width,mbimage.Height));
  MBCanvas.Invalidate;
  MBCanvas.Repaint;
  refreshMB;

  if hexedit.visible or textedit.visible then fcontrol2.SetFocus;
end;

procedure TMemoryBrowser.Panel2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fcontrol2.SetFocus;
end;

procedure TMemoryBrowser.ScrollBox1Resize(Sender: TObject);
begin
  sbShowFloats.Top:=scrollbox1.ClientHeight div 2-sbShowFloats.Height div 2; 
end;

procedure TMemoryBrowser.reloadStacktrace;
var s: pdwordarray;
    x: dword;
    
    i: integer;
    address, bytes, details: string;
    li: tlistitem;
    c: TListcolumn;
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
          c.Caption:='Return Address';
          c.Width:=120;

          c:=lvstacktracedata.Columns.Add;
          c.Caption:='Parameters';
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
          c.Caption:='Address';
          c.Width:=80;
          c:=lvstacktracedata.Columns.Add;
          c.Caption:='DWORD';
          c.Width:=80;
          c:=lvstacktracedata.Columns.Add;
          c.Caption:='Value';
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
          readprocessmemory(processhandle, pointer(lastdebugcontext.Esp),s, FStacktraceSize,x);
          strace.Clear;
          ce_stacktrace(lastdebugcontext.esp, lastdebugcontext.ebp, lastdebugcontext.eip, s,x, strace,false,Nonsystemmodulesonly1.checked or modulesonly1.Checked,Nonsystemmodulesonly1.checked,0);

          lvstacktracedata.Items.Count:=strace.Count;
        finally
          freemem(s);
        end;
      end else
      begin
        lvstacktracedata.Items.Count:=4096 div 4;
      end;

    end;
  finally
    lvStacktraceData.Items.EndUpdate;
  end;
{
  lvStacktraceData.Items.BeginUpdate;
  lvStacktraceData.count:=0;

  lvStacktraceData.Items.EndUpdate;

  trace:=tstringlist.Create;
  getmem(s,FStacktraceSize);
  try
    readprocessmemory(processhandle, pointer(lastdebugcontext.Esp),s, FStacktraceSize,x);
    ce_stacktrace(lastdebugcontext.esp, lastdebugcontext.ebp, lastdebugcontext.eip, s,x, trace,false,Nonsystemmodulesonly1.checked or modulesonly1.Checked,Nonsystemmodulesonly1.checked,0);

    for i:=0 to trace.count-1 do
    begin
      seperatestacktraceline(trace[i], address,bytes,details);
      li:=lvStacktrace.Items.Add;
      li.Caption:=address;
      li.SubItems.Add(bytes);
      li.SubItems.Add(details);
    end;
  finally
    freemem(s);
    trace.free;
  end;}

end;

procedure TMemoryBrowser.Maxstacktracesize1Click(Sender: TObject);
var
  s: string;
begin
  s:=inttostr(stacktraceSize);
  InputQuery('Stacktrace','New size:',s);
  try
    stacktraceSize:=strtoint(s);
    Maxstacktracesize1.Caption:='Max stacktrace size: '+inttostr(stacktracesize);
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

procedure TMemoryBrowser.Referencedstrings1Click(Sender: TObject);
begin
  if (frmDissectCode=nil) or (frmDissectCode.dissectcode=nil) then
  begin
    if MessageDlg('You will need to run the dissect code routine first before this window is usable. Run it now?', mtConfirmation, [mbyes, mbno], 0)=mryes then
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


function TMemoryBrowser.GetReturnaddress: dword;
var haserror: boolean;
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

end;

procedure TMemoryBrowser.Executetillreturn1Click(Sender: TObject);
var x: dword;
begin
  x:=getreturnaddress;
  if x>0 then
  begin
    disassemblerview.SelectedAddress:=x;
    Runtill1.Click;
  end else beep; //not possible
end;

procedure TMemoryBrowser.lvStacktraceDataData(Sender: TObject; Item: TListItem);
var
  value,x: dword;
  a: dword;
  address,bytes,details: string;
  v: TVariableType;
begin
  if stacktrace2.checked then
  begin
    //show frmstacktrace
    if frmStacktrace=nil then
      frmstacktrace:=TfrmStacktrace.Create(self); //should never happen

    if item.Index<frmStacktrace.ListView1.Items.Count then
    begin
      item.Caption:=frmStacktrace.ListView1.Items[item.index].SubItems[2]; //returnaddress
      item.SubItems.Add(frmStacktrace.ListView1.Items[item.index].SubItems[3]); //subitems address
    end;
  end
  else
  if all1.checked then
  begin
    //show for each dword what it is
    a:=lastdebugcontext.Esp+item.Index*4;
    item.Caption:=inttohex(a,8);
    if readprocessmemory(processhandle, pointer(a), @value, sizeof(value),x) then
    begin
      item.SubItems.Add(inttohex(value,8));
      v:=FindTypeOfData(a,@value,sizeof(value));
      case v of
        vtSingle:
          item.SubItems.Add(format('%.4f',[psingle(@value)^]));

        vtPointer:
        begin
          item.SubItems.Add(symhandler.getNameFromAddress(value));
        end;

        else
          item.SubItems.Add(inttostr(value));

      end;
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

end.
