unit frmautoinjectunit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport, LCLProc,
  {$else}
  windows,
  {$endif}
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, MemoryRecordUnit, commonTypeDefs, CustomTypeHandler,
  disassembler, symbolhandler, symbolhandlerstructs, SynEdit, SynHighlighterCpp,
  SynHighlighterAA, LuaSyntax, SynPluginMultiCaret, SynEditSearch, tablist,
  SynGutterBase, SynEditMarks, math, SynEditMiscClasses, SynEditTextBase,
  SynEditTextBuffer, LazSynEditText, SynEditLines, SynEditKeyCmds, betterControls;


type
  TCallbackRoutine=procedure(memrec: TMemoryRecord; script: string; changed: boolean) of object;
  TCustomCallbackRoutine=procedure(ct: TCustomType; script: string; changed: boolean; lua: boolean) of object;

  TPlusSynEdit=class(TSynEdit)
  private
  public
    property SLines: TSynEditStrings read GetTextBuffer;
  end;

  TSynEditPlus = class(TPlusSynEdit)
  published
    // inherited properties
    property Align;
    property Beautifier;
    property BlockIndent;
    property BlockTabIndent;
    property BorderSpacing;
    property Anchors;
    property Constraints;
    property Color;
    property Cursor default crIBeam;
    property OffTextCursor default crDefault;
    property Enabled;
    property Font;
    property Height;
    property Name;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Visible;
    property Width;
    // inherited events
    property OnClick;
    property OnDblClick;
    property OnTripleClick;
    property OnQuadClick;
    property OnDragDrop;
    property OnDragOver;
// ToDo Docking
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnClickLink;
    property OnMouseLink;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
// ToDo Docking
    property OnStartDock;
    property OnStartDrag;
    property OnUTF8KeyPress;
    // TCustomSynEdit properties
    property BookMarkOptions;
    property BorderStyle default bsSingle;
    property ExtraCharSpacing;
    property ExtraLineSpacing;
    property Gutter;
    property RightGutter;
    property HideSelection;
    property Highlighter;
    property InsertCaret;
    property InsertMode;
    property Keystrokes;
    property MouseActions;
    property MouseTextActions;
    property MouseSelActions;
    property Lines;
    property MaxLeftChar;
    property MaxUndo;
    property Options;
    property Options2;
    property MouseOptions;
    property VisibleSpecialChars;
    property OverwriteCaret;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
    property ScrollBars;
    property SelectedColor;
    property IncrementColor;
    property HighlightAllColor;
    property BracketHighlightStyle;
    property BracketMatchColor;
    property FoldedCodeColor;
    property MouseLinkColor;
    property LineHighlightColor;
    property DefaultSelectionMode;
    property SelectionMode;
    property TabWidth;
    property WantTabs;
    // TCustomSynEdit events
    property OnChange;
    property OnChangeUpdating;
    property OnCutCopy;
    property OnPaste;
    property OnClearBookmark;                                                   // djlp 2000-08-29
    property OnCommandProcessed;
    property OnDropFiles;
    property OnGutterClick;
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnShowHint;
    property OnSpecialLineColors; deprecated;
    property OnSpecialLineMarkup;
    property OnStatusChange;
  end;

  TAAScriptTabData=class
  public
    script: string;
    filename: string;
    undogroups: tlist;
    carretpos: Tpoint;
    topline: integer;
    flags: array of TSynEditStringFlags;
  end;

  TBooleanArray = Array of Boolean;

{
The TDisassemblyLine originates from jgoemat  ( http://forum.cheatengine.org/viewtopic.php?t=566415 )
Originally it was just an Object but I changed it to a TObject because I think a
standalone TDisassembler object might be more efficient reducing the amount of
string parsing
}
  TDisassemblyLine = class(TObject)
    Address: ptrUint;                // actual address value
    AddressString: String;           // module+offset if specified
    Comment: String;                 // comment part (second parameter of disassembly)
    OriginalHexBytes : String;       // original hex from disassembly (grouped)
    Code: String;                    // code portion of disassembly
    Size: Integer;                   // number of bytes for this instruction
    Disassembler: TDisassembler;     // The disassembler used to disassemble (free by caller)

    procedure Init(_address: ptrUint; _mi: TModuleInfo);
    procedure Shorten(_newsize: Integer); // if we overran our injection point, change to 'db'
    function IsStarter : Boolean;
    function IsEnder : Boolean;
    function IsValid : Boolean;
    function GetHexBytes : String; // hex bytes with spaces between each byte
    function GetMaskFlags : TBooleanArray;
    constructor create;
    destructor destroy; override;
  end;

  TAOBFind = Object
    Address: ptrUint;               // address where AOB was found
    CodeSize: Integer;              // size of code we will always use
    Size: Integer;
    Bytes: Array of Byte;           // bytes we'll read from memory

    procedure Init(_address: ptrUint; _codesize: Integer);
    function IsMatch(var maskBytes: Array Of Byte; var maskFlags : TBooleanArray; startIndex, endIndex: Integer): Boolean;
  end;

  TScriptMode=(smAutoAssembler, smLua, smGnuAssembler);

  TAutoAssemblerTemplateCallback=procedure(script: TStrings; sender: TObject) of object;
  TAutoAssemblerTemplate=record
                           name: string;
                           m: TAutoAssemblerTemplateCallback;
                           shortcut: TShortCut;
                         end;

  TAutoAssemblerTemplates=array of TAutoAssemblerTemplate;

type

  { TfrmAutoInject }

  TfrmAutoInject = class(TForm)
    aaImageList: TImageList;
    btnExecute: TButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    menuAOBInjection: TMenuItem;
    menuFullInjection: TMenuItem;
    MenuItem1: TMenuItem;
    mi1ByteExceptionJMP: TMenuItem;
    mi14ByteJMP: TMenuItem;
    mi5ByteJMP: TMenuItem;
    N1: TMenuItem;
    miMoveLeft: TMenuItem;
    miMoveRight: TMenuItem;
    miLuaSyntaxCheck: TMenuItem;
    miRenameTab: TMenuItem;
    miReplace: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miRedo: TMenuItem;
    mifindNext: TMenuItem;
    mifindPrevious: TMenuItem;
    miCallLua: TMenuItem;
    miNewWindow: TMenuItem;
    Panel1: TPanel;
    Load1: TMenuItem;
    Panel2: TPanel;
    Panel3: TPanel;
    ReplaceDialog1: TReplaceDialog;
    Save1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Exit1: TMenuItem;
    Assigntocurrentcheattable1: TMenuItem;
    emplate1: TMenuItem;
    Codeinjection1: TMenuItem;
    CheatTablecompliantcodee1: TMenuItem;
    APIHook1: TMenuItem;
    SaveAs1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Coderelocation1: TMenuItem;
    miNewTab: TMenuItem;
    N2: TMenuItem;
    Separator1: TMenuItem;
    Syntaxhighlighting1: TMenuItem;
    TabMenu: TPopupMenu;
    Close1: TMenuItem;
    Inject1: TMenuItem;
    Injectincurrentprocess1: TMenuItem;
    Injectintocurrentprocessandexecute1: TMenuItem;
    miFind: TMenuItem;
    miPaste: TMenuItem;
    miCopy: TMenuItem;
    miCut: TMenuItem;
    miUndo: TMenuItem;
    N6: TMenuItem;
    FindDialog1: TFindDialog;
    undotimer: TTimer;
    View1: TMenuItem;
    AAPref1: TMenuItem;
    procedure btnExecuteClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure Load1Click(Sender: TObject);
    procedure menuAOBInjectionClick(Sender: TObject);
    procedure menuFullInjectionClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure mi1ByteExceptionJMPClick(Sender: TObject);
    procedure miLuaSyntaxCheckClick(Sender: TObject);
    procedure miMoveLeftClick(Sender: TObject);
    procedure miMoveRightClick(Sender: TObject);
    procedure miRenameTabClick(Sender: TObject);
    procedure miReplaceClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure mifindNextClick(Sender: TObject);
    procedure mifindPreviousClick(Sender: TObject);
    procedure miCallLuaClick(Sender: TObject);
    procedure miNewWindowClick(Sender: TObject);
    procedure miRedoClick(Sender: TObject);
    procedure ReplaceDialog1Find(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Codeinjection1Click(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure CheatTablecompliantcodee1Click(Sender: TObject);

    procedure Assigntocurrentcheattable1Click(Sender: TObject);
    procedure APIHook1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure assemblescreenKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Coderelocation1Click(Sender: TObject);
    procedure miNewTabClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TabMenuPopup(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure Syntaxhighlighting1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Injectincurrentprocess1Click(Sender: TObject);
    procedure Injectintocurrentprocessandexecute1Click(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miFindClick(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure AAPref1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
  private
    { Private declarations }

    LoadedFormPosition: boolean;

    AAHighlighter: TSynAASyn;
    CPPHighlighter: TSynCppSyn;
    LuaHighlighter: TSynLuaSyn;

    assemblescreenCaret: TSynPluginMultiCaret;
    assembleSearch: TSynEditSearch;

    newtabCount: integer;
    selectedtab: integer;

    fScriptMode: TScriptMode;
    fCustomTypeScript: boolean;

    shownonce: boolean;

    procedure setluamode(state: boolean);
    procedure setScriptMode(mode: TScriptMode);

    procedure injectscript(createthread: boolean);
    procedure tlistOnTabChange(sender: TObject; oldselection: integer);
    procedure tlistOnTabCreate(sender: TObject; index: integer);
    procedure tlistOnTabDestroy(sender: TObject; index: integer);

    procedure setCustomTypeScript(x: boolean);
    procedure gutterclick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
    procedure assemblescreenchange(sender: TObject);

    procedure CustomTemplateClick(sender: tobject);
    function getIsEditing: boolean;
    function getTabCount: integer;
    procedure setTabCount(count: integer);

    function getTabScript(index: integer): string;
    procedure setTabScript(index: integer; script: string);

  public
    { Public declarations }

    assemblescreen: TSynEditPlus;
    tablist: TTablist;

    editscript: boolean;
    editscript2: boolean;
    memrec: TMemoryRecord;

    customtype: TCustomType;

    callbackroutine: TCallbackroutine;
    CustomTypeCallback: TCustomCallbackroutine;
    injectintomyself: boolean;

    procedure deleteTab(index: integer);

    procedure reloadHighlighterSettings;
    procedure addTemplate(id: integer);
    procedure removeTemplate(id: integer);
    procedure loadfile(filename: string);
    property CustomTypeScript: boolean read fCustomTypeScript write setCustomTypeScript;
    property TabScript[index: integer]: string read getTabScript write setTabScript;
  published
    property ScriptMode: TScriptMode read fScriptMode write setScriptMode;
    property isEditing: boolean read getIsEditing;
    property TabCount: integer read getTabCount write setTabCount;
  end;


procedure generateAPIHookScript(script: tstrings; address: string; addresstogoto: string; addresstostoreneworiginalfunction: string=''; nameextension:string='0'; targetself: boolean=false);
procedure GenerateCodeInjectionScript(script: tstrings; addressstring: string; farjmp: boolean=false; jmp1:boolean=false);
procedure GenerateAOBInjectionScript(script: TStrings; address: string; symbolname: string; commentradius: integer=10; farjmp: boolean=false; jmp1: boolean=false);
procedure GenerateFullInjectionScript(Script: tstrings; address: string; commentradius: integer=10; farjmp: boolean=false; jmp1: boolean=false);

function registerAutoAssemblerTemplate(name: string; m: TAutoAssemblerTemplateCallback; shortcut: TShortCut=0): integer;
procedure unregisterAutoAssemblerTemplate(id: integer);

function GetUniqueAOB(mi: TModuleInfo; address: ptrUint; codesize: Integer; var resultOffset: Integer) : string;
function GetNextAllocNumber(script: tstrings): integer;
procedure AddSnapshotAsComment(script: tstrings; address: ptruint; radius: integer=10);

procedure GetOriginalInstruction(var address: ptruint; instructioncode: tstrings; farjmp: boolean; skipsymbols: boolean=FALSE);

procedure ReloadAllAutoInjectHighlighters;

implementation


uses frmAAEditPrefsUnit,MainUnit,memorybrowserformunit,APIhooktemplatesettingsfrm,
  Globals, Parsers, MemoryQuery, {$ifdef windows}GnuAssembler,{$endif} LuaCaller, SynEditTypes, CEFuncProc,
  StrUtils, types, ComCtrls, LResources, NewKernelHandler, MainUnit2, Assemblerunit,
  autoassembler,  registry, luahandler, memscan, foundlisthelper, ProcessHandlerUnit,
  frmLuaEngineUnit, frmSyntaxHighlighterEditor, lua, lualib, lauxlib, LuaClass,
  LuaForm, SymbolListHandler, contexthandler;

resourcestring
  rsExecuteScript = 'Execute script';
  rsLuaFilter = 'LUA Script (*.LUA)|*.LUA|All Files ( *.* )|*.*';
  rsLUAScript = 'LUA Script';
  rsGNUAScript = 'GNU Assembler Script';
  rsWriteCode = 'Write code';
  rsCEAFilter = strCheatEngine+' Assembly (*.CEA)|*.CEA|All Files ( *.* )|*.*';
  rsCEGAFilter = strCheatEngine+' GNU Assembly (*.CEGA)|*.CEGA|All Files ( *.* )|*.*';
  rsAutoAssembler = 'Auto assembler';
  rsCodeNeedsEnableAndDisable = 'The code needs an [ENABLE] and a [DISABLE] section if you want to use this script as a table entry';
  rsNotAllCodeIsInjectable = 'Not all code is injectable.'#13#10'%s'#13#10'Are you sure you want to edit it to this?';
  rsCodeInjectTemplate = 'Code inject template';
  rsOnWhatAddressDoYouWantTheJump = 'On what address do you want the jump?';
  rsFailedToAddToTableNotAllCodeIsInjectable = 'Failed to add to table. Not all code is injectable';
  rsStartAddress = 'Start address';
  rsCodeRelocationTemplate = 'Code relocation template';
  rsEndAddressLastBytesAreIncludedIfNecesary = 'End address (last bytes are included if necessary)';
  rsAreYouSureYouWantToClose = 'Are you sure you want to close %s ?';
  rsWhatIdentifierDoYouWantToUse = 'What do you want to name the symbol for the injection point?';
  rsThumbInstructionsAreNotYetImplemented = 'Thumb instructions are not yet implemented';
  rsScript1 = 'Script 1';
  rsScript = 'Script ';
  rsFailedAllocatingMemoryForTheScript = 'Failed allocating memory for the script';
  rsFailedWritingTheScriptToTheProcess = 'failed writing the script to the process';
  rsFailureLoadingUndercdll = 'Failure loading undercdll';
  rsFailedCreatingCallingStubForScriptLocatedAtAddress = 'Failed creating calling stub for script located at address ';
  rsERRORCouldNotFindUniqueAOBTriedCode = 'ERROR: Could not find unique AOB, tried code "';
  rsErrorInScript = 'Error in script %s : %s';
  rsErrorInScriptNoTab = 'Error in script : %s';
  rsEverythingOk = 'Everything ok';
  rsRenameTab = 'Rename tab';
  rsNewNameQuestion = 'What should the new name be?';
  rsDescribeThatThisCodeWillDisableTheScript = 'code from here till the end of the code will be used to disable the cheat';
  rsDescribeThatThisCodeWillEnableTheScript = 'code from here to ''[DISABLE]'' will be used to enable the cheat';
  rsAADescribeAllocatedMemory = 'this is allocated memory, you have read,write'
    +',execute access';
  rsPlaceYourCodeHere = 'place your code here';
  rsAAAOBTemplate_Game = 'Game';
  rsAAAOBTemplate_Version = 'Version';
  rsAAAOBTemplate_Date = 'Date';
  rsAAAOBTemplate_Author = 'Author';
  rsAAAOBTemplate_blabla = 'This script does blah blah blah';
  rsOriginalCode = 'Original code';
  rsLessThan2GBDistance = '<2GB Distance';
  rsMoreThan2GBDistance = '>2GB Distance';
  rs5ByteJMP = '5 Byte JMP (<2GB Distance)';
  rs14ByteJMP = '14 Byte JMP (>2GB Distance)';

var
  AutoAssemblerTemplates: TAutoAssemblerTemplates;


procedure ReloadAllAutoInjectHighlighters;
var
  i: integer;
  f: TCustomForm;
  aif: TfrmAutoInject absolute f;
begin
  for i:=0 to screen.FormCount-1 do
  begin
    f:=screen.Forms[i];
    if f is TfrmAutoInject then
      aif.reloadHighlighterSettings;
  end;

end;

function registerAutoAssemblerTemplate(name: string; m: TAutoAssemblerTemplateCallback; shortcut: TShortCut=0): integer;
var i: integer;
begin
  //find a spot in the current list
  result:=-1;
  for i:=0 to length(AutoAssemblerTemplates)-1 do
    if not assigned(AutoAssemblerTemplates[i].m) then
    begin
      AutoAssemblerTemplates[i].name:=name;
      AutoAssemblerTemplates[i].m:=m;
      AutoAssemblerTemplates[i].shortcut:=shortcut;
      result:=i;
      break;
    end;

  if result=-1 then
  begin
    i:=length(AutoAssemblerTemplates);
    setlength(AutoAssemblerTemplates,i+1);
    AutoAssemblerTemplates[i].name:=name;
    AutoAssemblerTemplates[i].m:=m;
    AutoAssemblerTemplates[i].shortcut:=shortcut;
    result:=i;
  end;

  //check for open autoassembler windows
  for i:=0 to screen.FormCount-1 do
  begin
    if screen.Forms[i] is TfrmAutoInject then
      TfrmAutoInject(screen.Forms[i]).addTemplate(result);
  end;
end;

procedure unregisterAutoAssemblerTemplate(id: integer);
var i: integer;
begin
  if id<length(AutoAssemblerTemplates) then
  begin
    //check for open autoassembler windows
    for i:=0 to screen.FormCount-1 do
    begin
      if screen.Forms[i] is TfrmAutoInject then
        TfrmAutoInject(screen.Forms[i]).removeTemplate(id);
    end;

    CleanupLuaCall(TMethod(AutoAssemblerTemplates[id].m));

    AutoAssemblerTemplates[id].name:='';
    AutoAssemblerTemplates[id].m:=nil;
  end;
end;

procedure TfrmAutoInject.removeTemplate(id: integer);
var i: integer;
begin
  for i:=emplate1.Count-1 downto 0 do
    if emplate1.Items[i].Tag=id+1 then
      emplate1.Items[i].Free;

end;

procedure TfrmAutoInject.addTemplate(id: integer);
var
  mi: TMenuItem;
  t: TAutoAssemblerTemplate;
begin
  if id<length(AutoAssemblerTemplates) then
  begin
    t:=AutoAssemblerTemplates[id];
    if assigned(t.m) then
    begin
      if ScriptMode=smAutoAssembler then
      begin
        mi:=TMenuItem.create(MainMenu1);
        mi.Caption:=t.name;
        mi.Tag:=id+1;
        mi.OnClick:=CustomTemplateClick;
        mi.ShortCut:=t.shortcut;
        emplate1.Add(mi);
      end;
    end;
  end;
end;

procedure TfrmAutoInject.CustomTemplateClick(sender: tobject);
var
  i: integer;
  t: TAutoAssemblerTemplate;
begin
  if sender is TMenuItem then
  begin
    i:=TMenuItem(sender).Tag-1;
    if i<length(AutoAssemblerTemplates) then
    begin
      t:=AutoAssemblerTemplates[i];
      if assigned(t.m) then
        t.m(assemblescreen.Lines, self);
    end;
  end;
end;

procedure TfrmAutoInject.setCustomTypeScript(x: boolean);
begin
  fCustomTypeScript:=x;
  if x then
    editscript:=true;
end;

procedure TfrmAutoInject.setScriptMode(mode: TScriptMode);
begin
  fScriptMode:=mode;
  case mode of
    smLua:
    begin
      if assemblescreen<>nil then
        assemblescreen.Highlighter:=LuaHighlighter;

      //change gui to lua style
      btnExecute.Caption:=rsExecuteScript;
      opendialog1.DefaultExt:='LUA';
      opendialog1.Filter:=rsLuaFilter;
      savedialog1.DefaultExt:='LUA';
      savedialog1.Filter:=rsLuaFilter;
      Assigntocurrentcheattable1.visible:=false;
      emplate1.Visible:=false;
      caption:=rsLUAScript;
     // inject1.Visible:=true;
      helpcontext:=19; //c-script help

      Syntaxhighlighting1.visible:=true;

      miLuaSyntaxCheck.visible:=true;
    end;

    smAutoAssembler:
    begin
      if assemblescreen<>nil then
        assemblescreen.Highlighter:=AAHighlighter;


      //change gui to autoassembler style
      btnExecute.caption:=rsWriteCode;
      opendialog1.DefaultExt:='CEA';
      opendialog1.Filter:=rsCEAFilter;
      savedialog1.DefaultExt:='CEA';
      savedialog1.Filter:=rsCEAFilter;
      Assigntocurrentcheattable1.Visible:=true;
      emplate1.Visible:=processhandler.SystemArchitecture=archX86;
      caption:=rsAutoAssembler;
      inject1.Visible:=false;
      helpcontext:=18; //auto asm help

      Syntaxhighlighting1.visible:=true;
    end;

    smGnuAssembler:
    begin
      if assemblescreen<>nil then
        assemblescreen.Highlighter:=nil; //no highlighter for it yet

      btnExecute.Caption:=rsWriteCode;
      opendialog1.DefaultExt:='CEGA';
      opendialog1.Filter:=rsCEGAFilter;
      savedialog1.DefaultExt:='CEGA';
      savedialog1.Filter:=rsCEGAFilter;
      Assigntocurrentcheattable1.visible:=true; //yup
      emplate1.Visible:=false; //no templates right now
      caption:=rsGNUAScript;

      Syntaxhighlighting1.visible:=false;
    end;

  end;
end;

procedure TfrmAutoInject.setluamode(state: boolean);
begin

end;


procedure TfrmAutoInject.btnExecuteClick(Sender: TObject);
var
    a,b,i: integer;

    disableinfo: TDisableInfo;


    //variables for injectintomyself:
    check: boolean;
    errmsg: string;

    sl: TStringlist;
begin
{$ifndef standalonetrainerwithassembler}
  disableinfo:=TDisableInfo.create;

  case scriptmode of
    smlua:
    begin
      //execute
      LUA_DoScript(assemblescreen.Text);
      modalresult:=mrok; //not modal anymore, but can still be used to pass info
      if editscript2 or CustomTypeScript then close;
    end;

    smAutoAssembler:
    begin
      if editscript then
      begin

        //check if both scripts are valid before allowing the edit

        getenableanddisablepos(assemblescreen.Lines,a,b);
        if not CustomTypeScript then
          if (a=-1) and (b=-1) then raise exception.create(rsCodeNeedsEnableAndDisable);



        try
          check:=autoassemble(assemblescreen.lines,false,true,true,injectintomyself,disableinfo,memrec) and
                 autoassemble(assemblescreen.lines,false,false,true,injectintomyself,disableinfo,memrec);

          if not check then
            errmsg:=format(rsNotAllCodeIsInjectable,['']);
        except
          on e: exception do
          begin
            check:=false;
            errmsg:=format(rsNotAllCodeIsInjectable,['('+e.Message+')']);
          end;
        end;

        if check then
        begin
          modalresult:=mrok; //not modal anymore, but can still be used to pass info
          if editscript2 or CustomTypeScript then close; //can only be used when not modal
        end
        else
        begin
          if messagedlg(errmsg, mtWarning, [mbyes, mbno], 0)=mryes then
          begin
            modalresult:=mrok; //not modal anymore, but can still be used to pass info
            if editscript2 or CustomTypeScript then close;
          end;
        end;
      end
      else
      begin
        try
          disableinfo.ccodesymbols.name:='AA Single Execute';

          autoassemble(assemblescreen.lines,true,true,false,false,disableinfo);
          if disableinfo.ccodesymbols.count>0 then
          begin
            sl:=tstringlist.create;
            disableinfo.ccodesymbols.GetSymbolList(sl);
            if MessageDlg('The following C-Code symbols where registered:'+sl.text+#13#10+'Do you wish to keep these?',mtConfirmation, [mbyes,mbno],0)=mryes then
            begin
              disableinfo.ccodesymbols.refcount:=0;
              disableinfo.donotfreeccodedata:=true; //has to be manually deleted

              //the sourcecode lines will stay. You've lost the ability to free this code anyhow
            end;

            sl.free;
          end;

        except
          on e:exception do
            MessageDlg(e.message,mtError,[mbOK],0);
        end;
      end;
    end;


    smGnuAssembler:
    begin
      {$ifdef windows}
      GnuAssemble(assemblescreen.lines);
      {$endif}

    end;

  end;

  disableinfo.free;
{$endif}
end;

procedure TfrmAutoInject.FormDropFiles(Sender: TObject; const FileNames: array of String);
var load: boolean;
begin
  if length(filenames)=0 then exit;

  if mainform.editedsincelastsave then
    load:=MessageDlg('Your last changes will be lost if you proceed. Continue?',mtConfirmation,[mbyes,mbno],0,mbNo)=mryes
  else
    load:=true;

  if load then
    loadfile(FileNames[0]);
end;

procedure TfrmAutoInject.loadFile(filename: string);
begin
  assemblescreen.Lines.Clear;
  assemblescreen.Lines.LoadFromFile(filename{$if FPC_FULLVERSION >= 030200}, true{$endif});
  savedialog1.FileName:=filename;
  assemblescreen.AfterLoadFromFile;

  case ScriptMode of
    smAutoAssembler: caption:=rsAutoAssembler+':'+extractfilename(opendialog1.FileName);
    smLua: caption:=rsLUAScript+':'+extractfilename(opendialog1.FileName);
    smGnuAssembler: caption:=rsGNUAScript+':'+extractfilename(opendialog1.FileName);
  end;
end;

procedure TfrmAutoInject.Load1Click(Sender: TObject);
begin
  if opendialog1.Execute then
    loadFile(opendialog1.filename);
end;

procedure TfrmAutoInject.mifindNextClick(Sender: TObject);
begin
  finddialog1.Options:=finddialog1.Options+[frFindNext];
  finddialog1.OnFind(finddialog1);
end;

procedure TfrmAutoInject.mifindPreviousClick(Sender: TObject);
begin

  // Reverse Search Direction
  if (frDown in finddialog1.Options) then
  begin
    finddialog1.Options:=finddialog1.Options-[frDown];
  end
  else
  begin
    finddialog1.Options:=finddialog1.Options+[frDown];
  end;

  finddialog1.Options:=finddialog1.Options+[frFindNext];
  finddialog1.OnFind(finddialog1);

  // Change Search Direction back to original
  if (frDown in finddialog1.Options) then
  begin
    finddialog1.Options:=finddialog1.Options-[frDown];
  end
  else
  begin
    finddialog1.Options:=finddialog1.Options+[frDown];
  end;

end;



procedure TfrmAutoInject.miNewWindowClick(Sender: TObject);
var f: TfrmAutoInject;
begin
  f:=TfrmAutoInject.Create(application);
  f.scriptmode:=ScriptMode;

  f.show;
end;

procedure TfrmAutoInject.miRedoClick(Sender: TObject);
begin
  assemblescreen.Redo;
end;

procedure TfrmAutoInject.ReplaceDialog1Find(Sender: TObject);
var so: TSynSearchOptions;
begin
  so:=[];
  if not (frDown in ReplaceDialog1.Options) then
    so:=so+[ssoBackwards];

  if (frEntireScope in ReplaceDialog1.Options) then
    so:=so+[ssoEntireScope];

  if (frMatchCase in ReplaceDialog1.Options) then
    so:=so+[ssoMatchCase];

  if (frPromptOnReplace in ReplaceDialog1.Options) then
    so:=so+[ssoPrompt];

  if (frFindNext in ReplaceDialog1.Options) then
    so:=so+[ssoFindContinue];

  if (frWholeWord in ReplaceDialog1.Options) then
    so:=so+[ssoWholeWord];

 { if assemblescreen.SelAvail then
    so:=so+[ssoSelectedOnly];   }

  assemblescreen.SearchReplace(ReplaceDialog1.FindText,'',so);
end;

procedure TfrmAutoInject.ReplaceDialog1Replace(Sender: TObject);
var so: TSynSearchOptions;
begin
  so:=[];
  if not (frDown in ReplaceDialog1.Options) then
    so:=so+[ssoBackwards];

  if (frEntireScope in ReplaceDialog1.Options) then
    so:=so+[ssoEntireScope];

  if (frMatchCase in ReplaceDialog1.Options) then
    so:=so+[ssoMatchCase];

  if (frPromptOnReplace in ReplaceDialog1.Options) then
    so:=so+[ssoPrompt];

  if (frReplace in ReplaceDialog1.Options) then
    so:=so+[ssoReplace];

  if (frReplaceAll in ReplaceDialog1.Options) then
    so:=so+[ssoReplaceAll];

  if (frFindNext in ReplaceDialog1.Options) then
    so:=so+[ssoFindContinue];

  if (frWholeWord in ReplaceDialog1.Options) then
    so:=so+[ssoWholeWord];

  if assemblescreen.SelAvail then
    so:=so+[ssoSelectedOnly];

  assemblescreen.SearchReplace(ReplaceDialog1.FindText,ReplaceDialog1.ReplaceText,so);
end;

procedure TfrmAutoInject.Save1Click(Sender: TObject);
var f: tfilestream;
    s: string;
begin
  if (savedialog1.filename='') and (not savedialog1.Execute) then exit;   //filename was empty and the user clicked cancel

  f:=tfilestream.Create(savedialog1.filename,fmcreate);
  try
    s:=assemblescreen.text;
    f.Write(s[1],length(assemblescreen.text));

    assemblescreen.MarkTextAsSaved;


    case ScriptMode of
      smAutoAssembler: caption:=rsAutoAssembler+':'+extractfilename(savedialog1.FileName);
      smLua: caption:=rsLUAScript+':'+extractfilename(savedialog1.FileName);
      smGnuAssembler: caption:=rsGNUAScript+':'+extractfilename(savedialog1.FileName);
    end;

    OpenDialog1.FileName:=SaveDialog1.FileName;

  finally
    f.Free;
  end;
end;

procedure TfrmAutoInject.Exit1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmAutoInject.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
{$ifndef standalonetrainerwithassembler}

  if not editscript then
  begin
    if self<>MainForm.frmLuaTableScript then //don't free the lua table script
      action:=cafree;
  end
  else
  begin
    try
      if editscript2 then
      begin
        //call finish routine with script

        if modalresult=mrok then
          callbackroutine(memrec, assemblescreen.text,true)
        else
          callbackroutine(memrec, assemblescreen.text,false);

        action:=cafree;
      end
      else
      if CustomTypeScript then
      begin

        if modalresult=mrok then
          CustomTypeCallback(customtype, assemblescreen.text,true,scriptmode=smLua)
        else
          CustomTypeCallback(customtype, assemblescreen.text,false,scriptmode=smLua);

        action:=cafree;
      end;

    except
      on e: exception do
      begin
        modalresult:=mrNone;
        raise exception.create(e.message);
      end;
    end;
  end;
{$endif}
end;

function canBeUsedAsAddressStorage(regname: string; out actualregname: string): boolean;
//Function to convert a register into a base register
begin
  regname:=lowercase(regname);
  result:=false;
  if (regname='rax') or (regname='eax') then  //no lower types like ah,al, as reads on these do not clear the upper bits
  begin
    actualregname:='rax';
    exit(true);
  end;

  if (regname='rbx') or (regname='ebx') then
  begin
    actualregname:='rbx';
    exit(true);
  end;

  if (regname='rcx') or (regname='ecx') then
  begin
    actualregname:='rcx';
    exit(true);
  end;

  if (regname='rdx') or (regname='edx') then
  begin
    actualregname:='rdx';
    exit(true);
  end;

  if (regname='rsi') or (regname='esi') then
  begin
    actualregname:='rsi';
    exit(true);
  end;

  if (regname='rdi') or (regname='edi') then
  begin
    actualregname:='rdi';
    exit(true);
  end;

  if (regname='rbp') or (regname='ebp') then
  begin
    actualregname:='rbp';
    exit(true);
  end;

  if (regname='rsp') or (regname='esp') then
  begin
    actualregname:='rsp';
    exit(true);
  end;

  if (regname='r8') or (regname='r8d') then
  begin
    actualregname:='r8';
    exit(true);
  end;

  if (regname='r9') or (regname='r9d') then
  begin
    actualregname:='r9';
    exit(true);
  end;

  if (regname='r10') or (regname='r10d') then
  begin
    actualregname:='r10';
    exit(true);
  end;

  if (regname='r11') or (regname='r11d') then
  begin
    actualregname:='r11';
    exit(true);
  end;

  if (regname='r12') or (regname='r12d') then
  begin
    actualregname:='r12';
    exit(true);
  end;

  if (regname='r13') or (regname='r13d') then
  begin
    actualregname:='r13';
    exit(true);
  end;

  if (regname='r14') or (regname='r14d') then
  begin
    actualregname:='r14';
    exit(true);
  end;

  if (regname='r15') or (regname='r15d') then
  begin
    actualregname:='r15';
    exit(true);
  end;
end;

procedure GetOriginalInstruction(var address: ptruint; instructioncode: tstrings; farjmp: boolean; skipsymbols: boolean=false);
var
  d: TDisassembler;

  i: integer;
  addressSpecifierIndexPos: integer;
  addressSpecifierString: string;

  paramsplit: array of string;
  usedreg, usedreg2: string;
  temps: string;

  rewritten: boolean;
  commapos: integer;
begin
  d:=TDisassembler.create;
  if skipsymbols then
    d.showsymbols:=false
  else
    d.showsymbols:=symhandler.showsymbols;

  d.showmodules:=symhandler.showmodules;
  d.showsections:=symhandler.showsections;
  d.disassemble(address);

  addressSpecifierIndexPos:=d.LastDisassembleData.parameters.IndexOf('[');
  commapos:=d.LastDisassembleData.parameters.IndexOf(',');

  if (processhandler.SystemArchitecture=archX86) and (farjmp) and (d.LastDisassembleData.riprelative>0) and (addressSpecifierIndexPos<>-1 ) then
  begin
    //needs a rewrite
    rewritten:=false;

    instructioncode.add('//'+rsOriginalCode+':'+d.LastDisassembleData.opcode+' '+d.LastDisassembleData.parameters);
    addressSpecifierString:=d.LastDisassembleData.parameters.Substring(d.LastDisassembleData.parameters.IndexOf('[')+1);
    addressSpecifierString:=addressSpecifierString.Substring(0, addressSpecifierString.IndexOf(']'));



    //check if it's a read, and if so, which reg is overwritten. Perhaps it can be used as a temp reg
    if (addressSpecifierIndexPos>0) and (d.LastDisassembleData.parameters.Contains(',')) then
    begin
      //read op
      paramsplit:=d.LastDisassembleData.parameters.Split(',');
      if length(paramsplit)=2 then
      begin
        paramsplit[0]:=trim(paramsplit[0]);

        if d.LastDisassembleData.opcode='lea' then
        begin
          //lea r8,[address] = mov r8,address , which can be encoded fully
          instructioncode.add('mov '+paramsplit[0]+','+addressSpecifierString);
          rewritten:=true;
        end
        else
        if canBeUsedAsAddressStorage(paramsplit[0], usedreg) then //e.g movss xmm1,[address] is a big no here
        begin
          instructioncode.add('mov '+usedreg+','+addressSpecifierString);
          instructioncode.add(d.LastDisassembleData.opcode+' '+d.LastDisassembleData.parameters.Replace(addressSpecifierString,usedreg));
          rewritten:=true;
        end;
      end;
    end;

    if rewritten=false then
    begin
      //not a write, or formatted in a way not handled
      usedReg:='';
      usedReg2:='';
      if not (d.LastDisassembleData.parameters.Contains('rax') or
         d.LastDisassembleData.parameters.Contains('eax') or
         d.LastDisassembleData.parameters.Contains('ax') or
         d.LastDisassembleData.parameters.Contains('ah') or
         d.LastDisassembleData.parameters.Contains('al'))
      then
        usedReg:='rax'
      else
      if not (d.LastDisassembleData.parameters.Contains('rbx') or
         d.LastDisassembleData.parameters.Contains('ebx') or
         d.LastDisassembleData.parameters.Contains('bx') or
         d.LastDisassembleData.parameters.Contains('bh') or
         d.LastDisassembleData.parameters.Contains('bl'))
      then
      begin
        if usedReg='' then
          usedreg:='rbx'
        else
          usedreg2:='rbx';
      end
      else
      if not (d.LastDisassembleData.parameters.Contains('rcx') or
         d.LastDisassembleData.parameters.Contains('ecx') or
         d.LastDisassembleData.parameters.Contains('cx') or
         d.LastDisassembleData.parameters.Contains('ch') or
         d.LastDisassembleData.parameters.Contains('cl'))
      then
      begin
        if usedReg='' then
          usedreg:='rcx'
        else
          usedreg2:='rcx';
      end
      else
      if not (d.LastDisassembleData.parameters.Contains('rdx') or
         d.LastDisassembleData.parameters.Contains('edx') or
         d.LastDisassembleData.parameters.Contains('dx') or
         d.LastDisassembleData.parameters.Contains('dh') or
         d.LastDisassembleData.parameters.Contains('dl'))
      then
      begin
        if usedReg='' then
          usedreg:='rdx'
        else
          usedreg2:='rdx';
      end
      else
      if not d.LastDisassembleData.parameters.Contains('r8') then
      begin
        if usedReg='' then   //impossible...
          usedreg:='r8'
        else
          usedreg2:='r8';
      end;

      if d.LastDisassembleData.parameters.Contains('rsp')=false then
      begin
        instructioncode.add('push '+usedreg);
        instructioncode.add('mov '+usedreg+','+addressSpecifierString);
        instructioncode.add(d.LastDisassembleData.opcode+' '+d.LastDisassembleData.parameters.Replace(addressSpecifierString,usedreg));
        instructioncode.add('pop '+usedreg);
        rewritten:=true;
      end
      else
      begin
        //uses RSP: e.g: mov [address],rsp
        if usedreg2<>'' then
        begin
          instructioncode.add('push '+usedreg);
          instructioncode.add('push '+usedreg2);
          instructioncode.add('mov '+usedreg2+',rsp');
          instructioncode.add('add '+usedreg2+',10');

          instructioncode.add('mov '+usedreg+','+addressSpecifierString);

          temps:=d.LastDisassembleData.parameters.Replace(addressSpecifierString,usedreg); //mov [usedreg],rsp  (rsp=-10)
          temps:=temps.replace('rsp',usedreg2); //mov [usedreg],usedreg2  (usedreg2=rsp+10=originalrsp)

          instructioncode.add(d.LastDisassembleData.opcode+' '+temps);
          instructioncode.add('pop '+usedreg2);
          instructioncode.add('pop '+usedreg);

          rewritten:=true;
        end;
      end;
    end;
  end
  else  //no change needed
    instructioncode.add(d.LastDisassembleData.opcode+' '+d.LastDisassembleData.parameters);

  d.free;
end;

procedure GenerateCodeInjectionScript(script: tstrings; addressstring: string; farjmp: boolean=false; jmp1: boolean=false);
function inttostr(i:int64):string;
begin
  if i=0 then result:='' else result:=sysutils.IntToStr(i);
end;

var
    originalcode: tstringlist;
    originalbytes: array of byte;
    codesize: integer;
    a: ptrUint;
    br: ptruint;

    c: ptrUint;
    x: string;
    i,j,k: integer;
    injectnr: integer;

    enablepos: integer;
    disablepos: integer;
    enablecode: tstringlist;
    disablecode: tstringlist;
    jmpsize: integer;

    rewrite: tstringlist;

begin

  if not processhandler.is64Bit then
    farjmp:=false;



  if jmp1 then
    jmpsize:=1
  else
    jmpsize:=ifthen(farjmp, 14, 5);

  try
    a:=StrToQWordEx('$'+addressstring);
  except
    a:=symhandler.getaddressfromname(addressstring);
  end;

  c:=a;

  injectnr:=GetNextAllocNumber(script);

  //disassemble the old code
  codesize:=0;

  originalcode:=tstringlist.create;
  enablecode:=tstringlist.Create;
  disablecode:=tstringlist.Create;

  while codesize<jmpsize do
  begin
    GetOriginalInstruction(c, originalcode, farjmp);
    codesize:=c-a;
  end;

  setlength(originalbytes,codesize);
  ReadProcessMemory(processhandle, pointer(a), @originalbytes[0], codesize, br);


  try
    with enablecode do
    begin
      if processhandler.is64bit and (not farjmp) then
        add('alloc(newmem'+inttostr(injectnr)+',2048,'+addressstring+') ')
      else
        add('alloc(newmem'+inttostr(injectnr)+',2048)');
      add('label(returnhere'+inttostr(injectnr)+')');
      add('label(originalcode'+inttostr(injectnr)+')');
      add('label(exit'+inttostr(injectnr)+')');
      add('');
      add('newmem'+inttostr(injectnr)+': //'+rsAADescribeAllocatedMemory);
      add('//'+rsPlaceYourCodeHere);

      add('');
      add('originalcode'+inttostr(injectnr)+':');
      for i:=0 to originalcode.Count-1 do
        add(originalcode[i]);
      add('');
      add('exit'+inttostr(injectnr)+':');
      add('jmp returnhere'+inttostr(injectnr)+'');

      add('');
      add(addressstring+':');
      if jmp1 then
        add('jmp1 newmem')
      else
      begin
        if farjmp then
          add('jmp far newmem'+inttostr(injectnr)+'')
        else
          add('jmp newmem'+inttostr(injectnr)+'');
      end;

      if codesize>jmpsize then
      begin
        if codesize-jmpsize>1 then
          add('nop '+inttohex(codesize-jmpsize,1))
        else
          add('nop');
      end;

      add('returnhere'+inttostr(injectnr)+':');
      add('');
    end;

    with disablecode do
    begin
      add('dealloc(newmem'+inttostr(injectnr)+')');
      add(addressstring+':');
      x:='db';
      for i:=0 to length(originalbytes)-1 do
        x:=x+' '+inttohex(originalbytes[i],2);
      add(x);

      for i:=0 to originalcode.count-1 do
      begin
        add('//'+originalcode[i]);
      end;

    end;

    getenableanddisablepos(script,enablepos,disablepos);
    //skip first comment(s)
    if enablepos>=0 then
    begin
      while enablepos<script.Count-1 do
      begin
        if pos('//',trim(script[enablepos+1]))=1 then inc(enablepos) else break;
      end;
    end;

    for i:=enablecode.Count-1 downto 0 do
      script.Insert(enablepos+1,enablecode[i]);

    getenableanddisablepos(script,enablepos,disablepos);
    //skip first comment(s)
    if disablepos>=0 then
    begin
      while disablepos<script.Count-1 do
      begin
        if pos('//',trim(script[disablepos+1]))=1 then inc(enablepos) else break;
          inc(disablepos);
      end;
      //only if there actually is a disable section place this code
      for i:=disablecode.Count-1 downto 0 do
        script.Insert(disablepos+1,disablecode[i]);
    end;
  finally
    enablecode.free;
    disablecode.Free;
    originalcode.free;
  end;

end;

procedure TfrmAutoInject.Codeinjection1Click(Sender: TObject);
var
  a: ptruint;
  mi: TModuleInfo;
  address: string;
begin
  if parent is TMemoryBrowser then
    a:=TMemoryBrowser(parent).disassemblerview.SelectedAddress
  else
    a:=memorybrowser.disassemblerview.SelectedAddress;

  if symhandler.getmodulebyaddress(a,mi) then
    address:='"'+mi.modulename+'"+'+inttohex(a-mi.baseaddress,1)
  else
    address:=symhandler.getNameFromAddress(a);

  if processhandler.is64Bit and mi5ByteJMP.checked and (FindFreeBlockForRegion(a,4096)=nil) then
    mi14ByteJMP.Checked:=true;


  if inputquery(rsCodeInjectTemplate, rsOnWhatAddressDoYouWantTheJump, address) then
    GenerateCodeInjectionScript(assemblescreen.lines, address, (ssCtrl in GetKeyShiftState) or mi14ByteJMP.checked, mi1ByteExceptionJMP.checked);
end;

procedure TfrmAutoInject.Panel1Resize(Sender: TObject);
begin

end;


procedure TfrmAutoInject.CheatTablecompliantcodee1Click(Sender: TObject);
var e,d: integer;
begin
{$ifndef standalonetrainerwithassembler}

  getenableanddisablepos(assemblescreen.lines,e,d);

  if e=-1 then //-2 is 2 or more, so bugged, and >=0 is has one
  begin
    assemblescreen.Lines.Insert(0,'[ENABLE]');
    assemblescreen.Lines.Insert(1, '//'+rsDescribeThatThisCodeWillEnableTheScript);
    assemblescreen.Lines.Insert(2,'');
  end;

  if d=-1 then
  begin
    assemblescreen.Lines.Add(' ');
    assemblescreen.Lines.Add(' ');
    assemblescreen.Lines.Add('[DISABLE]');
    assemblescreen.Lines.Add('//'+rsDescribeThatThisCodeWillDisableTheScript);
  end;
{$endif}
end;

procedure TfrmAutoInject.assemblescreenChange(Sender: TObject);
begin
  if self=mainform.frmLuaTableScript then
    mainform.editedsincelastsave:=true;
end;


procedure TfrmAutoInject.Assigntocurrentcheattable1Click(Sender: TObject);
var
  a,b: integer;
  di: TDisableInfo;
begin

  getenableanddisablepos(assemblescreen.Lines,a,b);
  if (a=-1) and (b=-1) then raise exception.create(rsCodeNeedsEnableAndDisable);

  di:=TDisableInfo.create;
  if autoassemble(assemblescreen.lines,true,true,true,false,di) and
     autoassemble(assemblescreen.lines,true,false,true,false,di) then
  begin
    //add a entry with type 255
    mainform.AddAutoAssembleScript(assemblescreen.text);
  end
  else showmessage(rsFailedToAddToTableNotAllCodeIsInjectable);

  di.free;

end;



procedure generateAPIHookScript(script: tstrings; address: string; addresstogoto: string; addresstostoreneworiginalfunction: string=''; nameextension:string='0'; targetself: boolean=false);
var originalcode: array of string;
    originaladdress: array of ptrUint;
    i,j: integer;
    codesize: integer;
    a,b,c: ptrUint;
    br: ptruint;
    x: string;

    enablepos,disablepos: integer;
    disablescript: tstringlist;
    enablescript: tstringlist;

    originalcodebuffer: Pbytearray;
    ab: TAssemblerBytes;

    jumpsize: integer;
    tempaddress: ptrUint;

    specifier: array of ptrUint;
    specifiernr: integer;
    s,s2: string;

    d: TDisassembler;

    originalcodestart: integer;

    isThumbOrigin: boolean;
    isThumbDestination: boolean;

    oldhandle: thandle;
    oldsymhandler: TSymHandler;
    processhandle: THandle;
    ProcessID: DWORD;
begin
  if targetself then
  begin
    //get this function to use the symbolhandler that's pointing to CE itself and the self processid/handle
    oldhandle:=processhandlerunit.ProcessHandle;
    processid:=getcurrentprocessid;
    processhandle:=getcurrentprocess;
    oldsymhandler:=symhandler;
    symhandler:=selfsymhandler;
    processhandler.processhandle:=processhandle;
  end
  else
  begin
    processhandle:=processhandler.processhandle;
  end;

  try

    //disassemble the old code
    d:=TDisassembler.Create;
    d.showmodules:=false;
    d.showsymbols:=false;
    d.showsections:=false;

    setlength(specifier,0);
    setlength(originalcode,0);
    setlength(ab,0);
    specifiernr:=0;


    try
      a:=symhandler.getAddressFromName(address);
    except
      on e: exception do
        raise exception.create(address+':'+e.message);
    end;

    try
      b:=symhandler.getAddressFromName(addresstogoto);
    except
      on e: exception do
        raise exception.create(addresstogoto+':'+e.message);
    end;

    if processhandler.SystemArchitecture=archarm then
    begin
      isThumbOrigin:=(a and 1)=1; //assuming that a name is used and not the real address it occurs on
      isThumbDestination:=(b and 1)=1;

      if isThumbOrigin or isThumbDestination then
        raise exception.create('The thumb instruction set is not yet suppported');


      jumpsize:=8;
      c:=ptruint(FindFreeBlockForRegion(a,2048));
      if (c>0) and (abs(integer(c-a))<31*1024*1024) then
        jumpsize:=4; //can be done with one instruction B <a>
    end
    else
    begin
      if processhandler.is64bit then
      begin
        //check if there is a region I can make use of for a jump trampoline
        if FindFreeBlockForRegion(a,2048)=nil then
        begin
          Assemble('jmp '+inttohex(b,8),a,ab);
          jumpsize:=length(ab);
        end
        else
          jumpsize:=5;
      end
      else
        jumpsize:=5;
    end;



    disablescript:=tstringlist.Create;
    enablescript:=tstringlist.Create;

    codesize:=0;
    b:=a;
    while codesize<jumpsize do
    begin
      setlength(originalcode,length(originalcode)+1);
      setlength(originaladdress,length(originalcode));

      originaladdress[length(originaladdress)-1]:=a;
      originalcode[length(originalcode)-1]:=d.disassemble(a,x);
      i:=posex('-',originalcode[length(originalcode)-1]);
      i:=posex('-',originalcode[length(originalcode)-1],i+1);
      originalcode[length(originalcode)-1]:=copy(originalcode[length(originalcode)-1],i+2,length(originalcode[length(originalcode)-1]));

      codesize:=a-b;
    end;


    getmem(originalcodebuffer,codesize);
    if ReadProcessMemory(processhandle,pointer(b), originalcodebuffer, codesize, br) then
    begin
      disablescript.Add(address+':');
      x:='db';

      for i:=0 to br-1 do
        x:=x+' '+inttohex(originalcodebuffer[i],2);

      disablescript.Add(x);
    end;

    freememandnil(originalcodebuffer);
    originalcodebuffer:=nil;




    with enablescript do
    begin
      if (processhandler.SystemArchitecture=archx86) and (not processhandler.is64bit) then
        add('alloc(originalcall'+nameextension+',2048)')
      else
      begin
        add('alloc(originalcall'+nameextension+',2048,'+address+')');
        add('alloc(jumptrampoline'+nameextension+',64,'+address+') //special jump trampoline in the current region (64-bit)');

        if processhandler.SystemArchitecture=archx86 then
          add('label(jumptrampoline'+nameextension+'address)');
      end;

      add('label(returnhere'+nameextension+')');
      add('');
      if addresstostoreneworiginalfunction<>'' then
      begin
        add(addresstostoreneworiginalfunction+':');
        if processhandler.is64Bit then
          add('dq originalcall'+nameextension)
        else
          add('dd originalcall'+nameextension);
      end;
      add('');
      add('originalcall'+nameextension+':');

      originalcodestart:=enablescript.Count;

      for i:=0 to length(originalcode)-1 do
      begin
        {if hasAddress(originalcode[i], tempaddress, nil ) then
        begin
          if InRangeX(tempaddress, b,b+codesize) then
          begin
            s2:='specifier'+nameextension+inttostr(specifiernr);
            setlength(specifier,length(specifier)+1);
            specifier[specifiernr]:=tempaddress;

            Insert(0,'label('+s2+')');
            if has4ByteHexString(originalcode[i], s) then //should be yes
            begin
              s:=copy(s,2,length(s)-1);

              originalcode[i]:=StringReplace(originalcode[i],s,s2,[rfIgnoreCase]);
            end;

            inc(specifiernr);
          end;
        end;  }
        add(originalcode[i]);
      end;

      //now find the originalcode line that belongs to the specifier
      inc(originalcodestart,specifiernr);
      for i:=0 to length(specifier)-1 do
      begin
        for j:=0 to length(originaladdress)-1 do
        begin
          if specifier[i]=originaladdress[j] then
          begin
            enablescript[originalcodestart+j]:='specifier'+nameextension+inttostr(i)+':'+enablescript[originalcodestart+j]
          end;
        end;
      end;

      i:=0;

      while i<enablescript.count do
      begin
        j:=pos(':',enablescript[i]);

        if j>0 then
        begin
          s:=enablescript[i];
          s2:=copy(s,j+1,length(s));
          delete(i);
          Insert(i,copy(s,1,j));
          inc(i);
          Insert(i,s2);
        end;

        inc(i);
      end;

      if processhandler.SystemArchitecture=archarm then
        add('b returnhere'+nameextension)
      else
        add('jmp returnhere'+nameextension);

      add('');

      if processhandler.systemarchitecture=archarm then
      begin
        add('jumptrampoline'+nameextension+':');
        if isThumbDestination then
        begin
          raise exception.create(rsThumbInstructionsAreNotYetImplemented);
          if isThumbOrigin then
          begin
            add('thumb:b '+addresstogoto);
          end
          else
          begin
            add('bx jumptrampoline_armtothumb+1');
            add('jumptrampoline_armtothumb:');
            add('thumb:bl '+addresstogoto);
            add('thumb:bx jumptrampoline_thumbtoarm');
            add('jumptrampoline_thumbtoarm');
            add('bx lr');
          end;
        end
        else
          add('b '+addresstogoto);

      end
      else
      if processhandler.is64bit then
      begin
        add('jumptrampoline'+nameextension+':');
        add('jmp [jumptrampoline'+nameextension+'address]');
        add('jumptrampoline'+nameextension+'address:');
        add('dq '+addresstogoto);
        add('');
      end;


      add(address+':');

      if processhandler.SystemArchitecture=archarm then
      begin
        add('B jumptrampoline'+nameextension);
      end
      else
      begin
        if processhandler.is64bit then
          add('jmp jumptrampoline'+nameextension)
        else
          add('jmp '+addresstogoto);

        while codesize>jumpsize do
        begin
          add('nop');
          dec(codesize);
        end;
      end;

      add('returnhere'+nameextension+':');

      add('');
    end;


    getenableanddisablepos(script,enablepos,disablepos);

    if disablepos<>-1 then
    begin
      with disablescript do
      begin
        add('dealloc(originalcall'+nameextension+')');
        if processhandler.is64bit then
          add('dealloc(jumptrampoline)');
      end;

      for i:=0 to disablescript.Count-1 do
        script.Insert(disablepos+i+1,disablescript[i]);
    end;

    getenableanddisablepos(script,enablepos,disablepos); //idiots putting disable first

    if enablepos<>-1 then
    begin
      for i:=0 to enablescript.Count-1 do
        script.Insert(enablepos+i+1,enablescript[i]);
    end
    else
      script.AddStrings(enablescript);

    disablescript.free;
    enablescript.free;

    d.free;

  finally
    if targetself then
    begin
      processhandler.processhandle:=oldhandle;
      symhandler:=oldsymhandler;
    end;
  end;
end;



procedure TfrmAutoInject.APIHook1Click(Sender: TObject);
function inttostr(i:int64):string;
begin
  if i=0 then result:='' else result:=sysutils.IntToStr(i);
end;

var address: string;

    a: ptrUint;
    x: string;
    i,j,k: integer;

    injectnr: integer;

begin
  if parent is TMemoryBrowser then
    a:=TMemoryBrowser(parent).disassemblerview.SelectedAddress
  else
    a:=memorybrowser.disassemblerview.SelectedAddress;

  address:=inttohex(a,8);

  with tfrmapihooktemplatesettings.create(self) do
//  if inputquery('Give the address of the api you want to hook',address) and inputquery('Give the address of the replacement function',address) then
  begin
    try
      injectnr:=0;
      for i:=0 to assemblescreen.Lines.Count-1 do
      begin
        j:=pos('alloc(newmem',lowercase(assemblescreen.lines[i]));
        if j<>0 then
        begin
          x:=copy(assemblescreen.Lines[i],j+12,length(assemblescreen.Lines[i]));
          x:=copy(x,1,pos(',',x)-1);
          try
            k:=strtoint(x);
            if injectnr<=k then
              injectnr:=k+1;
          except
            inc(injectnr);
          end;
        end;
      end;

      edit1.text:=address;
      if showmodal<>mrok then exit;


      generateAPIHookScript(assemblescreen.Lines,edit1.Text, edit2.Text, edit3.Text, inttostr(injectnr));


    finally
      free;
    end;
  end;

end;

procedure TfrmAutoInject.SaveAs1Click(Sender: TObject);
begin
  if savedialog1.Execute then
    save1.Click;
end;

procedure TfrmAutoInject.FormShow(Sender: TObject);
var
  reg: Tregistry;

  preferedwidth, preferedheight: integer;
begin
  if shownonce=false then
  begin
    if overridefont<>nil then
      assemblescreen.Font.assign(overridefont)
    else
      assemblescreen.Font.Size:=10;

    reg:=tregistry.create;
    try
      if reg.OpenKey('\Software\'+strCheatEngine+'\Auto Assembler\',false) then
      begin
        if reg.valueexists('Font.name') then
          assemblescreen.Font.Name:=reg.readstring('Font.name');

        if reg.valueexists('Font.size') then
          assemblescreen.Font.size:=reg.ReadInteger('Font.size');

        if reg.valueexists('Font.quality') then
          assemblescreen.Font.quality:=TFontQuality(reg.ReadInteger('Font.quality'));
      end;
    finally
      reg.free;
    end;

    if LoadedFormPosition=false then
    begin
      preferedwidth:=assemblescreen.CharWidth*50+assemblescreen.Gutter.Width;
      preferedheight:=assemblescreen.LineHeight*8+panel1.height;

      if clientwidth<preferedwidth then
        clientwidth:=preferedwidth;

      if clientheight<preferedheight then
        clientheight:=preferedheight;
    end;

    shownonce:=true;
  end;

  if editscript then
    btnExecute.Caption:=strOK;

  assemblescreen.SetFocus;
end;

procedure TfrmAutoInject.assemblescreenKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
{   if (ssCtrl in Shift) and (key=ord('A'))  then
   begin
     TMemo(Sender).SelectAll;
     Key := 0;
   end; }
end;

procedure TfrmAutoInject.miCallLuaClick(Sender: TObject);
var
  luaserverinit: tstringlist;
  i: integer;

  needsinit1: boolean;
begin
  needsinit1:=true;

  for i:=0 to assemblescreen.Lines.Count-1 do
    if trim(assemblescreen.lines[i])='luacall(openLuaServer(''CELUASERVER''))' then
      needsinit1:=false;

  if needsinit1 then
  begin
    luaserverinit:=tstringlist.create;
    if processhandler.is64bit then
      luaserverinit.add('loadlibrary(luaclient-x86_64.dll)')
    else
      luaserverinit.add('loadlibrary(luaclient-i386.dll)');


    luaserverinit.add('luacall(openLuaServer(''CELUASERVER''))');

    luaserverinit.add('');
    luaserverinit.add('CELUA_ServerName:');
    luaserverinit.add('db ''CELUASERVER'',0');
    luaserverinit.add('');

    luaserverinit.add('{');
    luaserverinit.add('//luacall call example:');
    if processhandler.is64bit then
    begin
      luaserverinit.add('//Make sure rsp is aligned on a 16-byte boundary when calling this function');
      luaserverinit.add('mov rcx, addresstostringwithfunction //(The lua function will have access to the variable passed by name "parameter")');
      luaserverinit.add('mov rdx, integervariableyouwishtopasstolua');
      luaserverinit.add('sub rsp,20');
      luaserverinit.add('call CELUA_ExecuteFunction // or CELUA_ExecuteFunctionAsync if you don''t need GUI access or want to handle it yourself');
      luaserverinit.add('add rsp,20');
      luaserverinit.add('');
      luaserverinit.add('//------');
      luaserverinit.add('//Alternate call by ref example:');
      luaserverinit.add('');
      luaserverinit.add('mov ecx,[addresswithluafunctionidstored]');
      luaserverinit.add('test ecx,ecx');
      luaserverinit.add('jne short hasrefid');
      luaserverinit.add('');
      luaserverinit.add('mov rcx,addresswithluafunctionname');
      luaserverinit.add('call CELUA_GetFunctionReferenceFromName  //Basically calls createRef(functionname) and returns the value');
      luaserverinit.add('mov [addresswithluafunctionidstored],eax');
      luaserverinit.add('mov ecx,eax');
      luaserverinit.add('');
      luaserverinit.add('hasrefid:');
      luaserverinit.add('mov edx,numberofparameterstopass');
      luaserverinit.add('mov r8,addresswithparameterlist  //could be the stack.  e.g lea r8,[rsp+8]');
      luaserverinit.add('mov [r8],param1');
      luaserverinit.add('mov [r8+8],param2');
      luaserverinit.add('mov [r8+c],param3');
      luaserverinit.add('//...');
      luaserverinit.add('mov r9,0 //0=no async, 1=async.  Use async if you do not wish to update the GUI. Faster');
      luaserverinit.add('call CELUA_ExecuteFunctionByReference');
      luaserverinit.add('');
      luaserverinit.add('When done RAX will contain the result of the lua function');
      luaserverinit.add('And as per 64-bit calling convention, RCX, RDX, R8, R9, R10, R11 may have been altered. So save/restore them beforehand');


    end
    else
    begin
      luaserverinit.add('push integervariableyouwishtopasstolua');
      luaserverinit.add('push addresstostringwithfunction  //(The lua function will have access to the variable passed by name "parameter")');
      luaserverinit.add('call CELUA_ExecuteFunction');
      luaserverinit.add('');
      luaserverinit.add('//------');
      luaserverinit.add('//Alternate call by ref example:');
      luaserverinit.add('');
      luaserverinit.add('mov eax,[addresswithluafunctionidstored]');
      luaserverinit.add('test eax,eax');
      luaserverinit.add('jne short hasrefid');
      luaserverinit.add('');
      luaserverinit.add('push addresswithluafunctionname');
      luaserverinit.add('call CELUA_GetFunctionReferenceFromName  //Basically calls createRef(functionname) and returns the value');
      luaserverinit.add('mov [addresswithluafunctionidstored],eax');

      luaserverinit.add('hasrefid:');
      luaserverinit.add('mov [addresswithparameterlist],param1');
      luaserverinit.add('mov [addresswithparameterlist+4],param2');
      luaserverinit.add('mov [addresswithparameterlist+8],param3');
      luaserverinit.add('//...');
      luaserverinit.add('push 0 //0=no async, 1=async.  Use async if you do not wish to update the GUI. Faster');
      luaserverinit.add('push addresswithparameterlist');
      luaserverinit.add('push numberofparameterstopass');
      luaserverinit.add('push eax //push the reference ID of the function');
      luaserverinit.add('call CELUA_ExecuteFunctionByReference');
      luaserverinit.add('');

      luaserverinit.add('When done EAX will contain the result of the lua function');
      luaserverinit.add('And as per common 32-bit calling convention, EDX and ECX could have been altered. So save/restore them beforehand');
    end;

    luaserverinit.add('}');

    for i:=0 to luaserverinit.count-1 do
      assemblescreen.Lines.Insert(0+i, luaserverinit[i]);

    luaserverinit.free;
  end;





end;

procedure TfrmAutoInject.Coderelocation1Click(Sender: TObject);
var starts,stops: string;
    start,stop,current: ptrUint;
    x: ptrUint;
    i,j: integer;

    labels: tstringlist;
    output: tstringlist;
    s: string;

    a,b: string;
    prev: ptrUint;

    ok: boolean;

begin
{$ifndef standalonetrainerwithassembler}
  starts:=inttohex(memorybrowser.disassemblerview.SelectedAddress,8);
  stops:=inttohex(memorybrowser.disassemblerview.SelectedAddress+128,8);

  if inputquery(rsStartAddress+':', rsCodeRelocationTemplate, starts) then
  begin
    start:=StrToQWordEx('$'+starts);
    if inputquery(rsEndAddressLastBytesAreIncludedIfNecesary, rsCodeRelocationTemplate, stops) then
    begin
      stop:=StrToQWordEx('$'+stops);

      output:=tstringlist.Create;
      labels:=tstringlist.create;
      labels.Duplicates:=dupIgnore;
      labels.Sorted:=true;

      output.add('alloc(newmem,'+inttostr(abs(integer(stop-start))*2)+')');
      output.add('');
      output.add('newmem:');


      try
        current:=start;

        while current<stop do
        begin
          prev:=current;
          s:=disassemble(current);
          i:=posex('-',s);
          i:=posex('-',s,i+1);
          s:=copy(s,i+2,length(s));

          i:=pos(' ',s);
          a:=copy(s,1,i-1);
          b:=copy(s,i+1,length(s));


          if length(a)>1 then
          begin
            if (lowercase(a)='loop') or (lowercase(a[1])='j') or (lowercase(a)='call') then
            begin
              try
                x:=symhandler.getAddressFromName(b);
                if (x>=start) and (x<=stop) then
                begin
                  labels.Add('orig_'+inttohex(x,8));
                  s:=a+' orig_'+inttohex(x,8);
                end;
              except
                //nolabel
              end;
            end;
          end;

          output.add('orig_'+inttohex(prev,8)+':');
          output.add(s);
        end;

        labels.Sort;
        //now clean up output so that the result is a readable program
        for i:=0 to labels.Count-1 do
          output.Insert(2+i,'label('+labels[i]+')');

        output.Insert(2+labels.Count,'');

        i:=2+labels.Count+1;
        while i<output.Count do
        begin
          if pos('orig_',output[i])>0 then
          begin
            //determine if it's valid or not
            ok:=false;
            for j:=0 to labels.Count-1 do
              if labels[j]+':'=output[i] then
              begin
                ok:=true;
                break;
              end;

            if not ok then
              output.Delete(i)
            else
            begin
              output.Insert(i,'');
              inc(i,2);
            end;
          end
          else inc(i);
        end;

        assemblescreen.Lines.AddStrings(output);

      finally
        output.free;
      end;

    end;

  end;
{$endif}
end;

function TfrmAutoInject.getTabCount: integer;
begin
  if tablist<>nil then
    result:=tablist.Count
  else
    result:=1;
end;

procedure TfrmAutoInject.setTabCount(count: integer);
begin
  if tablist=nil then
    miNewTab.Click;

  while tablist.Count>count do
    deleteTab(TabCount-1);

  while tablist.count<count do
    miNewTab.Click;

  if count=1 then
    tablist.visible:=false;
end;

function TfrmAutoInject.getTabScript(index: integer): string;
begin
  result:='';
  if index>=0 then
  begin
    if (tablist=nil) and (index=0) then
      exit(assemblescreen.Lines.Text);

    if index<tablist.count then
    begin
      if tablist.SelectedTab=index then
        exit(assemblescreen.Lines.Text)
      else
        exit(TAAScriptTabData(tablist.TabData[index]).script);
    end;
  end;

end;

procedure TfrmAutoInject.setTabScript(index: integer; script: string);
var
  i: integer;
  td: TAAScriptTabData;
begin
  if index>=0 then
  begin
    if (tablist=nil) and (index=0) then
    begin
      assemblescreen.Lines.Text:=script;
      exit;
    end;

    if index<tablist.count then
    begin
      if tablist.SelectedTab=index then
      begin
        assemblescreen.Lines.Text:=script;
        assemblescreen.ClearUndo;
      end
      else
      begin
        td:=TAAScriptTabData(tablist.TabData[index]);
        td.script:=script;
        //clear the undo data for that tab
        if td.undogroups<>nil then
        begin
          for i:=0 to td.undogroups.Count-1 do
            TSynEditUndoGroup(td.undogroups[i]).Free;

          td.undogroups.clear;
        end;
      end;
    end;
  end;
end;

procedure TfrmAutoInject.miNewTabClick(Sender: TObject);
var i: integer;
begin
{$ifndef standalonetrainerwithassembler}
  if miNewTab.visible=false then exit;

  if tablist=nil then
  begin
    tablist:=TTablist.Create(self);
    tablist.height:=20;
    tablist.Align:=alTop;
    tablist.Visible:=false;
    tablist.OnTabChange:=tlistOnTabChange;
    tablist.OnTabCreate:=tlistOnTabCreate;
    tablist.OnTabDestroy:=tlistOnTabDestroy;


    tablist.color:=GetRGBColorResolvingParent; //panel2.color;

    tablist.Parent:=panel2;

    tablist.height:=tablist.Canvas.TextHeight('WwJjDdQq')+4;
    tablist.BorderSpacing.Top:=4;
    tablist.PopupMenu:=TabMenu;


    inc(newtabcount);
    i:=tablist.AddTab(rsScript1);
  end;

  tablist.Visible:=true;

  inc(newtabcount);
  i:=tablist.AddTab(rsScript+inttostr(newtabCount));
  tablist.SelectedTab:=i;
{$endif}
end;

procedure tfrmautoinject.tlistOnTabChange(sender: TObject; oldselection: integer);
var
  undolist:  TSynEditUndoList;
  undoitem: TSynEditUndoItem;
  undogroup: TSynEditUndoGroup;

  ssl: TSynEditStringList;
  sel: TSynEditLines;
  i,j: integer;

  p: tpoint;


  l: tlist;

begin
{$ifndef standalonetrainerwithassembler}

  ssl:=TSynEditStringList(assemblescreen.SLines);

  if oldselection>=0 then
  begin
    TAAScriptTabData(tablist.TabData[oldselection]).script:=assemblescreen.text;
    TAAScriptTabData(tablist.TabData[oldselection]).filename:=opendialog1.FileName;
    TAAScriptTabData(tablist.TabData[oldselection]).topline:=assemblescreen.TopLine;
    p:=assemblescreen.CaretXY;
    TAAScriptTabData(tablist.TabData[oldselection]).carretpos:=p;

    if ssl is TSynEditStringList then
    begin
      if TAAScriptTabData(tablist.TabData[oldselection]).undogroups=nil then
        TAAScriptTabData(tablist.TabData[oldselection]).undogroups:=tlist.Create;

      TAAScriptTabData(tablist.TabData[oldselection]).undogroups.Clear;

      repeat
        undogroup:=ssl.UndoList.PopItem;
        if undogroup<>nil then
          TAAScriptTabData(tablist.TabData[oldselection]).undogroups.Add(undogroup);


      until undogroup=nil;


      //save flags
      setlength(TAAScriptTabData(tablist.TabData[oldselection]).flags, ssl.count);
      for i:=0 to ssl.Count-1 do
        TAAScriptTabData(tablist.TabData[oldselection]).flags[i]:=ssl.Flags[i];
    end;

  end;





  assemblescreen.BeginUpdate(false);
  assemblescreen.text:=TAAScriptTabData(tablist.CurrentTabData).script;
  opendialog1.FileName:=TAAScriptTabData(tablist.CurrentTabData).filename;
  assemblescreen.TopLine:=TAAScriptTabData(tablist.CurrentTabData).topline;
  p:=TAAScriptTabData(tablist.CurrentTabData).carretpos;
  assemblescreen.CaretXY:=p;
  assemblescreen.EndUpdate;

  //restore undo
  assemblescreen.ClearUndo; //get rid of the old one (just in case the previous tabswitch failed...)
  l:=tlist.create;

  if (ssl is TSynEditStringList) and (TAAScriptTabData(tablist.CurrentTabData).undogroups<>nil) then
  begin
    for i:=TAAScriptTabData(tablist.CurrentTabData).undogroups.Count-1 downto 0 do
    begin
      l.clear;
      undogroup:=TAAScriptTabData(tablist.CurrentTabData).undogroups[i];

      repeat
        undoitem:=undogroup.Pop;
        if undoitem<>nil then
          l.add(undoitem)
        else
          undogroup.free;
      until undoitem=nil;

      ssl.UndoList.BeginBlock;
      for j:=l.count-1 downto 0 do
      begin
        undoitem:=l[j];
        ssl.UndoList.AddChange(undoitem);
      end;
      ssl.UndoList.EndBlock;
    end;

    TAAScriptTabData(tablist.CurrentTabData).undogroups.Clear;

    //flags
    for i:=0 to length(TAAScriptTabData(tablist.CurrentTabData).flags)-1 do
       ssl.Flags[i]:=TAAScriptTabData(tablist.CurrentTabData).flags[i];
  end;


  l.free;

{$endif}
end;

procedure tfrmautoinject.tlistOnTabCreate(sender: TObject; index: integer);
begin
  ttablist(sender).TabData[index]:=TAAScriptTabData.Create;
end;

procedure tfrmautoinject.tlistOnTabDestroy(sender: TObject; index: integer);
var d: TAAScriptTabData;
begin
  if ttablist(sender).TabData[index]<>nil then
  begin
    d:=TAAScriptTabData(ttablist(sender).TabData[index]);
    if d.undogroups<>nil then
    begin
      while d.undogroups.Count>0 do
      begin
        TSynEditUndoGroup(d.undogroups[0]).Free;
        d.undogroups.Delete(0);
      end;

      d.undogroups.free;
    end;
    d.free;
    ttablist(sender).TabData[index]:=nil;
  end;
end;


function checkScript(script: string; out errorreason: string): boolean;
var
  r: integer;
begin
  r:=luaL_loadstring(luavm,pchar(script));

  result:=(r=0) and lua_isfunction(luavm,-1);
  if not result then
    errorReason:=lua_tostring(luavm,-1);

  lua_pop(luavm,1);
end;

procedure TfrmAutoInject.miLuaSyntaxCheckClick(Sender: TObject);
var
  i,r: integer;
  s: string;
  hasError: boolean;
begin
  hasError:=false;

  if tablist=nil then
  begin
    if checkscript(assemblescreen.text,s)=false then
    begin
      hasError:=true;
      lua_getglobal(luavm,'print');
      lua_pushstring(LuaVM, format(rsErrorInScriptNoTab, [s]));
      lua_pcall(luavm,1,0,0);
    end;
  end
  else
  begin
    TAAScriptTabData(tablist.CurrentTabData).script:=assemblescreen.text;

    for i:=0 to tablist.count-1 do
    begin
      if checkscript(TAAScriptTabData(tablist.TabData[i]).script, s)=false then
      begin
        hasError:=true;
        s:=format(rsErrorInScript, [tablist.TabText[i], s]);

        lua_getglobal(luavm,'print');
        lua_pushstring(LuaVM, s);
        lua_pcall(luavm,1,0,0);

      end;
    end;
  end;

  if not hasError then
    showMessage(rsEverythingOk); //todo: Get a thumbs up dialog
end;

procedure TfrmAutoInject.miMoveLeftClick(Sender: TObject);
begin
  //has no effect on AA scripts, but for the lua tablescripts it does
  tablist.MoveTabLeft(selectedtab);
end;

procedure TfrmAutoInject.miMoveRightClick(Sender: TObject);
begin
  tablist.MoveTabRight(selectedtab);
end;

procedure TfrmAutoInject.miRenameTabClick(Sender: TObject);
var v: string;
begin
  v:=tablist.TabText[selectedtab];
  if InputQuery(rsRenameTab, rsNewNameQuestion, v) then
    tablist.TabText[selectedtab]:=v;
end;


procedure tfrmAutoInject.gutterclick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
begin
  if assemblescreen.Lines.Count>line then
  begin
    assemblescreen.CaretY:=line;
    assemblescreen.CaretX:=0;
    assemblescreen.SelectLine(true);
  end;
end;



procedure TfrmAutoInject.FormCreate(Sender: TObject);
var
  i: integer;
  x: array of integer;
  reg: tregistry;

  fq: TFontQuality;
begin
  {$ifndef ONEBYTEJUMPS}
  mi1ByteExceptionJMP.visible:=false;
  Separator1.visible:=false;
  {$endif}


  {$ifndef standalonetrainerwithassembler}

  AAHighlighter:=TSynAASyn.Create(self);
  CPPHighlighter:=TSynCppSyn.create(self);
  LuaHighlighter:=TSynLuaSyn.Create(self);
  reloadHighlighterSettings;

  assembleSearch:=TSyneditSearch.Create;


  assemblescreen:=TSynEditPlus.Create(self);
  assemblescreen.BorderStyle:=bsNone;
  assemblescreen.Highlighter:=AAHighlighter;
  assemblescreen.Options:=SYNEDIT_DEFAULT_OPTIONS - [eoScrollPastEol]+[eoTabIndent]+[eoKeepCaretX];
  fq:=assemblescreen.Font.Quality;
  if not (fq in [fqCleartypeNatural, fqDefault]) then
    assemblescreen.Font.quality:=fqDefault;

  //assemblescreen.Font.Quality:=fqDefault;
  assemblescreen.WantTabs:=true;
  assemblescreen.TabWidth:=4;

  assemblescreenCaret:=TSynPluginMultiCaret.Create(assemblescreen);
  assemblescreenCaret.EnableWithColumnSelection:=true;
  assemblescreenCaret.DefaultMode:=mcmMoveAllCarets;
  assemblescreenCaret.DefaultColumnSelectMode:=mcmCancelOnCaretMove;

  assemblescreen.Gutter.MarksPart.Visible:=false;
  assemblescreen.Gutter.Visible:=true;
  assemblescreen.Gutter.LineNumberPart.Visible:=true;
  assemblescreen.Gutter.LeftOffset:=1;
  assemblescreen.Gutter.RightOffset:=1;

  assemblescreen.Align:=alClient;
  assemblescreen.PopupMenu:=PopupMenu1;
  assemblescreen.Parent:=panel2;

  assemblescreen.Gutter.OnGutterClick:=gutterclick;

  assemblescreen.name:='Assemblescreen';
  assemblescreen.Text:='';

  assemblescreen.OnChange:=assemblescreenchange;

  assemblescreen.Color:=colorset.TextBackground;
  assemblescreen.Font.color:=colorset.FontColor;
  assemblescreen.Gutter.Color:=clBtnFace;
  assemblescreen.Gutter.LineNumberPart.MarkupInfo.Background:=clBtnFace;
  assemblescreen.Gutter.SeparatorPart.MarkupInfo.Background:=clBtnFace;



  setlength(x,0);
  LoadedFormPosition:=loadformposition(self,x);

  reg:=tregistry.create;
  try
    if reg.OpenKey('\Software\'+strCheatEngine+'\Auto Assembler\',false) then
    begin
      if reg.valueexists('Show Line Numbers') then
        assemblescreen.Gutter.linenumberpart.visible:=reg.ReadBool('Show Line Numbers');

      if reg.valueexists('Show Gutter') then
        assemblescreen.Gutter.Visible:=reg.ReadBool('Show Gutter');

      if reg.valueexists('smart tabs') then
        if reg.ReadBool('smart tabs') then assemblescreen.Options:=assemblescreen.options+[eoSmartTabs]
                                      else assemblescreen.Options:=assemblescreen.options-[eoSmartTabs];

      if reg.valueexists('tabs to spaces') then
        if reg.ReadBool('tabs to spaces') then assemblescreen.Options:=assemblescreen.options+[eoTabsToSpaces]
                                          else assemblescreen.Options:=assemblescreen.options-[eoTabsToSpaces];

      if reg.valueexists('tab width') then
        assemblescreen.tabwidth:=reg.ReadInteger('tab width');
    end;

  finally
    reg.free;
  end;


  for i:=0 to length(AutoAssemblerTemplates)-1 do
    addTemplate(i);

{$endif}

{$ifdef darwin}
  miCut.ShortCut:=TextToShortCut('Meta+X');
  miCopy.ShortCut:=TextToShortCut('Meta+C');
  miPaste.ShortCut:=TextToShortCut('Meta+V');
  miUndo.ShortCut:=TextToShortCut('Meta+Z');
  miRedo.ShortCut:=TextToShortCut('Shift+Meta+X');
  miFind.ShortCut:=TextToShortCut('Meta+F');
  mifindNext.ShortCutKey2:=TextToShortcut('Meta+G');

  miNewWindow.Shortcut:=TextToShortCut('Meta+N');
  load1.Shortcut:=TextToShortCut('Meta+O');
  save1.Shortcut:=TextToShortCut('Meta+S');

  if assemblescreen<>nil then
  begin
    i:=assemblescreen.Keystrokes.FindCommand(ecSelectAll);
    if i<>-1 then assemblescreen.Keystrokes[i].ShortCut:=TextToShortCut('Meta+A');
  end;

  if processhandler.SystemArchitecture=archArm then emplate1.visible:=false;

{$endif}

end;

procedure TfrmAutoInject.TabMenuPopup(Sender: TObject);
var p: tpoint;
begin
  p:=tablist.ScreenToClient(mouse.CursorPos);
  selectedtab:=tablist.GetTabIndexAt(p.x,p.y);

  miMoveLeft.enabled:=selectedtab>0;
  miMoveRight.enabled:=selectedtab<tablist.Count-1;
end;

procedure TfrmAutoInject.TabControl1Change(Sender: TObject);
begin

end;

procedure TfrmAutoInject.Syntaxhighlighting1Click(Sender: TObject);
begin
{$ifndef standalonetrainerwithassembler}

  Syntaxhighlighting1.checked:=not Syntaxhighlighting1.checked;
  if Syntaxhighlighting1.checked then //enable
  begin
    if ScriptMode=smLua then
      assemblescreen.Highlighter:=LuaHighlighter
    else if ScriptMode=smAutoAssembler then
      assemblescreen.Highlighter:=AAHighlighter
  end
  else //disabl
    assemblescreen.Highlighter:=nil;

{$endif}
end;


procedure TfrmAutoInject.deleteTab(index: integer);
var
  oldtabindex: integer;
begin
  if (tablist<>nil) and (index>=0) and (tablist.count>1) and (index<tablist.count) then
  begin
    oldtabindex:=tablist.SelectedTab;
    TAAScriptTabData(tablist.CurrentTabData).script:=assemblescreen.text;
    TAAScriptTabData(tablist.CurrentTabData).filename:=OpenDialog1.FileName;
    tablist.RemoveTab(index);

    if tablist.SelectedTab=-1 then
    begin
      if tablist.Count>oldtabindex then
        tablist.SelectedTab:=oldtabindex
      else
        tablist.SelectedTab:=tablist.count-1;
    end;

    if tablist.count=1 then
      tablist.Visible:=false;
  end;
end;

procedure TfrmAutoInject.Close1Click(Sender: TObject);
begin
{$ifndef standalonetrainerwithassembler}

  if messagedlg(Format(rsAreYouSureYouWantToClose, [tablist.TabText[selectedtab]]), mtConfirmation, [mbyes, mbno], 0)=mryes then
    deleteTab(selectedTab);
{$endif}
end;

procedure TfrmAutoInject.injectscript(createthread: boolean);
var i: integer;
    setenvscript: tstringlist;
    CEAllocArray: TCEAllocArray;
    callscriptscript: tstringlist;

    totalmem: dword;
    totalwritten: dword;
    address: pointer;
    mi: TModuleInfo;
    hasjustloadedundercdll: boolean;

    aawindowwithstub: tfrmautoinject;
   // setenv_done: dword;
//    setenv_done_value: dword;
    s: string;

    ignore: dword;
    th: thandle;
begin
{$ifndef standalonetrainerwithassembler}
 {
 obsolete
  //this will inject the script dll and generate a assembler script the user can use to call the script
  //first set the environment var for uc_home
  s:=assemblescreen.text;
  if not symhandler.getmodulebyname('undercdll.dll',mi) then
  begin
    //dll was not loaded yet

    setenvscript:=tstringlist.Create;

    with setenvscript do
    begin
      add('[enable]');
      Add('alloc(envname,8)');
      add('alloc(envvar,512)');
      add('alloc(myscript,512)');

      add('envname:');
      add('db ''UC_HOME'',0');
      add('envvar:');
      add('db '''+cheatenginedir+''' ,0');
      add('myscript:');
      add('push envvar');
      add('push envname');
      add('call SetEnvironmentVariableA');
      add('ret');

      //cleanup part:
      add('[disable]');
      add('dealloc(myscript)');
      add('dealloc(envvar)');
      add('dealloc(envname)');
    end;

    setlength(CEAllocArray,1);
    if autoassemble(setenvscript,false,true,false,false,CEAllocArray) then //enabled
    begin
      for i:=0 to length(ceallocarray)-1 do
        if ceallocarray[i].varname='myscript' then
        begin
          th:=createremotethread(processhandle,nil,0,pointer(ceallocarray[i].address),nil,0,ignore);
          if th<>0 then
            waitforsingleobject(th,4000); //4 seconds max

          break;
        end;



      //wait done
      autoassemble(setenvscript,false,false,false,false,CEAllocArray); //disable for the deallocs
    end;

    setenvscript.free;


    injectdll(cheatenginedir+'undercdll.dll','');
    symhandler.reinitialize;
    hasjustloadedundercdll:=true;
  end else hasjustloadedundercdll:=false;

  //now allocate memory for the script and write it to there
  totalmem:=length(assemblescreen.text);
  address:=VirtualAllocEx(processhandle,nil,totalmem+512,mem_commit,page_execute_readwrite);
  if address=nil then raise exception.create(rsFailedAllocatingMemoryForTheScript);
  if not WriteProcessMemory(processhandle,address,@s[1],totalmem,totalwritten) then
    raise exception.create(rsFailedWritingTheScriptToTheProcess);



  callscriptscript:=tstringlist.create;
  try
    with callscriptscript do
    begin
      add('label(result)');
      add(inttohex((ptrUint(address)+totalmem+$20) - (ptrUint(address) mod $10),8)+':');
      add('pushfd');
      add('pushad');
      add('push '+inttohex(ptrUint(address),8));
      add('call underc_executescript');
      add('mov [result],eax');
      add('popad');
      add('popfd');
      add('mov eax,[result]');
      add('ret');
      add('result:');
      add('dd 0');
    end;

    if hasjustloadedundercdll then
    begin
      //lets wait before injecting the callscript script
      symhandler.waitforsymbolsloaded;
      if not symhandler.getmodulebyname('undercdll.dll',mi) then
        raise exception.Create(rsFailureLoadingUndercdll);
    end;
    if not autoassemble(callscriptscript,false,true,false,false,CEAllocArray) then raise exception.Create(rsFailedCreatingCallingStubForScriptLocatedAtAddress+inttohex(ptrUint(address),8));
  finally
    callscriptscript.free;
  end;

  aawindowwithstub:=tfrmautoinject.create(memorybrowser);
  with aawindowwithstub.assemblescreen.Lines do
  begin
    if createthread then
    begin
      add('createthread(myscript)');
      add('alloc(myscript,256)');
      add('myscript:');
    end;

    add('//Call this code to execute the script from assembler');
    add('call '+inttohex((ptrUint(address)+totalmem+$20) - (ptrUint(address) mod $10),8));
    add('');
    add('//eax==0 when successfully executed');
    add('//''call underc_geterror'' to get a pointer to the last generated error buffer');

    if createthread then
      add('ret //interesing thing with createthread is that the return param points to exitthread');
  end;
  aawindowwithstub.show;
     }
{$endif}
end;



procedure TfrmAutoInject.Injectincurrentprocess1Click(Sender: TObject);
begin
  injectscript(false);


end;

procedure TfrmAutoInject.Injectintocurrentprocessandexecute1Click(
  Sender: TObject);
begin
  injectscript(true);
end;

procedure TfrmAutoInject.miCutClick(Sender: TObject);
begin
  assemblescreen.CutToClipboard;
end;

procedure TfrmAutoInject.miCopyClick(Sender: TObject);
begin
  assemblescreen.CopyToClipboard;
end;

procedure TfrmAutoInject.miPasteClick(Sender: TObject);
begin
  assemblescreen.PasteFromClipboard;
end;

procedure TfrmAutoInject.miFindClick(Sender: TObject);
begin
  finddialog1.Options:=finddialog1.Options-[frFindNext];
  if finddialog1.Execute then
  begin
    mifindNext.visible:=true;
    mifindPrevious.visible:=true;
  end;
end;

procedure TfrmAutoInject.FindDialog1Find(Sender: TObject);
begin
  //scan the text for the given text
  ReplaceDialog1.Options:=finddialog1.Options;
  ReplaceDialog1.FindText:=finddialog1.FindText;
  ReplaceDialog1.OnFind(ReplaceDialog1);

  finddialog1.options:=finddialog1.options+[frFindNext];
end;

function TfrmAutoInject.getIsEditing: boolean;
begin
  result:=editscript or editscript2;
end;

//follow is just a emergency fix since undo is messed up. At least it's better than nothing
procedure TfrmAutoInject.AAPref1Click(Sender: TObject);
var reg: tregistry;
begin
  with TfrmAAEditPrefs.create(self) do
  begin
    try
      if execute(assemblescreen) then
      begin
        //save these settings
        reg:=tregistry.create;
        try
          if reg.OpenKey('\Software\'+strCheatEngine+'\Auto Assembler\',true) then
          begin
            reg.WriteString('Font.name', assemblescreen.Font.Name);
            reg.WriteInteger('Font.size', assemblescreen.Font.size);
            reg.WriteInteger('Font.quality', integer(assemblescreen.Font.Quality));



            //assemblescreen.Font.

            reg.WriteBool('Show Line Numbers', assemblescreen.Gutter.linenumberpart.visible);
            reg.WriteBool('Show Gutter', assemblescreen.Gutter.Visible);

            reg.WriteBool('smart tabs', eoSmartTabs in assemblescreen.Options);
            reg.WriteBool('tabs to spaces', eoTabsToSpaces in assemblescreen.Options);
            reg.WriteInteger('tab width', assemblescreen.TabWidth);
          end;

        finally
          reg.free;
        end;
      end;
    finally
      free;
    end;
  end;
end;

procedure TfrmAutoInject.FormDestroy(Sender: TObject);
begin
  //if editscript or editscript2 then
  begin
    saveformposition(self);

  end;
end;

procedure TfrmAutoInject.miUndoClick(Sender: TObject);
begin
  assemblescreen.Undo;
end;

procedure AddSnapshotAsComment(script: TStrings; address: ptruint; radius: integer=10);
var
  i,j: integer;
  a: ptruint;
  s,tmps: string;
  addressString: string;
  bytesString: string;
  opcodeString: string;
  specialString: string;


  addresslist: Tstringlist;
  byteslist: TStringlist;
  codelist: TStringList;
  maxBytesSize: integer;
  addressLinenr: integer;

  d: TDisassembler;

begin
  script.Add('');
  script.Add('{');
  script.Add('// ORIGINAL CODE - INJECTION POINT: ' + symhandler.getNameFromAddress(address) );
  script.Add('');

  maxBytesSize := 0;


  d:=TDisassembler.Create;
  d.showmodules:=symhandler.showmodules;
  d.showsymbols:=symhandler.showsymbols;
  d.showsections:=symhandler.showsections;

  addresslist:=tstringlist.create;
  byteslist:=tstringlist.create;
  codelist:=tstringlist.create;

  try
    a:=address;

    for i:=1 to radius do
      a:=previousopcode(a);

    addressLinenr:=radius; //usually good enough

    for i:=1 to radius*2+1 do
    begin
      addressString:=symhandler.getNameFromAddress(a);
      s:=d.disassemble(a, tmps);
      splitDisassembledString(s,false,tmps, bytesString, opcodeString,specialString);

      bytesstring:='';
      if (a>address) and (d.LastDisassembleData.address<address) then
      begin
        //cut it into bytes
        opcodeString:='db ';

        setlength(d.LastDisassembleData.Bytes,length(d.LastDisassembleData.Bytes)-(a-address));
        for j:=0 to length(d.LastDisassembleData.Bytes)-1 do
        begin
          opcodeString:=opcodeString+inttohex(d.LastDisassembleData.Bytes[j],2)+' ';
          bytesstring:=bytesString+inttohex(d.LastDisassembleData.Bytes[j],2)+' ';
        end;

        a:=address;
      end
      else
      begin
        for j:=0 to length(d.LastDisassembleData.Bytes)-1 do
          bytesstring:=bytesString+inttohex(d.LastDisassembleData.Bytes[j],2)+' ';
      end;



      addressList.add(addressString);
      bytesList.add(bytesstring);
      codeList.add(opcodeString);

      maxBytesSize:=max(length(bytesstring), maxBytesSize);
    end;

    for i:=0 to addresslist.Count-1 do
    begin
      if i = addressLinenr then script.Add('// ---------- INJECTING HERE ----------');
      script.Add(addressList[i] + ': ' + PadRight(bytesList[i],maxBytesSize) + ' - ' + codeList[i]);
      if i = addressLinenr then script.Add('// ---------- DONE INJECTING  ----------');
    end;

    script.Add('}');

  finally
    addresslist.free;
    byteslist.free;
    codelist.free;

    d.free;

  end;
end;

function GetNextAllocNumber(Script: TStrings): integer;
var
  i,j: integer;
  injectnr: integer;
  x: string;
begin
  result:=0;
  for i:=0 to script.Count-1 do
  begin
    x:=lowercase(trim(script[i]));

    if copy(x,1,12)='alloc(newmem' then
    begin
      x:=copy(x,13,pos(',',x)-13);
      try
        if x='' then
        begin
          if result=0 then
            result:=2
        end
        else
        begin
          j:=strtoint(x);
          if result<=j then
            result:=j+1;
        end;
      except
        inc(result);
      end;
    end;
  end;
end;

// \/   http://forum.cheatengine.org/viewtopic.php?t=566415 (jgoemat and some mods by db)
procedure GenerateFullInjectionScript(Script: tstrings; address: string; commentRadius: integer=10; farjmp: boolean=false; jmp1:boolean=false);
var
  originalcode: tstringlist;
  originalbytes: array of byte;
  codesize: integer;
  a: ptrUint;
  br: ptruint;
  c: ptrUint;
  x: string;
  i,j,k: integer;
  injectnr: integer;
  nr: string; // injectnr as string
  aobString: string;
  p: integer;

  enablepos: integer;
  disablepos: integer;
  initialcode: tstringlist;
  enablecode: tstringlist;
  disablecode: tstringlist;

  originalAddress: ptrUint;
  AddressString: string;
  maxBytesSize: integer;
  addressList: tstringlist;
  bytesList: tstringlist;
  codeList: tstringlist;
  startIndex: integer;

  injectFirstLine: Integer;
  injectLastLine: Integer;
  dline: TDisassemblyLine;
  ddBytes: string;

  mi: TModuleInfo;
  jmpsize: integer;
begin
  if not processhandler.is64Bit then
    farjmp:=false;

  if jmp1 then
    jmpsize:=1
  else
    jmpsize:=ifthen(farjmp, 14, 5);

  try
    a:=StrToQWordEx('$'+address);
  except
    a:=symhandler.getaddressfromname(address);
  end;

  ZeroMemory(@mi, sizeof(mi));
  symhandler.getmodulebyaddress(a, mi);


  c:=a;
  injectnr:=GetNextAllocNumber(script);
  if injectnr = 0 then nr := '' else nr := sysutils.IntToStr(injectnr);


  // disassemble the old code, simply for putting original code in the script
  // and for the bytes we assert must be there and will replace
  originalcode:=tstringlist.create;
  codesize:=0;

  while codesize<jmpsize do
  begin
    GetOriginalInstruction(c, originalcode, farjmp);
    codesize:=c-a;
  end;

  setlength(originalbytes,codesize);
  ReadProcessMemory(processhandle, pointer(a), @originalbytes[0], codesize, br);


  // same as menu option "Cheat Engine framework code", make sure we
  // have enable and disable
  getenableanddisablepos(script,enablepos,disablepos);

  if enablepos=-1 then //-2 is 2 or more, so bugged, and >=0 is has one
  begin
    script.Insert(0,'[ENABLE]');
    script.Insert(1,'');
  end;

  if disablepos=-1 then
  begin
    script.Add('[DISABLE]');
    script.Add('');
  end;


  dline:=TDisassemblyLine.create;
  initialcode:=tstringlist.Create;
  enablecode:=tstringlist.Create;
  disablecode:=tstringlist.Create;
  addressList:=tstringlist.Create;
  bytesList:=tstringlist.Create;
  codeList:=tstringlist.Create;

  try
    aobString:='';
    for i:=0 to length(originalbytes)-1 do
    begin
      if i > 0 then
        aobString := aobString + ' ';
      aobString := aobString + inttohex(originalbytes[i], 2);
    end;

    with initialcode do
    begin
      add('define(address' + nr + ',' + address + ')');
      add('define(bytes' + nr + ',' + aobString + ')');
      add('');
    end;

    with enablecode do
    begin
      add('assert(address'+nr+',bytes'+nr+')');
      if processhandler.is64bit and (not farjmp) then
        add('alloc(newmem' + nr + ',$1000,' + address + ')')
      else
        add('alloc(newmem' + nr + ',$1000)');
      add('');
      add('label(code'+nr+')');
      add('label(return'+nr+')');
      add('');
      add('newmem'+nr+':');

      add('');
      add('code'+nr+':');
      for i:=0 to originalcode.count-1 do
        add('  '+originalcode[i]);
      add('  jmp return'+nr+'');

      add('');
      add('address'+nr+':');
      if jmp1 then
        add('  jmp1 newmem')
      else
      begin
        if farjmp then
          add('  jmp far newmem'+nr+'')
        else
          add('  jmp newmem'+nr+'');
      end;

      if codesize>jmpsize then
      begin
        if codesize-jmpsize>1 then
          add('  nop '+inttohex(codesize-jmpsize,1))
        else
          add('  nop');
      end;

      add('return'+nr+':');
      add('');
    end;

    with disablecode do
    begin
      add('address'+nr+':');
      add('  db bytes'+nr);
      for i:=0 to originalcode.count-1 do
        add('  // ' + originalcode[i]);
      add('');
      add('dealloc(newmem'+nr+')');
    end;


    // add initial defines before enable
    getenableanddisablepos(script,enablepos,disablepos);
    p:=0;
    if (enablepos>0) then
      p:=enablepos;
    for i:=initialcode.Count-1 downto 0 do
      script.Insert(p,initialcode[i]);

    // add enable lines before disable
    getenableanddisablepos(script,enablepos,disablepos);
    p:=script.Count-1;
    if(disablepos>0) then
      p:=disablepos;
    for i:=enablecode.Count-1 downto 0 do
      script.Insert(p,enablecode[i]);

    // add disable lines at very end
    for i:=0 to disablecode.Count-1do
      script.Add(disablecode[i]);

    // finally add comment at the beginning
    script.Insert(0,'{ Game   : ' + copy(mainform.ProcessLabel.Caption, pos('-', mainform.ProcessLabel.Caption) + 1, length(mainform.ProcessLabel.Caption)));
    script.Insert(1,'  Version: ');
    script.Insert(2,'  Date   : ' + FormatDateTime('YYYY-MM-DD', Now));
    script.Insert(3,'  Author : ' + UserName);
    script.Insert(4,'');
    script.Insert(5,'  This script does blah blah blah');
    script.Insert(6,'}');
    script.Insert(7,'');

    // now we disassemble quite a bit more code for comments at the
    // bottom so someone can easily find the code again if the game
    // is updated
    addSnapshotAsComment(script, a, commentradius);

  finally
    initialcode.free;
    enablecode.free;
    disablecode.Free;
    addressList.Free;
    bytesList.Free;
    codeList.Free;
    dline.free;

    originalcode.free;
  end;


end;

procedure TfrmAutoInject.menuFullInjectionClick(Sender: TObject);
var
  a: ptruint;
  address: string;
  mi: TModuleInfo;
begin
  a:=memorybrowser.disassemblerview.SelectedAddress;

  if symhandler.getmodulebyaddress(a,mi) then
    address:='"'+mi.modulename+'"+'+inttohex(a-mi.baseaddress,1)
  else
    address:=inttohex(a,8);

  if processhandler.is64Bit and mi5ByteJMP.checked and (FindFreeBlockForRegion(a,4096)=nil) then
    mi14ByteJMP.Checked:=true;

  if inputquery(rsCodeInjectTemplate, rsOnWhatAddressDoYouWantTheJump, address) then
    generateFullInjectionScript(assemblescreen.Lines, address, 10, (ssCtrl in GetKeyShiftState) or mi14ByteJMP.checked, mi1ByteExceptionJMP.checked);
end;

procedure TfrmAutoInject.miReplaceClick(Sender: TObject);
begin
  ReplaceDialog1.execute;
end;

procedure TfrmAutoInject.reloadHighlighterSettings;
begin
  LuaHighlighter.LoadFromRegistry(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\Lua Highlighter'+darkmodestring);
  AAHighlighter.LoadFromRegistry(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\AA Highlighter'+darkmodestring);
  CPPHighlighter.LoadFromRegistry(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\CPP Highlighter'+darkmodestring);
end;

procedure TfrmAutoInject.MenuItem2Click(Sender: TObject);
var
  frmHighlighterEditor: TfrmHighlighterEditor;
begin
  frmHighlighterEditor:=TfrmHighlighterEditor.create(self);
  LuaHighlighter.LoadFromRegistry(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\Lua Highlighter'+darkmodestring);
  frmHighlighterEditor.highlighter:=LuaHighlighter;
  if frmHighlighterEditor.showmodal=mrok then
  begin
    LuaHighlighter.SaveToRegistry(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\Lua Highlighter'+darkmodestring);
    reloadHighlighterSettings;
    ReloadAllLuaEngineHighlighters;
  end;

  frmHighlighterEditor.free;

end;

procedure TfrmAutoInject.MenuItem3Click(Sender: TObject);
var
  frmHighlighterEditor: TfrmHighlighterEditor;
begin
  frmHighlighterEditor:=TfrmHighlighterEditor.create(self);
  AAHighlighter.LoadFromRegistry(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\AA Highlighter'+darkmodestring);
  frmHighlighterEditor.highlighter:=AAHighlighter;
  if frmHighlighterEditor.showmodal=mrok then
  begin
    AAHighlighter.SaveToRegistry(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\AA Highlighter'+darkmodestring);
    ReloadAllAutoInjectHighlighters;
  end;

  frmHighlighterEditor.free;
end;

procedure TfrmAutoInject.MenuItem1Click(Sender: TObject);
var
  frmHighlighterEditor: TfrmHighlighterEditor;
begin
  frmHighlighterEditor:=TfrmHighlighterEditor.create(self);
  CPPHighlighter.loadFromRegistryDefault(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\CPP Highlighter'+darkmodestring);

  frmHighlighterEditor.highlighter:=CPPHighlighter;
  if frmHighlighterEditor.showmodal=mrok then
  begin
    CPPHighlighter.SaveToRegistry(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\CPP Highlighter'+darkmodestring);
    ReloadAllAutoInjectHighlighters;
  end;

  frmHighlighterEditor.free;
end;

procedure TfrmAutoInject.mi1ByteExceptionJMPClick(Sender: TObject);
begin
  if mi1ByteExceptionJMP.checked then
  begin
    mi5ByteJMP.Caption:=rsLessThan2GBDistance;
    mi14ByteJMP.Caption:=rsMoreThan2GBDistance;
  end
  else
  begin
    mi5ByteJMP.Caption:=rs5ByteJMP;
    mi14ByteJMP.Caption:=rs14ByteJMP;
  end;

end;

procedure GenerateAOBInjectionScript(script: TStrings; address: string; symbolname: string; commentradius: integer=10; farjmp: boolean=false; jmp1:boolean=false);
var
  a,a2: ptrUint;                  // pointer to injection point
  originalcode: tstringlist;      // disassembled code we're replacing
  originalbytes: array of byte;   // bytes we're replacing
  codesize: integer;              // # of bytes we're replacing
  aobString: string;              // hex bytes we're replacing
  injectnr: integer;              // # of this injection (multiple can be in 1 script)
  nr: string;                     // injectnr as string

  // lines where [ENABLE] and [DISABLE] are
  enablepos: integer;
  disablepos: integer;

  // temp variables
  br: ptruint;
  c: ptrUint;
  x: string;
  i,j,k: integer;
  p: integer;
  count: integer;

  // lines of code to inject in certain places
  initialcode: tstringlist;
  enablecode: tstringlist;
  disablecode: tstringlist;

  // these are for code in comment at bottom
  maxBytesSize: Integer;
  addressList: TStringList;
  bytesList: TStringList;
  codeList: TStringList;
  ddBytes: String;

  mi: TModuleInfo;            // info on the module
  dline: TDisassemblyLine;    // for disassembling code in the bottom comment
  injectFirstLine: Integer;
  injectLastLine: Integer;
  resultAOB: String;
  resultOffset: Integer;
  symbolNameWithOffset: String;
  jmpsize: integer;
begin
  if not processhandler.is64Bit then
    farjmp:=false;


  if jmp1 then
    jmpsize:=1
  else
    jmpsize:=ifthen(farjmp, 14, 5);


  try
    a:=StrToQWordEx('$'+address);
  except
    a:=symhandler.getaddressfromname(address);
  end;

  mi.baseaddress:=0;
  symhandler.getmodulebyaddress(a,mi);

  c:=a;
  injectnr:=GetNextAllocNumber(script);
  if injectnr = 0 then nr := '' else nr := sysutils.IntToStr(injectnr);


  // disassemble the old code, simply for putting original code in the script
  // and for the bytes we assert must be there and will replace
  originalcode:=tstringlist.create;
  codesize:=0;

  while codesize<jmpsize do
  begin
    GetOriginalInstruction(c, originalcode, farjmp);
    codesize:=c-a;
  end;

  setlength(originalbytes, codesize);
  ReadProcessMemory(processhandle, pointer(a), @originalbytes[0], codesize, br);

  // same as menu option "Cheat Engine framework code", make sure we
  // have enable and disable
  getenableanddisablepos(script,enablepos,disablepos);

  if enablepos=-1 then //-2 is 2 or more, so bugged, and >=0 is has one
  begin
    script.Insert(0,'[ENABLE]');
    script.Insert(1,'');
  end;

  if disablepos=-1 then
  begin
    script.Add('[DISABLE]');
    script.Add('');
  end;

  dline:=TDisassemblyLine.create;
  initialcode:=tstringlist.Create;
  enablecode:=tstringlist.Create;
  disablecode:=tstringlist.Create;
  addressList:=tstringlist.Create;
  bytesList:=tstringlist.Create;
  codeList:=tstringlist.Create;

  try
    //************************************************************************
    //* Now do AOBScan and get name for injection symbol
    //************************************************************************
    resultAOB := GetUniqueAOB(mi, a, codesize, resultOffset);

    if resultOffset <> 0 then
      symbolNameWithOffset := symbolName + '+' + IntToHex(resultOffset, 2)
    else
      symbolNameWithOffset := symbolName;

    aobString:='';
    for i:=0 to length(originalbytes)-1 do
    begin
      if i > 0 then
        aobString := aobString + ' ';
      aobString := aobString + IntToHex(originalbytes[i], 2);
    end;

    with enablecode do
    begin
      if (mi.baseAddress > 0) then
        add('aobscanmodule(' + symbolName + ',' + mi.modulename + ',' + resultAOB + ') // should be unique')
      else
        add('aobscan(' + symbolName + ',' + resultAOB + ') // should be unique');

      if processhandler.is64bit and (not farjmp) then
        add('alloc(newmem' + nr + ',$1000,' + symbolname + ')')
      else
        add('alloc(newmem' + nr + ',$1000)');
      add('');
      add('label(code'+nr+')');
      add('label(return'+nr+')');
      add('');
      add('newmem'+nr+':');

      add('');
      add('code' + nr + ':');
      for i:=0 to originalcode.count - 1 do
        add('  ' + originalcode[i]);
      add('  jmp return'+nr+'');

      add('');
      add(symbolNameWithOffset + ':');
      if jmp1 then
        add('  jmp1 newmem')
      else
      begin
        if farjmp then
          add('  jmp far newmem' + nr + '')
        else
          add('  jmp newmem' + nr + '');
      end;

      if codesize>jmpsize then
      begin
        if codesize-jmpsize>1 then
          add('  nop '+inttohex(codesize-jmpsize,1))
        else
          add('  nop');
      end;

      add('return' + nr + ':');
      add('registersymbol(' + symbolName + ')');
      add('');
    end;

    with disablecode do
    begin
      add(symbolNameWithOffset+':');
      add('  db ' + aobString);
      add('');
      add('unregistersymbol(' + symbolName + ')');
      add('dealloc(newmem'+nr+')');
    end;


    // add initial defines before enable
    getenableanddisablepos(script,enablepos,disablepos);
    p:=0;
    if (enablepos>0) then
      p:=enablepos;
    for i:= initialcode.Count-1 downto 0 do
      script.Insert(p, initialcode[i]);

    // add enable lines before disable
    getenableanddisablepos(script, enablepos, disablepos);
    p := script.Count - 1;
    if(disablepos > 0) then
      p := disablepos;
    for i:= enablecode.Count - 1 downto 0 do
      script.Insert(p,enablecode[i]);

    // add disable lines at very end
    for i:= 0 to disablecode.Count - 1 do
      script.Add(disablecode[i]);

    // add template comment at the beginning
    script.Insert(0, '{ '+rsAAAOBTemplate_Game+'   : ' + copy(mainform.ProcessLabel.Caption, pos('-', mainform.ProcessLabel.Caption) + 1, length(mainform.ProcessLabel.Caption)));
    script.Insert(1, '  '+rsAAAOBTemplate_Version+': ');
    script.Insert(2, '  '+rsAAAOBTemplate_Date+'   : ' + FormatDateTime('YYYY-MM-DD', Now));
    script.Insert(3, '  '+rsAAAOBTemplate_Author+' : ' + UserName);
    script.Insert(4,'');
    script.Insert(5, '  '+rsAAAOBTemplate_blabla);
    script.Insert(6,'}');
    script.Insert(7,'');

    // now we disassemble quite a bit more code for comments at the
    // bottom so someone can easily find the code again if the game
    // is updated
    addSnapshotAsComment(script, a, commentradius);


  finally
    initialcode.free;
    enablecode.free;
    disablecode.Free;
    addressList.Free;
    bytesList.Free;
    codeList.Free;
    dline.free;
  end;


end;

procedure TfrmAutoInject.menuAOBInjectionClick(Sender: TObject);
var
  a: ptruint;
  address: string;
  nr: string;
  i,j,k: integer;
  injectnr: integer;
  x: string;
  mi: TModuleInfo;
  symbolname: string;
begin
  a:=memorybrowser.disassemblerview.SelectedAddress;

  if symhandler.getmodulebyaddress(a,mi) then
    address:='"'+mi.modulename+'"+'+inttohex(a-mi.baseaddress,1)
  else
    address:=inttohex(a,8);

  if processhandler.is64Bit and mi5ByteJMP.checked and (FindFreeBlockForRegion(a,4096)=nil) then
    mi14ByteJMP.Checked:=true;


  if inputquery(rsCodeInjectTemplate, rsOnWhatAddressDoYouWantTheJump, address) then
  begin
    injectnr:=GetNextAllocNumber(assemblescreen.lines);
    if injectnr = 0 then nr := '' else nr := sysutils.IntToStr(injectnr);

    symbolname:='INJECT'+nr;

    if inputquery(rsCodeInjectTemplate, rsWhatIdentifierDoYouWantToUse, symbolName) then
      GenerateAOBInjectionScript(assemblescreen.Lines, address, symbolname, 10, (ssCtrl in GetKeyShiftState) or mi14ByteJMP.checked, mi1ByteExceptionJMP.checked);
  end;
end;

function GetUniqueAOB(mi: TModuleInfo; address: ptrUint; codesize: Integer; var resultOffset: Integer) : string;
var
  size: integer;
  dline: TDisassemblyLine;

  maskFlags : Array of Boolean; // true if we need to use **
  maskBytes : Array of Byte;    // bytes around code we're replacing
  flags : Array of Boolean;     // temp for single instruction
  br : ptruint;
  aob : string;
  i, j, k : Integer;

  // variables used for memory scan
  ms : TMemScan;
  minaddress: ptruint;
  maxaddress: ptrUint;
  foundAddress: ptrUint;
  foundCount: Integer;
  fl: TFoundList;

  instructionOffset: Integer; // offset for copying mask flags to main list from instruction list
  shortestAfter: Integer; // # of bytes, including codesize, index is 20 of course because it only counts starting at original code
  shortestBeforeIndex: Integer; // index to start at, will be 0 - 20
  shortestBeforeLength: Integer; // # of bytes, including before, original code, and possibly after

  finds: Array of TAOBFind; // for each found address has bytes to use for comparison

  // count how many found addresses match the criteria
  function CountMatches(offset: Integer; size: Integer) : Integer;
  var
    i: Integer;
    count: Integer;
    flength: Integer;
  begin
    count := 0;
    for i := 0 to Length(finds) - 1 do
    begin
      if finds[i].IsMatch(maskBytes, maskFlags, offset, offset + size - 1) then count := count + 1;
      if count > 1 then break; // short-circuit, we only care if there is more than 1
    end;
    result := count;
  end;
begin
  size := 40 + codesize; // 20 bytes on each side of replaced code
  SetLength(maskBytes, size); // setup array for bytes around code we're looking for
  SetLength(maskFlags, size); // flags on whether they need masking or not
  ReadProcessMemory(processhandle, pointer(address - 20), @maskBytes[0], size, br);

  dline:=TDisassemblyLine.create;

  // get AOB to search for using the code we're replacing
  aob := '';
  for i := 0 to codesize - 1 do
  begin
    if (i > 0) then aob := aob + ' ';
    aob := aob + inttohex(maskBytes[20 + i], 2);
  end;

  // Do AOBSCAN for replaced code
  ms := tmemscan.create(nil);
  ms.parseProtectionflags('');
  ms.onlyone := false;
  if mi.baseaddress > 0 then
  begin
    minaddress := mi.baseaddress;
    maxaddress := mi.baseaddress + mi.basesize;
  end else
  begin
    minaddress := 0;
    {$ifdef cpu64}
    if processhandler.is64Bit then
      maxaddress := qword($7fffffffffffffff)
    else
    {$endif}
    begin
      if Is64bitOS then
        maxaddress := $ffffffff
      else
        maxaddress := $7fffffff;
    end;
  end;
  ms.OnlyOne := false;
  fl := TFoundlist.create(nil, ms, '');
  ms.FirstScan(soExactValue, vtByteArray, rtTruncated, aob, '', minaddress, maxaddress, true, false, false, true, fsmAligned, '1');
  ms.WaitTillReallyDone; //wait till it's finished scanning
  foundCount := fl.Initialize(vtByteArray, nil);



  // if there's only one result, the code's AOB is fine
  if foundCount = 1 then
  begin
    resultOffset := 0;
    result := aob;
    fl.free;
    ms.free;
    exit;
  end;

  // now we need to narrow it down.  start by disassembling around the injection
  // point and creating flags on which bytes need to be masked because they are
  // probably pointers to code or data that may frequently change
  dline.Init(address - 128, mi);

  // 0 to 19: address - 20 to address - 1: before
  // 20 to 20 + codesize - 1): original code
  // 20 + codesize to 39 + codesize: after
  while (dline.Address <= (address + 20)) do
  begin
    // if we overran injection address, shorten to 'db X X X' statement
    if (dline.Address < address) and ((dline.Address + dline.Size) > address) then dline.Shorten(address - dline.Address);
    j := (dline.Address + 20) - address;
    k := j + dline.Size - 1;
    if (k >= 0) and (j <= (codesize + 39)) then
    begin
      // we're in range, get mask flags
      flags := dline.GetMaskFlags();
      for i := j to k do
      begin
        instructionOffset := i - j;
        if (i >= 0) and (i <= 39 + codesize) and (instructionOffset >= 0) then
        begin
          if (i < 20) or (i >= (20 + codesize)) then
            maskFlags[i] := flags[instructionOffset]
          else
            maskFlags[i] := false;
        end;
      end;
    end;

    dline.Init(dline.Address + dline.Size, mi); // next instruction
  end;

  // prep 'finds' array to read memory and make searching easier
  SetLength(finds, foundCount);
  for i := 0 to foundCount - 1 do
  begin
    finds[i].Init(fl.GetAddress(i), codesize);
  end;

  //not needed anymore
  fl.free;
  ms.free;


  // find shortest way to get a single match starting at original code
  shortestAfter := 100;
  shortestBeforeIndex := 19;
  shortestBeforeLength := 100;
  for i := codesize + 1 to codesize + 20 do
  begin
    if CountMatches(20, i) = 1 then
    begin
      shortestAfter := i;
      break;
    end;
  end;

  // now for before, we step back one at a time and loop up to shortestAfter bytes
  for i := 19 downto 0 do
  begin
    // i is index, j is length (checking indices i to i+j-1
    for j := codesize + (20 - i) to Min(shortestBeforeLength - 1, Min(shortestAfter - 6, (40 + codesize) - i)) do // first round, 6 to 26
    begin
      if CountMatches(i, j) = 1 then
      begin
        shortestBeforeIndex := i;
        shortestBeforeLength := j;
        break;
      end;
    end;
  end;

  if shortestAfter < shortestBeforeLength then
  begin
    shortestBeforeLength := shortestAfter;
    shortestBeforeIndex := 20;
  end;

  // if we can't find unique AOB, return earlier aob with error
  if shortestBeforeLength >= 100 then begin
    result := rsERRORCouldNotFindUniqueAOBTriedCode + aob + '"';
    exit;
  end;

  // create AOB using masking
  aob := '';
  for i := 0 to shortestBeforeLength - 1 do
  begin
    if i <> 0 then aob := aob + ' ';
    if maskFlags[i + shortestBeforeIndex] then
      aob := aob + '*'
    else
      aob := aob + IntToHex(maskBytes[i + shortestBeforeIndex], 2);
  end;

  dline.free;

  resultOffset := 20 - shortestBeforeIndex;
  result := aob;
end;

procedure TDisassemblyLine.Init(_address: ptrUint; _mi: TModuleInfo);
var x:string;
    pos1:integer;
    pos2:integer;
    i:integer;
    original: string;
begin
  Address := _address;
  Original := disassembler.disassemble(_address, Comment);

  Size := _address - Address;
  OriginalHexBytes := disassembler.getLastBytestring;
  Code:=disassembler.LastDisassembleData.prefix+' '+Disassembler.LastDisassembleData.opcode+' '+disassembler.LastDisassembleData.parameters;

  if (_mi.basesize = 0) or (_address < _mi.baseaddress) or (_address > (_mi.baseaddress + _mi.basesize)) then
    AddressString := inttohex(Address, 8)
  else
    AddressString := '"' + _mi.modulename + '"+' + inttohex(Address - _mi.baseaddress, 1);
end;

function TDisassemblyLine.GetHexBytes : String;
var i: Integer;
begin
  result:='';

  if length(Disassembler.LastDisassembleData.Bytes)>=size then
  begin
    for i:=0 to size-1 do
      result:=result+inttohex(Disassembler.LastDisassembleData.Bytes[i],2)+' ';
  end;
end;

// true if it is an instruction that probably starts a procedure so we can
// start our commented code here
function TDisassemblyLine.IsStarter : Boolean;
begin
  result:=code = 'push ebp';
end;

// true if it is an instruction that probably ends a procedure so we can end
// our commented code here
function TDisassemblyLine.IsEnder : Boolean;
begin
  result := Disassembler.LastDisassembleData.isret;
end;

// true if it not an instruction (int3, or add [eax],al : 00 00) that probably is not meant to be
// executed, so we know if we are outside a group of code
function TDisassemblyLine.IsValid : Boolean;
begin
  result:=true;
  if size>0 then //always true (if init is called once)
  begin
    if Disassembler.LastDisassembleData.Bytes[0]=$cc then
      result := false
    else
    if size>1 then
    begin
      if (Disassembler.LastDisassembleData.Bytes[0]=0) and (Disassembler.LastDisassembleData.Bytes[1]=0) then
        result:=false;
    end;
  end;
end;

// array with a boolean for each byte telling if it should be masked or not
function TDisassemblyLine.GetMaskFlags : TBooleanArray;
var
  masked : TBooleanArray;
  index : Integer;
  i, pos1, pos2 : Integer;
  part : String;
  mask : Boolean;
  count : Integer;
begin
  result:=[];
  setlength(result, size);


  pos1:=Disassembler.LastDisassembleData.prefixsize;
  for i:=0 to Disassembler.LastDisassembleData.SeperatorCount-1 do
  begin
    pos2:=Disassembler.LastDisassembleData.Seperators[i];
    if pos2>size then
      pos2:=size;


    mask:=(pos2<=size) and (pos2-pos1=4) and (abs(pinteger(@Disassembler.LastDisassembleData.Bytes[pos1])^)>=$10000); //value is bigger than 65535 (positive and negative)

    for index := pos1 to pos2-1 do
      result[index] := mask;

    pos1:=pos2;
  end;

  for index := pos1 to size-1 do
    result[index]:=false;
end;

procedure TDisassemblyLine.Shorten(_newSize: Integer);
var
  i, j: Integer;
  hexbytes: String;
begin
  // GetHexBytes() gives us the bytes split out with spaces between
  // all, this way we can write our 'db' statement and all bytes will
  // be unmasked
  Size := _newSize;
  OriginalHexBytes := GetHexBytes;
  Code := 'db ' + OriginalHexBytes + ' // SHORTENED TO HIT INJECTION FROM: ' + Code;
end;

constructor TDisassemblyLine.create;
begin
  disassembler:=TDisassembler.Create;
  Disassembler.showsymbols:=false; //seeing that mi is given explicitly to init() I assume that modules are prefered over exports
  Disassembler.showmodules:=true;
  Disassembler.showsections:=false;
  Disassembler.dataOnly:=false;
end;

destructor TDisassemblyLine.destroy;
begin
  if assigned(Disassembler) then
    Disassembler.free;

  inherited destroy;
end;


procedure TAOBFind.Init(_address: ptrUint; _codesize: Integer);
var
  i: integer;
  br: ptruint; // bytes actually read
begin
  Address := _address;
  Size := _codeSize + 40;
  SetLength(Bytes, Size);
  ReadProcessMemory(processhandle, pointer(Address - 20), @Bytes[0], Size, br);
end;

function TAOBFind.IsMatch(var maskBytes: Array Of Byte; var maskFlags : TBooleanArray; startIndex, endIndex: Integer): Boolean;
var
  i: Integer;
  mf: Boolean;
  mb: Byte;
  b: Byte;
begin
  for i := startIndex to endIndex do
  begin
    if (i > 0) and (i < Length(Bytes)) then
    begin
      mf := maskFlags[i];
      mb := maskBytes[i];
      b := Bytes[i];
      if not maskFlags[i] then
      begin
        if maskBytes[i] <> Bytes[i] then
        begin
          result := false;
          exit;
        end;
      end;
    end;
  end;
  result := true;
end;

// /\   http://forum.cheatengine.org/viewtopic.php?t=566415 (jgoemat and some mods by db)

function lua_getTabScript(L: PLua_State): integer; cdecl;
var
  frm: TfrmAutoInject;
  index: integer;
begin
  result:=0;
  frm:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    index:=lua_tointeger(L,1);
    lua_pushstring(L, frm.TabScript[index]);
    result:=1;
  end;
end;

function lua_setTabScript(L: PLua_State): integer; cdecl;
var
  frm: TfrmAutoInject;
  index: integer;
begin
  result:=0;
  frm:=luaclass_getClassObject(L);
  if lua_gettop(L)=2 then
  begin
    index:=lua_tointeger(L,1);
    frm.TabScript[index]:=lua_tostring(L,2);
  end;
end;

function lua_addTab(L: PLua_State): integer; cdecl;
var
  frm: TfrmAutoInject;
begin
  frm:=luaclass_getClassObject(L);
  frm.miNewTab.Click;
  if frm.tablist<>nil then
    lua_pushinteger(L,frm.tablist.Count-1)
  else
    lua_pushinteger(L,0);

  result:=1;
end;

function lua_deleteTab(L: PLua_State): integer; cdecl;
var
  frm: TfrmAutoInject;
  index: integer;
  oldtabindex: integer;
begin
  frm:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L,1);
    frm.deleteTab(index);
  end;
  result:=0;
end;

procedure frmAutoInject_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customForm_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addTab', lua_addTab);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'deleteTab', lua_deleteTab);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'TabScript', lua_getTabScript, lua_setTabScript);
end;

initialization
  luaclass_register(TfrmAutoInject, frmAutoInject_addMetaData);

  {$i frmautoinjectunit.lrs}

end.

