unit StructuresFrm2;


{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, math,
  StdCtrls, ComCtrls, Menus, lmessages, scrolltreeview, byteinterpreter, symbolhandler, cefuncproc,
  newkernelhandler, frmSelectionlistunit, frmStructuresConfigUnit, registry, Valuechange, DOM,
  XMLRead, XMLWrite, Clipbrd, CustomTypeHandler;




  { TfrmStructures2 }
type
  TdisplayMethod=(dtUnsignedInteger, dtSignedInteger, dtHexadecimal );
  TStructOperation=(soAdd, soDelete, soSort);

  TDissectedStruct=class;
  TStructelement=class
  private
    fparent: TDissectedStruct;
    foffset: integer;
    fbytesize: integer;
    fname: string;
    fvartype: TVariableType;
    fCustomType: TCustomtype;
    fdisplayMethod: TdisplayMethod;
    fchildstruct: TDissectedStruct;
    fchildstructstart: integer; //offset into the childstruct where this pointer starts. Always 0 for local structs, can be higher than 0 for other defined structs
  public
    delayLoadedStructname: string;
    constructor create(parent:TDissectedStruct);
    destructor destroy; override;
    function getParent: TDissectedStruct;
    function getOffset: integer;
    procedure setOffset(newOffset: integer);
    function getName: string;
    procedure setName(newname: string);
    function getVartype: TVariableType;
    procedure setVartype(newVartype: TVariableType);
    function getCustomType: TCustomType;
    procedure setCustomType(newCustomtype: TcustomType);
    function getDisplayMethod: TdisplayMethod;
    procedure setDisplayMethod(newDisplayMethod: TdisplayMethod);
    function getBytesize: integer;
    procedure setBytesize(newByteSize: integer);
    function getValue(address: ptruint; hashexprefix: boolean=false): string;
    procedure setvalue(address: ptruint; value: string);
    function getValueFromBase(baseaddress: ptruint): string;
    procedure setValueFromBase(baseaddress: ptruint; value: string);
    function isPointer: boolean;
    function getChildStruct: TDissectedStruct;
    procedure setChildStruct(newChildStruct: TDissectedStruct);
    procedure setChildStructStart(offset: integer);

    function getIndex: integer;

    procedure AutoCreateChildStruct(name: string; address: ptruint);

    property Name: string read getName write setName; //stored as utf8
    property VarType: TVariableType read getVarType write setVarType;
    property CustomType: TCustomType read getCustomType write setCustomType;
    property Offset: integer read getOffset write setOffset;
    property DisplayMethod: TdisplayMethod read getDisplayMethod write setDisplayMethod;
    property Bytesize: integer read getByteSize write setByteSize;
    property ChildStruct: TDissectedStruct read getChildStruct write setChildStruct;
    property ChildStructStart: integer read fchildstructstart write setChildStructStart;
    property index: integer read getIndex;
    property parent: TDissectedStruct read getParent;
  end;



  TDissectedStruct=class
  private
    structname: string;
    structelementlist: tlist;

    fAutoCreate: boolean;
    fAutoCreateStructsize: integer;
    fDoNotSaveLocal: boolean;
    fAutoDestroy: boolean;
    fAutoFill: boolean;
    fDefaultHex: boolean;


    fUpdateCounter: integer;
    fullstructupdate: boolean;

    updatecalledSort: boolean;
    updateChangedInformation: boolean;
    updatedelements: Tlist;



    function isUpdating: boolean;
    function getStructureSize: integer;
    procedure DoOptionsChangedNotification;
    procedure DoFullStructChangeNotification;
    procedure DoDeleteStructNotification;
    procedure setupDefaultSettings;

    procedure setDoNotSaveLocal(state: boolean);
    procedure setAutoCreateStructsize(size: integer);
    procedure setAutoCreate(state: boolean);
    procedure setAutoDestroy(state: boolean);
    procedure setAutoFill(state: boolean);
    procedure setDefaultHex(state: boolean);
  public
    constructor create(name: string);
    constructor createFromXMLNode(structure: TDOMNode);
    constructor createFromOutdatedXMLNode(structure: TDOMNode);

    procedure WriteToXMLNode(node: TDOMNode);

    destructor destroy; override;




    function getName: string;
    procedure setName(newname: string);
    function getElementCount: integer;
    function getElement(index: integer): TStructelement;
    procedure fillDelayLoadedChildstructs;

    procedure beginUpdate;
    procedure endUpdate;
    procedure DoElementChangeNotification(element: TStructelement);

    procedure OnDeleteStructNotification(structtodelete: TDissectedStruct);

    procedure sortElements;
    function addElement(name: string=''; offset: integer=0; vartype: TVariableType=vtByte; customtype:TCustomtype=nil; bytesize: integer=0; childstruct: TDissectedStruct=nil): TStructelement;
    procedure removeElement(element: TStructelement);
    procedure delete(index: integer);
    procedure autoGuessStruct(baseaddress: ptruint; offset: integer; bytesize: integer);
    procedure fillGaps(structbase: ptruint; askiftoobig: boolean);
    procedure addToGlobalStructList;
    procedure removeFromGlobalStructList;
    function isInGlobalStructList: boolean;
    function getIndexOf(element: TStructElement): integer;
    function getIndexOfOffset(offset: dword): integer;
    property structuresize : integer read getStructureSize;
    property name: string read getName write setName;

    //these properties are just for the gui
    property DoNotSaveLocal: boolean read fDoNotSaveLocal write setDoNotSaveLocal;
    property AutoCreateStructsize: integer read fAutoCreateStructsize write setAutoCreateStructsize;
    property AutoCreate: boolean read fAutoCreate write setAutoCreate;
    property AutoDestroy: boolean read fAutoDestroy write setAutoDestroy;
    property AutoFill: boolean read fAutoFill write setAutoFill;
    property DefaultHex: boolean read fDefaultHex write setDefaultHex;



    property count: integer read getElementCount;
    property element[Index: Integer]: TStructelement read getElement; default;
  end;

  TfrmStructures2=class;
  TStructColumn=class;

  TStructGroup=class //Will handle the group compares for each group
  private
    parent: TfrmStructures2;
    fcolumns: TList;
    fgroupname: string;
    fMatches: boolean;
    fcurrentString: string;
    isempty: boolean;

    GroupBox: TGroupbox;
    refcount: integer;
    grouppopup: TPopupMenu;
    miRename: TMenuItem;
    miDelete: TMenuItem;

    function getColumnCount: integer;
    function getColumn(i: integer): TStructColumn;
    procedure RenameClick(sender: tobject);
    procedure DeleteClick(sender: Tobject);
    procedure setGroupName(newname: string);
  public
    procedure setPositions;
    constructor create(parent: TfrmStructures2; GroupName: string);
    destructor destroy; override;

    procedure clear; //sets matches to true
    property Matches: boolean read fMatches;
    procedure addString(s:string); //sets matches to false if it doesn't match the previous string (also sets currentStrign to '' on false)


    property currentString: string read fCurrentString;
    property groupname: string read fGroupName write setGroupName;
    property box: TGroupbox read GroupBox;
    property columnCount: integer read getColumnCount;
    property columns[index: integer]: TStructColumn read Getcolumn;
  end;


  TStructColumn=class
  private
    parent: TStructGroup;

    faddress: ptruint;
    fsavedstate: pointer; //points to a copy made in the target process
    fsavedstatesize: integer;
    fFocused: boolean;
    edtAddress: TEdit;
    columneditpopupmenu: TPopupMenu;

    miToggleLock: TMenuItem;
    miChangeGroup: TMenuItem;
    miDelete: TMenuItem;

    focusedShape: TShape;


    fcompareValue: string;

    procedure ChangeGroupClick(sender: tobject);
    procedure DeleteClick(sender: TObject);
    procedure ToggleLockClick(sender: TObject);


    procedure edtAddressChange(sender: TObject);
    function getAddress: ptruint;
    procedure setAddress(address: ptruint);
    procedure setFocused(state: boolean);
    function getFocused: boolean;
    function getGlobalIndex: integer;
    procedure setNewParent(group: TStructGroup);
  public
    currentNodeAddress: string;    //temporary storage for rendering
    currentNodeValue: string;
    currentNodeColor: Tcolor;

    constructor create(parent: TStructGroup);
    destructor destroy; override;


    procedure focus;
    procedure edtAddressEnter(sender: tobject);
    procedure edtaddressMousedown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure clearSavedState;
    function saveState: boolean;
    function getSavedState: pointer;
    function getSavedStateSize: integer;

    function getEditWidth: integer;
    function getEditLeft: integer;
    function isXInColumn(x: integer): boolean;
    procedure SetProperEditboxPosition;
    property EditWidth: integer read getEditwidth;
    property EditLeft: integer read getEditleft;
    property Address: ptruint read getAddress write setAddress;
    property Focused: boolean read getFocused write setFocused;
    property CompareValue: string read fcompareValue write fcompareValue;
    property GlobalIndex: integer read getGlobalIndex;
  end;

  TfrmStructures2 = class(TForm)
    MenuItem5: TMenuItem;
    miGenerateGroupscan: TMenuItem;
    miDefaultHexadecimal: TMenuItem;
    miFindRelations: TMenuItem;
    miShowTypeForEntriesWithNoDescription: TMenuItem;
    miAutoDestroyLocal: TMenuItem;
    miAutoFillGaps: TMenuItem;
    miFillGaps: TMenuItem;
    miChangeValue: TMenuItem;
    miShowAddresses: TMenuItem;
    miDoNotSaveLocal: TMenuItem;
    miFullUpgrade: TMenuItem;
    miAddChildElement: TMenuItem;
    miAddElement: TMenuItem;
    Addextraaddress1: TMenuItem;
    miAddToAddresslist: TMenuItem;
    miAutoGuess: TMenuItem;
    miChangeElement: TMenuItem;
    miCommands: TMenuItem;
    Definenewstructure1: TMenuItem;
    Deletecurrentstructure1: TMenuItem;
    miDeleteElement: TMenuItem;
    File1: TMenuItem;
    HeaderControl1: THeaderControl;
    MainMenu1: TMainMenu;
    miBrowsePointer: TMenuItem;
    miBrowseAddress: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miAutoCreate: TMenuItem;
    miAutostructsize: TMenuItem;
    miChangeColors: TMenuItem;
    miUpdateInterval: TMenuItem;
    miUpdateOffsets: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    miNewWindow: TMenuItem;
    Open1: TMenuItem;
    OpenDialog1: TOpenDialog;
    pnlGroups: TPanel;
    pmStructureView: TPopupMenu;
    Recalculateaddress1: TMenuItem;
    Renamestructure1: TMenuItem;
    Save1: TMenuItem;
    SaveDialog1: TSaveDialog;
    saveValues: TSaveDialog;
    Structures1: TMenuItem;
    updatetimer: TTimer;
    tvStructureView: TTreeView;
    procedure Addextraaddress1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure miGenerateGroupscanClick(Sender: TObject);
    procedure miAutoCreateClick(Sender: TObject);
    procedure miAutoDestroyLocalClick(Sender: TObject);
    procedure miAutoFillGapsClick(Sender: TObject);
    procedure miChangeValueClick(Sender: TObject);
    procedure miBrowseAddressClick(Sender: TObject);
    procedure miBrowsePointerClick(Sender: TObject);
    procedure miAddToAddresslistClick(Sender: TObject);
    procedure Deletecurrentstructure1Click(Sender: TObject);
    procedure miAutoGuessClick(Sender: TObject);
    procedure miAutostructsizeClick(Sender: TObject);
    procedure miChangeColorsClick(Sender: TObject);
    procedure miDefaultHexadecimalClick(Sender: TObject);
    procedure miDoNotSaveLocalClick(Sender: TObject);
    procedure miFillGapsClick(Sender: TObject);
    procedure miFindRelationsClick(Sender: TObject);
    procedure miFullUpgradeClick(Sender: TObject);
    procedure miChangeElementClick(Sender: TObject);
    procedure Definenewstructure1Click(Sender: TObject);
    procedure miDeleteElementClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HeaderControl1SectionTrack(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection; Width: Integer; State: TSectionTrackState);
    procedure miAddChildElementClick(Sender: TObject);
    procedure miAddElementClick(Sender: TObject);
    procedure miShowAddressesClick(Sender: TObject);
    procedure miUpdateOffsetsClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure pmStructureViewPopup(Sender: TObject);
    procedure miNewWindowClick(Sender: TObject);
    procedure miUpdateIntervalClick(Sender: TObject);
    procedure pnlGroupsClick(Sender: TObject);
    procedure Renamestructure1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure tvStructureViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure tvStructureViewDblClick(Sender: TObject);
    procedure tvStructureViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure updatetimerTimer(Sender: TObject);
    procedure tvStructureViewCollapsed(Sender: TObject; Node: TTreeNode);
    procedure tvStructureViewCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure tvStructureViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure tvStructureViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  private
    { private declarations }
    fmainStruct: TDissectedStruct;
    fgroups: Tlist;


    fDefaultColor: TColor;
    fNoMatchColor: TColor; //The color to use when not all elements have the same color
    fMatchColor: tcolor; //The color to use when all elements in the group match
    fAllMatchColorSame: TColor; //The color to use when all groups have matching elements AND the same value
    fAllMatchColorDiff: TColor; //The color to use when all groups have matching alements but different values between groups

    procedure UpdateCurrentStructOptions;
    procedure setupColors;

    procedure miSelectStructureClick(Sender: tobject);
    procedure InitializeFirstNode;
    procedure RefreshStructureList;
    procedure TreeViewHScroll(sender: TObject; scrolledleft, maxscrolledleft: integer);
    procedure TreeViewVScroll(sender: TObject);
    procedure addColumn;
    procedure removeColumn(columnid: integer);
    procedure FillTreenodeWithStructData(currentnode: TTreenode);
    procedure setupNodeWithElement(node: TTreenode; element: TStructElement);
    procedure setCurrentNodeStringsInColumns(node: TTreenode; element: TStructElement);  //sets the value for the current node into the columns
    procedure RefreshVisibleNodes;
    procedure setMainStruct(struct: TDissectedStruct);
    function getColumn(i: integer): TStructColumn;
    function getColumnCount: integer;

    function getGroup(i: integer): TStructGroup;
    function getGroupCount: integer;
    procedure EditValueOfSelectedNodes(c:TStructColumn);
  public
    { public declarations }
    initialaddress: integer;
    function getFocusedColumn: TStructColumn;
    function getColumnAtXPos(x: integer): TStructColumn;
    procedure changeNodes;
    procedure addFromNode(n: TTreenode; asChild: boolean=false);
    function getStructElementFromNode(node: TTreenode): TStructelement;
    function getStructFromNode(node: TTreenode): TDissectedStruct;
    function getChildStructFromNode(node: TTreenode): TDissectedStruct;
    function getMainStruct: TDissectedStruct;

    procedure getPointerFromNode(node: TTreenode; column:TStructcolumn; var baseaddress: ptruint; var offsetlist: toffsetlist);
    function getAddressFromNode(node: TTreenode; column: TStructColumn; var hasError: boolean): ptruint;

    procedure onAddedToStructList(sender: TDissectedStruct);
    procedure onRemovedFromStructList(sender: TDissectedStruct);
    procedure onFullStructChange(sender: TDissectedStruct);   //called when a structure is changed (sort/add/remove entry)
    procedure onStructOptionsChange(sender: TDissectedStruct);
    procedure onElementChange(struct:TDissectedStruct; element: TStructelement); //called when an element of a structure is changed
    procedure onStructureDelete(sender: TDissectedStruct);

    property DefaultColor: TColor read fDefaultColor;
    property MatchColor: TColor read fMatchColor;
    property NoMatchColor: Tcolor read fNoMatchcolor;
    property AllMatchColorSame: TColor read fAllMatchColorSame;
    property AllMatchColorDiff: TColor read fAllMatchColorDiff;

    property mainStruct : TDissectedStruct read fmainStruct write setMainStruct;
    property columnCount: integer read getColumnCount;
    property columns[index: integer]: TStructColumn read Getcolumn;
    property groupcount: integer read getGroupCount;
    property group[index: integer]: TStructGroup read getGroup;
  end;

var
  frmStructures2: TList;
  DissectedStructs: Tlist;   //these get saved to the table and show up in the structure list


implementation

{$R *.lfm}

uses MainUnit, frmStructures2ElementInfoUnit, MemoryBrowserFormUnit,
  frmStructureLinkerUnit, frmgroupscanalgoritmgeneratorunit;

resourcestring
  rsAddressValue = 'Address: Value';
  rsUndefined = 'undefined';

  rsThisIsQuiteABigStructureHowManyBytesDoYouWantToSav = 'This is quite a big '
     +'structure. How many bytes do you want to save?';
   rsStructureViewLock = 'Structure view lock';
   rsPointerTo = 'Pointer';
   rsUnnamedStructure = 'unnamed structure';
   rsStructureDefine = 'Structure define';
   rsGiveTheNameForThisStructure = 'Give the name for this structure';
   rsDoYouWantCheatEngineToTryAndFillInTheMostBasicType = 'Do you want Cheat '
     +'Engine to try and fill in the most basic types of the struct using the '
     +'current address?';
   rsPleaseGiveAStartingSizeOfTheStructYouCanChangeThis = 'Please give a '
     +'starting size of the struct (You can change this later if needed)';

   rsMemoryDissect = 'Memory dissect';
   rsFirstSelectAStructureYouWantToModifyOrDefine = 'First select a structure '
     +'you want to modify or define one first';
   rsUpdateInterval = 'Update interval';
   rsNewInterval = 'New interval';
   rsDissectData = 'Dissect Data';
   rsHowManyBytesDoYouWantToShiftThisAndFollowingOffset = 'How many bytes do '
     +'you want to shift this and following offsets? (Decimal)';
   rsAreYouSureYouWantToDelete = 'Are you sure you want to delete %s?';
   rsThisIsNotAValidStructureFile = 'This is not a valid structure file';
   rsWrongVersion = 'This structure fils was generated with a newer version of '
     +'Cheat Engine. (That means there''s more than likely a new version so '
     +'please update....)';
   rsUnkownFileExtension = 'Unkown file extension';
   rsAreYouSureYouWantToRemoveAllStructures = 'Are you sure you want to remove '
     +'all structures?';
   rsRecalculateBaseOfStructure = 'Recalculate base of structure';
   rsGiveTheAddressOfThisElement = 'Give the address of this element';
   rsIHaveNoIdeaWhatMeans = 'I have no idea what %s means';
   rsChangeGroup = 'Change group';
   rsLockMemory = 'Lock memory';
   rsUnlockMemory = 'Unlock memory';
   rsRenameStructure = 'Rename structure';
   rsGiveTheNewNameOfThisStructure = 'Give the new name of this structure';
   rsPleaseGiveAStartingOffsetToEvaluate = 'Please give a starting offset to '
     +'evaluate';
   rsPleaseGiveTheSizeOfTheBlockToEvaluate = 'Please give the size of the '
     +'block to evaluate';
   rsStructureDefiner = 'Structure definer';
   rsWhichGroupDoYouWantToSetThisAddressTo = 'Which group do you want to set '
     +'this address to?';
   rsAutogeneratedFor = 'Autogenerated for %s';

function DisplaymethodToString(d:TdisplayMethod): string;
begin
  case d of
    dtUnsignedInteger: result:='Unsigned Integer';
    dtSignedInteger: result:='Signed Integer';
    dtHexadecimal: result:='Hexadecimal';
  end;
end;

function StringToDisplayMethod(s: string): TdisplayMethod;
begin
  s:=LowerCase(s);
  result:=dtUnsignedInteger;

  if s='unsigned integer' then result:=dtUnsignedInteger else
  if s='signed integer' then result:=dtSignedInteger else
  if s='hexadecimal' then result:=dtHexadecimal;
end;

{Struct}

function TStructelement.getParent: TDissectedStruct;
begin
  result:=fParent;
end;

function TStructelement.getOffset: integer;
begin
  result:=fOffset;
end;

procedure TStructelement.setOffset(newOffset: integer);
begin
  if newOffset<>fOffset then
  begin
    fOffset:=newOffset;
    parent.sortElements;
  end;
end;

function TStructelement.getName: string;
begin
  result:=fname;
end;

procedure TStructelement.setName(newname: string);
begin
  if newname<>fname then
  begin
    fname:=newname;
    parent.DoElementChangeNotification(Self);
  end;
end;

function TStructelement.getVartype: TVariableType;
begin
  result:=fVartype;
end;

procedure TStructelement.setVartype(newVartype: TVariableType);
begin
  if newVartype<>fVartype then
  begin
    fVartype:=newVartype;
    parent.DoElementChangeNotification(self);
  end;
end;

function TStructelement.getCustomType: TCustomType;
begin
  result:=fCustomType;
end;

procedure TStructelement.setCustomType(newCustomtype: TcustomType);
begin
  if newCustomtype<>fCustomType then
  begin
    fCustomType:=newCustomtype;
    parent.DoElementChangeNotification(self);
  end;
end;

function TStructelement.getDisplayMethod: TdisplayMethod;
begin
  result:=fDisplayMethod;
end;

procedure TStructelement.setDisplayMethod(newDisplayMethod: TdisplayMethod);
begin
  if newDisplayMethod<>fDisplayMethod then
    fDisplayMethod:=newDisplayMethod;

  parent.DoElementChangeNotification(self);
end;

function TStructelement.getBytesize: integer;
begin
  if vartype in [vtByteArray, vtString, vtUnicodeString] then
    result:=fbytesize
  else
  begin
    result:=1;
    case vartype of
      vtByte: result:=1;
      vtWord: result:=2;
      vtDword: result:=4;
      vtQword: result:=8;
      vtSingle: result:=4;
      vtDouble: result:=8;
      vtPointer: result:=processhandler.pointersize;
      vtCustom: if customtype<>nil then result:=CustomType.bytesize;
    end;
  end;
end;

procedure TStructelement.setBytesize(newByteSize: integer);
begin
  if newByteSize<>fbytesize then
  begin
    fbytesize:=max(1,newByteSize); //at least 1 byte
    parent.DoElementChangeNotification(self);
  end;
end;

function TStructelement.getValue(address: ptruint; hashexprefix: boolean=false): string;
var vt: TVariableType;
  ashex: boolean;
begin
  if vartype=vtPointer then
  begin
    ashex:=true;
    result:='P->';

    if processhandler.is64Bit then
      vt:=vtQword
    else
      vt:=vtDword;
  end
  else
  begin
    result:='';
    vt:=vartype;
    ashex:=displaymethod=dtHexadecimal;
  end;

  if hashexprefix and ashex then
    result:='0x'; //also takes care of P->


  result:=result+readAndParseAddress(address, vt,  fCustomType, ashex, displayMethod=dtSignedInteger, bytesize);
end;

procedure TStructelement.setvalue(address: ptruint; value: string);
var hex: boolean;
  vt: TVariableType;
begin
  if vartype=vtPointer then
  begin
    if processhandler.is64Bit then
      vt:=vtQword
    else
      vt:=vtDword;
    hex:=true;
  end
  else
  begin
    vt:=vartype;
    hex:=displaymethod=dtHexadecimal;
  end;

  try
    ParseStringAndWriteToAddress(value, address, vt, hex, fCustomType);
    parent.DoElementChangeNotification(self);
  except
  end;
end;

function TStructelement.getValueFromBase(baseaddress: ptruint): string;
begin
  result:=getvalue(baseaddress+offset);
end;

procedure TStructelement.setValueFromBase(baseaddress: ptruint; value: string);
begin
  setvalue(baseaddress+offset, value);
end;

function TStructelement.isPointer: boolean;
begin
  result:=vartype=vtPointer;
end;

function TStructelement.getChildStruct: TDissectedStruct;
begin
  result:=fchildstruct;
end;

procedure TStructelement.setChildStruct(newChildStruct: TDissectedStruct);
begin
  fchildstruct:=newChildStruct;
  parent.DoElementChangeNotification(self);
end;

procedure TStructelement.setChildStructStart(offset: integer);
begin
  fchildstructstart:=offset;
  parent.DoElementChangeNotification(self);
end;

function TStructelement.getIndex: integer;
begin
  result:=parent.getIndexOf(self);
end;

procedure TStructelement.AutoCreateChildStruct(name: string; address: ptruint);
var c: TDissectedStruct;
begin
  if isPointer and (ChildStruct=nil) then
  begin
    c:=TDissectedStruct.create(name);
    c.autoGuessStruct(address, 0, parent.autoCreateStructsize);

    if c.count>0 then
      ChildStruct:=c
    else
      c.free;
  end;
end;

destructor TStructelement.destroy;
begin
  parent.removeElement(self);
  inherited destroy;
end;

constructor TStructelement.create(parent:TDissectedStruct);
begin
  fparent:=parent;
  fbytesize:=1;
end;


{TDissectedStruct}


function TDissectedStruct.getName: string;
begin
  result:=structname;
end;

procedure TDissectedStruct.setName(newname: string);
begin
  structname:=newname;
  DoFullStructChangeNotification;
end;

function TDissectedStruct.getElementCount: integer;
begin
  result:=structelementlist.Count;
end;

function TDissectedStruct.getElement(index: integer): TStructelement;
begin
  if (index>=0) and (index<structelementlist.Count) then
    result:=TStructelement(structelementlist.Items[index])
  else
    result:=nil;
end;

function TDissectedStruct.isUpdating: boolean;
begin
  result:=fUpdateCounter>0;
end;

function elementsort(Item1, Item2: Pointer): Integer;
begin
  result:=CompareValue(TStructelement(item1).offset, TStructelement(item2).offset);
end;


procedure TDissectedStruct.sortElements;
begin
  if isUpdating=false then
  begin
    structelementlist.Sort(elementsort);
    DoFullStructChangeNotification;
  end
  else
    updateCalledSort:=true;

end;

procedure TDissectedStruct.DoOptionsChangedNotification;
//update all windows with this as the current structure
var i: integer;
begin
  for i:=0 to frmStructures2.Count-1 do
    TfrmStructures2(frmStructures2[i]).onStructOptionsChange(self);

end;


procedure TDissectedStruct.DoFullStructChangeNotification;
var i: integer;
begin
  if isUpdating=false then
  begin
    for i:=0 to frmStructures2.Count-1 do
      TfrmStructures2(frmStructures2[i]).onFullStructChange(self);
  end
  else
    fullstructupdate:=true;
end;



procedure TDissectedStruct.DoElementChangeNotification(element: TStructelement);
var i: integer;
begin
  if isUpdating=false then
  begin
    for i:=0 to frmStructures2.Count-1 do
      TfrmStructures2(frmStructures2[i]).onElementChange(self, element);
  end
  else
  begin
    if updatedelements.IndexOf(element)=-1 then  //add this element to the list of updated elements that endupdate should then update
      updatedelements.Add(element);

    updateChangedInformation:=true;
  end;
end;

procedure TDissectedStruct.beginUpdate;
begin
  inc(fUpdateCounter);
  updatecalledSort:=false;
  updateChangedInformation:=false;
  fullstructupdate:=false;

  if updatedelements=nil then
    updatedelements:=TList.Create;

  updatedelements.clear;
end;

procedure TDissectedStruct.endUpdate;
var i: integer;
begin
  if isUpdating then
    dec(fUpdateCounter);

  if fUpdateCounter=0 then
  begin
    if updatecalledsort then
    begin
      sortElements; //sort them now
      updatecalledSort:=false;
    end
    else
    if fullstructupdate then
    begin
      DoFullStructChangeNotification;
      fullstructupdate:=false;
    end
    else //no need to call the individual updates if sort was done.
    if updateChangedInformation then //not set for changing the offset, only for other visual stuff
    begin
      for i:=0 to updatedelements.count-1 do
        DoElementChangeNotification(TStructelement(updatedelements[i]));

      updatedelements.clear;
      updateChangedInformation:=false;
    end;


  end;
end;

function TDissectedStruct.addElement(name: string=''; offset: integer=0; vartype: TVariableType=vtByte; customType: TCustomtype=nil; bytesize: integer=0; childstruct: TDissectedStruct=nil): TStructelement;
begin
  beginUpdate;
  result:=TStructelement.create(self);
  structelementlist.Add(result);

  result.name:=name;
  result.offset:=offset;
  result.vartype:=vartype;
  result.CustomType:=customType;
  result.childstruct:=childstruct;
  result.bytesize:=bytesize;

  sortElements;
  endUpdate;
end;

procedure TDissectedStruct.removeElement(element: TStructelement);
begin
  structelementlist.Remove(element);

  DoFullStructChangeNotification;
end;

procedure TDissectedStruct.delete(index: integer);
begin
  removeElement(element[index]);
end;

procedure TDissectedStruct.fillGaps(structbase: ptruint; askiftoobig: boolean);
//Will find gaps and fill them up. If a gap is bigger than 512, ask the user, or skip if not asked
var i,j: integer;
  size: integer;
  smallestacceptedsize: integer;
  v: TVariableType;

  newoffset: integer;
begin
  i:=1;
  smallestacceptedsize:=512;
  while i<count do
  begin
    size:=element[i].Offset-(element[i-1].Offset+element[i-1].Bytesize);
    if size>0 then
    begin
      if size>smallestacceptedsize then
      begin
        if (askiftoobig=false) or (MessageDlg('The gap between offset '+inttohex(element[i-1].Offset,1)+' and '+inttohex(element[i-1].Offset,1)+' is '+inttostr(size)+' bytes long. Autofill this?', mtConfirmation, [mbyes,mbno],0)<>mryes) then
        begin
          inc(i);
          continue;
        end;
      end;
      smallestacceptedsize:=max(smallestacceptedsize, size); //update the smallestacceptedsize

      //and fill this range
      v:=element[i-1].VarType;
      newoffset:=element[i-1].Offset+element[i-1].Bytesize;
      if (v in [vtDword, vtSingle]) and (size in [4,8,12,16,20,24,32]) then //previous was a 4 byte type and nicely aligned, so fill with the same type
      begin
        for j:=0 to (size div 4)-1 do
        begin
          addElement('', newOffset, v);
          inc(newOffset,4);
        end;
      end
      else
      begin
        //not really sure what to fill it with so use the autoguess method
        autoGuessStruct(structbase+newoffset, newoffset, size);
      end;
    end;

    inc(i);
  end;
end;

procedure TDissectedStruct.autoGuessStruct(baseaddress: ptruint; offset: integer; bytesize: integer);
var
  buf: pbytearray;

  currentOffset: integer;
  x: dword;
  i,j: integer;
  bs: integer;
  vt: TVariableType;

  e: TStructelement;

  customtype: TCustomType;
  ctp: PCustomType;
begin
  if frmStructuresConfig.cbAutoGuessCustomTypes.checked then
    ctp:=@customtype
  else
    ctp:=nil;

  //figure out the structure for this base address
  getmem(buf, bytesize);

  try
    beginUpdate;

    if readprocessmemory(processhandle,pointer(baseaddress),@buf[0],bytesize,x) then
    begin
      currentOffset:=offset;

      i:=0;
      while i<x do
      begin
        vt:=FindTypeOfData(baseAddress+i,@buf[i],bytesize-i, ctp);
        e:=addElement();
        e.Offset:=currentOffset;
        e.vartype:=vt;
        if vt=vtCustom then
          e.CustomType:=customtype;

        if vt in [vtByte..vtQword] then
        begin
          if fDefaultHex or ((vt=vtDword) and (not isHumanReadableInteger(pinteger(@buf[i])^))) then
            e.DisplayMethod:=dtHexadecimal;

        end
        else
        if vt in [vtString, vtUnicodeString] then
        begin
          //find out the size of the string
          bs:=0;
          j:=i;


          while (j<bytesize) and (buf[j]>=32) and (buf[j]<=127) do
          begin
            if vt=vtString then
              inc(j)
            else
              inc(j,2);

            inc(bs);
          end;

          if (j<bytesize-1) and (buf[j]=0) then //add the zero terminator if one exists
            inc(bs);

          e.Bytesize:=bs;
        end;

        inc(i, e.Bytesize);
        inc(currentOffset, e.ByteSize);
      end;
    end;
  finally
    endUpdate;
    DoFullStructChangeNotification;
    freemem(buf);
  end;
end;

function TDissectedStruct.getStructureSize: integer;
var e: TStructelement;
begin
  result:=0;
  if getElementCount>0 then
  begin
    e:=getElement(getElementCount-1);
    result:=e.Offset+e.Bytesize;
  end;
end;

procedure TDissectedStruct.addToGlobalStructList;
var i: integer;
begin
  if not isInGlobalStructList then
  begin
    DissectedStructs.Add(self);

    //notify that this structure has been added
    for i:=0 to frmStructures2.Count-1 do
       TfrmStructures2(frmStructures2[i]).onAddedToStructList(self);
  end;
end;

procedure TDissectedStruct.removeFromGlobalStructList;
var i: integer;
begin
  if isInGlobalStructList then
  begin
    DissectedStructs.Remove(self);

    //notify that this structure has been removed
    for i:=0 to frmStructures2.Count-1 do
       TfrmStructures2(frmStructures2[i]).onRemovedFromStructList(self);
  end;
end;

function TDissectedStruct.isInGlobalStructList: boolean;
begin
  result:=DissectedStructs.IndexOf(self)<>-1;
end;

function TDissectedStruct.getIndexOf(element: TStructElement): integer;
begin
  result:=structelementlist.IndexOf(element);
end;

function TDissectedStruct.getIndexOfOffset(offset: dword): integer;
//Find the first index where the offset is equal or bigger than the searched for index
begin
  result:=0;
  while result<count do
  begin
    if element[result].Offset>=offset then break; //found it
    inc(result);
  end;
  //if nothing is found result will contain the current count, resulting in nothing
end;

procedure TDissectedStruct.OnDeleteStructNotification(structtodelete: TDissectedStruct);
var
  i: integer;
  s: TDissectedStruct;
begin
  //remove all mentioning of this struct
  if structtodelete=self then exit;

  beginUpdate;
  for i:=0 to count-1 do
  begin
    s:=element[i].ChildStruct;

    if s<>nil then
    begin
      if element[i].ChildStruct=structtodelete then
        element[i].ChildStruct:=nil
      else
      begin
        //a struct but not the deleted one. Make sure it is a LOCAL one to prevent an infinite loop (a global struct can point to itself)
        if not s.isInGlobalStructList then
          s.OnDeleteStructNotification(structtodelete);
      end;
    end;
  end;
  endUpdate;
end;

procedure TDissectedStruct.DoDeleteStructNotification;
var i: integer;
begin
  //tell each form that it should close this structure
  for i:=0 to frmStructures2.Count-1 do
    TfrmStructures2(frmStructures2[i]).OnStructureDelete(self);


  //tell each structure that it should remove all the childstruct mentions of this structure
  for i:=0 to DissectedStructs.count-1 do
  begin
    if DissectedStructs[i]<>self then
      TDissectedStruct(DissectedStructs[i]).OnDeleteStructNotification(self);
  end;
end;


procedure TDissectedStruct.WriteToXMLNode(node: TDOMNode);
var
  i: integer;
  doc: TDOMDocument;

  structnode: TDOMElement;
  elementnodes, elementnode: TDOMElement;
begin
  doc:=node.OwnerDocument;

  structnode:=TDOMElement(node.AppendChild(doc.CreateElement('Structure')));
  TDOMElement(structnode).SetAttribute('Name',Utf8ToAnsi(name));



  TDOMElement(structnode).SetAttribute('DoNotSaveLocal',BoolToStr(fDoNotSaveLocal,'1','0'));
  TDOMElement(structnode).SetAttribute('AutoCreate',BoolToStr(fAutoCreate,'1','0'));
  TDOMElement(structnode).SetAttribute('AutoCreateStructsize',inttostr(fAutoCreateStructsize));
  TDOMElement(structnode).SetAttribute('AutoDestroy',BoolToStr(fAutoDestroy,'1','0'));
  TDOMElement(structnode).SetAttribute('AutoFill',BoolToStr(fAutoFill,'1','0'));
  TDOMElement(structnode).SetAttribute('DefaultHex',BoolToStr(fDefaultHex,'1','0'));

  elementnodes:=TDOMElement(structnode.AppendChild(TDOMNode(doc.CreateElement('Elements'))));

  for i:=0 to count-1 do
  begin
    elementnode:=TDOMElement(elementnodes.AppendChild(doc.CreateElement('Element')));

    elementnode.SetAttribute('Offset', IntToStr(element[i].Offset));
    if element[i].Name<>'' then
      elementnode.SetAttribute('Description', utf8toansi(element[i].Name));

    elementnode.SetAttribute('Vartype', VariableTypeToString(element[i].VarType));
    if element[i].CustomType<>nil then
      elementnode.SetAttribute('Customtype', element[i].CustomType.name);
    elementnode.SetAttribute('Bytesize', IntToStr(element[i].Bytesize));
    elementnode.SetAttribute('DisplayMethod', DisplaymethodToString(element[i].DisplayMethod));

    if element[i].ChildStructStart<>0 then
      elementnode.SetAttribute('ChildStructStart', IntToStr(element[i].ChildStructStart));

    if (element[i].isPointer) and (element[i].ChildStruct<>nil) then
    begin
      if (element[i].ChildStruct.isInGlobalStructList) then
      begin
        //set childstruct as an attribute
        elementnode.SetAttribute('ChildStruct', utf8toansi(element[i].ChildStruct.Name));
      end
      else
      begin
        //local struct, only save if allowed
        if doNotSaveLocal=false then
        begin
          //save this whole struct
          element[i].ChildStruct.WriteToXMLNode(elementnode);
        end;
      end;
    end;
  end;
end;

procedure TDissectedStruct.fillDelayLoadedChildstructs;
//call this when all structures have been loaded
var
  i,j: integer;
  sn: string;
begin
  beginUpdate;
  for i:=0 to count-1 do
  begin
    sn:=element[i].delayLoadedStructname;
    if sn<>'' then
    begin
      //find and fill in the struct
      for j:=0 to DissectedStructs.count-1 do
      begin
        if TDissectedStruct(DissectedStructs[j]).name=sn then
        begin
          //found it, set the struct
          element[i].ChildStruct:=TDissectedStruct(DissectedStructs[j]);
          break;
        end;
      end;
    end
    else
    begin
      if (element[i].isPointer) and (element[i].ChildStruct<>nil) and (element[i].ChildStruct.isInGlobalStructList=false) then //local structure, scan this one as well
        element[i].ChildStruct.fillDelayLoadedChildstructs;
    end;
  end;
  endUpdate;
end;

procedure TDissectedStruct.setDoNotSaveLocal(state: boolean);
begin
  fDoNotSaveLocal:=state;
  DoOptionsChangedNotification;
end;

procedure TDissectedStruct.setAutoCreateStructsize(size: integer);
begin
  fAutoCreateStructsize:=size;
  DoOptionsChangedNotification;
end;

procedure TDissectedStruct.setAutoCreate(state: boolean);
begin
  fAutoCreate:=state;
  DoOptionsChangedNotification;
end;

procedure TDissectedStruct.setAutoDestroy(state: boolean);
begin
  fAutoDestroy:=state;
  DoOptionsChangedNotification;
end;

procedure TDissectedStruct.setAutoFill(state: boolean);
begin
  fAutoFill:=state;
  DoOptionsChangedNotification;
end;

procedure TDissectedStruct.setDefaultHex(state: boolean);
begin
  fDefaultHex:=state;
  DoOptionsChangedNotification;
end;

procedure TDissectedStruct.setupDefaultSettings;
//loads the default settings for new structures
var reg: Tregistry;
begin
  fAutoCreate:=true; //default settings in case of no previous settings
  fAutoCreateStructsize:=4096;

  reg:=tregistry.create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine\DissectData',false) then
    begin
      if reg.ValueExists('Autocreate') then fAutoCreate:=reg.ReadBool('Autocreate');
      if reg.ValueExists('Autocreate Size') then fAutoCreateStructsize:=reg.ReadInteger('Autocreate Size');
      if reg.ValueExists('Autodestroy') then fAutoDestroy:=reg.ReadBool('Autodestroy');
      if reg.ValueExists('Don''t save local') then fDoNotSaveLocal:=reg.ReadBool('Don''t save local');
      if reg.ValueExists('Autofill') then fAutoFill:=reg.ReadBool('Autofill');
      if reg.ValueExists('DefaultHex') then fDefaultHex:=reg.ReadBool('DefaultHex');

    end;
  finally
    reg.free;
  end;
end;

constructor TDissectedStruct.createFromOutdatedXMLNode(structure: TDOMNode);
//Constructor for loading old V1 structures
var tempnode: TDOMNode;
  elements: TDOMNode;
  element, tempelement: TDOMNode;
  i: integer;
  currentOffset: dword;
  findoffset: boolean;

  offset: integer;
  description: string;
  vartype: TVariableType;
  bytesize: integer;
  displaymethod: TdisplayMethod;
  childstruct: string;
  isPointer: boolean;
  structurenr: integer;

  se: TStructelement;
begin
  currentoffset:=0;

  self.name:='';
  structelementlist:=tlist.Create;
  autoCreateStructsize:=4096; //default autocreate size
  setupDefaultSettings;

  beginupdate;
  try
    if structure.NodeName='Structure' then
    begin
      tempnode:=structure.FindNode('Name');
      if tempnode<>nil then
        self.name:=tempnode.TextContent;

      elements:=structure.FindNode('Elements');
      for i:=0 to elements.childnodes.count-1 do
      begin
        element:=elements.ChildNodes[i];

        childstruct:='';
        findoffset:=true;
        tempnode:=element.FindNode('Offset');
        if tempnode<>nil then
        begin
          try
            offset:=strtoint(tempnode.textcontent);
            findoffset:=false; //the offset was fetched properly, no need to calculate it
          except

          end;
        end;

        if findoffset then //it couldn't be read out
          offset:=currentoffset;  //calculated offset


        tempnode:=element.FindNode('Description');
        if tempnode<>nil then
          description:=tempnode.TextContent;

        {
        tempnode:=element.FindNode('PointerTo');
        tempnode:=element.FindNode('PointerToSize');   }


        tempnode:=element.FindNode('Structurenr');
        if tempnode<>nil then
        begin
          displaymethod:=dtUnsignedInteger;
          structurenr:=strtoint(tempnode.TextContent);
          case structurenr of
            -1,-2,-3: //byte
            begin
              vartype:=vtByte;
              bytesize:=1;

              if structurenr=-2 then
                displaymethod:=dtSignedInteger
              else
              if structurenr=-3 then
                displaymethod:=dtHexadecimal;
            end;

            -4,-5,-6: //word
            begin
              vartype:=vtWord;
              bytesize:=2;

              if structurenr=-5 then
                displaymethod:=dtSignedInteger
              else
              if structurenr=-6 then
                displaymethod:=dtHexadecimal;
            end;

            -7,-8,-9: //dword
            begin
              vartype:=vtDWord;
              bytesize:=4;

              if structurenr=-8 then
                displaymethod:=dtSignedInteger
              else
              if structurenr=-9 then
                displaymethod:=dtHexadecimal;
            end;

            -10,-11: //qword
            begin
              vartype:=vtQWord;
              bytesize:=8;

              if structurenr=-11 then
                displaymethod:=dtHexadecimal;
            end;

            -12: //single
            begin
              vartype:=vtSingle;
              bytesize:=4;
            end;

            -13: //double
            begin
              vartype:=vtDouble;
              bytesize:=8;
            end;

            -14: //string
            begin
              vartype:=vtString;
            end;

            -15: //unicode string
            begin
              vartype:=vtUnicodeString;
            end;
          end;
        end;

        tempnode:=element.FindNode('Bytesize');
        if tempnode<>nil then
          bytesize:=strtoint(tempnode.TextContent);


        se:=addElement(description, offset, vartype,nil, bytesize, nil);
        se.DisplayMethod:=displaymethod;

        currentoffset:=offset+Bytesize;
      end;


    end; //structure.NodeName='Structure'
  finally
    endupdate;
  end;
end;

constructor TDissectedStruct.createFromXMLNode(structure: TDOMNode);
//Create the structure with the given data and possible local structs as well
var
  elementnodes, elementnode: TDOMElement;
  childnode: TDOMElement;
  i: integer;
  se: TStructelement;

  offset: integer;
  description: string;
  vartype: TVariableType;
  customtype: TCustomType;
  bytesize: integer;
  displaymethod :TdisplayMethod;


  childstruct: TDissectedStruct;
  childname: string;
  ChildStructStartS: string;
  ChildStructStart: integer;
begin
  self.name:='';
  structelementlist:=tlist.Create;
  autoCreateStructsize:=4096; //default autocreate size
  setupDefaultSettings;

  beginUpdate;
  try
    if structure.NodeName='Structure' then
    begin
      self.name:=TDOMElement(structure).GetAttribute('Name');

      fDoNotSaveLocal:=TDOMElement(structure).GetAttribute('DoNotSaveLocal')='1';
      fAutoCreate:=TDOMElement(structure).GetAttribute('AutoCreate')='1';
      fAutoCreateStructsize:=StrToIntDef(TDOMElement(structure).GetAttribute('AutoCreate'), 4096);
      fAutoDestroy:=TDOMElement(structure).GetAttribute('AutoDestroy')='1';
      fAutoFill:=TDOMElement(structure).GetAttribute('AutoFill')='1';
      fDefaultHex:=TDOMElement(structure).GetAttribute('DefaultHex')='1';


      elementnodes:=TDOMElement(structure.FindNode('Elements'));
      if elementnodes<>nil then
      begin
        for i:=0 to elementnodes.ChildNodes.Count-1 do
        begin
          offset:=strtoint(tdomelement(elementnodes.ChildNodes[i]).GetAttribute('Offset'));
          description:=AnsiToUtf8(tdomelement(elementnodes.ChildNodes[i]).GetAttribute('Description'));
          vartype:=StringToVariableType(tdomelement(elementnodes.ChildNodes[i]).GetAttribute('Vartype'));
          CustomType:=GetCustomTypeFromName(tdomelement(elementnodes.ChildNodes[i]).GetAttribute('Customtype'));
          bytesize:=strtoint(tdomelement(elementnodes.ChildNodes[i]).GetAttribute('Bytesize'));
          displaymethod:=StringToDisplayMethod(tdomelement(elementnodes.ChildNodes[i]).GetAttribute('DisplayMethod'));

          ChildStructStartS:=tdomelement(elementnodes.ChildNodes[i]).GetAttribute('ChildStructStart');
          if ChildStructStartS<>'' then
            ChildStructStart:=strtoint(ChildStructStartS)
          else
            ChildStructStart:=0;


          childstruct:=nil;
          childname:='';
          if vartype=vtPointer then
          begin
            //check if it has a child struct
            childnode:=TDOMElement(elementnodes.ChildNodes[i].FindNode('Structure'));
            if childnode<>nil then
              childstruct:=TDissectedStruct.createFromXMLNode(childnode)
            else
              childname:=AnsiToUtf8(tdomelement(elementnodes.ChildNodes[i]).GetAttribute('ChildStruct'));
          end;

          se:=addElement(description, offset, vartype, customtype, bytesize, childstruct);
          se.DisplayMethod:=displaymethod;

          if (childstruct=nil) and (childname<>'') then
            se.delayLoadedStructname:=childname;

          se.ChildStructStart:=ChildStructStart;

        end;
      end;
    end;
  finally
    endUpdate;
  end;
end;

constructor TDissectedStruct.create(name: string);
begin
  self.name:=name;
  structelementlist:=tlist.Create;

  autoCreateStructsize:=4096; //default autocreate size
  setupDefaultSettings;
end;

destructor TDissectedStruct.destroy;
var i: integer;
begin
  beginUpdate; //never endupdate


  if structelementlist<>nil then
  begin
    while structelementlist.Count>0 do
      TStructelement(structelementlist.Items[0]).free;

    freeandnil(structelementlist);
  end;

  DoDeleteStructNotification;

  removeFromGlobalStructList;


  inherited destroy;
end;


{ TStructColumn }

procedure TStructColumn.setNewParent(group: TStructGroup);
begin
  parent.fcolumns.Remove(self);

  if parent.fcolumns.Count=0 then //group has 0 entries , destroy the group
    parent.Free;


  parent:=group;
  parent.fcolumns.Add(self);

  edtAddress.parent:=group.box;
  focusedShape.parent:=group.box;

  group.setPositions; //update the gui
end;

procedure TStructColumn.DeleteClick(sender: TObject);
begin
  if parent.parent.columnCount>1 then //leave one intact
    free;
end;

procedure TStructColumn.ChangeGroupClick(sender: tobject);
var
  grouplist: TStringList;
  l: TfrmSelectionList;
  i: integer;

  g: TStructGroup;
  newname: string;
begin
  //input a window to choose which group
  grouplist:=TStringList.create;
  for i:=0 to parent.parent.groupcount-1 do
    grouplist.AddObject(parent.parent.group[i].groupname, parent.parent.group[i]);

  grouplist.AddObject('<New group>',nil);

  l := TfrmSelectionList.Create(parent.parent, grouplist);
  l.Caption := 'Group picker';
  l.label1.Caption := 'Select the group this column should become part of';
  l.ItemIndex := 0;

  if (l.showmodal = mrOk) and (l.ItemIndex <> -1) then
  begin
    //apply change
    if grouplist.Objects[l.itemindex]=nil then //new group
    begin
      newname:='Group '+inttostr(parent.parent.groupcount+1);
      if inputquery('New group','Give the new name', newname) then
        g:=TStructGroup.create(parent.parent, newname)
      else
        exit; //no new group, no change
    end
    else
      g:=TStructGroup(grouplist.Objects[l.itemindex]);

    setNewParent(g);
  end;
  l.free;
  grouplist.free;





end;

function TStructColumn.getGlobalIndex: integer;
var i: integer;
begin
  //can be optimized
  result:=-1;
  for i:=0 to parent.parent.columnCount-1 do
    if parent.parent.columns[i]=self then
    begin
      result:=-1;
      exit;
    end;
end;

function TStructColumn.isXInColumn(x: integer): boolean;
var i: integer;
  hsection: Theadersection;
begin
  //check if this x position is in the current columns
  i:=getGlobalIndex;
  hsection:=parent.parent.HeaderControl1.Sections[i+1];
  result:=InRange(x, hsection.left, hsection.Right);
end;

procedure TStructColumn.focus;
begin
  focused:=true;
end;

procedure TStructColumn.edtAddressEnter(sender: tobject);
begin
  focus;
end;

procedure TStructColumn.edtaddressMousedown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  focus;
end;

function TStructColumn.getFocused:boolean;
begin
  result:=fFocused;
end;

procedure TStructColumn.setFocused(state: boolean);
var i: integer;
begin
  if fFocused=state then exit;

  //sets this column as the focused column
  //(there can be only one focused column)


  if state then //set all others to false
  begin
    for i:=0 to parent.parent.columncount-1 do
      if parent.parent.columns[i]<>self then
        parent.parent.columns[i].focused:=false;
  end;

  //make focus visible
  focusedShape.visible:=state;
  fFocused:=state;
end;

procedure TStructColumn.clearSavedState;
begin

  if fsavedstate<>nil then
  begin
    VirtualFreeEx(processhandle, fsavedstate, fsavedstatesize, MEM_RELEASE);
    fsavedstatesize:=0;
    fsavedstate:=nil;
  end;

  miToggleLock.Checked:=false;

end;

function TStructColumn.getAddress: ptruint;
begin
  result:=faddress;
end;

procedure TStructColumn.setAddress(address: ptruint);
begin
  clearSavedState;
  faddress:=address;

  edtAddress.Text:=IntToHex(faddress,8);
end;

function TStructColumn.saveState: boolean;
var buf: pointer;
  x: dword;
begin
  clearSavedState;
  result:=false;

  if parent.parent.MainStruct<>nil then
  begin
    fsavedstatesize:=parent.parent.MainStruct.getStructureSize;
    getmem(buf, fsavedstatesize);
    try
      if readprocessmemory(processhandle, pointer(faddress), buf, fsavedstatesize, x) then
      begin
        fsavedstate:=VirtualAllocEx(processhandle, nil, fsavedstatesize, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
        if fsavedstate<>nil then
        begin
          //copy the original bytes to the copy
          if WriteProcessMemory(processhandle, pointer(fsavedstate), buf, fsavedstatesize, x) then
            result:=true
          else
            VirtualFreeEx(processhandle, fsavedstate, fsavedstatesize, MEM_RELEASE);   //copy failed for some unknown reason, free the allocated buffer
        end;
      end;

    finally
      freemem(buf);
    end;

  end;


end;

function TStructColumn.getSavedState: pointer;
begin
  result:=fsavedstate;
end;

function TStructColumn.getSavedStateSize: integer;
begin
  if fsavedstate<>nil then
    result:=fsavedstatesize
  else
    result:=0;
end;

function TStructColumn.getEditWidth: integer;
begin
  if edtAddress<>nil then
    result:=edtAddress.Width
  else
    result:=0;
end;

function TStructColumn.getEditLeft: integer;
begin
  if edtAddress<>nil then
    result:=edtAddress.left
  else
    result:=0;
end;

procedure TStructColumn.edtAddressChange(sender: TObject);
var
  invalidaddress: boolean;
  a: ptruint;
begin
  //get the string in the editbox and parse it. On error, indicate with red text
  a:=symhandler.getAddressFromName(edtAddress.Text, false, invalidaddress);

  if invalidaddress then
    edtAddress.Font.Color:=clred
  else
  begin
    faddress:=a;
    edtAddress.Font.Color:=clWindowText;
  end;

  parent.parent.tvStructureView.Refresh;
end;

procedure TStructColumn.ToggleLockClick(sender: TObject);
begin
  //togglelock is not set to autocheck, so if it's not checked, it needs to be checked

  if not miToggleLock.Checked then
    miToggleLock.Checked:=saveState
  else
    clearSavedState;

  parent.parent.tvStructureView.Refresh;
end;

procedure TStructColumn.SetProperEditboxPosition;
var
  i: integer;
  edtWidth: integer;
  edtLeft: integer;
begin
  i:=parent.fcolumns.IndexOf(self);

  if i=0 then
  begin
    edtWidth:=120;
    edtLeft:=3;
  end
  else
  begin
    //get the editwidth of the previous item in the columns list
    edtWidth:=parent.columns[i-1].EditWidth;
    edtLeft:=parent.columns[i-1].EditLeft+edtWidth+3;
  end;

  edtAddress.left:=edtLeft;
  edtAddress.ClientWidth:=edtWidth;


  edtAddress.top:=(parent.box.clientHeight div 2)-(edtAddress.height div 2);

  focusedShape.Left:=edtAddress.left-1;
  focusedShape.Width:=edtAddress.width+2;
  focusedShape.Top:=edtAddress.top-1;
  focusedShape.Height:=edtAddress.Height+2;

end;

constructor TStructColumn.create(parent: TStructGroup);
var hsection: THeaderSection;
begin
  if parent=nil then raise exception.create('TStructColumn.create Error');
  self.parent:=parent;

  columneditpopupmenu:=TPopupMenu.Create(parent.parent);

  miToggleLock:=TMenuItem.create(columneditpopupmenu);
  miToggleLock.caption:='Lock';
  miToggleLock.OnClick:=ToggleLockClick;
  columneditpopupmenu.Items.Add(miToggleLock);

  miChangeGroup:=TMenuItem.Create(columneditpopupmenu);
  miChangeGroup.Caption:='Change Group';
  miChangeGroup.OnClick:=ChangeGroupClick;
  columneditpopupmenu.Items.Add(miChangeGroup);

  miDelete:=TMenuItem.create(columneditpopupmenu);
  miDelete.caption:='Delete address';
  miDelete.OnClick:=DeleteClick;
  columneditpopupmenu.Items.Add(miDelete);




  parent.fcolumns.add(self);

  focusedShape:=TShape.Create(parent.parent);
  focusedShape.Shape:=stRectangle;  //stRoundRect;
  focusedShape.visible:=false;
  focusedShape.parent:=parent.box;

  edtAddress:=TEdit.Create(parent.parent);
  edtAddress.Tag:=ptruint(self);
  edtAddress.OnChange:=edtAddressChange;
  edtAddress.PopupMenu:=columneditpopupmenu;
  edtAddress.OnEnter:=edtAddressEnter;
  edtAddress.Parent:=parent.box;
  edtAddress.OnMouseDown:=edtaddressMousedown;

  hsection:=parent.parent.headercontrol1.Sections.Add;
  hsection.Text:=rsAddressValue;
  hsection.Width:=parent.parent.headercontrol1.Sections[parent.parent.headercontrol1.Sections.Count-2].width;
  hsection.MinWidth:=20;

  parent.setPositions;
end;

destructor TStructColumn.destroy;
var i: integer;
begin
  parent.fcolumns.remove(self);
  parent.parent.headercontrol1.Sections.Delete(parent.parent.HeaderControl1.Sections.Count-1);

  clearSavedState;

  if edtAddress<>nil then
    freeandnil(edtAddress);

  if focusedShape<>nil then
    focusedShape.free;

  parent.setPositions;

  if parent.fcolumns.Count=0 then
    parent.Free;

end;

{ Tstructgroup }
procedure TStructGroup.addString(s:string);
begin
  if isempty then
  begin
    fcurrentString:=s;
    isempty:=false;
  end
  else
    if fcurrentString<>s then
      fMatches:=false;
end;

procedure TStructGroup.clear;
begin
  fMatches:=true;
  isempty:=true;
end;

procedure TStructGroup.setGroupName(newname: string);
begin
  groupbox.Caption:=newname;
  fgroupname:=newname;
end;

procedure TStructGroup.RenameClick(sender: tobject);
var newname: string;
begin
  newname:=groupname;
  if inputquery('Rename group', 'Give the new name for the group', newname) then
    groupname:=newname;
end;

procedure TStructGroup.DeleteClick(sender: tobject);
begin
  //delete he current group if it's not the last one
  if parent.groupcount>1 then
    free;
end;

function TStructGroup.getColumnCount: integer;
begin
  result:=fcolumns.count;
end;

function TStructGroup.getColumn(i: integer): TStructColumn;
begin
  result:=fcolumns[i];
end;

procedure TStructGroup.setPositions;
var i,j: integer;
begin

  for i:=0 to parent.groupcount-1 do
  begin
    //update the editboxes inside each group
    for j:=0 to parent.group[i].columnCount-1 do
      parent.group[i].columns[j].SetProperEditboxPosition;

    //and then update the groupbox to fit in he grouppanel and right size
    //set the left position
    if i=0 then
      parent.group[i].box.Left:=3
    else
      parent.group[i].box.Left:=parent.group[i-1].box.Left+parent.group[i-1].box.Width+10;

    //set the width
    if parent.group[i].columnCount>0 then
      parent.group[i].box.width:=parent.group[i].columns[parent.group[i].columnCount-1].EditLeft+parent.group[i].columns[parent.group[i].columnCount-1].EditWidth+10
    else
      parent.group[i].box.width:=20;

  end;

end;

constructor TStructGroup.create(parent: TfrmStructures2; GroupName: string);
begin
  self.parent:=parent;
  parent.fgroups.Add(self);

  fGroupName:=groupname;

  fcolumns:=tlist.create;

  grouppopup:=Tpopupmenu.create(parent);
  miRename:=TmenuItem.create(grouppopup);
  miRename.caption:='Rename';
  miRename.OnClick:=RenameClick;
  grouppopup.items.Add(miRename);

  miDelete:=TMenuItem.create(grouppopup);
  miDelete.caption:='Delete group';
  miDelete.OnClick:=DeleteClick;
  grouppopup.items.Add(miDelete);

  //create the groupbox
  GroupBox:=TGroupBox.Create(parent);
  GroupBox.Caption:=groupname;
  GroupBox.height:=parent.pnlGroups.ClientHeight;
  groupbox.parent:=parent.pnlGroups;

  groupbox.popupmenu:=grouppopup;
end;

destructor TStructGroup.destroy;
var i: integer;
begin
  //delete all the columns first
  parent.fgroups.Remove(self);

  while fcolumns.count>0 do
    TStructColumn(fcolumns[0]).free;

  if groupbox<>nil then
    freeandnil(groupbox);

  if grouppopup<>nil then
    freeandnil(grouppopup);

  setPositions;

  inherited destroy;
end;

{ TfrmStructures2 }

procedure TfrmStructures2.TreeViewVScroll(sender: TObject);
begin
  RefreshVisibleNodes;
end;

procedure TfrmStructures2.TreeViewHScroll(sender: TObject; scrolledleft, maxscrolledleft: integer);
begin
  HeaderControl1.Left:=-scrolledleft;
  HeaderControl1.Width:=tvStructureView.clientwidth  +maxscrolledleft+100;
end;


procedure TfrmStructures2.FormDestroy(Sender: TObject);
var showaddress: integer;
  descriptionsize: integer;
  autoguess: integer;
  defaultstructsize: integer;
begin
  frmStructures2.Remove(self);

  //save the settings
  if miShowAddresses.checked then showaddress:=1 else showaddress:=0;
  if miAutoCreate.checked then autoguess:=1 else autoguess:=0;

  descriptionsize:=HeaderControl1.Sections[0].Width;

  SaveFormPosition(self, [showaddress,autoguess, descriptionsize]);
end;

procedure TfrmStructures2.FormCreate(Sender: TObject);
var x: array of integer;
begin
  //set default colors

  if frmStructuresConfig=nil then
    frmStructuresConfig:=TfrmStructuresConfig.Create(self);

  tvStructureView.Top:=headercontrol1.top+headercontrol1.height;
  tvStructureView.left:=0;
  tvStructureView.width:=clientwidth;
  tvStructureView.height:=clientheight-tvStructureView.Top;

  tvStructureView.onHScroll:=TreeViewHScroll;
  tvStructureView.onVScroll:=TreeViewVScroll;

  frmStructures2.Add(self);

  RefreshStructureList; //get the current structure list

  fgroups:=tlist.create;

  setlength(x,3);
  if LoadFormPosition(self, x) then
  begin
    if length(x)>0 then
    begin
      miShowAddresses.checked:=x[0]=1;
      miAutoCreate.checked:=x[1]=1;
      HeaderControl1.Sections[0].Width:=x[2];
    end;
  end;


  setupColors; //load colors and default struct options
end;

procedure TfrmStructures2.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=cafree;
end;




procedure TfrmStructures2.FormShow(Sender: TObject);
begin
  addColumn;
  columns[0].setAddress(initialaddress);
end;

procedure TfrmStructures2.HeaderControl1SectionTrack(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection; Width: Integer;
  State: TSectionTrackState);
var x: integer;
    s: string;

begin

  x:=(HeaderControl1.Sections[HeaderControl1.Sections.Count-1].Left+HeaderControl1.Sections[HeaderControl1.Sections.Count-1].Width);
  tvStructureView.ClientWidth:=x;


  if tvStructureView.Items.Count>0 then
  begin
    //make the first line (structname) as long as x
    if mainStruct<>nil then
    begin
      s:='                                         ';

      while tvStructureView.Canvas.TextWidth(s)<x do
        s:=s+'        ';

      tvStructureView.items[0].Text:=s;
    end;

  end;

  tvStructureView.Refresh;

end;

procedure TfrmStructures2.RefreshVisibleNodes;
begin
  tvStructureView.Repaint;
end;

procedure TfrmStructures2.getPointerFromNode(node: TTreenode;
  column: TStructcolumn; var baseaddress: ptruint; var offsetlist: toffsetlist);
var
  i: integer;
  lastoffsetentry: integer;
  offset0: integer; //the offset at the base of the structure
  prevnode: TTreenode;
  displacement: integer;

  parentelement: TStructelement;
begin
  baseaddress:=column.Address;
  setlength(offsetlist,0);

  if (node=nil) or (node.level=0) then exit; //first entry in the mainstruct

  setlength(offsetlist, node.Level-1);
  lastoffsetentry:=node.level-2;


  i:=0;
  while node.level>1 do
  begin
    prevnode:=node.parent;

    parentelement:=getStructElementFromNode(node.parent);
    if parentelement<>nil then
      displacement:=parentelement.ChildStructStart
    else
      displacement:=0;

    offsetlist[i]:=getStructElementFromNode(node).Offset-displacement;
    inc(i);

    node:=prevnode;
  end;

  //now at node.level=1
  //add the starting offset
  inc(baseaddress, getStructElementFromNode(node).Offset);
end;

function TfrmStructures2.getAddressFromNode(node: TTreenode; column: TStructColumn; var hasError: boolean): ptruint;
//Find out the address of this node
var
  baseaddress: ptruint;
  offsets: array of integer;
begin
  getPointerFromNode(node,column, baseaddress, offsets);
  result:=getPointerAddress(baseaddress,  offsets, hasError);
end;


procedure TfrmStructures2.setCurrentNodeStringsInColumns(node: TTreenode; element: TStructElement);
{
This method will get the address and value of the current node and store them temporarily in the column for the renderer to fetch
}
var
    i: integer;
    error: boolean;
    address, displayaddress: ptruint;

    groupvalue: string;
    allmatch: boolean;
    allsame: boolean;

    c: TStructColumn;
    savedstate: ptruint;
begin
  if element<>nil then
  begin
    for i:=0 to groupcount-1 do
      group[i].clear;

    for i:=0 to columnCount-1 do
    begin
      c:=columns[i];

      displayaddress:=getAddressFromNode(node, columns[i], error);  //get the address to show the user

      savedstate:=ptruint(c.getSavedState);
      if (savedstate<>0) and (node.level=1) then //locked and it's the first level (main), set the address to actually read from
        address:=savedstate+element.Offset
      else
        address:=displayaddress;

      if not error then
      begin
        c.currentNodeAddress:=inttohex(displayaddress,1)+' : ';
        c.currentNodeValue:=element.getValue(address);
      end
      else
      begin
        c.currentNodeAddress:='??? : ';
        c.currentNodeValue:='???';
      end;

      //add this string for comparison
      c.parent.addString(c.currentNodeValue);
    end;

    //now find out which groups have matching sets
    allmatch:=true;
    allsame:=true;

    groupvalue:='';
    for i:=0 to groupcount-1 do
    begin
      if group[i].Matches=false then
      begin
        allmatch:=false;
        allsame:=false;
        break;
      end;

      if i=0 then
        groupvalue:=group[0].currentString
      else
      begin
        if groupvalue<>group[i].currentString then
          allsame:=false;
      end;
    end;


    for i:=0 to columncount-1 do
    begin
      c:=columns[i];

      if c.parent.Matches then
      begin
        c.currentNodeColor:=fMatchColor;  //default match color

        if (groupcount>1) and allmatch then //if all the groups have columns that match, and more than 1 group, then
        begin
          if allsame then //if all the groups have the same value then
            c.currentNodeColor:=fAllMatchColorSame
          else
            c.currentNodeColor:=fAllMatchColorDiff;
        end;
      end
      else
        c.currentNodeColor:=fNoMatchColor;
    end;

  end
  else
  begin
    //invalid struct
    for i:=0 to columnCount-1 do
    begin
      c:=columns[i];
      c.currentNodeColor:=tvStructureView.BackgroundColor;
      c.currentNodeValue:='';
      c.currentNodeAddress:='';
    end;
  end;
end;

procedure TfrmStructures2.setupNodeWithElement(node: TTreenode; element: TStructElement);
begin
  if (element.isPointer) then
  begin
    node.Data:=element.ChildStruct;
    node.HasChildren:=true;
  end
  else
  begin
    //an update caused this node to lose it's pointerstate. If it had children, it doesn't anymore
    node.data:=nil;
    if node.HasChildren then
      node.DeleteChildren;

    node.haschildren:=false;
  end;
end;

procedure TfrmStructures2.FillTreenodeWithStructData(currentnode: TTreenode);
var
  struct: TDissectedStruct;
  se: TStructelement;
  newnode: TTreenode;
  i: integer;
  startindex: integer;
begin
  tvStructureView.OnExpanded:=nil;
  tvStructureView.OnCollapsed:=nil;

  tvStructureView.BeginUpdate;
  currentnode.DeleteChildren;

  struct:=TDissectedStruct(currentnode.data);



  if struct<>nil then
  begin
    //get the start index of this structure
    startindex:=0;
    se:=getStructElementFromNode(currentnode);
    if (se<>nil) and (se.ChildStructStart<>0) then
      startindex:=struct.getIndexOfOffset(se.ChildStructStart);

    for i:=startindex to struct.count-1 do
    begin
      newnode:=tvStructureView.Items.AddChild(currentnode,'');
      setupNodeWithElement(newnode, struct[i]);
    end;

    currentnode.HasChildren:=true;
    currentnode.Expand(false);
  end;

  tvStructureView.EndUpdate;

  tvStructureView.OnExpanded:=tvStructureViewExpanded;
  tvStructureView.OnCollapsed:=tvStructureViewCollapsed;
end;


procedure TfrmStructures2.tvStructureViewCollapsed(Sender: TObject; Node: TTreeNode);
var struct, childstruct: TDissectedStruct;
begin
  tvStructureView.BeginUpdate;
  node.DeleteChildren; //delete the children when collapsed

  if node.parent<>nil then //almost always, and then it IS a child
  begin
    //get the structure this node belongs to

    struct:=getStructFromNode(node);

    //now get the element this node represents and check if it is a pointer
    node.HasChildren:=struct[node.Index].isPointer;

    if miAutoDestroyLocal.checked then //delete autocreated local structs when closed
    begin
      childstruct:=TDissectedStruct(node.data);
      if childstruct<>nil then
      begin
        if not childstruct.isInGlobalStructList then
        begin
          //delete this local struct
          childstruct.free;

          {$ifdef DEBUG}
          assert(node.data=nil);
          {$endif}
          node.data:=nil;   //not necesary
        end;
      end;

    end;

  end
  else //root node (mainstruct)
  if node.data<>nil then //weird if not...
  begin
    node.HasChildren:=true;
    node.Expand(false); //causes the expand the fill in the nodes
  end;

  tvStructureView.EndUpdate;




end;

procedure TfrmStructures2.tvStructureViewCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse:=node.level>0;


end;

procedure TfrmStructures2.tvStructureViewExpanded(Sender: TObject;
  Node: TTreeNode);
begin
  if node.data<>nil then
    FillTreenodeWithStructData(node)
end;

procedure TfrmStructures2.tvStructureViewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var n: TStructelement;
  error: boolean;
  address: ptruint;
  c: TStructColumn;
  x: dword;
begin
  AllowExpansion:=true;
  n:=getStructElementFromNode(node);
  if (n<>nil) and (n.isPointer) and (n.ChildStruct=nil) then
  begin
    if miAutoCreate.Checked then
    begin

      //create a structure
      c:=getFocusedColumn;
      if c=nil then
        c:=columns[0];

      address:=getAddressFromNode(node, c, error);

      if not error then
      begin
        //dereference the pointer and fill it in if possible
        if ReadProcessMemory(processhandle, pointer(address), @address, processhandler.pointersize, x) then
          n.AutoCreateChildStruct('Autocreated from '+inttohex(address,8), address);
      end;

      AllowExpansion:=not error;
    end
    else
      AllowExpansion:=false;

  end;
end;

function TfrmStructures2.getMainStruct: TDissectedStruct;
begin
  result:=mainStruct;
end;

procedure TfrmStructures2.InitializeFirstNode;
//Clear the screen and setup the first node
var tn: TTreenode;
begin
  tvStructureView.Items.Clear;
  if mainStruct<>nil then
  begin
    tn:=tvStructureView.Items.Add(nil, '');
    tn.Data:=mainStruct;
    tn.HasChildren:=true;
    tn.Expand(false);
  end;
end;

procedure TfrmStructures2.onAddedToStructList(sender: TDissectedStruct);
begin
  RefreshStructureList;
end;

procedure TfrmStructures2.onRemovedFromStructList(sender: TDissectedStruct);
begin
  RefreshStructureList;
end;

procedure TfrmStructures2.onStructureDelete(sender: TDissectedStruct);
begin
  if sender=mainStruct then
  begin
    mainstruct:=nil;
    tvStructureView.Items.Clear;
  end;
end;

procedure TfrmStructures2.onStructOptionsChange(sender: TDissectedStruct);
begin
  if mainStruct=sender then
    UpdateCurrentStructOptions;
end;

procedure TfrmStructures2.onFullStructChange(sender: TDissectedStruct);
var currentNode: TTreenode;
    nextnode: TTreenode;
begin
  //update the childnode of the treenode with this struct to represent the new state
  if mainStruct<>nil then
  begin

    currentNode:=tvStructureView.Items.GetFirstNode;
    if currentnode=nil then
    begin
      InitializeFirstNode;
      currentNode:=tvStructureView.Items.GetFirstNode;
    end;

    while currentnode<>nil do
    begin
      //go through all entries

      //check if currentnode.data is of the type that needs to be updated
      if (currentnode.Data=sender) and (currentnode.Expanded or (currentNode.level=0)) then  //node is of the updated type and currently has children , or it's the root node
        FillTreeNodeWithStructData(currentnode);


      //nothing else to be done, get the next one
      nextnode:=currentnode.GetFirstChild;
      if nextnode=nil then
        nextnode:=currentnode.GetNextSibling;
      if nextnode=nil then
      begin
        //up one level
        nextnode:=currentnode.Parent;
        if nextnode<>nil then
          nextnode:=nextnode.GetNextSibling;
      end;
      currentnode:=nextnode;
    end;

  end;


  //else nothing to update

  //and also the structure list in case it's one I didn't know of
  RefreshStructureList;
end;

procedure TfrmStructures2.onElementChange(struct:TDissectedStruct; element: TStructelement);
var i: integer;
    n: Ttreenode;
begin
  //find the treenodes that belong to this specific element and change them accordingly
  for i:=0 to tvStructureView.Items.Count-1 do
    if tvStructureView.Items[i].Data=struct then //this node contains the element
    begin
      if tvStructureView.Items[i].Expanded then
      begin
        //it's expanded so visible. Find the specific node and apply a update
        n:=tvStructureView.Items[i].Items[element.index];

        setupNodeWithElement(n, element);
      end;
    end;



end;

procedure TfrmStructures2.Definenewstructure1Click(Sender: TObject);
var
  structName: string;
  autoFillIn: integer;
  sstructsize: string;
  structsize: integer;
begin
  structname:=rsUnnamedStructure;

  //get the name
  if not inputquery(rsStructureDefine, rsGiveTheNameForThisStructure, structName) then exit;

  //ask if it should be filled in automatically
  autoFillIn:=messagedlg(rsDoYouWantCheatEngineToTryAndFillInTheMostBasicType, mtconfirmation, [mbyes, mbno, mbcancel], 0);
  if autoFillIn=mrcancel then exit;

  mainStruct:=nil;
  tvStructureView.items.clear;


  mainStruct:=TDissectedStruct.create(structname);

  if autofillin=mryes then
  begin
    sstructsize:='4096';
    if not inputquery(rsStructureDefine, rsPleaseGiveAStartingSizeOfTheStructYouCanChangeThis, Sstructsize) then exit;
    structsize:=strtoint(sstructsize);

    mainStruct.autoGuessStruct(TStructColumn(columns[0]).getAddress, 0, structsize );
  end;

  mainStruct.addToGlobalStructList;
  UpdateCurrentStructOptions;
end;


function TfrmStructures2.getStructElementFromNode(node: TTreenode): TStructelement;
var i: integer;
  s: TDissectedStruct;
begin
  result:=nil;
  if (node<>nil) and (node.level>0) then
  begin
    i:=0;
    s:=getStructFromNode(node.parent);
    if s<>nil then
    begin
      i:=s[node.parent.Index].ChildStructStart;
      i:=getStructFromNode(node).getIndexOfOffset(i);
    end;

    result:=getStructFromNode(node)[node.index+i];
  end;

end;

function TfrmStructures2.getStructFromNode(node: TTreenode): TDissectedStruct;
begin
  result:=mainStruct;

  if node<>nil then
  begin
    node:=node.parent;

    if node<>nil then
      result:=TDissectedStruct(node.data);
  end;
end;

function TfrmStructures2.getChildStructFromNode(node: TTreenode): TDissectedStruct;
begin
  result:=nil;
  if node=nil then exit;

  result:=TDissectedStruct(node.data);
end;

procedure TfrmStructures2.changeNodes;
var
  s, structelement: TStructElement;
  n: TTreenode;
  i: integer;
  ei: TfrmStructures2ElementInfo;
begin
  n:=tvStructureView.selected;
  if n=nil then exit;

  structElement:=getStructElementFromNode(n);
  if structElement=nil then exit;

  ei:=tfrmstructures2ElementInfo.create(self);

  with ei do
  begin
    description:=structelement.name;
    offset:=structelement.offset;
    vartype:=structelement.vartype;
    customtype:=structelement.CustomType;

    bytesize:=structelement.bytesize;
    childstruct:=structelement.childstruct;
    hexadecimal:=structelement.displayMethod=dtHexadecimal;
    signed:=structelement.displaymethod=dtSignedInteger;

    if tvStructureView.SelectionCount>1 then
      edtOffset.Enabled:=false;

    //fill in basic info



    if showmodal=mrok then
    begin
      for i:=0 to tvStructureView.SelectionCount-1 do
      begin
        tvStructureView.Selections[i].Collapse(true); //close the selections (destroys autocreated structure nodes)

        structElement:=getStructElementFromNode(tvStructureView.Selections[i]);
        if structelement=nil then continue;


        structElement.parent.beginUpdate;
        try
          structElement.name:=description;

          if tvStructureView.SelectionCount=1 then //only update the offset if only one entry is selected (e.g the user might be so stupid to select a level 1 and a level 3 of a completly different structure....)
            structElement.offset:=offset;

          structElement.vartype:=vartype;
          structElement.CustomType:=customtype;
          structElement.bytesize:=bytesize;
          structElement.childstruct:=childstruct;
          if hexadecimal then
            structelement.displayMethod:=dtHexadecimal
          else
          if signed then
            structelement.displayMethod:=dtSignedInteger
          else
            structelement.displayMethod:=dtUnsignedInteger;

          structelement.ChildStructStart:=childstructstart;

        finally
          structElement.parent.endupdate;
        end;

      end;
    end;

    free;
  end;

end;

procedure TfrmStructures2.addFromNode(n: TTreenode; asChild: boolean=false);
var
  struct: TDissectedStruct;
  structElement: TStructElement;
begin
  if asChild then
    struct:=getChildStructFromNode(n)
  else
    struct:=getStructFromNode(n);  //find out what node this belongs to. (n can be nil, as that will return the mainstruct)

  if struct<>nil then
  begin
    with tfrmstructures2ElementInfo.create(self) do
    begin
      //fill in some basic info
      structElement:=getStructElementFromNode(n);



      if structElement<>nil then
      begin
        //set the default variabes to the type of the currently selected item
        vartype:=structElement.VarType;
        customtype:=structElement.CustomType;
        bytesize:=structElement.Bytesize;
        signed:=structElement.DisplayMethod=dtSignedInteger;


        if struct.DefaultHex and (vartype in [vtByte..vtQword, vtByteArray]) then   //if defaulthex is set and the previous item was an integer type then use the defaulthex's state
          hexadecimal:=struct.DefaultHex
        else
          hexadecimal:=structElement.DisplayMethod=dtHexadecimal

      end
      else
      begin
        vartype:=vtDword;
        hexadecimal:=struct.DefaultHex;
      end;

      if asChild then
        structElement:=nil //adding as child from a rootnode
      else
        structElement:=getStructElementFromNode(n); //can return nil if no node was selected, or the mainnode was selected

      if structelement=nil then
        offset:=struct.getStructureSize
      else         //clicked on a node, find the offset and add it after that
        offset:=structElement.Offset+structElement.Bytesize;

      //show the form to make modification
      if showmodal=mrok then
      begin
        structElement:=struct.addElement(description, offset, vartype, customtype, bytesize, childstruct);
        if hexadecimal then
          structelement.DisplayMethod:=dtHexadecimal
        else
        if signed then
          structElement.DisplayMethod:=dtSignedInteger
        else
          structElement.DisplayMethod:=dtUnsignedInteger; //default, but set anyhow

        //set the selection to this entry
        if not asChild then
        begin
          if (n=nil) or (n.level=0) then
            n:=tvStructureView.Items.GetFirstNode
          else
            n:=n.parent;
        end;

        tvStructureView.Items.SelectOnlyThis(n.Items[structElement.Index]);
      end;

      free;
    end;
  end;

end;

procedure TfrmStructures2.miChangeElementClick(Sender: TObject);
begin
  ChangeNodes;
end;


procedure TfrmStructures2.miAddChildElementClick(Sender: TObject);
begin
  addFromNode(tvStructureView.selected, true);
end;


procedure TfrmStructures2.miAddElementClick(Sender: TObject);
begin
  addFromNode(tvStructureView.selected);
end;

procedure TfrmStructures2.miShowAddressesClick(Sender: TObject);
begin
  RefreshVisibleNodes;
end;

procedure TfrmStructures2.miUpdateOffsetsClick(Sender: TObject);
var offsetstring: string;
  struct: TDissectedStruct;
  element: TStructelement;
  i: integer;
  offset: integer;
begin
  //get the structure and the element to start from
  struct:=getStructFromNode(tvStructureView.selected);
  if struct<>nil then
  begin
    element:=getStructElementFromNode(tvStructureView.selected);
    if (element=nil) and (struct.count>0) then
      element:=struct[0];

    if element<>nil then
    begin
      offsetstring:='0';
      if InputQuery(rsDissectData, rsHowManyBytesDoYouWantToShiftThisAndFollowingOffset, offsetstring) then
      begin

        offset:=StrToInt(offsetstring);

        struct.beginUpdate; //prevents sorting
        for i:=element.index to struct.count-1 do
          struct[i].Offset:=struct[i].offset+offset;

        struct.endUpdate;//resort and update
      end;
    end;

  end;
end;

procedure TfrmStructures2.Open1Click(Sender: TObject);
var doc: TXMLDocument;
  structnode: TDOMNode;
  s: TDissectedStruct;
begin
  if Opendialog1.Execute then
  begin
    mainstruct:=nil;
    tvStructureView.items.clear;

    ReadXMLFile(doc, Opendialog1.FileName);

    if doc<>nil then
    begin
      structnode:=doc.FindNode('Structures');

      if structnode.ChildNodes.Count>0 then
        s:=TDissectedStruct.createFromXMLNode(structnode.ChildNodes[0]);

      if s<>nil then
        s.addToGlobalStructList;

      mainstruct:=s;


      onFullStructChange(mainstruct);
      RefreshStructureList;
    end;

    doc.Free;
  end;
end;

procedure TfrmStructures2.Save1Click(Sender: TObject);
var doc: TXMLDocument;
  structnode: TDOMNode;
begin
  if mainstruct=nil then exit;

  if Savedialog1.Execute then
  begin
    doc:=TXMLDocument.Create;
    structnode:=TDOMElement(doc.AppendChild(TDOMNode(doc.CreateElement('Structures'))));

    mainstruct.WriteToXMLNode(structnode);
    WriteXML(structnode, savedialog1.filename);

    doc.Free;
  end;
end;

procedure TfrmStructures2.pmStructureViewPopup(Sender: TObject);
var childstruct: TDissectedStruct;
  ownerstruct: TDissectedStruct;
  structelement: TStructelement;
begin
  ownerstruct:=getStructFromNode(tvStructureView.selected);
  childstruct:=getChildStructFromNode(tvStructureView.selected);
  structelement:=getStructElementFromNode(tvStructureView.Selected);

  miFullUpgrade.visible:=(childstruct<>nil) and (not childstruct.isInGlobalStructList);
  miAddElement.visible:=(ownerstruct<>nil) or (childstruct<>nil);
  miAddChildElement.visible:=(childstruct<>nil);
  miDeleteElement.visible:=tvStructureView.Selected<>nil;
  miChangeElement.visible:=structElement<>nil;

  miBrowseAddress.Visible:=tvStructureView.Selected<>nil;
  miBrowsePointer.visible:=(structelement<>nil) and (structelement.isPointer);


  miChangeValue.Visible:=structelement<>nil;
  miUpdateOffsets.visible:=structelement<>nil;
  miAddToAddresslist.Visible:=structelement<>nil;

  n1.visible:=ownerstruct<>nil;
  n2.visible:=ownerstruct<>nil;
end;

procedure TfrmStructures2.miNewWindowClick(Sender: TObject);
begin
  with tfrmstructures2.create(application) do
    show;
end;

procedure TfrmStructures2.miUpdateIntervalClick(Sender: TObject);
var interval: string;
begin
  interval:=inttostr(updatetimer.interval);
  if InputQuery(rsUpdateInterval, rsNewInterval+':', interval) then
  begin
    try
      updatetimer.interval:=strtoint(interval);
    except
    end;

    miUpdateInterval.caption:=rsUpdateInterval+': '+inttostr(
      updatetimer.interval);
  end;
end;

procedure TfrmStructures2.pnlGroupsClick(Sender: TObject);
begin

end;

procedure TfrmStructures2.Renamestructure1Click(Sender: TObject);
var newname: string;
begin
  if mainstruct<>nil then
  begin
    newname:=mainStruct.name;
    if InputQuery('Give the new name for this structure', 'Structure rename', newname) then
      mainStruct.name:=newname;
  end;
end;




procedure TfrmStructures2.tvStructureViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
  c: TStructColumn;
  n: TTreenode;
begin
  c:=getColumnAtXPos(x);
  if c<>nil then
    c.focus;

  if (button=mbRight) then //lazarus 32774: Is rightclickselect is on it does not deselect other lines
  begin
    n:=tvStructureView.GetNodeAt(x,y);
    if n<>nil then
    begin
      if not ((ssShift in Shift) or (ssCtrl in Shift)) then
        tvStructureView.Items.SelectOnlyThis(n)
      else
        n.Selected:=true;
    end;
  end;
end;

function TfrmStructures2.getFocusedColumn: TStructColumn;
var i: integer;
begin
  for i:=0 to columnCount-1 do
    if columns[i].Focused then
    begin
      result:=columns[i];
      exit;
    end;

  result:=columns[0]; //nothing found, so give the first column
end;


function TfrmStructures2.getColumnAtXPos(x: integer): TStructColumn;
var i: integer;
begin
  //find which header is clicked. This coincides with the form's structure list
  result:=nil;
  for i:=1 to HeaderControl1.Sections.count-1 do
    if inrange(x, HeaderControl1.Sections[i].Left, HeaderControl1.Sections[i].Right) then
    begin
      result:=columns[i-1];
      exit;
    end;
end;



procedure TfrmStructures2.updatetimerTimer(Sender: TObject);
begin
  //refresh the visible nodes
  RefreshVisibleNodes;
end;

procedure TfrmStructures2.miDeleteElementClick(Sender: TObject);
var elementlist: Tlist;
  e: TStructelement;

  struct: TDissectedStruct;
  i: integer;

  originalindex: integer;
begin
  //save the old selection pos
  if tvStructureView.Selected<>nil then
    originalindex:=tvStructureView.Selected.AbsoluteIndex
  else
    originalindex:=-1;


  //fill the nodelist with all the selected entries that match the requirement: Same siblings only
  struct:=nil;
  elementlist:=tlist.create;
  try
    for i:=0 to tvStructureView.SelectionCount-1 do
    begin
      e:=getStructElementFromNode(tvStructureView.Selections[i]);
      if (e<>nil) and ((struct=nil) or (e.parent=struct))  then //the element can be null if it's the origin
      begin
        if struct=nil then
          struct:=e.parent;

        elementlist.add(e);
      end;
    end;

    //now delete the entries in the list (if there are any)
    if struct<>nil then
    begin
      struct.beginUpdate;
      try
        for i:=0 to elementlist.count-1 do
          struct.removeElement(TStructelement(elementlist[i]));

      finally
        struct.endUpdate;

        if (not struct.isInGlobalStructList) and (struct.count=0) then
          struct.free;
      end;
    end;

  finally
    elementlist.free;
  end;

  //restore the selection pos
  if originalindex>=0 then
    tvStructureView.Items.SelectOnlyThis(tvStructureView.Items[min(tvStructureView.items.count-1, originalindex)]);

end;

procedure TfrmStructures2.addColumn;
var c: TStructColumn;
begin
  if groupcount=0 then
    TStructGroup.create(self,'Group 1');

  c:=getFocusedColumn;
  if c=nil then
    TStructColumn.create(group[0])
  else
    TStructColumn.create(c.parent);

  RefreshVisibleNodes;
end;

procedure TfrmStructures2.removeColumn(columnid: integer);
begin
  columns[columnid].free;
  RefreshVisibleNodes;
end;

procedure TfrmStructures2.Addextraaddress1Click(Sender: TObject);
begin
  addColumn;
end;

procedure TfrmStructures2.MenuItem3Click(Sender: TObject);
var i,j: integer;
  se: TStructelement;
  c: TStructColumn;
  s,s2: string;
  f: TStringList;
  node: TTreenode;
begin
  if saveValues.execute then
  begin
    f:=tstringlist.create;
    for i:=0 to tvStructureView.Items.Count-1 do
    begin
      node:=tvStructureView.Items[i];
      s:='';
      se:=getStructElementFromNode(node);

      if se<>nil then
      begin
        for j:=0 to node.level-1 do
          s:=s+'     ';

        setCurrentNodeStringsInColumns(node,se);

        //column now contains the strings
        for j:=0 to columnCount-1 do
        begin
          c:=columns[j];

          if miShowAddresses.checked then
            s2:=c.currentNodeAddress
          else
            s2:='';

          s:=s+s2+c.currentNodeValue+'  -  ';
        end;

        s:=copy(s,1,length(s)-5); //strip the '     ' or '  -  '

      end;

      f.add(s);
    end;

    f.SaveToFile(saveValues.FileName);

    f.free;


  end;
end;

procedure TfrmStructures2.MenuItem5Click(Sender: TObject);
var s: string;
  g: Tstructgroup;
begin
  s:='Group '+inttostr(groupcount+1);
  if InputQuery('Name for this group','Structure define', s) then
  begin
    g:=TStructGroup.create(self, s);

    //add the first address as well
    TStructColumn.create(g);
  end;
end;

procedure TfrmStructures2.miGenerateGroupscanClick(Sender: TObject);
var gcf: TfrmGroupScanAlgoritmGenerator;
  previous, e: TStructelement;
  n: TTreeNode;
  err: boolean;
  address: ptruint;
  i,j: integer;
begin
  if mainstruct<>nil then
  begin
    gcf:=TfrmGroupScanAlgoritmGenerator.create(self);

    //fill the algoritm with what is currently selected

    for i:=0 to tvStructureView.items.Count-1 do //find the first selected element
    begin
      if tvStructureView.items[i].MultiSelected or tvStructureView.items[i].Selected then
      begin
        //found the first element, from here, add all selected siblings and fill in wildcards
        n:=tvStructureView.items[i];
        previous:=nil;
        while n<>nil do
        begin

          if n.MultiSelected or n.Selected then
          begin
            e:=getStructElementFromNode(n);
            if (e<>nil) then
            begin
              //add this element (if it's a valid type)
              if e.vartype in [vtByte..vtDouble, vtstring, vtunicodestring, vtCustom, vtPointer] then
              begin
                //get the address
                address:=getAddressFromNode(n, getFocusedColumn, err);
                if not err then
                begin
                  //everything ok, add it
                  if previous<>nil then //Fill gaps with the previous element
                  begin

                    j:=e.Offset-(previous.Offset+previous.Bytesize);   //get the number of bytes to fill
                    while j>0 do
                    begin
                      if j>=8 then
                      begin
                        gcf.addline(vtQword, '*');
                        dec(j,8)
                      end
                      else
                      if j>=4 then
                      begin
                        gcf.addline(vtDword, '*');
                        dec(j,4)
                      end
                      else
                      if j>=2 then
                      begin
                        gcf.addline(vtWord, '*');
                        dec(j,2)
                      end
                      else
                      begin
                        gcf.addline(vtByte, '*');
                        dec(j);
                      end;
                    end;

                  end;


                  gcf.AddLine(e.VarType, e.CustomType, e.getValue(address, true));
                  previous:=e;
                end;
              end;
            end;
          end;
          n:=n.GetNextSibling;
        end;

        //    gcf.addByte(value)

        break;
      end;

    end;

    if gcf.showmodal=mrok then
      mainform.scanvalue.text:=gcf.getparameters;

    clipboard.astext:=gcf.getparameters;

    gcf.free;

  end;
end;

procedure TfrmStructures2.miAutoCreateClick(Sender: TObject);
begin
  if mainstruct<>nil then
    mainStruct.AutoCreate:=not mainstruct.AutoCreate;
end;

procedure TfrmStructures2.miAutoDestroyLocalClick(Sender: TObject);
begin
  if mainstruct<>nil then
    mainstruct.AutoDestroy:=not mainstruct.AutoDestroy;
end;

procedure TfrmStructures2.miAutoFillGapsClick(Sender: TObject);
begin
  if mainstruct<>nil then
    mainstruct.AutoFill:=not mainstruct.AutoFill;
end;

procedure TfrmStructures2.miChangeValueClick(Sender: TObject);
begin
  EditValueOfSelectedNodes(getFocusedColumn);
end;

procedure TfrmStructures2.miBrowseAddressClick(Sender: TObject);
var
  n: ttreenode;
  a: ptruint;
  error: boolean;
  x: dword;
begin
  n:=tvStructureView.Selected;
  if n<>nil then
  begin
    a:=getAddressFromNode(n, getFocusedColumn, error);

    if not error then
      MemoryBrowser.hexview.address:=a;
  end;
end;

procedure TfrmStructures2.miBrowsePointerClick(Sender: TObject);
var
  n: ttreenode;
  a: ptruint;
  error: boolean;
  x: dword;
begin
  n:=tvStructureView.Selected;
  if n<>nil then
  begin
    a:=getAddressFromNode(n, getFocusedColumn, error);

    if not error then
    begin
      //get the address this address points to
      if ReadProcessMemory(processhandle, pointer(a), @a, processhandler.pointersize, x) and (x=processhandler.pointersize) then
        MemoryBrowser.hexview.address:=a;
    end;
  end;
end;


procedure TfrmStructures2.miAddToAddresslistClick(Sender: TObject);
var baseaddress: ptruint;
  offsetlist: array of integer;
  element, element2: TStructelement;

  sname: string;
  n: ttreenode;
  name: string;
begin
  n:=tvStructureView.Selected;
  if n<>nil then
  begin
    element:=getStructElementFromNode(n);
    if element<>nil then
    begin
      baseaddress:=0;
      getPointerFromNode(n, getFocusedColumn, baseaddress, offsetlist);
      if baseaddress<>0 then
      begin
        //add this baseaddress with it's offsetlist to the addresslist

        sname:=element.Name;
        while (n<>nil) and (n.level>=1) do
        begin
          element2:=getStructElementFromNode(n);
          if element2<>nil then
            sname:=element.name+'->'+sname;

          n:=n.parent;
        end;

        name:=element.Name;
        if name='' then
          name:=VariableTypeToString(element.VarType);

        mainform.addresslist.addaddress(name, inttohex(baseaddress,1), offsetlist, length(offsetlist), element.VarType,'',element.Bytesize);
      end;


    end;


    //mainform.a
  end;
end;

procedure TfrmStructures2.Deletecurrentstructure1Click(Sender: TObject);
begin
  if mainstruct<>nil then
  begin
    if messagedlg('Are you sure you want to delete the structure named :'+mainstruct.structname+' ?', mtConfirmation, [mbyes,mbno],0) = mryes then
    begin
      mainstruct.free;
      mainstruct:=nil; //should happen automatically thanks to the destroy procedure of the struct
    end;
  end;
end;

procedure TfrmStructures2.miAutoGuessClick(Sender: TObject);
var
  sStartOffset: string;
  sStructSize: string;
  //base: TbaseStructure;
  startOffset: integer;
  structSize: integer;
begin
  if mainStruct<>nil then
  begin
    if not inputquery(rsStructureDefine, rsPleaseGiveAStartingOffsetToEvaluate, sStartOffset) then exit;
    startOffset:=StrToInt('$'+sStartOffset);

    sStructSize:='4096';
    if not inputquery(rsStructureDefine, rsPleaseGiveTheSizeOfTheBlockToEvaluate, sStructSize) then exit;
    structSize:=StrToInt(sStructSize);


    mainStruct.autoGuessStruct(getFocusedColumn.Address+startOffset, startOffset, structsize);
  end;
end;

procedure TfrmStructures2.miAutostructsizeClick(Sender: TObject);
var newsize: string;
begin
  if mainstruct<>nil then
  begin
    newsize:=inttostr(mainstruct.autoCreateStructsize);
    if InputQuery('Autocreate structure', 'Default size:', newsize) then
      mainstruct.autoCreateStructsize:=strtoint(newsize);
  end;
end;

procedure TfrmStructures2.setupColors;
begin
  fDefaultColor:=frmStructuresConfig.defaultText;
  fMatchColor:=frmStructuresConfig.equalText;
  fNoMatchColor:=frmStructuresConfig.differentText;
  fAllMatchColorSame:=frmStructuresConfig.groupequalText;
  fAllMatchColorDiff:=frmStructuresConfig.groupDifferentText;


end;

procedure TfrmStructures2.UpdateCurrentStructOptions;
begin
  if mainStruct<>nil then
  begin
    miAutoCreate.Checked:=mainstruct.AutoCreate;
    miAutostructsize.caption:='Autocreate structure size: '+inttostr(mainstruct.autoCreateStructsize);
    miAutoDestroyLocal.Checked:=mainstruct.AutoDestroy;
    miDoNotSaveLocal.checked:=mainstruct.DoNotSaveLocal;
    miAutoFillGaps.Checked:=mainStruct.AutoFill;
    miDefaultHexadecimal.checked:=mainstruct.DefaultHex;
  end;
end;


procedure TfrmStructures2.miChangeColorsClick(Sender: TObject);
var c: TfrmStructuresConfig;
begin
  //setup the colors for the edit window
  {
  fDefaultColor: TColor;
  fNoMatchColor: TColor; //The color to use when not all elements have the same color
  fMatchColor: tcolor; //The color to use when all elements in the group match
  fAllMatchColorSame: TColor; //The color to use when all groups have matching elements AND the same value
  fAllMatchColorDiff: TColor; //The color to use when all groups have matching elements but at least one has different values between groups

  }
  {
  c.defaultText:=fDefaultColor;
  c.equalText:=fMatchColor;
  c.differentText:=fNoMatchColor;
  c.groupequalText:=fAllMatchColorSame;
  c.groupDifferentText:=fAllMatchColorDiff;




  }
  //show and wait for the user
  if frmStructuresConfig.showmodal=mrok then
  begin
    //just apply new colors
    setupColors; //gets the colors from the structures config

   { fDefaultColor:=c.defaultText;
    fMatchColor:=c.equalText;
    fNoMatchColor:=c.differentText;
    fAllMatchColorSame:=c.groupequalText;
    fAllMatchColorDiff:=c.groupDifferentText; }


    //and show the new colors
    RefreshVisibleNodes;
  end;


end;

procedure TfrmStructures2.miDefaultHexadecimalClick(Sender: TObject);
begin
  if mainstruct<>nil then
    mainstruct.DefaultHex:=not mainstruct.DefaultHex;
end;



procedure TfrmStructures2.miDoNotSaveLocalClick(Sender: TObject);
begin
  if mainstruct<>nil then
    mainstruct.DoNotSaveLocal:=not mainstruct.DoNotSaveLocal;
end;

procedure TfrmStructures2.miFillGapsClick(Sender: TObject);
begin

  if mainstruct<>nil then
    mainstruct.fillGaps(getFocusedColumn.address, true);

end;



procedure TfrmStructures2.miFullUpgradeClick(Sender: TObject);
var struct: TDissectedStruct;
begin
  struct:=getChildStructFromNode(tvStructureView.Selected);
  if struct<>nil then
    struct.addToGlobalStructList;
end;

procedure TfrmStructures2.miSelectStructureClick(Sender: tobject);
//a structure has been selected from the menu. Handle it
begin
  mainStruct:=TdissectedStruct(TmenuItem(sender).Tag);
  InitializeFirstNode;
  UpdateCurrentStructOptions;
end;

procedure TfrmStructures2.RefreshStructureList;
var
  i: integer;
  s: string;
  mi: TMenuItem;
begin
  while structures1.count>2 do
    Structures1.Delete(2);

  for i:=0 to DissectedStructs.count-1 do
  begin
    s:=TDissectedStruct(DissectedStructs[i]).structname;
    mi:=tmenuitem.Create(Structures1);
    mi.Caption:=s;
    mi.OnClick:=miSelectStructureClick;
    mi.Tag:=ptruint(DissectedStructs[i]);
    Structures1.Add(mi);
  end;
end;

procedure TfrmStructures2.setMainStruct(struct: TDissectedStruct);
begin
  fmainStruct:=struct;

  if struct=nil then
    tvStructureView.Items.Clear;

  miCommands.Enabled:=struct<>nil;
end;

function TfrmStructures2.getColumn(i: integer): TStructColumn;
//look through the groups to find this column
var j,c: integer;
begin
  result:=nil;
  c:=0;
  for j:=0 to groupcount-1 do
  begin
    if c+group[j].columnCount>i then
    begin
      result:=TStructColumn(group[j].columns[i-c]);
      exit;
    end
    else
      inc(c,group[j].columnCount);
  end;
end;


function TfrmStructures2.getColumnCount: integer;
var i: integer;
begin
  result:=0;
  for i:=0 to groupcount-1 do
    inc(result, group[i].columncount);
end;

function TfrmStructures2.getGroup(i: integer): TStructGroup;
begin
  result:=TStructGroup(fgroups[i]);
end;

function TfrmStructures2.getGroupCount: integer;
begin
  result:=fgroups.count;
end;

procedure TfrmStructures2.tvStructureViewAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  textrect: trect;
  linerect: trect;

  textlinerect: trect;
  fulltextline: trect;

  clip: TRect;

  se, se2: TStructelement;
  description: string;
  s: string;

  c: TStructColumn;
  i: integer;
  selected: boolean;

  nodescription: boolean; //if set render the description using a lighter color
  r,g,b: byte;

  displacement: integer;
  varname: string;

begin
  if mainstruct=nil then exit; //no rendering

  if stage=cdPostPaint then
  begin
    textrect:=node.DisplayRect(true);
    linerect:=node.DisplayRect(false);

    fulltextline:=linerect;
    fulltextline.Left:=textrect.Left;
    fulltextline.Right:=tvStructureView.ClientWidth;

    //get the next text
    se:=getStructElementFromNode(node);

    nodescription:=false;
    if (se=nil) then
      description:=mainstruct.name
    else
    begin
      if (se.name='') and (miShowTypeForEntriesWithNoDescription.checked) then
      begin
        nodescription:=true;

        if se.vartype=vtCustom then
          varname:=se.CustomType.name
        else
          varname:=VariableTypeToString(se.VarType);

        description:=inttohex(se.Offset,4)+' - '+varname;

        //show nondefault displaymethods
        case se.DisplayMethod of
          dtHexadecimal: description:=description+' (Hexadecimal)';
          dtSignedInteger: description:=description+' (Signed)';
        end;

        if (se.VarType=vtPointer) and (se.ChildStruct<>nil) then
        begin
          description:=description+' to '+se.ChildStruct.name;
          if se.ChildStructStart<>0 then
            description:=description+'+'+inttohex(se.ChildStructStart,1);
        end;
      end
      else
        description:=inttohex(se.Offset,4)+' - '+se.Name;
    end;

    setCurrentNodeStringsInColumns(node,se);


    //draw an empty line.
    textlinerect.left:=textrect.left;
    textlinerect.Top:=linerect.Top;
    textlinerect.Right:=max(tvStructureView.clientwidth, headercontrol1.left+headercontrol1.Sections.Items[headercontrol1.Sections.Count-1].Left+headercontrol1.Sections.Items[headercontrol1.Sections.Count-1].Width);
    textlinerect.Bottom:=linerect.Bottom;
    if textlinerect.right<textlinerect.left then
      textlinerect.right:=textlinerect.left;


    //draw the description
    clip:=textrect;
    clip.Right:=headercontrol1.left+headercontrol1.Sections[0].Left+headercontrol1.Sections[0].Width;
    selected:=(cdsSelected in State) or (cdsMarked in state);

    if selected then
      sender.Canvas.Font.Color:=InvertColor(fDefaultColor)
    else
      sender.Canvas.Font.Color:=fDefaultColor;

    if nodescription then
    begin        //blatantly stolen from DecColor   (why the fuck is there no incColor ?)
      RedGreenBlue(ColorToRGB(sender.Canvas.Font.Color), R, G, B);
      R := Max(0, Integer(R) + 75);
      G := Max(0, Integer(G) + 75);
      B := Max(0, Integer(B) + 75);
      sender.Canvas.Font.Color := RGBToColor(R, G, B);
    end;


    sender.Canvas.Refresh;
    sender.Canvas.TextRect(clip,textrect.Left,textrect.Top,description);

    //draw the columns
    for i:=0 to columnCount-1 do
    begin
      c:=columns[i];
      clip.left:=headercontrol1.left+headercontrol1.Sections[i+1].Left;
      clip.right:=headercontrol1.left+headercontrol1.Sections[i+1].Right;

      if selected then
        sender.canvas.font.Color:=InvertColor(c.currentNodeColor)
      else
        sender.canvas.font.Color:=c.currentNodeColor;

      sender.Canvas.Refresh;


      if miShowAddresses.checked then
        s:=c.currentNodeAddress
      else
        s:='';

      s:=s+c.currentNodeValue;
      sender.Canvas.TextRect(clip,clip.left,textrect.Top,s);
    end;
  end;
  DefaultDraw:=true;
end;

procedure TfrmStructures2.EditValueOfSelectedNodes(c:TStructColumn);
var a: PtrUInt;
  error: boolean;
  se: Tstructelement;
  node: TTreeNode;
  i: integer;
  s: string;
begin
  node:=tvStructureView.Selected;
  if node=nil then exit;

  se:=getStructElementFromNode(node);
  if se<>nil then
  begin
    a:=getAddressFromNode(node, c, error);

    if not error then
    begin
      //show the change value dialog
      s:=se.getValue(a);
      if InputQuery('Change Value','New value for this address:', s) then
      begin
        //try setting the value
        for i:=0 to tvStructureView.SelectionCount-1 do
        begin
          se:=getStructElementFromNode(tvStructureView.Selections[i]);
          a:=getAddressFromNode(tvStructureView.Selections[i], c, error);
          if not error then
            se.setvalue(a, s); //I knew there was a reason I implemented this
        end;
      end;
    end;
  end;
end;

procedure TfrmStructures2.tvStructureViewDblClick(Sender: TObject);
var
  m: TPoint;
  c: TStructColumn;
begin
  //if first column is doubleclicked edit the type, else edit the value
  m:=mouse.CursorPos;
  m:=tvStructureView.ScreenToClient(m);
  c:=getColumnAtXPos(m.x);

  if c=nil then
    miChangeElementClick(miChangeElement)
  else
    EditValueOfSelectedNodes(c);
end;

procedure TfrmStructures2.miFindRelationsClick(Sender: TObject);
var sl: TfrmStructureLinker;
begin
  //show the "find Relations" form where the user can fill in known addresses for structures
  //the structures will then check each pointer and fill them in

  sl:=TfrmStructureLinker.Create(self);
  sl.ShowModal;
  sl.free;

end;

initialization
  DissectedStructs:=TList.create;
  frmStructures2:=tlist.Create;

end.

