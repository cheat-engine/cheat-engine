unit StructuresFrm2;


{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, math,
  StdCtrls, ComCtrls, Menus, lmessages, scrolltreeview, byteinterpreter, symbolhandler, cefuncproc,
  newkernelhandler, frmSelectionlistunit;




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
    fdisplayMethod: TdisplayMethod;
    fchildstruct: TDissectedStruct;
  public
    constructor create(parent:TDissectedStruct);
    function getParent: TDissectedStruct;
    function getOffset: integer;
    procedure setOffset(newOffset: integer);
    function getName: string;
    procedure setName(newname: string);
    function getVartype: TVariableType;
    procedure setVartype(newVartype: TVariableType);
    function getDisplayMethod: TdisplayMethod;
    procedure setDisplayMethod(newDisplayMethod: TdisplayMethod);
    function getBytesize: integer;
    procedure setBytesize(newByteSize: integer);
    function getValue(address: ptruint): string;
    procedure setvalue(address: ptruint; value: string);
    function getValueFromBase(baseaddress: ptruint): string;
    procedure setValueFromBase(baseaddress: ptruint; value: string);
    function isPointer: boolean;
    function getChildStruct: TDissectedStruct;
    procedure setChildStruct(newChildStruct: TDissectedStruct);

    function getIndex: integer;

    procedure AutoCreateChildStruct(name: string; address: ptruint);

    property Name: string read getName write setName;
    property VarType: TVariableType read getVarType write setVarType;
    property Offset: integer read getOffset write setOffset;
    property DisplayMethod: TdisplayMethod read getDisplayMethod write setDisplayMethod;
    property Bytesize: integer read getByteSize write setByteSize;
    property ChildStruct: TDissectedStruct read getChildStruct write setChildStruct;
    property index: integer read getIndex;
    property parent: TDissectedStruct read getParent;
  end;



  TDissectedStruct=class
  private
    structname: string;
    structelementlist: tlist;

    fDoNotSaveLocal: boolean;
    fAutoCreateStructsize: integer;

    fUpdateCounter: integer;
    fullstructupdate: boolean;

    updatecalledSort: boolean;
    updateChangedInformation: boolean;
    updatedelements: Tlist;



    function isUpdating: boolean;
    function getStructureSize: integer;
    procedure DoFullStructChangeNotification;
    procedure DoDeleteStructNotification;
  public
    constructor create(name: string);
    destructor destroy; override;
    function getName: string;
    procedure setName(newname: string);
    function getElementCount: integer;
    function getElement(index: integer): TStructelement;
    procedure beginUpdate;
    procedure endUpdate;
    procedure DoElementChangeNotification(element: TStructelement);

    procedure OnDeleteStructNotification(structtodelete: TDissectedStruct);

    procedure sortElements;
    function addElement(name: string=''; offset: integer=0; vartype: TVariableType=vtByte; bytesize: integer=0; childstruct: TDissectedStruct=nil): TStructelement;
    procedure removeElement(element: TStructelement);
    procedure delete(index: integer);
    procedure autoGuessStruct(baseaddress: ptruint; offset: integer; bytesize: integer);
    procedure addToGlobalStructList;
    procedure removeFromGlobalStructList;
    function isInGlobalStructList: boolean;
    function getIndexOf(element: TStructElement): integer;
    property structuresize : integer read getStructureSize;
    property name: string read getName write setName;
    property doNotSaveLocal: boolean read fDoNotSaveLocal write fDoNotSaveLocal; //information for the saving/loading. Does not affect the sructure itself
    property autoCreateStructsize: integer read fAutoCreateStructsize write fAutoCreateStructsize;
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
    fNoMatchColor: TColor; //The color to use when not all elements have the same color
    fMatchColor: tcolor; //The color to use when all elements in the group match
    fAllMatchColorSame: TColor; //The color to use when all groups have matching elements AND the same value
    fAllMatchColorDiff: TColor; //The color to use when all groups have matching alements but different values between groups

    GroupBox: TGroupbox;
    refcount: integer;
    grouppopup: TPopupMenu;
    miRename: TMenuItem;

    function getColumnCount: integer;
    function getColumn(i: integer): TStructColumn;
    procedure RenameClick(sender: tobject);
    procedure setGroupName(newname: string);
  public
    procedure setPositions;
    constructor create(parent: TfrmStructures2; GroupName: string);
    {
    destructor destroy;
    procedure addString(s:string); //sets matches to false if it doesn't match the previous string (also sets currentStrign to '' on false)
    procedure clear; //sets matches to true
    procedure ResetColors; //sets the colors based on the settings and position in the grouplist (e.g group deleted)
    property Matches: boolean read fMatches;

    property currentString: string read fCurrentString;
    property NoMatchColor: Tcolor read fNoMatchcolor;
    property MatchColor: TColor read fMatchColor;
    property AllMatchColorSame: TColor read fAllMatchColorSame;
    property AllMatchColorDiff: TColor read fAllMatchColorDiff; }
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
    miChangeGroup: TMenuItem;

    focusedShape: TShape;


    fcompareValue: string;

    procedure ChangeGroupClick(sender: tobject);


    procedure edtAddressChange(sender: TObject);
    function getAddress: ptruint;
    procedure setAddress(address: ptruint);
    procedure setFocused(state: boolean);
    function getFocused: boolean;
    function getGlobalIndex: integer;
    procedure setNewParent(group: TStructGroup);
  public
    constructor create(parent: TStructGroup);
    destructor destroy; override;


    procedure focus;
    procedure edtAddressEnter(sender: tobject);

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
    pnlGroups: TPanel;
    pmStructureView: TPopupMenu;
    Recalculateaddress1: TMenuItem;
    Renamestructure1: TMenuItem;
    Save1: TMenuItem;
    Structures1: TMenuItem;
    updatetimer: TTimer;
    tvStructureView: TTreeView;
    procedure Addextraaddress1Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure miBrowseAddressClick(Sender: TObject);
    procedure miBrowsePointerClick(Sender: TObject);
    procedure miAddToAddresslistClick(Sender: TObject);
    procedure Deletecurrentstructure1Click(Sender: TObject);
    procedure miAutoGuessClick(Sender: TObject);
    procedure miAutostructsizeClick(Sender: TObject);
    procedure miDoNotSaveLocalClick(Sender: TObject);
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
    procedure pmStructureViewPopup(Sender: TObject);
    procedure miNewWindowClick(Sender: TObject);
    procedure miUpdateIntervalClick(Sender: TObject);
    procedure Renamestructure1Click(Sender: TObject);
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


    procedure ApplyStructureCompare;
    procedure miSelectStructureClick(Sender: tobject);
    procedure InitializeFirstNode;
    procedure RefreshStructureList;
    procedure TreeViewHScroll(sender: TObject; scrolledleft, maxscrolledleft: integer);
    procedure TreeViewVScroll(sender: TObject);
    procedure addColumn;
    procedure removeColumn(columnid: integer);
    procedure FillTreenodeWithStructData(currentnode: TTreenode);
    procedure setupNodeWithElement(node: TTreenode; element: TStructElement);
    procedure setNodeValues(node: TTreenode; element: TStructElement); //sets the values for the current nodes
    procedure RefreshVisibleNodes;
    procedure setMainStruct(struct: TDissectedStruct);
    function getColumn(i: integer): TStructColumn;
    function getColumnCount: integer;

    function getGroup(i: integer): TStructGroup;
    function getGroupCount: integer;
  public
    { public declarations }
    initialaddress: integer;
    function getFocusedColumn: TStructColumn;
    function getColumnAtXPos(x: integer): TStructColumn;
    procedure changeNode(n: ttreenode);
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
    procedure onElementChange(struct:TDissectedStruct; element: TStructelement); //called when an element of a structure is changed
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

uses MainUnit, frmStructures2ElementInfoUnit, MemoryBrowserFormUnit;

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
     +'you want to shift this and following offsets?';
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
      else
        result:=1; //in case I forgot something
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

function TStructelement.getValue(address: ptruint): string;
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

  result:=result+readAndParseAddress(address, vt,  nil, ashex, displayMethod=dtSignedInteger, bytesize);
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

  ParseStringAndWriteToAddress(value, address, vt, hex);
  parent.DoElementChangeNotification(self);
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

function TStructelement.getIndex: integer;
begin
  result:=parent.getIndexOf(self);
end;

procedure TStructelement.AutoCreateChildStruct(name: string; address: ptruint);
begin
  if isPointer and (ChildStruct=nil) then
  begin
    ChildStruct:=TDissectedStruct.create(name);
    ChildStruct.autoGuessStruct(address, 0, parent.autoCreateStructsize);
  end;
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
  result:=TStructelement(structelementlist.Items[index]);
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

function TDissectedStruct.addElement(name: string=''; offset: integer=0; vartype: TVariableType=vtByte; bytesize: integer=0; childstruct: TDissectedStruct=nil): TStructelement;
begin
  beginUpdate;
  result:=TStructelement.create(self);
  structelementlist.Add(result);

  result.name:=name;
  result.offset:=offset;
  result.vartype:=vartype;
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

procedure TDissectedStruct.autoGuessStruct(baseaddress: ptruint; offset: integer; bytesize: integer);
var
  buf: pbytearray;

  currentOffset: integer;
  x: dword;
  i,j: integer;
  bs: integer;
  vt: TVariableType;

  e: TStructelement;
begin
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
        vt:=FindTypeOfData(baseAddress+i,@buf[i],bytesize-i);
        e:=addElement();
        e.Offset:=currentOffset;
        e.vartype:=vt;
        e.name:=VariableTypeToString(vt);

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

procedure TDissectedStruct.OnDeleteStructNotification(structtodelete: TDissectedStruct);
var
  i: integer;
  s: TDissectedStruct;
begin
  //remove all mentioning of this struct
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
  //tell each structure that it should remove all the childstruct mentions of this structure
  for i:=0 to DissectedStructs.count-1 do
  begin
    if DissectedStructs[i]<>self then
      TDissectedStruct(DissectedStructs[i]).OnDeleteStructNotification(self);
  end;
end;

destructor TDissectedStruct.destroy;
var i: integer;
begin
  beginUpdate; //never endupdate

  DoDeleteStructNotification;


  for i:=0 to structelementlist.Count-1 do
    TStructelement(structelementlist.Items[i]).free;

  if structelementlist<>nil then
    freeandnil(structelementlist);


  removeFromGlobalStructList;


  inherited destroy;
end;

constructor TDissectedStruct.create(name: string);
begin
  self.name:=name;
  structelementlist:=tlist.Create;

  autoCreateStructsize:=4096; //default autocreate size
end;

{ TStructColumn }

procedure TStructColumn.setNewParent(group: TStructGroup);
begin

  parent.fcolumns.Remove(self);
  parent:=group;
  parent.fcolumns.Add(self);

  edtAddress.parent:=group.box;
  focusedShape.parent:=group.box;

  group.setPositions; //update the gui
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

end;

function TStructColumn.getAddress: ptruint;
begin
  result:=faddress;;
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

    if readprocessmemory(processhandle, pointer(faddress), buf, fsavedstatesize, x) then
      fsavedstate:=VirtualAllocEx(processhandle, nil, fsavedstatesize, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);

  end;

  result:=fsavedstate<>nil;
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
    edtLeft:=10;
  end
  else
  begin
    //get the editwidth of the previous item in the columns list
    edtWidth:=parent.columns[i-1].EditWidth;
    edtLeft:=parent.columns[i-1].EditLeft+edtWidth+10;
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


  miChangeGroup:=TMenuItem.Create(columneditpopupmenu);
  miChangeGroup.Caption:='Change Group';
  miChangeGroup.OnClick:=ChangeGroupClick;
  columneditpopupmenu.Items.Add(miChangeGroup);


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

  hsection:=parent.parent.headercontrol1.Sections.Add;
  hsection.Text:=rsAddressValue;
  hsection.Width:=200;
  hsection.MinWidth:=20;

  parent.setPositions;
end;

destructor TStructColumn.destroy;
var i: integer;
begin


  parent.fcolumns.remove(self);
  //parent.parent.headercontrol1.Sections.Delete(hsection.Index);

  clearSavedState;

  if edtAddress<>nil then
    freeandnil(edtAddress);

  if focusedShape<>nil then
    focusedShape.free;

  parent.setPositions;
end;

{ Tstructgroup }
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
      parent.group[i].box.Left:=10
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



  groupbox.popupmenu:=grouppopup;




  //create the groupbox
  GroupBox:=TGroupBox.Create(parent);
  GroupBox.Caption:=groupname;
  GroupBox.height:=parent.pnlGroups.ClientHeight;
  groupbox.parent:=parent.pnlGroups;

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

procedure TfrmStructures2.FormCreate(Sender: TObject);
begin
  tvStructureView.Top:=headercontrol1.top+headercontrol1.height;
  tvStructureView.left:=0;
  tvStructureView.width:=clientwidth;
  tvStructureView.height:=clientheight-tvStructureView.Top;

  tvStructureView.onHScroll:=TreeViewHScroll;
  tvStructureView.onVScroll:=TreeViewVScroll;

  frmStructures2.Add(self);

  RefreshStructureList; //get the current structure list

  fgroups:=tlist.create;
end;

procedure TfrmStructures2.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=cafree;
end;



procedure TfrmStructures2.FormDestroy(Sender: TObject);
begin
  frmStructures2.Remove(self);


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
      s:=mainStruct.structname+#13;

      while tvStructureView.Canvas.TextWidth(s)<x do
        s:=s+'PADDING';

      tvStructureView.items[0].Text:=s;
    end;

  end;

end;

procedure TfrmStructures2.RefreshVisibleNodes;
var
  i: integer;
  start: integer;
  stop: integer;

  top: TTreenode;
  Bottom: TTreenode;
  e: Tstructelement;
begin
  top:=tvStructureView.TopItem;
  bottom:=tvStructureView.BottomItem;

  if (top=nil) or (bottom=nil) then exit; //empty list or bug

  start:=top.AbsoluteIndex;
  stop:=bottom.AbsoluteIndex;

  tvStructureView.BeginUpdate;
  for i:=start to stop do
  begin
    e:=getStructElementFromNode(tvStructureView.items[i]);
    if e<>nil then
      setNodeValues(tvStructureView.items[i],e);
  end;
  tvStructureView.EndUpdate;
end;

procedure TfrmStructures2.getPointerFromNode(node: TTreenode; column:TStructcolumn; var baseaddress: ptruint; var offsetlist: ToffsetList);
var
  i: integer;
  lastoffsetentry: integer;
  offset0: integer; //the offset at the base of the structure
begin
  baseaddress:=column.Address;
  setlength(offsetlist,0);

  if (node=nil) or (node.level=0) then exit; //first entry in the mainstruct

  setlength(offsetlist, node.Level-1);
  lastoffsetentry:=node.level-2;

  while node.level>1 do
  begin
    offsetlist[lastoffsetentry]:=getStructElementFromNode(node).Offset;
    dec(lastoffsetentry);

    node:=node.parent;
  end;

  //now at node.level=1
  //add the starting offset
  inc(baseaddress, getStructElementFromNode(node).Offset);
end;

function TfrmStructures2.getAddressFromNode(node: TTreenode; column: TStructColumn; var hasError: boolean): ptruint;
//Find out the address of this node
var
  baseaddress: ptruint;
  offsets: array of dword;
begin
  getPointerFromNode(node,column, baseaddress, offsets);
  result:=getPointerAddress(baseaddress,  offsets, hasError);
end;

procedure TfrmStructures2.ApplyStructureCompare;
{
will compare the groups to eachother and set the proper colorcodes
}
var
  groups: array of integer;
  i: integer;
begin
   {
  if columnCount>0 then
  begin
    for i:=0 to columncount-1 do
      columns[i].GroupID:=;

  end;}
end;

procedure TfrmStructures2.setNodeValues(node: TTreenode; element: TStructElement);
var
    addresslist: array of string;

    values: string;
    i: integer;
    error: boolean;
    address: ptruint;
    addressstring: string;
begin
  values:=inttohex(element.Offset,4)+' - '+element.Name;

  setlength(addresslist, columncount);
  for i:=0 to columnCount-1 do
  begin
    address:=getAddressFromNode(node, TStructColumn(columns[i]), error);

    if miShowAddresses.checked then
    begin
      if not error then
        TStructColumn(columns[i]).compareValue:=inttohex(address,1)+' : '
      else
        TStructColumn(columns[i]).compareValue:='??? :'
    end
    else
      TStructColumn(columns[i]).compareValue:='';


    if not error then
      TStructColumn(columns[i]).compareValue:=element.getValue(address)
    else
      TStructColumn(columns[i]).compareValue:='???';
  end;

  ApplyStructureCompare; //sets the proper color in front of the columnvalue

  for i:=0 to columncount-1 do
    values:=#13+TStructColumn(columns[i]).compareValue;

  node.Text:=values;
end;

procedure TfrmStructures2.setupNodeWithElement(node: TTreenode; element: TStructElement);
begin
  setNodeValues(node, element);
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
  newnode: TTreenode;
  i: integer;
begin
  tvStructureView.OnExpanded:=nil;

  tvStructureView.BeginUpdate;
  struct:=TDissectedStruct(currentnode.data);
  currentnode.DeleteChildren;

  for i:=0 to struct.count-1 do
  begin
    newnode:=tvStructureView.Items.AddChild(currentnode,'');
    setupNodeWithElement(newnode, struct[i]);
  end;

  currentnode.Expand(false);

  tvStructureView.EndUpdate;

  tvStructureView.OnExpanded:=tvStructureViewExpanded;
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
begin
  AllowExpansion:=true;
  n:=getStructElementFromNode(node);
  if (n<>nil) and (n.isPointer) and (n.ChildStruct=nil) then
  begin
    if miAutoCreate.Checked then
    begin
      //create a structure
      address:=getAddressFromNode(node, TStructColumn(columns[0]), error);
      if not error then
        n.AutoCreateChildStruct('Autocreated from '+inttohex(address,8), address);

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
    tn:=tvStructureView.Items.Add(nil, mainstruct.name);
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

    if currentnode<>nil then
    begin
      if currentnode.data=sender then
        currentnode.text:=sender.name;
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

  mainStruct:=TDissectedStruct.create(structname);

  if autofillin=mryes then
  begin
    sstructsize:='4096';
    if not inputquery(rsStructureDefine, rsPleaseGiveAStartingSizeOfTheStructYouCanChangeThis, Sstructsize) then exit;
    structsize:=strtoint(sstructsize);

    mainStruct.autoGuessStruct(TStructColumn(columns[0]).getAddress, 0, structsize );
  end;

  mainStruct.addToGlobalStructList;
end;


function TfrmStructures2.getStructElementFromNode(node: TTreenode): TStructelement;
var i: integer;
begin
  result:=nil;
  if (node<>nil) and (node.level>0) then
    result:=getStructFromNode(node)[node.index];
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

procedure TfrmStructures2.changeNode(n: TTreenode);
var
  structelement: TStructElement;

begin
  structElement:=getStructElementFromNode(n);

  if structElement<>nil then
  begin
    with tfrmstructures2ElementInfo.create(self) do
    begin
      //fill in basic info
      description:=structelement.name;
      offset:=structelement.offset;
      vartype:=structelement.vartype;
      bytesize:=structelement.bytesize;
      childstruct:=structelement.childstruct;
      hexadecimal:=structelement.displayMethod=dtHexadecimal;
      signed:=structelement.displaymethod=dtSignedInteger;


      if showmodal=mrok then
      begin
        structElement.parent.beginUpdate;
        try
          structElement.name:=description;
          structElement.offset:=offset;
          structElement.vartype:=vartype;
          structElement.bytesize:=bytesize;
          structElement.childstruct:=childstruct;
          if hexadecimal then
            structelement.displayMethod:=dtHexadecimal
          else
          if signed then
            structelement.displayMethod:=dtSignedInteger
          else
            structelement.displayMethod:=dtUnsignedInteger;

        finally
          structElement.parent.endupdate;
        end;
      end;

      free;
    end;
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
        structElement:=struct.addElement(description, offset, vartype, bytesize, childstruct);
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
  ChangeNode(tvStructureView.selected);
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

procedure TfrmStructures2.pmStructureViewPopup(Sender: TObject);
var childstruct: TDissectedStruct;
  ownerstruct: TDissectedStruct;
  structelement: TStructelement;
begin
  ownerstruct:=getStructFromNode(tvStructureView.selected);
  childstruct:=getChildStructFromNode(tvStructureView.selected);
  structelement:=getStructElementFromNode(tvStructureView.Selected);

  miFullUpgrade.visible:=(childstruct<>nil) and (not childstruct.isInGlobalStructList);
  miAddElement.visible:=(childstruct<>nil);
  miAddChildElement.visible:=(childstruct<>nil);
  miDeleteElement.visible:=tvStructureView.Selected<>nil;
  miChangeElement.visible:=structElement<>nil;


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
begin
  c:=getColumnAtXPos(x);
  if c<>nil then
    c.focus;
end;

function TfrmStructures2.getFocusedColumn: TStructColumn;
var i: integer;
begin
  result:=nil;
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
    for i:=0 to tvStructureView.SelectionCount do
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
  offsetlist: array of dword;
  element, element2: TStructelement;

  sname: string;
  n: ttreenode;
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


        mainform.addresslist.addaddress(element.Name, inttohex(baseaddress,1), offsetlist, length(offsetlist), element.VarType,'',element.Bytesize);
      end;


    end;


    //mainform.a
  end;
end;

procedure TfrmStructures2.Deletecurrentstructure1Click(Sender: TObject);
var s: TDissectedStruct;
begin
  s:=mainStruct;
  mainstruct:=nil;

  s.free;
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
    {
    base:=definedstructures[currentstructure.basestructure];
    if length(base.structelement)>0 then
      sStartOffset:=inttohex(base.structelement[length(base.structelement)-1].offset+base.structelement[length(base.structelement)-1].bytesize,1)
    else
      sStartOffset:='0';

    if not inputquery(rsStructureDefine, rsPleaseGiveAStartingOffsetToEvaluate, sStartOffset) then exit;
    startOffset:=StrToInt('$'+sStartOffset);

    sStructSize:='4096';
    if not inputquery(rsStructureDefine, rsPleaseGiveTheSizeOfTheBlockToEvaluate, sStructSize) then exit;
    structSize:=StrToInt(sStructSize);

    automaticallyGuessOffsets(addresses[0], startOffset, structsize, currentstructure.basestructure); }
  end;
end;

procedure TfrmStructures2.miAutostructsizeClick(Sender: TObject);
var newsize: string;
begin
  if mainstruct<>nil then
  begin
    newsize:=inttostr(mainstruct.autoCreateStructsize);
    if InputQuery('Autocreate structure', 'Default size:', newsize) then
    begin
      mainstruct.autoCreateStructsize:=strtoint(newsize);
      miAutostructsize.caption:='Autocreate structure size: '+inttostr(mainstruct.autoCreateStructsize);
    end;
  end;
end;



procedure TfrmStructures2.miDoNotSaveLocalClick(Sender: TObject);
begin
  if mainstruct<>nil then
    mainstruct.DoNotSaveLocal:=miDoNotSaveLocal.checked;
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

initialization
  DissectedStructs:=TList.create;
  frmStructures2:=tlist.Create;

end.

