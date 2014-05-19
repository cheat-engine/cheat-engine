unit Structuresfrm;


{      O
      OB
     OBS
    OBSO
   OBSOL
  OBSOLE
 OBSOLET
OBSOLETE
BSOLETE
SOLETE
OLETE
LETE
ETE
TE
E

}

{$MODE Delphi}

interface

uses
  windows, LCLIntf, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, Menus, StdCtrls, ExtCtrls, ComCtrls,CEFuncProc,NewKernelHandler,
  symbolhandler, {XMLDoc, XMLIntf,} byteinterpreter, dom, xmlread, xmlwrite,
  LResources, registry, scrollTreeView;

const structureversion=2;



type TAddressData=record
 { address: ptrUint;
  lockedMemory: pbytearray;
  lockedsize: integer; }
end;

type TbaseStructure=record
 { name: string;
  donotsave: boolean;
  structelement: array of TStructElement;  }
  end;

  Tstructure=class; //fpc 2.5.1: Without this the child: TStructure would not compile

  //TfrmStructures = class;



  Tstructure=class
  private
    //frmStructures: TfrmStructures; //obsolete
    treeviewused: ttreeview;
    addresses: array of Taddressdata;
    basestructure: integer;
    parentnode: ttreenode; //owner of this object
    objects: array of record //same size as the structelement of the base object
                        nodetoupdate: ttreenode; //same size as the structelement of the base object
                        child: Tstructure; //if it is a pointer then this points to the structure that defines it
                        //currentvalue: string; //obsolete, just get it on request
                      end;

     //  lockedmemory: array of pbyte;

     overrideReadWith: pByteArray;
     overrideReadSize: integer;
     overrideReadBase: ptruint;
  public
    function readProcessMemoryS(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL;

    procedure refresh;
    procedure removeAddress(i: integer);
    procedure setaddress(i: integer; x:ptrUint);
    function  lockaddressmemory(i: integer): boolean;
    procedure unlockaddressmemory(i: integer);
    constructor create(treeviewused: ttreeview;parentnode: ttreenode; addresses: array of ptrUint; basestructure: integer);
    destructor destroy; override;
  end;

  TfrmStructures = class(TForm)
    HeaderControl1: THeaderControl;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miSaveToCT: TMenuItem;
    miAutoCreate: TMenuItem;
    miAutostructsize: TMenuItem;
    miLockMem: TMenuItem;
    miUpdateInterval: TMenuItem;
    miUpdateOffsets: TMenuItem;
    miChangeColors: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    saveValues: TSaveDialog;
    Structures1: TMenuItem;
    Definenewstructure1: TMenuItem;
    N1: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    Addelement1: TMenuItem;
    tvStructureView: TTreeView;
    updatetimer: TTimer;
    Deleteelement1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ChangeElement1: TMenuItem;
    N2: TMenuItem;
    Addtoaddresslist1: TMenuItem;
    Recalculateaddress1: TMenuItem;
    N3: TMenuItem;
    Addextraaddress1: TMenuItem;
    edtAddress: TEdit;
    PopupMenu2: TPopupMenu;
    Paste1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    N4: TMenuItem;
    Remove1: TMenuItem;
    N5: TMenuItem;
    SelectAll1: TMenuItem;
    N6: TMenuItem;
    Undo1: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Newwindow1: TMenuItem;
    Commands1: TMenuItem;
    Deletecurrentstructure1: TMenuItem;
    Renamestructure1: TMenuItem;
    Memorybrowsepointer1: TMenuItem;
    Memorybrowsethisaddress1: TMenuItem;
    Autoguessoffsets1: TMenuItem;
    Setgroup1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure Commands1Click(Sender: TObject);
    procedure Definenewstructure1Click(Sender: TObject);
    procedure Addelement1Click(Sender: TObject);
    procedure HeaderControl1Resize(Sender: TObject);
    procedure HeaderControl1SectionTrack(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection; Width: Integer; State: TSectionTrackState);

    procedure MenuItem3Click(Sender: TObject);
    procedure miAutostructsizeClick(Sender: TObject);
    procedure miLockMemClick(Sender: TObject);
    procedure miChangeColorsClick(Sender: TObject);
    procedure miSaveToCTClick(Sender: TObject);
    procedure miUpdateIntervalClick(Sender: TObject);
    procedure miUpdateOffsetsClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure updatetimerTimer(Sender: TObject);
    procedure tvStructureViewCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure edtAddressChange(Sender: TObject);
    procedure tvStructureViewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure tvStructureViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Deleteelement1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure ChangeElement1Click(Sender: TObject);
    procedure tvStructureViewDblClick(Sender: TObject);
    procedure Addtoaddresslist1Click(Sender: TObject);
    procedure Recalculateaddress1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Addextraaddress1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure tvStructureViewAdvancedCustomDrawItem(
      Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure Renamestructure1Click(Sender: TObject);
    procedure Deletecurrentstructure1Click(Sender: TObject);
    procedure Newwindow1Click(Sender: TObject);
    procedure Memorybrowsepointer1Click(Sender: TObject);
    procedure Memorybrowsethisaddress1Click(Sender: TObject);
    procedure Autoguessoffsets1Click(Sender: TObject);
    procedure Setgroup1Click(Sender: TObject);
  private
    { Private declarations }
    backgroundcolor: TColor;
    defaultText: TColor;
    equaltext: TColor;
    differentText: TColor;
    groupequalText: TColor;

    selectedbackgroundcolor: TColor;
    selectedDefaultText: TColor;
    selectedEqualText: TColor;
    selectedDifferentText: TColor;
    selectedGroupEqualText: TColor;

    currentstructure: tstructure;


    addresses: array of ptrUint;  //first address (old compat)

    groupindex: array of integer; //e.g 1,3,9,10000000
    groups: array of dword;               //the grouplist holds the groupnumbers provided by the user
    internalgrouplist: array of dword;    //where as the internelgrouplist holds the entrypoints into the groupindex for each specific address 
    edits: array of tedit;
    lastnewedit: TEdit;



    procedure refreshmenuitems;
    procedure definedstructureselect(sender:tobject);
    function convertVariableTypeTostructnr(d: TVariableType): integer;
//    function RawToType(address: dword; const buf: array of byte; size: integer):integer;
    procedure ExtraEnter(Sender: TObject);
    function guessTypeOfAddress(address: ptruint): integer;
    procedure automaticallyGuessOffsets(baseaddress, baseOffset: ptrUint; structsize: integer; structureid: integer);
    procedure autoCreateStructure(address: ptruint; var newstructureid: integer);

    procedure UpdateGroupIndex;

    procedure SaveColors;
    procedure LoadColors;
    procedure TreeViewHScroll(sender: TObject; scrolledleft, maxscrolledleft: integer);
  public
    { Public declarations }
    procedure setaddress(i: integer; x:ptrUint);
    function getaddress(i: integer): ptruint;
    procedure applyChanges(doOthers: boolean);
  end;


procedure sortStructure(struct: TbaseStructure);

resourcestring

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
  rsWrongVersion = 'This structure file was generated with a newer version of '
    +'Cheat Engine. (That means there''s more than likely a new version so '
    +'please update....)';
  rsUnkownFileExtension = 'Unknown file extension';
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

implementation


uses StructuresAddElementfrm,Valuechange,MainUnit, MemoryBrowserFormUnit, OpenSave,
  frmStructuresConfigUnit, MemoryRecordUnit;




destructor TStructure.destroy;
var i: integer;
begin
 { for i:=0 to length(objects)-1 do
    if objects[i].child<>nil then objects[i].child.Free;

  inherited destroy;  }
end;

constructor TStructure.create(treeviewused: ttreeview;parentnode: ttreenode; addresses: array of ptrUint; basestructure: integer);
var i: integer;
begin
 {
  setlength(self.addresses,length(addresses));
  for i:=0 to length(addresses)-1 do
  begin
    self.addresses[i].address:=addresses[i];
    self.addresses[i].lockedMemory:=nil;
    self.addresses[i].lockedsize:=0;
  end;

  self.basestructure:=basestructure;
  self.treeviewused:=treeviewused;
  self.parentnode:=parentnode;
  inherited create;    }
end;

procedure TStructure.removeAddress(i: integer);
var j: integer;
begin
{  if addresses[i].lockedMemory<>nil then
  begin
    freemem(addresses[i].lockedMemory);
    addresses[i].lockedMemory:=nil;
  end;

  for j:=i to length(addresses)-2 do
    addresses[j]:=addresses[j+1];

  if i<length(addresses) then //just in case it didn't get the previous add...
    setlength(addresses,length(addresses)-1);

  for j:=0 to length(objects)-1 do
    if objects[j].child<>nil then
      objects[j].child.removeAddress(i);

  refresh;      }
end;


procedure TStructure.setaddress(i: integer; x:ptrUint);
var j,k: integer;
begin
{  if i>=length(addresses) then
  begin
    j:=length(addresses);
    setlength(addresses,i+1);

    //init these new addresses
    for k:=length(addresses)-1 downto j do
    begin
      addresses[k].address:=x;
      addresses[k].lockedMemory:=nil;
      addresses[k].lockedsize:=0;
    end;
  end;

  //update children
  for j:=0 to length(objects)-1 do
  begin
    if objects[j].child<>nil then
      objects[j].child.setaddress(i,x);
  end;


  if addresses[i].lockedmemory=nil then
    addresses[i].address:=x; //else leave it alone

  refresh;      }
end;

procedure TStructure.unlockaddressmemory(i: integer);
begin
  //unlock
 { if addresses[i].lockedMemory<>nil then
  begin
    freemem(addresses[i].lockedMemory);
    addresses[i].lockedMemory:=nil;
  end;      }

end;

function TStructure.lockaddressmemory(i: integer): boolean;
var
  lastentry: integer;
  offsetsize: integer;
  soffsetsize: string;
  x: dword;
begin
 { if self.parentnode.Level>1 then
    raise exception.create('lock called for a child');

  result:=false;
  lastentry:=length(definedstructures[basestructure].structelement)-1;


  offsetsize:=definedstructures[basestructure].structelement[lastentry].offset+definedstructures[basestructure].structelement[lastentry].bytesize;

  if offsetsize>65536 then
  begin
    soffsetsize:=inttostr(offsetsize);
    if InputQuery(rsThisIsQuiteABigStructureHowManyBytesDoYouWantToSav,
      rsStructureViewLock, soffsetsize)=false then exit;
    offsetsize:=strtoint(soffsetsize);
  end;



  //allocate the locked buffer
  addresses[i].lockedsize:=offsetsize;
  getmem(addresses[i].lockedMemory, offsetsize);
  if addresses[i].lockedMemory<>nil then
  begin
    //fill it
    result:=ReadProcessMemory(processhandle, pointer(addresses[i].address), addresses[i].lockedmemory, addresses[i].lockedsize, x);
    if result then
      addresses[i].lockedsize:=x
    else
    begin
      //on fail, free
      freemem(addresses[i].lockedMemory);
      addresses[i].lockedMemory:=nil;
    end;
  end;    }
end;

function TStructure.readProcessMemoryS(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL;
begin
 { if (overrideReadWith=nil) or (ptruint(lpBaseAddress)<overrideReadBase) or ((ptrUint(lpBaseAddress)-overrideReadBase)+nSize > overrideReadSize) then
    result:=ReadProcessMemory(hProcess, lpBaseAddress, lpBuffer, nSize, lpNumberOfBytesRead)
  else
  begin
    //read from the override
    CopyMemory(lpBuffer, pointer(ptruint(overrideReadWith)+(ptrUint(lpBaseAddress)-overrideReadBase)), nsize);
    lpNumberOfBytesRead:=nSize;
    result:=true;
  end;
              }
end;

procedure TStructure.refresh;
var c,i,j,k: integer;
    newtext,typename: string;
    snr: integer;
    elementoffset: dword;
    buf: array of byte;
    x: dword;

    ws: widestring;
    pc: pchar;
    pwc: pwidechar;
    newaddress: dword;


    s: tstructure;
    st: string;

    elementnr: integer;
    currentvalues: array of string;

begin

end;

procedure sortStructure(struct: TbaseStructure);
begin


end;

procedure TfrmStructures.applyChanges(doOthers: boolean);
{Called twice, first time true, 2nd time false. Only on false actually update}
var i: integer;
begin

end;

function TfrmStructures.getAddress(i: integer): ptruint;
begin

end;

procedure TfrmStructures.setaddress(i: integer; x: ptrUint);
begin

end;


function TfrmStructures.convertVariableTypeTostructnr(d: TVariableType): integer;
begin

end;

procedure TfrmStructures.Definenewstructure1Click(Sender: TObject);
var sstructsize:string;
    autofillin,structsize: integer;
    structname: string;

begin

end;

procedure TfrmStructures.Button1Click(Sender: TObject);
begin

end;

procedure TfrmStructures.miSaveToCTClick(Sender: TObject);
begin

end;

procedure TfrmStructures.Commands1Click(Sender: TObject);
begin

end;

procedure TfrmStructures.definedstructureselect(sender:tobject);
{
When the user selects a structure from the list
}
begin

end;

procedure TfrmStructures.refreshmenuitems;
var i: integer;
    mi: tmenuitem;
begin

end;


procedure TfrmStructures.Addelement1Click(Sender: TObject);
var d,i,j,k:integer;
    size: dword;
    selectedstructure: tstructure;
    selectedelement: integer;
    selectednode: ttreenode;
    base: dword;
begin

end;

procedure TfrmStructures.HeaderControl1Resize(Sender: TObject);
begin

end;



procedure TfrmStructures.HeaderControl1SectionTrack(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection; Width: Integer;
  State: TSectionTrackState);
var x: integer;
    s: string;
begin


end;


procedure TfrmStructures.MenuItem3Click(Sender: TObject);
var
  i,j: integer;
  s: string;
  laststart: integer;
  sections,sections2: array of string;
  currentsection: integer;

  f: tstringlist;
begin

end;

procedure TfrmStructures.miAutostructsizeClick(Sender: TObject);
var newsize: string;
begin

end;


procedure TfrmStructures.miChangeColorsClick(Sender: TObject);
var c: TfrmStructuresConfig;
begin

end;


procedure TfrmStructures.miUpdateIntervalClick(Sender: TObject);
var interval: string;
begin

end;

procedure TfrmStructures.miUpdateOffsetsClick(Sender: TObject);
var
  offsetstring: string;
  offset: integer;
  s: tstructure;
  elementnr: integer;
  i: integer;
begin

end;

procedure TfrmStructures.Panel1Click(Sender: TObject);
begin

end;



procedure TfrmStructures.SaveColors;
var reg: TRegistry;
begin

end;

procedure TfrmStructures.LoadColors;
var reg: TRegistry;
begin

end;




procedure TfrmStructures.updatetimerTimer(Sender: TObject);
begin
  if currentstructure<>nil then currentstructure.refresh;
end;

procedure TfrmStructures.tvStructureViewCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin

end;

procedure TfrmStructures.edtAddressChange(Sender: TObject);
begin

end;

procedure TfrmStructures.autoCreateStructure(address: ptruint; var newstructureid: integer);
var
  i: integer;
  a: ptruint;
  x: dword;
begin

end;


procedure TfrmStructures.tvStructureViewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var s: tstructure;
    elementnr: integer;
    basestruct: integer;
    elementaddress: array of ptrUint;
    i,j: integer;

    offset: dword;
begin

end;

procedure TfrmStructures.tvStructureViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tn: ttreenode;
begin

end;

procedure TfrmStructures.PopupMenu1Popup(Sender: TObject);
var
  i: integer;
  s: Tstructure;
  elementnr: integer;
begin

end;

procedure TfrmStructures.Deleteelement1Click(Sender: TObject);

begin

end;

procedure TfrmStructures.Save1Click(Sender: TObject);
var f: tfilestream;
    i,j: integer;
    x: dword;
    cemarker: string;

    doc: TXMLDocument;
    CheatTable: TDOMNode;
    Structures: TDOMNode;
begin

end;

procedure TfrmStructures.Open1Click(Sender: TObject);
var f: tfilestream;
    i,j: integer;
    startindex: integer;
    x: dword;
    cemarker: string;
    c: pchar;
    s: string;
    oldsize: integer;
    structures, structure: TDOMNode;
    CheatTable: TDOMNode;
    doc: TXMLDocument;
begin

end;

procedure TfrmStructures.New1Click(Sender: TObject);
begin

end;

procedure TfrmStructures.ChangeElement1Click(Sender: TObject);
var i,j: integer;
    size: dword;
    selectedstructure: tstructure;
    selectedelement: integer;
    selectednode: ttreenode;
begin


end;

procedure TfrmStructures.tvStructureViewDblClick(Sender: TObject);
var
  selectedstructure: tstructure;
  selectednode: ttreenode;
  selectedelement: integer;
  i: integer;
  a: dword;
  cursorpos: tpoint;
  tvrect: trect;
  selectedsection: integer;
begin

end;

procedure TfrmStructures.Addtoaddresslist1Click(Sender: TObject);

begin

end;


procedure TfrmStructures.Recalculateaddress1Click(Sender: TObject);

begin

end;

procedure TfrmStructures.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin

end;



procedure TfrmStructures.TreeViewHScroll(sender: TObject; scrolledleft, maxscrolledleft: integer);
begin

end;

procedure TfrmStructures.FormCreate(Sender: TObject);
begin


end;

procedure TfrmStructures.ExtraEnter(Sender: TObject);
begin
  popupmenu2.PopupComponent:=TComponent(sender);
end;

procedure TfrmStructures.Addextraaddress1Click(Sender: TObject);
var x: tedit;
    newsection: THeadersection;
begin

end;

procedure TfrmStructures.Remove1Click(Sender: TObject);
var x: tedit;
    i: integer;
begin

end;

procedure TfrmStructures.Undo1Click(Sender: TObject);
begin

end;

procedure TfrmStructures.Cut1Click(Sender: TObject);
begin

end;



procedure TfrmStructures.Copy1Click(Sender: TObject);
begin

end;

procedure TfrmStructures.Paste1Click(Sender: TObject);
begin

end;

procedure TfrmStructures.SelectAll1Click(Sender: TObject);
begin

end;




procedure TfrmStructures.tvStructureViewAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
{
multigroup usage:
If all entries of the same group are the same, mark them green, otherwise red
If the value of another group does not match the value of the first group, mark it red
}



begin

end;

procedure TfrmStructures.FormDestroy(Sender: TObject);
var i,j: integer;
begin

end;

procedure TfrmStructures.PopupMenu2Popup(Sender: TObject);
var x: tedit;
begin

end;

procedure TfrmStructures.Renamestructure1Click(Sender: TObject);
begin

end;

procedure TfrmStructures.Deletecurrentstructure1Click(Sender: TObject);
var i,j: integer;
begin

end;

procedure TfrmStructures.Newwindow1Click(Sender: TObject);
begin

end;

procedure TfrmStructures.Memorybrowsepointer1Click(Sender: TObject);

begin

end;

procedure TfrmStructures.Memorybrowsethisaddress1Click(Sender: TObject);
var
  i: integer;
  s: Tstructure;
  elementnr: integer;
  tvrect: trect;
  clickpos: tpoint;
  section: integer;
begin

end;

function TfrmStructures.guessTypeOfAddress(address: ptruint): integer;
var buf: array of byte;
  x: dword;
  t: TVariableType;
begin

end;

procedure TfrmStructures.automaticallyGuessOffsets(baseaddress, baseOffset: ptrUint; structsize: integer; structureid: integer);
var
  buf: array of byte;
  buf2: array of byte;
  i,t: integer;
  t2: TVariableType;
  x,y: dword;
  a: qword;
begin

end;

procedure TfrmStructures.Autoguessoffsets1Click(Sender: TObject);
var
  sStartOffset: string;
  sStructSize: string;
  base: TbaseStructure;
  startOffset: integer;
  structSize: integer;
begin

end;

procedure  TfrmStructures.UpdateGroupIndex;
var i,j: integer;
    alreadyIndexed: boolean;
begin

end;


procedure TfrmStructures.Setgroup1Click(Sender: TObject);
var
  x: tedit;
  sgroup: string;
begin

end;


procedure TfrmStructures.miLockMemClick(Sender: TObject);
var
  x: tedit;
  sid: integer;

begin


end;



initialization
  {$i Structuresfrm.lrs}

end.



