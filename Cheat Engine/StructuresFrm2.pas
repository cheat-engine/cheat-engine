unit StructuresFrm2;


{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, math,
  StdCtrls, ComCtrls, Menus, lmessages, scrolltreeview, byteinterpreter, symbolhandler, cefuncproc, newkernelhandler;




  { TfrmStructures2 }
type
  TdisplayMethod=(dtUnsignedInteger, dtSignedInteger, dtHexadecimal );
  TStructOperation=(soAdd, soDelete, soSort);

  TDissectedStruct=class;
  TStructelement=class
  private
    parent: TDissectedStruct;
    foffset: integer;
    fbytesize: integer;
    fname: string;
    fvartype: TVariableType;
    fdisplayMethod: TdisplayMethod;
    fchildstruct: TDissectedStruct;
  public
    constructor create(parent:TDissectedStruct);
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

    property Name: string read getName write setName;
    property VarType: TVariableType read getVarType write setVarType;
    property Offset: integer read getOffset write setOffset;
    property DisplayMethod: TdisplayMethod read getDisplayMethod write setDisplayMethod;
    property Bytesize: integer read getByteSize write setByteSize;
    property ChildStruct: TDissectedStruct read getChildStruct write setChildStruct;
    property index: integer read getIndex;
  end;



  TDissectedStruct=class
  private
    structname: string;
    structelementlist: tlist;

    fUpdateCounter: integer;

    updatecalledSort: boolean;
    updateChangedInformation: boolean;
    updatedelements: Tlist;

    function isUpdating: boolean;
    function getStructureSize: integer;
    procedure DoFullStructChangeNotification;
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
    property count: integer read getElementCount;
    property element[Index: Integer]: TStructelement read getElement; default;
  end;

  TfrmStructures2=class;
  TStructColumn=class
  private
    parent: TfrmStructures2;
    faddress: ptruint;
    fgroupid: integer;
    fsavedstate: pointer; //points to a copy made in the target process
    fsavedstatesize: integer;
    edtAddress: TEdit;
    columneditpopupmenu: TPopupMenu;
    hsection: THeaderSection;
    procedure edtAddressChange(sender: TObject);
  public
    constructor create(parent: TfrmStructures2);
    destructor destroy; override;
    function getAddress: ptruint;
    procedure setAddress(address: ptruint);
    procedure clearSavedState;
    function saveState: boolean;
    function getSavedState: pointer;
    function getSavedStateSize: integer;
    function getEditWidth: integer;
    function getEditLeft: integer;
    procedure SetProperEditboxPosition;
    property EditWidth: integer read getEditwidth;
    property EditLeft: integer read getEditleft;
  end;

  TfrmStructures2 = class(TForm)
    miAddElement: TMenuItem;
    Addextraaddress1: TMenuItem;
    Addtoaddresslist1: TMenuItem;
    Autoguessoffsets1: TMenuItem;
    ChangeElement1: TMenuItem;
    Commands1: TMenuItem;
    Definenewstructure1: TMenuItem;
    Deletecurrentstructure1: TMenuItem;
    Deleteelement1: TMenuItem;
    File1: TMenuItem;
    HeaderControl1: THeaderControl;
    MainMenu1: TMainMenu;
    Memorybrowsepointer1: TMenuItem;
    Memorybrowsethisaddress1: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miAutoCreate: TMenuItem;
    miAutostructsize: TMenuItem;
    miChangeColors: TMenuItem;
    miSaveToCT: TMenuItem;
    miUpdateInterval: TMenuItem;
    miUpdateOffsets: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Newwindow1: TMenuItem;
    Open1: TMenuItem;
    pnlAddresses: TPanel;
    structviewMenu: TPopupMenu;
    Recalculateaddress1: TMenuItem;
    Renamestructure1: TMenuItem;
    Save1: TMenuItem;
    Structures1: TMenuItem;
    tvStructureView: TTreeView;
    procedure Addextraaddress1Click(Sender: TObject);
    procedure Definenewstructure1Click(Sender: TObject);
    procedure Deleteelement1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HeaderControl1SectionTrack(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection; Width: Integer; State: TSectionTrackState);
    procedure miAddElementClick(Sender: TObject);
    procedure tvStructureViewCollapsed(Sender: TObject; Node: TTreeNode);
    procedure tvStructureViewCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure tvStructureViewExpanded(Sender: TObject; Node: TTreeNode);
  private
    { private declarations }
    mainStruct: TDissectedStruct;
    columns: Tlist;



//    procedure createNewStructure(name: string; baseAddress: ptruint);

    {
    frmstructures
    =============
    -mainStruct: DissectedStruct
    -columns: list of Column
    -treeview.treenodes
    -------------
    -getStructElementFromTreenode(treenode): DissectedStruct
    -structchangedevent(DissectedStruct) :
      Called whenever anything changed in the struct
      Go through the list of treenodes and find nodes that has as treenode.data the struct.
      Go through all the children and make changes where necesary. If treenode.data is not correct. Delete all children
      Remember the current scroll position


    +getColumnCount()
    +getColumn(index)
    +addColumn()
    +removeColumn(columnid)
    +getMainStruct(): struct
    +createChildStruct(ownerstruct, index, address)
    +getStructAtIndex(index): DissectedStruct;
    +getName(index): string
    +setName(index, string)
    +getType(index)
    +setType(index, vartype, size OPT, pointerto OPT)
    +getValue(index, columnid): string
    +getStructElement(index)
    }
    procedure InitializeFirstNode;
    procedure RefreshStructureList;
    procedure TreeViewHScroll(sender: TObject; scrolledleft, maxscrolledleft: integer);
    procedure addColumn;
    procedure removeColumn(columnid: integer);
    procedure FillTreenodeWithStructData(currentnode: TTreenode);
  public
    { public declarations }
    initialaddress: integer;
    function getMainStruct: TDissectedStruct;
    procedure onFullStructChange(sender: TDissectedStruct);   //called when a structure is changed (sort/add/remove entry)
    procedure onElementChange(struct:TDissectedStruct; element: TStructelement); //called when an element of a structure is changed
  end; 

var
  frmStructures2: TList;
  DissectedStructs: Tlist;   //these get saved to the table and show up in the structure list


implementation

{$R *.lfm}

uses frmStructures2ElementInfoUnit, Structuresfrm;

resourcestring
  rsAddressValue = 'Address: Value';
  rsUndefined = 'undefined';

{Struct}

function TStructelement.getOffset: integer;
begin
  result:=fOffset;
end;

procedure TStructelement.setOffset(newOffset: integer);
begin
  fOffset:=newOffset;
  parent.sortElements;
end;

function TStructelement.getName: string;
begin
  result:=fname;
end;

procedure TStructelement.setName(newname: string);
begin
  fname:=newname;
  parent.DoElementChangeNotification(Self);
end;

function TStructelement.getVartype: TVariableType;
begin
  result:=fVartype;
end;

procedure TStructelement.setVartype(newVartype: TVariableType);
begin
  fVartype:=newVartype;
  parent.DoElementChangeNotification(self);
end;

function TStructelement.getDisplayMethod: TdisplayMethod;
begin
  result:=fDisplayMethod;
end;

procedure TStructelement.setDisplayMethod(newDisplayMethod: TdisplayMethod);
begin
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
  fbytesize:=max(1,newByteSize); //at least 1 byte
  parent.DoElementChangeNotification(self);
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

constructor TStructelement.create(parent:TDissectedStruct);
begin
  self.parent:=parent;
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
  end;
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
begin
  if not isInGlobalStructList then
  begin
    DissectedStructs.Add(self);
    DoFullStructChangeNotification; //perhaps a bit overkill
  end;
end;

procedure TDissectedStruct.removeFromGlobalStructList;
begin
  if isInGlobalStructList then
    DissectedStructs.Remove(self);
end;

function TDissectedStruct.isInGlobalStructList: boolean;
begin
  result:=DissectedStructs.IndexOf(self)<>-1;
end;

function TDissectedStruct.getIndexOf(element: TStructElement): integer;
begin
  result:=structelementlist.IndexOf(element);
end;

destructor TDissectedStruct.destroy;
var i: integer;
begin
  beginUpdate; //never endupdate
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
end;

{ TStructColumn }

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

  if parent.getMainStruct<>nil then
  begin
    fsavedstatesize:=parent.getMainStruct.getStructureSize;

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
  i:=parent.columns.IndexOf(self);

  if i=0 then
  begin
    edtWidth:=parent.pnlAddresses.Canvas.TextWidth('DDDDDDDDDDDDDDDD');
    edtLeft:=10;
  end
  else
  begin
    //get the editwidth of the previous item in the columns list
    edtWidth:=TStructColumn(parent.columns.items[i-1]).EditWidth;
    edtLeft:=TStructColumn(parent.columns.items[i-1]).EditLeft+edtWidth+10;
  end;

  edtAddress.left:=edtLeft;
  edtAddress.ClientWidth:=edtWidth;


  edtAddress.top:=(parent.pnlAddresses.clientHeight div 2)-(edtAddress.height div 2);

end;

constructor TStructColumn.create(parent: TfrmStructures2);
begin
  if parent=nil then raise exception.create('TStructColumn.create Error');
  self.parent:=parent;

  columneditpopupmenu:=TPopupMenu.Create(parent);

  parent.columns.Add(self);

  edtAddress:=TEdit.Create(parent);
  edtAddress.Tag:=ptruint(self);
  edtAddress.OnChange:=edtAddressChange;
  edtAddress.PopupMenu:=columneditpopupmenu;
  edtAddress.Parent:=parent.pnlAddresses;

  hsection:=parent.headercontrol1.Sections.Add;
  hsection.Text:=rsAddressValue;
  hsection.Width:=200;
  hsection.MinWidth:=20;

  SetProperEditboxPosition;
end;

destructor TStructColumn.destroy;
var i: integer;
begin
  if edtAddress<>nil then
    freeandnil(edtAddress);

  parent.columns.Remove(self);

  parent.headercontrol1.Sections.Delete(hsection.Index);


  for i:=0 to parent.columns.Count-1 do
    TStructColumn( parent.columns.items[i]).SetProperEditboxPosition;

  clearSavedState;
end;

{ TfrmStructures2 }
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

  frmStructures2.Add(self);

  columns:=tlist.create;
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
  if columns.Count=0 then //add the first column if necesary
  begin
    addColumn;
    TStructColumn(columns[0]).setAddress(initialaddress);
  end;


end;

procedure TfrmStructures2.HeaderControl1SectionTrack(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection; Width: Integer;
  State: TSectionTrackState);
var x: integer;
    s: string;
begin
  tvStructureView.refresh;

  if tvStructureView.Items.Count>0 then
  begin
    //make the first line (structname) as long as x
    if mainStruct<>nil then
    begin
      s:=mainStruct.structname+#13;
      x:=(HeaderControl1.Sections[HeaderControl1.Sections.Count-1].Left+HeaderControl1.Sections[HeaderControl1.Sections.Count-1].Width);

      while tvStructureView.Canvas.TextWidth(s)<x do
        s:=s+'PADDING';

      tvStructureView.items[0].Text:=s;

      // tvStructureView.Resize;
    end;

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
    newnode:=tvStructureView.Items.AddChild(currentnode,inttohex(struct[i].Offset,4)+' - '+struct[i].Name);

    if (struct[i].isPointer) then
    begin
      newnode.Data:=struct[i].ChildStruct;
      newnode.HasChildren:=true;
    end;
  end;

  currentnode.Expand(false);

  tvStructureView.EndUpdate;

  tvStructureView.OnExpanded:=tvStructureViewExpanded;
end;


procedure TfrmStructures2.tvStructureViewCollapsed(Sender: TObject; Node: TTreeNode);
var struct: TDissectedStruct;
begin
  tvStructureView.BeginUpdate;
  node.DeleteChildren; //delete the children when collapsed

  if node.parent<>nil then //almost always, and then it IS a child
  begin
    //get the structure this node belongs to
    struct:=TDissectedStruct(node.parent.data);

    //now get the element this node represents and check if it is a pointer
    node.HasChildren:=struct[node.Index].isPointer;
  end
  else
  if node.data<>nil then //root node (mainstruct)
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
  else
  begin
    //unknown pointer. Ask or autodefine a new one
    showmessage('not yet implemented');
  end;
end;

function TfrmStructures2.getMainStruct: TDissectedStruct;
begin
  result:=mainStruct;

end;

procedure TfrmStructures2.InitializeFirstNode;
var tn: TTreenode;
begin
  if (tvStructureView.Items.Count=0) and (mainStruct<>nil) then
  begin
    tn:=tvStructureView.Items.Add(nil, mainstruct.name);
    tn.Data:=mainStruct;
    tn.HasChildren:=true;
  end;
end;

procedure TfrmStructures2.onFullStructChange(sender: TDissectedStruct);
var currentNode: TTreenode;
    nextnode: TTreenode;
begin
  //update the childnode of the treenode with this struct to represent the new state
  if mainStruct<>nil then
  begin
    InitializeFirstNode;
    currentNode:=tvStructureView.Items.GetFirstNode;

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
begin
  //find the treenodes that belong to this specific element and change them accordingly

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


procedure TfrmStructures2.miAddElementClick(Sender: TObject);
var
  n: TTreenode;
  struct: TDissectedStruct;


begin
  n:=tvStructureView.Selected;
  if n=nil then    //if nothing is selected use the main structure
    n:=tvStructureView.Items.GetFirstNode;

  if n<>nil then
  begin
    with tfrmstructures2ElementInfo.create(self) do
    begin
      //fill in some basic info
      if n.parent=nil then
      begin
        //clicked on the main stuct element , set as initial offset the last offset
        struct:=mainStruct;
        offset:=struct.getStructureSize;
      end
      else
      begin
        //clicked on a node, find the offset and add it after that
        struct:=TDissectedStruct(n.parent.data);
        offset:=struct[n.Index].Offset+struct[n.Index].Bytesize;
      end;

      //show
      if showmodal=mrok then
        struct.addElement(description, offset, vartype, bytesize, childstruct);

      free;
    end;

  end;
end;

procedure TfrmStructures2.Deleteelement1Click(Sender: TObject);
var n: TTreenode;
  struct: TDissectedStruct;
begin
  n:=tvStructureView.Selected;
  if (n<>nil) and (n.level>0) then
  begin
    struct:=TDissectedStruct(n.parent.data);
    struct.delete(n.index);
  end;

end;

procedure TfrmStructures2.addColumn;
begin
  TStructColumn.create(self);
end;

procedure TfrmStructures2.removeColumn(columnid: integer);
begin
  columns.Delete(columnid);
end;

procedure TfrmStructures2.Addextraaddress1Click(Sender: TObject);
begin
  addColumn;
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
    Structures1.Add(mi);
  end;
end;

initialization
  DissectedStructs:=TList.create;
  frmStructures2:=tlist.Create;

end.

