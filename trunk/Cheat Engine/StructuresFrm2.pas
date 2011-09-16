unit StructuresFrm2;
{
9/15/2011:
Redesign as the original structure viewer has grown too much beyond it's original design specs
Perhaps makes integration with lua more managable as well

TdisplayMethod=enum (dtUnsignedInteger, dtSignedInteger, dtHexadecimal )

stuctelement
this class contains information about each element in a structure.
===========
-parent: DissectedStruct : If set, points to the owner struct
-offset: int
-bytesize: int
-name: string
-vartype: TVariableType
-displayMethod: TDisplayMethod
-childstruct: Class  : If type is vtPointer. Can be null to fill in later
---------------------
++create(parentstruct)
+getOffset(): int :
+setOffset(int) : Calls the parent's sort command when changed
+getName(): string
+setName(string)
+getType(): variabletype:  If it's a pointer, returns vtPointer
+setType(variabletype, struct optional)
+getDisplayMethod(): TDisplayMethod
+setDisplayMethod(TDisplayMethod)
+getByteSize(): int
+setByteSize(): int : Only valid for string and AOB
+getValue(address):string :Independant of offset, just uses the type for conversion to string
+setValue(address, string)
+getValueFromBase(structbaseaddress): string
+setValueFromBase(structbaseaddress, string)
+isPointer(): bool : Returns true if type is vtPointer
+getChildStruct(): struct
+setChildStruct(struct)



DissectedStruct
The DissectedStruct class is a global class that contains all the data about the structure.
======
-structname
-structelementlist : List
-isUpdating: boolean
-updateCalledSort: boolean
-updateChangedInformation: boolean;
-onChange: functionlist
---------------------
++Create(name: string)
+AddOnchangeNotification(function): Register a function to be called when anything has changed
+RemoveOnChangeNotification(function)
+getName(): string
+setName(structname)
+getElementCount()
+getElement(index): structelement

+beginUpdate()
+autoGuessStruct(baseaddress, offset, bytesize)
+addElement(): structelement :  Will create an empty struct element
+removeElement(structelement)
+sortElements(): Will sort the list based on offsets if beginUpdate isn't blocking it
+endUpdate() : If sortElements was called call it now and call DoChange if anything has changed

+DoChangeNotification(): Call onChange unless beginUpdate was called  (called by elements when changed)


Column
=======
-address: ptruint
-groupid: int
-savedstate: pointer
-savedstatesize: int
--------------------
+getAddress(): ptruint
+setAddress(ptruint)
+saveState(): bool
+clearSavedState(): void
+getSavedState(): savedstate
+getSavedStateSize(): int



(Visible stuff)
treenode.data contain the structure class their children belong to. The first node in the treeview is the main structure
Children that are no pointer to another struct have nil for treenode.data
A child that has a pointer but no struct assigned yet, will have an extract button but treenode.data will be null until extracted and the structure is created

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
+getStructAtIndex(index): DissectedStruct;
+getName(index): string
+setName(index, string)
+getType(index)
+setType(index, vartype, size OPT, pointerto OPT)
+getValue(index, columnid): string
+getStructElement(index)





}


{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, math,
  StdCtrls, ComCtrls, Menus, lmessages, scrolltreeview, byteinterpreter, cefuncproc, newkernelhandler;




  { TfrmStructures2 }
type
  TdisplayMethod=(dtUnsignedInteger, dtSignedInteger, dtHexadecimal );


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


    property Name: string read getName write setName;
    property VarType: TVariableType read getVarType write setVarType;
    property Offset: integer read getOffset write setOffset;
    property DisplayMethod: TdisplayMethod read getDisplayMethod write setDisplayMethod;
    property Bytesize: integer read getByteSize write setByteSize;
    property ChildStruct: TDissectedStruct read getChildStruct write setChildStruct;
  end;



  TDissectedStruct=class
  private
    structname: string;
    structelementlist: tlist;

    fUpdateCounter: integer;
    updateChangedInformation: boolean;
    updateCalledSort: boolean;
    onChange: array of TNotifyEvent;
    function isUpdating: boolean;
    procedure dosort;
  public
    constructor create(name: string);
    destructor destroy; override;
    procedure AddOnChangeNotification(event: TNotifyEvent);
    procedure RemoveOnChangeNotification(event: TNotifyEvent);
    function getName: string;
    procedure setName(newname: string);
    function getElementCount: integer;
    function getElement(index: integer): TStructelement;
    procedure beginUpdate;
    procedure endUpdate;
    procedure DoChangeNotification;
    procedure sortElements;
    function addElement(name: string=''; offset: integer=0; vartype: TVariableType=vtByte; bytesize: integer=0; childstruct: TDissectedStruct=nil): TStructelement;
    procedure removeElement(element: TStructelement);
    procedure autoGuessStruct(baseaddress: ptruint; offset: integer; bytesize: integer);

  end;

  {
  DissectedStruct
  The DissectedStruct class is a global class that contains all the data about the structure.
  ======
  -structname
  -structelementlist : List
  -isUpdating: boolean
  -updateCalledSort: boolean
  -updateChangedInformation: boolean;
  -onChange: functionlist
  ---------------------
  ++Create(name: string)
  +AddOnchangeNotification(function): Register a function to be called when anything has changed
  +RemoveOnChangeNotification(function)
  +getName(): string
  +setName(structname)
  +getElementCount()
  +getElement(index): structelement

  +beginUpdate()
  +autoGuessStruct(baseaddress, offset, bytesize)
  +addElement(): structelement :  Will create an empty struct element
  +removeElement(structelement)
  +sortElements(): Will sort the list based on offsets if beginUpdate isn't blocking it
  +endUpdate() : If sortElements was called call it now and call DoChange if anything has changed

  +DoChangeNotification(): Call onChange unless beginUpdate was called

  }


  TfrmStructures2 = class(TForm)
    Addextraaddress1: TMenuItem;
    Autoguessoffsets1: TMenuItem;
    Commands1: TMenuItem;
    Definenewstructure1: TMenuItem;
    Deletecurrentstructure1: TMenuItem;
    edtAddress: TEdit;
    File1: TMenuItem;
    HeaderControl1: THeaderControl;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miAutoCreate: TMenuItem;
    miAutostructsize: TMenuItem;
    miChangeColors: TMenuItem;
    miSaveToCT: TMenuItem;
    miUpdateInterval: TMenuItem;
    N1: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Newwindow1: TMenuItem;
    Open1: TMenuItem;
    Panel1: TPanel;
    Renamestructure1: TMenuItem;
    Save1: TMenuItem;
    Structures1: TMenuItem;
    tvStructureView: TTreeView;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HeaderControl1SectionTrack(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection; Width: Integer; State: TSectionTrackState);
  private
    { private declarations }
    mainStruct: TDissectedStruct;
    procedure TreeViewHScroll(sender: TObject; scrolledleft, maxscrolledleft: integer);
  public
    { public declarations }
  end; 

var
  frmStructures2: TList;


implementation

{$R *.lfm}

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
  parent.DoChangeNotification;
end;

function TStructelement.getVartype: TVariableType;
begin
  result:=fVartype;
end;

procedure TStructelement.setVartype(newVartype: TVariableType);
begin
  fVartype:=newVartype;
  parent.DoChangeNotification;
end;

function TStructelement.getDisplayMethod: TdisplayMethod;
begin
  result:=fDisplayMethod;
end;

procedure TStructelement.setDisplayMethod(newDisplayMethod: TdisplayMethod);
begin
  fDisplayMethod:=newDisplayMethod;
  parent.DoChangeNotification;
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
  parent.DoChangeNotification;
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
  parent.DoChangeNotification;
end;

function TStructelement.getValueFromBase(baseaddress: ptruint): string;
begin
  result:=getvalue(baseaddress+offset);
end;

procedure TStructelement.setValueFromBase(baseaddress: ptruint; value: string);
begin
  setvalue(baseaddress+offset, value);
  parent.DoChangeNotification;
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
  parent.DoChangeNotification;
end;

constructor TStructelement.create(parent:TDissectedStruct);
begin
  self.parent:=parent;
  fbytesize:=1;
end;


{TDissectedStruct}

procedure TDissectedStruct.AddOnChangeNotification(event: TNotifyEvent);
begin
  RemoveOnChangeNotification(event);
  setlength(onChange, length(onChange)+1);
  onChange[length(onchange)-1]:=event;
end;

procedure TDissectedStruct.RemoveOnChangeNotification(event: TNotifyEvent);
var i,j: integer;
begin
  //find it
  For i:=0 to length(onChange)-1 do
  begin
    if (tmethod(onChange[i]).Data=tmethod(event).Data) and (tmethod(onChange[i]).Code=tmethod(event).Code) then
    begin
      //found it
      for j:=i to length(onchange)-2 do
        onchange[i]:=onchange[i+1];

      setlength(onchange, length(onchange)-1);
      break;
    end;
  end;

end;

function TDissectedStruct.getName: string;
begin
  result:=structname;
end;

procedure TDissectedStruct.setName(newname: string);
begin
  structname:=newname;
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

procedure TDissectedStruct.dosort;
begin
  structelementlist.Sort(elementsort);
end;

procedure TDissectedStruct.sortElements;
var i: integer;
begin
  //sort the list
  if isUpdating =false then
    dosort
  else
    updateCalledSort:=true;



  //done, so notify
  DoChangeNotification;
end;

procedure TDissectedStruct.DoChangeNotification;
var i: integer;
begin
  if isUpdating=false then
  begin
    for i:=0 to length(onChange)-1 do
      if assigned(onChange[i]) then
        onChange[i](self);


  end
  else
    updateChangedInformation:=true;
end;

procedure TDissectedStruct.beginUpdate;
begin
  inc(fUpdateCounter)
end;

procedure TDissectedStruct.endUpdate;
begin
  if isUpdating then
    dec(fUpdateCounter);

  if fUpdateCounter=0 then
  begin
    if updatecalledsort then
    begin
      doSort;
      updatecalledsort:=false;
    end;


    if updateChangedInformation then
    begin
      DoChangeNotification;
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
  structelementlist.Remove(element); //no need for sort
  DoChangeNotification;
end;

procedure TDissectedStruct.autoGuessStruct(baseaddress: ptruint; offset: integer; bytesize: integer);
var
  buf: array of byte;

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
    freemem(buf);
  end;
end;

destructor TDissectedStruct.destroy;
begin
  if structelementlist<>nil then
    freeandnil(structelementlist);

  inherited destroy;
end;

constructor TDissectedStruct.create(name: string);
begin
  structname:=name;
  structelementlist:=tlist.Create;
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

  //also add myself to all the struct's change notification
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
   { if mainStruct<>nil then
    begin
      s:=mainStruct.getname+#13;
      x:=(HeaderControl1.Sections[HeaderControl1.Sections.Count-1].Left+HeaderControl1.Sections[HeaderControl1.Sections.Count-1].Width);

      while tvStructureView.Canvas.TextWidth(s)<x do
        s:=s+'PADDING';

      tvStructureView.items[0].Text:=s;

      // tvStructureView.Resize;
    end;   }

  end;

end;

initialization
  frmStructures2:=tlist.Create;

end.

