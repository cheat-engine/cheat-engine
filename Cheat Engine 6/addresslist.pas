unit addresslist;

{$mode DELPHI}

interface

uses
  Windows, Classes, SysUtils, controls, stdctrls, comctrls, ExtCtrls, graphics,
  math, MemoryRecordUnit, FPCanvas, cefuncproc, newkernelhandler, menus,dom,
  XMLRead,XMLWrite, symbolhandler;

type
  TDropByListviewEvent=procedure(sender: TObject; node: TTreenode; attachmode: TNodeAttachMode) of object;
  TAutoAssemblerEditEvent=procedure(sender: TObject; memrec: TMemoryRecord) of object;

  TAddresslist=class(TPanel)
  private
    lastSelected: integer;

    header: THeaderControl;
    Treeview: TTreeview;
    CurrentlyDraggedOverNode: TTreenode;
    CurrentlyDraggedOverBefore: boolean; //set to true if inserting before
    CurrentlyDraggedOverAfter: boolean; //set to true if inserting after
    fOnDropByListview: TDropByListviewEvent;
    fOnAutoAssemblerEdit: TAutoAssemblerEditEvent;

    function getTreeNodes: TTreenodes;
    procedure setTreeNodes(t: TTreenodes);
    procedure AdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure SelectionUpdate(sender: TObject);
    procedure sectiontrack(HeaderControl: TCustomHeaderControl; Section: THeaderSection; Width: Integer; State: TSectionTrackState);
    procedure FocusChange(sender: TObject);
    procedure DragOver(Sender, Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DragDrop(Sender, Source: TObject; X,Y: Integer);
    procedure DragEnd(Sender, Target: TObject; X,Y: Integer);
    procedure TreeviewOnCollapse(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    procedure TreeviewOnExpand(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure TreeviewDblClick(Sender: TObject);
    procedure TreeviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
   // procedure TreeviewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure descriptiondblclick(node: TTreenode);
    procedure addressdblclick(node: TTreenode);
    procedure typedblclick(node: TTreenode);
    procedure valuedblclick(node: TTreenode);
    function GetCount: integer;
    function GetSelcount: integer;
    function GetMemRecItemByIndex(i: integer): TMemoryRecord;
    procedure setPopupMenu(menu: TPopupMenu);
    function getPopupMenu: TPopupMenu;
    function getSelectedRecord: TMemoryRecord;

    function CheatTableNodeHasOnlyAutoAssemblerScripts(CheatTable: TDOMNode): boolean; //helperfunction
  public
    procedure RefreshCustomTypes;
    procedure ReinterpretAddresses;
    procedure ApplyFreeze;
    procedure refresh;
    procedure SelectAll;
    procedure saveTableXMLToNode(CheatEntries: TDOMNode; selectedOnly: boolean=false);
    procedure loadTableXMLFromNode(CheatEntries: TDOMNode);
    function GetTableXMLAsText(selectedonly: boolean): string;
    procedure AddTableXMLAsText(xml: string; simpleCopyPaste: boolean=true);
    procedure DeleteSelected(ask: boolean=true);
    procedure ActivateSelected;
    procedure DeactivateSelected;
    procedure CreateGroup(groupname: string);
    procedure addAutoAssembleScript(script: string);
    function addaddress(description: string; address: string; const offsets: array of dword; offsetcount: integer; vartype: TVariableType; customtypename: string=''; length: integer=0; startbit: integer=0; unicode: boolean=false; node: TTreenode=nil; attachmode: TNodeAttachMode=naAdd): TMemoryRecord;
    function findRecordWithDescription(description: string): TMemoryRecord;

    procedure doDescriptionChange;
    procedure doAddressChange;
    procedure doTypeChange;
    procedure doValueChange;

    constructor Create(AOwner: TComponent); override;
    property Items: TTreeNodes read getTreeNodes write SetTreeNodes;

    procedure clear;
    property Count: Integer read GetCount;
    property SelCount: Integer read GetSelCount;
    property MemRecItems[Index: Integer]: TMemoryRecord read GetMemRecItemByIndex; default;

    property OnDropByListview: TDropByListviewEvent read FOnDropByListview write FOnDropByListview;
    property OnAutoAssemblerEdit: TAutoAssemblerEditEvent read fOnAutoAssemblerEdit write fOnAutoAssemblerEdit;
    property PopupMenu: TpopupMenu read getPopupMenu write setPopupMenu;
    property selectedRecord: TMemoryRecord read getSelectedRecord;
    property headers: THeaderControl read header;
  published

  end;

implementation

uses dialogs, formAddressChangeUnit, TypePopup, PasteTableentryFRM;

procedure TAddresslist.refresh;
begin
  if treeview<>nil then
    treeview.Refresh;
end;

procedure TAddresslist.clear;
var i: integer;
begin
  //first check if it's being edited
  for i:=0 to count-1 do
    if (MemRecItems[i].isBeingEdited) then exit;

  //still here so nothing is being edited, so, delete
  while count>0 do
     MemRecItems[0].Free;
end;

procedure TAddresslist.RefreshCustomTypes;
var i: integer;
begin
  for i:=0 to count-1 do
    MemRecItems[i].RefreshCustomType;
end;

procedure TAddresslist.ReinterpretAddresses;
var i: integer;
begin
  RefreshCustomTypes;
  for i:=0 to count-1 do
    MemRecItems[i].ReinterpretAddress;
end;

procedure TAddresslist.setPopupMenu(menu: TPopupMenu);
begin
  treeview.popupmenu:=menu;
end;

function TAddresslist.getPopupMenu: TPopupMenu;
begin
  result:=treeview.popupmenu;
end;

function TAddresslist.getSelectedRecord: TMemoryRecord;
var i: integer;
begin
  result:=nil;
  if treeview.selected<>nil then
    result:=TMemoryRecord(treeview.selected.data)
  else
  begin
    for i:=0 to count-1 do
      if MemRecItems[i].isSelected then
        result:=MemRecItems[i];
  end;
end;

function TAddresslist.GetSelcount: integer;
var i: integer;
begin
  result:=0;
  for i:=0 to count-1 do
    if MemRecItems[i].isSelected then
      inc(result);
end;

function TAddresslist.GetCount: integer;
begin
  if treeview<>nil then
    result:=treeview.items.count
  else
    result:=0;
end;

function TAddresslist.GetMemRecItemByIndex(i: integer): TMemoryRecord;
begin
  result:=TMemoryRecord(treeview.items[i].data);
end;

procedure TAddresslist.ActivateSelected;
var i: integer;
begin
  for i:=0 to count-1 do
    if memrecitems[i].isSelected then
      memrecitems[i].active:=true;
end;

procedure TAddresslist.DeactivateSelected;
var i: integer;
begin
  for i:=0 to count-1 do
    if memrecitems[i].isSelected then
      memrecitems[i].active:=false;
end;





procedure TAddresslist.SelectAll;
var i: integer;
begin
  for i:=0 to count-1 do
    MemRecItems[i].isSelected:=true;

  refresh;
end;

procedure TAddresslist.DeleteSelected(ask: boolean=true);
var i: integer;
multi: string;
oldindex: integer;
begin
  oldindex:=selectedRecord.treenode.AbsoluteIndex;

  if count=0 then exit;

  if selcount=0 then exit;
  if selcount=1 then multi:='' else multi:='es';

  if (not ask) or (messagedlg('Do you want to delete the selected address'+multi+'?',mtConfirmation, [mbyes,mbno],0) = mryes) then
  begin
    i:=0;
    while i<count do
    begin
      if MemRecItems[i].isSelected and (MemRecItems[i].isBeingEdited=false) then
        MemRecItems[i].Free //Free also cleans up it's associated treenode, and all it's children
      else
        inc(i);
    end;
  end;

  if oldindex>=treeview.items.count then
    oldindex:=treeview.items.count-1;

  if oldindex>-1 then
    treeview.items[oldindex].Selected:=true

end;

procedure TAddresslist.ApplyFreeze;
{Freeze all the records that are active}
var i: integer;
begin
  for i:=0 to count-1 do
    memrecitems[i].ApplyFreeze;
end;

procedure TAddresslist.saveTableXMLToNode(CheatEntries: TDOMNode; selectedOnly: boolean=false);
var tn: TTreenode;
begin
  tn:=treeview.Items.GetFirstNode;
  while tn<>nil do
  begin
    TMemoryRecord(tn.data).getXMLNode(CheatEntries,selectedonly);
    tn:=tn.GetNextSibling;
  end;
end;

procedure TAddresslist.loadTableXMLFromNode(CheatEntries: TDOMNode);
var currentEntry: TDOMNode;
memrec: TMemoryRecord;
begin
  currentEntry:=CheatEntries.FirstChild;
  while currententry<>nil do
  begin
    if tdomelement(currententry).TagName='CheatEntry' then
    begin
      //create a blank entry
      memrec:=TMemoryRecord.create(self);
      memrec.treenode:=Treeview.Items.AddObject(nil,'',memrec);

      //fill the entry with the node info
      memrec.setXMLnode(currentEntry);
    end;
    currentEntry:=currentEntry.NextSibling;
  end;

end;

function TAddresslist.GetTableXMLAsText(selectedonly: boolean): string;
var
  doc: TXMLDocument;
  cheattable,CheatEntries: TDOMNode;
  i: integer;

  s: TStringstream;


begin
  result:='';
  doc:=TXMLDocument.Create;
  s:=TStringstream.create('');


  cheattable:=doc.CreateElement('CheatTable');
  doc.AppendChild(cheattable);

  CheatEntries:=doc.CreateElement('CheatEntries');
  cheattable.AppendChild(CheatEntries);

  try
    saveTableXMLToNode(CheatEntries, selectedOnly);
    WriteXMLFile(doc,s);
    result:=s.DataString;
  finally
    doc.free;
    s.free;
  end;
end;

function TAddresslist.CheatTableNodeHasOnlyAutoAssemblerScripts(CheatTable: TDOMNode): boolean;
{
private
checks if the given xml document contains cheatentries that aren't aa scripts
}
var CheatEntries, currentEntry: TDOMNode;
begin
  result:=true;
  //go through the list untill one is found that has the custom type
  CheatEntries:=CheatTable.FindNode('CheatEntries');
  if cheatentries<>nil then
  begin
    currentEntry:=CheatEntries.FirstChild;
    while currententry<>nil do
    begin
      if StringToVariableType(currententry.findnode('VariableType').TextContent)<>vtAutoAssembler then
      begin
        result:=false;
        exit;
      end;
      currentEntry:=currentEntry.NextSibling;
    end;
  end;
end;

procedure TAddresslist.AddTableXMLAsText(xml: string; simpleCopyPaste: boolean=true);
var doc: TXMLDocument;
    insertafter: TTreenode;
    memrec: TMemoryRecord;

    CheatTable: TDOMNode;
    CheatEntries: TDOMNode;

    currentEntry: TDOMNode;

    s: TStringStream;

    replace_find: string;
    replace_with: string;
    changeoffsetstring: string;
    changeoffset: ptrUint;
    x: ptrUint;

begin
  doc:=nil;
  s:=nil;

  s:=TStringstream.Create(xml);

  try
    try
      ReadXMLFile(doc, s);

      insertafter:=treeview.Selected;

      CheatTable:=doc.FindNode('CheatTable');
      if cheattable<>nil then
      begin
        CheatEntries:=CheatTable.FindNode('CheatEntries');
        if CheatEntries<>nil then
        begin
          currentEntry:=CheatEntries.FirstChild;
          if currententry<>nil then //valid
          begin
            frmPasteTableentry:=TfrmPasteTableentry.create(self);
            try
              if not simplecopypaste then
              begin
                //check if it's needed (is at least one address not an auto assembler script ?
                if not CheatTableNodeHasOnlyAutoAssemblerScripts(CheatTable) then
                  if frmpastetableentry.showmodal=mrcancel then exit;
              end;

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
              freeandnil(frmPasteTableentry);
            end;

          end;


          while currententry<>nil do
          begin
            if tdomelement(currententry).TagName='CheatEntry' then
            begin

              //create a blank entry
              memrec:=TMemoryRecord.create(self);
              memrec.treenode:=Treeview.Items.AddObject(nil,'',memrec);
              if insertAfter<>nil then
                memrec.treenode.MoveTo(insertafter, naInsertBehind);

              //fill the entry with the node info
              memrec.setXMLnode(currentEntry);

              if replace_find<>'' then
                memrec.Description:=stringreplace(memrec.Description,replace_find,replace_with,[rfReplaceAll,rfIgnoreCase]);

              if memrec.interpretableaddress<>'' then //always true
              begin
                try
                  x:=symhandler.getAddressFromName(memrec.interpretableaddress);
                  x:=x+changeoffset;
                  memrec.interpretableaddress:=symhandler.getNameFromAddress(x,true,true)
                except
                  memrec.interpretableaddress:=inttohex(memrec.getBaseAddress+changeoffset,8);
                end;
              end;
            end;
            currentEntry:=currentEntry.NextSibling;
          end;
        end;
      end;
    finally
      if doc<>nil then
        doc.free;

      if s<>nil then
        s.free;
    end;
  except
    //don't complain
  end;
end;

procedure TAddresslist.CreateGroup(groupname: string);
var
  memrec: TMemoryRecord;
begin
  memrec:=TMemoryrecord.Create(self);
  memrec.isGroupHeader:=true;
  memrec.Description:=groupname;
  memrec.treenode:=Treeview.Items.AddObject(nil,'',memrec);
  memrec.treenode.DropTarget:=true;
end;

procedure TAddresslist.addAutoAssembleScript(script: string);
var
  memrec: TMemoryRecord;
begin
  memrec:=TMemoryrecord.Create(self);
  memrec.isGroupHeader:=false;
  memrec.Description:='Auto Assemble script';
  memrec.AutoAssemblerData.script:=tstringlist.create;
  memrec.AutoAssemblerData.script.text:=script;

  memrec.VarType:=vtAutoAssembler;

  memrec.treenode:=Treeview.Items.AddObject(nil,'',memrec);
  memrec.treenode.DropTarget:=true;
end;

function TAddresslist.findRecordWithDescription(description: string): TMemoryRecord;
var i: integer;
begin
  result:=nil;
  for i:=0 to count-1 do
    if uppercase(MemRecItems[i].Description)=uppercase(description) then
    begin
      result:=MemRecItems[i];
      exit;
    end;

end;

function TAddresslist.addaddress(description: string; address: string; const offsets: array of dword; offsetcount: integer; vartype: TVariableType; customtypename: string=''; length: integer=0; startbit: integer=0; unicode: boolean=false; node: TTreenode=nil; attachmode: TNodeAttachMode=naAdd): TMemoryRecord;
var
  memrec: TMemoryRecord;
  i: integer;
  t: TTreenode;
begin
  memrec:=TMemoryRecord.create(self);

  memrec.Description:=description;
  memrec.interpretableaddress:=address;


  memrec.VarType:=vartype;
  memrec.CustomTypeName:=customtypename;

  setlength(memrec.pointeroffsets,offsetcount);
  for i:=0 to offsetcount-1 do
    memrec.pointeroffsets[i]:=offsets[i];


  case vartype of
    vtString:
    begin
      memrec.extra.stringData.unicode:=unicode;
      memrec.Extra.stringData.length:=length;
    end;

    vtBinary:
    begin
      memrec.Extra.bitData.Bit:=startbit;
      memrec.Extra.bitData.bitlength:=length;
    end;

    vtByteArray:
    begin
      memrec.showAsHex:=true; //aob's are hex by default
      memrec.Extra.byteData.bytelength:=length;
    end;
  end;

  memrec.ReinterpretAddress;

  memrec.treenode:=Treeview.Items.AddObject(nil,'',memrec);
  if node<>nil then
    memrec.treenode.MoveTo(node, attachmode);

  result:=memrec;
end;

procedure TAddresslist.setTreeNodes(t: TTreenodes);
begin
  TreeView.Items:=t;
end;

function TAddresslist.getTreeNodes: TTreenodes;
begin
  result:=TreeView.Items;
end;

procedure TAddresslist.doDescriptionChange;
begin
  if treeview.selected<>nil then descriptiondblclick(treeview.selected);
end;

procedure TAddresslist.doAddressChange;
begin
  if treeview.selected<>nil then addressdblclick(treeview.selected);
end;

procedure TAddresslist.doTypeChange;
begin
  if treeview.selected<>nil then typedblclick(treeview.selected);
end;

procedure TAddresslist.doValueChange;
begin
  if treeview.selected<>nil then valuedblclick(treeview.selected);
end;



procedure TAddresslist.descriptiondblclick(node: TTreenode);
var i: integer;
    description: string;
begin
  description:=tmemoryrecord(node.data).description;

  if InputQuery('Change Description','What will be the new description?', description) then
    tmemoryrecord(node.data).description:=description;

  node.update;
end;

procedure TAddresslist.addressdblclick(node: TTreenode);
begin


  with TFormaddresschange.Create(self) do
  begin
    memoryrecord:=TMemoryRecord(node.data);
    showmodal;

    free;

    memoryrecord.ReinterpretAddress;
    node.update;
  end;
end;

procedure TAddresslist.typedblclick(node: TTreenode);
var
  i, j: integer;
  newtype,oldType: TVariableType;
  memrec: TMemoryRecord;
begin
  TypeForm.RefreshCustomTypes;

  memrec:=TMemoryRecord(node.data);
  OldType:=memrec.Vartype;


  case memrec.vartype of
    vtCustom:  typeform.VarType.itemindex:=typeform.VarType.Items.IndexOf(memrec.CustomTypeName);

    vtBinary:
    begin
      TypeForm.VarType.itemindex:=1;
      TypeForm.Edit2.text:=IntToStr(memrec.extra.bitData.bitlength);

      case memrec.extra.bitData.Bit of
        0     :       TypeForm.RadioButton1.checked:=true;
        1     :       TypeForm.RadioButton2.checked:=true;
        2     :       TypeForm.RadioButton3.checked:=true;
        3     :       TypeForm.RadioButton4.checked:=true;
        4     :       TypeForm.RadioButton5.checked:=true;
        5     :       TypeForm.RadioButton6.checked:=true;
        6     :       TypeForm.RadioButton7.checked:=true;
        7     :       TypeForm.RadioButton8.checked:=true;
      end;
    end;
    vtByte:   TypeForm.VarType.itemindex:=1;
    vtWord:   TypeForm.VarType.itemindex:=2;
    vtDword:  TypeForm.VarType.itemindex:=3;
    vtQword:  TypeForm.VarType.itemindex:=4;
    vtSingle: TypeForm.VarType.itemindex:=5;
    vtDouble: TypeForm.VarType.itemindex:=6;
    vtString:
    begin
      TypeForm.VarType.itemindex:=7;
      TypeForm.Edit1.text:=inttostr(memrec.Extra.stringData.length);
      typeform.cbunicode.checked:=memrec.Extra.stringData.unicode;
    end;
    vtByteArray:
    begin
      TypeForm.edit1.text:=inttostr(memrec.Extra.byteData.bytelength);
      TypeForm.VarType.itemindex:=8;
      Typeform.cbunicode.visible:=false;
    end;
  end;

  typeform.MemoryRecord:=memrec;
  if TypeForm.Showmodal=mrNo then exit;

  newtype:=memrec.VarType;

  for i:=0 to count-1 do
  begin
    if (MemRecItems[i].isSelected) then
    begin
      if MemRecItems[i].vartype<>vtAutoAssembler then
        MemRecItems[i].VarType:=newtype;

      MemRecItems[i].active:=false;

      MemRecItems[i].treenode.update;
    end;
  end;

end;

procedure TAddresslist.valuedblclick(node: TTreenode);
{
Doubeclcik on the value
This results in showing the value editor mode
}
var
  value: string;
  memrec: TMemoryRecord;
  i: integer;
  someerror: boolean;
  allError: boolean;
begin
  memrec:=TMemoryRecord(node.data);
  value:=memrec.GetValue;

  if (value = '??') or (value = 'NAN') or (value = 'INF') then
  begin
    beep; //my favourite sound
    exit;
  end;


  if (selcount=1) and (selectedRecord.VarType=vtAutoAssembler) then
  begin
    //if it's an autoassemblerscript then spawn the autoassembler script editor that the owner might want to use
    if assigned(fOnAutoAssemblerEdit) then
    begin
      fOnAutoAssemblerEdit(self, self.selectedRecord);
      exit;
    end;
  end;




  if InputQuery('Change Value', 'what value to change this to?', value) then
  begin
    allError:=true;
    someError:=false;
    for i:=0 to count-1 do
      if memrecitems[i].isSelected then
      begin
        try
          memrecitems[i].SetValue(value);
          memrecitems[i].treenode.update;
          allError:=false;
        except
          someError:=true;
        end;
      end;

    if AllError then raise exception.create('The value '+value+' could not be parsed');
    if SomeError then raise exception.create('Not all value types could handle the value '+value);
  end;
end;

procedure TAddresslist.TreeviewOnExpand(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var r: TMemoryRecord;
begin
  AllowExpansion:=true;

  r:=TMemoryRecord(node.data);
  if (moHideChildren in r.options) and (not r.active) then //if not active then don't allow expanding
    AllowExpansion:=false;
end;

procedure TAddresslist.TreeviewOnCollapse(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
var r: TMemoryRecord;
begin
  AllowCollapse:=false;
  r:=TMemoryRecord(node.data);
  if (moHideChildren in r.options) and (not r.active) then //if not active then allow collapse
    AllowCollapse:=true;
end;

procedure TAddresslist.TreeviewDblClick(Sender: TObject);
var
  tvRect: TRect;
  x,y: integer;
  node: TTreenode;
  i: integer;
begin
  GetWindowRect(treeview.Handle, tvrect);
  x:=mouse.CursorPos.x-tvrect.left;
  y:=mouse.CursorPos.y-tvrect.top;

  node:=treeview.GetNodeAt(x,y);
  if node<>nil then
  begin
    //at least something was clicked

    if TMemoryRecord(node.data).isGroupHeader then
    begin
      //it's a group doubleclick
      descriptiondblclick(node);
      exit;
    end;

    for i:=0 to header.Sections.count-1 do
      if inrange(x,header.Sections[i].Left,header.Sections[i].right) then
      begin
        //if it's a auto assemble script only do the description and value
        if (TMemoryRecord(node.data).VarType=vtAutoAssembler) then
        begin
          case i of
            0: ; //frozen doubleclick
            1: descriptiondblclick(node);
            2: ; //valuedblclick(node);
            3: ; //valuedblclick(node);
            4: valuedblclick(node);
          end;
        end
        else
        begin
          case i of
            0: ; //frozen doubleclick
            1: descriptiondblclick(node);
            2: addressdblclick(node);
            3: typedblclick(node);
            4: valuedblclick(node);
          end;
        end;
      end;
  end;
end;

procedure TAddresslist.TreeviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  linerect,textrect: TRect;
  node: TTreenode;

  checkboxstart, checkboxend: integer;
begin
  node:=treeview.GetNodeAt(x,y);
  if node<>nil then
  begin
    textrect:=node.DisplayRect(true);
    linerect:=node.DisplayRect(false);
    //check if the checkbox is clicked

    checkboxstart:=textrect.left+1;
    checkboxend:=textrect.left+1+(linerect.bottom-linerect.top)-2;
    if inrange(x, checkboxstart, checkboxend ) then
    begin
      //checkbox click
      TMemoryRecord(node.data).Active:=not TMemoryRecord(node.data).Active;
    end;

    if TMemoryRecord(node.data).Active then
    begin
      //arrow spot is clicked
      //nothing->increased->decreased->nothing->...
      if inrange(x, checkboxend+1, checkboxend+9) then
      begin
        if TMemoryRecord(node.data).allowIncrease then TMemoryRecord(node.data).allowDecrease:=true
        else
        if TMemoryRecord(node.data).allowDecrease then TMemoryRecord(node.data).allowDecrease:=false
        else
          TMemoryRecord(node.data).allowIncrease:=true

      end;
    end;
  end;
end;

{
procedure TAddresslist.TreeviewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=VK_DELETE then
    deleteSelected;
end;   }

procedure TAddresslist.sectiontrack(HeaderControl: TCustomHeaderControl; Section: THeaderSection; Width: Integer; State: TSectionTrackState);
begin
  treeview.Refresh;
end;

procedure TAddresslist.FocusChange(sender: TObject);
begin
  treeview.refresh;
end;

procedure TAddresslist.DragEnd(Sender, Target: TObject; X,Y: Integer);
begin
  CurrentlyDraggedOverNode:=nil;
end;

procedure TAddresslist.DragOver(Sender, Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
begin
  CurrentlyDraggedOverNode:=TreeView.GetNodeAt(x,y);

  CurrentlyDraggedOverBefore:=false;
  CurrentlyDraggedOverAfter:=false;

  if (CurrentlyDraggedOverNode<>nil) and (TMemoryRecord(CurrentlyDraggedOverNode.data).isGroupHeader=false) then //if something focused AND not a groupheader
  begin
    outputdebugstring(inttostr(y-(CurrentlyDraggedOverNode.top)));
    CurrentlyDraggedOverBefore:=(y-CurrentlyDraggedOverNode.top)<(CurrentlyDraggedOverNode.height div 3); //it's before if the offset into the node is smaller than half the height - 2
    CurrentlyDraggedOverAfter:=(y-CurrentlyDraggedOverNode.top)>(CurrentlyDraggedOverNode.height div 3)*2;
  end; //groupheaders are always perfect targets


  accept:=true;
  treeview.refresh;
end;

procedure TAddresslist.DragDrop(Sender, Source: TObject; X,Y: Integer);
var
  node: TTreenode;
  i: integer;

  selectednodelist: array of TTreenode;
begin
  setlength(selectednodelist,0);
  for i:=0 to treeview.items.count-1 do
    if TMemoryRecord(treeview.items[i].data).isSelected then
    begin
      setlength(selectednodelist,length(selectednodelist)+1);
      selectednodelist[length(selectednodelist)-1]:=treeview.items[i];
    end;

  node:=TreeView.GetNodeAt(x,y);




  if node<>nil then
  begin
    if not (CurrentlyDraggedOverBefore or CurrentlyDraggedOverAfter) then //add it
    begin
      //add it to this entry at the end

      if source=treeview then //just be sure
        for i:=0 to length(selectednodelist)-1 do
          selectednodelist[i].MoveTo(node,naAddChild);

      if source is TListView then
        if assigned(fOnDropByListview) then
          fOnDropByListview(self, node, naAddChild);

    end
    else
    begin
      //else place it before or after this object   depending on the x,y pos
      if CurrentlyDraggedOverBefore then //before
      begin
        if source=treeview then
          for i:=0 to length(selectednodelist)-1 do
            selectednodelist[i].MoveTo(node, naInsert); //in front of destination


        if source is TListView then
          if assigned(fOnDropByListview) then
            fOnDropByListview(self, node, naInsert);
      end
      else
      begin  //after
        if source=treeview then
          for i:=length(selectednodelist)-1 downto 0 do
            selectednodelist[i].MoveTo(node, naInsertBehind); //after

        if source is Tlistview then
        begin
          //just insert it before the next node if possible
          if node.GetNextSibling<>nil then
          begin
            node:=node.GetNextSibling;

            if assigned(fOnDropByListview) then
              fOnDropByListview(self, node, naInsert);
          end
          else
            fOnDropByListview(self, node, naAdd);
        end;
      end;
    end;
  end else
  begin
    //place it at the very end

    if source=treeview then
      for i:=length(selectednodelist)-1 downto 0 do
        selectednodelist[i].MoveTo(nil, naAdd); //last sibling


    if source is Tlistview then
      if assigned(fOnDropByListview) then
        fOnDropByListview(self, node, naInsertBehind);

  end;

  treeview.DropTarget:=nil;
  treeview.refresh;
end;

procedure TAddresslist.SelectionUpdate(sender: TObject);
var shift:TShiftState;
    i: integer;
   // firstnode, lastNode: TTreenode;
begin
  //Because the multiselect of lazarus is horribly broken in the build I use, I've just implemented it myself

  shift:=GetKeyShiftState;

  if Treeview.Selected<>nil then
  begin
    if ssShift in shift then
    begin
      //if shift is held then unselect the old selection and select everything between the last selection and the current selection as selected , and don't update the last selection
      //deselect everything
      for i:=0 to Count-1 do
        MemRecItems[i].isSelected:=false;

      //select verything inbetween

      for i:=min(lastselected,treeview.selected.absoluteIndex) to max(lastselected,treeview.selected.absoluteIndex) do
        MemRecItems[i].isSelected:=true;
    end
    else
    if (ssCtrl in shift)  then //ctrl only works when used with left mouse
    begin
      //if control is held then leave everything selected and add the current item to the selected list
      TMemoryRecord(Treeview.Selected.data).isSelected:=not TMemoryRecord(Treeview.Selected.data).isSelected;

      lastSelected:=Treeview.Selected.AbsoluteIndex;

      if not TMemoryRecord(Treeview.Selected.data).isSelected then
        Treeview.Selected:=nil;

    end
    else
    begin
      //else unselect all old selections (and select the current item)
      for i:=0 to Count-1 do
        MemRecItems[i].isSelected:=false;

      TMemoryRecord(Treeview.Selected.data).isSelected:=true;
      lastSelected:=Treeview.Selected.AbsoluteIndex;
    end;
  end;
end;

procedure TAddresslist.AdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  textrect: trect;
  linerect: trect;
  fulltextline: trect;
  memrec: TMemoryRecord;

  checkbox: trect;
  oldbrushcolor: TColor;
  pointertag: string;
  oldpenmode: TFPPenMode;
  oldpencolor: tcolor;

  descriptionstart: integer;
begin
  //multiselect implementation
  DefaultDraw:=true;
  PaintImages:=true;

  if stage=cdPostPaint then
  begin
    oldbrushcolor:=sender.Canvas.Brush.Color;
    textrect:=node.DisplayRect(true);
    linerect:=node.DisplayRect(false);
    fulltextline:=linerect;
    fulltextline.Left:=textrect.Left;

    memrec:=TMemoryRecord(Node.data);


    sender.Canvas.Brush.color:=sender.Color;
    fulltextline.left:=0;
    sender.Canvas.FillRect(fulltextline); //whipe the original text

    //exit;


    if memrec.isSelected then
    begin
      if node.Selected then
        sender.Canvas.Brush.Color:=clHighlight
      else
        sender.Canvas.Brush.Color:=clActiveCaption;

      oldpenmode:=sender.Canvas.Pen.Mode;
      sender.Canvas.Pen.Mode:=pmMask;
      sender.canvas.pen.color:=sender.Canvas.Brush.Color;

      sender.Canvas.Rectangle(linerect);
      sender.Canvas.Pen.Mode:=oldpenmode;
    end;


    sender.canvas.pen.color:=clWindowtext;

    sender.Canvas.Font.Color:=memrec.Color;

    checkbox.Left:=textrect.left+1; //(header.Sections[0].Width div 2)-((linerect.bottom-linerect.top) div 2)+1;
    checkbox.Right:=checkbox.left+(linerect.bottom-linerect.top)-2; //(header.Sections[0].Width div 2)+((linerect.bottom-linerect.top) div 2)-1;
    checkbox.Top:=linerect.top+1;
    checkbox.Bottom:=linerect.bottom-1;
    sender.Canvas.Rectangle(checkbox);

    if memrec.Active then
    begin
      oldpencolor:=sender.canvas.pen.color;

      if memrec.isSelected then
        sender.canvas.pen.color:=clBlack
      else
        sender.canvas.pen.color:=clRed;
      sender.canvas.Line(checkbox.left+1,checkbox.Top+1, checkbox.Right-1,checkbox.bottom-1);
      sender.canvas.line(checkbox.right-1-1,checkbox.top+1, checkbox.left,checkbox.bottom-1);

      sender.canvas.pen.color:=oldpencolor;


      if (not memrec.isGroupHeader) and (memrec.VarType<>vtAutoAssembler) then
      begin
        //draw the arrow up/down, unless it's a group or auto assembler type
        if memrec.allowIncrease then
        begin
          sender.Canvas.Pen.Color:=clGreen;
          sender.canvas.line(checkbox.right+5, checkbox.bottom-1, checkbox.right+5,checkbox.top+1);
          sender.canvas.line(checkbox.right+5,checkbox.top+1,checkbox.Right+5-4,checkbox.top+1+4);
          sender.canvas.line(checkbox.right+5,checkbox.top+1,checkbox.Right+5+4,checkbox.top+1+4);
          sender.canvas.pen.color:=clWindowtext;
        end;

        if memrec.allowDecrease then
        begin
          sender.Canvas.Pen.Color:=clRed;
          sender.canvas.line(checkbox.right+5, checkbox.bottom-1, checkbox.right+5,checkbox.top+1);
          sender.canvas.line(checkbox.right+5,checkbox.bottom-1,checkbox.Right+5-4,checkbox.bottom-1-4);
          sender.canvas.line(checkbox.right+5,checkbox.bottom-1,checkbox.Right+5+4,checkbox.bottom-1-4);
          sender.canvas.pen.color:=clWindowtext;
        end;
      end;

    end;

    descriptionstart:=max(checkbox.right+10,header.Sections[1].Left);


    if (memrec.isGroupHeader=false) and (memrec.VarType<>vtAutoAssembler) then //if it's not a groupheader of auto assemble script then show the extra data
    begin
      //limit how far the texts go depending on the sections
      sender.Canvas.TextRect(rect(descriptionstart, textrect.Top, header.Sections[1].right, textrect.bottom), descriptionstart, textrect.Top, memrec.description);

      //if this is not the currently dragged over node
      //or if it is and either CurrentlyDraggedOverBefore or CurrentlyDraggedOverAfter is set then draw the rest
      if not ((node=CurrentlyDraggedOverNode) and (not (CurrentlyDraggedOverBefore or CurrentlyDraggedOverAfter))) then //don't draw the rest on insert drag/drop
      begin
        sender.Canvas.TextRect(rect(header.Sections[2].left, textrect.Top, header.Sections[2].right, textrect.bottom),header.Sections[2].Left, textrect.Top, memrec.addressString);
        case memrec.vartype of
          vtCustom: sender.Canvas.TextRect(rect(header.Sections[3].left, textrect.Top, header.Sections[3].right, textrect.bottom),header.sections[3].left, textrect.top, memrec.CustomTypeName);
          vtString: sender.Canvas.TextRect(rect(header.Sections[3].left, textrect.Top, header.Sections[3].right, textrect.bottom),header.sections[3].left, textrect.top, VariableTypeToString(memrec.VarType)+'['+inttostr(memrec.Extra.stringData.length)+']');
          else sender.Canvas.TextRect(rect(header.Sections[3].left, textrect.Top, header.Sections[3].right, textrect.bottom),header.sections[3].left, textrect.top, VariableTypeToString(memrec.VarType));
        end;


        sender.Canvas.TextRect(rect(header.Sections[4].left, textrect.Top, header.Sections[4].right, textrect.bottom),header.sections[4].left, textrect.top, memrec.GetValue);
      end;
    end
    else
    begin
      sender.Canvas.TextOut(descriptionstart, textrect.Top, memrec.description); //no limit on how far

      if (memrec.VarType=vtAutoAssembler) then //give it the <script> text for value
        sender.Canvas.TextRect(rect(header.Sections[4].left, textrect.Top, header.Sections[4].right, textrect.bottom),header.sections[4].left, textrect.top, '<script>');

    end;

    if node=CurrentlyDraggedOverNode then
    begin
      if CurrentlyDraggedOverBefore then //draw before
        sender.Canvas.Line(0,max(0,linerect.top-1),linerect.right,max(0,linerect.top-1))
      else
      if CurrentlyDraggedOverAfter then //raw after
        sender.Canvas.Line(0,linerect.bottom-1,linerect.right,linerect.bottom-1)
      else  //draw inside
        sender.Canvas.Line(descriptionstart+sender.canvas.textwidth(memrec.description)+1,(linerect.top+linerect.Bottom) div 2,linerect.right,(linerect.top+linerect.Bottom) div 2)
    end;


    if sender.Focused and node.Selected then
      sender.Canvas.DrawFocusRect(linerect);

    sender.Canvas.Brush.Color:=oldbrushcolor;
  end;


end;


constructor TAddresslist.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  treeview:=TTreeview.create(self);

  treeview.RowSelect:=true;
  treeview.ReadOnly:=true;
  treeview.ShowRoot:=false;
 // treeview.multiselect:=true;  bad
  Treeview.RightClickSelect:=true;

  treeview.DragMode:=dmAutomatic;
  treeview.HideSelection:=false;

  treeview.ShowButtons:=true;


  treeview.AutoExpand:=true;
  treeview.Options:=treeview.options+[tvoAutoExpand];


  treeview.OnAdvancedCustomDrawItem:=AdvancedCustomDrawItem;
  treeview.OnSelectionChanged:=SelectionUpdate;
  treeview.OnExit:=Focuschange;
  treeview.OnEnter:=Focuschange;

  treeview.OnDragOver:=DragOver;
  treeview.OnDragDrop:=DragDrop;
  treeview.OnEndDrag:=DragEnd;
 // treeview.OnKeyDown:=treeviewkeydown;
  //treeview.Indent:=2;
  treeview.OnCollapsing:=TreeviewOnCollapse;
  treeview.OnExpanding:=TreeviewOnExpand;

  treeview.OnMouseDown:=TreeviewMouseDown;
  treeview.OnDblClick:=TreeviewDblClick;

  //treeview.Options:=treeview.Options+[tvoAllowMultiselect];    Horribly broken

  treeview.parent:=self;




  header:=THeaderControl.Create(self);
  header.parent:=self;
  header.Align:=alTop;
  header.height:=20;

  with header.Sections.Add do
  begin
    Text:='Active';
    Width:=40;
  end;

  with header.Sections.Add do
  begin
    Text:='Description';
    Width:=160;
  end;

  with header.Sections.Add do
  begin
    Text:='Address';
    Width:=85;
  end;

  with header.Sections.Add do
  begin
    Text:='Type';
    Width:=60;
  end;

  with header.Sections.Add do
  begin
    Text:='Value';
    Width:=9000000;
  end;

  header.OnSectionTrack:=SectionTrack;

  treeview.ScrollBars:=ssVertical;
  treeview.Align:=alClient;

end;

end.

