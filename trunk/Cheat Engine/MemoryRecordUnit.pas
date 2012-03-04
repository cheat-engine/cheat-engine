unit MemoryRecordUnit;

{$mode DELPHI}

interface

uses
  Windows, forms, graphics, Classes, SysUtils, controls, stdctrls, comctrls,symbolhandler,
  cefuncproc,newkernelhandler, autoassembler, hotkeyhandler, dom, XMLRead,XMLWrite,
  customtypehandler, fileutil, LCLProc;

type TMemrecHotkeyAction=(mrhToggleActivation, mrhToggleActivationAllowIncrease, mrhToggleActivationAllowDecrease, mrhSetValue, mrhIncreaseValue, mrhDecreaseValue);

type TFreezeType=(ftFrozen, ftAllowIncrease, ftAllowDecrease);



type TMemrecOption=(moHideChildren, moBindActivation, moRecursiveSetValue, moAllowManualCollapseAndExpand);
type TMemrecOptions=set of TMemrecOption;

type TMemrecStringData=record
  unicode: boolean;
  length: integer;
  ZeroTerminate: boolean;
end;

type TMemRecBitData=record
      Bit     : Byte;
      bitlength: integer;
      showasbinary: boolean;
    end;

type TMemRecByteData=record
      bytelength: integer;
    end;

type TMemRecAutoAssemblerData=record
      script: tstringlist;
      allocs: TCEAllocArray;
      registeredsymbols: TStringlist;
    end;

type TMemRecExtraData=record
    case integer of
      1: (stringData: TMemrecStringData); //if this is the last level (maxlevel) this is an PPointerList
      2: (bitData: TMemRecBitData);   //else it's a PReversePointerListArray
      3: (byteData: TMemRecByteData);
  end;




type
  TActivateEvent=function (sender: TObject; before, currentstate: boolean): boolean of object;
  TMemoryRecordHotkey=class;
  TMemoryRecord=class
  private
    fID: integer;
    FrozenValue : string;
    CurrentValue: string;
    UndoValue   : string;  //keeps the last value before a manual edit


    UnreadablePointer: boolean;
    BaseAddress: ptrUint; //Base address
    RealAddress: ptrUint; //If pointer, or offset the real address
    fIsOffset: boolean;


    fShowAsSignedOverride: boolean;
    fShowAsSigned: boolean;

    fActive: boolean;
    fAllowDecrease: boolean;
    fAllowIncrease: boolean;
    fOwner: TObject;

    fShowAsHex: boolean;
    editcount: integer; //=0 when not being edited

    fOptions: TMemrecOptions;

    CustomType: TCustomType;
    fCustomTypeName: string;
    fColor: TColor;
    fVisible: boolean;

    fVarType : TVariableType;

    couldnotinterpretaddress: boolean; //set when the address interpetation has failed since last eval

    hknameindex: integer;

    Hotkeylist: tlist;

    fonactivate, fondeactivate: TActivateEvent;
    fOnDestroy: TNotifyEvent;
    function getByteSize: integer;
    function BinaryToString(b: pbytearray; bufsize: integer): string;
    function getAddressString: string;
    function getuniquehotkeyid: integer;
    procedure setActive(state: boolean);
    procedure setAllowDecrease(state: boolean);
    procedure setAllowIncrease(state: boolean);
    procedure setVisible(state: boolean);
    procedure setShowAsHex(state: boolean);
    procedure setOptions(newOptions: TMemrecOptions);
    procedure setCustomTypeName(name: string);
    procedure setColor(c: TColor);
    procedure setVarType(v:  TVariableType);
    function getHotkeyCount: integer;
    function getHotkey(index: integer): TMemoryRecordHotkey;
    function GetshowAsSigned: boolean;
    procedure setShowAsSigned(state: boolean);


    function getChildCount: integer;
    function getChild(index: integer): TMemoryRecord;



    procedure setID(i: integer);
    function getParent: TMemoryRecord;
  public


    isGroupHeader: Boolean; //set if it's a groupheader, only the description matters then


    Description : string;
    interpretableaddress: string;


    pointeroffsets: array of integer; //if set this is an pointer



    Extra: TMemRecExtraData;
    AutoAssemblerData: TMemRecAutoAssemblerData;



    treenode: TTreenode;

    isSelected: boolean; //lazarus bypass. Because lazarus does not implement multiselect I have to keep track of which entries are selected

    showAsHex: boolean;

    //free for editing by user:
    autoAssembleWindow: TForm; //window storage for an auto assembler editor window

    function hasSelectedParent: boolean;
    function hasParent: boolean;


    function isBeingEdited: boolean;
    procedure beginEdit;
    procedure endEdit;

    function isPointer: boolean;
    function isOffset: boolean;
    procedure ApplyFreeze;

    function GetValue: string;
    procedure SetValue(v: string); overload;
    procedure SetValue(v: string; isFreezer: boolean); overload;
    procedure UndoSetValue;
    function canUndo: boolean;
    procedure increaseValue(value: string);
    procedure decreaseValue(value: string);
    function GetRealAddress: PtrUInt;
    function getBaseAddress: ptrUint; //return the base address, if offset, the calculated address
    procedure RefreshCustomType;
    function ReinterpretAddress(forceremovalofoldaddress: boolean=false): boolean;
    property Value: string read GetValue write SetValue;
    property bytesize: integer read getByteSize;

    function hasHotkeys: boolean;
    function Addhotkey(keys: tkeycombo; action: TMemrecHotkeyAction; value, description: string): TMemoryRecordHotkey;
    function removeHotkey(hk: TMemoryRecordHotkey): boolean;

    procedure DoHotkey(hk :TMemoryRecordHotkey); //execute the specific hotkey action

    procedure disablewithoutexecute;
    procedure refresh;

    procedure getXMLNode(node: TDOMNode; selectedOnly: boolean);
    procedure setXMLnode(CheatEntry: TDOMNode);

    procedure SetVisibleChildrenState;

    constructor Create(AOwner: TObject);
    destructor destroy; override;

    property ID: integer read fID write setID;
    property addressString: string read getAddressString;
    property active: boolean read fActive write setActive;

    property allowDecrease: boolean read fallowDecrease write setAllowDecrease;
    property allowIncrease: boolean read fallowIncrease write setAllowIncrease;
    property showAsHex: boolean read fShowAsHex write setShowAsHex;
    property showAsSigned: boolean read getShowAsSigned write setShowAsSigned;
    property options: TMemrecOptions read fOptions write setOptions;
    property CustomTypeName: string read fCustomTypeName write setCustomTypeName;
    property Color: TColor read fColor write setColor;
    property VarType: TVariableType read fVarType write setVarType;
    property Value: string read GetValue write SetValue;
    property HotkeyCount: integer read getHotkeyCount;
    property Hotkey[index: integer]: TMemoryRecordHotkey read getHotkey;

    property visible: boolean read fVisible write setVisible;
    property Count: integer read getChildCount;
    property Child[index: integer]: TMemoryRecord read getChild; default;
    property Parent: TMemoryRecord read getParent;

    property onActivate: TActivateEvent read fOnActivate write fOnActivate;
    property onDeactivate: TActivateEvent read fOnDeActivate write fOndeactivate;
    property onDestroy: TNotifyEvent read fOnDestroy write fOnDestroy;
  end;

  TMemoryRecordHotkey=class
  private
    fOnHotkey: TNotifyevent;
    fOnPostHotkey: TNotifyevent;
  public
    id: integer;
    description: string;
    keys: Tkeycombo;
    action: TMemrecHotkeyAction;
    value: string;
    owner: TMemoryRecord;
    procedure doHotkey;
    constructor create(AnOwner: TMemoryRecord);
    destructor destroy; override;
    property OnHotkey: TNotifyEvent read fOnHotkey write fOnHotkey;
    property OnPostHotkey: TNotifyEvent read fOnPostHotkey write fOnPostHotkey;
  end;

function MemRecHotkeyActionToText(action: TMemrecHotkeyAction): string;
function TextToMemRecHotkeyAction(text: string): TMemrecHotkeyAction;

implementation

uses mainunit, addresslist, formsettingsunit, LuaHandler;

{-----------------------------TMemoryRecordHotkey------------------------------}
constructor TMemoryRecordHotkey.create(AnOwner: TMemoryRecord);
begin
  //add to the hotkeylist
  id:=-1;
  owner:=AnOwner;
  owner.hotkeylist.Add(self);

  keys[0]:=0;

  RegisterHotKey2(mainform.handle, 0, keys, self);
end;

destructor TMemoryRecordHotkey.destroy;
begin
  UnregisterAddressHotkey(self);

  //remove this hotkey from the memoryrecord
  if owner<>nil then
    owner.hotkeylist.Remove(self);
end;

procedure TMemoryRecordHotkey.doHotkey;
begin
  if assigned(fonhotkey) then
    fOnHotkey(self);

  if owner<>nil then //just be safe (e.g other app sending message)
    owner.DoHotkey(self);

  if assigned(fonPostHotkey) then
    fOnPostHotkey(self);
end;

{---------------------------------MemoryRecord---------------------------------}

function TMemoryRecord.getChildCount: integer;
begin
  result:=0;
  if treenode<>nil then
    result:=treenode.Count;
end;

function TMemoryRecord.getChild(index: integer): TMemoryRecord;
begin
  if index<Count then
    result:=TMemoryRecord(treenode.Items[index].Data)
  else
    result:=nil;
end;

function TMemoryRecord.getHotkeyCount: integer;
begin
  result:=hotkeylist.count;
end;

function TMemoryRecord.getHotkey(index: integer): TMemoryRecordHotkey;
begin
  result:=nil;

  if index<hotkeylist.count then
    result:=TMemoryRecordHotkey(hotkeylist[index]);
end;

constructor TMemoryRecord.create(AOwner: TObject);
begin
  fVisible:=true;
  fid:=-1;
  fOwner:=AOwner;
  fColor:=clWindowText;

  hotkeylist:=tlist.create;

  foptions:=[];

  inherited create;
end;

destructor TMemoryRecord.destroy;
var i: integer;
begin
  if assigned(fOnDestroy) then
    fOnDestroy(self);

  //unregister hotkeys
  if hotkeylist<>nil then
  begin
    while hotkeylist.count>0 do
      TMemoryRecordHotkey(hotkeylist[0]).free;

    hotkeylist.free;
  end;

  //free script space
  if autoassemblerdata.script<>nil then
    autoassemblerdata.script.free;

  //free script info
  if autoassemblerdata.registeredsymbols<>nil then
    autoassemblerdata.registeredsymbols.free;

  //free the group's children
  while (treenode.count>0) do
    TMemoryRecord(treenode[0].data).free;


  if treenode<>nil then
    treenode.free;

  inherited Destroy;

end;



procedure TMemoryRecord.SetVisibleChildrenState;
{Called when options change and when children are assigned}
begin
  if (not factive) and (moHideChildren in foptions) then
    treenode.Collapse(true)
  else
    treenode.Expand(true);
end;

procedure TMemoryRecord.setOptions(newOptions: TMemrecOptions);
begin
  foptions:=newOptions;
  //apply changes (moHideChildren, moBindActivation, moRecursiveSetValue)
  SetVisibleChildrenState;
end;

procedure TMemoryRecord.setCustomTypeName(name: string);
begin
  fCustomTypeName:=name;
  RefreshCustomType;
end;

procedure TMemoryRecord.setVarType(v:  TVariableType);
begin
  //setup some of the default settings
  case v of
    vtUnicodeString: //this type was added later. convert it to a string
    begin
      fvartype:=vtString;
      extra.stringData.unicode:=true;
      extra.stringData.ZeroTerminate:=true;
    end;

    vtPointer:  //also added later. In this case show as a hex value
    begin
      if processhandler.is64bit then
        fvartype:=vtQword
      else
        fvartype:=vtDword;

      showAsHex:=true;
    end;


    vtString: //if setting to the type of string enable the zero terminate method by default
      extra.stringData.ZeroTerminate:=true;

    vtAutoAssembler:
      if AutoAssemblerData.script=nil then
        AutoAssemblerData.script:=tstringlist.create;
  end;



  fVarType:=v;
end;

procedure TMemoryRecord.setColor(c: TColor);
begin
  fColor:=c;
  TAddresslist(fOwner).Update;
end;

procedure TMemoryRecord.setXMLnode(CheatEntry: TDOMNode);
var
  tempnode,tempnode2: TDOMNode;
  i,j,k,l: integer;

  currentEntry: TDOMNode;

  hk: TMemoryRecordHotkey;
  memrec: TMemoryRecord;
  a:TDOMNode;
begin
  if TDOMElement(CheatEntry).TagName<>'CheatEntry' then exit; //invalid node type

  tempnode:=Cheatentry.FindNode('ID');
  if tempnode<>nil then
    id:=strtoint(tempnode.textcontent);

  tempnode:=CheatEntry.FindNode('Description');
  if tempnode<>nil then
    Description:=ansitoutf8(tempnode.TextContent);

  if (description<>'') and ((description[1]='"') and (description[length(description)]='"')) then
    description:=copy(description,2,length(description)-2);


  tempnode:=CheatEntry.FindNode('Options');
  if tempnode<>nil then
  begin
    if tempnode.HasAttributes then
    begin
      a:=tempnode.Attributes.GetNamedItem('moHideChildren');
      if (a<>nil) and (a.TextContent='1') then
          foptions:=foptions+[moHideChildren];

      a:=tempnode.Attributes.GetNamedItem('moBindActivation');
      if (a<>nil) and (a.TextContent='1') then
        foptions:=foptions+[moBindActivation];

      a:=tempnode.Attributes.GetNamedItem('moRecursiveSetValue');
      if (a<>nil) and (a.TextContent='1') then
        foptions:=foptions+[moRecursiveSetValue];
    end;
  end;


  tempnode:=CheatEntry.FindNode('ShowAsHex');
  if tempnode<>nil then
    fshowashex:=tempnode.textcontent='1';

  tempnode:=CheatEntry.FindNode('ShowAsSigned');
  if tempnode<>nil then
  begin
    fShowAsSignedOverride:=true;
    fShowAsSigned:=tempnode.textcontent='1';
  end;


  tempnode:=CheatEntry.FindNode('Color');
  if tempnode<>nil then
  begin
    try
      fColor:=strtoint('$'+tempnode.textcontent);
    except
    end;
  end;

  tempnode:=CheatEntry.FindNode('GroupHeader');
  if tempnode<>nil then
  begin
    isGroupHeader:=tempnode.TextContent='1';
  end;


  tempnode:=CheatEntry.FindNode('CheatEntries');
  if tempnode<>nil then
  begin
    currentEntry:=tempnode.FirstChild;
    while currentEntry<>nil do
    begin
      //create a blank entry
      memrec:=TMemoryRecord.create(fOwner);

      memrec.treenode:=treenode.owner.AddObject(nil,'',memrec);
      memrec.treenode.MoveTo(treenode, naAddChild); //make it the last child of this node

      //fill the entry with the node info
      memrec.setXMLnode(currentEntry);
      currentEntry:=currentEntry.NextSibling;
    end;

  end;

  treenode.Expand(true);



  begin
    tempnode:=CheatEntry.FindNode('VariableType');
    if tempnode<>nil then
      VarType:=StringToVariableType(tempnode.TextContent);

    case VarType of
      vtCustom:
      begin
        tempnode:=CheatEntry.FindNode('CustomType');
        if tempnode<>nil then
          setCustomTypeName(tempnode.TextContent);
      end;

      vtBinary:
      begin
        tempnode:=CheatEntry.FindNode('BitStart');
        if tempnode<>nil then
          extra.bitData.Bit:=strtoint(tempnode.TextContent);

        tempnode:=CheatEntry.FindNode('BitLength');
        if tempnode<>nil then
          extra.bitData.bitlength:=strtoint(tempnode.TextContent);

        tempnode:=CheatEntry.FindNode('ShowAsBinary');
        if tempnode<>nil then
          extra.bitData.ShowAsBinary:=tempnode.TextContent='1';
      end;

      vtString:
      begin
        tempnode:=CheatEntry.FindNode('Length');
        if tempnode<>nil then
          extra.stringData.length:=strtoint(tempnode.TextContent);

        tempnode:=CheatEntry.FindNode('Unicode');
        if tempnode<>nil then
          extra.stringData.Unicode:=tempnode.TextContent='1';

        tempnode:=CheatEntry.FindNode('ZeroTerminate');
        if tempnode<>nil then
          extra.stringdata.ZeroTerminate:=tempnode.TextContent='1';
      end;

      vtByteArray:
      begin
        tempnode:=CheatEntry.FindNode('ByteLength');
        if tempnode<>nil then
          extra.byteData.bytelength:=strtoint(tempnode.TextContent);
      end;

      vtAutoAssembler:
      begin
        tempnode:=Cheatentry.FindNode('AssemblerScript');

        if tempnode<>nil then
        begin
          if AutoAssemblerData.script<>nil then
            freeAndNil(AutoAssemblerData.script);

          setlength(AutoAssemblerData.allocs,0);
          if AutoAssemblerData.registeredsymbols<>nil then
            freeandnil(AutoAssemblerData.registeredsymbols);

          AutoAssemblerData.script:=tstringlist.Create;
          AutoAssemblerData.script.text:=tempnode.TextContent;

        end;
      end;

    end;

    tempnode:=CheatEntry.FindNode('Address');
    if tempnode<>nil then
      interpretableaddress:=tempnode.TextContent;


    tempnode:=CheatEntry.FindNode('Offsets');
    if tempnode<>nil then
    begin
      setlength(pointeroffsets,tempnode.ChildNodes.Count);
      j:=0;
      for i:=0 to tempnode.ChildNodes.Count-1 do
      begin

        if tempnode.ChildNodes[i].NodeName='Offset' then
        begin
          pointeroffsets[j]:=strtoint('$'+tempnode.ChildNodes[i].TextContent);
          inc(j);
        end;
      end;

      setlength(pointeroffsets,j); //set to the proper size
    end;

    tempnode:=CheatEntry.FindNode('Hotkeys');

    if tempnode<>nil then
    begin
      while hotkeycount>0 do //erase the old hotkey list
        hotkey[0].free;


      for i:=0 to tempnode.ChildNodes.count-1 do
      begin
        hk:=TMemoryRecordHotkey.Create(self);

        if tempnode.ChildNodes[i].NodeName='Hotkey' then
        begin
          hk.value:='';
          ZeroMemory(@hk.keys,sizeof(TKeyCombo));

          tempnode2:=tempnode.childnodes[i].FindNode('Description');
          if tempnode2<>nil then
            hk.description:=tempnode2.textcontent;

          tempnode2:=tempnode.childnodes[i].FindNode('ID');

          if tempnode2<>nil then
            hk.id:=strtoint(tempnode2.textcontent);


          tempnode2:=tempnode.childnodes[i].FindNode('Action');
          if tempnode2<>nil then
            hk.action:=TextToMemRecHotkeyAction(tempnode2.TextContent);

          tempnode2:=tempnode.childnodes[i].findnode('Value');
          if tempnode2<>nil then
            hk.value:=tempnode2.TextContent;

          tempnode2:=tempnode.ChildNodes[i].FindNode('Keys');
          if tempnode2<>nil then
          begin
            l:=0;
            for k:=0 to tempnode2.ChildNodes.Count-1 do
            begin
              if tempnode2.ChildNodes[k].NodeName='Key' then
              begin
                try
                  hk.keys[l]:=StrToInt(tempnode2.ChildNodes[k].TextContent);
                  inc(l);
                except
                end;
              end;
            end;

          end;
        end;
      end;

      //check if a hotkey has an id, and if not create one for it
      for i:=0 to HotkeyCount-1 do
        if hotkey[i].id=-1 then
          hotkey[i].id:=getuniquehotkeyid;
    end;
    ReinterpretAddress;
    refresh;
  end;


  SetVisibleChildrenState;


end;

function TMemoryRecord.getParent: TMemoryRecord;
var tn: TTreenode;
begin
  result:=nil;
  tn:=treenode.parent;
  if tn<>nil then
    result:=TMemoryRecord(tn.data);
end;

function TMemoryRecord.hasParent: boolean;
begin
  result:=treenode.parent<>nil;
end;

function TMemoryRecord.hasSelectedParent: boolean;
var tn: TTreenode;
  m: TMemoryRecord;
begin
  //if this node has a direct parent that is selected it returns true, else it will ask the parent if that one has a selected parent etc... untill there is no more parent, or one is selected
  result:=false;
  tn:=treenode.Parent;
  if tn<>nil then
  begin
    m:=TMemoryRecord(tn.data);
    if m.isSelected then
      result:=true
    else
      result:=m.hasSelectedParent;
  end;
end;

procedure TMemoryRecord.getXMLNode(node: TDOMNode; selectedOnly: boolean);
var
  doc: TDOMDocument;
  cheatEntry: TDOMNode;
  cheatEntries: TDOMNode;
  offsets: TDOMNode;
  hks, hk,hkkc: TDOMNode;
  opt: TDOMNode;

  tn: TTreenode;
  i,j: integer;
  a:TDOMAttr;

  s: ansistring;
begin
  if selectedonly then
  begin
    if (not isselected) then exit; //don't add if not selected and only the selected items should be added

    //it is selected, check if it has a parent that is selected, if not, continue, else exit
    if hasSelectedParent then exit;
  end;


  doc:=node.OwnerDocument;
  cheatEntry:=doc.CreateElement('CheatEntry');
  cheatEntry.AppendChild(doc.CreateElement('ID')).TextContent:=IntToStr(ID);



  s:=utf8tosys(description);
  cheatEntry.AppendChild(doc.CreateElement('Description')).TextContent:='"'+s+'"';

  //save options
  //(moHideChildren, moBindActivation, moRecursiveSetValue);
  if options<>[] then
  begin
    opt:=cheatEntry.AppendChild(doc.CreateElement('Options'));

    if moHideChildren in options then
    begin
      a:=doc.CreateAttribute('moHideChildren');
      a.TextContent:='1';
      opt.Attributes.SetNamedItem(a);
    end;

    if moBindActivation in options then
    begin
      a:=doc.CreateAttribute('moBindActivation');
      a.TextContent:='1';
      opt.Attributes.SetNamedItem(a);
    end;

    if moRecursiveSetValue in options then
    begin
      a:=doc.CreateAttribute('moRecursiveSetValue');
      a.TextContent:='1';
      opt.Attributes.SetNamedItem(a);
    end;
  end;

  if showAsHex then
    cheatEntry.AppendChild(doc.CreateElement('ShowAsHex')).TextContent:='1';

  if fShowAsSignedOverride then
  begin
    if fShowAsSigned then
      cheatEntry.AppendChild(doc.CreateElement('ShowAsSigned')).TextContent:='1'
    else
      cheatEntry.AppendChild(doc.CreateElement('ShowAsSigned')).TextContent:='0';
  end;


  cheatEntry.AppendChild(doc.CreateElement('Color')).TextContent:=inttohex(fcolor,6);

  if isGroupHeader then
  begin
    cheatEntry.AppendChild(doc.CreateElement('GroupHeader')).TextContent:='1';
  end
  else
  begin
    cheatEntry.AppendChild(doc.CreateElement('VariableType')).TextContent:=VariableTypeToString(vartype);
    case VarType of
      vtCustom:
      begin
        cheatentry.AppendChild(doc.CreateElement('CustomType')).TextContent:=CustomTypeName;
      end;

      vtBinary:
      begin
        cheatEntry.AppendChild(doc.CreateElement('BitStart')).TextContent:=inttostr(extra.bitData.Bit);
        cheatEntry.AppendChild(doc.CreateElement('BitLength')).TextContent:=inttostr(extra.bitData.BitLength);
        cheatEntry.AppendChild(doc.CreateElement('ShowAsBinary')).TextContent:=BoolToStr(extra.bitData.showasbinary,'1','0');
      end;

      vtString:
      begin
        cheatEntry.AppendChild(doc.CreateElement('Length')).TextContent:=inttostr(extra.stringData.length);
        cheatEntry.AppendChild(doc.CreateElement('Unicode')).TextContent:=BoolToStr(extra.stringData.unicode,'1','0');
        cheatEntry.AppendChild(doc.CreateElement('ZeroTerminate')).TextContent:=BoolToStr(extra.stringData.ZeroTerminate,'1','0');

      end;

      vtByteArray:
      begin
        cheatEntry.AppendChild(doc.CreateElement('ByteLength')).TextContent:=inttostr(extra.byteData.bytelength);
      end;

      vtAutoAssembler:
      begin
        cheatEntry.AppendChild(doc.CreateElement('AssemblerScript')).TextContent:=AutoAssemblerData.script.Text;
      end;
    end;

    if VarType<>vtAutoAssembler then
    begin
      cheatEntry.AppendChild(doc.CreateElement('Address')).TextContent:=interpretableaddress;

      if isPointer then
      begin
        Offsets:=cheatEntry.AppendChild(doc.CreateElement('Offsets'));

        for i:=0 to length(pointeroffsets)-1 do
          Offsets.AppendChild(doc.CreateElement('Offset')).TextContent:=inttohex(pointeroffsets[i],1);

        cheatEntry.AppendChild(Offsets);
      end;
    end;

    //hotkeys

    if HotkeyCount>0 then
    begin
      hks:=cheatentry.AppendChild(doc.CreateElement('Hotkeys'));
      for i:=0 to HotkeyCount-1 do
      begin
        hk:=hks.AppendChild(doc.CreateElement('Hotkey'));
        hk.AppendChild(doc.CreateElement('Action')).TextContent:=MemRecHotkeyActionToText(hotkey[i].action);
        hkkc:=hk.AppendChild(doc.createElement('Keys'));
        j:=0;
        while (j<5) and (hotkey[i].keys[j]<>0) do
        begin
          hkkc.appendchild(doc.createElement('Key')).TextContent:=inttostr(hotkey[i].keys[j]);
          inc(j);
        end;

        if hotkey[i].value<>'' then
          hk.AppendChild(doc.CreateElement('Value')).TextContent:=hotkey[i].value;

        if hotkey[i].description<>'' then
          hk.AppendChild(doc.CreateElement('Description')).TextContent:=hotkey[i].description;

        if hotkey[i].id>=0 then
          hk.AppendChild(doc.CreateElement('ID')).TextContent:=inttostr(hotkey[i].id);
      end;

    end;
  end;

  //append the children if it has any
  if treenode.HasChildren then
  begin
    CheatEntries:=doc.CreateElement('CheatEntries');
    tn:=treenode.GetFirstChild;
    while tn<>nil do
    begin
      TMemoryRecord(tn.data).getXMLNode(CheatEntries, false); //take over ALL attached nodes, not just the selected ones
      tn:=tn.GetNextSibling;
    end;

    cheatentry.AppendChild(CheatEntries);
  end;


  node.AppendChild(cheatEntry);
end;

procedure TMemoryRecord.refresh;
begin
  treenode.Update;
end;


procedure TMemoryRecord.setShowAsSigned(state: boolean);
begin
  fShowAsSignedOverride:=true;
  fShowAsSigned:=state;
  refresh;
end;

function TMemoryRecord.GetShowAsSigned: boolean;
begin
  if fShowAsSignedOverride then
    result:=fShowAsSigned
  else
    result:=formSettings.cbShowAsSigned.checked;
end;

function TMemoryRecord.isBeingEdited: boolean;
begin
  result:=editcount>0;
end;

procedure TMemoryRecord.beginEdit;
begin
  inc(editcount);
end;

procedure TMemoryRecord.endEdit;
begin
  dec(editcount);
end;

function TMemoryRecord.isPointer: boolean;
begin
  result:=length(pointeroffsets)>0;
end;

function TMemoryRecord.isOffset: boolean;
begin
  result:=fIsOffset;
end;

function TMemoryRecord.hasHotkeys: boolean;
begin
  result:=HotkeyCount>0;
end;

function TMemoryRecord.removeHotkey(hk: TMemoryRecordHotkey): boolean;
begin
  hk.free;
  result:=true;
end;

procedure TMemoryRecord.setID(i: integer);
var a: TAddresslist;

begin
  if i<>fid then
  begin
    //new id, check fo duplicates (e.g copy/paste)
    a:=TAddresslist(fOwner);

    if a.getRecordWithID(i)<>nil then
      fid:=a.GetUniqueMemrecId
    else
      fid:=i;
  end;
end;

function TMemoryRecord.getuniquehotkeyid: integer;
//goes through the hotkeylist and returns an unused id
var i: integer;
  isunique: boolean;
begin
  result:=0;
  for result:=0 to maxint-1 do
  begin
    isunique:=true;
    for i:=0 to hotkeycount-1 do
      if hotkey[i].id=result then
      begin
        isunique:=false;
        break;
      end;

    if isunique then break;
  end;
end;

function TMemoryRecord.Addhotkey(keys: tkeycombo; action: TMemrecHotkeyAction; value, description: string): TMemoryRecordHotkey;
{
adds and registers a hotkey and returns the hotkey index for this hotkey
return -1 if failure
}
var
  hk: TMemoryRecordHotkey;
begin
  hk:=TMemoryRecordHotkey.create(self);

  hk.id:=getuniquehotkeyid;
  hk.keys:=keys;
  hk.action:=action;
  hk.value:=value;
  hk.description:=description;

  result:=hk;
end;

procedure TMemoryRecord.increaseValue(value: string);
var
  oldvalue: qword;
  oldvaluedouble: double;
  increasevalue: qword;
  increasevaluedouble: double;
begin
  if VarType in [vtByte, vtWord, vtDword, vtQword, vtSingle, vtDouble, vtCustom] then
  begin
    try
      if showAsHex then //seperate handler for hexadecimal. (handle as int, even for the float types)
      begin
        oldvalue:=StrToQWordEx('$'+getvalue);
        increasevalue:=StrToQwordEx('$'+value);
        setvalue(IntTohex(oldvalue+increasevalue,1));
      end
      else
      begin
        if VarType in [vtByte, vtWord, vtDword, vtQword, vtCustom] then
        begin
          oldvalue:=StrToQWordEx(getvalue);
          increasevalue:=StrToQWordEx(value);
          setvalue(IntToStr(oldvalue+increasevalue));
        end
        else
        begin
          oldvaluedouble:=StrToFloat(getValue);
          increasevalueDouble:=StrToFloat(value);
          setvalue(FloatToStr(oldvaluedouble+increasevalueDouble));
        end;
      end;
    except

    end;
  end;
end;

procedure TMemoryRecord.decreaseValue(value: string);
var
  oldvalue: qword;
  oldvaluedouble: double;
  decreasevalue: qword;
  decreasevaluedouble: double;
begin
  if VarType in [vtByte, vtWord, vtDword, vtQword, vtSingle, vtDouble] then
  begin
    try
      if VarType in [vtByte, vtWord, vtDword, vtQword] then
      begin
        oldvalue:=StrToQWordEx(getvalue);
        decreasevalue:=StrToQWordEx(value);
        setvalue(IntToStr(oldvalue-decreasevalue));
      end
      else
      begin
        oldvaluedouble:=StrToFloat(getValue);
        decreasevalueDouble:=StrToFloat(value);
        setvalue(FloatToStr(oldvaluedouble-decreasevalueDouble));
      end;
    except

    end;
  end;
end;

procedure TMemoryRecord.disablewithoutexecute;
begin
  factive:=false;
  SetVisibleChildrenState;
  treenode.Update;
end;

procedure TMemoryRecord.DoHotkey(hk: TMemoryRecordhotkey);
begin
  if (hk<>nil) and (hk.owner=self) then
  begin
    try
      case hk.action of
        mrhToggleActivation: active:=not active;
        mrhSetValue:         SetValue(hk.value);
        mrhIncreaseValue:    increaseValue(hk.value);
        mrhDecreaseValue:    decreaseValue(hk.value);


        mrhToggleActivationAllowDecrease:
        begin
          allowDecrease:=True;
          active:=true;
        end;

        mrhToggleActivationAllowIncrease:
        begin
          allowIncrease:=True;
          active:=true;
        end;


      end;
    except
      //don't complain about incorrect values
    end;
  end;

  treenode.update;
end;

procedure TMemoryRecord.setAllowDecrease(state: boolean);
begin
  fAllowDecrease:=state;
  if state then
    fAllowIncrease:=false; //at least one of the 2 must always be false

  treenode.update;
end;

procedure TMemoryRecord.setAllowIncrease(state: boolean);
begin
  fAllowIncrease:=state;
  if state then
    fAllowDecrease:=false; //at least one of the 2 must always be false

  treenode.update;
end;

procedure TMemoryRecord.setActive(state: boolean);
var f: string;
    i: integer;
begin
  //6.0 compatibility
  if state=fActive then exit; //no need to execute this is it's the same state

  if (state) then
    LUA_memrec_callback(self, '_memrec_'+description+'_activating')
  else
    LUA_memrec_callback(self, '_memrec_'+description+'_deactivating');

  //6.1+
  if state then
  begin
    //activating , before
    if assigned(fonactivate) then
      if not fonactivate(self, true, fActive) then exit; //do not activate if it returns false
  end
  else
  begin
    if assigned(fondeactivate) then
      if not fondeactivate(self, true, fActive) then exit;
  end;


  if not isGroupHeader then
  begin
    if self.VarType = vtAutoAssembler then
    begin
      //aa script
      try
        if autoassemblerdata.registeredsymbols=nil then
          autoassemblerdata.registeredsymbols:=tstringlist.create;

        if autoassemble(autoassemblerdata.script, false, state, false, false, autoassemblerdata.allocs, autoassemblerdata.registeredsymbols) then
        begin
          fActive:=state;
          if autoassemblerdata.registeredsymbols.Count>0 then //if it has a registered symbol then reinterpret all addresses
            TAddresslist(fOwner).ReinterpretAddresses;
        end;
      except
        //running the script failed, state unchanged
      end;

    end
    else
    begin
      //freeze/unfreeze
      if state then
      begin
        f:=GetValue;

        try
          SetValue(f);
        except
          fActive:=false;
          beep;
          exit;
        end;

        //still here so F is ok
        //enabled
        FrozenValue:=f;
      end;

      fActive:=state;
    end;

  end else fActive:=state;


  if state=false then
  begin
    //on disable or failure setting the state to true, also reset the option if it's allowed to increase/decrease
    allowDecrease:=false;
    allowIncrease:=false;
  end;
  treenode.update;

  if moBindActivation in options then
  begin
    //apply this state to all the children
    for i:=0 to treenode.Count-1 do
      TMemoryRecord(treenode[i].data).setActive(active);
  end;


  //6.0 compat
  if state then
    LUA_memrec_callback(self, '_memrec_'+description+'_activated')
  else
    LUA_memrec_callback(self, '_memrec_'+description+'_deactivated');


  //6.1+
  if state then
  begin
    //activating , before
    if assigned(fonactivate) then
      if not fonactivate(self, false, factive) then exit; //do not activate if it returns false
  end
  else
  begin
    if assigned(fondeactivate) then
      if not fondeactivate(self, false, factive) then exit;
  end;


  SetVisibleChildrenState;

end;

procedure TMemoryRecord.setVisible(state: boolean);
begin
  fVisible:=state;
  if treenode<>nil then
    treenode.update;
end;

procedure TMemoryRecord.setShowAsHex(state:boolean);
begin
  fShowAsHex:=state;
  if treenode<>nil then
    treenode.Update;
end;

function TMemoryRecord.getByteSize: integer;
begin
  result:=0;
  case VarType of
    vtByte: result:=1;
    vtWord: result:=2;
    vtDWord: result:=4;
    vtSingle: result:=4;
    vtDouble: result:=8;
    vtQword: result:=8;
    vtString:
    begin
      result:=Extra.stringData.length;
      if extra.stringData.unicode then result:=result*2;
    end;

    vtByteArray: result:=extra.byteData.bytelength;
    vtBinary: result:=1+(extra.bitData.Bit+extra.bitData.bitlength div 8);
    vtCustom:
    begin
      if customtype<>nil then
        result:=customtype.bytesize;
    end;
  end;
end;

procedure TMemoryRecord.RefreshCustomType;
begin
  if vartype=vtCustom then
    CustomType:=GetCustomTypeFromName(fCustomTypeName);
end;

function TMemoryRecord.ReinterpretAddress(forceremovalofoldaddress: boolean=false): boolean;
//Returns false if interpretation failed (not really used for anything right now)
var
  a: ptrUint;
  s: string;
  i: integer;
begin
  if forceremovalofoldaddress then
  begin
    RealAddress:=0;
    baseaddress:=0;
  end;

  a:=symhandler.getAddressFromName(interpretableaddress,false,couldnotinterpretaddress);
  result:=not couldnotinterpretaddress;

  if result then
  begin

    s:=trim(interpretableaddress);
    fIsOffset:=(s<>'') and (s[1] in ['+','-']);
    baseaddress:=a;
  end;


  //update the children
  for i:=0 to count-1 do
    Child[i].ReinterpretAddress(forceremovalofoldaddress);
end;

procedure TMemoryRecord.ApplyFreeze;
var oldvalue, newvalue: string;
  olddecimalvalue, newdecimalvalue: qword;
  oldfloatvalue, newfloatvalue: double;
begin
  if (not isgroupheader) and active and (VarType<>vtAutoAssembler) then
  begin
    try

      if allowIncrease or allowDecrease then
      begin
        //get the new value
        oldvalue:=frozenValue;
        newvalue:=GetValue;
        if showashex or (VarType in [vtByte..vtQword, vtCustom]) then
        begin
          //handle as a decimal


          if showAsHex then
          begin
            newdecimalvalue:=StrToInt('$'+newvalue);
            olddecimalvalue:=StrToInt('$'+oldvalue);
          end
          else
          begin
            newdecimalvalue:=StrToInt(newvalue);
            olddecimalvalue:=StrToInt(oldvalue);
          end;

          if (allowIncrease and (newdecimalvalue>olddecimalvalue)) or
             (allowDecrease and (newdecimalvalue<olddecimalvalue))
          then
            frozenvalue:=newvalue;

        end
        else
        if Vartype in [vtSingle, vtdouble] then
        begin
          //handle as floating point value
          oldfloatvalue:=strtofloat(oldvalue);
          newfloatvalue:=strtofloat(newvalue);

          if (allowIncrease and (newfloatvalue>oldfloatvalue)) or
             (allowDecrease and (newfloatvalue<oldfloatvalue))
          then
            frozenvalue:=newvalue;

        end;

        try
          setValue(frozenValue, true);
        except
          //new value gives an error, use the old one
          frozenvalue:=oldvalue;
        end;
      end
      else
        setValue(frozenValue, true);
    except
    end;
  end;
end;

function TMemoryRecord.getAddressString: string;
begin
  GetRealAddress;

  if length(pointeroffsets)>0 then
  begin
    if UnreadablePointer then
      result:='P->????????'
    else
      result:='P->'+inttohex(realaddress,8);
  end else
  begin
    if (realaddress=0) and (couldnotinterpretaddress) then
      result:='('+interpretableaddress+')'
    else
      result:=inttohex(realaddress,8);
  end;
end;

function TMemoryRecord.BinaryToString(b: pbytearray; bufsize: integer): string;
{Seperate function for the binary value since it's a bit more complex}
var
  temp,mask: qword;
begin
  temp:=0; //initialize

  if bufsize>8 then bufsize:=8;

  CopyMemory(@temp,b,bufsize);

  temp:=temp shr extra.bitData.Bit; //shift to the proper start
  mask:=qword($ffffffffffffffff) shl extra.bitData.bitlength; //create a mask that stripps of the excessive bits

  temp:=temp and (not mask); //temp now only contains the bits that are of meaning


  if not extra.bitData.showasbinary then
    result:=inttostr(temp)
  else
    result:=IntToBin(temp);
end;

function TMemoryRecord.GetValue: string;
var
  br: dword;
  bufsize: integer;
  buf: pointer;
  pb: pbyte absolute buf;
  pba: pbytearray absolute buf;
  pw: pword absolute buf;
  pdw: pdword absolute buf;
  ps: psingle absolute buf;
  pd: pdouble absolute buf;
  pqw: PQWord absolute buf;

  wc: PWideChar absolute buf;
  c: PChar absolute buf;

  i: integer;
begin
  result:='';
  if isGroupHeader then exit;

  bufsize:=getbytesize;
  if bufsize=0 then exit;

  if vartype=vtString then
  begin
    inc(bufsize);
    if Extra.stringData.unicode then
      inc(bufsize);
  end;

  getmem(buf,bufsize);



  GetRealAddress;

  if ReadProcessMemory(processhandle, pointer(realAddress), buf, bufsize,br) then
  begin
    case vartype of
      vtCustom:
      begin
        if customtype<>nil then
        begin
          if customtype.scriptUsesFloat then
            result:=FloatToStr(customtype.ConvertDataToFloat(buf))
          else
            if showashex then result:=inttohex(customtype.ConvertDataToInteger(buf),8) else if showassigned then result:=inttostr(integer(customtype.ConvertDataToInteger(buf))) else result:=inttostr(customtype.ConvertDataToInteger(buf));
        end
        else
          result:='error';
      end;

      vtByte : if showashex then result:=inttohex(pb^,2) else if showassigned then result:=inttostr(shortint(pb^)) else result:=inttostr(pb^);
      vtWord : if showashex then result:=inttohex(pw^,4) else if showassigned then result:=inttostr(SmallInt(pw^)) else result:=inttostr(pw^);
      vtDWord: if showashex then result:=inttohex(pdw^,8) else if showassigned then result:=inttostr(Integer(pdw^)) else result:=inttostr(pdw^);
      vtQWord: if showashex then result:=inttohex(pqw^,16) else if showassigned then result:=inttostr(Int64(pqw^)) else result:=inttostr(pqw^);
      vtSingle: if showashex then result:=inttohex(pdw^,8) else result:=FloatToStr(ps^);
      vtDouble: if showashex then result:=inttohex(pqw^,16) else result:=FloatToStr(pd^);
      vtBinary: result:=BinaryToString(buf,bufsize);

      vtString:
      begin
        pba[bufsize-1]:=0;
        if Extra.stringData.unicode then
        begin
          pba[bufsize-2]:=0;
          result:={ansitoutf8}(wc);
        end
        else
          result:={ansitoutf8}(c);
      end;

      vtByteArray:
      begin
        for i:=0 to bufsize-1 do
          if showashex then
            result:=result+inttohex(pba[i],2)+' '
          else
            result:=result+inttostr(pba[i])+' ';

        if result<>'' then
          result:=copy(result,1,length(result)-1); //cut off the last space
      end;


    end;
  end
  else
    result:='??';

  freemem(buf);
end;

function TMemoryrecord.canUndo: boolean;
begin
  result:=undovalue<>'';
end;

procedure TMemoryRecord.UndoSetValue;
begin
  if canUndo then
  begin
    try
      setvalue(UndoValue, false);
    except
    end;
  end;
end;

procedure TMemoryRecord.SetValue(v: string);
begin
  SetValue(v,false);
end;

procedure TMemoryRecord.SetValue(v: string; isFreezer: boolean);
{
Changes this address to the value V
}
var
  buf: pointer;
  bufsize: integer;
  x: dword;
  i: integer;
  pb: pbyte absolute buf;
  pba: pbytearray absolute buf;
  pw: pword absolute buf;
  pdw: pdword absolute buf;
  ps: psingle absolute buf;
  pd: pdouble absolute buf;
  pqw: PQWord absolute buf;

  li: PLongInt absolute buf;
  li64: PQWord absolute buf;

  wc: PWideChar absolute buf;
  c: PChar absolute buf;
  originalprotection: dword;

  bts: TBytes;
  mask: qword;
  temp: qword;
  temps: string;

  tempsw: widestring;
  tempsa: ansistring;

  mr: TMemoryRecord;

  unparsedvalue: string;
  check: boolean;
  fs: TFormatSettings;
begin
  //check if it is a '(description)' notation

  unparsedvalue:=v;

  if vartype<>vtString then
  begin
    v:=trim(v);

    if (length(v)>2) and (v[1]='(') and (v[length(v)]=')') then
    begin
      //yes, it's a (description)
      temps:=copy(v, 2,length(v)-2);
      //search the addresslist for a entry with name (temps)

      mr:=TAddresslist(fOwner).getRecordWithDescription(temps);
      if mr<>nil then
        v:=mr.GetValue;

    end;
  end;

  if (not isfreezer) then
    undovalue:=GetValue;


  if (not isfreezer) and (moRecursiveSetValue in options) then //do this for all it's children
  begin
    for i:=0 to treenode.Count-1 do
    begin
      try
        TMemoryRecord(treenode[i].data).SetValue(v);
      except
        //some won't take the value, like 12.1112 on a 4 byte value, so just skip that error
      end;
    end;
  end;

  //and now set it for myself


  realAddress:=GetRealAddress; //quick update

  currentValue:={utf8toansi}(v);

  if fShowAsHex and (not (vartype in [vtSingle, vtDouble, vtByteArray, vtString] )) then
  begin
    currentvalue:=trim(currentValue);
    if length(currentvalue)>1 then
    begin
      if currentvalue[1]='-' then
      begin
        currentvalue:='-$'+copy(currentvalue,2,length(currentvalue));
      end
      else
        currentvalue:='$'+currentvalue;
    end;
  end;


  bufsize:=getbytesize;

  if (vartype=vtbinary) and (bufsize=3) then bufsize:=4;
  if (vartype=vtbinary) and (bufsize>4) then bufsize:=8;

  getmem(buf,bufsize);



  VirtualProtectEx(processhandle, pointer(realAddress), bufsize, PAGE_EXECUTE_READWRITE, originalprotection);
  try
    check:=ReadProcessMemory(processhandle, pointer(realAddress), buf, bufsize,x);
    if vartype in [vtBinary, vtByteArray] then //fill the buffer with the original byte
      if not check then exit;

    case VarType of
      vtCustom:
      begin
        if customtype<>nil then
        Begin
          if customtype.scriptUsesFloat then
            customtype.ConvertFloatToData(strtofloat(currentValue), ps)
          else
            customtype.ConvertIntegerToData(strtoint(currentValue), pdw);

        end;
      end;
      vtByte: pb^:=StrToQWordEx(currentValue);
      vtWord: pw^:=StrToQWordEx(currentValue);
      vtDword: pdw^:=StrToQWordEx(currentValue);
      vtQword: pqw^:=StrToQWordEx(currentValue);
      vtSingle: if (not fShowAsHex) or (not TryStrToInt('$'+currentvalue, li^)) then
      begin
        try
          fs:=DefaultFormatSettings;
          ps^:=StrToFloat(currentValue, fs);
        except
          if fs.DecimalSeparator='.' then
            fs.DecimalSeparator:=','
          else
          fs.DecimalSeparator:='.';

          ps^:=StrToFloat(currentValue, fs);
        end;
      end;

      vtDouble: if (not fShowAsHex) or (not TryStrToQWord('$'+currentvalue, li64^)) then
      begin
        try
          fs:=DefaultFormatSettings;
          pd^:=StrToFloat(currentValue, fs);
        except
          if fs.DecimalSeparator='.' then
            fs.DecimalSeparator:=','
          else
          fs.DecimalSeparator:='.';

          pd^:=StrToFloat(currentValue, fs);
        end;
      end;

      vtBinary:
      begin
        if not Extra.bitData.showasbinary then
          temps:=currentValue
        else
          temps:=IntToStr(BinToInt(currentValue));

        temp:=StrToQWordEx(temps);
        temp:=temp shl extra.bitData.Bit;
        mask:=qword($ffffffffffffffff) shl extra.bitData.BitLength;
        mask:=not mask; //mask now contains the length of the bits (4 bits would be 0001111)


        mask:=mask shl extra.bitData.Bit; //shift the mask to the proper start position
        temp:=temp and mask; //cut off extra bits

        case bufsize of
          1: pb^:=(pb^ and (not mask)) or temp;
          2: pw^:=(pw^ and (not mask)) or temp;
          4: pdw^:=(pdw^ and (not mask)) or temp;
          8: pqw^:=(pqw^ and (not mask)) or temp;
        end;
      end;

      vtString:
      begin
        //x contains the max length in characters for the string
        if extra.stringData.length<length(currentValue) then
        begin
          extra.stringData.length:=length(currentValue);
          freemem(buf);
          bufsize:=getbytesize;
          getmem(buf, bufsize);
        end;

        x:=bufsize;
        if extra.stringData.unicode then
          x:=bufsize div 2; //each character is 2 bytes so only half the size is available

        if Extra.stringData.ZeroTerminate then
          x:=min(length(currentValue)+1,x) //also copy the zero terminator
        else
          x:=min(length(currentValue),x);


        tempsw:=currentvalue;
        tempsa:=currentvalue;

        //copy the string to the buffer
        for i:=0 to x-1 do
        begin
          if extra.stringData.unicode then
          begin
            wc[i]:=pwidechar(tempsw)[i];
          end
          else
          begin
            c[i]:=pchar(tempsa)[i];
          end;
        end;

        if extra.stringData.unicode then
          bufsize:=x*2 //two times the number of characters
        else
          bufsize:=x;
      end;

      vtByteArray:
      begin
        ConvertStringToBytes(currentValue, showAsHex, bts);
        if length(bts)>bufsize then
        begin
          //the user wants to input more bytes than it should have
          Extra.byteData.bytelength:=length(bts);  //so next time this won't happen again
          bufsize:=length(bts);
          freemem(buf);
          getmem(buf,bufsize);
          if not ReadProcessMemory(processhandle, pointer(realAddress), buf, bufsize,x) then exit;
        end;


        bufsize:=min(length(bts),bufsize);
        for i:=0 to bufsize-1 do
          if bts[i]<>-1 then
            pba[i]:=bts[i];
      end;
    end;

    WriteProcessMemory(processhandle, pointer(realAddress), buf, bufsize, x);


  finally
    VirtualProtectEx(processhandle, pointer(realAddress), bufsize, originalprotection, originalprotection);
  end;

  freemem(buf);

  frozenValue:=unparsedvalue;     //we got till the end, so update the frozen value
end;

function TMemoryRecord.getBaseAddress: ptrUint;
begin
  if fIsOffset and hasParent then
    result:=parent.RealAddress+baseaddress //assuming that the parent has had it's real address calculated first
  else
    result:=BaseAddress;
end;

function TMemoryRecord.GetRealAddress: PtrUInt;
var
  check: boolean;
  realaddress, realaddress2: PtrUInt;
  i: integer;
  count: dword;
begin
  realAddress:=0;
  realAddress2:=0;

  if length(pointeroffsets)>0 then //it's a pointer
  begin
    //find the address this pointer points to
    result:=getPointerAddress(getBaseAddress, pointeroffsets, UnreadablePointer);
    if UnreadablePointer then
    begin
      realAddress:=0;
      result:=0;
    end;
  end
  else
    result:=getBaseAddress; //not a pointer

  self.RealAddress:=result;
end;



function MemRecHotkeyActionToText(action: TMemrecHotkeyAction): string;
begin
  //type TMemrecHotkeyAction=(mrhToggleActivation, mrhToggleActivationAllowIncrease, mrhToggleActivationAllowDecrease, mrhSetValue,
  //mrhIncreaseValue, mrhDecreaseValue);
  case action of
    mrhToggleActivation: result:='Toggle Activation';
    mrhToggleActivationAllowIncrease: result:='Toggle Activation Allow Increase';
    mrhToggleActivationAllowDecrease: result:='Toggle Activation Allow Decrease';
    mrhSetValue: result:='Set Value';
    mrhIncreaseValue: result:='Increase Value';
    mrhDecreaseValue: result:='Decrease Value';
  end;
end;

function TextToMemRecHotkeyAction(text: string): TMemrecHotkeyAction;
begin
  if text = 'Toggle Activation' then result:=mrhToggleActivation else
  if text = 'Toggle Activation Allow Increase' then result:=mrhToggleActivationAllowIncrease else
  if text = 'Toggle Activation Allow Decrease' then result:=mrhToggleActivationAllowDecrease else
  if text = 'Set Value' then result:=mrhSetValue else
  if text = 'Increase Value' then result:=mrhIncreaseValue else
  if text = 'Decrease Value' then result:=mrhDecreaseValue
  else
    result:=mrhToggleActivation;
end;

end.

