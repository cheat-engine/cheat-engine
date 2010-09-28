unit MemoryRecordUnit;

{$mode DELPHI}

interface

uses
  Windows, forms, graphics, Classes, SysUtils, controls, stdctrls, comctrls,symbolhandler,
  cefuncproc,newkernelhandler, autoassembler, hotkeyhandler, dom, XMLRead,XMLWrite,
  customtypehandler;

type TMemrecHotkeyAction=(mrhToggleActivation, mrhToggleActivationAllowIncrease, mrhToggleActivationAllowDecrease, mrhSetValue, mrhIncreaseValue, mrhDecreaseValue);

type TFreezeType=(ftFrozen, ftAllowIncrease, ftAllowDecrease);

type TMemrecOption=(moHideChildren, moBindActivation, moRecursiveSetValue);
type TMemrecOptions=set of TMemrecOption;

type TMemrecStringData=record
  unicode: boolean;
  length: integer;
end;

type TMemRecBitData=record
      Bit     : Byte;
      bitlength: integer;
      showasbinary: boolean;
    end;

type TMemRecByteData=record
      ShowAsHexadecimal: boolean;
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
  TMemoryRecord=class
  private
    FrozenValue : string;
    CurrentValue: string;
    UnreadablePointer: boolean;
    BaseAddress: ptrUint;
    RealAddress: ptrUint;

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

    function getByteSize: integer;
    function BinaryToString(b: pbytearray; bufsize: integer): string;
    function getAddressString: string;
    procedure setActive(state: boolean);
    procedure setAllowDecrease(state: boolean);
    procedure setAllowIncrease(state: boolean);
    procedure setShowAsHex(state: boolean);
    procedure setOptions(newOptions: TMemrecOptions);
    procedure setCustomTypeName(name: string);
    procedure setColor(c: TColor);
  public
    isGroupHeader: Boolean; //set if it's a groupheader, only the description matters then


    Description : string;
    interpretableaddress: string;


    pointeroffsets: array of dword; //if set this is an pointer

    VarType : TVariableType;

    Extra: TMemRecExtraData;
    AutoAssemblerData: TMemRecAutoAssemblerData;

    Hotkeys: array of record
      active: boolean; //seperate active boolean so the index doesn't shift when it's disabled
      keys: Tkeycombo;
      action: TMemrecHotkeyAction;
      value: string;
    end;

    treenode: TTreenode;

    isSelected: boolean; //lazarus bypass. Because lazarus does not implement multiselect I have to keep track of which entries are selected
    showAsSigned: boolean;
    showAsHex: boolean;

    //free for editing by user:
    autoAssembleWindow: TForm; //window storage for an auto assembler editor window


    function isBeingEdited: boolean;
    procedure beginEdit;
    procedure endEdit;

    function isPointer: boolean;
    procedure ApplyFreeze;
    function GetValue: string;
    procedure SetValue(v: string);
    procedure increaseValue(value: string);
    procedure decreaseValue(value: string);
    function GetRealAddress: PtrUInt;
    function getBaseAddress: ptrUint; //if it's a pointer, return the base address
    procedure RefreshCustomType;
    procedure ReinterpretAddress;
    property Value: string read GetValue write SetValue;
    property bytesize: integer read getByteSize;

    function Addhotkey(windowhandle: THandle; keys: tkeycombo; action: TMemrecHotkeyAction; value: string): integer;
    function removeHotkey(i: integer): boolean;

    procedure DoHotkey(i: integer); //execute the specific hotkey action

    procedure disablewithoutexecute;
    procedure refresh;

    procedure getXMLNode(node: TDOMNode; selectedOnly: boolean);
    procedure setXMLnode(CheatEntry: TDOMNode);

    procedure SetVisibleChildrenState;

    constructor Create(AOwner: TObject);
    destructor destroy; override;



    property addressString: string read getAddressString;
    property active: boolean read fActive write setActive;

    property allowDecrease: boolean read fallowDecrease write setAllowDecrease;
    property allowIncrease: boolean read fallowIncrease write setAllowIncrease;
    property showAsHex: boolean read fShowAsHex write setShowAsHex;
    property options: TMemrecOptions read fOptions write setOptions;
    property CustomTypeName: string read fCustomTypeName write setCustomTypeName;
    property Color: TColor read fColor write setColor;
  end;

function MemRecHotkeyActionToText(action: TMemrecHotkeyAction): string;
function TextToMemRecHotkeyAction(text: string): TMemrecHotkeyAction;

implementation

uses mainunit, addresslist, formsettingsunit;

constructor TMemoryRecord.create(AOwner: TObject);
begin
  fOwner:=AOwner;
  fColor:=clWindowText;
  inherited create;
end;

destructor TMemoryRecord.destroy;
var i: integer;
begin
  //unregister hotkeys
  for i:=0 to length(hotkeys)-1 do
    removehotkey(i);

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
end;



procedure TMemoryRecord.SetVisibleChildrenState;
{Called when options change and when children are assigned}
begin
  if moHideChildren in foptions then
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

  memrec: TMemoryRecord;
  a:TDOMNode;
begin
  if TDOMElement(CheatEntry).TagName<>'CheatEntry' then exit; //invalid node type

  tempnode:=CheatEntry.FindNode('Description');
  if tempnode<>nil then
    Description:=tempnode.TextContent;

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
      end;

      vtByteArray:
      begin
        tempnode:=CheatEntry.FindNode('ByteLength');
        if tempnode<>nil then
          extra.byteData.bytelength:=strtoint(tempnode.TextContent);

        tempnode:=CheatEntry.FindNode('ShowAsHexadecimal');
        if tempnode<>nil then
          extra.byteData.ShowAsHexadecimal:=tempnode.TextContent='1';
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
      j:=0;
      setlength(hotkeys, tempnode.ChildNodes.Count);
      for i:=0 to tempnode.ChildNodes.count-1 do
      begin
        if tempnode.ChildNodes[i].NodeName='Hotkey' then
        begin
          Hotkeys[j].value:='';
          ZeroMemory(@Hotkeys[j].keys,sizeof(TKeyCombo));

          tempnode2:=tempnode.childnodes[i].FindNode('Action');
          if tempnode2<>nil then
            hotkeys[j].action:=TextToMemRecHotkeyAction(tempnode2.TextContent);

          tempnode2:=tempnode.childnodes[i].findnode('Value');
          if tempnode2<>nil then
            hotkeys[j].value:=tempnode2.TextContent;

          tempnode2:=tempnode.ChildNodes[i].FindNode('Keys');
          if tempnode2<>nil then
          begin
            l:=0;
            for k:=0 to tempnode2.ChildNodes.Count-1 do
            begin
              if tempnode2.ChildNodes[k].NodeName='Key' then
              begin
                try
                  hotkeys[j].keys[l]:=StrToInt(tempnode2.ChildNodes[k].TextContent);
                  inc(l);
                except
                end;
              end;
            end;

          end;

          hotkeys[j].active:=true;

          RegisterHotKey2(mainform.handle, 0, hotkeys[j].keys, self, j);


          inc(j);
        end;
      end;

      setlength(hotkeys,j);
    end;
    ReinterpretAddress;
    refresh;
  end;


  SetVisibleChildrenState;


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
begin
  if selectedonly and (not isselected) then exit; //don't add if not selected and only the selected items should be added

  doc:=node.OwnerDocument;
  cheatEntry:=doc.CreateElement('CheatEntry');
  cheatEntry.AppendChild(doc.CreateElement('Description')).TextContent:='"'+description+'"';

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
      end;

      vtByteArray:
      begin
        cheatEntry.AppendChild(doc.CreateElement('ByteLength')).TextContent:=inttostr(extra.byteData.bytelength);
        cheatEntry.AppendChild(doc.CreateElement('ShowAsHexadecimal')).TextContent:=booltostr(extra.byteData.ShowAsHexadecimal,'1','0');
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
    if length(hotkeys)>0 then
    begin
      hks:=cheatentry.AppendChild(doc.CreateElement('Hotkeys'));
      for i:=0 to length(Hotkeys)-1 do
      begin
        hk:=hks.AppendChild(doc.CreateElement('Hotkey'));
        hk.AppendChild(doc.CreateElement('Action')).TextContent:=MemRecHotkeyActionToText(hotkeys[i].action);
        hkkc:=hk.AppendChild(doc.createElement('Keys'));
        j:=0;
        while (j<5) and (hotkeys[i].keys[j]<>0) do
        begin
          hkkc.appendchild(doc.createElement('Key')).TextContent:=inttostr(hotkeys[i].keys[j]);
          inc(j);
        end;

        if hotkeys[i].value<>'' then
          hk.AppendChild(doc.CreateElement('Value')).TextContent:=hotkeys[i].value;
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

function TMemoryRecord.removeHotkey(i: integer): boolean;
begin
  result:=false;

  if i>=length(hotkeys) then exit;
  if not hotkeys[i].active then exit;

  UnregisterAddressHotkey(self,i);
  hotkeys[i].active:=false;
  result:=true;
end;

function TMemoryRecord.Addhotkey(windowhandle: THandle; keys: tkeycombo; action: TMemrecHotkeyAction; value: string): integer;
{
adds and registers a hotkey and returns the hotkey index for this hotkey
return -1 if failure
}
var i,j: integer;
begin
  //convert the string to keys
  j:=-1;
  for i:=0 to length(Hotkeys)-1 do
    if hotkeys[i].active then
    begin
      j:=i;
      break;
    end;

  if j=-1 then
  begin
    setlength(hotkeys,length(hotkeys)+1);
    j:=length(hotkeys)-1;
  end;

  hotkeys[j].keys:=keys;
  hotkeys[j].action:=action;
  hotkeys[j].value:=value;
  hotkeys[j].active:=true;

  RegisterHotKey2(windowhandle, 0, keys, self, j);
  result:=j;
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
      if VarType in [vtByte, vtWord, vtDword, vtQword, vtCustom] then
      begin
        oldvalue:=StrToInt64(getvalue);
        increasevalue:=StrToInt64(value);
        setvalue(IntToStr(oldvalue+increasevalue));
      end
      else
      begin
        oldvaluedouble:=StrToFloat(getValue);
        increasevalueDouble:=StrToFloat(value);
        setvalue(FloatToStr(oldvaluedouble+increasevalueDouble));
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
        oldvalue:=StrToInt64(getvalue);
        decreasevalue:=StrToInt64(value);
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
  treenode.Update;
end;

procedure TMemoryRecord.DoHotkey(i: integer);
begin
  if i<length(Hotkeys) then
  begin
    case hotkeys[i].action of
      mrhToggleActivation: active:=not active;
      mrhSetValue:         SetValue(hotkeys[i].value);
      mrhIncreaseValue:    increaseValue(hotkeys[i].value);
      mrhDecreaseValue:    decreaseValue(hotkeys[i].value);


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
  end;
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
  if not isGroupHeader then
  begin
    if self.VarType = vtAutoAssembler then
    begin
      //aa script
      try
        if autoassemblerdata.registeredsymbols=nil then
          autoassemblerdata.registeredsymbols:=tstringlist.create;

        if autoassemble(autoassemblerdata.script, false, state, false, false, autoassemblerdata.allocs, autoassemblerdata.registeredsymbols) then
          fActive:=state;
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

  if active then SetVisibleChildrenState;
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
    vtBinary: result:=(extra.bitData.Bit+extra.bitData.bitlength div 8);
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

procedure TMemoryRecord.ReinterpretAddress;
var he: boolean;
a: ptrUint;
begin
  a:=symhandler.getAddressFromName(interpretableaddress,false,he);
  if not he then
    baseaddress:=a;


end;

procedure TMemoryRecord.ApplyFreeze;
begin
  if (not isgroupheader) and active and (VarType<>vtAutoAssembler) then
    setValue(frozenValue);
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
  end else result:=inttohex(realaddress,8);
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

  temp:=temp xor mask; //temp now only contains the bits that are of meaning

  if formsettings.cbBinariesAsDecimal.checked then
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
          result:=wc;
        end
        else
          result:=c;
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

procedure TMemoryRecord.SetValue(v: string);
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

  wc: PWideChar absolute buf;
  c: PChar absolute buf;
  originalprotection: dword;

  bts: TBytes;
  mask: qword;
  temp: qword;
  temps: string;

  mr: TMemoryRecord;
begin
  //check if it is a '(description)' notation
  if vartype<>vtString then
  begin
    v:=trim(v);

    if (length(v)>2) and (v[1]='(') and (v[length(v)]=')') then
    begin
      //yes, it's a (description)
      temps:=copy(v, 2,length(v)-2);
      //search the addresslist for a entry with name (temps)

      mr:=TAddresslist(fOwner).findRecordWithDescription(temps);
      if mr<>nil then
        v:=mr.GetValue;

    end;
  end;


  if moRecursiveSetValue in options then //do this for all it's children
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

  currentValue:=v;
  frozenValue:=currentValue;

  bufsize:=getbytesize;

  if (vartype=vtbinary) and (bufsize=3) then bufsize:=4;
  if (vartype=vtbinary) and (bufsize>4) then bufsize:=8;

  getmem(buf,bufsize);



  VirtualProtectEx(processhandle, pointer(realAddress), bufsize, PAGE_EXECUTE_READWRITE, originalprotection);
  try

    if vartype in [vtBinary, vtByteArray] then //fill the buffer with the original byte
      if not ReadProcessMemory(processhandle, pointer(realAddress), buf, bufsize,x) then exit;

    case VarType of
      vtCustom: if customtype<>nil then customtype.ConvertIntegerToData(strtoint(FrozenValue), pdw);
      vtByte: pb^:=strtoint(FrozenValue);
      vtWord: pw^:=strtoint(FrozenValue);
      vtDword: pdw^:=strtoint(FrozenValue);
      vtQword: pqw^:=strtoint64(FrozenValue);
      vtSingle: ps^:=StrToFloat(FrozenValue);
      vtDouble: pd^:=StrToFloat(FrozenValue);
      vtBinary:
      begin
        if not Extra.bitData.showasbinary then
          temps:=FrozenValue
        else
          temps:=IntToStr(BinToInt(FrozenValue));

        temp:=StrToInt64(temps);
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
        bufsize:=min(length(frozenvalue),bufsize);

        for i:=1 to bufsize do
        begin
          if extra.stringData.unicode then
          begin
            wc[i-1]:=FrozenValue[i];
          end
          else
          begin
            c[i-1]:=FrozenValue[i];
          end;
        end;
      end;

      vtByteArray:
      begin
        ConvertStringToBytes(FrozenValue, showAsHex, bts);
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


end;

function TMemoryRecord.getBaseAddress: ptrUint;
begin
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


    //find what writes to the address pointer at by this pointer
    realaddress2:=BaseAddress;
    for i:=length(pointeroffsets)-1 downto 0 do
    begin
      realaddress:=0;
      check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,processhandler.pointersize,count);
      if check and (count=processhandler.pointersize) then
        realaddress2:=realaddress+pointeroffsets[i]
      else
      begin
        result:=0;
        UnreadablePointer:=true;
        realAddress:=0;
        exit;
      end;
    end;
    UnreadablePointer:=false;
    Result:=realaddress2;
  end
  else
    result:=BaseAddress; //not a pointer

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

