unit MemoryRecordUnit;

{$mode DELPHI}

interface

uses
  Windows, Classes, SysUtils, controls, stdctrls, comctrls,symbolhandler, cefuncproc,newkernelhandler, autoassembler, hotkeyhandler, dom, XMLRead,XMLWrite;

type TMemrecHotkeyAction=(mrhToggleActivation, mrhToggleActivationAllowIncrease, mrhToggleActivationAllowDecrease, mrhSetValue, mrhIncreaseValue, mrhDecreaseValue);

type TFreezeType=(ftFrozen, ftAllowIncrease, ftAllowDecrease);

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




type TMemoryRecord=class
  private
    FrozenValue : string;
    CurrentValue: string;
    UnreadablePointer: boolean;
    BaseAddress: ptrUint;
    RealAddress: ptrUint;

    fActive: boolean;
    fAllowDecrease: boolean;
    fAllowIncrease: boolean;

    fShowAsHex: boolean;
    editcount: integer; //=0 when not being edited
    function getByteSize: integer;
    function BinaryToString(b: pbytearray; bufsize: integer): string;
    function getAddressString: string;
    procedure setActive(state: boolean);
    procedure setAllowDecrease(state: boolean);
    procedure setAllowIncrease(state: boolean);
    procedure setShowAsHex(state: boolean);
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
    procedure ReinterpretAddress;
    property Value: string read GetValue write SetValue;
    property bytesize: integer read getByteSize;

    function Addhotkey(windowhandle: THandle; keys: tkeycombo; action: TMemrecHotkeyAction; value: string): integer;
    function removeHotkey(i: integer): boolean;

    procedure DoHotkey(i: integer); //execute the specific hotkey action

    procedure disablewithoutexecute;
    procedure refresh;

    function getXMLNode(node: TDOMNode; selectedOnly: boolean): TDOMNode;
    procedure setXMLnode(CheatEntry: TDOMNode);


    property addressString: string read getAddressString;
    property active: boolean read fActive write setActive;

    property allowDecrease: boolean read fallowDecrease write setAllowDecrease;
    property allowIncrease: boolean read fallowIncrease write setAllowIncrease;
    property showAsHex: boolean read fShowAsHex write setShowAsHex;

    destructor destroy; override;
  end;

implementation

uses formsettingsunit;

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

procedure TMemoryRecord.setXMLnode(CheatEntry: TDOMNode);
var tempnode: TDOMNode;
i,j: integer;

currentEntry: TDOMNode;

memrec: TMemoryRecord;
begin
  if TDOMElement(CheatEntry).TagName<>'CheatEntry' then exit; //invalid node type

  tempnode:=CheatEntry.FindNode('Description');
  if tempnode<>nil then Description:=tempnode.TextContent;

  tempnode:=CheatEntry.FindNode('GroupHeader');
  if tempnode<>nil then
    isGroupHeader:=tempnode.TextContent='1';

  if isGroupHeader then
  begin
    tempnode:=CheatEntry.FindNode('CheatEntries');
    currentEntry:=tempnode.FirstChild;
    while currentEntry<>nil do
    begin
      //create a blank entry
      memrec:=TMemoryRecord.create;

      memrec.treenode:=treenode.owner.AddObject(nil,'',memrec);
      memrec.treenode.MoveTo(treenode, naAddChild); //make it the last child of this node

      //fill the entry with the node info
      memrec.setXMLnode(currentEntry);
      currentEntry:=currentEntry.NextSibling;
    end;
  end
  else
  begin
    tempnode:=CheatEntry.FindNode('VariableType');
    if tempnode<>nil then
      VarType:=StringToVariableType(tempnode.TextContent);

    case VarType of
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
    ReinterpretAddress;
    refresh;
  end;


end;

function TMemoryRecord.getXMLNode(node: TDOMNode; selectedOnly: boolean): TDOMNode;
var
  doc: TDOMDocument;
  cheatEntry: TDOMNode;
  cheatEntries: TDOMNode;
  offsets: TDOMNode;

  tn: TTreenode;
  i: integer;
begin
  if selectedonly and (not isselected) then exit; //don't add if not selected and only the selected items should be added

  doc:=node.OwnerDocument;
  cheatEntry:=doc.CreateElement('CheatEntry');
  cheatEntry.AppendChild(doc.CreateElement('Description')).TextContent:=description;


  if isGroupHeader then
  begin
    cheatEntry.AppendChild(doc.CreateElement('GroupHeader')).TextContent:='1';
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
  end
  else
  begin
    cheatEntry.AppendChild(doc.CreateElement('VariableType')).TextContent:=VariableTypeToString(vartype);
    case VarType of
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

      vtCustom:
      begin
        cheatEntry.AppendChild(doc.CreateElement('AssemblerScript')).TextContent:=AutoAssemblerData.script.Text;
      end;
    end;

    if VarType<>vtCustom then
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
  if VarType in [vtByte, vtWord, vtDword, vtQword, vtSingle, vtDouble] then
  begin
    try
      if VarType in [vtByte, vtWord, vtDword, vtQword] then
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

  if isGroupHeader then
  begin
    //apply this state to all the children
    for i:=0 to treenode.Count-1 do
      TMemoryRecord(treenode[i].data).setActive(state);

    fActive:=state;
  end
  else
  begin
    if self.VarType = vtCustom then
    begin
      //aa script
      try
        if autoassemblerdata.registeredsymbols=nil then
          autoassemblerdata.registeredsymbols:=tstringlist.create;

        autoassemble(autoassemblerdata.script, false, state, false, false, autoassemblerdata.allocs, autoassemblerdata.registeredsymbols);
        fActive:=state;
      except
        //running the script failed, state unchanged
      end;

    end
    else
    begin
      //freeze/unfreeze
      f:=GetValue;

      try
        SetValue(f);
      except
        fActive:=false;
        beep;
        exit;
      end;

      //still here so F is ok

      if state then //enabled
        FrozenValue:=f;

      fActive:=state;
    end;


    if state=false then
    begin
      //on disable or failure setting the state to true, also reset the option if it's allowed to increase/decrease
      allowDecrease:=false;
      allowIncrease:=false;
    end;
    treenode.update;

  end;
end;

procedure TMemoryRecord.setShowAsHex(state:boolean);
begin
  fShowAsHex:=state;
  self.treenode.Update;
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
  end;
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
  if (not isgroupheader) and active and (VarType<>vtCustom) then
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

  getmem(buf,bufsize);


  GetRealAddress;

  if ReadProcessMemory(processhandle, pointer(realAddress), buf, bufsize,br) then
  begin
    case vartype of
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
begin
  if isGroupHeader then exit;

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
      vtByte: pb^:=strtoint(FrozenValue);
      vtWord: pw^:=strtoint(FrozenValue);
      vtDword: pdw^:=strtoint(FrozenValue);
      vtQword: pqw^:=strtoint64(FrozenValue);
      vtSingle: ps^:=StrToFloat(FrozenValue);
      vtDouble: ps^:=StrToFloat(FrozenValue);
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
        for i:=0 to min(length(FrozenValue), bufsize)-1 do
        begin
          if extra.stringData.unicode then
          begin
            wc[i]:=FrozenValue[i];
          end
          else
          begin
            c[i]:=FrozenValue[i];
          end;
        end;
      end;

      vtByteArray:
      begin
        ConvertStringToBytes(FrozenValue, showAsHex, bts);
        for i:=0 to min(length(bts),bufsize)-1 do
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
    for i:=0 to length(pointeroffsets)-1 do
    begin
      check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,processhandler.pointersize,count);
      if check and (count=sizeof(PtrUInt)) then
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


end.

