unit formChangedAddresses;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEFuncProc, ExtCtrls, ComCtrls, Menus, NewKernelHandler, LResources,
  disassembler, symbolhandler, byteinterpreter, CustomTypeHandler, maps, math, Clipbrd,
  addressparser, commonTypeDefs;

type
  TAddressEntry=class
  public
    address: ptruint; //for whatever reason it could be used in the future
    context: TContext;
    stack: record
      stack: pbyte;
      savedsize: PtrUInt;
    end;
    count: integer;

    group: integer;

    procedure savestack;
    destructor destroy; override;
  end;


  { TfrmChangedAddresses }

  TfrmChangedAddresses = class(TForm)
    lblInfo: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miCommonalitiesSubgroup: TMenuItem;
    miMarkAsGroup1: TMenuItem;
    miMarkAsGroup2: TMenuItem;
    miScanForCommonalities: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    miGroupClear: TMenuItem;
    miResetCount: TMenuItem;
    miDeleteSelectedEntries: TMenuItem;
    miCopyToAddresslist: TMenuItem;
    miDissect: TMenuItem;
    micbShowAsHexadecimal: TMenuItem;
    Panel1: TPanel;
    OKButton: TButton;
    Changedlist: TListView;
    cbDisplayType: TComboBox;
    Timer1: TTimer;
    PopupMenu1: TPopupMenu;
    Showregisterstates1: TMenuItem;
    Browsethismemoryregion1: TMenuItem;
    procedure ChangedlistColumnClick(Sender: TObject; Column: TListColumn);
    procedure ChangedlistCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ChangedlistCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure miGroupClearClick(Sender: TObject);
    procedure miMarkAsGroupClick(Sender: TObject);
    procedure miDeleteSelectedEntriesClick(Sender: TObject);
    procedure micbShowAsHexadecimalClick(Sender: TObject);
    procedure miDissectClick(Sender: TObject);
    procedure miResetCountClick(Sender: TObject);
    procedure miScanForCommonalitiesClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ChangedlistDblClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Showregisterstates1Click(Sender: TObject);
    procedure Browsethismemoryregion1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    hassetsizes: boolean;
    addresslist: TMap;
    faddress: ptruint;
    defaultcolor: TColor;
    procedure refetchValues(specificaddress: ptruint=0);
    procedure setAddress(a: ptruint);
  public
    { Public declarations }
    equation: string;
    foundcodedialog: pointer;

    procedure AddRecord;
    property address: ptruint read fAddress write setAddress;
  end;


implementation


uses CEDebugger, MainUnit, frmRegistersunit, MemoryBrowserFormUnit, debughelper,
  debugeventhandler, debuggertypedefinitions, FoundCodeUnit, StructuresFrm2,
  processhandlerunit, Globals, Parsers, frmStackViewUnit, frmSelectionlistunit,
  frmChangedAddressesCommonalityScannerUnit;

resourcestring
  rsStop='Stop';
  rsClose='Close';
  rsNoDescription = 'No Description';
  rsChangedAddressesBy = 'Changed Addresses by %x';
  rsDesignateSomeAddresses = 'Please designate a group to at least some '
    +'addresses';
  rsNoAddressesLeftForGroup = 'There are no addresses left for group %d';
  rsInvalidGroups = 'Invalid groups';
  rsDeleteAddresses = 'Delete addresses';
  rsAreYouWishToDelete = 'Are you sure you wish to delete these entries(s)?';

destructor TAddressEntry.destroy;
begin
  if stack.stack<>nil then
    freememandnil(stack.stack);

  inherited destroy;
end;

procedure TAddressEntry.savestack;
begin
  getmem(stack.stack, savedStackSize);
  if ReadProcessMemory(processhandle, pointer(context.{$ifdef cpu64}Rsp{$else}esp{$endif}), stack.stack, savedStackSize, stack.savedsize)=false then
  begin
    //for some reason this sometimes returns 0 bytes read even if some of the bytes are readable.
    stack.savedsize:=4096-(context.{$ifdef cpu64}Rsp{$else}esp{$endif} mod 4096);
    ReadProcessMemory(processhandle, pointer(context.{$ifdef cpu64}Rsp{$else}esp{$endif}), stack.stack, stack.savedsize, stack.savedsize);
  end;
end;


procedure TfrmChangedAddresses.AddRecord;
var
  haserror: boolean;
  address: ptrUint;
  i: integer;
  li: tlistitem;
  currentthread: TDebugThreadHandler;

  x: TaddressEntry;
  s: string;
begin
  //the debuggerthread is idle at this point
  currentThread:=debuggerthread.CurrentThread;
  if currentthread<>nil then
  begin
    //get the instruction address
    address:=symhandler.getAddressFromName(equation, false, haserror, currentthread.context);

    if not hasError then
    begin
      //check if this address is already in the list
      if addresslist.GetData(address, x) then
      begin
        inc(x.count);
        refetchValues(x.address);
        exit;
      end;

      s:=inttohex(address,8);

      //and if not, add it

      if (foundcodedialog=nil) or (changedlist.Items.Count<8) then
      begin
        li:=changedlist.Items.add;
        li.caption:=s;
        li.SubItems.Add('');
        li.subitems.add('1');


        x:=TAddressEntry.create;
        x.context:=currentthread.context^;
        x.address:=address;
        x.count:=1;
        x.savestack;


        li.Data:=x;

        addresslist.Add(address, x);
        refetchValues(x.address);

        if foundcodedialog<>nil then
        begin
          TFoundCodeDialog(foundcodedialog).setChangedAddressCount(currentthread.context.{$ifdef cpu64}Rip{$else}eip{$endif});
          if (changedlist.Items.Count>=8) then //remove this breakpoint
            debuggerthread.FindWhatCodeAccessesStop(self);
        end;
      end;
    end;
  end;
end;


procedure TfrmChangedAddresses.OKButtonClick(Sender: TObject);
var temp: dword;
    i: integer;

begin

  if OKButton.caption=rsStop then
  begin
    debuggerthread.FindWhatCodeAccessesStop(self);
    okButton.Caption:=rsClose;
  end
  else
    close;

end;

procedure TfrmChangedAddresses.micbShowAsHexadecimalClick(Sender: TObject);
begin
  refetchvalues;
end;

procedure TfrmChangedAddresses.miDissectClick(Sender: TObject);
var
  i: integer;
  ae: TAddressEntry;
  ap: TAddressParser;
  address: ptruint;

  s: tstringlist;
  f: TfrmSelectionList;

  structurefrm: TfrmStructures2;
  new: boolean;

  maxoffset: dword;

begin
  ap:=TAddressParser.Create;

  if changedlist.SelCount>0 then
  begin
    //find out which data dissect windows are open
    s:=tstringlist.create;

    if frmStructures2=nil then
      raise exception.create(rsTheStructuresListIsBroken);

    for i:=0 to frmStructures2.Count-1 do
      s.add(TfrmStructures2(frmStructures2[i]).Caption);

    s.add(rsNewWindow);

    f:=TfrmSelectionList.Create(self, s);

    f.caption:=rsLockAndAddToStructureDissect;
    f.label1.Caption:=rsSelectTheStructureDissectWindowYouWishToAddThisReg;

    if f.showmodal=mrok then
    begin
      if f.itemindex=-1 then f.itemindex:=0;

      if f.itemindex>=frmStructures2.Count then       //new window
      begin
        structurefrm:=tfrmstructures2.create(application);
        structurefrm.show;
      end
      else
        structurefrm:=TfrmStructures2(frmStructures2[f.itemindex]);

      //add the addresses


      maxoffset:=4096;
      for i:=0 to changedlist.Items.Count-1 do
      begin
        if changedlist.Items[i].Selected then
        begin
          ae:=changedlist.items[i].data;
          ap.setSpecialContext(@ae.context);
          address:=ap.getBaseAddress(equation);

          maxoffset:=max(maxoffset, 8+strtoint64('$'+changedlist.Items[i].Caption)-address);

          if address<>0 then
            structurefrm.addColumn.Address:=address;
        end;
      end;


      structurefrm.show;

      if structurefrm.mainStruct=nil then //if no structure is selected define it then
        structurefrm.DefineNewStructure(maxoffset);

    end;

  end;



  ap.free;

end;



procedure TfrmChangedAddresses.ChangedlistColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  changedlist.SortColumn:=Column.Index;
  changedlist.SortType:=stData;
end;

procedure TfrmChangedAddresses.ChangedlistCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var i1, i2: TAddressEntry;
    hex, hex2: qword;
    s: single absolute hex;
    s2: single absolute hex2;
    d: double absolute hex;
    d2: double absolute hex2;
    x: PtrUInt;
    varsize: integer;

begin
  compare:=0;
  i1:=TAddressEntry(item1.data);
  i2:=TAddressEntry(item2.data);

  if (i1=nil) or (i2=nil) then exit;

  case changedlist.SortColumn of
    0: //sort by address
    begin
      compare:=CompareValue(i1.address, i2.address);
    end;


    1: //sort by value
    begin
      hex:=0;
      hex2:=0;
      case cbDisplayType.itemindex of
        0: varsize:=1;
        1: varsize:=2;
        2: varsize:=4;
        3: varsize:=4;
        4: varsize:=8;
        else
        begin
          Compare:=0;
          exit;
        end;
      end;

      if not ReadProcessMemory(processhandle, pointer(i1.address), @hex, VarSize, x) then
      begin
        compare:=-1;//1<2
        exit;
      end;

      if not ReadProcessMemory(processhandle, pointer(i2.address), @hex2, VarSize, x) then
      begin
        compare:=1;//1>2
        exit;
      end;

      if micbShowAsHexadecimal.checked then
      begin
        compare:=hex-hex2;
      end
      else
      begin
        case cbDisplayType.itemindex of
          0: compare:=byte(hex)-byte(hex2);
          1: compare:=word(hex)-word(hex2);
          2: compare:=dword(hex)-dword(hex2);
          3:
          begin
            try
              if s>s2 then
                compare:=1
              else
              if s=s2 then
                compare:=0
              else
                compare:=-1;

            except
              compare:=-1;
            end;
          end;

          4:
          begin
            try
              if d>d2 then
                compare:=1
              else
              if d=d2 then
                compare:=0
              else
                compare:=-1;
            except
              compare:=-1;
            end;
          end;
        end;
      end;
    end;

    2: //sort by count
    begin
      compare:=CompareValue(i1.count, i2.count);
    end;
  end;
end;

procedure TfrmChangedAddresses.ChangedlistCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);

var
  e: TAddressEntry;
  i: integer;
begin
  i:=item.index;
  if tobject(item.Data) is TAddressEntry then
  begin
    e:=TAddressEntry(item.data);


    case e.group of
      1: sender.Canvas.Font.color:=clBlue;
      2: sender.Canvas.Font.color:=clRed;
      else
        sender.Canvas.Font.color:=defaultcolor;
    end;
  end
  else
  begin
    asm
    nop
    end;
  end;

  DefaultDraw:=true;
end;


procedure TfrmChangedAddresses.MenuItem1Click(Sender: TObject);
var
  list: Tstringlist;
  i: integer;
begin
  list:=tstringlist.create;
  for i:=0 to changedlist.Items.Count-1 do
    if changedlist.Items[i].Selected then
      list.add(changedlist.Items[i].Caption);

  clipboard.AsText:=list.text;
  list.free;
end;

procedure TfrmChangedAddresses.MenuItem3Click(Sender: TObject);
var
  i: integer;
  a: ptruint;
  value: string;

  vartype: TVariableType;
  ct: TCustomType;
begin
  if changedlist.ItemIndex<>-1 then
  begin
    value:=changedlist.Items[changedlist.ItemIndex].SubItems[0];
    if InputQuery('Value Change','Give the new value',value)=false then exit;
  end;

  for i:=0 to changedlist.Items.Count-1 do
  begin
    if changedlist.items[i].Selected then
    begin
      a:=symhandler.getAddressFromName(changedlist.items[i].Caption);

      vartype:=vtDword;
      ct:=nil;

      ct:=TCustomType(cbDisplayType.Items.Objects[cbDisplayType.ItemIndex]);
      if ct=nil then
      begin
        case cbDisplayType.ItemIndex of
          0: vartype:=vtByte;
          1: vartype:=vtWord;
          3: vartype:=vtSingle;
          4: vartype:=vtDouble;
        end;
      end
      else
        vartype:=vtCustom;

      ParseStringAndWriteToAddress(value,a, vartype,false,ct);
    end;
  end;

end;

procedure TfrmChangedAddresses.miGroupClearClick(Sender: TObject);
var
  i: integer;
  e: TAddressEntry;
begin
  for i:=0 to Changedlist.Items.count-1 do
  begin
    e:=TAddressEntry(changedlist.items[i].Data);
    e.group:=0;
  end;
end;

procedure TfrmChangedAddresses.miMarkAsGroupClick(Sender: TObject);
var
  i: integer;
  e: TAddressEntry;
begin
  for i:=0 to Changedlist.Items.count-1 do
    if changedlist.items[i].Selected then
    begin
      e:=TAddressEntry(changedlist.items[i].Data);

      e.group:=tmenuitem(sender).tag;
    end;

  changedlist.Refresh;
end;

procedure TfrmChangedAddresses.miResetCountClick(Sender: TObject);
var
  i: integer;
  ae: TAddressEntry;
begin
  for i:=changedlist.items.Count-1 downto 0 do
  begin
    ae:=TAddressEntry(changedlist.Items[i].Data);
    ae.count:=0;
  end;

  refetchValues;
end;

procedure TfrmChangedAddresses.miScanForCommonalitiesClick(Sender: TObject);
var
  i,j,k: integer;
  g0: array of TAddressEntry;
  g1: array of TAddressEntry;
  g2: array of TAddressEntry;
  e: TAddressEntry;

  gs: array of TAddressEntry;


  f:TfrmChangedAddressesCommonalityScanner;
begin
  setlength(g0,0);
  setlength(g1,0);
  setlength(g2,0);
  setlength(gs,0);

  for i:=0 to changedlist.items.Count-1 do
  begin
    e:=TAddressEntry(changedlist.items[i].data);
    case e.group of
      0:
      begin
        setlength(g0,length(g0)+1);
        g0[length(g0)-1]:=e;
      end;

      1:
      begin
        setlength(g1,length(g1)+1);
        g1[length(g1)-1]:=e;
      end;

      2:
      begin
        setlength(g2,length(g2)+1);
        g2[length(g2)-1]:=e;
      end;

      else raise exception.create(rsInvalidGroups);
    end;

    if changedlist.items[i].Selected then
    begin
      setlength(gs,length(gs)+1);
      gs[length(gs)-1]:=e;
    end;
  end;

  if (length(g1)=0) and (length(g2)=0) then
  begin
    if length(gs)>1 then //nothing marked, but more than 1 address selected
    begin
      g1:=gs;
      //delete all g0 entries that are in g1(gs)
      for i:=0 to length(g1)-1 do
      begin
        for j:=0 to length(g0)-1 do
          if g0[j]=g1[i] then
          begin
            //found one
            for k:=j to length(g0)-2 do
              g0[k]:=g0[k+1];

            setlength(g0,length(g0)-1);
          end;
      end;

    end
    else
      raise exception.create(rsDesignateSomeAddresses);
  end;

  if (length(g1)>0) and (length(g2)=0) then
  begin
    if length(g0)=0 then raise exception.create(format(rsNoAddressesLeftForGroup, [1]));
    g2:=g0;
  end
  else
  if (length(g1)=0) and (length(g2)>0) then
  begin
    if length(g0)=0 then raise exception.create(format(rsNoAddressesLeftForGroup, [1]));
    g2:=g0;
  end;

  //process g1 and g2

  f:=TfrmChangedAddressesCommonalityScanner.Create(application); //don't destroy when you close this window
  f.setgroup(1, g1);
  f.setgroup(2, g2);
  f.show;

  f.initlist;
end;

procedure TfrmChangedAddresses.miDeleteSelectedEntriesClick(Sender: TObject);
var
  i: integer;
  ae: TAddressEntry;
begin
  if changedlist.SelCount>=1 then
  begin
    if MessageDlg(rsDeleteAddresses, rsAreYouWishToDelete, mtConfirmation, [mbyes,mbno],0) = mryes then
    begin
      i:=0;
      while i<changedlist.items.Count do
      begin
        if changedlist.items[i].Selected then
        begin
          ae:=TAddressEntry(changedlist.Items[i].Data);

          if addresslist<>nil then
            addresslist.Delete(ae.address);

          if ae<>nil then
            ae.free;

          changedlist.Items[i].data:=nil;
          changedlist.Items[i].Delete;
        end
        else
          inc(i);
      end;
    end;
  end;
end;

procedure TfrmChangedAddresses.FormClose(Sender: TObject;
  var Action: TCloseAction);
var temp:dword;
    i: integer;
    ae: TAddressEntry;
begin
  if foundcodedialog=nil then
    action:=caFree
  else
    action:=caHide; //let the foundcodedialog free it



end;

procedure TfrmChangedAddresses.FormShow(Sender: TObject);
begin
//  defaultcolor:=
  defaultcolor:=Changedlist.GetDefaultColor(dctFont);

  OKButton.Caption:=rsStop;

  if not hassetsizes then
  begin
    changedlist.Column[0].Width:=max(changedlist.Column[0].Width, canvas.TextWidth('DDDDDDDDFFFFF'));
    changedlist.Column[1].Width:=max(changedlist.Column[1].Width, canvas.TextWidth('9999999.999'));
    changedlist.Column[2].Width:=max(changedlist.Column[2].Width, canvas.TextWidth('999999'));

    ClientWidth:=max(clientwidth, changedlist.Column[0].Width+changedlist.Column[1].Width+changedlist.Column[2].Width+20);
    hassetsizes:=true;
  end;
end;

procedure TfrmChangedAddresses.ChangedlistDblClick(Sender: TObject);
var i: integer;
    ad: dword;
    vartype: TVariableType;
    ct: TCustomType;
begin
  vartype:=vtDword;
  ct:=nil;


  ct:=TCustomType(cbDisplayType.Items.Objects[cbDisplayType.ItemIndex]);
  if ct=nil then
  begin
    case cbDisplayType.ItemIndex of
      0: vartype:=vtByte;
      1: vartype:=vtWord;
      3: vartype:=vtSingle;
      4: vartype:=vtDouble;
    end;
  end
  else
    vartype:=vtCustom;

  for i:=0 to changedlist.Items.Count-1 do
    if changedlist.Items[i].Selected then
      mainform.addresslist.addaddress(rsNoDescription, changedlist.Items[i].caption, [], 0, vartype, cbDisplayType.Text );
end;

procedure TfrmChangedAddresses.PopupMenu1Popup(Sender: TObject);
begin
  Showregisterstates1.enabled:=changedlist.selected<>nil;
  Browsethismemoryregion1.enabled:=changedlist.selected<>nil;
  miDeleteSelectedEntries.enabled:=changedlist.SelCount>0;

  miDissect.enabled:=changedlist.SelCount>0;
end;

procedure TfrmChangedAddresses.refetchValues(specificaddress: ptruint=0);
var i: integer;
    s: string;
    handled: boolean;
    startindex: integer;
    stopindex: integer;
begin
  if changedlist.Items.Count>0 then
  begin
    if Changedlist.TopItem=nil then exit;

    startindex:=Changedlist.TopItem.Index;
    stopindex:=min(Changedlist.TopItem.Index+changedlist.VisibleRowCount, Changedlist.Items.Count-1);

    for i:=startindex to stopindex do
    begin

      if (specificaddress<>0) and (TAddressEntry(changedlist.items[i].Data).address <> specificaddress) then //check if this is the line to update, if not, don't read and parse
        continue;


      case cbDisplayType.ItemIndex of
        0: s:=ReadAndParseAddress(TAddressEntry(changedlist.items[i].Data).address, vtByte,  nil, micbShowAsHexadecimal.checked);
        1: s:=ReadAndParseAddress(TAddressEntry(changedlist.items[i].Data).address, vtWord,  nil, micbShowAsHexadecimal.checked);
        2: s:=ReadAndParseAddress(TAddressEntry(changedlist.items[i].Data).address, vtDWord, nil, micbShowAsHexadecimal.checked);
        3: s:=ReadAndParseAddress(TAddressEntry(changedlist.items[i].Data).address, vtSingle,nil, micbShowAsHexadecimal.checked);
        4: s:=ReadAndParseAddress(TAddressEntry(changedlist.items[i].Data).address, vtDouble,nil, micbShowAsHexadecimal.checked);
        else
        begin
          //custom type
          s:=ReadAndParseAddress(TAddressEntry(changedlist.items[i].Data).address, vtCustom, TCustomType(cbDisplayType.Items.Objects[cbDisplayType.ItemIndex]), micbShowAsHexadecimal.checked);
        end;
      end;



      while Changedlist.Items[i].SubItems.Count<2 do
        Changedlist.Items[i].SubItems.Add('');


      changedlist.items[i].SubItems[1]:=inttostr(TAddressEntry(changedlist.items[i].Data).count);



      Changedlist.Items[i].SubItems[0]:=s;
    end;
  end;
end;

procedure TfrmChangedAddresses.Timer1Timer(Sender: TObject);
begin
  refetchValues;
end;

procedure TfrmChangedAddresses.Showregisterstates1Click(Sender: TObject);
var ae: TAddressEntry;
begin
  if changedlist.Selected<>nil then
  begin
    with TRegisters.create(self) do
    begin
      //borderstyle:=bsSingle;

      ae:=TAddressEntry(changedlist.Selected.Data);

      SetContextPointer(@ae.context, ae.stack.stack, ae.stack.savedsize);

      show;
    end;
  end;
end;

procedure TfrmChangedAddresses.Browsethismemoryregion1Click(
  Sender: TObject);
begin
  if changedlist.Selected<>nil then
  begin
    memorybrowser.memoryaddress:=StrToQWordEx('$'+changedlist.Selected.Caption);
    if not memorybrowser.visible then
      memorybrowser.show;
  end;
end;

procedure TfrmChangedAddresses.setAddress(a: ptruint);
begin
  faddress:=a;
  caption:=format(rsChangedAddressesBy, [a]);
end;

procedure TfrmChangedAddresses.FormDestroy(Sender: TObject);
var
  i: integer;
  ae: TAddressEntry;
  x: array of integer;
begin
  for i:=0 to changedlist.Items.Count-1 do
  begin
    ae:=TAddressEntry(changedlist.Items[i].Data);
    ae.free;
  end;

  if OKButton.caption=rsStop then
    debuggerthread.FindWhatCodeAccessesStop(self);

  setlength(x,3);
  x[0]:=changedlist.Column[0].Width;
  x[1]:=changedlist.Column[1].Width;
  x[2]:=changedlist.Column[2].Width;

  saveformposition(self,x);
  if addresslist<>nil then
    addresslist.Free;
end;

procedure TfrmChangedAddresses.FormCreate(Sender: TObject);
var x: array of integer;
    i: integer;
begin
  okbutton.caption:=rsStop;

  setlength(x, 0);
  if loadformposition(self,x) then
  begin
    if length(x)>0 then
    begin
      changedlist.Column[0].Width:=x[0];
      changedlist.Column[1].Width:=x[1];
      changedlist.Column[2].Width:=x[2];
      hassetsizes:=true;
    end;

  end;

  //fill in the custom types
  for i:=0 to customTypes.count-1 do
    cbDisplayType.Items.AddObject(TCustomType(customTypes[i]).name, customTypes[i]);

  addresslist:=TMap.Create(ituPtrSize,sizeof(pointer));
end;

initialization
  {$i formChangedAddresses.lrs}

end.

