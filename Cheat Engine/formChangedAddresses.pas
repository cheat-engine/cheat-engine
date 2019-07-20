unit formChangedAddresses;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEFuncProc, ExtCtrls, ComCtrls, Menus, NewKernelHandler, LResources,
  disassembler, symbolhandler, byteinterpreter, CustomTypeHandler, maps, math, Clipbrd,
  addressparser, commonTypeDefs, DBK32functions, vmxfunctions;

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
  TfrmChangedAddresses=class;

  TDBVMWatchExecutePollThread=class(TThread)
  private
    results: PPageEventListDescriptor;
    resultsize: integer;

    cr3disassembler: Tcr3Disassembler;
    procedure addEntriesToList;
  public
    id: integer;
    fca: TfrmChangedAddresses;
    procedure execute; override;
  end;

  TfrmChangedAddresses = class(TForm)
    caImageList: TImageList;
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

    fdbvmwatchid: integer;
    dbvmwatchpollthread: TDBVMWatchExecutePollThread;
    procedure stopdbvmwatch;
    procedure refetchValues(specificaddress: ptruint=0;countonly: boolean=false);
    procedure setAddress(a: ptruint);
    procedure setdbvmwatchid(id: integer);
  public
    { Public declarations }
    equation: string;
    foundcodedialog: pointer;

    dbvmwatch_unlock: qword;

    procedure AddRecord;
    property address: ptruint read fAddress write setAddress;
    property dbvmwatchid: integer read fdbvmwatchid write setdbvmwatchid;
  end;


implementation


uses CEDebugger, MainUnit, frmRegistersunit, MemoryBrowserFormUnit, debughelper,
  debugeventhandler, debuggertypedefinitions, FoundCodeUnit, StructuresFrm2,
  processhandlerunit, Globals, Parsers, frmStackViewUnit, frmSelectionlistunit,
  frmChangedAddressesCommonalityScannerUnit, LastDisassembleData;

resourcestring
  rsStop='Stop';
  rsClose='Close';
  rsNoDescription = 'No Description';
  rsChangedAddressesBy = 'Accessed addresses by %x';
  rsDesignateSomeAddresses = 'Please designate a group to at least some '
    +'addresses';
  rsNoAddressesLeftForGroup = 'There are no addresses left for group %d';
  rsInvalidGroups = 'Invalid groups';
  rsDeleteAddresses = 'Delete addresses';
  rsAreYouWishToDelete = 'Are you sure you wish to delete these entries(s)?';
  rsCodeAccessesSingleAddress = 'This address has been accessed by the code '
    +'you selected';
  rsCodeAccessesAddresses = 'The following %d addresses have been accessed by '
    +'the code you selected';
  rsValueChange = 'Value Change';
  rsGiveTheNewValue = 'Give the new value';



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


//-----------------------------

procedure TDBVMWatchExecutePollThread.addEntriesToList;
var
  c: TContext;
  coderecord: TCodeRecord;
  i,j: integer;
  opcode,desc: string;
  li: TListItem;
  ldi: TLastDisassembleData;

  basicinfo: TPageEventBasic;

  basic: PPageEventBasicArray;
  extended: PPageEventExtendedArray absolute basic;
  basics: PPageEventBasicWithStackArray absolute basic;
  extendeds: PPageEventExtendedWithStackArray absolute basic;

  address, address2: ptruint;

  skip: boolean;

  debug,debug2: pointer;
  x: TaddressEntry;
  haserror: boolean;
  s: string;
begin
  outputdebugstring('addEntriesToList ('+inttostr(results^.numberOfEntries)+')');

  try
    basic:=PPageEventBasicArray(ptruint(results)+sizeof(TPageEventListDescriptor));

    for i:=0 to results^.numberOfEntries-1 do
    begin

      case results^.entryType of
        0: basicinfo:=basic^[i];
        1: basicinfo:=extended^[i].basic;
        2: basicinfo:=basics^[i].basic;
        3: basicinfo:=extendeds^[i].basic;
      end;

      ZeroMemory(@c,sizeof(c));

      c.{$ifdef cpu64}Rax{$else}Eax{$endif}:=basicinfo.RAX;
      c.{$ifdef cpu64}Rbx{$else}Ebx{$endif}:=basicinfo.RBX;
      c.{$ifdef cpu64}Rcx{$else}Ecx{$endif}:=basicinfo.RCX;
      c.{$ifdef cpu64}Rdx{$else}Edx{$endif}:=basicinfo.RDX;
      c.{$ifdef cpu64}Rsi{$else}Esi{$endif}:=basicinfo.RSI;
      c.{$ifdef cpu64}Rdi{$else}Edi{$endif}:=basicinfo.RDI;
      c.{$ifdef cpu64}Rbp{$else}Ebp{$endif}:=basicinfo.RBP;
      c.{$ifdef cpu64}Rsp{$else}Esp{$endif}:=basicinfo.Rsp;
      c.{$ifdef cpu64}Rip{$else}Eip{$endif}:=basicinfo.Rip;
      {$ifdef cpu64}
      c.R8:=basicinfo.R8;
      c.R9:=basicinfo.R9;
      c.R10:=basicinfo.R10;
      c.R11:=basicinfo.R11;
      c.R12:=basicinfo.R12;
      c.R13:=basicinfo.R13;
      c.R14:=basicinfo.R14;
      c.R15:=basicinfo.R15;
      {$endif}

      address:=symhandler.getAddressFromName(fca.equation, false, haserror, @c);
      if haserror=false then
      begin

        //check if address is in the list
        if fca.addresslist.GetData(address,x) then
        begin
          inc(x.count,basicinfo.Count+1);
          fca.refetchValues(x.address, true);
          continue;
        end;

        //still here so not in the list, get more info and add it

        c.EFlags:=basicinfo.FLAGS;
        c.SegCs:=basicinfo.CS;
        c.SegSs:=basicinfo.SS;
        c.SegDs:=basicinfo.DS;
        c.SegEs:=basicinfo.ES;
        c.SegFs:=basicinfo.FS;
        c.SegGs:=basicinfo.GS;

        x:=TAddressEntry.Create;
        x.address:=address;
        x.count:=basicinfo.Count+1;

        case results^.entryType of
          1:
          begin
            //fpu
            copymemory(@c.{$ifdef cpu64}FltSave{$else}ext{$endif}, @extended^[i].fpudata, sizeof(c.{$ifdef cpu64}FltSave{$else}ext{$endif}));
          end;
          2:
          begin
            //stack
            getmem(x.stack.stack, 4096);
            copymemory(x.stack.stack, @basics^[i].stack[0], 4096);
          end;

          3:
          begin
            //fpu/stack
            copymemory(@c.{$ifdef cpu64}FltSave{$else}ext{$endif}, @extended^[i].fpudata, sizeof(c.{$ifdef cpu64}FltSave{$else}ext{$endif}));
            getmem(x.stack.stack, 4096);
            copymemory(x.stack.stack, @basics^[i].stack[0], 4096);
          end;
        end;

        x.context:=c;

        OutputDebugString('adding to the lists');
        s:=inttohex(address,8);

        li:=fca.changedlist.Items.add;
        li.caption:=s;
        li.SubItems.Add('');
        li.subitems.add('1');
        li.Data:=x;

        fca.addresslist.Add(address, x);
        fca.refetchValues(x.address);
      end
      else
      begin
        OutputDebugString(fca.equation+' did not end up into an address');
      end;
    end;

  except
    on e: exception do
      outputdebugstring('TDBVMWatchPollThread.addEntriesToList error: '+e.message);
  end;
end;

procedure TDBVMWatchExecutePollThread.execute;
var
  i: integer;
  size: integer;
begin
  cr3disassembler:=TCR3Disassembler.create;
  getmem(results,4096);
  resultsize:=4096;
  zeromemory(results,resultsize);

  try
    while not terminated do
    begin
      size:=resultsize;
      i:=dbvm_watch_retrievelog(id, results, size);
      if i=0 then
      begin
        //OutputDebugString('TDBVMWatchExecutePollThread returned 0.  results^.numberOfEntries='+inttostr(results^.numberOfEntries));

        //process data
        if results^.numberOfEntries>0 then
        begin
          //OutputDebugString('calling addEntriesToList');
          synchronize(addEntriesToList);
          sleep(10);
        end
        else
          sleep(50);
      end
      else
      if i=2 then
      begin
        //not enough memory. Allocate twice the needed amount
        outputdebugstring(inttostr(resultsize)+' is too small for the buffer. It needs to be at least '+inttostr(size));
        freememandnil(results);


        resultsize:=size*2;
        getmem(results, resultsize);
        zeromemory(results,resultsize);

        continue; //try again, no sleep
      end else
      begin
        outputdebugstring('dbvm_watch_retrievelog returned '+inttostr(i)+' which is not supported');
        exit;
      end;
    end;
  except
    on e: exception do
      outputdebugstring('TDBVMWatchExecutePollThread crash:'+e.Message);
  end;

  freememandnil(results);
  freeandnil(cr3disassembler);
end;

//-----------------------------

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
        refetchValues(x.address,true);
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
  stopdbvmwatch;

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
  value:='';
  if changedlist.ItemIndex<>-1 then
    value:=changedlist.Items[changedlist.ItemIndex].SubItems[0];

  if InputQuery(rsValueChange, rsGiveTheNewValue, value)=false then exit;


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

    ClientWidth:=max(clientwidth, integer(changedlist.Column[0].Width+changedlist.Column[1].Width+changedlist.Column[2].Width+20));
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

procedure TfrmChangedAddresses.refetchValues(specificaddress: ptruint=0; countonly: boolean=false);
var i: integer;
    s: string;
    handled: boolean;
    startindex: integer;
    stopindex: integer;
begin
  s:='';
  if changedlist.Items.Count>0 then
  begin
    if Changedlist.TopItem=nil then exit;

    startindex:=Changedlist.TopItem.Index;
    stopindex:=min(Changedlist.TopItem.Index+changedlist.VisibleRowCount, Changedlist.Items.Count-1);

    for i:=startindex to stopindex do
    begin

      if (specificaddress<>0) and (TAddressEntry(changedlist.items[i].Data).address <> specificaddress) then //check if this is the line to update, if not, don't read and parse
        continue;

      if countonly=false then
      begin
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

      end;



      while Changedlist.Items[i].SubItems.Count<2 do
        Changedlist.Items[i].SubItems.Add('');


      changedlist.items[i].SubItems[1]:=inttostr(TAddressEntry(changedlist.items[i].Data).count);


      if countonly=false then
        Changedlist.Items[i].SubItems[0]:=s;
    end;
  end;
end;

procedure TfrmChangedAddresses.Timer1Timer(Sender: TObject);
var
  c: integer;
  s: string;

begin
  refetchValues;
  c:=changedlist.Items.Count;

  if c=1 then
    s:=rsCodeAccessesSingleAddress
  else
    s:=format(rsCodeAccessesAddresses, [c]);

  lblInfo.caption:=s;

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
  stopDBVMWatch();

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
  fdbvmwatchid:=-1;
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

procedure TfrmChangedAddresses.stopdbvmwatch;
begin
  if dbvmwatchpollthread<>nil then
  begin
    dbvmwatchpollthread.Terminate;
    dbvmwatchpollthread.WaitFor;
    freeandnil(dbvmwatchpollthread);
  end;

  if dbvmwatchid<>-1 then
  begin
    dbvm_watch_delete(dbvmwatchid);
    dbvmwatchid:=-1;
  end;

  if dbvmwatch_unlock<>0 then
  begin
    UnlockMemory(dbvmwatch_unlock);
    dbvmwatch_unlock:=0;
  end;
end;

procedure TfrmChangedAddresses.setdbvmwatchid(id: integer);
begin
  fdbvmwatchid:=id;

  if id<>-1 then
  begin
    if dbvmwatchpollthread<>nil then
      freeandnil(dbvmwatchpollthread);

    dbvmwatchpollthread:=TDBVMWatchExecutePollThread.Create(true);
    dbvmwatchpollthread.id:=id;
    dbvmwatchpollthread.fca:=self;
    dbvmwatchpollthread.Start;
  end;
end;

initialization
  {$i formChangedAddresses.lrs}

end.

