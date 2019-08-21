unit PointerscannerSettingsFrm;

{$MODE Delphi}
{$warn 3057 off}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, LResources, EditBtn, Buttons, Contnrs,
  CEFuncProc, NewKernelHandler, symbolhandler, multilineinputqueryunit,
  registry, resolve, math, PointerscanSettingsIPConnectionList, types, commonTypeDefs;


type
  TPointerFileEntry=class(TCustomPanel)
  private
    addresslist: TStringlist;

    fimagelist: Timagelist;
    ffilename: string;
    fOnDelete: TNotifyEvent;
    fOnSetfilename: TNotifyEvent;
    lblFilename: TLabel;
    btnSetFile: TSpeedButton;
    btnDelete: TSpeedButton;
    cbAddress: TComboBox;
    procedure btnSetFileClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure setFileName(filename: string);
    procedure cbAddressDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  public
    property filename: string read ffilename write setFileName;
    property OnDelete: TNotifyEvent read fOnDelete write fOnDelete;
    property OnSetFileName: TNotifyEvent read fOnSetFileName write fOnSetFileName;
    constructor create(imagelist: TImageList; AOwner: TComponent);
    destructor destroy; override;
  end;


type
  //TPointerFileEntries = TFPGList<TPointerFileEntry>;

  TPointerFileList=class(TPanel)
  private
    lblFilenames: TLabel;
    lblAddress: TLabel;
    fimagelist: TImageList;
    fOnEmptyList: TNotifyEvent;
    Entries: TList;

    function getCount: integer;
    function getFilename(index: integer): string;
    function getAddress(index: integer): ptruint;
    procedure DeleteEntry(sender: TObject);
    procedure FilenameUpdate(sender: TObject);
    function AddEntry: TPointerFileEntry;
  private
    procedure AdjustPos(sender: tobject);
  public
    constructor create(imagelist: TImageList; AOwner: TComponent; w: integer);
    destructor destroy; override;

    property OnEmptyList: TNotifyEvent read fOnEmptyList write fOnEmptyList;
    property Count: integer read getCount;
    property filenames[index: integer]: string read getFilename;
    property addresses[index: integer]: ptruint read getAddress;
  end;

type tmoduledata = class
  public
    moduleaddress: dword;
    modulesize: dword;
  end;

type TOffsetEntry=class(Tedit)
  private
    function getOffset: dword;
    procedure setOffset(x: dword);
  protected
    procedure KeyPress(var Key: Char); override;
  public
    constructor create(AOwner: TComponent); override;
  published
    property offset: dword read getOffset write setOffset;
end;

type

  { TfrmPointerScannerSettings }

  TfrmPointerScannerSettings = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    cbAcceptNonModuleVtable: TCheckBox;
    CbAlligned: TCheckBox;
    cbAllowRuntimeWorkers: TCheckBox;
    cbClassPointersOnly: TCheckBox;
    cbCompressedPointerscanFile: TCheckBox;
    cbConnectToNode: TCheckBox;
    cbHeapOnly: TCheckBox;
    cbIncludeSystemModules: TCheckBox;
    cbLimitScanToRegionFile: TCheckBox;
    cbMaxOffsetsPerNode: TCheckBox;
    cbMustEndWithSpecificOffset: TCheckBox;
    cbMustStartWithBase: TCheckBox;
    cbNoLoop: TCheckBox;
    cbNoReadOnly: TCheckBox;
    cbOnlyOneStatic: TCheckBox;
    cbStackOnly: TCheckBox;
    cbStaticOnly: TCheckBox;
    cbStaticStacks: TCheckBox;
    cbUseHeapData: TCheckBox;
    cbUseLoadedPointermap: TCheckBox;
    cbCompareToOtherPointermaps: TCheckBox;
    cbShowAdvancedOptions: TCheckBox;
    cbAddress: TComboBox;
    cbNegativeOffsets: TCheckBox;
    ComboBox1: TComboBox;
    editMaxLevel: TEdit;
    editStructsize: TEdit;
    edtBaseFrom: TEdit;
    edtBaseTo: TEdit;
    edtDistributedPassword: TEdit;
    edtDistributedPort: TEdit;
    edtMaxOffsetsPerNode: TEdit;
    edtReverseStart: TEdit;
    edtReverseStop: TEdit;
    edtStackSize: TEdit;
    edtThreadcount: TEdit;
    edtThreadStacks: TEdit;
    il: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label9: TLabel;
    lblNumberOfStackThreads: TLabel;
    lblPort: TLabel;
    lblStackSize: TLabel;
    odLoadPointermap: TOpenDialog;
    odLoadRegionFile: TOpenDialog;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel3: TPanel;
    cbValueType: TComboBox;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    rbGeneratePointermap: TRadioButton;
    rbFindAddress: TRadioButton;
    rbFindValue: TRadioButton;
    procedure btnOkClick(Sender: TObject);

    procedure canNotReuse(Sender: TObject);
    procedure cbAddressDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure cbLimitScanToRegionFileChange(Sender: TObject);
    procedure cbMustStartWithBaseChange(Sender: TObject);
    procedure cbConnectToNodeChange(Sender: TObject);
    procedure cbAllowRuntimeWorkersChange(Sender: TObject);
    procedure cbMaxOffsetsPerNodeChange(Sender: TObject);
    procedure cbMustEndWithSpecificOffsetChange(Sender: TObject);
    procedure cbNegativeOffsetsChange(Sender: TObject);
    procedure cbShowAdvancedOptionsChange(Sender: TObject);
    procedure cbStaticOnlyChange(Sender: TObject);
    procedure cbStaticStacksChange(Sender: TObject);
    procedure cbUseLoadedPointermapChange(Sender: TObject);
    procedure cbCompareToOtherPointermapsChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbMustEndWithSpecificOffsetClick(Sender: TObject);
    procedure cbUseHeapDataClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure rbFindValueClick(Sender: TObject);
    procedure edtAddressChange(Sender: TObject);
    procedure cbHeapOnlyClick(Sender: TObject);
  private
    { Private declarations }
    //iplist: tstringlist;
    firstshow: boolean;

    mainaddressList: TStringlist;

    warnedAboutDisablingInstantRescan: boolean;


    procedure iplistResize(Sender: TObject);
    procedure iplistWantedToDeleteLastItem(Sender: TObject);
    procedure btnAddClick(sender: TObject);
    procedure btnRemoveClick(sender: TObject);
    procedure updatepositions;
    procedure PointerFileListEmpty(sender: TObject);
    procedure PointerFileListResize(sender: TObject);
    procedure UpdateGuiBasedOnSavedPointerScanUsage;
  public
    { Public declarations }
    reverse: boolean; //indicates to use the reverse method
    start:ptrUint;
    stop: ptrUint;
    unalligned: boolean;
    automaticaddress: ptrUint;
    structsize: integer;
    maxOffsetsPerNode: integer;
    maxlevel: integer;
    codescan: boolean;
    threadcount: integer;


    baseAddressRange: TComponentList;

    offsetlist: TComponentList;
    btnAddOffset: TButton;
    btnRemoveOffset: TButton;
    lblInfoLastOffset: TLabel;

    threadstacks: integer;
    stacksize: integer;
    scannerpriority: TThreadPriority;
    distributedport: word;

    baseStart: ptruint;
    baseStop: ptruint;

    iplist: TIpList;
{    resolvediplist: array of THostAddr;
    }
    pdatafilelist: TPointerFileList;
  end;

var frmpointerscannersettings: tfrmpointerscannersettings;

implementation

uses MainUnit, frmMemoryAllocHandlerUnit, MemoryBrowserFormUnit, ProcessHandlerUnit,
  Globals, parsers, DPIHelper;



resourcestring
  rsAdd = 'Add';
  rsRemove = 'Remove';
  rsIdle = 'Idle';
  rsLowest = 'Lowest';
  rsLower = 'Lower';
  rsNormal = 'Normal';
  rsHigher = 'Higher';
  rsHighest = 'Highest';
  rsTimeCritical = 'TimeCritical';

  strMaxOffsetsIsStupid = 'Sorry, but the max offsets should be 1 or higher, or else disable the checkbox'; //'Are you a fucking retard?';
  rsUseLoadedPointermap = 'Use saved pointermap';

  rsNoCompareFiles = 'If you do not use the compare results with other saved pointermap option you will get billions of useless results and giga/terrabytes of wasted diskspace and rescans will take hours if not days. Are you sure ?';
  rsSelectAFile = '<Select a file>';
  rsScandataFilter = 'All files (*.*)|*.*|Scan Data (*.scandata)|*.scandata';
  rsReusedTheSameFile = 'This file is already in the list of scandata files to be used'; //alternatively: 'For fucks sake dude. You already picked this file. Pick something else!'
  rsFilename = 'Filename';
  rsAddress = 'Address';
  rsInvalidAddress = 'Invalid address';
  rsPleaseFillInAnAddressToLookFor = 'Please fill in an address to look for';
  rsCouldNotBeResolvedToAnIPAddress = 'could not be resolved to an IP address';
  rsHasAnInvalidPort = '%s has an invalid port (%s)';
  rsFrom = 'From';
  rsTo = 'To';
  rsLastOffset = 'Last offset';
  rsHasNotBeenGivenAValidAddress = '%s has not been given a valid address';
  rsLimitScanToSpecifiedRegionFile = 'Limit scan to specified region file';


//helper
procedure UpdateAddressList(combobox: TCombobox);
var
  i: integer;
  maxwidth: integer;
  list: tstringlist;
begin
  list:=tstringlist(combobox.tag);
  if list=nil then exit;

  combobox.Items.Clear;

  maxwidth:=combobox.clientwidth-combobox.Left;

  for i:=0 to list.count-1 do
  begin
    combobox.items.Add(list.Names[i]);
    maxwidth:=max(maxwidth, combobox.Canvas.TextWidth(list[i]));
  end;

  combobox.DropDownCount:=max(16, combobox.Items.Count);

  SendMessage(combobox.Handle, CB_SETDROPPEDWIDTH, maxwidth+10, 0);
end;





//-----------TPointerFileEntry--------------

procedure TPointerFileEntry.cbAddressDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  s: string;
  c: TCombobox;
  l: TStrings;
begin
  c:=tcombobox(control);
  l:=tstrings(c.tag);

  c.Canvas.FillRect(ARect);
  s:=l.Strings[Index];
  c.Canvas.TextOut(ARect.Left, ARect.Top, s);
end;

constructor TPointerFileEntry.create(imagelist: TImageList; AOwner: TComponent);
var bm: tbitmap;
begin
  inherited create(Aowner);

  AutoSize:=true;

  fimagelist:=imagelist;

  bevelouter:=bvNone;

  lblFileName:=TLabel.create(self);
  lblFileName.OnClick:=btnSetFileClick;
  lblFilename.Cursor:=crHandPoint;

  btnSetFile:=TSpeedButton.Create(self);
  btnSetFile.OnClick:=btnSetFileClick;

  btnDelete:=TSpeedButton.Create(self);
  btnDelete.OnClick:=btnDeleteClick;

  cbAddress:=TComboBox.Create(self);
  cbAddress.Enabled:=false;

  btnDelete.Parent:=self;
  btnDelete.AnchorSideRight.Side:=asrRight;
  btnDelete.AnchorSideRight.Control:=self;
  btnDelete.Anchors:=[aktop, akRight];
  btnDelete.BorderSpacing.Right:=4;



  bm:=tbitmap.Create;
  imagelist.GetBitmap(0, bm);
  btnDelete.Glyph:=bm;
  bm.free;

  cbAddress.parent:=self;
  cbAddress.AnchorSideRight.Control:=btnDelete;
  cbAddress.AnchorSideRight.side:=asrLeft;
  cbAddress.Constraints.MinWidth:=TPointerFileList(aowner).canvas.TextWidth(' DDDDDDDDDDDDDDDD ');
  //cbAddress.clientwidth:=tcustomform(aowner).canvas.TextWidth('DDDDDDDDDDDD');
  cbAddress.anchors:=[aktop, akright];
  cbAddress.BorderSpacing.Right:=8;
  cbAddress.style:=csOwnerDrawFixed;
  cbAddress.OnDrawItem:=cbAddressDrawItem;
  cbAddress.ItemHeight:=TfrmPointerScannerSettings(TPointerFileList(aowner).Owner).cbAddress.ItemHeight;
  //cbAddress.Height:=btnDelete.Height;

  Addresslist:=tstringlist.create;
  Addresslist.NameValueSeparator:='=';

  cbAddress.Tag:=ptrint(Addresslist);


  btnsetfile.parent:=self;
  btnSetFile.AnchorSideRight.control:=cbAddress;
  btnSetFile.AnchorSideRight.side:=asrLeft;
  btnSetFile.Anchors:=[aktop, akright];
  btnSetFile.BorderSpacing.Right:=8;

  bm:=tbitmap.Create;
  imagelist.GetBitmap(1, bm);
  btnSetFile.Glyph:=bm;

  bm.free;


  lblFilename.parent:=self;
 // lblFilename.AutoSize:=false;
  lblFilename.AnchorSideRight.control:=btnsetfile;
  lblFilename.AnchorSideRight.side:=asrleft;
  lblFilename.AnchorSideLeft.control:=self;
  lblFilename.AnchorSideLeft.side:=asrleft;
  lblFilename.AnchorSideTop.Control:=btnSetFile;
  lblfilename.AnchorSideTop.Side:=asrCenter;

  lblFilename.BorderSpacing.Right:=8;
  lblFilename.BorderSpacing.Left:=4;


  lblfilename.anchors:=[aktop, akleft, akright];
  lblFilename.Caption:=rsSelectAFile;

  height:=cbAddress.Height+2;




  DPIHelper.AdjustSpeedButtonSize(btnSetFile);
  DPIHelper.AdjustSpeedButtonSize(btnDelete);


end;

destructor TPointerFileEntry.destroy;
begin
  if addresslist<>nil then
    addresslist.free;

  inherited destroy;
end;

procedure TPointerFileEntry.btnSetFileClick(Sender: TObject);
var
  od: TOpenDialog;
  l: TPointerFileList;
  e: TPointerFileEntry;
  s: TfrmPointerScannerSettings;
  i: integer;


begin
  od:=TOpenDialog.Create(self);
  try
    od.DefaultExt:='.scandata';
    od.Filter:=rsScandataFilter;
    od.FilterIndex:=2;
    od.filename:=filename;
    if od.execute then
    begin
      //check if this filename is present in one of the other entries, or main entry
      l:=TPointerFileList(parent);

      for i:=0 to l.Entries.Count-1 do
      begin
        e:=TPointerFileEntry(l.Entries[i]);
        if (e<>self) and (e.filename=od.FileName) then
          raise exception.create(rsReusedTheSameFile);
      end;

      if l.Owner is TfrmPointerScannerSettings then
      begin
        s:=TfrmPointerScannerSettings(l.owner);
        if s.cbUseLoadedPointermap.checked and (s.odLoadPointermap.FileName=od.FileName) then
          raise exception.create(rsReusedTheSameFile);
      end;

      filename:=od.filename;
      cbAddress.Enabled:=true;
    end;
  finally
    od.free;
  end;
end;

procedure TPointerFileEntry.btnDeleteClick(Sender: TObject);
begin
  if assigned(OnDelete) then
    OnDelete(self);
end;

procedure TPointerFileEntry.setFileName(filename: string);
begin
  ffilename:=filename;
  lblfilename.caption:=extractfilename(filename);
  lblfilename.Hint:=filename;
  lblfilename.ShowHint:=true;

  if fileexists(filename+'.addresslist') then
    tstrings(cbAddress.tag).LoadFromFile(filename+'.addresslist',true);

  UpdateAddressList(cbAddress);

  if assigned(fonsetfilename) then
    fonSetFileName(self);
end;





procedure TPointerFileList.DeleteEntry(sender: TObject);
var
  e: TPointerFileEntry;
  i: integer;

  before, after: TControl;
begin

  e:=TPointerFileEntry(sender);
  i:=entries.IndexOf(e);

  if (i<>0) and (i=entries.count-1) and (TPointerFileEntry(entries[i]).filename='') then exit; //don't delete the last one if there asre entries

  if entries.Count>i+1 then
  begin
    before:=e.AnchorSideTop.Control;
    after:=TPointerFileEntry(entries[i+1]);

    after.AnchorSideTop.Control:=before;
  end;


  entries.Delete(i);
  e.Free;

  if entries.count=0 then
  begin
    if assigned(fOnEmptyList) then
      fOnEmptyList(self);
  end;
end;

procedure TPointerFileList.FilenameUpdate(sender: TObject);
var i: integer;
begin
  //check if there is a empty line, and if not, add a new one
  for i:=0 to entries.count-1 do
    if TPointerFileEntry(entries[i]).filename='' then exit;

  //no empty line. Add a new one
  AddEntry;
end;

function TPointerFileList.addentry:TPointerFileEntry;
var e: TPointerFileEntry;
begin
  e:=TPointerFileEntry.create(fimagelist, self);
  e.Anchors:=[akTop, akLeft, akRight];
  e.parent:=self;

 // e.color:=clBlue;

  if entries.Count=0 then
    e.AnchorSideTop.Control:=lblFilenames
  else
    e.AnchorSideTop.Control:=TPointerFileEntry(entries[entries.count-1]);

  e.AnchorSideTop.side:=asrBottom;
  e.AnchorSideRight.control:=self;
  e.AnchorSideRight.Side:=asrRight;

  e.AnchorSideLeft.Control:=self;
  e.AnchorSideLeft.Side:=asrLeft;

  e.BorderSpacing.Top:=2;

  e.OnDelete:=DeleteEntry;
  e.OnSetFileName:=FilenameUpdate;

  entries.Add(e);
  result:=e;


end;

function TPointerFileList.getFilename(index: integer): string;
begin
  if (index>=0) and (index<count) then
    result:=TPointerFileEntry(entries[index]).filename
  else
    result:='';
end;

function TPointerFileList.getAddress(index: integer): ptruint;
begin
  if filenames[index]<>'' then
  begin
    try
      result:=StrToQWord('$'+TPointerFileEntry(entries[index]).cbAddress.Text);
    except
      raise exception.create(Format(rsHasNotBeenGivenAValidAddress, [filenames[index]]));
    end;
  end;
end;

function TPointerFileList.getCount: integer;
begin
  result:=entries.count;
end;


procedure TPointerFileList.AdjustPos(sender: tobject);
var e: TPointerFileEntry;
begin
  if entries.count>0 then
  begin
    e:=TPointerFileEntry(Entries[0]);
    lblFilenames.left:=e.lblFilename.Left;
    lblAddress.left:=e.cbAddress.Left+(e.cbAddress.width div 2)-(lbladdress.width div 2);
  end;
end;

constructor TPointerFileList.create(imagelist: TImageList; AOwner: TComponent; w: integer);
var e: TPointerFileEntry;
begin
  fimagelist:=imagelist;
  inherited create(AOwner);


  autosize:=true;

 // color:=clgreen;

  if aowner is twincontrol then
  begin
    width:=w;
    parent:=twincontrol(aowner);
  end;

  bevelouter:=bvNone;


  entries:=Tlist.create;
  lblFilenames:=TLabel.create(self);
  lblFilenames.caption:=rsFilename;
  lblFilenames.parent:=self;

  lblAddress:=TLabel.create(self);
  lblAddress.caption:=rsAddress;
  lblAddress.parent:=self;

  lblAddress.AnchorSideTop.Control:=self;
  lblAddress.AnchorSideTop.Side:=asrTop;
  lblFilenames.AnchorSideTop.Control:=self;
  lblFilenames.AnchorSideTop.Side:=asrTop;

  e:=AddEntry;

  e.OnResize:=AdjustPos;
end;

destructor TPointerFileList.destroy;
var i: integer;
begin
  for i:=0 to entries.count-1 do
    TPointerFileEntry(entries[i]).Free;

  entries.free;

  inherited destroy;
end;

//------------TOffsetEntry-------------------

constructor TOffsetEntry.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  text:='0';

  width:=50;
end;

procedure TOffsetEntry.KeyPress(var Key: Char);
begin
  if key in ['A'..'F', 'a'..'f','0'..'9','+','-',#0..#31] then
    inherited KeyPress(key)
  else
    key:=#0;
end;

function TOffsetEntry.getOffset: dword;
var o: integer;
begin
  if TryStrToInt('$'+text, o) then result:=o else result:=0;
end;

procedure TOffsetEntry.setOffset(x: dword);
begin
  text:=inttohex(x,1);
end;

procedure TfrmPointerScannerSettings.btnOkClick(Sender: TObject);
var
  i,j: integer;
  r: THostResolver=nil;
  p: ptruint=0;
  comparecount: integer=0;
  reg: TRegistry=nil;
begin
  if cbMaxOffsetsPerNode.checked then
  begin
    maxOffsetsPerNode:=strtoint(edtMaxOffsetsPerNode.text);
    if maxOffsetsPerNode<=0 then
    begin
      MessageDlg(strMaxOffsetsIsStupid, mtError, [mbok], 0);
      exit;
    end;
  end;

  if cbCompareToOtherPointermaps.checked then
  begin
    //check if the addresses are valid
    try
      for i:=0 to pdatafilelist.Count-1 do
      begin
        if pdatafilelist.filenames[i]<>'' then
        begin
          p:=pdatafilelist.addresses[i];
          inc(comparecount);
        end;
      end;
    except
      on e:exception do
      begin
        MessageDlg(e.Message, mtError, [mbok], 0);
        exit;
      end;
    end;

    if (rbGeneratePointermap.checked=false) and (comparecount=0) then
    begin
      //bug the user one time about this
      if (not warnedAboutDisablingInstantRescan) and (MessageDlg(rsNoCompareFiles, mtWarning, [mbyes, mbno], 0)<>mryes) then
        exit;

      warnedAboutDisablingInstantRescan:=true;
    end;
  end
  else
  begin
    if (rbGeneratePointermap.checked=false) and (cbMustStartWithBase.Checked=false) then
    begin
      if  (not warnedAboutDisablingInstantRescan) and (MessageDlg(rsNoCompareFiles, mtWarning, [mbyes, mbno], 0)<>mryes) then
        exit;

      warnedAboutDisablingInstantRescan:=true;
    end;
  end;

  start:=StrToQWordEx('$'+edtReverseStart.text);
  stop:=StrToQWordEx('$'+edtReverseStop.text);


  if cbMustStartWithBase.checked then
  begin
    baseStart:=symhandler.getAddressFromName(edtBaseFrom.text);
    baseStop:=symhandler.getAddressFromName(edtBaseTo.text);
  end;



  if rbFindAddress.checked then
  begin
    try
      automaticaddress:=symhandler.getAddressFromName(cbAddress.text);
    except
      on e:exception do
      begin
        if cbAddress.text='' then
          MessageDlg(rsPleaseFillInAnAddressToLookFor, mtError, [mbok], 0)
        else
          MessageDlg(rsInvalidAddress+' ('+cbAddress.text+')', mtError, [mbok], 0);
        exit;
      end;
    end;
  end;

  unalligned:=not cballigned.checked;

  structsize:=strtoint(editstructsize.text);
  maxlevel:=strtoint(editMaxLevel.text)+1;

  codescan:=false;

  threadcount:=strtoint(edtthreadcount.text);
  case combobox1.itemindex of
    0: scannerpriority:=tpIdle;
    1: scannerpriority:=tpLowest;
    2: scannerpriority:=tpLower;
    3: scannerpriority:=tpNormal;
    4: scannerpriority:=tpHigher;
    5: scannerpriority:=tpHighest;
    6: scannerpriority:=tpTimeCritical;
  end;

  if cbStaticStacks.checked then
  begin
    threadstacks:=strtoint(edtThreadStacks.text);
    stacksize:=strtoint(edtStackSize.text);
  end;

  distributedport:=strtoint(edtDistributedPort.text);


  if cbConnectToNode.checked then
  begin
    r:=THostResolver.create(nil);
    r.RaiseOnError:=false;

    for i:=0 to iplist.count-1 do
      if (iplist[i].host<>'') then
      begin
        r.NameLookup(iplist[i].host);
        if r.HostAddress.s_addr=0 then
        begin
          MessageDlg(iplist[i].host+' '+rsCouldNotBeResolvedToAnIPAddress,  mtError, [mbok], 0);
          exit;
        end;

        if TryStrToInt(iplist[i].port, j)=false then
        begin
          MessageDlg(Format(rsHasAnInvalidPort, [iplist[i].host, iplist[i].port]), mtError, [mbok], 0);
          exit;
        end;
      end;

    r.free;
  end;

  modalresult:=mrok;
end;

procedure TfrmPointerScannerSettings.canNotReuse(Sender: TObject);
begin
  cbAcceptNonModuleVtable.enabled:=cbClassPointersOnly.checked;
end;

procedure TfrmPointerScannerSettings.cbAddressDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var s: string;
begin
  cbAddress.Canvas.FillRect(ARect);
  s:=mainaddressList.Strings[Index];
  cbAddress.Canvas.TextOut(ARect.Left, ARect.Top, s);
end;



procedure TfrmPointerScannerSettings.cbMustStartWithBaseChange(Sender: TObject);
begin
  panel12.visible:=cbMustStartWithBase.checked;
  updatepositions;

  if cbMustStartWithBase.checked then
    cbStaticOnly.Checked:=true;
end;

procedure TfrmPointerScannerSettings.iplistResize(Sender: TObject);
begin
  updatepositions;
end;

procedure TfrmPointerScannerSettings.iplistWantedToDeleteLastItem(Sender: TObject);
begin
  cbConnectToNode.checked:=false;
end;

procedure TfrmPointerScannerSettings.cbConnectToNodeChange(Sender: TObject);
begin
  if cbConnectToNode.checked then
    iplist.visible:=true
  else
    iplist.visible:=false;

  updatepositions;
end;

procedure TfrmPointerScannerSettings.cbAllowRuntimeWorkersChange(Sender: TObject);
begin
  edtDistributedPort.enabled:=cbAllowRuntimeWorkers.checked;
  edtDistributedPassword.enabled:=cbAllowRuntimeWorkers.checked;
end;

procedure TfrmPointerScannerSettings.cbMaxOffsetsPerNodeChange(Sender: TObject);
begin
  edtMaxOffsetsPerNode.enabled:=cbMaxOffsetsPerNode.checked;
end;

procedure TfrmPointerScannerSettings.cbMustEndWithSpecificOffsetChange(Sender: TObject);
var offsetentry: TOffsetEntry;
  i: integer;
begin
  //create an offset entry
  if (sender as tcheckbox).Checked then //create the first one and the add button
  begin
    offsetentry:=TOffsetEntry.Create(self);

    offsetlist:=TComponentList.create;
    offsetlist.Add(offsetentry);


    with offsetentry do
    begin
      offsetentry.Name:='edtOffset'+inttostr(offsetlist.Count);
      AnchorSideTop.Control:=cbMustEndWithSpecificOffset;
      AnchorSideTop.Side:=asrBottom;
      AnchorSideLeft.Control:=cbMustEndWithSpecificOffset;
      AnchorSideLeft.Side:=asrLeft;
      BorderSpacing.Left:=15;

      parent:=panel10;
    end;

    if lblInfoLastOffset=nil then
      lblInfoLastOffset:=TLabel.Create(self);

    with lblInfoLastOffset do
    begin
      caption:=rsLastOffset;
      AnchorSideLeft.Control:=offsetentry;
      AnchorSideLeft.Side:=asrRight;
      BorderSpacing.Left:=5;

      parent:=panel10;
      visible:=false;
    end;

    if btnAddOffset=nil then
      btnAddOffset:=TButton.Create(self);

    with btnAddOffset do
    begin
      name:='btnAddOffset';
      caption:=rsAdd;
      AnchorSideLeft.Control:=offsetentry;
      AnchorSideLeft.Side:=asrRight;
      AnchorSideTop.Control:=offsetentry;
      AnchorSideTop.Side:=asrTop;

      BorderSpacing.Left:=3;
      constraints.MinWidth:=60;
      autosize:=true;

      parent:=panel10;
      onclick:=btnAddClick;
      visible:=true;
    end;

    if btnRemoveOffset=nil then
      btnRemoveOffset:=TButton.Create(self);

    with btnRemoveOffset do
    begin
      name:='btnRemoveOffset';
      caption:=rsRemove;
      AnchorSideLeft.Control:=btnAddOffset;
      AnchorSideLeft.Side:=asrRight;
      AnchorSideTop.Control:=btnAddOffset;
      AnchorSideTop.Side:=asrTop;
      BorderSpacing.Left:=3;

      constraints.MinWidth:=60;
      autosize:=true;

      parent:=panel10;
      onclick:=btnRemoveClick;
      visible:=true;
    end;

    btnAddOffset.AutoSize:=false;
    btnRemoveOffset.autosize:=false;
    i:=max(btnAddOffset.Width, btnRemoveOffset.width);
    btnAddOffset.Width:=i;
    btnRemoveOffset.Width:=i;

  end
  else
  begin
    //delete all the objects in the list
    //clientheight:=cbMustEndWithSpecificOffset.top+cbMustEndWithSpecificOffset.Height+2+panel1.height;
    freeandnil(offsetlist); //deletes all the assigned objects
    btnAddOffset.Visible:=false;
    btnRemoveOffset.Visible:=false;
    lblInfoLastOffset.Visible:=false;
  end;

  updatepositions;

end;

procedure TfrmPointerScannerSettings.cbNegativeOffsetsChange(Sender: TObject);
begin
  cbCompressedPointerscanFile.enabled:=not cbNegativeOffsets.checked;
  if cbNegativeOffsets.checked then
    cbCompressedPointerscanFile.checked:=false;
end;

procedure TfrmPointerScannerSettings.cbShowAdvancedOptionsChange(Sender: TObject);
begin
  panel3.visible:=cbShowAdvancedOptions.checked;
  updatepositions;
end;

procedure TfrmPointerScannerSettings.cbStaticOnlyChange(Sender: TObject);
begin
  if cbStaticOnly.Checked=false then
    cbMustStartWithBase.Checked:=false;
end;

procedure TfrmPointerScannerSettings.cbLimitScanToRegionFileChange(
  Sender: TObject);
begin
  if cbLimitScanToRegionFile.Checked then
  begin
    cbLimitScanToRegionFile.OnChange:=nil;
    if odLoadRegionFile.execute then
    begin
      cbLimitScanToRegionFile.Caption:=rsLimitScanToSpecifiedRegionFile+' '+extractfilename(odLoadRegionFile.FileName);
    end
    else
    begin
      cbLimitScanToRegionFile.Checked:=false;
      cbLimitScanToRegionFile.Caption:=rsLimitScanToSpecifiedRegionFile;
    end;
    cbLimitScanToRegionFile.OnChange:=cbLimitScanToRegionFileChange;
  end;
end;

procedure TfrmPointerScannerSettings.cbUseLoadedPointermapChange(Sender: TObject);
begin
  if cbUseLoadedPointermap.checked then
  begin
    cbUseLoadedPointermap.OnChange:=nil;
    try
      if odLoadPointermap.Execute then
      begin
        cbUseLoadedPointermap.Caption:=rsUseLoadedPointermap+':'+ExtractFileName(odLoadPointermap.FileName);

        if fileexists(odLoadPointermap.FileName+'.addresslist') then
        begin
          tstrings(cbAddress.tag).LoadFromFile(odLoadPointermap.FileName+'.addresslist', true);
          UpdateAddressList(cbAddress);
        end;

      end
      else
        cbUseLoadedPointermap.checked:=false;


    finally
      cbUseLoadedPointermap.OnChange:=cbUseLoadedPointermapChange;
    end;

  end
  else
    cbUseLoadedPointermap.Caption:=rsUseLoadedPointermap;

  UpdateGuiBasedOnSavedPointerScanUsage;



end;

procedure TfrmPointerScannerSettings.PointerFileListEmpty(sender: TObject);
begin
  cbCompareToOtherPointermaps.checked:=false;
end;


procedure TfrmPointerScannerSettings.PointerFileListResize(sender: TObject);
begin
  updatepositions;
end;

procedure TfrmPointerScannerSettings.cbCompareToOtherPointermapsChange(Sender: TObject);
begin
  if cbCompareToOtherPointermaps.checked then
  begin
    pdatafilelist:=TPointerFileList.create(il, self, cbShowAdvancedOptions.left-cbCompareToOtherPointermaps.left-8);
    pdatafilelist.Color:=clWindow;
    pdatafilelist.BevelOuter:=bvNone;
    pdatafilelist.BorderStyle:=bsSingle;
    pdatafilelist.AnchorSideTop.Control:=cbCompareToOtherPointermaps;
    pdatafilelist.AnchorSideTop.Side:=asrBottom;
    pdatafilelist.AnchorSideLeft.Control:=cbCompareToOtherPointermaps;
    pdatafilelist.AnchorSideLeft.Side:=asrLeft;

    //pdatafilelist.AnchorSideRight.Control:=self;
    //pdatafilelist.AnchorSideRight.Side:=asrRight;

    pdatafilelist.OnEmptyList:=PointerFileListEmpty;
    pdatafilelist.OnResize:=PointerFileListResize;

    pdatafilelist.Anchors:=[akTop, akLeft]; //, akRight];

    pdatafilelist.AutoSize:=true;
    pdatafilelist.DoAutoSize;
    pdatafilelist.AdjustPos(pdatafilelist);

    cbShowAdvancedOptions.AnchorSideTop.Control:=pdatafilelist;
  end
  else
  begin
    warnedAboutDisablingInstantRescan:=true;
    pdatafilelist.OnResize:=nil;
    pdatafilelist.OnEmptyList:=nil;
    pdatafilelist.visible:=false;
    pdatafilelist.free;
    pdatafilelist:=nil;

    cbShowAdvancedOptions.AnchorSideTop.Control:=cbCompareToOtherPointermaps;
  end;

  //UpdateGuiBasedOnSavedPointerScanUsage;
  updatepositions;
end;

procedure TfrmPointerScannerSettings.FormDestroy(Sender: TObject);
var
  reg: TRegistry;
  i: integer;
  oldlist: tstringlist;
begin
  if iplist<>nil then
  begin
    reg:=TRegistry.Create;
    reg.RootKey := HKEY_CURRENT_USER;


    if Reg.OpenKey('\Software\Cheat Engine\'+ClassName, true) then
    begin
      reg.WriteBool('Advanced', cbShowAdvancedOptions.checked);
      reg.WriteBool('warnedAboutDisablingInstantRescan', warnedAboutDisablingInstantRescan);

      if TryStrToInt(edtMaxOffsetsPerNode.text,i) then
      begin
        reg.WriteBool('MaxOffsetsPerNode Checked', cbMaxOffsetsPerNode.checked);
        reg.WriteInteger('MaxOffsetsPerNode Value', i);
      end;

    end;

    if Reg.OpenKey('\Software\Cheat Engine\PSNNodeList', false) then
    begin
      oldlist:=tstringlist.create;
      try
        reg.GetKeyNames(oldlist);
      except
      end;

      for i:=0 to oldlist.count-1 do
        reg.DeleteKey(oldlist[i]);

      oldlist.free;
    end;


    for i:=0 to iplist.count-1 do
    begin
      if iplist[i].host<>'' then
      begin
        if Reg.OpenKey('\Software\Cheat Engine\PSNNodeList\'+iplist[i].host+':'+iplist[i].port,true) then
        begin
          reg.WriteString('Password', iplist[i].password);
          reg.WriteBool('StableConnection', iplist[i].stable);
        end;
      end;
    end;

    reg.free;
    freeandnil(iplist);
  end;

  if mainaddressList<>nil then
    freeandnil(mainaddressList);
end;

procedure TfrmPointerScannerSettings.cbStaticStacksChange(Sender: TObject);
begin
  lblNumberOfStackThreads.enabled:=cbStaticStacks.checked;
  edtThreadStacks.enabled:=cbStaticStacks.checked;

  lblStackSize.enabled:=cbStaticStacks.checked;
  edtStackSize.enabled:=cbStaticStacks.checked;
  cbStackOnly.enabled:=cbStaticStacks.checked;

end;



procedure TfrmPointerScannerSettings.FormShow(Sender: TObject);
var
  cpucount: integer;
  i: integer;
begin
  cpucount:=GetCPUCount;

  i:=max(label4.width, label2.width);
  edtBaseFrom.Width:=cbMustStartWithBase.width-i;
  edtBaseTo.Width:=cbMustStartWithBase.width-i;

  //assumption: when a core with hyperthreading core is running at 100% it's hyperthreaded processor will be running at 90%
  //This means that 10 cores are needed to provide an equivalent for one extra core when hyperthreading is used
  //In short, leave the hyperhtreaded processors alone so the user can use that hardly useful processing power to surf the web or move the mouse...
  //(at most use one)
  if HasHyperthreading then
    cpucount:=ceil((cpucount / 2)+(cpucount / 4));


  rbFindValueClick(rbFindAddress);


  edtThreadcount.text:=inttostr(cpucount);


  //check what type of process it is
  if processhandler.is64Bit then
  begin
    edtReverseStart.maxlength:=16;
    edtReverseStop.MaxLength:=edtReverseStart.MaxLength;

    //if it's not edited by the user, then fill in the default ranges for 64-bit
    if edtReverseStart.text='00000000' then
      edtReverseStart.text:='0000000000000000';

    if (edtReverseStop.text='7FFFFFFF') or (edtReverseStop.text='FFFFFFFF') then
    begin
      edtReverseStop.text:='7FFFFFFFFFFFFFFF';
    end;
  end
  else
  begin
    //if it's not edited by the user, then fill in the default ranges for 32-bit
    if edtReverseStart.text='0000000000000000' then
      edtReverseStart.text:='00000000';

    if (edtReverseStop.text='7FFFFFFFFFFFFFFF') or (edtReverseStop.text='7FFFFFFF') then
    begin
      if Is64bitOS then
        edtReverseStop.text:='FFFFFFFF'
      else
        edtReverseStop.text:='7FFFFFFF';
    end;

    edtReverseStart.maxlength:=8;
    edtReverseStop.MaxLength:=edtReverseStart.MaxLength;
  end;

  i:=editMaxLevel.left+editMaxLevel.width;
  combobox1.width:=combobox1.width+(i-(combobox1.left+combobox1.width));


  i:=max(edtReverseStart.clientwidth, max(canvas.TextWidth(edtReverseStart.text), canvas.TextWidth(edtReverseStop.text)))+8;
  edtReverseStart.clientwidth:=i;
  edtReverseStop.clientwidth:=i;


  i:=max(canvas.TextWidth('XXXX')+DPIHelper.GetEditBoxMargins(editStructsize), editStructsize.clientwidth);
  editStructsize.clientwidth:=i;

  i:=max(btnOk.width, btnCancel.width);
  btnok.autosize:=false;
  btnCancel.autosize:=false;
  btnok.width:=i;
  btnCancel.width:=i;

  firstshow:=false;


  if cbUseLoadedPointermap.checked then
  begin
    //get the addresslist from the scandata.addresslist file (if it exists)
    if fileexists(odLoadPointermap.filename+'.addresslist') then
      tstrings(cbAddress.tag).LoadFromFile(odLoadPointermap.filename+'.addresslist', true);
  end
  else
    MainForm.addresslist.getAddressList(tstrings(cbAddress.tag));

  UpdateAddressList(cbAddress);
  AdjustComboboxSize(cbValueType, self.canvas);
  cbAddress.ItemHeight:=cbValueType.ItemHeight;
  cbAddress.height:=cbValueType.Height;


end;

procedure TfrmPointerScannerSettings.FormCreate(Sender: TObject);
var
  reg: tregistry;
  list: Tstringlist;
  i: integer;
  host, port: string;
begin
  ComboBox1.Items.Clear;
  with ComboBox1.items do
  begin
    add(rsIdle);
    add(rsLowest);
    add(rsLower);
    add(rsNormal);
    add(rsHigher);
    add(rsHighest);
    add(rsTimeCritical);
  end;

  ComboBox1.itemindex:=3;

  iplist:=TIpList.create(panel11);

  iplist.AnchorSideLeft.Control:=cbConnectToNode;
  iplist.AnchorSideLeft.side:=asrLeft;
  iplist.AnchorSideTop.Control:=cbConnectToNode;
  iplist.AnchorSideTop.Side:=asrBottom;
  iplist.AnchorSideRight.Control:=panel11;
  iplist.AnchorSideRight.Side:=asrRight;
  iplist.Anchors:=[akTop, akLeft, akRight];

  iplist.OnResize:=iplistResize;
  iplist.OnWantedToDeleteLastItem:=iplistWantedToDeleteLastItem;

  reg:=tregistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;

  if Reg.OpenKey('\Software\Cheat Engine\'+ClassName, false) then
  begin
    if reg.ValueExists('Advanced') then
      cbShowAdvancedOptions.checked:=reg.ReadBool('Advanced');

    if reg.ValueExists('warnedAboutDisablingInstantRescan') then
      warnedAboutDisablingInstantRescan:=reg.ReadBool('warnedAboutDisablingInstantRescan');

    if reg.ValueExists('MaxOffsetsPerNode Checked') then
      cbMaxOffsetsPerNode.checked:=reg.ReadBool('MaxOffsetsPerNode Checked');

    if reg.ValueExists('MaxOffsetsPerNode Value') then
      edtMaxOffsetsPerNode.Text:=inttostr(reg.ReadInteger('MaxOffsetsPerNode Value'));

  end;

  if Reg.OpenKey('\Software\Cheat Engine\PSNNodeList', false) then
  begin
    list:=tstringlist.create;
    try
      Reg.GetKeyNames(list);
    except
    end;


    for i:=0 to list.count-1 do
    begin
      if reg.OpenKey('\Software\Cheat Engine\PSNNodeList\'+list[i], false) then
      begin
        while iplist.count<=i do
          iplist.add;

        iplist[i].host:=copy(list[i], 1, pos(':', list[i])-1);
        iplist[i].port:=copy(list[i], pos(':', list[i])+1, length(list[i]));

        if reg.ValueExists('Password') then
          iplist[i].password:=reg.ReadString('Password');

        if reg.ValueExists('StableConnection') then
          iplist[i].stable:=reg.ReadBool('StableConnection');
      end;
    end;

    list.free;

  end;

  reg.free;
  firstshow:=true;

  mainaddressList:=tstringlist.create;
  mainaddresslist.NameValueSeparator:='=';

  cbAddress.tag:=ptruint(mainaddressList);
end;


procedure tfrmPointerScannerSettings.btnAddClick(sender: TObject);
var
  offsetentry: TOffsetEntry;
  before: TOffsetEntry;
begin
  before:=TOffsetEntry(offsetlist[offsetlist.count-1]);
  offsetentry:=TOffsetEntry.Create(self);
  offsetlist.Add(offsetentry);
    
  with offsetentry do
  begin
    offsetentry.Name:='edtOffset'+inttostr(offsetlist.Count);
    AnchorSideLeft.Control:=cbMustEndWithSpecificOffset;
    AnchorSideLeft.Side:=asrLeft;
    AnchorSideTop.Control:=before;
    AnchorSideTop.Side:=asrBottom;

    BorderSpacing.Left:=15;
    parent:=panel10;
  end;

  updatepositions;

  if offsetlist.count=2 then
  begin
    lblInfoLastOffset.visible:=true;
    lblInfoLastOffset.AnchorSideTop.Control:=TOffsetEntry(offsetlist[0]);
    lblInfoLastOffset.AnchorSideTop.Side:=asrCenter;
  end;

  btnAddOffset.AnchorSideTop.Control:=offsetentry;
end;

procedure tfrmPointerScannerSettings.btnRemoveClick(sender: TObject);
var before: TOffsetEntry;
begin


  offsetlist.delete(offsetlist.count-1);
  if offsetlist.count>0 then
  begin
    before:=TOffsetEntry(offsetlist[offsetlist.count-1]);
    btnAddOffset.AnchorSideTop.Control:=before;

    if offsetlist.count=1 then lblInfoLastOffset.visible:=false;
  end
  else
    cbMustEndWithSpecificOffset.checked:=false;

  updatepositions;

end;


procedure TfrmPointerScannerSettings.cbMustEndWithSpecificOffsetClick(Sender: TObject);
begin

end;

procedure TfrmPointerScannerSettings.cbUseHeapDataClick(Sender: TObject);
begin
  cbHeapOnly.Enabled:=cbUseHeapData.Checked;
  if (frmMemoryAllocHandler<>nil) and (frmMemoryAllocHandler.hookedprocessid<>processid) then
    freeandnil(frmMemoryAllocHandler);

  frmMemoryAllocHandler:=TfrmMemoryAllocHandler.Create(memorybrowser);
  frmMemoryAllocHandler.WaitForInitializationToFinish;

  edtAddressChange(cbAddress);
end;

procedure TfrmPointerScannerSettings.Panel1Click(Sender: TObject);
begin

end;

procedure TfrmPointerScannerSettings.rbFindValueClick(Sender: TObject);
var gpm: boolean;
  fv: boolean;
begin
  gpm:=rbGeneratePointermap.checked;
  fv:=rbFindValue.checked;

  cbCompareToOtherPointermaps.enabled:=not gpm;
  cbAddress.enabled:=not gpm;
  cbValueType.enabled:=not gpm;
  cbStaticOnly.enabled:=not gpm;
  cbOnlyOneStatic.enabled:=not gpm;
  cbUseHeapData.enabled:=not gpm;
  cbHeapOnly.enabled:=not gpm;
  cbNoLoop.enabled:=not gpm;
  cbMaxOffsetsPerNode.enabled:=not gpm;
  cbUseLoadedPointermap.enabled:=not gpm;
  cbMustStartWithBase.enabled:=not gpm;
  cbMustEndWithSpecificOffset.enabled:=not gpm;

  cbAllowRuntimeWorkers.enabled:=not gpm;
  cbConnectToNode.enabled:=not gpm;
  edtThreadcount.enabled:=not gpm;
  editStructsize.enabled:=not gpm;
  combobox1.enabled:=not gpm;
  editMaxLevel.enabled:=not gpm;

  cbCompressedPointerscanFile.enabled:=not gpm;

  label9.enabled:=not gpm;
  label3.enabled:=not gpm;
  label12.enabled:=not gpm;

  if rbFindAddress.Checked then
  begin
    cbAddress.visible:=true;
    cbAddress.Width:=clientwidth-cbAddress.left*2;
    cbValueType.Visible:=false;
  end
  else
  if rbFindValue.checked then
  begin
    cbAddress.visible:=true;
    cbAddress.Width:=cbValueType.left-cbAddress.Left-3;
    cbValueType.Visible:=true;
  end;

  if gpm then
    cbUseLoadedPointermap.checked:=false
  else
    cbAddress.SetFocus

end;

procedure TfrmPointerScannerSettings.edtAddressChange(Sender: TObject);
var haserror: boolean;
begin
  automaticaddress:=symhandler.getAddressFromName(cbAddress.text, false,haserror); //ignore error


  if cbHeapOnly.Checked then
  begin
   if (frmMemoryAllocHandler.FindAddress(@frmMemoryAllocHandler.HeapBaselevel, automaticaddress)<>nil) then
     cbAddress.Font.Color:=clGreen
   else
     cbAddress.Font.Color:=clRed; //BAD
  end else cbAddress.Font.Color:=clWindowText;

end;

procedure TfrmPointerScannerSettings.cbHeapOnlyClick(Sender: TObject);
begin
  edtAddressChange(cbAddress);
end;

procedure TfrmPointerScannerSettings.UpdateGuiBasedOnSavedPointerScanUsage;
begin
  //make rbFindValue enabled or disabled based on the current settings

  rbFindValue.enabled:=not (cbUseLoadedPointermap.checked);

  if rbFindValue.enabled=false then
    rbFindAddress.Checked:=true;

  if cbUseLoadedPointermap.checked then
  begin
    CbAlligned.enabled:=false;

    cbHeapOnly.checked:=false;
    cbHeapOnly.enabled:=false;

    cbUseHeapData.checked:=false;
    cbUseHeapData.enabled:=false;


    edtReverseStart.Enabled:=false;
    edtReverseStop.enabled:=false;

    cbMustStartWithBase.checked:=false;
    cbMustStartWithBase.enabled:=false;

    cbClassPointersOnly.checked:=false;
    cbClassPointersOnly.enabled:=false;


    cbAcceptNonModuleVtable.checked:=false;
    cbAcceptNonModuleVtable.enabled:=false;

    cbStaticStacks.enabled:=false;
  end
  else
  begin
    CbAlligned.enabled:=true;
    cbUseHeapData.enabled:=true;
    cbHeapOnly.enabled:=cbUseHeapData.checked;

    edtReverseStart.Enabled:=true;
    edtReverseStop.enabled:=true;
    cbMustStartWithBase.enabled:=true;
    cbClassPointersOnly.enabled:=true;
    cbAcceptNonModuleVtable.enabled:=cbClassPointersOnly.checked;
    cbStaticStacks.enabled:=true;;
  end;

  cbStaticStacksChange(cbStaticStacks);

end;

procedure TfrmPointerScannerSettings.updatepositions;
begin


  DoAutoSize;
end;

initialization
  {$i PointerscannerSettingsFrm.lrs}

end.





