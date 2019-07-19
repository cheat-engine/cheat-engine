unit frmstructurecompareunit;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  cefuncproc, newkernelhandler, frmStringMapUnit, MemFuncs, AvgLvlTree, Menus,
  bigmemallochandler, math, maps, oldRegExpr, symbolhandler, commonTypeDefs,
  pagemap, syncobjs2, syncobjs, Clipbrd;

const compareversion=1;

type

  { TfrmStructureCompare }
  TPointerpath=array of dword;
  TDiffkind=(dkDontCare, dkMustBeDifferent, dkMustBeSame);



type

  PMappedRegion=^TMappedRegion;
  TMappedRegion=record
    baseaddress: ptruint; //if a new section overlaps, append to this
    size: ptruint; //if a new section ovrlaps increase this
    previous: PMappedRegion;
    next: PMappedRegion;
  end;

  PPointerListEntry=^TPointerListEntry;
  TPointerListEntry=record
    address: ptruint;
    pointsto: ptruint;
    previous: PPointerListEntry;
    next: PPointerListEntry;
  end;


  type TPointerRecord=packed record
    level: integer;
    offset: TDwordArray;
  end;
  PPointerRecord=^TPointerRecord;



  TPointerfileReader=class
  private
    files: array of record
      f: Tfilestream;
      startindex: qword;
      lastindex: qword;
    end;
    entrysize: integer;
    fcount: qword;
    pointerfileLevelwidth: integer;

    bufferindex: integer;
    buffersize: integer;
    pointerrecords: PPointerRecord;    //actually an array...

    stringbuf: pchar;
    widestringbuf: pwidechar;

    pointermap: TMap;
    ffilename: string;
    fhexadecimal: boolean;

    function getByteFromAddress(address: ptruint; var error: boolean): byte;
    function getWordFromAddress(address: ptruint; var error: boolean): word;
    function getDWordFromAddress(address: ptruint; var error: boolean): dword;
    function getQWordFromAddress(address: ptruint; var error: boolean): qword;
    function getSingleFromAddress(address: ptruint; var error: boolean): single;
    function getDoubleFromAddress(address: ptruint; var error: boolean): double;
    function getPointerFromAddress(address: ptruint; var error: boolean): ptruint;

    procedure writeRecord(filestream: TfileStream; precord: PPointerRecord); //for sorting
  public
    vartype: TVariableType;
    procedure clearPointerCache;
    function getPointerRec(index: qword): PPointerRecord;
    function getAddressFromPointerRecord(p: ppointerrecord; baseaddress: ptruint; shadow: ptruint; shadowsize: integer): ptruint;
    function getStringFromPointerRecord(p: ppointerrecord; address: ptruint; shadow: ptruint; shadowsize: integer): string;
    function getStringAndAddress(index: qword; var address: ptruint; out p: PPointerRecord; shadow: ptruint; shadowsize: integer): string;

    procedure sort(offsetnr: integer);

    procedure CloseFile;
    procedure LoadFile(filename: string);
    constructor create(filename: string);
    destructor destroy; override;

    property levelWidth: integer read pointerfileLevelwidth;
    property count: qword read fcount;
    property filename: string read ffilename;
    property hexadecimal: boolean read fhexadecimal write fhexadecimal;
  end;

  TAddressWithShadow=record
    address: ptruint;
    shadow: ptruint;
    shadowsize: ptruint;
  end;

  TAddressWithShadowList=array of TAddressWithShadow;


  TStructureCompareWorkItem=record
    currentLevel: integer;
    path: TPointerPath;
    LookingForList: array of ptruint;
    NotLookingForList: array of ptruint;
  end;


  TfrmStructureCompare=class;
  TStructCompareRescan=class(tthread)
  private
    LFList: TAddressWithShadowList;
    NLFList: TAddressWithShadowList;

    outputfile: tfilestream;
    pointerfilereader: TPointerfileReader;

    outputfilename: string;
    oldpointerfilename: string;

    results: TMemorystream;
    fcount: qword;
    fCurrentPosition: qword;
    fMaxPosition: qword;
    sameonly: boolean;

    lastwrite: qword;


    ownerFrmStructureCompare: TfrmStructureCompare;
    comparesize: integer;

    procedure addPointer(p: PPointerRecord);
    procedure flushresults;



  public
    progressbar: TProgressBar;
    errorstring:string;

    procedure execute; override;
    constructor create(comparesize: integer; sameonly: boolean; const LFL: TAddressWithShadowList; const NLFL: TAddressWithShadowList; oldpointerfilename: string; outputfilename: string; ownerFrmStructureCompare: TfrmStructureCompare );
    destructor destroy; override;
    property count: qword read fcount;
    property currentPosition: qword read fCurrentPosition;
    property maxPosition: qword read fMaxPosition;
  end;



  TStructCompareController=class;

  TStructCompareScanner=class(TThread)
  private
    owner: TStructCompareController;
    is64bittarget: boolean;
    maxlevel: integer;
    structsize: integer;
    alignment: integer;
    LookingForShadowList: array of TAddressWithShadow;
    NotLookingForShadowList: array of TAddressWithShadow;

    memoryblockLF:  array of PByteArray;
    memoryblockNLF: array of PByteArray;
    blocksize: integer;

    newwi: TStructureCompareWorkItem;

    readmemorycache: array of record
      pageindex: ptruint;
      data: pointer;
      lasttimeaccess: qword;
    end;

    filename: string;
    lastwrite: qword;
    results: TMemorystream;
    resultfile: TFileStream;

    sameonly: boolean;


    procedure flushresults;
    procedure writeResult(path: TPointerpath; level: integer);
    function readMemory(address: ptruint; destination: pointer; size: integer): boolean;

    function getWorkItem(var wi: TStructureCompareWorkItem): boolean; //gets a workitem from the queue, waits if empty
    procedure addNewWorkItemToQueue; //add the new workitem to the queue (handles it itself if the que is full)
    procedure HandleWorkItem(wi: TStructureCompareWorkItem);
  public
    foundcount: integer;

    idle: boolean;
    procedure execute; override;
    destructor destroy; override;
  end;

  TStructCompareController=class(tthread)
  private
    LFList: TAddressWithShadowList;
    NLFList: TAddressWithShadowList;

    structsize: ptruint;
    maxlevel: integer;

    foundcount: integer;

    //levelblock: array of Puint64Array;  //I think this is obsolete

   // block64: array of Pint64Array;
    sameonly: boolean;

    results: TMemorystream;
    resultfile: tfilestream;
    lastwrite: dword;

    alignment: integer;

    mustbeinregion: boolean;
    pointerstart: ptruint;
    pointerstop: ptruint;
    owner: TfrmStructureCompare;

    is64bittarget: boolean;
  public
    workqueue: array [0..63] of TStructureCompareWorkItem;
    workqueueCS: TCriticalSection;
    workqueueSemaphore: TSemaphore;
    workqueuepos: integer;

    workers: array of TStructCompareScanner;

    memorymap: TPageMap;
    memorymapCS: TCriticalSection;
    filename: string;

    configfile: tfilestream;
    errorstring: string;

    procedure execute; override;
    constructor create(alignment: integer; sameonly: boolean; const LFL: TAddressWithShadowList; const NLFL: TAddressWithShadowList; structsize: integer; maxlevel: integer; filename: string; owner: TfrmStructureCompare);
    destructor destroy; override;
  end;

  TfrmStructureCompare = class(TForm)
    btnAddAddressLF: TButton;
    btnAddAddressNLF: TButton;
    btnNewScan: TButton;
    btnScan: TButton;
    cbHexadecimal: TCheckBox;
    cbSameOnly: TCheckBox;
    comboType: TComboBox;
    edtAlignsize: TEdit;
    edtMaxLevel: TEdit;
    edtStructsize: TEdit;
    FindDialog1: TFindDialog;
    gbLF: TGroupBox;
    gbNLF: TGroupBox;
    scImageList: TImageList;
    lblAlign: TLabel;
    lblInfo: TLabel;
    lblMaxLevel: TLabel;
    lblStructsize: TLabel;
    lblvds: TLabel;
    lvResults: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miMaxAddresses: TMenuItem;
    miDeleteAddress: TMenuItem;
    miShadow: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    MenuItem9: TMenuItem;
    miFind: TMenuItem;
    miFindNext: TMenuItem;
    miNewScan: TMenuItem;
    miOpen: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem7: TMenuItem;
    miClearCache: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel9: TPanel;
    pnlNLF: TPanel;
    pnlLF: TPanel;
    Panel7: TPanel;
    pmPointerfile: TPopupMenu;
    pmAddressPopup: TPopupMenu;
    SaveDialog1: TSaveDialog;
    sbLF: TScrollBox;
    sbNLF: TScrollBox;
    Splitter1: TSplitter;
    statusupdater: TTimer;
    tRefresher: TTimer;
    procedure btnAddAddressClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure cbHexadecimalChange(Sender: TObject);
    procedure comboTypeChange(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gbNLFClick(Sender: TObject);
    procedure lvResultsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvResultsCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure lvResultsData(Sender: TObject; Item: TListItem);
    procedure lvResultsDblClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miDeleteAddressClick(Sender: TObject);
    procedure miFindClick(Sender: TObject);
    procedure miMaxAddressesClick(Sender: TObject);
    procedure miNewScanClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure miClearCacheClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miShadowClick(Sender: TObject);
    procedure pmAddressPopupPopup(Sender: TObject);
    procedure statusupdaterTimer(Sender: TObject);
    procedure tRefresherTimer(Sender: TObject);
  private
    { private declarations }
    scanner: TStructCompareController;
    rescanner: TStructCompareRescan;
    pointerfilereader: TPointerfilereader;

    btncancel: TButton;
    maxaddresses: integer;

    function mapCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    function pointerCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    procedure cleanup;
    procedure OpenPointerfile(filename: string);
    procedure scanDone;
    procedure rescanDone;
    procedure scanError;

    function getStringFromPointer(address: ptruint; offsets: TDwordArray; level, bytesize: integer; unicode: boolean; var a: ptruint): string;
    procedure setGUIStateEnabled(state: boolean);
    procedure reloadlistviewcolumns;
    procedure AddressEditChange(Sender: TObject);
    procedure btnCancelClick(sender: TObject);
  public
    { public declarations }

    edtLF: TList;
    edtNLF: TList;

    donotfreeonclose: boolean;
    procedure disableGui;
    procedure enablegui;
    procedure addAddress(address: ptruint; shadow: qword; shadowsize: integer; group: integer);
  end;



var
  frmStructureCompare: TfrmStructureCompare;



implementation

{ TfrmStructureCompare }

uses frmStructPointerRescanUnit, MemoryBrowserFormUnit, ProcessHandlerUnit,
  Parsers, addressedit, PointerscanresultReader, DPIHelper, ceregistry;


resourcestring
  rsGeneratingStringmap = 'Generating stringmap';
  rsGeneratedScanning = 'Generated. Scanning...';
  rsThisAddressIsNotAccessible = 'This address is not accessible';
  rsStop = 'Stop';
  rsTerminating = 'Terminating...';
  rsAreYouSureYo = 'Are you sure you wish to start a new scan?';
  rsScan = 'Scan';
  rsScanningFoun = 'Scanning... Found %s';
  rsSPSUNotYetImplemented = 'Not yet implemented';
  rsSPSUFUUUU = 'FUUUU';
  rsSPSUFUUUUU = 'FUUUUU';
  rsSPSUException = 'Exception:';
  rsSPSUOffset = 'Offset ';
  rsSPSUAddress = 'Address';
  rsSPSUAddress2 = 'Address 2';
  rsSPSURescan = 'Rescan';
  rsSPSUFound = 'Found:';
  rsSPSUScanDoneFound = 'Scan done! Found ';
  rsSPSUErrorduringScanNoScanresults = 'Error during scan. No scanresults available';
  rsInvalidGroup = 'Group %d address %s (%s) is not valid';
  rsTooSmallAlign = 'Alignsize must be greater than 0';
  rsSPSInvalidstruct = 'Invalid structure pointerfile';
  rsSPSNeedNewCE = 'You''ll need a newer CE version to open this file';
  rsSPSNoError = 'No error';
  rsSPSUnlock = 'Unlock (%.8x-%.8x)';
  rsSPSLock = 'Lock';
  rsStructureCompare = 'Structure Compare';
  rsMaxAddressesShown = 'Max addresses shown :';
  rsMaxReached = '...<max reached>...';
  rsSameAddress = 'This will result in 0 results as address %s appears multiple times';
//----------TPointerfileReader---------


type
  TShadow=class
    address: ptruint;
    size: ptruint;
    donotfree: boolean;
  end;


function TPointerfilereader.getByteFromAddress(address: ptruint; var error: boolean): byte;
var x: ptruint;
begin
  error:=not readprocessmemory(processhandle, pointer(address), @result, 1, x);
  error:=error or (x<>1);
end;

function TPointerfilereader.getWordFromAddress(address: ptruint; var error: boolean): word;
var x: ptruint;
begin
  error:=not readprocessmemory(processhandle, pointer(address), @result, 2, x);
  error:=error or (x<>2);
end;

function TPointerfilereader.getDWordFromAddress(address: ptruint; var error: boolean): dword;
var x: ptruint;
begin
  error:=not readprocessmemory(processhandle, pointer(address), @result, 4, x);
  error:=error or (x<>4);
end;

function TPointerfilereader.getQWordFromAddress(address: ptruint; var error: boolean): qword;
var x: ptruint;
begin
  error:=not readprocessmemory(processhandle, pointer(address), @result, 8, x);
  error:=error or (x<>8);
end;

function TPointerfilereader.getSingleFromAddress(address: ptruint; var error: boolean): single;
var x: ptruint;
begin
  error:=not readprocessmemory(processhandle, pointer(address), @result, 4, x);
  error:=error or (x<>4);
end;

function TPointerfilereader.getDoubleFromAddress(address: ptruint; var error: boolean): double;
var x: ptruint;
begin
  error:=not readprocessmemory(processhandle, pointer(address), @result, 8, x);
  error:=error or (x<>8);
end;

function TPointerfilereader.getPointerFromAddress(address: ptruint; var error: boolean): ptruint;
var x: ptruint;
begin
  result:=0;
  error:=not readprocessmemory(processhandle, pointer(address), @result, processhandler.pointersize, x);
  error:=error or (x<>processhandler.pointersize);
end;


procedure TPointerfilereader.clearPointerCache;
begin
  pointermap.Clear;
end;

function TPointerfileReader.getPointerRec(index: qword): PPointerRecord;
var
  blocksize: integer;
  i: integer;

  relativeindex: qword;
  found: boolean;

  pos: int64;
begin
  if index>fcount then exit(nil);

  result:=nil;


  if (buffersize=0) or (not InRangeQ(index, bufferindex, bufferindex+buffersize-1)) then
  begin
    //find the file this index belongs to
    found:=false;
    for i:=0 to length(files)-1 do
      if InRangeQ(index, files[i].startindex, files[i].lastindex) then
      begin
        relativeindex:=index-files[i].startindex;

        blocksize:=files[i].lastindex-files[i].startindex+1-relativeindex;
        blocksize:=min(blocksize, 4096);

        pos:=relativeindex*entrysize;
        files[i].f.position:=pos;
        if files[i].f.Read(pointerrecords^, entrysize*blocksize)=entrysize*blocksize then
        begin
          bufferindex:=index;
          buffersize:=blocksize;
        end else exit(nil);

        found:=true;
        break;
      end;

    if not found then exit(nil);
  end;

  result:=PPointerRecord(ptruint(pointerrecords)+(index-bufferindex)*entrysize);

end;

function TPointerfilereader.getAddressFromPointerRecord(p: ppointerrecord; baseaddress: ptruint; shadow: ptruint; shadowsize: integer): ptruint;
var address: ptruint;
  a: ptruint;
  i: integer;
  x: ptruint;
  dp: pptruint;
begin

  result:=0;
  if p=nil then exit;

  address:=baseaddress+p.offset[0];

  if (shadow<>0) and inrangeq(address, baseaddress, baseaddress+shadowsize) then
    address:=address+(shadow-baseaddress);

  for i:=1 to p.level do
  begin
    dp:=pointermap.GetDataPtr(address);
    if dp<>nil then
    begin
      address:=dp^;
      if address=0 then
        exit; //unreadable
    end
    else
    begin
      a:=0;
      if not readprocessmemory(processhandle, pointer(address), @a, processhandler.pointersize, x) then
        a:=0;


      if (shadow<>0) and (inrangeq(a, baseaddress, baseaddress+shadowsize)) then
        a:=a+(shadow-baseaddress);

      pointermap.Add(address, a);
      address:=a;

      if a=0 then
        exit; //unreadable
    end;


    address:=address+p.offset[i]
  end;

  result:=address;
end;

function TPointerfileReader.getStringFromPointerRecord(p: ppointerrecord; address: ptruint; shadow: ptruint; shadowsize: integer): string;
var i,j: integer;
  x: ptruint;
  e: boolean;
begin
  e:=false;
  result:='';
  address:=getAddressFromPointerRecord(p, address, shadow, shadowsize);

  case vartype of
    vtByte, vtWord, vtDword, vtQword, vtSingle, vtDouble, vtPointer:
    begin
      e:=false;
      case vartype of
        vtByte: if fhexadecimal then result:=inttohex(getByteFromAddress(address,e),2) else result:=inttostr(getByteFromAddress(address,e));
        vtWord: if fhexadecimal then result:=inttohex(getWordFromAddress(address,e),4) else result:=inttostr(getWordFromAddress(address,e));
        vtDword: if fhexadecimal then result:=inttohex(getDwordFromAddress(address,e),8) else result:=inttostr(getDwordFromAddress(address,e));
        vtQword: if fhexadecimal then result:=inttohex(getQwordFromAddress(address,e),16) else result:=inttostr(getQwordFromAddress(address,e));
        vtSingle: result:=format('%.3f',[getSingleFromAddress(address,e)]);
        vtDouble: result:=format('%.3f',[getDoubleFromAddress(address,e)]);
        vtPointer: result:=IntToHex(getPointerFromAddress(address,e), processhandler.pointersize*2);
      end;
    end;

    vtString:
    begin

      //string
      i:=16;
      if readprocessmemory(processhandle, pointer(address), stringbuf, i, x) then
      begin
        stringbuf[i]:=#0;
        stringbuf[i+1]:=#0;

        result:=stringbuf;
      end;
    end;

    vtUnicodeString:
    begin
      //string
      i:=16;
      if readprocessmemory(processhandle, pointer(address), stringbuf, i, x) then
      begin
        stringbuf[i]:=#0;
        stringbuf[i+1]:=#0;

        result:=widestringbuf;
      end;
    end
    else
      result:=rsSPSUNotYetImplemented;

  end;


  if e then
    result:='???';
end;

function TPointerfileReader.getStringAndAddress(index: qword; var address: ptruint; out p: PPointerRecord; shadow: ptruint; shadowsize: integer): string;
begin
  result:='';
  address:=0;
  p:=getPointerRec(index);
  if p<>nil then
  begin
    result:=getStringFromPointerRecord(p, address, shadow, shadowsize);
    address:=getAddressFromPointerRecord(p, address, shadow, shadowsize);
  end;
end;

procedure TPointerfileReader.writeRecord(filestream: TfileStream; precord: PPointerRecord);
begin
  filestream.WriteBuffer(precord^,entrysize);
end;

procedure TPointerfileReader.sort(offsetnr: integer);  //sorts more on level than on offsetvalues
var
  configfile: TFilestream=nil;

  results: Array of Tfilestream;
  resultcount: integer;

  tempfilename: string;
  i: qword;
  j: integer;
  pr: PPointerRecord;

  fl: Tstringlist;

  s1,s2: string;
begin
  if count=0 then exit;
  if offsetnr>pointerfilelevelwidth then exit;

  tempfilename:=filename+'.sorting';
  configfile:=tfilestream.create(tempfilename, fmCreate);

  configfile.WriteByte($ec); //HEADER
  configfile.WriteByte(compareversion);
  configfile.WriteDWord(pointerfileLevelwidth);
  configfile.free;
  configfile:=nil;

  setlength(results,pointerfileLevelwidth);
  resultcount:=pointerfileLevelwidth;
  for j:=0 to length(results)-1 do
    results[j]:=tfilestream.create(tempfilename+'.results.'+inttostr(j+1), fmCreate);

  i:=0;
  while (i <= count-1) do
  begin
    pr:=getPointerRec(i);
    writeRecord(results[pr^.level], pr);
    inc(i);
  end;

  for j:=0 to length(results)-1 do
  begin
    results[j].free;
    results[j]:=nil
  end;
  setlength(results,0);

  closeFile;

  fl:=tstringlist.create;
  findAllResultFilesForThisPtr(filename,fl);
  for j:=0 to fl.Count-1 do
    DeleteFile(fl[j]);

  DeleteFile(filename);

  fl.free;
  RenameFile(tempfilename, filename);
  for j:=1 to resultcount do
  begin
    s1:=tempfilename+'.results.'+inttostr(j);
    s2:=filename+'.results.'+inttostr(j);
    if RenameFile(s1, s2)=false then
      OutputDebugString('Failed to rename sorted results');
  end;

  loadFile(filename);
end;

procedure TPointerFileReader.closeFile;
var i: integer;
begin
  if pointerrecords<>nil then
    FreeMemAndNil(pointerrecords);

  if stringbuf<>nil then
    FreeMemAndNil(stringbuf);

  widestringbuf:=nil;

  if pointermap<>nil then
    freeandnil(pointermap);

  for i:=0 to length(files)-1 do
  begin
    if files[i].f<>nil then
      freeandnil(files[i].f);
  end;

  setlength(files,0);

  fcount:=0;
  entrysize:=0;
end;

procedure TPointerfileReader.LoadFile(filename: string);
var
  f: Tstringlist=nil;
  configfile: TFilestream=nil;
  i,j: integer;
begin
  ffilename:=filename;

  try
    configfile:=TFileStream.Create(filename, fmOpenRead);
    if configfile.ReadByte<>$ec then raise exception.create(rsSPSInvalidstruct);
    if configfile.readbyte>compareversion then raise exception.create(rsSPSNeedNewCE);
    pointerfileLevelwidth:=configfile.ReadDWord;
    entrysize:=sizeof(pointerfileLevelwidth)+pointerfileLevelwidth*sizeof(dword);

    f:=tstringlist.create;
    findAllResultFilesForThisPtr(ffilename, f);

    setlength(files, f.count);
    for i:=0 to f.count-1 do
      files[i].f:=nil;

    fcount:=0;

    try
      for i:=0 to f.count-1 do
      begin
        files[i].f:=tfilestream.create(f[i], fmOpenRead);
        files[i].startindex:=fcount;

        if files[i].f.Size>0 then
        begin
          inc(fcount, files[i].f.Size div entrysize);
          files[i].lastindex:=fcount-1;
        end
        else
          freeandnil(files[i].f);
      end;

      i:=0;
      while i<length(files) do
      begin
        if files[i].f=nil then
        begin
          for j:=i to length(files)-2 do
            files[j]:=files[j+1];

          setlength(files, length(files)-1);
        end
        else
          inc(i);
      end;

    except
      for i:=0 to f.count-1 do
        if files[i].f<>nil then
          freeandnil(files[i].f);
      raise;
    end;

    getmem(pointerrecords, entrysize*4096);

    getmem(stringbuf, 512);
    widestringbuf:=pwidechar(stringbuf);

    pointermap:=TMap.Create(ituPtrSize,sizeof(pointer))

  finally
    if f<>nil then
      freeandnil(f);

    if configfile<>nil then
      freeandnil(configfile);
  end;
end;

constructor TPointerfileReader.create(filename: string);
begin
  loadFile(filename);
end;

destructor TPointerfileReader.destroy;
var i: integer;
begin
  closefile;


  //cleanup the maps
  inherited destroy;
end;



//--------------TStructCompareRescan----------------
procedure TStructCompareRescan.flushResults;
begin
  lastwrite:=GetTickCount;
  outputfile.WriteBuffer(results.Memory^, results.position);
  results.position:=0;
end;

procedure TStructCompareRescan.addPointer(p: PPointerRecord);
begin
  inc(fcount);
  results.WriteBuffer(p^, pointerfilereader.entrysize);

  if ((gettickcount64-lastwrite)>5*60*1000) or (results.Position>=(15*1024*1024)) then
    flushResults;
end;


procedure TStructCompareRescan.execute;
var index, i,j: integer;
  p: PPointerRecord;

  passed: boolean;

  g1: array of pointer;
  g2: array of pointer;

  g1same, g2same: boolean;

  a: ptruint;
  x: ptruint;

  r: tstringlist=nil;
begin
  setlength(g1, length(LFList));
  setlength(g2, length(NLFList));
  for i:=0 to length(g1)-1 do
    getmem(g1[i], comparesize);

  for i:=0 to length(g2)-1 do
    getmem(g2[i], comparesize);


  try
    try
      for index:=0 to pointerfilereader.count-1 do
      begin
        fCurrentPosition:=index;
        p:=pointerfilereader.getPointerRec(index);
        if p=nil then continue;

        passed:=true;

        for j:=0 to length(LFList)-1 do
        begin
          a:=pointerfilereader.getAddressFromPointerRecord(p,LFList[j].address,LFList[j].shadow, LFList[j].shadowsize);
          if a=0 then
          begin
            passed:=false;
            break;
          end;

          if not readprocessmemory(processhandle, pointer(a),g1[j],comparesize, x) then
          begin
            passed:=false;
            break;
          end;
        end;
        if not passed then continue;

        for j:=0 to length(NLFList)-1 do
        begin
          a:=pointerfilereader.getAddressFromPointerRecord(p,NLFList[j].address,NLFList[j].shadow, NLFList[j].shadowsize);
          if a=0 then
          begin
            passed:=false;
            break;
          end;

          if not readprocessmemory(processhandle, pointer(a),g2[j],comparesize, x) then
          begin
            passed:=false;
            break;
          end;
        end;
        if not passed then continue;

        //still here so readable
        //compare the groups
        g1same:=true;
        g2same:=true;
        for i:=0 to length(g2)-1 do
        begin
          //check if these values are in g1. If so, not valid
          for j:=0 to length(g1)-1 do
          begin
            //first iteration checks if g1 is the same or not
            if (i=0) and (j>0) and (CompareMem(g1[0], g1[j],comparesize)=false) then
            begin
              g1same:=false;
              if sameonly then
              begin
                passed:=false;
                break;
              end;
            end;

            if CompareMem(g2[i], g1[j], comparesize) then
            begin
              passed:=false;
              break;
            end;
          end;
          if passed=false then break;

          //also check if g2 is the same during this loop
          if (i>0) and (CompareMem(g2[0], g2[i],comparesize)=false) then
          begin
            g2same:=false;
            if sameonly then
            begin
              passed:=false;
              break;
            end;
          end;
        end;

        if passed and ((g1same=false) and (g2same=false)) then
          passed:=false;


        if passed then //add it
          addPointer(p);

        //check for forced exit
        if terminated then exit;
      end;

      flushresults;

      if outputfile<>nil then
        freeandnil(outputfile);

      if results<>nil then
        freeandnil(results);

      if pointerfilereader<>nil then
        freeandnil(pointerfilereader);


      //delete all old files
      r:=tstringlist.create;
      findAllResultFilesForThisPtr(outputfilename, r);

      for i:=0 to r.count-1 do
        DeleteFile(r[i]);

      freeandnil(r);

      if deletefile(outputfilename)=false then
        OutputDebugString('Failure deleting '+outputfilename);

      RenameFile(outputfilename+'.temp', outputfilename);
      RenameFile(outputfilename+'.results.0.temp', outputfilename+'.results.0');

      Queue(ownerFrmStructureCompare.rescandone);
    finally
      if r<>nil then
        freeandnil(r);

      if outputfile<>nil then
        freeandnil(outputfile);

      if results<>nil then
        freeandnil(results);

      if pointerfilereader<>nil then
        freeandnil(pointerfilereader);

      for i:=0 to length(g1)-1 do
        FreeMemAndNil(g1[i]);

      for i:=0 to length(g2)-1 do
        FreeMemAndNil(g2[i]);
    end;
  except
    on e:exception do
    begin
      errorstring:=e.message;
      Queue(ownerFrmStructureCompare.scanerror);
    end;
  end;


end;

constructor TStructCompareRescan.create(comparesize: integer; sameonly: boolean; const LFL: TAddressWithShadowList; const NLFL: TAddressWithShadowList; oldpointerfilename: string; outputfilename: string; ownerFrmStructureCompare: TfrmStructureCompare );
var
  i: integer;
  regflags: tregexprflags;
  lw: integer;
  configfile: tfilestream;
begin
  progressbar:=tprogressbar.create(ownerFrmStructureCompare);
  progressbar.align:=alBottom;
  progressbar.parent:=ownerFrmStructureCompare;

  self.comparesize:=comparesize;

  setlength(LFList, length(LFL));
  for i:=0 to length(LFL)-1 do
    LFList[i]:=LFL[i];

  setlength(NLFList, length(NLFL));
  for i:=0 to length(NLFL)-1 do
    NLFList[i]:=NLFL[i];

  self.oldpointerfilename:=oldpointerfilename;
  pointerfilereader:=TPointerfileReader.create(oldpointerfilename);
  fmaxPosition:=pointerfilereader.count;

  self.outputfilename:=outputfilename;
  self.ownerFrmStructureCompare:=ownerFrmStructureCompare;
  self.sameonly:=sameonly;
  pointerfilereader.clearPointercache;

  //build the configfile
  configfile:=TFileStream.Create(outputfilename+'.temp', fmCreate);
  configfile.WriteByte($ec); //header (not to be confused with pointerscan)
  configfile.WriteByte(compareversion);
  configfile.WriteDWord(pointerfilereader.levelWidth);
  configfile.free;

  outputfile:=Tfilestream.create(outputfilename+'.results.0.temp', fmcreate);

  lastwrite:=GetTickCount64;
  results:=TMemoryStream.Create;
  results.size:=16*1024*1024;
  inherited create(suspended);
end;


destructor TStructCompareRescan.destroy;
begin
  if pointerfilereader<>nil then
    freeandnil(pointerfilereader);

  if outputfile<>nil then
    freeandnil(outputfile);

  if results<>nil then
    freeandnil(results);

  if progressbar<>nil then
    freeandnil(progressbar);

  inherited destroy;
end;

//--------------TStructCompareScanner------------------

procedure TStructCompareScanner.flushresults;
begin
  resultfile.WriteBuffer(results.Memory^, results.position);
  results.position:=0;
  lastwrite:=GetTickCount;
end;

procedure TStructCompareScanner.writeResult(path: TPointerpath; level: integer);
begin
  results.WriteBuffer(level, sizeof(level));
  results.WriteBuffer(path[0], sizeof(path[0])*(maxlevel+1));

  if (getTickCount-lastwrite>5*60*1000) or (results.Position>=(15*1024*1024)) then
    flushResults;

  inc(foundcount);
end;

function TStructCompareScanner.readMemory(address: ptruint; destination: pointer; size: integer): boolean;
var
  pi: PPageInfo;
  pageindex: ptruint;
  i: integer;
  x: ptruint;
  readok: boolean;

  data: PByteArray;
  offset: word;

  block: integer;
begin

  while size>0 do
  begin
    pageindex:=address shr 12;
    offset:=address and $fff;

    pi:=owner.memorymap.GetPageInfo(pageindex);
    if pi=nil then
    begin
      getmem(data,4096);
      x:=0;
      readok:=ReadProcessMemory(processhandle, pointer(pageindex shl 12), data, 4096, x);

      if (not readok) or (x<>4096) then
      begin
        //unreadable memory or buggy rpm hook
        FreeMemAndNil(data);

        exit(false)
      end;

      owner.memorymapCS.Enter;
      try
        owner.memorymap.Add(pageindex, data);
      finally
        owner.memorymapCS.Leave;
      end;


    end
    else
      data:=pi^.data;

    block:=4096-offset;

    if block>size then
      block:=size;

    copymemory(destination,@data[offset],block);

    dec(size,block);
    destination:=pointer(ptruint(destination)+block);
  end;

  result:=true;
end;

function TStructCompareScanner.getWorkItem(var wi: TStructureCompareWorkItem): boolean;
{pre: wi has the proper array lengths setup}
var i: integer;
begin
  result:=false;
  if owner.workqueueSemaphore.tryAcquire=false then
  begin
    //the list is empty, wait
    idle:=true;
    owner.workqueueSemaphore.Acquire;
  end;
  idle:=false;

  owner.workqueueCS.Enter;
  try
    if owner.workqueuepos>0 then
    begin
      wi.currentLevel:=owner.workqueue[owner.workqueuepos-1].currentLevel;
      for i:=0 to length(wi.LookingForList)-1 do
        wi.LookingForList[i]:=owner.workqueue[owner.workqueuepos-1].LookingForList[i];

      for i:=0 to length(wi.NotLookingForList)-1 do
        wi.NotLookingForList[i]:=owner.workqueue[owner.workqueuepos-1].NotLookingForList[i];

      for i:=0 to length(wi.path)-1 do
        wi.path[i]:=owner.workqueue[owner.workqueuepos-1].path[i];

      dec(owner.workqueuepos);
      result:=true;
    end;
  finally
    owner.workqueueCS.Leave;
  end;

  if terminated then result:=false;
end;

procedure TStructCompareScanner.addNewWorkItemToQueue;
var
  addedtoqueue: boolean=false;
  i: integer;
begin
  if owner.workqueuepos<60 then
  begin
    if owner.workqueueCS.tryenter then
    begin
      try
        if (owner.workqueuepos<64) then
        begin
          for i:=0 to length(newwi.NotLookingForList)-1 do
            owner.workqueue[owner.workqueuepos].NotLookingForList[i]:=newwi.NotLookingForList[i];

          for i:=0 to length(newwi.LookingForList)-1 do
            owner.workqueue[owner.workqueuepos].LookingForList[i]:=newwi.LookingForList[i];

          for i:=0 to length(newwi.path)-1 do
            owner.workqueue[owner.workqueuepos].path[i]:=newwi.path[i];

          owner.workqueue[owner.workqueuepos].currentLevel:=newwi.currentLevel;

          inc(owner.workqueuepos);
          addedtoqueue:=true;
          owner.workqueueSemaphore.Release;
        end;
      finally
        owner.workqueueCS.Leave;
      end;
    end;
  end;

  if (not addedtoqueue) and (not terminated) then
    handleWorkItem(newwi); //do it yourself
end;

procedure TStructCompareScanner.handleWorkItem(wi: TStructureCompareWorkItem);
var
  i,j: integer;
  a: ptruint;
  pi: PPageInfo;
  pageindex: integer;

  pos: integer;
  valid: boolean;

  t1same: boolean;
  t2same: boolean;

  potentialpointer: boolean;
  allreadablepointers: boolean;
  p: ptruint;
begin
  //do stuff

  //matching rules:    (looking for is table 1, not looking for is table 2)
  //if any value of table 2 is in table 1, then not valid
  //if above passes then
  //if table 1 fields are all the same and table 2 fields are all the same: match
  //if table 1 fields are all the same and table 2 fields are not all the same: match
  //if table 1 fields are all different and table 2 fields are all the same: match

  //if no match and ALL fields represent an address then add it as a new workitem (fields where the player is null and the enemy is filled in have no further use besides checking if it's valid or not)

  pos:=0;
  for i:=0 to wi.currentLevel-1 do
    newwi.path[i]:=wi.path[i];


  while pos<structsize do
  begin
    for i:=0 to length(memoryblockLF)-1 do
    begin
      a:=wi.LookingForList[i]+pos;
      if (LookingForShadowList[i].shadow<>0) and inrangeq(a, LookingForShadowList[i].address, LookingForShadowList[i].address+LookingForShadowList[i].shadowsize) then
      begin
        //make it a relative address in the shadow copy
        a:=a-LookingForShadowList[i].address+LookingForShadowList[i].shadow;
        if readMemory(a, memoryblockLF[i], blocksize)=false then
        begin
          inc(pos,alignment);
          continue;
        end;
      end
      else
      if readMemory(a, memoryblockLF[i], blocksize)=false then
        exit; //unreadable
    end;

    for i:=0 to length(memoryblockNLF)-1 do
    begin
      a:=wi.NotLookingForList[i]+pos;
      if (NotLookingForShadowList[i].shadow<>0) and inrangeq(a, NotLookingForShadowList[i].address, NotLookingForShadowList[i].address+NotLookingForShadowList[i].shadowsize) then
      begin
        //make it a relative address in the shadow copy
        a:=(a-NotLookingForShadowList[i].address)+NotLookingForShadowList[i].shadow;
        if readMemory(a, memoryblockNLF[i], blocksize)=false then
        begin
          inc(pos,alignment);
          continue;
        end;
      end
      else
      if readMemory(a, memoryblockNLF[i], blocksize)=false then
        exit; //unreadable
    end;

    //do the check

    valid:=true;
    t1same:=true;
    t2same:=true;
    potentialpointer:=true;
    for i:=0 to length(memoryblockNLF)-1 do
    begin
      //check if these values are in memoryblockLF. If so, not valid
      for j:=0 to length(memoryblockLF)-1 do
      begin
        //use the first iteration to see of LF is all the same nor not
        if (i=0) and (j>0) and (CompareMem(memoryblockLF[0], memoryblockLF[j],alignment)=false) then
        begin
          t1same:=false;
          if sameonly then
          begin
            valid:=false;
            break;
          end;
        end;

        if CompareMem(memoryblockNLF[i], memoryblockLF[j], alignment) then
        begin
          //a value in table2 matches table1
          valid:=false;
          potentialpointer:=false;
          break;
        end;

      end;
      if valid=false then break;

      if (i>0) and (CompareMem(memoryblockNLF[0], memoryblockNLF[i],alignment)=false) then
      begin
        t2same:=false;
        if sameonly then
        begin
          valid:=false;
          break;
        end;
      end;
    end;

    if valid then
      if (t1same=false) and (t2same=false) then valid:=false;

    if valid then
    begin
      //found a possible identifier to use
      wi.path[wi.currentLevel]:=pos;
      writeResult(wi.path,wi.currentLevel);
    end;

    if (potentialpointer) then
    begin
      allreadablepointers:=(wi.currentLevel<maxlevel) and potentialpointer;

      if allreadablepointers then
      begin
        for i:=0 to length(memoryblockLF)-1 do
        begin
          p:=0;
          if is64bittarget then
          begin
            newwi.LookingForList[i]:=pqword(memoryblockLF[i])^;
          end
          else
          begin
            newwi.LookingForList[i]:=pdword(memoryblockLF[i])^;
          end;

          if readMemory(newwi.LookingForList[i], @p, 1)=false then
          begin
            allreadablepointers:=false;
            break;
          end;
        end;
      end;

      if allreadablepointers then
      begin
        for i:=0 to length(memoryblockNLF)-1 do
        begin
          p:=0;
          if is64bittarget then
          begin
            newwi.NotLookingForList[i]:=pqword(memoryblockNLF[i])^;
          end
          else
          begin
            newwi.NotLookingForList[i]:=pdword(memoryblockNLF[i])^;
          end;

          if readMemory(newwi.NotLookingForList[i], @p, 1)=false then
          begin
            allreadablepointers:=false;
            break;
          end;
        end;
      end;
      if allreadablepointers then
      begin
        newwi.currentLevel:=wi.currentLevel+1;

        newwi.path[wi.currentLevel]:=pos;
        addNewWorkItemToQueue;
      end;

    end;


    inc(pos,alignment);
  end;

  //profit
end;

procedure TStructCompareScanner.execute;
var
  wi: TStructureCompareWorkItem;
  i: integer;
begin
  //initialize the temp and new workitem object (saves on allocs)
  results:=tmemorystream.create;

  lastwrite:=gettickcount64;
  resultfile:=TFileStream.Create(filename,fmCreate);


  setlength(newwi.LookingForList,length(LookingForShadowList));
  setlength(newwi.NotLookingForList,length(NotLookingForShadowList));
  setlength(newwi.path,maxlevel+1); //for the final offset

  setlength(memoryblockLF,length(LookingForShadowList));
  setlength(memoryblockNLF,length(NotLookingForShadowList));

  blocksize:=max(alignment, processhandler.pointersize);
  for i:=0 to length(memoryblockLF)-1 do
    getmem(memoryblockLF[i],blocksize);

  for i:=0 to length(memoryblockNLF)-1 do
    getmem(memoryblockNLF[i],blocksize);

  setlength(readmemorycache,length(LookingForShadowList)+length(NotLookingForShadowList));
  for i:=0 to length(readmemorycache)-1 do
  begin
    readmemorycache[i].pageindex:=0;
    readmemorycache[i].lasttimeaccess:=0;
    readmemorycache[i].data:=nil;
  end;

  try
    setlength(wi.path,maxlevel+1);
    setlength(wi.LookingForList,length(LookingForShadowList));
    setlength(wi.NotLookingForList,length(NotLookingForShadowList));
    while not terminated and GetWorkItem(wi) do
      HandleWorkItem(wi);
  except
    on e:exception do
      OutputDebugString('TStructCompareScanner.Error:'+e.message);
  end;

  idle:=true;
  flushresults;

  freeandnil(results);
  freeandnil(resultfile);
end;

destructor TStructCompareScanner.destroy;
var i: integer;
begin
  for i:=0 to length(memoryblockLF)-1 do
    FreeMemAndNil(memoryblockLF[i]);

  for i:=0 to length(memoryblockNLF)-1 do
    FreeMemAndNil(memoryblockNLF[i]);

  if results<>nil then
    freeandnil(results);

  if resultfile<>nil then
    freeandnil(resultfile);
  inherited destroy;
end;

//--------------TStructCompareController---------------
procedure TStructCompareController.execute;
var
  count: integer;
  pointerpath: TPointerpath;
  i,j: integer;

  t: TStructCompareScanner;

  released: boolean;
  allidle: boolean;
begin
  try
    errorstring:=rsSPSNoError;

    memorymap:=TPageMap.create;
    memorymapCS:=TCriticalSection.create;;
    try
      count:=GetCPUCount;
      workqueueCS:=TCriticalSection.Create;
      workqueueSemaphore:=TSemaphore.create(64, true);

      configfile:=tfilestream.create(filename, fmCreate);

      setlength(workers, count);
      for i:=0 to length(workers)-1 do
      begin
        workers[i]:=TStructCompareScanner.Create(true);
        workers[i].LookingForShadowList:=LFList;
        workers[i].NotLookingForShadowList:=NLFList;
        workers[i].maxlevel:=maxlevel;
        workers[i].structsize:=structsize;
        workers[i].alignment:=alignment;
        workers[i].is64bittarget:=is64bittarget;
        workers[i].filename:=filename+'.results.'+inttostr(i);
        workers[i].sameonly:=sameonly;
        workers[i].owner:=self;
        workers[i].Start;
      end;

      //allocate space for the workitems
      for i:=0 to 63 do
      begin
        setlength(workqueue[i].path, maxlevel+1);
        for j:=0 to maxlevel do
          workqueue[i].path[j]:=0;

        setlength(workqueue[i].LookingForList, length(LFList));
        setlength(workqueue[i].NotLookingForList, length(NLFList));
      end;

      //input the first workitem
      for i:=0 to length(LFList)-1 do
        workqueue[0].LookingForList[i]:=LFList[i].address;

      for i:=0 to length(NLFList)-1 do
        workqueue[0].NotLookingForList[i]:=NLFList[i].address;

      workqueue[0].currentLevel:=0;

      workqueuepos:=1;
      workqueueSemaphore.Release; //the first worker will now add new items to the queue launching other workers


      //create the results config file (pointerscan file, not compressed config)
      configfile.WriteByte($ec); //header (not to be confused with pointerscan)
      configfile.WriteByte(compareversion);
      configfile.WriteDword(maxlevel+1); //number of offsets in each row (with level 0 that is 1 offset)
      freeandnil(configfile);

      allidle:=false;
      released:=false;

      while (not terminated) and ((not allidle) or (workqueuepos>0)) do
      begin
        j:=0;
        allidle:=true;
        for i:=0 to length(workers)-1 do
        begin
          inc(j,workers[i].foundcount);
          if workers[i].idle=false then
            allidle:=false;
        end;

        foundcount:=j;

      end;


    finally
      for i:=0 to length(workers)-1 do
        workers[i].Terminate;

      while workqueueSemaphore.Release<>-1 do;

      for i:=0 to length(workers)-1 do
      begin
        workers[i].WaitFor;
        workers[i].Free;
      end;

      setlength(workers,0);

      //reached the end, tell the main thread that the scan is done
      if configfile<>nil then
        freeandnil(configfile);

      Queue(owner.scandone);

      freeandnil(workqueueCS);
      freeandnil(workqueueSemaphore);
      freeandnil(memorymapCS);
      freeandnil(memorymap);
    end;

  except
    on e:exception do
    begin
      errorstring:=e.message;
      Queue(owner.scanerror);
    end;
  end;
end;

constructor TStructCompareController.create(alignment: integer; sameonly: boolean; const LFL: TAddressWithShadowList; const NLFL: TAddressWithShadowList; structsize: integer; maxlevel: integer; filename: string; owner: TfrmStructureCompare);
var
  i: integer;
  r: tstringlist;
begin
  setlength(LFList, length(LFL));
  for i:=0 to length(LFL)-1 do
    LFList[i]:=LFL[i];

  setlength(NLFList, length(NLFL));
  for i:=0 to length(NLFL)-1 do
    NLFList[i]:=NLFL[i];

  self.is64bittarget:=processhandler.is64Bit;
  self.structsize:=structsize;
  self.maxlevel:=maxlevel;

  self.alignment:=alignment;
  self.mustbeinregion:=mustbeinregion;

  self.owner:=owner;
  self.filename:=filename;
  self.sameonly:=sameonly;

  r:=tstringlist.create;
  findAllResultFilesForThisPtr(filename, r);
  for i:=0 to r.count-1 do
    DeleteFile(r[i]);

  r.free;
  inherited create(false); //let's get started...
end;

destructor TStructCompareController.destroy;
var i: integer;
begin
  if resultfile<>nil then
    freeandnil(resultfile);

  inherited destroy;
end;


//----------------------------

function TfrmStructureCompare.getStringFromPointer(address: ptruint; offsets: TDwordArray; level, bytesize: integer; unicode: boolean; var a: ptruint): string;
var i: integer;
  x: ptruint;

  b: pchar;
  wb: pwidechar absolute b;
begin
  result:='';
  a:=address+offsets[0];
  for i:=1 to level do
  begin
    if readprocessmemory(processhandle, pointer(a), @a, processhandler.pointersize, x) then
    begin
      a:=a+offsets[i];
    end
    else
    begin
      result:='???';
      exit;
    end;
  end;

  getmem(b, bytesize+2);
  if ReadProcessMemory(processhandle, pointer(a), b, bytesize, x) then
  begin
    b[bytesize]:=#0;
    b[bytesize+1]:=#0;

    if unicode then
      result:=wb
    else
      result:=b;

  end;
  FreeMemAndNil(b);

end;

procedure TfrmStructureCompare.lvResultsColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if pointerfilereader<>nil then
  begin
    if (pointerfilereader.count>10000000) and (MessageDlg('There are over 10000000 entries in this list. This may take a while. Are you sure?', mtConfirmation,[mbyes,mbno],0)<>mryes) then exit;

    pointerfilereader.sort(column.Index);
  end;
end;

procedure TfrmStructureCompare.lvResultsData(Sender: TObject; Item: TListItem);
var
  i: integer;
  s,s2: string;
  a: ptruint;

  a2: ptruint;

  p: PPointerRecord;

  count: integer;

  procedure HandleAddress(edt: TAddressEdit);
  var
    shadow: ptruint;
    shadowsize: integer;
  begin
    a:=edt.address;

    if edt.tag<>0 then
    begin
      shadow:=tshadow(edt.tag).address;
      shadowsize:=tshadow(edt.tag).size;
    end
    else
    begin
      shadow:=0;
      shadowsize:=0;
    end;

    if not edt.invalidAddress then
    begin
      s:=pointerfilereader.getStringFromPointerRecord(p, a, shadow, shadowsize);
      a:=pointerfilereader.getAddressFromPointerRecord(p, a, shadow, shadowsize);

      //check if a is in the shadow, and if so, change it back (for visual purpose)
      if (shadow<>0) and inrangeq(a,shadow,shadow+shadowsize-1) then
        a:=(a-shadow)+edt.address;

      item.SubItems.Add(inttohex(a,8)+' : '+s)
    end
    else
    begin
      item.subitems.add(' ');
    end;
  end;
begin
  if pointerfilereader<>nil then
  begin
    item.data:=nil;

    p:=pointerfilereader.getPointerRec(item.index);

    if p<>nil then
    begin
      item.caption:=inttohex(p.offset[0],1);
      for i:=1 to p.level do
        item.SubItems.add(inttohex(p.offset[i],1));

      for i:=p.level+1 to pointerfilereader.levelWidth-1 do
        item.SubItems.Add(' ');


      count:=0;

      for i:=0 to edtLF.count-1 do
      begin
        inc(count);
        if count>maxAddresses then
        begin
          item.subitems.add(rsMaxReached);
          exit;
        end;

        HandleAddress(TAddressEdit(edtLF[i]));
      end;

      for i:=0 to edtNLF.count-1 do
      begin
        inc(count);
        if count>maxAddresses then
        begin
          item.subitems.add(rsMaxReached);
          exit;
        end;
        HandleAddress(TAddressEdit(edtNLF[i]));
      end;


    end;
  end;
end;

procedure TfrmStructureCompare.lvResultsDblClick(Sender: TObject);
var
  p: tpoint;
  i,j: integer;
  x: integer;
  a: ptruint;
begin
  if (lvResults.Selected<>nil) and (pointerfilereader<>nil) then
  begin
    if edtLF.count>1 then
      a:=TAddressEdit(edtLF[0]).address
    else
    if edtNLF.count>1 then
      a:=TAddressEdit(edtNLF[0]).address
    else
      exit;


    //get the mousecursor
    p:=lvResults.ScreenToClient(mouse.CursorPos);
    x:=0;
    for i:=0 to lvResults.Columns.Count-1 do
    begin
      if (p.x>x) and (p.x<x+lvResults.Column[i].Width) then
      begin
        //found the columns
        j:=i-pointerfilereader.pointerfileLevelwidth;
        if j>=0 then
        begin
          if j>=edtLF.count then
          begin
            dec(j, edtLF.count);
            a:=TAddressEdit(edtNLF[j]).address;
          end
          else
            a:=TAddressEdit(edtLF[j]).address;
        end;
        break;
      end;

      inc(x, lvResults.Column[i].Width);
    end;
    MemoryBrowser.hexview.address:=pointerfilereader.getAddressFromPointerRecord(pointerfilereader.getPointerRec(lvResults.Selected.Index), a, 0, 0);
  end;
end;

procedure TfrmStructureCompare.miCopyClick(Sender: TObject);
var
  e: TAddressEdit;
  i: integer;
begin
  e:=TAddressEdit(pmAddressPopup.PopupComponent);

  for i:=0 to edtLF.Count-1 do
    if TAddressEdit(edtLF[i]).Focused then
    begin
      e:=edtLF[i];
      break;
    end;

  for i:=0 to edtNLF.Count-1 do
    if TAddressEdit(edtNLF[i]).Focused then
    begin
      e:=edtNLF[i];
      break;
    end;

  if e=nil then exit;

  if e.SelLength>0 then
    e.CopyToClipboard;
end;

procedure TfrmStructureCompare.miCutClick(Sender: TObject);
var
  e: TAddressEdit;
  i: integer;
begin
  e:=TAddressEdit(pmAddressPopup.PopupComponent);

  for i:=0 to edtLF.Count-1 do
    if TAddressEdit(edtLF[i]).Focused then
    begin
      e:=edtLF[i];
      break;
    end;

  for i:=0 to edtNLF.Count-1 do
    if TAddressEdit(edtNLF[i]).Focused then
    begin
      e:=edtNLF[i];
      break;
    end;

  if e=nil then exit;

  if e.SelLength>0 then
  begin
    if e.ReadOnly then
      e.CopyToClipboard
    else
      e.CutToClipboard;
  end;
end;

procedure TfrmStructureCompare.miDeleteAddressClick(Sender: TObject);
var
  e: TAddressEdit;
  i: integer;
begin
  e:=TAddressEdit(pmAddressPopup.PopupComponent);
  i:=edtLF.IndexOf(e);
  if i<>-1 then
  begin
    if edtLF.count=1 then exit;
    edtLF.delete(i);
  end;

  i:=edtNLF.IndexOf(e);
  if i<>-1 then
  begin
    if edtNLF.Count=1 then exit;
    edtNLF.delete(i);
  end;

  e.free;
end;

procedure TfrmStructureCompare.miFindClick(Sender: TObject);
begin
  finddialog1.execute;
end;

procedure TfrmStructureCompare.miMaxAddressesClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  s:=inttostr(maxaddresses);
  if InputQuery(rsStructureCompare, rsMaxAddressesShown, s) then
  begin
    maxaddresses:=strtoint(s);
    if maxaddresses<0 then
      maxaddresses:=0;

    miMaxAddresses.caption:=format(rsMaxAddressesShown+' %d',[maxaddresses]);
  end;
end;

procedure TfrmStructureCompare.OpenPointerfile(filename: string);
begin
  cleanup;

  pointerfilereader:=TPointerfileReader.Create(filename);
  comboType.OnChange(comboType);
  cbHexadecimal.OnChange(cbHexadecimal);

  reloadlistviewcolumns;



  lvResults.items.count:=dword(min(dword(1000000), dword(pointerfilereader.count)));
  lblInfo.caption:=inttostr(pointerfilereader.count);
end;

procedure TfrmStructureCompare.scanerror;
begin
  if rescanner<>nil then
    messagedlg(rescanner.errorstring,mtError,[mbok],0);

  if scanner<>nil then
    messagedlg(scanner.errorstring,mtError,[mbok],0);

  cleanup;
end;

procedure TfrmStructureCompare.rescanDone;
begin
  if rescanner<>nil then
    lblInfo.caption:=rsSPSUFound+inttostr(rescanner.count);

  cleanup;
  beep;

  btnScan.Visible:=true;
  btnNewScan.Visible:=true;

  OpenPointerfile(SaveDialog1.FileName);

  if btncancel<>nil then
    freeandnil(btncancel);

  EnableGui;
  beep;
end;

procedure TfrmStructureCompare.scanDone;
begin
  if scanner<>nil then
    lblInfo.caption:=rsSPSUFound+inttostr(scanner.foundcount);

  cleanup;

  OpenPointerfile(SaveDialog1.FileName);

  btnScan.visible:=true;
  btnScan.caption:=rsSPSURescan;
  btnScan.tag:=1;

  btnNewScan.visible:=true;
  btnNewScan.enabled:=true;

  if btncancel<>nil then
    freeandnil(btncancel);

  EnableGui;
  beep;
end;

function TfrmStructureCompare.mapCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  result:=CompareValue(PMappedRegion(Data1).baseaddress, PMappedRegion(Data2).baseaddress);
end;

function TfrmStructureCompare.pointerCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  result:=CompareValue(PPointerListEntry(Data1).address, PPointerListEntry(Data2).address);
end;

procedure TfrmStructureCompare.cleanup;
var r: TAvgLvlTreeNode;
  p,m: PMappedRegion;
begin
  statusupdater.enabled:=false;

  if rescanner<>nil then
  begin
    rescanner.terminate;
    rescanner.waitfor;
    freeandnil(rescanner);
  end;

  if scanner<>nil then
  begin
    scanner.terminate;
    scanner.WaitFor;
    freeandnil(scanner);
  end;

  lvResults.items.count:=0;
  while lvResults.ColumnCount>0 do
    lvResults.Columns.Delete(0);

  if pointerfilereader<>nil then
    freeandnil(pointerfilereader);
end;

procedure TfrmStructureCompare.btnCancelClick(sender: TObject);
begin
  if scanner<>nil then
    scanner.Terminate;

  if rescanner<>nil then
    rescanner.terminate;

  btncancel.enabled:=false;
end;

procedure TfrmStructureCompare.btnScanClick(Sender: TObject);
var baseaddress: ptruint;
  baseaddress2: ptruint;
  structsize: integer;
  maxlevel: integer;
  alignsize: integer;
  pointerstart: ptruint;
  pointerstop: ptruint;

  diffkind: TDiffkind;
  vartype: TVariableType;

  shadow, shadow2: ptruint;
  shadowsize, shadowsize2: ptruint;

  oldpointerfile: string;

  i,j: integer;
  LF: array of TAddressWithShadow;
  NLF: array of TAddressWithShadow;
begin

  if pointerfilereader<>nil then
    oldpointerfile:=pointerfilereader.filename
  else
    oldpointerfile:='';

  setlength(LF, edtLF.count);

  for i:=0 to edtLF.count-1 do
  begin
    lf[i].address:=TAddressEdit(edtLF[i]).address;

    if TAddressEdit(edtLF[i]).tag<>0 then
    begin
      lf[i].shadow:=TShadow(TAddressEdit(edtLF[i]).tag).Address;
      lf[i].shadowsize:=TShadow(TAddressEdit(edtLF[i]).tag).Size;
    end
    else
    begin
      lf[i].shadow:=0;
      lf[i].shadowsize:=0;
    end;

    if TAddressEdit(edtLF[i]).invalidAddress then raise exception.create(Format(rsInvalidGroup, [1, inttostr(i+1), TAddressEdit(edtLF[i]).text]));

    for j:=0 to i-1 do
      if (lf[j].address=lf[i].address) and (lf[i].shadow=lf[j].shadow) then raise exception.create(Format(rsSameAddress, [TAddressEdit(edtLF[i])]));
  end;

  setlength(NLF, edtNLF.count);
  for i:=0 to edtNLF.count-1 do
  begin
    nlf[i].address:=TAddressEdit(edtNLF[i]).address;

    if TAddressEdit(edtNLF[i]).tag<>0 then
    begin
      nlf[i].shadow:=TShadow(TAddressEdit(edtNLF[i]).tag).Address;
      nlf[i].shadowsize:=TShadow(TAddressEdit(edtNLF[i]).tag).Size;
    end
    else
    begin
      nlf[i].shadow:=0;
      nlf[i].shadowsize:=0;
    end;
    if TAddressEdit(edtNLF[i]).invalidAddress then raise exception.create(Format(rsInvalidGroup, [2, inttostr(i+1), TAddressEdit(edtNLF[i]).text]));

    for j:=0 to i-1 do
      if (nlf[j].address=nlf[i].address) and (nlf[i].shadow=nlf[j].shadow) then raise exception.create(Format(rsSameAddress, [TAddressEdit(edtLF[i])]));

    for j:=0 to length(lf)-1 do
      if (lf[j].address=nlf[i].address) and (lf[i].shadow=nlf[j].shadow) then raise exception.create(Format(rsSameAddress, [TAddressEdit(edtLF[i])]));
  end;

  maxlevel:=strtoint(edtMaxLevel.text);
  structsize:=strtoint(edtStructsize.text);
  alignsize:=strtoint(edtAlignsize.text);

  vartype:=vtPointer;

  if alignsize<=0 then raise exception.create(rsTooSmallAlign);

  if savedialog1.execute=false then exit;


  cleanup;
  disableGui;

  if btnscan.tag=1 then
  begin
    if oldpointerfile<>'' then
      rescanner:=TStructCompareRescan.create(alignsize,cbSameOnly.checked, lf,nlf, oldpointerfile, savedialog1.filename, self)
    else
      rescandone;
  end
  else
  begin
    scanner:=TStructCompareController.create(alignsize,cbSameOnly.checked, lf,nlf, structsize, maxlevel, savedialog1.filename, self);
    statusupdater.enabled:=true;
  end;

  btnscan.visible:=false;
  btnNewScan.visible:=false;

  btncancel:=TButton.create(self);
  btncancel.parent:=panel9;
  btncancel.caption:=rsStop;
  btncancel.AutoSize:=true;
  btncancel.OnClick:=btnCancelClick;
end;

procedure TfrmStructureCompare.cbHexadecimalChange(Sender: TObject);
begin
  if pointerfilereader<>nil then
    pointerfilereader.hexadecimal:=cbHexadecimal.checked;

  lvResults.Refresh;
end;

procedure TfrmStructureCompare.reloadlistviewcolumns;
var
  nr: integer;
  lc: TListColumn;
  i: integer;
begin
  if pointerfilereader=nil then
  begin
    if lvResults.Columns.Count>0 then
      lvResults.Columns.Clear;
  end
  else
  begin
    nr:=0;
    for i:=0 to pointerfilereader.levelWidth-1 do
    begin
      if lvResults.Columns.count<=nr then
      begin
        lc:=lvResults.Columns.Add;
        lc.MinWidth:=2;
        lc.Width:=70;
      end
      else
        lc:=lvResults.Column[nr];

      lc.Caption:=rsSPSUOffset+inttostr(i);
      inc(nr);
    end;


    for i:=0 to edtLF.Count-1 do
    begin
      if lvResults.Columns.count<=nr then
      begin
        lc:=lvResults.Columns.Add;
        lc.MinWidth:=2;
        lc.Width:=120;
      end
      else
        lc:=lvResults.Column[nr];

      lc.Caption:='G1:'+TAddressEdit(edtLF[i]).Text;
      inc(nr);
    end;

    for i:=0 to edtNLF.Count-1 do
    begin
      if lvResults.Columns.count<=nr then
      begin
        lc:=lvResults.Columns.Add;
        lc.MinWidth:=2;
        lc.Width:=120;
      end
      else
        lc:=lvResults.Column[nr];

      lc.Caption:='G2:'+TAddressEdit(edtNLF[i]).Text;
      inc(nr);
    end;

    while lvResults.columns.count>nr do
      lvResults.Columns.Delete(lvResults.columns.count-1);

  end;
end;

procedure TfrmStructureCompare.AddressEditChange(Sender: TObject);
begin
  reloadlistviewcolumns;
  lvResults.Refresh;
end;

procedure TfrmStructureCompare.btnAddAddressClick(Sender: TObject);
var e: TaddressEdit;
begin
  e:=TAddressEdit.Create(self);
  e.OnChange:=AddressEditChange;
  e.PopupMenu:=pmAddressPopup;

  if (sender as Tbutton).tag=0 then
  begin
    e.parent:=pnlLF;
    edtLF.Add(e);
  end
  else
  begin
    e.parent:=pnlNLF;
    edtNLF.Add(e);
  end;
end;


procedure TfrmStructureCompare.comboTypeChange(Sender: TObject);
begin
  if pointerfilereader<>nil then
  begin
    case comboType.itemindex of
      0: pointerfilereader.vartype:=vtString;
      1: pointerfilereader.vartype:=vtUnicodeString;
      2: pointerfilereader.vartype:=vtByte;
      3: pointerfilereader.vartype:=vtWord;
      4: pointerfilereader.vartype:=vtDword;
      5: pointerfilereader.vartype:=vtQword;
      6: pointerfilereader.vartype:=vtSingle;
      7: pointerfilereader.vartype:=vtDouble;
      8: pointerfilereader.vartype:=vtPointer;
    end;

    lvResults.Refresh;

  end;
end;


procedure TfrmStructureCompare.FindDialog1Find(Sender: TObject);
var
  i,start: integer;
  s: string;
  FindText: string;
  cs: boolean;
begin
  FindText:=finddialog1.FindText;
  if FindText='' then exit;

  miFindNext.enabled:=true;

  cs:=frMatchCase in finddialog1.Options;
  if not cs then findtext:=uppercase(findtext);

  for i:=lvResults.ItemIndex+1 to lvResults.Items.Count-1 do
  begin
    s:=lvResults.items[i].SubItems.Text;
    if not cs then s:=uppercase(s);

    if pos(FindText, s)>0 then
    begin
      lvResults.ItemIndex:=i;
      lvResults.Items[i].MakeVisible(false);
      exit;
    end;
  end;

  beep;
end;

procedure TfrmStructureCompare.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  cleanup;

  if donotfreeonclose then exit;

  if self<>frmStructureCompare then closeaction:=cafree;
end;

procedure TfrmStructureCompare.FormCreate(Sender: TObject);
begin
  edtLF:=Tlist.create;
  edtNLF:=Tlist.create;
  autosize:=false;
  LoadFormPosition(self);

  maxAddresses:=cereg.readInteger('Structure Compare Max Addresses', 32);
end;

procedure TfrmStructureCompare.FormDestroy(Sender: TObject);
begin
  edtLF.free;
  edtNLF.free;

  SaveFormPosition(self);
  cereg.writeInteger('Structure Compare Max Addresses', maxaddresses);
end;

procedure TfrmStructureCompare.FormShow(Sender: TObject);
var w,b,mh: integer;
begin
  //panel1.Constraints.MinHeight:=btnNewScan.Top+btnNewScan.Height+lblInfo.Height+4;

  if edtLF.Count=0 then
    btnAddAddressLF.click;

  if edtNLF.count=0 then
    btnAddAddressNLF.click;

  AdjustComboboxSize(comboType, canvas);

 // gbLF.Constraints.MinWidth:=GetEditBoxMargins(tedit(edtLF[0]))+canvas.TextWidth(' XXXXXXXXXXXXXXXX ');
  w:=GetEditBoxMargins(edtMaxLevel)+canvas.TextWidth(' XXXXXXXXXXXXXXXX ');      ;


  b:=tedit(edtLF[0]).ClientToParent(point(0,0),gbLF).x;


  gbLF.Constraints.MinWidth:=w+b;
  gbNLF.Constraints.MinWidth:=w+b;

  gbLF.ClientWidth:=w+b;
  gbNLF.ClientWidth:=w+b;

  Constraints.MinWidth:=panel7.left+panel7.Width;

  mh:=panel9.top+panel9.height+lblinfo.height;
  panel3.Constraints.MinHeight:=mh;
end;




procedure TfrmStructureCompare.gbNLFClick(Sender: TObject);
begin

end;



procedure TfrmStructureCompare.lvResultsCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; var DefaultDraw: Boolean);
var i: integer;
begin
  defaultdraw:=true;
  if pointerfilereader<>nil then
  begin
    i:=subitem-pointerfilereader.levelWidth;
    if i>=0 then
    begin

      sender.Canvas.Font.Color:=clBlue;
      if i>=edtLF.count then
      begin
        sender.Canvas.Font.Color:=clRed;
      end;
    end;

  end;
end;

procedure TfrmStructureCompare.miNewScanClick(Sender: TObject);
begin
  if MessageDlg(rsAreYouSureYo, mtConfirmation, mbYesNo, 0)=mryes then
  begin
    cleanup;
    btnScan.tag:=0;
    btnScan.caption:=rsScan;

    btnScan.Left:=(panel1.ClientWidth div 2)-(btnscan.Width div 2);

    EnableGui;

    edtMaxLevel.Enabled:=true;
    edtStructsize.enabled:=true;
    lblMaxLevel.enabled:=true;
    lblStructsize.enabled:=true;

    btnNewScan.Visible:=false;
  end;
end;

procedure TfrmStructureCompare.miOpenClick(Sender: TObject);
begin
  if (scanner=nil) and (rescanner=nil) and OpenDialog1.Execute then
  begin
    OpenPointerfile(opendialog1.filename);
    enablegui;

    btnScan.caption:=rsSPSURescan;
    btnScan.tag:=1;

    btnNewScan.visible:=true;
    btnNewScan.enabled:=true;
  end;


end;

procedure TfrmStructureCompare.MenuItem7Click(Sender: TObject);
begin
  with TfrmStructureCompare.create(Application) do show;
end;

procedure TfrmStructureCompare.miClearCacheClick(Sender: TObject);
begin
  if pointerfilereader<>nil then
    pointerfilereader.clearPointerCache;

  lvResults.Refresh;
end;

procedure TfrmStructureCompare.miPasteClick(Sender: TObject);
var e: TAddressEdit;
  i: integer;
begin
  e:=TAddressEdit(pmAddressPopup.PopupComponent);

  for i:=0 to edtLF.Count-1 do
    if TAddressEdit(edtLF[i]).Focused then
    begin
      e:=edtLF[i];
      break;
    end;

  for i:=0 to edtNLF.Count-1 do
    if TAddressEdit(edtNLF[i]).Focused then
    begin
      e:=edtNLF[i];
      break;
    end;

  if e=nil then exit; //?
  if e.readonly then exit;

  if Clipboard.AsText<>'' then
    e.PasteFromClipboard;
end;

procedure TfrmStructureCompare.miShadowClick(Sender: TObject);
var
  e: TAddressEdit;
  a: ptruint;

  temp: pointer;
  size: integer;
  x: ptruint;
  s: tshadow;
begin
  e:=TAddressEdit(pmAddressPopup.PopupComponent);
  if e=nil then exit;

  a:=e.address;
  if e.invalidAddress then exit;

  if e.tag<>0 then
  begin
    s:=TShadow(e.tag);
    if s.donotfree=false then
      VirtualFreeEx(processhandle, pointer(TShadow(e.tag).address), 0, MEM_DECOMMIT);

    s.free;
    e.tag:=0;
    e.ReadOnly:=false;

    lvResults.Refresh;
    exit;
  end;


  size:=strtoint(edtStructsize.Text);
  getmem(temp, size);

  if ReadProcessMemory(processhandle, pointer(a), temp, size,x) then
  begin
    a:=ptruint(VirtualAllocEx(processhandle, nil, size,mem_Commit or mem_Reserve, PAGE_READWRITE));
    if a<>0 then
    begin
      WriteProcessMemory(processhandle, pointer(a),temp,size,x);
      s:=tshadow.create;
      s.address:=a;
      s.size:=size;
      e.tag:=ptruint(s);
      e.readonly:=true;
    end;
  end;

  FreeMemAndNil(temp);
end;

procedure TfrmStructureCompare.pmAddressPopupPopup(Sender: TObject);
var e: TAddressEdit;
begin
  e:=TAddressEdit(pmAddressPopup.PopupComponent);
  miPaste.enabled:=Clipboard.AsText<>'';
  miCut.enabled:=e.SelLength>0;
  miCopy.enabled:=e.SelLength>0;
  miShadow.enabled:=(e.invalidAddress=false) or (e.tag<>0);

  if e.tag<>0 then
    miShadow.Caption:=format(rsSPSUnlock,[TShadow(e.tag).address, TShadow(e.tag).address+TShadow(e.tag).size])
  else
    miShadow.caption:=rsSPSLock;
end;



procedure TfrmStructureCompare.statusupdaterTimer(Sender: TObject);
var
  scannerprogress: double;
  scannerTotal: double;
  i: integer;
  x: integer;
begin
{
  if (rescanner<>nil) and (pointerfilereader<>nil) then
    progressbar1.position:=trunc((rescanner.currentPosition / pointerfilereader.count) * 1000);
 }
  if scanner<>nil then
  begin
    lblInfo.caption:=Format(rsScanningFoun, [inttostr(scanner.foundcount)]);
 {
    scannerTotal:=power(scanner.structsize, scanner.maxlevel+1);

    scannerprogress:=0;
    for i:=0 to scanner.maxlevel-1 do
    begin
      x:=scanner.progress[i];
      scannerprogress:=scannerprogress+x*power(scanner.structsize, (scanner.maxlevel+1)-i-1);
    end;

    progressbar1.position:=trunc((scannerprogress / scannertotal) * 1000);

      }
  end
  else
  if rescanner<>nil then
  begin
    if rescanner.progressbar<>nil then
      rescanner.progressbar.position:=(rescanner.currentPosition*100) div rescanner.maxPosition;

    lblinfo.caption:=Format(rsScanningFoun, [inttostr(rescanner.count)])
  end;

end;

procedure TfrmStructureCompare.tRefresherTimer(Sender: TObject);
begin
  lvResults.refresh;
end;

procedure TfrmStructureCompare.setGUIStateEnabled(state: boolean);
begin
  lblvds.enabled:=state;
  comboType.enabled:=state;

  btnNewScan.enabled:=state;
  btnScan.enabled:=state;
end;

procedure TfrmStructureCompare.disableGui;
begin
  setGUIStateEnabled(false);
end;

procedure TfrmStructureCompare.enableGui;
begin
  setGUIStateEnabled(true);
end;

procedure TfrmStructureCompare.addAddress(address: ptruint; shadow: qword; shadowsize: integer; group: integer);
var
  e: TAddressEdit;
  s: Tshadow;
begin
  if group=0 then
  begin
    btnAddAddressLF.click;
    e:=TAddressEdit(edtLF[edtLF.count-1]);
  end
  else
  begin
    btnAddAddressNLF.click;
    e:=TAddressEdit(edtNLF[edtNLF.count-1]);
  end;

  e.Text:=inttohex(address,8);

  if shadow<>0 then
  begin
    s:=tshadow.create;
    s.address:=shadow;
    s.size:=shadowsize;
    s.donotfree:=true;
    e.tag:=ptruint(s);
    e.ReadOnly:=true;
  end;
end;

initialization
  {$I frmstructurecompareunit.lrs}

end.

