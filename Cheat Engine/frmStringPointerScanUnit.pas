{
todo:
This has shown a usefull method to dissect structures
wat can be done is add every dword offset in a structure as an offset and compare against the 4 bytes there
}
unit frmStringPointerScanUnit;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  cefuncproc, newkernelhandler, frmStringMapUnit, MemFuncs, AvgLvlTree, Menus,
  bigmemallochandler, math, maps, oldRegExpr, symbolhandler, commonTypeDefs;

const
  wm_sps_done=wm_user+1;

type

  { TfrmStringPointerScan }
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
    stringsize: integer;
    unicode: BOOL;
    offset: TDwordArray;
  end;
  PPointerRecord=^TPointerRecord;



  TPointerfileReader=class
  private
    pointerfile: TFilestream;
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

    function getByteFromAddress(address: ptruint; var error: boolean): byte;
    function getWordFromAddress(address: ptruint; var error: boolean): word;
    function getDWordFromAddress(address: ptruint; var error: boolean): dword;
    function getQWordFromAddress(address: ptruint; var error: boolean): qword;
    function getSingleFromAddress(address: ptruint; var error: boolean): single;
    function getDoubleFromAddress(address: ptruint; var error: boolean): double;
    function getPointerFromAddress(address: ptruint; var error: boolean): ptruint;

  public
    vartype: TVariableType;
    procedure clearPointerCache;
    function getPointerRec(index: qword): PPointerRecord;
    function getAddressFromPointerRecord(p: ppointerrecord; baseaddress: ptruint; shadow: ptruint; shadowsize: integer): ptruint;
    function getStringFromPointerRecord(p: ppointerrecord; address: ptruint; shadow: ptruint; shadowsize: integer): string;
    function getStringAndAddress(index: qword; var address: ptruint; out p: PPointerRecord; shadow: ptruint; shadowsize: integer): string;

    constructor create(filename: string);
    destructor destroy; override;

    property levelWidth: integer read pointerfileLevelwidth;
    property count: qword read fcount;
    property filename: string read ffilename;
  end;


  Trescan=class(tthread)
  private
    outputfile: tfilestream;
    isstringscan: boolean;
    mustbestart: boolean;
    regEx: TRegExprEngine;
    diffkind: TDiffkind;
    pointerfilereader: TPointerfileReader;

    address, address2: ptruint;
    shadow, shadow2: ptruint;
    shadowsize, shadowsize2: integer;

    mustbeinregion: boolean;
    pointerstart, pointerend: ptruint;
    outputfilename: string;
    oldpointerfilename: string;

    results: TMemorystream;
    fcount: qword;
    fCurrentPosition: qword;

    vartype: TVariableType;
    lastwrite: dword;

    ownerFrmStringPointerScan: TCustomForm;

    procedure addPointer(p: PPointerRecord);
    procedure flushresults;

    function checkByte(p: PPointerRecord): boolean;
    function checkWord(p: PPointerRecord): boolean;
    function checkDWord(p: PPointerRecord): boolean;
    function checkQWord(p: PPointerRecord): boolean;
    function checkSingle(p: PPointerRecord): boolean;
    function checkDouble(p: PPointerRecord): boolean;
    function checkPointer(p: PPointerRecord): boolean;

  public
    procedure execute; override;
    constructor create(suspended: boolean; address, address2: ptruint; mustbeinregion: boolean; pointerstart, pointerend: ptruint; isstringscan, caseSensitive, mustbestart: boolean; regExstr: string; diffkind: TDiffkind; vartype: TVariableType; oldpointerfilename: string; outputfilename: string; ownerFrmStringPointerScan: TCustomForm );
    destructor destroy; override;
    property count: qword read fcount;
    property currentPosition: qword read fCurrentPosition;
  end;


  TScanner=class(tthread)
  private
    baseaddress: ptruint;
    shadow: ptruint;
    shadowsize: ptruint;
    baseaddress2: ptruint;
    shadow2: ptruint;
    shadowsize2: ptruint;

    diffkind: Tdiffkind;
    vartype: TVariableType;
    structsize: ptruint;
    maxlevel: ptruint;
    mappedregions: TAvgLvlTree;
    pointerlist: TAvgLvlTree;

    tempvariablebuffer, tempvariablebuffer2: pbytearray; //storage for the value compare. Preallocate so no need to call getmem/freemem each time
    variablesize: integer;
    valuemap: tmap;

    bma: TBigMemoryAllocHandler;

    count: integer;

    //levelblock: array of Puint64Array;  //I think this is obsolete
    fillpointerblock: pointer; //memory block allocated for the fuillpointers function

   // block64: array of Pint64Array;

    results: TMemorystream;
    resultfile: tfilestream;
    lastwrite: dword;

    //datascan variables
    isDataScan: boolean;
    alignment: integer;

    mustbeinregion: boolean;
    pointerstart: ptruint;
    pointerstop: ptruint;
    ownerFrmStringPointerScan: TCustomForm;

    is64bittarget: boolean;




    procedure handleBlock(blockaddress: ptruint; level: integer; path: TPointerpath);
    function addStringPath(level: integer; path: tpointerpath; stringsize: integer; unicode: bool): boolean;
    function comparePath(level: integer; path: tpointerpath; stringsize: integer): boolean;

    function getAddressFromPath(base :ptruint; column: integer; level: integer; const path: TPointerPath): ptruint;

    procedure mapRegionIfNeeded(blockaddress: ptruint; size: integer);
    procedure fillPointers(base: ptruint; size: integer);
    function getFirstPointerEntry(base: ptruint): PPointerListEntry;

    function getPointerValue(address: ptruint; column: integer): ptruint;

    procedure flushResults;

  public
    progress: TPointerpath;

    procedure execute; override;
    constructor create(isDataScan, mustbeinregion: boolean; alignment: integer; pointerstart, pointerstop: ptruint; baseaddress: ptruint; shadow: ptruint; shadowsize: ptruint; baseaddress2: ptruint; shadow2: ptruint; shadowsize2: integer; diffkind: TDiffkind; vartype: TVariableType; mapvalues: boolean; structsize: integer; maxlevel: integer; mappedregions,pointerlist: TAvgLvlTree; bma: TBigMemoryAllocHandler; filename: string; ownerFrmStringPointerScan: TCustomForm);
    destructor destroy; override;
  end;

  TfrmStringPointerScan = class(TForm)
    btnNewScan: TButton;
    btnScan: TButton;
    cbCaseSensitive: TCheckBox;
    cbHasShadow: TCheckBox;
    cbHasShadow2: TCheckBox;
    cbMustBeStart: TCheckBox;
    cbRegExp: TCheckBox;
    cbPointerInRange: TCheckBox;
    cbMapPointerValues: TCheckBox;
    cbReuseStringmap: TCheckBox;
    comboCompareType: TComboBox;
    comboType: TComboBox;
    edtBase: TEdit;
    edtMaxLevel: TEdit;
    edtShadowAddress: TEdit;
    edtShadowAddress2: TEdit;
    edtPointerStart: TEdit;
    edtPointerStop: TEdit;
    edtAlignsize: TEdit;
    edtExtra: TEdit;
    edtRegExp: TEdit;
    edtShadowSize: TEdit;
    edtShadowSize2: TEdit;
    edtStructsize: TEdit;
    FindDialog1: TFindDialog;
    spImageList: TImageList;
    lblBaseRegion: TLabel;
    lblInfo: TLabel;
    lblMaxLevel: TLabel;
    lblSize: TLabel;
    lblsize2: TLabel;
    lblStructsize: TLabel;
    lblCompare: TLabel;
    lblAlign: TLabel;
    lblAnd: TLabel;
    lblString: TLabel;
    lblExtra: TLabel;
    lblvds: TLabel;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
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
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pmPointerfile: TPopupMenu;
    ProgressBar1: TProgressBar;
    rbDiffDontCare: TRadioButton;
    rbMustBeDifferent: TRadioButton;
    rbMustBeSame: TRadioButton;
    rbStringscan: TRadioButton;
    rbDatascan: TRadioButton;
    SaveDialog1: TSaveDialog;
    statusupdater: TTimer;
    procedure btnScanClick(Sender: TObject);
    procedure cbHasShadowChange(Sender: TObject);
    procedure cbRegExpChange(Sender: TObject);
    procedure cbPointerInRangeChange(Sender: TObject);
    procedure cbReuseStringmapChange(Sender: TObject);
    procedure comboTypeChange(Sender: TObject);
    procedure edtBaseChange(Sender: TObject);
    procedure edtExtraChange(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ListView1CustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);

    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1DblClick(Sender: TObject);
    procedure miFindClick(Sender: TObject);
    procedure miNewScanClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure miClearCacheClick(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure rbDatascanChange(Sender: TObject);
    procedure rbDiffDontCareChange(Sender: TObject);
    procedure statusupdaterTimer(Sender: TObject);
  private
    { private declarations }
    mappedRegions: TAvgLvlTree; //holds the map of the regions that have been mapped

    pointerlist: TAvgLvlTree; //holds the pointers in the app of the mapped regions

    bma: TBigMemoryAllocHandler;

    scanner: TScanner;
    rescanner: TRescan;
    pointerfilereader: TPointerfilereader;

    address, address2: ptruint;
    shadow, shadow2: ptruint;
    shadowsize, shadowsize2: integer;
    hasAddress2: boolean;



    function mapCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    function pointerCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    procedure cleanup;
    procedure OpenPointerfile(filename: string);
    procedure scanDone;

    function getStringFromPointer(address: ptruint; offsets: TDwordArray; level, bytesize: integer; unicode: boolean; var a: ptruint): string;
    procedure setGUIStateEnabled(state: boolean);
  public
    { public declarations }
    procedure disableGui;
    procedure enablegui;
  end;



var
  frmStringPointerScan: TfrmStringPointerScan;



implementation

{ TfrmStringPointerScan }

uses frmStructPointerRescanUnit, MemoryBrowserFormUnit, ProcessHandlerUnit, Parsers;

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
//----------TPointerfileReader---------

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
var blocksize: integer;
begin
  result:=nil;

  if (buffersize=0) or (not InRangeQ(index, bufferindex, bufferindex+buffersize-1)) then
  begin
    blocksize:=count-index;
    blocksize:=min(blocksize, 4096);

    pointerfile.Position:=sizeof(pointerfileLevelwidth)+index*entrysize;
    if pointerfile.Read(pointerrecords^, entrysize*blocksize)=entrysize*blocksize then
    begin
      bufferindex:=index;
      buffersize:=blocksize;
    end;
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

  address:=baseaddress+p.offset[0];

  if shadow<>0 then
  begin
    if inrangeq(address, baseaddress, baseaddress+shadowsize) then
      address:=address+(shadow-baseaddress);
  end;



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
  result:='';
  address:=getAddressFromPointerRecord(p, address, shadow, shadowsize);
  if address>0 then
  begin
    case vartype of
      vtByte, vtWord, vtDword, vtQword, vtSingle, vtDouble, vtPointer:
      begin
        e:=false;
        case vartype of
          vtByte: result:=inttostr(getByteFromAddress(address,e));
          vtWord: result:=inttostr(getWordFromAddress(address,e));
          vtDword: result:=inttostr(getDwordFromAddress(address,e));
          vtQword: result:=inttostr(getQwordFromAddress(address,e));
          vtSingle: result:=format('%.3f',[getSingleFromAddress(address,e)]);
          vtDouble: result:=format('%.3f',[getDoubleFromAddress(address,e)]);
          vtPointer: result:=IntToHex(getPointerFromAddress(address,e), processhandler.pointersize*2);
        end;
      end;

      vtString:
      begin

        //string

        if p.stringsize>0 then
          i:=min(p.stringsize, 510)
        else
          i:=16; //it was a data type

        if readprocessmemory(processhandle, pointer(address), stringbuf, i, x) then
        begin
          stringbuf[i]:=#0;
          stringbuf[i+1]:=#0;

          if p.stringsize<=0 then
          begin
            //guess the stringtype (if it is one to begin with)
            if stringbuf[1]=#0 then
              p.unicode:=true;

          end;


          if p.unicode then
            result:=widestringbuf
          else
            result:=stringbuf;

        end;

      end
      else
        result:=rsSPSUNotYetImplemented;

    end;
  end;

end;

function TPointerfileReader.getStringAndAddress(index: qword; var address: ptruint; out p: PPointerRecord; shadow: ptruint; shadowsize: integer): string;
begin
  result:='';
  p:=getPointerRec(index);
  if p<>nil then
  begin
    result:=getStringFromPointerRecord(p, address, shadow, shadowsize);
    address:=getAddressFromPointerRecord(p, address, shadow, shadowsize);
  end;
end;

constructor TPointerfileReader.create(filename: string);
begin
  ffilename:=filename;
  pointerfile:=TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  pointerfile.ReadBuffer(pointerfileLevelwidth, sizeof(pointerfileLevelwidth));

  entrysize:=(pointerfileLevelwidth+4)*sizeof(dword); //+4 for : Levelsize of pointer, stringsize and the isunicode boolean and levelwidth is based on 0 (0=1 offset, 1=2 offsets, etc..)
  fcount:=(pointerfile.size-sizeof(pointerfileLevelwidth)) div (entrysize);

  getmem(pointerrecords, entrysize*4096);

  getmem(stringbuf, 512);
  widestringbuf:=pwidechar(stringbuf);

  pointermap:=TMap.Create(ituPtrSize,sizeof(pointer))

end;

destructor TPointerfileReader.destroy;
begin
  if pointerfile<>nil then
    freeandnil(pointerfile);

  if pointerrecords<>nil then
    FreeMemAndNil(pointerrecords);

  if stringbuf<>nil then
    FreeMemAndNil(stringbuf);

  if pointermap<>nil then
    freeandnil(pointermap);

  //cleanup the maps
  inherited destroy;
end;



//--------------TRescan----------------
procedure TRescan.flushResults;
begin
  lastwrite:=GetTickCount;
  outputfile.WriteBuffer(results.Memory^, results.position);
  results.position:=0;
end;

procedure TRescan.addPointer(p: PPointerRecord);
begin
  inc(fcount);
  results.WriteBuffer(p^, pointerfilereader.entrysize);

  if ((gettickcount-lastwrite)>5*60*1000) or (results.Position>=(15*1024*1024)) then
    flushResults;
end;

function TRescan.checkByte(p: PPointerRecord): boolean;
var error: boolean;
  a: ptruint;
  v,v2: byte;
begin
  result:=false;
  a:=pointerfilereader.getAddressFromPointerRecord(p, address, shadow, shadowsize);
  if (a<>0) and ((not mustbeinregion) or (InRangeX(a, pointerstart, pointerend)))  then
  begin
    v:=pointerfilereader.getByteFromAddress(a, error);
    result:=not error;


    if result and (address2<>0) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2, shadow2, shadowsize2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getByteFromAddress(a, error);
        result:=not error;

        if result and (diffkind<>dkDontcare) then
        begin
          if diffkind=dkMustBeDifferent then
            result:=v<>v2
          else
            result:=v=v2; //must be equal
        end;

      end
      else
        result:=false;
    end;
  end;
end;

function TRescan.checkWord(p: PPointerRecord): boolean;
var error: boolean;
  a: ptruint;
  v,v2: word;
begin
  result:=false;
  a:=pointerfilereader.getAddressFromPointerRecord(p, address, shadow, shadowsize);
  if (a<>0) and ((not mustbeinregion) or (InRangeX(a, pointerstart, pointerend)))  then
  begin
    v:=pointerfilereader.getWordFromAddress(a, error);
    result:=not error;

    if result and (address2<>0) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2, shadow2, shadowsize2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getWordFromAddress(a, error);
        result:=not error;

        if result and (diffkind<>dkDontCare) then
        begin
          if diffkind=dkMustBeDifferent then
            result:=v<>v2
          else
            result:=v=v2; //must be equal
        end;

      end
      else
        result:=false;
    end;
  end;
end;

function TRescan.checkDWord(p: PPointerRecord): boolean;
var error: boolean;
  a: ptruint;
  v,v2: Dword;
begin
  result:=false;
  a:=pointerfilereader.getAddressFromPointerRecord(p, address, shadow, shadowsize);
  if (a<>0) and ((not mustbeinregion) or (InRangeX(a, pointerstart, pointerend)))  then
  begin
    v:=pointerfilereader.getDwordFromAddress(a, error);
    result:=not error;

    if result and (address2<>0) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2, shadow2, shadowsize2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getDwordFromAddress(a, error);
        result:=not error;

        if result and (diffkind<>dkDontCare) then
        begin
          if diffkind=dkMustBeDifferent then
            result:=v<>v2
          else
            result:=v=v2; //must be equal
        end;

      end
      else
        result:=false;
    end;
  end;
end;

function TRescan.checkQWord(p: PPointerRecord): boolean;
var error: boolean;
  a: ptruint;
  v,v2: Qword;
begin
  result:=false;
  a:=pointerfilereader.getAddressFromPointerRecord(p, address, shadow, shadowsize);
  if (a<>0) and ((not mustbeinregion) or (InRangeX(a, pointerstart, pointerend)))  then
  begin
    v:=pointerfilereader.getQwordFromAddress(a, error);
    result:=not error;

    if result and (address2<>0) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2, shadow2, shadowsize2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getQwordFromAddress(a, error);
        result:=not error;

        if result and (diffkind<>dkDontCare) then
        begin
          if diffkind=dkMustBeDifferent then
            result:=v<>v2
          else
            result:=v=v2; //must be equal
        end;

      end
      else
        result:=false;
    end;
  end;
end;

function TRescan.checkSingle(p: PPointerRecord): boolean;
var error: boolean;
  a: ptruint;
  v,v2: Single;
begin
  result:=false;
  a:=pointerfilereader.getAddressFromPointerRecord(p, address, shadow, shadowsize);
  if (a<>0) and ((not mustbeinregion) or (InRangeX(a, pointerstart, pointerend)))  then
  begin
    v:=pointerfilereader.getSingleFromAddress(a, error);
    result:=not error;

    if result and (address2<>0) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2, shadow2, shadowsize2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getSingleFromAddress(a, error);
        result:=not error;

        if result and (diffkind<>dkDontCare) then
        begin
          if diffkind=dkMustBeDifferent then
            result:=v<>v2
          else
            result:=v=v2; //must be equal
        end;

      end
      else
        result:=false;
    end;
  end;
end;

function TRescan.checkDouble(p: PPointerRecord): boolean;
var error: boolean;
  a: ptruint;
  v,v2: Double;
begin
  result:=false;
  a:=pointerfilereader.getAddressFromPointerRecord(p, address, shadow, shadowsize);
  if (a<>0) and ((not mustbeinregion) or (InRangeX(a, pointerstart, pointerend)))  then
  begin
    v:=pointerfilereader.getDoubleFromAddress(a, error);
    result:=not error;

    if result and (address2<>0) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2, shadow2, shadowsize2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getDoubleFromAddress(a, error);
        result:=not error;

        if result and (diffkind<>dkDontCare) then
        begin
          if diffkind=dkMustBeDifferent then
            result:=v<>v2
          else
            result:=v=v2; //must be equal
        end;

      end
      else
        result:=false;
    end;
  end;
end;

function TRescan.checkPointer(p: PPointerRecord): boolean;
var error: boolean;
  a: ptruint;
  v,v2: PtrUint;
begin
  result:=false;
  a:=pointerfilereader.getAddressFromPointerRecord(p, address, shadow, shadowsize);
  if (a<>0) and ((not mustbeinregion) or (InRangeX(a, pointerstart, pointerend)))  then
  begin
    v:=pointerfilereader.getPointerFromAddress(a, error);
    result:=not error;

    if result and (address2<>0) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2, shadow2, shadowsize2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getPointerFromAddress(a, error);
        result:=not error;

        if result and (diffkind<>dkDontCare) then
        begin
          if diffkind=dkMustBeDifferent then
            result:=v<>v2
          else
            result:=v=v2; //must be equal
        end;

      end
      else
        result:=false;
    end;
  end;
end;



procedure TRescan.execute;
var i,j: integer;
  p: PPointerRecord;

  s,s2: string;
  passed: boolean;
  index,len: integer;
begin

  try
    for i:=0 to pointerfilereader.count-1 do
    begin
      p:=pointerfilereader.getPointerRec(i);

      case vartype of
        vtByte: passed:=checkByte(p);
        vtWord: passed:=checkWord(p);
        vtDword: passed:=checkDword(p);
        vtQword: passed:=checkQword(p);
        vtSingle: passed:=checkSingle(p);
        vtDouble: passed:=checkDouble(p);
        vtPointer: passed:=checkPointer(p);


        vtString:   //todo, move to checkString
        begin
          s:=pointerfilereader.getStringFromPointerRecord(p,address, shadow, shadowsize);

          if (address<>0) and ((not mustbeinregion) or (InRangeX(address, pointerstart, pointerend))) then
          begin

            if s<>'' then
            begin
              if regex<>nil then
              begin
                //use the regex engine to test this string
                index:=0;
                len:=0;
                passed:=RegExprPos(regex, pchar(s), index, len);

                if passed and mustbestart then
                  passed:=index=0;
              end
              else
              begin
                passed:=true;
                if isstringscan then
                begin //check if the first 4 characters are valid

                  for j:=1 to min(length(s), 4) do
                    if not (s[j] in [#$20..#$7f]) then
                    begin
                      passed:=false;
                      break;
                    end;

                end;
              end;
            end
            else
              passed:=false;


            if passed and (address2<>0) then
            begin
              s2:=pointerfilereader.getStringFromPointerRecord(p, address2, shadow2, shadowsize2);

              if (s2<>'') then
              begin
                if (diffkind<>dkDontCare) then
                begin
                  if diffkind=dkMustBeDifferent then
                    passed:=s<>s2
                  else
                    passed:=s=s2;
                end;
              end
              else
                passed:=false;

              if (passed) and (regex<>nil) then
              begin
                index:=0;
                len:=0;
                passed:=RegExprPos(regex, pchar(s2), index, len);

                if passed and mustbestart then
                  passed:=index=0;
              end;
            end;

          end //else the address is not in the region
        end;
      end;

      if passed then //add it
        addPointer(p);

      //check for forced exit
      if terminated then exit;

      fCurrentPosition:=i;
    end;

    flushresults;



  finally
    if outputfile<>nil then
      freeandnil(outputfile);

    if results<>nil then
      freeandnil(results);

    if pointerfilereader<>nil then
      freeandnil(pointerfilereader);

    if deletefile(outputfilename)=false then
      OutputDebugString('Failure deleting '+outputfilename);

    RenameFile(outputfilename+'.temp', outputfilename);

    Queue(TfrmStringPointerScan(ownerFrmStringPointerScan).scandone);
  end;


end;

constructor TRescan.create(suspended: boolean; address, address2: ptruint; mustbeinregion: boolean; pointerstart, pointerend: ptruint; isstringscan, caseSensitive, mustbestart: boolean; regExstr: string; diffkind: TDiffkind; vartype: TVariableType; oldpointerfilename: string; outputfilename: string ; ownerFrmStringPointerScan: TCustomForm);
var regflags: tregexprflags;
  lw: integer;
begin
  self.vartype:=vartype;

  self.isstringscan:=isstringscan;
  self.mustbestart:=mustbestart;
  self.diffkind:=diffkind;


  self.oldpointerfilename:=oldpointerfilename;
  pointerfilereader:=TPointerfileReader.create(oldpointerfilename);
  pointerfilereader.vartype:=vartype;


  self.address:=address;
  self.address2:=address2;
  self.mustbeinregion:=mustbeinregion;
  self.pointerstart:=pointerstart;
  self.pointerend:=pointerend;
  self.outputfilename:=outputfilename;

  self.ownerFrmStringPointerScan:=ownerFrmStringPointerScan;


  pointerfilereader.clearPointercache;


  if isstringscan then
  begin
    if regexstr<>'' then
    begin
      if CaseSensitive then
        regflags:=[]
      else
        regflags:=[ref_caseinsensitive];

      self.regex:=GenerateRegExprEngine(pchar(regexstr), regflags);
    end;
  end;



  outputfile:=TFileStream.Create(outputfilename+'.temp', fmCreate or fmShareDenyNone);
  outputfile.Free;     //so it can be reopened by other processes
  outputfile:=TFileStream.create(Outputfilename+'.temp', fmOpenWrite or fmShareDenyNone);


  lastwrite:=GetTickCount;

  lw:=pointerfilereader.levelWidth;
  outputfile.WriteBuffer(lw, sizeof(lw));


  results:=TMemoryStream.Create;

  results.size:=16*1024*1024;

  inherited create(suspended);
end;


destructor TRescan.destroy;
begin
  if pointerfilereader<>nil then
    freeandnil(pointerfilereader);

  if outputfile<>nil then
    freeandnil(outputfile);

  if results<>nil then
    freeandnil(results);

  inherited destroy;
end;

//--------------TScanner---------------
function TScanner.getPointerValue(address: ptruint; column: integer): ptruint;
{
returns 0 if not found
}
var
  search: TPointerListEntry;
  pe: TAvgLvlTreeNode;
  r: PPointerListEntry;
begin
  if column=1 then
  begin
    if (shadow<>0) and (InRangeQ(address, baseaddress, baseaddress+shadowsize)) then
      address:=address+(shadow-baseaddress);
  end
  else
  begin
    if (shadow2<>0) and (InRangeQ(address, baseaddress2, baseaddress2+shadowsize2)) then
      address:=address+(shadow2-baseaddress2);
  end;

  result:=0;
  mapRegionIfNeeded(address,structsize);
  search.address:=address;
  pe:=pointerlist.Find(@search);
  if pe<>nil then
  begin
    r:=pe.data;
    result:=r.pointsto;
  end;
end;

function TScanner.getFirstPointerEntry(base: ptruint): PPointerListEntry;
var pe: TAvgLvlTreeNode;
  search: TPointerListEntry;
  r: PPointerListEntry;
begin
  r:=nil;
  zeromemory(@search, sizeof(search));
  search.address:=base;

  pe:=pointerlist.FindNearest(@search);

  if pe<>nil then
  begin
    r:=pe.data;

    while (r<>nil) and (r.address<base) do
      r:=r.next;

  end;

  result:=r;
end;

procedure TScanner.fillPointers(base: ptruint; size: integer);
var x: ptruint;
  i: integer;
  p: PPointerListEntry;
  pe, prev, next: TAvgLvlTreeNode;
  a: ptruint;
begin
  if readprocessmemory(processhandle, pointer(base), fillpointerblock, size, x) then
  begin
    //walk through the array of dwords
    if is64bittarget then
    begin
      for i:=0 to (x div 8)-1 do
      begin
        a:=PPtrUintArray(fillpointerblock)[i];

        if shadow<>0 then
        begin
          if InRangeQ(a, baseaddress, baseaddress+shadowsize) then
            a:=a+(shadow-baseaddress);

          PPtrUintArray(fillpointerblock)[i]:=a;
        end;

        if (a>$10000) and ((a mod 8)=0) and (isreadable(a)) then    //<-- isReadable can be sped up by caching in a tree
        begin
          p:=bma.alloc(sizeof(TPointerListEntry));

          p.address:=base+(i*8);
          p.pointsto:=PPtrUintArray(fillpointerblock)[i];

          pe:=pointerlist.Add(p);
          prev:=pointerlist.FindPrecessor(pe);
          next:=pointerlist.FindSuccessor(pe);

          if prev=nil then
            p.previous:=nil
          else
          begin
            p.previous:=prev.Data;
            PPointerListEntry(prev.data).next:=p;
          end;

          if next=nil then
            p.next:=nil
          else
          begin
            p.next:=next.Data;
            PPointerListEntry(next.data).previous:=p;
          end;


        end;
      end;
    end
    else
    begin
      for i:=0 to (x div 4)-1 do
      begin
        a:=PDwordArray(fillpointerblock)[i];

        if shadow<>0 then
        begin
          if InRangeQ(a, baseaddress, baseaddress+shadowsize) then
            a:=a+(shadow-baseaddress);

          PDwordArray(fillpointerblock)[i]:=a;
        end;

        if (a>$10000) and ((a mod 4)=0) and (isreadable(a)) then
        begin
          p:=bma.alloc(sizeof(TPointerListEntry));

          p.address:=base+(i*4);
          p.pointsto:=PDwordArray(fillpointerblock)[i];

           //   debug code
         { if pointerlist.Find(p)<>nil then
          begin
         //   freemem(p);
            messagebox(0,rsSPSUFUUUU,rsSPSUFUUUUU,0);
            continue;
          end;   }

          pe:=pointerlist.Add(p);
          prev:=pointerlist.FindPrecessor(pe);
          next:=pointerlist.FindSuccessor(pe);

          if prev=nil then
            p.previous:=nil
          else
          begin
            p.previous:=prev.Data;
            PPointerListEntry(prev.data).next:=p;
          end;

          if next=nil then
            p.next:=nil
          else
          begin
            p.next:=next.Data;
            PPointerListEntry(next.data).previous:=p;
          end;


        end;
      end;
    end;

  end;
end;

procedure TScanner.mapRegionIfNeeded(blockaddress: ptruint; size: integer);
var search: TMappedRegion;
  result: TAvgLvlTreeNode;

   map, oldmap: PMappedRegion;

  prev, next: TAvgLvlTreeNode;

  size2: integer;
begin
  search.baseaddress:=blockaddress;
  result:=mappedregions.FindNearest(@search);

  if result<>nil then
    map:=result.data
  else
    map:=nil;

  if (map<>nil) and (not ((map.baseaddress<=blockaddress) and (map.baseaddress+map.size>=blockaddress))) then
  begin
    //if the found map isn't a good enough match then check the next and previous one
    if (map.next<>nil) and ((map.next.baseaddress<=blockaddress) and (map.next.baseaddress+map.next.size>=blockaddress)) then
      map:=map.next
    else
    if (map.previous<>nil) and ((map.previous.baseaddress<=blockaddress) and (map.previous.baseaddress+map.previous.size>=blockaddress)) then
      map:=map.previous
    else
      map:=nil;

  end;

  if (map<>nil) then
  begin
    //this map can contain or add the extra bytes needed

    size2:=((blockaddress+size) - (map.baseaddress+map.size)); //size now contains the total bytes that need to appended to this map
    if size2>0 then
    begin
      inc(blockaddress, size-size2);
      size:=size2;

      if (map.next<>nil) and (map.next.baseaddress<blockaddress+size) then
      begin
        //adjust the size so it won't overlap with the next map
        size2:=map.next.baseaddress-(map.baseaddress+map.size);
      end;

      if size2>0 then
      begin
        fillPointers(map.baseaddress+map.size, size2);
        map.size:=map.size+size2;
        dec(size, size2);
      end;

      if (size>0) then //this map has been filled as much as it can, try the next one
        mapRegionIfNeeded(blockaddress, size);
    end; //else it's already fully mapped
  end
  else
  begin
    //create a new one
//    map:=bma.alloc(sizeof(TMappedRegion));
    //no bma, a map can be freed
    map:=getmem(sizeof(TMappedRegion));

    //ZeroMemory(map, sizeof(TMappedRegion));
    map.baseaddress:=blockaddress;


    result:=mappedregions.Add(map);
    prev:=mappedregions.FindPrecessor(result);
    next:=mappedregions.FindSuccessor(result);

    if prev=nil then
      map.previous:=nil
    else
    begin
      map.previous:=prev.Data;
      PMappedRegion(prev.data).next:=map;
    end;

    if next=nil then
      map.next:=nil
    else
    begin
      map.next:=next.Data;
      PMappedRegion(next.data).previous:=map;
    end;

    map.size:=structsize;
    //adjust the size so it won't overlap with the next block
    if (map.next<>nil) and (map.next.baseaddress<map.baseaddress+map.size) then
      map.size:=map.next.baseaddress-map.baseaddress;

    fillPointers(map.baseaddress, map.size);

    if (map.next<>nil) and (map.next.baseaddress=map.baseaddress+map.size) then
    begin
      //merge (delte old one)
      result:=mappedregions.Find(map.next);
      if result<>nil then
      begin
        oldmap:=result.data;    //adjust the linked list
        map.next:=oldmap.next;
        if map.next<>nil then
          map.next.previous:=map;

        map.size:=map.size+oldmap.size;

        mappedregions.Remove(oldmap);
        FreeMemAndNil(oldmap);
      end;
    end;
  end;

end;

procedure TScanner.flushResults;
begin
  lastwrite:=GetTickCount;
  resultfile.WriteBuffer(results.Memory^, results.position);
  results.position:=0;
end;

function TScanner.getAddressFromPath(base :ptruint; column: integer; level: integer; const path: TPointerPath): ptruint;
var x: ptruint;
  i: integer;
begin
  result:=base+path[0];

  if column=1 then
  begin
    if (shadow<>0) and (InRangeQ(result, baseaddress, baseaddress+shadowsize)) then
      result:=result+(shadow-baseaddress);
  end
  else
  begin
    if (shadow2<>0) and (InRangeQ(result, baseaddress2, baseaddress2+shadowsize2)) then
      result:=result+(shadow2-baseaddress2);
  end;


  for i:=1 to level do
  begin
    result:=getPointerValue(result, column);
    if result=0 then exit;

    result:=result+path[i];
  end;
end;

function TScanner.comparePath(level: integer; path: tpointerpath; stringsize: integer): boolean;
var i: integer;
  x: ptruint;
  address, address2: ptruint;
  e: boolean;

  value,value2: pbytearray;
  br: ptruint;
begin
  result:=false;

  address2:=getAddressFromPath(baseaddress2, 2, level, path);
  if address2=0 then exit;

  address:=getAddressFromPath(baseaddress, 1, level, path);
  if address=0 then exit; //weird


  //still here so both addresses are readable and not the same address
  //
  if valuemap<>nil then
  begin
    value:=valuemap.GetDataPtr(address);

    if address2=address then //if they both have the same address assign th same map pointer
      value2:=value
    else
      value2:=valuemap.GetDataPtr(address2);
  end
  else
  begin
    value:=nil;
    value2:=nil;
  end;

  //check address1
  if value=nil then //not found, so read it manually
  begin
    if readprocessmemory(processhandle, pointer(address), @tempvariablebuffer[1], variablesize, br) then
    begin
      tempvariablebuffer[0]:=1; //mark as readable
      value:=tempvariablebuffer;

      if address2=address then
        value2:=value;
    end
    else
      tempvariablebuffer[0]:=0; //mark as unreadable

    if valuemap<>nil then //add it
      valuemap.Add(address, tempvariablebuffer^);
  end;

  if value=nil then exit; //unreadable

  //check address2
  if value2=nil then //not found, so read it manually
  begin
    if readprocessmemory(processhandle, pointer(address2), @tempvariablebuffer2[1], variablesize, br) then
    begin
      tempvariablebuffer2[0]:=1; //mark as readable
      value2:=tempvariablebuffer2;
    end
    else
      tempvariablebuffer2[0]:=0; //mark as unreadable

    if valuemap<>nil then //add it
      valuemap.Add(address2, tempvariablebuffer2^);
  end;

  if value2=nil then exit; //unreadable

  if value[0]=0 then exit; //marked as unreadable
  if value2[0]=0 then exit; //  "    "     "

  if diffkind=dkDontCare then exit(true);

  value:=@value[1];
  value2:=@value2[1];



  if address=address2 then
  begin
    result:=diffkind=dkMustBeSame;
    exit;
  end;

  //still here so not the same address
  if isDataScan then
  begin
    result:=CompareMem(value, value2, variablesize);

    if diffkind=dkMustBeDifferent then result:=not result; //invert the result if it's a mustbedifferent type
  end
  else
  begin
    result:=CompareMem(value, value2, min(stringsize, variablesize));
    if diffkind=dkMustBeDifferent then result:=not result; //invert the result if it's a mustbedifferent type
  end;


end;

function TScanner.addStringPath(level: integer; path: tpointerpath; stringsize: integer; unicode: BOOL): boolean;
begin
  result:=false;
  if (baseaddress2<>0) and (not comparePath(level, path, stringsize)) then exit;


  results.WriteBuffer(level, sizeof(level));
  results.WriteBuffer(stringsize, sizeof(stringsize));
  results.WriteBuffer(unicode, sizeof(unicode));
  results.WriteBuffer(path[0], sizeof(path[0])*(maxlevel+1));

  if (getTickCount-lastwrite>5*60*1000) or (results.Position>=(15*1024*1024)) then
    flushResults;

  inc(count);

  result:=true;
end;

procedure TScanner.handleBlock(blockaddress: ptruint; level: integer; path: TPointerpath);
var
  x: dword;
  i: integer;

  newaddress: ptruint;

  p: Pstringdata;

 // block: pdwordarray;

  pe: PPointerListEntry;
begin
  if terminated then exit;

  if shadow<>0 then
  begin
    if inrangeq(blockaddress, baseaddress, baseaddress+shadowsize) then
      blockaddress:=blockaddress+(shadow-baseaddress);
  end;


  if level>0 then
  begin
    progress[level-1]:=path[level-1];
    if (diffkind<>dkDontCare) and (getAddressFromPath(baseaddress2, 2, level-1, path)=0) then exit; //nothing to be found in this block
  end;

  if isDataScan then
  begin
    i:=0;
    while i<structsize do
    begin
      path[level]:=i;


      if (not mustbeinregion) or (InRangeX(blockaddress+i, pointerstart, pointerstop)) then
        addStringPath(level, path, -1,false);

      inc(i, alignment);
    end;
  end
  else
  begin
    //check which strings can be found in this block
    p:=frmStringMap.findNearestString(blockaddress);

    if p<>nil then
    begin
      while (p<>nil) and (p.address<blockaddress+structsize) do       //found a string, add it
      begin
        if p.address>=blockaddress then
        begin
          path[level]:=p.address-blockaddress;

          if (not mustbeinregion) or (InRangeX(p.address, pointerstart, pointerstop)) then
            addStringPath(level, path, p.stringsize, p.unicode);

        end
        else
        begin
          path[level]:=0;
          if (not mustbeinregion) or (InRangeX(blockaddress, pointerstart, pointerstop)) then
            addStringPath(level, path, p.stringsize-(blockaddress-p.address), p.unicode);

        end;
        p:=p.next;
      end;
    end;

  end;

  if level>=maxlevel then exit; //max level reached, no need to scan for pointers in this block


  //still here, so go through this block looking for pointers

  //see if this block already has it's pointers mapped, if not, map them
  mapRegionIfNeeded(blockaddress, structsize);

  pe:=getFirstPointerEntry(blockaddress);
  while (pe<>nil) and (pe.address<blockaddress+structsize) do
  begin
    path[level]:=pe.address-blockaddress;
    handleBlock(pe.pointsto, level+1, path);
    pe:=pe.next;
  end;
end;

procedure TScanner.execute;
var pointerpath: TPointerpath;
  i: integer;
begin
  try


    try


      setlength(pointerpath, maxlevel+1); //maxlevel=0 means 1 offset
     { setlength(levelblock, maxlevel+1);
      for i:=0 to length(levelblock)-1 do
        getmem(levelblock[i], structsize); //one time alloc for each level so it can be reused without having to reallocate each recursion
     }

      getmem(fillpointerblock, max(4096, structsize));

      results:=TMemoryStream.Create;
      results.Size:=16*1024*1024;
      results.Position:=0;


      handleBlock(baseaddress, 0, pointerpath);
      flushResults;


    except
      on e: exception do
        messagebox(0,pchar(rsSPSUException+e.Message),'ps',0);
    end;


  finally
    //reached the end, tell the main thread that the scan is done
    Queue(TfrmStringPointerScan(ownerFrmStringPointerScan).scandone);
  end;
end;

constructor TScanner.create(isDataScan, mustbeinregion: boolean; alignment: integer; pointerstart, pointerstop: ptruint; baseaddress: ptruint; shadow: ptruint; shadowsize: ptruint; baseaddress2: ptruint; shadow2: ptruint; shadowsize2: integer; diffkind: TDiffkind; vartype: TVariableType; mapvalues: boolean; structsize: integer; maxlevel: integer; mappedregions,pointerlist: TAvgLvlTree; bma: TBigMemoryAllocHandler; filename: string; ownerFrmStringPointerScan: TCustomForm);
begin
  self.is64bittarget:=processhandler.is64Bit;
  self.baseaddress:=baseaddress;
  self.shadow:=shadow;
  self.shadowsize:=shadowsize;
  self.baseaddress2:=baseaddress2;
  self.shadow2:=shadow2;
  self.shadowsize2:=shadowsize2;
  self.diffkind:=diffkind;
  self.vartype:=vartype;
  self.structsize:=structsize;
  self.maxlevel:=maxlevel;
  self.mappedregions:=mappedregions;
  self.pointerlist:=pointerlist;
  self.bma:=bma;

  self.isDataScan:=isDataScan;
  self.alignment:=alignment;
  self.pointerstart:=pointerstart;
  self.pointerstop:=pointerstop;
  self.mustbeinregion:=mustbeinregion;

  self.ownerFrmStringPointerScan:=ownerFrmStringPointerScan;

  //if diffkind<>dkDontCare then
  begin
    case vartype of
      vtString: variablesize:=8; //string
      vtByte: variablesize:=1; //byte
      vtWord: variablesize:=2; //word
      vtDword: variablesize:=4; //dword
      vtQword: variablesize:=8; //qword
      vtSingle: variablesize:=4; //single
      vtDouble: variablesize:=8; //double
      vtPointer: variablesize:=processhandler.pointersize; //pointer
    end;

    if mapvalues then
      valuemap:=TMap.create(ituPtrSize,variablesize+1);

    getmem(tempvariablebuffer, variablesize+1);
    getmem(tempvariablebuffer2, variablesize+1);
  end;

  resultfile:=TFileStream.Create(filename, fmCreate or fmShareDenyNone);
  resultfile.Free;
  resultfile:=TFileStream.Create(filename, fmOpenWrite or fmShareDenyNone); //make it accessible by other files

  resultfile.Write(maxlevel, sizeof(maxlevel));

  count:=0;
  setlength(progress,maxlevel+1);

  inherited create(false); //let's get started...
end;

destructor TScanner.destroy;
var i: integer;
begin
 { for i:=0 to length(levelblock)-1 do
    freemem(levelblock[i]); }


  if fillpointerblock<>nil then
    FreeMemAndNil(fillpointerblock);

  if resultfile<>nil then
    freeandnil(resultfile);

  if tempvariablebuffer<>nil then
    FreeMemAndNil(tempvariablebuffer);

  if tempvariablebuffer2<>nil then
    FreeMemAndNil(tempvariablebuffer2);

  if valuemap<>nil then
    freeandnil(valuemap);
end;


//----------------------------


function TfrmStringPointerScan.getStringFromPointer(address: ptruint; offsets: TDwordArray; level, bytesize: integer; unicode: boolean; var a: ptruint): string;
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

procedure TfrmStringPointerScan.ListView1CustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  defaultdraw:=true;

  if item.data<>nil then
    sender.Canvas.Font.Color:=clblue;
end;



procedure TfrmStringPointerScan.ListView1Data(Sender: TObject; Item: TListItem);
var
  i: integer;
  s,s2: string;
  a: ptruint;

  a2: ptruint;


  p: PPointerRecord;
begin
  if pointerfilereader<>nil then
  begin
    item.data:=nil;

    a:=address;

    s:=pointerfilereader.getStringAndAddress(item.index, a, p, shadow, shadowsize);



    if (shadow<>0) and (inrangeq(a, address, address+shadowsize)) then
      a:=a+(shadow-address);

    if p<>nil then
    begin
      item.caption:=inttohex(p.offset[0],1);
      for i:=1 to p.level do
        item.SubItems.add(inttohex(p.offset[i],1));

      for i:=p.level+1 to pointerfilereader.levelWidth do
        item.SubItems.Add('');
    end;

    if a>0 then
      item.SubItems.Add(inttohex(a,8)+' : '+s)
    else
      item.subitems.add('??? : ???');


    if hasAddress2 then
    begin
      a:=address2;
      s2:=pointerfilereader.getStringAndAddress(item.index, a, p, shadow2, shadowsize2);
      if a>0 then
        item.SubItems.Add(inttohex(a,8)+' : '+s2)
      else
        item.subitems.add('??? : ???');

      if s<>s2 then
        item.data:=pointer(1);
    end;
  end;
end;

procedure TfrmStringPointerScan.ListView1DblClick(Sender: TObject);
begin
  if listview1.Selected<>nil then
    MemoryBrowser.hexview.address:=pointerfilereader.getAddressFromPointerRecord(pointerfilereader.getPointerRec(listview1.Selected.Index), address, shadow, shadowsize2);
end;

procedure TfrmStringPointerScan.miFindClick(Sender: TObject);
begin
  finddialog1.execute;
end;

procedure TfrmStringPointerScan.OpenPointerfile(filename: string);
var lc: TListColumn;
  i: integer;
begin
  cleanup;


  pointerfilereader:=TPointerfileReader.Create(filename);
  comboType.OnChange(comboType);


  for i:=0 to pointerfilereader.levelWidth do
  begin
    lc:=listview1.Columns.Add;
    lc.MinWidth:=2;
    lc.Width:=70;
    lc.Caption:=rsSPSUOffset+inttostr(i);
  end;

  lc:=listview1.Columns.Add;
  lc.MinWidth:=2;
  lc.Width:=120;
  lc.Caption:=rsSPSUAddress;

  lc:=listview1.Columns.Add;
  lc.MinWidth:=2;
  lc.Width:=120;
  lc.Caption:=rsSPSUAddress2;


  lblExtra.enabled:=true;
  edtExtra.Enabled:=true;

  edtExtraChange(edtExtra);


  listview1.items.count:=dword(min(dword(1000000), dword(pointerfilereader.count)));

  //setup rescan mode

  btnScan.caption:=rsSPSURescan;
  btnScan.tag:=1;
end;

procedure TfrmStringPointerScan.scanDone;
begin
  if scanner<>nil then
    lblInfo.caption:=rsSPSUFound+inttostr(scanner.count);

  if rescanner<>nil then
    lblInfo.caption:=rsSPSUFound+inttostr(rescanner.count);


  cleanup;

  OpenPointerfile(SaveDialog1.FileName);

  btnScan.caption:=rsSPSURescan;
  btnScan.tag:=1;
  btnScan.Left:=panel1.clientwidth-btnScan.width-btnNewScan.left;

  btnNewScan.visible:=true;
  btnNewScan.enabled:=true;


  if not rbDiffDontCare.checked then
    comboType.itemindex:=comboCompareType.itemindex;

  if pointerfilereader<>nil then
    showmessage(rsSPSUScanDoneFound+inttostr(pointerfilereader.count))
  else
    raise exception.create(rsSPSUErrorduringScanNoScanresults);

  //restore the gui
  EnableGui;
end;

function TfrmStringPointerScan.mapCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  result:=CompareValue(PMappedRegion(Data1).baseaddress, PMappedRegion(Data2).baseaddress);
end;

function TfrmStringPointerScan.pointerCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  result:=CompareValue(PPointerListEntry(Data1).address, PPointerListEntry(Data2).address);
end;

procedure tfrmStringPointerScan.cleanup;
var r: TAvgLvlTreeNode;
  p,m: PMappedRegion;
begin
  statusupdater.enabled:=false;
  progressbar1.Visible:=false;
  progressbar1.Position:=0;

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

  listview1.items.count:=0;
  while listview1.ColumnCount>0 do
    listview1.Columns.Delete(0);

  if bma<>nil then
    freeAndNil(bma);

  if pointerlist<>nil then
    freeandnil(pointerlist);

  {
  if pointerlistMemManager<>nil then
    freeandnil(pointerlistMemManager);    }

  if pointerlist<>nil then
    freeandnil(pointerlist);

  if mappedRegions<>nil then
  begin
    r:=mappedRegions.FindLowest;
    if r<>nil then
    begin
      m:=PMappedRegion(r.data);
      while m<>nil do
      begin
        p:=m;
        m:=m.next;
        FreeMemAndNil(p);
      end;
    end;
    freeandnil(mappedRegions);
  end;

  if pointerfilereader<>nil then
    freeandnil(pointerfilereader);

end;

procedure TfrmStringPointerScan.btnScanClick(Sender: TObject);
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
begin

  vartype:=vtPointer;
  if (scanner=nil) and (rescanner=nil) then
  begin
    baseaddress:=symhandler.getAddressFromName(edtBase.text);
    baseaddress2:=0;

    shadow:=0;
    shadowsize:=0;
    shadow2:=0;
    shadowsize2:=0;

    pointerstart:=0;
    pointerstop:=0;
    alignsize:=4;

    if cbHasShadow.checked then
    begin
      shadow:=symhandler.getAddressFromName(edtShadowAddress.text);
      shadowsize:=strtoint(edtShadowSize.text);
    end;

    if ((shadow<>0) and (not isreadable(shadow))) or
       ((shadow=0) and (not isreadable(baseaddress))) then
      raise exception.create(rsThisAddressIsNotAccessible);

    structsize:=strtoint(edtStructsize.text);
    maxlevel:=strtoint(edtMaxLevel.text);

    if edtExtra.text='' then
      rbDiffDontCare.Checked:=true
    else
    begin
      baseaddress2:=symhandler.getAddressFromName(edtExtra.text);
      if cbHasShadow2.checked then
      begin
        shadow2:=symhandler.getAddressFromName(edtShadowAddress2.text);
        shadowsize2:=strtoint(edtShadowSize2.text);
      end;
    end;

    if rbDatascan.checked then
    begin
      alignsize:=strtoint(edtAlignsize.text);

      if cbPointerInRange.checked then
      begin
        pointerstart:=symhandler.getAddressFromName(edtPointerStart.text);
        pointerstop:=symhandler.getAddressFromName(edtPointerStop.text);
      end;
    end;

    if savedialog1.execute then
    begin
      if pointerfilereader<>nil then
        oldpointerfile:=pointerfilereader.filename
      else
        oldpointerfile:='';

      cleanup;





      //we got till this point so everything is fine, disable the gui
      disableGui;

      if rbMustBeSame.checked then
        diffkind:=dkMustBeSame
      else
      if rbMustBeDifferent.checked then
        diffkind:=dkMustBeDifferent
      else
        diffkind:=dkDontCare;

      if diffkind=dkDontCare then
        vartype:=vtByte
      else
      case comboCompareType.itemindex of
        0: vartype:=vtString;
        1: vartype:=vtByte;
        2: vartype:=vtWord;
        3: vartype:=vtDword;
        4: vartype:=vtQword;
        5: vartype:=vtSingle;
        6: vartype:=vtDouble;
        7: vartype:=vtPointer;
      end;

      if rbStringscan.checked then
          vartype:=vtString;

      if btnScan.tag=0 then //first scan
      begin
        mappedRegions:=TAvgLvlTree.CreateObjectCompare(mapCompare);
        pointerlist:=TAvgLvlTree.CreateObjectCompare(pointerCompare);


        bma:=TBigMemoryAllocHandler.create;

        if rbStringscan.checked then
        begin
          vartype:=vtString;

          if (frmStringMap<>nil) and (cbReuseStringmap.checked=false) then
            freeandnil(frmStringMap);

          if frmStringMap=nil then
          begin
            frmStringMap:=tfrmStringMap.Create(application);

            //fill the stringmap
            frmstringmap.cbRegExp.checked:=cbRegExp.checked;
            frmstringmap.cbCaseSensitive.checked:=cbCaseSensitive.checked;
            frmstringmap.cbMustBeStart.checked:=cbMustBeStart.checked;
            frmstringmap.edtRegExp.text:=edtRegExp.text;

            frmstringmap.btnScan.click;
            lblInfo.caption:=rsGeneratingStringmap;
            lblInfo.Repaint;
            frmstringmap.scanner.WaitFor;

          end;

        end;
        lblInfo.caption:=rsGeneratedScanning;

        //everything has been configured



        scanner:=Tscanner.create(rbDatascan.checked, cbPointerInRange.checked, alignsize, pointerstart, pointerstop, baseAddress, shadow, shadowsize, baseaddress2, shadow2, shadowsize2, diffkind, vartype, cbMapPointerValues.checked, structsize, maxlevel, mappedRegions, pointerlist, bma, savedialog1.filename, self);

      end
      else
      begin
        //next scan aka Rescan
        listview1.items.count:=0;
        rescanner:=trescan.create(false, address, address2, cbpointerinrange.checked, pointerstart, pointerstop, rbStringscan.checked, cbCaseSensitive.checked, cbMustBeStart.checked, edtRegExp.text, diffkind, vartype, oldpointerfile, savedialog1.filename , self);
      end;
      btnScan.caption:=rsStop;
      btnScan.enabled:=true;
      progressbar1.visible:=true;
      statusupdater.enabled:=true;
    end;

  end
  else
  begin
    btnScan.enabled:=false;
    btnScan.caption:=rsTerminating;

    if scanner<>nil then
      scanner.terminate;

    if rescanner<>nil then
      rescanner.terminate;
  end;


end;

procedure TfrmStringPointerScan.cbHasShadowChange(Sender: TObject);
begin


    edtShadowAddress.visible:=cbHasShadow.checked or cbHasShadow2.checked;
    edtShadowAddress2.Visible:=edtShadowAddress.visible;
    lblSize.visible:=edtShadowAddress.visible;
    lblSize2.visible:=edtShadowAddress.visible;
    edtShadowSize.visible:=edtShadowAddress.visible;
    edtShadowSize2.visible:=edtShadowAddress.visible;
    edtBaseChange(edtBase);

    shadow:=0;
end;

procedure TfrmStringPointerScan.cbRegExpChange(Sender: TObject);
begin
  cbCaseSensitive.enabled:=rbstringscan.checked and cbRegExp.enabled and cbRegExp.checked;
  cbMustBeStart.enabled:=cbCaseSensitive.enabled;
  edtRegExp.enabled:=cbCaseSensitive.enabled;
  lblString.enabled:=cbCaseSensitive.enabled;
end;

procedure TfrmStringPointerScan.cbPointerInRangeChange(Sender: TObject);
begin
  edtPointerStart.enabled:=cbPointerInRange.checked;
  edtPointerStop.enabled:=cbPointerInRange.checked;
  lbland.enabled:=cbPointerInRange.checked;
end;

procedure TfrmStringPointerScan.cbReuseStringmapChange(Sender: TObject);
begin

end;

procedure TfrmStringPointerScan.comboTypeChange(Sender: TObject);
begin
  if pointerfilereader<>nil then
  begin
    case comboType.itemindex of
      0: pointerfilereader.vartype:=vtString;
      1: pointerfilereader.vartype:=vtByte;
      2: pointerfilereader.vartype:=vtWord;
      3: pointerfilereader.vartype:=vtDword;
      4: pointerfilereader.vartype:=vtQword;
      5: pointerfilereader.vartype:=vtSingle;
      6: pointerfilereader.vartype:=vtDouble;
      7: pointerfilereader.vartype:=vtPointer;
    end;

    listview1.Refresh;

  end;
end;

procedure TfrmStringPointerScan.edtBaseChange(Sender: TObject);
var
  err: boolean;
begin
  try
    address:=symhandler.getAddressFromName(edtBase.text);
    shadow:=symhandler.getAddressFromName(edtShadowAddress.text, false, err);
    if err then
      shadow:=0;

    try
      shadowsize:=strtoint(edtShadowSize.text);
    except
    end;


    address2:=symhandler.getAddressFromName(edtBase.text, false, err);
    if err then
      address2:=0;

    shadow2:=symhandler.getAddressFromName(edtShadowAddress2.text, false, err);
    if err then
      shadow2:=0;

    try
      shadowsize2:=strtoint(edtShadowSize.text);
    except
    end;


    listview1.Refresh;
  except
  end;
end;

procedure TfrmStringPointerScan.edtExtraChange(Sender: TObject);
begin
  hasAddress2:=edtExtra.Text<>'';

  rbMustBeDifferent.enabled:=hasAddress2;
  rbMustBeSame.enabled:=hasAddress2;
  rbDiffDontCare.enabled:=hasAddress2;

  try
    address2:=symhandler.getAddressFromName(edtExtra.text);
    listview1.Refresh;
  except
  end;

  rbDiffDontCareChange(nil);
end;

procedure TfrmStringPointerScan.FindDialog1Find(Sender: TObject);
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

  for i:=listview1.ItemIndex+1 to listview1.Items.Count-1 do
  begin
    s:=listview1.items[i].SubItems.Text;
    if not cs then s:=uppercase(s);

    if pos(FindText, s)>0 then
    begin
      listview1.ItemIndex:=i;
      listview1.Items[i].MakeVisible(false);
      exit;
    end;
  end;

  beep;
end;

procedure TfrmStringPointerScan.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  cleanup;
end;

procedure TfrmStringPointerScan.FormShow(Sender: TObject);
begin
  //panel1.Constraints.MinHeight:=btnNewScan.Top+btnNewScan.Height+lblInfo.Height+4;
  cbHasShadowChange(nil);
end;

procedure TfrmStringPointerScan.miNewScanClick(Sender: TObject);
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

    lblBaseRegion.enabled:=true;
    lblExtra.enabled:=true;
    edtBase.enabled:=true;
    edtExtra.enabled:=true;

    cbHasShadow.enabled:=true;
    cbHasShadow2.enabled:=true;

    edtShadowAddress.Enabled:=true;
    edtShadowAddress2.enabled:=true;

    edtShadowSize.enabled:=true;
    edtShadowSize2.enabled:=true;

    lblSize.enabled:=true;
    lblSize2.enabled:=true;






    btnNewScan.Visible:=false;
  end;
end;

procedure TfrmStringPointerScan.miOpenClick(Sender: TObject);
begin
  if (scanner=nil) and (rescanner=nil) and OpenDialog1.Execute then
  begin
    OpenPointerfile(opendialog1.filename);
    enablegui;
  end;


end;

procedure TfrmStringPointerScan.MenuItem7Click(Sender: TObject);
begin
  with Tfrmstringpointerscan.create(Application) do show;
end;

procedure TfrmStringPointerScan.miClearCacheClick(Sender: TObject);
begin
  if pointerfilereader<>nil then
    pointerfilereader.clearPointerCache;

  listview1.Refresh;
end;

procedure TfrmStringPointerScan.MenuItem6Click(Sender: TObject);
begin

end;

procedure TfrmStringPointerScan.Panel3Click(Sender: TObject);
begin

end;

procedure TfrmStringPointerScan.rbDatascanChange(Sender: TObject);
begin
  cbRegExp.enabled:=rbStringscan.enabled and rbStringscan.checked;
  cbCaseSensitive.enabled:=cbRegExp.enabled and rbStringscan.checked and cbRegExp.checked;
  cbMustBeStart.enabled:=cbCaseSensitive.enabled;
  lblString.enabled:=cbCaseSensitive.enabled;
  edtRegExp.enabled:=cbCaseSensitive.enabled;
  cbReuseStringmap.enabled:=cbCaseSensitive.enabled;

  lblAlign.enabled:=rbDatascan.enabled and rbDatascan.checked;
  edtAlignsize.enabled:=lblAlign.enabled;
  edtPointerStart.enabled:=cbPointerInRange.enabled and cbPointerInRange.checked;
  lblAnd.enabled:=edtPointerStart.enabled;
  edtPointerStop.enabled:=edtPointerStart.enabled;
end;

procedure TfrmStringPointerScan.rbDiffDontCareChange(Sender: TObject);
begin
  lblCompare.enabled:=rbDiffDontCare.enabled and (rbDiffDontCare.checked = false);
  comboCompareType.enabled:=lblCompare.enabled;
  cbMapPointerValues.Enabled:=comboCompareType.enabled;
end;

procedure TfrmStringPointerScan.statusupdaterTimer(Sender: TObject);
var
  scannerprogress: double;
  scannerTotal: double;
  i: integer;
  x: integer;
begin

  if (rescanner<>nil) and (pointerfilereader<>nil) then
    progressbar1.position:=trunc((rescanner.currentPosition / pointerfilereader.count) * 1000);

  if scanner<>nil then
  begin
    lblInfo.caption:=Format(rsScanningFoun, [inttostr(scanner.count)]);

    scannerTotal:=power(scanner.structsize, scanner.maxlevel+1);

    scannerprogress:=0;
    for i:=0 to scanner.maxlevel-1 do
    begin
      x:=scanner.progress[i];
      scannerprogress:=scannerprogress+x*power(scanner.structsize, (scanner.maxlevel+1)-i-1);
    end;

    progressbar1.position:=trunc((scannerprogress / scannertotal) * 1000);


  end
  else
  if rescanner<>nil then
    lblinfo.caption:=Format(rsScanningFoun, [inttostr(rescanner.count)])


end;

procedure TfrmStringPointerScan.setGUIStateEnabled(state: boolean);
begin
  lblBaseRegion.enabled:=state;
  lblExtra.enabled:=state;

  cbPointerInRange.enabled:=state;
  cbPointerInRange.OnChange(cbPointerInRange);

  lblBaseRegion.enabled:=state;
  lblExtra.enabled:=state;
  lblvds.enabled:=state;

  edtBase.enabled:=state;
  edtExtra.enabled:=state;
  edtExtra.OnChange(edtExtra); //makes the difftypes enabled or not

  rbDatascan.enabled:=state;
  rbStringscan.enabled:=state;

  cbRegExpChange(rbStringscan);
  comboType.enabled:=state;

  rbDatascanChange(Nil);

  cbHasShadow.enabled:=state;
  cbHasShadow2.enabled:=state;

  edtShadowAddress.Enabled:=state;
  edtShadowAddress2.enabled:=state;

  edtShadowSize.enabled:=state;
  edtShadowSize2.enabled:=state;

  lblSize.enabled:=state;
  lblSize2.enabled:=state;

  btnScan.enabled:=state;
end;

procedure TfrmStringPointerScan.disableGui;
begin
  setGUIStateEnabled(false);
end;

procedure TfrmStringPointerScan.enableGui;
begin
  setGUIStateEnabled(true);
end;

initialization
  {$I frmStringPointerScanUnit.lrs}

end.

