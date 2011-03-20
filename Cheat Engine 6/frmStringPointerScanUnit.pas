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
  cefuncproc, newkernelhandler, frmStringMapUnit, MemFuncs, AvgLvlTree, Menus, bigmemallochandler, math, maps, RegExpr;

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
    function getPointerRecord(index: qword): PPointerRecord;
    function getAddressFromPointerRecord(p: ppointerrecord; baseaddress: ptruint): ptruint;
    function getStringFromPointerRecord(p: ppointerrecord; address: ptruint): string;
    function getStringAndAddress(index: qword; var address: ptruint; out p: PPointerRecord): string;

    constructor create(filename: string);
    destructor destroy; override;

    property levelWidth: integer read pointerfileLevelwidth;
    property count: qword read fcount;
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

    mustbeinregion: boolean;
    pointerstart, pointerend: ptruint;

    results: TMemorystream;
    fcount: qword;
    fCurrentPosition: qword;

    vartype: TVariableType;
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
    constructor create(suspended: boolean; address, address2: ptruint; mustbeinregion: boolean; pointerstart, pointerend: ptruint; isstringscan, caseSensitive, mustbestart: boolean; regExstr: string; diffkind: TDiffkind; pointerfilereader: TPointerfileReader; outputfilename: string );
    destructor destroy; override;
    property count: qword read fcount;
    property currentPosition: qword read fCurrentPosition;
  end;


  TScanner=class(tthread)
  private
    baseaddress: ptruint;
    baseaddress2: ptruint;
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

    levelblock: array of pdwordarray;
    fillpointerblock: pdwordarray;
   // block64: array of Pint64Array;

    results: TMemorystream;
    resultfile: tfilestream;

    //datascan variables
    isDataScan: boolean;
    alignment: integer;

    mustbeinregion: boolean;
    pointerstart: ptruint;
    pointerstop: ptruint;


    procedure handleBlock(blockaddress: ptruint; level: integer; path: TPointerpath);
    procedure addStringPath(level: integer; path: tpointerpath; stringsize: integer; unicode: bool);
    function comparePath(level: integer; path: tpointerpath; stringsize: integer): boolean;


    procedure mapRegionIfNeeded(blockaddress: ptruint; size: integer);
    procedure fillPointers(base: ptruint; size: integer);
    function getFirstPointerEntry(base: ptruint): PPointerListEntry;

    function getPointerValue(address: ptruint): ptruint;

    procedure flushResults;

  public
    procedure execute; override;
    constructor create(isDataScan, mustbeinregion: boolean; alignment: integer; pointerstart, pointerstop: ptruint; baseaddress, baseaddress2: ptruint; diffkind: TDiffkind; vartype: TVariableType; mapvalues: boolean; structsize: integer; maxlevel: integer; mappedregions,pointerlist: TAvgLvlTree; bma: TBigMemoryAllocHandler; filename: string);
    destructor destroy; override;
  end;

  TfrmStringPointerScan = class(TForm)
    Button1: TButton;
    cbCaseSensitive: TCheckBox;
    cbMustBeStart: TCheckBox;
    cbRegExp: TCheckBox;
    cbPointerInRange: TCheckBox;
    cbMapPointerValues: TCheckBox;
    comboType: TComboBox;
    comboCompareType: TComboBox;
    edtPointerStart: TEdit;
    edtPointerStop: TEdit;
    edtAlignsize: TEdit;
    edtExtra: TEdit;
    edtBase: TEdit;
    edtMaxLevel: TEdit;
    edtRegExp: TEdit;
    edtStructsize: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblCompare: TLabel;
    lblAlign: TLabel;
    lblAnd: TLabel;
    lblString: TLabel;
    lblInfo: TLabel;
    Label4: TLabel;
    lblExtra: TLabel;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miClearCache: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pmPointerfile: TPopupMenu;
    ProgressBar1: TProgressBar;
    rbDiffDontCare: TRadioButton;
    rbMustBeDifferent: TRadioButton;
    rbMustBeSame: TRadioButton;
    rbStringscan: TRadioButton;
    rbDatascan: TRadioButton;
    SaveDialog1: TSaveDialog;
    statusupdater: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure cbRegExpChange(Sender: TObject);
    procedure cbPointerInRangeChange(Sender: TObject);
    procedure comboTypeChange(Sender: TObject);
    procedure edtBaseChange(Sender: TObject);
    procedure edtExtraChange(Sender: TObject);
    procedure ListView1CustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);

    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1DblClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure miClearCacheClick(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure rbDatascanChange(Sender: TObject);
    procedure rbDiffDontCareChange(Sender: TObject);
    procedure statusupdaterTimer(Sender: TObject);
  private
    { private declarations }
    mappedRegions: TAvgLvlTree; //holds the map of the regions that have been mapped

    pointerlist: TAvgLvlTree; //holds the pointers in the app of the mapped regions
    pointerlistMemManager: TAvgLvlTreeNodeMemManager;
    bma: TBigMemoryAllocHandler;

    scanner: TScanner;
    rescanner: TRescan;
    pointerfilereader: TPointerfilereader;

    address, address2: ptruint;
    hasAddress2: boolean;



    function mapCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    function pointerCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    procedure cleanup;
    procedure OpenPointerfile(filename: string);
    procedure scanDone(var m: tmessage); message wm_sps_done;

    function getStringFromPointer(address: ptruint; offsets: TDwordArray; level, bytesize: integer; unicode: boolean; var a: ptruint): string;
  public
    { public declarations }
  end;



var
  frmStringPointerScan: TfrmStringPointerScan;



implementation

{ TfrmStringPointerScan }

uses frmStructPointerRescanUnit, MemoryBrowserFormUnit;

resourcestring
  rsGeneratingStringmap = 'Generating stringmap';
  rsGeneratedScanning = 'Generated. Scanning...';
  rsThisAddressIsNotAccessible = 'This address is not accessible';

//----------TPointerfileReader---------

function TPointerfilereader.getByteFromAddress(address: ptruint; var error: boolean): byte;
var x: dword;
begin
  error:=not readprocessmemory(processhandle, pointer(address), @result, 1, x);
  error:=error or (x<>1);
end;

function TPointerfilereader.getWordFromAddress(address: ptruint; var error: boolean): word;
var x: dword;
begin
  error:=not readprocessmemory(processhandle, pointer(address), @result, 2, x);
  error:=error or (x<>2);
end;

function TPointerfilereader.getDWordFromAddress(address: ptruint; var error: boolean): dword;
var x: dword;
begin
  error:=not readprocessmemory(processhandle, pointer(address), @result, 4, x);
  error:=error or (x<>4);
end;

function TPointerfilereader.getQWordFromAddress(address: ptruint; var error: boolean): qword;
var x: dword;
begin
  error:=not readprocessmemory(processhandle, pointer(address), @result, 8, x);
  error:=error or (x<>8);
end;

function TPointerfilereader.getSingleFromAddress(address: ptruint; var error: boolean): single;
var x: dword;
begin
  error:=not readprocessmemory(processhandle, pointer(address), @result, 4, x);
  error:=error or (x<>4);
end;

function TPointerfilereader.getDoubleFromAddress(address: ptruint; var error: boolean): double;
var x: dword;
begin
  error:=not readprocessmemory(processhandle, pointer(address), @result, 8, x);
  error:=error or (x<>8);
end;

function TPointerfilereader.getPointerFromAddress(address: ptruint; var error: boolean): ptruint;
var x: dword;
begin
  result:=0;
  error:=not readprocessmemory(processhandle, pointer(address), @result, processhandler.pointersize, x);
  error:=error or (x<>processhandler.pointersize);
end;


procedure TPointerfilereader.clearPointerCache;
begin
  pointermap.Clear;
end;

function TPointerfileReader.getPointerRecord(index: qword): PPointerRecord;
var blocksize: integer;
begin
  result:=nil;

  if (buffersize=0) or (not InRangeQ(index, bufferindex, bufferindex+buffersize-1)) then
  begin
    blocksize:=count-index-1;
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

function TPointerfilereader.getAddressFromPointerRecord(p: ppointerrecord; baseaddress: ptruint): ptruint;
var address: ptruint;
  a: ptruint;
  i: integer;
  x: dword;
  dp: pptruint;
begin
  result:=0;

  address:=baseaddress+p.offset[0];
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

      pointermap.Add(address, a);
      address:=a;

      if a=0 then
        exit; //unreadable
    end;


    address:=address+p.offset[i]
  end;

  result:=address;
end;

function TPointerfileReader.getStringFromPointerRecord(p: ppointerrecord; address: ptruint): string;
var i,j: integer;
  x: dword;
  e: boolean;
begin
  address:=getAddressFromPointerRecord(p, address);
  if address=0 then
    result:='???'
  else
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

        if e then
          result:='???';
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

        end
        else
          result:='???';

      end
      else
        result:='Not yet implemented';

    end;
  end;

end;

function TPointerfileReader.getStringAndAddress(index: qword; var address: ptruint; out p: PPointerRecord): string;
begin
  p:=getPointerRecord(index);
  if p<>nil then
    result:=getStringFromPointerRecord(p, address);

  address:=getAddressFromPointerRecord(p, address);
end;

constructor TPointerfileReader.create(filename: string);
begin
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
    freemem(pointerrecords);

  if stringbuf<>nil then
    freemem(stringbuf);

  if pointermap<>nil then
    freeandnil(pointermap);

  //cleanup the maps
  inherited destroy;
end;



//--------------TRescan----------------
procedure TRescan.flushResults;
begin
  outputfile.WriteBuffer(results.Memory^, results.position);
  results.position:=0;
end;

procedure TRescan.addPointer(p: PPointerRecord);
begin
  inc(fcount);
  results.WriteBuffer(p^, pointerfilereader.entrysize);

  if results.Position>=(15*1024*1024) then
    flushResults;
end;

function TRescan.checkByte(p: PPointerRecord): boolean;
var error: boolean;
  a: ptruint;
  v,v2: byte;
begin
  result:=false;
  a:=pointerfilereader.getAddressFromPointerRecord(p, address);
  if a<>0 then
  begin
    v:=pointerfilereader.getByteFromAddress(a, error);
    result:=not error;

    if result and (diffkind<>dkDontCare) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getByteFromAddress(a, error);
        result:=not error;

        if result then
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
  a:=pointerfilereader.getAddressFromPointerRecord(p, address);
  if a<>0 then
  begin
    v:=pointerfilereader.getWordFromAddress(a, error);
    result:=not error;

    if result and (diffkind<>dkDontCare) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getWordFromAddress(a, error);
        result:=not error;

        if result then
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
  a:=pointerfilereader.getAddressFromPointerRecord(p, address);
  if a<>0 then
  begin
    v:=pointerfilereader.getDwordFromAddress(a, error);
    result:=not error;

    if result and (diffkind<>dkDontCare) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getDwordFromAddress(a, error);
        result:=not error;

        if result then
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
  a:=pointerfilereader.getAddressFromPointerRecord(p, address);
  if a<>0 then
  begin
    v:=pointerfilereader.getQwordFromAddress(a, error);
    result:=not error;

    if result and (diffkind<>dkDontCare) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getQwordFromAddress(a, error);
        result:=not error;

        if result then
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
  a:=pointerfilereader.getAddressFromPointerRecord(p, address);
  if a<>0 then
  begin
    v:=pointerfilereader.getSingleFromAddress(a, error);
    result:=not error;

    if result and (diffkind<>dkDontCare) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getSingleFromAddress(a, error);
        result:=not error;

        if result then
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
  a:=pointerfilereader.getAddressFromPointerRecord(p, address);
  if a<>0 then
  begin
    v:=pointerfilereader.getDoubleFromAddress(a, error);
    result:=not error;

    if result and (diffkind<>dkDontCare) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getDoubleFromAddress(a, error);
        result:=not error;

        if result then
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
  a:=pointerfilereader.getAddressFromPointerRecord(p, address);
  if a<>0 then
  begin
    v:=pointerfilereader.getPointerFromAddress(a, error);
    result:=not error;

    if result and (diffkind<>dkDontCare) then
    begin
      a:=pointerfilereader.getAddressFromPointerRecord(p, address2);
      if a<>0 then
      begin
        v2:=pointerfilereader.getPointerFromAddress(a, error);
        result:=not error;

        if result then
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
  terminatecheck: integer;
  p: PPointerRecord;

  s,s2: string;
  passed: boolean;
  index,len: integer;
begin

  try
    for i:=0 to pointerfilereader.count-1 do
    begin
      p:=pointerfilereader.getPointerRecord(i);

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
          s:=pointerfilereader.getStringFromPointerRecord(p,address);
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


          if passed and (diffkind<>dkDontCare) then
          begin
            s2:=pointerfilereader.getStringFromPointerRecord(p,address2);

            if diffkind=dkMustBeDifferent then
              passed:=s<>s2
            else
              passed:=s=s2;

            if (passed) and (regex<>nil) then
            begin
              index:=0;
              len:=0;
              passed:=RegExprPos(regex, pchar(s), index, len);

              if passed and mustbestart then
                passed:=index=0;
            end;
          end;
        end;
      end;




      if passed then //add it
        addPointer(p);

      //check for forced exit
      inc(terminatecheck);
      if (terminatecheck>10000) then
      begin
        if terminated then exit;
        terminatecheck:=0;
      end;

      fCurrentPosition:=i;
    end;

    flushresults;

  finally
    if outputfile<>nil then
      freeandnil(outputfile);

    if results<>nil then
      freeandnil(results);

    PostMessage(frmStringPointerScan.Handle, wm_sps_done, 0,0);
  end;


end;

constructor TRescan.create(suspended: boolean; address, address2: ptruint; mustbeinregion: boolean; pointerstart, pointerend: ptruint; isstringscan, caseSensitive, mustbestart: boolean; regExstr: string; diffkind: TDiffkind; pointerfilereader: TPointerfileReader; outputfilename: string );
var regflags: tregexprflags;
  lw: integer;
begin
  self.vartype:=pointerfilereader.vartype;

  self.isstringscan:=isstringscan;
  self.mustbestart:=mustbestart;
  self.diffkind:=diffkind;
  self.pointerfilereader:=pointerfilereader;
  self.address:=address;
  self.address2:=address2;
  self.mustbeinregion:=mustbeinregion;
  self.pointerstart:=pointerstart;
  self.pointerend:=pointerend;


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
  outputfile:=TFileStream.Create(outputfilename, fmCreate or fmShareDenyNone);

  lw:=pointerfilereader.levelWidth;
  outputfile.WriteBuffer(lw, sizeof(lw));


  results:=TMemoryStream.Create;

  results.size:=16*1024*1024;

  inherited create(suspended);
end;


destructor TRescan.destroy;
begin
  if outputfile<>nil then
    freeandnil(outputfile);

  if results<>nil then
    freeandnil(results);

  inherited destroy;
end;

//--------------TScanner---------------
function TScanner.getPointerValue(address: ptruint): ptruint;
{
returns 0 if not found
}
var
  search: TPointerListEntry;
  pe: TAvgLvlTreeNode;
  r: PPointerListEntry;
begin
  result:=0;
  mapRegionIfNeeded(address,4096);
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
var x: dword;
  i: integer;
  p: PPointerListEntry;
  pe, prev, next: TAvgLvlTreeNode;
begin
  if readprocessmemory(processhandle, pointer(base), fillpointerblock, size, x) then
  begin
    for i:=0 to (x div 4)-1 do
    begin
      if (fillpointerblock[i]>$10000) and ((fillpointerblock[i] mod 4)=0) and (isreadable(fillpointerblock[i])) then
      begin
        p:=bma.alloc(sizeof(TPointerListEntry));

        p.address:=base+(i*4);
        p.pointsto:=fillpointerblock[i];

        {    debug code
        if pointerlist.Find(p)<>nil then
        begin
          freemem(p);
          continue;
        end; }

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
        freemem(oldmap);
      end;
    end;
  end;

end;

procedure TScanner.flushResults;
begin
  resultfile.WriteBuffer(results.Memory^, results.position);
  results.position:=0;
end;

function TScanner.comparePath(level: integer; path: tpointerpath; stringsize: integer): boolean;
var i: integer;
  x: ptruint;
  address, address2: ptruint;
  e: boolean;

  value,value2: pbytearray;
  br: dword;
begin
  result:=false;
  address:=baseaddress+path[0];
  address2:=baseaddress2+path[0];
  for i:=1 to level do
  begin
    x:=getPointerValue(address2);
    if x<>0 then
    begin
      address2:=address2+x;

      x:=getPointerValue(address);
      if x<>0 then //not much chance it's 0 else it would never got here...
        address:=address+x
      else
        exit;

    end else exit;
  end;


  //still here so both addresses are readable
  //
  if valuemap<>nil then
  begin
    value:=valuemap.GetDataPtr(address);
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

  if value[0]=0 then exit; //unreadable but mapped as unreadable
  if value[1]=0 then exit; //    "       "     "    "     "

  value:=@value[1];
  value2:=@value2[1];

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

procedure TScanner.addStringPath(level: integer; path: tpointerpath; stringsize: integer; unicode: BOOL);
begin
  if (diffkind<>dkDontCare) and (not comparePath(level, path, stringsize)) then exit;

  results.WriteBuffer(level, sizeof(level));
  results.WriteBuffer(stringsize, sizeof(stringsize));
  results.WriteBuffer(unicode, sizeof(unicode));
  results.WriteBuffer(path[0], sizeof(path[0])*(maxlevel+1));

  if results.Position>=(15*1024*1024) then
    flushResults;

  inc(count);
end;

procedure TScanner.handleBlock(blockaddress: ptruint; level: integer; path: TPointerpath);
var
  x: dword;
  i: integer;

  newaddress: ptruint;

  p: Pstringdata;

  block: pdwordarray;

  pe: PPointerListEntry;
begin

  if isDataScan then
  begin
    i:=0;
    while i<structsize do
    begin
      path[level]:=i;

      if (not mustbeinregion) or (InRangeX(blockaddress+i, pointerstart, pointerstop)) then
        addStringPath(level, path, -1,false); //use -1 to identify a data entry

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
      setlength(levelblock, maxlevel+1);
      for i:=0 to length(levelblock)-1 do
        getmem(levelblock[i], structsize); //one time alloc for each level so it can be reused without having to reallocate each recursion

      getmem(fillpointerblock, structsize);

      results:=TMemoryStream.Create;
      results.Size:=16*1024*1024;
      results.Position:=0;


      handleBlock(baseaddress, 0, pointerpath);
      flushResults;

      //messagebox(0,pchar(inttostr(count)),'ps',0);

    except
      on e: exception do
        messagebox(0,pchar('Exception:'+e.Message),'ps',0);
    end;

  finally
    PostMessage(frmStringPointerScan.Handle, wm_sps_done, 0,0);

  end;
end;

constructor TScanner.create(isDataScan, mustbeinregion: boolean; alignment: integer; pointerstart, pointerstop: ptruint; baseaddress, baseaddress2: ptruint; diffkind: TDiffkind; vartype: TVariableType; mapvalues: boolean; structsize: integer; maxlevel: integer; mappedregions,pointerlist: TAvgLvlTree; bma: TBigMemoryAllocHandler; filename: string);
begin
  self.baseaddress:=baseaddress;
  self.baseaddress2:=baseaddress2;
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

  if diffkind<>dkDontCare then
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
  resultfile.Write(maxlevel, sizeof(maxlevel));

  count:=0;

  inherited create(false); //let's get started...
end;

destructor TScanner.destroy;
var i: integer;
begin
  for i:=0 to length(levelblock)-1 do
    freemem(levelblock[i]);


  if fillpointerblock<>nil then
    freemem(fillpointerblock);

  if resultfile<>nil then
    freeandnil(resultfile);

  if tempvariablebuffer<>nil then
    freemem(tempvariablebuffer);

  if tempvariablebuffer2<>nil then
    freemem(tempvariablebuffer2);

  if valuemap<>nil then
    freeandnil(valuemap);
end;


//----------------------------


function TfrmStringPointerScan.getStringFromPointer(address: ptruint; offsets: TDwordArray; level, bytesize: integer; unicode: boolean; var a: ptruint): string;
var i: integer;
  x: dword;

  b: pchar;
  wb: pwidechar absolute b;
begin
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
  freemem(b);

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


  p: PPointerRecord;
begin
  if pointerfilereader<>nil then
  begin
    item.data:=nil;

    a:=address;
    s:=pointerfilereader.getStringAndAddress(item.index, a, p);

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
      s2:=pointerfilereader.getStringAndAddress(item.index, a, p);
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
    MemoryBrowser.hexview.address:=pointerfilereader.getAddressFromPointerRecord(pointerfilereader.getPointerRecord(listview1.Selected.Index), address);
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
    lc.Caption:='Offset '+inttostr(i);
  end;

  lc:=listview1.Columns.Add;
  lc.MinWidth:=2;
  lc.Width:=120;
  lc.Caption:='Address';

  lc:=listview1.Columns.Add;
  lc.MinWidth:=2;
  lc.Width:=120;
  lc.Caption:='Address 2';


  lblExtra.enabled:=true;
  edtExtra.Enabled:=true;

  edtExtraChange(edtExtra);


  listview1.items.count:=min(10000000, pointerfilereader.count);
end;

procedure TfrmStringPointerScan.scanDone(var m: tmessage);
begin
  if scanner<>nil then
    lblInfo.caption:='Found:'+inttostr(scanner.count);

  if rescanner<>nil then
    lblInfo.caption:='Found:'+inttostr(rescanner.count);


  cleanup;
  OpenPointerfile(frmStringPointerScan.SaveDialog1.FileName);

  if pointerfilereader<>nil then
    showmessage('Scan done! Found '+inttostr(pointerfilereader.count))
  else
    raise exception.create('Error during scan. No scanresults available');
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

  if pointerlistMemManager<>nil then
    freeandnil(pointerlistMemManager);

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
        freemem(p);
      end;
    end;
    freeandnil(mappedRegions);
  end;

  if pointerfilereader<>nil then
    freeandnil(pointerfilereader);

end;

procedure TfrmStringPointerScan.Button1Click(Sender: TObject);
var baseaddress: ptruint;
  baseaddress2: ptruint;
  structsize: integer;
  maxlevel: integer;
  alignsize: integer;
  pointerstart: ptruint;
  pointerstop: ptruint;

  diffkind: TDiffkind;
  vartype: TVariableType;
begin
  cleanup;

  baseaddress:=StrToQWordEx('$'+edtBase.text);
  if not isreadable(baseaddress) then
    raise exception.create(rsThisAddressIsNotAccessible);

  structsize:=strtoint(edtStructsize.text);
  maxlevel:=strtoint(edtMaxLevel.text);

  if edtExtra.text='' then
  begin
    rbDiffDontCare.Checked:=true;
    baseaddress2:=StrToQwordEx('$'+edtextra.text);
  end;

  if rbDatascan.checked then
  begin
    alignsize:=strtoint(edtAlignsize.text);

    if cbPointerInRange.checked then
    begin
      pointerstart:=StrToQWordEx('$'+edtPointerStart.text);
      pointerstop:=StrToQWordEx('$'+edtPointerStop.text);
    end;
  end;

  if savedialog1.execute then
  begin






    mappedRegions:=TAvgLvlTree.CreateObjectCompare(mapCompare);
    pointerlist:=TAvgLvlTree.CreateObjectCompare(pointerCompare);
    pointerlistMemManager:=TAvgLvlTreeNodeMemManager.Create;
    pointerlist.NodeMemManager:=pointerlistMemManager;
    pointerlistMemManager.MinimumFreeNode:=102400;
    pointerlistMemManager.MaximumFreeNodeRatio:=32;

    bma:=TBigMemoryAllocHandler.create;

    if rbStringscan.checked then
    begin
      if frmStringMap=nil then
        frmStringMap:=tfrmStringMap.Create(nil);

      frmstringmap.cbRegExp.checked:=cbRegExp.checked;
      frmstringmap.cbCaseSensitive.checked:=cbCaseSensitive.checked;
      frmstringmap.cbMustBeStart.checked:=cbMustBeStart.checked;
      frmstringmap.edtRegExp.text:=edtRegExp.text;

      frmstringmap.btnScan.click;
      lblInfo.caption:=rsGeneratingStringmap;
      lblInfo.Repaint;
      frmstringmap.scanner.WaitFor;

    end;



    lblInfo.caption:=rsGeneratedScanning;

    statusupdater.enabled:=true;

    if rbMustBeSame.checked then
      diffkind:=dkMustBeSame
    else
    if rbMustBeDifferent.checked then
      diffkind:=dkMustBeDifferent
    else
      diffkind:=dkDontCare;


    if diffkind<>dkDontCare then
    begin
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
    end;



    //everything has been configured
    scanner:=Tscanner.create(rbDatascan.checked, cbPointerInRange.checked, alignsize, pointerstart, pointerstop, baseAddress, baseaddress2, diffkind, vartype, cbMapPointerValues.checked, structsize, maxlevel, mappedRegions, pointerlist, bma, savedialog1.filename);

  end;
end;

procedure TfrmStringPointerScan.cbRegExpChange(Sender: TObject);
begin
  cbCaseSensitive.enabled:=cbRegExp.checked;
  cbMustBeStart.enabled:=cbRegExp.checked;
  edtRegExp.enabled:=cbRegExp.checked;
end;

procedure TfrmStringPointerScan.cbPointerInRangeChange(Sender: TObject);
begin
  edtPointerStart.enabled:=cbPointerInRange.checked;
  edtPointerStop.enabled:=cbPointerInRange.checked;
  lbland.enabled:=cbPointerInRange.checked;
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
begin
  try
    address:=StrToQword('$'+edtBase.text);
    listview1.Refresh;
  except
  end;
end;

procedure TfrmStringPointerScan.edtExtraChange(Sender: TObject);
begin
  if listview1.ColumnCount>1 then
    hasAddress2:=edtExtra.Text<>'';

  rbMustBeDifferent.enabled:=hasAddress2;
  rbMustBeSame.enabled:=hasAddress2;
  rbDiffDontCare.enabled:=hasAddress2;

  try
    address2:=StrToQword('$'+edtExtra.text);
    listview1.Refresh;
  except
  end;
end;

procedure TfrmStringPointerScan.MenuItem2Click(Sender: TObject);
begin
  cleanup;
end;

procedure TfrmStringPointerScan.MenuItem3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    OpenPointerfile(opendialog1.filename);

end;

procedure TfrmStringPointerScan.miClearCacheClick(Sender: TObject);
begin
  if pointerfilereader<>nil then
    pointerfilereader.clearPointerCache;

  listview1.Refresh;
end;

procedure TfrmStringPointerScan.MenuItem6Click(Sender: TObject);
var f: TfrmStructPointerRescan;
  diffkind : TDiffkind;
  pointerstart: ptruint;
  pointerstop: ptruint;
begin

  f:=TfrmStructPointerRescan.Create(self);
  f.comboType.itemindex:=combotype.itemindex;


  f.rbDiffDontCare.enabled:=hasAddress2;
  f.rbMustBeDifferent.enabled:=hasAddress2;
  f.rbMustBeSame.enabled:=hasAddress2;

  if f.showmodal=mrok then
  begin
    //rescan
    if f.rbMustBeSame.checked then
      diffkind:=dkMustBeSame
    else
    if f.rbMustBeDifferent.checked then
      diffkind:=dkMustBeDifferent
    else
      diffkind:=dkDontCare;

    if cbPointerInRange.checked then
    begin
      pointerstart:=StrToQWordEx('$'+f.edtPointerStart.text);
      pointerstop:=StrToQWordEx('$'+f.edtPointerStop.text);
    end;



    if savedialog1.execute then
    begin
      progressbar1.visible:=true;
      statusupdater.enabled:=true;
      listview1.items.count:=0;

      case f.comboType.itemindex of
        0: pointerfilereader.vartype:=vtString;
        1: pointerfilereader.vartype:=vtByte;
        2: pointerfilereader.vartype:=vtWord;
        3: pointerfilereader.vartype:=vtDword;
        4: pointerfilereader.vartype:=vtQword;
        5: pointerfilereader.vartype:=vtSingle;
        6: pointerfilereader.vartype:=vtDouble;
        7: pointerfilereader.vartype:=vtPointer;
      end;



      rescanner:=trescan.create(false, address, address2, cbpointerinrange.checked, pointerstart, pointerstop, combotype.itemindex=0, f.cbCaseSensitive.checked, f.cbMustBeStart.checked, f.edtRegExp.text, diffkind, pointerfilereader, savedialog1.filename );
    end;

  end;

  f.close;
  f.free;
end;

procedure TfrmStringPointerScan.rbDatascanChange(Sender: TObject);
begin
  cbRegExp.enabled:=rbStringscan.checked;
  cbCaseSensitive.enabled:=rbStringscan.checked and cbRegExp.checked;
  cbMustBeStart.enabled:=rbStringscan.checked and cbRegExp.checked;
  lblString.enabled:=rbStringscan.checked and cbRegExp.checked;
  edtRegExp.enabled:=rbStringscan.checked and cbRegExp.checked;

  lblAlign.enabled:=rbDatascan.checked;
  edtAlignsize.enabled:=rbDatascan.checked;
  edtPointerStart.enabled:=rbDatascan.checked and cbPointerInRange.checked;
  lblAnd.enabled:=rbDatascan.checked and cbPointerInRange.checked;
  edtPointerStop.enabled:=rbDatascan.checked and cbPointerInRange.checked;
end;

procedure TfrmStringPointerScan.rbDiffDontCareChange(Sender: TObject);
begin
  lblCompare.enabled:=rbDiffDontCare.checked = false;
  comboCompareType.enabled:=rbDiffDontCare.checked = false;
end;

procedure TfrmStringPointerScan.statusupdaterTimer(Sender: TObject);
begin
  if (rescanner<>nil) and (pointerfilereader<>nil) then
    progressbar1.position:=trunc((rescanner.currentPosition / pointerfilereader.count) * 1000);

  if scanner<>nil then
    lblInfo.caption:='Scanning... Found '+inttostr(scanner.count)
  else
  if rescanner<>nil then
    lblinfo.caption:='Scanning... Found '+inttostr(rescanner.count)


end;

initialization
  {$I frmStringPointerScanUnit.lrs}

end.

