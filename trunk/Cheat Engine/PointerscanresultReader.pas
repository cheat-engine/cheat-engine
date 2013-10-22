unit PointerscanresultReader;

{$MODE Delphi}

{
The TPointerscanresultReader will read the results from the pointerfile and present them to the pointerscanner as if it is just one file
}
interface

uses LCLIntf, sysutils, classes, CEFuncProc, NewKernelHandler, symbolhandler, math;

const maxcachecount=2560;

type TPointerscanResult=record
  modulenr: integer;
  moduleoffset: integer;
  offsetcount: integer;
  offsets: array [0..1000] of dword;
end;
type PPointerscanResult= ^TPointerscanResult;

type
  TPointerscanresultReader=class;
  TPointerscanresultReader=class
  private
    Fcount: qword;
    sizeOfEntry: integer;
    maxlevel: integer;
    modulelist: tstringlist;

    FFileName: string;
    files: array of record
      startindex: qword;
      lastindex: qword;
      f: TFileStream;
    end;

    cacheStart: integer;
    cacheSize: integer;
    cache: pointer;
    fExternalScanners: integer;
    fGeneratedByWorkerID: integer;

    fmergedresults: array of integer;
    function InitializeCache(i: qword): boolean;
    function getModuleListCount: integer;

    function getMergedResultCount: integer;
    function getMergedResult(index: integer): integer;
  public
    procedure resyncModulelist;
    procedure saveModulelistToResults(s: Tstream);
    function getModulename(modulenr: integer): string;
    function getModuleBase(modulenr: integer): ptrUint;
    procedure setModuleBase(modulenr: integer; newModuleBase: ptruint);
    function getPointer(i: qword): PPointerscanResult; overload;
    function getPointer(i: qword; var pointsto: ptrUint): PPointerscanResult; overload;
    procedure getFileList(list: TStrings);
    constructor create(filename: string; original: TPointerscanresultReader=nil);

    destructor destroy; override;
    property count: qword read FCount;
    property offsetCount: integer read maxlevel;
    property filename: string read FFilename;
    property entrySize: integer read sizeOfEntry;
    property externalScanners: integer read fExternalScanners;
    property generatedByWorkerID: integer read fGeneratedByWorkerID;
    property modulelistCount: integer read getModuleListcount;
    property modulebase[index: integer]: ptruint read getModuleBase write setModuleBase;
    property mergedresultcount: integer read getMergedResultCount;
    property mergedresults[index: integer]: integer read getMergedResult;
end;

implementation

function TPointerscanresultreader.getMergedResultCount: integer;
begin
  length(fmergedresults);
end;

function TPointerscanresultreader.getMergedResult(index: integer): integer;
begin
  if index<mergedresultcount then
    result:=fmergedresults[index]
  else
    result:=-1;
end;

procedure TPointerscanresultreader.resyncModulelist;
var
  tempmodulelist: TStringList;
  i,j: integer;
begin
  tempmodulelist:=tstringlist.Create;
  try
    symhandler.getModuleList(tempmodulelist);
    //sift through the list filling in the modulelist of the opened pointerfile
    for i:=0 to modulelist.Count-1 do
    begin
      j:=tempmodulelist.IndexOf(modulelist[i]);
      if j<>-1 then
        modulelist.Objects[i]:=tempmodulelist.Objects[j];
    end;
  finally
    tempmodulelist.free;
  end;
end;

procedure TPointerscanresultReader.saveModuleListToResults(s: TStream);
var i: integer;
  x: dword;
begin
  //save the number of modules
  x:=modulelist.Count;
  s.Write(x,sizeof(x));

  for i:=0 to modulelist.Count-1 do
  begin
    //for each module
    //save the length
    x:=length(modulelist[i]);
    s.Write(x,sizeof(x));

    //and the name
    s.Write(modulelist[i][1],x);
  end;
end;

function TPointerscanresultReader.InitializeCache(i: qword): boolean;
var j: integer;
    actualread: dword;
begin
  result:=false;
  if i>=fcount then exit;

  //find which file to use
  for j:=0 to length(files)-1 do
  begin
    if InRangeQ(i, files[j].startindex, files[j].lastindex) then
    begin
      files[j].f.Position:=sizeOfEntry*(i-files[j].startindex);

      actualread:=files[j].f.Read(cache^, sizeofentry*maxcachecount);
      cachesize:=actualread div sizeofentry;

      if cachesize>0 then
        result:=true;   
      exit;
    end;
  end;
  //nothing found...
end;

function TPointerscanresultReader.getModuleListCount: integer;
begin
  result:=modulelist.count;
end;

function TPointerscanresultReader.getModuleBase(modulenr: integer): ptrUint;
{pre: modulenr must be valid, so not -1 }
begin
  if modulenr<modulelist.Count then
    result:=ptrUint(modulelist.Objects[modulenr])
  else
    result:=$12345678;
end;

procedure TPointerscanresultReader.setModuleBase(modulenr: integer; newModuleBase: ptruint);
begin
  if modulenr<modulelist.Count then
    modulelist.objects[modulenr]:=pointer(newModulebase);
end;

function TPointerscanresultReader.getModulename(modulenr: integer): string;
begin
  if modulenr<modulelist.Count then
    result:=modulelist[modulenr]
  else
    result:='BuggedList';
end;

function TPointerscanresultReader.getPointer(i: uint64): PPointerscanResult;
{
for those that know what they want
}
var
  cachepos: integer;
begin
  result:=nil;
  if i>=count then exit;

  //check if i is in the cache
  if not InRange(i, cachestart,cachestart+cachesize-1) then
  begin
    //if not, reload the cache starting from i
    if not InitializeCache(i) then exit;

    cachestart:=i;
  end;

  //find out at which position in the cache this index is
  cachepos:=i-cachestart;
  result:=PPointerscanResult(ptrUint(cache)+(cachepos*sizeofentry));
end;

function TPointerscanresultReader.getPointer(i: qword; var pointsto: ptrUint): PPointerscanResult;
{
For use for simple display
}
var
  x: dword;
  address,address2: ptrUint;
  j: integer;
begin
  result:=getpointer(i);
  address:=0;
  address2:=0;

  //resolve the pointer
  pointsto:=0;
  if result.modulenr=-1 then
    address:=result.moduleoffset
  else
  begin
    if result.modulenr<modulelist.count then
    begin
      address:=ptruint(modulelist.objects[result.modulenr]);
      address:=address+result.moduleoffset;
    end
    else
    begin
      //error. Should never happen
      address:=$12345678;
    end;

  end;

  for j:=result.offsetcount-1 downto 0 do
  begin
    if readprocessmemory(processhandle, pointer(address), @address2, processhandler.pointersize, x) then
      address:=address2+result.offsets[j]
    else
      exit; //can't fully resolve
  end;

  pointsto:=address;
end;

procedure TPointerscanresultReader.getFileList(list: TStrings);
var i: integer;
begin
  for i:=0 to length(files)-1 do
    list.add(files[i].f.FileName);
end;

constructor TPointerscanresultReader.create(filename: string; original: TPointerscanresultReader=nil);
var
  configfile: TFileStream;
  modulelistLength: integer;
  tempmodulelist: Tstringlist;
  x: dword;
  i,j: integer;
  temppchar: pchar;
  temppcharmaxlength: dword;

  filenamecount: integer;
  error: boolean;
  a: ptruint;
begin
  FFilename:=filename;
  configfile:=TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  configfile.ReadBuffer(modulelistlength,sizeof(modulelistlength));
  modulelist:=tstringlist.create;
  tempmodulelist:=tstringlist.create;

  temppcharmaxlength:=256;
  getmem(temppchar, temppcharmaxlength);


  //get the module list myself
  if original=nil then
    symhandler.getModuleList(tempmodulelist)
  else
    modulelist.Assign(original.modulelist);

  //sift through the list filling in the modulelist of the opened pointerfile
  for i:=0 to modulelistlength-1 do
  begin
    configfile.Read(x,sizeof(x));
    while x>=temppcharmaxlength do
    begin
      temppcharmaxlength:=temppcharmaxlength*2;
      ReallocMem(temppchar, temppcharmaxlength);
    end;

    configfile.Read(temppchar[0], x);
    temppchar[x]:=#0;

    if original=nil then
    begin
      j:=tempmodulelist.IndexOf(temppchar);
      if j<>-1 then
        modulelist.Addobject(temppchar, tempmodulelist.Objects[j]) //add it and store the base address
      else
      begin
        a:=symhandler.getAddressFromName(temppchar,false,error,nil);
        if not error then
          modulelist.Addobject(temppchar, pointer(a))
        else
          modulelist.Add(temppchar);

      end;
    end;
  end;


  configfile.Read(maxlevel,sizeof(maxlevel));
  sizeofentry:=12+(4*maxlevel);

  //get filename count
  configfile.Read(filenamecount,sizeof(filenamecount));
  setlength(files,filenamecount);

  fcount:=0;
  //open the files
  i:=0;
  j:=0;
  while i<filenamecount do
  begin
    configfile.Read(x,sizeof(x));
    while x>=temppcharmaxlength do
    begin
      temppcharmaxlength:=temppcharmaxlength*2;
      ReallocMem(temppchar, temppcharmaxlength);
    end;

    configfile.Read(temppchar[0],x);
    temppchar[x]:=#0;


    try
      if pos(PathDelim, temppchar)=0 then
        files[j].f:=TFileStream.Create(ExtractFilePath(filename)+temppchar, fmOpenRead or fmShareDenyWrite)
      else
        files[j].f:=TFileStream.Create(temppchar, fmOpenRead or fmShareDenyWrite);

      files[j].startindex:=fcount;
      fcount:=fcount+uint64(files[j].f.Size div uint64(sizeofentry));
      files[j].lastindex:=fcount-1;
      inc(j);
    except
    end;

    inc(i);
  end;
  setlength(files,j);


  fExternalScanners:=0;
  try
    if configfile.Position<configfile.Size then
      fExternalScanners:=configfile.ReadDWord;

    if configfile.Position<configfile.Size then
      fGeneratedByWorkerID:=configfile.ReadDWord;

    //all following entries are worker id's when merged (this info is used by rescans)
    while configfile.Position<configfile.Size do
    begin
      setlength(fmergedresults, length(fmergedresults)+1);
      fmergedresults[length(fmergedresults)-1]:=configfile.ReadDWord;
    end;

  except

  end;


  getmem(cache, sizeofEntry*maxcachecount);
  InitializeCache(0);

  freemem(temppchar);
  configfile.Free;
end;

destructor TPointerscanresultReader.destroy;
var i: integer;
begin
  if modulelist<>nil then
    modulelist.free;

  if cache<>nil then
    freemem(cache);

  for i:=0 to length(files)-1 do
    if files[i].f<>nil then
      files[i].f.Free;
end;

end.
