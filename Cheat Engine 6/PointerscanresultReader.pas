unit PointerscanresultReader;

{$MODE Delphi}

{
The TPointerscanresultReader will read the results from the pointerfile and present them to the pointerscanner as if it is just one file
}
interface

uses LCLIntf, sysutils, classes, CEFuncProc, NewKernelHandler, symbolhandler, math;

const maxcachecount=256;

type TPointerscanResult=record
  modulenr: integer;
  moduleoffset: dword;
  offsetcount: integer;
  offsets: array [0..1000] of dword;
end;
type PPointerscanResult= ^TPointerscanResult;

type TPointerscanresultReader=class
  private
    Fcount: qword;
    sizeOfEntry: integer;
    maxlevel: integer;
    modulelist: tstringlist;

    FFileName: string;
    files: array of record
      startindex: integer;
      lastindex: integer;
      f: TFileStream;
    end;

    cacheStart: integer;
    cacheSize: integer;
    cache: pointer;
    function InitializeCache(i: qword): boolean;
  public
    procedure resyncModulelist;
    procedure saveModulelistToResults(s: Tstream);
    function getModulename(modulenr: integer): string;
    function getModuleBase(modulenr: integer): ptrUint;
    function getPointer(i: qword): PPointerscanResult; overload;
    function getPointer(i: qword; var pointsto: ptrUint): PPointerscanResult; overload;
    constructor create(filename: string);
    destructor destroy; override;
    property count: qword read FCount;
    property offsetCount: integer read maxlevel;
    property filename: string read FFilename;
    property entrySize: integer read sizeOfEntry;
end;

implementation

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
    if InRange(i, files[j].startindex, files[j].lastindex) then
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

function TPointerscanresultReader.getModuleBase(modulenr: integer): ptrUint;
{pre: modulenr must be valid, so not -1 }
begin
  result:=ptrUint(modulelist.Objects[modulenr]);
end;

function TPointerscanresultReader.getModulename(modulenr: integer): string;
begin
  if modulenr<modulelist.Count then
    result:=modulelist[modulenr]
  else
    result:='';
end;

function TPointerscanresultReader.getPointer(i: uint64): PPointerscanResult;
{
for those that know what they want
}
var
  cachepos: integer;
begin
  result:=nil;

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
  cachepos: integer;

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
    address:=dword(modulelist.objects[result.modulenr])+result.moduleoffset;
    
  for j:=result.offsetcount-1 downto 0 do
  begin
    if readprocessmemory(processhandle, pointer(address), @address2, processhandler.pointersize, x) then
      address:=address2+result.offsets[j]
    else
      exit; //can't fully resolve
  end;

  pointsto:=address;
end;

constructor TPointerscanresultReader.create(filename: string);
var
  configfile: TFileStream;
  modulelistLength: integer;
  tempmodulelist: Tstringlist;
  x: dword;
  i,j: integer;
  temppchar: pchar;
  temppcharmaxlength: dword;

  filenamecount: integer;
begin
  FFilename:=filename;
  configfile:=TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  configfile.ReadBuffer(modulelistlength,sizeof(modulelistlength));
  modulelist:=tstringlist.create;
  tempmodulelist:=tstringlist.create;

  temppcharmaxlength:=256;
  getmem(temppchar, temppcharmaxlength);
    
  symhandler.getModuleList(tempmodulelist);

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

    j:=tempmodulelist.IndexOf(temppchar);
    if j<>-1 then
      modulelist.Addobject(temppchar, tempmodulelist.Objects[j]) //add it and store the base address
    else
      modulelist.Add(temppchar);
  end;

  configfile.Read(maxlevel,sizeof(maxlevel));
  sizeofentry:=12+(4*maxlevel);

  //get filename count
  configfile.Read(filenamecount,sizeof(filenamecount));
  setlength(files,filenamecount);

  fcount:=0;
  //open the files
  for i:=0 to filenamecount-1 do
  begin
    configfile.Read(x,sizeof(x));
    while x>=temppcharmaxlength do
    begin
      temppcharmaxlength:=temppcharmaxlength*2;
      ReallocMem(temppchar, temppcharmaxlength);
    end;

    configfile.Read(temppchar[0],x);
    temppchar[x]:=#0;


    files[i].f:=TFileStream.Create(ExtractFilePath(filename)+temppchar, fmOpenRead or fmShareDenyWrite);
    files[i].startindex:=fcount;
    fcount:=fcount+uint64(files[i].f.Size div uint64(sizeofentry));
    files[i].lastindex:=fcount-1;
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
