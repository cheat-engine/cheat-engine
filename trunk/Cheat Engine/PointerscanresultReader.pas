unit PointerscanresultReader;

{$MODE Delphi}

{
The TPointerscanresultReader will read the results from the pointerfile and present them to the pointerscanner as if it is just one file
}
interface

uses windows, LCLIntf, sysutils, classes, CEFuncProc, NewKernelHandler, symbolhandler, math;

function GetFileSizeEx(hFile:HANDLE; FileSize:PQWord):BOOL; stdcall; external 'kernel32.dll' name 'GetFileSizeEx';


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
      filename: string;
      filesize: qword;
      f,fm: Thandle;
    end;

    cacheStart: integer;
    cacheSize: integer;
    cache: pointer;

    cacheStart2: integer;
    cacheSize2: integer;
    cache2: pointer;

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
  result:=length(fmergedresults);
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
        modulelist.Objects[i]:=tempmodulelist.Objects[j]
      else
      begin
        try
          modulelist.Objects[i]:=pointer(symhandler.getAddressFromName(modulelist[i]));
        except
          modulelist.Objects[i]:=nil;
        end;
      end;
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

    wantedoffset: qword;
    offset: qword;
begin
  result:=false;
 // OutputDebugString('InitializeCache');

  if i>=fcount then exit;





  //find which file to use
  for j:=0 to length(files)-1 do
  begin
    if InRangeQ(i, files[j].startindex, files[j].lastindex) then
    begin
      wantedoffset:=int64(sizeOfEntry*(i-files[j].startindex));


      if (wantedoffset mod systeminfo.dwAllocationGranularity)<>0 then
      begin
        //bring offset down to a location matching systeminfo.dwAllocationGranularity
        offset:=wantedoffset-(wantedoffset mod systeminfo.dwAllocationGranularity);
      end
      else
        offset:=wantedoffset;


      cachesize:=min(files[j].filesize-offset, systeminfo.dwAllocationGranularity*32);    //normally 2MB
      if cache2<>nil then
        unmapviewoffile(cache2);



      cache2:=MapViewOfFile(files[j].fm, FILE_MAP_READ, offset shr 32, offset and $ffffffff, cachesize );

     // if cache2=nil then
     //   OutputDebugString('Failure to map view of file');

      //point cache to the start of the wanted offset
      cache:=pointer(ptruint(cache2)+(wantedoffset-offset));
      cachesize:=(cachesize-(wantedoffset-offset)) div sizeofentry;

      result:=cache2<>nil;

     // OutputDebugString('InitializeCache success');
      exit;
    end;
  end;
  //nothing found...

 // OutputDebugString('InitializeCache nothing found');
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
    list.add(files[i].FileName);
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

  fn: string;
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
        fn:=ExtractFilePath(filename)+temppchar
      else
        fn:=temppchar;

      files[j].filename:=fn;


      files[j].f:=CreateFile(pchar(fn), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_DELETE, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);

      if (files[j].f<>0) and (files[j].f<>INVALID_HANDLE_VALUE) then
      begin
        files[j].startindex:=fcount;

        if GetFileSizeEx(files[j].f, @files[j].filesize)=false then
        begin
          OutputDebugString('GetFileSizeEx failed with error: '+inttostr(GetLastError));
        end;

        if files[j].filesize>0 then
        begin
          fcount:=fcount+uint64(files[j].filesize div uint64(sizeofentry));
          files[j].lastindex:=fcount-1;

          files[j].fm:=CreateFileMapping(files[j].f, nil,PAGE_READONLY, 0,0,nil);
        end
        else
          files[j].fm:=0;

        if (files[j].fm=0) then
          closehandle(files[j].f)
        else
          inc(j);
      end;
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


 // getmem(cache, sizeofEntry*maxcachecount);
 // getmem(cache2, sizeofEntry*maxcachecount);
  InitializeCache(0);

  freemem(temppchar);
  configfile.Free;
end;

destructor TPointerscanresultReader.destroy;
var i: integer;
begin
  if modulelist<>nil then
    modulelist.free;

  if cache2<>nil then
    UnmapViewOfFile(cache2);

  for i:=0 to length(files)-1 do
    if files[i].f<>0 then
    begin
      if files[i].fm<>0 then
        closehandle(files[i].fm);

      closehandle(files[i].f);
    end;
end;

end.
