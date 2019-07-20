unit PointerscanresultReader;

{$MODE Delphi}

{
The TPointerscanresultReader will read the results from the pointerfile and present them to the pointerscanner as if it is just one file
}
interface

uses windows, LCLIntf, sysutils, classes, CEFuncProc, NewKernelHandler,
  symbolhandler, math, dialogs, LazUTF8;

resourcestring
  rsPSRCorruptedPointerscanFile = 'Corrupted pointerscan file';
  rsPSRInvalidPointerscanFileVersion = 'Invalid pointerscan file version';
  rsBuggedList = 'BuggedList';

function GetFileSizeEx(hFile:HANDLE; FileSize:PQWord):BOOL; stdcall; external 'kernel32.dll' name 'GetFileSizeEx';


type TPointerscanResult=packed record
  modulenr: integer;
  moduleoffset: int64;
  offsetcount: integer;
  offsets: array [0..1000] of integer;
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
    cacheSize: size_t;
    cache: pointer;

    cacheStart2: integer;
    cacheSize2: integer;
    cache2: pointer;

    fExternalScanners: integer;
    fGeneratedByWorkerID: integer;
    fCompressedPtr: boolean;
    fAligned: boolean;

    MaskModuleIndex: dword;
    MaskLevel: dword;
    MaskOffset: dword;

    fMaxBitCountModuleIndex: dword;
    fMaxBitCountModuleOffset: dword;
    fMaxBitCountLevel: dword;
    fMaxBitCountOffset: dword;

    fEndsWithOffsetList: array of dword;

    fCanResume: boolean;
    fdidBaseRangeScan: boolean;
    foriginalBaseScanRange: qword;


    CompressedPointerScanResult: PPointerscanResult;
    compressedTempBuffer: PByteArray;

    fLastRawPointer: pointer;


    fmergedresults: array of integer;
    function InitializeCache(i: qword): boolean;
    function getModuleListCount: integer;

    function getMergedResultCount: integer;
    function getMergedResult(index: integer): integer;

    function getEndsWithOffsetListCount: integer;
    function getEndsWithOffsetListEntry(index: integer): dword;
  public
    procedure resyncModulelist;
    procedure saveModulelistToResults(s: Tstream);
    function getModulename(modulenr: integer): string;
    function getModuleBase(modulenr: integer): ptrUint;
    procedure setModuleBase(modulenr: integer; newModuleBase: ptruint);
    function getPointer(i: qword): PPointerscanResult; overload;
    function getPointer(i: qword; var pointsto: ptrUint): PPointerscanResult; overload;
    procedure getFileList(list: TStrings);

    procedure ReleaseFiles;

    constructor create(filename: string; original: TPointerscanresultReader=nil);

    destructor destroy; override;
    property count: qword read FCount;
    property offsetCount: integer read maxlevel;
    property filename: string read FFilename;
    property entrySize: integer read sizeOfEntry;
    property modulelistCount: integer read getModuleListcount;
    property modulename[index: integer]: string read getModuleName;
    property modulebase[index: integer]: ptruint read getModuleBase write setModuleBase;
    property mergedresultcount: integer read getMergedResultCount;
    property mergedresults[index: integer]: integer read getMergedResult;
    property EndsWithOffsetListCount: integer read getEndsWithOffsetListCount;
    property EndsWithOffsetList[index: integer]: dword read getEndsWithOffsetListEntry;

    property compressedptr: boolean read fCompressedPtr;
    property aligned: boolean read fAligned;
    property MaxBitCountModuleIndex: dword read fMaxBitCountModuleIndex;
    property MaxBitCountModuleOffset: dword read fMaxBitCountModuleOffset;
    property MaxBitCountLevel: dword read fMaxBitCountLevel;
    property MaxBitCountOffset: dword read fMaxBitCountOffset;
    property LastRawPointer: pointer read fLastRawPointer;
    property CanResume: boolean read fCanResume;
    property DidBaseRangeScan: boolean read fdidBaseRangeScan;
    property BaseScanRange: qword read foriginalBaseScanRange;
end;

procedure findAllResultFilesForThisPtr(filename: string; rs: TStrings; lookupmode: integer=0);

implementation

uses ProcessHandlerUnit, PointerscanStructures, Maps, AvgLvlTree;


procedure findAllResultFilesForThisPtr(filename: string; rs: TStrings; lookupmode: integer=0);
var
  fr: TRawbyteSearchRec;
  i,j: integer;
  ext1, ext2: string;

  basesort: boolean;
  v1, v2: int64;

  sepindex: integer;
  temp: string;
  swap: boolean;

  filemap: TMap;

  f: string;
  fn: pchar;

  path: string;


  it: TMapIterator;
begin
  //search the folder this ptr file is in for .result.* files
  //extract1

  filemap:=TMap.Create(its8, sizeof(pointer));

  if lookupmode=1 then
    filename:=UTF8ToWinCP(filename);

  path:=ExtractFilePath(filename);

  if FindFirst(filename+'.results.*', 0, fr)=0 then
  begin
    repeat
      ext1:=ExtractFileExt(fr.name);
      ext1:=copy(ext1, 2, length(ext1)-1);

      if TryStrToInt64('$'+ext1, v1) then
      begin
        f:=path+fr.name;
        getmem(fn, length(f)+1);
        strcopy(fn, @f[1]);

        filemap.Add(v1, fn);
      end;
    until FindNext(fr)<>0;

    FindClose(fr);
  end
  else
  begin
    if lookupmode<1 then
    begin
      findAllResultFilesForThisPtr(filename, rs, lookupmode+1);
      exit;
    end;
  end;


  it:=TMapIterator.Create(filemap);
  it.First;
  while not it.EOM do
  begin
    it.GetData(fn);
    rs.add(fn);
    freememandnil(fn);
    it.Next;
  end;

  it.free;
  filemap.Clear;
  freeandnil(filemap);

end;

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

function TPointerscanresultreader.getEndsWithOffsetListCount: integer;
begin
  result:=length(fEndsWithOffsetList);
end;

function TPointerscanresultreader.getEndsWithOffsetListEntry(index: integer): dword;
begin
  result:=fEndsWithOffsetList[index]
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

    //and the base (for debugging info)
    s.WriteQWord(qword(modulelist.Objects[i]));
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

{$if FPC_FULLVERSION<30200}
      cachesize:=min(files[j].filesize-offset, systeminfo.dwAllocationGranularity*32);    //normally 2MBZ
{$else}
      cachesize:=min(files[j].filesize-offset, qword(systeminfo.dwAllocationGranularity*32));    //normally 2MBZ
{$endif}
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

  if (modulenr>=0) and (modulenr<modulelist.Count) then
    result:=ptrUint(modulelist.Objects[modulenr])
  else
    result:=0;
end;

procedure TPointerscanresultReader.setModuleBase(modulenr: integer; newModuleBase: ptruint);
begin
  if (modulenr>=0) and (modulenr<modulelist.Count) then
    modulelist.objects[modulenr]:=pointer(newModulebase);
end;

function TPointerscanresultReader.getModulename(modulenr: integer): string;
begin
  if (modulenr>=0) and (modulenr<modulelist.Count) then
    result:=modulelist[modulenr]
  else
    result:=rsBuggedList;
end;

function TPointerscanresultReader.getPointer(i: uint64): PPointerscanResult;
{
for those that know what they want
}
var
  cachepos: integer;

  p: PByteArray;
  bit: integer;
  j: integer;
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

  if fCompressedPtr then
  begin
    p:=PByteArray(ptrUint(cache)+(cachepos*sizeofentry));

    CopyMemory(compressedTempBuffer, p, sizeofentry);

    if MaxBitCountModuleOffset=32 then //only 2 possibilities
      compressedPointerScanResult.moduleoffset:=PInteger(compressedTempBuffer)^
    else
      compressedPointerScanResult.moduleoffset:=PInt64(compressedTempBuffer)^;


    bit:=MaxBitCountModuleOffset;

    compressedPointerScanResult.modulenr:=pdword(@compressedTempBuffer[bit shr 3])^;
    compressedPointerScanResult.modulenr:=compressedPointerScanResult.modulenr and MaskModuleIndex;

    if compressedPointerScanResult.modulenr shr (fMaxBitCountModuleIndex-1) = 1 then //most significant bit is set, sign extent this value
    begin
      //should be -1 as that is the only one possible
      dword(compressedPointerScanResult.modulenr):=dword(compressedPointerScanResult.modulenr) or (not MaskModuleIndex);

      if compressedPointerScanResult.modulenr<>-1 then
      begin
        //my assumption was wrong
        compressedPointerScanResult.modulenr:=-1;
      end;
    end;

    inc(bit, fMaxBitCountModuleIndex);

    {
    compressedPointerScanResult.offsetcount:=pdword(@compressedTempBuffer[bit div 8])^;
    compressedPointerScanResult.offsetcount:=compressedPointerScanResult.offsetcount shr (bit mod 8);
    compressedPointerScanResult.offsetcount:=compressedPointerScanResult.offsetcount and MaskLevel;
    }
    compressedPointerScanResult.offsetcount:=(pdword(@compressedTempBuffer[bit shr 3])^ shr (bit and $7)) and MaskLevel;
    inc(compressedPointerScanResult.offsetcount, length(fEndsWithOffsetList));

    inc(bit, fMaxBitCountLevel);

    for j:=0 to length(fEndsWithOffsetList)-1 do
      compressedPointerScanResult.offsets[j]:=fEndsWithOffsetList[j];



    for j:=length(fEndsWithOffsetList) to compressedPointerScanResult.offsetcount-1 do
    begin
      {
      compressedPointerScanResult.offsets[j]:=pdword(@compressedTempBuffer[bit div 8])^;
      compressedPointerScanResult.offsets[j]:=compressedPointerScanResult.offsets[j] shr (bit mod 8);
      compressedPointerScanResult.offsets[j]:=compressedPointerScanResult.offsets[j] and MaskOffset;

      if aligned then
        compressedPointerScanResult.offsets[j]:=compressedPointerScanResult.offsets[j] shl 2;
      }

      if aligned then
        compressedPointerScanResult.offsets[j]:=((pdword(@compressedTempBuffer[bit shr 3])^ shr (bit and $7) ) and MaskOffset) shl 2
      else
        compressedPointerScanResult.offsets[j]:=(pdword(@compressedTempBuffer[bit shr 3])^ shr (bit and $7) ) and MaskOffset;

      inc(bit, fMaxBitCountOffset);
    end;



    result:=compressedPointerScanResult;
    fLastRawPointer:=compressedTempBuffer;
  end
  else
  begin
    result:=PPointerscanResult(ptrUint(cache)+(cachepos*sizeofentry));
    fLastRawPointer:=result;
  end;
end;

function TPointerscanresultReader.getPointer(i: qword; var pointsto: ptrUint): PPointerscanResult;
{
For use for simple display
}
var
  x: ptruint;
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
  filenames: array of string;

  fnames: tstringlist;

  pscanversion: byte;

begin
  FFilename:=filename;
  configfile:=TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);

  if configfile.ReadByte<>$ce then
    raise exception.create(rsPSRCorruptedPointerscanFile);

  pscanversion:=configfile.ReadByte;
  if pscanversion>pointerscanfileversion then
    raise exception.create(rsPSRInvalidPointerscanFileVersion);

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

    a:=configfile.ReadQWord;  //discard this info (only used for scandata)

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

  //read maxlevel
  configfile.Read(maxlevel,sizeof(maxlevel));



  //read compressedptr info
  fCompressedPtr:=configfile.ReadByte=1;
  if fCompressedPtr then
  begin
    fAligned:=configFile.ReadByte=1;
    fMaxBitCountModuleIndex:=configfile.ReadByte;
    fMaxBitCountModuleOffset:=configfile.ReadByte;
    fMaxBitCountLevel:=configfile.ReadByte;
    fMaxBitCountOffset:=configfile.ReadByte;
    setlength(fEndsWithOffsetList, configfile.ReadByte);
    for i:=0 to length(fEndsWithOffsetList)-1 do
      fEndsWithOffsetList[i]:=configfile.ReadDWord;

    sizeofentry:=MaxBitCountModuleOffset+MaxBitCountModuleIndex+MaxBitCountLevel+MaxBitCountOffset*(maxlevel-length(fEndsWithOffsetList));
    sizeofentry:=(sizeofentry+7) div 8;

    MaskModuleIndex:=0;
    for i:=1 to MaxBitCountModuleIndex do
      MaskModuleIndex:=(MaskModuleIndex shl 1) or 1;

    for i:=1 to MaxBitCountLevel do
      MaskLevel:=(MaskLevel shl 1) or 1;

    for i:=1 to MaxBitCountOffset do
      MaskOffset:=(MaskOffset shl 1) or 1;

    getmem(CompressedPointerScanResult, 16+4*maxlevel);
    getmem(CompressedTempBuffer, sizeofentry+4);
  end
  else
  begin
    sizeofentry:=16+(4*maxlevel)
  end;

  if pscanversion>=2 then
  begin
    fdidBaseRangeScan:=configFile.readByte=1;
    if fdidBaseRangeScan then
      foriginalBaseScanRange:=configfile.ReadQWord;
  end;




  //get the filenames
  fnames:=tstringlist.create;
  findAllResultFilesForThisPtr(filename, fnames);




  setlength(filenames, fnames.count);
  for i:=0 to fnames.count-1 do
    filenames[i]:=fnames[i];

  fnames.free;

  setlength(files, length(filenames));
  j:=0;

  for i:=0 to length(filenames)-1 do
  begin
    try
      if pos(PathDelim, filenames[i])=0 then
        fn:=ExtractFilePath(filename)+filenames[i]
      else
        fn:=filenames[i];

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
  end;
  setlength(files,j);








 // getmem(cache, sizeofEntry*maxcachecount);
 // getmem(cache2, sizeofEntry*maxcachecount);
  InitializeCache(0);

  freememandnil(temppchar);
  configfile.Free;


  fCanResume:=fileexists(filename+'.resume.config') and fileexists(filename+'.resume.scandata') and fileexists(filename+'.resume.queue');

  if length(filenames)=0 then
    MessageDlg('There was an error loading the results. Check that the path is readable', mtError, [mbok],0);

end;

procedure TPointerscanresultReader.ReleaseFiles;
//if only the config file is needed. This releases the results
var i: integer;
begin
  for i:=0 to length(files)-1 do
    if files[i].f<>0 then
    begin
      if files[i].fm<>0 then
        closehandle(files[i].fm);

      closehandle(files[i].f);
    end;
end;

destructor TPointerscanresultReader.destroy;
var i: integer;
begin
  if modulelist<>nil then
    modulelist.free;

  if cache2<>nil then
    UnmapViewOfFile(cache2);

  ReleaseFiles;

  if compressedTempBuffer<>nil then
    freememandnil(compressedTempBuffer);

  if compressedPointerScanResult<>nil then
    freememandnil(compressedPointerScanResult);

end;

end.
