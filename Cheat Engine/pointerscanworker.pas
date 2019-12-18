unit PointerscanWorker;

//class responsible for receiving paths and working them out

{$mode delphi}

interface

{$ifdef darwin}
uses
  macport, Classes, SysUtils, syncobjs, PointerscanStructures, ProcessHandlerUnit, pointervaluelist,
  pointeraddresslist, NewKernelHandler, zstream, zstreamext, macportdefines, SyncObjs2, math;
{$endif}

{$ifdef windows}
uses
  windows, Classes, SysUtils, syncobjs, PointerscanStructures, ProcessHandlerUnit, pointervaluelist,
  pointeraddresslist, NewKernelHandler, zstream, zstreamext;
{$endif}

type
  TPointerscanWorker = class (tthread)
  private


    pointersize: integer;
    fOnException: TNotifyEvent;

    temppathqueue: TPathQueueElement;


    procedure rscan(valuetofind:ptrUint; level: valSint);
    procedure StorePath(level: valSint; moduleid: integer; offset: ptruint);
    function DoRescan(level: valSint; moduleid: integer; offset: ptruint): boolean;
  protected
    fhasresults: boolean;
    results: tstream;
    procedure initialize; virtual; abstract;
    procedure flushresults; virtual; abstract;
    procedure flushifneeded; virtual;
  public
    pointerlisthandler: TReversePointerListHandler;
    {$ifdef windows}
    pathqueuesemaphore: THandle;
    {$else}
    pathqueuesemaphore: TSemaphore;
    {$endif}
    pathqueuelength: ^integer;
    pathqueueCS: TCriticalSection;
    pathqueue: PMainPathQueue;

    OutOfDiskSpace: ^boolean;

    mustEndWithSpecificOffset: boolean;
    mustendwithoffsetlist: array of dword;

    useHeapData: boolean;
    useOnlyHeapData: boolean;

    onlyOneStaticInPath: boolean;



    valuetofind: ptrUint;
    maxlevel: integer;
    structsize: integer;
//    startaddress: dword;
    startlevel: integer;
    alligned: boolean;
    staticonly: boolean;
    noLoop: boolean;

    LimitToMaxOffsetsPerNode: boolean;
    MaxOffsetsPerNode: integer;


    isFlushing: boolean;
    timespentwriting: qword;
    currentwritestart: qword;



    isdone: boolean;
    hasTerminated: boolean;
//    savestate: boolean;
    stop: boolean;

    //staticscanner: TStaticscanner;
    tempresults: array of dword; //offsetlist
    valuelist: array of ptruint; //used by noLoop  .
    {
    I could have used a map, but inserting in a map takes longer than a array append
    Also, the array is maxlevel big, and usually not that long
    Really not sure what's the best solution in this case though
    }


    //info:
    currentaddress: pointer;
    currentlevel: integer;
    LookingForMin: ptrUint;
    LookingForMax: ptrUint;
    //lastaddress: ptrUint;



    haserror: boolean;
    errorstring: string;

    pathsEvaluated: qword;
    pointersfound: qword;

    NegativeOffsets: boolean;
    compressedptr: boolean;
    MaxBitCountModuleIndex: dword;
    MaxBitCountModuleOffset: dword;
    MaxBitCountLevel: dword;
    MaxBitCountOffset: dword;

    MaskModuleIndex: dword;
    MaskLevel: dword;
    MaskOffset: dword;

    compressedEntry: pbytearray;
    compressedEntrySize: integer;

    mustendwithoffsetlistlength: integer;


    instantrescan: boolean;
    instantrescanlistcount: integer;
    instantrescanlist: array of TPointerListHandler;
    instantrescanaddress: array of ptruint;

    savestate: boolean;
    overflowqueuewriter:TQueueWriterMethod;

    function HasResultsPending: boolean;

    procedure SaveStateAndTerminate;

    procedure execute; override;
    constructor create(suspended: boolean);
    destructor destroy; override;

    property OnException: TNotifyEvent read fOnException write fOnException;
  end;

  TFlushResultsEvent=function(size: integer; m: TMemoryStream): boolean of object;
  TPointerscanWorkerNetwork=class(TPointerscanWorker)
  private
    fFlushSize: integer;
    fOnFlushResults: TFlushResultsEvent;
    resultscs: TCompressionstreamWithPositionSupport;
    resultsms: TMemorystream;
    procedure setFlushSize(size: integer);
  protected
    procedure initialize; override;
    procedure flushresults; override;
    procedure flushIfNeeded; override;
  public
    destructor destroy; override;

    property OnFlushResults: TFlushResultsEvent read fOnFlushResults write fOnFlushResults;
    property FlushSize: integer read fFlushSize write setFlushSize;
  end;

  TPointerscanWorkerLocal=class(TPointerscanWorker)
  private
    ffilename: string;
    resultsfile: tfilestream;
    resultsms: TMemorystream;
  protected
    procedure initialize; override;
    procedure flushresults; override;
  public
    constructor create(suspended: boolean; filename: string);
    destructor destroy; override;

    property filename: string read ffilename;
  end;

implementation

uses {$ifdef windows}frmMemoryAllocHandlerUnit,{$endif} pointerscancontroller;

//---------------Reversescanworker

procedure TPointerscanWorkerNetwork.initialize;
begin
  // nothing for now
  //fflushsize:=15*1024*1024;

  resultsms:=tmemorystream.create;
  resultscs:=TCompressionstreamWithPositionSupport.create(cldefault, resultsms);

  results:=resultscs;
end;

procedure TPointerscanWorkerNetwork.setFlushSize(size: integer);
begin
  fflushsize:=max(4096, min(size, 15*1024*1024)); //value between 1kb and 15mb

  //debug:
  //fflushsize:=0; //make it flush every time
end;

procedure TPointerscanWorkerNetwork.flushIfNeeded;
begin

  if (resultscs.Position>fflushsize) or (resultsms.position>fflushsize) then
    flushresults;
end;

procedure TPointerscanWorkerNetwork.flushresults;
var size: integer;
begin
  if not haserror then
  begin
    currentwritestart:=gettickcount64;
    isFlushing:=true;


    size:=resultscs.Position;

    resultscs.free;
    resultsms.position:=0;

    if assigned(fOnFlushResults) then
    begin
      while fOnFlushResults(size, resultsms)=false do
      begin
        if terminated then break;
        sleep(10+random(500));
      end;
    end;

    resultsms.position:=0;
    resultscs:=TCompressionstreamWithPositionSupport.create(cldefault, resultsms);

    results:=resultscs;

    isFlushing:=false;
    inc(timespentwriting, gettickcount64-currentwritestart);

    fHasResults:=false;
  end;
end;

destructor TPointerscanWorkerNetwork.destroy;
begin
  if resultscs<>nil then
    resultscs.free;

  if resultsms<>nil then
    resultsms.free;

  inherited destroy;
end;

//------------

procedure TPointerscanWorkerLocal.initialize;
begin
  resultsms:=tmemorystream.Create;
  resultsms.SetSize(16*1024*1024);

  results:=resultsms;



  if fileexists(filename) then
  begin
    //append to the end
    resultsfile:=tfilestream.Create(filename,fmOpenWrite or fmShareDenyNone);
    resultsfile.Seek(0, soEnd);
  end
  else
  begin
    //new file
    resultsfile:= tfilestream.Create(filename,fmcreate);
    resultsfile.free;
    resultsfile:= tfilestream.Create(filename,fmOpenWrite or fmShareDenyNone);
  end;
end;

procedure TPointerscanWorkerLocal.flushresults;
begin
  if not haserror then
  begin
    currentwritestart:=gettickcount64;
    isFlushing:=true;

    resultsfile.WriteBuffer(resultsms.Memory^,resultsms.Position);
    results.Seek(0,sofrombeginning);

    isFlushing:=false;
    inc(timespentwriting, gettickcount64-currentwritestart);
  end;
end;

constructor TPointerscanWorkerLocal.create(suspended: boolean; filename: string);
begin
  self.ffilename:=filename;

  inherited create(suspended);
end;

destructor TPointerscanWorkerLocal.destroy;
begin
  if resultsfile<>nil then
    freeandnil(resultsfile);

  if resultsms<>nil then
    resultsms.free;

  inherited destroy;
end;

procedure TPointerscanWorker.SaveStateAndTerminate;
begin
  savestate:=true;
  Terminate;
end;

function TPointerscanWorker.HasResultsPending: boolean;
begin
  result:=fHasResults;
end;

constructor TPointerscanWorker.create(suspended:boolean);
begin
  isdone:=true;

  pointersize:=processhandler.pointersize;

  inherited create(suspended);
end;

destructor TPointerscanWorker.destroy;
begin
  if compressedEntry<>nil then
    FreeMemAndNil(compressedEntry);

  inherited destroy;
end;



procedure TPointerscanWorker.execute;
var
  wr: dword;
  i: integer;
begin

  self.NameThreadForDebugging('Worker thread');
  try
    try
      Initialize;

      compressedEntrySize:=MaxBitCountModuleOffset+MaxBitCountModuleIndex+MaxBitCountLevel+MaxBitCountOffset*(maxlevel-mustendwithoffsetlistlength);
      compressedEntrySize:=(compressedEntrySize+7) div 8;

      getmem(compressedEntry, compressedEntrySize+4); //+4 so there's some space for overhead (writing using a dword pointer to the last byte)

      MaskModuleIndex:=0;
      for i:=1 to MaxBitCountModuleIndex do
        MaskModuleIndex:=(MaskModuleIndex shl 1) or 1;

      MaskLevel:=0;
      for i:=1 to MaxBitCountLevel do
        MaskLevel:=(MaskLevel shl 1) or 1;

      MaskOffset:=0;
      for i:=1 to MaxBitCountOffset do
        MaskOffset:=(MaskOffset shl 1) or 1;



      while (not terminated) do
      begin
        {$ifdef windows}
        wr:=WaitForSingleObject(pathqueueSemaphore, 500); //obtain semaphore
        if wr=WAIT_OBJECT_0 then
        {$else}
        if pathqueueSemaphore.TryAcquire(500) then
        {$endif}
        begin
          if stop or terminated then
          begin
            {$ifdef windows}
            ReleaseSemaphore(pathqueueSemaphore, 1, nil);
            {$else}
            pathqueueSemaphore.Release;
            {$endif}
            exit;
          end;


          //fetch the data from the queue and staticscanner
          if outofdiskspace^ then
          begin
            {$ifdef windows}
            ReleaseSemaphore(pathqueueSemaphore, 1, nil); //don't use it. give the semaphore back
            {$else}
            pathqueueSemaphore.Release;
            {$endif}

            sleep(2000);
            continue;
          end;



          pathqueueCS.Enter;
          if pathqueuelength^>0 then //should always be true due to the semaphore
          begin
            dec(pathqueuelength^);
            i:=pathqueuelength^;


            valuetofind:=pathqueue[i].valuetofind;
            startlevel:=pathqueue[i].startlevel;

            CopyMemory(@tempresults[0], @pathqueue[i].tempresults[0], maxlevel*sizeof(dword));
            if noLoop then
              CopyMemory(@valuelist[0], @pathqueue[i].valuelist[0], maxlevel*sizeof(ptruint));
          end;

          isdone:=false;
          pathqueueCS.Leave;


          try
            rscan(valuetofind,startlevel);
          finally
            isdone:=true;  //set isdone to true
          end;
        end;

        if stop or terminated then exit;
      end;

    except
      on e: exception do
      begin
        OutputDebugString('ScanWorker has error');
        haserror:=true;
        errorstring:='ReverseScanWorker:'+e.message;

        //tell all siblings they should kill themself. There is no reason to live for them...
        if assigned(fOnException) then
          fOnException(self);

        terminate;
      end;
    end;
  finally
    isdone:=true;
    hasTerminated:=true;

    flushresults;

    OutputDebugString('Scanworker is done');
  end;

end;




function TPointerscanWorker.DoRescan(level: valSint; moduleid: integer; offset: ptruint): boolean;
var
  i,j: integer;
  a: ptruint;

begin
  result:=false;
  for i:=0 to instantrescanlistcount-1 do
  begin
    a:=instantrescanlist[i].getAddressFromModuleIndexPlusOffset(moduleid, offset);

    for j:=level downto 0 do
    begin
      a:=instantrescanlist[i].getPointer(a);
      if a=0 then exit;
      a:=a+tempresults[j];
    end;

    if a<>instantrescanaddress[i] then exit;
  end;

  result:=true;
end;

procedure TPointerscanWorker.StorePath(level: valSint; moduleid: integer; offset: ptruint);
{Store the current path to memory and flush if needed}
var
  i: integer;

  bd8, bm8: dword;

  bit: integer;
begin
  if instantrescan and (not DoRescan(level, moduleid, offset)) then exit;

  //fill in the offset list
  inc(pointersfound);



  {
  if databaseptr? then
  begin
    //table with last offsets
    //table with secondary offsets
    //...
    //table with first offsets

    //table with results, containing columns for every offset and the base
    //moduleindex base offset1  offset2  offset3  offset4
    //------------------------------------------------------
    //0           1    reftooff1 reftooff2       3        4        5
  end
  else
  }
  if compressedptr then
  begin
    //leave the offset alone
    //compress the module index
    //compress the level
    //compress the tempresults (additionally, if alligned, shift by 2)


    //e.g: structsize 2048, maxlevel 5 , alligned, 100 modules in target
    //offset: 32 bits
    //module index(100) : 7 bits
    //level(5): 3 bits
    //tempresults(2048 alligned=512 , 9 bits/offset): 5*9=45
    // total/entry: 32+7+3+45=87 bits.  Align it to a byte boundary(88 bits)=11 bytes


    //as opposed to:
    //offset: 32 bits:
    //module index: 32 bits
    //level(5): 32
    //tempresults: 5*32=160
    //total/entry: 32+32+32+160=256 bits = 32 bytes

    //so, the compressed version should be almost 3 times as small on a default scan (the shifting and alignment might cause a slightly slower scan)

    if level<(mustendwithoffsetlistlength-1) then exit; //on a multi offset end scan, entries with a partial match resulting in a static are saved as well. Don't as they are not what the user wished, and would cause problems


    bit:=0;

    pqword(compressedEntry)^:=offset;
    bit:=bit+MaxBitCountModuleOffset;

    bd8:=bit shr 3; //bit div 8;
    pdword(@compressedEntry[bd8])^:=moduleid;
    bit:=bit+MaxBitCountModuleIndex;


    bd8:=bit shr 3; //bit div 8;
    bm8:=bit and $7; //bit mod 8;

    pdword(@compressedEntry[bd8])^:=pdword(@compressedEntry[bd8])^ and (not (MaskLevel shl bm8)) or ((1+(level-mustendwithoffsetlistlength)) shl bm8);
    bit:=bit+MaxBitCountLevel;    //next section



    //compress the offsets
    for i:=mustendwithoffsetlistlength to level do
    begin
      bd8:=bit shr 3; //bit div 8;
      bm8:=bit and $7; //bit mod 8;

      if alligned then
        pdword(@compressedEntry[bd8])^:=pdword(@compressedEntry[bd8])^ and (not (MaskOffset shl bm8)) or ((tempresults[i] shr 2) shl bm8)
      else
        pdword(@compressedEntry[bd8])^:=pdword(@compressedEntry[bd8])^ and (not (MaskOffset shl bm8)) or ((tempresults[i]) shl bm8);

      bit:=bit+MaxBitCountOffset;
    end;

    results.WriteBuffer(compressedEntry^, compressedEntrySize);

  end
  else
  begin
    results.WriteDword(moduleid);
    results.WriteQword(offset);

    i:=level+1; //store how many offsets are actually used (since all are saved)
    results.WriteDword(i);
    results.WriteBuffer(tempresults[0], maxlevel*sizeof(tempresults[0]) );
  end;

  fhasresults:=true;
  flushIfNeeded;
end;

procedure TPointerscanWorker.flushifneeded;
begin
  //default behaviour. Override for smaller buffers
  if results.position>15*1024*1024 then
    flushresults;
end;


procedure TPointerscanWorker.rscan(valuetofind:ptrUint; level: valSint);
{
scan through the memory for a address that points in the region of address, if found, recursive call till level maxlevel
}
var p: ^byte;
    pd: ^dword absolute p;
    pq: ^qword absolute p;


    i,j: valSint;
    addedToQueue: boolean;


    ExactOffset: boolean;
    {$ifdef windows}
    mae: TMemoryAllocEvent;
    {$endif}

  startvalue: ptrUint;
  stopvalue: ptrUint;
  plist: PPointerlist;

  nostatic: TStaticData;
  DontGoDeeper: boolean;
  DifferentOffsetsInThisNode: integer;
  locked: boolean;

begin
  if (level>=maxlevel) or (terminated and (savestate=false)) then
    exit;


  currentlevel:=level;
  DifferentOffsetsInThisNode:=0;


  exactOffset:=mustEndWithSpecificOffset and (length(mustendwithoffsetlist)-1>=level);

  if exactOffset then
  begin
    startvalue:=valuetofind-mustendwithoffsetlist[level];
    stopvalue:=startvalue;
  end
  else
  begin
    startvalue:=valuetofind-structsize;
    stopvalue:=valuetofind;
    if NegativeOffsets then inc(stopvalue, structsize);

    if startvalue>stopvalue then startvalue:=0;

    {$ifdef windows}
    if useheapdata then
    begin
      mae:=frmMemoryAllocHandler.FindAddress(@frmMemoryAllocHandler.HeapBaselevel, valuetofind);
      if mae<>nil then
      begin
        exactoffset:=true;
        startvalue:=mae.BaseAddress;
        stopvalue:=startvalue;
      end
      else //not static and not in heap
       if useOnlyHeapData then
         exit;
    end;
    {$endif}
  end;


  if noLoop then
  begin
    //check if this valuetofind is already in the list
    for i:=0 to level-1 do
      if valuelist[i]=valuetofind then
      begin
        exit;
      end;

    //add this valuetofind to the list
    valuelist[level]:=valuetofind;
  end;

  //lastaddress:=maxaddress;

  LookingForMin:=startvalue;
  LookingForMax:=stopvalue;

  dontGoDeeper:=false;
  plist:=nil;
  while stopvalue>=startvalue do
  begin
    if plist=nil then
      plist:=pointerlisthandler.findPointerValue(startvalue, stopvalue);

    if plist<>nil then
    begin
     { if stopvalue>valuetofind then
      asm
      nop
      end;  }
      tempresults[level]:=valuetofind-stopvalue; //store the offset




      //go through the list of addresses that have this address(stopvalue) as their value
      for j:=0 to plist.pos-1 do
      begin
        {$ifdef benchmarkps}
        inc(pathsevaluated);
        {$endif}


        if (plist.list[j].staticdata=nil) then //this removes a lot of other possible paths. Perhaps a feature to remove this check ?
        begin
          if (not dontGoDeeper) then
          begin
            //check if we should go deeper into these results (not if max level has been reached)


            if (level+1) < maxlevel then
            begin
              addedToQueue:=false;

              if (not terminated) and (not outofdiskspace^) then //if there is not enough diskspace left wait till it's terminated, or diskspace is freed
              begin

                if (
                    (level+3<maxlevel) and
                    (
                       ((pathqueuelength^<MAXQUEUESIZE - (MAXQUEUESIZE div 3))) or
                       ((level<=2) and (pathqueuelength^<MAXQUEUESIZE - (MAXQUEUESIZE div 8))) or
                       ((level<=1) and (pathqueuelength^<MAXQUEUESIZE - (MAXQUEUESIZE div 16))) or
                       ((level=0) and (pathqueuelength^<MAXQUEUESIZE - 1))
                    )
                   )
                or
                   (pathqueuelength^=0) //completely empty
                then //there's room and not a crappy work item. Add it
                begin
                  if (not Terminated) or savestate then
                  begin
                    //try to lock multiple times if high level pointers
                    locked:=pathqueueCS.tryEnter;
                    if not locked and (level<=2) then locked:=pathqueueCS.tryEnter;
                    if not locked and (level<=1) then
                    begin
                      //Two previous locks failed. Yield and try a lock again
                      sleep(0);
                      locked:=pathqueueCS.tryEnter;
                      if not locked then
                      begin
                        //one more time
                        sleep(0);
                        locked:=pathqueueCS.tryEnter;
                      end;
                    end;

                    if not locked and (level=0) then
                    begin
                      //I must have this lock
                      pathqueueCS.Enter;
                      locked:=true;
                    end;


                    if locked then
                    begin
                      if pathqueuelength^<MAXQUEUESIZE-1 then
                      begin
                        //still room

                        CopyMemory(@pathqueue[pathqueuelength^].tempresults[0], @tempresults[0], maxlevel*sizeof(dword));
                        if noLoop then
                          CopyMemory(@pathqueue[pathqueuelength^].valuelist[0], @valuelist[0], maxlevel*sizeof(ptruint));

                        pathqueue[pathqueuelength^].startlevel:=level+1;
                        pathqueue[pathqueuelength^].valuetofind:=plist.list[j].address;

                        inc(pathqueuelength^);
                        {$ifdef windows}
                        ReleaseSemaphore(pathqueueSemaphore, 1, nil);
                        {$else}
                        pathqueueSemaphore.Release;
                        {$endif}

                        addedToQueue:=true;
                      end;
                      pathqueueCS.Leave;
                    end;
                  end
                  else
                    exit;
                end;


                if not addedToQueue then
                begin
                  //I'll have to do it myself
                  rscan(plist.list[j].address,level+1);

                  ///done with this branch
                end;

              end
              else
              begin

                //!!Out of diskspace or terminated!!

                if outofdiskspace^ or savestate then //save to the overflowqueueu and return
                begin
                  //fill in the temppathqueue and send it to the overflowqueuewriter
                  temppathqueue.startlevel:=level+1;
                  temppathqueue.valuetofind:=plist.list[j].address;

                  if length(temppathqueue.tempresults)<length(tempresults) then
                    setlength(temppathqueue.tempresults, length(tempresults));

                  CopyMemory(@temppathqueue.tempresults[0], @tempresults[0], maxlevel*sizeof(dword));

                  if length(temppathqueue.valuelist)<length(valuelist) then
                     setlength(temppathqueue.valuelist, length(valuelist));

                  CopyMemory(@temppathqueue.valuelist[0], @valuelist[0], maxlevel*sizeof(PtrUInt));

                  overflowqueuewriter(self, temppathqueue);
                end
                else
                  exit;//it was a terminate

                //^^^^out of diskspace or save state!^^^^

              end;

              if (not staticonly) then //store this results entry
              begin
                nostatic.moduleindex:=$FFFFFFFF;
                nostatic.offset:=plist.list[j].address;
                StorePath(level,-1, plist.list[j].address);
              end;

            end
            else
            begin
              //end of the line
              if (not staticonly) then //store this results entry
              begin
                nostatic.moduleindex:=$FFFFFFFF;
                nostatic.offset:=plist.list[j].address;
                StorePath(level, -1, plist.list[j].address);
              end;
            end

          end; //else don't go deeper
        end
        else
        begin
          //found a static one
          StorePath(level, plist.list[j].staticdata.moduleindex, plist.list[j].staticdata.offset);

          if onlyOneStaticInPath then DontGoDeeper:=true;
        end;
      end;


      if LimitToMaxOffsetsPerNode then //check if the current itteration is less than maxOffsetsPerNode
      begin
        if level>0 then
          inc(DifferentOffsetsInThisNode);

        if (DifferentOffsetsInThisNode>=maxOffsetsPerNode) then
          exit; //the max node has been reached
      end;


      plist:=plist.previous;
      if plist<>nil then
        stopvalue:=plist.pointervalue
      else
        exit; //nothing else to be found

    end else
    begin
      {$ifdef benchmarkps}
      inc(pathsevaluated);
      {$endif}
      exit;
    end;

  end;
end;

end.

