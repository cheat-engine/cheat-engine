unit pointerscannerfrm;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, LResources, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, syncobjs,syncobjs2, Menus, math,
  frmRescanPointerUnit, pointervaluelist, rescanhelper,
  virtualmemory, symbolhandler,MainUnit,disassembler,CEFuncProc,NewKernelHandler,
  valuefinder, PointerscanresultReader, maps, zstream;


const staticscanner_done=wm_user+1;
const rescan_done=wm_user+2;
const open_scanner=wm_user+3;
const wm_starttimer=wm_user+4;

type
  TfrmPointerscanner = class;
  TRescanWorker=class(TThread)
  private
    procedure flushresults;
    function isMatchToValue(p: pointer): boolean;
  public
    filename: string;
    tempfile: tfilestream;
    tempbuffer: TMemoryStream;

    novaluecheck: boolean;
    PointerAddressToFind: ptrUint;
    forvalue: boolean;
    valuetype: TVariableType;
    valuescandword: dword;
    valuescansingle: single;
    valuescandouble: double;
    valuescansinglemax: single;
    valuescandoublemax: double;

    mustbeinrange: boolean;
    baseStart: ptruint;
    baseEnd: ptruint;

    startOffsetValues: array of dword;
    endoffsetvalues: array of dword;


    //---
    Pointerscanresults: TPointerscanresultReader;

    startentry: qword;
    EntriesToCheck: qword;

    rescanhelper: TRescanhelper;

    evaluated: qword;

    useluafilter: boolean; //when set to true each pointer will be passed on to the luafilter function
    luafilter: string; //function name of the luafilter

    procedure execute; override;
    destructor destroy; override;
  end;


  Trescanpointers=class(tthread)
  private

    procedure closeOldFile;
  public
    ownerform: TFrmPointerScanner;
    progressbar: tprogressbar;
    filename: string;
    overwrite: boolean;
    address: ptrUint;
    forvalue: boolean;
    delay: integer;
    valuetype: TVariableType;
    valuescandword: dword;
    valuescansingle: single;
    valuescandouble: double;
    valuescansinglemax: single;
    valuescandoublemax: double;

    mustbeinrange: boolean;
    baseStart: ptruint;
    baseEnd: ptruint;

    startOffsetValues: array of dword;
    endoffsetvalues: array of dword;

    novaluecheck: boolean; //when set to true the value and final address are not compared, just check that he final address is in fact readable
    useluafilter: boolean; //when set to true each pointer will be passed on to the luafilter function
    luafilter: string; //function name of the luafilter
    procedure execute; override;
  end;



  toffsetlist = array of dword;

  TStaticscanner = class;

  TReverseScanWorker = class (tthread)
  private
    offsetlist: array of dword;

    results: tmemorystream;
    resultsfile: tfilestream;
    pointersize: integer;

    procedure flushresults;
    procedure rscan(valuetofind:ptrUint; level: valSint);
    procedure StorePath(level: valSint; staticdata: PStaticData);

  public
    ownerform: TFrmPointerscanner;
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



    isdone: boolean;
    hasTerminated: boolean;
    stop: boolean;

    staticscanner: TStaticscanner;
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
    
    filename: string;

    haserror: boolean;
    errorstring: string;

    procedure execute; override;
    constructor create(suspended: boolean);
    destructor destroy; override;
  end;


  TStaticscanner = class(TThread)
  private
    reversescanners: array of treversescanworker;
    pointersize: integer;

    function ismatchtovalue(p: pointer): boolean;  //checks if the pointer points to a value matching the user's input
    procedure reversescan;

  public
    //reverse
    firstaddress: pointer;
    currentaddress: pointer;
    //lastaddress: pointer;

    lookingformin: ptrUint;
    lookingformax: ptrUint;

    reverseScanCS: TCriticalSection;
        
    //reverse^

    ownerform: TfrmPointerscanner;
    
    reverse: boolean;
    automatic: boolean;
    automaticaddress: ptrUint;

    startaddress: ptrUint;
    stopaddress: ptrUint;
    progressbar: TProgressbar;
    sz,sz0: integer;
    maxlevel: integer;
    unalligned: boolean;
    codescan: boolean;

    LimitToMaxOffsetsPerNode: boolean;
    MaxOffsetsPerNode: integer; //Sets how many different offsets per node should be handled at most (specifically mentioning different offsets since a pointervalue can have multiple addresses, meaning the same offset, different paths)


    fast: boolean;
    psychotic: boolean;
    writableonly: boolean;
    unallignedbase: boolean;

    useheapdata: boolean;
    useOnlyHeapData: boolean;

    findValueInsteadOfAddress: boolean;
    valuetype: TVariableType;
    valuescandword: dword;
    valuescansingle: single;
    valuescandouble: double;
    valuescansinglemax: single;
    valuescandoublemax: double;


    mustEndWithSpecificOffset: boolean;
    mustendwithoffsetlist: array of dword;
    onlyOneStaticInPath: boolean;
    noReadOnly: boolean;
    mustBeClassPointers: boolean; //when set the pointers must all point to a class object
    noLoop: boolean; //when set a pointerpath may not have the same address multiple times

    useStacks: boolean; //when set the stack regions will be marked as static
    stacksAsStaticOnly: boolean; //when set the only static addresses are stack addresses
    threadstacks: integer; //the number of stacks used as a lookup. (counted from first stack to newer ones)
    stacksize: integer; //Number of bytes in a stack


    threadcount: integer;
    scannerpriority: TThreadPriority;

    filename: string; //the final filename
    phase: integer;

    starttime: dword;

    isdone: boolean;

    staticonly: boolean; //for reverse

    pointersfound: qword;

    hasError: boolean;
    errorString: string;

    LoadedPointermapFilename: string;
    UseLoadedPointermap: boolean;

    pathqueuelength: integer;
    pathqueue: array [0..63] of record
      tempresults: array of dword;
      valuelist: array of ptruint;
      valuetofind: ptruint;
      startlevel: integer;

    end;
    pathqueueCS: TCriticalSection; //critical section used to add/remove entries
    pathqueueSemaphore: THandle; //Event to notify sleeping threads to wake up that there is a new path in the queue

    procedure execute; override;
    constructor create(suspended: boolean);
    destructor destroy; override;
  end;

  { Tfrmpointerscanner }

  Tfrmpointerscanner = class(TForm)
    btnStopRescanLoop: TButton;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    N2: TMenuItem;
    Open1: TMenuItem;
    Pointerscanner1: TMenuItem;
    Method3Fastspeedandaveragememoryusage1: TMenuItem;   //I should probably rename this, it's not really, 'average memory usage' anymore...
    N1: TMenuItem;
    Rescanmemory1: TMenuItem;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Timer2: TTimer;
    pgcPScandata: TPageControl;
    tsPSReverse: TTabSheet;
    tvRSThreads: TTreeView;
    Panel2: TPanel;
    Label5: TLabel;
    lblRSTotalStaticPaths: TLabel;
    Panel3: TPanel;
    btnStopScan: TButton;
    Label6: TLabel;
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    Resyncmodulelist1: TMenuItem;
    cbType: TComboBox;
    procedure btnStopRescanLoopClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1Resize(Sender: TObject);
    procedure Method3Fastspeedandaveragememoryusage1Click(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Rescanmemory1Click(Sender: TObject);
    procedure btnStopScanClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure New1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure Resyncmodulelist1Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
  private
    { Private declarations }
    start:tdatetime;

    rescan: trescanpointers;
    rescanpointerform: TFrmRescanPointer;
    pointerlisthandler: TReversePointerListHandler;   //handled by the form for easy reuse


    procedure m_staticscanner_done(var message: tmessage); message staticscanner_done;
    procedure rescandone(var message: tmessage); message rescan_done;
    procedure openscanner(var message: tmessage); message open_scanner;
    procedure _starttimer(var message: TMessage); message wm_starttimer;
    procedure doneui;
    procedure resyncloadedmodulelist;
    procedure OpenPointerfile(filename: string);
  public
    { Public declarations }
    Staticscanner:TStaticScanner;

    Pointerscanresults: TPointerscanresultReader;
  end;

//var
//  frmPointerScanner: TfrmPointerScanner;


implementation


uses PointerscannerSettingsFrm, frmMemoryAllocHandlerUnit, frmSortPointerlistUnit,
  LuaHandler, lauxlib, lua;

resourcestring
  rsErrorDuringScan = 'Error during scan';
  rsGeneratingPointermap = 'Generating pointermap...';
  rsIsNotAValid4ByteValue = '%s is not a valid 4 byte value';
  rsIsNotAValidFloatingPointValue = '%s is not a valid floating point value';
  rsIsNotAValidDoubleValue = '%s is not a valid double value';
  rsAddressSpecifiersFoundInTheWholeProcess = 'Address specifiers found in '
    +'the whole process';
  rsPointerPathsFound = 'Pointer paths found';
  rsThreads = 'Threads';
  rsEvaluated = 'Evaluated';
  rsTime = 'Time';
  rsThread = 'Thread';
  rsCurrentLevel = 'Current Level';
  rsLookingFor = 'Looking for';
  rsSleeping = 'Sleeping';
  rsActive = 'Active';
  rsBaseAddress = 'Base Address';
  rsOffset = 'Offset';
  rsPointsTo = 'Points to';
  rsPointercount = 'pointercount';
  rsOnlyTheFirst1000000EntriesWillBeDisplayed = 'Only the first 1000000 '
    +'entries will be displayed. Rescan will still work with all results.  ('
    +'This is completly normal for a pointerscan, you MUST do a few rescans)';
  rsPointerScan = 'Pointer scan';
  rsPointerscanResult = 'pointerscan result';

  rsTerminating = 'Terminating';
  rsStop = 'Stop';

//----------------------- scanner info --------------------------
//----------------------- staticscanner -------------------------


{$ifdef benchmarkps}
var
  totalpathsevaluated: qword;

  starttime: dword;
  startcount: qword;
{$endif}



procedure TFrmpointerscanner.doneui;
begin
  progressbar1.position:=0;
  progressbar1.visible:=false;

  pgcPScandata.Visible:=false;
  open1.Enabled:=true;
  new1.enabled:=true;
  rescanmemory1.Enabled:=true;

  if staticscanner<>nil then
    OpenPointerfile(staticscanner.filename);

  if rescan<>nil then
  begin
    OpenPointerfile(rescan.filename);
    freeandnil(rescan);
  end;
end;

procedure Tfrmpointerscanner.m_staticscanner_done(var message: tmessage);
begin
  if staticscanner=nil then exit;

  if staticscanner.useHeapData then
    frmMemoryAllocHandler.memrecCS.leave;  //continue adding new entries

  //update the treeview
  if message.WParam<>0 then
    messagedlg(rsErrorDuringScan+': '+pchar(message.LParam), mtError, [mbok] , 0);

  doneui;
end;


procedure TReverseScanWorker.flushresults;
begin
  resultsfile.WriteBuffer(results.Memory^,results.Position);
  results.Seek(0,sofrombeginning);
 // results.Clear;
end;

constructor TReverseScanWorker.create(suspended:boolean);
begin
  results:=tmemorystream.Create;
  results.SetSize(16*1024*1024);

  isdone:=true;

  pointersize:=processhandler.pointersize;

  inherited create(suspended);
end;

destructor TReverseScanWorker.destroy;
begin
  results.free;
  if resultsfile<>nil then
    freeandnil(resultsfile);
end;

procedure TReverseScanWorker.execute;
var wr: dword;
i: integer;
begin
  try
    try
      resultsfile:= tfilestream.Create(filename,fmcreate);
      resultsfile.free;
      resultsfile:= tfilestream.Create(filename,fmOpenWrite or fmShareDenyNone);

      while (not terminated) and (not self.staticscanner.Terminated) do
      begin
        wr:=WaitForSingleObject(self.staticscanner.pathqueueSemaphore, INFINITE);
        if stop then exit;
        if terminated then exit;
        if self.staticscanner.Terminated then exit;

        if wr=WAIT_OBJECT_0 then
        begin
          //fetch the data from the queue and staticscanner
          self.staticscanner.pathqueueCS.Enter;
          if self.staticscanner.pathqueuelength>0 then
          begin
            dec(staticscanner.pathqueuelength);
            i:=staticscanner.pathqueuelength;
            isdone:=false;
            maxlevel:=staticscanner.maxlevel;
            valuetofind:=staticscanner.pathqueue[i].valuetofind;
            startlevel:=staticscanner.pathqueue[i].startlevel;
            noLoop:=staticscanner.noLoop;
            structsize:=staticscanner.sz;

            CopyMemory(@tempresults[0], @staticscanner.pathqueue[i].tempresults[0], maxlevel*sizeof(dword));
            if noLoop then
              CopyMemory(@valuelist[0], @staticscanner.pathqueue[i].valuelist[0], maxlevel*sizeof(ptruint));
          end
          else
          begin
            //the semaphore is bugged
            beep;
          end;
          self.staticscanner.pathqueueCS.Leave;


          try
            rscan(valuetofind,startlevel);
          finally
            isdone:=true;  //set isdone to true
          end;
        end;
      end;

    except
      on e: exception do
      begin
        OutputDebugString('ScanWorker has error');
        haserror:=true;
        errorstring:='ReverseScanWorker:'+e.message;

        //tell all siblings they should terminate
        staticscanner.reverseScanCS.Enter;
        for i:=0 to length(staticscanner.reversescanners)-1 do
          staticscanner.reversescanners[i].Terminate;

        staticscanner.reverseScanCS.leave;
        terminate;
      end;
    end;
  finally
    isdone:=true;
    hasTerminated:=true;
    OutputDebugString('Scanworker is done');
  end;

end;


var fcount:qword=0;
var scount:qword=0;

procedure TReverseScanWorker.StorePath(level: valSint; staticdata: PStaticData);
{Store the current path to memory and flush if needed}
var i: integer;
begin
  inc(fcount); //increme
  if (staticdata=nil) then exit; //don't store it

  inc(scount);


  //fill in the offset list
  inc(staticscanner.pointersfound);

  results.WriteBuffer(staticdata.moduleindex, sizeof(staticdata.moduleindex));
  results.WriteBuffer(staticdata.offset,sizeof(staticdata.offset));
  i:=level+1; //store how many offsets are actually used (since all are saved)
  results.WriteBuffer(i,sizeof(i));
  results.WriteBuffer(tempresults[0], maxlevel*sizeof(tempresults[0]) ); //todo for 6.3+: Change sizeof(tempresult[0]) with the max size the structsize can generate./ (e.g 4096 is  only 2 bytes, 65536 =3)

  if results.position>15*1024*1024 then //bigger than 15mb
    flushresults;
end;

procedure TReverseScanWorker.rscan(valuetofind:ptrUint; level: valSint);
{
scan through the memory for a address that points in the region of address, if found, recursive call till level maxlevel
}
var p: ^byte;
    pd: ^dword absolute p;
    pq: ^qword absolute p;


    i,j: valSint;
    addedToQueue: boolean;


    ExactOffset: boolean;
    mae: TMemoryAllocEvent;

  startvalue: ptrUint;
  stopvalue: ptrUint;
  plist: PPointerlist;

  nostatic: TStaticData;
  DontGoDeeper: boolean;
  DifferentOffsetsInThisNode: integer;

begin
  if (level>=maxlevel) or (self.staticscanner.Terminated) or (terminated) then exit;

  currentlevel:=level;
  DifferentOffsetsInThisNode:=0;


  exactOffset:=staticscanner.mustEndWithSpecificOffset and (length(staticscanner.mustendwithoffsetlist)-1>=level);

  if exactOffset then
  begin
    startvalue:=valuetofind-staticscanner.mustendwithoffsetlist[level];
    stopvalue:=startvalue;
  end
  else
  begin
    startvalue:=valuetofind-structsize;
    stopvalue:=valuetofind;

    if staticscanner.useheapdata then
    begin
      mae:=frmMemoryAllocHandler.FindAddress(@frmMemoryAllocHandler.HeapBaselevel, valuetofind);
      if mae<>nil then
      begin
        exactoffset:=true;
        startvalue:=mae.BaseAddress;
        stopvalue:=startvalue;
      end
      else //not static and not in heap
       if staticscanner.useOnlyHeapData then
         exit;
    end;
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
      plist:=ownerform.pointerlisthandler.findPointerValue(startvalue, stopvalue);

    if plist<>nil then
    begin
      for j:=0 to plist.pos-1 do
      begin
        {$ifdef benchmarkps}
        inc(totalpathsevaluated);
        {$endif}

        tempresults[level]:=valuetofind-stopvalue; //store the offset
        if (plist.list[j].staticdata=nil) then
        begin
          if (not dontGoDeeper) then
          begin
            //check if whe should go deeper into these results (not if max level has been reached)


            if (level+1) < maxlevel then
            begin
              //todo: Use queue.  If there is space in the queue add it, else do it myself

              //not at max level, so scan for it
              //scan for this address
              //either wake a sleeping thread that can do this, or do it myself

              addedToQueue:=false;

              if staticscanner.pathqueuelength<63 then //there's room. Add it
              begin
                if (not Terminated) and (not self.staticscanner.Terminated) then
                begin
                  staticscanner.pathqueueCS.Enter;
                  if staticscanner.pathqueuelength<63 then
                  begin
                    //still room
                    CopyMemory(@staticscanner.pathqueue[staticscanner.pathqueuelength].tempresults[0], @tempresults[0], maxlevel*sizeof(dword));
                    if noLoop then
                      CopyMemory(@staticscanner.pathqueue[staticscanner.pathqueuelength].valuelist[0], @valuelist[0], maxlevel*sizeof(ptruint));

                    staticscanner.pathqueue[staticscanner.pathqueuelength].startlevel:=level+1;
                    staticscanner.pathqueue[staticscanner.pathqueuelength].valuetofind:=plist.list[j].address;

                    inc(staticscanner.pathqueuelength);
                    ReleaseSemaphore(staticscanner.pathqueueSemaphore, 1, nil);
                    addedToQueue:=true;
                  end;
                  staticscanner.pathqueueCS.Leave;
                end
                else
                  exit;
              end;


              if not addedToQueue then
              begin
                //I'll have to do it myself
                rscan(plist.list[j].address,level+1);
              end;
            end
            else
            begin
              //end of the line
              if (not staticonly) then //store this results entry
              begin
                nostatic.moduleindex:=$FFFFFFFF;
                nostatic.offset:=plist.list[j].address;
                StorePath(level,@nostatic);
              end;
            end

          end; //else don't go deeper
        end
        else
        begin
          //found a static one
          StorePath(level, plist.list[j].staticdata);

          if staticscanner.onlyOneStaticInPath then DontGoDeeper:=true;
        end;
      end;


      if LimitToMaxOffsetsPerNode then //check if the current itteration is less than maxOffsetsPerNode
      begin
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
      inc(totalpathsevaluated);
      {$endif}
      exit;
    end;

  end;
end;


//--------------------------STATICSCANNER------------------
function TStaticScanner.ismatchtovalue(p: pointer): boolean;
begin
  case valuetype of
    vtDword: result:=pdword(p)^=valuescandword;
    vtSingle: result:=(psingle(p)^>=valuescansingle) and (psingle(p)^<valuescansinglemax);
    vtDouble: result:=(pdouble(p)^>=valuescandouble) and (pdouble(p)^<valuescandoublemax);
  end;
end;

procedure TStaticScanner.reversescan;
{
Do a reverse pointer scan
}
var
  i,j: integer;
  alldone: boolean;

  currentaddress: ptrUint;
  addedToQueue: boolean;

  valuefinder: TValueFinder;
begin
  //scan the buffer
  fcount:=0; //debug counter to 0
  scount:=0;
  alldone:=false;

  try

    if maxlevel>0 then
    begin

      //initialize the first reverse scan worker
      //that one will spawn of all his other siblings if needed

      if Self.findValueInsteadOfAddress then
      begin
        //scan the memory for the value
        ValueFinder:=TValueFinder.create(startaddress,stopaddress);
        ValueFinder.alligned:=not unalligned;
        ValueFinder.valuetype:=valuetype;
        ValueFinder.valuescandword:=valuescandword;
        ValueFinder.valuescansingle:=valuescansingle;
        ValueFinder.valuescandouble:=valuescandouble;
        ValueFinder.valuescansinglemax:=valuescansinglemax;
        ValueFinder.valuescandoublemax:=valuescandoublemax;

        currentaddress:=ptrUint(ValueFinder.FindValue(startaddress));
        while (not terminated) and (currentaddress>0) do
        begin
          //if found, find a idle thread and tell it to look for this address starting from level 0 (like normal)
          addedToQueue:=false;
          while (not terminated) and (not addedToQueue) do
          begin
            if pathqueuelength<63 then //no need to lock
            begin
              pathqueueCS.enter;
              //setup the queueelement
              if pathqueuelength<63 then
              begin
                pathqueue[pathqueuelength].startlevel:=0;
                pathqueue[pathqueuelength].valuetofind:=currentaddress;

                inc(pathqueuelength);
                ReleaseSemaphore(pathqueueSemaphore, 1, nil);

                if unalligned then
                  currentaddress:=ValueFinder.FindValue(currentaddress+1)
                else
                  currentaddress:=ValueFinder.FindValue(currentaddress+pointersize);

                addedToQueue:=true;
              end;

              pathqueueCS.enter;
            end;

            if (not addedToQueue) and (not terminated) then
              sleep(500); //wait till there is space in the queue
          end;

        end;

        //done with the value finder, wait till all threads are done
        valuefinder.free;
      end
      else
      begin
        //initialize the first thread (it'll spawn all other threads)
        pathqueue[pathqueuelength].startlevel:=0;
        pathqueue[pathqueuelength].valuetofind:=self.automaticaddress;
        inc(pathqueuelength);
        ReleaseSemaphore(pathqueueSemaphore, 1, nil);
      end;

      //wait till all threads are in isdone state

      while (not alldone) do
      begin
        if Terminated then
        begin
          OutputDebugString('Forced terminate. Telling the scanworkers to die as well');
          //force the workers to die if they are sleeping
          for i:=0 to length(reversescanners)-1 do
          begin
            reversescanners[i].stop:=true;
            reversescanners[i].Terminate;
          end;

          ReleaseSemaphore(pathqueueSemaphore, 64, nil);

        end;

        sleep(500);
        alldone:=true;

        if pathqueuelength=0 then //it's 0
        begin
          //aquire a lock to see if it's still 0
          pathqueueCS.Enter;
          if pathqueuelength=0 then
          begin //still 0
            for i:=0 to length(reversescanners)-1 do
            begin
              if reversescanners[i].haserror then
              begin

                OutputDebugString('A worker had an error: '+reversescanners[i].errorstring);

                haserror:=true;
                errorstring:=reversescanners[i].errorstring;

                for j:=0 to length(reversescanners)-1 do reversescanners[j].terminate; //even though the reversescanner already should have done this, let's do it myself as well

                alldone:=true;
                break;
              end;

              if not (reversescanners[i].hasTerminated or reversescanners[i].isdone) then //isdone might be enabled
              begin
                if terminated then
                  OutputDebugString('Worker '+inttostr(i)+' is still active');

                alldone:=false;
                break;
              end;
            end;
          end
          else
            alldone:=false;

          pathqueueCS.Leave;
        end
        else
          alldone:=false;



      end;
    end;

    isdone:=true;


    //all threads are done
    for i:=0 to length(reversescanners)-1 do
      reversescanners[i].stop:=true;

    ReleaseSemaphore(pathqueueSemaphore, 64, nil);


    for i:=0 to length(reversescanners)-1 do
    begin
      reversescanners[i].WaitFor; //wait till this thread has terminated because the main thread has terminated
      if not haserror then
        reversescanners[i].flushresults;  //write unsaved results to disk

      reversescanners[i].Free;
      reversescanners[i]:=nil;
    end;

    setlength(reversescanners,0);


  finally
    if haserror then
      postmessage(ownerform.Handle,staticscanner_done,1,ptrUint(pchar(errorstring)))
    else
      postmessage(ownerform.Handle,staticscanner_done,0,maxlevel);
  end;

  terminate;
end;

procedure TStaticScanner.execute;
var
    i: integer;

    result: tfilestream;
    temp: dword;
    tempstring: string;

    f: tfilestream;
    ds: Tdecompressionstream;
begin
  if terminated then exit;
  try

    if ownerform.pointerlisthandler=nil then
    begin
      phase:=1;
      progressbar.Position:=0;
      try
        if useLoadedPointermap then
        begin
          f:=tfilestream.create(LoadedPointermapFilename, fmOpenRead);
          try
            ds:=Tdecompressionstream.create(f);
            try
              ownerform.pointerlisthandler:=TReversePointerListHandler.createFromStream(ds, progressbar);
            finally
              ds.free;
            end;
          finally
            f.free;
          end;
        end
        else
          ownerform.pointerlisthandler:=TReversePointerListHandler.Create(startaddress,stopaddress,not unalligned,progressbar, noreadonly, MustBeClassPointers, useStacks, stacksAsStaticOnly, threadstacks, stacksize);

        postmessage(ownerform.Handle, wm_starttimer, 0,0);


      except
        on e: exception do
        begin
          postmessage(ownerform.Handle,staticscanner_done,1,ptrUint(pchar('Failure copying target process memory'))); //I can just provide this string as it's static in the .code section
          terminate;
          exit;
        end;
      end;
    end; 


    phase:=2;
    progressbar.Position:=0;
  


    i:=0;

    maxlevel:=maxlevel-1;

    pathqueuelength:=0;
    pathqueueCS:=TCriticalSection.create;
    pathqueueSemaphore:=CreateSemaphore(nil, 0, 64, nil);

    for i:=0 to 63 do
    begin
      setlength(pathqueue[i].tempresults, maxlevel+1);
      if noLoop then
        setlength(pathqueue[i].valuelist, maxlevel+1);
    end;


    reverseScanCS:=tcriticalsection.Create;
    try
      setlength(reversescanners,threadcount);
      for i:=0 to threadcount-1 do
      begin
        reversescanners[i]:=TReverseScanWorker.Create(true);
        reversescanners[i].ownerform:=ownerform;
        reversescanners[i].Priority:=scannerpriority;
        reversescanners[i].staticscanner:=self;
        setlength(reversescanners[i].tempresults,maxlevel);
        setlength(reversescanners[i].offsetlist,maxlevel);

        if noloop then
          setlength(reversescanners[i].valuelist,maxlevel);

        reversescanners[i].staticonly:=staticonly;
        reversescanners[i].noLoop:=noLoop;

        reversescanners[i].LimitToMaxOffsetsPerNode:=LimitToMaxOffsetsPerNode;
        reversescanners[i].MaxOffsetsPerNode:=MaxOffsetsPerNode;

        reversescanners[i].alligned:=not self.unalligned;
        reversescanners[i].filename:=self.filename+'.'+inttostr(i);

        reversescanners[i].start;
      end;

      //create the headerfile
      result:=TfileStream.create(filename,fmcreate or fmShareDenyWrite);

      //save header (modulelist, and levelsize)
      ownerform.pointerlisthandler.saveModuleListToResults(result);

      //levelsize
      result.Write(maxlevel,sizeof(maxlevel)); //write max level (maxlevel is provided in the message (it could change depending on the settings)

      //pointerstores
      temp:=length(reversescanners);
      result.Write(temp,sizeof(temp));
      for i:=0 to length(reversescanners)-1 do
      begin
        tempstring:=ExtractFileName(reversescanners[i].filename);
        temp:=length(tempstring);
        result.Write(temp,sizeof(temp));
        result.Write(tempstring[1],temp);
      end;

      result.Free;


      reversescan;
    finally
      freeandnil(reverseScanCS);

      freeandnil(pathqueueCS);
      closehandle(pathqueueSemaphore);
    end;



  except
    on e: exception do
    begin


      haserror:=true;
      errorstring:='StaticScanner:'+e.message;
      postmessage(ownerform.Handle,staticscanner_done,1,ptrUint(pchar(errorstring)));
      terminate;
    end;
  end;

    
end;

constructor TStaticscanner.create(suspended: boolean);
begin
  pointersize:=processhandler.pointersize;

  inherited create(suspended);
end;

destructor TStaticscanner.destroy;
begin
  terminate;
  waitfor;

  //clean up other stuff
  inherited destroy;
end;

//---------------------------------main--------------------------

procedure Tfrmpointerscanner.Method3Fastspeedandaveragememoryusage1Click(
  Sender: TObject);
var
  i: integer;
  floataccuracy: integer;
  floatsettings: TFormatSettings;
begin
  FloatSettings:=DefaultFormatSettings;

  start:=now;
  if frmpointerscannersettings=nil then
    frmpointerscannersettings:=tfrmpointerscannersettings.create(application);

  if frmpointerscannersettings.Visible then exit; //already open, so no need to make again

  {
  if vm<>nil then
    frmpointerscannersettings.cbreuse.Caption:='Reuse memory copy from previous scan';}

  if frmpointerscannersettings.Showmodal=mrok then
  begin
    new1.click;

    starttime:=0;

    if not savedialog1.Execute then exit;

    if (frmpointerscannersettings.cbReusePointermap.checked=false) and (pointerlisthandler<>nil) then
      freeandnil(pointerlisthandler);

    frmpointerscannersettings.cbReusePointermap.enabled:=true; //if it was disabled it's safe to re-enable it now

        
    btnStopScan.enabled:=true;
    btnStopScan.Caption:=rsStop;

    pgcPScandata.Visible:=false;
    open1.Enabled:=false;
    new1.enabled:=false;
    rescanmemory1.Enabled:=false;

    cbType.Visible:=false;
    listview1.Visible:=false;




    //initialize array's




    //default scan
    staticscanner:=TStaticscanner.Create(true);

    label5.caption:=rsGeneratingPointermap;
    progressbar1.Visible:=true;

    try
      staticscanner.ownerform:=self;
      staticscanner.filename:=utf8toansi(savedialog1.FileName);
      staticscanner.reverse:=true; //since 5.6 this is always true

      staticscanner.noReadOnly:=frmpointerscannersettings.cbNoReadOnly.checked;
      staticscanner.mustBeClassPointers:=frmpointerscannersettings.cbClassPointersOnly.checked;

      staticscanner.useStacks:=frmpointerscannersettings.cbStaticStacks.checked;
      staticscanner.stacksAsStaticOnly:=frmPointerscannersettings.cbStackOnly.checked;
      staticscanner.threadstacks:=frmPointerscannersettings.threadstacks;
      staticscanner.stacksize:=frmPointerscannersettings.stacksize;

      staticscanner.UseLoadedPointermap:=frmpointerscannersettings.cbUseLoadedPointermap.Checked;
      staticscanner.LoadedPointermapFilename:=frmpointerscannersettings.odLoadPointermap.FileName;


      staticscanner.startaddress:=frmpointerscannersettings.start;
      staticscanner.stopaddress:=frmpointerscannersettings.Stop;

      staticscanner.unalligned:=not frmpointerscannersettings.CbAlligned.checked;
      pgcPScandata.ActivePage:=tsPSReverse;
      tvRSThreads.Items.Clear;


      staticscanner.codescan:=frmpointerscannersettings.codescan;
      staticscanner.staticonly:=frmpointerscannersettings.cbStaticOnly.checked;
      staticscanner.noLoop:=frmpointerscannersettings.cbNoLoop.checked;
      staticscanner.LimitToMaxOffsetsPerNode:=frmpointerscannersettings.cbMaxOffsetsPerNode.Checked;
      staticscanner.maxOffsetsPerNode:=frmpointerscannersettings.maxOffsetsPerNode;


      staticscanner.automatic:=true;

      staticscanner.automaticaddress:=frmpointerscannersettings.automaticaddress;
      staticscanner.sz:=frmpointerscannersettings.structsize;
      staticscanner.sz0:=frmpointerscannersettings.level0structsize;
      staticscanner.maxlevel:=frmpointerscannersettings.maxlevel;


      staticscanner.progressbar:=progressbar1;
      staticscanner.threadcount:=frmpointerscannersettings.threadcount;
      staticscanner.scannerpriority:=frmpointerscannersettings.scannerpriority;

      staticscanner.mustEndWithSpecificOffset:=frmpointerscannersettings.cbMustEndWithSpecificOffset.checked;
      if staticscanner.mustEndWithSpecificOffset then
      begin
        setlength(staticscanner.mustendwithoffsetlist, frmpointerscannersettings.offsetlist.count);
        for i:=0 to frmpointerscannersettings.offsetlist.count-1 do
          staticscanner.mustendwithoffsetlist[i]:=TOffsetEntry(frmpointerscannersettings.offsetlist[i]).offset;
      end;



      staticscanner.onlyOneStaticInPath:=frmpointerscannersettings.cbOnlyOneStatic.checked;

{$ifndef injectedpscan}
      staticscanner.useHeapData:=frmpointerscannersettings.cbUseHeapData.Checked;
      staticscanner.useOnlyHeapData:=frmpointerscannersettings.cbHeapOnly.checked;


      if staticscanner.useHeapData then
        frmMemoryAllocHandler.memrecCS.enter; //stop adding entries to the list
{$endif}        

      //check if the user choose to scan for addresses or for values
      staticscanner.findValueInsteadOfAddress:=frmpointerscannersettings.rbFindValue.checked;
      if staticscanner.findValueInsteadOfAddress then
      begin
        //if values, check what type of value
        floataccuracy:=pos(FloatSettings.DecimalSeparator,frmpointerscannersettings.edtAddress.Text);
        if floataccuracy>0 then
          floataccuracy:=length(frmpointerscannersettings.edtAddress.Text)-floataccuracy;

        case frmpointerscannersettings.cbValueType.ItemIndex of
          0:
          begin
            staticscanner.valuetype:=vtDword;
            val(frmpointerscannersettings.edtAddress.Text, staticscanner.valuescandword, i);
            if i>0 then raise exception.Create(Format(rsIsNotAValid4ByteValue, [frmpointerscannersettings.edtAddress.Text]));
          end;

          1:
          begin
            staticscanner.valuetype:=vtSingle;
            val(frmpointerscannersettings.edtAddress.Text, staticscanner.valuescansingle, i);
            if i>0 then raise exception.Create(Format(rsIsNotAValidFloatingPointValue, [frmpointerscannersettings.edtAddress.Text]));
            staticscanner.valuescansingleMax:=staticscanner.valuescansingle+(1/(power(10,floataccuracy)));
          end;

          2:
          begin
            staticscanner.valuetype:=vtDouble;
            val(frmpointerscannersettings.edtAddress.Text, staticscanner.valuescandouble, i);
            if i>0 then raise exception.Create(Format(rsIsNotAValidDoubleValue, [frmpointerscannersettings.edtAddress.Text]));
            staticscanner.valuescandoubleMax:=staticscanner.valuescandouble+(1/(power(10,floataccuracy)));            
          end;
        end;
      end;


      progressbar1.Max:=staticscanner.stopaddress-staticscanner.startaddress;


      open1.Enabled:=false;
      staticscanner.starttime:=gettickcount;
      staticscanner.start;


      pgcPScandata.Visible:=true;

      Method3Fastspeedandaveragememoryusage1.Enabled:=false;
    except
      on e: exception do
      begin
        staticscanner.Free;
        staticscanner:=nil;
        raise exception.create(e.message);
      end;
    end;

  end;
end;

procedure Tfrmpointerscanner.ListView1Resize(Sender: TObject);
var i,l: integer;
begin
  if listview1.Columns.Count>0 then
  begin
    l:=0;
    for i:=0 to listview1.columns.count-2 do
      inc(l,listview1.Columns[i].Width);

    l:=listview1.ClientWidth-l;
    l:=max(120,l);
    listview1.Columns[listview1.columns.count-1].Width:=l;
  end;
end;

procedure Tfrmpointerscanner.FormDestroy(Sender: TObject);
var x: array of integer;
begin
  setlength(x,1);
  x[0]:=cbtype.itemindex;
  SaveFormPosition(self, x);
end;

procedure Tfrmpointerscanner.btnStopRescanLoopClick(Sender: TObject);
begin
  btnStopRescanLoop.visible:=false;
  rescanpointerform.cbRepeat.checked:=false;
end;

procedure Tfrmpointerscanner.Button1Click(Sender: TObject);
var f: tfilestream;

  c: Tcompressionstream;
begin
  f:=tfilestream.create(Staticscanner.filename+'.scandata', fmCreate);

  c:=Tcompressionstream.Create(clfastest, f);
  pointerlisthandler.exportToStream(c);

  c.free;
  f.free;
end;

procedure Tfrmpointerscanner.FormResize(Sender: TObject);
begin
  btnStopRescanLoop.Left:=(clientwidth div 2) - (btnStopRescanLoop.Width div 2);
end;

procedure Tfrmpointerscanner.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
//Using dark byte's super secret "Screw this, I'll just split it into chunks" algorithm
var
  c: integer;
  frmSortPointerlist: TfrmSortPointerlist;
  tempname: string;

  oldname: string;
  oldlist: Tstringlist;
  tempfilelist: tstringlist;

  newname: string;
  i: integer;
  s: string;
begin
  c:=column.index;
  if c=listview1.ColumnCount-1 then exit; //raise exception.create('The result/value list is unsortable');
  if Pointerscanresults.count<=1 then exit; //don't even bother


  frmSortPointerlist:=TfrmSortPointerlist.Create(self);
  oldname:=Pointerscanresults.filename;
  oldlist:=tstringlist.create;
  tempfilelist:=tstringlist.create;
  Pointerscanresults.getFileList(oldlist);

  if frmSortPointerlist.dowork(column.index, oldname , tempname, tempfilelist) then
  begin
    //sorting done

    new1.Click;

    //delete the old pointerfiles
    for i:=0 to oldlist.Count-1 do
    begin
      s:=oldlist[i];
      DeleteFile(s);
    end;

    deletefile(oldname);
    renamefile(tempname, oldname);

    for i:=0 to tempfilelist.count-1 do
    begin
      newname:=StringReplace(tempfilelist[i], tempname, oldname,[]);
      DeleteFile(newname);

      RenameFile(tempfilelist[i], newname);
    end;


    OpenPointerfile(oldname);
  end;

  oldlist.free;

  frmSortPointerlist.free;
end;

procedure Tfrmpointerscanner.Timer2Timer(Sender: TObject);
var i,j: integer;
    s: string;

    tn,tn2: TTreenode;
begin
  if listview1.Visible then
    listview1.repaint;

  if pointerlisthandler<>nil then
    label6.caption:=rsAddressSpecifiersFoundInTheWholeProcess+':'+inttostr(pointerlisthandler.count);

  if staticscanner<>nil then
  try
    if staticscanner.isdone then
    begin
      if tvRSThreads.Items.Count>0 then
        tvRSThreads.Items.Clear;

      exit;
    end;

    if staticscanner.reverse then
    begin
      lblRSTotalStaticPaths.caption:=format(rsPointerPathsFound+': %d', [scount]);

{$ifdef benchmarkps}
      if (starttime=0) and (totalpathsevaluated<>0) then
      begin
        startcount:=totalpathsevaluated;  //get the count from this point on
        starttime:=gettickcount;
      end;

      //label5.caption:=format('Threads: Evaluated: %d Time: %d  (%d / s)',[totalpathsevaluated-startcount, ((gettickcount-starttime) div 1000), trunc(((totalpathsevaluated-startcount)/((gettickcount-starttime) / 1000))) ]);
      label5.caption:=format(rsThreads+': '+rsEvaluated+': %d '+rsTime+': %' +'d  (%d / ' +'s)', [totalpathsevaluated-startcount, ((gettickcount-starttime) div 1000), trunc(((totalpathsevaluated-startcount)/(gettickcount-starttime))*1000)]);
      label5.Width:=label5.Canvas.TextWidth(label5.caption);
{$endif}


      if tvRSThreads.Items.Count<length(staticscanner.reversescanners) then
      begin
        //add them

        for i:=0 to length(staticscanner.reversescanners)-1 do
        begin
          tn:=tvRSThreads.Items.Add(nil, rsThread+' '+inttostr(i+1));
          tvRSThreads.Items.AddChild(tn, rsCurrentLevel+':0');
          tvRSThreads.Items.AddChild(tn, rsLookingFor+' :0-0');
        end;
      end;

      tn:=tvRSThreads.Items.GetFirstNode;
      i:=0;
      while tn<>nil do
      begin
        if staticscanner.reversescanners[i].isdone then
        begin
          tn.Text:=rsThread+' '+inttostr(i+1)+' ('+rsSleeping+')';
          tn2:=tn.getFirstChild;
          tn2.text:=rsSleeping;
          tn2:=tn2.getNextSibling;
          tn2.text:=rsSleeping;
        end
        else
        begin
          tn.text:=rsThread+' '+inttostr(i+1)+' ('+rsActive+')';
          tn2:=tn.getFirstChild;

          begin
            s:='';
            for j:=0 to staticscanner.reversescanners[i].currentlevel-1 do
              s:=s+' '+inttohex(staticscanner.reversescanners[i].tempresults[j],8);


            tn2.text:=rsCurrentLevel+':'+inttostr(staticscanner.reversescanners[
              i].currentlevel)+' ('+s+')';
            tn2:=tn2.getNextSibling;
            tn2.text:=rsLookingFor+' :'+inttohex(staticscanner.reversescanners[i
              ].lookingformin, 8)+'-'+inttohex(staticscanner.reversescanners[i
              ].lookingformax, 8); ;
          end;
        end;

        tn:=tn.getNextSibling;
        inc(i);
      end;
    end
    else
    begin

    end;


  except

  end;
end;

procedure Tfrmpointerscanner.OpenPointerfile(filename: string);
var
  i: integer;

  col_baseaddress:TListColumn;
  col_pointsto: TListColumn;
  col_offsets: Array of TListColumn;
begin
  new1.Click;

  Pointerscanresults:=TPointerscanresultReader.create(filename);

  listview1.Items.BeginUpdate;
  listview1.Columns.BeginUpdate;  
  listview1.Items.Count:=0;
  listview1.Columns.Clear;

  col_baseaddress:=listview1.Columns.Add;
  col_baseaddress.Caption:=rsBaseAddress;
  col_baseaddress.Width:=150;
  col_baseaddress.MinWidth:=20;

  setlength(col_offsets, Pointerscanresults.offsetCount);
  for i:=0 to Pointerscanresults.offsetCount-1 do
  begin
    col_offsets[i]:=listview1.Columns.Add;
    col_offsets[i].Caption:=rsOffset+' '+inttostr(i);
    col_offsets[i].Width:=80;
    col_offsets[i].MinWidth:=10;
  end;

  col_pointsto:=listview1.Columns.Add;
  col_pointsto.Caption:=rsPointsTo+':';
  col_pointsto.Width:=120;
  col_pointsto.MinWidth:=10;
  col_pointsto.AutoSize:=true;




  panel1.Caption:=rsPointercount+':'+inttostr(Pointerscanresults.count);

  if (Pointerscanresults.count>1000000) then
  begin
    listview1.Items.Count:=1000000;
    showmessage(rsOnlyTheFirst1000000EntriesWillBeDisplayed);
  end else listview1.Items.Count:=Pointerscanresults.count;

  listview1.Align:=alClient;
  listview1.Visible:=true;

  listview1.Columns.EndUpdate;
  listview1.Items.EndUpdate;

  cbtype.top:=0;
  cbtype.height:=panel1.ClientHeight;
  cbtype.Visible:=true;

  Rescanmemory1.Enabled:=true;
  new1.Enabled:=true;

  caption:=rsPointerScan+' : '+extractfilename(filename);
end;

procedure Tfrmpointerscanner.Open1Click(Sender: TObject);
begin
  if opendialog1.Execute then
    OpenPointerfile(utf8toansi(Opendialog1.filename));
end;

function TRescanWorker.isMatchToValue(p:pointer): boolean;
begin
  case valuetype of
    vtDword: result:=pdword(p)^=valuescandword;
    vtSingle: result:=(psingle(p)^>=valuescansingle) and (psingle(p)^<valuescansinglemax);
    vtDouble: result:=(pdouble(p)^>=valuescandouble) and (pdouble(p)^<valuescandoublemax);
  end;
end;

procedure TRescanWorker.flushresults;
begin
  tempfile.WriteBuffer(tempbuffer.Memory^,tempbuffer.Position);
  tempbuffer.Seek(0,sofrombeginning);
end;

destructor TRescanworker.destroy;
begin
  if Pointerscanresults<>nil then
    Pointerscanresults.Free;
end;

procedure TRescanWorker.execute;
var
    currentEntry: qword;
    i,j: integer;

    baseaddress, address,address2: ptrUint;
    pa: PPointerAddress;
    x: dword;
    valid: boolean;

    tempvalue: pointer;
    value: pointer;
    valuesize: integer;

    p: ppointerscanresult;
    pointersize: integer;

    L: Plua_State;
    lref: integer;
    lfun: integer;
    ltable: integer;

begin
  l:=nil;
  if useluafilter then
  begin
    //create a new lua thread
    luacs.enter;
    try
      l:=lua_newthread(luavm); //pushes the thread on the luavm stack.
      lref:=luaL_ref(luavm,LUA_REGISTRYINDEX); //add a reference so the garbage collector wont destroy the thread (pops the thread off the stack)
    finally
      luacs.leave;
    end;


    lua_getglobal(L, pchar(luafilter));
    lfun:=lua_gettop(L);

    //create a table for the offset
    lua_createtable(L, Pointerscanresults.offsetCount+1,0);   //+1 for a nil
    ltable:=lua_gettop(L);
  end;


  tempfile:=nil;
  tempbuffer:=nil;
  address:=0;
  address2:=0;
  pointersize:=processhandler.pointersize;

  if forvalue and (valuetype=vtDouble) then valuesize:=8 else valuesize:=4;

  getmem(tempvalue,valuesize);

  try
    tempfile:=tfilestream.Create(self.filename, fmCreate);
    tempbuffer:=TMemoryStream.Create;
    tempbuffer.SetSize(16*1024*1024);

    evaluated:=0;
    currentEntry:=self.startentry;

    if currentEntry>Pointerscanresults.count then exit;


    while evaluated < self.EntriesToCheck do
    begin
      p:=Pointerscanresults.getPointer(currentEntry);
      if p<>nil then
      begin
        valid:=true;
        if p.modulenr=-1 then
          address:=p.moduleoffset
        else
          address:=Pointerscanresults.getModuleBase(p.modulenr)+p.moduleoffset;

        baseaddress:=address;

        if address>0 then
        begin
          //if the base must be in a range then check if the base address is in the given range
          if (not mustbeinrange) or (inrangex(address, baseStart, baseEnd)) then
          begin
            //don't care or in range.

            //check if start offet values are given
            if length(startOffsetValues)>0 then
            begin
              //check the offsets
              for i:=0 to length(startOffsetValues)-1 do
                if p.offsets[p.offsetcount-1-i]<>startOffsetValues[i] then
                begin
                  valid:=false;
                  break;
                end;
            end;

            if valid and (length(endoffsetvalues)>0) then
            begin
              j:=0;
              for i:=length(endoffsetvalues)-1 downto 0 do
              begin
                if p.offsets[j]<>endoffsetvalues[i] then
                begin
                  valid:=false;
                  break;
                end;
                inc(j);
              end;
            end;

            if valid then
            begin
              //evaluate the pointer to address
              for i:=p.offsetcount-1 downto 0 do
              begin
                pa:=rescanhelper.findPointer(address);
                if pa=nil then
                begin
                  if readprocessmemory(processhandle, pointer(address), @address2, pointersize, x) then
                    pa:=rescanhelper.AddPointer(address, address2)
                  else
                    pa:=rescanhelper.AddPointer(address, 0);
                end;

                if pa.value>0 then
                  address:=pa.value+p.offsets[i]
                else
                begin
                  valid:=false;
                  break; //invalid pointer
                end;
              end;

            end;

            if valid then
            begin
              if novaluecheck or forvalue then
              begin
                //evaluate the address (address must be accessible)
                if rescanhelper.ispointer(address) then
                begin
                  if novaluecheck=false then //check if the value is correct
                  begin
                    value:=rescanhelper.findAddress(address);
                    if value=nil then
                    begin
                      //value is not yet stored, fetch it and add it to the list
                      if ReadProcessMemory(processhandle,pointer(address),tempvalue,valuesize,x) then
                        value:=rescanhelper.AddAddress(address, tempvalue, valuesize)
                      else
                        valid:=false; //unreadable even though ispointer returned true....
                    end;


                    if (value=nil) or (not isMatchToValue(value)) then
                      valid:=false; //invalid value
                  end;
                end else valid:=false; //unreadable address
              end
              else
              begin
                //check if the address matches
                if address<>PointerAddressToFind then
                  valid:=false;
              end;
            end;

            if valid and useluafilter then
            begin
              //check the lua function
              //first set the offsets
              for i:=0 to p.offsetcount-1 do
              begin
                lua_pushinteger(L, p.offsets[i]);
                lua_rawseti(L, ltable, i+1);
              end;

              //end the table with a nil marker
              lua_pushnil(L);
              lua_rawseti(L, ltable, p.offsetcount+1);

              //setup the function call
              lua_pushvalue(L, lfun);           //function
              lua_pushinteger(L, baseaddress);  //base
              lua_pushvalue(L, ltable);         //offsets
              lua_pushinteger(L, address);      //address
              lua_call(L, 3,1);                 //call and don't expect any errors
              valid:=lua_toboolean(L, -1);
              lua_pop(L, 1);
            end;

            if valid then
            begin
              //checks passed, it's valid
              tempbuffer.Write(p^,Pointerscanresults.entrySize);
              if tempbuffer.Position>16*1024*1024 then flushresults;
            end;


          end; //must be in range and it wasn't in the range
        end; //else not a valid module
      end;

      inc(evaluated);
      inc(currentEntry);
    end;

    flushresults;
  finally
    freemem(tempvalue);
    
    if tempfile<>nil then
      freeandnil(tempfile);

    if tempbuffer<>nil then
      freeandnil(tempbuffer);

    if l<>nil then
    begin
      lua_settop(L, 0);

      //remove the reference to the thread
      luacs.enter;
      try
        luaL_unref(LuaVM, LUA_REGISTRYINDEX, lref);
      finally
        luacs.leave;
      end;

    end;

  end;
end;

procedure TRescanpointers.closeOldFile;
begin
  ownerform.New1Click(ownerform.new1);
end;

procedure TRescanpointers.execute;
var
  tempstring: string;
  i,j: integer;

  TotalPointersToEvaluate: qword;
  PointersEvaluated: qword;

  rescanworkercount: integer;
  rescanworkers: array of TRescanWorker;
  blocksize: qword;

  threadhandles: array of Thandle;
  result: tfilestream;

  rescanhelper: TRescanHelper;
  temp: dword;


begin

  progressbar.Min:=0;
  progressbar.Max:=100;
  progressbar.Position:=0;
  result:=nil;


  sleep(delay*1000);

  rescanhelper:=TRescanHelper.create;

  ownerform.resyncloadedmodulelist;

  //fill the modulelist with baseaddresses
  try
    //the modulelist now holds the baseaddresses (0 if otherwhise)
    TotalPointersToEvaluate:=ownerform.pointerscanresults.count;


    //spawn all threads
    rescanworkercount:=GetCPUCount;
    if HasHyperthreading then rescanworkercount:=(rescanworkercount div 2)+1;

//    rescanworkercount:=1;   //only one for now. Todo: Make this multithreaded

    blocksize:=TotalPointersToEvaluate div rescanworkercount;
    if blocksize<8 then blocksize:=8;

    setlength(rescanworkers, rescanworkercount);
    setlength(threadhandles, rescanworkercount);
    for i:=0 to rescanworkercount-1 do
    begin
      rescanworkers[i]:=TRescanWorker.Create(true);


      rescanworkers[i].Pointerscanresults:=TPointerscanresultReader.create(ownerform.Pointerscanresults.filename);
     { rescanworkers[i].OriginalFilename:=ownerform.pointerscanresults.filename;
      rescanworkers[i].OriginalFileEntrySize:=ownerform.pointerscanresults.sizeOfEntry;
      rescanworkers[i].OriginalFileStartPosition:=ownerform.pointerscanresults.StartPosition;
      rescanworkers[i].offsetlength:=ownerform.OpenedPointerfile.offsetlength;
      rescanworkers[i].modulelist:=ownerform.OpenedPointerfile.modulelist;    }
      rescanworkers[i].PointerAddressToFind:=self.address;
      rescanworkers[i].novaluecheck:=novaluecheck;

      rescanworkers[i].forvalue:=forvalue;
      rescanworkers[i].valuetype:=valuetype;
      rescanworkers[i].valuescandword:=valuescandword;
      rescanworkers[i].valuescansingle:=valuescansingle;
      rescanworkers[i].valuescandouble:=valuescandouble;
      rescanworkers[i].valuescansinglemax:=valuescansinglemax;
      rescanworkers[i].valuescandoublemax:=valuescandoublemax;

      rescanworkers[i].rescanhelper:=rescanhelper;

      if overwrite then
        rescanworkers[i].filename:=self.filename+'.'+inttostr(i)+'.overwrite'      
      else
        rescanworkers[i].filename:=self.filename+'.'+inttostr(i);

      rescanworkers[i].startEntry:=blocksize*i;
      rescanworkers[i].entriestocheck:=blocksize;
      if i=rescanworkercount-1 then
        rescanworkers[i].entriestocheck:=TotalPointersToEvaluate-rescanworkers[i].startEntry; //to the end


      rescanworkers[i].mustbeinrange:=mustbeinrange;
      rescanworkers[i].baseStart:=baseStart;
      rescanworkers[i].baseEnd:=baseEnd;
      setlength(rescanworkers[i].startOffsetValues, length(startoffsetvalues));
      for j:=0 to length(startOffsetValues)-1 do
        rescanworkers[i].startOffsetValues[j]:=startOffsetValues[j];

      setlength(rescanworkers[i].endoffsetvalues, length(endoffsetvalues));
      for j:=0 to length(EndOffsetValues)-1 do
        rescanworkers[i].EndOffsetValues[j]:=EndOffsetValues[j];


      rescanworkers[i].useluafilter:=useluafilter;
      rescanworkers[i].luafilter:=luafilter;


      threadhandles[i]:=rescanworkers[i].Handle;
      rescanworkers[i].start;
    end;


    if overwrite then
      result:=TFileStream.Create(filename+'.overwrite',fmCreate)
    else
      result:=TFileStream.Create(filename,fmCreate);

    //write header
    //modulelist
    ownerform.pointerscanresults.saveModulelistToResults(result);

    //offsetlength
    result.Write(ownerform.pointerscanresults.offsetcount, sizeof(dword));

    //pointerstores
    temp:=length(rescanworkers);
    result.Write(temp,sizeof(temp));
    for i:=0 to length(rescanworkers)-1 do
    begin
      tempstring:=ExtractFileName(rescanworkers[i].filename);
      if overwrite then
        tempstring:=copy(tempstring,1,length(tempstring)-10);
        
      temp:=length(tempstring);
      result.Write(temp,sizeof(temp));
      result.Write(tempstring[1],temp);
    end;

    result.Free;

    while WaitForMultipleObjects(rescanworkercount, @threadhandles[0], true, 1000) = WAIT_TIMEOUT do      //wait
    begin
      //query all threads the number of pointers they have evaluated
      PointersEvaluated:=0;
      for i:=0 to rescanworkercount-1 do
        inc(PointersEvaluated,rescanworkers[i].evaluated);

      progressbar.Position:=PointersEvaluated div (TotalPointersToEvaluate div 100);
    end;
    //no timeout, so finished or crashed

    if overwrite then //delete the old ptr file
    begin
      synchronize(closeoldfile);
      DeleteFile(filename);
      RenameFile(filename+'.overwrite',filename);
    end;

    //destroy workers
    for i:=0 to rescanworkercount-1 do
    begin
      rescanworkers[i].WaitFor; //just to be sure
      rescanworkers[i].Free;

      if overwrite then
      begin
        DeleteFile(filename+'.'+inttostr(i));
        RenameFile(filename+'.'+inttostr(i)+'.overwrite', filename+'.'+inttostr(i));
      end;
    end;


    rescanworkercount:=0;
    setlength(rescanworkers,0);


  finally
    if rescanhelper<>nil then
      freeandnil(rescanhelper);

    progressbar.Position:=0;
    postmessage(ownerform.Handle,rescan_done,0,0);


  end;

end;

procedure Tfrmpointerscanner.Rescanmemory1Click(Sender: TObject);
var address: ptrUint;
    FloatSettings: TFormatSettings;
    floataccuracy: integer;
    i: integer;
begin
  floatsettings:=DefaultFormatSettings;


  if rescan<>nil then
    freeandnil(rescan);

  rescan:=trescanpointers.create(true);
  rescan.ownerform:=self;
  rescan.progressbar:=progressbar1;
  progressbar1.visible:=true;



  try
    if rescanpointerform=nil then
      rescanpointerform:=TFrmRescanPointer.Create(self);

    with rescanpointerform do
    begin

      if (rescanpointerform.cbRepeat.checked) or (showmodal=mrok) then
      begin
        if (rescanpointerform.cbRepeat.checked) or savedialog1.Execute then
        begin
          rescan.novaluecheck:=cbNoValueCheck.checked;

          if cbRepeat.Checked then
          begin
            //show the stop rescan repeat button
            btnStopRescanLoop.Visible:=true;
            btnStopRescanLoop.BringToFront;
          end;



          rescan.filename:=utf8toansi(savedialog1.filename);
          if cbDelay.checked then
            rescan.delay:=delay
          else
            rescan.delay:=0;

          rescan.mustbeinrange:=cbBasePointerMustBeInRange.checked;
          if rescan.mustbeinrange then
          begin
            rescan.BaseStart:=baseStart;
            rescan.BaseEnd:=baseEnd;
          end;

          if cbMustStartWithSpecificOffsets.checked then
          begin
            setlength(rescan.startOffsetValues, length(startOffsetValues));
            for i:=0 to length(startOffsetValues)-1 do
              rescan.startOffsetValues[i]:=startOffsetValues[i];
          end
          else
            setlength(rescan.startOffsetValues,0); //shouldn't be necesary, but just in case


          if cbMustEndWithSpecificOffsets.checked then
          begin
            setlength(rescan.endOffsetValues, length(endOffsetValues));
            for i:=0 to length(endOffsetValues)-1 do
              rescan.endOffsetValues[i]:=endOffsetValues[i];
          end
          else
            setlength(rescan.endoffsetvalues,0);

          if uppercase(rescan.filename)=uppercase(pointerscanresults.filename) then
            rescan.overwrite:=true;



          Rescanmemory1.Enabled:=false;
          new1.Enabled:=false;

          if cbNoValueCheck.checked=false then
          begin
            if rbFindAddress.Checked then
            begin
              try
                address:=StrToQWordEx('$'+edtAddress.Text);


              //rescan the pointerlist
              except
                raise exception.create('Find by address requires an address. "'+edtaddress.text+'" is not a valid address');
              end;

              rescan.address:=address;
              rescan.forvalue:=false;

            end
            else
            begin

              //if values, check what type of value
              floataccuracy:=pos(FloatSettings.DecimalSeparator,edtAddress.Text);
              if floataccuracy>0 then
                floataccuracy:=length(edtAddress.Text)-floataccuracy;

              case cbValueType.ItemIndex of
                0:
                begin
                  rescan.valuetype:=vtDword;
                  val(edtAddress.Text, rescan.valuescandword, i);
                  if i>0 then raise exception.Create(Format(
                    rsIsNotAValid4ByteValue, [edtAddress.Text]));
                end;

                1:
                begin
                  rescan.valuetype:=vtSingle;
                  val(edtAddress.Text, rescan.valuescansingle, i);
                  if i>0 then raise exception.Create(Format(
                    rsIsNotAValidFloatingPointValue, [edtAddress.Text]));
                  rescan.valuescansingleMax:=rescan.valuescansingle+(1/(power(10,floataccuracy)));
                end;

                2:
                begin
                  rescan.valuetype:=vtDouble;
                  val(edtAddress.Text, rescan.valuescandouble, i);
                  if i>0 then raise exception.Create(Format(
                    rsIsNotAValidDoubleValue, [edtAddress.Text]));
                  rescan.valuescandoubleMax:=rescan.valuescandouble+(1/(power(10,floataccuracy)));
                end;
              end;

              rescan.forvalue:=true;
            end;


          end;

          rescan.useLuaFilter:=cbLuaFilter.checked;
          rescan.LuaFilter:=edtRescanFunction.text;



          rescan.start;
        end;
      end;

    end;


  except
    on e: exception do
    begin
      Rescanmemory1.Enabled:=true;
      new1.Enabled:=true;


      freeandnil(rescan);
      raise exception.create(e.message);
    end;

  end;


end;

procedure tfrmpointerscanner.rescandone(var message: tmessage);
{
The rescan is done. rescan.oldpointerlist (the current pointerlist) can be deleted
and the new pointerlist becomes the current pointerlist
}
begin
  doneui;

  if rescan<>nil then
    freeandnil(rescan);
    


  if (rescanpointerform<>nil) and rescanpointerform.cbRepeat.checked then
  begin
    //repeat
    Rescanmemory1.Click;
  end
  else
  begin
    Rescanmemory1.Enabled:=true;
    new1.Enabled:=true;
  end;
end;

procedure Tfrmpointerscanner.btnStopScanClick(Sender: TObject);
begin
  if staticscanner<>nil then
  begin
    btnStopScan.Caption:=rsTerminating;
    btnStopScan.enabled:=false;
    staticscanner.Terminate;
  end;
end;

procedure Tfrmpointerscanner.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Staticscanner<>nil then
  begin
    Staticscanner.Terminate;
    Staticscanner.WaitFor;
    freeandnil(Staticscanner);
  end;

  new1.Click;

  if pointerlisthandler<>nil then
    freeandnil(pointerlisthandler);


  action:=cafree; //on close free itself
end;

procedure Tfrmpointerscanner.openscanner(var message: tmessage);
begin
  if frmpointerscannersettings=nil then
    frmpointerscannersettings:=tfrmpointerscannersettings.create(application);

  frmpointerscannersettings.edtAddress.text:=inttohex(message.WParam,8);
  Method3Fastspeedandaveragememoryusage1.Click;
end;

procedure Tfrmpointerscanner._starttimer(var message: TMessage);
begin
  ProgressBar1.Visible:=false;
  timer2.enabled:=true;
end;


procedure Tfrmpointerscanner.New1Click(Sender: TObject);
begin
  btnStopScan.click;

  if staticscanner<>nil then
    freeandnil(staticscanner);

 
  pgcPScandata.Visible:=false;
  panel1.Caption:='';
  open1.Enabled:=true;
  new1.enabled:=true;
  rescanmemory1.Enabled:=false;

  listview1.Items.BeginUpdate;
  listview1.columns.BeginUpdate;
  listview1.Columns.Clear;
  listview1.Items.Count:=0;

  listview1.Items.EndUpdate;
  listview1.Columns.EndUpdate;

  Method3Fastspeedandaveragememoryusage1.Enabled:=true;

  if Pointerscanresults<>nil then
    freeandnil(Pointerscanresults);

end;



procedure Tfrmpointerscanner.FormCreate(Sender: TObject);
var x: array of integer;
begin
  tsPSReverse.TabVisible:=false;

  {$ifdef injectedpscan}
  caption:='CE Injected Pointerscan';
  {$endif}
  listview1.DoubleBuffered:=true;

  listview1.Align:=alClient;
  listview1.Visible:=true;

  setlength(x,1);
  if loadformposition(self,x) then
    cbtype.itemindex:=x[0];

end;

procedure Tfrmpointerscanner.ListView1Data(Sender: TObject;
  Item: TListItem);
var
  p: PPointerscanResult;
  i: integer;
  s: string;
  check: boolean; 
  doublevalue: double;
  dwordvalue: dword absolute doublevalue; //make sure of the same memory
  floatvalue: single absolute doublevalue;
  x: dword;

  address: ptrUint;

begin
  if Pointerscanresults<>nil then
  begin

    p:=Pointerscanresults.getPointer(item.index, address);
    if p<>nil then //just to be safe
    begin
      if p.modulenr=-1 then
        item.Caption:=inttohex(p.moduleoffset,8)
      else
      begin
        if p.moduleoffset>=0 then
          item.Caption:=ansitoutf8('"'+pointerscanresults.getModulename(p.modulenr)+'"+'+inttohex(p.moduleoffset,8))
        else
          item.Caption:=ansitoutf8('"'+pointerscanresults.getModulename(p.modulenr)+'"-'+inttohex(-p.moduleoffset,8));
      end;

      for i:=p.offsetcount-1 downto 0 do
        item.SubItems.Add(inttohex(p.offsets[i],1));

      for i:=p.offsetcount to Pointerscanresults.offsetCount-1 do
        item.SubItems.Add('');

      if address=0 then
        item.SubItems.Add('-') else
      begin
        s:=inttohex(address,8);
        if cbType.ItemIndex<>-1 then
        begin
          s:=s+' = ';
          if cbType.ItemIndex=2 then
            check:=readprocessmemory(processhandle, pointer(address),@doublevalue,8,x) else
            check:=readprocessmemory(processhandle, pointer(address),@doublevalue,4,x);

          if check then
          begin
            case cbType.ItemIndex of
              0: s:=s+inttostr(dwordvalue);
              1: s:=s+floattostr(floatvalue);
              2: s:=s+floattostr(doublevalue);
            end;
          end else s:=s+'??';
        end;

        item.SubItems.Add(s);

      end;
    end;
  end;
end;

procedure Tfrmpointerscanner.resyncloadedmodulelist;
begin
  pointerscanresults.resyncModulelist;
end;

procedure Tfrmpointerscanner.Resyncmodulelist1Click(Sender: TObject);
begin
  resyncloadedmodulelist;
  listview1.Refresh;
end;

procedure Tfrmpointerscanner.ListView1DblClick(Sender: TObject);
var
  li: tlistitem;
  i: integer;
  offsets: array of integer;
  t: string;
  c: integer;

  vtype: TVariableType;
begin
  if listview1.ItemIndex<>-1 then
  begin
    li:=listview1.Items.Item[listview1.ItemIndex];
    t:=utf8toansi(li.caption);

    try
      setlength(offsets,li.SubItems.Count);
      c:=0;

      for i:=li.SubItems.Count-2 downto 0 do
      begin
        if li.SubItems[i]='' then continue;
        offsets[c]:=strtoint('$'+li.SubItems[i]);
        inc(c);
      end;


      case cbType.ItemIndex of
        1: vtype:=vtSingle;
        2: vtype:=vtDouble;
        else vtype:=vtDword;
      end;

      mainform.addresslist.addaddress(rsPointerscanResult, t, offsets, c, vtype);
    except

    end;
  end;
end;

procedure Tfrmpointerscanner.cbTypeChange(Sender: TObject);
begin
  listview1.Refresh;

end;

initialization
  {$i pointerscannerfrm.lrs}

end.


