unit pointerscannerfrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, syncobjs,syncobjs2, Menus, math,
  frmRescanPointerUnit, pointervaluelist, rescanhelper, 
  virtualmemory, symbolhandler,mainunit,disassembler,cefuncproc,newkernelhandler;


const staticscanner_done=wm_user+1;
const rescan_done=wm_user+2;
const open_scanner=wm_user+3;

type
  TfrmPointerscanner = class;
  TRescanWorker=class(TThread)
  private
    procedure flushresults;
  public
    filename: string;
    tempfile: tfilestream;
    tempbuffer: TMemoryStream;

    PointerAddressToFind: dword;
    //---
    OriginalFilename: string;
    OriginalFileEntrySize: integer;
    OriginalFileStartPosition: integer;
    offsetlength: integer;
    modulelist: tstringlist;

    startentry: uint64;
    EntriesToCheck: uint64;

    rescanhelper: TRescanhelper;

    evaluated: uint64;
    procedure execute; override;
  end;


  Trescanpointers=class(tthread)
  private
    function ismatchtovalue(p: pointer): boolean;
  public
    ownerform: TFrmPointerScanner;
    progressbar: tprogressbar;
    filename: string;
    address: dword;
    forvalue: boolean;
    valuetype: TVariableType;
    valuescandword: dword;
    valuescansingle: single;
    valuescandouble: double;
    valuescansinglemax: single;
    valuescandoublemax: double;

    procedure execute; override;
  end;



  toffsetlist = array of dword;

  TStaticscanner = class;

  TReverseScanWorker = class (tthread)
  private
    offsetlist: array of dword;
    results: tmemorystream;
    resultsfile: tfilestream;


    procedure flushresults;
    procedure rscan(valuetofind:dword; level: integer);
    procedure StorePath(level: integer; staticdata: PStaticData=nil);

  public
    ownerform: TFrmPointerscanner;
    valuetofind: dword;
    maxlevel: integer;
    structsize: integer;
//    startaddress: dword;
    startlevel: integer;
    alligned: boolean;
    staticonly: boolean;

    isdone: boolean;
    startworking: tevent;
    stop: boolean;

    staticscanner: TStaticscanner;
    tempresults: array of dword;


    //info:
    currentaddress: pointer;
    currentlevel: integer;
    LookingForMin: dword;
    LookingForMax: dword;
    lastaddress: dword;
    
    filename: string;

    procedure execute; override;
    constructor create(suspended: boolean);
    destructor destroy; override;
  end;


  TStaticscanner = class(TThread)
  private
    updateline: integer; //not used for addentry

    memoryregion: array of tmemoryregion;

    reversescanners: array of treversescanworker;

    function ismatchtovalue(p: pointer): boolean;  //checks if the pointer points to a value matching the user's input
    procedure reversescan;

  public
    //reverse
    firstaddress: pointer;
    currentaddress: pointer;
    lastaddress: pointer;

    lookingformin: dword;
    lookingformax: dword;

    reverseScanCS: TCriticalSection;
    reverseScanSemaphore: tsemaphore;
        
    //reverse^

    ownerform: TfrmPointerscanner;
    
    reverse: boolean;
    automatic: boolean;
    automaticaddress: dword;

    start: dword;
    stop: dword;
    progressbar: TProgressbar;
    sz,sz0: integer;
    maxlevel: integer;
    unalligned: boolean;
    codescan: boolean;


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


    threadcount: integer;
    scannerpriority: TThreadPriority;

    filenames: array of string;
    filename: string; //the final filename
    phase: integer;
    currentpos: ^Dword;

    starttime: dword;

    isdone: boolean;

    staticonly: boolean; //for reverse

    pointersfound: uint64;

    procedure execute; override;
    destructor destroy; override;
  end;

  Tfrmpointerscanner = class(TForm)
    ProgressBar1: TProgressBar;
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    N2: TMenuItem;
    Open1: TMenuItem;
    Pointerscanner1: TMenuItem;
    Method3Fastspeedandaveragememoryusage1: TMenuItem;
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
    lblRSTotalPaths: TLabel;
    Panel3: TPanel;
    btnStopScan: TButton;
    Label6: TLabel;
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    Resyncmodulelist1: TMenuItem;
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
  private
    { Private declarations }
    start:tdatetime;

    rescan: trescanpointers;
    cewindowhandle: thandle;
    pointerlisthandler: TReversePointerListHandler;   //handled by the form for easy reuse

    procedure m_staticscanner_done(var message: tmessage); message staticscanner_done;
    procedure rescandone(var message: tmessage); message rescan_done;
    procedure openscanner(var message: tmessage); message open_scanner;
    procedure doneui;
    procedure resyncloadedmodulelist;
    procedure OpenPointerfile(filename: string);
  public
    { Public declarations }
    Staticscanner:TStaticScanner;

    OpenedPointerfile: record
      filename: string;
      pointerfile: Tfilestream;
      modulelist: Tstringlist;
      offsetlength: integer;
      StartPosition: integer;
      SizeOfEntry: integer;
      TotalPointers: uint64;
    end;

    tempoffset: array of dword; //used by ondata

  end;

//var
//  frmPointerScanner: TfrmPointerScanner;


implementation

{$R *.dfm}

uses PointerscannerSettingsFrm, frmMemoryAllocHandlerUnit;


//----------------------- scanner info --------------------------
//----------------------- staticscanner -------------------------




procedure TFrmpointerscanner.doneui;
begin
  progressbar1.position:=0;

  pgcPScandata.Visible:=false;
  open1.Enabled:=true;
  new1.enabled:=true;
  rescanmemory1.Enabled:=true;

  OpenPointerfile(staticscanner.filename);

end;

procedure Tfrmpointerscanner.m_staticscanner_done(var message: tmessage);
var x: tfilestream;
    result: tfilestream;
    i: integer;
begin

  if staticscanner=nil then exit;

{$ifndef injectedpscan}
  if staticscanner.useHeapData then
    frmMemoryAllocHandler.displaythread.Resume; //continue adding new entries
{$endif}


  //now combile all thread results to 1 file
  result:=TfileStream.create(staticscanner.filename,fmcreate);

  //save header (modulelist, and levelsize)
  pointerlisthandler.saveModuleListToResults(result);

  result.Write(message.LParam,sizeof(message.LParam)); //write max level (maxlevel is provided in the message (it could change depending on the settings)



  for i:=0 to length(staticscanner.filenames)-1 do
  begin
    x:=tfilestream.Create(staticscanner.filenames[i],fmopenread);
    result.CopyFrom(x,0);
    x.free;
    deletefile(staticscanner.filenames[i]);
  end;
  result.Free;

  setlength(staticscanner.filenames,1);
  staticscanner.filenames[0]:={$ifdef injectedpscan}scansettings.{$endif}cheatenginedir+'result.ptr';

  //update the treeview
  if message.WParam<>0 then
  begin
    messagedlg('Error during scan: '+pchar(message.LParam), mtError, [mbok] ,0);


  end;


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

  startworking:=tevent.create(nil,false,false,'');
  isdone:=true;

  inherited create(suspended);
end;

destructor TReverseScanWorker.destroy;
begin
  results.free;
  if resultsfile<>nil then
    freeandnil(resultsfile);

  startworking.free;
end;

procedure TReverseScanWorker.execute;
var wr: twaitresult;
begin
  filename:=staticscanner.filename+'.'+inttostr(getcurrentthreadid);
  resultsfile:= tfilestream.Create(filename,fmcreate);

  while not terminated do
  begin
    wr:=startworking.WaitFor(infinite);
    if stop then exit;

    if wr=wrSignaled then
    begin
      try
        rscan(valuetofind,startlevel);
      finally
        isdone:=true;  //set isdone to true
        staticscanner.reversescansemaphore.release;
      end;
    end;
  end;

end;


var fcount:uint64=0;
var scount:uint64=0;

procedure TReverseScanWorker.StorePath(level: integer; staticdata: PStaticData=nil);
{Store the current path to memory and flush if needed}
var i: integer;
    x: dword;

    foundstatic: boolean;
    mi: tmoduleinfo;
begin
  inc(fcount); //increme
  if (staticdata=nil) and staticonly then exit; //don't store it

  inc(scount);


  //fill in the offset list
  inc(staticscanner.pointersfound);

  //for i:=0 to level do
  //  offsetlist[level-i]:=tempresults[i];


  results.WriteBuffer(staticdata.moduleindex, sizeof(staticdata.moduleindex));
  results.WriteBuffer(staticdata.offset,sizeof(staticdata.offset));
  i:=level+1; //store many offsets are actually used (since all are saved)
  results.WriteBuffer(i,sizeof(i));
  results.WriteBuffer(tempresults[0], maxlevel*sizeof(tempresults[0]) );

  if results.position>15*1024*1024 then //bigger than 15mb
    flushresults;

end;

procedure TReverseScanWorker.rscan(valuetofind:dword; level: integer);
{
scan through the memory for a address that points in the region of address, if found, recursive call till level maxlevel
}
var p: ^byte;
    pd: ^dword absolute p;
    maxaddress: dword;
    AddressMinusMaxStructSize: dword;
    found: boolean;
    i,j,k: integer;
    createdworker: boolean;

    mi: tmoduleinfo;
    mbi: _MEMORY_BASIC_INFORMATION;

    ExactOffset: boolean;
{$ifndef injectedpscan}
    mae: TMemoryAllocEvent;
{$endif}

  originalStartvalue: dword;
  startvalue: dword;
  stopvalue: dword;
  plist: PPointerlist;
begin
  currentlevel:=level;
  if (level>=maxlevel) then //in the previous version the check if it was a static was done here, that is now done earlier
  begin
    //reached max level
    if (not staticonly) then //store this results entry
      StorePath(level-1);
    exit;
  end;

  if self.staticscanner.Terminated then
    exit;


 {
  p:=vm.GetBuffer;  }

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
 {
  maxaddress:=dword(p)+vm.GetBufferSize;   }
  lastaddress:=maxaddress;

  LookingForMin:=startvalue;
  LookingForMax:=stopvalue;


  found:=false;
  while startvalue<=stopvalue do
  begin

    plist:=ownerform.pointerlisthandler.findPointerValue(startvalue, stopvalue);
    if plist<>nil then
    begin
      found:=true;
      for j:=0 to plist.pos-1 do
      begin

        tempresults[level]:=valuetofind-startvalue; //store the offset
        if plist.list[j].staticdata=nil then
        begin
          //check if whe should go deeper into these results (not if max level has been reached)

          if (level+1) >= maxlevel then
          begin
            if (not staticonly) then //store this results entry
              StorePath(level-1);
          end
          else
          begin
            //not at max level, so scan for it
            //scan for this address
            //either spawn of a new thread that can do this, or do it myself

            createdworker:=false;
            staticscanner.reverseScanCS.Enter;

            //scan the worker thread array for a idle one, if found use it
            for i:=0 to length(staticscanner.reversescanners)-1 do
            begin
              if staticscanner.reversescanners[i].isdone then
              begin
                staticscanner.reversescanners[i].isdone:=false;
                staticscanner.reversescanners[i].maxlevel:=maxlevel;
                staticscanner.reversescanners[i].valuetofind:=plist.list[j].address;

                for k:=0 to maxlevel-1 do
                  staticscanner.reversescanners[i].tempresults[k]:=tempresults[k]; //copy results

                staticscanner.reversescanners[i].startlevel:=level+1;
                staticscanner.reversescanners[i].structsize:=structsize;
                staticscanner.reversescanners[i].startworking.SetEvent;
                createdworker:=true;
                break;
              end;
            end;

            staticscanner.reverseScanCS.Leave;


            if not createdworker then
            begin
              //I'll have to do it myself
              rscan(plist.list[j].address,level+1);
            end;
          end;

        end
        else
        begin
          //found a static one
          StorePath(level, plist.list[j].staticdata);

          if staticscanner.onlyOneStaticInPath then exit;
        end;
      end;

      if not staticscanner.unalligned then
        startvalue:=startvalue+4
      else
        startvalue:=startvalue+1;
        
    end else
    begin
      if (not found) and (not staticonly) then
      begin
        //nothing was found, let's just say this is the final level and store it...
        StorePath(level-1);
      end;
      exit;
    end;

  end;
end;

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
var p: ^byte;
    pd: ^dword absolute p;
    maxaddress: dword;
    automaticAddressMinusMaxStructSize: dword;

    results: array of dword;
    i,j: integer;
    alldone: boolean;
    {$ifdef injectedpscan}
    mbi: _MEMORY_BASIC_INFORMATION;
    {$endif}

    exactoffset: boolean;
{$ifndef injectedpscan}
    mae: TMemoryAllocEvent;
{$endif}

  startvalue: dword;
  stopvalue: dword;

  plist: PPointerList;
begin
  //scan the buffer
  fcount:=0; //debug counter to 0
  scount:=0;
  alldone:=false;

  if not findValueInsteadOfAddress then
    maxlevel:=maxlevel-1; //adjustment for this kind of scan

  setlength(results,maxlevel);  

  //initialize the first reverse scan worker
  //that one will spawn of all his other siblings

  reversescanners[0].isdone:=false;
  reversescanners[0].maxlevel:=maxlevel;

  reversescanners[0].valuetofind:=self.automaticaddress;
  reversescanners[0].structsize:=sz;
  reversescanners[0].startlevel:=0;
  reversescanners[0].startworking.SetEvent;


  //wait till all threads are in isdone state
  while (not alldone) do
  begin
    sleep(500);
    alldone:=true;

    //no need for a CS here since it's only a read, and even when a new thread is being made, the creator also has the isdone boolean to false
    for i:=0 to length(reversescanners)-1 do
    begin
      if not reversescanners[i].isdone then
      begin
        alldone:=false;
        break;
      end;
    end;
  end;

  isdone:=true;


  //all threads are done
  setlength(filenames,length(reversescanners));

  for i:=0 to length(reversescanners)-1 do
  begin
    reversescanners[i].stop:=true;
    reversescanners[i].startworking.SetEvent;  //run it in case it was waiting
    reversescanners[i].WaitFor; //wait till this thread has terminated because the main thread has terminated
    reversescanners[i].flushresults;  //write results to disk
    filenames[i]:=reversescanners[i].filename;
    reversescanners[i].Free;
    reversescanners[i]:=nil;
  end;

  setlength(reversescanners,0);

  postmessage(ownerform.Handle,staticscanner_done,0,maxlevel);
  terminate;
  freeandnil(reversescansemaphore);
end;

procedure TStaticScanner.execute;
var
    i,j,k: integer;
    x,opcode:string;

    t:dword;
    hexcount,hexstart: integer;
    isstruct: boolean;
    isstatic: boolean;
    found: boolean;

    mbi: _MEMORY_BASIC_INFORMATION;

    tn,tempnode: ttreenode;
    lastnode: ttreenode;
    oldshowsymbols: boolean;
    oldshowmodules: boolean;


    bitcount: integer;

    scanregions: tmemoryregions;
    currentregion: integer;
    maxpos: dword;
    dw: byte;

begin
  if terminated then exit;

  if ownerform.pointerlisthandler=nil then
  begin
    phase:=1;
    progressbar.Position:=0;
    try
      ownerform.pointerlisthandler:=TReversePointerListHandler.Create(start,stop,not unalligned,progressbar);
    except
      postmessage(ownerform.Handle,staticscanner_done,1,dword(pchar('Failure copying target process memory'))); //I can just priovide this string as it's static in the .code section
      terminate;
      exit;
    end;
  end; 


  phase:=2;
  progressbar.Position:=0;
  
  currentpos:=pointer(start);





  i:=0;

  if reverse then  //always true since 5.6
  begin
    reverseScanCS:=tcriticalsection.Create;
    try
      reverseScanSemaphore:=tsemaphore.create(threadcount);
      setlength(reversescanners,threadcount);
      for i:=0 to threadcount-1 do
      begin
        reversescanners[i]:=TReverseScanWorker.Create(true);
        reversescanners[i].ownerform:=ownerform;
        reversescanners[i].Priority:=scannerpriority;
        reversescanners[i].staticscanner:=self;
        setlength(reversescanners[i].tempresults,maxlevel);
        setlength(reversescanners[i].offsetlist,maxlevel);
        reversescanners[i].staticonly:=staticonly;
        reversescanners[i].alligned:=not self.unalligned;


        reversescanners[i].Resume;
      end;
      reversescan;
    finally
      freeandnil(reverseScanCS);
    end;

  end;

  {
  if (vm<>nil) and (not reuse) then
    freeandnil(vm);   }
    
end;


destructor TStaticscanner.destroy;
begin
  terminate;
  waitfor;

  if reverseScanSemaphore<>nil then
    freeandnil(reverseScanSemaphore);

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
  GetLocaleFormatSettings(GetThreadLocale, FloatSettings);
  
  start:=now;
  if frmpointerscannersettings=nil then
    frmpointerscannersettings:=tfrmpointerscannersettings.create(nil);

  if frmpointerscannersettings.Visible then exit; //already open, so no need to make again

  {
  if vm<>nil then
    frmpointerscannersettings.cbreuse.Caption:='Reuse memory copy from previous scan';}

  if frmpointerscannersettings.Showmodal=mrok then
  begin
    new1.click;

    if not savedialog1.Execute then exit;
        

    pgcPScandata.Visible:=false;
    open1.Enabled:=false;
    new1.enabled:=false;
    rescanmemory1.Enabled:=false;

    timer2.Enabled:=true;

    //initialize array's




    //default scan
    staticscanner:=TStaticscanner.Create(true);

    try
      staticscanner.ownerform:=self;
      staticscanner.filename:=savedialog1.FileName;
      staticscanner.reverse:=true; //since 5.6 this is always true

      staticscanner.start:=frmpointerscannersettings.start;
      staticscanner.stop:=frmpointerscannersettings.Stop;

      staticscanner.unalligned:=not frmpointerscannersettings.CbAlligned.checked;
      pgcPScandata.ActivePage:=tsPSReverse;
      tvRSThreads.Items.Clear;


      staticscanner.codescan:=frmpointerscannersettings.codescan;
      staticscanner.staticonly:=frmpointerscannersettings.cbStaticOnly.checked;

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
        frmMemoryAllocHandler.displaythread.Suspend; //stop adding entries to the list
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
            if i>0 then raise exception.Create(frmpointerscannersettings.edtAddress.Text+' is not a valid 4 byte value');
          end;

          1:
          begin
            staticscanner.valuetype:=vtSingle;
            val(frmpointerscannersettings.edtAddress.Text, staticscanner.valuescansingle, i);
            if i>0 then raise exception.Create(frmpointerscannersettings.edtAddress.Text+' is not a valid floating point value');
            staticscanner.valuescansingleMax:=staticscanner.valuescansingle+(1/(power(10,floataccuracy)));
          end;

          2:
          begin
            staticscanner.valuetype:=vtDouble;
            val(frmpointerscannersettings.edtAddress.Text, staticscanner.valuescandouble, i);
            if i>0 then raise exception.Create(frmpointerscannersettings.edtAddress.Text+' is not a valid double value');
            staticscanner.valuescandoubleMax:=staticscanner.valuescandouble+(1/(power(10,floataccuracy)));            
          end;
        end;
      end;


      progressbar1.Max:=staticscanner.stop-staticscanner.start;


      open1.Enabled:=false;
      staticscanner.starttime:=gettickcount;
      staticscanner.Resume;


      pgcPScandata.Visible:=true;
    except
      on e: exception do
      begin
        staticscanner.Free;
        staticscanner:=nil;
        raise e;
      end;
    end;

  end;
end;

procedure Tfrmpointerscanner.Timer2Timer(Sender: TObject);
var i,j,l: integer;
    s: string;
    a: string;
    smallestaddress: dword;
    todo: dword;
    done: dword;
    donetime,todotime: integer;
    oneaddresstime: double;
    _h,_m,_s: integer;
    tn,tn2: TTreenode;
begin

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
      lblRSTotalPaths.caption:=format('Total pointer paths encountered: %d ',[fcount]);
      lblRSTotalStaticPaths.caption:=format('Of those %d have a static base',[scount]);

      if scount>fcount then  lblRSTotalStaticPaths.caption:= lblRSTotalStaticPaths.caption+' WTF?';

      if pointerlisthandler<>nil then
        label6.caption:='Pointer addresses found in the whole process:'+inttostr(pointerlisthandler.count);
        
      //{$ifdef injectedpscan
      //lblRSCurrentAddress.Caption:=format('Currently at address %p (going till %p)',[staticscanner.currentaddress, staticscanner.lastaddress]);
     // {$else
      //if vm<>nil then
  //    lblRSCurrentAddress.Caption:=format('Currently at address %0.8x (going till %0.8x)',[vm.PointerToAddress(staticscanner.currentaddress), vm.PointerToAddress(staticscanner.lastaddress)]);
      //{$endif

      {

      label2.Caption:=inttostr(scount)+' of '+inttostr(fcount);
      label6.caption:='Looking for :'+inttohex(staticscanner.lookingformin,8)+'-'+inttohex(staticscanner.lookingformax,8);;
     
      if staticscanner.phase=2 then
      begin
        //calculate time left
        todo:=dword(staticscanner.lastaddress)-dword(staticscanner.currentaddress);
        done:=dword(staticscanner.currentaddress)-dword(staticscanner.firstaddress);
      end;

      }
      if tvRSThreads.Items.Count<length(staticscanner.reversescanners) then
      begin
        //add them

        for i:=0 to length(staticscanner.reversescanners)-1 do
        begin
          tn:=tvRSThreads.Items.Add(nil,'Thread '+inttostr(i+1));
          tvRSThreads.Items.AddChild(tn,'Current Level:0');
          tvRSThreads.Items.AddChild(tn,'Looking for :0-0');
        end;
      end;

      tn:=tvRSThreads.Items.GetFirstNode;
      i:=0;
      while tn<>nil do
      begin
        if staticscanner.reversescanners[i].isdone then
        begin
          tn.Text:='Thread '+inttostr(i+1)+' (Sleeping)';
          tn2:=tn.getFirstChild;
          tn2.text:='Sleeping';
          tn2:=tn2.getNextSibling;
          tn2.text:='Sleeping';
        end
        else
        begin
          tn.text:='Thread '+inttostr(i+1)+' (Active)';
          tn2:=tn.getFirstChild;

          begin
            s:='';
            for j:=0 to staticscanner.reversescanners[i].currentlevel-1 do
              s:=s+' '+inttohex(staticscanner.reversescanners[i].tempresults[j],8);


            tn2.text:='Current Level:'+inttostr(staticscanner.reversescanners[i].currentlevel)+' ('+s+')';
            tn2:=tn2.getNextSibling;
            tn2.text:='Looking for :'+inttohex(staticscanner.reversescanners[i].lookingformin,8)+'-'+inttohex(staticscanner.reversescanners[i].lookingformax,8);;
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
  modulelistlength: dword;
  i,j: integer;
  x: dword;
  temppchar: pchar;
  temppcharmaxlength: integer;

  col_baseaddress:TListColumn;
  col_pointsto: TListColumn;
  col_offsets: Array of TListColumn;
  tempmodulelist: tstringlist;
begin
  temppcharmaxlength:=256;
  getmem(temppchar, temppcharmaxlength);

  OpenedPointerfile.filename:=filename;
  OpenedPointerfile.pointerfile:=tfilestream.Create(Filename, fmopenRead or fmShareDenyNone);
  OpenedPointerfile.pointerfile.Read(modulelistlength,sizeof(modulelistlength)); //modulelistcount
  OpenedPointerfile.modulelist:=tstringlist.Create;


  tempmodulelist:=tstringlist.Create;
  symhandler.getModuleList(tempmodulelist);
  //sift through the list filling in the modulelist of the opened pointerfile


  for i:=0 to modulelistlength-1 do
  begin
    OpenedPointerfile.pointerfile.Read(x,sizeof(x));
    while x>temppcharmaxlength do
    begin
      temppcharmaxlength:=temppcharmaxlength*2;
      getmem(temppchar, temppcharmaxlength);
    end;

    OpenedPointerfile.pointerfile.Read(temppchar[0], x);
    temppchar[x]:=#0;

    j:=tempmodulelist.IndexOf(temppchar);
    if j<>-1 then
      OpenedPointerfile.modulelist.Addobject(temppchar, tempmodulelist.Objects[j])
    else
      OpenedPointerfile.modulelist.Add(temppchar);
  end;

  tempmodulelist.free;



  //modulelist has been loaded
  OpenedPointerfile.pointerfile.read(OpenedPointerfile.offsetlength, sizeof(OpenedPointerfile.offsetlength));
  OpenedPointerfile.StartPosition:=OpenedPointerfile.pointerfile.Position;

  setlength(tempoffset, OpenedPointerfile.offsetlength);

  listview1.Columns.Clear;

  col_baseaddress:=listview1.Columns.Add;
  col_baseaddress.Caption:='Base Address';
  col_baseaddress.Width:=130;
  col_baseaddress.MinWidth:=20;

  setlength(col_offsets, OpenedPointerfile.offsetlength);
  for i:=0 to OpenedPointerfile.offsetlength-1 do
  begin
    col_offsets[i]:=listview1.Columns.Add;
    col_offsets[i].Caption:='Offset '+inttostr(i);
    col_offsets[i].Width:=80;
    col_offsets[i].MinWidth:=10;

  end;

  col_pointsto:=listview1.Columns.Add;
  col_pointsto.Caption:='Points to:';
  col_pointsto.Width:=80;
  col_pointsto.MinWidth:=10;

  OpenedPointerfile.sizeOfEntry:=(12+OpenedPointerfile.offsetlength*4);

  OpenedPointerFile.TotalPointers:=(OpenedPointerfile.pointerfile.size-OpenedPointerfile.StartPosition) div OpenedPointerfile.sizeofentry;
  listview1.Items.Count:=OpenedPointerFile.TotalPointers;


  listview1.Align:=alClient;
  listview1.Visible:=true;


  Rescanmemory1.Enabled:=true;
end;

procedure Tfrmpointerscanner.Open1Click(Sender: TObject);
begin
  if opendialog1.Execute then
    OpenPointerfile(Opendialog1.filename);
end;


procedure TRescanWorker.flushresults;
begin
  tempfile.WriteBuffer(tempbuffer.Memory^,tempbuffer.Position);
  tempbuffer.Seek(0,sofrombeginning);
end;


procedure TRescanWorker.execute;
var origin: TFileStream;
    pointercache: pbytearray;  //holds a chunk of memory from the original file
    pointercached: pdwordarray;
    batchsize: integer;
    currentEntry: integer;
    i,j: integer;

    address,address2: dword;
    pa: PPointerAddress;
    x: dword;
    found: integer;
begin
  found:=0;
  pointercache:=nil;
  origin:=nil;
  tempfile:=nil;
  tempbuffer:=nil;
  try

    origin:=tfilestream.Create(OriginalFilename, fmOpenRead or fmShareDenyNone);
    tempfile:=tfilestream.Create(self.filename, fmCreate);
    tempbuffer:=TMemoryStream.Create;
    tempbuffer.SetSize(16*1024*1024);

    getmem(pointercache, self.OriginalFileEntrySize*256);

    evaluated:=0;
    origin.Position:=self.OriginalFileStartPosition+(self.startentry*self.OriginalFileEntrySize);

    while evaluated < self.EntriesToCheck do
    begin
      batchsize:=min(256, self.EntriesToCheck-evaluated);
      origin.ReadBuffer(pointercache[0], batchsize*OriginalFileEntrySize);
      currentEntry:=0;

      while currententry<batchsize do
      begin
        pointercached:=@pointercache[currententry*originalfileentrysize];
        //pointercached now points to the start of the current entry

        //format:
        //moduleindex
        //offset
        //numberofoffsets used
        //offset x0, x1, x2, x3....
        //--------------------------
        //pointercached[0]=moduleindex
        //pointercached[1]=offset
        //pointercached[2]=offsetcount
        //pointercached[3]=offset0
        //pointercached[4]=offset1
        //....

        address:=dword(self.modulelist.Objects[pointercached[0]]); //base address
        if address>0 then
        begin
          address:=address+pointercached[1]; //offset

          for i:=pointercached[2]-1 downto 0 do
          begin
            pa:=rescanhelper.findPointer(address);
            if pa=nil then
            begin
              if readprocessmemory(processhandle, pointer(address), @address2, 4, x) then
                pa:=rescanhelper.AddPointer(address, address2)
              else
                pa:=rescanhelper.AddPointer(address, 0);
            end;

            if pa.value>0 then
              address:=pa.value+pointercached[3+i]
            else
            begin
              address:=PointerAddressToFind-1;
              break; //invalid pointer
            end;
          end;

          if address=PointerAddressToFind then
          begin
            tempbuffer.Write(pointercached[0],OriginalFileEntrySize);
            if tempbuffer.Position>16*1024*1024 then flushresults;
          end;

        end;

        inc(evaluated);
        inc(currententry);
      end;

    end;

    flushresults;
  finally
    if origin<>nil then
      origin.Free;

    if tempfile<>nil then
      tempfile.Free;

    if tempbuffer<>nil then
      tempbuffer.free;

    if pointercache<>nil then
      freemem(pointercache);
  end;
end;

function TRescanpointers.ismatchtovalue(p: pointer): boolean;
begin
  case valuetype of
    vtDword: result:=pdword(p)^=valuescandword;
    vtSingle: result:=(psingle(p)^>=valuescansingle) and (psingle(p)^<valuescansinglemax);
    vtDouble: result:=(pdouble(p)^>=valuescandouble) and (pdouble(p)^<valuescandoublemax);
  end;
end;

procedure TRescanpointers.execute;
var offsetsize: dword;
    offsetlist: array of dword;
    tempbuf: array [0..7] of byte;

    i,j: integer;
    mi: TModuleInfo;

    stringlength: dword;
    ssize: dword;
    s: pchar;
    offset: dword;
    pointermatch: boolean;
    //---------------------

    TotalPointersToEvaluate: UINT64;
    PointersEvaluated: UINT64;

    rescanworkercount: integer;
    rescanworkers: array of TRescanWorker;
    blocksize: uint64;

    threadhandles: array of Thandle;
    result,x: tfilestream;

    modulecount: dword;
    rescanhelper: TRescanHelper;
    temp: dword;
begin
  progressbar.Min:=0;
  progressbar.Max:=100;
  progressbar.Position:=0;
  result:=nil;
  x:=nil;

  rescanhelper:=TRescanHelper.create;

  ownerform.resyncloadedmodulelist;

  //fill the modulelist with baseaddresses
  try


    //the modulelist now holds the baseaddresses (0 if otherwhise)
    TotalPointersToEvaluate:=ownerform.OpenedPointerfile.TotalPointers;


    //spawn all threads
    rescanworkercount:=GetCPUCount;
    if HasHyperthreading then rescanworkercount:=(rescanworkercount div 2)+1;

    rescanworkercount:=1;

    blocksize:=TotalPointersToEvaluate div rescanworkercount;


    setlength(rescanworkers, rescanworkercount);
    setlength(threadhandles, rescanworkercount);
    for i:=0 to rescanworkercount-1 do
    begin
      rescanworkers[i]:=TRescanWorker.Create(true);
      rescanworkers[i].OriginalFilename:=ownerform.OpenedPointerfile.filename;
      rescanworkers[i].OriginalFileEntrySize:=ownerform.OpenedPointerfile.sizeOfEntry;
      rescanworkers[i].OriginalFileStartPosition:=ownerform.OpenedPointerfile.StartPosition;
      rescanworkers[i].offsetlength:=ownerform.OpenedPointerfile.offsetlength;
      rescanworkers[i].modulelist:=ownerform.OpenedPointerfile.modulelist;
      rescanworkers[i].PointerAddressToFind:=self.address;
      rescanworkers[i].rescanhelper:=rescanhelper;

      rescanworkers[i].filename:=self.filename+'.'+inttostr(rescanworkers[i].ThreadID);

      rescanworkers[i].startEntry:=blocksize*i;
      rescanworkers[i].entriestocheck:=blocksize;
      if i=rescanworkercount-1 then
        rescanworkers[i].entriestocheck:=TotalPointersToEvaluate-rescanworkers[i].startEntry; //to the end

      threadhandles[i]:=rescanworkers[i].Handle;
      rescanworkers[i].Resume;
    end;


    result:=TFileStream.Create(filename,fmCreate);
    //write header

    modulecount:=ownerform.OpenedPointerfile.modulelist.count;
    result.Write(modulecount, sizeof(modulecount));

    for i:=0 to modulecount-1 do
    begin
      temp:=length(ownerform.OpenedPointerfile.modulelist[i]);
      result.Write(temp,sizeof(temp));
      result.Write(ownerform.OpenedPointerfile.modulelist[i][1],temp);
    end;

    result.Write(ownerform.OpenedPointerfile.offsetlength, sizeof(ownerform.OpenedPointerfile.offsetlength));

    //loop:

    while WaitForMultipleObjects(rescanworkercount, @threadhandles[0], true, 1000) = WAIT_TIMEOUT do      //wait
    begin
      //query all threads the number of pointers they have evaluated
      PointersEvaluated:=0;
      for i:=0 to rescanworkercount-1 do
        inc(PointersEvaluated,rescanworkers[i].evaluated);

      progressbar.Position:=PointersEvaluated div (TotalPointersToEvaluate div 100);
    end;
    //no timeout, so finished or crashed


    //append all results into one new file
    for i:=0 to rescanworkercount-1 do
    begin
      rescanworkers[i].WaitFor; //just to be sure
      x:=tfilestream.Create(rescanworkers[i].filename,fmopenread);
      result.CopyFrom(x,0);
      x.free;
      deletefile(rescanworkers[i].filename);

      rescanworkers[i].Free;
    end;
    result.Free;






  finally
    progressbar.Position:=0;
    postmessage(ownerform.Handle,rescan_done,0,0);
    if rescanhelper<>nil then
      freeandnil(rescanhelper);    
  end;

end;

procedure Tfrmpointerscanner.Rescanmemory1Click(Sender: TObject);
var address: dword;
    saddress: string;
    FloatSettings: TFormatSettings;
    floataccuracy: integer;
    i: integer;
begin
  if savedialog1.Execute then
  begin
    GetLocaleFormatSettings(GetThreadLocale, FloatSettings);
    saddress:='';

    rescan:=trescanpointers.create(true);
    rescan.ownerform:=self;
    rescan.progressbar:=progressbar1;
    rescan.filename:=savedialog1.filename;


    try

      with TFrmRescanPointer.Create(self) do
      begin
        try
          if showmodal=mrok then
          begin
            Rescanmemory1.Enabled:=false;
            new1.Enabled:=false;

            if rbFindAddress.Checked then
            begin
              address:=strtoint('$'+edtAddress.Text);

              //rescan the pointerlist

              rescan.address:=address;
              rescan.forvalue:=false;

            end
            else
            begin

              //if values, check what type of value
              floataccuracy:=pos(FloatSettings.DecimalSeparator,frmpointerscannersettings.edtAddress.Text);
              if floataccuracy>0 then
                floataccuracy:=length(frmpointerscannersettings.edtAddress.Text)-floataccuracy;

              case cbValueType.ItemIndex of
                0:
                begin
                  rescan.valuetype:=vtDword;
                  val(edtAddress.Text, rescan.valuescandword, i);
                  if i>0 then raise exception.Create(edtAddress.Text+' is not a valid 4 byte value');
                end;

                1:
                begin
                  rescan.valuetype:=vtSingle;
                  val(edtAddress.Text, rescan.valuescansingle, i);
                  if i>0 then raise exception.Create(edtAddress.Text+' is not a valid floating point value');
                  rescan.valuescansingleMax:=rescan.valuescansingle+(1/(power(10,floataccuracy)));
                end;

                2:
                begin
                  rescan.valuetype:=vtDouble;
                  val(edtAddress.Text, rescan.valuescandouble, i);
                  if i>0 then raise exception.Create(edtAddress.Text+' is not a valid double value');
                  rescan.valuescandoubleMax:=rescan.valuescandouble+(1/(power(10,floataccuracy)));
                end;
              end;



              rescan.forvalue:=true;

            end;
            rescan.resume;
          end;


        finally
          free;
        end;
      end;


    except
      on e: exception do
      begin
        Rescanmemory1.Enabled:=true;
        new1.Enabled:=true;


        freeandnil(rescan);
        raise e;
      end;

    end;
  end;

end;

procedure tfrmpointerscanner.rescandone(var message: tmessage);
{
The rescan is done. rescan.oldpointerlist (the current pointerlist) can be deleted
and the new pointerlist becomes the current pointerlist
}
begin
  if rescan<>nil then
    freeandnil(rescan);

  doneui;
    
  Rescanmemory1.Enabled:=true;
  new1.Enabled:=true;
end;

procedure Tfrmpointerscanner.btnStopScanClick(Sender: TObject);
begin
  if staticscanner<>nil then
  begin
    staticscanner.Terminate;
    staticscanner.WaitFor;
  end;
end;

procedure Tfrmpointerscanner.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Staticscanner<>nil then
  begin
    Staticscanner.FreeOnTerminate:=true; //I don't feel like waiting for it to clean up
    Staticscanner.Terminate;
  end;
  
  action:=cafree; //on close free itself
end;

procedure Tfrmpointerscanner.openscanner(var message: tmessage);
begin
  if frmpointerscannersettings=nil then
    frmpointerscannersettings:=tfrmpointerscannersettings.create(nil);

  frmpointerscannersettings.edtAddress.text:=inttohex(message.WParam,8);
  Method3Fastspeedandaveragememoryusage1.Click;
end;


procedure Tfrmpointerscanner.New1Click(Sender: TObject);
var i: integer;
begin
  btnStopScan.click;
  if staticscanner<>nil then
    freeandnil(staticscanner);

 
  pgcPScandata.Visible:=false;
  panel1.Caption:='';
  open1.Enabled:=true;
  new1.enabled:=true;
  rescanmemory1.Enabled:=false;
end;



procedure Tfrmpointerscanner.FormCreate(Sender: TObject);
begin
  tsPSReverse.TabVisible:=false;

  {$ifdef injectedpscan}
  caption:='CE Injected Pointerscan';
  {$endif}

end;

procedure Tfrmpointerscanner.ListView1Data(Sender: TObject;
  Item: TListItem);
var i: integer;
    offset: dword;
    actualoffsetcount: integer;

    address,address2: dword;
    x: dword;
begin
  if OpenedPointerfile.pointerfile<>nil then
  begin
    OpenedPointerfile.pointerfile.Position:=OpenedPointerfile.StartPosition+item.Index*OpenedPointerfile.sizeofentry;
    OpenedPointerfile.pointerfile.Read(i,sizeof(i));
    OpenedPointerfile.pointerfile.read(offset,sizeof(offset));
    item.Caption:=OpenedPointerfile.modulelist[i]+'+'+inttohex(offset,1);

    OpenedPointerfile.pointerfile.Read(actualoffsetcount,sizeof(actualoffsetcount));

    address:=dword(OpenedPointerfile.modulelist.Objects[i])+offset;

    {
    results.WriteBuffer(staticdata.offset,sizeof(staticdata.offset));
    i:=level+1; //store many offsets are actually used (since all are saved)
    results.WriteBuffer(i,sizeof(i));
    results.WriteBuffer(tempresults[0], maxlevel*sizeof(tempresults[0]) );
    }
    OpenedPointerfile.pointerfile.Read(tempoffset[0],sizeof(tempoffset[0])*actualoffsetcount);

    for i:=actualoffsetcount-1 downto 0 do
    begin
      item.SubItems.Add(inttohex(tempoffset[i],1))
    end;


    for i:=actualoffsetcount to OpenedPointerfile.offsetlength-1 do
      item.SubItems.Add('');

    for i:=actualoffsetcount-1 downto 0 do
    begin
      if readprocessmemory(processhandle, pointer(address),@address2,4,x) then
        address:=address2+tempoffset[i]
      else
      begin
        item.SubItems.Add('-');
        exit;
      end;
    end;

    item.SubItems.Add(inttohex(address,8));
  end;
end;

procedure Tfrmpointerscanner.resyncloadedmodulelist;
var
  tempmodulelist: TStringList;
  i,j: integer;
begin
  tempmodulelist:=tstringlist.Create;
  try
    symhandler.getModuleList(tempmodulelist);
    //sift through the list filling in the modulelist of the opened pointerfile


    for i:=0 to OpenedPointerfile.modulelist.Count-1 do
    begin
      j:=tempmodulelist.IndexOf(OpenedPointerfile.modulelist[i]);
      if j<>-1 then
        OpenedPointerfile.modulelist.Objects[i]:=tempmodulelist.Objects[j];
    end;
  finally
    tempmodulelist.free;
  end;
end;

procedure Tfrmpointerscanner.Resyncmodulelist1Click(Sender: TObject);
begin
  resyncloadedmodulelist;


end;

end.


