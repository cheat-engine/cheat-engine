unit symbolsync;

{$mode ObjFPC}{$H+}

//xml format:
//<symbols>
//  <process pid:int name: string>
//    <symbol name: string address: integer donotsave: boolean size: integer></symbol>
//    <symbol name: string address: integer donotsave: boolean size: integer></symbol>
//    ....
//    <symbollist name: string uniquename: string>
//      <module name: string address: integer length: integer></module>
//      <symbol name: string address: integer length: integer modulename: string></symbol>
//      <symbol name: string address: integer length: integer></symbol>
//      ...
//    </symbollist>
//  </process>
//  ...
//</symbols>


interface

uses
  jwawindows, windows, Classes, SysUtils, XMLRead, XMLWrite, DOM, NewKernelHandler;

function SyncSymbolsNow(retrieveOnly: boolean=false):boolean; //collect all non-table saved symbols and store them in the symbol database file. Check if the last sync time is higher than the previous one, and if so, load added entries in the (synchronized) symbollist , else write the current symbols to the database and delete older entries that are not there anymore

procedure EnableSymbolSyncThread;
procedure DisableSymbolSyncThread;

implementation

uses LazFileUtils, ProcessHandlerUnit, FileUtil, Globals, mainunit2,
  symbolhandler, symbolhandlerstructs, SymbolListHandler, ProcessList,
  maps, syncobjs, forms;

type
  TSymbolSyncThread=class(TThread)
  private
    canceled: TEvent;
  protected
    procedure TerminatedSet; override;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=DefaultStackSize);
  end;

var
  lastsync: longint;
  symbolSyncThread: TSymbolSyncThread;

constructor TSymbolSyncThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt=DefaultStackSize);
begin
  canceled:=TEvent.Create(nil,false,false,'');
  inherited create(CreateSuspended, stacksize);
end;

procedure TSymbolSyncThread.Execute;
begin
  try
    {$ifdef THREADNAMESUPPORT}
    SetThreadDebugName(GetCurrentThreadId, 'Symbol synchronize thread');
    {$endif}
    Priority:=tpIdle;
    while not terminated do
    begin
      if symsync_Interval=0 then
        symsync_Interval:=1;

      if canceled.WaitFor(symsync_Interval*1000)=wrSignaled then break;
      if (lastsync=0) or (processid=0) then continue;

      if SyncSymbols then
      begin
        if symhandler<>nil then
          SyncSymbolsNow(false);
      end;
    end;
  except
    on e: exception do
      if assigned(application.OnException) then
        application.OnException(self, e);
  end;
end;

procedure TSymbolSyncThread.TerminatedSet;
begin
  inherited terminatedset;
  canceled.SetEvent;
end;

procedure EnableSymbolSyncThread;
begin
  DisableSymbolSyncThread; //restart in case the old timer was taking too long
  symbolSyncThread:=TSymbolSyncThread.create(false);
end;

procedure DisableSymbolSyncThread;
begin
  if symbolSyncThread<>nil then
    freeandnil(symbolSyncThread);
end;

function SyncSymbolsFromNode(n: TDOMNode; HasBeenUpdatedSinceLastSync: boolean): boolean;
//HasBeenUpdatedSinceLastSync is true when another CE instance has updated the symbols


var
  userdefinedSymbols: TUserdefinedSymbolsList;
  symbollists: TSymbolListHandlerArray;

  s,s2: TDOMNode;
  nname, nuniquename, naddress, ndonotsave, nis64bit, nsize, nmodulename: TDOMNode;
  i,j: integer;

  nsl: TDOMNode;

  symbolname, modulename: string;
  a: ptruint;
  size: integer;
  is64bit: boolean;
  news: TUserdefinedsymbol;

  address: ptruint;

  newsl: TSymbolListHandler;
  e,e2: TDOMElement;
begin
  result:=false;
  userdefinedSymbols:=[];


 // if HasBeenUpdatedSinceLastSync and (n.ChildNodes.Count>0) then
 // begin
    //read symbols and check which ones are not in and newer than last sync
    for i:=0 to n.ChildNodes.Count-1 do
    begin
      s:=n.ChildNodes[i];
      if s.NodeName='symbol' then
      begin
        nname:=s.Attributes.GetNamedItem('name');
        naddress:=s.Attributes.GetNamedItem('address');
        ndonotsave:=s.Attributes.GetNamedItem('donotsave');
        nsize:=s.Attributes.GetNamedItem('size');

        if (nname=nil) or (naddress=nil) then continue;


        news.symbolname:=nname.NodeValue;
        news.addressstring:=naddress.NodeValue;

        try
          news.address:=StrToInt64(naddress.NodeValue);
        except
          continue;
        end;
        news.donotsave:=false;
        news.allocsize:=0;
        news.processid:=0;
        news.addressstring:=inttohex(news.address,1);

        if ndonotsave<>nil then
          news.donotsave:=ndonotsave.NodeValue='1';

        if nsize<>nil then
        begin
          try
            news.allocsize:=StrToInt(nsize.NodeValue);
            news.processid:=processid;
          except
          end;
        end;

        setlength(userdefinedSymbols,length(userdefinedSymbols)+1);
        userdefinedSymbols[length(userdefinedSymbols)-1]:=news;
      end
      else
      if s.NodeName='symbollist' then
      begin
        nname:=s.Attributes.GetNamedItem('name');
        nuniquename:=s.Attributes.GetNamedItem('uniquename');

        if (nname=nil) or (nuniquename=nil) then continue;

        newsl:=TSymbolListHandler.create(nname.NodeValue, nuniquename.NodeValue);

        if s is TDOMElement then
          newsl.loadFromXML(TDOMElement(s));



        setlength(symbollists,length(symbollists)+1);
        symbollists[length(symbollists)-1]:=newsl;
      end;
    end;

    //all symbols are done reading. Apply the status to the current symbols
    if HasBeenUpdatedSinceLastSync then
      result:=symhandler.SyncSymbols(userdefinedSymbols, symbollists, symsync_DontDeleteSymbolsWhenSynchronizing, true)
    else
      result:=symhandler.SyncSymbols(userdefinedSymbols, symbollists, true, false);

 // end
 // else
//    result:=symhandler.SyncSymbols(userdefinedSymbols, symbollists, true, false); //gets the symbols, doesn't write anything



  if result then //changes where made
  begin

    //reading done, delete the old entries and prepare new entries based on the current state
    //  n.ChildNodes.free;
    while n.ChildNodes.Count>0 do
      n.ChildNodes[0].Free;


    //write the userdefined symbols and symbollists to the xml file

    for i:=0 to length(userdefinedsymbols)-1 do
    begin
      e:=TDOMElement(n.AppendChild(n.OwnerDocument.CreateElement('symbol')));


      e.AttribStrings['name']:=userdefinedsymbols[i].symbolname;
      e.AttribStrings['address']:=userdefinedsymbols[i].address.ToString;
      if userdefinedsymbols[i].doNotSave then
        e.AttribStrings['donotsave']:='1';

      if userdefinedsymbols[i].allocsize<>0 then
        e.AttribStrings['size']:=userdefinedsymbols[i].allocsize.ToString;
    end;

    for i:=0 to length(symbollists)-1 do
    begin
      e:=TDOMElement(n.AppendChild(n.OwnerDocument.CreateElement('symbollist')));
      e.AttribStrings['name']:=symbollists[i].name;
      e.AttribStrings['uniquename']:=symbollists[i].internalname;

      symbollists[i].saveToXML(e);
    end;
  end;


  for i:=0 to length(symbollists)-1 do
    symbollists[i].Free;
end;

function SyncSymbolsNow(retrieveOnly: boolean=false):boolean;  //can be from any thread, or process
var
  symbolpath: string;
  symbolfilepath: string;

  fs: TFilestream;
  trycount: integer;

  d: TXMLDocument=nil;
  n,symbolsyncnode, processnode: TDomElement;
  usedtempdir: string;

  HasBeenUpdatedSinceLastSync: boolean;

  ths: THandle;
  pe: TProcessEntry32;
  pidlookup: TMap=nil;
  pname: string;
  t: pchar;
  mi: TMapIterator;

  i: integer;
  nodelist: TDOMNodeList;
  pidattrib: TDOMNode;
  pidstring: string;

  nameattrib: TDOMNode;
  namestring: string;
  pid: qword;
  deleteEntry: boolean;

  symfileage: longint;

  updated: boolean;
  madeChanges: boolean=false;
begin

  if (length(trim(tempdiralternative))>2) and dontusetempdir then
    usedtempdir:=trim(tempdiralternative)
  else
    usedtempdir:=GetTempDir;

  symbolpath:=usedtempdir+strCheatEngine+' Symbols'+pathdelim;
  ForceDirectory(symbolpath);

  trycount:=0;

  symbolfilepath:=symbolpath+'cesymbols.xml';
  while trycount<10 do
  begin
    try
      fs:=TFileStream.Create(symbolfilepath+'.lock', fmCreate, fmShareExclusive);
      fs.WriteAnsiString(GetCurrentProcessId.ToString+'.'+GetCurrentThreadId.ToString);
      break;
    except
      //try again
      sleep(10+random(100));
      inc(trycount);
    end;
  end;

  if trycount>=10 then
  begin
    outputdebugstring('symbolsync acquire lock failed');
    exit(false);
  end;

  HasBeenUpdatedSinceLastSync:=false;

  d:=nil;
  try
    symfileage:=FileAge(symbolfilepath);
    XMLRead.ReadXMLFile(d,symbolfilepath);
    HasBeenUpdatedSinceLastSync:=(lastsync<>0) and (symfileage<>-1) and (symfileage>lastsync);
  except
    DeleteFileUTF8(symbolfilepath);

    d:=TXMLDocument.Create;
    symbolsyncnode:=TDOMElement(d.CreateElement('symbolsync'));
    d.AppendChild(symbolsyncnode);
  end;

  if retrieveonly then //only set when on openprocess
  begin
    HasBeenUpdatedSinceLastSync:=true;

    if symsync_ClearSymbolListWhenOpeningADifferentProcess then
      symhandler.DeleteAllUserdefinedSymbols;
  end;


  try


    //delete process entries that do not exist anymore (or wrong pid)
    pidlookup:=tmap.Create(itu8,sizeof(pchar));
    ths:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
    if (ths<>0) and (ths<>INVALID_HANDLE_VALUE) then
    begin

      if Process32First(ths, pe) then
      repeat
        if pe.th32ProcessID=0 then continue;

        pname:=pchar(@pe.szExeFile[0]);
        pname:=extractfilename(pname);

        t:=strnew(pchar(pname));
        pid:=pe.th32ProcessID;
        pidlookup.Add(pid, t);
      until Process32Next(ths,pe)=false;

      closehandle(ths);
    end;

    updated:=false;
    symbolsyncnode:=TDomElement(d.FindNode('symbolsync'));
    if symbolsyncnode<>nil then
      nodelist:=symbolsyncnode.ChildNodes
    else
      nodelist:=nil;

    i:=0;
    if nodelist<>nil then
    begin

      while i<nodelist.Count do
      begin

        pidattrib:=nodelist[i].Attributes.GetNamedItem('pid');
        nameattrib:=nodelist[i].Attributes.GetNamedItem('name');
        if (pidattrib<>nil) and (nameattrib<>nil) then
        begin
          pidstring:=pidattrib.NodeValue;
          namestring:=nameattrib.NodeValue;
          if pidstring<>'' then
          begin
            pid:=pidstring.ToInt64;
            deleteEntry:=true;
            if pidlookup.GetData(pid,t) then
            begin
              if namestring=t then
                deleteEntry:=false;
            end;

            if deleteEntry then
            begin
              nodelist[i].Free;
              madeChanges:=true;
              continue;
            end
            else
            begin
              if pid=processid then
              begin
                if SyncSymbolsFromNode(nodelist[i], HasBeenUpdatedSinceLastSync) then
                  madeChanges:=true;
                updated:=true;
              end;
            end;
          end;


        end;
        inc(i);
      end;
    end;

    if not updated then
    begin
      //create a new node
      pid:=processid;

      if pidlookup.GetData(pid,t) then
      begin
        processnode:=TDOMElement(symbolsyncnode.AppendChild(d.CreateElement('process')));
        processnode.AttribStrings['pid']:=processid.ToString;
        processnode.AttribStrings['name']:=t;

        if SyncSymbolsFromNode(processnode, false) then
          madeChanges:=true;
      end;
    end;

    if (not retrieveonly) and madeChanges then
    begin
      try
        WriteXMLFile(d,symbolfilepath);
      except
        on e: exception do
        begin
          outputdebugstring('symbolsync failed write:'+e.message);
          exit;
        end;
      end;
    end;

    lastsync:=FileAge(symbolfilepath);


  finally
    fs.Size:=0;
    fs.free;

    DeleteFile(symbolfilepath+'.lock');

    if pidlookup<>nil then
    begin
      mi:=TMapIterator.Create(pidlookup);
      mi.First;
      while not mi.EOM do
      begin
        t:=nil;
        mi.GetData(t);
        if t<>nil then
          StrDispose(t);

        mi.Next;
      end;

      mi.free;
      pidlookup.Free;
    end;

    if d<>nil then
      freeandnil(d);

  end;

  if madeChanges then
    symhandler.refreshGUI;
end;

finalization
  if symbolSyncThread<>nil then
    freeandnil(symbolSyncThread);

end.

