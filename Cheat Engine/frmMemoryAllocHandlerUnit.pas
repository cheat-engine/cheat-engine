unit frmMemoryAllocHandlerUnit;

{
ZwVirtualAllocEx
}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, symbolhandler, cefuncproc,newkernelhandler, autoassembler,
  ExtCtrls, ComCtrls, stacktrace2, math, Menus;


type TmemoryAllocevent=class
  private
  public
    handle: dword;
    baseaddress: dword;
    allocationType: dword;
    protect: dword;
    size: dword;
    esp: dword;

    _free: boolean;
    FreeType: dword;
    FreeSize: dword;

    stack: array [0..4095] of byte;
    stacksize: dword;
    procedure assign(o: TMemoryAllocEvent);
end;
  
type TAllocWatcher=class(TThread)
  private
    HasSetupDataEvent: THandle;
    CEHasHandledItEvent: THandle;
    eventtypeaddress: dword;
    CreateEventDataAddress: dword;
    FreeEventDataAddress: dword;
    o: TMemoryAllocEvent;
    errorcount: dword; //debug variable
    procedure allocevent;
    procedure freeEvent;
    procedure addObject;
  public
    procedure execute; override;  
    constructor create(suspended: boolean; HasSetupDataEvent: THandle; CEHasHandledItEvent: THandle; eventtypeaddress: THandle; CreateEventDataAddress: THandle; FreeEventDataAddress: THandle );
end;

type
  TfrmMemoryAllocHandler = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblHandle: TLabel;
    lblBaseAddress: TLabel;
    lblAllocationType: TLabel;
    lblProtect: TLabel;
    lblSize: TLabel;
    ListView1: TListView;
    rbStacktraceAll: TRadioButton;
    rbStackTraceModules: TRadioButton;
    rdNonsystemModulesOnly: TRadioButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Saveselectedfingerprint1: TMenuItem;
    Searchfingerprint1: TMenuItem;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    lbSearchResults: TListBox;
    PopupMenu1: TPopupMenu;
    Closesearchresults1: TMenuItem;
    Search1: TMenuItem;
    Findtext1: TMenuItem;
    Searchagain1: TMenuItem;
    FindDialog1: TFindDialog;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure rbStackTraceModulesClick(Sender: TObject);
    procedure Saveselectedfingerprint1Click(Sender: TObject);
    procedure Searchfingerprint1Click(Sender: TObject);
    procedure Closesearchresults1Click(Sender: TObject);
    procedure Findtext1Click(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure Searchagain1Click(Sender: TObject);
  private
    { Private declarations }
    HasSetupDataEvent: THandle;
    CEHasHandledItEvent: THandle;
    watcher: TAllocWatcher;
    eventtypeaddress: dword;
    CreateEventDataAddress: dword;
    FreeEventDataAddress: dword;
  public
    { Public declarations }
    hookedprocessid: dword;
  end;

var
  frmMemoryAllocHandler: TfrmMemoryAllocHandler;

implementation

{$R *.dfm}

uses frmautoinjectunit, memorybrowserformunit, frmMemoryAllocHandlerSearchFingerprintunit;



type TCreateEvent=record
    handle: dword;
    baseaddress: dword;
    allocationType: dword;
    protect: dword;
    size: dword;
    esp: dword;
  end;


type TFreeEvent=record
  handle: dword;
  baseaddress: dword;
  size: dword;
  FreeType: dword;
end;

procedure TmemoryAllocevent.assign(o: TMemoryAllocEvent);
var i: integer;
begin
  self.handle:=o.handle;
  self.baseaddress:=o.baseaddress;
  self.allocationType:=o.allocationType;
  self.protect:=o.protect;
  self.size:=o.size;
  self.esp:=o.esp;
  self._free:=o._free;
  self.FreeType:=o.FreeType;
  self.FreeSize:=o.FreeSize;

  for i:=0 to o.stacksize-1 do
    self.stack[i]:=o.stack[i];

  self.stacksize:=o.stacksize;
end;

procedure TAllocWatcher.addObject;
var
  first,last: integer;
  i,j,k,l: integer;
  temp: dword;
  s: string;
  insert: boolean;
begin
  if o._free then
  begin
    first:=0;
    last:=frmMemoryAllocHandler.listbox1.Items.Count-1;

    k:=0;

    repeat

      j:=first+((last-first) div 2);
      if k=j then
      begin
        if k=first then
        begin
          first:=last;
          j:=last;
        end;

        if k=last then
        begin
          last:=first;
          j:=last;
        end;
      end;

      k:=j;
      if j<0 then break;

      temp:=TmemoryAllocevent(frmMemoryAllocHandler.listbox1.Items.Objects[j]).baseaddress;

      if temp=o.baseaddress then
      begin
        //already in the list
        //update this line and exit
        TmemoryAllocevent(frmMemoryAllocHandler.listbox1.Items.Objects[j]).Free;
        frmMemoryAllocHandler.listbox1.Items.Delete(j);
        exit;
      end
      else
      begin
        if temp<o.baseaddress then
          first:=j //not far enough
        else
          last:=j; //too far
      end;

    until first<=last;
  end
  else
  begin

    s:=inttohex(o.baseaddress,8)+'-'+inttohex(o.baseaddress+o.size,8);

    //find and replace, else insert (sorted by address)
    first:=0;
    last:=frmMemoryAllocHandler.listbox1.Items.Count-1;

    k:=0;

    repeat

      j:=first+((last-first) div 2);
      if k=j then
      begin
        if k=first then
        begin
          first:=last;
          j:=last;
        end;

        if k=last then
        begin
          last:=first;
          j:=last;
        end;
      end;

      k:=j;
      if j<0 then break;

      temp:=TmemoryAllocevent(frmMemoryAllocHandler.listbox1.Items.Objects[j]).baseaddress;

      if temp=o.baseaddress then
      begin
        //already in the list
        //update this line and exit
        frmMemoryAllocHandler.listbox1.Items[j]:=s;
        TmemoryAllocevent(frmMemoryAllocHandler.listbox1.Items.Objects[j]).Free;

        frmMemoryAllocHandler.listbox1.Items.Objects[j]:=o;

        exit;
      end
      else
      begin
        if temp<o.baseaddress then
          first:=j //not far enough
        else
          last:=j; //too far
      end;

    until first<=last;


    //still here but not found


    j:=max(0,min(max(0,min(first-1,last+1)),frmMemoryAllocHandler.listbox1.Items.Count-1));
    k:=min(max(0,max(first-1,last+1)),frmMemoryAllocHandler.listbox1.Items.Count-1);

    //search in the range of j to k
    insert:=false;

    l:=0;
    for i:=j to k do
    begin
      temp:=TmemoryAllocevent(frmMemoryAllocHandler.listbox1.Items.Objects[i]).baseaddress;
      if temp>o.baseaddress then
      begin
        insert:=true;
        l:=i;
        break;
      end;
    end;

    if insert then
      frmMemoryAllocHandler.listbox1.Items.InsertObject(l,s,O)
    else
      frmMemoryAllocHandler.listbox1.Items.AddObject(s,o);
      
   { if j=-1 then
     then
      frmMemoryAllocHandler.listbox1.Items.InsertObject(k,s,O)
    else
      frmMemoryAllocHandler.listbox1.Items.AddObject(s,o);
    }


//    TmemoryAllocevent(frmMemoryAllocHandler.listbox1.Items.

  end;
end;

procedure TAllocWatcher.allocEvent;
var x: TCreateEvent;
    br: dword;
    s: integer;
begin
  if readprocessmemory(processhandle, pointer(CreateEventDataAddress),@x,sizeof(x), br) then
  begin
    o:=TmemoryAllocevent.create;
    o.handle:=x.handle;
    o.baseaddress:=x.baseaddress;
    o.allocationType:=x.allocationType;
    o.protect:=x.protect;
    o.size:=x.size;
    o.esp:=x.esp;
    if not ReadProcessMemory(processhandle,pointer(o.esp),@o.stack[0],4096,o.stacksize) then
    begin
      //probably couldn't read because of pageerror
      //read the remaining of the page
      s:=o.esp mod 4096;
      ReadProcessMemory(processhandle,pointer(o.esp),@o.stack[0],s,o.stacksize);
    end;


    setevent(CEHasHandledItEvent);

    synchronize(addObject);
  end else
  begin
    setevent(CEHasHandledItEvent);
    outputdebugstring(format('TAllocWatcher.allocEvent: Failed reading the CreateEventDataAddress (%x)',[CreateEventDataAddress]));
  end;
end;

procedure TAllocWatcher.freeEvent;
var x: TFreeEvent;
    br: dword;
begin
  if readprocessmemory(processhandle, pointer(FreeEventDataAddress),@x,sizeof(x), br) then
  begin
    o:=TmemoryAllocevent.create;
    o._free:=true;
    o.handle:=x.handle;
    o.baseaddress:=x.baseaddress;
    o.freetype:=x.FreeType;
    o.FreeSize:=x.size;
    setevent(CEHasHandledItEvent);
    synchronize(addObject);
    //listbox1.Items.AddObject('Free:'+inttohex(x.baseaddress,8)+' ('+inttostr(x.size)+')',O);
  end else
  begin
    outputdebugstring(format('TAllocWatcher.freeEvent: Failed reading the FreeEventDataAddress (%x)',[CreateEventDataAddress]));
    setevent(CEHasHandledItEvent);
  end;
end;

procedure TAllocWatcher.execute;
var x: dword;
    eventtype: dword;
begin
  while not terminated do
  begin
    try
      if waitforsingleobject(HasSetupDataEvent,500)=WAIT_OBJECT_0 then
      begin
        if readprocessmemory(processhandle, pointer(eventtypeaddress),@eventtype,sizeof(eventtype),x) then
        begin
          if eventtype=0 then
            allocevent
          else
            freeEvent;
        end;

      end;
    except
      inc(errorcount);
      OutputDebugString('Error in TAllocWatcher.execute');
    end;
  end;
end;

constructor TAllocWatcher.create(suspended: boolean; HasSetupDataEvent: THandle; CEHasHandledItEvent: THandle; eventtypeaddress: THandle; CreateEventDataAddress: THandle; FreeEventDataAddress: THandle );
begin
  self.HasSetupDataEvent:=HasSetupDataEvent;
  self.CeHasHandledItEvent:=CeHasHandledItEvent;

  self.eventtypeaddress:=eventtypeaddress;
  self.CreateEventDataAddress:=CreateEventDataAddress;
  self.FreeEventDataAddress:=FreeEventDataAddress;

  inherited create(suspended);
end;



procedure TfrmMemoryAllocHandler.FormCreate(Sender: TObject);
var injectionscript: TStringlist;
var x,y: THandle;
begin
  injectionscript:=tstringlist.Create;
  try
    //inject allochook.dll
    injectdll(CheatEngineDir+'allochook.dll');
    symhandler.reinitialize;


    HasSetupDataEvent:=CreateEvent(nil, false, false, nil);
    CEHasHandledItEvent:=CreateEvent(nil,false,false,nil);

    x:=0;
    if not DuplicateHandle(GetCurrentProcess, HasSetupDataEvent, processhandle, @x, 0, false, DUPLICATE_SAME_ACCESS	) then
      raise exception.Create('Event1 failure:'+inttostr(getlasterror));

    y:=0;
    if not DuplicateHandle(GetCurrentProcess, CEHasHandledItEvent, processhandle, @y, 0, false, DUPLICATE_SAME_ACCESS	) then
      raise exception.Create('Event2 failure:'+inttostr(getlasterror));

    //set event handles
    injectionscript.Add('HasSetupDataEvent:');
    injectionscript.Add('DD '+inttohex(x,8));
    injectionscript.Add('CEHasHandledItEvent:');
    injectionscript.Add('DD '+inttohex(y,8));

    //hook apis
    generateAPIHookScript(injectionscript,'NtAllocateVirtualMemory','CeAllocateVirtualMemory', 'NtAllocateVirtualMemoryOrig','0');
    generateAPIHookScript(injectionscript,'NtFreeVirtualMemory','CeFreeVirtualMemory', 'NtFreeVirtualMemoryOrig','1');

    if not autoassemble(injectionscript,false) then raise exception.Create('Failure hooking apis');


    eventtypeaddress:=symhandler.getAddressFromName('eventtype');
    CreateEventDataAddress:=symhandler.getAddressFromName('CreateEventData');
    FreeEventDataAddress:=symhandler.getAddressFromName('FreeEventData');

    //everything configured successful, start thread that watches for HasSetupDataEvent events
    watcher:=TAllocWatcher.create(false, HasSetupDataEvent, CEHasHandledItEvent, eventtypeaddress, CreateEventDataAddress, FreeEventDataAddress);
  finally
    injectionscript.Free;
  end;
  hookedprocessid:=processid;
end;

procedure TfrmMemoryAllocHandler.ListBox1Click(Sender: TObject);
var
  o: TMemoryAllocEvent;
  trace: TStringList;
  i: integer;
  address,bytes,details: string;
  li: tlistitem;
  oldindex: integer;
begin
  if (sender as tlistbox).ItemIndex<>-1 then
  begin
    if not panel1.Visible then panel1.visible:=true;
    o:=TMemoryAllocEvent((sender as tlistbox).Items.Objects[(sender as tlistbox).ItemIndex]);
    lblhandle.caption:=inttohex(o.handle,1);
    lblBaseAddress.Caption:=inttohex(o.baseaddress,8);

    if o._free then
    begin
      label3.Caption:='Free Type:';
      lblAllocationType.caption:=freetypetostring(o.FreeType);

      label4.Caption:='Size:';
      lblProtect.Caption:=inttostr(o.size);;
      label6.Visible:=false;
      listview1.Visible:=false;
    end
    else
    begin
      label3.Caption:='Allocation Type:';
      lblAllocationType.Caption:=allocationtypetostring(o.allocationType);

      label4.Caption:='Protect:';
      lblProtect.Caption:=allocationprotecttostring(o.protect);
      lblSize.Caption:=inttostr(o.size);
      label6.Visible:=true;

      listview1.Items.BeginUpdate;
      trace:=TStringlist.Create;
      try
        oldindex:=listview1.ItemIndex;

        listview1.Items.Clear;
        ce_stacktrace(o.esp,0,0,@o.stack[0],o.stacksize, trace, true,rbStackTraceModules.checked or rdNonsystemModulesOnly.checked,rdNonsystemModulesOnly.checked);
        for i:=0 to trace.Count-1 do
        begin
          seperatestacktraceline(trace[i],address,bytes,details);
          li:=listview1.Items.Add;
          li.Caption:=address;
          li.SubItems.Add(bytes);
          li.SubItems.Add(details);
        end;


      finally
        trace.free;
      end;


      if (oldindex<>-1) and (listview1.Items.Count>oldindex) then
      begin
        listview1.ItemIndex:=oldindex;
        listview1.Selected.Focused:=true;
        if oldindex<listview1.Items.Count then
          listview1.Items[oldindex].MakeVisible(false);
      end;

      listview1.Items.EndUpdate;
      listview1.Visible:=true;
    end;
  end else
  begin
    panel1.Visible:=false;
  end;
end;

procedure TfrmMemoryAllocHandler.ListBox1DblClick(Sender: TObject);
var o: TMemoryAllocEvent;
begin
  if (sender as tlistbox).ItemIndex<>-1 then
  begin
    o:=TMemoryAllocEvent((sender as tlistbox).Items.Objects[(sender as tlistbox).ItemIndex]);
    memorybrowser.memoryaddress:=o.baseaddress;
    memorybrowser.RefreshMB;
  end;
end;

procedure TfrmMemoryAllocHandler.FormDestroy(Sender: TObject);
var i: integer;
begin
  for i:=0 to listbox1.Items.Count-1 do
    if listbox1.Items.Objects[i]<>nil then
      TMemoryAllocEvent(listbox1.Items.Objects[i]).Free;

  for i:=0 to lbSearchResults.Items.Count-1 do
    if lbSearchResults.Items.Objects[i]<>nil then
      TMemoryAllocEvent(lbSearchResults.Items.Objects[i]).Free;

  if watcher<>nil then
    freeandnil(watcher);

  if HasSetupDataEvent>0 then
    closehandle(HasSetupDataEvent);

  if CEHasHandledItEvent>0 then
    closehandle(CEHasHandledItEvent);
end;

procedure TfrmMemoryAllocHandler.ListView1DblClick(Sender: TObject);
begin
  if listview1.ItemIndex<>-1 then
    memorybrowser.disassemblerview.SelectedAddress:=strtoint('$'+listview1.items[listview1.itemindex].subitems[0]);
end;

procedure TfrmMemoryAllocHandler.rbStackTraceModulesClick(Sender: TObject);
begin
  listbox1.OnClick(listbox1);
end;

procedure TfrmMemoryAllocHandler.Saveselectedfingerprint1Click(
  Sender: TObject);
var
  t: tstringlist;
  o: TmemoryAllocevent;
  tracefull, tracehalf, traceapp: tstringlist;
  seperator: tstringlist;
begin
  if (listbox1.ItemIndex<>-1) and savedialog1.execute then
  begin
    t:=tstringlist.Create;
    tracefull:=TStringlist.Create;
    tracehalf:=TStringlist.Create;
    traceapp:=TStringlist.Create;
    seperator:=TStringlist.Create;
    try
      seperator.Add('========');

      o:=TmemoryAllocevent(listbox1.items.objects[listbox1.ItemIndex]);
      ce_stacktrace(o.esp,0,0,@o.stack[0],o.stacksize, tracefull, true,false,false);
      ce_stacktrace(o.esp,0,0,@o.stack[0],o.stacksize, tracehalf, true,true,false);
      ce_stacktrace(o.esp,0,0,@o.stack[0],o.stacksize, traceapp, true,true,true);

      t.AddStrings(tracefull);
      t.AddStrings(seperator);
      t.AddStrings(tracehalf);
      t.AddStrings(seperator);
      t.AddStrings(traceapp);

      t.SaveToFile(savedialog1.FileName);
    finally
      t.Free;
      tracefull.free;
      tracehalf.free;
      traceapp.free;
      seperator.free;
    end;
  end;
end;

procedure TfrmMemoryAllocHandler.Searchfingerprint1Click(Sender: TObject);
var
  t: TStringlist;
  i,j: integer;
  tracefull, tracehalf, traceapp: tstringlist;
  lookfor: tstringlist;
  temptrace: tstringlist;
  depth: integer;
  saddress, sbytes, sdetails: string;
  o,o2:TmemoryAllocevent;
  match: boolean;
begin
  if opendialog1.Execute then
  begin
    t:=tstringlist.Create;
    tracefull:=TStringlist.Create;
    tracehalf:=TStringlist.Create;
    traceapp:=TStringlist.Create;
    lookfor:=TStringlist.Create;
    temptrace:=TStringlist.Create;

    lbSearchResults.Items.BeginUpdate;

    for i:=0 to lbSearchResults.items.Count-1 do
      TmemoryAllocevent(lbSearchResults.Items.Objects[i]).Free;


    lbSearchResults.Clear;

    t.LoadFromFile(opendialog1.FileName);
    try
      //seperate the traces
      i:=0;
      while i<t.Count-1 do
      begin
        if t[i]='========' then
        begin
          inc(i);
          break;
        end;

        tracefull.Add(t[i]);
        inc(i);
      end;

      while i<t.Count-1 do
      begin
        if t[i]='========' then
        begin
          inc(i);
          break;
        end;

        tracehalf.Add(t[i]);
        inc(i);
      end;

      while i<t.Count do
      begin
        traceapp.Add(t[i]);
        inc(i);
      end;


      //ask which one to use
      with tfrmMemoryAllocHandlerSearchFingerprint.create(self) do
      begin
        if showmodal=mrok then
        begin
          if not TryStrToInt(edtDepth.text, depth) then
            depth:=5;
            
          if rbTypeAll.checked then
          begin
            for i:=0 to depth-1 do
            begin
              seperatestacktraceline(tracefull[i],saddress, sbytes, sdetails);
              lookfor.Add(sdetails);
            end;

            for i:=0 to listbox1.Count-1 do
            begin
              o:=TmemoryAllocevent(listbox1.Items.Objects[i]);

              temptrace.Clear;
              ce_stacktrace(o.esp,0,0,@o.stack[0],o.stacksize, temptrace, true,false,false,depth);
              if temptrace.count>0 then //at least 1
              begin
                match:=true;
                for j:=0 to min(min(depth-1,temptrace.Count),lookfor.Count-1) do
                begin
                  //check
                  if temptrace[j]<>lookfor[j] then
                  begin
                    match:=false;
                    break;
                  end;
                end;

                if match then
                begin
                  o2:=TmemoryAllocevent.Create;
                  o2.assign(o);
                  lbSearchResults.Items.AddObject(listbox1.Items[i], o2);
                end;
              end;
            end;

          end
          else
          if rbTypeModules.checked then
          begin
            //module sonly
            for i:=0 to depth-1 do
            begin
              seperatestacktraceline(tracehalf[i],saddress, sbytes, sdetails);
              lookfor.Add(sdetails);
            end;

            for i:=0 to listbox1.Count-1 do
            begin
              o:=TmemoryAllocevent(listbox1.Items.Objects[i]);

              temptrace.Clear;
              ce_stacktrace(o.esp,0,0,@o.stack[0],o.stacksize, temptrace, true,true,false,depth);
              if temptrace.count>0 then //at least 1
              begin
                match:=true;
                for j:=0 to min(min(depth-1,temptrace.Count),lookfor.Count-1) do
                begin
                  //check
                  if temptrace[j]<>lookfor[j] then
                  begin
                    match:=false;
                    break;
                  end;
                end;

                if match then
                begin
                  o2:=TmemoryAllocevent.Create;
                  o2.assign(o);
                  lbSearchResults.Items.AddObject(listbox1.Items[i], o2);
                end;
              end;
            end;
          end
          else
          begin
            //app modules only
            for i:=0 to depth-1 do
            begin
              seperatestacktraceline(traceapp[i],saddress, sbytes, sdetails);
              lookfor.Add(sdetails);
            end;

            for i:=0 to listbox1.Count-1 do
            begin
              o:=TmemoryAllocevent(listbox1.Items.Objects[i]);

              temptrace.Clear;
              ce_stacktrace(o.esp,0,0,@o.stack[0],o.stacksize, temptrace, true,true,true,depth);
              if temptrace.count>0 then //at least 1
              begin
                match:=true;
                for j:=0 to min(min(depth-1,temptrace.Count-1),lookfor.Count-1) do
                begin
                  //check
                  seperatestacktraceline(temptrace[j],saddress, sbytes, sdetails);
                  if sdetails<>lookfor[j] then
                  begin
                    match:=false;
                    break;
                  end;
                end;

                if match then
                begin
                  o2:=TmemoryAllocevent.Create;
                  o2.assign(o);
                  lbSearchResults.Items.AddObject(listbox1.Items[i], o2);
                end;
              end;
            end;
          end;



        end;

        free;
      end;

      lbSearchResults.Visible:=true;
    finally
      lbSearchResults.Items.EndUpdate;
      t.free;
      temptrace.free;
      tracefull.free;
      tracehalf.free;
      traceapp.free;
      lookfor.free;
    end;
  end;
end;

procedure TfrmMemoryAllocHandler.Closesearchresults1Click(Sender: TObject);
var i: integer;
begin
  lbSearchResults.Visible:=false;
  for i:=0 to lbSearchResults.items.Count-1 do
    TmemoryAllocevent(lbSearchResults.Items.Objects[i]).Free;

  lbSearchResults.Clear;
end;

procedure TfrmMemoryAllocHandler.Findtext1Click(Sender: TObject);
begin
  finddialog1.Execute;
end;

procedure TfrmMemoryAllocHandler.FindDialog1Find(Sender: TObject);
var objectstart: integer;
    viewstart: integer;
    searchTextUP: string;
    i: integer;
    wholeword: boolean;
begin
  searchTextUP:=uppercase(finddialog1.FindText);

  wholeword:=frWholeWord in finddialog1.Options;

  listbox1.Items.BeginUpdate;
  listview1.Items.BeginUpdate;
  try

    objectstart:=listbox1.ItemIndex;
    if objectstart=-1 then objectstart:=0;



    for objectstart:=objectstart to listbox1.Items.Count-1 do
    begin
      if listbox1.ItemIndex<>objectstart then
      begin
        listbox1.ItemIndex:=objectstart;
        viewstart:=0;
      end
      else
      begin
        //check if something is already selected in listview1
        viewstart:=listview1.ItemIndex;
      end;

      for i:=viewstart+1 to listview1.Items.Count-1 do //start from next index
      begin
        if (wholeword and (searchTextUP=uppercase(listview1.Items[i].SubItems[1]))) or //if whole word and word matches
           ((not wholeword) and (pos(searchTextUP, uppercase(listview1.Items[i].SubItems[1]))>0)) //or not whole word and partial match then
        then
        begin
          //found it
          listview1.ItemIndex:=i;
          listview1.Items[i].MakeVisible(false);
          exit;
        end;
      end;
    end;

  finally
    listbox1.Items.EndUpdate;
    listview1.Items.EndUpdate;
  end;
end;

procedure TfrmMemoryAllocHandler.Searchagain1Click(Sender: TObject);
begin
  if finddialog1.FindText<>'' then
    finddialog1.OnFind(finddialog1)
  else
    finddialog1.Execute;
end;

end.
