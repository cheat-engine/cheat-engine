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
    Search1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure rbStackTraceModulesClick(Sender: TObject);
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

uses frmautoinjectunit, memorybrowserformunit;



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
  if listbox1.ItemIndex<>-1 then
  begin
    if not panel1.Visible then panel1.visible:=true;
    o:=TMemoryAllocEvent(listbox1.Items.Objects[listbox1.ItemIndex]);
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
  if listbox1.ItemIndex<>-1 then
  begin
    o:=TMemoryAllocEvent(listbox1.Items.Objects[listbox1.ItemIndex]);
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

end.
