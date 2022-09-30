unit iptlogdisplay;

{$mode ObjFPC}{$H+}

interface

uses
  {$ifdef windows}windows,{$endif} ProcessHandlerUnit, Classes, SysUtils, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, betterControls,
  libipt, pagemap, Contnrs, syncobjs,maps;

type

  { TfrmIPTLogDisplay }

{$ifdef windows}

  TIPList=record
    list: pqword;   //max 64MB(8388608 entries) / block
    listpos: dword;
    listsize: integer;
  end;
  PIPList=^TIPList;
  PPIPList=^PIPList;


  TIPTLogLoaderCommand=(illGetCurrentRange, illGetPreviousRange, illGetNextRange, illGetAll);
  TIPTLogLoaderCommandData=record
    command: TIPTLogLoaderCommand;
  end;
  PIPTLogLoaderCommandData=^TIPTLogLoaderCommandData;

  TIPTLogDataReadyEvent=procedure(sender: TObject; log: PPIPList; count: integer) of object;

  TIPTLogLoader=class(TThread)
  private
    hascommand: TEvent;
    commandscs: TCriticalSection;
    commands: TQueue;
    fOnDataReady: TIPTLogDataReadyEvent;
    pagemap: TPageMap;

    rangelist: TFPList; //contains offsets of syncpoints
    currentrange: integer;

    data: PPIPList;
    datapos: integer;
    datasize: integer;

    logname: string;
    iptlog: pointer;
    logsize: qword;

    progress:qword;

    iptConfig: pt_config;
    decoder: ppt_insn_decoder;
    callbackImage: PPT_Image;

    procedure cleardata;
    procedure doDataReady;


    procedure getCurrentRangeInternal(skipdonenotification: boolean=false);
    procedure getPreviousRangeInternal;
    procedure getNextRangeInternal;
    procedure getAllInternal;

  protected
    procedure TerminatedSet; override;
  public

    stopprocessing: boolean;
    ripseen: tmap;

    procedure getCurrentRange;
    procedure getPreviousRange;
    procedure getNextRange;
    procedure getAll;

    procedure execute; override;
    constructor Create(_logname: string; _iptlog: pointer; _logsize: dword);
    destructor destroy; override;
    property OnDataReady: TIPTLogDataReadyEvent read fOnDataReady write fOnDataReady;
  end;
{$endif}
  TfrmIPTLogDisplay = class(TForm)
    btnFetchMore: TButton;
    btnFetchAll: TButton;
    lblCount: TLabel;
    lvResults: TListView;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miSavetoFile: TMenuItem;
    ButtonPanel: TPanel;
    Progressbar: TProgressBar;
    Timer1: TTimer;

    procedure btnFetchMoreClick(Sender: TObject);
    procedure btnFetchAllClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvResultsData(Sender: TObject; Item: TListItem);
    procedure lvResultsDblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    {$ifdef windows}
    logloader: TIPTLogLoader;

    iptracedata: PPIPList;
    iptracedatacount: integer;

    totalcount: qword;

    ripseen: TMap; //counter for each address seen (counter/totalresults*100 is percentage cpu)
    fetchingData: boolean;
    stopIP: ptruint;
    procedure cleardata;
    procedure LogLoaderDataReady(sender: TObject; log: PPIPList; count: integer);
    {$endif}
  public
    procedure loadlog(logname: string; iptlog: pointer; size:dword; endAtIP: ptruint=0);

  end;


implementation

uses math, cefuncproc, disassembler, MemoryBrowserFormUnit, symbolhandler;

{$ifdef windows}
function iptReadMemory(buffer: PByte; size: SIZE_T; asid: PPT_ASID; ip: uint64; context: pointer): integer; cdecl;
var
  br: ptruint;
  self: TIPTLogLoader;
  pi: PPageInfo;
  b: pbyte;
begin
  result:=0;

  self:=TIPTLogLoader(context);

  pi:=self.pagemap.GetPageInfo(ip shr 12);
  b:=nil;
  if pi=nil then
  begin
    getmem(b,4096);
    if readprocessmemory(processhandle, pointer(ip and $fffffffffffff000), b,4096,br)=false then
    begin
      freemem(b);
      b:=nil;
    end;
    self.pagemap.Add(ip shr 12, b);
  end
  else
    b:=pbyte(pi^.data);

  if b=nil then exit(0);

  //send bytes from data
  if (ip and $fff)+size>$1000 then
  begin
    result:=$1000-(ip and $fff);
    copymemory(buffer, @b[ip and $fff], result);


    size:=size-result;
    ip:=ip+result;
    result:=result+iptReadMemory(@buffer[result], size, asid, ip, context);
  end
  else
  begin
    copymemory(buffer, @b[ip and $fff], size);
    result:=size;
  end;

end;

procedure TIPTLogLoader.doDataReady;
begin
  if assigned(fOnDataReady) then
    fOnDataReady(self, data,datapos+1);

  data:=nil; //eaten by the handler
end;

procedure TIPTLogLoader.cleardata;
var i: integer;
begin
  if data<>nil then
  begin
    for i:=0 to datapos-1 do
    begin
      freemem(data[i]^.list);
      freemem(data[i]);
    end;

    freemem(data);
    data:=nil;
  end;
end;


procedure TIPTLogLoader.getCurrentRangeInternal(skipdonenotification: boolean=false);
var
  insn: pt_insn;
  di: integer;
  counter: qword;

  ripcounter: pqword;
  val1: qword=1;
  i: integer;
begin
  if data=nil then
  begin
    //init a new list
    datasize:=4;
    getmem(data, 4*sizeof(PIPList));
    datapos:=0;

    data[0]:=getmem(sizeof(TIPList));
    data[0]^.listpos:=0;
    data[0]^.listsize:=1024;
    data[0]^.list:=getmem(1024*8);
  end;

  di:=datapos;


  counter:=0;
  ripseen.clear;

  if (rangelist.count>0) and (currentrange>=0) and (pt_insn_sync_set(decoder, qword(rangelist[currentrange]))>=0) then
  repeat
    zeromemory(@insn,sizeof(insn));
    while (pt_insn_next(decoder, @insn, sizeof(insn))>=0) do
    begin
      if  insn.iclass=ptic_error then
        insn.ip:=0;

      ripcounter:=ripseen.GetDataPtr(insn.ip);
      if ripcounter<>nil then
        inc(ripcounter^)
      else
        ripseen.Add(insn.ip, val1);


      data[di]^.list[data[di]^.listpos]:=insn.ip;
      inc(data[di]^.listpos);
      if data[di]^.listpos=data[di]^.listsize then
      begin
        //new block
        inc(datapos);
        if datapos>=datasize then
        begin
          datasize:=datasize*2;
          ReAllocMem(data,datasize*sizeof(PIPList));
        end;

        data[datapos]:=getmem(sizeof(TIPList));
        data[datapos]^.listpos:=0;
        data[datapos]^.listsize:=min(8388608, data[di]^.listsize*2);
        data[datapos]^.list:=getmem(data[datapos]^.listsize*8);;

        di:=datapos;
      end;

      inc(counter);
      if counter mod 512=0 then
      begin
        pt_insn_get_offset(decoder, @progress);

        if terminated or stopprocessing then break;
      end;
    end;
  until pt_insn_sync_forward(decoder)<0;



  if (not terminated) and (not skipdonenotification) then
    Synchronize(@dodataready);
end;

procedure TIPTLogLoader.getPreviousRangeInternal;
begin

  if currentrange<=0 then
  begin
    Synchronize(@doDataReady)
  end
  else
  begin
    dec(currentrange);
    getCurrentRangeInternal;
  end;
end;

procedure TIPTLogLoader.getNextRangeInternal;
begin

  if currentrange>=rangelist.count-1 then
  begin
    Synchronize(@doDataReady)
  end
  else
  begin
    inc(currentrange);
    getCurrentRangeInternal;
  end;
end;

procedure TIPTLogLoader.getAllInternal;
begin
  currentrange:=0;
  getCurrentRangeInternal;
end;

procedure TIPTLogLoader.getCurrentRange;
var c: PIPTLogLoaderCommandData;
begin
  c:=getmem(sizeof(TIPTLogLoaderCommandData));
  c^.command:=illGetCurrentRange;
  commandscs.Enter;
  commands.Push(c);
  hascommand.SetEvent;
  commandscs.Leave;


end;

procedure TIPTLogLoader.getPreviousRange;
var c: PIPTLogLoaderCommandData;
begin
  c:=getmem(sizeof(TIPTLogLoaderCommandData));
  c^.command:=illGetPreviousRange;
  commandscs.Enter;
  commands.Push(c);
  hascommand.SetEvent;
  commandscs.Leave;


end;

procedure TIPTLogLoader.getNextRange;
var c: PIPTLogLoaderCommandData;
begin
  c:=getmem(sizeof(TIPTLogLoaderCommandData));
  c^.command:=illGetNextRange;
  commandscs.Enter;
  commands.Push(c);
  hascommand.SetEvent;
  commandscs.Leave;


end;


procedure TIPTLogLoader.getAll;
var c: PIPTLogLoaderCommandData;
begin
  c:=getmem(sizeof(TIPTLogLoaderCommandData));
  c^.command:=illGetAll;
  commandscs.Enter;
  commands.Push(c);
  hascommand.SetEvent;
  commandscs.Leave;
end;


procedure TIPTLogLoader.execute;
var
  command: PIPTLogLoaderCommandData;
  o: qword;
  i: integer;
begin
  //setup the ipt tracer
  callbackImage:=pt_image_alloc(pchar(logname));
  try
    pt_image_set_callback(callbackImage,@iptReadMemory,self);

    pt_config_init(@iptConfig);
    pt_cpu_read(@iptConfig.cpu);
    pt_cpu_errata(@iptConfig.errata, @iptConfig.cpu);

    iptConfig.beginaddress:=pointer(iptlog);
    iptConfig.endaddress:=iptConfig.beginaddress+logSize;

    decoder:=pt_insn_alloc_decoder(@iptConfig);

    if decoder<>nil then
    begin
      try
        pt_insn_set_image(decoder, callbackImage);

        i:=pt_insn_sync_set(decoder,0);

        while pt_insn_sync_forward(decoder)>=0 do
        begin
          if pt_insn_get_sync_offset(decoder, @o)>=0 then
            rangelist.add(pointer(o));
        end;

        currentrange:=rangelist.count-1;

        //now wait for commands
        while not terminated do
        begin
          hascommand.WaitFor(10000);
          if terminated then exit;

          repeat
            commandscs.enter;
            command:=commands.Pop;
            commandscs.leave;

            if command<>nil then
            begin
              stopprocessing:=false;
              case command^.command of
                illGetCurrentRange:  getCurrentRangeInternal;
                illGetPreviousRange: getPreviousRangeInternal;
                illGetNextRange:     getNextRangeInternal;
                illGetAll:           getAllInternal;
              end;
            end;

            freemem(command);

          until (command=nil) or terminated;

        end;

      finally
        pt_insn_free_decoder(decoder);
        decoder:=nil;
      end;
    end;
  finally
    pt_image_free(callbackImage);
    callbackImage:=nil
  end;
end;

procedure TIPTLogLoader.TerminatedSet;
begin
  if hascommand<>nil then
    hascommand.SetEvent;

  inherited TerminatedSet;
end;

destructor TIPTLogLoader.destroy;
begin
  terminate;
  waitfor;

  if commands<>nil then
    freeandnil(commands);

  if commandscs<>nil then
    freeandnil(commandscs);

  if hascommand<>nil then
    freeandnil(hascommand);

  if pagemap<>nil then
    freeandnil(pagemap);

  cleardata;

  if rangelist<>nil then
    freeandnil(rangelist);

  if decoder<>nil then
  begin
    pt_insn_free_decoder(decoder);
    decoder:=nil;
  end;

  if callbackImage<>nil then
  begin
    pt_image_free(callbackImage);
    callbackImage:=nil;
  end;

  if iptlog<>nil then
    FreeMemAndNil(iptlog);

  inherited destroy;
end;

constructor TIPTLogLoader.Create(_logname: string; _iptlog: pointer; _logsize: dword);
begin
  commands:=TQueue.create;
  commandscs:=TCriticalSection.create;
  hascommand:=TEvent.Create(nil,false,false,'');

  pagemap:=TPageMap.create;

  logname:=_logname;
  logsize:=_logsize;
  getmem(iptlog, logsize);
  copymemory(iptlog, _iptlog, logsize);

  rangelist:=TFPList.Create;


  inherited create(false);
end;
{$endif}


procedure TfrmIPTLogDisplay.loadlog(logname: string; iptlog: pointer; size:dword; endAtIP: ptruint=0);
begin
  //reset to default
  {$ifdef windows}
  if logloader<>nil then
  begin
    logloader.terminate;
    logloader.waitfor;
    freeandnil(logloader);
  end;
  ripseen.clear;
  cleardata;

  lvresults.items.count:=0;


  logloader:=TIPTLogLoader.Create(logname, iptlog, size);
  logloader.ripseen:=ripseen;
  logloader.OnDataReady:=@LogLoaderDataReady;

  logloader.getCurrentRange;

  lblcount.caption:='0';
  btnFetchMore.visible:=true;
  btnFetchAll.visible:=false;

  btnFetchMore.enabled:=false;
  btnFetchAll.enabled:=false;
  lvResults.cursor:=crHourGlass;

  fetchingData:=true;

  stopIP:=endAtIP;
  {$endif}
end;

procedure TfrmIPTLogDisplay.btnFetchMoreClick(Sender: TObject);
begin
 {$ifdef windows}

  btnFetchMore.enabled:=false;
  btnFetchAll.enabled:=false;
  lvResults.cursor:=crHourGlass;

  if logloader.currentrange>0 then
    logloader.getPreviousRange
  else
  begin
    btnFetchMore.visible:=false;
    btnFetchAll.visible:=false;
  end;

  fetchingData:=true;
  {$endif}
end;

procedure TfrmIPTLogDisplay.btnFetchAllClick(Sender: TObject);
begin


  {$IFDEF WINDOWS}
  btnFetchMore.enabled:=false;
  btnFetchAll.enabled:=false;
  lvResults.cursor:=crHourGlass;

  btnFetchMore.visible:=false;
  btnFetchAll.visible:=false;

  logloader.getAll;

  fetchingData:=true;

  timer1.enabled:=true;
  Progressbar.visible:=true;
  {$ENDIF}
end;

{$IFDEF WINDOWS}
procedure TfrmIPTLogDisplay.cleardata;
var i: integer;
begin
  if iptracedata<>nil then
  begin
    for i:=0 to iptracedatacount-1 do
      freemem(iptracedata[i]^.list);

    freemem(iptracedata);
    iptracedata:=nil
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
procedure TfrmIPTLogDisplay.LogLoaderDataReady(sender: TObject; log: PPIPList; count: integer);
var oldtop: integer;
  oldindex: integer;
  vo: tpoint;

  i,j: integer;
  wasempty: boolean;

  lineheight: integer;
  r: trect;

  newentrycount: qword;
  diffentrycount: qword;
  found: boolean;

  delcount: integer;
begin
  //logloader data ready. yay
  fetchingdata:=false;

  if log=nil then exit;

  wasempty:=lvResults.items.count=0;


  cleardata;


  newentrycount:=0;
  for i:=0 to count-1 do
    inc(newentrycount, log[i]^.listpos);

  if stopip<>0 then
  begin
    found:=false;


    //delete from the end until the address is eip , if not found, go to an earlier list
    while count>0 do
    begin
      while log[count-1]^.listpos>0 do
      begin
        if InRangeX(log[count-1]^.list[log[count-1]^.listpos-1], stopip-32, stopip+32) then //range as previous opcode could be buggy, and it's scanned from newest to oldest anyhow. So anything near is actually what we want
        begin
          found:=true;
          break;
        end;
        dec(log[count-1]^.listpos);
      end;

      if found then break;

      freemem(log[count-1]);
      dec(count);
    end;

    if not found then
    begin
      if logloader.currentrange>0 then
      begin
        btnFetchMore.click;
        exit;
      end;
    end;
  end;




  newentrycount:=0;
  for i:=0 to count-1 do
    inc(newentrycount, log[i]^.listpos);


  totalcount:=newentrycount;

  diffentrycount:=newentrycount-lvResults.items.count;


  iptracedata:=log;
  iptracedatacount:=count;

  vo:=lvResults.ViewOrigin;
  oldindex:=lvResults.ItemIndex;

  if not wasempty then
  begin
    r:=lvResults.items[0].DisplayRect(drBounds);
    lineheight:=r.Height;
  end
  else
    lineheight:=0;


  lvResults.items.Count:=newentrycount;

  if oldindex<>-1 then
  begin
    lvResults.ItemIndex:=oldindex+diffentrycount;
    lvResults.items[lvResults.ItemIndex].Selected:=true;
    lvResults.selected:=lvResults.items[lvResults.ItemIndex];
    lvResults.ItemFocused:=lvResults.items[lvResults.ItemIndex];

    if lineheight<>0 then
    begin
      vo.y:=vo.y+lineheight*diffentrycount;
      lvResults.ViewOrigin:=vo;
    end;
  end;

  if wasempty then
  begin
    //go to the last item in the list
    lvResults.Items[lvResults.items.count-1].MakeVisible(false);
    lvResults.itemindex:=lvResults.items.count-1;
  end;

  lblCount.caption:=lvResults.items.Count.ToString;
  //adjust the listview size and position as well


  btnFetchMore.enabled:=true;
  btnFetchAll.enabled:=true;
  lvResults.cursor:=crHandPoint;

  Timer1.enabled:=false;
  Progressbar.visible:=false;

  if logloader.currentrange=0 then
  begin
    btnFetchMore.visible:=false;
    btnFetchAll.visible:=false;
  end;


end;
{$ENDIF}

procedure TfrmIPTLogDisplay.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  {$IFDEF WINDOWS}
  if logloader<>nil then
    logloader.Terminate;

  closeaction:=cafree;
  {$ENDIF}
end;

procedure TfrmIPTLogDisplay.FormCreate(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  ripseen:=tmap.Create(ituPtrSize,sizeof(qword));

  LoadFormPosition(self);
  if not libIptInit then raise exception.create('Failure loading libipt');
  {$ENDIF}
end;

procedure TfrmIPTLogDisplay.FormDestroy(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  if logloader<>nil then
  begin
    logloader.terminate;
    logloader.waitfor;
    freeandnil(logloader);
  end;

  ripseen.free;

  cleardata;

  SaveFormPosition(Self);
  {$ENDIF}
end;

procedure TfrmIPTLogDisplay.FormShow(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  lvResults.Column[0].width:=canvas.TextWidth(' Frequency ( ) ');
  lvResults.Column[1].width:=canvas.TextWidth(' XXXXXXXXXXXXXXXXXX ');
  {$ENDIF}
end;

procedure TfrmIPTLogDisplay.lvResultsData(Sender: TObject; Item: TListItem);
var
  address: ptruint;
  c: integer;
  i: integer;
  current: integer;
  timesseen: pqword;
begin
  {$IFDEF WINDOWS}
 current:=0;
  c:=item.Index;



  for i:=0 to iptracedatacount-1 do
  begin
    if current+iptracedata[i]^.listpos>c then
    begin
      //found the list that holds this entry

      address:=iptracedata[i]^.list[c-current];

      if not fetchingdata then
      begin
        timesseen:=ripseen.GetDataPtr(address);
        if timesseen<>nil then
          item.caption:=format('%d (%.2f)',[timesseen^, timesseen^/totalcount*100])
        else
          item.caption:='?';
      end
      else
        item.caption:='';

      item.SubItems.add(symhandler.getNameFromAddress(address));

      defaultDisassembler.disassemble(address);
      item.subitems.add(defaultDisassembler.LastDisassembleData.opcode+' '+defaultDisassembler.LastDisassembleData.parameters);

      exit;
    end;

    inc(current, iptracedata[i]^.listpos);
  end;

  item.caption:='error '+item.index.ToString;
 {$ENDIF}

end;

procedure TfrmIPTLogDisplay.lvResultsDblClick(Sender: TObject);
var i: integer;
  current: integer;
  c: integer;
begin
  {$IFDEF WINDOWS}
 current:=0;
  if lvResults.selected<>nil then
  begin
    c:=lvResults.Selected.Index;
    for i:=0 to iptracedatacount-1 do
    begin
      if current+iptracedata[i]^.listpos>c then
      begin
        //found the list that holds this entry
        memorybrowser.backlist.Push(pointer(memorybrowser.disassemblerview.SelectedAddress));
        memorybrowser.disassemblerview.SelectedAddress:=iptracedata[i]^.list[c-current];

        memorybrowser.show;
        break;
      end
      else
        inc(current, iptracedata[i]^.listpos);
    end;
  end;
 {$ENDIF}
end;

procedure TfrmIPTLogDisplay.Timer1Timer(Sender: TObject);
begin
{$IFDEF WINDOWS}
Progressbar.Position:=ceil((logloader.progress / logloader.logsize) * 100);
{$ENDIF}
end;


initialization
  {$I iptlogdisplay.lrs}

end.

