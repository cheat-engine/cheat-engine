unit frmEditHistoryUnit;

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Menus, NewKernelHandler, syncobjs,
  symbolhandler, lua, lauxlib, lualib;

type

  { TfrmEditHistory }

  TfrmEditHistory = class(TForm)
    Button1: TButton;
    cbLogWrites: TCheckBox;
    edtMaxWriteLogSize: TEdit;
    wlImageList: TImageList;
    Label1: TLabel;
    lvWriteLog: TListView;
    miUndo: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure cbLogWritesChange(Sender: TObject);
    procedure edtMaxWriteLogSizeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvWriteLogDblClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }

    procedure refreshWriteLogView;
  end;

  TWriteLogEntry=class
  public
    id: integer;
    address: ptruint;
    originalsize: dword;
    originalbytes: pbyte;

    newsize: dword;
    newbytes: pbyte;
    destructor destroy; override;
  end;


var
  frmEditHistory: TfrmEditHistory;
  logWrites: boolean=false;


procedure addWriteLogEntryToList(wle: TWriteLogEntry);
procedure setMaxWriteLogSize(size: integer);
procedure clearWriteLog;
procedure undoLastWrite;

procedure initializeLuaWriteLog;

function hasAddressBeenChanged(address: ptruint): boolean;
function hasAddressBeenChangedRange(startaddress, stopaddress: ptruint): boolean;

procedure undowrite(address: ptruint); overload;

implementation

{$R *.lfm}

uses MemoryBrowserFormUnit, ProcessHandlerUnit, globals, luahandler, LuaClass,
  LuaByteTable, LuaObject, maps;

var
  nextid: integer;
  writelog: TList; //for a sorted list in the order it got changed
  writelogmap: TMap; //for an address lookup to see if an address has been changed   (holds every single byte)
  writelogcs: TCriticalSection;
  maxwritelogsize: integer=250;

{ TfrmEditHistory }



destructor TWriteLogEntry.destroy;
begin
  if originalbytes<>nil then
    FreeMemAndNil(originalbytes);

  if newbytes<>nil then
    FreeMemAndNil(newbytes);

  inherited destroy;
end;

procedure removeBytesFromWriteLogMap(wle: TWriteLogEntry);
var
  i: integer;
  data: TObject;
  a: ptruint;
  oldwle: TWriteLogEntry;
begin
  for i:=0 to wle.newsize-1 do
  begin
    a:=wle.address+i;
    if writelogmap.GetData(a,data) then
    begin
      if data is TWriteLogEntry then //no list
        writelogmap.Delete(a)
      else //tlist
      begin
        tlist(data).Remove(wle);
        if tlist(data).Count=1 then //no list needed anymore
        begin
          oldwle:=TWriteLogEntry(tlist(data)[0]);
          tlist(data).free;
          writelogmap.SetData(a,oldwle);
        end;
      end;
    end;
  end;
end;



procedure eraseWriteLogEntry(i: integer);
var wle: TWriteLogEntry;
begin

  writelogcs.Enter;
  try
    if i<writelog.count then
    begin
      wle:=writelog[i];
      writelog.Delete(i);
      removeBytesFromWriteLogMap(wle);
      wle.free;


    end;

  finally
    writelogcs.Leave;
  end;
end;

procedure clearWriteLog;
begin
  while writelog.Count>0 do
    eraseWriteLogEntry(writelog.count-1);

  writelogmap.Clear; //should be empty already

end;

procedure addBytesToWriteLogMap(wle: TWriteLogEntry);
var
  i: integer;
  data: TObject;

  newlist: TList;
  a: ptruint;
begin
  for i:=0 to wle.newsize-1 do
  begin
    a:=wle.address+i;
    if writelogmap.GetData(a,data) then
    begin
      if data is TWriteLogEntry then
      begin
        //change the old entry to a list
        newlist:=tlist.create;
        newlist.add(data); //old entry first in the list
        newlist.add(wle);  //and now the new one

        writelogmap.SetData(a, newlist);
      end
      else //tlist
        tlist(data).Add(wle);
    end
    else
      writelogmap.Add(a,wle);
  end;

end;


procedure addWriteLogEntryToList(wle: TWriteLogEntry);
begin
  wle.id:=nextid;
  inc(nextid);

  writelogcs.Enter;
  try
    if writelog.Count>maxwritelogsize then
      eraseWriteLogEntry(0);

    writelog.Add(wle);

    addBytesToWriteLogMap(wle);
  finally
    writelogcs.leave;
  end;

  if (MainThreadID=GetCurrentThreadId) and (frmEditHistory<>nil) and (not frmEditHistory.Timer1.Enabled) then
    frmEditHistory.Timer1.Enabled:=true;

end;

procedure setMaxWriteLogSize(size: integer);
begin
  maxwritelogsize:=size;

  while writelog.count>maxwritelogsize do
    eraseWriteLogEntry(0);
end;



procedure undowriteInternal(id: integer); overload;
var i: integer;
  wle: TWriteLogEntry;
  x: ptruint;
  oldprot: dword;

  oldLogWrites: boolean;
  vpe: boolean;
begin
  writelogcs.enter;
  try
    for i:=0 to writelog.count-1 do
    begin
      wle:=TWriteLogEntry(writelog[i]);

      if wle.id=id then
      begin
        vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(processhandle, pointer(wle.address), wle.originalsize, PAGE_EXECUTE_READWRITE, oldprot);

        oldLogWrites:=logWrites;
        logWrites:=false;
        WriteProcessMemory(processhandle, pointer(wle.address), wle.originalbytes, wle.originalsize, x);
        logWrites:=oldLogWrites;
        if vpe then VirtualProtectEx(processhandle, pointer(wle.address), wle.originalsize, oldprot, oldprot);
        eraseWriteLogEntry(i);
        break;
      end;
    end;
  finally
    writelogcs.leave;
  end;

  if frmEditHistory<>nil then
    frmEditHistory.refreshWriteLogView;
end;

procedure undowrite(address: ptruint); overload;
var
  data: TObject;
  list: TList absolute data;
begin
  if writelogmap.GetData(address, data) then
  begin
    if data is TWriteLogEntry then
      undowriteInternal(TWriteLogEntry(data).id)
    else
      undowriteInternal(TWriteLogEntry(list[list.Count-1]).id);
  end;
end;

procedure undoLastWrite;
begin
  writelogcs.enter;
  try
    if writelog.Count>0 then
      undowriteInternal(TWriteLogEntry(writelog[writelog.count-1]).id);
  finally
    writelogcs.leave;
  end;
end;


procedure TfrmEditHistory.refreshWriteLogView;
var
  i,j: integer;
  wle: TWriteLogEntry;
  e: tlistitem;

  s: string;

  selectedids: TIntegerSet;
  mainselectedid: integer;
begin
  if lvWriteLog.itemindex<>-1 then
    mainselectedid:=integer(ptruint(lvWriteLog.items[lvWriteLog.ItemIndex].Data))
  else
    mainselectedid:=-1;

  selectedids:=[];
  for i:=0 to lvWriteLog.Items.Count-1 do
    if lvWriteLog.Items[i].Selected then
      selectedids:=selectedids+[integer(ptruint(lvWriteLog.Items[i].data))];



  lvwritelog.BeginUpdate;
  lvWriteLog.Clear;
  writelogcs.enter;
  try
    for i:=0 to writelog.Count-1 do
    begin
      wle:=writelog[i];
      e:=lvwritelog.Items.Add;
      e.Caption:=symhandler.getNameFromAddress(wle.address);

      s:='';
      for j:=0 to wle.originalsize-1 do
        s:=s+inttohex(wle.originalbytes[j],2)+' ';

      e.SubItems.add(s);

      s:='';
      for j:=0 to wle.newsize-1 do
        s:=s+inttohex(wle.newbytes[j],2)+' ';

      e.SubItems.add(s);

      e.Data:=pointer(wle);
    end;

  finally
    writelogcs.leave;
  end;

  for i:=0 to lvwritelog.items.count-1 do
  begin
    if uintptr(lvWriteLog.Items[i].data) in selectedids then
      lvWriteLog.Items[i].selected:=true;

    if (mainselectedid<>-1) and (uintptr(lvWriteLog.Items[i].data)=mainselectedid) then
      lvWriteLog.ItemIndex:=i;
  end;

  lvWriteLog.EndUpdate;
end;

procedure TfrmEditHistory.Button1Click(Sender: TObject);
begin
  refreshWriteLogView;
end;

procedure TfrmEditHistory.cbLogWritesChange(Sender: TObject);
begin
  logwrites:=cbLogWrites.checked;
end;

procedure TfrmEditHistory.edtMaxWriteLogSizeChange(Sender: TObject);
var v: integer;
begin
  try
    setMaxWriteLogSize(StrToInt(edtMaxWriteLogSize.text));
  except
  end;
end;

procedure TfrmEditHistory.FormShow(Sender: TObject);
begin
  cbLogWrites.Checked:=logWrites;
  edtMaxWriteLogSize.text:=inttostr(maxwritelogsize);
end;

procedure TfrmEditHistory.lvWriteLogDblClick(Sender: TObject);
begin
  if lvWriteLog.Selected<>nil then
    MemoryBrowser.disassemblerview.SelectedAddress:=symhandler.getAddressFromName(lvWriteLog.Selected.Caption);
end;



procedure TfrmEditHistory.miUndoClick(Sender: TObject);
var i: integer;
begin
  for i:=lvWriteLog.items.count-1 downto 0 do
  begin
    if lvWriteLog.Items[i].Selected then
      undowrite(TWriteLogEntry(lvWriteLog.Items[i].Data).address);
  end;

end;

procedure TfrmEditHistory.Timer1Timer(Sender: TObject);
begin
  timer1.enabled:=false;
  refreshWriteLogView;
end;

//--------------------Lua-----------------
type
  TWriteLog=class
  private
    function getWriteLogStatus: boolean;
    procedure setWriteLogStatus(status: boolean);
    function getWriteLogSize: integer;
    procedure setWriteLogSize(size: integer);
  public
  published
    property status: boolean read getWriteLogStatus write setWriteLogStatus;
    property logsize: integer read getWriteLogSize write setWriteLogSize;
  end;

var _writelog: TWriteLog;

function TWriteLog.getWriteLogStatus: boolean;
begin
  result:=logWrites;
end;

procedure TWriteLog.setWriteLogStatus(status: boolean);
begin
  logWrites:=status;
end;

function TWriteLog.getWriteLogSize: integer;
begin
  result:=maxwritelogsize;
end;

procedure TWriteLog.setWriteLogSize(size: integer);
begin
  maxwritelogsize:=size;
end;


function lua_getWriteLog(L:PLua_state): integer; cdecl;
begin
  if _writelog=nil then
    _writelog:=TWritelog.create;

  luaclass_newClass(L,_writelog);
  result:=1;
end;

function writelog_getLog(L:PLua_state): integer; cdecl;
var
  wle: TWriteLogEntry;
  i: integer;
begin
  writelogcs.enter;
  try
    lua_newtable(L);

    for i:=0 to writelog.count-1 do
    begin
      lua_pushinteger(L,i);
      lua_newtable(L);
      wle:=TWriteLogEntry(writelog[i]);

      lua_pushstring(L, 'address');
      lua_pushinteger(L, wle.address);
      lua_settable(L,-3);

      lua_pushstring(L, 'original');
      CreateByteTableFromPointer(L, pbytearray(wle.originalbytes), wle.originalsize);
      lua_settable(L,-3);

      lua_pushstring(L, 'new');
      CreateByteTableFromPointer(L, pbytearray(wle.newbytes), wle.newsize);
      lua_settable(L,-3);

      lua_settable(L,-3);
    end;

    result:=1;
  finally
    writelogcs.leave;
  end;
end;

procedure writelog_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getLog', writelog_getLog);
end;

procedure initializeLuaWriteLog;
begin
  luaclass_register(TWriteLog, writelog_addMetaData);
  lua_register(LuaVM, 'getWriteLog', lua_getWriteLog);
end;

function hasAddressBeenChanged(address: ptruint): boolean;
begin
  if logwrites then
    result:=writelogmap.HasId(address)
  else
    result:=false;
end;

function hasAddressBeenChangedRange(startaddress, stopaddress: ptruint): boolean;
var i: integer;

  startb: ptruint;
  stopb: ptruint;
  wle: TWriteLogEntry;
begin
  if stopaddress-startaddress<writelog.count*4 then
  begin
    while startaddress<stopaddress do
    begin
      if hasAddressBeenChanged(startaddress) then
        exit(true);

      inc(startaddress);
    end;
  end
  else
  begin
    //search the list
    for i:=0 to writelog.count-1 do
    begin
      wle:=TWriteLogEntry(writelog[i]);
      startb:=wle.address;
      stopb:=wle.address+wle.newsize;

      if ((startaddress < stopb) and (startb < stopaddress)) then
        exit(true);
    end;
  end;

  result:=false;


end;

initialization
  writelog:=TList.create;
  writelogmap:=TMap.Create(itsPtrSize, sizeof(TObject));
  writelogcs:=TCriticalSection.create;




finalization


  while writelog.count>0 do
    eraseWriteLogEntry(writelog.count-1);

  writelog.free;

  writelogcs.free;

end.

