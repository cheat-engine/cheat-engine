unit pluginexports;

{$MODE Delphi}

interface

uses jwawindows, windows, comctrls, Graphics, StdCtrls,sysutils,Controls, SyncObjs,dialogs,LCLIntf,classes,autoassembler,
     CEFuncProc,NewKernelHandler,CEDebugger,kerneldebugger, plugin, math,
     debugHelper, debuggertypedefinitions;

type TPluginFunc=function(parameters: pointer): pointer;
function pluginsync(func: TPluginFunc; parameters: pointer): pointer;

procedure ce_showmessage(s: pchar); stdcall;
function ce_registerfunction(pluginid,functiontype:integer; init: pointer):integer; stdcall;
function ce_unregisterfunction(pluginid,functionid: integer): BOOL; stdcall;
function ce_AutoAssemble(s: pchar):BOOL; stdcall;
function ce_GetMainWindowHandle:thandle; stdcall;
function ce_ChangeRegistersAtAddress(address:ptrUint; changereg: pregistermodificationBP):BOOL; stdcall;

function ce_assembler(address:ptrUint; instruction: pchar; output: PByteArray; maxlength: integer; actualsize: pinteger):BOOL; stdcall;
function ce_disassembler(address: ptrUint; output: pchar; maxsize: integer): BOOL; stdcall;
function ce_disassemble(address: pptrUint; output: pchar; maxsize: integer): BOOL; stdcall;
function ce_previousOpcode(address:ptrUint): ptrUint; stdcall;
function ce_nextOpcode(address:ptrUint): ptrUint; stdcall;
function ce_InjectDLL(dllname: pchar; functiontocall: pchar):BOOL; stdcall;
function ce_processlist(listbuffer: pchar; listsize: integer):BOOL; stdcall;
function ce_reloadsettings:BOOL; stdcall;
function ce_getaddressfrompointer(baseaddress: ptrUint; offsetcount: integer; offsets: PDwordArray):dword; stdcall;

function ce_freezemem(address: ptrUint; size: integer):integer; stdcall;
function ce_unfreezemem(id: integer):BOOL; stdcall;

function ce_sym_addressToName(address:ptrUint; name: pchar; maxnamesize: integer):BOOL; stdcall;
function ce_sym_nameToAddress(name: pchar; address: PDWORD):BOOL; stdcall;
function ce_generateAPIHookScript(address, addresstojumpto, addresstogetnewcalladdress, script: pchar; maxscriptsize: integer): BOOL; stdcall;


function ce_loadModule(modulepath: pchar; exportlist: pchar; maxsize: pinteger): BOOL; stdcall;
function ce_createTableEntry: pointer; stdcall;
function ce_getTableEntry(description: pchar): pointer; stdcall;
function ce_memrec_setDescription(memrec: pointer; description: pchar): BOOL; stdcall;
function ce_memrec_getDescription(memrec: pointer): pchar; stdcall;
function ce_memrec_getAddress(memrec: pointer; address: pptruint; offsets: PDwordArray; maxoffsets: integer; neededOffsets: pinteger): BOOL; stdcall;
function ce_memrec_setAddress(memrec: pointer; address: pchar; offsets: PDwordArray; offsetcount: integer): BOOL; stdcall;
function ce_memrec_getType(memrec: pointer): integer; stdcall;
function ce_memrec_setType(memrec: pointer; vtype: integer): BOOL; stdcall;
function ce_memrec_getValue(memrec: pointer; value: pchar; maxsize: integer): BOOL; stdcall;
function ce_memrec_setValue(memrec: pointer; value: pchar): BOOL; stdcall;
function ce_memrec_getScript(memrec: pointer): pchar; stdcall;
function ce_memrec_setScript(memrec: pointer; script: pchar): BOOL; stdcall;
function ce_memrec_freeze(memrec: pointer; direction: integer): BOOL; stdcall;
function ce_memrec_unfreeze(memrec: pointer): BOOL; stdcall;
function ce_memrec_setColor(memrec: pointer; color: TColor): BOOL; stdcall;
function ce_memrec_appendtoentry(memrec1: pointer; memrec2: pointer): BOOL; stdcall;
function ce_memrec_delete(memrec: pointer): BOOL; stdcall;


implementation

uses MainUnit,MainUnit2,Assemblerunit,disassembler,frmModifyRegistersUnit,
     formsettingsunit, symbolhandler,frmautoinjectunit, manualModuleLoader,
     MemoryRecordUnit;

var plugindisassembler: TDisassembler;

type TFreezeMem_Entry=record
  id: integer;
  address: ptrUint;
  originalbytes: array of byte;
end;

type TFreezeMem = class (tthread)
  private
  public
    addresslist: array of TFreezeMem_Entry;
    interval: integer;
    cs: TCriticalSection;
    constructor create(suspended: boolean);
    procedure execute; override;
end;

procedure TFreezeMem.execute;
var i: integer;
    x: dword;
begin
  while not terminated do
  begin
    cs.Enter;
    for i:=0 to length(addresslist)-1 do
      writeprocessmemory(processhandle,pointer(addresslist[i].address),@addresslist[i].originalbytes[0],length(addresslist[i].originalbytes),x);
    cs.Leave;

    sleep(interval);
  end;
end;

constructor TFreezeMem.create(suspended: boolean);
begin
  cs:=TCriticalSection.create;
  inherited create(suspended);
end;

var FreezeMem: TFreezeMem;

procedure ce_showmessage(s: pchar); stdcall;
begin
  showmessage(s);
end;


{
function ce_getControlName(controlpointer: pointer; objectname: pchar; maxsize: integer):integer; stdcall;
//retrieves the name of a gui object
var name: string;
begin
  result:=0;
  if maxsize=0 then exit;

  try
    name:=tcontrol(controlpointer).Name;
    if maxsize<=length(name) then
      name:=copy(name,1,maxsize-1);

    copymemory(objectname,@name[1],length(name));
  except
    result:=-1;
  end;
end;  }
   {
function addresslist_getcount: integer; stdcall;
begin
  result:=mainform.NumberOfRecords;
end;

function addresslist_additem(newitem: PPlugin0_SelectedRecord): BOOL; stdcall;
begin
//  mainform.addaddress();
end;

function addresslist_getitem(itemnr: integer; item:PPlugin0_SelectedRecord): BOOL; stdcall;
var i: integer;
begin
  result:=false;
  if itemnr>=mainform.NumberOfRecords then exit;

  item.interpretedaddress:=pchar(mainform.memrec[itemnr].interpretableaddress);
  item.address:=mainform.memrec[itemnr].Address;
  item.ispointer:=mainform.memrec[itemnr].IsPointer;

  item.countoffsets:=length(mainform.memrec[itemnr].pointers);
  getmem(item.offsets,  item.countoffsets*4);
  for i:=0 to item.countoffsets-1 do
    item.countoffsets
  item.description:=pchar(mainform.memrec[itemnr].description);

  //mainform.memrec[i]
end;

function addresslist_setitem(itemnr: integer; item:PPlugin0_SelectedRecord): BOOL; stdcall;
begin
end; }


function ce_generateAPIHookScript(address, addresstojumpto, addresstogetnewcalladdress, script: pchar; maxscriptsize: integer): BOOL; stdcall;
var s: tstringlist;
begin

  result:=false;
  s:=tstringlist.create;
  try
    try
      generateAPIHookScript(s,address,addresstojumpto,addresstogetnewcalladdress);

      //now copy the script to the caller
      if (length(s.Text)+1) > maxscriptsize then
      begin
        CopyMemory(script, s.GetText, maxscriptsize)
      end
      else
        CopyMemory(script, s.GetText, length(s.Text)+1);

      script[maxscriptsize-1]:=#0;

      result:=true;
    except

    end;
  finally
    s.free;
  end;
end;

function ce_sym_addressToName(address:ptrUint; name: pchar; maxnamesize: integer):BOOL; stdcall;
var s: string;
begin
  result:=false;
  try
    s:=symhandler.getNameFromAddress(address,true,true);
    if length(s)<maxnamesize then
      copymemory(name,@s[1],length(s))
    else
      copymemory(name,@s[1],maxnamesize);
      
    result:=true;
  except

  end;
end;


function ce_sym_nameToAddress(name: pchar; address: PDWORD):BOOL; stdcall;
var haserror: boolean;
begin
  address^:=symhandler.getAddressFromName(name,false,haserror);
  result:=not haserror;
end;


function ce_reloadsettings:BOOL; stdcall;
begin
  mainunit2.LoadSettingsFromRegistry;
  result:=true;
end;

function ce_getaddressfrompointer(baseaddress: ptrUint; offsetcount: integer; offsets: PDwordArray):dword; stdcall;
var a: ptrUint;
    x: dword;
    i: integer;
begin
  result:=0;
  a:=baseaddress;
  i:=0;
  while (i<offsetcount) do
  begin
    if not readprocessmemory(processhandle,pointer(a),@a,4,x) then exit;

    inc(a,offsets[i]);
    inc(i);
  end;

  result:=a;
end;


function ce_freezemem(address: ptrUint; size: integer):integer; stdcall; //should be obsolete as it has no interpretable address support
var b: array of byte;
    x: dword;
    i,j: integer;
    maxid: integer;
begin
  result:=-1;

  if freezemem=nil then
  begin
    freezemem:=tfreezemem.Create(true);
    freezemem.interval:=mainform.FreezeTimer.Interval;
    freezemem.Resume;
  end;

  setlength(b,size);
  if readprocessmemory(processhandle,pointer(address),@b[0],size,x) then
  begin
    maxid:=1;
    freezemem.cs.Enter;

    for i:=0 to length(freezemem.addresslist)-1 do
      if maxid<=freezemem.addresslist[i].id then maxid:=freezemem.addresslist[i].id+1;

    i:=length(freezemem.addresslist);
    setlength(freezemem.addresslist,i+1);

    freezemem.addresslist[i].id:=maxid;
    freezemem.addresslist[i].address:=address;
    setlength(freezemem.addresslist[i].originalbytes,size);
    for j:=0 to size-1 do
      freezemem.addresslist[i].originalbytes[j]:=b[j];

    freezemem.cs.Leave;

    result:=maxid;
  end;

  setlength(b,0);

end;

function ce_unfreezemem(id: integer):BOOL; stdcall;
var i,j: integer;
begin
  result:=false;
  if freezemem=nil then exit;

  freezemem.cs.Enter;
  for i:=0 to length(freezemem.addresslist)-1 do
    if freezemem.addresslist[i].id=id then
    begin
      setlength(freezemem.addresslist[i].originalbytes,0);
      for j:=i to length(freezemem.addresslist)-2 do
        freezemem.addresslist[j]:=freezemem.addresslist[j+1];

      setlength(freezemem.addresslist,length(freezemem.addresslist)-1);
      result:=true;
      break;
    end;
    
  freezemem.cs.Leave;
end;

function ce_processlist(listbuffer: pchar; listsize: integer):BOOL; stdcall;
var SNAPHandle: THandle;
    ProcessEntry: ProcessEntry32;
    Check: Boolean;
    s: string;
    s2: string;
begin
  result:=true;
  s2:='';

  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  try
    If SnapHandle>0 then
    begin
      ProcessEntry.dwSize:=SizeOf(ProcessEntry);
      Check:=Process32First(SnapHandle,ProcessEntry);
      while check do
      begin
        if processentry.th32ProcessID<>0 then
        begin
          s:=inttohex(processentry.th32ProcessID,8)+'-'+ExtractFilename(processentry.szExeFile);
          if (length(s)+2)<(listsize-length(s2)) then
          begin
            if s2='' then s2:=s else s2:=s2+#13#10+s;
          end
          else
          begin
            result:=false;
            break;
          end;
        end;

        check:=Process32Next(SnapHandle,ProcessEntry);
      end;

      if (listsize-length(s2))>=0 then
      begin
        copymemory(listbuffer,@s2[1],length(s2));
        listbuffer[length(s2)]:=#0;
      end;
    end;
  finally
    closehandle(snaphandle);
  end;
end;

function ce_InjectDLL(dllname: pchar; functiontocall: pchar):BOOL; stdcall;
var dllname_s,functiontocall_s: string;
begin
  try
    dllname_s:=dllname;
    functiontocall_s:=functiontocall;
    injectdll(dllname_s,functiontocall_s);
    symhandler.reinitialize;
    symhandler.waitforsymbolsloaded;
    result:=true;
  except
    result:=false;
  end;
end;

function ce_ChangeRegistersAtAddress(address:ptrUint; changereg: pregistermodificationBP):BOOL; stdcall;
var frmModifyRegisters:tfrmModifyRegisters;
begin
  result:=false;
 { if (formsettings.cbKdebug.checked) and (debuggerthread3<>nil) and (debuggerthread2.nrofbreakpoints=4) then raise exception.Create('You have reached the maximum of 4 debugregs. Disable at least one breakpoint first'); //all spots filled up}

  if (not formsettings.cbKdebug.checked) then
    if (not startdebuggerifneeded) then exit;
    
  frmModifyRegisters:=tfrmModifyRegisters.create(nil,address);

  with frmModifyRegisters do
  begin
    if changereg.change_eax then
    begin
      checkbox1.checked:=true;
      edit1.Text:=inttohex(changereg.new_eax,8);
    end;

    if changereg.change_ebx then
    begin
      checkbox2.checked:=true;
      edit2.Text:=inttohex(changereg.new_ebx,8);
    end;

    if changereg.change_ecx then
    begin
      checkbox3.checked:=true;
      edit3.Text:=inttohex(changereg.new_ecx,8);
    end;
    if changereg.change_edx then
    begin
      checkbox4.checked:=true;
      edit4.Text:=inttohex(changereg.new_edx,8);
    end;
    if changereg.change_esi then
    begin
      checkbox5.checked:=true;
      edit5.Text:=inttohex(changereg.new_esi,8);
    end;
    if changereg.change_edi then
    begin
      checkbox6.checked:=true;
      edit6.Text:=inttohex(changereg.new_edi,8);
    end;
    if changereg.change_ebp then
    begin
      checkbox7.checked:=true;
      edit7.Text:=inttohex(changereg.new_ebp,8);
    end;
    if changereg.change_esp then
    begin
      checkbox8.checked:=true;
      edit8.Text:=inttohex(changereg.new_esp,8);
    end;
    if changereg.change_eip then
    begin
      checkbox9.checked:=true;
      edit9.Text:=inttohex(changereg.new_eip,8);
    end;

    if changereg.change_cf then
    begin
      checkbox10.checked:=true;
      checkbox16.checked:=changereg.new_cf;
    end;
    if changereg.change_pf then
    begin
      checkbox11.checked:=true;
      checkbox17.checked:=changereg.new_pf;
    end;
    if changereg.change_af then
    begin
      checkbox12.checked:=true;
      checkbox18.checked:=changereg.new_af;
    end;
    if changereg.change_zf then
    begin
      checkbox13.checked:=true;
      checkbox19.checked:=changereg.new_zf;
    end;
    if changereg.change_sf then
    begin
      checkbox14.checked:=true;
      checkbox20.checked:=changereg.new_sf;
    end;
    if changereg.change_of then
    begin
      checkbox15.checked:=true;
      checkbox21.checked:=changereg.new_of;
    end;

  end;

  changereg.address:=address;

  //frmModifyRegisters.Showmodal;
  frmModifyRegisters.Button1.Click;
  //frmModifyRegisters.Free;
       
  result:=true;
end;

function ce_AutoAssemble(s: pchar):BOOL; stdcall;
var script: tstringlist;
begin
  result:=true;
  script:=tstringlist.create;
  try
    script.Text:=s;
    autoassemble(script,false);
  except
    result:=false;
  end;
  script.free;

end;

function ce_GetMainWindowHandle:thandle; stdcall;
begin
  result:=mainform.handle;
end;

function ce_registerfunction(pluginid,functiontype:integer; init: pointer):integer; stdcall;
begin
  result:=pluginhandler.registerfunction(pluginid,functiontype,init);
end;

function ce_unregisterfunction(pluginid,functionid: integer): BOOL; stdcall;
begin
  result:=pluginhandler.unregisterfunction(pluginid,functionid);
end;


function ce_assembler(address:ptrUint; instruction: pchar; output: PByteArray; maxlength: integer; actualsize: pinteger):BOOL; stdcall;
var x: tassemblerbytes;
    i: integer;
begin
  setlength(x,0);
  assemble(instruction,address,x);
  if length(x)>maxlength then
  begin
    setlasterror(ERROR_NOT_ENOUGH_MEMORY);
    result:=false;
    exit;
  end;

  for i:=0 to length(x)-1 do
    output[i]:=x[i];

  actualsize^:=length(x);

  setlength(x,0);

  setlasterror(NO_ERROR);
  result:=true;
end;

function ce_disassembler(address: ptrUint; output: pchar; maxsize: integer): BOOL; stdcall;
var s: string;
    p: pchar;
    extra: string;
begin
  s:=plugindisassembler.disassemble(address,extra);
  if length(s)>maxsize then
  begin
    setlasterror(ERROR_NOT_ENOUGH_MEMORY);
    result:=false;
    exit;
  end;

  p:=pchar(s);
  StrCopy(output,p);
  result:=true;
end;

function ce_disassemble(address: pptrUint; output: pchar; maxsize: integer): BOOL; stdcall;
var s: string;
    p: pchar;
    extra: string;
    a: ptrUint;
begin
  a:=address^;
  s:=plugindisassembler.disassemble(a,extra);
  if length(s)>maxsize then
  begin
    setlasterror(ERROR_NOT_ENOUGH_MEMORY);
    result:=false;
    exit;
  end;

  address^:=a;

  p:=pchar(s);
  StrCopy(output,p);
  result:=true;
end;

function ce_previousOpcode(address:ptrUint): ptrUint; stdcall;
begin
  result:=previousopcode(address);
end;

function ce_nextOpcode(address:ptrUint): ptrUint; stdcall;
var x: string;
begin
  plugindisassembler.disassemble(address, x);
  result:=address;
end;

function ce_loadModule(modulepath: pchar; exportlist: pchar; maxsize: pinteger): BOOL; stdcall;
var
  ml: TModuleLoader;
  s: string;
  i: integer;
begin
  result:=false;
  try
    ml:=TModuleLoader.create(modulepath);
    if ml.loaded then
    begin
      s:=ml.Exporttable.Text;
      i:=min(maxsize^, length(s));
      CopyMemory(exportlist, @s[1], i);
      maxsize^:=i;

      result:=true;
    end;

  except
  end;
end;

function pluginsync(func: TPluginFunc; parameters: pointer): pointer; stdcall;
{
Pluginsync calls the required function from the mainthread and returns a pointer.
This pointer can be an allocated block of data, or just a result, depending on the function (boolean might be stored as 0 or 1)
}
begin
  result:=pointer(SendMessage(mainform.handle, wm_pluginsync, ptruint(@func), ptruint(parameters) ));
end;

function ce_createTableEntry2(parameters: pointer): pointer;
begin
  result:=MainForm.addresslist.addaddress('Plugin Address', '0',[],0,vtDword);
end;

function ce_createTableEntry: pointer; stdcall;
begin
  if GetCurrentThreadId=MainThreadID then
    result:=ce_createTableEntry2(nil)
  else
    result:=pluginsync(ce_createTableEntry2, nil);
end;

function ce_getTableEntry2(description: pointer): pointer;
begin
  result:=mainform.addresslist.findRecordWithDescription(pchar(description));
end;

function ce_getTableEntry(description: pchar): pointer; stdcall;
begin
  if GetCurrentThreadId=MainThreadID then
    result:=ce_getTableEntry2(description)
  else
    result:=pluginsync(ce_getTableEntry2, description);
end;

type
  TSetDescription=record
    memrec: tmemoryrecord;
    description: pchar;
  end;
  PSetDescription=^TSetDescription;

function ce_memrec_setDescription2(params: pointer): pointer;
var
  memrec: TMemoryRecord;
begin
  result:=nil;
  memrec:=PSetDescription(params).memrec;

  try
    if (memrec is TMemoryRecord) then
    begin
      memrec.Description:=PSetDescription(params).description;
      memrec.refresh;
    end;

    result:=pointer(1);
  except
  end;
end;

function ce_memrec_setDescription(memrec: pointer; description: pchar): BOOL; stdcall;
var d: TsetDescription;
begin
  d.memrec:=memrec;
  d.description:=description;
  if GetCurrentThreadId=MainThreadID then
    result:=ce_memrec_setDescription2(@d)<>nil
  else
    result:=pluginsync(ce_memrec_setDescription2, @d)<>nil;
end;

function ce_memrec_getDescription(memrec: pointer): pchar; stdcall;
var m: TMemoryRecord;
begin
  result:=nil;
  try
    m:=memrec;
    if (m is TMemoryRecord) then
      result:=pchar(m.Description);
  except
  end;

end;

function ce_memrec_getAddress(memrec: pointer; address: pptruint; offsets: PDwordArray; maxoffsets: integer; neededOffsets: pinteger): BOOL; stdcall;
var m: TMemoryRecord;
  i: integer;
begin
  result:=false;
  try
    m:=memrec;
    if (m is TMemoryRecord) then
    begin

      if neededoffsets<>nil then
        neededOffsets^:=length(m.pointeroffsets);

      if offsets<>nil then
        for i:=0 to maxoffsets-1 do
          offsets[i]:=m.pointeroffsets[i];

      if address<>nil then
        address^:=m.GetRealAddress;

      result:=true;
    end;
  except
  end;
end;

type
  TSetAddress=record
    memrec: tmemoryrecord;
    address: pchar;
    offsets: pdwordarray;
    offsetcount: integer;
  end;
  PSetAddress=^TSetAddress;

function ce_memrec_setAddress2(params: pointer): pointer;
var p: PSetAddress;
  i: integer;
begin
  result:=nil;
  try
    p:=params;
    if (p.memrec is TMemoryRecord) then
    begin
      setlength(p.memrec.pointeroffsets, p.offsetcount);

      p.memrec.interpretableaddress:=p.address;
      for i:=0 to p.offsetcount-1 do
        p.memrec.pointeroffsets[i]:=p.offsets[i];

      result:=pointer(1);

      p.memrec.ReinterpretAddress;
      p.memrec.refresh;
    end;
  except
  end;
end;

function ce_memrec_setAddress(memrec: pointer; address: pchar; offsets: PDwordArray; offsetcount: integer): BOOL; stdcall;
var
  a: TSetAddress;
begin
  result:=false;
  a.memrec:=memrec;
  a.address:=address;
  a.offsets:=offsets;
  a.offsetcount:=offsetcount;
  if GetCurrentThreadId=MainThreadID then
    result:=ce_memrec_setAddress2(@a)<>nil
  else
    result:=pluginsync(ce_memrec_setAddress2, @a)<>nil;
end;

function ce_memrec_getType(memrec: pointer): integer; stdcall;
var m: TMemoryRecord;
begin
  result:=-1;
  try
    m:=memrec;
    if (m is TMemoryRecord) then
      result:=integer(m.VarType);
  except
  end;
end;

type
  TSetType=record
    memrec: TMemoryRecord;
    vtype: integer;
  end;
  PSetType=^TSetType;

function ce_memrec_setType2(params: pointer): pointer;
var p: PSetType;
begin
  result:=nil;
  p:=params;
  try
    if p.memrec is TMemoryRecord then
    begin
      p.memrec.VarType:=Tvariabletype(p.vtype);
      p.memrec.refresh;

      result:=pointer(1); //still here
    end;
  except
  end;
end;

function ce_memrec_setType(memrec: pointer; vtype: integer): BOOL; stdcall;
var p: TSetType;
begin
  result:=false;
  p.memrec:=memrec;
  p.vtype:=vtype;
  if GetCurrentThreadId=MainThreadID then
    result:=ce_memrec_setType2(@p)<>nil
  else
    result:=pluginsync(ce_memrec_setType2, @p)<>nil;
end;

function ce_memrec_getValue(memrec: pointer; value: pchar; maxsize: integer): BOOL; stdcall;
var m: TMemoryRecord;
  v: string;
  i: integer;
begin
  result:=false;
  try
    m:=memrec;
    if (m is TMemoryRecord) then
    begin
      v:=m.Value;

      for i:=0 to min(length(v) , maxsize-1) do
        value[i]:=v[i+1];

      if maxsize>0 then
        value[maxsize-1]:=#0;

      if maxsize>length(v) then
        value[length(v)]:=#0;

      result:=true;
    end;

    result:=true;
  except
  end;
end;

function ce_memrec_setValue(memrec: pointer; value: pchar): BOOL; stdcall;
var m: TMemoryRecord;
begin
  result:=false;
  try
    m:=memrec;
    if (m is TMemoryRecord) then
    begin
      m.Value:=value;
      result:=true;
    end;
  except
  end;

end;


function ce_memrec_getScript(memrec: pointer): pchar; stdcall;
var m: TMemoryRecord;
begin
  result:=nil;
  try
    m:=memrec;
    if (m is TMemoryRecord) then
    begin
      if m.VarType=vtAutoAssembler then
        result:=pchar(m.AutoAssemblerData.script.text);
    end;
  except
  end;
end;

function ce_memrec_setScript(memrec: pointer; script: pchar): BOOL; stdcall;
var m: TMemoryRecord;
begin
  result:=false;
  try
    m:=memrec;
    if (m is TMemoryRecord) then
    begin
      if m.VarType=vtAutoAssembler then
      begin
        m.AutoAssemblerData.script.text:=script;
        result:=true;
      end;

    end;
  except
  end;
end;


type
  TFreezeparams=record
    memrec: TMemoryRecord;
    direction: integer;
  end;
  PFreezeParams=^Tfreezeparams;

function ce_memrec_freeze2(params: pointer): pointer;
var m: TMemoryRecord;
  p: PFreezeParams;
begin
  p:=params;


  result:=nil;
  try
    m:=p.memrec;
    if (m is TMemoryRecord) then
    begin
      m.active:=true;

      m.allowIncrease:=p.direction=1;
      m.allowDecrease:=p.direction=2;
      result:=pointer(1);

      m.refresh;
    end;

  except
  end;
end;

function ce_memrec_freeze(memrec: pointer; direction: integer): BOOL; stdcall;
var p: TFreezeparams;
begin
  p.memrec:=memrec;
  p.direction:=direction;
  if GetCurrentThreadId=MainThreadID then
    result:=ce_memrec_freeze2(@p)<>nil
  else
    result:=pluginsync(ce_memrec_freeze2, @p)<>nil;

end;

function ce_memrec_unfreeze2(params: pointer): pointer;
var m: TMemoryRecord;
begin
  result:=nil;
  try
    m:=params;
    if (m is TMemoryRecord) then
    begin
      m.active:=false;
      m.refresh;
      result:=pointer(1);
    end;

  except
  end;
end;

function ce_memrec_unfreeze(memrec: pointer): BOOL; stdcall;
begin
  if GetCurrentThreadId=MainThreadID then
    result:=ce_memrec_unfreeze2(memrec)<>nil
  else
    result:=pluginsync(ce_memrec_unfreeze2, memrec)<>nil;
end;


type
  TSetColorparams=record
    memrec: TMemoryRecord;
    color: TColor;
  end;
  PSetColorparams=^TSetColorparams;

function ce_memrec_setColor2(params: pointer): pointer;
var m: TMemoryRecord;
  p: PSetColorparams;
begin
  p:=params;


  result:=nil;
  try
    m:=p.memrec;
    if (m is TMemoryRecord) then
    begin
      m.Color:=p.color;
      m.refresh;
      result:=pointer(1);
    end;

  except
  end;
end;

function ce_memrec_setColor(memrec: pointer; color: TColor): BOOL; stdcall;
var p: TSetColorparams;
begin
  p.memrec:=memrec;
  p.color:=color;
  if GetCurrentThreadId=MainThreadID then
    result:=ce_memrec_setColor2(@p)<>nil
  else
    result:=pluginsync(ce_memrec_setColor2, @p)<>nil;

end;



type
  TAppendParams=record
    memrec1: TMemoryRecord;
    memrec2: TMemoryRecord;
  end;
  PAppendParams=^TAppendParams;

function ce_memrec_appendToEntry2(params: pointer): pointer;
var m1,m2: TMemoryRecord;
  p: PAppendParams;
begin
  p:=params;


  result:=nil;
  try
    m1:=p.memrec1;
    m2:=p.memrec2;
    if (m1 is TMemoryRecord) and (m2 is TMemoryRecord) then
    begin
      m1.treenode.MoveTo(m2.treenode, naAddChild);
      result:=pointer(1);
    end;

  except
  end;
end;

function ce_memrec_appendToEntry(memrec1: pointer; memrec2: pointer): BOOL; stdcall;
var p: TAppendParams;
begin
  p.memrec1:=memrec1;
  p.memrec2:=memrec2;
  if GetCurrentThreadId=MainThreadID then
    result:=ce_memrec_appendToEntry2(@p)<>nil
  else
    result:=pluginsync(ce_memrec_appendToEntry2, @p)<>nil;

end;




function ce_memrec_delete2(params: pointer): pointer;
var m: TMemoryRecord;
begin
  result:=nil;
  try
    m:=params;
    if (m is TMemoryRecord) then
      m.Free;
  except
  end;
end;

function ce_memrec_delete(memrec: pointer): BOOL; stdcall;
begin
  if GetCurrentThreadId=MainThreadID then
    result:=ce_memrec_delete2(memrec)<>nil
  else
    result:=pluginsync(ce_memrec_delete2, memrec)<>nil;
end;


initialization
  plugindisassembler:=TDisassembler.create;
  plugindisassembler.showsymbols:=false;
  plugindisassembler.showmodules:=false;
  plugindisassembler.isdefault:=false;


finalization
  if plugindisassembler<>nil then
    plugindisassembler.free;
end.




