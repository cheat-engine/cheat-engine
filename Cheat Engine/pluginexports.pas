unit pluginexports;

interface

uses StdCtrls,sysutils,Controls, SyncObjs,dialogs,windows,classes,autoassembler,
     cefuncproc,newkernelhandler,debugger,debugger2,tlhelp32, plugin;

procedure ce_showmessage(s: pchar); stdcall;
function ce_registerfunction(pluginid,functiontype:integer; init: pointer):integer; stdcall;
function ce_unregisterfunction(pluginid,functionid: integer): BOOL; stdcall;
function ce_AutoAssemble(s: pchar):BOOL; stdcall;
function ce_GetMainWindowHandle:thandle; stdcall;
function ce_ChangeRegistersAtAddress(address:dword; changereg: pregistermodificationBP):BOOL; stdcall;

function ce_assembler(address:dword; instruction: pchar; output: PByteArray; maxlength: integer; actualsize: pinteger):BOOL; stdcall;
function ce_disassembler(address: dword; output: pchar; maxsize: integer): BOOL; stdcall;
function ce_InjectDLL(dllname: pchar; functiontocall: pchar):BOOL; stdcall;
function ce_processlist(listbuffer: pchar; listsize: integer):BOOL; stdcall;
function ce_fixmem:BOOL; stdcall;
function ce_reloadsettings:BOOL; stdcall;
function ce_getaddressfrompointer(baseaddress: dword; offsetcount: integer; offsets: PDwordArray):dword; stdcall;

function ce_freezemem(address: dword; size: integer):integer; stdcall;
function ce_unfreezemem(id: integer):BOOL; stdcall;

//version 2:
function ce_sym_addressToName(address:dword; name: pchar; maxnamesize: integer):BOOL; stdcall;
function ce_sym_nameToAddress(name: pchar; address: PDWORD):BOOL; stdcall;
function ce_generateAPIHookScript(address, addresstojumpto, addresstogetnewcalladdress, script: pchar; maxscriptsize: integer): BOOL; stdcall;
  {
function addresslist_getcount: integer; stdcall;
function addresslist_additem(newitem: PPlugin0_SelectedRecord): BOOL; stdcall;
function addresslist_getitem(itemnr: integer; item:PPlugin0_SelectedRecord): BOOL; stdcall;
function addresslist_setitem(itemnr: integer; item:PPlugin0_SelectedRecord): BOOL; stdcall;
       }

implementation

uses mainunit,mainunit2,Assemblerunit,disassembler,frmModifyRegistersUnit,
     formsettingsunit,undochanges, symbolhandler, frmautoinjectunit;

type TFreezeMem_Entry=record
  id: integer;
  address: dword;
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

      script[maxscriptsize]:=#0;

      result:=true;
    except

    end;
  finally
    s.free;
  end;
end;

function ce_sym_addressToName(address:dword; name: pchar; maxnamesize: integer):BOOL; stdcall;
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
begin
  result:=false;
  try
    address^:=symhandler.getAddressFromName(name);
    result:=true;
  except

  end;
end;


function ce_reloadsettings:BOOL; stdcall;
begin
  mainunit2.LoadSettingsFromRegistry;
  result:=true;
end;

function ce_getaddressfrompointer(baseaddress: dword; offsetcount: integer; offsets: PDwordArray):dword; stdcall;
var a: dword;
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


function ce_freezemem(address: dword; size: integer):integer; stdcall;
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

function ce_fixmem:BOOL; stdcall;
begin
  if formsettings.cbUndoMemoryChanges.checked then
  begin
    CheckForChanges;
    result:=true;
  end
  else
    result:=false;
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

function ce_ChangeRegistersAtAddress(address:dword; changereg: pregistermodificationBP):BOOL; stdcall;
var frmModifyRegisters:tfrmModifyRegisters;
begin
  result:=false;
  if (formsettings.cbKdebug.checked) and (debuggerthread2<>nil) and (debuggerthread2.nrofbreakpoints=4) then raise exception.Create('You have reached the maximum of 4 debugregs. Disable at least one breakpoint first'); //all spots filled up

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


function ce_assembler(address:dword; instruction: pchar; output: PByteArray; maxlength: integer; actualsize: pinteger):BOOL; stdcall;
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

function ce_disassembler(address: dword; output: pchar; maxsize: integer): BOOL; stdcall;
var s: string;
    p: pchar;
begin
  s:=disassemble(address);
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

end.


