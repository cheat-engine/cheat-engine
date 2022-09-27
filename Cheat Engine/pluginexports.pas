unit pluginexports;

{$MODE Delphi}

interface

uses {$ifdef darwin}macport,macportdefines,{$endif}
     {$ifdef windows}jwawindows, windows,{$endif}
     ExtCtrls , comctrls, Graphics, forms, StdCtrls,sysutils,Controls,
     SyncObjs,dialogs,LCLIntf,classes,autoassembler,
     CEFuncProc,NewKernelHandler,CEDebugger,KernelDebugger, plugin, math,
     debugHelper, debuggertypedefinitions, typinfo, ceguicomponents, strutils,
     commonTypeDefs, luahandler, lua, betterControls;

type TPluginFunc=function(parameters: pointer): pointer;
function pluginsync(func: TPluginFunc; parameters: pointer): pointer; stdcall;

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
function ce_sym_nameToAddress(name: pchar; address: PPtrUInt):BOOL; stdcall;
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
function ce_memrec_isSelected(memrec: pointer): BOOL; stdcall;
function ce_memrec_isFrozen(memrec: pointer): BOOL; stdcall;
function ce_memrec_freeze(memrec: pointer; direction: integer): BOOL; stdcall;
function ce_memrec_unfreeze(memrec: pointer): BOOL; stdcall;
function ce_memrec_setColor(memrec: pointer; color: TColor): BOOL; stdcall;
function ce_memrec_appendtoentry(memrec1: pointer; memrec2: pointer): BOOL; stdcall;
function ce_memrec_delete(memrec: pointer): BOOL; stdcall;
function ce_getProcessIDFromProcessName(name: pchar): DWORD; stdcall;
function ce_openProcess(pid: dword): BOOL; stdcall;
function ce_debugProcess(debuggerinterface: integer): BOOL; stdcall;
procedure ce_pause; stdcall;
procedure ce_unpause; stdcall;

function ce_debug_setBreakpoint(address: ptruint; size: integer; trigger: TBreakpointTrigger): BOOL; stdcall;
function ce_debug_removeBreakpoint(address: ptruint): BOOL; stdcall;
function ce_debug_continueFromBreakpoint(ContinueOption: TContinueOption): BOOL; stdcall;

procedure ce_closeCE; stdcall;
procedure ce_hideAllCEWindows; stdcall;
procedure ce_unhideMainCEwindow; stdcall;
function ce_createForm(visible: boolean): pointer; stdcall;
procedure ce_form_centerScreen(f: pointer); stdcall;
procedure ce_form_hide(f: pointer); stdcall;
procedure ce_form_show(f: pointer); stdcall;
procedure ce_form_onClose(frm: pointer; f: pointer); stdcall;
procedure ce_form_onCloseLua(frm: pointer; f: integer);

function ce_createPanel(owner: pointer): pointer; stdcall;
function ce_createGroupBox(owner: pointer): pointer; stdcall;
function ce_createButton(owner: pointer): pointer; stdcall;
function ce_createImage(owner: pointer): pointer; stdcall;
function ce_image_loadImageFromFile(image: pointer; filename: pchar): BOOL; stdcall;
procedure ce_image_transparent(image: pointer; transparent: boolean); stdcall;
procedure ce_image_stretch(image: pointer; stretch: boolean); stdcall;

function ce_createLabel(owner: pointer): pointer; stdcall;
function ce_createEdit(owner: pointer): pointer; stdcall;
function ce_createMemo(owner: pointer): pointer; stdcall;
function ce_createTimer(owner: pointer): pointer; stdcall;
procedure ce_timer_setInterval(timer: pointer; interval: integer); stdcall;
procedure ce_timer_onTimer(t: pointer; f: pointer); stdcall;
procedure ce_timer_onTimerLua(t: pointer; f: integer);
procedure ce_control_setCaption(control: pointer; caption: pchar); stdcall;
function ce_control_getCaption(control: pointer; caption: pchar; maxsize: integer): BOOL; stdcall;
procedure ce_control_setPosition(control: pointer; x,y: integer); stdcall;
function ce_control_getX(control: pointer): integer; stdcall;
function ce_control_getY(control: pointer): integer; stdcall;
procedure ce_control_setSize(control: pointer; width,height: integer); stdcall;
function ce_control_getWidth(control: pointer): integer; stdcall;
function ce_control_getHeight(control: pointer): integer; stdcall;
procedure ce_control_setAlign(control: pointer; align: integer); stdcall;
procedure ce_control_onClick(c: pointer; f: pointer); stdcall;
procedure ce_control_onClickLua(c: pointer; f: integer); stdcall;

procedure ce_object_destroy(o: pointer); stdcall;
function ce_messageDialog(message: pchar; messagetype: integer; buttoncombination: integer): integer; stdcall;
function ce_messageDialog_Lua(message: pchar; messagetype: integer; buttoncombination: TMsgDlgButtons): integer;
function ce_speedhack_setSpeed(speed: single): BOOL; stdcall;

function ce_getAutoAttachList: pointer; stdcall;
function ce_stringlist_getCount(c: pointer): integer; stdcall;
procedure ce_stringlist_add(c: pointer; s: pchar); stdcall;
procedure ce_stringlist_remove(c: pointer; s: pchar); stdcall;

function ce_createProcess(path,params: pchar; debug, breakonentry: BOOL): BOOL; stdcall;

//ce6.1
function ce_getPropertylist(c: tobject): pointer; stdcall;
function ce_setProperty(c: tobject; propertyname: pchar; value: pchar): BOOL; stdcall;
function ce_getProperty(c: tobject; propertyname: pchar; value: pchar; maxsize: integer): integer; stdcall;


//7.1
function plugin_checksynchronize(timeout: integer):boolean; stdcall;
procedure plugin_processmessages; stdcall;
function plugin_getluastate: Plua_State; stdcall;

implementation

uses MainUnit,MainUnit2, AdvancedOptionsUnit, Assemblerunit,disassembler,
     frmModifyRegistersUnit, formsettingsunit, symbolhandler,frmautoinjectunit,
     {$ifdef windows}manualModuleLoader,{$endif} MemoryRecordUnit, MemoryBrowserFormUnit,
     ProcessHandlerUnit, ProcessList, BreakpointTypeDef;

resourcestring
  rsLoadModuleFailed = 'LoadModule failed';
  rsPluginAddress = 'Plugin Address';

var
  plugindisassembler: TDisassembler;


type TNotifyCall2=procedure(sender: TObject); stdcall;

type
  TComponentFunctionHandlerClass=class //just a handler class for functions of type object
  private
    lowestfreetag: integer;
    Components: array of record
      Component: TObject;
      OnClick: TNotifyCall2;
      OnClose: TNotifyCall2;
      OnTimer: TNotifyCall2;
      LuaOnClick: integer;
      LuaOnClose: integer;
      LuaOnTimer: integer;
    end;
    procedure OnClick(sender: TObject);
    procedure OnClose(Sender: TObject; var Action: TCloseAction);
    procedure OnTimer(Sender: TObject);
  public
    function inputComponent(c: TComponent): integer;
    procedure removeComponent(c: TComponent);
    procedure DefaultOnClose(Sender: TObject; var Action: TCloseAction);

    procedure setOnClick(control: Tcontrol; functiontocall: pointer; luafunction: integer);
    procedure setOnClose(control: TForm; functiontocall: pointer; luafunction: integer);
    procedure setOnTimer(control: TTimer; functiontocall: pointer; luafunction: integer);
    constructor create;
end;
var ComponentFunctionHandlerClass: TComponentFunctionHandlerClass;

constructor TComponentFunctionHandlerClass.create;
var i: integer;
begin
  lowestfreetag:=0;
  setlength(components,16);

  for i:=0 to 15 do
    components[i].component:=nil;
end;

procedure TComponentFunctionHandlerClass.removeComponent(c: TComponent);
begin
  if c<>nil then
  begin
    if lowestfreetag>c.tag then
      lowestfreetag:=c.tag;

    components[c.Tag].Component:=nil;
  end

end;

function TComponentFunctionHandlerClass.inputComponent(c: TComponent): integer;
var i,j: integer;
begin
  //check in the list if there is a free spot
  for i:=lowestfreetag to length(Components)-1 do
    if Components[i].Component=nil then
    begin
      Components[i].component:=c;
      c.tag:=i;
      result:=i;
      if lowestfreetag>=i then
        lowestfreetag:=i+1;
      exit;
    end;

  //still here so no free spot, allocate new room and place it there
  j:=length(components);
  setlength(components,length(components)*2);
  for i:=j to length(components)-1 do
    components[i].component:=nil;

  components[j].component:=c;
  c.tag:=j;
  result:=j;
  if lowestfreetag>=j then
    lowestfreetag:=j+1;
end;

procedure TComponentFunctionHandlerClass.OnClick(sender: TObject);
begin
  if assigned(components[TControl(sender).tag].onclick) then
    components[TControl(sender).tag].onclick(sender)
  else
    LUA_onNotify(components[TControl(sender).tag].LuaOnclick, sender);     //lua call
end;

procedure TComponentFunctionHandlerClass.OnClose(Sender: TObject; var Action: TCloseAction);
begin
  if assigned(components[TControl(sender).tag].onClose) then
    components[TControl(sender).tag].onClose(sender)
  else
    LUA_onNotify(components[TControl(sender).tag].LuaOnClose, sender);     //lua call

  action:=caFree;
end;

procedure TComponentFunctionHandlerClass.DefaultOnClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=cafree;
end;

procedure TComponentFunctionHandlerClass.OnTimer(Sender: TObject);
begin
  if assigned(components[TControl(sender).tag].onTimer) then
    components[TControl(sender).tag].onTimer(sender)
  else
    LUA_onNotify(components[TControl(sender).tag].LuaOnTimer, sender);     //lua call
end;




procedure TComponentFunctionHandlerClass.setOnClick(control: TControl; functiontocall: pointer; luafunction: integer);
begin
  control.OnClick:=OnClick;
  if functiontocall<>nil then
    components[control.tag].OnClick:=functiontocall
  else
  begin
    components[control.tag].OnClick:=nil;
    components[control.tag].LuaOnClick:=luafunction
  end;

end;

procedure TComponentFunctionHandlerClass.setOnClose(control: TForm; functiontocall: pointer; luafunction: integer);
begin
  control.OnClose:=OnClose;
  if functiontocall<>nil then
    components[control.tag].OnClose:=functiontocall
  else
  begin
    components[control.tag].OnClose:=nil;
    components[control.tag].LuaOnClose:=luafunction
  end;

end;

procedure TComponentFunctionHandlerClass.setOnTimer(control: TTimer; functiontocall: pointer; luafunction: integer);
begin
  control.OnTimer:=OnTimer;
  control.enabled:=true;

  if functiontocall<>nil then
    components[control.tag].OnTimer:=functiontocall
  else
  begin
    components[control.tag].OnTimer:=nil;
    components[control.tag].LuaOnTimer:=luafunction
  end;

end;


//---------------------------------

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
    x: ptruint;
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

function ce_showmessage2(params: pointer): pointer;
begin

  if params<>nil then
  begin
    showmessage(pchar(params));
    result:=pointer(1);
  end
  else
    result:=pointer(0);
end;

procedure ce_showmessage(s: pchar); stdcall;
begin
  pluginsync(ce_showmessage2, s);

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
var
  s: string;
  l: integer;
begin
  result:=false;
  try
    s:=symhandler.getNameFromAddress(address,true,true, false);

    l:=min(maxnamesize-1, length(s));
    copymemory(name,@s[1],l);

    name[l]:=#0;
      
    result:=true;
  except

  end;
end;


function ce_sym_nameToAddress(name: pchar; address: PPtrUInt):BOOL; stdcall;
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
var a,b: ptrUint;
    x: ptruint;
    i: integer;
begin
  result:=0;
  a:=baseaddress;
  i:=0;
  while (i<offsetcount) do
  begin
    b:=0;
    if not readprocessmemory(processhandle,pointer(a),@b,processhandler.pointersize,x) then exit;
    a:=b;

    inc(a,offsets[i]);
    inc(i);
  end;

  result:=a;
end;


function ce_freezemem(address: ptrUint; size: integer):integer; stdcall; //should be obsolete as it has no interpretable address support
var b: array of byte;
    x: ptruint;
    i,j: integer;
    maxid: integer;
begin
  result:=-1;

  if freezemem=nil then
  begin
    freezemem:=tfreezemem.Create(true);
    freezemem.interval:=mainform.FreezeTimer.Interval;
    freezemem.start;
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

    {$ifdef darwin}
    pl: tstringstream;
    {$endif}
begin
  {$ifdef windows}
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
  {$else}
  result:=false;
  {$endif}


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
    on e:exception do
    begin
      outputdebugstring('ce_InjectDLL('''+dllname+''','''+functiontocall+''') error: '+e.Message);
      result:=false;
    end;

  end;
end;

function ce_ChangeRegistersAtAddress(address:ptrUint; changereg: pregistermodificationBP):BOOL; stdcall;
var frmModifyRegisters:tfrmModifyRegisters;
begin
  result:=false;
 { if (formsettings.cbKdebug.checked) and (debuggerthread3<>nil) and (debuggerthread2.nrofbreakpoints=4) then raise exception.Create('You have reached the maximum of 4 debugregs. Disable at least one breakpoint first'); //all spots filled up}

  if (not startdebuggerifneeded) then exit;
    
  frmModifyRegisters:=tfrmModifyRegisters.create(nil,address);

  with frmModifyRegisters do
  begin
    if changereg.change_eax then
      edtEAX.Text:=inttohex(changereg.new_eax,8);

    if changereg.change_ebx then
      edtEbX.Text:=inttohex(changereg.new_ebx,8);

    if changereg.change_ecx then
      edtECX.Text:=inttohex(changereg.new_ecx,8);

    if changereg.change_edx then
      edtEDX.Text:=inttohex(changereg.new_edx,8);

    if changereg.change_esi then
      edtESI.Text:=inttohex(changereg.new_esi,8);

    if changereg.change_edi then
      edtEDI.Text:=inttohex(changereg.new_edi,8);

    if changereg.change_ebp then
      edtEBP.Text:=inttohex(changereg.new_ebp,8);

    if changereg.change_esp then
      edtESP.Text:=inttohex(changereg.new_esp,8);

    if changereg.change_eip then
      edtEIP.Text:=inttohex(changereg.new_eip,8);
    {$ifdef cpu64}
    if changereg.change_r8 then
      edtR8.Text:=inttohex(changereg.new_r8,8);

    if changereg.change_r9 then
      edtR9.Text:=inttohex(changereg.new_r9,8);

    if changereg.change_r10 then
      edtR10.Text:=inttohex(changereg.new_r10,8);

    if changereg.change_r11 then
      edtR11.Text:=inttohex(changereg.new_r11,8);

    if changereg.change_r12 then
      edtR12.Text:=inttohex(changereg.new_r12,8);

    if changereg.change_r13 then
      edtR13.Text:=inttohex(changereg.new_r13,8);

    if changereg.change_r14 then
      edtR14.Text:=inttohex(changereg.new_r14,8);

    if changereg.change_r15 then
      edtR15.Text:=inttohex(changereg.new_r15,8);
    {$endif}


    if changereg.change_cf then
      cbCF.checked:=changereg.new_cf;

    if changereg.change_pf then
      cbPF.checked:=changereg.new_pf;

    if changereg.change_af then
      cbAF.checked:=changereg.new_af;

    if changereg.change_zf then
      cbZF.checked:=changereg.new_zf;

    if changereg.change_sf then
      cbSF.checked:=changereg.new_sf;

    if changereg.change_of then
      cbOF.checked:=changereg.new_of;


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
{$ifdef windows}
var
  ml: TModuleLoader;
  s: string;
  i: integer;
  {$endif}
begin
  {$ifdef windows}
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
    on e: exception do
    begin

      messagebox(0, pchar(e.Message), pchar(rsLoadModuleFailed), MB_OK);
    end;
  end;
  {$else}
  result:=false;
  {$endif}

end;

function pluginsync(func: TPluginFunc; parameters: pointer): pointer; stdcall;
{
Pluginsync calls the required function from the mainthread and returns a pointer.
This pointer can be an allocated block of data, or just a result, depending on the function (boolean might be stored as 0 or 1)
}
begin
  if GetCurrentThreadId=MainThreadID then
    result:=func(parameters)
  else
    result:=pointer(SendMessage(mainform.handle, wm_pluginsync, ptruint(@func), ptruint(parameters) ));
end;

function ce_createTableEntry2(parameters: pointer): pointer;
begin
  result:=MainForm.addresslist.addaddress(rsPluginAddress, '0',[],0,vtDword);
end;

function ce_createTableEntry: pointer; stdcall;
begin
  result:=pluginsync(ce_createTableEntry2, nil);
end;

function ce_getTableEntry2(description: pointer): pointer;
begin
  result:=mainform.addresslist.getRecordWithDescription(pchar(description));
end;

function ce_getTableEntry(description: pchar): pointer; stdcall;
begin
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
        neededOffsets^:=m.offsetCount;

      if address<>nil then
        address^:=m.GetRealAddress;

      if offsets<>nil then
      begin
        for i:=0 to maxoffsets-1 do
          offsets[i]:=m.offsets[i].offset;
      end;



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
      p.memrec.offsetCount:=p.offsetcount;

      p.memrec.interpretableaddress:=p.address;
      for i:=0 to p.offsetcount-1 do
        p.memrec.offsets[i].offset:=p.offsets[i];

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
        if m.AutoAssemblerData.script=nil then
          m.AutoAssemblerData.script:=TStringList.create;

        m.AutoAssemblerData.script.text:=script;
        result:=true;
      end;

    end;
  except
  end;
end;

function ce_memrec_isSelected(memrec: pointer): BOOL; stdcall;
var m: TMemoryRecord;
begin
  result:=false;
  try
    m:=memrec;
    if (m is TMemoryRecord) then
      result:=m.isSelected;
  except
  end;
end;

function ce_memrec_isFrozen(memrec: pointer): BOOL; stdcall;
var m: TMemoryRecord;
begin
  result:=false;
  try
    m:=memrec;
    if (m is TMemoryRecord) then
      result:=m.active;
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

      m2.SetVisibleChildrenState;
    end;

  except
  end;
end;

function ce_memrec_appendToEntry(memrec1: pointer; memrec2: pointer): BOOL; stdcall;
var p: TAppendParams;
begin
  p.memrec1:=memrec1;
  p.memrec2:=memrec2;
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
  result:=pluginsync(ce_memrec_delete2, memrec)<>nil;
end;


function ce_getProcessIDFromProcessName(name: pchar): DWORD; stdcall;
var plist: TStringlist;
  i,j: integer;
  pname: string;
  compareto: string;
  {$IFDEF WINDOWS}
  ProcessListInfo: PProcessListInfo;
  {$ENDIF}

  bestpick: record
    i: integer;
    pos: integer;
  end;
begin
  result:=0;
  if name=nil then exit;
  compareto:=uppercase(name);

  plist:=TStringList.Create;
  try
    GetProcessList(plist);

    for i:=plist.Count-1 downto 0 do  //reverse order because the newest process is at the bottom
    begin
      pname:=plist[i];
      j:=Pos('-', pname);


      pname:=uppercase(copy(pname, j+1,length(pname)));


      //processname found
      if compareto=pname then
      begin
        {$IFDEF WINDOWS}
        ProcessListInfo:=PProcessListInfo(plist.Objects[i]);
        result:=ProcessListInfo.processID;
        {$ELSE}
        result:=strtoint('$'+copy(plist[i],1,j-1));
        {$endif}
        exit;
      end;
    end;

    //not found, try a half match

    bestpick.i:=-1;
    bestpick.pos:=65535;
    for i:=plist.Count-1 downto 0 do
    begin
      pname:=plist[i];
      j:=Pos('-', pname);
      pname:=uppercase(copy(pname, j+1,length(pname)));


      //processname found
      j:=pos(compareto, pname);
      if j>0 then
      begin
        if j=1 then
        begin
          {$IFDEF WINDOWS}
          ProcessListInfo:=PProcessListInfo(plist.Objects[i]);
          result:=ProcessListInfo.processID;
          {$ELSE}
          result:=strtoint('$'+copy(plist[i],1,j-1));
          {$ENDIF}
          exit;
        end;

        //not 1, just add it to the best pick if a smaller pos
        if j<bestpick.pos then
        begin
          bestpick.i:=i;
          bestpick.pos:=j;
        end;

      end;

      if bestpick.i<>-1 then
      begin
        {$IFDEF WINDOWS}
        ProcessListInfo:=PProcessListInfo(plist.Objects[bestpick.i]);
        result:=ProcessListInfo.processID;
        {$ELSE}
        result:=strtoint('$'+copy(plist[i],1,j-1));
        {$ENDIF}
        exit;
      end;
    end;

  finally
    cleanProcessList(plist);
    plist.free;
  end;
end;


function ce_openProcess2(pid: pointer): pointer;
var p: ptruint;
  oldprocessname: string;
  oldprocess: dword;
  oldprocesshandle: thandle;
begin
  p:=ptruint(pid);

  result:=nil;
  try
    if not mainform.openprocessPrologue then exit;

    DetachIfPossible;

    oldprocessname := copy(mainform.ProcessLabel.Caption, pos('-', mainform.ProcessLabel.Caption) + 1, length(mainform.ProcessLabel.Caption));
    oldprocess := processID;
    oldprocesshandle := processhandle;

    processhandler.processid:=p;
    Open_Process;

    if processhandle<>0 then
    begin
      MainForm.ProcessLabel.caption:=inttohex(ptruint(pid),8) + '-'+getProcessnameFromProcessID(ptruint(pid));
      mainform.openProcessEpilogue(oldprocessname, oldprocess, oldprocesshandle,true);
    end;

    result:=pointer(1); //made it till here, so no exception
  except
  end;
end;

function ce_openProcess(pid: dword): BOOL; stdcall;
begin
  result:=pluginsync(ce_openProcess2, pointer(PtrUInt(pid)))<>nil;
end;

function ce_pause2(params: pointer): pointer;
begin
  if AdvancedOptions=nil then
    AdvancedOptions:=TAdvancedOptions.Create(application);

  AdvancedOptions.Pausebutton.Down:=true;
  AdvancedOptions.Pausebutton.Click;
  result:=nil;
end;

procedure ce_pause; stdcall;
begin
  pluginsync(ce_pause2, nil);
end;

function ce_unpause2(params: pointer): pointer;
begin
  AdvancedOptions.Pausebutton.Down:=false;
  AdvancedOptions.Pausebutton.Click;
  result:=nil;
end;

procedure ce_unpause; stdcall;
begin
  pluginsync(ce_unpause2, nil);
end;

function ce_debugProcess2(params: pointer):pointer;
var debuggerinterface: integer;
begin
  debuggerinterface:=PtrUInt(params);
  case debuggerinterface of
    1: formSettings.cbUseWindowsDebugger.checked:=true;
    2: formSettings.cbUseVEHDebugger.checked:=true;
    3: formSettings.cbKDebug.checked:=true;
  end;

  if startdebuggerifneeded(false) then
    result:=pointer(1)
  else
    result:=nil;
end;

function ce_debugProcess(debuggerinterface: integer): BOOL; stdcall;
begin
  result:=pluginsync(ce_debugProcess2, pointer(PtrUInt(debuggerinterface)))<>nil;
end;

type
  TsetBreakpointParams=record
    address: ptruint;
    size: integer;
    trigger: TBreakpointTrigger;
  end;
  PSetBreakpointParams=^TSetBreakpointParams;

function ce_debug_setBreakpoint2(params: pointer): pointer;
var p: PSetBreakpointParams;
begin
  p:=params;
  if startdebuggerifneeded(false) then
  begin
    case p.trigger of
      bptAccess: debuggerthread.SetOnAccessBreakpoint(p.address, p.size);
      bptWrite: debuggerthread.SetOnWriteBreakpoint(p.address, p.size);
      bptExecute: debuggerthread.SetOnExecuteBreakpoint(p.address);
    end;

    MemoryBrowser.hexview.update;
    Memorybrowser.disassemblerview.Update;
  end;

  result:=pointer(1);
end;

function ce_debug_setBreakpoint(address: ptruint; size: integer; trigger: TBreakpointTrigger): BOOL; stdcall;
var p: TsetBreakpointParams;
begin
  p.address:=address;
  p.size:=size;
  p.trigger:=trigger;
  result:=pluginsync(ce_debug_setBreakpoint2, pointer(@p))<>nil;
end;

function ce_debug_removeBreakpoint(address: ptruint): BOOL; stdcall;
var bp: PBreakpoint;
begin
  result:=false;
  if debuggerthread<>nil then
  begin
    debuggerthread.lockbplist;
    try
      bp:=debuggerthread.isBreakpoint(address);
      if bp<>nil then
      begin
        debuggerthread.RemoveBreakpoint(bp);
        result:=true;
      end;
    finally
      debuggerthread.unlockbplist;
    end;
  end;
end;

function ce_debug_continueFromBreakpoint2(params: pointer): pointer;
var ContinueOption: TContinueOption;
begin
  ContinueOption:=TContinueOption(ptruint(params));

  if debuggerthread<>nil then
  begin
    {$ifdef windows}
    if ContinueOption=co_stepover then
    begin
      MemoryBrowser.miDebugStepOver.OnClick(MemoryBrowser.miDebugStepOver);
      //MemoryBrowser.miDebugStepOver.Click //use the memorybrowser step code for this case. Based on the debugstate and not gui state so should work
    end
    else
    {$endif}
      debuggerthread.ContinueDebugging(continueoption);

    result:=pointer(1);
  end
  else
    result:=nil;
end;

function ce_debug_continueFromBreakpoint(ContinueOption: TContinueOption): BOOL; stdcall;
begin
  result:=pluginsync(ce_debug_continueFromBreakpoint2, pointer(PtrUInt(ContinueOption)))<>nil;
end;

function ce_closeCE2(params: pointer):pointer;
begin
  mainform.mustClose:=true;
  mainform.Close;
  result:=nil;
end;

procedure ce_closeCE; stdcall;
begin
  //try to do a "normal" close
  pluginsync(ce_closeCE2,nil);

end;

function ce_hideAllCEWindows2(params: pointer):pointer;
var i: integer;
begin
  for i:=0 to screen.FormCount-1 do
  begin
    if (copy(screen.forms[i].name,1, 4)<>'UDF_') and ((screen.forms[i] is TCEForm)=false) then //if not a userdefined form
      screen.Forms[i].Visible:=false;
  end;

  mainform.visible:=false;
  MemoryBrowser.visible:=false;
  result:=nil;
end;

procedure ce_hideAllCEWindows; stdcall;
begin
  pluginsync(ce_hideAllCEWindows2,nil);

end;

function ce_unhideMainCEwindow2(params: pointer):pointer;
begin
  Mainform.show;
  result:=nil;
end;

procedure ce_unhideMainCEwindow; stdcall;
begin
  pluginsync(ce_unhideMainCEwindow2,nil);

end;

function ce_createForm2(params: pointer):pointer;
var f: TCEForm;
  visible: ^boolean;
begin
  visible:=params;

  f:=TCEForm.CreateNew(nil); //6.3: was (aplication)
  f.borderstyle:=bsSingle;

  if visible^ then f.show;

  result:=f;
  ComponentFunctionHandlerClass.inputComponent(result);
  f.name:='UDF_'+inttostr(f.tag);
  f.onclose:=ComponentFunctionHandlerClass.DefaultOnClose;
end;

function ce_createForm(visible: boolean): pointer; stdcall;
begin
  result:=pluginsync(ce_createForm2,@visible);

end;

function ce_form_centerScreen2(params: pointer):pointer;
var f: Tcustomform;
begin
  f:=Tcustomform(params);

  try
    if (f is Tcustomform) then
      f.Position:=poScreenCenter;

  except
  end;
  result:=nil;
end;

procedure ce_form_centerScreen(f: pointer); stdcall;
begin
  pluginsync(ce_form_centerScreen2,f);
end;

function ce_form_hide2(params: pointer): pointer;
begin
  Tcustomform(params).Hide;
  result:=nil;
end;

procedure ce_form_hide(f: pointer); stdcall;
begin
  pluginsync(ce_form_hide2,f);
end;

function ce_form_show2(params: pointer): pointer;
begin
  Tcustomform(params).show;
  result:=nil;
end;

procedure ce_form_show(f: pointer); stdcall;
begin
  pluginsync(ce_form_show2,f);
end;

function ce_createPanel2(params: pointer):pointer;
var p: TCEPanel;
begin
  p:=TCEPanel.Create(tcontrol(params));
  p.parent:=twincontrol(params);
  result:=p;
  ComponentFunctionHandlerClass.inputComponent(result);
end;

function ce_createPanel(owner: pointer): pointer; stdcall;
begin
  result:=pluginsync(ce_createPanel2,owner);
end;

function ce_createGroupBox2(params: pointer):pointer;
var g: TCEGroupBox;
begin
  g:=TCEGroupBox.Create(tcontrol(params));
  g.parent:=twincontrol(params);
  result:=g;
  ComponentFunctionHandlerClass.inputComponent(result);
end;

function ce_createGroupBox(owner: pointer): pointer; stdcall;
begin
  result:=pluginsync(ce_createGroupBox2,owner);
end;

function ce_createButton2(params: pointer):pointer;
var b: TCEButton;
begin
  b:=TCEbutton.Create(tcontrol(params));
  b.parent:=twincontrol(params);
  result:=b;
  ComponentFunctionHandlerClass.inputComponent(result);
end;

function ce_createButton(owner: pointer): pointer; stdcall;
begin
  result:=pluginsync(ce_createButton2,owner);
end;

function ce_createImage2(params: pointer):pointer;
var i: TCEImage;
begin
  i:=TCEImage.Create(tcontrol(params));
  i.parent:=twincontrol(params);
  result:=i;
  ComponentFunctionHandlerClass.inputComponent(result);
end;

function ce_createImage(owner: pointer): pointer; stdcall;
begin
  result:=pluginsync(ce_createImage2,owner);
end;

function ce_image_loadImageFromFile2(params: pointer): pointer;
type tp=record
  image: TImage;
  filename: pchar;
end;
var p: ^tp;
begin
  result:=nil;
  p:=params;
  try
    p.image.Picture.LoadFromFile(p.filename);
    result:=pointer(1);
  except
  end;
end;

function ce_image_loadImageFromFile(image: pointer; filename: pchar): BOOL; stdcall;
var p: record
  image: TImage;
  filename: pchar;
end;
begin
  p.image:=image;
  p.filename:=filename;
  result:=pluginsync(ce_image_loadImageFromFile2,@p)<>nil;
end;

function ce_image_stretch2(params: pointer): pointer;
type tp=record
  image: TImage;
  stretch: boolean;
end;
var p: ^tp;
begin
  result:=nil;
  p:=params;
  try
    p.image.Stretch:=p.stretch;
  except
  end;
end;

procedure ce_image_stretch(image: pointer; stretch: boolean); stdcall;
var p: record
  image: TImage;
  stretch: boolean;
end;
begin
  p.image:=image;
  p.stretch:=stretch;
  pluginsync(ce_image_stretch2,@p);
end;

function ce_image_transparent2(params: pointer): pointer;
type tp=record
  image: TImage;
  transparent: boolean;
end;
var p: ^tp;
begin
  result:=nil;
  p:=params;
  try
    p.image.transparent:=p.transparent;
  except
  end;
end;

procedure ce_image_transparent(image: pointer; transparent: boolean); stdcall;
var p: record
  image: TImage;
  transparent: boolean;
end;
begin
  p.image:=image;
  p.transparent:=transparent;
  pluginsync(ce_image_transparent2,@p);
end;

function ce_createLabel2(params: pointer):pointer;
var i: TLabel;
begin
  i:=TLabel.Create(tcontrol(params));
  i.parent:=twincontrol(params);
  result:=i;
  ComponentFunctionHandlerClass.inputComponent(result);
end;

function ce_createLabel(owner: pointer): pointer; stdcall;
begin
  result:=pluginsync(ce_createLabel2,owner);
end;

function ce_createEdit2(params: pointer):pointer;
var i: TEdit;
begin
  i:=TEdit.Create(tcontrol(params));
  i.parent:=twincontrol(params);
  result:=i;
  ComponentFunctionHandlerClass.inputComponent(result);
end;

function ce_createEdit(owner: pointer): pointer; stdcall;
begin
  result:=pluginsync(ce_createEdit2,owner);
end;

function ce_createMemo2(params: pointer):pointer;
var i: TCEMemo;
begin
  i:=TCEMemo.Create(tcontrol(params));
  i.parent:=twincontrol(params);
  result:=i;
  ComponentFunctionHandlerClass.inputComponent(result);
end;

function ce_createMemo(owner: pointer): pointer; stdcall;
begin
  result:=pluginsync(ce_createMemo2,owner);
end;

function ce_createTimer2(params: pointer):pointer;
var i: TCETimer;
begin
  i:=TCETimer.Create(tcontrol(params));

  result:=i;
  ComponentFunctionHandlerClass.inputComponent(result);
end;

function ce_createTimer(owner: pointer): pointer; stdcall;
begin
  result:=pluginsync(ce_createTimer2, owner);
end;

function ce_timer_setInterval2(params: pointer):pointer;
type tp=record
  timer: TCETimer;
  interval: integer;
end;
var p: ^tp;
begin
  p:=params;
  p.timer.interval:=p.interval;
  result:=nil;
end;

procedure ce_timer_setInterval(timer: pointer; interval: integer); stdcall;
var p:record
  timer: TCETimer;
  interval: integer;
end;
begin
  p.timer:=timer;
  p.interval:=interval;
  pluginsync(ce_timer_setInterval2, @p);
end;

type TOnTimer=record
    t: pointer;
    f: pointer;
    luafunction: integer;
end;
  POnTimer=^TOnTimer;


function ce_timer_onTimer2(params: pointer): pointer;
var p: POnTimer;
  t: TCETimer;
begin
  p:=params;
  t:=p.t;
  if (p.f=nil) and (p.luafunction=-1) then
  begin
    t.enabled:=false;
    t.Ontimer:=nil;
  end
  else
  begin
    ComponentFunctionHandlerClass.setOnTimer(tTimer(p.t), p.f, p.luafunction);
  end;
  result:=nil;
end;


procedure ce_timer_onTimer(t: pointer; f: pointer); stdcall;
var p: TONTimer;
begin
  p.luafunction:=-1;
  p.t:=t;
  p.f:=f;
  pluginsync(ce_Timer_onTimer2, @p)
end;

procedure ce_timer_onTimerLua(t: pointer; f: integer);
var p: TONTimer;
begin
  p.luafunction:=f;
  p.t:=t;
  p.f:=nil;

  pluginsync(ce_Timer_onTimer2, @p)
end;


function ce_control_setCaption2(params: pointer):pointer;
type TP=record
  control: TControl;
  caption: pchar;
end;
var  p: ^TP;
begin
  p:=params;
  try
    p.control.Caption:=p.caption;
  except
  end;
  result:=nil;
end;

procedure ce_control_setCaption(control: pointer; caption: pchar); stdcall;
var
  p: record
    control: TControl;
    caption: pchar;
  end;
begin
  p.control:=control;
  p.caption:=caption;
  pluginsync(ce_control_setCaption2, @p);
end;

function ce_control_getCaption2(params: pointer): pointer;
type
  tp= record
    control: TControl;
    caption: pchar;
    maxsize: integer;
  end;
var p: ^tp;
  s: pchar;
  l: integer;
begin
  p:=params;

  s:=pchar(p.control.Caption);

  l:=min(length(s), p.maxsize-1);
  CopyMemory(p.caption, s, l);

  p.caption[l]:=#0;

  result:=pointer(1);
end;

function ce_control_getCaption(control: pointer; caption: pchar; maxsize: integer): BOOL; stdcall;
var
  p: record
    control: TControl;
    caption: pchar;
    maxsize: integer;
  end;
begin
  p.control:=control;
  p.caption:=caption;
  p.maxsize:=maxsize;
  result:=pluginsync(ce_control_getCaption2, @p)<>nil;
end;

function ce_control_setPosition2(params: pointer): pointer;
type
  tp= record
    control: TControl;
    x: integer;
    y: integer;
  end;
var p: ^tp;
begin
  result:=nil;

  p:=params;
  p.control.Left:=p.x;
  p.control.top:=p.y;
end;


procedure ce_control_setPosition(control: pointer; x,y: integer); stdcall;
var
  p: record
    control: TControl;
    x: integer;
    y: integer;
  end;
begin
  p.control:=control;
  p.x:=x;
  p.y:=y;
  pluginsync(ce_control_setPosition2, @p);
end;


function ce_control_getX2(params: pointer): pointer;
begin
  result:=pointer(PtrUInt(TControl(params).left));
end;


function ce_control_getX(control: pointer): integer; stdcall;
begin
  result:=PtrUInt(pluginsync(ce_control_getX2, control));
end;

function ce_control_getY2(params: pointer): pointer;
begin
  result:=pointer(PtrUInt(TControl(params).top));
end;


function ce_control_getY(control: pointer): integer; stdcall;
begin
  result:=PtrUInt(pluginsync(ce_control_getY2, control));
end;

function ce_control_setSize2(params: pointer): pointer;
type tp=record
  control: Tcontrol;
  width,height: integer;
end;
var p:^tp;
begin
  p:=params;
  if p.width>=0 then
    p.control.Width:=p.width;

  if p.height>=0 then
    p.control.height:=p.height;

  result:=nil;
end;

procedure ce_control_setSize(control: pointer; width,height: integer); stdcall;
var p: record
  control: Tcontrol;
  width,height: integer;
end;
begin
  p.control:=control;
  p.width:=width;
  p.height:=height;
  pluginsync(ce_control_setSize2, @p);
end;

function ce_control_getWidth2(params: pointer): pointer;
begin
  result:=pointer(PtrUInt(TControl(params).width));
end;


function ce_control_getWidth(control: pointer): integer; stdcall;
begin
  result:=PtrUInt(pluginsync(ce_control_getWidth2, control));
end;

function ce_control_getHeight2(params: pointer): pointer;
begin
  result:=pointer(PtrUInt(TControl(params).height));
end;


function ce_control_getHeight(control: pointer): integer; stdcall;
begin
  result:=PtrUInt(pluginsync(ce_control_getHeight2, control));
end;

function ce_control_setAlign2(params: pointer): pointer;
type Tp= record
    control: Tcontrol;
    align: talign;
  end;
var p: ^tp;
begin
  p:=params;
  result:=nil;
  p.control.Align:=p.align;
end;

procedure ce_control_setAlign(control: pointer; align: integer); stdcall;
var p: record
    control: Tcontrol;
    align: talign;
  end;
begin
  case align of
    0: p.align:=alNone;
    1: p.align:=alTop;
    2: p.align:=alBottom;
    3: p.align:=alLeft;
    4: p.align:=alRight;
    5: p.align:=alClient;
    else p.align:=alNone;
  end;
  p.control:=control;
  pluginsync(ce_control_setAlign2, @p)
end;

function ce_component_destroy2(params: pointer): pointer;
begin
  if (tobject(params) is Tcomponent) then
    ComponentFunctionHandlerClass.removeComponent(params);

  if (TComponent(params) is TForm) then
    TForm(params).close
  else
    TComponent(params).Free;

  result:=nil;
end;

procedure ce_object_destroy(o: pointer); stdcall;
begin
  pluginsync(ce_component_destroy2, o)
end;

function ce_messageDialog2(params: pointer): pointer;
type tp= record
    message: pchar;
    messagetype: TMsgDlgType;
    buttons: TMsgDlgButtons;
end;
var p:^tp;
begin
  p:=params;
  result:=pointer(PtrUInt(MessageDlg(p.message, p.messagetype, p.buttons,0)));
end;

function ce_messageDialog(message: pchar; messagetype: integer; buttoncombination: integer): integer; stdcall;
var p: record
    message: pchar;
    messagetype: TMsgDlgType;
    buttons: TMsgDlgButtons;
end;

begin
  p.message:=message;
  case messagetype of
    0: p.messagetype:=mtWarning;
    1: p.messagetype:=mtError;
    2: p.messagetype:=mtInformation;
    3: p.messagetype:=mtConfirmation;
    else
      p.messagetype:=mtInformation;
  end;

  case buttoncombination of
    0: p.buttons:=[mbok];
    1: p.buttons:=mbYesNo;
    2: p.buttons:=mbYesNo+[mbCancel];
    3: p.buttons:=mbOKCancel;
  end;

  result:=PtrUInt(pluginsync(ce_messageDialog2, @p));
end;

function ce_messageDialog_Lua(message: pchar; messagetype: integer; buttoncombination: TMsgDlgButtons): integer;
var p: record
    message: pchar;
    messagetype: TMsgDlgType;
    buttons: TMsgDlgButtons;
end;

begin
  p.message:=message;
  case messagetype of
    0: p.messagetype:=mtWarning;
    1: p.messagetype:=mtError;
    2: p.messagetype:=mtInformation;
    3: p.messagetype:=mtConfirmation;
    else
      p.messagetype:=mtInformation;
  end;

  p.buttons:=buttoncombination;
  result:=ptruint(pluginsync(ce_messageDialog2, @p));

end;

function ce_speedhack2_setSpeed(params: pointer): pointer;
var speed: psingle;
  s: string;
begin
  result:=nil;
  try
    speed:=params;
    if not MainForm.cbSpeedhack.checked then
      MainForm.cbSpeedhack.Checked:=true;

    if mainform.cbspeedhack.checked then
    begin
      s:=floattostr(speed^);
      mainform.editsh2.Text:=s;
      mainform.btnSetSpeedhack2.Click;
    end;
    result:=pointer(1);
  except
  end;
end;

function ce_speedhack_setSpeed(speed: single): BOOL; stdcall;
begin
  result:=pluginsync(ce_speedhack2_setSpeed, @speed)<>nil;
end;

type TOnclick=record
    c: pointer;
    f: pointer;
    luafunction: integer;
end;
  POnClick=^TOnClick;


function ce_control_onClick2(params: pointer): pointer;
var p: POnClick;
begin
  p:=params;
  if (p.f=nil) and (p.luafunction=-1) then
    TControl(p.c).OnClick:=nil
  else
    ComponentFunctionHandlerClass.setOnClick(tcontrol(p.c), p.f, p.luafunction);

  result:=nil;
end;


procedure ce_control_onClick(c: pointer; f: pointer); stdcall;
var p: TONClick;
begin
  p.luafunction:=-1;
  p.c:=c;
  p.f:=f;
  pluginsync(ce_control_onClick2, @p)
end;

procedure ce_control_onClickLua(c: pointer; f: integer);
var p: TONClick;
begin
  p.luafunction:=f;
  p.c:=c;
  p.f:=nil;

  pluginsync(ce_control_onClick2, @p)
end;


type TOnCloseParams=record
    frm: pointer;
    f: pointer;
    luafunction: integer;
end;
  POnCloseParams=^TOnCloseParams;


function ce_form_onClose2(params: pointer): pointer;
var p: POnCloseParams;
begin
  p:=params;
  if (p.f=nil) and (p.luafunction=-1) then
    TForm(p.frm).OnClose:=nil
  else
    ComponentFunctionHandlerClass.setOnClose(tform(p.frm), p.f, p.luafunction);

  result:=nil;
end;


procedure ce_form_onClose(frm: pointer; f: pointer); stdcall;
var p: TOnCloseParams;
begin
  p.luafunction:=-1;
  p.frm:=frm;
  p.f:=f;
  pluginsync(ce_form_onClose2, @p)
end;

procedure ce_form_onCloseLua(frm: pointer; f: integer);
var p: TOnCloseParams;
begin
  p.luafunction:=f;
  p.frm:=frm;
  p.f:=nil;

  pluginsync(ce_form_onClose2, @p)
end;


function ce_getAutoAttachList2(params: pointer): pointer;
begin
  result:=mainform.extraautoattachlist;
end;

function ce_getAutoAttachList: pointer; stdcall;
begin
  result:=pluginsync(ce_getAutoAttachList2, nil);
end;

function ce_stringlist_getCount(c: pointer): integer; stdcall;
begin
  result:=tstringlist(c).Count;
end;


function ce_stringlist_add2(params: pointer): pointer;
type TP=record
  c: tstringlist;
  s: pchar;
end;
var p: ^TP;
begin
  p:=params;
  p.c.Add(p.s);
  result:=nil;
end;

procedure ce_stringlist_add(c: pointer; s: pchar); stdcall;
var p: record
  c: tstringlist;
  s: pchar;
end;
begin
  p.c:=c;
  p.s:=s;
  pluginsync(ce_stringlist_add2, @p);
end;

function ce_stringlist_remove2(params: pointer): pointer;
type TP=record
  c: tstrings;
  s: pchar;
end;
var p: ^TP;
  i: integer;
begin
  p:=params;
  i:=p.c.IndexOf(p.s);
  if i<>-1 then
    p.c.Delete(i);

  result:=nil;
end;

procedure ce_stringlist_remove(c: pointer; s: pchar); stdcall;
var p: record
  c: tstrings;
  s: pchar;
end;
begin
  p.c:=c;
  p.s:=s;
  pluginsync(ce_stringlist_remove2, @p);
end;

function ce_createProcess2(params: pointer): pointer;
{$ifdef windows}
type Tp= record
  path,params: pchar;
  debug: boolean;
  breakonentry: boolean;
end;
var p: ^tp;

  startupinfo: windows.STARTUPINFO;
  processinfo: windows.PROCESS_INFORMATION;
{$endif}
begin
  {$ifdef windows}
  p:=params;

  try
    if p.debug then
    begin
      DetachIfPossible;
      formsettings.cbUseWindowsDebugger.checked:=true;
      debuggerthread:=TDebuggerthread.MyCreate2(p.path, p.params, p.breakonentry);
    end
    else
    begin
      zeromemory(@startupinfo,sizeof(startupinfo));
      zeromemory(@processinfo,sizeof(processinfo));

      GetStartupInfo(@startupinfo);

      if windows.CreateProcess(
        pchar(p.path),
        pchar('"'+p.path+'" '+p.params),
        nil,
        nil,
        false,
        0,
        nil,
        pchar(extractfilepath(p.path)), //lpCurrentDirectory
        @startupinfo, //lpStartupInfo
        @processinfo //lpProcessInformation
      )=false then exit;

      processhandler.processid:=processinfo.dwProcessId;
      Open_Process;
    end;


    mainform.openProcessEpilogue('',0,0,true);
    mainform.ProcessLabel.caption:=inttohex(processid,8)+'-'+p.path;
    result:=pointer(1);
  except
    result:=nil;

  end;
  {$endif}
end;

function ce_createProcess(path,params: pchar; debug, breakonentry: BOOL): BOOL; stdcall;
var p: record
  path,params: pchar;
  debug: boolean;
  breakonentry: boolean;
end;

begin
  p.path:=path;
  p.params:=params;
  p.debug:=debug;
  p.breakonentry:=breakonentry;
  result:=pluginsync(ce_createProcess2, @p)<>nil;
end;


function ce_getPropertylist(c: tobject): pointer; stdcall;
var sl: tstringlist;
  s: string;
  i,j: integer;
  PP : PPropList;
begin
  result:=nil;
  sl:=nil;
  try
    pp:=nil;
    i:=GetPropList(c, pp);
    if i>0 then
    begin
      sl:=tstringlist.create;

      for j:=0 to i-1 do
      begin
        if pp^[j].PropType^.Kind<>tkMethod then
          sl.add(pp^[j].Name);
      end;


      result:=sl;
    end;
    if pp<>nil then
    begin
      freememandnil(pp);

    end;

  except
  end;
end;

function ce_setProperty2(params: pointer): pointer;
type tp= record
  c: tobject;
  propertyname: pchar;
  value: pchar;
end;
var p: ^tp;
begin
  result:=nil;
  p:=params;
  try
    SetPropValue(p.c, p.propertyname, p.value);
    result:=pointer(1);
  except
  end;
end;

function ce_setProperty(c: TObject; propertyname: pchar; value: pchar): BOOL; stdcall;
var p: record
  c: tobject;
  propertyname: pchar;
  value: pchar;
end;
begin
  p.c:=c;
  p.propertyname:=propertyname;
  p.value:=value;
  result:=pluginsync(ce_setProperty2, @p)<>nil;
end;

function ce_getProperty2(params: pointer): pointer;
type tp= record
  c: tobject;
  propertyname: pchar;
  value: pchar;
  maxsize: integer;
end;
var p: ^tp;
  s: string;
begin
  result:=nil;

  p:=params;
  //try
    s:=GetPropValue(p.c, p.propertyname, true);
    CopyMemory(p.value, pchar(s), min(length(s)+1, p.maxsize));

    result:=pointer(length(s)+1);
    if p.maxsize>0 then
      p.value[p.maxsize-1]:=#0;
  //except
  //  if p.maxsize>0 then
 //     p.value[0]:=#0;
 // end;
end;

function ce_getProperty(c: tobject; propertyname: pchar; value: pchar; maxsize: integer): integer; stdcall;
var p: record
  c: tobject;
  propertyname: pchar;
  value: pchar;
  maxsize: integer;
end;
begin
  p.c:=c;
  p.propertyname:=propertyname;
  p.value:=value;
  p.maxsize:=maxsize;
  result:=ptruint(pluginsync(ce_getProperty2, @p));
end;


function plugin_checksynchronize(timeout: integer):boolean; stdcall;
begin
  result:=CheckSynchronize(timeout);
end;

procedure plugin_processmessages; stdcall;
begin
  Application.ProcessMessages;
end;

function plugin_getluastate: Plua_State; stdcall;
begin
  result:=GetLuaState;
end;

initialization
  plugindisassembler:=TDisassembler.create;
  plugindisassembler.showsymbols:=false;
  plugindisassembler.showmodules:=false;
  plugindisassembler.showsections:=false;

  plugindisassembler.isdefault:=false;

  ComponentFunctionHandlerClass:=TComponentFunctionHandlerClass.create;

finalization
  if plugindisassembler<>nil then
    plugindisassembler.free;

  if ComponentFunctionHandlerClass<>nil then
    ComponentFunctionHandlerClass.free;
end.




