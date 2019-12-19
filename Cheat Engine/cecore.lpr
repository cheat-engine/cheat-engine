library cecore;
{$mode DELPHI}


//This is only for support with the android version. 


uses cthreads, classes, jni, networkInterfaceApi, NewKernelHandler,
  networkInterface, sysutils, unixporthelper, ProcessHandlerUnit, elfsymbols,
  resolve, Sockets, ProcessList, memscan, Parsers, Globals, commonTypeDefs,
  strutils, savedscanhandler, VirtualQueryExCache, foundlisthelper,
  groupscancommandparser, CustomTypeHandler, SymbolListHandler, symbolhandler,
  DotNetTypes, DotNetPipe, byteinterpreter, MemoryRecordUnit, jnitfoundlist,
  jniTMemScan, jniTObject, jniSymbolHandler, jniByteInterpreter, pointerparser,
  Assemblerunit, DisassemblerArm, assemblerArm, DisassemblerThumb,
  LastDisassembleData, autoassembler, disassembler, jniAddressList, jniTDisassembler;


type TMainThread=class(TThread)
public
  procedure execute; override;
end;
var
  MainThread: TMainThread;


  memscan: TMemScan;


procedure TMainThread.execute;
//this thread will be responsible for calling functions that wish to synchronize with 'something' that can access the gui
begin

  //curvm^.AttachCurrentThreadAsDaemon(curvm, @env, nil);
  curvm^.AttachCurrentThread(curvm, @MainThreadEnv, nil);
  MainThreadID:=GetCurrentThreadId;

  while not terminated do
    CheckSynchronize(1000);

  curvm^.DetachCurrentThread(curvm);
end;

//-----
type
  TTestThread=class(TThread)
  private
    procedure runinmain;
  public
    procedure execute; override;
  end;

procedure TTestThread.runinmain;
begin
  log(IntToHex(GetThreadID,8)+':runinmain:This should be running from the main thread');
end;

procedure TTestThread.execute;
begin

  log(IntToHex(GetThreadID,8)+':This should run from a separate thread');
  synchronize(runinmain);

  log(IntToHex(GetThreadID,8)+':After runinmain');

  FreeOnTerminate:=true;
end;

procedure Java_org_cheatengine_cecore_f1(PEnv: PJNIEnv; Obj: JObject); cdecl;
var i: integer;

begin
//small test funtion
  log('Java_org_cheatengine_jnitest_cecore_f1 called');

  log('Creating test thread');
  TTestThread.Create(false);
  log('Thread created');



end;

function CEConnect(PEnv: PJNIEnv; Obj: JObject; hname: jstring; timeout: integer): jboolean; cdecl;
var
  c: TCEConnection;
  hostname: string;

  t: qword;
begin
  result:=0;

  log('CEConnect called');

  hostname:=jniGetString(PEnv, hname);
  log('address='+hostname);

  host:=StrToNetAddr(hostname);
  port:=ShortHostToNet(52736);

  log('Host='+inttohex(host.s_addr,1));
  log('Port='+inttohex(port,1));

  c:=nil;

  t:=GetTickCount64;
  while GetTickCount64<t+timeout do
  begin
    c:=getConnection;
    if c<>nil then break;
    sleep(200);
  end;
  log('c='+inttohex(ptruint(c),1));

  if c=nil then
    result:=0
  else
    result:=1;
end;

function GetProcessList(PEnv: PJNIEnv; Obj: JObject):jobject; cdecl;
var
  pl: TStringList;
  i: integer;

  cl: jclass;
  arraylist: jobject;
  initmethod: jmethodID;
  addmethod: jmethodID;
  tempstr: jstring;

begin
  pl:=tstringlist.Create;

  try
    ProcessList.GetProcessList(pl, false, true);

  except
    on e: exception do
      log('EXCEPTION:'+e.message);
  end;

  cl:=Penv^.FindClass(PEnv, 'java/util/ArrayList');

  initmethod:=Penv^.GetMethodID(penv, cl, '<init>', '()V');
  addmethod:=Penv^.GetMethodID(penv, cl, 'add', '(Ljava/lang/Object;)Z');
  arraylist:=Penv^.NewObject(penv, cl, initmethod);

  for i:=0 to pl.Count-1 do
  begin
    tempstr:=penv^.NewStringUTF(penv, pchar(pl[i]));
    penv^.CallBooleanMethodA(penv, arraylist, addmethod, @tempstr);

    penv^.DeleteLocalRef(penv, tempstr);

  end;

  pl.free;

  result:=arraylist;
end;

function ReadPointer(PEnv: PJNIEnv; Obj: JObject; Address: jlong): jlongArray; cdecl;  //(name: 'ReadPointer'; signature: '(J)[J'; fnPtr: @readPointer),
//reads a pointer from the current process and returns an 2 length long array. field 0 means success/failure field 1 returns the value
var
  res: jlong;
  x: ptruint;
  iscopy: jboolean;

  resarr: PJLong;

begin
  res:=0;
  result:=penv^.NewLongArray(penv, 2);
  resarr:=penv^.GetLongArrayElements(penv, result, iscopy);

  if readProcessmemory(processhandle, pointer(address), @res, processhandler.pointersize, x) then
  begin
    Puint64Array(resarr)[1]:=1;
    Puint64Array(resarr)[0]:=res;
  end
  else
  begin
    Puint64Array(resarr)[1]:=0;
  end;

  penv^.ReleaseLongArrayElements(penv, result, resarr, 0);
end;

function ReadMemoryIntoBuffer(PEnv: PJNIEnv; Obj: JObject; baseAddress: jlong; buffer: jbyteArray):jboolean; cdecl; //(name: 'ReadMemoryIntoBuffer'; signature: '(L[B)Z'; fnPtr: @ReadMemoryIntoBuffer)
var
  buffersize: jint;
  iscopy: jboolean;
  pbuf: PJByte;
  x: ptruint;

  test: PByteArray;
  i: integer;
begin
  result:=0;
  //log('ReadMemoryIntoBuffer');
  //log('address:'+inttohex(qword(baseAddress),8));


  buffersize:=PEnv^.GetArrayLength(penv, buffer);

 //   log('buffersize='+inttostr(buffersize));

  iscopy:=0;
  pbuf:=penv^.GetByteArrayElements(penv, buffer, iscopy);
  if (pbuf<>nil) then
  begin

   // log('acquired buffer at '+inttohex(ptruint(pbuf),8)+' iscopy='+inttostr(iscopy));

    for i:=0 to buffersize-1 do
      pbytearray(pbuf)[i]:=i;

    if ReadProcessMemory(processhandle, pointer(baseaddress), pbuf, buffersize, x) then
      result:=1;


    //  log('releasing buffer');

    penv^.ReleaseByteArrayElements(penv, buffer, pbuf, 0);
  end
  else
  begin

     // log('failure to aquire buffer');
  end;


end;

function GetRegionInfoString(PEnv: PJNIEnv; Obj: JObject; address: jlong): jstring; cdecl;
var
  mbi: TMemoryBasicInformation;
  mapsline: string;
begin
  log('GetRegionInfoString');
  Log('1');
  mapsline:='';
  Log('2');
  if GetRegionInfo(processhandle, pointer(address), mbi, sizeof(mbi), mapsline)>0 then
  begin
    Log('3a');
    log('success');

    if (mapsline='') then
      mapsline:=inttohex(ptruint(mbi.BaseAddress),8)+' -> '+inttohex(ptruint(mbi.baseaddress)+mbi.RegionSize,8);

    if (mbi.Protect=PAGE_NOACCESS) then
      mapsline:='Not readable: '+inttohex(ptruint(mbi.BaseAddress),8)+' -> '+inttohex(ptruint(mbi.baseaddress)+mbi.RegionSize,8);


    result:=penv^.NewStringUTF(penv, pchar(mapsline));
  end
  else
  begin
    Log('3b');
    Log('fail');
    result:=penv^.newStringUTF(penv, '----');
  end;

end;

procedure SetNetworkRPMCacheTimeout(PEnv: PJNIEnv; Obj: JObject; timeout: jfloat); cdecl;
begin
  if timeout>0 then
    networkRPMCacheTimeout:=timeout
  else
    networkRPMCacheTimeout:=0;
end;

procedure SelectProcess(PEnv: PJNIEnv; Obj: JObject; pid: jint); cdecl;
begin
  processhandler.processid:=pid;
  processhandler.processhandle:=OpenProcess(PROCESS_ALL_ACCESS, false, pid);
  symhandler.reinitialize;

end;


function GetSelectedProcessID(PEnv: PJNIEnv; Obj: JObject): jint; cdecl;
begin
  result:=processhandler.processid;
end;

//creatememscan ?

//test:
procedure FirstScan(PEnv: PJNIEnv; Obj: JObject; value: jint); cdecl;
begin
  log('FirstScan');

  if memscan=nil then
    memscan:=TMemScan.create(nil)
  else
    memscan.newscan;

  log('created memscan');
  memscan.firstscan(soExactValue, vtDword, rtTruncated, inttostr(value), '',0,$ffffffff, false, false, false, false);

end;

procedure FetchSymbols(PEnv: PJNIEnv; Obj: JObject; state: jboolean); cdecl;
begin
  Globals.fetchSymbols:=state<>0;
end;

procedure SetTempPath(PEnv: PJNIEnv; Obj: JObject; path: jstring); cdecl;
var
  _path: string;
begin
  log('SetTempPath');
  _path:=jniGetString(penv, path);
  dontusetempdir:=true;
  tempdiralternative:=_path;
  tempdirOverride:=_path;
end;

procedure TerminateCEServer (PEnv: PJNIEnv; Obj: JObject); cdecl;
var c: TCEConnection;
begin
  c:=getConnection;
  if c<>nil then
    c.TerminateServer;
end;

const methodcount=11;

  //experiment: make a memscan class in java and give it references to things like memscan_firstscan where the java class contains the memscan long

var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'CEConnect'; signature: '(Ljava/lang/String;I)Z'; fnPtr: @CEConnect),
  (name: 'GetProcessList'; signature: '()Ljava/util/ArrayList;'; fnPtr: @GetProcessList),
  (name: 'SelectProcess'; signature: '(I)V'; fnPtr: @SelectProcess),
  (name: 'GetSelectedProcessID'; signature: '()I'; fnPtr: @GetSelectedProcessID),

  (name: 'FetchSymbols'; signature: '(Z)V'; fnPtr: @FetchSymbols),
  (name: 'SetTempPath'; signature: '(Ljava/lang/String;)V'; fnPtr: @SetTempPath),
  (name: 'SetNetworkRPMCacheTimeout'; signature: '(F)V'; fnPtr: @SetNetworkRPMCacheTimeout),
  (name: 'ReadPointer'; signature: '(J)[J'; fnPtr: @ReadPointer),
  (name: 'ReadMemoryIntoBuffer'; signature: '(J[B)Z'; fnPtr: @ReadMemoryIntoBuffer),
  (name: 'GetRegionInfoString'; signature: '(J)Ljava/lang/String;'; fnPtr: @GetRegionInfoString),

  (name: 'TerminateCEServer'; signature: '()V'; fnPtr: @TerminateCEServer)
);

function JNI_OnLoad(vm: PJavaVM; reserved: pointer): jint; cdecl;
var env: PJNIEnv;
  c: JClass;
  pname: string;
begin
  curVM:=vm;

  if (vm^.GetEnv(vm, @env, JNI_VERSION_1_6) <> JNI_OK) then
    result:=-1
  else
  begin
    InitializeNetworkInterface;

    log('Creating main thread');
    MainThread:=TMainThread.Create(false);


    c:=env^.FindClass(env, 'org/cheatengine/cecore');  //'org/cheatengine/jnitest/cecore';
    env^.RegisterNatives(env, c, @jnimethods[0], methodcount);
    env^.DeleteLocalRef(env, c);

    InitializeJniTObject(env);
    InitializeJniTMemScan(env);
    InitializeJniTFoundList(env);
    InitializeJniSymbolHandler(env);
    InitializeJniByteInterpreter(env);
    InitializeJniAddressList(env);
    InitializeJniTDisassembler(env);


    result:=JNI_VERSION_1_6;
  end;
end;

procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer); cdecl;
begin
  //this doesn't seem to get called
  Log('Goodbye');
  if memscan<>nil then
    freeandnil(memscan);

  if MainThread<>nil then
  begin
    MainThread.Terminate;
    MainThread.WaitFor;
    freeandnil(MainThread);
  end;




end;


exports Java_org_cheatengine_cecore_f1;
exports JNI_OnLoad;
exports JNI_OnUnload;

begin
  Log('CECORE entry point executing');
  Log('calling symhandlerInitialize');
  symhandlerInitialize;
  Log('Returned from symhandlerInitialize');

end.
