library cecore;
{$mode DELPHI}


//This is only for support with the android version. 


uses cthreads, classes, jni, networkInterfaceApi, NewKernelHandler,
  networkInterface, sysutils, unixporthelper, ProcessHandlerUnit, elfsymbols,
  resolve, Sockets, ProcessList, memscan, Parsers, Globals, commonTypeDefs,
  strutils, savedscanhandler, VirtualQueryExCache, foundlisthelper,
  groupscancommandparser, CustomTypeHandler, SymbolListHandler, symbolhandler,
  DotNetTypes, DotNetPipe, byteinterpreter, jnitfoundlist, jniTMemScan,
  jniTObject, jniSymbolHandler, jniByteInterpreter;


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

function CEConnect(PEnv: PJNIEnv; Obj: JObject; hname: jstring): jboolean; cdecl;
var
  c: TCEConnection;
  hostname: string;
begin
  log('CEConnect called');

  hostname:=jniGetString(PEnv, hname);
  log('address='+hostname);

  host:=StrToNetAddr(hostname);
  port:=ShortHostToNet(52736);

  log('Host='+inttohex(host.s_addr,1));
  log('Port='+inttohex(port,1));

  c:=getConnection;
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

procedure SelectProcess(PEnv: PJNIEnv; Obj: JObject; pid: jint); cdecl;
begin
  processhandler.processid:=pid;
  processhandler.processhandle:=OpenProcess(PROCESS_ALL_ACCESS, false, pid);
  symhandler.reinitialize;

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

const methodcount=5;

  //experiment: make a memscan class in java and give it references to things like memscan_firstscan where the java class contains the memscan long

var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'CEConnect'; signature: '(Ljava/lang/String;)Z'; fnPtr: @CEConnect),
  (name: 'GetProcessList'; signature: '()Ljava/util/ArrayList;'; fnPtr: @GetProcessList),
  (name: 'SelectProcess'; signature: '(I)V'; fnPtr: @SelectProcess),
  (name: 'FetchSymbols'; signature: '(Z)V'; fnPtr: @FetchSymbols),
  (name: 'SetTempPath'; signature: '(Ljava/lang/String;)V'; fnPtr: @SetTempPath)
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
