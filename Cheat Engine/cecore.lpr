library cecore;
{$mode DELPHI}


//This is only for support with the android version. 


uses cthreads, classes, jni, networkInterfaceApi, NewKernelHandler,
  networkInterface, sysutils, unixporthelper, ProcessHandlerUnit, elfsymbols,
  resolve, Sockets, ProcessList, memscan, Parsers, Globals, strutils;

const
  packagename='org.cheatengine.jnitest';
  classname='cecore';

type TTestThread=class(TThread)
public
  procedure execute; override;
end;


procedure TTestThread.execute;
begin
  log('Alive');
  sleep(1000);
  log('Still alive');
  sleep(1000);
  log('And I''m still alive');

  FreeOnTerminate:=true;
end;

procedure Java_org_cheatengine_jnitest_cecore_f1(PEnv: PJNIEnv; Obj: JObject); cdecl;
var i: integer;
  t: TTestThread;
begin
//small test funtion
  log('Java_org_cheatengine_jnitest_cecore_f1 called');

  log('Creating thread');
  t:=TTestThread.Create(false);
  log('Thread created');

  //todo: test a custom mainthread
  //MainThreadID:= ... and in there call checkforsync

end;

procedure CEConnect(PEnv: PJNIEnv; Obj: JObject); cdecl;
var c: TCEConnection;
begin
  log('CEConnect called');
  host:=StrToNetAddr('192.168.0.12');
  port:=ShortHostToNet(52736);

  log('Host='+inttohex(host.s_addr,1));
  log('Port='+inttohex(port,1));

  c:=getConnection;
  log('c='+inttohex(ptruint(c),1));
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
end;

const methodcount=3;

var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'CEConnect'; signature: '()V'; fnPtr: @CEConnect),
  (name: 'GetProcessList'; signature: '()Ljava/util/ArrayList;'; fnPtr: @GetProcessList),
  (name: 'SelectProcess'; signature: '(I)V'; fnPtr: @SelectProcess)
);

function JNI_OnLoad(vm: PJavaVM; reserved: pointer): jint; cdecl;
var env: PJNIEnv;
  c: JClass;
  m: array [0..2] of JNINativeMethod;
  r: integer;
  pname: string;
begin
  if (vm^.GetEnv(vm, @env, JNI_VERSION_1_6) <> JNI_OK) then
    result:=-1
  else
  begin
    InitializeNetworkInterface;

    pname:=StringReplace(packagename, '.','/',[rfReplaceAll]);


    c:=env^.FindClass(env, pchar(pname+'/'+classname));  //'org/cheatengine/jnitest/cecore';
    r:=env^.RegisterNatives(env, c, @jnimethods[0], methodcount);
    result:=JNI_VERSION_1_6;
  end;
end;


exports Java_org_cheatengine_jnitest_cecore_f1;
exports JNI_OnLoad;

begin

end.
