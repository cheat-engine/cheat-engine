library cecore;
{$mode DELPHI}


//This is only for support with the android version. 


uses {$ifdef unix}cthreads,{$endif}classes, jni, networkInterfaceApi, NewKernelHandler, networkInterface, sysutils,
  unixporthelper, ProcessHandlerUnit, elfsymbols, resolve, Sockets, ProcessList;


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

procedure Java_org_cheatengine_jnitest_MainActivity_f1(PEnv: PJNIEnv; Obj: JObject); cdecl;
var i: integer;
  t: TTestThread;
begin
//small test funtion
  log('Java_org_cheatengine_jnitest_MainActivity_f1 called');

  log('Creating thread');
  t:=TTestThread.Create(false);
  log('Thread created');

end;

procedure CEConnect(PEnv: PJNIEnv; Obj: JObject); cdecl;
var c: TCEConnection;
begin
  log('CEConnect called');
  host:=StrToNetAddr('127.0.0.1');
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

  log('pl.count='+inttostr(pl.Count));

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


function JNI_OnLoad(vm: PJavaVM; reserved: pointer): jint; cdecl;
var env: PJNIEnv;
  c: JClass;
  m: array [0..2] of JNINativeMethod;
  r: integer;
begin
  if (vm^.GetEnv(vm, @env, JNI_VERSION_1_6) <> JNI_OK) then
    result:=-1
  else
  begin
    log('JNI_OnLoad');

    InitializeNetworkInterface;


    log('env='+inttohex(ptruint(env),8));

    c:=env^.FindClass(env, 'org/cheatengine/jnitest/MainActivity');
    log('C='+inttohex(ptruint(c),1));


    m[0].fnPtr:=@CEConnect;
    m[0].name:='CEConnect';
    m[0].signature:='()V';

    m[1].fnPtr:=@GetProcessList;
    m[1].name:='GetProcessList';
    m[1].signature:='()Ljava/util/ArrayList;';

    m[2].fnPtr:=@SelectProcess;
    m[2].name:='SelectProcess';
    m[2].signature:='(I)V';

    r:=env^.RegisterNatives(env, c, @m[0], 3);

    log('after RegisterNatives. r='+inttostr(r));


    c:=env^.FindClass(env, 'org/cheatengine/jnitest/ProcessPicker');
    log('C='+inttohex(ptruint(c),1));

    m[0].fnPtr:=@GetProcessList;
    m[0].name:='GetProcessList';
    m[0].signature:='()Ljava/util/ArrayList;';

    m[1].fnPtr:=@SelectProcess;
    m[1].name:='SelectProcess';
    m[1].signature:='(I)V';

    r:=env^.RegisterNatives(env, c, @m[0], 2);

    log('after RegisterNatives. r='+inttostr(r));

    result:=JNI_VERSION_1_6;
  end;
end;


exports Java_org_cheatengine_jnitest_MainActivity_f1;
exports JNI_OnLoad;

begin

end.
