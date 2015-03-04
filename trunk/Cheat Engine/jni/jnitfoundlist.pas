unit jnitfoundlist;

{$mode delphi}

interface

uses
  Classes, SysUtils, jni;

procedure InitializeJniTFoundList(env: PJNIEnv);

implementation

uses unixporthelper, jniTObject, foundlisthelper, memscan, symbolhandler, math;


function TFoundList_initialize(PEnv: PJNIEnv; Obj: JObject): jlong; cdecl;
var fl: TFoundList;
begin
  result:=0;

  log('TFoundList_initialize');
  fl:=TFoundList(JObjectToTObject(penv, obj));

  if fl<>nil then
  begin
    try
      result:=fl.Initialize;
    except
      on e:exception do
      begin
        log(e.Message);
      end;
    end;
  end
  else
    log('fl is nil');

end;

procedure TFoundList_deinitialize(PEnv: PJNIEnv; Obj: JObject); cdecl;
var fl: TFoundList;
begin
  fl:=TFoundList(JObjectToTObject(penv, obj));
  fl.Deinitialize;
end;


procedure TFoundList_refetchValueList(PEnv: PJNIEnv; Obj: JObject); cdecl;
var fl: TFoundList;
begin
  fl:=TFoundList(JObjectToTObject(penv, obj));
  fl.RefetchValueList;
end;



function TFoundList_getCount(PEnv: PJNIEnv; Obj: JObject): jlong; cdecl;
var
  extra: dword;
  value: string;
begin
  result:=TFoundList(JObjectToTObject(penv, obj)).count;
end;

function TFoundList_inModule(PEnv: PJNIEnv; Obj: JObject; index: jint): jboolean; cdecl;
begin
  if TFoundList(JObjectToTObject(penv, obj)).inModule(index) then
    result:=1
  else
    result:=0;
end;

function TFoundList_getAddress(PEnv: PJNIEnv; Obj: JObject; index: jint): jlong; cdecl;
begin
  result:=TFoundList(JObjectToTObject(penv, obj)).GetAddress(index);
end;

function TFoundList_getAddressString(PEnv: PJNIEnv; Obj: JObject; index: jint): jstring; cdecl;
var
  fl: TFoundList;
  address: ptruint;
  r: string;
begin
  fl:=TFoundList(JObjectToTObject(penv, obj));
  address:=fl.GetAddress(index);

  r:=symhandler.getNameFromAddress(address);
  result:=penv^.NewStringUTF(penv, pchar(r));
end;

function TFoundList_getVarType(PEnv: PJNIEnv; Obj: JObject): jint; cdecl;
begin
  result:=integer(TFoundList(JObjectToTObject(penv, obj)).vartype);
end;

function TFoundList_getVarLength(PEnv: PJNIEnv; Obj: JObject): jint; cdecl;
begin
  result:=TFoundList(JObjectToTObject(penv, obj)).GetVarLength;
end;


function TFoundList_isUnicode(PEnv: PJNIEnv; Obj: JObject): jboolean; cdecl;
begin
  result:=ifthen(TFoundList(JObjectToTObject(penv, obj)).isUnicode,1,0);
end;


function TFoundList_getValue(PEnv: PJNIEnv; Obj: JObject; index: jint): jstring; cdecl;
var
  s: string;
  extra: dword;
  a: ptruint;
begin
  a:=TFoundList(JObjectToTObject(penv, obj)).GetAddress(index, extra, s);

  result:=penv^.NewStringUTF(penv, pchar(s));
end;

function TFoundList_Create(PEnv: PJNIEnv; Obj: JObject; ownerms: JObject): jlong; cdecl;
var ms: TMemscan;
  fl: TFoundList;
begin
  log('Creating a foundlist object');
  ms:=TMemScan(JObjectToTObject(penv, ownerms));

  if (ms<>nil) then
    log('ms.ScanResultFolder='+ms.ScanresultFolder)
  else
    log('ms = nil');


  fl:=TFoundList.create(nil, ms);

  result:=ptrint(fl);
end;


const methodcount=12;
var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'create'; signature: '(Lorg/cheatengine/TMemScan;)J'; fnPtr: @TFoundList_Create),
  (name: 'initialize'; signature: '()J'; fnPtr: @TFoundList_initialize),
  (name: 'deinitialize'; signature: '()V'; fnPtr: @TFoundList_deinitialize),
  (name: 'getCount'; signature: '()J'; fnPtr: @TFoundList_getCount),
  (name: 'getAddress'; signature: '(I)J'; fnPtr: @TFoundList_getAddress),
  (name: 'getAddressString'; signature: '(I)Ljava/lang/String;'; fnPtr: @TFoundList_getAddressString),
  (name: 'getValue'; signature: '(I)Ljava/lang/String;'; fnPtr: @TFoundList_getValue),
  (name: 'getVarType'; signature: '()I'; fnPtr: @TFoundList_getVarType),
  (name: 'getVarLength'; signature: '()I'; fnPtr: @TFoundList_getVarLength),
  (name: 'isUnicode'; signature: '()Z'; fnPtr: @TFoundList_isUnicode),
  (name: 'inModule'; signature: '(I)Z'; fnPtr: @TFoundList_inModule),
  (name: 'refetchValueList'; signature: '()V'; fnPtr: @TFoundList_refetchValueList)

  );

procedure InitializeJniTFoundList(env: PJNIEnv);
var c: jclass;
begin
  log('InitializeJniTFoundList entry');
  c:=env^.FindClass(env, 'org/cheatengine/TFoundList');
  env^.RegisterNatives(env, c, @jnimethods[0], methodcount);

  log('InitializeJniTFoundList exit');
end;

end.

