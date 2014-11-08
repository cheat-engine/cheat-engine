unit jniTMemScan;

{$mode delphi}

interface

uses
  Classes, SysUtils, jni, unixporthelper;

procedure InitializeJniTMemScan(env: PJNIEnv);

implementation

uses jniTObject, memscan;

function TMemScan_Create(PEnv: PJNIEnv; Obj: JObject): jlong; cdecl;
begin
  result:=ptrint(TMemScan.create(nil));
end;

procedure TMemScan_NewScan(PEnv: PJNIEnv; Obj: JObject); cdecl;
var ms: TMemScan;
begin
  ms:=TMemScan(JObject2TObject(penv, obj));
  ms.newscan;
end;

const methodcount=2;
var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'create'; signature: '()J'; fnPtr: @TMemScan_Create),
  (name: 'newScan'; signature: '()V'; fnPtr: @TMemScan_NewScan)
  );

procedure InitializeJniTMemScan(env: PJNIEnv);
var c: jclass;
begin
  log('InitializeJniTMemScan entry');
  c:=env^.FindClass(env, 'org/cheatengine/TMemScan');
  env^.RegisterNatives(env, c, @jnimethods[0], methodcount);

  env^.DeleteLocalRef(env, c);
  log('InitializeJniTMemScan exit');
end;

end.

