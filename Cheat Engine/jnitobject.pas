unit jniTObject;

{$mode delphi}

interface

uses
  Classes, SysUtils, jni, unixporthelper;

procedure InitializeJniTObject(env: PJNIEnv);
function JObject2TObject(PEnv: PJNIEnv; Obj: JObject): TObject;


implementation

const methodcount=1;

//experiment: make a memscan class in java and give it references to things like memscan_firstscan where the java class contains the memscan long
var field_object: JFieldID;

function JObject2TObject(PEnv: PJNIEnv; Obj: JObject): TObject;
begin
  result:=TObject(penv^.GetLongField(penv, obj, field_object));
end;

procedure TObject_destroy(PEnv: PJNIEnv; Obj: JObject); cdecl;
var o: TObject;
begin
  //get the "object" field inside Obj, free what it represents, and then set it to 0
  log('TObject_destroy');

  o:=JObject2TObject(PEnv, Obj);
  if o<>nil then
    tobject(o).free
  else
    log('Tried to call TObject_destroy on an already destroyed object');

  log('TObject_destroy returned');
end;

var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'TObject_destroy'; signature: '()V'; fnPtr: @TObject_destroy)
  );

procedure InitializeJniTObject(env: PJNIEnv);
var c: jclass;
begin
  log('InitializeJniObject entry');
  c:=env^.FindClass(env, 'org/cheatengine/TObject');

  log('Getting the required fields');
  field_object:=env^.GetFieldID(env, c, 'object','L');
  log('After getting the fields');


  env^.RegisterNatives(env, c, @jnimethods[0], methodcount);
  log('InitializeJniObject exit');
end;

end.

