unit jniTObject;

{$mode delphi}

interface

uses
  Classes, SysUtils, jni, unixporthelper;

procedure InitializeJniTObject(env: PJNIEnv);
function JObjectToTObject(PEnv: PJNIEnv; Obj: JObject): TObject;


implementation



//experiment: make a memscan class in java and give it references to things like memscan_firstscan where the java class contains the memscan long
var field_object: JFieldID;

function JObjectToTObject(PEnv: PJNIEnv; Obj: JObject): TObject;
//pre: obj Must be a TObject descendant
begin
  result:=TObject(penv^.GetLongField(penv, obj, field_object));
end;

procedure TObject_destroy(PEnv: PJNIEnv; Obj: JObject); cdecl;
var o: TObject;
begin
  //get the "object" field inside Obj, free what it represents, and then set it to 0
  log('TObject_destroy');

  o:=JObjectToTObject(PEnv, Obj);
  if o<>nil then
    tobject(o).free
  else
    log('Tried to call TObject_destroy on an already destroyed object');

  log('TObject_destroy returned');
end;

const methodcount=1;
var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'destroy'; signature: '()V'; fnPtr: @TObject_destroy)
  );

procedure InitializeJniTObject(env: PJNIEnv);
var c: jclass;
begin
  log('InitializeJniObject entry');
  c:=env^.FindClass(env, 'org/cheatengine/TObject');

  log('Getting the required fields');
  field_object:=env^.GetFieldID(env, c, 'object','J');
  log('After getting the fields ('+IntToHex(ptruint(field_object),8)+')');


  env^.RegisterNatives(env, c, @jnimethods[0], methodcount);
  env^.DeleteLocalRef(env, c);
  log('InitializeJniObject exit');
end;

end.

