unit jniSymbolHandler;
{
Not an TObject implementation. This is accessible without creating an object
}

{$mode delphi}

interface

uses
  Classes, SysUtils, jni;

procedure InitializeJniSymbolHandler(env: PJNIEnv);

implementation

uses symbolhandler;


function symbolhandler_getAddressFromName(PEnv: PJNIEnv; Obj: JObject; name: jstring): jlong; cdecl;
var _name: string;
begin
  result:=0;
  _name:=jniGetString(PEnv, name);
  try
    result:=symhandler.getAddressFromName(_name, false);
  except
  end;
end;

function symbolhandler_getAddressFromName2(PEnv: PJNIEnv; Obj: JObject; name: jstring; waitforsymbols: jboolean): jlong; cdecl;
var _name: string;
begin
  result:=0;
  _name:=jniGetString(PEnv, name);
  try
    result:=symhandler.getAddressFromName(_name, waitforsymbols<>0);
  except
  end;
end;

function symbolhandler_inModule(PEnv: PJNIEnv; Obj: JObject; address: jlong): jboolean; cdecl;
begin
  if symhandler.inModule(address) then
    result:=1
  else
    result:=0;

end;

const methodcount=3;
var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'getAddressFromName'; signature: '(Ljava/lang/String;)J'; fnPtr: @symbolhandler_getAddressFromName),
  (name: 'getAddressFromName'; signature: '(Ljava/lang/String;Z)J'; fnPtr: @symbolhandler_getAddressFromName2),
  (name: 'inModule'; signature: '(J)Z'; fnPtr: @symbolhandler_inModule)
  );

procedure InitializeJniSymbolHandler(env: PJNIEnv);
var c: jclass;
begin
  c:=env^.FindClass(env, 'org/cheatengine/SymbolHandler');
  env^.RegisterNatives(env, c, @jnimethods[0], methodcount);
end;

end.

