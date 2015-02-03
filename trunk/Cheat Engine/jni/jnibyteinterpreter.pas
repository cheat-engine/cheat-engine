unit jniByteInterpreter;

{$mode delphi}

interface

uses
  Classes, SysUtils, jni;

procedure InitializeJniByteInterpreter(env: PJNIEnv);

implementation

uses byteinterpreter, commonTypeDefs;

//function readAndParseAddress(address: ptrUint; variableType: TVariableType; customtype: TCustomType=nil; showashexadecimal: Boolean=false; showAsSigned: boolean=false; bytesize:integer=1): string;


function byteInterpreter_readAndParseAddressJI(PEnv: PJNIEnv; Obj: JObject; address: jlong; vtype: jint): jstring; cdecl;
begin
  result:=PEnv^.NewStringUTF(PEnv, pchar(readAndParseAddress(address, Tvariabletype(vtype))));
end;

function byteInterpreter_readAndParseAddressJIZ(PEnv: PJNIEnv; Obj: JObject; address: jlong; vtype: jint; showAsHexadecimal: jboolean): jstring; cdecl;
begin
  result:=PEnv^.NewStringUTF(PEnv, pchar(readAndParseAddress(address, Tvariabletype(vtype),nil, showAsHexadecimal<>0)));
end;

function byteInterpreter_readAndParseAddressJIZZ(PEnv: PJNIEnv; Obj: JObject; address: jlong; vtype: jint; showAsHexadecimal: jboolean; showAsSigned: jboolean): jstring; cdecl;
begin
  result:=PEnv^.NewStringUTF(PEnv, pchar(readAndParseAddress(address, Tvariabletype(vtype),nil, showAsHexadecimal<>0, showAsSigned<>0)));
end;

function byteInterpreter_readAndParseAddressJIZZI(PEnv: PJNIEnv; Obj: JObject; address: jlong; vtype: jint; showAsHexadecimal: jboolean; showAsSigned: jboolean; bytesize: jint): jstring; cdecl;
begin
  result:=PEnv^.NewStringUTF(PEnv, pchar(readAndParseAddress(address, Tvariabletype(vtype),nil, showAsHexadecimal<>0, showAsSigned<>0, bytesize)));
end;

const methodcount=4;
var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'readAndParseAddress'; signature: '(JI)Ljava/lang/String;'; fnPtr: @byteInterpreter_readAndParseAddressJI),
  (name: 'readAndParseAddress'; signature: '(JIZ)Ljava/lang/String;'; fnPtr: @byteInterpreter_readAndParseAddressJIZ),
  (name: 'readAndParseAddress'; signature: '(JIZZ)Ljava/lang/String;'; fnPtr: @byteInterpreter_readAndParseAddressJIZZ),
  (name: 'readAndParseAddress'; signature: '(JIZZI)Ljava/lang/String;'; fnPtr: @byteInterpreter_readAndParseAddressJIZZI)
  {
  address: ptrUint;                        J
  variableType: TVariableType;             I
  showashexadecimal: Boolean=false;        Z
  showAsSigned: boolean=false;             Z
  bytesize:integer=1): string;             I
  }

  );

procedure InitializeJniByteInterpreter(env: PJNIEnv);
var c: jclass;
begin
  c:=env^.FindClass(env, 'org/cheatengine/ByteInterpreter');
  env^.RegisterNatives(env, c, @jnimethods[0], methodcount);

end;

end.

