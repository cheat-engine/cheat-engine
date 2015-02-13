unit jniByteInterpreter;

{$mode delphi}

interface

uses
  Classes, SysUtils, jni;

procedure InitializeJniByteInterpreter(env: PJNIEnv);

implementation

uses byteinterpreter, commonTypeDefs, unixporthelper;

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

//parseStringAndWriteToAddress(String value, long address, int variabletype, boolean hexadecimal);
function byteInterpreter_parseStringAndWriteToAddress(PEnv: PJNIEnv; Obj: JObject; value: jstring; address: jlong; variabletype: integer; hexadecimal: jboolean): jboolean;
var v: string;
begin
  result:=0;
  try
    v:=jniGetString(penv, value);
    ParseStringAndWriteToAddress(v, address, TVariableType(variabletype), hexadecimal<>0);
    result:=1;
  except
    on e: exception do
    begin
      log('byteInterpreter_parseStringAndWriteToAddress:'+e.message);
    end;
  end;
end;

const methodcount=5;
var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'readAndParseAddress'; signature: '(JI)Ljava/lang/String;'; fnPtr: @byteInterpreter_readAndParseAddressJI),
  (name: 'readAndParseAddress'; signature: '(JIZ)Ljava/lang/String;'; fnPtr: @byteInterpreter_readAndParseAddressJIZ),
  (name: 'readAndParseAddress'; signature: '(JIZZ)Ljava/lang/String;'; fnPtr: @byteInterpreter_readAndParseAddressJIZZ),
  (name: 'readAndParseAddress'; signature: '(JIZZI)Ljava/lang/String;'; fnPtr: @byteInterpreter_readAndParseAddressJIZZI),
  (name: 'parseStringAndWriteToAddress'; signature: '(Ljava/lang/String;JIZ)Z'; fnPtr: @byteInterpreter_parseStringAndWriteToAddress)
{
boolean parseStringAndWriteToAddress(String value, long address, int variabletype, boolean hexadecimal);
  String value
  long address
  int variabletype
  boolean hexadecimal


  (Ljava/lang/String;JIZ)Z
}

  );

procedure InitializeJniByteInterpreter(env: PJNIEnv);
var c: jclass;
begin
  c:=env^.FindClass(env, 'org/cheatengine/ByteInterpreter');
  env^.RegisterNatives(env, c, @jnimethods[0], methodcount);

end;

end.

