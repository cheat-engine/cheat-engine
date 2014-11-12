unit jniTMemScan;

{$mode delphi}

interface

uses
  Classes, SysUtils, jni, unixporthelper, commonTypeDefs;

procedure InitializeJniTMemScan(env: PJNIEnv);

implementation

uses jniTObject, memscan;

var scanDoneMethodID: jmethodID;

type TJniMemscan=class(TMemScan)
private
  procedure cleanup;
protected
  procedure ScanDone; override;
public
  javaowner: jobject;
  destructor destroy; override;
end;

procedure TJniMemscan.ScanDone;  //called by synchronize
begin
  log('TJniMemscan.ScanDone');

  if javaowner<>nil then
  begin
    MainThreadEnv^.CallVoidMethod(MainThreadEnv, javaowner, scanDoneMethodID);
    log('Returned from OnScanDone');
  end;
end;

procedure TJniMemscan.cleanup;
begin
  log('TJniMemscan.destroy');
  MainThreadEnv^.DeleteGlobalRef(MainThreadEnv, javaowner);
end;

destructor TJniMemscan.destroy;
begin
  log('TJniMemscan.destroy');
  if javaowner<>nil then
    TThread.Synchronize(nil, cleanup);

  inherited destroy;
end;

function TMemScan_Create(PEnv: PJNIEnv; Obj: JObject; owner: JObject): jlong; cdecl;
var ms: TJniMemscan;
begin
  log('Creating a memscan object');
  ms:=TJniMemscan.create(nil);

  if penv^.IsSameObject(PEnv, obj, owner)<>0 then
    log('Weird');

  ms.javaowner:=penv^.NewGlobalRef(penv, owner);
  result:=ptrint(ms);
end;

procedure TMemScan_NewScan(PEnv: PJNIEnv; Obj: JObject); cdecl;
var ms: TMemScan;
begin
  log('new scan');
  ms:=TMemScan(JObjectToTObject(penv, obj));
  ms.newscan;
end;

procedure TMemScan_FirstScan(PEnv: PJNIEnv; Obj: JObject; scanOption: jint; variabletype: jint; roundingtype: jint; sv1: jstring; sv2: jstring; startaddress: jlong; stopaddress: jlong; fastscanmethod: jint; fastscanparameter: jstring; hexadecimal: jboolean; binaryasstring: jboolean; unicode: jboolean; casesensitive: jboolean); cdecl;
var
  ms: TJniMemscan;
  scanvalue1, scanvalue2: string;
begin
  log('First scan');
  ms:=TJniMemscan(JObjectToTObject(penv, obj));




  scanvalue1:=jniGetString(penv, sv1);
  scanvalue2:=jniGetString(penv, sv2);

  ms.firstscan(TScanOption(scanOption), TVariableType(variabletype), TRoundingType(roundingtype), scanvalue1, scanvalue2, startaddress, stopaddress, hexadecimal<>0, binaryasstring<>0, unicode<>0, casesensitive<>0, TFastScanMethod(fastscanmethod), jnigetstring(penv, fastscanparameter), nil);
end;

const methodcount=3;
var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'create'; signature: '(Lorg/cheatengine/TMemScan;)J'; fnPtr: @TMemScan_Create),
  (name: 'newScan'; signature: '()V'; fnPtr: @TMemScan_NewScan),
  (name: 'firstScan'; signature: '(IIILjava/lang/String;Ljava/lang/String;JJILjava/lang/String;ZZZZ)V'; fnPtr: @TMemScan_FirstScan)

  );

procedure InitializeJniTMemScan(env: PJNIEnv);
var c: jclass;
begin
  log('InitializeJniTMemScan entry');
  c:=env^.FindClass(env, 'org/cheatengine/TMemScan');
  env^.RegisterNatives(env, c, @jnimethods[0], methodcount);

  scanDoneMethodID:=env^.GetMethodID(env, c, 'OnScanDone', '()V');

  log('scanDoneMethodID='+inttohex(ptruint(scanDoneMethodID),8));
  log('InitializeJniTMemScan exit');
end;

end.

