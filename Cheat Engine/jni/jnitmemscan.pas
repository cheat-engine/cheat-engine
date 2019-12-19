unit jniTMemScan;

{$mode delphi}

interface

uses
  Classes, SysUtils, jni, unixporthelper, commonTypeDefs, math;

procedure InitializeJniTMemScan(env: PJNIEnv);

implementation

uses jniTObject, memscan, Globals;

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
  result:=ptruint(ms);
end;

procedure TMemScan_NewScan(PEnv: PJNIEnv; Obj: JObject); cdecl;
var ms: TMemScan;
begin
  log('new scan');
  ms:=TMemScan(JObjectToTObject(penv, obj));
  ms.newscan;
end;

function TMemScan_GetProgress(PEnv: PJNIEnv; Obj: JObject):jint; cdecl;
var
  ms: TMemscan;
  total,current: qword;

  p: integer;

begin
  result:=0;
  p:=0;
  try
    ms:=TMemScan(JObjectToTObject(penv, obj));
    if (ms<>nil) then
    begin
      p:=1;
      ms.GetProgress(total, current);
      p:=2;

      if total>0 then
      begin
        p:=3;
        result:=(current*100) div total;
        p:=4;
        //log(format('TMemScan_GetProgress returns %d',[result]));
      end
    end;

    p:=5;
  except
    on e:exception do
      log('Exception in TMemScan_GetProgress:'+e.message+'  (p='+inttostr(p));
  end;
end;

procedure TMemScan_NextScan(PEnv: PJNIEnv; Obj: JObject; scanOption: jint; roundingtype: jint; sv1: jstring; sv2: jstring; hexadecimal: jboolean; binaryasstring: jboolean; unicode: jboolean; casesensitive: jboolean; percentage: jboolean; compareToSavedScan: jboolean; ssn: jstring); cdecl;
var
  ms: TJniMemscan;
  scanvalue1, scanvalue2, savedscanname: string;
begin
  log('Next scan');
  ms:=TJniMemscan(JObjectToTObject(penv, obj));

  scanvalue1:=jniGetString(penv, sv1);
  scanvalue2:=jniGetString(penv, sv2);
  savedscanname:=jniGetString(penv, ssn);

  ms.NextScan(TScanOption(scanOption), TRoundingType(roundingtype), scanValue1, scanValue2, hexadecimal<>0, binaryAsString<>0, unicode<>0, casesensitive<>0, percentage<>0, comparetoSavedScan<>0, savedscanname);
end;

procedure TMemScan_FirstScan(PEnv: PJNIEnv; Obj: JObject; scanOption: jint; variabletype: jint; roundingtype: jint; sv1: jstring; sv2: jstring; pf: jstring; startaddress: jlong; stopaddress: jlong; fastscanmethod: jint; fastscanparameter: jstring; hexadecimal: jboolean; binaryasstring: jboolean; unicode: jboolean; casesensitive: jboolean; pagedonly: jboolean; dirtyonly: jboolean; noshared: jboolean); cdecl;
var
  ms: TJniMemscan;
  scanvalue1, scanvalue2, protectionflags: string;
  c: jclass;
begin
  log('First scan');

  c:=penv^.FindClass(penv, 'org/cheatengine/TObject');

  if penv^.IsInstanceOf(penv, obj, c)=0 then
  begin
    log('memscantest: obj<>TObject');
  end;

  penv^.DeleteLocalRef(penv, c);


  ms:=TJniMemscan(JObjectToTObject(penv, obj));

  Scan_MEM_MAPPED:=noshared=0;
  scan_dirtyonly:=dirtyonly<>0;
  scan_pagedonly:=pagedonly<>0;

  scanvalue1:=jniGetString(penv, sv1);
  scanvalue2:=jniGetString(penv, sv2);
  protectionflags:=jniGetString(penv, pf);

  log('scanvalue1='+scanvalue1);
  log('scanvalue2='+scanvalue2);
  log('protectionflags='+protectionflags);

  ms.parseProtectionflags(protectionflags);
  ms.firstscan(TScanOption(scanOption), TVariableType(variabletype), TRoundingType(roundingtype), scanvalue1, scanvalue2, startaddress, stopaddress, hexadecimal<>0, binaryasstring<>0, unicode<>0, casesensitive<>0, TFastScanMethod(fastscanmethod), jnigetstring(penv, fastscanparameter), nil);
end;

function TMemScan_HasDoneFirstScan(PEnv: PJNIEnv; Obj: JObject):jboolean; cdecl;
var
  ms: TJniMemscan;
begin
  ms:=TJniMemscan(JObjectToTObject(penv, obj));
  result:=ifthen(ms.LastScanType<>stNewScan,1,0);
end;

function TMemScan_GetVarType(PEnv: PJNIEnv; Obj: JObject):jint; cdecl;
var
  ms: TJniMemscan;
begin
  ms:=TJniMemscan(JObjectToTObject(penv, obj));
  result:=integer(ms.VarType);
end;


function TMemScan_GetBinarySize(PEnv: PJNIEnv; Obj: JObject):jint; cdecl;
var
  ms: TJniMemscan;
begin
  ms:=TJniMemscan(JObjectToTObject(penv, obj));
  result:=ms.Getbinarysize;
end;

const methodcount=8;
var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'create'; signature: '(Lorg/cheatengine/TMemScan;)J'; fnPtr: @TMemScan_Create),
  (name: 'newScan'; signature: '()V'; fnPtr: @TMemScan_NewScan),
  (name: 'firstScan'; signature: '(IIILjava/lang/String;Ljava/lang/String;Ljava/lang/String;JJILjava/lang/String;ZZZZZZZ)V'; fnPtr: @TMemScan_FirstScan),
  (name: 'nextScan'; signature: '(IILjava/lang/String;Ljava/lang/String;ZZZZZZLjava/lang/String;)V'; fnPtr: @TMemScan_NextScan),
  (name: 'hasDoneFirstScan'; signature: '()Z'; fnPtr: @TMemScan_HasDoneFirstScan),
  (name: 'getVarType'; signature: '()I'; fnPtr: @TMemScan_GetVarType),
  (name: 'getProgress'; signature: '()I'; fnPtr: @TMemScan_GetProgress),
  (name: 'getBinarySize'; signature: '()I'; fnPtr: @TMemScan_GetBinarySize)

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

