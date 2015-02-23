unit jniAddressList;
{
AddressList replacement to be used by the java version. It only contains data and not responsible for gui updates
}

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, jni;

procedure InitializeJniAddressList(env: PJNIEnv);

implementation

uses MemoryRecordUnit, unixporthelper, commonTypeDefs;

var
 addresslist: TList=nil; //list of MemoryRecord's
 AddressListEntry_class: jclass; //global ref
 AddressListEntry_init_method: jmethodID;
 description_fieldid: JFieldID;
 vartype_fieldid: JFieldID;
 addressString_fieldid: JFieldID;
 address_fieldid: JFieldID;
 offsets_fieldid: JFieldID;

function addresslist_getCount(PEnv: PJNIEnv; Obj: JObject): jint; cdecl;
begin
  log('addresslist_getCount');
  if (addresslist<>nil) then
    result:=addresslist.Count
  else
  begin
    result:=0;
    log('addresslist=nil');
  end;
end;

function addresslist_getEntry(PEnv: PJNIEnv; Obj: JObject; index: jint): jobject; cdecl;
var
 r: TMemoryRecord;
 descriptionstring: jstring;
begin
  result:=nil;
  log('addresslist_getEntry');
  if (index>=0) and (index<addresslist.count) then
  begin
    r:=TMemoryRecord(addresslist[index]);
    result:=penv^.NewObject(penv, AddressListEntry_class, AddressListEntry_init_method);

    log('Created result. Creating description string');
    descriptionstring:=penv^.NewStringUTF(penv, pchar(r.Description));

    log('Created descriptionstring, assigning it to the description field');

    penv^.SetObjectField(penv, result, description_fieldid, descriptionstring);

    log('Assigned the string to the description field');
  end;
end;

procedure addresslist_setEntry(PEnv: PJNIEnv; Obj: JObject; index: jint; entry: JObject) cdecl;
type TJintArray=Array[0..1000] of jint;
type PJintArray=^TJintArray;
var
  r: TMemoryRecord;
  descriptionstring,addressString: jstring;

  l: jint;
  offsetlist: PJintArray;
  offsets: jintArray;
  iscopy: jboolean;

  i: integer;
begin
  log('setting memory record');
  if (index<0) or (index>=addresslist.count) then exit;

  r:=TMemoryRecord(addresslist[index]);

  log('accessing provided entry');
  descriptionstring:=penv^.GetObjectField(penv, entry, description_fieldid);
  if (descriptionstring<>nil) then
  begin
    log('got the description');
    r.Description:=jniGetString(penv, descriptionstring);
  end
  else
    log('description=null');

  log('getting vartype');
  r.VarType:=TVariableType(penv^.GetIntField(penv, entry, vartype_fieldid));

  log('Getting addressString');
  addressString:=penv^.GetObjectField(penv, entry, addressString_fieldid);
  if (addressString<>nil) then
  begin
    log('got the addressString');
    r.interpretableaddress:=jniGetString(penv, addressString);
  end
  else
    log('description=null');


  offsets:=penv^.GetObjectField(penv, entry, offsets_fieldid);
  if (offsets<>nil) then
  begin
    log('offsets is not null');

    l:=penv^.GetArrayLength(penv, offsets);
    log('the offsets array length='+inttostr(l));

    offsetlist:=PJintArray(penv^.GetIntArrayElements(penv,offsets, iscopy));

    setlength(r.pointeroffsets, l);
    for i:=0 to l-1 do
      r.pointeroffsets[i]:=offsetlist[i];

    penv^.ReleaseIntArrayElements(penv, offsets, PJint(offsetlist),0);
  end
  else
    log('offsets=null');

  r.ReinterpretAddress;
end;

function addresslist_addEntry(PEnv: PJNIEnv; Obj: JObject; entry: JObject): jint; cdecl;
var
  r: TMemoryrecord;
begin
  log('creating memory record');
  r:=TMemoryrecord.Create(nil);
  addresslist.Add(r);
  result:=addresslist.count;

  log('Initializing memory record');
  addresslist_setEntry(penv, obj, result, entry);
end;

procedure addresslist_deleteEntry(PEnv: PJNIEnv; Obj: JObject; index: jint); cdecl;
begin
  log('Delete entry');
  if (index>=0) and (index<addresslist.count) then
  begin
    TMemoryRecord(addresslist[index]).free;
    addresslist.Delete(index);
  end;
end;

procedure addresslist_clear(PEnv: PJNIEnv; Obj: JObject); cdecl;
var i: integer;
begin
  log('addresslist.Clear');
  for i:=0 to addresslist.count-1 do
    TMemoryrecord(addresslist[i]).Free;

  addresslist.Clear;
end;


const methodcount=6;
var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'GetCount'; signature: '()I'; fnPtr: @addresslist_getCount),
  (name: 'GetEntry'; signature: '(I)Lorg/cheatengine/AddressListEntry;'; fnPtr: @addresslist_getEntry),
  (name: 'SetEntry'; signature: '(ILorg/cheatengine/AddressListEntry;)V'; fnPtr: @addresslist_setEntry),
  (name: 'AddEntry'; signature: '(Lorg/cheatengine/AddressListEntry;)I'; fnPtr: @addresslist_addEntry),
  (name: 'DeleteEntry'; signature: '(I)V'; fnPtr: @addresslist_deleteEntry),
  (name: 'Clear'; signature: '()V'; fnPtr: @addresslist_clear)

  );

procedure InitializeJniAddressList(env: PJNIEnv);
var c: jclass;
begin
  if addresslist=nil then
    addresslist:=Tlist.create;

  c:=env^.FindClass(env, 'org/cheatengine/AddressList');
  env^.RegisterNatives(env, c, @jnimethods[0], methodcount);

  c:=env^.FindClass(env, 'org/cheatengine/AddressListEntry');
  AddressListEntry_class:=env^.NewGlobalRef(env, c); //I'll need this later

  AddressListEntry_init_method:=env^.GetMethodID(env, c, '<init>', '()V');
  description_fieldid:=env^.GetFieldID(env, AddressListEntry_class, 'description','Ljava/lang/String;');
  vartype_fieldid:=env^.GetFieldID(env, AddressListEntry_class, 'vartype','I');
  addressString_fieldid:=env^.GetFieldID(env, AddressListEntry_class, 'addressString','Ljava/lang/String;');
  address_fieldid:=env^.GetFieldID(env, AddressListEntry_class, 'address','J');
  offsets_fieldid:=env^.GetFieldID(env, AddressListEntry_class, 'offsets','[I');

  log('description_fieldid='+inttohex(ptruint(description_fieldid),8));
  log('vartype_fieldid='+inttohex(ptruint(vartype_fieldid),8));
end;


end.

