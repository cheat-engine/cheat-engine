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

uses MemoryRecordUnit, unixporthelper, commonTypeDefs, syncobjs, math;

type
  TAddressListFreezer=class(TThread)
  private
  public
    procedure execute; override;
  end;

var
 addresslist: TList=nil; //list of MemoryRecord's
 addresslistcs: TCriticalsection; //used to protect the list from the freezer thread. Changing memory records is not part of this.

 AddressListEntry_class: jclass; //global ref
 AddressListEntry_init_method: jmethodID;
 description_fieldid: JFieldID;
 vartype_fieldid: JFieldID;
 addressString_fieldid: JFieldID;
 address_fieldid: JFieldID;
 offsets_fieldid: JFieldID;
 index_fieldid: JFieldID;
 active_fieldid: jfieldID;

 isUnicode_fieldid: jfieldID;
 size_fieldid: jFieldid;


 freezeInterval: integer;
 freezerthread: TAddressListFreezer;

procedure TAddressListFreezer.execute;
{
'timer' that freezes the addresslist
}
var
  i: integer;
  r: TMemoryrecord;
begin
  while not terminated do
  begin
    addresslistcs.enter;
    try
      for i:=0 to addresslist.count-1 do
      begin
        r:=TMemoryrecord(addresslist[i]);
        if (r.Active) then
          r.ApplyFreeze;
      end;
    finally
      addresslistcs.Leave;
    end;
    sleep(freezeInterval);
  end;
end;


function addresslist_getCount(PEnv: PJNIEnv; Obj: JObject): jint; cdecl;
begin
 // log('addresslist_getCount');
  if (addresslist<>nil) then
    result:=addresslist.Count
  else
  begin
    result:=0;
    log('addresslist=nil');
  end;
end;

function addresslist_setActive(PEnv: PJNIEnv; Obj: JObject; index: jint; state: jboolean): jboolean; cdecl;
var r: TMemoryRecord;
begin
  log('addresslist_setActive');
  if (index>=0) and (index<addresslist.count) then
  begin
    r:=TMemoryRecord(addresslist[index]);
    r.Active:=state=1;

    result:=ifthen(r.Active,1,0);

    if r.Active and (freezerthread=nil) then //start the freezer thread
      freezerthread:=TAddressListFreezer.Create(false);
  end;
end;

function addresslist_setFreezeTimer(PEnv: PJNIEnv; Obj: JObject; interval: jint): jobject; cdecl;
begin
  log('Setting freeze interval to '+inttostr(interval));
  freezeInterval:=interval;
end;

procedure addresslist_setEntryValue(PEnv: PJNIEnv; Obj: JObject; index: jint; value: jstring); cdecl;
var r: TMemoryRecord;
begin
  if (index>=0) and (index<addresslist.count) then
  begin
    r:=TMemoryRecord(addresslist[index]);
    try
      r.Value:=jniGetString(penv, value);
    except
      log('addresslist_setEntryValue: Error trying to set value');
    end;
  end;
end;

function addresslist_getEntryValue(PEnv: PJNIEnv; Obj: JObject; index: jint): jstring; cdecl;
var
  r: TMemoryRecord;
  v: string;
begin
  if (index>=0) and (index<addresslist.count) then
  begin
    r:=TMemoryRecord(addresslist[index]);
    v:=r.value;
    result:=penv^.NewStringUTF(penv, pchar(v));
  end;
end;


function addresslist_getEntry(PEnv: PJNIEnv; Obj: JObject; index: jint): jobject; cdecl;
var
 i: integer;
 r: TMemoryRecord;
 descriptionstring, addressString: jstring;

 offsetsarr: jintArray;
 offsets: Pjint;
 iscopy: jboolean;
begin
  result:=nil;
  //log('addresslist_getEntry');
  if (index>=0) and (index<addresslist.count) then
  begin
    r:=TMemoryRecord(addresslist[index]);
    result:=penv^.NewObject(penv, AddressListEntry_class, AddressListEntry_init_method);

    //log('Created result. Creating description string ('+r.description+')');
    descriptionstring:=penv^.NewStringUTF(penv, pchar(r.Description));

   // log('Created descriptionstring, assigning it to the description field');

    penv^.SetObjectField(penv, result, description_fieldid, descriptionstring);

    //log('Assigned the string to the description field');

    //log('Creating addressString ('+r.AddressString+')');
    addressString:=penv^.NewStringUTF(penv, pchar(r.interpretableaddress));
    //log('Created addressString ('+r.AddressString+')');

   // log('Assigning addressString');
    penv^.SetObjectField(penv, result, addressString_fieldid, addressString);

    penv^.SetLongField(penv, result, address_fieldid, r.GetRealAddress);
    penv^.SetIntField(penv, result, vartype_fieldid, integer(r.VarType));
    penv^.SetIntField(penv, result, index_fieldid, index);
    penv^.SetBooleanField(penv, result, active_fieldid, ifthen(r.active,1,0));

    //assign the offsets (if there are any)


    offsetsarr:=penv^.NewIntArray(penv, r.offsetCount);
    offsets:=penv^.GetIntArrayElements(penv, offsetsarr, iscopy);


    for i:=0 to r.offsetCount-1 do
      PIntegerArray(offsets)[i]:=r.offsets[i].offset;

    penv^.ReleaseIntArrayElements(penv, offsetsarr, offsets, 0);

    penv^.SetObjectField(penv, result, offsets_fieldid, offsetsarr);

    if r.vartype=vtString then
    begin
      penv^.SetIntField(penv, result, size_fieldid, r.Extra.stringData.length);
      penv^.SetBooleanField(penv, result, isUnicode_fieldid, ifthen(r.extra.stringData.unicode,1,0));
    end;

    if r.vartype=vtByteArray then
      penv^.SetIntField(penv, result, size_fieldid, r.Extra.byteData.bytelength);

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
 // log('setting memory record');
  if (index<0) or (index>=addresslist.count) then
  begin
    log('index ('+inttostr(index)+') is outside bounds');
    exit;
  end;

  r:=TMemoryRecord(addresslist[index]);

 // log('accessing provided entry');
  descriptionstring:=penv^.GetObjectField(penv, entry, description_fieldid);
  if (descriptionstring<>nil) then
  begin
  //  log('got the description');
    r.Description:=jniGetString(penv, descriptionstring);

 //   log('description='+r.Description);
  end
  else
    log('description=null');

 // log('getting vartype');
  i:=penv^.GetIntField(penv, entry, vartype_fieldid);
 // log('vartype='+inttostr(i));
  r.VarType:=TVariableType(i);

 // log('Getting addressString');
  addressString:=penv^.GetObjectField(penv, entry, addressString_fieldid);
  if (addressString<>nil) then
  begin
  //  log('got the addressString');
    r.interpretableaddress:=jniGetString(penv, addressString);
  end
  else
    log('addressString=null');


  offsets:=penv^.GetObjectField(penv, entry, offsets_fieldid);
  if (offsets<>nil) then
  begin
  //  log('offsets is not null');

    l:=penv^.GetArrayLength(penv, offsets);
  //  log('the offsets array length='+inttostr(l));

    iscopy:=0;
    offsetlist:=PJintArray(penv^.GetIntArrayElements(penv,offsets, iscopy));
    if offsetlist<>nil then
    begin
      r.offsetCount:=l;
      for i:=0 to l-1 do
        r.offsets[i].offset:=offsetlist[i];

      penv^.ReleaseIntArrayElements(penv, offsets, PJint(offsetlist),JNI_ABORT);
    end
    else
      log('offsetlist=nil');
  end;


  if r.vartype=vtString then
  begin
    Log('set address: string');
    r.extra.stringData.length:=penv^.GetIntField(penv, entry, size_fieldid);
    r.Extra.stringData.unicode:=penv^.GetBooleanField(penv, entry, isUnicode_fieldid)<>0;

    Log('Length='+inttostr(r.extra.stringData.length));
    Log('isUnicode='+BoolToStr(r.extra.stringData.unicode));
  end;

  if r.vartype=vtByteArray then
    r.Extra.byteData.bytelength:=penv^.GetIntField(penv, entry, size_fieldid);

  r.ReinterpretAddress;
end;

function addresslist_addEntry(PEnv: PJNIEnv; Obj: JObject; entry: JObject): jint; cdecl;
var
  r: TMemoryrecord;
begin
  //log('creating memory record');
  r:=TMemoryrecord.Create(nil);
  addresslistcs.enter;
  try
    addresslist.Add(r);
    result:=addresslist.count;

   // log('Initializing memory record. (result='+inttostr(result)+')');
    addresslist_setEntry(penv, obj, result-1, entry);

  finally
    addresslistcs.leave;
  end;

end;

procedure addresslist_deleteEntry(PEnv: PJNIEnv; Obj: JObject; index: jint); cdecl;
begin
  log('Delete entry');

  if (index>=0) and (index<addresslist.count) then
  begin
    addresslistcs.enter;
    try
      TMemoryRecord(addresslist[index]).free;
      addresslist.Delete(index);
    finally
      addresslistcs.leave;
    end;
  end;

end;

procedure addresslist_clear(PEnv: PJNIEnv; Obj: JObject); cdecl;
var i: integer;
begin
  log('addresslist.Clear');
  addresslistcs.enter;
  try
    for i:=0 to addresslist.count-1 do
      TMemoryrecord(addresslist[i]).Free;

    addresslist.Clear;
  finally
    addresslistcs.leave;
  end;
end;


const methodcount=10;
var jnimethods: array [0..methodcount-1] of JNINativeMethod =(
  (name: 'GetCount'; signature: '()I'; fnPtr: @addresslist_getCount),
  (name: 'GetEntry'; signature: '(I)Lorg/cheatengine/AddressListEntry;'; fnPtr: @addresslist_getEntry),
  (name: 'SetEntry'; signature: '(ILorg/cheatengine/AddressListEntry;)V'; fnPtr: @addresslist_setEntry),
  (name: 'AddEntry'; signature: '(Lorg/cheatengine/AddressListEntry;)I'; fnPtr: @addresslist_addEntry),
  (name: 'SetEntryActive'; signature: '(IZ)Z'; fnPtr: @addresslist_setActive),
  (name: 'SetFreezeTimer'; signature: '(I)V'; fnPtr: @addresslist_setFreezeTimer),
  (name: 'DeleteEntry'; signature: '(I)V'; fnPtr: @addresslist_deleteEntry),
  (name: 'Clear'; signature: '()V'; fnPtr: @addresslist_clear) ,

  (name: 'SetEntryValue'; signature: '(ILjava/lang/String;)V'; fnPtr: @addresslist_setEntryValue),
  (name: 'GetEntryValue'; signature: '(I)Ljava/lang/String;'; fnPtr: @addresslist_getEntryValue)


  );

procedure InitializeJniAddressList(env: PJNIEnv);
var c: jclass;
begin
  if addresslist=nil then
  begin
    addresslist:=Tlist.create;
    addresslistcs:=TCriticalSection.Create;
  end;

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
  active_fieldid:=env^.GetFieldID(env, AddressListEntry_class, 'active','Z');

  index_fieldid:=env^.GetFieldID(env, AddressListEntry_class, 'index','I');

  isUnicode_fieldid:=env^.GetFieldID(env, AddressListEntry_class, 'isUnicode','Z');
  size_fieldid:=env^.GetFieldID(env, AddressListEntry_class, 'size','I');

  log('description_fieldid='+inttohex(ptruint(description_fieldid),8));
  log('vartype_fieldid='+inttohex(ptruint(vartype_fieldid),8));
end;


end.

