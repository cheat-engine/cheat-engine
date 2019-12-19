unit DotNetPipe;

{$mode objfpc}{$H+}

interface

{$ifdef jni}
//mainly for some defines for easy compilation
uses unixporthelper, Unix, Classes, SysUtils, syncobjs, NewKernelHandler, Globals;

{$else}
uses
  {$ifdef darwin}
  macport, mactypes,
  {$endif}
  {$ifdef windows}
  jwawindows, windows,
  {$endif}
  Classes, SysUtils, CEFuncProc, syncobjs, NewKernelHandler, Globals, maps;
{$endif}







type
  TDotNetDomain=record
    hDomain: uint64;
    name: widestring;
  end;
  TDotNetDomainArray=array of TDotNetDomain;

  TDotNetModule=record
    hModule: uint64;
    baseaddress: uint64;
    name: widestring;
  end;
  TDotNetModuleArray=array of TDotNetModule;

  TDotNetTypeDef=record
    token: dword;
    name: widestring;
    flags: dword;
    extends: dword;
  end;
  TDotNetTypeDefArray=array of TDotNetTypeDef;

  TNativeCode=record
    address: uint64;
    size: dword;
  end;

  TDotNetMethod=record
    token: dword;
    name: widestring;

    attributes: dword;
    implflags: dword;

    ILCOde: uint64;
    NativeCode: uint64;
    SecondaryNativeCode: array of TNativeCode;
  end;

  TDotNetMethodArray=array of TDotNetMethod;


  TFieldInfo=record
    offset: dword;
    fieldtype: dword;
    name: widestring;
  end;

  TAddressData=record
    startaddress: ptruint;
    objecttype: dword;
    elementtype: dword; //the data type of elements if this is an array
    countoffset, elementsize, firstelementoffset: ulong32; //misc data for objecttype = array or szarray

    classname: widestring;
    fields: array of TFieldInfo;
  end;

  type COR_TYPEID=record
    token1: QWORD;
    token2: QWORD;
  end;

  TDotNetObject=record
    startaddress: qword;
    size: dword;
    typeid: COR_TYPEID;
    classname: pwidechar;
  end;
  PDotNetObject=^TDotNetObject;

  TDOTNETObjectList=tmap;

  TDotNetPipe=class
  private
    pipe: THandle;
    fConnected: boolean;
    fAttached: boolean;
    fSupportsDotNet4_5: boolean;

    pHandle: THandle;
    pipecs: TCriticalsection;
    procedure Read(var o; size: integer);
    procedure Write(const o; size: integer);
  public
    constructor create;
    destructor destroy; override;

    function Connect(processid: dword; is64bit: boolean; timeout:dword=10000):boolean;
    procedure disconnect;

    procedure ReleaseObject(hObject: UINT64);
    procedure EnumDomains(var domains: TDotNetDomainArray);
    procedure EnumModuleList(hDomain: UINT64; var Modules: TDotNetModuleArray);
    procedure EnumTypeDefs(hModule: UINT64; var TypeDefs: TDotNetTypeDefArray);
    procedure GetTypeDefMethods(hModule: UINT64; typedef: DWORD; var Methods: TDotNetMethodArray);
    procedure GetAddressData(address: UINT64; var addressdata: TAddressData);
    function EnumAllObjects: TDOTNETObjectList;
    procedure freeNETObjectList(list: TDOTNETObjectList);

    property Connected: boolean read fConnected;
    property Attached: boolean read fAttached;
    property SupportsDotNet4_5: boolean read fSupportsDotNet4_5;
  end;

implementation

uses DotNetTypes;

const
  CMD_TARGETPROCESS=0;
  CMD_CLOSEPROCESSANDQUIT=1;
  CMD_RELEASEOBJECTHANDLE=2;
  CMD_ENUMDOMAINS=3;
  CMD_ENUMMODULELIST=4;
  CMD_ENUMTYPEDEFS=5;
  CMD_GETTYPEDEFMETHODS=6;
  CMD_GETADDRESSDATA=7;
  CMD_GETALLOBJECTS=8;


procedure TDotNetPipe.freeNETObjectList(list: TDOTNETObjectList);
var
  i: TMapIterator;
  o: PDotNetObject;
begin
  i:=TMapIterator.Create(list);
  i.First;
  while not i.EOM do
  begin
    o:=i.DataPtr;
    if o^.classname<>nil then
    begin
      FreeMemAndNil(o^.classname);

    end;

    i.Next;
  end;

  freeandnil(i);
  freeandnil(list);
end;

function TDotNetPipe.EnumAllObjects: TDOTNETObjectList;
var
  msg: byte;
  r: TDOTNETObjectList;

  done: boolean=false;

  o: TDotNetObject;
  stringlength: DWORD;
begin
  msg:=CMD_GETALLOBJECTS;


  r:=TDOTNETObjectList.Create(itu8,sizeof(TDotNetObject));

  pipecs.enter;
  try
    write(msg, sizeof(msg));

    while not done do
    begin
      read(o.startaddress,sizeof(o.startaddress));
      read(o.size, sizeof(o.size));
      read(o.typeid,sizeof(o.typeid));
      read(stringlength, sizeof(stringlength));
      getmem(o.classname, stringlength+2);
      read(o.classname^,stringlength);
      o.classname[stringlength div 2]:=#0;

      if (o.startaddress=0) and (o.size=0) and (o.typeid.token1=0) and (o.typeid.token2=0) and (stringlength=0) then //end of list marker
        break;


      r.Add(o.startaddress,o);
    end;

  finally
    pipecs.Leave;
  end;

  result:=r;
end;

procedure TDotNetPipe.GetAddressData(address: UINT64; var addressdata: TAddressData);
var
  msg: packed record
    command: byte;
    address: UINT64;
  end;

  classnamesize: dword;
  cname: pwidechar;

  fieldnamesize: dword;
  fieldname: pwidechar;

  fieldcount: ulong32;
  i,j,k: integer;

  fi: TFieldInfo;
  inserted: boolean;
begin
  if fConnected=false then
  begin
    addressdata.startaddress:=0;
    setlength(addressdata.fields,0);
    exit;
  end;



  msg.command:=CMD_GETADDRESSDATA;
  msg.address:=address;

  pipecs.enter;
  try
    write(msg, sizeof(msg));

    read(addressdata.startaddress, sizeof(addressdata.startaddress));
    if addressdata.startaddress<>0 then
    begin
      read(addressdata.objecttype, sizeof(addressdata.objecttype));

      //array support patch by justa_dude
      if (addressdata.objecttype=ELEMENT_TYPE_ARRAY) or (addressdata.objecttype=ELEMENT_TYPE_SZARRAY) then
      begin
        addressdata.classname := 'Array';
        read(addressdata.elementtype, sizeof(addressdata.elementtype));
        read(addressdata.countoffset, sizeof(addressdata.countoffset));
        read(addressdata.elementsize, sizeof(addressdata.elementsize));
        read(addressdata.firstelementoffset, sizeof(addressdata.firstelementoffset));
        if addressdata.elementtype=$FFFFFFFF then //we couldn't determine the array shape
        begin
          addressdata.elementtype := ELEMENT_TYPE_VOID;
          addressdata.elementsize := 0;
        end
      end
      else //then //if true then //addressdata.objecttype=ELEMENT_TYPE_CLASS then
      begin
        read(classnamesize, sizeof(classnamesize));
        if classnamesize>0 then
        begin
          getmem(cname, classnamesize+2);
          read(cname[0], classnamesize);
          cname[classnamesize div 2]:=#0;
          addressdata.classname:=cname;

          FreeMemAndNil(cname);
        end;

        read(fieldcount, sizeof(fieldcount));
        setlength(addressdata.fields, fieldcount);

        for i:=0 to fieldcount-1 do
        begin
          read(fi.offset, sizeof(dword));
          read(fi.fieldtype, sizeof(dword));

          read(fieldnamesize, sizeof(fieldnamesize));
          getmem(fieldname, fieldnamesize+2);
          read(fieldname[0], fieldnamesize);
          fieldname[fieldnamesize div 2]:=#0;
          fi.name:=fieldname;
          FreeMemAndNil(fieldname);

          //sort while adding
          inserted:=false;
          for j:=0 to i-1 do
          begin
            if fi.offset<addressdata.fields[j].offset then //insert it before this one
            begin
              //shift this one and all subsequent items
              for k:=i-1 downto j do
                addressdata.fields[k+1]:=addressdata.fields[k];

              addressdata.fields[j]:=fi;
              inserted:=true;
              break;
            end;
          end;
          if not inserted then
            addressdata.fields[i]:=fi;

        end;
      end;

    end;


  finally
    pipecs.leave;
  end;
end;

procedure TDotNetPipe.GetTypeDefMethods(hModule: UINT64; typedef: DWORD; var Methods: TDotNetMethodArray);
var
  msg: packed record
    command: byte;
    hModule: UINT64;
    typedef: dword;
  end;

  numberofmethods: ULONG;
  i,j: integer;

  mname: pwidechar;
  methodnamesize: dword;

  SecondaryCodeBlocks: ULONG32;
begin
  if fConnected=false then
  begin
    setlength(Methods,0);
    exit;
  end;

  msg.command:=CMD_GETTYPEDEFMETHODS;
  msg.hModule:=hModule;
  msg.typedef:=typedef;

  pipecs.enter;
  try
    write(msg, sizeof(msg));

    read(numberofmethods, sizeof(numberofmethods));
    setlength(methods, numberofmethods);
    for i:=0 to numberofmethods-1 do
    begin
      read(methods[i].token, sizeof(methods[i].token));

      read(methodnamesize, sizeof(methodnamesize));
      getmem(mname, methodnamesize+2);
      try
        read(mname[0], methodnamesize);
        mname[methodnamesize div 2]:=#0;
        methods[i].name:=mname;
      finally
        FreeMemAndNil(mname);
      end;

      read(methods[i].attributes, sizeof(methods[i].attributes));
      read(methods[i].implflags, sizeof(methods[i].implflags));

      read(methods[i].ILCODE, sizeof(methods[i].ILCODE));
      read(methods[i].NativeCode, sizeof(methods[i].NativeCode));
      read(SecondaryCodeBlocks, sizeof(SecondaryCodeBlocks));
      setlength(methods[i].SecondaryNativeCode, SecondaryCodeBlocks);
      for j:=0 to SecondaryCodeBlocks-1 do
        read(methods[i].SecondaryNativeCode[j], sizeof(TNativeCode));
    end;


  finally
    pipecs.leave;
  end;
end;

procedure TDotNetPipe.EnumTypeDefs(hModule: UINT64; var TypeDefs: TDotNetTypeDefArray);
var
  msg: packed record
    command: byte;
    hModule: UINT64;
  end;

  NumberOfTypeDefs: DWORD;
  i: integer;
  typedefnamesize: dword;
  typedefname: pwidechar;
begin
  if fConnected=false then
  begin
    setlength(TypeDefs,0);
    exit;
  end;

  msg.command:=CMD_ENUMTYPEDEFS;
  msg.hModule:=hModule;

  pipecs.enter;
  try
    write(msg, sizeof(msg));

    read(NumberOfTypeDefs, sizeof(NumberOfTypeDefs));

    setlength(typedefs, NumberOfTypeDefs);

    for i:=0 to NumberOfTypeDefs-1 do
    begin
      read(typedefs[i].token, sizeof(ULONG32));
      read(typedefnamesize, sizeof(typedefnamesize));
      getmem(typedefname, typedefnamesize+2);

      try
        read(typedefname[0], typedefnamesize);
        typedefname[typedefnamesize div 2]:=#0;

        typedefs[i].name:=typedefname;
        read(typedefs[i].flags, sizeof(typedefs[i].flags));
        read(typedefs[i].extends, sizeof(typedefs[i].extends));
      finally
        FreeMemAndNil(typedefname);
      end;
    end;

  finally
    pipecs.leave;
  end;
end;

procedure TDotNetPipe.EnumModuleList(hDomain: UINT64; var Modules: TDotNetModuleArray);
var
  msg: packed record
    command: byte;
    hDomain: UINT64;
  end;

  NumberOfModules: DWORD;

  name: pwidechar;
  namelength: ULONG32;
  i,j: integer;
  temp: TDotNetModule;

  _windir: pchar;
  windir: string;
begin
{$ifdef windows}
  if fconnected=false then
  begin
    setlength(modules,0);
    exit;
  end;

  msg.command:=CMD_ENUMMODULELIST;
  msg.hDomain:=hDomain;
  pipecs.enter;
  try
    write(msg, sizeof(msg));

    read(NumberOfModules, sizeof(NumberOfModules));
    setlength(modules, NumberOfModules);
    for i:=0 to NumberOfModules-1 do
    begin
      read(Modules[i].hModule, sizeof(UINT64));
      read(Modules[i].baseaddress, sizeof(UINT64));
      read(namelength, sizeof(namelength));
      getmem(name, namelength+2);
      try
        Read(name[0], namelength);
        name[namelength div 2]:=#0;
        Modules[i].name:=name;
      finally
        FreeMemAndNil(name);
      end;

    end;

  finally
    pipecs.leave;
  end;


  //sort the list so the most interesting ones will be on top

  //find modules not in the windir path

  getmem(_windir,256);
  GetWindowsDirectory(_windir, 255);
  windir:=lowercase(_windir);
  FreeMemAndNil(_windir);


  for i:=0 to length(modules)-1 do
  begin
    if pos(windir, lowercase(modules[i].name))=0 then //not in the windows path
    begin
      //move it to 0
      temp:=modules[i];
      for j:=i downto 1 do
        modules[j]:=modules[j-1];

      modules[0]:=temp;
    end;
  end;


  //make .exe's go on top
  for i:=0 to length(modules)-1 do
  begin
    if ExtractFileExt(lowercase(modules[i].name))='.exe' then
    begin
      //move it to 0
      temp:=modules[i];
      for j:=i downto 1 do
        modules[j]:=modules[j-1];

      modules[0]:=temp;
    end;
  end;
{$endif}

end;

procedure TDotNetPipe.EnumDomains(var domains: TDotNetDomainArray);
var
  msg: packed record
    command: byte;
  end;

  NumberOfDomains: UInt32;
  i: integer;

  namelength: ULONG32;
  name: PWideChar;
begin
  if fConnected=false then
  begin
    setlength(domains,0);
    exit;
  end;


  msg.command:=CMD_ENUMDOMAINS;

  pipecs.enter;
  try
    write(msg, sizeof(msg));
    read(NumberOfDomains, sizeof(NumberOfDomains));

    setlength(domains, NumberOfDomains);

    for i:=0 to NumberOfDomains-1 do
    begin
      read(domains[i].hDomain, sizeof(uint64));
      read(namelength, sizeof(namelength));

      getmem(name, namelength+2);
      try
        Read(name[0], namelength);
        name[namelength div 2]:=#0;

        domains[i].name:=name;
      finally
        FreeMemAndNil(name);
      end;
    end;
  finally
    pipecs.leave;
  end;
end;

procedure TDotNetPipe.ReleaseObject(hObject: uint64);
var
  msg: packed record
    command: byte;
    hobject: uint64;
  end;
begin
  msg.command:=CMD_RELEASEOBJECTHANDLE;
  msg.hobject:=hobject;

  pipecs.enter;
  try
    write(msg, sizeof(msg));
  finally
    pipecs.leave;
  end;
end;

procedure TDotNetPipe.Read(var o; size: integer);
var br: dword;
begin
  if (size=0) then exit;
  {$ifdef unix}
  fconnected:=false;
  {$else}
  fconnected:=fconnected and readfile(pipe, o, size, br, nil);
  {$endif}
end;

procedure TDotNetPipe.Write(const o; size: integer);
var bw: dword;
begin
  if size=0 then exit;
  {$ifdef unix}
  fconnected:=false;
  {$else}
  fconnected:=fconnected and writefile(pipe, o, size, bw, nil);
  {$endif}
end;


function TDotNetPipe.Connect(processid: dword; is64bit: boolean; timeout:dword=10000):boolean;
{
Connects to a dotnet data collector and tells it to open a specific process
}
{$ifdef windows}
var
  starttime: qword;

  msg: packed record
    command: byte;
    pid: dword;
  end;
  r: BOOL;

  pipename: string;

  si: TStartupInfo;
  pi: TProcessInformation;
  bitstring: string;

  ths: THandle;
  me32: TModuleEntry32;
{$endif}
begin
  {$IFDEF windows}
  if fConnected then
    disconnect;


  result:=false;

  //first check if this process uses .net
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE or TH32CS_SNAPMODULE32, processid);
  if ths<>INVALID_HANDLE_VALUE then
  begin
    me32.dwSize:=sizeof(MODULEENTRY32);
    if Module32First(ths, me32) then
    repeat
      if (uppercase(copy(extractfilename(me32.szExePath),1,5))='MSCOR') or
         (uppercase(copy(extractfilename(me32.szExePath),1,4))='CLR.') or
         (uppercase(copy(extractfilename(me32.szExePath),1,7))='CLRJIT.') or
         (uppercase(copy(extractfilename(me32.szExePath),1,10))='SYSTEM.NI.')
      then
      begin
        result:=true;
        break;
      end;
    until Module32Next(ths,me32)=false;

    closehandle(ths);
  end;

  if result=false then exit;

  result:=false;

  pipename:='cedotnetpipe'+inttostr(ProcessID)+'_'+inttostr(GetTickCount64); //unique pipename


  ZeroMemory(@si, sizeof(si));
  ZeroMemory(@pi, sizeof(pi));
  if is64bit then
    bitstring:='64'
  else
    bitstring:='32';

  if CreateProcess(nil, pchar('"'+CheatEngineDir+'DotNetDataCollector'+bitstring+'.exe" '+pipename), nil, nil, false, 0, nil, nil, si, pi)=false then exit;

  closehandle(pi.hThread);
  pHandle:=pi.hProcess;


  //try sending the attach message till write succeeds or timeout
  starttime:=gettickcount64;
  repeat
    pipe:=CreateFile(pchar('\\.\pipe\'+pipename), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if (pipe<>INVALID_HANDLE_VALUE) then
    begin
      fConnected:=true;
      break; //open
    end;


    sleep(10);
  until gettickcount64>starttime+timeout;

  if fConnected then
  begin
    msg.command:=CMD_TARGETPROCESS;
    msg.pid:=processid;
    write(msg, sizeof(msg));
    read(r, sizeof(r));

    fAttached:=r;

    if fAttached then
    begin
      read(r, sizeof(r));
      fSupportsDotNet4_5:=r;
    end;


  end;

  result:=fAttached;

  if not result then
    disconnect;
  {$ENDIF}
end;

procedure TDotNetPipe.disconnect;
var
  msg: packed record
    command: byte;
  end;
  x: dword;
begin
  {$IFDEF windows}
  if fConnected then
  begin
    msg.command:=CMD_CLOSEPROCESSANDQUIT;
    writefile(pipe, msg, sizeof(msg), x,nil);
  end
  else
  begin
    //something bad happened
    if pHandle<>0 then
      TerminateProcess(pHandle, UINT(-1));
  end;

  if (pipe<>INVALID_HANDLE_VALUE) then
  begin
    FlushFileBuffers(pipe);
    DisconnectNamedPipe(pipe);
    closehandle(pipe);
    pipe:=0;
  end;
  {$ENDIF}

  if pHandle<>0 then
    pHandle:=0;

  fConnected:=false;
  fAttached:=false;
end;

constructor TDotNetPipe.create;
begin
  pipecs:=TCriticalsection.create;
  inherited create;
end;


destructor TDotNetPipe.destroy;
begin
  disconnect;
  pipecs.Free;
  inherited destroy;
end;

end.

